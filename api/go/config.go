package datakit

import (
	"fmt"
	"log"
	"strconv"
	"strings"

	"golang.org/x/net/context"
)

type Version int

var InitialVersion = Version(0)

// Record is a typed view on top of a database branch
type Record struct {
	client   *Client
	path     []string // directory inside the store
	version  Version
	schemaF  *IntField
	fields   []*StringField // registered fields, for schema upgrades
	branch   string
	w        *watch
	onUpdate [](func(*Snapshot, Version))
}

func NewRecord(ctx context.Context, client *Client, branch string, path []string) (*Record, error) {
	if err := client.Mkdir(ctx, "branch", branch); err != nil {
		return nil, err
	}
	w, err := NewWatch(ctx, client, branch, path)
	if err != nil {
		return nil, err
	}
	onUpdate := make([](func(*Snapshot, Version)), 0)
	fields := make([]*StringField, 0)
	r := &Record{client: client, path: path, version: InitialVersion, fields: fields, w: w, onUpdate: onUpdate}
	r.schemaF = r.IntField("schema-version", 1)
	return r, nil
}

func (r *Record) Wait(ctx context.Context) error {
	snapshot, err := r.w.Next(ctx)
	if err != nil {
		return err
	}
	r.version = r.version + 1
	for _, fn := range r.onUpdate {
		fn(snapshot, r.version)
	}
	return nil
}

func (r *Record) Upgrade(ctx context.Context, schemaVersion int) error {
	currentVersion, _ := r.schemaF.Get()
	if schemaVersion <= currentVersion {
		log.Printf("No schema upgrade necessary because new version (%d) <= current version (%d)\n", schemaVersion, currentVersion)
		return nil
	}
	r.schemaF.defaultInt = schemaVersion
	r.schemaF.raw.defaultValue = (fmt.Sprintf("%d", schemaVersion))
	// Create defaults branch
	log.Printf("Performing schema upgrade to version %d\n", schemaVersion)
	t, err := NewTransaction(ctx, r.client, "master", "defaults")
	if err != nil {
		return err
	}
	// For each known field, write default value to branch
	for _, f := range r.fields {
		p := append(r.path, f.path...)
		err = t.Write(ctx, p, f.defaultValue)
		if err != nil {
			return err
		}
	}

	// Merge branch to master
	err = t.Commit(ctx, fmt.Sprintf("Upgrade to schema version %d", schemaVersion))
	if err != nil {
		return err
	}
	return r.Wait(ctx)
}

// fillInDefault writes the default value to the store if no Value
// is already present. This ensures that the system state is always
// in sync with the database, and we don't have to also know what
// default values are also baked into the application.
func (r *Record) fillInDefault(path []string, value string) error {
	ctx := context.Background()
	head, err := Head(ctx, r.client, "master")
	if err != nil {
		return err
	}
	snap := NewSnapshot(ctx, r.client, COMMIT, head)
	p := append(r.path, path...)
	current, err := snap.Read(ctx, p)
	if err != nil {
		return err
	}
	if current != "" {
		return nil
	}
	log.Printf("Writing default value to store: %#v=%s\n", p, value)
	t, err := NewTransaction(ctx, r.client, "master", "fill-in-default")
	if err != nil {
		return err
	}
	err = t.Write(ctx, p, value)
	if err != nil {
		return err
	}
	return t.Commit(ctx, fmt.Sprintf("fill-in default for %s", path))
}

// StringField is a key which is associated with a string value
type StringField struct {
	path         []string
	value        string
	defaultValue string
	version      Version // version of last change
	record       *Record
}

// Set unconditionally sets the value of the key
func (f *StringField) Set(description string, value string) error {
	// TODO: maybe this should return Version, too?
	ctx := context.Background()
	p := append(f.record.path, f.path...)
	log.Printf("Setting value in store: %#v=%s\n", p, value)
	t, err := NewTransaction(ctx, f.record.client, "master", description)
	if err != nil {
		return err
	}
	err = t.Write(ctx, p, value)
	if err != nil {
		return err
	}
	return t.Commit(ctx, fmt.Sprintf("Unconditionally set %s", f.path))
}

// Get retrieves the current value of the key
func (f *StringField) Get() (string, Version) {
	return f.value, f.version
}

// HasChanged returns true if the key has changed since the given version
func (f *StringField) HasChanged(version Version) bool {
	return version < f.version
}

// StringField defines a string option with a specified key and default value.
func (f *Record) StringField(key string, value string) *StringField {
	path := strings.Split(key, "/")

	field := &StringField{path: path, value: value, defaultValue: value, version: InitialVersion, record: f}
	// If the value is not in the database, write the default Value.
	err := f.fillInDefault(path, value)
	if err != nil {
		log.Println("Failed to write default value", key, "=", value)
	}
	fn := func(snap *Snapshot, version Version) {
		ctx := context.Background()
		newValue, err := snap.Read(ctx, path)
		if err != nil {
			log.Println("Failed to read key", key, "from directory snapshot", snap)
			return
		}
		if field.value != newValue {
			field.value = newValue
			field.version = version
		}
	}
	f.onUpdate = append(f.onUpdate, fn)
	//fn(f.version)
	f.fields = append(f.fields, field)
	return field
}

type IntField struct {
	raw        *StringField
	defaultInt int
}

// Get retrieves the current value of the key
func (f *IntField) Get() (int, Version) {
	value64, err := strconv.ParseInt(strings.TrimSpace(f.raw.value), 10, 0)
	if err != nil {
		// revert to default if we can't parse the result
		log.Printf("Failed to parse int in database: '%s', defaulting to %d", f.raw.value, f.defaultInt)
		return f.defaultInt, f.raw.version
	}
	return int(value64), f.raw.version
}

// HasChanged returns true if the key has changed since the given version
func (f *IntField) HasChanged(version Version) bool {
	return version < f.raw.version
}

// IntField defines an boolean option with a specified key and default value
func (f *Record) IntField(key string, value int) *IntField {
	stringValue := fmt.Sprintf("%d", value)
	raw := f.StringField(key, stringValue)
	return &IntField{raw: raw, defaultInt: value}
}

type BoolField struct {
	raw         *StringField
	defaultBool bool
}

// Get retrieves the current value of the key
func (f *BoolField) Get() (bool, Version) {
	value, err := strconv.ParseBool(strings.TrimSpace(f.raw.value))
	if err != nil {
		// revert to default if we can't parse the result
		log.Printf("Failed to parse boolean in database: '%s', defaulting to %t", f.raw.value, f.defaultBool)
		return f.defaultBool, f.raw.version
	}
	return value, f.raw.version
}

// HasChanged returns true if the key has changed since the given version
func (f *BoolField) HasChanged(version Version) bool {
	return version < f.raw.version
}

// BoolField defines an boolean option with a specified key and default value
func (f *Record) BoolField(key string, value bool) *BoolField {
	stringValue := fmt.Sprintf("%t", value)
	raw := f.StringField(key, stringValue)
	return &BoolField{raw: raw, defaultBool: value}
}
