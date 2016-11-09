using System;
using System.Collections.Generic;
using System.Linq;

namespace Datakit
{
    public interface IRecord
    {
        List<string> Path { get; set; }
        List<Field> Fields { get; set; }
        IBranch Parent { get; set; }
        int Version { get; set; }
        void Sync();
        void WaitForUpdates();
        void Upgrade(int schemaVersion);
    }

    public class Record : IRecord
    {
        private const string SchemaVersionKey = "schema-version";
        private const int DefaultSchemaVersion = 1;
        private const string Defaults = "defaults";
        private const string SchemaUpgrade = "schema_upgrade";
        private const string DefaultCommitMessage = "Set default values";
        private const int DefaultVersion = 1;
        private readonly IClient _client;
        private readonly IWatch _watch;
        public Field<int> SchemaVersion;

        public Record(IClient client, IBranch parent, List<string> path)
        {
            _client = client;
            Parent = parent;
            Path = path;
            _watch = new Watch(_client, Parent.Path, path);
            SchemaVersion = new Field<int>(this, SchemaVersionKey, DefaultSchemaVersion);
            Fields = new List<Field> {SchemaVersion};
            Version = DefaultVersion;
        }

        public List<string> Path { get; set; }
        public List<Field> Fields { get; set; }
        public IBranch Parent { get; set; }
        public int Version { get; set; }

        public void Sync()
        {
            var transaction = Parent.NewTransaction(Defaults);
            var operations = 0;
            var noCommits = false;
            Snapshot snapshot = null;
            try
            {
                snapshot = new CommmitSnapshot(_client, Parent.Head);
            } catch (Exception)
            {
                noCommits = true;
            }
            if (!noCommits)
            {
                foreach (var field in Fields)
                {
                    var path = Path.ToList();
                    path.AddRange(field.Path);
                    try
                    {
                        snapshot.Read(path);
                    }
                    catch (Exception)
                    {
                        transaction.Write(path, field.DefaultValue);
                        operations++;
                    }
                }
            }
            else
            {
                foreach (var field in Fields)
                {
                    var path = Path.ToList();
                    path.AddRange(field.Path);

                    transaction.Write(path, field.DefaultValue);
                    operations++;
                }
            }

            if (operations > 0)
            {
                transaction.Message = DefaultCommitMessage;
                transaction.Commit();
            }
            else
            {
                transaction.Close();
            }
        }

        public void WaitForUpdates()
        {
            var snapshot = _watch.Next();
            Version++;
            foreach (var field in Fields)
            {
                field.OnUpdate(Version, snapshot, Path);
            }
        }

        public void Upgrade(int schemaVersion)
        {
            if (schemaVersion <= SchemaVersion.RawValue)
                return;
            SchemaVersion.RawDefaultValue = schemaVersion;
            var transaction = Parent.NewTransaction(SchemaUpgrade);
            foreach (var field in Fields)
            {
                var path = Path.ToList();
                path.AddRange(field.Path);
                transaction.Write(path, field.DefaultValue);
            }
            transaction.Commit();
        }
    }
}