.PHONY: all sdk db static

all: sdk db
	@

sdk:
	make -C sdk

db:
	make -C db

db: sdk
	make -C static

clean:
	rm -rf pinata

release:
	if [ -z "$(VERSION)" ]; then echo "VERSION is not set"; exit 1; fi
	git tag $(VERSION) -m "Version $(VERSION)"
	git push upstream master $(VERSION)
