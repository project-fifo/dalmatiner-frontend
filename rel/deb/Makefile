VERSION = $(BUILD_NUMBER) # THis does not seem to work at the moment
VERSION ?= 0
COMPONENT = ddb

.PHONY: prepare package

package: clean prepare
#	We keep actuall packaging in jenkins for now
#	dpkg-deb -b stage ddb_$(VERSION)

prepare:
	mkdir -p stage/etc
	mkdir -p stage/usr/bin
	mkdir -p stage/usr/lib/dfe
	mkdir -p stage/var/lib/dfe
	mkdir -p stage/var/log/dfe
	cp -r ../../_build/deb/rel/dalmatinerfe/bin stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/etc stage/etc/dfe
	cp -r ../../_build/deb/rel/dalmatinerfe/erts-* stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/lib stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/releases stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/share stage/usr/lib/dfe/

clean:
	rm -rf ./stage
	rm -f *.deb
