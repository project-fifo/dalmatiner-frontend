INSTALL_DIR=usr/local/lib
DEPS="coreutils" "sudo"
REBARPROFILE ?= default

include ../../config.mk
include ../../_build/${REBARPROFILE}/lib/fifo_utils/priv/pkgng.mk

.PHONY: package prepare clean

prepare:
	-rm -r $(STAGE_DIR)
	mkdir -p $(STAGE_DIR)/$(INSTALL_DIR)
	cp -r ../../_build/${REBARPROFILE}/rel/$(COMPONENT_INTERNAL) $(STAGE_DIR)/$(INSTALL_DIR)
	rm -rf $(STAGE_DIR)/$(INSTALL_DIR)/lib/*/c_src
	mkdir -p $(STAGE_DIR)/usr/local/etc/rc.d
	cp ../freebsd/dalmatinerfe $(STAGE_DIR)/usr/local/etc/rc.d/
