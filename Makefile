VERSION:=0.0.1
PACKAGE_NAME:=slackmacs-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

all: clean package

build:
	cargo build --manifest-path ./rust/Cargo.toml;
	cp ./rust/target/debug/libslackmacs_module_rs.so ./elisp/slackmacs-module-rs.so

package: build 
	mkdir $(PACKAGE_DIR)
	cp -r ./elisp/* $(PACKAGE_DIR)
	tar cvf ./$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

clean:
	rm -f ./$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)
	rm -f ./elisp/slackmacs-module-rs.so

# end
