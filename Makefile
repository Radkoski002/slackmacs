VERSION:=1.0.0
PACKAGE_NAME:=slackmacs-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

all: clean package 

build-backend:
	cargo build --manifest-path ./rust-backend/Cargo.toml;
	cp ./rust-backend/target/debug/slackmacs-rust-backend ./elisp/slackmacs-rust-backend

build:
	cargo build --manifest-path ./emacs-rust/Cargo.toml;
	cp ./emacs-rust/target/debug/libslackmacs_module_rs.so ./elisp/slackmacs-module-rs.so

package: build build-backend
	mkdir $(PACKAGE_DIR)
	cp -r ./elisp/* $(PACKAGE_DIR)
	tar cvf ./$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

test:
	cargo test --manifest-path ./emacs-rust/Cargo.toml

clean:
	rm -f ./$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)
	rm -f ./elisp/slackmacs-module-rs.so

# end
