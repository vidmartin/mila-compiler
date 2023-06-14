
all: src/*.rs
	cargo build

	if [[ -e ./build ]]; then rm -R ./build; fi
	mkdir build
	cp ./target/debug/mila ./build/mila
