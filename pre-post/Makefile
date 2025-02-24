##
# Socket filters

filter ?= simple-socket-filter

build:
	EBPF_FILTER=$(filter) cargo build

run:
	EBPF_FILTER=$(filter) RUST_LOG=info cargo run --config 'target."cfg(all())".runner="sudo -E"'

clean:
	cargo clean

# end
