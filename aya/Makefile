##
# Socket filters

filter ?= simple-socket-filter
server ?= simple-server
flags ?=

build:
	EBPF_FILTER=$(filter) cargo build

run:
	EBPF_FILTER=$(filter) cargo run --config 'target."cfg(all())".runner="sudo -E"' -- $(flags)

clean:
	cargo clean

# end
