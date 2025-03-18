##
# Socket filters

filter ?= simple-socket-filter
server ?= simple-server
flags ?=

build:
	EBPF_FILTER=$(filter) cargo build -p $(server)

build-release:
	EBPF_FILTER=simple-socket-filter cargo build --release -p $(server) --target-dir simple-socket-filter
	EBPF_FILTER=valid-command cargo build --release -p $(server) --target-dir valid-command
	EBPF_FILTER=reject-filter cargo build --release -p $(server) --target-dir reject-filter
	EBPF_FILTER=robust-valid-command cargo build --release -p $(server) --target-dir robust-valid-command
	EBPF_FILTER=parameterized-filter cargo build --release -p parameterized-server --target-dir parameterized-filter

run:
	EBPF_FILTER=$(filter) cargo run -p $(server) --config 'target."cfg(all())".runner="sudo -E"' -- $(flags)

clean:
	cargo clean
	rm -rf simple-socket-filter valid-command robust-valid-command reject-filter parameterized-filter

# end
