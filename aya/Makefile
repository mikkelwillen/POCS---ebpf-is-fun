##
ifndef p
	$(error p is not defined. Please run 'make p=dir/')
endif

i ?= wlp1s0

# Generate a new Aya project.
generate:
	cargo generate https://github.com/aya-rs/aya-template

# Build program at target
build:
	cargo build --manifest-path $(p)Cargo.toml

# Run program at target
run:
	cargo run --manifest-path $(p)Cargo.toml --config 'target."cfg(all())".runner="sudo -E"' -- \
  --iface $(i)

# Run program at target with log
run-log:
	RUST_LOG=info cargo run --manifest-path $(p)Cargo.toml --config 'target."cfg(all())".runner="sudo -E"' -- \
  --iface $(i)
