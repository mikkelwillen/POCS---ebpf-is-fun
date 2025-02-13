##
# Target to check if p is defined
check-p:
	@if [ -z "$(p)" ]; then \
	  echo "p is not defined. Please run 'make p=dir/'"; \
	  exit 1; \
	fi

.PHONY: generate build run run-log clean $(SUBDIRS)

# List of all subdirectories
SUBDIRS := $(shell find . -maxdepth 1 -type d)

# Default interface
i ?= wlp1s0

# Generate a new Aya project.
generate:
	cargo generate https://github.com/aya-rs/aya-template

# Build program at target
build: check-p
	cargo build --manifest-path $(p)Cargo.toml

# Run xdp program at target
runxdp: check-p
	cargo run --manifest-path $(p)Cargo.toml --config 'target."cfg(all())".runner="sudo -E"' -- \
  --iface $(i)

# Run xdp program at target with log
runxdp-log: check-p
	RUST_LOG=info cargo run --manifest-path $(p)Cargo.toml --config 'target."cfg(all())".runner="sudo -E"' -- \
  --iface $(i)

# Run socket program at target
runsoc : check-p
	cargo run --manifest-path $(p)Cargo.toml --config 'target."cfg(all())".runner="sudo -E"'

# Run socket program at target with log
runsoc-log : check-p
	RUST_LOG=info cargo run --manifest-path $(p)Cargo.toml --config 'target."cfg(all())".runner="sudo -E"'

# Clean all build files
clean:
	@for dir in $(SUBDIRS); do \
		echo "Running cargo clean in $$dir"; \
		cargo clean --manifest-path $$dir/Cargo.toml; \
	done
