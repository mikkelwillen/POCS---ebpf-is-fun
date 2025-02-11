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

# Run program at target
run: check-p
	cargo run --manifest-path $(p)Cargo.toml --config 'target."cfg(all())".runner="sudo -E"' -- \
  --iface $(i)

# Run program at target with log
run-log: check-p
	RUST_LOG=info cargo run --manifest-path $(p)Cargo.toml --config 'target."cfg(all())".runner="sudo -E"' -- \
  --iface $(i)

# Clean all build files
clean:
	@for dir in $(SUBDIRS); do \
		echo "Running cargo clean in $$dir"; \
		cargo clean --manifest-path $$dir/Cargo.toml; \
	done
