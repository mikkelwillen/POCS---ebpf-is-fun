use anyhow::{anyhow, Context as _};
use aya_build::cargo_metadata;
use std::env;

fn main() -> anyhow::Result<()> {
    // Get the package name from an environment variable or default to "simple-socket-filter"
    let filter_name = env::var("EBPF_FILTER").unwrap_or_else(|_| "simple-socket-filter".to_string());

    // Locate packages in the cargo workspace
    let cargo_metadata::Metadata { packages, .. } = cargo_metadata::MetadataCommand::new()
        .no_deps()
        .exec()
        .context("MetadataCommand::exec")?;

    // Find the specified eBPF package
    let ebpf_package = packages
        .into_iter()
        .find(|pkg| pkg.name == filter_name)
        .ok_or_else(|| anyhow!("{} package not found", filter_name))?;

    // Build the eBPF program
    aya_build::build_ebpf([ebpf_package])
}
