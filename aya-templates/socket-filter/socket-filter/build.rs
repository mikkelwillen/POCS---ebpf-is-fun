use anyhow::{anyhow, Context as _};
use aya_build::cargo_metadata;

fn main() -> anyhow::Result<()> {
    // Locate packages in the cargo workspace
    let cargo_metadata::Metadata { packages, .. } = cargo_metadata::MetadataCommand::new()
        .no_deps()
        .exec()
        .context("MetadataCommand::exec")?;

    // Find the socket-filter-ebpf package
    let ebpf_package = packages
        .into_iter()
        .find(|cargo_metadata::Package { name, .. }| name == "socket-filter-ebpf")
        .ok_or_else(|| anyhow!("socket-filter-ebpf package not found"))?;

    // Build the eBPF program
    aya_build::build_ebpf([ebpf_package])
}
