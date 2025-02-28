use server_lib::run_server;
use server_lib::logging;
use std::net::UdpSocket;
use std::collections::HashMap;
use aya::Ebpf;
use aya::maps::PerCpuArray;
use aya::maps::PerCpuValues;
use aya::Pod;
use std::io;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Buf {
    pub buf: [u8; 1500],
}

unsafe impl Pod for Buf {}

fn main() -> anyhow::Result<()> {
    // Define only the pre and post hooks specific to this server instance.
    let pre_hook = | _socket: &UdpSocket
                   , _state: &mut HashMap<u32, i64>
                   , _ebpf: &mut Ebpf
                   , _verbose: bool
                   , _capacity: usize | {
        println!("Pre hook");
        // Additional instance-specific pre-processing can go here.
    };

    let post_hook = | _socket: &UdpSocket
                    , _state: &mut HashMap<u32, i64>
                    , _ebpf: &mut Ebpf
                    , _verbose: bool
                    , _capacity: usize | {
        println!("Post hook");
    };

    run_server(pre_hook, post_hook)
}
