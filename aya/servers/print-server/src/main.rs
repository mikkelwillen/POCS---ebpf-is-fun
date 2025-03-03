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

        Ok(())
    };

    let post_hook = | _socket: &UdpSocket
                    , _state: &mut HashMap<u32, i64>
                    , ebpf: &mut Ebpf
                    , verbose: bool
                    , _capacity: usize | {
        println!("Post hook");
        let array = PerCpuArray::try_from(ebpf.map_mut("PKT_PRT_ARRAY").unwrap())?;

        logging(verbose, "Printing contents of array");

        let print_array: PerCpuValues<Buf> = array.get(&0, 0)?;
        // Print contents of the array
        for i in 0..print_array.len() {
            println!("{}: {:?}", i, print_array.get(i).unwrap().buf);
        };
        Ok(())
    };
    run_server(pre_hook, post_hook)
}
