use server_lib::run_server;
use std::net::UdpSocket;
use std::collections::HashMap;
use aya::Ebpf;
use aya::maps::PerCpuArray;
use aya::maps::PerCpuValues;
use aya::util::nr_cpus;

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
                    , _verbose: bool
                    , _capacity: usize | {
        let array = PerCpuArray::try_from(ebpf.map_mut("counter").unwrap())?;

        let counterArray: PerCpuValues<u32> = array.get(&0, 0)?;
        let mut total: u32 = 0;
        for i in 0..nr_cpus().expect("failed to get nr_cpus") {
            println!("counterArray[{}]: {}", i, counterArray[i]);
            total += counterArray[i];
        }
        println!("total: {}", total);

        Ok(())
    };

    run_server(pre_hook, post_hook)
}
