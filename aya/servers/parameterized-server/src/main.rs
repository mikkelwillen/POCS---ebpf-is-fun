use server_lib::run_server;
use std::net::UdpSocket;
use std::collections::HashMap;
use aya::Ebpf;
use aya::maps::Array;

fn main() -> anyhow::Result<()> {
    // Define only the pre and post hooks specific to this server instance.
    let pre_hook = | _socket: &UdpSocket
                   , _state: &mut HashMap<u32, i64>
                   , ebpf: &mut Ebpf
                   , verbose: bool
                   , _capacity: usize | {
        println!("Pre hook");

        let mut array = Array::try_from(ebpf.map_mut("parameter").unwrap())?;

        if verbose {
            println!("Setting parameter[0] to 0");
            array.set(0, 0, 0)?;
        } else {
            println!("Setting parameter[0] to 1");
            array.set(0, 1, 0)?;
        }

        Ok(())
    };

    let post_hook = | _socket: &UdpSocket
                    , _state: &mut HashMap<u32, i64>
                    , _ebpf: &mut Ebpf
                    , _verbose: bool
                    , _capacity: usize | {
        println!("Post hook");

        Ok(())
    };

    run_server(pre_hook, post_hook)
}
