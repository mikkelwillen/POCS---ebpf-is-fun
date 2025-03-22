use server_lib::run_server;
use std::net::UdpSocket;
use std::collections::HashMap;
use std::io::Read;
use aya::Ebpf;

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
                    , _ebpf: &mut Ebpf
                    , _verbose: bool
                    , _capacity: usize | {
        // Get PID of this program
        let pid = std::process::id();

        // Path
        let path = format!("/proc/{}/fdinfo/4", pid);

        // Open the file descriptor for the ebpf_stats
        let mut file = std::fs::File::open(path).unwrap();

        // Read the file descriptor and print run_time_ns and run_cnt
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        println!("Post hook: {}", contents);

        Ok(())
    };

    run_server(pre_hook, post_hook)
}
