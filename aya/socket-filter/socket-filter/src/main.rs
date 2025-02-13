use aya::programs::SocketFilter;

fn main() -> anyhow::Result<()> {
    // Load eBPF bytecode
    let mut ebpf = aya::Ebpf::load(aya::include_bytes_aligned!(concat!(
        env!("OUT_DIR"),
        "/socket-filter"
    )))?;

    // Bind UDP socket to port 12345 on localhost
    let socket = std::net::UdpSocket::bind("127.0.0.1:12345")?;

    // Load the socket_filter program, and attach to socket
    let prog: &mut SocketFilter = ebpf.program_mut("socket_filter").unwrap().try_into()?;
    prog.load()?;
    prog.attach(&socket)?;

    // Debug msg
    println!("Ready to receive");

    // Receive msg(s) and print
    let mut buf = [0; 100];
    match socket.recv(&mut buf) {
        Ok(received) => println!("received {received} bytes {:?}", &buf[..received]),
        Err(e) => println!("recv function failed: {e:?}"),
    }

    Ok(())
}
