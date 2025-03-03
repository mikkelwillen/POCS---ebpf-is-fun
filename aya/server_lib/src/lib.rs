use aya::programs::SocketFilter;
use std::collections::HashMap;
use std::io;

// Command Line Argument Parsing
use clap::Parser;

mod parser;
use parser:: {
    Message,
    parse_message,
};

// Command line arguments
#[derive(Parser)]
#[clap(version = "1.0")]
struct CommandLineArgs {
    #[clap(short, long)]
    verbose: bool,

    #[clap(short, long, default_value_t = 100000)]
    capacity: usize,
}

// Logging function for when verbose flag is set
pub fn logging( verbose: bool
              , msg: &str) {
    if verbose {
        println!("{}", msg);
    }
}

// Process a message and update the state map
fn process_message( state: &mut HashMap<u32, i64>
                  , msg: Message
                  , verbose: bool
                  , capacity: usize
                  ) -> Option<Vec<u8>> {
    match msg {
        Message::Put(key, value) => {
            if state.len() < capacity || state.contains_key(&key) {
                *state.entry(key).or_insert(0) += value;
            } else {
                logging(verbose, "State map is full, cannot add new key");
            }
            None
        }
        Message::Get(key) => {
            state.get(&key).map(|&count| count.to_string().into_bytes())
        }
        Message::Delete(key) => {
            state.remove(&key);
            None
        }
        Message::Stop => {
            logging(verbose, "process_message should not receive Stop message");
            None
        }
    }
}

// Serve function to handle incoming messages
fn serve( socket: &std::net::UdpSocket
        , state: &mut HashMap<u32, i64>
        , verbose: bool
        , capacity: usize
        ) -> io::Result<()> {
    let mut buf = [0; 1024];

    // Loop until we receive a Stop message
    loop {
        match socket.recv_from(&mut buf) {
            Ok((num_bytes, sender)) => {
                // Parse the recieved message in buf
                let msg_option = parse_message(&buf[..num_bytes]);
                match msg_option {
                    // Stop the server if we receive a Stop message
                    Some(Message::Stop) => {
                        logging(verbose, "Stopping server");
                        break Ok(());
                    }
                    // Process other valid messages
                    Some(msg) => {
                        logging(verbose, &format!("Got message {:?} from {}", msg, sender));
                        let reply = process_message(state, msg, verbose, capacity);
                        if let Some(response) = reply {
                            logging(verbose, &format!("Sending response {:?}", std::str::from_utf8(&response).unwrap()));
                            socket.send_to(&response, sender)?;
                        }
                    }
                    // Case for invalid messages
                    None => {
                        logging(verbose, "Received invalid message");
                        ()
                    }
                }
            }
            Err(e) => println!("recv function failed: {:?}", e),
        }
    }
}

pub fn run_server<F, G>( pre: F
                       , post: G
                       ) -> anyhow::Result<()>
    where
        F: Fn( &std::net::UdpSocket
             , &mut std::collections::HashMap<u32, i64>
             , &mut aya::Ebpf
             , bool
             , usize)
             -> anyhow::Result<()>,
        G: Fn( &std::net::UdpSocket
             , &mut std::collections::HashMap<u32, i64>
             , &mut aya::Ebpf
             , bool
             , usize)
             -> anyhow::Result<()> {
    // Parse command line arguments
    let args = CommandLineArgs::parse();

    let verbose = args.verbose;
    let capacity = args.capacity;

    // Load eBPF bytecode
    let mut ebpf = aya::Ebpf::load(aya::include_bytes_aligned!(concat!(
        env!("OUT_DIR"),
        "/socket-filter"
    )))?;
    logging(verbose, &format!("Loaded eBPF bytecode from {:?}", env!("OUT_DIR")));

    // Bind UDP socket to port 12345 on localhost
    let socket = std::net::UdpSocket::bind("127.0.0.1:12345")?;
    logging(verbose, &format!("UdpSocket bound to {:?}", socket.local_addr().unwrap()));

    // Load the socket_filter program, and attach to socket
    let prog: &mut SocketFilter = ebpf.program_mut("socket_filter").unwrap().try_into()?;
    prog.load()?;
    prog.attach(&socket)?;

    // Create the state map
    let mut state = HashMap::<u32, i64>::new();

    // Pre function
    if let Err(e) = pre(&socket, &mut state, &mut ebpf, verbose, capacity) {
        eprintln!("Pre function failed: {:?}", e);
    }

    // Start the server
    logging(verbose, "Starting server");
    serve(&socket, &mut state, verbose, capacity)?;

    // Post function
    if let Err(e) = post(&socket, &mut state, &mut ebpf, verbose, capacity) {
        eprintln!("Post function failed: {:?}", e);
    }

    Ok(())
}
