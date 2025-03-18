use std::net::{SocketAddr, UdpSocket};
use std::thread;
use std::time::Duration;
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(name = "UDP Client", version = "1.0")]
struct CommandLineArgs {
	#[clap(short, long)]
	verbose: bool,

	#[clap(short, long, default_value = "thor")]
	behaviour: String,

	#[clap(short, long, default_value_t = 0)]
	percent: usize,

	#[clap(short, long, default_value_t = 100000)]
	number_of_packets: usize,
}

mod lib;
use lib::*;

fn thor(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	send_put_cmds(socket, addr, verbose);
	send_put_cmds(socket, addr, verbose);
	send_stop_cmd(socket, addr, verbose);
}

fn odin(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	send_put_cmds(socket, addr, verbose);
	send_get_cmd(socket, addr, verbose);
	send_put_cmds(socket, addr, verbose);
	send_get_cmd(socket, addr, verbose);
	send_stop_cmd(socket, addr, verbose);
}

fn loki(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	send_put_cmds(socket, addr, verbose);
	send_get_cmd(socket, addr, verbose);
	send_bad_cmds(socket, addr, verbose);
	send_put_cmds(socket, addr, verbose);
	send_get_cmd(socket, addr, verbose);
	send_stop_cmd(socket, addr, verbose);
}

fn njord(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	send_put_cmds(socket, addr, verbose);
	send_get_cmd(socket, addr, verbose);

	for _ in 0..5 {
		send_del_cmd(socket, addr, verbose);
	}

	send_put_cmds(socket, addr, verbose);
	send_get_cmd(socket, addr, verbose);
	send_del_cmd(socket, addr, verbose);

	send_stop_cmd(socket, addr, verbose);
}

fn sylvie(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	logging(verbose, "Sending 10 million PUT commands");
	for _ in 0..10_000_000 {
		send_command(socket, addr, &Message::Put(10, 1), verbose);
	}
	send_stop_cmd(socket, addr, verbose);
}

fn sif(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	send_get_cmd(socket, addr, verbose);
	send_put9_cmd(socket, addr, verbose);
	send_get_cmd(socket, addr, verbose);
	send_get_cmd_with_key(socket, addr, verbose, 18);
	send_get_cmd_with_key(socket, addr, verbose, 20);
	send_del_cmd(socket, addr, verbose);
	send_get_cmd(socket, addr, verbose);
	send_stop_cmd(socket, addr, verbose);
}

// Sends number_of_packets commands where 100-percent% are PUT and percent% are BAD
// where percent is CLA
fn frey(socket: &UdpSocket, addr: &SocketAddr, verbose: bool, percent: usize, number_of_packets: usize) {
	let total = number_of_packets;
	let bad = (total * percent) / 100;
	let good = total - bad;

	logging(verbose, &format!("Sending {} PUT commands", good));
	for _ in 0..good {
		send_command(socket, addr, &Message::Put(10, 1), verbose);
	}

	logging(verbose, &format!("Sending {} BAD commands", bad));
	for _ in 0..bad {
		send_command(socket, addr, &Message::Bad(256), verbose);
	}

	send_get_cmd(socket, addr, verbose);
	send_stop_cmd(socket, addr, verbose);
}

// Sends number_of_packets commands where 100-percent% are PUT and percent% are BAD
// where number_of_packets and percent is CLA
// Sends PUT and BAD commands in iterations of 100
fn frigg(socket: &UdpSocket, addr: &SocketAddr, verbose: bool, percent: usize, number_of_packets: usize) {
	let total = number_of_packets;
	let bad = (total * percent) / 100;
	let good = total - bad;

	for i in 0..(total / 100) {
		logging(verbose, &format!("Sending {} PUT commands (iteration {})", good / (total / 100), i));
		for _ in 0..good / (total / 100) {
			send_command(socket, addr, &Message::Put(10, 1), verbose);
		}

		logging(verbose, &format!("Sending {} BAD commands (iteration {})", bad / (total / 100), i));
		for _ in 0..bad / (total / 100) {
			send_command(socket, addr, &Message::Bad(256), verbose);
		}
	}

	send_get_cmd(socket, addr, verbose);
	send_stop_cmd(socket, addr, verbose);
}

fn main() {
	// Parse command line arguments
	let args = CommandLineArgs::parse();

	let verbose = args.verbose;
	let behaviour = args.behaviour;
	let percent = args.percent;
	let number_of_packets = args.number_of_packets;

	// Create UDP socket and server address
	let socket = UdpSocket::bind("127.0.0.1:0").expect("Failed to bind UDP socket");
	let server_addr = "127.0.0.1:12345".parse().expect("Invalid server address");

	thread::sleep(Duration::from_secs(1));

	// Run behaviour based on argument
	match behaviour.as_str() {
		"thor" => thor(&socket, &server_addr, verbose),
		"odin" => odin(&socket, &server_addr, verbose),
		"loki" => loki(&socket, &server_addr, verbose),
		"njord" => njord(&socket, &server_addr, verbose),
		"sylvie" => sylvie(&socket, &server_addr, verbose),
		"sif" => sif(&socket, &server_addr, verbose),
		"frey" => frey(&socket, &server_addr, verbose, percent, number_of_packets),
		"frigg" => frigg(&socket, &server_addr, verbose, percent, number_of_packets),
		_ => logging(verbose, "Not a valid behaviour"),
	}
}
