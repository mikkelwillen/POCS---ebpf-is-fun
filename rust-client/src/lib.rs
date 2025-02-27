use std::net::{UdpSocket, SocketAddr};
use std::time::Duration;

#[derive(Debug)]
pub enum Message {
	Get(u32),
	Put(u32, i64),
	Delete(u32),
	Stop,
	Bad(i64),
}

pub fn logging(verbose: bool, msg: &str) {
	if verbose {
		println!("{}", msg);
	}
}

// Encode a message into a byte array
pub fn encode_message(msg: &Message) -> Vec<u8> {
	let mut buf = Vec::new();

	match msg {
		Message::Put(key, value) => {
			buf.extend_from_slice(b"PUT");
			buf.extend_from_slice(&key.to_le_bytes());
			buf.extend_from_slice(&value.to_le_bytes());
		}
		Message::Get(key) => {
			buf.extend_from_slice(b"GET");
			buf.extend_from_slice(&key.to_le_bytes());
		}
		Message::Delete(key) => {
			buf.extend_from_slice(b"DELETE");
			buf.extend_from_slice(&key.to_le_bytes());
		}
		Message::Bad(size) => {
			buf.extend_from_slice(b"BAD");
			buf.extend(vec![0x42; *size as usize]); // Simulating bad data
		}
		Message::Stop => {
			buf.extend_from_slice(b"STOP");
		}
	}

	buf
}

pub fn send_command(socket: &UdpSocket, addr: &SocketAddr, msg: &Message, verbose: bool) {
	let encoded_msg = encode_message(msg);
	if let Err(e) = socket.send_to(&encoded_msg, addr) {
		logging(verbose, &format!("Failed to send message: {}", e));
	}
}

pub fn send_put_cmds(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	logging(verbose, "Sending PUT commands");
	for &val in &[2, 4, 6, -5] {
		send_command(socket, addr, &Message::Put(10, val), verbose);
	}
	logging(verbose, "PUT commands sent");
}

// Send GET command to the server with specified key 10
pub fn send_get_cmd(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	send_get_cmd_with_key(socket, addr, verbose, 10);
}

pub fn send_get_cmd_with_key(socket: &UdpSocket, addr: &SocketAddr, verbose: bool, key: u32) {
	logging(verbose, "Sending GET command");
	send_command(socket, addr, &Message::Get(key), verbose);

	// Receive response from the server
	let mut buf = [0; 1024];

	// Set a timeout of 10,000 microseconds
	socket.set_read_timeout(Some(Duration::new(0, 10_000))).unwrap();

	match socket.recv_from(&mut buf) {
		Ok((resp, _)) => {
			let resp = &buf[..resp];
			let resp = String::from_utf8_lossy(resp);
			logging(verbose, &format!("Received response: {}", resp));
		}
		Err(_) => {
			logging(verbose, "No response from the server");
		}
	}
}

// Sends 1 million BAD commands to the server
pub fn send_bad_cmds(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	logging(verbose, "Sending many BAD commands");
	for _ in 0..1_000_000 {
		send_command(socket, addr, &Message::Bad(1024), verbose);
	}
	logging(verbose, "BAD commands sent");
}

pub fn send_del_cmd(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	logging(verbose, "Sending DELETE command");
	send_command(socket, addr, &Message::Delete(10), verbose);
	logging(verbose, "DELETE command sent");
}

pub fn send_stop_cmd(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	logging(verbose, "Sending STOP command");
	send_command(socket, addr, &Message::Stop, verbose);
	logging(verbose, "STOP command sent");
}

pub fn send_put9_cmd(socket: &UdpSocket, addr: &SocketAddr, verbose: bool) {
	logging(verbose, "Sending PUT 9 commands");
	for &key in &[4660, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20] {
		send_command(socket, addr, &Message::Put(key, 10), verbose);
	}
	logging(verbose, "PUT 9 commands sent");
}
