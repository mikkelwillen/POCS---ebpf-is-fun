use std::convert::TryInto;
use std::str;


#[derive(Debug)]
pub enum Message {
	Get(u32),
	Put(u32, i64),
	Delete(u32),
	Stop,
}

pub fn parse_message(request: &[u8]) -> Option<Message> {
	let pos = request.iter().position(|&b| !b.is_ascii_alphabetic()).unwrap_or(request.len());
	let (cmd_bytes, rest) = request.split_at(pos);
	let cmd = str::from_utf8(cmd_bytes).ok()?;

	match cmd {
		"PUT" => {
			if rest.len() < 12 {
				return None;
			}
		    let key = u32::from_le_bytes(rest[0..4].try_into().ok()?);
			let value = i64::from_le_bytes(rest[4..12].try_into().ok()?);
			Some(Message::Put(key, value))
		},
		"GET" => {
			if rest.len() < 4 {
				return None;
			}
			let key = u32::from_le_bytes(rest[0..4].try_into().ok()?);
			Some(Message::Get(key))
		},
		"DELETE" => {
			if rest.len() < 4 {
				return None;
			}
			let key = u32::from_le_bytes(rest[0..4].try_into().ok()?);
			Some(Message::Delete(key))
		},
		"STOP" => Some(Message::Stop),
		_ => None,
	}
}
