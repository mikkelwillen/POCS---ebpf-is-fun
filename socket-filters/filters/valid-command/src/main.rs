#![no_std]
#![no_main]

// Import the `socket_filter` macro used to specify the entry point of the eBPF program
// Import the `SkBuffContext` struct with metadata and payload
use aya_ebpf::{
    macros::socket_filter,
    programs::SkBuffContext
};

// Filter that lets valid commands through
#[socket_filter]
pub fn socket_filter(ctx: SkBuffContext) -> i64 {
    match try_socket_filter(ctx) {
        Ok(ret) => ret,
        Err(_) => 0
    }
}

pub fn try_socket_filter(ctx: SkBuffContext) -> Result<i64, ()> {
    // Offset to the start of the payload
    let payload_offset = 8;

    // Get the length of the packet
    let len = ctx[4..6].load::<u8>().unwrap() as usize;
    Ok(len)
}

// Simple panic handler
#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
