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
        Ok(_) => -1,
        Err(_) => 0,
    }
}

pub fn try_socket_filter(ctx: SkBuffContext) -> Result<(), ()> {
    // Get package length
    let len = ctx.len();
    if len == 0 {
        return Err(());
    }

    let read_len = core::cmp::min(len, 64);

    // Buffer to store the package
    let mut command: [u8; 64] = [0; 64];

    // Copy the command to the buffer
    ctx.load_bytes(0, &mut command[..read_len as usize]).map_err(|_| ())?;

    // Check if the command is valid
    if command[8] == 0x50
    && command[9] == 0x55
    && command[10] == 0x54 {
        Ok(())
    } else if command[8] == 0x47
           && command[9] == 0x45
           && command[10] == 0x54 {
        Ok(())
    } else if command[8] == 0x44
           && command[9] == 0x45
           && command[10] == 0x4C
           && command[11] == 0x45
           && command[12] == 0x54
           && command[13] == 0x45 {
        Ok(())
    } else if command[8] == 0x53
           && command[9] == 0x54
           && command[10] == 0x4F
           && command[11] == 0x50 {
        Ok(())
    } else {
        Err(())
    }
}

// Simple panic handler
#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
