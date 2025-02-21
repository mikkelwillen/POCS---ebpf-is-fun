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
    // Get package length
    let len = ctx.len();
    if len == 0 {
        return Err(());
    }

    // Buffer to store the package
    let mut command: [u8; 6] = [0; 6];

    // Copy the command to the buffer
    ctx.load_bytes(8, &mut command[..6 as usize]).map_err(|_| ())?;

    // Check if the command is valid
    // match command {
    //     [0x50, 0x55, 0x54, _, _ ,_ ] =>
    //         Ok(len.into()),
    //     _ =>
    //         Err(())
    // }
    Ok(len.into())
}

// Simple panic handler
#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
