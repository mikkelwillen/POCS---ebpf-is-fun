#![no_std]
#![no_main]

use aya_ebpf::{
    macros::socket_filter,
    bpf_printk,
    programs::SkBuffContext
};

#[socket_filter]
pub fn socket_filter(ctx: SkBuffContext) -> i64 {
    match try_socket_filter(ctx) {
        Ok(_) => -1, // Allow packet through
        Err(_) => 0, // Drop packet (error case)
    }
}

// Helper function
fn try_socket_filter(ctx: SkBuffContext) -> Result<(), ()> {
    let pkt_len = ctx.len(); // Get actual packet length
    if pkt_len == 0 {
        return Err(()); // Avoid zero-length reads
    }

    let read_len = core::cmp::min(pkt_len, 100); // Ensure we don't read past packet bounds
    let mut temp_buf: [u8; 100] = [0; 100]; // Use a smaller buffer to fit the verifier's constraints

    // Load only available bytes, up to 500 bytes max
    ctx.load_bytes(0, &mut temp_buf[..read_len as usize])
        .map_err(|_| ())?;

    unsafe {
        for i in 0..read_len {
            bpf_printk!(b"Byte %u: %hhx", i, temp_buf[i as usize]);
        }
    }

    Ok(())
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
