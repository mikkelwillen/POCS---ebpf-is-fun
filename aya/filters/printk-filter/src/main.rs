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

    unsafe {
        bpf_printk!(b"dec: %d, 0x%08X", 42, 0x1234);
    }

    Ok(())
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
