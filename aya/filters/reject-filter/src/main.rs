#![no_std]
#![no_main]

// Import the `socket_filter` macro used to specify the entry point of the eBPF program
// Import the `SkBuffContext` struct with metadata and payload
use aya_ebpf::{
    macros::socket_filter,
    programs::SkBuffContext
};

// Filter that lest no packets through
#[socket_filter]
pub fn socket_filter(_ctx: SkBuffContext) -> i64 {
    0
}

// Simple panic handler
#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
