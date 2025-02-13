#![no_std]
#![no_main]

use aya_ebpf::{macros::socket_filter, programs::SkBuffContext};

#[socket_filter]
pub fn socket_filter(_ctx: SkBuffContext) -> i64 {
    match try_socket_filter(_ctx) {
        0 => -1,
        _ => -1,
    }
}

fn try_socket_filter(_ctx: SkBuffContext) -> i64 {
    1
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
