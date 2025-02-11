#![no_std]
#![no_main]

use aya_ebpf::{macros::socket_filter, programs::SkBuffContext};
use aya_log_ebpf::info;

#[socket_filter]
pub fn socket_filter(_ctx: SkBuffContext) -> i64 {
    match try_socket_filter(_ctx) {
        0 => 0,
        _ => 1,
    }
}

fn try_socket_filter(_ctx: SkBuffContext) -> i64 {
    info!(&_ctx, "received packet");
    0
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
