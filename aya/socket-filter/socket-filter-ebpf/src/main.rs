#![no_std]
#![no_main]

use aya_ebpf::{macros::socket_filter, programs::SkBuffContext};

#[socket_filter]
pub fn socket_filter(_ctx: SkBuffContext) -> i64 {
    0
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
