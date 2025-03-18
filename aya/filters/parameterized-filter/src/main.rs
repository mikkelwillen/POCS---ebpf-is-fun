#![no_std]
#![no_main]

// Import the `socket_filter` macro used to specify the entry point of the eBPF program
// Import the `SkBuffContext` struct with metadata and payload
use aya_ebpf::{
    macros::socket_filter,
    macros::map,
    programs::SkBuffContext,
    maps::Array,
};

#[map(name = "parameter")]
static mut PARAMETER: Array<u32> = Array::with_max_entries(1, 0);

// Filter that lets all packets through
#[socket_filter]
pub fn socket_filter(ctx: SkBuffContext) -> i64 {
    match try_socket_filter(ctx) {
        Ok(_) => -1,
        Err(_) => 0
    }
}

fn try_socket_filter(ctx: SkBuffContext) -> Result<(), ()> {
    unsafe {
        let param = PARAMETER.get_ptr_mut(0).ok_or(())?;
        match *param {
            0 => Ok(()),
            _ => Err(()),
        }
    }
}
// Simple panic handler
#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { core::hint::unreachable_unchecked() }
}
