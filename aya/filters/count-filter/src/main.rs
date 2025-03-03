#![no_std]
#![no_main]

// Import the `socket_filter` macro used to specify the entry point of the eBPF program
// Import the `SkBuffContext` struct with metadata and payload
use aya_ebpf::{
    macros::{ socket_filter
            , map},
    programs::SkBuffContext,
    maps::PerCpuArray,
};

const CPU_CORES: u32 = 16;

#[map(name = "counter")]
static mut PACKET_COUNTER: PerCpuArray<u32> = PerCpuArray::with_max_entries(CPU_CORES, 0);

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
        let counter = PACKET_COUNTER.get_ptr_mut(0).ok_or(())?;
        *counter += 1;
    }
    Ok(())
}

// Simple panic handler
#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { core::hint::unreachable_unchecked() }
}
