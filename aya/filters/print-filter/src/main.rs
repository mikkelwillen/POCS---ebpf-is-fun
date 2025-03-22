#![no_std]
#![no_main]

use aya_ebpf::{
    macros::socket_filter,
    macros::map,
    maps::PerCpuArray,
    programs::SkBuffContext
};

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Buf {
    pub buf: [u8; 500],  // Buffer to store packet data
}

#[map(name="PKT_PRT_ARRAY")]pub static mut BUF: PerCpuArray<Buf> = PerCpuArray::with_max_entries(1, 0);

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

    let read_len = core::cmp::min(pkt_len, 500); // Ensure we don't read past packet bounds
    let mut temp_buf: [u8; 500] = [0; 500]; // Use a smaller buffer to fit the verifier's constraints

    // Load only available bytes, up to 500 bytes max
    ctx.load_bytes(0, &mut temp_buf[..read_len as usize])
        .map_err(|_| ())?;

    // Retrieve the eBPF map entry
    let buf = unsafe {
        let ptr = BUF.get_ptr_mut(0).ok_or(())?;
        &mut *ptr
    };

    // Copy only the loaded portion into the eBPF map
    buf.buf[..read_len as usize].copy_from_slice(&temp_buf[..read_len as usize]);

    Ok(())
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
