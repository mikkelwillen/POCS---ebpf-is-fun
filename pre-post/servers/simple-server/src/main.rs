use server_lib::run_server;

fn main() -> anyhow::Result<()> {
    // Define only the pre and post hooks specific to this server instance.
    let pre_hook = || {
        println!("Pre hook: Initializing additional resources...");
        // Additional instance-specific pre-processing can go here.
    };

    let post_hook = || {
        println!("Post hook: Cleaning up instance-specific resources...");
        // Additional instance-specific post-processing can go here.
    };

    run_server(pre_hook, post_hook)
}
