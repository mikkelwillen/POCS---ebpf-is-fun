#!/usr/bin/env python3

import subprocess
import itertools
import time
import os


# Paths
PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
BENCHMARK_LOG = os.path.join(PROJECT_ROOT, "benchmarking", "packet_loss.log")

# Iterators
percent = [1, 25, 50, 75, 99]
number_of_packets = [100, 1000, 10000, 100000, 1000000, 10000000]
haskell_servers = ["./plain-server", "./socketfilter-server"]
rust_servers = ["simple-socket-filter", "valid-command", "robust-valid-command"]

# Open and clear the log file
with open(BENCHMARK_LOG, "w") as f:
    f.write("Packet Loss\n")
    f.write("=" * 50+ "\n")

def rust_server_rust_client():
    for filter_name, percent, packets in itertools.product(rust_servers, percent, number_of_packets):
        # Start the server instance
        server_process = subprocess.Popen(
            ["make", "run", f"filter={filter_name}"],
            cwd=os.path.join(PROJECT_ROOT, "aya"))

        # Allow some time for the server to start
        time.sleep(2)

        print(f"Running client with percent={percent} and packets={packets}")

        # Run the client instance
        client_process = subprocess.run(
            ["cargo", "run", "--", "-v", "-p", str(percent), "-n", str(packets), "-b", "frey"],
            cwd=os.path.join(PROJECT_ROOT, "rust-client"),
            capture_output=True)

        # Store cleint output
        client_output = client_process.stdout

        # Write to log file
        with open(BENCHMARK_LOG, "a") as f:
            f.write(f"Filter: {filter_name}\n")
            f.write(f"Percent: {percent}\n")
            f.write(f"Packets: {packets}\n")
            f.write(client_output.decode() + "\n")
            f.write("-" * 50 + "\n")

            print(client_output)


def parse_log():
    with open(BENCHMARK_LOG, "r") as f:
        print(f.read())
print("All tests completed")
