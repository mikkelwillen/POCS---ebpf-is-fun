#!/usr/bin/env python3
import subprocess
import itertools
import time
import os
import re
import pandas as pd

# Constants
NUM_RUNS = 2

# Iterators
PERCENTS = [1, 25, 50, 75, 99]
NUMBER_OF_PACKETS = [100, 1000, 10000, 100000, 1000000,10000000]
SERVERS = ["simple-socket-filter", "valid-command"]

# Paths
PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
THROUGHPUT = os.path.join(PROJECT_ROOT, "experiments/throughput/throughput.log")

def build_project():
    """Build the project"""
    subprocess.run(
        ["make", "clean"],
        cwd=os.path.join(PROJECT_ROOT, "aya"),
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )

    subprocess.run(
        ["make", "build-release"],
        cwd=os.path.join(PROJECT_ROOT, "aya"),
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )

    subprocess.run(
        ["cargo", "build"],
        cwd=os.path.join(PROJECT_ROOT, "rust-client"),
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )

def run_test(servers=SERVERS, percents=PERCENTS, num_packets=NUMBER_OF_PACKETS):
    """Run the server and client, logging the wall-clock time"""

    # Open and clear the log file
    with open(THROUGHPUT, "w") as f:
        f.write("THROUGHPUT TEST\n")
        f.write("=" * 50 + "\n")

    for filter_name, percent, packets in itertools.product(servers, percents, num_packets):
        aggregate_time = 0

        print(f"Running tests for filter: {filter_name}:")
        print(f"Running client with percent={percent} and packets={packets}, {NUM_RUNS} times")
        for i in range(NUM_RUNS):

            # Start the server instance and time it
            server_process = subprocess.Popen(
                ["time", "sudo", f"./{filter_name}/release/simple-server"],
                cwd=os.path.join(PROJECT_ROOT, "aya"),
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )

            # Run the client instance
            client_process = subprocess.run(
                ["./udp_client", "-p", str(percent), "-n", str(packets), "-b", "frey"],
                cwd=os.path.join(PROJECT_ROOT, "rust-client/target/release"),
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )

            # Store the output of the server
            server_process.wait()

            # Parse the output of the server
            output, error = server_process.communicate()

            # Extract the time from the output
            match = re.search(r'(\d+:\d+\.\d+)elapsed', error.decode())
            if match:
                elapsed_time = match.group(1)

            minutes, sec = elapsed_time.split(':')
            total_seconds = int(minutes) * 60 + float(sec)

            aggregate_time += total_seconds

        # Write to log file
        average_time = aggregate_time / NUM_RUNS

        print(f"Average time: {average_time}\n")
        with open(THROUGHPUT, "a") as f:
            f.write(f"Filter: {filter_name}\n")
            f.write(f"Percent: {percent}\n")
            f.write(f"Packets: {packets}\n")
            f.write(f"Average time: {average_time}\n")
            f.write(f"-" * 50 + "\n")

if __name__ == "__main__":
    run_test()
