#!/usr/bin/env python3
import subprocess
import itertools
import time
import os
import re
import pandas as pd
import matplotlib.pyplot as plt

# Constants
NUM_RUNS = 2

# Iterators
PERCENTS = [75, 99]
# [1, 25, 50, 75, 99]
NUMBER_OF_PACKETS = [100, 1000, 10000, 100000, 1000000] #, 10000000]
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
            client_process = subprocess.Popen(
                ["./udp_client", "-p", str(percent), "-n", str(packets), "-b", "frigg"],
                cwd=os.path.join(PROJECT_ROOT, "rust-client/target/release"),
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )

            # Wait for the server to finish
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


def parse_log(log_file):
    """Parse the log file and return a pandas DataFrame"""
    with open(log_file, "r") as f:
        lines = f.readlines()

    data = []
    for i in range(2, len(lines), 5):
        filter_name = lines[i].split(": ")[1].strip()
        percent = lines[i + 1].split(": ")[1].strip()
        packets = lines[i + 2].split(": ")[1].strip()
        average_time = lines[i + 3].split(": ")[1].strip()

        data.append([filter_name, percent, packets, average_time])

    df = pd.DataFrame(data, columns=["Filter", "Percent", "Packets", "Average Time"])

    dfs = {}
    for percent in df['Percent'].unique():
        # Filter rows for the given percent.
        subset = df[df['Percent'] == percent]
        # Pivot so that rows = Filter, columns = Packets, and cell values = Average Time.
        pivot_df = subset.pivot(index='Packets', columns='Filter', values='Average Time')
        dfs[percent] = pivot_df

    # Display the pivoted DataFrames:
    for percent, pivot_df in dfs.items():
        print(f"\nDataFrame for Percent = {percent}:")
        print(pivot_df)

    return dfs

def plot_results(dfs):
    """Plot the results"""
    for percent, df in dfs.items():
        plt.figure()
        plt.plot(df['simple-socket-filter'], label='simple-socket-filter')
        plt.plot(df['valid-command'], label='valid-command')
        plt.xlabel("Number of Packets")
        plt.ylabel("Average Time (s)")
        plt.title(f"Average Time vs. Number of Packets for {percent}%")
        plt.legend()
        plt.grid(True)
        plt.show()

if __name__ == "__main__":
    # Run the tests
    run_test()

    # Parse the log file
    dfs = parse_log(THROUGHPUT)

    # Plot the results
    plot_results(dfs)
