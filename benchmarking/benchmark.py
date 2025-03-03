#!/usr/bin/env python3

import subprocess
import time
import csv
import os
import signal
import pandas as pd
import matplotlib.pyplot as plt

# Filters and commands
SERVER_PATH = "../servers"
CLIENT_PATH = "../../rust_client"

FILTERS = ["simple-socket-filter"
          , "valid-command"]

RUNS = 10
OUTPUT_CSV = "benchmark_results.csv"

def run_benchmark(filter_name):
    # Run the benchmark for the given filter
    # Figure out how to give sudo access to the server instance
    #
    return

def benchmark_filters():
    """Runs benchmarks for all filters and stores results."""
    all_results = []
    for filter_name in FILTERS:
        results = run_benchmark(filter_name)
        all_results.extend(results)

    # Save results
    save_results(all_results)
    print(f"Results saved to {OUTPUT_CSV}")

    # Generate graphs
    generate_graphs(OUTPUT_CSV)

    # Generate terminal output
    generate_output(filter_name, run, time)

def get_bpf_stats(pid):
    """Returns BPF stats for a given PID, by reading /proc filesystem."""
    try:
        with open(f"/proc/{pid}/bpf/stats", "r") as f:
            return f.read()
    except FileNotFoundError:
        return "No BPF stats available"

def save_results(results):
    """Saves benchmark results to a CSV file."""
    file_exists = os.path.isfile(OUTPUT_CSV)
    with open(OUTPUT_CSV, mode='a' if file_exists else 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=["Filter", "Run", "Time (s)"])
        if not file_exists:
            writer.writeheader()
        writer.writerows(results)

def generate_output(filter_name, run, time):
    """Pretty prints benchmark results."""
    print(f"Filter: {filter_name}")

def generate_graphs(csv_file):
    """Reads benchmark results from CSV and generates performance graphs."""
    df = pd.read_csv(csv_file)

    # Plot execution times
    plt.figure(figsize=(10, 5))
    for filter_name in df["Filter"].unique():
        subset = df[df["Filter"] == filter_name]
        plt.plot(subset["Run"], subset["Time (s)"], marker='o', linestyle='-', label=filter_name)

    plt.xlabel("Run Number")
    plt.ylabel("Processing Time (s)")
    plt.title("Filter Processing Time Comparison")
    plt.legend()
    plt.grid(True)
    plt.savefig("benchmark_results.png")
    plt.show()

if __name__ == "__main__":
    benchmark_filters()
