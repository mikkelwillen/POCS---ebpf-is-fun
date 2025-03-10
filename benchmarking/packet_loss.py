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
NUMBER_OF_PACKETS = [100, 1000] #[100, 1000, 10000, 100000, 1000000, 10000000]
RUST_SERVERS = ["simple-socket-filter"] # "valid-command", "robust-valid-command"]
HASKELL_SERVERS = ["plain-server", "socketfilter-server"]

# Paths
PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
BENCHMARK_RUST_RUST = os.path.join(PROJECT_ROOT, "benchmarking", "packet_loss_rust_rust.log")
BENCHMARK_RUST_HASKELL = os.path.join(PROJECT_ROOT, "benchmarking", "packet_loss_rust_haskell.log")
BENCHMARK_HASKELL_HASKELL = os.path.join(PROJECT_ROOT, "benchmarking", "packet_loss_haskell_haskell.log")
BENCHMARK_HASKELL_RUST = os.path.join(PROJECT_ROOT, "benchmarking", "packet_loss_haskell_rust.log")

def rust_server_rust_client(rust_servers=RUST_SERVERS, percents=PERCENTS, number_of_packets=NUMBER_OF_PACKETS):
    """Runs the Rust server and client, logging packet loss data."""

    # Open and clear the log file
    with open(BENCHMARK_RUST_RUST, "w") as f:
        f.write("Packet Loss\n")
        f.write("=" * 50 + "\n")

    for filter_name in rust_servers:

        subprocess.run(
            ["make", "clean"],
            cwd=os.path.join(PROJECT_ROOT, "aya"),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )

        print("Building the server. Please wait")
        # Build the server
        subprocess.run(
            ["make", "build", f"filter={filter_name}"],
            cwd=os.path.join(PROJECT_ROOT, "aya"),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )

        # time.sleep(30)
        for percent, packets in itertools.product(percents, number_of_packets):

            packet_sum = 0
            no_responses = 0

            for i in range(NUM_RUNS):

                # Start the server instance
                server_process = subprocess.Popen(
                    ["make", "run", f"filter={filter_name}"],
                    cwd=os.path.join(PROJECT_ROOT, "aya"),
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL
                )

                # Allow some time for the server to start
                time.sleep(1)

                print(f"Running client with percent={percent} and packets={packets}\n")

                # Run the client instance
                client_process = subprocess.run(
                    ["cargo", "run", "--", "-v", "-p", str(percent), "-n", str(packets), "-b", "frey"],
                    cwd=os.path.join(PROJECT_ROOT, "rust-client"),
                    capture_output=True
                )

                # Store client output
                client_output = client_process.stdout.decode()

                # print(client_output)

                for line in client_output.splitlines():
                    match = re.search(r"Received response\s*(\d+)", line)
                    if match:
                        packet_sum += int(match.group(1))
                    elif "No response from the server" in line:
                        no_responses += 1

                # Stop the server
                server_process.terminate()
                try:
                    server_process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    server_process.kill()

            # Write to log file
            average_received = packet_sum/(NUM_RUNS - no_responses) if (NUM_RUNS - no_responses) > 0 else 0
            expected_good_packets = int(packets * (1 - percent / 100))
            packet_loss = expected_good_packets - average_received
            packet_loss_percentage = (packet_loss / expected_good_packets) * 100 if expected_good_packets > 0 else 0
            runs_with_response = NUM_RUNS - no_responses if (NUM_RUNS - no_responses) > 0 else 0

            print(f"average received: {average_received}\n")
            with open(BENCHMARK_RUST_RUST, "a") as f:
                f.write(f"Filter: {filter_name}\n")
                f.write(f"Percent: {percent}\n")
                f.write(f"Packets: {packets}\n")
                f.write(f"Runs that received response: {runs_with_response}\n")
                f.write(f"Average packets received: {average_received}\n")
                f.write(f"Average packet loss percentage: {packet_loss_percentage}\n")
                f.write("-" * 50 + "\n")

def rust_server_haskell_client(rust_servers=RUST_SERVERS, percents=PERCENTS, number_of_packets=NUMBER_OF_PACKETS):
    """Runs the Rust server and client, logging packet loss data."""

    # Open and clear the log file
    with open(BENCHMARK_RUST_HASKELL, "w") as f:
        f.write("Packet Loss\n")
        f.write("=" * 50 + "\n")

    for filter_name in rust_servers:

        subprocess.run(
            ["make", "clean"],
            cwd=os.path.join(PROJECT_ROOT, "aya"),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )

        print("Building the server. Please wait")
        # Build the server
        subprocess.run(
            ["make", "build", f"filter={filter_name}"],
            cwd=os.path.join(PROJECT_ROOT, "aya"),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )

        for percent, packets in itertools.product(percents, number_of_packets):

            packet_sum = 0
            no_responses = 0

            for i in range(NUM_RUNS):

                # Start the server instance
                server_process = subprocess.Popen(
                    ["make", "run", f"filter={filter_name}"],
                    cwd=os.path.join(PROJECT_ROOT, "aya"),
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL
                )

                # Allow some time for the server to start
                time.sleep(1)

                print(f"Running client with percent={percent} and packets={packets}\n")

                # Run the client instance
                client_process = subprocess.run(
                    ["./test-client", "-v", "-p", str(percent), "-c", str(packets), "-b", "frey2"],
                    cwd=os.path.join(PROJECT_ROOT, "funebpf/server-lib"),
                    capture_output=True
                )

                # Store client output
                client_output = client_process.stdout.decode()

                # print(client_output)

                for line in client_output.splitlines():
                    match = re.search(r'Server response:\s*"(\d+)"', line)
                    if match:
                        packet_sum += int(match.group(1))
                    elif "No response from the server" in line:
                        no_responses += 1

                # Stop the server
                server_process.terminate()
                try:
                    server_process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    server_process.kill()

            # Write to log file
            average_received = packet_sum/(NUM_RUNS - no_responses) if (NUM_RUNS - no_responses) > 0 else 0
            expected_good_packets = int(packets * (1 - percent / 100))
            packet_loss = expected_good_packets - average_received
            packet_loss_percentage = (packet_loss / expected_good_packets) * 100 if expected_good_packets > 0 else 0
            runs_with_response = NUM_RUNS - no_responses if (NUM_RUNS - no_responses) > 0 else 0

            print(f"average received: {average_received}\n")
            with open(BENCHMARK_RUST_HASKELL, "a") as f:
                f.write(f"Filter: {filter_name}\n")
                f.write(f"Percent: {percent}\n")
                f.write(f"Packets: {packets}\n")
                f.write(f"Runs that received response: {runs_with_response}\n")
                f.write(f"Average packets received: {average_received}\n")
                f.write(f"Average packet loss percentage: {packet_loss_percentage}\n")
                f.write("-" * 50 + "\n")

def haskell_server_haskell_client(haskell_servers=HASKELL_SERVERS, percents=PERCENTS, number_of_packets=NUMBER_OF_PACKETS):
    """Runs the Haskell server and client, logging packet loss data."""

    # Open and clear the log file
    with open(BENCHMARK_HASKELL_RUST, "w") as f:
        f.write("Packet Loss\n")
        f.write("=" * 50 + "\n")

    # subprocess.run(
    #     ["make", "all"],
    #     cwd=os.path.join(PROJECT_ROOT, "funebpf/server-lib"),
    #     stdout=subprocess.DEVNULL,
    #     stderr=subprocess.DEVNULL
    # )

    for filter_name in haskell_servers:

        for percent, packets in itertools.product(percents, number_of_packets):

            packet_sum = 0
            no_responses = 0

            for i in range(NUM_RUNS):

                # Start the server instance
                server_process = subprocess.Popen(
                    [f"./{filter_name}"],
                    cwd=os.path.join(PROJECT_ROOT, "funebpf/server-lib"),
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL
                )

                print(f"Running client with percent={percent} and packets={packets}\n")

                # Run the client instance
                client_process = subprocess.run(
                    ["./test-client", "-v", "-p", str(percent), "-c", str(packets), "-b", "frey2"],
                    cwd=os.path.join(PROJECT_ROOT, "funebpf/server-lib"),
                    capture_output=True
                )

                # Store client output
                client_output = client_process.stdout.decode()

                # print(client_output")

                for line in client_output.splitlines():
                    match = re.search(r'Server response:\s*"(\d+)"', line)
                    if match:
                        packet_sum += int(match.group(1))
                    elif "No response from the server" in line:
                        no_responses += 1

                # Stop the server
                server_process.terminate()
                try:
                    server_process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    server_process.kill()

            # Write to log file
            average_received = packet_sum/(NUM_RUNS - no_responses) if (NUM_RUNS - no_responses) > 0 else 0
            expected_good_packets = int(packets * (1 - percent / 100))
            packet_loss = expected_good_packets - average_received
            packet_loss_percentage = (packet_loss / expected_good_packets) * 100 if expected_good_packets > 0 else 0
            runs_with_response = NUM_RUNS - no_responses if (NUM_RUNS - no_responses) > 0 else 0

            print(f"average received: {average_received}\n")
            with open(BENCHMARK_HASKELL_HASKELL, "a") as f:
                f.write(f"Filter: {filter_name}\n")
                f.write(f"Percent: {percent}\n")
                f.write(f"Packets: {packets}\n")
                f.write(f"Runs that received response: {runs_with_response}\n")
                f.write(f"Average packets received: {average_received}\n")
                f.write(f"Average packet loss percentage: {packet_loss_percentage}\n")
                f.write("-" * 50 + "\n")


def haskell_server_rust_client(haskell_servers=HASKELL_SERVERS, percents=PERCENTS, number_of_packets=NUMBER_OF_PACKETS):
    """Runs the Haskell server and client, logging packet loss data."""

    # Open and clear the log file
    with open(BENCHMARK_HASKELL_RUST, "w") as f:
        f.write("Packet Loss\n")
        f.write("=" * 50 + "\n")

    # subprocess.run(
    #     ["make", "all"],
    #     cwd=os.path.join(PROJECT_ROOT, "funebpf/server-lib"),
    #     stdout=subprocess.DEVNULL,
    #     stderr=subprocess.DEVNULL
    # )

    for filter_name in haskell_servers:

        for percent, packets in itertools.product(percents, number_of_packets):

            packet_sum = 0
            no_responses = 0

            for i in range(NUM_RUNS):

                # Start the server instance
                server_process = subprocess.Popen(
                    [f"./{filter_name}"],
                    cwd=os.path.join(PROJECT_ROOT, "funebpf/server-lib"),
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL
                )

                print(f"Running client with percent={percent} and packets={packets}\n")

                # Run the client instance
                client_process = subprocess.run(
                    ["cargo", "run", "--", "-v", "-p", str(percent), "-n", str(packets), "-b", "frey"],
                    cwd=os.path.join(PROJECT_ROOT, "rust-client"),
                    capture_output=True
                )

                # Store client output
                client_output = client_process.stdout.decode()

                # print(client_output)

                for line in client_output.splitlines():
                    match = re.search(r"Received response\s*(\d+)", line)
                    if match:
                        packet_sum += int(match.group(1))
                    elif "No response from the server" in line:
                        no_responses += 1

                # Stop the server
                server_process.terminate()
                try:
                    server_process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    server_process.kill()

            # Write to log file
            average_received = packet_sum/(NUM_RUNS - no_responses) if (NUM_RUNS - no_responses) > 0 else 0
            expected_good_packets = int(packets * (1 - percent / 100))
            packet_loss = expected_good_packets - average_received
            packet_loss_percentage = (packet_loss / expected_good_packets) * 100 if expected_good_packets > 0 else 0
            runs_with_response = NUM_RUNS - no_responses if (NUM_RUNS - no_responses) > 0 else 0

            print(f"average received: {average_received}\n")
            with open(BENCHMARK_HASKELL_RUST, "a") as f:
                f.write(f"Filter: {filter_name}\n")
                f.write(f"Percent: {percent}\n")
                f.write(f"Packets: {packets}\n")
                f.write(f"Runs that received response: {runs_with_response}\n")
                f.write(f"Average packets received: {average_received}\n")
                f.write(f"Average packet loss percentage: {packet_loss_percentage}\n")
                f.write("-" * 50 + "\n")


def parse_and_generate_packet_loss_table(log_file):
    """
    Parses the log file, extracts packet loss data, and generates a formatted table.
    The table displays packet loss percentage and responding runs with Percent Bad (Y-Axis) and Packets Sent (X-Axis).
    """
    packet_loss_data = []

    # Read and parse log file
    with open(log_file, "r") as f:
        lines = f.readlines()

    filter_name, percent, responding_runs, packets, packet_loss_percentage = None, None, None, None, None

    for line in lines:
        if "Filter:" in line:
            filter_name = line.split(":")[1].strip()
        elif "Percent:" in line:
            percent = int(line.split(":")[1].strip())
        elif "Packets:" in line:
            packets = int(line.split(":")[1].strip())
        elif "Runs that received response:" in line:
            responding_runs = int(line.split(":")[1].strip())
        elif "Average packet loss percentage:" in line:
            match = re.search(r"[-+]?\d*\.\d+|\d+", line.split(":")[1].strip())
            if match:
                packet_loss_percentage = float(match.group())

        if filter_name and percent is not None and packets is not None and responding_runs is not None and packet_loss_percentage is not None:
            packet_loss_data.append({
                "Filter": filter_name,
                "Percent Bad": percent,
                "Responding Runs": responding_runs,
                "Packets Sent": packets,
                "Packet Loss %": packet_loss_percentage
            })
            filter_name, percent, responding_runs, packets, packet_loss_percentage = None, None, None, None, None

    # Process data into a table
    percent_values = sorted(set(entry["Percent Bad"] for entry in packet_loss_data))
    packet_counts = sorted(set(entry["Packets Sent"] for entry in packet_loss_data))
    filter_names = sorted(set(entry["Filter"] for entry in packet_loss_data))

    tables = {}
    for filter_name in filter_names:
        # Initialize table structure
        table_data = {packet: [] for packet in packet_counts}
        for percent in percent_values:
            row = []
            for packet in packet_counts:
                # Get corresponding packet loss percentage and responding runs
                matching_entry = next(
                    (entry for entry in packet_loss_data if entry["Filter"] == filter_name
                     and entry["Percent Bad"] == percent and entry["Packets Sent"] == packet),
                    None
                )
                if matching_entry:
                    formatted_value = f"{matching_entry['Packet Loss %']}% ({matching_entry['Responding Runs']})"
                else:
                    formatted_value = "-"

                row.append(formatted_value)

            for i, packet in enumerate(packet_counts):
                table_data[packet].append(row[i])

        # Convert to DataFrame
        df = pd.DataFrame(table_data, index=percent_values)
        df.index.name = "Percent"
        tables[filter_name] = df

    return tables

if __name__ == "__main__":
    # Run rust/rust tests
    rust_server_rust_client()
    tables = parse_and_generate_packet_loss_table(BENCHMARK_RUST_RUST)
    for filter_name, df in tables.items():
        print(f"\nPacket Loss Table for {filter_name}")
        print(df.to_string())

    # Run rust/haskell tests
    rust_server_haskell_client()
    tables = parse_and_generate_packet_loss_table(BENCHMARK_RUST_HASKELL)
    for filter_name, df in tables.items():
        print(f"\nPacket Loss Table for {filter_name}")
        print(df.to_string())

    # Run haskell/haskell tests
    haskell_server_haskell_client()
    tables = parse_and_generate_packet_loss_table(BENCHMARK_HASKELL_HASKELL)
    for filter_name, df in tables.items():
        print(f"\nPacket Loss Table for {filter_name}")
        print(df.to_string())

    # Run haskell/rust tests
    haskell_server_rust_client()
    tables = parse_and_generate_packet_loss_table(BENCHMARK_HASKELL_RUST)
    for filter_name, df in tables.items():
        print(f"\nPacket Loss Table for {filter_name}")
        print(df.to_string())
