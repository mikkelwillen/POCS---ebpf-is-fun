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
PERCENTS = [1] #, 25, 50, 75, 99]
NUMBER_OF_PACKETS = [100, 1000] #, 10000, 100000, 1000000, 10000000]
RUST_SERVERS = ["simple-socket-filter", "valid-command", "robust-valid-command"]
HASKELL_SERVERS = ["plain-server", "socketfilter-server"]

# Paths
PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
BENCHMARK_RUST_RUST = os.path.join(PROJECT_ROOT, "experiments/packet_loss", "packet_loss_rust_rust.log")
BENCHMARK_RUST_HASKELL = os.path.join(PROJECT_ROOT, "experiments/packet_loss", "packet_loss_rust_haskell.log")
BENCHMARK_HASKELL_HASKELL = os.path.join(PROJECT_ROOT, "experiments/packet_loss", "packet_loss_haskell_haskell.log")
BENCHMARK_HASKELL_RUST = os.path.join(PROJECT_ROOT, "experiments/packet_loss", "packet_loss_haskell_rust.log")

def build_project():
    """Builds the Rust and Haskell projects."""
    subprocess.run(
        ["sudo", "make", "clean"],
        cwd=os.path.join(PROJECT_ROOT, "funebpf/server-lib"),
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )

    subprocess.run(
        ["sudo", "make", "all"],
        cwd=os.path.join(PROJECT_ROOT, "funebpf/server-lib"),
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )

    subprocess.run(
        ["make", "clean"],
        cwd=os.path.join(PROJECT_ROOT, "aya"),
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )

    subprocess.run(
        ["make", "build-release"],
        cwd=os.path.join(PROJECT_ROOT, "aya"),
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )

def rust_server_rust_client(rust_servers=RUST_SERVERS, percents=PERCENTS, number_of_packets=NUMBER_OF_PACKETS):
    """Runs the Rust server and client, logging packet loss data."""

    # Open and clear the log file
    with open(BENCHMARK_RUST_RUST, "w") as f:
        f.write("Packet Loss\n")
        f.write("=" * 50 + "\n")

    for filter_name, percent, packets in itertools.product(rust_servers, percents, number_of_packets):
        packet_sum = 0
        no_responses = 0

        print(f"Running tests for filter: {filter_name}:")
        print(f"Running client with percent={percent} and packets={packets}, {NUM_RUNS} times")
        for i in range(NUM_RUNS):

            # Start the server instance
            server_process = subprocess.Popen(
                ["sudo", f"./{filter_name}/release/simple-server"],
                cwd=os.path.join(PROJECT_ROOT, "aya"),
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )

            # Allow some time for the server to start
            # time.sleep(0.5)

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
                match = re.search(r"Received response:\s*(\d+)", line)
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

    for filter_name, percent, packets in itertools.product(rust_servers, percents, number_of_packets):
        packet_sum = 0
        no_responses = 0

        print(f"Running tests for filter: {filter_name}:")
        print(f"Running client with percent={percent} and packets={packets}, {NUM_RUNS} times")
        for i in range(NUM_RUNS):

            # Start the server instance
            server_process = subprocess.Popen(
                ["sudo", f"./{filter_name}/release/simple-server"],
                cwd=os.path.join(PROJECT_ROOT, "aya"),
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )

            # Allow some time for the server to start
            # time.sleep(1)

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

    for filter_name, percent, packets in itertools.product(haskell_servers, percents, number_of_packets):
        packet_sum = 0
        no_responses = 0

        print(f"Running tests for filter: {filter_name}:")
        print(f"Running client with percent={percent} and packets={packets}, {NUM_RUNS} times")
        for i in range(NUM_RUNS):

            # Start the server instance
            server_process = subprocess.Popen(
                [f"./{filter_name}"],
                cwd=os.path.join(PROJECT_ROOT, "funebpf/server-lib"),
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )

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

    for filter_name, percent, packets in itertools.product(haskell_servers, percents, number_of_packets):
        packet_sum = 0
        no_responses = 0

        print(f"Running tests for filter: {filter_name}:")
        print(f"Running client with percent={percent} and packets={packets}, {NUM_RUNS} times")
        for i in range(NUM_RUNS):

            # Start the server instance
            server_process = subprocess.Popen(
                [f"./{filter_name}"],
                cwd=os.path.join(PROJECT_ROOT, "funebpf/server-lib"),
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )

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
                match = re.search(r"Received response:\s*(\d+)", line)
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

def generate_assembled_packet_loss_table(
    percent_values=PERCENTS,
    packet_values=[100, 1000, 10000, 100000, 1000000, 10000000]
):
    """
    Reads the four benchmark log files, aggregates the data for each combination,
    and generates one assembled table with the following columns:

      percent | server-client combination | <packet count columns>

    For each percent value, there will be one row per combination (in the order):
      Haskell/Haskell, Haskell/Rust, Rust/Rust, Rust/Haskell.

    Each cell displays the average packet loss percentage (formatted to 2 decimals)
    and the number of runs that received a response in parentheses.
    """
    import re
    import pandas as pd

    # Mapping from combination to its corresponding log file.
    log_files = {
        "Haskell/Haskell": BENCHMARK_HASKELL_HASKELL,
        "Haskell/Rust": BENCHMARK_HASKELL_RUST,
        "Rust/Rust": BENCHMARK_RUST_RUST,
        "Rust/Haskell": BENCHMARK_RUST_HASKELL,
    }

    # For each log file, parse the records.
    # Each record is expected to have the following fields:
    #   "Filter", "Percent", "Packets", "Runs that received response", "Average packet loss percentage"
    # We will aggregate by (Percent, Packets).
    combination_data = {}
    for combo, log_file in log_files.items():
        records = []
        with open(log_file, "r") as f:
            lines = f.readlines()
        current_record = {}
        for line in lines:
            line = line.strip()
            if line.startswith("Filter:"):
                current_record["Filter"] = line.split(":", 1)[1].strip()
            elif line.startswith("Percent:"):
                current_record["Percent"] = int(line.split(":", 1)[1].strip())
            elif line.startswith("Packets:"):
                current_record["Packets"] = int(line.split(":", 1)[1].strip())
            elif line.startswith("Runs that received response:"):
                current_record["Runs"] = int(line.split(":", 1)[1].strip())
            elif line.startswith("Average packet loss percentage:"):
                match = re.search(r"[-+]?\d*\.\d+|\d+", line.split(":", 1)[1].strip())
                if match:
                    current_record["Loss"] = float(match.group())
            # When all fields are present, store the record and reset.
            if ("Filter" in current_record and "Percent" in current_record and
                "Packets" in current_record and "Runs" in current_record and
                "Loss" in current_record):
                records.append(current_record)
                current_record = {}

        # Aggregate records for this combination by (Percent, Packets).
        agg = {}
        for rec in records:
            key = (rec["Percent"], rec["Packets"])
            if key not in agg:
                agg[key] = {"Loss": rec["Loss"], "Runs": rec["Runs"], "count": 1}
            else:
                agg[key]["Loss"] += rec["Loss"]
                agg[key]["Runs"] += rec["Runs"]
                agg[key]["count"] += 1
        # For each (percent, packets) key, compute average loss and runs.
        # Format as "XX.XX% (Y)".
        for key, val in agg.items():
            avg_loss = val["Loss"] / val["count"]
            avg_runs = val["Runs"] / val["count"]
            agg[key] = f"{avg_loss:.2f}% ({int(avg_runs)})"
        combination_data[combo] = agg

    # Build the assembled table.
    rows = []
    # We use the order below for the combinations.
    combo_order = ["Haskell/Haskell", "Haskell/Rust", "Rust/Rust", "Rust/Haskell"]
    for percent in percent_values:
        for combo in combo_order:
            row = {"percent": percent, "server-client combination": combo}
            for packet in packet_values:
                key = (percent, packet)
                cell = combination_data.get(combo, {}).get(key, "-")
                row[str(packet)] = cell
            rows.append(row)

    df = pd.DataFrame(rows)

    # (Optional) For visual clarity, you might want to only show the percent on the first row
    # of each group. Here we blank out duplicate percent values.
    df['percent'] = df.groupby('percent')['percent'].transform(
        lambda x: [x.iloc[0]] + [""] * (len(x) - 1)
    )

    return df

import matplotlib.pyplot as plt
import re

def plot_packet_loss_graphs(assembled_df):
    """
    Given an assembled packet loss table (DataFrame) with columns:
      - "percent" (the percent value),
      - "server-client combination" (one of the four combinations), and
      - one column per packet count (with cell values formatted as "XX.XX% (Y)")
    this function creates a graph for each unique percent value.

    In each graph:
      - The x-axis represents the number of packets.
      - The y-axis represents the packet loss percentage.
      - There are four lines (one per server-client combination).
    """
    # Before grouping by percent, fill any blank percent values (from the display transform)
    # so that every row has the correct percent.
    df = assembled_df.copy()
    df['percent'] = df['percent'].replace("", None)
    df['percent'] = df['percent'].ffill()  # fill down missing percent values

    # Identify packet count columns: those whose names are digits.
    packet_cols = [col for col in df.columns if col.isdigit()]
    packet_cols = sorted(packet_cols, key=int)

    # Get unique percent values (as they appear in the table)
    unique_percents = sorted(df['percent'].unique(), key=float)

    # Create one plot for each percent value.
    for pct in unique_percents:
        # Subset the DataFrame for this percent value.
        sub_df = df[df['percent'] == pct]
        plt.figure()
        for _, row in sub_df.iterrows():
            # For each combination, extract the numeric packet loss percentage values.
            x_vals = []
            y_vals = []
            for col in packet_cols:
                cell = row[col]
                # Expecting cell format "XX.XX% (Y)". If missing or "-", skip.
                if cell == "-" or cell.strip() == "":
                    continue
                match = re.search(r"([-+]?\d*\.\d+|\d+)%", cell)
                if match:
                    try:
                        y = float(match.group(1))
                    except ValueError:
                        continue
                    x_vals.append(int(col))
                    y_vals.append(y)
            if x_vals and y_vals:
                plt.plot(x_vals, y_vals, marker='o', label=row["server-client combination"])
        plt.xlabel("Number of Packets")
        plt.ylabel("Packet Loss Percentage")
        plt.title(f"Packet Loss Graph for Percent: {pct}")
        plt.legend()
        plt.grid(True)
        plt.ylim(0, 100)
        plt.tight_layout()
        plt.show()


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


    # Generate the assembled packet loss table
    assembled_df = generate_assembled_packet_loss_table()
    print("\nAssembled Packet Loss Table:")
    print(assembled_df.to_string(index=False))

    # Generate the packet loss graphs
    plot_packet_loss_graphs(assembled_df)
