#!/usr/bin/env python3
import subprocess
import itertools
import time
import os
import re
import pandas as pd


# Paths
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
benchmark_log = os.path.join(project_root, "benchmarking", "packet_loss.log")

# Iterators
percents = [1, 25, 50, 75, 99]
number_of_packets = [100, 1000] #[100, 1000, 10000, 100000, 1000000, 10000000]
haskell_servers = ["./plain-server", "./socketfilter-server"]
rust_servers = ["simple-socket-filter", "valid-command", "robust-valid-command"]

# Open and clear the log file
with open(benchmark_log, "w") as f:
    f.write("Packet Loss\n")
    f.write("=" * 50 + "\n")

def rust_server_rust_client(rust_servers=rust_servers, percents=percents, number_of_packets=number_of_packets):
    """Runs the Rust server and client, logging packet loss data."""
    for filter_name in rust_servers:

        subprocess.run(
            ["make", "clean"],
            cwd=os.path.join(project_root, "aya"),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )

        print("Building the server. Please wait")
        # Build the server
        subprocess.run(
            ["make", "build", f"filter={filter_name}"],
            cwd=os.path.join(project_root, "aya"),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )

        # time.sleep(30)

        for percent, packets in itertools.product(percents, number_of_packets):

            # Start the server instance
            server_process = subprocess.Popen(
                ["make", "run", f"filter={filter_name}"],
                cwd=os.path.join(project_root, "aya"),
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )

            # Allow some time for the server to start
            time.sleep(1)

            print(f"Running client with percent={percent} and packets={packets}\n")

            # Run the client instance
            client_process = subprocess.run(
                ["cargo", "run", "--", "-v", "-p", str(percent), "-n", str(packets), "-b", "frey"],
                cwd=os.path.join(project_root, "rust-client"),
                capture_output=True
            )

            # Store client output
            client_output = client_process.stdout.decode()

            # Write to log file
            with open(benchmark_log, "a") as f:
                f.write(f"Filter: {filter_name}\n")
                f.write(f"Percent: {percent}\n")
                f.write(f"Packets: {packets}\n")
                f.write(client_output + "\n")
                f.write("-" * 50 + "\n")

            # print(client_output)

            # Stop the server
            server_process.terminate()
            try:
                server_process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                server_process.kill()
    
def parse_received_responses():
    """Parses the log file to extract 'Received response' values as integers."""
    received_responses = []

    with open(benchmark_log, "r") as f:
        for line in f:
            match = re.search(r"Received response:\s*(\d+)", line)
            if match:
                received_responses.append(int(match.group(1)))
            elif "No response from the server" in line:
                received_responses.append(-1)

    return received_responses

def parse_packet_loss():
    """Parses the log file to calculate packet loss percentage."""
    losses = []
    with open(benchmark_log, "r") as f:
        lines = f.readlines()

    filter_name, percent, packets, received = None, None, None, None

    for line in lines:
        if "Filter:" in line:
            filter_name = line.split(":")[1].strip()
        elif "Percent:" in line:
            percent = int(line.split(":")[1].strip())
        elif "Packets:" in line:
            packets = int(line.split(":")[1].strip())
        elif "Received response:" in line:
            received = int(line.split(":")[1].strip())
        elif "No response from the server" in line:
            received = -1

        if filter_name and percent is not None and packets is not None and received is not None:
            # Calculate expected good packets
            expected_good_packets = int(packets * (1 - percent / 100))
            packet_loss = expected_good_packets - received
            packet_loss_percentage = (packet_loss / expected_good_packets) * 100 if expected_good_packets > 0 else 0

            losses.append({
                "Filter": filter_name,
                "Percent Bad": percent,
                "Packets Sent": packets,
                "Expected Good Packets": expected_good_packets,
                "Received Packets": received,
                "Packets Lost": packet_loss,
                "Packet Loss %": round(packet_loss_percentage, 2)
            })

            # Reset values for next entry
            filter_name, percent, packets, received = None, None, None, None

    return losses

def generate_packet_loss_table(packet_loss_data):
    """Generates and prints a formatted table of packet loss percentages."""
    # Extract unique percent values and packet counts
    percent_values = sorted(set(entry["Percent Bad"] for entry in packet_loss_data))
    packet_counts = sorted(set(entry["Packets Sent"] for entry in packet_loss_data))
    filter_names = sorted(set(entry["Filter"] for entry in packet_loss_data))

    tables = {}

    for filter_name in filter_names:
        # Create a dictionary for storing packet loss percentages
        table_data = {packet: [] for packet in packet_counts}

        for percent in percent_values:
            row = []
            for packet in packet_counts:
                # Find the corresponding packet loss percentage
                matching_entry = next(
                    (entry for entry in packet_loss_data if entry["Filter"] == filter_name
                     and entry["Percent Bad"] == percent and entry["Packets Sent"] == packet),
                    None
                )
                row.append(matching_entry["Packet Loss %"] if matching_entry else "-")

            for i, packet in enumerate(packet_counts):
                table_data[packet].append(row[i])

        # Convert dictionary to DataFrame
        df = pd.DataFrame(table_data, index=percent_values)
        df.index.name = "Percent"

        tables[filter_name] = df

    return tables

if __name__ == "__main__":
    # Run tests
    rust_server_rust_client()
    print("All tests completed")

    # Parse and print results
    responses = parse_received_responses()
    print("Received Responses:", responses)

    # Calculate packet loss
    losses = parse_packet_loss()
    for loss in losses:
        print(loss)

    # Display tables
    tables = generate_packet_loss_table(losses)
    for filter_name, df in tables.items():
        print(f"\nPacket Loss Table for {filter_name}")
        print(df.to_string())
