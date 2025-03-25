# Writing eBPF Programs with High-level Libraries: A Comparison of Aya and fun eBPF
A Project Outside Project Scope (PCOS) by Caroline Kierkeaard and Mikkel Willén

## Abstract 
This paper presents a comparative study of two high-level frameworks for developing eBPF code: Aya, implemented in Rust, and fun eBPF, implemented in Haskell. We developed an experimental setup with a Rust-based server and client to replicate and extend previous throughput experiments originally performed with fun eBPF. Our investigation focused on evaluating performance differences, particularly the impact of filtering in kernel space versus user space, and assessing the usability of specific library features. In addition to throughput measurements, we conducted packet loss experiments to address a potential threat to the validity of our performance tests. The results indicate that, while the Haskell implementation shows minor differences in packet loss between filtering modes, the Rust-based system maintained practically no packet loss. Furthermore, our analysis highlights a difference between ease of use and control. Aya offers higher-level abstractions that simplify stack management and map operations, whereas fun eBPF provides more control over low-level operations. Overall, this work provides insights into the differences in developing eBPF programs with these two libraries and suggests directions for future experiments to further expand the comparative analysis.

## Description of directories
* `/aya` contains the implementation of the Rust server and eBPF program written with Aya  
* `/experiments` contains the scripts and result from the experiments  
* `/funebpf` contains the implemenation of the Haskell server, client and eBPF program including our changes  
* `/handout-artifact` contains the uchanged artifact we were given by our supervisors  
* `/rust-client` contains the client written in Rust  

## How to run the code
### Run the server instance
To run the server instance, run the following command from the `aya` folder: 
``` sh
make run filter=<filter_name> server=<server_name> flags=<flags>
```
where `<filter_name>` is one of the following:
``` sh
simple-socket-filter
reject-filter
valid-command
robust-valid-command
parameterized-filter
print-filter
count-filter
```

and `<server_name>` is one of the following:
``` sh
simple-server
parameterized-server
print-server
count-server
ebpf-stats-server
```

`<flags>` are optional, and the follow are allowed
``` sh
--verbose -v
--capacity -c
```

### Build release version of the server
To build release files, run the follwing command from the `aya` folder:
``` sh
make build-release
```

These servers can then be run from their respective folders in the `aya` folder in `target/release`.

### Run the client instance
To run the client instance, run the following command from the `rust-client` folder:

``` sh
cargo run
```
with optional flags:
``` sh
--verbose -v
--behaviour -b
--percent -p
--number_of_packets -n
```
