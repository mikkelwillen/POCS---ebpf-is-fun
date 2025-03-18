#!/usr/bin/env sh

logfile=throughput.txt

rm -f $logfile

for s in "simple-socket-filter" "valid-command" "robust-valid-command"
do
    echo "Running tests for $s-server"
    for p in 1 25 50 75 99
    do
		for n in 100 1000 10000 100000 1000000 10000000
		do
			for _i in $(seq 1 10)
			do
				echo "Running test for $s-server with $n commands and $p% packet loss"
				command time -a -v -o $logfile sudo ./../../aya/$s/release/simple-server >/dev/null & ./../../rust-client/target/release/udp_client -p $p -n $n -b frigg
			done
        done
    done
done
