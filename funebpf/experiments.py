#!/usr/bin/env python3
import re
import os

def print_begin_test(test, server):
    print("=".center(65, '='))
    print(f"Statistics for {test} ({server})".center(65, ' '))
    print("-".center(65, '-'))

def print_end_test():
    print("-".center(65, '-'))
    print(" ")


patterns = [
      "Percent of CPU this job got"
    , "Minor (reclaiming a frame) page faults"
    , "Voluntary context switches"
    , "Involuntary context switches"
    , "Bytes allocated in the heap"
    , "Bytes copied during GC"
    , "MiB total memory in use"
    ]


def handle_files(j, prefix, data):
    pre = ''.join([prefix, str(j)])

    for f in os.listdir(pre):
        filename = ''.join([pre, '/', f])
        try:
            with open(filename, 'r') as file:
                lines = file.readlines()

            for line in lines:
                handle_line(j, line, data)

        except FileNotFoundError:
            print(f"Error: The file '{filename}' was not found.")

def handle_line(j, line, data):
    value = None
    # extract numeric value
    time = re.search(r"[0-9]*\:[0-9][0-9]\.[0-9][0-9]", line)
    if time:
        ts = time.group(0).split(':')
        mins = float(ts[0]) * 60
        secs = float(ts[1])
        value = mins + secs

    else:
        res = re.search(r"([0-9]*\:[0-9][0-9]\.[0-9][0-9])|([-+]?[0-9]*\.?[0-9,\,]+)", line)
        if res:
            value = float(res.group(0).replace(',',''))

    if value != None:
        # determine where to save the value
        for p in patterns:
            if re.search(re.escape(p), line, re.IGNORECASE):
                if data.get((p, j)):
                    # print(data.get((p, j)))
                    data[(p, j)].append(value)
                    # print(data.get((p, j)))
                else:
                    data.update({(p, j) : [value]})
                    # print(data.get((p, j)))


def find_average(j, data, server):
    for p in patterns:
        try:
            values = data[(p, j)]  # should probably treat elapsed wall clock time etc differently?
            if len(values) != 0:
                # print(f"Pattern: {p}, server: {server}, percentage: {j}")
                # print(f"Values: {values}")
                # print(f"Sum: {sum(values)}")
                # print(f"Len: {len(values)}")
                average = sum(values) / len(values)
                print_data.update({(p,j, server) : average})
            else:
                print(f"Data length is zero: {p}")
        except KeyError:
            return()
            # print("Key doesn't exist")


def timing(server, data):
    test(server, data, "timing")

def memory(server, data):
    test(server, data, "memory")

def test(server, data, test):
    pre = ''.join(['data/', server, '/', test, '/'])
    if len(os.listdir(pre)) == 0:
        print(f"No {test} data for {server}")

    else:
        for j in [1,25,50,75,99]:
            handle_files(j, pre, data)
            find_average(j, data, server)



def print_results(servers):
    start = "Test".center(49, ' ')
    sep = 49

    for s in servers:
        start += s.rjust(18, ' ')
        sep += 18
    print(sep*"=")
    print(start)
    print(sep*"=")

    for p in patterns:
        for j in [1,25,50,75,99]:
            pstr = str(j).rjust(2,' ')
            string = f"{p.ljust(40, ' ')} ({pstr}%) : "
            for s in servers:
                v = print_data[p, j, s]
                if v != None:
                    astr = "{:,.2f}".format(v).rjust(18, ' ')
                    string += astr
            print(string)
        print(sep*"-")



# run for the server instances
print_data = dict()

server_instances = ['plain', 'socketfilter']
for server in server_instances:
    stat_data = dict()
    memory(server, stat_data)
    timing(server, stat_data)

print_results(server_instances)
