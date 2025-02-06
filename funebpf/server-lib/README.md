server-lib 
==========
A library implementing a simple bag server and client.

Purpose
-------
Initial purpose was to create a use-case for ebpf, and to investigate whether moving part of a web-servers behavior to kernel-space via ebpf can result in any advantageous changes in performance (ressources use, such as time, memory, energy).

Hypothesis 
----------
The hypothesis is that we can improve performance by moving parsing of invalid messages into kernel-space.
Here, performance is /time/ and /memory/, but could also be /energy consumption/.


Contents
--------
This server-lib consists of:
# The server library
which defines how a server sends and receives packets on a socket.
It has the following methods:
- `defaultMain` which is the method for creating a server, that takes a pre and post function, and starts the server.
  The `pre` and `post` defines what should happen before the server is started, and after the server is shut down.
- `serve` which receives requests on a given socket until the server is shut down. 
  The request is parsed by `parseMessage`, and is then handled accordingly:
  If the message is `Stop`, it stops the server.
  If it is `Nothing`, it causes the server to loop again without any action.
  Otherwise it is a message `msg` fs, which is then processed, and the state is updated.
- `processMessage` which parses the possible server messages (other than `Stop`).
  A message can be either `Put` a `multiplicity` to a `value`, `Get` a `value`, or `Delete` a `value`.
  `Put` only puts if the action does not make the bag server exceed the cap on values.
  `Get` only gets if the value is present in the bag server.
- `parseMessage` which parses a request (received on a socket) into a message (to be handled by `processMessage`).

# The client 
runs a UDP client with a specified behavior.
The behavior determines which requests are sent to the server.
Eg. the `odin` client sends (twice) a number of `Put` requests and a `Get` request, before sending a `Stop` request.
The `loki` client does the same, but also sends a large amount of invalid requests.
When running these two clients with a plain server, we see that the `loki` client uses far more resources than the `odin` client, even though the output of the server is the same(#).

# Multiple versions of server
The directory contains a number of sub-directories, defining versions of the server.
The `plain` directory defines a plain server that has no additional actions than those of the default server.
The `snoop` directory defines a server version that uses ebpf to "snoop" on the requests that are sent, recording the number of requests.
When running the `odin` and `loki` client with this server instance we see how the `loki` client sends a magnitude of requests more than the `odin` client.
The `socketfilter` directory defines a server instance that filters invalid requests in the kernel, which means that this work will not happen in the Haskell program.
When running the `odin` and `loki` clients using this server instance, the resource use should be approximately the same.

There is a number of other server instances, feel free to explore ;)


Building and installing with Cabal
----------------------------------
The project can be built and installed with cabal by navigating to the `functional-ebpf/code/server-lib` folder and executing the following command:

```
make all
```
which builds and installs the server-lib target, and places the executables in the server-lib folder.


Running examples
----------------
To run a server and client, I usually run the server in one terminal window, and the client in another. I have experimented with running them in the same window, but many times weird things happened and I haven't taken the time to figure out why.

# Vagrant
I always run the experiments in Vagrant, just in case. Would be bad to mess with the kernel.

``` sh
vagrant status           # lets you know if your Vagrant image is already up and running 
vagrant up               # for starting it if it is down 
vagrant ssh              # for ssh'ing into the Vagrant image 
```
Remember that when entering the image you need to navigate into '/vagrant' to see your files.

# Running the server 
The following comman will run the plain server instance in a verbose fashion.
``` sh
./plain-server -v
```
The other instances are run similarly.

# Running the client 
To run the client use the following command (again in a verbose fashion):
``` sh
./client -b <BEHAVIOR> -v
```
The behavior can be eg "thor", "odin" or "loki". Take a look at the 'client/Main.hs' for more options (though I would reccommend starting with these three).
Some of the behaviors need additional commandline arguments, if you call them withinsufficient argmumnents they will let you know.

