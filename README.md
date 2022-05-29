Azul
----

A GUI + scripting environment intended for testing the Zephyr bluetooth stack.

Goals
-----

- Scan, Advertise, Connect
  - ADV data editor
- List display: RSSI, address, ID, flags, name
  - Treeview for attributes
- Periodic advertising
- Create, connect, disconnect, reconfigure L2CAP CoC channels
- GATT editor

UI decoupled from backend.
Event and command queues, with inspector.

Implementation
--------------

Host:
Structs are defined using CFFI, kept in sync with Zephyr sources.
Simple loop:
- Wait for events from backend
- Process events (GUI)
- Send command(s) to backend

Target:
- Receive command
- Copy data object(s) from rxbuf to heap
  - e.g. structs for objects, gatt char data etc..
  - transmitted in binary, TLVC
- Get encoded command (fixed-size buffer)
- Decode command, and call zephyr API with data from heap
- Send command result back 
- Once sending is over, free heap

Serializing: 
just serialize the (packed) struct in little endian, what could possibly go wrong.

Diagram
-------

``` 
|``````````````|  socket  |`````````|  socket  |```````|  UART  |`````|
| GUI / script |----------| Backend |----------| socat |--------| SoC |
|..............|          |.........|          |.......|        |.....|
```
 
Backends
--------

Backend listens locally on port.
Can attach GUI or script, or both.

Backend commands
----------------
TARGET
init
attach
reset

GAP
advertise
scan
connect
list-connections
disconnect

GATT
show-local-db
read-local
write-local
discover-db
show-remote-db
read-remote
write-remote
notify
subscribe

L2CAP
register
connect
reconfigure
disconnect

SMP
tbd

RPC
---

RPC over UART.
Logging over RTT.

Rough outline:
- list serial ports
  - possibly using `nrfjprog --com`
- start socat w/ selected port 
- exchange handshake
- listen for events from target
- listen for commands from host


HCI
---

HCI over UART.
Logging over RTT.

Links / Libraries
-----------------

- safe-queue (https://quickdocs.org/safe-queue)
- queues (https://github.com/oconnore/queues)
- event-glue (https://quickdocs.org/event-glue)
- cffi (https://cffi.common-lisp.dev/manual/html_node/Foreign-Types.html)
- sockets (https://lispcookbook.github.io/cl-cookbook/sockets.html)
