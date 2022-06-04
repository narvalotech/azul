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

Scratch
-------

GUI:
- send over `socket-backend`: id-create :addr blabla

Backend:
- receive over `socket-backend`: id-create :addr blabla
- insert opened backend at beginning of paramlist
- call matching method with paramlist

Backend[host]:
-> in `id-create`
- parse address into class instance
- serialize into binary:
  - get function index in table
  - serialize params following the matching C struct format
- call command dispatch w/ TLV binary array
  "Driver":
  - send bin array to serial interface:
  -> `socket-serial` (talks to socat)
  - wait for response on `socket-serial`
  - return bin array from serial interface
    - format: cmd-response frame + cmd counter + an error code
- `parse-cmd-response`:
- parse cmd response into cmd response object
- serialize object into s-exp
- send s-exp on `socket-backend`

Backend main loop:
- listen on socket-serial for events
- call `parse-event` with bin array + backend class:
  - gets the event idx from the event table
  - deserializes the parameters into objects
- call `dispatch-event`:
  - serializes the objects to s-expression
  - sends s-exp on `socket-backend`:
    - format: (event event-paramlist)

Proof of concept:
- Scan & display adv reports in list
  - commands:
    - scan
    - scan-filter-set
    - scan-filter-clear
  - events:
    - scan-device-report
      - addr [le-addr]
      - sid [u8]
      - rssi [i8]
      - tx-power [i8]
      - adv-type [u8]
      - adv-props [u16]
      - interval [u16]
      - primary-phy [u8]
      - secondary-phy [u8]
      - adv-data
        - len [u16]
        - data [u8 array]
    - scan-timeout

- Connect to any advertiser listed
  - commands:
    - connect
    - conn-param-update
    - data-length-update
    - phy-update
    - list-connections
  - events:
    - connected
      - conn [conn]
        - mem [u32]
        - idx [u8]
      - err [u8] / [hci-error]
    - disconnected
      - conn [conn]
      - reason [u8] / [hci-error]
    - le-param-updated
      - conn [conn]
      - param [conn-param]
        - interval-min [u16]
        - interval-max [u16]
        - latency [u16]
        - timeout [u16]
    - conn-remote-info
      - conn [conn]
      - remote-info [remote-info]
        - type [u8]
        - version [u8]
        - manufacturer [u16]
        - subversion [u16]
        - features [u8]
    - le-phy-updated
      - conn [conn]
      - phy-info [phy-info]
        - tx-phy [u8]
        - rx-phy [u8]
    - data-length-updated
      - conn [u8]
      - data-len-info [data-len-info]
        - tx-max-len [u16]
        - tx-max-time [u16]
        - rx-max-len [u16]
        - rx-max-time [u16]
    - connection-info (output of list-connections)
      - type [u8]
      - role [u8]
      - id [u8]
      - conn-le-info [conn-le-info]
        - local [bt-addr]
        - remote [bt-addr]
        - local-setup [bt-addr]
        - remote-setup [bt-addr]
        - param [conn-param]

- Dump GATT db & display in list
