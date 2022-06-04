;; AZUL: Bluetooth test desktop application
;;
;; Create generic backend functions from a list
(defmacro register-bt-generics (fn-name-list functions)
  (append '(progn)
          (list `(defparameter ,fn-name-list nil))
          (loop for line in functions
                collect
                (destructuring-bind (fn-name fn-param-list fn-docstring) line
                  (let ((bt-fn-name (intern (concatenate 'string "BT-" (string fn-name)))))
                    `(progn
                       (defgeneric ,bt-fn-name (backend ,@fn-param-list)
                         (:documentation ,fn-docstring))
                       (setf ,fn-name-list (append ,fn-name-list (list ',bt-fn-name)))))))
          ))

;; Some testing
;; (macroexpand '(register-bt-generics *fn-list*
;;   ((id-create (&key bt-addr) "Create a Bluetooth identity.")
;;    (id-reset (id &key bt-addr) "Reset an identity."))))

;; Will create generic functions based on this.
;; Also pass device instance as first param.
;; (name (param-list) docstring)
;; The order should be kept in sync with the target (C) code
;; as it will be used for the function IDs.
(register-bt-generics
 *fn-list*
 (
  ;; Scan
  (scan (type &key filters filter-accept-list) "Scan for BLE devices.")
  (scan-filter-set (filter) "Set a scan filter (e.g. name, addr, rssi).")
  (scan-filter-clear () "Clear all scan filters.")

  ;; Connections
  (connect (&key bt-addr) "Connect to a BLE device.")
  (conn-param-update (&key conn-id min max latency timeout)
                     "Update connection parameters.")
  (data-length-update (&key conn-id tx-max-length tx-max-time) "Perform DLE.")
  (phy-update (&key tx-phy rx-phy s2 s8) "Perform a PHY update procedure.")
  (list-connections () "List active connections.")

  ))

(defun get-idx-by-name (name-list name)
  ;; Get function or event index in list
  (loop for el in name-list
        counting t into idx
        when (eql el name) do (return (1- idx))))

;; (nth (get-idx-by-name *fn-list* 'bt-connect) *fn-list*)


;; Zephyr host RPC-ish backend
(defclass zephyr-host () nil)
;; (defparameter *zephyr-host-inst* (make-instance 'zephyr-host))

(defclass bt-addr ()
    ((addr :initarg :addr :accessor bt-addr :initform "00:00:00:00:00:00")
     (type :initarg :type :accessor bt-addr-type :initform 'random)))

;; (make-instance 'bt-addr :addr "FF:EE:DD:00:11:22" :type 'public)

(defmethod bt-id-create ((type zephyr-host) &key bt-addr)
  ;; TODO: serialize into 'serial' buffer
  (format t "id-create [~a] ~a~%"
          (get-idx-by-name *fn-list* 'bt-id-create)
          bt-addr))

;; (bt-id-create *zephyr-host-inst* :bt-addr "randomaddress")

(ql:quickload "usocket")

(defun create-server-and-listen (port)
  (format t "Opening backend on port ~a...~%" port)
  (usocket:with-socket-listener (server-socket "127.0.0.1" port :reuse-address t)
    (usocket:wait-for-input server-socket)
    (usocket:with-connected-socket (server-conn (usocket:socket-accept server-socket))
      (let ((server-stream (usocket:socket-stream server-conn)))
        (format t "Client connected~%")
        (handler-case
            (loop
              (format t "~A~%" (read-line server-stream)))
          (end-of-file (c)
            (format t "Client disconnected, exiting backend~%")
            (values 0 c))
          )))))

;; Can use netcat to talk to it:
;; nc 127.0.0.1 42069
(create-server-and-listen 42069)

(defun create-client (port)
  (usocket:with-client-socket (socket stream "127.0.0.1" port :element-type 'character)
    (loop repeat 3 do
      (progn
        (format stream "Hello World~%")
        (force-output stream))
      )))

(create-client 42069)
