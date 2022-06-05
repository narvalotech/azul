;; AZUL: Bluetooth test desktop application
;;
(defun prepend-to-symbol (str sym)
  (read-from-string (concatenate 'string str (string sym))))

;; Create generic backend functions from a list
(defmacro register-bt-generics (fn-name-list functions)
  (append '(progn)
          (list `(defparameter ,fn-name-list nil))
          (loop for line in functions
                collect
                (destructuring-bind (fn-name fn-param-list fn-docstring) line
                  (let ((bt-fn-name (prepend-to-symbol "BT-" fn-name)))
                    `(progn
                       (defgeneric ,bt-fn-name (backend ,@fn-param-list)
                         (:documentation ,fn-docstring))
                       (setf ,fn-name-list (append ,fn-name-list (list ',bt-fn-name)))))))
          ))

(defmacro register-bt-events (return-list input-list)
  (append '(progn)
          (list `(defparameter ,return-list nil))
          (loop for event in input-list
                collect
                (destructuring-bind (evt-name param-list docstring) event
                  `(setf ,return-list
                         (append ,return-list
                                 (list (list ',evt-name ',param-list ,docstring))))
                  ))))

(register-bt-events
 *evt-list*
 (
  (scan-device-report ((addr bt-addr)
                       (sid u8)
                       (rssi i8)
                       (tx-power i8)
                       (adv-type u8)
                       (adv-props u16)
                       (interval u16)
                       (primary-phy u8)
                       (secondary-phy u8)
                       (adv-data bt-adv-data))
                      "Scan report")
  (scan-timeout () "Scan timeout")
  (connected ((conn bt-conn)
              (err hci-error))
             "Connection was successful.")
  ))

;; (equal 'err (car (nth 1 (nth 1 (nth 2 *evt-list*)))))
;; (equal 'hci-error (nth 1 (nth 1 (nth 1 (nth 2 *evt-list*)))))

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

(register-bt-events
 *evt-list*
 (
  ;; Scan
  (scan-device-report ((addr 'bt-addr)
                       (sid 'u8)
                       (rssi 'i8)
                       (tx-power 'i8)
                       (adv-type 'u8)
                       (adv-props 'u16)
                       (interval 'u16)
                       (primary-phy 'u8)
                       (secondary-phy 'u8)
                       (adv-data 'bt-adv-data))
                      "Scanned an advertiser.")
  (scan-timeout () "Scanning has timed out.")

  ;; Connections
  (connected ((conn 'bt-conn)
              (err 'hci-error))
             "Connection was successful.")
  (disconnected ((conn 'bt-conn)
                 (reason 'hci-error))
                "Connection was not successful.")
  (param-updated ((conn 'bt-conn)
                  (param 'bt-conn-params))
                 "Connection parameters were updated.")
  (conn-remote-info ((conn 'conn)
                     (remote-info 'remote-info))
                    "Received information about the remote device.")
  (phy-updated ((conn 'bt-conn)
                (phy-info 'phy-info))
               "PHY has changed.")
  (data-length-updated ((conn 'bt-conn)
                        (data-len-info 'data-len-info))
                       "Data length was updated.")
  (connection-info ((conn 'bt-conn)
                    (type 'u8)
                    (role 'u8)
                    (id 'u8)
                    (conn-le-info 'conn-le-info))
                   "Received information about a given connection.")
  ))

(defun get-idx-by-name (name-list name)
  ;; Get function or event index in list
  (loop for el in name-list
        counting t into idx
        when (eql el name) do (return (1- idx))))

;; (nth (get-idx-by-name *fn-list* 'bt-connect) *fn-list*)


;; Zephyr host RPC-ish backend
(defclass zephyr-host () nil)
(defparameter *backend-inst* (make-instance 'zephyr-host))

(defclass bt-addr ()
    ((addr :initarg :addr :initform '(00 00 00 00 00 00))
     (type :initarg :type :initform 'random)))

;; (defparameter *test-inst* (make-instance 'bt-addr :addr '(#xFF #xEE #xDD #x00 #x11 #x22) :type 'public))
;; (format t "~S~%" *test-inst*)
;; (with-slots (addr type) *test-inst*
;;   (print addr)
;;   (print type))

(defmethod bt-id-create ((type zephyr-host) &key bt-addr)
  ;; TODO: serialize into 'serial' buffer
  (format t "id-create [~a] ~a~%"
          (get-idx-by-name *fn-list* 'bt-id-create)
          bt-addr))

(defmethod bt-id-list ((type zephyr-host))
  (format t "bt-id-list executed~%"))

;; (bt-id-create *backend-inst* :bt-addr "randomaddress")

;; For now only supports s-exps on a single line
(defun parse-command (str)
  (let ((cmd (read-from-string str)))
    (eval (append (list (first cmd) *backend-inst*) (rest cmd)))))

(defparameter *test-list* '(bt-id-create :bt-addr '((00 11 22 33 44 55) 'random)))
(print *test-list*)
(parse-command (format nil "~S" *test-list*))

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
              (let ((line (read-line server-stream)))
              (format t "RX: ~A~%" line)
              (parse-command line)))
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
        (format stream "~S~%" *test-list*)
        (force-output stream))
      )))

(create-client 42069)
