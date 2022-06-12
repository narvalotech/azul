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
   (type :initarg :type :initform :random)))

;; Get list of slots
(defun get-slot-names (object)
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-direct-slots (class-of object))))

;; (defparameter *test-inst* (make-instance 'bt-addr :addr '(#xFF #xEE #xDD #x00 #x11 #x22) :type :public))
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

;; Binary packing
;; Returns a byte array / vector
(defgeneric encode-binary (object)
  (:documentation "Encode a given object into binary.")
  (:method (value) (format nil "No encode method defined for given type: ~a." (type-of object))))

(defun encode-le (bytes value)
   (loop for byte from 0 to (1- bytes) collect
         (ldb (byte 8 (* 8 byte)) value)))

(defun lists->vector (&rest lists)
  (coerce (loop for l in lists nconc l) 'vector))

(defun vector->list (vector)
  (coerce vector 'list))

;; Generates encode-binary methods AND convenience fns to make the int classes
(defmacro generate-encode-binary (int)
  (let ((fname (read-from-string (concatenate 'string "make-" (string int)))))
  `(progn
     (defmethod encode-binary ((object ,int))
       (lists->vector (encode-le
                       (slot-value object 'bytes)
                       (slot-value object 'value))))
     (defun ,fname (value)
       (make-instance ',int :value value)))))

(defun make-u8 (value)
  (make-instance 'u8 :value value))

(defclass u8 ()
  ((value :initarg :value :initform 0)
   (bytes :initarg :bytes :initform 1)))

(generate-encode-binary u8)
(encode-binary (make-u8 123))

(defclass i8 ()
  ((value :initarg :value :initform 0)
   (bytes :initarg :bytes :initform 1)))

(generate-encode-binary i8)
(encode-binary (make-i8 -10))

(defclass u16 ()
  ((value :initarg :value :initform 0)
   (bytes :initarg :bytes :initform 2)))

(generate-encode-binary u16)
(encode-binary (make-u16 #xffab))

(defmethod encode-binary ((object cons))
  (lists->vector object))

(defparameter *bt-addr-types* '(:public 2
                                :random 3))

(defmethod encode-binary ((object bt-addr))
  (with-slots (addr type) object
    (concatenate 'vector
                 (lists->vector addr)
                 (encode-le 1 (getf *bt-addr-types* type)))))

;; (encode-binary (make-instance 'bt-addr
;;                               :addr '(#xFF #xEE #xDD #x00 #x11 #x22)
;;                               :type :public))

;; TODO: set error name automatically
(defclass hci-error ()
  ((code :initarg :code :initform 0)
   (name :initarg :name :initform "")))

(defmethod encode-binary ((object hci-error))
  (lists->vector (encode-le 1 (slot-value object 'code))))

;; (encode-binary (make-instance 'hci-error :code 2))

(defclass bt-adv-data ()
  ((len :initarg :len :initform 0)
   (data :initarg :data :initform 0)))

(defmethod encode-binary ((object bt-adv-data))
  (lists->vector
    (encode-le 2 (slot-value object 'len))
    (slot-value object 'data)))

;; (defparameter *test-inst* (make-instance 'bt-adv-data :len (make-u16 1650) :data '(1 2 3 #xFF)))

;; Ideas for auto-generating the encoder:
;; might still need custom ones for types which don't encode all their
;; slots, e.g. hci-error.
;; (loop for slot in (get-slot-names *test-inst*)
;;       nconc (vector->list
;;              (encode-binary (slot-value *test-inst* slot))))

(defclass bt-conn ()
  ((ptr :initarg :ptr :initform 0)
   (idx :initarg :idx :initform 0)))

(defmethod encode-binary ((object bt-conn))
  (lists->vector
    (encode-le 4 (slot-value object 'ptr))
    (encode-le 1 (slot-value object 'idx))))

;; (encode-binary (make-instance 'bt-conn :ptr #x2000ffab :idx 1))

(defclass bt-conn-param ()
  ((interval-min :initarg :interval-min :initform 0)
   (interval-max :initarg :interval-max :initform 0)
   (latency :initarg :latency :initform 0)
   (timeout :initarg :timeout :initform 0)))

(defmethod encode-binary ((object bt-conn-param))
  (with-slots (interval-min interval-max latency timeout) object
    (lists->vector
      (encode-le 2 interval-min)
      (encode-le 2 interval-max)
      (encode-le 2 latency)
      (encode-le 2 timeout))))

;; (encode-binary (make-instance 'bt-conn-param
;;                               :interval-min 20
;;                               :interval-max 3000
;;                               :latency 3
;;                               :timeout 4000))

(defclass bt-remote-info ()
  ((type :initarg :type :initform 0)
   (version :initarg :version :initform 0)
   (manufacturer :initarg :manufacturer :initform 0)
   (subversion :initarg :subversion :initform 0)
   (features :initarg :features :initform 0)))

(defmethod encode-binary ((object bt-remote-info))
  (with-slots (type version manufacturer subversion features) object
     (lists->vector
      (encode-le 1 type)
      (encode-le 1 version)
      (encode-le 2 manufacturer)
      (encode-le 2 subversion)
      (encode-le 1 features))))

;; (encode-binary (make-instance 'bt-remote-info
;;                               :type #xFE
;;                               :version 233
;;                               :manufacturer #xabcd
;;                               :subversion #x1234
;;                               :features 3))

(defclass bt-phy-info ()
  ((tx :initarg :tx :initform 0)
   (rx :initarg :rx :initform 0)))

(defmethod encode-binary ((object bt-phy-info))
  (with-slots (tx rx) object
    (lists->vector
     (encode-le 1 tx)
     (encode-le 1 rx))))

;; (encode-binary (make-instance 'bt-phy-info
;;                               :tx 2
;;                               :rx 1))

(defclass bt-data-len-info ()
  ((tx-max-len :initarg :tx-max-len :initform 0)
   (tx-max-time :initarg :tx-max-time :initform 0)
   (rx-max-len :initarg :rx-max-len :initform 0)
   (rx-max-time :initarg :rx-max-time :initform 0)))

(defmethod encode-binary ((object bt-data-len-info))
  (with-slots (tx-max-len tx-max-time rx-max-len rx-max-time) object
    (lists->vector
     (encode-le 2 tx-max-len)
     (encode-le 2 tx-max-time)
     (encode-le 2 rx-max-len)
     (encode-le 2 rx-max-time))))

;; (encode-binary (make-instance 'bt-data-len-info
;;                               :tx-max-len 266
;;                               :rx-max-time #xfeef))

(defclass bt-conn-le-info ()
  ((local :initarg :local :initform (make-instance 'bt-addr))
   (remote :initarg :remote :initform (make-instance 'bt-addr))
   (local-setup :initarg :local-setup :initform (make-instance 'bt-addr))
   (remote-setup :initarg :remote-setup :initform (make-instance 'bt-addr))
   (param :initarg :param :initform (make-instance 'bt-conn-param))))

;; TODO: use :initform and class-of to autogenerate encode-binary
(defmethod encode-binary ((object bt-conn-le-info))
  (with-slots (local remote local-setup remote-setup param) object
    (concatenate 'vector
                 (encode-binary local)
                 (encode-binary remote)
                 (encode-binary local-setup)
                 (encode-binary remote-setup)
                 (encode-binary param))))

;; (let ((local-addr
;;         (make-instance 'bt-addr
;;                        :addr '(#xFF #xEE #xDD #x00 #x11 #x22) :type :public))
;;       (remote-addr
;;         (make-instance 'bt-addr
;;                        :addr '(#xab #xcd #xef #x00 #x11 #x22) :type :public))
;;       (param
;;         (make-instance 'bt-conn-param :interval-max 100 :timeout 10)))
;;   (encode-binary (make-instance 'bt-conn-le-info
;;                                 :local local-addr
;;                                 :remote (make-instance 'bt-addr )
;;                                 :local-setup (make-instance 'bt-addr)
;;                                 :remote-setup remote-addr
;;                                 :param param)))
