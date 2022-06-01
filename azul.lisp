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
(register-bt-generics *fn-list*
  (
   ;; Identity & name
   (id-create (&key bt-addr) "Create a Bluetooth identity.")
   (id-reset (id &key bt-addr) "Reset an identity.")
   (id-delete (id) "Delete an identity.")
   (id-list () "List available identities.")
   (id-select (id) "Select an identiy to use.")
   (get-name () "Get the name of the device.")
   (set-name (name-str) "Set the name of the device.")

   ;; Scan
   (scan (type &key filters filter-accept-list) "Scan for BLE devices.")
   (scan-filter-set (filter) "Set a scan filter (e.g. name, addr, rssi).")
   (scan-filter-clear () "Clear all scan filters.")

   (advertise (&key type mode filter-accept-list identity no-name
                one-time name-ad channel-mask)
     "Start legacy advertiser.")

   ;; Extended advertising only
   (adv-create (&key num type ext-adv phy filter-accept-list identity
                 name name-ad directed-addr duty-cycle channel-mask)
     "Create advertising set.")
   (adv-list () "List available advertising sets.")
   (adv-param-set (&key num type ext-adv phy filter-accept-list identity
                    name name-ad directed-addr duty-cycle channel-mask)
     "Set advertising parameters.")
   (adv-data-set (&key num data scan-response-data)
     "Set advertising data.")
   (adv-start (&key num timeout num-events) "Start advertising set.")
   (adv-stop (&key num) "Stop advertising set.")
   (adv-delete (&key num) "Delete advertising set.")
   (per-adv (type) "Start the periodic advertiser.")
   (per-adv-param (&key int-min int-max tx-power)
     "Set periodic advertising parameters.")
   (per-adv-data (data) "Set periodic advertising data.")
   (per-adv-sync-create (addr) "Sync to a periodic advertiser.")
   (per-adv-sync-delete (&key id) "Stop periodic advertiser sync.")

   ;; Connections
   (connect (&key bt-addr) "Connect to a BLE device.")
   (conn-param-update (&key conn-id min max latency timeout)
     "Update connection parameters.")
   (data-length-update (&key conn-id tx-max-length tx-max-time) "Perform DLE.")
   (phy-update (&key tx-phy rx-phy s2 s8) "Perform a PHY update procedure.")

   ;; Security
   (bond-clear (&key bt-addr) "Clear active bonds.")
   (set-security (&key level force-pair) "Set link security: L1-L4.")
   (set-bondable (value) "Enable or disable bonding.")
   (list-bonds () "List active bonds.")
   (list-connections () "List active connections.")
   (auth-set-method (method)
     "Set authentication methods: all, input, display, yesno, confirm, oob,
     status, none.")
   (auth-cancel () "Cancel ongoing authentication procedure.")
   (auth-enter-passkey (passkey) "Enter passkey.")
   (auth-passkey-confirm () "Confirm displayed passkey.")
   (auth-pairing-confirm () "Confirm pairing request.")
   (set-fixed-passkey (passkey) "Set a fixed passkey.")

   ))

(defun get-fn-idx (fn-name-list name)
  ;; Get RPC fn index
  (loop for el in fn-name-list
        counting t into idx
        when (eql el name) do (return (1- idx))))

;; (nth (get-fn-idx *fn-list* 'bt-connect) *fn-list*)

(defclass zephyr-host () nil)
;; (defparameter *zephyr-host-inst* (make-instance 'zephyr-host))

;; Zephyr host RPC-ish backend
(defmethod id-create (type zephyr-host) (&key bt-addr)
  (format t "id-create"))
