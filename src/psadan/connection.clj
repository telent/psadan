(ns psadan.connection
  (:require [psadan.protocol :as proto]))

(defn global-object-factory []
  ;; this is a singleton object according to wayland, but will
  ;; be a different singleton for each connection, iyswim
  {:interface (proto/find-interface-by-name :wl_display)
   :id 1})

(defn open-connection [name]
  (let [s (cx.ath.matthew.unix.UnixSocket. name)
        in (. s getInputStream)
        out (. s getOutputStream)
        wl-display (global-object-factory)
        ]
    {:socket s
     :input in
     :output out
     :display wl-display
     :objects (atom (assoc {} 1 wl-display))
     :globals (atom {})
     }))

(defn remember-object [conn object]
  (let [id (or (:id object) (+ 1 (apply max (keys @(:objects conn)))))
        o (assoc object :id id)]
    (swap! (:objects conn) assoc id o)
    o))

(defn register-global [conn name interface version]
  (swap! (:globals conn) assoc name {:interface (keyword interface)
                                     :version version}))

(defn get-object [conn id]
  (let [o (get @(:objects conn) id)]
    o))

(defn write-buffer [connection buf]
  (. (:output connection)
     (write (into-array Byte/TYPE buf))))
      
(defn read-buffer [connection]
  (let [buf (byte-array 1024)
        len (. (:input connection) (read buf))]
    (subvec (vec buf) 0 len)))

