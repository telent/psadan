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
     }))

(defn remember-object [conn id object]
  (swap! (:objects conn) assoc id object)
  object)

(defn get-object [conn id]
  (let [o (get @(:objects conn) id)]
    o))
