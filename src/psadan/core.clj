(ns psadan.core
  (:require [psadan.protocol :as proto]
            [psadan.buffer :as buf]))

(def global-object
  {:interface (proto/find-interface-by-name :wl_display)
   :id 1})

(defn open-connection [name]
  (let [s (cx.ath.matthew.unix.UnixSocket. name)
        in (. s getInputStream)
        out (. s getOutputStream)
        ]
    {:socket s
     :input in
     :output out
     :objects (atom (assoc {} 1 global-object))
     }))

(defn remember-object [socket id object]
  (swap! (:objects socket) assoc id object))

(defn get-object [socket id]
  (let [o (get @(:objects socket) id)]
    o))

;;; here endeth the code that will one day be library code.  Below here
;;; it's all client code and/or mucking around


(def connection (open-connection "/home/dan/private/wayland-0"))

(defn foo []
  (. (:output connection)
     ;; this byte string is the initial client greeting performed by
     ;; weston-info, as made visible by strace
     (write (.getBytes "\1\0\0\0\1\0\f\0\2\0\0\0\1\0\0\0\0\0\f\0\3\0\0\0"))))

(defn rd []
  ;; 580 is the size of the response that the compositor sends to weston-info
  ;; when it gets the string in (foo)
  (let [buf (byte-array 580)] 
    (. (:input connection) (read buf))
    buf))


(defn test-pack-message []
  (let [callback (remember-object
                  socket 2
                  {:interface (proto/find-interface-by-name :wl_callback)})
        ]
    (pack-message socket global-object :get_registry callback)))



