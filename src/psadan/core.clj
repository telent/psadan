(ns psadan.core
  (:refer-clojure)                      ;apparently this is not the default?!
  (:require
   [psadan.connection :as conn]
   [psadan.protocol :as proto]
   [psadan.buffer :as buf]))


;;; here endeth the code that will one day be library code.  Below here
;;; it's all client code and/or mucking around


(def connection (conn/open-connection "/home/dan/private/wayland-0"))

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
  (let [callback (conn/remember-object
                  connection 2
                  {:interface (proto/find-interface-by-name :wl_callback)})
        ]
    (buf/pack-message connection (:display connection) :get_registry callback)))
