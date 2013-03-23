(ns psadan.core
  (:refer-clojure)                      ;apparently this is not the default?!
  (:require
   [psadan.connection :as conn]
   [psadan.protocol :as proto]
   [psadan.pack :as pack]
   [psadan.channel :as chan]
   [psadan.buffer :as buf]))

;;; here endeth the code that will one day be library code.  Below here
;;; it's all client code and/or mucking around

(def channel (chan/open-channel "/home/dan/private/wayland-0"))

(defn fake-weston-info-client-send []
  (. (:output @channel)
     ;; this byte string is the initial client greeting performed by
     ;; weston-info, as made visible by strace
     (write (.getBytes "\1\0\0\0\1\0\f\0\2\0\0\0\1\0\0\0\0\0\f\0\3\0\0\0"))))


(defn test-parse-messages []
  (let [connection @channel
        m (buf/parse-messages (vec (.getBytes "\1\0\0\0\1\0\f\0\2\0\0\0\1\0\0\0\0\0\f\0\3\0\0\0")) connection :requests)]
    (assert (= (map :message m) '(:get_registry :sync)))
    (map :message m)))

(defn test-pack-message []
  (let [connection @channel
        callback (conn/remember-object
                  connection
                  {:id 2                   
                   :interface (proto/find-interface-by-name :wl_callback)})
        callback1 (conn/remember-object
                   connection
                   {:id 3
                    :interface (proto/find-interface-by-name :wl_callback)})
        ]
    (assert (=
             (pack/pack-message connection (:display connection)
                                :requests :get_registry [callback])
             [1 0 0 0 1 0 12 0 2 0 0 0]))
    (assert (=
             (pack/pack-message connection (:display connection)
                                :requests :sync [callback1])
             [1 0 0 0 0 0 12 0 3 0 0 0]))))



(defn registry-bind [channel interface-name]
  (let [registry (conn/get-object @channel 2)
        [intfname interface] (conn/get-global @channel interface-name)
        bound (conn/remember-object
               @channel
               {:interface (proto/find-interface-by-name interface-name)})]
    (conn/write-buffer
     @channel
     (pack/pack-message registry :requests :bind
                        [intfname
                         (name (:interface interface))
                         (:version interface)
                         bound]))
    bound))

