(ns psadan.channel
  (:use clojure.pprint)
  (:require
   [psadan.connection :as conn]
   [psadan.protocol :as proto]
   [psadan.pack :as pack]
   [psadan.buffer :as buf]))

(defmulti handle-message 
  (fn [conn m] 
    [(:name (:interface m)) (:message m)]))

(defmethod handle-message [:wl_registry :global] [conn m]
  ;; according to the protocol, 'name' is a uint.  Confusing, 
  ;; but we stick with that nomenclature for consistency
  (let [[name interface version] (:args m)]
    (println ["global " name interface version])
    (conn/register-global conn name interface version)))

(defmethod handle-message [:wl_callback :done] [conn m]
  (let [object (conn/get-object  conn (:object-id m))
        promise (:promise object)]
    (when promise
      (deliver promise m))))

(defmethod handle-message :default [conn m]
  (println ["unknown message" (:name (:interface m)) (:message m)]))

(defn listen [conn]
  (let [buf (conn/read-buffer conn)
        messages (buf/parse-messages buf conn :events)]
    (printf "got a buffer, %d messages" (count messages))
    (dorun (map #(handle-message conn %) messages)))
  (send-off *agent* listen)
  conn)

(defn open-channel [name]
  (let [agent (agent (conn/open-connection name))]
    (send-off agent listen)
    agent))


(defn get-registry [channel]
  (let [connection @channel
        registry
        (conn/remember-object
         connection
         {:id 2 :interface (proto/find-interface-by-name :wl_registry)})
        promise (promise)
        done-cb
        (conn/remember-object
         connection
         {:id 3 
          :promise promise
          :interface (proto/find-interface-by-name :wl_callback)})
        ]
    (conn/write-buffer connection
                       (pack/pack-message (:display connection)
                                         :requests :get_registry [registry]))
    (conn/write-buffer connection
                       (pack/pack-message (:display connection)
                                         :requests :sync [done-cb]))
    @promise))