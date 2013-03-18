(ns psadan.channel
  (:use clojure.pprint)
  (:require
   [psadan.connection :as conn]
   [psadan.protocol :as proto]
   [psadan.buffer :as buf]))

(defmulti handle-message 
  (fn [conn m] 
    [(:name (:interface m)) (:message m)]))

(defmethod handle-message [:wl_registry :global] [conn m]
  (println ["fake register global" (:args m)]))

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