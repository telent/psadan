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
    (assoc conn :globals (assoc (:globals conn)
                           name 
                           {:interface (keyword interface)
                            :version version})) ))

(defmethod handle-message [:wl_callback :done] [conn m]
  (let [object (conn/get-object  conn (:object-id m))
        promise (:promise object)]
    (when promise
      (deliver promise m))
    conn))

(defmethod handle-message [:wl_drm :device] [conn m]
  (let [oid (:object-id m)
        object (conn/get-object conn oid)
        [device] (:args m)]
    (conn/remember-object conn (assoc object :device device))
    conn))

(defmethod handle-message [:wl_drm :format] [conn m]
  (let [oid (:object-id m)
        object (conn/get-object conn oid)
        [format] (:args m)
        formats (conj (get object :formats []) format)]
    (conn/remember-object conn (assoc object :formats formats))
    conn))

(defmethod handle-message :default [conn m]
  (println ["unhandled message" (:name (:interface m)) (:message m) (:args m)])
  conn)

(defn listen [conn]
  (let [buf (conn/read-buffer conn)
        messages (buf/parse-messages buf conn :events)]
    (printf "got a buffer, %d messages" (count messages))
    (send-off *agent* listen)
    (reduce handle-message conn messages)))

(defn open-channel [name]
  (let [agent (agent (conn/open-connection name))]
    (send-off agent listen)
    agent))
 
(defmacro with-sync [[channel] & body]
  (let [promis (gensym "promise") done (gensym "done")]
    `(do
       ~@body
       (let [~promis (promise)
             ~done (conn/remember-object
                    @~channel
                    {:promise ~promis
                     :interface (proto/find-interface-by-name :wl_callback)})]
         (conn/write-buffer
          @~channel
          (pack/pack-message (:display @~channel) :requests :sync [~done]))
         @~promis))))


(defn get-registry [channel]
  (with-sync [channel]
    (let [connection @channel
          registry (conn/remember-object
                    connection
                    {:interface (proto/find-interface-by-name :wl_registry)})
          display (:display connection)]
      (conn/write-buffer
       connection
       (pack/pack-message display :requests :get_registry [registry])))))
