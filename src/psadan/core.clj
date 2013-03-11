(ns psadan.core
  (:require [clojure.xml]
            [clojure.data.zip :as dz]
            [clojure.data.zip.xml :as x]
            [clojure.walk]
            [clojure.zip :as z]))

(defn parse-message [i m]
  (let [el (z/node m)
        descr (x/xml1-> m :description)
        args (map (fn [a]
                    (let [a (:attrs (z/node a))]
                      {:name (:name a)
                       :type (keyword (:type a))
                       :interface (keyword (:interface a))}))
                  (x/xml-> m :arg))
        ]
    {:index i
     :name (keyword (:name (:attrs el)))
     :summary (and descr (:summary (:attrs (z/node descr))))
     :args args
     }))

(defn parse-enum [e]
  (let [name (keyword (:name (:attrs (z/node e))))
        entries (map z/node (x/xml-> e :entry))]
    {:name name
     :summary (:summary (:attrs (z/node (x/xml1-> e :description))))
     :entries (map :attrs entries)
     }))

(defn parse-interface [i n]
  (let [el (z/node n)
        requests (map-indexed parse-message (x/xml-> n :request))
        events (map-indexed parse-message (x/xml-> n :event))
        enums (map parse-enum (x/xml-> n :enum))]
    {:index i
     :name (keyword (:name (:attrs el)))
     :requests requests
     :events events
     :enums enums
     }))

(defn parse-protocol [protocol]
  (map-indexed parse-interface (x/xml-> protocol :interface)))

(def the-protocol
  (->
   "/home/dan/wayland/source/wayland/protocol/wayland.xml"
   clojure.xml/parse
   z/xml-zip
   parse-protocol))

(defn find-interface-by-name [name]
  (first (filter #(= (:name %) name) the-protocol)))

(def global-object
  {:interface (find-interface-by-name :wl_display)
   :id 1})

(defn open-socket [name]
  (let [s (cx.ath.matthew.unix.UnixSocket. name)
        in (. s getInputStream)
        out (. s getOutputStream)
        ]
    {:socket s :input in :output out
     :objects (atom (assoc {} 1 global-object))
     }))

(defn remember-object [socket id object]
  (swap! (:objects socket) assoc id object))

(defn get-object [socket id]
  (let [o (get @(:objects socket) id)]
    o))
    

(def socket (open-socket "/home/dan/private/wayland-0"))


(defn foo []
  (. (:output socket) (write (.getBytes "\1\0\0\0\1\0\f\0\2\0\0\0\1\0\0\0\0\0\f\0\3\0\0\0"))))

(defn rd []
  (let [buf (byte-array 580)] 
    (. (:input socket) (read buf))
    buf))


(defn word-at [buf offset]
  (bit-or (nth buf (+ 0 offset))
          (bit-shift-left (nth buf (+ 1 offset)) 8) 
          (bit-shift-left (nth buf (+ 2 offset)) 16)
          (bit-shift-left  (nth buf (+ 3 offset)) 24)))

(defn hword-at [buf offset]
  (bit-or (nth buf (+ 0 offset))
          (bit-shift-left (nth buf (+ 1 offset)) 8) ))

(defn buffer-get-arguments [buffer offset types]
  (case (first types)
    (nil) ()
    (:int :uint :object :new_id)
    (conj (buffer-get-arguments buffer (+ 4 offset) (rest types))
          (word-at buffer offset))
    :string (let [l (word-at buffer offset)] ;this probably doesn't work yet
              (subvec buffer (+ 4 offset) (+ 4 l offset)))))


(defn parse-message-from-buf [buf socket message-type]
  (let [object-id (word-at buf 0)
        bytes (hword-at buf 6)
        opcode (hword-at buf 4)
        object (find-object socket object-id)
        interface-def (:interface object) ; (nth the-protocol (:interface object))
        message-def (nth (get interface-def message-type) opcode)
        args (buffer-get-arguments buf 8 (map :type (:args message-def)))]
    {:object-id object-id :bytes bytes
     :interface interface-def
     :message (:name message-def)
     :args args}))
    

(let [callback (remember-object
                socket 2
                {:interface (find-interface-by-name :wl_callback)})
      ]
  (pack-message socket 
  callback)


(defn parse-messages-from-buf 
  ([buf socket message-type]
     (parse-messages-from-buf buf socket message-type 0))
  ([buf socket message-type start]
     (let [length-bytes (hword-at buf (+ 6 start))
           end (+ length-bytes start)
           message (parse-message-from-buf (subvec buf start end)
                                           socket message-type)]
       (if (>= end (count buf))
         (conj '() message)
         (conj (parse-messages-from-buf buf socket message-type end)
               message)))))
  
(defn pack-message [socket object message & args]
  (let [interface (:interface object)
        ]))



(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
