(ns psadan.protocol
  (:require [clojure.xml]
            [clojure.data.zip :as dz]
            [clojure.data.zip.xml :as x]
            [clojure.walk]
            [clojure.zip :as z]))

;;; parse the wayland wire protocol from wayland.xml

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
