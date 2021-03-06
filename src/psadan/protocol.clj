;;;; parse the wayland wire protocol from wayland.xml

(ns psadan.protocol
  (:require [clojure.xml]
            [clojure.data.zip :as dz]
            [clojure.data.zip.xml :as x]
            [clojure.walk]
            [clojure.zip :as z]))

(defn extra-args-for-new-ids [[arg & args]]
  ;; if any arg is declared in the XML as having a new_id arg with null 
  ;; interface, that *actually* means it takes three args, those being
  ;; string interface_name, uint version, and the new_id itself
  (when arg
    (if (and (= (:type arg) :new_id)
             (nil? (:interface arg)))
      (let [version {:name (str (:name arg) "/version") :type :uint}
            name {:name (str (:name arg) "/interface") :type :string}]
        (conj
         (conj (conj (extra-args-for-new-ids args)
                     arg)
               version)
         name))
      (conj (extra-args-for-new-ids args) arg))))

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
     :args (extra-args-for-new-ids args)
     }))

(defn parse-enum [e]
  (let [name (keyword (:name (:attrs (z/node e))))
        entries (map z/node (x/xml-> e :entry))]
    {:name name
     :summary (let [d (x/xml1-> e :description)]
                (and d (:summary (:attrs (z/node d)))))
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
  (let [parse (fn [file]
                (->
                 file
                 clojure.xml/parse
                 z/xml-zip
                 parse-protocol))
        wayland (parse 
                 "/home/dan/wayland/source/wayland/protocol/wayland.xml")
        drm (parse 
             "/home/dan/wayland/source/mesa/src/egl/wayland/wayland-drm/wayland-drm.xml")
        ]
    (concat wayland drm)))

(defn find-interface-by-name [name]
  (first (filter #(= (:name %) name) the-protocol)))

