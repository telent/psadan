(ns psadan.pack)

(defn pack-arg [type value]
  (case type
    :new_id
    (pack-arg :uint (:id value))
    (:int :uint :object)
    [(bit-shift-right (bit-and value 0x000000ff) 0)
     (bit-shift-right (bit-and value 0x0000ff00) 8)
     (bit-shift-right (bit-and value 0x00ff0000) 16)
     (bit-shift-right (bit-and value 0xff000000) 24)]
    :string
    (let [bytes (.getBytes value)]
      (vec (concat (pack-arg :int (count bytes))
                   (vec bytes)
                   (subvec [0 0 0 0] (mod (count bytes) 4)))))))

(defn test-pack-arg []
  (assert (= (map #(format "%x" %) (pack-arg :int 0x12345678))
             '("78" "56" "34" "12")))
  (assert (= (pack-arg :string "hello")
             [5 0 0 0 104 101 108 108 111 0 0 0]))
  (assert (= (pack-arg :new_id {:id 1234}) [210 4 0 0]))
  (assert (= (count (pack-arg :string "hello ")) 12))
  (assert (= (count (pack-arg :string "hello 1")) 12))
  (assert (= (count (pack-arg :string "hello 12")) 16)))


(defn pack-message [object message-type message-name args]
  (let [iface (:interface object)
        messages (get iface message-type)
        message (first (filter #(= (:name %) message-name) messages))
        arg-types (map :type (:args message))
        arg-bytes (mapcat pack-arg arg-types args)]
    (vec (concat (pack-arg :uint (:id object))
                 (pack-arg :uint
                           (bit-or (bit-shift-left (+ 8 (count arg-bytes)) 16)
                                   (:index message)))
                 arg-bytes
                 ))))

