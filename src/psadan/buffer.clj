(ns psadan.buffer
  (:require [psadan.connection :as conn]))

(defn word-at [buf offset]
  (bit-or (nth buf (+ 0 offset))
          (bit-shift-left (nth buf (+ 1 offset)) 8)
          (bit-shift-left (nth buf (+ 2 offset)) 16)
          (bit-shift-left (nth buf (+ 3 offset)) 24)))

(defn halfword-at [buf offset]
  (bit-or (nth buf (+ 0 offset))
          (bit-shift-left (nth buf (+ 1 offset)) 8)))

(defn buffer-get-arguments [buffer offset types]
  (case (first types)
    (nil) ()
    (:int :uint :object :new_id)
    (conj (buffer-get-arguments buffer (+ 4 offset) (rest types))
          (word-at buffer offset))
    :string (let [l (word-at buffer offset)
                  round (fn [x] (+ (bit-and x 0xfffffffc) 4))
                  end (+ 4 (round l) offset)]
              (conj (buffer-get-arguments buffer end (rest types))
                    (subvec buffer (+ 4 offset) (+ 4 l offset))))))
              
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


(defn parse-message [buf connection message-type]
  (let [object-id (word-at buf 0)
        bytes (halfword-at buf 6)
        opcode (halfword-at buf 4)
        object (conn/get-object connection object-id)
        interface-def (:interface object) 
        message-def (nth (get interface-def message-type) opcode)
        args (buffer-get-arguments buf 8 (map :type (:args message-def)))]
    {:object-id object-id :bytes bytes
     :interface interface-def
     :message (:name message-def)
     :args args}))
    
(defn parse-messages 
  ([buf socket message-type]
     (parse-messages buf socket message-type 0))
  ([buf socket message-type start]
     (let [length-bytes (halfword-at buf (+ 6 start))
           end (+ length-bytes start)
           message (parse-message (subvec buf start end)
                                  socket message-type)]
       (if (>= end (count buf))
         (conj '() message)
         (conj (parse-messages buf socket message-type end)
               message)))))


(defn pack-message [connection object message-type message-name args]
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

