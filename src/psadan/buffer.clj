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
    :string
    (let [start (+ offset 4)
          len (word-at buffer offset)
          ;; The string length includes a terminating nul byte.  If the
          ;; length is a multiple of 4, there is no following padding
          pad (case (bit-and len 0x3)
                0 0
                1 3
                2 2
                3 1)
          end (+ start len pad)]
      (conj (buffer-get-arguments buffer end (rest types))
            (apply str
                   (map char
                        ;; lose leading length word, trailing \0
                        (subvec buffer start (+ start -1 len))))))))




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

