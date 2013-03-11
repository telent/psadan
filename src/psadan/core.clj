(ns psadan.core
  (:require [psadan.protocol :as proto]))

(def global-object
  {:interface (proto/find-interface-by-name :wl_display)
   :id 1})

(defn open-connection [name]
  (let [s (cx.ath.matthew.unix.UnixSocket. name)
        in (. s getInputStream)
        out (. s getOutputStream)
        ]
    {:socket s
     :input in
     :output out
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


;;; Code that will one day be library code lives above this line

;;;;                 -------------------------------------

;;; Client code that is really just me screwing around lives below that line


(def connection (open-connection "/home/dan/private/wayland-0"))

(defn foo []
  (. (:output connection)
     ;; this byte string is the initial client greeting performed by
     ;; weston-info, as made visible by strace
     (write (.getBytes "\1\0\0\0\1\0\f\0\2\0\0\0\1\0\0\0\0\0\f\0\3\0\0\0"))))

(defn rd []
  ;; 580 is the size of the response that the compositor sends to weston-info
  ;; when it gets the string in (foo)
  (let [buf (byte-array 580)] 
    (. (:input connection) (read buf))
    buf))


(defn test-pack-message []
  (let [callback (remember-object
                  socket 2
                  {:interface (proto/find-interface-by-name :wl_callback)})
        ]
    (pack-message socket global-object :get_registry callback)))



