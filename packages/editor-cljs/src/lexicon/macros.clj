(ns lexicon.macros
  "Metaprogramming macros for defining Evil-style motions and operators")

(defmacro def-evil-motion
  "Define an Evil-style motion command with metadata."
  [name args docstring metadata & body]
  (let [motion-metadata (merge {:type :motion
                               :motion? true}
                              metadata)]
    `(do
       (defn ~name ~args
         ~docstring
         ~@body)
       
       ;; Attach metadata to the function var
       (alter-meta! (var ~name) merge ~motion-metadata)
       
       ;; Return the function var for further use
       (var ~name))))

(defmacro def-evil-operator
  "Define an Evil-style operator command with metadata."
  [name args docstring metadata & body]
  (let [operator-metadata (merge {:type :operator
                                 :operator? true}
                                metadata)]
    `(do
       (defn ~name ~args
         ~docstring
         ~@body)
       
       ;; Attach metadata to the function var
       (alter-meta! (var ~name) merge ~operator-metadata)
       
       ;; Return the function var for further use
       (var ~name))))

(defmacro def-evil-text-object
  "Define an Evil-style text object with metadata."
  [name args docstring metadata & body]
  (let [text-object-metadata (merge {:type :text-object
                                    :text-object? true}
                                   metadata)]
    `(do
       (defn ~name ~args
         ~docstring
         ~@body)
       
       ;; Attach metadata to the function var
       (alter-meta! (var ~name) merge ~text-object-metadata)
       
       ;; Return the function var for further use
       (var ~name))))

(defmacro def-evil-command
  "Define a general Evil-style command with metadata."
  [name args docstring metadata & body]
  (let [command-metadata (merge {:type :command
                                :command? true}
                               metadata)]
    `(do
       (defn ~name ~args
         ~docstring
         ~@body)
       
       ;; Attach metadata to the function var
       (alter-meta! (var ~name) merge ~command-metadata)
       
       ;; Return the function var for further use
       (var ~name))))

(defmacro with-count
  "Execute body with count variable bound to current count register."
  [count-sym & body]
  `(let [~count-sym (or (get-in @re-frame.db/app-db [:fsm :count-register]) 1)]
     ~@body))