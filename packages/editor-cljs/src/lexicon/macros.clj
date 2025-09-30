(ns lexicon.macros
  "Metaprogramming DSL for defining evil-mode style commands with rich metadata")

(defmacro def-evil-motion
  "Define a motion command with evil-mode style metadata.
  
  Usage:
    (def-evil-motion my-forward-word [count]
      \"Move forward by count words\"
      {:type :exclusive :jump? false}
      (do-forward-word count))
  
  Args:
    name - symbol name for the motion function
    args - argument vector (typically includes count)
    docstring - documentation string
    metadata-map - map containing motion metadata:
      :type - :exclusive, :inclusive, or :line
      :jump? - boolean indicating if this is a jump motion
    body - implementation body"
  [name args docstring metadata-map & body]
  `(def ~(with-meta name (assoc metadata-map 
                                :motion? true
                                :doc docstring))
     (fn ~args ~@body)))

(defmacro def-evil-operator
  "Define an operator command with evil-mode style metadata.
  
  Usage:
    (def-evil-operator my-delete [motion]
      \"Delete text defined by motion\"
      {:repeat? true}
      (do-delete motion))
  
  Args:
    name - symbol name for the operator function
    args - argument vector (typically includes motion)
    docstring - documentation string
    metadata-map - map containing operator metadata:
      :repeat? - boolean indicating if this operator can be repeated
    body - implementation body"
  [name args docstring metadata-map & body]
  `(def ~(with-meta name (assoc metadata-map 
                                :operator? true
                                :type :operator
                                :doc docstring))
     (fn ~args ~@body)))

(defmacro def-evil-command
  "Define a general evil command with metadata.
  
  Usage:
    (def-evil-command my-command []
      \"A simple command\"
      {:type :command}
      (do-something))
  
  Args:
    name - symbol name for the command function
    args - argument vector
    docstring - documentation string
    metadata-map - map containing command metadata
    body - implementation body"
  [name args docstring metadata-map & body]
  `(def ~(with-meta name (assoc metadata-map :doc docstring))
     (fn ~args ~@body)))