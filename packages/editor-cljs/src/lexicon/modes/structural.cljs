(ns lexicon.modes.structural
  "Structural navigation and editing commands using AST information"
  (:require [re-frame.core :as rf])
  (:require-macros [lexicon.macros :refer [def-evil-motion def-evil-operator def-evil-command]]))

;; -- AST Navigation Helpers --

(defn line-col-to-linear-pos
  "Convert line/column coordinates to linear position"
  [text line column]
  (let [lines (clojure.string/split-lines text)]
    (if (>= line (count lines))
      (count text)
      (let [lines-before (take line lines)
            chars-before-line (reduce + 0 (map count lines-before))
            newlines-before line ; One newline per line before current
            line-content (nth lines line "")
            safe-column (min column (count line-content))]
        (+ chars-before-line newlines-before safe-column)))))

(defn find-nodes-by-type
  "Find all nodes of a given type in the AST"
  [ast node-type]
  (when ast
    (let [matches (atom [])]
      (letfn [(traverse [node]
                (when (= (:type node) node-type)
                  (swap! matches conj node))
                (when-let [children (:children node)]
                  (doseq [child children]
                    (traverse child))))]
        (traverse ast))
      @matches)))

(defn find-nearest-node
  "Find the nearest node of a given type to the cursor position"
  [ast cursor-line node-type direction]
  (let [nodes (find-nodes-by-type ast node-type)
        nodes-with-lines (map (fn [node]
                                {:node node
                                 :line (:row (:startPosition node))})
                              nodes)]
    (case direction
      :forward  (first (filter #(> (:line %) cursor-line) 
                               (sort-by :line nodes-with-lines)))
      :backward (last (filter #(< (:line %) cursor-line) 
                              (sort-by :line nodes-with-lines)))
      :current  (first (filter #(and (<= (:line %) cursor-line)
                                     (>= (:row (:endPosition (:node %))) cursor-line))
                               nodes-with-lines)))))

(defn node-to-cursor-position
  "Convert AST node position to cursor coordinates"
  [node]
  (when node
    (let [start-pos (:startPosition node)]
      {:line (:row start-pos)
       :column (:column start-pos)})))

;; -- Structural Motion Commands --

(def-evil-motion structural-next-function
  [count]
  "Move cursor to the start of the next function definition"
  {:type :exclusive :jump? false}
  (let [db @re-frame.db/app-db
        active-window (get (:windows db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        ast (:ast active-buffer)
        cursor-line (get-in active-buffer [:cursor-position :line] 0)]
    (when ast
      (let [target-node (find-nearest-node ast cursor-line "function_definition" :forward)]
        (when target-node
          (let [new-position (node-to-cursor-position (:node target-node))]
            (rf/dispatch [:update-cursor-position 
                         (line-col-to-linear-pos 
                          (.getText (:wasm-instance active-buffer)) 
                          (:line new-position) 
                          (:column new-position))])))))))

(def-evil-motion structural-prev-function
  [count]
  "Move cursor to the start of the previous function definition"
  {:type :exclusive :jump? false}
  (let [db @re-frame.db/app-db
        active-window (get (:windows db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        ast (:ast active-buffer)
        cursor-line (get-in active-buffer [:cursor-position :line] 0)]
    (when ast
      (let [target-node (find-nearest-node ast cursor-line "function_definition" :backward)]
        (when target-node
          (let [new-position (node-to-cursor-position (:node target-node))]
            (rf/dispatch [:update-cursor-position 
                         (line-col-to-linear-pos 
                          (.getText (:wasm-instance active-buffer)) 
                          (:line new-position) 
                          (:column new-position))])))))))

(def-evil-motion structural-next-class
  [count]
  "Move cursor to the start of the next class definition"
  {:type :exclusive :jump? false}
  (let [db @re-frame.db/app-db
        active-window (get (:windows db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        ast (:ast active-buffer)
        cursor-line (get-in active-buffer [:cursor-position :line] 0)]
    (when ast
      (let [target-node (find-nearest-node ast cursor-line "class_definition" :forward)]
        (when target-node
          (let [new-position (node-to-cursor-position (:node target-node))]
            (rf/dispatch [:update-cursor-position 
                         (line-col-to-linear-pos 
                          (.getText (:wasm-instance active-buffer)) 
                          (:line new-position) 
                          (:column new-position))])))))))

(def-evil-motion structural-prev-class
  [count]
  "Move cursor to the start of the previous class definition"
  {:type :exclusive :jump? false}
  (let [db @re-frame.db/app-db
        active-window (get (:windows db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        ast (:ast active-buffer)
        cursor-line (get-in active-buffer [:cursor-position :line] 0)]
    (when ast
      (let [target-node (find-nearest-node ast cursor-line "class_definition" :backward)]
        (when target-node
          (let [new-position (node-to-cursor-position (:node target-node))]
            (rf/dispatch [:update-cursor-position 
                         (line-col-to-linear-pos 
                          (.getText (:wasm-instance active-buffer)) 
                          (:line new-position) 
                          (:column new-position))])))))))

(def-evil-motion structural-beginning-of-defun
  [count]
  "Move cursor to the beginning of the current function/class definition"
  {:type :exclusive :jump? false}
  (let [db @re-frame.db/app-db
        active-window (get (:windows db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        ast (:ast active-buffer)
        cursor-line (get-in active-buffer [:cursor-position :line] 0)]
    (when ast
      ;; Try to find a containing function or class
      (let [function-node (find-nearest-node ast cursor-line "function_definition" :current)
            class-node (find-nearest-node ast cursor-line "class_definition" :current)
            target-node (or function-node class-node)]
        (when target-node
          (let [new-position (node-to-cursor-position (:node target-node))]
            (rf/dispatch [:update-cursor-position 
                         (line-col-to-linear-pos 
                          (.getText (:wasm-instance active-buffer)) 
                          (:line new-position) 
                          (:column new-position))])))))))

(def-evil-motion structural-end-of-defun
  [count]
  "Move cursor to the end of the current function/class definition"
  {:type :exclusive :jump? false}
  (let [db @re-frame.db/app-db
        active-window (get (:windows db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)
        ast (:ast active-buffer)
        cursor-line (get-in active-buffer [:cursor-position :line] 0)]
    (when ast
      ;; Try to find a containing function or class
      (let [function-node (find-nearest-node ast cursor-line "function_definition" :current)
            class-node (find-nearest-node ast cursor-line "class_definition" :current)
            target-node (or function-node class-node)]
        (when target-node
          (let [end-pos (:endPosition (:node target-node))
                new-position {:line (:row end-pos) :column (:column end-pos)}]
            (rf/dispatch [:update-cursor-position 
                         (line-col-to-linear-pos 
                          (.getText (:wasm-instance active-buffer)) 
                          (:line new-position) 
                          (:column new-position))])))))))

;; -- Export functions for registration --
;; These functions will be registered as re-frame events in the events namespace