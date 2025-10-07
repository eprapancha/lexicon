(ns lexicon.api.export
  "Pluggable backend framework for AST processing using multimethods"
  (:require [re-frame.core :as rf]))

;; -- Core Multimethod Framework --

(defmulti process-ast-node
  "Core multimethod for processing AST nodes. Dispatch on node type."
  (fn [node context] (:type node)))

;; Default implementation for unknown node types
(defmethod process-ast-node :default
  [node context]
  {:type :unknown
   :original-type (:type node)
   :text (:text node)
   :children (when-let [children (:children node)]
               (mapv #(process-ast-node % context) children))})

;; -- Export Context Management --

(defn create-export-context
  "Create an export context for processing"
  [export-type options]
  {:export-type export-type
   :options (merge {:include-metadata true
                   :preserve-structure true
                   :line-numbers false} 
                  options)
   :metadata {:timestamp (js/Date.)
             :source "lexicon-editor"}
   :output-buffer []})

(defn append-to-output
  "Append content to the export output buffer"
  [context content]
  (update context :output-buffer conj content))

(defn get-export-output
  "Get the final export output as a string"
  [context]
  (clojure.string/join "" (:output-buffer context)))

;; -- Built-in Node Processors --

(defmethod process-ast-node "source_file"
  [node context]
  (let [processed-children (when-let [children (:children node)]
                            (mapv #(process-ast-node % context) children))]
    {:type :document
     :children processed-children
     :metadata {:total-children (count processed-children)}}))

(defmethod process-ast-node "keyword"
  [node context]
  {:type :keyword
   :text (:text node)
   :language-construct true
   :position {:start (:startPosition node)
             :end (:endPosition node)}})

(defmethod process-ast-node "string"
  [node context]
  {:type :string-literal
   :text (:text node)
   :value (subs (:text node) 1 (dec (count (:text node))))  ; Remove quotes
   :position {:start (:startPosition node)
             :end (:endPosition node)}})

(defmethod process-ast-node "number"
  [node context]
  {:type :number-literal
   :text (:text node)
   :value (js/parseFloat (:text node))
   :position {:start (:startPosition node)
             :end (:endPosition node)}})

(defmethod process-ast-node "comment"
  [node context]
  {:type :comment
   :text (:text node)
   :content (clojure.string/trim (subs (:text node) 2))  ; Remove //
   :position {:start (:startPosition node)
             :end (:endPosition node)}})

(defmethod process-ast-node "function_definition"
  [node context]
  (let [processed-children (when-let [children (:children node)]
                            (mapv #(process-ast-node % context) children))]
    {:type :function
     :text (:text node)
     :children processed-children
     :metadata {:complexity (count processed-children)
               :start-line (:row (:startPosition node))
               :end-line (:row (:endPosition node))}
     :position {:start (:startPosition node)
               :end (:endPosition node)}}))

(defmethod process-ast-node "class_definition"
  [node context]
  (let [processed-children (when-let [children (:children node)]
                            (mapv #(process-ast-node % context) children))]
    {:type :class
     :text (:text node)
     :children processed-children
     :metadata {:member-count (count processed-children)
               :start-line (:row (:startPosition node))
               :end-line (:row (:endPosition node))}
     :position {:start (:startPosition node)
               :end (:endPosition node)}}))

;; -- Export Operations --

(defn export-ast
  "Export an AST using the pluggable backend framework"
  [ast export-type options]
  (when ast
    (let [context (create-export-context export-type options)
          processed-ast (process-ast-node ast context)]
      {:processed-ast processed-ast
       :context context
       :export-type export-type
       :metadata (:metadata context)})))

(defn export-to-json
  "Export processed AST to JSON format"
  [processed-export]
  (js/JSON.stringify (clj->js (:processed-ast processed-export)) nil 2))

(defn export-to-outline
  "Export processed AST to a text outline format"
  [processed-export]
  (letfn [(render-node [node depth]
            (let [indent (apply str (repeat (* depth 2) " "))
                  node-info (str indent "- " (:type node)
                               (when (:text node)
                                 (str ": " (clojure.string/trim 
                                           (first (clojure.string/split-lines (:text node)))))))]
              (str node-info "\n"
                   (when-let [children (:children node)]
                     (apply str (map #(render-node % (inc depth)) children))))))]
    (render-node (:processed-ast processed-export) 0)))

;; -- Extension Registration System --

(defonce registered-exporters (atom {}))

(defn register-exporter!
  "Register a new export format"
  [export-type {:keys [name description processor]}]
  (swap! registered-exporters assoc export-type
         {:name name
          :description description
          :processor processor}))

(defn get-available-exporters
  "Get list of available export formats"
  []
  @registered-exporters)

(defn export-with-registered-processor
  "Export using a registered processor"
  [ast export-type options]
  (if-let [exporter (get @registered-exporters export-type)]
    ((:processor exporter) ast options)
    (throw (js/Error. (str "Unknown export type: " export-type)))))

;; Register built-in exporters
(register-exporter! :json
  {:name "JSON Export"
   :description "Export AST as formatted JSON"
   :processor (fn [ast options]
                (-> (export-ast ast :json options)
                    export-to-json))})

(register-exporter! :outline
  {:name "Text Outline"
   :description "Export AST as hierarchical text outline"
   :processor (fn [ast options]
                (-> (export-ast ast :outline options)
                    export-to-outline))})

;; -- Re-frame Integration --

(rf/reg-event-fx
 :export/process-buffer
 (fn [{:keys [db]} [_ export-type options]]
   "Export the current buffer's AST using the specified format"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         ast (:ast active-buffer)]
     (if ast
       (try
         (let [exported (export-with-registered-processor ast export-type options)]
           {:db (assoc-in db [:system :last-export] 
                         {:content exported
                          :type export-type
                          :timestamp (js/Date.)})
            :fx [[:dispatch [:show-export-result exported export-type]]]})
         (catch js/Error error
           {:fx [[:dispatch [:show-error (str "Export failed: " (.-message error))]]]}))
       {:fx [[:dispatch [:show-error "No AST available for export"]]]}))))

(rf/reg-event-fx
 :show-export-result
 (fn [{:keys [db]} [_ content export-type]]
   "Display export result to user"
   (println "Export result (" export-type "):\n" content)
   {:db db}))

;; -- Validation Framework --

(defn validate-export-result
  "Validate that an export result is well-formed"
  [export-result]
  (and (map? export-result)
       (contains? export-result :processed-ast)
       (contains? export-result :export-type)
       (contains? export-result :metadata)))

;; Export the main API functions
(def api
  {:process-ast-node process-ast-node
   :export-ast export-ast
   :register-exporter! register-exporter!
   :get-available-exporters get-available-exporters
   :export-with-registered-processor export-with-registered-processor})