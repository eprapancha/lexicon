(ns lexicon.api.export-examples
  "Proof-of-concept backend implementations demonstrating the pluggable export framework"
  (:require [lexicon.api.export :as export]
            [re-frame.core :as rf]))

;; -- HTML Export Backend --

(defmethod export/process-ast-node "html-export"
  [node context]
  (case (:type node)
    "source_file"
    {:type :html-document
     :content (str "<html><head><title>Source Code</title></head><body>"
                  (when-let [children (:children node)]
                    (apply str (map #(export/process-ast-node % context) children)))
                  "</body></html>")}
    
    "function_definition"
    {:type :html-function
     :content (str "<div class='function'>"
                  "<h3>Function</h3>"
                  "<pre><code>" (:text node) "</code></pre>"
                  "</div>")}
    
    "class_definition"
    {:type :html-class
     :content (str "<div class='class'>"
                  "<h3>Class</h3>"
                  "<pre><code>" (:text node) "</code></pre>"
                  "</div>")}
    
    "comment"
    {:type :html-comment
     :content (str "<div class='comment'>"
                  "<em>" (:text node) "</em>"
                  "</div>")}
    
    ;; Default case - wrap in code block
    {:type :html-code
     :content (str "<code>" (:text node) "</code>")}))

;; -- Markdown Export Backend --

(defmethod export/process-ast-node "markdown-export"
  [node context]
  (case (:type node)
    "source_file"
    {:type :markdown-document
     :content (str "# Source Code Analysis\n\n"
                  (when-let [children (:children node)]
                    (apply str (map #(export/process-ast-node % context) children))))}
    
    "function_definition"
    {:type :markdown-function
     :content (str "## Function Definition\n\n"
                  "```javascript\n" (:text node) "\n```\n\n")}
    
    "class_definition"
    {:type :markdown-class
     :content (str "## Class Definition\n\n"
                  "```javascript\n" (:text node) "\n```\n\n")}
    
    "comment"
    {:type :markdown-comment
     :content (str "> " (clojure.string/trim (subs (:text node) 2)) "\n\n")}
    
    "keyword"
    {:type :markdown-keyword
     :content (str "**" (:text node) "**")}
    
    ;; Default case
    {:type :markdown-code
     :content (str "`" (:text node) "`")}))

;; -- Statistics Export Backend --

(defmethod export/process-ast-node "stats-export"
  [node context]
  (let [stats (atom {:total-nodes 0
                    :functions 0
                    :classes 0
                    :comments 0
                    :keywords 0
                    :strings 0
                    :numbers 0})]
    (letfn [(count-nodes [n]
              (swap! stats update :total-nodes inc)
              (case (:type n)
                "function_definition" (swap! stats update :functions inc)
                "class_definition" (swap! stats update :classes inc)
                "comment" (swap! stats update :comments inc)
                "keyword" (swap! stats update :keywords inc)
                "string" (swap! stats update :strings inc)
                "number" (swap! stats update :numbers inc)
                nil)
              (when-let [children (:children n)]
                (doseq [child children]
                  (count-nodes child))))]
      (count-nodes node))
    {:type :statistics
     :stats @stats
     :summary (str "Code Statistics:\n"
                  "- Total nodes: " (:total-nodes @stats) "\n"
                  "- Functions: " (:functions @stats) "\n"
                  "- Classes: " (:classes @stats) "\n"
                  "- Comments: " (:comments @stats) "\n"
                  "- Keywords: " (:keywords @stats) "\n"
                  "- Strings: " (:strings @stats) "\n"
                  "- Numbers: " (:numbers @stats) "\n")}))

;; -- AST Diff Export Backend --

(defmethod export/process-ast-node "diff-export"
  [node context]
  (let [previous-ast (get-in context [:options :previous-ast])
        changes (atom [])]
    (letfn [(compare-nodes [current previous path]
              (cond
                (and (nil? current) previous)
                (swap! changes conj {:type :deleted :path path :node previous})
                
                (and current (nil? previous))
                (swap! changes conj {:type :added :path path :node current})
                
                (and current previous (not= (:type current) (:type previous)))
                (swap! changes conj {:type :modified :path path :old previous :new current})
                
                (and current previous)
                (when-let [current-children (:children current)]
                  (let [previous-children (:children previous)]
                    (doseq [[idx child] (map-indexed vector current-children)]
                      (compare-nodes child (nth previous-children idx nil) (conj path idx)))))))]
      (when previous-ast
        (compare-nodes node previous-ast [])))
    {:type :diff-analysis
     :changes @changes
     :summary (str "AST Changes:\n"
                  "- Added: " (count (filter #(= (:type %) :added) @changes)) "\n"
                  "- Deleted: " (count (filter #(= (:type %) :deleted) @changes)) "\n"
                  "- Modified: " (count (filter #(= (:type %) :modified) @changes)) "\n")}))

;; -- Register Example Exporters --

(defn register-example-exporters!
  "Register all example export backends"
  []
  ;; HTML Exporter
  (export/register-exporter! :html
    {:name "HTML Export"
     :description "Export AST as formatted HTML with syntax highlighting"
     :processor (fn [ast options]
                  (let [processed (export/process-ast-node ast 
                                                          (export/create-export-context "html-export" options))]
                    (:content processed)))})
  
  ;; Markdown Exporter
  (export/register-exporter! :markdown
    {:name "Markdown Export"
     :description "Export AST as Markdown documentation"
     :processor (fn [ast options]
                  (let [processed (export/process-ast-node ast
                                                          (export/create-export-context "markdown-export" options))]
                    (:content processed)))})
  
  ;; Statistics Exporter
  (export/register-exporter! :statistics
    {:name "Code Statistics"
     :description "Generate statistical analysis of the code structure"
     :processor (fn [ast options]
                  (let [processed (export/process-ast-node ast
                                                          (export/create-export-context "stats-export" options))]
                    (:summary processed)))})
  
  ;; Diff Exporter
  (export/register-exporter! :diff
    {:name "AST Diff Analysis"
     :description "Compare current AST with a previous version"
     :processor (fn [ast options]
                  (let [processed (export/process-ast-node ast
                                                          (export/create-export-context "diff-export" options))]
                    (:summary processed)))}))

;; -- Command Integration --

(rf/reg-event-fx
 :examples/export-as-html
 (fn [{:keys [db]} [_]]
   "Export current buffer as HTML"
   {:fx [[:dispatch [:export/process-buffer :html {:include-metadata true}]]]}))

(rf/reg-event-fx
 :examples/export-as-markdown
 (fn [{:keys [db]} [_]]
   "Export current buffer as Markdown"
   {:fx [[:dispatch [:export/process-buffer :markdown {:include-metadata true}]]]}))

(rf/reg-event-fx
 :examples/show-statistics
 (fn [{:keys [db]} [_]]
   "Show code statistics for current buffer"
   {:fx [[:dispatch [:export/process-buffer :statistics {}]]]}))

;; -- Validation Tests --

(defn validate-export-framework
  "Validate that the export framework works correctly"
  []
  (let [test-ast {:type "source_file"
                 :children [{:type "function_definition"
                            :text "function test() { return 42; }"
                            :startPosition {:row 0 :column 0}
                            :endPosition {:row 0 :column 30}}
                           {:type "comment"
                            :text "// This is a test comment"
                            :startPosition {:row 1 :column 0}
                            :endPosition {:row 1 :column 25}}]}
        
        json-result (export/export-with-registered-processor test-ast :json {})
        html-result (export/export-with-registered-processor test-ast :html {})
        markdown-result (export/export-with-registered-processor test-ast :markdown {})
        stats-result (export/export-with-registered-processor test-ast :statistics {})]
    
    {:validation-results
     {:json {:success (string? json-result) :length (count json-result)}
      :html {:success (string? html-result) :contains-html (.includes html-result "<html>")}
      :markdown {:success (string? markdown-result) :contains-markdown (.includes markdown-result "#")}
      :statistics {:success (string? stats-result) :contains-stats (.includes stats-result "Total nodes")}}
     :test-ast test-ast}))

;; Auto-register examples on namespace load
(register-example-exporters!)

;; Export validation function for testing
(def validation-api
  {:validate-export-framework validate-export-framework
   :register-example-exporters! register-example-exporters!})