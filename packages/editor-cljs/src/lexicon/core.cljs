(ns lexicon.core
  (:require [re-frame.core :as rf]
            [reagent.dom :as rdom]
            [lexicon.events]    ; Load event handlers
            [lexicon.subs]      ; Load subscriptions
            [lexicon.views :as views]))

(defn load-wasm-module
  "Asynchronously load the WebAssembly module"
  []
  (-> (js/fetch "/core-wasm/build/debug.wasm")
      (.then #(.arrayBuffer %))
      (.then #(js/WebAssembly.instantiate % #js {:env #js {:abort #(throw (js/Error. "WASM aborted"))}}))
      (.then (fn [result]
               (let [wasm-instance (.-exports (.-instance result))]
                 ;; Initialize the WASM module
                 (.init wasm-instance)
                 ;; Store in re-frame db
                 (rf/dispatch [:wasm-module-loaded wasm-instance])
                 (println "✅ WASM module loaded and initialized"))))
      (.catch (fn [error]
                (println "❌ Failed to load WASM module:" error)))))

(defn mount-app
  "Mount the main application component"
  []
  (let [container (.getElementById js/document "editor")]
    (rdom/render [views/main-app] container)))

(defn init
  "Initialize the Lexicon editor application"
  []
  (println "🚀 Initializing Lexicon editor...")
  
  ;; Initialize re-frame database
  (rf/dispatch [:initialize-db])
  
  ;; Mount the React application
  (mount-app)
  
  ;; Load WASM module asynchronously
  (load-wasm-module))

(defn ^:export main []
  (init))