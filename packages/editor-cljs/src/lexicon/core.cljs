(ns lexicon.core
  (:require [re-frame.core :as rf]
            [reagent.dom :as rdom]
            [lexicon.events]    ; Load event handlers
            [lexicon.subs]      ; Load subscriptions
            [lexicon.views :as views]))

(defn load-wasm-module
  "Asynchronously load the WebAssembly module"
  []
  ;; Load the WASM module from the generated package
  (-> (js/import "./lexicon-engine/wasm/pkg/lexicon_wasm.js")
      (.then (fn [wasm-module]
               ;; Initialize the WASM module first
               (-> (.default wasm-module)
                   (.then (fn []
                            ;; Create a WasmEditorCore instance with initial content
                            (let [WasmEditorCore (.-WasmEditorCore ^js wasm-module)
                                  wasm-instance (WasmEditorCore.)]
                              (.init wasm-instance "")
                              ;; Store in re-frame db
                              (rf/dispatch [:wasm-module-loaded wasm-instance])
                              (println "✅ WASM module loaded and initialized")))))))
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