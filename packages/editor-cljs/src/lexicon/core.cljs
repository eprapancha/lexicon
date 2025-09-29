(ns lexicon.core
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [lexicon.events]    ; Load event handlers
            [lexicon.subs]      ; Load subscriptions
            [lexicon.views :as views]))

(defn load-wasm-module
  "Asynchronously load the WebAssembly module"
  []
  ;; Create a script tag to load the WASM module
  (let [script (.createElement js/document "script")]
    (set! (.-src script) "./lexicon-engine/wasm/pkg/lexicon_wasm.js")
    (set! (.-type script) "module")
    (set! (.-onload script)
          (fn []
            (try
              ;; Access the globally available module
              (if (exists? js/wasm_bindgen)
                (-> (js/wasm_bindgen "./lexicon-engine/wasm/pkg/lexicon_wasm_bg.wasm")
                    (.then (fn []
                             ;; Create a WasmEditorCore instance with initial content
                             (let [wasm-instance (js/WasmEditorCore.)]
                               (.init wasm-instance "")
                               ;; Store in re-frame db
                               (rf/dispatch [:wasm-module-loaded wasm-instance])
                               (println "✅ WASM module loaded and initialized"))))
                    (.catch (fn [error]
                              (println "❌ Failed to initialize WASM:" error))))
                (println "❌ WASM bindgen not available"))
              (catch js/Error error
                (println "❌ Failed to load WASM module:" error)))))
    (set! (.-onerror script)
          (fn [error]
            (println "❌ Failed to load WASM script:" error)))
    (.appendChild (.-head js/document) script)))

(defn mount-app
  "Mount the main application component"
  []
  (let [container (.getElementById js/document "editor")]
    (if (exists? js/ReactDOM.createRoot)
      ;; React 18+ API
      (let [root (js/ReactDOM.createRoot container)]
        (.render root (r/as-element [views/main-app])))
      ;; Fallback to React 17 API
      (rdom/render [views/main-app] container))))

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