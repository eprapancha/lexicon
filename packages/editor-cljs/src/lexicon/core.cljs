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
  (try
    (println "🔍 Loading WASM module...")
    ;; Try to load the WASM module using dynamic import via fetch
    (-> (js/fetch "./lexicon-engine/wasm/pkg/lexicon_wasm.js")
        (.then (fn [response]
                 (if (.-ok response)
                   (.text response)
                   (throw (js/Error. (str "Failed to fetch WASM JS: " (.-status response)))))))
        (.then (fn [js-code]
                 ;; Execute the WASM JavaScript code
                 (js/eval js-code)
                 ;; Now try to initialize
                 (if (exists? js/wasm_bindgen)
                   (-> (js/wasm_bindgen "./lexicon-engine/wasm/pkg/lexicon_wasm_bg.wasm")
                       (.then (fn []
                                (println "✅ WASM bindgen initialized")
                                ;; Check if WasmEditorCore is available
                                (if (exists? js/WasmEditorCore)
                                  (let [wasm-instance (js/WasmEditorCore.)]
                                    (println "✅ WasmEditorCore created")
                                    (.init wasm-instance "")
                                    (rf/dispatch [:wasm-module-loaded wasm-instance])
                                    (println "✅ WASM module loaded and initialized"))
                                  (throw (js/Error. "WasmEditorCore not available after WASM load")))))
                       (.catch (fn [error]
                                 (println "❌ Failed to initialize WASM bindgen:" error)
                                 (rf/dispatch [:wasm-load-failed error]))))
                   (throw (js/Error. "wasm_bindgen function not available after loading JS")))))
        (.catch (fn [error]
                  (println "❌ Failed to load WASM module:" error)
                  (rf/dispatch [:wasm-load-failed error]))))
    (catch js/Error error
      (println "❌ Failed to start WASM loading:" error)
      (rf/dispatch [:wasm-load-failed error]))))

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