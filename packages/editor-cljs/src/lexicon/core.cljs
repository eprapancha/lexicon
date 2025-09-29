(ns lexicon.core
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]
            [lexicon.events]    ; Load event handlers
            [lexicon.subs]      ; Load subscriptions
            [lexicon.views :as views]))

(defn load-wasm-module
  "Asynchronously load the WebAssembly module using dynamic import"
  []
  (try
    (println "🔍 Loading WASM module...")
    ;; Get the base path from the current document location
    (let [pathname (.-pathname (.-location js/document))
          path-parts (js->clj (.split pathname "/"))
          base-parts (butlast path-parts)
          base-path (str/join "/" base-parts)
          wasm-js-path (str base-path "/lexicon-engine/wasm/pkg/lexicon_wasm.js")
          wasm-bg-path (str base-path "/lexicon-engine/wasm/pkg/lexicon_wasm_bg.wasm")]
      
      (println "🔍 Base path:" base-path)
      (println "🔍 WASM JS path:" wasm-js-path)
      
      ;; Use JavaScript's dynamic import() function with dynamic path
      (-> (js/eval (str "import('" wasm-js-path "')"))
          (.then (fn [wasm-module]
                   (println "✅ WASM JS module loaded")
                   ;; Initialize the WASM module - the default export is the init function
                   (let [init-fn (.-default wasm-module)]
                     (-> (init-fn wasm-bg-path)
                       (.then (fn []
                                (println "✅ WASM initialized")
                                ;; WasmEditorCore is available as a named export
                                (let [WasmEditorCore (.-WasmEditorCore wasm-module)
                                      wasm-instance (WasmEditorCore.)]
                                  (println "✅ WasmEditorCore created")
                                  (.init wasm-instance "")
                                  ;; Store both the instance and the constructor
                                  (rf/dispatch [:wasm-module-loaded {:instance wasm-instance
                                                                    :constructor WasmEditorCore}])
                                  (println "✅ WASM module loaded and initialized"))))
                       (.catch (fn [error]
                                 (println "❌ Failed to initialize WASM:" error)
                                 (rf/dispatch [:wasm-load-failed error])))))))
          (.catch (fn [error]
                    (println "❌ Failed to load WASM module:" error)
                    (rf/dispatch [:wasm-load-failed error])))))
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