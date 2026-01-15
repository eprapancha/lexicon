(ns lexicon.test-setup
  "Test setup - loads event handlers and initializes WASM for tests"
  (:require [lexicon.events]   ; Register event handlers
            [lexicon.subs]     ; Register subscriptions
            [cljs.test :refer [use-fixtures async]]
            [re-frame.core :as rf]))

;; Load WASM module when this namespace loads
(.log js/console "🔍 Test setup namespace loaded, starting WASM load...")

(defonce wasm-load-promise
  (-> (js/Promise.resolve)
      (.then (fn []
               (.log js/console "Starting dynamic import...")
               (js/eval "import('/lexicon-engine/wasm/pkg/lexicon_wasm.js')")))
      (.then (fn [^js wasm-module]
               (.log js/console "✅ WASM JS module loaded")
               (let [init-fn (.-default wasm-module)]
                 ;; Wait for init to complete, then return the module
                 (-> (init-fn #js {:module_or_path "/lexicon-engine/wasm/pkg/lexicon_wasm_bg.wasm"})
                     (.then (fn [] wasm-module))))))
      (.then (fn [^js wasm-module]
               (.log js/console "✅ WASM initialized")
               (let [WasmGapBuffer (.-WasmGapBuffer wasm-module)
                     instance (new WasmGapBuffer "")]
                 (.log js/console "✅ Dispatching :wasm-module-loaded")
                 (rf/dispatch-sync [:wasm-module-loaded {:instance instance
                                                         :constructor WasmGapBuffer}])
                 :loaded)))
      (.catch (fn [e]
                (.error js/console "❌ WASM load failed:" e)
                :error))))

(.log js/console "Test setup: WASM load initiated")

;; Fixture to ensure WASM is loaded before tests run
(defn with-wasm-loaded [f]
  (async done
    (-> wasm-load-promise
        (.then (fn [result]
                 (if (= result :loaded)
                   (do
                     (.log js/console "✅ WASM ready, running test")
                     (f)
                     (done))
                   (do
                     (.error js/console "❌ WASM failed to load")
                     (done)))))
        (.catch (fn [e]
                  (.error js/console "❌ WASM fixture error:" e)
                  (done))))))
