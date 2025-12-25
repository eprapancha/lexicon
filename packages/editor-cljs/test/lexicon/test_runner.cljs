(ns lexicon.test-runner
  "Test runner for all Lexicon tests."
  (:require [cljs.test :refer [run-tests]]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.events]
            [lexicon.subs]
            [lexicon.db]
            [lexicon.core-test]
            [lexicon.window-test]
            [lexicon.regression-test]))

(defn load-wasm-module-for-tests
  "Load WASM module for testing"
  []
  (println "🔍 Loading WASM module for tests...")
  (let [wasm-js-path "/lexicon-engine/wasm/pkg/lexicon_wasm.js"
        wasm-bg-path "/lexicon-engine/wasm/pkg/lexicon_wasm_bg.wasm"]

    (-> (js/eval (str "import('" wasm-js-path "')"))
        (.then (fn [^js wasm-module]
                 (println "✅ WASM JS module loaded")
                 (let [^js init-fn (.-default wasm-module)]
                   (-> (init-fn #js {:module_or_path wasm-bg-path})
                     (.then (fn []
                              (println "✅ WASM initialized")
                              (let [WasmGapBuffer (.-WasmGapBuffer ^js wasm-module)
                                    wasm-instance (new WasmGapBuffer "")]
                                (println "✅ WasmGapBuffer created for tests")
                                (rf/dispatch [:wasm-module-loaded {:instance wasm-instance
                                                                  :constructor WasmGapBuffer}])
                                ;; Run tests after WASM is loaded
                                (run-all-tests))))
                     (.catch (fn [error]
                               (println "❌ Failed to initialize WASM:" error)))))))
        (.catch (fn [error]
                  (println "❌ Failed to load WASM module:" error))))))

(defn ^:export run-all-tests []
  (println "\n🧪 Running Lexicon test suite...\n")
  (run-tests 'lexicon.core-test
             'lexicon.window-test
             'lexicon.regression-test))

(defn ^:export init-tests []
  (println "🚀 Initializing test environment...")

  ;; Initialize re-frame database
  (rf/dispatch-sync [:initialize-db])

  ;; Initialize faces
  (rf/dispatch-sync [:faces/initialize])

  ;; Initialize themes
  (rf/dispatch-sync [:theme/initialize])

  (println "✅ Test environment initialized")

  ;; Load WASM and run tests
  (load-wasm-module-for-tests))

;; Auto-run on load for browser-test
(defn ^:dev/after-load after-load []
  (init-tests))
