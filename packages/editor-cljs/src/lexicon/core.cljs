(ns lexicon.core
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]
            [lexicon.events]    ; Load event handlers
            [lexicon.subs]      ; Load subscriptions
            [lexicon.lsp]       ; Load LSP handlers
            [lexicon.effects]   ; Load DOM effect handlers
            [lexicon.ui.faces]  ; Load face system
            [lexicon.ui.text-properties]  ; Load text properties (Phase 6B Week 2)
            [lexicon.ui.overlays]         ; Load overlay system (Phase 6B Week 2)
            [lexicon.ui.frames]           ; Load child frames (Phase 6B Week 2)
            [lexicon.modes.special-mode]  ; Load special-mode (Phase 6B Week 3)
            [lexicon.ui.mode-line]        ; Load mode-line formatter (Phase 6B Week 3)
            [lexicon.modes.help-mode]     ; Load help-mode (Phase 6B Week 3)
            [lexicon.modes.buffer-menu-mode]  ; Load buffer-menu-mode (Phase 6B Week 3)
            [lexicon.ui.themes]           ; Load theme system (Phase 6B Week 4)
            [lexicon.completion.metadata] ; Load completion metadata (Phase 6C Week 1-2)
            [lexicon.completion.styles]   ; Load completion styles (Phase 6C Week 3-4)
            [lexicon.completion.tables]   ; Load completion tables (Phase 6C Week 5)
            [lexicon.completion.capf]     ; Load completion-at-point (Phase 6C Week 5)
            [lexicon.packages.project]    ; Load project.el (Phase 6C Week 6)
            [lexicon.packages.imenu]      ; Load imenu (Phase 6C Week 6)
            [lexicon.packages.recentf]    ; Load recentf (Phase 6C Week 6)
            [lexicon.packages.savehist]   ; Load savehist (Phase 6C Week 6)
            [lexicon.views :as views]
            [lexicon.package-loader])) ; Load all packages

(defn load-wasm-module
  "Asynchronously load the WebAssembly module using dynamic import"
  []
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
        (.then (fn [^js wasm-module]
                 (println "✅ WASM JS module loaded")
                 ;; Initialize the WASM module - the default export is the init function
                 (let [^js init-fn (.-default wasm-module)]
                   (-> (init-fn #js {:module_or_path wasm-bg-path})
                     (.then (fn []
                              (println "✅ WASM initialized")
                              ;; WasmGapBuffer is available as a named export
                              (let [WasmGapBuffer (.-WasmGapBuffer ^js wasm-module)
                                    wasm-instance (new WasmGapBuffer "")]
                                (println "✅ WasmGapBuffer created")
                                ;; Store both the instance and the constructor
                                (rf/dispatch [:wasm-module-loaded {:instance wasm-instance
                                                                  :constructor WasmGapBuffer}])
                                (println "✅ WASM Gap Buffer loaded and initialized"))))
                     (.catch (fn [error]
                               (println "❌ Failed to initialize WASM:" error)
                               (rf/dispatch [:wasm-load-failed error])))))))
        (.catch (fn [error]
                  (println "❌ Failed to load WASM module:" error)
                  (rf/dispatch [:wasm-load-failed error]))))))

(defn mount-app
  "Mount the main application component"
  []
  (let [container (.getElementById js/document "editor")]
    ;; Use standard Reagent render - it handles React 18 internally
    (rdom/render [views/main-app] container)))

(defn init
  "Initialize the Lexicon editor application"
  []
  (println "🚀 Initializing Lexicon editor...")

  ;; Initialize re-frame database
  (rf/dispatch [:initialize-db])

  ;; Initialize face system (Phase 6B Week 1)
  (rf/dispatch [:faces/initialize])

  ;; Initialize theme system (Phase 6B Week 4)
  (rf/dispatch [:theme/initialize])

  ;; Mount the React application
  (mount-app)

  ;; Load WASM module asynchronously
  (load-wasm-module))

(defn ^:export main []
  (init))
