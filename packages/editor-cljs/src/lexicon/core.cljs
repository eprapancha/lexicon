(ns lexicon.core
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]
            [lexicon.db :as db]  ; For find-window-in-tree
            [lexicon.log]       ; Load log bus (Issue #73)
            [lexicon.events]    ; Load event handlers
            [lexicon.subs]      ; Load subscriptions
            [lexicon.lsp]       ; Load LSP handlers
            [lexicon.effects]   ; Load DOM effect handlers
            [lexicon.effects.log]   ; Load log effect handlers (Issue #73)
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
            [lexicon.buffer-local]        ; Load buffer-local variables (Phase 6D Week 1)
            [lexicon.hooks]               ; Load enhanced hooks (Phase 6D Week 1)
            [lexicon.advice]              ; Load advice system (Phase 6D Week 2)
            [lexicon.thing-at-point]      ; Load thing-at-point (Phase 6D Week 2)
            [lexicon.advanced-undo]       ; Load advanced undo (Phase 6D Week 2)
            [lexicon.commands.universal-argument]  ; Load prefix arguments (Phase 6.5 Week 1-2)
            [lexicon.variables]           ; Load buffer-local variables (Phase 6.5 Week 5-6)
            [lexicon.modes]               ; Load minor mode infrastructure (Phase 6.5 Week 5-6)
            [lexicon.modes.line-number]   ; Load line-number-mode (Phase 6.5 Week 5-6)
            [lexicon.modes.auto-save]     ; Load auto-save-mode (Phase 6.5 Week 5-6)
            [lexicon.eval]                ; Load runtime evaluation (Phase 6.5 Week 7-8)
            [lexicon.init]                ; Load init file system (Phase 6.5 Week 7-8)
            [lexicon.views :as views]
            [lexicon.package-loader]   ; Load all packages
            ;; Re-export core API functions
            [lexicon.api.message :as msg]
            [lexicon.api.buffer :as buf]))

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

(defn- expose-editor-state-for-tests
  "Expose editor state to JavaScript for E2E tests.
   Creates window.editorState with properties:
   - point: current cursor position (linear)
   - buffer: current buffer text
   - prefixArg: current prefix argument (Phase 6.5)
   - minibufferStack: minibuffer stack for depth tracking (Phase 6.5 Week 3-4)
   - messagesBuffer: *Messages* buffer text (Issue #84)"
  []
  (when goog.DEBUG  ; Only in development mode
    (let [get-state (fn []
                      (let [app-db @re-frame.db/app-db
                            active-window-id (:active-window-id app-db)
                            window-tree (:window-tree app-db)
                            active-window (when window-tree
                                            (db/find-window-in-tree window-tree active-window-id))
                            buffer-id (:buffer-id active-window)
                            buffer (get-in app-db [:buffers buffer-id])
                            wasm-instance (:wasm-instance buffer)
                            cursor-pos (get-in app-db [:ui :cursor-position] 0)
                            buffer-text (when wasm-instance (.getText wasm-instance))
                            prefix-arg (:prefix-arg app-db)
                            ;; Convert prefix-arg to JS-friendly format
                            prefix-arg-js (cond
                                            (nil? prefix-arg) nil
                                            (list? prefix-arg) (clj->js prefix-arg)
                                            (= prefix-arg '-) "-"
                                            :else prefix-arg)
                            ;; Expose minibuffer stack (Phase 6.5 Week 3-4)
                            minibuffer-stack (:minibuffer-stack app-db)
                            minibuffer-stack-js (clj->js minibuffer-stack)
                            ;; Expose *Messages* buffer (Issue #84)
                            messages-buffer (get-in app-db [:buffers 2])
                            messages-wasm (:wasm-instance messages-buffer)
                            messages-text (when messages-wasm
                                           (try (.getText messages-wasm)
                                                (catch :default _ "")))]
                        #js {:point cursor-pos
                             :buffer (or buffer-text "")
                             :prefixArg prefix-arg-js
                             :minibufferStack minibuffer-stack-js
                             :messagesBuffer (or messages-text "")}))]
      ;; Expose as a getter function so it always returns fresh state
      (aset js/window "editorState"
            (js/Object.defineProperty
             #js {}
             "valueOf"
             #js {:get get-state
                  :enumerable true
                  :configurable true}))
      ;; Also expose direct properties for simpler access
      (js/Object.defineProperty
       js/window
       "editorState"
       #js {:get get-state
            :enumerable true
            :configurable true}))))

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

  ;; Initialize minor modes (Phase 6.5 Week 5-6)
  (lexicon.modes.line-number/init-line-number-mode!)
  (lexicon.modes.auto-save/init-auto-save-mode!)

  ;; Mount the React application
  (mount-app)

  ;; Expose state for E2E tests
  (expose-editor-state-for-tests)

  ;; Load WASM module asynchronously
  (load-wasm-module)

  ;; Load user init file after everything else is initialized (Phase 6.5 Week 7-8)
  ;; We do this after WASM loading to ensure all systems are ready
  (js/setTimeout #(rf/dispatch [:init/load-file]) 1000))

(defn ^:export main []
  (init))

;; -- Core API Re-exports --
;; These are the main entry points for packages and extensions.
;; Usage: (:require [lexicon.core :refer [message point goto-char ...]])

;; Message/Minibuffer API
(def message
  "Display a message in the minibuffer and append to *Messages* buffer.
   See: lexicon.api.message/message"
  msg/message)

;; Buffer API
(def point
  "Return current point (cursor position) as linear byte offset.
   See: lexicon.api.buffer/point"
  buf/point)

(def point-min
  "Return minimum valid point position (always 0).
   See: lexicon.api.buffer/point-min"
  buf/point-min)

(def point-max
  "Return maximum valid point position (end of buffer).
   See: lexicon.api.buffer/point-max"
  buf/point-max)

(def goto-char
  "Move cursor to position POS (linear byte offset).
   See: lexicon.api.buffer/goto-char"
  buf/goto-char)

(def buffer-string
  "Return entire buffer contents as string.
   See: lexicon.api.buffer/buffer-string"
  buf/buffer-string)

(def buffer-substring
  "Return text between START and END positions.
   See: lexicon.api.buffer/buffer-substring"
  buf/buffer-substring)

(def line-beginning-position
  "Return position of beginning of current line.
   See: lexicon.api.buffer/line-beginning-position"
  buf/line-beginning-position)

(def line-end-position
  "Return position of end of current line.
   See: lexicon.api.buffer/line-end-position"
  buf/line-end-position)

(def get-buffer
  "Get buffer by name. Returns buffer map or nil.
   See: lexicon.api.buffer/get-buffer"
  buf/get-buffer)

(def buffer-name
  "Return name of current buffer.
   See: lexicon.api.buffer/buffer-name"
  buf/buffer-name)

(def buffer-size
  "Return size of buffer in characters.
   See: lexicon.api.buffer/buffer-size"
  buf/buffer-size)

(def buffer-local-value
  "Get value of buffer-local variable VAR in BUFFER.
   See: lexicon.api.buffer/buffer-local-value"
  buf/buffer-local-value)
