(ns lexicon.core.main
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]
            [lexicon.core.db :as db]  ; For find-window-in-tree
            [lexicon.core.log]       ; Load log bus (Issue #73)
            [lexicon.core.events]    ; Load event handlers (MUST be before eval)
            [lexicon.core.eval :as eval]  ; For E2E test eval
            [lexicon.core.subs]      ; Load subscriptions
            [lexicon.core.lsp]       ; Load LSP handlers
            [lexicon.core.effects]   ; Load DOM effect handlers
            [lexicon.core.effects.log]   ; Load log effect handlers (Issue #73)
            [lexicon.core.ui.faces]  ; Load face system
            [lexicon.core.ui.text-properties]  ; Load text properties (Phase 6B Week 2)
            [lexicon.core.ui.overlays]         ; Load overlay system (Phase 6B Week 2)
            [lexicon.core.ui.frames]           ; Load child frames (Phase 6B Week 2)
            [lexicon.core.modes.special-mode]  ; Load special-mode (Phase 6B Week 3)
            [lexicon.core.ui.mode-line]        ; Load mode-line formatter (Phase 6B Week 3)
            [lexicon.core.modes.help-mode]     ; Load help-mode (Phase 6B Week 3)
            [lexicon.core.modes.buffer-menu-mode]  ; Load buffer-menu-mode (Phase 6B Week 3)
            [lexicon.core.ui.themes]           ; Load theme system (Phase 6B Week 4)
            [lexicon.core.completion.metadata] ; Load completion metadata (Phase 6C Week 1-2)
            [lexicon.core.completion.styles]   ; Load completion styles (Phase 6C Week 3-4)
            [lexicon.core.completion.tables]   ; Load completion tables (Phase 6C Week 5)
            [lexicon.core.completion.capf]     ; Load completion-at-point (Phase 6C Week 5)
            [lexicon.core.packages.project]    ; Load project.el (Phase 6C Week 6)
            [lexicon.core.packages.imenu]      ; Load imenu (Phase 6C Week 6)
            [lexicon.core.packages.recentf]    ; Load recentf (Phase 6C Week 6)
            [lexicon.core.packages.savehist]   ; Load savehist (Phase 6C Week 6)
            [lexicon.core.buffer-local]        ; Load buffer-local variables (Phase 6D Week 1)
            [lexicon.core.hooks]               ; Load enhanced hooks (Phase 6D Week 1)
            [lexicon.core.advice]              ; Load advice system (Phase 6D Week 2)
            [lexicon.core.thing-at-point]      ; Load thing-at-point (Phase 6D Week 2)
            [lexicon.core.advanced-undo]       ; Load advanced undo (Phase 6D Week 2)
            [lexicon.core.commands.universal-argument]  ; Load prefix arguments (Phase 6.5 Week 1-2)
            [lexicon.core.variables]           ; Load buffer-local variables (Phase 6.5 Week 5-6)
            [lexicon.core.modes]               ; Load minor mode infrastructure (Phase 6.5 Week 5-6)
            [lexicon.core.modes.line-number]   ; Load line-number-mode (Phase 6.5 Week 5-6)
            [lexicon.core.modes.auto-save]     ; Load auto-save-mode (Phase 6.5 Week 5-6)
            [lexicon.core.modes.electric-pair] ; Load electric-pair-mode
            [lexicon.core.modes.delete-selection] ; Load delete-selection-mode (#121)
            [lexicon.core.modes.hl-line]       ; Load hl-line-mode (#123)
            [lexicon.core.modes.show-paren]    ; Load show-paren-mode (#123)
            [lexicon.core.modes.whitespace]    ; Load whitespace-mode (#123)
            [lexicon.core.modes.display-line-numbers]  ; Load display-line-numbers-mode (#123)
            [lexicon.core.kmacro]              ; Load keyboard macros (#121)
            [lexicon.core.modes.rectangle]     ; Load rectangle operations (#121)
            [lexicon.core.dabbrev]             ; Load dabbrev M-/ expansion (#120)
            [lexicon.core.icomplete]           ; Load icomplete incremental completion (#128)
            [lexicon.core.windmove]            ; Load windmove S-arrow navigation (#117)
            [lexicon.core.eval]                ; Load runtime evaluation (Phase 6.5 Week 7-8)
            [lexicon.core.init]                ; Load init file system (Phase 6.5 Week 7-8)
            [lexicon.core.views :as views]
            [lexicon.core.package-loader]   ; Load all packages
            ;; Re-export core API functions
            [lexicon.core.api.message :as msg]
            [lexicon.core.api.buffer :as buf]))

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
   - messagesBuffer: *Messages* buffer text (Issue #84)
   - windowList: array of window IDs (for window count tests)
   - selectedWindow: currently active window ID
   - killRing: array of kill ring entries
   - mark: current mark position in active window (nil if not set)"
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
                                                (catch :default _ "")))
                            ;; Expose window list for E2E tests (Tier 2 fix)
                            all-windows (when window-tree
                                          (db/get-all-leaf-windows window-tree))
                            window-ids (mapv :id all-windows)
                            ;; Expose kill ring for E2E tests (Tier 2 fix)
                            kill-ring (:kill-ring app-db)
                            ;; Expose mark position from active window (Tier 2 fix)
                            mark-position (:mark-position active-window)]
                        #js {:point cursor-pos
                             :buffer (or buffer-text "")
                             :prefixArg prefix-arg-js
                             :minibufferStack minibuffer-stack-js
                             :messagesBuffer (or messages-text "")
                             :windowList (clj->js window-ids)
                             :selectedWindow active-window-id
                             :killRing (clj->js kill-ring)
                             :mark mark-position}))]
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
            :configurable true}))
    ;; Expose re-frame dispatch for testing (Issue #97)
    (aset js/window "lexiconDispatch" rf/dispatch)
    ;; Expose evalLisp for E2E tests (Issue #101)
    (aset js/window "evalLisp"
          (fn [code-str]
            (clj->js (eval/eval-string code-str))))))

(defn init
  "Initialize the Lexicon editor application"
  []
  (println "🚀 Initializing Lexicon editor...")

  ;; Initialize re-frame database
  (rf/dispatch-sync [:initialize-db])

  ;; Initialize built-in commands first
  (rf/dispatch-sync [:initialize-commands])

  ;; Initialize eval commands (must be after :initialize-commands)
  (eval/init-eval-commands!)

  ;; Initialize init file commands (must be after :initialize-commands)
  (lexicon.core.init/init-init-commands!)

  ;; Register all packages (must be after db init)
  ;; Called directly, not via event, because define-command uses dispatch-sync
  (lexicon.core.package-loader/register-all-packages!)

  ;; Initialize face system (Phase 6B Week 1)
  (rf/dispatch [:faces/initialize])

  ;; Initialize theme system (Phase 6B Week 4)
  (rf/dispatch [:theme/initialize])

  ;; Initialize minor modes (Phase 6.5 Week 5-6)
  (lexicon.core.modes.line-number/init-line-number-mode!)
  (lexicon.core.modes.auto-save/init-auto-save-mode!)
  (lexicon.core.modes.electric-pair/init-electric-pair-mode!)
  (lexicon.core.modes.delete-selection/init-delete-selection-mode!)
  (lexicon.core.modes.hl-line/init-hl-line-mode!)
  (lexicon.core.modes.show-paren/init-show-paren-mode!)
  (lexicon.core.modes.whitespace/init-whitespace-mode!)
  (lexicon.core.modes.display-line-numbers/init-display-line-numbers-mode!)

  ;; Initialize keyboard macros (#121)
  (lexicon.core.kmacro/init-kmacro!)

  ;; Initialize rectangle operations (#121)
  (lexicon.core.modes.rectangle/init-rectangle!)

  ;; Initialize dabbrev M-/ expansion (#120)
  (lexicon.core.dabbrev/init-dabbrev!)

  ;; Initialize icomplete incremental completion (#128)
  (lexicon.core.icomplete/init-icomplete!)

  ;; Initialize windmove S-arrow navigation (#117)
  (lexicon.core.windmove/init-windmove!)

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
;; Usage: (:require [lexicon.core.core :refer [message point goto-char ...]])

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
