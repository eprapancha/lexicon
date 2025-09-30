(ns lexicon.db
  (:require [lexicon.constants :as const]))

(def default-db
  "Default application state database for re-frame"
  {:buffers {1 {:id 1
                :wasm-instance nil                     ; Will be set when WASM loads
                :file-handle nil
                :name "*scratch*"
                :is-modified? false
                :mark-position nil}}                   ; For region selection
   :windows {1 {:id 1, :buffer-id 1, :viewport {:start-line 0, :end-line const/DEFAULT_VIEWPORT_LINES}}}
   :active-window-id 1
   :line-height const/DEFAULT_LINE_HEIGHT
   :kill-ring []                                       ; Clipboard history
   :initialized? false                                 ; Whether WASM module is loaded
   :ui {:cursor-position 0                            ; Current cursor position
        :selection {:start 0 :end 0}                  ; Current selection range
        :ime-composing? false                          ; IME composition state
        :ime-composition-text ""                       ; Current IME composition text
        :view-needs-update? false                      ; Flag to trigger view reconciliation
        :text-cache {}                                 ; Text range cache for performance
        :viewport {:start 0 :end 1000}                ; Currently visible text range
        :scroll-position 0}                            ; Current scroll position
   :editor {:mode :normal                             ; Editor mode (normal, insert, etc.)
            :keymap :emacs                             ; Active keymap
            :commands {}}                              ; Available commands
   :system {:last-transaction-id 0                    ; For transaction ordering
            :mutation-observer nil                     ; MutationObserver instance
            :reconciliation-active? false}})           ; Prevent recursive reconciliation

(defn create-buffer
  "Create a new buffer data structure"
  [buffer-id name wasm-instance]
  {:id buffer-id
   :wasm-instance wasm-instance
   :file-handle nil
   :name name
   :is-modified? false
   :mark-position nil})

(defn create-buffer-with-content
  "Create a new buffer with initial content"
  [buffer-id name content]
  (let [wasm-instance (when content
                        ;; Will need to create WasmEditorCore instance
                        ;; For now, return the structure and set instance later
                        nil)]
    (create-buffer buffer-id name wasm-instance)))

(defn next-buffer-id
  "Generate next available buffer ID"
  [buffers]
  (inc (apply max 0 (keys buffers))))