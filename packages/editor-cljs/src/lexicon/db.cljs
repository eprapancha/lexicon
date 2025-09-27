(ns lexicon.db)

(def default-db
  "Default application state database for re-frame"
  {:wasm-handle nil                                    ; WebAssembly module instance
   :initialized? false                                 ; Whether WASM module is loaded
   :buffers {}                                         ; Map of buffer-id -> buffer-data
   :active-buffer-id nil                              ; Currently active buffer ID
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
  [buffer-id content]
  {:id buffer-id
   :content content
   :length (count content)
   :modified? false
   :created-at (js/Date.now)
   :last-modified (js/Date.now)})

(defn default-buffer
  "Create a default empty buffer"
  []
  (create-buffer :default ""))