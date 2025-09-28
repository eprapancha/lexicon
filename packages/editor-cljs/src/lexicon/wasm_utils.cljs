(ns lexicon.wasm-utils
  "Utilities for working with WASM memory and string marshalling")

;; Enhanced WASM text range getter that handles string marshalling
(defn get-text-range-safe
  "Get text range from WASM with proper string marshalling.
   Returns [text-string success?] tuple."
  [wasm-handle start end]
  (try
    (let [length (.getLength wasm-handle)
          clamped-start (max 0 (min start length))
          clamped-end (max clamped-start (min end length))]
      
      (if (= clamped-start clamped-end)
        ["" true]
        (let [text (.getTextInRange wasm-handle clamped-start clamped-end)]
          ;; getTextInRange returns string directly according to TypeScript definition
          [text true])))
    (catch js/Error e
      (js/console.error "Failed to get text range:" (.-message e))
      ["" false])))

;; Enhanced character access
(defn get-char-safe
  "Get character at position with error handling."
  [wasm-handle position]
  (try
    (let [length (.getLength wasm-handle)]
      (if (and (>= position 0) (< position length))
        (.getCharacterAt wasm-handle position)
        nil))
    (catch js/Error e
      (js/console.error "Failed to get character:" (.-message e))
      nil)))

;; Test if WASM instance is properly initialized
(defn wasm-initialized?
  "Check if WASM instance is properly initialized and responsive."
  [wasm-handle]
  (try
    (and wasm-handle
         (>= (.getLength wasm-handle) 0))
    (catch js/Error e
      false)))