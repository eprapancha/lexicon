(ns lexicon.core.constants
  "Constants for the Lexicon editor to eliminate magic numbers")

;; Transaction types that correspond to WASM API
(def TRANSACTION_INSERT 0)
(def TRANSACTION_DELETE 1)
(def TRANSACTION_REPLACE 2)
(def TRANSACTION_COMPOUND 3)

;; Editor defaults
(def DEFAULT_LINE_HEIGHT 15)
(def DEFAULT_VIEWPORT_LINES 50)
(def DEFAULT_FONT_SIZE 14)

;; Performance tuning
(def CACHE_MAX_SIZE 1000)
(def KILL_RING_MAX_SIZE 60)

;; Error codes
(def WASM_SUCCESS 0)
(def WASM_ERROR 1)
(def WASM_PARSE_ERROR 2)