(ns lexicon.effects.test
  "Re-frame effect handlers for test signals (Issue #74)"
  (:require [re-frame.core :as rf]
            [lexicon.test.signals :as signals]))

;; Effect handler to emit test signals
(rf/reg-fx
 :test/signal
 (fn [signal-key]
   "Emit a test signal for event-driven test synchronization.

   Usage in event:
   {:fx [[:test/signal :wasm-loaded]
         [:test/signal :ui-ready]]}"
   (signals/signal! signal-key)))

;; Effect handler to clear test signals
(rf/reg-fx
 :test/clear-signal
 (fn [signal-key]
   "Clear a specific test signal.

   Usage in event:
   {:fx [[:test/clear-signal :idle]]}"
   (signals/clear-signal! signal-key)))

;; Effect handler to reset all test signals
(rf/reg-fx
 :test/reset-signals
 (fn [_]
   "Reset all test signals (typically used between tests).

   Usage in event:
   {:fx [[:test/reset-signals nil]]}"
   (signals/reset-signals!)))
