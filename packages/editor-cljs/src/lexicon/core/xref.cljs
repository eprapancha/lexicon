(ns lexicon.core.xref
  "Cross-reference and definition lookup (xref.el).

  Implements Emacs xref.el core functionality:
  - xref-find-definitions (M-.): Jump to definition
  - xref-find-references: Find all references
  - xref-go-back (M-,): Return to previous location
  - xref-find-apropos: Search for pattern in definitions

  Navigation uses a marker stack to track jump history.

  Key bindings:
  - M-. : xref-find-definitions
  - M-, : xref-go-back
  - M-? : xref-find-references

  Based on Emacs lisp/progmodes/xref.el"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Xref Marker Stack
;; =============================================================================

(rf/reg-event-db
 :xref/push-marker
 (fn [db [_ marker]]
   (update-in db [:xref :marker-stack] (fnil conj []) marker)))

(rf/reg-event-fx
 :xref/pop-marker
 (fn [{:keys [db]} [_]]
   (let [stack (get-in db [:xref :marker-stack] [])
         marker (peek stack)]
     (if marker
       {:db (update-in db [:xref :marker-stack] pop)
        :fx [[:dispatch [:switch-buffer (:buffer-id marker)]]
             [:dispatch [:goto-char (:buffer-id marker) (:position marker)]]]}
       {:fx [[:dispatch [:echo/message "Xref marker stack is empty"]]]}))))

;; =============================================================================
;; Xref Commands
;; =============================================================================

(rf/reg-event-fx
 :xref/find-definitions
 (fn [{:keys [db]} [_]]
   "Find definitions of identifier at point (M-.)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         cursor-pos (get-in db [:ui :cursor-position] 0)
         buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)
         text (when wasm (try (.getText ^js wasm) (catch :default _ "")))
         ;; Extract identifier at point
         identifier (when text
                      (let [before (subs text (max 0 (- cursor-pos 50)) cursor-pos)
                            after (subs text cursor-pos (min (count text) (+ cursor-pos 50)))
                            word-before (re-find #"[\w-]+$" before)
                            word-after (re-find #"^[\w-]+" after)]
                        (str (or word-before "") (or word-after ""))))]
     ;; Save current position to marker stack
     (when buffer-id
       (rf/dispatch [:xref/push-marker {:buffer-id buffer-id
                                         :position cursor-pos}]))
     (if (and identifier (seq identifier))
       {:fx [[:dispatch [:echo/message
                         (str "xref-find-definitions: " identifier
                              " (no backend available)")]]]}
       {:fx [[:dispatch [:echo/message "No identifier at point"]]]}))))

(rf/reg-event-fx
 :xref/find-references
 (fn [{:keys [db]} [_]]
   "Find references to identifier at point (M-?)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         cursor-pos (get-in db [:ui :cursor-position] 0)
         buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)
         text (when wasm (try (.getText ^js wasm) (catch :default _ "")))
         identifier (when text
                      (let [before (subs text (max 0 (- cursor-pos 50)) cursor-pos)
                            after (subs text cursor-pos (min (count text) (+ cursor-pos 50)))
                            word-before (re-find #"[\w-]+$" before)
                            word-after (re-find #"^[\w-]+" after)]
                        (str (or word-before "") (or word-after ""))))]
     (if (and identifier (seq identifier))
       {:fx [[:dispatch [:echo/message
                         (str "xref-find-references: " identifier
                              " (no backend available)")]]]}
       {:fx [[:dispatch [:echo/message "No identifier at point"]]]}))))

(rf/reg-event-fx
 :xref/go-back
 (fn [_ [_]]
   "Return to previous location in xref marker stack (M-,)."
   {:fx [[:dispatch [:xref/pop-marker]]]}))

(rf/reg-event-fx
 :xref/find-apropos
 (fn [{:keys [db]} [_]]
   "Search for pattern in definitions."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Xref apropos (regexp): "
                      :on-confirm [:xref/find-apropos-exec]}]]]}))

(rf/reg-event-fx
 :xref/find-apropos-exec
 (fn [{:keys [db]} [_ pattern]]
   {:fx [[:dispatch [:echo/message
                     (str "xref-find-apropos: " pattern
                          " (no backend available)")]]]}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize xref module and register commands."
  []
  (rf/dispatch [:register-command :xref-find-definitions
                {:docstring "Find definitions of identifier at point (M-.)"
                 :interactive nil
                 :handler [:xref/find-definitions]}])

  (rf/dispatch [:register-command :xref-find-references
                {:docstring "Find all references to identifier at point (M-?)"
                 :interactive nil
                 :handler [:xref/find-references]}])

  (rf/dispatch [:register-command :xref-go-back
                {:docstring "Return to previous location (M-,)"
                 :interactive nil
                 :handler [:xref/go-back]}])

  (rf/dispatch [:register-command :xref-find-apropos
                {:docstring "Search for pattern in definitions"
                 :interactive nil
                 :handler [:xref/find-apropos]}]))
