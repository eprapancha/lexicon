(ns lexicon.packages.which-func
  "Which-func-mode: Display the current function in the mode line.

  This minor mode displays the name of the function/method/class the cursor
  is currently in. It uses imenu to determine the current function.

  Based on Emacs lisp/which-func.el

  This package uses only lisp.cljs primitives."
  (:require [lexicon.lisp :as lisp]
            [lexicon.core.packages.imenu :as imenu]
            [re-frame.db :as rfdb]
            [lexicon.core.db :as db]))

;; =============================================================================
;; State (package-local)
;; =============================================================================

(defonce which-func-enabled? (atom false))

;; =============================================================================
;; Function Detection
;; =============================================================================

(defn find-current-function
  "Find the function containing the cursor position.

   Args:
     index - Imenu index (list of {:name :position})
     cursor-pos - Current cursor position (linear)

   Returns:
     Function name string or nil if not in a function."
  [index cursor-pos]
  (when (and (seq index) cursor-pos)
    (let [;; Sort index by position descending to find the nearest function before cursor
          sorted (sort-by :position > index)
          ;; Find the first function whose start position is before cursor
          current-fn (first (filter #(<= (:position %) cursor-pos) sorted))]
      (:name current-fn))))

(defn update-which-func!
  "Update which-func display for a buffer."
  []
  (when @which-func-enabled?
    (let [db @rfdb/app-db
          buffer-id (lisp/current-buffer)
          cursor-pos (lisp/point)
          index (imenu/create-imenu-index db buffer-id)
          flat-index (when index (imenu/flatten-imenu-index index))
          current-fn (find-current-function flat-index cursor-pos)]
      (swap! rfdb/app-db assoc-in [:buffers buffer-id :which-func-format]
             (when current-fn (str "[" current-fn "]"))))))

;; =============================================================================
;; Commands
;; =============================================================================

(defn which-func-mode!
  "Toggle which-func-mode."
  []
  (swap! which-func-enabled? not)
  (if @which-func-enabled?
    (do
      ;; Register hook to update on cursor movement
      (lisp/add-hook 'post-command-hook update-which-func!)
      (lisp/message "Which-func-mode enabled"))
    (do
      (lisp/remove-hook 'post-command-hook update-which-func!)
      (lisp/message "Which-func-mode disabled"))))

;; =============================================================================
;; Integration with Cursor Movement
;; =============================================================================

(defn on-cursor-move
  "Called when cursor moves to update which-func display.
   Should be called from cursor/set-position event."
  [db buffer-id cursor-pos]
  (if @which-func-enabled?
    (let [index (imenu/create-imenu-index db buffer-id)
          flat-index (when index (imenu/flatten-imenu-index index))
          current-fn (find-current-function flat-index cursor-pos)]
      (assoc-in db [:buffers buffer-id :which-func-format]
                (when current-fn (str "[" current-fn "]"))))
    db))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-which-func!
  "Initialize which-func-mode commands."
  []
  (lisp/define-command 'which-func-mode
    which-func-mode!
    "Toggle display of current function in mode line"))
