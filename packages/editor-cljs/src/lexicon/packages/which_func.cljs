(ns lexicon.packages.which-func
  "Which-func-mode: Display the current function in the mode line.

  This minor mode displays the name of the function/method/class the cursor
  is currently in. It uses imenu to determine the current function.

  Based on Emacs lisp/which-func.el

  This package uses only lisp.cljs primitives."
  (:require [lexicon.lisp :as lisp]))

;; =============================================================================
;; State (package-local)
;; =============================================================================

(defonce which-func-enabled? (atom false))
(defonce which-func-cache (atom {}))  ;; {buffer-id {:format "string", :cursor-pos N}}

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
  "Update which-func display for current buffer."
  []
  (when @which-func-enabled?
    (let [buffer-id (lisp/current-buffer)
          cursor-pos (lisp/point)
          ;; Check cache to avoid unnecessary imenu computation
          cached (get @which-func-cache buffer-id)
          cached-pos (:cursor-pos cached)]
      ;; Only recompute if cursor moved significantly
      (when (or (nil? cached)
                (nil? cached-pos)
                (> (abs (- cursor-pos cached-pos)) 50))
        (let [index (lisp/imenu-create-index-for buffer-id)
              flat-index (when index (lisp/imenu-flatten-index index))
              current-fn (find-current-function flat-index cursor-pos)
              format-str (when current-fn (str "[" current-fn "]"))]
          (swap! which-func-cache assoc buffer-id
                 {:format format-str
                  :cursor-pos cursor-pos})
          ;; Store which-func-format as buffer-local variable
          (lisp/setq-local 'which-func-format format-str))))))

(defn get-which-func-format
  "Get the which-func format string for current buffer."
  []
  (let [buffer-id (lisp/current-buffer)]
    (or (get-in @which-func-cache [buffer-id :format])
        (lisp/buffer-local-value 'which-func-format buffer-id))))

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
      (reset! which-func-cache {})
      (lisp/message "Which-func-mode disabled"))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-which-func!
  "Initialize which-func-mode commands."
  []
  (lisp/define-command 'which-func-mode
    which-func-mode!
    "Toggle display of current function in mode line"))
