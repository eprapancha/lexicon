(ns lexicon.test-helpers
  "Convenience namespace that re-exports lexicon.semantic.helpers.

  This allows tests to use (require [lexicon.test-helpers :as helpers])
  regardless of whether they're in the semantic/ subdirectory or not."
  (:require [lexicon.semantic.helpers :as h])
  (:require-macros [lexicon.core.macros :refer [save-excursion save-current-buffer with-current-buffer]]
                   [lexicon.core.dynamic :refer [with-inhibit-read-only without-undo]]
                   [lexicon.semantic.helpers :refer [with-test-buffer]]))

;; Re-export all public functions from semantic.helpers
(def reset-editor-db! h/reset-editor-db!)
(def with-wasm h/with-wasm)
(def create-buffer h/create-buffer)
(def buffer-text h/buffer-text)
(def insert-text h/insert-text)
(def buffer-exists? h/buffer-exists?)
(def visit-file h/visit-file)
(def buffer-file h/buffer-file)
(def set-read-only h/set-read-only)
(def point h/point)
(def set-point h/set-point)
(def current-buffer h/current-buffer)
(def kill-region h/kill-region)
(def yank h/yank)
(def undo h/undo)
(def undo-boundary h/undo-boundary)
(def enable-major-mode h/enable-major-mode)
(def current-major-mode h/current-major-mode)
(def enable-minor-mode h/enable-minor-mode)
(def minor-mode-enabled? h/minor-mode-enabled?)
(def split-window-horizontally h/split-window-horizontally)
(def delete-other-windows h/delete-other-windows)
(def show-buffer-in-two-windows h/show-buffer-in-two-windows)
(def show-buffer-in-new-window h/show-buffer-in-new-window)
(def window-text h/window-text)
(def activate-minibuffer h/activate-minibuffer)
(def deactivate-minibuffer h/deactivate-minibuffer)
(def minibuffer-insert h/minibuffer-insert)
(def minibuffer-contents h/minibuffer-contents)
(def set-global-key h/set-global-key)
(def set-buffer-local-key h/set-buffer-local-key)
(def lookup-key h/lookup-key)
(def press-key-sequence h/press-key-sequence)
(def in-prefix-state? h/in-prefix-state?)
(def last-invoked-command h/last-invoked-command)
(def eval-lisp h/eval-lisp)
(def save-buffer h/save-buffer)
(def revert-buffer h/revert-buffer)
(def message h/message)
(def echo-area-text h/echo-area-text)
(def define-test-command h/define-test-command)
(def command-name h/command-name)
(def command-doc h/command-doc)
(def command-fn h/command-fn)
(def invoke-command h/invoke-command)
(def with-instrumented-dispatch h/with-instrumented-dispatch)
(def dispatch-was-used? h/dispatch-was-used?)

;; Text Properties
(def put-text-property h/put-text-property)
(def get-text-property h/get-text-property)
(def text-properties-at h/text-properties-at)
(def propertize h/propertize)
(def get-text-property-from-string h/get-text-property-from-string)
(def visible-text h/visible-text)

;; Convenience wrappers
(def insert h/insert)
(def goto-char h/goto-char)
(def delete-region h/delete-region)
(def erase-buffer h/erase-buffer)
(def buffer-name h/buffer-name)
(def buffer-read-only? h/buffer-read-only?)
(def set-buffer-read-only h/set-buffer-read-only)

;; Mark and Region
(def set-mark h/set-mark)
(def mark h/mark)
(def activate-mark h/activate-mark)
(def deactivate-mark h/deactivate-mark)
(def region-active? h/region-active?)

;; Search and Navigation
(def forward-word h/forward-word)
(def backward-word h/backward-word)
(def word-at-point h/word-at-point)
(def looking-at h/looking-at)
(def search-forward h/search-forward)

;; Narrowing
(def narrow-to-region h/narrow-to-region)

;; Hooks
(def add-hook h/add-hook)
(def remove-hook h/remove-hook)
(def run-hooks h/run-hooks)
(def run-hook-with-args-until-success h/run-hook-with-args-until-success)
(def run-hook-with-args-until-failure h/run-hook-with-args-until-failure)

;; Keymaps
(def make-keymap h/make-keymap)
(def define-key h/define-key)

;; Buffer ID
(def current-buffer-id h/current-buffer-id)
(def kill-buffer h/kill-buffer)
(def toggle-read-only h/toggle-read-only)
(def mode-line-string h/mode-line-string)
(def forward-char h/forward-char)
(def backward-char h/backward-char)
(def dired h/dired)

;; Windows
(def current-window h/current-window)
(def select-window h/select-window)
(def split-window h/split-window)

;; Markers
(def make-marker h/make-marker)
(def marker-position h/marker-position)
(def marker-buffer h/marker-buffer)
(def set-marker h/set-marker)
(def move-marker h/move-marker)
(def copy-marker h/copy-marker)
(def marker-insertion-type h/marker-insertion-type)
(def set-marker-insertion-type h/set-marker-insertion-type)
(def markerp h/markerp)
(def insert-before-markers h/insert-before-markers)
(def buffer-string h/buffer-string)

;; Macros - with-test-buffer is imported via :require-macros from lexicon.semantic.helpers
