# The Missing Parts: A Low-Level Design for Lexicon's Standard Library

**To:** Lexicon Development Team
**From:** Gemini AI Assistant
**Date:** 2025-12-24
**Subject:** A Detailed Implementation Plan for Foundational Emacs Subsystems

## 1. Executive Summary

Lexicon has a sound architectural core. This document provides the low-level design to build the next, essential layer: a "standard library" of foundational subsystems. Implementing these will enable a rich, idiomatic Emacs experience and support a vibrant package ecosystem, as exemplified by modern packages like Vertico, Corfu, and Embark.

This is a technical design document intended for developers. It leaves as little as possible to the imagination and provides a clear, actionable path from design to running code, including code structure, data models, API definitions, and a task tracker for each component.

---

## 2. Foundational Buffer and Mode Architecture

### 2.1. Introduction

**Problem:** A truly Emacs-like editor must gracefully handle a huge variety of "buffer types": editable files, read-only help buffers (`*Help*`), interactive directory listings (`dired`), REPLs (`*cider-repl*`), and more. A rigid, object-oriented approach with different "buffer classes" would be brittle and un-Emacs-like.

**Solution:** We will adopt Emacs's foundational philosophy: a buffer is a generic container for text. Its "personality" and behavior are defined entirely by its **major mode** and a set of **buffer-local state variables**. This design is infinitely flexible and is the key to Emacs's extensibility.

### 2.2. Core Data Structures (`db.cljs`)

There will only be **one** buffer data structure. Its flexibility comes from its generic state flags and a "grab-bag" for mode-specific local variables.

```clojure
;; in packages/editor-cljs/src/lexicon/db.cljs

;; This is the UNIFIED structure for ALL buffers.
(def default-buffer
  {
   :id 0                  ;; Unique integer ID
   :name ""                ;; Buffer name string, e.g., "core.cljs" or "*Help*"

   ;; Core Text Engine
   :wasm-instance nil      ;; The Rust gap buffer instance for this buffer
   :file-path nil          ;; Absolute path to the associated file, or nil

   ;; State Flags
   :is-modified? false     ;; Has the buffer changed since the last save?
   :is-read-only? false    ;; Is the buffer read-only?

   ;; Mode and Keymap Configuration
   :major-mode :fundamental-mode ;; Keyword identifying the major mode
   :minor-modes #{}            ;; A set of keywords for active minor modes

   ;; Buffer-Local Variables
   ;; A generic map for modes to store their own state. CRITICAL for extensibility.
   :locals {}

   ;; UI Systems (as defined in subsequent sections)
   :overlays {}
   :next-overlay-id 1

   ;; Undo System (as defined in subsequent sections)
   :undo-stack []
   :undo-recording-enabled? true

   ;; Cached data for performance
   :cache {:text ""         ;; A recent snapshot of the full text
           :line-count 1}
   })
```

**Key Architectural Decisions:**

1.  **`:is-read-only?` Flag:** All core editing events (`:editor/queue-transaction` for insert, delete, etc.) **must** first check this flag for the active buffer. If `true`, the event must fail and show a "Buffer is read-only" message. This is the primary mechanism for protecting special buffers.
2.  **`:locals` Map:** This is our `buffer-local variables`. It is the *only* place mode-specific state should be stored. `dired-mode` will store its `ls` switches here; a future `comint-mode` (for shells) will store the process ID and prompt boundaries here. This prevents the top-level buffer map from becoming polluted with mode-specific keys and is the key to our future extensibility.
3.  **Convention over Type:** A buffer's "type" is determined by convention. Its major mode gives it behavior, and if its name is surrounded by `*`, it's considered "special" and not meant to be saved to a file by default.

### 2.3. Core API and Implementation

#### A. The Major Mode as a "Personality Switch"

The `:set-major-mode` event must be a central coordinator that orchestrates the transition of a buffer's personality.

```clojure
;; in packages/editor-cljs/src/lexicon/events.cljs

(rf/reg-event-fx
 :set-major-mode
 (fn [{:keys [db]} [_ buffer-id new-mode-keyword]]
   (let [old-mode-keyword (get-in db [:buffers buffer-id :major-mode] :fundamental-mode)
         ;; Convention: a mode's activation hook is named `mode-name-hook`
         activation-hook (keyword (str (name new-mode-keyword) "-hook"))
         ;; Convention: a mode's deactivation hook is named `mode-name-kill-hook`
         kill-hook (keyword (str (name old-mode-keyword) "-kill-hook"))]
     (if (= old-mode-keyword new-mode-keyword)
       {:db db} ;; No change, do nothing
       {:db (assoc-in db [:buffers buffer-id :major-mode] new-mode-keyword)
        :fx [;; IMPORTANT: Run the kill hook for the OLD mode first to clean up.
             [:dispatch [:run-hook kill-hook {:buffer-id buffer-id}]]
             ;; Then, run the activation hook for the NEW mode to set things up.
             [:dispatch [:run-hook activation-hook {:buffer-id buffer-id}]]]}))))
```

#### B. The Initial Standard Library of Modes

We must ship with a few essential modes. These will live in a new directory: `packages/editor-cljs/src/lexicon/modes/`.

**1. `fundamental_mode.cljs`**
*   **Purpose:** The base mode from which all others inherit. The "void" of modes.
*   **`fundamental-mode-hook`:** Does nothing.
*   **`fundamental-mode-kill-hook`:** Does nothing.
*   **Keymap:** Empty. The buffer will only use the `global-map`.

**2. `special_mode.cljs`**
*   **Purpose:** The base mode for all non-file, interactive buffers like `*Help*`, `*Buffer List*`, etc.
*   **`special-mode-hook`:**
    *   Sets the buffer to read-only: `(assoc-in db [:buffers buffer-id :is-read-only?] true)`.
    *   Registers a local keymap (`special-mode-map`) via the keymap API.
        *   `q` -> `:kill-this-buffer`
        *   `g` -> `:revert-buffer`
*   **`special-mode-kill-hook`:**
    *   Sets the buffer read-only state back to `false`.

**3. `dired_mode.cljs` (A Complex Example)**
*   **Purpose:** To demonstrate how a complex, interactive UI is just another major mode.
*   **Activation (`dired-mode-hook`):**
    1.  Runs the `special-mode-hook` to become a read-only, special buffer.
    2.  Defines its own `dired-mode-map` with many bindings (`n`, `p`, `^`, `m` for marking, `x` for executing). This map's parent will be `special-mode-map`.
    3.  Sets a buffer-local variable in `:locals`: `{:revert-function :dired/regenerate-listing}`.
*   **Key Command (`:dired/regenerate-listing`):**
    *   This event reads the file path from the buffer state.
    *   It uses a shell effect to list the directory contents.
    *   It programmatically makes the buffer writable (`:is-read-only? false`).
    *   It deletes the entire buffer content.
    *   It inserts the new directory listing.
    *   It makes the buffer read-only again (`:is-read-only? true`).

#### C. The Generic `revert-buffer` Command

This command must be smart, using the buffer-local revert function if it exists.

```clojure
;; in packages/editor-cljs/src/lexicon/events.cljs

(rf/reg-event-fx
 :revert-buffer
 (fn [{:keys [db]} [_ buffer-id]]
   (let [buffer (get-in db [:buffers buffer-id])
         ;; 1. Check for a mode-specific revert function in :locals
         local-revert-event (get-in buffer [:locals :revert-function])
         file-path (:file-path buffer)]
     (cond
       local-revert-event
       {:fx [[:dispatch (conj local-revert-event {:buffer-id buffer-id})]]}

       file-path
       {:fx [[:dispatch [:editor/read-file-into-buffer {:file-path file-path
                                                        :buffer-id buffer-id}]]]}
       :else
       {:fx [[:dispatch [:echo/message "Buffer cannot be reverted"]]]})))
```

### 2.4. Test-Driven Development Plan

1.  **Test Buffer State:**
    *   Write a test for an `:editor/insert-char` event. It should fail if `:is-read-only?` is true.
    *   Create a mock `comint-mode` test where a `comint-prompt-boundary` is set in `:locals`. Test that insertion fails if the cursor is before the boundary.
2.  **Test Major Mode Switching:**
    *   Create two dummy modes, `:test-mode-A` and `:test-mode-B`, with corresponding hook handlers.
    *   Dispatch `:set-major-mode` from A to B.
    *   Assert that `test-mode-A-kill-hook` was called, followed by `test-mode-B-hook`.
3.  **Test `revert-buffer`:**
    *   Create a buffer with a `:file-path`. Dispatch `:revert-buffer` and assert that the file-reading event was dispatched.
    *   Create a buffer and set a `:revert-function` in its `:locals`. Dispatch `:revert-buffer` and assert that the local event was dispatched *instead* of the file-reading one.

### 2.5. Project Task Tracker

-   [ ] **Task 1: Update Buffer Data Structure**
    -   [ ] Add `:is-read-only?` and `:locals` to the `default-buffer` map in `db.cljs`.
-   [ ] **Task 2: Enforce Read-Only State**
    -   [ ] Modify the core transaction processor (`:editor/process-queue`) to check `:is-read-only?` before applying any text-modification operation.
-   [ ] **Task 3: Implement `:set-major-mode`**
    -   [ ] Create the `:set-major-mode` event handler as specified above.
    -   [ ] Ensure it correctly dispatches the old mode's kill hook and the new mode's activation hook.
-   [ ] **Task 4: Implement `fundamental-mode` and `special-mode`**
    -   [ ] Create `packages/editor-cljs/src/lexicon/modes/` directory.
    -   [ ] Create `fundamental_mode.cljs` and `special_mode.cljs` with their corresponding activation/kill hook handlers.
-   [ ] **Task 5: Implement Generic `:revert-buffer`**
    -   [ ] Create the `:revert-buffer` event handler as specified, ensuring it respects the `:locals` map first.
-   [ ] **Task 6: Write Tests**
    -   [ ] Implement all tests outlined in the TDD plan to verify the new architecture.

---

## 3. The Pluggable Completion Subsystem

### 3.1. Introduction

**Problem:** Lexicon's current completion is a simple, hardcoded minibuffer prompt. Emacs's power comes from its *pluggable* completion system, where the UI and logic can be completely replaced by packages. **Vertico** replaces the minibuffer UI, **Corfu** provides in-buffer completion, and **Orderless** changes the filtering logic.

**Solution:** We must refactor our completion to be driven by a central, overridable function, `completing-read`, and provide the necessary surrounding metadata and hooks.

### 3.2. Proposed Code Reorganization

1.  **New Namespace:** Create a central place for all completion-related logic.
    *   `packages/editor-cljs/src/lexicon/completion.cljs`
2.  **Minibuffer Logic:** The logic for activating and managing the minibuffer UI should be separated from the completion logic itself.
    *   Much of the logic in `:minibuffer/activate` in `events.cljs` will move into `completion.cljs`.

### 3.3. Core Data Structures (`db.cljs`)

We need to add configuration variables to our `default-db` that mirror Emacs's completion system.

```clojure
;; in packages/editor-cljs/src/lexicon/db.cljs
(def default-db
  {
   ;; ... existing state
   :completion {:completing-read-function :lexicon.completion/default-completing-read ;; The KEY variable to swap out
                :completion-in-region-function :lexicon.completion/default-completion-in-region
                :completion-styles [:basic :substring] ;; Default styles
                :completion-category-defaults [] ;; For category overrides
                :completion-category-overrides {}}
   :minibuffer {;; ... existing minibuffer state
                :completion-meta {} ;; To hold metadata for the active completion
                :active? false
                :prompt ""
                ;; ...
                }})
```

### 3.4. Core API and Implementation

#### A. `completing-read` - The Heart of the System

This will be the main event that all other commands call to get input from the user.

```clojure
;; in packages/editor-cljs/src/lexicon/events.cljs

;; This is the new entry point for all interactive input.
(rf/reg-event-fx
 :completing-read
 (fn [{:keys [db]} [_ & {:keys [prompt collection predicate require-match initial-input history def on-confirm on-cancel]}]]
   (let [handler-key (get-in db [:completion :completing-read-function])]
     {:fx [[:dispatch (conj [handler-key]
                            {:prompt prompt
                             :collection collection
                             :predicate predicate
                             :require-match require-match
                             :initial-input initial-input
                             :history history
                             :def def
                             :on-confirm on-confirm
                             :on-cancel on-cancel})]]}))))
```

#### B. The Default Minibuffer Implementation

This is the standard, built-in completion UI that Vertico will eventually replace.

```clojure
;; in packages/editor-cljs/src/lexicon/completion.cljs

(ns lexicon.completion
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; Helper function to filter candidates
(defn- filter-candidates [input collection styles]
  ;; In a real implementation, this would be a multi-method
  ;; dispatching on the active `completion-styles`.
  (let [pattern (re-pattern (str "(?i)" input))]
    (filter #(re-find pattern %) collection)))

(rf/reg-event-fx
 :lexicon.completion/default-completing-read
 (fn [{:keys [db]} [_ {:keys [prompt collection predicate require-match initial-input history def on-confirm on-cancel]}]]
   (let [candidates (filter-candidates initial-input collection (get-in db [:completion :completion-styles]))]
     {:db (assoc-in db [:minibuffer] {:active? true
                                      :prompt prompt
                                      :input (or initial-input "")
                                      :candidates candidates
                                      :selected-index -1 ;; Nothing selected initially
                                      :completion-meta {:collection collection ;; Store for later filtering
                                                        :on-confirm on-confirm
                                                        :on-cancel on-cancel}})
      :fx [[:dom/focus-minibuffer]]})))

;; Events for handling user input IN the minibuffer
(rf/reg-event-fx
 :minibuffer/update-input
 (fn [{:keys [db]} [_ new-input]]
   (let [collection (get-in db [:minibuffer :completion-meta :collection])
         styles (get-in db [:completion :completion-styles])
         new-candidates (filter-candidates new-input collection styles)]
     {:db (-> db
              (assoc-in [:minibuffer :input] new-input)
              (assoc-in [:minibuffer :candidates] new-candidates))})))

(rf/reg-event-fx
 :minibuffer/confirm
 (fn [{:keys [db]} [_]]
   (let [minibuffer-state (:minibuffer db)
         on-confirm (:on-confirm (:completion-meta minibuffer-state))
         selected-candidate (if (>= (:selected-index minibuffer-state) 0)
                              (get (:candidates minibuffer-state) (:selected-index minibuffer-state))
                              (:input minibuffer-state))]
     {:db (assoc db :minibuffer db/default-minibuffer-state) ;; Reset minibuffer
      :fx [[:dispatch (conj on-confirm selected-candidate)]
           [:dom/focus-main-editor]]})))

;; Other handlers needed: :minibuffer/cancel, :minibuffer/next-candidate, :minibuffer/previous-candidate
```

#### C. Refactoring Existing Commands

Now, commands like `switch-to-buffer` must be changed to use the new system.

**Old way (`events.cljs`):**
```clojure
(rf/reg-event-fx
 :switch-to-buffer
 (fn [{:keys [db]} [_]]
   {:fx [[:minibuffer/activate
          {:prompt "Switch to buffer: "
           :on-confirm [:switch-to-buffer-by-name]
           :completions (keys (:buffers db)) }]]}))
```

**New, idiomatic way:**
```clojure
(rf/reg-event-fx
 :switch-to-buffer
 (fn [{:keys [db]} [_]]
   {:fx [[:dispatch [:completing-read
                     {:prompt "Switch to buffer: "
                      :collection (keys (:buffers db))
                      :on-confirm [:switch-to-buffer-by-name]}]]]}))
```

### 3.5. Test-Driven Development Plan

1.  **Test `:completing-read` dispatch:** Write a test to ensure that dispatching `:completing-read` correctly calls the function stored in `:completing-read-function`.
2.  **Test default implementation:**
    *   Given a collection, does `:default-completing-read` correctly populate the minibuffer with filtered candidates?
    *   Does `:minibuffer/update-input` refilter the candidates?
    *   Does `:minibuffer/confirm` call the `:on-confirm` handler with the correct value?
    *   Does `:minibuffer/cancel` work as expected?
3.  **Test pluggability:**
    *   Create a dummy `test-completing-read` event handler.
    *   Write a test that temporarily sets `:completing-read-function` to `:test-completing-read`.
    *   Dispatch `:completing-read` and assert that the dummy handler was called, NOT the default one. This proves the core mechanism works.

### 3.6. Project Task Tracker

-   [ ] **Task 1: Refactor `db.cljs`**
    -   [ ] Add the `:completion` map to `default-db`.
    -   [ ] Add `:completion-meta` to the `:minibuffer` state in `default-db`.
-   [ ] **Task 2: Create `lexicon.completion.cljs`**
    -   [ ] Create the new file.
    -   [ ] Implement the `filter-candidates` helper function.
    -   [ ] Implement the `:lexicon.completion/default-completing-read` event handler.
-   [ ] **Task 3: Implement Minibuffer Interaction Events**
    -   [ ] In `completion.cljs`, create `:minibuffer/update-input`.
    -   [ ] In `completion.cljs`, create `:minibuffer/confirm`.
    -   [ ] In `completion.cljs`, create `:minibuffer/cancel`.
    -   [ ] In `completion.cljs`, create `:minibuffer/next-candidate` and `:minibuffer/previous-candidate`.
-   [ ] **Task 4: Create the `:completing-read` Entry Point**
    -   [ ] In `events.cljs`, create the main `:completing-read` event that dispatches to the configurable function.
-   [ ] **Task 5: Refactor Existing Commands**
    -   [ ] Change `:switch-to-buffer` to use `:completing-read`.
    -   [ ] Change `:execute-extended-command` (`M-x`) to use `:completing-read`.
    -   [ ] Audit all other commands that use the minibuffer and refactor them.
-   [ ] **Task 6: Write Tests**
    -   [ ] Implement all tests outlined in the TDD plan.

---

## 4. The Rich UI Subsystem (Overlays, Faces, Child Frames)

### 4.1. Introduction

**Problem:** Lexicon can only display plain text. A modern editor needs to highlight search results, show linting errors, display popups, and apply syntax highlighting. This requires Overlays (for temporary highlights), Faces (for styling), and Child Frames (for popups).

**Solution:** Implement three interconnected systems that map directly to their Emacs counterparts, but use web technologies (CSS and DOM manipulation) for the rendering.

### 4.2. Part A: Faces

#### Code Reorganization
*   `packages/editor-cljs/src/lexicon/ui/faces.cljs`

#### Data Structures (`db.cljs`)
```clojure
;; in packages/editor-cljs/src/lexicon/db.cljs
(def default-db
  {
   ;; ...
   :faces {;; name -> css-class map
           :default "face-default"
           :highlight "face-highlight"
           :error "face-error"
           :vertico-current "face-vertico-current"}
   ;; ...
  })
```

#### Implementation
The core idea is to generate a stylesheet from the `:faces` map and inject it into the DOM. The views will then use the face names to look up the correct CSS class.

```clojure
;; in packages/editor-cljs/src/lexicon/ui/faces.cljs

(defn- generate-stylesheet
  "Generates a CSS string from the faces map"
  [faces-map]
  (str/join "\n"
    [".face-default { background-color: #fff; color: #000; }"
     ".face-highlight { background-color: yellow; }"
     ".face-error { text-decoration: underline wavy red; }"
     ".face-vertico-current { background-color: #c0efff; }"]))

(rf/reg-event-fx
 :faces/initialize
 (fn [{:keys [db]} [_]]
   (let [stylesheet (generate-stylesheet (:faces db))]
     {:fx [[:dom/inject-stylesheet {:id "lexicon-faces" :content stylesheet}]]})))

(rf/reg-sub
 :faces/class-for
 (fn [db [_ face-name]]
   (get-in db [:faces face-name] "face-default")))
```

#### Task Tracker (Faces)
-   [ ] **Task 1:** Add `:faces` map to `db.cljs`.
-   [ ] **Task 2:** Implement `faces.cljs` with `generate-stylesheet` and `:faces/initialize`.
-   [ ] **Task 3:** Implement `:faces/class-for` subscription.
-   [ ] **Task 4:** Create a `:dom/inject-stylesheet` effect handler that creates/updates a `<style>` tag in the document head.
-   [ ] **Task 5:** Call `:faces/initialize` once on application startup.

### 4.3. Part B: Overlays

#### Code Reorganization
*   `packages/editor-cljs/src/lexicon/ui/overlays.cljs`

#### Data Structures (`db.cljs`)
Overlays are buffer-specific. We will store them with the buffer.

```clojure
;; in packages/editor-cljs/src/lexicon/db.cljs
;; Inside the buffer map structure in the new unified design
{:id 1
 ;; ...
 :overlays {1 {:id 1 :start 10 :end 20 :face :highlight}
            2 {:id 2 :start 25 :end 30 :face :error}}
 :next-overlay-id 3
 ;; ...
}
```

#### Implementation
Overlays are not stored in the WASM gap buffer. They live entirely in the ClojureScript world and are applied at render time.

```clojure
;; in packages/editor-cljs/src/lexicon/ui/overlays.cljs

(rf/reg-event-db
 :overlays/make-overlay
 (fn [db [_ buffer-id start end face]]
   (let [overlay-id (get-in db [:buffers buffer-id :next-overlay-id] 1)
         new-overlay {:id overlay-id :start start :end end :face face}]
     (-> db
         (assoc-in [:buffers buffer-id :overlays overlay-id] new-overlay)
         (update-in [:buffers buffer-id :next-overlay-id] (fnil inc 1))))))

(rf/reg-event-db
 :overlays/delete-overlay
 (fn [db [_ buffer-id overlay-id]]
   (update-in db [:buffers buffer-id :overlays] dissoc overlay-id)))

;; The view logic will need to be updated to handle this.
;; The text rendering component will:
;; 1. Get the raw text from the WASM buffer.
;; 2. Get the list of overlays for the buffer.
;; 3. For each line, iterate through the text and check if any overlays apply.
;; 4. Wrap the text segments in `<span>` tags with the appropriate face class.
;; This is complex and a performance consideration. A virtualized renderer is a must.
```

#### Task Tracker (Overlays)
-   [ ] **Task 1:** Update the `default-buffer` structure in `db.cljs` to include `:overlays` and `:next-overlay-id`.
-   [ ] **Task 2:** Implement the `:overlays/make-overlay` event handler.
-   [ ] **Task 3:** Implement the `:overlays/delete-overlay` event handler.
-   [ ] **Task 4:** **(MAJOR TASK)** Modify the main editor view component (`views.cljs`).
    -   [ ] On render, subscribe to the current buffer's overlays.
    -   [ ] Implement a rendering algorithm that takes the plain text and a list of overlays, and outputs styled HTML. This will involve slicing the text at overlay boundaries and wrapping each segment in a `<span>` with the correct CSS class from the face system.

### 4.4. Part C: Child Frames (Popups)

#### Code Reorganization
*   `packages/editor-cljs/src/lexicon/ui/frames.cljs`

#### Data Structures (`db.cljs`)
```clojure
;; in packages/editor-cljs/src/lexicon/db.cljs
(def default-db
  {
   ;; ...
   :child-frames {;; id -> frame-data map
                  :corfu-popup {:id :corfu-popup
                                :visible? false
                                :x 0 :y 0
                                :width 300 :height 200
                                :content ["Candidate 1" "Candidate 2"]
                                :face :default}}
   ;; ...
  })
```

#### Implementation
This will be a UI-heavy task. We'll render child frames as absolutely positioned divs at the top level of our application.

```clojure
;; in packages/editor-cljs/src/lexicon/ui/frames.cljs

(rf/reg-event-db
 :frames/show
 (fn [db [_ frame-id frame-data]]
   (update-in db [:child-frames frame-id] merge frame-data {:visible? true})))

(rf/reg-event-db
 :frames/hide
 (fn [db [_ frame-id]]
   (assoc-in db [:child-frames frame-id :visible?] false)))

;; in packages/editor-cljs/src/lexicon/views.cljs

;; Top-level app component
(defn main-app []
  (let [child-frames @(rf/subscribe [:child-frames/all-visible])]
    [:div
     ;; ... the main editor UI (splits, windows, etc)
     [:div {:class "child-frames-container"}
      (for [frame child-frames]
        ^{:key (:id frame)}
        [child-frame-view frame])]]))

;; Child frame view component
(defn child-frame-view [frame]
  (let [face-class @(rf/subscribe [:faces/class-for (:face frame)])]
    [:div {:class (str "child-frame " face-class)
           :style {:position "absolute"
                   :left (str (:x frame) "px")
                   :top (str (:y frame) "px")
                   :width (str (:width frame) "px")
                   :height (str (:height frame) "px")
                   :visibility (if (:visible? frame) "visible" "hidden")}} 
     ;; Render the content of the frame
     (for [line (:content frame)]
       [:div {:key line} line])]))
```

#### Task Tracker (Child Frames)
-   [ ] **Task 1:** Add `:child-frames` map to `db.cljs`.
-   [ ] **Task 2:** Implement `:frames/show` and `:frames/hide` event handlers.
-   [ ] **Task 3:** Create a subscription `:child-frames/all-visible` that returns a sequence of all visible frames.
-   [ ] **Task 4:** Create the `child-frame-view` Reagent component.
-   [ ] **Task 5:** Update the `main-app` view to render the child frames container.

---

## 5. Contextual "Thing-at-Point" Subsystem

### 5.1. Introduction

**Problem:** Commands like `find-file-at-point` or context menus like **Embark** need to know what "thing" the cursor is on. Is it a file path? A URL? A symbol? Lexicon currently has no such semantic understanding.

**Solution:** Implement a `thing-at-point` utility and a system for registering "target finders" that commands can use to get contextual information.

### 5.2. Code Reorganization

*   `packages/editor-cljs/src/lexicon/thing_at_point.cljs`

### 5.3. Core API and Implementation

This system will be mostly pure functions that operate on the buffer's text.

```clojure
;; in packages/editor-cljs/src/lexicon/thing_at_point.cljs

(defmulti thing-at-point
  "Returns the 'thing' of a given type at a position in text."
  (fn [text position type] type))

;; Default implementation is a no-op
(defmethod thing-at-point :default [_ _ _] nil)

(defmethod thing-at-point :symbol [text position _]
  ;; Implementation would use regex to find symbol boundaries
  ;; around the position.
  ;; Returns {:text "my-symbol" :start 10 :end 19 :type :symbol} or nil
  )

(defmethod thing-at-point :url [text position _]
  ;; Implementation would use regex to find URL boundaries.
  )

;; The 'target finder' system for Embark
(rf/reg-event-fx
 :context/find-target
 (fn [{:keys [db]} [_ on-confirm-event]]
   (let [;; This is a simplified version. A real one would use a registry of finders.
         buffer-text (get-in db [:buffers (active-buffer-id db) :cache :text])
         cursor-pos (get-in db [:ui :cursor-position])
         target (or (thing-at-point buffer-text cursor-pos :url)
                    (thing-at-point buffer-text cursor-pos :symbol))]
     {:fx [[:dispatch (conj on-confirm-event target)]]})))
```

### 5.4. Test-Driven Development Plan

1.  **Unit test `thing-at-point` methods:**
    *   For `:symbol`, test it on `(foo-|bar baz)`, `|foo-bar`, `foo-bar|`.
    *   For `:url`, test it on various URL formats.
2.  **Test `:context/find-target`:** Create a test that sets up buffer text and a cursor position, then dispatches the event and checks that the correct target is passed to the `on-confirm-event`.

### 5.5. Project Task Tracker

-   [ ] **Task 1:** Create `thing_at_point.cljs`.
-   [ ] **Task 2:** Implement the `thing-at-point` multimethod.
-   [ ] **Task 3:** Implement the `:symbol` method for `thing-at-point`.
-   [ ] **Task 4:** Implement the `:url` method for `thing-at-point`.
-   [ ] **Task 5:** Implement the `:context/find-target` event handler.
-   [ ] **Task 6:** Write unit tests for all `thing-at-point` methods.

---

## 6. Dynamic Theming and Customization

### 6.1. Introduction

**Problem:** The current UI design is static. A core part of the Emacs experience is the ability for a user to dynamically change colors, fonts, and spacing, and to load different "themes". Our static, class-based face system is insufficient for this.

**Solution:** We will refactor our styling to be based on **CSS Variables (Custom Properties)**. This is the modern web standard for dynamic theming and allows for efficient, runtime changes to the entire UI's appearance.

### 6.2. Core Data Structures (`db.cljs`)

We will introduce a more sophisticated `:theme` map to our `app-db`.

```clojure
;; in packages/editor-cljs/src/lexicon/db.cljs
(def default-db
  {
   ;; ...
   :theme {
           ;; 1. A map of all defined faces and their attributes.
           ;; This is the "defface" equivalent. It maps attributes to CSS vars.
           :faces {:default {:foreground "--lexicon-fg-default"
                             :background "--lexicon-bg-default"}
                   :highlight {:background "--lexicon-bg-highlight"}
                   :error {:underline {:style "wavy" :color "--lexicon-fg-error"}}
                   :minibuffer-prompt {:foreground "--lexicon-fg-prompt" :bold true}}

           ;; 2. A map of available themes. A theme is a map of CSS variable -> value.
           ;; This is the "custom-set-faces" equivalent.
           :themes {:base {;; This theme defines all the core variables
                           "--lexicon-font-family-mono" "monospace"
                           "--lexicon-font-size" "16px"
                           "--lexicon-fg-default" "#000000"
                           "--lexicon-bg-default" "#ffffff"
                           "--lexicon-bg-highlight" "yellow"
                           "--lexicon-fg-error" "red"
                           "--lexicon-fg-prompt" "blue"}
                    ;; A user theme, loaded on top.
                    :user {}}
           
           ;; 3. The stack of active theme names. Order matters for overrides.
           :active-themes [:base :user]
           }
   ;; ...
  })
```

### 6.3. Core API and Implementation

A new namespace will manage converting this DB state into live styles.

**File:** `packages/editor-cljs/src/lexicon/ui/theming.cljs`

```clojure
(ns lexicon.ui.theming
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; --- Face to CSS Generation ---
;; This generates the static structure, e.g., .face-default { color: var(...) }
(defn- face-spec-to-css [face-name spec]
  (let [rules (transduce
               (map (fn [[attr value]]
                      (case attr
                        :foreground (str "  color: var(" value ");")
                        :background (str "  background-color: var(" value ");")
                        :bold (str "  font-weight: bold;")
                        ;; etc. for :italic, :underline...
                        "")))
               str/join "\n"
               spec)]
    (str ".\nface-" (name face-name) " {\n" rules "\n}")))

(defn- generate-face-stylesheet [faces-map]
  (->> faces-map
       (map (fn [[name spec]] (face-spec-to-css name spec)))
       (str/join "\n")))

;; --- Theme (CSS Variable) Management ---
;; This generates the dynamic values, e.g., :root { --lexicon-fg-default: #000; }
(defn- active-themes->css-vars [themes active-themes]
  (->> active-themes
       (map #(get themes %))
       (apply merge))) ; Last theme in the list wins, which is correct.

(defn- css-vars->stylesheet [vars-map]
  (let [rules (->> vars-map
                   (map (fn [[var value]] (str "  " (name var) ": " value ";")))
                   (str/join "\n"))]
    (str ":root {\n" rules "\n}")))


;; --- Event Handlers ---

(rf/reg-event-fx
 :theme/initialize
 (fn [{:keys [db]} _]
   (let [theme-state (:theme db)
         face-css (generate-face-stylesheet (:faces theme-state))
         active-vars (active-themes->css-vars (:themes theme-state) (:active-themes theme-state))
         theme-css (css-vars->stylesheet active-vars)]
     {:db db
      :fx [[:dom/inject-stylesheet {:id "lexicon-faces" :content face-css}]
           [:dom/inject-stylesheet {:id "lexicon-theme-vars" :content theme-css}]]})))

(rf/reg-event-fx
 ;; For M-x set-font-size, etc.
 :theme/set-user-customization
 (fn [{:keys [db]} [_ var-name value]]
   (let [new-db (assoc-in db [:theme :themes :user var-name] value)
         theme-state (:theme new-db)
         active-vars (active-themes->css-vars (:themes theme-state) (:active-themes theme-state))
         theme-css (css-vars->stylesheet active-vars)]
     {:db new-db
      :fx [;; Just update the variables stylesheet
           [:dom/inject-stylesheet {:id "lexicon-theme-vars" :content theme-css}]]})))
```

### 6.4. Test-Driven Development Plan

1.  **Test Stylesheet Generation:**
    *   Unit test `face-spec-to-css` to ensure it correctly converts a face map to a CSS rule string.
    *   Unit test `css-vars->stylesheet` to ensure it correctly converts a theme map to a `:root` CSS rule.
2.  **Test Theme Merging:** Unit test `active-themes->css-vars` to ensure that themes later in the `:active-themes` list correctly override variables from themes earlier in the list.
3.  **Test Event Logic:**
    *   Write a test for `:theme/initialize` and verify that it dispatches two `:dom/inject-stylesheet` effects with the correctly generated CSS content.
    *   Write a test for `:theme/set-user-customization`. It should add the custom value to the `:user` theme in the `db`, and then dispatch a `:dom/inject-stylesheet` effect with the updated CSS variables.

### 6.5. Project Task Tracker

-   [ ] **Task 1: Refactor `db.cljs` for Theming**
    -   [ ] Replace the simple `:faces` map with the new, more detailed `:theme` map containing `:faces`, `:themes`, and `:active-themes`.
    -   [ ] Define a base set of CSS variables (for fonts, colors, etc.) and map faces to them.
-   [ ] **Task 2: Create Theming Engine (`theming.cljs`)**
    -   [ ] Create the new file `packages/editor-cljs/src/lexicon/ui/theming.cljs`.
    -   [ ] Implement all helper functions and event handlers as specified above.
-   [ ] **Task 3: Integrate Theming Engine**
    -   [ ] Ensure `:theme/initialize` is called once on application startup.
    -   [ ] Remove the old `:faces/initialize` event and `:faces/class-for` subscription. The view layer will now use static CSS classes (`.face-highlight`) and the styling will be applied automatically by the browser via CSS variables.
-   [ ] **Task 4: Create User-Facing Customization Commands**
    -   [ ] Implement simple commands like `:theme/increase-font-size` that dispatch to `:theme/set-user-customization`.
    -   [ ] **(EPIC)** Begin design for a `:theme/customize-face` command and its associated `customize-mode`.
-   [ ] **Task 5: Write Tests**
    -   [ ] Implement all tests outlined in the TDD plan.

---

## 7. Advanced Undo Subsystem

### 7.1. Introduction

**Problem:** The current undo system is likely a simple stack of inverse operations. Emacs allows commands to group multiple edits into a single undo step. For example, a code formatting command makes many small edits, but from the user's perspective, it's one action. **Corfu** uses this when inserting a completion.

**Solution:** Implement an explicit "undo grouping" mechanism in our transaction processor.

### 7.2. Data Structures (`db.cljs`)

We need to add a boundary marker to the undo stack.

```clojure
;; in packages/editor-cljs/src/lexicon/db.cljs
;; in the buffer map
:undo-stack [;; inverse-op-3
              ;; inverse-op-2
              :undo-boundary ;; A marker
              ;; inverse-op-1
              ]
```

### 7.3. Core API and Implementation

We'll add two new commands for this.

```clojure
;; in packages/editor-cljs/src/lexicon/events.cljs

(rf/reg-event-db
 :undo/begin-change-group
 (fn [db [_ buffer-id]]
   (update-in db [:buffers buffer-id :undo-stack] conj :undo-boundary)))

(rf/reg-event-db
 :undo/end-change-group
 (fn [db [_ buffer-id]]
   ;; This could be a no-op, or it could do something more complex
   ;; like amalgamating the operations between boundaries. For now,
   ;; the boundary marker is enough for the :undo event to use.
   db))

(rf/reg-event-fx
 :undo
 (fn [{:keys [db]} [_]]
   (let [buffer-id (active-buffer-id db)
         undo-stack (get-in db [:buffers buffer-id :undo-stack])
         ;; Find the first boundary
         [ops-to-undo rest-stack] (split-with #(not= % :undo-boundary) undo-stack)
         ;; Drop the boundary marker itself
         final-stack (rest rest-stack)]
     {:db (assoc-in db [:buffers buffer-id :undo-stack] final-stack)
      ;; We must disable undo recording while we are performing an undo!
      :fx [[:dispatch [:undo/set-recording-status false]]
           ;; Dispatch all the inverse operations
           (for [op ops-to-undo]
             [:dispatch [:editor/queue-transaction op]])
           [:dispatch-later {:ms 100 :dispatch [:undo/set-recording-status true]}]]})))

;; The `record-undo!` function must be updated to check a flag before recording.
(defn record-undo! [db operation]
  (when (get-in db [:buffers (active-buffer-id db) :undo-recording-enabled?]))
    ;; ... rest of existing logic
    ))
```

### 7.4. Project Task Tracker

-   [ ] **Task 1:** Update the `:undo` event to respect `:undo-boundary` markers. It should undo all operations up to the last boundary.
-   [ ] **Task 2:** Implement `:undo/begin-change-group` and `:undo/end-change-group` events.
-   [ ] **Task 3:** Add an `:undo-recording-enabled?` flag to the buffer state.
-   [ ] **Task 4:** Modify `record-undo!` to respect the `:undo-recording-enabled?` flag.
-   [ ] **Task 5:** Modify the `:undo` event to disable undo recording while it is running.
-   [ ] **Task 6:** Refactor complex commands (like a future "format-buffer") to wrap their edits in `begin/end-change-group` calls.

This document provides a clear, developer-focused roadmap. By completing these tasks, Lexicon will gain the foundational "standard library" necessary to become a truly powerful and extensible Emacs platform for the web.
