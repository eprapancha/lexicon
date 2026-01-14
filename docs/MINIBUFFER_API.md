# Minibuffer Customization API

**Phase 6.5 Week 3-4 + Issue #46**

This document describes the minibuffer API for command authors who want to customize minibuffer behavior and rendering.

## Overview

Lexicon's minibuffer system is stack-based (like Emacs) and supports:
- Recursive minibuffer activation
- Custom rendering components
- Configurable display options
- Command-specific keybindings
- Completion systems

## Basic Usage

### Activating the Minibuffer

Commands activate the minibuffer by pushing a frame onto the minibuffer stack:

```clojure
(ns my-package.commands
  (:require [re-frame.core :as rf]
            [lexicon.minibuffer :as mb]))

(rf/reg-event-fx
  :my-command
  (fn [{:keys [db]} [_]]
    {:db (mb/push-frame db {:prompt "Enter text: "
                            :on-confirm [:my-command/confirm]})}))
```

### Frame Configuration

A minibuffer frame accepts the following configuration keys:

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `:prompt` | string | `""` | Prompt text shown to user |
| `:input` | string | `""` | Initial input text |
| `:on-confirm` | vector | `nil` | Event to dispatch on RET (required) |
| `:on-cancel` | vector | `[:minibuffer/deactivate]` | Event to dispatch on C-g |
| `:completions` | vector | `[]` | List of completion candidates |
| `:completion-index` | number | `0` | Currently selected completion index |
| `:metadata` | map | `nil` | Completion metadata (e.g., category, affixation) |
| `:persist?` | boolean | `false` | Don't auto-deactivate on confirm |
| `:show-completions?` | boolean | `false` | Show completion list |
| `:filtered-completions` | vector | `[]` | Filtered completion candidates |
| `:height-lines` | number | `1` | Minibuffer height in lines |
| `:custom-renderer` | function | `nil` | Custom Reagent component for rendering |
| `:renderer-props` | map | `{}` | Props to pass to custom renderer |

## Custom Renderers

### Overview

Commands can provide custom Reagent components to completely control minibuffer rendering. This enables Vertico-style UIs, preview panes, multi-column layouts, and more.

### Custom Renderer Function

A custom renderer is a Reagent component function that receives props:

```clojure
(defn my-custom-renderer
  "Custom minibuffer renderer with preview pane.

  Props passed automatically:
  - :minibuffer - Full minibuffer state map
  - :input-ref - Atom containing input element ref
  - :mode-line-style - Theme style for mode line
  - :prompt-style - Theme style for prompt

  Additional props from :renderer-props also merged in."
  [props]
  (let [{:keys [minibuffer mode-line-style my-custom-data]} props
        input (:input minibuffer)
        candidates (:filtered-completions minibuffer)]
    [:div.my-custom-ui
     {:style {:display "flex" :flex-direction "row"}}

     ;; Left: Candidate list
     [:div.candidates
      {:style {:flex "1" :overflow-y "auto"}}
      (for [candidate candidates]
        ^{:key candidate}
        [:div candidate])]

     ;; Right: Preview pane
     [:div.preview
      {:style {:flex "1" :border-left "1px solid gray"}}
      [:pre my-custom-data]]]))
```

### Using Custom Renderer

```clojure
(rf/reg-event-fx
  :my-fancy-command
  (fn [{:keys [db]} [_]]
    {:db (mb/push-frame db {:prompt "Select file: "
                            :custom-renderer my-custom-renderer
                            :renderer-props {:my-custom-data "Additional context"}
                            :height-lines 15
                            :on-confirm [:my-fancy-command/confirm]})}))
```

### Props Available to Custom Renderer

Your custom renderer receives a merged map of:

1. **Auto-injected props:**
   - `:minibuffer` - Full minibuffer state (includes `:input`, `:prompt`, `:completions`, etc.)
   - `:input-ref` - Atom containing reference to input DOM element
   - `:mode-line-style` - Theme face styles for mode line
   - `:prompt-style` - Theme face styles for prompt

2. **Your custom props** from `:renderer-props`

### Example: File Browser with Preview

```clojure
(defn file-browser-renderer
  [{:keys [minibuffer mode-line-style preview-content]}]
  (let [input (:input minibuffer)
        candidates (:filtered-completions minibuffer)
        selected-idx (:completion-index minibuffer)]
    [:div.file-browser
     {:style {:display "flex" :height "100%"}}

     ;; File list
     [:div.file-list
      {:style {:flex "1" :overflow-y "auto"}}
      (for [[idx file] (map-indexed vector candidates)]
        ^{:key idx}
        [:div.file-item
         {:style {:background-color (when (= idx selected-idx)
                                       "rgba(100, 100, 200, 0.3)")
                  :padding "4px"}
          :on-click #(rf/dispatch [:minibuffer/select-completion idx])}
         file])]

     ;; Preview pane
     [:div.preview-pane
      {:style {:flex "1" :border-left "1px solid gray" :padding "8px"}}
      [:pre preview-content]]]))

(rf/reg-event-fx
  :browse-files-with-preview
  (fn [{:keys [db]} [_]]
    (let [files (get-file-list)]
      {:db (mb/push-frame db {:prompt "File: "
                              :completions files
                              :filtered-completions files
                              :show-completions? true
                              :custom-renderer file-browser-renderer
                              :renderer-props {:preview-content "Select a file..."}
                              :height-lines 20
                              :on-confirm [:open-file]})})))
```

## Stack Operations

### Push Frame

Add a new minibuffer activation:

```clojure
(mb/push-frame db config)
```

### Pop Frame

Remove the current minibuffer frame:

```clojure
(mb/pop-frame db)
```

### Replace Frame

Replace current frame (useful for M-x → command transitions):

```clojure
(mb/replace-current-frame db config)
```

### Update Frame

Update current frame properties:

```clojure
(mb/update-current-frame db {:input "new text"
                             :filtered-completions new-candidates})
```

## Query Functions

### Check if Active

```clojure
(mb/minibuffer-active? db)  ; => true/false
```

### Get Depth

```clojure
(mb/minibuffer-depth db)  ; => 0 (inactive), 1+ (active)
```

### Get Current Frame

```clojure
(mb/current-frame db)  ; => frame map or nil
```

## Completion System

### Setting Completions

```clojure
{:completions ["option-1" "option-2" "option-3"]
 :show-completions? true
 :completion-index 0}
```

### Updating Filtered Completions

As user types, update filtered candidates:

```clojure
(rf/reg-event-db
  :my-command/filter
  (fn [db [_ input]]
    (let [all-candidates (:completions (mb/current-frame db))
          filtered (filter #(clojure.string/includes? % input) all-candidates)]
      (mb/update-current-frame db {:filtered-completions filtered}))))
```

### Completion Metadata

Provide metadata for completion styling/affixation:

```clojure
{:metadata {:category :file
            :affixation-function my-affixation-fn
            :annotation-function my-annotation-fn}}
```

## Recursive Minibuffer

The stack-based architecture supports recursive minibuffer activation:

```clojure
(rf/reg-event-fx
  :outer-command
  (fn [{:keys [db]} [_]]
    {:db (mb/push-frame db {:prompt "Outer: "
                            :on-confirm [:outer-done]})}))

(rf/reg-event-fx
  :outer-done
  (fn [{:keys [db]} [_]]
    ;; Don't pop yet - activate inner minibuffer first
    {:db (mb/push-frame db {:prompt "Inner: "
                            :on-confirm [:inner-done]})}))

(rf/reg-event-fx
  :inner-done
  (fn [{:keys [db]} [_]]
    ;; Pop inner, then pop outer
    {:db (-> db mb/pop-frame mb/pop-frame)}))
```

## Best Practices

1. **Always provide `:on-confirm`** - Required for proper cleanup
2. **Use `:persist?` sparingly** - Most commands should auto-close
3. **Set `:height-lines` for completion lists** - Default is 1 line
4. **Dispatch updates incrementally** - Don't batch frame updates
5. **Test with keyboard navigation** - Arrow keys, TAB, C-g should work
6. **Custom renderers should be pure** - Use Reagent best practices
7. **Handle empty completions gracefully** - Check `(seq filtered-completions)`

## Events Reference

### Standard Events

| Event | Description |
|-------|-------------|
| `[:minibuffer/set-input text]` | Update input text |
| `[:minibuffer/confirm]` | Confirm current input (calls `:on-confirm`) |
| `[:minibuffer/deactivate]` | Cancel and pop frame |
| `[:minibuffer/complete]` | TAB completion |
| `[:minibuffer/completion-next]` | Move to next candidate |
| `[:minibuffer/completion-prev]` | Move to previous candidate |
| `[:minibuffer/select-completion idx]` | Select specific candidate |

## See Also

- `lexicon.minibuffer` - Core minibuffer stack operations
- `lexicon.views/minibuffer-view` - Default renderer implementation
- Phase 6.5 Week 3-4 implementation
- Issue #72 (query-replace-regexp stack fix)
- Issue #46 (custom renderer API)
