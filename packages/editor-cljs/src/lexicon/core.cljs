(ns lexicon.core
  (:require ["@codemirror/state" :refer [EditorState]]
            ["@codemirror/view" :refer [EditorView keymap]]
            ["@codemirror/commands" :refer [defaultKeymap]]))

(defn create-editor-state []
  (.create EditorState
           #js {:doc "Welcome to Lexicon!\n\nStart typing to see the editor in action."
                :extensions #js [(.of keymap defaultKeymap)]}))

(defn create-editor-view [state container]
  (EditorView.
   #js {:state state
        :parent container}))

(defn init []
  (let [container (.getElementById js/document "editor")
        state (create-editor-state)
        view (create-editor-view state container)]
    (println "Lexicon editor initialized!")))

(defn ^:export main []
  (init))