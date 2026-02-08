(ns lexicon.core.events.window
  "Window management events - splitting, deleting, navigation.

  Emacs semantics:
  - Windows form a binary tree structure
  - Windows are views into buffers (not copies)
  - Window deletion preserves buffers"
  (:require [re-frame.core :as rf]
            [lexicon.core.db :as db]
            [lexicon.core.log :as log]))

(defn- next-window-id
  "Get next available window ID from db."
  [db]
  (let [next-id (get db :next-window-id 2)]
    next-id))

(rf/reg-event-db
 :window/split
 (fn [db [_ direction]]
   "Split the active window horizontally or vertically.

   Creates a binary tree node with two children:
   - First child: existing window
   - Second child: new window showing same buffer

   Args:
     direction - :horizontal or :vertical

   Returns:
     Updated db with new window tree structure"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         new-window-id (next-window-id db)

         ;; Find and replace the active window with a split
         split-tree (letfn [(split-at-window [tree]
                             (cond
                               (nil? tree) nil

                               ;; Found the active window - replace with split
                               (and (= (:type tree) :leaf)
                                    (= (:id tree) active-window-id))
                               {:type :split
                                :direction direction
                                :first tree
                                :second {:type :leaf
                                        :id new-window-id
                                        :buffer-id (:buffer-id tree)
                                        :cursor-position {:line 0 :column 0}
                                        :mark-position nil
                                        :viewport {:start-line 0 :end-line 40}
                                        :dimensions {:x 0 :y 0 :width 50 :height 100}}}

                               ;; Recurse through split nodes
                               (= (:type tree) :split)
                               (assoc tree
                                      :first (split-at-window (:first tree))
                                      :second (split-at-window (:second tree)))

                               ;; Not the target, return unchanged
                               :else tree))]
                     (split-at-window window-tree))]

     (-> db
         (assoc :window-tree split-tree)
         (assoc :next-window-id (inc new-window-id))))))

(rf/reg-event-db
 :window/delete-others
 (fn [db [_]]
   "Delete all windows except the active one.

   Replaces the window tree with just the active window."
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)

         ;; Find the active window in the tree
         find-active (fn find-active [tree]
                       (cond
                         (nil? tree) nil

                         (and (= (:type tree) :leaf)
                              (= (:id tree) active-window-id))
                         tree

                         (= (:type tree) :split)
                         (or (find-active (:first tree))
                             (find-active (:second tree)))

                         :else nil))

         active-window (find-active window-tree)]

     (if active-window
       (assoc db :window-tree active-window)
       db))))

(rf/reg-event-db
 :window/show-buffer
 (fn [db [_ buffer-id]]
   "Show a buffer in the active window.

   Args:
     buffer-id - Buffer to display"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)

         update-window (fn update-window [tree]
                         (cond
                           (nil? tree) nil

                           (and (= (:type tree) :leaf)
                                (= (:id tree) active-window-id))
                           (assoc tree :buffer-id buffer-id)

                           (= (:type tree) :split)
                           (assoc tree
                                  :first (update-window (:first tree))
                                  :second (update-window (:second tree)))

                           :else tree))]

     (assoc db :window-tree (update-window window-tree)))))

(rf/reg-event-fx
 :window/set-configuration
 (fn [{:keys [db]} [_ config]]
   "Restore a complete window configuration.

   Used by winner-mode to undo/redo window layouts.

   Args:
     config - Window tree configuration to restore"
   (let [all-windows (db/get-all-leaf-windows config)
         ;; Try to preserve active window if it exists in new config
         current-active (:active-window-id db)
         active-in-new? (some #(= (:id %) current-active) all-windows)
         new-active-id (if active-in-new?
                         current-active
                         (:id (first all-windows)))]
     {:db (-> db
              (assoc :window-tree config)
              (assoc :active-window-id new-active-id)
              (assoc :cursor-owner new-active-id))})))
