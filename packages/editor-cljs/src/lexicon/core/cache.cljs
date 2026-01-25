(ns lexicon.core.cache
  "Text range caching system for efficient virtual scrolling and large document handling"
  (:require [lexicon.core.wasm-utils :as wasm]))

;; Cache entry structure
(defrecord CacheEntry [start end content timestamp valid?])

;; Cache configuration
(def cache-config
  {:max-entries 100        ; Maximum number of cached ranges
   :chunk-size 1000        ; Default chunk size for caching
   :overlap-size 200       ; Overlap between chunks for smooth scrolling
   :ttl-ms (* 5 60 1000)   ; Time-to-live: 5 minutes
   :cleanup-interval-ms (* 1 60 1000)}) ; Cleanup every minute

;; Cache utilities
(defn cache-key [start end]
  "Generate cache key for a text range"
  [start end])

(defn entry-expired? [entry current-time]
  "Check if cache entry has expired"
  (> (- current-time (:timestamp entry)) 
     (:ttl-ms cache-config)))

(defn entries-overlap? [entry1 entry2]
  "Check if two cache entries overlap"
  (let [{start1 :start end1 :end} entry1
        {start2 :start end2 :end} entry2]
    (not (or (>= start1 end2) (>= start2 end1)))))

(defn merge-overlapping-entries [entry1 entry2 ^js wasm-handle]
  "Merge two overlapping cache entries"
  (let [merged-start (min (:start entry1) (:start entry2))
        merged-end (max (:end entry1) (:end entry2))
        merged-content (.getRange wasm-handle merged-start merged-end)]
    (->CacheEntry merged-start merged-end merged-content (js/Date.now) true)))

(defn find-covering-entry [cache start end]
  "Find cache entry that completely covers the requested range"
  (some (fn [[_ entry]]
          (when (and (:valid? entry)
                     (<= (:start entry) start)
                     (>= (:end entry) end))
            entry))
        cache))

(defn find-overlapping-entries [cache start end]
  "Find all cache entries that overlap with the requested range"
  (filter (fn [[_ entry]]
            (and (:valid? entry)
                 (entries-overlap? entry {:start start :end end})))
          cache))

(defn extract-from-cache [entry start end]
  "Extract a substring from a cached entry"
  (let [entry-start (:start entry)
        entry-content (:content entry)
        relative-start (- start entry-start)
        relative-end (- end entry-start)
        content-length (count entry-content)]
    (if (and (>= relative-start 0) 
             (<= relative-end content-length))
      (subs entry-content relative-start relative-end)
      nil)))

;; Cache operations
(defn cleanup-cache [cache]
  "Remove expired and invalid entries from cache"
  (let [current-time (js/Date.now)]
    (->> cache
         (remove (fn [[_ entry]]
                   (or (not (:valid? entry))
                       (entry-expired? entry current-time))))
         (into {}))))

(defn cache-size [cache]
  "Get the number of valid entries in cache"
  (count (filter (fn [[_ entry]] (:valid? entry)) cache)))

(defn evict-oldest [cache]
  "Remove the oldest entry if cache is full"
  (if (> (cache-size cache) (:max-entries cache-config))
    (let [oldest-key (->> cache
                         (filter (fn [[_ entry]] (:valid? entry)))
                         (sort-by (fn [[_ entry]] (:timestamp entry)))
                         first
                         first)]
      (dissoc cache oldest-key))
    cache))

(defn add-to-cache [cache start end content]
  "Add a new entry to the cache"
  (let [key (cache-key start end)
        entry (->CacheEntry start end content (js/Date.now) true)
        updated-cache (assoc cache key entry)]
    (evict-oldest updated-cache)))

(defn invalidate-range [cache start end]
  "Invalidate cache entries that overlap with the modified range"
  (reduce (fn [acc [key entry]]
            (if (entries-overlap? entry {:start start :end end})
              (assoc acc key (assoc entry :valid? false))
              acc))
          cache
          cache))

(defn get-cached-text [cache ^js wasm-handle start end]
  "Get text from cache or fetch from WASM if not cached"
  (cond
    ;; Check if we have a covering entry
    (when-let [covering-entry (find-covering-entry cache start end)]
      (extract-from-cache covering-entry start end))
    
    ;; If not, try to assemble from overlapping entries
    (let [overlapping (find-overlapping-entries cache start end)]
      (when (and (seq overlapping)
                 ;; Simple case: single overlapping entry that covers our range
                 (= (count overlapping) 1))
        (let [[_ entry] (first overlapping)]
          (extract-from-cache entry start end))))
    
    ;; Fallback: fetch from WASM and cache
    :else
    (let [[content success?] (wasm/get-text-range-safe wasm-handle start end)]
      (if success?
        [content (add-to-cache cache start end content)]
        ["" cache]))))

(defn prefetch-ranges [cache ^js wasm-handle start end]
  "Prefetch adjacent ranges for smooth scrolling"
  (let [chunk-size (:chunk-size cache-config)
        overlap (:overlap-size cache-config)
        
        ; Calculate prefetch ranges
        before-start (max 0 (- start chunk-size))
        before-end (+ before-start chunk-size)
        
        after-start (max end (- end overlap))
        after-end (+ after-start chunk-size)
        
        updated-cache (atom cache)]
    
    ; Prefetch before range if not cached
    (when-not (find-covering-entry @updated-cache before-start before-end)
      (let [[content success?] (wasm/get-text-range-safe wasm-handle before-start before-end)]
        (when success?
          (swap! updated-cache add-to-cache before-start before-end content))))
    
    ; Prefetch after range if not cached
    (when-not (find-covering-entry @updated-cache after-start after-end)
      (let [[content success?] (wasm/get-text-range-safe wasm-handle after-start after-end)]
        (when success?
          (swap! updated-cache add-to-cache after-start after-end content))))
    
    @updated-cache))

;; Public API functions
(defn create-cache []
  "Create a new empty text cache"
  {})

(defn get-text-range [cache ^js wasm-handle start end & {:keys [prefetch?] :or {prefetch? true}}]
  "Get text range with caching - returns [text updated-cache]"
  (if-not wasm-handle
    ["" cache]
    (let [cleaned-cache (cleanup-cache cache)
          [text updated-cache] (get-cached-text cleaned-cache wasm-handle start end)
          final-cache (if prefetch?
                       (prefetch-ranges updated-cache wasm-handle start end)
                       updated-cache)]
      [text final-cache])))

(defn invalidate-cache-range [cache start end]
  "Invalidate cache entries affected by text modifications"
  (invalidate-range cache start end))

(defn cache-stats [cache]
  "Get cache statistics for debugging"
  (let [valid-entries (filter (fn [[_ entry]] (:valid? entry)) cache)
        total-chars (reduce + (map (fn [[_ entry]] 
                                    (count (:content entry))) 
                                  valid-entries))]
    {:total-entries (count cache)
     :valid-entries (count valid-entries)
     :total-cached-chars total-chars
     :memory-estimate-kb (/ total-chars 512)})) ; Rough estimate