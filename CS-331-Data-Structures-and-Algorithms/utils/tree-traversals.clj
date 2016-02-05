(ns tree)

{:left nil :data "x" :right nil}

(def ops #{"+" "-" "*" "/"})

(defn preorder-to-tree
  "Create a tree from a-postorder list."
  [xx]
  (cond (empty? xx) [nil nil]
        (ops (first xx))
        (let [[left-sub-tree left-leftovers]
              (preorder-to-tree (rest xx))
              [right-sub-tree right-leftovers]
              (preorder-to-tree left-leftovers)]
          [{:left left-sub-tree
            :data (first xx)
            :right right-sub-tree}
           right-leftovers])
        :else [ {:left nil :data (first xx) :right nil}
                (rest xx)]))

(defn tree-to-postorder [xx]
  (if (nil? xx) '()
      (concat (tree-to-postorder (:left xx))
              (tree-to-postorder (:right xx))
              (list (:data xx)))))

(defn preorder-to-postorder [xx]
  (-> xx preorder-to-tree first
      tree-to-postorder))
