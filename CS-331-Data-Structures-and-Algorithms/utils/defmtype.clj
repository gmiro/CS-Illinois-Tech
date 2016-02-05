(defmacro defmtype
  "Create a primitive type with unsynchronized-mutable fields and a protocol specifying getters, swappers, and reseters.  Note
that the swap! functions are *not* atomic."
  [name fields & forms]
  (let [proto (symbol (str name "P"))
        p-getters  (map #(let [get (symbol (str "get-" %))
                               field %]
                           `(~get [this#] ~field)) fields)
        p-resetters (map #(let [reset (symbol (str "reset-" % "!"))
                               field %]
                           `(~reset [this# val#]
                                    (set! ~field val#))) fields)
        p-swappers (map #(let [swap (symbol (str "swap-" % "!"))
                               field %]
                           `(~swap [this# fun#]
                                   (set! ~field (fun# ~field)))) fields)
        m-fields (mapv (fn [x] (with-meta x {:unsynchronized-mutable true})) fields)
        ]
    `(do
       (defprotocol ~proto
         ~@p-getters
         ~@p-swappers
         ~@p-resetters)
       (deftype ~name ~m-fields
         ~proto
         ~@p-getters
         ~@p-swappers
         ~@p-resetters
         ~@forms)
         )))
