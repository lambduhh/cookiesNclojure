(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn error [& args]
  (apply println args)
  :error)

(def baking {:recipes     {:cake     {:ingredients {:egg   2
                                                    :flour 2
                                                    :sugar 1
                                                    :milk  1}
                                      :steps       [[:add :all]
                                                    [:mix]
                                                    [:pour]
                                                    [:bake 25]
                                                    [:cool]]}
                           :cookies  {:ingredients {:egg    1
                                                    :flour  1
                                                    :butter 1
                                                    :sugar  1}
                                      :steps       [[:add :all]
                                                    [:mix]
                                                    [:pour]
                                                    [:bake 30]
                                                    [:cool]]}
                           :brownies {:ingredients {:egg    2
                                                    :flour  2
                                                    :butter 2
                                                    :cocoa  2
                                                    :sugar  1
                                                    :milk   1}
                                      :steps       [[:add :butter]
                                                    [:add :cocoa]
                                                    [:add :sugar]
                                                    [:mix]
                                                    [:add :egg]
                                                    [:add :flour]
                                                    [:add :milk]
                                                    [:mix]
                                                    [:pour]
                                                    [:bake 35]
                                                    [:cool]]}}
             :ingredients {:egg    {:storage :fridge
                                    :usage   :squeezed}
                           :milk   {:storage :fridge
                                    :usage   :scooped}
                           :flour  {:storage :pantry
                                    :usage   :scooped}
                           :butter {:storage :fridge
                                    :usage   :simple}
                           :sugar  {:storage :pantry
                                    :usage   :scooped}
                           :cocoa  {:storage :pantry
                                    :usage   :scooped}}})

(def usage {:squeezed (fn [ingredient amount]
                        (dotimes [i amount]
                          (grab ingredient)
                          (squeeze)
                          (add-to-bowl)))
            :simple   (fn [ingredient amount]
                        (dotimes [i amount]
                          (grab ingredient)
                          (add-to-bowl)))
            :scooped  (fn [ingredient amount]
                        (grab :cup)
                        (dotimes [i amount]
                          (scoop ingredient)
                          (add-to-bowl))
                        (release))})

(defn usage-type [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (get info :usage)))

(defn add
  ([ingredient]
   (add ingredient 1))
  ([ingredient amount]
   (let [ingredient-type (usage-type ingredient)]
     (if (contains? usage ingredient-type)
       (let [f (get usage ingredient-type)]
         (f ingredient amount))
       (error "I do not know the ingredient" ingredient)))))

(def actions {:cool (fn [ingredients step]
                      (cool-pan))
              :mix  (fn [ingredients step]
                      (mix))
              :pour (fn [ingredients step]
                      (pour-into-pan))
              :bake (fn [ingredients step]
                      (bake-pan (second step)))
              :add  (fn [ingredients step]
                      (cond
                        (and (= 2 (count step))
                             (= :all (second step)))
                        (doseq [kv ingredients]
                          (add (first kv) (second kv)))
                        (and (= 2 (count step))
                             (contains? ingredients (second step)))
                        (add (second step) (get ingredients (second step)))
                        (= 3 (count step))
                        (add (second step) (get step 2))
                        :else
                        (error "I don't know how to add" (second step) (get step 2))))})

;(defn perform [ingredients step]
;  (let [f (get actions (first step) (fn [ingredients step]
;                                      (println "I do not know how to" (first step))))]
;    (f ingredients step)))

(declare perform)

(defn bake-recipe [recipe]
  (last
    (for [step (get recipe :steps)]
      (perform (get recipe :ingredients) step))))
;
(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))
;
(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (let [ingredients (get baking :ingredients)
         info (get ingredients ingredient)]
     (if (contains? ingredients ingredient)
       (do
         (go-to (get info :storage))
         (load-up-amount ingredient amount)
         (go-to :prep-area)
         (unload-amount ingredient amount))
       (error "I don't know the ingredient" ingredient)))))

(defn storage-location [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (get info :storage)))

(defn fetch-list [shopping]
  (let [by-location (group-by (fn [item-amount]
                                (storage-location (first item-amount)))
                              shopping)]
    (doseq [loc by-location]
      (go-to (first loc))
      (doseq [item-amount (second loc)]
        (load-up-amount (first item-amount) (second item-amount)))))

  (go-to :prep-area)
  (doseq [item-amount shopping]
    (unload-amount (first item-amount) (second item-amount))))

(defn add-ingredients [a b]
  (merge-with + a b))

(defn multiply-ingredients [n ingredients]
  (into {}
        (for [kv ingredients]
          [(first kv) (* n (second kv))])))

(defn order->ingredients [order]
  (let [recipes (get baking :recipes)
        items (get order :items)]
    (reduce add-ingredients {}
            (for [kv items]
              (let [recipe (get recipes (first kv))
                    ingredients (get recipe :ingredients)]
                (multiply-ingredients (second kv) ingredients))))))

(defn orders->ingredients [orders]
  (reduce add-ingredients {}
          (for [order orders]
            (order->ingredients order))))

(defn bake [item]
  (let [recipes (get baking :recipes)]
    (bake-recipe (get recipes item))))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders-day3)
        ingredients (orders->ingredients orders)]
    (fetch-list ingredients)
    (doseq [order orders]
      (let [items (get order :items)
            racks (for [kv items
                        i (range (second kv))]
                    (bake (first kv)))
            receipt {:orderid (get order :orderid)
                     :address (get order :address)
                     :rackids racks}]
        (delivery receipt)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write programs here ;) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;code i coded

(def baking {:recipes     {:cake     {:ingredients {:egg   2
                                                    :flour 2
                                                    :sugar 1
                                                    :milk  1}
                                      :steps       [[:add :all]
                                                    [:mix]
                                                    [:pour]
                                                    [:bake 25]
                                                    [:cool]]}
                           :cookies  {:ingredients {:egg    1
                                                    :flour  1
                                                    :butter 1
                                                    :sugar  1}
                                      :steps       [[:add :all]
                                                    [:mix]
                                                    [:pour]
                                                    [:bake 30]
                                                    [:cool]]}
                           :brownies {:ingredients {:egg    2
                                                    :flour  2
                                                    :butter 2
                                                    :cocoa  2
                                                    :sugar  1
                                                    :milk   1}
                                      :steps       [[:add :butter]
                                                    [:add :cocoa]
                                                    [:add :sugar]
                                                    [:mix]
                                                    [:add :egg]
                                                    [:add :flour]
                                                    [:add :milk]
                                                    [:mix]
                                                    [:pour]
                                                    [:bake 35]
                                                    [:cool]]}}})

(defn perform [ingredients step]
  (println "performing")
  (cond
    (= :cool (first step)) (cool-pan)
    (= :mix (first step)) (mix)
    (= :pour (first step)) (pour-into-pan)
    (= :bake (first step)) (bake-pan (second step))
    (= :add (first step))
    (let []
      (println "Adding")
      (cond
        (and (= 2 (count step))
             (= :all (second step)))
        (doseq [kv ingredients]
          (add (first kv) (second kv)))
        (and (= 2 (count step))
             (contains? ingredients (second step)))
        (add (second step) (get ingredients (second step)))
        (= 3 (count step))
        (add (second step) (get step 2))
        :else
        (error "I can't add" (second step) (get step 2))))
    :else
    (error "i dunno how to do" (first step))))

(defn bake-recipe [recipe]
  (println recipe)
  (let [ingredients (get recipe :ingredients)]
    (last
      (for [step (get recipe :steps)]
        (perform ingredients step)))))

(comment
  (bake-recipe (get (get baking :recipes) :cake))
  )


(def scooped-ing #{:milk :flour :sugar :cocoa})

(defn scooped? [ing]
  (contains? scooped-ing ing))

(def squeezed-ing #{:egg})

(defn squeezed? [ing]
  (contains? squeezed-ing ing))

(def simple-ing #{:butter})

(defn simple? [ing]
  (contains? simple-ing ing))

;; workflow

;; writing basic functions


;;;  IF'S AND DO'S

:Refactored

(defn add-squeezed
  ([ingredient n]
   (if (squeezed? ingredient)
     (do
       (dotimes [i n]
         (grab ingredient)
         (squeeze)
         (add-to-bowl))
       :ok)
     (error "this only works on sqeezzeddd")))
  ([ingredient]
   (add-squeezed ingredient 1)))


(defn add-scooped
  ([ing n]
   (if (scooped? ing)
     (do
       (dotimes [i n]
         (grab :cup)
         (scoop ing)
         (add-to-bowl)
         (release)) :ok)
     (error "this only works on scooped")))
  ([ing]
   (add-scooped ing 1)))


(defn add-simple
  ([ing n]
   (if (simple? ing)
     (do
       (dotimes [i n]
         (grab ing)
         (add-to-bowl) :ok)
       (error "not butta "))))
  ([ing]
   (add-simple ing 1)))



;; dotimes (exercise 9)
;
;(defn add-eggs [n]
;  (dotimes [e n]
;    (add-egg))
;  :ok)
;
;(defn add-flour-cups [n]
;  (dotimes [e n]
;    (add-flour))
;  :ok)
;
;(defn add-sugar-cups [n]
;  (dotimes [e n]
;    (add-sugar)) :ok)
;
;(defn add-milk-cups [n]
;  (dotimes [e n]
;    (add-milk)) :ok)
;
;(defn add-butters [n]
;  (dotimes [e n]
;    (add-butter)) :ok)

;; Exercise 10 Rewrite bake-cake to use the new functions

#_(defn bake-cake []
  (add-flour-cups 2)
  (add-eggs 2)
  (add-milk)
  (add-sugar)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))


; Exercise 12- Rewrite bake-cake to use the new add function

(defn bake-cake []
  (add :flour 2)
  (add :egg 2)
  (add :milk)
  (add :sugar)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))
;Exercise 13- Write a fn 'bake-cookies' to make it have structure similar
;to the recipe
(defn bake-cookies []
  (add :egg)
  (add :flour)
  (add :sugar)
  (add :butter)
  (mix)
  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(defn bake-brownies []
  (add :butter 2)
  (add :sugar 1)
  (add :cocoa 2)
  (mix)
  (add :flour 2)
  (add :milk 1)
  (add :egg 2)
  (mix)
  (pour-into-pan)
  (bake-pan 35)
  (cool-pan))



; DAY 2 AT THE BAKERY

; Rest Arguments
(defn my-fn [& rs]
  (apply + rs))

; Def - defines a local variable
; Sets #{}  - Order doesn't matter, no duplicates, fast containment check
; (contains?) - checks whether item is present
; any value can go inside, even other sets
(def pantry-ingredients #{:flour :sugar :cocoa})

(defn from-pantry? [ing]
  (contains? pantry-ingredients ing))

; Exercise 1- Write a fn (fridge-ing) and (from-fridge?) using this stuff
(def fridge-ingredients #{:egg :butter :milk})

(defn from-fridge? [ing]
  (contains? fridge-ingredients ing))

; Exercise 2- Refactor (scooped?) (squeezed?) and (simple?) to use new shit

(def scooped-ing #{:milk :flour :sugar :cocoa})

(defn scooped? [ing]
  (contains? scooped-ing ing))

(def squeezed-ing #{:egg})

(defn squeezed? [ing]
  (contains? squeezed-ing ing))

(def simple-ing #{:butter})

(defn simple? [ing]
  (contains? simple-ing ing))

; Write a fn to fetch things from pantry
; (go-to :pantry, get-ing, load-up, bring back and unload
; Similar pattern to add-scooped etc AKA varadic fns
(defn fetch-from-pantry
  ([ing]
   (fetch-from-pantry ing 1))
  ([ing n]
   (if (from-pantry? ing)
     (do
       (go-to :pantry)
       (dotimes [i n]
         (load-up ing))
       (go-to :prep-area)
       (dotimes [i n]
         (unload ing)))
     (error "not in pantry bro, try again" ing))))

(defn fetch-from-fridge
  ([ing]
   (fetch-from-fridge ing 1))
  ([ing n]
   (if (from-fridge? ing)
     (do
       (go-to :fridge)
       (dotimes [i n]
         (load-up ing))
       (go-to :prep-area)
       (dotimes [i n]
         (unload ing)))
     (error "Do I look like a cold ingredient?!? try again " ing))))
; Exercise 3- Write a fn (fetch-ing) which takes the name of an ing
; and an amount [n] and fetches that amount of the ing. Also make it accept
; just the ing, with a default amount of 1
(defn fetch-ing
  ([ing]
   (fetch-ing ing 1))
  ([ing n]
   (cond
     (from-pantry? ing) (fetch-from-pantry ing n)
     (from-fridge? ing) (fetch-from-fridge ing n)
     :else (error "idk where that is" ing))))
; Hash-Maps {} have key assoc w value as pairs. order doesnt matter, fast lookup by key, any type
; Uses (get- map key) and (contains? map key)=> true
; (get map key *optional 3rd arg is what to return instead of nil)

; (when)- a conditional used when you only have one thing to react to. value of last expression returned
;(when (scooped? :flour)
; (println "flour is scooped")
;:ok) ;value is the value of last expression

; Write a fn that can fetch a list of ingredients
(def ingredients
  {:flour  10
   :egg    9
   :sugar  18
   :milk   20
   :butter 9})

; Putting a 4th arg on get allows the not found ing to resolve to 0 instead do nil
; 0 can be manipulated by dotimes and other fns

; (doseq) goes over items in a seq in order. creates side effects
; (doseq) [variable-vector sequence]
;     (println x))

(defn load-up-amount [ing n]
  (dotimes [i n]
    (load-up ing)))
(defn unload-amount [ing n]
  (dotimes [i n]
    (unload ing)))

(def locations {:pantry pantry-ingredients
                :fridge fridge-ingredients})

(defn fetch-list [ls]
  (doseq [location (keys locations)]
    (go-to location)
    (doseq [ing (get locations location)]
      (load-up-amount ing (get ls ing 0))))
  (go-to :prep-area)
  (doseq [location (keys locations)]
    (doseq [ing (get locations location)]
      (unload-amount ing (get ls ing 0)))))

; Day at the Bakery II
; 1- (get-morning-orders)
; 2- (fetch) get necessary ingredients
; 3- (bake)
; 4- (delivery)
;Exercise 4- Write a fn (day-at-the-bakery) which represents what X5 does at the bakery
;Must get orders, bake items and send receipts



;(defn day-at-the-bakery []
;  (doseq [order (get-morning-orders)]
;    (let [items (get order :items)]
;      (dotimes [n (get items :cake 0)]
;        (fetch-list {:egg   2
;                     :flour 2
;                     :sugar 1
;                     :milk  1})
;        (let [rack-id (bake-cake)]
;          (delivery {:orderid (get order :orderid)
;                     :address (get order :address)
;                     :rackids [rack-id]})))
;      (dotimes [n (get items :cookies 0)]
;        (fetch-list {:flour  1
;                     :sugar  1
;                     :butter 1
;                     :egg    1})
;        (let [rack-id (bake-cookies)]
;          (delivery {:orderid (get order :orderid)
;                     :address (get order :address)
;                     :rackids [rack-id]}))))))

;Exercise 5 - Name flaw in this code and ways to fix them
; 1a- X5 goes to pantry before each time he bakes each individual cake/cookies (dotimes[n get items :cake)
; 1b- He should make one trip to pantry and fridge to get all ing for cookies cake
; 2a- There is prob a way he can do the receipt process one time as well
; 3a- Can baking the cake/cookies and the fetch-list map be abstracted/factor out of the fn?

;Exercise 6- Write a function (add-ingredients) which takes two ing lists and adds them together
; using (merge-with)

(def cookie-ing {:flour  1
                 :sugar  1
                 :butter 1
                 :egg    1})

(def cake-ing {:flour 2
               :sugar 1
               :egg   2
               :milk  1})

(def brown1-ing {:butter 2
                 :cocoa 2
                 :sugar 1} )
(def brown2 {:flour 2
             :milk 1
             :egg 2})

(def all-brownie-ing (merge-with + brown1-ing brown2 ))

(defn add-ingredients [a & b]
  (merge-with + a b ))


;(merge-with + a b))

;; For stuff    ;first kv   ;second kv
;(def foo (for [[ingredient quantity]
;             {:flour 2
;              :sugar 2
;              :egg   2
;              :milk  1}]
;         (do
;           (println "bloop!")
;           [ingredient (* 2 quantity)])))

;Exercise 7- Write a fn (multiply-ings) that takes a quantity and and ing list and returns a new
; ing list with all the amounts multiplied by the quantity. Use into and for.



(defn multiply-ings [n ingredientlist]
  (into {}
        (for [[ing amount] ingredientlist]
          [ing (* n amount)])))

;Exercise 8- Write a fn (order->ings) which takes an order and returns an ing list for everything
; needed in that order. You should use add-ing and multiply-ing

(defn order->ings [order]
  (let [items (get order :items 0)
        cake-count (get items :cake 0)
        cookie-count (get items :cookies 0)
        brownie-count (get items :brownies 0)
        cake-total (multiply-ings cake-count cake-ing)
        cookie-total (multiply-ings cookie-count cookie-ing)
        brownie-total (multiply-ings brownie-count all-brownie-ing)]
    (add-ingredients cake-total cookie-total brownie-total)))

; Destructuring practice ^ is the same thing as
(defn order->ings [{:keys [items]}]
  (let [{:keys [cake cookies brownies] :or {cake 0 cookies 0 brownies 0 }} items
        cake-total (multiply-ings cake cake-ing)
        cookie-total (multiply-ings cookies cookie-ing)
        brownie-total (multiply-ings brownies all-brownie-ing)]
    (add-ingredients cake-total cookie-total brownie-total)))

;Exercise 9-Write a fn [plural](orders->ings) that builds a total ing list from a list of orders.
; use (map) and (reduce)

(defn orders->ings [ls & args]
  (reduce add-ingredients {}
          (for [order ls]
            (order->ings order))))
;Exercise 10- Rewrite (day-at-the-bakery) to make a single shopping trip before starting to bake.
;Exercise 11- Rework (day-at-the-bakery) to make X5 deliver one receipt per order (instead of per item) use for
;; and range
(defn bake [item]
  (cond
    (= item :cake) (bake-cake)
    (= item :cookies) (bake-cookies)
    (= item :brownies) (bake-brownies)
    :else (error "idk how to bake" item)))


(defn day-at-the-bakery []
  (let [orders (get-morning-orders)
        ing (orders->ings orders)]
    (fetch-list ing)
    (doseq [order orders]
      (let [items (get order :items)
            racks (for [kv items
                        i (range (second kv))]
                    (bake (first kv)))]
        (delivery {:orderid (get order :orderid)
                     :address (get order :address)
                     :rackids racks})))))










(comment




  )




;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#_(defn -main []
  (day-at-the-bakery))
