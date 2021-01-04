(ns ^:figwheel-hooks cljs-snake.app
  (:require [axios]
            [reagent.core :as r]
            [reagent.dom :as dom]))

;; ----------------------------------------------------------
;; linear algebra
;; ----------------------------------------------------------
(def no-turn [[1 0] [0 1]])
(def full-turn [[-1 0] [0 -1]])
(def clockwise-turn [[0 1] [-1 0]])
(def counterclockwise-turn [[0 -1] [1 0]])
(def x-offset 100)
(def y-offset 100)
(def n 7)
(def dt 1000)

(defn scale-vector
  [c v]
  (into [] (map #(* c %) v)))

(defn lin-combination
  ([v w]
   (into [] (map + v w)))
  ([v1 v2 c1 c2]
   (into [] (map + (scale-vector c1 v1) (scale-vector c2 v2)))))

(defn scalar-product
  [v w]
  (reduce + (map * v w)))

(defn lin-transform
  [M x]
  (into [] (map #(scalar-product % x) M)))

(defn vect=
  [v w]
  {:pre [(= (count v) (count w))]}
  (= v w))

;; ----------------------------------------------------------
;; snake stuff
;; ----------------------------------------------------------
(defn init-snake
  "Create snake data struct. Init head."
  []
  {:b [[0 0]]
   :v [0 1]
   :a [0 1]
   :ate false
   :running false})

(defn turn-snake
  [snake turn]
  (let [v (:v snake)]
    (assoc snake :v (lin-transform turn v))))

(defn turn-right
  [state]
  (let [v (:v state)]
    (cond
      (vect= v [0 1]) (turn-snake state clockwise-turn)
      (vect= v [0 -1]) (turn-snake state counterclockwise-turn)
      :else state)))

(defn turn-left
  [state]
  (let [v (:v state)]
    (cond
      (vect= v [0 -1]) (turn-snake state clockwise-turn)
      (vect= v [0 1]) (turn-snake state counterclockwise-turn)
      :else state)))

(defn turn-up
  [state]
  (let [v (:v state)]
    (cond
      (vect= v [1 0]) (turn-snake state clockwise-turn)
      (vect= v [-1 0]) (turn-snake state counterclockwise-turn)
      :else state)))

(defn turn-down
  [state]
  (let [v (:v state)]
    (cond
      (vect= v [-1 0]) (turn-snake state clockwise-turn)
      (vect= v [1 0]) (turn-snake state counterclockwise-turn)
      :else state)))

(defn just-ate?
  [snake]
  (:ate snake))

(defn eat!
  [snake]
  (assoc snake :ate true))

(defn move-snake
  "Return an updated snake after a move."
  [snake]
  (let [body (:b snake)
        head (first body)
        v (:v snake)
        new-head (conj [] (lin-combination v head))
        new-body (into new-head body)]
    (if (just-ate? snake)
      (assoc snake :b new-body :ate false)
      (assoc snake :b (pop new-body)))))

(defn snack-time?
  [snake]
  (vect= (first (:b snake)) (:a snake)))

(defn snake-nom
  [snake]
  (if (snack-time? snake)
    (eat! snake)
    snake))

(defn respawn-apple
  "TODO: ye here"
  [state]
  (let [apple (:a state)
        body (:b state)
        new-apple [(rand-int n) (rand-int n)]]
    (if (snack-time? state)
      (if (some #{new-apple} body)
        (respawn-apple state)
        (assoc state :a new-apple))
      state)))

(defn game-over? [state]
  (let [body (:b state)
        flat-body (flatten body)
        l (count body)
        m (count (into #{} body))]
    (or
     (some neg? flat-body)
     (not-every? #(< % n) flat-body)
     (not= l m))))

(defn time-step
  [snake]
  (-> snake
      snake-nom
      respawn-apple
      move-snake))

;; ----------------------------------------------------------
;; state and params
;; ----------------------------------------------------------
(def initial-state (init-snake))
(defonce game-state (r/atom initial-state))

;; ----------------------------------------------------------
;; utils
;; ----------------------------------------------------------
(defn px [n]
  (str n "px"))

(defn create-grid [n]
  [:div 
   (for [i (range (* n n))]
     (let [l 50
           g 10
           d (+ l g)
           p (mod i n)
           q (quot i n)]
       ^{:key i}
       [:div.cell
        {:style {:left (px (+ x-offset (* d p)))
                 :top (px (+ y-offset (* d q)))}}]))])

(defn create-snake [state]
  [:div
   (for [body-part (:b state)]
     (let [x (first body-part)
           y (last body-part)
           l 50
           g 10
           d (+ l g)]
       ^{:key (str (random-uuid))}
       [:div.snake
        {:style {:left (px (+ x-offset (* d x)))
                 :top (px (+ y-offset (* d y)))}}]))])

(defn create-apple [state]
  [:div 
   (let [l 50
         g 10
         d (+ l g)
         apple (:a state)
         x (first apple)
         y (last apple)]
     [:div.apple
      {:style {:left (px (+ x-offset (* d x)))
               :top (px (+ y-offset (* d y)))}}])])

(defn switch-game [state]
  (if (:running state)
    (assoc state :running false)
    (assoc state :running true)))

(defn reset-game []
  (reset! game-state initial-state))

;; ----------------------------------------------------------
;; page rendering
;; ----------------------------------------------------------
(defn app
  []
  [:div 
   [:h1 {:style {:color "green"}} "Welcome to the snake game! HISSSS~~~~"]
   [:input {:type "button" :value (if (:running @game-state) "Stop" "Start")
            :on-click #(swap! game-state switch-game)}]
   [create-grid n]
   [create-apple @game-state]
   [create-snake @game-state]])

(defn render-app
  "Connect this clojure code to html."
  []
  (dom/render [app] (.getElementById js/document "snake_container")))

(defn ^:after-load re-render
  "This auxilary function makes sure that page is
  re-rendered on each figwheel code reload."
  []
  (render-app))

(defonce init-app
  (do
    (.addEventListener js/document "keydown"
                       (fn [e]
                         (cond
                           (= (.-key e) "ArrowLeft") (swap! game-state turn-left)
                           (= (.-key e) "ArrowRight") (swap! game-state turn-right)
                           (= (.-key e) "ArrowUp") (swap! game-state turn-up)
                           (= (.-key e) "ArrowDown") (swap! game-state turn-down))))
    (.setInterval js/window
                  (fn []
                    (if (game-over? @game-state)
                      (do
                        (js/alert "GAME OVER")
                        (reset-game))
                      (when (:running @game-state)
                        (swap! game-state time-step))))
                  dt)
    (render-app)
    true))

;; ;; Apples
;; ;; 1. apple should appear at the random position on the game grid
;; ;; 2. if head and apple have the same coord on some time step then the
;; ;;    apple should reappear somewhere else for the next time step
;; ;; 3. apple can not appear on any tile that is occupied by the snake's
;; ;;    body. Moreover it cannot appear in the same position again
;; ;;    because that position is then occupied by the snake's head
;; TODO: [X] add check on intersections;
;; TODO: [ ] spawn apple outside of snake body;
;; TODO: [X] change the turns of the snake according to the current
;; direction it is facing and then left/right accordingly
