(ns lisb.examples.connect-four
  (:require [lisb.core :refer [evaluate state-space]])
  (:require [lisb.translation.representation :refer :all])
  (:require [lisb.translation.translationOLD :refer [to-ast]])
  (:require [lisb.translation.data-conversion :refer [convert]])
  
  #_(:require [seesaw.core :refer [repaint! canvas show! frame]]
            [seesaw.graphics :refer [circle style draw]]))


;;(defpred won?-pred [board winners]
;;  (and (subset? winners board)
;;       (member? board (+-> (struct "x" (bint-set) "y" (bint-set)) #{:r :y}))
;;       (member? winners (+-> (struct "x" (bint-set) "y" (bint-set)) #{:r :y}))
;;       (not= :r :y)
;;       (= winners #{:e1 :e2 :e3 :e4})
;;       (not= :e1 :e2 :e3 :e4)
;;       (let [x1 (brec-get :e1 "x")
;;             x2 (brec-get :e2 "x")
;;             x3 (brec-get :e3 "x")
;;             x4 (brec-get :e4 "x")
;;             y1 (brec-get :e1 "y")
;;             y2 (brec-get :e2 "y")
;;             y3 (brec-get :e3 "y")
;;             y4 (brec-get :e4 "y")
;;             xs #{x1 x2 x3 x4}
;;             ys #{y1 y2 y3 y4}
;;             ]
;;         (or (and (= x1 x2 x3 x4)
;;                  (= (- (max ys) (min ys)) 3))
;;             (and (= y1 y2 y3 y4)
;;                  (= (- (max xs) (min xs)) 3))
;;             (and (= x1 (inc x2) (+ x3 2) (+ x4 3))
;;                  (= y1 (inc y2) (+ y3 2) (+ y4 3)))
;;             (and (= x1 (inc x2) (+ x3 2) (+ x4 3))
;;                  (= y1 (dec y2) (- y3 2) (- y4 3)))))))

(defpred won?-pred [board winners]
  (and #_(member? board   (+-> (struct "x" (bint-set) "y" (bint-set)) #{:r :y}))
       #_(member? winners (+-> (struct "x" (bint-set) "y" (bint-set)) #{:r :y}))
       (= (count winners) 4)
       (subset? winners board)
       (= #{:r :y} #{0 1})
       (member? :x #{:r :y})
       (= winners #{[:e1 :x] [:e2 :x] [:e3 :x] [:e4 :x]})
       (none= :e1 :e2 :e3 :e4)  
       (let [x1 (brec-get :e1 "x")
             x2 (brec-get :e2 "x")
             x3 (brec-get :e3 "x")
             x4 (brec-get :e4 "x")
             y1 (brec-get :e1 "y")
             y2 (brec-get :e2 "y")
             y3 (brec-get :e3 "y")
             y4 (brec-get :e4 "y")
             xs #{x1 x2 x3 x4}
             ys #{y1 y2 y3 y4}]
         (or (and (= x1 x2 x3 x4)
                  (= (- (max ys) (min ys)) 3))
             (and (= y1 y2 y3 y4)
                  (= (- (max xs) (min xs)) 3))
             (and (= x1 (inc x2) (+ x3 2) (+ x4 3))
                  (= y1 (inc y2) (+ y3 2) (+ y4 3)))
             (and (= x1 (inc x2) (+ x3 2) (+ x4 3))
                  (= y1 (dec y2) (- y3 2) (- y4 3)))))))


(def height 6)
(def width 7)

(defn new-game []
  {:board {}
   :turn :r
   :won false})

(def state (atom nil))

(defn init! [] (reset! state (new-game)))


(defn get-new-pos [b c]
  (let [ypos (->> b
                  (keys)
                  (filter #(= (:x %) c))
                  (map :y)
                  (reduce max -1)
                  (inc))]
    {:x c, :y ypos}))



(defn won? [board _]
  (prn :board board)
  (let [bboard (convert board :fn [[:record [{:x :int, :y :int}]
                                   [:set :thingies]]])]
    (prn :bboard bboard)
    (evaluate (to-ast (won?-pred bboard :winners)))))


(defn turn [m c f]
  (let [new-stone (get-new-pos (:board m) c)
        new-board (assoc (:board m) new-stone f)
        new-turn ({:r :y, :y :r} f)
        new-won (won? new-board new-stone)]
    (prn :trololo new-won)
    {:board new-board
     :turn new-turn
     :won new-won}))


(defn turn! [c f]
  (let [s @state]
    (when (and (= f (:turn s)) ; it's the player's turn
               (not ((:board s) {:x c :y (dec height)})) ; column is not full
               (not (:won s)))
      (swap! state turn c f))))




(defn get-color [c]
  (get {:r :red :y :yellow} c :grey))

#_(defn paint-board [c g]
  (let [s @state
        board (:board s)]
    (doseq [x (range 0 7) y (range 0 6)]
      (let [color (get-color (board {:x x :y y}))]
        (draw g (circle (* (inc x) 50) (* (- 6 y) 50) 20) 
                (style :foreground :black
                       :background color))))
    #_(when (:won s)
      (doseq [winner (:won s)
              stone winner]
          (draw g (circle (* (inc (:x (first stone))) 50) 
                          (* (- 6 (:y (first stone))) 50)
                          20) 
                  (style :foreground :black
                         :background :lightgreen))))))



(defn click-event [e] 
  (let [col (int (/ (- (.getX e) 25) 50))]
    (when (<= 0 col 6)
      (turn! col (:turn @state)))))

#_(defn gui! []
    (let [cvs (canvas :id :canvas :background :blue :listen [:mouse-clicked click-event] :paint #(paint-board % %2))]
       (-> (frame
         :title "Seesaw Connect Four"
         :width 400 :height 360
         :content cvs) show!)
       (add-watch state :watcher (fn [_ _ _ _] (repaint! cvs)))
       nil))
