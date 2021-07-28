; Inspired by the snakes the have gone before:
; Abhishek Reddy's snake: http://www.plt1.com/1070/even-smaller-snake/
; Mark Volkmann's snake: http://www.ociweb.com/mark/programming/ClojureSnake.html 

(ns examples.atom-snake
  (:require [membrane.ui :as ui]))

(defonce log (atom []))
; ----------------------------------------------------------
; functional model
; ----------------------------------------------------------
(def width 28)
(def height 45)
(def point-size 10)
(def turn-millis 75)
(def win-length 5)
(def dirs {:left  [-1  0]
           :right [ 1  0]
           :up    [ 0 -1]
	   :down  [ 0  1]})

(defn add-points [& pts] 
  (vec (apply map + pts)))

(defn point-to-screen-rect [pt] 
  (map #(* point-size %) 
       [(pt 0) (pt 1) 1 1]))

(defn create-apple [] 
  {:location [(rand-int width) (rand-int height)]
   :color [0.823 0.196 0.352]
   :type :apple}) 

(defn create-snake []
  {:body (list [1 1]) 
   :dir [1 0]
   :type :snake
   :color [0.058 0.627 0.274]})

(defn move [{:keys [body dir] :as snake} & grow]
  (assoc snake :body (cons (add-points (first body) dir) 
			   (if grow body (butlast body)))))

(defn turn [snake newdir] 
  (if newdir (assoc snake :dir newdir) snake))

(defn win? [{body :body}]
  (>= (count body) win-length))

(defn head-overlaps-body? [{[head & body] :body}]
  (contains? (set body) head))

(def lose? head-overlaps-body?)

(defn eats? [{[snake-head] :body} {apple :location}]
   (= snake-head apple))

; START: update-positions
(defn update-positions [{snake :snake, apple :apple, :as game}]
  (if (eats? snake apple)
    (merge game {:apple (create-apple) :snake (move snake :grow)})
    (merge game {:snake (move snake)})))
; END: update-positions

(defn update-direction [{snake :snake :as game} newdir]
  (merge game {:snake (turn snake newdir)}))

(defn reset-game
  ([]
   (reset-game {}))
  ([game]
   (merge game {:apple (create-apple) :snake (create-snake)})))

(defonce game-state (atom (reset-game {})))

; ----------------------------------------------------------
; gui
; ----------------------------------------------------------
(defn fill-point [pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (ui/translate x y
                  (ui/filled-rectangle color
                                       width height))))

(defmulti paint (fn [object & _] (:type object)))

(defmethod paint :apple [{:keys [location color]}] 
  (fill-point location color))

(defmethod paint :snake [{:keys [body color]}] 
  (vec
   (for [point body]
     (fill-point point color))))

(defn big-button [text]
  (let [lbl (ui/label text
                      (ui/font nil 42))
        body (ui/padding 0 4 14 6
                         lbl)
        [w h] (ui/bounds body)]
    [(ui/with-style
       :membrane.ui/style-stroke
       (ui/rounded-rectangle (+ w 6) h 8))
     (ui/with-color [1 1 1]
      (ui/rounded-rectangle (+ w 6) h 8))
     body]))

(def segment-size 20)
(def left-arrow
  (ui/path [(* 2 segment-size) segment-size]
           [0 segment-size]
           [segment-size 0]
           [0 segment-size]
           [segment-size (* 2 segment-size)]))

(def right-arrow
  (ui/path [0 segment-size]
           [(* 2 segment-size) segment-size]
           [segment-size 0]
           [(* 2 segment-size) segment-size]
           [segment-size (* 2 segment-size)]))

(def up-arrow
  (ui/path [segment-size (* 2 segment-size)]
           [segment-size 0]
           [(* 2 segment-size) segment-size]
           [segment-size 0]
           [0 segment-size]))

(def down-arrow
  (ui/path [segment-size 0]
           [segment-size (* 2 segment-size)]
           [(* 2 segment-size) segment-size]
           [segment-size (* 2 segment-size)]
           [0 segment-size])
  )

(defn game-panel [game]
  (let [snake (game :snake)
        apple (game :apple)
        ]
    (into
     (mapv paint [snake apple])
     [(ui/with-style :membrane.ui/style-stroke
        (ui/rectangle (* (inc width) point-size) 
                      (* (inc height) point-size)))
      (ui/translate 0 475
                    (ui/on
                     :mouse-down
                     (fn [_]
                       (swap! game-state update :running? not)
                       nil)
                     (big-button (if (:running? game)
                                   "stop"
                                   "start"))))
      (ui/translate 0 550
                    (ui/with-style :membrane.ui/style-stroke
                      (apply
                       ui/horizontal-layout
                       (interpose
                        (ui/spacer 40)
                        (for [[arrow dir] [[left-arrow :left]
                                           [up-arrow :up]
                                           [down-arrow :down]
                                           [right-arrow :right]]]
                          (ui/on
                           :mouse-down
                           (fn [_]
                             (swap! log conj dir)
                             (swap! game-state update-direction (dirs dir))
                             nil)
                           arrow))))))])))

(comment
  (defn game-panel [frame game]
    (proxy [JPanel ActionListener KeyListener] []
      (paintComponent [g] 
        (proxy-super paintComponent g)
        (paint g (@game :snake))
        (paint g (@game :apple)))
                                        ; START: swap!
      (actionPerformed [e] 
        (swap! game update-positions)
        (when (lose? (@game :snake))
	  (swap! game reset-game)
	  (JOptionPane/showMessageDialog frame "You lose!"))
                                        ; END: swap!
        (when (win? (@game :snake))
	  (swap! game reset-game)
	  (JOptionPane/showMessageDialog frame "You win!"))
        (.repaint this))
      (keyPressed [e] 
        (swap! game update-direction (dirs (.getKeyCode e))))
      (getPreferredSize [] 
        (Dimension. (* (inc width) point-size) 
		    (* (inc height) point-size)))
      (keyReleased [e])
      (keyTyped [e]))))



(defn step []
  (swap! game-state update-positions)
  (when (lose? (@game-state :snake))
    (swap! game-state reset-game)
    (prn "you lose!")))



(defn repaint! []
  (reset! main-view
          [(ui/wrap-on
            :mouse-down
            (fn [handler p]
              (swap! log conj (mapv int p))
              (handler p))
            [#_(ui/translate 250 20
                           (apply
                            ui/vertical-layout
                            (for [msg (take-last 5 @log)]
                              (ui/label (pr-str msg)))))

             (ui/translate 10 50
                           (game-panel @game-state))])])
  )

(add-watch game-state ::update-view
           (fn [k ref old updated]
             (repaint!)))

(add-watch log ::update-view2
           (fn [k ref old updated]
             (repaint!)))


(swap! game-state identity)

(add-watch game-state ::run-snake
           (fn [k ref old updated]
             (when (and (:running? updated)
                        (not (:running? old)))
               (future
                 (while (:running? @game-state)
                   (step)
                   (sleep turn-millis))))))

