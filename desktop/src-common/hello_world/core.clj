(ns hello-world.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]))

(declare title-screen main-screen)

(def ^:const viewport-width 5)
(def ^:const viewport-height 5)
(def ^:const speed 14)
(def ^:const pix-map-width 32)
(def ^:const pix-map-height 32)
(def ^:const num-sprites 5)


(defscreen blank-screen
  :on-render
  (fn [screen entities]
    (clear!)))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    (assoc (label "0" (color :white))
           :id :fps
           :x 5))

  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :fps (doto entity (label! :set-text (str (game :fps))))
             entity))
         (render! screen)))

  :on-resize
  (fn [screen entities]
    (height! screen 300)))

(defgame hello-world-game
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))

(set-screen-wrapper! (fn [screen screen-fn]
                       (try (screen-fn)
                         (catch Exception e
                           (.printStackTrace e)
                           (set-screen! hello-world-game blank-screen)))))

(defn move
  [entity direction]
  (case direction
    :down (assoc entity :y (- (:y entity) speed))
    :up (assoc entity :y (+ (:y entity) speed))
    :right (assoc entity :x (+ (:x entity) speed))
    :left (assoc entity :x (- (:x entity) speed))
    nil))

(defn make-sprite []
  (let [pm (doto (->> :r-g-b-a8888 pixmap-format
                      (pixmap* pix-map-width pix-map-height))
             (.setColor 1 0 0 0.5) .fill (.setColor 1 1 0 1)
             (.drawLine 0 0 pix-map-width pix-map-height)
             (.drawLine pix-map-width 0 0 pix-map-height) (.setColor 0 1 1 1)
             (.drawRectangle 0 0 pix-map-width pix-map-height)
             )
        s (doto (-> pm texture :object com.badlogic.gdx.graphics.g2d.Sprite.)
            (.setSize 1 1))
        sw (.getWidth s), sh (.getHeight s)
        s (doto s (.setOrigin (/ sw 2.0) (/ sh 2.0))
                (.setPosition (- (rand 4) 2) (- (rand 4) 2)))
        ;;t (assoc (texture s) :x 700 :y 500)
        t (texture s)
        ]
    (println (texture! t :get-region-width) " : " sw)
    (println "x: " (:x t))
    (println "sx: " (.getX (:object t)))
    t
    )
  )

(defn make-sprites
  "make some sprite entities"
  []
  (let [sprites (repeatedly num-sprites make-sprite)]
    (println (first sprites))
    sprites))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (app! :set-log-level com.badlogic.gdx.Application/LOG_DEBUG)
    (add-timer! screen :spawn-enemy 10 2 19)
    (update! screen :renderer (stage) :camera (orthographic) :paused? false)
    (conj (make-sprites)
          (assoc (texture "drinnon.jpg")
                   :x 100 :y 100 :width 300 :height 300
                   :angle 45 :origin-x 0 :origin-y 0)))
  
  :on-timer
  (fn [screen entities]
    )

  :on-resize
  (fn [screen entities]
    (height! screen 600))

  :on-render
  (fn [screen entities]
    #_(println screen)
    (when-not (:paused? screen) (graphics! :get-delta-time))
    (clear! (/ 0x64 255.0) (/ 0x95 255.0) (/ 0xed 255.0) (/ 0xff 255.0))
    (render! screen entities))

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :dpad-up))
      (move (first entities) :up)
      (= (:key screen) (key-code :dpad-down))
      (move (first entities) :down)
      (= (:key screen) (key-code :dpad-right))
      (move (first entities) :right)
      (= (:key screen) (key-code :dpad-left))
      (move (first entities) :left)
      (= (:key screen) (key-code :r))
      (on-gl (set-screen! hello-world-game main-screen text-screen))))

  :on-touch-down
  (fn [screen entities]
    (cond
      (> (game :y) (* (game :height) (/ 2 3)))
      (move (first entities) :up)
      (< (game :y) (/ (game :height) 3))
      (move (first entities) :down)
      (> (game :x) (* (game :width) (/ 2 3)))
      (move (first entities) :right)
      (< (game :x) (/ (game :width) 3))
      (move (first entities) :left)))

  :on-pause
  (fn [screen entities]
    (println ":on pause!")
    (let [state (first entities)]
      (update! screen :paused? true)
      entities))

  :on-resume
  (fn [screen entities]
    (println ":on resume!")
    (let [state (first entities)]
      (update! screen :paused? false)
      entities)))

(comment
  
  )
