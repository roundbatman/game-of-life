(ns game-of-life.gol
  (:gen-class))

(def dead 0)
(def alive 1)

(defn init-cell []
  (rand-int 2))

(defn next-state
  [cell neighbours]
  (case neighbours
    0 dead
    1 dead
    2 cell
    3 alive
    dead))

(defn get-at [board x y]
  (try
    (nth (nth board y) x)
    (catch IndexOutOfBoundsException e 0)))

(defn get-neighbour-values [board x y]
  (+
   (get-at board (- x 1) (- y 1))
   (get-at board x (- y 1))
   (get-at board (+ x 1) (- y 1))
   (get-at board (- x 1) y)
   (get-at board (+ x 1) y)
   (get-at board (- x 1) (+ y 1))
   (get-at board x (+ y 1))
   (get-at board (+ x 1) (+ y 1))))

(defn next-cell-value [board x y]
  (next-state (get-at board x y)
              (get-neighbour-values board x y)))

(defn update-board [board]
  (let [height (count board)
        width (count (nth board 0))]
    (loop [result (transient [])
           y 0]
      (if (= y height)
        (persistent! result)
        (recur (conj! result
                      (map (fn [x] (next-cell-value board x y)) (vec (range 0 width))))
               (inc y))))))

(defn create-board
  [width height]
  (loop [result (transient [])
         y 0]
    (if (<= height y)
      (persistent! result)
      (recur (conj! result (vec (repeatedly width #(init-cell))))
             (inc y)))))

(defn still-alive? [board]
  (< 0 (reduce + (flatten board))))

(defn print-board
  [board]
  (doseq [row board]
    (doseq [cell row] (print (if (= cell alive) "*" " ") " "))
    (println)))

(defn board-iter [board tics]
  (print-board board)
  (Thread/sleep 1000)
  (if (or (not (still-alive? board)) (= tics 0))
   tics
    (recur (update-board board) (dec tics))))

(defn -main []
  (board-iter (create-board 20 20) 50))
