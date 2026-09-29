(ns hopfield-network.core
  (:import
    [java.io File]
    [javax.imageio ImageIO]

    ))



(def patterns '((1 -1 1 -1 -1 1)
                (1 -1 -1 1 -1 -1)
                (-1 1 -1 -1 -1 1)
                (-1 1 1 1 1 -1)
                ))

(defn normalize-image
  [black-white-matrix]
  (map (fn [row]
         (map (fn [item] (if (= 0 item) -1 1)) row)

         )
       black-white-matrix))




; the new pattern we want to compare
(def S '(1 -1 -1 1 1 -1))

(defn xxT [tensor]
  (map
    (fn [x]
      (map
        (fn [y] (* x y))
        tensor))
    tensor))

;Digonal of matrix
(defn D [matrix]
  (map-indexed
    (fn [i row]
      (map-indexed
        (fn [j column]
          (if (= i j)
            0
            column))
        row))
    matrix)
  )


(defn Add [matrix-a matrix-b]
  (map
    (fn [row-a row-b]
      (map + row-a row-b))
    matrix-a matrix-b)
  )

(defn DotProduct [vector-a vector-b] (reduce + (map * vector-a vector-b)))


;Calculo de W
;Tomas un patrón.
;Calculas su outer product \(xx^T\).
;A esa matriz le pones la diagonal en cero.
;Si tienes varios patrones, repites eso con cada uno y luego sumas las matrices.
;El resultado final es la weight matrix \(W\).
(defn W [patterns]
  (reduce
    Add
    (map
      (fn [pattern]
        (D (xxT pattern)))
      patterns))
  )

; calculo de h
; ejecutas producto punto entre w (pesos calculados) y s el nuevo patron donde w es un tensor 2d y s un tensor 1d
(defn h [w s] (map (fn [w_i] (DotProduct w_i s)) w))

(defn s-prime [h s]
  (map
    (fn [h_i s_i]
      (cond (pos? h_i) 1
            (neg? h_i) -1
            (= h_i 0) s_i
            ))
    h
    s
    ))


(defn recalled-state [w initial-s]
  (loop [current-s initial-s
         pass 0]
    (let [next-s (s-prime (h w current-s) current-s)]
      (cond
        (= current-s next-s)
        {:status :converged
         :state  next-s
         :passes pass}

        (>= pass 100)
        {:status :not-converged
         :state  current-s
         :passes pass}

        :else
        (recur next-s (inc pass))))))



(defn -main [& args]
  (let [predicted-s (recalled-state (W patterns) S)]
    (println "Input pattern:")
    (println S)
    (println "Recalled pattern:")
    (println predicted-s)

    )
  )