(ns pci4clojure.ch2
  (:use [clojure.set :only [intersection]]))
;;A dictionary of movie critics and their ratings of a small
;;set of movies
(def critics {"Lisa Rose", {"Lady in the Water", 2.5  "Snakes on a Plane", 3.5 
 "Just My Luck", 3.0  "Superman Returns", 3.5  "You  Me and Dupree", 2.5  
 "The Night Listener", 3.0} 
"Gene Seymour", {"Lady in the Water", 3.0  "Snakes on a Plane", 3.5  
 "Just My Luck", 1.5  "Superman Returns", 5.0  "The Night Listener", 3.0  
 "You  Me and Dupree", 3.5}  
"Michael Phillips", {"Lady in the Water", 2.5  "Snakes on a Plane", 3.0 
 "Superman Returns", 3.5  "The Night Listener", 4.0} 
"Claudia Puig", {"Snakes on a Plane", 3.5  "Just My Luck", 3.0 
 "The Night Listener", 4.5  "Superman Returns", 4.0  
 "You  Me and Dupree", 2.5} 
"Mick LaSalle", {"Lady in the Water", 3.0  "Snakes on a Plane", 4.0  
 "Just My Luck", 2.0  "Superman Returns", 3.0  "The Night Listener", 3.0 
 "You  Me and Dupree", 2.0}  
"Jack Matthews", {"Lady in the Water", 3.0  "Snakes on a Plane", 4.0 
 "The Night Listener", 3.0  "Superman Returns", 5.0  "You  Me and Dupree", 3.5} 
"Toby", {"Snakes on a Plane",4.5 "You  Me and Dupree",1.0 "Superman Returns",4.0}})

;;欧几里德距离
(defn sim-distance
  [prefs person1 person2]
  (let [pitem1 (get prefs person1)
        pitem2 (get prefs person2)
        items (intersection (set (keys (get prefs person1))) (set (keys (get prefs person2))))]
    (if (empty? items)
      0
      (/ 1 (+ 1 (Math/sqrt (reduce + 0 (for [item items] (Math/pow (- (get pitem1 item) (get pitem2 item)) 2)))))))))
(println (sim-distance critics "Lisa Rose" "Gene Seymour"))

(defn- sum-by
  [ks fun]
  (reduce + 0 (for [k ks] (fun k))))
  
;;皮尔逊相关
(defn sim-pearson
  [prefs person1 person2]
   (let [pitem1 (get prefs person1)
        pitem2 (get prefs person2)
        items (intersection (set (keys (get prefs person1))) (set (keys (get prefs person2))))
        n (count items)]
    (if (empty? items)
      0
      (let [sum1 (sum-by items #(get pitem1 %))
            sum2 (sum-by items #(get pitem2 %))
            sum1sq (sum-by items #(Math/pow (get pitem1 %) 2))
            sum2sq (sum-by items #(Math/pow (get pitem2 %) 2))
            psum (sum-by items #(* (get pitem1 %) (get pitem2 %)))
            nu (- psum (/ (* sum1 sum2) n))
            den (Math/sqrt (* (- sum1sq (/ (Math/pow sum1 2) n)) (- sum2sq (/ (Math/pow sum2 2) n))))]
      (if (= den 0)
        0
        (/ nu den))))))
            
(println (sim-pearson critics "Lisa Rose" "Gene Seymour"))  

(defn top-matchers
  ([prefs person]
    (top-matchers prefs person 5))
  ([prefs person n]
   (top-matchers prefs person n sim-pearson))
  ([prefs person n sim]
   (let 
     [scores (apply hash-map (flatten (for [other (keys prefs) :when (not (= other person))] (list (sim prefs person other) other))))]
     (take n (reverse (sort scores))))))
(println (top-matchers critics "Toby" 3))



(def myadd (fnil + 0))
(defn get-recommendations
  ([prefs person]
    (get-recommendations prefs person sim-pearson))
  ([prefs person similarity]
    (let [person-prefs (get prefs person)
          others (filter #(> (similarity prefs person %) 0) (filter #(not (= % person)) (keys prefs)))
          simsum (transient (hash-map))
          total (transient (hash-map))]
          (doall (map (fn [other] 
                (let [sim (similarity prefs person other)
                      other-prefs (get prefs other)]
                 (doall
                   (map (fn [item]
                    (let [score (get person-prefs item)]
                       (if (or (not score) (= score 0))
                          (do (assoc! total item (myadd (get total item) (* sim (get other-prefs item))))
                              (assoc! simsum item (myadd (get simsum item) sim)))))) (keys (get prefs other)))))) others))
          (let [psimsum (persistent! simsum)
                ptotal (persistent! total)]
               (reverse (sort (apply hash-map (flatten (for [item (keys ptotal)] (list (/ (get ptotal item) (get psimsum item)) item))))))         
            ))))
;;(println (get-recommendations critics "Toby"))
(println (get-recommendations critics "Toby" sim-distance))
(defn transform
  [prefs]
 (let [new-map (transient (hash-map))]
   (doseq [person (keys prefs)]
     (doseq [[item score] (get prefs person)]
       (let [sub-map (get new-map item)]
         (if sub-map
           (assoc! new-map item (assoc sub-map person score))
           (assoc! new-map item  (assoc (hash-map) person score))))))
  (persistent! new-map)))
  
        
(println (transform critics))
(println (top-matchers (transform critics) "Superman Returns" 10))
