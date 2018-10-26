(ns miner.rally
  (:require [clojure.string :as str]))


(def init-state {:sides [:a :b :c :d] :server :a :score {:ab 0 :cd 0}})


;; not used
(defn side [state player]
  (let [sides (:sides state)]
    (condp = player
      (sides 0) :right
      (sides 1) :left
      (sides 2) :right
      (sides 3) :left
      nil)))

(defn partner [player]
  (get {:a :b :b :a :c :d :d :c} player))

(defn team [player]
  (get {:a :ab :b :ab :ab :ab :c :cd :d :cd :cd :cd} player))

;; service order [:a :d :b :c]
(defn next-server [player]
  (get {:a :d :d :b :b :c :c :a} player))

(defn previous-server [player]
  (get {:a :c :d :a :b :d :c :b} player))


(defn other-side [side]
  (if (= side :right) :left :right))


(defn swap-sides [sides team]
  (let [[x y z w] sides]
    (if (= team :ab)
      [y x z w]
      [x y w z])))

(defn add-point [state winner]
  (let [wteam (team winner)
        state2 (update-in state [:score wteam] inc)]
    (if (= wteam (team (:server state)))
      ;; server won
      (update state2 :sides swap-sides wteam)
      (update state2 :server next-server))))


(defn player-name  [p] (str/upper-case (name p)))

(defn server-star [p server]
  (if (= p server) "*" " "))

(defn str-player [p serv]
  (str (player-name p) (server-star p serv)))

(defn server [state]
  (:server state))

(defn receiver [state]
  (let [[x y z w] (:sides state)]
    (condp = (:server state)
      x z
      y w
      z x
      w y)))

(defn calc-side [state player]
  (let [server (:server state)
        player-score (get-in state [:score (team player)])]
    (condp = player
      server (if (even? player-score) :right :left)
      (partner server) (if (odd? player-score) :right :left)
      ;; player on receiving side
      (previous-server server) (if (even? player-score) :right :left)
      (if (odd? player-score) :right :left))))

(defn print-state [state]
  (let [[x y z w] (:sides state)
        ab (get-in state [:score :ab])
        cd (get-in state [:score :cd])
        serv (:server state)]
    (println)
    (println "AB=" ab " CD=" cd)
    #_ (println (case serv
               :a "A serv,"
               :b "p serv,"
               (:c :d) (if (= :a (receiver state))
                         "A rec ,"
                         "p rec ,"))
             "A side/oe" (if (= (get-in state [:sides 0]) :a) "Right/" "Left /")
             (if (odd? (get-in state [:score :ab])) "Odd ," "Even,")
             "Total" (if (odd? (+ (get-in state [:score :ab]) (get-in state [:score :cd])))
                       "Odd"
                       "Even"))


    (do
    (println "" (str-player x serv) "  " (str-player y serv))
    (println "----+----")
    (println "" (str-player w serv) "  " (str-player z serv)))))



(defn assert-state [state]
  (doseq [p [:a :b :c :d]]
    (assert (= (side state p) (calc-side state p))))
  true)

;; run-sim will run random winners unless overridden with explicit `winner-coll` which will
;; cycle winners as necessary to reach `N` rallies.
(defn run-sim
  ([] (run-sim 100))
  ([n] (run-sim n (repeatedly n #(rand-nth [:a :b :c :d]))))
  ([n winner-coll]
   (doseq [state (reductions add-point init-state (take n (cycle winner-coll)))]
    (assert-state state)
    (print-state state))))
    



;; Haven't figured out pattern relative to Total score.


;; For ANY server, always my score Odd = Left, Even = Right.  Server's partner is opposite.
;;
;; For ANY receiver, by opponent's score OpEven = Right , OpOdd = Left
;;
;; Conjecture:  For A Receiving, if A was last server, then Odd=Left, Right=Even, but if
;; partner B was last server, then opposite.  Even/Right when A or D serving.  Even/Left
;; when C or B serving.



;; A serving, Score Odd = Left, Even = Right
;; B serving, Score Odd = Left, Even = Right
;;     When B serving, A position is Score Odd = Right, Even = Left (opposite of server)

;; probably same as above for C/A B/D

;; looks like A receiving is the same Odd=Left, Even=Right

;; A rec  Right if Odd/Odd or Even/Even;  Left if Odd/Even or Even/Odd
;;   which is really if the opponents are OpEven=Right, OpOdd=Left
;;
;; B rec (swap A)  OpEven=Left, OpOdd=Right




;; One thing that might help people remember their sides in rally scoring is that the
;; rotation of servers is fixed by the positions at the start of the game. In the diagram,
;; the teams are A and B vs C and D. The first server is A and the first receiver is C, on
;; their respective right sides. The service order cycles through A, D, B, and C for the
;; rest of the game. Note, the first receiver is the last server. So you always know the
;; order if you remember how you started. When your side is serving, it’s easy to remember
;; even score = right side, odd = left. The receiving team never switches sides so if you
;; can remember where you were for the previous rally, go there again. If you need
;; confirmation, think about who is serving and who was the previous server. If you were the
;; previous server, you are the “even” player — on the right if your score is even, on the
;; left if your score is odd.
