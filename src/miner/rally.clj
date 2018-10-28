(ns miner.rally
  (:require [clojure.string :as str]))


(def init-state {:sides [:a :b :c :d] :server :a :score {:ab 0 :cd 0}})



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


(defn side [state player]
  (let [sides (:sides state)]
    (condp = player
      (sides 0) :right
      (sides 1) :left
      (sides 2) :right
      (sides 3) :left
      nil)))

(defn score [state player-or-team]
  (get-in state [:score (team player-or-team)]))

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
        player-score (score state player)]
    (condp = player
      server (if (even? player-score) :right :left)
      (partner server) (if (odd? player-score) :right :left)
      ;; player on receiving side
      (previous-server server) (if (even? player-score) :right :left)
      (if (odd? player-score) :right :left))))

(defn print-state [state]
  (let [[x y z w] (:sides state)
        ab (score state :ab)
        cd (score state :cd)
        serv (:server state)]
    (println)
    (println "AB=" ab " CD=" cd)
    (println "" (str-player x serv) "  " (str-player y serv))
    (println "----+----")
    (println "" (str-player w serv) "  " (str-player z serv))))



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
    
;; For ANY server, always my score Even = Right, Odd = Left.  Server's partner is opposite.
;;
;; For receiving team, go by previous server with EROL convention.
;;
;; One thing that might help people remember their sides in rally scoring is that the
;; rotation of servers is fixed by the positions at the start of the game.  Let's say
;; the teams are A and B vs C and D. The first server is A and the first receiver is C, on
;; their respective right sides. The service order cycles through A, D, B, and C for the
;; rest of the game. Note, the first receiver is the last server. So you always know the
;; order if you remember how you started. When your side is serving, it’s easy to remember
;; even score = right side, odd = left. The receiving team never switches sides so if you
;; can remember where you were for the previous rally, go there again. If you need
;; confirmation, think about who is serving and who was the previous server. If you were the
;; previous server, you are the “even” player — on the right if your score is even, on the
;; left if your score is odd.
