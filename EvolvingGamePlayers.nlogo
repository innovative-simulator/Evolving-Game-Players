;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evolving Game Players: Evolutionary Games & Population Dynamics
;; This program (C) Christopher J Watts, 2021.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [rnd]

globals [
  min-payoff
  max-payoff

  sorted-patches
  sorted-populations

  num-pops-with-group1-dom
  num-pops-with-group2-dom
  num-pops-with-groups-equal

]

breed [arrows arrow]
breed [texts text]
breed [populations population]
breed [players player]

populations-own [
  x
  y
  po-players
  po-group1
  po-group2
]

players-own [
  pl-population
  pl-group
  pl-strategy
  pl-action
  pl-fitness
  pl-memory
  pl-other-interactions ; Number of interactions with members of other groups.
  pl-belief ; Number of "1" actions entered into memory (but doesn't forget).
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  set sorted-patches sort patches
  foreach sorted-patches [pa ->
    ask pa [
      set pcolor white
      set plabel-color black
    ]
  ]
  draw-axes

  set min-payoff min map [A -> min payoffs (first A) (last A)] (list [0 0] [0 1] [1 0] [1 1]) ; Used to remove negative payoffs, and for scale-color.
  set max-payoff max map [A -> max payoffs (first A) (last A)] (list [0 0] [0 1] [1 0] [1 1]) ; Used for scale-color.

  reset-ticks
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Payoff table cells

to-report payoffs [a-move b-move]
  if game = "Hawk-Dove" [report payoffs-hawk-dove a-move b-move]
  if game = "Mutualism" [report payoffs-mutualism a-move b-move]
  if game = "Prisoner's Dilemma" [report payoffs-Prisoners-Dilemma a-move b-move]
  if game = "Donation" [report payoffs-donation a-move b-move]
  report false
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report Payoffs-Mutualism [A-Move B-Move]
  report item (A-Move * 2 + B-Move) (list
    (list 0 0) ; Selfish-Selfish
    (list 2 1) ; ; Selfish-Genererous
    (list 1 2) ; Genererous-Selfish
    (list k k) ; Genererous-Genererous
    )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report Payoffs-Hawk-Dove [A-Move B-Move]
  report item (A-Move * 2 + B-Move) (list
    (list (value / 2) (value / 2)) ; Dove-Dove
    (list 0 value) ; ; Dove-Hawk
    (list value 0) ; Hawk-Dove
    (list ((value - cost) / 2) ((value - cost) / 2)) ; Hawk-Hawk
    )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report Payoffs-Prisoners-Dilemma [A-Move B-Move]
  report item (A-Move * 2 + B-Move) (list
    (list reward reward) ; Cooperate-Cooperate
    (list sucker temptation) ; ; C-D
    (list temptation sucker) ; D-C
    (list punishment punishment) ; Defect-Defect
    )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report Payoffs-Donation [A-Move B-Move]
  ; Donation Game is a special case of the Prisoner's Dilemma.
  ; We parameterise it with Value ("Benefit") and Cost, like Hawk-Dove.

  report item (A-Move * 2 + B-Move) (list
    (list (value - cost) (value - cost)) ; Cooperate (donate)-Cooperate
    (list (0 - cost) value) ; ; C-D
    (list value (0 - cost)) ; D-C
    (list 0 0) ; Defect (don't donate)-Defect
    )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report cost
  report value * 100 / Value-As-Perc-Of-Cost
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report made-neat [given-list]
  ; Don't display all the decimal places
  report map [a -> precision a 2] given-list
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report payoff-for-A-move-given-perc [A-move given-prop]
  report ((given-prop * first Payoffs A-move 1) +
    ((1.0 - given-prop) * first Payoffs A-move 0))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report payoff-for-B-move-given-perc [B-move given-prop]
  report ((given-prop * last Payoffs 1 B-move) +
    ((1.0 - given-prop) * last Payoffs 0 B-move))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visuals

to draw-axes
  ask arrows [die]

  ; X-axis
  create-arrows 1 [
    set shape "default"
    set color black
    set pen-size 2
    set heading 90
    setxy -1 0
    pen-down
    setxy max-pxcor 0
  ]

  ; Y-axis
  create-arrows 1 [
    set shape "default"
    set color black
    set pen-size 2
    set heading 0
    setxy 0 -1
    pen-down
    setxy 0 max-pycor
  ]

  ; Diagonal
  create-arrows 1 [
    set color black
    set pen-size 2
    set heading 45
    setxy 0 0
    foreach (range 2 100 2) [a100 ->
      setxy (x-max * a100 / 100) (y-max * a100 / 100)
      ifelse 0 = a100 mod 4 [pen-down] [pen-up]
    ]
    die
  ]

  ; Labels
  create-texts 1 [
    set size 0
    set color black
    set label-color black
    set label "1"
    set pen-size 2
    setxy x-max 0
    pen-down
    setxy x-max -0.25
    pen-up
    setxy x-max -1
  ]
  create-texts 1 [
    set size 0
    set color black
    set label-color black
    set label "0.5"
    set pen-size 2
    setxy (0.5 * x-max) 0
    pen-down
    setxy (0.5 * x-max) -0.25
    pen-up
    setxy (0.5 * x-max) -1
  ]
  create-texts 1 [
    set size 0
    set color black
    set label-color black
    set label "1"
    set pen-size 2
    setxy 0 y-max
    pen-down
    setxy -0.25 y-max
    pen-up
    setxy -0.25 y-max
  ]
  create-texts 1 [
    set size 0
    set color black
    set label-color black
    set label "0.5"
    set pen-size 2
    setxy 0 (0.5 * y-max)
    pen-down
    setxy -0.25 (0.5 * y-max)
    pen-up
    setxy -0.25 (0.5 * y-max)
  ]
  create-texts 1 [
    set size 0
    set color black
    set label-color black
    set label "O"
    setxy -0.5 -1
  ]
  create-texts 1 [
    set size 0
    set label-color black
    set label "y = x"
    setxy (x-max + 0.5) (y-max)
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report mouse-payoffs
  if not mouse-inside? [report ""]
  let p1 mouse-xcor / x-max
  let p2 mouse-ycor / y-max
  report payoffs-xy p1 p2
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report payoffs-xy [p1 p2]
  report (list
    (precision ((p1 * payoff-for-A-move-given-perc 1 p2) + (1 - p1) * payoff-for-A-move-given-perc 0 p2) 3)
    (precision ((p2 * payoff-for-B-move-given-perc 1 p1) + (1 - p2) * payoff-for-B-move-given-perc 0 p1) 3)
    )
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Color-Patches-By-Payoff1
  foreach sorted-patches [pa ->
    ask pa [
      if (pxcor >= 0 and pycor >= 0 and pxcor <= x-max and pycor <= y-max) [
        set pcolor scale-color blue
         (first payoffs-xy (pxcor / x-max) (pycor / y-max))
         min-payoff max-payoff
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Color-Patches-By-Payoff2
  foreach sorted-patches [pa ->
    ask pa [
      if (pxcor >= 0 and pycor >= 0 and pxcor <= x-max and pycor <= y-max) [
        set pcolor scale-color red
         (last payoffs-xy (pxcor / x-max) (pycor / y-max))
         min-payoff max-payoff
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report x-max
  report max-pxcor - 2
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report y-max
  report max-pycor - 2
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replicator dynamics

to Replicator-Dynamics-By-ABM
  setup

  foreach (range 0 100 10) [x100 ->
    foreach (range 0 100 10) [y100 ->
      create-populations 1 [
        set shape "default"
        ;set color yellow
        setxy (x-max * x100 / 100) (y-max * y100 / 100)
        set x x100 / 100
        set y y100 / 100
        setxy (x-max * x) (y-max * y)
        facexy (x-max * next-x) (y-max * next-y)
        if Population-Pen-Down? [pen-down]

        ; Create Population's players
        set po-players []
        set po-group1 []
        set po-group2 []
        let num-red round (population-size * perc-group2 / 100)
        ; 4 sub-populations (2 groups * 2 strategies)
        (foreach (list
          (round ((population-size - num-red) * x))
          ((population-size - num-red) - (round ((population-size - num-red) * x)))
          (round (num-red * y))
          (num-red - (round (num-red * y)))
          )
          [ 1 1 2 2 ]
          [ 1 0 1 0 ]
          [[n g s] ->
            hatch-players n [
              set hidden? true
              set color item g (list grey red blue)
              set pl-population myself
              set pl-group g
              ask pl-population [
                set po-players fput myself po-players
                ifelse g = 1 [
                  set po-group1 fput myself po-group1
                ]
                [
                  set po-group2 fput myself po-group2
                ]
              ]
              set pl-strategy strategy-reporter
              set pl-action s

            ]
        ]
        )
        foreach po-players [pl ->
          ask pl [setup-initial-memory]
        ]
        ; Actual population might be slightly different from ideal.
        set x mean map [a -> [pl-action] of a] po-group1
        set y mean map [a -> [pl-action] of a] po-group2
        setxy (x-max * x) (y-max * y)
        facexy (x-max * next-x) (y-max * next-y)
      ]
    ]
  ]

  set sorted-populations sort populations

;  recolor-pops-by-players
  reset-ticks
  reposition-pops
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-initial-memory
  ; Run as player.
  set pl-memory []
  ; (Actually, we're currently using belief instead of memory.)
  if memory-initialization = "Empty" [
    set pl-belief 0
    set pl-other-interactions 0
    stop
  ]
  set pl-other-interactions ifelse-value (pl-group = 1) [Memory-Initial-Weight-1] [Memory-Initial-Weight-2]
  let prob 0.0
  if memory-initialization = "Fixed-Proportion-50:50" [
    set prob 0.5
    set pl-belief round (prob * pl-other-interactions)
    stop
  ]
  if memory-initialization = "Fixed-Proportion-MSNE" [
    set prob msne
    set pl-belief round (prob * pl-other-interactions)
    stop
  ]
  if memory-initialization = "Fixed-Proportion-Action" [
    set prob mean map [pl -> [pl-action] of pl] ifelse-value (pl-group = 1) [[po-group2] of pl-population] [[po-group1] of pl-population]
    set pl-belief round (prob * pl-other-interactions)
    stop
  ]
  if memory-initialization = "Random-50:50" [
    set prob 0.5
  ]
  if memory-initialization = "Random-MSNE" [
    set prob msne
  ]
  if memory-initialization = "Random-Action" [
    set prob mean map [pl -> [pl-action] of pl] ifelse-value (pl-group = 1) [[po-group2] of pl-population] [[po-group1] of pl-population]
  ]
  set pl-belief sum n-values pl-other-interactions [ifelse-value (prob > random-float 1) [1] [0]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to play-game
  foreach sorted-populations [po ->
    ask po [
      foreach shuffle po-players [ego ->
        ask ego [
          set pl-fitness 0
          let alter nobody
          let selected-item 0
          let opponents []
          ifelse Opponents-Include-Own-Group? [
            set opponents [po-players] of pl-population
          ]
          [
            ifelse pl-group = 1 [
              set opponents [po-group2] of pl-population
            ]
            [
              set opponents [po-group1] of pl-population
            ]
          ]
          let ego-action 0
          let alter-action 0
          let ego-payoff 0.0
          repeat 1 [
            set alter one-of opponents
            while [alter = ego] [set alter one-of opponents]
            set ego-action (runresult pl-strategy alter)
            set alter-action [(runresult pl-strategy ego)] of alter
            if 0 < playing-noise [
              if playing-noise > random-float 100 [set ego-action 1 - ego-action]
              if playing-noise > random-float 100 [set alter-action 1 - alter-action]
            ]
            set ego-payoff first payoffs ego-action alter-action
            set pl-fitness pl-fitness + ego-payoff ; For evolutionary dynamics
            set pl-action ego-action ; Should we record the pre-noise version instead?
            if pl-group != [pl-group] of alter [
              ; Only remembering interactions with other groups.
              ;set pl-memory fput (list alter-action ego-action ego-payoff alter) sublist pl-memory 0 (min (list (length pl-memory) memory-length)) ; For cultural models
              ; If using "unlimited" memory, then storing in pl-memory is a waste. Better to just keep a count.
              set pl-belief pl-belief + alter-action ; NB: Updating beliefs during the round. Ego could now become an opponent in this same round, and base actions on this updated belief.
              set pl-other-interactions pl-other-interactions + 1
            ]
            ; Doesn't alter get any payoff from this???
          ]
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report strategy-reporter
  if playing-strategy = "Last Action" [report [a -> pl-action]]
  if playing-strategy = "Amadae" [report [a -> playing-amadae a]]
  if playing-strategy = "Amadae2" [report [a -> playing-amadae2 a]]

  if playing-strategy = "MSNE" [report [a -> playing-msne]]
  if playing-strategy = "Memory" [report [a -> playing-memory]]
  if playing-strategy = "Stochastic Memory" [report [a -> playing-memory-stochastic]]

  report [-> pl-action]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report playing-amadae [given-alter]
  if pl-group = [pl-group] of given-alter [report playing-msne]
  report playing-memory
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report playing-amadae2 [given-alter]
  if pl-group = [pl-group] of given-alter [report playing-msne]
  report playing-memory-stochastic
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report playing-msne
  ; Amadae's players play a "Nash Mixed Strategy Equilibrium" (NMSE)
  ; She means a Mixed Strategy Nash Equilibrium (MSNE)!
  let prob MSNE
  if prob > 1 [report 1]
  report ifelse-value (prob > random-float 1) [1] [0]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report MSNE
  ; If their opponent plays this mixed strategy,
  ; (i.e. a probabilistic strategy),
  ; then player should be indifferent between actions.
  ; pi(1,1)*p + pi(1,0)*(1-p) = pi(0,1)*p + pi(0,0)*(1-p)
  ; p = ( p(0,0) - pi(1,0) ) / ( pi(1,1) - pi(1,0) - pi(0,1) + pi(0,0) )
  report ( (first payoffs 0 0) - (first payoffs 1 0) ) / ( (first payoffs 1 1) - (first payoffs 1 0) - (first payoffs 0 1) + (first payoffs 0 0) )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report MSNE-Payoff
  let prob msne
  report (prob * first payoffs 1 1) + ((1 - prob) * first payoffs 1 0)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report playing-memory
  if pl-other-interactions = 0 [report playing-msne]
  ;if empty? pl-memory [report playing-msne]
  ;let prob (length filter [h -> 1 = first h] pl-memory) / length pl-memory
  let prob pl-belief / pl-other-interactions
  let a (prob * first payoffs 0 1) + ((1 - prob) * first payoffs 0 0)
  let b (prob * first payoffs 1 1) + ((1 - prob) * first payoffs 1 0)
  if a > b [report 0]
  if b > a [report 1]
  report random 2
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report playing-memory-stochastic
  if pl-other-interactions = 0 [report playing-msne]
  let p pl-belief / pl-other-interactions
  let a (p * first payoffs 0 1) + ((1 - p) * first payoffs 0 0)
  let b (p * first payoffs 1 1) + ((1 - p) * first payoffs 1 0)

  if a > random-float (a + b) [report 1]
  report 0
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to replicate
  let fitness-corrective 0 - min (list 0 min-payoff) ; rnd:weighted cannot use negative payoffs.
  let m (delta * speed-1 / 100)
  let n (delta * speed-2 / 100)
  foreach sorted-populations [po ->
    ask po [
      let num-updating-1 round (m * length po-group1)
      let num-updating-2 round (n * length po-group2)
      let sampled-strategies1 map [af -> [pl-action] of last af] rnd:weighted-n-of-list-with-repeats num-updating-1 (map [a -> (list ([pl-fitness + fitness-corrective] of a) a)] po-group1) [af -> first af]
      let sampled-strategies2 map [af -> [pl-action] of last af] rnd:weighted-n-of-list-with-repeats num-updating-2 (map [a -> (list ([pl-fitness + fitness-corrective] of a) a)] po-group2) [af -> first af]
      (foreach (n-of num-updating-1 po-group1) sampled-strategies1 [[pl strat] ->
        ask pl [set pl-action ifelse-value (replication-noise > random-float 100) [1 - strat] [strat]]
      ])
      (foreach (n-of num-updating-2 po-group2) sampled-strategies2 [[pl strat] ->
        ask pl [set pl-action ifelse-value (replication-noise > random-float 100) [1 - strat] [strat]]
      ])
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to re-position-pops-by-actions
  foreach sorted-populations [po ->
    ask po [
      set x mean map [a -> [pl-action] of a] po-group1 ; x is G1's mean action.
      set y mean map [a -> [pl-action] of a] po-group2 ; y is G2's mean action.

      setxy (x-max * x) (y-max * y)
      facexy (x-max * next-x) (y-max * next-y)
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to re-position-pops-by-beliefs
  foreach sorted-populations [po ->
    ask po [
      set y mean map [a -> [ifelse-value (pl-other-interactions = 0) [msne] [pl-belief / pl-other-interactions]] of a] po-group1 ; y is G1's belief about G2.
      set x mean map [a -> [ifelse-value (pl-other-interactions = 0) [msne] [pl-belief / pl-other-interactions]] of a] po-group2 ; x is G2's belief about G1.

      setxy (x-max * x) (y-max * y)
      facexy (x-max * next-x) (y-max * next-y)
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reposition-pops
  if Reposition-Populations = "By Mean Belief" [re-position-pops-by-beliefs]
  if Reposition-Populations = "By Mean Action" [re-position-pops-by-actions]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go-sim
  if ticks = rounds [stop]
  play-game
  if Replicate? [replicate]
  reposition-pops

  set num-pops-with-group1-dom count populations with [x > y]
  set num-pops-with-group2-dom count populations with [x < y]
  set num-pops-with-groups-equal count populations with [x = y]

  if Recolor-Populations? [recolor-pops-by-players]
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-pops-by-players
  foreach sorted-populations [po ->
    ask po [
      ; Assuming pl-fitness is payoff from just one round.
      set color scale-color green (mean map [a -> [pl-fitness] of a] po-players) (5 + max-payoff) (-5 + min-payoff)
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Replicator-Dynamics-By-Equation
  setup

  foreach (range 10 100 10) [x100 ->
    foreach (range 10 100 10) [y100 ->
      create-populations 1 [
        set shape "default"
        ;set color yellow
        setxy (x-max * x100 / 100) (y-max * y100 / 100)
        set x x100 / 100
        set y y100 / 100
        let x-dot rd-x-dot
        let y-dot rd-y-dot
        facexy (xcor + x-max * x-dot) (ycor + y-max * y-dot)
        facexy (x-max * next-x) (y-max * next-y)
        pen-down
;        if Population-Pen-Down? [pen-down]
      ]
    ]
  ]

  set sorted-populations sort populations

  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report next-x
  let m speed-1 / 100
  if not Opponents-Include-Own-Group? [
;    report (1 - m * delta) * x +
;    (m * delta * x * payoff-for-A-move-given-perc 1 y ) /
;    ( (x * payoff-for-A-move-given-perc 1 y ) + ((1 - x) * payoff-for-A-move-given-perc 0 y))

    ; From Bergstrom & Lachmann (2003, Appendix)
    ; x_t+1 = ( (1 - m * delta) * x_t + m * delta * x_t * pi(U,y_t) ) / ( (1 - m * delta) + m * delta * (x_t * pi(U,y_t) + (1 - x_t) * pi(D,y_t)) )
    report ( (1 - m * delta) * x + m * delta * x * payoff-for-A-move-given-perc 1 y ) /
    ( (1 - m * delta) + m * delta * ((x * payoff-for-A-move-given-perc 1 y) + ((1 - x) * payoff-for-A-move-given-perc 0 y)) )
  ]
  ; Needs checking?
  let z ((perc-group2 * y) + ((100 - perc-group2) * x)) / 100
  report ( (1 - m * delta) * x + m * delta * x * payoff-for-A-move-given-perc 1 z ) /
  ( (1 - m * delta) + m * delta * ((x * payoff-for-A-move-given-perc 1 z) + ((1 - x) * payoff-for-A-move-given-perc 0 z)) )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report next-y
  let n speed-2 / 100
  if not Opponents-Include-Own-Group? [
;    report (1 - n * delta) * y +
;    (n * delta * y * payoff-for-B-move-given-perc 1 x ) /
;    ( (y * payoff-for-B-move-given-perc 1 x ) + ((1 - y) * payoff-for-B-move-given-perc 0 x))

    ; From Bergstrom & Lachmann (2003, Appendix)
    ; y_t+1 = ( (1 - n * delta) * y_t + n * delta * y_t * pi(L,x_t) ) / ( (1 - n * delta) + n * delta * (y_t * pi(L,x_t) + (1 - y_t) * pi(R,x_t)) )
    report ( (1 - n * delta) * y + n * delta * y * payoff-for-B-move-given-perc 1 x ) /
    ( (1 - n * delta) + n * delta * ((y * payoff-for-B-move-given-perc 1 x) + ((1 - y) * payoff-for-B-move-given-perc 0 x)) )
  ]
  ; Needs checking?
  let z ((perc-group2 * y) + ((100 - perc-group2) * x)) / 100
  report ( (1 - n * delta) * y + n * delta * y * payoff-for-B-move-given-perc 1 z ) /
  ( (1 - n * delta) + n * delta * ((y * payoff-for-B-move-given-perc 1 z) + ((1 - y) * payoff-for-B-move-given-perc 0 z)) )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report rd-x-dot
  let m speed-1
  if not Opponents-Include-Own-Group? [
    ; x-dot = m * x * ( pi(U y) - [x * pi(U y) + (1-x) * pi(D y)]) ; From Bergstrom & Lachmann (2003, Appendix)
    report m * x * ( (payoff-for-A-move-given-perc 1 y) - (x * (payoff-for-A-move-given-perc 1 y) + (1 - x) * (payoff-for-A-move-given-perc 0 y)) )
  ]
  ; Needs checking?
  let z ((perc-group2 * y) + ((100 - perc-group2) * x)) / 100
  report m * x * ( (payoff-for-A-move-given-perc 1 z) - (x * (payoff-for-A-move-given-perc 1 z) + (1 - x) * (payoff-for-A-move-given-perc 0 z)) )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report rd-y-dot
  let n speed-2
  if not Opponents-Include-Own-Group? [
    ; y-dot = n * y * ( pi(L x) - [y * pi(L x) + (1-y) * pi(R x)]) ; From Bergstrom & Lachmann (2003, Appendix)
    report n * y * ( (payoff-for-B-move-given-perc 1 x) - (y * (payoff-for-B-move-given-perc 1 x) + (1 - y) * (payoff-for-B-move-given-perc 0 x)) )
  ]
  ; Needs checking?
  let z ((perc-group2 * y) + ((100 - perc-group2) * x)) / 100
  report n * y * ( (payoff-for-B-move-given-perc 1 z) - (y * (payoff-for-B-move-given-perc 1 z) + (1 - y) * (payoff-for-B-move-given-perc 0 z)) )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to rd-move-delta [given-delta]
  ; Run by population
  set x next-x
  set y next-y
  ifelse (x > 1.0 or y > 1.0 or x < 0.0 or y < 0.0) [ ; Point outside world
    facexy (x-max * next-x) (y-max * next-y)
    while [(xcor >= 0 and xcor <= x-max) and (ycor >= 0 and ycor <= y-max) and nobody != patch-ahead delta] [fd delta]
;    while [nobody != patch-ahead delta] [fd delta]
    set hidden? true
  ]
  [ ; Point inside world
    set hidden? false
    setxy (x-max * x) (y-max * y)
    facexy (x-max * next-x) (y-max * next-y)
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go-eqn
  if ticks = rounds [stop]
  foreach sorted-populations [po ->
    ask po [rd-move-delta delta]
  ]
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@#$#@#$#@
GRAPHICS-WINDOW
245
10
637
403
-1
-1
16.0
1
12
1
1
1
0
0
0
1
-1
22
-1
22
1
1
1
ticks
30.0

INPUTBOX
10
125
162
185
Population-Size
200.0
1
0
Number

SLIDER
10
190
182
223
Perc-Group2
Perc-Group2
0
100
90.0
5
1
%
HORIZONTAL

INPUTBOX
10
565
70
625
Value
10.0
1
0
Number

MONITOR
170
565
305
610
D-D: [V/2 V/2]
made-neat Payoffs-Hawk-Dove 0 0
17
1
11

MONITOR
310
565
445
610
D-H: [0 V]
made-neat Payoffs-Hawk-Dove 0 1
17
1
11

MONITOR
170
615
305
660
H-D: [V 0]
made-neat Payoffs-Hawk-Dove 1 0
17
1
11

MONITOR
310
615
445
660
H-H: [(V-C)/2 (V-C)/2]
made-neat Payoffs-Hawk-Dove 1 1
17
1
11

TEXTBOX
15
505
170
530
Hawk-Dove Payoffs:
16
0.0
1

MONITOR
75
615
155
660
Value / Cost
value / cost
5
1
11

SLIDER
10
530
202
563
Value-As-Perc-Of-Cost
Value-As-Perc-Of-Cost
0
200
90.0
10
1
%
HORIZONTAL

MONITOR
75
565
155
610
NIL
Cost
3
1
11

MONITOR
100
230
187
275
Group2 (Red)
Perc-group2 * population-size / 100
1
1
11

MONITOR
10
230
97
275
Group1 (Blue)
(100 - Perc-Group2) * population-size / 100
1
1
11

BUTTON
650
240
862
273
NIL
Replicator-Dynamics-By-Equation
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
820
140
900
200
Delta
0.1
1
0
Number

CHOOSER
10
325
157
370
Game
Game
"Hawk-Dove" "Prisoner's Dilemma" "Mutualism" "Donation"
0

SLIDER
475
535
647
568
k
k
0
5
1.5
.25
1
NIL
HORIZONTAL

MONITOR
475
575
532
620
S-S
made-neat Payoffs-mutualism 0 0
2
1
11

MONITOR
535
575
592
620
S-G
made-neat Payoffs-mutualism 0 1
2
1
11

MONITOR
475
625
532
670
G-S
made-neat Payoffs-mutualism 1 0
2
1
11

MONITOR
535
625
592
670
G-G
made-neat Payoffs-mutualism 1 1
2
1
11

TEXTBOX
475
505
650
530
Mutualism Payoffs:
16
0.0
1

BUTTON
650
280
712
313
Go 1
go-eqn
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
715
280
777
313
Go 10
repeat 10 [go-eqn]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
780
280
847
313
Go 100
repeat 100 [go-eqn]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
850
280
912
313
Go
go-eqn
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
650
340
707
385
Mouse-X
ifelse-value mouse-inside? [mouse-xcor / x-max] [\"\"]
3
1
11

MONITOR
710
340
767
385
Mouse-Y
ifelse-value mouse-inside? [mouse-ycor / y-max] [\"\"]
3
1
11

TEXTBOX
650
115
800
135
Replicator Dynamics:
16
0.0
1

BUTTON
955
255
1017
288
Go 1
go-sim
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
955
215
1142
248
NIL
Replicator-Dynamics-By-ABM
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1020
255
1082
288
Go
go-sim
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
10
100
160
125
Simulate Groups:
16
0.0
1

SWITCH
1125
175
1302
208
Population-Pen-Down?
Population-Pen-Down?
1
1
-1000

SLIDER
1135
260
1307
293
Playing-Noise
Playing-Noise
0
20
0.0
0.5
1
%
HORIZONTAL

SLIDER
1075
140
1247
173
Replication-Noise
Replication-Noise
0
20
0.0
0.5
1
%
HORIZONTAL

SWITCH
10
285
242
318
Opponents-Include-Own-Group?
Opponents-Include-Own-Group?
0
1
-1000

SLIDER
645
140
817
173
Speed-1
Speed-1
0
100
10.0
5
1
%
HORIZONTAL

SLIDER
645
175
817
208
Speed-2
Speed-2
0
100
40.0
5
1
%
HORIZONTAL

INPUTBOX
680
535
760
595
Reward
3.0
1
0
Number

INPUTBOX
760
535
840
595
Sucker
0.0
1
0
Number

INPUTBOX
680
595
760
655
Temptation
5.0
1
0
Number

INPUTBOX
760
595
840
655
Punishment
1.0
1
0
Number

MONITOR
860
535
927
580
C-C: [R R]
made-neat Payoffs-Prisoners-Dilemma 0 0
17
1
11

MONITOR
860
585
927
630
D-C: [T S]
made-neat Payoffs-Prisoners-Dilemma 1 0
17
1
11

MONITOR
930
535
997
580
C-D: [S T]
made-neat Payoffs-Prisoners-Dilemma 0 1
17
1
11

MONITOR
930
585
997
630
D-D: [P P]
made-neat Payoffs-Prisoners-Dilemma 1 1
17
1
11

TEXTBOX
680
505
905
530
Prisoner's Dilemma Payoffs:
16
0.0
1

TEXTBOX
475
675
600
721
0 = S = Selfish\n1 = G = Generous
13
0.0
1

TEXTBOX
865
635
1015
666
0 = C = Cooperate\n1 = D = Defect
13
0.0
1

TEXTBOX
315
530
425
565
1 = H = Hawk\n0 = D = Dove
13
0.0
1

TEXTBOX
10
690
160
726
Donation Payoffs (Alternative PD):
16
0.0
1

MONITOR
170
690
260
735
C-C: [V-C V-C]
made-neat Payoffs-donation 0 0
17
1
11

MONITOR
265
690
355
735
C-D:  [-C V]
made-neat Payoffs-donation 0 1
17
1
11

MONITOR
170
740
260
785
D-C: [V -C]
made-neat Payoffs-donation 1 0
17
1
11

MONITOR
265
740
355
785
D-D: [0 0]
made-neat Payoffs-donation 1 1
17
1
11

TEXTBOX
10
740
160
771
0 = C = Cooperate\n1 = D = Defect
13
0.0
1

TEXTBOX
10
10
225
35
Evolving Game Players
20
0.0
1

TEXTBOX
10
40
230
60
This program (C) Christopher J Watts, 2021.
11
0.0
1

INPUTBOX
1275
395
1427
455
Memory-Length
2000.0
1
0
Number

CHOOSER
960
295
1127
340
Playing-Strategy
Playing-Strategy
"Last Action" "MSNE" "Memory" "Amadae" "Stochastic Memory" "MSNE / Stoch Memory"
3

MONITOR
815
10
1002
55
Mixed-Strategy Nash Equilibrium
MSNE
5
1
11

MONITOR
1005
10
1087
55
NIL
MSNE-Payoff
3
1
11

MONITOR
925
60
1032
105
G2 > G1 : (y > x)
num-pops-with-group2-dom
17
1
11

MONITOR
815
60
922
105
G1 > G2 : (x > y)
num-pops-with-group1-dom
17
1
11

SWITCH
955
140
1067
173
Replicate?
Replicate?
1
1
-1000

TEXTBOX
650
220
800
238
Use Equations:
14
0.0
1

TEXTBOX
955
120
1130
138
Use Agent-Base Simulation:
14
0.0
1

MONITOR
770
340
862
385
Mouse-Payoffs
mouse-payoffs
17
1
11

CHOOSER
1315
135
1462
180
Reposition-Populations
Reposition-Populations
"By Mean Action" "By Mean Belief"
1

BUTTON
1315
185
1432
218
Reposition Pops
reposition-pops
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
650
390
822
423
Color-Patches-By-Payoff1
Color-Patches-By-Payoff1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
650
425
822
458
Color-Patches-By-Payoff2
Color-Patches-By-Payoff2
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
650
460
792
493
Color Patches White
foreach sorted-patches [pa -> ask pa [set pcolor white]]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

INPUTBOX
10
375
85
435
Rounds
200.0
1
0
Number

BUTTON
170
135
232
168
NIL
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1115
60
1227
105
NIL
Count Populations
17
1
11

MONITOR
1035
60
1100
105
G1 = G2
num-pops-with-groups-equal
17
1
11

TEXTBOX
1325
235
1565
375
Key:\nAxes range from 0 (all Dove) to 1 (all Hawk).\nMean-Belief:\nx represents Group 2's beliefs about Group 1.\ny represents Group 1's beliefs about Group 2.\nMean-Action:\nx represents Group 1's most recent action.\ny represents Group 2's most recent action.\n
11
0.0
1

CHOOSER
960
345
1137
390
Memory-Initialization
Memory-Initialization
"Empty" "Random-50:50" "Random-MSNE" "Random-Action" "Fixed-Proportion-50:50" "Fixed-Proportion-MSNE" "Fixed-Proportion-Action"
0

INPUTBOX
960
395
1112
455
Memory-Initial-Weight-1
10.0
1
0
Number

SWITCH
955
175
1122
208
Recolor-Populations?
Recolor-Populations?
1
1
-1000

INPUTBOX
1115
395
1267
455
Memory-Initial-Weight-2
10.0
1
0
Number

TEXTBOX
1275
375
1425
393
(Not currently used)
11
0.0
1

@#$#@#$#@
# Evolving Game Players

## WHAT IS IT?

Simulates population dynamics for evolving populations of game players. Games are defined by their payoff function. The following games are included:

* __Hawk & Dove__
* __Prisoner's Dilemma__ (with actions "Cooperate" and "Defect")
* __Donation__ (a special case of Prisoner's Dilemma, but with same parameters as Hawk-Dove)
* __Mutualism__ (with actions "Selfish" and "Generous")

Population dynamics can be studied using equations describing a theoretical population, or using an agent-based simulation of actual players, with explicit representation of individuals being replicated.

This program was developed to illustrate the model of Bergstrom & Lachmann (2003), and similar work. (See references below.) 

This program (C) Christopher J Watts, 2021.

## HOW IT WORKS

The NetLogo world is used to draw a graph representing the state space for the games. Turtles are used to represent Populations of Players. Each population is divided into two groups (representing, e.g., species). Each player will have a strategy that determines which of two possible actions that player plays. (Games with more than two possible actions are not currently covered.) The population turtle has x and y coordinates. The x coordinate represents the proportion of group 1 who play the action denoted "1". (So for Hawk & Dove, x = the proportion of group 1 who play "Hawk".) The y coordinate represents the proportion of group 2 who play "1". If these proportions change, the position of the Population turtle will change. If the turtle's pen is down, a line is drawn tracing the population's evolution. Population turtles are created at multiple points in the state space, representing different proportions of "1"-players in groups 1 and 2. Thus we can study the evolution that follows from various initial positions.

Multiple ways of evolving a population are included:

* __Replicator-Dynamics-By-Equation__ : This uses equations to simulate the dynamics according to replicator theory.
* __Replicator-Dynamics-By-ABM__ : For each population, this creates a given number of players, and divides them between the two groups and the two strategies. Actual one-round games are then simulated between random pairs of players. The payoffs from these are used to sample for each group a number of "fit" players. Their strategies are then used to replace those held by randomly chosen players. The new proportions of "1"-players then determine the new x and y coordinates of the Population turtle. Dynamics will be stochastic, but should resemble those of the equation-based Draw-Replicator-Dynamics.

## HOW TO USE IT

Choose which game you want to study. Review the corresponding parameters and Payoffs Table for that game. Then choose a method of analysis, and click the corresponding button. Populations will be created at different positions, representing different proportions of "1"-players in groups 1 (x-axis) and 2 (y-axis). Population then move in this state space, according to the evolving proportions of "1"-players in the two groups.

The parameter __Opponents-Include-Own-Group?__ determines whether players play only members of the other group (Off) or play any players, including from their own group (On). This can have a big affect on the shape of the state space. N.B. This parameter does not affect replication, however: a player can only replace their strategy with one sampled from their own group.

## THINGS TO NOTICE

Most populations will converge on a small number of attractor points (representing equilibria). E.g. the Prisoner's Dilemma has one Nash Equilibrium (Defect-Defect). Hawk-Dove has three attractors: (x=1, y=0), (x=0, y=1), and a single point, defined by the ratio between parameters Value (V) and Cost (C), at (x=V/C, y=V/C).

The populations in the stochastic agent-based model will tend to follow the equation-based model, but with some random variation. However, if a group loses its last player of a particular strategy, the population then becomes stuck on a line (e.g. the line x=1 when group 1 has lost all its "0" players). Applying some Replication-Noise can reintroduce the missing strategy. Too much Replication-Noise, however, and the population might be unable to find the attractors at all.

## THINGS TO TRY

How do the game parameters affect the positions of attractors?

How do the evolution parameters (speed, delta) affect the positions of attractors?

How do the relative sizes of the two groups, and the size of the player population in the ABM, affect the position of attractors?

## EXTENDING THE MODEL

* Study players with learning. They remember the outcomes of previous interactions, and based their future actions on what they learnt from those interactions.
* Use NetLogo3D (or a 2D transformation of 3D points) to study games with three actions.
* Study more than two groups.
* Constrain players' interactions by organising players into social networks, and choosing their interaction partners from their neighbours in the network. See Hammond & Axelrod (2006) for why this might be interesting.



## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

Axelrod (1997, Ch.1-3) contains several simulation models based around the Prisoner's Dilemma.

## CREDITS AND REFERENCES

Amadae, S.M. (2020) "Binary Labels Reinforce Systemic Discrimination". Noema, November 17 2020. https://www.noemamag.com/binary-labels-reinforce-systemic-discrimination/

Axelrod, R. M. (1997). The complexity of cooperation : agent-based models of competition and collaboration. Princeton, N.J. ; Chichester: Princeton University Press.

Bergstrom, C. T., & Lachmann, M. (2003). The Red King effect: When the slowest runner wins the coevolutionary race. Proceedings of the National Academy of Sciences, 100(2), 593-598. doi:10.1073/pnas.0134966100

Bruner, J. P. (2019). Minority (dis)advantage in population games. Synthese, 196(1), 413-427. doi:10.1007/s11229-017-1487-8

Hammond, R. A., & Axelrod, R. (2006). The Evolution of Ethnocentrism. Journal of Conflict Resolution, 50(6), 926-936. doi:10.1177/0022002706293470

Hofbauer, J., & Sigmund, K. (1998). Evolutionary games and population dynamics. Cambridge: Cambridge University Press.

O’Connor, C. (2017). The cultural Red King effect. The Journal of Mathematical Sociology, 41(3), 155-171. doi:10.1080/0022250X.2017.1335723
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment-Amadae" repetitions="1" runMetricsEveryStep="false">
    <setup>Replicator-Dynamics-By-ABM</setup>
    <go>go-sim</go>
    <metric>timer</metric>
    <metric>count populations</metric>
    <metric>num-pops-with-group1-dom</metric>
    <metric>num-pops-with-group2-dom</metric>
    <metric>num-pops-with-groups-equal</metric>
    <metric>Cost</metric>
    <metric>msne</metric>
    <metric>msne-payoff</metric>
    <steppedValueSet variable="Perc-Group2" first="10" step="10" last="90"/>
    <steppedValueSet variable="Value-As-Perc-Of-Cost" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Strategy">
      <value value="&quot;MSNE&quot;"/>
      <value value="&quot;Amadae&quot;"/>
      <value value="&quot;MSNE / Stoch Memory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initialization">
      <value value="&quot;Empty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-2">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Belief&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-All" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Perc-Group2">
      <value value="65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-As-Perc-Of-Cost">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Strategy">
      <value value="&quot;Amadae&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-1">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-2">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Delta">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sucker">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temptation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Punishment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reward">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Belief&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
