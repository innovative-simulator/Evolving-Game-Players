;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evolving Game Players: Evolutionary Games & Population Dynamics
;; This program (C) Christopher J Watts, 2021.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [rnd csv]

globals [
  ; Last Seeds used by random number generator.
  prev-seed-setup
  prev-seed-go

  min-payoff
  max-payoff

  sorted-patches
  sorted-populations

  ; Crude method G2's belief x > G1's belief y etc.
  num-pops-with-group1-dom
  num-pops-with-group2-dom
  num-pops-with-groups-equal
  group1-payoff ; Mean over populations of mean over group members of last payoff received
  group2-payoff

  num-pops-g1-dominant
  num-pops-g2-dominant
  num-pops-none-dominant

  num-pops-g1-hawkish
  num-pops-g2-hawkish
  num-pops-none-hawkish

  stat-avg ; For displaying average of multiple populations
]

breed [arrows arrow]
breed [texts text]
breed [populations population]
breed [players player]
breed [stats stat] ; Agents used to represent statistical aggregates of populations

populations-own [
  x
  y
  po-players
  po-group1
  po-group2
  po-dominant-group ; No players left who beliefs could move them over the V/C line.
  po-round-dominant
  po-expectant-group ; Mean Beliefs of one group < those of the other
  po-round-expectant
  po-hawkish-group ; One group's out-actions are all H, while other's are all D.
  po-round-hawkish
  po-dom-groups ; Dominant group at round 50, 100, 150, ...
  po-exp-groups ; Expectant group at round 50, 100, 150, ...
  po-haw-groups ; Hawkish group at round 50, 100, 150, ...

]

players-own [
  pl-population
  pl-group
  pl-strategy
  pl-action
  pl-opponents-action
  pl-played-out-group?
  pl-fitness
  pl-memory
  pl-memory-length
  pl-other-interactions ; Number of interactions with members of other groups.
  pl-belief ; Number of "1" actions performed against this agent and entered into memory (but doesn't forget).
  pl-history ; Number of "1" actions performed by this agent in personal history.
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
  if Draw-X-And-Y-Axes? [draw-axes]

  set min-payoff min map [A -> min payoffs (first A) (last A)] (list [0 0] [0 1] [1 0] [1 1]) ; Used to remove negative payoffs, and for scale-color.
  set max-payoff max map [A -> max payoffs (first A) (last A)] (list [0 0] [0 1] [1 0] [1 1]) ; Used for scale-color.

  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-rng [given-variable-name]
  ifelse 0 = runresult given-variable-name [
    run (word "set prev-" given-variable-name " " new-seed)
  ]
  [
    run (word "set prev-" given-variable-name " " given-variable-name)
  ]
  random-seed runresult (word "prev-" given-variable-name)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Payoff table cells

to-report payoffs [a-move b-move]
  if game = "Hawk-Dove" [report payoffs-hawk-dove a-move b-move]
  if game = "Hawk-Dove Optimal" [report payoffs-hawk-dove-optimal a-move b-move]
  if game = "Mutualism" [report payoffs-mutualism a-move b-move]
  if game = "Prisoner's Dilemma" [report payoffs-Prisoners-Dilemma a-move b-move]
  if game = "Donation" [report payoffs-donation a-move b-move]
  if game = "Battle of the Sexes" [report payoffs-battle-of-the-sexes a-move b-move]
  report false
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report Payoffs-Mutualism [A-Move B-Move]
  report item (A-Move * 2 + B-Move) (list
    (list k k) ; Genererous-Genererous
    (list 1 2) ; Genererous-Selfish
    (list 2 1) ; ; Selfish-Genererous
    (list 0 0) ; Selfish-Selfish
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

to-report Payoffs-Hawk-Dove-Optimal [A-Move B-Move]
  let cur-payoffs Payoffs-Hawk-Dove a-move b-move
  report ifelse-value (first Payoffs-Hawk-Dove a-move b-move >= first Payoffs-Hawk-Dove (1 - a-move) b-move) [
    ifelse-value (last Payoffs-Hawk-Dove a-move b-move >= last Payoffs-Hawk-Dove a-move (1 - b-move)) [[1 1]] [[1 0]]
  ] [
    ifelse-value (last Payoffs-Hawk-Dove a-move b-move >= last Payoffs-Hawk-Dove a-move (1 - b-move)) [[0 1]] [[0 0]]
  ]
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

to-report Payoffs-Battle-Of-The-Sexes [A-Move B-Move]
  report item (A-Move * 2 + B-Move) (list
    (list your-preference my-preference) ; We both went with B's ideal.
    (list 0 0) ; ; We missed each other.
    (list 0 0) ; ; We missed each other.
    (list my-preference your-preference) ; We both went with A's ideal.
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
  if Draw-Diagonal? [
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
  ]

  ; MSNE
  if Draw-MSNE-Lines? [
    create-arrows 1 [
      set color grey
      set pen-size 2
      set heading 90
      setxy 0 (msne * y-max)
      foreach (range 2 100 2) [a100 ->
        setxy (x-max * a100 / 100) (msne * y-max)
        ifelse 0 = a100 mod 4 [pen-down] [pen-up]
      ]
      pen-up
      setxy (msne * x-max) 0
      foreach (range 2 100 2) [a100 ->
        setxy (msne * x-max) (y-max * a100 / 100)
        ifelse 0 = a100 mod 4 [pen-down] [pen-up]
      ]
      die
    ]
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

  if Draw-Diagonal? [
    create-texts 1 [
      set size 0
      set label-color black
      set label "y = x"
      setxy (x-max + 0.5) (y-max)
    ]
  ]

  if Draw-MSNE-Lines? [
    create-texts 1 [
      set size 0
      set color black
      set label-color black
      set label "V/C"
      setxy 0 (msne * y-max)
      set pen-size 2
      pen-down
      setxy -0.25 (msne * y-max)
      pen-up
      setxy -0.25 (msne * y-max)
    ]
    create-texts 1 [
      set size 0
      set color black
      set label-color black
      set label "V / C"
      setxy (msne * x-max) 0
      set pen-size 2
      pen-down
      setxy (msne * x-max) -0.25
      pen-up
      setxy (msne * x-max) -1
    ]
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

to Color-Patches-By-Payoff [first-not-last?]
  foreach sorted-patches [pa ->
    ask pa [
      if (pxcor >= 0 and pycor >= 0 and pxcor <= x-max and pycor <= y-max) [
        let cur-payoffs payoffs-xy (pxcor / x-max) (pycor / y-max)
        ifelse first-not-last? [
          set pcolor scale-color blue (first cur-payoffs) min-payoff max-payoff
        ]
        [
          set pcolor scale-color red (last cur-payoffs) min-payoff max-payoff
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-pops-by-players
  foreach sorted-populations [po ->
    ask po [
      ; Assuming pl-fitness is payoff from just one round.
      set color scale-color green (mean map [a -> [pl-fitness] of a] po-players) (-1 + min-payoff) (1 + max-payoff)
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-pops-by-xy
  foreach sorted-populations [po ->
    ask po [
      ; Assuming pl-fitness is payoff from just one round.
      let cur-payoffs payoffs-xy x y
      set color scale-color green (first cur-payoffs) (-1 + min-payoff) (1 + max-payoff)
    ]
  ]
end

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
;; Agent-Based Simulation using Replication and/or Memory

to Setup-Stochastic-Sim
  ; Setup populations at evenly-spaced points in state space.
  ; Each population contains a number of player agents,
  ; divided between two groups.
  ; Players will then:
  ; either base play same pl-action move each match,
  ; after which population is updated using stochastic replication,
  ; or players will base decisions on sampling and memory,
  ; and memory will be updated after each match against
  ; a member of the opposite group.

  setup
  setup-rng "Seed-Setup"
  setup-initial-populations

  set sorted-populations sort populations


;  recolor-pops-by-players
  reset-ticks
  reposition-pops
  reposition-players
  setup-stats

  ; Draw paths?
  if Population-Pen-Down? [
    foreach sorted-populations [po ->
      ask po [ifelse (Population-Pen-Down? and memory-initialization != "Empty") [pen-down] [pen-up]]
      ; If Empty, initial position is meaningless.
    ]
  ]
  setup-rng "Seed-Go"
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-initial-populations
  if initial-populations = "11x11 Evenly Spaced" [
    foreach (range 0 110 10) [x100 ->
      foreach (range 0 110 10) [y100 ->
        setup-population x100 y100
      ]
    ]
    stop
  ]

  if initial-populations = "6x6 Evenly Spaced" [
    foreach (range 0 120 20) [x100 ->
      foreach (range 0 120 20) [y100 ->
        setup-population x100 y100
      ]
    ]
    stop
  ]

  if initial-populations = "1 at Initial-X/Y" [
    setup-population initial-x initial-y
    stop
  ]
  if initial-populations = "10 at Initial-X/Y" [
    repeat 10 [setup-population initial-x initial-y]
    stop
  ]
  if initial-populations = "100 at Initial-X/Y" [
    repeat 100 [setup-population initial-x initial-y]
    stop
  ]

  if initial-populations = "1 at MSNE" [
    setup-population (100 * msne) (100 * msne)
    stop
  ]
  if initial-populations = "10 at MSNE" [
    repeat 10 [setup-population (100 * msne) (100 * msne)]
    stop
  ]
  if initial-populations = "100 at MSNE" [
    repeat 100 [setup-population (100 * msne) (100 * msne)]
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-population [x100 y100]
  create-populations 1 [
    set hidden? hide-populations?
    set shape "default"
    ;set color yellow
    setxy (x-max * x100 / 100) (y-max * y100 / 100)
    set x x100 / 100
    set y y100 / 100
    setxy (x-max * x) (y-max * y)
    facexy (x-max * next-x) (y-max * next-y)
    ;        if Population-Pen-Down? [pen-down]

    ; Create Population's players
    set po-players []
    set po-group1 []
    set po-group2 []
    set po-dominant-group 0 ; 0 means neither group 1 nor group 2 dominates.
    set po-round-dominant false ; false means no group dominant yet.
    set po-dom-groups [] ; Dominant group at round 50, 100, 150, ...
    set po-exp-groups [] ; Expectant group at round 50, 100, 150, ...
    set po-haw-groups [] ; Hawkish group at round 50, 100, 150, ...

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
          set hidden? hide-players?
          set shape "dot"
          set color item g (list grey blue red)
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
            if player-pen-down? [pen-down]
          ]
          set pl-strategy strategy-reporter
          set pl-action s
          set pl-played-out-group? true

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
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-initial-memory
  ; Run as player.

  set pl-memory []
  set pl-memory-length ifelse-value (pl-group = 1) [Memory-Length-1] [Memory-Length-2]
  ; (Actually, we're currently using belief instead of memory.)
  if memory-initialization = "Empty" [
    set pl-belief 0
    set pl-other-interactions 0
    stop
  ]
  set pl-other-interactions ifelse-value (pl-group = 1) [Memory-Initial-Weight-1] [Memory-Initial-Weight-2]
  let prob mean map [pl -> [pl-action] of pl] ifelse-value (pl-group = 1) [[po-group1] of pl-population] [[po-group2] of pl-population]
  set pl-history round (prob * pl-other-interactions)

  if memory-initialization = "Fixed-Proportion-50:50" [
    ; Assumes past opponents played "1" in 50% of matches, "0" otherwise.
    set prob 0.5
    set pl-belief round (prob * pl-other-interactions)
    stop
  ]
  if memory-initialization = "Fixed-Proportion-MSNE" [
    ; Assumes past opponents played the Mixed-Strategy Nash Equilibrium.
    set prob msne
    set pl-belief round (prob * pl-other-interactions)
    stop
  ]
  if memory-initialization = "Fixed-Proportion-xy" [
    ; Makes use of the fact that players have been initialised with pl-action
    ; values in proportion to the x and y positions of the population.
    set prob mean map [pl -> [pl-action] of pl] ifelse-value (pl-group = 1) [[po-group2] of pl-population] [[po-group1] of pl-population]
    set pl-belief round (prob * pl-other-interactions)
    stop
  ]

  ; Random memories, using the fixed proportions as probabilities
  ; and Bernoulli-distributed random sampling.

  if memory-initialization = "Random-50:50" [
    set prob 0.5
  ]
  if memory-initialization = "Random-MSNE" [
    set prob msne
  ]
  if memory-initialization = "Random-xy" [
    set prob mean map [pl -> [pl-action] of pl] ifelse-value (pl-group = 1) [[po-group2] of pl-population] [[po-group1] of pl-population]
  ]

  ifelse unlimited-memory? [
    ; (NetLogo doesn't have a random-binomial procedure, sum bernoulli trials instead.)
    set pl-belief sum n-values pl-other-interactions [ifelse-value (prob > random-float 1) [1] [0]]
  ]
  [
    ; (list opponents-action pl-action players-payoff given-opponent)
    set pl-memory (map [[x1 x2 x3 x4] -> (list x1 x2 x3 x4)]
      (n-values pl-other-interactions [ifelse-value (prob > random-float 1) [1] [0]])
      (n-values pl-other-interactions [[]]) ; Not in use yet.
      (n-values pl-other-interactions [0]) ; Not in use yet.
      (n-values pl-other-interactions [nobody]) ; Not in use yet.
      )
    set pl-belief sum map [a -> first a] pl-memory
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to play-game
  foreach sorted-populations [po ->
    ask po [
      let participants shuffle po-players

      ifelse Opponents-Include-Own-Group? [
        (foreach (sublist participants 0 (int ((length participants) / 2))) (sublist participants (int ((length participants) / 2)) (length participants)) [[ego alter] ->
          play-given-participants ego alter
        ])
      ]
      [
        let alter nobody
        foreach participants [ego ->
          set alter one-of ifelse-value ([pl-group = 1] of ego) [po-group2] [po-group1]
          play-given-participants ego alter
        ]
      ]
      update-at-end-of-round
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to play-given-participants [ego alter]
  let ego-action 0
  let alter-action 0
  let cur-payoffs [0.0 0.0]
  repeat 1 [
    ask ego [play-move alter]
    ask alter [play-move ego]

    set cur-payoffs payoffs ([pl-action] of ego) ([pl-action] of alter)
    ask ego [update-after-match-given alter cur-payoffs]
    ask alter [update-after-match-given ego (reverse cur-payoffs)]
  ]
end

to play-move [opponent]
  set pl-action (runresult pl-strategy opponent)
  if 0 < playing-noise [
    if playing-noise > random-float 100 [set pl-action 1 - pl-action]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-after-match-given [alter cur-payoffs]
  set pl-played-out-group? pl-group != [pl-group] of alter
  if Print-Out-Match-Results? [if pl-played-out-group? [print (word self " played " alter " : " pl-action ", " ([pl-action] of alter) " for payoffs " (map [p -> precision p 2] cur-payoffs))]]
  set pl-opponents-action [pl-action] of alter
  set pl-fitness (first cur-payoffs) ; For evolutionary dynamics
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to play-game-old
  foreach sorted-populations [po ->
    ask po [
      foreach shuffle po-players [ego -> ; Giving everyone a chance to be updated.
        ask ego [
;          set pl-fitness 0
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
          let cur-payoffs [0.0 0.0]
          repeat 1 [
            set alter one-of opponents
            while [alter = ego] [set alter one-of opponents]
            set ego-action (runresult pl-strategy alter)
            set alter-action [(runresult pl-strategy ego)] of alter
            if 0 < playing-noise [
              if playing-noise > random-float 100 [set ego-action 1 - ego-action]
              if playing-noise > random-float 100 [set alter-action 1 - alter-action]
            ]
            set cur-payoffs payoffs ego-action alter-action
            set pl-played-out-group? pl-group != [pl-group] of alter
            if Print-Out-Match-Results? [if pl-played-out-group? [print (word ego " played " alter " : " ego-action ", " alter-action " for payoffs " (map [p -> precision p 2] cur-payoffs))]]
;            if pl-played-out-group? [set pl-action ego-action] ; Should we record the pre-noise version instead?
            set pl-action ego-action ; Should we record the pre-noise version instead?
            set pl-opponents-action alter-action
            set pl-fitness (first cur-payoffs) ; For evolutionary dynamics
;            update-after-match ego alter alter-action (first cur-payoffs)
;            update-after-match alter-action (first cur-payoffs)
            ; NB: alter doesn't get get any payoff from this.
            ;update-after-match alter ego ego-action (last cur-payoffs)
          ]
        ]
      ]
      update-at-end-of-round
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-after-match
;to update-after-match [given-player given-opponent opponents-action players-payoff]
  ;set pl-fitness pl-fitness + players-payoff ; For evolutionary dynamics
  if pl-played-out-group? [
    ifelse not Unlimited-Memory? [
    ; Only remembering interactions with other groups.
      set pl-memory fput (list pl-opponents-action pl-action) sublist pl-memory 0 (min (list (length pl-memory) pl-memory-length)) ; For cultural models
;      set pl-memory fput (list opponents-action pl-action players-payoff given-opponent) sublist pl-memory 0 (min (list (length pl-memory) pl-memory-length)) ; For cultural models
      set pl-belief sum map [m -> first m] pl-memory
      set pl-history sum map [m -> item 1 m] pl-memory
      set pl-other-interactions length pl-memory
    ]
    [
      ; If using "unlimited" memory, then storing in pl-memory is a waste. Better to just keep a count.
      set pl-belief pl-belief + pl-opponents-action ; NB: Updating beliefs during the round. Ego could now become an opponent in this same round, and base actions on this updated belief.
      set pl-history pl-history + pl-action
      set pl-other-interactions pl-other-interactions + 1
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-at-end-of-round
  foreach filter [pl -> [pl-played-out-group? ] of pl] po-players [pl ->
    ask pl [
      update-after-match
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report strategy-reporter
  ; For belief learning
  if playing-strategy = "Play-MSNE" [report [a -> bl-playing msne a]]
  if playing-strategy = "Expect-MSNE" [report [a -> bl-expecting msne a]]
  if playing-strategy = "Play-Initial-X-Y" [report [a -> bl-playing (ifelse-value (pl-group = 1) [initial-x / 100] [initial-y / 100]) a]]
  if playing-strategy = "Expect-Initial-X-Y" [report [a -> bl-expecting (ifelse-value (pl-group = 1) [initial-x / 100] [initial-y / 100]) a]]
  if playing-strategy = "Play-Random" [report [a -> bl-playing 0.5 a]]
  if playing-strategy = "Play-H-D" [report [a -> bl-playing (ifelse-value (pl-group = 1) [1] [0]) a]]
  if playing-strategy = "Play-D-H" [report [a -> bl-playing (ifelse-value (pl-group = 1) [0] [1]) a]]
  if playing-strategy = "Expect-D-D" [report [a -> bl-expecting 0 a]]
  if playing-strategy = "Expect-H-H" [report [a -> bl-expecting 1 a]]
  if playing-strategy = "Expect-H-0.5" [report [a -> bl-expecting 0.5 a]]

  ; Other methods for decisions (for testing)
  if playing-strategy = "MSNE" [report [a -> bl-playing msne a]]
  if playing-strategy = "Memory" [report [a -> playing-memory]]
  if playing-strategy = "Stochastic Memory" [report [a -> playing-memory-stochastic]]

  ; For replicator dynamics
  if playing-strategy = "Last Action" [report [a -> pl-action]]

  user-message (word "Warning! Playing-Strategy " playing-strategy " was not identified.")
  report [-> pl-action]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report best-response-to [given-expectation]
  report ifelse-value (given-expectation * 100.0 < 1.0 * value-as-perc-of-cost) [1] [
    ifelse-value (given-expectation * 100.0 = 1.0 * value-as-perc-of-cost) [
      random 2
    ]
    [0]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report bl-playing [given-prob given-alter]
  if pl-group = [pl-group] of given-alter [report playing-mixed-strategy msne]
  if pl-other-interactions = 0 [report playing-mixed-strategy given-prob]
  report playing-memory
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report bl-expecting [given-prob given-alter]
  if pl-group = [pl-group] of given-alter [report playing-mixed-strategy msne]
  if pl-other-interactions = 0 [report best-response-to given-prob]
  report playing-memory
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report playing-mixed-strategy [given-prob]
  report ifelse-value given-prob > random-float 1 [1] [0]
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
  if pl-other-interactions = 0 [report playing-mixed-strategy msne]
  ;if empty? pl-memory [report playing-msne]
  ;let prob (length filter [h -> 1 = first h] pl-memory) / length pl-memory
;  let prob pl-belief / pl-other-interactions
;  let a (prob * first payoffs 0 1) + ((1 - prob) * first payoffs 0 0)
;  let b (prob * first payoffs 1 1) + ((1 - prob) * first payoffs 1 0)
  ; Keeping as many integers as possible.

;  ; Hawk-Dove specific
;  let a pl-belief * 100
;  let b pl-other-interactions * value-as-perc-of-cost

  ; Would work for any payoffs table
  let a (pl-belief * first payoffs 0 1) + ((pl-other-interactions - pl-belief) * first payoffs 0 0)
  let b (pl-belief * first payoffs 1 1) + ((pl-other-interactions - pl-belief) * first payoffs 1 0)

  if a > b [report 0]
  if b > a [report 1]
  report random 2 ; Not neutral! Should have played MSNE instead!
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report playing-memory-stochastic
  if pl-other-interactions = 0 [report playing-mixed-strategy msne]
  let p pl-belief / pl-other-interactions
  let a (p * first payoffs 0 1) + ((1 - p) * first payoffs 0 0)
  let b (p * first payoffs 1 1) + ((1 - p) * first payoffs 1 0)

  if a > random-float (a + b) [report 0]
  report 1
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reproduction of population by random sampling

to replicate
  ; Use random sampling to build the next generation.
  let fitness-corrective 0 - min (list 0 min-payoff) ; rnd:weighted cannot use negative payoffs.
  let m (delta * speed-1 / 100)
  let n (delta * speed-2 / 100)
  foreach sorted-populations [po ->
    ask po [
      let num-updating-1 round (m * length po-group1)
      let num-updating-2 round (n * length po-group2)
      let sampled-players rnd:weighted-n-of-list-with-repeats num-updating-1 (map [a -> (list ([pl-fitness + fitness-corrective] of a) a)] po-group1) [af -> first af]
      let sampled-strategies map [af -> [(list pl-action pl-belief pl-other-interactions (map [a -> a] pl-memory) pl-history pl-fitness)] of last af] sampled-players
      (foreach (n-of num-updating-1 po-group1) sampled-strategies [[pl strat] ->
        ask pl [
          set pl-action ifelse-value (replication-noise > random-float 100) [1 - item 0 strat] [item 0 strat]
          set pl-belief item 1 strat
          set pl-other-interactions item 2 strat
          set pl-memory item 3 strat
          set pl-history item 4 strat
          set pl-fitness item 5 strat
        ]
      ])

      set sampled-players rnd:weighted-n-of-list-with-repeats num-updating-2 (map [a -> (list ([pl-fitness + fitness-corrective] of a) a)] po-group2) [af -> first af]
      set sampled-strategies map [af -> [(list pl-action pl-belief pl-other-interactions (map [a -> a] pl-memory) pl-history pl-fitness)] of last af] sampled-players
      (foreach (n-of num-updating-2 po-group2) sampled-strategies [[pl strat] ->
        ask pl [
          set pl-action ifelse-value (replication-noise > random-float 100) [1 - item 0 strat] [item 0 strat]
          set pl-belief item 1 strat
          set pl-other-interactions item 2 strat
          set pl-memory item 3 strat
          set pl-history item 4 strat
          set pl-fitness item 5 strat
        ]
      ])
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repositioning populations
;; (setting their x and y variables, then xcor and ycor.)

to re-position-pops-by-actions
  foreach sorted-populations [po ->
    ask po [
      set hidden? hide-populations?
      set x mean map [a -> [pl-action] of a] po-group1 ; x is G1's mean action.
      set y mean map [a -> [pl-action] of a] po-group2 ; y is G2's mean action.

      setxy (x-max * x) (y-max * y)
      facexy (x-max * next-x) (y-max * next-y)
      ifelse population-pen-down? [pen-down] [pen-up]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to re-position-pops-by-out-actions
  foreach sorted-populations [po ->
    ask po [
      set hidden? hide-populations?
      let g-out filter [a -> [pl-played-out-group?] of a] po-group1
      if not empty? g-out [set x mean map [a -> [pl-action] of a] g-out] ; x is G1's mean action against out-group G2.
      set g-out filter [a -> [pl-played-out-group?] of a] po-group2
      if not empty? g-out [set y mean map [a -> [pl-action] of a] g-out] ; y is G2's mean action against out-group G1.

      setxy (x-max * x) (y-max * y)
      facexy (x-max * next-x) (y-max * next-y)
      ifelse population-pen-down? [pen-down] [pen-up]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to re-position-pops-by-beliefs
  foreach sorted-populations [po ->
    ask po [
      set hidden? hide-populations?
      let g-out filter [pl -> [pl-other-interactions > 0] of pl] po-group1
      if not empty? g-out [set y mean map [pl -> [pl-belief / pl-other-interactions] of pl] g-out] ; y is G1's belief about G2.
      set g-out filter [pl -> [pl-other-interactions > 0] of pl] po-group2
      if not empty? g-out [set x mean map [pl -> [pl-belief / pl-other-interactions] of pl] g-out] ; x is G2's belief about G1.

      setxy (x-max * x) (y-max * y)
      facexy (x-max * next-x) (y-max * next-y)
      ifelse population-pen-down? [pen-down] [pen-up]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reposition-pops
  if Reposition-Populations = "By Mean Belief" [re-position-pops-by-beliefs]
  if Reposition-Populations = "By Mean Action" [re-position-pops-by-actions]
  if Reposition-Populations = "By Mean Out Action" [re-position-pops-by-out-actions]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reposition-players
  foreach sorted-populations [po ->
    ask po [
      foreach po-group1 [pl ->
        ask pl [
;          setxy (x-max * pl-action) (y-max * pl-belief / pl-other-interactions)
          if pl-other-interactions > 0 [
            setxy (x-max * pl-history / pl-other-interactions) (y-max * pl-belief / pl-other-interactions)
          ]
        ]
      ]
      foreach po-group2 [pl ->
        ask pl [
;          setxy (x-max * pl-belief / pl-other-interactions) (y-max * pl-action)
          if pl-other-interactions > 0 [
            setxy (x-max * pl-belief / pl-other-interactions) (y-max * pl-history / pl-other-interactions)
          ]
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main simulation step for the stochastic sim.

to go-sim
  if ticks = rounds [stop]
  if calculate-dominance? [if num-pops-g1-dominant + num-pops-g2-dominant = count populations [stop]]

  play-game
  if Replicate? [replicate]
  reposition-pops
  reposition-players
  draw-stats

  if Calculate-Dominance? [
    calc-pop-dominance
  ]
  set group1-payoff mean [mean map [pl -> [pl-fitness] of pl] po-group1] of populations
  set group2-payoff mean [mean map [pl -> [pl-fitness] of pl] po-group2] of populations

;  if Recolor-Populations? [recolor-pops-by-players]
  if Recolor-Populations? [recolor-pops-by-xy]
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to print-beliefs
  ask min-one-of populations [who] [
    print ""
    show (word "Round " ticks ":")
    let memory-types []
    (foreach ["Group 1:" "Group 2:"] (list po-group1 po-group2) [[gn gp] ->
      print gn
      ifelse unlimited-memory? [
        set memory-types sort map [pl -> [ifelse-value (pl-other-interactions = 0) ["Empty"] [pl-belief / pl-other-interactions]] of pl] gp
      ]
      [
        set memory-types sort map [pl -> [ifelse-value (empty? pl-memory) ["Empty"] [reduce word map [m -> (word first m)] pl-memory]] of pl] gp
      ]
      let cur-item "Empty"
      let cur-freq 0
      foreach memory-types [m ->
        ifelse m = cur-item [
          set cur-freq cur-freq + 1
        ]
        [
          print (word cur-freq " : " cur-item)
          set cur-freq 1
          set cur-item m
        ]
      ]
      print (word cur-freq " : " cur-item)
    ])
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to calc-pop-dominance
  ; Definitive measure if no noise.
  ; No players left who beliefs could move them over the V/C line.
  set num-pops-g1-dominant 0
  set num-pops-g2-dominant 0
  set num-pops-none-dominant 0

  foreach sorted-populations [po ->
    ask po [
      set po-dominant-group dominant-group
      if po-dominant-group != 0 [
        if po-round-dominant = false [ ; Just became dominated.
          set po-round-dominant ticks
        ]
      ]
      ifelse po-dominant-group = 0 [
        set num-pops-none-dominant num-pops-none-dominant + 1
      ]
      [
        ifelse po-dominant-group = 2 [
          set num-pops-g2-dominant num-pops-g2-dominant + 1
        ]
        [
          set num-pops-g1-dominant num-pops-g1-dominant + 1
        ]
      ]
    ]
  ]

  ; Quick-and-dirty measure
  ; Mean Beliefs of one group < those of the other
  set num-pops-with-group1-dom 0
  set num-pops-with-group2-dom 0
  set num-pops-with-groups-equal 0

  foreach sorted-populations [po ->
    ask po [
      let g1-bel 0
      let g2-bel 0
      let g-out filter [pl -> [pl-other-interactions > 0] of pl] po-group1
      if not empty? g-out [set g1-bel mean map [pl -> [pl-belief / pl-other-interactions] of pl] g-out] ; y is G1's belief about G2.
      set g-out filter [pl -> [pl-other-interactions > 0] of pl] po-group2
      if not empty? g-out [set g2-bel mean map [pl -> [pl-belief / pl-other-interactions] of pl] g-out] ; x is G2's belief about G1.
      ifelse g2-bel > g1-bel [
        if po-expectant-group != 1 [set po-round-expectant ticks]
        set po-expectant-group 1
        set num-pops-with-group1-dom num-pops-with-group1-dom + 1
      ]
      [
        ifelse g2-bel < g1-bel [
          if po-expectant-group != 2 [set po-round-expectant ticks]
          set po-expectant-group 2
          set num-pops-with-group2-dom num-pops-with-group2-dom + 1
        ]
        [
          if po-expectant-group != 0 [set po-round-expectant false]
          set po-expectant-group 0
          set num-pops-with-groups-equal num-pops-with-groups-equal + 1
        ]
      ]
    ]
  ]

  ; Ari-Pekka's method
  ; One group's out-actions are all H, while other's are all D.
  set num-pops-g1-hawkish 0
  set num-pops-g2-hawkish 0
  set num-pops-none-hawkish 0

  foreach sorted-populations [po ->
    ask po [
      let g1-act 0
      let g2-act 0
      let g-out filter [a -> [pl-played-out-group?] of a] po-group1
      if not empty? g-out [set g1-act mean map [a -> [pl-action] of a] g-out] ; x is G1's mean action against out-group G2.
      set g-out filter [a -> [pl-played-out-group?] of a] po-group2
      if not empty? g-out [set g2-act mean map [a -> [pl-action] of a] g-out] ; y is G2's mean action against out-group G1.

      ifelse g1-act > g2-act [
        if po-round-hawkish = false [if po-hawkish-group != 1 [set po-round-hawkish ticks]]
        set po-hawkish-group 1
        set num-pops-g1-hawkish num-pops-g1-hawkish + 1
      ]
      [
        ifelse g1-act < g2-act [
          if po-round-hawkish = false [if po-hawkish-group != 2 [set po-round-hawkish ticks]]
          set po-hawkish-group 2
          set num-pops-g2-hawkish num-pops-g2-hawkish + 1
        ]
        [
;          if po-hawkish-group != 0 [set po-round-hawkish false]
          set po-hawkish-group 0
          set num-pops-none-hawkish num-pops-none-hawkish + 1
        ]
      ]
    ]
  ]

  if 9 = ticks mod 10 [
    foreach sorted-populations [po ->
      ask po [
        ; Dominant group at round 50, 100, 150, ...
        set po-dom-groups fput po-dominant-group po-dom-groups
        set po-exp-groups fput po-expectant-group po-exp-groups
        set po-haw-groups fput po-hawkish-group po-haw-groups
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to print-dominance-errors
  ; To be run when all populations have converged.
  print ""
  print "Dominance Metrics Errors:"
  print "ticks \tPop\tPRed\tV/C\tExpectnt\tHawkish\tDominant\tG1Dom\tG2Dom\tNoneDom\tStrategy"
  let num-entries max [length po-dom-groups] of populations
  foreach n-values num-entries [i -> i] [i ->
    print (word ((i + 1) * 10) "\t"
      (count populations) "\t"
      (perc-group2) "\t"
      (value-as-perc-of-cost / 100) "\t"
      ((count populations) - count populations with [po-dominant-group = item (num-entries - i - 1) po-exp-groups]) "\t"
      ((count populations) - count populations with [po-dominant-group = item (num-entries - i - 1) po-haw-groups]) "\t"
      ((count populations) - count populations with [po-dominant-group = item (num-entries - i - 1) po-dom-groups]) "\t"
      Num-Pops-G1-Dominant "\t"
      Num-Pops-G2-Dominant "\t"
      Num-Pops-None-Dominant "\t"
      Playing-Strategy
    )
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to file-print-dominance-errors
  ; To be run when all populations have converged.
  file-open (word "Dom_Errors_BS" behaviorspace-run-number ".csv")
  file-print (csv:to-row (list "ticks" "Pop" "PRed" "VOverC" "Expectant" "Hawkish" "Dominant" "G1Dom" "G2Dom" "NoneDom" "Strategy") ",")
  let num-entries max [length po-dom-groups] of populations
  foreach n-values num-entries [i -> i] [i ->
    file-print (csv:to-row (list ((i + 1) * 10)
      (count populations)
      (perc-group2)
      (value-as-perc-of-cost / 100)
      ((count populations) - count populations with [po-dominant-group = item (num-entries - i - 1) po-exp-groups])
      ((count populations) - count populations with [po-dominant-group = item (num-entries - i - 1) po-haw-groups])
      ((count populations) - count populations with [po-dominant-group = item (num-entries - i - 1) po-dom-groups])
      Num-Pops-G1-Dominant
      Num-Pops-G2-Dominant
      Num-Pops-None-Dominant
      Playing-Strategy
    ) ",")
  ]
  file-close
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report Dominant-Group
  ; Run by population.
  ; Returns 0 if neither group meets definition,
  ; or 1 for group 1, 2 for group 2.
  if Belief-And-V-Over-C-Test? po-group1 true [
    ; G1 can only play Dove
    if not Belief-And-V-Over-C-Test? po-group2 false [
      ; It's not the case that G2 can only play Hawk.
      report 0
    ]
    ; G2 can only play Hawk.
    report 2
  ]
  if not Belief-And-V-Over-C-Test? po-group1 false [
    ; It's not the case that G1 can only play Hawk.
    report 0
  ]
  ; G1 can only play Hawk.
  if not Belief-And-V-Over-C-Test? po-group2 true [
    ; It's not the case that G2 can only play Dove.
    report 0
  ]
  ; G2 can only play Dove.
  report 1
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report Belief-And-V-Over-C-Test? [given-group above-not-below?]
  if above-not-below? [
    foreach given-group [pl ->
      if [(100 * pl-belief) <= (pl-other-interactions * value-as-perc-of-cost)] of pl [
        report false
      ]
    ]
    report true
  ]
  foreach given-group [pl ->
    if [(100 * pl-belief) >= (pl-other-interactions * value-as-perc-of-cost)] of pl [
      report false
    ]
  ]
  report true
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statistics aboust all populations, represented by a turtle

to setup-stats
  ; Exactly what this is the average of will depend
  ; on how xcor and ycor are being set for populations,
  ; e.g. mean group beliefs, mean group actions, or mean group out actions.

  create-stats 1 [
    set hidden? hide-statistics? or memory-initialization = "Empty"
    set stat-avg self
    set pen-size 3
    set color grey
;    setxy (mean [xcor] of populations) (mean [ycor] of populations)
    setxy (median [xcor] of populations) (median [ycor] of populations)
    if memory-initialization != "Empty" [
      if not hide-statistics? [pen-down]
      if Print-Statistics? [print (word ticks "\t " (xcor / x-max) "\t" (ycor / y-max))]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to draw-stats
  ask stat-avg [
;    setxy (mean [xcor] of populations) (mean [ycor] of populations)
    setxy (median [xcor] of populations) (median [ycor] of populations)
    set color item (ticks mod length stat-colors) stat-colors
    set hidden? hide-statistics?
    if not hide-statistics? [pen-down]
    if Print-Statistics? [print (word ticks "\t " (precision (xcor / x-max) 3) "\t" (precision (ycor / y-max) 3))]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report stat-colors
  report (list (orange + 2) (green - 2))
  report (list red green blue)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replicator Dynamics using Equations

to Replicator-Dynamics-By-Equation
  ; Setup populations as evenly-spaced points in state space.
  ; Ready for evolving positions using equations.

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
;        facexy (xcor + x-max * x-dot) (ycor + y-max * y-dot)
        facexy (x-max * next-x) (y-max * next-y)
;        pen-down
        if Population-Pen-Down? [pen-down]
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
  ; If scored on games against both groups
  ; Needs checking?
;  let z ((perc-group2 * y) + ((100 - perc-group2) * x)) / 100
;  report ( (1 - m * delta) * x + m * delta * x * payoff-for-A-move-given-perc 1 z ) /
;  ( (1 - m * delta) + m * delta * ((x * payoff-for-A-move-given-perc 1 z) + ((1 - x) * payoff-for-A-move-given-perc 0 z)) )

  ; If scored on games against other group (which occur at a rate dependent on proportion of population in that group.)
  report ( (1 - m * delta * perc-group2 / 100) * x + m * delta * (perc-group2 / 100) * x * payoff-for-A-move-given-perc 1 y ) /
  ( (1 - m * delta * perc-group2 / 100) + m * delta * (perc-group2 / 100) * ((x * payoff-for-A-move-given-perc 1 y) + ((1 - x) * payoff-for-A-move-given-perc 0 y)) )

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
  ; If scored on games against both groups
  ; Needs checking?
;  let z ((perc-group2 * y) + ((100 - perc-group2) * x)) / 100
;  report ( (1 - n * delta) * y + n * delta * y * payoff-for-B-move-given-perc 1 z ) /
;  ( (1 - n * delta) + n * delta * ((y * payoff-for-B-move-given-perc 1 z) + ((1 - y) * payoff-for-B-move-given-perc 0 z)) )

  ; If scored on games against other group (which occur at a rate dependent on proportion of population in that group.)
  report ( (1 - n * delta * ((100 - perc-group2) / 100)) * y + n * delta * ((100 - perc-group2) / 100) * y * payoff-for-B-move-given-perc 1 x ) /
  ( (1 - n * delta * ((100 - perc-group2) / 100)) + n * delta * ((100 - perc-group2) / 100) * ((y * payoff-for-B-move-given-perc 1 x) + ((1 - y) * payoff-for-B-move-given-perc 0 x)) )
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

  set num-pops-with-group1-dom count populations with [x > y]
  set num-pops-with-group2-dom count populations with [x < y]
  set num-pops-with-groups-equal count populations with [x = y]
  set group1-payoff mean [first payoffs-xy x y] of populations
  set group2-payoff mean [first payoffs-xy y x] of populations

  if Recolor-Populations? [recolor-pops-by-xy]
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick setup of parameters for reproducing various models.

to setup-model
  let scen []

  if model = "Amadae" [set scen model-amadae]
  if model = "Amadae + Prior Memory" [set scen model-amadae-prior-memory]
  if model = "Amadae + Prior Limited Memory" [set scen model-amadae-prior-limited-memory]
  if model = "Bergstrom & Lachmann" [set scen model-bergstrom-lachmann]
  if model = "B & L + Hawk & Dove" [set scen model-bergstrom-lachmann-HD]
  if model = "B & L + HD + Stochastics" [set scen model-bergstrom-lachmann-HD-stochastic]
  if scen = [] [user-message "FATAL! Didn't recognise value of parameter \"Model\". See procedure \"Setup-Model\" to investigate (e.g. typos?)" stop]

  foreach first scen [a ->
    ifelse is-string? (last a) [
      run (word "set " (first a) " \"" (last a) "\"")
    ]
    [
      run (word "set " (first a) " " (last a))
    ]
  ]
  run last scen
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report model-amadae
  ; Amadae's cultural evolution model (in preparation).
  report (list
    (list
      ["Game" "Hawk-Dove"]
      ["Rounds" 200]
      ["Population-Size" 200]
      ["Perc-Group2" 80]
      ["Opponents-Include-Own-Group?" true]
      ["Playing-Strategy" "Amadae"]
      ["Initial-Populations" "11x11 Evenly Spaced"]
;      ["Initial-X" 10]
;      ["Initial-Y" 10]
      ["Memory-Initialization" "Empty"]
      ["Memory-Initial-Weight-1" 10]
      ["Memory-Initial-Weight-2" 10]
;      ["Memory-Length-1" 10]
;      ["Memory-Length-2" 10]
      ["Unlimited-Memory?" true]
      ["Playing-Noise" 0]
      ["Replicate?" false]
      ["Replication-Noise" 0]
      ["Speed-1" 10]
      ["Speed-2" 10]
      ["Delta" 0.1]
      ["Value" 10]
      ["Value-As-Perc-Of-Cost" 10]
      ["Punishment" 1]
      ["Reward" 3]
      ["Sucker" 0]
      ["Temptation" 5]
      ["k" 1.5]
      ["My-Preference" 40]
      ["Your-Preference" 20]
      ["Draw-X-And-Y-Axes?" true]
      ["Reposition-Populations" "By Mean Belief"]
      ["Population-Pen-Down?" false]
      ["Recolor-Populations?" false]
      ["Player-Pen-Down?" false]
      ["Hide-Populations?" false]
      ["Hide-Players?" true]
    )
    "setup-stochastic-sim"
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report model-amadae-prior-memory
  ; Amadae's rules, but players have prior memories.
  ; Group 2 has more prior memory than Group 1,
  ; and so will learn / update slower.

  report (list
    (list
      ["Game" "Hawk-Dove"]
      ["Rounds" 200]
      ["Population-Size" 200]
      ["Perc-Group2" 80]
      ["Opponents-Include-Own-Group?" true]
      ["Playing-Strategy" "Play-MSNE"]
      ["Initial-Populations" "11x11 Evenly Spaced"]
;      ["Initial-X" 10]
;      ["Initial-Y" 10]
      ["Memory-Initialization" "Fixed-Proportion-xy"]
      ["Memory-Initial-Weight-1" 10]
      ["Memory-Initial-Weight-2" 10]
;      ["Memory-Length-1" 10]
;      ["Memory-Length-2" 10]
      ["Unlimited-Memory?" true]
      ["Playing-Noise" 0]
      ["Replicate?" false]
      ["Replication-Noise" 0]
      ["Speed-1" 10]
      ["Speed-2" 10]
      ["Delta" 0.1]
      ["Value" 10]
      ["Value-As-Perc-Of-Cost" 10]
      ["Punishment" 1]
      ["Reward" 3]
      ["Sucker" 0]
      ["Temptation" 5]
      ["k" 1.5]
      ["My-Preference" 40]
      ["Your-Preference" 20]
      ["Draw-X-And-Y-Axes?" true]
      ["Reposition-Populations" "By Mean Belief"]
      ["Population-Pen-Down?" true]
      ["Recolor-Populations?" false]
      ["Player-Pen-Down?" false]
      ["Hide-Populations?" false]
      ["Hide-Players?" true]
    )
    "setup-stochastic-sim"
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report model-amadae-prior-limited-memory
  ; Amadae's rules, but players have prior memories.
  ; Group 2 has more prior memory than Group 1,
  ; and so will learn / update slower.

  report (list
    (list
      ["Game" "Hawk-Dove"]
      ["Rounds" 200]
      ["Population-Size" 200]
      ["Perc-Group2" 80]
      ["Opponents-Include-Own-Group?" true]
      ["Playing-Strategy" "Play-MSNE"]
      ["Initial-Populations" "11x11 Evenly Spaced"]
;      ["Initial-X" 10]
;      ["Initial-Y" 10]
      ["Memory-Initialization" "Random-xy"]
      ["Memory-Initial-Weight-1" 10]
      ["Memory-Initial-Weight-2" 10]
      ["Memory-Length-1" 10]
      ["Memory-Length-2" 10]
      ["Unlimited-Memory?" false]
      ["Playing-Noise" 0]
      ["Replicate?" false]
      ["Replication-Noise" 0]
      ["Speed-1" 10]
      ["Speed-2" 10]
      ["Delta" 0.1]
      ["Value" 10]
      ["Value-As-Perc-Of-Cost" 10]
      ["Punishment" 1]
      ["Reward" 3]
      ["Sucker" 0]
      ["Temptation" 5]
      ["k" 1.5]
      ["My-Preference" 40]
      ["Your-Preference" 20]
      ["Draw-X-And-Y-Axes?" true]
      ["Reposition-Populations" "By Mean Belief"]
      ["Population-Pen-Down?" true]
      ["Recolor-Populations?" false]
      ["Player-Pen-Down?" false]
      ["Hide-Populations?" false]
      ["Hide-Players?" true]
    )
    "setup-stochastic-sim"
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report model-bergstrom-lachmann
  ; The equation-based model of Bergstrom & Lachmann (2003).
  ; We've given Group 2 faster updating than Group 1,
  ; but made group sizes equal.
  report (list
    (list
;      ["Game" "Hawk-Dove"]
      ["Game" "Mutualism"]
      ["Rounds" 200]
      ["Population-Size" 200]
      ["Perc-Group2" 50]
      ["Opponents-Include-Own-Group?" false]
;      ["Playing-Strategy" "Play-MSNE"]
      ["Initial-Populations" "11x11 Evenly Spaced"]
;      ["Initial-X" 10]
;      ["Initial-Y" 10]
;      ["Memory-Initialization" "Empty"]
;      ["Memory-Initial-Weight-1" 10]
;      ["Memory-Initial-Weight-2" 10]
;      ["Memory-Length-1" 10]
;      ["Memory-Length-2" 10]
;      ["Unlimited-Memory?" true]
;      ["Playing-Noise" 0]
;      ["Replicate?" false]
;      ["Replication-Noise" 0]
      ["Speed-1" 10]
      ["Speed-2" 30]
      ["Delta" 0.1]
      ["Value" 10]
      ["Value-As-Perc-Of-Cost" 10]
      ["Punishment" 1]
      ["Reward" 3]
      ["Sucker" 0]
      ["Temptation" 5]
      ["k" 1.5]
      ["My-Preference" 40]
      ["Your-Preference" 20]
      ["Draw-X-And-Y-Axes?" true]
      ["Reposition-Populations" "By Mean Action"]
      ["Population-Pen-Down?" true]
      ["Recolor-Populations?" false]
      ["Player-Pen-Down?" false]
      ["Hide-Populations?" false]
      ["Hide-Players?" true]
    )
    "Replicator-Dynamics-By-Equation"
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report model-bergstrom-lachmann-hd
  ; The equation-based model of Bergstrom & Lachmann (2003),
  ; but applied to Hawk & Dove, instead of mutualism.
  ; We've given Group 2 faster updating than Group 1,
  ; but made group sizes equal.
  report (list
    (list
      ["Game" "Hawk-Dove"]
      ["Rounds" 200]
      ["Population-Size" 200]
      ["Perc-Group2" 50]
      ["Opponents-Include-Own-Group?" false]
;      ["Playing-Strategy" "Play-MSNE"]
      ["Initial-Populations" "11x11 Evenly Spaced"]
;      ["Initial-X" 10]
;      ["Initial-Y" 10]
;      ["Memory-Initialization" "Empty"]
;      ["Memory-Initial-Weight-1" 10]
;      ["Memory-Initial-Weight-2" 10]
;      ["Memory-Length-1" 10]
;      ["Memory-Length-2" 10]
;      ["Unlimited-Memory?" true]
;      ["Playing-Noise" 0]
;      ["Replicate?" false]
;      ["Replication-Noise" 0]
      ["Speed-1" 10]
      ["Speed-2" 30]
      ["Delta" 0.1]
      ["Value" 10]
      ["Value-As-Perc-Of-Cost" 10]
      ["Punishment" 1]
      ["Reward" 3]
      ["Sucker" 0]
      ["Temptation" 5]
      ["k" 1.5]
      ["My-Preference" 40]
      ["Your-Preference" 20]
      ["Draw-X-And-Y-Axes?" true]
      ["Reposition-Populations" "By Mean Action"]
      ["Population-Pen-Down?" true]
      ["Recolor-Populations?" false]
      ["Player-Pen-Down?" false]
      ["Hide-Populations?" false]
      ["Hide-Players?" true]
    )
    "Replicator-Dynamics-By-Equation"
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report model-bergstrom-lachmann-hd-stochastic
  ; Similar model to Bergstrom & Lachmann (2003),
  ; but representing copies of strategies explicitly,
  ; and sampling randomly to generate new generations.
  ; Also: applied to Hawk & Dove, instead of mutualism.
  ; We've given Group 2 faster updating than Group 1,
  ; but made group sizes equal.
  report (list
    (list
      ["Game" "Hawk-Dove"]
      ["Rounds" 2000]
      ["Population-Size" 200]
      ["Perc-Group2" 50]
      ["Opponents-Include-Own-Group?" false]
      ["Playing-Strategy" "Last Action"]
      ["Initial-Populations" "11x11 Evenly Spaced"]
;      ["Initial-X" 10]
;      ["Initial-Y" 10]
;      ["Memory-Initialization" "Empty"]
;      ["Memory-Initial-Weight-1" 10]
;      ["Memory-Initial-Weight-2" 10]
;      ["Memory-Length-1" 10]
;      ["Memory-Length-2" 10]
;      ["Unlimited-Memory?" true]
;      ["Playing-Noise" 0]
      ["Replicate?" true]
      ["Replication-Noise" 0]
      ["Speed-1" 10]
      ["Speed-2" 30]
      ["Delta" 0.1]
      ["Value" 10]
      ["Value-As-Perc-Of-Cost" 10]
      ["Punishment" 1]
      ["Reward" 3]
      ["Sucker" 0]
      ["Temptation" 5]
      ["k" 1.5]
      ["My-Preference" 40]
      ["Your-Preference" 20]
      ["Draw-X-And-Y-Axes?" true]
      ["Reposition-Populations" "By Mean Action"]
      ["Population-Pen-Down?" false]
      ["Recolor-Populations?" false]
      ["Player-Pen-Down?" false]
      ["Hide-Populations?" false]
      ["Hide-Players?" true]
    )
    "setup-stochastic-sim"
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@#$#@#$#@
GRAPHICS-WINDOW
250
10
642
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
300
110
360
Population-Size
200.0
1
0
Number

SLIDER
10
210
182
243
Perc-Group2
Perc-Group2
0
100
80.0
5
1
%
HORIZONTAL

INPUTBOX
10
700
70
760
Value
10.0
1
0
Number

MONITOR
95
700
230
745
D-D: [V/2 V/2]
made-neat Payoffs-Hawk-Dove 0 0
17
1
11

MONITOR
235
700
370
745
D-H: [0 V]
made-neat Payoffs-Hawk-Dove 0 1
17
1
11

MONITOR
95
750
230
795
H-D: [V 0]
made-neat Payoffs-Hawk-Dove 1 0
17
1
11

MONITOR
235
750
370
795
H-H: [(V-C)/2 (V-C)/2]
made-neat Payoffs-Hawk-Dove 1 1
17
1
11

TEXTBOX
15
585
170
610
Hawk-Dove Payoffs:
16
0.0
1

MONITOR
290
600
370
645
Value / Cost
value / cost
5
1
11

SLIDER
10
610
202
643
Value-As-Perc-Of-Cost
Value-As-Perc-Of-Cost
0
100
80.0
5
1
%
HORIZONTAL

MONITOR
205
600
285
645
Cost (C)
Cost
3
1
11

MONITOR
100
250
187
295
Group2 (Red)
Perc-group2 * population-size / 100
1
1
11

MONITOR
10
250
97
295
Group1 (Blue)
(100 - Perc-Group2) * population-size / 100
1
1
11

BUTTON
655
315
795
348
Setup for Equations
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
825
215
905
275
Delta
0.1
1
0
Number

CHOOSER
10
160
240
205
Game
Game
"Hawk-Dove" "Prisoner's Dilemma" "Mutualism" "Donation" "Hawk-Dove Optimal"
0

SLIDER
455
645
627
678
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
455
685
512
730
G-G
made-neat Payoffs-mutualism 0 0
2
1
11

MONITOR
515
685
572
730
G-S
made-neat Payoffs-mutualism 0 1
2
1
11

MONITOR
455
735
512
780
S-G
made-neat Payoffs-mutualism 1 0
2
1
11

MONITOR
515
735
572
780
S-S
made-neat Payoffs-mutualism 1 1
2
1
11

TEXTBOX
460
610
635
635
Mutualism Payoffs:
16
0.0
1

BUTTON
865
315
927
348
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
800
355
862
388
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
865
355
932
388
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
800
315
862
348
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
130
707
175
Mouse-X
ifelse-value mouse-inside? [mouse-xcor / x-max] [\"\"]
3
1
11

MONITOR
710
130
767
175
Mouse-Y
ifelse-value mouse-inside? [mouse-ycor / y-max] [\"\"]
3
1
11

TEXTBOX
650
190
940
208
Replicator Dynamics (Equation or Stochastic):
14
0.0
1

BUTTON
1140
415
1202
448
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
415
1070
448
Setup ABM
Setup-Stochastic-Sim
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
1075
415
1137
448
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

SWITCH
755
470
932
503
Population-Pen-Down?
Population-Pen-Down?
1
1
-1000

SLIDER
1135
360
1307
393
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
460
1247
493
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
365
242
398
Opponents-Include-Own-Group?
Opponents-Include-Own-Group?
0
1
-1000

SLIDER
650
215
822
248
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
650
250
822
283
Speed-2
Speed-2
0
100
10.0
5
1
%
HORIZONTAL

INPUTBOX
665
600
745
660
Reward
3.0
1
0
Number

INPUTBOX
745
600
825
660
Sucker
0.0
1
0
Number

INPUTBOX
665
660
745
720
Temptation
5.0
1
0
Number

INPUTBOX
745
660
825
720
Punishment
1.0
1
0
Number

MONITOR
845
600
912
645
C-C: [R R]
made-neat Payoffs-Prisoners-Dilemma 0 0
17
1
11

MONITOR
845
650
912
695
D-C: [T S]
made-neat Payoffs-Prisoners-Dilemma 1 0
17
1
11

MONITOR
915
600
982
645
C-D: [S T]
made-neat Payoffs-Prisoners-Dilemma 0 1
17
1
11

MONITOR
915
650
982
695
D-D: [P P]
made-neat Payoffs-Prisoners-Dilemma 1 1
17
1
11

TEXTBOX
665
570
890
595
Prisoner's Dilemma Payoffs:
16
0.0
1

TEXTBOX
455
785
580
831
0 = G = Generous\n1 = S = Selfish\n
13
0.0
1

TEXTBOX
850
700
1000
731
0 = C = Cooperate\n1 = D = Defect
13
0.0
1

TEXTBOX
290
655
400
690
1 = H = Hawk\n0 = D = Dove
13
0.0
1

TEXTBOX
10
850
160
886
Donation Payoffs (Alternative PD):
16
0.0
1

MONITOR
170
850
260
895
C-C: [V-C V-C]
made-neat Payoffs-donation 0 0
17
1
11

MONITOR
265
850
355
895
C-D:  [-C V]
made-neat Payoffs-donation 0 1
17
1
11

MONITOR
170
900
260
945
D-C: [V -C]
made-neat Payoffs-donation 1 0
17
1
11

MONITOR
265
900
355
945
D-D: [0 0]
made-neat Payoffs-donation 1 1
17
1
11

TEXTBOX
10
900
160
931
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
1295
220
1405
280
Memory-Length-1
10.0
1
0
Number

CHOOSER
955
360
1122
405
Playing-Strategy
Playing-Strategy
"Play-MSNE" "Expect-MSNE" "Play-Random" "Expect-D-D" "Expect-H-H" "Expect-H-0.5" "Play-H-D" "Play-D-H" "Play-Initial-X-Y" "Expect-Initial-X-Y" "Last Action" "MSNE" "Memory" "Stochastic Memory" "MSNE / Stoch Memory"
0

MONITOR
10
650
197
695
Mixed-Strategy Nash Equilibrium
MSNE
5
1
11

MONITOR
200
650
282
695
NIL
MSNE-Payoff
3
1
11

MONITOR
760
30
867
75
G2 > G1 : (y > x)
num-pops-with-group2-dom
17
1
11

MONITOR
650
30
757
75
G1 > G2 : (x > y)
num-pops-with-group1-dom
17
1
11

SWITCH
955
460
1067
493
Replicate?
Replicate?
1
1
-1000

TEXTBOX
655
295
805
313
Use Equations:
14
0.0
1

TEXTBOX
960
95
1135
113
Use Agent-Based Simuation:
14
0.0
1

MONITOR
770
130
885
175
Mouse-Payoffs
mouse-payoffs
17
1
11

CHOOSER
10
405
167
450
Reposition-Populations
Reposition-Populations
"By Mean Action" "By Mean Belief" "By Mean Out Action"
2

BUTTON
170
410
270
443
Reposition Now
reposition-pops\nreposition-players
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
625
425
742
458
By Payoff to G1
Color-Patches-By-Payoff true
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
745
425
862
458
By Payoff to G2
Color-Patches-By-Payoff false
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
865
425
950
458
White
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
115
300
190
360
Rounds
2000.0
1
0
Number

BUTTON
10
120
117
153
Setup Model
Setup-model
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
950
30
1062
75
NIL
Count Populations
17
1
11

MONITOR
870
30
935
75
G1 = G2
num-pops-with-groups-equal
17
1
11

TEXTBOX
280
410
555
591
Key:\nAxes range from 0 (all Dove) to 1 (all Hawk).\nMean-Belief:\nx represents Group 2's beliefs about Group 1.\ny represents Group 1's beliefs about Group 2.\nMean-Action:\nx represents Group 1's most recent action.\ny represents Group 2's most recent action.\nUnhidden Players:\nx represents a Group 1 player's action history, and a Group 2 player's beliefs about Group 1.\ny represents a Group 2 player's action history, and a Group 1 player's beliefs about Group 2.\n
11
0.0
1

CHOOSER
955
170
1130
215
Memory-Initialization
Memory-Initialization
"Empty" "Random-50:50" "Random-MSNE" "Random-xy" "Fixed-Proportion-50:50" "Fixed-Proportion-MSNE" "Fixed-Proportion-xy"
0

INPUTBOX
955
220
1107
280
Memory-Initial-Weight-1
10.0
1
0
Number

SWITCH
585
470
752
503
Recolor-Populations?
Recolor-Populations?
1
1
-1000

INPUTBOX
1110
220
1262
280
Memory-Initial-Weight-2
10.0
1
0
Number

SLIDER
680
790
852
823
My-Preference
My-Preference
0
100
40.0
5
1
NIL
HORIZONTAL

SLIDER
680
825
852
858
Your-Preference
Your-Preference
0
100
20.0
5
1
NIL
HORIZONTAL

TEXTBOX
680
760
945
785
Battle of the Sexes Payoffs:
16
0.0
1

MONITOR
880
800
945
845
B's Pref
made-neat Payoffs-Battle-Of-The-Sexes 0 0
17
1
11

MONITOR
950
800
1015
845
Missed
made-neat Payoffs-Battle-Of-The-Sexes 0 1
17
1
11

MONITOR
880
850
945
895
Missed
made-neat Payoffs-Battle-Of-The-Sexes 1 0
17
1
11

MONITOR
950
850
1015
895
A's Pref
made-neat Payoffs-Battle-Of-The-Sexes 1 1
17
1
11

TEXTBOX
685
870
885
915
0 = I opt for B's ideal.\n1 = I opt for A's ideal.
13
0.0
1

TEXTBOX
930
780
990
798
B's options:
11
0.0
1

TEXTBOX
860
835
875
853
A's
11
0.0
1

TEXTBOX
955
285
1285
355
Initial memory weights:\nThe more past memory a group's members have, the less influence new evidence has on their beliefs.\nIf Group 1 has higher weight than Group 2, Group 1 players update their beliefs (about Group 2's actions and shown on y-axis) slower.
11
0.0
1

TEXTBOX
655
405
805
423
Colour Patches:
13
0.0
1

TEXTBOX
650
90
885
136
Identify positions in state space using the mouse pointer:
13
0.0
1

TEXTBOX
650
10
800
28
Estimate Domination:
13
0.0
1

TEXTBOX
875
760
1025
778
(Not currently implemented.)
11
0.0
1

TEXTBOX
585
505
770
546
Recolor populations based on payoff received from playing x against y.\nLighter color = Higher payoff.
11
0.0
1

SWITCH
10
470
182
503
Draw-X-And-Y-Axes?
Draw-X-And-Y-Axes?
0
1
-1000

CHOOSER
10
70
240
115
Model
Model
"Bergstrom & Lachmann" "Amadae" "B & L + Hawk & Dove" "B & L + HD + Stochastics" "Amadae + Prior Memory" "Amadae + Prior Limited Memory"
1

MONITOR
1080
40
1172
85
NIL
Group1-Payoff
1
1
11

MONITOR
1175
40
1267
85
NIL
Group2-Payoff
1
1
11

TEXTBOX
1085
10
1265
36
Average over Populations of Average over Group Members:
11
0.0
1

CHOOSER
955
120
1130
165
Initial-Populations
Initial-Populations
"11x11 Evenly Spaced" "6x6 Evenly Spaced" "1 at Initial-X/Y" "10 at Initial-X/Y" "100 at Initial-X/Y" "1 at MSNE" "10 at MSNE" "100 at MSNE"
4

SLIDER
1135
120
1307
153
Initial-X
Initial-X
0
100
50.0
5
1
%
HORIZONTAL

SLIDER
1135
155
1307
188
Initial-Y
Initial-Y
0
100
50.0
5
1
%
HORIZONTAL

SWITCH
1280
415
1435
448
Hide-Populations?
Hide-Populations?
0
1
-1000

SWITCH
1280
450
1435
483
Hide-Players?
Hide-Players?
0
1
-1000

SWITCH
775
505
932
538
Player-Pen-Down?
Player-Pen-Down?
1
1
-1000

SWITCH
1295
285
1452
318
Unlimited-Memory?
Unlimited-Memory?
0
1
-1000

INPUTBOX
1410
220
1520
280
Memory-Length-2
10.0
1
0
Number

BUTTON
1285
545
1382
578
NIL
Print-Beliefs
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1285
580
1477
613
Print-Out-Match-Results?
Print-Out-Match-Results?
1
1
-1000

SWITCH
1280
485
1417
518
Hide-Statistics?
Hide-Statistics?
1
1
-1000

SWITCH
1285
615
1422
648
Print-Statistics?
Print-Statistics?
1
1
-1000

SWITCH
1460
505
1617
538
Draw-MSNE-Lines?
Draw-MSNE-Lines?
0
1
-1000

SWITCH
1460
545
1602
578
Draw-Diagonal?
Draw-Diagonal?
0
1
-1000

INPUTBOX
1110
785
1255
845
Seed-Setup
0.0
1
0
Number

INPUTBOX
1110
845
1255
905
Seed-Go
0.0
1
0
Number

TEXTBOX
1110
760
1285
785
Random Number Seeds:
14
0.0
1

BUTTON
1255
795
1317
828
Clear
set seed-setup 0
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
1255
855
1317
888
Clear
set seed-go 0
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
1320
795
1422
828
Use Previous
set seed-setup prev-seed-setup
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
1320
855
1422
888
Use Previous
set seed-go prev-seed-go
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
1590
260
1732
293
NIL
Calc-Pop-Dominance
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
1535
355
1592
400
Group 1
Num-Pops-G1-Dominant
17
1
11

MONITOR
1595
355
1652
400
Group 2
Num-Pops-G2-Dominant
17
1
11

MONITOR
1655
355
1712
400
Neither
Num-Pops-None-Dominant
17
1
11

SWITCH
1535
405
1712
438
Calculate-Dominance?
Calculate-Dominance?
0
1
-1000

BUTTON
1535
440
1682
473
Print Round Dominant
foreach sorted-populations [po -> print [po-round-dominant] of po]
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
1720
355
1777
400
Group 1
num-pops-g1-hawkish
17
1
11

MONITOR
1780
355
1837
400
Group 2
num-pops-g2-hawkish
17
1
11

MONITOR
1840
355
1897
400
Neither
num-pops-none-hawkish
17
1
11

TEXTBOX
1720
330
1870
348
All H vs All D:
13
0.0
1

TEXTBOX
1535
300
1685
346
Dominance:\n(All Expect < V/C vs All Expect > V/C)
13
0.0
1

BUTTON
1720
440
1897
473
NIL
Print-Dominance-Errors
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
1720
480
1902
513
NIL
File-Print-Dominance-Errors
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
# Evolving Game Players

## WHAT IS IT?

Simulates population dynamics for evolving populations of game players. Games are defined by their payoff function. The following games are included:

* __Hawk & Dove__
* __Prisoner's Dilemma__ (with actions "Cooperate" and "Defect")
* __Donation__ (a special case of Prisoner's Dilemma, but with same parameters as Hawk-Dove)
* __Mutualism__ (with actions "Selfish" and "Generous", also known as a Mixed-Motives Game)

Population dynamics can be studied using equations describing a theoretical population, or using an agent-based simulation of actual players, with explicit representation of the individual players being replicated. 

In the agent-based simulation, player agents can:

* either play the same action each match, after which players are sampled with preference for payoffs won to create a new generation, 
* or play a strategy based on a stochastic rule (e.g. the Mixed-Strategy Nash Equilibrium, MSNE) and/or their memories of past matches with members of the opposite group, after which players update their memories with the new match information.

This program was developed to illustrate the model of Bergstrom & Lachmann (2003), and related work by Amadae. (See references below.)

To switch between different models and their parameter settings, use the Chooser "Model" and click "Setup Model". Models include:

* __Bergstrom & Lachmann__ : Based on their 2003 paper. An equation-based model of population dynamics. Each item in the population is a one-move strategy for the binary game, "Mutualism". Two groups of strategies (representing "species") are given different speeds of evolution. (See _Speed-1_ and _Speed-2_.)
* __Amadae__ : An agent-based model of cultural learning, based on ideas from Amadae (2020 and in preparation), and applied to the Hawk & Dove game. Following Axtell et al. (2000), player agents maintain memories of past matches played against members of their opposite group. Against members of their own group, they play the Mixed-Strategy Nash Equilibrium. Against members of the other group they play whichever move will deliver the maximum expected payoff to themselves. A player's expectation (belief) is based on that memory of past matches. Players are initialised with empty memories, and play MSNE until they have experienced a match against a member of the other group. The two groups of players differ in size. (See _Perc-Group2_).
* __B & L + Hawk & Dove__ : The Bergstrom & Lachmann model applied to the Hawk & Dove game.
* __B & L + HD + Stochastics__ : Similar scenario to \"B & L + Hawk & Dove\", but instead of updating populations with equations, individual copies of strategies are explicitly represented, and updates are performed by random sampling, weighted by payoff received.
* __Amadae + Prior Memory__ : An ABM where players with memories apply Amadae's rules and Bayesian updating of memory, but instead of being initialised with empty memories, these players are given prior beliefs. The two groups have the same size (_Perc-Group2_ = 50%), but differ in the numbers of prior memories. (See _Memory-Initial-Weight1_ and _Memory-Initial-Weight2_.)

This program (C) Christopher J. Watts, 2021.

## HOW IT WORKS

The NetLogo world is used to draw a graph representing the state space for the games. Turtles are used to represent Populations of Players. Each population is divided into two groups (representing, e.g., species). Each player will have a strategy that determines which of two possible actions that player plays. (Games with more than two possible actions are not currently covered.) 

The population turtle has x and y coordinates. There are currently two distinct uses of these coordinates, as chosen by the parameter _Reposition-Populations_:

* __By Mean Action__ : The __x__ coordinate represents the proportion of __group 1__ who play the action denoted "1". (So for Hawk & Dove, x = the proportion of group 1 who play "Hawk".) The __y__ coordinate represents the proportion of __group 2__ who play "1". 
* __By Mean Out Action__ : Like "Mean Action", but restricted to interactions between agents of different groups (i.e. actions played against an out-group).
* __By Mean Belief__ : The __x__ coordinate represents the average belief among ___group 2___ players, the __y__ coordinate the average belief among ___group 1___ members. A belief is the proportion of actions in your memory made by _members of the other group_. __So x represents Group 2's belief about Group 1's actions.__

If the relevant proportions change, the position of the Population turtle will change. If the turtle's pen is down, a line is drawn tracing the population's evolution through the state space. Population turtles can be created at multiple points in the state space, representing different proportions of "1"-players (or "1"-memories) in groups 1 and 2. Thus we can study the evolution that follows from various initial positions.

Multiple ways of evolving a population are included:

* __Replicator Dynamics By Equation__ : This uses equations to simulate the dynamics according to replicator theory.
* __Replicator Dynamics By ABM__ : For each population, this creates a given number of players, and divides them between the two groups and the two strategies. Actual one-round games are then simulated between random pairs of players. The payoffs from these are used to sample for each group a number of "fit" players. Their strategies are then used to replace those held by randomly chosen players. The new proportions of "1"-players then determine the new x and y coordinates of the Population turtle. Dynamics will be stochastic, but should resemble those of the equation-based Draw-Replicator-Dynamics.
* __Cultural Learning By ABM__ : or each population, this creates a given number of players, and divides them between the two groups and the two strategies. Actual one-round games are then simulated between random pairs of players. A number of possible decision rules are provided, including playing a Mixed-Strategy Nash Equilibrium (MSNE), and playing the action with the maximum expected payoff, where expectations (beliefs) are based on memories of past matches against members of the other group to oneself. Beliefs are updated after the match, so dynamics need not come from replication and fitness/payoffs.

## HOW TO USE IT

Choose which game you want to study. Review the corresponding parameters and Payoffs Table for that game (located below the NetLogo world). 

Then choose a method of analysis, and click the corresponding Setup button. 

Two graph axes will be drawn. Populations will be created at different positions in the NetLogo world, representing different proportions of "1"-players in groups 1 (x-axis) and 2 (y-axis). Population then move in this state space, according to the evolving proportions of "1"-players in the two groups.

The parameter __Opponents-Include-Own-Group?__ determines whether players play only members of the other group (Off) or play any players, including from their own group (On). This can have a big affect on the shape of the state space. N.B. This parameter does not affect replication: a player can only replace their strategy with one sampled from their own group.

## THINGS TO NOTICE

Most populations will converge on a small number of attractor points (representing equilibria). E.g. the Prisoner's Dilemma has one Nash Equilibrium (Defect-Defect, the top right-hand corner). Hawk-Dove has three attractors: (x=1, y=0), (x=0, y=1), and a single point, defined by the ratio between parameters Value (V) and Cost (C), at (x=V/C, y=V/C).

The populations in the stochastic replicator model will tend to follow the equation-based model, but with some random variation. However, if a group loses its last player of a particular strategy, the population then becomes stuck on a line (e.g. the line x=1 when group 1 has lost all its "0" players). Applying some Replication-Noise can reintroduce the missing strategy. Too much Replication-Noise, however, and the population might be unable to find the attractors at all.

## THINGS TO TRY

How do the game parameters affect the positions attractors and and relative sizes of their basins of attraction?

How do the evolution parameters (speed, delta, initial memory weight) affect the attractors and their basins?

How do the relative sizes of the two groups, and the size of the player population in the ABM, affect the attractors and their basins?

### The Red King and Red Queen Effects

For some games, and some parameter settings, you may observe one group tending to dominate the other, as the population converges to attractor where the two groups concentrate on opposite actions, and payoffs favour one group over another. (E.g. In Hawk & Dove, the dominant group focuses on Hawk, the other group on Dove.) Two contrasting forms of this outcome are:

* __The Red Queen Effect__ : Better payoffs go to the faster evolving group.
* __The Red King Effect__ : Better payoffs go to the slower evolving group.

Factors that can alter the relative speeds of evolution of groups include:

* __Perc-Group2__ : The percentage of each population who are in Group 2 rather than Group 1. The larger group provides more information for the opposing group to learn from.
* __Speed-1 and Speed-2__ : These parameters control how much of the population is updated during replication (in both equation-based and stochastic simulation.)
* __Memory-Initial-Weight1 and Memory-Initial-Weight2__ : In the cultural learning ABM, agents can be given initial memories (prior probabilities). If these are large, then new individual simulated matches do not have much effect on the player's beliefs. i.e. Bayesian belief updates are slower, the larger your memory.

In Hawk & Dove in a cultural learning ABM, changing from high to low Cost (via the slider Value-As-Perc-Cost), and hence changing the position of the MSNE, can reverse which of the Majority group (Red King Effect) or Minority group (Red Queen Effect) dominate.

## EXTENDING THE MODEL

* Use NetLogo3D (or a 2D transformation of 3D coordinates) to study games with three actions.
* Study more than two groups.
* Constrain players' interactions by organising players into social networks, and choosing their interaction partners from their neighbours in the network. See Hammond & Axelrod (2006) for why this might be interesting.

## NETLOGO FEATURES

Note the visible turtles represent populations of agents, not agents themselves. (The player agents are always hidden.) Our interest here is in studying population dynamics, not the course of particular sequences of matches, nor the diversity within particular populations.

## RELATED MODELS

Axelrod (1997, Ch.1-3) contains several simulation models based around the Prisoner's Dilemma.

## CREDITS AND REFERENCES

Amadae, S.M. (2020) "Binary Labels Reinforce Systemic Discrimination". Noema, November 17 2020. https://www.noemamag.com/binary-labels-reinforce-systemic-discrimination/

Axelrod, R. M. (1997). The complexity of cooperation : agent-based models of competition and collaboration. Princeton, N.J. ; Chichester: Princeton University Press.

Axtell, Robert L., Joshua M. Epstein, and H. Peyton Young (2001) "The emergence of
classes in a multiagent bargaining model." Social dynamics 27: 191-211.

Bergstrom, C. T., & Lachmann, M. (2003). The Red King effect: When the slowest runner wins the coevolutionary race. Proceedings of the National Academy of Sciences, 100(2), 593-598. doi:10.1073/pnas.0134966100

Bruner, J. P. (2019). Minority (dis)advantage in population games. Synthese, 196(1), 413-427. doi:10.1007/s11229-017-1487-8

Hammond, R. A., & Axelrod, R. (2006). The Evolution of Ethnocentrism. Journal of Conflict Resolution, 50(6), 926-936. doi:10.1177/0022002706293470

Hofbauer, J., & Sigmund, K. (1998). Evolutionary games and population dynamics. Cambridge: Cambridge University Press.

O’Connor, C. (2017). The cultural Red King effect. The Journal of Mathematical Sociology, 41(3), 155-171. doi:10.1080/0022250X.2017.1335723

## 
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
  <experiment name="experiment-Amadae" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup-stochastic-sim</setup>
    <go>go-sim</go>
    <metric>timer</metric>
    <metric>prev-seed-setup</metric>
    <metric>prev-seed-go</metric>
    <metric>count populations</metric>
    <metric>num-pops-g1-dominant</metric>
    <metric>num-pops-g2-dominant</metric>
    <metric>num-pops-none-dominant</metric>
    <metric>num-pops-g1-hawkish</metric>
    <metric>num-pops-g2-hawkish</metric>
    <metric>num-pops-none-hawkish</metric>
    <metric>num-pops-with-group1-dom</metric>
    <metric>num-pops-with-group2-dom</metric>
    <metric>num-pops-with-groups-equal</metric>
    <metric>Cost</metric>
    <metric>msne</metric>
    <metric>msne-payoff</metric>
    <metric>group1-payoff</metric>
    <metric>group2-payoff</metric>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Perc-Group2" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Strategy">
      <value value="&quot;Play-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Populations">
      <value value="&quot;100 at Initial-X/Y&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-X">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Y">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initialization">
      <value value="&quot;Empty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Unlimited-Memory?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Delta">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Value-As-Perc-Of-Cost" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Punishment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reward">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sucker">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temptation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="My-Preference">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Your-Preference">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-X-And-Y-Axes?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Belief&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Recolor-Populations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Calculate-Dominance?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Print-Out-Match-Results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-Amadae-EmptyMemoryRules" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup-stochastic-sim</setup>
    <go>go-sim</go>
    <metric>timer</metric>
    <metric>prev-seed-setup</metric>
    <metric>prev-seed-go</metric>
    <metric>count populations</metric>
    <metric>num-pops-g1-dominant</metric>
    <metric>num-pops-g2-dominant</metric>
    <metric>num-pops-none-dominant</metric>
    <metric>num-pops-g1-hawkish</metric>
    <metric>num-pops-g2-hawkish</metric>
    <metric>num-pops-none-hawkish</metric>
    <metric>num-pops-with-group1-dom</metric>
    <metric>num-pops-with-group2-dom</metric>
    <metric>num-pops-with-groups-equal</metric>
    <metric>Cost</metric>
    <metric>msne</metric>
    <metric>msne-payoff</metric>
    <metric>group1-payoff</metric>
    <metric>group2-payoff</metric>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Perc-Group2" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Strategy">
      <value value="&quot;Play-MSNE&quot;"/>
      <value value="&quot;Play-Random&quot;"/>
      <value value="&quot;Expect-D-D&quot;"/>
      <value value="&quot;Expect-H-H&quot;"/>
      <value value="&quot;Expect-H-0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Populations">
      <value value="&quot;100 at Initial-X/Y&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-X">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Y">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initialization">
      <value value="&quot;Empty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Unlimited-Memory?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Delta">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Value-As-Perc-Of-Cost" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Punishment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reward">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sucker">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temptation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="My-Preference">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Your-Preference">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-X-And-Y-Axes?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Belief&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Recolor-Populations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Calculate-Dominance?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Print-Out-Match-Results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-Amadae-PriorBeliefs" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup-stochastic-sim</setup>
    <go>go-sim</go>
    <metric>timer</metric>
    <metric>prev-seed-setup</metric>
    <metric>prev-seed-go</metric>
    <metric>count populations</metric>
    <metric>num-pops-g1-dominant</metric>
    <metric>num-pops-g2-dominant</metric>
    <metric>num-pops-none-dominant</metric>
    <metric>num-pops-g1-hawkish</metric>
    <metric>num-pops-g2-hawkish</metric>
    <metric>num-pops-none-hawkish</metric>
    <metric>num-pops-with-group1-dom</metric>
    <metric>num-pops-with-group2-dom</metric>
    <metric>num-pops-with-groups-equal</metric>
    <metric>Cost</metric>
    <metric>msne</metric>
    <metric>msne-payoff</metric>
    <metric>group1-payoff</metric>
    <metric>group2-payoff</metric>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Perc-Group2" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Strategy">
      <value value="&quot;Play-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Populations">
      <value value="&quot;100 at Initial-X/Y&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-X" first="0" step="10" last="100"/>
    <steppedValueSet variable="Initial-Y" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="Memory-Initialization">
      <value value="&quot;Fixed-Proportion-xy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Unlimited-Memory?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Delta">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Value-As-Perc-Of-Cost" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Punishment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reward">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sucker">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temptation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="My-Preference">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Your-Preference">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-X-And-Y-Axes?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Belief&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Recolor-Populations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Calculate-Dominance?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Print-Out-Match-Results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-Amadae-EmptyMemoryRules-LimitMemory" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup-stochastic-sim</setup>
    <go>go-sim</go>
    <metric>timer</metric>
    <metric>prev-seed-setup</metric>
    <metric>prev-seed-go</metric>
    <metric>count populations</metric>
    <metric>num-pops-g1-dominant</metric>
    <metric>num-pops-g2-dominant</metric>
    <metric>num-pops-none-dominant</metric>
    <metric>num-pops-g1-hawkish</metric>
    <metric>num-pops-g2-hawkish</metric>
    <metric>num-pops-none-hawkish</metric>
    <metric>num-pops-with-group1-dom</metric>
    <metric>num-pops-with-group2-dom</metric>
    <metric>num-pops-with-groups-equal</metric>
    <metric>Cost</metric>
    <metric>msne</metric>
    <metric>msne-payoff</metric>
    <metric>group1-payoff</metric>
    <metric>group2-payoff</metric>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Perc-Group2" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Strategy">
      <value value="&quot;Play-MSNE&quot;"/>
      <value value="&quot;Play-Random&quot;"/>
      <value value="&quot;Expect-D-D&quot;"/>
      <value value="&quot;Expect-H-H&quot;"/>
      <value value="&quot;Expect-H-0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Populations">
      <value value="&quot;100 at Initial-X/Y&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-X">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Y">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initialization">
      <value value="&quot;Empty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-1">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-2">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Unlimited-Memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Delta">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Value-As-Perc-Of-Cost" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Punishment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reward">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sucker">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temptation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="My-Preference">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Your-Preference">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-X-And-Y-Axes?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Belief&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Recolor-Populations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Calculate-Dominance?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Print-Out-Match-Results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-Amadae-InitialWeights" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup-stochastic-sim</setup>
    <go>go-sim</go>
    <metric>timer</metric>
    <metric>prev-seed-setup</metric>
    <metric>prev-seed-go</metric>
    <metric>count populations</metric>
    <metric>num-pops-g1-dominant</metric>
    <metric>num-pops-g2-dominant</metric>
    <metric>num-pops-none-dominant</metric>
    <metric>num-pops-g1-hawkish</metric>
    <metric>num-pops-g2-hawkish</metric>
    <metric>num-pops-none-hawkish</metric>
    <metric>num-pops-with-group1-dom</metric>
    <metric>num-pops-with-group2-dom</metric>
    <metric>num-pops-with-groups-equal</metric>
    <metric>Cost</metric>
    <metric>msne</metric>
    <metric>msne-payoff</metric>
    <metric>group1-payoff</metric>
    <metric>group2-payoff</metric>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Group2">
      <value value="50"/>
      <value value="60"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Strategy">
      <value value="&quot;Play-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Populations">
      <value value="&quot;100 at Initial-X/Y&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-X" first="0" step="10" last="100"/>
    <steppedValueSet variable="Initial-Y" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="Memory-Initialization">
      <value value="&quot;Fixed-Proportion-xy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-1">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Unlimited-Memory?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Delta">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Value-As-Perc-Of-Cost" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Punishment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reward">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sucker">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temptation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="My-Preference">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Your-Preference">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-X-And-Y-Axes?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Belief&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Recolor-Populations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Calculate-Dominance?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Print-Out-Match-Results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-Amadae-P05" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup-stochastic-sim</setup>
    <go>go-sim</go>
    <metric>timer</metric>
    <metric>prev-seed-setup</metric>
    <metric>prev-seed-go</metric>
    <metric>count populations</metric>
    <metric>num-pops-g1-dominant</metric>
    <metric>num-pops-g2-dominant</metric>
    <metric>num-pops-none-dominant</metric>
    <metric>num-pops-g1-hawkish</metric>
    <metric>num-pops-g2-hawkish</metric>
    <metric>num-pops-none-hawkish</metric>
    <metric>num-pops-with-group1-dom</metric>
    <metric>num-pops-with-group2-dom</metric>
    <metric>num-pops-with-groups-equal</metric>
    <metric>Cost</metric>
    <metric>msne</metric>
    <metric>msne-payoff</metric>
    <metric>group1-payoff</metric>
    <metric>group2-payoff</metric>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Perc-Group2" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Strategy">
      <value value="&quot;Expect-H-0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Populations">
      <value value="&quot;100 at Initial-X/Y&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-X">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Y">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initialization">
      <value value="&quot;Empty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Unlimited-Memory?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Delta">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Value-As-Perc-Of-Cost" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Punishment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reward">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sucker">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temptation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="My-Preference">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Your-Preference">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-X-And-Y-Axes?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Belief&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Recolor-Populations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Calculate-Dominance?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Print-Out-Match-Results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-Amadae-EmptyMemoryRules-LimitMemory-P05" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup-stochastic-sim</setup>
    <go>go-sim</go>
    <metric>timer</metric>
    <metric>prev-seed-setup</metric>
    <metric>prev-seed-go</metric>
    <metric>count populations</metric>
    <metric>num-pops-g1-dominant</metric>
    <metric>num-pops-g2-dominant</metric>
    <metric>num-pops-none-dominant</metric>
    <metric>num-pops-g1-hawkish</metric>
    <metric>num-pops-g2-hawkish</metric>
    <metric>num-pops-none-hawkish</metric>
    <metric>num-pops-with-group1-dom</metric>
    <metric>num-pops-with-group2-dom</metric>
    <metric>num-pops-with-groups-equal</metric>
    <metric>Cost</metric>
    <metric>msne</metric>
    <metric>msne-payoff</metric>
    <metric>group1-payoff</metric>
    <metric>group2-payoff</metric>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Perc-Group2" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Strategy">
      <value value="&quot;Expect-H-0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Populations">
      <value value="&quot;100 at Initial-X/Y&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-X">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Y">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initialization">
      <value value="&quot;Empty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-1">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-2">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Unlimited-Memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Delta">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Value-As-Perc-Of-Cost" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Punishment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reward">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sucker">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temptation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="My-Preference">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Your-Preference">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-X-And-Y-Axes?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Belief&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Recolor-Populations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Calculate-Dominance?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Print-Out-Match-Results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-RepDyn" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>Replicator-Dynamics-By-Equation</setup>
    <go>go-eqn</go>
    <metric>timer</metric>
    <metric>count populations</metric>
    <metric>num-pops-with-group1-dom</metric>
    <metric>num-pops-with-group2-dom</metric>
    <metric>num-pops-with-groups-equal</metric>
    <metric>Cost</metric>
    <metric>msne</metric>
    <metric>msne-payoff</metric>
    <metric>group1-payoff</metric>
    <metric>group2-payoff</metric>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Perc-Group2" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Populations">
      <value value="&quot;1 at Initial-X/Y&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-X" first="0" step="10" last="100"/>
    <steppedValueSet variable="Initial-Y" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Delta">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Value-As-Perc-Of-Cost" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Punishment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reward">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sucker">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temptation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="My-Preference">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Your-Preference">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-X-And-Y-Axes?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Action&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Recolor-Populations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-DomErrors" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup-stochastic-sim</setup>
    <go>go-sim</go>
    <final>file-print-dominance-errors</final>
    <metric>timer</metric>
    <metric>prev-seed-setup</metric>
    <metric>prev-seed-go</metric>
    <metric>count populations</metric>
    <metric>num-pops-g1-dominant</metric>
    <metric>num-pops-g2-dominant</metric>
    <metric>num-pops-none-dominant</metric>
    <metric>num-pops-g1-hawkish</metric>
    <metric>num-pops-g2-hawkish</metric>
    <metric>num-pops-none-hawkish</metric>
    <metric>num-pops-with-group1-dom</metric>
    <metric>num-pops-with-group2-dom</metric>
    <metric>num-pops-with-groups-equal</metric>
    <metric>Cost</metric>
    <metric>msne</metric>
    <metric>msne-payoff</metric>
    <metric>group1-payoff</metric>
    <metric>group2-payoff</metric>
    <enumeratedValueSet variable="Game">
      <value value="&quot;Hawk-Dove&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rounds">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Perc-Group2" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Opponents-Include-Own-Group?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Strategy">
      <value value="&quot;Play-MSNE&quot;"/>
      <value value="&quot;Play-Random&quot;"/>
      <value value="&quot;Expect-D-D&quot;"/>
      <value value="&quot;Expect-H-H&quot;"/>
      <value value="&quot;Expect-H-0.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Populations">
      <value value="&quot;100 at Initial-X/Y&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-X">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Y">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initialization">
      <value value="&quot;Empty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Initial-Weight-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory-Length-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Unlimited-Memory?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Playing-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replicate?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Replication-Noise">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Speed-2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Delta">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Value-As-Perc-Of-Cost" first="10" step="10" last="90"/>
    <enumeratedValueSet variable="Punishment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reward">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sucker">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Temptation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="My-Preference">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Your-Preference">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-X-And-Y-Axes?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reposition-Populations">
      <value value="&quot;By Mean Out Action&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Pen-Down?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Recolor-Populations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Calculate-Dominance?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Print-Out-Match-Results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
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
