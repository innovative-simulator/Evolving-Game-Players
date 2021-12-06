# Evolving-Game-Players

This page brings together various programs exploring the evolutionary dynamics of game theory agents. There is currently one program in NetLogo, EvolvingGamePlayers.nlogo, that reproduces a variety of models, plus two Excel workbooks that provide simpler versions of the models.

## ReactiveLearningXL.xlsx

This implements a model by Amadae & Watts (in preparation), which builds on the cultural evolution models of Axtell et al. (2001), and O'Connor (2017). Whereas previous models have employed the Nash Demand Game, this applies the same agent decision mechanisms to the Hawk & Dove Binary Game.

Players belong to one of two groups. Players maintain memories of what moves have been played by their opponents in their other group, and use this information when planning their next move against a member of the other group. If the two groups differ in size, then the minority group will acquire these memories of the other, majority, group faster, and thus may differ from the majority in their ability to adapt. A "Red Queen effect" occurs when the faster adapting group comes to dominate. (In Hawk & Dove, a dominant group plays Hawk against its opponent's Dove.) A "Red King effect" occurs when the slower adapting group comes to dominate.

Players with memories can employ these memories as expectations about how an opponent from the other group will behave in a new match. Players choose the move with the maximum expected payoff. Players may be initialised with empty memories - in which case they choose a random first move. The probability of playing "Hawk" on this first move is then a parameter. E.g. one can set it to the Mixed Strategy Nash Equilibrium (MSNE). Alternatively, players can be initialised with memories of a number of past matches ("prior beliefs"), of which a given proportion saw opponents play Hawk. Larger prior memories weight the player's expectations towards the influence of the past, and reduce the influence of newly experienced matches. The two groups of players can differ in these initial weights and initial beliefs. Try starting at the (0.5, 0.5) position and compare weights = 10 with weights = 0 (the empty memory condition). Where do the two groups end up? Who dominates, the majority or the minority group? What effect does changing the payoff parameters (especially Value-As-%-Cost)?

NB: This is a stochastic simulation - its processes use random numbers. Hit the F9 key to force Excel to recalculate with a fresh stream of random numbers.

## ReplicatorDynamicsXL.xlsx

This is an extension of the replicator dynamics model of Bergstrom & Lachmann (2003). In this version, the game is the Hawk & Dove Binary Game. In addition, players can play against members of both their own group and the other group. Thus, two different-sized groups may differ in the rates with which they adapt to each other. Each group of players is represented by a single variable, denoting the proportion of players in that group playing "Hawk". Updates for each generation of players are performed deterministically according to replicator dynamics equations. 

## EvolvingGamePlayers.nlogo

EvolvingGamePlayers.nlogo is a NetLogo program for evolving populations of Game Theoretic players via replication and cultural learning.
Binary games covered include the Priosner's Dilemma, Hawk & Dove, and Mutualism. 

All programs (C) Christopher J. Watts, 2021. Note the licensing information.

Three types of evolutionary dynamics are provided within the same NetLogo program:
* __Equation-based replicator population dynamics__ : Populations of game strategies, divided into two groups (or "species") is updated using population dynamics equations to determine the relative proportions of strategies in the next generation.
* __Simulated replicator dynamics__ : Populations of game strategies updated using stochastic simulation (i.e. random sampling) to determine which strategies are replicated in the next generation. Each individual strategy copy is explicitly represented in the simulation. 
* __Simulated cultural learning__ : An agent-based model of players who a strategy based on their memories of past moves by opponents. The population of players is fixed, but dynamics come from the players updating their memories over time.

The equation-based replicator dynamics was included so as to reproduce the work described in Bergstrom & Lachmann (2003). The cultural learning ABM was included so as to reproduce the model by S. M. Amadae (2020) that, following Axtell et al. (2000), replaces the biology-inspired genetic evolution with a cultural mechanism. Common to all three mechanisms is the division of the population into two groups (representing "species" when the model is of biological evolution, and cultural groups in models for social sciences). In cultural models the division into two groups is taken to be based on some arbitirary feature (called "tags", e.g. hair colour, eye colour, etc.), with no basis in different skills or ability to play the game. The two groups may be of different sizes (leading them to be referred to as "the majority group" and "the minority group"), and may evolve at different speeds. Each group may contain players who vary in their strategies and the moves they make. The models reported in Bruner (2019) and O'Connor (2017) demonstrate situations in which the slow-adapting majority group come to dominate the minority ("the Red King Effect"). It has been suggested that cultural models might produce opposite behaviour. By including both replicator models and cultural model in the same program, we aim to explore whether this is true, and, if so, why. 

### How to use the model
Download and install NetLogo from https://ccl.northwestern.edu/netlogo/ . The model was developed in version 6.1.1 and tested also in 6.2.0.
Download the file EvolvingGamePlayers.nlogo from this github page (you may need to do something like Right-Click on the filename, and choose "Save target as..." from the menu.) Alternatively, you can install git on your computer and clone this repository.

On opening the model in NetLogo, the Info tab will contain more information and instructions.

## References
Amadae, S.M. (2020) “Binary Labels Reinforce Systemic Discrimination”. Noema, November 17 2020. https://www.noemamag.com/binary-labels-reinforce-systemic-discrimination/

Amadae, S.M. and Christopher J.  Watts, “Red Queen and Red King Effects in Cultural Agent-Based Modeling:  Hawk Dove Binary and Systemic Discrimination,” Journal of Mathematical Sociology, accepted, in press, Nov. 2021, DOI:  10.1080/0022250X.2021.2012668 .

Axtell, Robert L., Joshua M. Epstein, and H. Peyton Young (2001) "The emergence of
classes in a multiagent bargaining model." Social dynamics 27: 191-211.

Bergstrom, C. T., & Lachmann, M. (2003). The Red King effect: When the slowest runner wins the coevolutionary race. Proceedings of the National Academy of Sciences, 100(2), 593-598. doi:10.1073/pnas.0134966100

See also:
Axelrod, R. M. (1997). The complexity of cooperation : agent-based models of competition and collaboration. Princeton, N.J. ; Chichester: Princeton University Press.

Bruner, J. P. (2019). Minority (dis)advantage in population games. Synthese, 196(1), 413-427. doi:10.1007/s11229-017-1487-8

Hammond, R. A., & Axelrod, R. (2006). The Evolution of Ethnocentrism. Journal of Conflict Resolution, 50(6), 926-936. doi:10.1177/0022002706293470

Hofbauer, J., & Sigmund, K. (1998). Evolutionary games and population dynamics. Cambridge: Cambridge University Press.

O’Connor, C. (2017). The cultural Red King effect. The Journal of Mathematical Sociology, 41(3), 155-171. doi:10.1080/0022250X.2017.1335723

