:-consult('src/data/train_trials/rule3/train3.b').
eastbound(G) :- short(F), has_car(G,F), open(F).
