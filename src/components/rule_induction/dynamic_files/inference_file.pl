:-consult('src/data/train_trials/rule3/train3.b').
eastbound(F) :- has_car(F,G), short(G).
