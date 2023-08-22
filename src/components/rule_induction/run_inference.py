from pyswip import Prolog
import os
import subprocess

root_path = os.path.join(os.getcwd(), 'src/components/rule_induction/')
inference_file_path = os.path.join(root_path, 'dynamic_files/inference_file.pl')
output_path = os.path.join(root_path, "positive_preds.txt")

prolog = Prolog()
prolog.consult(inference_file_path)
pos_preds = list(prolog.query("eastbound(X)."))
print([list(pred.values())[0] for pred in pos_preds])