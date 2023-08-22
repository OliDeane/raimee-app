from swiplserver import PrologMQI, PrologThread
import re

def replace_numeric_substring_clause(input_string):
    unique_substrings = {}
    output_strings = []

    # for input_string in strings:
    output_string = ""
    pattern = r'(_\d+)'
    matches = re.findall(pattern, input_string)

    for match in matches:
        if match in unique_substrings:
            replacement = unique_substrings[match]
        else:
            replacement = chr(ord('A') + len(unique_substrings))
            unique_substrings[match] = replacement

        input_string = input_string.replace(match, replacement)
    output = input_string
    output_strings.append(input_string)

    return output

def map_to_nl(clause):
    predicate = clause.split('(')[0]
    args = clause.split(predicate)[1]
    args = args[1:-1].split(',')

    proglish_mapping = {
                    f'short' : f'car {args[0]} is short,',
                    f'closed': f'car {args[0]} is closed,',
                    f'long' : f'car {args[0]} is long,',
                    f'open' : f'car {args[0]} is open,',
                    f'double' : f'car {args[0]} is double-walled,',
                    f'jagged' : f'car {args[0]} has a jagged roof,',
                    f'not_double' : f'car {args[0]} is double-edged,',
                }

    nth_car_mapping = {
        "1": 'st',
        "2": 'nd',
        "3": 'rd',
        "4": 'th',
        "5": 'th'

    }

    if predicate == 'eastbound':
        translation = f'Train {args[0]} is eastbound if'
    elif predicate == 'load':
        translation = f'car {args[0]} has {args[2]} {args[1]}-shaped load(s),'
    elif predicate == 'wheels':
        translation =  f'car {args[0]} has {args[1]} wheels,'
    elif predicate == 'shape':
        translation = f'car {args[0]} is {args[1]}-shaped,'
    elif predicate == 'has_car':
        translation = f'train {args[0]} has a car {args[1]}),'
    elif predicate == 'nth_car':
        translation = f'car {args[0]} is the {args[1]}{nth_car_mapping[args[1]]} car),'
        # translation = nth_car_mapping[args[1]]
    else:
        translation = proglish_mapping[predicate]

    # Remove extra brackets
    if translation[-3] != 's' and translation[-2] == ')':
        translation = translation[:-2] +  ','


    return translation

def convert_clause_to_string(data):
    string_list = [f"{item['functor']}({','.join(item['args'])})" for item in data]
    return ', '.join(string_list)

def convert_integers_to_string(clause):

    new_clause = []
    for predicate in clause:
        predicate_args = [str(arg) for arg in predicate['args']]
        new_clause.append({'functor': predicate['functor'], 'args': predicate_args})
    return new_clause

def pop_the_head(predicate_list, label):
    head_predicate = []
    new_predicate_list = []

    for predicate in predicate_list:
        if predicate.startswith('eastbound('):
            head_string = predicate
        else:
            new_predicate_list.append(predicate)

    body_string = ', '.join(new_predicate_list)

    if label == 'neg':
        head_string = head_string.replace('eastbound', 'negative_class')

    return head_string, body_string

def induce_with_swiplserver(main_prolog_thread, negative_prediction_thread, acuity_path, data_path, label):
    '''Runs induction with swiplserver. Returns a list of induced rules for positive and negaitve predictions.
    At present, this takes the two prolog threads and runs a separate loop for inducing the positive and negative
    rules'''
    
    # Run the positive prediction inference on the main thread
    main_prolog_thread.query("induce.")
    hyp = main_prolog_thread.query("get_hypothesis_as_list(HypothesisBody).")

    hypothesis_body = hyp[0]['HypothesisBody']

    pos_rules = []
    for clause in hypothesis_body:
        clause = convert_integers_to_string(clause)
        clause_as_list = [f"{item['functor']}({','.join(item['args'])})" for item in clause]
        print(clause_as_list)
        head_string, body_string = pop_the_head(clause_as_list, label)
        pos_rules.append(head_string + ' :- ' + body_string)

    # Run the negative prediction inference on the negative prediction thread
    pycuity_path = "src/data/acuityFiles/pycuity"
    neg_data_path = f"'src/data/train/pred_neg/train'"
    negative_prediction_thread.query(f"['{pycuity_path}'].")
    negative_prediction_thread.query(f"read_all({neg_data_path}).")
    negative_prediction_thread.query("induce.")
    neg_hyp = negative_prediction_thread.query("get_hypothesis_as_list(HypothesisBody).")

    neg_hypothesis_body = neg_hyp[0]['HypothesisBody']
    neg_rules = []
    for clause in neg_hypothesis_body:
        clause = convert_integers_to_string(clause)
        clause_as_list = [f"{item['functor']}({','.join(item['args'])})" for item in clause]
        print(clause_as_list)
        head_string, body_string = pop_the_head(clause_as_list, label)
        neg_rules.append(head_string + ' :- ' + body_string)

    return pos_rules, neg_rules

def run_inference_with_swiplserver(main_prolog_thread, inference_file_path):
    '''Runs inference with swiplserver. This creates an inference file
    containing induced rules and runs inference on the file. Returns a 
    list of positive predictions.'''
    inference_file_path = f"{inference_file_path}"
    main_prolog_thread.query(f"[{inference_file_path}].")
    pos_preds = main_prolog_thread.query("eastbound(X).")
    positive_predictions = [list(pred.values())[0] for pred in pos_preds]

    return positive_predictions


def fetch_clean_search_space(main_prolog_thread):
    '''
    We are using a nested for loop here to accounf for longer clauses. If we change themax clause length to longer than 3,
    then we'll need to generate an additional nested list. 
    '''
    search_and_constraints =  main_prolog_thread.query(f"fetch_search_space_constraints(Search_space, Constraint_violations).")
    raw_search_space, raw_constraint_violations = (search_and_constraints[0]['Search_space'], search_and_constraints[0]['Constraint_violations'])

    search_space_list = []
    for node in raw_search_space[:-10]:
        node = node['args']
        # print(node)
        clause_list = []
        for current_clause in node:
            if type(current_clause['args'][0]) is dict:
                current_clause = current_clause['args']
                for nested_clause in current_clause:
                        try:
                            clause_args = [str(arg) for arg in nested_clause['args']]
                            string_clause = f"{nested_clause['functor']}({','.join(clause_args)})"
                            clause_list.append(string_clause)
                        except:
                            continue
            else:
                try:
                    clause_args = [str(arg) for arg in current_clause['args']]
                    string_clause = f"{current_clause['functor']}({','.join(clause_args)})"
                    clause_list.append(string_clause)
                except:
                    continue
        search_space_list.append(clause_list)
    
    theory = []
    for reduced_hypothesis in search_space_list:
        head_string, body_string = pop_the_head(reduced_hypothesis, label='pos')
        # print('eastbound' + ' :- ' + body_string)
        theory.append(':- ' + body_string)

    return theory


def assert_pick_constraint(main_prolog_thread, acuity_path, data_path, example_number, constraint_predicate):
    '''Runs the pick_with_python prolog function. Takes the path to the dataset and selected example and 
    constraint predicate as input'''

    main_prolog_thread.query("initialise_incremental().")

    # assert the must_not constraint
    main_prolog_thread.query(f"initialise_shaping_example(eastbound(train{example_number}),N), sat(N),\
        bottom(B), pick_with_python(B,{constraint_predicate}).")

def get_bottom_clause(main_prolog_thread, acuity_path, data_path, example_number):

    '''
    Extracts the bottom clause from aleph and returns a bottom clause list and a reduced clause 
    string given the user-selected example
    '''


    main_prolog_thread.query("initialise_incremental().")
    bc = main_prolog_thread.query(f"initialise_shaping_example(eastbound(train{example_number}),N), sat(N),\
        bottom(B), find_clause(bf,_,RClause),\
        search_shaping:clause_with_index(B, ClauseListWithIndex).")    
    

    bottom_clause_as_list = []
    for current_clause in bc[0]["ClauseListWithIndex"]:
        current_clause = current_clause['args'][1]
        clause_args = [str(arg) for arg in current_clause['args']]
        string_clause = f"{current_clause['functor']}({','.join(clause_args)})"
        if not string_clause.startswith('eastbound('):
            bottom_clause_as_list.append(string_clause)

    # Get the reduced clause given the example selected
    RClause_list = bc[0]['RClause']
    reduced_hypothesis = []
    for current_clause in RClause_list:
        clause_args = [str(arg) for arg in current_clause['args']]
        string_clause = f"{current_clause['functor']}({','.join(clause_args)})"
        reduced_hypothesis.append(string_clause)

    head_string, body_string = pop_the_head(reduced_hypothesis, label='pos')
    full_clause = head_string + ' :- ' + body_string
    
    return bottom_clause_as_list, full_clause




def get_nl_bottom_clause(main_prolog_thread, acuity_path, data_path, example_number):
    '''
    Extracts the bottom clause from aleph and returns a bottom clause list and a reduced clause 
    string given the user-selected example. The reduced clause is in proglish
    '''


    main_prolog_thread.query("initialise_incremental().")
    bc = main_prolog_thread.query(f"initialise_shaping_example(eastbound(train{example_number}),N), sat(N),\
        bottom(B), find_clause(bf,RClause,_),\
        search_shaping:clause_with_index(B, ClauseListWithIndex), term_string(RClause, Proglish_clause).")    
    

    bottom_clause_as_list = []
    for current_clause in bc[0]["ClauseListWithIndex"]:
        current_clause = current_clause['args'][1]
        clause_args = [str(arg) for arg in current_clause['args']]
        string_clause = f"{current_clause['functor']}({','.join(clause_args)})"
        if not string_clause.startswith('eastbound('):
            bottom_clause_as_list.append(string_clause)


    output_clause = bc[0]['Proglish_clause']#[1:-1]
    output_clause = replace_numeric_substring_clause(output_clause)

    # Split the clause to allow for NL translation
    output_clause_list = re.split("\),|:-", output_clause)
    output_clause_list = [i + ')' if i[-1]!=')' else i for i in output_clause_list]
    
    new_clause = [map_to_nl(i) for i in output_clause_list]
    
    # end with a full stop
    new_clause[-1] = new_clause[-1][:-1] + '.'

    full_clause = ' '.join(map(str, new_clause))

    return bottom_clause_as_list, full_clause





def find_bottom_clause_indices(string_list, target_string):
    """Takes the bottom clause list and returns the indices of all elements that contain the 
    target predicate selected by the user."""
    result = []
    for i, string in enumerate(string_list):
        if target_string in string:
            result.append(i+1)
    return result

def clear_all_constraints(main_prolog_thread):
    '''Runs inference with swiplserver. This creates an inference file
    containing induced rules and runs inference on the file. Returns a 
    list of positive predictions.'''
    main_prolog_thread.query("clear_constraints(_,_).")
    # pos_preds = main_prolog_thread.query("eastbound(X).")
    # positive_predictions = [list(pred.values())[0] for pred in pos_preds]

