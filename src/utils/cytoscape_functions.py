
# This file contains functions for generating the search space cytoscape used in the interactive rule induction tab

import re
from dash import Dash, html
import dash_cytoscape as cyto
import json
from utils.swiplserver_functions import map_to_nl


def replace_numeric_substring(input_string):
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

def count_preds(clause):

    # This ensures the eastbound(A) cluase has value 0
    if ':-' not in clause:
        return -1
     
    predicates = re.findall(r'\),', clause)
    return len(predicates)

def extract_predicates(input_list):
    # result = []
    # for item in input_list:
    predicates = re.findall(r'\b(?!(?:eastbound|has_car)\()\w+\([^)]*\)', input_list)
        # result.append(predicates)
    return predicates

def is_sublist(sublist, list):
    n = len(sublist)
    return any(sublist == list[i:i+n] for i in range(len(list)-n+1))

def find_specialization(toy_search_space):
    '''This takes in a search space dictionary and outputs the (source,target) pairs
    for each node by finding the specialisations of each clause'''
    specs = []
    for step in toy_search_space:
        # step = toy_search_space[2]

        current_layer_num = step['pred_count']
        current_predicates = extract_predicates(step['clause'])

        next_layer_ids = [d['id'] for d in toy_search_space if d['pred_count'] == current_layer_num+1]
        next_layer_clauses = [d['clause'] for d in toy_search_space if d['pred_count'] == current_layer_num+1]
        next_layer_predicates = [extract_predicates(d['clause']) for d in toy_search_space if d['pred_count'] == current_layer_num+1]

        # for each current predicate, find the clauses in the next layer that contain it
        for id, next_preds in zip(next_layer_ids, next_layer_predicates):    
            if is_sublist(current_predicates, next_preds):
                specs.append((step['id'],id))
        
        
    return specs

def insert_newline(string):
    '''
    This takes in a clause and inserts a new line for printing in a cytoscape.
    '''
    string = string.replace(':-', ':-\n')
    string = string.replace('),', '),\n')
    return string

def generate_search_cytoscape_elements(aleph_output_path, selected_constraints):


    with open(aleph_output_path) as f:
        aleph_output = json.load(f)
    
    print(selected_constraints)

    # Trying to combine the vioalted constraints with the full search space.
    search_space = (aleph_output['search_space'])
    # constraint_violations =  aleph_output[selected_constraints[0]]

    constraint_violations = []
    for conn in selected_constraints:
        if conn in aleph_output:
            constraint_violations.extend(aleph_output[conn])

    # Process the search space
    search_space.reverse()
    search_space = [item for item in search_space if item.startswith("eastbound(")] # Remove if does not begin with pos-class

    # Replace numbers with letter variables and remove duplicates
    search_space = [replace_numeric_substring(clause) for clause in search_space]
    search_space = list(set(search_space))

    # Do the same for constraint violations and add a marker to the clause if it is a constraint violation
    constraint_violations = [replace_numeric_substring(clause) for clause in constraint_violations]
    search_space = ["~"+c if c in constraint_violations else c for c in search_space]


    # Add new line markers and sdd the number of predicates in each clause and store in a dictionary
    search_space = [insert_newline(text) for text in search_space]

    pred_counts = [count_preds(clause)+1 for clause in search_space]
    search_space = [{'clause':clause, 
                        'pred_count':pred_count}
                            for clause, pred_count in zip(search_space,pred_counts)]
    # #Order dictionaries according to pred_count
    search_space = sorted(search_space, key=lambda x: x["pred_count"])
    
    # Add an id key
    for i, d in enumerate(search_space):
            d['id'] = str(i)

    specs = find_specialization(search_space)


    nodes = []
    for i in search_space:
        nodes.append({'data' : {'id':i['id'], 'label':i['clause']}})

    edges = [
        {'data': {'source': source, 'target': target}}
        for source, target in specs
    ]

    elements = nodes + edges

    return elements

def get_alternative_hypotheses(good_hyp_filename = 'good_hypotheses.txt'):
    '''Fetches alternative good hypotheses as output by ALEPH'''

    # Read in the alternative hypotheses

    with open(good_hyp_filename, 'r') as file:
        hyps = [line.strip() for line in file]
    
    hyps = [insert_newline(text) for text in hyps]
    # Replace numeric with letter variables in the clauses
    hyps = [replace_numeric_substring(clause) for clause in hyps]

    # Return as a list
    return hyps

def extract_coverage_stats(string):
    pattern = r'\[([\d,\s]+)\]'
    matches = re.findall(pattern, string)
    
    if matches:
        return matches[0].split(',')
    
    return []


def extract_substring(string):
    pattern = r'1,pos,(.*)'
    match = re.search(pattern, string)

    if match:
        return match.group(1).lstrip(',')

    return ''


def get_good_clauses(good_clause_filename):
    '''This takes in the filename for good clauses found by ALEPH and returns the clause
    and the relative pos and neg cover stats for each clause
    Edited to only include the clause to allow for removal of duplicates'''

    with open(good_clause_filename, 'r') as file:
        hyps = [line.strip() for line in file]


    # Return as a single string with each on different lines
    # In places, it does not seem to succeffully output a cluase - fixed for now with try, except
    alt_hyp_list = []
    for hyp in hyps:

        try:
            stats = extract_coverage_stats(hyp)
            clause = extract_substring(hyp)[1:-2]
            clause = replace_numeric_substring(clause)

            # Convert to NL - this should be put into a function during refactoring
            output_clause_list = re.split("\),|:-", clause)
            output_clause_list = [i + ')' if i[-1]!=')' else i for i in output_clause_list]
        
            new_clause = [map_to_nl(i) for i in output_clause_list]
            # end with a full stop
            new_clause[-1] = new_clause[-1][:-1] + '.'
            
            # remove excess brackets
            full_clause = ' '.join(map(str, new_clause))
            full_clause = str(full_clause).replace(')','')

            if clause not in alt_hyp_list:
                # alt_hyp_list.append('--------------------------------')
                # alt_hyp_list.append(extract_substring(hyp)[1:-2])
                alt_hyp_list.append(full_clause + f" -> Pos:{stats[0]} | Neg:{stats[1]}")
                # alt_hyp_list.append(f'Pos: {stats[0]} | Neg: {stats[1]}')
        except:
            print(f'No clause found: {hyp}')



    return list(set(alt_hyp_list))

