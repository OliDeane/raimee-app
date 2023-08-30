import os
import re

def get_arity_substring(search_string, file_path):
    with open(file_path, 'r') as file:
        for line in file:
            index = line.find(search_string)
            if index != -1:
                # found the search string, now look for the number after the '/'
                number_index = index + len(search_string) + 1 # skip the '/'
                number_str = ''
                while number_index < len(line) and line[number_index].isdigit():
                    number_str += line[number_index]
                    number_index += 1
                if number_str:
                    num_underscores = int(number_str)
                    result_str = '(' + ','.join(['_' for _ in range(num_underscores)]) + ')'
                    return result_str
    return '(_)'


def add_integrity_constraint(constraint_predicate, class_type = 'pos', dataset = 'train'):
    """
    Currently just removes a predicate from a hypothesis
    Path included so requires editing 
    """

    path = os.getcwd()
    file_path = os.path.join(path,f'data/{dataset}/pred_{class_type}/{dataset}.b')

    # Get arity of predicate
    arity_substring = get_arity_substring(constraint_predicate, file_path)
    bk_file = open(file_path, 'a')
    
    bk_file.write(f"\nfalse :- hypothesis(_,Body,_), bodyList(Body, List), !, member({constraint_predicate}{arity_substring}, List).\n\n")
    
    bk_file.close()

def write_operator(operator, dataset = 'train'):
    path = os.getcwd()
    file_path = os.path.join(path,f'data/{dataset}/pred_pos/{dataset}.b')
    bk_file = open(file_path, 'a')
    bk_file.write(operator)
    bk_file.close()

def add_positive(example_num, dataset='mutag188'):
    """
    Currently just adds an example to the positives file.
    We need to implement something that checks whether the example already exists there
    and something that removes the example from the negative file. 
    """
    print("Hello")
    file = open('../components/induce_rules/aleph_input/{dataset}/{dataset}.f', 'a')
    file.write(f"\ntrue_mutagenic(d{example_num}).\n")
    
    file.close()

def initiate_refiner(pred, target_pred = 'eastbound', dataset='train'):

    new_clause = f"{target_pred}(X):-{pred}(X,Y)"
    operator = f"refine({target_pred}(X),({new_clause}))."


    return new_clause, operator

def add_refiner_predicate(prev_clause, pred, target_pred = 'eastbound', dataset = 'train'):

    # Remove final two brackets and full stop (if they exist)
    prev_clause = re.sub(r'\)\)\.$', '', prev_clause)
    new_clause = prev_clause + f", {pred}(Y)"
    operator = f"\nrefine(({prev_clause}), Clause):-Clause = ({new_clause}).\n\n"
    return new_clause, operator

def test():
    # Example string
    example_str = "eastbound(X):-has_car(X,Y), short(Y)))."

    # Remove final two brackets and full stop (if they exist)
    print(re.sub(r'\)\)\.$', '', example_str))

def remove_mode_declaration(file_path, substring):


    with open(file_path, 'r') as f:
        lines = f.readlines()

    with open(file_path, 'w') as f:
        for line in lines:
            if substring in line and ':- modeb(' in line:
                f.write('% ' + line)
            else:
                f.write(line)

def reset_file(file_path):

    with open(file_path, 'r') as f:
        lines = f.readlines()

    with open(file_path, 'w') as f:
        for line in lines:
            if '% :-' in line:
                new_line = line.replace("% ", "")
                f.write(new_line)
            else:
                f.write(line)
    
    # Clear added rules
    file_to_delete = open("components/model_editor/added_rule.txt",'w')
    file_to_delete.close()
    return None


# if __name__=='__main__':
#     reset_file('data/breast_cancer/pred_pos/breast_cancer.b')
    # clause, operator = initiate_refiner('has_car', target_pred='eastbound')
    # print(operator)
    # clause, operator = add_refiner_predicate(prev_clause = clause, pred = 'short', target_pred='eastbound')
    # print(operator)

    # clause, operator = add_refiner_predicate(prev_clause = clause, pred = 'closed', target_pred='eastbound')
    # print(operator)

    # predicate = 'attended'
    # dataset = 'train'
    # file_path = f'data/{dataset}/pred_pos/{dataset}.b'
    # remove_mode_declaration(predicate, file_path)

