import dash_bootstrap_components as dbc
from dash import Input, Output, State, html, Dash, dcc, dash_table, callback, no_update
import dash_cytoscape as cyto
import dash
from dash_bootstrap_components._components.Container import Container
import plotly.express as px
import numpy as np
import pandas as pd
import json
import os
import subprocess
import random

from utils.app_functions import *

def load_mutag(pos_ids):
    # Load in dataset and remove primary key row and mutagenic column (this is unknownd data)
    mutagDf = pd.read_csv(f"data/mutag_plus/raw_data/molecule.csv")
    mutagDf = mutagDf.drop(0)
    mutagDf = mutagDf.drop(['mutagenic'], axis=1)

    # List of positive prediction ids
    mutagDf['pred'] = ['pos' if id_ in pos_ids else 'neg' for id_ in mutagDf['molecule_id']]
    mutagDf.insert(0,'.', ["\U0001f50e"] * len(mutagDf['pred'])) 

    return mutagDf

def read_last_file_as_dataframe(directory):
    # Get a list of all files in the directory
    file_list = os.listdir(directory)
    if len(file_list) == 0:
        print("No files found in directory.")
        return None
    # Sort the file list alphabetically
    file_list.sort()
    # Get the last file in the list
    first_file = file_list[-1]
    # Read the contents of the file into a pandas dataframe
    df = pd.read_csv(os.path.join(directory, first_file))
    return df

def load_assert_examples_table(dataset):
    path = f'data/{dataset}/raw_data/ae_file.csv'
    # df = read_last_file_as_dataframe(path)
    df = pd.read_csv(path)    
    # Drop needless row
    df = df.drop(0)
    df.insert(0,'.', ["\U0001f50e"] * len(df['class'])) 

    unknown = df[df['class'] == 'unknown']
    pos = df[df['class'] == 'pos']
    neg = df[df['class'] == 'neg']

    return unknown, pos, neg

def test_fetch_mutag_arrays(exampleNum):
    molecule_df = pd.read_csv('data/mutag_plus/raw_data/molecule.csv')
    atom_df = pd.read_csv('data/mutag_plus/raw_data/atom.csv')
    bond_df = pd.read_csv('data/mutag_plus/raw_data/bond.csv')

    mol_array = molecule_df[molecule_df['molecule_id'] == molecule_df['molecule_id'][exampleNum+2]].values.tolist()
    bond_df['molecule_id'] = [id.split('_')[0] for id in bond_df['atom1_id']]
    bond_array = bond_df[bond_df['molecule_id'] == bond_df['molecule_id'][exampleNum+2]].values.tolist()
    atom_array = atom_df[atom_df['molecule_id'] == atom_df['molecule_id'][exampleNum+2]].values.tolist()

    return mol_array, bond_array, atom_array


style_data_conditional=[
                {
                    'if': {
                        'filter_query': "{pred} = 'pos'",
                    },
                    'backgroundColor': '#1f77b4'
                },
                {
                    'if': {
                        'filter_query': "{pred} = 'neg'",
                    },
                    'backgroundColor': '#FF4136'
                },
                {
                    'if': {
                                'column_id': '.',
                            },
                            'border': '1px solid rgb(0, 0, 0)',
                            'borderRight': '1px rgb(0, 0, 0) solid',
                            'backgroundColor': 'white',
                            'borderLeft': '1px rgb(0, 0, 0) solid',

                },

            ]

assertExamples_button_group = html.Div(
    [
        dbc.RadioItems(
            id="assertExamples_radios",
            className="btn-group",
            inputClassName="btn-check",
            labelClassName="btn btn-outline-primary",
            labelCheckedClassName="active",
            options=[ 
                {"label": "Unknown", "value": "unknown"},
                {"label": "Positive", "value": "positive"},
                {"label": "Negative", "value": "negative"},
            ],
            # value="unknown",
        ),
        html.Div(id="assertExamples_output"),
    ],
    className="radio-group",
)

assertExamplesCard = dbc.Card(
    [
        dbc.Accordion(
            [
                dbc.AccordionItem(
                    [
                        # html.H4("Assert Examples", className="card-title"),
                        html.H6("Add counter examples to guide model learning", className="card-subtitle", style={"margin-bottom":"1rem"}),
                        assertExamples_button_group
                    ],
                    title="Assert Examples",
                )
            ]
        )
    ]
)

@callback(
    Output("assertExamples_output", "children"),
    [Input("assertExamples_radios", "value")]
)
def display_assertExamples(value):

    if not value:
        return no_update

    selected_dataset = get_selected_dataset()

    # Extract information required to run induction on the selected data
    unknown, pos, neg = load_assert_examples_table(selected_dataset)

    ae_tables = {'unknown':unknown, 'positive':pos, 'negative':neg}
    mutagDf = ae_tables[value]

    # Access the positive predictions and add them to the dataframe
    with open('components/rule_induction/positive_preds.json') as f:
        pos_pred_dict = json.load(f)
    
    pred_pos = pos_pred_dict['pos_preds']
    pred_pos_lst = []
    for i in mutagDf['id']:
        if i in pred_pos:
            pred_pos_lst.append('pos')
        else:
            pred_pos_lst.append('neg')

    mutagDf['pred'] = pred_pos_lst

    return html.Div(
            [
                dash_table.DataTable(
                    id="assertExamplesTable",
                    columns=[{"name": i, "id": i} for i in mutagDf.columns],
                    data=mutagDf.head(20).to_dict('records'),
                    style_cell={'textAlign':'left'},
                    # style_as_list_view=True,
                    style_header={
                        'font_family': 'Times New Roman',
                        'backgroundColor': 'white',
                        'fontWeight': 'bold'
                    },
                    style_data_conditional=style_data_conditional
                ),
                dcc.Store(id='selected-example-store', data=[], storage_type='memory'),
                html.Div(id="test_output"),
            ],
            style={"margin-top":"1rem"}
        )

@callback(
    # Output("test_output", "children"),
    Output('selected-example-store', "data"),
    [Input("assertExamplesTable", "active_cell")]
)
def present_number(active):
    # mutagdF = load_mutag()
    if not active:
        return no_update

    mol_array, bond_array, atom_array = test_fetch_mutag_arrays(active["row"])
    print(active)
    return {
            'row_id':active['row_id'],
            'molecule': mol_array,
            'atom': atom_array,
            'bond': bond_array
        }

@callback(
    Output("test_output", "children"),
    [Input("selected-example-store", "data")]
 )
def present_modal(data):
    # Graph view 
    left_panel = html.Div(
        [
            html.Div(id="assertSelectedExampleInfo")
        ],
        style = {'margin-left':'1rem', 'margin-top':'2rem'}    
    )
    right_panel = html.Div(
        [
            html.Div([
                dbc.Toast(
                id="assert-sample-info",
                header="Selected Molecule",
                icon="primary",
                dismissable=True,
                is_open=True,
                )
            ],
            style={'margin-bottom':'2rem'}
            ),   
            dbc.Toast(
                id="assert-node-info",
                header="Selected Atom",
                icon="primary",
                dismissable=True,
                is_open=True,
            ),
        ],
        # style={'margin-right':'3rem'}
    )
    graph_view = html.Div([
        dbc.Row([
            dbc.Col([
                left_panel
            ]),
            
            dbc.Col([
                right_panel
            ]),    
        ])
    ])


    return html.Div(
        [
            dbc.Modal(
                [
                    dbc.ModalHeader(
                        [
                            dbc.ModalTitle(f"Example ID: {data['row_id']}"),
                            dbc.ButtonGroup(
                                [
                                    dbc.Button("<", outline=True, color="primary"),
                                    dbc.Button(">", outline=True, color="primary"),
                                ]
                            )   
                            # dbc.Button(">", className = 'ms-auto', color="primary")
                        ],
                        close_button=False
                    ),
                    dbc.ModalBody(
                        [   
                            html.H6('Class Label: Unknown'),
                            html.H6('Current Prediction: Negative'),
                            graph_view,
                            html.Div(id='pos_assertion_confirmed'),
                            html.Div(id='neg_assertion_confirmed')   
                        ]
                    ),
                    
                    dbc.ModalFooter([
                        dbc.ButtonGroup(
                            [
                                dbc.Button("Assert Positive", id='pos_assert_button', outline=False, color="success"),
                                dbc.Button("Assert Negative", id='neg_assert_button', outline=False, color="danger"),
                            ]
                        )
                    ])
                ],
                id="modal-centered",
                size="lg",
                centered=True,
                is_open=True,
            ),
        ]
    )

@callback(
    Output('pos_assertion_confirmed', 'children'),
    [Input('selected-example-store', 'data'),
     Input('pos_assert_button', 'n_clicks')]
)
def assert_button_action(data, n_clicks):
    if not n_clicks:
        return no_update
    
    assert_example_positive(data['row_id'])
    change_aetable_class(data['row_id'], new_class = 'pos')

    # Add action to the event log
    save_event_to_log('Positive Assertion', data['row_id'])
    

    return html.H6('Assert: POSITIVE')

@callback(
    Output('neg_assertion_confirmed', 'children'),
    [Input('selected-example-store', 'data'),
     Input('neg_assert_button', 'n_clicks')]
)
def assert_button_action(data, n_clicks):
    if not n_clicks:
        return no_update
    
    assert_example_negative(data['row_id'])
    change_aetable_class(data['row_id'], new_class = 'neg')

    save_event_to_log('Negative Assertion', data['row_id'])

    return html.H6('Assert: NEGATIVE')

@callback(
    Output('assert-sample-info', 'children'),
    Input('selected-example-store', 'data')
)
def create_exampleInfo(data):

    mol_array = data['molecule'][0]
    mol_dict = {'ID':mol_array[0],
        'Mutagenic':mol_array[5],
        'logP':mol_array[3],
        'ind1':mol_array[4]
    }
    return html.Pre(json.dumps(mol_dict, indent=2))

@callback(
    Output('assertSelectedExampleInfo', 'children'),
    Input('selected-example-store', 'data')
)
def returnExample(data):

    if data:

        atom_array = data['atom']
        bond_array = data['bond']
        mol_array = data['molecule'][0]

        nodes = [
        {'data': {'id': atom_id, 'label': f'{atom_id}:{element}'}}
        for atom_id, _, element, _, _ in atom_array]
        edges = [{'data': {'source': source, 'target': target}}
        for source, target,_,_ in bond_array]

        cytoscape = html.Div(
            [
            html.Div(
                [
                cyto.Cytoscape(
                    id='cytoscape-event-callbacks-1',
                    layout={'name': 'cose'},
                    elements=edges+nodes,
                    style={'width': '100%', 'height': '400px'}
                ),
                dcc.Store(id='assert-node-store', data=[], storage_type='memory')
                ],
                style={"width":"23rem"}
            ),
            ]
        )
        im_numm = random.randint(0, 5)
        img = html.Div([
            html.Img(src=f"/assets/images/train_imgs/train_{im_numm}.png")
            ])

        return img

@callback(Output('assert-node-store', 'data'),
              Input('cytoscape-event-callbacks-1', 'tapNodeData'))
def displayTapNodeData(data):
    if not data:
        output_data = {
        'id':'None',
        'molecule_id':'None',
        'element': 'None',
        'atype': 'None',
        'charge': 'None'
        }
        return json.dumps(output_data, indent=2)
    else:
        atom_df = pd.read_csv('data/mutag_plus/raw_data/atom.csv')
        selected_atom = atom_df[atom_df['atom_id']==data['id']].values.tolist()[0]
        output_data = {
            'id':selected_atom[0],
            'molecule_id':selected_atom[1],
            'element': selected_atom[2],
            'atype': selected_atom[3],
            'charge':selected_atom[4]
        }
        return json.dumps(output_data, indent=2)

@callback(
    Output('assert-node-info', 'children'),
    Input('assert-node-store', 'data')
)
def create_nodeInfo(data):
    if data:
        # convert string to json (needs fixing)
        node_dict = json.loads(data) 
        return html.Pre(json.dumps(node_dict, indent=2))

