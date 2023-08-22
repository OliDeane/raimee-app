from dash import html, Input, Output, State, dcc, callback, no_update, dash_table, callback_context
import dash_bootstrap_components as dbc
from pyswip import Prolog
import subprocess
import json
import pandas as pd
import time
import re
import os
import ast

from utils.app_functions import str2lst, parameter_dropdown, get_selected_dataset, save_event_to_log
from utils.prolog_functions import add_integrity_constraint, remove_mode_declaration, reset_file
from utils.swiplserver_functions import *

nlSwitch = html.Div(
    [
        dbc.Switch(
                    value=False,
                    id='nl-switch',
                    label='Display in natural language',
                    input_class_name='bg-success'
                    ),
        dcc.Store(id='nl-store', data=[], storage_type='memory')
    ]
)

# Generate dropdowns for the parameters
eval_list = ['coverage', 'compression', 'positive only', 'accuracy', 'laplace', 'entropy', 'gini', 'sd']
search_list = ['Breadth-first', 'Depth-first', 'A-star']
min_pos_list = ['1', '2', '3', '4', '5']

def parameter_dropdown(d_id, placeholder, options):
    submit_button = dbc.Button("+", id = 'fake_induce', color = "primary", className='me-2', n_clicks=0)

    return html.Div(
        [
            html.Div(
                [
                    dcc.Dropdown(
                        options,
                        placeholder = placeholder,
                        id = d_id,
                        style={
                            'height':'37px',
                            'width': '412px',
                            'borderWidth': '1px',
                            'borderRadius': '1px',
                            'textAlign': 'center',
                            'margin-bottom':'2px'
                        }   
                    )
                ], 
                style={'display': 'inline-block'},
            ),
            html.Div([submit_button], style={'display': 'inline-block', 'margin-left':'1rem'}),
            dcc.Store(id='predicate-store-1', data=[], storage_type='memory')
        ],
        style={"display": "flex", "margin-bottom":"1rem"}
    )

predicate_selectors = html.Div(
    [
        dbc.Row(
            [
                dbc.Col([
                    html.P('Cost Function'),
                    parameter_dropdown('eval_dropdown', 'coverage', eval_list),
                    html.Div(id = 'selected-evalfn', children = [], style = {'display':'none'}),
                    html.Div('Search Strategy') ,
                    parameter_dropdown('search_dropdown', 'Breadth-first', search_list), 
                    html.P('Minimum Positive Examples'),
                    parameter_dropdown('min_pos_dropdown', '1', min_pos_list), 
                ]),
            ]
        )
    ]
)
refine_footer = html.Div(
    [
        dbc.Button("Submit", id = 'fake_induce', color = "primary", className='me-2', n_clicks=0),
        dbc.Button("Reset", id = 'fake_induce', color = "primary", className='me-2', n_clicks=0)
    ],
    style = {"margin-top":"1rem"}
)


# Hypothesis editing accordion
inConHeader = html.Div(
    [
        html.H6(["Highlight predicates and submit constraints", html.Br(), html.Br()], className="card-subtitle"),
        dbc.ButtonGroup(
            [
                dbc.Button("Positive", outline=False, color="success"),
                dbc.Button("Negative", outline=True, color="danger"),
            ]
        )
    ]
)
inConInnerDiv = html.Div(
    [
        html.Div(id = 'hypothesisIC-box', children = [
            html.Div(id = 'interactive-hypothesis-disp', children = [])
        ])
    ], style={"height":"30rem", "margin-bottom":"1rem"} # Add "overflow": "scroll" if needed
)


# Hypothesis shaping

# This is the input box. The input string will only be processed on the Submit button press. 
# It allows for drag and drop functionality.
text_input = html.Div(
    [   
        html.Div(
            [
                dbc.Input(id="shaping-example-input", 
                          placeholder="Enter Example",
                          type="string",
                          style = {
                              "width":"80%", "display": "inline-block", "margin-right":"1rem"
                            }
                        ),
                dbc.Button('Submit', 
                            id='submit-shaping-val', 
                            n_clicks=0,
                            color = "primary", 
                            style = {"display": "inline-block", "margin-right":"0.25rem"}),
                dcc.Store(id='selected-shaping-example-store', data=[], storage_type='memory'),                
            ],
        ),
        html.P(id="selected-shaping-example")
    ],
)


hypothesis_shaping_header = html.Div(
    [
        html.H6(["Shape the hypothesis via bottom clause predicates", html.Br(), html.Br()], className="card-subtitle"),
    ]
)
hypothesis_shaping_innerDiv = html.Div(
        [
            text_input,
        ], 
        style={"height":"39rem", "margin-bottom":"1rem"} # Add "overflow": "scroll" if needed
    )

hypothesis_shaping_card = html.Div([
    hypothesis_shaping_header,
    hypothesis_shaping_innerDiv
])

# Data View card
interaction_card = dbc.Card(
    [
        dbc.CardBody(
            [
                html.H4("Collaborative Classification", className="card-title", style = {'margin-left':'1rem'}),
                html.H6(["Iteratively build hypotheses and select mechanisms to deliver feedback"], className="card-subtitle", style={"margin-bottom":"1rem", 'margin-left':'1rem'}),
                text_input
            ]
        ),
    ],
    style={"margin-top":"1rem", "margin-left":"1rem", "width": "95%", "height":"55rem", "overflow-y":"scroll"}
)


# interaction_card = dbc.Card(
#     [   
#         dbc.Accordion(
#             [
#                 dbc.AccordionItem(
#                     [
#                         hypothesis_shaping_card
#                     ],
#                     title="Shape Hypothesis", 
#                 ),

#                 dbc.AccordionItem(
#                     [
#                         inConHeader,
#                         inConInnerDiv,
#                     ],
#                     title="Edit Hypothesis",
#                 ),
#             ],
#             start_collapsed=False,
#         ),
#     ],
#     style={"margin-top":"1rem", "margin-left":"1rem", "margin-right":"1rem"},
# )

# @callback([Output("selected-shaping-example", "children"),
#            Output("selected-shaping-example-store", "data")], 
#           [Input("shaping-example-input", "value")])
# def display_bottom_clause(value):
#     if not value:
#         return no_update
    
#     # Fetch current dataset path
#     selected_dataset = get_selected_dataset()
#     pycuity_path = "src/data/acuityFiles/pycuity"
#     pos_data_path = f"'src/data/{selected_dataset}/pred_pos/{selected_dataset}'" 


#     bottom_clause_list_raw, reduced_clause = get_bottom_clause(pycuity_path, pos_data_path, value)
    
#     print(reduced_clause)

#     # Add the html.Br() to the bottom clause list to display it properly
#     bottom_clause_list = []
#     for i in bottom_clause_list_raw:
#         bottom_clause_list.append(i)
#         bottom_clause_list.append(html.Br())


#     interactive_bottom_clause = html.Div(
#             [
#                 html.Hr(),
#                 html.Div(
#                     [   
#                         html.H4('Reduced Clause'),
#                         html.P(reduced_clause),
#                         html.Hr(),
#                         html.Br(), 
#                         html.H4('Bottom Clause'),
#                         # html.P(id='selection-container', children=html.Div([html.P(string) for string in bottom_clause_list])),
#                         html.P(id='selection-container', children=bottom_clause_list),
#                         dcc.Input(id='selection-target', value='', style=dict(display='none')),
#                     ],
#                     style={"height":"25rem", "overflow-y": "scroll", "margin-bottom":"1rem"}
#                 ),
#                 dbc.Button(id='bc-submit', children='Must', color = "success", className='me-2', n_clicks=0),
#                 dbc.Button(id='bc-mustnot-button', children='Must Not', color = "danger", className='me-2', n_clicks=0),
#                 dbc.Button(id='bc-constraint-button', children='Constraint', color = "warning", className='me-2', n_clicks=0),
#                 dbc.Button(id = 'bc-reset-button-1', children='Reset', color = "secondary", className='me-2', n_clicks=0),
#                 html.P(id='bc-removed-predicate', children=[], style={'top-margin':'0rem'}),
#                 html.P(id='bc-constraint-confirmed', children=[], style={'top-margin':'0rem'}),
#                 html.P(id='bc-mustnot-confirmation', children=[], style={'top-margin':'0rem'}),
#             ]
#         )

#     return interactive_bottom_clause, {'current_example_number':value}


# @callback(
#     Output('mustnot-confirmation','children'),
#     # [Input('selected-shaping-example-store','data'),
#     [Input('bc-mustnot-button','n_clicks')],
# )
# def add_mustnot_constraint(n_clicks):
#     if not n_clicks:
#         return no_update
    
#     # print(data)
#     # example_number = data['current_example_number']
#     print('Hello')
#     example_number = '5'
#     # return html.P(data['current_example_number'])
#     return html.Span(f'Removed Predicate: "{example_number}"', style=dict(color='red')) 


# @callback(
#     Output('bc-removed-predicate', 'children'),
#     [Input('bc-mustnot-button', 'n_clicks'),
#      Input('selected-shaping-example-store','data')],
#     [State('selection-target', 'value')],)
# def removePredicate(n_clicks, data, value):
    # if not n_clicks:
    #     return no_update

    # # Add constraint to the background knowledge
    
    # # Fetch current dataset path
    # selected_dataset = get_selected_dataset()

    # # predicate = re.sub(r'\(.+', '', value)

    # # Assert the mustnot constraint
    # pycuity_path = "src/data/acuityFiles/pycuity"
    # pos_data_path = f"'src/data/{selected_dataset}/pred_pos/{selected_dataset}'" 
    # assert_pick_constraint(pycuity_path, pos_data_path, data['current_example_number'], constraint_predicate = 'must_not([5])')
    

    # # Save event to the event log
    # save_event_to_log('MustNot Constrain Added', value)

    # return html.Span(f'Removed Predicate: "{value}"', style=dict(color='red'))