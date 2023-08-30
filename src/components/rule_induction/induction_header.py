import dash_bootstrap_components as dbc
from swiplserver import PrologMQI, PrologThread
from dash import Input, Output, html, dcc, no_update, callback, State
import subprocess
import json
import os
import re

from utils.app_functions import *
from utils.prolog_functions import add_integrity_constraint, remove_mode_declaration, reset_file
from utils.swiplserver_functions import induce_with_swiplserver, run_inference_with_swiplserver

# button to induce rules
induce_button = dbc.Button('Induce', id = 'induce-button2', color = "primary", size="lg", className='me-2', style = {"margin-left": "40%"}, n_clicks=0)

# button to reset rules
reset_button = dbc.Button('Reset', id = 'reset-button-header', color = "secondary", size="lg", className='me-2', style = {"margin-left": "40%"}, n_clicks=0)

# button to view change log
change_log_button = dbc.Button('Change Log', id = 'change-log-button', color = "secondary", size="lg", style = {"margin-left": "40%"}, className='me-2', n_clicks=0)

# text info about the model and dataset
data_info = html.Div([html.H5("Instances: 188 | Label T : 94 (50%) |  Label F : 94 (50%)")], style = {"margin-top": "1rem"})

# Header card displaying the three buttons side by side
header_card = dbc.Card(
        [
            dbc.CardBody(
                [
                    dbc.Row(
                        [
                            dbc.Col(
                                [
                                    reset_button,
                                    html.P(id='reset-complete', children=[], style={'top-margin':'0rem'})
                                ]
                            ),
                            dbc.Col(
                                [
                                    induce_button,
                                    data_info
                                ]

                            ),
                            dbc.Col(
                                [
                                    change_log_button,
                                    html.Div(id="log-output")
                                ]
                            ),
                        ]
                    ),
                ]
            ),
        ],
        style={"margin-top":"1rem", "margin-left":"5rem", "width": "85%", "margin-right":"5rem"},
    )

    
@callback(
    Output('removed-predicate', 'children'),
    [Input('remove-button', 'n_clicks')],
    [State('selection-target', 'value')],)
def removePredicate(n_clicks, value):
    if value:
        # Add constraint to the background knowledge


        # Fetch current dataset path
        selected_dataset = get_selected_dataset()

        predicate = re.sub(r'\(.+', '', value)

        file_path = f'data/{selected_dataset}/pred_pos/{selected_dataset}.b'
        remove_mode_declaration(file_path, predicate)

        # Save event to the event log
        save_event_to_log('Predicated Removed', value)

        return html.Span(f'Removed Predicate: "{value}"', style=dict(color='red'))

@callback(
    Output('reset-complete', 'children'),
    [Input('reset-button-header', 'n_clicks')])
def reset_prolog_files(n_clicks):
    if n_clicks:

        # Fetch current dataset path
        selected_dataset = get_selected_dataset()

        file_path = f'data/{selected_dataset}/pred_pos/{selected_dataset}.b'
        reset_file(file_path)

        return html.P('Reset Complete')

@callback(
    Output('constraint-confirmed', 'children'),
    [Input('constraint-button', 'n_clicks')],
    [State('selection-target', 'value')])
def add_constraint(n_clicks, value):
    if not n_clicks:
        return None
    
    selected_dataset = get_selected_dataset()
    add_integrity_constraint(value, 'pos', selected_dataset)
    return html.Span(f'Constraint Added: "{value}"', style=dict(color='red'))

@callback(
    Output('log-output', 'children'),
    Input('change-log-button', 'n_clicks')
)
def log_modal(n_clicks):
    if not n_clicks:
        return no_update
    
    # load in the current event log
    log_dict = get_event_log()

    return html.Div(
    [
        dbc.Modal(
            [
                dbc.ModalHeader(
                    [
                        dbc.ModalTitle("Event Log"),
                        # dbc.ButtonGroup(
                        #     [
                        #         dbc.Button("<", outline=True, color="primary"),
                        #         dbc.Button(">", outline=True, color="primary"),
                        #     ]
                        # )   
                    ],
                    close_button=False
                ),
                dbc.ModalBody(
                    [   
                        html.Pre(json.dumps(log_dict, indent=2))
                    ]
                ),
                
                dbc.ModalFooter([
                    dbc.ButtonGroup(
                        [
                            dbc.Button("Save", outline=False, color="success"),
                            dbc.Button("Delete", outline=False, color="danger"),
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