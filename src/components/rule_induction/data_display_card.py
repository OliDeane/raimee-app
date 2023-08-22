'''
This file contains the Data Display div for the Rule Induction tab.
It has the ability to display both the additional data and the model's training
data with a button group. The button group can show joined buttons which 
are highlighted depending on which button was pressed most recently. For newer versions
of the system, this has been replaced with a simple "reload data" button to enable
"extent"-based updates. 
 '''

import dash_bootstrap_components as dbc
from dash import Input, Output, html, dcc, no_update, callback, callback_context
import subprocess
import json
import os
from utils.app_functions import *
from dash.exceptions import PreventUpdate

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

data_lake_button_group = html.Div(
    [
        dbc.ButtonGroup(
            [
                dbc.Button("Dataset", id="test", outline=True, color = 'primary'),
                dbc.Button("Additional Data", id="data_lake", outline=True, color='primary')
            ]
        ),
        html.Div(id="data_lake_display"),
    ],
    className="p-3",
)

data_display_div = html.Div(
    [
        dbc.ButtonGroup(
            [
                dbc.Button("Display Coverage", id="all_data_button", outline=False, color='primary')
            ]
        ),
        html.Div(id="all_data_display"),
    ],
    className="p-3",
)


# Data View card
displayDataCard = dbc.Card(
    [
        dbc.CardBody(
            [
                html.H4("Dataset View", className="card-title", style = {'margin-left':'1rem'}),
                html.H6(["View the original labelled data and the additional data lake "], className="card-subtitle", style={'margin-left':'1rem'}),
                # data_lake_button_group
                data_display_div
            ]
        ),
    ],
    style={"margin-top":"1rem", "margin-left":"2rem", "margin-right":"2rem", "width": "95%", "height":"55rem", "overflow":"-moz-scrollbars-vertical", "overflow-y":"scroll"}
)


@callback(
    [
        Output("data_lake_display", "children"),
        Output("test", "active"),
        Output("data_lake", "active"),
    ],
    [
        Input("test", "n_clicks"),
        Input("data_lake", "n_clicks")
    ],
)
def display_data_toggle(n_test, n_data_lake):
    
    if not n_test and not n_data_lake:
        button_id = 'data_lake'
    else:
        # Check which button was pressed
        ctx = callback_context
        button_id = ctx.triggered[0]['prop_id'].split('.')[0]


    # Fetch list of covered examples
    with open('src/data/meta_data/working_data.json') as meta_data:
        dataset_info = json.load(meta_data)
    print(dataset_info)
    pos_list = dataset_info['current_coverage']

    # Display the dataset
    eastbound_display_info = dict([(example,"#1f77b4") if example in pos_list else (example,"white")
                                   for example in dataset_info[f'eastbound_{button_id}_examples']])
    

    westbound_display_info = dict([(example,"#1f77b4") if example in pos_list else (example,"white")
                                   for example in dataset_info[f'westbound_{button_id}_examples']])

    trial_number = dataset_info['trial_number']

    # Display the key
    car_shapes = ['bucket.gif', 'u_shaped.gif', 'rectangle.gif', 'ellipse.gif', 'hexagon.gif']
    roof_shapes = ['flat.gif', 'jagged.gif', 'peaked.gif']
    load_shapes = ['hexagon_.gif', 'diamond.gif', 'inverse_triangle.gif', 'circle.gif', 'rectangle_.gif', 'triangle.gif']
    extra_shapes = ['double.gif', 'example.gif', 'open.gif', 'closed.gif']


    key_card = html.Div([

        dbc.Button(
            "Show Key",
            id="auto-toast-toggle",
            color="primary",
            className="mb-3",
            n_clicks=0,
        ),

        dbc.Toast(
            [
                dbc.Row(
                    [   
                        dbc.Col([
                            html.H6("Car Shapes"),
                            dbc.Container([
                                dbc.Row([
                                    dbc.Col([generate_component_img(comp), html.P([comp[:-4]])]),
                                ]) for comp in car_shapes
                            ])
                        ]),
                        dbc.Col([
                            html.H6("Load Shapes"),
                            dbc.Container([
                                dbc.Row([
                                    dbc.Col([generate_component_img(comp), html.P([comp[:-4]])]),
                                ]) for comp in load_shapes
                            ])
                        ]),
                        dbc.Col([
                            html.H6("Roof Shapes"),
                            dbc.Container([
                                dbc.Row([
                                    dbc.Col([generate_component_img(comp), html.P([comp[:-4]])]),
                                ]) for comp in roof_shapes
                            ])
                        ]),
                        dbc.Col([
                            html.H6("Misc."),
                            dbc.Container([
                                dbc.Row([
                                    dbc.Col([generate_component_img(comp), html.P([comp[:-4]])]),
                                ]) for comp in extra_shapes
                            ])
                        ]),
                    ]
                )
            ],
            style={'width':'35rem'},
            header="Component Key",
            icon="primary",
            id = 'key_toast',
            is_open = False
        )
    ])
        


    # Combine it together
    data_div = html.Div(
        [
            html.Div(
                        [   
                            dbc.Row([
                                dbc.Col(
                                    [
                                        dbc.Row([
                                        dbc.Col([
                                            html.H6("Eastbound"),
                                            dbc.Container([
                                                dbc.Row([
                                                    dbc.Col(html.Div([t]),width=1),
                                                    dbc.Col(html.Div([generate_train_img('eastbound',t, trial_number = trial_number, data_type=button_id)])),
                                                ], style = {"border":"1px black solid", "width":"100%", "background-color": eastbound_display_info[t]}
                                                ) for t in dataset_info[f"eastbound_{button_id}_examples"]
                                            ])
                                        ]),
                                        dbc.Col([
                                            html.H6("Westbound"),
                                            dbc.Container([
                                                dbc.Row([
                                                    dbc.Col(html.Div([t]),width=1),
                                                    dbc.Col([generate_train_img('westbound',t, trial_number = trial_number, data_type=button_id)]),
                                                ], style = {"border":"1px black solid", "height":"100%", "background-color": westbound_display_info[t]}
                                                ) for t in dataset_info[f"westbound_{button_id}_examples"]
                                            ])
                                        ]),
                                        ])
                                    ]
                                ),
                            ])
                        ],style={'margin-left':'1rem', 'margin-bottom':'1rem', 'margin-top':'2rem', 'width':'100%'}
                    ),
            html.Div([key_card], style = {'margin-top':'2rem', 'margin-left':'1rem'})
        ]
    )


    if not any([n_test, n_data_lake]):
        return data_div, False, False
    else:
        if button_id == 'test':
            selection = [True, False]
        elif button_id == 'data_lake':
            selection = [False, True]
        else:
            selection = [False, False]
        
    
        return (data_div, selection[0], selection[1])



@callback(
    [
        Output("all_data_display", "children"),
        Output("all_data_button", "active"),
    ],
    [
        Input("all_data_button", "n_clicks")
    ],
)
def display_all_data(n_clicks):
    
    # if not n_clicks:
    #     return (html.Div([]), False)
        
    button_id = "data_lake"

    # Fetch list of covered examples
    with open('src/data/meta_data/working_data.json') as meta_data:
        dataset_info = json.load(meta_data)
    print(dataset_info)
    pos_list = dataset_info['current_coverage']

    # Display the dataset
    eastbound_display_info = dict([(example,"#1f77b4") if example in pos_list else (example,"white")
                                   for example in dataset_info[f'eastbound_{button_id}_examples']])
    

    westbound_display_info = dict([(example,"#1f77b4") if example in pos_list else (example,"white")
                                   for example in dataset_info[f'westbound_{button_id}_examples']])

    trial_number = dataset_info['trial_number']

    # Display the key
    car_shapes = ['bucket.gif', 'u_shaped.gif', 'rectangle.gif', 'ellipse.gif', 'hexagon.gif']
    roof_shapes = ['flat.gif', 'jagged.gif', 'peaked.gif']
    load_shapes = ['hexagon_.gif', 'diamond.gif', 'inverse_triangle.gif', 'circle.gif', 'rectangle_.gif', 'triangle.gif']
    extra_shapes = ['double.gif', 'example.gif']


    key_card = html.Div([

        dbc.Button(
            "Show Key",
            id="auto-toast-toggle",
            color="primary",
            className="mb-3",
            n_clicks=0,
        ),

        dbc.Toast(
            [
                dbc.Row(
                    [   
                        dbc.Col([
                            html.H6("Car Shapes"),
                            dbc.Container([
                                dbc.Row([
                                    dbc.Col([generate_component_img(comp), html.P([comp[:-4]])]),
                                ]) for comp in car_shapes
                            ])
                        ]),
                        dbc.Col([
                            html.H6("Load Shapes"),
                            dbc.Container([
                                dbc.Row([
                                    dbc.Col([generate_component_img(comp), html.P([comp[:-4]])]),
                                ]) for comp in load_shapes
                            ])
                        ]),
                        dbc.Col([
                            html.H6("Roof Shapes"),
                            dbc.Container([
                                dbc.Row([
                                    dbc.Col([generate_component_img(comp), html.P([comp[:-4]])]),
                                ]) for comp in roof_shapes
                            ])
                        ]),
                        dbc.Col([
                            html.H6("Misc."),
                            dbc.Container([
                                dbc.Row([
                                    dbc.Col([generate_component_img(comp), html.P([comp[:-4]])]),
                                ]) for comp in extra_shapes
                            ])
                        ]),
                    ]
                )
            ],
            style={'width':'35rem'},
            header="Component Key",
            icon="primary",
            id = 'key_toast',
            is_open = False
        )
    ])
        


    # Combine it together
    data_div = html.Div(
        [
            html.Div(
                        [   
                            dbc.Row([
                                dbc.Col(
                                    [
                                        dbc.Row([
                                        dbc.Col([
                                            html.H6("Eastbound"),
                                            dbc.Container([
                                                dbc.Row([
                                                    dbc.Col(html.Div([t]),width=1),
                                                    dbc.Col(html.Div([generate_train_img('eastbound',t, trial_number = trial_number, data_type=button_id)])),
                                                ], style = {"border":"1px black solid", "width":"100%", "background-color": eastbound_display_info[t]}
                                                ) for t in dataset_info[f"eastbound_{button_id}_examples"]
                                            ])
                                        ]),
                                        dbc.Col([
                                            html.H6("Westbound"),
                                            dbc.Container([
                                                dbc.Row([
                                                    dbc.Col(html.Div([t]),width=1),
                                                    dbc.Col([generate_train_img('westbound',t, trial_number = trial_number, data_type=button_id)]),
                                                ], style = {"border":"1px black solid", "height":"100%", "background-color": westbound_display_info[t]}
                                                ) for t in dataset_info[f"westbound_{button_id}_examples"]
                                            ])
                                        ]),
                                        ])
                                    ]
                                ),
                            ])
                        ],style={'margin-left':'1rem', 'margin-bottom':'1rem', 'margin-top':'2rem', 'width':'100%'}
                    ),
            html.Div([key_card], style = {'margin-top':'2rem', 'margin-left':'1rem'})
        ]
    )
    
    return (data_div, True)




@callback(
    Output("key_toast", "is_open"), [Input("auto-toast-toggle", "n_clicks")]
)
def open_toast(n):
    if n == 0:
        return no_update
    return True
