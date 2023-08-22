# Package imports
from dash import html, dcc, Input, Output, callback, State, no_update, callback_context
import dash_bootstrap_components as dbc
import dash_cytoscape as cyto
import json 
import pandas as pd
import os
import re

# Local imports
from utils.app_functions import fetch_mutag_arrays, parse_upload, generate_train_cytoscape, generate_train_img, generate_component_img

# Select Trial Number
datasetDropdown = html.Div(
    [
        html.Div(
            [
                
                dcc.Dropdown(
                    ['Trial_1', 'Trial_3'],
                    id = 'dataset_dropdown',
                    style={
                    'width': '100%',
                    'borderWidth': '1px',
                    'borderRadius': '5px',
                    'textAlign': 'center',
                    'margin-top':'1rem'
                    },
                    value='Trial_3'
                ),
            ], 
            style={'width':'80rem'},
        ),
        dcc.Store(id = 'selected-dataset-store', data = [], storage_type = 'memory')
    ],
)
selectDataCard = dbc.Card(
    [
        html.Div(
            [
                html.H5("Select Trial Number", className="card-subtitle", style={"margin-bottom":"rem"}),
                datasetDropdown,
                dbc.Button('Submit', id = 'dataSubmit-button', color = "primary", className='me-2', n_clicks=0, style={"margin-top":"1rem"})
            ],
        style={"margin-top":"1rem", "margin-left":"1rem", "margin-right":"1rem", "margin-bottom":"1rem"},
        )
    ],
    style={"margin-top":"1rem", "margin-left":"1rem", "margin-right":"1rem"},
)

graphical_switch = html.Div(
    [
        dbc.Switch(
                    value=False,
                    id='nl-switch',
                    label='Graph View',
                    input_class_name='bg-success'
                    ),
        dcc.Store(id='nl-store', data=[], storage_type='memory')
    ]
)
full_dataset_view = html.Div(
    [
        html.Div(id='display_graphical_dataset',children=[])
    ]
)

presentDataCard = dbc.Card(
    [   
        dbc.Accordion(
            [
                dbc.AccordionItem(
                    [
                        html.Div(id = 'relational_schema')
                    ],
                    title="Relational Schema"
                ),

                dbc.AccordionItem(
                    [
                        # dataset_button_group
                        html.Div(id = 'dataset_button_group')
                    ],
                    title="Table View"
                ),
                dbc.AccordionItem(
                    [
                        full_dataset_view
                    ],
                    title="Graphical View"
                ),
            ],
            start_collapsed=True,
            always_open=True

        ),
    ],
    style={"margin-top":"1rem", "margin-left":"1rem", "margin-right":"1rem"},
)


@callback(
    Output('selected-dataset-store', 'data'),
    Input('dataset_dropdown','value')
)
def store_selected_dataset(value):

    '''
    Load the json file containing pre-loaded dataset meta data.
    Returns the dictionary associated with the selected dataset.
    '''

    # Load in meta_data on all pre-loaded datasets
    if not value:
        return no_update
    
    # trial_number = int(value[-1])
    trial_number = value[-1]
    with open('src/data/meta_data/meta_data.json') as meta_data:
        selected_data = json.load(meta_data)
    selected_data = selected_data[str(trial_number)]
    

    # Save selected data to a file
    with open('src/data/meta_data/working_data.json', 'w') as f:
        json.dump(selected_data, f)

    return selected_data


@callback(
        Output('relational_schema','children'),
        [Input('selected-dataset-store','data')]
        )
def display_rel_schema(meta_data):
    
    schema_img_path = meta_data['rel_schama_img_path'] 

    rel_schame_div = html.Div([
        html.H4(meta_data['title'], className="card-title", style={"margin-left":"1rem", "margin-top":"1rem"}),
        html.H6(meta_data['description'],className="card-subtitle", style={"margin-bottom":"1rem", "margin-left":"1rem"}),
        dbc.Row([
            html.Div([
            html.Img(src=schema_img_path)
        ],
        style={'margin-left':'35rem', 'margin-bottom':'1rem', 'margin-top':'2rem'})
        ],
        align='center'
        )

    ])

    return rel_schame_div

@callback(Output('dataset_button_group', 'children'),
          Input('selected-dataset-store','data'))
def generate_button_group(meta_data):
    '''Generates the button group for the table view'''
    if not meta_data:
        return no_update

    data_folder_path = meta_data['data_folder_path']

    # Ge all the csv files in the given data folder
    files = os.listdir(data_folder_path)

    # Define the names of each table
    table_names = ['trains','cars']
    button_options = [{"label": name, "value":name} for name in table_names]

    button_group = html.Div(
        [
            dbc.RadioItems(
                id="dataset_radios",
                className="btn-group",
                inputClassName="btn-check",
                labelClassName="btn btn-outline-primary",
                labelCheckedClassName="active",
                options = button_options,
                value=table_names[0],
            ),
            html.Div(id="output"),
        ],
        className="radio-group",
    )

    return button_group

@callback(
    Output("output", "children"),
    [Input("dataset_radios", "value"),
    Input('selected-dataset-store','data')]
)
def display_data_table(value, datraset_info):

    data_path = datraset_info['data_folder_path'] + f'/{value}.csv'

    df = pd.read_csv(data_path)
    return html.Div(
        [
            dbc.Table.from_dataframe(df.head(8), striped=True, bordered=True, hover=True)
        ],
        style={"margin-top":"1rem", "margin-left":"1rem", "margin-right":"1rem"},
    )

@callback(
    Output("display_graphical_dataset", "children"),
    [Input('selected-dataset-store','data')]
)
def display_graphical_dataset(dataset_info):
    if not dataset_info:
        return no_update

    trial_number = int(dataset_info['trial_number'])
    
    car_shapes = ['bucket.gif', 'u_shaped.gif', 'rectangle.gif', 'ellipse.gif', 'hexagon.gif']
    roof_shapes = ['flat.gif', 'jagged.gif', 'peaked.gif']
    load_shapes = ['hexagon_.gif', 'diamond.gif', 'inverse_triangle.gif', 'circle.gif', 'rectangle_.gif', 'triangle.gif']
    extra_shapes = ['double.gif', 'example.gif', 'open.gif', 'closed.gif']


    key_card = dbc.Toast(
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
        style={'width':'40rem'},
        header="Component Key",
        icon="primary",
    )


    return html.Div(
        [
            dbc.Row([
                dbc.Col(
                    [
                        dbc.Row(
                            [   
                                dbc.Col([
                                    html.H6("Eastbound"),
                                    dbc.Container([
                                        dbc.Row([
                                            dbc.Col([generate_train_img('eastbound',t, trial_number = trial_number, data_type = 'data_lake')]),
                                        ]) for t in dataset_info["eastbound_data_lake_examples"]
                                    ])
                                ]),
                                dbc.Col([
                                    html.H6("Westbound"),
                                    dbc.Container([
                                        dbc.Row([
                                            dbc.Col([generate_train_img('westbound',t, trial_number = trial_number, data_type = 'data_lake')]),
                                        ]) for t in dataset_info["westbound_data_lake_examples"]
                                    ])
                                ]),
                            ]
                        )
                    ], width = 5
                ),
                dbc.Col([
                    # html.Div(id='graphical_representation', children=[]),
                    html.Div([key_card])
                ])
            ])
        ],style={'margin-left':'1rem', 'margin-bottom':'1rem', 'margin-top':'2rem'}
    ),


@callback(
    Output('nl-store', 'data'),
    [Input('nl-switch', 'value')]
)
def update_output(value):
    return value

@callback(
    Output('graphical_representation', 'children'),
    [Input('selected-dataset-store', 'data'),
    Input('train2-button', 'n_clicks'),
     Input('train3-button', 'n_clicks'),
    Input('train5-button', 'n_clicks'),
    Input('train6-button', 'n_clicks'),
    Input('train9-button', 'n_clicks'),
    ]
)
def present_cytoscape(dataset_info, n_clicks, n_clicks2, n_clicks3, n_clicks4, n_clicks5):
    if not n_clicks:
        return dbc.Toast(
                [],
                header="Graphical Repretentation",
                icon="primary",
                is_open=True,
                style={"width": '40rem', "height":"15rem"},

            )    
    
    # Identify which train was pressed on 
    train_num = re.search(r'train(\d+)', callback_context.triggered[0]['prop_id'])
    train_num = int(train_num.group(1))

    train_data_dict = {
        3 : {
            'car_1':
                {'attributes':{'length':'short', 'shape':'bucket', 'load_shape':'triangle'},
                'x':100,
                'y':100},

            'car_2': 
                {'attributes':{'length':'short','shape':'rectangle','load_shape':'triangle', 'roof':'open', 'roof_type':'arch'},
                'x':400,
                'y':100}
        },

        2: {
            'car_1':
                {'attributes':{'length':'short', 'shape':'bucket', 'load_shape':'triangle'},
                'x':100,
                'y':100},

            'car_2': 
                {'attributes':{'length':'short','shape':'rectangle','load_shape':'triangle', 'roof':'open', 'roof_type':'arch'},
                'x':400,
                'y':100},
            
            'car_3': 
                {'attributes':{'length':'short','shape':'rectangle','roof':'open', 'load_type':'circle'},
                'x':700,
                'y':100},
            
            'car_4': 
                {'attributes':{'length':'long','shape':'rectangle','roof':'closed', 'roof_type':'flat'},
                'x':1000,
                'y':100},
            
        },

        5: {
            'car_1':
                {'attributes':{'length':'short', 'shape':'hexagon', 'load_shape':'circle'},
                'x':100,
                'y':100},

            'car_2': 
                {'attributes':{'length':'short','shape':'hexagon','load_shape':'square', 'roof':'closed', 'roof_type':'arch'},
                'x':400,
                'y':100}
        },

        6: {
            'car_1':
                {'attributes':{'length':'short', 'shape':'bucket', 'load_shape':'triangle'},
                'x':100,
                'y':100},

            'car_2': 
                {'attributes':{'length':'short','shape':'rectangle','load_shape':'triangle', 'roof':'open', 'roof_type':'arch'},
                'x':400,
                'y':100},
            
            'car_3': 
                {'attributes':{'length':'long','shape':'rectangle','roof':'closed', 'roof_type':'flat'},
                'x':700,
                'y':100},
            
        },
        9: {
            'car_1':
                {'attributes':{'length':'short', 'shape':'hexagon', 'load_shape':'circle'},
                'x':100,
                'y':100},

            'car_2': 
                {'attributes':{'length':'short','shape':'hexagon','load_shape':'square', 'roof':'closed', 'roof_type':'arch'},
                'x':400,
                'y':100}
        },
    }

    # Get the label for display and for adding to the train_img path
    if int(train_num) <= 5:
        label = 'eastbound'
    elif int(train_num) > 5:
        label = 'westbound'
    else:
        print('Error: unacceptable train number') 

    # Fill the Graph Rep. Card with cytoscape and train image
    cytoscape = generate_train_cytoscape(train_data_dict[train_num])
    train_img = generate_train_img(label, train_num, str(dataset_info['trial_number']))

    return dbc.Toast(
                [html.P(f'Label: {label}'), train_img, cytoscape],
                header="Graphical Repretentation",
                icon="primary",
                is_open=True,
                style={"width": '40rem'},
            )   
        
