import dash_bootstrap_components as dbc
from dash import Input, Output, html, dcc, no_update, callback
import plotly.express as px
import plotly.graph_objs as go

import subprocess
import json
import os
import numpy as np
from utils.app_functions import *
import plotly.tools

def generate_stacked_bar(pos_score, neg_score):
    trace1 = go.Bar(
    x=['correct predictions'],
    y=[pos_score],
    name='positive'
    )
    trace2 = go.Bar(
        x=['coverage'],
        y=[neg_score],
        name='negative'
    )

    return html.Div([
    dcc.Graph(id='bar_plot',
              figure=go.Figure(data=[trace1, trace2],
                               layout=go.Layout(barmode='stack'))
              )
    ])

def generate_confusion_matrix(pos_score, neg_score):
    z = [[pos_score, neg_score],
     [5-pos_score, 5-neg_score]]

    fig = px.imshow(z, text_auto=True)
    return fig


# Rule extraction tab
diff_matrix_path = '/assets/images/diff_matrix.png'


data = [[0.00, -4.46, 0.81],
     [4.46, 0.000, -1.42],
     [-0.81, 1.42, 0.00]]
def generate_matrix(data, shape = None):

    fig = px.imshow(data, x=['v0', 'v1', 'v2'], y=['v0', 'v1', 'v2'], text_auto=True,
                    color_continuous_scale='oranges')

    fig.update_xaxes(side="top")
    fig.update_coloraxes(showscale=False)
    fig.update_layout(clickmode='event+select', margin=dict(l=20, r=20, t=20, b=20))
    
    if shape:
        fig.update_layout(shapes=[shape])
    
    return fig

differenceMatrixCard = dbc.Card(
    [
        dbc.CardBody(
            [
                html.H4("Difference Matrix", className="card-title"),
                html.H6("Select versions of the model to compare. Colored according to accuracy.", className="card-subtitle"),
                dcc.Graph(
                    id = 'heatmap',
                    figure=generate_matrix(data),
                    style={"height": "450px", "width" : "450px"}
                )
            ]
        ),
    ],
    style={"margin-top":"1rem", "margin-left":"1rem", "width": "30rem", "height":"35rem"},
)

barCard = dbc.Card(
    [
        dbc.CardBody(
            [
                html.H4("Compare Rules", className="card-title"),
                html.H6("Induced rules and associated coverage for selected model versions", className="card-subtitle"),
                html.Div(id='bar-fig')
                
            ]
        ),
    ],
    style={"margin-top":"1rem", "margin-left":"1rem", "margin-right":"2rem", "width": "69rem", "height":"55rem"}
)

# Model cards for each saved model


saved_models_path = 'src/components/rule_induction/dynamic_files/saved_models.json'
with open(saved_models_path, 'r') as f:
    saved_models = json.load(f)

# Select Trial Number
left_dropdown = html.Div(
    [
        html.Div(
            [
                
                dcc.Dropdown(
                    ['model_1', 'model_2', 'model_3', 'model_4', 'model_5'],
                    id = 'left_model_dropdown',
                    style={
                    'width': '100%',
                    'borderWidth': '1px',
                    'borderRadius': '5px',
                    'textAlign': 'center',
                    'margin-top':'1rem'
                    },
                ),
            ], 
            style={'width':'30rem', 'margin-top':'1rem', 'margin-left':'1rem'},
        ),
    ],
)

# Select Trial Number
right_dropdown = html.Div(
    [
        html.Div(
            [
                
                dcc.Dropdown(
                    ['model_1', 'model_2', 'model_3', 'model_4', 'model_5'],
                    id = 'right_model_dropdown',
                    style={
                    'width': '100%',
                    'borderWidth': '1px',
                    'borderRadius': '5px',
                    'textAlign': 'center',
                    'margin-top':'1rem'
                    },
                ),
            ], 
            style={'width':'30rem', 'margin-top':'1rem', 'margin-left':'1rem'},
        ),
    ],
)




left_model_card = dbc.Card([
        html.Div([html.H4("Select Model 1")], style = {'margin-left':'1rem','margin-top':'1rem'}),
        left_dropdown,
        html.Div(id='left-model-info', children=[])
    ],
    style={"margin-top":"2rem", "margin-left":"2rem", "margin-right":"1rem", "width": "40rem", "height":"60rem"}
)

right_model_card = dbc.Card([
        html.Div([html.H4("Select Model 2")], style = {'margin-left':'1rem','margin-top':'1rem'}),
        right_dropdown,
        html.Div(id='right-model-info', children=[])
    ],
    style={"margin-top":"2rem", "margin-left":"1rem", "margin-right":"2rem", "width": "40rem", "height":"60rem"}
)


@callback(
    Output("heatmap", "figure"),
    Output("bar-fig", "children"),
    [Input("heatmap", "clickData")]
)
def update_heatmap(clickData):
    '''Returns an updated heatmap with the selected cell highlighted'''
    if not clickData:
        no_update
        
    if clickData and clickData["points"]:
        point = clickData["points"][0]

        x0, x1 = (int(point['x'][1])-0.5, int(point['x'][1])+0.5)
        y0, y1 = (int(point['y'][1])-0.5, int(point['y'][1])+0.5)

        shape = {
            "type": "rect",
            "x0": x0,
            "y0": y0,
            "x1": x1,
            "y1": y1,
            "line": {"color": "black", "width": 4},
            "opacity":0.5,
        }

        label = point['x'][1]
        fig = generate_matrix(data = data, shape=shape)
        img_path = f'/assets/images/model_comparison_v0v{label}.png'

    return fig, html.Div([html.Img(src=img_path)])

@callback(
    Output('right-model-info', 'children'),
    Input('right_model_dropdown','value')
)
def display_model_info(value):
    if not value:
        return no_update
    saved_models_path = 'src/components/rule_induction/dynamic_files/saved_models.json'
    with open(saved_models_path, 'r') as f:
        saved_models = json.load(f)
    
    model_id = value[-1]
    all_model_ids = list(saved_models.keys())
    if value[-1] not in all_model_ids:
        return html.P('This model does not seem to exist. Please select another.')

    pos_score, neg_score = (int(saved_models[model_id]['coverage']['pos']), int(saved_models[model_id]['coverage']['neg']))
    saved_constraints = saved_models[model_id]['constraints']

    acc = ((saved_models[model_id]['coverage']['pos']+(5-saved_models[model_id]['coverage']['neg']))/10)*100

    return html.Div([
        html.H4('Hypothesis:'),
        dcc.Markdown(saved_models[model_id]['hypothesis']),
        html.H4('Coverage'),
        html.P(f"Positives: {saved_models[model_id]['coverage']['pos']} | Negatives: {saved_models[model_id]['coverage']['neg']} | Accuracy: {str(acc)}"),
        html.H4('Constraints'),
        html.Ul([html.Li(x) for x in saved_constraints]),
        dcc.Graph(id="right-matrix")],
        style = {"margin-top":"1rem", "margin-left":"1rem"})

@callback(
    Output("right-matrix", "figure"), 
    Input("right_model_dropdown", "value"))
def filter_right_heatmap(value):
    if value:
        saved_models_path = 'src/components/rule_induction/dynamic_files/saved_models.json'
        with open(saved_models_path, 'r') as f:
            saved_models = json.load(f)
    
        model_id = value[-1]

        pos_score, neg_score = (int(saved_models[model_id]['coverage']['pos']), int(saved_models[model_id]['coverage']['neg']))
        # coverage_stack = generate_confusion_matrix(pos_score, neg_score)
        z = [[pos_score, neg_score],
        [5-pos_score, 5-neg_score]]

        x = ['Actual Eastbound','Actual Westbound']
        y = ['Pred Eastbound','Pred Westbound']
        
        fig = px.imshow(z, x=x, y=y, color_continuous_scale='Viridis', aspect="auto", text_auto=True)
        fig.update_xaxes(side="top")
        
        return fig
    
@callback(
    Output('left-model-info', 'children'),
    Input('left_model_dropdown','value')
)
def display_left_model_info(value):
    if not value:
        return no_update
    saved_models_path = 'src/components/rule_induction/dynamic_files/saved_models.json'
    with open(saved_models_path, 'r') as f:
        saved_models = json.load(f)
    
    model_id = value[-1]
    all_model_ids = list(saved_models.keys())
    if value[-1] not in all_model_ids:
        return html.P('This model does not seem to exist. Please select another.')

    pos_score, neg_score = (int(saved_models[model_id]['coverage']['pos']), int(saved_models[model_id]['coverage']['neg']))

    saved_constraints = saved_models[model_id]['constraints']
    print(saved_constraints)

    acc = ((saved_models[model_id]['coverage']['pos']+(5-saved_models[model_id]['coverage']['neg']))/10)*100

    return html.Div([
        html.H4('Hypothesis:'),
        dcc.Markdown(saved_models[model_id]['hypothesis']),
        html.H4('Coverage'),
        html.P(f"Positives: {saved_models[model_id]['coverage']['pos']} | Negatives: {saved_models[model_id]['coverage']['neg']} | Accuracy: {str(acc)}"),
        html.H4('Constraints'),
        html.Ul([html.Li(x) for x in saved_constraints]),
        dcc.Graph(id="left-matrix")],
        style = {"margin-top":"1rem", "margin-left":"1rem"})

@callback(
    Output("left-matrix", "figure"), 
    Input("left_model_dropdown", "value"))
def filter_left_heatmap(value):
    if value:
        saved_models_path = 'src/components/rule_induction/dynamic_files/saved_models.json'
        with open(saved_models_path, 'r') as f:
            saved_models = json.load(f)
    
        model_id = value[-1]

        pos_score, neg_score = (int(saved_models[model_id]['coverage']['pos']), int(saved_models[model_id]['coverage']['neg']))
        # coverage_stack = generate_confusion_matrix(pos_score, neg_score)
        z = [[pos_score, neg_score],
        [5-pos_score, 5-neg_score]]

        x = ['Actual Eastbound','Actual Westbound']
        y = ['Pred Eastbound','Pred Westbound']
        
        fig = px.imshow(z, x=x, y=y, color_continuous_scale='Viridis', aspect="auto", text_auto=True)
        fig.update_xaxes(side="top")
        return fig
    
