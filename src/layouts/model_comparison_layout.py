import dash_bootstrap_components as dbc
from dash import html

from components.model_comparison.model_comparison import differenceMatrixCard, barCard, left_model_card, right_model_card


compare_models = html.Div([
    dbc.Row([
        dbc.Col([
            # differenceMatrixCard,
            left_model_card
        ]),
        
        dbc.Col([
            # barCard
            right_model_card
        ])
    
    ])
])