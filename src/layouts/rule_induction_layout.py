from dash import html
import dash_bootstrap_components as dbc

from components.rule_induction.induction_header import header_card
from components.rule_induction.data_display_card import displayDataCard
from components.rule_induction.main_interaction import interaction_card
from components.rule_induction.assert_example import assertExamplesCard


model_editor = html.Div([

    dbc.Row([
        dbc.Col([
            interaction_card
        ]),
        
        dbc.Col([
            displayDataCard,
        ])
    
    ]),
])

