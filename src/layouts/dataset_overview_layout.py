from dash import html
from components.dataset_overview.dataset_overview import selectDataCard, presentDataCard

# Combine into dataset_overview variable
dataset_overview = html.Div([
    html.Div([
        selectDataCard,
        presentDataCard
    ]),
])

