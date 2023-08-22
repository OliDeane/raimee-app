from dash import Dash, dcc, html, Input, Output, no_update
import plotly.express as px
import plotly.graph_objs as go
import pandas as pd
import numpy as np
import json

app = Dash(__name__)

data = [[0.00, -4.46, 0.81],
     [4.46, 0.000, -1.42],
     [-0.81, 1.42, 0.00]]


def generate_matrix(data, shape = None):

    fig = px.imshow(data, x=['v0', 'v1', 'v2'], y=['v0', 'v1', 'v2'], text_auto=True,
                    color_continuous_scale='oranges')

    fig.update_xaxes(side="top")
    fig.update_coloraxes(showscale=False)
    fig.update_layout(clickmode='event+select')

    if shape:
        fig.update_layout(shapes=[shape])
    
    return fig

app.layout = html.Div(
    [
        dcc.Graph(
            id = 'heatmap',
            figure=generate_matrix(data),
            style={"height": "500px", "width" : "500px"}
        )
    ]
)

# Define callback function
@app.callback(
    Output("heatmap", "figure"),
    [Input("heatmap", "clickData")]
)
def update_heatmap(clickData):

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

        fig = generate_matrix(data = data, shape=shape)

    return fig


pp.run_server(debug=True)


