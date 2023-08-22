# package imports
import dash_bootstrap_components as dbc
from dash import html, dcc, Input, Output, callback



PLOTLY_LOGO = "https://images.plot.ly/logo/new-branding/plotly-logomark.png"

navbar = dbc.Navbar(
    dbc.Container(
        [
            html.A(
                # Use row and col to control vertical alignment of logo / brand
                dbc.Row(
                    [
                        dbc.Col(html.Img(src=PLOTLY_LOGO, height="30px")),
                        dbc.Col(dbc.NavbarBrand("I-ILP", className="ms-2")),
                    ],
                    align="left",
                    className="g-0",
                ),
                href="https://plotly.com",
                style={"textDecoration": "none"},
            )
        ],
        fluid=True
    ),
    color="dark",
    dark=True,
)

tab_selected_style = {
    'borderTop': '1px solid #d6d6d6',
    'borderBottom': '1px solid #d6d6d6',
    'backgroundColor': '#119DFF',
    'color': 'white',
    'padding': '6px'
}
tab_style = {
    'borderBottom': '1px solid #d6d6d6',
    'padding': '6px',
    'fontWeight': 'bold'
}
tabs_styles = {
    'height': '44px'
}
