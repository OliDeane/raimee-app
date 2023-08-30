# package imports
from dash import html, Dash, dcc, Input, Output, callback, State, no_update, dash_table, ctx, callback_context, Patch, ALL
import dash_bootstrap_components as dbc
from swiplserver import PrologMQI, PrologThread
import json
from utils.swiplserver_functions import induce_with_swiplserver, run_inference_with_swiplserver
import re
import os
import glob

# local imports 
from components.common_components.common_components import navbar, tab_style, tab_selected_style, tabs_styles
from layouts import intro_layout, dataset_overview_layout, model_comparison_layout, rule_induction_layout

from utils.app_functions import *
from utils.prolog_functions import add_integrity_constraint, remove_mode_declaration, reset_file
from utils.swiplserver_functions import *
from utils.cytoscape_functions import generate_search_cytoscape_elements, get_alternative_hypotheses, get_good_clauses

def load_assert_examples_table(dataset):
    path = f'src/data/{dataset}/raw_data/ae_file.csv'
    # df = read_last_file_as_dataframe(path)
    df = pd.read_csv(path)    
    # Drop needless row
    df = df.drop(0)
    df.insert(0,'.', ["\U0001f50e"] * len(df['class'])) 

    unknown = df[df['class'] == 'unknown']
    pos = df[df['class'] == 'pos']
    neg = df[df['class'] == 'neg']

    return df, unknown, pos, neg

app = Dash(external_stylesheets=[dbc.themes.BOOTSTRAP], suppress_callback_exceptions=True)
server=app.server

app.layout = html.Div(
    [
        navbar,
        html.Div(
            [
                dcc.Tabs(id="tabs-styled-with-inline", value='intro-tab', children=[
                dcc.Tab(label='Introduction', value='intro-tab', style=tab_style, selected_style=tab_selected_style),
                dcc.Tab(label='Dataset Overview', value='tab-2', style=tab_style, selected_style=tab_selected_style),
                # dcc.Tab(label='Induced Rules', value='tab-3', style=tab_style, selected_style=tab_selected_style),
                dcc.Tab(label='Rule Induction', value='tab-4', style=tab_style, selected_style=tab_selected_style),
                dcc.Tab(label='Model Comparison', value='tab-5', style=tab_style, selected_style=tab_selected_style),
                # dcc.Tab(label='Export Model', value='tab-6', style=tab_style, selected_style=tab_selected_style),
                ], 
                style=tabs_styles),
                html.Div(id='tabs-content-inline')
            ]
        )
    ]
)

@callback(Output('tabs-content-inline', 'children'),
              Input('tabs-styled-with-inline', 'value'))
def render_content(tab):
    if tab == 'intro-tab':
        return intro_layout.intro

    elif tab == 'tab-2':
        return dataset_overview_layout.dataset_overview
    
    elif tab == 'tab-4':
        return rule_induction_layout.model_editor
    
    elif tab == 'tab-5':
        return model_comparison_layout.compare_models
    
    # elif tab == 'tab-6':
    #     return export_model_layout.export_model

@callback([
        Output("selected-shaping-example", "children"),
           Output("selected-shaping-example-store", "data"),
           Output('shaping-example-input', 'value')], 
          [Input("submit-shaping-val", "n_clicks")],
          State("shaping-example-input","value"))
def display_clause_and_acuity_options(n_clicks, text_input):
    '''Returns: 1) a div containing the bottom clause, reduced clause and ACUITY options
    2) a stable dictionary with the selected example number and the extracted bottom clause
    3) An empty string that will replace the input string in the input box. NOTE: the bottom clause list this returns 
    within the dictionary is the original one produced by ALEPH. The BC displayed is de-duplicated so the indices do not
     match with that of the original BC. '''
    if not n_clicks:
        return no_update
    

    if "http" in text_input:
        value = re.search(r'(\d+)\.gif$', text_input)
        value = int(value.group(1))
    else:
        value = int(text_input)
    

    # Check it is a permissable example number
    if int(value) < 1 or int(value) > 5:
        clause_and_options_list = html.Div(
                    [
                        html.Hr(),
                        html.Div(
                            [   
                                html.H4("Sorry, that example number is not valid"),
                                html.Div([html.P("The system cannot accept the provided example number. Please insert an Eastbound example from the right-hand list.")],
                                         style = {'margin-top':'1rem', 'margin-bottom':'1rem'}),
                                html.Hr(),
                            ],
                        ),
                        dbc.Button(id='extent-button', children='Edit', color = "info", className='me-2', n_clicks=0),
                        dbc.Button(id='pick-button', children='Constrain', color = "primary", className='me-2', n_clicks=0),
                        dbc.Button(id='alt_clause-button', children='Other Hypotheses', color = "warning", className='me-2', n_clicks=0),
                        dbc.Button(id = 'searchviz-button', children='Explore', color = "secondary", className='me-2', n_clicks=0),
                        dbc.Button(id = 'clear_constraints-button', children='Manage Constraints', color = "dark", className='me-2', n_clicks=0),
                        dbc.Button(id = 'save-model-button', children='Save Model', color = "info", className='me-2', n_clicks=0),
                        html.Div(id="selected-options-output", children=[], style={'top-margin':'0rem'}),
                        html.Div(id="searchviz-modal")
                    ]
                )
        return clause_and_options_list, {}, str(value)

    # Fetch current dataset path
    selected_dataset = get_selected_dataset()
    pycuity_path = "data/acuityFiles/pycuity"
    # pos_data_path = f"'src/data/{selected_dataset}/pred_pos/{selected_dataset}'" 
    pos_data_path = f"'src/data/train_trials/rule{selected_dataset}/train{selected_dataset}'" 


    try:
        bottom_clause_list_raw, reduced_clause = get_bottom_clause(main_prolog_thread, pycuity_path, pos_data_path, value)

        # Convert the bottom clause to natural language (without the comma at the end)
        bottom_clause_list_raw = [map_to_nl(i)[:-1] for i in bottom_clause_list_raw]

        # bottom_clause_list_raw = [''.join(ch for ch in s if not ch.isupper()) for s in bottom_clause_list_raw]

        # Get the reduced clause string and add asterisks to allow for italicised variables
        _, pretty_reduced_clause = get_nl_bottom_clause(main_prolog_thread, pycuity_path, pos_data_path, value)
        pretty_reduced_clause = add_asterisks_to_capitalized(pretty_reduced_clause) 

        bottom_clause_for_initial_printing = [i + ' | ' for i in bottom_clause_list_raw]
        bottom_clause_for_initial_printing[-1] = bottom_clause_for_initial_printing[-1][:-2] + '.....'

        
        # Fetch the search space if we haven't generated one already for the given example
        search_space_constraints = main_prolog_thread.query('fetch_search_space_constraints(Search_space, Constraint_violations, MustOrNots, Constraint_predicates)')
        search_space, constraint_violations = (search_space_constraints[0]['Search_space'], search_space_constraints[0]['Constraint_violations'])
        
        constraint_mustornots, constraint_predicates = (search_space_constraints[0]['MustOrNots'], search_space_constraints[0]['Constraint_predicates'])
        constraint_definitions = [m + '->' + cp for m,cp in zip(constraint_mustornots, constraint_predicates)]
        
        # Remove all the number variables from constraint defintions and replace with an '_'. This enables the detection of the unique predicates
        constraint_definitions = [re.sub(r'_(\d+)', '_', string) for string in constraint_definitions]

        # Store the constraints in a dictionary
        search_constraint_dict = {}
        for constraint_key, constraint_value in zip(constraint_definitions, constraint_violations):
            # Check if key is already in the dictionary
            if constraint_key in search_constraint_dict:
                search_constraint_dict[constraint_key].append(constraint_value)
            else:
                #create new list with current value
                search_constraint_dict[constraint_key] = [constraint_value]

        search_constraint_dict['constraint_violations'] = constraint_violations # This adds a section for all constraint_violations
        search_constraint_dict['search_space'] = search_space # This adds a section for all constraint_violations

        # Save constraints and violations to an aleph output path - will depend on the example selected so 
        # need to add a store as input!!!!
        aleph_output_path = 'src/components/all_dynamic_files/search_spaces/search_and_constraints_example2.json'
        with open(aleph_output_path, "w") as f:
            json.dump(search_constraint_dict, f, indent=4)


        search_constraint_path = f"components/all_dynamic_files/search_spaces/search_and_constraints_example{value}.json"

        if os.path.isfile(search_constraint_path):
            # update the constraint violations only
            with open(search_constraint_path, "r") as f:
                search_constraint_json = json.load(f)
            search_constraint_json['constraint_violations'] = constraint_violations

            with open(search_constraint_path, "w") as f:
                json.dump(search_constraint_json, f, indent=4)
        
        else:
            # write both the search_space and constraint_violations
            # search_constraint_json = {'constraint_violations':constraint_violations, 'search_space':search_space}
            with open(search_constraint_path, "w") as f:
                json.dump(search_constraint_dict, f, indent=4)


        # Run inference using the reduced clause for use by extent
        inference_file_path = "'src/components/all_dynamic_files/inference_file.pl'"

        # Write the positive hypothesis to a prolog file
        # Note the [1:-1] to remove the single quotes from the string (included for swiplserver compatibility)
        write_to_prolog_file(inference_file_path[1:-1], [reduced_clause], dataset = '3')

        # Run logical inference to ascertain list of positive predictions
        positive_predictions = run_inference_with_swiplserver(main_prolog_thread, inference_file_path)

        #For now, store as a list of numbers, rather than id - so pop the final integer from the string
        positive_predictions = list(set([int(p[-1]) for p in positive_predictions]))

        # Compute coverage: assume all positives are 1-5 and negatives are 6-10
        positive_predictions = [i for i in positive_predictions if i != 0]
        pos_coverage, neg_coverage = (sum(i <= 5 for i in positive_predictions), sum(i > 5 for i in positive_predictions))
        acc = ((pos_coverage+(5-neg_coverage))/10)*100
        coverage_display = html.P(f"Pos: {pos_coverage} | Neg: {neg_coverage} | Accuracy: {str(acc)}")

        # Save coverage to current working.json
        with open('src/data/meta_data/working_data.json') as f:
            meta_data = json.load(f)
        meta_data['current_coverage'] = positive_predictions
            # Save selected data to a file
        with open('src/data/meta_data/working_data.json', 'w') as f:
            json.dump(meta_data, f)

        # Add information to an output Div
        clause_and_options_list = html.Div(
                    [
                        html.Hr(),
                        html.Div(
                            [   
                                html.H4('Bottom Clause'),
                                html.Div([html.P(bottom_clause_for_initial_printing)], className='custom-scrollbar', style={"height":"4rem"}),
                                html.Hr(),
                            ],
                        ),
                        html.Div(
                            [   
                                html.H4('Our Working Hypothesis'),
                                dcc.Markdown(pretty_reduced_clause),
                                html.H4('Coverage'),
                                html.P(html.P(coverage_display)),
                                html.Hr(),
                            ],
                            # style={"height":"25rem", "overflow-y": "scroll", "margin-bottom":"1rem"}
                        ),
                        dbc.Button(id='extent-button', children='Edit', color = "info", className='me-2', n_clicks=0),
                        dbc.Button(id='pick-button', children='Constrain', color = "primary", className='me-2', n_clicks=0),
                        dbc.Button(id='alt_clause-button', children='Other Hypotheses', color = "warning", className='me-2', n_clicks=0),
                        dbc.Button(id = 'searchviz-button', children='Explore', color = "secondary", className='me-2', n_clicks=0),
                        dbc.Button(id = 'clear_constraints-button', children='Manage Constraints', color = "dark", className='me-2', n_clicks=0),
                        dbc.Button(id = 'save-model-button', children='Save Model', color = "info", className='me-2', n_clicks=0),
                        html.Div(id="selected-options-output", children=[], style={'top-margin':'0rem'}),
                        html.Div(id="searchviz-modal")
                    ]
                )

        return clause_and_options_list, \
            {'current_example_number':value, 'bottom_clause_list':bottom_clause_list_raw, 'positive_predictions':positive_predictions, \
            'reduced_hypothesis':pretty_reduced_clause, 'coverage':{'pos':pos_coverage, 'neg':neg_coverage}},\
                str(value)
    except Exception as e:
        print("Error in Induction")
        print(e)
        clause_and_options_list = html.Div(
                    [
                        html.Hr(),
                        html.Div(
                            [   
                                html.H4("We couldn't find a rule."),
                                html.Div([html.P("The model couldn't find a rule satisfying all those constraints."),
                                        html.P("Remove some constratints and try again.")], style = {'margin-top':'1rem', 'margin-bottom':'1rem'}),
                                html.Hr(),
                            ],
                        ),
                        dbc.Button(id='extent-button', children='Edit', color = "info", className='me-2', n_clicks=0),
                        dbc.Button(id='pick-button', children='Constrain', color = "primary", className='me-2', n_clicks=0),
                        dbc.Button(id='alt_clause-button', children='Other Hypotheses', color = "warning", className='me-2', n_clicks=0),
                        dbc.Button(id = 'searchviz-button', children='Explore', color = "secondary", className='me-2', n_clicks=0),
                        dbc.Button(id = 'clear_constraints-button', children='Manage Constraints', color = "dark", className='me-2', n_clicks=0),
                        dbc.Button(id = 'save-model-button', children='Save Model', color = "info", className='me-2', n_clicks=0),
                        html.Div(id="selected-options-output", children=[], style={'top-margin':'0rem'}),
                        html.Div(id="searchviz-modal")
                    ]
                )
        return clause_and_options_list, {}, str(value)

@callback(
        Output('searchviz-modal','children'),
        [Input('searchviz-button', 'n_clicks')]
)
def list_constraint_options(n_clicks):

    if not n_clicks:
        return no_update

    # This will have to be generated depending on the selected example
    aleph_output_path = 'src/components/all_dynamic_files/search_spaces/search_and_constraints_example2.json'

    # Fetch all the search constraints
    with open(aleph_output_path) as f:
        aleph_output = json.load(f)
    
    constraint_definitions = list(aleph_output.keys())

    # remove the search_space as an option
    constraint_definitions.remove('search_space')

    constraint_options = [{"label":cd, "value":cd} for cd in constraint_definitions]

    constraint_display = html.Div([
        html.Div(
            [
                dbc.Label("Define Constraints"),
                dbc.Checklist(
                    options=constraint_options,
                    value=constraint_definitions,
                    id="constraint-checklist-input",
                ),
            ]
        )
    ])

    search_viz_modal = html.Div(
        [
            dbc.Modal(
                [
                    dbc.ModalHeader(
                        [
                            dbc.ModalTitle("Example Exploration"),
                        ],
                        close_button=False
                    ),
                    dbc.ModalBody(
                        [   
                            dbc.Row(
                                [
                                    dbc.Col([
                                        html.H6('Class Label: Unknown'),
                                        html.H6('Current Prediction: Negative'),
                                        html.Div(id='searchviz-cytoscape')
                                    ]),
                                    dbc.Col([
                                        html.H6("User-defined Constraints"),
                                        html.Hr(),
                                        constraint_display
                                    ])
                                ]
                            )
                            
                        ]
                    ),
                    
                ],
                id="modal-centered",
                size="xl",
                centered=True,
                is_open=True,
            ),
        ]
    )

    return search_viz_modal

@callback(
        Output('searchviz-cytoscape','children'),
        [Input('constraint-checklist-input', 'value')]
)
def display_search_cytoscape(checklist_value):
    if not checklist_value:
        return no_update
    
    search_cytoscape_elements = generate_search_cytoscape_elements('src/components/all_dynamic_files/search_spaces/search_and_constraints_example2.json',
                                                                   checklist_value)


    search_cytoscape = cyto.Cytoscape(
        id='cytoscape-layout-1',
        elements=search_cytoscape_elements,
        style={'width': '50rem', 'height': '40rem'},
        layout={
            'name': 'breadthfirst',
            'roots': '[id = "0"]',
            'directed':'true'
        },
        stylesheet=[
        # Group selectors
        {
            'selector': 'node',
            'style': {
                "width":"310",
                "height":"310",
                "content": "data(label)",
                "font-size": "36",
                'text-halign':'center',
                'text-valign':'center',
                'shape':'square',
                "text-wrap": "wrap",
                'backgroundColor': '#3D9970'
            }
        },
        {
                'selector': '[label *= "~"]',
                'style': {
                    "width":"310",
                    "height":"310",
                    "content": "data(label)",
                    "font-size": "42",
                    'text-halign':'center',
                    'text-valign':'center',
                    'shape':'square',
                    "text-wrap": "wrap",
                    'background-color': '#FF4136',
                }
            }
        ]
    )
    return search_cytoscape

@callback(Output("selected-options-output", "children"),
           [Input("selected-shaping-example-store", "data"),
            Input("pick-button", "n_clicks"),
            Input("extent-button", "n_clicks"),
            Input("alt_clause-button", "n_clicks"),
            Input("clear_constraints-button", "n_clicks"),
            Input("save-model-button", "n_clicks")])
def display_bottom_clause(data, pick, extent, alternatives, clear_constraints, save_model):

    # Use the ctx.triggered_id to determine which button was pressed
    button_id = ctx.triggered_id

    if not button_id:
        return no_update
    
    elif button_id == 'pick-button':

        # bottom_clause_list_raw, reduced_clause = get_bottom_clause(main_prolog_thread, pycuity_path, pos_data_path, value)
        bottom_clause_list_raw = data['bottom_clause_list']
        reduced_clause_string = data['reduced_hypothesis'].replace("*", "")

        # Add the html.Br() to the bottom clause list to display it properly
        bottom_clause_list = []
        for i in bottom_clause_list_raw:
            bottom_clause_list.append(i)
            bottom_clause_list.append(html.Br())

        bottom_clause_list = [reduced_clause_string, html.Br(), "------------", html.Br()] + bottom_clause_list
        lower_case_bc_list = [''.join(ch for ch in s if not ch.isupper()) for s in bottom_clause_list_raw]

        # Remove duplicate values and create a checklist dictionary for constraint selection
        unique_values = set()
        bc_dict = {idx+1:value for idx,value in enumerate(lower_case_bc_list)}
        bc_options = [{"label":value, "value":(key,value)} for key, value in bc_dict.items() if value not in unique_values and not unique_values.add(value)] # Remove dupliocate values
        # bc_options = [{"label":cd, "value":idx+1} for idx,cd in enumerate(lower_case_bc_list)]


        bottom_clause_checklist = dbc.RadioItems(
                    options=bc_options,
                    value='',
                    id="bc-checklist-input",
                )

        interactive_bottom_clause = html.Div(
                [
                    html.Hr(),
                    html.Div(
                        [   
                            # html.P(id='selection-container', children=bottom_clause_list),
                            # dcc.Input(id='selection-target', value='', style=dict(display='none')),
                            bottom_clause_checklist
                        ],
                        className='custom-scrollbar'
                    ),
                    dbc.Button(id='bc-must-button', children='Must', color = "success", className='me-2', n_clicks=0),
                    dbc.Button(id='bc-mustnot-button', children='Must Not', color = "danger", className='me-2', n_clicks=0),
                    html.P(id='bc-added-predicate', children=[], style={'top-margin':'0rem'}),
                    html.P(id='bc-removed-predicate', children=[], style={'top-margin':'0rem'}),
                    html.P(id='bc-constraint-confirmed', children=[], style={'top-margin':'0rem'}),
                    html.P(id='bc-mustnot-confirmation', children=[], style={'top-margin':'0rem'}),
                ]
            )

        return interactive_bottom_clause
    
    elif button_id == 'extent-button':
        

        reduced_clause_string = data['reduced_hypothesis'].replace("*", "")

        interactive_rule_editor = html.Div(
                [
                    html.Hr(),
                    html.Div(
                        [   
                            html.P(id='selection-container', children=reduced_clause_string),
                            dcc.Input(id='selection-target', value='', style=dict(display='none')),
                        ],
                        className='custom-scrollbar'
                    ),
                    dbc.Button(id='rule-must-button', children='Must', color = "success", className='me-2', n_clicks=0),
                    dbc.Button(id='rule-mustnot-button', children='Must Not', color = "danger", className='me-2', n_clicks=0),
                    html.P(id='rule-added-predicate', children=[], style={'top-margin':'0rem'}),
                    html.P(id='rule-removed-predicate', children=[], style={'top-margin':'0rem'}),
                    html.P(id='rule-constraint-confirmed', children=[], style={'top-margin':'0rem'}),
                    html.P(id='rule-mustnot-confirmation', children=[], style={'top-margin':'0rem'}),
                ]
            )

        return interactive_rule_editor

    elif button_id == 'alt_clause-button':

        alternative_hypotheses = get_good_clauses("components/all_dynamic_files/good_clauses.txt")
        
        alt_hyp_display = html.Div(
            [
                html.P(hyp) for hyp in alternative_hypotheses
            ],
            className = "custom-scrollbar",
            style={"height":"16rem", "margin-top":"1rem", "margin-bottom":"1rem"}
        )
        return alt_hyp_display, 

    elif button_id == 'clear_constraints-button':

        # If the clear constraints button is pressed, we us Patch() to generate a list of constraints accompanied by buttons. 
        # When that button is pressed, the relevant constraint is removed. - we send the patch to the remove-constraints
        # callback which executes the relevent swiplserver functions to delete the constraint and display
        # confirmation on the right hand column. 

        constraints = main_prolog_thread.query("fetch_constraints(Constraints,MustOrNots).")
        mustornots = constraints[0]['MustOrNots']
        constraints = constraints[0]['Constraints']

        if constraints == []:
            return html.Div(['No constraints defined'], style = {'margin-top':'1rem'})

        constraints = convert_integers_to_string(constraints)
        constraints = [f"{item['functor']}({','.join(item['args'])})" for item in constraints]
        full_constraints = [f'{m}:{pred}' for pred,m in list(zip(constraints,mustornots))]


        patched_children = Patch()

        for i in full_constraints:
            new_button = dbc.Button(f"Remove",
            id={"type": "city-filter-dropdown", "index": i},
            )
            new_constraint = html.Div(
                [
                    dbc.Row(
                        [
                            dbc.Row([
                                dbc.Col(html.P(i)),
                                dbc.Col(new_button),
                                dbc.Col(html.Div(id='constraint-container-output-div')) # Confirm deletion
                            ])
                        ]
                    )
                ], style = {'margin-top':'1rem'}
            )

            # new_constraint = html.Div(
            #     [
            #         html.Div(
            #             [
            #                 html.P(i), 
            #                 new_button, 
            #                 html.Div(id='constraint-container-output-div')
            #             ], style = {'margin-top': "0.5rem"}
            #         )
            #     ],
            #     style = {'display':'inline-block', 'margin-top':'1rem'}
            # )

            patched_children.append(new_constraint)
        

        # clear_all_constraints(main_prolog_thread)
        # return constraint_display #html.Div([html.P(string) for string in full_constraints])
        return patched_children

    elif button_id == 'save-model-button':

        # fetch the constraints (should be in a function - repeated code from above)
        constraints = main_prolog_thread.query("fetch_constraints(Constraints,MustOrNots).")
        mustornots = constraints[0]['MustOrNots']
        constraints = constraints[0]['Constraints']

        if constraints != []:
            constraints = convert_integers_to_string(constraints)
            constraints = [f"{item['functor']}({','.join(item['args'])})" for item in constraints]
            full_constraints = [f'{m}:{pred}' for pred,m in list(zip(constraints,mustornots))]
        else:
            full_constraints = ['No constraints defined.']


        saved_models_path = 'src/components/all_dynamic_files/saved_models.json'

        with open(saved_models_path, 'r') as f:
            saved_models = json.load(f)

        saved_model_dict = {
            'hypothesis':data['reduced_hypothesis'],
            'coverage':data['coverage'],
            'constraints':full_constraints,
        }
        model_id = len(list(saved_models.keys()))+1

        saved_models[model_id] = saved_model_dict  # or whatever
        with open(saved_models_path, 'w') as f:
            json.dump(saved_models, f)

        return html.P('Model Saved Successfully.')

@callback(
    Output("constraint-container-output-div", "children"),
    [Input({"type": "city-filter-dropdown", "index": ALL}, "n_clicks")]
)
def display_output(values):

    if not ctx.triggered_id:
        return no_update
    
    # Get the selected constraint
    constraint_id = ctx.triggered_id
    print(constraint_id)
    removable_constraint = constraint_id['index']

    # Remove constraint with pycuity
    predicate = removable_constraint.split(':')[1]
    mustornot = removable_constraint.split(':')[0]
    main_prolog_thread.query(f"clear_constraints({predicate},{mustornot}).")
    print(removable_constraint)
    return html.Div(html.P(f'Removed: {removable_constraint}'))


@callback(Output("selected-pick-output", "children"),
           [Input("extent-button", "n_clicks"),
            Input("selected-shaping-example-store", "data")])
def display_extent(n_clicks, data):
    if not n_clicks:
        return no_update
    
    # Extract information required to run induction on the selected data
    unknown, pos, neg = load_assert_examples_table('train')

    ae_tables = {'unknown':unknown, 'positive':pos, 'negative':neg}
    extent_df = ae_tables['positive']

    extent_table_div = html.Div(
            [
                html.Hr(),
                html.Div(
                    [   
                        html.H4('Clause Coverage'),
                        dash_table.DataTable(
                            id="assertExamplesTable",
                            columns=[{"name": i, "id": i} for i in extent_df.columns],
                            data=extent_df.head(5).to_dict('records'),
                            style_cell={'textAlign':'left'},
                            # style_as_list_view=True,
                            style_header={
                                'font_family': 'Times New Roman',
                                'backgroundColor': 'white',
                                'fontWeight': 'bold'
                            },
                        )
                    ],
                    # style={"height":"15rem", "overflow-y": "scroll", "margin-bottom":"1rem"}
                ),
            ]
        )

    return extent_table_div

@callback(
    Output('rule-removed-predicate', 'children'),
    [Input('rule-mustnot-button', 'n_clicks'),
     Input('selected-shaping-example-store','data')],
    [State('selection-target', 'value')],)
def removePredicate(n_clicks, data, value):
    if not n_clicks:
        return no_update

    if value == '':
        return html.Span(f"Sorry! That predicate wasn't recognise. Please try again.", style=dict(color='red'))
    # Remove car from front of string to make it compatable with the raw_bottom_clause that we use to create constraint indices
    # value = remove_car_at_beginning(value)

    # Fetch current dataset path
    selected_dataset = get_selected_dataset()

    # predicate = re.sub(r'\(.+', '', value)

    # Assert the mustnot constraint
    pycuity_path = "data/acuityFiles/pycuity"
    pos_data_path = f"'src/data/train_trials/rule{selected_dataset}/train{selected_dataset}'" 

    # generate the must_not constraint from the selected predicate
    bottom_clause_list = data['bottom_clause_list']

    predicate_indices = find_bottom_clause_indices(bottom_clause_list, value)


    # Catch when the users selection is not valid
    if predicate_indices == []:
        return html.Span(f"Sorry! That predicate wasn't recognise. Please try again.", style=dict(color='red'))
    
    constraint_predicate = f'must_not({predicate_indices})'
    print(constraint_predicate)
    assert_pick_constraint(main_prolog_thread, pycuity_path, pos_data_path, data['current_example_number'], constraint_predicate)
    

    # Save event to the event log
    save_event_to_log('MustNot Constraint Added', value)


    return html.Span(f'Removed Predicate: "{value}"', style=dict(color='blue'))

@callback(
    Output('rule-added-predicate', 'children'),
    [Input('rule-must-button', 'n_clicks'),
     Input('selected-shaping-example-store','data')],
    [State('selection-target', 'value')],)
def removePredicate(n_clicks, data, value):
    if not n_clicks:
        return no_update

    # Add constraint to the background knowledge
    
    # Fetch current dataset path
    selected_dataset = get_selected_dataset()

    # predicate = re.sub(r'\(.+', '', value)
    # remove the car from the beginning of the selected string if it exists to make compatable with the raw bottom_clause list used to define constraints
    # value = remove_car_at_beginning(value)

    # Assert the mustnot constraint
    pycuity_path = "data/acuityFiles/pycuity"
    # pos_data_path = f"'src/data/{selected_dataset}/pred_pos/{selected_dataset}'" 
    pos_data_path = f"'src/data/train_trials/pred_pos/rule{selected_dataset}/train{selected_dataset}'" 

    # generate the must_not constraint from the selected predicate
    bottom_clause_list = data['bottom_clause_list']
    predicate_indices = find_bottom_clause_indices(bottom_clause_list, value)
    
    if predicate_indices == []:
        return html.Span(f"Sorry! That predicate wasn't recognise. Please try again.", style=dict(color='red'))

    constraint_predicate = f'must({predicate_indices})'
    assert_pick_constraint(main_prolog_thread, pycuity_path, pos_data_path, data['current_example_number'], constraint_predicate)
    

    # Save event to the event log
    save_event_to_log('Must Constraint Added', value)

    return html.Span(f'Added Predicate: "{value}"', style=dict(color='blue'))

@callback(
    Output('bc-removed-predicate', 'children'),
    [Input('bc-mustnot-button', 'n_clicks'),
     Input('selected-shaping-example-store','data'),
     Input('bc-checklist-input', 'value')]
    # [State('selection-target', 'value')],
    )
def removePredicate(n_clicks, data, value):
    if not n_clicks:
        return no_update
    # value = value[0]
    # Remove car from front of string to make it compatable with the raw_bottom_clause that we use to create constraint indices
    # value = remove_car_at_beginning(value)

    # Fetch current dataset path
    selected_dataset = get_selected_dataset()

    # predicate = re.sub(r'\(.+', '', value)

    # Assert the mustnot constraint
    pycuity_path = "data/acuityFiles/pycuity"
    # pos_data_path = f"'src/data/{selected_dataset}/pred_pos/{selected_dataset}'" 
    pos_data_path = f"'src/data/train_trials/rule{selected_dataset}/train{selected_dataset}'" 

    # generate the must_not constraint from the selected predicate
    bottom_clause_list = data['bottom_clause_list']

    # predicate_indices = find_bottom_clause_indices(bottom_clause_list, value)


    # Catch when the users selection is not valid
    # if predicate_indices == []:
    #     return html.Span(f"Sorry! That predicate wasn't recognise. Please try again.", style=dict(color='red'))
    
    # We only accept single predicate constraints so we index the first constraint
    # The input value is a tuple with the predicate idx and predicate string
    predicate_idx = value[0]
    predicate_string = value[1]

    constraint_predicate = f'must_not([{predicate_idx}])'
    assert_pick_constraint(main_prolog_thread, pycuity_path, pos_data_path, data['current_example_number'], constraint_predicate)
    

    # Save event to the event log
    save_event_to_log('MustNot Constraint Added', constraint_predicate)


    return html.Span(f'Defined Constraint: "{predicate_string}"', style=dict(color='blue'))

@callback(
    Output('bc-added-predicate', 'children'),
    [Input('bc-must-button', 'n_clicks'),
     Input('selected-shaping-example-store','data'),
     Input('bc-checklist-input', 'value')])
    # [State('selection-target', 'value')],)
def addPredicate(n_clicks, data, value):
    if not n_clicks:
        return no_update

    # value = value[0]
    # Add constraint to the background knowledge
    
    # Fetch current dataset path
    selected_dataset = get_selected_dataset()

    # predicate = re.sub(r'\(.+', '', value)
    # remove the car from the beginning of the selected string if it exists to make compatable with the raw bottom_clause list used to define constraints
    # value = remove_car_at_beginning(value)

    # Assert the mustnot constraint
    pycuity_path = "data/acuityFiles/pycuity"
    pos_data_path = f"'src/data/train_trials/pred_pos/rule{selected_dataset}/train{selected_dataset}'" 

    # generate the must_not constraint from the selected predicate
    bottom_clause_list = data['bottom_clause_list']
    # predicate_indices = find_bottom_clause_indices(bottom_clause_list, value)
    
    # if predicate_indices == []:
    #     return html.Span(f"Sorry! That predicate wasn't recognise. Please try again.", style=dict(color='red'))

    predicate_idx = value[0]
    predicate_string = value[1]
    constraint_predicate = f'must([{predicate_idx}])'
    assert_pick_constraint(main_prolog_thread, pycuity_path, pos_data_path, data['current_example_number'], constraint_predicate)
    

    # Save event to the event log
    save_event_to_log('Must Constraint Added', value)

    return html.Span(f'Defined Constraint: "{predicate_string}"', style=dict(color='blue'))


@callback(
    [Output('hypothesis-disp', 'children'),
     Output('interactive-hypothesis-disp', 'children')],
    Input('induce-button2', 'n_clicks')
)
def induce_all(n_clicks):
    '''
    Induces the hypothesis via external python scripts.
    Returns a dictionary with hypotheses for the pos and negative class.
    Also saves the dictionary to a json file for access from another tab.
    Returns model info (accuracy) and formatted generated rules box.
    '''
    if not n_clicks:
        return no_update

    # Extract information required to run induction on the selected data
    selected_dataset = get_selected_dataset()

    # Use swipleserver to induce hypotheses for the positive and negative classes - DEPRICATED, NEED TO EDIT PATHS
    acuity_path = "data/acuityFiles/pycuity"
    pos_data_path = f"'src/data/{selected_dataset}/pred_pos/{selected_dataset}'" # Note the single quotes - REQUIRED
    neg_data_path = f"'src/data/{selected_dataset}/pred_neg/{selected_dataset}'" # Note the single quotes - REQUIRED

    # pos_rule_list = induce_with_swiplserver(main_prolog_thread, acuity_path, pos_data_path, label = 'pos')
    # neg_rule_list = induce_with_swiplserver(main_prolog_thread, acuity_path, neg_data_path, label = 'neg')

    pos_rule_list, neg_rule_list = induce_with_swiplserver(main_prolog_thread, negative_prediction_thread, acuity_path, pos_data_path, neg_data_path)


    # Run inference for the Assert Examples Card.
    inference_file_path = "'src/components/all_dynamic_files/inference_file.pl'"
    positive_predictions_file_path = "components/rule_induction/positive_preds.json"

    # Write the positive hypothesis to a prolog file
    # Note the [1:-1] to remove the single quotes from the string (included for swiplserver compatibility)
    write_to_prolog_file(inference_file_path[1:-1], pos_rule_list, selected_dataset)

    # Rune logical inference to ascertain list of positive predictions
    positive_predictions = run_inference_with_swiplserver(main_prolog_thread, inference_file_path)

    # save the positive predictions to a json file for access by the Assert Examples card
    with open(positive_predictions_file_path, "w") as outfile:
        json.dump({'pos_preds':positive_predictions}, outfile)

    

    # Create a div containing the hypothesis for both pos and neg rules
    hypothesisDisplay = html.Div(
        [
            html.Hr(),
            html.Div(
                [
                    html.H5("Label: Positive", className="card-subtitle"),
                    html.Br(), 
                    html.P(children=html.Div([html.P(string) for string in pos_rule_list]))
                ],
                style={'margin-top':'1rem'}
            ),
            html.Hr(),
            html.Div(
                [
                    html.H5("Label: Negative", className="card-subtitle"),
                    html.Br(), 
                    html.P(children=html.Div([html.P(string) for string in neg_rule_list]))
                ],
                style={'margin-top':'1rem'}
            ),

        ])

    interactiveHypothesis = html.Div(
        [
            html.Hr(),
            html.Div(
                [   
                    html.H5("Label: Positive", className="card-subtitle"),
                    html.Br(), 
                    html.P(id='selection-container', children=pos_rule_list),
                    dcc.Input(id='selection-target', value='', style=dict(display='none')),
                ],
                style={"height":"25rem", "overflow-y": "scroll", "margin-bottom":"1rem"}
            ),
            dbc.Button(id='submit', children='Add', color = "success", className='me-2', n_clicks=0),
            dbc.Button(id='remove-button', children='Remove', color = "danger", className='me-2', n_clicks=0),
            dbc.Button(id='constraint-button', children='Constraint', color = "warning", className='me-2', n_clicks=0),
            dbc.Button(id = 'reset-button-1', children='Reset', color = "secondary", className='me-2', n_clicks=0),
            html.P(id='removed-predicate', children=[], style={'top-margin':'0rem'}),
            html.P(id='constraint-confirmed', children=[], style={'top-margin':'0rem'}),
            html.P(id='added-predicate', children=[], style={'top-margin':'0rem'}),
        ]
    )
    
    return hypothesisDisplay, interactiveHypothesis


if __name__ == '__main__':
    '''We open two prolog threads. One is the main thread for running induction for the
    positive class. This is the thread used for induce_incremental functions.
    The second thread is used for running the negative prediction inference only. Negative
    predictions are not currently used for inference, and are only displayed in the 
    Generated Rules card. We currently define the main positive data path before initialising the app.
    This should be changed to allow for induction with the mutagenesis dataset.'''

    # Reset the saved_models file, the working_data file (that stores info about the selected trial) and file containing alternative clauses
    clear_json_file(path = 'src/components/all_dynamic_files/saved_models.json')
    clear_json_file(path = 'src/data/meta_data/working_data.json')
    clear_text_file(path = 'src/components/all_dynamic_files/good_clauses.txt')


    with PrologMQI() as mqi:
        with mqi.create_thread() as main_prolog_thread:
            with mqi.create_thread() as negative_prediction_thread:
                pycuity_path = "data/acuityFiles/pycuity"
                pos_data_path = "'src/data/train_trials/rule3/train3'"
                main_prolog_thread.query(f"['{pycuity_path}'].")
                main_prolog_thread.query(f"read_all({pos_data_path}).")
                
                # Delete all files for resetting the search space
                files = glob.glob('src/components/all_dynamic_files/search_spaces/*')
                for f in files:
                    os.remove(f)

                app.run_server(debug=True)