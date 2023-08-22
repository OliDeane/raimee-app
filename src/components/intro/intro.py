#package imports
import dash_bootstrap_components as dbc
from dash import html, dcc

# introduction tab
introCard = dbc.Card(
    [
        dbc.CardBody(
            [
                html.H2("Refining Relational Rules; An A.I Model Explorer and Editor for Relational Datasets", className="card-title"),
                html.Br(),
                html.P("Thank you for taking the time to participate in our study. We are interested in understanding how end users can understand a \
                       (symbolic) machine learning model’s output and provide effective feedback. Your responses will only be used for the purposes of\
                        this study, which include informing the design of an interactive machine learning tool and contributing to a research paper. \
                       Your responses will be anonymised, meaning they won't be tied to your identity in any way. Your participation is entirely voluntary. \
                       You are welcome to stop at any point, and we will delete any responses given so far if requested to do so.", 
                    className="card-text"),
                html.Br(),

                html.H4("Study Overview", className="card-title"),
                html.P("Your task is to come up with rules that can be used to predict the direction of trains. You will first have a go generating \
                       rules on your own before collaborating with a machine learning system; the system will come up with potential rules and you \
                       will need to provide feedback that will guide it towards more accurate ones.", 
                    className="card-text"),
                html.Br(),
                html.H4("The Task", className="card-title"),
                html.P("You will be presented with a set of trains. Half are 'Eastbound', and half are 'Westbound'. Each train consists\
                        of a variable number of Carriages (we call them “Cars” in this study). Each car is composed of different components.\
                        Your task is to observe the composition of each train and come up with a rule that can be used to detect the \
                       Eastbound from the Westbound trains. For example:", 
                    className="card-text"),

                dcc.Markdown("*A train is Eastbound if it contains a car **B**. Car **B** has a flat roof and car **B** has 3 wheels.*"),
                dcc.Markdown("*A train is Eastbound if it contains a car **B** and a car **C**. Car **B** is circular and carries a\
                              triangle load. Car **C** has a peaked roof. Car **B** is positioned in front of Car **C**.*"),
                

                html.Br(),
                html.H4("Phase One (5 minutes)", className="card-title"),
                dcc.Markdown("**Press on the 'Rule Induction' tab.** In this phase, you will work together with a symbolic machine learning algorithm.\
                            This algorithm is trained on a subset of the labelled data so can produce some decent (but not perfect) rules. \
                             It will produce an initial rule, and you will have to give some feedback based on what you think is wrong with it.", 
                    className="card-text"),

                dcc.Markdown("**Press on the 'Rule Induction' tab.** In this phase, you will work together with a symbolic machine learning algorithm.\
                            This algorithm is trained on a subset of the labelled data so can produce some decent (but not perfect) rules. \
                             It will produce an initial rule, and you will have to give some feedback based on what you think is wrong with it.", 
                    className="card-text"),

                html.P("Here's how that works:"),
                html.P("Select an example for the ML model to focus on; either by dragging train images into the input box or typing in the relevant \
                       train number directly. The system will return a potential classification rule and some stats on the rule’s coverage and accuracy.\
                        It takes the form of “First Order Logic” – so consider the italicised capital letters as variables. ",
                    className="card-text"),
                
                html.P("At this point, you have a choice of actions:", className="card-text"),
                html.P("a.	EXTENT. Press the 'extent' button to view the examples that the system’s rule covers. (Hint: press the right-hand “DATA” \
                       button to see the covered examples highlighted in the list)", className="card-text"),
                html.P("b.	CONSTRAIN. Press 'constrain' to deliver feedback. Here, the system will present ‘predicates’ that the algorithm considered when producing\
                        the rule. Based on what you can see in the data, you can select predicates that you think should or should not appear in the \
                       classification rule. Simply highlight the relevant predicate from the list and press the “Must” OR “Must Not” button.", 
                       className="card-text"),
                html.P("c.	MANAGE: press the 'manage' button to view the constraints that you have defined previously. Here, you can also remove any \
                       constraints to see how that impacts the system’s output.",
                       className="card-text"),
                
                html.P("After each bit of feedback, press the “Submit” button again, to see how the system’s output has changed.",
                       className="card-text"),
                
                html.P("If you think your rule has improved, hit the “Save” button. Remember, you are optimising for accuracy, so when you see the accuracy score increase, \
                       it is worth saving your progress so you can access it later. Repeat these steps as many times as you’d like.",
                       className = "card-text"),
                
                
                html.Br(),

                html.H4("Phase Three (5 minutes)", className="card-title"),
                html.P("Press on the “Compare Models” tab. Compare your saved models (rules) by selecting different versions \
                       from the right- and left-hand dropdowns. Insert the most accurate into the dropdown at the bottom of the page. \
                       And that’s it, you’re done!", 
                    className="card-text"),
                
                html.Br(),
                html.Br(),
                html.H4("References", className="card-title"),
                html.P(["Alkan, Öznur, Dennis Wei, Massimiliano Matteti, Rahul Nair, Elizabeth M. Daly, \
                        and Diptikalyan Saha. 'FROTE: Feedback Rule-Driven Oversampling for Editing Models.' \
                        arXiv preprint arXiv:2201.01070 (2022).", html.Br(),
                    "Ray, O., & Moyle, S. (2021, November). Towards expert-guided elucidation of cyber attacks \
                        through interactive inductive logic programming. In 2021 13th International Conference on \
                            Knowledge and Systems Engineering (KSE) (pp. 1-7). IEEE.", html.Br(),
                    "Alkan, Öznur, Dennis Wei, Massimiliano Matteti, Rahul Nair, Elizabeth M. Daly, \
                        and Diptikalyan Saha. 'FROTE: Feedback Rule-Driven Oversampling for Editing Models.' \
                        arXiv preprint arXiv:2201.01070 (2022).", html.Br(), 
                    "Ray, O., & Moyle, S. (2021, November). Towards expert-guided elucidation of cyber attacks \
                        through interactive inductive logic programming. In 2021 13th International Conference on \
                            Knowledge and Systems Engineering (KSE) (pp. 1-7). IEEE.", html.Br()],
                    className="card-text")
                
            ]
        ),
    ],
    style={"margin-top":"1rem", "margin-left":"1rem", "margin-right":"1rem", "width": "103rem", "height":"90rem"},
)

