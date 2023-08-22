/* pycuity.pl
 *  (C) 2023  Oliver Deane, Oliver Ray, Steve Moyle.
 *  @copyright Oliver Deane, Oliver Ray, Steve Moyle.
 *
 *
 *   @tbd
 *   1. Initialization predicates for facilitating ALEPH's induce_incremental command through Python. 
 *   2. Return hypotheses and scores for incremental rule induction with ALEPH. 
 *
 *   */

:- dynamic('$acuity_global'/2). %+Name, -Term
:- include('./acuity.pl').
:- include('./pycuity_utils.pl').
:- use_module(library(http/json)).

get_hypothesis_as_list(TheoryList) :- 
    findall(
        Atom, 
        ('$aleph_global'(theory,theory(_,_,Clause,_,_)), 
        compound(Clause), 
        Clause = (Head :- Body),
        bodyList(Clause, BodyList), Atom=BodyList), TheoryList).

get_hypothesis_head(Head) :- 
    '$aleph_global'(theory,theory(_,_,Clause,_,_)), 
    compound(Clause), 
    Clause = (Head :- Body).

initialise_incremental() :-
    clean_up,
    retractall('$aleph_global'(search_stats,search_stats(_,_))),
    store_values([interactive,portray_search,proof_strategy,mode]),
    set(portray_search,false),
    set(proof_strategy,sld),
    set(interactive,true),
    record_settings.

initialise_shaping_example(E,N) :-
    once(record_example(check,pos,E,N)),
    retractall('$aleph_global'(example_selected,
                    example_selected(_,_))),
    asserta('$aleph_global'(example_selected,
                    example_selected(pos,N))).

get_indexed_bottom_clause(ClauseList, BodyListWithIndex) :-
    numbervars(ClauseList, 0, _),
    ClauseList = [0-Head | BodyListWithIndex],
    format('~n~t~4|~q :-~n', [Head]),
    true.

% This searches for best clause for a single example (i.e., run on the reduce command)
% I have edited this so that it returns the best clause
find_clause(Search, RClause, RClauseList):-
    set(stage,reduction),
    set(searchstrat,Search),
    p_message('reduce'),
    reduce_prelims(L,P,N),
    asserta('$aleph_search'(openlist,[])),
    get_search_settings(S),
    arg(4,S,_/Evalfn),
    get_start_label(Evalfn,Label),
    ('$aleph_sat'(example,example(Num,Type)) ->
        '$aleph_example'(Num,Type,Example),
        asserta('$aleph_search'(selected,selected(Label,(Example:-true),
                            [Num-Num],[])));
        asserta('$aleph_search'(selected,selected(Label,(false:-true),[],[])))),
    arg(13,S,MinPos),
    interval_count(P,PosLeft),
    PosLeft >= MinPos,
    '$aleph_search'(selected,selected(L0,C0,P0,N0)),
    add_hyp(L0,C0,P0,N0),
        ('$aleph_global'(max_set,max_set(Type,Num,Label1,ClauseNum))->
        BestSoFar = Label1/ClauseNum;
        ('$aleph_global'(best,set(best,Label2))->
            BestSoFar = Label2/0;
            BestSoFar = Label/0)),
        asserta('$aleph_search'(best_label,BestSoFar)),
    p1_message(1,'best label so far'), p_message(1,BestSoFar),
        arg(3,S,RefineOp),
    stopwatch(StartClock),
        (RefineOp = false ->
                get_gains(S,0,BestSoFar,[],false,[],0,L,[1],P,N,[],1,Last,NextBest),
        update_max_head_count(0,Last);
        clear_cache,
        interval_count(P,MaxPC),
        asserta('$aleph_local'(max_head_count,MaxPC)),
        StartClause = 0-[Num,Type,[],false],
                get_gains(S,0,BestSoFar,StartClause,_,_,_,L,[StartClause],
                P,N,[],1,Last,NextBest)),
        asserta('$aleph_search_expansion'(1,0,1,Last)),
    get_nextbest(S,_),
    asserta('$aleph_search'(current,current(1,Last,NextBest))),
    search(S,Nodes),
    stopwatch(StopClock),
    Time is StopClock - StartClock,
        '$aleph_search'(selected,selected(BestLabel,RClause,PCover,NCover)),
    retract('$aleph_search'(openlist,_)),
    add_hyp(BestLabel,RClause,PCover,NCover),
    p1_message(1,'clauses constructed'), p_message(1,Nodes),
    p1_message(1,'search time'), p_message(1,Time),
    p_message(1,'best clause'),
    pp_dclause(RClause), bodyList(RClause,RClauseList),
    show_stats(Evalfn,BestLabel),
    update_search_stats(Nodes,Time),
    record_search_stats(RClause,Nodes,Time),
    noset(stage),
    !.


pick_with_python(Clause, STerm) :-
    search_shaping:clause_with_index(Clause, ClauseListWithIndex),
    % repeat,
    search_shaping:check(STerm, ClauseListWithIndex),
    search_shaping:make_constraint_spec(STerm,ClauseListWithIndex,AcuityConstraintSpec),
    % format('~n~t~8|Proposed constraint is: ~n', []),
    format('~n~t~8|     ~q~n', [AcuityConstraintSpec]),
    % format('~n~t~8|-> \'ok.\' to accept constraint or \'none.\' to abort.~n'),
    % read(Action),
    % (   Action = ok
    assert(user:'$acuity_global'(constraint_spec, AcuityConstraintSpec), DBRef),
    debug(constraint_spec, 'Constraint added at ref:  ~q.', [DBRef]),
    !,
    true.

% For removing specific, or all constraints. To remove all, pass in _,_. 
clear_constraints(Predicates,MustOrNot) :- 
    retractall('$acuity_global'(constraint_spec,constraint_clause(_,true,[Predicates],MustOrNot,_,_))),
    retractall('$pycuity_constraint_violations'(X)).


% Fetch the existing constraints
fetch_constraints(Constraints, MustOrNots) :- 
    findall(Predicates, '$acuity_global'(constraint_spec,constraint_clause(_,true,[Predicates],_,_,_)), Constraints),
    findall(Ms, '$acuity_global'(constraint_spec,constraint_clause(_,true,_,Ms,_,_)), MustOrNots).


% Fecth the search space and accompanying constraints/constraint violations
% fetch_search_space_constraints(Search_space, Constraint_violations) :-
%     findall(X, '$pycuity_search_store'(X), Ss),
%     findall(Y, '$pycuity_constraint_violations'(Y,_,_), Cv),
%     apply_term_string(Ss, Search_space),
% 	apply_term_string(Cv, Constraint_violations).


% This allows for extraction of the Cv that has been found
fetch_search_space_constraints(Search_space, Constraint_violations, MustOrNots, Constraint_predicates) :-
    findall(X, '$pycuity_search_store'(X), Ss),
    findall(Y, '$pycuity_constraint_violations'(Y,_,_), Cvs),
    findall(MoN, '$pycuity_constraint_violations'(_,MoN,_), MoNs),
    findall(Cp, '$pycuity_constraint_violations'(_,_,Cp), Cps),
    apply_term_string(Ss, Search_space),
	apply_term_string(Cvs, Constraint_violations),
    apply_term_string(MoNs, MustOrNots),
    apply_term_string(Cps, Constraint_predicates).


% Store ALEPH output in a json file
save_json_object(FilePath, Search_space, Constraint_violations) :-
    % Create a JSON object
    JSON = json{
        search_space: Search_space,
        constraint_violations: Constraint_violations
    },

    % Open the file in write mode
    open(FilePath, write, Stream),

    % Convert the JSON object to a string
    json_write(Stream, JSON, [width(0)]),

    % Close the file
    close(Stream).

save_search_constraints(Filename) :- 
	fetch_search_space_constraints(Ss, Cv),
	apply_term_string(Ss, Search_space),
	apply_term_string(Cv, Constraint_violations),
	save_json_object(Filename,Search_space, Constraint_violations).


% Convert compiunds in the list to strings to enable storage in json
apply_term_string([], []).
apply_term_string([Term|Rest], [String|StringRest]) :-
    term_string(Term, String),
    apply_term_string(Rest, StringRest).

% This is the clause used to display when a clause is found. Use this to shtore clause options.
% show_clause(Flag,Label,Clause,Nodes):-
%     broadcast(clause(Flag,Label,Clause,Nodes)),
% 	p_message('-------------------------------------'),
% 	(Flag=good -> p_message('good clause');
% 		(Flag=sample-> p_message('selected from sample');
% 			p_message('found clause'))),
% 	pp_dclause(Clause),
% 	(setting(evalfn,Evalfn)-> true; Evalfn = coverage),
% 	show_stats(Evalfn,Label),
% 	p1_message('clause label'), p_message(Label),
% 	p1_message('clauses constructed'), p_message(Nodes),
% 	p_message('-------------------------------------').
