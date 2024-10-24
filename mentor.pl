:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pure_input)).
:- use_module(library(lists)).

% Dynamic predicates
:- dynamic chat_history/2.
:- dynamic session_id_counter/1.

% Initialize the session ID counter
session_id_counter(0).

% Define the CORS headers
add_cors_headers :-
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: POST, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type~n').

% Main entry point for the server
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Handle CORS preflight requests (OPTIONS request)
:- http_handler(root(chat), handle_options, [method(options)]).
handle_options(_Request) :-
    add_cors_headers,
    format('~n', []).

% Chat endpoint to handle POST requests
:- http_handler(root(chat), handle_chat, [method(post)]).
handle_chat(Request) :-
    add_cors_headers,
    format('Request received: ~w~n', [Request]),  % Log the incoming request

    % Read and parse JSON
    catch(http_read_json_dict(Request, JSONIn), Error, handle_error(Error)),
    format('Received JSON: ~w~n', [JSONIn]),

    % Extract parameters with fallback
    (   get_dict(query, JSONIn, Query) 
    ->  true
    ;   Query = ""  
    ),

    (   get_dict(session_id, JSONIn, SessionID) 
    ->  true
    ;   SessionID = ""  
    ),

    format('Session ID: ~w, Query: ~w~n', [SessionID, Query]),

    % Handle session ID creation if empty
    (   SessionID == "" 
    ->  generate_session_id(NewSessionID),
        assertz(chat_history(NewSessionID, []))  
    ;   NewSessionID = SessionID
    ),

    chat_history(NewSessionID, CurrentHistory),
    NewHistory = [Query | CurrentHistory],
    retract(chat_history(NewSessionID, CurrentHistory)),
    assertz(chat_history(NewSessionID, NewHistory)),

    % Sample responses based on the query
    (   Query == "list prompts"
    ->  list_prompts(Prompts),
        reply_json_dict(_{response: Prompts})
    ;   Query == "all advice"
    ->  all_financial_advice(AllAdvice),
        reply_json_dict(_{response: AllAdvice})
    ;   (   financial_advice(Query, Response)
        ->  reply_json_dict(_{response: Response, history: NewHistory, session_id: NewSessionID})
        ;   reply_json_dict(_{error: "Unknown query."})
        )
    ).

% Error handling
handle_error(Error) :-
    format('Error during request: ~w~n', [Error]),
    reply_json_dict(_{error: "Internal server error."}).

% Generate unique session ID
generate_session_id(SessionID) :-
    retract(session_id_counter(Current)),
    NewID is Current + 1,
    assertz(session_id_counter(NewID)),
    atom_number(SessionID, NewID).

% Prompt listing
list_prompts(Prompts) :-
    findall(Prompt, available_prompt(Prompt), AllPrompts),
    numbered_list(AllPrompts, 1, Prompts).

available_prompt("investment").
available_prompt("investment risk").
available_prompt("investment returns").
available_prompt("types of investments").
available_prompt("savings").
available_prompt("emergency fund").
available_prompt("best savings accounts").
available_prompt("debt").
available_prompt("debt repayment").
available_prompt("budgeting").
available_prompt("retirement").
available_prompt("insurance").

% Create a numbered list from the prompts
numbered_list([], _, []).
numbered_list([H|T], N, [N-H|NumberedTail]) :-
    N1 is N + 1,
    numbered_list(T, N1, NumberedTail).

% Financial advice predicates
financial_advice("investment", "Consider diversifying your investments in stocks, bonds, and mutual funds.").
financial_advice("savings", "Put your money into a high-yield savings account.").
financial_advice("debt", "Pay off high-interest debts first.").
financial_advice("budgeting", "Track your income and expenses to create a realistic budget.").
financial_advice("retirement", "Start saving early for retirement and consider a diversified portfolio.").

:- debug.

% Start the server on port 8000
:- initialization(start_server(8000)).
