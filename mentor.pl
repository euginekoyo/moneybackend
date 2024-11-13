:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pure_input)).  % For enhanced input handling
:- use_module(library(lists)).  % For list processing

% CORS headers for cross-origin requests
add_cors_headers :-
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: POST, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type~n').

% Start the server on a dynamic port
start_server :-
    getenv('PORT', PortStr),
    atom_number(PortStr, Port),
    http_server(http_dispatch, [port(Port)]).

% Handle CORS preflight requests
:- http_handler(root(chat), handle_options, [method(options)]).

handle_options(_Request) :-
    add_cors_headers,
    format('~n', []).

% Main chat handler
:- http_handler(root(chat), handle_chat, [method(post)]).

handle_chat(Request) :-
    add_cors_headers,
    http_read_json_dict(Request, JSONIn),
    (   get_dict(query, JSONIn, Query)
    ->  process_user_input(Query, Response),
        reply_json_dict(_{response: Response})
    ;   reply_json_dict(_{error: "Missing or invalid query parameter."})
    ).

% Track the last topic for expounding
:- dynamic last_topic/1.

% Process user input
process_user_input(UserInput, Response) :-
    % Extract keywords
    extract_keywords(UserInput, Keywords),
    (   member("expound", Keywords)
    ->  (   last_topic(Topic),
            expanded_response(Topic, ExpandedResponse)
        ->  Response = ExpandedResponse
        ;   Response = "I don't have more details on that."
        )
    ;   generate_response(Keywords, BasicResponse),
        asserta(last_topic(BasicResponse)),  % Save last topic for potential "expound" request
        Response = BasicResponse
    ).

% Related terms for flexible matching
related_term("investment", ["invest", "investing", "investment"]).
related_term("savings", ["saving", "savings", "save", "emergency fund"]).
related_term("budgeting", ["budget", "budgeting", "expenses", "spending"]).
related_term("retirement", ["retire", "retirement", "pension", "401k", "IRA"]).
related_term("debt", ["debt", "loans", "borrow", "borrowing", "repayment"]).

% Basic responses for each topic
generate_response(Keywords, Response) :-
    (   member(Keyword, Keywords),
        related_term("investment", RelatedTerms),
        member(Keyword, RelatedTerms)
    ->  Response = "Consider diversifying your investments across stocks, bonds, and mutual funds."
    ;   member(Keyword, Keywords),
        related_term("savings", RelatedTerms),
        member(Keyword, RelatedTerms)
    ->  Response = "Set up an emergency fund and explore high-interest savings accounts."
    ;   member(Keyword, Keywords),
        related_term("budgeting", RelatedTerms),
        member(Keyword, RelatedTerms)
    ->  Response = "Using budgeting tools like Mint or YNAB can help you track expenses."
    ;   member(Keyword, Keywords),
        related_term("retirement", RelatedTerms),
        member(Keyword, RelatedTerms)
    ->  Response = "Start contributing to retirement accounts like a 401k or IRA."
    ;   member(Keyword, Keywords),
        related_term("debt", RelatedTerms),
        member(Keyword, RelatedTerms)
    ->  Response = "Avoiding bad debt can involve focusing on lower-interest loans and repaying high-interest debt first."
    ;   Response = "I don't have advice on that. Could you clarify your question?"
    ).

% Expanded responses for detailed explanations
expanded_response("investment", "Investing wisely involves spreading your funds across various assets. Stocks offer growth potential but come with higher risk, while bonds offer stability. Consider mutual funds for diversification.").
expanded_response("savings", "Building savings involves setting aside money regularly. Start with an emergency fund covering 3-6 months of expenses, then explore high-interest accounts or certificates of deposit (CDs) for better returns.").
expanded_response("budgeting", "Effective budgeting means tracking income and expenses each month. Use tools like Mint or YNAB to categorize spending, identify savings opportunities, and set realistic financial goals.").
expanded_response("retirement", "Retirement planning involves contributing to accounts like 401(k)s or IRAs early. The power of compounding interest helps your funds grow over time, making it easier to meet retirement goals.").
expanded_response("debt", "Managing debt means paying down high-interest loans first to save on interest. Debt consolidation may also help reduce rates and simplify payments, making repayment manageable.").

% Extract key terms from input text
extract_keywords(UserInput, Keywords) :-
    downcase_atom(UserInput, LowerCaseText),
    split_string(LowerCaseText, " ", "", KeywordList),
    filter_keywords(KeywordList, Keywords).

% Filter out stopwords
filter_keywords([], []).
filter_keywords([H|T], Keywords) :-
    (   is_stopword(H)
    ->  filter_keywords(T, Keywords)
    ;   Keywords = [H|RestKeywords],
        filter_keywords(T, RestKeywords)
    ).

% Define basic stopwords to ignore
is_stopword(Word) :-
    member(Word, ["the", "is", "of", "a", "an", "and", "for", "in", "to", "how", "i", "do", "can", "please"]).

% Start the server on a dynamic port based on environment variable
:- initialization(start_server).
