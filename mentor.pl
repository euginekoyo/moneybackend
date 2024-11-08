:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pure_input)).  % For enhanced input handling
:- use_module(library(lists)).  % For list processing

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

% Consolidate handle_chat/1 predicates
:- discontiguous handle_chat/1.

% Handle POST requests for /chat
:- http_handler(root(chat), handle_chat, [method(post)]).

handle_chat(Request) :-
    add_cors_headers,
    http_read_json_dict(Request, JSONIn),  % Use http_read_json_dict to parse as dict
    (   get_dict(query, JSONIn, Query)
    ->  (   Query == "list prompts"
        ->  list_prompts(Prompts),
            reply_json_dict(_{response: Prompts})
        ;   Query == "all advice"
        ->  all_financial_advice(AllAdvice),
            reply_json_dict(_{response: AllAdvice})
        ;   financial_advice(Query, Response),
            reply_json_dict(_{response: Response})
        )
    ;   get_dict(query, JSONIn, UserInput)
    ->  process_user_input(UserInput, Response),
        reply_json_dict(_{response: Response})
    ;   reply_json_dict(_{error: "Missing or invalid query parameter."})
    ).

% Prompt listing
list_prompts(Prompts) :-
    findall(Prompt, available_prompt(Prompt), AllPrompts),
    numbered_list(AllPrompts, 1, Prompts).

% Basic Keyword Extraction
process_user_input(UserInput, Response) :-
    extract_keywords(UserInput, Keywords),
    generate_response(Keywords, Response).

% Extract key terms (e.g., 'save', 'retirement', 'investment') from the input text
extract_keywords(UserInput, Keywords) :-
    % Normalize the input (convert to lowercase, remove punctuation, etc.)
    normalize_text(UserInput, NormalizedInput),
    % Use basic keyword matching
    findall(Keyword, keyword_match(NormalizedInput, Keyword), Keywords).

% Normalize input text (e.g., lowercase, removing punctuation)
normalize_text(Text, NormalizedText) :-
    downcase_atom(Text, LowerCaseText),
    atom_chars(LowerCaseText, Chars),
    exclude(is_punctuation, Chars, CleanChars), % Remove punctuation
    exclude(is_stopword, CleanChars, FilteredChars), % Remove stopwords like 'mama'
    atom_chars(NormalizedText, FilteredChars).

% Stopwords removal (like 'mama', 'how', etc.)
is_stopword(Char) :-
    member(Char, ['mama', 'please', 'help', 'how']), % Define more stopwords as needed
    !.
is_stopword(_) :- false.

% Check for a match with known keywords
keyword_match(Text, Keyword) :-
    member(Keyword, ["investment", "invest", "investing", "savings", "budgeting", "retirement", "debt", "stocks", "bonds"]),
    sub_atom(Text, _, _, _, Keyword).

% Generate response based on extracted keywords
generate_response(Keywords, Response) :-
    (   member("investment", Keywords)
    ->  Response = "Diversifying your investments across stocks, bonds, and mutual funds is key."
    ;   member("invest", Keywords)
    ->  Response = "Investing wisely can help you grow your wealth. Consider stocks, bonds, or mutual funds."
    ;   member("investing", Keywords)
    ->  Response = "Investing is crucial for long-term wealth accumulation. Think about diverse assets."
    ;   member("savings", Keywords)
    ->  Response = "Consider setting up an emergency fund and exploring high-interest savings accounts."
    ;   member("budgeting", Keywords)
    ->  Response = "Using budgeting tools like Mint or YNAB can help you track expenses."
    ;   member("retirement", Keywords)
    ->  Response = "Start contributing to retirement accounts like a 401k or IRA."
    ;   member("debt", Keywords)
    ->  Response = "Consider consolidating debt or negotiating lower interest rates."
    ;   Response = "I don't understand that. Could you clarify your question?"
    ).

% Available prompts
available_prompt("investment").
available_prompt("invest").
available_prompt("investing").
available_prompt("investment risk").
available_prompt("investment returns").
available_prompt("types of investments").
available_prompt("investment diversification").
available_prompt("savings").
available_prompt("investment").
available_prompt("emergency fund").
available_prompt("best savings accounts").
available_prompt("debt").
available_prompt("debt repayment").
available_prompt("debt consolidation").
available_prompt("budgeting").
available_prompt("budgeting tools").
available_prompt("retirement").
available_prompt("catching up retirement").
available_prompt("all advice").
available_prompt("list prompts").

% Create a numbered list from the prompts
numbered_list([], _, []).
numbered_list([H|T], N, [N-H|NumberedTail]) :-
    N1 is N + 1,
    numbered_list(T, N1, NumberedTail).

% Financial advice predicates (consolidated)
:- discontiguous financial_advice/2.

% General Investment Advice
financial_advice("investment", "Consider diversifying into stocks, bonds, and mutual funds.").
financial_advice("investment", Advice) :- 
    Advice = "Diversify your investments in different sectors for better risk management.".

financial_advice("investment risk", "High-risk investments include stocks, while low-risk options include bonds.").
financial_advice("investment risk", Advice) :-
    Advice = "To balance risk, consider spreading investments across both high-risk (e.g., stocks) and low-risk (e.g., bonds) assets.".

financial_advice("investment returns", "Historical returns suggest a diversified portfolio often outperforms individual investments.").
financial_advice("investment returns", Advice) :-
    Advice = "Over the long term, diversified investments tend to have more stable returns compared to concentrated investments.".

% Other financial advice clauses
financial_advice("savings", "Consider setting up an emergency fund and exploring high-interest savings accounts.").
financial_advice("savings", "Aim to save at least 3-6 months of expenses in case of emergencies.").

financial_advice("budgeting", "Using budgeting tools like Mint or YNAB can help you track expenses.").
financial_advice("budgeting", "Create a monthly budget by tracking income and expenses.").

financial_advice("retirement", "Start contributing to retirement accounts like a 401k or IRA.").
financial_advice("retirement", "Make sure to diversify your retirement savings across different assets.").

financial_advice("debt", "Consider consolidating debt or negotiating lower interest rates.").
financial_advice("debt", "Paying off high-interest debt first can help you save money in the long run.").

% Collect all financial advice for the "all advice" request
all_financial_advice(AllAdvice) :-
    findall(AdvicePair, financial_advice(_, AdvicePair), AllAdviceList),
    list_to_set(AllAdviceList, AllAdvice). % Remove duplicates if any

% Default Catch-All Response
financial_advice(_, "I don't have advice on that. Could you clarify your query?").

% Start the server on port 8000
:- initialization(start_server(8000)).
