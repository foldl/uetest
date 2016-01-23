%@doc Command parser and random generation.
-module(cmd_parser).

-export([parse/1, compose/1, set_cell_identifier/1, set_random_flag/1, parse_simple/1]).

-type template() :: {text, string()} | {choice, [string()]} | {integer, From :: integer(), To :: integer} 
        | {double, From :: float(), To :: float()}.

%@doc Parse a command string from tester manual to get a template which can be used
%     to generate a random command by calling {@link compose/1. <em>compose/1</em>}. 
-spec parse(string()) -> false | [[template()]].
parse(Cmd) ->
    Cmd10 = string:strip(Cmd),
    Template = case parse0(Cmd10, [], []) of
        [Pre, {choice, CellType}, {choice, CellIndex} | T] ->
            CIs = [list_to_integer(II) || II <- CellIndex],
            case lists:member("LTET", CellType) andalso lists:member("LTEF", CellType) of
               true ->
                   [Pre, {lte, CIs} | T];
               _ ->
                   case lists:member("LTET", CellType) of
                       true -> [Pre, {ltetdd, CIs} | T];
                       _    -> [Pre, {ltefdd, CIs} | T]
                   end
            end;
        X -> X
    end,
    expand_cmd(Template, [[]]).

expand_cmd([], Acc) -> [lists:reverse(X) || X <- Acc];
expand_cmd([{text, Text} | T] = L, Acc) ->
    case lists:member($  , Text) of
        true -> [lists:reverse(X) ++ L || X <- Acc];
        _ -> expand_cmd(T, [[{text, Text} | Y] || Y <- Acc])
    end;
expand_cmd([{choice, L} | T], Acc) ->
    expand_cmd(T, [[{text, Y} | X] || X <- Acc, Y <- L]);
expand_cmd([X | T], Acc) -> 
    expand_cmd(T, [[X | Y] || Y <- Acc]).

parse0([], Text, Acc) ->
    lists:reverse(add_text(Text, Acc));
parse0([$[ | T], Text, Acc) ->
    {_X, [$] | T10]} = lists:splitwith(fun (C) -> C /= $] end, T),
    parse0(T10, Text, Acc);
parse0([$, | T], Text, Acc) ->
    parse0(T, [], add_text(",", add_text(Text, Acc)));
parse0([$< | T], Text, Acc) ->
    case lists:splitwith(fun (C) -> C /= $> end, T) of
        {Int, [$> | T10]} when (Int == "integer") or (Int == "int") ->
            {Range, Remain} = parse_int_range(T10),
            parse0(Remain, [], [{integer, Range} | add_text(Text, Acc)]);
        {"double", [$> | T10]} ->
            {Range, Remain} = parse_double_range(T10),
            parse0(Remain, [], [{double, Range} | add_text(Text, Acc)]);
        {Ls, [$> | T10]} ->
            case string:tokens(Ls, ",|") of
                Choices when length(Choices) > 1 -> 
                    parse0(T10, [], [{choice, Choices} | add_text(Text, Acc)]);
                _ -> 
                    parse0(T10, [], [{choice, [Ls]} | add_text(Text, Acc)])
            end
    end;
parse0([$  | T], Text, Acc) ->
    parse0(T, [], add_text(" ", add_text(Text, Acc)));
parse0([Char | T], Text, Acc) ->
    parse0(T, [Char | Text], Acc).

add_text([], Acc) -> Acc;
add_text(Text, Acc) -> 
    case string:tokens(Text, "|") of
        Choices when length(Choices) > 1 -> 
            [{choice, [lists:reverse(X) || X <- Choices]} | Acc];
        _ -> 
            case Acc of
                [{text, LastText} | T] ->
                    [{text, LastText ++ lists:reverse(Text)} | T];
                _ -> 
                    [{text, lists:reverse(Text)} | Acc]
            end
    end.

-define(max_value, 16#7ffffff).

parse_int_range([$[ | T]) -> parse_int_range(T, close);
parse_int_range([$( | T]) -> parse_int_range(T, open);
parse_int_range(T) -> {{0, ?max_value}, T}.

parse_double_range([$[ | T]) -> parse_double_range(T, close);
parse_double_range([$( | T]) -> parse_double_range(T, open);
parse_double_range(T) -> {{0.0, 1.0 * ?max_value}, T}.

parse_int_range(L, StartFlag) ->
    {Start0, Remain} = string:to_integer(L),
    Start = case StartFlag of
        close -> Start0;
        open  -> Start0 + 1
    end,
    case skip(Remain) of
        [$, | T10] ->
            T20 = skip(T10),
            case string:to_integer(T20) of
                {End, [$] | T30]} -> {{Start, End}, T30};
                {End, [$) | T30]} -> {{Start, End - 1}, T30};
                {error, _Reason} -> 
                    case T20 of
                        [$I, $n, $f, _ | T40] -> {{Start, ?max_value}, T40};
                        [$I, $N, $F, _ | T40] -> {{Start, ?max_value}, T40}
                    end
            end;
         [$., $. | T10] ->
            T20 = skip(T10),
            case string:to_integer(T20) of
                {End, [$] | T30]} -> {{Start, End}, T30};
                {End, [$) | T30]} -> {{Start, End - 1}, T30};
                {error, _Reason} -> 
                    case T20 of
                        [$I, $n, $f, _ | T40] -> {{Start, ?max_value}, T40};
                        [$I, $N, $F, _ | T40] -> {{Start, ?max_value}, T40}
                    end
            end
    end.

-define(epsilon, 0.0000001).

parse_double_range(L, StartFlag) ->
    {Start0, Remain} = to_double(L),
    Start = case StartFlag of
        close -> Start0;
        open  -> 
            case Start0 > 0 of
                true -> Start0 * (1 + ?epsilon);
                _    -> Start0 * (1 - ?epsilon)
            end
    end,
    [$, | T10] = skip(Remain),
    T20 = skip(T10),
    case to_double(T20) of
        {End, [$] | T30]} -> {{Start, End}, T30};
        {End, [$) | T30]} when End > 0 -> {{Start, End * (1 - ?epsilon)}, T30};
        {End, [$) | T30]} -> {{Start, End * (1 + ?epsilon)}, T30};
        error -> 
            case T20 of
                [$I, $n, $f, _ | T40] -> {{Start, 1.0 * ?max_value}, T40};
                [$I, $N, $F, _ | T40] -> {{Start, 1.0 * ?max_value}, T40}
            end
     end.

collect_dig([X | T], Acc) when X >= $0 andalso X =< $9 ->
    collect_dig(T, [X | Acc]);
collect_dig(X, Acc) -> {lists:reverse(Acc), X}.

str_to_double(L) ->
    case collect_dig(L, []) of
        {Int, [$. | Remain]} ->
            {Fract, Remain10} = collect_dig(Remain, []), 
            {string:to_float(Int ++ "." ++ Fract), Remain10};
        {[], Remain} ->
            {error, Remain};
        {Int, Remain} ->
            {string:to_integer(Int), Remain}
    end.

to_double(L) ->
    case str_to_double(L) of
        {{Value, []}, Remain} when is_number(Value) -> {Value, Remain};
        _ -> 
            case string:to_integer(L) of
                {Value, Remain} when is_integer(Value) -> {Value * 1.0, Remain};
                _ -> error
            end
    end.

skip([$  | T]) -> skip(T);
skip(L) -> L.

select({text, T}) -> T;
select({choice, L}) -> lists:nth(random:uniform(length(L)), L);
select({double, {From, End}}) -> 
    V = case get(random_flag) of
        min -> From;
        max -> End;
        mid -> (From + End) / 2;
        _ -> From + (End - From) * random:uniform()
    end,
    [L] = io_lib:format("~.2f",[V]),
    string:strip(string:strip(L, right, $0), right, $.);
select({integer, {From, End}}) ->
    V = case get(random_flag) of
        min -> From;
        max -> End;
        mid -> (From + End) div 2;
        _ -> round(From + (End - From) * random:uniform())
    end,
    integer_to_list(V);
select({ltetdd, Indices}) -> 
    case get(cell_identifier) of
        {tdd, L} -> 
            case lists:member(L, Indices) of
                true -> "LTET" ++ integer_to_list(L);
                _ -> undefined
            end;              
        _ -> undefined        
    end;
select({ltefdd, Indices}) -> 
    case get(cell_identifier) of
        {fdd, L} -> 
            case lists:member(L, Indices) of
                true -> "LTEF" ++ integer_to_list(L);
                _ -> undefined
            end;  
        _ -> undefined        
    end;
select({lte, Indices}) -> 
    case get(cell_identifier) of
        {tdd, L} -> 
            case lists:member(L, Indices) of
                true -> "LTET" ++ integer_to_list(L);
                _ -> undefined
            end;
        {fdd, L} ->
            case lists:member(L, Indices) of
                true -> "LTEF" ++ integer_to_list(L);
                _ -> undefined
            end;
        undefined -> undefined      
    end.

%@doc Set current cell identifier, such as {tdd, 0}
-spec set_cell_identifier(Id :: {tdd | fdd, I :: integer()}) -> any().
set_cell_identifier(Id) -> put(cell_identifier, Id).

%@doc We can use this function to temperarily disble "randomness" 
%     and get specified values.
-spec set_random_flag(min | max | mid | any) -> any().
set_random_flag(F) -> put(random_flag, F).

%@doc Compose a command from a template.
-spec compose(Template :: [template()]) -> false | {ok, string()}.
compose(Template) -> 
    R = lists:flatten([select(X) || X <- Template]),
    case lists:all(fun (X) -> is_integer(X) end, R) of
        true -> {ok, R};
        _ -> false
    end.

%@doc Parse and compose a simple command
-spec parse_simple(Cmd :: string()) -> false | {ok, string()}.
parse_simple(Cmd) when is_list(Cmd) ->
    case parse(Cmd) of
        [Template] -> compose(Template);
        _ -> false
    end.
