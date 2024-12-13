-module(advent).

-compile(export_all).

part_1(Filename) ->
    Inputs = parse_file(Filename),
    Solutions = process(Inputs),
    Costs = lists:map(fun(S) -> calculate_cost(S) end, Solutions),
    {Costs, lists:sum(Costs)}.

parse_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    Sections = binary:split(Content, <<"\n\n">>, [global]),
    lists:map(fun(Section) -> parse_data(Section) end, Sections).
    

parse_data(Binary) ->
    [LineA, LineB, LineP] = binary:split(Binary, <<"\n">>, [global]),
    [X1, Y1] = get_xy(LineA),
    [X2, Y2] = get_xy(LineB),
    [R1, R2] = get_results(LineP),
    [X1, X2, R1, Y1, Y2, R2].


get_xy(Line) ->
    NewLine = binary:replace(Line, <<",">>, <<>>, [global]),
    [_, _, XPart, YPart] = binary:split(NewLine, <<" ">>, [global]),
    [_, X] = binary:split(XPart, <<"+">>),
    [_, Y] = binary:split(YPart, <<"+">>),
    [binary_to_integer(X), binary_to_integer(Y)].


get_results(Line) ->
    NewLine = binary:replace(Line, <<",">>, <<>>, [global]),
    [_, XPart, YPart] = binary:split(NewLine, <<" ">>, [global]),
    [_, X] = binary:split(XPart, <<"=">>),
    [_, Y] = binary:split(YPart, <<"=">>),
    [binary_to_integer(X), binary_to_integer(Y)].

process(Equations) ->
    lists:map(fun(Equation) -> erlang:apply(advent, solve, Equation) end, Equations).


solve(X1, Y1, R1, X2, Y2, R2) ->
    Scale = X2/X1,
    0 = round(X2 - Scale * X1),
    Y3 = Y2 - Scale * Y1,
    R3 = R2 - Scale * R1,
    Y = round(R3/Y3),
    X = round((R1 - (Y1 * Y))/X1),
    case R1 == X1 * X + Y1 * Y andalso R2 == X2 * X + Y2 * Y of
        true ->
            {X, Y};
        false ->
            {999, 999}
        end.

calculate_cost({A, B}) when 100 < A orelse 100 < B ->
    0;
calculate_cost({A, B}) ->
    (A * 3) + B.