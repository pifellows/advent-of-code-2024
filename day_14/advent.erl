-module(advent).

-compile(export_all).


part_1(Type) ->
    {Agents, Size} = parse_file(Type),
    FinalPositions = simulate(Agents, 100, Size),
    Quadrants = group_by_quadrant(FinalPositions, Size),
    SafetyComponents = lists:map(fun(Q) -> length(Q) end, Quadrants),
    SafetyRating = lists:foldl(fun(C, Acc) -> Acc * C end, 1, SafetyComponents),
    SafetyRating.


part_2() ->
    {Agents, Size} = parse_file(input),
    AgentCount = length(Agents),
    Until = fun(Points) -> all_unique_positions(Points, AgentCount) end,
    step(Agents, Size, Until).


parse_file(Type) ->
    {Content, Size} = get_data(Type),
    Lines = binary:split(Content, <<"\n">>, [global]),
    Data = lists:map(fun(Line) -> parse_line(Line) end, Lines),
    {Data, Size}.


get_data(test) ->
    {ok, Content} = file:read_file("test.txt"),
    {Content, {11, 7}};
get_data(input) ->
    {ok, Content} = file:read_file("input.txt"),
    {Content, {101, 103}}.


parse_line(Line) ->
    Fields = binary:split(Line, <<" ">>),
    [Pos, Dir] = lists:map(fun(Field) -> get_values(Field) end, Fields),
    {Pos, Dir}.


get_values(Field) ->
    [_Key, Value] = binary:split(Field, <<"=">>),
    as_point(Value).


as_point(Value) ->
    [X, Y] = binary:split(Value, <<",">>),
    {binary_to_integer(X), binary_to_integer(Y)}.


simulate(Agents, Steps, Size) ->
    lists:map(
      fun(Agent) ->
              project(Agent, Steps, Size)
      end,
      Agents).


project({{X0, Y0}, {Dx, Dy}}, Steps, {MaxX, MaxY}) ->
    Vx = Dx * Steps,
    Vy = Dy * Steps,
    X1 = X0 + Vx,
    Y1 = Y0 + Vy,
    {normalize(X1, MaxX), normalize(Y1, MaxY)}.


normalize(N, MaxN) ->
    case N rem MaxN of
        R when R < 0 ->
            MaxN + R;
        R ->
            R
    end.


group_by_quadrant(Positions, {MaxX, MaxY}) ->
    MidX = trunc(MaxX / 2),
    MidY = trunc(MaxY / 2),
    {A, B, C, D} = lists:foldl(fun(Point, Acc) -> place_point(Point, MidX, MidY, Acc) end, {[], [], [], []}, Positions),
    [A, B, C, D].


place_point({X, _Y}, X, _, {A, B, C, D}) ->
    {A, B, C, D};
place_point({_X, Y}, _, Y, {A, B, C, D}) ->
    {A, B, C, D};
place_point({X, Y} = P, MidX, MidY, {A, B, C, D}) ->
    case X of
        X1 when X1 < MidX ->
            case Y of
                Y1 when Y1 < MidY ->
                    {[P | A], B, C, D};
                _Y2 ->
                    {A, [P | B], C, D}
            end;
        _X2 ->
            case Y of
                Y1 when Y1 < MidY ->
                    {A, B, [P | C], D};
                _Y2 ->
                    {A, B, C, [P | D]}
            end
    end.


all_unique_positions(Batch, AgentCount) ->
    Set = sets:from_list(proplists:get_keys(Batch), [{version, 2}]),
    sets:size(Set) == AgentCount.


draw({Points, _N}, {MaxX, MaxY}) ->
    Set = sets:from_list(Points, [{version, 2}]),
    Lines = lists:map(fun(L) -> draw_line(MaxX, L, Set) end, lists:seq(0, MaxY - 1)),
    Picture = lists:join("\n", Lines),
    io:format("~s~n", [Picture]).


draw_line(MaxX, Y, Set) ->
    Characters = lists:map(fun(N) ->
                                   case sets:is_element({N, Y}, Set) of
                                       true ->
                                           "X";
                                       false ->
                                           "."
                                   end
                           end,
                           lists:seq(0, MaxX - 1)),
    lists:join("", Characters).

step(Agents, Size, Until) ->
    step(Agents, Size, Until, 0).

step(Agents, {Mx, My} = Size, Until, Steps) ->
    NewAgents = lists:map(
        fun({{X0, Y0}, {Dx, Dy} = Direction}) -> 
            {{normalize(X0 + Dx, Mx), normalize(Y0 + Dy, My)}, Direction}
        end, Agents),
    case Until(NewAgents) of
        true ->
            {proplists:get_keys(NewAgents), Steps + 1};
        false ->
            step(NewAgents, Size, Until, Steps + 1)
        end.
