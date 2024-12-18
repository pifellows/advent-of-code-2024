-module(advent).

-compile(export_all).

part_1(Filename, Num, Size) ->
    Points = points_from_file(Filename, Num),
    Graph = points_to_graph(Points, Size),
    Visited = walk({0,0}, Size, Graph),
    maps:get(Size, Visited).

points_from_file(Filename) ->
    points_from_file(Filename, 4000).

points_from_file(Filename, Num) ->
    {ok, Content} = file:read_file(Filename),
    Lines = binary:split(Content, <<"\n">>, [global]),
    ToTake = take(Num, Lines),
    sets:from_list(lists:map(fun parse_line_to_point/1, ToTake)).


parse_line_to_point(Line) ->
    [X, Y] = binary:split(Line, <<",">>),
    {binary_to_integer(X), binary_to_integer(Y)}.


print_points(Points, {MaxX, MaxY} = _Size) ->
    BoardPoints = [ {X, Y} || X <- lists:seq(0, MaxX - 1), Y <- lists:seq(0, MaxY) ],
    Print = lists:map(fun({_X, Y}) when Y == MaxY ->
                              "\n";
                         (P) ->
                              case sets:is_element(P, Points) of
                                  false ->
                                      ".";
                                  true ->
                                      "#"
                              end
                      end,
                      BoardPoints),
    io:format("~s~n", [Print]).


take(N, List) ->
    take(N, List, []).


take(0, _List, Acc) ->
    Acc;
take(_N, [], Acc) ->
    Acc;
take(N, [H | T], Acc) ->
    take(N - 1, T, [H | Acc]).


points_to_graph(Points, {MaxX, MaxY} = _Size) ->
    Board = sets:from_list([ {X, Y} || X <- lists:seq(0, MaxX), Y <- lists:seq(0, MaxY) ]),
    Nodes = sets:subtract(Board, Points),
    ConnectedNodes = sets:map(fun(N) ->
                     connect_nodes(N, Nodes)
             end,
             Nodes),
    maps:from_list(sets:to_list(ConnectedNodes)).

connect_nodes({X, Y} = Node, Nodes) ->
    Neighbours = [{X-1, Y}, {X+1, Y}, {X, Y+1}, {X, Y-1}],
    Connections = lists:filter(fun(N) -> sets:is_element(N, Nodes) end, Neighbours),
    {Node, Connections}.

walk(Start, End, Nodes) ->
    walk(Start, End, Nodes, 0, #{Start => 0}).

walk(End, End, _Nodes, _CurrentScore, Visited) ->
    Visited;
walk(Position, End, Nodes, CurrentScore, Visited) ->
    Neighbours = maps:get(Position, Nodes),
    lists:foldl(fun(N, Accum) ->
        case should_move_to_node(N, CurrentScore + 1, Accum) of
            {false, Accum1} ->
                Accum1;
            {true, Accum1} ->
                walk(N, End, Nodes, CurrentScore + 1, Accum1)
            end
end, Visited, Neighbours).

should_move_to_node(Position, CurrentScore, Scores) ->
    case maps:get(Position, Scores, undefined) of
        undefined ->
            {true, maps:put(Position, CurrentScore, Scores)};
        OldScore when CurrentScore < OldScore ->
            {true, maps:put(Position, CurrentScore, Scores)};
        _OldScore ->
            {false, Scores}
        end.