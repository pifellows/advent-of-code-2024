-module(advent).

-compile(export_all).


part_1(Filename, Num, Size) ->
    AllPoints = points_from_file(Filename),
    Points = take(Num, AllPoints),
    get_route_out(Points, Size).


get_route_out(Points, Size) ->
    Graph = points_to_graph(Points, Size),
    Visited = walk({0, 0}, Size, Graph),
    maps:get(Size, Visited, undefined).


part_2(Filename, Size) ->
    AllPoints = points_from_file(Filename),
    Tail = lists:nthtail(12, AllPoints),
    First = take(12, AllPoints),
    AllPoints = First ++ Tail,
    get_path_blocked_point(First, Tail, Size).


get_path_blocked_point(Points, NextPoints, Size) ->
    {_Score, Path} = get_route_out(Points, Size),
    FlattenedPath = lists:flatten(Path),
    PathSet = sets:from_list(FlattenedPath),
    get_path_blocked_point(Points, NextPoints, PathSet, Size).


get_path_blocked_point(Points, [H | Tail], Path, Size) ->
    NewPoints = [H | Points],
    
    case sets:is_element(H, Path) of
        true ->
            Graph = points_to_graph(NewPoints, Size),
            Visited = walk({0, 0}, Size, Graph),
            case maps:get(Size, Visited, undefined) of
                undefined ->
                    H;
                {_Score, NewPath} ->
                    NewPathSet = sets:from_list(lists:flatten(NewPath)),
                    get_path_blocked_point(NewPoints, Tail, NewPathSet, Size)
            end;
        false ->
            get_path_blocked_point(NewPoints, Tail, Path, Size)
    end.


points_from_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    Lines = binary:split(Content, <<"\n">>, [global]),
    lists:map(fun parse_line_to_point/1, Lines).


parse_line_to_point(Line) ->
    [X, Y] = binary:split(Line, <<",">>),
    {binary_to_integer(X), binary_to_integer(Y)}.


print_points(Points, Size) when is_list(Points) ->
    print_points(Points, Size);
print_points(Points, {MaxX, MaxY} = _Size) ->
    BoardPoints = [ {X, Y} || X <- lists:seq(0, MaxX), Y <- lists:seq(0, MaxY) ],
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
    lists:reverse(Acc);
take(_N, [], Acc) ->
    lists:reverse(Acc);
take(N, [H | T], Acc) ->
    take(N - 1, T, [H | Acc]).


points_to_graph(Points, Size) when is_list(Points) ->
    points_to_graph(sets:from_list(Points), Size);
points_to_graph(Points, {MaxX, MaxY} = _Size) ->
    Board = sets:from_list([ {X, Y} || X <- lists:seq(0, MaxX), Y <- lists:seq(0, MaxY) ]),
    Nodes = sets:subtract(Board, Points),
    ConnectedNodes = sets:map(fun(N) ->
                                      connect_nodes(N, Nodes)
                              end,
                              Nodes),
    maps:from_list(sets:to_list(ConnectedNodes)).


connect_nodes({X, Y} = Node, Nodes) ->
    Neighbours = [{X - 1, Y}, {X + 1, Y}, {X, Y + 1}, {X, Y - 1}],
    Connections = lists:filter(fun(N) -> sets:is_element(N, Nodes) end, Neighbours),
    {Node, Connections}.


walk(Start, End, Nodes) ->
    walk(Start, End, Nodes, 0, [], #{Start => 0}).


walk(End, End, _Nodes, _CurrentScore, _CurrentPath, Visited) ->
    Visited;
walk(Position, End, Nodes, CurrentScore, CurrentPath, Visited) ->
    Neighbours = maps:get(Position, Nodes),
    lists:foldl(fun(N, Accum) ->
                        case should_move_to_node(N, CurrentScore + 1, [N | CurrentPath], Accum) of
                            {false, Accum1} ->
                                Accum1;
                            {true, Accum1} ->
                                walk(N, End, Nodes, CurrentScore + 1, [N | CurrentPath], Accum1)
                        end
                end,
                Visited,
                Neighbours).


should_move_to_node(Position, CurrentScore, CurrentPath, Scores) ->
    case maps:get(Position, Scores, undefined) of
        undefined ->
            {true, maps:put(Position, {CurrentScore, [CurrentPath]}, Scores)};
        {OldScore, _} when CurrentScore < OldScore ->
            {true, maps:put(Position, {CurrentScore, [CurrentPath]}, Scores)};
        {OldScore, OtherPaths} when CurrentScore == OldScore ->
            {false, maps:put(Position, {CurrentScore, [CurrentPath | OtherPaths]}, Scores)};
        _Otherwise ->
            {false, Scores}
    end.
