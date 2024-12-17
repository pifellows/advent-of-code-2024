-module(advent).

-compile(export_all).

-define(NON_VALUE, {-1, -1}).
-define(NORTH,     {-1, 0}).
-define(EAST,      {0, 1}).
-define(SOUTH,     {1, 0}).
-define(WEST,      {0, -1}).


part_1(Filename) ->
    {Start, End, Points} = points_from_file(Filename),
    Graph = points_to_graph(Points),
    DirectedGraph = graph_to_directed_graph(Start, End, ?EAST, Graph),
    EndScores = get_scores_for_node(End, DirectedGraph),
    get_minimum_score(EndScores).


get_scores_for_node(Point, DirectedGraph) ->
    maps:filter(fun({P, _D}, _V) ->
         P == Point 
        end, DirectedGraph).

get_minimum_score(EndScores) ->
    AsList = maps:to_list(EndScores),
    lists:foldl(fun({_K, V}, Acc) -> min(Acc, V) end, infinity, AsList).


points_from_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    {Start, End, Points} = parse_points(Content),
    {Start, End, Points}.


parse_points(Content) ->
    {Start, End, Points} = parse_points(Content, 0, 0, {?NON_VALUE, ?NON_VALUE, sets:new([{version, 2}])}),
    {Start, End, Points}.


parse_points(<<>>, _X, _Y, {_Start, _End, _Points} = Accs) ->
    Accs;
parse_points(<<".", Rest/binary>>, X, Y, {Start, End, Points}) ->
    parse_points(Rest, X, Y + 1, {Start, End, sets:add_element({X, Y}, Points)});
parse_points(<<"\n", Rest/binary>>, X, _Y, Accs) ->
    parse_points(Rest, X + 1, 0, Accs);
parse_points(<<"S", Rest/binary>>, X, Y, {_Start, End, Points}) ->
    parse_points(Rest, X, Y + 1, {{X, Y}, End, sets:add_element({X, Y}, Points)});
parse_points(<<"E", Rest/binary>>, X, Y, {Start, _End, Points}) ->
    parse_points(Rest, X, Y + 1, {Start, {X, Y}, sets:add_element({X, Y}, Points)});
parse_points(<<_C, Rest/binary>>, X, Y, Accs) ->
    parse_points(Rest, X, Y + 1, Accs).


add({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.


inverse({X, Y}) ->
    {-X, -Y}.


points_to_graph(Points) ->
    PointsList = sets:to_list(Points),
    PointsWithEdges = lists:filtermap(fun(Point) ->
                                                    Edges = get_edge_directions(Point, Points),
                                                    case Edges of
                                                        [A, B] ->
                                                            case A /= inverse(B) of
                                                                true ->
                                                                    {true, {Point, Edges}};
                                                                false ->
                                                                    false
                                                                end;
                                                        Edges ->
                                                            {true, {Point, Edges}}
                                                    end
                                            end,
                                            PointsList),

    EdgeMap = maps:from_list(PointsWithEdges),
    maps:map(fun(Point, Edges) -> 
        NextPositions = get_next_nodes(Point, Edges, EdgeMap),
        {Edges, NextPositions}
    end, EdgeMap).

get_next_nodes(Point, Edges, Points) ->
    lists:map(fun(Edge) -> get_next_node_position(Point, Edge, Points) end, Edges).


get_edge_directions(P, Points) ->
    Directions = [?NORTH, ?EAST, ?SOUTH, ?WEST],
    Connections = lists:filter(fun(D) -> sets:is_element(add(P, D), Points) end, Directions),
    Connections.


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


get_distance({X1, Y1} = _Point1, {X2, Y2} = _Point2) ->
    abs(X1 - X2) + abs(Y1 - Y2).


get_turns(From, From) ->
    0;
get_turns({X1, Y1} = _Direction1, {X2, Y2} = _Direction2) when X1 == -X2 orelse Y1 == -Y2 ->
    2;
get_turns(_Point1, _Point2) ->
    1.


update_score({Edges, infinity}, Distance) ->
    {Edges, Distance};
update_score({Edges, Distance}, NewDistance) when NewDistance < Distance ->
    {Edges, NewDistance};
update_score(Node, _NewDistance) ->
    Node.


graph_to_directed_graph(Start, End, Direction, Graph) ->
    StartKey = {Start, Direction},
    graph_to_directed_graph(Start, End, Direction, Graph, #{StartKey => 0}).


graph_to_directed_graph(End, End, _Direction, _Graph, Visited) ->
    Visited;
graph_to_directed_graph(Position, End, Direction, Graph, Visited) ->
    CurrentScore = maps:get({Position, Direction}, Visited),
    NextPositions = maps:get(Position, Graph),
    {UpdatedVisited, NextPoints} = update_score_paths(NextPositions, CurrentScore, Position, Direction, {Visited, []}),
    lists:foldl(fun({NextPosition, NextDirection}, VisitedAcc) ->
        graph_to_directed_graph(NextPosition, End, NextDirection, Graph, VisitedAcc)
    end, UpdatedVisited, NextPoints).


update_score_paths({[], []}, _CurrentScore, _CurrentPosition, _CurrentDirection, Acc) ->
    Acc;
update_score_paths({[NextDirection | RestDirections], [NextPosition | RestPositions]}, CurrentScore, CurrentPosition, CurrentDirection, {Visited, NextPoints}) ->
    NewScore = CurrentScore + 1000 * get_turns(NextDirection, CurrentDirection) + get_distance(NextPosition, CurrentPosition),
    NextPositionCurrentScore = maps:get({NextPosition, NextDirection}, Visited, undefined),
    NewAccs = case NextPositionCurrentScore of
        undefined ->
            {maps:put({NextPosition, NextDirection}, NewScore, Visited), [{NextPosition, NextDirection} | NextPoints]};
        NextPositionCurrentScore when NewScore < NextPositionCurrentScore ->
            {maps:put({NextPosition, NextDirection}, NewScore, Visited), [{NextPosition, NextDirection} | NextPoints]};
        NextPositionCurrentScore ->
            {Visited, NextPoints}
        end,
    update_score_paths({RestDirections, RestPositions}, CurrentScore, CurrentPosition, CurrentDirection, NewAccs).


get_next_node_position(Start, Direction, Points) ->
    NextPos = add(Start, Direction),
    case sets:is_element(NextPos, Points) of
        false ->
            get_next_node_position(NextPos, Direction, Points);
        true ->
            NextPos
    end.


update_score_for_position(Position, NewScore, Graph) ->
    {Edges, Score} = maps:get(Position, Graph),
    case Score of
        infinity ->
            maps:put(Position, {Edges, NewScore}, Graph);
        Score when NewScore < Score ->
            maps:put(Position, {Edges, NewScore}, Graph);
        Score ->
            Graph
    end.
