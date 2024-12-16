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
    Scores = walk_graph(Start, End, ?EAST, Graph),
    lists:min(Scores).


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
    PointsWithDirections = sets:map(
                             fun(P) -> add_edge_directions(P, Points)
                             end,
                             Points),
    FilterdNonNodes = sets:filter(
                        fun({_P, E}) ->
                                case E of
                                    [?NORTH, ?SOUTH] ->
                                        false;
                                    [?EAST, ?WEST] ->
                                        false;
                                    _Otherwise ->
                                        true
                                end
                        end,
                        PointsWithDirections),
    maps:from_list(sets:to_list(FilterdNonNodes)).


add_edge_directions(P, Points) ->
    Directions = [?NORTH, ?EAST, ?SOUTH, ?WEST],
    Connections = lists:filter(fun(D) -> sets:is_element(add(P, D), Points) end, Directions),
    {P, Connections}.


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


remove_edge(Direction, {Edges, Distance} = _Node) ->
    NewEdges = lists:filter(fun(P) -> not (P == Direction) end, Edges),
    {NewEdges, Distance}.

update_start_position(Start, Graph) ->
    {Edges, _} = maps:get(Start, Graph),
    maps:put(Start, {Edges, 0}, Graph).

walk_graph(Start, End, Direction, Graph) ->
    Visited = sets:from_list([Start], [{version, 2}]),
    CurrentScore = 0,
    Walker = {Start, Direction, CurrentScore, Visited},
    walk(Walker, Graph, End, []).

walk({End, _Direction, Score, _Visited}, _Graph, End, Acc) ->
    [Score | Acc];
walk({Position, Direction, CurrentScore, Visited}, Graph, End, Acc) ->
    Edges = maps:get(Position, Graph),
    lists:foldl(fun(Edge, Accu) ->
        NextPos = get_next_node_position(Position, Edge, Graph),
        case sets:is_element(NextPos, Visited) of
            true ->
                Accu;
            false ->
                ScoreChange = 1000 * get_turns(Direction, Edge) + get_distance(Position, NextPos),
                walk({NextPos, Edge, CurrentScore + ScoreChange, sets:add_element(Position, Visited)}, Graph, End, Accu)
            end
    end, Acc, Edges).

get_next_node_position(Start, Direction, Graph) ->
    NextPos = add(Start, Direction),
    case maps:get(NextPos, Graph, error) of
        error ->
            get_next_node_position(NextPos, Direction, Graph);
        _Node ->
            NextPos
        end.

