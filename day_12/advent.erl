-module(advent).

-compile(export_all).


part_1(Filename) ->
    {MaxSize, Points} = from_file(Filename),
    Nodes = create_edges(Points, MaxSize),
    Graphs = partition_graphs(Nodes),
    Prices = lists:map(fun({Key, Graph}) -> {Key, advent:calculate_fence_price(Graph)} end, Graphs),
    lists:sum(lists:map(fun({_K, V}) -> V end, Prices)).


data(Filename) ->
    {MaxSize, Points} = from_file(Filename),
    Nodes = create_edges(Points, MaxSize),
    partition_graphs(Nodes).

part_2(Filename) ->
    {MaxSize, Points} = from_file(Filename),
    Nodes = create_edges(Points, MaxSize),
    Graphs = partition_graphs(Nodes),
    Results = lists:map(fun({Tag, Graph}) -> {Tag, calcualte_discount_print(Graph)} end, Graphs),
    Sum = lists:sum(lists:map(fun({_, Price}) -> Price end, Results)),
    {Results, Sum}.


from_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    Lines = binary:split(Content, <<"\n">>, [global]),
    MaxSize = get_max_size(Lines),
    Points = parse_to_points(Content),
    {MaxSize, Points}.


get_max_size(Lines) when is_list(Lines) ->
    {length(Lines), size(hd(Lines))}.


print_graph(Key, Graphs, Size) ->
    {Key, Graph} = lists:keyfind(Key, 1, Graphs),
    Points = maps:map(fun(_K, _V) -> Key end, Graph),
    print_map(Points, Size).


print_map(Points, Size) ->
    Map = compile_map(Points, Size),
    io:format("~s", [Map]).


compile_map(Points, {MaxX, MaxY}) ->
    lists:foldl(fun(X, Acc1) ->
                        NewLine = lists:foldl(fun(Y, Acc2) ->
                                                      Value = maps:get({X, Y}, Points, <<".">>),
                                                      <<Acc2/binary, Value/binary>>
                                              end,
                                              <<>>,
                                              lists:seq(0, MaxY - 1)),
                        <<Acc1/binary, NewLine/binary, "\n">>
                end,
                <<>>,
                lists:seq(0, MaxX - 1)).


parse_to_points(Content) when is_binary(Content) ->
    parse_to_points(binary:split(Content, <<"\n">>, [global]));
parse_to_points(Lines) when is_list(Lines) ->
    parse_to_points(Lines, 0, #{}).


parse_to_points([], _LineNum, Acc) ->
    Acc;
parse_to_points([Line | Rest], LineNum, Acc) ->
    LineVals = parse_line(Line),
    NewAcc = lists:foldl(fun({Y, Val}, AllPoints) -> maps:put({LineNum, Y}, Val, AllPoints) end, Acc, LineVals),
    parse_to_points(Rest, LineNum + 1, NewAcc).


parse_line(Line) ->
    parse_line(Line, 0, []).


parse_line(<<>>, _CharNum, Acc) ->
    Acc;
parse_line(<<C:1/binary, Rest/binary>>, CharNum, Acc) ->
    parse_line(Rest, CharNum + 1, [{CharNum, C} | Acc]).


create_edges(Points, MaxSizes) ->
    %% Convert the current map so that it will include edges
    Nodes = maps:map(
              fun(_Point, Value) ->
                      {Value, []}
              end,
              Points),
    do_create_edges(Nodes, MaxSizes).


do_create_edges(Nodes, _MaxSize) ->
    lists:foldl(fun(Point, Acc) ->
                        test_create_edges(Point, Acc)
                end,
                Nodes,
                maps:keys(Nodes)).


test_create_edges({X, Y} = Point, Acc) ->
    {Value, _Edges} = maps:get(Point, Acc),
    NeighboursToCheck = [{X + 1, Y}, {X, Y + 1}],
    ConnectTo = lists:filter(fun(N) -> can_connect(Value, N, Acc) end, NeighboursToCheck),
    lists:foldl(fun(N, IntAcc) ->
                        connect(Point, N, IntAcc)
                end,
                Acc,
                ConnectTo).


connect(PointA, PointB, Nodes) ->
    {Value, EdgesA} = maps:get(PointA, Nodes),
    {Value, EdgesB} = maps:get(PointB, Nodes),
    Nodes1 = maps:put(PointA, {Value, [PointB | EdgesA]}, Nodes),
    Nodes2 = maps:put(PointB, {Value, [PointA | EdgesB]}, Nodes1),
    Nodes2.


can_connect(Value, Point, Nodes) ->
    case maps:get(Point, Nodes, undefined) of
        {Value, _} ->
            true;
        _Otherwise ->
            false
    end.


connect_nodes(LPoint, {LValue, LEdges1}, RPoint, PointMap) ->
    case maps:get(RPoint, PointMap, undefined) of
        undefined ->
            PointMap;
        {LValue, REdges1} ->  %% same value so can connect
            PointMap1 = maps:put(LPoint, {LValue, [RPoint | LEdges1]}, PointMap),
            maps:put(RPoint, {LValue, [LPoint | REdges1]}, PointMap1);
        {_RValue, _REdges} ->
            PointMap
    end.


partition_graphs(Nodes) ->
    partition_graphs(Nodes, []).


partition_graphs(Nodes, Graphs) ->
    case get_first_node(Nodes) of
        empty ->
            Graphs;
        FirstNode ->
            {NewNodes, NewGraph} = get_graph_from_node(FirstNode, Nodes),
            partition_graphs(NewNodes, [NewGraph | Graphs])
    end.


get_first_node(Nodes) when is_map(Nodes) ->
    case get_first_node(maps:to_list(Nodes)) of
        empty ->
            empty;
        FirstNode ->
            FirstNode
    end;
get_first_node([]) ->
    empty;
get_first_node(Nodes) when is_list(Nodes) ->
    hd(Nodes).


get_graph_from_node({Point, {Value, _Edges}}, Nodes) ->
    {RemainingNodes, Graph} = get_graph(Point, Nodes),
    {RemainingNodes, {Value, Graph}}.


get_graph(Point, Nodes) ->
    get_graph(Point, Nodes, #{}).


get_graph(Point, Nodes, Graph) ->
    case maps:is_key(Point, Nodes) of
        false ->
            {Nodes, Graph};
        true ->
            {_Value, Edges} = maps:get(Point, Nodes),
            NewNodes = maps:remove(Point, Nodes),
            NewGraph = maps:put(Point, Edges, Graph),
            lists:foldl(
              fun(NextPoint, {NodeAcc, GraphAcc}) ->
                      get_graph(NextPoint, NodeAcc, GraphAcc)
              end,
              {NewNodes, NewGraph},
              Edges)
    end.


calculate_fence_price(Graph) ->
    Area = maps:size(Graph),
    Perimeter = maps:fold(fun(_Key, Value, Acc) -> (4 - length(Value)) + Acc end, 0, Graph),
    Area * Perimeter.


calcualte_discount_print(Graph) ->
    Area = maps:size(Graph),
    Sides = count_sides(Graph),
    Area * Sides.


count_sides(Graph) ->
    case maps:size(Graph) of
        0 ->
            0;
        1 ->
            4;
        2 ->
            4;
        _Otherwise ->
            get_first_perimeter_node(Graph)
    end.


get_first_perimeter_node(Graph) ->
    Sorted = sort_graph_by_edges(Graph),
    Start = hd(Sorted),
    walk_perimeter(Start, Graph).


sort_graph_by_edges(Graph) ->
    lists:sort(fun({_, A}, {_, B}) -> length(A) < length(B) end, maps:to_list(Graph)).


walk_perimeter({Point, [Edge]}, Graph) ->
    %% we have 1 edge. That means we are in a "spur" - therefore there are at least 2 edges we can account for
    {Px, Py} = Point,
    {Ex, Ey} = Edge,

    Direction = {Ex - Px, Ey - Py},
    walk(Point, Edge, Direction, Graph, 2);
walk_perimeter({Point, [Edge | _]}, Graph) ->
    {Px, Py} = Point,
    {Ex, Ey} = Edge,

    InitalDirection = {Ex - Px, Ey - Py},
    Direction = case can_turn_left(Point, InitalDirection, Graph) of
                    {true, D} ->
                        D;
                    false ->
                        InitalDirection
                end,
    {Dx, Dy} = Direction,
    Next = {Px + Dx, Py + Dy},
    walk(Point, Next, {Dx, Dy}, Graph, 1).


walk(StartPoint, StartPoint, _Direction, _Graph, Sides) ->
    Sides;
walk(StartPoint, CurrentPoint, Direction, Graph, Sides) ->
    case must_turn(CurrentPoint, Direction, Graph) of
        {true, NewDirection, NumberOfTurns} ->
            NextPoint = get_next_point(CurrentPoint, NewDirection),
            walk(StartPoint, NextPoint, NewDirection, Graph, Sides + NumberOfTurns);
        false ->
            NextPoint = get_next_point(CurrentPoint, Direction),
            walk(StartPoint, NextPoint, Direction, Graph, Sides)
    end.


must_turn(Point, Direction, Graph) ->
    case can_turn_left(Point, Direction, Graph) of
        {true, NewDirection} ->
            {true, NewDirection, 1};
        false ->
            case can_go_forward(Point, Direction, Graph) of
                true ->
                    false;
                false ->
                    case can_turn_right(Point, Direction, Graph) of
                        {true, NewDirection} ->
                            {true, NewDirection, 1};
                        false ->
                            {true, reverse(Direction), 2}
                    end
            end
    end.


can_turn_left({Px, Py} = _Point, Direction, Graph) ->
    LeftDirection = turn_left(Direction),
    {Lx, Ly} = LeftDirection,
    case maps:is_key({Px + Lx, Py + Ly}, Graph) of
        true ->
            {true, LeftDirection};
        false ->
            false
    end.


turn_left({1, 0}) ->
    {0, 1};
turn_left({0, 1}) ->
    {-1, 0};
turn_left({-1, 0}) ->
    {0, -1};
turn_left({0, -1}) ->
    {1, 0}.


can_turn_right({Px, Py} = _Point, Direction, Graph) ->
    RightDirection = turn_right(Direction),
    {Rx, Ry} = RightDirection,
    case maps:is_key({Px + Rx, Py + Ry}, Graph) of
        true ->
            {true, RightDirection};
        false ->
            false
    end.


turn_right({1, 0}) ->
    {0, -1};
turn_right({0, 1}) ->
    {1, 0};
turn_right({-1, 0}) ->
    {0, 1};
turn_right({0, -1}) ->
    {-1, 0}.


can_go_forward({Px, Py}, {Dx, Dy}, Graph) ->
    maps:is_key({Px + Dx, Py + Dy}, Graph).


reverse({X, Y}) ->
    {-X, -Y}.


get_next_point({Px, Py} = _Point, {X, Y} = _Direction) ->
    {Px + X, Py + Y}.
