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
    Results = lists:map(fun({Tag, Graph}) -> {Tag, calcualte_discount_print(Graph, MaxSize)} end, Graphs),
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


graph_to_points(Graph) ->
    graph_to_points(Graph, <<"X">>).


graph_to_points(Graph, Marker) when is_list(Marker) ->
    graph_to_points(Graph, list_to_binary(Marker));
graph_to_points(Graph, Marker) ->
    maps:map(fun(_Point, _Edges) -> Marker end, Graph).


points_to_binary(Points, MaxSize) ->
    compile_map(Points, MaxSize).


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


calcualte_discount_print(Graph, MaxSize) ->
    Area = maps:size(Graph),
    Sides = count_sides(Graph, MaxSize),
    Area * Sides.


count_sides(Graph, {H, W} = _MaxSize) ->
    %% Convert Graph into a list of binary lines.
    %% For each line, check the line below it
    %% if values are different, we are at an edge
    %% if value changes on next position, we complete an edge
    Points = graph_to_points(Graph),
    VerticalEdges = find_edges(Points, {H, W}),
    TransposedPoints = transpose(Points),
    HorizontalEdges = find_edges(TransposedPoints, {W, H}),
    HorizontalEdges + VerticalEdges.


transpose(Points) ->
    L = maps:to_list(Points),
    T = lists:map(fun({{X, Y}, V}) -> {{Y, X}, V} end, L),
    maps:from_list(T).


find_edges(Points, Size) ->
    find_edges(Points, Size, 0).


find_edges(_Points, {X, _Y} = _Size, Count) when X < -1 ->
    Count;
find_edges(Points, {X, Y} = _Size, Count) ->
    NewEdges = trace_line(Points, X, Y),
    find_edges(Points, {X - 1, Y}, Count + NewEdges).


trace_line(Points, LineNum, Width) ->
    trace_line(Points, LineNum, Width - 1, 0).


trace_line(_Points, _X, Y, Count) when Y < -1 ->
    Count;
trace_line(Points, X, Y, Count) ->
    CurrentPointInGraph = maps:is_key({X, Y}, Points),
    PairedPointInGraph = maps:is_key({X - 1, Y}, Points),
    PreviousPointInGraph = maps:is_key({X, Y + 1}, Points),
    PreviousPairedPointInGraph = maps:is_key({X - 1, Y + 1}, Points),
    case (CurrentPointInGraph /= PairedPointInGraph) of
        true ->
            case (PreviousPairedPointInGraph == PairedPointInGraph andalso CurrentPointInGraph == PreviousPointInGraph) of
                true ->
                    %% Paired Points are different, but Current and last Are the same
                    %% So this is an edge we have seen before
                    trace_line(Points, X, Y - 1, Count);
                false ->
                    %% Paired are different and the Current and last are different
                    %% So this is a new edge
                    trace_line(Points, X, Y - 1, Count + 1)
            end;
        false ->
            %% Paired points are the same, no edge
            trace_line(Points, X, Y - 1, Count)
    end.
