-module(advent).

-compile(export_all).


part_1(Filename) ->
    {PointSets, MaxSize} = parse_file_to_pointsets(Filename),
    AntinodePositions = maps:map(fun(_Key, Points) ->
                                         Positions = get_antinode_positions(Points),
                                         lists:filter(fun(Point) ->
                                                              is_in_bounds(Point, MaxSize)
                                                      end,
                                                      Positions)
                                 end,
                                 PointSets),
    ToAdd = lists:flatten(maps:values(AntinodePositions)),
    Set = sets:from_list(ToAdd),
    sets:size(Set).


parse_file_to_pointsets(Filename) ->
    {ok, Content} = file:read_file(Filename),
    Lines = binary:split(Content, <<"\n">>, [global]),
    get_point_sets(Lines).


get_point_sets(Lines) ->
    MaxX = size(hd(Lines)),
    MaxY = length(Lines),
    Sets = get_point_sets(Lines, {0, #{}}),
    {Sets, {MaxX, MaxY}}.


get_point_sets([], {_CurrentLineNum, PointSets}) ->
    PointSets;
get_point_sets([Line | Rest], {CurrentLineNum, PointSets}) ->
    NewPointSets = get_points_in_line(Line, CurrentLineNum, PointSets),
    get_point_sets(Rest, {CurrentLineNum + 1, NewPointSets}).


get_points_in_line(Line, CurrentLineNum, PointsSet) ->
    get_points_in_line(Line, CurrentLineNum, 0, PointsSet).


get_points_in_line(<<>>, _LineNum, _PosNum, PointsSet) ->
    PointsSet;
get_points_in_line(<<".", Rest/binary>>, LineNum, PosNum, PointsSet) ->
    get_points_in_line(Rest, LineNum, PosNum + 1, PointsSet);
get_points_in_line(<<C, Rest/binary>>, LineNum, PosNum, PointsSet) ->
    CurrentPoints = maps:get(C, PointsSet, sets:new()),
    NewPoints = sets:add_element({PosNum, LineNum}, CurrentPoints),
    UpdatedPointSets = maps:put(C, NewPoints, PointsSet),
    get_points_in_line(Rest, LineNum, PosNum + 1, UpdatedPointSets).


get_antinode_positions(Points) when not is_list(Points) ->
    get_antinode_positions(sets:to_list(Points));
get_antinode_positions(Points) when is_list(Points) ->
    PointPairs = get_point_combinations(Points),
    lists:foldl(fun({Point1, Point2}, Acc) ->
                        get_antinodes(Point1, Point2) ++ Acc
                end,
                [],
                PointPairs).


get_point_combinations(Points) ->
    get_point_combinations(Points, []).


get_point_combinations([], Acc) ->
    Acc;
get_point_combinations([CurrentPoint | OtherPoints], Acc) ->
    PointPairs = get_point_pairs(CurrentPoint, OtherPoints),
    get_point_combinations(OtherPoints, PointPairs ++ Acc).


get_point_pairs(CurrentPoint, OtherPoints) ->
    lists:map(fun(OtherPoint) -> {CurrentPoint, OtherPoint} end, OtherPoints).


get_antinodes(Point1, Point2) ->
    [get_antinode(Point1, Point2), get_antinode(Point2, Point1)].


get_antinode({X1, Y1}, {X2, Y2}) ->
    Dx = X1 - X2,
    Dy = Y1 - Y2,
    {X1 + Dx, Y1 + Dy}.


is_in_bounds({X, Y}, {MaxX, MaxY}) ->
    -1 < X andalso X < MaxX andalso
    -1 < Y andalso Y < MaxY.
