-module(advent).

-compile(export_all).


part_1(Filename) ->
    {PointSets, MaxSize} = parse_file_to_pointsets(Filename),
    Set = get_pattern_antinodes(PointSets, MaxSize),
    sets:size(Set).


part_2(Filename) ->
    {PointSets, MaxSize} = parse_file_to_pointsets(Filename),
    PatternAntinodes = get_pattern_antinodes(PointSets, MaxSize),
    ProjectedAntinodes = get_projected_antinodes(PointSets, MaxSize),
    AllAntinodes = sets:union(PatternAntinodes, ProjectedAntinodes),
    sets:size(AllAntinodes).


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


get_pattern_antinodes(PointSets, MaxSize) ->
    AntinodePositions = maps:map(fun(_Key, Points) ->
                                         Positions = get_antinode_positions(Points),
                                         lists:filter(fun(Point) ->
                                                              is_in_bounds(Point, MaxSize)
                                                      end,
                                                      Positions)
                                 end,
                                 PointSets),
    ToAdd = lists:flatten(maps:values(AntinodePositions)),
    sets:from_list(ToAdd).


get_projected_antinodes(PointSets, MaxSize) ->
    Antinodes = maps:map(fun(_Key, Points) ->
                                get_projected_positions(Points, MaxSize)
                        end,
                        PointSets),
    ToAdd = lists:flatten(maps:values(Antinodes)),
    sets:from_list(ToAdd).


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


get_projected_positions(Points, MaxSize) when not is_list(Points) ->
    get_projected_positions(sets:to_list(Points), MaxSize);
get_projected_positions(Points, MaxSize) ->
    PointPairs = get_point_combinations(Points),
    lists:foldl(fun(PointPair, Acc) ->
                        ProjectedPoints = project_points(PointPair, MaxSize),
                        ProjectedPoints ++ Acc
                end,
                [],
                PointPairs).


project_points({{X1, Y1} = Point1, {X2, Y2} = Point2}, MaxSize) ->
    Step1 = {X2 - X1, Y2 - Y1},
    Step2 = {X1 - X2, Y1 - Y2},
    Points = project_points(Point1, Step1, MaxSize, []),
    project_points(Point2, Step2, MaxSize, Points).


project_points({X, Y} = _Start, {Dx, Dy} = Step, MaxSize, Acc) ->
    NewPos = {X + Dx, Y + Dy},
    case is_in_bounds(NewPos, MaxSize) of
        true ->
            project_points(NewPos, Step, MaxSize, [NewPos | Acc]);
        false ->
            Acc
    end.
