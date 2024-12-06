-module(advent).

-compile(export_all).


part_1(Filename) ->
    {Map, Objects, [Guard]} = parse_file(Filename),
    PathedMap = walk(Guard, Objects, Map),
    count_spaces(PathedMap).


part_2(Filename) ->
    {Map, Objects, [Guard]} = parse_file(Filename),
    PathedMap = walk(Guard, Objects, Map),
    PlacementPoints = get_possible_placement_positions(PathedMap, Guard),
    MaxY = length(PathedMap),
    MaxX = size(hd(PathedMap)),
    Results = lists:map(fun(NewPoint) -> 
        NewObjects = [NewPoint | Objects],
        follow(Guard, NewObjects, MaxX, MaxY)
    end, PlacementPoints),
    Loops = lists:filter(fun(Result) -> Result == loop end, Results),
    length(Loops).

parse_file(Filename) ->
    {ok, Contents} = file:read_file(Filename),
    Rows = binary:split(Contents, <<"\n">>, [global]),
    RowInfo = lists:map(fun(Row) -> parse_info(Row) end, Rows),
    {Objects, Guards} = combine_info(RowInfo),
    {Rows, Objects, Guards}.


parse_info(Row) ->
    parse_info(Row, {0, [], []}).


parse_info(<<>>, {_CurrentColumn, Objects, Guards}) ->
    {Objects, Guards};
parse_info(<<"#", Rest/binary>>, {CurrentColumn, Objects, Guards}) ->
    parse_info(Rest, {CurrentColumn + 1, [CurrentColumn | Objects], Guards});
parse_info(<<C, Rest/binary>>, {CurrentColumn, Objects, Guards}) ->
    case guard_info(C, CurrentColumn) of
        not_a_guard ->
            parse_info(Rest, {CurrentColumn + 1, Objects, Guards});
        Info ->
            parse_info(Rest, {CurrentColumn + 1, Objects, [Info | Guards]})
    end.


guard_info($^, Column) ->
    {Column, up};
guard_info($>, Column) ->
    {Column, right};
guard_info($<, Column) ->
    {Column, left};
guard_info($v, Column) ->
    {Column, down};
guard_info(_C, _Column) ->
    not_a_guard.


combine_info(RowInfo) ->
    combine_info(RowInfo, {0, {[], []}}).


combine_info([], {_TotalRows, {Objects, Guards}}) ->
    {lists:reverse(lists:flatten(Objects)), lists:reverse(lists:flatten(Guards))};
combine_info([{Objects, Guards} | Rest], {CurrentRow, {AllObjects, AllGuards}}) ->
    ObjectLocations = lists:map(fun(X) -> {X, CurrentRow} end, Objects),
    GuardLocations = lists:map(fun({Column, Direction}) -> {Column, CurrentRow, Direction} end, Guards),
    combine_info(Rest, {CurrentRow + 1, {[ObjectLocations | AllObjects], [GuardLocations | AllGuards]}}).


walk(Guard, Objects, Map) ->
    RowBoundary = length(Map),
    ColumnBoundry = size(hd(Map)),
    walk(Guard, Objects, Map, {RowBoundary, ColumnBoundry}).


walk({GColumn, GRow, _Direction}, _Objects, Map, {RowBoundard, ColumnBoundary}) when GColumn == ColumnBoundary orelse GRow == RowBoundard orelse GColumn < 0 orelse GRow < 0 ->
    %% Out of bounds, so done
    Map;
walk(Guard, Objects, Map, Boundaries) ->
    {Infront, _L} = get_tile_infront(Guard, Objects),
    {NewGuard, NewMap} = step(Infront, Guard, Map),
    walk(NewGuard, Objects, NewMap, Boundaries).


get_tile_infront({X, Y, up}, Objects) ->
    check_tile({X, Y - 1}, Objects);
get_tile_infront({X, Y, right}, Objects) ->
    check_tile({X + 1, Y}, Objects);
get_tile_infront({X, Y, down}, Objects) ->
    check_tile({X, Y + 1}, Objects);
get_tile_infront({X, Y, left}, Objects) ->
    check_tile({X - 1, Y}, Objects).


check_tile(Location, Objects) ->
    case lists:member(Location, Objects) of
        true ->
            {object, Location};
        false ->
            {empty, Location}
    end.


step(object, Guard, Map) ->
    {rotate(Guard), Map};
step(empty, Guard, Map) ->
    {move(Guard), mark(Guard, Map)}.

follow_step(object, Guard) ->
    rotate(Guard);
follow_step(empty, Guard) ->
    move(Guard).


rotate({X, Y, up}) ->
    {X, Y, right};
rotate({X, Y, right}) ->
    {X, Y, down};
rotate({X, Y, down}) ->
    {X, Y, left};
rotate({X, Y, left}) ->
    {X, Y, up}.


move({X, Y, up}) ->
    {X, Y - 1, up};
move({X, Y, right}) ->
    {X + 1, Y, right};
move({X, Y, down}) ->
    {X, Y + 1, down};
move({X, Y, left}) ->
    {X - 1, Y, left}.


mark({X, Y, _Direction}, Map) ->
    mark(X, Y, Map, {0, []}).


mark(_X, _Y, [], {_CurrentRow, Board}) ->
    lists:reverse(Board);
mark(X, Y, [H | Rest], {Y, Board}) ->
    %% Same Row, so Replace point
    NewLine = mark_line(X, H),
    mark(X, Y, Rest, {Y + 1, [NewLine | Board]});
mark(X, Y, [H | Rest], {CurrentRow, Board}) ->
    mark(X, Y, Rest, {CurrentRow + 1, [H | Board]}).


mark_line(X, Line) ->
    mark_line(X, Line, 0, <<>>).


mark_line(_X, <<>>, _CurrentColumn, NewLine) ->
    NewLine;
mark_line(X, <<_C, Rest/binary>>, X, NewLine) ->
    mark_line(X, Rest, X + 1, <<NewLine/binary, "X">>);
mark_line(X, <<C, Rest/binary>>, CurrentColumn, NewLine) ->
    mark_line(X, Rest, CurrentColumn + 1, <<NewLine/binary, C>>).


count_spaces(Map) ->
    lists:foldl(fun(Line, Acc) -> count_spaces_line(Line) + Acc end, 0, Map).


count_spaces_line(Line) ->
    count_spaces_line(Line, 0).


count_spaces_line(<<>>, Acc) ->
    Acc;
count_spaces_line(<<"X", Rest/binary>>, Acc) ->
    count_spaces_line(Rest, Acc + 1);
count_spaces_line(<<_C, Rest/binary>>, Acc) ->
    count_spaces_line(Rest, Acc).

get_possible_placement_positions(PathedMap, {X, Y, _Direction} = _GuardStartPosition) ->
    PlacementPositions = do_get_possible_placement_positions(PathedMap, {0, []}),
    PlacementPositions -- [{X,Y}]. %% Exclude Start Position

do_get_possible_placement_positions([], {_RowNum, PlacementPositions}) ->
    PlacementPositions;
do_get_possible_placement_positions([Row | Rest], {RowNum, PlacementPositions}) ->
    RowPositions = lists:map(fun(X) -> {X, RowNum} end, get_row_positions(Row)),
    do_get_possible_placement_positions(Rest, {RowNum + 1, PlacementPositions ++ RowPositions}).

get_row_positions(Row) ->
    get_row_positions(Row, {0, []}).

get_row_positions(<<>>, {_ColumnNum, Acc}) ->
    Acc;
get_row_positions(<<"X", Rest/binary>>, {ColumnNum, Acc}) ->
    get_row_positions(Rest, {ColumnNum + 1, [ColumnNum | Acc]});
get_row_positions(<<_C, Rest/binary>>, {ColumnNum, Acc}) ->
    get_row_positions(Rest, {ColumnNum + 1, Acc}).

follow(Start, Objects, MaxX, MaxY) ->
    follow(Start, Objects, MaxX, MaxY, #{}).

follow({X, Y, _Direction}, _Objects, MaxX, MaxY, _PathPoints) when
    X < 0 orelse Y < 0 orelse MaxX < X orelse MaxY < Y ->
        left_board;
follow(Pos, Objects, MaxX, MaxY, PathPoints) ->
    %% Follow the Path until we leave the board or encounter 
    %% an existing point from the same direction
    {Infront, NextTile} = get_tile_infront(Pos, Objects),
    case have_visited_before(Infront, NextTile, Pos, PathPoints) of
        true ->
            loop;
        false ->
            NewPathPoints = update_path_points(Infront, NextTile, Pos, PathPoints),
            NewPos = follow_step(Infront, Pos),
            follow(NewPos, Objects, MaxX, MaxY, NewPathPoints)
        end.

have_visited_before(empty, _NextTile, _Pos, _PathPoints) ->
    false;
have_visited_before(object, NextTile, {_X, _Y, Dir} = _Pos, PathPoints) ->
    maps:is_key({NextTile, Dir}, PathPoints).

update_path_points(empty, _NextTile, _Pos, PathPoints) ->
    PathPoints;
update_path_points(object, NextTile, {_X, _Y, Dir} = _Pos, PathPoints) ->
    maps:put({NextTile, Dir}, true, PathPoints).
