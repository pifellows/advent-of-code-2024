-module(advent).

-compile(export_all).

part_1(Filename) ->
    {Map, Guard, Objects} = parse_file(Filename),
    {Map, Guard, Objects}.


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
    parse_info(Rest, {CurrentColumn+1, [CurrentColumn | Objects], Guards});
parse_info(<<C, Rest/binary>>, {CurrentColumn, Objects, Guards}) ->
    case guard_info(C, CurrentColumn) of
        not_a_guard ->
            parse_info(Rest, {CurrentColumn+1, Objects, Guards});
        Info ->
            parse_info(Rest, {CurrentColumn+1, Objects, [Info|Guards]})
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
combine_info([{Objects, Guards}|Rest], {CurrentRow, {AllObjects, AllGuards}}) ->
    ObjectLocations = lists:map(fun(X) -> {X, CurrentRow} end, Objects),
    GuardLocations = lists:map(fun({Column, Direction}) -> {Column, CurrentRow, Direction} end, Guards),
    combine_info(Rest, {CurrentRow + 1, {[ObjectLocations | AllObjects], [GuardLocations | AllGuards]}}).