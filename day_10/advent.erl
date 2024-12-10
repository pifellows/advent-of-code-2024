-module(advent).

-compile(export_all).


part_1(Filename) ->
    {ok, Content} = file:read_file(Filename),
    {Map, Trailheads, _MaxSize} = parse_map(Content),
    UniquePeaksByTrailhead = lists:map(
                               fun(Trailhead) ->
                                       PeakEnds = find_trail_peak_paths(Trailhead, Map),
                                       UniquePeaks = sets:from_list(PeakEnds),
                                       sets:size(UniquePeaks)
                               end,
                               Trailheads),
    lists:sum(UniquePeaksByTrailhead).


parse_map(Content) ->
    {Lines, MaxSize} = get_lines(Content),
    {Map, Trailheads} = generate_map(Lines),
    {Map, Trailheads, MaxSize}.


get_lines(Content) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    MaxSize = {length(Lines), size(hd(Lines))},
    {Lines, MaxSize}.


generate_map(Lines) ->
    generate_map(Lines, #{}, sets:new()).


generate_map([], Map, Trailheads) ->
    {Map, Trailheads};
generate_map([CurrentLine | Lines], Map, AllTrailheads) ->
    {LineList, Trailheads} = parse_line(CurrentLine),
    NewTrailheads = sets:union(AllTrailheads, Trailheads),
    NewMap = lists:foldl(
               fun({Y, V}, Acc) ->
                       maps:put({CurrentLine, Y}, V, Acc)
               end,
               Map,
               LineList),
    generate_map(Lines, NewMap, NewTrailheads).


parse_line(CurrentLine) ->
    parse_line(CurrentLine, 0, [], []).


parse_line(<<>>, _Pos, LineList, Trailheads) ->
    {LineList, Trailheads};
parse_line(<<"0", Rest/binary>>, Pos, LineList, Trailheads) ->
    parse_line(Rest, Pos + 1, [{Pos, $0} | LineList], [Pos | Trailheads]);
parse_line(<<C, Rest/binary>>, Pos, LineList, Trailheads) ->
    parse_line(Rest, Pos + 1, [{Pos, C} | LineList], Trailheads).


find_trail_peak_paths(Map, TrailheadSet) ->
    Trailheads = sets:to_list(TrailheadSet),
    lists:map(
      fun(Trailhead) ->
              0
      end,
      Trailheads).
