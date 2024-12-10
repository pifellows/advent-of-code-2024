-module(advent).

-compile(export_all).


part_1(Filename) ->
    {ok, Content} = file:read_file(Filename),
    {Map, Trailheads, _MaxSize} = parse_map(Content),
    PeaksFromTrailheads = find_trail_peak_paths(Map, Trailheads),
    UniquePeaksByTrailhead = lists:map(
                               fun(PeakEnds) ->
                                       UniquePeaks = sets:from_list(PeakEnds),
                                       sets:size(UniquePeaks)
                               end,
                               PeaksFromTrailheads),
    {UniquePeaksByTrailhead, lists:sum(UniquePeaksByTrailhead)}.


parse_map(Content) ->
    {Lines, MaxSize} = get_lines(Content),
    {Map, Trailheads} = generate_map(Lines),
    {Map, Trailheads, MaxSize}.


get_lines(Content) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    MaxSize = {length(Lines), size(hd(Lines))},
    {Lines, MaxSize}.


generate_map(Lines) ->
    generate_map(Lines, 0, #{}, sets:new()).


generate_map([], _LineNumber, Map, Trailheads) ->
    {Map, Trailheads};
generate_map([CurrentLine | Lines], LineNumber, Map, AllTrailheads) ->
    {LineList, Trailheads} = parse_line(CurrentLine),
    TrailheadsSet = sets:from_list(lists:map(fun(Y) -> {LineNumber, Y} end, Trailheads)),
    NewTrailheads = sets:union(AllTrailheads, TrailheadsSet),
    NewMap = lists:foldl(
               fun({Y, V}, Acc) ->
                       maps:put({LineNumber, Y}, list_to_integer([V]), Acc)
               end,
               Map,
               LineList),
    generate_map(Lines, LineNumber + 1, NewMap, NewTrailheads).


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
              find_peaks(Trailhead, Map)
      end,
      Trailheads).


find_peaks(Trailhead, Map) ->
    Peaks = do_find_peaks(Trailhead, Map),
    sets:to_list(sets:from_list(lists:flatten(Peaks))).


do_find_peaks(Point, Map) ->
    do_find_peaks(Point, 0, Map, []).

do_find_peaks(Point, Step, Map, Acc) ->
    case find_next_steps(Point, Step, Map) of
        {peak, Peak} ->
            [Peak | Acc];
        {trail, []} ->
            Acc;
        {trail, NextSteps} ->
            lists:map(
              fun(NextStep) ->
                      do_find_peaks(NextStep, Step + 1, Map, Acc)
              end,
              NextSteps)
    end.


find_next_steps(Position, 9, _Map) ->
    {peak, Position};
find_next_steps(Position, CurrentStep, Map) ->
    PotentialNextSteps = generate_potential_next_steps(Position),

    {trail, lists:filter(fun(P) -> can_move_to(P, CurrentStep + 1, Map) end, PotentialNextSteps)}.


generate_potential_next_steps({X, Y}) ->
    [{X - 1, Y},
     {X + 1, Y},
     {X, Y - 1},
     {X, Y + 1}].


can_move_to(P, DesiredStep, Map) ->
    maps:get(P, Map, undefined) == DesiredStep.
