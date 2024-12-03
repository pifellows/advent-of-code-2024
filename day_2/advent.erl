-module(advent).

-compile(export_all).


part_1() ->
    analyse_reports(
      get_reports("input.txt"),
      fun is_safe/1).


part_2() ->
    analyse_reports(
      get_reports("input.txt"),
      fun dampened_is_safe/1).


get_reports(Filename) ->
    {ok, File} = file:read_file(Filename),
    Lines = binary:split(File, <<"\n">>, [global]),
    lists:map(fun(Line) ->
                      lists:map(fun binary_to_integer/1,
                                string:split(Line, " ", all))
              end,
              Lines).


analyse_reports(Reports, AnalyseFun) ->
    lists:foldl(fun(Report, Acc) ->
                        case AnalyseFun(Report) of
                            true ->
                                Acc + 1;
                            false ->
                                Acc
                        end
                end,
                0,
                Reports).


is_safe(Report) ->
    DirectionCheck = get_direction(Report),
    ConsecutiveCheck = fun distance_check/2,
    is_safe(Report, DirectionCheck, ConsecutiveCheck).


is_safe([], _DirectionCheck, _ConsecutiveCheck) ->
    true;
is_safe([_Last], _DirectionCheck, _ConsecutiveCheck) ->
    true;
is_safe([H1 | T], DirectionCheck, ConsecutiveCheck) ->
    H2 = hd(T),
    case DirectionCheck(H1, H2) andalso ConsecutiveCheck(H1, H2) of
        false ->
            false;
        true ->
            is_safe(T, DirectionCheck, ConsecutiveCheck)
    end.


dampened_is_safe(Report) when length(Report) < 3 ->
    %% A List with 0, 1 or 2 entries will always be safe
    %% 0/1 because there are no fluxuations
    %% 2 because you can just remove a bad entry and be done
    true;
dampened_is_safe(Report) ->
    [H1, H2 | T] = Report,
    %% tolerant_is_safe can catch a single issue when it doesn't
    %% occur in the first two entries of the list so if this fails
    %% we can try looking at the new list with either the first or
    %% second entry removed - this _could_ cause the direction to
    %% change but we can use the standard is_safe function as we
    %% have already removed a potential problem entry
    tolerant_is_safe(Report) orelse
    is_safe([H1 | T]) orelse
    is_safe([H2 | T]).


tolerant_is_safe(Report) ->
    DirectionCheck = get_direction(Report),
    ConsecutiveCheck = fun distance_check/2,
    tolerant_is_safe(Report, DirectionCheck, ConsecutiveCheck, 0).


tolerant_is_safe([], _DirectionCheck, _ConsecutiveCheck, _Dampened) ->
    true;
tolerant_is_safe([_Last], _DirectionCheck, _ConsecutiveCheck, _Dampened) ->
    true;
tolerant_is_safe([H1 | T], DirectionCheck, ConsecutiveCheck, Dampened) ->
    H2 = hd(T),
    case DirectionCheck(H1, H2) andalso ConsecutiveCheck(H1, H2) of
        false ->
            case Dampened == 0 of
                true ->
                    [H2 | T2] = T,
                    tolerant_is_safe([H1 | T2], DirectionCheck, ConsecutiveCheck, 1);
                false ->
                    false
            end;
        true ->
            tolerant_is_safe(T, DirectionCheck, ConsecutiveCheck, Dampened)
    end.


get_direction([First, Second | _T]) ->
    case First - Second of
        Result when Result < 0 ->
            fun is_ascending/2;
        Result when Result > 0 ->
            fun is_decending/2;
        _Otherwise ->
            fun flatlined/2
    end.


is_ascending(L, R) ->
    L < R.


is_decending(L, R) ->
    R < L.


flatlined(_L, _R) ->
    false.


distance_check(L, R) ->
    D = abs(L - R),
    0 < D andalso D < 4.
