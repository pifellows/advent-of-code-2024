-module(advent).

-compile(export_all).

part_1(Filename) ->
    calculate_length_new_line(Filename, 25).

part_2(Filename) ->
    Numbers = parse_file(Filename),
    NumbersCounter = to_counters(Numbers),
    Results = step_counters(NumbersCounter, 75),
    collect_results(Results).

collect_results(Results) ->
    maps:fold(fun(_, Count, Acc) -> Acc + Count end, 0, Results).


to_counters(Numbers) ->
    to_counters(Numbers, #{}).


to_counters([], Counters) ->
    Counters;
to_counters([Number | Rest], Counters) ->
    Count = maps:get(Number, Counters, 0),
    NewCounters = maps:put(Number, Count + 1, Counters),
    to_counters(Rest, NewCounters).


step_counters(Stones, 0) ->
    Stones;
step_counters(Stones, Steps) ->
    NewStones = maps:fold(
        fun(Value, Count, Acc) -> 
            NewValues = step(Value),
            add_to_counter(NewValues, Count, Acc)
        end, #{}, Stones),
    step_counters(NewStones, Steps - 1).


add_to_counter(Values, Count, Acc) when is_list(Values) ->
    lists:foldl(fun(Value, Acc1) -> add_to_counter(Value, Count, Acc1) end, Acc, Values);
add_to_counter(Value, Count, Acc) ->
    CurrentCount = maps:get(Value, Acc, 0),
    maps:put(Value, CurrentCount + Count, Acc).

%% Part 1 is 25 Steps, Part 2 is 75
%% 75 Steps is _really_ slow, so there must be some sort of trick
calculate_length_new_line(Filename, Steps) ->
    Numbers = parse_file(Filename),
    NewNumberCollection = steps(Numbers, Steps),
    Flattened = lists:flatten(NewNumberCollection),
    length(Flattened).


parse_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    parse_numbers(Content).


parse_numbers(Line) ->
    binary:split(Line, <<" ">>, [global]).


steps(Numbers, 0) ->
    Numbers;
steps(Numbers, Steps) when Steps > 0 ->
    NewNumbers = step(Numbers),
    steps(NewNumbers, Steps - 1).


step(Numbers) when is_list(Numbers) ->
    lists:map(fun(Number) -> step(Number) end, Numbers);
step(Number) when is_binary(Number) ->
    apply_rules(Number).


apply_rules(<<"0">>) ->
    <<"1">>;
apply_rules(Number) ->
    case size(Number) rem 2 == 0 of
        true ->
            %% split
            Take = trunc(size(Number) / 2),
            <<L:Take/binary, R:Take/binary>> = Number,
            [reformat(L), reformat(R)];
        false ->
            N = binary_to_integer(Number),
            integer_to_binary(N * 2024)
    end.


%% Conversts a binary to an integer and back again
%% this removes leading zeros
reformat(Number) when is_binary(Number) ->
    integer_to_binary(
      binary_to_integer(Number)).


%% Parallel Map Function
pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
    pmap_gather(Pids).


pmap_gather([H | T]) ->
    receive
        {H, Ret} -> [Ret | pmap_gather(T)]
    end;
pmap_gather([]) ->
    [].


pmap_f(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.
