-module(aoc).

-export([solve_part_1/1,
         solve_part_2/1,
         read_and_pair/1,
         to_arrays/1,
         get_diffs/1,
         get_similarities/1]).


solve_part_1(Filename) ->
    PairedIntegers = read_and_pair(Filename),
    Arrays = to_arrays(PairedIntegers),
    Diffs = get_diffs(Arrays),
    lists:sum(Diffs).


solve_part_2(Filename) ->
    PairedIntegers = read_and_pair(Filename),
    Arrays = to_arrays(PairedIntegers),
    Similarities = get_similarities(Arrays),
    lists:sum(Similarities).


read_and_pair(Filename) ->
    {ok, Lines} = file:read_file(Filename),
    lists:map(fun(Line) ->
                      LineL = binary_to_list(Line),
                      [First, Second] = string:tokens(LineL, " "),
                      {list_to_integer(First), list_to_integer(Second)}
              end,
              binary:split(Lines, <<"\n">>, [global])).


to_arrays(Paired) ->
    {First, Second} = lists:foldl(fun({L, R}, {LAcc, RAcc}) ->
                                          {[L | LAcc], [R | RAcc]}
                                  end,
                                  {[], []},
                                  Paired),
    {lists:sort(First), lists:sort(Second)}.


get_diffs({Left, Right}) ->
    get_diffs(Left, Right, []).


get_diffs([], [], Acc) ->
    Acc;
get_diffs([L | Left], [R | Right], Acc) ->
    Result = abs(L - R),
    get_diffs(Left, Right, [Result | Acc]).


get_similarities({Left, Right}) ->
    lists:map(fun(N) ->
                      Count = length(lists:filter(fun(X) -> N == X end, Right)),
                      Count * N
              end,
              Left).
