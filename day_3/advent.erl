-module(advent).

-compile(export_all).


part1(Filename) ->
    {ok, Line} = file:read_file(Filename),
    get_value_from_section(Line).


part2(Filename) ->
    {ok, Lines} = file:read_file(Filename),
    Sections = get_do_sections(Lines),
    Values = lists:map(fun(Section) -> get_value_from_section(Section) end, Sections),
    lists:sum(Values).


get_value_from_section(Binary) ->
    Numbers = find_valid_muls(Binary),
    sum_products(Numbers).


find_valid_muls(Binary) ->
    {ok, RE} = re:compile("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"),
    {match, NumberPairList} = re:run(Binary, RE, [global, {capture, [1, 2], list}]),
    lists:map(fun([L, R]) -> {list_to_integer(L), list_to_integer(R)} end, NumberPairList).


sum_products(NumberPairList) ->
    lists:foldl(fun({L, R}, Acc) ->
                        (L * R) + Acc
                end,
                0,
                NumberPairList).


get_do_sections(Binary) ->
    {ok, DoDontMatcher} = re:compile("don't\(\)|do\(\)"),
    {match, Matches} = re:run(Binary, DoDontMatcher, [global, {capture, [0]}]),
    FirstChunk = get_first_chunk(Binary, Matches),
    Chunks = create_chunks(Binary, Matches),
    [FirstChunk | get_do_chunks(Chunks)].


get_first_chunk(Binary, []) ->
    Binary;
get_first_chunk(Binary, [[{MatchStart, _L}] | _T]) ->
    binary:part(Binary, 0, MatchStart).


create_chunks(Binary, Matches) ->
    create_chunks(Binary, Matches, []).


create_chunks(_Binary, [], Acc) ->
    Acc;
create_chunks(Binary, [[{LastStart, _LastLength}]], Acc) ->
    BinaryLength = size(Binary),
    NewChunk = binary:part(Binary, LastStart, BinaryLength - LastStart),
    create_chunks(Binary, [], [NewChunk | Acc]);
create_chunks(Binary, [[{CurrentStart, _Length}] | Remainder], Acc) ->
    [{NextStart, _NextLength}] = hd(Remainder),
    NewChunk = binary:part(Binary, CurrentStart, NextStart - CurrentStart),
    create_chunks(Binary, Remainder, [NewChunk | Acc]).


get_do_chunks(Chunks) ->
    lists:filter(fun(Chunk) when is_binary(Chunk) ->
                         case Chunk of
                             <<"do()", _Rest/binary>> ->
                                 true;
                             _OtherWise ->
                                 false
                         end
                 end,
                 Chunks).
