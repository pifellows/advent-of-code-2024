-module(advent).

-compile(export_all).


part_1(Filename) ->
    {ok, Board} = file:read_file(Filename),
    BoardWidth = get_board_width(Board),
    count_all_xmas(Board).


get_board_width(Board) ->
    [H | _T] = binary:split(Board, <<"\n">>),
    size(H).


count_all_xmas(Board) ->
    count_all_xmas(Board, Board, {0, 0}, 0).


count_all_xmas(<<>>, _Board, _ColumnRow, NuFound) ->
    NuFound;
count_all_xmas(<<"\n", Rest/binary>>, Board, {_Column, Row}, NuFound) ->
    count_all_xmas(Rest, Board, {0, Row + 1}, NuFound);
count_all_xmas(<<"XMAS", Rest/binary>>, Board, {Column, Row}, NuFound) ->
    count_all_xmas(Rest, Board, {Column + 4, Row}, NuFound + 1);
count_all_xmas(<<"X", Rest/binary>>, Board, {Column, Row}, NuFound) ->
    Up = count_up(Board, {Column, Row}, "XMAS"),
    count_all_xmas(Rest, Board, {Column + 1, Row}, NuFound + Up);
count_all_xmas(<<"SAMX", Rest/binary>>, Board, {Column, Row}, NuFound) ->
    Up = count_up(Board, {Column, Row}, "SAMX"),
    count_all_xmas(Rest, Board, {Column + 4, Row}, NuFound + 1 + Up);
count_all_xmas(<<"S", Rest/binary>>, Board, {Column, Row}, NuFound) ->
    Up = count_up(Board, {Column, Row}, "SAMX"),
    count_all_xmas(Rest, Board, {Column + 1, Row}, NuFound + Up);
count_all_xmas(<<_C, Rest/binary>>, Board, {Column, Row}, NuFound) ->
    count_all_xmas(Rest, Board, {Column + 1, Row}, NuFound).


count_up(_Board, {_Column, Row}, _Word) when Row < 3 ->
    0;
count_up(Board, ColumnRow, Word) ->
    %% As the board contains Newlines, the tile directly above our own
    %% should be Column*Row + Row
    match_up(Board, ColumnRow, Word) +
    match_up_left(Board, ColumnRow, Word) +
    match_up_right(Board, ColumnRow, Word).


match_up(Board, {Column, Row}, Word) ->
    Rows = lists:seq(Row, Row - 3, -1),
    Cols = lists:duplicate(4, Column),
    FoundWord = make_word(Board, {Cols, Rows}),
    case FoundWord == Word of
        true ->
            1;
        false ->
            0
    end.


match_up_left(_Board, {Column, _Row}, _Word) when Column < 3 ->
    0;
match_up_left(Board, {Column, Row}, Word) ->
    Rows = lists:seq(Row, Row - 3, -1),
    Columns = lists:seq(Column, Column - 3, -1),
    FoundWord = make_word(Board, {Columns, Rows}),
    case FoundWord == Word of
        true ->
            1;
        false ->
            0
    end.


match_up_right(Board, {Column, Row}, Word) ->
    Rows = lists:seq(Row, Row - 3, -1),
    Columns = lists:seq(Column, Column + 3),
    FoundWord = make_word(Board, {Columns, Rows}),
    case FoundWord == Word of
        true ->
            1;
        false ->
            0
    end.


make_word(Board, ColumnRows) ->
    make_word(Board, ColumnRows, []).


make_word(_Board, {[], []}, Acc) ->
    io:format("Found Word=~p~n", [Acc]),
    Acc;
make_word(Board, {[C | Cs], [R | Rs]}, Acc) ->
    Character = find_char(Board, C, R),
    make_word(Board, {Cs, Rs}, binary_to_list(Character) ++ Acc).

find_char(Board, C, 0) ->
    binary:part(Board, C, 1);
find_char(Board, C, R) ->
    Position = ((C * (R + 1))),
    binary:part(Board, Position, 1).
