-module(advent).

-compile(export_all).


part_1(Filename) ->
    {ok, Board} = file:read_file(Filename),
    BoardWidth = get_board_width(Board),
    count_all_xmas(Board, BoardWidth).


get_board_width(Board) ->
    [H | _T] = binary:split(Board, <<"\n">>),
    size(H).


count_all_xmas(Board, BoardWidth) ->
    count_all_xmas(Board, {Board, BoardWidth}, {0, 0}, 0).


count_all_xmas(<<>>, _Board, _ColumnRow, NuFound) ->
    NuFound;
count_all_xmas(<<"\n", Rest/binary>>, Board, {_Column, Row}, NuFound) ->
    count_all_xmas(Rest, Board, {0, Row + 1}, NuFound);
count_all_xmas(<<"XMAS", Rest/binary>>, Board, {Column, Row}, NuFound) ->
    Up = count_up(Board, {Column, Row}, "SAMX"),
    count_all_xmas(<<"S", Rest/binary>>, Board, {Column + 3, Row}, NuFound + 1 + Up);
count_all_xmas(<<"X", Rest/binary>>, Board, {Column, Row}, NuFound) ->
    Up = count_up(Board, {Column, Row}, "SAMX"),
    count_all_xmas(Rest, Board, {Column + 1, Row}, NuFound + Up);
count_all_xmas(<<"SAMX", Rest/binary>>, Board, {Column, Row}, NuFound) ->
    Up = count_up(Board, {Column, Row}, "XMAS"),
    count_all_xmas(<<"X", Rest/binary>>, Board, {Column + 3, Row}, NuFound + 1 + Up);
count_all_xmas(<<"S", Rest/binary>>, Board, {Column, Row}, NuFound) ->
    Up = count_up(Board, {Column, Row}, "XMAS"),
    count_all_xmas(Rest, Board, {Column + 1, Row}, NuFound + Up);
count_all_xmas(<<_C, Rest/binary>>, Board, {Column, Row}, NuFound) ->
    count_all_xmas(Rest, Board, {Column + 1, Row}, NuFound).


count_up(_Board, {_Column, Row}, _Word) when Row < 3 ->
    0;
count_up(Board, ColumnRow, Word) ->
    %% As the board contains Newlines, the tile directly above our own
    %% should be Column*Row + Row
    Found = match_up(Board, ColumnRow, Word) +
        match_up_left(Board, ColumnRow, Word) +
        match_up_right(Board, ColumnRow, Word),

    %% io:format("Found ~p at ~p when looking for ~p~n", [Found, ColumnRow, Word]),
    Found.


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
    %% io:format("Found Word=~p~n", [Acc]),
    Acc;
make_word(Board, {[C | Cs], [R | Rs]}, Acc) ->
    Character = find_char(Board, C, R),
    make_word(Board, {Cs, Rs}, binary_to_list(Character) ++ Acc).


find_char({Board, BoardWidth}, C, R) ->
    Position = (BoardWidth * R) + R + C,
    binary:part(Board, Position, 1).


part_2(Filename) ->
    {ok, Board} = file:read_file(Filename),
    {Rows, RowWidth} = split_board(Board),
    count_cross_mas(Rows, RowWidth, 0).


split_board(Board) ->
    [H | T] = binary:split(Board, <<"\n">>, [global]),
    Width = size(H),
    {{[H], T}, Width}.


count_cross_mas({_L, []}, _Width, Acc) ->
    Acc;
count_cross_mas({_L, [_Last]}, _Width, Acc) ->
    Acc;
count_cross_mas({L, [Current | R]}, Width, Acc) ->
    [U | _] = L,
    [D | _] = R,
    Found = check_line(Current, U, D, Width),
    count_cross_mas({[Current | L], R}, Width, Found + Acc).


check_line(Line, L, R, Width) ->
    check_line(Line, L, R, Width, {0, 0}).


check_line(<<>>, _L, _R, _Width, {_Col, Total}) ->
    Total;
check_line(<<"A", Rest/binary>>, L, R, Width, {Col, Total}) ->
    case is_cross(L, R, Col, Width) of
        true ->
            check_line(Rest, L, R, Width, {Col + 1, Total + 1});
        false ->
            check_line(Rest, L, R, Width, {Col + 1, Total})
    end;
check_line(<<_C, Rest/binary>>, L, R, Width, {Col, Total}) ->
    check_line(Rest, L, R, Width, {Col + 1, Total}).


is_cross(_L, _R, 0 = _Col, _Width) ->
    false;
is_cross(L, R, Col, Width) ->
    %% io:format("~p~n",[Width]),
    case Col == Width - 1 of
        true ->
            false;
        false ->
    UL = binary:part(L, Col - 1, 1),
    UR = binary:part(L, Col + 1, 1),
    LL = binary:part(R, Col - 1, 1),
    LR = binary:part(R, Col + 1, 1),

    ((<<"M">> == UL andalso <<"S">> == LR) orelse
    (<<"S">> == UL andalso <<"M">> == LR)) andalso
    ((<<"M">> == UR andalso <<"S">> == LL) orelse
    (<<"S">> == UR andalso <<"M">> == LL))
    end.
