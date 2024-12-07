-module(advent).

-compile(export_all).


part_1(Filename) ->
    Lines = parse_file(Filename),
    CalibrationResults = lists:map(fun(Line) -> calibration_possible(Line) end, Lines),
    lists:sum(CalibrationResults).


part_2(Filename) ->
    Lines = parse_file(Filename),
    CalibrationResults = lists:map(fun(Line) -> calibration_possible(Line, concat) end, Lines),
    lists:sum(CalibrationResults).


parse_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    Lines = binary:split(Content, <<"\n">>, [global]),
    lists:map(fun(Line) -> parse_line(Line) end, Lines).


parse_line(Line) ->
    [TotalBin, PartsBin] = binary:split(Line, <<":">>),
    Total = binary_to_integer(TotalBin),
    Parts = format_parts(PartsBin),
    {Total, Parts}.


format_parts(Parts) ->
    Tokens = binary:split(Parts, <<" ">>, [global, trim_all]),
    lists:map(fun(X) -> binary_to_integer(X) end, Tokens).


calibration_possible({Total, [First | Operands]}) ->
    case calibration_possible(Total, Operands, First) of
        true ->
            Total;
        false ->
            0
    end.


calibration_possible({Total, [First | Operands]}, concat) ->
    case calibration_possible_with_concat(Total, Operands, First) of
        true ->
            Total;
        false ->
            0
    end.


calibration_possible(Target, [], Acc) ->
    Target == Acc;
calibration_possible(Target, [Operand | Rest], Acc) ->
    case try_multiply(Target, Rest, Acc * Operand) of
        false ->
            try_addition(Target, Rest, Acc + Operand);
        true ->
            true
    end.


calibration_possible_with_concat(Target, [Operand | Rest], Acc) ->
    case try_multiply(Target, Rest, Acc * Operand, concat) orelse
         try_addition(Target, Rest, Acc + Operand, concat) of
        false ->
            try_concat(Target, Rest, concat(Acc, Operand));
        true ->
            true
    end.


try_multiply(Target, Operands, Acc) ->
    try_multiply(Target, Operands, Acc, no_concat).


try_multiply(_Target, [], 0, _Option) ->
    false;
try_multiply(Target, [], Acc, _Option) ->
    Target == Acc;
try_multiply(Target, _Operands, Acc, _Option) when Target < Acc ->
    false;
try_multiply(Target, Operands, Acc, no_concat) ->
    calibration_possible(Target, Operands, Acc);
try_multiply(Target, Operands, Acc, concat) ->
    calibration_possible_with_concat(Target, Operands, Acc).


try_addition(Target, Operands, Acc) ->
    try_addition(Target, Operands, Acc, no_concat).


try_addition(Target, [], Acc, _Option) ->
    Target == Acc;
try_addition(Target, _Operands, Acc, _Option) when Target < Acc ->
    false;
try_addition(Target, Operands, Acc, no_concat) ->
    calibration_possible(Target, Operands, Acc);
try_addition(Target, Operands, Acc, concat) ->
    calibration_possible_with_concat(Target, Operands, Acc).


concat(A, B) when is_integer(A) andalso is_integer(B) ->
    list_to_integer(integer_to_list(A) ++ integer_to_list(B)).


try_concat(Total, [], Acc) ->
    Total == Acc;
try_concat(Total, _Operands, Acc) when Total < Acc ->
    false;
try_concat(Target, Operands, Acc) ->
    calibration_possible_with_concat(Target, Operands, Acc).
