-module(advent).

-compile(export_all).


part_1(Filename) ->
    Lines = parse_file(Filename),
    CalibrationResults = lists:map(fun(Line) -> calibration_possible(Line) end, Lines),
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


calibration_possible(Target, [], Acc) ->
    Target == Acc;
calibration_possible(Target, [Operand | Rest], Acc) ->
    case try_multiply(Target, Rest, Acc * Operand) of
        false ->
            try_addition(Target, Rest, Acc + Operand);
        true ->
            true
    end.


try_multiply(Target, [], Acc) ->
    Target == Acc;
try_multiply(Target, _Operands, Acc) when Target < Acc ->
    false;
try_multiply(Target, Operands, Acc) ->
    calibration_possible(Target, Operands, Acc).


try_addition(Target, [], Acc) ->
    Target == Acc;
try_addition(Target, _Operands, Acc) when Target < Acc ->
    false;
try_addition(Target, Operands, Acc) ->
    calibration_possible(Target, Operands, Acc).
