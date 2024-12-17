-module(advent).

-compile(export_all).

-define(ADV, 0).
-define(BXL, 1).
-define(BST, 2).
-define(JNZ, 3).
-define(BXC, 4).
-define(OUT, 5).
-define(BDV, 6).
-define(CDV, 7).


part_1(Filename) ->
    {Registers, Program} = parse_program(Filename),
    Output = run(Registers, Program, 0, []),
    AsStrings = lists:map(fun(X) -> integer_to_list(X) end, Output),
    string:join(AsStrings, ",").


part_2(Filename) ->
    {{A, B, C}, Program} = parse_program(Filename),
    try
        lists:foreach(fun(N) -> repeat({N, B, C}, Program) end, lists:seq(0, A * 100))
    catch
        throw:A ->
            A
    end.


repeat({A, _, _} = Registers, Program) ->
    Output = run(Registers, Program, 0, []),
    OutputBin = list_to_binary([integer_to_list(X) || X <- Output]),
    case Program == OutputBin of
        true ->
            throw(A);
        false ->
            ok
    end.


parse_program(Filename) ->
    {ok, Content} = file:read_file(Filename),
    [RegisterBlob, ProgramBlob] = binary:split(Content, <<"\n\n">>),
    Registers = parse_registers(RegisterBlob),
    Program = parse_program_instructions(ProgramBlob),
    {Registers, Program}.


parse_registers(RegistersBlob) ->
    [{<<"A">>, A}, {<<"B">>, B}, {<<"C">>, C}] = lists:map(fun(Line) ->
                                                                   parse_register_line(Line)
                                                           end,
                                                           binary:split(RegistersBlob, <<"\n">>, [global])),
    {A, B, C}.


parse_register_line(<<"Register ", R:1/binary, ": ", Number/binary>>) ->
    {R, binary_to_integer(Number)}.


parse_program_instructions(<<"Program: ", Program/binary>>) ->
    binary:replace(Program, <<",">>, <<>>, [global]).


run(Registers, Program, ProgramPointer, Output) ->
    Operation = get_instuction(Program, ProgramPointer),
    case Operation of
        invalid ->
            [];
        halt ->
            lists:reverse(Output);
        {Opcode, Operand} ->
            {NewRegisters, NewProgramPointer, NewOutput} = process_safe(Opcode, Operand, Registers, ProgramPointer, Output),
            run(NewRegisters, Program, NewProgramPointer, NewOutput)
    end.


process_safe(Operator, Operand, Registers, ProgramPointer, Output) ->
    try
        process(Operator, Operand, Registers, ProgramPointer, Output)
    catch
        throw:invalid ->
            {Registers, invalid, Output}
    end.


process(?ADV, Operand, {A, B, C} = Registers, ProgramPointer, Output) ->
    Value = get_combo_operator(Operand, Registers),
    NewA = trunc(A / math:pow(2, Value)),
    {{NewA, B, C}, ProgramPointer + 2, Output};
process(?BXL, Operand, {A, B, C} = _Registers, ProgramPointer, Output) ->
    NewB = B bxor Operand,
    {{A, NewB, C}, ProgramPointer + 2, Output};
process(?BST, Operand, {A, _B, C} = Registers, ProgramPointer, Output) ->
    NewB = get_combo_operator(Operand, Registers) rem 8,
    {{A, NewB, C}, ProgramPointer + 2, Output};
process(?JNZ, _Operand, {0 = _A, _B, _C} = Registers, ProgramPointer, Output) ->
    {Registers, ProgramPointer + 2, Output};
process(?JNZ, Operand, Registers, _ProgramPointer, Output) ->
    {Registers, Operand, Output};
process(?BXC, _Operand, {A, B, C} = _Registers, ProgramPointer, Output) ->
    NewB = B bxor C,
    {{A, NewB, C}, ProgramPointer + 2, Output};
process(?OUT, Operand, Registers, ProgramPointer, Output) ->
    Value = get_combo_operator(Operand, Registers) rem 8,
    {Registers, ProgramPointer + 2, [Value | Output]};
process(?BDV, Operand, {A, _B, C} = Registers, ProgramPointer, Output) ->
    Value = get_combo_operator(Operand, Registers),
    NewB = trunc(A / math:pow(2, Value)),
    {{A, NewB, C}, ProgramPointer + 2, Output};
process(?CDV, Operand, {A, B, _C} = Registers, ProgramPointer, Output) ->
    Value = get_combo_operator(Operand, Registers),
    NewC = trunc(A / math:pow(2, Value)),
    {{A, B, NewC}, ProgramPointer + 2, Output}.


get_instuction(_Program, invalid) ->
    invalid;
get_instuction(Program, ProgramPointer) when size(Program) =< ProgramPointer ->
    halt;
get_instuction(Program, ProgramPointer) ->
    <<Opcode/integer, Operand/integer>> = binary:part(Program, ProgramPointer, 2),
    {Opcode - $0, Operand - $0}.


get_combo_operator(0, _Registers) ->
    0;
get_combo_operator(1, _Registers) ->
    1;
get_combo_operator(2, _Registers) ->
    2;
get_combo_operator(3, _Registers) ->
    3;
get_combo_operator(4, {A, _, _}) ->
    A;
get_combo_operator(5, {_, B, _}) ->
    B;
get_combo_operator(6, {_, _, C}) ->
    C;
get_combo_operator(7, _Registers) ->
    not_in_use.
