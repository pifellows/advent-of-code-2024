-module(advent).

-compile(export_all).


part_1(Filename) ->
    {ok, Contents} = file:read_file(Filename),
    {FileBlocks, EmptyBlocks, _Size} = parse_line(Contents),
    Backfilled = backfill(FileBlocks, EmptyBlocks),
    FillList = get_fill_list(Backfilled),
    Checksums = lists:map(fun(Item) -> expand(Item) end, FillList),
    lists:sum(lists:flatten(Checksums)).


part_2(Filename) ->
    {ok, Contents} = file:read_file(Filename),
    {FileBlocks, EmptyBlocks, _Size} = parse_line(Contents),
    {Files, Fills} = relocate_chunks(FileBlocks, EmptyBlocks),
    Normalised = lists:flatten(lists:map(
        fun(Item) ->
            normalise_empty(Item)
        end, Fills)),
    FillList = lists:sort(fun(X, Y) -> element(1, X) < element(1, Y) end, Files ++ Normalised),
    Checksums = lists:map(fun(Item) -> expand(Item) end, FillList),
    lists:sum(lists:flatten(Checksums)).


parse_line(Contents) ->
    %% List of FileBlocks and a List of Empty Blocks
    %% Each must have: Offset, Size
    %% FileBlock Needs ID
    %% EmptyBlock Needs Capacity and list of values
    parse_line(Contents, file, 0, {[], []}, 0).



parse_line(<<>>, _Type, _ID, {FileBlocks, EmptyBlocks}, Offset) ->
    {lists:reverse(FileBlocks), lists:reverse(EmptyBlocks), Offset};
parse_line(<<C, Rest/binary>>, file, ID, {FileBlocks, EmptyBlocks}, Offset) ->
    {FileBlock, Size} = create_file_block(ID, C, Offset),
    parse_line(Rest, empty, ID + 1, {[FileBlock | FileBlocks], EmptyBlocks}, Offset + Size);
parse_line(<<C, Rest/binary>>, empty, ID, {FileBlocks, EmptyBlocks}, Offset) ->
    {EmptyBlock, Size} = create_empty_block(C, Offset),
    parse_line(Rest, file, ID, {FileBlocks, [EmptyBlock | EmptyBlocks]}, Offset + Size).


create_file_block(ID, C, Offset) ->
    Size = list_to_integer([C]),
    {{Offset, Size, ID}, Size}.


create_empty_block(C, Offset) ->
    Size = list_to_integer([C]),
    {{Offset, Size, 0, []}, Size}.


backfill(FileBlocks, EmptyBlocks) ->
    ReversedFilesBlocks = lists:reverse(FileBlocks),
    backfill(ReversedFilesBlocks, EmptyBlocks, []).


backfill(FileBlocks, [], FilledEmptyBlocks) ->
    combine(lists:reverse(FileBlocks), lists:reverse(FilledEmptyBlocks));
backfill([{FileOffset, _, _} | _] = FileBlocks, [{EmptyOffset, _, _, _} | _] = EmptyBlocks, FilledEmptyBlocks) when (FileOffset =< EmptyOffset) ->
    AllEmpties = lists:reverse(FilledEmptyBlocks) ++ EmptyBlocks,
    combine(lists:reverse(FileBlocks), AllEmpties);
backfill([FileBlock | FileBlocks], [EmptyBlock | EmptyBlocks], FilledEmptyBlocks) ->
    {NewFileBlock, NewEmptyBlock} = fill_empty_block(FileBlock, EmptyBlock),
    NewFileBlocks = maybe_add_fileblock(NewFileBlock, FileBlocks),
    {NewEmptyBlocks, NewFilledBlocks} = maybe_add_emptyblock(NewEmptyBlock, EmptyBlocks, FilledEmptyBlocks),
    backfill(NewFileBlocks, NewEmptyBlocks, NewFilledBlocks).

fill_empty_block({FileOffset, FileSize, ID}, {EmptyOffset, EmptySize, Capacity, Values}) ->
    CanTake = EmptySize - Capacity,
    case FileSize =< CanTake of
        true ->
            {done, {EmptyOffset, EmptySize, Capacity + FileSize, [{FileSize, ID} | Values]}};
        false ->
            {{FileOffset + CanTake, FileSize - CanTake, ID}, {EmptyOffset, EmptySize, EmptySize, [{CanTake, ID} | Values]}}
        end.

maybe_add_fileblock(done, FileBlocks) ->
    FileBlocks;
maybe_add_fileblock(FileBlock, FileBlocks) ->
    [FileBlock | FileBlocks].

maybe_add_emptyblock({Offset, Size, Size, Values}, EmptyBlocks, FilledEmptyBlocks) ->
    {EmptyBlocks, [{Offset, Size, Size, lists:reverse(Values)} | FilledEmptyBlocks]};
maybe_add_emptyblock(PartiallyFilledBlock, EmptyBlocks, FilledEmptyBlocks) ->
    {[PartiallyFilledBlock | EmptyBlocks], FilledEmptyBlocks}.

combine(FileBlocks, EmptyBlocks) ->
    combine(FileBlocks, EmptyBlocks, []).

combine([], Emptys, Acc) ->
    lists:reverse(Acc) ++ Emptys;
combine(Files, [], Acc) ->
    lists:reverse(Acc) ++ Files;
combine([FH | FT], [EH | ET], Acc) ->
    combine(FT, ET, [EH, FH | Acc]).

get_fill_list(Backfilled) ->
    L = lists:map(fun(Block) -> to_fill_list(Block) end, Backfilled),
    lists:flatten(L).

to_fill_list({_Offset, _Size, _ID} = Block) ->
    Block;
to_fill_list({StartOffset, _StartSize, _Capacity, Values} = _Blocks) ->
    {Return, _FinalOffset} = lists:foldl(fun({Size, ID}, {Acc, Offset}) -> 
        {[{Offset, Size, ID} | Acc], Offset + Size}
    end, {[], StartOffset}, Values),
    lists:reverse(Return).

get_checksum({Offset, Size, ID}) ->
    get_checksum(Offset, Size, ID, 0).

get_checksum(_Pos, 0, _ID, Acc) ->
    Acc;
get_checksum(Pos, Remaining, ID, Acc) ->
    Checksum = Pos * ID,
    get_checksum(Pos + 1, Remaining -1, ID, Acc + Checksum). 

expand({Offset, Size, Value}) ->
    Seq = lists:seq(Offset, Offset + Size - 1),
    Vals = lists:duplicate(Size, Value),
    Zipped = lists:zip(Seq, Vals),
    lists:map(fun({X, Y}) -> X * Y end, Zipped).

print({_Offset, Size, Value}) ->
    Vals = lists:duplicate(Size, integer_to_list(Value)),
    Vals.


relocate_block(_FileBlock, [], Acc) ->
    {not_moved, lists:reverse(Acc)};
relocate_block(FileBlock, [CurrentEmpty | Remaining], Acc) ->
    case can_fit_file_in_space(FileBlock, CurrentEmpty) of
        {true, NewEmpty} ->
            {moved, lists:reverse(Acc) ++ [NewEmpty | Remaining]};
        false ->
            relocate_block(FileBlock, Remaining, [CurrentEmpty | Acc])
        end.

relocate_chunks(FileBlocks, EmptyBlocks) ->
    Reversed = lists:reverse(FileBlocks),
    relocate_chunks(Reversed, EmptyBlocks, []).


relocate_chunks([], EmptyBlocks, Acc) ->
    {Acc, EmptyBlocks};
relocate_chunks([H | T], EmptyBlocks, Acc) ->
    {Moved, NewEmptyBlocks} = relocate_block(H, EmptyBlocks, []),
    NewAcc = case Moved of
        not_moved ->
            [H | Acc];
        moved ->
            Acc
        end,
    relocate_chunks(T, NewEmptyBlocks, NewAcc).

can_fit_file_in_space(FileBlock, EmptyBlock) ->
    {FileOffset, FileSize, Value} = FileBlock,
    {EmptyOffset, EmptySize, Capacity, Values} = EmptyBlock,
    case FileOffset =< EmptyOffset orelse FileSize > EmptySize of
        true ->
            false;
        false ->
            NewEmptyBlock = {EmptyOffset + FileSize, EmptySize - FileSize, Capacity, [{EmptyOffset, FileSize, Value} | Values]}, %% may need to reverse this
            {true, NewEmptyBlock}
        end.

normalise_empty({Offset, Size, _, []}) ->
    {Offset, Size, 0};
normalise_empty({_Offset, 0, _, Filled}) ->
    lists:reverse(Filled);
normalise_empty({Offset, Size, _, Filled}) ->
    lists:reverse([{Offset, Size, 0} |Filled]).

