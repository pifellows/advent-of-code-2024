-module(advent).

-compile(export_all).


part_1(Filename) ->
    {Files, Empty} = parse_file(Filename),
    Backfilled = backfill(Files, Empty),
    %% Calculate Checksum
    calculate_checksum(Backfilled).


part_2(Filename) ->
    Something = parse_file(Filename),
    Something.


parse_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    parse_line(Content).


parse_line(<<First, Rest/binary>>) ->
    %% Accumulator - List of Blocks with their IDs/Size and A List of Free B locks
    ID = 0,
    Acc = {[{ID, list_to_integer([First])}], []},
    parse_line(Rest, Acc, ID + 1, empty).


parse_line(<<>>, {Files, Empty} = _Acc, _ID, _Flag) ->
    {lists:reverse(Files), lists:reverse(Empty)};
parse_line(<<C/integer, Rest/binary>>, {Files, Empty} = _Acc, ID, empty) ->
    NewEmpty = [{list_to_integer([C]), 0, []} | Empty],
    parse_line(Rest, {Files, NewEmpty}, ID, file);
parse_line(<<C/integer, Rest/binary>>, {Files, Empty} = _Acc, ID, file) ->
    NewFiles = [{ID, list_to_integer([C])} | Files],
    parse_line(Rest, {NewFiles, Empty}, ID + 1, empty).


backfill(Files, Empty) ->
    ReversedFiles = lists:reverse(Files),
    backfill(ReversedFiles, Empty, []).


backfill(Files, [], Acc) ->
    reorder_blocks(lists:reverse(Files), lists:reverse(Acc));
backfill([File | RestFiles], [Empty | RestEmpty], Acc) ->
    {NewFile, NewEmpty, NewAcc} = move_files(File, Empty, Acc),
    NewFiles = maybe_add_back_file(NewFile, RestFiles),
    NewEmpties = maybe_add_back_empty(NewEmpty, RestEmpty),
    backfill(NewFiles, NewEmpties, NewAcc).


move_files({ID, Amount} = _File, {Size, Capacity, Values} = _Empty, Acc) ->
    CanTake = Size - Capacity,
    {FileBlock, EmptyBlock} = case Amount =< CanTake of
                                  true ->
                                      {done, {Size, Capacity + Amount, [{ID, Amount} | Values]}};
                                  false ->
                                      {{ID, Amount - CanTake}, {Size, Capacity + CanTake, [{ID, CanTake} | Values]}}
                              end,

    NewAcc = case EmptyBlock of
                 {Size, Size, _Values} ->
                     [EmptyBlock | Acc];
                 _ ->
                     Acc
             end,

    {FileBlock, EmptyBlock, NewAcc}.


maybe_add_back_file(done, Files) ->
    Files;
maybe_add_back_file(File, Files) ->
    [File | Files].


maybe_add_back_empty(Empty, Empties) ->
    {Size, Capacity, _Value} = Empty,
    case Size == Capacity of
        true ->
            Empties;
        false ->
            [Empty | Empties]
    end.


reorder_blocks(Files, EmptyBlocks) ->
    reorder_blocks(Files, EmptyBlocks, []).

reorder_blocks([], _EmptyBlocks, Acc) ->
    lists:reverse(Acc);
reorder_blocks(Files, [], Acc) ->
    lists:reverse(Files ++ Acc);
reorder_blocks([File | Files], [{_Size, _Capacity, Empty} | EmptyBlocks], Acc) ->
    reorder_blocks(Files, EmptyBlocks, [Empty, File | Acc]).

calculate_checksum(Blocks) ->
    calculate_checksums(lists:flatten(Blocks),0, 0).

calculate_checksums([], _Position, Checksum) ->
    Checksum;
calculate_checksums([{_ID, Amount} = Block | Blocks], Position, Checksum) ->
    Value = checksum_block(Block, Position),
    calculate_checksums(Blocks, Position + Amount, Checksum + Value).

checksum_block(Block, Position) ->
    checksum_block(Block, Position, 0).

checksum_block({_ID, 0}, _Position, Checksum) ->
    Checksum;
checksum_block({ID, Amount}, Position, Checksum) ->
    Value = ID * Position,
    checksum_block({ID, Amount - 1}, Position + 1, Checksum + Value).