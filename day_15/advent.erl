-module(advent).

-compile(export_all).


parse_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    [WarehouseBlock, MovementBlock] = binary:split(Content, <<"\n\n">>),
    Movements = parse_movements(MovementBlock),
    WarehouseData = parse_warehouse(WarehouseBlock),
    {WarehouseData, Movements}.


parse_movements(MovementBlock) ->
    parse_movements(MovementBlock, {<<"">>, 0}, []).


parse_movements(<<>>, FinalMovement, Acc) ->
    tl(lists:reverse([FinalMovement | Acc]));  %% Tl (tail) removes the first element which would be the empty starter
parse_movements(<<C:1/binary, Rest/binary>>, {C, N}, Acc) ->
    parse_movements(Rest, {C, N + 1}, Acc);
parse_movements(<<C:1/binary, Rest/binary>>, LastMovement, Acc) ->
    parse_movements(Rest, {C, 1}, [LastMovement | Acc]).


parse_warehouse(WarehouseBlock) ->
    Rows = binary:split(WarehouseBlock, <<"\n">>, [global]),
    Size = {length(Rows), size(hd(Rows))},
    WarehouseTiles = lists:foldl(
      fun({X, L}, W) ->
              Ys = parse_warehouse_line(L),
              lists:foldl(fun({Y, V}, W1) -> maps:put({X, Y}, V, W1) end, W, Ys)
      end,
      #{},
      lists:enumerate(0, Rows)),
      [{RK, _}] = maps:to_list(maps:filter(fun(_K, V) -> V == robot end, WarehouseTiles)),
      WarehouseTilesNoRobot = maps:remove(RK, WarehouseTiles),
      {RK, WarehouseTilesNoRobot, Size}.


parse_warehouse_line(L) ->
    parse_warehouse_line(L, 0, []).


parse_warehouse_line(<<>>, _Y, Acc) ->
    Acc;
parse_warehouse_line(<<"#", Rest/binary>>, Y, Acc) ->
    parse_warehouse_line(Rest, Y + 1, [{Y, wall} | Acc]);
parse_warehouse_line(<<"O", Rest/binary>>, Y, Acc) ->
    parse_warehouse_line(Rest, Y + 1, [{Y, box} | Acc]);
parse_warehouse_line(<<"@", Rest/binary>>, Y, Acc) ->
    parse_warehouse_line(Rest, Y + 1, [{Y, robot} | Acc]);
parse_warehouse_line(<<_C:1/binary, Rest/binary>>, Y, Acc) ->
    parse_warehouse_line(Rest, Y + 1, Acc).


print_warehouse_map(Robot, Points, Size) ->
    NewPoints = maps:put(Robot, robot, Points),
    print_warehouse_map(NewPoints, Size).


print_warehouse_map(Points, Size) ->
    Map = get_warehouse_map(Points, Size),
    io:format("~s", [Map]).

get_warehouse_map(Points, {MaxX, MaxY}) ->
    WarehouseTiles = [ {X, Y} || X <- lists:seq(0, MaxX - 1), Y <- lists:seq(0, MaxY) ],
    lists:map(
      fun({_X, Y}) when Y == MaxY ->
              "\n";
         ({X, Y}) ->
              case maps:get({X, Y}, Points, undefined) of
                  undefined ->
                      ".";
                  V ->
                      value_atom_to_character(V)
              end
      end,
      WarehouseTiles).


value_atom_to_character(wall) ->
    "#";
value_atom_to_character(robot) ->
    "@";
value_atom_to_character(box) ->
    "O".