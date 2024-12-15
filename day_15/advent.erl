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


expand_movements(Movements) ->
    lists:flatten(
      lists:map(fun({D, A}) -> lists:duplicate(A, {D, 1}) end, Movements)).


print_steps(Robot, Movements, Tiles, Size) ->
    print_steps(Robot, Movements, Tiles, Size, 0).


print_steps(Robot, [] = _Movements, Tiles, Size, _Step) ->
    io:format("Final Position:~n"),
    print_warehouse_map(Robot, Tiles, Size);
print_steps(Robot, Movements, Tiles, Size, 0) ->
    io:format("Step ~p:~n", [0]),
    print_warehouse_map(Robot, Tiles, Size),
    io:format("~n"),
    print_steps(Robot, expand_movements(Movements), Tiles, Size, 1);
print_steps(Robot, [Movement | Movements], Tiles, Size, Step) ->
    {NewRobot, NewTiles} = apply_movement(Robot, Movement, Tiles),
    io:format("Movement=~p, Step ~p:~n", [Step, Movement]),
    print_warehouse_map(NewRobot, NewTiles, Size),
    io:format("~n"),
    print_steps(NewRobot, Movements, NewTiles, Size, Step + 1).


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


add({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.


subtract({X1, Y1}, {X2, Y2}) ->
    {X1 - X2, Y1 - Y2}.


multiply({X1, Y1}, S) ->
    {X1 * S, Y1 * S}.


inverse({X, Y}) ->
    {-X, -Y}.


apply_all_movement(Robot, [] = _Movements, Tiles) ->
    {Robot, Tiles};
apply_all_movement(Robot, [Movement | Movements], Tiles) ->
    {NewRobot, NewTiles} = apply_movement(Robot, Movement, Tiles),
    apply_all_movement(NewRobot, Movements, NewTiles).


apply_movement(Robot, Movement, Tiles) ->
    {MovementDirection, MovementAmount} = get_movement_data(Movement),
    {MaxFreeEndPosition, Boxes} = get_robot_movement_data(Robot, MovementDirection, MovementAmount, Tiles),
    {NewRobot, NewTiles} = move_boxes(MaxFreeEndPosition, Boxes, inverse(MovementDirection), Tiles),
    {NewRobot, NewTiles}.


get_movement_data({<<"^">>, Amount}) ->
    {{-1, 0}, Amount};
get_movement_data({<<"<">>, Amount}) ->
    {{0, -1}, Amount};
get_movement_data({<<"v">>, Amount}) ->
    {{1, 0}, Amount};
get_movement_data({<<">">>, Amount}) ->
    {{0, 1}, Amount}.


get_robot_movement_data(Robot, MovementDirection, MovementAmount, Tiles) ->
    get_robot_movement_data(Robot,
                            MovementDirection,
                            MovementAmount,
                            Tiles,
                            {add(Robot, multiply(MovementDirection, MovementAmount)), []}).


get_robot_movement_data(_Robot, _MovementDirection, 0 = _MovementAmount, _Tiles, Acc) ->
    Acc;
get_robot_movement_data(Robot, MovementDirection, MovementAmount, Tiles, {CurrentFinalPosition, Boxes} = Acc) ->
    NextRobot = add(Robot, MovementDirection),
    case maps:get(NextRobot, Tiles, undefined) of
        wall ->
            {Robot, Boxes};
        box ->
            get_robot_movement_data(NextRobot,
                                    MovementDirection,
                                    MovementAmount,
                                    Tiles,
                                    {CurrentFinalPosition, [NextRobot | Boxes]});
        undefined ->
            get_robot_movement_data(NextRobot, MovementDirection, -1, Tiles, Acc)
    end.


move_boxes(Pos, [] = _Boxes, _MovementDirection, Tiles) ->
    {Pos, Tiles};
move_boxes(Pos, [Box | Boxes], MovementDirection, Tiles) ->
    %% remove the old box location and add it to the new position
    Tiles1 = maps:remove(Box, Tiles),
    Tiles2 = maps:put(Pos, box, Tiles1),
    move_boxes(add(Pos, MovementDirection), Boxes, MovementDirection, Tiles2).
