-module(advent).

-compile(export_all).


part_1(Filename) ->
    {Rules, Updates} = parse_file(Filename),
    MiddleNums = lists:map(fun(Update) ->
                                   case is_update_complient(Update, Rules) of
                                       true ->
                                           find_middle_number(Update);
                                       false ->
                                           0
                                   end
                           end,
                           Updates),
    lists:sum(MiddleNums).


part_2(Filename) ->
    {Rules, Updates} = parse_file(Filename),
    ToReorder = lists:filter(fun(Update) -> false == is_update_complient(Update, Rules) end, Updates),
    Corrected = lists:map(fun(Update) ->
                                  Reordered = reorder_update(Update, Rules),
                                  find_middle_number(Reordered)
                          end,
                          ToReorder),
    lists:sum(Corrected).


parse_file(Filename) ->
    {ok, Contents} = file:read_file(Filename),
    [RulesSection, UpdatesSection] = binary:split(Contents, <<"\n\n">>),
    Rules = parse_rules(RulesSection),
    Updates = parse_update(UpdatesSection),
    {Rules, Updates}.


parse_rules(RulesSection) ->
    TupleList = lists:map(fun(Line) ->
                                  [Before, After] = binary:split(Line, <<"|">>),
                                  {binary_to_integer(Before), binary_to_integer(After)}
                          end,
                          binary:split(RulesSection, <<"\n">>, [global])),
    create_rule_sets(TupleList, #{}).


create_rule_sets([], RuleSets) ->
    RuleSets;
create_rule_sets([{Before, After} | Rest], RuleSets) ->
    CurrentSet = maps:get(Before, RuleSets, sets:new()),
    NewSet = sets:add_element(After, CurrentSet),
    create_rule_sets(Rest, maps:put(Before, NewSet, RuleSets)).


parse_update(UpdatesSection) ->
    lists:map(fun(Line) ->
                      Tokens = binary:split(Line, <<",">>, [global]),
                      lists:map(fun(X) -> binary_to_integer(X) end, Tokens)
              end,
              binary:split(UpdatesSection, <<"\n">>, [global])).


is_update_complient(Update, RuleSet) ->
    is_update_complient(Update, RuleSet, sets:new()).


is_update_complient([], _RuleSet, _BeforeSet) ->
    true;
is_update_complient([Entry | Rest], RuleSet, BeforeSet) ->
    %% We have a set of rules where the Key Must Appear before the values
    %% in it's corresponding set.
    %% Therefore, if we have already seen any value in the set before,
    %% then we are out of order
    EntryRules = maps:get(Entry, RuleSet, sets:new()),
    Intersection = sets:intersection(EntryRules, BeforeSet),
    case sets:is_empty(Intersection) of
        false ->
            false;
        true ->
            is_update_complient(Rest, RuleSet, sets:add_element(Entry, BeforeSet))
    end.


find_middle_number(Update) ->
    TotalLength = length(Update),
    Take = trunc(math:ceil(TotalLength / 2)),
    lists:nth(Take, Update).


reorder_update(Update, Rules) ->
    DistilledRuleSet = distill_rulesets(Update, Rules),
    OrderedRuleSets = order_rulesets(DistilledRuleSet),
    lists:map(fun({X, _Set}) -> X end, OrderedRuleSets).


distill_rulesets(Updates, Rules) ->
    UpdateSet = sets:from_list(Updates),
    lists:map(fun(Entry) ->
                      EntryRules = maps:get(Entry, Rules, sets:new()),
                      {Entry, sets:intersection(UpdateSet, EntryRules)}
              end,
              Updates).


order_rulesets(RuleSets) ->
    %% Order by the size of the sets - from Largest to smallest
    %% This means that the first entry is dependent on the most 
    %% things after it
    lists:sort(fun({_N1, Set1}, {_N2, Set2}) ->
                       sets:size(Set1) > sets:size(Set2)
               end,
               RuleSets).
