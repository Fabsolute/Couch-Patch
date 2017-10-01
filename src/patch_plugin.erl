-module(patch_plugin).

-export([parse_ops/1, apply_patch/2]).

apply_patch([], Object) ->
  {ok, Object};

apply_patch([{_Op, [], _Value} | _T], _Object) ->
  throw({error, not_supported});

apply_patch([{_Op, [<<"">>], _Value} | _T], _Object) ->
  throw({error, not_supported});

apply_patch([Op | T], Object) ->
  case apply_patch(Op, Object) of
    {ok, NewObject} -> apply_patch(T, NewObject);
    {error, Error} -> throw({error, Error})
  end;

apply_patch({add, FieldList, Value}, Object) -> add(FieldList, Value, Object, nothing);
apply_patch({remove, FieldList}, Object) -> remove(FieldList, Object);
apply_patch({replace, FieldList, Value}, Object) -> replace(FieldList, Value, Object);
apply_patch({move, FromFieldList, ToFieldList}, Object) -> move(FromFieldList, ToFieldList, Object);
apply_patch({copy, FromFieldList, ToFieldList}, Object) -> copy(FromFieldList, ToFieldList, Object);
apply_patch({test, FieldList, Value}, Object) -> test(FieldList, Value, Object).

add([Field], Value, Object, Operation) ->
  add(Field, Value, Object, Operation);

add([Field | FieldList], Value, Object, Operation) ->
  case get(Field, Object) of
    notfound -> {error, not_found};
    FieldObject ->
      case add(FieldList, Value, FieldObject, Operation) of
        {ok, NewValue} -> set(Field, NewValue, Object);
        Other -> Other
      end
  end;

add(Field, Value, Object, _Operation) when is_integer(Field) andalso Field >= 0 andalso Field < length(Object) ->
  {L1, L2} = lists:split(Field, Object),
  {ok, L1 ++ [Value | L2]};

add(Field, Value, Object, ignore_index) when is_integer(Field) andalso Field == length(Object) ->
  add(<<"-">>, Value, Object, ignore_index);

add(<<"-">>, Value, Object, _Operation) ->
  {ok, Object ++ [Value]};

add(Field, Value, Object, _Operation) ->
  set(Field, Value, Object).


remove([Field], Object) ->
  remove(Field, Object);

remove([Field | FieldList], Object) ->
  case get(Field, Object) of
    notfound -> {error, notfound};
    FieldObject ->
      case remove(FieldList, FieldObject) of
        {ok, NewValue} -> set(Field, NewValue, Object);
        Other -> Other
      end
  end;

remove(Field, Object) when is_integer(Field) andalso Field >= 0 andalso Field < length(Object) ->
  NewObject = case lists:split(Field, Object) of
                {[], [_ | T]} -> T;
                {H1, [_ | T]} -> H1 ++ T;
                {H1, []} -> lists:droplast(H1)
              end,
  {ok, NewObject};

remove(Field, {Object}) ->
  case get(Field, Object) of
    notfound -> {error, notfound};
    _ -> {ok, {lists:keydelete(Field, 1, Object)}}
  end;

remove(_Field, _Object) ->
  {error, not_found}.

replace(FieldList, Value, Object) ->
  case remove(FieldList, Object) of
    {ok, NewObject} ->
      add(FieldList, Value, NewObject, ignore_index);
    Other -> Other
  end.

move(FromFieldList, ToFieldList, Object) ->
  case get(FromFieldList, Object) of
    notfound ->
      {error, notfound};
    Value ->
      Operation = case lists:droplast(FromFieldList) == lists:droplast(ToFieldList) of
                    true -> ignore_index;
                    _ -> nothing
                  end,
      {ok, NewObject} = remove(FromFieldList, Object),
      add(ToFieldList, Value, NewObject, Operation)
  end.


copy(FromFieldList, ToFieldList, Object) ->
  case get(FromFieldList, Object) of
    notfound ->
      {error, notfound};
    Value ->
      add(ToFieldList, Value, Object, nothing)
  end.


test(FieldList, Value, Object) ->
  case get(FieldList, Object) of
    notfound ->
      {error, notfound};
    Value -> {ok, Object};
    _ -> {error, test_failed}
  end.


set(Field, Value, {Object}) when is_integer(Field) -> set(Field, Value, Object);

set(Field, Value, Object) when is_integer(Field) andalso Field >= 0 andalso Field < length(Object) ->
  {ok, case lists:split(Field, Object) of
         {[], [_ | T]} -> [Value] ++ T;
         {H1, [_ | T]} -> H1 ++ [Value] ++ T;
         {H1, []} -> H1 ++ [Value]
       end};

set(Field, _Value, _Object) when is_integer(Field) andalso Field >= 0 ->
  {error, not_found};

set(Field, Value, {Object}) ->
  Property = proplists:property(Field, Value),
  case proplists:is_defined(Field, Object) of
    true -> {ok, {lists:keyreplace(Field, 1, Object, Property)}};
    false -> {ok, {Object ++ [Property]}}
  end;

set(Field, Value, _Object) ->
  {ok, {[proplists:property(Field, Value)]}}.

get([Field], Object) -> get(Field, Object);

get([Field | FieldList], Object) ->
  case get(Field, Object) of
    notfound -> notfound;
    FieldObject -> get(FieldList, FieldObject)
  end;

get(Field, {Object}) ->
  get(Field, Object);

get(Field, Object) when is_integer(Field) ->
  if Field >= 0 andalso Field < length(Object) ->
    lists:nth(Field + 1, Object);
    true -> notfound
  end;

get(Field, Object) when is_list(Object) ->
  proplists:get_value(Field, Object, notfound);

get(_Field, _Object) ->
  notfound.

parse_ops({Data}) ->
  parse_ops(Data);

parse_ops(Data) ->
  SortedData = [{lists:sort(fun({KeyA, _}, {KeyB, _}) -> KeyA =< KeyB end, X)} || {X} <- Data],
  parse_ops(SortedData, []).

parse_ops([], Accumulator) ->
  {ok, lists:reverse(Accumulator)};

parse_ops([{[{<<"op">>, <<"add">>}, {<<"path">>, Path}, {<<"value">>, Value}]} | T], Accumulator) ->
  parse_ops(T, [{add, parse_path(Path), Value} | Accumulator]);

parse_ops([{[{<<"op">>, <<"remove">>}, {<<"path">>, Path}]} | T], Accumulator) ->
  parse_ops(T, [{remove, parse_path(Path)} | Accumulator]);

parse_ops([{[{<<"op">>, <<"replace">>}, {<<"path">>, Path}, {<<"value">>, Value}]} | T], Accumulator) ->
  parse_ops(T, [{replace, parse_path(Path), Value} | Accumulator]);

parse_ops([{[{<<"from">>, From}, {<<"op">>, <<"move">>}, {<<"path">>, To}]} | T], Accumulator) ->
  parse_ops(T, [{move, parse_path(From), parse_path(To)} | Accumulator]);

parse_ops([{[{<<"from">>, From}, {<<"op">>, <<"copy">>}, {<<"path">>, To}]} | T], Accumulator) ->
  parse_ops(T, [{copy, parse_path(From), parse_path(To)} | Accumulator]);

parse_ops([{[{<<"op">>, <<"test">>}, {<<"path">>, Path}, {<<"value">>, Value}]} | T], Accumulator) ->
  parse_ops(T, [{test, parse_path(Path), Value} | Accumulator]);

parse_ops(_, _Accumulator) -> throw({error, invalid_format}).

parse_path(Path) ->
  FieldList = tl(binary:split(Path, <<"/">>, [global])),
  [regex_field(X) || X <- FieldList].

regex_field(Field) ->
  case re:run(Field, "^[0-9]+$") of
    {match, _} -> binary_to_integer(Field);
    _ -> replace_field(Field)
  end.

replace_field(OldField) ->
  Field = re:replace(re:replace(OldField, "\~1", "/"), "\~0", "~"),
  if is_list(Field) -> list_to_binary(Field);
    true -> Field
  end.
