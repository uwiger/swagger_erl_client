%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(swec_schema).

-export([parse/2]).
-compile({parse_transform, parse_trans_codegen}).

parse(File, Mod) ->
    rules(fun yamerl:decode_file/1, File, Mod).


rules(Fun, Arg, Mod) ->
    application:ensure_started(yamerl),
    YamlDoc = case Fun(Arg) of
		  [YD|_] -> YD;
		  []     -> [] end,
    {MetaExps, MetaFs} = gen_meta(YamlDoc),
    {DefExps, DefFs} =
        gen_definitions(
          proplists:get_value("definitions", YamlDoc, [])),
    Forms =
        [{attribute, 1, module, Mod},
         {attribute, 2, export, lists:flatten([MetaExps, DefExps])}
         | lists:flatten([MetaFs, DefFs])],
    compile:forms(Forms, [return_errors, return_warnings]).

gen_meta(Doc) ->
    Res =
        lists:flatmap(
          fun({"swagger", X}) ->
                  [{{swagger, 0},
                    codegen:gen_function(
                      swagger, fun() -> {'$var', X} end)}];
             ({"openapi", X}) ->
                  [{{openapi, 0},
                    codegen:gen_function(
                      openapi, fun() -> {'$var', X} end)}];
             ({"info", I}) ->
                  [{{info, 0},
                    codegen:gen_function(
                      info, fun() -> {'$var', I} end)},
                   {{info, 1},
                    codegen:gen_function(
                      info, [fun({'$var', X}) -> {'$var', Y} end
                             || {X, Y} <- I])}];
             ({"basePath", X}) ->
                  [{{basePath, 0},
                    codegen:gen_function(
                      basePath,
                      fun() -> {'$var', X} end)}];
             ({"tags", X0}) ->
                  X = lists:flatten(X0),
                  [{{tags, 0},
                    codegen:gen_function(
                      tags,
                      fun() -> {'$var', X} end)},
                   {{tags, 1},
                    codegen:gen_function(
                      tags,
                      [fun({'$var', K}) -> {'$var', V} end
                       || {K, V} <- X])}];
             (_) ->
                  []
          end, Doc),
    lists:unzip(Res).

gen_definitions(Defs) ->
    Types = [{Name, proplists:get_value("type", Opts)}
             || {Name, Opts} <- Defs],
    Props = [{Name, proplists:get_value("properties", Opts)}
             || {Name, Opts} <- Defs],
    TypeF = codegen:gen_function(
              type,
              [fun({'$var', X}) -> {'$var', Y} end
               || {X, Y} <- Types]),
    PropF = codegen:gen_function(
              properties,
              [fun({'$var', X}) -> {'$var', Y} end
               || {X, Y} <- Props]),
    PropF2 = codegen:gen_function(
               properties,
               [fun({'$var', X}, {'$var', P}) ->
                        {'$var', V}
                end || {X, P, V} <- [{X1,P1,V1}
                                     || {X1,Y} <- Props,
                                        {P1,V1} <- Y]]),
    PropF3 = codegen:gen_function(
               properties,
               [fun({'$var', X}, {'$var', P}, {'$var', Y}) ->
                        {'$var', V}
                end || {X, P, Y, V} <- [{X1,P1,Y1,V1}
                                        || {X1,L1} <- Props,
                                           {P1,L2} <- L1,
                                           {Y1, V1} <- L2]]),
    {[{type, 1}, {properties, 1}, {properties, 2}, {properties, 3}],
     [TypeF, PropF, PropF2, PropF3]}.
