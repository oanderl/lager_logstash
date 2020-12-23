-module(lager_logstash).

-export([format/1, jsonify/1]).

-spec format(term()) -> atom() | binary() | list() | map().
format(Pid) when erlang:is_pid(Pid) ->
    erlang:list_to_binary(erlang:pid_to_list(Pid));
format(Port) when erlang:is_port(Port) ->
    erlang:list_to_binary(erlang:port_to_list(Port));
format(Ref) when erlang:is_reference(Ref) ->
    erlang:list_to_binary(erlang:ref_to_list(Ref));
format(Fun) when erlang:is_function(Fun) ->
    erlang:list_to_binary(erlang:fun_to_list(Fun));
format(Tuple) when erlang:is_tuple(Tuple) ->
    erlang:list_to_binary(io_lib:format("~p", [Tuple]));
format(List) when erlang:is_list(List) ->
    case io_lib:char_list(List) of
        true -> erlang:list_to_binary(List);
        false -> jsonify(List)
    end;
format(Map) when erlang:is_map(Map) ->
    jsonify(Map);
format(Safe) -> Safe.

-spec jsonify(map() | list()) -> map() | list().
jsonify(List) when erlang:is_list(List) ->
    case catch maps:from_list(List) of
        #{} = Map -> jsonify(Map);
        _ -> lists:map(fun format/1, List)
    end;
jsonify(Map) when erlang:is_map(Map) ->
    maps:fold(fun (Key, Value, Acc) ->
        Acc#{format(Key) => format(Value)}
    end, #{}, Map).
