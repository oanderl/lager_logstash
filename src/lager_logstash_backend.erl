-module(lager_logstash_backend).

-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    terminate/1
]).

-record(state, {level, host, port, socket, blacklist, whitelist}).

-spec init(list() | map()) -> {ok, #state{}}.
init(Opts) when erlang:is_list(Opts) ->
    init(maps:from_list(Opts));
init(#{} = Opts) ->
    State = init_state(Opts),
    init_socket(State).

-spec handle_call(term(), #state{}) -> {ok, term(), #state{}}.
handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, #state{} = State) ->
    {ok, ok, State#state{level = lager_util:config_to_mask(Level)}};
handle_call(_, State) ->
    {ok, ok, State}.

-spec handle_event(term(), #state{}) -> {ok, #state{}}.
handle_event({log, Message}, #state{level = Level} = State) ->
    case lager_util:is_loggable(Message, Level, ?MODULE) of
        true -> send_message(Message, State);
        false -> {ok, State}
    end;
handle_event(_, State) ->
    {ok, State}.

-spec handle_info(_, #state{}) -> {ok, #state{}}.
handle_info(_, State) ->
    {ok, State}.

-spec terminate(#state{}) -> ok.
terminate(#state{socket = Socket}) ->
    gen_udp:close(Socket).

-spec init_state(#state{}) -> #state{}.
init_state(#{} = Opts) ->
    #state{
        level = lager_util:config_to_mask(maps:get(level, Opts, info)),
        host = maps:get(host, Opts),
        port = maps:get(port, Opts),
        blacklist = maps:get(blacklist, Opts, []),
        whitelist = maps:get(whitelist, Opts, [])
    }.

-spec init_socket(#state{}) -> #state{}.
init_socket(#state{} = State) ->
    case gen_udp:open(0, [binary, {active, false}]) of
        {ok, Socket} -> {ok, State#state{socket = Socket}};
        {error, Reason} -> {error, Reason}
    end.

-spec send_message(lager_msg:lager_msg(), #state{}) -> {ok, #state{}}.
send_message(Message, #state{socket = Socket, host = Host, port = Port} = State) ->
    Payload = [
        {severity, lager_msg:severity(Message)},
        {'@.timestamp', get_timestamp(lager_msg:timestamp(Message))},
        {message, unicode:characters_to_binary(lager_msg:message(Message))},
        {fields, get_safe_fields(lager_msg:metadata(Message), State)}
    ],
    {gen_udp:send(Socket, Host, Port, jsx:encode(Payload)), State}.

-spec get_timestamp(erlang:timestamp()) -> binary().
get_timestamp({MegaSecs, Secs, MicroSecs}) ->
    MilliSecs = MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000,
    DateTime = calendar:system_time_to_rfc3339(MilliSecs, [{offset, "Z"}, {unit, millisecond}]),
    erlang:list_to_binary(DateTime).

-spec get_safe_fields(list(), #state{}) -> map().
get_safe_fields(Meta, #state{blacklist = Blacklist} = State) ->
    lists:foldl(fun ({Key, Value}, Acc) ->
        case lists:member(Key, Blacklist) of
            false -> Acc ++ [get_safe_field({Key, Value}, State)];
            true -> Acc
        end
    end, [], Meta).

-spec get_safe_field({term(), term()}, #state{}) -> {atom() | binary(), term()}.
get_safe_field({Key, Value}, #state{whitelist = Whitelist}) ->
    case lists:member(Key, Whitelist) of
        false -> {get_safe_key(Key), get_safe_value(Value)};
        true -> {get_safe_key(Key), Value}
    end.

-spec get_safe_key(term()) -> atom() | binary().
get_safe_key(Key) when erlang:is_atom(Key); erlang:is_binary(Key) -> Key;
get_safe_key(Key) when erlang:is_list(Key) -> erlang:list_to_binary(Key).

-spec get_safe_value(term()) -> binary().
get_safe_value(Value) when erlang:is_atom(Value) -> Value;
get_safe_value(Value) when erlang:is_binary(Value) -> Value;
get_safe_value(Value) when erlang:is_pid(Value) ->
    erlang:list_to_binary(erlang:pid_to_list(Value));
get_safe_value(Value) when erlang:is_port(Value) ->
    erlang:list_to_binary(erlang:port_to_list(Value));
get_safe_value(Value) when erlang:is_reference(Value) ->
    erlang:list_to_binary(erlang:ref_to_list(Value));
get_safe_value(Value) when erlang:is_function(Value) ->
    erlang:list_to_binary(erlang:fun_to_list(Value));
get_safe_value(Value) when not erlang:is_list(Value) ->
    erlang:list_to_binary(io_lib:format("~p", [Value]));
get_safe_value(Value) when erlang:is_list(Value) ->
    case io_lib:char_list(Value) of
        true -> erlang:list_to_binary(Value);
        false -> erlang:list_to_binary(io_lib:format("~p", [Value]))
    end.
