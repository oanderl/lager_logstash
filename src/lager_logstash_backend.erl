-module(lager_logstash_backend).

-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    terminate/1
]).

-record(state, {level, socket, destination, formatter}).

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
        true -> do_log(Message, State);
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
    Destination = {maps:get(host, Opts), maps:get(port, Opts)},
    Level = lager_util:config_to_mask(maps:get(level, Opts, info)),
    Formatter = maps:get(formatter, Opts, undefined),
    #state{level = Level, destination = Destination, formatter = Formatter}.

-spec init_socket(#state{}) -> #state{}.
init_socket(#state{} = State) ->
    case gen_udp:open(0, [binary, {active, false}]) of
        {ok, Socket} -> {ok, State#state{socket = Socket}};
        {error, Reason} -> {error, Reason}
    end.

-spec do_log(lager_msg:lager_msg(), #state{}) -> {ok, #state{}}.
do_log(Message, #state{socket = Socket, destination = Destination} = State) ->
    Payload = [
        {message, lager_msg:message(Message)},
        {severity, lager_msg:severity(Message)},
        {'@.timestamp', get_timestamp(lager_msg:timestamp(Message))},
        {fields, do_format(lager_msg:metadata(Message), State)}
    ],
    {gen_udp:send(Socket, Destination, jsx:encode(Payload)), State}.

-spec do_format(list(), #state{}) -> map().
do_format(Meta, #state{formatter = undefined}) ->
    lager_logstash:format(Meta);
do_format(Meta, #state{formatter = {Module, Function}}) ->
    erlang:apply(Module, Function, [Meta]).

-spec get_timestamp(erlang:timestamp()) -> binary().
get_timestamp({MegaSecs, Secs, MicroSecs}) ->
    MilliSecs = MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000,
    DateTime = calendar:system_time_to_rfc3339(MilliSecs, [{offset, "Z"}, {unit, millisecond}]),
    erlang:list_to_binary(DateTime).
