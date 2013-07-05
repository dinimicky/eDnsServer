%%% -------------------------------------------------------------------
%%% Author  : ezonghu
%%% Description :
%%%
%%% Created : 2012-8-24
%%% -------------------------------------------------------------------
-module(dns_udp_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("dns_udp.hrl").
-include_lib("kernel/src/inet_dns.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([
		 start_link/1,
		 stop/1
		 ]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3
		
		]).

%% internal function
-export([send_dns_response/3]). 
%% ====================================================================
%% External functions
%% ====================================================================
%%-----------------------------------------------------------------
%% Func: start_link/1
%% Description: Starts the process that keeps track of an UDP 
%%              socket.
%%-----------------------------------------------------------------
start_link(Options) ->
	gen_server:start_link(?MODULE, Options, []).

%%-----------------------------------------------------------------
%% Func: stop/1
%% Description: Stops the process that keeps track of an UDP 
%%              socket.
%%-----------------------------------------------------------------
stop(Pid) ->
    call(Pid, stop).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Options) ->
	case parse_options(Options, #dns_udp{}, []) of
		{ok, UdpRec} ->
			IpOpts = [binary, {reuseaddr, true}, {active, once}|
						  UdpRec#dns_udp.options],
			try gen_udp:open(UdpRec#dns_udp.port, IpOpts) of
				{ok, Socket} ->
					?DEBUG("udp open", []),
					{ok, UdpRec#dns_udp{socket = Socket}};
				{error, Reason} = Error ->
					?WARNING_MSG("udp open failed; Error: ~p~n", [Reason]),
					{stop, Error}
			catch
				X:Y ->
					?WARNING_MSG("udp open failed; Error: [~p:~p]~n", [X, Y])
			end;
		{error, Reason} = Error ->
			?DEBUG("udp open failed; Error: ~p; Options:~p~n", [Reason, Options]),
			{stop, Error}
	end.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(stop, _From, UdpRec) ->
	Reply = do_stop(UdpRec),
	{stop, shutdown, Reply, UdpRec};
handle_call(Request, From, State) ->
	?WARNING_MSG("Unknow Request(~p) receive; Server:~p; From:~p; State:~p~n", [Request, self(), From, State]),
    Reply = {error, {invalid_request, Request}},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, UdpRec) ->
	do_stop(UdpRec),
	{stop, shutdown, UdpRec};
handle_cast(Msg, State) ->
	?WARNING_MSG("received unexpected message(~p); State:~p;", [Msg, State]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'EXIT', _Pid,normal}, UdpRec)->
    {noreply, UdpRec};
handle_info({udp, Socket, Ip, Port, Msg}, #dns_udp{socket = Socket} = UdpRec) ->
	TimeStampWhenRecv = erlang:now(),
	SH = create_send_handle(Socket, Ip, Port),
	_Pid = spawn_link(?MODULE, send_dns_response, [SH, Msg, TimeStampWhenRecv]),
	inet:setopts(Socket, [{active, once}]),
	{noreply, UdpRec};
handle_info(Info, UdpRec) ->
	?WARNING_MSG("received unexpected info(~p); State:~p~n", [Info, UdpRec]),
    {noreply, UdpRec}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Termination function for the generic server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	?DEBUG("State:~p~n udp server terminating, Pid:~p; Reason:~p~n", [State, self(), Reason]),
    ok.


%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	?INFO_MSG("code changed; OldVsn:~p; State:~p; Extra:~p~n", [OldVsn, State, Extra]),
    {ok, State}.

do_stop(#dns_udp{socket = Socket}) ->
	gen_udp:close(Socket).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------
%% Func: create_send_handle
%% Description: Function is used for creating the handle used when 
%%    sending data on the UDP socket
%%-----------------------------------------------------------------
create_send_handle(Socket, {_, _, _, _} = Addr, Port) ->
    do_create_send_handle(Socket, Addr, Port);
create_send_handle(Socket, Addr0, Port) ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    do_create_send_handle(Socket, Addr, Port).

do_create_send_handle(Socket, Addr, Port) ->
    %% If neccessary create snmp counter's
    SH = #send_handle{socket = Socket, addr = Addr, port = Port},
%%     maybe_create_snmp_counters(SH),
    SH.




%%-----------------------------------------------------------------
%% Func: parse_options
%% Description: Function that parses the options sent to the UDP 
%%              module.
%%-----------------------------------------------------------------
parse_options([{Tag, Val} | T], UdpRec, Mand) ->
    Mand2 = Mand -- [Tag],
    case Tag of
	port ->
	    parse_options(T, UdpRec#dns_udp{port = Val}, Mand2);
	udp_options when is_list(Val) ->
	    parse_options(T, UdpRec#dns_udp{options = Val}, Mand2);
    Bad ->
	    {error, {bad_option, Bad}}
    end;
parse_options([], UdpRec, []) ->
    {ok, UdpRec};
parse_options([], _UdpRec, Mand) ->
    {error, {missing_options, Mand}};
parse_options(BadList, _UdpRec, _Mand) ->
    {error, {bad_option_list, BadList}}.


call(Pid, Req) ->
    gen_server:call(Pid, Req).

%% --------------------------------------------------------------------
%%% Query functions
%% --------------------------------------------------------------------
get_dns_domain_type(#dns_query{domain = Domain, type = Type,class = in} = _Query)-> 
    {Type, Domain}.

search_list({Type, Domain}) ->
	case ets:lookup(Type, Domain) of
		[] -> {error, {Type, not_found}};
		[{Domain, Result}] ->
			Answers = 
				case Type of	  
					a -> [#dns_rr{domain = Domain, type = a, class = in, data = IP}|| IP <- Result];
					srv -> [#dns_rr{domain = Domain, type = srv, class = in, data = {0, 100, Port, NewDomain}}|| {NewDomain, Port} <- Result];
					naptr -> [#dns_rr{domain = Domain, type = naptr, class = in, data = {100, 10, "u", "sip+e2u", NewDomain, "."}}|| NewDomain <- Result]	  
				end,
			{ok, {Type, Answers}}
	end.
							  
send_dns_response(#send_handle{socket = Sock, addr = IP, port = Port} = _SH, Request, TimeStamp)->
	{ok, #dns_rec{header=Header, qdlist=QList}} = inet_dns:decode(Request),
	Response = 
		case QList of
			[] ->
				common_stats:inc(?DNS_STATS_TAB, format_error),
				inet_dns:encode(#dns_rec{header = Header#dns_header{rcode = 1}, qdlist=QList});
			[Query] ->
				case search_list(get_dns_domain_type(Query)) of 
					{ok, {Type, Answers}} ->
						common_stats:inc(?DNS_STATS_TAB, Type),
						common_stats:inc(?DNS_STATS_TAB, {IP, ok}, Type),
						inet_dns:encode(#dns_rec{header=Header#dns_header{rcode=0}, qdlist=QList, anlist=Answers});
					{error, {Type, _Reason}} ->
						common_stats:inc(?DNS_STATS_TAB, name_error),
						common_stats:inc(?DNS_STATS_TAB, {IP, name_error}, Type),
						inet_dns:encode(#dns_rec{header=Header#dns_header{rcode=3}, qdlist=QList})
				end;
			_MultiQueries ->
				common_stats:inc(?DNS_STATS_TAB, not_implemented),
				inet_dns:encode(#dns_rec{header=Header#dns_header{rcode=4}, qdlist=QList})
		end,
	gen_udp:send(Sock, IP, Port, Response),
	Diff = timer:now_diff(erlang:now(), TimeStamp), 
	if 
		Diff >= 1000000 -> 
			?WARNING_MSG("send out message more than 1sec, actual spend ~pmicrosec~n", [Diff]),
			ok;
		true -> ok
	end.