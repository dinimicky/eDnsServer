%% Author: ezonghu
%% Created: 2012-8-24
%% Description: TODO: Add description to dns_udp
-module(dns_udp).

%%=================================================================
%% Include files
%%=================================================================
-include("dns_udp.hrl").


%%=================================================================
%% Exported Functions
%%=================================================================
-export([
		 start_transport/1,
		 start_transport/3,
		 stop_transport/1

		 ]).

%%=================================================================
%% API Functions
%%=================================================================
%%-----------------------------------------------------------------
%% Func: start_transport
%% Description: Starts the UDP transport service
%%-----------------------------------------------------------------
start_transport(Loglevel)->
	start_transport(?DNS_DB_List, [], Loglevel).
start_transport(DNS_DB_List, Options, LogLevel) ->
	error_logger:add_report_handler(ejabberd_logger_h, "./execution.log"),
	ejabberd_loglevel:set(LogLevel),
	set_all_dns_tables(DNS_DB_List),
	try common_stats:init(?DNS_STATS_TAB, ?DNS_Glob_Cnt)
	catch
		X:Y -> 
			?WARNING_MSG("receive error info: [~p:~p]", [X, Y])
	end,
	dns_udp_sup:start_link(Options).

%%-----------------------------------------------------------------
%% Func: stop_transport
%% Description: Stop the UDP transport service
%%-----------------------------------------------------------------
stop_transport(Pid) ->
	error_logger:delete_report_handler(ejabberd_logger_h),
	try unlink(Pid)
	catch
		X:Y -> 
			?WARNING_MSG("receive error info: [~p:~p]", [X, Y])
	after
    	stop_transport(Pid, shutdown)
	end.

stop_transport(Pid, Reason) ->
    exit(Pid, Reason).


%%=================================================================
%% Local Functions
%%=================================================================
%%-----------------------------------------------------------------
%% Func: list2ets/2
%% Description: Function is used for storing the list into
%%              some ets table
%%-----------------------------------------------------------------
list2ets(TableId, [H|T]) ->
%%    io:format("Insert Table [~p]:[~p]~n", [TableId, H]),
    ets:insert(TableId, H),
    list2ets(TableId, T);
list2ets(_TableId, []) ->
    finish.
%%-----------------------------------------------------------------
%% Func: set_all_dns_tables/1
%% Description: Function is used for create different dns type  
%%              ets table
%%-----------------------------------------------------------------
set_all_dns_tables([{Type, List}|Rest]) ->
    ets:new(Type, [set, named_table]),
    finish = list2ets(Type, List),
	set_all_dns_tables(Rest);
set_all_dns_tables([]) ->
    ok.



