-define(DNS_Glob_Cnt, [a, srv, naptr, not_implemented, format_error, name_error]).
-define(DNS_STATS_TAB, dns_udp_stats).
-record(send_handle, {socket, addr, port}).
-record(dns_udp, {
				  port = 53,
				  options =[],
				  socket,
				  module,
				  serialize = false  % false: Spawn a new process for each message
				  }).

-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
    ejabberd_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
    ejabberd_logger:info_msg(?MODULE,?LINE,Format, Args)).

-define(WARNING_MSG(Format, Args),
    ejabberd_logger:warning_msg(?MODULE,?LINE,Format, Args)).

-define(ERROR_MSG(Format, Args),
    ejabberd_logger:error_msg(?MODULE,?LINE,Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    ejabberd_logger:critical_msg(?MODULE,?LINE,Format, Args)).

-define(ALIST, [
                {"barna.carrtb.telefonica.net",[{11,10,1,11}]},
                {"icscf1.iv.ims.cbc",[{192,168,1,100}, {192,168,1,101}]},
                {"icscf2.iv.ims.cbc",[{192,168,55,242}, {192,168,55,243}]},
                {"ims.vodafone.pt",[{10,170,1,10}, {10,170,1,11}]}
               ]).
-define(SRVLIST, [
                  {"_sip._udp.barna.carrtb.telefonica.net",[{"barna.carrtb.telefonica.net",5060}]}, 
                  {"_sip._udp.cisco4.iv.ims.cbc",[{"icscf2.iv.ims.cbc",5120}]},
                  {"_sip._udp.icscf.iv.ims.cbc", [{"icscf1.iv.ims.cbc", 5060},{"icscf2.iv.ims.cbc", 5060}]},
                  {"_sip._udp.920.iv.ims.cbc", [{"920.iv.ims.cbc", 5061}]}
                 ]).
-define(NAPTRLIST, [{"4.3.2.1.0.2.1.5.2.4.2.8.8.8.0.0.e164.arpa",["!^.*$!sip:8882425120123456@cisco4.iv.ims.cbc!"]},
                    {"5.4.3.2.1.0.2.2.2.4.4.4.e164.arpa",["!^.*$!sip:444222012345@cisco4.iv.ims.cbc!"]},
                    {"0.9.8.7.6.5.e164.arpa",["!^.*$!sip:567890@icscf.iv.ims.cbc!"]},
                    {"5.4.3.2.1.e164.arpa", ["!^.*$!sip:12345@cisco4.iv.ims.cbc!"]}
                   ]).
-define(DNS_DB_List, [{a, ?ALIST}, {srv, ?SRVLIST}, {naptr, ?NAPTRLIST}]).