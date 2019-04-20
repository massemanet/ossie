-module(m3ua_codec_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include("../include/sctp.hrl").
-include("../include/m3ua.hrl").
-include("../include/mtp3.hrl").


% individual message encode/decode tests

-define(MSG_ENC, <<1,0,1,1,0,0,0,168,2,0,0,8,1,171,205,239,0,6,0,8,17,34,51,68,2,16,0,136, 0,1,3,5,0,1,1,1,3,2,8,14,9,1,3,14,25,11,146,6,0,18,4,25,153,150,118,57, 152,11,18,8,0,18,4,25,137,150,146,153,41,90,98,88,72,4,134,18,5,114,107, 26,40,24,6,7,0,17,134,5,1,1,1,160,13,96,11,161,9,6,7,4,0,0,1,0,5,3,108, 52,161,50,2,1,128,2,1,22,48,42,128,7,145,25,153,150,118,57,152,131,1,0, 133,1,1,134,7,145,25,137,150,146,153,41,135,8,155,172,3,185,8,16,174, 125,171,6,3,2,6,192,5,0,0,19,0,8,252,220,18,152>>).
-define(MSG_DEC, {m3ua_msg,1,1,1,164,[{512,<<1,171,205,239>>},{6,<<17,34,51,68>>},{528,{mtp3_msg,2,3,{mtp3_routing_label,14,65793,66309},8,<<9,1,3,14,25,11,146,6,0,18,4,25,153,150,118,57,152,11,18,8,0,18,4,25,137,150,146,153,41,90,98,88,72,4,134,18,5,114,107,26,40,24,6,7,0,17,134,5,1,1,1,160,13,96,11,161,9,6,7,4,0,0,1,0,5,3,108,52,161,50,2,1,128,2,1,22,48,42,128,7,145,25,153,150,118,57,152,131,1,0,133,1,1,134,7,145,25,137,150,146,153,41,135,8,155,172,3,185,8,16,174,125,171,6,3,2,6,192,5,0>>}},{19,<<252,220,18,152>>}]}).

pcap_dir() ->
    filename:join(filename:dirname(filename:dirname(code:which(?MODULE))), pcap).

%% parser test for real-world ISUP data
pcap_parse_test() ->
        Wildcard = filename:join(pcap_dir(),"*pcap*"),
        Files = filelib:wildcard(Wildcard),
        lists:map(fun pcap_parse_file/1, Files).

pcap_parse_file(File) ->
	Args = [{rewrite_fn, fun pcap_cb/5}],
        {ok, NrPkts} = osmo_ss7_pcap:pcap_apply(File, "", Args),
        io:fwrite(user, "parsed ~p - ~s~n", [NrPkts, filename:basename(File)]),
        {filename:basename(File), NrPkts}.

pcap_cb(sctp, _From, _Path, ?SCTP_PPI_M3UA, DataBin) ->
	%?debugFmt("PCAP ~p~n", [DataBin]),
	M3ua = m3ua_codec:parse_m3ua_msg(DataBin),
	NewBin = m3ua_codec:encode_m3ua_msg(M3ua),
	handle_m3ua(M3ua),
	?assertEqual(DataBin, NewBin);
pcap_cb(sctp, _From, _Path, _, _Data) ->
        ok.

handle_m3ua(#m3ua_msg{msg_class = ?M3UA_MSGC_TRANSFER,
                      msg_type = ?M3UA_MSGT_XFR_DATA,
                      payload = OptList}) ->
	Mtp3 = proplists:get_value(?M3UA_IEI_PROTOCOL_DATA, OptList),
	handle_mtp3(Mtp3);
handle_m3ua(#m3ua_msg{}) ->
	ok.

handle_mtp3(#mtp3_msg{service_ind = ?MTP3_SERV_SCCP,
                      payload = Payload}) ->
	{ok, SccpDec} = sccp_codec:parse_sccp_msg(Payload),
	SccpEnc = sccp_codec:encode_sccp_msg(SccpDec),
	{ok, SccpReDec} = sccp_codec:parse_sccp_msg(SccpEnc),
	?assertEqual(SccpDec, SccpReDec);
handle_mtp3(_) ->
	ok.
