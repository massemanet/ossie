-module(m3ua_codec_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-define(EUNIT_DEBUG_VAL_DEPTH, 100).
-include_lib("eunit/include/eunit.hrl").

-include("../include/sctp.hrl").
-include("../include/m3ua.hrl").
-include("../include/mtp3.hrl").

m3ua_ssnm_duna_test() ->
    Bin = <<16#01,16#00,16#02,16#01,16#00,16#00,16#00,16#10,16#00,16#12,16#00,16#08,16#00,16#00,16#05,16#04>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_m3ua(Bin).

m3ua_ssnm_dava_test() ->
    Bin = <<16#01,16#00,16#02,16#02,16#00,16#00,16#00,16#10,16#00,16#12,16#00,16#08,16#00,16#00,16#05,16#04>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_m3ua(Bin).

m3ua_aspup_test() ->
    Bin = <<16#01,16#00,16#03,16#01,16#00,16#00,16#00,16#08>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_m3ua(Bin).

m3ua_aspup_ack_test() ->
    Bin = <<16#01,16#00,16#03,16#04,16#00,16#00,16#00,16#08>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_m3ua(Bin).

m3ua_ntfy_test() ->
    Bin = <<16#01,16#00,16#00,16#01,16#00,16#00,16#00,16#10,16#00,16#0d,16#00,16#08,16#00,16#01,16#00,16#02>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_m3ua(Bin).

m3ua_aspac_test() ->
    Bin = <<16#01,16#00,16#04,16#01,16#00,16#00,16#00,16#08>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_m3ua(Bin).

m3ua_aspac_ack_test() ->
    Bin = <<16#01,16#00,16#04,16#03,16#00,16#00,16#00,16#10,16#00,16#0b,16#00,16#08,16#00,16#00,16#00,16#02>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_m3ua(Bin).

m3ua_tcap_empty_begin_test() ->
    Bin = <<16#01,16#00,16#01,16#01,16#00,16#00,16#00,16#60,16#02,16#10,16#00,
            16#58,16#00,16#00,16#05,16#04,16#00,16#00,16#35,16#a7,16#03,16#03,
            16#00,16#08,16#11,16#80,16#0f,16#39,16#03,16#0e,16#00,16#0b,16#12,
            16#08,16#00,16#11,16#04,16#64,16#77,16#77,16#77,16#77,16#07,16#28,
            16#62,16#26,16#48,16#04,16#72,16#10,16#01,16#b3,16#6b,16#1e,16#28,
            16#1c,16#06,16#07,16#00,16#11,16#86,16#05,16#01,16#01,16#01,16#a0,
            16#11,16#60,16#0f,16#80,16#02,16#07,16#80,16#a1,16#09,16#06,16#07,
            16#04,16#00,16#00,16#01,16#00,16#15,16#03,16#0b,16#12,16#08,16#00,
            16#11,16#04,16#64,16#77,16#77,16#77,16#77,16#07>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_m3ua(Bin).

m3ua_tcap_empty_continue_test() ->
    Bin = <<16#01,16#00,16#01,16#01,16#00,16#00,16#00,16#6c,16#02,16#10,16#00,
            16#64,16#00,16#00,16#35,16#a7,16#00,16#00,16#05,16#04,16#03,16#03,
            16#00,16#c4,16#09,16#81,16#03,16#0e,16#19,16#0b,16#12,16#08,16#00,
            16#11,16#04,16#64,16#77,16#77,16#77,16#77,16#07,16#0b,16#12,16#08,
            16#00,16#11,16#04,16#64,16#77,16#77,16#77,16#77,16#07,16#36,16#65,
            16#34,16#48,16#04,16#5f,16#f3,16#71,16#68,16#49,16#04,16#72,16#10,
            16#01,16#b3,16#6b,16#26,16#28,16#24,16#06,16#07,16#00,16#11,16#86,
            16#05,16#01,16#01,16#01,16#a0,16#19,16#61,16#17,16#a1,16#09,16#06,
            16#07,16#04,16#00,16#00,16#01,16#00,16#15,16#03,16#a2,16#03,16#02,
            16#01,16#00,16#a3,16#05,16#a1,16#03,16#02,16#01,16#00>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_m3ua(Bin).

m3ua_tcap_abort_test() ->
    Bin = <<16#01,16#00,16#01,16#01,16#00,16#00,16#00,16#44,16#02,16#10,16#00,
            16#39,16#00,16#00,16#35,16#a7,16#00,16#00,16#05,16#04,16#03,16#03,
            16#00,16#ee,16#09,16#01,16#03,16#0e,16#19,16#0b,16#12,16#08,16#00,
            16#11,16#04,16#64,16#07,16#77,16#77,16#77,16#77,16#0b,16#12,16#08,
            16#00,16#11,16#04,16#64,16#77,16#77,16#77,16#77,16#77,16#0b,16#67,
            16#09,16#49,16#04,16#23,16#03,16#7b,16#22,16#4a,16#01,16#01,16#00,
            16#00,16#00>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_m3ua(Bin).


parse_m3ua(DataBin) ->
    M3ua = ossie_m3ua_codec:parse_m3ua_msg(DataBin),
    ?debugVal(M3ua),
    NewBin = ossie_m3ua_codec:encode_m3ua_msg(M3ua),
    handle_m3ua(M3ua),
    ?assertEqual(DataBin, NewBin).

handle_m3ua(#m3ua_msg{msg_class = ?M3UA_MSGC_TRANSFER,
                      msg_type = ?M3UA_MSGT_XFR_DATA,
                      payload = OptList}) ->
    Mtp3 = proplists:get_value(m3ua_iei_protocol_data, OptList),
    handle_mtp3(Mtp3);
handle_m3ua(#m3ua_msg{}) ->
    ok.

handle_mtp3(#mtp3_msg{service_ind = ?MTP3_SERV_SCCP,
                      payload = Payload}) ->
    {ok, SccpDec} = ossie_sccp_codec:parse_sccp_msg(Payload),
    ?debugVal(SccpDec),
    SccpEnc = ossie_sccp_codec:encode_sccp_msg(SccpDec),
    {ok, SccpReDec} = ossie_sccp_codec:parse_sccp_msg(SccpEnc),
    ?assertEqual(SccpDec, SccpReDec);
handle_mtp3(_) ->
    ok.
