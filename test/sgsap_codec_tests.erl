-module(sgsap_codec_tests).

-define(EUNIT_DEBUG_VAL_DEPTH, 100).
-include_lib("eunit/include/eunit.hrl").

-include("../include/sgsap.hrl").

sgsap_location_update_request_test() ->
    Bin = <<16#09,16#01,16#08,16#09,16#10,16#10,16#00,16#00,16#00,16#00,16#30,16#09,16#37,16#06,16#6d,16#6d,
            16#65,16#63,16#30,16#31,16#09,16#6d,16#6d,16#65,16#67,16#69,16#30,16#30,16#30,16#32,16#03,16#6d,
            16#6d,16#65,16#03,16#65,16#70,16#63,16#06,16#6d,16#6e,16#63,16#30,16#30,16#31,16#06,16#6d,16#63,
            16#63,16#30,16#30,16#31,16#0b,16#33,16#67,16#70,16#70,16#6e,16#65,16#74,16#77,16#6f,16#72,16#6b,
            16#03,16#6f,16#72,16#67,16#0a,16#01,16#01,16#04,16#05,16#00,16#f1,16#10,16#00,16#07>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_sgsap(Bin).

sgsap_location_update_accept_test() ->
    Bin = <<16#0a,16#01,16#08,16#09,16#10,16#10,16#00,16#00,16#00,16#00,16#30,16#04,16#05,16#00,16#f1,16#10,
            16#00,16#07,16#0e,16#05,16#f4,16#02,16#e1,16#02,16#89>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_sgsap(Bin).

sgsap_tmsi_reallocation_complete_test() ->
    Bin = <<16#0c,16#01,16#08,16#09,16#10,16#10,16#00,16#00,16#00,16#00,16#30>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_sgsap(Bin).

sgsap_mm_information_request_test() ->
    Bin = <<16#1a,16#01,16#08,16#09,16#10,16#10,16#00,16#00,16#00,16#00,16#30,16#17,16#1c,16#43,16#08,16#87,
            16#cf,16#79,16#fb,16#dd,16#9c,16#0e,16#01,16#45,16#08,16#87,16#cf,16#79,16#fb,16#dd,16#9c,16#0e,
            16#01,16#47,16#02,16#50,16#01,16#00,16#80,16#31,16#00>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_sgsap(Bin).

sgsap_paging_request_test() ->
    Bin = <<16#01,16#01,16#08,16#09,16#10,16#10,16#00,16#00,16#00,16#00,16#30,16#02,16#29,16#03,16#76,16#6c,
            16#72,16#06,16#6d,16#73,16#63,16#30,16#30,16#31,16#06,16#6d,16#6e,16#63,16#30,16#30,16#31,16#06,
            16#6d,16#63,16#63,16#30,16#30,16#31,16#0b,16#33,16#67,16#70,16#70,16#6e,16#65,16#74,16#77,16#6f,
            16#72,16#6b,16#03,16#6f,16#72,16#67,16#20,16#01,16#01,16#04,16#05,16#00,16#f1,16#10,16#00,16#07>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_sgsap(Bin).

sgsap_service_request_test() ->
    Bin = <<16#06,16#01,16#08,16#09,16#10,16#10,16#00,16#00,16#00,16#00,16#30,16#20,16#01,16#01,16#25,16#01,
            16#01>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_sgsap(Bin).

sgsap_service_abort_request_test() ->
    Bin = <<16#17,16#01,16#08,16#09,16#10,16#10,16#00,16#00,16#00,16#00,16#30>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_sgsap(Bin).

sgsap_ue_unreachable_test() ->
    Bin = <<16#1f,16#01,16#08,16#09,16#10,16#10,16#00,16#00,16#00,16#00,16#40,16#08,16#01,16#06>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_sgsap(Bin).

sgsap_mo_csfb_indication_test() ->
    Bin = <<16#18,16#01,16#08,16#09,16#10,16#10,16#00,16#00,16#00,16#00,16#40>>,
    ?debugMsg(?FUNCTION_NAME),
    parse_sgsap(Bin).


parse_sgsap(DataBin) ->
    SGsAP = ossie_sgsap_codec:parse_sgsap_msg(DataBin),
    ?debugVal(SGsAP)%% ,
    %% NewBin = ossie_sgsap_codec:encode_sgsap_msg(SGsAP),
    %% ?assertEqual(DataBin, NewBin)
        .
