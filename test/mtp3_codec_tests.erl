-module(mtp3_codec_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include("../include/mtp3.hrl").

-define(MTP3_MSG_BIN, <<131,92,64,0,192,9,0,3,13,24,10,18,7,0,18,4,83,132,9,0,23,11,18,6,0,18,4,68,119,88,16,70,35,67,100,65,73,4,81,1,2,200,107,42,40,40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,5,161,3,2,1,0,108,13,163,11,2,1,64,2,1,8,48,3,10,1,0>>).
-define(MTP3_MSG_DEC, {mtp3_msg,2,mtp3_serv_sccp,{mtp3_routing_label,12,1,92},undefined,<<9,0,3,13,24,10,18,7,0,18,4,83,132,9,0,23,11,18,6,0,18,4,68,119,88,16,70,35,67,100,65,73,4,81,1,2,200,107,42,40,40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,1,3,162,3,2,1,0,163,5,161,3,2,1,0,108,13,163,11,2,1,64,2,1,8,48,3,10,1,0>>}).

parse_test() ->
	?assertEqual(?MTP3_MSG_DEC, ossie_mtp3_codec:parse_mtp3_msg(?MTP3_MSG_BIN)).
encode_test() ->
	?assertEqual(?MTP3_MSG_BIN, ossie_mtp3_codec:encode_mtp3_msg(?MTP3_MSG_DEC)).
