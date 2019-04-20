-module(isup_codec_tests).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("eunit/include/eunit.hrl").

-include("../include/sctp.hrl").
-include("../include/isup.hrl").
-include("../include/xua.hrl").
-include("../include/m2ua.hrl").
-include("../include/mtp3.hrl").


                                                % individual message encode/decode tests

-define(ISUP_GRS_BIN, <<1,0,23,1,1,14>>).
-define(ISUP_GRS_DEC, #isup_msg{msg_type = 23,cic = 1, parameters = [{22,{1,<<14>>}}]}).

grs_dec_test() ->
    ?assertEqual(?ISUP_GRS_DEC, ossie_isup_codec:parse_isup_msg(?ISUP_GRS_BIN)).
grs_enc_test() ->
    ?assertEqual(?ISUP_GRS_BIN, ossie_isup_codec:encode_isup_msg(?ISUP_GRS_DEC)).

-define(ISUP_IAM_BIN, <<9,0,1,16,72,0,10,3,2,10,8,131,16,41,153,36,0,128,15,10,8,3,19,148,3,
                        66,48,147,32,242,21,54,25,8,0,0,21,255,255,255,255,255,255,255,255,
                        255,255,29,69,56,203,32,0>>).

-define(ISUP_IAM_DEC, {isup_msg,1,9,[{conn_ind_nature,16},{fw_call_ind,18432},{calling_cat,10},{transm_medium_req,3},{4,{party_number,3,0,undefined,1,undefined,undefined,[9,2,9,9,4,2,0,0,0,8,15]}},{10,{party_number,3,undefined,0,1,0,3,[4,9,3,0,2,4,0,3,3,9,0,2]}},{242,{21,<<54,25,8,0,0,21,255,255,255,255,255,255,255,255,255,255,29,69,56,203,32>>}}]}).

iam_dec_test() ->
    ?assertEqual(?ISUP_IAM_DEC, ossie_isup_codec:parse_isup_msg(?ISUP_IAM_BIN)).
iam_enc_test() ->
    ?assertEqual(?ISUP_IAM_BIN, ossie_isup_codec:encode_isup_msg(?ISUP_IAM_DEC)).


pcap_dir() ->
    filename:join(filename:dirname(filename:dirname(code:which(?MODULE))), pcap).

%% parser test for real-world ISUP data
pcap_parse_test() ->
    Wildcard = filename:join(pcap_dir(), "*isup*pcap*"),
    Files = filelib:wildcard(Wildcard),
    lists:map(fun pcap_parse_file/1, Files).

pcap_parse_file(File) ->
    Args = [{rewrite_fn, fun pcap_cb/5}],
    {ok, NrPkts} = ossie_pcap:pcap_apply(File, "", Args),
    io:fwrite(user, "parsed ~p - ~s~n", [NrPkts, filename:basename(File)]),
    {filename:basename(File), NrPkts}.

pcap_cb(sctp, _From, _Path, ?SCTP_PPI_M2UA, DataBin) ->
    M2ua = ossie_m2ua_codec:parse_m2ua_msg(DataBin),
    handle_m2ua(M2ua);
pcap_cb(sctp, _From, _Path, _, _Data) ->
    ok.

handle_m2ua(#xua_msg{msg_class = ?M2UA_MSGC_MAUP,
                     msg_type = ?M2UA_MAUP_MSGT_DATA,
                     payload = Params}) ->
    {_Len, M2uaPayload} = proplists:get_value(16#300, Params),
    Mtp3 = ossie_mtp3_codec:parse_mtp3_msg(M2uaPayload),
    handle_mtp3(Mtp3);
handle_m2ua(#m2ua_msg{}) ->
    ok.

handle_mtp3(#mtp3_msg{service_ind = ?MTP3_SERV_ISUP,
                      payload = Payload}) ->
    #isup_msg{} = IsupDec = osie_isup_codec:parse_isup_msg(Payload),
    IsupEnc = ossie_isup_codec:encode_isup_msg(IsupDec),
    ?assertEqual(Payload, IsupEnc);
handle_mtp3(#mtp3_msg{}) ->
    ok.
