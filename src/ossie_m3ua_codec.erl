%% M3UA in accordance with RFC4666 (http://tools.ietf.org/html/rfc4666)

%% (C) 2011 by Harald Welte <laforge@gnumonks.org>
%%
%% All Rights Reserved
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as
%% published by the Free Software Foundation; either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% Additional Permission under GNU AGPL version 3 section 7:
%%
%% If you modify this Program, or any covered work, by linking or
%% combining it with runtime libraries of Erlang/OTP as released by
%% Ericsson on http://www.erlang.org (or a modified version of these
%% libraries), containing parts covered by the terms of the Erlang Public
%% License (http://www.erlang.org/EPLICENSE), the licensors of this
%% Program grant you additional permission to convey the resulting work
%% without the need to license the runtime libraries of Erlang/OTP under
%% the GNU Affero General Public License. Corresponding Source for a
%% non-source form of such a combination shall include the source code
%% for the parts of the runtime libraries of Erlang/OTP used as well as
%% that of the covered work.

-module(ossie_m3ua_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("../include/m3ua.hrl").
-include("../include/mtp3.hrl").

-export([parse_m3ua_msg/1, encode_m3ua_msg/1]).

%% compute the number of pad bits required after a binary parameter
get_num_pad_bytes(BinLenBytes) ->
    case BinLenBytes rem 4 of
        0 ->    0;
        Val ->  4 - Val
    end.

parse_m3ua_msg(DataBin) when is_binary(DataBin) ->
    <<Version:8, _Reserved:8, MsgClass:8, MsgType:8, MsgLen:32/big, Remain/binary>> = DataBin,
    OptList = parse_m3ua_opts(Remain),
    #m3ua_msg{version = Version, msg_class = MsgClass, msg_type = MsgType,
              msg_length = MsgLen-4, payload = OptList};
parse_m3ua_msg(Data) when is_list(Data) ->
    parse_m3ua_msg(list_to_binary(Data)).

parse_m3ua_opts(OptBin) when is_binary(OptBin) ->
    parse_m3ua_opts(OptBin, []).

parse_m3ua_opts(<<>>, OptList) when is_list(OptList) ->
    OptList;
parse_m3ua_opts(OptBin, OptList) when is_binary(OptBin), is_list(OptList) ->
    <<IEI:16/big, Length:16/big, Remain/binary>> = OptBin,
    PadLen = get_num_pad_bytes(Length),
    LengthNet = Length - 4,
    <<CurOpt:LengthNet/binary, 0:PadLen/integer-unit:8, Remain2/binary>> = Remain,
    NewOpt = parse_m3ua_opt(dec_iei(IEI), CurOpt),
    parse_m3ua_opts(Remain2, OptList ++ [NewOpt]).

parse_m3ua_opt(Opt = m3ua_iei_protocol_data, MsgBin) when is_binary(MsgBin) ->
    <<Opc:32/big, Dpc:32/big, Si:8, Ni:8, Mp:8, Sls:8, Payload/binary>> = MsgBin,
    %% The idea is to hand back a #mtp3_msg{} to make upper layers beyond
    %% MTP-TRANSFR.{ind,req} unaware of a MTP3 or M3UA lower layer
    {Opt, #mtp3_msg{network_ind = Ni, service_ind = Si,
                    routing_label = #mtp3_routing_label{sig_link_sel = Sls,
                                                        origin_pc = Opc,
                                                        dest_pc = Dpc},
                    payload = Payload, m3ua_mp = Mp}};
parse_m3ua_opt(Opt, Msg) ->
    {Opt, Msg}.

encode_m3ua_msg(#m3ua_msg{version = Version, msg_class = MsgClass,
                          msg_type = MsgType, payload = OptList}) ->
    OptBin = encode_m3ua_opts(OptList),
    MsgLen = byte_size(OptBin) + 8,
    <<Version:8, 0:8, MsgClass:8, MsgType:8, MsgLen:32/big, OptBin/binary>>.

encode_m3ua_opts(OptList) when is_list(OptList) ->
    encode_m3ua_opts(OptList, <<>>).

encode_m3ua_opts([], Bin) ->
    Bin;
encode_m3ua_opts([{Iei, Attr}|Tail], Bin) ->
    OptBin = encode_m3ua_opt(Iei, Attr),
    encode_m3ua_opts(Tail, <<Bin/binary, OptBin/binary>>).

encode_m3ua_opt(m3ua_iei_protocol_data, Mtp3) when is_record(Mtp3, mtp3_msg) ->
    #mtp3_msg{network_ind = Ni, service_ind = Si,
              routing_label = #mtp3_routing_label{sig_link_sel = Sls,
                                                  origin_pc = OpcIn,
                                                  dest_pc = DpcIn},
              payload = Payload, m3ua_mp = Mp} = Mtp3,
    Opc = ossie_util:pointcode2int(OpcIn),
    Dpc = ossie_util:pointcode2int(DpcIn),
    case Mp of
        undefined -> MpD = 0;
        _ -> MpD = Mp
    end,
    PayBin = <<Opc:32/big, Dpc:32/big, Si:8, Ni:8, MpD:8, Sls:8, Payload/binary>>,
    encode_m3ua_opt(m3ua_iei_protocol_data, PayBin);
encode_m3ua_opt(Iei, Data) when is_binary(Data) ->
    Length = byte_size(Data) + 4,
    PadLen = get_num_pad_bytes(Length),
    IEI = enc_iei(Iei),
    <<IEI:16/big, Length:16/big, Data/binary, 0:PadLen/integer-unit:8>>.

dec_iei(?M3UA_IEI_INFO_STRING)    -> m3ua_iei_info_string;
dec_iei(?M3UA_IEI_ROUTE_CTX)      -> m3ua_iei_route_ctx;
dec_iei(?M3UA_IEI_DIAG_INFO)      -> m3ua_iei_diag_info;
dec_iei(?M3UA_IEI_HEARTB_DATA)    -> m3ua_iei_heartb_data;
dec_iei(?M3UA_IEI_TRAF_MODE_TYPE) -> m3ua_iei_traf_mode_type;
dec_iei(?M3UA_IEI_ERR_CODE)       -> m3ua_iei_err_code;
dec_iei(?M3UA_IEI_STATUS)         -> m3ua_iei_status;
dec_iei(?M3UA_IEI_ASP_ID)         -> m3ua_iei_asp_id;
dec_iei(?M3UA_IEI_AFFECTED_PC)    -> m3ua_iei_affected_pc;
dec_iei(?M3UA_IEI_CORR_ID)        -> m3ua_iei_corr_id;
dec_iei(?M3UA_IEI_NET_APPEARANCE) -> m3ua_iei_net_appearance;
dec_iei(?M3UA_IEI_USER_CAUSE)     -> m3ua_iei_user_cause;
dec_iei(?M3UA_IEI_CONGESTION_IND) -> m3ua_iei_congestion_ind;
dec_iei(?M3UA_IEI_CONCERNED_IND)  -> m3ua_iei_concerned_ind;
dec_iei(?M3UA_IEI_ROUTING_KEY)    -> m3ua_iei_routing_key;
dec_iei(?M3UA_IEI_REG_RESULT)     -> m3ua_iei_reg_result;
dec_iei(?M3UA_IEI_DEREG_RESULT)   -> m3ua_iei_dereg_result;
dec_iei(?M3UA_IEI_LOCAL_RKEY_ID)  -> m3ua_iei_local_rkey_id;
dec_iei(?M3UA_IEI_DEST_PC)        -> m3ua_iei_dest_pc;
dec_iei(?M3UA_IEI_SERVICE_IND)    -> m3ua_iei_service_ind;
dec_iei(?M3UA_IEI_ORIG_PC_LIST)   -> m3ua_iei_orig_pc_list;
dec_iei(?M3UA_IEI_PROTOCOL_DATA)  -> m3ua_iei_protocol_data;
dec_iei(?M3UA_IEI_REG_STATUS)     -> m3ua_iei_reg_status;
dec_iei(?M3UA_IEI_DEREG_STATUS)   -> m3ua_iei_dereg_status.

enc_iei(m3ua_iei_info_string)    -> ?M3UA_IEI_INFO_STRING;
enc_iei(m3ua_iei_route_ctx)      -> ?M3UA_IEI_ROUTE_CTX;
enc_iei(m3ua_iei_diag_info)      -> ?M3UA_IEI_DIAG_INFO;
enc_iei(m3ua_iei_heartb_data)    -> ?M3UA_IEI_HEARTB_DATA;
enc_iei(m3ua_iei_traf_mode_type) -> ?M3UA_IEI_TRAF_MODE_TYPE;
enc_iei(m3ua_iei_err_code)       -> ?M3UA_IEI_ERR_CODE;
enc_iei(m3ua_iei_status)         -> ?M3UA_IEI_STATUS;
enc_iei(m3ua_iei_asp_id)         -> ?M3UA_IEI_ASP_ID;
enc_iei(m3ua_iei_affected_pc)    -> ?M3UA_IEI_AFFECTED_PC;
enc_iei(m3ua_iei_corr_id)        -> ?M3UA_IEI_CORR_ID;
enc_iei(m3ua_iei_net_appearance) -> ?M3UA_IEI_NET_APPEARANCE;
enc_iei(m3ua_iei_user_cause)     -> ?M3UA_IEI_USER_CAUSE;
enc_iei(m3ua_iei_congestion_ind) -> ?M3UA_IEI_CONGESTION_IND;
enc_iei(m3ua_iei_concerned_ind)  -> ?M3UA_IEI_CONCERNED_IND;
enc_iei(m3ua_iei_routing_key)    -> ?M3UA_IEI_ROUTING_KEY;
enc_iei(m3ua_iei_reg_result)     -> ?M3UA_IEI_REG_RESULT;
enc_iei(m3ua_iei_dereg_result)   -> ?M3UA_IEI_DEREG_RESULT;
enc_iei(m3ua_iei_local_rkey_id)  -> ?M3UA_IEI_LOCAL_RKEY_ID;
enc_iei(m3ua_iei_dest_pc)        -> ?M3UA_IEI_DEST_PC;
enc_iei(m3ua_iei_service_ind)    -> ?M3UA_IEI_SERVICE_IND;
enc_iei(m3ua_iei_orig_pc_list)   -> ?M3UA_IEI_ORIG_PC_LIST;
enc_iei(m3ua_iei_protocol_data)  -> ?M3UA_IEI_PROTOCOL_DATA;
enc_iei(m3ua_iei_reg_status)     -> ?M3UA_IEI_REG_STATUS;
enc_iei(m3ua_iei_dereg_status)   -> ?M3UA_IEI_DEREG_STATUS.
