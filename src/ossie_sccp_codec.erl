%% ITU-T Q.71x SCCP Message coding / decoding

%% (C) 2010 by Harald Welte <laforge@gnumonks.org>
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

-module(ossie_sccp_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("../include/sccp.hrl").

-export([parse_sccp_msg/1, encode_sccp_msg/1, encode_sccp_msgt/2]).

-export([gen_gt_helper/1, gen_addr_helper/2, gen_addr_helper/3]).

binarify(In) when is_binary(In) ->
    In;
binarify(In) when is_list(In) ->
    list_to_binary(In).

parse_point_code(BinPC, PCind) when is_binary(BinPC) ->
    case PCind of
        1 ->
            <<PointCode:16/little, Remain/binary>> = BinPC;
        _ ->
            Remain = BinPC,
            PointCode = undefined
    end,
    {Remain, PointCode}.

parse_ssn(BinSSN, SSNind) ->
    case SSNind of
        1 ->
            <<SSN:8, Remain/binary>> = BinSSN;
        _ ->
            Remain = BinSSN,
            SSN = undefined
    end,
    {Remain, SSN}.

enc_is_odd(Enc) ->
    case Enc of
        1 -> 1;
        _ -> 0
    end.

parse_gt(BinGT, GTind) ->
    case GTind of
        ?SCCP_GTI_NO_GT ->
            undefined;
        ?SCCP_GTI_NAT_ONLY ->
            %% Figure 7/Q.713
            <<Odd:1, Nature:7, Digits/binary>> = BinGT,
            PhoneNum = ossie_isup_codec:parse_isup_party(Digits, Odd),
            #global_title{gti = GTind,
                          encoding = undefined,
                          numbering_plan = undefined,
                          nature_of_addr_ind = Nature,
                          phone_number = PhoneNum};
        ?SCCP_GTI_TT_ONLY ->
            %% Figure 9/Q.913
            <<TransType:8, Digits/binary>> = BinGT,
            %% Used in national interfaces only, we cannot parse Digits
            #global_title{gti = GTind,
                          trans_type = TransType,
                          encoding = undefined,
                          numbering_plan = undefined,
                          nature_of_addr_ind = undefined,
                          phone_number = Digits};
        ?SCCP_GTI_TT_NP_ENC ->
            %% Figure 10/Q.713
            <<TransType:8, NumPlan:4, Enc:4, Digits/binary>> = BinGT,
            PhoneNum = ossie_isup_codec:parse_isup_party(Digits, enc_is_odd(Enc)),
            #global_title{gti = GTind,
                          encoding = undefined,
                          nature_of_addr_ind = undefined,
                          trans_type = TransType,
                          numbering_plan = NumPlan,
                          phone_number = PhoneNum};
        ?SCCP_GTI_TT_NP_ENC_NAT ->
            %% Figure 11/Q.713
            <<TransType:8, NumPlan:4, Enc:4, 0:1, Nature:7, Digits/binary>> = BinGT,
            PhoneNum = ossie_isup_codec:parse_isup_party(Digits, enc_is_odd(Enc)),
            #global_title{gti = GTind,
                          encoding = undefined,
                          trans_type = TransType,
                          numbering_plan = NumPlan,
                          nature_of_addr_ind = Nature,
                          phone_number = PhoneNum};
        _ ->
            BinGT
    end.

%% parse SCCP Address
parse_sccp_addr(BinAddr) when is_binary(BinAddr) ->
    <<ResNatUse:1, RoutInd:1, GTind:4, SSNind:1, PCind:1, Remain/binary>> = BinAddr,
    {RemainPC, OptPC} = parse_point_code(Remain, PCind),
    {RemainSSN, OptSSN} = parse_ssn(RemainPC, SSNind),
    OptGT = parse_gt(RemainSSN, GTind),
    #sccp_addr{res_nat_use = ResNatUse, route_on_ssn = RoutInd,
               point_code = OptPC, ssn = OptSSN, global_title = OptGT}.

%% parse SCCP Optional Part
parse_sccp_opt(OptType, _OptLen, Content) ->
    OptAtom = opt_to_atom(OptType),
    {OptAtom, Content}.

parse_sccp_opts(<<>>, OptList) ->
    %% empty list
    OptList;
parse_sccp_opts(<<0>>, OptList) ->
    %% end of options
    OptList;
parse_sccp_opts(OptBin, OptList) ->
    <<OptType, OptLen, Content:OptLen/binary, Remain/binary>> = OptBin,
    NewOpt = parse_sccp_opt(OptType, OptLen, Content),
    parse_sccp_opts(Remain, [NewOpt|OptList]).


%% Parse incoming SCCP message, one function for every message type
parse_sccp_msgt(sccp_msgt_cr, DataBin) ->
    %% first get the fixed part
    <<_:8, SrcLocalRef:24/big, PCOpt:4, ProtoClass:4, RemainVar/binary >> = DataBin,
    %% variable length fixed part
    <<PtrVar:8, PtrOpt:8, _/binary>> = RemainVar,
    CalledPartyLen = binary:at(RemainVar, PtrVar),
    CalledParty = binary:part(RemainVar, PtrVar+1, CalledPartyLen),
    CalledPartyDec = parse_sccp_addr(CalledParty),
    %% optional part
    OptBin = binary:part(RemainVar, 1 + PtrOpt, byte_size(RemainVar)-(1+PtrOpt)),
    OptList = parse_sccp_opts(OptBin, []),
    %% build parsed list of message
    #sccp_msg_params_cr{src_local_ref = SrcLocalRef,
                        protocol_class = dec_protocol_class_and_options({ProtoClass, PCOpt}),
                        called_party_addr = CalledPartyDec,
                        credit = proplists:get_value(credit, OptList),
                        calling_party_addr = proplists:get_value(calling_party_addr, OptList),
                        data = proplists:get_value(data, OptList),
                        hop_counter = proplists:get_value(hop_counter, OptList),
                        importance = proplists:get_value(importance, OptList)
                       };
parse_sccp_msgt(sccp_msgt_cc, DataBin) ->
    %% first get the fixed part
    <<_:8, DstLocalRef:24/big, SrcLocalRef:24/big, PCOpt:4, ProtoClass:4, PtrOpt:8, Remain/binary >> = DataBin,
    %% optional part
    OptBin = binary:part(Remain, PtrOpt-1, byte_size(Remain)-(PtrOpt-1)),
    OptList = parse_sccp_opts(OptBin, []),
    %% build parsed list of message
    #sccp_msg_params_cc{dst_local_ref = DstLocalRef,
                        src_local_ref = SrcLocalRef,
                        protocol_class = dec_protocol_class_and_options({ProtoClass, PCOpt}),
                        credit = proplists:get_value(credit, OptList),
                        called_party_addr = proplists:get_value(called_party_addr, OptList),
                        data = proplists:get_value(data, OptList),
                        importance = proplists:get_value(importance,  OptList)
                       };
parse_sccp_msgt(sccp_msgt_cref, DataBin) ->
    %% first get the fixed part
    <<_:8, DstLocalRef:24/big, RefusalCause:8, Remain/binary >> = DataBin,
    %% optional part
    OptList = parse_sccp_opts(Remain, []),
    %% build parsed list of message
    #sccp_msg_params_cref{dst_local_ref = DstLocalRef,
                          refusal_cause = dec_refusal_cause(RefusalCause),
                          called_party_addr = proplists:get_value(called_party_addr, OptList),
                          data = proplists:get_value(data, OptList),
                          importance = proplists:get_value(importance, OptList)
                         };
parse_sccp_msgt(sccp_msgt_rlsd, DataBin) ->
    <<_:8, DstLocalRef:24/big, SrcLocalRef:24/big, ReleaseCause:8, Remain/binary >> = DataBin,
    %% optional part
    OptList = parse_sccp_opts(Remain, []),
    %% build parsed list of message
    #sccp_msg_params_rlsd{dst_local_ref = DstLocalRef,
                          src_local_ref = SrcLocalRef,
                          release_cause = ReleaseCause,
                          data = proplists:get_value(data, OptList),
                          importance = proplists:get_value(importance, OptList)
                         };
parse_sccp_msgt(sccp_msgt_rlc, DataBin) ->
    <<_:8, DstLocalRef:24/big, SrcLocalRef:24/big>> = DataBin,
    %% build parsed list of message
    #sccp_msg_params_rlc{dst_local_ref = DstLocalRef,
                         src_local_ref = SrcLocalRef
                        };
parse_sccp_msgt(sccp_msgt_dt1, DataBin) ->
    <<_:8, DstLocalRef:24/big, SegmReass:8, DataPtr:8, Remain/binary >> = DataBin,
    DataLen = binary:at(Remain, DataPtr-1),
    UserData = binary:part(Remain, DataPtr-1+1, DataLen),
    %% build parsed list of message
    #sccp_msg_params_dt1{dst_local_ref = DstLocalRef,
                         segm_reass = SegmReass,
                         data = UserData
                        };
parse_sccp_msgt(sccp_msgt_dt2, DataBin) ->
    <<_:8, DstLocalRef:24/big, SeqSegm:16, DataPtr:8, Remain/binary >> = DataBin,
    DataLen = binary:at(Remain, DataPtr-1),
    UserData = binary:part(Remain, DataPtr-1+1, DataLen),
    %% build parsed list of message
    #sccp_msg_params_dt2{dst_local_ref = DstLocalRef,
                         seq_segm = SeqSegm,
                         data = UserData
                        };
parse_sccp_msgt(sccp_msgt_ak, DataBin) ->
    <<_:8, DstLocalRef:24/big, RxSeqnr:8, Credit:8>> = DataBin,
    #sccp_msg_params_ak{dst_local_ref = DstLocalRef,
                        rx_seq_nr = RxSeqnr,
                        credit = Credit
                       };
parse_sccp_msgt(sccp_msgt_udt, DataBin) ->
    <<_:8, PCOpt:4, ProtoClass:4, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, Remain/binary >> = DataBin,
    %% variable part
    CalledPartyLen = binary:at(Remain, CalledPartyPtr-3),
    CalledParty = binary:part(Remain, CalledPartyPtr-3+1, CalledPartyLen),
    CalledPartyDec = parse_sccp_addr(CalledParty),
    CallingPartyLen = binary:at(Remain, CallingPartyPtr-2),
    CallingParty = binary:part(Remain, CallingPartyPtr-2+1, CallingPartyLen),
    CallingPartyDec = parse_sccp_addr(CallingParty),
    DataLen = binary:at(Remain, DataPtr-1),
    UserData = binary:part(Remain, DataPtr-1+1, DataLen),
    #sccp_msg_params_udt{protocol_class = dec_protocol_class_and_options({ProtoClass, PCOpt}),
                         called_party_addr = CalledPartyDec,
                         calling_party_addr = CallingPartyDec,
                         data = UserData
                        };
parse_sccp_msgt(sccp_msgt_udts, DataBin) ->
    <<_:8, ReturnCause:8, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, Remain/binary >> = DataBin,
    %% variable part
    CalledPartyLen = binary:at(Remain, CalledPartyPtr-3),
    CalledParty = binary:part(Remain, CalledPartyPtr-3+1, CalledPartyLen),
    CalledPartyDec = parse_sccp_addr(CalledParty),
    CallingPartyLen = binary:at(Remain, CallingPartyPtr-2),
    CallingParty = binary:part(Remain, CallingPartyPtr-2+1, CallingPartyLen),
    CallingPartyDec = parse_sccp_addr(CallingParty),
    DataLen = binary:at(Remain, DataPtr-1),
    UserData = binary:part(Remain, DataPtr-1+1, DataLen),
    #sccp_msg_params_udts{return_cause = dec_return_cause(ReturnCause),
                          called_party_addr = CalledPartyDec,
                          calling_party_addr = CallingPartyDec,
                          data = UserData
                         };
parse_sccp_msgt(sccp_msgt_ed, DataBin) ->
    <<_:8, DstLocalRef:24/big, DataPtr:8, Remain/binary>> = DataBin,
    DataLen = binary:at(Remain, DataPtr-1),
    UserData = binary:part(Remain, DataPtr-1+1, DataLen),
    #sccp_msg_params_ed{dst_local_ref = DstLocalRef,
                        data = UserData
                       };
parse_sccp_msgt(sccp_msgt_ea, DataBin) ->
    <<_:8, DstLocalRef:24/big>> = DataBin,
    #sccp_msg_params_ea{dst_local_ref = DstLocalRef
                       };
parse_sccp_msgt(sccp_msgt_rsr, DataBin) ->
    <<_:8, DstLocalRef:24/big, SrcLocalRef:24/big, ResetCause:8>> = DataBin,
    #sccp_msg_params_rsr{dst_local_ref = DstLocalRef,
                         src_local_ref = SrcLocalRef,
                         reset_cause = dec_reset_cause(ResetCause)
                        };
parse_sccp_msgt(sccp_msgt_rsc, DataBin) ->
    <<_:8, DstLocalRef:24/big, SrcLocalRef:24/big>> = DataBin,
    #sccp_msg_params_rsc{dst_local_ref = DstLocalRef,
                         src_local_ref = SrcLocalRef
                        };
parse_sccp_msgt(sccp_msgt_err, DataBin) ->
    <<_:8, DstLocalRef:24/big, ErrCause:8>> = DataBin,
    #sccp_msg_params_err{dst_local_ref = DstLocalRef,
                         error_cause = dec_error_cause(ErrCause)
                        };
parse_sccp_msgt(sccp_msgt_it, DataBin) ->
    <<_:8, DstLocalRef:24/big, SrcLocalRef:24/big, PCOpt: 4, ProtoClass:4, SegmSeq:16, Credit:8>> = DataBin,
    #sccp_msg_params_it{dst_local_ref = DstLocalRef,
                        src_local_ref = SrcLocalRef,
                        protocol_class = dec_protocol_class_and_options({ProtoClass, PCOpt}),
                        seq_segm = SegmSeq,
                        credit = Credit
                       };
parse_sccp_msgt(sccp_msgt_xudt, DataBin) ->
    <<_:8, PCOpt: 4, ProtoClass:4, HopCounter:8, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, OptPtr:8, Remain/binary>> = DataBin,
    CalledPartyLen = binary:at(Remain, CalledPartyPtr-4),
    CalledParty = binary:part(Remain, CalledPartyPtr-4+1, CalledPartyLen),
    CalledPartyDec = parse_sccp_addr(CalledParty),
    CallingPartyLen = binary:at(Remain, CallingPartyPtr-3),
    CallingParty = binary:part(Remain, CallingPartyPtr-3+1, CallingPartyLen),
    CallingPartyDec = parse_sccp_addr(CallingParty),
    DataLen = binary:at(Remain, DataPtr-2),
    Data = binary:part(Remain, DataPtr-2+1, DataLen),

    OptBin = case OptPtr of
                 0 -> <<>>;
                 _ -> binary:part(Remain, OptPtr-1, byte_size(Remain)-(OptPtr-1))
             end,
    OptList = parse_sccp_opts(OptBin, []),
    #sccp_msg_params_xudt{protocol_class = dec_protocol_class_and_options({ProtoClass, PCOpt}),
                          hop_counter = HopCounter,
                          called_party_addr = CalledPartyDec,
                          calling_party_addr = CallingPartyDec,
                          data = Data,
                          segmentation = proplists:get_value(segmentation, OptList),
                          importance = proplists:get_value(importance, OptList)
                         };
parse_sccp_msgt(sccp_msgt_xudts, DataBin) ->
    <<_:8, ReturnCause:8, HopCounter:8, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, OptPtr:8, Remain/binary>> = DataBin,
    CalledPartyLen = binary:at(Remain, CalledPartyPtr-4),
    CalledParty = binary:part(Remain, CalledPartyPtr-4+1, CalledPartyLen),
    CalledPartyDec = parse_sccp_addr(CalledParty),
    CallingPartyLen = binary:at(Remain, CallingPartyPtr-3),
    CallingParty = binary:part(Remain, CallingPartyPtr-3+1, CallingPartyLen),
    CallingPartyDec = parse_sccp_addr(CallingParty),
    DataLen = binary:at(Remain, DataPtr-2),
    Data = binary:part(Remain, DataPtr-2+1, DataLen),

    OptBin = case OptPtr of
                 0 -> <<>>;
                 _ -> binary:part(Remain, OptPtr-1, byte_size(Remain)-(OptPtr-1))
             end,
    OptList = parse_sccp_opts(OptBin, []),
    #sccp_msg_params_xudts{return_cause = dec_return_cause(ReturnCause),
                           hop_counter = HopCounter,
                           called_party_addr = CalledPartyDec,
                           calling_party_addr = CallingPartyDec,
                           data = Data,
                           segmentation = proplists:get_value(segmentation, OptList),
                           importance = proplists:get_value(importance, OptList)
                          };
parse_sccp_msgt(sccp_msgt_ludt, DataBin) ->
    <<_:8, PCOpt: 4, ProtoClass:4, HopCounter:8, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, OptPtr:8, Remain/binary>> = DataBin,
    CalledPartyLen = binary:at(Remain, CalledPartyPtr-4),
    CalledParty = binary:part(Remain, CalledPartyPtr-4+1, CalledPartyLen),
    CalledPartyDec = parse_sccp_addr(CalledParty),
    CallingPartyLen = binary:at(Remain, CallingPartyPtr-3),
    CallingParty = binary:part(Remain, CallingPartyPtr-3+1, CallingPartyLen),
    CallingPartyDec = parse_sccp_addr(CallingParty),
    DataLen = binary:at(Remain, DataPtr-2),
    Data = binary:part(Remain, DataPtr-2+1, DataLen),

    OptBin = case OptPtr of
                 0 -> <<>>;
                 _ -> binary:part(Remain, OptPtr-1, byte_size(Remain)-(OptPtr-1))
             end,
    OptList = parse_sccp_opts(OptBin, []),
    #sccp_msg_params_ludt{protocol_class = dec_protocol_class_and_options({ProtoClass, PCOpt}),
                          hop_counter = HopCounter,
                          called_party_addr = CalledPartyDec,
                          calling_party_addr = CallingPartyDec,
                          long_data = Data,
                          segmentation = proplists:get_value(segmentation, OptList),
                          importance = proplists:get_value(importance, OptList)
                         };
parse_sccp_msgt(sccp_msgt_ludts, DataBin) ->
    <<_:8, ReturnCause:8, HopCounter:8, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, OptPtr:8, Remain/binary>> = DataBin,
    CalledPartyLen = binary:at(Remain, CalledPartyPtr-4),
    CalledParty = binary:part(Remain, CalledPartyPtr-4+1, CalledPartyLen),
    CalledPartyDec = parse_sccp_addr(CalledParty),
    CallingPartyLen = binary:at(Remain, CallingPartyPtr-3),
    CallingParty = binary:part(Remain, CallingPartyPtr-3+1, CallingPartyLen),
    CallingPartyDec = parse_sccp_addr(CallingParty),
    DataLen = binary:at(Remain, DataPtr-2),
    Data = binary:part(Remain, DataPtr-2+1, DataLen),

    OptBin = case OptPtr of
                 0 -> <<>>;
                 _ -> binary:part(Remain, OptPtr-1, byte_size(Remain)-(OptPtr-1))
             end,
    OptList = parse_sccp_opts(OptBin, []),
    #sccp_msg_params_ludts{return_cause = dec_return_cause(ReturnCause),
                           hop_counter = HopCounter,
                           called_party_addr = CalledPartyDec,
                           calling_party_addr = CallingPartyDec,
                           long_data = Data,
                           segmentation = proplists:get_value(segmentation, OptList),
                           importance = proplists:get_value(importance, OptList)
                          }.

%% process one incoming SCCP message
parse_sccp_msg(DataBin) ->
    MsgType = dec_msg_type(binary:first(DataBin)),
    Parsed = parse_sccp_msgt(MsgType, DataBin),
    {ok, #sccp_msg{msg_type = MsgType, parameters = Parsed}}.

%% Encoding Part

gt_enc_by_odd(Odd) ->
    if Odd == 1 ->
            1;
       true ->
            2
    end.

encode_gt(undefined) ->
    {?SCCP_GTI_NO_GT, <<>>};
encode_gt(#global_title{gti = GTind, phone_number = PhoneNum,
                        nature_of_addr_ind = Nature,
                        trans_type = TransType,
                        numbering_plan = NumPlan}) ->
    case GTind of
        ?SCCP_GTI_NO_GT ->
            {GTind, <<>>};
        ?SCCP_GTI_NAT_ONLY ->
            %% Figure 7/Q.713
            {PhoneBin, OddEven} = ossie_isup_codec:encode_isup_party(PhoneNum),
            {GTind, <<OddEven:1, Nature:7, PhoneBin/binary>>};
        ?SCCP_GTI_TT_ONLY ->
            %% Figure 9/Q.913
            %% Used in national interfaces only, we cannot parse Digits
            {GTind, <<TransType:8, PhoneNum/binary>>};
        ?SCCP_GTI_TT_NP_ENC ->
            %% Figure 10/Q.713
            {PhoneBin, OddEven} = ossie_isup_codec:encode_isup_party(PhoneNum),
            Enc = gt_enc_by_odd(OddEven),
            {GTind, <<TransType:8, NumPlan:4, Enc:4, PhoneBin/binary>>};
        ?SCCP_GTI_TT_NP_ENC_NAT ->
            %% Figure 11/Q.713
            {PhoneBin, OddEven} = ossie_isup_codec:encode_isup_party(PhoneNum),
            Enc = gt_enc_by_odd(OddEven),
            {GTind, <<TransType:8, NumPlan:4, Enc:4, 0:1, Nature:7, PhoneBin/binary>>}
    end.

encode_pc(undefined) ->
    {0, <<>>};
encode_pc(PointCode) when is_integer(PointCode) ->
    {1, <<PointCode:16/little>>};
encode_pc(PcRec) ->
    PcInt = ossie_util:pointcode2int(PcRec),
    encode_pc(PcInt).

encode_ssn(SSN) ->
    case SSN of
        undefined ->
            {0, <<>>};
        _ ->
            {1, <<SSN:8>>}
    end.

undef_or_true(Foo) ->
    case Foo of
        undefined -> 0;
        0 -> 0;
        _ -> 1
    end.


encode_sccp_addr(#sccp_addr{res_nat_use = ResNatUse,
                            route_on_ssn = RoutInd,
                            point_code = PointCode,
                            ssn = SSN,
                            global_title = GT}) ->

    {GTind, GTbin} = encode_gt(GT),
    {SSNind, SSNbin} = encode_ssn(SSN),
    {PCind, PCbin} = encode_pc(PointCode),
    ResNatOut = undef_or_true(ResNatUse),
    RoutIndOut = undef_or_true(RoutInd),
    <<ResNatOut:1, RoutIndOut:1, GTind:4, SSNind:1, PCind:1, PCbin/binary, SSNbin/binary, GTbin/binary>>.


encode_sccp_opt({AddrTag, AddrVal}) when AddrTag == ?SCCP_PNC_CALLED_PARTY_ADDRESS;
                                         AddrTag == ?SCCP_PNC_CALLING_PARTY_ADDRESS ->
    AddrEnc = encode_sccp_addr(AddrVal),
    AddrLen = byte_size(AddrEnc),
    <<AddrTag:8, AddrLen:8, AddrEnc/binary>>;
encode_sccp_opt({OptInt, DataBin}) when is_binary(DataBin), is_integer(OptInt) ->
    DataBinLen = byte_size(DataBin),
    <<OptInt:8, DataBinLen:8, DataBin/binary>>;
encode_sccp_opt({Opt, DataBin}) when is_atom(Opt) ->
    OptNum = atom_to_opt(Opt),
    encode_sccp_opt({OptNum, DataBin});
encode_sccp_opt({Opt, DataInt}) when is_integer(DataInt), DataInt =< 255 ->
    encode_sccp_opt({Opt, <<DataInt:8>>});
encode_sccp_opt({Opt, DataList}) when is_list(DataList) ->
    encode_sccp_opt({Opt, list_to_binary(DataList)}).

encode_sccp_opts(OptList) ->
    FilteredList = lists:filter(fun({_Tag, Val}) -> undefined =/= Val end, OptList),
    e_sccp_opts(FilteredList, <<>>).

e_sccp_opts([], <<>>) ->
    <<>>;
e_sccp_opts([], OptEnc) ->
    %% end of options + convert to binary
    list_to_binary([OptEnc, ?SCCP_PNC_END_OF_OPTIONAL]);
e_sccp_opts([CurOpt|OptPropList], OptEnc) ->
    CurOptEnc = encode_sccp_opt(CurOpt),
    e_sccp_opts(OptPropList, list_to_binary([OptEnc,CurOptEnc])).


encode_sccp_msgt(?SCCP_MSGT_CR, P) ->
    #sccp_msg_params_cr{src_local_ref = SrcLocalRef,
                        protocol_class = ProtocolClassOpts,
                        called_party_addr = CalledParty,
                        credit = Credit,
                        calling_party_addr = CallingParty,
                        data = Data,
                        hop_counter = HopCounter,
                        importance = Importance
                       } = P,
    CalledPartyEnc = encode_sccp_addr(CalledParty),
    CalledPartyLen = byte_size(CalledPartyEnc),
    PtrOpt = CalledPartyLen+1+1,
    Params = [{credit, Credit},
              {calling_party_addr, CallingParty},
              {data, Data},
              {hop_counter, HopCounter},
              {importance, Importance}],
    OptBin = encode_sccp_opts(Params),
    {ProtoClass, PCOpt} = enc_protocol_class_and_options(ProtocolClassOpts),
    <<?SCCP_MSGT_CR:8, SrcLocalRef:24/big, PCOpt:4, ProtoClass:4, 2:8, PtrOpt:8, CalledPartyLen:8, CalledPartyEnc/binary, OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_CC, P) ->
    #sccp_msg_params_cc{dst_local_ref = DstLocalRef,
                        src_local_ref = SrcLocalRef,
                        protocol_class = ProtocolClassOpts,
                        credit = Credit,
                        called_party_addr = CalledParty,
                        data = Data,
                        importance = Importance
                       } = P,
    Params = [{credit, Credit},
              {called_party_addr, CalledParty},
              {data, Data},
              {importance, Importance}],
    OptBin = encode_sccp_opts(Params),
    {ProtoClass, PCOpt} = enc_protocol_class_and_options(ProtocolClassOpts),
    <<?SCCP_MSGT_CC:8, DstLocalRef:24/big, SrcLocalRef:24/big, PCOpt:4, ProtoClass:4, OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_CREF, P) ->
    #sccp_msg_params_cref{dst_local_ref = DstLocalRef,
                          refusal_cause = RefCause,
                          called_party_addr = CalledParty,
                          data = Data,
                          importance = Importance
                         } = P,
    Params = [{called_party_addr, CalledParty},
              {data, Data},
              {importance, Importance}],
    OptBin = encode_sccp_opts(Params),
    RefusalCause = enc_refusal_cause(RefCause),
    <<?SCCP_MSGT_CREF:8, DstLocalRef:24/big, RefusalCause:8, OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_RLSD, P) ->
    #sccp_msg_params_rlsd{dst_local_ref = DstLocalRef,
                          src_local_ref = SrcLocalRef,
                          release_cause = ReleaseCause,
                          data = Data,
                          importance = Importance
                         } = P,
    Params = [{data, Data},
              {importance, Importance}],
    OptBin = encode_sccp_opts(Params),
    <<?SCCP_MSGT_RLSD:8, DstLocalRef:24/big, SrcLocalRef:24/big, ReleaseCause:8, OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_RLC, P) ->
    #sccp_msg_params_rlc{dst_local_ref = DstLocalRef,
                         src_local_ref = SrcLocalRef
                        } = P,
    <<?SCCP_MSGT_RLC:8, DstLocalRef:24/big, SrcLocalRef:24/big>>;
encode_sccp_msgt(?SCCP_MSGT_DT1, P) ->
    #sccp_msg_params_dt1{dst_local_ref = DstLocalRef,
                         segm_reass = SegmReass,
                         data = Data
                        } = P,
    UserData = binarify(Data),
    UserDataLen = byte_size(UserData),
    <<?SCCP_MSGT_DT1:8, DstLocalRef:24/big, SegmReass:8, 1:8, UserDataLen:8, UserData/binary>>;
encode_sccp_msgt(?SCCP_MSGT_DT2, P) ->
    #sccp_msg_params_dt2{dst_local_ref = DstLocalRef,
                         seq_segm = SeqSegm,
                         data = Data
                        } = P,
    UserData = binarify(Data),
    UserDataLen = byte_size(UserData),
    <<?SCCP_MSGT_DT2:8, DstLocalRef:24/big, SeqSegm:16, 1:8, UserDataLen:8, UserData/binary>>;
encode_sccp_msgt(?SCCP_MSGT_AK, P) ->
    #sccp_msg_params_ak{dst_local_ref = DstLocalRef,
                        rx_seq_nr = RxSeqnr,
                        credit = Credit
                       } = P,
    <<?SCCP_MSGT_AK:8, DstLocalRef:24/big, RxSeqnr:8, Credit:8>>;
encode_sccp_msgt(?SCCP_MSGT_UDT, P) ->
    #sccp_msg_params_udt{protocol_class = ProtocolClassOpts,
                         called_party_addr = CalledParty,
                         calling_party_addr = CallingParty,
                         data = Data
                        } = P,
    CalledPartyEnc = encode_sccp_addr(CalledParty),
    CalledPartyLen = byte_size(CalledPartyEnc),
    CallingPartyEnc = encode_sccp_addr(CallingParty),
    CallingPartyLen = byte_size(CallingPartyEnc),
    UserData = binarify(Data),
    UserDataLen = byte_size(UserData),
    %% variable part
    CalledPartyPtr = 3,
    CallingPartyPtr = 2 + (1 + CalledPartyLen),
    DataPtr = 1 + (1 + CalledPartyLen) + (1 + CallingPartyLen),
    Remain = <<CalledPartyLen:8, CalledPartyEnc/binary,
               CallingPartyLen:8, CallingPartyEnc/binary,
               UserDataLen:8, UserData/binary>>,
    {ProtoClass, PCOpt} = enc_protocol_class_and_options(ProtocolClassOpts),
    <<?SCCP_MSGT_UDT:8, PCOpt:4, ProtoClass:4, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, Remain/binary>>;
encode_sccp_msgt(?SCCP_MSGT_UDTS, P) ->
    #sccp_msg_params_udts{return_cause = ReturnCause,
                          called_party_addr = CalledParty,
                          calling_party_addr = CallingParty,
                          data = Data
                         } = P,
    CalledPartyEnc = encode_sccp_addr(CalledParty),
    CalledPartyLen = byte_size(CalledPartyEnc),
    CallingPartyEnc = encode_sccp_addr(CallingParty),
    CallingPartyLen = byte_size(CallingPartyEnc),
    UserData = binarify(Data),
    UserDataLen = byte_size(UserData),
    %% variable part
    CalledPartyPtr = 3,
    CallingPartyPtr = 2 + (1 + CalledPartyLen),
    DataPtr = 1 + (1 + CalledPartyLen) + (1 + CallingPartyLen),
    Remain = <<CalledPartyLen:8, CalledPartyEnc/binary,
               CallingPartyLen:8, CallingPartyEnc/binary,
               UserDataLen:8, UserData/binary>>,
    RetCause = enc_return_cause(ReturnCause),
    <<?SCCP_MSGT_UDTS:8, RetCause:8, CalledPartyPtr:8, CallingPartyPtr:8, DataPtr:8, Remain/binary>>;
encode_sccp_msgt(?SCCP_MSGT_ED, P) ->
    #sccp_msg_params_ed{dst_local_ref = DstLocalRef,
                        data = Data
                       } = P,
    UserData = binarify(Data),
    UserDataLen = byte_size(UserData),
    DataPtr = 1,
    <<?SCCP_MSGT_ED:8, DstLocalRef:24/big, DataPtr:8, UserDataLen:8, UserData/binary>>;
encode_sccp_msgt(?SCCP_MSGT_EA, P) ->
    #sccp_msg_params_ea{dst_local_ref = DstLocalRef
                       } = P,
    <<?SCCP_MSGT_EA:8, DstLocalRef:24/big>>;
encode_sccp_msgt(?SCCP_MSGT_RSR, P) ->
    #sccp_msg_params_rsr{dst_local_ref = DstLocalRef,
                         src_local_ref = SrcLocalRef,
                         reset_cause = ResCause
                        } = P,
    ResetCause = enc_reset_cause(ResCause),
    <<?SCCP_MSGT_RSR:8, DstLocalRef:24/big, SrcLocalRef:24/big, ResetCause:8>>;
encode_sccp_msgt(?SCCP_MSGT_RSC, P) ->
    #sccp_msg_params_rsc{dst_local_ref = DstLocalRef,
                         src_local_ref = SrcLocalRef
                        } = P,
    <<?SCCP_MSGT_RSC:8, DstLocalRef:24/big, SrcLocalRef:24/big>>;
encode_sccp_msgt(?SCCP_MSGT_ERR, P) ->
    #sccp_msg_params_err{dst_local_ref = DstLocalRef,
                         error_cause = ErrCause
                        } = P,
    ErrorCause = enc_error_cause(ErrCause),
    <<?SCCP_MSGT_ERR:8, DstLocalRef:24/big, ErrorCause:8>>;
encode_sccp_msgt(?SCCP_MSGT_IT, P) ->
    #sccp_msg_params_it{dst_local_ref = DstLocalRef,
                        src_local_ref = SrcLocalRef,
                        protocol_class = ProtocolClassOpts,
                        seq_segm = SeqSegm,
                        credit = Credit
                       } = P,
    {ProtoClass, PCOpt} = enc_protocol_class_and_options(ProtocolClassOpts),
    <<?SCCP_MSGT_IT:8, DstLocalRef:24/big, SrcLocalRef:24/big, PCOpt:4, ProtoClass:4, SeqSegm:16, Credit:8>>;
encode_sccp_msgt(?SCCP_MSGT_XUDT, P) ->
    #sccp_msg_params_xudt{protocol_class = ProtocolClassOpts,
                          hop_counter = HopCounter,
                          called_party_addr = CalledParty,
                          calling_party_addr = CallingParty,
                          data = Data,
                          segmentation = Segmentation,
                          importance = Importance
                         } = P,
    CalledPartyEnc = encode_sccp_addr(CalledParty),
    CalledPartyLen = byte_size(CalledPartyEnc),
    CallingPartyEnc = encode_sccp_addr(CallingParty),
    CallingPartyLen = byte_size(CallingPartyEnc),
    DataLen = byte_size(Data),
    Params = [{segmentation, Segmentation},
              {importance, Importance}],
    OptBin = encode_sccp_opts(Params),
    %% (after four pointers)
    PtrCalledParty = 4,
    %% (after three pointers plus called party with length)
    PtrCallingParty = 3 + (1 + CalledPartyLen),
    %% (after two pointers plus called and calling parties with lengths)
    PtrData = 2 + (1 + CalledPartyLen) + (1 + CallingPartyLen),
    PtrOpt = case OptBin of
                 <<>> -> 0;
                 %% after one pointer and called/calling parties and data, all with lengths
                 _ -> 1 + (1 + CalledPartyLen) + (1 + CallingPartyLen) + (1 + DataLen)
             end,
    {ProtoClass, PCOpt} = enc_protocol_class_and_options(ProtocolClassOpts),
    <<?SCCP_MSGT_XUDT:8, PCOpt:4, ProtoClass:4, HopCounter:8,
      PtrCalledParty:8, PtrCallingParty:8, PtrData:8, PtrOpt:8,
      CalledPartyLen:8, CalledPartyEnc/binary,
      CallingPartyLen:8, CallingPartyEnc/binary,
      DataLen:8, Data/binary,
      OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_XUDTS, P) ->
    #sccp_msg_params_xudts{return_cause = ReturnCause,
                           hop_counter = HopCounter,
                           called_party_addr = CalledParty,
                           calling_party_addr = CallingParty,
                           data = Data,
                           segmentation = Segmentation,
                           importance = Importance
                          } = P,
    CalledPartyEnc = encode_sccp_addr(CalledParty),
    CalledPartyLen = byte_size(CalledPartyEnc),
    CallingPartyEnc = encode_sccp_addr(CallingParty),
    CallingPartyLen = byte_size(CallingPartyEnc),
    DataLen = byte_size(Data),
    Params = [{segmentation, Segmentation},
              {importance, Importance}],
    OptBin = encode_sccp_opts(Params),
    %% (after four pointers)
    PtrCalledParty = 4,
    %% (after three pointers plus called party with length)
    PtrCallingParty = 3 + (1 + CalledPartyLen),
    %% (after two pointers plus called and calling parties with lengths)
    PtrData = 2 + (1 + CalledPartyLen) + (1 + CallingPartyLen),
    PtrOpt = case OptBin of
                 <<>> -> 0;
                 %% after one pointer and called/calling parties and data, all with lengths
                 _ -> 1 + (1 + CalledPartyLen) + (1 + CallingPartyLen) + (1 + DataLen)
             end,
    RetCause = enc_return_cause(ReturnCause),
    <<?SCCP_MSGT_XUDTS:8, RetCause:8, HopCounter:8,
      PtrCalledParty:8, PtrCallingParty:8, PtrData:8, PtrOpt:8,
      CalledPartyLen:8, CalledPartyEnc/binary,
      CallingPartyLen:8, CallingPartyEnc/binary,
      DataLen:8, Data/binary,
      OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_LUDT, P) ->
    #sccp_msg_params_ludt{protocol_class = ProtocolClassOpts,
                          hop_counter = HopCounter,
                          called_party_addr = CalledParty,
                          calling_party_addr = CallingParty,
                          long_data = Data,
                          segmentation = Segmentation,
                          importance = Importance
                         } = P,
    CalledPartyEnc = encode_sccp_addr(CalledParty),
    CalledPartyLen = byte_size(CalledPartyEnc),
    CallingPartyEnc = encode_sccp_addr(CallingParty),
    CallingPartyLen = byte_size(CallingPartyEnc),
    DataLen = byte_size(Data),
    Params = [{segmentation, Segmentation},
              {importance, Importance}],
    OptBin = encode_sccp_opts(Params),
    %% (after four pointers)
    PtrCalledParty = 4,
    %% (after three pointers plus called party with length)
    PtrCallingParty = 3 + (1 + CalledPartyLen),
    %% (after two pointers plus called and calling parties with lengths)
    PtrData = 2 + (1 + CalledPartyLen) + (1 + CallingPartyLen),
    PtrOpt = case OptBin of
                 <<>> -> 0;
                 %% after one pointer and called/calling parties and data, all with lengths
                 _ -> 1 + (1 + CalledPartyLen) + (1 + CallingPartyLen) + (1 + DataLen)
             end,
    {ProtoClass, PCOpt} = enc_protocol_class_and_options(ProtocolClassOpts),
    <<?SCCP_MSGT_LUDT:8, PCOpt:4, ProtoClass:4, HopCounter:8,
      PtrCalledParty:8, PtrCallingParty:8, PtrData:8, PtrOpt:8,
      CalledPartyLen:8, CalledPartyEnc/binary,
      CallingPartyLen:8, CallingPartyEnc/binary,
      DataLen:8, Data/binary,
      OptBin/binary>>;
encode_sccp_msgt(?SCCP_MSGT_LUDTS, P) ->
    #sccp_msg_params_ludts{return_cause = ReturnCause,
                           hop_counter = HopCounter,
                           called_party_addr = CalledParty,
                           calling_party_addr = CallingParty,
                           long_data = Data,
                           segmentation = Segmentation,
                           importance = Importance
                          } = P,
    CalledPartyEnc = encode_sccp_addr(CalledParty),
    CalledPartyLen = byte_size(CalledPartyEnc),
    CallingPartyEnc = encode_sccp_addr(CallingParty),
    CallingPartyLen = byte_size(CallingPartyEnc),
    DataLen = byte_size(Data),
    Params = [{segmentation, Segmentation},
              {importance, Importance}],
    OptBin = encode_sccp_opts(Params),
    %% (after four pointers)
    PtrCalledParty = 4,
    %% (after three pointers plus called party with length)
    PtrCallingParty = 3 + (1 + CalledPartyLen),
    %% (after two pointers plus called and calling parties with lengths)
    PtrData = 2 + (1 + CalledPartyLen) + (1 + CallingPartyLen),
    PtrOpt = case OptBin of
                 <<>> -> 0;
                 %% after one pointer and called/calling parties and data, all with lengths
                 _ -> 1 + (1 + CalledPartyLen) + (1 + CallingPartyLen) + (1 + DataLen)
             end,
    RetCause = enc_return_cause(ReturnCause),
    <<?SCCP_MSGT_LUDTS:8, RetCause:8, HopCounter:8,
      PtrCalledParty:8, PtrCallingParty:8, PtrData:8, PtrOpt:8,
      CalledPartyLen:8, CalledPartyEnc/binary,
      CallingPartyLen:8, CallingPartyEnc/binary,
      DataLen:8, Data/binary,
      OptBin/binary>>.

%% encode one sccp message data structure into the on-wire format
encode_sccp_msg(#sccp_msg{msg_type = MsgType, parameters = Params}) ->
    MsgT = enc_msg_type(MsgType),
    encode_sccp_msgt(MsgT, Params).

gen_gt_helper(Number) when is_list(Number) ->
    #global_title{gti=?SCCP_GTI_NAT_ONLY,
                  numbering_plan = undefined,
                  encoding = undefined,
                  nature_of_addr_ind=?SCCP_NAI_INTERNATIONAL,
                  phone_number = Number}.

gen_addr_helper(Gt, Pc, Ssn) when is_record(Gt, global_title) ->
    #sccp_addr{point_code=Pc, ssn=Ssn, global_title=Gt};
gen_addr_helper(Number, Pc, Ssn) when is_list(Number) ->
    Gt = gen_gt_helper(Number),
    gen_addr_helper(Gt, Pc, Ssn).


gen_addr_helper(Gt, Pc) when is_record(Gt, global_title) ->
    #sccp_addr{point_code=Pc, global_title=Gt};
gen_addr_helper(Number, Pc) when is_list(Number) ->
    Gt = gen_gt_helper(Number),
    gen_addr_helper(Gt, Pc).

opt_to_atom(Num) ->
    case Num of
        ?SCCP_PNC_DESTINATION_LOCAL_REFERENCE -> dst_local_ref;
        ?SCCP_PNC_SOURCE_LOCAL_REFERENCE ->     src_local_ref;
        ?SCCP_PNC_CALLED_PARTY_ADDRESS ->       called_party_addr;
        ?SCCP_PNC_CALLING_PARTY_ADDRESS ->      calling_party_addr;
        ?SCCP_PNC_PROTOCOL_CLASS ->             protocol_class;
        ?SCCP_PNC_SEGMENTING ->                 segmenting;
        ?SCCP_PNC_RECEIVE_SEQ_NUMBER ->         rx_seq_number;
        ?SCCP_PNC_SEQUENCING ->                 seq_segm;
        ?SCCP_PNC_CREDIT ->                     credit;
        ?SCCP_PNC_RELEASE_CAUSE ->              release_cause;
        ?SCCP_PNC_RETURN_CAUSE ->               return_cause;
        ?SCCP_PNC_RESET_CAUSE ->                reset_cause;
        ?SCCP_PNC_ERROR_CAUSE ->                error_cause;
        ?SCCP_PNC_REFUSAL_CAUSE ->              refusal_cause;
        ?SCCP_PNC_DATA ->                       data;
        ?SCCP_PNC_SEGMENTATION ->               segmentation;
        ?SCCP_PNC_HOP_COUNTER ->                hop_counter;
        ?SCCP_PNC_IMPORTANCE ->                 importance;
        ?SCCP_PNC_LONG_DATA ->                  long_data;
        Foo -> Foo
    end.

atom_to_opt(Atom) ->
    %% Even though the tags are defined for these parameters, they are not optional.
    %% See optional parts for the different messages Q.713 - Chapter 1 and 4
    case Atom of
        %% dst_local_ref   -> ?SCCP_PNC_DESTINATION_LOCAL_REFERENCE;
        %% src_local_ref   -> ?SCCP_PNC_SOURCE_LOCAL_REFERENCE;
        called_party_addr  -> ?SCCP_PNC_CALLED_PARTY_ADDRESS;
        calling_party_addr -> ?SCCP_PNC_CALLING_PARTY_ADDRESS;
        %% protocol_class  -> ?SCCP_PNC_PROTOCOL_CLASS;
        %% segmenting      -> ?SCCP_PNC_SEGMENTING;
        %% rx_seq_number   -> ?SCCP_PNC_RECEIVE_SEQ_NUMBER;
        %% seq_segm        -> ?SCCP_PNC_SEQUENCING;
        credit          -> ?SCCP_PNC_CREDIT;
        %% release_cause   -> ?SCCP_PNC_RELEASE_CAUSE;
        %% return_cause    -> ?SCCP_PNC_RETURN_CAUSE;
        %% reset_cause     -> ?SCCP_PNC_RESET_CAUSE;
        %% error_cause     -> ?SCCP_PNC_ERROR_CAUSE;
        %% refusal_cause   -> ?SCCP_PNC_REFUSAL_CAUSE;
        data            -> ?SCCP_PNC_DATA;
        segmentation    -> ?SCCP_PNC_SEGMENTATION;
        hop_counter     -> ?SCCP_PNC_HOP_COUNTER;
        importance      -> ?SCCP_PNC_IMPORTANCE
        %% long_data       -> ?SCCP_PNC_LONG_DATA;
    end.

dec_msg_type(?SCCP_MSGT_CR) -> sccp_msgt_cr;
dec_msg_type(?SCCP_MSGT_CC) -> sccp_msgt_cc;
dec_msg_type(?SCCP_MSGT_CREF) -> sccp_msgt_cref;
dec_msg_type(?SCCP_MSGT_RLSD) -> sccp_msgt_rlsd;
dec_msg_type(?SCCP_MSGT_RLC) -> sccp_msgt_rlc;
dec_msg_type(?SCCP_MSGT_DT1) -> sccp_msgt_dt1;
dec_msg_type(?SCCP_MSGT_DT2) -> sccp_msgt_dt2;
dec_msg_type(?SCCP_MSGT_AK) -> sccp_msgt_ak;
dec_msg_type(?SCCP_MSGT_UDT) -> sccp_msgt_udt;
dec_msg_type(?SCCP_MSGT_UDTS) -> sccp_msgt_udts;
dec_msg_type(?SCCP_MSGT_ED) -> sccp_msgt_ed;
dec_msg_type(?SCCP_MSGT_EA) -> sccp_msgt_ea;
dec_msg_type(?SCCP_MSGT_RSR) -> sccp_msgt_rsr;
dec_msg_type(?SCCP_MSGT_RSC) -> sccp_msgt_rsc;
dec_msg_type(?SCCP_MSGT_ERR) -> sccp_msgt_err;
dec_msg_type(?SCCP_MSGT_IT) -> sccp_msgt_it;
dec_msg_type(?SCCP_MSGT_XUDT) -> sccp_msgt_xudt;
dec_msg_type(?SCCP_MSGT_XUDTS) -> sccp_msgt_xudts;
dec_msg_type(?SCCP_MSGT_LUDT) -> sccp_msgt_ludt;
dec_msg_type(?SCCP_MSGT_LUDTS) -> sccp_msgt_ludts.

enc_msg_type(sccp_msgt_cr) -> ?SCCP_MSGT_CR;
enc_msg_type(sccp_msgt_cc) -> ?SCCP_MSGT_CC;
enc_msg_type(sccp_msgt_cref) -> ?SCCP_MSGT_CREF;
enc_msg_type(sccp_msgt_rlsd) -> ?SCCP_MSGT_RLSD;
enc_msg_type(sccp_msgt_rlc) -> ?SCCP_MSGT_RLC;
enc_msg_type(sccp_msgt_dt1) -> ?SCCP_MSGT_DT1;
enc_msg_type(sccp_msgt_dt2) -> ?SCCP_MSGT_DT2;
enc_msg_type(sccp_msgt_ak) -> ?SCCP_MSGT_AK;
enc_msg_type(sccp_msgt_udt) -> ?SCCP_MSGT_UDT;
enc_msg_type(sccp_msgt_udts) -> ?SCCP_MSGT_UDTS;
enc_msg_type(sccp_msgt_ed) -> ?SCCP_MSGT_ED;
enc_msg_type(sccp_msgt_ea) -> ?SCCP_MSGT_EA;
enc_msg_type(sccp_msgt_rsr) -> ?SCCP_MSGT_RSR;
enc_msg_type(sccp_msgt_rsc) -> ?SCCP_MSGT_RSC;
enc_msg_type(sccp_msgt_err) -> ?SCCP_MSGT_ERR;
enc_msg_type(sccp_msgt_it) -> ?SCCP_MSGT_IT;
enc_msg_type(sccp_msgt_xudt) -> ?SCCP_MSGT_XUDT;
enc_msg_type(sccp_msgt_xudts) -> ?SCCP_MSGT_XUDTS;
enc_msg_type(sccp_msgt_ludt) -> ?SCCP_MSGT_LUDT;
enc_msg_type(sccp_msgt_ludts) -> ?SCCP_MSGT_LUDTS.

dec_protocol_class_and_options({0, 0}) -> {basic_connectionless, discard_on_error};
dec_protocol_class_and_options({0, 8}) -> {basic_connectionless, return_on_error};
dec_protocol_class_and_options({0, S}) -> {basic_connectionless, {spare, S}};
dec_protocol_class_and_options({1, 0}) -> {sequenced_connectionless, discard_on_error};
dec_protocol_class_and_options({1, 8}) -> {sequenced_connectionless, return_on_error};
dec_protocol_class_and_options({1, S}) -> {sequenced_connectionless, {spare, S}};
dec_protocol_class_and_options({2, S}) -> {basic_connection_oriented, {spare, S}};
dec_protocol_class_and_options({3, S}) -> {flow_control_connection_oriented, {spare, S}}.

enc_protocol_class_and_options({basic_connectionless, discard_on_error}) -> {0, 0};
enc_protocol_class_and_options({basic_connectionless, return_on_error}) -> {0, 8};
enc_protocol_class_and_options({basic_connectionless, {spare, S}}) -> {0, S};
enc_protocol_class_and_options({sequenced_connectionless, discard_on_error}) -> {1, 0};
enc_protocol_class_and_options({sequenced_connectionless, return_on_error}) -> {1, 8};
enc_protocol_class_and_options({sequenced_connectionless, {spare, S}}) -> {1, S};
enc_protocol_class_and_options({basic_connection_oriented, {spare, S}}) -> {2, S};
enc_protocol_class_and_options({flow_control_connection_oriented, {spare, S}}) -> {3, S}.

dec_return_cause(?SCCP_CAUSE_RET_NOTRANS_NATURE) -> no_translation_of_addr_nature;
dec_return_cause(?SCCP_CAUSE_RET_NOTRANS_ADDR) ->  no_translation_of_addr;
dec_return_cause(?SCCP_CAUSE_RET_SUBSYS_CONG) -> subsystem_congestion;
dec_return_cause(?SCCP_CAUSE_RET_SUBSYS_FAILURE) -> subsystem_failure;
dec_return_cause(?SCCP_CAUSE_RET_UNEQUIP_USER) -> unequipped_user;
dec_return_cause(?SCCP_CAUSE_RET_MTP_FAILURE) -> network_failure;
dec_return_cause(?SCCP_CAUSE_RET_NET_CONG) -> network_congestion;
dec_return_cause(?SCCP_CAUSE_RET_UNQUALIFIED) -> unqualified;
dec_return_cause(?SCCP_CAUSE_RET_ERR_MSG_TRANSP) -> error_message_transport;
dec_return_cause(?SCCP_CAUSE_RET_ERR_LOCAL_PROC) -> error_local_processing;
dec_return_cause(?SCCP_CAUSE_RET_DEST_NO_REASS) -> destination_cannot_reassemble;
dec_return_cause(?SCCP_CAUSE_RET_SCCP_FAILURE) -> sccp_failure;
dec_return_cause(?SCCP_CAUSE_RET_HOP_CTR_FAIL) -> hop_counter_violation;
dec_return_cause(?SCCP_CAUSE_RET_SEG_NOT_SUPP) -> segmentation_unsupported;
dec_return_cause(?SCCP_CAUSE_RET_SEG_FAILURE) -> segmentation_failure;
dec_return_cause(Ret) -> {reserved, Ret}.

enc_return_cause(no_translation_of_addr_nature) -> ?SCCP_CAUSE_RET_NOTRANS_NATURE;
enc_return_cause(no_translation_of_addr) -> ?SCCP_CAUSE_RET_NOTRANS_ADDR;
enc_return_cause(subsystem_congestion) -> ?SCCP_CAUSE_RET_SUBSYS_CONG;
enc_return_cause(subsystem_failure) -> ?SCCP_CAUSE_RET_SUBSYS_FAILURE;
enc_return_cause(unequipped_user) -> ?SCCP_CAUSE_RET_UNEQUIP_USER;
enc_return_cause(network_failure) -> ?SCCP_CAUSE_RET_MTP_FAILURE;
enc_return_cause(network_congestion) -> ?SCCP_CAUSE_RET_NET_CONG;
enc_return_cause(unqualified) -> ?SCCP_CAUSE_RET_UNQUALIFIED;
enc_return_cause(error_message_transport) -> ?SCCP_CAUSE_RET_ERR_MSG_TRANSP;
enc_return_cause(error_local_processing) -> ?SCCP_CAUSE_RET_ERR_LOCAL_PROC;
enc_return_cause(destination_cannot_reassemble) -> ?SCCP_CAUSE_RET_DEST_NO_REASS;
enc_return_cause(sccp_failure) -> ?SCCP_CAUSE_RET_SCCP_FAILURE;
enc_return_cause(hop_counter_violation) -> ?SCCP_CAUSE_RET_HOP_CTR_FAIL;
enc_return_cause(segmentation_unsupported) -> ?SCCP_CAUSE_RET_SEG_NOT_SUPP;
enc_return_cause(segmentation_failure) -> ?SCCP_CAUSE_RET_SEG_FAILURE;
enc_return_cause({reserved, Ret}) -> Ret.

dec_reset_cause(?SCCP_CAUSE_RES_ENDU_ORIGINATED) -> end_user_originated;
dec_reset_cause(?SCCP_CAUSE_RES_SCCP_USER_ORIG) -> sccp_user_originated;
dec_reset_cause(?SCCP_CAUSE_RES_MSGO_OOO_PS) -> msg_out_of_order_incorrect_ps;
dec_reset_cause(?SCCP_CAUSE_RES_MSGO_OOO_PR) -> msg_out_of_order_incorrect_pr;
dec_reset_cause(?SCCP_CAUSE_RES_MSGO_OO_WIND) -> remote_procedure_error_message_out_of_window;
dec_reset_cause(?SCCP_CAUSE_RES_INC_PS_REINIT) -> remote_procedure_error_incorrect_ps_after_reinitialization;
dec_reset_cause(?SCCP_CAUSE_RES_REM_GENERAL) -> remote_procedure_error_general;
dec_reset_cause(?SCCP_CAUSE_RES_REM_OPERATIONAL) -> remote_end_user_operational;
dec_reset_cause(?SCCP_CAUSE_RES_NET_OPERATIONAL) -> network_operational;
dec_reset_cause(?SCCP_CAUSE_RES_ACC_OPERATIONAL) -> access_operational;
dec_reset_cause(?SCCP_CAUSE_RES_NET_CONG) -> network_congestion;
dec_reset_cause(?SCCP_CAUSE_RES_UNQUALIFIED) -> unqualified.

enc_reset_cause(end_user_originated) -> ?SCCP_CAUSE_RES_ENDU_ORIGINATED;
enc_reset_cause(sccp_user_originated) -> ?SCCP_CAUSE_RES_SCCP_USER_ORIG;
enc_reset_cause(msg_out_of_order_incorrect_ps) -> ?SCCP_CAUSE_RES_MSGO_OOO_PS;
enc_reset_cause(msg_out_of_order_incorrect_pr) -> ?SCCP_CAUSE_RES_MSGO_OOO_PR;
enc_reset_cause(remote_procedure_error_message_out_of_window) -> ?SCCP_CAUSE_RES_MSGO_OO_WIND;
enc_reset_cause(remote_procedure_error_incorrect_ps_after_reinitialization) -> ?SCCP_CAUSE_RES_INC_PS_REINIT;
enc_reset_cause(remote_procedure_error_general) -> ?SCCP_CAUSE_RES_REM_GENERAL;
enc_reset_cause(remote_end_user_operational) -> ?SCCP_CAUSE_RES_REM_OPERATIONAL;
enc_reset_cause(network_operational) -> ?SCCP_CAUSE_RES_NET_OPERATIONAL;
enc_reset_cause(access_operational) -> ?SCCP_CAUSE_RES_ACC_OPERATIONAL;
enc_reset_cause(network_congestion) -> ?SCCP_CAUSE_RES_NET_CONG;
enc_reset_cause(unqualified) -> ?SCCP_CAUSE_RES_UNQUALIFIED.

dec_error_cause(?SCCP_CAUSE_ERR_LRN_UNASSIGNED) -> local_reference_number_mismatch_unassigned_destination;
dec_error_cause(?SCCP_CAUSE_ERR_LRN_MISMATCH) -> local_reference_number_mismatch_inconsistent_source;
dec_error_cause(?SCCP_CAUSE_ERR_PC_MISMATCH) -> point_code_mismatch;
dec_error_cause(?SCCP_CAUSE_ERR_SCLASS_MISMATCH) -> service_class_mismatch;
dec_error_cause(?SCCP_CAUSE_ERR_UNQUALIFIED) -> unqualified.

enc_error_cause(local_reference_number_mismatch_unassigned_destination) -> ?SCCP_CAUSE_ERR_LRN_UNASSIGNED;
enc_error_cause(local_reference_number_mismatch_inconsistent_source) -> ?SCCP_CAUSE_ERR_LRN_MISMATCH;
enc_error_cause(point_code_mismatch) -> ?SCCP_CAUSE_ERR_PC_MISMATCH;
enc_error_cause(service_class_mismatch) -> ?SCCP_CAUSE_ERR_SCLASS_MISMATCH;
enc_error_cause(unqualified) -> ?SCCP_CAUSE_ERR_UNQUALIFIED.

dec_refusal_cause(?SCCP_CAUSE_REF_ENDU_ORIGINATED) -> end_user_originated;
dec_refusal_cause(?SCCP_CAUSE_REF_ENDU_CONGESTION) -> end_user_congestion;
dec_refusal_cause(?SCCP_CAUSE_REF_ENDU_FAILURE) -> end_user_failure;
dec_refusal_cause(?SCCP_CAUSE_REF_USER_ORIGINATED) -> sccp_user_originated;
dec_refusal_cause(?SCCP_CAUSE_REF_DEST_UNKNOWN) -> destination_address_unknown;
dec_refusal_cause(?SCCP_CAUSE_REF_DEST_INACCESS) -> destination_inaccess;
dec_refusal_cause(?SCCP_CAUSE_REF_QOS_UNAVAIL_NTRANS) -> network_resource_qos_unavailable_non_transient;
dec_refusal_cause(?SCCP_CAUSE_REF_QOS_UNAVAIL_TRANS) -> network_resource_qos_unavailable_transient;
dec_refusal_cause(?SCCP_CAUSE_REF_ACCESS_FAIL) -> access_failure;
dec_refusal_cause(?SCCP_CAUSE_REF_ACCESS_CONGESTION) -> access_congestion;
dec_refusal_cause(?SCCP_CAUSE_REF_SUBSYS_FAILURE) -> subsystem_failure;
dec_refusal_cause(?SCCP_CAUSE_REF_SUBSYS_CONGESTION) -> subsystem_congestion;
dec_refusal_cause(?SCCP_CAUSE_REF_EXP_CONN_EST_TMR) -> expiration_of_the_connection_established_timer;
dec_refusal_cause(?SCCP_CAUSE_REF_INCOMP_USER_DATA) -> incompatible_user_data;
dec_refusal_cause(?SCCP_CAUSE_REF_RESERVED) -> reserved;
dec_refusal_cause(?SCCP_CAUSE_REF_UNQUALIFIED) -> unqualified;
dec_refusal_cause(?SCCP_CAUSE_REF_HOP_COUNTER_VIOL) -> hop_counter_violation;
dec_refusal_cause(?SCCP_CAUSE_REF_SCCP_FAIL) -> sccp_failure;
dec_refusal_cause(?SCCP_CAUSE_REF_NO_GTT_FOR_NATURE) -> no_translation_for_an_address_of_such_nature;
dec_refusal_cause(?SCCP_CAUSE_REF_UNEQUIPPED_USER) -> unequipped_user.

enc_refusal_cause(end_user_originated) -> ?SCCP_CAUSE_REF_ENDU_ORIGINATED;
enc_refusal_cause(end_user_congestion) -> ?SCCP_CAUSE_REF_ENDU_CONGESTION;
enc_refusal_cause(end_user_failure) -> ?SCCP_CAUSE_REF_ENDU_FAILURE;
enc_refusal_cause(sccp_user_originated) -> ?SCCP_CAUSE_REF_USER_ORIGINATED;
enc_refusal_cause(destination_address_unknown) -> ?SCCP_CAUSE_REF_DEST_UNKNOWN;
enc_refusal_cause(destination_inaccess) -> ?SCCP_CAUSE_REF_DEST_INACCESS;
enc_refusal_cause(network_resource_qos_unavailable_non_transient) -> ?SCCP_CAUSE_REF_QOS_UNAVAIL_NTRANS;
enc_refusal_cause(network_resource_qos_unavailable_transient) -> ?SCCP_CAUSE_REF_QOS_UNAVAIL_TRANS;
enc_refusal_cause(access_failure) -> ?SCCP_CAUSE_REF_ACCESS_FAIL;
enc_refusal_cause(access_congestion) -> ?SCCP_CAUSE_REF_ACCESS_CONGESTION;
enc_refusal_cause(subsystem_failure) -> ?SCCP_CAUSE_REF_SUBSYS_FAILURE;
enc_refusal_cause(subsystem_congestion) -> ?SCCP_CAUSE_REF_SUBSYS_CONGESTION;
enc_refusal_cause(expiration_of_the_connection_established_timer) -> ?SCCP_CAUSE_REF_EXP_CONN_EST_TMR;
enc_refusal_cause(incompatible_user_data) -> ?SCCP_CAUSE_REF_INCOMP_USER_DATA;
enc_refusal_cause(reserved) -> ?SCCP_CAUSE_REF_RESERVED;
enc_refusal_cause(unqualified) -> ?SCCP_CAUSE_REF_UNQUALIFIED;
enc_refusal_cause(hop_counter_violation) -> ?SCCP_CAUSE_REF_HOP_COUNTER_VIOL;
enc_refusal_cause(sccp_failure) -> ?SCCP_CAUSE_REF_SCCP_FAIL;
enc_refusal_cause(no_translation_for_an_address_of_such_nature) -> ?SCCP_CAUSE_REF_NO_GTT_FOR_NATURE;
enc_refusal_cause(unequipped_user) -> ?SCCP_CAUSE_REF_UNEQUIPPED_USER.
