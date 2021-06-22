%% 3GPP TS 29.118 version 16.0.0 Release 16
-module(ossie_sgsap_codec).

-include("../include/sgsap.hrl").

-export([parse_sgsap_msg/1, encode_sgsap_msg/1]).

parse_sgsap_msg(DataBin) when is_binary(DataBin) ->
    <<MsgType:8, Remain/binary>> = DataBin,
    MsgT = parse_msg_type(MsgType),
    OptList = parse_sgsap_opts(Remain),
    P = parse_sgsap_params(MsgT, OptList),
    #sgsap_msg{msg_type = MsgT, msg_length = byte_size(DataBin), payload = P};
parse_sgsap_msg(Data) when is_list(Data) ->
    parse_sgsap_msg(list_to_binary(Data)).

%% Chapter 8 Message functional definitions and contents
parse_sgsap_params(sgsap_msgt_alert_ack, OptList) ->
    #sgsap_msg_params_alert_ack{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList)
      };
parse_sgsap_params(sgsap_msgt_alert_reject, OptList) ->
    #sgsap_msg_params_alert_reject{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       sgs_cause = proplists:get_value(sgsap_iei_sgs_clause, OptList)
      };
parse_sgsap_params(sgsap_msgt_alert_request, OptList) ->
    #sgsap_msg_params_alert_request{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList)
      };
parse_sgsap_params(sgsap_msgt_downlink_unitdata, OptList) ->
    #sgsap_msg_params_downlink_unitdata{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       nas_message_container = proplists:get_value(sgsap_iei_nas_message_container, OptList)
      };
parse_sgsap_params(sgsap_msgt_eps_detach_ack, OptList) ->
    #sgsap_msg_params_eps_detach_ack{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList)
      };
parse_sgsap_params(sgsap_msgt_eps_detach_indication, OptList) ->
    #sgsap_msg_params_eps_detach_indication{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       mme_name = proplists:get_value(sgsap_iei_mme_name, OptList),
       imsi_detach_from_eps_service_type = proplists:get_value(sgsap_iei_imsi_detach_from_eps_service_type, OptList)
      };
parse_sgsap_params(sgsap_msgt_imsi_detach_ack, OptList) ->
    #sgsap_msg_params_imsi_detach_ack{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList)
      };
parse_sgsap_params(sgsap_msgt_imsi_detach_indication, OptList) ->
    #sgsap_msg_params_imsi_detach_indication{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       mme_name = proplists:get_value(sgsap_iei_mme_name, OptList),
       imsi_detach_from_non_eps_service_type = proplists:get_value(sgsap_iei_imsi_detach_from_non_eps_service_type, OptList)
      };
parse_sgsap_params(sgsap_msgt_location_update_accept, OptList) ->
    #sgsap_msg_params_location_update_accept{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       location_area_identifier = proplists:get_value(sgsap_iei_location_area_identifier, OptList),
       mobile_identity = proplists:get_value(sgsap_iei_mobile_identity, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_location_update_reject, OptList) ->
    #sgsap_msg_params_location_update_reject{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       reject_cause = proplists:get_value(sgsap_iei_reject_cause, OptList),
       location_area_identifier = proplists:get_value(sgsap_iei_location_area_identifier, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_location_update_request, OptList) ->
    [LAI1|LAI2] = proplists:get_all_values(sgsap_iei_location_area_identifier, OptList),
    #sgsap_msg_params_location_update_request{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       mme_name = proplists:get_value(sgsap_iei_mme_name, OptList),
       eps_location_update_type = proplists:get_value(sgsap_iei_eps_location_update_type, OptList),
       new_location_area_identifier = LAI1,
       old_location_area_identifier = case LAI2 of [] -> undefined; [L|_] -> L end,
       tmsi_status = proplists:get_value(sgsap_iei_tmsi_status, OptList, undefined),
       imeisv = proplists:get_value(sgsap_iei_imeisv, OptList, undefined),
       tracking_area_identity = proplists:get_value(sgsap_iei_tracking_area_identity, OptList, undefined),
       e_utran_cell_global_identity = proplists:get_value(sgsap_iei_e_utran_cell_global_identity, OptList, undefined),
       tmsi_based_nri_container = proplists:get_value(sgsap_iei_tmsi_based_nri_container, OptList, undefined),
       selected_cs_domain_operator = proplists:get_value(sgsap_iei_selected_cs_domain_operator, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_mm_information_request, OptList) ->
    #sgsap_msg_params_mm_information_request{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       mm_information = proplists:get_value(sgsap_iei_mm_information, OptList)
      };
parse_sgsap_params(sgsap_msgt_paging_reject, OptList) ->
    #sgsap_msg_params_paging_reject{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       sgs_cause = proplists:get_value(sgsap_iei_sgs_clause, OptList)
      };
parse_sgsap_params(sgsap_msgt_paging_request, OptList) ->
    #sgsap_msg_params_paging_request{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       vlr_name = proplists:get_value(sgsap_iei_vlr_name, OptList),
       service_indicator = proplists:get_value(sgsap_iei_service_indicator, OptList),
       tmsi = proplists:get_value(sgsap_iei_tmsi, OptList,  undefined),
       cli = proplists:get_value(sgsap_iei_cli, OptList, undefined),
       ss_code = proplists:get_value(sgsap_iei_ss_code, OptList, undefined),
       lcs_indicator = proplists:get_value(sgsap_iei_lcs_indicator, OptList, undefined),
       lcs_client_identity = proplists:get_value(sgsap_iei_lcs_client_identity, OptList, undefined),
       channel_needed = proplists:get_value(sgsap_iei_channel_needed, OptList, undefined),
       emlpp_priority = proplists:get_value(sgsap_iei_emlpp_priority, OptList, undefined),
       additional_paging_indicators = proplists:get_value(sgsap_iei_additional_paging_indicators, OptList, undefined),
       sm_delivery_timer = proplists:get_value(sgsap_iei_sm_delivery_timer, OptList, undefined),
       sm_delivery_start_time = proplists:get_value(sgsap_iei_sm_delivery_start_time, OptList, undefined),
       maximum_retransmission_time = proplists:get_value(sgsap_iei_maximum_retransmission_time, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_reset_ack, OptList) ->
    #sgsap_msg_params_reset_ack{
       mme_name = proplists:get_value(sgsap_iei_mme_name, OptList, undefined),
       vlr_name = proplists:get_value(sgsap_iei_vlr_name, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_reset_indication, OptList) ->
    #sgsap_msg_params_reset_indication{
       mme_name = proplists:get_value(sgsap_iei_mme_name, OptList, undefined),
       vlr_name = proplists:get_value(sgsap_iei_vlr_name, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_service_request, OptList) ->
    #sgsap_msg_params_service_request{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       service_indicator = proplists:get_value(sgsap_iei_service_indicator, OptList),
       imeisv = proplists:get_value(sgsap_iei_imeisv, OptList, undefined),
       ue_time_zone = proplists:get_value(sgsap_iei_ue_time_zone, OptList, undefined),
       mobile_station_classmark_2 = proplists:get_value(sgsap_iei_mobile_station_classmark_2, OptList, undefined),
       tracking_area_identity = proplists:get_value(sgsap_iei_tracking_area_identity, OptList, undefined),
       e_utran_cell_global_identity = proplists:get_value(sgsap_iei_e_utran_cell_global_identity, OptList, undefined),
       ue_emm_mode = proplists:get_value(sgsap_iei_ue_emm_mode, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_status, OptList) ->
    #sgsap_msg_params_status{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList, undefined),
       sgs_cause = proplists:get_value(sgsap_iei_sgs_clause, OptList),
       erroneous_message = proplists:get_value(sgsap_iei_erroneous_message, OptList)
      };
parse_sgsap_params(sgsap_msgt_tmsi_reallocation_complete, OptList) ->
    #sgsap_msg_params_tmsi_reallocation_complete{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList)
      };
parse_sgsap_params(sgsap_msgt_ue_activity_indication, OptList) ->
    #sgsap_msg_params_ue_activity_indication{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       maximum_ue_availability_time = proplists:get_value(sgsap_iei_maximum_ue_availability_time, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_ue_unreachable, OptList) ->
    #sgsap_msg_params_ue_unreachable{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       sgs_cause = proplists:get_value(sgsap_iei_sgs_clause, OptList),
       requested_retransmission_time = proplists:get_value(sgsap_iei_requested_retransmission_time, OptList, undefined),
       additional_ue_unreachable_indicators = proplists:get_value(sgsap_iei_additional_ue_unreachable_indicators, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_uplink_unitdata, OptList) ->
    #sgsap_msg_params_uplink_unitdata{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       nas_message_container = proplists:get_value(sgsap_iei_nas_message_container, OptList),
       imeisv = proplists:get_value(sgsap_iei_imeisv, OptList, undefined),
       ue_time_zone = proplists:get_value(sgsap_iei_ue_time_zone, OptList, undefined),
       mobile_station_classmark_2 = proplists:get_value(sgsap_iei_mobile_station_classmark_2, OptList, undefined),
       tracking_area_identity = proplists:get_value(sgsap_iei_tracking_area_identity, OptList, undefined),
       e_utran_cell_global_identity = proplists:get_value(sgsap_iei_e_utran_cell_global_identity, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_release_request, OptList) ->
    #sgsap_msg_params_release_request{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       sgs_cause = proplists:get_value(sgsap_iei_sgs_clause, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_service_abort_request, OptList) ->
    #sgsap_msg_params_service_abort_request{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList)
      };
parse_sgsap_params(sgsap_msgt_mo_csfb_indication, OptList) ->
    #sgsap_msg_params_mo_csfb_indication{
       imsi = proplists:get_value(sgsap_iei_imsi, OptList),
       tracking_area_identity = proplists:get_value(sgsap_iei_tracking_area_identity, OptList, undefined),
       e_utran_cell_global_identity = proplists:get_value(sgsap_iei_e_utran_cell_global_identity, OptList, undefined)
      };
parse_sgsap_params(sgsap_msgt_unknown, OptList) ->
    OptList.

parse_sgsap_opts(OptBin) when is_binary(OptBin) ->
    parse_sgsap_opts(OptBin, []).

parse_sgsap_opts(<<>>, OptList) when is_list(OptList) ->
    OptList;
parse_sgsap_opts(OptBin, OptList) when is_binary(OptBin), is_list(OptList) ->
    <<IEI:8/big, Length:8/big, Remain/binary>> = OptBin,
    <<CurOpt:Length/binary, Remain2/binary>> = Remain,
    NewOpt = parse_sgsap_opt(dec_iei(IEI), CurOpt),
    parse_sgsap_opts(Remain2, OptList ++ [NewOpt]).

%% Chapter 9 Information element coding
parse_sgsap_opt(sgsap_iei_cli = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_eps_location_update_type = Opt, OptBin) ->
    C = case OptBin of
            <<2#00000001:8>> -> imsi_attach;
            <<2#00000010:8>> -> normal_location_update;
            _ -> normal_location_update
        end,
    {Opt, C};
parse_sgsap_opt(sgsap_iei_erroneous_message = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_e_utran_cell_global_identity = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_global_cn_id = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_imeisv = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_imsi = Opt, OptBin) ->
    %% 3GPP TS 29.018 - 18.4.10
    {Opt, lists:nthtail(1, ossie_util:decode_tbcd(OptBin))};
parse_sgsap_opt(sgsap_iei_imsi_detach_from_eps_service_type = Opt, OptBin) ->
    C = case OptBin of
            <<2#00000001:8>> -> network_initiated_imsi_detach_from_eps_services;
            <<2#00000010:8>> -> ue_initiated_imsi_detach_from_eps_services;
            <<2#00000011:8>> -> eps_services_not_allowed;
            _ -> {reserved, OptBin}
        end,
    {Opt, C};
parse_sgsap_opt(sgsap_iei_imsi_detach_from_non_eps_service_type = Opt, OptBin) ->
    C = case OptBin of
            <<2#00000001:8>> -> explicit_ue_initiated_imsi_detach_from_non_eps_services;
            <<2#00000010:8>> -> combined_ue_initiated_imsi_detach_from_eps_and_non_eps_services;
            <<2#00000011:8>> -> implicit_network_initiated_imsi_detach_from_eps_and_non_eps_services;
            _ -> {reserved, OptBin}
        end,
    {Opt, C};
parse_sgsap_opt(sgsap_iei_lcs_client_identity = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_lcs_indicator = Opt, OptBin) ->
    C = case OptBin of
            <<2#00000001:8>> -> mt_lr;
            _ -> {unspecified, OptBin}
        end,
    {Opt, C};
parse_sgsap_opt(sgsap_iei_location_area_identifier = Opt, OptBin) ->
    <<M:3/binary, Lac/binary>> = OptBin,
    MCCMNC = ossie_util:decode_mcc_mnc(M),
    {Opt, {MCCMNC, Lac}};
parse_sgsap_opt(sgsap_iei_mm_information = Opt, OptBin) ->
    MM = decode_mm_information(OptBin),
    {Opt, MM};
parse_sgsap_opt(sgsap_iei_mme_name = Opt, OptBin) ->
    Chars = decode_name(OptBin),
    {Opt, Chars};
parse_sgsap_opt(sgsap_iei_mobile_identity = Opt, OptBin) ->
    <<D1:4, _IsOdd:1, Type:3, R/binary>> = OptBin,
    T = case Type of
            2#001 -> imsi;
            2#010 -> imei;
            2#011 -> imeisv;
            2#100 -> tmsi;
            2#101 -> tmgi;
            2#000 -> no_identity
        end,
    case T of
        _ when T == imsi; T == imei; T == imeisv ->
            [_|Ds] = ossie_util:decode_tbcd(<<D1:4, 0:4, R/binary>>),
            {Opt, {T, Ds}};
        tmsi ->
            {Opt, {T, R}};
        tmgi ->
            <<_:2, Options:2>> = <<D1:4>>,
            <<MBMSServiceId:3/binary, Extras/binary>> = R,
            O = case Options of
                    2#00 ->
                        [];
                    2#01 ->
                        [{mcc_mnc, ossie_util:decode_mcc_mnc(Extras)}];
                    2#10 ->
                        [{mbms_session_id, Extras}];
                    2#11 ->
                        <<MCCMNC:3/binary, SessionId:1/binary>> = Extras,
                        [{mcc_mnc, ossie_util:decode_mcc_mnc(MCCMNC)},
                         {mbms_session_id, SessionId}]
                end,
            {Opt, [{mbms_service_id, MBMSServiceId}|O]};
        no_identity ->
            {Opt, no_identity}
    end;
parse_sgsap_opt(sgsap_iei_mobile_station_classmark_2 = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_nas_message_container = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_reject_cause = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_service_indicator = Opt, OptBin) ->
    C = case OptBin of
            <<2#00000001:8>> -> cs_call_indicator;
            <<2#00000010:8>> -> sms_indicator;
            _ -> cs_call_indicator
        end,
    {Opt, C};
parse_sgsap_opt(sgsap_iei_sgs_cause = Opt, OptBin) ->
    C = case OptBin of
            <<2#00000001:8>> -> imsi_detached_for_eps_services;
            <<2#00000010:8>> -> imsi_detached_for_eps_and_non_eps_services;
            <<2#00000011:8>> -> imsi_unknown;
            <<2#00000100:8>> -> imsi_detached_for_non_eps_services;
            <<2#00000101:8>> -> imsi_implicitly_detached_for_non_eps_services;
            <<2#00000110:8>> -> ue_unreachable;
            <<2#00000111:8>> -> message_not_compatible_with_the_protocol_state;
            <<2#00001000:8>> -> missing_mandatory_information_element;
            <<2#00001001:8>> -> invalid_mandatory_information;
            <<2#00001010:8>> -> conditional_information_element_error;
            <<2#00001011:8>> -> semantically_incorrect_message;
            <<2#00001100:8>> -> message_unknown;
            <<2#00001101:8>> -> mobile_terminating_cs_fallback_call_rejected_by_the_user;
            <<2#00001110:8>> -> ue_temporarily_unreachable;
            _ -> {unspecified, OptBin}
        end,
    {Opt, C};
parse_sgsap_opt(sgsap_iei_ss_code = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_tmsi = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_tmsi_status = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_tracking_area_identity = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_ue_time_zone = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_ue_emm_mode = Opt, OptBin) ->
    C = case OptBin of
            <<2#00000000:8>> -> emm_idle;
            <<2#00000001:8>> -> emm_connected;
            _ -> {reserved, OptBin}
        end,
    {Opt, C};
parse_sgsap_opt(sgsap_iei_vlr_name = Opt, OptBin) ->
    Chars = decode_name(OptBin),
    {Opt, Chars};
parse_sgsap_opt(sgsap_iei_channel_needed = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_emlpp_priority = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_additional_paging_indicators = Opt, OptBin) ->
    C = case OptBin of
            <<0:7, 2#1:1>> -> cs_restore_indicator_not_set;
            <<0:7, 2#0:1>> -> cs_restore_indicator_set
        end,
    {Opt, C};
parse_sgsap_opt(sgsap_iei_tmsi_based_nri_container = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_selected_cs_domain_operator = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_maximum_ue_availability_time = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_sm_delivery_start_time = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_additional_ue_unreachable_indicators = Opt, OptBin) ->
    C = case OptBin of
            <<0:7, 2#1:1>> -> sm_buffer_request_indicator_not_set;
            <<0:7, 2#0:1>> -> sm_buffer_request_indicator_set
        end,
    {Opt, C};
parse_sgsap_opt(sgsap_iei_maximum_retransmission_time = Opt, OptBin) ->
    {Opt, OptBin};
parse_sgsap_opt(sgsap_iei_requested_retransmission_time = Opt, OptBin) ->
    {Opt, OptBin}.

encode_sgsap_opt(sgsap_iei_cli = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_eps_location_update_type = Opt, Option) ->
    IEI = enc_iei(Opt),
    OptBin = case Option of
                 imsi_attach -> <<2#00000001:8>>;
                 normal_location_update -> <<2#00000010:8>>
             end,
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_erroneous_message = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_e_utran_cell_global_identity = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_global_cn_id = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_imeisv = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_imsi = Opt, Imsi) ->
    %% 3GPP TS 29.018 - 18.4.10
    IEI = enc_iei(Opt),
    OptBin = ossie_util:encode_tbcd(Imsi),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin:Length/binary>>;
encode_sgsap_opt(sgsap_iei_imsi_detach_from_eps_service_type = Opt, Option) ->
    IEI = enc_iei(Opt),
    OptBin = case Option of
                 network_initiated_imsi_detach_from_eps_services -> <<2#00000001:8>>;
                 ue_initiated_imsi_detach_from_eps_services -> <<2#00000010:8>>;
                 eps_services_not_allowed -> <<2#00000011:8>>;
                 {reserved, Bin} -> Bin
             end,
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin:Length/binary>>;
encode_sgsap_opt(sgsap_iei_imsi_detach_from_non_eps_service_type = Opt, Option) ->
    IEI = enc_iei(Opt),
    OptBin = case Option of
                 explicit_ue_initiated_imsi_detach_from_non_eps_services -> <<2#00000001:8>>;
                 combined_ue_initiated_imsi_detach_from_eps_and_non_eps_services -> <<2#00000010:8>>;
                 implicit_network_initiated_imsi_detach_from_eps_and_non_eps_services -> <<2#00000011:8>>;
                 {reserved, Bin} -> Bin
             end,
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin:Length/binary>>;
encode_sgsap_opt(sgsap_iei_lcs_client_identity = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_lcs_indicator = Opt, Option) ->
    IEI = enc_iei(Opt),
    OptBin = case Option of
                 mt_lr -> <<2#00000001:8>>;
                 {unspecified, Bin} -> Bin
             end,
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_location_area_identifier = Opt, {MCCMNC, Lac}) ->
    IEI = enc_iei(Opt),
    M = ossie_util:encode_mcc_mnc(MCCMNC),
    OptBin = <<M/binary, Lac/binary>>,
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_mm_information = Opt, MM) ->
    IEI = enc_iei(Opt),
    OptBin = encode_mm_information(MM),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_mme_name = Opt, Chars) ->
    IEI = enc_iei(Opt),
    OptBin = encode_name(Chars),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_mobile_identity = Opt, Option) ->
    case Option of
        {imsi, Ds} ->
            IsOdd = length(Ds) rem 2,
            Type = 2#001,
            <<_D1:4, _IsOdd:1, _Type:3, _R/binary>> = ossie_util:encode_tbcd([IsOdd*8+Type|Ds]);
        {imei, Ds} ->
            IsOdd = length(Ds) rem 2,
            Type = 2#010,
            <<_D1:4, _IsOdd:1, _Type:3, _R/binary>> = ossie_util:encode_tbcd([IsOdd*8+Type|Ds]);
        {imeisv, Ds} ->
            IsOdd = length(Ds) rem 2,
            Type = 2#011,
            <<_D1:4, _IsOdd:1, _Type:3, _R/binary>> = ossie_util:encode_tbcd([IsOdd*8+Type|Ds]);
        {tmsi, R} ->
            D1 = 0,
            IsOdd = 0,
            Type = 2#100,
            <<D1:4, IsOdd:1, Type:3, R/binary>>;
        {tmgi, Opts} ->
            IsOdd = 0,
            Type = 2#101,
            MBMSServiceId = proplists:get_value(mbms_service_id, Opts),
            {O1, V1} = case proplists:is_defined(mcc_mnc, Opts) of
                           true ->
                               MCCMNC = proplists:get_value(mcc_mnc, Opts),
                               EncMCCMNC = ossie_util:encode_mcc_mnc(MCCMNC),
                               {2#01, <<EncMCCMNC:3/binary>>};
                           false ->
                               {2#00, <<>>}
                       end,
            {O2, V2} = case proplists:is_defined(mbms_session_id, Opts) of
                           true ->
                               SessionId = proplists:get_value(mbms_session_id, Opts),
                               {2#10, <<SessionId:1/binary>>};
                           false ->
                               {2#00, <<>>}
                       end,
            Options = O1+O2,
            Extras = <<V1, V2>>,
            <<0:2, Options:2, IsOdd:1, Type:3, MBMSServiceId:3/binary, Extras/binary>>;
        no_identity ->
            D1 = 0,
            IsOdd = 0,
            Type = 2#000,
            R = <<>>,
            <<D1:4, IsOdd:1, Type:3, R/binary>>
    end;
encode_sgsap_opt(sgsap_iei_mobile_station_classmark_2 = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_nas_message_container = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_reject_cause = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_service_indicator = Opt, Option) ->
    IEI = enc_iei(Opt),
    OptBin = case Option of
                 cs_call_indicator -> <<2#00000001:8>>;
                 sms_indicator -> <<2#00000010:8>>
             end,
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_sgs_cause = Opt, Option) ->
    IEI = enc_iei(Opt),
    OptBin = case Option of
                 imsi_detached_for_eps_services -> <<2#00000001:8>>;
                 imsi_detached_for_eps_and_non_eps_services -> <<2#00000010:8>>;
                 imsi_unknown -> <<2#00000011:8>>;
                 imsi_detached_for_non_eps_services -> <<2#00000100:8>>;
                 imsi_implicitly_detached_for_non_eps_services -> <<2#00000101:8>>;
                 ue_unreachable -> <<2#00000110:8>>;
                 message_not_compatible_with_the_protocol_state -> <<2#00000111:8>>;
                 missing_mandatory_information_element -> <<2#00001000:8>>;
                 invalid_mandatory_information -> <<2#00001001:8>>;
                 conditional_information_element_error -> <<2#00001010:8>>;
                 semantically_incorrect_message -> <<2#00001011:8>>;
                 message_unknown -> <<2#00001100:8>>;
                 mobile_terminating_cs_fallback_call_rejected_by_the_user -> <<2#00001101:8>>;
                 ue_temporarily_unreachable -> <<2#00001110:8>>;
                 {unspecified, Bin} -> Bin
             end,
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_ss_code = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_tmsi = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_tmsi_status = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_tracking_area_identity = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_ue_time_zone = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_ue_emm_mode = Opt, Option) ->
    IEI = enc_iei(Opt),
    OptBin = case Option of
                 emm_idle -> <<2#00000000:8>>;
                 emm_connected -> <<2#00000001:8>>;
                 {reserved, Bin} -> Bin
             end,
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_vlr_name = Opt, Chars) ->
    IEI = enc_iei(Opt),
    OptBin = encode_name(Chars),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_channel_needed = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_emlpp_priority = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_additional_paging_indicators = Opt, Option) ->
    IEI = enc_iei(Opt),
    OptBin = case Option of
                 cs_restore_indicator_not_set -> <<0:7, 2#1:1>>;
                 cs_restore_indicator_set -> <<0:7, 2#0:1>>
             end,
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_tmsi_based_nri_container = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_selected_cs_domain_operator = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_maximum_ue_availability_time = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_sm_delivery_start_time = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_additional_ue_unreachable_indicators = Opt, Option) ->
    IEI = enc_iei(Opt),
    OptBin = case Option of
                 sm_buffer_request_indicator_not_set -> <<0:7, 2#1:1>>;
                 sm_buffer_request_indicator_set -> <<0:7, 2#0:1>>
             end,
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_maximum_retransmission_time = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>;
encode_sgsap_opt(sgsap_iei_requested_retransmission_time = Opt, OptBin) ->
    IEI = enc_iei(Opt),
    Length = byte_size(OptBin),
    <<IEI, Length, OptBin>>.

encode_sgsap_msg(#sgsap_msg{msg_type = MsgT, payload = Params}) ->
    MsgType = enc_msg_type(MsgT),
    OptList = encode_sgsap_params(Params),
    OptBin = encode_sgsap_opts(OptList),
    <<MsgType:8, OptBin/binary>>.

encode_sgsap_params(#sgsap_msg_params_alert_ack{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_alert_ack.imsi}];
encode_sgsap_params(#sgsap_msg_params_alert_reject{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_alert_reject.imsi},
     {sgsap_iei_sgs_cause, Param#sgsap_msg_params_alert_reject.sgs_cause}];
encode_sgsap_params(#sgsap_msg_params_alert_request{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_alert_request.imsi}];
encode_sgsap_params(#sgsap_msg_params_downlink_unitdata{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_downlink_unitdata.imsi},
     {sgsap_iei_nas_message_container, Param#sgsap_msg_params_downlink_unitdata.nas_message_container}];
encode_sgsap_params(#sgsap_msg_params_eps_detach_ack{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_eps_detach_ack.imsi}];
encode_sgsap_params(#sgsap_msg_params_eps_detach_indication{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_eps_detach_indication.imsi},
     {sgsap_iei_mme_name, Param#sgsap_msg_params_eps_detach_indication.mme_name},
     {sgsap_iei_imsi_detach_from_eps_service_type, Param#sgsap_msg_params_eps_detach_indication.imsi_detach_from_eps_service_type}];
encode_sgsap_params(#sgsap_msg_params_imsi_detach_ack{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_imsi_detach_ack.imsi}];
encode_sgsap_params(#sgsap_msg_params_imsi_detach_indication{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_imsi_detach_indication.imsi},
     {sgsap_iei_mme_name, Param#sgsap_msg_params_imsi_detach_indication.mme_name},
     {sgsap_iei_imsi_detach_from_non_eps_service_type, Param#sgsap_msg_params_imsi_detach_indication.imsi_detach_from_non_eps_service_type}];
encode_sgsap_params(#sgsap_msg_params_location_update_accept{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_location_update_accept.imsi},
     {sgsap_iei_location_area_identifier, Param#sgsap_msg_params_location_update_accept.location_area_identifier},
     {sgsap_iei_mobile_identity, Param#sgsap_msg_params_location_update_accept.mobile_identity}];
encode_sgsap_params(#sgsap_msg_params_location_update_reject{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_location_update_reject.imsi},
     {sgsap_iei_reject_cause, Param#sgsap_msg_params_location_update_reject.reject_cause},
     {sgsap_iei_location_area_identifier, Param#sgsap_msg_params_location_update_reject.location_area_identifier}];
encode_sgsap_params(#sgsap_msg_params_location_update_request{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_location_update_request.imsi},
     {sgsap_iei_mme_name, Param#sgsap_msg_params_location_update_request.mme_name},
     {sgsap_iei_eps_location_update_type, Param#sgsap_msg_params_location_update_request.eps_location_update_type},
     {sgsap_iei_location_area_identifier, Param#sgsap_msg_params_location_update_request.new_location_area_identifier},
     {sgsap_iei_location_area_identifier, Param#sgsap_msg_params_location_update_request.old_location_area_identifier},
     {sgsap_iei_tmsi_status, Param#sgsap_msg_params_location_update_request.tmsi_status},
     {sgsap_iei_imeisv, Param#sgsap_msg_params_location_update_request.imeisv},
     {sgsap_iei_tracking_area_identity, Param#sgsap_msg_params_location_update_request.tracking_area_identity},
     {sgsap_iei_e_utran_cell_global_identity, Param#sgsap_msg_params_location_update_request.e_utran_cell_global_identity},
     {sgsap_iei_tmsi_based_nri_container, Param#sgsap_msg_params_location_update_request.tmsi_based_nri_container},
     {sgsap_iei_selected_cs_domain_operator, Param#sgsap_msg_params_location_update_request.selected_cs_domain_operator}];
encode_sgsap_params(#sgsap_msg_params_mm_information_request{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_mm_information_request.imsi},
     {sgsap_iei_mm_information, Param#sgsap_msg_params_mm_information_request.mm_information}];
encode_sgsap_params(#sgsap_msg_params_paging_reject{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_paging_reject.imsi},
     {sgsap_iei_sgs_clause, Param#sgsap_msg_params_paging_reject.sgs_cause}];
encode_sgsap_params(#sgsap_msg_params_paging_request{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_paging_request.imsi},
     {sgsap_iei_vlr_name, Param#sgsap_msg_params_paging_request.vlr_name},
     {sgsap_iei_service_indicator, Param#sgsap_msg_params_paging_request.service_indicator},
     {sgsap_iei_tmsi, Param#sgsap_msg_params_paging_request.tmsi},
     {sgsap_iei_cli, Param#sgsap_msg_params_paging_request.cli},
     {sgsap_iei_ss_code, Param#sgsap_msg_params_paging_request.ss_code},
     {sgsap_iei_lcs_indicator, Param#sgsap_msg_params_paging_request.lcs_indicator},
     {sgsap_iei_lcs_client_identity, Param#sgsap_msg_params_paging_request.lcs_client_identity},
     {sgsap_iei_channel_needed, Param#sgsap_msg_params_paging_request.channel_needed},
     {sgsap_iei_emlpp_priority, Param#sgsap_msg_params_paging_request.emlpp_priority},
     {sgsap_iei_additional_paging_indicators, Param#sgsap_msg_params_paging_request.additional_paging_indicators},
     {sgsap_iei_sm_delivery_timer, Param#sgsap_msg_params_paging_request.sm_delivery_timer},
     {sgsap_iei_sm_delivery_start_time, Param#sgsap_msg_params_paging_request.sm_delivery_start_time},
     {sgsap_iei_maximum_retransmission_time, Param#sgsap_msg_params_paging_request.maximum_retransmission_time}];
encode_sgsap_params(#sgsap_msg_params_reset_ack{} = Param) ->
    [{sgsap_iei_mme_name, Param#sgsap_msg_params_reset_ack.mme_name},
     {sgsap_iei_vlr_name, Param#sgsap_msg_params_reset_ack.vlr_name}];
encode_sgsap_params(#sgsap_msg_params_reset_indication{} = Param) ->
    [{sgsap_iei_mme_name, Param#sgsap_msg_params_reset_indication.mme_name},
     {sgsap_iei_vlr_name, Param#sgsap_msg_params_reset_indication.vlr_name}];
encode_sgsap_params(#sgsap_msg_params_service_request{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_service_request.imsi},
     {sgsap_iei_service_indicator, Param#sgsap_msg_params_service_request.service_indicator},
     {sgsap_iei_imeisv, Param#sgsap_msg_params_service_request.imeisv},
     {sgsap_iei_ue_time_zone, Param#sgsap_msg_params_service_request.ue_time_zone},
     {sgsap_iei_mobile_station_classmark_2, Param#sgsap_msg_params_service_request.mobile_station_classmark_2},
     {sgsap_iei_tracking_area_identity, Param#sgsap_msg_params_service_request.tracking_area_identity},
     {sgsap_iei_e_utran_cell_global_identity, Param#sgsap_msg_params_service_request.e_utran_cell_global_identity},
     {sgsap_iei_ue_emm_mode, Param#sgsap_msg_params_service_request.ue_emm_mode}];
encode_sgsap_params(#sgsap_msg_params_status{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_status.imsi},
     {sgsap_iei_sgs_clause, Param#sgsap_msg_params_status.sgs_cause},
     {sgsap_iei_erroneous_message, Param#sgsap_msg_params_status.erroneous_message}];
encode_sgsap_params(#sgsap_msg_params_tmsi_reallocation_complete{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_tmsi_reallocation_complete.imsi}];
encode_sgsap_params(#sgsap_msg_params_ue_activity_indication{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_ue_activity_indication.imsi},
     {sgsap_iei_maximum_ue_availability_time, Param#sgsap_msg_params_ue_activity_indication.maximum_ue_availability_time}];
encode_sgsap_params(#sgsap_msg_params_ue_unreachable{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_ue_unreachable.imsi},
     {sgsap_iei_sgs_clause, Param#sgsap_msg_params_ue_unreachable.sgs_cause},
     {sgsap_iei_requested_retransmission_time, Param#sgsap_msg_params_ue_unreachable.requested_retransmission_time},
     {sgsap_iei_additional_ue_unreachable_indicators, Param#sgsap_msg_params_ue_unreachable.additional_ue_unreachable_indicators}];
encode_sgsap_params(#sgsap_msg_params_uplink_unitdata{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_uplink_unitdata.imsi},
     {sgsap_iei_nas_message_container, Param#sgsap_msg_params_uplink_unitdata.nas_message_container},
     {sgsap_iei_imeisv, Param#sgsap_msg_params_uplink_unitdata.imeisv},
     {sgsap_iei_ue_time_zone, Param#sgsap_msg_params_uplink_unitdata.ue_time_zone},
     {sgsap_iei_mobile_station_classmark_2, Param#sgsap_msg_params_uplink_unitdata.mobile_station_classmark_2},
     {sgsap_iei_tracking_area_identity, Param#sgsap_msg_params_uplink_unitdata.tracking_area_identity},
     {sgsap_iei_e_utran_cell_global_identity, Param#sgsap_msg_params_uplink_unitdata.e_utran_cell_global_identity}];
encode_sgsap_params(#sgsap_msg_params_release_request{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_release_request.imsi},
     {sgsap_iei_sgs_clause, Param#sgsap_msg_params_release_request.sgs_cause}];
encode_sgsap_params(#sgsap_msg_params_service_abort_request{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_service_abort_request.imsi}];
encode_sgsap_params(#sgsap_msg_params_mo_csfb_indication{} = Param) ->
    [{sgsap_iei_imsi, Param#sgsap_msg_params_mo_csfb_indication.imsi},
     {sgsap_iei_tracking_area_identity, Param#sgsap_msg_params_mo_csfb_indication.tracking_area_identity},
     {sgsap_iei_e_utran_cell_global_identity, Param#sgsap_msg_params_mo_csfb_indication.e_utran_cell_global_identity}];
encode_sgsap_params(OptList) when is_list(OptList) ->
    OptList.

encode_sgsap_opts(OptList) when is_list(OptList) ->
    encode_sgsap_opts(OptList, <<>>).

encode_sgsap_opts([], OptBin) when is_binary(OptBin) ->
    OptBin;
encode_sgsap_opts([{_, undefined}|OptList], OptBin) when is_list(OptList), is_binary(OptBin) ->
    encode_sgsap_opts(OptList, OptBin);
encode_sgsap_opts([{K, V}|OptList], OptBin) when is_list(OptList), is_binary(OptBin) ->
    IEI = enc_iei(K),
    CurOpt = encode_sgsap_opt(K, V),
    Length = byte_size(CurOpt),
    Remain = <<CurOpt:Length/binary, OptBin/binary>>,
    Remain2 = <<IEI:8/big, Length:8/big, Remain/binary>>,
    encode_sgsap_opts(OptList, Remain2).

decode_name(Bin) ->
    lists:flatten(lists:join(".", decode_name(Bin, []))).

decode_name(<<>>, Acc) ->
    lists:reverse(Acc);
decode_name(Bin, Acc) ->
    <<L:8, R/binary>> = Bin,
    <<C:L/binary, Rest/binary>> = R,
    decode_name(Rest, [binary_to_list(C)|Acc]).

encode_name(String) ->
    Groups = string:split(String, ".", all),
    encode_name(Groups, <<>>).

encode_name([], Acc) ->
    Acc;
encode_name([L|Rest], Acc) ->
    Bin = list_to_binary(L),
    Length = byte_size(Bin),
    encode_name(Rest, <<Length, Bin, Acc>>).


-define(MM_IEI_FULL_NAME_FOR_NETWORK, 16#43).
-define(MM_IEI_SHORT_NAME_FOR_NETWORK, 16#45).
-define(MM_IEI_LOCAL_TIME_ZONE, 16#46).
-define(MM_IEI_UNIVERSAL_TIME_AND_LOCAL_TIME_ZONE , 16#47).
-define(MM_IEI_LSA_IDENTITY, 16#48).
-define(MM_IEI_NETWORK_DAYLIGHT_SAVING_TIME, 16#49).

decode_mm_information(<<>>) ->
    [];
decode_mm_information(<<?MM_IEI_FULL_NAME_FOR_NETWORK:8, L:8, R/binary>>) ->
    <<Name:L/binary, Rest/binary>> = R,
    [{full_name_for_network, decode_network_name(Name)}|decode_mm_information(Rest)];
decode_mm_information(<<?MM_IEI_SHORT_NAME_FOR_NETWORK:8, L:8, R/binary>>) ->
    <<Name:L/binary, Rest/binary>> = R,
    [{short_name_for_network, decode_network_name(Name)}|decode_mm_information(Rest)];
decode_mm_information(<<?MM_IEI_LOCAL_TIME_ZONE:8, Tz:1/binary, R/binary>>) ->
    [{local_time_zone, Tz}|decode_mm_information(R)];
decode_mm_information(<<?MM_IEI_UNIVERSAL_TIME_AND_LOCAL_TIME_ZONE:8, Time:7/binary, R/binary>>) ->
    %% 3GPP TS 23.040 - 9.2.3.11 TP-Service-Centre-Time-Stamp (TP-SCTS)
    io:format(user, "WHA: ~p~n", [Time]),
    <<Y2:4, Y1:4, M2:4, M1:4, D2:4, D1:4, H2:4, H1:4, Mi2:4, Mi1:4, S2:4, S1:4, TzRaw/binary>> = Time,
    Y = 2000+Y1*10+Y2,
    M = M1*10+M2,
    D = D1*10+D2,
    H = H1*10+H2,
    Mi = Mi1*10+Mi2,
    S = S1*10+S2,
    <<Tz2:4, TzSign:1, Tz1:3>> = TzRaw,
    <<TzQuarters>> = <<0:1, Tz1:3, Tz2:4>>,
    Tz = case TzSign of
             1 -> [$-, TzQuarters];
             0 -> [$+, TzQuarters]
         end,
    [{universal_time_zone_and_local_time_zone, {{{Y, M, D}, {H, Mi, S}}, Tz}}|decode_mm_information(R)];
decode_mm_information(<<?MM_IEI_LSA_IDENTITY:8, L:8, R/binary>>) ->
    <<Identity:L/binary, Rest/binary>> = R,
    [{lsa_identity, Identity}|decode_mm_information(Rest)];
decode_mm_information(<<?MM_IEI_NETWORK_DAYLIGHT_SAVING_TIME:8, L:8, R/binary>>) ->
    <<DST:L/binary, Rest/binary>> = R,
    [{network_daylight_saving_time, DST}|decode_mm_information(Rest)].

encode_mm_information([]) ->
    <<>>;
encode_mm_information([{full_name_for_network, FullName}|Rest]) ->
    EncName = encode_network_name(FullName),
    Length = byte_size(EncName),
    <<?MM_IEI_FULL_NAME_FOR_NETWORK:8, Length:8, EncName, (encode_mm_information(Rest))/binary>>;
encode_mm_information(<<?MM_IEI_SHORT_NAME_FOR_NETWORK:8, L:8, R/binary>>) ->
    <<Name:L/binary, Rest/binary>> = R,
    [{short_name_for_network, decode_network_name(Name)}|encode_mm_information(Rest)];
encode_mm_information(<<?MM_IEI_LOCAL_TIME_ZONE:8, Tz:1/binary, R/binary>>) ->
    [{local_time_zone, Tz}|encode_mm_information(R)];
encode_mm_information(<<?MM_IEI_UNIVERSAL_TIME_AND_LOCAL_TIME_ZONE:8, Time:7/binary, R/binary>>) ->
    %% 3GPP TS 23.040 - 9.2.3.11 TP-Service-Centre-Time-Stamp (TP-SCTS)
    io:format(user, "WHA: ~p~n", [Time]),
    <<Y2:4, Y1:4, M2:4, M1:4, D2:4, D1:4, H2:4, H1:4, Mi2:4, Mi1:4, S2:4, S1:4, TzRaw/binary>> = Time,
    Y = 2000+Y1*10+Y2,
    M = M1*10+M2,
    D = D1*10+D2,
    H = H1*10+H2,
    Mi = Mi1*10+Mi2,
    S = S1*10+S2,
    <<Tz2:4, TzSign:1, Tz1:3>> = TzRaw,
    <<TzQuarters>> = <<0:1, Tz1:3, Tz2:4>>,
    Tz = case TzSign of
             1 -> [$-, TzQuarters];
             0 -> [$+, TzQuarters]
         end,
    [{universal_time_zone_and_local_time_zone, {{{Y, M, D}, {H, Mi, S}}, Tz}}|encode_mm_information(R)];
encode_mm_information(<<?MM_IEI_LSA_IDENTITY:8, L:8, R/binary>>) ->
    <<Identity:L/binary, Rest/binary>> = R,
    [{lsa_identity, Identity}|encode_mm_information(Rest)];
encode_mm_information(<<?MM_IEI_NETWORK_DAYLIGHT_SAVING_TIME:8, L:8, R/binary>>) ->
    <<DST:L/binary, Rest/binary>> = R,
    [{network_daylight_saving_time, DST}|encode_mm_information(Rest)].


decode_network_name(Bin) ->
    <<_Ext:1, CodingScheme:3, _AddCI:1, NumSpares:3, Rest/binary>> = Bin,
    case CodingScheme of
        0 -> %% GSM default alphabet, language unspecified, 3GPP TS 23.038
            ossie_util:decode_gsm_string(Rest, NumSpares);
        1 -> %% UCS2 (16 bit)
            Bin;
        _ -> %% reserved
            Bin
    end.

encode_network_name(Bin) when is_binary(Bin) ->
    Bin;
encode_network_name(GsmString) when is_list(GsmString) ->
    NumSpares = length(GsmString) rem 7,
    Bin = ossie_util:encode_gsm_string(GsmString, NumSpares),
    CodingScheme = 0, %% GSM - only one we know right now
    <<0:1, CodingScheme:3, 0:1, NumSpares:3, Bin/binary>>.

parse_msg_type(?SGSAP_MSGT_PAGING_REQUEST) -> sgsap_msgt_paging_request;
parse_msg_type(?SGSAP_MSGT_PAGING_REJECT) -> sgsap_msgt_paging_reject;
parse_msg_type(?SGSAP_MSGT_SERVICE_REQUEST) -> sgsap_msgt_service_request;
parse_msg_type(?SGSAP_MSGT_DOWNLINK_UNITDATA) -> sgsap_msgt_downlink_unitdata;
parse_msg_type(?SGSAP_MSGT_UPLINK_UNITDATA) -> sgsap_msgt_uplink_unitdata;
parse_msg_type(?SGSAP_MSGT_LOCATION_UPDATE_REQUEST) -> sgsap_msgt_location_update_request;
parse_msg_type(?SGSAP_MSGT_LOCATION_UPDATE_ACCEPT) -> sgsap_msgt_location_update_accept;
parse_msg_type(?SGSAP_MSGT_LOCATION_UPDATE_REJECT) -> sgsap_msgt_location_update_reject;
parse_msg_type(?SGSAP_MSGT_TMSI_REALLOCATION_COMPLETE) -> sgsap_msgt_tmsi_reallocation_complete;
parse_msg_type(?SGSAP_MSGT_ALERT_REQUEST) -> sgsap_msgt_alert_request;
parse_msg_type(?SGSAP_MSGT_ALERT_ACK) -> sgsap_msgt_alert_ack;
parse_msg_type(?SGSAP_MSGT_ALERT_REJECT) -> sgsap_msgt_alert_reject;
parse_msg_type(?SGSAP_MSGT_UE_ACTIVITY_INDICATION) -> sgsap_msgt_ue_activity_indication;
parse_msg_type(?SGSAP_MSGT_EPS_DETACH_INDICATION) -> sgsap_msgt_eps_detach_indication;
parse_msg_type(?SGSAP_MSGT_EPS_DETACH_ACK) -> sgsap_msgt_eps_detach_ack;
parse_msg_type(?SGSAP_MSGT_IMSI_DETACH_INDICATION) -> sgsap_msgt_imsi_detach_indication;
parse_msg_type(?SGSAP_MSGT_IMSI_DETACH_ACK) -> sgsap_msgt_imsi_detach_ack;
parse_msg_type(?SGSAP_MSGT_RESET_INDICATION) -> sgsap_msgt_reset_indication;
parse_msg_type(?SGSAP_MSGT_RESET_ACK) -> sgsap_msgt_reset_ack;
parse_msg_type(?SGSAP_MSGT_SERVICE_ABORT_REQUEST) -> sgsap_msgt_service_abort_request;
parse_msg_type(?SGSAP_MSGT_MO_CSFB_INDICATION) -> sgsap_msgt_mo_csfb_indication;
parse_msg_type(?SGSAP_MSGT_MM_INFORMATION_REQUEST) -> sgsap_msgt_mm_information_request;
parse_msg_type(?SGSAP_MSGT_RELEASE_REQUEST) -> sgsap_msgt_release_request;
parse_msg_type(?SGSAP_MSGT_STATUS) -> sgsap_msgt_status;
parse_msg_type(?SGSAP_MSGT_UE_UNREACHABLE) -> sgsap_msgt_ue_unreachable;
parse_msg_type(MsgT) -> {sgsap_msgt_unknown, MsgT}.

enc_msg_type(sgsap_msgt_paging_request) -> ?SGSAP_MSGT_PAGING_REQUEST;
enc_msg_type(sgsap_msgt_paging_reject) -> ?SGSAP_MSGT_PAGING_REJECT;
enc_msg_type(sgsap_msgt_service_request) -> ?SGSAP_MSGT_SERVICE_REQUEST;
enc_msg_type(sgsap_msgt_downlink_unitdata) -> ?SGSAP_MSGT_DOWNLINK_UNITDATA;
enc_msg_type(sgsap_msgt_uplink_unitdata) -> ?SGSAP_MSGT_UPLINK_UNITDATA;
enc_msg_type(sgsap_msgt_location_update_request) -> ?SGSAP_MSGT_LOCATION_UPDATE_REQUEST;
enc_msg_type(sgsap_msgt_location_update_accept) -> ?SGSAP_MSGT_LOCATION_UPDATE_ACCEPT;
enc_msg_type(sgsap_msgt_location_update_reject) -> ?SGSAP_MSGT_LOCATION_UPDATE_REJECT;
enc_msg_type(sgsap_msgt_tmsi_reallocation_complete) -> ?SGSAP_MSGT_TMSI_REALLOCATION_COMPLETE;
enc_msg_type(sgsap_msgt_alert_request) -> ?SGSAP_MSGT_ALERT_REQUEST;
enc_msg_type(sgsap_msgt_alert_ack) -> ?SGSAP_MSGT_ALERT_ACK;
enc_msg_type(sgsap_msgt_alert_reject) -> ?SGSAP_MSGT_ALERT_REJECT;
enc_msg_type(sgsap_msgt_ue_activity_indication) -> ?SGSAP_MSGT_UE_ACTIVITY_INDICATION;
enc_msg_type(sgsap_msgt_eps_detach_indication) -> ?SGSAP_MSGT_EPS_DETACH_INDICATION;
enc_msg_type(sgsap_msgt_eps_detach_ack) -> ?SGSAP_MSGT_EPS_DETACH_ACK;
enc_msg_type(sgsap_msgt_imsi_detach_indication) -> ?SGSAP_MSGT_IMSI_DETACH_INDICATION;
enc_msg_type(sgsap_msgt_imsi_detach_ack) -> ?SGSAP_MSGT_IMSI_DETACH_ACK;
enc_msg_type(sgsap_msgt_reset_indication) -> ?SGSAP_MSGT_RESET_INDICATION;
enc_msg_type(sgsap_msgt_reset_ack) -> ?SGSAP_MSGT_RESET_ACK;
enc_msg_type(sgsap_msgt_service_abort_request) -> ?SGSAP_MSGT_SERVICE_ABORT_REQUEST;
enc_msg_type(sgsap_msgt_mo_csfb_indication) -> ?SGSAP_MSGT_MO_CSFB_INDICATION;
enc_msg_type(sgsap_msgt_mm_information_request) -> ?SGSAP_MSGT_MM_INFORMATION_REQUEST;
enc_msg_type(sgsap_msgt_release_request) -> ?SGSAP_MSGT_RELEASE_REQUEST;
enc_msg_type(sgsap_msgt_status) -> ?SGSAP_MSGT_STATUS;
enc_msg_type(sgsap_msgt_ue_unreachable) -> ?SGSAP_MSGT_UE_UNREACHABLE;
enc_msg_type({sgsap_msgt_unknown, MsgT}) -> MsgT.


dec_iei(?SGSAP_IEI_IMSI) -> sgsap_iei_imsi;
dec_iei(?SGSAP_IEI_VLR_NAME) -> sgsap_iei_vlr_name;
dec_iei(?SGSAP_IEI_TMSI) -> sgsap_iei_tmsi;
dec_iei(?SGSAP_IEI_LOCATION_AREA_IDENTIFIER) -> sgsap_iei_location_area_identifier;
dec_iei(?SGSAP_IEI_CHANNEL_NEEDED) -> sgsap_iei_channel_needed;
dec_iei(?SGSAP_IEI_EMLPP_PRIORITY) -> sgsap_iei_emlpp_priority;
dec_iei(?SGSAP_IEI_TMSI_STATUS) -> sgsap_iei_tmsi_status;
dec_iei(?SGSAP_IEI_SGS_CAUSE) -> sgsap_iei_sgs_cause;
dec_iei(?SGSAP_IEI_MME_NAME) -> sgsap_iei_mme_name;
dec_iei(?SGSAP_IEI_EPS_LOCATION_UPDATE_TYPE) -> sgsap_iei_eps_location_update_type;
dec_iei(?SGSAP_IEI_GLOBAL_CN_ID) -> sgsap_iei_global_cn_id;
dec_iei(?SGSAP_IEI_MOBILE_IDENTITY) -> sgsap_iei_mobile_identity;
dec_iei(?SGSAP_IEI_REJECT_CAUSE) -> sgsap_iei_reject_cause;
dec_iei(?SGSAP_IEI_IMSI_DETACH_FROM_EPS_SERVICE_TYPE) -> sgsap_iei_imsi_detach_from_eps_service_type;
dec_iei(?SGSAP_IEI_IMSI_DETACH_FROM_NON_EPS_SERVICE_TYPE) -> sgsap_iei_imsi_detach_from_non_eps_service_type;
dec_iei(?SGSAP_IEI_IMEISV) -> sgsap_iei_imeisv;
dec_iei(?SGSAP_IEI_NAS_MESSAGE_CONTAINER) -> sgsap_iei_nas_message_container;
dec_iei(?SGSAP_IEI_MM_INFORMATION) -> sgsap_iei_mm_information;
dec_iei(?SGSAP_IEI_ERRONEOUS_MESSAGE) -> sgsap_iei_erroneous_message;
dec_iei(?SGSAP_IEI_CLI) -> sgsap_iei_cli;
dec_iei(?SGSAP_IEI_LCS_CLIENT_IDENTITY) -> sgsap_iei_lcs_client_identity;
dec_iei(?SGSAP_IEI_LCS_INDICATOR) -> sgsap_iei_lcs_indicator;
dec_iei(?SGSAP_IEI_SS_CODE) -> sgsap_iei_ss_code;
dec_iei(?SGSAP_IEI_SERVICE_INDICATOR) -> sgsap_iei_service_indicator;
dec_iei(?SGSAP_IEI_UE_TIME_ZONE) -> sgsap_iei_ue_time_zone;
dec_iei(?SGSAP_IEI_MOBILE_STATION_CLASSMARK_2) -> sgsap_iei_mobile_station_classmark_2;
dec_iei(?SGSAP_IEI_TRACKING_AREA_IDENTITY) -> sgsap_iei_tracking_area_identity;
dec_iei(?SGSAP_IEI_E_UTRAN_CELL_GLOBAL_IDENTITY) -> sgsap_iei_e_utran_cell_global_identity;
dec_iei(?SGSAP_IEI_UE_EMM_MODE) -> sgsap_iei_ue_emm_mode;
dec_iei(?SGSAP_IEI_ADDITIONAL_PAGING_INDICATORS) -> sgsap_iei_additional_paging_indicators;
dec_iei(?SGSAP_IEI_TMSI_BASED_NRI_CONTAINER) -> sgsap_iei_tmsi_based_nri_container;
dec_iei(?SGSAP_IEI_SELECTED_CS_DOMAIN_OPERATOR) -> sgsap_iei_selected_cs_domain_operator;
dec_iei(?SGSAP_IEI_MAXIMUM_UE_AVAILABILITY_TIME) -> sgsap_iei_maximum_ue_availability_time;
dec_iei(?SGSAP_IEI_SM_DELIVERY_TIMER) -> sgsap_iei_sm_delivery_timer;
dec_iei(?SGSAP_IEI_SM_DELIVERY_START_TIME) -> sgsap_iei_sm_delivery_start_time;
dec_iei(?SGSAP_IEI_ADDITIONAL_UE_UNREACHABLE_INDICATORS) -> sgsap_iei_additional_ue_unreachable_indicators;
dec_iei(?SGSAP_IEI_MAXIMUM_RETRANSMISSION_TIME) -> sgsap_iei_maximum_retransmission_time;
dec_iei(?SGSAP_IEI_REQUESTED_RETRANSMISSION_TIME) -> sgsap_iei_requested_retransmission_time.

enc_iei(sgsap_iei_imsi) -> ?SGSAP_IEI_IMSI;
enc_iei(sgsap_iei_vlr_name) -> ?SGSAP_IEI_VLR_NAME;
enc_iei(sgsap_iei_tmsi) -> ?SGSAP_IEI_TMSI;
enc_iei(sgsap_iei_location_area_identifier) -> ?SGSAP_IEI_LOCATION_AREA_IDENTIFIER;
enc_iei(sgsap_iei_channel_needed) -> ?SGSAP_IEI_CHANNEL_NEEDED;
enc_iei(sgsap_iei_emlpp_priority) -> ?SGSAP_IEI_EMLPP_PRIORITY;
enc_iei(sgsap_iei_tmsi_status) -> ?SGSAP_IEI_TMSI_STATUS;
enc_iei(sgsap_iei_sgs_cause) -> ?SGSAP_IEI_SGS_CAUSE;
enc_iei(sgsap_iei_mme_name) -> ?SGSAP_IEI_MME_NAME;
enc_iei(sgsap_iei_eps_location_update_type) -> ?SGSAP_IEI_EPS_LOCATION_UPDATE_TYPE;
enc_iei(sgsap_iei_global_cn_id) -> ?SGSAP_IEI_GLOBAL_CN_ID;
enc_iei(sgsap_iei_mobile_identity) -> ?SGSAP_IEI_MOBILE_IDENTITY;
enc_iei(sgsap_iei_reject_cause) -> ?SGSAP_IEI_REJECT_CAUSE;
enc_iei(sgsap_iei_imsi_detach_from_eps_service_type) -> ?SGSAP_IEI_IMSI_DETACH_FROM_EPS_SERVICE_TYPE;
enc_iei(sgsap_iei_imsi_detach_from_non_eps_service_type) -> ?SGSAP_IEI_IMSI_DETACH_FROM_NON_EPS_SERVICE_TYPE;
enc_iei(sgsap_iei_imeisv) -> ?SGSAP_IEI_IMEISV;
enc_iei(sgsap_iei_nas_message_container) -> ?SGSAP_IEI_NAS_MESSAGE_CONTAINER;
enc_iei(sgsap_iei_mm_information) -> ?SGSAP_IEI_MM_INFORMATION;
enc_iei(sgsap_iei_erroneous_message) -> ?SGSAP_IEI_ERRONEOUS_MESSAGE;
enc_iei(sgsap_iei_cli) -> ?SGSAP_IEI_CLI;
enc_iei(sgsap_iei_lcs_client_identity) -> ?SGSAP_IEI_LCS_CLIENT_IDENTITY;
enc_iei(sgsap_iei_lcs_indicator) -> ?SGSAP_IEI_LCS_INDICATOR;
enc_iei(sgsap_iei_ss_code) -> ?SGSAP_IEI_SS_CODE;
enc_iei(sgsap_iei_service_indicator) -> ?SGSAP_IEI_SERVICE_INDICATOR;
enc_iei(sgsap_iei_ue_time_zone) -> ?SGSAP_IEI_UE_TIME_ZONE;
enc_iei(sgsap_iei_mobile_station_classmark_2) -> ?SGSAP_IEI_MOBILE_STATION_CLASSMARK_2;
enc_iei(sgsap_iei_tracking_area_identity) -> ?SGSAP_IEI_TRACKING_AREA_IDENTITY;
enc_iei(sgsap_iei_e_utran_cell_global_identity) -> ?SGSAP_IEI_E_UTRAN_CELL_GLOBAL_IDENTITY;
enc_iei(sgsap_iei_ue_emm_mode) -> ?SGSAP_IEI_UE_EMM_MODE;
enc_iei(sgsap_iei_additional_paging_indicators) -> ?SGSAP_IEI_ADDITIONAL_PAGING_INDICATORS;
enc_iei(sgsap_iei_tmsi_based_nri_container) -> ?SGSAP_IEI_TMSI_BASED_NRI_CONTAINER;
enc_iei(sgsap_iei_selected_cs_domain_operator) -> ?SGSAP_IEI_SELECTED_CS_DOMAIN_OPERATOR;
enc_iei(sgsap_iei_maximum_ue_availability_time) -> ?SGSAP_IEI_MAXIMUM_UE_AVAILABILITY_TIME;
enc_iei(sgsap_iei_sm_delivery_timer) -> ?SGSAP_IEI_SM_DELIVERY_TIMER;
enc_iei(sgsap_iei_sm_delivery_start_time) -> ?SGSAP_IEI_SM_DELIVERY_START_TIME;
enc_iei(sgsap_iei_additional_ue_unreachable_indicators) -> ?SGSAP_IEI_ADDITIONAL_UE_UNREACHABLE_INDICATORS;
enc_iei(sgsap_iei_maximum_retransmission_time) -> ?SGSAP_IEI_MAXIMUM_RETRANSMISSION_TIME;
enc_iei(sgsap_iei_requested_retransmission_time) -> ?SGSAP_IEI_REQUESTED_RETRANSMISSION_TIME.
