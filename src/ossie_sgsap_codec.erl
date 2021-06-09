%% 3GPP TS 29.118 version 16.0.0 Release 16
-module(ossie_sgsap_codec).

-include("../include/sgsap.hrl").

-export([parse_sgsap_msg/1, encode_sgsap_msg/1]).

parse_sgsap_msg(DataBin) when is_binary(DataBin) ->
    <<MsgType:8, Remain/binary>> = DataBin,
    MsgT = parse_msg_type(MsgType),
    #sgsap_msg{msg_type = MsgT, msg_length = byte_size(DataBin), payload = parse_opts(MsgT, Remain)};
parse_sgsap_msg(Data) when is_list(Data) ->
    parse_sgsap_msg(list_to_binary(Data)).

parse_msg_type(?SGsAP_PAGING_REQUEST) -> sgsap_paging_request;
parse_msg_type(?SGsAP_PAGING_REJECT) -> sgsap_paging_reject;
parse_msg_type(?SGsAP_SERVICE_REQUEST) -> sgsap_service_request;
parse_msg_type(?SGsAP_DOWNLINK_UNITDATA) -> sgsap_downlink_unitdata;
parse_msg_type(?SGsAP_UPLINK_UNITDATA) -> sgsap_uplink_unitdata;
parse_msg_type(?SGsAP_LOCATION_UPDATE_REQUEST) -> sgsap_location_update_request;
parse_msg_type(?SGsAP_LOCATION_UPDATE_ACCEPT) -> sgsap_location_update_accept;
parse_msg_type(?SGsAP_LOCATION_UPDATE_REJECT) -> sgsap_location_update_reject;
parse_msg_type(?SGsAP_TMSI_REALLOCATION_COMPLETE) -> sgsap_tmsi_reallocation_complete;
parse_msg_type(?SGsAP_ALERT_REQUEST) -> sgsap_alert_request;
parse_msg_type(?SGsAP_ALERT_ACK) -> sgsap_alert_ack;
parse_msg_type(?SGsAP_ALERT_REJECT) -> sgsap_alert_reject;
parse_msg_type(?SGsAP_UE_ACTIVITY_INDICATION) -> sgsap_ue_activity_indication;
parse_msg_type(?SGsAP_EPS_DETACH_INDICATION) -> sgsap_eps_detach_indication;
parse_msg_type(?SGsAP_EPS_DETACH_ACK) -> sgsap_eps_detach_ack;
parse_msg_type(?SGsAP_IMSI_DETACH_INDICATION) -> sgsap_imsi_detach_indication;
parse_msg_type(?SGsAP_IMSI_DETACH_ACK) -> sgsap_imsi_detach_ack;
parse_msg_type(?SGsAP_RESET_INDICATION) -> sgsap_reset_indication;
parse_msg_type(?SGsAP_RESET_ACK) -> sgsap_reset_ack;
parse_msg_type(?SGsAP_SERVICE_ABORT_REQUEST) -> sgsap_service_abort_request;
parse_msg_type(?SGsAP_MO_CSFB_INDICATION) -> sgsap_mo_csfb_indication;
parse_msg_type(?SGsAP_MM_INFORMATION_REQUEST) -> sgsap_mm_information_request;
parse_msg_type(?SGsAP_RELEASE_REQUEST) -> sgsap_release_request;
parse_msg_type(?SGsAP_STATUS) -> sgsap_status;
parse_msg_type(?SGsAP_UE_UNREACHABLE) -> sgsap_ue_unreachable;
parse_msg_type(_) -> unknown_msg_type.

%% Chapter 8 Message functional definitions and contents
opt_types(sgsap_alert_ack) -> [imsi];
opt_types(sgsap_alert_reject) -> [imsi, sgs_cause];
opt_types(sgsap_alert_request) -> [imsi];
opt_types(sgsap_downlink_unitdata) -> [imsi, nas_message_container];
opt_types(sgsap_eps_detach_ack) -> [imsi];
opt_types(sgsap_eps_detach_indication) -> [imsi, mme_name, imsi_detach_from_eps_service_type];
opt_types(sgsap_imsi_detach_ack) -> [imsi];
opt_types(sgsap_imsi_detach_indication) -> [imsi, mme_name, imsi_detach_from_non_eps_service_type];
opt_types(sgsap_location_update_accept) -> [imsi, location_area_identifier, mobile_identity];
opt_types(sgsap_location_update_reject) -> [imsi, reject_cause, location_area_identifier];
opt_types(sgsap_location_update_request) -> [imsi, mme_name, eps_location_update_type, {new_location_area_identifier, location_area_identifier}, {old_location_area_identifier, location_area_identifier}, tmsi_status, imeisv, tracking_area_identity, tracking_area_identity, tmsi_based_nri_container, selected_cs_domain_operator];
opt_types(sgsap_mm_information_request) -> [imsi, mm_information];
opt_types(sgsap_paging_reject) -> [imsi, sgs_cause];
opt_types(sgsap_paging_request) -> [imsi, vlr_name, service_indicator, tmsi, cli, ss_code, lcs_indicator, lcs_client_identity, channel_needed, emlpp_priority, additional_paging_indicators, sm_delivery_timer, sm_delivery_start_time, maximum_retransmission_time];
opt_types(sgsap_reset_ack) -> [mme_name, vlr_name];
opt_types(sgsap_reset_indication) -> [mme_name, vlr_name];
opt_types(sgsap_service_request) -> [imsi, service_indicator, imeisv, ue_time_zone, mobile_station_classmark_2, tracking_area_identity, e_utran_cell_global_identity, ue_emm_mode];
opt_types(sgsap_status) -> [imsi, sgs_cause, erroneous_message];
opt_types(sgsap_tmsi_reallocation_complete) -> [imsi];
opt_types(sgsap_ue_activity_indication) -> [imsi, maximum_ue_availability_time];
opt_types(sgsap_ue_unreachable) -> [imsi, sgs_cause, requested_retransmission_time, additional_ue_unreachable_indicators];
opt_types(sgsap_uplink_unitdata) -> [imsi, nas_message_container, imeisv, ue_time_zone, mobile_station_classmark_2, tracking_area_identity, e_utran_cell_global_identity];
opt_types(sgsap_release_request) -> [imsi, sgs_cause];
opt_types(sgsap_service_abort_request) -> [imsi];
opt_types(sgsap_mo_csfb_indication) -> [imsi, tracking_area_identity, e_utran_cell_global_identity];
opt_types(unknown_msg_type) -> undefined.

parse_opts(MsgT, Bin) ->
    parse_opts(opt_types(MsgT), Bin, []).

parse_opts(_, <<>>, Acc) -> Acc;
parse_opts([], _, Acc) -> Acc;
parse_opts([{K, T}|Rest], Remain, Acc) ->
    {Opt, R} = parse_opt(T, Remain),
    parse_opts(Rest, R, [{K, Opt}|Acc]);
parse_opts([T|Rest], Remain, Acc) ->
    {Opt, R} = parse_opt(T, Remain),
    parse_opts(Rest, R, [{T, Opt}|Acc]).

%% Chapter 9 Information element coding
parse_opt(cli, Bin) ->
    <<?CLI:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>>,
    {CurOpt, R};
parse_opt(eps_location_update_type, Bin) ->
    <<?EPS_LOCATION_UPDATE_TYPE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> imsi_attach;
            2#00000010 -> normal_location_update;
            _ -> normal_location_update
        end,
    {C, Remain};
parse_opt(erroneous_message, Bin) ->
    <<?ERRONEOUS_MESSAGE:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_opt(e_utran_cell_global_identity, Bin) ->
    <<?E_UTRAN_CELL_GLOBAL_IDENTITY:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_opt(global_cn_id, Bin) ->
    ;
parse_opt(imeisv, Bin) ->
    ;
parse_opt(imsi, Bin) ->
    ;
parse_opt(imsi_detach_from_eps_service_type, Bin) ->
    <<?IMSI_DETACH_FROM_EPS_SERVICE_TYPE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> network_initiated_imsi_detach_from_eps_services;
            2#00000010 -> ue_initiated_imsi_detach_from_eps_services;
            2#00000011 -> eps_services_not_allowed;
            _ -> {reserved, CurOpt}
        end,
    {C, Remain};
parse_opt(imsi_detach_from_non_eps_service_type, Bin) ->
    <<?IMSI_DETACH_FROM_NON_EPS_SERVICE_TYPE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> explicit_ue_initiated_imsi_detach_from_non_eps_services;
            2#00000010 -> combined_ue_initiated_imsi_detach_from_eps_and_non_eps_services;
            2#00000011 -> implicit_network_initiated_imsi_detach_from_eps_and_non_eps_services;
            _ -> {reserved, CurOpt}
        end,
    {C, Remain};
parse_opt(lcs_client_identity, Bin) ->
    <<?LCS_CLIENT_IDENTITY:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_opt(lcs_indicator, Bin) ->
    <<?LCS_INDICATOR:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> mt_lr;
            _ -> {unspecified, CurOpt}
        end,
    {C, Remain};
parse_opt(location_area_identifier, Bin) ->
    <<?LOCATION_AREA_IDENTIFIER:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_opt(mm_information, Bin) ->
    ;
parse_opt(mme_name, Bin) ->
    <<?MME_NAME:8, L:8, Remain/binary>> = Bin,
    <<CharBin:((57-2)*8)/binary, R/binary>> = Remain,
    Chars = [C || <<_L:8, C:8, _:8>> <= CharBin],
    {Chars, R};
parse_opt(mobile_identity, Bin) ->
    ;
parse_opt(mobile_station_classmark_2, Bin) ->
    ;
parse_opt(nas_message_container, Bin) ->
    <<?NAS_MESSAGE_CONTAINER:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_opt(reject_cause, Bin) ->
    ;
parse_opt(service_indicator, Bin) ->
    <<?SERVICE_INDICATOR:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    <<CurOpt:8, R/binary>>,
    C = case CurOpt of
            2#00000001 -> cs_call_indicator;
            2#00000010 -> sms_indicator;
            _ -> cs_call_indicator
        end,
    {C, R};
parse_opt(sgs_cause, Bin) ->
    <<?SGS_CAUSE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> imsi_detached_for_eps_services;
            2#00000010 -> imsi_detached_for_eps_and_non_eps_services;
            2#00000011 -> imsi_unknown;
            2#00000100 -> imsi_detached_for_non_eps_services;
            2#00000101 -> imsi_implicitly_detached_for_non_eps_services;
            2#00000110 -> ue_unreachable;
            2#00000111 -> message_not_compatible_with_the_protocol_state;
            2#00001000 -> missing_mandatory_information_element;
            2#00001001 -> invalid_mandatory_information;
            2#00001010 -> conditional_information_element_error;
            2#00001011 -> semantically_incorrect_message;
            2#00001100 -> message_unknown;
            2#00001101 -> mobile_terminating_cs_fallback_call_rejected_by_the_user;
            2#00001110 -> ue_temporarily_unreachable;
            _ -> {unspecified, CurOpt}
        end,
    {C, Remain};
parse_opt(ss_code, Bin) ->
    <<?SS_CODE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_opt(tmsi, Bin) ->
    ;
parse_opt(tmsi_status, Bin) ->
    ;
parse_opt(tracking_area_identity) ->
    <<?TRACKING_AREA_IDENTITY:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_opt(ue_time_zone, Bin) ->
    <<?UE_TIME_ZONE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_opt(ie_emm_mode) ->
    <<?UE_EMM_MODE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> emm_idle;
            2#00000010 -> emm_connected;
            _ -> {reserved, CurOpt}
        end,
    {C, Remain};
parse_opt(vlr_name, Bin) ->
    <<?VLR_NAME:8, L:8, Remain/binary>> = Bin,
    <<CharBin:L/binary, R/binary>> = Remain,
    Chars = [C || <<_L:8, C:8, _:8>> <= CharBin],
    {Chars, R};
parse_opt(channel_needed, Bin) ->
    ;
parse_opt(emlpp_priority, Bin) ->
    ;
parse_opt(additional_paging_indicators, Bin) ->
    <<?ADDITIONAL_PAGING_INDICATORS:8, _L:8, 0:7, CurOpt:1/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#1 -> cs_restore_indicator_not_set;
            2#0 -> cs_restore_indicator_set
        end,
    {C, Remain};
parse_opt(tmsi_based_nri_container, Bin) ->
    ;
parse_opt(selected_cs_domain_operator, Bin) ->
    <<?SELECTED_CS_DOMAIN_OPERATOR:8, _L:8, CurOpt:24/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_opt(maximum_ue_availability_time, Bin) ->
    <<?MAXIMUM_UE_AVAILABILITY_TIME:8, _L:8, CurOpt:32/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_opt(sm_delivery_start_time, Bin) ->
    <<?SM_DELIVERY_START_TIME:8, _L:8, CurOpt:32/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_opt(additional_ue_unreachable_indicators, Bin) ->
    <<?ADDITIONAL_UE_UNREACHABLE_INDICATORS:8, _L:8, 0:7, CurOpt:1/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#1 -> sm_buffer_request_indicator_not_set;
            2#0 -> sm_buffer_request_indicator_set
        end,
    {C, Remain};
parse_opt(maximum_retransmission_time, Bin) ->
    <<?MAXIMUM_RETRANSMISSION_TIME:8, _L:8, CurOpt:32/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_opt(requested_retransmission_time, Bin) ->
    <<?REQUESTED_RETRANSMISSION_TIME:8, _L:8, CurOpt:32/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
