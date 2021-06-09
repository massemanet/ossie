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
    <<CurOpt:8, R/binary>>,
    C = case CurOpt of
            1 -> imsi_attach;
            2 -> normal_location_update;
            _ -> normal_location_update
        end,
    {C, R};
parse_opt(erroneous_message, Bin) ->
    <<?ERRONEOUS_MESSAGE:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_opt() ->
