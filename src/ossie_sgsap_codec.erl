%% 3GPP TS 29.118 version 16.0.0 Release 16
-module(ossie_sgsap_codec).

-include("../include/sgsap.hrl").

-export([parse_sgsap_msg/1, encode_sgsap_msg/1]).

parse_sgsap_msg(DataBin) when is_binary(DataBin) ->
    <<MsgType:8, Remain/binary>> = DataBin,
    MsgT = parse_msg_type(MsgType),
    OptList = parse_sgsap_opts(Remain),
    #sgsap_msg{msg_type = MsgT, msg_length = byte_size(DataBin), payload = OptList};
parse_sgsap_msg(Data) when is_list(Data) ->
    parse_sgsap_msg(list_to_binary(Data)).

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
parse_msg_type(_) -> sgsap_msgt_unknown.

%% Chapter 8 Message functional definitions and contents
opt_types(sgsap_msgt_alert_ack) -> [imsi];
opt_types(sgsap_msgt_alert_reject) -> [imsi, sgs_cause];
opt_types(sgsap_msgt_alert_request) -> [imsi];
opt_types(sgsap_msgt_downlink_unitdata) -> [imsi, nas_message_container];
opt_types(sgsap_msgt_eps_detach_ack) -> [imsi];
opt_types(sgsap_msgt_eps_detach_indication) -> [imsi, mme_name, imsi_detach_from_eps_service_type];
opt_types(sgsap_msgt_imsi_detach_ack) -> [imsi];
opt_types(sgsap_msgt_imsi_detach_indication) -> [imsi, mme_name, imsi_detach_from_non_eps_service_type];
opt_types(sgsap_msgt_location_update_accept) -> [imsi, location_area_identifier, mobile_identity];
opt_types(sgsap_msgt_location_update_reject) -> [imsi, reject_cause, location_area_identifier];
opt_types(sgsap_msgt_location_update_request) -> [imsi, mme_name, eps_location_update_type, {new_location_area_identifier, location_area_identifier}, {old_location_area_identifier, location_area_identifier}, tmsi_status, imeisv, tracking_area_identity, tracking_area_identity, tmsi_based_nri_container, selected_cs_domain_operator];
opt_types(sgsap_msgt_mm_information_request) -> [imsi, mm_information];
opt_types(sgsap_msgt_paging_reject) -> [imsi, sgs_cause];
opt_types(sgsap_msgt_paging_request) -> [imsi, vlr_name, service_indicator, tmsi, cli, ss_code, lcs_indicator, lcs_client_identity, channel_needed, emlpp_priority, additional_paging_indicators, sm_delivery_timer, sm_delivery_start_time, maximum_retransmission_time];
opt_types(sgsap_msgt_reset_ack) -> [mme_name, vlr_name];
opt_types(sgsap_msgt_reset_indication) -> [mme_name, vlr_name];
opt_types(sgsap_msgt_service_request) -> [imsi, service_indicator, imeisv, ue_time_zone, mobile_station_classmark_2, tracking_area_identity, e_utran_cell_global_identity, ue_emm_mode];
opt_types(sgsap_msgt_status) -> [imsi, sgs_cause, erroneous_message];
opt_types(sgsap_msgt_tmsi_reallocation_complete) -> [imsi];
opt_types(sgsap_msgt_ue_activity_indication) -> [imsi, maximum_ue_availability_time];
opt_types(sgsap_msgt_ue_unreachable) -> [imsi, sgs_cause, requested_retransmission_time, additional_ue_unreachable_indicators];
opt_types(sgsap_msgt_uplink_unitdata) -> [imsi, nas_message_container, imeisv, ue_time_zone, mobile_station_classmark_2, tracking_area_identity, e_utran_cell_global_identity];
opt_types(sgsap_msgt_release_request) -> [imsi, sgs_cause];
opt_types(sgsap_msgt_service_abort_request) -> [imsi];
opt_types(sgsap_msgt_mo_csfb_indication) -> [imsi, tracking_area_identity, e_utran_cell_global_identity];
opt_types(sgsap_msgt_unknown) -> undefined.

get_num_pad_bytes(BinLenBytes) ->
    case BinLenBytes rem 2 of
        0 ->    0;
        Val ->  2 - Val
    end.

parse_sgsap_opts(OptBin) when is_binary(OptBin) ->
    parse_sgsap_opts(OptBin, []).

parse_sgsap_opts(<<>>, OptList) when is_list(OptList) ->
    OptList;
parse_sgsap_opts(OptBin, OptList) when is_binary(OptBin), is_list(OptList) ->
    <<IEI:8/big, Length:8/big, Remain/binary>> = OptBin,
    PadLen = get_num_pad_bytes(Length),
    LengthNet = Length - 4,
    <<CurOpt:LengthNet/binary, 0:PadLen/integer-unit:4, Remain2/binary>> = Remain,
    NewOpt = parse_sgsap_opt(dec_iei(IEI), CurOpt);
    parse_sgsap_opts(Remain2, OptList ++ [NewOpt]).

%% Chapter 9 Information element coding
parse_sgsap_opt(sgsap_iei_cli, Bin) ->
    <<?CLI:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>>,
    {CurOpt, R};
parse_sgsap_opt(sgsap_iei_eps_location_update_type, Bin) ->
    <<?EPS_LOCATION_UPDATE_TYPE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> imsi_attach;
            2#00000010 -> normal_location_update;
            _ -> normal_location_update
        end,
    {C, Remain};
parse_sgsap_opt(sgsap_iei_erroneous_message, Bin) ->
    <<?ERRONEOUS_MESSAGE:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_sgsap_opt(sgsap_iei_e_utran_cell_global_identity, Bin) ->
    <<?E_UTRAN_CELL_GLOBAL_IDENTITY:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_sgsap_opt(sgsap_iei_global_cn_id, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_imeisv, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_imsi, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_imsi_detach_from_eps_service_type, Bin) ->
    <<?IMSI_DETACH_FROM_EPS_SERVICE_TYPE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> network_initiated_imsi_detach_from_eps_services;
            2#00000010 -> ue_initiated_imsi_detach_from_eps_services;
            2#00000011 -> eps_services_not_allowed;
            _ -> {reserved, CurOpt}
        end,
    {C, Remain};
parse_sgsap_opt(sgsap_iei_imsi_detach_from_non_eps_service_type, Bin) ->
    <<?IMSI_DETACH_FROM_NON_EPS_SERVICE_TYPE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> explicit_ue_initiated_imsi_detach_from_non_eps_services;
            2#00000010 -> combined_ue_initiated_imsi_detach_from_eps_and_non_eps_services;
            2#00000011 -> implicit_network_initiated_imsi_detach_from_eps_and_non_eps_services;
            _ -> {reserved, CurOpt}
        end,
    {C, Remain};
parse_sgsap_opt(sgsap_iei_lcs_client_identity, Bin) ->
    <<?LCS_CLIENT_IDENTITY:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_sgsap_opt(sgsap_iei_lcs_indicator, Bin) ->
    <<?LCS_INDICATOR:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> mt_lr;
            _ -> {unspecified, CurOpt}
        end,
    {C, Remain};
parse_sgsap_opt(sgsap_iei_location_area_identifier, Bin) ->
    <<?LOCATION_AREA_IDENTIFIER:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_sgsap_opt(sgsap_iei_mm_information, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_mme_name, Bin) ->
    <<?MME_NAME:8, L:8, Remain/binary>> = Bin,
    <<CharBin:((57-2)*8)/binary, R/binary>> = Remain,
    Chars = [C || <<_L:8, C:8, _:8>> <= CharBin],
    {Chars, R};
parse_sgsap_opt(sgsap_iei_mobile_identity, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_mobile_station_classmark_2, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_nas_message_container, Bin) ->
    <<?NAS_MESSAGE_CONTAINER:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_sgsap_opt(sgsap_iei_reject_cause, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_service_indicator, Bin) ->
    <<?SERVICE_INDICATOR:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    <<CurOpt:8, R/binary>>,
    C = case CurOpt of
            2#00000001 -> cs_call_indicator;
            2#00000010 -> sms_indicator;
            _ -> cs_call_indicator
        end,
    {C, R};
parse_sgsap_opt(sgsap_iei_sgs_cause, Bin) ->
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
parse_sgsap_opt(sgsap_iei_ss_code, Bin) ->
    <<?SS_CODE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_sgsap_opt(sgsap_iei_tmsi, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_tmsi_status, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_tracking_area_identity) ->
    <<?TRACKING_AREA_IDENTITY:8, L:8, Remain/binary>> = Bin,
    <<CurOpt:L/binary, R/binary>> = Remain,
    {CurOpt, R};
parse_sgsap_opt(sgsap_iei_ue_time_zone, Bin) ->
    <<?UE_TIME_ZONE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_sgsap_opt(sgsap_iei_ie_emm_mode) ->
    <<?UE_EMM_MODE:8, _L:8, CurOpt:8/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#00000001 -> emm_idle;
            2#00000010 -> emm_connected;
            _ -> {reserved, CurOpt}
        end,
    {C, Remain};
parse_sgsap_opt(sgsap_iei_vlr_name, Bin) ->
    <<?VLR_NAME:8, L:8, Remain/binary>> = Bin,
    <<CharBin:L/binary, R/binary>> = Remain,
    Chars = [C || <<_L:8, C:8, _:8>> <= CharBin],
    {Chars, R};
parse_sgsap_opt(sgsap_iei_channel_needed, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_emlpp_priority, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_additional_paging_indicators, Bin) ->
    <<?ADDITIONAL_PAGING_INDICATORS:8, _L:8, 0:7, CurOpt:1/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#1 -> cs_restore_indicator_not_set;
            2#0 -> cs_restore_indicator_set
        end,
    {C, Remain};
parse_sgsap_opt(sgsap_iei_tmsi_based_nri_container, Bin) ->
    ;
parse_sgsap_opt(sgsap_iei_selected_cs_domain_operator, Bin) ->
    <<?SELECTED_CS_DOMAIN_OPERATOR:8, _L:8, CurOpt:24/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_sgsap_opt(sgsap_iei_maximum_ue_availability_time, Bin) ->
    <<?MAXIMUM_UE_AVAILABILITY_TIME:8, _L:8, CurOpt:32/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_sgsap_opt(sgsap_iei_sm_delivery_start_time, Bin) ->
    <<?SM_DELIVERY_START_TIME:8, _L:8, CurOpt:32/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_sgsap_opt(sgsap_iei_additional_ue_unreachable_indicators, Bin) ->
    <<?ADDITIONAL_UE_UNREACHABLE_INDICATORS:8, _L:8, 0:7, CurOpt:1/binary, Remain/binary>> = Bin,
    C = case CurOpt of
            2#1 -> sm_buffer_request_indicator_not_set;
            2#0 -> sm_buffer_request_indicator_set
        end,
    {C, Remain};
parse_sgsap_opt(sgsap_iei_maximum_retransmission_time, Bin) ->
    <<?MAXIMUM_RETRANSMISSION_TIME:8, _L:8, CurOpt:32/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};
parse_sgsap_opt(sgsap_iei_requested_retransmission_time, Bin) ->
    <<?REQUESTED_RETRANSMISSION_TIME:8, _L:8, CurOpt:32/binary, Remain/binary>> = Bin,
    {CurOpt, Remain};

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
