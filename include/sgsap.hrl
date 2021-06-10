%% 3GPP TS 29.118 version 16.0.0 Release 16

%% Chapter 6.3 SGs transport
-define(SGSAP_PORT, 29118).
-define(SGSAP_PPID, 0).

%% Chapter 9.2 Message type
%% 2#00000000 % Unassigned: treated as an unknown Message type
-define(SGSAP_MSGT_PAGING_REQUEST, 2#00000001).
-define(SGSAP_MSGT_PAGING_REJECT, 2#00000010).
%% 2#00000011 to 2#00000101 % Unassigned: treated as an unknown Message type
-define(SGSAP_MSGT_SERVICE_REQUEST, 2#00000110).
-define(SGSAP_MSGT_DOWNLINK_UNITDATA, 2#00000111).
-define(SGSAP_MSGT_UPLINK_UNITDATA, 2#00001000).
-define(SGSAP_MSGT_LOCATION_UPDATE_REQUEST, 2#00001001).
-define(SGSAP_MSGT_LOCATION_UPDATE_ACCEPT, 2#10000101).
-define(SGSAP_MSGT_LOCATION_UPDATE_REJECT, 2#00001011).
-define(SGSAP_MSGT_TMSI_REALLOCATION_COMPLETE, 2#00000110).
-define(SGSAP_MSGT_ALERT_REQUEST, 2#00001101).
-define(SGSAP_MSGT_ALERT_ACK, 2#00001110).
-define(SGSAP_MSGT_ALERT_REJECT, 2#10000111).
-define(SGSAP_MSGT_UE_ACTIVITY_INDICATION, 2#00010000).
-define(SGSAP_MSGT_EPS_DETACH_INDICATION, 2#00001000).
-define(SGSAP_MSGT_EPS_DETACH_ACK, 2#00010010).
-define(SGSAP_MSGT_IMSI_DETACH_INDICATION, 2#00010011).
-define(SGSAP_MSGT_IMSI_DETACH_ACK, 2#00010100).
-define(SGSAP_MSGT_RESET_INDICATION, 2#00010101).
-define(SGSAP_MSGT_RESET_ACK, 2#00010110).
-define(SGSAP_MSGT_SERVICE_ABORT_REQUEST, 2#00010111).
-define(SGSAP_MSGT_MO_CSFB_INDICATION, 2#00011000).
%% 2#00011001 to 2#00011001 % Unassigned: treated as an unknown Message type
-define(SGSAP_MSGT_MM_INFORMATION_REQUEST, 2#00011010).
-define(SGSAP_MSGT_RELEASE_REQUEST, 2#00011011).
%% 2#00011100 % Unassigned: treated as an unknown Message type
-define(SGSAP_MSGT_STATUS, 2#00011101).
%% 2#00011110 % Unassigned: treated as an unknown Message type
-define(SGSAP_MSGT_UE_UNREACHABLE, 2#00011111).


%% Chapter 9.3 Information element identifiers
-define(SGSAP_IEI_IMSI, 2#00000001).
-define(SGSAP_IEI_VLR_NAME, 2#00000010).
-define(SGSAP_IEI_TMSI, 2#00000011).
-define(SGSAP_IEI_LOCATION_AREA_IDENTIFIER, 2#00000100).
-define(SGSAP_IEI_CHANNEL_NEEDED, 2#00000101).
-define(SGSAP_IEI_EMLPP_PRIORITY, 2#00000110).
-define(SGSAP_IEI_TMSI_STATUS, 2#00000111).
-define(SGSAP_IEI_SGS_CAUSE, 2#00001000).
-define(SGSAP_IEI_MME_NAME, 2#00001001).
-define(SGSAP_IEI_EPS_LOCATION_UPDATE_TYPE, 2#00001010).
-define(SGSAP_IEI_GLOBAL_CN_ID, 2#00001011).
-define(SGSAP_IEI_MOBILE_IDENTITY, 2#00001110).
-define(SGSAP_IEI_REJECT_CAUSE, 2#00001111).
-define(SGSAP_IEI_IMSI_DETACH_FROM_EPS_SERVICE_TYPE, 2#00010000).
-define(SGSAP_IEI_IMSI_DETACH_FROM_NON_EPS_SERVICE_TYPE, 2#00010001).
-define(SGSAP_IEI_IMEISV, 2#00010101).
-define(SGSAP_IEI_NAS_MESSAGE_CONTAINER, 2#00010110).
-define(SGSAP_IEI_MM_INFORMATION, 2#00010111).
-define(SGSAP_IEI_ERRONEOUS_MESSAGE, 2#00011011).
-define(SGSAP_IEI_CLI, 2#00011100).
-define(SGSAP_IEI_LCS_CLIENT_IDENTITY, 2#00011101).
-define(SGSAP_IEI_LCS_INDICATOR, 2#00011110).
-define(SGSAP_IEI_SS_CODE, 2#00011111).
-define(SGSAP_IEI_SERVICE_INDICATOR, 2#00100000).
-define(SGSAP_IEI_UE_TIME_ZONE, 2#00100001).
-define(SGSAP_IEI_MOBILE_STATION_CLASSMARK_2, 2#00100010).
-define(SGSAP_IEI_TRACKING_AREA_IDENTITY, 2#00100011).
-define(SGSAP_IEI_E_UTRAN_CELL_GLOBAL_IDENTITY, 2#00100100).
-define(SGSAP_IEI_UE_EMM_MODE, 2#00100101).
-define(SGSAP_IEI_ADDITIONAL_PAGING_INDICATORS, 2#00100110).
-define(SGSAP_IEI_TMSI_BASED_NRI_CONTAINER, 2#00100111).
-define(SGSAP_IEI_SELECTED_CS_DOMAIN_OPERATOR, 2#00101000).
-define(SGSAP_IEI_MAXIMUM_UE_AVAILABILITY_TIME, 2#00101001).
-define(SGSAP_IEI_SM_DELIVERY_TIMER, 2#00101010).
-define(SGSAP_IEI_SM_DELIVERY_START_TIME, 2#00101011).
-define(SGSAP_IEI_ADDITIONAL_UE_UNREACHABLE_INDICATORS, 2#00101100).
-define(SGSAP_IEI_MAXIMUM_RETRANSMISSION_TIME, 2#00101101).
-define(SGSAP_IEI_REQUESTED_RETRANSMISSION_TIME, 2#00101110).

-record(sgsap_msg_params_alert_ack,
        {imsi}).
-record(sgsap_msg_params_alert_reject,
        {imsi,
         sgs_cause}).
-record(sgsap_msg_params_alert_request,
        {imsi}).
-record(sgsap_msg_params_downlink_unitdata,
        {imsi,
         nas_message_container}).
-record(sgsap_msg_params_eps_detach_ack,
        {imsi}).
-record(sgsap_msg_params_eps_detach_indication,
        {imsi,
         mme_name,
         imsi_detach_from_eps_service_type}).
-record(sgsap_msg_params_imsi_detach_ack,
        {imsi}).
-record(sgsap_msg_params_imsi_detach_indication,
        {imsi,
         mme_name,
         imsi_detach_from_non_eps_service_type}).
-record(sgsap_msg_params_location_update_accept,
        {imsi,
         location_area_identifier,
         mobile_identity}).
-record(sgsap_msg_params_location_update_reject,
        {imsi,
         reject_cause,
         location_area_identifier}).
-record(sgsap_msg_params_location_update_request,
        {imsi,
         mme_name,
         eps_location_update_type,
         new_location_area_identifier,
         old_location_area_identifier,
         tmsi_status,
         imeisv,
         tracking_area_identity,
         e_utran_cell_global_identity,
         tmsi_based_nri_container,
         selected_cs_domain_operator}).
-record(sgsap_msg_params_mm_information_request,
        {imsi,
         mm_information}).
-record(sgsap_msg_params_paging_reject,
        {imsi,
         sgs_cause}).
-record(sgsap_msg_params_paging_request,
        {imsi,
         vlr_name,
         service_indicator,
         tmsi,
         cli,
         ss_code,
         lcs_indicator,
         lcs_client_identity,
         channel_needed,
         emlpp_priority,
         additional_paging_indicators,
         sm_delivery_timer,
         sm_delivery_start_time,
         maximum_retransmission_time}).
-record(sgsap_msg_params_reset_ack,
        {mme_name,
         vlr_name}).
-record(sgsap_msg_params_reset_indication,
        {mme_name,
         vlr_name}).
-record(sgsap_msg_params_service_request,
        {imsi,
         service_indicator,
         imeisv,
         ue_time_zone,
         mobile_station_classmark_2,
         tracking_area_identity,
         e_utran_cell_global_identity,
         ue_emm_mode}).
-record(sgsap_msg_params_status,
        {imsi,
         sgs_cause,
         erroneous_message}).
-record(sgsap_msg_params_tmsi_reallocation_complete,
        {imsi}).
-record(sgsap_msg_params_ue_activity_indication,
        {imsi,
         maximum_ue_availability_time}).
-record(sgsap_msg_params_ue_unreachable,
        {imsi,
         sgs_cause,
         requested_retransmission_time,
         additional_ue_unreachable_indicators}).
-record(sgsap_msg_params_uplink_unitdata,
        {imsi,
         nas_message_container,
         imeisv,
         ue_time_zone,
         mobile_station_classmark_2,
         tracking_area_identity,
         e_utran_cell_global_identity}).
-record(sgsap_msg_params_release_request,
        {imsi,
         sgs_cause}).
-record(sgsap_msg_params_service_abort_request,
        {imsi}).
-record(sgsap_msg_params_mo_csfb_indication,
        {imsi,
         tracking_area_identity,
         e_utran_cell_global_identity}).


-record(sgsap_msg, {msg_type, msg_length, payload}).
