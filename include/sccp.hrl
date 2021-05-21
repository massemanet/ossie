
% Table 1 / Q.713 - SCCP Message Types
-define(SCCP_MSGT_CR,		1).	% Connection request
-define(SCCP_MSGT_CC,		2).	% Connection confirm
-define(SCCP_MSGT_CREF,		3).	% Connection refused
-define(SCCP_MSGT_RLSD,		4).	% Released
-define(SCCP_MSGT_RLC,		5).	% Release complete
-define(SCCP_MSGT_DT1,		6).	% Data form 1
-define(SCCP_MSGT_DT2,		7).	% Data form 2
-define(SCCP_MSGT_AK,		8).	% Data acknowledgement
-define(SCCP_MSGT_UDT,		9).	% Unitdata
-define(SCCP_MSGT_UDTS,		10).	% Unitdata service
-define(SCCP_MSGT_ED,		11).	% Expedited data
-define(SCCP_MSGT_EA,		12).	% Expedited data ack
-define(SCCP_MSGT_RSR,		13).	% Reset Request
-define(SCCP_MSGT_RSC,		14).	% Reset Confirmation
-define(SCCP_MSGT_ERR,		15).	% Protocol data unit error
-define(SCCP_MSGT_IT,		16).	% Inactivity test
-define(SCCP_MSGT_XUDT,		17).	% Extended unitdata
-define(SCCP_MSGT_XUDTS,	18).	% Extended unitdata service
-define(SCCP_MSGT_LUDT,		19).	% Long unitdata
-define(SCCP_MSGT_LUDTS,	20).	% Long unitdata service

%% Q.713 (03/01) - Chapter 4
-record(sccp_msg_params_cr,
        {src_local_ref,
         protocol_class,
         called_party_addr,
         credit = undefined,
         calling_party_addr = undefined,
         data = undefined,
         hop_counter = undefined,
         importance = undefined
        }).
-record(sccp_msg_params_cc,
        {dst_local_ref,
         src_local_ref,
         protocol_class,
         credit = undefined,
         called_party_addr = undefined,
         data = undefined,
         importance = undefined
        }).
-record(sccp_msg_params_cref,
        {dst_local_ref,
         refusal_cause,
         called_party_addr = undefined,
         data = undefined,
         importance = undefined
        }).
-record(sccp_msg_params_rlsd,
        {dst_local_ref,
         src_local_ref,
         release_cause,
         data = undefined,
         importance = undefined
        }).
-record(sccp_msg_params_rlc,
        {dst_local_ref,
         src_local_ref
        }).
-record(sccp_msg_params_dt1,
        {dst_local_ref,
         segm_reass,
         data
        }).
-record(sccp_msg_params_dt2,
        {dst_local_ref,
         seq_segm,
         data
        }).
-record(sccp_msg_params_ak,
        {dst_local_ref,
         rx_seq_nr,
         credit
        }).
-record(sccp_msg_params_udt,
        {protocol_class,
         called_party_addr,
         calling_party_addr,
         data
        }).
-record(sccp_msg_params_udts,
        {return_cause,
         called_party_addr,
         calling_party_addr,
         data
        }).
-record(sccp_msg_params_ed,
        {dst_local_ref,
         data
        }).
-record(sccp_msg_params_ea,
        {dst_local_ref
        }).
-record(sccp_msg_params_rsr,
        {dst_local_ref,
         src_local_ref,
         reset_cause
        }).
-record(sccp_msg_params_rsc,
        {dst_local_ref,
         src_local_ref
        }).
-record(sccp_msg_params_err,
        {dst_local_ref,
         error_cause
        }).
-record(sccp_msg_params_it,
        {dst_local_ref,
         src_local_ref,
         protocol_class,
         seq_segm,
         credit
        }).
-record(sccp_msg_params_xudt,
        {protocol_class,
         hop_counter,
         called_party_addr,
         calling_party_addr,
         data,
         segmentation = undefined,
         importance = undefined
        }).
-record(sccp_msg_params_xudts,
        {return_cause,
         hop_counter,
         called_party_addr,
         calling_party_addr,
         data,
         segmentation = undefined,
         importance = undefined
        }).
-record(sccp_msg_params_ludt,
        {protocol_class,
         hop_counter,
         called_party_addr,
         calling_party_addr,
         long_data,
         segmentation = undefined,
         importance = undefined
        }).
-record(sccp_msg_params_ludts,
        {return_cause,
         hop_counter,
         called_party_addr,
         calling_party_addr,
         long_data,
         segmentation = undefined,
         importance = undefined
        }).

% Table 2 / Q.713 - SCCP parameter name codes
-define(SCCP_PNC_END_OF_OPTIONAL,		0).
-define(SCCP_PNC_DESTINATION_LOCAL_REFERENCE,	1).
-define(SCCP_PNC_SOURCE_LOCAL_REFERENCE,	2).
-define(SCCP_PNC_CALLED_PARTY_ADDRESS,		3).
-define(SCCP_PNC_CALLING_PARTY_ADDRESS,		4).
-define(SCCP_PNC_PROTOCOL_CLASS,		5).
-define(SCCP_PNC_SEGMENTING,			6).
-define(SCCP_PNC_RECEIVE_SEQ_NUMBER,		7).
-define(SCCP_PNC_SEQUENCING,			8).
-define(SCCP_PNC_CREDIT,			9).
-define(SCCP_PNC_RELEASE_CAUSE,			10).
-define(SCCP_PNC_RETURN_CAUSE,			11).
-define(SCCP_PNC_RESET_CAUSE,			12).
-define(SCCP_PNC_ERROR_CAUSE,			13).
-define(SCCP_PNC_REFUSAL_CAUSE,			14).
-define(SCCP_PNC_DATA,				15).
-define(SCCP_PNC_SEGMENTATION,			16).
-define(SCCP_PNC_HOP_COUNTER,			17).
-define(SCCP_PNC_IMPORTANCE,			18).
-define(SCCP_PNC_LONG_DATA,			19).

% According to Q.713 Section 3.4.1
-define(SCCP_GTI_NO_GT,		2#0000).
-define(SCCP_GTI_NAT_ONLY,	2#0001).
-define(SCCP_GTI_TT_ONLY,	2#0010).
-define(SCCP_GTI_TT_NP_ENC,	2#0011).
-define(SCCP_GTI_TT_NP_ENC_NAT,	2#0100).

% According to Q.713 Section 3.4.2.2
-define(SCCP_SSN_UNKNOWN,	2#00000000).
-define(SCCP_SSN_SCCP_MGMT,	2#00000001).
-define(SCCP_SSN_ITU_T,		2#00000010).
-define(SCCP_SSN_ISUP,		2#00000011).
-define(SCCP_SSN_OAM,		2#00000100).
-define(SCCP_SSN_MAP,		2#00000101).
-define(SCCP_SSN_HLR,		2#00000110).
-define(SCCP_SSN_VLR,		2#00000111).
-define(SCCP_SSN_MSC,		2#00001000).
-define(SCCP_SSN_EIR,		2#00001001).
-define(SCCP_SSN_AUC,		2#00001010).
-define(SCCP_SSN_ISDN_SS,	2#00001011).
-define(SCCP_SSN_RES_NAT,	2#00001100).
-define(SCCP_SSN_BISDN,		2#00001101).
-define(SCCP_SSN_TC_TEST,	2#00001110).

% According to Q.713 Section 3.4.2.3.1
% Bit 8 of octet 1 contains the odd/even indicator and is coded as follows:
% 0 - even number of address signals
% 1 - odd number of address signals
-define(SCCP_NAI_SUBSCRIBER,	2#00000001).
-define(SCCP_NAI_NATIONA_SIGN,	2#00000011).
-define(SCCP_NAI_INTERNATIONAL,	2#00000100).

% According to Q.713 Section 3.11 - Release Cause
-define(SCCP_CAUSE_REL_USER_ORIG,	2#00000000).
-define(SCCP_CAUSE_REL_USER_CONG,	2#00000001).
-define(SCCP_CAUSE_REL_USER_FAILURE,	2#00000010).
-define(SCCP_CAUSE_REL_SCCP_USER_ORIG,	2#00000011).
-define(SCCP_CAUSE_REL_REM_PROC_ERR,	2#00000100).
-define(SCCP_CAUSE_REL_INCONS_CONN_DAT,	2#00000101).
-define(SCCP_CAUSE_REL_ACCESS_FAIL,	2#00000110).
-define(SCCP_CAUSE_REL_ACCESS_CONG,	2#00000111).
-define(SCCP_CAUSE_REL_SUBSYS_FAILURE,	2#00001000).
-define(SCCP_CAUSE_REL_SUBSYS_CONG,	2#00001001).
-define(SCCP_CAUSE_REL_MTP_FAILURE,	2#00001010).
-define(SCCP_CAUSE_REL_NET_CONG,	2#00001011).
-define(SCCP_CAUSE_REL_EXP_T_RES,	2#00001100).
-define(SCCP_CAUSE_REL_EXP_T_IAR,	2#00001101).
-define(SCCP_CAUSE_REL_UNQUALIFIED,	2#00001111).
-define(SCCP_CAUSE_REL_SCCP_FAILURE,	2#00010000).

% According to Q.713 Section 3.12 - Return Cause
-define(SCCP_CAUSE_RET_NOTRANS_NATURE,	2#00000000).
-define(SCCP_CAUSE_RET_NOTRANS_ADDR,	2#00000001).
-define(SCCP_CAUSE_RET_SUBSYS_CONG,	2#00000010).
-define(SCCP_CAUSE_RET_SUBSYS_FAILURE,	2#00000011).
-define(SCCP_CAUSE_RET_UNEQUIP_USER,	2#00000100).
-define(SCCP_CAUSE_RET_MTP_FAILURE,	2#00000101).
-define(SCCP_CAUSE_RET_NET_CONG,	2#00000110).
-define(SCCP_CAUSE_RET_UNQUALIFIED,	2#00000111).
-define(SCCP_CAUSE_RET_ERR_MSG_TRANSP,	2#00001000).
-define(SCCP_CAUSE_RET_ERR_LOCAL_PROC,	2#00001001).
-define(SCCP_CAUSE_RET_DEST_NO_REASS,	2#00001010).
-define(SCCP_CAUSE_RET_SCCP_FAILURE,	2#00001011).
-define(SCCP_CAUSE_RET_HOP_CTR_FAIL,	2#00001100).
-define(SCCP_CAUSE_RET_SEG_NOT_SUPP,	2#00001101).
-define(SCCP_CAUSE_RET_SEG_FAILURE,	2#00001110).

% According to Q.713 Section 3.13 - Reset Cause
-define(SCCP_CAUSE_RES_ENDU_ORIGINATED,	2#00000000).
-define(SCCP_CAUSE_RES_SCCP_USER_ORIG,	2#00000001).
-define(SCCP_CAUSE_RES_MSGO_OOO_PS,	2#00000010).
-define(SCCP_CAUSE_RES_MSGO_OOO_PR,	2#00000011).
-define(SCCP_CAUSE_RES_MSGO_OO_WIND,	2#00000100).
-define(SCCP_CAUSE_RES_INC_PS_REINIT,	2#00000101).
-define(SCCP_CAUSE_RES_REM_GENERAL,	2#00000110).
-define(SCCP_CAUSE_RES_REM_OPERATIONAL,	2#00000111).
-define(SCCP_CAUSE_RES_NET_OPERATIONAL,	2#00001000).
-define(SCCP_CAUSE_RES_ACC_OPERATIONAL,	2#00001001).
-define(SCCP_CAUSE_RES_NET_CONG,	2#00001010).
-define(SCCP_CAUSE_RES_UNQUALIFIED,	2#00001100).

% According to Q.713 Section 3.14 - Error Cause
-define(SCCP_CAUSE_ERR_LRN_UNASSIGNED,	2#00000000).
-define(SCCP_CAUSE_ERR_LRN_MISMATCH,	2#00000001).
-define(SCCP_CAUSE_ERR_PC_MISMATCH,	2#00000010).
-define(SCCP_CAUSE_ERR_SCLASS_MISMATCH,	2#00000011).
-define(SCCP_CAUSE_ERR_UNQUALIFIED,	2#00000100).

% According to Q.713 Section 3.15 - Refusal Cause
-define(SCCP_CAUSE_REF_ENDU_ORIGINATED,		2#00000000).
-define(SCCP_CAUSE_REF_ENDU_CONGESTION,		2#00000001).
-define(SCCP_CAUSE_REF_ENDU_FAILURE,		2#00000010).
-define(SCCP_CAUSE_REF_USER_ORIGINATED,		2#00000011).
-define(SCCP_CAUSE_REF_DEST_UNKNOWN,		2#00000100).
-define(SCCP_CAUSE_REF_DEST_INACCESS,		2#00000101).
-define(SCCP_CAUSE_REF_QOS_UNAVAIL_NTRANS,	2#00000110).
-define(SCCP_CAUSE_REF_QOS_UNAVAIL_TRANS,	2#00000111).
-define(SCCP_CAUSE_REF_ACCESS_FAIL,		2#00001000).
-define(SCCP_CAUSE_REF_ACCESS_CONGESTION,	2#00001001).
-define(SCCP_CAUSE_REF_SUBSYS_FAILURE,		2#00001010).
-define(SCCP_CAUSE_REF_SUBSYS_CONGESTION,	2#00001011).
-define(SCCP_CAUSE_REF_EXP_CONN_EST_TMR,	2#00001100).
-define(SCCP_CAUSE_REF_INCOMP_USER_DATA,	2#00001101).
-define(SCCP_CAUSE_REF_RESERVED,		2#00001110).
-define(SCCP_CAUSE_REF_UNQUALIFIED,		2#00001111).
-define(SCCP_CAUSE_REF_HOP_COUNTER_VIOL,	2#00010000).
-define(SCCP_CAUSE_REF_SCCP_FAIL,		2#00010001).
-define(SCCP_CAUSE_REF_NO_GTT_FOR_NATURE,	2#00010010).
-define(SCCP_CAUSE_REF_UNEQUIPPED_USER,		2#00010011).

% 3GPP TS 23.003, Chapter 8.1 - Global subsystem numbers
-define(SCCP_SSN_MAP_HLR,	2#00000110).
-define(SCCP_SSN_MAP_VLR,	2#00000111).
-define(SCCP_SSN_MAP_MSC,	2#00001000).
-define(SCCP_SSN_MAP_EIR,	2#00001001).

% 3GPP TS 23.003, Chapter 8.2 - National subsystem numbers
-define(SCCP_SSN_RANAP,		2#10001110).
-define(SCCP_SSN_RNSAP,		2#10001111).
-define(SCCP_SSN_MAP_GMLC,	2#10010001).
-define(SCCP_SSN_CAP,		2#10010010).
-define(SCCP_SSN_MAP_gsmSCF,	2#10010011).
-define(SCCP_SSN_MAP_SIWF,	2#10010100).
-define(SCCP_SSN_MAP_SGSN,	2#10010101).
-define(SCCP_SSN_MAP_GGSN,	2#10010110).

-type sccp_msgt() :: sccp_msgt_cr    |	% Connection request
                     sccp_msgt_cc    |	% Connection confirm
                     sccp_msgt_cref  |	% Connection refused
                     sccp_msgt_rlsd  |	% Released
                     sccp_msgt_rlc   |	% Release complete
                     sccp_msgt_dt1   |	% Data form 1
                     sccp_msgt_dt2   |	% Data form 2
                     sccp_msgt_ak    |	% Data acknowledgement
                     sccp_msgt_udt   |	% Unitdata
                     sccp_msgt_udts  |	% Unitdata service
                     sccp_msgt_ed    |	% Expedited data
                     sccp_msgt_ea    |	% Expedited data ack
                     sccp_msgt_rsr   |	% Reset Request
                     sccp_msgt_rsc   |	% Reset Confirmation
                     sccp_msgt_err   |	% Protocol data unit error
                     sccp_msgt_it    |	% Inactivity test
                     sccp_msgt_xudt  |	% Extended unitdata
                     sccp_msgt_xudts |	% Extended unitdata service
                     sccp_msgt_ludt  |	% Long unitdata
                     sccp_msgt_ludts.	% Long unitdata service

-type sccp_msg_type()		:: 0..255 | sccp_msgt().
-type sccp_proto_class()	:: 0..3.

% a single parsed SCCP message
-record(sccp_msg, {
	msg_type	:: sccp_msg_type(),
	parameters
	}).


-record(global_title, {
	  gti :: 0..15,
	  nature_of_addr_ind :: 0..127 | undefined,
	  trans_type,
	  encoding	 :: 0..15 | undefined,
	  numbering_plan :: 0..15 | undefined,
	  phone_number
	}).

-record(sccp_addr, {
	  res_nat_use,
	  route_on_ssn,
	  point_code,		% optional
	  ssn,			% optional
	  global_title		% optional
	}).
