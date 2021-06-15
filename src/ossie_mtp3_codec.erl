%% ITU-T Q.704 (MTP Level 3) coding / decoding

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

-module(ossie_mtp3_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("../include/mtp3.hrl").

-export([parse_mtp3_msg/1, encode_mtp3_msg/1, invert_rout_lbl/1]).

%% Parse standard routing label according to Section 2.2 of ITU-T Q.704
parse_mtp3_routing_label(LabelBin) when is_binary(LabelBin) ->
    %% we need to swap the four bytes and then parse the fields
    <<Label32:32/little, Remain/binary>> = LabelBin,
    LabelRev = <<Label32:32/big>>,
    <<Sls:4/big, Opc:14/big, Dpc:14/big>> = LabelRev,
    {ok, #mtp3_routing_label{sig_link_sel = Sls, origin_pc = Opc, dest_pc = Dpc}, Remain}.

parse_mtp3_msg(DataBin) when is_binary(DataBin) ->
    <<NetInd:2, 0:2, SI:4, Remain/binary>> = DataBin,
    ServiceInd = parse_mtp3_service_indicator(SI),
    {ok, RoutLbl, Payload} = parse_mtp3_routing_label(Remain),
    PayloadDec = decode_payload(ServiceInd, Payload),
    #mtp3_msg{network_ind = NetInd, service_ind = ServiceInd, routing_label = RoutLbl,
              payload = PayloadDec}.


encode_mtp3_routing_label(#mtp3_routing_label{sig_link_sel = Sls, origin_pc = OpcIn,
                                              dest_pc = DpcIn}) ->
    Opc = ossie_util:pointcode2int(OpcIn),
    Dpc = ossie_util:pointcode2int(DpcIn),
    %% we need to swap the four bytes after encoding the fields
    <<Label32:32/little>> = <<Sls:4/big, Opc:14/big, Dpc:14/big>>,
    <<Label32:32/big>>.

encode_mtp3_msg(#mtp3_msg{network_ind = NetInd, service_ind = ServiceInd,
                          routing_label = RoutLbl, payload = Payload}) ->
    RoutLblBin = encode_mtp3_routing_label(RoutLbl),
    PayloadBin = payload_to_binary(ServiceInd, Payload),
    SI = enc_mtp3_service_indicator(ServiceInd),
    <<NetInd:2, 0:2, SI:4, RoutLblBin/binary, PayloadBin/binary>>.

decode_payload(mtp3_serv_mtn, Payload) ->
    <<H1:4, H0:4, _Len:4, 0:4, TP/binary>> = Payload,
    #mtp3mg_msg{h0 = H0, h1 = H1, payload = TP};
decode_payload(mtp3_serv_mgmt, Payload) ->
    <<H1:4, H0:4, Remain/binary>> = Payload,
    #mtp3mg_msg{h0 = H0, h1 = H1, payload = Remain};
decode_payload(_, Payload) ->
    Payload.

payload_to_binary(mtp3_serv_mtn, #mtp3mg_msg{h0=H0, h1=H1, payload=TP}) ->
    Len = byte_size(TP),
    <<H1:4, H0:4, Len:4, 0:4, TP/binary>>;
payload_to_binary(mtp3_serv_mgmt, #mtp3mg_msg{h0=H0, h1=H1, payload=Payload}) ->
    <<H1:4, H0:4, Payload/binary>>;
payload_to_binary(_, Whatever) ->
    Whatever.

parse_mtp3_service_indicator(?MTP3_SERV_MGMT) ->
    mtp3_serv_mgmt;
parse_mtp3_service_indicator(?MTP3_SERV_MTN) ->
    mtp3_serv_mtn;
parse_mtp3_service_indicator(?MTP3_SERV_SCCP) ->
    mtp3_serv_sccp;
parse_mtp3_service_indicator(?MTP3_SERV_TUP) ->
    mtp3_serv_tup;
parse_mtp3_service_indicator(?MTP3_SERV_ISUP) ->
    mtp3_serv_isup.

enc_mtp3_service_indicator(mtp3_serv_mgmt) ->
    ?MTP3_SERV_MGMT;
enc_mtp3_service_indicator(mtp3_serv_mtn) ->
    ?MTP3_SERV_MTN;
enc_mtp3_service_indicator(mtp3_serv_sccp) ->
    ?MTP3_SERV_SCCP;
enc_mtp3_service_indicator(mtp3_serv_tup) ->
    ?MTP3_SERV_TUP;
enc_mtp3_service_indicator(mtp3_serv_isup) ->
    ?MTP3_SERV_ISUP.

invert_rout_lbl(L = #mtp3_routing_label{origin_pc = Opc, dest_pc = Dpc}) ->
    L#mtp3_routing_label{origin_pc = Dpc, dest_pc = Opc}.
