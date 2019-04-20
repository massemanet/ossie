
-define(SCTP_PPI_M2UA, 2).
-define(SCTP_PPI_M3UA, 3).
-define(SCTP_PPI_SUA,  4).
-define(SCTP_PPI_M2PA, 5).
-define(SCTP_PPI_DIAMETER, 46).

-type sctp_payload_protocol_id() :: ?SCTP_PPI_M2UA | ?SCTP_PPI_M3UA |
                                    ?SCTP_PPI_SUA | ?SCTP_PPI_M2PA |
                                    ?SCTP_PPI_DIAMETER.
