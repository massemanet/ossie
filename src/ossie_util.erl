%% Osmocom Erlang utility functions

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

-module(ossie_util).
-author('Harald Welte <laforge@gnumonks.org>').

-export([digit_list2int/1, int2digit_list/1]).
-export([reload_config/0]).
-export([tuple_walk/3, tuple_walk_print_cb/3]).
-export([make_prim/4, make_prim/3]).
-export([pointcode2int/1, pointcode2int/2, pointcode_fmt/2]).
-export([asn_val/1]).
-export([get_env/2, get_env/3]).
-export([decode_tbcd/1, encode_tbcd/1]).
-export([decode_mcc_mnc/1, encode_mcc_mnc/1]).
-export([decode_gsm_string/2, encode_gsm_string/2]).

-include_lib("include/util.hrl").

%% Convert a list of digits to an integer value
digit_list2int(Int, []) ->
    Int;
digit_list2int(Int, [Digit|Tail]) ->
    digit_list2int(Int*10 + Digit, Tail).
digit_list2int(Digits) when is_list(Digits) ->
    digit_list2int(0, Digits).

%% Convert an integer value into a list of decimal digits
int2digit_list(0, Digits) when is_list(Digits) ->
    Digits;
int2digit_list(Int, Digits) when is_integer(Int), is_list(Digits) ->
    Digit = Int rem 10,
    int2digit_list(Int div 10, [Digit|Digits]).
int2digit_list(Int) when is_integer(Int) ->
    int2digit_list(Int, []).

%% reload configuration of an application
reload_config() ->
    case init:get_argument(config) of
        {ok, [ Files ]} ->
            ConfFiles = [begin
                             S = filename:basename(F,".config"),
                             filename:join(filename:dirname(F),
                                           S ++ ".config")
                         end || F <- Files],
            %% Move sys.config to the head of the list
            Config = lists:sort(fun("sys.config", _) -> true;
                                   (_, _) -> false end, ConfFiles),

            OldEnv = application_controller:prep_config_change(),

            Apps = [{application, A, make_appl(A)}
                    || {A,_,_} <- application:which_applications()],
            application_controller:change_application_data(Apps, Config),
            application_controller:config_change(OldEnv);
        _ ->
            {ok, []}
    end.

make_appl(App) when is_atom(App) ->
    AppList  = element(2,application:get_all_key(App)),
    FullName = code:where_is_file(atom_to_list(App) ++ ".app"),
    case file:consult(FullName) of
        {ok, [{application, _, Opts}]} ->
            Env = proplists:get_value(env, Opts, []),
            lists:keyreplace(env, 1, AppList, {env, Env});
        {error, _Reason} ->
            lists:keyreplace(env, 1, AppList, {env, []})
    end.


%% Walk a named tuple and (recursively) all its fields, call user-supplied
%% callback for each of them
tuple_walk(Tpl, TupleCb, Args) when is_tuple(Tpl), is_function(TupleCb),
                                    is_list(Args) ->
    tuple_walk([], Tpl, TupleCb, Args).

tuple_walk(Path, Tpl, TupleCb, Args) when is_list(Path), is_tuple(Tpl),
                                          is_list(Args) ->
    %% call Callback
    RetVal = TupleCb(Path, Tpl, Args),
    if
        is_tuple(RetVal) ->
            [TplName|TplList] = tuple_to_list(RetVal),
            NewTplList = tuple_fieldlist_walk(Path, TplName, TplList, TupleCb, Args),
            list_to_tuple([TplName|NewTplList]);
        true ->
            RetVal
    end;
tuple_walk(Path, TplL, TupleCb, Args) when is_list(Path), is_list(TplL),
                                           is_list(Args) ->
    tuple_walk_list(Path, TplL, TupleCb, Args, []).

tuple_walk_list(_Path, [], _TupleCb, _Args, OutList) ->
    OutList;
tuple_walk_list(Path, [Head|Tail], TupleCb, Args, OutList) ->
    if
        is_tuple(Head) ->
            NewHead = tuple_walk(Path, Head, TupleCb, Args);
        is_list(Head) ->
            NewHead = tuple_walk(Path, Head, TupleCb, Args);
        true ->
            NewHead = Head
    end,
    tuple_walk_list(Path, Tail, TupleCb, Args, OutList++[NewHead]).


tuple_fieldlist_walk(Path, TplName, FieldList, TupleCb, Args) ->
    tuple_fieldlist_walk(Path, TplName, FieldList, TupleCb, Args, []).

tuple_fieldlist_walk(_Path, _TplName, [], _TplCb, _Args, OutList) ->
    OutList;
tuple_fieldlist_walk(Path, TplName, [Head|List], TupleCb, Args, OutList) ->
    if
        is_tuple(Head) ->
            NewHead = tuple_walk(Path++[TplName], Head, TupleCb, Args);
        is_list(Head) ->
            NewHead = tuple_walk(Path++[TplName], Head, TupleCb, Args);
        true ->
            NewHead = Head
    end,
    tuple_fieldlist_walk(Path, TplName, List, TupleCb, Args, OutList++[NewHead]).


tuple_walk_print_cb(Path, Tpl, _Args) when is_list(Path), is_tuple(Tpl) ->
    io:format("~p:~p~n", [Path, Tpl]),
    Tpl.

%% helper function to create a #primitive record
make_prim(Subsys, GenName, SpecName) ->
    make_prim(Subsys, GenName, SpecName, []).
make_prim(Subsys, GenName, SpecName, Param) ->
    #primitive{subsystem = Subsys, gen_name = GenName,
               spec_name = SpecName, parameters = Param}.

%% parse a 3-tuple pointcode into a raw integer
pointcode2int(Int) when is_integer(Int) ->
    Int;
pointcode2int(undefined) ->
    undefined;
pointcode2int(#pointcode{repr=Type, value=Value}) ->
    pointcode2int(Type, Value);
pointcode2int({Std, Param}) ->
    pointcode2int(Std, Param).

pointcode2int(itu, {A, B, C}) ->
    <<PcInt:14/big>> = <<A:3, B:8, C:3>>,
    PcInt;
pointcode2int(ansi, {A, B, C}) ->
    <<PcInt:24/big>> = <<A:8, B:8, C:8>>,
    PcInt;
pointcode2int(ttc, {A, B, C}) ->
    <<PcInt:16/big>> = <<A:5, B:4, C:7>>,
    PcInt.

%% format a point-code into a 3-tuple according to the standard used
pointcode_fmt(Std, P) when is_binary(P) ->
    <<PcInt/integer>> = P,
    pointcode_fmt(Std, PcInt);
pointcode_fmt(itu, PcInt) when is_integer(PcInt) ->
    <<A:3, B:8, C:3>> = <<PcInt:14/big>>,
    {pointcode, itu, {A, B, C}};
pointcode_fmt(ansi, PcInt) ->
    <<A:8, B:8, C:8>> = <<PcInt:24/big>>,
    {pointcode, ansi, {A, B, C}};
pointcode_fmt(ttc, PcInt) ->
    <<A:5, B:4, C:7>> = <<PcInt:16/big>>,
    {pointcode, ttc, {A, B, C}}.

asn_val(undefined) ->
    asn1_NOVALUE;
asn_val([]) ->
    asn1_NOVALUE;
asn_val(Foo) ->
    Foo.

%% wrapper around application:get_env() thwowing exception on undef
get_env(App, Var) when is_atom(App), is_atom(Var) ->
    case application:get_env(App, Var) of
        undefined ->
            throw(undefined);
        {ok, Value} ->
            Value
    end.

get_env(App, Var, Default) ->
    case application:get_env(App, Var) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end.

%% "telephony binary coded decimal"
%% TS 29.002, type TBCD-STRING
%% "123" -> <<2:4, 1:4, 15:4, 3:4>>.
decode_tbcd(asn1_NOVALUE) ->
    undefined;
decode_tbcd(Bin) ->
    lists:reverse(decode_tbcd(Bin, [])).

decode_tbcd(<<>>, Acc) ->
    Acc;
decode_tbcd(<<2#1111:4, D1:4>>, Acc) ->
    [dec_tbcd_digit(D1) | Acc];
decode_tbcd(<<D1:4, D2:4, Ds/binary>>, Acc) ->
    decode_tbcd(Ds, [dec_tbcd_digit(D1), dec_tbcd_digit(D2) | Acc]).

dec_tbcd_digit(D) when D >= 0, D =< 9 -> D + $0;
dec_tbcd_digit(2#1010) -> $*;
dec_tbcd_digit(2#1011) -> $#;
dec_tbcd_digit(2#1100) -> $a;
dec_tbcd_digit(2#1101) -> $b;
dec_tbcd_digit(2#1110) -> $c.

encode_tbcd(Str) ->
    encode_tbcd(Str, <<>>).

encode_tbcd([D1, D2 | Ds], Acc) ->
    encode_tbcd(Ds, enc_tbcd_digits(D1, D2, Acc));
encode_tbcd([D], Acc) ->
    enc_tbcd_digits(D, pad, Acc);
encode_tbcd([], Acc) ->
    Acc.

enc_tbcd_digits(D1, D2, Acc) ->
    <<Acc/binary, (enc_tbcd_digit(D2)):4, (enc_tbcd_digit(D1)):4>>.

enc_tbcd_digit(D) when D >= $0, D =< $9   -> D - $0;
enc_tbcd_digit(pad)                       -> 2#1111;
enc_tbcd_digit($*)                        -> 2#1010;
enc_tbcd_digit($#)                        -> 2#1011;
enc_tbcd_digit(A) when A =:= $A; A =:= $a -> 2#1100;
enc_tbcd_digit(B) when B =:= $B; B =:= $b -> 2#1101;
enc_tbcd_digit(C) when C =:= $C; C =:= $c -> 2#1110.

decode_mcc_mnc(<<MCC2:4, MCC1:4, 2#1111:4, MCC3:4, MNC2:4, MNC1:4>>) ->
    lists:map(fun dec_tbcd_digit/1, [MCC1, MCC2, MCC3, MNC1, MNC2]);
decode_mcc_mnc(<<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4>>) ->
    lists:map(fun dec_tbcd_digit/1, [MCC1, MCC2, MCC3, MNC1, MNC2, MNC3]).

encode_mcc_mnc(MCCMNC) when length(MCCMNC) == 5 ->
    [MCC1, MCC2, MCC3, MNC1, MNC2] = lists:map(fun enc_tbcd_digit/1, MCCMNC),
    <<MCC2:4, MCC1:4, 2#1111:4, MCC3:4, MNC2:4, MNC1:4>>;
encode_mcc_mnc(MCCMNC) when length(MCCMNC) == 6 ->
    [MCC1, MCC2, MCC3, MNC1, MNC2, MNC3] = lists:map(fun enc_tbcd_digit/1, MCCMNC),
    <<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4>>.

%% 3GPP TS 23.038
decode_gsm_string(Packed, NumSpares) ->
    Reversed = reverse_binary(Packed),
    <<0:NumSpares, Unpadded/bits>> = Reversed,
    Unpacked = << <<X:8>> || <<X:7>> <= Unpadded >>,
    binary_to_list(reverse_binary(Unpacked)).
encode_gsm_string(String, NumSpares) ->
    Unpacked = reverse_binary(list_to_binary(String)),
    Packed = << <<X:7>> || <<X:8>> <= Unpacked >>,
    Reversed = <<0:NumSpares, Packed/bits>>,
    reverse_binary(Reversed).

reverse_binary(Bin) ->
    binary:encode_unsigned(binary:decode_unsigned(Bin, little), big).
