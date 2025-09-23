%%-----------------------------------------------------------------------------
%% @author Roman Parykin <github@donderom.com>
%% @doc
%% The implementation of the French stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/french/stemmer.html">
%% The French stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_fr).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
-export([stem/1]).

%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------
-define(is_a_vowel(Char),
        ((Char =:= $a)
         orelse (Char =:= $e)
         orelse (Char =:= $i)
         orelse (Char =:= $o)
         orelse (Char =:= $u)
         orelse (Char =:= $y)
         orelse (Char =:= $â)
         orelse (Char =:= $à)
         orelse (Char =:= $ë)
         orelse (Char =:= $é)
         orelse (Char =:= $ê)
         orelse (Char =:= $è)
         orelse (Char =:= $ï)
         orelse (Char =:= $î)
         orelse (Char =:= $ô)
         orelse (Char =:= $û)
         orelse (Char =:= $ù))).

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------
-type r1pos() :: non_neg_integer().
-type r2pos() :: non_neg_integer().
-type rvpos() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

%% @doc     Stem of a French word form.
%% @param   Word a French word form
%% @returns stem of a Word in French
-spec stem(Word :: string()) -> string().
stem(Word) ->
    {Word1, R1Pos, R2Pos, RVPos} = bootstrap(Word),
    ReversedWord = lists:reverse(Word1),
    stem(ReversedWord, R1Pos, R2Pos, RVPos).

-spec stem(string(), r1pos(), r2pos(), rvpos()) -> string().
stem(ReversedWord, R1Pos, R2Pos, RVPos) ->
    Step1 = step1(ReversedWord, R1Pos, R2Pos, RVPos),
    Step5 = step5(Step1),
    Step6 = step6(Step5),
    post_process(Step6).

%%-----------------------------------------------------------------------------
%% Steps
%%-----------------------------------------------------------------------------

%% step 1
-spec step1(string(), r1pos(), r2pos(), rvpos()) -> string().
step1("secna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3(Tail);
step1("seUqi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3(Tail);
step1("semsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3(Tail);
step1("selba" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3(Tail);
step1("setsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3(Tail);
step1("ecna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(Tail);
step1("eUqi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(Tail);
step1("emsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(Tail);
step1("elba" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(Tail);
step1("etsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(Tail);
step1("xue" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> step3(Tail);

step1("secirta" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3(step1a(Tail, R2Pos));
step1("ecirta" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(step1a(Tail, R2Pos));
step1("srueta" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(step1a(Tail, R2Pos));
step1("snoita" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(step1a(Tail, R2Pos));
step1("rueta" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> step3(step1a(Tail, R2Pos));
step1("noita" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> step3(step1a(Tail, R2Pos));

step1("seigol" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3("gol" ++ Tail);
step1("eigol" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3("gol" ++ Tail);

step1("snoisu" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3([$u | Tail]);
step1("snoitu" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3([$u | Tail]);
step1("noisu" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3([$u | Tail]);
step1("noitu" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3([$u | Tail]);

step1("secne" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3("tne" ++ Tail);
step1("ecne" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3("tne" ++ Tail);

step1("stnemessi" ++ Tail, R1Pos, _, _) when length(Tail) >= R1Pos,
                                             not ?is_a_vowel(hd(Tail)) ->
    step3(Tail);
step1("tnemessi" ++ Tail = Word, R1Pos, R2Pos, RVPos) ->
    case ((not ?is_a_vowel(hd(Tail))) andalso (length(Tail) >= R1Pos)) of
        true ->
            step3(Tail);
        false ->
            step2a(Word, R2Pos, RVPos)
    end;

step1("tnemma" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= RVPos ->
    step2a("tna" ++ Tail, R2Pos, RVPos);
step1("tnemme" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= RVPos ->
    step2a("tne" ++ Tail, R2Pos, RVPos);

step1("stneme" ++ Tail, R1Pos, R2Pos, RVPos) when length(Tail) >= RVPos ->
    step3(step1b(Tail, R1Pos, R2Pos, RVPos));
step1("tneme" ++ Tail, R1Pos, R2Pos, RVPos) when length(Tail) >= RVPos  ->
    step3(step1b(Tail, R1Pos, R2Pos, RVPos));

step1("séti" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3(step1d(Tail, R2Pos));
step1("éti" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(step1d(Tail, R2Pos));
step1("sevi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3(step1e(Tail, R2Pos));
step1("sfi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(step1e(Tail, R2Pos));
step1("evi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(step1e(Tail, R2Pos));
step1("fi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> step3(step1e(Tail, R2Pos));

step1("xuae" ++ Tail, _, _, _)                               -> step3("uae" ++ Tail);
step1("xua" ++ Tail, R1Pos, _, _) when length(Tail) >= R1Pos -> step3("la" ++ Tail);

step1("sesue" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step3(Tail);
step1("esue" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step3(Tail);
step1("sesue" ++ Tail, R1Pos, _, _) when length(Tail) >= R1Pos -> step3("xue" ++ Tail);
step1("esue" ++ Tail, R1Pos, _, _) when length(Tail) >= R1Pos  -> step3("xue" ++ Tail);

step1("stnem" ++ Tail, _, R2Pos, RVPos) when length(tl(Tail)) >= RVPos,
                                             ?is_a_vowel(hd(Tail)) ->
    step2a(Tail, R2Pos, RVPos);
step1("tnem" ++ Tail, _, R2Pos, RVPos) when length(tl(Tail)) >= RVPos,
                                            ?is_a_vowel(hd(Tail))  ->
    step2a(Tail, R2Pos, RVPos);

step1(Word, _, R2Pos, RVPos) -> step2a(Word, R2Pos, RVPos).

%% step 1a
-spec step1a(string(), r2pos()) -> string().
step1a("ci" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1a("ci" ++ Tail, _)                                -> "Uqi" ++ Tail;
step1a(Word, _)                                        -> Word.

%% step 1b
-spec step1b(string(), r1pos(), r2pos(), rvpos()) -> string().
step1b("vi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step1c(Tail, R2Pos);
step1b("sue" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> Tail;
step1b("sue" ++ Tail, R1Pos, _, _) when length(Tail) >= R1Pos -> "xue" ++ Tail;
step1b("lba" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> Tail;
step1b("Uqi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> Tail;
step1b("rèi" ++ Tail, _, _, RVPos) when length(Tail) >= RVPos -> [$i |Tail];
step1b("rèI" ++ Tail, _, _, RVPos) when length(Tail) >= RVPos -> [$i |Tail];
step1b(Word, _, _, _)                                         -> Word.

%% step 1c
-spec step1c(string(), r2pos()) -> string().
step1c("ta" ++ Tail, R2Pos) when length(Tail) =:= R2Pos -> Tail;
step1c(Word, _)                                         -> Word.

%% step 1d
-spec step1d(string(), r2pos()) -> string().
step1d("liba" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1d("liba" ++ Tail, _)                                -> "lba" ++ Tail;
step1d("ci" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step1d("ci" ++ Tail, _)                                  -> "Uqi" ++ Tail;
step1d("vi" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step1d(Word, _)                                          -> Word.

%% step 1e
-spec step1e(string(), r2pos()) -> string().
step1e("ta" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> step1a(Tail, R2Pos);
step1e(Word, _)                                        -> Word.

%% step 2a
%%   where T = Tail, W = Word, R1 = R1 position, R2 = R2 position,
%%     RV = RV position and C = Character
-spec step2a(string(), r2pos(), rvpos()) -> string().
step2a("tneIassi" ++ T = W, R2, RV) when length(T) >= RV -> step2a(T, R2, RV, W);
step2a("setnassi" ++ T = W, R2, RV) when length(T) >= RV -> step2a(T, R2, RV, W);
step2a("tneIari" ++ T = W, R2, RV) when length(T) >= RV  -> step2a(T, R2, RV, W);
step2a("etnassi" ++ T = W, R2, RV) when length(T) >= RV  -> step2a(T, R2, RV, W);
step2a("stnassi" ++ T = W, R2, RV) when length(T) >= RV  -> step2a(T, R2, RV, W);
step2a("snoissi" ++ T = W, R2, RV) when length(T) >= RV  -> step2a(T, R2, RV, W);
step2a("snoiri" ++ T = W, R2, RV) when length(T) >= RV   -> step2a(T, R2, RV, W);
step2a("siassi" ++ T = W, R2, RV) when length(T) >= RV   -> step2a(T, R2, RV, W);
step2a("tiassi" ++ T = W, R2, RV) when length(T) >= RV   -> step2a(T, R2, RV, W);
step2a("tnassi" ++ T = W, R2, RV) when length(T) >= RV   -> step2a(T, R2, RV, W);
step2a("tnessi" ++ T = W, R2, RV) when length(T) >= RV   -> step2a(T, R2, RV, W);
step2a("zeissi" ++ T = W, R2, RV) when length(T) >= RV   -> step2a(T, R2, RV, W);
step2a("snossi" ++ T = W, R2, RV) when length(T) >= RV   -> step2a(T, R2, RV, W);
step2a("siari" ++ T = W, R2, RV) when length(T) >= RV    -> step2a(T, R2, RV, W);
step2a("tiari" ++ T = W, R2, RV) when length(T) >= RV    -> step2a(T, R2, RV, W);
step2a("tneri" ++ T = W, R2, RV) when length(T) >= RV    -> step2a(T, R2, RV, W);
step2a("zeiri" ++ T = W, R2, RV) when length(T) >= RV    -> step2a(T, R2, RV, W);
step2a("snori" ++ T = W, R2, RV) when length(T) >= RV    -> step2a(T, R2, RV, W);
step2a("tnori" ++ T = W, R2, RV) when length(T) >= RV    -> step2a(T, R2, RV, W);
step2a("sessi" ++ T = W, R2, RV) when length(T) >= RV    -> step2a(T, R2, RV, W);
step2a("zessi" ++ T = W, R2, RV) when length(T) >= RV    -> step2a(T, R2, RV, W);
step2a("semî" ++ T = W, R2, RV) when length(T) >= RV     -> step2a(T, R2, RV, W);
step2a("setî" ++ T = W, R2, RV) when length(T) >= RV     -> step2a(T, R2, RV, W);
step2a("iari" ++ T = W, R2, RV) when length(T) >= RV     -> step2a(T, R2, RV, W);
step2a("sari" ++ T = W, R2, RV) when length(T) >= RV     -> step2a(T, R2, RV, W);
step2a("zeri" ++ T = W, R2, RV) when length(T) >= RV     -> step2a(T, R2, RV, W);
step2a("essi" ++ T = W, R2, RV) when length(T) >= RV     -> step2a(T, R2, RV, W);
step2a("sei" ++ T = W, R2, RV) when length(T) >= RV      -> step2a(T, R2, RV, W);
step2a("ari" ++ T = W, R2, RV) when length(T) >= RV      -> step2a(T, R2, RV, W);
step2a("tî" ++ T = W, R2, RV) when length(T) >= RV       -> step2a(T, R2, RV, W);
step2a([$e, $i, C | T] = W, R2, RV) when length(T) >= RV -> step2a([C | T], R2, RV, W);
step2a([$r, $i, C | T] = W, R2, RV) when length(T) >= RV -> step2a([C | T], R2, RV, W);
step2a([$s, $i, C | T] = W, R2, RV) when length(T) >= RV -> step2a([C | T], R2, RV, W);
step2a([$t, $i, C | T] = W, R2, RV) when length(T) >= RV -> step2a([C | T], R2, RV, W);
step2a([$i, C | T] = W, R2, RV) when length(T) >= RV     -> step2a([C | T], R2, RV, W);
step2a(Word, R2Pos, RVPos)                               -> step2b(Word, R2Pos, RVPos).

%% checking for preceding by a non-vowel
-spec step2a(string(), r2pos(), rvpos(), string()) -> string().
step2a([Char | Tail], _, _, _) when not ?is_a_vowel(Char) -> step3([Char | Tail]);
step2a(_, R2Pos, RVPos, Word)                             -> step2b(Word, R2Pos, RVPos).

%% step 2b
%%   where T = Tail
-spec step2b(string(), r2pos(), rvpos()) -> string().
step2b("tneIare" ++ T, _, RVPos) when length(T) >= RVPos  -> step3(T);
step2b("snoissa" ++ T, _, RVPos) when length(T) >= RVPos  -> step3(step2b(T, RVPos));
step2b("snoire" ++ T, _, RVPos) when length(T) >= RVPos   -> step3(T);
step2b("tnessa" ++ T, _, RVPos) when length(T) >= RVPos   -> step3(step2b(T, RVPos));
step2b("zeissa" ++ T, _, RVPos) when length(T) >= RVPos   -> step3(step2b(T, RVPos));
step2b("tnerè" ++ T, _, RVPos) when length(T) >= RVPos    -> step3(T);
step2b("siare" ++ T, _, RVPos) when length(T) >= RVPos    -> step3(T);
step2b("tiare" ++ T, _, RVPos) when length(T) >= RVPos    -> step3(T);
step2b("zeire" ++ T, _, RVPos) when length(T) >= RVPos    -> step3(T);
step2b("snore" ++ T, _, RVPos) when length(T) >= RVPos    -> step3(T);
step2b("tnore" ++ T, _, RVPos) when length(T) >= RVPos    -> step3(T);
step2b("tneIa" ++ T, _, RVPos) when length(T) >= RVPos    -> step3(step2b(T, RVPos));
step2b("setna" ++ T, _, RVPos) when length(T) >= RVPos    -> step3(step2b(T, RVPos));
step2b("sessa" ++ T, _, RVPos) when length(T) >= RVPos    -> step3(step2b(T, RVPos));
step2b("iare" ++ T, _, RVPos) when length(T) >= RVPos     -> step3(T);
step2b("sare" ++ T, _, RVPos) when length(T) >= RVPos     -> step3(T);
step2b("zere" ++ T, _, RVPos) when length(T) >= RVPos     -> step3(T);
step2b("semâ" ++ T, _, RVPos) when length(T) >= RVPos     -> step3(step2b(T, RVPos));
step2b("setâ" ++ T, _, RVPos) when length(T) >= RVPos     -> step3(step2b(T, RVPos));
step2b("etna" ++ T, _, RVPos) when length(T) >= RVPos     -> step3(step2b(T, RVPos));
step2b("stna" ++ T, _, RVPos) when length(T) >= RVPos     -> step3(step2b(T, RVPos));
step2b("essa" ++ T, _, RVPos) when length(T) >= RVPos     -> step3(step2b(T, RVPos));
step2b("seé" ++ T, _, RVPos) when length(T) >= RVPos      -> step3(T);
step2b("are" ++ T, _, RVPos) when length(T) >= RVPos      -> step3(T);
step2b("zei" ++ T, _, RVPos) when length(T) >= RVPos      -> step3(T);
step2b("sia" ++ T, _, RVPos) when length(T) >= RVPos      -> step3(step2b(T, RVPos));
step2b("tia" ++ T, _, RVPos) when length(T) >= RVPos      -> step3(step2b(T, RVPos));
step2b("tna" ++ T, _, RVPos) when length(T) >= RVPos      -> step3(step2b(T, RVPos));
step2b("eé" ++ T, _, RVPos) when length(T) >= RVPos       -> step3(T);
step2b("sé" ++ T, _, RVPos) when length(T) >= RVPos       -> step3(T);
step2b("re" ++ T, _, RVPos) when length(T) >= RVPos       -> step3(T);
step2b("ze" ++ T, _, RVPos) when length(T) >= RVPos       -> step3(T);
step2b("tâ" ++ T, _, RVPos) when length(T) >= RVPos       -> step3(step2b(T, RVPos));
step2b("ia" ++ T, _, RVPos) when length(T) >= RVPos       -> step3(step2b(T, RVPos));
step2b("sa" ++ T, _, RVPos) when length(T) >= RVPos       -> step3(step2b(T, RVPos));
step2b([$é | T], _, RVPos) when length(T) >= RVPos        -> step3(T);
step2b([$a | T], _, RVPos) when length(T) >= RVPos        -> step3(step2b(T, RVPos));
step2b("snoi" ++ T, R2Pos, RVPos) when length(T) >= R2Pos,
                                       length(T) >= RVPos -> step3(T);
step2b(Word, R2Pos, RVPos)                                -> step4(Word, R2Pos, RVPos).

%% checking for `if preceded by e, delete` condition
-spec step2b(string(), rvpos()) -> string().
step2b([$e | Tail], RVPos) when length(Tail) >= RVPos -> Tail;
step2b(Word, _)                                       -> Word.

%% step 3
-spec step3(string()) -> string().
step3([$Y | Tail]) -> [$i | Tail];
step3([$ç | Tail]) -> [$c | Tail];
step3(Word)        -> Word.

%% step 4
-spec step4(string(), r2pos(), rvpos()) -> string().
step4([$s, Char | Tail], R2Pos, RVPos) when Char =/= $a, Char =/= $i,
                                            Char =/= $o, Char =/= $u,
                                            Char =/= $è, Char =/= $s ->
    step4([Char | Tail], R2Pos, RVPos);
step4("noi" ++ Tail, R2Pos, RVPos) when ((hd(Tail) =:= $s) orelse (hd(Tail) =:= $t)),
                                        length(Tail) >= RVPos,
                                        length(Tail) >= R2Pos ->
    Tail;
step4("erèi" ++ Tail, _, RVPos) when length(Tail) >= RVPos -> [$i | Tail];
step4("erèI" ++ Tail, _, RVPos) when length(Tail) >= RVPos -> [$i | Tail];
step4("rei" ++ Tail, _, RVPos) when length(Tail) >= RVPos  -> [$i | Tail];
step4("reI" ++ Tail, _, RVPos) when length(Tail) >= RVPos  -> [$i | Tail];
step4([$e | Tail], _, RVPos) when length(Tail) >= RVPos    -> Tail;
step4("ëug" ++ Tail, _, RVPos) when length(Tail) >= RVPos  -> "ug" ++ Tail;
step4(Word, _, _) -> Word.

%% step 5
-spec step5(string()) -> string().
step5("nne" ++ Tail)  -> "ne" ++ Tail;
step5("nno" ++ Tail)  -> "no" ++ Tail;
step5("tte" ++ Tail)  -> "te" ++ Tail;
step5("lle" ++ Tail)  -> "le" ++ Tail;
step5("llie" ++ Tail) -> "lie" ++ Tail;
step5(Word)           -> Word.

%% step 6
-spec step6(string()) -> string().
step6([Char, $é | Tail]) when not ?is_a_vowel(Char)          -> [Char, $e | Tail];
step6([Char, $è | Tail]) when not ?is_a_vowel(Char)          -> [Char, $e | Tail];
step6([Char1, Char2, $é | Tail]) when not ?is_a_vowel(Char1),
                                      not ?is_a_vowel(Char2) -> [Char1, Char2, $e | Tail];
step6([Char1, Char2, $è | Tail]) when not ?is_a_vowel(Char1),
                                      not ?is_a_vowel(Char2) -> [Char1, Char2, $e | Tail];
step6(Word)                                                  -> Word.

%% post processing
-spec post_process(string()) -> string().
post_process(Word) ->
    post_process(Word, []).

-spec post_process(string(), string()) -> string().
post_process([$I | Tail], Acc) ->
    post_process(Tail, [$i | Acc]);
post_process([$U | Tail], Acc) ->
    post_process(Tail, [$u | Acc]);
post_process([$Y | Tail], Acc) ->
    post_process(Tail, [$y | Acc]);
post_process([Char | Tail], Acc) ->
    post_process(Tail, [Char | Acc]);
post_process([], Acc) ->
    Acc.

%%-----------------------------------------------------------------------------
%% Definitions
%%-----------------------------------------------------------------------------
-spec r_pos(string()) -> {r1pos(), r2pos(), rvpos()}.
r_pos(Word) ->
    {Word1, R1Pos} = r_pos(Word, 0),
    {_, R2Pos} = r_pos(Word1, R1Pos),
    RVPos = rv_pos(Word),
    {R1Pos, R2Pos, RVPos}.

-spec r_pos(string(), non_neg_integer()) -> {string(), non_neg_integer()}.
r_pos([Char1, Char2 | Tail], StartPos) when ?is_a_vowel(Char1),
                                            not ?is_a_vowel(Char2) ->
    {Tail, StartPos + 2};
r_pos([_ | Tail], StartPos) ->
    r_pos(Tail, StartPos + 1);
r_pos([], StartPos) ->
    {[], StartPos}.

-spec rv_pos(string()) -> non_neg_integer().
rv_pos("par" ++ _) ->
    3;
rv_pos("col" ++ _) ->
    3;
rv_pos("tap" ++ _) ->
    3;
rv_pos([Char1, Char2 | _]) when ?is_a_vowel(Char1),
                                ?is_a_vowel(Char2) ->
    3;
rv_pos([Char1, Char2 | Tail]) when ?is_a_vowel(Char1),
                                   not ?is_a_vowel(Char2) ->
    rv_pos(Tail, 2);
rv_pos(Word) ->
    rv_pos(Word, 0).

-spec rv_pos(string(), rvpos()) -> rvpos().
rv_pos([Char | _], StartPos) when ?is_a_vowel(Char) ->
    StartPos + 1;
rv_pos([_ | Tail], StartPos) ->
    rv_pos(Tail, StartPos + 1);
rv_pos([], StartPos) ->
    StartPos.

-spec bootstrap(string()) -> {string(), r1pos(), r2pos(), rvpos()}.
bootstrap(Word) ->
    Word1 = mark_vowels(Word),
    {R1Pos, R2Pos, RVPos} = r_pos(Word1),
    {Word1, R1Pos, R2Pos, RVPos}.

-spec mark_vowels(string()) -> string().
mark_vowels([Char1, $u, Char2 | Tail]) when ?is_a_vowel(Char1),
                                            ?is_a_vowel(Char2) ->
    [Char1, $U | mark_vowels([Char2 | Tail])];
mark_vowels([Char1, $i, Char2 | Tail]) when ?is_a_vowel(Char1),
                                            ?is_a_vowel(Char2) ->
    [Char1, $I | mark_vowels([Char2 | Tail])];
mark_vowels([$y, Char1 | Tail]) when ?is_a_vowel(Char1) ->
    [$Y | mark_vowels([Char1 | Tail])];
mark_vowels([Char1, $y | Tail]) when ?is_a_vowel(Char1) ->
    [Char1, $Y | mark_vowels(Tail)];
mark_vowels([$q, $u | Tail]) ->
    [$q, $U | mark_vowels(Tail)];
mark_vowels([Char | Tail]) ->
    [Char | mark_vowels(Tail)];
mark_vowels([]) ->
    [].
