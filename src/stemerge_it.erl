%%-----------------------------------------------------------------------------
%% @author Roman Parykin <donderom@ymail.com>
%% @doc
%% The implementation of the italian stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/italian/stemmer.html">
%% The Italian stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_it).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
-export([stem/1]).

%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------
-define(is_a_vowel(Char),
        ((Char =:= $a)
         or (Char =:= $e)
         or (Char =:= $i)
         or (Char =:= $o)
         or (Char =:= $u)
         or (Char =:= $à)
         or (Char =:= $è)
         or (Char =:= $ì)
         or (Char =:= $ò)
         or (Char =:= $ù))).

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------
-type r1pos() :: non_neg_integer().
-type r2pos() :: non_neg_integer().
-type rvpos() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

%% @doc
%% Returns the stem of a word.
-spec stem(string()) -> string().
stem(Word) ->
    {Word1, R1Pos, R2Pos, RVPos} = bootstrap(Word),
    ReversedWord = lists:reverse(Word1),
    stem(ReversedWord, R1Pos, R2Pos, RVPos).

-spec stem(string(), r1pos(), r2pos(), rvpos()) -> string().
stem(ReversedWord, R1Pos, R2Pos, RVPos) ->
    Step0 = step0(ReversedWord, RVPos),
    Step1 = step1(Step0, R1Pos, R2Pos, RVPos),
    Step3a = step3a(Step1, RVPos),
    Step3b = step3b(Step3a, RVPos),
    post_process(Step3b).

%%-----------------------------------------------------------------------------
%% Steps
%%-----------------------------------------------------------------------------

%% step 0
-spec step0(string(), rvpos()) -> string().
step0("aleilg" ++ Tail = Word, RVPos) when length(Tail) >= RVPos -> step0(Tail, Word, RVPos);
step0("eleilg" ++ Tail = Word, RVPos) when length(Tail) >= RVPos -> step0(Tail, Word, RVPos);
step0("ileilg" ++ Tail = Word, RVPos) when length(Tail) >= RVPos -> step0(Tail, Word, RVPos);
step0("oleilg" ++ Tail = Word, RVPos) when length(Tail) >= RVPos -> step0(Tail, Word, RVPos);
step0("eneilg" ++ Tail = Word, RVPos) when length(Tail) >= RVPos -> step0(Tail, Word, RVPos);
step0("enes" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("alem" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("elem" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("ilem" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("olem" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("enem" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("alet" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("elet" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("ilet" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("olet" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("enet" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("alec" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("elec" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("ilec" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("olec" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("enec" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("alev" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("elev" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("ilev" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("olev" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("enev" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step0(Tail, Word, RVPos);
step0("ilg" ++ Tail = Word, RVPos) when length(Tail) >= RVPos    -> step0(Tail, Word, RVPos);
step0("ic" ++ Tail = Word, RVPos) when length(Tail) >= RVPos     -> step0(Tail, Word, RVPos);
step0("al" ++ Tail = Word, RVPos) when length(Tail) >= RVPos     -> step0(Tail, Word, RVPos);
step0("el" ++ Tail = Word, RVPos) when length(Tail) >= RVPos     -> step0(Tail, Word, RVPos);
step0("il" ++ Tail = Word, RVPos) when length(Tail) >= RVPos     -> step0(Tail, Word, RVPos);
step0("ol" ++ Tail = Word, RVPos) when length(Tail) >= RVPos     -> step0(Tail, Word, RVPos);
step0("im" ++ Tail = Word, RVPos) when length(Tail) >= RVPos     -> step0(Tail, Word, RVPos);
step0("en" ++ Tail = Word, RVPos) when length(Tail) >= RVPos     -> step0(Tail, Word, RVPos);
step0("is" ++ Tail = Word, RVPos) when length(Tail) >= RVPos     -> step0(Tail, Word, RVPos);
step0("it" ++ Tail = Word, RVPos) when length(Tail) >= RVPos     -> step0(Tail, Word, RVPos);
step0("iv" ++ Tail = Word, RVPos) when length(Tail) >= RVPos     -> step0(Tail, Word, RVPos);
step0(Word, _)                                                   -> Word.

%% step 0
-spec step0(string(), string(), rvpos()) -> string().
step0("odna" ++ Tail = T, _, RVPos) when length(Tail) >= RVPos -> T;
step0("odne" ++ Tail = T, _, RVPos) when length(Tail) >= RVPos -> T;
step0("ra" ++ Tail, _, RVPos) when length(Tail) >= RVPos       -> "era" ++ Tail;
step0("re" ++ Tail, _, RVPos) when length(Tail) >= RVPos       -> "ere" ++ Tail;
step0("ri" ++ Tail, _, RVPos) when length(Tail) >= RVPos       -> "eri" ++ Tail;
step0(_, Word, _)                                              -> Word.

%% step 1
-spec step1(string(), r1pos(), r2pos(), rvpos()) -> string().
step1("etnema" ++ Tail, R1Pos, R2Pos, _) when length(Tail) >= R1Pos -> step1b(Tail, R2Pos);

step1("ecirta" ++ Tail = Word, _, R2Pos, _) -> step1e(Tail, Word, R2Pos);
step1("icirta" ++ Tail = Word, _, R2Pos, _) -> step1e(Tail, Word, R2Pos);

step1("eliba" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> Tail;
step1("iliba" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> Tail;
step1("elibi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> Tail;
step1("ilibi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> Tail;
step1("etnem" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> Tail;
step1("azna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("ezna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("ehci" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("ihci" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("omsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("imsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("atsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("etsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("itsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("àtsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("ètsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("ìtsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("etna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("itna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("oci" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> Tail;
step1("ici" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> Tail;
step1("aci" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> Tail;
step1("eci" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> Tail;
step1("oso" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> Tail;
step1("iso" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> Tail;
step1("aso" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> Tail;
step1("eso" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> Tail;

step1("enoiza" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1a(Tail, R2Pos);
step1("inoiza" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1a(Tail, R2Pos);
step1("erota" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step1a(Tail, R2Pos);
step1("irota" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step1a(Tail, R2Pos);

step1("aigol" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> "gol" ++ Tail;
step1("eigol" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> "gol" ++ Tail;

step1("enoizu" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> [$u | Tail];
step1("inoizu" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> [$u | Tail];
step1("enoisu" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> [$u | Tail];
step1("inoisu" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> [$u | Tail];

step1("azne" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> "etne" ++ Tail;
step1("ezne" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> "etne" ++ Tail;

step1("otnema" ++ Tail, _, _, RVPos) when length(Tail) >= RVPos -> Tail;
step1("itnema" ++ Tail, _, _, RVPos) when length(Tail) >= RVPos -> Tail;
step1("otnemi" ++ Tail, _, _, RVPos) when length(Tail) >= RVPos -> Tail;
step1("itnemi" ++ Tail, _, _, RVPos) when length(Tail) >= RVPos -> Tail;

step1("àti" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1d(Tail, R2Pos);

step1("ovi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1a(step1c(Tail, R2Pos), R2Pos);
step1("ivi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1a(step1c(Tail, R2Pos), R2Pos);
step1("avi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1a(step1c(Tail, R2Pos), R2Pos);
step1("evi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1a(step1c(Tail, R2Pos), R2Pos);

step1(Word, _, _, RVPos) -> step2(Word, RVPos).

%% step 1a
-spec step1a(string(), r2pos()) -> string().
step1a("ci" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1a(Word, _)                                        -> Word.

%% step 1b
-spec step1b(string(), r2pos()) -> string().
step1b("vi" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> step1c(Tail, R2Pos);
step1b("so" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step1b("ci" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step1b("liba" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1b(Word, _)                                          -> Word.

%% step 1c
-spec step1c(string(), r2pos()) -> string().
step1c("ta" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1c(Word, _)                                        -> Word.

%% step 1d
-spec step1d(string(), r2pos()) -> string().
step1d("vi" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step1d("ci" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step1d("liba" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1d(Word, _)                                          -> Word.

%% step 1e
-spec step1e(string(), string(), r2pos()) -> string().
step1e(Tail, _, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1e(_, Word, _)                                -> Word.

%% step 2
-spec step2(string(), rvpos()) -> string().
step2("orebbere" ++ Tail, RVPos) when length(Tail) >= RVPos -> Tail;
step2("orebberi" ++ Tail, RVPos) when length(Tail) >= RVPos -> Tail;
step2("oressa" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("omissa" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("onnare" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("ebbere" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("ommere" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("etsere" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("itsere" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("oresse" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("onnari" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("ebberi" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("ommeri" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("etseri" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("itseri" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("onacsi" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("onocsi" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("oressi" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2("onora" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("omava" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("onava" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("etava" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("omere" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("etere" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("onore" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("omave" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("onave" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("etave" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("omeri" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("eteri" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("onori" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("omavi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("onavi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("etavi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2("omma" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("odna" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("essa" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("issa" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("omme" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("adne" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("edne" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("idne" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("odne" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("iare" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("iere" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("omaY" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("omai" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("iari" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("ommi" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("ieri" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("acsi" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("ecsi" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("icsi" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("ocsi" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2("ona" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("era" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ata" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("eta" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ita" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ota" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ava" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("iva" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ova" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("àre" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ere" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("òre" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ete" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ave" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ive" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ove" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("àri" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("eri" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("òri" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ati" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("eti" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("iti" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("oti" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("avi" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ivi" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ovi" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ono" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("atu" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("etu" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("itu" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("otu" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2("ra" ++ Tail, RVPos) when length(Tail) >= RVPos       -> Tail;
step2("ri" ++ Tail, RVPos) when length(Tail) >= RVPos       -> Tail;
step2(Word, _)                                              -> Word.

%% step 3a
-spec step3a(string(), rvpos()) -> string().
step3a([$a | Tail], RVPos) when length(Tail) >= RVPos -> after_step3a(Tail, RVPos);
step3a([$e | Tail], RVPos) when length(Tail) >= RVPos -> after_step3a(Tail, RVPos);
step3a([$i | Tail], RVPos) when length(Tail) >= RVPos -> after_step3a(Tail, RVPos);
step3a([$o | Tail], RVPos) when length(Tail) >= RVPos -> after_step3a(Tail, RVPos);
step3a([$à | Tail], RVPos) when length(Tail) >= RVPos -> after_step3a(Tail, RVPos);
step3a([$è | Tail], RVPos) when length(Tail) >= RVPos -> after_step3a(Tail, RVPos);
step3a([$ì | Tail], RVPos) when length(Tail) >= RVPos -> after_step3a(Tail, RVPos);
step3a([$ò | Tail], RVPos) when length(Tail) >= RVPos -> after_step3a(Tail, RVPos);
step3a(Word, _)                                       -> Word.

%% after step 3a
-spec after_step3a(string(), rvpos) -> string().
after_step3a([$i | Tail], RVPos) when length(Tail) >= RVPos -> Tail;
after_step3a(Word, _)                                       -> Word.

%% step 3b
-spec step3b(string(), rvpos()) -> string().
step3b("hc" ++ Tail, RVPos) when length(Tail) >= RVPos -> [$c | Tail];
step3b("hg" ++ Tail, RVPos) when length(Tail) >= RVPos -> [$g | Tail];
step3b(Word, _)                                        -> Word.

%% post processing
-spec post_process(string()) -> string().
post_process(Word) ->
    post_process(Word, []).

-spec post_process(string(), string()) -> string().
post_process([$I | Tail], Acc) ->
    post_process(Tail, [$i | Acc]);
post_process([$U | Tail], Acc) ->
    post_process(Tail, [$u | Acc]);
post_process([Char | Tail], Acc) ->
    post_process(Tail, [Char | Acc]);
post_process([], Acc) ->
    Acc.

%%-----------------------------------------------------------------------------
%% Definitions
%%-----------------------------------------------------------------------------
-spec bootstrap(string()) -> {string(), r1pos(), r2pos(), rvpos()}.
bootstrap(Word) ->
    Word1 = mark_vowels(replace_accents(Word)),
    {R1Pos, R2Pos, RVPos} = r_pos(Word1),
    {Word1, R1Pos, R2Pos, RVPos}.

-spec replace_accents(string()) -> string().
replace_accents([$á | Tail]) ->
    [$à | replace_accents(Tail)];
replace_accents([$é | Tail]) ->
    [$è | replace_accents(Tail)];
replace_accents([$í | Tail]) ->
    [$ì | replace_accents(Tail)];
replace_accents([$ó | Tail]) ->
    [$ò | replace_accents(Tail)];
replace_accents([$ú | Tail]) ->
    [$ù | replace_accents(Tail)];
replace_accents([Char | Tail]) ->
    [Char | replace_accents(Tail)];
replace_accents([]) ->
    [].

-spec mark_vowels(string()) -> string().
mark_vowels([Char1, $u, Char2 | Tail]) when ?is_a_vowel(Char1),
                                            ?is_a_vowel(Char2) ->
    [Char1, $U | mark_vowels([Char2 | Tail])];
mark_vowels([Char1, $i, Char2 | Tail]) when ?is_a_vowel(Char1),
                                            ?is_a_vowel(Char2) ->
    [Char1, $I | mark_vowels([Char2 | Tail])];
mark_vowels([$q, $u | Tail]) ->
    [$q, $U | mark_vowels(Tail)];
mark_vowels([Char | Tail]) ->
    [Char | mark_vowels(Tail)];
mark_vowels([]) ->
    [].

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

-spec rv_pos(string()) -> string().
rv_pos([_, Char1 | Tail]) when not ?is_a_vowel(Char1) ->
    rv_pos(vowel, Tail, 2);
rv_pos([Char1, Char2 | Tail]) when ?is_a_vowel(Char1),
                                   ?is_a_vowel(Char2) ->
    rv_pos(consonant, Tail, 2);
rv_pos(Word) when length(Word) > 3 ->
    3;
rv_pos(Word) ->
    length(Word).

-spec rv_pos(atom(), string(), rvpos()) -> rvpos().
rv_pos(vowel, [Char | _], StartPos) when ?is_a_vowel(Char) ->
    StartPos + 1;
rv_pos(consonant, [Char | _], StartPos) when not ?is_a_vowel(Char) ->
    StartPos + 1;
rv_pos(Type, [_ | Tail], StartPos) ->
    rv_pos(Type, Tail, StartPos + 1);
rv_pos(_, [], StartPos) ->
    StartPos.
