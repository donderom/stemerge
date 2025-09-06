%%-----------------------------------------------------------------------------
%% @author Roman Parykin <github@donderom.com>
%% @doc
%% The implementation of the portuguese stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/portuguese/stemmer.html">
%% The Portuguese stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_pt).

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
         orelse (Char =:= $á)
         orelse (Char =:= $é)
         orelse (Char =:= $í)
         orelse (Char =:= $ó)
         orelse (Char =:= $ú)
         orelse (Char =:= $â)
         orelse (Char =:= $ê)
         orelse (Char =:= $ô))).

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
    Step1 = step1(ReversedWord, R1Pos, R2Pos, RVPos),
    Step5 = step5(Step1, RVPos),
    post_process(Step5).

%%-----------------------------------------------------------------------------
%% Steps
%%-----------------------------------------------------------------------------

-spec step1(string(), r1pos(), r2pos(), rvpos()) -> string().
step1("sotnema" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos -> step3(Tail, RVPos);
step1("sotnemi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos -> step3(Tail, RVPos);
step1("otnema" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos  -> step3(Tail, RVPos);
step1("otnemi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos  -> step3(Tail, RVPos);
step1("saroda" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos  -> step3(Tail, RVPos);
step1("seroda" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos  -> step3(Tail, RVPos);
step1("se~oça" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos  -> step3(Tail, RVPos);
step1("somsi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos   -> step3(Tail, RVPos);
step1("satsi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos   -> step3(Tail, RVPos);
step1("aroda" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos   -> step3(Tail, RVPos);
step1("o~aça" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos   -> step3(Tail, RVPos);
step1("setna" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos   -> step3(Tail, RVPos);
step1("aicnâ" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos   -> step3(Tail, RVPos);
step1("saze" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("soci" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("saci" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("omsi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("levá" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("leví" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("atsi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("soso" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("saso" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("roda" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("etna" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos    -> step3(Tail, RVPos);
step1("aze" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos     -> step3(Tail, RVPos);
step1("oci" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos     -> step3(Tail, RVPos);
step1("aci" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos     -> step3(Tail, RVPos);
step1("oso" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos     -> step3(Tail, RVPos);
step1("aso" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos     -> step3(Tail, RVPos);

step1("aígol" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos  -> step3("gol" ++ Tail, RVPos);
step1("saígol" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos -> step3("gol" ++ Tail, RVPos);

step1("nóicu" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos   -> step3([$u | Tail], RVPos);
step1("senoicu" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos -> step3([$u | Tail], RVPos);

step1("aicnê" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos  -> step3("etne" ++ Tail, RVPos);
step1("saicnê" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos -> step3("etne" ++ Tail, RVPos);

step1("etnema" ++ Tail, R1Pos, R2Pos, RVPos) when length(Tail) >= R1Pos -> step3(step1a(Tail, R2Pos), RVPos);

step1("etnem" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos -> step3(step1c(Tail, R2Pos), RVPos);

step1("sedadi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos -> step3(step1d(Tail, R2Pos), RVPos);
step1("edadi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos  -> step3(step1d(Tail, R2Pos), RVPos);

step1("savi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos -> step3(step1b(Tail, R2Pos), RVPos);
step1("sovi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos -> step3(step1b(Tail, R2Pos), RVPos);
step1("avi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos  -> step3(step1b(Tail, R2Pos), RVPos);
step1("ovi" ++ Tail, _, R2Pos, RVPos) when length(Tail) >= R2Pos  -> step3(step1b(Tail, R2Pos), RVPos);

step1("sari" ++ Tail, _, _, RVPos) when length(Tail) >= RVPos,
                                        hd(Tail) =:= $e -> "ri" ++ Tail;
step1("ari" ++ Tail, _, _, RVPos) when length(Tail) >= RVPos,
                                        hd(Tail) =:= $e -> "ri" ++ Tail;

step1(Word, _, _, RVPos) -> step2(Word, RVPos).

%% step 1a
-spec step1a(string(), r2pos()) -> string().
step1a("vi" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> step1b(Tail, R2Pos);
step1a("so" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1a("ci" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1a("da" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1a(Word, _)                                        -> Word.

%% step 1b
-spec step1b(string(), r2pos()) -> string().
step1b("ta" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1b(Word, _)                                        -> Word.

%% step 1c
-spec step1c(string(), r2pos()) -> string().
step1c("etna" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1c("leva" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1c("leví" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1c(Word, _)                                          -> Word.

%% step 1d
-spec step1d(string(), r2pos()) -> string().
step1d("liba" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1d("ci" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step1d("vi" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step1d(Word, _)                                          -> Word.

%% step 2
-spec step2(string(), rvpos()) -> string().
step2("somaíra" ++ Tail, RVPos) when length(Tail) >= RVPos -> step3(Tail, RVPos);
step2("somaíre" ++ Tail, RVPos) when length(Tail) >= RVPos -> step3(Tail, RVPos);
step2("somaíri" ++ Tail, RVPos) when length(Tail) >= RVPos -> step3(Tail, RVPos);
step2("somessá" ++ Tail, RVPos) when length(Tail) >= RVPos -> step3(Tail, RVPos);
step2("somessê" ++ Tail, RVPos) when length(Tail) >= RVPos -> step3(Tail, RVPos);
step2("somessí" ++ Tail, RVPos) when length(Tail) >= RVPos -> step3(Tail, RVPos);
step2("sieíra" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("sieíre" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("sieíri" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("siessá" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("siessê" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("siessí" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("somará" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("somaré" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("somarí" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("somavá" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("somera" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("somere" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("someri" ++ Tail, RVPos) when length(Tail) >= RVPos  -> step3(Tail, RVPos);
step2("maira" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("maire" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("mairi" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("messa" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("messe" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("messi" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("o~ara" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("o~are" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("o~ari" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("saira" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("saire" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sairi" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sedra" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sedre" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sedri" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sessa" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sesse" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sessi" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("setsa" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("setse" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("setsi" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sierá" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("siera" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sieré" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("siere" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sierí" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sieri" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("sievá" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("somaí" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("somra" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("somre" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("somri" ++ Tail, RVPos) when length(Tail) >= RVPos   -> step3(Tail, RVPos);
step2("aira" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("aire" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("airi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("essa" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("esse" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("essi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("etsa" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("etse" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("etsi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("iera" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("iere" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("ieri" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("mara" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("mare" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("mari" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("mava" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("mera" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("mere" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("meri" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("odna" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("odne" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("odni" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sada" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sadi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sára" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sara" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sáre" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sare" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sári" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sava" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sera" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sere" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("seri" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sieí" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("soda" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sodi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("somá" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("soma" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("some" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("somi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("sari" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step3(Tail, RVPos);
step2("ada" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("adi" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("ára" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("ara" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("áre" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("are" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("ári" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("ava" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("mai" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("oda" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("odi" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("sai" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("sia" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("sie" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("ari" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step3(Tail, RVPos);
step2("ai" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("ie" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("ma" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("me" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("ra" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("re" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("ri" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("sa" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("se" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("si" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("ue" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("ui" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2("uo" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step3(Tail, RVPos);
step2(Word, RVPos)                                         -> step4(Word, RVPos).

%% step 3
-spec step3(string(), rvpos()) -> string().
step3([$i | Tail], RVPos) when length(Tail) >= RVPos,
                               hd(Tail) =:= $c -> Tail;
step3(Word, _)                                 -> Word.

%% step 4
-spec step4(string(), rvpos()) -> string().
step4("so" ++ Tail, RVPos) when length(Tail) >= RVPos -> Tail;
step4("a" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step4("i" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step4("o" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step4("á" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step4("í" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step4("ó" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step4(Word, _)                                        -> Word.

%% step 5
-spec step5(string(), rvpos()) -> string().
step5("eu" ++ Tail, RVPos) when length(Tail) >= RVPos,
                                hd(Tail) =:= $g      -> Tail;
step5("éu" ++ Tail, RVPos) when length(Tail) >= RVPos,
                                hd(Tail) =:= $g      -> Tail;
step5("êu" ++ Tail, RVPos) when length(Tail) >= RVPos,
                                hd(Tail) =:= $g      -> Tail;
step5("ei" ++ Tail, RVPos) when length(Tail) >= RVPos,
                                hd(Tail) =:= $c      -> Tail;
step5("éi" ++ Tail, RVPos) when length(Tail) >= RVPos,
                                hd(Tail) =:= $c      -> Tail;
step5("êi" ++ Tail, RVPos) when length(Tail) >= RVPos,
                                hd(Tail) =:= $c      -> Tail;
step5([$e | Tail], RVPos) when length(Tail) >= RVPos -> Tail;
step5([$é | Tail], RVPos) when length(Tail) >= RVPos -> Tail;
step5([$ê | Tail], RVPos) when length(Tail) >= RVPos -> Tail;
step5([$ç | Tail], _)                                -> [$c | Tail];
step5(Word, _)                                       -> Word.

%% post processing
-spec post_process(string()) -> string().
post_process(Word) ->
    post_process(Word, []).

-spec post_process(string(), string()) -> string().
post_process("~a" ++ Tail, Acc) ->
    post_process(Tail, [$ã | Acc]);
post_process("~o" ++ Tail, Acc) ->
    post_process(Tail, [$õ | Acc]);
post_process([Char | Tail], Acc) ->
    post_process(Tail, [Char | Acc]);
post_process([], Acc) ->
    Acc.

%%-----------------------------------------------------------------------------
%% Definitions
%%-----------------------------------------------------------------------------
-spec bootstrap(string()) -> {string(), r1pos(), r2pos(), rvpos()}.
bootstrap(Word) ->
    Word1 = mark_nasalised_vowels(Word),
    {R1Pos, R2Pos, RVPos} = r_pos(Word1),
    {Word1, R1Pos, R2Pos, RVPos}.

-spec mark_nasalised_vowels(string()) -> string().
mark_nasalised_vowels([$ã | Tail]) ->
    [$a, $~ | mark_nasalised_vowels(Tail)];
mark_nasalised_vowels([$õ | Tail]) ->
    [$o, $~ | mark_nasalised_vowels(Tail)];
mark_nasalised_vowels([Char | Tail]) ->
    [Char | mark_nasalised_vowels(Tail)];
mark_nasalised_vowels([]) ->
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

-spec rv_pos(string()) -> non_neg_integer().
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
