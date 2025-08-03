%%-----------------------------------------------------------------------------
%% @author Roman Parykin <donderom@ymail.com>
%% @doc
%% The implementation of the spanish stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/spanish/stemmer.html">
%% The Spanish stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_es).

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
         or (Char =:= $á)
         or (Char =:= $é)
         or (Char =:= $í)
         or (Char =:= $ó)
         or (Char =:= $ú)
         or (Char =:= $ü))).

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
    {R1Pos, R2Pos, RVPos} = r_pos(Word),
    ReversedWord = lists:reverse(Word),
    stem(ReversedWord, R1Pos, R2Pos, RVPos).

-spec stem(string(), r1pos(), r2pos(), rvpos()) -> string().
stem(ReversedWord, R1Pos, R2Pos, RVPos) ->
    Step0 = step0(ReversedWord, RVPos),
    Step1 = step1(Step0, R1Pos, R2Pos, RVPos),
    Step3 = step3(Step1, RVPos),
    post_process(Step3).

%%-----------------------------------------------------------------------------
%% Steps
%%-----------------------------------------------------------------------------

%% step 0
-spec step0(string(), rvpos()) -> string().
step0("em" ++ Tail = Word, RVPos)    -> step0(Tail, Word, RVPos);
step0("es" ++ Tail = Word, RVPos)    -> step0(Tail, Word, RVPos);
step0("el" ++ Tail = Word, RVPos)    -> step0(Tail, Word, RVPos);
step0("son" ++ Tail = Word, RVPos)   -> step0(Tail, Word, RVPos);
step0("sales" ++ Tail = Word, RVPos) -> step0(Tail, Word, RVPos);
step0("soles" ++ Tail = Word, RVPos) -> step0(Tail, Word, RVPos);
step0("ales" ++ Tail = Word, RVPos)  -> step0(Tail, Word, RVPos);
step0("oles" ++ Tail = Word, RVPos)  -> step0(Tail, Word, RVPos);
step0("sal" ++ Tail = Word, RVPos)   -> step0(Tail, Word, RVPos);
step0("sel" ++ Tail = Word, RVPos)   -> step0(Tail, Word, RVPos);
step0("sol" ++ Tail = Word, RVPos)   -> step0(Tail, Word, RVPos);
step0("al" ++ Tail = Word, RVPos)    -> step0(Tail, Word, RVPos);
step0("ol" ++ Tail = Word, RVPos)    -> step0(Tail, Word, RVPos);
step0(Word, _)                       -> Word.

-spec step0(string(), string(), rvpos()) -> string().
step0("odnéi" ++ Tail, _, RVPos) when length(Tail) >= RVPos -> "odnei" ++ Tail;
step0("odná" ++ Tail, _, RVPos) when length(Tail) >= RVPos  -> "odna" ++ Tail;
step0("rá" ++ Tail, _, RVPos) when length(Tail) >= RVPos    -> "ra" ++ Tail;
step0("ré" ++ Tail, _, RVPos) when length(Tail) >= RVPos    -> "re" ++ Tail;
step0("rí" ++ Tail, _, RVPos) when length(Tail) >= RVPos    -> "ri" ++ Tail;
step0("odnei" ++ Tail, _, RVPos) when length(Tail) >= RVPos -> "odnei" ++ Tail;
step0("odna" ++ Tail, _, RVPos) when length(Tail) >= RVPos  -> "odna" ++ Tail;
step0("ra" ++ Tail, _, RVPos) when length(Tail) >= RVPos    -> "ra" ++ Tail;
step0("re" ++ Tail, _, RVPos) when length(Tail) >= RVPos    -> "re" ++ Tail;
step0("ri" ++ Tail, _, RVPos) when length(Tail) >= RVPos    -> "ri" ++ Tail;
step0("odney" ++ Tail, _, RVPos) when length(Tail) >= RVPos,
                                      hd(Tail) =:= $u       -> "odney" ++ Tail;
step0(_, Word, _)                                           -> Word.

%% step 1
-spec step1(string(), r1pos(), r2pos(), rvpos()) -> string().
step1("sotneima" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> Tail;
step1("sotneimi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> Tail;
step1("otneima" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("otneimi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> Tail;
step1("sazna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos    -> Tail;
step1("somsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos    -> Tail;
step1("selba" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos    -> Tail;
step1("selbi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos    -> Tail;
step1("satsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos    -> Tail;
step1("azna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos     -> Tail;
step1("soci" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos     -> Tail;
step1("saci" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos     -> Tail;
step1("omsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos     -> Tail;
step1("elba" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos     -> Tail;
step1("elbi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos     -> Tail;
step1("atsi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos     -> Tail;
step1("soso" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos     -> Tail;
step1("saso" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos     -> Tail;
step1("oci" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos      -> Tail;
step1("aci" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos      -> Tail;
step1("oso" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos      -> Tail;
step1("aso" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos      -> Tail;

step1("senoica" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1a(Tail, R2Pos);
step1("saroda" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step1a(Tail, R2Pos);
step1("seroda" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step1a(Tail, R2Pos);
step1("saicna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step1a(Tail, R2Pos);
step1("aroda" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> step1a(Tail, R2Pos);
step1("nóica" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> step1a(Tail, R2Pos);
step1("setna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> step1a(Tail, R2Pos);
step1("aicna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> step1a(Tail, R2Pos);
step1("roda" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos    -> step1a(Tail, R2Pos);
step1("etna" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos    -> step1a(Tail, R2Pos);

step1("saígol" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> "gol" ++ Tail;
step1("aígol" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> "gol" ++ Tail;

step1("nóicu" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> [$u | Tail];
step1("senoicu" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> [$u | Tail];

step1("saicne" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> "etne" ++ Tail;
step1("aicne" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> "etne" ++ Tail;

step1("etnema" ++ Tail, R1Pos, R2Pos, _) when length(Tail) >= R1Pos -> step1b(Tail, R2Pos);

step1("etnem" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1d(Tail, R2Pos);

step1("sedadi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1e(Tail, R2Pos);
step1("dadi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos   -> step1e(Tail, R2Pos);

step1("savi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1c(Tail, R2Pos);
step1("sovi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos -> step1c(Tail, R2Pos);
step1("avi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step1c(Tail, R2Pos);
step1("ovi" ++ Tail, _, R2Pos, _) when length(Tail) >= R2Pos  -> step1c(Tail, R2Pos);

step1(Word, _, _, RVPos) -> step2a(Word, RVPos).

%% step 1a
-spec step1a(string(), r2pos()) -> string().
step1a("ci" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1a(Word, _)                                        -> Word.

%% step 1b
-spec step1b(string(), r2pos()) -> string().
step1b("vi" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> step1c(Tail, R2Pos);
step1b("so" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1b("ci" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1b("da" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1b(Word, _)                                        -> Word.

%% step 1c
-spec step1c(string(), r2pos()) -> string().
step1c("ta" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1c(Word, _)                                        -> Word.

%% step 1d
-spec step1d(string(), r2pos()) -> string().
step1d("etna" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1d("elba" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1d("elbi" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1d(Word, _)                                          -> Word.

%% step 1e
-spec step1e(string(), r2pos()) -> string().
step1e("liba" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step1e("ci" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step1e("vi" ++ Tail, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step1e(Word, _)                                          -> Word.

%% step 2a
-spec step2a(string(), rvpos()) -> string().
step2a("norey" ++ Tail = Word, RVPos) when length(Tail) >= RVPos -> step2a(Tail, Word, RVPos);
step2a("odney" ++ Tail = Word, RVPos) when length(Tail) >= RVPos -> step2a(Tail, Word, RVPos);
step2a("somay" ++ Tail = Word, RVPos) when length(Tail) >= RVPos -> step2a(Tail, Word, RVPos);
step2a("siay" ++ Tail = Word, RVPos) when length(Tail) >= RVPos  -> step2a(Tail, Word, RVPos);
step2a("nay" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step2a(Tail, Word, RVPos);
step2a("ney" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step2a(Tail, Word, RVPos);
step2a("say" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step2a(Tail, Word, RVPos);
step2a("sey" ++ Tail = Word, RVPos) when length(Tail) >= RVPos   -> step2a(Tail, Word, RVPos);
step2a("ay" ++ Tail = Word, RVPos) when length(Tail) >= RVPos    -> step2a(Tail, Word, RVPos);
step2a("ey" ++ Tail = Word, RVPos) when length(Tail) >= RVPos    -> step2a(Tail, Word, RVPos);
step2a("oy" ++ Tail = Word, RVPos) when length(Tail) >= RVPos    -> step2a(Tail, Word, RVPos);
step2a("óy" ++ Tail = Word, RVPos) when length(Tail) >= RVPos    -> step2a(Tail, Word, RVPos);
step2a(Word, RVPos)                                              -> step2b(Word, RVPos).

-spec step2a(string(), string(), rvpos()) -> string().
step2a([$u | Tail], _, _) -> [$u | Tail];
step2a(_, Word, RVPos)    -> step2b(Word, RVPos).

%% step 2b
-spec step2b(string(), rvpos()) -> string().
step2b("somaíra" ++ Tail, RVPos) when length(Tail) >= RVPos -> Tail;
step2b("somaíre" ++ Tail, RVPos) when length(Tail) >= RVPos -> Tail;
step2b("somaíri" ++ Tail, RVPos) when length(Tail) >= RVPos -> Tail;
step2b("somaréi" ++ Tail, RVPos) when length(Tail) >= RVPos -> Tail;
step2b("someséi" ++ Tail, RVPos) when length(Tail) >= RVPos -> Tail;
step2b("siaíra" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("somera" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("siaíre" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("somere" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("siaíri" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("someri" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("siarei" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("siesei" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("sietsa" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("sietsi" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("somabá" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("somará" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("somesá" ++ Tail, RVPos) when length(Tail) >= RVPos  -> Tail;
step2b("naíra" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("saíra" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("siéra" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("naíre" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("saíre" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("siére" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("naíri" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("saíri" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("siéri" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("narei" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("nesei" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("norei" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("odnei" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("sarei" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("sesei" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("siaba" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("siara" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("siesa" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("somaí" ++ Tail, RVPos) when length(Tail) >= RVPos   -> Tail;
step2b("some" ++ Tail, RVPos) when length(Tail) >= RVPos    -> step2b(Tail);
step2b("nára" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("sára" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("aíra" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("náre" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("sáre" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("aíre" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("nári" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("sári" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("aíri" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("arei" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("esei" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("etsa" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("etsi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("naba" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("nara" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("nesa" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("nora" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("odna" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("saba" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("sada" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("sadi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("sara" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("sesa" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("siaí" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("soda" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("sodi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("soma" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("somi" ++ Tail, RVPos) when length(Tail) >= RVPos    -> Tail;
step2b("sié" ++ Tail, RVPos) when length(Tail) >= RVPos     -> step2b(Tail);
step2b("ára" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("éra" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("áre" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("ére" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("ári" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("éri" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("aba" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("ada" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("adi" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("ara" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("esa" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("naí" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("oda" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("odi" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("saí" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("siá" ++ Tail, RVPos) when length(Tail) >= RVPos     -> Tail;
step2b("ne" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step2b(Tail);
step2b("se" ++ Tail, RVPos) when length(Tail) >= RVPos      -> step2b(Tail);
step2b("aí" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b("da" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b("de" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b("di" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b("na" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b("ói" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b("ra" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b("re" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b("ri" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b("sa" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b("sí" ++ Tail, RVPos) when length(Tail) >= RVPos      -> Tail;
step2b(Word, _)                                             -> Word.

-spec step2b(string()) -> string().
step2b("ug" ++ Tail) -> [$g | Tail];
step2b(Word)         -> Word.

%% step 3
-spec step3(string(), rvpos()) -> string().
step3("so" ++ Tail, RVPos) when length(Tail) >= RVPos -> Tail;
step3([$a | Tail], RVPos) when length(Tail) >= RVPos  -> Tail;
step3([$o | Tail], RVPos) when length(Tail) >= RVPos  -> Tail;
step3([$á | Tail], RVPos) when length(Tail) >= RVPos  -> Tail;
step3([$í | Tail], RVPos) when length(Tail) >= RVPos  -> Tail;
step3([$ó | Tail], RVPos) when length(Tail) >= RVPos  -> Tail;

step3("eu" ++ Tail, RVPos) when length(Tail) >= RVPos,
                                hd(Tail) =:= $g       -> Tail;
step3("éu" ++ Tail, RVPos) when length(Tail) >= RVPos,
                                hd(Tail) =:= $g       -> Tail;
step3([$e | Tail], RVPos) when length(Tail) >= RVPos  -> Tail;
step3([$é | Tail], RVPos) when length(Tail) >= RVPos  -> Tail;
step3(Word, _)                                        -> Word.

%% post processing
-spec post_process(string()) -> string().
post_process(Word) ->
    post_process(Word, []).

-spec post_process(string(), string()) -> string().
post_process([$á | Tail], Acc) ->
    post_process(Tail, [$a | Acc]);
post_process([$é | Tail], Acc) ->
    post_process(Tail, [$e | Acc]);
post_process([$í | Tail], Acc) ->
    post_process(Tail, [$i | Acc]);
post_process([$ó | Tail], Acc) ->
    post_process(Tail, [$o | Acc]);
post_process([$ú | Tail], Acc) ->
    post_process(Tail, [$u | Acc]);
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
