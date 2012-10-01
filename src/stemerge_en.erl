%%-----------------------------------------------------------------------------
%% @author Roman Parykin <donderom@ymail.com>
%% @doc
%% The implementation of the english (Porter2) stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/english/stemmer.html">
%% The English (Porter2) stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_en).

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
         or (Char =:= $y))).

-define(is_a_double(Char1, Char2),
        ((Char1 =:= Char2)
         and ((Char1 =:= $b)
              or (Char1 =:= $d)
              or (Char1 =:= $f)
              or (Char1 =:= $g)
              or (Char1 =:= $m)
              or (Char1 =:= $n)
              or (Char1 =:= $p)
              or (Char1 =:= $r)
              or (Char1 =:= $t)))).

-define(is_a_valid_li_ending(Char),
        ((Char =:= $c)
         or (Char =:= $d)
         or (Char =:= $e)
         or (Char =:= $g)
         or (Char =:= $h)
         or (Char =:= $k)
         or (Char =:= $m)
         or (Char =:= $n)
         or (Char =:= $r)
         or (Char =:= $t))).

-define(is_invariant_after_step1a(Word),
        ((Word =:= "gninni")
         or (Word =:= "gnituo")
         or (Word =:= "gninnac")
         or (Word =:= "gnirreh")
         or (Word =:= "gnirrae")
         or (Word =:= "deecorp")
         or (Word =:= "deecxe")
         or (Word =:= "deeccus"))).

-define(is_in_r(Word, RPos, OnSuccess, OnFail),
        case length(Word) >= RPos of
            true  ->
                OnSuccess;
            false ->
                OnFail
        end).

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------
-type r1pos() :: non_neg_integer().
-type r2pos() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

%% @doc
%% Returns the stem of a word if the word has more than 2 letters.
-spec stem(string()) -> string().

%% special changes
stem("skis")   -> "ski";
stem("skies")  -> "sky";
stem("dying")  -> "die";
stem("lying")  -> "lie";
stem("tying")  -> "tie";
%% special -ly cases
stem("idly")   -> "idl";
stem("gently") -> "gentl";
stem("ugly")   -> "ugli";
stem("early")  -> "earli";
stem("only")   -> "onli";
stem("singly") -> "singl";
%% invariant form
stem("sky")    -> "sky";
stem("news")   -> "news";
stem("howe")   -> "howe";
%% not plural forms
stem("atlas")  -> "atlas";
stem("cosmos") -> "cosmos";
stem("bias")   -> "bias";
stem("andes")  -> "andes";

stem(Word) when length(Word) > 2 ->
    {Word1, R1Pos, R2Pos} = bootstrap(Word),
    ReversedWord = lists:reverse(Word1),
    stem(ReversedWord, R1Pos, R2Pos);
stem(ShortWord) ->
    ShortWord.

-spec stem(string(), r1pos(), r2pos()) -> string().
stem(ReversedWord, R1Pos, R2Pos) ->
    Step0 = step0(ReversedWord),
    Step1a = step1a(Step0),
    case ?is_invariant_after_step1a(Step1a) of
        true ->
            post_process(Step1a);
        false ->
            Step1b = step1b(Step1a, R1Pos),
            Step1c = step1c(Step1b),
            Step2 = step2(Step1c, R1Pos),
            Step3 = step3(Step2, R1Pos, R2Pos),
            Step4 = step4(Step3, R2Pos),
            Step5 = step5(Step4, R1Pos, R2Pos),
            Stem = post_process(Step5),
            Stem
    end.

%%-----------------------------------------------------------------------------
%% Definitions
%%-----------------------------------------------------------------------------

%% bootstrapping
-spec bootstrap(string()) -> {string(), r1pos(), r2pos()}.
bootstrap(Word) ->
    Word1 = mark_vowels(rm_apostrophe(Word)),
    {R1Pos, R2Pos} = r_pos(Word1),
    {Word1, R1Pos, R2Pos}.

-spec rm_apostrophe(string()) -> string().
rm_apostrophe([$' | Word]) ->
    Word;
rm_apostrophe(Word)        ->
    Word.

-spec mark_vowels(string()) -> string().
mark_vowels(Word) ->
    mark_vowels_rec(mark_initial_vowel(Word)).

-spec mark_initial_vowel(string()) -> string().
mark_initial_vowel([$y | Tail]) ->
    [$Y | mark_vowels_rec(Tail)];
mark_initial_vowel(Word) ->
    Word.

-spec mark_vowels_rec(string()) -> string().
mark_vowels_rec([Char, $y | Tail]) when ?is_a_vowel(Char) ->
    [Char, $Y | mark_vowels_rec(Tail)];
mark_vowels_rec([Char | Tail]) ->
    [Char | mark_vowels_rec(Tail)];
mark_vowels_rec([]) ->
    [].

%% if the words begins gener, commun or arsen, set R1 to be the remainder of the word
-spec r_pos(string()) -> {r1pos(), r2pos()}.
r_pos("gener" ++ Tail) ->
    R1Pos = 5,
    {_, R2Pos} = r_pos(Tail, R1Pos),
    {R1Pos, R2Pos};
r_pos("commun" ++ Tail) ->
    R1Pos = 6,
    {_, R2Pos} = r_pos(Tail, R1Pos),
    {R1Pos, R2Pos};
r_pos("arsen" ++ Tail) ->
    R1Pos = 5,
    {_, R2Pos} = r_pos(Tail, R1Pos),
    {R1Pos, R2Pos};
r_pos(Word) ->
    {Word1, R1Pos} = r_pos(Word, 0),
    {_, R2Pos} = r_pos(Word1, R1Pos),
    {R1Pos, R2Pos}.

-spec r_pos(string(), non_neg_integer()) -> {string(), non_neg_integer()}.
r_pos([Char1, Char2 | Tail], StartPos) when ?is_a_vowel(Char1),
                                            not ?is_a_vowel(Char2) ->
    {Tail, StartPos + 2};
r_pos([_ | Tail], StartPos) ->
    r_pos(Tail, StartPos + 1);
r_pos([], StartPos) ->
    {[], StartPos}.

%%-----------------------------------------------------------------------------
%% Steps
%%-----------------------------------------------------------------------------

%% step 0
-spec step0(string()) -> string().
step0("'s'" ++ Tail) -> Tail;
step0("s'" ++ Tail)  -> Tail;
step0("'" ++ Tail)   -> Tail;
step0(Word)          -> Word.

%% step 1a
-spec step1a(string()) -> string().
step1a("sess" ++ Tail)                      -> "ss" ++ Tail;
step1a("dei" ++ Tail) when length(Tail) > 1 -> "i" ++ Tail;
step1a("dei" ++ Tail)                       -> "ei" ++ Tail;
step1a("sei" ++ Tail) when length(Tail) > 1 -> "i" ++ Tail;
step1a("sei" ++ Tail)                       -> "ei" ++ Tail;
step1a("ss" ++ _ = Word)                    -> Word;
step1a("su" ++ _ = Word)                    -> Word;
step1a([$s, Char | Tail] = Word)   ->
    case contains_a_vowel(Tail) of
        true  ->
            [Char | Tail];
        false ->
            Word
    end;
step1a(Word) -> Word.

%% step 1b
-spec step1b(string(), r1pos()) -> string().
step1b("yldee" ++ Tail = Word, R1Pos) -> ?is_in_r(Tail, R1Pos, "ee" ++ Tail, Word);
step1b("dee" ++ Tail = Word, R1Pos)   -> ?is_in_r(Tail, R1Pos, "ee" ++ Tail, Word);
step1b("ylde" ++ Tail = Word, R1Pos)  -> after_step1b(Tail, R1Pos, Word);
step1b("de" ++ Tail = Word, R1Pos)    -> after_step1b(Tail, R1Pos, Word);
step1b("ylgni" ++ Tail = Word, R1Pos) -> after_step1b(Tail, R1Pos, Word);
step1b("gni" ++ Tail = Word, R1Pos)   -> after_step1b(Tail, R1Pos, Word);
step1b(Word, _)                       -> Word.

-spec after_step1b(string(), r1pos(), string()) -> string().
after_step1b(Tail, R1Pos, Word) ->
    case contains_a_vowel(Tail) of
        true  ->
            after_step1b(Tail, R1Pos);
        false ->
            Word
    end.

-spec after_step1b(string(), r1pos()) -> string().
after_step1b("ta" ++ _ = Word, _)                                  -> [$e | Word];
after_step1b("lb" ++ _ = Word, _)                                  -> [$e | Word];
after_step1b("zi" ++ _ = Word, _)                                  -> [$e | Word];
after_step1b([Char, Char | Tail], _) when ?is_a_double(Char, Char) -> [Char | Tail];
after_step1b(Word, R1Pos)                                          ->
    case is_a_short_word(Word, R1Pos) of
        true  ->
            [$e | Word];
        false ->
            Word
    end.

%% step 1c
-spec step1c(string()) -> string().
step1c([$y, _] = Word) ->
    Word;
step1c([$y, Char, Char2 | Tail]) when not ?is_a_vowel(Char) ->
    [$i, Char, Char2 | Tail];
step1c([$Y, Char, Char2 | Tail]) when not ?is_a_vowel(Char) ->
    [$i, Char, Char2 | Tail];
step1c(Word) ->
    Word.

%% step 2
-spec step2(string(), r1pos()) -> string().
step2("lanoita" ++ Tail = Word, R1Pos) -> ?is_in_r(Tail, R1Pos, "eta" ++ Tail, Word);
step2("lanoit" ++ Tail = Word, R1Pos)  -> ?is_in_r(Tail, R1Pos, "noit" ++ Tail, Word);
step2("icne" ++ Tail = Word, R1Pos)    -> ?is_in_r(Tail, R1Pos, "ecne" ++ Tail, Word);
step2("icna" ++ Tail = Word, R1Pos)    -> ?is_in_r(Tail, R1Pos, "ecna" ++ Tail, Word);
step2("ilba" ++ Tail = Word, R1Pos)    -> ?is_in_r(Tail, R1Pos, "elba" ++ Tail, Word);
step2("iltne" ++ Tail = Word, R1Pos)   -> ?is_in_r(Tail, R1Pos, "tne" ++ Tail, Word);
step2("rezi" ++ Tail = Word, R1Pos)    -> ?is_in_r(Tail, R1Pos, "ezi" ++ Tail, Word);
step2("noitazi" ++ Tail = Word, R1Pos) -> ?is_in_r(Tail, R1Pos, "ezi" ++ Tail, Word);
step2("noita" ++ Tail = Word, R1Pos)   -> ?is_in_r(Tail, R1Pos, "eta" ++ Tail, Word);
step2("rota" ++ Tail = Word, R1Pos)    -> ?is_in_r(Tail, R1Pos, "eta" ++ Tail, Word);
step2("msila" ++ Tail = Word, R1Pos)   -> ?is_in_r(Tail, R1Pos, "la" ++ Tail, Word);
step2("itila" ++ Tail = Word, R1Pos)   -> ?is_in_r(Tail, R1Pos, "la" ++ Tail, Word);
step2("illa" ++ Tail = Word, R1Pos)    -> ?is_in_r(Tail, R1Pos, "la" ++ Tail, Word);
step2("ssenluf" ++ Tail = Word, R1Pos) -> ?is_in_r(Tail, R1Pos, "luf" ++ Tail, Word);
step2("ilsuo" ++ Tail = Word, R1Pos)   -> ?is_in_r(Tail, R1Pos, "suo" ++ Tail, Word);
step2("ssensuo" ++ Tail = Word, R1Pos) -> ?is_in_r(Tail, R1Pos, "suo" ++ Tail, Word);
step2("ssenevi" ++ Tail = Word, R1Pos) -> ?is_in_r(Tail, R1Pos, "evi" ++ Tail, Word);
step2("itivi" ++ Tail = Word, R1Pos)   -> ?is_in_r(Tail, R1Pos, "evi" ++ Tail, Word);
step2("itilib" ++ Tail = Word, R1Pos)  -> ?is_in_r(Tail, R1Pos, "elb" ++ Tail, Word);
step2("ilb" ++ Tail = Word, R1Pos)     -> ?is_in_r(Tail, R1Pos, "elb" ++ Tail, Word);
step2("igol" ++ Tail = Word, R1Pos)    -> ?is_in_r([$l | Tail], R1Pos, "gol" ++ Tail, Word);
step2("illuf" ++ Tail = Word, R1Pos)   -> ?is_in_r(Tail, R1Pos, "luf" ++ Tail, Word);
step2("ilssel" ++ Tail = Word, R1Pos)  -> ?is_in_r(Tail, R1Pos, "ssel" ++ Tail, Word);
step2("il" ++ Tail = Word, R1Pos)      -> ?is_in_r(Tail, R1Pos,
                                                   case ?is_a_valid_li_ending(hd(Tail)) of
                                                       true  ->
                                                           Tail;
                                                       false ->
                                                           Word
                                                   end,
                                                   Word);
step2(Word, _)                         -> Word.

%% step 3
-spec step3(string(), r1pos(), r2pos()) -> string().
step3("lanoita" ++ Tail = Word, R1Pos, _) -> ?is_in_r(Tail, R1Pos, "eta" ++ Tail, Word);
step3("lanoit" ++ Tail = Word, R1Pos, _)  -> ?is_in_r(Tail, R1Pos, "noit" ++ Tail, Word);
step3("ezila" ++ Tail = Word, R1Pos, _)   -> ?is_in_r(Tail, R1Pos, "la" ++ Tail, Word);
step3("etaci" ++ Tail = Word, R1Pos, _)   -> ?is_in_r(Tail, R1Pos, "ci" ++ Tail, Word);
step3("itici" ++ Tail = Word, R1Pos, _)   -> ?is_in_r(Tail, R1Pos, "ci" ++ Tail, Word);
step3("laci" ++ Tail = Word, R1Pos, _)    -> ?is_in_r(Tail, R1Pos, "ci" ++ Tail, Word);
step3("luf" ++ Tail = Word, R1Pos, _)     -> ?is_in_r(Tail, R1Pos, Tail, Word);
step3("ssen" ++ Tail = Word, R1Pos, _)    -> ?is_in_r(Tail, R1Pos, Tail, Word);
step3("evita" ++ Tail = Word, _, R2Pos)   -> ?is_in_r(Tail, R2Pos, Tail, Word);
step3(Word, _, _)                         -> Word.

%% step 4
-spec step4(string(), r2pos()) -> string().
step4("tneme" ++ Tail = Word, R2Pos) -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("tnem" ++ Tail = Word, R2Pos)  -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("tne" ++ Tail = Word, R2Pos)   -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("la" ++ Tail = Word, R2Pos)    -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("ecna" ++ Tail = Word, R2Pos)  -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("ecne" ++ Tail = Word, R2Pos)  -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("re" ++ Tail = Word, R2Pos)    -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("ci" ++ Tail = Word, R2Pos)    -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("elba" ++ Tail = Word, R2Pos)  -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("elbi" ++ Tail = Word, R2Pos)  -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("tna" ++ Tail = Word, R2Pos)   -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("msi" ++ Tail = Word, R2Pos)   -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("eta" ++ Tail = Word, R2Pos)   -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("iti" ++ Tail = Word, R2Pos)   -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("suo" ++ Tail = Word, R2Pos)   -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("evi" ++ Tail = Word, R2Pos)   -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("ezi" ++ Tail = Word, R2Pos)   -> ?is_in_r(Tail, R2Pos, Tail, Word);
step4("nois" ++ Tail = Word, R2Pos)  -> ?is_in_r([$s | Tail], R2Pos, [$s | Tail], Word);
step4("noit" ++ Tail = Word, R2Pos)  -> ?is_in_r([$t | Tail], R2Pos, [$t | Tail], Word);
step4(Word, _)                       -> Word.

%% step 5
-spec step5(string(), r1pos(), r2pos()) -> string().
step5([$e | Tail], _, R2Pos) when length(Tail) >= R2Pos ->
    Tail;
step5([$e | Tail] = Word, R1Pos, _) when length(Tail) >= R1Pos ->
    case ends_in_a_short_syllable(Tail) of
        true  ->
            Word;
        false ->
            Tail
    end;
step5("ll" ++ Tail, _, R2Pos) when length(Tail) + 1 >= R2Pos ->
    [$l | Tail];
step5(Word, _, _) ->
    Word.

%% post processing
-spec post_process(string()) -> string().
post_process(Word) ->
    post_process(Word, []).

-spec post_process(string(), string()) -> string().
post_process([$Y | Tail], Acc) ->
    post_process(Tail, [$y | Acc]);
post_process([Char | Tail], Acc) ->
    post_process(Tail, [Char | Acc]);
post_process([], Acc) ->
    Acc.

%%-----------------------------------------------------------------------------
%% Definitions
%%-----------------------------------------------------------------------------

-spec contains_a_vowel(string()) -> boolean().
contains_a_vowel([Char | _]) when ?is_a_vowel(Char) ->
    true;
contains_a_vowel([_ | Tail]) ->
    contains_a_vowel(Tail);
contains_a_vowel([]) ->
    false.

-spec ends_in_a_short_syllable(string()) -> boolean().
ends_in_a_short_syllable([Char1, Char2])
  when not ?is_a_vowel(Char1),
       ?is_a_vowel(Char2) ->
    true;
ends_in_a_short_syllable([Char1, Char2, Char3 | _])
  when not ?is_a_vowel(Char1),
       ((Char1 =/= $w)
        and (Char1 =/= $x)
        and (Char1 =/= $Y)),
       ?is_a_vowel(Char2),
       not ?is_a_vowel(Char3) ->
    true;
ends_in_a_short_syllable(_) ->
    false.

-spec is_a_short_word(string(), r1pos()) -> boolean().
is_a_short_word(Word, R1Pos) ->
    ((ends_in_a_short_syllable(Word)) and (length(Word) =:= R1Pos)).
