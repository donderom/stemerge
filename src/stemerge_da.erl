%%-----------------------------------------------------------------------------
%% @author Roman Parykin <donderom@ymail.com>
%% @doc
%% The implementation of the danish stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/danish/stemmer.html">
%% The Danish stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_da).

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
         or (Char =:= $y)
         or (Char =:= $æ)
         or (Char =:= $å)
         or (Char =:= $ø))).

-define(is_a_valid_s_ending(Char),
        ((Char =:= $a)
         or (Char =:= $b)
         or (Char =:= $c)
         or (Char =:= $d)
         or (Char =:= $f)
         or (Char =:= $g)
         or (Char =:= $h)
         or (Char =:= $j)
         or (Char =:= $k)
         or (Char =:= $l)
         or (Char =:= $m)
         or (Char =:= $n)
         or (Char =:= $o)
         or (Char =:= $p)
         or (Char =:= $r)
         or (Char =:= $t)
         or (Char =:= $v)
         or (Char =:= $y)
         or (Char =:= $z)
         or (Char =:= $å))).

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------
-type r1pos() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

%% @doc
%% Returns the stem of a word.
-spec stem(string()) -> string().
stem(Word) when length(Word) > 2 ->
    R1Pos = r_pos(Word),
    ReversedWord = lists:reverse(Word),
    stem(ReversedWord, R1Pos);
stem(ShortWord) ->
    ShortWord.

-spec stem(string(), r1pos()) -> string().
stem(ReversedWord, R1Pos) ->
    Step1 = step1(ReversedWord, R1Pos),
    Step2 = step2(Step1, R1Pos),
    Step3 = step3(pre_step3(Step2), R1Pos),
    Step4 = step4(Step3, R1Pos),
    lists:reverse(Step4).

%%-----------------------------------------------------------------------------
%% Steps
%%-----------------------------------------------------------------------------

%% step 1
-spec step1(string(), r1pos()) -> string().
step1("sednere" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("ednere" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("snedeh" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("dehte" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("edere" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("nedeh" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("redeh" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("sedne" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("senre" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("snere" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("stere" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("dere" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("edne" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("enre" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("nere" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("rere" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("sdeh" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("sene" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("sere" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("tere" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("deh" ++ Tail, R1Pos) when length(Tail) >= R1Pos        -> Tail;
step1("ene" ++ Tail, R1Pos) when length(Tail) >= R1Pos        -> Tail;
step1("ere" ++ Tail, R1Pos) when length(Tail) >= R1Pos        -> Tail;
step1("sne" ++ Tail, R1Pos) when length(Tail) >= R1Pos        -> Tail;
step1("sre" ++ Tail, R1Pos) when length(Tail) >= R1Pos        -> Tail;
step1("ste" ++ Tail, R1Pos) when length(Tail) >= R1Pos        -> Tail;
step1("ne" ++ Tail, R1Pos) when length(Tail) >= R1Pos         -> Tail;
step1("re" ++ Tail, R1Pos) when length(Tail) >= R1Pos         -> Tail;
step1("se" ++ Tail, R1Pos) when length(Tail) >= R1Pos         -> Tail;
step1("te" ++ Tail, R1Pos) when length(Tail) >= R1Pos         -> Tail;
step1("e" ++ Tail, R1Pos) when length(Tail) >= R1Pos          -> Tail;
step1("s" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                               ?is_a_valid_s_ending(hd(Tail)) -> Tail;
step1(Word, _)                                                -> Word.

%% step 2
-spec step2(string(), r1pos()) -> string().
step2("dg" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$g | Tail];
step2("td" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$d | Tail];
step2("tg" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$g | Tail];
step2("tk" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$k | Tail];
step2(Word, _)                                        -> Word.

%% pre-step 3
-spec pre_step3(string()) -> string().
pre_step3("tsgi" ++ Tail) -> "gi" ++ Tail;
pre_step3(Word) -> Word.

%% step 3
-spec step3(string(), r1pos()) -> string().
step3("gile" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> step2(Tail, R1Pos);
step3("gil" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> step2(Tail, R1Pos);
step3("gi" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> step2(Tail, R1Pos);
step3("sle" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> step2(Tail, R1Pos);
step3("tsøl" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "søl" ++ Tail;
step3(Word, _)                                          -> Word.

%% step 4
-spec step4(string(), r1pos()) -> string().
step4([Char, Char | Tail], R1Pos) when length(Tail) + 1 >= R1Pos,
                                       not ?is_a_vowel(Char)  ->
    [Char | Tail];
step4(Word, _) ->
    Word.

%%-----------------------------------------------------------------------------
%% Definitions
%%-----------------------------------------------------------------------------
-spec r_pos(string()) -> r1pos().
r_pos(Word) ->
    {_, R1Pos} = r_pos(Word, 0),
    case R1Pos =< 3 of
        true  ->
            3;
        false ->
            R1Pos
    end.

-spec r_pos(string(), non_neg_integer()) -> {string(), non_neg_integer()}.
r_pos([Char1, Char2 | Tail], StartPos) when ?is_a_vowel(Char1),
                                            not ?is_a_vowel(Char2) ->
    {Tail, StartPos + 2};
r_pos([_ | Tail], StartPos) ->
    r_pos(Tail, StartPos + 1);
r_pos([], StartPos) ->
    {[], StartPos}.
