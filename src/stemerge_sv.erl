%%-----------------------------------------------------------------------------
%% @author Roman Parykin <donderom@ymail.com>
%% @doc
%% The implementation of the swedish stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/swedish/stemmer.html">
%% The Swedish stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_sv).

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
         orelse (Char =:= $ä)
         orelse (Char =:= $å)
         orelse (Char =:= $ö))).

-define(is_a_valid_s_ending(Char),
        ((Char =:= $b)
         orelse (Char =:= $c)
         orelse (Char =:= $d)
         orelse (Char =:= $f)
         orelse (Char =:= $g)
         orelse (Char =:= $h)
         orelse (Char =:= $j)
         orelse (Char =:= $k)
         orelse (Char =:= $l)
         orelse (Char =:= $m)
         orelse (Char =:= $n)
         orelse (Char =:= $o)
         orelse (Char =:= $p)
         orelse (Char =:= $r)
         orelse (Char =:= $t)
         orelse (Char =:= $v)
         orelse (Char =:= $y))).

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
    Step3 = step3(Step2, R1Pos),
    lists:reverse(Step3).

%%-----------------------------------------------------------------------------
%% Steps
%%-----------------------------------------------------------------------------

%% step 1
-spec step1(string(), r1pos()) -> string().
step1("anreteh" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> Tail;
step1("sneteh" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> Tail;
step1("nedna" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("neteh" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("reteh" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("sanra" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("sanre" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("sanro" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("sedna" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("snera" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("tedna" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("anra" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("anre" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("anro" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("edna" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("enra" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("etsa" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("nera" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("seda" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("snre" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("eda" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("era" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("nre" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("sne" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("teh" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("tsa" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("da" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("ne" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("ra" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("re" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("ro" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("sa" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("se" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("ta" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("a" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("e" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("s" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                               ?is_a_valid_s_ending(hd(Tail))-> Tail;
step1(Word, _)                                             -> Word.

%% step 2
-spec step2(string(), r1pos()) -> string().
step2("dd" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$d | Tail];
step2("dg" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$g | Tail];
step2("nn" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$n | Tail];
step2("td" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$d | Tail];
step2("tg" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$g | Tail];
step2("tk" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$k | Tail];
step2("tt" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$t | Tail];
step2(Word, _)                                        -> Word.

%% step 3
-spec step3(string(), r1pos()) -> string().
step3("gil" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step3("gi" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step3("sle" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step3("tsöl" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> "söl" ++ Tail;
step3("tlluf" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "lluf" ++ Tail;
step3(Word, _)                                           -> Word.

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
