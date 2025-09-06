%%-----------------------------------------------------------------------------
%% @author Roman Parykin <github@donderom.com>
%% @doc
%% The implementation of the norwegian stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/norwegian/stemmer.html">
%% The Norwegian stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_no).

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
         orelse (Char =:= $æ)
         orelse (Char =:= $å)
         orelse (Char =:= $ø))).

-define(is_a_valid_s_ending(PrecededChar, Char),
        ((Char =:= $b)
         orelse (Char =:= $c)
         orelse (Char =:= $d)
         orelse (Char =:= $f)
         orelse (Char =:= $g)
         orelse (Char =:= $h)
         orelse (Char =:= $j)
         orelse (Char =:= $l)
         orelse (Char =:= $m)
         orelse (Char =:= $n)
         orelse (Char =:= $o)
         orelse (Char =:= $p)
         orelse (Char =:= $r)
         orelse (Char =:= $t)
         orelse (Char =:= $v)
         orelse (Char =:= $y)
         orelse (Char =:= $z)
         orelse ((Char =:= $k) andalso (not ?is_a_vowel(PrecededChar))))).

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
step1("seneteh" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> Tail;
step1("eneteh" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> Tail;
step1("sneteh" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> Tail;
step1("neteh" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("reteh" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("sedne" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step1("edna" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("edne" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("sede" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("sene" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step1("etre" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> "re" ++ Tail;
step1("tre" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> "re" ++ Tail;
step1("ede" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("ena" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("ene" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("sne" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("sre" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("ste" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("teh" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("tsa" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step1("ne" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("ra" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("re" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("sa" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("se" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("te" ++ Tail, R1Pos) when length(Tail) >= R1Pos      -> Tail;
step1("a" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("e" ++ Tail, R1Pos) when length(Tail) >= R1Pos       -> Tail;
step1("s" ++ Tail, R1Pos)
  when length(Tail) >= R1Pos,
       ?is_a_valid_s_ending(hd(tl(Tail)), hd(Tail))        -> Tail;
step1(Word, _)                                             -> Word.

%% step 2
-spec step2(string(), r1pos()) -> string().
step2("td" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$d | Tail];
step2("tv" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> [$v | Tail];
step2(Word, _)                                        -> Word.

%% step 3
-spec step3(string(), r1pos()) -> string().
step3("volsteh" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> Tail;
step3("vols" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step3("vole" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step3("vol" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step3("sle" ++ Tail, R1Pos) when length(Tail) >= R1Pos     ->  Tail;
step3("gele" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step3("gel" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step3("gile" ++ Tail, R1Pos) when length(Tail) >= R1Pos    ->  Tail;
step3("gil" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step3("gie" ++ Tail, R1Pos) when length(Tail) >= R1Pos     -> Tail;
step3("gi" ++ Tail, R1Pos) when length(Tail) >= R1Pos      ->  Tail;
step3(Word, _)                                             -> Word.

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
