%%-----------------------------------------------------------------------------
%% @author Roman Parykin <github@donderom.com>
%% @doc
%% The implementation of the German stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/german/stemmer.html">
%% The German stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_de).

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
         orelse (Char =:= $ö)
         orelse (Char =:= $ü))).

-define(is_a_valid_s_ending(Char),
        ((Char =:= $b)
         orelse (Char =:= $d)
         orelse (Char =:= $f)
         orelse (Char =:= $g)
         orelse (Char =:= $h)
         orelse (Char =:= $k)
         orelse (Char =:= $l)
         orelse (Char =:= $m)
         orelse (Char =:= $n)
         orelse (Char =:= $r)
         orelse (Char =:= $t))).

-define(is_a_valid_st_ending(Char),
        ((Char =/= $r)
         andalso ?is_a_valid_s_ending(Char))).

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------
-type r1pos() :: non_neg_integer().
-type r2pos() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

%% @doc     Stem of a German word form.
%% @param   Word a German word form
%% @returns stem of a Word in German
-spec stem(Word :: string()) -> string().
stem(Word) ->
    {Word1, R1Pos, R2Pos} = bootstrap(Word),
    ReversedWord = lists:reverse(Word1),
    stem(ReversedWord, R1Pos, R2Pos).

-spec stem(string(), r1pos(), r2pos()) -> string().
stem(ReversedWord, R1Pos, R2Pos) ->
    Step1 = step1(ReversedWord, R1Pos),
    Step2 = step2(Step1, R1Pos),
    Step3 = step3(Step2, R1Pos, R2Pos),
    post_process(Step3).

%%-----------------------------------------------------------------------------
%% Steps
%%-----------------------------------------------------------------------------

%% step 1
-spec step1(string(), r1pos()) -> string().
step1("me" ++ Tail, R1Pos) when length(Tail) >= R1Pos           -> Tail;
step1("nre" ++ Tail, R1Pos) when length(Tail) >= R1Pos          -> Tail;
step1("re" ++ Tail, R1Pos) when length(Tail) >= R1Pos           -> Tail;
step1("e" ++ Tail, R1Pos) when length(Tail) >= R1Pos            -> after_step1(Tail);
step1("ne" ++ Tail, R1Pos) when length(Tail) >= R1Pos           -> after_step1(Tail);
step1("se" ++ Tail, R1Pos) when length(Tail) >= R1Pos           -> after_step1(Tail);
step1([$s, Char | Tail], R1Pos) when length(Tail) + 1 >= R1Pos,
                                     ?is_a_valid_s_ending(Char) -> [Char | Tail];
step1(Word, _)                                                  -> Word.

-spec after_step1(string()) -> string().
after_step1("ssin" ++ Tail) -> "sin" ++ Tail;
after_step1(Word)           -> Word.

%% step 2
-spec step2(string(), r1pos()) -> string().
step2("ne" ++ Tail, R1Pos) when length(Tail) >= R1Pos           -> Tail;
step2("re" ++ Tail, R1Pos) when length(Tail) >= R1Pos           -> Tail;
step2("tse" ++ Tail, R1Pos) when length(Tail) >= R1Pos          -> Tail;
step2("ts" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                                length(Tail) > 3,
                                ?is_a_valid_st_ending(hd(Tail)) -> Tail;
step2(Word, _)                                                  -> Word.

%% step 3
-spec step3(string(), r1pos(), r2pos()) -> string().
step3("dnegi" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos,
                                      hd(Tail) =/= $e          -> Tail;
step3("gnugi" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos,
                                      hd(Tail) =/= $e          -> Tail;
step3("dne" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos      -> Tail;
step3("gnu" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos      -> Tail;
step3("gi" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos,
                                   hd(Tail) =/= $e             -> Tail;
step3("ki" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos,
                                   hd(Tail) =/= $e             -> Tail;
step3("hcsi" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos,
                                     hd(Tail) =/= $e           -> Tail;
step3("hcilre" ++ Tail, R1Pos, _) when length(Tail) >= R1Pos   -> Tail;
step3("hcilne" ++ Tail, R1Pos, _) when length(Tail) >= R1Pos   -> Tail;
step3("tiehre" ++ Tail, R1Pos, _) when length(Tail) >= R1Pos   -> Tail;
step3("tiehne" ++ Tail, R1Pos, _) when length(Tail) >= R1Pos   -> Tail;
step3("hcil" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos     -> Tail;
step3("tieh" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos     -> Tail;
step3("tiekhcil" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos -> Tail;
step3("tiekgi" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos   -> Tail;
step3("tiek" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos     -> Tail;
step3(Word, _, _)                                              -> Word.

%% post processing
-spec post_process(string()) -> string().
post_process(Word) ->
    post_process(Word, []).

-spec post_process(string(), string()) -> string().
post_process([$Y | Tail], Acc) ->
    post_process(Tail, [$y | Acc]);
post_process([$U | Tail], Acc) ->
    post_process(Tail, [$u | Acc]);
post_process([$ä | Tail], Acc) ->
    post_process(Tail, [$a | Acc]);
post_process([$ö | Tail], Acc) ->
    post_process(Tail, [$o | Acc]);
post_process([$ü | Tail], Acc) ->
    post_process(Tail, [$u | Acc]);
post_process([Char | Tail], Acc) ->
    post_process(Tail, [Char | Acc]);
post_process([], Acc) ->
    Acc.

%%-----------------------------------------------------------------------------
%% Definitions
%%-----------------------------------------------------------------------------
-spec r_pos(string()) -> {r1pos(), r2pos()}.
r_pos(Word) ->
    {Word1, R1Pos} = r_pos(Word, 0),
    {_, R2Pos} = r_pos(Word1, R1Pos),
    case R1Pos =< 3 of
        true  ->
            {3, R2Pos};
        false ->
            {R1Pos, R2Pos}
    end.

-spec r_pos(string(), non_neg_integer()) -> {string(), non_neg_integer()}.
r_pos([Char1, Char2 | Tail], StartPos) when ?is_a_vowel(Char1),
                                            not ?is_a_vowel(Char2) ->
    {Tail, StartPos + 2};
r_pos([_ | Tail], StartPos) ->
    r_pos(Tail, StartPos + 1);
r_pos([], StartPos) ->
    {[], StartPos}.

-spec bootstrap(string()) -> {string(), r1pos(), r2pos()}.
bootstrap(Word) ->
    Word1 = mark_vowels(replace_ss(Word)),
    {R1Pos, R2Pos} = r_pos(Word1),
    {Word1, R1Pos, R2Pos}.

-spec replace_ss(string()) -> string().
replace_ss("\x{00DF}" ++ Tail) ->
    [$s, $s | replace_ss(Tail)];
replace_ss([Char | Tail]) ->
    [Char | replace_ss(Tail)];
replace_ss([]) ->
    [].

-spec mark_vowels(string()) -> string().
mark_vowels(Word) ->
    mark_vowels_rec(mark_initial_vowel(Word)).

-spec mark_initial_vowel(string()) -> string().
mark_initial_vowel([$y | Tail]) ->
    [$Y | mark_vowels_rec(Tail)];
mark_initial_vowel(Word) ->
    Word.

-spec mark_vowels_rec(string()) -> string().
mark_vowels_rec([Char1, $y, Char2 | Tail]) when ?is_a_vowel(Char1),
                                                ?is_a_vowel(Char2) ->
    [Char1, $Y | mark_vowels_rec([Char2 | Tail])];
mark_vowels_rec([Char1, $u, Char2 | Tail]) when ?is_a_vowel(Char1),
                                                ?is_a_vowel(Char2) ->
    [Char1, $U | mark_vowels_rec([Char2 | Tail])];
mark_vowels_rec([Char | Tail]) ->
    [Char | mark_vowels_rec(Tail)];
mark_vowels_rec([]) ->
    [].
