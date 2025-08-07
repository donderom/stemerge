%%-----------------------------------------------------------------------------
%% @author Roman Parykin <donderom@ymail.com>
%% @doc
%% The implementation of the dutch stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/dutch/stemmer.html">
%% The Dutch stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_nl).

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
         orelse (Char =:= $è))).

-define(is_a_valid_s_ending(Char),
        ((Char =/= $j)
         andalso (not ?is_a_vowel(Char)))).

-define(is_a_double(Char),
        ((Char =:= $k)
         orelse (Char =:= $d)
         orelse (Char =:= $t))).

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------
-type r1pos() :: non_neg_integer().
-type r2pos() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

%% @doc
%% Returns the stem of a word.
-spec stem(string()) -> string().
stem(Word) ->
    {Word1, R1Pos, R2Pos} = bootstrap(Word),
    ReversedWord = lists:reverse(Word1),
    stem(ReversedWord, R1Pos, R2Pos).

-spec stem(string(), r1pos(), r2pos()) -> string().
stem(ReversedWord, R1Pos, R2Pos) ->
    Step1 = step1(ReversedWord, R1Pos),
    {IsEndingRemoved, Step2} = step2(Step1, R1Pos),
    Step3 = step3(Step2, R1Pos, R2Pos),
    Step3b = step3b(Step3, IsEndingRemoved, R1Pos, R2Pos),
    Step4 = step4(Step3b),
    post_process(Step4).

%%-----------------------------------------------------------------------------
%% Steps
%%-----------------------------------------------------------------------------

%% step 1
-spec step1(string(), r1pos()) -> string().
step1("nedeh" ++ Tail = Word, R1Pos) ->
    case length(Tail) >= R1Pos of
        true ->
            "dieh" ++ Tail;
        false ->
            Word
    end;
step1("enemeg" ++ _ = Word, _) ->
    Word;
step1("nemeg" ++ _ = Word, _) ->
    Word;
step1("ene" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                                 not ?is_a_vowel(hd(Tail)) ->
    Tail;
step1("ne" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                                not ?is_a_vowel(hd(Tail)) ->
    undouble(Tail);
step1("es" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                                ?is_a_valid_s_ending(hd(Tail)) ->
    Tail;
step1("s" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                               ?is_a_valid_s_ending(hd(Tail)) ->
    Tail;
step1(Word, _) ->
    Word.

%% step 2
-spec step2(string(), r1pos()) -> {boolean(), string()}.
step2([$e, Char | Tail], R1Pos) when length(Tail) + 1 >= R1Pos,
                                     not ?is_a_vowel(Char) ->
    {true, undouble([Char | Tail])};
step2(Word, _) ->
    {false, Word}.

%% step 3
-spec step3(string(), r1pos(), r2pos()) -> string().
step3("dieh" ++ Tail, R1Pos, R2Pos) when length(Tail) >= R2Pos,
                                         hd(Tail) =/= $c ->
    after_step3(Tail, R1Pos);
step3(Word, _, _) ->
    Word.

%% after step 3
-spec after_step3(string(), r1pos()) -> string().
after_step3("ne" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                                      not ?is_a_vowel(hd(Tail)) ->
    undouble(Tail);
after_step3(Word, _) ->
    Word.

%% step 3b
-spec step3b(string(), boolean(), r1pos(), r2pos()) -> string().
step3b("dnegi" ++ Tail, _, _, R2Pos) when length(Tail) >= R2Pos,
                                          hd(Tail) =/= $e ->
    Tail;
step3b("gnigi" ++ Tail, _, _, R2Pos) when length(Tail) >= R2Pos,
                                          hd(Tail) =/= $e ->
    Tail;
step3b("dne" ++ Tail, _, _, R2Pos) when length(Tail) >= R2Pos ->
    undouble(Tail);
step3b("gni" ++ Tail, _, _, R2Pos) when length(Tail) >= R2Pos ->
    undouble(Tail);

step3b("gi" ++ Tail, _, _, R2Pos) when length(Tail) >= R2Pos,
                                       hd(Tail) =/= $e ->
    Tail;
step3b("kjil" ++ Tail, _, R1Pos, R2Pos) when length(Tail) >= R2Pos ->
    {_, Step2} = step2(Tail, R1Pos),
    Step2;
step3b("raab" ++ Tail, _, _, R2Pos) when length(Tail) >= R2Pos ->
    Tail;
step3b("rab" ++ Tail, true,  _, R2Pos) when length(Tail) >= R2Pos ->
    Tail;
step3b(Word, _, _, _) ->
    Word.

%% step 4
-spec step4(string()) -> string().
step4([D, V, V, C | Tail]) when D =/= $I,
                                not ?is_a_vowel(D),
                                ((V =:= $a)
                                 orelse (V =:= $e)
                                 orelse (V =:= $o)
                                 orelse (V =:= $u)),
                                not ?is_a_vowel(C) ->
    [D, V, C | Tail];
step4(Word) ->
    Word.

%% post processing
-spec post_process(string()) -> string().
post_process(Word) ->
    post_process(Word, []).

-spec post_process(string(), string()) -> string().
post_process([$Y | Tail], Acc) ->
    post_process(Tail, [$y | Acc]);
post_process([$I | Tail], Acc) ->
    post_process(Tail, [$i | Acc]);
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
    Word1 = mark_vowels(remove_accents(Word)),
    {R1Pos, R2Pos} = r_pos(Word1),
    {Word1, R1Pos, R2Pos}.

-spec remove_accents(string()) -> string().
remove_accents([Char | Tail]) when ((Char =:= $ä) or (Char =:= $á)) ->
    [$a | remove_accents(Tail)];
remove_accents([Char | Tail]) when ((Char =:= $ë) or (Char =:= $é)) ->
    [$e | remove_accents(Tail)];
remove_accents([Char | Tail]) when ((Char =:= $ï) or (Char =:= $í)) ->
    [$i | remove_accents(Tail)];
remove_accents([Char | Tail]) when ((Char =:= $ö) or (Char =:= $ó)) ->
    [$o | remove_accents(Tail)];
remove_accents([Char | Tail]) when ((Char =:= $ü) or (Char =:= $ú)) ->
    [$u | remove_accents(Tail)];
remove_accents([Char | Tail]) ->
    [Char | remove_accents(Tail)];
remove_accents([]) ->
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
mark_vowels_rec([Char, $y | Tail]) when ?is_a_vowel(Char) ->
    [Char, $Y | mark_vowels_rec(Tail)];
mark_vowels_rec([Char1, $i, Char2 | Tail]) when ?is_a_vowel(Char1),
                                                ?is_a_vowel(Char2) ->
    [Char1, $I | mark_vowels_rec([Char2 | Tail])];
mark_vowels_rec([Char | Tail]) ->
    [Char | mark_vowels_rec(Tail)];
mark_vowels_rec([]) ->
    [].

-spec undouble(string()) -> string().
undouble([Char, Char | Tail]) when ?is_a_double(Char) ->
    [Char | Tail];
undouble(Word) ->
    Word.
