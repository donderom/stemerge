%%-----------------------------------------------------------------------------
%% @author Roman Parykin <donderom@ymail.com>
%% @doc
%% The implementation of the finnish stemming algorithm.
%% @reference
%% <a href="http://snowball.tartarus.org/algorithms/finnish/stemmer.html">
%% The Finnish stemming algorithm</a>
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge_fi).

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
         or (Char =:= $�)
         or (Char =:= $�))).

-define(is_a_V(Char),
        ((Char =:= $a)
         or (Char =:= $e)
         or (Char =:= $i)
         or (Char =:= $o)
         or (Char =:= $u)
         or (Char =:= $�)
         or (Char =:= $�))).

-define(is_a_Vi(Char1, Char2),
        ((?is_a_V(Char1))
         and (Char2 =:= $i))).

-define(is_a_long_vowel(Char1, Char2),
        ((Char1 =:= Char2)
         and ((Char1 =:= $a)
              or (Char1 =:= $e)
              or (Char1 =:= $i)
              or (Char1 =:= $o)
              or (Char1 =:= $u)
              or (Char1 =:= $�)
              or (Char1 =:= $�)))).

-define(is_a_cv(Char1, Char2),
        ((not ?is_a_vowel(Char1))
         and (?is_a_vowel(Char2)))).

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------
-type r1pos() :: non_neg_integer().
-type r2pos() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

%% @doc
%% Returns the stem of a word if the word has more than 1 letter.
-spec stem(string()) -> string().
stem(Word) when length(Word) > 1 ->
    {R1Pos, R2Pos} = r_pos(Word),
    ReversedWord = lists:reverse(Word),
    stem(ReversedWord, R1Pos, R2Pos);
stem(ShortWord) ->
    ShortWord.

-spec stem(string(), r1pos(), r2pos()) -> string().
stem(ReversedWord, R1Pos, R2Pos) ->
    Step1 = step1(ReversedWord, R1Pos, R2Pos),
    Step2 = step2(Step1, R1Pos),
    {IsEndingRemoved, Step3} = step3(Step2, R1Pos),
    Step4 = step4(Step3, R2Pos),
    Step5 = step5(Step4, IsEndingRemoved, R1Pos, R2Pos),
    Step6 = step6(Step5, R1Pos),
    lists:reverse(Step6).

%%-----------------------------------------------------------------------------
%% Steps
%%-----------------------------------------------------------------------------

%% step 1 condition
-spec step1_check(string(), string()) -> string().
step1_check([$n | Tail], _)                          -> [$n | Tail];
step1_check([$t | Tail], _)                          -> [$t | Tail];
step1_check([Char | Tail], _) when ?is_a_vowel(Char) -> [Char | Tail];
step1_check(_, Word)                                 -> Word.

%% step 1
-spec step1(string(), r1pos(), r2pos()) -> string().
step1("nik" ++ Tail = Word, R1Pos, _) when length(Tail) >= R1Pos  -> step1_check(Tail, Word);
step1("naak" ++ Tail = Word, R1Pos, _) when length(Tail) >= R1Pos -> step1_check(Tail, Word);
step1("n��k" ++ Tail = Word, R1Pos, _) when length(Tail) >= R1Pos -> step1_check(Tail, Word);
step1("ok" ++ Tail = Word, R1Pos, _) when length(Tail) >= R1Pos   -> step1_check(Tail, Word);
step1("�k" ++ Tail = Word, R1Pos, _) when length(Tail) >= R1Pos   -> step1_check(Tail, Word);
step1("nah" ++ Tail = Word, R1Pos, _) when length(Tail) >= R1Pos  -> step1_check(Tail, Word);
step1("n�h" ++ Tail = Word, R1Pos, _) when length(Tail) >= R1Pos  -> step1_check(Tail, Word);
step1("ap" ++ Tail = Word, R1Pos, _) when length(Tail) >= R1Pos   -> step1_check(Tail, Word);
step1("�p" ++ Tail = Word, R1Pos, _) when length(Tail) >= R1Pos   -> step1_check(Tail, Word);
step1("its" ++ Tail, _, R2Pos) when length(Tail) >= R2Pos         -> Tail;
step1(Word, _, _)                                                 -> Word.

%% step 2
-spec step2(string(), r1pos()) -> string().
step2("is" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                                hd(Tail) =/= $k          -> Tail;
step2("inesk" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "isk" ++ Tail;
step2("in" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> Tail;
step2("asn" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step2("�sn" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step2("emm" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step2("enn" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> Tail;
step2("naat" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> "at" ++ Tail;
step2("naass" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "ass" ++ Tail;
step2("naats" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "ats" ++ Tail;
step2("naall" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "all" ++ Tail;
step2("naatl" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "atl" ++ Tail;
step2("naan" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> "an" ++ Tail;
step2("n��t" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> "�t" ++ Tail;
step2("n��ss" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "�ss" ++ Tail;
step2("n��ts" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "�ts" ++ Tail;
step2("n��ll" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "�ll" ++ Tail;
step2("n��tl" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "�tl" ++ Tail;
step2("n��n" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> "�n" ++ Tail;
step2("neell" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "ell" ++ Tail;
step2("neeni" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> "eni" ++ Tail;
step2(Word, _)                                           -> Word.

%% step 3
-spec step3(string(), r1pos()) -> {boolean(), string()}.
step3([$n, Char, $h, Char2 | Tail] = Word, R1Pos) when length(Tail) + 1 >= R1Pos,
                                                       (Char =/= $u) ->
    case ((Char =:= Char2) and (?is_a_V(Char))) of
        true ->
            {true, [Char | Tail]};
        false ->
            {false, Word}
    end;
step3("niis" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                                  ?is_a_Vi(hd(tl(Tail)), hd(Tail)) ->
    {true, Tail};
step3("ned" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                                 ?is_a_Vi(hd(tl(Tail)), hd(Tail)) ->
    {true, Tail};
step3("nett" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                                  ?is_a_Vi(hd(tl(Tail)), hd(Tail)) ->
    {true, Tail};
step3("nees" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                                  ?is_a_long_vowel(hd(tl(Tail)), hd(Tail)) ->
    {true, Tail};
step3("atte" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> {true, [$e | Tail]};
step3("�tte" ++ Tail, R1Pos) when length(Tail) >= R1Pos -> {true, [$e | Tail]};
step3("ats" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("atl" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("at" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> {true, Tail};
step3("�ts" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("�tl" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("�t" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> {true, Tail};
step3("ass" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("�ss" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("all" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("�ll" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("ell" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("an" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> {true, Tail};
step3("�n" ++ Tail, R1Pos) when length(Tail) >= R1Pos   -> {true, Tail};
step3("isk" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("eni" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, Tail};
step3("n" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                               ?is_a_long_vowel(hd(tl(Tail)), hd(Tail)) ->
    {true, tl(Tail)};
step3("nei" ++ Tail, R1Pos) when length(Tail) >= R1Pos  -> {true, "i" ++ Tail};
step3("n" ++ Tail, R1Pos) when length(Tail) >= R1Pos    -> {true, Tail};
step3("a" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                               ?is_a_cv(hd(tl(Tail)), hd(Tail)) ->
    {true, Tail};
step3("�" ++ Tail, R1Pos) when length(Tail) >= R1Pos,
                               ?is_a_cv(hd(tl(Tail)), hd(Tail)) ->
    {true, Tail};
step3(Word, _) ->
    {false, Word}.

%% step 4
-spec step4(string(), r2pos()) -> string().
step4("ipmi" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step4("apmi" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step4("�mpi" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step4("immi" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step4("ammi" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step4("�mmi" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
step4("aje" ++ Tail, R2Pos) when length(Tail) >= R2Pos  -> Tail;
step4("�je" ++ Tail, R2Pos) when length(Tail) >= R2Pos  -> Tail;
step4("ipmop" ++ _ = Word, _)                           -> Word;
step4("ipm" ++ Tail, R2Pos) when length(Tail) >= R2Pos  -> Tail;
step4("apmop" ++ _ = Word, _)                           -> Word;
step4("apm" ++ Tail, R2Pos) when length(Tail) >= R2Pos  -> Tail;
step4("�pmop" ++ _ = Word, _)                           -> Word;
step4("�pm" ++ Tail, R2Pos) when length(Tail) >= R2Pos  -> Tail;
step4("immop" ++ _ = Word, _)                           -> Word;
step4("imm" ++ Tail, R2Pos) when length(Tail) >= R2Pos  -> Tail;
step4("ammop" ++ _ = Word, _)                           -> Word;
step4("amm" ++ Tail, R2Pos) when length(Tail) >= R2Pos  -> Tail;
step4("�mmop" ++ _ = Word, _)                           -> Word;
step4("�mm" ++ Tail, R2Pos) when length(Tail) >= R2Pos  -> Tail;
step4(Word, _)                                          -> Word.

%% step 5
-spec step5(string(), boolean(), r1pos(), r2pos()) -> string().
step5("i" ++ Tail, true, R1Pos, _) when length(Tail) >= R1Pos      -> Tail;
step5("j" ++ Tail, true, R1Pos, _) when length(Tail) >= R1Pos      -> Tail;
step5("t" ++ Tail, false, R1Pos, R2Pos) when length(Tail) >= R1Pos,
                                             ?is_a_vowel(hd(Tail)) ->
    after_step5(Tail, R2Pos);
step5(Word, _, _, _)                                               -> Word.

%% if `t` in R1 is removed in step 5
-spec after_step5(string(), r2pos()) -> string().
after_step5("ammi" ++ Tail, R2Pos) when length(Tail) >= R2Pos -> Tail;
after_step5("ammop" ++ _ = Word, _)                           -> Word;
after_step5("amm" ++ Tail, R2Pos) when length(Tail) >= R2Pos  -> Tail;
after_step5(Word, _)                                          -> Word.

%% step 6
-spec step6(string(), r1pos()) -> string().
step6(Word, R1Pos) ->
    Step6a = step6a(Word, R1Pos),
    Step6b = step6b(Step6a, R1Pos),
    Step6c = step6c(Step6b, R1Pos),
    Step6d = step6d(Step6c, R1Pos),
    Step6e = step6e(Step6d),
    Step6e.

%% step6a
-spec step6a(string(), r1pos()) -> string().
step6a([Char1, Char2 | Tail] = Word, R1Pos) when length(Word) - R1Pos > 1,
                                                 ?is_a_long_vowel(Char2, Char1) ->
    [Char2 | Tail];
step6a(Word, _) ->
    Word.

%% step6b
-spec step6b(string(), r1pos()) -> string().
step6b([$a, Char2 | Tail] = Word, R1Pos) when length(Word) - R1Pos > 1,
                                              not ?is_a_vowel(Char2) ->
    [Char2 | Tail];
step6b([$�, Char2 | Tail] = Word, R1Pos) when length(Word) - R1Pos > 1,
                                              not ?is_a_vowel(Char2) ->
    [Char2 | Tail];
step6b([$e, Char2 | Tail] = Word, R1Pos) when length(Word) - R1Pos > 1,
                                              not ?is_a_vowel(Char2) ->
    [Char2 | Tail];
step6b([$i, Char2 | Tail] = Word, R1Pos) when length(Word) - R1Pos > 1,
                                              not ?is_a_vowel(Char2) ->
    [Char2 | Tail];
step6b(Word, _) ->
    Word.

%% step6c
-spec step6c(string(), r1pos()) -> string().
step6c("jo" ++ Tail = Word, R1Pos) when length(Word) - R1Pos > 1 ->
    [$o | Tail];
step6c("ju" ++ Tail = Word, R1Pos) when length(Word) - R1Pos > 1 ->
    [$u | Tail];
step6c(Word, _) ->
    Word.

%% step6d
-spec step6d(string(), r1pos()) -> string().
step6d("oj" ++ Tail = Word, R1Pos) when length(Word) - R1Pos > 1 ->
    [$j | Tail];
step6d(Word, _) ->
    Word.

%% step6e
-spec step6e(string()) -> string().
step6e([Char, Char | Tail]) when not ?is_a_vowel(Char) ->
    [Char | Tail];
step6e([Char1, Char2, Char2 | Tail]) when ?is_a_vowel(Char1),
                                          not ?is_a_vowel(Char2) ->
    [Char1, Char2 | Tail];
step6e([Char1, Char2, Char3, Char3 | Tail]) when ?is_a_vowel(Char1),
                                                 ?is_a_vowel(Char2),
                                                 not ?is_a_vowel(Char3) ->
    [Char1, Char2, Char3 | Tail];
step6e([Char1, Char2, Char3, Char4, Char4 | Tail]) when ?is_a_vowel(Char1),
                                                        ?is_a_vowel(Char2),
                                                        ?is_a_vowel(Char3),
                                                        not ?is_a_vowel(Char4) ->
    [Char1, Char2, Char3, Char4 | Tail];
step6e(Word) ->
    Word.

%%-----------------------------------------------------------------------------
%% Definitions
%%-----------------------------------------------------------------------------
-spec r_pos(string()) -> {r1pos(), r2pos()}.
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
