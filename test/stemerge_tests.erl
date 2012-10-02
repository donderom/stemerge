-module(stemerge_tests).

-include_lib("eunit/include/eunit.hrl").

%%-----------------------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------------------

%% english
stemerge_en_test() ->
    {Words, Stems} = vocs("en"),
    ?assertEqual(stem_all(fun stemerge_en:stem/1, Words, Stems), []).

%% swedish
stemerge_sv_test() ->
    {Words, Stems} = vocs("sv"),
    ?assertEqual(stem_all(fun stemerge_sv:stem/1, Words, Stems), []).

%% norwegian
stemerge_no_test() ->
    {Words, Stems} = vocs("no"),
    ?assertEqual(stem_all(fun stemerge_no:stem/1, Words, Stems), []).

%% danish
stemerge_da_test() ->
    {Words, Stems} = vocs("da"),
    ?assertEqual(stem_all(fun stemerge_da:stem/1, Words, Stems), []).

%% finnish
stemerge_fi_test() ->
    {Words, Stems} = vocs("fi"),
    ?assertEqual(stem_all(fun stemerge_fi:stem/1, Words, Stems), []).

%% dutch
stemerge_nl_test() ->
    {Words, Stems} = vocs("nl"),
    ?assertEqual(stem_all(fun stemerge_nl:stem/1, Words, Stems), []).

%%-----------------------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------------------

-spec stem_all(fun(), [string()], [string()]) -> [{string(), string(), string()}].
stem_all(StemFunc, [OrigWord | OrigTail], [Stem | StemTail]) ->
    case StemFunc(OrigWord) of
        Stem ->
            stem_all(StemFunc, OrigTail, StemTail);
        UnmatchedStem ->
            FailReport = {OrigWord, Stem, UnmatchedStem},
            [FailReport | stem_all(StemFunc, OrigTail, StemTail)]
    end;
stem_all(_, [], []) ->
    [].

-spec vocs(string()) -> {[string()], [string()]}.
vocs(Lang) ->
    {voc_to_list(voc(Lang, "voc.txt")), voc_to_list(voc(Lang, "output.txt"))}.

-spec voc_to_list(string()) -> [string()].
voc_to_list(VocFileName) ->
    VocFile = filename:join([filename:absname(".."), "test", VocFileName]),
    {ok, Voc} = file:read_file(VocFile),
    string:tokens(binary_to_list(Voc), "\n").

-spec voc(string(), string()) -> string().
voc(Lang, FileName) ->
    filename:join(["voc", Lang, FileName]).
