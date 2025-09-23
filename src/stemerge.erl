%%-----------------------------------------------------------------------------
%% @author Roman Parykin <github@donderom.com>
%% @doc
%% The common interface for all stemmers.
%% @end
%%-----------------------------------------------------------------------------
-module(stemerge).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
-export([stem/2]).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

%% @doc     Stem of a word for a given language.
%% @param   Word a word to stem
%% @param   Lang word language
%% @returns the stem of a Word given a Lang
-spec stem(Word :: string(), Lang :: string()) -> string().
stem(Word, "en") ->
    stemerge_en:stem(Word);
stem(Word, "sv") ->
    stemerge_sv:stem(Word);
stem(Word, "no") ->
    stemerge_no:stem(Word);
stem(Word, "da") ->
    stemerge_da:stem(Word);
stem(Word, "fi") ->
    stemerge_fi:stem(Word);
stem(Word, "nl") ->
    stemerge_nl:stem(Word);
stem(Word, "de") ->
    stemerge_de:stem(Word);
stem(Word, "fr") ->
    stemerge_fr:stem(Word);
stem(Word, "es") ->
    stemerge_es:stem(Word);
stem(Word, "pt") ->
    stemerge_pt:stem(Word);
stem(Word, "it") ->
    stemerge_it:stem(Word);
stem(Word, _) ->
    Word.
