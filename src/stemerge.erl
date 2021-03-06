%%-----------------------------------------------------------------------------
%% @author Roman Parykin <donderom@ymail.com>
%% @doc
%% The common interface to all the stemmers.
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

%% @doc
%% Returns the stem of a word for given language.
-spec stem(string(), string()) -> string().
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
