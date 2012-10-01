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
stem(Word, _) ->
    Word.
