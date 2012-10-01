# Stemerge

Stemerge is a collection of [stemmers](http://en.wikipedia.org/wiki/Stemming) in Erlang.

## Supported languages

At the moment the following languages are supported:
* English (Porter2 stemming algorithm)
* Swedish
* Norwegian
* Danish
* Finnish

## Usage

Just specify the language code of a word you'd like to stem like this:

```erlang
stemerge:stem("stemerge", "en")
```

or call ```stem/1``` API of any available stemmer separately:

```erlang
stemerge_no:stem("havneeier")
```
