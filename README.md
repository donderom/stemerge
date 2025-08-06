# Stemerge

[![Build](https://img.shields.io/github/actions/workflow/status/donderom/stemerge/main.yml?style=flat-square&logo=github)](https://github.com/donderom/stemerge/actions/workflows/main.yml)
[![License](https://img.shields.io/github/license/donderom/stemerge?style=flat-square)](https://github.com/donderom/stemerge/blob/main/LICENSE)
[![Erlang Versions](https://img.shields.io/badge/Erlang-24%20to%2028-b83998?logo=erlang&style=flat-square)](https://www.erlang.org)

Stemerge is a collection of [stemmers](http://en.wikipedia.org/wiki/Stemming) in Erlang.

## Supported languages

At the moment, the following languages are supported:

* English (Porter2 stemming algorithm)
* Finnish

Romance:

* French
* Italian
* Portuguese
* Spanish

Germanic:

* Dutch
* German

Scandinavian:

* Danish
* Norwegian
* Swedish

## Usage

Just specify the language code of the word you'd like to stem, like this:

```erlang
stemerge:stem("stemerge", "en")
```

or call the ```stem/1``` API of any available stemmer separately:

```erlang
stemerge_no:stem("havneeier")
```
