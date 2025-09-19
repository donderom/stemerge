# Stemerge

[![Hex.pm Version](https://img.shields.io/hexpm/v/stemerge?style=flat-square&color=39b8a5)](https://hex.pm/packages/stemerge)
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

### Erlang

Add `stemerge` as a dependency to the `rebar.config`:

```erlang
{deps, [
    {"stemerge", "0.3.4"}
]}.
```

Specify the language code of the word you'd like to stem, like this:

```erlang
stemerge:stem("stemerge", "en")
```

or call the `stem/1` API of any available stemmer separately:

```erlang
stemerge_no:stem("havneeier")
```

### Elixir

Add `stemerge` as a dependency to the `mix.exs`:

```elixir
defp deps do
  [
    {:stemerge, "0.3.4"}
  ]
end
```

Call the Erlang API from Elixir:

```elixir
:stemerge.stem(~c"stemming", ~c"en")
```

### Gleam

Add `stemerge` as a dependency to the `gleam.toml`:

```toml
[dependencies]
stemerge = "0.3.4"
```

Define an external function:

```gleam
import gleam/erlang/charlist.{type Charlist}

@external(erlang, "stemerge", "stem")
pub fn stem(s: Charlist, lang: Charlist) -> Charlist
```

Call the external function from Gleam:

```gleam
charlist.to_string(stem(
  charlist.from_string("stemming"),
  charlist.from_string("en"),
))
```
