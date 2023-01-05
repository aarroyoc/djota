:- module(djota, [inline_text//1]).

:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).

inline_text(Text) -->
    backslash(Text).
inline_text(Text) -->
    ordinary_text(Text).
inline_text([]) -->
    [].

backslash([Char|Text]) -->
    "\\",
    [Char],
    inline_text(Text).

ordinary_text([Char|Text]) -->
    [Char],
    inline_text(Text).
