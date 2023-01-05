:- module(djota, [inline_text//2]).

:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).

inline_text(Text, Data) -->
    reference_image(Text, Data).
inline_text(Text, Data) -->
    inline_image(Text, Data).
inline_text(Text, Data) -->
    reference_link(Text, Data).
inline_text(Text, Data) -->
    inline_link(Text, Data).
inline_text(Text, Data) -->
    backslash(Text, Data).
inline_text(Text, Data) -->
    ordinary_text(Text, Data).
inline_text([], _) -->
    [].

reference_image(Text, Data) -->
    "![",
    inline_text(AltText, Data),
    "][",
    seq(RefName),
    "]",
    inline_text(Text1, Data),
    {
	( RefName \= "" ->
	  (
	      member(ref(RefName, LinkUrl), Data) ->
	      phrase(format_("<img alt=\"~s\" src=\"~s\">", [AltText, LinkUrl]), Text0)
	  ;   phrase(format_("<img alt=\"~s\">", [AltText]), Text0) 
	  )
	; (
	      member(ref(AltText, LinkUrl), Data) ->
	      phrase(format_("<img alt=\"~s\" src=\"~s\">", [AltText, LinkUrl]), Text0)
	  ;   phrase(format_("<img alt=\"~s\">", [AltText]), Text0)
	  )
	),
	append(Text0, Text1, Text)
    }.

inline_image(Text, Data) -->
    "![",
    inline_text(AltText, Data),
    "](",
    seq(Url),
    ")",
    inline_text(Text1, Data),
    {
	phrase(format_("<img alt=\"~s\" src=\"~s\">", [AltText, Url]), Text0),
	append(Text0, Text1, Text)
    }.

reference_link(Text, Data) -->
    "[",
    inline_text(LinkText, Data),
    "][",
    seq(RefName),
    "]",
    inline_text(Text1, Data),
    {
	( RefName \= "" ->
	  (
	      member(ref(RefName, LinkUrl), Data) ->
	      phrase(format_("<a href=\"~s\">~s</a>", [LinkUrl, LinkText]), Text0)
	  ;   phrase(format_("<a>~s</a>", [LinkText]), Text0) 
	  )
	; (
	      member(ref(LinkText, LinkUrl), Data) ->
	      phrase(format_("<a href=\"~s\">~s</a>", [LinkUrl, LinkText]), Text0)
	  ;   phrase(format_("<a>~s</a>", [LinkText]), Text0)
	  )
	),
	append(Text0, Text1, Text)
    }.

inline_link(Text, Data) -->
    "[",
    inline_text(LinkText, Data),
    "](",
    seq(LinkUrl),
    ")",
    inline_text(Text1, Data),
    {
	phrase(format_("<a href=\"~s\">~s</a>", [LinkUrl, LinkText]), Text0),
	append(Text0, Text1, Text)
    }.

backslash([Char|Text], Data) -->
    "\\",
    [Char],
    inline_text(Text, Data).

ordinary_text([Char|Text], Data) -->
    [Char],
    inline_text(Text, Data).
