:- module(djota, [djot/2, inline_text//2]).

:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).

% Block syntax

djot(Djot, Html) :-
    once(phrase(lines(Lines), Djot)),
    phrase(djot_(Lines), Html).

%% djot_([Line|Lines], Html0, Html) :-
%%     djot_paragraph_([Line|Lines], Html0, Html).
%% djot_([Line|Lines], Html0, Html) :-
%%     Line = [],
%%     djot_(Lines, Html0, Html).
%% djot_([], Html, Html).

djot_([Line|Lines]) -->
    { Line \= "" },
    djot_thematic_break_([Line|Lines]).
djot_([Line|Lines]) -->
    { Line \= "" },
    djot_paragraph_([Line|Lines], "").
djot_([[]|Lines]) -->
    djot_(Lines).
djot_([]) --> [].

djot_paragraph_([Line|Lines], Paragraph0) -->
    {
	Line \= "",
	(
	    Paragraph0 = "" ->
	    Line = Paragraph1
	;   append(Paragraph0, [' '|Line], Paragraph1)
	)
    },
    djot_paragraph_(Lines, Paragraph1).

djot_paragraph_([""|Lines], Paragraph) -->
    { phrase(inline_text(Html, []), Paragraph) },
    "<p>",
    Html,
    "</p>",
    djot_(Lines).

djot_paragraph_([], Paragraph) -->
    djot_paragraph_([""], Paragraph).

djot_thematic_break_([Line|Lines]) -->
    { phrase(thematic_break_line(0), Line) },
    "<hr>",
    djot_(Lines).

thematic_break_line(N) -->
    (" "|"\t"),
    thematic_break_line(N).

thematic_break_line(N0) -->
    "*",
    { N is N0 + 1},
    thematic_break_line(N).

thematic_break_line(N) -->
    { N >= 3 }.

char(X) -->
    [X],
    {
	\+ member(X, "\n\r")
    }.
line_ending --> "\n" | "\r" | "\r\n".

line_chars([X|Xs]) -->
    char(X),
    line_chars(Xs).
line_chars([X]) --> char(X).

line([X|Xs]) -->
    char(X),
    line(Xs).

line([]) --> line_ending.

lines([X|Xs]) -->
    line(X),
    lines(Xs).

lines([X]) -->
    line_chars(X),
    file_ending.

file_ending --> [].
file_ending --> line_ending.
file_ending --> line_ending, file_ending.

% Inline syntax
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
