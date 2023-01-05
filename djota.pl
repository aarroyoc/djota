:- module(djota, [djot/2, djot_ast/2, inline_text//2]).

:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).

% Block syntax

djot(Djot, Html) :-
    djot_ast(Djot, Ast),
    once(phrase(ast_html_(Ast), Html)).

djot_ast(Djot, Ast) :-
    once(phrase(lines(Lines), Djot)),
    once(phrase(djot_ast_(Lines), Ast)).    

% From Djot source to AST

% Thematic break
djot_ast_([Line|Lines]) -->
    { phrase(thematic_break_line(0), Line) },
    [thematic_break],
    djot_ast_(Lines).

% Heading
djot_ast_([Line|Lines]) -->
    { phrase(heading_line(N, Header), Line) },
    djot_heading_ast_(Lines, N, Header).

% Paragraph
djot_ast_([Line|Lines]) -->
    { Line \= "" },
    djot_paragraph_ast_([Line|Lines], "").

% Empty line
djot_ast_([[]|Lines]) -->
    djot_ast_(Lines).
% No more lines
djot_ast_([]) --> [].

djot_heading_ast_([Line|Lines], N, Header) -->
    { phrase(heading_line(N, Header), Line), append(Header, [' '|Line], Header1) },
    djot_heading_ast_(Lines, N, Header1).

djot_heading_ast_([[]|Lines], N, Header) -->
    {
	append(SectionLines, [HeadingLine|Rest], Lines),
	phrase(heading_line(NextN, _), HeadingLine),
	NextN =< N,
	phrase(djot_ast_(SectionLines), SectionAst)
    },
    [section(N, Header, SectionAst)],
    djot_ast_([HeadingLine|Rest]).

djot_heading_ast_([[]|Lines], N, Header) -->
    {
	phrase(djot_ast_(Lines), SectionAst)
    },
    [section(N, Header, SectionAst)].
    
djot_heading_ast_([], N, Header) -->
    djot_heading_ast([""], N, Header).

heading_line(1, Header) --> "# ", seq(Header).
heading_line(2, Header) --> "## ", seq(Header).
heading_line(3, Header) --> "### ", seq(Header).
heading_line(4, Header) --> "#### ", seq(Header).
heading_line(5, Header) --> "##### ", seq(Header).
heading_line(6, Header) --> "###### ", seq(Header).

section_lines([X|Xs], N) -->
    [X], { \+ phrase(heading_line(N, _), X) },
    section_lines(Xs, N).
    
section_lines([], N) -->
    heading_line(N, _).
section_lines([], _) --> [].

djot_paragraph_ast_([Line|Lines], Paragraph0) -->
    {
	Line \= "",
	(
	    Paragraph0 = "" ->
	    Line = Paragraph1
	;   append(Paragraph0, [' '|Line], Paragraph1)
	)
    },
    djot_paragraph_ast_(Lines, Paragraph1).

djot_paragraph_ast_([""|Lines], Paragraph) -->
    { phrase(inline_text(Html, []), Paragraph) },
    [paragraph(Html)],
    djot_ast_(Lines).

djot_paragraph_ast_([], Paragraph) -->
    djot_paragraph_ast_([""], Paragraph).

thematic_break_line(N) -->
    (" "|"\t"),
    thematic_break_line(N).

thematic_break_line(N0) -->
    "*",
    { N is N0 + 1},
    thematic_break_line(N).

thematic_break_line(N) -->
    { N >= 3 }.

% From AST to HTML

ast_html_([]) --> [].
ast_html_([X|Xs]) -->
    ast_html_node_(X),
    ast_html_(Xs).

ast_html_node_(thematic_break) -->
    "<hr>".
ast_html_node_(paragraph(Html)) -->
    "<p>",Html,"</p>".
ast_html_node_(section(N, Header, Child)) -->
    { phrase(ast_html_(Child), ChildHtml) },
    format_("<section><h~d>~s</h~d>~s</section>", [N, Header, N, ChildHtml]).

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
