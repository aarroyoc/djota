:- module(djota, [djot/2, djot_ast/2, inline_text_ast/2]).

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

% Blockquote
djot_ast_([Line|Lines]) -->
    { phrase(blockquote_line(Text), Line) },
    djot_blockquote_ast_(Lines, Text).

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

djot_blockquote_ast_([Line|Lines], Block) -->
    { phrase(blockquote_line(Text), Line), append(Block, ['\n'|Text], Block1) },
    djot_blockquote_ast_(Lines, Block1).

djot_blockquote_ast_([Line|Lines], Block) -->
    { Line \= "", \+ phrase(blockquote_line(_), Line), append(Block, ['\n'|Line], Block1) },
    djot_blockquote_ast_(Lines, Block1).

djot_blockquote_ast_([""|Lines], Block) -->
    { djot_ast(Block, InsideAst) },
    [blockquote(InsideAst)],
    djot_ast_(Lines).

djot_blockquote_ast_([], Block) -->
    djot_blockquote_ast_([""], Block).

blockquote_line(Text) -->
    "> ", seq(Text).

blockquote_line("") -->
    ">".

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
    { phrase(inline_text_ast_(InlineAst), Paragraph) },
    [paragraph(InlineAst)],
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
ast_html_node_(paragraph(InlineAst)) -->
    "<p>",
    ast_html_(InlineAst),
    "</p>".
ast_html_node_(section(N, Header, Child)) -->
    { phrase(ast_html_(Child), ChildHtml) },
    format_("<section><h~d>~s</h~d>~s</section>", [N, Header, N, ChildHtml]).
ast_html_node_(blockquote(Child)) -->
    { phrase(ast_html_(Child), ChildHtml) },
    "<blockquote>", ChildHtml, "</blockquote>".
ast_html_node_(link(Name, Url)) -->
    format_("<a href=\"~s\">~s</a>", [Url, Name]).
ast_html_node_(image(AltText, Url)) -->
    format_("<img alt=\"~s\" src=\"~s\">", [AltText, Url]).
ast_html_node_(verbatim(Text)) -->
    "<code>", Text, "</code>".
ast_html_node_(emphasis(Child)) -->
    { phrase(ast_html_(Child), ChildHtml) },
    "<em>", ChildHtml, "</em>".
ast_html_node_(strong(Child)) -->
    { phrase(ast_html_(Child), ChildHtml) },
    "<strong>", ChildHtml, "</strong>".
ast_html_node_(highlight(Str)) -->
    "<mark>", Str, "</mark>".
ast_html_node_(superscript(Str)) -->
    "<sup>", Str, "</sup>".
ast_html_node_(subscript(Str)) -->
    "<sub>", Str, "</sub>".
ast_html_node_(insert(Str)) -->
    "<ins>", Str, "</ins>".
ast_html_node_(delete(Str)) -->
    "<del>", Str, "</del>".
ast_html_node_(str(Str)) -->
    Str.

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

% Inline syntax AST
inline_text_ast(Text, Ast) :-
    once(phrase(inline_text_ast_(Ast), Text)).

inline_text_ast_(Ast) -->
    insert_ast_(Ast).
inline_text_ast_(Ast) -->
    delete_ast_(Ast).
inline_text_ast_(Ast) -->
    superscript_ast_(Ast).
inline_text_ast_(Ast) -->
    subscript_ast_(Ast).
inline_text_ast_(Ast) -->
    highlight_ast_(Ast).
inline_text_ast_(Ast) -->
    strong_ast_(Ast).
inline_text_ast_(Ast) -->
    emphasis_ast_(Ast).
inline_text_ast_(Ast) -->
    verbatim_ast_(Ast).
inline_text_ast_(Ast) -->
    autolink_ast_(Ast).
inline_text_ast_(Ast) -->
    reference_image_ast_(Ast).
inline_text_ast_(Ast) -->
    inline_image_ast_(Ast).
inline_text_ast_(Ast) -->
    reference_link_ast_(Ast).
inline_text_ast_(Ast) -->
    inline_link_ast_(Ast).
inline_text_ast_(Ast) -->
    str_ast_(Ast).
inline_text_ast_([]) -->
    [].

insert_ast_([insert(Str)|Ast0]) -->
    "{+",
    seq(Str),
    "+}",
    inline_text_ast_(Ast0).

delete_ast_([delete(Str)|Ast0]) -->
    "{-",
    seq(Str),
    "-}",
    inline_text_ast_(Ast0).

superscript_ast_([superscript(Str)|Ast0]) -->
    ( "^" | "{^" ),
    seq(Str),
    ( "^" | "^}" ),
    inline_text_ast_(Ast0).

subscript_ast_([subscript(Str)|Ast0]) -->
    ( "~" | "{~" ),
    seq(Str),
    ( "~" | "~}" ),
    inline_text_ast_(Ast0).

highlight_ast_([highlight(Str)|Ast0]) -->
    "{=",
    seq(Str),
    "=}",
    inline_text_ast_(Ast0).

emphasis_ast_([emphasis(InlineAst)|Ast0]) -->
    ( ("_", look_ahead(T), { T \= ' ' }) | "{_" ),
    inline_text_ast_(InlineAst),
    {
	append(_, [End], InlineAst),
	(
	    End = str(StrEnd) ->
	    ( append(_, [EndChar], StrEnd), EndChar \= ' ' )
	;   true
	)
    },
    "_",
    inline_text_ast_(Ast0).

emphasis_ast_([emphasis(InlineAst)|Ast0]) -->
    ( ("_", look_ahead(T), { T \= ' ' }) | "{_" ),
    inline_text_ast_(InlineAst),
    "_}",
    inline_text_ast_(Ast0).

strong_ast_([strong(InlineAst)|Ast0]) -->
    ( ("*", look_ahead(T), { T \= ' ' }) | "{*" ),
    inline_text_ast_(InlineAst),
    {
	append(_, [End], InlineAst),
	(
	    End = str(StrEnd) ->
	    ( append(_, [EndChar], StrEnd), EndChar \= ' ' )
	;   true
	)
    },
    "*",
    inline_text_ast_(Ast0).

strong_ast_([strong(InlineAst)|Ast0]) -->
    ( ("*", look_ahead(T), { T \= ' ' }) | "{*" ),
    inline_text_ast_(InlineAst),
    "*}",
    inline_text_ast_(Ast0).

verbatim_ast_([verbatim(Str)|Ast0]) -->
    backticks(N, _),
    backticks_end(N, Str),
    inline_text_ast_(Ast0).

backticks_end(N, Str) -->
    backticks(M, Str0), { M \= N },
    backticks_end(N, Str1),
    {
	append(Str0, Str1, Str)
    }.
backticks_end(N, "") -->
    backticks(N, _).
backticks_end(N, [Char|Str0]) -->
    [Char], { Char \= "`" },
    backticks_end(N, Str0).

backticks(N, ['`'|Str]) -->
    "`",
    backticks(N0, Str),
    { N is N0 + 1 }.
backticks(1, "`") --> "`".

autolink_ast_([link(Url, Url)|Ast0]) -->
    "<",
    seq(Url),
    ">",
    inline_text_ast_(Ast0),
    {
	append("http://", _, Url);append("https://", _, Url)
    }.

reference_image_ast_([Node|Ast0]) -->
    "![",
    seq(AltText),
    "][",
    seq(RefName),
    "]",
    inline_text_ast_(Ast0),
    {
	( RefName = "" ->
	  Node = image_ref(AltText)
	; Node = image_ref(AltText, RefName)
	)
    }.

inline_image_ast_([image(AltText, Url)|Ast0]) -->
    "![",
    seq(AltText),
    "](",
    seq(Url),
    ")",
    inline_text_ast_(Ast0).

reference_link_ast_([Node|Ast0]) -->
    "[",
    seq(LinkText),
    "][",
    seq(RefName),
    "]",
    inline_text_ast_(Ast0),
    {
	( RefName = "" ->
	  Node = link_ref(LinkText)
	; Node = link_ref(LinkText, RefName)
	)
    }.

inline_link_ast_([link(LinkText, LinkUrl)|Ast0]) -->
    "[",
    seq(LinkText),
    "](",
    seq(LinkUrl),
    ")",
    inline_text_ast_(Ast0).

str_ast_(Ast) -->
    "\\",
    [Char],
    inline_text_ast_([PrevNode|Ast1]),
    {
	(
	    PrevNode = str(Str) ->
	    Ast = [str([Char|Str])|Ast1]
	;   Ast = [str([Char]),PrevNode|Ast1]
	)
    }.

str_ast_([str([Char])]) -->
    "\\",
    [Char],
    inline_text_ast_([]).

str_ast_(Ast) -->
    [Char],
    inline_text_ast_([PrevNode|Ast1]),
    {
	(
	    PrevNode = str(Str) ->
	    Ast = [str([Char|Str])|Ast1]
	;   Ast = [str([Char]),PrevNode|Ast1]
	)
    }.

str_ast_([str([Char])]) -->
    [Char],
    inline_text_ast_([]).


look_ahead(T), [T] --> [T].
