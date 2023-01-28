:- module(djota, [djot/2, djot_ast/2, inline_text_ast/2]).

:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(dif)).

% Block syntax

djot(Djot, Html) :-
    djot_ast(Djot, Ast),
    once(phrase(ast_html_(Ast), Html)).

djot_ast(Djot, Ast) :-
    once(phrase(lines(Lines), Djot)),
    once(phrase(djot_ast_(Lines, []), Ast)).    

% From Djot source to AST

% Thematic break
djot_ast_([Line|Lines], Attrs) -->
    { phrase(thematic_break_line(0), Line) },
    [thematic_break(Attrs)],
    djot_ast_(Lines, []).

% Heading
djot_ast_([Line|Lines], Attrs) -->
    { phrase(heading_line(N, Header), Line) },
    djot_heading_ast_(Lines, N, Header, Attrs).

% Blockquote
djot_ast_([Line|Lines], Attrs) -->
    { phrase(blockquote_line(Text), Line) },
    djot_blockquote_ast_(Lines, Text, Attrs).

% List
djot_ast_([Line|Lines], Attrs) -->
    { phrase(list_line(Type, Text), Line) },
    djot_list_ast_(Type, Lines, Text, [], continue, Attrs).

% Code block
djot_ast_([Line|Lines], Attrs) -->
    { phrase(((backticks(N, _), " ", seq(Spec)) | backticks(N, _), ... ), Line), N >= 3 },
    djot_code_ast_(Lines, N, "", Spec, Attrs).

% Div block
djot_ast_([Line|Lines], Attrs) -->
    { phrase(((colons(N), " ", seq(ClassName)) | colons(N), ... ), Line), N >= 3 },
    { append(["class"-ClassName], Attrs, Attrs1) },
    djot_div_ast_(Lines, N, "", Attrs1).

% Pipe table
djot_ast_([Line|Lines], Attrs) -->
    { phrase(pipe_table(Row), Line) },
    djot_table_ast_(Lines, [row(Row)], Attrs).

% Block attribute
djot_ast_([Line|Lines], Attrs) -->
    { phrase(inline_attr_ast_(Attrs1), Line), append(Attrs, Attrs1, Attrs2) },
    djot_ast_(Lines, Attrs2).
    

% Paragraph
djot_ast_([Line|Lines], Attrs) -->
    { Line \= "" },
    djot_paragraph_ast_([Line|Lines], "", Attrs).

% Empty line
djot_ast_([[]|Lines], Attrs) -->
    djot_ast_(Lines, Attrs).
% No more lines
djot_ast_([], _) --> [].

% Code block
djot_code_ast_([Line|Lines], N, Code0, Spec, Attrs) -->
    { \+ ( phrase(backticks(M, _), Line), M >= N), append(Code0, ['\n'|Line], Code) },
    djot_code_ast_(Lines, N, Code, Spec, Attrs).

djot_code_ast_([Line|Lines], N, Code0, Spec, Attrs) -->
    { phrase(backticks(M, _), Line), M >= N },
    [code(Spec, Code0, Attrs)],
    djot_ast_(Lines, []).

djot_code_ast_([], _, Code0, Spec, Attrs) -->
    [code(Spec, Code0, Attrs)].

% List: Line of list type same as current list
djot_list_ast_(type(Level, Type, Mode), [Line|Lines], CurrentItem, Items, _, Attrs) -->
    {
	phrase(list_line(type(Level, Type, _), Text), Line),
	djot_ast(CurrentItem, ItemAst),
	append(Items, [item(ItemAst)], NewItems)
    },
    djot_list_ast_(type(Level, Type, Mode), Lines, Text, NewItems, continue, Attrs).
% List: Line non indented in continue mode
djot_list_ast_(Type, [Line|Lines], CurrentItem, Items, continue, Attrs) -->
    {
	Line \= "",
	\+ phrase(list_line(_, _), Line),
	append(CurrentItem, ['\n'|Line], CurrentItem1)
    },
    djot_list_ast_(Type, Lines, CurrentItem1, Items, continue, Attrs).
% List: Empty line
djot_list_ast_(type(Level, Type, _), [Line|Lines], CurrentItem, Items, _, Attrs) -->
    {
	phrase(whites(_), Line),
	append(CurrentItem, ['\n'|Line], CurrentItem1)
    },
    djot_list_ast_(type(Level, Type, loose), Lines, CurrentItem1, Items, jump, Attrs).

% List: Line indented in jump mode
djot_list_ast_(type(Level, Type, Mode), [Line|Lines], CurrentItem, Items, jump, Attrs) -->
    {
	phrase((whites(W), seq(Text)), Line),
	W > Level,
	append(CurrentItem, ['\n'|Text], CurrentItem1)
    },
    djot_list_ast_(type(Level, Type, Mode), Lines, CurrentItem1, Items, jump, Attrs).

% List: Line not indented in jump mode
djot_list_ast_(type(Level, Type, Mode), [Line|Lines], CurrentItem, Items, jump, Attrs) -->
    {
	phrase((whites(W), seq(_)), Line),
	W =< Level,
	djot_ast(CurrentItem, ItemAst),
	append(Items, [item(ItemAst)], NewItems)	
    },
    [list(type(Level, Type, Mode), NewItems, Attrs)],
    djot_ast_([Line|Lines], []).

% List: No more lines
djot_list_ast_(Type, [], CurrentItem, Items, _, Attrs) -->
    {
	djot_ast(CurrentItem, ItemAst),
        append(Items, [item(ItemAst)], NewItems)
    },
    [list(Type, NewItems, Attrs)].


list_type(bullet("-")) --> "-".
list_type(bullet("*")) --> "*".
list_type(bullet("+")) --> "+".
list_line(type(Level, Type, tight), Text) -->
    whites(Level), list_type(Type), " ", seq(Text).

whites(0) --> "".
whites(N) -->
    " ",
    whites(N0),
    { N is N0 + 1}.

djot_heading_ast_([Line|Lines], N, Header, Attrs) -->
    { phrase(heading_line(N, Header), Line), append(Header, [' '|Line], Header1) },
    djot_heading_ast_(Lines, N, Header1, Attrs).

djot_heading_ast_([[]|Lines], N, Header, Attrs) -->
    {
	append(SectionLines, [HeadingLine|Rest], Lines),
	phrase(heading_line(NextN, _), HeadingLine),
	NextN =< N,
	phrase(djot_ast_(SectionLines, []), SectionAst)
    },
    [section(N, Header, SectionAst, Attrs)],
    djot_ast_([HeadingLine|Rest], []).

djot_heading_ast_([[]|Lines], N, Header, Attrs) -->
    {
	phrase(djot_ast_(Lines, []), SectionAst)
    },
    [section(N, Header, SectionAst, Attrs)].
    
djot_heading_ast_([], N, Header, Attrs) -->
    djot_heading_ast_([""], N, Header, Attrs).

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

djot_blockquote_ast_([Line|Lines], Block, Attrs) -->
    { phrase(blockquote_line(Text), Line), append(Block, ['\n'|Text], Block1) },
    djot_blockquote_ast_(Lines, Block1, Attrs).

djot_blockquote_ast_([Line|Lines], Block, Attrs) -->
    { Line \= "", \+ phrase(blockquote_line(_), Line), append(Block, ['\n'|Line], Block1) },
    djot_blockquote_ast_(Lines, Block1, Attrs).

djot_blockquote_ast_([""|Lines], Block, Attrs) -->
    { djot_ast(Block, InsideAst) },
    [blockquote(InsideAst, Attrs)],
    djot_ast_(Lines, []).

djot_blockquote_ast_([], Block, Attrs) -->
    djot_blockquote_ast_([""], Block, Attrs).

blockquote_line(Text) -->
    "> ", seq(Text).

blockquote_line("") -->
    ">".

djot_div_ast_([Line|Lines], N, Block, Attrs) -->
    { \+ (phrase(colons(M), Line), M >= N), append(Block, ['\n'|Line], Block1) },
    djot_div_ast_(Lines, N, Block1, Attrs).

djot_div_ast_([Line|Lines], N, Block, Attrs) -->
    { phrase(colons(M), Line), M >= N, djot_ast(Block, InsideAst) },
    [div_block(InsideAst, Attrs)],
    djot_ast_(Lines, []).

djot_div_ast_([], _, Block, Attrs) -->
    { djot_ast(Block, InsideAst) },
    [div_block(InsideAst, Attrs)].

pipe_table(Row) -->
    "|",
    table_row(Row),
    "|",
    whites(_).

table_row([Ast|Xs]) -->
    table_str(X),
    {
	inline_text_ast(X, Ast),
	append(_, [T], X),
	dif(T, '\\')
    },
    "|",
    table_row(Xs).

table_row([Ast]) -->
    table_str(X), { length(X, N), N > 0, inline_text_ast(X, Ast) }.

table_str([]) --> [].
table_str([X|Xs]) -->
    [X],
    { dif(X, '`') },
    table_str(Xs).
table_str(X) -->
    backticks(N, Backticks),
    backticks_end(N, Str),
    table_str(Xs),
    { append(Backticks, Str, Str0), append(Str0, Backticks, Str1), append(Str1, Xs, X) }.

separator_table(Style) -->
    "|",
    table_style(Style),
    "|",
    whites(_).
table_style([none|Xs]) -->
    dashes,
    "|",
    table_style(Xs).
table_style([none]) -->
    dashes.
table_style([left|Xs]) -->
    ":", dashes,
    "|",
    table_style(Xs).
table_style([left]) -->
    ":", dashes.
table_style([right|Xs]) -->
    dashes, ":",
    "|",
    table_style(Xs).
table_style([right]) -->
    dashes, ":".
table_style([center|Xs]) -->
    ":", dashes, ":",
    "|",
    table_style(Xs).
table_style([center]) -->
    ":", dashes, ":".

dashes --> "-" | "-", dashes.
	

djot_table_ast_([Line|Lines], Rows, Attrs) -->
    {
	phrase(separator_table(Style), Line),
	append(RestRows, [row(Row)], Rows),
	append(RestRows, [header(Row), set_style(Style)], Rows1)
    },
    djot_table_ast_(Lines, Rows1, Attrs).

djot_table_ast_([Line|Lines], Rows, Attrs) -->
    {
	phrase(pipe_table(Row), Line),
	append(Rows, [row(Row)], Rows1)
    },
    djot_table_ast_(Lines, Rows1, Attrs).
    
djot_table_ast_([Line|Lines], Rows, Attrs) -->
    { \+ phrase(pipe_table(_), Line) },
    [table(Rows, Attrs)],
    djot_ast_(Lines, []).
djot_table_ast_([], Rows, Attrs) -->
    djot_table_ast_([""], Rows, Attrs).

djot_paragraph_ast_([Line|Lines], Paragraph0, Attrs) -->
    {
	Line \= "",
	(
	    Paragraph0 = "" ->
	    Line = Paragraph1
	;   append(Paragraph0, [' '|Line], Paragraph1)
	)
    },
    djot_paragraph_ast_(Lines, Paragraph1, Attrs).

djot_paragraph_ast_([""|Lines], Paragraph, Attrs) -->
    { phrase(inline_text_ast_(InlineAst), Paragraph) },
    [paragraph(InlineAst, Attrs)],
    djot_ast_(Lines, []).

djot_paragraph_ast_([], Paragraph, Attrs) -->
    djot_paragraph_ast_([""], Paragraph, Attrs).

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

ast_html_node_(thematic_break(Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },
    "<hr", AttrsHtml,">".
ast_html_node_(paragraph(InlineAst, Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },
    "<p", AttrsHtml, ">",
    ast_html_(InlineAst),
    "</p>".
ast_html_node_(section(N, Header, Child, Attrs)) -->
    { phrase(ast_html_(Child), ChildHtml) },
    { attrs_html(Attrs, AttrsHtml) },
    format_("<section~s><h~d>~s</h~d>~s</section>", [AttrsHtml, N, Header, N, ChildHtml]).
ast_html_node_(blockquote(Child, Attrs)) -->
    { phrase(ast_html_(Child), ChildHtml) },
    { attrs_html(Attrs, AttrsHtml) },    
    "<blockquote", AttrsHtml, ">", ChildHtml, "</blockquote>".
ast_html_node_(list(type(_, bullet(_), Mode), Items, Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },    
    "<ul", AttrsHtml, ">",
    ast_html_node_items_(Items, Mode),
    "</ul>".
ast_html_node_(code(Spec, Code, Attrs)) -->
    { dif(Spec, "=html"), phrase(escape_html_(Html), Code) },
    { attrs_html(Attrs, AttrsHtml) },    
    "<pre", AttrsHtml, "><code>", Html, "</pre></code>".
ast_html_node_(code("=html", Html, _)) -->
    Html.
ast_html_node_(div_block(Block, Attrs)) -->
    { phrase(ast_html_(Block), Html) },
    { attrs_html(Attrs, AttrsHtml) },    
    "<div", AttrsHtml, ">", Html, "</div>".
ast_html_node_(table(Rows, Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },    
    "<table", AttrsHtml, ">",
    ast_html_rows_(Rows, []),
    "</table>".
ast_html_node_(link(TextAst, Url, Attrs)) -->
    { phrase(ast_html_(TextAst), TextHtml) },
    { attrs_html(Attrs, AttrsHtml) },
    format_("<a href=\"~s\"~s>~s</a>", [Url, AttrsHtml, TextHtml]).
ast_html_node_(image(AltText, Url, Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },    
    format_("<img alt=\"~s\" src=\"~s\"~s>", [AltText, Url, AttrsHtml]).
ast_html_node_(verbatim(Text, Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },    
    "<code", AttrsHtml, ">", Text, "</code>".
ast_html_node_(emphasis(Child, Attrs)) -->
    { phrase(ast_html_(Child), ChildHtml) },
    { attrs_html(Attrs, AttrsHtml) },
    "<em", AttrsHtml,">", ChildHtml, "</em>".
ast_html_node_(strong(Child, Attrs)) -->
    { phrase(ast_html_(Child), ChildHtml) },
    { attrs_html(Attrs, AttrsHtml) },    
    "<strong", AttrsHtml, ">", ChildHtml, "</strong>".
ast_html_node_(highlight(Str, Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },    
    "<mark", AttrsHtml, ">", Str, "</mark>".
ast_html_node_(superscript(Str, Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },        
    "<sup", AttrsHtml, ">", Str, "</sup>".
ast_html_node_(subscript(Str, Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },    
    "<sub", AttrsHtml, ">", Str, "</sub>".
ast_html_node_(insert(Str, Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },
    "<ins", AttrsHtml, ">", Str, "</ins>".
ast_html_node_(delete(Str, Attrs)) -->
    { attrs_html(Attrs, AttrsHtml) },    
    "<del", AttrsHtml, ">", Str, "</del>".
ast_html_node_(str(Str)) -->
    { phrase(escape_html_(Html), Str) },
    Html.

ast_html_node_items_([], _) --> "".
ast_html_node_items_([item(Item)|Items], loose) -->
    { phrase(ast_html_(Item), Html) },
    "<li>",
    Html,
    "</li>",
    ast_html_node_items_(Items, loose).
ast_html_node_items_([item([paragraph(Item, _)])|Items], tight) -->
    { phrase(ast_html_(Item), Html) },
    "<li>",
    Html,
    "</li>",
    ast_html_node_items_(Items, tight).

ast_html_rows_([], _) --> "".
ast_html_rows_([row(Row)|Rows], Style) -->
    "<tr>",
    ast_html_cell_("td", Row, Style),
    "</tr>",
    ast_html_rows_(Rows, Style).
ast_html_rows_([header(Row)|Rows], Style) -->
    "<tr>",
    ast_html_cell_("th", Row, Style),
    "</tr>",
    ast_html_rows_(Rows, Style).

ast_html_rows_([set_style(Style)|Rows], _) -->
    ast_html_rows_(Rows, Style).

ast_html_cell_(_, [], _) --> "".
ast_html_cell_(Type, [X|Xs], [none|Style]) -->
    { phrase(ast_html_(X), Html) },
    "<", Type, ">",
    Html,
    "</", Type, ">",
    ast_html_cell_(Type, Xs, Style).
ast_html_cell_(Type, [X|Xs], [left|Style]) -->
    { phrase(ast_html_(X), Html) },
    "<", Type, " style=\"text-align:left;\">",
    Html,
    "</", Type, ">",
    ast_html_cell_(Type, Xs, Style).
ast_html_cell_(Type, [X|Xs], [right|Style]) -->
    { phrase(ast_html_(X), Html) },
    "<", Type, " style=\"text-align:right;\">",
    Html,
    "</", Type, ">",
    ast_html_cell_(Type, Xs, Style).
ast_html_cell_(Type, [X|Xs], [center|Style]) -->
    { phrase(ast_html_(X), Html) },
    "<", Type, " style=\"text-align:center;\">",
    Html,
    "</", Type, ">",
    ast_html_cell_(Type, Xs, Style).
ast_html_cell_(Type, [X|Xs], []) -->
    { phrase(ast_html_(X), Html) },
    "<", Type, ">",
    Html,
    "</", Type, ">",
    ast_html_cell_(Type, Xs, []).

escape_html_([]) --> [].
escape_html_(Html) -->
    [Char],
    {
        Char = '&', append("&amp;", Html0, Html)
    },
    escape_html_(Html0).
escape_html_(Html) -->
    [Char],
    {
	Char = (<), append("&lt;", Html0, Html)
    },
    escape_html_(Html0).
escape_html_(Html) -->
    [Char],
    {
	Char = (>), append("&gt;", Html0, Html)
    },
    escape_html_(Html0).
escape_html_([Char|Html0]) -->
    [Char],
    escape_html_(Html0).

attrs_html([], "").
attrs_html([Key-Value|Xs], Html) :-
    attrs_html(Xs, Html1),
    phrase(format_(" ~s=\"~s\"", [Key, Value]), Html0),
    append(Html0, Html1, Html).

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

insert_ast_([insert(Str, Attrs)|Ast0]) -->
    "{+",
    seq(Str),
    "+}",
    inline_attr_ast_(Attrs),    
    inline_text_ast_(Ast0).

delete_ast_([delete(Str, Attrs)|Ast0]) -->
    "{-",
    seq(Str),
    "-}",
    inline_attr_ast_(Attrs),    
    inline_text_ast_(Ast0).

superscript_ast_([superscript(Str, Attrs)|Ast0]) -->
    ( "^" | "{^" ),
    seq(Str),
    ( "^" | "^}" ),
    inline_attr_ast_(Attrs),    
    inline_text_ast_(Ast0).

subscript_ast_([subscript(Str, Attrs)|Ast0]) -->
    ( "~" | "{~" ),
    seq(Str),
    ( "~" | "~}" ),
    inline_attr_ast_(Attrs),    
    inline_text_ast_(Ast0).

highlight_ast_([highlight(Str, Attrs)|Ast0]) -->
    "{=",
    seq(Str),
    "=}",
    inline_attr_ast_(Attrs),    
    inline_text_ast_(Ast0).

emphasis_ast_([emphasis(InlineAst, Attrs)|Ast0]) -->
    ( ("_", look_ahead(T), { T \= ' ' }) | "{_" ),
    seq(Inline),
    "_",
    {
	append(_, [G], Inline),
	G \= ' ',
	inline_text_ast(Inline, InlineAst)
    },
    inline_attr_ast_(Attrs),    
    inline_text_ast_(Ast0).

emphasis_ast_([emphasis(InlineAst, Attrs)|Ast0]) -->
    ( ("_", look_ahead(T), { T \= ' ' }) | "{_" ),
    seq(Inline),
    "_}",
    { inline_text_ast(Inline, InlineAst) },
    inline_attr_ast_(Attrs),    
    inline_text_ast_(Ast0).

strong_ast_([strong(InlineAst, Attrs)|Ast0]) -->
    ( ("*", look_ahead(T), { T \= ' ' }) | "{*" ),
    seq(Inline),
    "*",
    {
	append(_, [G], Inline),
	G \= ' ',
	inline_text_ast(Inline, InlineAst)
    },
    inline_attr_ast_(Attrs),    
    inline_text_ast_(Ast0).

strong_ast_([strong(InlineAst, Attrs)|Ast0]) -->
    ( ("*", look_ahead(T), { T \= ' ' }) | "{*" ),
    seq(Inline),
    "*}",
    { inline_text_ast(Inline, InlineAst) },
    inline_attr_ast_(Attrs),
    inline_text_ast_(Ast0).

verbatim_ast_([verbatim(Str, Attrs)|Ast0]) -->
    backticks(N, _),
    backticks_end(N, Str),
    inline_attr_ast_(Attrs),
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

colons(N) -->
    ":",
    colons(N0),
    { N is N0 + 1 }.
colons(1) --> ":".

autolink_ast_([link(Url, Url, Attrs)|Ast0]) -->
    { append("http://", _, Url); append("https://", _, Url) },
    "<",
    seq(Url),
    ">",
    inline_attr_ast_(Attrs),
    inline_text_ast_(Ast0).

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

inline_image_ast_([image(AltText, Url, Attrs)|Ast0]) -->
    "![",
    seq(AltText),
    "](",
    seq(Url),
    ")",
    inline_attr_ast_(Attrs),
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

inline_link_ast_([link(LinkAst, LinkUrl, Attrs)|Ast0]) -->
    "[",
    seq(LinkText),
    "](",
    seq(LinkUrl),
    ")",
    { inline_text_ast(LinkText, LinkAst) },
    inline_attr_ast_(Attrs),
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

inline_attr_ast_(Attrs) -->
    inline_attr_ast_single_(Attr0),
    inline_attr_ast_(Attr1),
    { append(Attr0, Attr1, Attrs) }.
inline_attr_ast_([]) --> [].

inline_attr_ast_single_(Attrs) -->
    "{",
    seq(AttrText),
    "}",
    { phrase(attr_pairs(Attrs), AttrText) }.


attr_pairs(["id"-Name|Xs]) -->
    "#", seq(Name), whites(N), { N > 0 }, attr_pairs(Xs).

attr_pairs(["id"-Name]) -->
    "#", seq(Name), whites(0).

attr_pairs(["class"-Name|Xs]) -->
    ".", seq(Name), whites(N), { N > 0 }, attr_pairs(Xs).
attr_pairs(["class"-Name]) -->
    ".", seq(Name), whites(0).

attr_pairs([Key-Value|Xs]) -->
    seq(Key), { length(Key, N), N > 0 },
    "=",
    seq(Value), { length(Value, M), M > 0},
    whites(O), { O > 0 }, attr_pairs(Xs).

attr_pairs([Key-Value]) -->
    seq(Key), { length(Key, N), N > 0 },
    "=",
    seq(Value), { length(Value, M), M > 0},
    whites(0).


look_ahead(T), [T] --> [T].
