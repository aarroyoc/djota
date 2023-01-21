:- use_module(djota).

:- object(test, extends(lgtunit)).

test(backslash_ast) :-
    djota:inline_text_ast("Hello \\* Djota", [str("Hello * Djota")]).

test(backslash) :-
    djota:djot("Hello \\* Djota", "<p>Hello * Djota</p>").

test(link_ast) :-
    djota:inline_text_ast("My link [link](http://example.com) hola", [str("My link "),link([str("link")],"http://example.com", []),str(" hola")]),
    djota:inline_text_ast("[My link text][foo bar]", [link_ref("My link text","foo bar")]).

test(link) :-
    djota:djot("My link [link](http://example.com) hola", "<p>My link <a href=\"http://example.com\">link</a> hola</p>").

test(image_ast) :-
    djota:inline_text_ast("![picture of a cat](cat.jpg)", [image("picture of a cat", "cat.jpg", [])]),
    djota:inline_text_ast("![picture of a cat][cat]", [image_ref("picture of a cat", "cat")]).

test(image) :-
    djota:djot("![picture of a cat](cat.jpg)", "<p><img alt=\"picture of a cat\" src=\"cat.jpg\"></p>").

test(autolink_ast) :-
    djota:inline_text_ast("Welcome to <https://www.scryer.pl>!", [str("Welcome to "),link("https://www.scryer.pl","https://www.scryer.pl", []),str("!")]).

test(verbatim_ast) :-
    djota:inline_text_ast("``Verbatim with a backtick` character``", [verbatim("Verbatim with a backtick` character", [])]),
    djota:inline_text_ast("`Verbatim with three backticks ``` character`", [verbatim("Verbatim with three backticks ``` character", [])]).

test(verbatim) :-
    djota:djot("``Verbatim with a backtick` character``", "<p><code>Verbatim with a backtick` character</code></p>"),
    djota:djot("`Verbatim with three backticks ``` character`", "<p><code>Verbatim with three backticks ``` character</code></p>").

test(emphasis_ast) :-
    djota:inline_text_ast("Hello _Prolog_!", [str("Hello "),emphasis([str("Prolog")], []),str("!")]),
    djota:inline_text_ast("Hello _ Prolog_!", [str("Hello _ Prolog_!")]),
    djota:inline_text_ast("Hello _Hello_Prolog _ _!", [str("Hello "),emphasis([str("Hello")], []),str("Prolog _ _!")]),
    djota:inline_text_ast("Hello _Hello_Prolog_ _!", [str("Hello "),emphasis([str("Hello_Prolog")], []),str(" _!")]),
    djota:inline_text_ast("Hello _Hello_Prolog__!", [str("Hello "),emphasis([str("Hello"),emphasis([str("Prolog")], [])], []),str("!")]).

test(strong_ast) :-
    djota:inline_text_ast("*HelloHello*Prolog*_!", [strong([str("HelloHello*Prolog")], []),str("_!")]).

test(emphasis_strong) :-
    djota:djot("Hello _Prolog_! You said: _I *need* to_wake up__", "<p>Hello <em>Prolog</em>! You said: <em>I <strong>need</strong> to</em>wake up__</p>").

test(highlight_ast) :-
    djota:inline_text_ast("Hello {=Prolog=}!", [str("Hello "), highlight("Prolog", []), str("!")]).

test(highlight) :-
    djota:djot("Hello {=Prolog=}!", "<p>Hello <mark>Prolog</mark>!</p>").

test(super_subscript_ast) :-
    djota:inline_text_ast("H~2~O and djot^TM^", [str("H"),subscript("2", []),str("O and djot"),superscript("TM", [])]).

test(super_subscript) :-
    djota:djot("H~2~O and djot^TM^", "<p>H<sub>2</sub>O and djot<sup>TM</sup></p>").

test(insert_delete_ast) :-
    djota:inline_text_ast("My boss is {-mean-}{+nice+}.", [str("My boss is "),delete("mean", []),insert("nice", []),str(".")]).

test(insert_delete) :-
    djota:djot("My boss is {-mean-}{+nice+}.", "<p>My boss is <del>mean</del><ins>nice</ins>.</p>").

test(paragraph) :-
    djota:djot("Hello friends\nof [YouTube](https://youtube.com){.video}", "<p>Hello friends of <a href=\"https://youtube.com\" class=\"video\">YouTube</a></p>"),
    djota:djot("Hello Prolog!\n\nHello Djot!\n\n", "<p>Hello Prolog!</p><p>Hello Djot!</p>").

test(thematic_break) :-
    djota:djot("Then they went to sleep.\n\n  * * * *  \n\nWhen they woke up, ...", "<p>Then they went to sleep.</p><hr><p>When they woke up, ...</p>").

test(section) :-
    djota:djot("Then they went to\n\n# sleep.\n\n  * * * *  \n\nWhen they woke up, ...\n\n# Hola\n\nIt's me", "<p>Then they went to</p><section><h1>sleep.</h1><hr><p>When they woke up, ...</p></section><section><h1>Hola</h1><p>It\'s me</p></section>").

test(blockquote) :-
    djota:djot("> This is a block quote.\n>\n> Hello again", "<blockquote><p>This is a block quote.</p><p>Hello again</p></blockquote>"),
    djota:djot("> This is a block quote.\nAnd lazy", "<blockquote><p>This is a block quote. And lazy</p></blockquote>").

test(list_ast) :-
    djota:djot_ast("- Hola", [list(type(0, bullet("-"), tight),[item([paragraph([str("Hola")])])])]),
    djota:djot_ast("- Hola\n- Adios", [list(type(0, bullet("-"), tight),[item([paragraph([str("Hola")])]),item([paragraph([str("Adios")])])])]),
    djota:djot_ast("- Hola\namigos\n- Adios\namigos", [list(type(0,bullet("-"),tight),[item([paragraph([str("Hola amigos")])]),item([paragraph([str("Adios amigos")])])])]),
    djota:djot_ast("- Hola\namigos\n\n- Adios\namigos", [list(type(0,bullet("-"),loose),[item([paragraph([str("Hola amigos")])]),item([paragraph([str("Adios amigos")])])])]),    
    djota:djot_ast("- Hola\namigos\n\n - Sublist\n- Adios\namigos", [list(type(0, bullet("-"), loose),[item([paragraph([str("Hola amigos")]),list(type(0, bullet("-"), tight),[item([paragraph([str("Sublist")])])])]),item([paragraph([str("Adios amigos")])])])]).

test(list) :-
    djota:djot("- Hola\namigos\n\n - Sublist\n- Adios\namigos", "<ul><li><p>Hola amigos</p><ul><li>Sublist</li></ul></li><li><p>Adios amigos</p></li></ul>").

test(code_block) :-
    djota:djot("````\nThis is how you do a code block:\n\n``` ruby\nx = 5 * 6\n```\n````", "<pre><code>\nThis is how you do a code block:\n\n``` ruby\nx = 5 * 6\n```</pre></code>"),
    djota:djot("> ```\n> code in a\n> block quote\n\nParagraph.", "<blockquote><pre><code>\ncode in a\nblock quote</pre></code></blockquote><p>Paragraph.</p>"),
    djota:djot("````\nThis is <html></html>````", "<pre><code>\nThis is &lt;html&gt;&lt;/html&gt;````</pre></code>").

test(raw_block) :-
    djota:djot("``` =html\n<video width=320 height=240 controls>\n</video>\n```", "\n<video width=320 height=240 controls>\n</video>").

test(div_block) :-
    djota:djot("::: warning\nHere is a paragraph.\n\nAnd here is another.\n:::", "<div class=\"warning\"><p>Here is a paragraph.</p><p>And here is another.</p></div>").

test(attr_pairs) :-
    djota:djot_ast("[Hola](/){.blue}{#id hola=hello} hola", [paragraph([link([str("Hola")],"/",["class"-"blue","id"-"id","hola"-"hello"]),str(" hola")])]).

test(link_emphasis) :-
    djota:djot("CLP(B) is an instance of the general [CLP(_X_) scheme](#clp)", "<p>CLP(B) is an instance of the general <a href=\"#clp\">CLP(<em>X</em>) scheme</a></p>").

test(table_ast) :-
    djota:djot_ast("| 1 | 2 |\n| 3 | 4 | 5 |", [table([row([[str(" 1 ")],[str(" 2 ")]]),row([[str(" 3 ")],[str(" 4 ")],[str(" 5 ")]])])]).

test(table) :-
    djota:djot("| 1 | 2 |\n| 3 | 4 | 5 |", "<table><tr><td> 1 </td><td> 2 </td></tr><tr><td> 3 </td><td> 4 </td><td> 5 </td></tr></table>"),
    djota:djot("|fruit|price|\n|---|---:|\n|apple|4|\n|banana|10|", "<table><tr><th>fruit</th><th>price</th></tr><tr><td>apple</td><td style=\"text-align:right;\">4</td></tr><tr><td>banana</td><td style=\"text-align:right;\">10</td></tr></table>").

:- end_object.