:- use_module(djota).

:- object(test, extends(lgtunit)).

test(backslash_ast) :-
    djota:inline_text_ast("Hello \\* Djota", [str("Hello * Djota")]).

test(backslash) :-
    djota:djot("Hello \\* Djota", "<p>Hello * Djota</p>").

test(link_ast) :-
    djota:inline_text_ast("My link [link](http://example.com) hola", [str("My link "),link("link","http://example.com"),str(" hola")]),
    djota:inline_text_ast("[My link text][foo bar]", [link_ref("My link text","foo bar")]).

test(link) :-
    djota:djot("My link [link](http://example.com) hola", "<p>My link <a href=\"http://example.com\">link</a> hola</p>").

test(image_ast) :-
    djota:inline_text_ast("![picture of a cat](cat.jpg)", [image("picture of a cat", "cat.jpg")]),
    djota:inline_text_ast("![picture of a cat][cat]", [image_ref("picture of a cat", "cat")]).

test(image) :-
    djota:djot("![picture of a cat](cat.jpg)", "<p><img alt=\"picture of a cat\" src=\"cat.jpg\"></p>").

test(autolink_ast) :-
    djota:inline_text_ast("Welcome to <https://www.scryer.pl>!", [str("Welcome to "),link("https://www.scryer.pl","https://www.scryer.pl"),str("!")]).

test(paragraph) :-
    djota:djot("Hello friends\nof [YouTube](https://youtube.com)", "<p>Hello friends of <a href=\"https://youtube.com\">YouTube</a></p>"),
    djota:djot("Hello Prolog!\n\nHello Djot!\n\n", "<p>Hello Prolog!</p><p>Hello Djot!</p>").

test(thematic_break) :-
    djota:djot("Then they went to sleep.\n\n  * * * *  \n\nWhen they woke up, ...", "<p>Then they went to sleep.</p><hr><p>When they woke up, ...</p>").

test(section) :-
    djota:djot("Then they went to\n\n# sleep.\n\n  * * * *  \n\nWhen they woke up, ...\n\n# Hola\n\nIt's me", "<p>Then they went to</p><section><h1>sleep.</h1><hr><p>When they woke up, ...</p></section><section><h1>Hola</h1><p>It\'s me</p></section>").

:- end_object.