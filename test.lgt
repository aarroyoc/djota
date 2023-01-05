:- use_module(djota).

:- object(test, extends(lgtunit)).

test(backslash) :-
    phrase(djota:inline_text("Hello * Djota", []), "Hello \\* Djota").

test(link) :-
    phrase(djota:inline_text("<a href=\"http://example.com\">My link text</a>", []), "[My link text](http://example.com)"),
    phrase(djota:inline_text("<a href=\"http://example.com\">My link text</a>", [ref("foo bar", "http://example.com")]), "[My link text][foo bar]"),
    phrase(djota:inline_text("<a>foo</a>", []), "[foo][bar]"),
    phrase(djota:inline_text("<a href=\"/url\">My link text</a>", [ref("My link text", "/url")]), "[My link text][]").

test(image) :-
    phrase(djota:inline_text("<img alt=\"picture of a cat\" src=\"cat.jpg\">", []), "![picture of a cat](cat.jpg)"),
    phrase(djota:inline_text("<img alt=\"picture of a cat\" src=\"feline.jpg\">", [ref("cat", "feline.jpg")]), "![picture of a cat][cat]"),
    phrase(djota:inline_text("<img alt=\"cat\" src=\"feline.jpg\">", [ref("cat", "feline.jpg")]), "![cat][]").

test(paragraph) :-
    djota:djot("Hello friends\nof [YouTube](https://youtube.com)", "<p>Hello friends of <a href=\"https://youtube.com\">YouTube</a></p>"),
    djota:djot("Hello Prolog!\n\nHello Djot!\n\n", "<p>Hello Prolog!</p><p>Hello Djot!</p>").

test(thematic_break) :-
    djota:djot("Then they went to sleep.\n\n  * * * *  \n\nWhen they woke up, ...", "<p>Then they went to sleep.</p><hr><p>When they woke up, ...</p>").

test(section) :-
    djota:djot("Then they went to\n\n# sleep.\n\n  * * * *  \n\nWhen they woke up, ...\n\n# Hola\n\nIt's me", "<p>Then they went to</p><section><h1>sleep.</h1><hr><p>When they woke up, ...</p></section><section><h1>Hola</h1><p>It\'s me</p></section>").

:- end_object.