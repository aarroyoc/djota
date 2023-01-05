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



:- end_object.