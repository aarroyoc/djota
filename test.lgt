:- use_module(djota).

:- object(test, extends(lgtunit)).

test(backslash) :-
    phrase(djota:inline_text("Hello * Djota"), "Hello \\* Djota").

:- end_object.