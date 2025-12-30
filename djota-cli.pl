:- use_module(djota).
:- use_module(library(format)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(iso_ext)).
:- use_module(library(files)).

:- initialization(main).

main :-
    '$toplevel':argv(Argv),
    (
        length(Argv, 1) ->
        Argv = [File],
        ( file_exists(File) ->
          render_file(File),
          halt
        ; portray_clause(invalid_argument(file_does_not_exist, file(File))),
          halt(2)
        )
    ;   portray_clause(invalid_argument(one_input_file_required_as_argument, current_args(Argv))),
        halt(1)
    ).

render_file(File) :-
    setup_call_cleanup(open(File, read, Stream),(
      phrase_from_stream(seq(Djot), Stream),
      djot(Djot, Html),
      phrase_to_stream(seq(Html), user_output)
    ), close(Stream)).
        
