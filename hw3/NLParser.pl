main :-
    open('NL-input.txt', read, Str),
    read_file(Str,Lines),
    %Convert the lines in file to an list of sentences that are lists of words
    lines_to_words(Lines, Words),
    close(Str),
    write(Words), nl.


% Credit to StackOverflow and author Ishq for file parser
% https://stackoverflow.com/a/4805931
% https://stackoverflow.com/users/577045/ishq
read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).

%Converts sentence to a list of words
lines_to_words([], []).
lines_to_words([H|T], [H2|T2]) :-
	split_string(H, " ", "", H2),
	lines_to_words(T, T2).

article("a").
article("the").

subject("rat").
subject("rodent").

subject_phrase(["einstein"]).
subject_phrase(["he"]).
subject_phrase(["it"]).
subject_phrase([A,S|[]]) :-
	article(A),
	subject(S).

verb("ran").
verb("moved").
verb("pushed").
verb("scurried").

number_speech(N) :- number_codes(X, N), 0 < X, X < 10.

object("button").
object("cell").
object("cells").
object("square").
object("squares").

direction("up").
direction("down").
direction("left").
direction("right").

direction_object_phrase([N,O,D|[]]) :- number_speech(N), object(O), direction(D).

object_phrase([A,O|[]]) :- article(A), object(O).

verb_phrase([X|Y]) :- verb(X), (direction_object_phrase(Y); object_phrase(Y)).

sentence(S) :- append(X,Y,S), subject_phrase(X), verb_phrase(Y).