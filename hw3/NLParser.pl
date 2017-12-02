:- use_module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).

main :-
	open('NL-input.txt', read, Str),
	read_file(Str,Lines),
	%Convert the lines in file to an list of sentences that are lists of words
	lines_to_words(Lines, Words),
	close(Str),
	start(X, Y),
	open('NL-parse-solution.txt', write, Output),
	parse_sentences(X, Y, Words, Output),
	close(Output).

parse_sentences(_, _, [], _). % Base case, all sentences parsed
parse_sentences(X, Y, [Sentence|Sentences], Output) :-
	phrase(sentence, Sentence),
	phrase((subject_phrase,verb,list(M)), Sentence),
	(
		(
			phrase(object_phrase,M),
			button(X, Y, _),
			writeln(Output, "Valid move"),
			parse_sentences(X, Y, Sentences, Output)
		);
		(
			phrase(direction_object_phrase,M),
			phrase((list([A|[]]),object,list([D|[]])), M),
			number_codes(N, A),
			move(X, Y, NX, NY, N, D),
			writeln(Output, "Valid move"),
			parse_sentences(NX, NY, Sentences, Output)
		);
		writeln(Output, "Not a valid move")
	).
parse_sentences(X, Y, [Sentence|Sentences], Output) :-
	\+ phrase(sentence, Sentence),
	writeln(Output, "Not a valid sentence"),
	parse_sentences(X, Y, Sentences, Output).

translate(X, Y, X, NY, "up") :-
	NY is Y-1.
translate(X, Y, X, NY, "down") :-
	NY is Y+1.
translate(X, Y, NX, Y, "left") :-
	NX is X-1.
translate(X, Y, NX, Y, "right") :-
	NX is X+1.

move(X, Y, X, Y, 0, _).
move(X, Y, NX, NY, N, D) :-
	translate(X, Y, PX, PY, D),
	\+ wall(PX, PY),
	info(W, H, _),
	0 =< PX, PX < W, % Check bounds
	0 =< PY, PY < H,
	NN is N-1,
	move(PX, PY, NX, NY, NN, D).

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

list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

article --> ["a"]; ["the"].
subject --> ["rat"]; ["rodent"].
subject_phrase --> ["einstein"]; ["he"]; ["it"]; (article, subject).
verb --> ["moved"]; ["pushed"]; ["ran"]; ["scurried"].
number_speech --> ["1"]; ["2"]; ["3"]; ["4"]; ["5"]; ["6"]; ["7"]; ["8"]; ["9"].
object --> ["button"]; ["cell"]; ["cells"]; ["square"]; ["squares"].
direction --> ["up"]; ["down"]; ["left"]; ["right"].
direction_object_phrase --> number_speech, object, direction.
object_phrase --> article, object.
verb_phrase --> verb, (direction_object_phrase; object_phrase).
sentence --> subject_phrase, verb_phrase.