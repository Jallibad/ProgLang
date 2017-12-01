:- use_module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).

solve(_, X, Y, X, Y, Visited, [[X,Y]|Visited]).
solve(CanButton, X, Y, GX, GY, Visited, Solution) :-
	maplist(dif([X,Y]), Visited),
	info(W, H, _),
	0 =< X, X < W,
	0 =< Y, Y < H,
	(
		CanButton;
		\+ button(X, Y, _)
	),
	\+ wall(X,Y),
	X1 is X-1,
	X2 is X+1,
	Y1 is Y-1,
	Y2 is Y+1,
	NVisited = [[X,Y]|Visited],
	(
		solve(CanButton, X1, Y, GX, GY, NVisited, Solution);
		solve(CanButton, X, Y1, GX, GY, NVisited, Solution);
		solve(CanButton, X2, Y, GX, GY, NVisited, Solution);
		solve(CanButton, X, Y2, GX, GY, NVisited, Solution)
	).

solveButton(X, Y, ButtonID, Path) :-
	num_buttons(ButtonID),
	goal(GX, GY),
	solve(true, X, Y, GX, GY, [], Path).
solveButton(X, Y, ButtonID, Path) :-
	button(GX, GY, ButtonID),
	NButtonID is ButtonID+1,
	solveButton(GX, GY, NButtonID, PartialPath1),
	solve(false, X, Y, GX, GY, [], [_|PartialPath2]),
	append(PartialPath1, PartialPath2, Path).

main :-
	start(X,Y),
	goal(GX,GY),
	open("path-solution.txt", write, File),
	(
		(
			info(_,_,a),
			solve(true, X, Y, GX, GY, [], BSolution),
			reverse(BSolution, Solution),
			maplist(writeln(File), Solution)
		);
		(
			info(_,_,_),
			solveButton(X, Y, 1, BSolution),
			reverse(BSolution, Solution),
			maplist(writeln(File), Solution)
		)
	),
	close(File).