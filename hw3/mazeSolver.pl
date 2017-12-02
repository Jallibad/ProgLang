:- use_module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).

% This predicate solves the maze for arbitrary start and end points
% First argument is whether einstein can press any buttons (for type c)
% Second and third arguments are the starting location
% Fourth and fifth are the goal location
% Sixth is the list of visited locations to avoid infinite loops
% Seventh is the answer
solve(_, X, Y, X, Y, Visited, [[X,Y]|Visited]). % Base case
solve(CanButton, X, Y, GX, GY, Visited, Solution) :- % Recursive search
	maplist(dif([X,Y]), Visited), % Have we already passed here?
	info(W, H, _),
	0 =< X, X < W, % Check bounds
	0 =< Y, Y < H,
	( % Avoid buttons if need be
		CanButton;
		\+ button(X, Y, _)
	),
	\+ wall(X,Y), % Avoid walls
	X1 is X-1, % REEEEE
	X2 is X+1,
	Y1 is Y-1,
	Y2 is Y+1,
	NVisited = [[X,Y]|Visited],
	(
		solve(CanButton, X1, Y, GX, GY, NVisited, Solution); % Left
		solve(CanButton, X, Y1, GX, GY, NVisited, Solution); % Up
		solve(CanButton, X2, Y, GX, GY, NVisited, Solution); % Right
		solve(CanButton, X, Y2, GX, GY, NVisited, Solution)  % Down
	).

% This predicate solves the maze touching each button in order
% First two arguments are start location
% Third is the next button that should be pressed
% Fourth is the answer
solveButton(X, Y, ButtonID, Path) :- % Base case
	num_buttons(ButtonID), % If we've pressed every button
	goal(GX, GY),
	% Path directly to the goal, we can ignore buttons now
	solve(true, X, Y, GX, GY, [], Path).
solveButton(X, Y, ButtonID, Path) :- % Recusively path to each button
	button(GX, GY, ButtonID), % Get the location of the current button
	NButtonID is ButtonID+1,
	solveButton(GX, GY, NButtonID, PartialPath1), % Recurse from button
	solve(false, X, Y, GX, GY, [], [_|PartialPath2]), % Path to button
	append(PartialPath1, PartialPath2, Path). % Combine the two for the answer

main :-
	start(X,Y),
	goal(GX,GY),
	open("path-solution.txt", write, File),
	(
		(
			info(_,_,a), % a type case
			solve(true, X, Y, GX, GY, [], BSolution),
			reverse(BSolution, Solution),
			maplist(writeln(File), Solution)
		);
		(
			info(_,_,_), % Otherwise handle b and c the same
			solveButton(X, Y, 1, BSolution),
			reverse(BSolution, Solution),
			maplist(writeln(File), Solution)
		)
	),
	close(File).