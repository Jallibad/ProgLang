-module(simulation).
-export([run/1, process/7]).

makeNode(ID, Priority, Tolerance) ->
	spawn(simulation, process, [ID, Priority, Tolerance, undefined, undefined, true, undefined]).

tieList([A, B | C]) ->
	A ! {setRight, B},
	B ! {setLeft, A},
	tieList([B|C]);
tieList(_) -> [].

process(Id, Priority, Tolerance, Left, Right, AP, Leader) ->
	receive
		{setLeft, NewLeft} ->
			process(Id, Priority, Tolerance, NewLeft, Right, AP, Leader);
		{setRight, NewRight} ->
			process(Id, Priority, Tolerance, Left, NewRight, AP, Leader);
		{timeStamp, T} ->
			io:format("~w: t=~w~n", [Id, T]),
			Left ! {timeStamp, T+1},
			process(Id, Priority, Tolerance, Left, Right, AP, Leader);
		{rg, OriginalSender, Sender, P, LeaderBit, OTTL, TTL} ->
			if
				OriginalSender == self() ->
					io:format("~w elected Leader with priority ~w~n", [Id, Priority]);
				TTL == 0 ->
					%io:format("~w ~w~n",[P, Priority]),
					Sender ! {reply, LeaderBit, OTTL}; % Send a reply msg including leader bit
				Sender == Left -> % Otherwise forward to neighbor that didn't send the message
					Right ! {rg, OriginalSender, self(), P, LeaderBit and (Priority =< P), OTTL, TTL-1};
				Sender == Right ->
					Left ! {rg, OriginalSender, self(), P, LeaderBit and (Priority =< P), OTTL, TTL-1}
			end,
			if
				P > Priority ->
					io:format("~w can't become leader~n", [Id]),
					process(Id, Priority, Tolerance, Left, Right, false, Leader); % Become passive if the sender has a higher priority
				true ->
					process(Id, Priority, Tolerance, Left, Right, AP, Leader)
			end;
		{reply, LeaderBit, OTTL} ->
			if
				LeaderBit ->
					Left ! {rg, self(), self(), Priority, LeaderBit, OTTL*2, OTTL*2},
					Right ! {rg, self(), self(), Priority, LeaderBit, OTTL*2, OTTL*2},
					process(Id, Priority, Tolerance, Left, Right, AP, Leader);
				true ->
					process(Id, Priority, Tolerance, Left, Right, false, Leader)
			end;
		electLeader ->
			Left ! {rg, self(), self(), Priority, true, 1, 1},
			Right ! {rg, self(), self(), Priority, true, 1, 1};
		finish -> ok
	end.

run(File) ->
	P = [makeNode(ID, Priority, Tolerance) || {ID, _, _, Priority, Tolerance} <- parser:read(File)],
	tieList(P),
	lists:last(P) ! {setRight, hd(P)}, % Link front and back of list
	hd(P) ! {setLeft, lists:last(P)},
	[X ! electLeader || X <- P],
	[X ! finish || X <- P],
	%hd(P) ! {timeStamp, 0},
	io:format("~w~n", [hd(P)]).