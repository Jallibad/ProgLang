-module(simulation).
-export([run/1, process/10]).

makeNode(ID, Host, Node, Priority, Tolerance, Fd) ->
	spawn(list_to_atom(Node++"@"++Host), simulation, process, [ID, Priority, Tolerance, Tolerance, undefined, undefined, true, no, self(), Fd]).

tieList(P) ->
	tieListR(P),
	lists:last(P) ! {setRight, hd(P)}, % Link front and back of list
	hd(P) ! {setLeft, lists:last(P)}.

% Wrapped recursive function to set left and right "pointers" on each node
tieListR([A, B | C]) ->
	A ! {setRight, B},
	B ! {setLeft, A},
	tieListR([B|C]);
tieListR(_) -> [].

supervisor(Nodes, Leader, RunsLeft, Fd) ->
	receive
		{election, T} ->
			%io:format("Election now~n", []),
			if
				RunsLeft == 0 ->
					[X ! finish || X <- Nodes],
					file:write(Fd, io_lib:fwrite("End of simulation~n", []));
				true ->
					[X ! electLeader || X <- Nodes],
					supervisor(Nodes, T, RunsLeft-1, Fd)
			end;
		{elected, L, Id} ->
			if
				is_integer(Leader) -> % Basically just discard repeat elected messages
					file:write(Fd, io_lib:fwrite("ID=~w became leader at t=~w~n", [Id, Leader])),
					[X ! {electionEnded, Leader} || X <- Nodes], % Leader is actually the current time during an election.  Fuck.
					L ! {timeStamp, Leader, 0, trunc((length(Nodes)+1)/2)},
					supervisor(Nodes, L, RunsLeft, Fd);
				true -> supervisor(Nodes, Leader, RunsLeft, Fd)
			end
	end.

process(Id, Priority, Tolerance, CurrTolerance, Left, Right, AP, Leader, Supervisor, Fd) ->
	receive
		{setLeft, NewLeft} ->
			io:format("Setting left~n", []),
			process(Id, Priority, Tolerance, CurrTolerance, NewLeft, Right, AP, Leader, Supervisor, Fd);
		{setRight, NewRight} ->
			io:format("Setting right~n", []),
			process(Id, Priority, Tolerance, CurrTolerance, Left, NewRight, AP, Leader, Supervisor, Fd);
		{timeStamp, T, Revolts, NRevolts} ->
			%io:format("~w: t=~w~n", [Id, T]),
			if
				(Leader == yes) and (Revolts >= NRevolts) -> % If we're the Leader and half the nodes have revolted
					file:write(Fd, io_lib:fwrite("ID=~w was deposed at t=~w~n", [Id, T])),
					Supervisor ! {election, T+1},
					process(Id, Priority, Tolerance, CurrTolerance, Left, Right, AP, was, Supervisor, Fd);
				(Leader /= yes) and (CurrTolerance =< T) -> % If we're not the leader and we should revolt
					file:write(Fd, io_lib:fwrite("ID=~w revolted at t=~w~n", [Id, T])),
					Left ! {timeStamp, T+1, Revolts+1, NRevolts},
					process(Id, Priority, Tolerance, undefined, Left, Right, AP, Leader, Supervisor, Fd);
				true ->
					Left ! {timeStamp, T+1, Revolts, NRevolts},
					process(Id, Priority, Tolerance, CurrTolerance, Left, Right, AP, Leader, Supervisor, Fd)
			end;
		{rg, OriginalSender, Sender, P, LeaderBit, OTTL, TTL} ->
			%io:format("~w ~w~n", [self(), OriginalSender]),
			if
				(OriginalSender == self()) and LeaderBit ->
					%io:format("~w elected~n", [Id]),
					Supervisor ! {elected, self(), Id},
					process(Id, Priority, Tolerance, CurrTolerance, Left, Right, true, yes, Supervisor, Fd);
				TTL == 0 -> % If probe is done send reply msg
					%io:format("~w ~w~n",[P, Priority]),
					OriginalSender ! {reply, LeaderBit, OTTL}, % Send a reply msg including leader bit
					process(Id, Priority, Tolerance, CurrTolerance, Left, Right, AP and ((Leader == was) or comparePriority(P,{Id,Priority})), Leader, Supervisor, Fd); % Become passive if the sender has a higher priority
				Sender == Left -> % Otherwise forward to neighbor that didn't send the message
					Right ! {rg, OriginalSender, self(), P, LeaderBit and ((Leader == was) or comparePriority({Id,Priority},P)), OTTL, TTL-1},
					process(Id, Priority, Tolerance, CurrTolerance, Left, Right, AP and ((Leader == was) or comparePriority(P,{Id,Priority})), Leader, Supervisor, Fd); % There is nothing DRY about this
				Sender == Right ->
					Left ! {rg, OriginalSender, self(), P, LeaderBit and ((Leader == was) or comparePriority({Id,Priority},P)), OTTL, TTL-1},
					process(Id, Priority, Tolerance, CurrTolerance, Left, Right, AP and ((Leader == was) or comparePriority(P,{Id,Priority})), Leader, Supervisor, Fd) % WET
				true ->
					io:format("ID=~w Left=~w Right=~w~n", [Id, Left, Right]),
					io:format("Received from ~w: original sender ~w~n", [Sender, OriginalSender])
			end;
		{reply, LeaderBit, OTTL} ->
			if
				LeaderBit and AP ->
					Left ! {rg, self(), self(), {Id, Priority}, LeaderBit, OTTL*2, OTTL*2},
					Right ! {rg, self(), self(), {Id, Priority}, LeaderBit, OTTL*2, OTTL*2},
					process(Id, Priority, Tolerance, CurrTolerance, Left, Right, AP, Leader, Supervisor, Fd);
				true ->
					%io:format("can't be ~w~n", [Id]),
					process(Id, Priority, Tolerance, CurrTolerance, Left, Right, false, Leader, Supervisor, Fd)
			end;
		electLeader ->
			%io:format("Electing ~w~n", [Id]),
			if
				Leader == no ->
					Left ! {rg, self(), self(), {Id, Priority}, true, 1, 1},
					Right ! {rg, self(), self(), {Id, Priority}, true, 1, 1};
				true ->
					%io:format("Leader = ~w~n", [Leader]),
					ok
			end,
			process(Id, Priority, Tolerance, CurrTolerance, Left, Right, true, Leader, Supervisor, Fd);
		{electionEnded, T} ->
			%io:format("ID=~w, T=~w, Tolerance=~w~n", [Id, T, Tolerance]),
			process(Id, Priority, Tolerance, T+Tolerance, Left, Right, AP, Leader, Supervisor, Fd);
		finish ->
			io:format("Finishing node ~w~n", [Id]),
			exit(normal)
	end.

comparePriority({ID1, P1}, {ID2, P2}) ->
	if
		P1 == P2 ->
			ID1 =< ID2;
		true ->
			P1 =< P2
	end.

run(File) ->
	{ok, Fd} = file:open("output.txt", [write]),
	Nodes = [makeNode(Id, Host, Node, Priority, Tolerance, Fd) || {Id, Host, Node, Priority, Tolerance} <- parser:read(File)],
	tieList(Nodes),
	self() ! {election, 0},
	supervisor(Nodes, 0, length(Nodes), Fd).