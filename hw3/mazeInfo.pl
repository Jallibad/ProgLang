:- module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).

info(5, 5, c).


wall(0,4).
wall(1,1).
wall(1,4).
wall(2,1).
wall(2,3).
wall(2,4).
wall(3,1).
wall(4,3).
wall(4,4).


button(0,0,2).
button(0,3,1).

num_buttons(2).

start(3,4).

goal(1,0).
