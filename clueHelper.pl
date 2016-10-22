:- dynamic person/1, weapon/1, room/1, failed_guess/3.

ourList([],5).


%% possible suspects
person(colonel_mustard).
person(mrs_peacock).
person(mrs_scarlet).
person(professor_plum).
person(mr_green).
person(mrs_white).

%% possible murder weapons used
weapon(knife).
weapon(candlestick).
weapon(revolver).
weapon(rope).
weapon(lead_pipe).
weapon(wrench). 

%% possible rooms 
room(hall).
room(lounge).
room(dining_room). 
room(kitchen). 
room(ballroom). 
room(conservatory). 
room(billiard_room).
room(library).
room(study). 


%% people(L) is true if L is the list of all possible suspects
people(L) :- findall(X0, person(X0), L). 

%% weapons(L) is true if L is the list of all possible murder weapons
weapons(L) :- findall(X0, weapon(X0), L). 

%% rooms(L) is true if L is the list of all possible rooms
rooms(L) :- findall(X0, room(X0), L).

possible(P, W, R) :- person(P), weapon(W), room(R), \+ failed_guess(P, W, R). 

possible(a(P, W, R)) :- possible(P, W, R). 

all_possibilities(L) :- findall(X0, possible(X0), L).

%% possibilities(P, W, R, Chance) is true if the chance of P W and R being the answer
%% is at least Chance


%% chance(P, W, R, Chance) is true if person P, weapon W, and Room R have Chance chance
%% of being the correct answer


%% highest_chance(P, W, R) is true if person P, weapon W, and room R have the highest
%% chance of being the correct answer


%% total_possibilities(X) is true if X is the total number of possible combinations
%% that the answer could be

%% failed_guess(P, W, R) is true if person P, weapon W, and room R have been guessed 
%% and proven wrong i.e. one or more of the 3 is not the correct answer



