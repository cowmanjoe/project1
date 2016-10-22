:- dynamic person/1, weapon/1, room/1, cdef/0, abcd/0.

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


people(L) :- person(P), member(P,L), arePeople(T). 

arePeople([]).
arePeople(H|T) :- person(H), arePeople(T). 


possible(P, W, R) :- person(P), weapon(W), room(R), \+ failedGuess(P, W, R). 


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

failed_guess(colonel_mustard, knife, hall). 
failed_guess(mrs_scarlet, wrench, hall). 

