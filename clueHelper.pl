:- dynamic person/1, weapon/1, room/1, failed_guess/3, current_people/1, 
	current_weapons/1, current_rooms/1.

current_people([colonel_mustardk,mrs_peacock, mrs_scarlet, professor_plum, mr_green, mrs_white]).
current_weapons([knife, candlestick, revolver, rope, lead_pipe, wrench]). 
current_rooms([hall, lounge, dining_room, kitchen, ballroom, conservatory, billiard_room, library, study]).

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

%% possible(P, W, R) is true if person P, weapon W and room R could make a 
%% possible answer
possible(P, W, R) :- person(P), weapon(W), room(R), \+ failed_guess(P, W, R). 

%% does the same as possible but organized into a triple
possible(a(P, W, R)) :- possible(P, W, R). 

%% all_possibilities(L) is true when L is the list of all possible answers
all_possibilities(L) :- findall(X0, possible(X0), L).

%% total_possibilities(X) is true if X is the total number of possible combinations
%% that the answer could be
total_possibilities(X) :- all_possibilities(L), length(L, X). 

%% chance(X) is true if X is the percentage chance of any one of the answers left
%% to be true
chance(X) :- total_possibilities(Y), X is 100 / Y.

%% remove_person(X) retracts current_people(L), and then asserts current_people(L2)
%% where L2 is L without X in it 
remove_person(X) :- current_people(L), delete(L, X, L2), retract(current_people(L)), assert(current_people(L2)). 

%% remove_weapon(X) retracts current_weapons(L), and then asserts current_weapons(L2)
%% where L2 is L without X in it
remove_weapon(X) :- current_weapons(L), delete(L, X, L2), retract(current_weapons(L)), assert(current_weapons(L2)). 

%% remove_room(X) retracts current_rooms(L), adn then asserts current_rooms(L2)
%% where L2 is L without X in it
remove_room(X) :- current_rooms(L), delete(L, X, L2), retract(current_rooms(L)), assert(current_rooms(L2)). 

%% possibilities(P, W, R, Chance) is true if the chance of P W and R being the answer
%% is at least Chance


%% chance(P, W, R, Chance) is true if person P, weapon W, and Room R have Chance chance
%% of being the correct answer


%% highest_chance(P, W, R) is true if person P, weapon W, and room R have the highest
%% chance of being the correct answer




%% failed_guess(P, W, R) is true if person P, weapon W, and room R have been guessed 
%% and proven wrong i.e. one or more of the 3 is not the correct answer



