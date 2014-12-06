%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              BATTLESHIP              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Made by: Daniel Porter        %
%       For: Dr. Kart's COSC3325       %
%        Atificial Intelligence        %
%              Fall 2014               %
%       St. Edward's University        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% require lists:flatten.
usemodule(library(lists), [flatten, nth1, nth0, member]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Entry Point Predicates  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initial configuration entry point into player specific code
initial_configuration(player_dp, InitalConfiguration) :-
	ship_configurations_dp(InitialConfigurations),
	pick_random_dp(InitialConfiguration, InitialConfigurations).

% next moves entry point into player specific code
next_moves(player_dp, InitialConfiguration, OwnHistory, OpponentHistory, OwnBoard, OpponentBoard, [Move]) :-
    choose_best_move_dp(OpponentBoard, OwnHistory, Move).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Ship Configurations    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ship_configurations_dp([S1, S2, S3]) :- ships_1_dp(S1), ships_2_dp(S2), ships_3_dp(S3).

ships_1_dp([carrier(position(d, 9), position(h, 9)),
			battleship(position(h, 3), position(h, 6)),
			cruiser(position(d, 2), position(f, 2)),
			submarine(position(d, 4), position(f, 4)),
			destroyer(position(a, 6), position(a, 7))]).

ships_2_dp([carrier(position(g, 4), position(g, 8)),
			battleship(position(i, 6), position(i, 9)),
			cruiser(position(c, 10), position(e, 10)),
			submarine(position(b, 6), position(d, 6)),
			destroyer(position(c, 2), position(d, 2))]).

ships_3_dp([carrier(position(c, 4), position(g, 4)),
			battleship(position(h, 5), position(h, 8)),
			cruiser(position(d, 6), position(f, 6)),
			submarine(position(b, 5), position(b, 7)),
			destroyer(position(h, 2), position(h, 3))]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Game Code         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choose_best_move_dp(Board, TurnHistory, Move) :-
	adjacent_to_hits_fireable_dp(Board, AdjacentToHits),
	\+ length(AdjacentToHits, 0),
	pick_random_dp(Move, AdjacentToHits), !.

% add case for hits in a line

% choose_
choose_best_move_dp(Board, TurnHistory, Move) :-
	score_board_dp(Board, ScoredBoard, TurnHistory),
	get_best_positions_dp(ScoredBoard, BestPositions),
	pick_random_dp(Move, BestPositions).

adjacent_to_hits_fireable_dp(Board, AdjacentToHits) :-
	adjacent_to_hits_with_hits_dp(Board, AdjacentsWithHits),
	remove_hits_and_misses_dp(AdjacentsWithHits, Board, AdjacentToHits).

% look for open hits
adjacent_to_hits_with_hits_dp(Board, AdjacentToHits) :-
	gather_all_hits_dp(Board, HitPositions),
	adjacents_to_dp(HitPositions, AdjacentToHits).

adjacents_to_dp([], []).
adjacents_to_dp([Position | MorePositions], Adjacents) :-
	get_adjacents_dp(Position, AdjacentToPosition),
	adjacents_to_dp(MorePositions, MoreAdjacents),
	append(AdjacentToPosition, MoreAdjacents, AdjacentsPossibleDuplicates),
	list_to_set(AdjacentsPossibleDuplicates, Adjacents).

gather_all_hits_dp(Board, HitPositions) :-
	flatten(Board, FlatBoard),
	gather_all_hits_dp(FlatBoard, position(a, 1), [], HitPositions).

gather_all_hits_dp([hit], position(j, 10), HitPositions, [position(j, 10) | HitPositions]) :- !.
gather_all_hits_dp(_, position(j, 10), HitPositions, HitPositions) :- !.

gather_all_hits_dp([hit | RestOfFlatBoard], Position, PreviousHitPositions, HitPositions) :-
	increment_position_dp(Position, NextPosition),
	gather_all_hits_dp(RestOfFlatBoard, NextPosition, [Position | PreviousHitPositions], HitPositions), !.
gather_all_hits_dp([_ | RestOfFlatBoard], Position, PreviousHitPositions, HitPositions) :-
	increment_position_dp(Position, NextPosition),
	gather_all_hits_dp(RestOfFlatBoard, NextPosition, PreviousHitPositions, HitPositions), !.


remove_hits_and_misses_dp([], _, []) :- !.
remove_hits_and_misses_dp([position(RowLetter, ColumnNumber) | WithHits], Board, [position(RowLetter, ColumnNumber) | WithoutHits]) :-
	board_get_value(Board, RowLetter, ColumnNumber, unknown),
	remove_hits_and_misses_dp(WithHits, Board, WithoutHits), !.
remove_hits_and_misses_dp([_ | WithHits], Board, WithoutHits) :-
	remove_hits_and_misses_dp(WithHits, Board, WithoutHits), !.

% score_board_dp([Row | RestOfRows], Row, [ScoredRow | RestOfScoredRows]).
score_board_dp(Board, ScoredBoard, TurnHistory) :-
	score_board_dp(Board, a, ScoredBoard, Board, TurnHistory), !.

score_board_dp([LastRow], j, [ScoredRow], Board, TurnHistory) :-
	score_row_dp(LastRow, j, ScoredRow, Board, TurnHistory), !.

score_board_dp([Row | RestOfRows], RowLetter, [ScoredRow | RestOfScoredRows], Board, TurnHistory) :-
	score_row_dp(Row, RowLetter, ScoredRow, Board, TurnHistory),
	increment(RowLetter, NextRow),
	score_board_dp(RestOfRows, NextRow, RestOfScoredRows, Board, TurnHistory), !.

% score_row_dp(UnscoredRow, RowLetter, ScoredRow, Board).
score_row_dp(UnscoredRow, RowLetter, ScoredRow, Board, TurnHistory) :-
	score_row_dp(UnscoredRow, RowLetter, 1, ScoredRow, Board, TurnHistory), !.

score_row_dp([Element], RowLetter, 10, [Score], Board, TurnHistory) :-
	get_score_dp(Board, TurnHistory, RowLetter, 10, Score), !.

score_row_dp([Element | RestOfRow], RowLetter, ColumnNumber, [Score | RestOfScores], Board, TurnHistory) :-
	get_score_dp(Board, TurnHistory, RowLetter, ColumnNumber, Score), NextColumnNumber is ColumnNumber+1,
	score_row_dp(RestOfRow, RowLetter, NextColumnNumber, RestOfScores, Board, TurnHistory), !.


% get_score_dp(Board, RowLetter, ColumnNumber, Score).
get_score_dp(Board, TurnHistory, RowLetter, ColumnNumber, -1) :-
	member(turn(position(RowLetter, ColumnNumber), _, _), TurnHistory), !.
get_score_dp(Board, TurnHistory, RowLetter, ColumnNumber, Score) :- (
	((\+ member(turn(_, _, carrier), TurnHistory), get_5_ship_score_dp(Board, RowLetter, ColumnNumber, CarrierPoints)); CarrierPoints is 0),
	((\+ member(turn(_, _, battleship), TurnHistory), get_4_ship_score_dp(Board, RowLetter, ColumnNumber, BattleshipPoints)); BattleshipPoints is 0),
	((\+ member(turn(_, _, submarine), TurnHistory), get_3_ship_score_dp(Board, RowLetter, ColumnNumber, SubmarinePoints)); SubmarinePoints is 0),
	((\+ member(turn(_, _, cruiser), TurnHistory), get_3_ship_score_dp(Board, RowLetter, ColumnNumber, CruiserPoints)); CruiserPoints is 0),
	((\+ member(turn(_, _, destroyer), TurnHistory), get_2_ship_score_dp(Board, RowLetter, ColumnNumber, DestroyerPoints)); DestroyerPoints is 0)),
	Score is CarrierPoints + BattleshipPoints + SubmarinePoints + CruiserPoints + DestroyerPoints, !.


get_best_positions_dp(Scores, BestPositions) :-
	flatten(Scores, FlattenedScores),
	get_best_positions_dp(FlattenedScores, position(a, 1), 0, _BestScore, [], BestPositions), !.

% base cases (j, 10)
% This is the best score. Replace best positions.
get_best_positions_dp([LastScore], position(j, 10), PreviousBestScore, LastScore, _PreviousBestPositions, [position(j, 10)]) :-
	LastScore > PreviousBestScore, !.
% This is equal to the best score. Add it to the best positions.
get_best_positions_dp([LastScore], position(j, 10), LastScore, LastScore, PreviousBestPositions, [position(j, 10) | PreviousBestPositions]) :- !.
% Worse than best score.
get_best_positions_dp([LastScore], position(j, 10), BestScore, BestScore, BestPositions, BestPositions) :- !.

% recursive cases
% This space is better than the previous best score. It is the only best position.
get_best_positions_dp([Score | RestOfScores], Position, PreviousBestScore, BestScore, _PreviousBestPositions, BestPositions) :-
	Score > PreviousBestScore, increment_position_dp(Position, NextPosition),
	get_best_positions_dp(RestOfScores, NextPosition, Score, BestScore, [Position], BestPositions), !.

% This space is tied for best score. Add it to the list of positions.
get_best_positions_dp([Score | RestOfScores], Position, Score, BestScore, PreviousBestPositions, BestPositions) :-
	increment_position_dp(Position, NextPosition),
	get_best_positions_dp(RestOfScores, NextPosition, Score, BestScore, [Position | PreviousBestPositions], BestPositions), !.

% This space is worse than the best score. Move on.
get_best_positions_dp([Score | RestOfScores], Position, PreviousBestScore, BestScore, PreviousBestPositions, BestPositions) :-
	Score < PreviousBestScore, increment_position_dp(Position, NextPosition),
	get_best_positions_dp(RestOfScores, NextPosition, PreviousBestScore, BestScore, PreviousBestPositions, BestPositions), !.


get_5_ship_score_dp(Board, RowLetter, ColumnNumber, Score) :-
	aggregate_all(count, can_contain_5_ship_dp(Board, RowLetter, ColumnNumber), Score).

get_4_ship_score_dp(Board, RowLetter, ColumnNumber, Score) :-
	aggregate_all(count, can_contain_4_ship_dp(Board, RowLetter, ColumnNumber), Score).

get_3_ship_score_dp(Board, RowLetter, ColumnNumber, Score) :-
	aggregate_all(count, can_contain_3_ship_dp(Board, RowLetter, ColumnNumber), Score).

get_2_ship_score_dp(Board, RowLetter, ColumnNumber, Score) :-
	aggregate_all(count, can_contain_2_ship_dp(Board, RowLetter, ColumnNumber), Score).

can_contain_5_ship_dp(Board, RowLetter, ColumnNumber) :-
	has_4_adj_north_dp(Board, RowLetter, ColumnNumber);
	has_4_adj_east_dp(Board, RowLetter, ColumnNumber);
	has_4_adj_south_dp(Board, RowLetter, ColumnNumber);
	has_4_adj_west_dp(Board, RowLetter, ColumnNumber);
	(has_3_adj_north_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_south_dp(Board, RowLetter, ColumnNumber));
	(has_3_adj_east_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_west_dp(Board, RowLetter, ColumnNumber));
	(has_3_adj_south_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_north_dp(Board, RowLetter, ColumnNumber));
	(has_3_adj_west_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_east_dp(Board, RowLetter, ColumnNumber));
	(has_2_adj_north_dp(Board, RowLetter, ColumnNumber),
		has_2_adj_south_dp(Board, RowLetter, ColumnNumber));
	(has_2_adj_east_dp(Board, RowLetter, ColumnNumber),
		has_2_adj_west_dp(Board, RowLetter, ColumnNumber));
	(has_2_adj_south_dp(Board, RowLetter, ColumnNumber),
		has_2_adj_north_dp(Board, RowLetter, ColumnNumber));
	(has_2_adj_west_dp(Board, RowLetter, ColumnNumber),
		has_2_adj_east_dp(Board, RowLetter, ColumnNumber)).

can_contain_4_ship_dp(Board, RowLetter, ColumnNumber) :-
	has_3_adj_north_dp(Board, RowLetter, ColumnNumber);
	has_3_adj_east_dp(Board, RowLetter, ColumnNumber);
	has_3_adj_south_dp(Board, RowLetter, ColumnNumber);
	has_3_adj_west_dp(Board, RowLetter, ColumnNumber);
	(has_2_adj_north_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_south_dp(Board, RowLetter, ColumnNumber));
	(has_2_adj_east_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_west_dp(Board, RowLetter, ColumnNumber));
	(has_2_adj_south_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_north_dp(Board, RowLetter, ColumnNumber));
	(has_2_adj_west_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_east_dp(Board, RowLetter, ColumnNumber)).

can_contain_3_ship_dp(Board, RowLetter, ColumnNumber) :-
	has_2_adj_north_dp(Board, RowLetter, ColumnNumber);
	has_2_adj_east_dp(Board, RowLetter, ColumnNumber);
	has_2_adj_south_dp(Board, RowLetter, ColumnNumber);
	has_2_adj_west_dp(Board, RowLetter, ColumnNumber);
	(has_1_adj_north_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_south_dp(Board, RowLetter, ColumnNumber));
	(has_1_adj_east_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_west_dp(Board, RowLetter, ColumnNumber));
	(has_1_adj_south_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_north_dp(Board, RowLetter, ColumnNumber));
	(has_1_adj_west_dp(Board, RowLetter, ColumnNumber),
		has_1_adj_east_dp(Board, RowLetter, ColumnNumber)).

can_contain_2_ship_dp(Board, RowLetter, ColumnNumber) :-
	has_1_adj_north_dp(Board, RowLetter, ColumnNumber);
	has_1_adj_east_dp(Board, RowLetter, ColumnNumber);
	has_1_adj_south_dp(Board, RowLetter, ColumnNumber);
	has_1_adj_west_dp(Board, RowLetter, ColumnNumber).



% has_4_adj_<direction>(Board, RowLetter, ColumnNumber).
has_4_adj_north_dp(Board, RowLetter, ColumnNumber) :-
	count_north_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 4.
has_4_adj_east_dp(Board, RowLetter, ColumnNumber) :-
	count_east_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 4.
has_4_adj_south_dp(Board, RowLetter, ColumnNumber) :-
	count_south_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 4.
has_4_adj_west_dp(Board, RowLetter, ColumnNumber) :-
	count_west_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 4.

% has_3_adj_<direction>(Board, RowLetter, ColumnNumber).
has_3_adj_north_dp(Board, RowLetter, ColumnNumber) :-
	count_north_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 3.
has_3_adj_east_dp(Board, RowLetter, ColumnNumber) :-
	count_east_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 3.
has_3_adj_south_dp(Board, RowLetter, ColumnNumber) :-
	count_south_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 3.
has_3_adj_west_dp(Board, RowLetter, ColumnNumber) :-
	count_west_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 3.

% has_2_adj_<direction>(Board, RowLetter, ColumnNumber).
has_2_adj_north_dp(Board, RowLetter, ColumnNumber) :-
	count_north_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 2.
has_2_adj_east_dp(Board, RowLetter, ColumnNumber) :-
	count_east_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 2.
has_2_adj_south_dp(Board, RowLetter, ColumnNumber) :-
	count_south_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 2.
has_2_adj_west_dp(Board, RowLetter, ColumnNumber) :-
	count_west_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 2.

% has_1_adj_<direction>(Board, RowLetter, ColumnNumber).
has_1_adj_north_dp(Board, RowLetter, ColumnNumber) :-
	count_north_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 1.
has_1_adj_east_dp(Board, RowLetter, ColumnNumber) :-
	count_east_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 1.
has_1_adj_south_dp(Board, RowLetter, ColumnNumber) :-
	count_south_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 1.
has_1_adj_west_dp(Board, RowLetter, ColumnNumber) :-
	count_west_of_dp(Board, RowLetter, ColumnNumber, Count),
	Count >= 1.

% count_<direction>_of_dp(Board, Row , Column, Count) counts the number of spaces between (Row, Column) and an unplayable square.
count_north_of_dp(Board, RowLetter, ColumnNumber, Count) :-
	increment(NextRowLetter, RowLetter),
	board_get_value(Board, NextRowLetter, ColumnNumber, unknown),
	count_north_of_dp(Board, NextRowLetter, ColumnNumber, NextCount),
	Count is NextCount+1, !.
count_north_of_dp(Board, RowLetter, ColumnNumber, 0) :- !.


count_east_of_dp(Board,RowLetter, ColumnNumber, Count) :-
	increment(ColumnNumber, NextColumnNumber),
	board_get_value(Board, RowLetter, NextColumnNumber, unknown),
	count_east_of_dp(Board, RowLetter, NextColumnNumber, NextCount),
	Count is NextCount+1, !.
count_east_of_dp(Board, RowLetter, ColumnNumber, 0) :- !.

count_south_of_dp(Board, RowLetter, ColumnNumber, Count) :-
	increment(RowLetter, NextRowLetter),
	board_get_value(Board, NextRowLetter, ColumnNumber, unknown),
	count_south_of_dp(Board, NextRowLetter, ColumnNumber, NextCount),
    Count is NextCount+1, !.
count_south_of_dp(Board, RowLetter, ColumnNumber, 0) :- !.

count_west_of_dp(Board,RowLetter, ColumnNumber, Count) :-
	increment(NextColumnNumber, ColumnNumber),
	board_get_value(Board, RowLetter, NextColumnNumber, unknown),
	count_west_of_dp(Board,RowLetter, NextColumnNumber, NextCount),
    Count is NextCount+1, !.
count_west_of_dp(Board, RowLetter, ColumnNumber, 0) :- !.

% INCREMENT POSITION
% If column is 10 and row is not 10, increment the row.
increment_position_dp(position(R1, 10), position(R2, 1)) :- increment(R1,R2), !.
% Go to next column in same row.
increment_position_dp(position(R1, C1), position(R1, C2)) :- C2 is C1+1, !.

% get_adjacents_dp(Position, AdjacentPositions).
get_adjacents_dp(Position, AdjacentPositions) :-
	aggregate(bag(AdjacentPosition), get_adjacent_single_dp(Position, AdjacentPosition), AdjacentPositions).

% north adj
get_adjacent_single_dp(position(RowLetter, ColumnNumber), position(AdjacentRowLetter, ColumnNumber)) :-
	RowLetter \= a,
	increment(AdjacentRowLetter, RowLetter).
% east adj
get_adjacent_single_dp(position(RowLetter, ColumnNumber), position(RowLetter, AdjacentColumnNumber)) :-
	ColumnNumber < 10,
	AdjacentColumnNumber is ColumnNumber+1.
% south adj
get_adjacent_single_dp(position(RowLetter, ColumnNumber), position(AdjacentRowLetter, ColumnNumber)) :-
	RowLetter \= j,
	increment(RowLetter, AdjacentRowLetter).
% west adj
get_adjacent_single_dp(position(RowLetter, ColumnNumber), position(RowLetter, AdjacentColumnNumber)) :-
	ColumnNumber > 1,
	AdjacentColumnNumber is ColumnNumber-1.

% pick_random_move_dp(Positions, Position)
pick_random_dp(Element, List) :-
	length(List, Length),
	random(0, Length, Random),
	nth0(Random, List, Element), !.


% BOARD AND ROW PRINTING
print_board_dp([]) :- nl, !.
print_board_dp([Row | RestOfBoard]) :- print_row_dp(Row), print_board_dp(RestOfBoard), !.

print_row_dp([E]) :- print_board_element_dp(E), nl, !.
print_row_dp([E | RestOfRow]) :- print_board_element_dp(E), print_row_dp(RestOfRow), !.

print_board_element_dp(hit) :- write('H'), write(' '), !.
print_board_element_dp(miss) :- write('M'), write(' '), !.
print_board_element_dp(unknown) :- write('U'), write(' '), !.
print_board_element_dp(E) :- E < 10, E >= 0, write(0), write(E), write(' '), !.
print_board_element_dp(E) :- write(E), write(' '), !.
