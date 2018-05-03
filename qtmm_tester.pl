:-consult('quantum_three_mens_morris.pl').


% qtmm_advance/1
qtmm_advance(true):- write('.. '), !.
qtmm_advance(false).

% success/2
qtmm_success(N, true):- format('Cerința ~d a fost rezolvată!~n', [N]), !.
qtmm_success(_, false).

% qtmm_fail/2
qtmm_fail(N, true):-
    format('Cerința ~d NU este rezolvată corect!~n', [N]), fail.

% eval/2
qtmm_eval(Goal, Verbose):- call(Goal), qtmm_advance(Verbose).

% qtmm_eval_all/2
qtmm_eval_all(N, Verbose):-
    \+ (qtmm_test(N, Goal), \+ qtmm_eval(Goal, Verbose)).

% check_N/2
% check_N(+N, +DependsOn)
qtmm_check_N(N, Verbose):-
    (var(Verbose) -> Verbose = true ; true),
    qtmm_depends_on(N, DependsOn),
    \+ (member(D, DependsOn),
	\+ qtmm_check_N(D, false),
	format('Rezolvă întâi cerința ~d!~n', [D])
       ),
    ( qtmm_eval_all(N, Verbose) ->
      qtmm_success(N, Verbose) ;
      qtmm_fail(N, Verbose) ).


check_1:- qtmm_check_N(1, true).
check_2:- qtmm_check_N(2, true).
check_3:- qtmm_check_N(3, true).
check_4:- qtmm_check_N(4, true).
check_5:- qtmm_check_N(5, true).
check_6:- qtmm_check_N(6, true).
check_7:- qtmm_check_N(7, true).
check_8(P):-
     qtmm_depends_on(8, DependsOn),
     \+ (member(D, DependsOn),
         \+ qtmm_check_N(D, false),
         format('Rezolvă întâi cerința ~d!~n', [D])
        ),
     qtmm_eval_all(8, true),
     score_against_random((smart_place, smart_measure, smart_move), W),
     P1 is (W - 0.6) * 2 / 0.2,
     ( P1 < 0 -> P = 0 ; (P1 > 2 -> P = 2 ; P1 = P) ),
     format(' Smart wins: ~f%. Punctaj: ~f.~n ', [W, P]),
     W > 0.8.

check_9(P):-
     qtmm_depends_on(9, DependsOn),
     \+ (member(D, DependsOn),
         \+ qtmm_check_N(D, false),
         format('Rezolvă întâi cerința ~d!~n', [D])
        ),
     qtmm_eval_all(9, true),
     score_against_random((bonus_place, bonus_measure, bonus_move), W),
     P1 is (W - 0.8) * 2 / 0.15,
     ( P1 < 0 -> P = 0 ; (P1 > 2 -> P = 2 ; P1 = P) ),
     format(' Smart wins: ~f%. Punctaj: ~f.~n ', [W, P]),
     W > 0.8.


check_I:-
    ( check_1 -> S1 = 0.5; S1 is 0.0 ),
    ( check_2 -> S2 = 0.5; S2 is 0.0 ),
    ( check_3 -> S3 = 1.0; S3 is 0.0 ),
    ( check_4 -> S4 = 1.0; S4 is 0.0 ),
    ( check_5 -> S5 = 1.0; S5 is 0.0 ),
    ( check_6 -> S6 = 2.0; S6 is 0.0 ),
    ( check_7 -> S7 = 2.0; S7 is 0.0 ),
    Score is S1 + S2 + S3 + S4 + S5 + S6 + S7,
    format('Punctajul pentru partea întâi este: ~f!~n', [Score]).

check_II:-
    ( check_8(S8) -> true ; S8 is 0.0 ),
    format('Punctajul pentru partea a doua este: ~f!~n', [S8]).

check_BONUS:-
    ( check_9(S9) -> true; S9 is 0.0 ),
    format('Punctajul pentru BONUS este: ~f!~n', [S9]).

qtmm_depends_on(1, []).
qtmm_depends_on(2, [1]).
qtmm_depends_on(3, [2]).
qtmm_depends_on(4, [2]).
qtmm_depends_on(5, [2]).
qtmm_depends_on(6, [2]).
qtmm_depends_on(7, [2]).
qtmm_depends_on(8, [1, 2, 3, 4, 5, 6, 7]).
qtmm_depends_on(9, [1, 2, 3, 4, 5, 6, 7]).


qtmm_test(1, current_predicate(next_player/2)).
qtmm_test(1, (next_player(white, black), next_player(black, white))).
qtmm_test(1, findall(_, next_player(_,_), [_,_])).
qtmm_test(1, findall(_, next_player(white, _), [_])).
qtmm_test(1, findall(_, next_player(black, _), [_])).
qtmm_test(1, findall(_, next_player(_, white), [_])).
qtmm_test(1, findall(_, next_player(_, black), [_])).

qtmm_test(2, current_predicate(cell/1)).
qtmm_test(2, findall(_, cell(_), [_,_,_,_,_,_,_,_,_])).
qtmm_test(2, findall(_, (cell(X), var(X)), [])).
qtmm_test(2, findall(_, (cell(pos(X,Y)), (var(X);var(Y))), [])).
qtmm_test(2, findall(_, (cell(pos(1,Y)), var(Y)), [])).
qtmm_test(2, findall(_, (cell(pos(X,Y)), between(1, 3, Y), between(1, 3, X)),
		     [_,_,_,_,_,_,_,_,_])).
qtmm_test(2, findall(_, (between(1, 3, Y), between(1, 3, X), cell(pos(X,Y))),
		     [_,_,_,_,_,_,_,_,_])).

qtmm_test(3, current_predicate(valid_pairs/3)).
qtmm_test(3, (test_state_2(S), findall(_, valid_pairs(S, white, _), [_]))).
qtmm_test(3, (test_state_3(S), findall(_, valid_pairs(S, black, _), [_]))).
qtmm_test(3, (test_state_1(S), valid_pairs(S, white, Ps), length(Ps, 15))).
qtmm_test(3, (test_state_1(S), valid_pairs(S, black, Ps), length(Ps, 15))).
qtmm_test(3, (test_state_2(S), valid_pairs(S, white, Ps), length(Ps, 15))).
qtmm_test(3, (test_state_2(S), valid_pairs(S, black, Ps), length(Ps, 14))).
qtmm_test(3, (test_state_3(S), valid_pairs(S, white, Ps), length(Ps, 19))).
qtmm_test(3, (test_state_3(S), valid_pairs(S, black, Ps), length(Ps, 20))).
qtmm_test(3, (test_state_3(S), valid_pairs(S, black, Ps),
	      valid_pairs_black_3(Qs),
	      \+ (member(Q, Qs), \+ (member(P, Ps), P == Q)))).
qtmm_test(3, (test_state_3(S), valid_pairs(S, white, Ps),
	      valid_pairs_white_3(Qs),
	      \+ (member(Q, Qs), \+ (member(P, Ps), P == Q)))).

qtmm_test(4, current_predicate(valid_moves/3)).
qtmm_test(4, (test_state_4(S), findall(_, valid_moves(S, white, _), [_]))).
qtmm_test(4, (test_state_5(S), findall(_, valid_moves(S, black, _), [_]))).
qtmm_test(4, (test_state_4(S), valid_moves(S, white, Ps), length(Ps,39))).
qtmm_test(4, (test_state_4(S), valid_moves(S, white, Ps),
	      valid_moves_white_4(Qs),
	      \+ (member(Q, Qs), \+ (member(P, Ps), P == Q)))).
qtmm_test(4, (test_state_5(S), valid_moves(S, black, Ps), length(Ps,23))).
qtmm_test(4, (test_state_5(S), valid_moves(S, black, Ps),
	      valid_moves_black_5(Qs),
	      \+ (member(Q, Qs), \+ (member(P, Ps), P == Q)))).

qtmm_test(5, current_predicate(winner/2)).
qtmm_test(5, (test_state_1(S), \+ winner(S, _))).
qtmm_test(5, (test_state_2(S), \+ winner(S, _))).
qtmm_test(5, (test_state_3(S), \+ winner(S, _))).
qtmm_test(5, (test_state_4(S), \+ winner(S, _))).
qtmm_test(5, (test_state_5(S), \+ winner(S, _))).
qtmm_test(5, (test_state_6(S), winner(S, [white]))).
qtmm_test(5, (test_state_7(S), winner(S, W),
	      (W == [white, black] ; W == [black, white]))).
qtmm_test(5, (test_state_8(S), \+ winner(S, _))).

qtmm_test(6, current_predicate(has_cycle/1)).
qtmm_test(6, (test_state_1(S), \+ has_cycle(S))).
qtmm_test(6, (test_state_2(S), \+ has_cycle(S))).
qtmm_test(6, (test_state_3(S), \+ has_cycle(S))).
qtmm_test(6, (test_state_4(S), \+ has_cycle(S))).
qtmm_test(6, (test_state_5(S), \+ has_cycle(S))).
qtmm_test(6, (test_state_6(S), \+ has_cycle(S))).
qtmm_test(6, (test_state_7(S), \+ has_cycle(S))).
qtmm_test(6, (test_state_8(S), has_cycle(S))).
qtmm_test(6, (test_state_9(S), has_cycle(S))).

qtmm_test(7, current_predicate(collapse/4)).
qtmm_test(7, (test_state_8(S), test_state_8a(Sa),
	      collapse(S, quantum(pos(1,1), pos(3,1), white), pos(1,1), SC),
	      \+ (member(P, SC), \+ (member(Q, Sa), Q == P)),
	      \+ (member(P, Sa), \+ (member(Q, SC), Q == P)))).
qtmm_test(7, (test_state_8(S), test_state_8b(Sb),
	      collapse(S, quantum(pos(1,1), pos(3,1), white), pos(3,1), SC),
	      \+ (member(P, SC), \+ (member(Q, Sb), Q == P)),
	      \+ (member(P, Sb), \+ (member(Q, SC), Q == P)))).

qtmm_test(7, (test_state_8(S), test_state_8a(Sa),
	      collapse(S, quantum(pos(3,1), pos(1,2), black), pos(3,1), SC),
	      \+ (member(P, SC), \+ (member(Q, Sa), Q == P)),
	      \+ (member(P, Sa), \+ (member(Q, SC), Q == P)))).
qtmm_test(7, (test_state_8(S), test_state_8b(Sb),
	      collapse(S, quantum(pos(3,1), pos(1,2), black), pos(1,2), SC),
	      \+ (member(P, SC), \+ (member(Q, Sb), Q == P)),
	      \+ (member(P, Sb), \+ (member(Q, SC), Q == P)))).

qtmm_test(7, (test_state_9(S), test_state_9a(Sa),
	      collapse(S, quantum(pos(1,1), pos(1,3), black), pos(1,1), SC),
	      \+ (member(P, SC), \+ (member(Q, Sa), Q == P)),
	      \+ (member(P, Sa), \+ (member(Q, SC), Q == P)))).
qtmm_test(7, (test_state_9(S), test_state_9b(Sb),
	      collapse(S, quantum(pos(1,1), pos(1,3),black), pos(1,3), SC),
	      \+ (member(P, SC), \+ (member(Q, Sb), Q == P)),
	      \+ (member(P, Sb), \+ (member(Q, SC), Q == P)))).

qtmm_test(7, (test_state_9(S), test_state_9a(Sa),
	      collapse(S, quantum(pos(1,1), pos(2,1),black), pos(2,1), SC),
	      \+ (member(P, SC), \+ (member(Q, Sa), Q == P)),
	      \+ (member(P, Sa), \+ (member(Q, SC), Q == P)))).
qtmm_test(7, (test_state_9(S), test_state_9b(Sb),
	      collapse(S, quantum(pos(1,1), pos(2,1),black), pos(1,1), SC),
	      \+ (member(P, SC), \+ (member(Q, Sb), Q == P)),
	      \+ (member(P, Sb), \+ (member(Q, SC), Q == P)))).


qtmm_test(8, current_predicate(smart_place/5)).
qtmm_test(8, current_predicate(smart_measure/5)).
qtmm_test(8, current_predicate(smart_move/5)).


qtmm_test(9, current_predicate(bonus_place/5)).
qtmm_test(9, current_predicate(bonus_measure/5)).
qtmm_test(9, current_predicate(bonus_move/5)).


test_state_1([classic(pos(1,1), white),
	      classic(pos(3,3), black),
	      classic(pos(2,2), white)]).

test_state_2([quantum(pos(1,2), pos(3,1), black),
	      classic(pos(1,1), white),
	      classic(pos(3,3), black),
	      classic(pos(2,2), white)]).

test_state_3([quantum(pos(1,2), pos(2,2), white),
	      quantum(pos(2,2), pos(3,2), black),
	      quantum(pos(3,2), pos(3,3), white),
	      classic(pos(1,1), white),
	      classic(pos(3,1), black)]).

test_state_4([quantum(pos(3,2), pos(3,3), white),
	      quantum(pos(2,3), pos(3,3), black),
	      classic(pos(1,1), white),
	      classic(pos(2,2), black),
	      quantum(pos(3,1), pos(3,2), white),
	      quantum(pos(2,1), pos(3,1), black)]).

test_state_5([classic(pos(1,3), white),
	      quantum(pos(2,3), pos(3,3), black),
	      classic(pos(1,1), white),
	      classic(pos(2,2), black),
	      quantum(pos(3,1), pos(3,2), white),
	      quantum(pos(2,1), pos(3,1), black)]).

test_state_6([classic(pos(1,3), white),
	      quantum(pos(2,3), pos(3,3), black),
	      classic(pos(1,1), black),
	      classic(pos(2,2), white),
	      classic(pos(3,1), white),
	      quantum(pos(2,1), pos(1,2), black)]).

test_state_7([classic(pos(1,1), white),
	      classic(pos(1,2), white),
	      classic(pos(3,3), black),
	      classic(pos(1,3), white),
	      classic(pos(3,2), black),
	      classic(pos(3,1), black)]).

test_state_8([quantum(pos(1,1), pos(3,1), white),
	      quantum(pos(1,2), pos(3,2), white),
	      classic(pos(3,3), black),
	      classic(pos(1,3), white),
	      quantum(pos(3,2), pos(1,1), black),
	      quantum(pos(3,1), pos(1,2), black)]).

test_state_8a([classic(pos(1,1), white),
	       classic(pos(1,2), white),
	       classic(pos(3,3), black),
	       classic(pos(1,3), white),
	       classic(pos(3,2), black),
	       classic(pos(3,1), black)]).

test_state_8b([classic(pos(3,1), white),
	       classic(pos(3,2), white),
	       classic(pos(3,3), black),
	       classic(pos(1,3), white),
	       classic(pos(1,1), black),
	       classic(pos(1,2), black)]).


test_state_9([quantum(pos(2,1), pos(1,3),white),
	      quantum(pos(1,1), pos(2,1),black),
	      quantum(pos(2,3), pos(3,3),white),
	      quantum(pos(3,2), pos(3,3),black),
	      quantum(pos(2,1), pos(3,2),white),
	      quantum(pos(1,1), pos(1,3),black)]).

test_state_9a([classic(pos(1,3),white),
	       classic(pos(2,1),black),
	       classic(pos(2,3),white),
	       classic(pos(3,3),black),
	       classic(pos(3,2),white),
	       classic(pos(1,1),black)]).

test_state_9b([classic(pos(2,1),white),
	       classic(pos(1,1),black),
	       classic(pos(2,3),white),
	       classic(pos(3,3),black),
	       classic(pos(3,2),white),
	       classic(pos(1,3),black)]).


valid_pairs_black_3([(pos(1,2),pos(1,3)), (pos(1,2),pos(2,1)),
		     (pos(1,2),pos(2,2)), (pos(1,2),pos(2,3)),
		     (pos(1,2),pos(3,2)), (pos(1,2),pos(3,3)),
		     (pos(1,3),pos(2,1)), (pos(1,3),pos(2,2)),
		     (pos(1,3),pos(2,3)), (pos(1,3),pos(3,2)),
		     (pos(1,3),pos(3,3)), (pos(2,1),pos(2,2)),
		     (pos(2,1),pos(2,3)), (pos(2,1),pos(3,2)),
		     (pos(2,1),pos(3,3)), (pos(2,2),pos(2,3)),
		     (pos(2,2),pos(3,3)), (pos(2,3),pos(3,2)),
		     (pos(2,3),pos(3,3)), (pos(3,2),pos(3,3))]).

valid_pairs_white_3([(pos(1,2),pos(1,3)), (pos(1,2),pos(2,1)),
		     (pos(1,2),pos(2,3)), (pos(1,2),pos(3,2)),
		     (pos(1,2),pos(3,3)), (pos(1,3),pos(2,1)),
		     (pos(1,3),pos(2,2)), (pos(1,3),pos(2,3)),
		     (pos(1,3),pos(3,2)), (pos(1,3),pos(3,3)),
		     (pos(2,1),pos(2,2)), (pos(2,1),pos(2,3)),
		     (pos(2,1),pos(3,2)), (pos(2,1),pos(3,3)),
		     (pos(2,2),pos(2,3)), (pos(2,2),pos(3,2)),
		     (pos(2,2),pos(3,3)), (pos(2,3),pos(3,2)),
		     (pos(2,3),pos(3,3))]).

valid_moves_white_4(
    [move(quantum(pos(3,2),pos(3,3),white),quantum(pos(1,2),pos(1,3),white)),
     move(quantum(pos(3,2),pos(3,3),white),quantum(pos(1,2),pos(2,1),white)),
     move(quantum(pos(3,2),pos(3,3),white),quantum(pos(1,2),pos(2,3),white)),
     move(quantum(pos(3,2),pos(3,3),white),quantum(pos(1,2),pos(3,1),white)),
     move(quantum(pos(3,2),pos(3,3),white),quantum(pos(1,3),pos(2,1),white)),
     move(quantum(pos(3,2),pos(3,3),white),quantum(pos(1,3),pos(2,3),white)),
     move(quantum(pos(3,2),pos(3,3),white),quantum(pos(1,3),pos(3,1),white)),
     move(quantum(pos(3,2),pos(3,3),white),quantum(pos(2,1),pos(2,3),white)),
     move(quantum(pos(3,2),pos(3,3),white),quantum(pos(2,1),pos(3,1),white)),
     move(quantum(pos(3,2),pos(3,3),white),quantum(pos(2,3),pos(3,1),white)),
     move(classic(pos(1,1),white),quantum(pos(1,2),pos(1,3),white)),
     move(classic(pos(1,1),white),quantum(pos(1,2),pos(2,1),white)),
     move(classic(pos(1,1),white),quantum(pos(1,2),pos(2,3),white)),
     move(classic(pos(1,1),white),quantum(pos(1,2),pos(3,1),white)),
     move(classic(pos(1,1),white),quantum(pos(1,2),pos(3,2),white)),
     move(classic(pos(1,1),white),quantum(pos(1,2),pos(3,3),white)),
     move(classic(pos(1,1),white),quantum(pos(1,3),pos(2,1),white)),
     move(classic(pos(1,1),white),quantum(pos(1,3),pos(2,3),white)),
     move(classic(pos(1,1),white),quantum(pos(1,3),pos(3,1),white)),
     move(classic(pos(1,1),white),quantum(pos(1,3),pos(3,2),white)),
     move(classic(pos(1,1),white),quantum(pos(1,3),pos(3,3),white)),
     move(classic(pos(1,1),white),quantum(pos(2,1),pos(2,3),white)),
     move(classic(pos(1,1),white),quantum(pos(2,1),pos(3,1),white)),
     move(classic(pos(1,1),white),quantum(pos(2,1),pos(3,2),white)),
     move(classic(pos(1,1),white),quantum(pos(2,1),pos(3,3),white)),
     move(classic(pos(1,1),white),quantum(pos(2,3),pos(3,1),white)),
     move(classic(pos(1,1),white),quantum(pos(2,3),pos(3,2),white)),
     move(classic(pos(1,1),white),quantum(pos(2,3),pos(3,3),white)),
     move(classic(pos(1,1),white),quantum(pos(3,1),pos(3,3),white)),
     move(quantum(pos(3,1),pos(3,2),white),quantum(pos(1,2),pos(1,3),white)),
     move(quantum(pos(3,1),pos(3,2),white),quantum(pos(1,2),pos(2,1),white)),
     move(quantum(pos(3,1),pos(3,2),white),quantum(pos(1,2),pos(2,3),white)),
     move(quantum(pos(3,1),pos(3,2),white),quantum(pos(1,2),pos(3,3),white)),
     move(quantum(pos(3,1),pos(3,2),white),quantum(pos(1,3),pos(2,1),white)),
     move(quantum(pos(3,1),pos(3,2),white),quantum(pos(1,3),pos(2,3),white)),
     move(quantum(pos(3,1),pos(3,2),white),quantum(pos(1,3),pos(3,3),white)),
     move(quantum(pos(3,1),pos(3,2),white),quantum(pos(2,1),pos(2,3),white)),
     move(quantum(pos(3,1),pos(3,2),white),quantum(pos(2,1),pos(3,3),white)),
     move(quantum(pos(3,1),pos(3,2),white),quantum(pos(2,3),pos(3,3),white))]).


valid_moves_black_5(
    [move(quantum(pos(2,3),pos(3,3),black),quantum(pos(1,2),pos(2,1),black)),
     move(quantum(pos(2,3),pos(3,3),black),quantum(pos(1,2),pos(3,1),black)),
     move(quantum(pos(2,3),pos(3,3),black),quantum(pos(1,2),pos(3,2),black)),
     move(quantum(pos(2,3),pos(3,3),black),quantum(pos(2,1),pos(3,2),black)),
     move(quantum(pos(2,3),pos(3,3),black),quantum(pos(3,1),pos(3,2),black)),
     move(classic(pos(2,2),black),quantum(pos(1,2),pos(2,1),black)),
     move(classic(pos(2,2),black),quantum(pos(1,2),pos(2,3),black)),
     move(classic(pos(2,2),black),quantum(pos(1,2),pos(3,1),black)),
     move(classic(pos(2,2),black),quantum(pos(1,2),pos(3,2),black)),
     move(classic(pos(2,2),black),quantum(pos(1,2),pos(3,3),black)),
     move(classic(pos(2,2),black),quantum(pos(2,1),pos(2,3),black)),
     move(classic(pos(2,2),black),quantum(pos(2,1),pos(3,2),black)),
     move(classic(pos(2,2),black),quantum(pos(2,1),pos(3,3),black)),
     move(classic(pos(2,2),black),quantum(pos(2,3),pos(3,1),black)),
     move(classic(pos(2,2),black),quantum(pos(2,3),pos(3,2),black)),
     move(classic(pos(2,2),black),quantum(pos(3,1),pos(3,2),black)),
     move(classic(pos(2,2),black),quantum(pos(3,1),pos(3,3),black)),
     move(classic(pos(2,2),black),quantum(pos(3,2),pos(3,3),black)),
     move(quantum(pos(2,1),pos(3,1),black),quantum(pos(1,2),pos(2,3),black)),
     move(quantum(pos(2,1),pos(3,1),black),quantum(pos(1,2),pos(3,2),black)),
     move(quantum(pos(2,1),pos(3,1),black),quantum(pos(1,2),pos(3,3),black)),
     move(quantum(pos(2,1),pos(3,1),black),quantum(pos(2,3),pos(3,2),black)),
     move(quantum(pos(2,1),pos(3,1),black),quantum(pos(3,2),pos(3,3),black))]).
