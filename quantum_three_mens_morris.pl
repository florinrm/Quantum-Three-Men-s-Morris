% Quantum Three Men's Morris

% Se propune o variantă cuantică a jocului "Three Men's Morris", joc
% în care doi adversari plasează și apoi mută câte trei piese pe o
% tablă cu 3x3 celule. Un jucător va avea trei piese albe, iar cel
% de-al doilea trei piese negre. Scopul fiecăruia este de a-și aranja
% piesele pe aceeași linie, pe aceeași coloană sau pe aceeași
% diagonală.
%
%  (1,1) -- (1,2) -- (1,3)
%    |    \   |    /   |
%    |     \  |   /    |
%  (2,1) -- (2,2) -- (2,3)
%    |     /  |   \    |
%    |    /   |    \   |
%  (3,1) -- (3,2) -- (3,3)
%
% Pe tablă sunt 9 celule.
%
% Jocul are două etape:
%
%  i. Plasarea pieselor
%
%     Alternativ, fiecare jucător va plasa câte o piesă în stare
%     cuantică pe tablă. Asta presupune alegerea a două celule în care
%     NU se află o piesă în stare clasică. Cele două celule vor deveni
%     legate la nivel cuantic (eng. entangled).
%
%     Atunci când se construiește un ciclu de celule legate cuantic
%     (entangled) jucătorul următor (nu cel care a creat ciclul) va
%     "măsura" ("observa") poziția ultimei piese plasate pe tablă (cea
%     care închis ciclul) și va alege în care dintre cele două celule
%     va rămâne aceasta. Observarea unei poziții duce la colapsarea
%     întregii componente a grafului din care face parte ciclul.
%
%     Etapa de plasare a pieselor se va termina atunci când fiecare
%     dintre cei doi jucători are câte trei piese indiferent în ce
%     stare.  (Se poate produce un ciclu în această etapă sau nu.)
%
% ii. Mutarea pieselor
%
%     Alternativ, fiecare jucător alege o piesă pe care să o mute
%     într-o celulă liberă (în care nu se află o piesă în stare
%     clasică). Dacă piesa se află în stare cuantică, atunci ambele
%     celule posibile se vor schimba. Dacă piesa se alfă în stare
%     clasică, atunci se va indica o pereche de celule vecine, iar
%     piesa va ajunge într-o stare cuantică. Efectul unei mutări lasă
%     piesa mutată în stare cuantică, iar cele două celule posibile
%     sunt, desigur, legate la nivel cuantic.
%
%     Atunci când se construiește un ciclu de celule legate cuantic
%     jucătorul următor (nu cel care a creat ciclul) va "măsura"
%     poziția ultimei piese mutate (cea care a închis ciclul) și va
%     alege în care dintre cele două celule posibile va rămâne
%     aceasta. Observarea unei poziții poate duce la observarea
%     pozițiilor mai multor piese.
%
%     Jocul se încheie atunci când cel puțin unul dintre jucători are
%     trei piese în stare clasică pe aceeași linie, coloană sau
%     diagonală.
%
% Reprezentări folosite:
%
%  - O celulă va fi reprezentată printr-un tuplu pos(X,Y) unde X,Y
%    sunt 1, 2 sau 3.
%
%  - O piesă va fi reprezentată diferit în funcție de starea ei:
%     classic/2   e.g.  classic(pos(2,2), white)
%     quantum/3   e.g.  quantum(pos(1,2), pos(3,1), black)
%
%  - O stare va fi o listă de piese (maximum șase)
%     e.g.: [classic(pos(2,2), white), classic(pos(1,5), black),
%            quantum(pos(1,3), pos(2,3), white)]

% ----------------------------------------------------------------------

% Rezolvați pe rând cerințele de mai jos!

% [Cerința 1] Să se scrie predicatul next_player/2 care descrie
% alternanța culorilor jucătorilor. black îi urmează lui white, iar
% white îi urmează lui black.

% next_player/2
% next_player(?Color1, ?Color2)


% ----------------------------------------------------------------------

% [Cerința 2] Scrieți un predicat cell(?Cell) care să fie adevărat
% pentru atunci când Cell este o structură pos(X,Y) reprezentând o
% celulă de pe tablă.

% cell/1
% cell(?Cell)


% ----------------------------------------------------------------------

% [Cerința 3] Scrieți un predicat valid_pairs(+State, +Color, -Pairs)
% care să descrie legătura dintre starea State și toate perechile de
% celule în care jucătorul Color ar putea așeza o piesă în stare
% cuantică. Celulele ocupate de piese în stare clasică nu reprezintă
% soluții valide. De asemenea, nici perechile de celule deja legate
% cuantic de o piesă a aceluiași jucător nu reprezintă perchi valide.
% Lista Pairs trebuie să NU conțină și o pereche și inversa ei.


% valid_pairs/3
% valid_pairs(+State, +Color, -Pairs)



% ----------------------------------------------------------------------

% Cerința 4. Scrieți un predicat valid_moves(+State, +Color, -Moves)
% care leagă variabila Moves la lista tuturor mutărilor pe care le
% poate face jucătorul Color. O mutare poate fi de două feluri:
%
%  move(classic((1,2), white), quantum((1,3), (2,1), white))
%     sau
%  move(quantum((3,3), (1,1), white), quantum((1,3), (2,1), white))


% valid_moves/3
% valid_moves(+State, +Color, -Moves)


% ----------------------------------------------------------------------

% Cerința 5. Scrieți un predicat winner(+State, -Winner) care produce
% legarea variabilei Winner la lista jucătorilor care au cele trei
% piese în stare clasică aliniate. Dacă nimeni nu a câștigat, winner
% eșuează (nu leagă Winner la lista vidă).


% winner/2
% winner(+State, -Colors)



% ----------------------------------------------------------------------

% Cerința 6. Se cere scrierea unui predicat has_cycle(+State) care să
% fie adevărat dacă starea repsectivă conține un ciclu de celule
% legate cuantic.
%
% has_cycle([quantum(pos(1,1), pos(3,2), white),
%            quantum((2,1), (3,2), black),
%            quantum(pos(1,1), pos(2,1), white)])
%   => true.
%
% has_cycle([quantum(pos(1,1), pos(3,2), black),
%            quantum(pos(3,1), pos(3,2), white)])
%   => false.

% has_cycle/1
% has_cycle(+State)


% ----------------------------------------------------------------------

% Cerința 7. Se cere scrierea unui predicat collapse(+State, +Piece,
% +Cell, -NewState) care să producă starea obținută prin "măsurarea"
% piesei Piece în celula Cell. Starea NewState este rezulatul
% colapsării stării State. Piece este garantat un membru al lui State.

% collapse/4
% collapse(+State, +Piece, +Cell, -NewState)


% ----------------------------------------------------------------------
% ----------------------------------------------------------------------


% Un jucător trebuie să definească trei strategii:
%
%   - alegerea unei perechi de celule neocupate în care să plaseze
%     următoarea piesă (în etapa de plasare a pieselor)
%
%        place(+State, +Color, +Step, +ValidPairs, -Pair)
%
%   - alegerea unei mutări
%
%        move(+State, +Color, +Step, +ValidMoves, -Move)
%
%   - observarea unei piese într-una din cele două poziții posibile
%
%        measure(+State, +Color, +Step, +Piece, -Cell)
%
%   În toate cele trei cazuri, State reprezintă starea curentă a
%   jocului, Color culoarea jucătorului curent, iar Step numărul
%   mutării (important deoarece jocul se termină după maximum 50 de
%   mutări).
%
%
% Mai jos este descris un jucător cu strategii aleatoare.

rand_place(_State, _Color, _Step, ValidPairs, (Cell1, Cell2)):-
    random_member((Cell1, Cell2), ValidPairs), !.

rand_measure(_State, _Color, _Step, Piece, Cell):-
    Piece = quantum(Cell1, Cell2, _LastColor),
    random_member(Cell, [Cell1, Cell2]), !.

rand_move(_State, _Color, _Step, ValidMoves, Move):-
    random_member(Move, ValidMoves), !.

% ----------------------------------------------------------------------

% [Cerința 8] Definiți strategiile pentru un jucător care să câștige în
% medie mai mult de 50% dintre jocur împotriva jucătorul random.


smart_place(State, Color, Step, ValidPairs, Pair):-
    rand_place(State, Color, Step, ValidPairs, Pair).

smart_measure(State, Color, Step, Piece, Cell):-
    rand_measure(State, Color, Step, Piece, Cell).

smart_move(State, Color, Step, ValidMoves, Move):-
    rand_move(State, Color, Step, ValidMoves, Move).

% ----------------------------------------------------------------------

% [Bonus]. Definiți strategiile pentru un jucător care să câștige în
% medie mai mult de 95% dintre jocuri împotriva jucătorul random.


bonus_place(State, Color, Step, ValidPairs, Pair):-
    rand_place(State, Color, Step, ValidPairs, Pair).

bonus_measure(State, Color, Step, Piece, Cell):-
    rand_measure(State, Color, Step, Piece, Cell).

bonus_move(State, Color, Step, ValidMoves, Move):-
    rand_move(State, Color, Step, ValidMoves, Move).

% ----------------------------------------------------------------------
% ----------------------------------------------------------------------


play(Player1, Player2, State, Color, Step, LastPiece, Winner):-
    Player1 = (PPlace, PMeasure, PMove),
    ( has_cycle(State), !,
      call(PMeasure, State, Color, Step, LastPiece, Cell),
      collapse(State, LastPiece, Cell, NoCycle), !
    ; NoCycle = State ),
    ( winner(NoCycle, Winner), !
    ; Step =:= 50, !, Winner = [white, black]
    ; ( length(NoCycle, 6), !, valid_moves(NoCycle, Color, ValidMoves),
	call(PMove, NoCycle, Color, Step, ValidMoves, Move),
	Move = move(OldPiece, NewPiece),
	select(OldPiece, NoCycle, NewPiece, NextState), !
      ; valid_pairs(NoCycle, Color, ValidPairs),
	call(PPlace, NoCycle, Color, Step, ValidPairs, (Cell1, Cell2)),
	NewPiece = quantum(Cell1, Cell2, Color),
	NextState = [NewPiece | NoCycle], !),
      next_player(Color, NextColor), Step1 is Step + 1, !,
      play(Player2, Player1, NextState, NextColor, Step1, NewPiece, Winner)
    ).


play_against_random(Strategy, Winner):-
    %% Player is black, Rand is white
    Player = (Strategy, black),
    Random = ((rand_place, rand_measure, rand_move), white),
    random_permutation([Player, Random], [(Player1, Color1),(Player2, _)]),
    play(Player1, Player2, [], Color1, 0, none, Winner).


score_against_random(Strategy, Score):-
    score_against_random(Strategy, 1000, 0, 0, 0, WinsNo, DrawsNo, LosesNo),
    format(' Black: ~d, Draws: ~d, White: ~d. ', [WinsNo, DrawsNo, LosesNo]),
    Score is WinsNo / 1000.0.

score_against_random(_, 0, B, D, W, B, D, W):- !.
score_against_random(Strategy, N1, B1, D1, W1, B, D, W):-
    play_against_random(Strategy, Winner),
    (Winner = [black] -> B2 is B1 + 1 ; B2 = B1),
    (Winner = [white] -> W2 is W1 + 1 ; W2 = W1),
    (Winner = [_, _] -> D2 is D1 + 1 ; D2 = D1),
    N2 is N1 - 1,
    score_against_random(Strategy, N2, B2, D2, W2, B, D, W).
