% Utility predicates for Mahjong

:- module(
    utils,[
        % Tile
        tile/1,
        number_tile/1,
        honor_tile/1,
        terminal_tile/1,
        terminal_or_honor_tile/1,
        simple_tile/1,
        terminal_sequence/1,
        simple_sequence/1,
        tile_suit/2,
        tile_number/2,
        % Hand
        hand/1,
        hand_sort/2,
        % List
        select_unique/3
    ]
).

% ======= TILE =======

% tile(Tile)
% 
% true if Tile is a valid tile.
% A valid tile is an integer between 11 and 47, excluding 20, 30, and 40.
% 
% Tile: an integer
% 
% e.g. tile(11).

tile(Tile) :-
    (number_tile(Tile); honor_tile(Tile)).

% number_tile(+Tile).
%
% true if Tile is a numbered tile.
%
% Tile: a valid tile.
number_tile(Tile) :- 
    (simple_tile(Tile); terminal_tile(Tile)).

% honor_tile(+Tile).
%
% true if Tile is an honor tile.
%
% Tile: a valid tile.
honor_tile(Tile) :-
    between(41, 47, Tile).

% terminal_tile(+Tile).
%
% true if Tile is a terminal (number 1 or 9) tile.
%
% Tiles: an integer representing a tile.
terminal_tile(Tile) :-
    member(Tile, [11, 19, 21, 29, 31, 39]).

% terminal_or_honor_tile(+Tile).
%
% true if Tile is a terminal or honor tile.
%
% Tiles: a valid tile.
terminal_or_honor_tile(Tile) :- 
    (terminal_tile(Tile); honor_tile(Tile)).

% simple_tile(+Tile).
%
% true if Tile numbered from 2 through 8.
%
% Tile: a valid tile.
simple_tile(Tile) :-
    (between(12, 18, Tile); between(22, 28, Tile); between(32, 38, Tile)).

% terminal_sequence(+Tile).
%
% true if sequence represented by Tile is a terminal sequence.
%
% Tiles: a tile representing a sequence.
terminal_sequence(Tile) :-
    member(Tile, [11, 17, 21, 27, 31, 37]).

% simple_sequence(+Tile).
%
% true if sequence represented by Tile is a simple sequence.
%
% Tiles: a valid tile representing a valid sequence.
simple_sequence(Tile) :-
    (between(12, 16, Tile); between(22, 26, Tile); between(32, 36, Tile)).

% suit(+Suit)
%
% true if Suit is a valid suit 1, 2, or 3.
%
% Suit: an integer
%
suit(Suit) :-
    between(1, 3, Suit).

% tile_suit(+Tile, -Suit)
%
% true if Suit is the suit of Tile.
%
% Tile: a valid number tile.
% Suit: a valid suit.
%
tile_suit(Tile, Suit) :- Suit is Tile // 10.

% tile_number(+Tile, -Number)
%
% true if Number is the number of Tile.
%
% Tile: a valid number tile.
% Number: an integer
%
tile_number(Tile, Number) :- Number is Tile mod 10.


% ======= HAND =======

% hand(Hand)
%
% true if Hand is a valid hand.
% A valid hand is a list of 14 valid tiles.
% A valid hand must not contain five identical tiles.
%
% Hand: a list of 14 integers
%
% e.g. hand([11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24, 25]).

hand(Hand) :-
    maplist(tile, Hand),
    length(Hand, 14),
    check_no_fifth_tile(Hand).

% check_no_fifth_tile(+Hand)
%
% true if Hand does not contain five identical tiles.
%
% Hand: a list of valid tiles
%
% e.g. check_no_fifth_tile([11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24, 25]).

check_no_fifth_tile(Hand) :-
    hand_sort(Hand, SortedHand),
    \+ has_fifth_tile(SortedHand).

% has_fifth_tile(+SortedHand)
%
% true if SortedHand contains five identical tiles.
%
% SortedHand: a sorted list of valid tiles
%
% e.g. has_fifth_tile([11, 11, 11, 11, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21]).

has_fifth_tile([Head, Head, Head, Head, Head|_]).
has_fifth_tile([_|Rest]) :-
    has_fifth_tile(Rest).

% hand_sort(+Hand, -SortedHand)
%
% true if SortedHand is a sorted version of Hand.
%
% Hand: a list of valid tiles
% SortedHand: a sorted list of valid tiles
%
% e.g. hand_sort([11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24, 25], SortedHand).
% SortedHand = [11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24, 25].

hand_sort(Hand, SortedHand) :-
    msort(Hand, SortedHand).

% ======= LIST =======

% select_unique(+Element, +List, -Rest)
%
% true if Rest is the list after removing the first occurrence of Element.
%
% Element: an element
% List: a list
% Rest: a list
%
% e.g. select_unique(1, [1, 2, 3, 1, 4], Rest).
% Rest = [2, 3, 1, 4].

% Base case: the element is the head of the list.
select_unique(Element, [Element|Rest], Rest).
% Recursive case: the element is not the head of the list.
% Notice that we force Element to be different from Head.
% This is to ensure that we only remove the first occurrence of Element.
select_unique(Element, [Head|Rest], [Head|Rest1]) :-
    select_unique(Element, Rest, Rest1),
    Head < Element.
