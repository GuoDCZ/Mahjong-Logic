% Utility predicates for Mahjong

:- module(
    utils,[
        % Tile
        tile/1,
        number_tile/1,
        terminal_or_honor_tile/1,
        honor_tile/1,
        terminal_tile/1,
        simple_tile/1,
        terminal_sequence/1,
        simple_sequence/1,
        tile_suit/1,
        tile_suit/2,
        tile_number/1,
        tile_number/2,
        % Tiles
        sort_tiles/2,
        % Hand
        hand/1,
        % List
        select_tile/3,
        remove_tile/3,
        insert_tile/3
    ]
).

% ======= TILE =======

% tile(?Tile)
tile(Tile) :-
    (number_tile(Tile); honor_tile(Tile)).

% number_tile(?Tile).
number_tile(Tile) :- 
    (terminal_tile(Tile); simple_tile(Tile)).

% terminal_or_honor_tile(?Tile).
terminal_or_honor_tile(Tile) :- 
    (terminal_tile(Tile); honor_tile(Tile)).

% honor_tile(?Tile).
honor_tile(Tile) :-
    between(41, 47, Tile).

% terminal_tile(?Tile).
terminal_tile(Tile) :-
    member(Tile, [11, 19, 21, 29, 31, 39]).

% simple_tile(?Tile).
simple_tile(Tile) :-
    (between(12, 18, Tile); between(22, 28, Tile); between(32, 38, Tile)).

% terminal_sequence(?Tile).
% Tile: the first tile of a sequence.
terminal_sequence(Tile) :-
    member(Tile, [11, 17, 21, 27, 31, 37]).

% simple_sequence(?Tile).
simple_sequence(Tile) :-
    (between(12, 16, Tile); between(22, 26, Tile); between(32, 36, Tile)).

% tile_suit(?Suit)
tile_suit(Suit) :-
    between(1, 3, Suit).

% tile_suit(?Suit, ?Tile)
tile_suit(Suit, Tile) :-
    Suit is Tile // 10.

% tile_number(?Tile)
tile_number(Tile) :-
    between(1, 9, Tile).

% tile_number(?Tile, ?Number)
tile_number(Tile, Number) :-
    Number is Tile mod 10.

% ======= TILES =======

% hand_sort(+Tiles, -SortedTiles)
sort_tiles(Tiles, SortedTiles) :-
    msort(Tiles, SortedTiles).

% ======= HAND =======

% hand(Hand)
% Hand is a list of 14 tiles with no more than four identical tiles.
hand(Hand) :-
    length(Hand, 14),
    sort_tiles(Hand, SortedHand),
    check_no_fifth_tile_(SortedHand).

% check_no_fifth_tile(+Hand)
check_no_fifth_tile_(Hand) :-
    hand_sort(Hand, SortedHand),
    \+ has_fifth_tile_(SortedHand).

% has_fifth_tile(+SortedHand)
has_fifth_tile_([Head, Head, Head, Head, Head|_]).
has_fifth_tile_([_|Rest]) :-
    has_fifth_tile_(Rest).

% ======= LIST =======

% The following 3 predicates behave similarly to select/3. However, they are
% specifically designed due to:
% 1. We want to enforce determinism to avoid selecting the same tile.
% 2. We want to maintain the order of the tiles in the list.
% Note: When all arguments are instantiated, those predicates behave almost
% the same, while insert_tile/3 checks the order.

% select_tile(+Tile, +Tiles, ?Rest)
% Rest is Tiles with the first occurrence of Tile removed.
% Note: Elem must be instantiated, otherwise consider select_unique_elem/3.
select_tile(Tile, [Tile|Rest], Rest).
select_tile(Tile, [Head|Tiles], [Head|Rest]) :-
    Tile \= Head,
    select_tile(Tile, Tiles, Rest).

% remove_tile(?Tile, +Tiles, +Rest)
% Rest is Tiles with the first occurrence of Tile removed.
remove_tile(Tile, [Tile|Rest], Rest).
remove_tile(Tile, [Head|Tiles], [Head|Rest]) :-
    remove_tile(Tile, Tiles, Rest),
    Tile \= Head.

% insert_tile(+Tile, ?Tiles, +Rest)
% Tiles is Rest with Tile inserted in the correct position.
insert_tile(Tile, [Tile], []).
insert_tile(Tile, [Tile, Head|Rest], [Head|Rest]) :-
    Tile < Head.
insert_tile(Tile, [Head|Tail], [Head|Rest]) :-
    Tile >= Head,
    insert_tile(Tile, Tail, Rest).