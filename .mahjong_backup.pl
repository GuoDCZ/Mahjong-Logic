% Tiles are represented by integers.
% 11-19: Characters
% 21-29: Dots
% 31-39: Bamboos
% 41-47: Winds and Dragons
% other notation:
% 60: Draw & Discard
% "c111213": Chi 11-12-13
% "45p4545": Pon 45 From Opposite
% "r19": Riichi
% "424242a42": Concealed Kan 42
% "434343m43": Open Kan 43 From Right
% ... and so on

% tile(Tile)
% tile is a number between 11 and 47, excluding 20, 30, and 40.
% e.g. tile(11).
% e.g. tile(47).

tile(Tile) :-
    between(11, 47, Tile),
    Tile mod 10 =\= 0.

% Hand is a list of 13 or 14 tiles.

hand(Hand) :-
    maplist(tile, Hand).

% hand_sort(+Hand, -SortedHand)
% Sorts the hand.
% e.g. hand_sort([11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24], SortedHand).
% SortedHand = [11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24].

hand_sort(Hand, SortedHand) :-
    msort(Hand, SortedHand).

% Grouping notation:
% [0, 11]: A sequence starting from 11
% [1, 44]: A triplet of 44
% [2, 35]: A pair of 35
% GroupedHand is a list of groups.

% group(+Hand, -GroupedHand)
% Groups the hand.
% e.g. group([11, 12, 13, 14, 15, 16, 17, 18, 19, 24, 24, 41, 41, 41], GroupedHand).
% GroupedHand = [[0, 11], [0, 14], [0, 17], [1, 41], [2, 24]].
% Note: A hand may have multiple valid groupings.

group(Hand, Groups) :-
    hand(Hand),
    between(1, 3, Stage),
    group_helper([Stage, yes], Hand, Groups),
    writeln(Groups).

% group_helper(+State, +Hand, -Groups)
% Helper predicate for group.
group_helper([3, no], [], []).
group_helper(State, Hand, [Group|Groups]) :-
    extract_single(State, Hand, Group, StateNext, Rest),
    group_helper(StateNext, Rest, Groups).

% same_head(Hand1, Hand2)
% Checks if the heads of two hands are the same.
% e.g. same_head([11, 12, 13, 14, 15], [11, 16, 17, 18, 19]).
% true.
same_head([Head|_], [Head|_]).

% extract_single(+State, +Hand, -Group, -StateNext, -Rest)
% Extracts a group from a sorted hand.
% The group is a sequence, triplet, or pair.
% The group must contain the smallest tile in the hand.
% State is the current state of the extraction.
% The search must conduct in the following order:
% 1. Pair -> 2. Triplet -> 3. Sequence
% e.g. extract_single(s, [11, 12, 13, 14, 15, 16, 17, 18, 19, 24, 24, 41, 41, 41], Group, Rest).
% Group = [11, 12, 13],
% Rest = [14, 15, 16, 17, 18, 19, 24, 24, 41, 41, 41].
% Note: determine whether a pair has been extracted by checking the length of Hand.
% i.e. Length = 14, 11, 8, 5, 2 -> Pair not yet extracted
%      Length = 12, 9, 6, 3 -> Pair already extracted
%      Length = 13, 10, 7, 4, 1 -> Impossible

extract_single([1, yes], [Head, Head], [p, Head], [3, no], []).

extract_single([1, yes], [Head, Head, Head|Tail], [p, Head], [3, no], [Head|Tail]).

extract_single([1, yes], [Head, Head, Head2|Tail], [p, Head], [Stage2, no], [Head2|Tail]) :-
    between(1, 3, Stage2),
    Head \= Head2.

extract_single([2, no], [Head, Head, Head], [t, Head], [3, no], []).

extract_single([2, NeedPair], [Head, Head, Head, Head|Tail], [t, Head], [3, NeedPair], [Head|Tail]).

extract_single([2, NeedPair], [Head, Head, Head, Head2|Tail], [t, Head], [Stage2, NeedPair], [Head2|Tail]) :-
    between(1, 3, Stage2),
    Head \= Head2.

extract_single([3, no], [First|Rest], [s, First], [3, no], []) :-
    Second is First + 1,
    select_unique(Second, Rest, Rest1),
    Third is Second + 1,
    select_unique(Third, Rest1, []).

extract_single([3, NeedPair], [First|Rest], [s, First], [3, NeedPair], [First|Tail]) :-
    Second is First + 1,
    select_unique(Second, Rest, Rest1),
    Third is Second + 1,
    select_unique(Third, Rest1, [First|Tail]).

extract_single([3, NeedPair], [First|Rest], [s, First], [Stage2, NeedPair], [Head|Tail]) :-
    Second is First + 1,
    select_unique(Second, Rest, Rest1),
    Third is Second + 1,
    select_unique(Third, Rest1, [Head|Tail]),
    between(1, 3, Stage2),
    First \= Head.

% helper predicate for extract
% select_unique(+Element, +List, -Rest)
% Selects the first occurrence of Element in List.

select_unique(Element, [Element|Rest], Rest).
select_unique(Element, [Head|Rest], [Head|Rest1]) :-
    Head \= Element,
    select_unique(Element, Rest, Rest1).
