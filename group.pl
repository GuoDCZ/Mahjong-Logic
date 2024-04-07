:- module(group, [group/2]).
:- use_module(utils).

% Tiles are represented by integers.
% 11-19: Characters
% 21-29: Bamboos
% 31-39: Dots
% 41-47: Winds and Dragons
% other notation:
% 60: Draw & Discard
% "c111213": Chi 11-12-13
% "45p4545": Pon 45 From Opposite
% "r19": Riichi
% "424242a42": Concealed Kan 42
% "434343m43": Open Kan 43 From Right
% ... and so on



% group(+Hand, -Groups)
%
% true if Hand is a valid hand and Groups is a valid grouping of Hand.
% A valid grouping is a list of groups.
% A group is a pair, triplet, or sequence.
% A valid grouping should contain exactly 1 pair and 4 triplets or sequences.
%
% Hand: a list of 14 integers
% Groups: a list of groups
%
% Group notation:
% [p, 35]: A pair of 35
% [t, 44]: A triplet of 44
% [s, 11]: A sequence starting from 11
%
% NOTE: A hand may have multiple valid groupings.
%
% e.g. group([11, 11, 11, 12, 13, 14, 15, 15, 16, 17, 18, 19, 19, 19], Groups).
% Groups = [[t, 11], [s, 12], [p, 15], [s, 16], [t, 19]].

group(Hand, Groups) :-
    hand(Hand),
    hand_sort(Hand, SortedHand),
    group_helper([_, 1], SortedHand, Groups, "").

% group_helper(?State, +SortedHand, -Groups)
%
% true if Groups is a valid grouping of SortedHand.
%
% State: the current state of the extraction.
% State = [_, 1]: A pair has been extracted.
% State = [_, 0]: A pair has not been extracted.
% State = [p, _]: Extracting a pair.
% State = [t, _]: Extracting a triplet.
% State = [s, _]: Extracting a sequence.
%
% SortedHand: a sorted list of tiles from a valid hand.
% Groups: a list of groups
%
% e.g. group_helper([p, 1], [11, 11, 12, 12, 12], Groups).
% Groups = [[p, 11], [t, 12]].

% Base case: 0 more groups to extract.
group_helper([s, 0], [], [], _Indent).
% Recursive case: extract a group and continue extracting.
group_helper(State, Hand, [Group|Groups], Indent) :-
    extract_single(State, Hand, Group, Rest),
    write(Indent), writeln([Group, Rest]),   % Debugging
    atom_concat(Indent, "  ", NextIndent),   % Debugging
    mark_pair_found(State, NextState),
    mark_next_search(State, NextState, Hand, Rest),
    group_helper(NextState, Rest, Groups, NextIndent).

% mark_pair_found(+State, -NextState)
%
% true if NextState is the state after an extraction.
%
% State: the current state of the extraction.
% NextState: the state after an extraction.
%
% e.g. mark_pair_found([p, 1], NextState).
% NextState = [_, 0].

% A pair is extracted in this search. Thus, to indicate that a pair has
% been extracted, the NextState must be [_, 0].
mark_pair_found([p, 1], [_, 0]).
% Otherwise, the NextState must preserve the current NoPair/NeedPair status.
mark_pair_found([t, NeedPair], [_, NeedPair]).
mark_pair_found([s, NeedPair], [_, NeedPair]).

% mark_next_search(+State, +NextState, +Hand, +Rest)
%
% true if NextState is the state for the next search.
%
% State: the current state of the extraction.
% NextState: the state for the next search.
% Hand: the current hand.
% Rest: the remaining hand after an extraction.
%
% e.g. mark_next_search([p, 1], [_, _], Hand, Rest).
% Hand = [11, 11, 12, 12, 12],
% Rest = [12, 12].

% Whichever group is extracted, if the Hand and Rest have the same head,
% then the next search must be a sequence.
mark_next_search([_, _], [s, _], Hand, Rest) :-
    same_head(Hand, Rest).
% Otherwise, the next search can be any group.
mark_next_search([_, _], [_, _], Hand, Rest) :-
    \+ same_head(Hand, Rest).

same_head([Head|_], [Head|_]).

% extract_single(+State, +Hand, -Group, -Rest)
%
% true if Group is a valid group extracted from Hand and Rest is the
% remaining hand.
%
% State: the state of the extraction.
% Hand: the hand.
% Group: a valid group.
% Rest: the hand after an extraction.
%
% e.g. extract_single([p, 1], [11, 11, 12, 12, 12], Group, Rest).
% Group = [p, 11],
% Rest = [12, 12, 12].

% Extract a pair.
extract_single([p, 1], [Head, Head|Tail], [p, Head], Tail).
% Extract a triplet.
extract_single([t, _], [Head, Head, Head|Tail], [t, Head], Tail).
% Extract a sequence.
% Notice that the elements might not be consecutive.
extract_single([s, _], [First|Rest], [s, First], Tail) :-
    Second is First + 1,
    select_unique(Second, Rest, Rest1),
    Third is Second + 1,
    select_unique(Third, Rest1, Tail).

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
    Head < Element,
    select_unique(Element, Rest, Rest1).
