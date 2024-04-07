set_prolog_flag(answer_write_options,[max_depth(10000)]).
H0 = [11, 11, 11, 13, 13, 13, 16, 16, 16, 19, 19, 21, 21, 21], findall(GroupedHand, group(H0, GroupedHand), Results), !.
H1 = [11, 11, 11, 12, 13, 14, 15, 15, 16, 17, 18, 19, 19, 19], findall(GroupedHand, group(H1, GroupedHand), Results), !.
H2 = [11, 11, 12, 12, 13, 13, 21, 22, 23, 27, 28, 29, 41, 41], findall(GroupedHand, group(H2, GroupedHand), Results), !.
H3 = [11, 11, 11, 12, 12, 12, 13, 13, 13, 27, 28, 29, 41, 41], findall(GroupedHand, group(H3, GroupedHand), Results), !.
H4 = [11, 11, 11, 12, 13, 14, 15, 16, 17, 18, 19, 19, 19, WinningTile], findall([WinningTile, GroupedHand], group(H4, GroupedHand), Results), !.