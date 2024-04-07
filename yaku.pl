% A Yaku is a certain pattern in Riichi Mahjong hand that scores points. 
% This file contains the predicates that define the Yaku in Riichi Mahjong.

:- module(yaku, [yaku/3]).
:- use_module(group).
:- use_module(utils).

% yaku(+Info, -Yaku, -Han)
%
% true if Yaku is a Yaku in the hand defined by Info.
%
% Info: a list of information about the hand.
%       - Flags: a list of flags that indicate the state of the player.
%            - General flags:
%               - menzen [+]
%               - tsumo
%               - ryanmen [+]: two-sided wait.
%            - Yaku flags:
%               - riichi
%               - ippatsu
%               - haitei
%               - houtei
%               - rinshan
%               - chankan
%               - double_riichi
%               - sanankou [+]
%               - sankantsu [+]
%               - chiitoi [+]
%            - Yakuman flags:
%               - kokushi [+]
%               - kokushi_13_wait [+]
%               - suuankou [+]
%               - suuankou_tanki [+]
%               - chuuren [+]
%               - chuuren_9_wait [+]
%               - suukantsu [+]
%               - tenhou
%               - chiihou
%       - Group: the grouped form of the hand.
%            - Group = [Pairs, Sequences, Triplets]
%            - e.g. Group = [[12], [12, 23, 34], [16]]
%       - SeatWind: the seat wind of the player.
%       - RoundWind: the round wind.
%       - Dora: the number of dora.
%       - AkaDora: the number of aka dora.
%       - UraDora: the number of ura dora.
%
% Yaku: the name of the Yaku.
% Han: the number of Han of the Yaku

% 1. 門前清自摸和
yaku([Flags|_], '門前清自摸和', 1) :- 
    member(menzen, Flags),
    member(tsumo, Flags).

% 2. 立直
yaku([Flags|_], '立直', 1) :- member(riichi, Flags).

% 3. 一発
yaku([Flags|_], '一発', 1) :- member(ippatsu, Flags).

% 4. 平和
yaku([Flags, [[Pair],[],_], SeatWind, RoundWind|_], '平和', 1) :- 
    member(menzen, Flags),
    member(ryanmen, Flags),
    \+ member(Pair, [SeatWind, RoundWind, 45, 46, 47]).

% 5. 一盃口
yaku([Flags, [_,_,Sequences]|_], '一盃口', 1) :- 
    member(menzen, Flags),
    one_pair(Sequences),
    \+ yaku([Flags, [_,_,Sequences]|_], '二盃口', _).

% 6. 海底撈月
yaku([Flags|_], '海底撈月', 1) :- member(haitei, Flags).

% 7. 河底撈魚
yaku([Flags|_], '河底撈魚', 1) :- member(houtei, Flags).

% 8. 嶺上開花
yaku([Flags|_], '嶺上開花', 1) :- member(rinshan, Flags).

% 9. 槍槓
yaku([Flags|_], '槍槓', 1) :- member(chankan, Flags).

% 10. 斷么九
yaku([_, [Pairs, Triplets, Sequences]|_], '斷么九', 1) :- 
    maplist(simple_tile, Pairs),
    maplist(simple_tile, Triplets),
    maplist(simple_sequence, Sequences).

% 11. 役牌
yaku([_, [_, Triplets, _], 41|_   ], '自風 東', 1) :- member(41, Triplets).
yaku([_, [_, Triplets, _], _, 41|_], '場風 東', 1) :- member(41, Triplets).
yaku([_, [_, Triplets, _], 42|_   ], '自風 南', 1) :- member(42, Triplets).
yaku([_, [_, Triplets, _], _, 42|_], '場風 南', 1) :- member(42, Triplets).
yaku([_, [_, Triplets, _], 43|_   ], '自風 西', 1) :- member(43, Triplets).
yaku([_, [_, Triplets, _], _, 43|_], '場風 西', 1) :- member(43, Triplets).
yaku([_, [_, Triplets, _], 44|_   ], '自風 北', 1) :- member(44, Triplets).
yaku([_, [_, Triplets, _], _, 44|_], '場風 北', 1) :- member(44, Triplets).
yaku([_, [_, Triplets, _]|_       ], '役牌 白', 1) :- member(45, Triplets).
yaku([_, [_, Triplets, _]|_       ], '役牌 發', 1) :- member(46, Triplets).
yaku([_, [_, Triplets, _]|_       ], '役牌 中', 1) :- member(47, Triplets).

% 12. 両立直
yaku([Flags|_], '両立直', 2) :- member(double_riichi, Flags).

% 13. 混全帯幺九
yaku([Flags, [[Pair], Triplets, Sequences]|_], '混全帯幺九', Han) :- 
    lose_1_han_if_open(Flags, 2, Han),
    maplist(terminal_or_honor_tile, [Pair|Triplets]),
    maplist(terminal_sequence, Sequences),
    \+ yaku([Flags, [[Pair], Triplets, Sequences]|_], '純全帯幺九', _),
    \+ yaku([Flags, [[Pair], Triplets, Sequences]|_], '混老頭', _).

% 14. 三色同順
yaku([Flags, [_, _, Sequences]|_], '三色同順', Han) :- 
    lose_1_han_if_open(Flags, 2, Han),
    sublist([X, Y, Z], Sequences),
    three_color_tiles(X, Y, Z).

% 15. 一気通貫
yaku([Flags, [_, _, Sequences]|_], '一気通貫', Han) :- 
    lose_1_han_if_open(Flags, 2, Han),
    sublist([X, Y, Z], Sequences),
    straight_sequence(X, Y, Z).

% 16. 対々和
yaku([_, [_, _, []]|_], '対々和', 2).

% 17. 三暗刻
yaku([Flags|_], '三暗刻', 2) :- member(sanankou, Flags).

% 18. 三色同刻
yaku([_, [_, Triplets, _]|_], '三色同刻', 2) :- 
    sublist([X, Y, Z], Triplets),
    three_color_tiles(X, Y, Z).

% 19. 三槓子
yaku([Flags|_], '三槓子', 2) :- member(sankantsu, Flags).

% 20. 七対子
yaku([Flags|_], '七対子', 2) :- member(chiitoi, Flags).

% 21. 混老頭
yaku([_, [Pairs, Triplets, []]|_], '混老頭', 2) :- 
    maplist(terminal_or_honor_tile, Pairs),
    maplist(terminal_or_honor_tile, Triplets).

% 22. 小三元
yaku([_, [[Pair], Triplets, _]|_], '小三元', 2) :- 
    sort([Pair|Triplets], Sorted),
    sublist([45, 46, 47], Sorted).

% 23. 混一色
yaku([Flags, [Pairs, Triplets, Sequences]|_], '混一色', Han) :- 
    lose_1_han_if_open(Flags, 3, Han),
    suit(Suit),
    half_flush(Pairs, Suit),
    half_flush(Triplets, Suit),
    half_flush(Sequences, Suit),
    \+ yaku([Flags, [Pairs, Triplets, Sequences]|_], '清一色', _).

% 24. 純全帯幺九
yaku([Flags, [[Pair], Triplets, Sequences]|_], '純全帯幺九', Han) :- 
    lose_1_han_if_open(Flags, 3, Han),
    maplist(terminal_tile, [Pair|Triplets]),
    maplist(terminal_sequence, Sequences).

% 25. 二盃口
yaku([Flags, [_, [], [X, X, Y, Y]]|_], '二盃口', 3) :- 
    member(menzen, Flags).

% 26. 清一色
yaku([Flags, [Pairs, Triplets, Sequences]|_], '清一色', Han) :- 
    lose_1_han_if_open(Flags, 6, Han),
    suit(Suit),
    full_flush(Pairs, Suit),
    full_flush(Triplets, Suit),
    full_flush(Sequences, Suit).

% 27. ドラ
yaku([_, _, _, _, Dora|_], 'ドラ', Dora) :- Dora =\= 0.

% 28. 赤ドラ
yaku([_, _, _, _, _, AkaDora|_], '赤ドラ', AkaDora) :- AkaDora =\= 0.

% 29. 裏ドラ
yaku([_, _, _, _, _, _, UraDora|_], '裏ドラ', UraDora) :- UraDora =\= 0.

% ======= YAKUMAN =======

% yakuman(+Group, +Info, +Flags, -Yaku)
%
% Note: once a yakuman is found, no yaku will be checked.

% 30. 国士無双
yakuman([Flags|_], '国士無双') :- member(kokushi, Flags).

% 31. 国士無双１３面
yakuman([Flags|_], '国士無双１３面') :- member(kokushi_13_wait, Flags).

% 32. 四暗刻
yakuman([Flags|_], '四暗刻') :- member(suuankou, Flags).

% 33. 四暗刻単騎
yakuman([Flags|_], '四暗刻単騎') :- member(suuankou_tanki, Flags).

% 34. 大三元
yakuman([_, [_, Triplets, _]|_], '大三元') :- 
    sublist([45, 46, 47], Triplets).

% 35. 小四喜
yakuman([_, [[Pair], Triplets, _]|_], '小四喜') :- 
    sort([Pair|Triplets], Sorted),
    sublist([41, 42, 43, 44], Sorted),
    \+ yakuman([[Pair], Triplets, _], _, _, '大四喜').

% 36. 大四喜
yakuman([_, [_, [41, 42, 43, 44], []]|_], '大四喜').

% 37. 字一色
yakuman([_, [Pairs, Triplets, []]|_], '字一色') :- 
    maplist(honor_tile, Pairs),
    maplist(honor_tile, Triplets).

% 38. 緑一色
yakuman([_, [[Pair], Triplets, Sequences]|_], '緑一色') :-
    maplist(green_tile, [Pair|Triplets]),
    maplist(green_sequence, Sequences).

% 39. 九連宝燈
yakuman([Flags|_], '九連宝燈') :- member(chuuren, Flags).

% 40. 純正九蓮宝燈
yakuman([Flags|_], '九連宝燈') :- member(chuuren_9_wait, Flags).

% 41. 四槓子
yakuman([Flags|_], '四槓子') :- member(suukantsu, Flags).

% 42. 天和
yakuman([Flags|_], '天和') :- member(tenhou, Flags).

% 43. 地和
yakuman([Flags|_], '地和') :- member(chiihou, Flags).

% ====== UTILITY PREDICATES ======

% one_pair(+Tiles).
%
% true if Tiles contains exactly one pair of identical tiles.
% 
% Tiles: a list of Sorted tiles.
one_pair([X, X|_]).
one_pair([X, Y|T]) :- 
    X \= Y,
    one_pair([Y|T]).

% lose_1_han_if_open(+Flags, +Han, -NewHan).
%
% NewHan is Han - 1 if the hand is open.
%
% Flags: a list of flags that indicate the state of the player.
lose_1_han_if_open(Flags, Han, Han) :- 
    member(menzen, Flags).
lose_1_han_if_open(Flags, Han, NewHan) :- 
    \+ member(menzen, Flags),
    NewHan is Han - 1.

% three_color_tiles(+Tile1, +Tile2, +Tile3).
%
% true if Tiles contains three tiles of identical numbers and different suits.
%
% Tile(n): sorted number tiles in order.
three_color_tiles(X, Y, Z) :- 
    X is Y - 10,
    Y is Z - 10.

% sublists(?Sublist, ?List).
%
% true if Sublist is List with some elements removed.
% The order is preserved.
%
% Sublist, List: lists of tiles in ascending order.
sublist([], _).
sublist([Head|TailSub], [Head|Tail]) :-
    sublist(TailSub, Tail).
sublist([HeadSub|TailSub], [Head|Tail]) :-
    sublist([HeadSub|TailSub], Tail),
    Head < HeadSub.

% straight_sequence(+Tile1, +Tile2, +Tile3).
%
% true if Tiles contains three consecutive tiles of the same suit.
%
% Tile(n): sorted number tiles in order.
straight_sequence(X, Y, Z) :- 
    X is Y - 3,
    Y is Z - 3.

% full_flush(+Tiles, +Suit).
%
% true if Tiles contains only suit Suit.
%
% Tiles: a list of sorted tiles.
full_flush([]).
full_flush([First|Rest], Suit) :-
    last([First|Rest], Last),
    tile_suit(First, Suit),
    tile_suit(Last, Suit).

% half_flush(+Tiles, +Suit).
%
% true if Tiles contains only suit Suit and honor tiles.
%
% Note: A half flush must be a full flush.
%
% Tiles: a list of sorted tiles.
half_flush(Tiles, Suit) :-
    exclude(honor_tile, Tiles, NumberTiles),
    full_flush(NumberTiles, Suit).

% green_tile(+Tile).
%
% true if Tile is a green tile.
%
% Tile: a valid tile.
green_tile(22).
green_tile(23).
green_tile(24).
green_tile(26).
green_tile(28).
green_tile(46).

% green_sequence(+Sequence).
%
% true if Sequence is a green sequence.
%
% Sequence: a tile representing a sequence.
green_sequence(22).
