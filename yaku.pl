% A Yaku is a certain pattern in Riichi Mahjong hand that scores points. 
% This file contains the predicates that define the Yaku in Riichi Mahjong.
%
% yaku/3
% ├── yaku_yakuman/2
% │   ├── 天和
% │   ├── 地和
% │   ├── 国士無双
% │   ├── 四暗刻
% │   ├── 大三元
% │   ├── 緑一色
% │   ├── 字一色
% │   ├── 小四喜
% │   ├── 大四喜
% │   ├── 清老頭
% │   ├── 四槓子
% │   └── 九蓮宝燈
% └── yaku_normal/3
%     ├── yaku_flags/3
%     │   ├── 門前清自摸和
%     │   ├── 立直
%     │   ├── 一発
%     │   ├── 海底摸月
%     │   ├── 河底撈魚
%     │   ├── 槍槓
%     │   ├── 嶺上開花
%     │   ├── ダブル立直
%     │   ├── 三暗刻
%     │   ├── 三槓子
%     │   └── 七対子
%     ├── 平和
%     ├── yaku_group/3
%     │   ├── 斷么九
%     │   ├── 対々和
%     │   ├── 三色同刻
%     │   ├── 小三元
%     │   ├── 混老頭
%     ├── yaku_peikou/3
%     │   ├── 一盃口
%     │   ├── 二盃口
%     ├── yaku_sequence/3
%     │   ├── 三色同順
%     │   ├── 一気通貫
%     │   ├── 全帯幺九
%     │   ├── 純全帯幺九
%     ├── yaku_flush/3
%     │   ├── 混一色
%     │   ├── 清一色
%     ├── yaku_yakuhai/5
%     │   ├── 自風 東、南、西、北
%     │   ├── 場風 東、南、西、北
%     │   ├── 役牌 白、發、中
%     └── yaku_dora/5
%         ├── ドラ
%         ├── 赤ドラ
%         └── 裏ドラ
% <Utils>                     <Usage>
%   merge/3                 ->  小四喜, 小三元 
%   one_pair/1              ->  一盃口
%   lose_1_han_if_open/3    ->  yaku_sequence/3, yaku_flush/3
%   sublist/2               ->  大三元, 小四喜, 三色同刻, 小三元, 三色同順, 一気通貫
%   green_tile/1            ->  緑一色
%   full_flush/1            ->  清一色
%   half_flush/1            ->  混一色
%   kokushi_wait/1          ->  国士無双
%   chuuren_wait/1          ->  九蓮宝燈

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
%               - no_kan [+]: no kan was made, this must come with menzen.
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
%               - suuankou [+]
%               - suukantsu [+]
%               - tenhou
%               - chiihou
%       - Group: the grouped form of the hand.
%           - Group = (Pairs, Sequences, Triplets)
%           - e.g. Group = ([12], [12, 23, 34], [16])
%       - Tiles: a list of sorted tiles.
%       - Wind = (SeatWind, RoundWind)
%           - SeatWind: the seat wind of the player.
%           - RoundWind: the round wind.
%       - Dora = (Dora, AkaDora, UraDora)
%           - *Dora: the number of *Dora.
%
% Yaku: the name of the Yaku.
% Han: the number of Han of the Yaku

% ======= YAKU =======
%
% Note: no normal yaku will be checked if a yakuman is found.
%
yaku(Info, Yaku, -1) :-
    yaku_yakuman(Info, Yaku).
yaku(Info, Yaku, Han) :-
    \+ yaku_yakuman(Info, _),
    yaku_normal(Info, Yaku, Han).

% ======= YAKUMAN PREDICATES =======

% yaku_yakuman(+Info, +Flags, -Yaku)
% Info = [Flags, Group, Hand, _, _]
% Flags < {suuankou, menzen, no_kan, suukantsu, tenhou, chiihou}
% Group = (Pairs, Triplets, Sequences)
% Hand = (Tiles, WinningTile)
yaku_yakuman([Flags|_], '天和') :- member(tenhou, Flags).
yaku_yakuman([Flags|_], '地和') :- member(chiihou, Flags).
yaku_yakuman([_, _, Tiles|_], '国士無双') :-
    kokushi_wait(WaitHand),
    remove_tile(_, Tiles, WaitHand).
yaku_yakuman([Flags|_], '四暗刻') :- member(suuankou, Flags).
yaku_yakuman([_, ([_], Triplets, _)|_], '大三元') :- 
    sublist([45, 46, 47], Triplets).
yaku_yakuman([_, ([Pair], Triplets, Sequences)|_], '緑一色') :-
    green_tile(Pair),
    maplist(green_tile, Triplets),
    maplist(green_sequence, Sequences).
yaku_yakuman([_, (Pairs, Triplets, [])|_], '字一色') :- 
    maplist(honor_tile, Triplets),
    maplist(honor_tile, Pairs).
yaku_yakuman([_, ([Pair], Triplets, _)|_], '小四喜') :- 
    insert_tile(Pair, Triplets, Sorted),
    sublist([41, 42, 43, 44], Sorted),
    Triplets =\= [41, 42, 43, 44]. % incompatible with 大四喜
yaku_yakuman([_, ([_], [41, 42, 43, 44], [])|_], '大四喜').
yaku_yakuman([_, ([Pair], Triplets, [])|_], '清老頭') :-
    terminal_tile(Pair),
    maplist(terminal_tile, Triplets).
yaku_yakuman([Flags|_], '四槓子') :- member(suukantsu, Flags).
yaku_yakuman([Flags, _, Tiles|_], '九蓮宝燈') :-
    member(menzen, Flags),
    member(no_kan, Flags),
    chuuren_wait(WaitHand),
    remove_tile(_, Tiles, WaitHand).

% ======= YAKU NORMAL =======

% yaku_normal(+Info, -Yaku, -Han)
%
% true if Yaku is a Yaku in the hand defined by Info.
% yaku_normal will seperate the yaku into different categories and check 
% them in order.

% 1. yaku(11) - 門前清自摸和, 立直, 一発, 海底摸月, 河底撈魚, 槍槓, 嶺上開花,
%               ダブル立直, 三暗刻, 三槓子, 七対子
yaku_normal([Flags|_], Yaku, Han) :-
    yaku_flags(Flags, Yaku, Han).
% 2. yaku - 平和
yaku_normal([Flags, ([Pair], [], _), _, (SeatWind, RoundWind)|_], '平和', 1) :- 
    member(menzen, Flags),
    member(ryanmen, Flags),
    \+ member(Pair, [SeatWind, RoundWind, 45, 46, 47]).
% 3. yaku(5) - 斷么九, 対々和, 三色同刻, 小三元, 混老頭
yaku_normal([_, Group|_], Yaku, Han) :- 
    yaku_group(Group, Yaku, Han).
% 4. yaku(2) - 一盃口, 二盃口
yaku_normal([Flags, Group|_], Yaku, Han) :- 
    member(menzen, Flags),
    yaku_peikou(Group, Yaku, Han).
% 5. yaku(4) - 三色同順, 一気通貫, 全帯幺九, 純全帯幺九
yaku_normal([Flags, Group|_], Yaku, NewHan) :-
    yaku_sequence(Group, Yaku, Han),
    lose_1_han_if_open(Flags, Han, NewHan).
% 6. yaku(2) - 混一色, 清一色
yaku_normal([Flags, _, Tiles|_], Yaku, NewHan) :-
    yaku_flush(Tiles, Yaku, Han),
    lose_1_han_if_open(Flags, Han, NewHan).
% 7. yaku(11) - 役牌
yaku_normal([_, ([_], Triplets, _), _, Wind|_], Yaku, Han) :- 
    yaku_yakuhai(Triplets, Wind, Yaku, Han).
% 8. yaku(3) - ドラ
yaku_normal([_, _, _, _, Dora], Yaku, Han) :- 
    yaku_dora(Dora, Yaku, Han).

% ======= YAKU PREDICATES =======

% yaku_flags(+Flags, -Yaku, -Han)
yaku_flags(Flags, '門前清自摸和', 1) :-
    member(menzen, Flags),
    member(tsumo, Flags).
yaku_flags(Flags, '立直', 1) :- member(riichi, Flags).
yaku_flags(Flags, '一発', 1) :- member(ippatsu, Flags).
yaku_flags(Flags, '海底摸月', 1) :- member(haitei, Flags).
yaku_flags(Flags, '河底撈魚', 1) :- member(houtei, Flags).
yaku_flags(Flags, '槍槓', 1) :- member(chankan, Flags).
yaku_flags(Flags, '嶺上開花', 1) :- member(rinshan, Flags).
yaku_flags(Flags, 'ダブル立直', 2) :- member(double_riichi, Flags).
yaku_flags(Flags, '三暗刻', 2) :- member(sanankou, Flags).
yaku_flags(Flags, '三槓子', 2) :- member(sankantsu, Flags).
yaku_flags(Flags, '七対子', 2) :- member(chiitoi, Flags).

% yaku_group(+Group, -Yaku, -Han)
% Group = (Pairs, Triplets, Sequences)
yaku_group((Pairs, Triplets, Sequences), '斷么九', 1) :-
    maplist(simple_tile, Pairs),
    maplist(simple_tile, Triplets),
    maplist(simple_sequence, Sequences).
yaku_group(([_], _, []), '対々和', 2).
yaku_group(([_], Triplets, _), '三色同刻', 2) :- 
    sublist([X, Y, Z], Triplets),
    three_color_tiles(X, Y, Z).
yaku_group(([Pair], Triplets, _), '小三元', 2) :-
    insert_tile(Pair, Triplets, Sorted),
    sublist([45, 46, 47], Sorted).
yaku_group((Pairs, Triplets, []), '混老頭', 2) :- 
    maplist(terminal_or_honor_tile, Pairs),
    maplist(terminal_or_honor_tile, Triplets).

% yaku_peikou(+Group, -Yaku, -Han)
% Group = (Pairs, Triplets, Sequences)
yaku_peikou(([_], _, Sequences), '一盃口', 1) :- 
    one_pair(Sequences),
    Sequences =\= [X, X, Y, Y]. % incompatible with 二盃口
yaku_peikou(([_], [], [X, X, Y, Y]), '二盃口', 3).

% yaku_sequence(+Group, -Yaku, -Han)
% Group = (Pairs, Triplets, Sequences)
yaku_sequence(([_], _, Sequences), '三色同順', 2) :-
    sublist([X, Y, Z], Sequences),
    Z is Y + 10, Y is X + 10.
yaku_sequence(([_], _, Sequences), '一気通貫', 2) :-
    sublist([X, Y, Z], Sequences),
    Z is Y + 3, Y is X + 3.
yaku_sequence(([Pair], Triplets, Sequences), '全帯幺九', 2) :-
    terminal_or_honor_tile(Pair),
    maplist(terminal_or_honor_tile, Triplets),
    maplist(terminal_sequence, Sequences),
    Sequences =\= [], % incompatible with 混老頭
    \+ maplist(terminal_tile, [Pair|Triplets]). % incompatible with 純全帯幺九
yaku_sequence(([Pair], Triplets, Sequences), '純全帯幺九', 3) :-
    terminal_tile(Pair),
    maplist(terminal_tile, Triplets),
    maplist(terminal_sequence, Sequences).

% yaku_flush(+Tiles, -Yaku, -Han)
yaku_flush(Tiles, '混一色', 3) :-
    half_flush(Tiles). % incompatible with 清一色
yaku_flush(Tiles, '清一色', 6) :-
    full_flush(Tiles).

% yaku_yakuhai(+Triplets, +SeatWind, +RoundWind, -Yaku, -Han)
yaku_yakuhai(Triplets, (41, 41), '連風 東', 2) :- member(41, Triplets).
yaku_yakuhai(Triplets, (42, 42), '連風 南', 2) :- member(42, Triplets).
yaku_yakuhai(Triplets, (43, 43), '連風 西', 2) :- member(43, Triplets).
yaku_yakuhai(Triplets, (41, RoundWind), '自風 東', 1) :- member(41, Triplets), RoundWind =\= 41.
yaku_yakuhai(Triplets, (42, RoundWind), '自風 南', 1) :- member(42, Triplets), RoundWind =\= 42.
yaku_yakuhai(Triplets, (43, RoundWind), '自風 西', 1) :- member(43, Triplets), RoundWind =\= 43.
yaku_yakuhai(Triplets, (44, _), '自風 北', 1) :- member(44, Triplets).
yaku_yakuhai(Triplets, (SeatWind, 41), '場風 東', 1) :- member(41, Triplets), SeatWind =\= 41.
yaku_yakuhai(Triplets, (SeatWind, 42), '場風 南', 1) :- member(42, Triplets), SeatWind =\= 42.
yaku_yakuhai(Triplets, (SeatWind, 43), '場風 西', 1) :- member(43, Triplets), SeatWind =\= 43.
yaku_yakuhai(Triplets, _, '役牌 白', 1) :- member(45, Triplets).
yaku_yakuhai(Triplets, _, '役牌 發', 1) :- member(46, Triplets).
yaku_yakuhai(Triplets, _, '役牌 中', 1) :- member(47, Triplets).

% yaku_dora(+Dora, +AkaDora, +UraDora, -Yaku, -Han)
yaku_dora((Dora, _, _), 'ドラ', Dora) :- Dora =\= 0.
yaku_dora((_, AkaDora, _), '赤ドラ', AkaDora) :- AkaDora =\= 0.
yaku_dora((_, _, UraDora), '裏ドラ', UraDora) :- UraDora =\= 0.

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

% green_tile(+Tile).
% green_sequence(+Sequence).
%
% true if Tile, Sequence is a green tile, green sequence.
%
% Tile: a valid tile.
% Sequence: a tile representing a sequence.
green_tile(22).
green_tile(23).
green_tile(24).
green_tile(26).
green_tile(28).
green_tile(46).
green_sequence(22).

% half_flush(+Tiles).
%
% true if Tiles contains only tiles of the same suit and honor tiles.
%
% Tiles: a list of sorted tiles.
half_flush([]).
half_flush(Tiles) :-
    last(Tiles, HonorLast),
    tile_suit(4, HonorLast),
    exclude(honor_tile, Tiles, NumberTiles),
    full_flush(NumberTiles).

% full_flush(+Tiles).
%
% true if Tiles contains only tiles of the same suit.
%
% Tiles: a list of sorted tiles.
full_flush([]).
full_flush(Tiles) :-
    Tiles = [First|_],
    tile_suit(Suit, First),
    last(Tiles, Last),
    tile_suit(Suit, Last).

% kokushi_wait(+WaitHand).
%
% true if WaitHand is a kokushi wait.
%
% WaitHand: a list of sorted tiles.
kokushi_wait([11, 19, 21, 29, 31, 39, 41, 42, 43, 44, 45, 46, 47]).

% chuuren_wait(+WaitHand).
%
% true if WaitHand is a chuuren wait.
%
% WaitHand: a list of sorted tiles.
chuuren_wait([11, 11, 11, 12, 13, 14, 15, 16, 17, 18, 19, 19, 19]).
chuuren_wait([21, 21, 21, 22, 23, 24, 25, 26, 27, 28, 29, 29, 29]).
chuuren_wait([31, 31, 31, 32, 33, 34, 35, 36, 37, 38, 39, 39, 39]).
