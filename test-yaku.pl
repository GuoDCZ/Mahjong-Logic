% Test file for yaku(+Info, -Yaku, -Han) and yakuman(+Info, -Yakuman) predicate.
%
% [Yaku, Han] pairs all possiblities:
% ['門前清自摸和', 1]
% ['立直', 1]
% ['一発', 1]
% ['平和', 1]
% ['一盃口', 1]
% ['海底撈月', 1]
% ['河底撈魚', 1]
% ['嶺上開花', 1]
% ['槍槓', 1]
% ['斷么九', 1]
% [<役牌>, 1]
% ['両立直', 2]
% ['混全帯幺九', 1/2]
% ['三色同順', 1/2]
% ['一気通貫', 1/2]
% ['対々和', 2]
% ['三暗刻', 2]
% ['三色同刻', 2]
% ['三槓子', 2]
% ['七対子', 2]
% ['混老頭', 2]
% ['小三元', 2]
% ['混一色', 2/3]
% ['純全帯幺九', 2/3]
% ['二盃口', 3]
% ['清一色', 5/6]
% ['ドラ', n]
% ['赤ドラ', n]
% ['裏ドラ', n]
% ['二暗刻', 2]
%
% [Yakuman] all possiblities:
% ['国士無双']
% ['国士無双１３面']
% ['大三元']
% ['四暗刻']
% ['四暗刻単騎']
% ['小四喜']
% ['大四喜']
% ['字一色']
% ['緑一色']
% ['清老頭']
% ['九蓮宝燈']
% ['純正九蓮宝燈']
% ['四槓子']
% ['天和']
% ['地和']

:- use_module(yaku).

test_all :-
    test1,
    test2,
    test3,
    test4.

% 1. 門前清自摸和
test1 :-
    Info = [
        [menzen, tsumo, ryanmen],
        ([11], [45], [11, 23, 24]),
        x,
        (41, 42),
        (0, 1, 0)
    ],
    findall([Yaku, Han], yaku(Info, Yaku, Han), Result),
    writeln(Result),
    permutation(Result, [
        ['門前清自摸和', 1],
        ['役牌 白', 1],
        ['赤ドラ', 1]
    ]).

% 2. 立直
test2 :-
    Info = [
        [riichi, menzen, tsumo],
        ([11], [43], [22, 33, 34]),
        x,
        (43, 42),
        (0, 0, 1)
    ],
    findall([Yaku, Han], yaku(Info, Yaku, Han), Result),
    writeln(Result),
    permutation(Result, [
        ['立直', 1],
        ['門前清自摸和', 1],
        ['自風 西', 1],
        ['裏ドラ', 1]
    ]).

% 3. 一発
test3 :-
    Info = [
        [ippatsu, riichi],
        ([15], [41, 42], [22, 33]),
        x,
        (43, 43),
        (0, 0, 0)
    ],
    findall([Yaku, Han], yaku(Info, Yaku, Han), Result),
    writeln(Result),
    permutation(Result, [
        ['立直', 1],
        ['一発', 1]
    ]).

% 4. 平和
test4 :-
    Info = [
        [ryanmen, menzen],
        ([11], [], [12, 22, 33, 34]),
        x,
        (41, 42),
        (0, 0, 0)
    ],
    findall([Yaku, Han], yaku(Info, Yaku, Han), Result),
    writeln(Result),
    permutation(Result, [
        ['平和', 1]
    ]).

% 60. 国士無双
test60 :-
    Info = [
        x,
        x,
        [11, 19, 21, 29, 31, 31, 39, 41, 42, 43, 44, 45, 46, 47],
        (41, 42),
        (0, 0, 0)
    ],
    findall([Yaku, Han], yaku(Info, Yaku, Han), Result),
    writeln(Result),
    permutation(Result, [
        ['国士無双', -1]
    ]).

% 61. 九蓮宝燈
test61 :-
    Info = [
        [menzen, no_kan],
        x,
        [11, 11, 11, 12, 13, 14, 15, 16, 16, 17, 18, 19, 19, 19],
        (41, 42),
        (0, 0, 0)
    ],
    findall([Yaku, Han], yaku(Info, Yaku, Han), Result),
    writeln(Result),
    permutation(Result, [
        ['九蓮宝燈', -1]
    ]).

% 61. 九蓮宝燈
test62 :-
    Info = [
        [menzen, no_kan],
        x,
        [11, 11, 11, 12, 13, 14, 15, 16, 16, 17, 18, 19, 19, Tile],
        (41, 42),
        (0, 0, 0)
    ],
    Yaku = '九蓮宝燈',
    Han = -1,
    findall(Tile, yaku(Info, Yaku, Han), Result),
    writeln(Result),
    permutation(Result, [
        11, 11, 11, 12, 13, 14, 15, 16, 16, 17, 18, 19, 19
    ]).