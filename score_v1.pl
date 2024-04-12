% point(+Info, -Points)
%
% true if Point is the points to be paid for the winner.
% - Info: a list of information about the winning score and winds.
%   Info = [Han, Fu, IsOya, IsTsumo]
%     - Han: the number of han. Negative han is yakuman.
%     - Fu: the number of fu.
%     - IsOya: true if the winner is oya.
%     - IsTsumo: true if the winner wins by tsumo.
% - Points: a list of points to be paid.
%   if IsTsumo is true, Points = [KoPoint, OyaPoint]
%   if IsTsumo is false, Points = [Point]
point([Han, Fu, true, true], [Point, 0]) :- 
    point_oya_tsumo((Han, Fu), Point).
point([Han, Fu, true, false], [Point]) :- 
    point_oya_ron((Han, Fu), Point).
point([Han, Fu, false, true], Points) :- 
    point_ko_tsumo((Han, Fu), Points).
point([Han, Fu, false, false], [Point]) :-
    point_ko_ron((Han, Fu), Point).

% point_oya_tsumo((Han, Fu), Point)
%
% true if Point is the points to be paid for the oya's tsumo.
point_oya_tsumo((1, 40), 700).
point_oya_tsumo((1, 50), 800).
point_oya_tsumo((1, 60), 1000).
point_oya_tsumo((1, 70), 1200).
point_oya_tsumo((1, 80), 1300).
point_oya_tsumo((1, 90), 1500).
point_oya_tsumo((1, 100), 1600).
point_oya_tsumo((2, 20), 700).
point_oya_tsumo((2, 30), 1000).
point_oya_tsumo((2, 40), 1300).
point_oya_tsumo((2, 50), 1600).
point_oya_tsumo((2, 60), 2000).
point_oya_tsumo((2, 70), 2300).
point_oya_tsumo((2, 80), 2600).
point_oya_tsumo((2, 90), 2900).
point_oya_tsumo((2, 100), 3200).
point_oya_tsumo((2, 110), 3600).
point_oya_tsumo((3, 20), 1300).
point_oya_tsumo((3, 25), 1600).
point_oya_tsumo((3, 30), 2000).
point_oya_tsumo((3, 40), 2600).
point_oya_tsumo((3, 50), 3200).
point_oya_tsumo((3, F), 4000) :- F >= 60.
point_oya_tsumo((4, 20), 2600).
point_oya_tsumo((4, 25), 3200).
point_oya_tsumo((4, F), 4000) :- F >= 40.
point_oya_tsumo((5, _), 4000).
point_oya_tsumo((H, _), 6000) :- between(6, 7, H).
point_oya_tsumo((H, _), 8000) :- between(8, 10, H).
point_oya_tsumo((H, _), 12000) :- H >= 11.
point_oya_tsumo((H, _), Point) :- H < 0, Point is -H * 16000.

% point_oya_ron((Han, Fu), Point)
%
% true if Point is the points to be paid for the oya's ron.
point_oya_ron((1, 30), 1500).
point_oya_ron((1, 40), 2000).
point_oya_ron((1, 50), 2400).
point_oya_ron((1, 60), 2900).
point_oya_ron((1, 70), 3400).
point_oya_ron((1, 80), 3900).
point_oya_ron((1, 90), 4400).
point_oya_ron((1, 100), 4800).
point_oya_ron((2, 25), 2400).
point_oya_ron((2, 30), 2900).
point_oya_ron((2, 40), 3900).
point_oya_ron((2, 50), 4800).
point_oya_ron((2, 60), 5800).
point_oya_ron((2, 70), 6800).
point_oya_ron((2, 80), 7700).
point_oya_ron((2, 90), 8700).
point_oya_ron((2, 100), 9600).
point_oya_ron((2, 110), 10600).
point_oya_ron((3, 25), 4800).
point_oya_ron((3, 30), 5800).
point_oya_ron((3, 40), 7700).
point_oya_ron((3, 50), 9600).
point_oya_ron((3, F), 12000) :- F >= 60.
point_oya_ron((4, 25), 9600).
point_oya_ron((4, F), 12000) :- F >= 40.
point_oya_ron((5, _), 12000).
point_oya_ron((H, _), 18000) :- between(6, 7, H).
point_oya_ron((H, _), 24000) :- between(8, 10, H).
point_oya_ron((H, _), 36000) :- H >= 11.
point_oya_ron((H, _), Point) :- H < 0, Point is -H * 48000.

% point_ko_tsumo((Han, Fu), Points)
%
% true if Points is the points to be paid for the ko's tsumo.
point_ko_tsumo((1, 30), [300, 500]).
point_ko_tsumo((1, 40), [400, 700]).
point_ko_tsumo((1, 50), [400, 800]).
point_ko_tsumo((1, 60), [500, 1000]).
point_ko_tsumo((1, 70), [600, 1200]).
point_ko_tsumo((1, 80), [700, 1300]).
point_ko_tsumo((1, 90), [800, 1500]).
point_ko_tsumo((1, 100), [800, 1600]).
point_ko_tsumo((2, 20), [400, 700]).
point_ko_tsumo((2, 30), [500, 1000]).
point_ko_tsumo((2, 40), [700, 1300]).
point_ko_tsumo((2, 50), [800, 1600]).
point_ko_tsumo((2, 60), [1000, 2000]).
point_ko_tsumo((2, 70), [1200, 2300]).
point_ko_tsumo((2, 80), [1300, 2600]).
point_ko_tsumo((2, 90), [1500, 2900]).
point_ko_tsumo((2, 100), [1600, 3200]).
point_ko_tsumo((2, 110), [1800, 3600]).
point_ko_tsumo((3, 20), [700, 1300]).
point_ko_tsumo((3, 25), [800, 1600]).
point_ko_tsumo((3, 30), [1000, 2000]).
point_ko_tsumo((3, 40), [1300, 2600]).
point_ko_tsumo((3, 50), [1600, 3200]).
point_ko_tsumo((3, F), [2000, 4000]) :- F >= 60.
point_ko_tsumo((4, 20), [1300, 2600]).
point_ko_tsumo((4, 25), [1600, 3200]).
point_ko_tsumo((4, F), [2000, 4000]) :- F >= 40.
point_ko_tsumo((5, _), [2000, 4000]).
point_ko_tsumo((H, _), [3000, 6000]) :- between(6, 7, H).
point_ko_tsumo((H, _), [4000, 8000]) :- between(8, 10, H).
point_ko_tsumo((H, _), [6000, 12000]) :- H >= 11.
point_ko_tsumo((H, _), [Point, Point2]) :- 
    H < 0, Point is -H * 8000, Point2 is -H * 16000.

% point_ko_ron((Han, Fu), Point)
%
% true if Point is the points to be paid for the ko's ron.
point_ko_ron((1, 30), 1000).
point_ko_ron((1, 40), 1300).
point_ko_ron((1, 50), 1600).
point_ko_ron((1, 60), 2000).
point_ko_ron((1, 70), 2300).
point_ko_ron((1, 80), 2600).
point_ko_ron((1, 90), 2900).
point_ko_ron((1, 100), 3200).
point_ko_ron((2, 25), 1600).
point_ko_ron((2, 30), 2000).
point_ko_ron((2, 40), 2600).
point_ko_ron((2, 50), 3200).
point_ko_ron((2, 60), 3900).
point_ko_ron((2, 70), 4500).
point_ko_ron((2, 80), 5200).
point_ko_ron((2, 90), 5800).
point_ko_ron((2, 100), 6400).
point_ko_ron((2, 110), 7100).
point_ko_ron((3, 25), 3200).
point_ko_ron((3, 30), 3900).
point_ko_ron((3, 40), 5200).
point_ko_ron((3, 50), 6400).
point_ko_ron((3, F), 8000) :- F >= 60.
point_ko_ron((4, 25), 6400).
point_ko_ron((4, F), 8000) :- F >= 40.
point_ko_ron((5, _), 8000).
point_ko_ron((H, _), 12000) :- between(6, 7, H).
point_ko_ron((H, _), 16000) :- between(8, 10, H).
point_ko_ron((H, _), 24000) :- H >= 11.
point_ko_ron((H, _), Point) :- H < 0, Point is -H * 32000.
