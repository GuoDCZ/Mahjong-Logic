% point(+Info, -Points)
%
% true if Points is the points to be paid by East, and non-East players.
%
% - Info: a list of information about the winning score and winds.
%   Info = [Han, Fu, IsEast, IsTsumo]
%     - Han: the number of han. Negative han is yakuman.
%     - Fu: the number of fu.
%     - IsEast: true if the winner is East.
%     - IsTsumo: true if the winner wins by tsumo.
%
% - Points: a list of points to be paid.
%   if IsTsumo is true, Points = [NonEastPoint, EastPoint]
%   if IsTsumo is false, Points = [Point]

% East wins by tsumo
point([ 1,  40, y, y], [  700, 0]).
point([ 1,  50, y, y], [  800, 0]).
point([ 1,  60, y, y], [ 1000, 0]).
point([ 1,  70, y, y], [ 1200, 0]).
point([ 1,  80, y, y], [ 1300, 0]).
point([ 1,  90, y, y], [ 1500, 0]).
point([ 1, 100, y, y], [ 1600, 0]).
point([ 1, 110, y, y], [ 1800, 0]).
point([ 2,  20, y, y], [  700, 0]).
point([ 2,  30, y, y], [ 1000, 0]).
point([ 2,  40, y, y], [ 1300, 0]).
point([ 2,  50, y, y], [ 1600, 0]).
point([ 2,  60, y, y], [ 2000, 0]).
point([ 2,  70, y, y], [ 2300, 0]).
point([ 2,  80, y, y], [ 2600, 0]).
point([ 2,  90, y, y], [ 2900, 0]).
point([ 2, 100, y, y], [ 3200, 0]).
point([ 2, 110, y, y], [ 3600, 0]).
point([ 3,  20, y, y], [ 1300, 0]).
point([ 3,  25, y, y], [ 1600, 0]).
point([ 3,  30, y, y], [ 2000, 0]).
point([ 3,  40, y, y], [ 2600, 0]).
point([ 3,  50, y, y], [ 3200, 0]).
point([ 3,   F, y, y], [ 4000, 0]) :- F >= 60.
point([ 4,  20, y, y], [ 2600, 0]).
point([ 4,  25, y, y], [ 3200, 0]).
point([ 4,   F, y, y], [ 4000, 0]) :- F >= 40.
point([ 5,   _, y, y], [ 4000, 0]).
point([ H,   _, y, y], [ 6000, 0]) :- between(6, 7, H).
point([ H,   _, y, y], [ 8000, 0]) :- between(8, 10, H).
point([ H,   _, y, y], [12000, 0]) :- H >= 11.
point([ H,   _, y, y], [Point, 0]) :- H < 0, Point is -H * 16000. % yakuman

% East wins by ron
point([ 1,  30, y, n], [ 1500]).
point([ 1,  40, y, n], [ 2000]).
point([ 1,  50, y, n], [ 2400]).
point([ 1,  60, y, n], [ 2900]).
point([ 1,  70, y, n], [ 3400]).
point([ 1,  80, y, n], [ 3900]).
point([ 1,  90, y, n], [ 4400]).
point([ 1, 100, y, n], [ 4800]).
point([ 1, 110, y, n], [ 5300]).
point([ 2,  25, y, n], [ 2400]).
point([ 2,  30, y, n], [ 2900]).
point([ 2,  40, y, n], [ 3900]).
point([ 2,  50, y, n], [ 4800]).
point([ 2,  60, y, n], [ 5800]).
point([ 2,  70, y, n], [ 6800]).
point([ 2,  80, y, n], [ 7700]).
point([ 2,  90, y, n], [ 8700]).
point([ 2, 100, y, n], [ 9600]).
point([ 2, 110, y, n], [10600]).
point([ 3,  25, y, n], [ 4800]).
point([ 3,  30, y, n], [ 5800]).
point([ 3,  40, y, n], [ 7700]).
point([ 3,  50, y, n], [ 9600]).
point([ 3,   F, y, n], [12000]) :- F >= 60.
point([ 4,  25, y, n], [ 9600]).
point([ 4,   F, y, n], [12000]) :- F >= 40.
point([ 5,   _, y, n], [12000]).
point([ H,   _, y, n], [18000]) :- between(6, 7, H).
point([ H,   _, y, n], [24000]) :- between(8, 10, H).
point([ H,   _, y, n], [36000]) :- H >= 11.
point([ H,   _, y, n], [Point]) :- H < 0, Point is -H * 48000. % yakuman

% Non-East wins by tsumo
point([ 1,  30, n, y], [  300,  500]).
point([ 1,  40, n, y], [  400,  700]).
point([ 1,  50, n, y], [  400,  800]).
point([ 1,  60, n, y], [  500, 1000]).
point([ 1,  70, n, y], [  600, 1200]).
point([ 1,  80, n, y], [  700, 1300]).
point([ 1,  90, n, y], [  800, 1500]).
point([ 1, 100, n, y], [  800, 1600]).
point([ 1, 110, n, y], [  900, 1800]).
point([ 2,  20, n, y], [  400,  700]).
point([ 2,  30, n, y], [  500, 1000]).
point([ 2,  40, n, y], [  700, 1300]).
point([ 2,  50, n, y], [  800, 1600]).
point([ 2,  60, n, y], [ 1000, 2000]).
point([ 2,  70, n, y], [ 1200, 2300]).
point([ 2,  80, n, y], [ 1300, 2600]).
point([ 2,  90, n, y], [ 1500, 2900]).
point([ 2, 100, n, y], [ 1600, 3200]).
point([ 2, 110, n, y], [ 1800, 3600]).
point([ 3,  20, n, y], [  700, 1300]).
point([ 3,  25, n, y], [  800, 1600]).
point([ 3,  30, n, y], [ 1000, 2000]).
point([ 3,  40, n, y], [ 1300, 2600]).
point([ 3,  50, n, y], [ 1600, 3200]).
point([ 3,   F, n, y], [ 2000, 4000]) :- F >= 60.
point([ 4,  20, n, y], [ 1300, 2600]).
point([ 4,  25, n, y], [ 1600, 3200]).
point([ 4,   F, n, y], [ 2000, 4000]) :- F >= 40.
point([ 5,   _, n, y], [ 2000, 4000]).
point([ H,   _, n, y], [ 3000, 6000]) :- between(6, 7, H).
point([ H,   _, n, y], [ 4000, 8000]) :- between(8, 10, H).
point([ H,   _, n, y], [ 6000,12000]) :- H >= 11.
point([ H,   _, n, y], [Point,Point2]) :- 
    H < 0, Point is -H * 8000, Point2 is -H * 16000. % yakuman

% Non-East wins by ron
point([ 1,  30, n, n], [ 1000]).
point([ 1,  40, n, n], [ 1300]).
point([ 1,  50, n, n], [ 1600]).
point([ 1,  60, n, n], [ 2000]).
point([ 1,  70, n, n], [ 2300]).
point([ 1,  80, n, n], [ 2600]).
point([ 1,  90, n, n], [ 2900]).
point([ 1, 100, n, n], [ 3200]).
point([ 1, 110, n, n], [ 3600]).
point([ 2,  25, n, n], [ 1600]).
point([ 2,  30, n, n], [ 2000]).
point([ 2,  40, n, n], [ 2600]).
point([ 2,  50, n, n], [ 3200]).
point([ 2,  60, n, n], [ 3900]).
point([ 2,  70, n, n], [ 4500]).
point([ 2,  80, n, n], [ 5200]).
point([ 2,  90, n, n], [ 5800]).
point([ 2, 100, n, n], [ 6400]).
point([ 2, 110, n, n], [ 7100]).
point([ 3,  25, n, n], [ 3200]).
point([ 3,  30, n, n], [ 3900]).
point([ 3,  40, n, n], [ 5200]).
point([ 3,  50, n, n], [ 6400]).
point([ 3,   F, n, n], [ 8000]) :- F >= 60.
point([ 4,  25, n, n], [ 6400]).
point([ 4,   F, n, n], [ 8000]) :- F >= 40.
point([ 5,   _, n, n], [ 8000]).
point([ H,   _, n, n], [12000]) :- between(6, 7, H).
point([ H,   _, n, n], [16000]) :- between(8, 10, H).
point([ H,   _, n, n], [24000]) :- H >= 11.
point([ H,   _, n, n], [Point]) :- H < 0, Point is -H * 32000. % yakuman

% fu(+Fu)
%
% true if Fu is a valid number of fu.
%
% - Fu: the number of fu.
fu(F) :- between(20, 110, F), F mod 10 =:= 0.
fu(25).

% han(+Han)
%
% true if Han is a valid number of han.
%
% - Han: the number of han.
han(H) :- between(-10, 200, H).