% settle(+Info, -Settlement).
%
% true if Settlement is the settlement of a winning hand with Info.
%
% - Info: a list of information about the winning hand.
%   Info = [Hand, WinningTile,
%           SeatWind, RoundWind,
%           DoraIndicators, UradoraIndicators,
%           Flags]
%       Flags is a subset of {<yaku>, tsumo}, 
%            where <yaku> = {riichi, double_riichi, ippatsu, haitei, houtei,
%                            rinshan, chankan, tenhou, chiihou}.
% - Settlement: a list of information about the settlement.
%   Settlement = [Yakus, Han, Fu, Scores]
%   - Yakus: a list of yaku names.
%   - Han: the total number of han.
%   - Fu: the total number of fu.
%   - Points: a list of points to be paid by East, and non-East players.
%
% e.g.
% ?- settle([
%   [11, 12, 13, 21, 22, 23, 26, 27, 28, 33, 33, 36, 37], 35, % Hand, WinningTile
%   42, 41, % SeatWind, RoundWind
%   [11], [43], % DoraIndicators, UradoraIndicators
%   [riichi, tsumo]], % Flags
%   Settlement
% ).
% Settlement = [
%   ['立直', '門前清自摸和', '平和', 'ドラ'],
%   4, 20,
%   [1300, 2600]
% ] % 4番目のプレイヤーが2600点支払い、他のプレイヤーが1300点支払う。
%

% ?- settle([
%   [31, 31, 31, 32, 45, 45, 45, "c111213", "27c2829"], 33, % Hand, WinningTile
%   43, 42, % SeatWind, RoundWind
%   [11], [43], % DoraIndicators, UradoraIndicators
%   []], % Flags
%   Settlement
% ).
% Settlement = [
%   ['混全帯么九', '役牌白'],
%   2, 30,
%   [2900]
% ] % 2番目のプレイヤーが2900点支払い、他のプレイヤーが0点支払う。

% TODO: Implement this predicate.
