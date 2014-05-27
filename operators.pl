:-op(1188,  fx, select).
:-op(1185,  fx, distinct).
:-op(1192, xfx, from).
:-op(900,  xfx, as).
:-op(1196, xfx, where).
:-op(1200, xfx, order).
:-op(1198,  fx, by).
:-op(900,  xf, [asc,desc]).

%tuple-based window operators
:-op(1186,  xfx, top).
%ENDtuple-based window operators

%time-based window operators
:-op(899,  xfx, row).
:-op(899,  xfx, range).
:-op(898, xf , [seconds,minutes,hours,days]).
:-op(899, xf , now).
%ENEDtime-based window operators

%conjunction, disjunction
:-op(800,xfy, and).
:-op(801,xfy, or).
%ENDconjunction, disjunction

:-op(700,xfx,<>).
:-op(350,xfy,::).
