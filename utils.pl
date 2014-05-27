
in_left(V_right,Vertex,[E|Tail]):-
        member(V_left,Vertex),
        E=arc(V_left,V_right),
        difference(Vertex,[V_left],Vertex2),
        in_left(V_right,Vertex2,Tail),!.
in_left(_,[],[]).

in_right(V_left,Vertex,[E|Tail]):-
        member(V_right,Vertex),
        E=arc(V_left,V_right),
        difference(Vertex,[V_right],Vertex2),
        in_right(V_left,Vertex2,Tail),!.
in_right(_,[],[]).

replace_right(V_old_right,Edges,V_new_right,[E|Tail]):-
	member(E,Edges),
	E=arc(_,V_right),
	\+ V_right=V_old_right,
	difference(Edges,[E],Edges2),
	replace_right(V_old_right,Edges2,V_new_right,Tail),!.

replace_right(V_old_right,Edges,V_new_right,[arc(V_left,V_new_right)|Tail]):-
	member(E_old,Edges),
	E_old=arc(V_left,V_old_right),
	difference(Edges,[E_old],Edges2),
	replace_right(V_old_right,Edges2,V_new_right,Tail),!.
replace_right(_,[],_,[]).

replace_left(V_old_left,Edges,V_new_left,[E|Tail]):-
	member(E,Edges),
	E=arc(V_left,_),
	\+ V_left=V_old_left,
	difference(Edges,[E],Edges2),
	replace_left(V_old_left,Edges2,V_new_left,Tail),!.

replace_left(V_old_left,Edges,V_new_left,[arc(V_new_left,V_right)|Tail]):-
	member(E_old,Edges),
	E_old=arc(V_old_left,V_right),
	difference(Edges,[E_old],Edges2),
	replace_left(V_old_left,Edges2,V_new_left,Tail),!.
replace_left(_,[],_,[]).


superset(R,S) :- subset(S,R).
superset_strict(R,S) :-
	length(R,LR),
        length(S,LS),
        LR>LS,
	subset(S,R).

%subset([X|R],S) :- member(X,S), subset(R,S).
%subset([],_).

subset([], []).
subset([E|Tail], [E|NTail]):-
  subset(Tail, NTail).
subset(Tail, [_|NTail]):-
  subset(Tail, NTail).

subset_strict(R,S):-
	subset(R,S),
	length(R,LR),
	length(S,LS),
	LR<LS.

difference([], _, []) :- !.
difference([A|C], B, D) :-
        member(A, B), !,
        difference(C, B, D).
difference([A|B], C, [A|D]) :-
        difference(B, C, D).

symmetric_difference(A,B,SD):-
	difference(A,B,DIF_AB),
	difference(B,A,DIF_BA),
	unionAll([DIF_AB,DIF_BA],SD).


union([X|Y],Z,W):-
	member(X,Z),!,  union(Y,Z,W).
union([X|Y],Z,[X|W]):- \+ member(X,Z),!, union(Y,Z,W).
union([],Z,Z):-!.

union(L1,L2,L3,LR):-!,
	union(L1,L2,L_12),
	union(L_12,L3,LR).

unionAll(ListOfLists,UnionUnique):-
	do_unionAll(ListOfLists,Union),
	unique(Union,UnionUnique).

do_unionAll([],[]):-!.
do_unionAll([HeadList|TailList],Union):-
	unionAll(TailList,UnionTail),
	union(HeadList,UnionTail,Union).

unique([],[]):-!.
unique([Head|Tail],[Head|UTail]):-
	\+memberchk(Head,Tail),!,
	unique(Tail,UTail).

unique([Head|Tail],[Head|UTail]):-
	memberchk(Head,Tail),!,
	delete(Tail,Head,Tail2),
	unique(Tail2,UTail).

intersection([X|Y],M,[X|Z]) :- member(X,M), intersection(Y,M,Z).
intersection([X|Y],M,Z) :- \+ member(X,M), intersection(Y,M,Z).
intersection([],_,[]).



replace(_,_,[],[]):-!.
replace(LookFor,New,[Head|Tail],[NewInstance|TailR]):-
	subsumes_term(LookFor,Head),
	copy_term(New,NewInstance), %Se duplica New en NewInstance sin modificar New (para la recursividad)
	bind(NewInstance,Head),!,   %Se copian los valores bounded de Head a los valores unbounded de NewInstance. Se asume que en Head todas las variables estanbounded y que ambos (NewInstance y Head) tienen la misma estructura.
	replace(LookFor,New,Tail,TailR).

replace(LookFor,New,[Head|Tail],[New|TailR]):-
	subsumes_term(LookFor,Head),
	%ground(LookFor),
	ground(New),!,
	replace(LookFor,New,Tail,TailR).


replace(LookFor,New,[Head|Tail],[Head|TailR]):-
	\+ subsumes_term(LookFor,Head),!,
	replace(LookFor,New,Tail,TailR).
	
	
	

extract(_,[],[]):-!.
extract(LookFor,[Head|Tail],[Head|TailR]):-
	subsumes_term(LookFor,Head),!,
	extract(LookFor,Tail,TailR).

extract(LookFor,[Head|Tail],TailR):-
        \+ subsumes_term(LookFor,Head),!,
        extract(LookFor,Tail,TailR).

%+bind%
%Se copian los valores bounded de Head a los valores unbounded de NewInstance. Se asume que en Head todas las variables estanbounded y que ambos (NewInstance y Head) tienen la misma estructura.
bind([],[]):-!.
bind(UnboundedTerm,BoundedTerm):-
	compound(UnboundedTerm),
	\+  list(UnboundedTerm),!,
	UnboundedTerm=..[_|UArgList],
	BoundedTerm  =..[_|BArgsList],
	bind(UArgList,BArgsList).

bind(UnoundedList,BoundedList):-
	list(UnoundedList),!,
	UnoundedList=[UHead|UTail],
	BoundedList =[BHead|BTail],
	bind(UHead,BHead),
	bind(UTail,BTail).

bind(UnboundedVar,BoundedVar):-
	\+ compound(UnboundedVar),
	\+ compound(BoundedVar),
	var(UnboundedVar),
	nonvar(BoundedVar),!,
	copy_term(BoundedVar,UnboundedVar).


bind(UnboundedVar,BoundedVar):-
	\+ compound(UnboundedVar),
	\+ compound(BoundedVar),
	nonvar(UnboundedVar),
	nonvar(BoundedVar),!.

bind(UTerm,BTerm):-
	copy_term(BTerm,UTerm).
%-bind%


paths2(From,To,Graph,_):-
	path2(From,To,Graph,Path),
       nl,write('assertz(path('),write(Path),write('))'),
	assertz(path(Path)),fail.
paths2(_,_,_,Paths):-
	findall(Path,path(Path),Paths),
	retractall(path(_)).

path2(From,To,Graph):-
	path2(From,To,Graph,Path),
	Path\=[].

path2(From,To,[arc(From,To)|_],[arc(From,To)]).

path2(From,To,[arc(From,Other)|Tail],[arc(From,Other)|TailR]):-
	To\==Other,
	path2(Other,To,Tail,TailR).

path2(From,To,[arc(Other,To)|Tail],[arc(Other,To)|TailR]):-
	From\==Other,
	path2(From,Other,Tail,TailR).

path2(From,To,[Head|Tail],TailR):-
	\+ clause(appended(Head),_),
	append(Tail,[Head],New),
	Tail\==New,
	nl,write('assertz(appended('),write(Head),write('))'),
	assertz(appended(Head)),
	path2(From,To,New,TailR),
	retract(appended(Head)).

path2(_,_,_,_):-
	retractall(appended(_)),fail.


assertQW([]).
assertQW([Head|Tail]):-
	assertz(mem(Head)),
	assertQW(Tail).

%paths:  looks for all paths
paths(From,To,QW,_):-
	assertQW(QW),
	path(From,To,QW,Path),
	assertz(path(Path)),fail.

paths(_,_,_,Paths):-
	findall(Path,clause(path(Path),_),Paths),
	retractall(mem(_)),
	retractall(path(_)).

%path:  looks for all paths
path(From,To,_,[arc(From,To)]):-
	mem(arc(From,To)).

path(From,To,_,[arc(From,Other)|Tail]):-
	mem(arc(From,Other)),
	Other\==To,
	path(Other,To,_,Tail).

%path: only looks for a single path
path(From,To,QW):-
	assertQW(QW),
	path(From,To),!,
	retractall(mem(_)).

path(From,To):-
	mem(arc(From,To)),!.

path(From,To):-
	mem(arc(From,Other)),
	Other\==To,
	path(Other,To),!.


writeN(0):-!.
writeN(N):-
        write('\t'),
        N2 is N -1,
        writeN(N2).

random_member(_, []):- fail.
random_member(Item, [Item|Tail]) :-
        length([Item|Tail], Length),
        random(0, Length, Idx),
	Idx = 0.
random_member(Item, [_|Tail]) :-
	random_member(Item,Tail).
%        length(Tail, Length),
%        random(0, Length, Idx),
%        Index is Idx + 1,
%        nth(Index, Tail, Item).

random_memberchk(Item, [Item]):-!.
random_memberchk(Item, List) :-
        length(List, Length),
        random(0, Length, Idx),
        Index is Idx + 1,
        nth(Index, List, Item),!.

concat_all_atoms([Atom],Atom):-!.
concat_all_atoms([Atom|Tail],RAtom):-!,
	concat_all_atoms(Tail,Suffix),
	atom_concat(Atom,Suffix,RAtom).

