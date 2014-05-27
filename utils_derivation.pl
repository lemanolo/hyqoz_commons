clear_dtf_ids:-
	retractall(id(_)).

create_dtf(dtf,Aliases,Expression,Projection,Function):-!,
	create_id(dtf,Aliases,Id),
	Function=..[Id|[Aliases,Expression,Projection,Id]].

create_dtf(Type,Aliases,Expression,Projection,Function):-
	Type\==dtf,!,
	%create_id(dtf,[],Id),
	create_id(Type,Aliases,Id_2),

	%Function=..[Id|[Aliases,Expression,Projection,Id_2]],  %Abstract regresentation
	%Function=..[Id_2|[Aliases,Expression,Projection,Id_2]], %Friendly representation
	Function=..[dtf|[Aliases,Expression,Projection,Id_2]]. %Friendly representation

create_uqw(Activity,qw(A,P,V,E,in,out,cost(0,0,0))):-
       A=[Activity],
       P=[],
       unionAll([A,P,[in,out]],V),
       E=[arc(in,Activity),arc(Activity,out)].
	

	


create_id(Type,[],Id):-
        clause(id(_),true),!,
        %atom_concat(Prefix,'_',Type), atom_length(Prefix,4),!,
        findall(OtherId,(id(OtherId),
                         atom_concat(Type,_,OtherId)
                        ),List_of_ids),
        length(List_of_ids,L),
        Suffix is L+1,
	 number_atom(Suffix,SuffixAtom),
	 concat_all_atoms([Type,SuffixAtom],Id),
        %MEM nl,write('assertz(id('),write(Id),write('))'),
        assertz(id(Id)).

create_id(Type,[],Id):-
        \+clause(id(_),true),!,
        %atom_concat(Prefix,_,Type), atom_length(Prefix,4),!,
	 concat_all_atoms([Type,'1'],Id),
        %nl,write('assertz(id('),write(Id),write('))'),
        assertz(id(Id)).

create_id(Type,Aliases,Id):-
	clause(id(_),true),!,
	%atom_concat(Prefix,'_',Type), atom_length(Prefix,4),!,
        sort(Aliases,AliasesSorted),
        append([Type,'_'],AliasesSorted,Id_list),
        concat_all_atoms(Id_list,Id_prefix),	
	findall(OtherId,(id(OtherId),
                         atom_concat(Id_prefix,_,OtherId)
                        ),List_of_ids),
        length(List_of_ids,L),
        Suffix is L+1,
	number_atom(Suffix,SuffixAtom),
	concat_all_atoms([Id_prefix,SuffixAtom],Id),
       %nl,write('assertz(id('),write(Id),write('))'),
	assertz(id(Id)).

create_id(Type,Aliases,Id):-
	\+clause(id(_),true),!,
	%atom_concat(Prefix,_,Type), atom_length(Prefix,4),!,
        sort(Aliases,AliasesSorted),
        append([Type,'_'],AliasesSorted,Id_list1),
        append(Id_list1,['1'],Id_list),
        concat_all_atoms(Id_list,Id),	
        %nl,write('assertz(id('),write(Id),write('))'),
	assertz(id(Id)).

print_function([]):-!.
print_function([Function|Tail]):-!,
	print_function(Function),
	print_function(Tail).

print_function(Function):-!,
	functor(Function, Name, 4),
	nl,write(Name),write('('),
	arg(1,Function,DS),
	arg(2,Function,F),
	arg(3,Function,P),
	%Result arg(5,Function,Result),
     nl,writeN(1),write('   Aliases: { '),print_DS(DS),write(' },'),nl,
	writeN(1),write('Expression: { '),print_F(F),write(' }'),nl,
	writeN(1),write('Projection: { '),print_P(P),write(' }'),nl,
	%Result writeN(1),write('    Result: { '),print_Result(Result),write(' }'),
	nl,writeN(1),write(' )').

print_DS([]):-!.
print_DS([Alias]):-!,
	write(Alias).
print_DS([Alias|Tail]):-!,
	write(Alias),write(', '),
	print_DS(Tail).


print_F([]):-!.
print_F([Filter]):-!,
	write(Filter).
print_F([Filter|Tail]):-!,
	write(Filter),write(', '),
	print_F(Tail).

print_P([]):-!.
print_P([Path]):-!,
	write(Path).
print_P([Path|Tail]):-!,
	write(Path),write(', '),
	print_P(Tail).

print_Result([]):-!.
print_Result(Type):-!,
	print_type(Type).
print_Result([Type]):-!,
	print_type(Type).
print_Result([Type|Tail]):-
	print_type(Type),
	print_Result(Tail).



print_type(DataSet):-
        do_print_type(DataSet,3).

do_print_type([],_):-!.

do_print_type(DSName::DataSet,N):-
        atom(DSName),
        list(DataSet),!,
        nl,writeN(N),write(DSName),write('::<'),
        N2 is N+1,
        do_print_type(DataSet,N2),
	nl,writeN(N2),write('>').

do_print_type(DSName,N):-
	\+list(DSName),!,
        nl,writeN(N),write(DSName).%,write(', ').

do_print_type([Tuple],N):-!,
        do_print_type(Tuple,N).
do_print_type([Tuple|Tail],N):-
        do_print_type(Tuple,N),write(', '),
        do_print_type(Tail,N).

%do_printdataset([Name::Value|Tail],N):-
        %list(Value),!,
        %nl,writeN(N),write(Name),write('::'),
        %N2 is N+1,
        %do_printdataset(Value,N2),
        %do_printdataset(Tail,N).

%do_printdataset([Name::Value|Tail],N):-
        %atom(Value),!,
        %writeN(N),write(Name::Value),
        %do_printdataset(Tail,N).

print_relation_derivation(OldRelation,NewFunction,OldRelatedRelations,NewRelatedRelations):-!,
	OldRelation=..[OldRelType,A,B],
	%nl,write('         OldRelType: '),write(OldRelType),
	%nl,write('                  A: '),write(A),
	%nl,write('                  B: '),write(B),
	(OldRelType=dependent,  OldRelOp=('>>');
 	OldRelType=concurrent, OldRelOp=('><');
 	OldRelType=independent,OldRelOp=('||');
 	OldRelType=fail,OldRelOp=('//')
	),
	findall(_,
        	  ( member(NewRelatedRel,NewRelatedRelations),
         	    permutation([NewFunction,C],Perm),
         	    %nl,write('               Perm: '),write(Perm),
         	    NewRelatedRel=..[NewRelType|Perm],
         	    %nl,write('      NewRelatedRel: '),write(NewRelatedRel),
         	    replace(NewFunction,f,Perm,PermF),
         	    replace(C,c,PermF,PermFC),
         	    %NewRelatedRelNormal=..[NewRelType|PermFC],
         	    %nl,write('NewRelatedRelNormal: '),write(NewRelatedRelNormal),
         	    PermFC=[LeftFC,RightFC],
         	    (NewRelType=dependent,  NewRelOp=('>>');
          	    NewRelType=concurrent, NewRelOp=('><');
          	    NewRelType=independent,NewRelOp=('||');
          	    NewRelType=fail,NewRelOp=('//')
         	    ),
		    nl,write('\t\tRELS '),write('f= '),write(a),write(' '),write(OldRelOp),write(' '),write(b),write(' '),
		    %nl,write('\tRELS@'),write('f=@'),write(a),write('@'),write(OldRelOp),write('@'),write(b),write('@'),
         	   % nl,write('RELS@'),
        	    findall(_,( member(OldRelRelation,OldRelatedRelations),
         	                (permutation([A,C],PermRel),
                 	         %nl,write('              PermRel: '),write(PermRel),
                 	         OldRelRelation=..[OldRelRelationType|PermRel],
                 	         %nl,write('    OldRelRelation:'),write(OldRelRelation),
                 	         replace(A,a,PermRel,PermRel1)
         	                ),
              	                replace(C,c,PermRel1,PermRelC),
              	                PermRelC=[LeftOldRelatedRel,RightOldRelatedRel],
              	                (OldRelRelationType=dependent,  OldRelatedRelOp=('>>');
               	                 OldRelRelationType=concurrent, OldRelatedRelOp=('><');
               	                 OldRelRelationType=independent,OldRelatedRelOp=('||');
               	                 OldRelRelationType=fail,OldRelatedRelOp=('//')
              	                ),
              	                write(' '),write(LeftOldRelatedRel),write(' '),write(OldRelatedRelOp),write(' '),write(RightOldRelatedRel)
              	                %write('@'),write(LeftOldRelatedRel),write('@'),write(OldRelatedRelOp),write('@'),write(RightOldRelatedRel)
        	              ),_),
                    findall(_,( member(OldRelRelation,OldRelatedRelations),
                                (permutation([B,C],PermRel),
                                 %nl,write('              PermRel: '),write(PermRel),
                                 OldRelRelation=..[OldRelRelationType|PermRel],
                                 %nl,write('    OldRelRelation:'),write(OldRelRelation),
                                 replace(B,b,PermRel,PermRel1)
                                ),
                                replace(C,c,PermRel1,PermRelC),
                                PermRelC=[LeftOldRelatedRel,RightOldRelatedRel],
                                (OldRelRelationType=dependent,  OldRelatedRelOp=('>>');
                                 OldRelRelationType=concurrent, OldRelatedRelOp=('><');
                                 OldRelRelationType=independent,OldRelatedRelOp=('||');
                                 OldRelRelationType=fail,OldRelatedRelOp=('//')
                                ),
                                write(' & '),write(LeftOldRelatedRel),write(' '),write(OldRelatedRelOp),write(' '),write(RightOldRelatedRel)
                                %write('@&@'),write(LeftOldRelatedRel),write('@'),write(OldRelatedRelOp),write('@'),write(RightOldRelatedRel)
                              ),_),
         	    %write('@@-->@@'),write(LeftFC),write('@'),write(NewRelOp),write('@'),write(RightFC)
         	    write('  -->  '),write(LeftFC),write(' '),write(NewRelOp),write(' '),write(RightFC)
         	    %nl,write('             PermRel1:'),write(PermRel1),
         	    %nl,write('             PermRelC:'),write(PermRelC),
         	    %nl,write(' OlRelatedRelNormal:'),write(OlRelatedRelNormal)
        	  ),_).

print_function_short(Function):-
	print_function_short(Function,Output),
	write(Output).

print_function_short(Function,Output):-
	Function=..[_|[Id|_]],
	concat_all_atoms([Id],Output).%,'@',(Aliases1),'@',(Expression1),'@',(Projection1),'@',(Aliases2),'@',(Expression2),'@',(Projection2)],Output),

print_relation_short(Relation):-
	print_relation_short(Relation,Output),
	write(Output).

print_relation_short(Relation,Output):-
	print_relation_short(Relation,Id1,Op,Id2),
	concat_all_atoms([Id1,' ',Op,' ',Id2],Output).%,'@',(Aliases1),'@',(Expression1),'@',(Projection1),'@',(Aliases2),'@',(Expression2),'@',(Projection2)],Output),

print_relation_short(Relation,Id1,Op,Id2):-
	Relation=..[Type,F1,F2],
	F1=..[_|[Id1|_]],
	F2=..[_|[Id2|_]],
	((Type=dependent,  !, Op=('>>') );
	 (Type=concurrent, !, Op=('><') );
	 (Type=independent,!, Op=('||') )
	).

print_relation([]):-!.
print_relation([Relation|Tail]):-!,
	print_relation(Relation),
	nl,print_relation(Tail).
print_relation(Relation):-
	\+list(Relation),!,
	functor(Relation,RName,2),
	arg(1,Relation,F1),
	arg(2,Relation,F2),
	nl,write(RName),write('('),
	writeN(1),print_function(F1),
	writeN(1),print_function(F2),
	nl,write(')').

print_matrix_of_relations(Relations):-
	print_matrix_of_relations(Relations,0).
print_matrix_of_relations(Relations,Tabs):-
	findall(_,(member(Rel,Relations),assertz(Rel)),_),
	findall([F1,F2],(member(R,Relations),R=..[Rel|[F1,F2]]),Functions_lists),
	unionAll(Functions_lists,Functions),
	sort(Functions,FunctionsSorted),
	do_print_matrix_of_relations(FunctionsSorted,FunctionsSorted,Tabs),
	nl,writeN(Tabs),write('FUNCTIONS\t'),print_header(FunctionsSorted),
	retractall(independent(_,_)),
	retractall(concurrent(_,_)),
	retractall(dependent(_,_)).
	%nl,writeN(Tabs),print_all_functions(FunctionsSorted).

do_print_matrix_of_relations(_,[],_):-!.
do_print_matrix_of_relations(Functions,[Id|Tail],Tabs):-!,
	%arg(5,F,Id),
	nl,writeN(Tabs),write(Id),write('\t\t'),print_row(Id,Functions),
	do_print_matrix_of_relations(Functions,Tail,Tabs).

print_all_functions([]):-!.
print_all_functions([F|Tail]):-
	nl,write(F),
	print_all_functions(Tail).

print_row(_,[]):-!.
print_row(F,[OtherF]):-!,
	findall('>>',   (clause(dependent(F,OtherF),true),   dependent(F,OtherF)),  Dependent),
	findall('><',  (clause(concurrent(F,OtherF),true),  concurrent(F,OtherF)), Concurrent1),
	findall('><',  (clause(concurrent(OtherF,F),true),  concurrent(OtherF,F)), Concurrent2),
	findall('||', (clause(independent(F,OtherF),true), independent(F,OtherF)),Independent1),
	findall('||', (clause(independent(OtherF,F),true), independent(OtherF,F)),Independent2),
	findall('//', (clause(fail(F,OtherF),true), fail(F,OtherF)),Deadlock1),
	findall('//', (clause(fail(OtherF,F),true), fail(OtherF,F)),Deadlock2),
	unionAll([Dependent,Concurrent1,Concurrent2,Independent1,Independent2,Deadlock1,Deadlock2],Relations),
	print_cell(F,OtherF,Relations).

print_row(F,[OtherF|Tail]):-!,
	findall('>>',   (clause(dependent(F,OtherF),true),   dependent(F,OtherF)),  Dependent),
	findall('><',  (clause(concurrent(F,OtherF),true),  concurrent(F,OtherF)), Concurrent1),
	findall('><',  (clause(concurrent(OtherF,F),true),  concurrent(OtherF,F)), Concurrent2),
	findall('||', (clause(independent(F,OtherF),true), independent(F,OtherF)),Independent1),
	findall('||', (clause(independent(OtherF,F),true), independent(OtherF,F)),Independent2),
	findall('//', (clause(fail(F,OtherF),true), fail(F,OtherF)),Deadlock1),
	findall('//', (clause(fail(OtherF,F),true), fail(OtherF,F)),Deadlock2),
	unionAll([Dependent,Concurrent1,Concurrent2,Independent1,Independent2,Deadlock1,Deadlock2],Relations),
	print_cell(F,OtherF,Relations),write('\t'),
	print_row(F,Tail).
	

print_cell(_,_,['>>']):-!,
	write('>>').

print_cell(F,OtherF,[Relation]):-
	compare(>,F,OtherF),!,
	write(Relation).

print_cell(F,OtherF,Relations):-
	length(Relations,L),
	L>2,!,
	compare(>,F,OtherF),!,
	write(Relations).


print_cell(_,_,_):-!.

print_header([]):-!.
print_header([Id]):-!,
	%arg(5,F,Id),
	write(Id).
print_header([Id|Tail]):-!,
	%arg(5,F,Id),
	write(Id),write('\t'),
	print_header(Tail).



nodeId(1,'A').   nodeId(2,'B').   nodeId(3,'C').   nodeId(4,'D').   nodeId(5,'E').   nodeId(6,'F').   nodeId(7,'G').   nodeId(8,'H').   nodeId(9,'I').   nodeId(10,'J'). 
nodeId(11,'K').  nodeId(12,'L').  nodeId(13,'M').  nodeId(14,'N').  nodeId(15,'O').  nodeId(16,'P').  nodeId(17,'Q').  nodeId(18,'R').  nodeId(19,'S').  nodeId(20,'T'). 
nodeId(21,'U').  nodeId(22,'V').  nodeId(23,'W').  nodeId(24,'X').  nodeId(25,'Y').  nodeId(26,'Z').  nodeId(27,'AA'). nodeId(28,'AB'). nodeId(29,'AC'). nodeId(30,'AD'). 
nodeId(31,'AE'). nodeId(32,'AF'). nodeId(33,'AG'). nodeId(34,'AH'). nodeId(35,'AI'). nodeId(36,'AJ'). nodeId(37,'AK'). nodeId(38,'AL'). nodeId(39,'AM'). nodeId(40,'AN'). 

print_graph(Relations):-
        %findall([IdL,IdR],(member(Rel,Relations), Rel=..[_,L,R],arg(5,L,IdL),arg(5,R,IdR)),Functions),
        findall([IdL,IdR],(member(Rel,Relations), Rel=..[_,IdL,IdR]),Functions),
        unionAll(Functions,F),
	sort(Functions),
        %findall(Functor,(member(Rel,Relations),Rel=..[T,L,R],arg(5,L,IdL),arg(5,R,IdR),functor(Functor,T,2),arg(1,Functor,IdL),arg(2,Functor,IdR)),Rels),
        nl,write('GRAPH: '),
	nl,write('\\begin{graph}'),
        findall(_,(nth(N,F,Vertex), nl,write('\t\\node[vertex] ('), write(Vertex),
				    nodeId(N,Id),concat_all_atoms([Id,'x'],CoorX),concat_all_atoms([Id,'y'],CoorY),
			            write(')  at (\\'),write(CoorX),write(', \\'),write(CoorY),write(')   {\\fontSize$\\activity{'), write(Vertex),write('}$};')),_),
        %findall(_,(member(Rel,Rels), Rel=..[T,L,R],nl,
        findall(_,(member(Rel,Relations), Rel=..[T,L,R],nl,write('\t'),
                                        (
                                                (T=dependent,    write('\\myarc{'),Color=blue);
                                                (T=independent, write('\\myedge{'),Color=green);
                                                (T=concurrent,  write('\\myedge{'),Color=red)

                                        ), write(L),write('}{'),write(R),write('}{\\fontSize$\\'),write(T),write('{}{}$} {0.5}{'),write(Color),write('}')
                                      ),_),
	nl,write('\\end{graph}').

