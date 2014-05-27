test_runtime_apis:-
       init_qw_cost,
       build_runtime_apis,
       Alias=op1_op1,
       runtime_bounded(Alias,Bounded0),
       runtime_free(Alias,Free0),
       nl,nl,write('   Alias: '),write(Alias),
       nl,write('Bounded0: '),write(Bounded0),
       nl,write('   Free0: '),write(Free0),

       DTF1=dtf([Alias],[op1_op1::op1_op1_o_m=val_f_op1_op1_o_m],[op1_op1::op1_op1_i_a,op1_op1::op1_op1_o_a,op1_op1::op1_op1_o_b,op1_op1::op1_op1_o_c,op1_op1::op1_op1_o_d,op1_op1::op1_op1_o_e,op1_op1::op1_op1_o_f,op1_op1::op1_op1_o_g,op1_op1::op1_op1_o_h,op1_op1::op1_op1_o_i,op1_op1::op1_op1_o_j,op1_op1::op1_op1_o_k,op1_op1::op1_op1_o_l,op1_op1::op1_op1_o_m,op1_op1::op1_op1_o_o],f_op1_op11),
       compute_runtime_api(Alias,DTF1,RTAPI1),
       update_runtime_api(Alias,RTAPI1),

       runtime_bounded(Alias,Bounded1),
       runtime_free(Alias,Free1),
       nl,nl,write('   Alias: '),write(Alias),
       nl,write('Bounded1: '),write(Bounded1),
       nl,write('   Free1: '),write(Free1),

       DTF2=dtf([Alias],[op1_op1::op1_op1_o_m=val_f_op1_op1_o_m],[op1_op1::op1_op1_o_a,op1_op1::op1_op1_o_b,op1_op1::op1_op1_o_c,   op1_op1::op1_op1_o_d,op1_op1::op1_op1_o_e,op1_op1::op1_op1_o_f,op1_op1::op1_op1_o_g,op1_op1::op1_op1_o_h,op1_op1::op1_op1_o_i,op1_op1::op1_op1_o_j,      op1_op1::op1_op1_o_k,op1_op1::op1_op1_o_l,op1_op1::op1_op1_o_m,op1_op1::op1_op1_o_o,op1_op1::op1_op1_o_q],f_op1_op11),
       compute_runtime_api(Alias,DTF2,RTAPI2),
       update_runtime_api(Alias,RTAPI2),

       runtime_bounded(Alias,Bounded2),
       runtime_free(Alias,Free2),
       nl,nl,write('   Alias: '),write(Alias),
       nl,write('Bounded2: '),write(Bounded2),
       nl,write('   Free2: '),write(Free2),

       DTF3=dtf([Alias],[op1_op1::op1_op1_o_m=val_f_op1_op1_o_m],[ op1_op1::op1_op1_o_m],f_op1_op11),
       compute_runtime_api(Alias,DTF3,RTAPI3),
       update_runtime_api(Alias,RTAPI3),

       runtime_bounded(Alias,Bounded3),
       runtime_free(Alias,Free3),
       nl,nl,write('   Alias: '),write(Alias),
       nl,write('Bounded3: '),write(Bounded3),
       nl,write('   Free3: '),write(Free3).



build_runtime_apis:- retractall(runtime_api(_,_,_,_)),
                     retractall(dataset(_,_)),
                     findall(_,(type_name(Alias,S::M),
                                api(S::M,Bounded,Free,_),
                                assertz(runtime_api(Alias,Bounded,Free,_)),
                                assertz(    dataset(Alias,Alias)))
                            ,_).

compute_runtime_api(Alias,DTF,RTAPI):- runtime_api(Alias,Bounded,Free,_),
                                       findall(Alias::BAtt,member(BAtt,Bounded),DottedBounded),
                                       findall(Alias::FAtt,member(FAtt,Free),DottedFree),
                                       DTF=..[dtf|[_,_,P,_]],
                                       extract(Alias::_,P,ProjectionAlias),
                                       intersection(ProjectionAlias,DottedBounded,NewDottedBounded),!,
                                       intersection(ProjectionAlias,DottedFree,NewDottedFree),!,
                                       findall(BAtt,member(Alias::BAtt,NewDottedBounded),NewBounded),
                                       findall(FAtt,member(Alias::FAtt,NewDottedFree),NewFree),
                                       RTAPI=runtime_api(Alias,NewBounded,NewFree,_).

update_runtime_api(Alias,runtime_api(Alias,Bounded,Free,_)):- retractall(runtime_api(Alias,_,_,_)),
                                                                 assertz(runtime_api(Alias,Bounded,Free,_)).

runtime_bounded(Alias,Bounded):-
        runtime_api(Alias,B,_,_),
        dotnot(Alias::B,Bounded).

runtime_free(Alias,Free):-
        runtime_api(Alias,_,F,_),
        dotnot(Alias::F,Free).

runtime_atts(Alias,Atts):-
        runtime_bounded(Alias,Bounded),
        runtime_free(Alias,Free),
        union(Bounded,Free,Atts).

bounded(Alias,Bounded):-
        type_name(Alias,DSName),
        api(DSName,B,_,_),!,
        dotnot(Alias::B,Bounded).
        
free(Alias,Free):-
        type_name(Alias,DSName),
        api(DSName,_,F,_),!,
        dotnot(Alias::F,Free).

atts(Alias,Atts):-
        bounded(Alias,Bounded),
        free(Alias,Free),
        union(Bounded,Free,Atts).

dotnot(A::Type,[A]):- atom(A), \+list(A),atom(Type), \+list(Type).
dotnot(A,[A]):- atom(A),!.

dotnot(A::List,DotNot):-
        list(List),
        findall(DN,(member(A_i,List),
                    dotnot(A_i,NewA_i),
                    member(DN_Sub_A_i,NewA_i),
                    DN=A::DN_Sub_A_i),
                   DotNot).

clear_types(Alias):- retractall(type_name(Alias,_)).
