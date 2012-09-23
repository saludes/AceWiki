:- multifile isclass/1, isin/2, own/2, dsubclass/2.
:- op(1000, xfx, (=>)).

%:- dynamic isclass/2.


forget_all :-
	findall(E, (E=_ - _, E, retract(E)),_).

% 
% Classes
%
isclass(C) :- istopclass(C).
isclass(C) :- dsubclass(C,_).
isclass(D) :- dsubclass(_,D).

istopclass(countable).

sum([A]; A).
% sum([A,B]; A + B).
sum([A|As]; Bs + A) :- sum(As; Bs).

unsum(A+B; [B|As]) :- unsum(A; As),!.
unsum(A; [A]).



%
% Auto replace 'some' or predicates with variables
% (Replacing all 'some' is required for core language)
% 

getVar([V|Vss],V,Vss,_) :- !.
getVar([],_,_,X) :- throw('run out of vars'(X)).

getVarUi([],_,_,R) :- throw('run out of vars'(R)).
getVarUi(Vs,V,Vss,R) :- 
	makeDialog(R,Vs,V),
	append(V1,[V|V2],Vs),
	append(V1,V2,Vss).





unsome(Vs,Ds, [],[], Vs,Ds).
unsome(Vs,Ds, [X|Xs],[Xr|Xsr], Vr,Dr) :-
	unsome(Vs,Ds, X,Xr, V1,D1),
	unsome(V1,D1, Xs,Xsr, Vr,Dr).
unsome(Vs,Ds, P - A,P - Ar, Vr,Dr) :-
	unsome(Vs,Ds, A,Ar, Vr,Dr).
unsome(Vs, Ds, R, V*unit(C), Vr,[def(V,R)|Ds]) :-
	R=some(C), 
	(Vs=[V|Vr]; throw('no more vars')).  % getVarUi(Vs,V,Vr,R). 
unsome(Vs,Ds, A + B,Ar + Br, Vss,Dss) :-
	unsome(Vs,Ds, A,Ar, Vs1,Ds1), unsome(Vs1,Ds1, B,Br, Vss,Dss).
unsome(Vs,Ds, K*A,K*Ar, Vr,Dr) :- unsome(Vs,Ds, A,Ar, Vr,Dr).
unsome(Vs,Ds, aeq(A,B),aeq(Ar,Br), Vss,Dss) :-
	unsome(Vs,Ds, A,Ar, Vs1,Ds1), unsome(Vs1,Ds1, B,Br, Vss,Dss),!.
unsome(Vs,Ds, A,A, Vs,Ds).

make_vars(A1-A2; Vs) :- 
	atom_codes(A1,[Cd1]),
	atom_codes(A2,[Cd2]),
	findall(A, (between(Cd1,Cd2,N),atom_codes(A,[N])), Vs).

unmake_vars([Va,Vb]; Va-Vb).
unmake_vars([V|Vs]; V-T) :- unmake_vars(Vs;_-T).

do_unsome(Vrange, Xs, Xr, Ds, Vrange2) :- 
	make_vars(Vrange; Vs),
	unsome(Vs,[], Xs,Xr, Vs2, Ds),
        unmake_vars(Vs2; Vrange2).




%
% Type inference
%

stype(unit(C); K) :- class(C; K).
stype(some(C); K) :- class(C; K).

sclass(adj(_,C); C).
sclass(C; C) :- atom(C).

class(adj([],C); C).
class(adj([P|_],C); adj([P],C)).
class(adj([_|Ps],C); K) :- class(adj(Ps,C); K).
class(C; C) :- atom(C).

glb(top,C; C).
glb(C,top; C).
glb(C,D; G) :- findall(X, (subclass(X,C), subclass(X,D)), [G|_]).
lub([C|Cs]; L) :- lub(Cs; M), lub([C,M]; L).
lub([C,D]; L) :- findall(X, (subclass(C,X), subclass(D,X)), [L|_]).

getclass(K,lambda(_,Q); C) :- getclass(K,Q; C).
getclass(K,p(_,Q); C) :- getclass(K,Q; C).
getclass(K,F; C) :-
	arg(K,F,C), isclass(C). %%!; throw('not a class'(C))).


type(at(_,A); T) :- type(A; T).
type(aeq(A,B); ordering(amount(G))) :- type(A; amount(G)), type(B; amount(G)). %  glb(C,D; G).
type(lambda(_,Cn, A); Cn => T) :- type(A; T).
type(singleton(X); amount(C)) :- atom(X), isa(X,C).
type(p(_,C,_); amount(C)).
type(A; amount(D)) :- stype(A; D). % stype(A; C), subclass(C,D).
type(every(C); amount(C)).
type(times(_,A); T) :- type(A; T).
type(_ * A; T) :- type(A; T).
type(A + B; amount(C)) :- type(A; amount(Ca)), type(B; amount(Cb)), lub([Ca,Cb]; C).
type([]; _).
type([A|As]; amount(D)) :- type(A; amount(C1)), type(As; amount(C2)), lub([C1,C2]; D).
type(_ - A; T) :- type(A; T).
type(own(_,C); amount(C)) :- isclass(C).
type(own(_,A); T) :- type(A; T).
type(age(_); amount(year)).
type(isin(every(C),B); amount(C) => T) :- type(B; T). 
type(isin(A,every(C)); amount(C) => T) :- type(A; T). 
type(isin(A,B); T) :-
	isclass(A), T= amount(A);
	isclass(B), T = amount(B). 

subclass(A,A).
subclass(A,B) :- dsubclass(A,C), subclass(C,B).

decompose(Cy, p(X,Cx,isin(X,G)); compose(F,P)) :-
	P = p(Y,Cy,isin(Y,G)),
	F = lambda(Y,Cy, p(X,Cx,isin(X,Y))).



partition([],D) :- asserta(isclass(D)).
partition([C|Cs], D) :-
	asserta(dsubclass(C,D)),
	asserta(isclass(C)),
	partition(Cs,D).


%
% Examine structure
%

% A wrapped predicate:

ispredicate(p(_,_,_)).

% Amounts are either (wrapped) predicates or
% arithmetic combinations of 'unit' and 'some':

isamount(p(_,_,_)).
isamount(some(_)).
isamount(unit(_)).
isamount(A + B) :- isamount(A), isamount(B).
isamount(_ * A) :- isamount(A).

depth(aeq(A,B); N) :- depth(A;Na), depth(B;Nb), N is Na + Nb.
depth(A + B; N) :- sdepth(A + B; M), N is M + 1.
depth(_ * A; N) :- depth(A; M), N is M + 1.
depth(unit(_); 1).
depth(some(_); 0).
depth(p(_,_,_); 1).
sdepth(A + B; N) :- sdepth(A;Ma), sdepth(B;Mb), !, (Ma > Mb, !, N = Ma; N = Mb).
sdepth(A; N) :- depth(A; N).

% It is ground it does not contain numeric variables or 'some'.

% nonground(p(_,_,_)).
nonground(some(_)).
nonground(aeq(A,B)) :- nonground(A); nonground(B).
nonground(K * _) :- \+ number(K).
nonground(_ * A) :- nonground(A).
nonground(A + B) :- nonground(A); nonground(B).

% Core language forbids 'some' and bare predicates.

iscore(aeq(A,B)) :- iscore(A), iscore(B).
iscore(p(_,_,_)).
iscore(unit(_)).
iscore(_ * A) :- iscore(A).
iscore(A + B) :- iscore(A), iscore(B).

%% filter_core([]; []).
%% filter_core([E|Es]; Fss) :- filter_core(Es; Fs), (iscore(E), Fss=[E|Fs]; Fss=Fs).

% Well-formed sums of amounts are composed of distinct classes.

isdisjunctive(E) :- isdisjunctive(E,_),!.
isdisjunctive(_).
isdisjunctive(_ * A,Cs) :- isdisjunctive(A,Cs).
isdisjunctive(A + B,Cs) :-
	isdisjunctive(A,Cas),
	isdisjunctive(B,Cbs),!,
	intersection(Cas,Cbs,[]), append(Cas,Cbs,Cs).


% intersect([],_,[]).
% intersect([L|Ls],Ms,Ds) :- intersect(Ls,Ms,Cs), (member(L,Ms),!, Ds=[L|Cs]; Ds=Cs).


%
% Convert to core language
%

do_core(Vs,P,[P],[],Vs) :- P = dsubclass(_,_).
do_core(Vs,P,[P],[],Vs) :- P = istopclass(_).
do_core(Vrange,S, Cs, Defs, Vrange2) :-
	%findall(N, (member(S,Ss),normal(S;N)), Ns),
	normal(S;N),
	do_unsome(Vrange, N,Cs, Defs, Vrange2).

% normal(at(T,A); at(T, normal(A))).
normal(aeq(_,[]);_) :- fail.
normal(aeq(A,[B|_]); aeq(A,B)).
normal(aeq(A,[_|Bs]); E) :- normal(aeq(A,Bs); E).
% 'own' predicate
normal(find(own(O,Cl)); Pn) :-
	isclass(Cl),
	normal(own(O,some(Cl)); Pn).
normal(own(H,A); Ps) :-
	%check_normal(A; Ag),
	%type(A; amount(C)),
	full_disaggregate(A; DCs),
	findall(p(X,C,own(H,X)) - D,
			member(D - C, DCs),
			Ps).
% 'isin' predicate
normal(isin(A,every(D)); [P - Ag]) :-
	check_normal(A; Ag),type(Ag; amount(C)),
	P = lambda(Y,D,p(X,C,isin(X,Y))),!.
	%revar(P).
normal(isin(A,B); Ps) :-
	%check_normal(A; Ag), type(Ag; amount(C)),
	full_disaggregate(A; DCs),
	findall(p(X,C,isin(X,B)) - D, 
		member(D - C,DCs),
		Ps).
normal(isin(A,B); P - Ag) :-
	check_normal(B; Ag), type(Ag; amount(C)),
	P = p(Y,C,isin(A,Y)).
	%revar(P).
normal(find(isin(Cl,O)); Pn) :- 
	isclass(Cl),
	normal(isin(some(Cl),O); Pn).


unnormal(lambda(X,Cx,P) - A; N) :-
	X=every(Cx), unnormal(P - A; N).
unnormal(A - B; aeq(An,Bn)) :- 
	unnormal(A; An),
	unnormal(B; Bn).
unnormal(p(X,Cx,isin(X,L)); isin(Cx,L)) :- !.
unnormal(p(X,Cx,own(H,X)); own(H,Cx)) :- !.
unnormal(E; E).


revar(P) :- numbervars(P,23,24).

	
check_normal(A; Ag) :-
	isdisjunctive(A),!,disaggregate(A;Ag); 
	throw('not disjunctive'(A)).

%normal(own(_,[]);_) :- fail.
%normal(own(H,[A|As]); E) :- !, disaggregate([A|As]; B), normal(own(H,B); E).
%normal(own(H, +(A,B)); E) :- normal(own(H,A); E); normal(own(H,B); E),!.
% normal(own(H, +(_,B)); E) :- normal(own(H,B); E),!.
%normal(own(H,A); aeq(p(own,H,C),E)) :- normal(A; E),!, type(A;amount(C)); C = top.
%normal(E; E).


full_disaggregate(sumof(As); Ds) :-
	sum(As; An),
	findall(D - C,
		(disaggregate(An; D), type(D;amount(C))),
		Ds).
full_disaggregate(A; [A - C]) :- type(A; amount(C)).
disaggregate(A + B; C) :-
	disaggregate(A; Ag), C = Ag;
	disaggregate(B; Bg), C = Bg,!.
disaggregate(A; A).




%
% Find parts suitable to be replaced by a variable.
% Sort them by simplicity
%

definible(A) :- isamount(A), (nonground(A); ispredicate(A)).

definible_part(aeq(A,B); X) :- definible_part(A; X); definible_part(B; X).
definible_part(p(_,_,A); X) :- definible_part(A; X).
definible_part(A + B; X) :- definible_part(A; X); definible_part(B; X).
definible_part(_ * A; X) :- definible_part(A; X).
definible_part(A; A) :- definible(A).

find_definibles(A; Zs) :-
	findall((N-X), (definible_part(A; X),depth(X;N)), Xs),
	keysort(Xs,Zs).
find_must_define(A; Xs) :- findall(X, (definible_part(A;X),X=some(_)), Xs).
