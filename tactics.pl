convert_to_core(Vrange,Fs) :- 
	findall(P,
		(member(F/N,Fs), functor(P,F,N), P), Ps),
	do_core(Vrange, Ps,Ns, _),
	findall(N, (member(N,Ns), asserta(N)), _).



write_goal :- 
	prolog_current_frame(Frame),
	prolog_frame_attribute(Frame,parent,PFrame),
	prolog_frame_attribute(PFrame,clause,Value),
	write(Value).


auto(Nos) :-
	Tactics = [partite_tactic, decompose_tactic],
	findall(N , 
		(member(Tac,Tactics), P - A, call(Tac, aeqs([P,A]), E), card(E,N)),
		Ns),
	sort(Ns, Nos).

findone(R, Q) :-
	findall(R, R - _, [Q|_]).


partite_tactic(aeqs([A,B]), aeqs([Ap,Bp])) :-
	(partite_by_class(A, Ap); Ap=A),
	(partite_by_class(B, Bp); Bp=B).

partite_by_class(p(X,C,P), A) :-
	setof(Csub,
		(subclass(Csub,C), Csub\=C),
		Csubs),
	findall(Q,
		(member(D,Csubs), findone(p(X,D,P),Q)),
		 Ps),
	length(Ps,N), N > 0 -> hint_at(partite_by_class(p(X,C,P),Ps)),
	sum(Ps;A).

partite_by_elems(p(X,C,P),  S) :-
	findall(X, (isa(X,C), P - _),  Xs),
	hint_at(partite_by_elems(C)),
	sum(Xs; S).


decompose_tactic(P, B) :- P = p(_,_,_),
	findall(C, decompose_by(P,C), Cs),
	lub(Cs;D),
	decompose(D,P; compose(_,Q)),
	partite_by_class(Q,A), unsum(A;As),
	findall(L,
		(member(B,As), L=compose(_,B), decompose(_,P; L)),
		Bs),
	%hint_at(partite_by_class(Q)),
	sum(Bs; B).

decompose_tactic(aeqs(As), aeqs(Bs)) :-
	findall(Bd, (member(B,As), (decompose_tactic(B,Bd); Bd=B)), Bs).


unzip([],[],[]).
unzip([A - B|ABs], [A|As], Bss) :- 
	unzip(ABs, As, Bs), append(B,Bs,Bss).

decompose_by(P,C) :-
	decompose(C,P; compose(F,Q)),
	F - _, Q - _, type(Q;amount(C)),
	hint_at(decompose_by(F,Q)).




hint_at(P) :- recordz(hint, P).

