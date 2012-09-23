:- multifile rate/3.

card(A, N) :- app(countable,A, N).

nub([],[]).
nub([A|As], Bs) :- member(A,As),!,nub(As,Bs).
nub([A|As], [A|Bs]) :- nub(As, Bs).

app(F,aeqs(As), neqs(Cos)) :-
	app(F,As, Bs),
	nub(Bs,Cs), length(Cs,N), N >= 2, sort(Cs,Cos).
app(_, [], []).
app(F, [A|As], [N|Ns]) :-
	app(F,A, N), app(F,As, Ns).
app(F, A + B, N1 + N2) :- app(F,A,N1), app(F,B,N2).
app(F, K * A, N) :- app(F, A, N1), simplify(K*N1;N).
app(F, unit(C), R) :- rate(F,C, R).
app(_,some(C),_) :- throw('app on some'(C)).
app(F, P, N) :- P = p(_,_,_),
	P - A, app(F, A, N).

app(F, compose(L,P), M) :-
	app(F,L, K),
	app(countable,P, N),
	simplify(K*N; M).
app(F, L, N) :- L = lambda(_,_,_),
	L - A, app(F,A, N).

rate(countable,_, 1) :- !. % Assume all classes are countable!
rate(C, D, 1) :- subclass(D,C),!.
rate(C, D, 0) :- subclass(C,D), C \= D, throw('invalid rate'(D,C)); true.




flatten_sum(A + B; Cs) :-
	(flatten_sum(A; As); As = [A]),
	(flatten_sum(B; Bs); Bs = [B]),
	append(As,Bs, Cs).

unflatten_sum([X]; X).
unflatten_sum([X|Xs]; N + X) :-
	unflatten_sum(Xs; N).

simplify(neqs(Es); neqs(Fs)) :-
	simplify(Es;Fs),!.
simplify([];[]).
simplify([N|Ns]; Ss) :-
	simplify(N; Nr),
	simplify(Ns; Nsr),
	(member(Nr,Nsr), Ss=Nsr;  Ss=[Nr|Nsr]).
simplify(N; Nl + Lv) :- N = _ + _, 
	flatten_sum(N; Ns),
	findall(U, (member(T,Ns),simplify(T;U)), Us),
	findall(T, (member(T,Us),number(T)), Literals),
	findall(T, (member(T,Us), \+ number(T)), NotLiterals),
	unflatten_sum(Literals; L), Lv is L,
	unflatten_sum(NotLiterals; Nl).
simplify(K * A; O) :-
	simplify(A; Ar),
	simplify(K;Kr),
	one_simplify(Kr,Ar, O),!.
simplify(N; N).
zero_simplify(N,0,N).
zero_simplify(0,N,N).
zero_simplify(N,M,N + M).
one_simplify(_,0, 0).
one_simplify(K,1, K).
one_simplify(1,K, K).
one_simplify(K,L, K*L).

