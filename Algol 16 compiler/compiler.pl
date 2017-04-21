/*
W lekserze i parserze wykorzystalam kod while_parsera
*/

lexer(Tokens) -->
	skip,
	( ( ":=",		!, { Token = tokAssgn }
		; ";",		!, { Token = tokSColon }
		; ",",		!, { Token = tokComma }
		; "(",		!, { Token = tokLParen }
		; ")",		!, { Token = tokRParen }
		; "+",		!, { Token = tokPlus }
		; "-",		!, { Token = tokMinus }
		; "*",		!, { Token = tokTimes }
		; "=",		!, { Token = tokEq }
		; "<>",		!, { Token = tokNeq }
		; "<=",		!, { Token = tokLeq }
		; "<",		!, { Token = tokLt }
		; ">=",		!, { Token = tokGeq }
		; ">",		!, { Token = tokGt }
		; digit(D),	!,
				number(D, N),
				{ Token = tokNumber(N) }
		; letter(L), !, identifier(L, Id),
				{ member((Id, Token), [	(and, tokAnd),
										(begin, tokBegin),
										(call, tokCall),
										(div, tokDiv),
										(do, tokDo),
										(done, tokDone),
										(else, tokElse),
										(end, tokEnd),
										(fi, tokFi),
										(if, tokIf),
										(local, tokLocal),
										(mod, tokMod),
										(not, tokNot),
										(or, tokOr),
										(procedure, tokProcedure),
										(program, tokProgram),
										(read, tokRead),
										(return, tokReturn),
										(then, tokThen),
										(value, tokValue),
										(while, tokWhile),
										(write, tokWrite)]),
				!
				; Token = tokId(Id)
				}
		; [_],
				{ Token = tokUnknown }
		),
		!,
			{ Tokens = [Token | TokList] },
		lexer(TokList)
	; [],
			{ Tokens = [] }
	).

skip -->
	white_space,
	comment.

white_space -->
	[Char], { code_type(Char, space) }, !, white_space.
white_space -->
	[].
	
comment -->
	"(*", !, eoc, (white_space, comment).
comment -->
	[].
	
eoc -->
	( "*)", !
	; [_], eoc
	).

digit(D) -->
	[D],
		{ code_type(D, digit) }.

digits([D|T]) -->
	digit(D),
	!,
	digits(T).
digits([]) -->
	[].

number(D, N) -->
	digits(Ds),
		{ number_chars(N, [D|Ds]) }.

letter(L) -->
	[L],
		{ code_type(L, alpha) }.
	
alphanum([A|T]) -->
	[A],
		{ code_type(A, alnum)},!, alphanum(T).
alphanum([95|T]) -->
	[95],!, alphanum(T).
alphanum([39|T]) -->
	[39], !, alphanum(T).
alphanum([]) -->
	[].

identifier(L, Id) -->
	alphanum(As),
		{ atom_codes(Id, [L|As]) }.

identifier(L, Id) -->
	alphanum(As), !,
		{ atom_codes(Id, [L|As]) }.

program(Ast) -->
	[tokProgram], !, [tokId(_)], block(Block), 
		{ Ast = Block }.

block(Block) -->
	declarations(Decs), !, [tokBegin], complex_instruction(Instr), [tokEnd],
		{ flatten(Instr, NewInstr), flatten(Decs, NewDecs), Block = block(NewDecs, NewInstr) }.

complex_instruction(CInstr) -->
	instruction(Instr),
	( [tokSColon], !, complex_instruction(B),
		{CInstr = [Instr, B]}
	; [], {CInstr = Instr}
	).
	
instruction(Instr) -->
	( variable(Var), [tokAssgn], !, arith_expr(Expr),
		{ Instr =.. [assgn, Var, Expr] }
	; [tokIf], !, log_expr(Log), [tokThen], complex_instruction(ThenPart),
		( [tokElse], !, complex_instruction(ElsePart), [tokFi],
			{ flatten(Log, NewLog), flatten(ThenPart, NewThenPart), flatten(ElsePart, NewElsePart), Instr = if(NewLog, NewThenPart, NewElsePart) }
		; [tokFi],
			{flatten(Log, NewLog), flatten(ThenPart, NewThenPart),  Instr = if(NewLog, NewThenPart) }
		)
	; [tokWhile], !, log_expr(Log), [tokDo], complex_instruction(Body), [tokDone],
		{ flatten(Log, NewLog), flatten(Body, NewBody), Instr = while(NewLog, NewBody) }
	; [tokCall], !, procedure_calling(Call),
		{Instr = call(Call)}
	; [tokReturn], !, arith_expr(Expr),
		{Instr = return(Expr)}
	; [tokRead], !, variable(Var),
		{Instr = read(Var)}
	; [tokWrite], !, arith_expr(Expr),
		{Instr = write(Expr)}
	).
	
declarations(Decs) -->
	declaration(Dec), !, declarations(Dec, Decs)
	; [],
		{Decs = []}.
	
declarations(Acc, Decs) -->
	declaration(Dec),
		{ Acc1 = [Acc, Dec] }, declarations(Acc1, Decs).
declarations(Acc, Acc) -->
	[].
	
declaration(Dec) -->
	( declarator(D), !,
		{Dec = D}
	; procedure(Proc), !,
		{Dec = Proc}
	).
	
declarator(D) -->
	[tokLocal], !, variables(Vars),
		{D = Vars}.

variables(Vars) -->
	variable(Var),
	( [tokComma], !,
		{Vars = [Var| Rest]}, variables(Rest), !
	; [],
		{Vars = Var}
	).
	
variable(Var) -->
	[tokId(Id)],
		{ Var = var(Id) }.
		
procedure(Proc) -->
	[tokProcedure], !, procedureName(Name), [tokLParen], formalArguments(Args), [tokRParen], block(Block),
	{ Proc = procedure(Name, Args, Block) }.
	
procedureName(Name) -->
	[tokId(Id)],
		{ Name = Id }.
		
formalArguments(Args) -->
	( empty, !
	; formalArgumentsProgression(ArgsProg),
		{ Args = ArgsProg }
	).
	
formalArgumentsProgression(ArgsProg) -->
	formalArgument(Arg), formalArgumentsProgression(Arg, ArgsProg).
	
formalArgumentsProgression(Acc, ArgsProg) -->
	[tokComma], !, formalArgument(Arg),
		{ Acc1 =.. [Acc, Arg] }, formalArgumentsProgression(Acc1, ArgsProg).
formalArgumentsProgression(Acc, Acc) -->
	[].
	
formalArgument(Arg) -->
	( variable(Var), !,
		{Arg = Var}
	; [tokValue], !, variable(Var),
		{Arg = value(Var)}
	).



% ARYTMETYKA

arith_expr(NewExpr) -->
	summand(Summand), arith_expr(Summand, Expr), {flatten(Expr, NewExpr)}.

arith_expr(Acc, Expr) -->
	additive_op(Op), !, summand(Summand),
		{ Acc1 = [Acc, Summand, Op] }, arith_expr(Acc1, Expr).
arith_expr(Acc, Acc) -->
	[].

additive_op(plus) -->
	[tokPlus], !.
additive_op(minus) -->
	[tokMinus].
	
summand(Expr) -->
	factor(Factor), summand(Factor, Expr).

summand(Acc, Expr) -->
	multiplicative_op(Op), !, factor(Factor),
		{ Acc1 = [Acc, Factor, Op] }, summand(Acc1, Expr).
summand(Acc, Acc) -->
	[].

multiplicative_op(mult) -->
	[tokTimes], !.
multiplicative_op(divs) -->
	[tokDiv], !.
multiplicative_op(mods) -->
	[tokMod].
	
factor(Fact) -->
	( simple_expr(Expr), !,
		{ Fact = Expr }
	; [tokMinus], !, simple_expr(Expr),
		{ Fact = mins(Expr) }
	).

simple_expr(Expr) -->
	( atomic_expr(Atom), !,
		{Expr = Atom}
	;	[tokLParen], arith_expr(Expr), [tokRParen]
	).
	
atomic_expr(Atom) -->
	( variable(Atom), !
	; procedure_calling(Atom), !
	; [tokNumber(N)], !,
		{Atom = cons(N)}
	).
	
procedure_calling(Call) -->
	procedureName(Name), !, [tokLParen], actual_args(Args), [tokRParen],
		{Call = (Name, Args)}.
		
actual_args(Args) -->
	( empty, !
	; actual_args_progression(ArgsProg),
		{ Args = ArgsProg }
	).
	
actual_args_progression(ArgsProg) -->
	actual_arg(Arg), actual_args_progression(Arg, ArgsProg).
	
actual_args_progression(Acc, ArgsProg) -->
	[tokComma], !, actual_arg(Arg),
		{ Acc1 =.. [Acc, Arg] }, actual_args_progression(Acc1, ArgsProg).
actual_args_progression(Acc, Acc) -->
	[].
	
actual_arg(Arg) -->
	arith_expr(Arg).
	
log_expr(Log) -->
	conjunct(Conj), log_expr(Conj, Log).
	
log_expr(Acc, NewLog) -->
	[tokOr], !, conjunct(Conj), 
		{Acc1 = [Acc, Conj, or] }, log_expr(Acc1, Log), {flatten(Log, NewLog)}.
log_expr(Acc, Acc) -->
	[].
	
conjunct(Conj) -->
	condition(Con), conjunct(Con, Conj).
	
conjunct(Acc, NewConj) -->
	[tokAnd], !, condition(Con),
		{Acc1 = [Acc, Con, and] }, conjunct(Acc1, Conj), {flatten(Conj, NewConj)}.
conjunct(Acc, Acc) -->
	[].
		
condition(Con) -->
	( rel_expr(Rel), !,
		{Con = Rel}
	; [tokNot], !, rel_expr(Rel),
		{flatten(Rel, NewRel), Con = nots(NewRel)}
	).
	
rel_expr(Rel) -->
	( arith_expr(Expr1), !, rel_op(Op), arith_expr(Expr2),
		{Rel =.. [ Op, Expr1, Expr2] }
	;	[tokLParen], !, log_expr(Rel), [tokRParen]
	).

rel_op(eq) -->
	[tokEq], !.
rel_op(neq) -->
	[tokNeq], !.
rel_op(less) -->
	[tokLt], !.
rel_op(lesseq) -->
	[tokLeq], !.
rel_op(greater) -->
	[tokGt], !.
rel_op(greatereq) -->
	[tokGeq].
	
empty --> [] .


parse(CharCodeList, Ex) :-
	phrase(lexer(TokList), CharCodeList),
	phrase(program(Ex), TokList).

% asembler

nice_op(plus, add).
nice_op(minus, sub).
nice_op(mult, mul).
nice_op(divs, div).
rel_op(eq(_, _)) :- !.
rel_op(neq(_,_)) :- !.
rel_op(less(_,_)) :- !.
rel_op(greatereq(_,_)) :- !.
rel_op(greater(_,_)) :- !.
rel_op(lesseq(_,_)) :- !.

% Przetwarzanie wyrazen arytmetycznych
licz([], []).
licz([H | T], [New | E]) :-
	( H = var(X), 			!,	New = [const(var(X)), swapa, load, push]
	; H = cons(X), 			!,	New = [const(cons(X)), push]
	; H = mins(cons(X)),	!,	New = [const(cons(-X)), push]
	; nice_op(H, X),		!,	New = [pop, swapd, pop, X, push]
	; H = mods,				!,	New = [pop, swapd, pop, div, const(cons(-16)), swapd, shift, push]
	),
	licz(T, E).

% Przetwarzanie wyrazen relacyjnych - tu prawdopodobnie cos nie dziala
change_rel(A, E, Num, Num1) :-
	( A = eq(L, R),			!, licz(L, X), licz(R, Y),
		E = [X, Y, pop, swapd, const(et(eq, Num)), swapa, pop, sub, branchz, const(cons(0)), push, const(et(end, Num)), jump, et(eq, Num), const(cons(1)), push, et(end, Num)]
	; A = neq(L, R),		!, licz(L, X), licz(R, Y),
		E = [X, Y, pop, swapd, const(et(eq, Num)), swapa, pop, sub, branchz, const(cons(1)), push, const(et(end, Num)), jump, et(eq, Num), const(cons(0)), push, et(end, Num)]
	; A = less(L, R),		!, licz(L, X), licz(R, Y),
		E = [X, Y, const(et(a, Num)), swapa, pop, branchn, swapd, const(et(less, Num)), swapa, pop, branchn, sub, push, const(et(less, Num)), swapa, pop, branchn, const(et(nless, Num)), jump, et(a, Num), swapd, const(et(a2, Num)), swapa, pop, branchn, const(et(nless, Num)), jump, et(a2, Num), sub, push, const(et(less, Num)), swapa, pop, branchn, const(et(nless, Num)), jump, et(less, Num), const(cons(1)), push, const(et(end, Num)), jump, et(nless, Num), const(cons(0)), push, et(end, Num)]
	; A = greatereq(L, R),	!, licz(L, X), licz(R, Y),
		E = [X, Y, const(et(a, Num)), swapa, pop, branchn, swapd, const(et(less, Num)), swapa, pop, branchn, sub, push, const(et(less, Num)), swapa, pop, branchn, const(et(nless, Num)), jump, et(a, Num), swapd, const(et(a2, Num)), swapa, pop, branchn, const(et(nless, Num)), jump, et(a2, Num), sub, push, const(et(less, Num)), swapa, pop, branchn, const(et(nless, Num)), jump, et(less, Num), const(cons(0)), push, const(et(end, Num)), jump, et(nless, Num), const(cons(1)), push, et(end, Num)]
	; A = greater(L, R),	!, licz(L, X), licz(R, Y),
		E = [X, Y, const(et(a, Num)), swapa, pop, branchn, swapd, const(et(false, Num)), swapa, pop, branchn, push, et(x, Num), pop, sub, push, const(cons(-1)), swapd, pop, mul, branchn, const(et(false, Num)), jump, et(a, Num), swapd, const(et(true, Num)), swapa, pop, branchn, push, const(et(x, Num)), jump, et(true, Num), const(cons(1)), push, const(et(end, Num)), jump, et(false, Num), const(cons(0)), push, et(end, Num)]
	; A = lesseq(L, R),	!, licz(L, X), licz(R, Y),
		E = [X, Y, const(et(a, Num)), swapa, pop, branchn, swapd, const(et(true, Num)), swapa, pop, branchn, push, et(x, Num), pop, sub, push, const(cons(-1)), swapd, pop, mul, branchn, const(et(true, Num)), jump, et(a, Num), swapd, const(et(false, Num)), swapa, pop, branchn, push, const(et(x, Num)), jump, et(true, Num), const(cons(1)), push, const(et(end, Num)), jump, et(false, Num), const(cons(0)), push, et(end, Num)]
	), Num1 is Num + 1.

% Przetwarzanie wyrazen logicznych - tu tez cos nie dziala
change_log([], [], N, N).
change_log([H | T], [New | Ex], Num, ExNum) :-
	( rel_op(H),			!, change_rel(H, E, Num, Num2), 
		New = [E],
		NumE is Num2+1
	; H = and,				!,
		New = [pop, swapd, pop, mul, push],
		NumE is Num + 1
	; H = or,				!,
		New = [pop, swapd, pop, add, push],
		NumE is Num + 1
	; H = nots(X),			!, Num1 is Num + 1, change_log(X, Y, Num1, NumE),
		New = [Y, const(et(true, Num)), swapa, pop, branchz, const(cons(0)), push, const(et(end, Num)), jump, et(true, Num), const(cons(1)), push, et(end, Num)]
	),
	change_log(T, Ex, NumE, ExNum).

% Przetwarzanie ciagow instrukcji - w niewielkiej czesci powinno dzialac
change_instr([],[], N, N).
change_instr([H | T], [New | Ex], Num, ExNum) :-
	( H = assgn(A, B),		!,	licz(B, E),
		New = [E, pop, swapd, const(A), swapa, swapd, store],
		NumE is Num + 1
	; H = if(A, B),			!,	Num1 is Num + 1, change_log(A, X, Num1, Num2), Num3 is Num2 + 1, change_instr(B, Y, Num3, NumE),
		New = [X, pop, swapd, const(et(false, Num)), swapa, swapd, branchz, Y, et(false, Num)] % wynk change_log(A, X) na stosie
	; H = if(A, B, C),		!,	Num1 is Num + 1, change_log(A, X, Num1, Num2), Num3 is Num2 + 1, change_instr(B, Y, Num3, Num4), Num5 is Num4 + 1, change_instr(C, Z, Num5, NumE),
		New = [X, pop, swapd, const(et(false, Num)), swapa, swapd, branchz, Y, const(et(end, Num)), jump, et(false, Num), Z, et(end, Num)]
	; H = while(A, B),		!,	Num1 is Num+1, change_log(A, X, Num1, Num2), Num3 is Num2 + 1, change_instr(B, Y, Num3, NumE), 
		New = [et(jump, Num), X, pop, swapd, const(et(end, Num)), swapa, swapd, branchz, Y, const(et(jump, Num)), jump, et(end, Num)]
	; H = read(A),			!,
		New = [const(A), swapa, const(cons(1)), syscall, store],
		NumE is Num + 1
	; H = write(A),			!, licz(A, X),
		New = [X, pop, swapd, const(cons(2)), syscall],
		NumE is Num + 1
	),
	change_instr(T, Ex, NumE, ExNum).

% Przetwarzanie wczesniej wystepujacych pushow i popow
change_stack([],[]).
change_stack([H | T], [New | Ex]) :-
	( H = pop,				!,
		New = [const(cons(65534)),swapa,load,swapd,swapa,const(cons(1)),add,swapa,swapd,const(cons(65534)),swapa, store,swapd,swapa,const(cons(1)),swapd,sub,swapa,swapd,load]
	; H = push,				!,
		New = [swapd,const(cons(65534)),swapa,load,swapd,swapa,const(cons(1)),swapd,sub,swapa,store,const(cons(65534)),swapa,store]
	; New = H
	),
	change_stack(T, Ex).

% Dokladanie instrukcji "nic nie rob" przed etykietami i na koncu programu, aby uzupelnic do pelnych czworek, jednoczesne przypisywanie adresow etykietom i "wyciaganie" ich z listy
nops([], New, Num, _, E, E, _) :-
	N is Num mod 4, nop(N, L),
		New = L.
nops([H | T], [New | Ex], Num, ConsNum, EtAd, ExEtAd, Ad) :-
	( H = et(X, Y),			!, N is Num mod 4, nop(N, L),
		New = L, Num1 is 0, NewConsNum is 0,
		(N = 0,				!, NewAd is (Num // 4) + ConsNum + Ad
		; !, NewAd is (Num // 4) + ConsNum + 1 + Ad
		),
	  append(EtAd, [(et(X, Y), NewAd)], NewEtAd)
	; H = const(Z),			!, Num1 is Num + 1, NewConsNum is ConsNum + 1,
		New = const(Z), NewEtAd = EtAd, NewAd = Ad
	; !, New = H, Num1 is Num + 1, NewConsNum is ConsNum, NewEtAd = EtAd, NewAd = Ad
	),
	nops(T, Ex, Num1, NewConsNum, NewEtAd, ExEtAd, NewAd).

nop(0, []).
nop(3, [nop]).
nop(2, [nop, nop]).
nop(1, [nop, nop, nop]).

% Tworzenie lis adresow zmiennych na podstawie dlugosci programow i ilosci constow
varDict([], [], _, List, List, _).

varDict([V | Vars], [], ConstCounter, List, ExList, Len) :-
	Ad is (Len / 4) + ConstCounter,
	NewConstCounter is ConstCounter + 1,
	append(List, [(V, Ad)], NewList),
	varDict(Vars, [], NewConstCounter, NewList, ExList, Len).

varDict(Vars, [H | ComList], ConstCounter, List, ExList, Len) :-
	( H = const(_),			!,
		NewConstCounter is ConstCounter + 1
	; NewConstCounter is ConstCounter),
	varDict(Vars, ComList, NewConstCounter, List, ExList, Len).

% Dokladanie wartosci contow na koniec pelnej czworki 
divToFour([], [], _, _).
divToFour([Com | Commnds], [New | Ex], Counter, ConstList) :-
	( Com = const(X),		!,
		append(ConstList, [X], TempConstList),
		Temp is 9
	; TempConstList = ConstList, command(Com, Num), Temp = Num),
	TempCounter is Counter + 1,
	( TempCounter = 4, 		!,
		New = [Temp, TempConstList],
		NewConstList = [],
		NewCounter = 0
	;	New = Temp,
		NewConstList = TempConstList,
		NewCounter = TempCounter
	),
	divToFour(Commnds, Ex, NewCounter, NewConstList).

% Tworzenie listy liczb dziesietnych
trans_commnds([],[], _).
trans_commnds([Com | Commnds], [New | Ex], Dic) :-
	( Com = cons(X),		!,
		( X < 0,			!,	New is 32768 + (32768 + X)
		; 						New is X)
	; Com = var(X),			!, member((var(X), Y), Dic),
								New = Y
	; Com = et(N, Num),		!, member((et(N, Num), Y), Dic),
								New = Y
	),
	trans_commnds(Commnds, Ex, Dic).
trans_commnds([A, B, C, D | Commnds], [New | Ex], Dic) :-
	New is A*16*16*16+B*16*16+C*16+D,
	trans_commnds(Commnds, Ex, Dic).

begin(AST, Vars, Commnds) :-
	AST = block(Vars, Commnds).

% Zlozenie poprzednich predykatow
assembly(AST, NumList) :-
	begin(AST, Vars, Commnds),
	change_instr(Commnds, List, 0, _),
	flatten(List, FlattenedList),
	append([const(cons(65534)), swapa, const(cons(65533)), store], FlattenedList, NewFlattenedList),
	append(NewFlattenedList, [const(cons(0)), syscall], NewFlattenedList2),
	%write(NewFlattenedList2),
	change_stack(NewFlattenedList2, StackList),
	flatten(StackList, FlattenedStackList),
	nops(FlattenedStackList, NopsList, 0, 0, [], E, 0),
	flatten(NopsList, FlattenedNopsList),
	%write(FlattenedNopsList),
	length(FlattenedNopsList, Len),
	varDict(Vars, FlattenedNopsList, 0, E, ExList, Len),
	%write(ExList),
	divToFour(FlattenedNopsList, OkList, 0, []),
	flatten(OkList, FlattenedOkList), 
	trans_commnds(FlattenedOkList, NumList, ExList),
	 !.

command(nop, 0).
command(syscall, 1).
command(load, 2).
command(store, 3).
command(swapa, 4).
command(swapd, 5).
command(branchz, 6).
command(branchn, 7).
command(jump, 8).
command(const, 9).
command(add, 10).
command(sub, 11).
command(mul, 12).
command(div, 13).
command(shift, 14).

% Predykat wlasciwy
algol16(CharCodeList, SextiumBin) :-
	parse(CharCodeList, Ex),
	assembly(Ex, SextiumBin).
