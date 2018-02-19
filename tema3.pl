valnum(X):- char_type(X, alnum), char_type(X, ascii).
vother(X):- member(X, [';','<','+','-','*','(',')','{','}']).
validc(X):- valnum(X) ; vother(X) ;  X == '='.

lparseq(['='|L],'==',L).
lparseq([X|L],'=',[X|L]):-dif(X,'=').
lparseq([],'=',[]).

lparsealn([X|L],L2,R,L3):- valnum(X), lparsealn(L, [X|L2], R, L3).
lparsealn([X|L],L2,R,[X|L]):- \+valnum(X), reverse(L2, L3), atom_chars(R, L3).
lparsealn([],L2,R,[]):- reverse(L2, L3), atom_chars(R, L3).

lparse2(['='|L],L2,L3):- lparseq(L,R,L4), lparse2(L4,[R|L2],L3).
lparse2([X|L],L2,L3):- valnum(X),lparsealn(L,[X],R,L4), lparse2(L4,[R|L2],L3).
lparse2([X|L],L2,L3):- vother(X), lparse2(L,[X|L2],L3).
lparse2([X|L],L2,L3):- \+validc(X), lparse2(L,L2,L3).
lparse2([],L2,L3):- reverse(L2,L3).

lparse(S, L):- atom_chars(S, L2), lparse2(L2,[],L),!.

%parsam lista primita si evaluam rezultatul parsarii, reprezentat de o lista de prog-uri;
%daca evaluarea are loc cu succes se intoarce rezultatul obitnut
%daca se intalneste un assert care esueaza pe parcursul evaluarii se intoarce 'a'
parseInputAux(L,R):- parse_aux(L, [], T, _), eval_expr_list(T, R, [], _, 0), !.
parseInputAux(L,R):- parse_aux(L, [], T, _), eval_expr_list(T, R, [], _, 1), R = 'a', !.

parseInput(F,R):-read_file_to_string(F,S,[]), lparse(S,L), parseInputAux(L,R), !.

%predicat care realizeaza evaluarea unei liste de porgrame;
%primul parametru = lista de programe; al doilea parametru = rezultatul evaluarii, al treilea parametru = lista de variabile
%din program inainte de evaluarea curenta, al patrulea parametru = lista de variabile dupa evaluarea curenta;
%al cincelea parametru: 1 daca s-a intalnit un assert fals is 0 altfel.
%evaluarea listei vide nu intoarce un rezultat anume;
%evaluarea unei liste cu un singur prog va returna rezultatul intors de evaluarea prog-ului respectiv;
%evaluarea unei liste cu cel putin doua elemente (daca lista ar fi avut un singur element s-ar fi intrat pe cazul anterior) consta in
%evaluarea primului element, verificarea rezultatului si evaluarea restului listei prin apelul recursiv al functiei de evaluare pentru lista
%ramasa:
%daca rezultatul intors este 'a', atunci evaluarea continua, dar se semnaleaza aparitia ei prin setarea ultimului parametru drept 1 si a 
%rezultatului intors drept 'a';
%daca nu apare nicio eroare ultimul parametru va fi setat drept 0 si rezultatul intors de etapa curenta de evaluare va fi rezultatul intors
%de apelul recursiv.
eval_expr_list([], _, VarsBefore, VarsBefore, _) :- !.
eval_expr_list([H], Res, VarsBefore, VarsAfter, _) :- eval(H, Res, VarsBefore, VarsAfter), !.
eval_expr_list([H|T], Res, VarsBefore, VarsAfterF, _) :- eval(H, R, VarsBefore, VarsAfter), not(R == 'a'), eval_expr_list(T, Res, VarsAfter, VarsAfterF, 0), !.
eval_expr_list([H|T], R, VarsBefore, VarsAfterF, _) :- eval(H, R, VarsBefore, VarsAfter), R == 'a', eval_expr_list(T, R, VarsAfter, VarsAfterF, 1), !.

%predicat care realizeaza parsarea unei liste de tokeni prin apelul functiilor de parsare corespunzatoare fiecarui tip de prog;
%primul parametru = lista de parsat, al doilea parametru = lista de programe care au fost deja parsate, al treilea parametru = lista
%de programe incluzand programul nou parsat, al patrulea parametru = lista ramasa de parsat;
% 1) daca se ajunge la o lista care incepe cu ; atunci parsarea se operste, deoarece am ajuns la finalul unei instructiuni
% 2) incercam sa parsam un program de tip assign. Aceasta parsare este impartita pe mai multe cazuri:
	%a) am parsat un prog de tip assign, mai avem cel putin doua elemente in lista ramasa in urma parsarii, primul element din
	%lista ramasa este ;, vom adauga noul prog parsat la lista celor deja existente pentru a crea noua lista a programelor parsate.
	%Daca al doilea element din lista ramasa este { sau } atunci am ajuns la finalul corpului unui if sau unui for, deci nu mai are rost
	%sa continuam recursiv parsarea (acesta este singurul caz cand am putea sa dam de acolade in program). 
	%Daca al doilea element din lista ramasa nu este o acolada atunci vom continua parsarea printr-un apel recursiv pe lista ramasa.
	%b)am parsat un prog de tip assign, mai avem un singur element in lista ramasa in urma parsarii, ;. Adaugam noul program parsat la
	%lista programelor parsate si se incheie parsarea listei curente.
%3) parsarea unui program de tip assert: identica cu parsarea programelor de assign, se apeleaza predicatul parse_assert_expr in loc 
%de predicatul parse_assign pentru parsarea programului propriu-zis.
%4) incercam sa parsam un programul de tip if prin predicatul parse_if_expr. Indifernt de lista ramasa, se va adauga programul nou parsat 
%la lista programelor deja existente. Se evalueaza lista ramasa in urma parsarii:
	%a) lista este nevida, dar primul ei element este }, sau },deci am ajuns la finalul sau la inceputul unui nou bloc, asa ca nu mai parsam in continuare.
	%b) lista este nevida si primul element este diferit de { si }, deci vom continua parsarea programelor ce urmeaza.
	%c) lista este vida, asa ca doar adauga noul rezultat si parsarea nu mai poate continua.
%5) parsarea unui program for cu ajutorul predicatului parse_for_expr: identica cu parsarea unui if.
%6) parsarea unui program return cu ajutorul predicatului parse_return_expr: se parseaza programul return, se verfica ca lista ramasa
%sa inceapa cu ; si se adauga noul program in lista programelor existente.
parse_aux(L, Parsed, NewParsed, T) :- parse_assign(L, O, R), R = [H|T], H == ;, not(T == []) , append(Parsed, [O], NewParsed),
head(T, E), E == '}', !.
parse_aux(L, Parsed, NewParsed, T) :- parse_assign(L, O, R), R = [H|T], H == ;, not(T == []) , append(Parsed, [O], NewParsed),
head(T, E), E == '{', !.
parse_aux(L, Parsed, NewParsed2, Res) :- parse_assign(L, O, R), R = [H|T], H == ;, not(T == []) , append(Parsed, [O], NewParsed),
head(T, E), E \= '}', E \= '{', parse_aux(T, NewParsed, NewParsed2,Res), !.
parse_aux(L, Parsed, NewParsed, R) :- parse_assign(L, O, R), R == [;], append(Parsed, [O], NewParsed),!.
parse_aux(L, Parsed, NewParsed, T) :- parse_assert_expr(L, O, R), R = [H|T], H == ;, not(T == []) , append(Parsed, [O], NewParsed),
head(T, E), E == '}', !.
parse_aux(L, Parsed, NewParsed, T) :- parse_assert_expr(L, O, R), R = [H|T], H == ;, not(T == []) , append(Parsed, [O], NewParsed),
head(T, E), E == '{', !.
parse_aux(L, Parsed, NewParsed2, Res) :- parse_assert_expr(L,O,R), R = [H|T], H == ;, not(T == []),  append(Parsed, [O], NewParsed),
head(T, E), not(E == '}'), not(E == '{'), !,parse_aux(T, NewParsed, NewParsed2,Res), !.
parse_aux(L, Parsed, NewParsed, R) :- parse_assert_expr(L, O, R), R == [;], append(Parsed, [O], NewParsed),!.
parse_aux(L, Parsed, NewParsed, R) :- parse_if_expr(L, O, R),  R \= [] , append(Parsed, [O], NewParsed), head(R, E), E == '}', !.
parse_aux(L, Parsed, NewParsed, R) :- parse_if_expr(L, O, R), R \= [] , append(Parsed, [O], NewParsed),head(R, E), E == '{', !.
parse_aux(L, Parsed, NewParsed2, Res) :- parse_if_expr(L, O, R), R \= [], append(Parsed, [O], NewParsed),
head(R, E), E \= '}', E \= '{', parse_aux(R, NewParsed, NewParsed2,Res), !.
parse_aux(L, Parsed, NewParsed, R) :- parse_if_expr(L, O, R), R == [], append(Parsed, [O], NewParsed),!.
parse_aux(L, Parsed, NewParsed, R) :- parse_for_expr(L, O, R),  not(R == []) , append(Parsed, [O], NewParsed), head(R, E), E == '}', !.
parse_aux(L, Parsed, NewParsed, R) :- parse_for_expr(L, O, R), R \= [] , append(Parsed, [O], NewParsed),head(R, E), E == '{', !.
parse_aux(L, Parsed, NewParsed2, Res) :- parse_for_expr(L, O, R), R \= [], append(Parsed, [O], NewParsed),
head(R, E), E \= '}', E \= '{', parse_aux(R, NewParsed, NewParsed2,Res), !.
parse_aux(L, Parsed, NewParsed, R) :- parse_for_expr(L, O, R), R == [], append(Parsed, [O], NewParsed),!.
parse_aux(L, Parsed, NewParsed, T) :- parse_return_expr(L, O, R), R = [;|T], append(Parsed, [O], NewParsed),!.

%<cod curs>:
%parser pentru o litera
parse_alpha([L|T], L, T):- atom_chars(L, [H|_]), char_type(H, alpha).
%parser pentru o cifra
parse_int([L|T], L, T) :- atom_chars(L, [E|_]), char_type(E, digit).
%parser pentru un caracter dat
parse_char(C,[C|T], C, T).
%apeleaza parserul p de 0 sau mai multe ori
star(P, L, [F | FR], RF) :- call(P, L, F, R), star(P, R, FR, RF),!.
star(_,L,[],L).
%apeleaza parserul p cel putin o data
plus(P, L, [F|FR], RF) :- call(P,L,F, R), star(P, R, FR, RF).
%parseaza o variabila folosind parserul de litere si parserul plus si concateneaza literele rezultate intr-un string,
%reprezentand variabila parsata
parse_var(L, V, R) :- plus(parse_alpha, L, Op, R), atomic_list_concat(Op, O), atom_string(O, V).
%parseaza o valoare folosind parserul de cifre si parserul plus si concateneaza cifrele rezultate intr-un numar,
%reprezentand valoarea parsata
parse_val(L, N, R) :- plus(parse_int, L, Op, R), atomic_list_concat(Op, O), atom_number(O, N).
%parseaza o variabila sau o valoare
parse_tok(L,O,R):- parse_var(L,O,R);parse_val(L,O,R).
%</cod cors>

%parser pentru ==
parse_eq([L|T], L, T):- atom_chars(L, [H|_]), member(H, [==| _]), !.
%parser pentru <
parse_sm([L|T], L, T):- atom_chars(L, [H|_]), member(H, [<|_]), !.
%parser pentru {, }, (, ).
parse_par([L|T], L, T):- atom_chars(L, [H|_]), member(H, ['(', ')', '{', '}']), !.
%primul element al unei liste
head([H|_], H).

%parseaza o expresie de tip expresie == expresie (eq);
%fiecare expresie poate fi de tipul: adunare, inmultire, scadere, variabila sau valoare;
% L = lista de tokeni, O = obiectul parsat, R = lista ramasa;
% functionare: parsam o expresie , verificam lista ramasa incepe cu == (expresia parsata este prima expresie din eq) sau 
% cu ) (expresia parsata este a doua din eq) si  returnam rezultatul parsarii expresiei.
parse_expr_eq(L,O,R) :- parse_add_expr(L,O1,R1), head(R1, H), =(H, ==), O = O1, R = R1, !.
parse_expr_eq(L,O,R) :- parse_sub_expr(L,O1,R1), head(R1, H), =(H, ==), O = O1, R = R1, !.
parse_expr_eq(L,O,R) :- parse_var(L, O1, R1), head(R1, H), =(H, ==) , O = O1, R = R1, !. 
parse_expr_eq(L,O,R) :- parse_val(L, O1, R1), head(R1, H), =(H, ==), O = O1, R = R1, !. 
parse_expr_eq(L,O,R) :- parse_add_expr(L,O1,R1), head(R1, H), =(H, ')'), O = O1, R = R1, !.
parse_expr_eq(L,O,R) :- parse_sub_expr(L,O1,R1), head(R1, H), =(H, ')'), O = O1, R = R1, !.
parse_expr_eq(L,O,R) :- parse_var(L, O1, R1), head(R1, H), =(H, ')') , O = O1, R = R1, !. 
parse_expr_eq(L,O,R) :- parse_val(L, O1, R1), head(R1, H), =(H, ')'), O = O1, R = R1, !. 

%parseaza o expresie de tip expresie < expresie (smaller);
%fiecare expresie poate fi de tipul: adunare, inmultire, scadere, variabila sau valoare;
% L = lista de tokeni, O = obiectul parsat, R = lista ramasa;
% functionare: parsam o expresie , verificam lista ramasa incepe cu < (expresia parsata este prima expresie din smaller) sau 
% cu ) (expresia parsata este a doua din smaller) si  returnam rezultatul parsarii expresiei.
parse_expr_sm(L,O,R) :- parse_add_expr(L,O1,R1), head(R1, H), =(H, <), O = O1, R = R1, !.
parse_expr_sm(L,O,R) :- parse_sub_expr(L,O1,R1), head(R1, H), =(H, <), O = O1, R = R1, !.
parse_expr_sm(L,O,R) :- parse_var(L, O1, R1), head(R1, H), =(H, <) , O = O1, R = R1, !. 
parse_expr_sm(L,O,R) :- parse_val(L, O1, R1), head(R1, H), =(H, <), O = O1, R = R1, !. 
parse_expr_sm(L,O,R) :- parse_add_expr(L,O1,R1), head(R1, H), =(H, ')'), O = O1, R = R1, !.
parse_expr_sm(L,O,R) :- parse_sub_expr(L,O1,R1), head(R1, H), =(H, ')'), O = O1, R = R1, !.
parse_expr_sm(L,O,R) :- parse_var(L, O1, R1), head(R1, H), =(H, ')') , O = O1, R = R1, !. 
parse_expr_sm(L,O,R) :- parse_val(L, O1, R1), head(R1, H), =(H, ')'), O = O1, R = R1, !. 

%parser facut la curs
%parseaza o expresie de tip expresie1 * expresie2 (mult);
%expresiie pot fi o variabila o variabila, o valoare, o alta inmultire.
% L = lista de tokeni, X = expresie1, Y = expresie2, R = lista ramasa;
% obiectul parsat este de tip mult(X,Y) pentru a simbolliza faptul ca cele doua expresii vor trebui sa fie inmultite
% functionare: parsam o variabila sau o valoare(expresie1), parsam semnul de inmultite dintre expresii, parsam recursiv
% a doua valoare pentru a include si alte inmultiri legate de cea curenta
%caz de baza: parsam o variabila sau o valoare
parse_mult_expr(L,mult(X,Y),R) :- parse_tok(L,X,R1), parse_char('*',R1,_,R2), parse_mult_expr(R2, Y, R), !.
parse_mult_expr(L,O,R) :- parse_tok(L,O,R).

%parseaza o expresie de tip expresie1 - expresie2 (sub);
%expresiile pot fi o inmultire (care include si cazul variabila/valoare si are prioritate mai mare decat scaderea)
%o alta scadere, sau o adunare, care are aceeasti proritate cu scaderea;
%L = lista de tokeni, X = expresie1, Y = expresie2, R = lista ramasa;
%obiectul parsat este de tip sub(X,Y) pentru a simbolliza faptul ca cele doua expresii vor trebui sa fie scazute
%functionare: parsam o inmultire, parsam semnul de scadere dintre expresii, parsam o noua scadere recursiv;
%caz de baza: parsam o adunare (nu am gasit nici inmultiri nici alte scaderi).
parse_sub_expr(L, sub(X,Y), R) :- parse_mult_expr(L, X, R1), parse_char('-', R1, _, R2), parse_sub_expr(R2, Y, R), !.
parse_sub_expr(L,O,R) :- parse_mult_expr(L,O,R).

%parseaza o expresie de tip expresie1 + expresie2 (add);
%expresiile pot fi o inmultire (care include si cazul variabila/valoare si are prioritate mai mare decat adunarea)
%o alta adunare, sau o scadere, care are aceeasti proritate cu adunarea;
%L = lista de tokeni, X = expresie1, Y = expresie2, R = lista ramasa;
%obiectul parsat este de tip add(X,Y) pentru a simbolliza faptul ca cele doua expresii vor trebui sa fie adunate
%functionare: parsam o inmultire, parsam semnul de adunare dintre expresii, parsam o noua adunare recursiv;
%caz de baza: parsam o scadere (nu am gasit nici inmultiri nici alte adunari).
parse_add_expr(L, add(X,Y), R) :- parse_sub_expr(L, X, R1), parse_char('+', R1, _, R2), parse_add_expr(R2, Y, R), !.
parse_add_expr(L,O,R) :- parse_sub_expr(L,O,R).

%parseaza o expresie oarecare: o adunare sau o scadere (implicit includ si cazul de inmultire sau simple variabile / valori)
%L = lista de tokeni de parsat, O = obiectul parsat, R = restul listei.
%verificam ca expresiile parsate sa se termine in ; sau ).
parse_expr(L,O,R) :- parse_add_expr(L,O1,R1), head(R1, H), H == ;, O = O1, R = R1, !.
parse_expr(L,O,R) :- parse_sub_expr(L,O1,R1), head(R1, H), H == ;, O = O1, R = R1, !.
parse_expr(L,O,R) :- parse_add_expr(L,O1,R1), head(R1, H), H == ')', O = O1, R = R1, !.
parse_expr(L,O,R) :- parse_sub_expr(L,O1,R1), head(R1, H), H == ')', O = O1, R = R1, !.

%parseaza un prog de tip return prin parsarea expresiei componente.
%L = lista de tokeni, fara return-ul de inceput. X = expresia din return, R = lista ramasa dupa parsarea return-ului
%return-ul de inceput poate avea ; in fata daca au fost si alte instructiuni inainte.
%pentru parsarea expresiei se foloseste predicatul parse_expr.
parse_return_expr([return| L], return(X), R) :- parse_expr(L, X, R).
parse_return_expr([;, return| L], return(X), R) :- parse_expr(L, X, R).

%parseaza un program de tip assert prin parsarea expresiei componente.
%L = lista de tokeni, fara assert-ul de inceput. X = expresia din assert, R = lista ramasa dupa parsare
%assert-ul de inceput poate avea ; in fata daca au fost si alte instructiuni inainte.
%se parseaza prima paranteza care marcheaza inceputul expresiei de parsat.
%pentru parsarea expresiei se folosesc predicatele parse_eq_expr si parse_smaller_expr, pentru ca X este obligatoriu o expresie
%booleana daca se afla in interiorul unui assert.
%se parseaza paranteza care inchide expresia de parsat.
parse_assert_expr([assert| L], assert(X), R) :- parse_par(L, H, R1), H == '(', parse_eq_expr(R1, X, R2), parse_par(R2, _, R), !.
parse_assert_expr([assert| L], assert(X), R) :- parse_par(L, H, R1), H == '(', parse_smaller_expr(R1, X, R2), parse_par(R2, _, R), !.
parse_assert_expr([;, assert| L], assert(X), R) :- parse_par(L, H, R1), H == '(', parse_eq_expr(R1, X, R2), parse_par(R2, _, R), !.
parse_assert_expr([;, assert| L], assert(X), R) :- parse_par(L, H, R1), H == '(', parse_smaller_expr(R1, X, R2), parse_par(R2, _, R), !.

%parseaza o expresie de tip eq: expresie1 == expresie2.
%L = lista de tokeni X = expresie1, Y = expresie2, R = lista ramasa dupa parsare
%se parseaza expresie1 folosind parse_expr_eq, se parseaza semnul de ==  dintre cele doua expresii si se parseaza si expresie2
%folosind tot parse_expr_eq.
parse_eq_expr(L, eq(X,Y), R) :- parse_expr_eq(L, X, R1), parse_eq(R1, O, R2), =(O, ==), parse_expr_eq(R2, Y, R), !.
%parseaza o expresie de tip smaller: expresie1 < expresie2.
%L = lista de tokeni X = expresie1, Y = expresie2, R = lista ramasa dupa parsare
%se parseaza expresie1 folosind parse_expr_sm, se parseaza semnul de < dintre cele doua expresii si se parseaza si expresie2
%folosind tot parse_expr_sm.
parse_smaller_expr(L, smaller(X,Y), R) :- parse_expr_sm(L, X, R1), parse_sm(R1, O, R2), =(O, <), parse_expr_sm(R2, Y, R), !.

%parseaza un program de tip if prin parsarea expresiilor componente.
%L = lista de tokeni, fara if-ul de inceput. Cond = conditia pentru if, Then = programul de executat daca conditia este adevarata,
%Else = prog de executat daca conditia este falsa, R = lista ramasa dupa parsare;
%if-ul de inceput poate avea ; in fata daca au fost si alte instructiuni inainte.
%se parseaza prima paranteza care marcheaza inceputul expresiei de parsat.
%pentru parsarea expresiei se folosesc predicatele parse_eq_expr si parse_smaller_expr, pentru ca Cond este obligatoriu o expresie
%booleana.
%se parseaza paranteza care inchide conditia si se preia drept lista urmatoare lista fara then-ul de la inceput, pentru a evita inca o parsare.
%se parseaza paranteza de la inceputul then-ului
%se parseaza programele din interiorul ramurii then folosind parse_aux; then va fii deci o lista de programe;
%se parseaza paranteza care inchide then-ul, si se preia lista urmatoare fara else-ul de inceput;
%se parseaza paranteza de la inceputul else-ului
%se parseaza programele din interiorul ramurii else folosind parse_aux si se parseaza paranteza de la finalul else-ului.
parse_if_expr([if | L], if(Cond, Then, Else), R) :- parse_par(L, _, R1), parse_eq_expr(R1, Cond, R2), parse_par(R2, _, [then | R3]), parse_par(R3, _, R4),
parse_aux(R4,[], Then, R5), parse_par(R5, _, [else | R6]), parse_par(R6, _, R7), parse_aux(R7, [], Else, R8), parse_par(R8, _, R), !. 
parse_if_expr([;, if | L], if(Cond, Then, Else), R) :- parse_par(L, _, R1), parse_eq_expr(R1, Cond, R2), parse_par(R2, _, [then | R3]), parse_par(R3, _, R4),
parse_aux(R4,[], Then, R5), parse_par(R5, _, [else | R6]), parse_par(R6, _, R7), parse_aux(R7, [], Else, R8), parse_par(R8, _, R), !. 
parse_if_expr([if | L], if(Cond, Then, Else), R) :- parse_par(L, _, R1), parse_smaller_expr(R1, Cond, R2), parse_par(R2, _, [then | R3]), parse_par(R3, _, R4),
parse_aux(R4,[], Then, R5), parse_par(R5, _, [else | R6]), parse_par(R6, _, R7), parse_aux(R7, [], Else, R8), parse_par(R8, _, R), !. 
parse_if_expr([;, if | L], if(Cond, Then, Else), R) :- parse_par(L, _, R1), parse_smaller_expr(R1, Cond, R2), parse_par(R2, _, [then | R3]), parse_par(R3, _, R4),
parse_aux(R4,[], Then, R5), parse_par(R5, _, [else | R6]), parse_par(R6, _, R7), parse_aux(R7, [], Else, R8), parse_par(R8, _, R), !. 

%parseaza un program de tip for prin parsarea expresiilor componente.
%L = lista de tokeni, fara for-ul de inceput. Asgn1 = primul assign din for, Cond = conditia pentru for, Asgn2 = pasul din for,
%Prog = prog de executat daca conditia este adevarata, R = lista ramasa dupa parsare;
%for-ul de inceput poate avea ; in fata daca au fost si alte instructiuni inainte.
%se parseaza prima paranteza care marcheaza inceputul expresiei de parsat.
%se parseaza primul assign folosind parse_assign_for si se elimina ; din lista ramasa;
%se parseaza conditia, care este o expresie booleana, deci de tip smaller sau eq, folosind parserele parse_eq_expr_for si parse_smaller_expr_for. si se elimina
%; din lista ramsa;
%se parseaza al doilea assign din for(pasul) folosind parse_assign_for;
%se parseaza paranteza de la finalul for-ului si acolada de la inceputul programului de executat;
%se parseaza programul folosind parse_aux;
%se parseaza ultima acolada;
parse_for_expr([for | L], for(Asgn1, Cond, Asgn2, Prog), R) :- parse_par(L, _, R1), parse_assign_for(R1, Asgn1, [;|R2]), parse_eq_expr_for(R2, Cond, [;|R3]),
parse_assign_for(R3, Asgn2, R4), parse_par(R4, _, R5), parse_par(R5, _, R6), parse_aux(R6,[], Prog, R7), parse_par(R7,_,R), !.
parse_for_expr([for | L], for(Asgn1, Cond, Asgn2, Prog), R) :- parse_par(L, _, R1), parse_assign_for(R1, Asgn1, [;|R2]), parse_smaller_expr_for(R2, Cond, [;|R3]),
parse_assign_for(R3, Asgn2, R4), parse_par(R4, _, R5), parse_par(R5, _, R6), parse_aux(R6,[], Prog, R7), parse_par(R7,_,R), !.
parse_for_expr([;, for | L], for(Asgn1, Cond, Asgn2, Prog), R) :- parse_par(L, _, R1), parse_assign_for(R1, Asgn1, [;|R2]), parse_eq_expr_for(R2, Cond, [;|R3]),
parse_assign_for(R3, Asgn2, R4), parse_par(R4, _, R5), parse_par(R5, _, R6), parse_aux(R6,[], Prog, R7), parse_par(R7,_,R), !.
parse_for_expr([;, for | L], for(Asgn1, Cond, Asgn2, Prog), R) :- parse_par(L, _, R1), parse_assign_for(R1, Asgn1, [;|R2]), parse_smaller_expr_for(R2, Cond, [;|R3]),
parse_assign_for(R3, Asgn2, R4), parse_par(R4, _, R5), parse_par(R5, _, R6), parse_aux(R6,[], Prog, R7), parse_par(R7,_,R), !.

%pentru a retine variabilele din program am folosit liste de perechi de tipul (Variabila, Valoare);
%predicat pentru a obitne valoarea corespunzatoare variabilei X
search_val(X, [(X, V) | _], V):- !.
search_val(X, [_| T], R) :- search_val(X, T, R), !. 

%predicat pentru a parsa un program de tip assign pentru for;
%diferit de parse_assign prin faptul ca un assign din for se poate incheia si printr-o ), daca este al doilea assign, nu doar prin ;.
parse_assign_for(L, assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_val(R2, O2, R), head(R, H), H == ;, !.
parse_assign_for(L, assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_expr(R2, O2, R), head(R, H), H == ;, !.
parse_assign_for([;|L], assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_val(R2, O2, R), head(R, H), H == ;, !.
parse_assign_for([;|L], assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_expr(R2, O2, R), head(R, H), H == ;, !.
parse_assign_for(L, assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_val(R2, O2, R), head(R, H), H == ')', !.
parse_assign_for(L, assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_expr(R2, O2, R), head(R, H), H == ')', !.
parse_assign_for([;|L], assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_val(R2, O2, R), head(R, H), H == ')', !.
parse_assign_for([;|L], assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_expr(R2, O2, R), head(R, H), H == ')', !.

%predicat pentru a parsa un program de tip eq pentru for;
%diferit de parse_expr_eq prin faptul ca un eq din for se poate doar prin ;.
parse_expr_eq_for(L,O,R) :- parse_add_expr(L,O1,R1), head(R1, H), =(H, ==), O = O1, R = R1, !.
parse_expr_eq_for(L,O,R) :- parse_sub_expr(L,O1,R1), head(R1, H), =(H, ==), O = O1, R = R1, !.
parse_expr_eq_for(L,O,R) :- parse_var(L, O1, R1), head(R1, H), =(H, ==) , O = O1, R = R1, !. 
parse_expr_eq_for(L,O,R) :- parse_val(L, O1, R1), head(R1, H), =(H, ==), O = O1, R = R1, !. 
parse_expr_eq_for(L,O,R) :- parse_add_expr(L,O1,R1), head(R1, H), =(H, ;), O = O1, R = R1, !.
parse_expr_eq_for(L,O,R) :- parse_sub_expr(L,O1,R1), head(R1, H), =(H, ;), O = O1, R = R1, !.
parse_expr_eq_for(L,O,R) :- parse_var(L, O1, R1), head(R1, H), =(H, ;) , O = O1, R = R1, !. 
parse_expr_eq_for(L,O,R) :- parse_val(L, O1, R1), head(R1, H), =(H, ;), O = O1, R = R1, !. 

%predicat pentru a parsa un program de tip smaller pentru for;
%diferit de parse_sm_expr prin faptul ca un smaller din for se poate incheia doar prin ;.
parse_expr_sm_for(L,O,R) :- parse_add_expr(L,O1,R1), head(R1, H), =(H, <), O = O1, R = R1, !.
parse_expr_sm_for(L,O,R) :- parse_sub_expr(L,O1,R1), head(R1, H), =(H, <), O = O1, R = R1, !.
parse_expr_sm_for(L,O,R) :- parse_var(L, O1, R1), head(R1, H), =(H, <) , O = O1, R = R1, !. 
parse_expr_sm_for(L,O,R) :- parse_val(L, O1, R1), head(R1, H), =(H, <), O = O1, R = R1, !. 
parse_expr_sm_for(L,O,R) :- parse_add_expr(L,O1,R1), head(R1, H), =(H, ;), O = O1, R = R1, !.
parse_expr_sm_for(L,O,R) :- parse_sub_expr(L,O1,R1), head(R1, H), =(H, ;), O = O1, R = R1, !.
parse_expr_sm_for(L,O,R) :- parse_var(L, O1, R1), head(R1, H), =(H, ;) , O = O1, R = R1, !. 
parse_expr_sm_for(L,O,R) :- parse_val(L, O1, R1), head(R1, H), =(H, ;), O = O1, R = R1, !. 

%parseaza un program eq pentru un for folosind parse_eq_expr_for
parse_eq_expr_for(L, eq(X,Y), R) :- parse_expr_eq_for(L, X, R1), parse_eq(R1, O, R2), =(O, ==), parse_expr_eq_for(R2, Y, R), !.
%parseaza un program smaller pentru un for folosind parse_smaller_expr_for
parse_smaller_expr_for(L, smaller(X,Y), R) :- parse_expr_sm_for(L, X, R1), parse_sm(R1, O, R2), =(O, <), parse_expr_sm_for(R2, Y, R), !.
%parseaza un program de tip assign
%L = lista de tokeni, O1 = variabila, O2 = valoare/expresie, R = lista ramasa in urma parsarii
%poate aparea ; la inceput daca au mai fost alte instructiuni inainte;
%se parseaza variabila;
%se parseaza atribuirea (semnul =);
%se parseaza o valoare sau o expresie si se verifica sa se termine in ;.
parse_assign(L, assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_val(R2, O2, R), head(R, H), H == ;, !.
parse_assign(L, assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_expr(R2, O2, R), head(R, H), H == ;, !.
parse_assign([;|L], assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_val(R2, O2, R), head(R, H), H == ;, !.
parse_assign([;|L], assign(O1, O2), R) :- parse_var(L, O1, R1), parse_char('=', R1, _, R2), parse_expr(R2, O2, R), head(R, H), H == ;, !.

%evalueaza un program
%parametru1 = program, parametru12 = rezultat, parametru13 = lista de variabile inainte de evaluare;
%parametru14 = lista de variabile dupa evaluare;
%1) add(X, Y) : evalueaza X, evalueaza Y, aduna rezultatele celor doua evaluari. lista de variabile nu se poate schimba
%deoarece nu pot aparea assign-uri in interiorul expresiilor dintr-un add;
%2)	sub(X,Y) : evalueaza X, evalueaza Y, scade rezultatele celor doua evaluari. lista de variabile nu se poate schimba
%deoarece nu pot aparea assign-uri in interiorul expresiilor dintr-un sub;
%3)	mult(X,Y) : evalueaza X, evalueaza Y, inmulteste rezultatele celor doua evaluari. lista de variabile nu se poate schimba
%deoarece nu pot aparea assign-uri in interiorul expresiilor dintr-un mult;
%4) return(X) : evalueaza X si intoarce rezultatul; nu se pot produce schimbari in lista de variabile;
%5)	assign(X, Y) : evalueaza expresia Y; adauga perechea formata din X si rezultatul evaluarii la lista de variabile si da mai
%departe noua lista. Listele de variabile nu se pot modidica.
%6)	eq(X,Y) : evalueaza X, evalueaza Y, daca rezultatele celor doua evaluari sunt egale returneaza 1, altfel returneaza 0.
%7) smaller(X, Y) : evalueaza X, evalueaza Y, daca rezultatul primei evaluari este mai mic decat al celei de-a doua atunci returneaza 
% 1, altfel returneaza 0. Listele de variabile nu se pot modidica.
%8)	assert(X) : evalueaza X. Daca rezultatul este 1, atunci nu se schimba nimic (returneaza 1). Daca rezultatul este 0 returneaza 'a'. 
%Listele de variabile nu se pot modidica.
%9) if(Cond, Then, Else): evalueaza Cond: daca este adevarata (intoarce 1) atunci evalueaza Then, altfel evalueaza Else. Evaluarea se 
% realizeaza folosind eval_expr_list, deoarece Then si Else sunt liste de programe. Listele se pot modifica la evaluarea programelor
%din Else sau Then, deci se va mai departe lista rezultata in urma evaluarii ramurii corespunzatoare.
%10) for(Asgn1, Cond, Asgn2, Prog): evalueaza primul assign si conditia: daca este adevarata, atunci evalueaza Prog folosind
% eval_expr_list (este o lista de programe) si apeleaza recursiv functia de evaluare pentru a simula repetarea buclei. Lista de variabile
% se poate modidica la evaluarea lui Prog si a assign-urilor, deci se va da mereu mai departe lista nou calculata. In recursivitate se 
% Asgn1 si Asgn2 vor fi aceleasi, deoarece Asgn2 reprezinta "pasul" din for, iar Asgn1 se executa o singura data,la intrarea in for.
% Deci, pentru a simula executia pasului acesta, trebuie sa fie evaluat mereu inainte de a verifica conditia de la iteratia 2 incolo.
% Daca conditia este falsa se returneaza rezultatul assign-ului.
%11) pentru o valoare se verifica daca este numar si se returneaza direct
%12) pentru o variabila se verifica daca este string si se returneaza rezultatul cautarii ei in lista de variabile.
eval(add(X, Y), R, VarsBefore, VarsBefore) :- eval(X, R1, VarsBefore, VarsBefore), eval(Y, R2, VarsBefore, VarsBefore), R is R1 + R2, !.
eval(sub(X, Y), R, VarsBefore, VarsBefore) :- eval(X, R1, VarsBefore, VarsBefore), eval(Y, R2, VarsBefore, VarsBefore), R is R1 - R2, !.
eval(mult(X, Y), R, VarsBefore, VarsBefore) :- eval(X, R1, VarsBefore, VarsBefore), eval(Y, R2, VarsBefore, VarsBefore), R is R1 * R2, !.
eval(return(X), R, VarsBefore, VarsBefore) :- eval(X, R, VarsBefore, VarsBefore), !.
eval(assign(X, Y), R, VarsBefore, VarsAfter2) :- eval(Y, R, VarsBefore, _), append([(X,R)], VarsBefore, VarsAfter2),!.
eval(eq(X,Y), R, VarsBefore, VarsBefore) :- eval(X, R1, VarsBefore, VarsBefore), eval(Y, R2, VarsBefore, VarsBefore), R1 == R2, R = 1, !.
eval(eq(X,Y), R, VarsBefore, VarsBefore) :- eval(X, R1, VarsBefore, VarsBefore), eval(Y, R2, VarsBefore, VarsBefore), R1 \= R2, R = 0, !.
eval(smaller(X,Y), R, VarsBefore, VarsBefore) :- eval(X, R1, VarsBefore, VarsBefore), eval(Y, R2, VarsBefore, VarsBefore), R1 < R2, R = 1, !.
eval(smaller(X,Y), R, VarsBefore, VarsBefore) :- eval(X, R1, VarsBefore, VarsBefore), eval(Y, R2, VarsBefore, VarsBefore), R1 >= R2, R = 0, !.
eval(assert(X), R, VarsBefore, VarsBefore) :- eval(X, R, VarsBefore, VarsBefore), R == 1, !.
eval(assert(X), R, VarsBefore, VarsBefore) :- eval(X, R1, VarsBefore, VarsBefore), R1 == 0, R = 'a', !.
eval(if(Cond, Then, _), R, VarsBefore, VarsAfter) :- eval(Cond, R1, VarsBefore, VarsBefore), R1 == 1, eval_expr_list(Then, R, VarsBefore, VarsAfter, 0),!.
eval(if(Cond, _, Else), R, VarsBefore, VarsAfter) :- eval(Cond, R1, VarsBefore, VarsBefore), R1 == 0, eval_expr_list(Else, R, VarsBefore, VarsAfter, 0),!.
eval(for(Asgn1, Cond, Asgn2, Prog), R, VarsBefore, VarsAfterF) :- eval(Asgn1, _, VarsBefore, VarsAfter1), eval(Cond, R1, VarsAfter1, VarsAfter1), R1 == 1,
eval_expr_list(Prog, _, VarsAfter1, VarsAfter2, 0), eval(for(Asgn2, Cond, Asgn2, Prog), R, VarsAfter2, VarsAfterF), !.
eval(for(Asgn1, Cond, _, _), R, VarsBefore, VarsAfter) :-  eval(Asgn1, R, VarsBefore, VarsAfter),eval(Cond, R1, VarsAfter, VarsAfter), R1 == 0, !.
eval(C, C, _, _) :- number(C), !.
eval(Var, R, VarsBefore, _) :- string(Var), search_val(Var, VarsBefore, R), !.