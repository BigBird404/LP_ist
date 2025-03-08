% 106103- Filippo da Costa Bortoli
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.

%evento(ID, NomeDisciplina, Tipologia, NumAlunos, Sala)

%turno(ID, SiglaCurso, Ano, NomeTurma)

%horario(ID, DiaSemana, HoraInicio, HoraFim, Duracao, Periodo)

% 3.1---------------------------------------------------------------------

/*Este predicado verifica se Eventos e uma lista ordenada e
sem elementos repetidos de IDs de eventos sem sala */
eventosSemSalas(Eventos) :-
   findall(ID, evento(ID,_,_,_,'semSala'), Eventos).

/*Este predicado verifica se Eventos e uma lista ordenada e
sem elementos repetidos de IDs de eventos sem sala que decorrem em
DiaDaSemana(segunda-feira,terca-feira...) */
eventosSemSalasDiaSemana(DiaDaSemana,Eventos):-
    findall(ID,(horario(ID,DiaDaSemana,_,_,_,_),evento(ID,_,_,_,'semSala')),
            Eventos).

/*Este predicado verifica se Eventos e uma lista ordenada e
sem elementos repetidos de IDs de eventos sem sala que decorrem nos periodos na
lista ListaP */
eventosSemSalasPeriodo(LstP,Eventos):-
   eventosSemSalasPeriodo(LstP,Eventos,[]).

eventosSemSalasPeriodo([],Eventos,Eventos).

eventosSemSalasPeriodo([H|T],EventosF,Eventos):-
   findall(ID,(auxEP([H|T],ID),evento(ID,_,_,_,'semSala')),EventosNaoF),
   append(EventosNaoF,Eventos,TempNovoEventos),
   sort(TempNovoEventos,NovoEventos),
   eventosSemSalasPeriodo(T,EventosF,NovoEventos).

/*Este predicado auxiliar serve para quando se procuram IDs de discplinas
num determinado periodo se considerarem tambem as disciplinas semestrais */
auxEP(LstP,ID):-
   member(p1,LstP),
   (horario(ID,_,_,_,_,p1);horario(ID,_,_,_,_,p1_2));
   member(p2,LstP),
   (horario(ID,_,_,_,_,p2);horario(ID,_,_,_,_,p1_2));
   member(p3,LstP),
   (horario(ID,_,_,_,_,p3);horario(ID,_,_,_,_,p3_4));
   member(p4,LstP),
   (horario(ID,_,_,_,_,p4);horario(ID,_,_,_,_,p3_4)).

% 3.2---------------------------------------------------------------------
/*Este predicado verifica se EventosNoPer e uma lista ordenada e
sem elementos repetidos de IDs de eventos que pertencem a lista LstEventos
e que decorrem no Periodo Per */
organizaEventos(LstEventos,Per,EventosNoPer):-
   organizaEventos(LstEventos,Per,EventosNoPer,[]).

organizaEventos([],_,ENPer,ENPer).

organizaEventos([H|T],Per,ENPerFinal,ENPer):-
   (auxEP([Per],H)->append(ENPer,[H],TempNovoENPer),
    sort(TempNovoENPer,NovoENPer);
   organizaEventos(T,Per,ENPerFinal,ENPer)),
   organizaEventos(T,Per,ENPerFinal,NovoENPer).

/*Este predicado verifica se LstEventos e uma lista ordenada e
sem elementos repetidos de IDs de eventos que tem uma duracao menor que Dur*/
eventosMenoresQue(Dur,LstEventos):-
   findall(ID,(horario(ID,_,_,_,Tempo,_),Tempo =< Dur),LstEventos).

/*Este predicado verifica se o evento com ID ID tem uma duracao menor ou igual
que Dur*/
eventosMenoresQueBool(ID,Dur):-
   horario(ID,_,_,_,Tempo,_),
   Tempo =< Dur.

/*Este predicado verifica se LstD e uma lista ordenada alfabeticamente
do nome das disciplinas do curso Curso. */
procuraDisciplinas(Curso,LstD):-
   findall(NomeD,(turno(ID,Curso,_,_),evento(ID,NomeD,_,_,_)),TempLstD),
   sort(TempLstD,LstD).

/*Este predicado verifica se Semestres e uma lista com 2 listas,
ordenadas e sem elementos repetidos, sendo a primeira lista as
disciplinas da lista LD do Curso Curso do 1 semestre,a segunda
lista e o mesmo para o 2 semestre*/
organizaDisciplinas(LD,Curso,Semestres):-
   organizaDisciplinas(LD,Curso,Semestres,[[],[]]).

organizaDisciplinas([],_,Semestres,Semestres).

organizaDisciplinas([H|T],Curso,SemF,[S1,S2]):-
   (evento(ID,H,_,_,_),turno(ID,Curso,_,_)->horario(ID,_,_,_,_,P) ;
   organizaDisciplinas(T,Curso,SemF,[S1,S2])),
   ((P==p1;P==p2;P==p1_2)->append(S1,[H],TempNS1),sort(TempNS1,NS1),
    append([NS1],[S2],NSem);append(S2,[H],TempNS2),sort(TempNS2,NS2),
    append([S1],[NS2],NSem)),
   organizaDisciplinas(T,Curso,SemF,NSem).

/*Este predicado verifica se TotalHoras e a soma das duracoes das disciplinas do
curso Curso no periodo Per e no ano Ano */
horasCurso(Per,Curso,Ano,TotalHoras):-
   findall(ID,(turno(ID,Curso,Ano,_),auxEP([Per],ID)),TempLstID),
   sort(TempLstID,LstID),
   findall(Dur,(member(ID,LstID),horario(ID,_,_,_,Dur,_)),LstD),
   sumlist(LstD,TotalHoras).

/*Este predicado verifica se Evolcao e uma lista, ordenada por ordem cronologica,
de tuplos em que cada tuplo corresponde a um periodo de um ano do curso
Curso em que o primeiro elemento do tuplo corresponde ao ano, o segundo ao
periodo e o terceiro as horas do curso */
evolucaoHorasCurso(Curso, Evolucao):-
   horasCurso(p1,Curso,1,TH1),
   horasCurso(p2,Curso,1,TH2),
   horasCurso(p3,Curso,1,TH3),
   horasCurso(p4,Curso,1,TH4),
   horasCurso(p1,Curso,2,TH5),
   horasCurso(p2,Curso,2,TH6),
   horasCurso(p3,Curso,2,TH7),
   horasCurso(p4,Curso,2,TH8),
   horasCurso(p1,Curso,3,TH9),
   horasCurso(p2,Curso,3,TH10),
   horasCurso(p3,Curso,3,TH11),
   horasCurso(p4,Curso,3,TH12),
   Evolucao = [(1,p1,TH1),(1,p2,TH2),(1,p3,TH3),(1,p4,TH4),
(2,p1,TH5),(2,p2,TH6),(2,p3,TH7),(2,p4,TH8),
(3,p1,TH9),(3,p2,TH10),(3,p3,TH11),(3,p4,TH12)].


% 3.3---------------------------------------------------------------------
/*Este predicado verifica se Horas e o numero de horas coincidentes entre o slot
dado por HoraInicioDada e HoraFimDada e as horas em que ira decorrer o evento,
HoraInicioEvento e HoraFimEvento */
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas):-
   HoraFimEvento>HoraInicioDada,
   HoraInicioEvento<HoraFimDada,
   (HoraInicioDada>HoraInicioEvento->Min is HoraInicioDada;
   Min is HoraInicioEvento),
   (HoraFimDada<HoraFimEvento->Max is HoraFimDada;Max is HoraFimEvento),
   Horas is Max-Min.

/*Este predicado auxiliar serve para quando se procuram IDs de discplinas
num determinado periodo e dia da semana se considerarem tambem as disciplinas
semestrais */
auxEPDS(LstP,ID,DS):-
   member(p1,LstP),
   (horario(ID,DS,_,_,_,p1);horario(ID,DS,_,_,_,p1_2));
   member(p2,LstP),
   (horario(ID,DS,_,_,_,p2);horario(ID,DS,_,_,_,p1_2));
   member(p3,LstP),
   (horario(ID,DS,_,_,_,p3);horario(ID,DS,_,_,_,p3_4));
   member(p4,LstP),
   (horario(ID,DS,_,_,_,p4);horario(ID,DS,_,_,_,p3_4)).

/*Este predicado serve para verificar se SH e a soma das horas em que as salas do
tipo TipoSala,no periodo Per, no dia da semana DS e entre a HI e HF estao
ocupadas*/
numHorasOcupadas(Per, TipoSala, DS, HI, HF, SH):-
   salas(TipoSala,LSalasTipoDado),
   findall(ID,(auxEPDS([Per],ID,DS),evento(ID,_,_,_,Salas),
               member(Salas,LSalasTipoDado)),LIDs),
   nhoAux(LIDs,HI,HF,SH).

nhoAux(LIDs,HI,HF,SH):-
   nhoAux(LIDs,HI,HF,SH,0).

nhoAux([],_,_,SH,SH).

nhoAux([H|T],HI,HF,SH,SHTemp):-
   horario(H,_,HIR,HFR,_,_),
   (ocupaSlot(HI,HF,HIR,HFR,Horas)->
   (NovoSHTemp is SHTemp + Horas,
   nhoAux(T,HI,HF,SH,NovoSHTemp));
   nhoAux(T,HI,HF,SH,SHTemp)).

/*Este predicado serve para verificar se Max e o numero maximo de horas possiveis
de ser ocupadas por salas do tipo TipoSala entre HoraInicio e HoraFim */
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max):-
   salas(TipoSala,LstSalas),
   length(LstSalas,Len),
   Max is (HoraFim-HoraInicio)*Len.

/*Este predicado serve para verificar se percentagem e o valor da divisao de
SomaHoras por Max e multiplicado por 100 */
percentagem(SomaHoras, Max, Percentagem):-
   Percentagem is (SomaHoras/Max)*100.

/*Este predicado serve para verificar se Res e uma lista ordenada e sem elementos
repetidos de tuplos do tipo casosCriticos(DS,TS,Percent), sendo DS o dia
da semana, Ts o tipo de sala e Percent a percentagem, entre HI e HF e
com uma percentagem de ocupacao(Percent) maior que o threshold.*/
ocupacaoCritica(HI,HF,Threshold,Res):-
   findall(casosCriticos(DS,TS,Percent),(
               horario(ID,DS,_,_,_,Per),
               evento(ID,_,_,_,Sala),
               salas(TS, LstSalas),
               member(Sala,LstSalas),
               numHorasOcupadas(Per,TS, DS, HI, HF, SH),
               ocupacaoMax(TS,HI,HF,Max),
               percentagem(SH,Max,TempPercent),
               ceiling(TempPercent,Percent),
               Percent>Threshold),TempRes),
   sort(TempRes,Res).

 % 3.4---------------------------------------------------------------------
/*
Pontos pelo esforco e pelas 11724712238210938 tentativas que nao estao
aqui? :)
*/
/*
ocupacaoMesa(Pessoas, Restricoes, Ocupacao) :-
  permutation(Pessoas, PessoasPermutadas),

  (   verificaRestricoes(Restricoes,PessoasPermutadas)
  ->  divide(PessoasPermutadas, L1, [C1, C2], L3),
      Ocupacao = [L1, [C1, C2], L3]
  ).


divide([A, B, C, D, E, F, G, H], [A, B, C], [D, E], [F, G, H]).
*/

