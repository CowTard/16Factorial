:-use_module(library(lists)).
:-use_module(library(random)).

tabuleiro([
		[[ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ', ' ', ' '] ]],
		[[ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ', ' ', ' '] ]],
		[[ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ', ' ', ' '] ]],
		[[ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ',' ',' '] ], [ [' ',' ',' '],[' ',' ',' '],[' ', ' ', ' '] ]]
	]).

padroesDisponiveis([
		  [ ['o','o','o'],['o','.','o'],['o','o','o'] ], [ ['o','o','o'],['o','.','.'],['o','o','o'] ], [ ['o','o','o'],['o','.','.'],['.','o','o'] ], [ ['o','o','o'],['o','.','.'],['o','.','o'] ],
		  [ ['o','o','o'],['o','.','o'],['.','.','.'] ], [ ['o','o','o'],['.','.','o'],['.','o','o'] ], [ ['o','o','o'],['o','.','.'],['o','o','.'] ], [ ['o','o','o'],['o','.','.'],['o','.','.'] ],
		  [ ['o','o','o'],['o','.','.'],['.','.','.'] ], [ ['o','o','o'],['.','.','o'],['.','.','.'] ], [ ['o','o','o'],['.','.','.'],['.','.','.'] ], [ ['o','o','.'],['o','.','.'],['.','.','.'] ],
		  [ ['.','o','o'],['.','.','.'],['.','.','.'] ], [ ['o','o','.'],['.','.','.'],['.','.','.'] ], [ ['o','.','.'],['.','.','.'],['.','.','.'] ], [ ['.','o','.'],['.','.','.'],['.','.','.'] ]
		  ]).

iniciar:-
	tabuleiro(TabuleiroJogo),
	padroesDisponiveis(PadroesDisponiveis),
	write(' =============================================================='),nl,
	write(' PRETENDE JOGAR CONTRA O COMPUTADOR(1) OU PLAYER VS PLAYER(2) ?'),nl,
	write(' =============================================================='),nl,
	get_char(OpcaoJogoChar), get_char(_), number_chars(OpcaoJogo, [OpcaoJogoChar]), nl,
	(OpcaoJogo =:= 1
		->jogarCmp(TabuleiroJogo,PadroesDisponiveis,0)
		; jogar(TabuleiroJogo,PadroesDisponiveis,0)
	).

jogar(TabuleiroJogo, TabuleiroPadrao, NumeroJogador):-
		limparEcra(0),
		write('=============================================================='),nl,
		write('====================== A JOGAR COMO JOGADOR '), write(NumeroJogador), write(' ================'),nl,
		write('=============================================================='),nl,nl,nl,nl,
		escolherColocarPadrao(TabuleiroJogo, TabuleiroPadrao, NovoTabuleiro, NovoTabuleiroPadrao),
		verificarFimJogo(NovoTabuleiro,NovoTabuleiroPadrao, FimJogo),
		(FimJogo =\= 0
			-> mudarJogador(NumeroJogador,NovoNumeroJogador),jogar(NovoTabuleiro,NovoTabuleiroPadrao,NovoNumeroJogador) 
			; imprimirTabuleiro(NovoTabuleiro),nl,nl,nl,write('=================   Jogador '), write(NumeroJogador), write(' ganhou! ================='), nl
		).

escolherColocarPadrao(TabuleiroJogo, TabuleiroPadrao, NovoTabuleiro, NovoTabuleiroPadrao):-
	imprimirTabuleiro(TabuleiroJogo), nl,nl,nl,nl,
	imprimirPadroes(TabuleiroPadrao),nl,nl,nl,
	write(' Escolha o padrao que pretende jogar:'), nl,
	write(' Indice: '),
	get_char(IndiceChar), nl,get_char(_), number_chars(IndicePadrao, [IndiceChar]),
	%read_line(IndiceCodigos),number_codes(IndicePadrao,IndiceCodigos), nl,
	nth0(IndicePadrao, TabuleiroPadrao, Padrao),
	write('Quantas vezes pretende roda-lo? '),
	get_char(RotacoesChar), get_char(_), number_chars(Rotacoes, [RotacoesChar]), nl,
	rodarPadrao(Padrao, PadraoRodado, Rotacoes),
	write('Em que linha vai colocar o padrao escolhido? '),
	get_char(LinhaChar), get_char(_), number_chars(Linha, [LinhaChar]), nl,
	write('E em que coluna? '),
	get_char(ColunaChar), get_char(_), number_chars(Coluna, [ColunaChar]),
	nth0(Linha, TabuleiroJogo, LinhaComPadroes), %Apanhar lista com a linha do padrao a colocar
	nth0(Coluna, LinhaComPadroes, PadraoDoTabuleiroParaVerificacao),
	( compare(=, [[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']], PadraoDoTabuleiroParaVerificacao),
	  jogadaValidaCima(TabuleiroJogo, Linha, Coluna, PadraoRodado), jogadaValidaDir(TabuleiroJogo, Linha, Coluna, PadraoRodado), jogadaValidaBaixo(TabuleiroJogo, Linha, Coluna, PadraoRodado), jogadaValidaEsq(TabuleiroJogo, Linha, Coluna, PadraoRodado)
		-> substituir(LinhaComPadroes, Coluna, PadraoRodado, LinhaFinal),
		   substituir(TabuleiroJogo, Linha, LinhaFinal, NovoTabuleiro),
		   apagarElementoPadrao(Padrao, TabuleiroPadrao, NovoTabuleiroPadrao), nl, nl
		;  nl, nl, write('>>>>> ERRO: Nao pode colocar essa peca nesse lugar!'), nl, nl, nl,
		escolherColocarPadrao(TabuleiroJogo, TabuleiroPadrao, NovoTabuleiro, NovoTabuleiroPadrao)
	).
	
jogadaValidaCima(Tabuleiro, Linha, Coluna, Padrao):-
	nth0(0, Padrao, ParteSupPeca),
	OutraLinha is Linha - 1,
	(OutraLinha >= 0 ->
		nth0(OutraLinha, Tabuleiro, OutraLinhaComPadroes),
		nth0(Coluna, OutraLinhaComPadroes, OutraPeca),
		nth0(2, OutraPeca, ParteInfOutraPeca),
		(compare(=, ParteInfOutraPeca, ParteSupPeca); compare(=, ParteInfOutraPeca, [' ', ' ', ' ']));
		true
	).
	
jogadaValidaDir(Tabuleiro, Linha, Coluna, Padrao):-
	nth0(0, Padrao, ParteSupPeca),
	nth0(1, Padrao, ParteCentrPeca),
	nth0(2, Padrao, ParteInfPeca),
	nth0(2, ParteSupPeca, SupDirPeca),
	nth0(2, ParteCentrPeca, CentrDirPeca),
	nth0(2, ParteInfPeca, InfDirPeca),
	OutraColuna is Coluna + 1,
	(OutraColuna =< 3 ->
		nth0(Linha, Tabuleiro, LinhaComPadroes),
		nth0(OutraColuna, LinhaComPadroes, OutraPeca),
		nth0(0, OutraPeca, ParteSupOutraPeca),
		nth0(1, OutraPeca, ParteCentrOutraPeca),
		nth0(2, OutraPeca, ParteInfOutraPeca),
		nth0(0, ParteSupOutraPeca, SupEsqOutraPeca),
		nth0(0, ParteCentrOutraPeca, CentrEsqOutraPeca),
		nth0(0, ParteInfOutraPeca, InfEsqOutraPeca),
		(compare(=, [SupEsqOutraPeca, CentrEsqOutraPeca, InfEsqOutraPeca], [SupDirPeca, CentrDirPeca, InfDirPeca]); compare(=, [SupEsqOutraPeca, CentrEsqOutraPeca, InfEsqOutraPeca], [' ', ' ', ' ']));
		true
	).
	
jogadaValidaBaixo(Tabuleiro, Linha, Coluna, Padrao):-
	nth0(2, Padrao, ParteInfPeca),
	OutraLinha is Linha + 1,
	(OutraLinha =< 3 ->
		nth0(OutraLinha, Tabuleiro, OutraLinhaComPadroes),
		nth0(Coluna, OutraLinhaComPadroes, OutraPeca),
		nth0(0, OutraPeca, ParteSupOutraPeca),
		(compare(=, ParteSupOutraPeca, ParteInfPeca); compare(=, ParteSupOutraPeca, [' ', ' ', ' ']));
		true
	).
	
jogadaValidaEsq(Tabuleiro, Linha, Coluna, Padrao):-
	nth0(0, Padrao, ParteSupPeca),
	nth0(1, Padrao, ParteCentrPeca),
	nth0(2, Padrao, ParteInfPeca),
	nth0(0, ParteSupPeca, SupEsqPeca),
	nth0(0, ParteCentrPeca, CentrEsqPeca),
	nth0(0, ParteInfPeca, InfEsqPeca),
	OutraColuna is Coluna - 1,
	(OutraColuna >= 0 ->
		nth0(Linha, Tabuleiro, LinhaComPadroes),
		nth0(OutraColuna, LinhaComPadroes, OutraPeca),
		nth0(0, OutraPeca, ParteSupOutraPeca),
		nth0(1, OutraPeca, ParteCentrOutraPeca),
		nth0(2, OutraPeca, ParteInfOutraPeca),
		nth0(2, ParteSupOutraPeca, SupDirOutraPeca),
		nth0(2, ParteCentrOutraPeca, CentrDirOutraPeca),
		nth0(2, ParteInfOutraPeca, InfDirOutraPeca),
		(compare(=, [SupDirOutraPeca, CentrDirOutraPeca, InfDirOutraPeca], [SupEsqPeca, CentrEsqPeca, InfEsqPeca]); compare(=, [SupDirOutraPeca, CentrDirOutraPeca, InfDirOutraPeca], [' ', ' ', ' ']));
		true
	).

verificarFimJogo(Tabuleiro, TabuleiroPadrao, FimJogo):-
	length(TabuleiroPadrao, Tam),
	( Tam =:= 0
		-> FimJogo is 0
	; (\+verificarExistenciaJogadaValida(Tabuleiro, 0, 0, TabuleiroPadrao, 0)
		-> FimJogo is 0
		; FimJogo is 1
	  )
	).

verificarExistenciaJogadaValida(_,_,_,_,1):- !.
verificarExistenciaJogadaValida(_, _, _, [], _):- fail.
verificarExistenciaJogadaValida(Tabuleiro, 4, 0, [Cabeca|Resto], 0):-
	verificarExistenciaJogadaValida(Tabuleiro, 0, 0, Resto, 0).
verificarExistenciaJogadaValida(Tabuleiro,Linha,Coluna,[Cabeca|Resto], 0):-
	nth0(Linha, Tabuleiro, LinhaComPecas),
	nth0(Coluna, LinhaComPecas, PosicaoAVerificar),
	(\+compare(=, [[' ', ' ', ' '], [' ', ' ', ' '], [' ', ' ', ' ']], PosicaoAVerificar) 
		->  
			(Coluna =:= 3 
					-> Linha1 is Linha+1, Coluna1 is 0
					; Linha1 is Linha, Coluna1 is Coluna+1
			),
			verificarExistenciaJogadaValida(Tabuleiro, Linha1, Coluna1, [Cabeca|Resto], 0);

		(verificarJogadaValida(Tabuleiro, Linha, Coluna, Cabeca)
			->  verificarExistenciaJogadaValida(Tabuleiro, Linha, Coluna, [Cabeca|Resto], 1)
			;	(Coluna =:= 3
					-> Linha1 is Linha+1, Coluna1 is 0
					; Linha1 is Linha, Coluna1 is Coluna+1
				),
					verificarExistenciaJogadaValida(Tabuleiro, Linha1, Coluna1, [Cabeca|Resto], 0)
		)
	).
	
verificarJogadaValida(Tabuleiro, Linha, Coluna, Peca):-
	((jogadaValidaCima(Tabuleiro, Linha, Coluna, Peca), jogadaValidaDir(Tabuleiro, Linha, Coluna, Peca), jogadaValidaBaixo(Tabuleiro, Linha, Coluna, Peca), jogadaValidaEsq(Tabuleiro, Linha, Coluna, Peca));
	(rodarPadrao(Peca, PecaRodada1, 1), jogadaValidaCima(Tabuleiro, Linha, Coluna, PecaRodada1), jogadaValidaDir(Tabuleiro, Linha, Coluna, PecaRodada1), jogadaValidaBaixo(Tabuleiro, Linha, Coluna, PecaRodada1), jogadaValidaEsq(Tabuleiro, Linha, Coluna, PecaRodada1));
	(rodarPadrao(PecaRodada1, PecaRodada2, 1), jogadaValidaCima(Tabuleiro, Linha, Coluna, PecaRodada2), jogadaValidaDir(Tabuleiro, Linha, Coluna, PecaRodada2), jogadaValidaBaixo(Tabuleiro, Linha, Coluna, PecaRodada2), jogadaValidaEsq(Tabuleiro, Linha, Coluna, PecaRodada2));
	(rodarPadrao(PecaRodada2, PecaRodada3, 1), jogadaValidaCima(Tabuleiro, Linha, Coluna, PecaRodada3), jogadaValidaDir(Tabuleiro, Linha, Coluna, PecaRodada3), jogadaValidaBaixo(Tabuleiro, Linha, Coluna, PecaRodada3), jogadaValidaEsq(Tabuleiro, Linha, Coluna, PecaRodada3))).

mudarJogador(NumeroJogador,NovoNumeroJogador):-
	(NumeroJogador =:= 0
		-> NovoNumeroJogador is 1
		; NovoNumeroJogador is 0
	).

limparEcra(50):- !.
limparEcra(Numero):-
	nl, Maximo is Numero + 1, limparEcra(Maximo).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FUNÇÕES RELACIONADAS COM PLAYER VS COMPUTER   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


jogarCmp(TabuleiroJogo,TabuleiroPadrao,NumeroJogador):-	
		limparEcra(0),
		write('=============================================================='),nl,
		write('====================== A JOGAR COMO JOGADOR '), write(NumeroJogador), write(' ================'),nl,
		write('=============================================================='),nl,nl,nl,nl,
		(NumeroJogador =:= 3
			->jogadaComputador(TabuleiroJogo,TabuleiroPadrao,NovoTabuleiro,NovoTabuleiroPadrao)
			; escolherColocarPadrao(TabuleiroJogo, TabuleiroPadrao, NovoTabuleiro, NovoTabuleiroPadrao)
		),
		verificarFimJogo(NovoTabuleiro,NovoTabuleiroPadrao, FimJogo),
		(FimJogo =\= 0
			-> mudarJogadorCmp(NumeroJogador,NovoNumeroJogador),jogarCmp(NovoTabuleiro,NovoTabuleiroPadrao,NovoNumeroJogador) 
			; imprimirTabuleiro(NovoTabuleiro),nl,nl,nl,write('=================   Jogador '), write(NumeroJogador), write(' ganhou! ================='), nl
		).

jogadaComputador(TabuleiroJogo,TabuleiroPadrao,NovoTabuleiro,NovoTabuleiroPadrao):-
	length(TabuleiroPadrao, Tam),
	random(0, Tam, PadraoEscolhido),
	random(0, 4, Linha),
	random(0, 4, Coluna),
	nth0(IndicePadrao, TabuleiroPadrao, PadraoRodado),   %%%%%%%% ALTERAR O PADRAO RODADO %%%%%%%%%%%%
	%write('Quantas vezes pretende roda-lo? '),
	%rodarPadrao(Padrao, PadraoRodado, Rotacoes),
	nth0(Linha, TabuleiroJogo, LinhaComPadroes), %Apanhar lista com a linha do padrao a colocar
	nth0(Coluna, LinhaComPadroes, PadraoDoTabuleiroParaVerificacao),
	( compare(=, [[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']], PadraoDoTabuleiroParaVerificacao),
	  jogadaValidaCima(TabuleiroJogo, Linha, Coluna, PadraoRodado), jogadaValidaDir(TabuleiroJogo, Linha, Coluna, PadraoRodado), jogadaValidaBaixo(TabuleiroJogo, Linha, Coluna, PadraoRodado), jogadaValidaEsq(TabuleiroJogo, Linha, Coluna, PadraoRodado)
		-> substituir(LinhaComPadroes, Coluna, PadraoRodado, LinhaFinal),
		   substituir(TabuleiroJogo, Linha, LinhaFinal, NovoTabuleiro),
		   apagarElementoPadrao(PadraoRodado, TabuleiroPadrao, NovoTabuleiroPadrao)	%%%%%%%% ALTERAR O PADRAO RODADO %%%%%%%%%%%%
		; jogadaComputador(TabuleiroJogo,TabuleiroPadrao,NovoTabuleiro,NovoTabuleiroPadrao)
	).

mudarJogadorCmp(NumeroJogador,NovoNumeroJogador):-
	(NumeroJogador =:= 0
		-> NovoNumeroJogador is 3
		; NovoNumeroJogador is 0
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FUNÇÕES RELACIONADAS COM LISTAS   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% substituir(+Lista, +Posicao, +Elemento, -ListaFinal)
substituir([_|T], 0, Elemento, [Elemento|T]).
substituir([H|T], Posicao, Elemento, [H|T1]):-
	Posicao > 0,
	Posicao1 is Posicao-1,
	substituir(T, Posicao1, Elemento, T1).

apagarElementoPadrao(Padrao,[Padrao|Resto],Resto).
apagarElementoPadrao(Padrao,[Igual|Resto],[Igual|RestoNovoT]):-
    apagarElementoPadrao(Padrao,Resto,RestoNovoT).
	
% rodarPadrao(+Peca, -PecaRodada, +Rotacoes)
rodarPadrao(Peca, Peca, 0):- !.
rodarPadrao([[A1, B1, C1], [A2, B2, C2], [A3, B3, C3]], PecaRodada, Rotacoes):-
	Rotacoes1 is (Rotacoes-1) mod 4,
	rodarPadrao([[A3, A2, A1], [B3, B2, B1], [C3, C2, C1]], PecaRodada, Rotacoes1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FUNÇÕES DE IMPRIMIR TABULEIRO E PADROES  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imprimirTabuleiro([]):- 
	write('#########################################'),
	!.
imprimirTabuleiro([H|T]):-
	write('#########################################'), nl,
	imprimirLinha(H),
	imprimirTabuleiro(T).

imprimirLinha(Linha):-
	write('| '),
	imprimirLinhaPeca(Linha,0), nl, write('| '),
	imprimirLinha2Peca(Linha,0), nl, write('| '),
	imprimirLinha3Peca(Linha,0), nl.

imprimirLinhaPeca(_,4):- !.
imprimirLinhaPeca([[Linha|R]|T],Numero):-
	N1 is Numero + 1,
	nth0(0,Linha,LinhaPeca),
	nth0(1,Linha,LinhaPeca1),
	nth0(2,Linha,LinhaPeca2),
	write(LinhaPeca),
	write('  '),
	write(LinhaPeca1),
	write('  '),
	write(LinhaPeca2),
	write(' | '),
	%write(Linha),nl,write(R), nl, write(T), nl,
	imprimirLinhaPeca(T,N1).

imprimirLinha2Peca(_,4):- !.
imprimirLinha2Peca([[Linha|[R|X]]|T],Numero):-
	N1 is Numero + 1,
	nth0(0,R,LinhaPeca),
	nth0(1,R,LinhaPeca1),
	nth0(2,R,LinhaPeca2),
	write(LinhaPeca),
	write('  '),
	write(LinhaPeca1),
	write('  '),
	write(LinhaPeca2),
	write(' | '),
	imprimirLinha2Peca(T,N1).

imprimirLinha3Peca(_,4):- !.
imprimirLinha3Peca([[Linha|[R|[X|Lixo]]]|T],Numero):-
	N1 is Numero + 1,
	nth0(0,X,LinhaPeca),
	nth0(1,X,LinhaPeca1),
	nth0(2,X,LinhaPeca2),
	write(LinhaPeca),
	write('  '),
	write(LinhaPeca1),
	write('  '),
	write(LinhaPeca2),
	write(' | '),
	imprimirLinha3Peca(T,N1).

imprimirPadroes(Padrao):-
	write('| '),
	imprimirPadroesLinha1(Padrao), nl, write('| '),
	imprimirPadroesLinha2(Padrao), nl, write('| '),
	imprimirPadroesLinha3(Padrao).

imprimirPadroesLinha1([]):- !.
imprimirPadroesLinha1([[Cabeca|Resto]|T]):-
	nth0(0,Cabeca,Indice1),
	nth0(1,Cabeca,Indice2),
	nth0(2,Cabeca,Indice3),
	write(Indice1),write(' '),
	write(Indice2),write(' '),
	write(Indice3),write(' '),
	write('| '),
	imprimirPadroesLinha1(T).

imprimirPadroesLinha2([]):- !.
imprimirPadroesLinha2([[Cabeca|[CabecaResto|Resto]]|T]):-
	nth0(0,CabecaResto,Indice1),
	nth0(1,CabecaResto,Indice2),
	nth0(2,CabecaResto,Indice3),
	write(Indice1),write(' '),
	write(Indice2),write(' '),
	write(Indice3),write(' '),
	write('| '),
	imprimirPadroesLinha2(T).

imprimirPadroesLinha3([]):- !.
imprimirPadroesLinha3([[Cabeca|[CabecaResto|[Resto|Vazio]]]|T]):-
	nth0(0,Resto,Indice1),
	nth0(1,Resto,Indice2),
	nth0(2,Resto,Indice3),
	write(Indice1),write(' '),
	write(Indice2),write(' '),
	write(Indice3),write(' '),
	write('| '),
	imprimirPadroesLinha3(T).

