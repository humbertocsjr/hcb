DECLARE SUB Incluir ()
DECLARE SUB CriarWhile ()
DECLARE SUB CriarIf ()
DECLARE SUB Expr1 ()
DECLARE SUB Limpar ()
DECLARE SUB ChamarRotina ()
DECLARE SUB Expr3 ()
DECLARE SUB Expr2 ()
DECLARE FUNCTION NovoRotulo () AS STRING
DECLARE SUB Expr ()
DECLARE SUB CriarAtribuicaoDireta ()
DECLARE FUNCTION EncontrarReferencia (nome AS STRING, ponteiro AS INTEGER) AS STRING
DECLARE SUB CriarExternos ()
DECLARE SUB Erro (msg AS STRING)
DECLARE SUB CriarLocais ()
DECLARE SUB Processar ()
DECLARE SUB CriarRotina ()
DECLARE SUB LerTrecho ()
DECLARE FUNCTION LerCaractere () AS STRING
DECLARE FUNCTION EhLetra! (c AS STRING)
DECLARE FUNCTION EhNumero! (c AS STRING)
DECLARE FUNCTION EhAlfaNumerico! (c AS STRING)
CONST cTipoTexto = 1
CONST cTipoNumero = 2
CONST cTipoIdentificador = 3
CONST cTipoOperacao = 4
CONST cTipoOutros = 5
CONST cTipoFim = 6
CONST cTipoAbreParenteses = 7
CONST cTipoAbreBloco = 8
CONST cTipoFechaParenteses = 9
CONST cTipoFechaBloco = 10

TYPE tTrecho
    linha AS INTEGER
    coluna AS INTEGER
    tipo AS INTEGER
    tam AS INTEGER
    trecho AS STRING * 64
END TYPE

DIM SHARED gC AS STRING
DIM SHARED gM  AS STRING
DIM SHARED gAtual AS tTrecho
DIM SHARED gAnterior AS tTrecho
DIM SHARED gProximo AS tTrecho
DIM SHARED gLinha AS INTEGER
DIM SHARED gColuna AS INTEGER
DIM SHARED gLocais(64) AS STRING
DIM SHARED gExternos(64) AS STRING
DIM SHARED gPos AS INTEGER
DIM SHARED gStatus AS INTEGER
DIM SHARED gNomeRotulo AS STRING
DIM SHARED gRotulo AS INTEGER
DIM SHARED gArqAtual AS INTEGER
DIM SHARED gArqAtualNome AS STRING

gRotulo = 0
gStatus = 0

PRINT "HCB Compilador v0.9"
PRINT "Linguagem de Programacao HCB"
PRINT "Copyright (c) 2020 Humberto Costa dos Santos Junior"
PRINT

IF LEN(COMMAND$) = 0 THEN
    PRINT "Modo de uso: HCB [arquivo sem extensao]"
    PRINT "Entrada....: .HCB"
    PRINT "Saida......: .ASM"
    SYSTEM
END IF

OPEN (COMMAND$ + ".HCB") FOR INPUT AS #1
OPEN (COMMAND$ + ".ASM") FOR OUTPUT AS #2

gLinha = 1
gColuna = 0

gArqAtual = 1
gArqAtualNome$ = COMMAND$ + ".hcb"

PRINT "Compilando " + COMMAND$ + ".hcb ";

gNomeRotulo$ = COMMAND$

IF LEN(LerCaractere$) > 0 THEN
    LerTrecho
    LerTrecho
    Limpar
    DO WHILE gAtual.tipo <> cTipoFim
        Processar
    LOOP
END IF

IF gStatus = 0 THEN
    PRINT " [ OK ]"
ELSE
    PRINT " [ FALHA ]"
END IF

CLOSE #1
CLOSE #2
PRINT
PRINT "Compilacao concluida com sucesso"
PRINT "Chamando montador para 8086"
SHELL ("tinyasm -f bin -o " + COMMAND$ + ".com " + COMMAND$ + ".asm")

SUB ChamarRotina ()
    DIM nome AS STRING
    DIM qtd AS INTEGER
    nome$ = RTRIM$(gAtual.trecho$)
    LerTrecho
    LerTrecho
    qtd = 0
    DO WHILE RTRIM$(gAtual.trecho$) <> ")" AND gAtual.tipo <> cTipoFim
        Expr
        PRINT #2, "push ax"
        IF RTRIM$(gAtual.trecho$) = "," THEN
            LerTrecho
        END IF
        qtd = qtd + 1
    LOOP
    PRINT #2, "mov ax, " + STR$(qtd)
    PRINT #2, "push ax"
    PRINT #2, "call " + EncontrarReferencia$(nome$, 0)
    PRINT #2, "add sp, " + STR$((qtd * 2) + 2)
    LerTrecho
END SUB

SUB CriarAtribuicaoDireta ()
    DIM nome AS STRING
    nome$ = RTRIM$(gAtual.trecho$)
    nome$ = EncontrarReferencia(nome$, 1)
    IF LEN(nome$) = 0 THEN
        Erro "Referencia '" + RTRIM$(gAtual.trecho$) + "' nao encotrada, tente declara-la primeiro usando 'extern' ou 'auto'"
    END IF
    LerTrecho
    SELECT CASE RTRIM$(gAtual.trecho$)
    CASE "="
        LerTrecho
        Expr
        PRINT #2, "mov " + nome$ + ", ax"
    CASE "++"
        LerTrecho
        PRINT #2, "inc " + nome$
    CASE "--"
        LerTrecho
        PRINT #2, "dec " + nome$
    CASE "-="
        LerTrecho
        Expr
        PRINT #2, "sub " + nome$ + ", ax"
    CASE "+="
        LerTrecho
        Expr
        PRINT #2, "add " + nome$ + ", ax"
    END SELECT
    IF RTRIM$(gAtual.trecho$) <> ";" THEN
        Erro "Esperado ';' porem encontrado '" + RTRIM$(gAtual.trecho$) + "'"
    END IF
    LerTrecho
END SUB

SUB CriarExternos ()
    DIM i AS INTEGER
    LerTrecho
    FOR i = 1 TO 64
        IF gExternos(i) = "" THEN
            EXIT FOR
        END IF
    NEXT
    DO WHILE RTRIM$(gAtual.trecho$) <> ";"
        gExternos(i) = RTRIM$(gAtual.trecho$)
        PRINT #2, ";extern _" + gExternos(i)
        arg = arg + 2
        i = i + 1
        IF RTRIM$(gProximo.trecho$) = "," THEN
            LerTrecho
        END IF
        LerTrecho
    LOOP
    LerTrecho
END SUB

SUB CriarIf ()
    DIM rotulo AS STRING
    LerTrecho
    IF RTRIM$(gAtual.trecho$) <> "(" THEN
        Erro "Esperado '(' apos o 'if'"
    END IF
    LerTrecho
    Expr
    IF RTRIM$(gAtual.trecho$) <> ")" THEN
        Erro "Esperado ')' apos a comparacao, porem encontrado '" + RTRIM$(gAtual.trecho$) + "'"
    END IF
    LerTrecho
    PRINT #2, "cmp ax, 0"
    rotulo$ = NovoRotulo$()
    PRINT #2, "je .__" + rotulo$ + ""
    Processar
    IF RTRIM$(gAtual.trecho$) = "else" THEN
        PRINT #2, "jmp .__" + rotulo$ + "_fim"
        PRINT #2, ".__" + rotulo$ + ":"
            LerTrecho
        Processar
        PRINT #2, ".__" + rotulo$ + "_fim:"
    ELSE
        PRINT #2, ".__" + rotulo$ + ":"
    END IF
END SUB

SUB CriarLocais ()
    DIM i AS INTEGER
    LerTrecho
    FOR i = 1 TO 64
        IF gLocais(i) = "" THEN
            EXIT FOR
        END IF
    NEXT
    DO WHILE RTRIM$(gAtual.trecho$) <> ";"
        gPos = gPos - 2
        PRINT #2, "sub sp, 2"
        PRINT #2, "." + RTRIM$(gAtual.trecho$) + ": EQU " + STR$(gPos)
        gLocais(i) = RTRIM$(gAtual.trecho$)
        arg = arg + 2
        i = i + 1
        IF RTRIM$(gProximo.trecho$) = "," THEN
            LerTrecho
        END IF
        LerTrecho
    LOOP
    LerTrecho
END SUB

SUB CriarRotina ()
    DIM nome AS STRING
    DIM arg AS INTEGER
    DIM i AS INTEGER
    nome$ = RTRIM$(gAtual.trecho$)
    PRINT #2, "_" + nome$ + ":"
    PRINT #2, "push bp"
    PRINT #2, "mov bp, sp"
    LerTrecho
    LerTrecho
    arg = 0
    Limpar
    gLocais(1) = "__argc"
    gLocais(2) = "__argv"
    i = 3
    DO WHILE RTRIM$(gAtual.trecho$) <> ")"
        arg = arg + 2
        PRINT #2, "." + RTRIM$(gAtual.trecho$) + ": EQU .___TOTAL - " + STR$(arg)
        gLocais(i) = RTRIM$(gAtual.trecho$)
        i = i + 1
        IF RTRIM$(gProximo.trecho$) = "," THEN
            LerTrecho
        END IF
        LerTrecho
    LOOP
    PRINT #2, ".___TOTAL: EQU " + STR$(arg + 6)
    PRINT #2, ".__argc: EQU 4"
    PRINT #2, ".__argv: EQU 6"
    LerTrecho
    gPos = 0
    Processar
    PRINT #2, "mov sp, bp"
    PRINT #2, "pop bp"
    PRINT #2, "ret"
    Limpar
END SUB

SUB CriarWhile ()
    DIM rotulo AS STRING
    LerTrecho
    IF RTRIM$(gAtual.trecho$) <> "(" THEN
        Erro "Esperado '(' apos o 'while'"
    END IF
    LerTrecho
    rotulo$ = NovoRotulo$()
    PRINT #2, ".__" + rotulo$ + ":"
    Expr
    IF RTRIM$(gAtual.trecho$) <> ")" THEN
        Erro "Esperado ')' apos a comparacao, porem encontrado '" + RTRIM$(gAtual.trecho$) + "'"
    END IF
    LerTrecho
    PRINT #2, "cmp ax, 0"
    PRINT #2, "je .__" + rotulo$ + "_fim"
    Processar
    PRINT #2, "jmp .__" + rotulo$
    PRINT #2, ".__" + rotulo$ + "_fim:"
END SUB

FUNCTION EhAlfaNumerico (c AS STRING)
    EhAlfaNumerico = EhLetra(c$) OR EhNumero(c$)
END FUNCTION

FUNCTION EhLetra (c AS STRING)
    EhLetra = (c >= "A" AND c <= "Z") OR c = "_"
END FUNCTION

FUNCTION EhNumero (c AS STRING)
    EhNumero = c >= "0" AND c <= "9"
END FUNCTION

FUNCTION EncontrarReferencia (nome AS STRING, ponteiro AS INTEGER) AS STRING
    FOR i = 1 TO 64
        IF gLocais$(i) = nome$ THEN
            IF ponteiro > 0 THEN
                EncontrarReferencia$ = "word [bp + ." + nome$ + "]"
            ELSE
                EncontrarReferencia$ = "." + nome$
            END IF
            EXIT FUNCTION
        ELSEIF gExternos$(i) = nome$ THEN
            IF ponteiro > 0 THEN
                EncontrarReferencia$ = "word [_" + nome$ + "]"
            ELSE
                EncontrarReferencia$ = "_" + nome$
            END IF
            EXIT FUNCTION
        END IF
    NEXT
    EncontrarReferencia$ = ""
END FUNCTION

SUB Erro (msg AS STRING)
    PRINT
    PRINT "ERRO: " + STR$(gAtual.linha) + ":" + STR$(gAtual.coluna) + ": " + msg$
    PRINT "Continuando"
    gStatus = 1
    SYSTEM
END SUB

SUB Expr ()
    DIM op AS STRING
    DIM rotulo AS STRING
    Expr1
    op$ = RTRIM$(gAtual.trecho$)
    DO WHILE op$ = "==" OR op$ = ">" OR op$ = "<" OR op$ = ">=" OR op$ = "<=" OR op$ = "!="
        SELECT CASE op$
        CASE "=="
            PRINT #2, "push ax"
            LerTrecho
            Expr1
            PRINT #2, "pop bx"
            PRINT #2, "not bx"
            PRINT #2, "xor ax, bx"
            PRINT #2, "cmp ax, 0xffff"
            rotulo$ = NovoRotulo$
            PRINT #2, "je .__" + rotulo$
            PRINT #2, "mov ax, 0"
            PRINT #2, ".__" + rotulo$ + ":"
        CASE "!="
            PRINT #2, "push ax"
            LerTrecho
            Expr1
            PRINT #2, "pop bx"
            PRINT #2, "not bx"
            PRINT #2, "xor ax, bx"
            PRINT #2, "cmp ax, 0xffff"
            rotulo$ = NovoRotulo$
            PRINT #2, "jne .__" + rotulo$
            PRINT #2, "mov ax, 0"
            PRINT #2, ".__" + rotulo$ + ":"
        CASE "<"
            PRINT #2, "push ax"
            LerTrecho
            Expr1
            PRINT #2, "pop bx"
            PRINT #2, "cmp bx, ax"
            rotulo$ = NovoRotulo$
            PRINT #2, "jl .__" + rotulo$
            PRINT #2, "xor ax, ax"
            PRINT #2, "jmp .__" + rotulo$ + "_fim"
            PRINT #2, ".__" + rotulo$ + ":"
            PRINT #2, "mov ax, 0xffff"
            PRINT #2, ".__" + rotulo$ + "_fim:"
        CASE ">"
            PRINT #2, "push ax"
            LerTrecho
            Expr1
            PRINT #2, "pop bx"
            PRINT #2, "cmp bx, ax"
            rotulo$ = NovoRotulo$
            PRINT #2, "jg .__" + rotulo$
            PRINT #2, "xor ax, ax"
            PRINT #2, "jmp .__" + rotulo$ + "_fim"
            PRINT #2, ".__" + rotulo$ + ":"
            PRINT #2, "mov ax, 0xffff"
            PRINT #2, ".__" + rotulo$ + "_fim:"
        CASE "<="
            PRINT #2, "push ax"
            LerTrecho
            Expr1
            PRINT #2, "pop bx"
            PRINT #2, "cmp bx, ax"
            rotulo$ = NovoRotulo$
            PRINT #2, "jle .__" + rotulo$
            PRINT #2, "xor ax, ax"
            PRINT #2, "jmp .__" + rotulo$ + "_fim"
            PRINT #2, ".__" + rotulo$ + ":"
            PRINT #2, "mov ax, 0xffff"
            PRINT #2, ".__" + rotulo$ + "_fim:"
        CASE ">="
            PRINT #2, "push ax"
            LerTrecho
            Expr1
            PRINT #2, "pop bx"
            PRINT #2, "cmp bx, ax"
            rotulo$ = NovoRotulo$
            PRINT #2, "jge .__" + rotulo$
            PRINT #2, "xor ax, ax"
            PRINT #2, "jmp .__" + rotulo$ + "_fim"
            PRINT #2, ".__" + rotulo$ + ":"
            PRINT #2, "mov ax, 0xffff"
            PRINT #2, ".__" + rotulo$ + "_fim:"
        END SELECT
        op$ = RTRIM$(gAtual.trecho$)
    LOOP
END SUB

SUB Expr1 ()
    DIM op AS STRING
    DIM rotulo AS STRING
    IF gAtual.tipo = cTipoTexto THEN
        rotulo$ = NovoRotulo$()
        PRINT #2, "jmp ._fim_" + rotulo$
        PRINT #2, "._" + rotulo$ + ":"
        PRINT #2, "db " + CHR$(34) + MID$(gAtual.trecho$, 1, gAtual.tam) + CHR$(34) + ", 0, 0"
        PRINT #2, "._fim_" + rotulo$ + ":"
        PRINT #2, "mov ax, ._" + rotulo$
        LerTrecho
        EXIT SUB
    END IF
    Expr2
    op$ = RTRIM$(gAtual.trecho$)
    DO WHILE op$ = "+" OR op$ = "-"
        SELECT CASE op$
        CASE "+"
            LerTrecho
            PRINT #2, "push ax"
            Expr2
            PRINT #2, "pop bx"
            PRINT #2, "xchg ax, bx"
            PRINT #2, "add ax, bx"
        CASE "-"
            LerTrecho
            PRINT #2, "push ax"
            Expr2
            PRINT #2, "pop bx"
            PRINT #2, "xchg ax, bx"
            PRINT #2, "sub ax, bx"
        END SELECT
        op$ = RTRIM$(gAtual.trecho$)
    LOOP
END SUB

SUB Expr2 ()
    DIM op AS STRING
    Expr3
    op$ = RTRIM$(gAtual.trecho$)
    DO WHILE op$ = "*" OR op$ = "/" OR op$ = "%" OR op$ = "<<" OR op$ = ">>"
        SELECT CASE op$
        CASE "<<"
            PRINT #2, "push ax"
            LerTrecho
            Expr3
            PRINT #2, "mov cx, ax"
            PRINT #2, "rep shl ax, 1"
        CASE ">>"
            PRINT #2, "push ax"
            LerTrecho
            Expr3
            PRINT #2, "mov cx, ax"
            PRINT #2, "rep shr ax, 1"
        CASE "%"
            IF gProximo.tipo = cTipoNumero THEN
                LerTrecho
                PRINT #2, "mov bx, " + RTRIM$(gAtual.trecho$)
                LerTrecho
            ELSE
                LerTrecho
                PRINT #2, "push ax"
                Expr3
                PRINT #2, "pop bx"
                PRINT #2, "xchg ax, bx"
            END IF
            PRINT #2, "xor dx, dx"
            PRINT #2, "idiv bx"
            PRINT #2, "mov ax, dx"
        CASE "/"
            IF gProximo.tipo = cTipoNumero THEN
                LerTrecho
                PRINT #2, "mov bx, " + RTRIM$(gAtual.trecho$)
                LerTrecho
            ELSE
                LerTrecho
                PRINT #2, "push ax"
                Expr3
                PRINT #2, "pop bx"
                PRINT #2, "xchg ax, bx"
            END IF
            PRINT #2, "xor dx, dx"
            PRINT #2, "idiv bx"
        CASE "*"
            IF gProximo.tipo = cTipoNumero THEN
                LerTrecho
                PRINT #2, "mov bx, " + RTRIM$(gAtual.trecho$)
                LerTrecho
            ELSE
                LerTrecho
                PRINT #2, "push ax"
                Expr3
                PRINT #2, "pop bx"
                PRINT #2, "xchg ax, bx"
            END IF
            PRINT #2, "xor dx, dx"
            PRINT #2, "imul bx"
        END SELECT
        op$ = RTRIM$(gAtual.trecho$)
    LOOP
END SUB

SUB Expr3 ()
    IF gAtual.tipo = cTipoNumero THEN
        PRINT #2, "mov ax, " + RTRIM$(gAtual.trecho$)
        LerTrecho
    ELSEIF EncontrarReferencia$(RTRIM$(gAtual.trecho$), 0) <> "" THEN
        SELECT CASE RTRIM$(gProximo.trecho$)
        CASE "("
            ChamarRotina
        CASE ELSE
            PRINT #2, "mov ax, "; EncontrarReferencia$(RTRIM$(gAtual.trecho$), 1)
            LerTrecho
        END SELECT
    ELSE
        Erro "Expressao desconhecida, encontrado '" + RTRIM$(gAtual.trecho$) + "'"
        LerTrecho
    END IF
END SUB

SUB Incluir ()
    DIM arqNome AS STRING
    DIM trechoAnt AS tTrecho
    DIM trechoAtu AS tTrecho
    DIM trechoPro AS tTrecho
    DIM arqAtual AS STRING
    DIM arqAtualNro AS INTEGER
    DIM linha AS INTEGER
    DIM coluna AS INTEGER
    DIM c AS STRING
    DIM m AS STRING

    arqNome$ = RTRIM$(gAtual.trecho$)
    trechoAnt = gAnterior
    trechoAtu = gAtual
    trechoPro = gProximo
    linha = gLinha
    coluna = gColuna
    arqAtualNro = gArqAtual
    arqAtual$ = gArqAtualNome$
    c$ = gC$
    m$ = gM$

    gArqAtual = FREEFILE
    gArqAtualNome = arqNome$

    PRINT " [ INTERROMPIDO ]"

    gLinha = 1
    gColuna = 0

    PRINT "Compilando " + arqNome$ + " ";

    OPEN arqNome$ FOR INPUT AS #gArqAtual

    IF LEN(LerCaractere$) > 0 THEN
        LerTrecho
        LerTrecho
        DO WHILE gAtual.tipo <> cTipoFim
            Processar
        LOOP
    END IF

    CLOSE #gArqAtual

    PRINT " [ OK ]"
    PRINT "Continuando " + arqAtual$ + " ";

    gAnterior = trechoAnt
    gAtual = trechoAtu
    gProximo = trechoPro
    gLinha = linha
    gColuna = coluna
    gArqAtual = arqAtualNro
    gArqAtualNome$ = arqAtual$
    gC$ = c$
    gM$ = m$

END SUB

FUNCTION LerCaractere () AS STRING
    DIM tmp AS STRING
    IF EOF(gArqAtual) THEN
        gC$ = ""
        gM$ = gC$
        LerCaractere$ = gC$
        EXIT FUNCTION
    END IF

    gColuna = gColuna + 1
    gC$ = INPUT$(1, #gArqAtual)
    gM$ = UCASE$(gC$)
    LerCaractere$ = gM$
    IF gC$ = CHR$(13) THEN
        gLinha = gLinha + 1
        gColuna = 0
        tmp$ = LerCaractere$()
    ELSEIF gC$ = CHR$(10) THEN
        gColuna = 0
        tmp$ = LerCaractere$()
    END IF
END FUNCTION

SUB LerTrecho ()
    DIM tmp AS STRING
    DIM tmp2 AS STRING
    gAnterior = gAtual
    gAtual = gProximo
    IF gC$ = " " THEN
        DO WHILE LerCaractere$() = " "
        LOOP
    END IF
    gProximo.coluna = gColuna
    gProximo.linha = gLinha
    IF EhLetra(gM$) THEN
        gProximo.tipo = cTipoIdentificador
        tmp$ = gC$
        DO WHILE EhAlfaNumerico(LerCaractere$())
            tmp$ = tmp$ + gC$
        LOOP
    ELSEIF EhNumero(gC$) THEN
        gProximo.tipo = cTipoNumero
        tmp$ = gC$
        DO WHILE EhNumero(LerCaractere$())
            tmp$ = tmp$ + gC$
        LOOP
    ELSEIF gC$ = CHR$(34) THEN
        gProximo.tipo = cTipoTexto
        tmp$ = ""
        DO WHILE LerCaractere$() <> CHR$(34)
            IF gC$ = "*" THEN
                tmp2$ = LerCaractere$()
                IF gC$ = "n" THEN
                    tmp$ = tmp$ + CHR$(13) + CHR$(10)
                ELSEIF gC$ = "t" THEN
                    tmp$ = tmp$ + CHR$(9)
                ELSE
                    tmp$ = tmp$ + gC$
                END IF
            ELSE
                tmp$ = tmp$ + gC$
            END IF
        LOOP
        tmp2$ = LerCaractere$()
    ELSEIF LEN(gC$) = 0 THEN
        gProximo.tipo = cTipoFim
    ELSEIF gC$ = "(" THEN
        gProximo.tipo = cTipoAbreParenteses
        tmp$ = gC$
        tmp2$ = LerCaractere$()
    ELSEIF gC$ = ")" THEN
        gProximo.tipo = cTipoFechaParenteses
        tmp$ = gC$
        tmp2$ = LerCaractere$()
    ELSEIF gC$ = "{" THEN
        gProximo.tipo = cTipoAbreBloco
        tmp$ = gC$
        tmp2$ = LerCaractere$()
    ELSEIF gC$ = "}" THEN
        gProximo.tipo = cTipoFechaBloco
        tmp$ = gC$
        tmp2$ = LerCaractere$()
    ELSEIF gC$ = "+" OR gC$ = "-" OR gC$ = "=" OR gC$ = "<" OR gC$ = ">" OR gC$ = "!" THEN
        gProximo.tipo = cTipoOutros
        tmp$ = gC$
        tmp2$ = LerCaractere$()
        IF gC$ = tmp$ OR gC$ = "=" THEN
            tmp$ = tmp$ + gC$
            tmp2$ = LerCaractere$()
        END IF
    ELSE
        gProximo.tipo = cTipoOutros
        tmp$ = gC$
        tmp2$ = LerCaractere$()
    END IF

    gProximo.trecho$ = tmp$
    gProximo.tam = LEN(tmp$)
    PRINT ".";
END SUB

SUB Limpar ()
    DIM i AS INTEGER
    FOR i = 1 TO 64
        gLocais(i) = ""
        gExternos(i) = ""
    NEXT
END SUB

FUNCTION NovoRotulo () AS STRING
    gRotulo = gRotulo + 1
    NovoRotulo$ = gNomeRotulo$ + LTRIM$(STR$(gRotulo))
END FUNCTION

SUB Processar ()
    IF gAtual.tipo = cTipoIdentificador THEN
        SELECT CASE RTRIM$(gAtual.trecho$)
        CASE "auto"
            CriarLocais
        CASE "extern"
            CriarExternos
        CASE "if"
            CriarIf
        CASE "while"
            CriarWhile
        CASE ELSE
            SELECT CASE RTRIM$(gProximo.trecho$)
            CASE "("
                IF EncontrarReferencia$(RTRIM$(gAtual.trecho$), 0) = "" THEN
                    CriarRotina
                ELSE
                    ChamarRotina
                    IF RTRIM$(gAtual.trecho$) <> ";" THEN
                        Erro "Esperado ';' porem encontrado '" + RTRIM$(gAtual.trecho$) + "'"
                    END IF
                    LerTrecho
                END IF
            CASE "=", "++", "--", "+=", "-="
                CriarAtribuicaoDireta
            END SELECT
        END SELECT
    ELSEIF gAtual.tipo = cTipoAbreBloco THEN
        LerTrecho
        DO WHILE gAtual.tipo <> cTipoFechaBloco AND gAtual.tipo <> cTipoFim
            Processar
        LOOP
        LerTrecho
    ELSEIF RTRIM$(gAtual.trecho$) = "#" THEN
        LerTrecho
        SELECT CASE LCASE$(RTRIM$(gAtual.trecho$))
        CASE "include"
            LerTrecho
            Incluir
            LerTrecho
        CASE "includeasm"
            LerTrecho
            PRINT #2, "%include " + CHR$(34) + RTRIM$(gAtual.trecho$) + CHR$(34)
            LerTrecho
        CASE "asm"
            LerTrecho
            PRINT #2, RTRIM$(gAtual.trecho$)
            LerTrecho
        CASE ELSE
            Erro "Comando de preprocessador nao suportado: '" + RTRIM$(gAtual.trecho$) + "'"
        END SELECT
    ELSE
        Erro "Trecho nao reconhecido"
        LerTrecho
    END IF
END SUB

