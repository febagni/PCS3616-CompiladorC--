; -------------------------------------------------------------------
; Programa principal do Compilador de C--
; -------------------------------------------------------------------

; Origem relocável
                &       /0000

MAIN            JP      INI      ; salta para o início do programa
UL              K       /0000    ; parâmetro: UL onde está o arquivo code.cmm

; -------------------------------------------------------------------
; Subrotina: UNPACK
; Extrai os bytes de uma word contida no acumulador, colocando-os
; em dois endereços da memória.
;
; Exemplo: dada a word XYZT no acumulador, ao final da execução,
; UNP_B1="00XY" e UNP_B2="00ZT".
; -------------------------------------------------------------------

; Parâmetros
WORD            $       /0001       ; Word de entrada
UNP_B1          $       /0001       ; Byte mais significativo
UNP_B2          $       /0001       ; Byte menos significativo

; Constantes
SHIFT           K       /0100
CH_0            K       /0030
CH_F            K       /0046
X_INI           K       /003A
X_END           K       /0041
X_DIFF          K       /0007
ONE             K       /0001
MINUS_1         K       /FFFF
ZERO            K       /0000
EIGHT           K       /1000
FOUR            K       /0100
TWO             K       /0010

; Corpo da subrotina
UNPACK          $       /0001
                MM      WORD        ; Carrega word. Primeiramente faremos unpack de B2
                ML      SHIFT       ; Desloca os bytes para remover 2 primeiros hex
                SC      RSHIFT2     ; Desloca os bytes menos significativos pro seu lugar
                MM      UNP_B2      ; Salva resultado
                LD      WORD        ;
                SC      RSHIFT2     ;
                MM      UNP_B1      ;
                RS      UNPACK      ; Retorna

; -------------------------------------------------------------------
; Subrotina: RSHIFT2
; Faz um right shift (<) duas vezes do valor do acumulador
; -------------------------------------------------------------------

; Constantes
FIX             K       /8000
REFIX           K       /0080
; Corpo da subrotina
RSHIFT2         $       /0001
                JN      NEG         ; O número é negativo
                DV      SHIFT       ; Retorna os 2 bytes à posição inicial
                JP      FIM-RS      ; Vai para final de RSHIFT2
NEG             SB      FIX         ; Fix do shift em número negativo
                DV      SHIFT       ; Shift
                AD      REFIX       ; Fix para voltar número tirado
FIM-RS          RS      RSHIFT2     ; Retorno

; -------------------------------------------------------------------
; Subrotina: IS_HEX
; -------------------------------------------------------------------

  ;; Parâmetros
S_HEX           $       /0001
  ;; Corpo da subrotina
IS_HEX          $       /0001
                MM      S_HEX
  ;; Verifica se < '0'
                SB      CH_0
                JN      NOT_HEX
  ;; Verifica se > 'f'
                LD      S_HEX
                SB      CH_F
                SB      ONE ; we wanna include 'f'
                JN      MIGHTB
  ;; Não é hex. Retorna -1.
NOT_HEX         LD      MINUS_1
                RS      IS_HEX
  ;; Incrementa CH_F decrementado e verifica se é caractere especial.
MIGHTB          LD      S_HEX
                SB      X_INI
                JN      YES_HEX
                SB      X_DIFF
                JN      NOT_HEX
                LD      S_HEX
                SB      X_DIFF
                RS      IS_HEX
YES_HEX         LD      S_HEX
                RS      IS_HEX

; -------------------------------------------------------------------
; Subrotina: CHTOI
; Converte uma word em hexa para um número inteiro.
;
; Exemplo: CHTOI("0010") = 0010 (i.e., 16 em decimal)
; -------------------------------------------------------------------

  ;; Parâmetros
CH_ANS          $       /0001        ; Variável para guardar resultado
CH_IN_A         $       /0001        ; 2 bytes mais significativos (em ASCII)
CH_IN_B         $       /0001        ; 2 bytes menos signicativos (em ASCII)

  ;; Corpo da subrotina
CHTOI           $       /0001
  ;; Zera CH_ANS
                LD      ZERO
                MM      CH_ANS
  ;; Unpack primeira palavra
                LD      CH_IN_A
                MM      WORD
                SC      UNPACK
  ;; Processa primeira palavra
  ;; Processa primeiro byte
                LD      UNP_B1
                SC      IS_HEX
                JN      CH_RET
                SB      CH_0
                ML      EIGHT
                MM      CH_ANS
  ;; Processa segundo byte
                LD      UNP_B2
                SC      IS_HEX
                JN      CH_RET
                SB      CH_0
                ML      FOUR
                AD      CH_ANS
                MM      CH_ANS
  ;; Unpack segunda palavra
                LD      CH_IN_B
                MM      WORD
                SC      UNPACK
  ;; Processa segunda palavra
  ;; Processa primeiro byte
                LD      UNP_B1
                SC      IS_HEX
                JN      CH_RET
                SB      CH_0
                ML      TWO
                AD      CH_ANS
                MM      CH_ANS
  ;; Processa segundo byte
                LD      UNP_B2
                SC      IS_HEX
                JN      CH_RET
                SB      CH_0
                AD      CH_ANS
  ;; Valor da resposta está no acumulador!
CH_RET          RS      CHTOI

; -------------------------------------------------------------------
; Area de dados nova
; -------------------------------------------------------------------

EOL             K       /0A0A ; end of line

;Caracteres ASCII para analisador lexico

C_PLUS          K       /002B ; Cte "+" em ASCII
C_MINUS         K       /002D ; Cte "-" em ASCII
C_MULT          K       /002A ; Cte "*" em ASCII
C_DIV           K       /002F ; Cte "/" em ASCII
C_EQUAL         K       /003D ; Cte "=" em ASCII
C_GT            K       /003E ; Cte ">" em ASCII
C_LT            K       /003C ; Cte "<" em ASCII
C_PTO_VIR       K       /003B ; Cte ";" em ASCII
C_VIR           K       /002C ; Cte "," em ASCII
C_DOIS_PTOS     K       /003A ; Cte ":" em ASCII
C_BARRA         K       /002F ; Cte "/" em ASCII
C_L_PAR         K       /0028 ; Cte "(" em ASCII
C_R_PAR         K       /0029 ; Cte ")" em ASCII
C_L_CHA         K       /007B ; Cte "{" em ASCII
C_R_CHA         K       /007D ; Cte "}" em ASCII

C_SPACE         K       /0020 ; Cte [SPACE] em ASCII 

C_ZERO          K       /0030 ; Cte "0" em ASCII 
C_NOVE          K       /0039 ; Cte "9" em ASCII 

C_A_MSCLO       K       /0041 ; Cte "A" em ASCII 
C_Z_MSCLO       K       /005A ; Cte "Z" em ASCII 
C_A_MNSCLO      K       /0061 ; Cte "a" em ASCII 
C_Z_MNSCLO      K       /007A ; Cte "z" em ASCII 

K_00FF          K       /00FF ; cte 0x00FF
K_0FFE          K       /0FFE ; cte 0x0FFE
K_0800          K       /0800 ; cte 0x0800
K_0200          K       /0200 ; cte 0x0200
K_0000          K       /0000 ; cte 0x0000
K_0001          K       /0001 ; cte 0x0001
K_0002          K       /0002 ; cte 0x0002
K_0003          K       /0003 ; cte 0x0003
K_0004          K       /0004 ; cte 0x0004

CMD_LD          K       /8000 ; Instrucao LD (LOAD)
CMD_MM          K       /9000 ; Instrucao MM (Move to Memory)
CMD_GD          K       /D000  ; Instrucao GD (GET DATA)

ULJOB           K       /0300  ; Local onde esta o arquivo batch a ser lido

COMMAND         K       /0000 ; guarda o input de comando
CORRECT_CMD     K       /0000 ; guarda se o o comando esta correto ou nao 
TEMP            K       /0000 ; var temporaria

ERLEX           K       /0001 ; Cte erro de lexico
ERSIN           K       /0002 ; Cte erro de sintatico
ERSEM           K       /0003 ; Cte erro de semantico

ENDEXEC         K       /0000 ; endereco de execucao 

; -------------------------------------------------------------------
; Subrotina: ERRO_LEX
; Faz a rotina para erro de analisador lexico ER:LEX (01)
; -------------------------------------------------------------------
ERRO_LEX        K       /0000   ; endereco de retorno
                LD      ERLEX   ; carrega codigo de erro 
                OS      /0EE    ; escreve na tela o erro (cmd OS)
                HM      ERRO_LEX; para execucao

; -------------------------------------------------------------------
; Subrotina: ERRO_SIN
; Faz a rotina para erro de analisador sintatico ER:SIN (02)
; -------------------------------------------------------------------
ERRO_SIN        K       /0000   ; endereco de retorno
                LD      ERSIN   ; carrega codigo de erro 
                OS      /0EE    ; escreve na tela o erro (cmd OS)
                HM      ERRO_CMD; para execucao

; -------------------------------------------------------------------
; Subrotina: ERRO_SEM
; Faz a rotina para erro de analisador semantico ER:SEM (03)
; -------------------------------------------------------------------
ERRO_SEM        K       /0000   ; endereco de retorno
                LD      ERSEM   ; carrega codigo de erro 
                OS      /0EE    ; escreve na tela o erro (cmd OS)
                HM      ERRO_CMD; para execucao

; -------------------------------------------------------------------
; Subrotina: LEITURA
; Faz a leitura
; -------------------------------------------------------------------
LEITURA         K       /0000   ; endereco de retorno
                LD      ULJOB   ; faz o load do local do arquivo batch para leitura
                AD      LER     ; soma a instrucao de leitura (GD = GETDATA)
                MM      LENDO   ; guarda na endereco LENDO
LENDO           K       /0000   ; faz a leitura
                RS      LEITURA ; retorna

; -------------------------------------------------------------------
; Subrotina: LEITURA_TOTAL
; Faz a leitura total do arquivo a ser compilado
; -------------------------------------------------------------------
R_ADD           K       /0000       ; Endereco da ultima escrita

LEITURA_TOTAL   K       /0000       ; Endereco de retorno 
                LV      /000        ; Carrega 0 no acumulador
                MM      R_ADD       ; Coloca 0 no endereco da escrita
READ_LOOP       AD      MM_CMD      ; Adiciona com o comando de adicionar na memoria.
                AD      PROGRAMPTR  ; Adiciona ao endereco PROGRAM
                MM      MM_PROG     ; Armazena o comando para colocar no programa
                SC      LEITURA     ; chama subrotina de LEITURA atomica
MM_PROG         K       /0000       ; Espaco reservado para armazenar a memoria
                JZ      END_READ    ; Se for 0000, terminar leitura
                LD      K_0002      ; Se nao for 0000, somar 2 ao endereco
                AD      R_ADD       ; Soma 2 ao endereco R_ADD
                MM      R_ADD       ; Armazena o novo R_ADD
                JP      READ_LOOP   ; Retorna ao Loop de L_READ    
END_READ        LD      R_ADD       ; Load endereco final
                AD      PROGRAMPTR  ; Soma o endereco final com o endereco inicial
                MM      LAST_ADDR   ; Salva ultimo endereco
                RS      LEITURA_TOTAL ; Retorna da Leitura total

; -------------------------------------------------------------------
; Subrotina: READ_PROGRAM
; Leitura do programa salvo e escrito
; -------------------------------------------------------------------

CURRENT_ADDR    K       /0000       ; Proximo endereco a ser lido

READ_PROGRAM    K       /0000       ; Endereco de retorno
                LD      CURRENT_ADDR; Carrega o endereco atual
                AD      PROGRAMPTR  ; Soma o ponteiro do Programa
                AD      CMD_LD      ; Soma o comando de Load
                MM      READ_ADDR   ; Armazena o comando em READ_ADDR
                LD      CURRENT_ADDR; Carrega o endereco atual
                AD      K_0002      ; Soma 2 ao endereco lido para pegar o proximo
                MM      CURRENT_ADDR; Armazena o novo endereco a ser lido
READ_ADDR       K       /0000       ; Faz a leitura
                RS      READ_PROGRAM; Retorna

; -------------------------------------------------------------------
; Subrotina: IS_0to9
; Verifica se está entre [0,9] --> retorna 0 se sim, e 1 se não (!!!)
; -------------------------------------------------------------------
IS_0to9         K       /0000       ; Endereco de Retorno
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_ZERO      ; Subtrai zero em ASCII 
                JN      NAO_EH      ; Se LEX_WORD < '0': jump pra NAO_EH
                LD      C_NOVE      ; Load '9'
                SB      LEX_WORD    ; Subtrai LEX_WORD
                JN      NAO_EH      ; Se LEX_WORD > '9': jump pra NAO_EH
                LD      K_0000      ; Load zero
END_IS_0to9     RS      IS_0to9     ; Retorna  
NAO_EH          LD      K_0001      ; Load um 
                JP      END_IS_0to9 ; Jump pro retorno

; -------------------------------------------------------------------
; Subrotina: IS_atoZ
; Verifica se está entre [a, Z] --> retorna 0 se sim, e 1 se não (!!!)
; -------------------------------------------------------------------
IS_atoZ         K       /0000       ; Endereco de Retorno
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_A_MSCLO   ; Subtrai 'A' em ASCII 
                JN      NAO_EH2     ; Se LEX_WORD < 'A': jump pra NAO_EH2
                LD      C_Z_MNSCLO  ; Load 'z'
                SB      LEX_WORD    ; Subtrai LEX_WORD
                JN      NAO_EH2     ; Se LEX_WORD > 'z': jump pra NAO_EH2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_A_MNSCLO  ; Subtrai 'a' em ASCII 
                JN      PROB        ; Se LEX_WORD < 'a': jump pra PROB
                JP      OK          ; Jump pro OK
PROB            LD      C_Z_MSCLO   ; Load 'Z'
                SB      LEX_WORD    ; Subtrai LEX_WORD
                JN      NAO_EH2     ; Se LEX_WORD > 'Z': jump pra NAO_EH2
OK              LD      K_0000      ; Load zero
END_IS_atoZ     RS      IS_atoZ     ; Retorna  
NAO_EH2         LD      K_0001      ; Load um 
                JP      END_IS_atoZ ; Jump pro retorno

; -------------------------------------------------------------------
; Subrotina: IS_BAR
; Verifica "/"
; Retorna 0 se for "/" 
; -------------------------------------------------------------------
IS_BAR          K       /0000       ; endereco de retorno
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_BARRA     ; Subtrai "/" 
                JZ      OK3         ; Se LEX_WORD == '/': jump pra OK3
                LD      K_0001      ; Load um 
                JP      END_IS_BAR  ; Jump pro retorno
OK3             LD      K_0000      ; Load zero
END_IS_BAR      RS      IS_BAR      ; retorna

; -------------------------------------------------------------------
; Subrotina: IS_EQUAL
; Verifica "/"
; Retorna 0 se for "/" 
; -------------------------------------------------------------------
IS_EQUAL        K       /0000       ; endereco de retorno
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_EQUAL     ; Subtrai "=" 
                JZ      OK4         ; Se LEX_WORD == '=': jump pra OK4
                LD      K_0001      ; Load um 
                JP      END_IS_EQUAL; Jump pro retorno
OK4             LD      K_0000      ; Load zero
END_IS_EQUAL    RS      IS_EQUAL    ; retorna

; -------------------------------------------------------------------
; Subrotina: IS_BLANK
; Verifica "/"
; Retorna 0 se for "/" 
; -------------------------------------------------------------------
IS_BLANK        K       /0000       ; endereco de retorno
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_SPACE     ; Subtrai " " 
                JZ      OK5         ; Se LEX_WORD == ' ': jump pra OK4
                LD      K_0001      ; Load um 
                JP      END_IS_BLANK; Jump pro retorno
OK5             LD      K_0000      ; Load zero
END_IS_BLANK    RS      IS_BLANK    ; retorna
                
; -------------------------------------------------------------------
; Subrotina: IS_SPECIAL
; Verifica se é um dos caracteres especiais
; -------------------------------------------------------------------
IS_SPECIAL      K       /0000       ; Endereco de Retorno
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_MULT      ; Subtrai '*'
                JZ      OK2         ; Se LEX_WORD == '*': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_MINUS     ; Subtrai '-'
                JZ      OK2         ; Se LEX_WORD == '-': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_PLUS      ; Subtrai '+'
                JZ      OK2         ; Se LEX_WORD == '+': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_GT        ; Subtrai '>'
                JZ      OK2         ; Se LEX_WORD == '>': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_LT        ; Subtrai '<'
                JZ      OK2         ; Se LEX_WORD == '<': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_PTO_VIR   ; Subtrai ';'
                JZ      OK2         ; Se LEX_WORD == ';': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_VIR       ; Subtrai ','
                JZ      OK2         ; Se LEX_WORD == ',': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_DOIS_PTOS ; Subtrai ':'
                JZ      OK2         ; Se LEX_WORD == ':': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_L_PAR     ; Subtrai '('
                JZ      OK2         ; Se LEX_WORD == '(': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_R_PAR     ; Subtrai ')'
                JZ      OK2         ; Se LEX_WORD == ')': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_L_CHA     ; Subtrai '{'
                JZ      OK2         ; Se LEX_WORD == '{': jump pra OK2
                LD      LEX_WORD    ; Load LEX_WORD 
                SB      C_R_CHA     ; Subtrai '}'
                JZ      OK2         ; Se LEX_WORD == '}': jump pra OK2
NAO_EH3         LD      K_0001      ; Load um 
                JP      END_IS_SPECIAL ; Jump pro retorno
OK2             LD      K_0000      ; Load zero
END_IS_SPECIAL  RS      IS_0to9     ; Retorna  

                
; -------------------------------------------------------------------
; Subrotina: LEX_CALL
; Faz a chamada das rotinas de analise lexica
; -------------------------------------------------------------------
LEX_CONT        K       /0000       ; Indica qual word da vez

LEX_CALL        K       /0000       ; Endereco de retorno
                ;Fazer um loop que lê cada palavra em PROGRAM
                LD      K_0000      ; Load zero
                MM      CURRENT_ADDR; Guarda no CURRENT_ADDR
LEX_LOOP1       SC      READ_PROGRAM;
                SC      LEX_PARSE   ; Chama a rotina de parsing
                ;APLICAR O LEX STEP NO LEX CALL
                RS      LEX_CALL    ; Retorna 

; -------------------------------------------------------------------
; Subrotina: LEX_STEP
; Faz uma transicao na maquina de estados da analise lexical
; -------------------------------------------------------------------

LEX_WORD        K       /0000       ; Word analisada da vez 
LEX_STATE       K       /0000       ; Estado atual da analise lexical

LEX_STEP        K       /0000       ; Entrada do subrotina
                MM      LEX_WORD    ; Armazena o que está no acumulador em LEX_WORD
                LD      LEX_STATE   ; Carrega o LEX_STATE
                JZ      LEX_STATE0  ; Desvia se o estado for 0
                SB      K_0001      ; 
                JZ      LEX_STATE1  ; Desvia se o estado for 1
                SB      K_0001      ;
                JZ      LEX_STATE2  ; Desvia se o estado for 2
                SB      K_0001      ;
                JZ      LEX_STATE3  ; Desvia se o estado for 3
                SB      K_0001      ;
                JZ      LEX_STATE4  ; Desvia se o estado for 4
                SB      K_0001      ;
                JZ      LEX_STATE5  ; Desvia se o estado for 5
                SB      K_0001      ;
                JZ      LEX_STATE6  ; Desvia se o estado for 5
                chama o erro gostoso
                              
LEX_STATE0      SC      IS_BLANK    ; No estado 0, checa se for 0
                JZ      LEX_END_STEP; Se for 0, mantém o estado
                SC      IS_atoZ     ; 
                JZ      L_S0_ATOZ   ; Desvia para L_S0_ATOZ se for a até Z
                SC      IS_0to9     ;
                JZ      L_S0_0TO9   ; Desvia para L_S0_0to9 se for numeral
                SC      IS_BAR      ;
                JZ      L_S0_BAR    ; Desvia para L_S0_BAR se for /
                SC      IS_EQUAL    ; 
                JZ      L_S0_EQUAL  ; Desvia para L_S0_EQUAL se for =
                SC      IS_SPECIAL  ; 
                JZ      L_S0_SPECIAL; Desvia para L_S0_SPECIAL se for um dos caracteres especiais
                SC      ERRO_LEX    ; Dá erro se nao for nenhum desses

L_S0_ATOZ       LD      K_0001      ;
                MM      LEX_STATE   ; Altera o estado para 1
                JP      LEX_END_STEP; Finaliza o Step
L_S0_0TO9       LD      K_0002      ;
                MM      LEX_STATE   ; Altera o estado para 2
                JP      LEX_END_STEP; Finaliza o Step
L_S0_BAR        LD      K_0005      ;
                MM      LEX_STATE   ; Altera o estado para 5
                JP      LEX_END_STEP; Finaliza o Step
L_S0_EQUAL      LD      K_0006      ;
                MM      LEX_STATE   ; Altera o estado para 6
                JP      LEX_END_STEP; Finaliza o Step
L_S0_SPECIAL    LD      K_0004      ;
                MM      LEX_STATE   ; Altera para o estado 4
                JP      LEX_END_STEP; Finaliza o Step
                
LEX_STATE1      SC      IS_BLANK    ; No estado 1, checa se for espaco
                JZ      L_SX_BLANK  ; Se for espaco, vai para L_SX_BLANK
                SC      IS_atoZ     ; 
                JZ      LEX_END_STEP; Se for a até Z, mantem no estado 1
                SC      IS_0to9     ; 
                JZ      LEX_END_STEP; Se for 0 até 9, mantém no estado 1
                SC      ERRO_LEX    ; Da erro se nao for nenhum

LEX_STATE2      SC      IS_BLANK    ; No estado 2, checa se for espaco
                JZ      L_SX_BLANK  ; Se for espaco, vai para L_SX_BLANK
                SC      IS_0to9     ;
                JZ      LEX_END_STEP; Se for 0 ate 9, mantém no estado 2
                SC      ERRO_LEX    ; Da erro se nao for nenhum

LEX_STATE3      LD      LEX_WORD    ; No estado 3, checa se for end of line
                SB      EOL         ;
                JZ      L_S3_EOL    ;
                JP      LEX_END_STEP; Se nao for end of line, vale tudo pq eh comentario

L_S3_EOL        LD      K_0000      ; Se for end of line, no estado tres retorna ao estado 0
                MM      LEX_STATE   ;
                JP      LEX_END_STEP;

LEX_STATE4      SC      IS_BLANK    ; No estado 4, so eh valido ter espaco
                JZ      L_SX_BLANK  ; Se for espaco, vai para L_SX_BLANK
                SC      ERRO_LEX    ; Se nao for espaco, da erro

LEX_STATE5      SC      IS_BLANK    ; Checa se eh espaco
                JZ      L_SX_BLANK  ; Se for espaco, vai para L_SX_BLANK
                SC      IS_BAR      ; Checa se tem uma barra
                JZ      L_S5_BAR    ; Se tiver uma barra, vai para L_S5_BAR
                SC      ERRO_LEX    ; Se nao for barra, da erro

LEX_STATE6      SC      IS_EQUAL    ; Checa se eh =
                JZ      L_S6_EQUAL  ; Se for =, desvia para L_S6_EQUAL
                SC      IS_BLANK    ; Checa se eh espaco
                JZ      L_SX_BLANK  ; Se for espaco, vai para L_SX_BLANK
                SC      ERRO_LEX    ; Se nao for nenhum dos dois, da erro

L_S6_EQUAL      LD      K_0004      ; Se for =, vai para o estado 4
                MM      LEX_STATE   ;
                JP      LEX_END_STEP;   

L_SX_BLANK      LD      K_0000      ; Desvio usual, que redireciona para o estado 0
                MM      LEX_STATE   ; Quando encontra um espaco
                JP      LEX_END_STEP;

LEX_END_STEP    RS      LEX_STEP    ;

; -------------------------------------------------------------------
; Subrotina: LEX_PARSE
; Faz um passo da analise lexica para duas letras
; -------------------------------------------------------------------
   
LEX_WORD1       K       /0000       ; Primeiro char lido
LEX_WORD2       K       /0000       ; Segundo char lido

LEX_PARSE       K       /0000       ; Endereco de retorno
                MM      LEX_WORD2   ; Armazena a palavra em LEX_WORD2
                DV      SHIFT       ; Shifta para a direita
                MM      LEX_WORD1   ; Armazena o primeiro char lido
                LD      LEX_WORD2   ; Carrega a palavra lida
                ML      SHIFT       ; Shifta para a esquerda
                DV      SHIFT       ; Shifta para a direita
                MM      LEX_WORD2   ; Armazena a segunda palavra lida
                RS      LEX_PARSE   ; Retorna  

; -------------------------------------------------------------------
; Subrotina: PROCESS
; Faz a chamada do CHTOIN e guarda em TEMP
; -------------------------------------------------------------------
PROCESS         K       /0000   ; endereco de retorno
                SC      LEITURA ; chama subrotina de LEITURA
                MM      CH_IN_A ; guarda em CH_IN_A
                SC      LEITURA ; chama subrotina de LEITURA
                MM      CH_IN_B ; guarda em CH_IN_B
                SC      CHTOI   ; chama subrotina CHTOI
                MM      TEMP    ; guarda o output na var TEMP           
                RS      PROCESS ; retorna

; -------------------------------------------------------------------
; Subrotina: INITJOB
; Verifica "//"
; Retorna 0 se for "//" 
; -------------------------------------------------------------------
INITJOB         K       /0000       ; endereco de retorno
                SC      CHECK_HEAD  ; subrotina verifica //JB
                JZ      LOOP1       ; se for zero (nao deu erro), jump para LOOP1
                JP      FIM_INITJOB ; se der erro, jump pro FIM_INITJOB
LOOP1           SC      LEITURA     ; subrotina de leitura
                SC      CHECK_2BARS ; subrotina verifica //
                JZ      FLAG1       ; se for zero (nao deu erro), jump para FLAG1
                SC      ERRO_CMD    ; se deu erro, chama subrotina ERRO_CMD
FLAG1           SC      CHECK_CMD   ; chama subrotina para checar comando
                JZ      FIM_INITJOB ; se for zero, jump pro FIM_INITJOB
                SC      LEITURA     ; chama subrotina de LEITURA
                SB      EOL         ; subtrai EOL
                JZ      FLAG2       ; se for EOL, jump pra FLAG2  
                SC      ERRO_ARG    ; caso contrario, chama erro 
FLAG2           SC      CHECK_FIMJ  ; chama subrotina verifica fim do job
                JZ      FIM_INITJOB ; se terminou, pula para FIM_INITJOB
                JP      LOOP1       ; caso contrario, volta pro LOOP1
FIM_INITJOB     RS      INITJOB     ; retorna

; -------------------------------------------------------------------
; MAIN FUNCTION
; -------------------------------------------------------------------
INI             SC      INITJOB     ; Jump pro comeco do Job
FIM             HM      FIM         ; Fim do programa

PROGRAMPTR      K       PROGRAM     ; Ponteiro que aponta para o inicio do programa
LAST_ADDR       K       /0000       ;
PROGRAM         K       /0000       ; Salva o progama lido

# MAIN