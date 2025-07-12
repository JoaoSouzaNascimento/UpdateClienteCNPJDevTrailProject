       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEVXXP10.
      *
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DADOSCLI ASSIGN TO DADOSCLI
               FILE STATUS IS WS-FS-DADOSCLI.
           SELECT RLINCONS ASSIGN TO RLINCONS
               FILE STATUS IS WS-FS-RLINCONS.
      *
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD DADOSCLI
           RECORDING MODE F
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01 REG-DADOSCLI             PIC X(80).
      * 
       FD RLINCONS
           RECORDING MODE F
           RECORD CONTAINS 132 CHARACTERS. 
       01 REG-RLINCONS             PIC X(132).
      * 
       WORKING-STORAGE SECTION.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLIENTPJ END-EXEC.
      *
       COPY DEVRELTO.
       COPY DEVBKCLI.
      *
       01 WS-FS-DADOSCLI           PIC X(002) VALUE SPACES.
       01 WS-FS-RLINCONS           PIC X(002) VALUE SPACES.
       01 WS-CONTROLE.
           05 WS-REG-LIDOS         PIC 9(005) VALUE ZEROS.
           05 WS-REG-ATUAL         PIC 9(005) VALUE ZEROS.
           05 WS-REG-DESP          PIC 9(005) VALUE ZEROS.
       01 WS-FIM-ARQUIVO           PIC X(001) VALUE 'N'.
      * 
       01 WRK-DEVCDATA.
           05 WRK-DATADEV          PIC 9(8) VALUE ZEROS.
           05 WRK-CODRDEV          PIC X(2) VALUE SPACES.
      * 
       01 WRK-AREACNPJ.
           05 WRK-DADOS-CNPJ.
               10 WRK-CNPJ         PIC X(8).
               10 WRK-FILIAL       PIC X(4).
               10 WRK-CONTROLE     PIC 9(2).
           05 WRK-CODRCNPJ         PIC X(002).
      * 
       01 WRK-DATA-PROCESSAMENTO   PIC 9(8).
       01 WRK-HORA-PROCESSAMENTO   PIC 9(6).
       01 WRK-PROG-CNPJ            PIC X(8) VALUE 'DEVXXCPJ'.
       01 WRK-PROG-DAT             PIC X(8) VALUE 'DEVCDATA'.
       01 WS-PAGINA                PIC 9(03) VALUE ZERO.
       01 LD2-ERRO                 PIC X(18) VALUE SPACES.
      *
      * 
       PROCEDURE DIVISION.
      * 
       0000-INICIO.
           DISPLAY 'DEVXXP10   INICIO DO PROCESSAMENTO'.
           ACCEPT WRK-DATA-PROCESSAMENTO FROM DATE YYYYMMDD.
           ACCEPT WRK-HORA-PROCESSAMENTO FROM TIME.

           OPEN INPUT DADOSCLI
                OUTPUT RLINCONS.
      * 
           PERFORM 1500-VALIDA-ABERTURA-ARQUIVOS
      *
           ADD 1 TO WS-PAGINA
           MOVE WS-PAGINA TO CB1-PAG
           WRITE REG-RLINCONS FROM CABEC1.
           WRITE REG-RLINCONS FROM CABEC2.
      *      
           PERFORM 1600-VALIDA-ARQUIVO-VAZIO
      * 
           PERFORM UNTIL WS-FIM-ARQUIVO = 'S'
               ADD 1 TO WS-REG-LIDOS
               PERFORM 1000-TRATA-REGISTRO
               READ DADOSCLI INTO PRF-DADOSCLI
                   AT END MOVE 'S' TO WS-FIM-ARQUIVO
               END-READ
           END-PERFORM
      * 
           DISPLAY 'DEVXXP10-TOTAL DE REGISTROS LIDOS......: ' 
               WS-REG-LIDOS
           DISPLAY 'DEVXXP10-TOTAL DE REGISTROS ATUALIZADOS: ' 
               WS-REG-ATUAL
           DISPLAY 'DEVXXP10-TOTAL DE REGISTROS DESPREZADOS: '
               WS-REG-DESP
           DISPLAY 'DEVXXP10-PROCESSAMENTO ENCERRADO'
           PERFORM 9999-FIM.
      *     
       1000-TRATA-REGISTRO.
           EVALUATE TRUE
             WHEN NOVO-CLIENTE
               PERFORM 2000-PROCESSA-NOVO
             WHEN ATUALIZACAO
               PERFORM 3000-PROCESSA-ATUAL
             WHEN INATIVACAO
               PERFORM 4000-PROCESSA-INATIVACAO
             WHEN OTHER
               MOVE 'OPERACAO INVALIDA' TO LD2-ERRO
               PERFORM 8000-REG-INVALIDO
           END-EVALUATE.

       2000-PROCESSA-NOVO.
           IF PRF-CODIGOCLI = 0
              MOVE 'ERRO NUM. CLIENTE' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF
           IF PRF-RAZAOSOCIAL = SPACES
              MOVE 'ERRO RAZAO SOCIAL' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF
           IF PRF-CNPJ = SPACES OR PRF-FILIAL = SPACES
              MOVE 'ERRO CNPJ' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF
           MOVE PRF-CNPJ TO WRK-CNPJ
           MOVE PRF-FILIAL TO WRK-FILIAL
           MOVE PRF-CONTROLE TO WRK-CONTROLE
           CALL WRK-PROG-CNPJ USING WRK-AREACNPJ
           IF WRK-CODRCNPJ NOT = 'OK'
              MOVE 'ERRO CNPJ' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF
           IF PRF-VLRULTCOMPRA = 0
              MOVE 'ERRO VALOR' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF
           MOVE PRF-DATAOPER TO WRK-DATADEV
           PERFORM 1400-CALL-DEVCDATA
           IF WRK-CODRDEV NOT = 'OK'
              MOVE 'ERRO DATA' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF
           IF PRF-DATAOPER > WRK-DATA-PROCESSAMENTO
              MOVE 'ERRO DATA' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF

      *--- MOVENDO DADOS PARA VARIAVEIS HOST ---
           MOVE PRF-CODIGOCLI       TO CODIGO-CLI
           MOVE PRF-RAZAOSOCIAL     TO RAZSOCIAL-CLI
           MOVE PRF-CNPJ            TO NUMECNPJ-CLI
           MOVE PRF-FILIAL          TO FILIALCNPJ-CLI
           MOVE PRF-CONTROLE        TO CTLCNPJ-CLI
           MOVE PRF-VLRULTCOMPRA    TO VRULTCOMPRA-CLI
           MOVE PRF-DATAOPER        TO DTULTCOMPRA-CLI
           MOVE WRK-DATA-PROCESSAMENTO TO DTATLZDADOS-CLI

           EXEC SQL
               SELECT CODIGO_CLI INTO :CODIGO-CLI
                 FROM ALUNOXX.CLIENTPJ
                WHERE CODIGO_CLI = :CODIGO-CLI
           END-EXEC

           IF SQLCODE NOT = 0
               EXEC SQL
                   INSERT INTO ALUNOXX.CLIENTPJ (
                       CODIGO_CLI, RAZSOCIAL_CLI, 
                       NUMECNPJ_CLI, FILIALCNPJ_CLI, 
                       CTLCNPJ_CLI, VRULTCOMPRA_CLI, 
                       DTULTCOMPRA_CLI, DTATLZDADOS_CLI
                   ) VALUES (
                       :CODIGO-CLI, :RAZSOCIAL-CLI, 
                       :NUMECNPJ-CLI, :FILIALCNPJ-CLI, 
                       :CTLCNPJ-CLI, :VRULTCOMPRA-CLI,
                       :DTULTCOMPRA-CLI, :DTATLZDADOS-CLI
                   )
               END-EXEC

               IF SQLCODE NOT = 0
                   MOVE 'ERRO INSERT' TO LD2-ERRO
                   PERFORM 8000-REG-INVALIDO
               END-IF
           ELSE
               MOVE 'ERRO NUM CLIENTE' TO LD2-ERRO
               PERFORM 8000-REG-INVALIDO
           END-IF

           ADD 1 TO WS-REG-ATUAL
           PERFORM 7000-IMPRIME-OK.

       3000-PROCESSA-ATUAL.
           IF PRF-CODIGOCLI = 0
              MOVE 'ERRO NUM. CLIENTE' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF

           EXEC SQL
               SELECT CODIGO_CLI INTO :CODIGO-CLI
                 FROM ALUNOXX.CLIENTPJ
                WHERE CODIGO_CLI = :CODIGO-CLI
                  AND DTINATIVA_CLI IS NULL
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 'ERRO NUM CLIENTE' TO LD2-ERRO
               PERFORM 8000-REG-INVALIDO
           END-IF

           IF (PRF-CNPJ NOT = SPACES) OR
               (PRF-FILIAL NOT = SPACES) OR 
               (PRF-CONTROLE NOT = 0) OR 
               (PRF-VLRULTCOMPRA NOT = 0)

               MOVE PRF-CNPJ TO WRK-CNPJ
               MOVE PRF-FILIAL TO WRK-FILIAL
               MOVE PRF-CONTROLE TO WRK-CONTROLE
               CALL WRK-PROG-CNPJ USING WRK-AREACNPJ
               
               IF (WRK-CODRCNPJ NOT = 'OK')
                   MOVE 'ERRO CNPJ' TO LD2-ERRO
                   PERFORM 8000-REG-INVALIDO
               END-IF
           END-IF

           IF PRF-DATAOPER NOT = 0
               MOVE PRF-DATAOPER TO WRK-DATADEV
               PERFORM 1400-CALL-DEVCDATA
               IF WRK-CODRDEV NOT = 'OK'
                   MOVE 'ERRO DATA' TO LD2-ERRO
                   PERFORM 8000-REG-INVALIDO
               END-IF
               IF PRF-DATAOPER > WRK-DATA-PROCESSAMENTO
                   MOVE 'ERRO DATA' TO LD2-ERRO
                   PERFORM 8000-REG-INVALIDO
               END-IF
           END-IF

           MOVE PRF-CODIGOCLI       TO CODIGO-CLI
           MOVE PRF-RAZAOSOCIAL     TO RAZSOCIAL-CLI
           MOVE PRF-CNPJ            TO NUMECNPJ-CLI
           MOVE PRF-FILIAL          TO FILIALCNPJ-CLI
           MOVE PRF-CONTROLE        TO CTLCNPJ-CLI
           MOVE PRF-VLRULTCOMPRA    TO VRULTCOMPRA-CLI
           MOVE PRF-DATAOPER        TO DTULTCOMPRA-CLI
           MOVE WRK-DATA-PROCESSAMENTO TO DTATLZDADOS-CLI

           EXEC SQL
               UPDATE ALUNOXX.CLIENTPJ SET
                 RAZSOCIAL_CLI    = :RAZSOCIAL-CLI,
                 NUMECNPJ_CLI     = :NUMECNPJ-CLI,
                 FILIALCNPJ_CLI   = :FILIALCNPJ-CLI,
                 CTLCNPJ_CLI      = :CTLCNPJ-CLI,
                 VRULTCOMPRA_CLI  = :VRULTCOMPRA-CLI,
                 DTULTCOMPRA_CLI  = :DTULTCOMPRA-CLI,
                 DTATLZDADOS_CLI  = :DTATLZDADOS-CLI
               WHERE CODIGO_CLI = :CODIGO-CLI
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 'ERRO UPDATE' TO LD2-ERRO
               PERFORM 8000-REG-INVALIDO
           END-IF

           ADD 1 TO WS-REG-ATUAL
           PERFORM 7000-IMPRIME-OK.

       4000-PROCESSA-INATIVACAO.
           IF PRF-CODIGOCLI = 0
              MOVE 'ERRO NUM. CLIENTE' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF

           EXEC SQL
               SELECT CODIGO_CLI INTO :CODIGO-CLI
                 FROM ALUNOXX.CLIENTPJ
                WHERE CODIGO_CLI = :CODIGO-CLI
                  AND DTINATIVA_CLI IS NULL
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 'ERRO NUM CLIENTE' TO LD2-ERRO
               PERFORM 8000-REG-INVALIDO
           END-IF

           MOVE PRF-DATAOPER TO WRK-DATADEV
           PERFORM 1400-CALL-DEVCDATA
           IF WRK-CODRDEV NOT = 'OK'
              MOVE 'ERRO DATA' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF

           IF PRF-DATAOPER > WRK-DATA-PROCESSAMENTO
              MOVE 'ERRO DATA' TO LD2-ERRO
              PERFORM 8000-REG-INVALIDO
           END-IF

           MOVE PRF-CODIGOCLI       TO CODIGO-CLI
           MOVE PRF-DATAOPER        TO DTULTCOMPRA-CLI
           MOVE WRK-DATA-PROCESSAMENTO TO DTATLZDADOS-CLI

           EXEC SQL
               UPDATE ALUNOXX.CLIENTPJ SET
                 DTINATIVA_CLI    = :DTULTCOMPRA-CLI,
                 DTATLZDADOS_CLI  = :DTATLZDADOS-CLI
               WHERE CODIGO_CLI = :CODIGO-CLI
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 'ERRO UPDATE' TO LD2-ERRO
               PERFORM 8000-REG-INVALIDO
           END-IF

           ADD 1 TO WS-REG-ATUAL
           PERFORM 7000-IMPRIME-OK.

       7000-IMPRIME-OK.
           MOVE PRF-OPERACAO TO LD1-OPER
           EVALUATE PRF-OPERACAO
             WHEN 'N' MOVE 'INSERCAO' TO LD1-DESCOPER
             WHEN 'A' MOVE 'ATUALIZACAO' TO LD1-DESCOPER
             WHEN 'I' MOVE 'INATIVACAO' TO LD1-DESCOPER
             WHEN OTHER MOVE 'DESCONHECIDA' TO LD1-DESCOPER
           END-EVALUATE
           MOVE 'OPERACAO REALIZADA' TO LD1-RESULTADO
           WRITE REG-RLINCONS FROM LINDET1

           MOVE PRF-CODIGOCLI TO LD3-NUMCLI
           MOVE PRF-RAZAOSOCIAL TO LD3-RAZSOCIAL
           WRITE REG-RLINCONS FROM LINDET3.

       8000-REG-INVALIDO.
           ADD 1 TO WS-REG-DESP
           MOVE PRF-OPERACAO TO LD1-OPER
           EVALUATE PRF-OPERACAO
             WHEN 'N' MOVE 'INSERCAO' TO LD1-DESCOPER
             WHEN 'A' MOVE 'ATUALIZACAO' TO LD1-DESCOPER
             WHEN 'I' MOVE 'INATIVACAO' TO LD1-DESCOPER
             WHEN OTHER MOVE 'DESCONHECIDA' TO LD1-DESCOPER
           END-EVALUATE
           MOVE 'REG INCONSISTENTE' TO LD1-RESULTADO
           WRITE REG-RLINCONS FROM LINDET1

           MOVE LD2-ERRO TO LD2-RESULTADO
           WRITE REG-RLINCONS FROM LINDET2

           MOVE PRF-CODIGOCLI TO LD3-NUMCLI
           MOVE PRF-RAZAOSOCIAL TO LD3-RAZSOCIAL
           WRITE REG-RLINCONS FROM LINDET3.
      
       1400-CALL-DEVCDATA.
           CALL WRK-PROG-DAT USING WRK-DEVCDATA.

       1500-VALIDA-ABERTURA-ARQUIVOS.
           IF WS-FS-DADOSCLI NOT = '00'
               DISPLAY 'DEVXXP10   ERRO ABRIR DADOSCLI'
               PERFORM 9999-FIM
           END-IF

           IF WS-FS-RLINCONS NOT = '00'
               DISPLAY 'DEVXXP10   ERRO ABRIR RLINCONS'
               PERFORM 9999-FIM
           END-IF.

       1600-VALIDA-ARQUIVO-VAZIO.
           READ DADOSCLI INTO PRF-DADOSCLI
               AT END MOVE 'S' TO WS-FIM-ARQUIVO
           END-READ

           IF WS-FIM-ARQUIVO = 'S'
               DISPLAY 'DEVXXP10   ARQUIVO DADOSCLI VAZIO'
               MOVE 4 TO RETURN-CODE
               PERFORM 9999-FIM
           END-IF.

       9999-FIM.
           CLOSE DADOSCLI RLINCONS
           GOBACK.
