       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEV06P10.
      *
      *
       EXEC SQL INCLUDE CLIENTPJ END-EXEC.
      *
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-DADOSCLI ASSIGN TO 'DADOSCLI'
               FILE STATUS IS WS-FS-DADOSCLI.
           SELECT ARQ-RLINCONS ASSIGN TO 'RLINCONS'
               FILE STATUS IS WS-FS-RLINCONS.
      *
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD ARQ-DADOSCLI
           RECORDING MODE F
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01 REG-DADOSCLI             PIC X(80).
      * 
       FD ARQ-RLINCONS
           RECORDING MODE F
           RECORD CONTAINS 132 CHARACTERS. 
       01 REG-RLINCONS             PIC X(132).
      * 
       WORKING-STORAGE SECTION.
      * 
       01 WS-CLIENTE-HOST.
           05 WS-CODIGOCLI-HOST         PIC 9(9).
           05 WS-RAZAOSOCIAL-HOST       PIC X(60).
           05 WS-CNPJ-HOST              PIC X(8).
           05 WS-FILIAL-HOST            PIC X(4).
           05 WS-CONTROLE-HOST          PIC 9(2).
           05 WS-VLRULTCOMPRA-HOST      PIC S9(9)V99.
           05 WS-DATAOPER-HOST          PIC 9(8).
           05 WS-DATA-PROCESSAMENTO-HOST PIC 9(8).
      *
       01 WS-FS-DADOSCLI           PIC XX VALUE SPACES.
       01 WS-FS-RLINCONS           PIC XX VALUE SPACES.
       01 WS-CONTROLE.
           05 WS-REG-LIDOS         PIC 9(5) VALUE ZEROS.
           05 WS-REG-ATUAL         PIC 9(5) VALUE ZEROS.
           05 WS-REG-DESP          PIC 9(5) VALUE ZEROS.
       01 WS-FIM-ARQUIVO           PIC X VALUE 'N'.
      * 
      * COPY DEVBKCLI.
      * Descomente o copy acima e comente o abaixo para usar o copybook
       01 PRF-DADOSCLI.
            10 PRF-OPERACAO            PIC X(001)    VALUE SPACE.
               88 NOVO-CLIENTE                       VALUE 'N'.
               88 ATUALIZACAO                        VALUE 'A'.
               88 INATIVACAO                         VALUE 'I'.
            10 PRF-CODIGOCLI           PIC 9(005)    VALUE ZEROS.
            10 PRF-RAZAOSOCIAL         PIC X(040)    VALUE SPACE.
            10 PRF-CNPJ                PIC X(009)    VALUE SPACE.
            10 PRF-FILIAL              PIC X(004)    VALUE SPACE.
            10 PRF-CONTROLE            PIC 9(002)    VALUE ZEROS.
            10 PRF-VLRULTCOMPRA COMP-3 PIC 9(011)V99 VALUE ZEROS.
            10 PRF-DATAOPER            PIC 9(008)    VALUE ZEROS.
            10 PRF-RESERVA             PIC X(004)    VALUE SPACE.
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
      * 
       01 WRK-DATA-PROCESSAMENTO   PIC 9(8).
       01 WRK-HORA-PROCESSAMENTO   PIC 9(6).
       01 WRK-PROG-CNPJ            PIC X(8) VALUE 'DEV06CPJ'.
       01 WRK-PROG-DAT             PIC X(8) VALUE 'DEVCDATA'.
      * 
       PROCEDURE DIVISION.
      * 
       0000-INICIO.
           DISPLAY 'DEV06P10   INICIO DO PROCESSAMENTO'.
           ACCEPT WRK-DATA-PROCESSAMENTO FROM DATE YYYYMMDD.
           ACCEPT WRK-HORA-PROCESSAMENTO FROM TIME.
           OPEN INPUT ARQ-DADOSCLI
                OUTPUT ARQ-RLINCONS.
      * 
           IF WS-FS-DADOSCLI NOT = '00'
               DISPLAY 'DEV06P10   ERRO ABRIR DADOSCLI'
               PERFORM 9999-FIM
               STOP RUN
           END-IF
      * 
           IF WS-FS-RLINCONS NOT = '00'
               DISPLAY 'DEV06P10   ERRO ABRIR RLINCONS'
               PERFORM 9999-FIM
               STOP RUN
           END-IF
      * 
           READ ARQ-DADOSCLI INTO PRF-DADOSCLI
               AT END MOVE 'S' TO WS-FIM-ARQUIVO
           END-READ
      * 
           IF WS-FIM-ARQUIVO = 'S'
               DISPLAY 'DEV06P10   ARQUIVO DADOSCLI VAZIO'
               MOVE 4 TO RETURN-CODE
               PERFORM 9999-FIM
               STOP RUN
           END-IF
      * 
           PERFORM UNTIL WS-FIM-ARQUIVO = 'S'
               ADD 1 TO WS-REG-LIDOS
               PERFORM 1000-TRATA-REGISTRO
               READ ARQ-DADOSCLI INTO PRF-DADOSCLI
                   AT END MOVE 'S' TO WS-FIM-ARQUIVO
               END-READ
           END-PERFORM
      * 
           DISPLAY 'DEV06P10-TOTAL DE REGISTROS LIDOS......: ' 
               WS-REG-LIDOS
           DISPLAY 'DEV06P10-TOTAL DE REGISTROS ATUALIZADOS: ' 
               WS-REG-ATUAL
           DISPLAY 'DEV06P10-TOTAL DE REGISTROS DESPREZADOS: '
               WS-REG-DESP
           DISPLAY 'DEV06P10-PROCESSAMENTO ENCERRADO'
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
           MOVE PRF-CODIGOCLI       TO WS-CODIGOCLI-HOST
           MOVE PRF-RAZAOSOCIAL     TO WS-RAZAOSOCIAL-HOST
           MOVE PRF-CNPJ            TO WS-CNPJ-HOST
           MOVE PRF-FILIAL          TO WS-FILIAL-HOST
           MOVE PRF-CONTROLE        TO WS-CONTROLE-HOST
           MOVE PRF-VLRULTCOMPRA    TO WS-VLRULTCOMPRA-HOST
           MOVE PRF-DATAOPER        TO WS-DATAOPER-HOST
           MOVE WRK-DATA-PROCESSAMENTO TO WS-DATA-PROCESSAMENTO-HOST

           EXEC SQL
               SELECT CODIGO_CLI INTO :WS-CODIGOCLI-HOST
                 FROM ALUNO06.CLIENTPJ
                WHERE CODIGO_CLI = :WS-CODIGOCLI-HOST
           END-EXEC

           IF SQLCODE NOT = 0
               EXEC SQL
                   INSERT INTO ALUNO06.CLIENTPJ (
                       CODIGO_CLI, RAZSOCIAL_CLI, NUMECNPJ_CLI, FILIALCNPJ_CLI,
                       CTLCNPJ_CLI, VRULTCOMPRA_CLI, DTULTCOMPRA_CLI, DTATLZDADOS_CLI
                   ) VALUES (
                       :WS-CODIGOCLI-HOST, :WS-RAZAOSOCIAL-HOST, :WS-CNPJ-HOST,
                       :WS-FILIAL-HOST, :WS-CONTROLE-HOST, :WS-VLRULTCOMPRA-HOST,
                       :WS-DATAOPER-HOST, :WS-DATA-PROCESSAMENTO-HOST
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
               SELECT CODIGO_CLI INTO :WS-CODIGOCLI-HOST
                 FROM ALUNO06.CLIENTPJ
                WHERE CODIGO_CLI = :PRF-CODIGOCLI
                  AND DTINATIVA_CLI IS NULL
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 'ERRO NUM CLIENTE' TO LD2-ERRO
               PERFORM 8000-REG-INVALIDO
           END-IF

           IF PRF-CNPJ NOT = SPACES OR PRF-FILIAL NOT = SPACES OR PRF-CONTROLE NOT = 0 OR PRF-VLRULTCOMPRA NOT = 0
               MOVE PRF-CNPJ TO WRK-CNPJ
               MOVE PRF-FILIAL TO WRK-FILIAL
               MOVE PRF-CONTROLE TO WRK-CONTROLE
               CALL WRK-PROG-CNPJ USING WRK-AREACNPJ
               IF WRK-CODRCNPJ NOT = 'OK'
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

           MOVE PRF-CODIGOCLI       TO WS-CODIGOCLI-HOST
           MOVE PRF-RAZAOSOCIAL     TO WS-RAZAOSOCIAL-HOST
           MOVE PRF-CNPJ            TO WS-CNPJ-HOST
           MOVE PRF-FILIAL          TO WS-FILIAL-HOST
           MOVE PRF-CONTROLE        TO WS-CONTROLE-HOST
           MOVE PRF-VLRULTCOMPRA    TO WS-VLRULTCOMPRA-HOST
           MOVE PRF-DATAOPER        TO WS-DATAOPER-HOST
           MOVE WRK-DATA-PROCESSAMENTO TO WS-DATA-PROCESSAMENTO-HOST

           EXEC SQL
               UPDATE ALUNO06.CLIENTPJ SET
                 RAZSOCIAL_CLI    = :WS-RAZAOSOCIAL-HOST,
                 NUMECNPJ_CLI     = :WS-CNPJ-HOST,
                 FILIALCNPJ_CLI   = :WS-FILIAL-HOST,
                 CTLCNPJ_CLI      = :WS-CONTROLE-HOST,
                 VRULTCOMPRA_CLI  = :WS-VLRULTCOMPRA-HOST,
                 DTULTCOMPRA_CLI  = :WS-DATAOPER-HOST,
                 DTATLZDADOS_CLI  = :WS-DATA-PROCESSAMENTO-HOST
               WHERE CODIGO_CLI = :WS-CODIGOCLI-HOST
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
               SELECT CODIGO_CLI INTO :WS-CODIGOCLI-HOST
                 FROM ALUNO06.CLIENTPJ
                WHERE CODIGO_CLI = :PRF-CODIGOCLI
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

           MOVE PRF-CODIGOCLI       TO WS-CODIGOCLI-HOST
           MOVE PRF-DATAOPER        TO WS-DATAOPER-HOST
           MOVE WRK-DATA-PROCESSAMENTO TO WS-DATA-PROCESSAMENTO-HOST

           EXEC SQL
               UPDATE ALUNO06.CLIENTPJ SET
                 DTINATIVA_CLI    = :WS-DATAOPER-HOST,
                 DTATLZDADOS_CLI  = :WS-DATA-PROCESSAMENTO-HOST
               WHERE CODIGO_CLI = :WS-CODIGOCLI-HOST
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 'ERRO UPDATE' TO LD2-ERRO
               PERFORM 8000-REG-INVALIDO
           END-IF

           ADD 1 TO WS-REG-ATUAL
           PERFORM 7000-IMPRIME-OK.

       7000-IMPRIME-OK.
           STRING 'OPERACAO: ' PRF-OPERACAO DELIMITED BY SIZE
                  '  RESULTADO: OPERACAO REALIZADA'
             INTO REG-RLINCONS
           END-STRING
           WRITE REG-RLINCONS.

       8000-REG-INVALIDO.
           ADD 1 TO WS-REG-DESP
           STRING 'OPERACAO: ' PRF-OPERACAO DELIMITED BY SIZE
                  '  RESULTADO: REG INCONSISTENTE'
             INTO REG-RLINCONS
           END-STRING
           WRITE REG-RLINCONS
           STRING '  >> MOTIVO: ' LD2-ERRO DELIMITED BY SIZE
             INTO REG-RLINCONS
           END-STRING
           WRITE REG-RLINCONS.

       1400-CALL-DEVCDATA.
           CALL WRK-PROG-DAT USING WRK-DEVCDATA.

       9999-FIM.
           CLOSE ARQ-DADOSCLI ARQ-RLINCONS
           GOBACK.
