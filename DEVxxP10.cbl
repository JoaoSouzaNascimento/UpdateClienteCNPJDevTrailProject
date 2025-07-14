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
       COPY SYOUTP10.
      *
       01 WS-FS-DADOSCLI           PIC X(002) VALUE SPACES.
       01 WS-FS-RLINCONS           PIC X(002) VALUE SPACES.
      * 
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
       01 WRK-DATA-FORMATADA REDEFINES WRK-DATA-PROCESSAMENTO.
           05 WRK-ANO-PROC         PIC 9(4).
           05 WRK-MES-PROC         PIC 9(2).
           05 WRK-DIA-PROC         PIC 9(2).
      *     
       01 WRK-HORA-PROCESSAMENTO   PIC 9(6).
       01 WRK-HORA-FORMATADA REDEFINES WRK-HORA-PROCESSAMENTO.
           05 WRK-HORA-PROC        PIC 9(2).
           05 WRK-MIN-PROC         PIC 9(2).
           05 WRK-SEG-PROC         PIC 9(2).
      *
       01 WRK-PROG-CNPJ            PIC X(8) VALUE 'DEVXXCPJ'.
       01 WRK-PROG-DAT             PIC X(8) VALUE 'DEVCDATA'.
      *
       01 WRK-PAGINA                PIC 9(03) VALUE 60.
      *
       01 WRK-PILHA-ERROS.
           05 WRK-IDX-PILHA        PIC 9(001) VALUE ZERO.
           05 WRK-TABELA-ERROS OCCURS 05 TIMES
                                   PIC X(17).
      *
       01 LD2-ERROS.
           88 LD2-ERRO-RZSOCIAL    PIC X(017) VALUE 'ERRO RZ SOCIAL'.
           88 LD2-ERRO-CNPJ        PIC X(017) VALUE 'ERRO CNPJ'.
           88 LD2-ERRO-VALOR       PIC X(017) VALUE 'ERRO VALOR'.
           88 LD2-ERRO-DATA        PIC X(017) VALUE 'ERRO DATA'.
           88 LD2-ERRO-CODCLI      PIC X(017) VALUE 'ERRO NUM. CLIENTE'.
      *
       01 WRK-FATAL-ERROR          PIC X(002) VALUE '00'.
      *
       
      *
       PROCEDURE DIVISION.
      * 
       0000-PRINCIPAL.
           OPEN INPUT DADOSCLI
      * 
           IF WS-FS-DADOSCLI NOT = '00'
               
           END-IF
      *
           OPEN OUTPUT RLINCONS
      *
           IF WS-FS-RLINCONS NOT = '00'
               
           END-IF.
      *      
           READ DADOSCLI INTO PRF-DADOSCLI
              AT END MOVE 'S' TO WS-FIM-ARQUIVO
           END-READ
      * 
           IF WS-FIM-ARQUIVO = 'S'
               MOVE 4 TO RETURN-CODE
           END-IF.
      *
           ACCEPT WRK-DATA-PROCESSAMENTO FROM DATE YYYYMMDD.
           ACCEPT WRK-HORA-PROCESSAMENTO FROM TIME.
      * 
           PERFORM UNTIL WS-FIM-ARQUIVO = 'S'
      *        
              MOVE 'OPERACAO INVALIDA' TO LD1-RESULTADO
      *
              ADD 1 TO WS-REG-LIDOS
              PERFORM 1000-PROCESSAR-NOVO-CLIENTE
      *         
              READ DADOSCLI INTO PRF-DADOSCLI
                AT END MOVE 'S' TO WS-FIM-ARQUIVO
              END-READ
           END-PERFORM.
      *
           GOBACK.
       0000-FIM. EXIT.
      *     
       1000-PROCESSAR-NOVO-CLIENTE                         SECTION.
           IF NOVO-CLIENTE
              EXIT SECTION
           END-IF
      * 
           MOVE PRF-OPERACAO TO LD1-OPER
           MOVE 'NOVO-CLIENTE' TO LD1-DESCOPER
      * 
           IF PRF-CODIGOCLI = 0
              MOVE 'ERRO NUM. CLIENTE' 
                 TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
           END-IF
      * 
           IF PRF-RAZAOSOCIAL = SPACES
              MOVE 'ERRO RAZAO SOCIAL'
                 TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
           END-IF
      *
           MOVE PRF-CNPJ TO WRK-CNPJ
           MOVE PRF-FILIAL TO WRK-FILIAL
           MOVE PRF-CONTROLE TO WRK-CONTROLE
           CALL WRK-PROG-CNPJ USING WRK-AREACNPJ
      *
           IF WRK-CODRCNPJ NOT = 'OK'
              MOVE 'ERRO CNPJ'
                TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
           END-IF
      *
           IF PRF-VLRULTCOMPRA = 0
              MOVE 'ERRO VALOR'
                 TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
           END-IF
      *
           MOVE PRF-DATAOPER TO WRK-DATADEV
           CALL WRK-PROG-DAT USING WRK-DEVCDATA.
      *
           IF WRK-CODRDEV NOT = 'OK' 
              OR PRF-DATAOPER > WRK-DATA-PROCESSAMENTO
              MOVE 'ERRO DATA'
                TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
           END-IF
      *
           IF WRK-IDX-PILHA > 0
               EXIT SECTION 
           END-IF
      *
           MOVE PRF-CODIGOCLI            TO CODIGO-CLI
           MOVE PRF-RAZAOSOCIAL          TO RAZSOCIAL-CLI
           MOVE PRF-CONTROLE             TO CTLCNPJ-CLI
           MOVE PRF-VLRULTCOMPRA         TO VRULTCOMPRA-CLI
           MOVE PRF-DATAOPER             TO DTULTCOMPRA-CLI
           MOVE WRK-DATA-PROCESSAMENTO   TO DTATLZDADOS-CLI
      *
           EXEC SQL
              SELECT CODIGO_CLI INTO :CODIGO-CLI
                 FROM ALUNOXX.CLIENTPJ
                 WHERE CODIGO_CLI = :CODIGO-CLI
           END-EXEC
      *
           MOVE 'OPERACAO REALIZADA' TO LD1-RESULTADO
      *
           IF SQLCODE = +0
              MOVE 'ERRO NUM. CLIENTE' 
                 TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
              EXIT SECTION
           END-IF
      *
           IF PRF-CNPJ IS NUMERIC AND PRF-FILIAL IS NUMERIC 
              MOVE PRF-CNPJ                 TO NUMECNPJ-CLI
              MOVE PRF-FILIAL               TO FILIALCNPJ-CLI
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
           ELSE 
              MOVE PRF-CNPJ                 TO NUMECNPJA-CLI
              MOVE PRF-FILIAL               TO FILIALCNPJA-CLI
              EXEC SQL
                  INSERT INTO ALUNOXX.CLIENTPJ (
                      CODIGO_CLI, RAZSOCIAL_CLI, 
                      NUMECNPJA-CLI, FILIALCNPJA-CLI, 
                      CTLCNPJ_CLI, VRULTCOMPRA_CLI, 
                      DTULTCOMPRA_CLI, DTATLZDADOS_CLI
                  ) VALUES (
                      :CODIGO-CLI, :RAZSOCIAL-CLI, 
                      :NUMECNPJA-CLI, :FILIALCNPJA-CLI, 
                      :CTLCNPJ-CLI, :VRULTCOMPRA-CLI,
                      :DTULTCOMPRA-CLI, :DTATLZDADOS-CLI
                  )
              END-EXEC
           END-IF
      *
           IF SQLCODE NOT = +100
               MOVE 'SELECT' TO SYS-OPE-SQL              
           END-IF.

       1000-FIM. EXIT.
