       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEV08P10.
      *
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DADOSCLI ASSIGN TO DADOSCLI
               FILE STATUS IS WRK-FS-DADOSCLI.
           SELECT RLINCONS ASSIGN TO RLINCONS
               FILE STATUS IS WRK-FS-RLINCONS.
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
       01 WRK-FS-DADOSCLI           PIC X(002) VALUE SPACES.
       01 WRK-FS-RLINCONS           PIC X(002) VALUE SPACES.
      * 
       01 WRK-REG-CONTROLE.
           05 WRK-REG-LIDOS         PIC 9(005) VALUE ZEROS.
           05 WRK-REG-ATUAL         PIC 9(005) VALUE ZEROS.
           05 WRK-REG-DESP          PIC 9(005) VALUE ZEROS.
       01 WRK-FIM-ARQUIVO           PIC X(001) VALUE 'N'.
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
       01 WRK-PROG-CNPJ            PIC X(8) VALUE 'DEV08CPJ'.
       01 WRK-PROG-DAT             PIC X(8) VALUE 'DEVCDATA'.
      *
       01 WRK-LINHAS                PIC 9(03) VALUE 60.
      *
       01 WRK-PILHA-ERROS.
           05 WRK-IDX-PILHA        PIC 9(001) VALUE ZERO.
           05 WRK-TABELA-ERROS OCCURS 05 TIMES
                                   PIC X(17).
      *
       01 WRK-FATAL-ERROR          PIC X(002) VALUE '00'. 
      *
       PROCEDURE DIVISION.
      * 
       0000-PRINCIPAL                             SECTION.
           OPEN INPUT DADOSCLI
      * 
           IF WRK-FS-DADOSCLI NOT EQUAL '00'
               MOVE 'DADOSCLI'        TO SYS-NOME-ARQUIVO
               MOVE 'OPEN'            TO SYS-OPERACAO-ARQ
               MOVE WRK-FS-DADOSCLI   TO SYS-COD-ERRO

               DISPLAY SYSOUT-ERRO-ARQUIVO
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
      *
           OPEN OUTPUT RLINCONS
      *
           IF WRK-FS-RLINCONS NOT EQUAL '00'
               MOVE 'RLINCONS'        TO SYS-NOME-ARQUIVO
               MOVE 'OPEN'            TO SYS-OPERACAO-ARQ
               MOVE WRK-FS-RLINCONS   TO SYS-COD-ERRO

               DISPLAY SYSOUT-ERRO-ARQUIVO
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF.
      *      
           READ DADOSCLI INTO PRF-DADOSCLI
              AT END MOVE 'S' TO WRK-FIM-ARQUIVO
           END-READ
      *
           
           ACCEPT WRK-DATA-PROCESSAMENTO FROM DATE YYYYMMDD.
           ACCEPT WRK-HORA-PROCESSAMENTO FROM TIME.
           INITIALIZE SYSOUT-ERRO-ARQUIVO
           INITIALIZE SYSOUT-SUCESSO
           INITIALIZE SYSOUT-ERRO-SQL
      * 
           IF WRK-FIM-ARQUIVO EQUAL 'S'
               MOVE 4 TO RETURN-CODE
               SET ARQ-VAZIO TO TRUE
           END-IF.
      * 
           PERFORM UNTIL WRK-FIM-ARQUIVO EQUAL 'S' 
              OR SYS-COD-SQL NOT EQUAL 0
      *        
              MOVE 'OPERACAO INVALIDA' TO LD1-RESULTADO
      *
              ADD 1 TO WRK-REG-LIDOS
              PERFORM 1000-PROCESSAR-NOVO-CLIENTE
      *       
              IF LD1-OPER EQUAL SPACES
                   ADD 1 TO WRK-REG-DESP
              END-IF

              READ DADOSCLI INTO PRF-DADOSCLI
                AT END MOVE 'S' TO WRK-FIM-ARQUIVO
              END-READ
           END-PERFORM.
      *
           IF WRK-FIM-ARQUIVO EQUAL 'S' 
              OR SYS-COD-SQL EQUAL 0
               DISPLAY SYSOUT-SUCESSO
           END-IF

           CLOSE DADOSCLI
           CLOSE RLINCONS
           GOBACK.
       0000-FIM. EXIT.
      *     
       1000-PROCESSAR-NOVO-CLIENTE                         SECTION.
           IF NOVO-CLIENTE
              GO TO 1000-FIM
           END-IF
      * 
           MOVE PRF-OPERACAO TO LD1-OPER
           MOVE 'NOVO-CLIENTE' TO LD1-DESCOPER
      * 
           IF PRF-CODIGOCLI EQUAL 0
              MOVE 'ERRO NUM. CLIENTE' 
                 TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
           END-IF
      * 
           IF PRF-RAZAOSOCIAL EQUAL SPACES
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
           IF WRK-CODRCNPJ NOT EQUAL 'OK'
              MOVE 'ERRO CNPJ'
                TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
           END-IF
      *
           IF PRF-VLRULTCOMPRA EQUAL 0
              MOVE 'ERRO VALOR'
                 TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
           END-IF
      *
           MOVE PRF-DATAOPER TO WRK-DATADEV
           CALL WRK-PROG-DAT USING WRK-DEVCDATA.
      *
           IF WRK-CODRDEV NOT EQUAL 'OK' 
              OR PRF-DATAOPER > WRK-DATA-PROCESSAMENTO
              MOVE 'ERRO DATA'
                TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
           END-IF
      *
           IF WRK-IDX-PILHA > 0
               GO TO 1000-FIM 
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
                 FROM ALUNO08.CLIENTPJ
                 WHERE CODIGO_CLI = :CODIGO-CLI
           END-EXEC
      *
           MOVE 'OPERACAO REALIZADA' TO LD1-RESULTADO
      *
           IF SQLCODE EQUAL +0
              MOVE 'ERRO NUM. CLIENTE' 
                 TO WRK-TABELA-ERROS (WRK-IDX-PILHA + 1)
              ADD 1 TO WRK-IDX-PILHA
              ADD 1 TO WRK-REG-DESP
              GO TO 1000-FIM
           END-IF
      *
           IF PRF-CNPJ IS NUMERIC AND PRF-FILIAL IS NUMERIC 
              MOVE PRF-CNPJ                 TO NUMECNPJ-CLI
              MOVE PRF-FILIAL               TO FILIALCNPJ-CLI
              EXEC SQL
                  INSERT INTO ALUNO08.CLIENTPJ (
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
                  INSERT INTO ALUNO08.CLIENTPJ (
                      CODIGO_CLI, RAZSOCIAL_CLI, 
                      NUMECNPJA_CLI, FILIALCNPJA_CLI, 
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
           IF SQLCODE NOT EQUAL +100
               MOVE 'SELECT' TO SYS-OPE-SQL
               GO TO 1000-FIM            
           END-IF
      *
           PERFORM 4000-IMPRIMIR-RELATORIO.
       1000-FIM. EXIT.

       4000-IMPRIMIR-RELATORIO.
           MOVE PRF-CODIGOCLI TO LD3-NUMCLI
           MOVE PRF-RAZAOSOCIAL TO LD3-RAZSOCIAL
      *
           IF WRK-LINHAS = 60 OR WRK-LINHAS = 0
               ADD 1 TO CB1-PAG
               MOVE WRK-DIA-PROC TO CB1-DATA(1:2)
               MOVE '/' TO CB1-DATA(3:1)
               MOVE WRK-MES-PROC TO CB1-DATA(4:2)
               MOVE '/' TO CB1-DATA(6:1)
               MOVE WRK-ANO-PROC TO CB1-DATA(7:4)
      *
               WRITE REG-RLINCONS FROM CABEC1
           END-IF
      *    
           WRITE REG-RLINCONS FROM LINDET1

           PERFORM UNTIL WRK-IDX-PILHA EQUAL ZERO
               MOVE WRK-TABELA-ERROS (WRK-IDX-PILHA)
                 TO LD2-RESULTADO
      *
               WRITE REG-RLINCONS FROM LINDET2
               SUBTRACT 1 FROM WRK-IDX-PILHA
           END-PERFORM
      *
           WRITE REG-RLINCONS FROM CABEC2
      *
           WRITE REG-RLINCONS FROM LINDET3
      *
           WRITE REG-RLINCONS FROM CABEC3          
      *
           MOVE PRF-CNPJ              TO LD4-CNPJ
           MOVE PRF-FILIAL            TO LD4-FILIAL
           MOVE PRF-CONTROLE          TO LD4-CONTROLE 
      *
           MOVE PRF-VLRULTCOMPRA      TO LD4-VRULTCOMPRA
           MOVE PRF-DATAOPER          TO LD4-DATA.

       4000-FIM. EXIT.

            