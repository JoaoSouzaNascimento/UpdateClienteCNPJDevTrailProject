       ID DIVISION.
       PROGRAM-ID. DEV08P10.
       AUTHOR. AL08&13.
      *
      *----------------------------------------------------------------*
      * SUBPROGRAMA - RETORNA 'OK' PATIR DE UM CNPJ VALIDO             *
      *----------------------------------------------------------------*
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       01  I                          PIC  9(001) VALUE ZEROS.
       01  J                          PIC  9(002) VALUE ZEROS.
       01  K                          PIC  9(002) VALUE ZEROS.
      *
       01 WRK-AREACNPJ.
          05 WRK-DADOS-CNPJ.
             10 WRK-CNPJ              PIC X(008) VALUE SPACES.
             10 WRK-FILIAL            PIC X(004) VALUE SPACES.
             10 WRK-CONTROLE          PIC 9(002) VALUE ZEROS.
          05 WRK-DADOS-CNPJ-TAB       REDEFINES WRK-DADOS-CNPJ.
             10 WRK-CNPJ-CHAR         OCCURS 14 TIMES
                                      PIC X(001).
          05 WRK-CODRCNPJ             PIC X(002) VALUE SPACES.
      *
       01 WRK-CAL-CONTROLE.
          05 WRK-SUM                  PIC 9(004) VALUE ZEROS.
          05 WRK-TEMP-NUM             PIC 9(003) VALUE ZEROS.
          05 WRK-MOD                  PIC 9(002) VALUE ZEROS.
          05 WRK-CON                  PIC 9(001) VALUE ZEROS.
          05 WRK-PESO                 PIC 9(001) VALUE ZEROS.
          05 WRK-LMT-PJ               PIC 9(002) VALUE ZEROS.
      *
       01 WRK-CONSTANTES.
          05 WRK-TAB-ALPHA-MAP        PIC 9(002) VALUE 16.
      *
       01 WRK-LIST-ALPHA.
           05 FILLER PIC X(026) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
       01 WRK-TAB-APLHA               REDEFINES WRK-LIST-ALPHA.
           05 WRK-TAB-CHAR            OCCURS 26 TIMES
                                      PIC X(001).
       01 WRK-TAB-ALP-IND             PIC 9(002) VALUE  ZEROS.
      *
       LINKAGE SECTION.
      *
       01 LNK-AREACNPJ.
          05 LNK-DADOS-CNPJ.
             10 LNK-CNPJ              PIC X(008).
             10 LNK-FILIAL            PIC X(004).
             10 LNK-CONTROLE          PIC 9(002).
          05 LNK-CODRCNPJ             PIC X(002).
      *
       PROCEDURE DIVISION USING LNK-AREACNPJ.
       0000-INICIO                    SECTION.
           MOVE 'OK' TO LNK-CODRCNPJ.
           PERFORM 1000-VALI-ENTRADA.
           IF LNK-CODRCNPJ IS EQUAL TO 'OK'
               MOVE LNK-DADOS-CNPJ TO WRK-DADOS-CNPJ
               PERFORM 2000-VALI-DIG-VERIFICADOR
           END-IF.
           GOBACK.
       0000-FIM. EXIT.
      *
       1000-VALI-ENTRADA              SECTION.
           EVALUATE TRUE
               WHEN LNK-CNPJ IS NUMERIC AND LNK-CNPJ = ZEROS
                    PERFORM 9999-SET-ERRO
               WHEN LNK-CNPJ IS NOT NUMERIC AND LNK-CNPJ = SPACE
                    PERFORM 9999-SET-ERRO
               WHEN LNK-FILIAL IS NUMERIC AND LNK-FILIAL = ZEROS
                    PERFORM 9999-SET-ERRO
               WHEN LNK-FILIAL IS NOT NUMERIC AND LNK-FILIAL = SPACES
                    PERFORM 9999-SET-ERRO
               WHEN LNK-CONTROLE IS NOT NUMERIC
                    PERFORM 9999-SET-ERRO
               WHEN OTHER
                    CONTINUE
           END-EVALUATE.
       1000-FIM. EXIT.
      *
       2000-VALI-DIG-VERIFICADOR      SECTION.
           MOVE 12 TO WRK-LMT-PJ.
      *
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > 1
               DISPLAY 'ITDV:' I
               INITIALIZE WRK-SUM
               MOVE 2 TO WRK-PESO
      *
               PERFORM VARYING J FROM WRK-LMT-PJ BY -1 UNTIL J < 1
                   PERFORM 3000-EXTRAIR-SOMA
      *
                   IF WRK-PESO IS EQUAL TO 9
                       MOVE 2 TO WRK-PESO
                   ELSE
                       ADD 1 TO WRK-PESO
                   END-IF
      *
               END-PERFORM
      *
               DISPLAY 'SUM: ' WRK-SUM
               DIVIDE WRK-SUM BY 11 GIVING K REMAINDER WRK-MOD
               DISPLAY 'MOD: ' WRK-MOD
      *
               IF WRK-MOD < 2
                   MOVE 0 TO WRK-CON
               ELSE
                   COMPUTE WRK-CON = 11 - WRK-MOD
               END-IF
      *
               COMPUTE J = WRK-LMT-PJ + 1
               MOVE WRK-CNPJ-CHAR (J) TO WRK-TEMP-NUM
      *
               DISPLAY ' DV: ' WRK-CON ' DVF: ' WRK-CNPJ-CHAR (J)
               IF WRK-CON IS NOT EQUAL TO WRK-CNPJ-CHAR (J)
                   PERFORM 9999-SET-ERRO
                   MOVE 3 TO I
               END-IF
      *
               ADD 1 TO WRK-LMT-PJ
           END-PERFORM.
       2000-FIM. EXIT.
      *
       3000-EXTRAIR-SOMA              SECTION.
           IF WRK-CNPJ-CHAR (J) IS ALPHABETIC
               PERFORM 4000-MAPEAR-CNPJ
               COMPUTE WRK-SUM = WRK-SUM + (WRK-TAB-ALP-IND * WRK-PESO)
               DISPLAY ' VALOR: ' WRK-TAB-ALP-IND ' PESO: ' WRK-PESO
           ELSE
               MOVE WRK-CNPJ-CHAR (J) TO WRK-TEMP-NUM
               COMPUTE WRK-SUM = WRK-SUM + (WRK-TEMP-NUM * WRK-PESO)
               DISPLAY ' VALOR: ' WRK-TEMP-NUM ' PESO: ' WRK-PESO
           END-IF.
       3000-FIM. EXIT.
      *
       4000-MAPEAR-CNPJ               SECTION.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 26
               IF WRK-CNPJ-CHAR (J) IS EQUAL TO  WRK-TAB-CHAR(K)
                  COMPUTE WRK-TAB-ALP-IND = K + WRK-TAB-ALPHA-MAP
                  MOVE 27 TO K
               END-IF
           END-PERFORM.
       4000-FIM. EXIT.
      *
       9999-SET-ERRO                  SECTION.
           MOVE '**' TO LNK-CODRCNPJ.
       9999-FIM. EXIT.
