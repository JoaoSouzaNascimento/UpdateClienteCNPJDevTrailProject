      ******************************************************************
      * DCLGEN TABLE(ALUNOXX.CLIENTPJ)                                 *
      *        LIBRARY(ASE.ALUNOXX.COBLIB(CLIENTPJ))                   *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE ALUNOXX.CLIENTPJ TABLE
           ( CODIGO_CLI                     DECIMAL(5, 0) NOT NULL,
             RAZSOCIAL_CLI                  CHAR(40) NOT NULL,
             NUMECNPJ_CLI                   DECIMAL(8, 0),
             FILIALCNPJ_CLI                 DECIMAL(4, 0),
             CTLCNPJ_CLI                    CHAR(2),
             NUMECNPJA_CLI                  CHAR(8),
             FILIALCNPJA_CLI                CHAR(4),
             SALDO_CLI                      DECIMAL(13, 2),
             VRULTCOMPRA_CLI                DECIMAL(11, 2),
             DTULTCOMPRA_CLI                DATE,
             DTATLZDADOS_CLI                DATE,
             DTINATIVA_CLI                  DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE ALUNOXX.CLIENTPJ                   *
      ******************************************************************
       01  DCLCLIENTPJ.
           10 CODIGO-CLI           PIC S9(5)V USAGE COMP-3.
           10 RAZSOCIAL-CLI        PIC X(40).
           10 NUMECNPJ-CLI         PIC S9(8)V USAGE COMP-3.
           10 FILIALCNPJ-CLI       PIC S9(4)V USAGE COMP-3.
           10 CTLCNPJ-CLI          PIC X(2).
           10 NUMECNPJA-CLI        PIC X(8).
           10 FILIALCNPJA-CLI      PIC X(4).
           10 SALDO-CLI            PIC S9(11)V9(2) USAGE COMP-3.
           10 VRULTCOMPRA-CLI      PIC S9(9)V9(2) USAGE COMP-3.
           10 DTULTCOMPRA-CLI      PIC X(10).
           10 DTATLZDADOS-CLI      PIC X(10).
           10 DTINATIVA-CLI        PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
