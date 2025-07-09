      *---------------------------------------------------------*
      * REGISTRO COM DADOS PARA ATUALIZACAO DA TABELA CLIENTPJ  *
      * - ARQUIVO SEQUENCIAL - LRECL 80 POSICOES                *
      *---------------------------------------------------------*
      *
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
