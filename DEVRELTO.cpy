      *
      *----------------------------------------------------------------*
      * DEFINICOES DO RELATORIO - RELATO                               *
      *----------------------------------------------------------------*
      *
       01  CABEC1.
           10 FILLER                   PIC  X(005) VALUE SPACES.
           10 CB1-DATA                 PIC  X(010) VALUE SPACES.
           10 FILLER                   PIC  X(003) VALUE SPACES.
           10 FILLER                   PIC  X(046) VALUE
           'EMPRESA S/A ATUALIZACOES NA TABELA CLIENTES PJ'.
           10 CB1-PAG                  PIC  9(002) VALUE ZEROS.
           10 FILLER                   PIC  X(005) VALUE SPACES.
      *
       01  CABEC2.
           10 FILLER                   PIC  X(005) VALUE SPACES.
           10 FILLER                   PIC  X(022) VALUE
           'CLIENTE   RAZAO SOCIAL'.
           10 FILLER                   PIC  X(045) VALUE SPACES.
      *
       01  LINDET1.
           10 FILLER                   PIC  X(005) VALUE SPACES.
           10 FILLER                   PIC  X(009) VALUE
           'OPERACAO:'.
           10 LD1-OPER                 PIC  X(001) VALUE SPACES.
           10 FILLER                   PIC  X(003) VALUE ' - '.
           10 LD1-DESCOPER             PIC  X(012) VALUE SPACES.
           10 FILLER                   PIC  X(009) VALUE SPACES.
           10 FILLER                   PIC  X(017) VALUE
           'RES PROCESSAMENTO'.
           10 LD1-RESULTADO            PIC  X(018) VALUE SPACES.
           10 FILLER                   PIC  X(005) VALUE SPACES.
      *
       01  LINDET2.
           10 FILLER                   PIC  X(057) VALUE SPACES.
           10 LD2-RESULTADO            PIC  X(018) VALUE SPACES.
           10 FILLER                   PIC  X(005) VALUE SPACES.
      *
       01  LINDET3.
           10 FILLER                   PIC  X(006) VALUE SPACES.
           10 LD3-NUMCLI               PIC  9(005) VALUE ZEROS.
           10 FILLER                   PIC  X(004) VALUE SPACES.
           10 LD3-RAZSOCIAL            PIC  X(040) VALUE SPACES.
           10 FILLER                   PIC  X(025) VALUE SPACES.
      *
