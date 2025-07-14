       01 SYSOUT-ERRO-SQL.
           05 SYS-SQLMSG-LINHA-1.
              10 FILLER           PIC X(11) VALUE 'DEVxxP10 - '.
              10 FILLER           PIC X(11) VALUE 'TABELA...: '.
              10 FILLER           PIC X(11) VALUE 'CLIENTPJ   '.
           05 SYS-SQLMSG-LINHA-2.
              10 FILLER           PIC X(11) VALUE 'DEVxxP10 - '.
              10 FILLER           PIC X(11) VALUE 'OPERACAO.: '.
              10 SYS-OPE-SQL      PIC X(08).
           05 SYS-SQLMSG-LINHA-3.
              10 FILLER           PIC X(11) VALUE 'DEVxxP10 - '.
              10 FILLER           PIC X(11) VALUE 'SQLCODE..: '.
              10 SYS-COD-SQL       PIC -9(009).
              10 FILLER           PIC X(54) VALUE SPACES.
      *
       01 SYSOUT-SUCESSO.
           05 SYS-SYSOUT-SUCESSO-LINHA-1.
              10 FILLER           PIC X(034) 
                 VALUE 'DEVxxP10 - INICIO DO PROCESSAMENTO'.
           05 SYS-SYSOUT-SUCESSO-LINHA-2.
              10 FILLER           PIC X(018) 
                 VALUE 'DEVxxP10 - DATA.: '.
              10 SYS-SUC-DIA           PIC X(002).
              10 FILLER           PIC X(001) VALUE '/'.
              10 SYS-SUC-MES           PIC X(002).
              10 FILLER           PIC X(001) VALUE '/'.
              10 SYS-SUC-ANO           PIC X(004).
           05 SYS-SYSOUT-SUCESSO-LINHA-3.
              10 SYS-MSG-ARQ-VAZIO    PIC X(33). 
                 88 ARQ-VAZIO 
                    VALUE 'DEVXXP10 - ARQUIVO DADOSCLI VAZIO'.
                 88 ARQ-NAO-VAZIO 
                    VALUE SPACES.
           05 SYS-SYSOUT-SUCESSO-LINHA-4.
              10 FILLER           PIC X(041) 
                 VALUE 'DEVXXP10-TOTAL DE REGISTROS LIDOS......: '.
              10 SYS-REG-LIDOS    PIC Z(004)9.
           05 SYS-SYSOUT-SUCESSO-LINHA-5.
              10 FILLER           PIC X(041) 
                 VALUE 'DEVXXP10-TOTAL DE REGISTROS ATUALIZADOS: '.
              10 SYS-REG-ATUAL    PIC Z(004)9.
           05 SYS-SYSOUT-SUCESSO-LINHA-6.
              10 FILLER           PIC X(041) 
                 VALUE 'DEVXXP10-TOTAL DE REGISTROS DESPREZADOS: '.
              10 SYS-REG-DESP     PIC Z(004)9.
           05 SYS-SYSOUT-ENCERRADO.
              10 FILLER           PIC X(034) 
                 VALUE 'DEVXXP10 - PROCESSAMENTO ENCERRADO'.
      *
       01 SYSOUT-ERRO-ARQUIVO.
           05 SYS-ERROARQ-LINHA-1.
              10 FILLER           PIC X(034) 
                 VALUE 'DEVXXP10 - ERRO NO PROCESSAMENTO'.
           05 SYS-ERROARQ-LINHA-2.
              10 FILLER           PIC X(022) 
                 VALUE 'DEVXXP10 - ARQUIVO..: '.
              10 SYS-NOME-ARQUIVO PIC X(009).
           05 SYS-ERROARQ-LINHA-3.
              10 FILLER           PIC X(022) 
                 VALUE 'DEVXXP10 - OPERACAO.: '.
              10 SYS-OPERACAO-ARQ PIC X(008).
           05 SYS-ERROARQ-LINHA-4.
              10 FILLER           PIC X(022) 
                 VALUE 'DEVXXP10 - SQLCODE..: '.
              10 SYS-COD-ERRO     PIC X(002).
           05 SYS-ERROARQ-LINHA-5.
              10 FILLER           PIC X(034) 
                 VALUE 'DEVXXP10 - PROCESSAMENTO ENCERRADO'.