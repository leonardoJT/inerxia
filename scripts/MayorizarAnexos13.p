DISABLE TRIGGERS FOR LOAD OF anexos13.

DEFINE VARIABLE w_ano AS INTEGER.
DEFINE VARIABLE w_mes AS INTEGER NO-UNDO.
DEFINE VAR dia1 AS INTEGER.
DEFINE VAR dia2 AS INTEGER.
DEFINE VARIABLE w_FEC1 AS DATE.
DEFINE VARIABLE w_FEC2 AS DATE.
DEFINE VARIABLE i AS INTEGER.

w_FEC1 = 10/01/2012.
w_FEC2 = 10/31/2012.
w_mes = MONTH(W_Fec1).
dia1 = DAY(w_Fec1).
dia2 = DAY(w_fec2).
w_ano = YEAR(w_fec1).

MESSAGE "Inicia Mayorización de Anexos 13"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH anexos13 WHERE anexos13.ano = w_ano:
    ASSIGN anexos13.db[w_mes] = 0
           anexos13.cr[w_mes] = 0.
END.

FOR EACH agencias:
    DO i = dia1 TO dia2:
        w_FEC1 = DATE(w_mes,i,w_ano).

        FOR EACH Mov_Contable WHERE Mov_contable.agencia EQ agencias.agencia
                                AND Mov_Contable.Fec_Contable EQ W_Fec1
                                AND mov_contable.nit <> "" NO-LOCK:
            FIND FIRST anexos13 WHERE anexos13.Agencia EQ Mov_Contable.Agencia
                                  AND anexos13.Cen_Costos EQ Mov_Contable.Cen_Costos
                                  AND anexos13.Cuenta EQ Mov_Contable.Cuenta
                                  AND anexos13.Nit EQ Mov_Contable.Nit
                                  AND anexos13.Ano EQ w_ano USE-INDEX idprimanexos NO-ERROR.
            IF NOT AVAILABLE(anexos13) THEN DO:
                CREATE anexos13.
                ASSIGN anexos13.Agencia = Mov_Contable.Agencia
                       anexos13.Nit = Mov_Contable.Nit
                       anexos13.Cuenta = Mov_Contable.Cuenta
                       anexos13.Ano = w_ano
                       anexos13.Cen_Costos = Mov_Contable.Cen_Costos.
            END.

            anexos13.Db[w_mes] = anexos13.Db[w_mes] + Mov_Contable.Db.
            anexos13.Cr[w_mes] = anexos13.Cr[w_mes] + Mov_Contable.Cr.
        END.
    END.
END.

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
