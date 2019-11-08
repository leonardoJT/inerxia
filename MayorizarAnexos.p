DISABLE TRIGGERS FOR LOAD OF anexos.

DEFINE VARIABLE w_ano AS INTEGER NO-UNDO.
DEFINE VARIABLE w_mes AS INTEGER NO-UNDO.
DEFINE VAR dia1 AS INTEGER.
DEFINE VAR dia2 AS INTEGER.
DEFINE VARIABLE w_FEC1 AS DATE.
DEFINE VARIABLE w_FEC2 AS DATE.
DEFINE VARIABLE fechaAux AS DATE.
DEFINE VAR vCuenta AS CHARACTER INITIAL "16909502".
DEFINE VAR vAgencia AS INTEGER INITIAL 1.
DEFINE VAR wFecha AS DATE.
DEFINE VAR cont AS INTEGER.

w_FEC1 = 02/01/2014.
w_FEC2 = 02/28/2014.
w_mes = MONTH(W_Fec1).
w_ano = YEAR(W_Fec1).
dia1 = DAY(w_Fec1).
dia2 = DAY(w_fec2).

MESSAGE "Inicia Mayorización de Anexos"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


FOR EACH anexos WHERE anexos.cuenta = vCuenta
                  AND anexos.agencia = vAgencia
                  AND anexos.ano = 2014:
    DO cont = 2 TO 2:
        ASSIGN anexos.db[cont] = 0
               anexos.cr[cont] = 0.
    END.
END.

FOR EACH agencias WHERE agencias.agencia = vAgencia:
    wFecha = w_fec1.
    DO fechaAux = w_fec1 TO w_fec2:
        FOR EACH Mov_Contable WHERE Mov_contable.agencia EQ agencias.agencia
                                AND Mov_Contable.Fec_Contable EQ wFecha
                                AND mov_contable.nit <> ""
                                AND mov_contable.cuenta = vCuenta NO-LOCK:
            FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
            IF cuentas.id_nit = TRUE THEN DO:
                FIND FIRST anexos WHERE anexos.Agencia EQ Mov_Contable.Agencia
                                    AND anexos.Cen_Costos EQ Mov_Contable.Cen_Costos
                                    AND anexos.Cuenta EQ Mov_Contable.Cuenta
                                    AND anexos.Nit EQ Mov_Contable.Nit
                                    AND anexos.Ano EQ YEAR(mov_contable.fec_contable) USE-INDEX idprimanexos NO-ERROR.
                IF NOT AVAILABLE(anexos) THEN DO:
                    CREATE anexos.
                    ASSIGN anexos.Agencia = Mov_Contable.Agencia
                           anexos.Nit = Mov_Contable.Nit
                           anexos.Cuenta = Mov_Contable.Cuenta
                           anexos.Ano = YEAR(mov_contable.fec_contable)
                           anexos.Cen_Costos = Mov_Contable.Cen_Costos.
                END.
            
                anexos.Db[MONTH(mov_contable.fec_contable)] = anexos.Db[MONTH(mov_contable.fec_contable)] + Mov_Contable.Db.
                anexos.Cr[MONTH(mov_contable.fec_contable)] = anexos.Cr[MONTH(mov_contable.fec_contable)] + Mov_Contable.Cr.
            END.
        END.

        wFecha = wFecha + 1.
    END.
END.

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
