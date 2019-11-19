DEFINE VARIABLE w_ano AS INTEGER NO-UNDO.
DEFINE VARIABLE w_mes AS INTEGER NO-UNDO.
DEFINE VAR dia1 AS INTEGER.
DEFINE VAR dia2 AS INTEGER.
DEFINE VARIABLE w_FEC1 AS DATE.
DEFINE VARIABLE w_FEC2 AS DATE.
DEFINE VARIABLE i AS INTEGER.

w_FEC1 = 12/01/2015.
w_FEC2 = 12/31/2015.
w_ano = YEAR(w_fec1).
w_mes = MONTH(W_Fec1).
dia1 = DAY(w_Fec1).
dia2 = DAY(w_fec2).

MESSAGE "Inicia Mayorización NIIF"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH agencias WHERE agencias.agencia = 1 NO-LOCK:
    FOR EACH sal_cuenta_NIIF WHERE sal_cuenta_NIIF.agencia = agencias.agencia
                               AND SAL_CUENTA_NIIF.ano EQ w_ano:
        ASSIGN sal_cuenta_NIIF.db[w_mes] = 0
               sal_cuenta_NIIF.cr[w_mes] = 0.
    END.

    FOR EACH anexos_NIIF WHERE anexos_NIIF.agencia = agencias.agencia
                           AND ANEXOS_NIIF.ano = w_ano:
        ASSIGN anexos_NIIF.db[w_mes] = 0
               anexos_NIIF.cr[w_mes] = 0.
    END.

    DO i = dia1 TO dia2:
        w_FEC1 = DATE(w_mes,i,w_ano).

        FOR EACH Mov_Contable_NIIF WHERE Mov_contable_NIIF.agencia EQ agencias.agencia
                                     AND Mov_Contable_NIIF.Fec_Contable EQ W_Fec1 NO-LOCK:
            IF (Mov_Contable_NIIF.Cuenta) <> "" THEN DO:
                REPEAT:
                    FIND FIRST Sal_Cuenta_NIIF WHERE Sal_Cuenta_NIIF.Agencia EQ Mov_contable_NIIF.Agencia
                                                 AND Sal_cuenta_NIIF.Cen_Costos EQ Mov_contable_NIIF.Cen_Costos
                                                 AND Sal_Cuenta_NIIF.Cuenta EQ Mov_Contable_NIIF.Cuenta
                                                 AND Sal_Cuenta_NIIF.Ano EQ w_ano USE-INDEX idppal EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                    IF NOT AVAILABLE(Sal_Cuenta_NIIF) THEN DO:
                        CREATE Sal_Cuenta_NIIF.
                        ASSIGN Sal_Cuenta_NIIF.Agencia = Mov_contable_NIIF.Agencia
                               Sal_Cuenta_NIIF.Cuenta = Mov_contable_NIIF.Cuenta
                               Sal_Cuenta_NIIF.Ano = w_ano
                               Sal_Cuenta_NIIF.Cen_Costos = Mov_contable_NIIF.Cen_Costos.
                    END.

                    IF Mov_contable_NIIF.Db GT 0 THEN
                        Sal_Cuenta_NIIF.Db[w_mes] = Sal_Cuenta_NIIF.Db[w_mes] + Mov_Contable_NIIF.Db.

                    IF Mov_Contable_NIIF.Cr GT 0 THEN
                        Sal_Cuenta_NIIF.Cr[w_mes] = Sal_Cuenta_NIIF.Cr[w_mes] + Mov_contable_NIIF.Cr.

                    RELEASE Sal_Cuenta_NIIF.
                    LEAVE.
                END.
            END.

            FIND FIRST cuentas_NIIF WHERE cuentas_NIIF.cuenta = mov_contable_NIIF.cuenta NO-LOCK NO-ERROR.

            IF (Mov_Contable_NIIF.Nit) <> "" AND cuentas_NIIF.id_nit = TRUE THEN DO:
                REPEAT:
                    FIND FIRST Anexos_NIIF WHERE Anexos_NIIF.Agencia EQ Mov_Contable_NIIF.Agencia
                                             AND Anexos_NIIF.Cen_Costos EQ Mov_Contable_NIIF.Cen_Costos
                                             AND Anexos_NIIF.Cuenta EQ Mov_Contable_NIIF.Cuenta
                                             AND Anexos_NIIF.Nit EQ Mov_Contable_NIIF.Nit
                                             AND Anexos_NIIF.Ano EQ w_ano USE-INDEX idprimanexos EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                    IF NOT AVAILABLE(Anexos_NIIF) THEN DO:
                        CREATE Anexos_NIIF.
                        ASSIGN Anexos_NIIF.Agencia = Mov_Contable_NIIF.Agencia
                               Anexos_NIIF.Nit = Mov_Contable_NIIF.Nit
                               Anexos_NIIF.Cuenta = Mov_Contable_NIIF.Cuenta
                               Anexos_NIIF.Ano = w_ano
                               Anexos_NIIF.Cen_Costos = Mov_Contable_NIIF.Cen_Costos.
                    END.

                    IF Mov_Contable_NIIF.Db GT 0 THEN
                        Anexos_NIIF.Db[w_mes] = Anexos_NIIF.Db[w_mes] + Mov_Contable_NIIF.Db.

                    IF Mov_Contable_NIIF.Cr GT 0 THEN
                        Anexos_NIIF.Cr[w_mes] = Anexos_NIIF.Cr[w_mes] + Mov_Contable_NIIF.Cr.

                    RELEASE Anexos_NIIF.
                    LEAVE.
                END.
            END.
        END.
    END.
END.

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
