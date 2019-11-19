DEFINE VARIABLE w_ano AS INTEGER NO-UNDO.
DEFINE VARIABLE w_mes AS INTEGER NO-UNDO.
DEFINE VAR dia1 AS INTEGER.
DEFINE VAR dia2 AS INTEGER.
DEFINE VARIABLE w_FEC1 AS DATE.
DEFINE VARIABLE w_FEC2 AS DATE.
DEFINE VARIABLE i AS INTEGER.
DEFINE VAR vCuenta AS CHARACTER.

w_FEC1 = 06/01/2017.
w_FEC2 = 06/30/2017.
w_ano = YEAR(w_fec1).
w_mes = MONTH(W_Fec1).
dia1 = DAY(w_Fec1).
dia2 = DAY(w_fec2).
vCuenta = "14420501".

MESSAGE "Inicia Mayorización"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FIND FIRST cuentas_NIIF WHERE cuentas_NIIF.cuenta = vCuenta NO-LOCK NO-ERROR.

FOR EACH agencias WHERE agencias.agencia = 4 NO-LOCK:
    FOR EACH sal_cuenta_NIIF WHERE sal_cuenta_NIIF.agencia = agencias.agencia
                          AND sal_cuenta_NIIF.cuenta = vCuenta
                          AND SAL_CUENTA_NIIF.ano EQ w_ano:
        ASSIGN sal_cuenta_NIIF.db[w_mes] = 0
               sal_cuenta_NIIF.cr[w_mes] = 0.
    END.

    FOR EACH anexos_NIIF WHERE anexos_NIIF.agencia = agencias.agencia
                      AND anexos_NIIF.cuenta = vCuenta
                      AND ANEXOS_NIIF.ano = w_ano:
        ASSIGN anexos_NIIF.db[w_mes] = 0
               anexos_NIIF.cr[w_mes] = 0.
    END.

    DO i = dia1 TO dia2:
        w_FEC1 = DATE(w_mes,i,w_ano).

        FOR EACH Mov_Contable_NIIF WHERE Mov_Contable_NIIF.agencia = agencias.agencia
                                AND Mov_Contable_NIIF.cuenta = vCuenta
                                AND Mov_Contable_NIIF.Fec_Contable = W_Fec1 NO-LOCK:
            FIND FIRST Sal_Cuenta_NIIF WHERE Sal_Cuenta_NIIF.Agencia EQ Mov_Contable_NIIF.Agencia
                                    AND Sal_cuenta_NIIF.Cen_Costos EQ Mov_Contable_NIIF.Cen_Costos
                                    AND Sal_Cuenta_NIIF.Cuenta EQ Mov_Contable_NIIF.Cuenta
                                    AND Sal_Cuenta_NIIF.Ano EQ w_ano USE-INDEX idppal EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAILABLE(Sal_Cuenta_NIIF) THEN DO:
                CREATE Sal_Cuenta_NIIF.
                ASSIGN Sal_Cuenta_NIIF.Agencia = Mov_Contable_NIIF.Agencia
                       Sal_Cuenta_NIIF.Cuenta = Mov_Contable_NIIF.Cuenta
                       Sal_Cuenta_NIIF.Ano = w_ano
                       Sal_Cuenta_NIIF.Cen_Costos = Mov_Contable_NIIF.Cen_Costos.
            END.
            
            Sal_Cuenta_NIIF.Db[w_mes] = Sal_Cuenta_NIIF.Db[w_mes] + Mov_Contable_NIIF.Db.
            Sal_Cuenta_NIIF.Cr[w_mes] = Sal_Cuenta_NIIF.Cr[w_mes] + Mov_Contable_NIIF.Cr.

            IF (Mov_Contable_NIIF.Nit) <> "" AND cuentas_NIIF.id_nit = TRUE THEN DO:
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

                Anexos_NIIF.Db[w_mes] = Anexos_NIIF.Db[w_mes] + Mov_Contable_NIIF.Db.
                Anexos_NIIF.Cr[w_mes] = Anexos_NIIF.Cr[w_mes] + Mov_Contable_NIIF.Cr.
            END.
        END.  /* FOR EACH mov_contable*/
    END. /*do*/
END. /* agencias */

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
