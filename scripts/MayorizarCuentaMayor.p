DEFINE VARIABLE w_ano AS INTEGER NO-UNDO.
DEFINE VARIABLE w_mes AS INTEGER NO-UNDO.
DEFINE VAR dia1 AS INTEGER.
DEFINE VAR dia2 AS INTEGER.
DEFINE VARIABLE w_FEC1 AS DATE.
DEFINE VARIABLE w_FEC2 AS DATE.
DEFINE VARIABLE i AS INTEGER.
DEFINE VAR vCuenta AS CHARACTER.

w_FEC1 = 11/01/2018.
w_FEC2 = 11/30/2018.
w_ano = YEAR(w_fec1).
w_mes = MONTH(W_Fec1).
dia1 = DAY(w_Fec1).
dia2 = DAY(w_fec2).
vCuenta = "4".

MESSAGE "Inicia Mayorización"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH agencias WHERE agencias.agencia = 1 NO-LOCK:
    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND substring(sal_cuenta.cuenta,1,1) = vCuenta
                          AND SAL_CUENTA.ano EQ w_ano:
        ASSIGN sal_cuenta.db[w_mes] = 0
               sal_cuenta.cr[w_mes] = 0.
    END.

    FOR EACH anexos WHERE anexos.agencia = agencias.agencia
                      AND substring(anexos.cuenta,1,1) = vCuenta
                      AND ANEXOS.ano = w_ano:
        ASSIGN anexos.db[w_mes] = 0
               anexos.cr[w_mes] = 0.
    END.

    DO i = dia1 TO dia2:
        w_FEC1 = DATE(w_mes,i,w_ano).

        FOR EACH Mov_Contable WHERE Mov_contable.agencia = agencias.agencia
                                AND substring(mov_contable.cuenta,1,1) = vCuenta
                                AND Mov_Contable.Fec_Contable = W_Fec1 NO-LOCK:
            FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Agencia EQ Mov_contable.Agencia
                                    AND Sal_cuenta.Cen_Costos EQ Mov_contable.Cen_Costos
                                    AND Sal_Cuenta.Cuenta EQ Mov_Contable.Cuenta
                                    AND Sal_Cuenta.Ano EQ w_ano USE-INDEX idppal EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAILABLE(Sal_Cuenta) THEN DO:
                CREATE Sal_Cuenta.
                ASSIGN Sal_Cuenta.Agencia = Mov_contable.Agencia
                       Sal_Cuenta.Cuenta = Mov_contable.Cuenta
                       Sal_Cuenta.Ano = w_ano
                       Sal_Cuenta.Cen_Costos = Mov_contable.Cen_Costos.
            END.
            
            Sal_Cuenta.Db[w_mes] = Sal_Cuenta.Db[w_mes] + Mov_Contable.Db.
            Sal_Cuenta.Cr[w_mes] = Sal_Cuenta.Cr[w_mes] + Mov_contable.Cr.

            FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.

            IF (Mov_Contable.Nit) <> "" AND cuentas.id_nit = TRUE THEN DO:
                FIND FIRST Anexos WHERE Anexos.Agencia EQ Mov_Contable.Agencia
                                    AND Anexos.Cen_Costos EQ Mov_Contable.Cen_Costos
                                    AND Anexos.Cuenta EQ Mov_Contable.Cuenta
                                    AND Anexos.Nit EQ Mov_Contable.Nit
                                    AND Anexos.Ano EQ w_ano USE-INDEX idprimanexos EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                IF NOT AVAILABLE(Anexos) THEN DO:
                    CREATE Anexos.
            
                    ASSIGN Anexos.Agencia = Mov_Contable.Agencia
                           Anexos.Nit = Mov_Contable.Nit
                           Anexos.Cuenta = Mov_Contable.Cuenta
                           Anexos.Ano = w_ano
                           Anexos.Cen_Costos = Mov_Contable.Cen_Costos.
                END.

                Anexos.Base[w_mes] = Anexos.Base[w_mes] + Mov_Contable.Base.
                Anexos.Db[w_mes] = Anexos.Db[w_mes] + Mov_Contable.Db.
                Anexos.Cr[w_mes] = Anexos.Cr[w_mes] + Mov_Contable.Cr.
            END.
        END.  /* FOR EACH mov_contable*/
    END. /*do*/
END. /* agencias */

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
