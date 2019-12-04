DEFINE VARIABLE vYear AS INTEGER.
DEFINE VARIABLE vMes AS INTEGER.
DEFINE VAR dia1 AS INTEGER.
DEFINE VAR dia2 AS INTEGER.
DEFINE VARIABLE fecIni AS DATE.
DEFINE VARIABLE fecFin AS DATE.
DEFINE VARIABLE i AS INTEGER.
DEFINE VAR vAgencia AS INTEGER.

vAgencia = 4.
fecIni = 12/01/2019.
fecFin = 12/31/2019.
vYear = YEAR(fecIni).
vMes = MONTH(fecIni).
dia1 = DAY(fecIni).
dia2 = DAY(fecFin).

MESSAGE "Inicia Mayorización"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH agencias WHERE agencias.agencia = vAgencia NO-LOCK:
    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND SAL_CUENTA.ano = vYear:
        sal_cuenta.db[vMes] = 0.
        sal_cuenta.cr[vMes] = 0.
    END.

    FOR EACH anexos WHERE anexos.agencia = agencias.agencia
                      AND anexos.ano = vYear:
        anexos.db[vMes] = 0.
        anexos.cr[vMes] = 0.
    END.

    DO i = dia1 TO dia2:
        fecIni = DATE(vMes,i,vYear).

        FOR EACH Mov_Contable WHERE Mov_contable.agencia = agencias.agencia
                                AND Mov_Contable.Fec_Contable = fecIni NO-LOCK:
            IF (Mov_Contable.Cuenta) <> "" THEN DO:
                REPEAT:
                    FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Agencia = Mov_contable.Agencia
                                            AND Sal_cuenta.Cen_Costos = Mov_contable.Cen_Costos
                                            AND Sal_Cuenta.Cuenta = Mov_Contable.Cuenta
                                            AND Sal_Cuenta.Ano = vYear USE-INDEX idppal EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                    IF NOT AVAILABLE(Sal_Cuenta) THEN DO:
                        IF LOCKED(Sal_Cuenta) THEN /* oakley */
                            NEXT.
                        ELSE DO:
                            CREATE Sal_Cuenta.
                            ASSIGN Sal_Cuenta.Agencia = Mov_contable.Agencia
                                   Sal_Cuenta.Cuenta = Mov_contable.Cuenta
                                   Sal_Cuenta.Ano = vYear
                                   Sal_Cuenta.Cen_Costos = Mov_contable.Cen_Costos.
                        END.
                    END.

                    IF Mov_contable.Db GT 0 THEN
                        Sal_Cuenta.Db[vMes] = Sal_Cuenta.Db[vMes] + Mov_Contable.Db.
                
                    IF Mov_Contable.Cr GT 0 THEN
                        Sal_Cuenta.Cr[vMes] = Sal_Cuenta.Cr[vMes] + Mov_contable.Cr.

                    RELEASE Sal_Cuenta.
                    LEAVE.
                END. /* REPEAT */
            END.

            /*ANEXOS*/

            FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.

            IF (Mov_Contable.Nit) <> "" AND cuentas.id_nit = TRUE THEN DO:
                REPEAT:
                    FIND FIRST Anexos WHERE Anexos.Agencia EQ Mov_Contable.Agencia
                                        AND Anexos.Cen_Costos EQ Mov_Contable.Cen_Costos
                                        AND Anexos.Cuenta EQ Mov_Contable.Cuenta
                                        AND Anexos.Nit EQ Mov_Contable.Nit
                                        AND Anexos.Ano EQ vYear USE-INDEX idprimanexos EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                    IF NOT AVAILABLE(Anexos) THEN
                        IF LOCKED(Anexos) THEN
                            NEXT.
                        ELSE DO:
                            CREATE Anexos.
                            ASSIGN Anexos.Agencia = Mov_Contable.Agencia
                                   Anexos.Nit = Mov_Contable.Nit
                                   Anexos.Cuenta = Mov_Contable.Cuenta
                                   Anexos.Ano = vYear
                                   Anexos.Cen_Costos = Mov_Contable.Cen_Costos.
                        END.

                    Anexos.Base[vMes] = Anexos.Base[vMes] + Mov_Contable.Base.

                    IF Mov_Contable.Db GT 0 THEN
                        Anexos.Db[vMes] = Anexos.Db[vMes] + Mov_Contable.Db.
                        
                    IF Mov_Contable.Cr GT 0 THEN
                        Anexos.Cr[vMes] = Anexos.Cr[vMes] + Mov_Contable.Cr.
                        
                    RELEASE Anexos.
                    LEAVE.
                END. /* REPEAT */
            END. /*ANEXOS*/
        END.  /* FOR EACH mov_contable*/
    END. /*do*/
END. /* agencias */

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
