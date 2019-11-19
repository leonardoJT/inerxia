DISABLE TRIGGERS FOR LOAD OF anexos.
DISABLE TRIGGERS FOR LOAD OF anexos13.

DEFINE VARIABLE w_ano AS INTEGER.
DEFINE VARIABLE w_mes AS INTEGER NO-UNDO.
DEFINE VAR dia1 AS INTEGER.
DEFINE VAR dia2 AS INTEGER.
DEFINE VARIABLE w_FEC1 AS DATE.
DEFINE VARIABLE w_FEC2 AS DATE.
DEFINE VARIABLE cont AS INTEGER.
DEFINE VAR vCuenta AS CHARACTER.
DEFINE VAR vAgencia AS INTEGER.
DEFINE VAR tempFecha AS DATE.

vCuenta = "27101001".
w_FEC1 = 01/01/2015.
w_FEC2 = 12/31/2015.
dia1 = DAY(w_Fec1).
dia2 = DAY(w_fec2).
w_ano = YEAR(w_fec1).

MESSAGE "Inicia Mayorización de Anexos"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

DO cont = 1 TO 12:
    FOR EACH sal_cuenta WHERE sal_cuenta.ano = w_ano
                          AND sal_cuenta.cuenta = vCuenta:
        ASSIGN sal_cuenta.db[cont] = 0
               sal_cuenta.cr[cont] = 0.
    END.

    FOR EACH sal_cuenta13 WHERE sal_cuenta13.ano = w_ano
                          AND sal_cuenta13.cuenta = vCuenta:
        ASSIGN sal_cuenta13.db[cont] = 0
               sal_cuenta13.cr[cont] = 0.
    END.

    FOR EACH anexos13 WHERE anexos13.ano = w_ano
                        AND anexos13.cuenta = vCuenta:
        ASSIGN anexos13.db[cont] = 0
               anexos13.cr[cont] = 0.
    END.

    FOR EACH anexos WHERE anexos.ano = w_ano
                      AND anexos.cuenta = vCuenta:
        ASSIGN anexos.db[cont] = 0
               anexos.cr[cont] = 0.
    END.

    FOR EACH anexos13 WHERE anexos13.ano = w_ano
                        AND anexos13.cuenta = vCuenta:
        ASSIGN anexos13.db[cont] = 0
               anexos13.cr[cont] = 0.
    END.
END.

FOR EACH agencias NO-LOCK:
    DO tempFecha = w_fec1 TO w_fec2:
        w_mes = MONTH(tempFecha).

        FOR EACH Mov_Contable WHERE Mov_contable.agencia = agencias.agencia
                                AND mov_contable.cuenta = vCuenta
                                AND Mov_Contable.Fec_Contable = tempFecha NO-LOCK:
            FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Agencia = Mov_contable.Agencia
                                    AND Sal_cuenta.Cen_Costos = Mov_contable.Cen_Costos
                                    AND Sal_Cuenta.Cuenta = Mov_Contable.Cuenta
                                    AND Sal_Cuenta.Ano = w_ano USE-INDEX idppal EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAILABLE(Sal_Cuenta) THEN DO:
                CREATE Sal_Cuenta.
                ASSIGN Sal_Cuenta.Agencia = Mov_contable.Agencia
                       Sal_Cuenta.Cuenta = Mov_contable.Cuenta
                       Sal_Cuenta.Ano = w_ano
                       Sal_Cuenta.Cen_Costos = Mov_contable.Cen_Costos.
            END.

            Sal_Cuenta.Db[w_mes] = Sal_Cuenta.Db[w_mes] + Mov_Contable.Db.
            Sal_Cuenta.Cr[w_mes] = Sal_Cuenta.Cr[w_mes] + Mov_contable.Cr.

            FIND FIRST Sal_Cuenta13 WHERE Sal_Cuenta13.Agencia = Mov_contable.Agencia
                                      AND Sal_cuenta13.Cen_Costos = Mov_contable.Cen_Costos
                                      AND Sal_Cuenta13.Cuenta = Mov_Contable.Cuenta
                                      AND Sal_Cuenta13.Ano = w_ano USE-INDEX idppal EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAILABLE(Sal_Cuenta13) THEN DO:
                CREATE Sal_Cuenta13.
                ASSIGN Sal_Cuenta13.Agencia = Mov_contable.Agencia
                       Sal_Cuenta13.Cuenta = Mov_contable.Cuenta
                       Sal_Cuenta13.Ano = w_ano
                       Sal_Cuenta13.Cen_Costos = Mov_contable.Cen_Costos.
            END.

            Sal_Cuenta13.Db[w_mes] = Sal_Cuenta13.Db[w_mes] + Mov_Contable.Db.
            Sal_Cuenta13.Cr[w_mes] = Sal_Cuenta13.Cr[w_mes] + Mov_contable.Cr.


            IF mov_contable.nit <> "" THEN DO:
                FIND FIRST anexos WHERE anexos.Agencia = Mov_Contable.Agencia
                                    AND anexos.Cen_Costos = Mov_Contable.Cen_Costos
                                    AND anexos.Cuenta = Mov_Contable.Cuenta
                                    AND anexos.Nit = Mov_Contable.Nit
                                    AND anexos.Ano = w_ano USE-INDEX idprimanexos NO-ERROR.
                IF NOT AVAILABLE(anexos) THEN DO:
                    CREATE anexos.
                    ASSIGN anexos.Agencia = Mov_Contable.Agencia
                           anexos.Nit = Mov_Contable.Nit
                           anexos.Cuenta = Mov_Contable.Cuenta
                           anexos.Ano = w_ano
                           anexos.Cen_Costos = Mov_Contable.Cen_Costos.
                END.
            
                anexos.Db[w_mes] = anexos.Db[w_mes] + Mov_Contable.Db.
                anexos.Cr[w_mes] = anexos.Cr[w_mes] + Mov_Contable.Cr.
            END.
            ELSE DO:
                DISPLAY mov_contable WITH WIDTH 300 1 COL.
            END.

            /* Anexos13 */
           FIND FIRST anexos13 WHERE anexos13.Agencia = Mov_Contable.Agencia
                                AND anexos13.Cen_Costos = Mov_Contable.Cen_Costos
                                AND anexos13.Cuenta = Mov_Contable.Cuenta
                                AND anexos13.Nit = Mov_Contable.Nit
                                AND anexos13.Ano = w_ano USE-INDEX idprimanexos NO-ERROR.
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
