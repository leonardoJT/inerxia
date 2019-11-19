DEFINE VARIABLE w_ano AS INTEGER NO-UNDO.
DEFINE VARIABLE w_mes AS INTEGER NO-UNDO.
DEFINE VAR dia1 AS INTEGER.
DEFINE VAR dia2 AS INTEGER.
DEFINE VARIABLE w_FEC1 AS DATE.
DEFINE VARIABLE w_FEC2 AS DATE.
DEFINE VARIABLE i AS INTEGER.

w_FEC1 = 02/01/2017.
w_FEC2 = 02/28/2017.
w_ano = YEAR(w_fec1).
w_mes = MONTH(W_Fec1).
dia1 = DAY(w_Fec1).
dia2 = DAY(w_fec2).

MESSAGE "Inicia Mayorización"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH agencias WHERE agencias.agencia = 2 NO-LOCK:
    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND SAL_CUENTA.ano EQ w_ano:
        ASSIGN sal_cuenta.db[w_mes] = 0
               sal_cuenta.cr[w_mes] = 0.
    END.

    DO i = dia1 TO dia2:
        w_FEC1 = DATE(w_mes,i,w_ano).

        FOR EACH Mov_Contable WHERE Mov_contable.agencia EQ agencias.agencia
                                AND Mov_Contable.Fec_Contable EQ W_Fec1 NO-LOCK:
            IF (Mov_Contable.Cuenta) <> "" THEN DO:
                REPEAT:
                    FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Agencia EQ Mov_contable.Agencia
                                            AND Sal_cuenta.Cen_Costos EQ Mov_contable.Cen_Costos
                                            AND Sal_Cuenta.Cuenta EQ Mov_Contable.Cuenta
                                            AND Sal_Cuenta.Ano EQ w_ano USE-INDEX idppal EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                    IF NOT AVAILABLE(Sal_Cuenta) THEN DO:
                        IF LOCKED(Sal_Cuenta) THEN
                            NEXT.
                        ELSE DO:
                            CREATE Sal_Cuenta.
                            ASSIGN Sal_Cuenta.Agencia = Mov_contable.Agencia
                                   Sal_Cuenta.Cuenta = Mov_contable.Cuenta
                                   Sal_Cuenta.Ano = w_ano
                                   Sal_Cuenta.Cen_Costos = Mov_contable.Cen_Costos.
                        END.
                    END.

                    IF Mov_contable.Db GT 0 THEN
                        Sal_Cuenta.Db[w_mes] = Sal_Cuenta.Db[w_mes] + Mov_Contable.Db.
                
                    IF Mov_Contable.Cr GT 0 THEN
                        Sal_Cuenta.Cr[w_mes] = Sal_Cuenta.Cr[w_mes] + Mov_contable.Cr.

                    RELEASE Sal_Cuenta.
                    LEAVE.
                END. /* REPEAT */
            END.
        END.  /* FOR EACH mov_contable*/
    END. /*do*/
END. /* agencias */

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
