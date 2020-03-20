DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Balance_de_Prueba".
DEFINE VAR LisEx AS CHARACTER.
DEFINE VAR i AS INTEGER.
DEFINE VAR vYear AS INTEGER.
DEFINE VAR Ct AS DECIMAL.
DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".
DEFINE VAR w_conTodas AS LOGICAL.

DEFINE TEMP-TABLE IEx
    FIELD NLinea AS INTEGER FORMAT "999999"
    FIELD Linea AS CHARACTER FORMAT "X(150)".

DEFINE TEMP-TABLE tmp_saldo LIKE sal_cuenta.

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

DEFINE VAR W_Naturaleza AS CHARACTER.
DEFINE VAR W_CtrNat AS INTEGER.
DEFINE VAR L_CC AS LOGICAL INITIAL TRUE.
DEFINE VAR TotDeb AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotCre AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotActIni AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotPasIni AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotPtrIni AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotResIni AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotActFin AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotPasFin AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotPtrFin AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotResFin AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".

DEFINE TEMP-TABLE TSCuentas NO-UNDO
    FIELD TS_Agencia AS INTEGER FORMAT "99"
    FIELD TS_NomAgencia AS CHARACTER FORMAT "X(3)"
    FIELD TS_Cuenta AS CHARACTER FORMAT "X(12)"
    FIELD TS_Nombre AS CHARACTER FORMAT "X(20)"
    FIELD TS_Nivel AS INTEGER
    FIELD TS_Nat AS CHARACTER FORMAT "X(2)"
    FIELD TS_Ini AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD TS_Db AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD TS_Cr AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD TS_Fin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD Niv1 AS CHARACTER FORMAT "X(6)"
    FIELD Niv2 AS CHARACTER FORMAT "X(6)"
    FIELD Niv3 AS CHARACTER FORMAT "X(6)"
    FIELD Niv4 AS CHARACTER FORMAT "X(6)"
    INDEX idxcta ts_cuenta ts_nivel
    INDEX idxcta1 ts_cuenta.

DEFINE TEMP-TABLE TSCuentasComparado LIKE TSCuentas.

DEFINE BUFFER bAgencias FOR agencias.
DEFINE VAR flagAgencia AS LOGICAL.
DEFINE VAR month1 AS INTEGER.
DEFINE VAR year1 AS INTEGER.
DEFINE VAR month2 AS INTEGER.
DEFINE VAR year2 AS INTEGER.
DEFINE VAR pasoBalanceComparado AS INTEGER.
DEFINE VAR flagSigueComparado AS LOGICAL INITIAL TRUE.
DEFINE VAR fecComparado AS DATE.

{incluido/Pantalla_Validacion3.i}


/* 1. Proceso_Imprimir */
PROCEDURE Proceso_Imprimir:
    DEFINE VAR Listado AS CHARACTER INITIAL "".

    EMPTY TEMP-TABLE TSCuentas.
    EMPTY TEMP-TABLE TSCuentasComparado.
    
    TotCre = 0.
    TotDeb = 0.
    TotActIni = 0.
    TotActFin = 0.
    TotPasIni = 0.
    TotPasFin = 0.
    TotPtrIni = 0.
    TotPtrFin = 0.
    TotResIni = 0.
    TotResFin = 0.

    RUN Tabla_Temporal.

    Listado = w_Pathspl + "BlceUsu_" + TRIM(w_usuario) + ".lst".
    
    {incluido\IMPRIMIR_carta_ag.I "listado"}.
END PROCEDURE.


/* 2. Tabla temporal */
PROCEDURE Tabla_Temporal:
    DEFINE VAR ki AS INTEGER INITIAL 1.
    
    SESSION:SET-WAIT-STATE("GENERAL").

    EMPTY TEMP-TABLE TMP_saldo.

    vYear = YEAR(W_Fec1).

    IF chkComparado = FALSE THEN DO:
        IF W_ag1 = W_ag2 THEN DO:
            FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia = W_Ag1
                                  AND Sal_Cuenta.Ano = vYear
                                  AND sal_cuenta.cen_costos >= W_CC1
                                  AND sal_cuenta.cen_costos <= W_CC2
                                  AND Sal_Cuenta.Cuenta >= W_Cuenta1
                                  AND Sal_Cuenta.Cuenta <= W_Cuenta2 NO-LOCK:
                FIND FIRST tmp_saldo WHERE tmp_saldo.cuenta = Sal_cuenta.cuenta NO-ERROR.
                IF NOT AVAILABLE(tmp_saldo) THEN do:
                    CREATE tmp_saldo.
                    BUFFER-COPY sal_cuenta TO tmp_saldo.
                END.
                ELSE DO:
                    tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + sal_cuenta.sal_inicial.

                    DO Ki = 1 TO 12:
                        tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + sal_cuenta.Cr[Ki].
                        tmp_saldo.db[Ki] = tmp_saldo.db[Ki] + sal_cuenta.db[Ki].
                    END.
                END.
            END.
        END.
        ELSE DO:
            FOR EACH agencias NO-LOCK:
                FOR EACH Sal_Cuenta WHERE Sal_cuenta.Agencia = Agencias.agencia
                                      AND Sal_Cuenta.Ano = vYear
                                      AND Sal_Cuenta.Cuenta >= W_Cuenta1
                                      AND Sal_Cuenta.Cuenta <= W_Cuenta2 NO-LOCK:
                    FIND FIRST TMP_SALDO WHERE TMP_SALDO.CUENTA = Sal_cuenta.cuenta
                                           AND tmp_saldo.agencia = sal_cuenta.agencia NO-ERROR.
                    IF NOT AVAILABLE(tmp_saldo) THEN DO:
                        CREATE tmp_saldo.
                        BUFFER-COPY sal_cuenta TO tmp_saldo.
                    END.
                    ELSE DO:
                        tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + sal_cuenta.sal_inicial.

                        DO Ki = 1 TO 12:
                            tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + sal_cuenta.Cr[Ki].
                            tmp_saldo.db[Ki] = tmp_saldo.db[Ki] + sal_cuenta.db[Ki].
                        END.
                    END.
                END.
            END.
        END.

        FOR EACH TMP_SALDO NO-LOCK:
            RUN Buscar_Cuentas (INPUT TMP_Saldo.Cuenta).
            IF NOT AVAILABLE cuentas THEN
                MESSAGE "La cuenta contable" tmp_saldo.cuenta "no se encuentra disponible." SKIP
                        "Por favor, informe la novedad al Administrador del Sistema."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            RUN Mayorizar.
        END.
    END.
    ELSE DO:
        month1 = MONTH(w_fec1).
        year1 = YEAR(w_fec1).
        month2 = MONTH(w_fec2).
        year2 = YEAR(w_fec2).
        
        /* Balance inicial */
        pasoBalanceComparado = 1.

        IF W_ag1 = W_ag2 THEN DO:
            FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia = W_Ag1
                                  AND Sal_Cuenta.Ano = year1
                                  AND sal_cuenta.cen_costos >= W_CC1
                                  AND sal_cuenta.cen_costos <= W_CC2
                                  AND Sal_Cuenta.Cuenta >= W_Cuenta1
                                  AND Sal_Cuenta.Cuenta <= W_Cuenta2 NO-LOCK:
                FIND FIRST TMP_SALDO WHERE TMP_SALDO.CUENTA = Sal_cuenta.cuenta NO-ERROR.
                IF NOT AVAILABLE(tmp_saldo) THEN do:
                    CREATE tmp_saldo.
                    BUFFER-COPY sal_cuenta TO tmp_saldo.
                END.
                ELSE DO:
                    tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + sal_cuenta.sal_inicial.

                    DO Ki = 1 TO 12:
                        tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + sal_cuenta.Cr[Ki].
                        tmp_saldo.db[Ki] = tmp_saldo.db[Ki] + sal_cuenta.db[Ki].
                    END.
                END.
            END.
        END.
        ELSE DO:
            FOR EACH agencias NO-LOCK:
                FOR EACH Sal_Cuenta WHERE Sal_cuenta.Agencia = Agencias.agencia
                                      AND Sal_Cuenta.Ano = year1
                                      AND Sal_Cuenta.Cuenta >= W_Cuenta1
                                      AND Sal_Cuenta.Cuenta <= W_Cuenta2 NO-LOCK:
                    FIND FIRST TMP_SALDO WHERE TMP_SALDO.CUENTA = Sal_cuenta.cuenta
                                           AND tmp_saldo.agencia = sal_cuenta.agencia NO-ERROR.
                    IF NOT AVAILABLE(tmp_saldo) THEN DO:
                        CREATE tmp_saldo.
                        BUFFER-COPY sal_cuenta TO tmp_saldo.
                    END.
                    ELSE DO:
                        tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + sal_cuenta.sal_inicial.

                        DO Ki = 1 TO 12:
                            tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + sal_cuenta.Cr[Ki].
                            tmp_saldo.db[Ki] = tmp_saldo.db[Ki] + sal_cuenta.db[Ki].
                        END.
                    END.
                END.
            END.
        END.

        fecComparado = w_fec1.

        FOR EACH TMP_SALDO NO-LOCK:
            RUN Buscar_Cuentas (INPUT TMP_Saldo.Cuenta).
            
            IF NOT AVAILABLE cuentas THEN
                MESSAGE "La cuenta contable" tmp_saldo.cuenta "no se encuentra disponible." SKIP
                        "Por favor, informe la novedad al Administrador del Sistema."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RUN MayorizarComparado.
        END.

        FOR EACH TSCuentasComparado:
            CREATE TSCuentas.
            BUFFER-COPY TSCuentasComparado TO TSCuentas.
            TSCuentas.TS_Ini = TSCuentas.TS_Fin.
        END.

        /* Balance final */
        EMPTY TEMP-TABLE TSCuentasComparado.
        EMPTY TEMP-TABLE TMP_saldo.

        TotCre = 0.
        TotDeb = 0.
        TotActIni = 0.
        TotActFin = 0.
        TotPasIni = 0.
        TotPasFin = 0.
        TotPtrIni = 0.
        TotPtrFin = 0.
        TotResIni = 0.
        TotResFin = 0.
        pasoBalanceComparado = 2.
        
        IF W_ag1 = W_ag2 THEN DO:
            FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia = W_Ag1
                                  AND Sal_Cuenta.Ano = year2
                                  AND sal_cuenta.cen_costos >= W_CC1
                                  AND sal_cuenta.cen_costos <= W_CC2
                                  AND Sal_Cuenta.Cuenta >= W_Cuenta1
                                  AND Sal_Cuenta.Cuenta <= W_Cuenta2 NO-LOCK:
                FIND FIRST TMP_SALDO WHERE TMP_SALDO.CUENTA = Sal_cuenta.cuenta NO-ERROR.
                IF NOT AVAILABLE(tmp_saldo) THEN do:
                    CREATE tmp_saldo.
                    BUFFER-COPY sal_cuenta TO tmp_saldo.
                END.
                ELSE DO:
                    tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + sal_cuenta.sal_inicial.

                    DO Ki = 1 TO 12:
                        tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + sal_cuenta.Cr[Ki].
                        tmp_saldo.db[Ki] = tmp_saldo.db[Ki] + sal_cuenta.db[Ki].
                    END.
                END.
            END.
        END.
        ELSE DO:
            FOR EACH agencias NO-LOCK:
                FOR EACH Sal_Cuenta WHERE Sal_cuenta.Agencia = Agencias.agencia
                                      AND Sal_Cuenta.Ano = year2
                                      AND Sal_Cuenta.Cuenta >= W_Cuenta1
                                      AND Sal_Cuenta.Cuenta <= W_Cuenta2 NO-LOCK:
                    FIND FIRST TMP_SALDO WHERE TMP_SALDO.CUENTA = Sal_cuenta.cuenta
                                           AND tmp_saldo.agencia = sal_cuenta.agencia NO-ERROR.
                    IF NOT AVAILABLE(tmp_saldo) THEN DO:
                        CREATE tmp_saldo.
                        BUFFER-COPY sal_cuenta TO tmp_saldo.
                    END.
                    ELSE DO:
                        tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + sal_cuenta.sal_inicial.

                        DO Ki = 1 TO 12:
                            tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + sal_cuenta.Cr[Ki].
                            tmp_saldo.db[Ki] = tmp_saldo.db[Ki] + sal_cuenta.db[Ki].
                        END.
                    END.
                END.
            END.
        END.

        fecComparado = w_fec2.

        FOR EACH TMP_SALDO NO-LOCK:
            RUN Buscar_Cuentas (INPUT TMP_Saldo.Cuenta).
            IF NOT AVAILABLE cuentas THEN
                MESSAGE "La cuenta contable" tmp_saldo.cuenta "no se encuentra disponible." SKIP
                        "Por favor, informe la novedad al Administrador del Sistema."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RUN MayorizarComparado.
        END.

        FOR EACH TSCuentasComparado NO-LOCK:
            FIND FIRST TSCuentas WHERE TSCuentas.TS_agencia = TSCuentasComparado.TS_agencia
                                   AND TSCuentas.TS_NomAgencia = TSCuentasComparado.TS_NomAgencia
                                   AND TSCuentas.TS_Cuenta = TSCuentasComparado.TS_Cuenta
                                   AND TSCuentas.TS_Nombre = TSCuentasComparado.TS_Nombre
                                   AND TSCuentas.TS_Nivel = TSCuentasComparado.TS_Nivel
                                   AND TSCuentas.TS_Nat = TSCuentasComparado.TS_Nat
                                   AND TSCuentas.niv1 = TSCuentasComparado.niv1
                                   AND TSCuentas.niv2 = TSCuentasComparado.niv2
                                   AND TSCuentas.niv3 = TSCuentasComparado.niv3
                                   AND TSCuentas.niv4 = TSCuentasComparado.niv4 NO-ERROR.
            IF NOT AVAILABLE TSCuentas THEN DO:
                create TSCuentas.
                BUFFER-COPY TSCuentasComparado TO TSCuentas.
                TSCuentas.TS_Ini = 0.
            END.
            ELSE DO:
                TSCuentas.TS_fin = TSCuentasComparado.TS_Fin.    
            END.
        END.


        /* 3. Débitos y Créditos */
        EMPTY TEMP-TABLE TSCuentasComparado.
        EMPTY TEMP-TABLE TMP_saldo.

        TotCre = 0.
        TotDeb = 0.
        TotActIni = 0.
        TotActFin = 0.
        TotPasIni = 0.
        TotPasFin = 0.
        TotPtrIni = 0.
        TotPtrFin = 0.
        TotResIni = 0.
        TotResFin = 0.
        pasoBalanceComparado = 3.

        month1 = month1 + 1.

        IF month1 = 13 THEN DO:
            month1 = 12.
            year1 = year1 + 1.
        END.

        DO WHILE (month1 <= month2 AND year1 <= year2) OR (month1 > month2 AND year1 < year2):
            IF W_ag1 = W_ag2 THEN DO:
                FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia = W_Ag1
                                      AND Sal_Cuenta.Ano = year1
                                      AND sal_cuenta.cen_costos >= W_CC1
                                      AND sal_cuenta.cen_costos <= W_CC2
                                      AND Sal_Cuenta.Cuenta >= W_Cuenta1
                                      AND Sal_Cuenta.Cuenta <= W_Cuenta2 NO-LOCK:
                    FIND FIRST TMP_SALDO WHERE TMP_SALDO.CUENTA = Sal_cuenta.cuenta NO-ERROR.
                    IF NOT AVAILABLE(tmp_saldo) THEN do:
                        CREATE tmp_saldo.
                        BUFFER-COPY sal_cuenta TO tmp_saldo.
                    END.
                    ELSE DO:
                        tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + sal_cuenta.sal_inicial.

                        DO Ki = 1 TO 12:
                            tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + sal_cuenta.Cr[Ki].
                            tmp_saldo.db[Ki] = tmp_saldo.db[Ki] + sal_cuenta.db[Ki].
                        END.
                    END.
                END.
            END.
            ELSE DO:
                FOR EACH agencias NO-LOCK:
                    FOR EACH Sal_Cuenta WHERE Sal_cuenta.Agencia = Agencias.agencia
                                          AND Sal_Cuenta.Ano = year2
                                          AND Sal_Cuenta.Cuenta >= W_Cuenta1
                                          AND Sal_Cuenta.Cuenta <= W_Cuenta2 NO-LOCK:
                        FIND FIRST TMP_SALDO WHERE TMP_SALDO.CUENTA = Sal_cuenta.cuenta
                                               AND tmp_saldo.agencia = sal_cuenta.agencia NO-ERROR.
                        IF NOT AVAILABLE(tmp_saldo) THEN DO:
                            CREATE tmp_saldo.
                            BUFFER-COPY sal_cuenta TO tmp_saldo.
                        END.
                        ELSE DO:
                            tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + sal_cuenta.sal_inicial.

                            DO Ki = 1 TO 12:
                                tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + sal_cuenta.Cr[Ki].
                                tmp_saldo.db[Ki] = tmp_saldo.db[Ki] + sal_cuenta.db[Ki].
                            END.
                        END.
                    END.
                END.
            END.

            fecComparado = DATE(month1, 1, year1).

            FOR EACH TMP_SALDO NO-LOCK:
                RUN Buscar_Cuentas (INPUT TMP_Saldo.Cuenta).
                IF NOT AVAILABLE cuentas THEN
                    MESSAGE "La cuenta contable" tmp_saldo.cuenta "no se encuentra disponible." SKIP
                            "Por favor, informe la novedad al Administrador del Sistema."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RUN MayorizarComparado.
            END.

            FOR EACH TSCuentasComparado NO-LOCK:
                FIND FIRST TSCuentas WHERE TSCuentas.TS_agencia = TSCuentasComparado.TS_agencia
                                       AND TSCuentas.TS_NomAgencia = TSCuentasComparado.TS_NomAgencia
                                       AND TSCuentas.TS_Cuenta = TSCuentasComparado.TS_Cuenta
                                       AND TSCuentas.TS_Nombre = TSCuentasComparado.TS_Nombre
                                       AND TSCuentas.TS_Nivel = TSCuentasComparado.TS_Nivel
                                       AND TSCuentas.TS_Nat = TSCuentasComparado.TS_Nat
                                       AND TSCuentas.niv1 = TSCuentasComparado.niv1
                                       AND TSCuentas.niv2 = TSCuentasComparado.niv2
                                       AND TSCuentas.niv3 = TSCuentasComparado.niv3
                                       AND TSCuentas.niv4 = TSCuentasComparado.niv4 NO-ERROR.
                IF NOT AVAILABLE TSCuentas THEN DO:
                    create TSCuentas.
                    BUFFER-COPY TSCuentasComparado TO TSCuentas.
                    TSCuentas.TS_Ini = 0.
                    TSCuentas.TS_Fin = 0.
                END.
                ELSE DO:
                    TSCuentas.TS_db = TSCuentas.TS_db + TSCuentasComparado.TS_db.
                    TSCuentas.TS_cr = TSCuentas.TS_cr + TSCuentasComparado.TS_cr.
                END.
            END.

            month1 = month1 + 1.

            IF month1 = 13 THEN DO:
                month1 = 12.
                year1 = year1 + 1.
            END.
        END.
    END.

    SESSION:SET-WAIT-STATE("").
END PROCEDURE.


PROCEDURE Buscar_Cuentas:
    DEFINE INPUT PARAMETER Cta AS CHARACTER.

    FIND FIRST Cuentas WHERE Cuentas.Cuenta = Cta NO-LOCK NO-ERROR.

END PROCEDURE.

/* 3. Mayorizar */
PROCEDURE Mayorizar:
    DEFINE VAR SIni AS DECIMAL.
    DEFINE VAR SDb AS DECIMAL.
    DEFINE VAR SCr AS DECIMAL.
    DEFINE VAR SFin AS DECIMAL.
    DEFINE VAR i AS INTEGER.
    DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
    DEFINE VAR MDb AS DECIMAL.
    DEFINE VAR MCr AS DECIMAL.
    DEFINE VAR MIni AS DECIMAL.
    DEFINE VAR MFin AS DECIMAL.
    DEFINE VAR MNt AS CHARACTER.

    MCta = tmp_saldo.Cuenta.

    RUN HallarSdo (INPUT MONTH(W_Fec1),
                   OUTPUT SIni,
                   OUTPUT SFin).

    FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta = Cuentas.Cuenta
                           AND TSCuentas.TS_Agencia = tmp_saldo.agencia NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(TSCuentas) THEN DO:
        flagAgencia = TRUE.
        RUN Grabar_Enc_Temporal.
    END.

    TSCuentas.TS_Ini = TSCuentas.TS_Ini + SIni.
    MINI = TSCuentas.TS_Ini.
    TSCuentas.TS_Db = TSCuentas.TS_Db + tmp_saldo.Db[MONTH(W_Fec1)].
    MDb = TSCuentas.TS_Db.
    TSCuentas.TS_Cr = TSCuentas.TS_Cr + tmp_saldo.Cr[MONTH(W_Fec1)].
    MCr = TSCuentas.TS_Cr.
    TSCuentas.TS_Fin = TSCuentas.TS_Fin  + SFin.
    MFin = TSCuentas.TS_Fin.
    MNt = Cuentas.Naturaleza.

    /*IF MCta = "24650501" THEN DO:
        FOR EACH tscuentas WHERE ts_cuenta = "24650501" NO-LOCK:
            MESSAGE 1 TSCuentas.ts_ini TSCuentas.ts_db tscuentas.ts_cr tscuentas.ts_fin
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.*/


    DO i = Cuentas.Nivel TO 1 BY -1:
        IF LENGTH(MCta) > 2 THEN
            MCta = SUBSTRING(MCta,1,LENGTH(MCta) - 2).
        ELSE
            MCta = SUBSTRING(MCta,1,LENGTH(MCta) - 1).

        RUN Buscar_Cuentas (INPUT MCta).
        IF NOT AVAILABLE(Cuentas) THEN
            NEXT.

        FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta = MCta NO-ERROR.
        IF NOT AVAILABLE(TSCuentas) THEN DO:
            flagAgencia = FALSE.
            RUN Grabar_Enc_Temporal.
        END.

        TSCuentas.TS_Db = TSCuentas.TS_DB + tmp_saldo.Db[MONTH(W_Fec1)].
        TSCuentas.TS_Cr = TSCuentas.TS_CR + tmp_saldo.Cr[MONTH(W_Fec1)].

        IF Cuentas.Naturaleza <> MNt THEN DO:
            TSCuentas.TS_Ini = TSCuentas.TS_Ini - SIni.
            TSCuentas.TS_Fin = TSCuentas.TS_Fin - SFin.
        END.
        ELSE DO:
            TSCuentas.TS_Ini  = TSCuentas.TS_Ini + SIni.
            TSCuentas.TS_Fin  = TSCuentas.TS_Fin + SFin.
        END.
    END.

    /*IF MCta = "24650501" THEN DO:
        FOR EACH tscuentas WHERE ts_cuenta = "24650501" NO-LOCK:
            MESSAGE 2 TSCuentas.ts_ini TSCuentas.ts_db tscuentas.ts_cr tscuentas.ts_fin
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.*/


END PROCEDURE.


PROCEDURE MayorizarComparado:
    DEFINE VAR SIni AS DECIMAL.
    DEFINE VAR SDb AS DECIMAL.
    DEFINE VAR SCr AS DECIMAL.
    DEFINE VAR SFin AS DECIMAL.
    DEFINE VAR i AS INTEGER.
    DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
    DEFINE VAR MDb AS DECIMAL.
    DEFINE VAR MCr AS DECIMAL.
    DEFINE VAR MIni AS DECIMAL.
    DEFINE VAR MFin AS DECIMAL.
    DEFINE VAR MNt AS CHARACTER.

    MCta = tmp_saldo.Cuenta.

    RUN HallarSdo (INPUT MONTH(fecComparado),
                   OUTPUT SIni,
                   OUTPUT SFin).

    FIND FIRST TSCuentasComparado WHERE TSCuentasComparado.TS_Cuenta = Cuentas.Cuenta
                                    AND TSCuentasComparado.TS_Agencia = tmp_saldo.agencia NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(TSCuentasComparado) THEN DO:
        flagAgencia = TRUE.
        RUN Grabar_Enc_TemporalComparado.
    END.

    TSCuentasComparado.TS_Ini = TSCuentasComparado.TS_Ini + SIni.
    MINI = TSCuentasComparado.TS_Ini.
    TSCuentasComparado.TS_Db = TSCuentasComparado.TS_Db + tmp_saldo.Db[MONTH(fecComparado)].
    MDb = TSCuentasComparado.TS_Db.
    TSCuentasComparado.TS_Cr = TSCuentasComparado.TS_Cr + tmp_saldo.Cr[MONTH(fecComparado)].
    MCr = TSCuentasComparado.TS_Cr.
    TSCuentasComparado.TS_Fin = TSCuentasComparado.TS_Fin  + SFin.
    MFin = TSCuentasComparado.TS_Fin.
    MNt = Cuentas.Naturaleza.

    DO i = Cuentas.Nivel TO 1 BY -1:
        IF LENGTH(MCta) > 2 THEN
            MCta = SUBSTRING(MCta,1,LENGTH(MCta) - 2).
        ELSE
            MCta = SUBSTRING(MCta,1,LENGTH(MCta) - 1).

        RUN Buscar_Cuentas (INPUT MCta).
        IF NOT AVAILABLE(Cuentas) THEN
            NEXT.

        FIND FIRST TSCuentasComparado WHERE TSCuentasComparado.TS_Cuenta = MCta NO-ERROR.
        IF NOT AVAILABLE(TSCuentasComparado) THEN DO:
            flagAgencia = FALSE.
            RUN Grabar_Enc_TemporalComparado.
        END.

        TSCuentasComparado.TS_Db = TSCuentasComparado.TS_DB + tmp_saldo.Db[MONTH(fecComparado)].
        TSCuentasComparado.TS_Cr = TSCuentasComparado.TS_CR + tmp_saldo.Cr[MONTH(fecComparado)].

        IF Cuentas.Naturaleza <> MNt THEN DO:
            TSCuentasComparado.TS_Ini = TSCuentasComparado.TS_Ini - SIni.
            TSCuentasComparado.TS_Fin = TSCuentasComparado.TS_Fin - SFin.
        END.
        ELSE DO:
            TSCuentasComparado.TS_Ini  = TSCuentasComparado.TS_Ini + SIni.
            TSCuentasComparado.TS_Fin  = TSCuentasComparado.TS_Fin + SFin.
        END.
    END.

END PROCEDURE.



PROCEDURE HallarSdo:
    DEFINE INPUT PARAMETER Smes AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini AS DECIMAL.
    DEFINE OUTPUT PARAMETER SFin AS DECIMAL.

    DEFINE VAR i AS INTEGER.

    SFin = tmp_saldo.Sal_Inicial.

    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza = "DB" THEN DO:
            SFin = SFin + tmp_saldo.DB[i] - tmp_saldo.Cr[i].
            SIni = SFin - tmp_saldo.DB[i] + tmp_saldo.Cr[i].
        END.
        ELSE DO:
            SFin = SFin - tmp_saldo.DB[i] + tmp_saldo.Cr[i].
            SIni = SFin + tmp_saldo.DB[i] - tmp_saldo.Cr[i].
        END.

        IF i = smes THEN DO:
            TotDeb = TotDeb + tmp_saldo.DB[i].
            TotCre = TotCre +  tmp_saldo.CR[i].
        END.
    END.

    CASE Cuentas.Id_Cuenta: /*arma totales iniciales y finales*/
        WHEN 1 THEN DO:
            IF Cuentas.Naturaleza = "DB" THEN DO:
                TotActIni = TotActIni + SIni.
                TotActFin = TotActFin + SFin.
            END.
            ELSE DO:
                TotActIni = TotActIni - SIni.
                TotActFin = TotActFin - SFin.
            END.
        END.

        WHEN 2 THEN DO:
            IF Cuentas.Naturaleza = "CR" THEN DO:
                TotPasIni = TotPasIni + SIni.
                TotPasFin = TotPasFin + SFin.
            END.
            ELSE DO:
                TotPasIni = TotPasIni - SIni.
                TotPasFin = TotPasFin - SFin.
            END.
        END.

        WHEN 3 THEN DO:
            IF Cuentas.Naturaleza = "CR" THEN DO:
                TotPtrIni = TotPtrIni + SIni.
                TotPtrFin = TotPtrFin + SFin.
            END.
            ELSE DO:
                TotPtrIni = TotPtrIni - SIni.
                TotPtrFin = TotPtrFin - SFin.
            END.
        END.

        WHEN 4 THEN DO:
            IF Cuentas.Naturaleza = "DB" THEN DO:
                TotResIni = TotResIni + SIni.
                TotResFin = TotResFin + SFin.
            END.
            ELSE DO:
                TotResIni = TotResIni - SIni.
                TotResFin = TotResFin - SFin.
            END.
        END.
    END CASE.

END PROCEDURE.


PROCEDURE Grabar_Enc_Temporal:
    CREATE TSCuentas.
    TSCuentas.TS_Cuenta = Cuentas.Cuenta.
    TSCuentas.TS_Nombre = Cuentas.Nombre.
    TSCuentas.TS_Nivel = Cuentas.Nivel.
    TSCuentas.TS_Nat = Cuentas.Naturaleza.
    TSCuentas.Niv1 = SUBSTRING(TSCuentas.TS_Cuenta,1,1).
    TSCuentas.Niv2 = SUBSTRING(TSCuentas.TS_Cuenta,1,2).
    TSCuentas.Niv3 = SUBSTRING(TSCuentas.TS_Cuenta,1,4).
    TSCuentas.Niv4 = SUBSTRING(TSCuentas.TS_Cuenta,1,6).

    IF flagAgencia = TRUE THEN DO:
        FIND FIRST bAgencias WHERE bAgencias.agencia = Tmp_Saldo.agencia NO-LOCK NO-ERROR.
        IF AVAILABLE bAgencias THEN DO:
            TSCuentas.TS_Agencia = tmp_Saldo.agencia.
            TSCuentas.TS_NomAgencia = bAgencias.nombre.
        END.
    END.

END PROCEDURE.


PROCEDURE Grabar_Enc_TemporalComparado:
    CREATE TSCuentasComparado.
    TSCuentasComparado.TS_Cuenta = Cuentas.Cuenta.
    TSCuentasComparado.TS_Nombre = Cuentas.Nombre.
    TSCuentasComparado.TS_Nivel = Cuentas.Nivel.
    TSCuentasComparado.TS_Nat = Cuentas.Naturaleza.
    TSCuentasComparado.Niv1 = SUBSTRING(TSCuentasComparado.TS_Cuenta,1,1).
    TSCuentasComparado.Niv2 = SUBSTRING(TSCuentasComparado.TS_Cuenta,1,2).
    TSCuentasComparado.Niv3 = SUBSTRING(TSCuentasComparado.TS_Cuenta,1,4).
    TSCuentasComparado.Niv4 = SUBSTRING(TSCuentasComparado.TS_Cuenta,1,6).

    IF flagAgencia = TRUE THEN DO:
        FIND FIRST bAgencias WHERE bAgencias.agencia = Tmp_Saldo.agencia NO-LOCK NO-ERROR.
        IF AVAILABLE bAgencias THEN DO:
            TSCuentasComparado.TS_Agencia = tmp_Saldo.agencia.
            TSCuentasComparado.TS_NomAgencia = bAgencias.nombre.
        END.
    END.

END PROCEDURE.



PROCEDURE ProcesoImprimir:
    {Incluido\RepEncabezadoBalance.i}

    DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)".
    DEFINE VAR Nom_Cencosto AS CHARACTER FORMAT "X(2)".
    DEFINE VAR vNombreAgencia AS CHARACTER.

    IF SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,1,3)  = "000" THEN
        vNombreAgencia = STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(12)").
    ELSE DO:
        FIND FIRST agencias WHERE agencia.agencia = w_ag1 NO-LOCK NO-ERROR.
        vNombreAgencia = STRING(STRING(agencias.agencia) + " - " + Agencias.nombre,"X(12)").
    END.

    IF chkComparado = FALSE THEN DO:
        W_Reporte = "REPORTE   : BALANCE DE PRUEBA - Agencia: " + vNombreAgencia + " al " + STRING(W_Fec1,"99/99/9999") + " - Generado el: " + STRING(TODAY,"99/99/9999") + " - " + STRING(TIME,"hh:mm am").
        W_EncColumna = "CUENTA       NOMBRE               AGE       SALDO INICIAL      MOVIMIENTOS DB      MOVIMIENTOS CR         SALDO FINAL NAT".
    END.
    ELSE DO:
        W_Reporte = "REPORTE   : BALANCE DE COMPARADO - Agencia: " + vNombreAgencia + " del " + STRING(W_Fec1,"99/99/9999") + " al " + STRING(W_Fec2,"99/99/9999") + " - Generado el: " + STRING(TODAY,"99/99/9999") + " - " + STRING(TIME,"hh:mm am").
        W_EncColumna = "CUENTA       NOMBRE               AGE       SALDO INICIAL      MOVIMIENTOS DB      MOVIMIENTOS CR         SALDO FINAL NAT".
    END.


    

    DEFINE VAR TotIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
    DEFINE VAR TotFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".

    DEFINE FRAME F_Mov
            TSCuentas.TS_Cuenta
            TSCuentas.TS_Nombre
            TSCuentas.TS_NomAgencia
            TSCuentas.TS_Ini
            TSCuentas.TS_Db
            TSCuentas.TS_Cr
            TSCuentas.TS_Fin
            TSCuentas.TS_Nat
        WITH DOWN WIDTH 160 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

    VIEW FRAME F-Encabezado.

    EMPTY TEMP-TABLE IEx.

    CREATE IEx.
    Ct = Ct + 1.
    IEx.NLinea = Ct.
    IEx.Linea = "CUENTA" + Cma + "NOMBRE" + Cma + "INICIAL" + Cma + "DEBITO" + Cma + "CREDITO" + Cma + "FINAL" + Cma + "NAT".

    /*FOR EACH tscuentas WHERE ts_cuenta = "24650501" NO-LOCK:
        MESSAGE TSCuentas.ts_ini TSCuentas.ts_db tscuentas.ts_cr tscuentas.ts_fin
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.*/

    FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel <= Cmb_Nivel
                         AND TS_Cuenta <> "" BREAK BY TSCuentas.TS_Cuenta
                                                   BY TSCuentas.TS_Nivel:
        IF TSCuentas.TS_Ini <> 0 OR TSCuentas.TS_Db <> 0 OR TSCuentas.TS_CR <> 0 OR TSCuentas.TS_Fin <> 0 THEN DO:
            IF SUBSTRING(TSCuentas.TS_Cuenta,2,1) = " " THEN
                DISPLAY SKIP(1) WITH FRAME F_Mov5 USE-TEXT STREAM-IO NO-LABELS NO-BOX.

            IF SUBSTRING(TSCuentas.TS_Cuenta,3,1) = " " AND SUBSTRING(TSCuentas.TS_Cuenta,2,1) <> " " THEN
                DISPLAY SKIP(1) WITH FRAME F_Mov6 USE-TEXT STREAM-IO NO-LABELS NO-BOX.

            IF SUBSTRING(TSCuentas.TS_Cuenta,4,1) <> " " AND SUBSTRING(TSCuentas.TS_Cuenta,5,1) = " " THEN
                DISPLAY SKIP WITH FRAME F_Mov6 USE-TEXT STREAM-IO NO-LABELS NO-BOX.

            DISPLAY TSCuentas.TS_Cuenta
                    TSCuentas.TS_Nombre
                    TSCuentas.TS_NomAgencia
                    TSCuentas.TS_Ini
                    TSCuentas.TS_Db
                    TSCuentas.TS_Cr
                    TSCuentas.TS_Fin
                    TSCuentas.TS_Nat
                    SKIP(0)
                WITH WIDTH 160 FRAME F_Mov USE-TEXT STREAM-IO NO-LABELS NO-BOX.

            CREATE IEx.
            Ct = Ct + 1.
            IEx.NLinea = Ct.
            IEx.Linea  = STRING(TSCuentas.TS_Cuenta) + Cma +
                         STRING(TSCuentas.TS_Nombre) + Cma +
                         STRING(TSCuentas.TS_Ini,"->>>>>>>>>>>>.99") + Cma +
                         STRING(TSCuentas.TS_Db,"->>>>>>>>>>>>.99") + Cma +
                         STRING(TSCuentas.TS_Cr,"->>>>>>>>>>>>.99") + Cma +
                         STRING(TSCuentas.TS_Fin,"->>>>>>>>>>>>.99") + Cma +
                         STRING(TSCuentas.TS_Nat).
        END.
    END.

    TotIni = TotActIni - (TotPasIni + TotPtrIni) + TotResIni.
    TotFin = ROUND((TotActFin - (TotPasFin + TotPtrFin) + TotResFin),0).

    DISPLAY SKIP(1)
            "Totales de Control:"
            ROUND(TotIni,0) FORMAT "->>,>>>,>>>,>>>,>>9.99" TO 62
            ROUND(TotDeb,2) FORMAT "->>,>>>,>>>,>>>,>>9.99" TO 84
            ROUND(TotCre,2) FORMAT "->>,>>>,>>>,>>>,>>9.99" TO 106
            ROUND(TotFin,0) FORMAT "->>,>>>,>>>,>>>,>>9.99" TO 131
        WITH FRAME T_Tot WIDTH 160 NO-LABELS STREAM-IO USE-TEXT NO-BOX.

    PAGE.

    OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE Imprimir_Excel:
    LisEx = w_Pathspl + "BcePrueba.csv".

    OUTPUT TO VALUE(LisEx).
    FOR EACH IEx BY IEx.NLinea:
        PUT IEx.Linea SKIP.
    END.
    OUTPUT CLOSE.

    MESSAGE "Balance de Prueba para Excel se encuentra en:" SKIP
            LisEx
        VIEW-AS ALERT-BOX INFORMATION.

    FOR EACH IEx:
        DELETE IEx.
    END.
END PROCEDURE.


/* Procedimientos de eventos */
PROCEDURE Habilita_Deshabilita:
/* En este procedimiento se habilitan o deshabilitan las variables a pedir en pantalla segun el informe que se vaya a ejecutar. */
    ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.

    DISABLE Cmb_Comprob
            W_Fec2
            W_NomUsuario1
            W_NomUsuario2
            W_NomCuenta1
            W_NomCuenta2
            W_Nit1
            W_NomNit1
            W_Nit2
            W_NomNit2
            W_Base
            W_Porcentaje
        WITH FRAME F_Valida.

    Cmb_comprob:HIDDEN = TRUE.
    W_fec2:VISIBLE = FALSE.

    W_Fec1:LABEL IN FRAME F_Valida = "Fecha de Corte".
    
    IF NOT L_CC THEN
        DISABLE Cmb_CenCost WITH FRAME F_Valida.

END PROCEDURE.


PROCEDURE Busca_Cuenta:
    DEFINE INPUT PARAMETER T_ConsCtai AS CHARACTER.
    DEFINE OUTPUT PARAMETER T_ConsCta AS CHARACTER.
    DEFINE OUTPUT PARAMETER T_ConsNom AS CHARACTER.

    IF T_ConsCtai <> "" THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = T_ConsCtai NO-LOCK NO-ERROR.
        IF AVAILABLE(Cuentas) THEN DO:
            T_ConsCta = Cuentas.Cuenta.
            T_ConsNom = Cuentas.Nombre.
        END.
    END.

    IF T_ConsCta <> "" OR T_ConsCta <> "?" THEN DO:
        RUN C-Cuentas.r (OUTPUT T_ConsCta,
                         OUTPUT T_ConsNom,
                         OUTPUT W_Naturaleza,
                         OUTPUT W_CtrNat,
                         INPUT "T").

        IF T_ConsCta = ? THEN DO:
            FIND FIRST Cuentas WHERE Cuentas.Cuenta = T_ConsCta
                                 AND cuentas.estado = 1 NO-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE(Cuentas) THEN DO:
                T_ConsCta = "".
                T_ConsNom = "".
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE asignarVariables:
    IF SUBSTRING(Cmb_CenCost:SCREEN-VALUE IN FRAME F_Valida,1,3) = "000" THEN DO:
        W_CC1 = 0.
        W_CC2 = 999.
    END.
    ELSE DO:
        W_CC1 = INTEGER(SUBSTRING(Cmb_CenCost:SCREEN-VALUE IN FRAME F_Valida,1,3)).
        W_CC2 = INTEGER(SUBSTRING(Cmb_CenCost:SCREEN-VALUE IN FRAME F_Valida,1,3)).
    END.

    IF SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,1,3) EQ "000" THEN DO:
        W_AG1 = 0.
        W_AG2 = 999.
    END.
    ELSE DO:
        W_AG1 = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,1,3)).
        W_AG2 = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,1,3)).
    END.

    IF SUBSTRING(Cmb_Comprob:SCREEN-VALUE IN FRAME F_Valida,1,2) EQ "00" THEN DO:
        W_CB1 = 0.
        W_CB2 = 99.
    END.
    ELSE DO:
        W_CB1 = INTEGER(SUBSTRING(Cmb_Comprob:SCREEN-VALUE IN FRAME F_Valida,1,2)).
        W_CB2 = INTEGER(SUBSTRING(Cmb_Comprob:SCREEN-VALUE IN FRAME F_Valida,1,2)).
    END.

END PROCEDURE.
