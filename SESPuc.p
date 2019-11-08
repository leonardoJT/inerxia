DEFINE INPUT PARAMETER W_Fec1 AS DATE.

{Incluido\VARIABLE.I "SHARED"}

DEFINE VARIABLE XANO AS INTEGER.
DEFINE VARIABLE W_ag1 AS INTEGER INITIAL 1 NO-UNDO.
DEFINE VARIABLE W_ag2 AS INTEGER INITIAL 999 NO-UNDO.
DEFINE VARIABLE W_Cuenta1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE W_Cuenta2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE Cmb_Nivel AS INTEGER INITIAL 8 NO-UNDO.
DEFINE VAR TotDeb AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotCre AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotActIni AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotPasIni AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotPtrIni AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotResIni AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotActFin AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotPasFin AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotPtrFin AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotResFin  AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99".

ASSIGN W_Cuenta1 = "0"
       W_Cuenta2 = "999999999999".

DEFINE TEMP-TABLE TMP_SALDO LIKE SAL_CUENTA.

DEFINE TEMP-TABLE TSCuentas NO-UNDO
    FIELD TS_Cuenta AS CHARACTER FORMAT "X(14)"
    FIELD TS_Nombre AS CHARACTER FORMAT "X(25)"
    FIELD TS_Nivel LIKE Cuentas.Nivel
    FIELD TS_Nat LIKE Cuentas.Naturaleza
    FIELD TS_Ini AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>9.99"
    FIELD TS_Db AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>9.99"
    FIELD TS_Cr AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>9.99"
    FIELD TS_Fin AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>9.99"
    FIELD Niv1 AS CHARACTER FORMAT "X(6)"
    FIELD Niv2 AS CHARACTER FORMAT "X(6)"
    FIELD Niv3 AS CHARACTER FORMAT "X(6)"
    FIELD Niv4 AS CHARACTER FORMAT "X(6)"
    INDEX idxcta ts_cuenta ts_nivel
    INDEX idxcta1 ts_cuenta.

DEFINE VARIABLE ki AS INTEGER INITIAL 1.

EMPTY TEMP-TABLE TSCuentas.
EMPTY TEMP-TABLE TMP_saldo.

ASSIGN XANO = YEAR(W_Fec1).

FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia >= W_Ag1
                      AND sal_cuenta.agencia <= W_Ag2
                      AND Sal_Cuenta.Ano EQ XANO NO-LOCK:
    IF /*Sal_Cuenta.Cen_Costos EQ 999 AND*/ Sal_Cuenta.Cuenta GE W_Cuenta1 AND Sal_Cuenta.Cuenta LE W_Cuenta2 THEN DO:
        FIND FIRST TMP_SALDO WHERE TMP_SALDO.CUENTA EQ Sal_cuenta.cuenta NO-ERROR.
        IF NOT AVAILABLE(tmp_saldo) THEN do:
            CREATE tmp_saldo.
            BUFFER-COPY sal_cuenta TO tmp_saldo.
        END.
        ELSE DO:
            DO Ki = 1 TO MONTH(w_fec1) /*12*/:
                ASSIGN tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + sal_cuenta.Cr[Ki]
                       tmp_saldo.db[Ki] = tmp_saldo.db[Ki] + sal_cuenta.db[Ki].
            END.

            ASSIGN tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + sal_cuenta.sal_inicial.
        END.
    END.
END.

FOR EACH TMP_SALDO NO-LOCK:
    RUN Buscar_Cuentas (INPUT TMP_Saldo.Cuenta).
    RUN Mayorizar.
END.

OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "SSPUC" + "-" + STRING(YEAR(W_Fec1), "9999") + "-" + STRING(MONTH(W_Fec1), "99") + ".csv") NO-CONVERT.
    FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel BREAK BY TSCuentas.TS_Cuenta
                                                                   BY TSCuentas.TS_Nivel:
        CASE LENGTH(TS_Cuenta):
            WHEN 1 THEN ASSIGN TS_Cuenta = STRING(DECIMAL(TS_Cuenta) * 100000).
            WHEN 2 THEN ASSIGN TS_Cuenta = STRING(DECIMAL(TS_Cuenta) * 10000).
            WHEN 3 THEN ASSIGN TS_Cuenta = STRING(DECIMAL(TS_Cuenta) * 1000).
            WHEN 4 THEN ASSIGN TS_Cuenta = STRING(DECIMAL(TS_Cuenta) * 100).
            WHEN 5 THEN ASSIGN TS_Cuenta = STRING(DECIMAL(TS_Cuenta) * 10).
        END CASE.

        IF LENGTH(TS_Cuenta) GT 6 THEN
            NEXT.

        EXPORT DELIMITER ";" TSCuentas.TS_Cuenta
                             TSCuentas.TS_Nombre
                             ABS(TSCuentas.TS_Fin).
    END.
OUTPUT CLOSE.

/* Buscar cuentas */
PROCEDURE Buscar_Cuentas:
    DEFINE INPUT PARAMETER Cta LIKE Cuentas.Cuenta.
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cta
                         /*AND Cuentas.Estado EQ 1*/ NO-LOCK NO-ERROR.
END PROCEDURE.

/* Mayorizar */
PROCEDURE Mayorizar:
    DEFINE VARIABLE SIni LIKE Sal_Cuenta.Sal_Inicial INITIAL 0.
    DEFINE VARIABLE SDb LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VARIABLE SCr LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VARIABLE SFin /*LIKE Sal_Cuenta.Sal_Inicial*/ AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>9.99" INITIAL 0.
    DEFINE VAR i AS INTEGER.
    DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
    DEFINE VAR MDb LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MCr LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MIni LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MNt LIKE Cuentas.Naturaleza.

    MCta = tmp_saldo.Cuenta.

    RUN HallarSdo (INPUT MONTH(W_Fec1),
                   OUTPUT SIni,
                   OUTPUT SFin).

    FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta EQ Cuentas.Cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(TSCuentas) THEN
        RUN Grabar_Enc_Temporal.

    ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini + SIni
           MINI = TSCuentas.TS_Ini
           TSCuentas.TS_Db = TSCuentas.TS_Db + tmp_saldo.Db[MONTH(W_Fec1)]
           MDb = TSCuentas.TS_Db
           TSCuentas.TS_Cr = TSCuentas.TS_Cr + tmp_saldo.Cr[MONTH(W_Fec1)]
           MCr = TSCuentas.TS_Cr
           TSCuentas.TS_Fin = TSCuentas.TS_Fin  + SFin
           MFin = TSCuentas.TS_Fin
           MNt = Cuentas.Naturaleza.

    DO i = Cuentas.Nivel TO 1 BY -1:
        IF LENGTH(MCta) GT 2 THEN
            MCta = SUBSTRING(MCta,1,LENGTH(MCta) - 2).
        ELSE
            MCta = SUBSTRING(MCta,1,LENGTH(MCta) - 1).

        RUN Buscar_Cuentas (INPUT MCta).
        IF NOT AVAILABLE(Cuentas) THEN
            NEXT.

        FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta  EQ MCta NO-ERROR.
        IF NOT AVAILABLE(TSCuentas) THEN
            RUN Grabar_Enc_Temporal.

        ASSIGN TSCuentas.TS_Db = TSCuentas.TS_DB + tmp_saldo.Db[MONTH(W_Fec1)] /*MDb*/
               TSCuentas.TS_Cr = TSCuentas.TS_CR + tmp_saldo.Cr[MONTH(W_Fec1)]. /*MCr.*/

        IF Cuentas.Naturaleza NE MNt THEN
            ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini - SIni /*MIni*/
                   TSCuentas.TS_Fin = TSCuentas.TS_Fin - SFin. /*MFin.*/
        ELSE
            ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini + SIni /*MIni*/
                   TSCuentas.TS_Fin = TSCuentas.TS_Fin + SFin. /*MFin.*/
    END.
END PROCEDURE.

/* Hallar saldo */
PROCEDURE HallarSdo:
    DEFINE INPUT PARAMETER Smes AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SFin /*LIKE Sal_Cuenta.Sal_Inicial*/ AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>9.99".

    DEFINE VAR i AS INTEGER.

    SFin = tmp_saldo.Sal_Inicial.

    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
            ASSIGN SFin = SFin + tmp_saldo.DB[i] - tmp_saldo.Cr[i]
                   SIni = SFin - tmp_saldo.DB[i] + tmp_saldo.Cr[i].
        ELSE
            ASSIGN SFin = SFin - tmp_saldo.DB[i] + tmp_saldo.Cr[i]
                   SIni = SFin + tmp_saldo.DB[i] - tmp_saldo.Cr[i].

        IF i = smes THEN
            ASSIGN TotDeb = TotDeb + tmp_saldo.DB[i]
                   TotCre = TotCre +  tmp_saldo.CR[i].
    END.

    CASE Cuentas.Id_Cuenta: /*arma totales iniciales y finales*/
        WHEN 1 THEN DO:
            IF Cuentas.Naturaleza EQ "DB" THEN
                ASSIGN TotActIni = TotActIni + SIni
                       TotActFin = TotActFin + SFin.
            ELSE
                ASSIGN TotActIni = TotActIni - SIni
                       TotActFin = TotActFin - SFin.
        END.

        WHEN 2 THEN DO:
            IF Cuentas.Naturaleza EQ "CR" THEN
                ASSIGN TotPasIni = TotPasIni + SIni
                       TotPasFin = TotPasFin + SFin.
            ELSE
                ASSIGN TotPasIni = TotPasIni - SIni
                       TotPasFin = TotPasFin - SFin.
        END.

        WHEN 3 THEN DO:
            IF Cuentas.Naturaleza EQ "CR" THEN
                ASSIGN TotPtrIni = TotPtrIni + SIni
                       TotPtrFin = TotPtrFin + SFin.
            ELSE
                ASSIGN TotPtrIni = TotPtrIni - SIni
                       TotPtrFin = TotPtrFin - SFin.
        END.

        WHEN 4 THEN DO:
            IF Cuentas.Naturaleza EQ "DB" THEN
                ASSIGN TotResIni = TotResIni + SIni
                       TotResFin = TotResFin + SFin.
            ELSE
                ASSIGN TotResIni = TotResIni - SIni
                       TotResFin = TotResFin - SFin.
        END.
    END CASE.
END PROCEDURE.

/* graba encabezado */
PROCEDURE Grabar_Enc_Temporal:
    CREATE TSCuentas.
    ASSIGN TSCuentas.TS_Cuenta = Cuentas.Cuenta
           TSCuentas.TS_Nombre = Cuentas.Nombre
           TSCuentas.TS_Nivel = Cuentas.Nivel
           TSCuentas.TS_Nat = Cuentas.Naturaleza
           TSCuentas.Niv1 = SUBSTRING(TSCuentas.TS_Cuenta,1,1)
           TSCuentas.Niv2 = SUBSTRING(TSCuentas.TS_Cuenta,1,2)
           TSCuentas.Niv3 = SUBSTRING(TSCuentas.TS_Cuenta,1,4)
           TSCuentas.Niv4 = SUBSTRING(TSCuentas.TS_Cuenta,1,6).

END PROCEDURE.
