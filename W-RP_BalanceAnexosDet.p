DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Balance de Anexos".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
DEFINE VARIABLE W_CtrNat LIKE Cuentas.Ctr_Naturaleza.
DEFINE VARIABLE L_CC AS LOGICAL INITIAL YES.
DEFINE VAR W_NitEnc LIKE Clientes.Nit.
DEFINE VAR TotActIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPasIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPtrIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotResIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotActFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPasFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPtrFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotResFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".

DEFINE TEMP-TABLE TSCuentas
    FIELD TS_Cuenta AS CHARACTER FORMAT "X(14)"
    FIELD TS_Agencia AS INTEGER
    FIELD TS_Nit AS CHARACTER FORMAT "X(14)"
    FIELD TS_Nombre AS CHARACTER FORMAT "X(25)"
    FIELD TS_Nivel AS INTEGER
    FIELD TS_IdMov AS LOGICAL
    FIELD TS_Nat AS CHARACTER
    FIELD TS_Ini AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD TS_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9"

    /* oakley */

    FIELD TS_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD TS_Fin AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    INDEX idxcta ts_cuenta.

DEFINE TEMP-TABLE TD_Detalle
    FIELD TD_Age LIKE Agencias.Agencia
    FIELD TD_Cbt AS INTEG FORM "99"
    FIELD TD_Cta AS CHARACTER FORMAT "X(14)"
    FIELD TD_Nit AS CHARACTER FORMAT "X(14)"
    FIELD TD_Com AS CHARACTER FORMAT "X(20)"
    FIELD TD_Fec AS DATE
    FIELD TD_Doc AS INTEG FORM "9999999"
    FIELD TD_Deb LIKE Mov_Contable.Db
    FIELD TD_Cre LIKE Mov_Contable.Cr
    FIELD W_RowIDMov AS ROWID
    INDEX idxAge td_Age.

/* incluido de Pantalla con parametros*/
{incluido/Pantalla_Validacion.i}
/* fin incluido Pantalla parametros */

PROCEDURE Busca_Cuenta:
    DEFINE INPUT PARAMETER T_ConsCtai LIKE Cuentas.Cuenta.
    DEFINE OUTPUT PARAMETER T_ConsCta LIKE Cuentas.Cuenta.
    DEFINE OUTPUT PARAMETER T_ConsNom LIKE Cuentas.Nombre.

    IF T_ConsCtai NE "" THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ T_ConsCtai
                             /*AND Cuentas.Estado EQ 1*/ NO-LOCK NO-ERROR.
        IF AVAILABLE(Cuentas) THEN
            ASSIGN T_ConsCta = Cuentas.Cuenta
                   T_ConsNom = Cuentas.Nombre.
    END.

    IF T_ConsCta NE "" OR T_ConsCta NE "?" THEN DO:
        RUN C-Cuentas.r (OUTPUT T_ConsCta,
                         OUTPUT T_ConsNom,
                         OUTPUT W_Naturaleza,
                         OUTPUT W_CtrNat,
                         INPUT "T").

        IF T_ConsCta EQ ? THEN DO:
            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta
                                 /*AND Cuentas.Estado EQ 1*/ NO-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE(Cuentas) THEN
                ASSIGN T_ConsCta = ""
                       T_ConsNom = "".
        END.
    END.
END PROCEDURE.

PROCEDURE Mayorizar:
    DEFINE VARIABLE SIni LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VARIABLE SDb LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VARIABLE SCr LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VARIABLE SFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR i AS INTEGER.
    DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
    DEFINE VAR MDb LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MCr LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MIni LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MNt LIKE Cuentas.Naturaleza.

    MCta = Anexos.Cuenta.
    W_NitEnc = Anexos.Nit.

    RUN Buscar_Cuentas(INPUT Anexos.Cuenta).

    IF AVAILABLE Cuentas THEN DO:
        RUN HallarSdo (INPUT MONTH(W_Fec1),
                       OUTPUT SIni,
                       OUTPUT SFin).

        FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta EQ Anexos.Cuenta
                               AND TSCuentas.TS_Nit EQ W_NitEnc NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(TSCuentas) THEN
            RUN Grabar_Enc_Temporal.

        ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini + SIni
               MINI = TSCuentas.TS_Ini
               TSCuentas.TS_Db = TSCuentas.TS_Db + Anexos.Db[MONTH(W_Fec1)]
               MDb = TSCuentas.TS_Db
               TSCuentas.TS_Cr = TSCuentas.TS_Cr + Anexos.Cr[MONTH(W_Fec1)]
               MCr = TSCuentas.TS_Cr
               TSCuentas.TS_Fin = TSCuentas.TS_Fin + SFin
               MFin = TSCuentas.TS_Fin
               MNt = TSCuentas.TS_Nat.

        DO i = TSCuentas.TS_Niv TO 1 BY -1:
            IF LENGTH(MCta) GT 2 THEN
                MCta = SUBSTRING(MCta,1,LENGTH(MCta) - 2).
            ELSE
                MCta = SUBSTRING(MCta,1,LENGTH(MCta) - 1).

            RUN Buscar_Cuentas (INPUT MCta).
            IF NOT AVAILABLE(Cuentas) THEN
                NEXT.

            FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta  EQ MCta
                                   AND TSCuentas.TS_Nit EQ "" NO-ERROR.

            W_NitEnc = "".

            IF NOT AVAILABLE(TSCuentas) THEN
                RUN Grabar_Enc_Temporal.

            ASSIGN TSCuentas.TS_Db = TSCuentas.TS_DB + Anexos.Db[MONTH(W_Fec1)] /*MDb*/
                   TSCuentas.TS_Cr = TSCuentas.TS_CR + Anexos.Cr[MONTH(W_Fec1)]. /*MCr.*/

            IF TSCuentas.TS_Nat NE MNt THEN
                ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini - SIni /*MIni*/
                       TSCuentas.TS_Fin = TSCuentas.TS_Fin - SFin. /*MFin.*/
            ELSE
                ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini + SIni /*MIni*/
                       TSCuentas.TS_Fin = TSCuentas.TS_Fin + SFin. /*MFin.*/
        END.

        FOR EACH Mov_Contable WHERE Mov_Contable.Agencia EQ Anexos.Agencia
                                AND Mov_Contable.Cuenta EQ Anexos.Cuenta
                                AND Mov_Contable.Nit EQ Anexos.Nit
                                AND Mov_Contable.Fec_Contable GE DATE(STRING(W_Fec1 - DAY(W_Fec1) + 1))
                                AND Mov_Contable.Fec_Contable LE DATE(STRING(W_Fec1)) NO-LOCK:
            FIND FIRST TD_Detalle WHERE TD_Detalle.W_RowIDMov EQ ROWID(Mov_Contable) NO-ERROR.
            IF NOT AVAIL(TD_Detalle) THEN DO:
                CREATE TD_Detalle.
                ASSIGN TD_Detalle.TD_Age = Mov_Contable.Agencia
                       TD_Detalle.TD_Cta = Mov_Contable.Cuenta
                       TD_Detalle.TD_Nit = Mov_Contable.Nit
                       TD_Detalle.TD_Cbt = Mov_Contable.Comprobante
                       TD_Detalle.TD_Fec = Mov_Contable.Fec_Contable
                       TD_Detalle.TD_Com = Mov_Contable.Comentario
                       TD_Detalle.TD_Doc = Mov_Contable.Num_Documento
                       TD_Detalle.TD_Deb = Mov_Contable.Db
                       TD_Detalle.TD_Cre = Mov_Contable.Cr
                       W_RowIDMov = ROWID(Mov_Contable).
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE Grabar_Enc_Temporal:
    IF AVAILABLE Cuentas THEN DO:
        CREATE TSCuentas.
        ASSIGN TSCuentas.TS_Cuenta = Cuentas.Cuenta
               TSCuentas.TS_Nit = W_NitEnc
               TSCuentas.TS_Nombre = Cuentas.Nombre
               TSCuentas.TS_Nivel = Cuentas.Nivel
               TSCuentas.TS_Nat = Cuentas.Naturaleza.

        IF Cuentas.Tipo EQ 2 THEN
            TSCuentas.TS_IdMov = YES.
    END.
END PROCEDURE.

PROCEDURE Buscar_Cuentas:
    DEFINE INPUT PARAMETER Cta LIKE Cuentas.Cuenta.

    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cta
                         /*AND Cuentas.Estado EQ 1*/ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE HallarSdo:
    DEFINE INPUT PARAMETER Smes AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SFin LIKE Sal_Cuenta.Sal_Inicial.

    DEFINE VAR i AS INTEGER.

    SFin = Anexos.Sdo_Inicial.

    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
            ASSIGN SFin = SFin + Anexos.DB[i] - Anexos.Cr[i]
                   SIni = SFin - Anexos.DB[i] + Anexos.Cr[i].
        ELSE
            ASSIGN SFin = SFin - Anexos.DB[i] + Anexos.Cr[i]
                   SIni = SFin + Anexos.DB[i] - Anexos.Cr[i].
    END.

    CASE Cuentas.Id_Cuenta: /*arma totales iniciales y finales*/
        WHEN 1 THEN
            ASSIGN TotActIni = TotActIni + SIni
                   TotActFin = TotActFin + SFin.

        WHEN 2 THEN
            ASSIGN TotPasIni = TotPasIni + SIni
                   TotPasFin = TotPasFin + SFin.

        WHEN 3 THEN
            ASSIGN TotPtrIni = TotPtrIni + SIni
                   TotPtrFin = TotPtrFin + SFin.

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

PROCEDURE Habilita_Deshabilita:
    /* En este procedimiento se habilitan o deshabilitan las variables a pedir en pantalla segun el informe que se vaya a ejecutar. */
    ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.

    DISABLE Cmb_Comprob
            W_fec2
            W_NomUsuario1
            W_NomUsuario2
            W_NomCuenta1
            W_NomCuenta2
            W_NomNit1
            W_NomNit2
            W_Base
            W_Porcentaje
        WITH FRAME F_Valida.

    ASSIGN W_Fec1:LABEL IN FRAME F_Valida = "Fecha Corte"
           W_Fec2:LABEL IN FRAME F_Valida = "Fecha Día".

    IF NOT L_CC THEN
        DISABLE Cmb_CenCost WITH FRAME F_Valida.
END PROCEDURE.

PROCEDURE Tabla_Temporal:  /* TT */
    EMPTY TEMP-TABLE TSCuentas.
    EMPTY TEMP-TABLE TD_Detalle.

    IF YEAR(w_fec1 + 1) <> YEAR(w_fec1) THEN DO:
        MESSAGE "¿Desea ver los saldos antes del cierre de año?"
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO TITLE "Saldos de cierre de año" UPDATE flagAntesDeCierre AS LOGICAL.
    END.

    IF flagAntesDeCierre = NO THEN DO:
        FOR EACH Anexos WHERE Anexos.Agencia GE W_Ag1
                          AND Anexos.Agencia LE W_Ag2
                          AND Anexos.Cen_Costos GE W_CC1
                          AND Anexos.Cen_Costos LE W_CC2
                          AND Anexos.Cuenta GE W_Cuenta1
                          AND Anexos.Cuenta LE W_Cuenta2
                          AND ((anexos.Nit GE W_Nit1 AND anexos.Nit LE W_Nit2) OR
                               ((INDEX(anexos.nit,W_Nit1) = 1 AND LENGTH(W_Nit1) = LENGTH(anexos.nit) - 2) OR (INDEX(anexos.nit,W_Nit2) = 1 AND LENGTH(W_Nit2) = LENGTH(anexos.nit) - 2)) OR
                               ((INDEX(W_Nit1,anexos.nit) = 1 AND LENGTH(anexos.nit) = LENGTH(W_Nit1) - 2) OR (INDEX(W_Nit2,anexos.nit) = 1 AND LENGTH(anexos.nit) = LENGTH(W_Nit2) - 2)))
                          AND Anexos.Ano EQ YEAR(W_Fec1) NO-LOCK:
            RUN Buscar_Cuentas (INPUT Anexos.Cuenta).

            IF cuentas.id_nit THEN
                RUN Mayorizar.       /* Solo procesa las cuenta que manejan nit - abril 25/2006 w+c*/
        END.
    END.
    ELSE DO:
        FOR EACH anexos WHERE anexos.Agencia GE W_Ag1
                          AND anexos.Agencia LE W_Ag2
                          AND anexos.Cen_Costos GE W_CC1
                          AND anexos.Cen_Costos LE W_CC2
                          AND anexos.Cuenta GE W_Cuenta1
                          AND anexos.Cuenta LE W_Cuenta2
                          AND ((anexos.Nit GE W_Nit1 AND anexos.Nit LE W_Nit2) OR
                               ((INDEX(anexos.nit,W_Nit1) = 1 AND LENGTH(W_Nit1) = LENGTH(anexos.nit) - 2) OR (INDEX(anexos.nit,W_Nit2) = 1 AND LENGTH(W_Nit2) = LENGTH(anexos.nit) - 2)) OR
                               ((INDEX(W_Nit1,anexos.nit) = 1 AND LENGTH(anexos.nit) = LENGTH(W_Nit1) - 2) OR (INDEX(W_Nit2,anexos.nit) = 1 AND LENGTH(anexos.nit) = LENGTH(W_Nit2) - 2)))
                          AND anexos.Ano EQ YEAR(W_Fec1) NO-LOCK:
            RUN Buscar_Cuentas (INPUT anexos.Cuenta).

            IF cuentas.id_nit THEN
                RUN Mayorizar13.       /* Solo procesa las cuenta que manejan nit - abril 25/2006 w+c*/
        END.
    END.

    FOR EACH tscuentas WHERE tscuentas.ts_cuenta GT "  "
                         AND TSCuentas.TS_Ini = 0   /* Elimina cuentas en cero. */
                         AND TSCuentas.TS_Db = 0
                         AND TSCuentas.TS_Cr = 0
                         AND TSCuentas.TS_Fin = 0:
        DELETE tscuentas.
    END.
END PROCEDURE.

PROCEDURE Proceso_Imprimir:
    DEFINE VAR Listado AS CHARACTER INITIAL "".

    EMPTY TEMP-TABLE TSCuentas.

    ASSIGN TotActIni = 0
           TotActFin = 0
           TotPasIni = 0
           TotPasFin = 0
           TotPtrIni = 0
           TotPtrFin = 0
           TotResIni = 0
           TotResFin = 0.

    RUN Tabla_Temporal.

    Listado = w_Pathspl + "L-ENTIDA.lst".
    {incluido\IMPRIMIR.I "listado"}.
END PROCEDURE.

PROCEDURE ProcesoImprimir:
    {Incluido\RepEncabezado.i}

    DEFINE VARIABLE W_EstadoInf AS CHARACTER FORMAT "X(8)" INITIAL "".
    DEFINE VARIABLE Nom_Cencosto AS CHARACTER FORMAT "X(2)" INITIAL "".

    W_Reporte = "REPORTE   : BALANCE DE PRUEBA - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(20)") + " al " + STRING(W_Fec1) + " Sacado el: " + STRING(TODAY) + " - " +
                STRING(TIME,"hh:mm am").

    W_EncColumna = "CUENTA         NOMBRE                    NAT   SALDO INICIAL          DEBITO            CREDITO     SALDO FINAL".

    DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".
    DEFINE VAR TT_Ini AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR TT_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR TT_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR TT_Fin AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR TT_Cta AS CHARACTER FORMAT "X(40)".
    DEFINE VAR TotIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    DEFINE VAR TotFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".

    DEFINE FRAME F_Mov TT_Cta AT 1
                       TSCuentas.TS_Nat AT 42
                       TSCuentas.TS_Ini AT 46
                       TSCuentas.TS_Db AT 61
                       TSCuentas.TS_Cr AT 79
                       TSCuentas.TS_Fin AT 97
                       TSCuentas.TS_Nit AT 113
        WITH DOWN WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.

    FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel BREAK BY SUBSTRING(TSCuentas.TS_Cuenta,1,1)
                                                                   BY TSCuentas.TS_Cuenta
                                                                   BY TSCuentas.TS_Nivel
                                                                   BY TSCuentas.TS_Nit:
        IF TSCuentas.TS_IdMov EQ NO AND (TSCuentas.TS_Ini NE 0 OR TSCuentas.TS_Db NE 0 OR TSCuentas.TS_Cr NE 0) THEN DO:
            TT_Cta = STRING(TSCuentas.TS_Cuenta,"X(14)") + " " + TSCuentas.TS_Nombre.

            DISPLAY TT_Cta AT 1
                    TSCuentas.TS_Nat AT 42
                    TSCuentas.TS_Ini AT 46
                    TSCuentas.TS_Db AT 61
                    TSCuentas.TS_Cr AT 79
                    TSCuentas.TS_Fin AT 97
                WITH WIDTH 132 FRAME F_Mov USE-TEXT STREAM-IO NO-LABELS NO-BOX.
        END.
        ELSE DO:
            IF FIRST-OF(TSCuentas.TS_Cuenta) THEN DO:
                TT_Cta = STRING(TSCuentas.TS_Cuenta,"X(14)") + " " + TSCuentas.TS_Nombre.

                DISPLAY TT_Cta AT 1
                    WITH FRAME F_TitCta WIDTH 132 USE-TEXT NO-BOX NO-LABELS.
            END.

            FIND FIRST Clientes WHERE Clientes.Nit EQ TSCuentas.TS_Nit NO-LOCK NO-ERROR.
            IF AVAILABLE Clientes THEN
                TT_Cta = "  " + STRING(Clientes.Nit,"X(12)") + " " + STRING(clientes.agencia,"999") + " " + Clientes.Nombre + " " + Clientes.Apellido1.
            ELSE
                TT_Cta = "  " + STRING(TSCuentas.TS_Nit,"X(12)") + " " + "Nit. No Encontrado      ".

            IF TSCuentas.TS_Db EQ 0 AND TSCuentas.TS_Cr EQ 0 AND TSCuentas.TS_Ini EQ 0 AND TSCuentas.TS_Fin EQ 0 THEN
                NEXT.

            DISPLAY TT_Cta AT 1
                    TSCuentas.TS_Nat AT 42
                    TSCuentas.TS_Ini AT 46
                    TSCuentas.TS_Db AT 61
                    TSCuentas.TS_Cr AT 79
                    TSCuentas.TS_Fin AT 97
                WITH WIDTH 132 FRAME F_Mov1 USE-TEXT STREAM-IO NO-LABELS NO-BOX.

            IF LAST-OF(TSCuentas.TS_Nit) AND TSCuentas.TS_IdMov THEN
                FOR EACH TD_Detalle WHERE TD_Detalle.TD_Cta = TSCuentas.TS_Cuenta
                                      AND TD_Detalle.TD_Nit = TSCuentas.TS_Nit
                                      AND (TD_Detalle.TD_Deb NE 0 OR TD_Detalle.TD_Cre NE 0) BREAK BY TD_Detalle.TD_Nit
                                                                                                   BY TD_Detalle.TD_Fec
                                                                                                   BY TD_Detalle.TD_Cbt
                                                                                                   BY TD_Detalle.TD_Doc:
                    DISPLAY TD_Detalle.TD_Age AT 5 FORMAT "99"
                            TD_Detalle.TD_Com AT 9 FORMAT "X(24)"
                            TD_Detalle.TD_Fec AT 35 FORM "99/99/9999"
                            TD_Detalle.TD_Cbt AT 47 FORM "999"
                            TD_Detalle.TD_Doc AT 52 FORM "9999999"
                            TD_Detalle.TD_Deb AT 60
                            TD_Detalle.TD_Cre AT 78
                        WITH WIDTH 132 FRAME F_Det2 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
                END.

            ASSIGN TT_Ini = TT_Ini + TSCuentas.TS_Ini
                   TT_Db = TT_Db  + TSCuentas.TS_Db
                   TT_CR = TT_Cr  + TSCuentas.TS_Cr
                   TT_Fin = TT_Fin + TSCuentas.TS_Fin.
        END.

        IF LAST-OF(TSCuentas.TS_Cuenta) AND TSCuentas.TS_IdMov THEN DO:
            DISPLAY "     Total de la Cuenta:" AT 1
                    TT_Ini AT 46  
                    TT_Db AT 61 
                    TT_Cr AT 79
                    TT_Fin AT 97 SKIP
                WITH FRAME F_TotCta WIDTH 132 USE-TEXT NO-BOX NO-LABELS.

            ASSIGN TT_Ini = 0
                   TT_Db = 0
                   TT_Cr = 0
                   TT_Fin = 0.
        END.

        IF LAST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,1)) THEN
            DISPLAY SKIP(1).
    END.

    ASSIGN TotIni = TotActIni - (TotPasIni + TotPtrIni) + TotResIni
           TotFin = TotActFin - (TotPasFin + TotPtrFin) + TotResFin.

    PAGE.
OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE Imprimir_Excel:
    MESSAGE "Opción no 1 disponible"
        VIEW-AS ALERT-BOX INFORMATION.
END PROCEDURE.

PROCEDURE Mayorizar13:
    DEFINE VARIABLE SIni LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VARIABLE SDb LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VARIABLE SCr LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VARIABLE SFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR i AS INTEGER.
    DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
    DEFINE VAR MDb LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MCr LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MIni LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR MNt LIKE Cuentas.Naturaleza.

    MCta = anexos.Cuenta.
    W_NitEnc = anexos.Nit.

    RUN Buscar_Cuentas(INPUT anexos.Cuenta).

    IF AVAILABLE Cuentas THEN DO:
        RUN HallarSdo13 (INPUT MONTH(W_Fec1),
                         OUTPUT SIni,
                         OUTPUT SFin).

        FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta EQ anexos.Cuenta
                               AND TSCuentas.TS_Nit EQ W_NitEnc NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(TSCuentas) THEN
            RUN Grabar_Enc_Temporal.

        ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini + SIni
               MINI = TSCuentas.TS_Ini
               TSCuentas.TS_Db = TSCuentas.TS_Db + anexos.Db[MONTH(W_Fec1)]
               MDb = TSCuentas.TS_Db
               TSCuentas.TS_Cr = TSCuentas.TS_Cr + anexos.Cr[MONTH(W_Fec1)]
               MCr = TSCuentas.TS_Cr
               TSCuentas.TS_Fin = TSCuentas.TS_Fin + SFin
               MFin = TSCuentas.TS_Fin
               MNt = TSCuentas.TS_Nat.

        DO i = TSCuentas.TS_Niv TO 1 BY -1:
            IF LENGTH(MCta) GT 2 THEN
                MCta = SUBSTRING(MCta,1,LENGTH(MCta) - 2).
            ELSE
                MCta = SUBSTRING(MCta,1,LENGTH(MCta) - 1).

            RUN Buscar_Cuentas (INPUT MCta).
            IF NOT AVAILABLE(Cuentas) THEN
                NEXT.

            FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta  EQ MCta
                                   AND TSCuentas.TS_Nit EQ "" NO-ERROR.

            W_NitEnc = "".

            IF NOT AVAILABLE(TSCuentas) THEN
                RUN Grabar_Enc_Temporal.

            ASSIGN TSCuentas.TS_Db = TSCuentas.TS_DB + anexos.Db[MONTH(W_Fec1)] /*MDb*/
                   TSCuentas.TS_Cr = TSCuentas.TS_CR + anexos.Cr[MONTH(W_Fec1)]. /*MCr.*/

            IF TSCuentas.TS_Nat NE MNt THEN
                ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini - SIni /*MIni*/
                       TSCuentas.TS_Fin = TSCuentas.TS_Fin - SFin. /*MFin.*/
            ELSE
                ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini + SIni /*MIni*/
                       TSCuentas.TS_Fin = TSCuentas.TS_Fin + SFin. /*MFin.*/
        END.

        FOR EACH Mov_Contable WHERE Mov_Contable.Agencia EQ anexos.Agencia
                                AND Mov_Contable.Cuenta EQ anexos.Cuenta
                                AND Mov_Contable.Nit EQ Anexos.Nit
                                AND Mov_Contable.Fec_Contable GE DATE(STRING(W_Fec1 - DAY(W_Fec1) + 1))
                                AND Mov_Contable.Fec_Contable LE DATE(STRING(W_Fec1)) NO-LOCK:
            FIND FIRST TD_Detalle WHERE TD_Detalle.W_RowIDMov EQ ROWID(Mov_Contable) NO-ERROR.
            IF NOT AVAIL(TD_Detalle) THEN DO:
                CREATE TD_Detalle.
                ASSIGN TD_Detalle.TD_Age = Mov_Contable.Agencia
                       TD_Detalle.TD_Cta = Mov_Contable.Cuenta
                       TD_Detalle.TD_Nit = Mov_Contable.Nit
                       TD_Detalle.TD_Cbt = Mov_Contable.Comprobante
                       TD_Detalle.TD_Fec = Mov_Contable.Fec_Contable
                       TD_Detalle.TD_Com = Mov_Contable.Comentario
                       TD_Detalle.TD_Doc = Mov_Contable.Num_Documento
                       TD_Detalle.TD_Deb = Mov_Contable.Db
                       TD_Detalle.TD_Cre = Mov_Contable.Cr
                       W_RowIDMov = ROWID(Mov_Contable).
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE HallarSdo13:
    DEFINE INPUT PARAMETER Smes AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SFin LIKE Sal_Cuenta.Sal_Inicial.

    DEFINE VAR i AS INTEGER.

    SFin = anexos.Sdo_Inicial.

    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
            ASSIGN SFin = SFin + anexos.DB[i] - anexos.Cr[i]
                   SIni = SFin - anexos.DB[i] + anexos.Cr[i].
        ELSE
            ASSIGN SFin = SFin - anexos.DB[i] + anexos.Cr[i]
                   SIni = SFin + anexos.DB[i] - anexos.Cr[i].
    END.

    CASE Cuentas.Id_Cuenta: /*arma totales iniciales y finales*/
        WHEN 1 THEN
            ASSIGN TotActIni = TotActIni + SIni
                   TotActFin = TotActFin + SFin.

        WHEN 2 THEN
            ASSIGN TotPasIni = TotPasIni + SIni
                   TotPasFin = TotPasFin + SFin.

        WHEN 3 THEN
            ASSIGN TotPtrIni = TotPtrIni + SIni
                   TotPtrFin = TotPtrFin + SFin.

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
