DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Balance de Anexos" NO-UNDO.
DEFINE VAR LisEx AS CHARACTER NO-UNDO.
DEFINE VAR Ct AS DECIMAL NO-UNDO.
DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";" NO-UNDO.

DEFINE TEMP-TABLE IEx NO-UNDO
    FIELD NLinea AS INTEGER FORMAT "999999"
    FIELD Linea  AS CHARACTER FORMAT "X(170)".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

DEFINE VAR W_Naturaleza AS CHARACTER NO-UNDO.
DEFINE VAR W_CtrNat AS LOGICAL NO-UNDO.
DEFINE VAR L_CC AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VAR W_NitEnc AS CHARACTER NO-UNDO.
DEFINE VAR TotDeb AS DECIMAL FORMAT "->>>,>>>,>>>,>>9" NO-UNDO.
DEFINE VAR TotCre AS DECIMAL FORMAT "->>>,>>>,>>>,>>9" NO-UNDO.
DEFINE VAR TotActIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR TotPasIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR TotPtrIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR TotResIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR TotActFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR TotPasFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR TotPtrFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR TotResFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.

DEFINE TEMP-TABLE TSCuentas NO-UNDO
    FIELD TS_Cuenta AS CHARACTER FORMAT "X(14)"
    FIELD TS_Nit AS CHARACTER FORMAT "X(14)"
    FIELD TS_Nombre AS CHARACTER FORMAT "X(25)"
    FIELD TS_Nivel LIKE Cuentas.Nivel
    FIELD TS_Nat LIKE Cuentas.Naturaleza
    FIELD TS_Ini AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD TS_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD TS_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD TS_Fin AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD ts_niv1 AS INTEGER
    INDEX cuentanit ts_cuenta ts_nit
    INDEX idx1 ts_cuenta
    INDEX idx2 ts_nivel
    INDEX idx3 ts_niv1
    INDEX idx4 ts_cuenta ts_nivel.

DEFINE TEMP-TABLE cuentasProducto
    FIELD tipo_producto AS INTEGER
    FIELD cod_producto AS INTEGER
    FIELD cuenta AS CHARACTER.

FOR EACH cortoLargo NO-LOCK:
    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = cortoLargo.cta_AsoAd NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = cortolargo.clase_producto.
        cuentasProducto.cod_producto = cortoLargo.cod_producto.
        cuentasProducto.cuenta = cortoLargo.cta_AsoAd.
    END.
END.

FOR EACH liqui_int NO-LOCK:
    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = liqui_int.cta_cauCr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = liqui_int.clase_producto.
        cuentasProducto.cod_producto = liqui_int.cod_producto.
        cuentasProducto.cuenta = liqui_int.cta_cauCr.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = liqui_int.ctaCr_LiqAso NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = liqui_int.clase_producto.
        cuentasProducto.cod_producto = liqui_int.cod_producto.
        cuentasProducto.cuenta = liqui_int.ctaCr_LiqAso.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = liqui_int.ctaDb_DifCobAso NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = liqui_int.clase_producto.
        cuentasProducto.cod_producto = liqui_int.cod_producto.
        cuentasProducto.cuenta = liqui_int.ctaDb_DifCobAso.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = liqui_int.ctaDb_MoraAso NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = liqui_int.clase_producto.
        cuentasProducto.cod_producto = liqui_int.cod_producto.
        cuentasProducto.cuenta = liqui_int.ctaDb_MoraAso.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = liqui_int.ctaDb_Mora NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = liqui_int.clase_producto.
        cuentasProducto.cod_producto = liqui_int.cod_producto.
        cuentasProducto.cuenta = liqui_int.ctaDb_Mora.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = liqui_int.ctaDb_LiqAso NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = liqui_int.clase_producto.
        cuentasProducto.cod_producto = liqui_int.cod_producto.
        cuentasProducto.cuenta = liqui_int.ctaDb_LiqAso.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = liqui_int.ctaInt_AntAso NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = liqui_int.clase_producto.
        cuentasProducto.cod_producto = liqui_int.cod_producto.
        cuentasProducto.cuenta = liqui_int.ctaInt_AntAso.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = liqui_int.ctaCr_DifCobAso NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = liqui_int.clase_producto.
        cuentasProducto.cod_producto = liqui_int.cod_producto.
        cuentasProducto.cuenta = liqui_int.ctaCr_DifCobAso.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = liqui_int.ctaCr_MoraAso NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = liqui_int.clase_producto.
        cuentasProducto.cod_producto = liqui_int.cod_producto.
        cuentasProducto.cuenta = liqui_int.ctaCr_MoraAso.
    END.
END.

FOR EACH carteraVencida NO-LOCK:
    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = carteraVencida.cta_AsoAddb NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = 2.
        cuentasProducto.cod_producto = carteraVencida.cod_producto.
        cuentasProducto.cuenta = carteraVencida.cta_AsoAddb.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = carteraVencida.cta_NoaAddb NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = 2.
        cuentasProducto.cod_producto = carteraVencida.cod_producto.
        cuentasProducto.cuenta = carteraVencida.cta_NoaAddb.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = carteraVencida.ctaCal_interes NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = 2.
        cuentasProducto.cod_producto = carteraVencida.cod_producto.
        cuentasProducto.cuenta = carteraVencida.ctaCal_interes.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = carteraVencida.cta_AsoPrvAdCr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = 2.
        cuentasProducto.cod_producto = carteraVencida.cod_producto.
        cuentasProducto.cuenta = carteraVencida.cta_AsoPrvAdCr.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = carteraVencida.cta_AsoIntAdCr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = 2.
        cuentasProducto.cod_producto = carteraVencida.cod_producto.
        cuentasProducto.cuenta = carteraVencida.cta_AsoIntAdCr.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = carteraVencida.cta_AsoIntNaCr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = 2.
        cuentasProducto.cod_producto = carteraVencida.cod_producto.
        cuentasProducto.cuenta = carteraVencida.cta_AsoIntNaCr.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = carteraVencida.cta_NoaIntAdCr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = 2.
        cuentasProducto.cod_producto = carteraVencida.cod_producto.
        cuentasProducto.cuenta = carteraVencida.cta_NoaIntAdCr.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = carteraVencida.cta_NoaIntNaCr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = 2.
        cuentasProducto.cod_producto = carteraVencida.cod_producto.
        cuentasProducto.cuenta = carteraVencida.cta_NoaIntNaCr.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = carteraVencida.cta_AsoNaDb NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = 2.
        cuentasProducto.cod_producto = carteraVencida.cod_producto.
        cuentasProducto.cuenta = carteraVencida.cta_AsoNaDb.
    END.

    FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = carteraVencida.cta_NoaNaDb NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasProducto THEN DO:
        CREATE cuentasProducto.
        cuentasProducto.tipo_producto = 2.
        cuentasProducto.cod_producto = carteraVencida.cod_producto.
        cuentasProducto.cuenta = carteraVencida.cta_NoaNaDb.
    END.
END.


/* oakley */

/* incluido de Pantalla con parametros*/
{incluido/Pantalla_Validacion.i}
/* fin incluido Pantalla parametros */

PROCEDURE Busca_Cuenta:
DEFINE INPUT PARAMETER T_ConsCtai AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER T_ConsCta AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER T_ConsNom AS CHARACTER NO-UNDO.

IF T_ConsCtai <> "" THEN DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta = T_ConsCtai
                         AND Cuentas.Estado = 1 NO-LOCK NO-ERROR.
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
                                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE(Cuentas) THEN
                ASSIGN T_ConsCta = ""
                       T_ConsNom = "".
        END.
    END.
END PROCEDURE.

PROCEDURE Mayorizar:
    DEFINE VARIABLE SIni LIKE Sal_Cuenta.Sal_Inicial NO-UNDO.
    DEFINE VARIABLE SDb LIKE Sal_Cuenta.Sal_Inicial NO-UNDO.
    DEFINE VARIABLE SCr LIKE Sal_Cuenta.Sal_Inicial NO-UNDO.
    DEFINE VARIABLE SFin LIKE Sal_Cuenta.Sal_Inicial NO-UNDO.
    DEFINE VAR i AS INTEGER NO-UNDO.
    DEFINE VAR MCta AS CHARACTER FORMAT "X(14)" NO-UNDO.
    DEFINE VAR MDb LIKE Sal_Cuenta.Sal_Inicial NO-UNDO.
    DEFINE VAR MCr LIKE Sal_Cuenta.Sal_Inicial NO-UNDO.
    DEFINE VAR MIni LIKE Sal_Cuenta.Sal_Inicial NO-UNDO.
    DEFINE VAR MFin LIKE Sal_Cuenta.Sal_Inicial NO-UNDO.
    DEFINE VAR MNt LIKE Cuentas.Naturaleza NO-UNDO.

    MCta = Anexos.Cuenta.
    W_NitEnc = Anexos.Nit.

    RUN HallarSdo (INPUT MONTH(W_Fec1),
                   OUTPUT SIni,
                   OUTPUT SFin).

    FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta EQ Cuentas.Cuenta
                           AND TSCuentas.TS_Nit EQ W_NitEnc NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(TSCuentas) THEN
        RUN Grabar_Enc_Temporal.

    ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini + SIni
           MINI = TSCuentas.TS_Ini
           TSCuentas.TS_Db = TSCuentas.TS_Db + Anexos.Db[MONTH(W_Fec1)]
           MDb = TSCuentas.TS_Db
           TSCuentas.TS_Cr = TSCuentas.TS_Cr + Anexos.Cr[MONTH(W_Fec1)]
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

        FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta EQ MCta
                               AND TSCuentas.TS_Nit EQ "" NO-ERROR.

        W_NitEnc = "".

        IF NOT AVAILABLE(TSCuentas) THEN
            RUN Grabar_Enc_Temporal.

        ASSIGN TSCuentas.TS_Db = TSCuentas.TS_DB + Anexos.Db[MONTH(W_Fec1)] /*MDb*/
               TSCuentas.TS_Cr = TSCuentas.TS_CR + Anexos.Cr[MONTH(W_Fec1)]. /*MCr.*/

        IF Cuentas.Naturaleza NE MNt THEN
            ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini - SIni /*MIni*/
                   TSCuentas.TS_Fin = TSCuentas.TS_Fin - SFin. /*MFin.*/
        ELSE
            ASSIGN TSCuentas.TS_Ini = TSCuentas.TS_Ini + SIni /*MIni*/
                   TSCuentas.TS_Fin = TSCuentas.TS_Fin + SFin. /*MFin.*/
    END.
END PROCEDURE.

PROCEDURE Grabar_Enc_Temporal:
    CREATE TSCuentas.
    ASSIGN TSCuentas.TS_Cuenta = Cuentas.Cuenta
           TSCuentas.TS_Nit = W_NitEnc
           TSCuentas.TS_Nombre = Cuentas.Nombre
           TSCuentas.TS_Nivel = Cuentas.Nivel
           TSCuentas.TS_Nat = Cuentas.Naturaleza
           TSCuentas.ts_niv1 = integer(substring(TSCuentas.TS_Cuenta,1,1)).
END PROCEDURE.

PROCEDURE Buscar_Cuentas:
    DEFINE INPUT PARAMETER Cta LIKE Cuentas.Cuenta NO-UNDO.

    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cta
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE HallarSdo:
    DEFINE INPUT PARAMETER Smes AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER Sini LIKE Sal_Cuenta.Sal_Inicial NO-UNDO.
    DEFINE OUTPUT PARAMETER SFin LIKE Sal_Cuenta.Sal_Inicial NO-UNDO.

    DEFINE VAR i AS INTEGER NO-UNDO.

    SFin = Anexos.Sdo_Inicial.

    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
            ASSIGN SFin = SFin + Anexos.DB[i] - Anexos.Cr[i]
                   SIni = SFin - Anexos.DB[i] + Anexos.Cr[i].
        ELSE
            ASSIGN SFin = SFin - Anexos.DB[i] + Anexos.Cr[i]
                   SIni = SFin + Anexos.DB[i] - Anexos.Cr[i].

        ASSIGN TotDeb = TotDeb + ROUND(Anexos.DB[i],0)
               TotCre = TotCre + ROUND(Anexos.CR[i],0).
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
            W_Fec2
            W_NomUsuario1
            W_NomUsuario2
            W_NomCuenta1
            W_NomCuenta2
            W_NomNit1
            W_NomNit2
            W_Base
            W_Porcentaje
        WITH FRAME F_Valida.

    ASSIGN W_Fec1:LABEL IN FRAME F_Valida = "Fecha de Corte"
           W_Fec2:LABEL IN FRAME F_Valida = "Fecha del Día".

    IF NOT L_CC THEN
        DISABLE Cmb_CenCost WITH FRAME F_Valida.
END PROCEDURE.

PROCEDURE Tabla_Temporal:
    DEFINE VARIABLE Wano AS INTEGER.

    Wano = year(W_Fec1).

    FOR EACH agencias WHERE Agencias.Agencia >= W_Ag1
                        AND Agencias.agencia <= W_Ag2 NO-LOCK:
        FOR EACH Anexos WHERE Anexos.Agencia = agencias.agencia
                          AND Anexos.Ano = Wano NO-LOCK:
            IF Anexos.Cen_Costos >= W_CC1 AND
               Anexos.Cen_Costos <= W_CC2 AND
               Anexos.cuenta >= W_Cuenta1 AND
               Anexos.cuenta <= W_Cuenta2 AND
               ((anexos.Nit >= W_Nit1 AND anexos.Nit <= W_Nit2) OR
                ((INDEX(anexos.nit,W_Nit1) = 1 AND LENGTH(W_Nit1) = LENGTH(anexos.nit) - 2) OR (INDEX(anexos.nit,W_Nit2) = 1 AND LENGTH(W_Nit2) = LENGTH(anexos.nit) - 2)) OR
                ((INDEX(W_Nit1,anexos.nit) = 1 AND LENGTH(anexos.nit) = LENGTH(W_Nit1) - 2) OR (INDEX(W_Nit2,anexos.nit) = 1 AND LENGTH(anexos.nit) = LENGTH(W_Nit2) - 2))) THEN DO:
                RUN Buscar_Cuentas (INPUT Anexos.Cuenta).
                IF NOT AVAILABLE cuentas THEN
                    NEXT.

                FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
                IF AVAILABLE cuentasProducto THEN
                    NEXT.

                IF cuentas.id_nit THEN
                    RUN Mayorizar.       /* Solo procesa las cuenta que manejan nit - abril 25/2006 w+c*/
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE Proceso_Imprimir:
    DEFINE VAR Listado AS CHARACTER INITIAL "" NO-UNDO.

    EMPTY TEMP-TABLE tscuentas.

    ASSIGN TotCre = 0
           TotDeb = 0
           TotActIni = 0
           TotActFin = 0
           TotPasIni = 0
           TotPasFin = 0
           TotPtrIni = 0
           TotPtrFin = 0
           TotResIni = 0
           TotResFin = 0.

    FOR EACH cuentas WHERE cuentas.cuenta >= W_Cuenta1
                       AND cuentas.cuenta <= W_Cuenta2
                       AND cuentas.estado = 1 NO-LOCK:
        FIND FIRST cuentasProducto WHERE cuentasProducto.cuenta = cuentas.cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE cuentasProducto THEN
            MESSAGE "La cuenta" cuentas.cuenta cuentas.nombre "está configurada" SKIP
                    "como cuenta de producto. No se mostrarán saldos de anexos." SKIP
                    "Para consultar esta información diríjase a Informes Básicos." SKIP
                    cuentasProducto.tipo_producto cuentasProducto.cod_producto cuentasProducto.cuenta
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    RUN Tabla_Temporal.

    Listado = w_Pathspl + "L-ENTIDA.lst".
    {incluido\IMPRIMIR.I "listado"}.
END PROCEDURE.

PROCEDURE ProcesoImprimir:
    {Incluido\RepEncabezado.i}

    DEFINE VARIABLE W_EstadoInf AS CHARACTER FORMAT "X(8)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE Nom_Cencosto AS CHARACTER FORMAT "X(2)" INITIAL "" NO-UNDO.

    W_Reporte = "REPORTE   : BALANCE DE PRUEBA - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(20)") + " al " + STRING(W_Fec1) + " Sacado el: " + STRING(TODAY) + " - " +
                STRING(TIME,"hh:mm am").

    W_EncColumna = "CUENTA         NOMBRE                    NAT   SALDO INICIAL      DEBITO            CREDITO     SALDO FINAL".

    DEFINE VAR CtaAnt AS CHARACTER FORMAT "X" NO-UNDO.
    DEFINE VAR TT_Ini AS DECIMAL FORMAT "->>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR TT_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR TT_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR TT_Fin AS DECIMAL FORMAT "->>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR TT_Cta AS CHARACTER FORMAT "X(40)" NO-UNDO.
    DEFINE VAR TotIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR TotFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9" NO-UNDO.

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

    EMPTY TEMP-TABLE IEx.
    
    CREATE IEx.
    ASSIGN Ct = Ct + 1
           IEx.NLinea = Ct
           IEx.Linea = "CUENTA" + Cma + "NOM.CUENTA" + Cma + "NIT" + Cma + "NOMBRE" + Cma + "NAT" + Cma + "INICIAL" + Cma + "DEBITO" + Cma + "CREDITO" + Cma + "FINAL".

    FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel
                         AND (TSCuentas.TS_Ini <> 0 OR
                              TSCuentas.TS_Db <> 0 OR
                              TSCuentas.TS_Cr <> 0 OR
                              TSCuentas.TS_Fin <> 0) BREAK BY TSCuentas.ts_Niv1
                                                           BY TSCuentas.TS_Cuenta
                                                           BY TSCuentas.TS_Nivel:
        IF TSCuentas.TS_Nit EQ "" THEN DO:
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

                FIND FIRST Clientes WHERE Clientes.Nit EQ TSCuentas.TS_Nit NO-LOCK NO-ERROR.

                TT_Cta = "   Clientes Inconsistente".

                IF AVAILABLE Clientes THEN
                    TT_Cta = "   " + STRING(Clientes.Nit,"X(14)") + " " + Clientes.Nombre + " " + Clientes.Apellido1.
                
                DISPLAY TT_Cta AT 1
                        TSCuentas.TS_Nat AT 42
                        TSCuentas.TS_Ini AT 46
                        TSCuentas.TS_Db AT 61
                        TSCuentas.TS_Cr AT 79
                        TSCuentas.TS_Fin AT 97
                    WITH WIDTH 132 FRAME F_Mov1 USE-TEXT STREAM-IO NO-LABELS NO-BOX.

                CREATE IEx.
                ASSIGN Ct = Ct + 1
                       IEx.NLinea = Ct
                       IEx.Linea = STRING(TSCuentas.TS_Cuenta) + Cma + STRING(TSCuentas.TS_Nombre) + Cma + STRING(Clientes.Nit,"X(14)") + Cma +
                                   STRING((Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2),"X(45)") + Cma + STRING(TSCuentas.TS_Nat) + Cma +
                                   STRING(TSCuentas.TS_Ini,"->>>>>>>>>>>9.99") + Cma + STRING(TSCuentas.TS_Db,"->>>>>>>>>>>9.99")  + Cma + STRING(TSCuentas.TS_Cr,"->>>>>>>>>>>9.99") + Cma +
                                   STRING(TSCuentas.TS_Fin,"->>>>>>>>>>>9.99").
            END.
            ELSE DO:
                IF TSCuentas.TS_Nit NE "" THEN DO:
                    FIND FIRST Clientes WHERE Clientes.Nit EQ TSCuentas.TS_Nit NO-LOCK NO-ERROR.

                    TT_Cta = "   Clientes Inconsistente".

                    IF AVAILABLE Clientes THEN
                        TT_Cta = "   " + STRING(Clientes.Nit,"X(14)") + " " + Clientes.Nombre + " " + Clientes.Apellido1.

                    DISPLAY TT_Cta AT 1
                            TSCuentas.TS_Nat AT 42
                            TSCuentas.TS_Ini AT 46
                            TSCuentas.TS_Db AT 61 
                            TSCuentas.TS_Cr AT 79
                            TSCuentas.TS_Fin AT 97
                        WITH WIDTH 132 FRAME F_Mov2 USE-TEXT STREAM-IO NO-LABELS NO-BOX.

                    CREATE IEx.
                    ASSIGN Ct = Ct + 1
                           IEx.NLinea = Ct
                           IEx.Linea = STRING(TSCuentas.TS_Cuenta) + Cma + STRING(TSCuentas.TS_Nombre) + Cma + STRING(Clientes.Nit,"X(14)") + Cma +
                                       STRING((Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2),"X(45)") + Cma + STRING(TSCuentas.TS_Nat) + Cma +
                                       STRING(TSCuentas.TS_Ini,"->>>>>>>>>>>9.99") + Cma + STRING(TSCuentas.TS_Db,"->>>>>>>>>>>9.99")  + Cma + STRING(TSCuentas.TS_Cr,"->>>>>>>>>>>9.99") +
                                       Cma + STRING(TSCuentas.TS_Fin,"->>>>>>>>>>>9.99").
                END.
            END.
        END.

        IF LAST-OF(ts_niv1) THEN
            DISPLAY SKIP(1).
    END.

    ASSIGN TotIni = TotActIni - (TotPasIni + TotPtrIni) + TotResIni
           TotFin = TotActFin - (TotPasFin + TotPtrFin) + TotResFin.

    DISPLAY "Totales de Control: " AT 1
            TotIni AT 43
            TotDeb AT 60
            TotCre AT 78
            TotFin AT 97
        WITH FRAME T_Tot WIDTH 132 NO-LABELS STREAM-IO USE-TEXT NO-BOX.

    PAGE.
OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE Imprimir_Excel:

    LisEx = w_Pathspl + "SaldosAnexos_" + STRING(TIME,"99999") + "_" + w_usuario + ".csv".

    OUTPUT TO VALUE(LisEx).
        FOR EACH IEx BY IEx.NLinea:
            IEx.Linea = REPLACE(IEx.Linea,".",",").

            PUT IEx.Linea SKIP.
        END.
    OUTPUT CLOSE.

    MESSAGE "Saldos de Anexos para Excel se encuentra en:" SKIP
            LisEx
        VIEW-AS ALERT-BOX INFORMATION.

    EMPTY TEMP-TABLE IEx.
END PROCEDURE.
