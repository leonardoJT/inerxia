DEFINE VAR W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Auxiliar".
DEFINE VAR LisEx AS CHARACTER.
DEFINE VAR Ct AS DECIMAL.
DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".

DEFINE TEMP-TABLE IEx
    FIELD NLinea AS INTEGER FORMAT "999999"
    FIELD Linea AS CHARACTER FORMAT "X(150)".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

DEFINE VARIABLE W_Naturaleza AS CHARACTER.
DEFINE VARIABLE W_CtrNat AS LOGICAL.
DEFINE VARIABLE L_CC AS LOGICAL INITIAL YES.
DEFINE VARIABLE vcNomCli AS CHARACTER FORMAT "X(20)" NO-UNDO.

DEFINE TEMP-TABLE TSCuentas
    FIELD TS_Age AS INTEGER FORMAT "999"
    FIELD TS_Cuenta AS CHARACTER FORMAT "X(14)"
    FIELD TS_Nit AS CHARACTER
    FIELD TS_Com AS INTEGER
    FIELD TS_Con AS CHARACTER
    FIELD TS_Doc AS INTEGER
    FIELD TS_Fecha AS DATE FORMAT "99/99/9999"
    FIELD TS_DB AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
    FIELD TS_CR AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
    FIELD TS_Docref AS CHARACTER
    FIELD TS_Centro AS INTEGER
    FIELD TS_hora AS INTEGER
    FIELD TS_fecGrabacion AS DATE.

{incluido/Pantalla_Validacion.i}

PROCEDURE Busca_Cuenta:
    DEFINE INPUT PARAMETER T_ConsCtai AS CHARACTER.
    DEFINE OUTPUT PARAMETER T_ConsCta AS CHARACTER.
    DEFINE OUTPUT PARAMETER T_ConsNom AS CHARACTER.

    IF T_ConsCtai <> "" THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = T_ConsCtai
                             AND Cuentas.Estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(Cuentas) THEN DO:
            T_ConsCta = Cuentas.Cuenta.
            T_ConsNom = Cuentas.Nombre.
        END.
    END.

    IF T_ConsCta <> "" THEN DO:
        RUN C-Cuentas.r (OUTPUT T_ConsCta,
                         OUTPUT T_ConsNom,
                         OUTPUT W_Naturaleza,
                         OUTPUT W_CtrNat,
                         INPUT "T").

        IF T_ConsCta = ? THEN DO:
            FIND FIRST Cuentas WHERE Cuentas.Cuenta = T_ConsCta
                                 AND Cuentas.Estado = 1 NO-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE(Cuentas) THEN DO:
                T_ConsCta = "".
                T_ConsNom = "".
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE Buscar_Cuentas:
    DEFINE INPUT PARAMETER Cta AS CHARACTER.

    FIND FIRST Cuentas WHERE Cuentas.Cuenta = Cta
                         AND Cuentas.Estado = 1 NO-LOCK NO-ERROR.
END PROCEDURE.


PROCEDURE HallarSdo:
    DEFINE INPUT PARAMETER Smes AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini AS DECIMAL.
    DEFINE OUTPUT PARAMETER SFin AS DECIMAL.

    DEFINE VAR i AS INTEGER.

    SFin = Sal_Cuenta.Sal_Inicial.

    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza = "DB" THEN DO:
            SFin = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
            SIni = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
        END.
        ELSE DO:
            SFin = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
            SIni = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
        END.
    END.

END PROCEDURE.


PROCEDURE Habilita_Deshabilita:
    ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.

    DISABLE Cmb_Comprob
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

END PROCEDURE.


PROCEDURE Tabla_Temporal:
    DEFINE VAR vFecha AS DATE.

    IF W_Cuenta1 = "" THEN
        W_Cuenta1 = "0".

    IF W_Cuenta2 = "" THEN
        W_Cuenta2 = "99999999999999".

    DEFINE VAR W_FecIniMes AS DATE.

    W_FecIniMes = DATE("01/" + STRING(MONTH(W_Fec1)) + "/" + STRING(YEAR(W_Fec1))).

    FOR EACH agencias WHERE agencias.agencia >= w_ag1
                        AND agencias.agencia <= w_ag2 NO-LOCK:
        FOR EACH cen_costos WHERE cen_costos.cen_costo >= W_CC1
                              AND cen_costos.cen_costo <= W_CC2 NO-LOCK BREAK BY cen_costos.cen_costos:
            IF FIRST-OF(cen_costos.cen_costo) THEN DO:
                FOR EACH cuentas WHERE cuentas.cuenta >= W_cuenta1
                                   AND cuentas.cuenta <= W_cuenta2
                                   AND cuentas.tipo = 2 NO-LOCK:
                    DO vFecha = W_fecIniMes TO W_Fec2:
                        FOR EACH Mov_Contable WHERE Mov_Contable.Agencia = agencias.agencia
                                                AND Mov_Contable.Cen_Costos = cen_costos.cen_costos
                                                AND Mov_Contable.Cuenta = cuentas.cuenta
                                                AND Mov_Contable.Fec_Contable = vFecha NO-LOCK:
                            CREATE TSCuentas.
                            ASSIGN TSCuentas.TS_Cuenta = Mov_Contable.Cuenta
                                   TSCuentas.TS_Fecha = Mov_Contable.Fec_Contable
                                   TSCuentas.TS_Nit = Mov_Contable.Nit
                                   TSCuentas.TS_Age = Mov_Contable.Agencia
                                   TSCuentas.TS_Con = Mov_Contable.Comentario
                                   TSCuentas.TS_Com = Mov_Contable.Comprobante
                                   TSCuentas.TS_Doc = Mov_Contable.Num_Documento
                                   TSCuentas.TS_Db = TSCuentas.TS_Db + Mov_Contable.Db
                                   TSCuentas.TS_Cr = TSCuentas.TS_Cr + Mov_Contable.Cr
                                   TSCuentas.TS_Docref = Mov_Contable.Doc_Referencia
                                   TSCuentas.TS_Centro = Mov_Contable.Cen_Costos
                                   TSCuentas.TS_hora = mov_contable.hora
                                   TSCuentas.TS_fecGrabacion = mov_contable.fec_grabacion.
                        END.
                    END.
                END.
            END.
        END.
    END.
END PROCEDURE.


PROCEDURE Proceso_Imprimir:
    DEFINE VAR Listado AS CHARACTER INITIAL "".

    EMPTY TEMP-TABLE TSCuentas.

    RUN Tabla_Temporal.

    Listado = w_Pathspl + "BalanceGeneral.lst".

    {incluido\IMPRIMIR.I "listado"}.

END PROCEDURE.


PROCEDURE ProcesoImprimir:
    {Incluido\RepEncabezado.i}

    DEFINE VARIABLE W_EstadoInf AS CHARACTER FORMAT "X(8)".
    DEFINE VARIABLE Nom_Cencosto AS CHARACTER FORMAT "X(2)".

    W_Reporte = "REPORTE   : LIBRO AUXILIAR - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(25)") + " - "+ STRING(TIME,"hh:mm am").
    W_EncColumna = "AGE  NIT           CLIENTE             REF    CONCEPTO              CBT DOCTO     FECHA           DEBITO             CREDITO            SALDO FINAL  CENTRO".

    DEFINE VAR TT_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR TT_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR To_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR To_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR W_Cta AS CHARACTER FORMAT "X(14)".
    DEFINE VAR W_Nom AS CHARACTER FORMAT "X(20)".
    DEFINE VAR SFin AS DECIMAL.
    DEFINE VAR SFTo AS DECIMAL.
    DEFINE VAR Sini AS DECIMAL.
    DEFINE VAR SAnt AS DECIMAL.
    DEFINE VAR TSini AS DECIMAL.
    DEFINE VAR TSFin AS DECIMAL.
    DEFINE VAR Contador AS DECIMAL.

    DEFINE FRAME F_Mov
        TSCuentas.TS_Age    AT 1
        TSCuentas.TS_Nit    AT 5 FORMAT "X(12)"
        vcNomCli            AT 20 FORMAT "X(19)"
        TSCuentas.TS_Docref AT 40 FORMAT "x(4)"
        TSCuentas.TS_Con    AT 45 FORMAT "X(18)"
        TSCuentas.TS_Com    AT 65 FORMAT "999"
        TSCuentas.TS_Doc    AT 69 FORMAT "999999999"
        TSCuentas.TS_Fecha  AT 79
        TSCuentas.TS_Db     AT 89 FORMAT "->>,>>>,>>>,>>9.99"
        TSCuentas.TS_Cr     AT 108 FORMAT "->>,>>>,>>>,>>9.99"
        SFin                AT 128 FORMAT "->>,>>>,>>>,>>9.99"
        TSCuentas.TS_Centro AT 149 FORMAT "9999"
        WITH DOWN FRAME F_Mov WIDTH 160 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS STREAM-IO.

    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.

    EMPTY TEMP-TABLE IEx.

    CREATE IEx.
    ASSIGN Ct = Ct + 1
           IEx.NLinea = Ct
           IEx.Linea  = "AGENCIA" + Cma + "NIT" + Cma + "CUENTA" + Cma + "CONCEPTO" + Cma + "COMPROBANTE" + Cma + "DOCUMENTO" + Cma + "FECHA" + Cma + "DEBITO" + Cma + "CREDITO" + Cma + "SALDO FINAL" + Cma + "CENTRO" .

    FOR EACH TSCuentas BREAK BY TSCuentas.TS_Cuenta
                             BY TSCuentas.TS_Fecha
                             BY TSCuentas.TS_fecGrabacion
                             BY TSCuentas.TS_hora:
        IF FIRST-OF(TSCuentas.TS_Cuenta) THEN DO:
            Contador = 1.
            RUN Buscar_Cuentas (INPUT TSCuentas.TS_Cuenta).
            W_Nom = Cuentas.Nombre.

            FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia >= W_Ag1
                                  AND Sal_Cuenta.Agencia <= W_Ag2
                                  AND Sal_Cuenta.Cuenta = TSCuentas.TS_Cuenta
                                  AND Sal_Cuenta.Ano = YEAR(TSCuentas.TS_Fecha) NO-LOCK:
                RUN HallarSdo(INPUT MONTH(W_Fec1),
                              OUTPUT SIni,
                              OUTPUT SFTo).

                TSIni = TSIni + SIni.
                TSFin = TSFin + SFto.
            END.

            SAnt = TSIni.
        END.

        IF Cuentas.Naturaleza EQ "DB" THEN
            SFin = SAnt + TSCuentas.TS_Db - TSCuentas.TS_Cr.
        ELSE
            SFin = SAnt - TSCuentas.TS_Db + TSCuentas.TS_Cr.

        IF TSCuentas.TS_Fecha >= W_Fec1 THEN DO:
            IF FIRST-OF(TSCuentas.TS_Cuenta) THEN DO:
                DISPLAY TSCuentas.TS_Cuenta AT 1
                        W_Nom               AT 16 FORMAT "X(30)"
                        "SALDO: "           AT 82
                        TSIni              AT 103 FORMAT "->>,>>>,>>>,>>9.99" SKIP(1)
                    WITH WIDTH 160 NO-LABELS NO-BOX USE-TEXT.

                CREATE IEx.
                ASSIGN Ct = Ct + 1
                       IEx.NLinea = Ct
                       IEx.Linea = Cma + Cma + TSCuentas.TS_Cuenta + Cma + W_Nom + Cma + Cma + "Saldo: " + Cma + Cma + Cma + Cma + STRING(TSIni,"->>>>>>>>>>>9.99").
            END.

            FIND FIRST clientes WHERE clientes.nit EQ TSCuentas.TS_Nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN
                vcNomCli = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
            ELSE
                vcNomCli = "".

            DISPLAY TSCuentas.TS_Age    AT 1                                       /*1  */
                    TSCuentas.TS_Nit    AT 5 FORMAT "X(12)"
                    vcNomCli            AT 20 FORMAT "X(19)"
                    TSCuentas.TS_Docref AT 40 FORMAT "x(4)"
                    TSCuentas.TS_Con    AT 45 FORMAT "X(18)"                       /*20 */
                    TSCuentas.TS_Com    AT 65 FORMAT "999"                         /*40 */
                    TSCuentas.TS_Doc    AT 69 FORMAT "999999999"                   /*44 */
                    TSCuentas.TS_Fecha  AT 79                                      /*54 */
                    TSCuentas.TS_Db     AT 89 FORMAT "->>,>>>,>>>,>>9.99"          /*64 */
                    TSCuentas.TS_Cr     AT 108 FORMAT "->>,>>>,>>>,>>9.99"          /*83 */
                    SFin                AT 128 FORMAT "->>,>>>,>>>,>>9.99"         /*103*/
                    TSCuentas.TS_Centro AT 149 FORMAT "9999"
                WITH DOWN FRAME F_Mov WIDTH 160 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS STREAM-IO.

            /*informe excel*/
            CREATE IEx.
            ASSIGN Ct = Ct + 1
                   IEx.NLinea = Ct
                   IEx.Linea = STRING(TSCuentas.TS_Age) + Cma + STRING(TSCuentas.TS_Nit) + Cma + /***/ STRING(TSCuentas.TS_Cuenta) + Cma + STRING(TSCuentas.TS_Con) + Cma +
                               STRING(TSCuentas.TS_Com,"9999") + Cma + STRING(TSCuentas.TS_Doc,"9999999999") + Cma + STRING(TSCuentas.TS_Fecha) + Cma +
                               STRING(TSCuentas.TS_Db,"->>>>>>>>>>9.99") + Cma + STRING(TSCuentas.TS_Cr,"->>>>>>>>>>9.99") + Cma + STRING(SFin,"->>>>>>>>>>9.99") + Cma +
                               STRING(TSCuentas.TS_Centro,"9999").

            ASSIGN TT_Db = TT_Db + TSCuentas.TS_DB
                   To_Db = To_Db + TSCuentas.TS_DB
                   TT_Cr = TT_Cr + TSCuentas.TS_Cr
                   To_Cr = To_Cr + TSCuentas.TS_Cr
                   SAnt = SFin.
        END.
        ELSE DO:
            IF Cuentas.Naturaleza EQ "DB" THEN
                TSIni = TSIni + TSCuentas.TS_Db - TSCuentas.TS_Cr.
            ELSE
                TSIni = TSIni - TSCuentas.TS_Db + TSCuentas.TS_Cr.

            SAnt = TSIni.
        END.

        IF LAST-OF(TSCuentas.TS_Cuenta) THEN DO:
            DISPLAY "---------------------------------------------------------------------------------------" AT 25
                    "Total: " AT 15                                           /* 15  */
                    TSIni     AT 67 FORMAT "->>,>>>,>>>,>>9.99"               /* 42  */
                    TT_Db     AT 89 FORMAT "->>,>>>,>>>,>>9.99"               /* 64  */
                    TT_Cr     AT 108 FORMAT "->>,>>>,>>>,>>9.99"               /* 83  */
                    SFin /*TSFin*/   AT 150 FORMAT "->>,>>>,>>>,>>9.99"
                    "---------------------------------------------------------------------------------------" AT 25 SKIP(1)
                WITH WIDTH 180 FRAME F_Tot NO-LABELS NO-BOX USE-TEXT STREAM-IO.

            ASSIGN TT_db = 0
                   TT_Cr = 0
                   SAnt = 0
                   SFto = 0
                   TSIni = 0
                   TSFin = 0.
        END.
    END.

    DISPLAY "---------------------------------------------------------------------------------------" AT 25
            "Total Final: " AT 1
            To_Db   AT 75 FORMAT "->>,>>>,>>>,>>9.99"         /* 50  */
            To_Cr   AT 97 FORMAT "->>,>>>,>>>,>>9.99"         /* 72  */
            "---------------------------------------------------------------------------------------" AT 25 SKIP(1)
        WITH WIDTH 160 FRAME F_TotFinal NO-LABELS.

    PAGE.
    OUTPUT CLOSE.

    EMPTY TEMP-TABLE TSCuentas.
END PROCEDURE.


PROCEDURE Imprimir_Excel:
    LisEx = w_Pathspl + "AuxiliarDet.csv".

    OUTPUT TO VALUE(LisEx).
    FOR EACH IEx BY IEx.NLinea:
        PUT IEx.Linea SKIP.
    END.
    OUTPUT CLOSE.

    MESSAGE "Auxiliar Detallado para Excel se encuentra en:" SKIP
            LisEx
        VIEW-AS ALERT-BOX INFORMATION.

    EMPTY TEMP-TABLE IEx.
END PROCEDURE.
