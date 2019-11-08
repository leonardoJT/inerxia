DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Auxiliar".

/*para archivo de excel*/
DEFINE VAR LisEx AS CHARACTER.
DEFINE VAR Ct AS DECIMAL.
DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".

DEFINE TEMP-TABLE IEx
    FIELD NLinea AS INTEGER FORMAT "999999"
    FIELD Linea AS CHARACTER FORMAT "X(150)".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
DEFINE VARIABLE W_CtrNat LIKE Cuentas.Ctr_Naturaleza.
DEFINE VARIABLE L_CC AS LOGICAL INITIAL YES.
DEFINE VARIABLE W_Com AS LOGICAL INITIAL NO.

DEFINE TEMP-TABLE TSCuentas
    FIELD TS_Cuenta AS CHARACTER FORMAT "X(18)"
    FIELD TS_Fecha AS DATE FORMAT "99/99/9999"
    FIELD TS_DB AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
    FIELD TS_CR AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
    FIELD TS_CenCostos AS INTEGER.

/* incluido de Pantalla con parametros */
{incluido/Pantalla_Validacion.i}

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

    IF T_ConsCta NE "" THEN DO:
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

/* fin incluido Pantalla parametros */

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

    SFin = Sal_Cuenta.Sal_Inicial.

    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
            ASSIGN SFin = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i]
                   SIni = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
        ELSE
            ASSIGN SFin = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i]
                   SIni = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
    END.
END PROCEDURE.

PROCEDURE Habilita_Deshabilita:
    /* En este procedimiento se habilitan o deshabilitan las variables a pedir en pantalla segun el informe que se vaya a ejecutar. */
    ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.

    DISABLE Cmb_Comprob
            /*W_Fec2*/
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

    IF NOT L_CC THEN /* valida si la entidad maneja centros de costos */
        DISABLE Cmb_CenCost WITH FRAME F_Valida.
END PROCEDURE.

PROCEDURE Tabla_Temporal:  /* TT */
    IF W_Cuenta1 EQ "" THEN
        W_Cuenta1 = "0".

    IF W_Cuenta2 EQ "" THEN
        W_Cuenta2 = "99999999999999".

    DEFINE VAR W_FecIniMes AS DATE.

    W_FecIniMes = DATE("01/" + STRING(MONTH(W_Fec1)) + "/" + STRING(YEAR(W_Fec1))).

    FOR EACH Mov_Contable WHERE Mov_Contable.Agencia GE W_Ag1
                            AND Mov_Contable.Agencia LE W_Ag2
                            AND Mov_Contable.Cen_Costos GE W_CC1
                            AND Mov_Contable.Cen_Costos LE W_CC2
                            AND Mov_Contable.Cuenta GE W_Cuenta1
                            AND Mov_Contable.Cuenta LE W_Cuenta2
                            AND Mov_Contable.Fec_Contable GE W_FecIniMes /*W_Fec1*/
                            AND Mov_Contable.Fec_Contable LE W_Fec2 NO-LOCK:
        /*MONTH(Mov_Contable.Fec_Contable) EQ MONTH(W_Fec1) AND
        YEAR(Mov_Contable.Fec_Contable) EQ YEAR(W_Fec1)  NO-LOCK:*/
        FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta EQ Mov_Contable.Cuenta
                               AND TSCuentas.TS_CenCostos = mov_contable.cen_costos
                               AND TSCuentas.TS_Fecha EQ Mov_Contable.Fec_Contable NO-ERROR.
        IF NOT AVAILABLE(TSCuentas) THEN DO:
            CREATE TSCuentas.
            ASSIGN TSCuentas.TS_Cuenta = Mov_Contable.Cuenta
                   TSCuentas.TS_CenCostos = mov_contable.cen_costos
                   TSCuentas.TS_Fecha = Mov_Contable.Fec_Contable.
        END.

        ASSIGN TSCuentas.TS_Db = TSCuentas.TS_Db + Mov_Contable.Db
               TSCuentas.TS_Cr = TSCuentas.TS_Cr + Mov_Contable.Cr.
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

    DEFINE VARIABLE W_EstadoInf AS CHARACTER FORMAT "X(8)" INITIAL "".
    DEFINE VARIABLE Nom_Cencosto AS CHARACTER FORMAT "X(2)" INITIAL "".

    W_Reporte   = "REPORTE   : LIBRO AUXILIAR - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(25)") + " - "+ STRING(TIME,"hh:mm am").

    /*                             1         2         3         4         5         6         7         8         9         10
      12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/

    W_EncColumna = "CUENTA-CC          NOMBRE           SALDO INICIAL            DEBITO              CREDITO           SALDO FINAL".

    DEFINE VAR TT_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR TT_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR To_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR To_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR W_Cta AS CHARACTER FORMAT "X(14)".
    DEFINE VAR W_Nom AS CHARACTER FORMAT "X(20)".
    DEFINE VAR SFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR SFTo LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR Sini LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR SAnt LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR TSini LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR TSFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR vCuenta AS CHARACTER FORMAT "X(18)".

    DEFINE FRAME F_Mov
        TSCuentas.TS_Fecha AT 15
        TSCuentas.TS_Db    AT 35
        TSCuentas.TS_Cr    AT 61
        SFin               AT 90
        WITH DOWN FRAME F_Mov WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

    EMPTY TEMP-TABLE IEx.

    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.

    FOR EACH TSCuentas BREAK BY TSCuentas.TS_Cuenta
                             BY TSCuentas.TS_Fecha:
        IF FIRST-OF(TSCuentas.TS_Cuenta) THEN DO:
            RUN Buscar_Cuentas (INPUT TSCuentas.TS_Cuenta).

            W_Nom = Cuentas.Nombre.

            FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia GE W_Ag1
                                  AND Sal_Cuenta.Agencia LE W_Ag2
                                  AND Sal_Cuenta.Cuenta EQ TSCuentas.TS_Cuenta
                                  AND Sal_Cuenta.Ano EQ YEAR(TSCuentas.TS_Fecha) NO-LOCK:
                RUN HallarSdo (INPUT MONTH(W_Fec1),
                               OUTPUT SIni,
                               OUTPUT SFTo).

                ASSIGN TSIni = TSIni + SIni
                       TSFin = TSFin + SFto.
            END.

            /*              IF TSCuentas.TS_Fecha EQ W_Fec1 THEN DO:*/

            vCuenta = TSCuentas.TS_Cuenta.

            IF TSCuentas.TS_CenCostos <> 999 AND TSCuentas.TS_CenCostos <> 0 THEN
                vCuenta = vCuenta + "-" + STRING(TSCuentas.TS_CenCostos,"999").

            DISPLAY /*TSCuentas.TS_Cuenta AT 1*/
                    vCuenta              AT 1
                    W_Nom                AT 20 FORMAT "X(30)"
                    "SALDO: "            AT 89
                    TSIni                AT 98 FORMAT "->>,>>>,>>>,>>9.99"
                WITH WIDTH 132 NO-LABELS NO-BOX USE-TEXT.
                  /*informe excel*/
                  CREATE IEx.
                  ASSIGN Ct = Ct + 1
                         IEx.NLinea = Ct
                         IEx.Linea  = TSCuentas.TS_Cuenta + Cma + W_Nom + Cma + "Saldo: " + STRING(TSIni,"->>>>>>>>>>9.99").
                  CREATE IEx.
                  ASSIGN Ct = Ct + 1
                         IEx.NLinea = Ct
                         IEx.Linea  = "FECHA" + Cma + "DEBITO" + Cma + "CREDITO" + Cma + "SALDO FINAL".
                  SAnt = TSIni.
              /*END.*/
          END.
          IF Cuentas.Naturaleza EQ "DB" THEN
             ASSIGN SFin = SAnt + TSCuentas.TS_Db - TSCuentas.TS_Cr.
          ELSE
             ASSIGN SFin = SAnt - TSCuentas.TS_Db + TSCuentas.TS_Cr.
          IF TSCuentas.TS_Fecha GE W_Fec1 THEN DO:
              DISPLAY
                TSCuentas.TS_Fecha AT 16
                TSCuentas.TS_Db    AT 50 FORMAT "->>,>>>,>>>,>>9.99"
                TSCuentas.TS_Cr    AT 72 FORMAT "->>,>>>,>>>,>>9.99"
                SFin               AT 94 FORMAT "->>,>>>,>>>,>>9.99"
              WITH DOWN FRAME F_Mov WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
              /*informe excel*/
              CREATE IEx.
              ASSIGN Ct = Ct + 1
                     IEx.NLinea = Ct
                     IEx.Linea  = STRING(TSCuentas.TS_Fecha) + Cma  +
                                  STRING(TSCuentas.TS_Db,"->>>>>>>>>>9.99") + Cma + 
                                  STRING(TSCuentas.TS_Cr,"->>>>>>>>>>9.99") + Cma + 
                                  STRING(SFin,"->>>>>>>>>>9.99").
                                  
              ASSIGN TT_Db = TT_Db + TSCuentas.TS_DB
                     To_Db = To_Db + TSCuentas.TS_DB
                     TT_Cr = TT_Cr + TSCuentas.TS_Cr
                     To_Cr = To_Cr + TSCuentas.TS_Cr
                     SAnt  = SFin.
          END.
          ELSE DO:
              IF Cuentas.Naturaleza EQ "DB" THEN
                 ASSIGN TSIni = TSIni + TSCuentas.TS_Db - TSCuentas.TS_Cr.
              ELSE
                 ASSIGN TSIni = TSIni - TSCuentas.TS_Db + TSCuentas.TS_Cr.
              SAnt = TSIni.
          END.
          IF LAST-OF(TSCuentas.TS_Cuenta) THEN DO:
              DISPLAY "---------------------------------------------------------------------------------------" AT 29
                      "Total: " AT 19
                      TSIni   AT 32 FORMAT "->>,>>>,>>>,>>9.99"
                      TT_Db   AT 54 FORMAT "->>,>>>,>>>,>>9.99"
                      TT_Cr   AT 76 FORMAT "->>,>>>,>>>,>>9.99"
                      SFin /*TSFin*/   AT 98 FORMAT "->>,>>>,>>>,>>9.99" 
                      "---------------------------------------------------------------------------------------" AT 29
                      SKIP(1)
              WITH WIDTH 132 FRAME F_Tot NO-LABELS.
              ASSIGN TT_db = 0 TT_Cr = 0 SAnt = 0 SFto = 0 TSIni = 0 TSFin = 0.
          END.
      END.  
      DISPLAY "---------------------------------------------------------------------------------------" AT 29
              "Total Final: " AT 1
              To_Db   AT 54 FORMAT "->>,>>>,>>>,>>9.99"
              To_Cr   AT 76 FORMAT "->>,>>>,>>>,>>9.99"
              "---------------------------------------------------------------------------------------" AT 29
              SKIP(1)
      WITH WIDTH 132 FRAME F_TotFinal NO-LABELS.
      PAGE.
    OUTPUT CLOSE.
    FOR EACH TSCuentas: DELETE TSCuentas. END.
    
  END PROCEDURE.

  PROCEDURE Imprimir_Excel:
    LisEx = w_Pathspl + "Auxiliar.csv".
    OUTPUT TO VALUE(LisEx).
    FOR EACH IEx BY IEx.NLinea:
        PUT IEx.Linea SKIP.
    END.
    OUTPUT CLOSE.
    MESSAGE "Auxiliar para Excel se encuentra en:" SKIP
            LisEx VIEW-AS ALERT-BOX INFORMATION.
    FOR EACH IEx: DELETE IEx. END.
  END PROCEDURE.
                                           
