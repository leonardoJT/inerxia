  DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Auxiliar".
  DEFINE VAR W_FecIniMes AS DATE.
  /*para archivo de excel*/
  DEFINE VAR LisEx AS CHARACTER.
  DEFINE VAR Ct AS DECIMAL.
  DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".
  DEFINE TEMP-TABLE IEx
      FIELD NLinea AS INTEGER FORMAT "999999"
      FIELD Linea  AS CHARACTER FORMAT "X(150)".
  /**/
{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.
  DEFINE VARIABLE W_Com        AS LOGICAL INITIAL NO.

  DEFINE TEMP-TABLE TSCuentas
      FIELD TS_Cuenta      AS CHARACTER FORMAT "X(14)"
      FIELD TS_Fecha       AS DATE FORMAT "99/99/9999"
      FIELD TS_DB          AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
      FIELD TS_CR          AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99".

  DEFINE TEMP-TABLE TBrecha
      FIELD Tip     AS INTEGER FORMAT "9"
      FIELD Cod     AS INTEGER FORMAT "999"
      FIELD Cta     AS CHARACTER FORMAT "X(4)"
      FIELD SFecha  AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9"
      FIELD Meses   AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9" EXTENT 12
      FIELD Mes12   AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9".
/* incluido de Pantalla con parametros */
{incluido/Pantalla_Validacion.i}

  PROCEDURE Busca_Cuenta:
    DEFINE INPUT  PARAMETER T_ConsCtai LIKE Cuentas.Cuenta. 
    DEFINE OUTPUT PARAMETER T_ConsCta  LIKE Cuentas.Cuenta.
    DEFINE OUTPUT PARAMETER T_ConsNom  LIKE Cuentas.Nombre.
    IF T_ConsCtai NE "" THEN DO:
       FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCtai AND
                          Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
       IF AVAILABLE(Cuentas) THEN
          ASSIGN T_ConsCta = Cuentas.Cuenta
                 T_ConsNom = Cuentas.Nombre.
    END.
    IF T_ConsCta NE "" THEN DO:
       RUN C-Cuentas.r (OUTPUT T_ConsCta, OUTPUT T_ConsNom, OUTPUT W_Naturaleza, OUTPUT W_CtrNat, INPUT "T").
       IF T_ConsCta EQ ? THEN DO:
          FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
          IF NOT AVAILABLE(Cuentas) THEN
            ASSIGN T_ConsCta  = ""
                   T_ConsNom  = "".           
       END.
    END.
  END PROCEDURE.

/* fin incluido Pantalla parametros */

  PROCEDURE Buscar_Cuentas:
      DEFINE INPUT PARAMETER Cta LIKE Cuentas.Cuenta.
      FIND Cuentas WHERE Cuentas.Cuenta EQ Cta AND 
                         Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  END PROCEDURE.

  PROCEDURE HallarSdo:
    DEFINE INPUT  PARAMETER Smes AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR i AS INTEGER.
    SFin = Sal_Cuenta.Sal_Inicial.
    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
           ASSIGN SFin  = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i]
                  SIni  = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
        ELSE
           ASSIGN SFin  = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i]
                  SIni  = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
    END.
    
  END PROCEDURE.

  PROCEDURE Habilita_Deshabilita:
  /* En este procedimiento se habilitan o deshabilitan las variables
     a pedir en pantalla segun el informe que se vaya a ejecutar. */
      ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.
      DISABLE Cmb_Comprob 
/*              W_Fec2*/
              W_NomUsuario1  W_NomUsuario2          
              W_NomCuenta1   W_NomCuenta2         
              W_Nit1         W_NomNit1            
              W_Nit2         W_NomNit2            
              W_Base         W_Porcentaje WITH FRAME F_Valida.
      IF NOT L_CC THEN /* valida si la entidad maneja centros de costos */
         DISABLE Cmb_CenCost WITH FRAME F_Valida.
  END PROCEDURE.

  PROCEDURE Subir_Tabla_Brecha:
    INPUT FROM VALUE("c:\info_cooprudea\brecha.csv").
     REPEAT:
       CREATE TBrecha.
       IMPORT DELIMITER ";" TBrecha.
     END.
     INPUT CLOSE. 


  END PROCEDURE.

  PROCEDURE Tabla_Temporal:  /* TT */
      IF W_Cuenta1 EQ "" THEN W_Cuenta1 = "0".
      IF W_Cuenta2 EQ "" THEN W_Cuenta2 = "99999999999999".
      W_FecIniMes = DATE("01/" + STRING(MONTH(W_Fec1)) + "/" + STRING(YEAR(W_Fec1))).
      FOR EACH TBrecha: DELETE TBrecha. END.
      RUN Subir_Tabla_Brecha.
      FIND FIRST TBrecha NO-ERROR.
      FOR EACH TBrecha WHERE TBrecha.Cta NE "":
          FOR EACH Mov_Contable WHERE
                   Mov_Contable.Agencia       GE W_Ag1     AND
                   Mov_Contable.Agencia       LE W_Ag2     AND
                   Mov_Contable.Cen_Costos    GE W_CC1     AND
                   Mov_Contable.Cen_Costos    LE W_CC2     AND
                   Mov_Contable.Cuenta        BEGINS TBrecha.Cta AND
                  /* Mov_Contable.Cuenta        GE TBrecha.Cta AND
                   Mov_Contable.Cuenta        LE TBrecha.Cta AND*/
                   Mov_Contable.Fec_Contable  GE W_FecIniMes AND
                   Mov_Contable.Fec_Contable  LE W_Fec2 NO-LOCK:
             FIND TSCuentas WHERE TSCuentas.TS_Cuenta EQ Mov_Contable.Cuenta AND
                                  TSCuentas.TS_Fecha  EQ Mov_Contable.Fec_Contable NO-ERROR.
             IF NOT AVAILABLE(TSCuentas) THEN DO:
                 CREATE TSCuentas.
                 ASSIGN TSCuentas.TS_Cuenta = Mov_Contable.Cuenta
                        TSCuentas.TS_Fecha  = Mov_Contable.Fec_Contable.
             END.
             ASSIGN TSCuentas.TS_Db = TSCuentas.TS_Db + Mov_Contable.Db
                    TSCuentas.TS_Cr = TSCuentas.TS_Cr + Mov_Contable.Cr.
          END.
      END.
  END PROCEDURE.

  PROCEDURE Proceso_Imprimir:
      DEFINE VAR Listado AS CHARACTER INITIAL "".
      FOR EACH TSCuentas: DELETE TSCuentas. END.
      RUN Tabla_Temporal.                        
      Listado = w_Pathspl + "BalanceGeneral.lst".
     {incluido\IMPRIMIR.I "listado"}.
  END PROCEDURE.

  PROCEDURE ProcesoImprimir:
      {Incluido\RepEncabezado.i}
      DEFINE VAR  TT_Db  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  To_Db  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  To_Cr  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR Tri2  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR Tri3  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR Tri4  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      
      DEFINE VAR  W_Cta AS CHARACTER FORMAT "X(14)".
      DEFINE VAR  W_Nom AS CHARACTER FORMAT "X(20)".
      DEFINE VAR  SFin LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR  SFTo LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR  Sini  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR  SAnt  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR  TSini LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR  TSFin LIKE Sal_Cuenta.Sal_Inicial.

      W_Reporte   = "REPORTE   : BRECHA DE LIQUIDEZ - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(25)")
                    + " - "+ STRING(TIME,"hh:mm am").
      W_EncColumna = "CUENTA            Mes 1              Mes 2              Mes 3        Trimestre 2        Trimestre 3        Trimestre 4             Mes 12".
      W_FecIniMes = DATE("01/" + STRING(MONTH(W_Fec1)) + "/" + STRING(YEAR(W_Fec1))).
      FOR EACH TSCuentas BREAK BY TSCuentas.TS_Cuenta BY TSCuentas.TS_Fecha:
          IF FIRST-OF(TSCuentas.TS_Cuenta) THEN DO:
              RUN Buscar_Cuentas (INPUT TSCuentas.TS_Cuenta).
              W_Nom = Cuentas.Nombre.
              FOR EACH Sal_Cuenta WHERE 
                       Sal_Cuenta.Agencia GE W_Ag1 AND
                       Sal_Cuenta.Agencia LE W_Ag2 AND
                       Sal_Cuenta.Cuenta  EQ TSCuentas.TS_Cuenta AND
                       Sal_Cuenta.Ano     EQ YEAR(TSCuentas.TS_Fecha) NO-LOCK:
                RUN HallarSdo (INPUT MONTH(W_Fec1), OUTPUT SIni, OUTPUT SFTo).
                ASSIGN TSIni = TSIni + SIni
                       TSFin = TSFin + SFto.
              END.
              SAnt = TSIni.
          END.
          IF Cuentas.Naturaleza EQ "DB" THEN
             ASSIGN SFin = SAnt + TSCuentas.TS_Db - TSCuentas.TS_Cr.
          ELSE
             ASSIGN SFin = SAnt - TSCuentas.TS_Db + TSCuentas.TS_Cr.
          IF TSCuentas.TS_Fecha GE W_FecIniMes /*W_Fec1*/ THEN DO:
              /*DISPLAY
                TSCuentas.TS_Fecha AT 16
                TSCuentas.TS_Db    AT 50 FORMAT "->>,>>>,>>>,>>9.99"
                TSCuentas.TS_Cr    AT 72 FORMAT "->>,>>>,>>>,>>9.99"
                SFin               AT 94 FORMAT "->>,>>>,>>>,>>9.99"
              WITH DOWN FRAME F_Mov WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.*/
              
              FIND TBrecha WHERE TBrecha.Cta BEGINS SUBSTRING(TSCuentas.Ts_Cuenta,1,4) NO-ERROR.
              IF NOT AVAILABLE TBrecha THEN DO:
                 MESSAGE "Error en archivo de brecha" SKIP
                         "Cuenta: " TSCuentas.TS_Cuenta VIEW-AS ALERT-BOX.
              END.

              ASSIGN TBrecha.Meses[MONTH(TSCuentas.TS_Fecha)] = TBrecha.Meses[MONTH(TSCuentas.TS_Fecha)] + SFin
                     TBrecha.Mes12 = TBrecha.Mes12 + SFin.

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
              ASSIGN TT_db = 0 TT_Cr = 0 SAnt = 0 SFto = 0 TSIni = 0 TSFin = 0.
          END.
      END.  
      VIEW FRAME F-Encabezado.
      VIEW FRAME f-ftr.
      FOR EACH TBrecha:
          ASSIGN Tri2 = tbrecha.meses[4]  + tbrecha.meses[5]  + tbrecha.meses[6]
                 Tri3 = tbrecha.meses[7]  + tbrecha.meses[8]  + tbrecha.meses[9]
                 Tri4 = tbrecha.meses[10] + tbrecha.meses[11] + tbrecha.meses[12].
          DISPLAY tbrecha.cta 
                  tbrecha.meses[1] 
                  tbrecha.meses[2] 
                  tbrecha.meses[3]
                /*tbrecha.meses[4]
                  tbrecha.meses[5]
                  tbrecha.meses[6]*/
                  Tri2
                  Tri3
                  Tri4
                  Mes12
                   SKIP
          WITH FRAME ff WIDTH 150 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
          ASSIGN Tri2 = 0 Tri3 = 0 Tri4 = 0.
      END.
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
                                           
