  DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Cambio en la Posición Financiera".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.
  
  DEFINE VAR TotDeb  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  DEFINE VAR W_Var1        AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZ9".
  DEFINE VAR TotCre  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  DEFINE VAR TotActIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPasIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPtrIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotResIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotActFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPasFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPtrFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotResFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  

  DEFINE TEMP-TABLE TSCuentas
      FIELD TS_Cuenta      AS CHARACTER FORMAT "X(14)"
      FIELD TS_Nombre      AS CHARACTER FORMAT "X(25)"
      FIELD TS_Nivel       LIKE Cuentas.Nivel
      FIELD TS_IdCta       LIKE Cuentas.Id_Cuenta
      FIELD TS_CamPos      LIKE Cuentas.Cam_Posicion
      FIELD TS_Nat         LIKE Cuentas.Naturaleza
      FIELD TS_Ini         AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
      FIELD TS_Db          AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
      FIELD TS_Cr          AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
      FIELD TS_Fin         AS DECIMAL FORMAT "->>,>>>,>>>,>>9".

/* incluido de Pantalla con parametros*/
{incluido/Pantalla_Validacion.i}    
/* fin incluido Pantalla parametros */

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
    IF T_ConsCta NE "" OR T_ConsCta NE "?" THEN DO:
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

  PROCEDURE Mayorizar:
      DEFINE VARIABLE SIni LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SDb  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SCr  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SFin LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR i AS INTEGER.
      DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
      DEFINE VAR MDb  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MCr  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MIni LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MFin LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MNt  LIKE Cuentas.Naturaleza.

      MCta = Sal_Cuenta.Cuenta.
      RUN HallarSdo (INPUT MONTH(W_Fec1), INPUT MONTH(W_Fec2), OUTPUT SIni, OUTPUT SFin, OUTPUT SDb, OUTPUT SCr).
      FIND TSCuentas WHERE TSCuentas.TS_Cuenta EQ Cuentas.Cuenta NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(TSCuentas) THEN
         RUN Grabar_Enc_Temporal.
      ASSIGN TSCuentas.TS_Ini  = TSCuentas.TS_Ini  + SIni
             MINI              = TSCuentas.TS_Ini
             TSCuentas.TS_Db   = TSCuentas.TS_Db   + SDb /*Sal_Cuenta.Db[MONTH(W_Fec1)]*/
             MDb               = TSCuentas.TS_Db
             TSCuentas.TS_Cr   = TSCuentas.TS_Cr   + SCr /*Sal_Cuenta.Cr[MONTH(W_Fec1)]*/
             MCr               = TSCuentas.TS_Cr
             TSCuentas.TS_Fin  = TSCuentas.TS_Fin  + SFin
             MFin              = TSCuentas.TS_Fin
             MNt               = Cuentas.Naturaleza.
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
         ASSIGN TSCuentas.TS_Db = TSCuentas.TS_DB + SDb /*Sal_Cuenta.Db[MONTH(W_Fec1)] /*MDb*/*/
                TSCuentas.TS_Cr = TSCuentas.TS_CR + SCr. /*Sal_Cuenta.Cr[MONTH(W_Fec1)]. /*MCr.*/*/
         IF Cuentas.Naturaleza NE MNt THEN
            ASSIGN TSCuentas.TS_Ini  = TSCuentas.TS_Ini - SIni /*MIni*/
                   TSCuentas.TS_Fin  = TSCuentas.TS_Fin - SFin. /*MFin.*/
         ELSE
             ASSIGN TSCuentas.TS_Ini  = TSCuentas.TS_Ini + SIni /*MIni*/
                    TSCuentas.TS_Fin  = TSCuentas.TS_Fin + SFin. /*MFin.*/
      END.
  END PROCEDURE.

/*mayorixar las utiliadads*/
  PROCEDURE Mayorizar2:
      DEFINE VARIABLE SIni LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SDb  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SCr  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SFin LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR i AS INTEGER.
      DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
      DEFINE VAR MDb  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MCr  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MIni LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MFin LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MNt  LIKE Cuentas.Naturaleza.

      MCta = Sal_Cuenta.Cuenta.
      RUN HallarSdo (INPUT MONTH(W_Fec1), INPUT MONTH(W_Fec2), OUTPUT SIni, OUTPUT SFin, OUTPUT SDb, OUTPUT SCr).
      ASSIGN TSCuentas.TS_Ini  = TSCuentas.TS_Ini  + SIni
             MINI              = TSCuentas.TS_Ini
             TSCuentas.TS_Db   = TSCuentas.TS_Db   + SDb /*Sal_Cuenta.Db[MONTH(W_Fec1)]*/
             MDb               = TSCuentas.TS_Db
             TSCuentas.TS_Cr   = TSCuentas.TS_Cr   + SCr /*Sal_Cuenta.Cr[MONTH(W_Fec1)]*/
             MCr               = TSCuentas.TS_Cr
             TSCuentas.TS_Fin  = TSCuentas.TS_Fin  + SFin
             MFin              = TSCuentas.TS_Fin
             MNt               = Cuentas.Naturaleza.
  END PROCEDURE.

  PROCEDURE Grabar_Enc_Temporal:
    CREATE TSCuentas.
    ASSIGN TSCuentas.TS_Cuenta  = Cuentas.Cuenta
           TSCuentas.TS_Nombre  = Cuentas.Nombre
           TSCuentas.TS_Nivel   = Cuentas.Nivel
           TSCuentas.TS_Nat     = Cuentas.Naturaleza
           TSCuentas.TS_IdCta   = Cuentas.Id_Cuenta
           TSCuentas.TS_CamPos  = Cuentas.Cam_Posicion.
  END PROCEDURE.

  PROCEDURE Buscar_Cuentas:
      DEFINE INPUT PARAMETER Cta LIKE Cuentas.Cuenta.
      FIND Cuentas WHERE Cuentas.Cuenta EQ Cta AND 
                         Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  END PROCEDURE.

  PROCEDURE HallarSdo:
    DEFINE INPUT  PARAMETER SmesI AS INTEGER.
    DEFINE INPUT  PARAMETER SMesF AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SDeb LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SCre LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR i AS INTEGER.
    SFin = Sal_Cuenta.Sal_Inicial.
    DO i = 1 TO SMesF BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN DO:
           ASSIGN SFin  = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
           IF i LE SMesI THEN
              SIni  = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
        END.
        ELSE DO:
           ASSIGN SFin  = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
           IF i LE SMesI THEN
              SIni  = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
        END.
        ASSIGN TotDeb = TotDeb + ROUND(Sal_Cuenta.DB[i],0)
               TotCre = TotCre + ROUND(Sal_Cuenta.CR[i],0)
               SDeb   = SDeb   + Sal_Cuenta.DB[i]
               SCre   = SCre   + Sal_Cuenta.CR[i]. 
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
  /* En este procedimiento se habilitan o deshabilitan las variables
     a pedir en pantalla segun el informe que se vaya a ejecutar. */
      ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.
      DISABLE Cmb_Comprob 
              W_Usuario1     W_Usuario2
              W_NomUsuario1  W_NomUsuario2          
              W_NomCuenta1   W_NomCuenta2         
              W_Nit1         W_NomNit1            
              W_Nit2         W_NomNit2
              W_Cuenta1      W_Cuenta2            
              W_Base         W_Porcentaje WITH FRAME F_Valida.
      ASSIGN W_Fec1:LABEL IN FRAME F_Valida = "Fecha Inicial"
             W_Fec2:LABEL IN FRAME F_Valida = "Fecha Final".
      IF NOT L_CC THEN
         DISABLE Cmb_CenCost WITH FRAME F_Valida.
  END PROCEDURE.

  PROCEDURE Tabla_Temporal:  /* TT */
    FOR EACH Cuentas FIELDS (Cuentas.Cuenta Cuentas.Tipo Cuentas.Nombre Cuentas.Cam_Posicion
                             Cuentas.Naturaleza Cuentas.Nivel Cuentas.Id_Cuenta) NO-LOCK
                      WHERE (Cuentas.Cam_Posicion LT 4 AND
                             Cuentas.Tipo         EQ 2):
      FOR EACH Sal_Cuenta WHERE
               Sal_Cuenta.Agencia     GE W_Ag1     AND
               Sal_Cuenta.Agencia     LE W_Ag2     AND
               Sal_Cuenta.Cen_Costos  GE W_CC1     AND
               Sal_Cuenta.Cen_Costos  LE W_CC2     AND
               Sal_Cuenta.Cuenta      EQ Cuentas.Cuenta AND
               Sal_Cuenta.Ano         EQ YEAR(W_Fec1) NO-LOCK:
          RUN Buscar_Cuentas (INPUT Sal_Cuenta.Cuenta).
          RUN Mayorizar.
      END.
    END.
    CREATE TSCuentas.         
    ASSIGN TSCuentas.TS_Campos = 1
           TSCuentas.TS_Cuenta = "3605"
           TSCuentas.TS_Nombre = "Excedentes o Pérdidas".    
    FOR EACH Cuentas FIELDS (Cuentas.Cuenta Cuentas.Tipo Cuentas.Nombre Cuentas.Cam_Posicion
                             Cuentas.Naturaleza Cuentas.Nivel Cuentas.Id_Cuenta) NO-LOCK
                      WHERE (Cuentas.Id_Cuenta    EQ 4 AND /*para las de utilizada*/
                             Cuentas.Id_Paag = FALSE):
      FOR EACH Sal_Cuenta WHERE
               Sal_Cuenta.Agencia     GE W_Ag1     AND
               Sal_Cuenta.Agencia     LE W_Ag2     AND
               Sal_Cuenta.Cen_Costos  GE W_CC1     AND
               Sal_Cuenta.Cen_Costos  LE W_CC2     AND
               Sal_Cuenta.Cuenta      EQ Cuentas.Cuenta AND
               Sal_Cuenta.Ano         EQ YEAR(W_Fec1) NO-LOCK:
          RUN Buscar_Cuentas (INPUT Sal_Cuenta.Cuenta).
          RUN Mayorizar2.
      END.
    END.
  END PROCEDURE.

  PROCEDURE Proceso_Imprimir:
      DEFINE VAR Listado AS CHARACTER INITIAL "".
      FOR EACH TSCuentas: DELETE TSCuentas. END.
      ASSIGN TotCre = 0 TotDeb = 0 TotActIni = 0 TotActFin = 0 TotPasIni = 0
             TotPasFin = 0 TotPtrIni = 0 TotPtrFin = 0 TotResIni = 0 TotResFin = 0.
      RUN Tabla_Temporal.                        
      Listado = w_Pathspl + "L-ENTIDA.lst".
     {incluido\IMPRIMIR.I "listado"}.
  END PROCEDURE.

  PROCEDURE ProcesoImprimir:
     {Incluido\RepEncabezado.i}
      DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
      DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".
      W_Reporte   = "REPORTE   : BALANCE DE PRUEBA - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(20)")
                    + " al " + STRING(W_Fec1) + " Sacado el: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = "CUENTA         NOMBRE                          SALDO INICIAL     DISMINUCIONES   AUMENTOS       SALDO FINAL".

      DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".

      DEFINE VAR  TT_Ini AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
      DEFINE VAR  TT_Db  AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
      DEFINE VAR  TT_Fin AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
      
      DEFINE VAR TotIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
      DEFINE VAR TotFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
      DEFINE VAR Variacion AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".

      VIEW FRAME F-Encabezado.
      VIEW FRAME f-ftr.
      FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel
          BREAK BY 
          TSCuentas.TS_Campos BY
          SUBSTRING(TSCuentas.TS_Cuenta,1,4) BY TSCuentas.TS_Cuenta:
          IF FIRST-OF(TSCuentas.TS_CamPos) THEN
          DO:
           CASE TSCuentas.TS_Campos:
             WHEN 1 THEN
               DISPLAY  "UTILIDAD:"          @ TSCuentas.TS_Nombre
               WITH FRAME F-Detalle1 USE-TEXT STREAM-IO NO-LABELS.
             WHEN 2 THEN
               DISPLAY  "MAS OTRAS FUENTES:" @ TSCuentas.TS_Nombre
               WITH FRAME F-Detalle2 USE-TEXT STREAM-IO NO-LABELS.
             WHEN 3 THEN
               DISPLAY  "APLICACIONES:"      @ TSCuentas.TS_Nombre
               WITH FRAME F-Detalle3 USE-TEXT STREAM-IO NO-LABELS.
           END CASE.
          END.
          Variacion = TSCuentas.TS_Fin - TSCuentas.TS_Ini.
          DISPLAY 
            TSCuentas.TS_Cuenta     AT 1  
            TSCuentas.TS_Nombre     AT 16 
            TSCuentas.TS_Nat        AT 42 
            TSCuentas.TS_Fin        AT 46
            TSCuentas.TS_Ini        AT 65 
            Variacion               AT 85 
          WITH WIDTH 132 FRAME F_Mov112 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
          ASSIGN TT_Fin     = TT_Fin + TSCuentas.TS_Fin
                 TT_Db      = TT_Db  + TSCuentas.TS_Fin
                 TT_Cr      = TT_Cr  + TSCuentas.TS_Ini
                 TT_Ini     = TT_Ini + TSCuentas.TS_Ini.
          IF LAST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,4)) THEN DO:
            Variacion = TT_Db - TT_Cr.
            DISPLAY TSCuentas.TS_Cuenta         NO-LABEL     AT 1         
                    TSCuentas.TS_Nombre         NO-LABEL     AT 17 
                    TT_Db                       NO-LABEL     AT 46 /*SKIP */
                    TT_Cr                       NO-LABEL     AT 65 
                    Variacion                   NO-LABEL     AT 85
            WITH DOWN WIDTH 132 USE-TEXT FRAME F-Tot11 STREAM-IO NO-LABELS.
            ASSIGN TT_Db = 0
                   TT_Cr = 0.
          END. 
          IF LAST-OF(TSCuentas.TS_Campos) THEN
          DO:
            DOWN 1 WITH FRAME F-Detalle NO-LABELS.
            CASE TSCuentas.TS_Campos:
              WHEN 2 THEN
              DO: 
                Variacion = TT_Fin - TT_Ini.
                DISPLAY "TOTAL RECURSOS PROVISTOS" AT 1
                        TT_Fin       AT 46
                        TT_Ini       AT 65
                        Variacion    AT 85 SKIP(1)
                WITH FRAME F_Tto1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO. 
                ASSIGN W_Var1     = TT_Fin - TT_Ini
                       TT_Fin = 0 
                       TT_Ini = 0.
              END. 
              WHEN 3 THEN DO:
                Variacion = TT_Fin - TT_Ini.
                DISPLAY "TOTAL RECURSOS APLICADOS" AT 1
                        TT_Fin       AT 46
                        TT_Ini       AT 65
                        Variacion    AT 85 SKIP(1)
                WITH FRAME F_Tto2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
              END.
            END CASE. 
          END.
          IF LAST(TSCuentas.TS_Campos) THEN
          DO:
            Variacion = TT_Fin - TT_Ini.
            DISPLAY "VARIACION EN EL CAPITAL DE TRABAJO" AT 1
                    TT_Fin       AT 46
                    TT_Ini       AT 65
                    Variacion    AT 85 SKIP(1)
            WITH FRAME F_Tto3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO. 
            PAGE.
          END.
      END.  
      PAGE.
    OUTPUT CLOSE.
  END PROCEDURE.

  PROCEDURE Imprimir_Excel:
      {Incluido\Def_Excel.i}
       /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
       E_NumFila = 1.
       E_NumColumn = 7.
       E_Fila      = "014" + "Cuenta        "
                   + "025" + "Nombre                   "
                   + "002" + "Nt"
                   + "015" + "Sdo.Inicial    "
                   + "015" + "Debito         "
                   + "015" + "Credito        "
                   + "015" + "Sdo.Final      ".
       RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

      /* launch Excel so it is visible to the user */
      chExcelApp:Visible = TRUE.

      /* create a new Workbook */
      chWorkbook = chExcelApp:Workbooks:Add().

      /* get the active Worksheet */
      chWorkSheet = chExcelApp:Sheets:Item(1).

       DEFI VAR W_IdCC AS CHARACTER FORMAT "X(2)".
       FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel
           BREAK BY SUBSTRING(TSCuentas.TS_Cuenta,1,1) BY TSCuentas.TS_Cuenta BY TSCuentas.TS_Nivel:
            E_Fila2     = "".
            E_Fila2     = "014" + STRING(TScuentas.TS_Cuenta,"X(14)")
                        + "025" + STRING(TSCuentas.TS_Nombre,"X(25)")
                        + "002" + STRING(TSCuentas.TS_Nat,"99")
                        + "015" + STRING(TSCuentas.TS_Ini,"->>,>>>,>>>,>>>")
                        + "015" + STRING(TSCuentas.TS_Db,"->>,>>>,>>>,>>>")
                        + "015" + STRING(TSCuentas.TS_Cr,"->>,>>>,>>>,>>>")
                        + "015" + STRING(TSCuentas.TS_Fin,"->>,>>>,>>>,>>>").
            {Incluido\imprimir_Excel.i}
        
  END PROCEDURE.

 
