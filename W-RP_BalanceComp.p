  DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Balance General Comparativo".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.
  
  DEFINE VAR TotDeb  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
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
      DEFINE VARIABLE SIni LIKE Sal_Cuenta.Sal_Inicial INITIAL 0.
      DEFINE VARIABLE SDb  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SCr  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SFin LIKE Sal_Cuenta.Sal_Inicial INITIAL 0.
      DEFINE VAR i AS INTEGER.
      DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
      DEFINE VAR MDb  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MCr  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MIni LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MFin LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MNt  LIKE Cuentas.Naturaleza.
      
      MCta = Sal_Cuenta.Cuenta.
      RUN HallarSdo (INPUT MONTH(W_Fec1), OUTPUT SIni, OUTPUT SFin).
      FIND TSCuentas WHERE TSCuentas.TS_Cuenta EQ Cuentas.Cuenta NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(TSCuentas) THEN
         RUN Grabar_Enc_Temporal.
      ASSIGN TSCuentas.TS_Ini  = TSCuentas.TS_Ini  + SIni
             MINI              = TSCuentas.TS_Ini
             TSCuentas.TS_Db   = TSCuentas.TS_Db   + Sal_Cuenta.Db[MONTH(W_Fec1)]
             MDb               = TSCuentas.TS_Db
             TSCuentas.TS_Cr   = TSCuentas.TS_Cr   + Sal_Cuenta.Cr[MONTH(W_Fec1)]
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
         ASSIGN TSCuentas.TS_Db = TSCuentas.TS_DB + Sal_Cuenta.Db[MONTH(W_Fec1)] /*MDb*/
                TSCuentas.TS_Cr = TSCuentas.TS_CR + Sal_Cuenta.Cr[MONTH(W_Fec1)]. /*MCr.*/
         IF Cuentas.Naturaleza NE MNt THEN
            ASSIGN TSCuentas.TS_Ini  = TSCuentas.TS_Ini - SIni /*MIni*/
                   TSCuentas.TS_Fin  = TSCuentas.TS_Fin - SFin. /*MFin.*/
         ELSE
             ASSIGN TSCuentas.TS_Ini  = TSCuentas.TS_Ini + SIni /*MIni*/
                    TSCuentas.TS_Fin  = TSCuentas.TS_Fin + SFin. /*MFin.*/
    
      END.
  END PROCEDURE.

  PROCEDURE Grabar_Enc_Temporal:
    CREATE TSCuentas.
    ASSIGN TSCuentas.TS_Cuenta  = Cuentas.Cuenta
           TSCuentas.TS_Nombre  = Cuentas.Nombre
           TSCuentas.TS_Nivel   = Cuentas.Nivel
           TSCuentas.TS_Nat     = Cuentas.Naturaleza.
  END PROCEDURE.

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
         ASSIGN TotDeb = TotDeb + Sal_Cuenta.DB[i]
               TotCre = TotCre +  Sal_Cuenta.CR[i].
        /*ASSIGN TotDeb = TotDeb + ROUND(Sal_Cuenta.DB[i],0)
               TotCre = TotCre + ROUND(Sal_Cuenta.CR[i],0).*/
    END.
    CASE Cuentas.Id_Cuenta: /*arma totales iniciales y finales*/
     WHEN 1 THEN DO:
        IF   Cuentas.Naturaleza EQ "DB" THEN
             ASSIGN TotActIni = TotActIni + SIni
                    TotActFin = TotActFin + SFin.
        ELSE ASSIGN TotActIni = TotActIni - SIni
                   TotActFin = TotActFin - SFin.
     END.
     WHEN 2 THEN DO:
        IF   Cuentas.Naturaleza EQ "CR" THEN
            ASSIGN TotPasIni = TotPasIni + SIni
                   TotPasFin = TotPasFin + SFin.
        ELSE
            ASSIGN TotPasIni = TotPasIni - SIni
                   TotPasFin = TotPasFin - SFin.
     END.
     WHEN 3 THEN DO:
        IF   Cuentas.Naturaleza EQ "CR" THEN
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

  PROCEDURE Habilita_Deshabilita:
  /* En este procedimiento se habilitan o deshabilitan las variables
     a pedir en pantalla segun el informe que se vaya a ejecutar. */
      ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.
      DISABLE Cmb_Comprob 
              W_Fec2
              W_NomUsuario1  W_NomUsuario2          
              W_NomCuenta1   W_NomCuenta2         
              W_Nit1         W_NomNit1            
              W_Nit2         W_NomNit2            
              W_Base         W_Porcentaje WITH FRAME F_Valida.
     /* ASSIGN W_Fec1:LABEL IN FRAME F_Valida = "Fecha de Corte"
             W_Fec2:LABEL IN FRAME F_Valida = "Fecha del Día".*/
      IF NOT L_CC THEN
         DISABLE Cmb_CenCost WITH FRAME F_Valida.
      
      ASSIGN W_Fec1:LABEL IN FRAME F_Valida = "Fec.Final"
             W_Fec2:LABEL = "Fec.Mes Ant".

  END PROCEDURE.

  PROCEDURE Tabla_Temporal:  /* TT */
  SESSION:SET-WAIT-STATE("GENERAL").

    FOR EACH Sal_Cuenta WHERE
             Sal_Cuenta.Agencia     GE W_Ag1     AND
             Sal_Cuenta.Agencia     LE W_Ag2     AND
             Sal_Cuenta.Cen_Costos  GE W_CC1     AND
             Sal_Cuenta.Cen_Costos  LE W_CC2     AND
             Sal_Cuenta.Cuenta      GE W_Cuenta1 AND
             Sal_Cuenta.Cuenta      LE W_Cuenta2 AND 
             Sal_Cuenta.Ano         EQ YEAR(W_Fec1) NO-LOCK:
        RUN Buscar_Cuentas (INPUT Sal_Cuenta.Cuenta).
        RUN Mayorizar.
    END.

  SESSION:SET-WAIT-STATE("").
  END PROCEDURE.

  PROCEDURE Proceso_Imprimir:
      DEFINE VAR Listado AS CHARACTER INITIAL "".
      FOR EACH TSCuentas: DELETE TSCuentas. END.
      ASSIGN TotCre = 0 TotDeb = 0 TotActIni = 0 TotActFin = 0 TotPasIni = 0
             TotPasFin = 0 TotPtrIni = 0 TotPtrFin = 0 TotResIni = 0 TotResFin = 0.
      RUN Tabla_Temporal.                        
      Listado = w_Pathspl + "L-BceComp.lst".
     {incluido\IMPRIMIR_carta.I "listado"}.
  END PROCEDURE.

  PROCEDURE ProcesoImprimir:
     {Incluido\RepEncabezado.i}
      DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
      DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".
      DEFINE VARIABLE W_Comparado AS CHARACTER FORMAT "X(13)".
      DEFI VAR FecAnt AS DATE.
      DEFI VAR Exced1 LIKE TSCuentas.TS_Ini.
      DEFI VAR Exced2 LIKE TSCuentas.TS_Fin.
      
      ASSIGN FecAnt = DATE(MONTH(W_Fec1),1,YEAR(W_Fec1))
             FecAnt = FecAnt  - 1.

      W_Reporte   = "REPORTE   : BALANCE GENERAL - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(25)")
                    + " - "+ STRING(TIME,"hh:mm am") + " - Comparado".
      W_EncColumna = "CUENTA         NOMBRE                    NAT       " + STRING(FecAnt) + "       " + STRING(W_Fec1) + "          VARIACION           %".

      DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".

      DEFINE VAR  TT_Ini AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
      DEFINE VAR  TT_Db  AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
      DEFINE VAR  TT_Fin AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
      
      DEFINE VAR TotIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
      DEFINE VAR TotFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".

      FOR EACH TSCuentas WHERE TSCuentas.TS_Cuenta EQ "4" OR TSCuentas.TS_Cuenta EQ "5" OR
                               TSCuentas.TS_Cuenta EQ "6" OR TSCuentas.TS_Cuenta EQ "7":          
             IF TSCuentas.TS_Cuenta EQ "4" THEN
                ASSIGN Exced1 = Exced1 + TSCuentas.TS_Ini
                       Exced2 = Exced2 + TSCuentas.TS_Fin.
             ELSE 
                ASSIGN Exced1 = Exced1 - TSCuentas.TS_Ini 
                       Exced2 = Exced2 - TSCuentas.TS_Fin.          
      END.

      FIND FIRST Cuentas WHERE Cuentas.Id_Excendentes NO-LOCK NO-ERROR.
      IF AVAILABLE(Cuentas) THEN DO:
         CREATE TSCuentas.
         ASSIGN TSCuentas.TS_Cuenta = Cuentas.Cuenta
                TSCuentas.TS_Nombre = "Excedentes o Perdidas"
                TSCuentas.TS_Nat    = Cuentas.Naturaleza
                TSCuentas.TS_Ini    = Exced1
                TSCuentas.TS_Fin    = Exced2. 
      END.

      VIEW FRAME F-Encabezado.
      VIEW FRAME f-ftr.


      FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel
          BREAK BY SUBSTRING(TSCuentas.TS_Cuenta,1,1)  BY SUBSTRING(TSCuentas.TS_Cuenta,1,2)
                BY SUBSTRING(TSCuentas.TS_Cuenta,1,4)  BY SUBSTRING(TSCuentas.TS_Cuenta,1,6)
                BY TSCuentas.TS_Cuenta BY TSCuentas.TS_Nivel:

          IF FIRST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,1)) THEN
             DISPLAY SKIP(2) WITH FRAME F_Mov5 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
          ELSE
          IF FIRST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,6)) THEN
             DISPLAY SKIP(1) WITH FRAME F_Mov6 USE-TEXT STREAM-IO NO-LABELS NO-BOX.

          IF SUBSTRING(TSCuentas.TS_Cuenta,1,1) GT "3" THEN
             NEXT.

          IF TSCuentas.TS_Ini NE 0 OR TSCuentas.TS_Fin NE 0 THEN
              DISPLAY                                                 
                TSCuentas.TS_Cuenta     AT 1                          
                TSCuentas.TS_Nombre     AT 16                         
                TSCuentas.TS_Nat        AT 42                         
                TSCuentas.TS_Ini        AT 46                         
                TSCuentas.TS_Fin        AT 61
                TSCuentas.TS_Fin - TSCuentas.TS_Ini AT 79 FORMAT "->>>>>,>>>,>>9.99"
                ABS(TSCuentas.TS_Ini / (TSCuentas.TS_Fin - TSCuentas.TS_Ini))  AT 100 FORMAT "->>9.99"
                SKIP(0)                       
              WITH WIDTH 132 FRAME F_Mov USE-TEXT STREAM-IO NO-LABELS NO-BOX.

          IF FIRST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,1)) THEN
             DISPLAY SKIP(2) WITH FRAME F_Mov1 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
          ELSE
          IF FIRST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,2)) THEN
             DISPLAY SKIP(1) WITH FRAME F_Mov2 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
          ELSE
          IF FIRST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,4)) THEN
             DISPLAY SKIP(1) WITH FRAME F_Mov3 USE-TEXT STREAM-IO NO-LABELS NO-BOX.                  
      END.

      ASSIGN TotIni = TotActIni - (TotPasIni + TotPtrIni) + TotResIni
             TotFin = TotActFin - (TotPasFin + TotPtrFin) + TotResFin.

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
