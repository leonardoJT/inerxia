 DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Ejecución Presupuestal".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.
  DEFINE VARIABLE W_Com        AS LOGICAL INITIAL NO.
  
  DEFINE VAR TotMes    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  DEFINE VAR TotCom    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  DEFINE VAR totvaria  AS DECIMAL FORMAT "->>,>>>,>>>,>>9" INITIAL 0.
  DEFINE VAR totporc   AS DECIMAL FORMAT "->>9.99" INITIAL 0.00.
  DEFINE VAR Wacum     AS LOGICAL INITIAL TRUE.
  DEFINE VARIABLE xarchivo AS CHAR.

   DEFINE TEMP-TABLE TSPresup
      FIELD TS_Cuenta      AS CHARACTER FORMAT "X(14)"
      FIELD TS_SMes        AS DECIMAL FORMAT "->>,>>>,>>>,>>9" EXTENT 12
      FIELD ts_agencia     AS INTEGER.
      
  DEFINE TEMP-TABLE TSCuentas
      FIELD TS_Cuenta      AS CHARACTER FORMAT "X(14)"
      FIELD TS_Nombre      AS CHARACTER FORMAT "X(25)"
      FIELD TS_Nivel       LIKE Cuentas.Nivel
      FIELD TS_Nat         LIKE Cuentas.Naturaleza
      FIELD TS_IdCta       LIKE Cuentas.Id_Cuenta
      FIELD TS_SMes        AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9"
      FIELD TS_SCom        AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9"
      FIELD TS_Vari        AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9"
      FIELD TS_Porc        AS DECIMAL FORMAT "->>9.99"
      INDEX ts_cuenta ts_cuenta.

 
  ASSIGN xarchivo = "ppto" + trim(STRING(YEAR(TODAY))) + ".txt".
  INPUT FROM value(xarchivo).
  REPEAT:
      CREATE TSPresup.
      IMPORT DELIMITER ";" TSPresup.
  END.
  INPUT CLOSE.

  
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
                 T_ConsNom = substring(Cuentas.Nombre,1,22).
       ELSE MESSAGE "NO ENCONTRO " T_CONSCTAI VIEW-AS ALERT-BOX.
    END.
    IF T_ConsCta NE "" THEN DO:
       RUN C-Cuentas.r (OUTPUT T_ConsCta, OUTPUT T_ConsNom, OUTPUT W_Naturaleza, OUTPUT W_CtrNat, INPUT "T").
       IF T_ConsCta EQ ? THEN DO:
          FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
          IF NOT AVAILABLE(Cuentas) THEN DO:
              MESSAGE "NO ENCONTRO " T_CONSCTAI VIEW-AS ALERT-BOX.
              ASSIGN T_ConsCta  = ""
                   T_ConsNom  = "".           
          END.
       END.
    END.
  END PROCEDURE.

/* fin incluido Pantalla parametros */

  PROCEDURE Mayorizar:
      DEFINE VARIABLE SMes LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SCom LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE VARI LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE Porc LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SIni LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR i AS INTEGER.
      DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
      DEFINE VAR MMes LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MNt  LIKE Cuentas.Naturaleza.

      MCta = Sal_Cuenta.Cuenta.
      RUN HallarSdo (INPUT MONTH(W_Fec1), OUTPUT SIni, OUTPUT SMes).
      FIND FIRST TSCuentas WHERE TSCuentas.TS_Cuenta EQ Cuentas.Cuenta NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(TSCuentas) THEN
         RUN Grabar_Enc_Temporal.
      ASSIGN TSCuentas.TS_SMes  = TSCuentas.TS_SMes  + SMes
             MMes               = TSCuentas.TS_SMes
             MNt                = Cuentas.Naturaleza.
      IF TSCuentas.TS_Nat NE "CR" THEN TotMes = TotMes - SMes.
      ELSE TotMes = TotMes + SMes.
      
      IF SIni NE 0 THEN TSCuentas.TS_SCom = SIni.
      MCta = Cuentas.Cuenta.
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
         IF Cuentas.Naturaleza NE MNt THEN DO:
            ASSIGN TSCuentas.TS_SMes = TSCuentas.TS_SMes - MMes.
            IF SIni NE 0 THEN
               ASSIGN TSCuentas.TS_SCom = TSCuentas.TS_SCom - SIni.
         END.
         ELSE DO:
            ASSIGN TSCuentas.TS_SMes = TSCuentas.TS_SMes + MMes.
            IF SIni NE 0 THEN
               ASSIGN TSCuentas.TS_SCom = TSCuentas.TS_SCom + SIni.
         END.
      END.
  END PROCEDURE.

  PROCEDURE Mayorizar_Presup:
      DEFINE VARIABLE SCom LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VARIABLE SIni LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR i AS INTEGER.
      DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
      DEFINE VAR MMes LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR MNt  LIKE Cuentas.Naturaleza.

      MCta = TSPresup.TS_Cuenta.
      /*RUN HallarSdo (INPUT MONTH(W_Fec1), OUTPUT SIni, OUTPUT SCom).*/
      IF Wacum  THEN DO:
        DO i = 1 TO MONTH(w_fec1):
           ASSIGN Scom = Scom + (TSPresup.TS_SMes[i]).
        END.
      END.
      ELSE DO:
          ASSIGN Scom = TSPresup.TS_SMes[MONTH(w_fec1)].
      END.
      FIND FIRST TSCuentas WHERE trim(TSPresup.TS_Cuenta) EQ trim(TSPresup.TS_Cuenta) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(TSCuentas) THEN
         RUN Grabar_Enc_Temporal.
      ASSIGN TSCuentas.TS_SCom  = TSCuentas.TS_SCom  + SCom
             MMes               = TSCuentas.TS_SCom
             MNt                = Cuentas.Naturaleza.
      MCta = Cuentas.Cuenta.
      IF TSCuentas.TS_Nat NE "CR" THEN TotCom = TotCom - SCom.
      ELSE TotCom = TotCom + SCom.
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
         IF Cuentas.Naturaleza NE MNt THEN
            ASSIGN TSCuentas.TS_SCom = TSCuentas.TS_SCom - MMes.
         ELSE
            ASSIGN TSCuentas.TS_SCom = TSCuentas.TS_SCom + MMes.
      END.
  END PROCEDURE.

  PROCEDURE Grabar_Enc_Temporal:
    CREATE TSCuentas.
    ASSIGN TSCuentas.TS_Cuenta  = Cuentas.Cuenta
           TSCuentas.TS_Nombre  = Cuentas.Nombre
           TSCuentas.TS_Nivel   = Cuentas.Nivel
           TSCuentas.TS_Nat     = Cuentas.Naturaleza
           TSCuentas.TS_IdCta   = Cuentas.Id_Cuenta.
  END PROCEDURE.

  PROCEDURE Buscar_Cuentas:
      DEFINE INPUT PARAMETER Cta LIKE Cuentas.Cuenta.
      FIND Cuentas WHERE Cuentas.Cuenta EQ Cta AND 
                         Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  
      /*IF NOT AVAILABLE(Cuentas) THEN
         MESSAGE "NO ENCONTRO " cta VIEW-AS ALERT-BOX.*/

   END PROCEDURE.

  PROCEDURE HallarSdo:
    DEFINE INPUT  PARAMETER Smes AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR i AS INTEGER.
    SFin = Sal_Cuenta.Sal_Inicial.
    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
           ASSIGN SFin  = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
        ELSE
           ASSIGN SFin  = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
        IF YEAR(W_Fec1) EQ YEAR(W_Fec2) AND MONTH(W_Fec2) EQ i AND W_Com = NO AND 
           MONTH(W_Fec2) LT MONTH(W_Fec1) THEN 
           SIni = SFin.
    END.
  END PROCEDURE.

  PROCEDURE Habilita_Deshabilita:
  /* En este procedimiento se habilitan o deshabilitan las variables
     a pedir en pantalla segun el informe que se vaya a ejecutar. */
      ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.
      DISABLE Cmb_Comprob 
              W_Cuenta1      W_Cuenta2
              W_NomUsuario1  W_NomUsuario2          
              W_NomCuenta1   W_NomCuenta2         
              W_Nit1         W_NomNit1            
              W_Nit2         W_NomNit2            
              W_Base         W_Porcentaje WITH FRAME F_Valida.
      IF NOT L_CC THEN
         DISABLE Cmb_CenCost WITH FRAME F_Valida.
      ASSIGN W_Fec1:LABEL IN FRAME F_Valida = "Fec.Balance"
             W_Fec2:LABEL = "Fec.Compara".

  END PROCEDURE.

  PROCEDURE Tabla_Temporal:  /* TT */
    DEFINE VAR i AS INTEGER.
    DEFINE VAR TM_SMes        AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR TM_SCom        AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR TM_Vari        AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR TM_Porc        AS DECIMAL FORMAT "->>9.99".
    DEFINE VAR SIni           AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR SCom           AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR Mcta           AS CHARACTER FORMAT "X(14)".
    ASSIGN TotMes = 0 TotCom = 0.
    FOR EACH Sal_Cuenta WHERE
             Sal_Cuenta.Agencia     GE W_Ag1     AND
             Sal_Cuenta.Agencia     LE W_Ag2     AND
             Sal_Cuenta.Cen_Costos  GE W_CC1     AND
             Sal_Cuenta.Cen_Costos  LE W_CC2     AND
             Sal_Cuenta.Cuenta      GE W_Cuenta1 AND
             Sal_Cuenta.Cuenta      LE W_Cuenta2 AND 
             Sal_Cuenta.Ano         EQ YEAR(W_Fec1) NO-LOCK:
        RUN Buscar_Cuentas (INPUT Sal_Cuenta.Cuenta).
        IF NOT AVAILABLE(Cuentas) THEN
            NEXT.
        IF Cuentas.Id_Cuenta EQ 4 THEN DO:
          W_Com = NO.
          RUN Mayorizar.
        END.
    END.
    /* MAYORIZA PRESUPUESTO */
    FOR EACH TSPresup NO-LOCK:
        RUN Buscar_Cuentas (INPUT tspresup.TS_Cuenta).
        IF NOT AVAILABLE(Cuentas) THEN
            NEXT.
        IF Cuentas.Id_Cuenta EQ 4 THEN DO:
           W_Com = YES.
           RUN Mayorizar_Presup.
        END.
    END.
    /* Fin mayoriza presupuesto */
   
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
      DEFINE VARIABLE W_Comparado AS CHARACTER FORMAT "X(13)".
      DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
      DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".
      IF W_Fec1 NE W_Fec2 THEN W_Comparado = " - Comparado".
      W_Reporte   = "REPORTE   : EJECUCION PRESUPUESTAL - Fecha de Corte - " + STRING(W_Fec1)
                    + " - "+ STRING(TIME,"hh:mm am") + W_Comparado.
      W_EncColumna = "CUENTA         NOMBRE                    NAT       " + STRING(W_Fec1) + "       " + "Presupuesto" + "       VARIACION       %Cumpl.".

      DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".

      DEFINE VAR  TT_Mes AS DECIMAL FORMAT "->>,>>>,>>>,>>9" INITIAL 0.
      DEFINE VAR  TT_Com AS DECIMAL FORMAT "->>,>>>,>>>,>>9" INITIAL 0.
      DEFINE VAR  TT_Var AS DECIMAL FORMAT "->>,>>>,>>>,>>9" INITIAL 0.
      DEFINE VAR  TT_Por AS DECIMAL FORMAT "->>9.99" INITIAL 0.
      DEFINE VAR  TR_Mes AS DECIMAL FORMAT "->>,>>>,>>>,>>9" INITIAL 0.
      DEFINE VAR  TR_Com AS DECIMAL FORMAT "->>,>>>,>>>,>>9" INITIAL 0.
      DEFINE VAR  TR_Var AS DECIMAL FORMAT "->>,>>>,>>>,>>9" INITIAL 0.
      DEFINE VAR  TR_Por AS DECIMAL FORMAT "->>9.99" INITIAL 0.

      DEFINE FRAME F_Mov
        TSCuentas.TS_Cuenta     AT 1
        TSCuentas.TS_Nombre     AT 16
        TSCuentas.TS_Nat        AT 42
        TSCuentas.TS_SMes       AT 46
        TSCuentas.TS_SCom       AT 61
        TSCuentas.TS_Vari       AT 79
        TSCuentas.TS_Porc       AT 100
      WITH DOWN WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

      VIEW FRAME F-Encabezado.
      VIEW FRAME f-ftr.
      
      FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel
          BREAK BY SUBSTRING(TSCuentas.TS_Cuenta,1,1) BY TSCuentas.TS_Cuenta BY TSCuentas.TS_Nivel BY TSCuentas.TS_IdCta:
          ASSIGN TSCuentas.TS_Vari = TSCuentas.TS_SCom - TSCuentas.TS_SMes 
                 TSCuentas.TS_Porc =  (100 - (TSCuentas.TS_VARI / TSCuentas.TS_SCom) * 100).
          IF FIRST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,1)) AND TSCuentas.TS_IdCta NE 4 THEN DISPLAY SKIP(1).
            IF ts_smes GT 0 or ts_Scom GT 0  THEN
               DISPLAY 
                 TSCuentas.TS_Cuenta     AT 1  
                 TSCuentas.TS_Nombre     AT 15 
                 TSCuentas.TS_SMes       AT 40 
                 TSCuentas.TS_SCom       AT 61 
                 TSCuentas.TS_Vari       AT 79 
                 TSCuentas.TS_Porc       AT 100 
                WITH WIDTH 132 FRAME F_Mov USE-TEXT STREAM-IO NO-LABELS.
           
          IF TSCuentas.TS_Nivel EQ 2 THEN DO:
             ASSIGN TT_Mes = TT_Mes + TSCuentas.TS_SMes
                    TT_Com = TT_Com + TSCuentas.TS_SCom
                    TT_Var = TT_Com + TSCuentas.TS_Vari
                    TT_Por = TT_Var + TT_Com.
          END.
          IF LAST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,1))  THEN DO:
             ASSIGN TT_Mes = 0 TT_Com = 0 TT_Var = 0 TT_Por = 0.
             DISPLAY "____________________________________________________________________".
          END.
             
      END.
      ASSIGN totvaria  = totcom - totMes 
             totporc   = (100 - (totvaria / totcom) * 100).

      DISPLAY "Totales General "  AT 16
              TotMes              AT 40
              TotCom              AT 61 
              totvaria            AT 79 
              totporc             AT 100 

              WITH FRAME F_Tot WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
     DISPLAY "___________________________________________________________________________________________________________"
                      WITH WIDTH 132 FRAME F_tot USE-TEXT STREAM-IO NO-LABELS.
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
                   + "015" + "SaldoMes:" + STRING(MONTH(W_Fec1),"99") + "/"
                   + "015" + "Saldo Comparado"
                   + "015" + "Variacion      "
                   + "015" + "Porcentaje     ".
       RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

      /* launch Excel so it is visible to the user */
      chExcelApp:Visible = TRUE.

      /* create a new Workbook */
      chWorkbook = chExcelApp:Workbooks:Add().

      /* get the active Worksheet */
      chWorkSheet = chExcelApp:Sheets:Item(1).

       DEFI VAR W_IdCC AS CHARACTER FORMAT "X(2)".
       FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel
           BREAK BY SUBSTRING(TSCuentas.TS_Cuenta,1,1) BY TSCuentas.TS_Cuenta BY TSCuentas.TS_Nivel BY TSCuentas.TS_IdCta:
           ASSIGN TSCuentas.TS_Vari = TSCuentas.TS_SMes - TSCuentas.TS_SCom
                  TSCuentas.TS_Porc = (100 - (TSCuentas.TS_VARI / TSCuentas.TS_SCom) * 100).
            E_Fila2     = "".
            E_Fila2     = "014" + STRING(TScuentas.TS_Cuenta,"X(14)")
                        + "025" + STRING(TSCuentas.TS_Nombre,"X(25)")
                        + "002" + STRING(TSCuentas.TS_Nat,"99")
                        + "015" + STRING(TSCuentas.TS_SMes,"->>,>>>,>>>,>99")
                        + "015" + STRING(TSCuentas.TS_SCom,"->>,>>>,>>>,>99")
                        + "015" + STRING(TSCuentas.TS_Vari,"->>,>>>,>>>,>99")
                        + "015" + STRING(TSCuentas.TS_Porc,"->99.99").
            {Incluido\imprimir_Excel.i}
  END PROCEDURE.
