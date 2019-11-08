  DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Libro Mayor y Balance".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.

  DEFINE TEMP-TABLE TSCuentas
      FIELD TS_Cuenta      AS CHARACTER FORMAT "X(8)"
      FIELD TS_Nombre      AS CHARACTER FORMAT "X(23)"
      FIELD TS_Nivel       LIKE Cuentas.Nivel
      FIELD TS_Nat         LIKE Cuentas.Naturaleza
      FIELD TS_Ini         AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
      FIELD TS_Db          AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"
      FIELD TS_Cr          AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"
      FIELD TS_Fin         AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".

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

  PROCEDURE Mayorizar:
      DEFINE VARIABLE SIni LIKE sal_ctacon.Sal_Inicial.
      DEFINE VARIABLE SDb  LIKE sal_ctacon.Sal_Inicial.
      DEFINE VARIABLE SCr  LIKE sal_ctacon.Sal_Inicial.
      DEFINE VARIABLE SFin LIKE sal_ctacon.Sal_Inicial.
      DEFINE VAR i AS INTEGER.
      DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
      DEFINE VAR MDb  LIKE sal_ctacon.Sal_Inicial.
      DEFINE VAR MCr  LIKE sal_ctacon.Sal_Inicial.
      DEFINE VAR MIni LIKE sal_ctacon.Sal_Inicial.
      DEFINE VAR MFin LIKE sal_ctacon.Sal_Inicial.
      DEFINE VAR MNt  LIKE Cuentas.Naturaleza.

      MCta = sal_ctacon.Cuenta.
      RUN HallarSdo (INPUT MONTH(W_Fec1), OUTPUT SIni, OUTPUT SFin).
      FIND TSCuentas WHERE TSCuentas.TS_Cuenta EQ Cuentas.Cuenta NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(TSCuentas) THEN
         RUN Grabar_Enc_Temporal.
      ASSIGN TSCuentas.TS_Ini  = TSCuentas.TS_Ini  + SIni
             MINI              = TSCuentas.TS_Ini
             TSCuentas.TS_Db   = TSCuentas.TS_Db   + sal_ctacon.Db[MONTH(W_Fec1)]
             MDb               = TSCuentas.TS_Db
             TSCuentas.TS_Cr   = TSCuentas.TS_Cr   + sal_ctacon.Cr[MONTH(W_Fec1)]
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
         ASSIGN TSCuentas.TS_Db = TSCuentas.TS_DB + sal_ctacon.Db[MONTH(W_Fec1)] /*MDb*/
                TSCuentas.TS_Cr = TSCuentas.TS_CR + sal_ctacon.Cr[MONTH(W_Fec1)]. /*MCr.*/
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
    DEFINE OUTPUT PARAMETER Sini LIKE sal_ctacon.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SFin LIKE sal_ctacon.Sal_Inicial.
    DEFINE VAR i AS INTEGER.
    SFin = sal_ctacon.Sal_Inicial.
    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
           ASSIGN SFin  = SFin + sal_ctacon.DB[i] - sal_ctacon.Cr[i]
                  SIni  = SFin - sal_ctacon.DB[i] + sal_ctacon.Cr[i].
        ELSE
           ASSIGN SFin  = SFin - sal_ctacon.DB[i] + sal_ctacon.Cr[i]
                  SIni  = SFin + sal_ctacon.DB[i] - sal_ctacon.Cr[i].
    END.
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
              Cmb_Nivel
              W_Base         W_Porcentaje WITH FRAME F_Valida.
      Cmb_Nivel:SCREEN-VALUE = "4".
      IF NOT L_CC THEN
         DISABLE Cmb_CenCost WITH FRAME F_Valida.
  END PROCEDURE.

  PROCEDURE Tabla_Temporal:  /* TT */
    /*FOR EACH sal_ctacon WHERE
             sal_ctacon.Agencia     GE W_Ag1     AND
             sal_ctacon.Agencia     LE W_Ag2     AND
             sal_ctacon.Cen_Costos  GE W_CC1     AND
             sal_ctacon.Cen_Costos  LE W_CC2     AND
             sal_ctacon.Cuenta      GE W_Cuenta1 AND
             sal_ctacon.Cuenta      LE W_Cuenta2 AND 
             sal_ctacon.Ano         EQ YEAR(W_Fec1) NO-LOCK:*/

    FOR EACH agencias :
      FOR EACH sal_ctacon WHERE
             sal_ctacon.Agencia     GE agencias.agencia AND
             sal_ctacon.Ano         EQ YEAR(W_Fec1) NO-LOCK:
         IF sal_ctacon.Cuenta      GE W_Cuenta1 AND
             sal_ctacon.Cuenta      LE W_Cuenta2 THEN DO:
             RUN Buscar_Cuentas (INPUT sal_ctacon.Cuenta).
             RUN Mayorizar.
         END.
      END.
    END.
  END PROCEDURE.

  PROCEDURE Proceso_Imprimir:
      DEFINE VAR Listado AS CHARACTER INITIAL "".
      FOR EACH TSCuentas: DELETE TSCuentas. END.
      RUN Tabla_Temporal.                        
      Listado = w_Pathspl + "L-ENTIDA.lst".
     {incluido\IMPRIMIR_carta_CamCcio.I "listado"}.
  END PROCEDURE.

  PROCEDURE ProcesoImprimir:
     {Incluido\RepEncabezado.i}
      DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
      DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".
      W_Reporte   = STRING(W_nom_Entidad,"X(54)") + " - " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(20)").
          
          
          /*"REPORTE   : Libro Mayor y Balance - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(20)")
                    + " al " + STRING(W_Fec1) + " Hora: " + STRING(TIME,"hh:mm am").*/
      W_EncColumna = "CUENTA         NOMBRE                    NAT   SALDO INICIAL      DEBITO            CREDITO     SALDO FINAL".

      DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".

      DEFINE VAR  TT_Ini AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
      DEFINE VAR  TT_Db  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  TT_Fin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
      DEFINE VAR  NOMAGE AS CHARACTER FORMAT "X(50)".
      NOMAGE = STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(30)").

      DISPLAY "ENTIDAD         : JURISCOOP        INFORME CONSOLIDADO"  SKIP
              "NIT             : 860.075.780-9"   SKIP
              "REPORTE         : LIBRO MAYOR"     SKIP
              "FECHA DE CORTE  : " STRING(W_Fec1) SKIP
              "--------------------------------------------------------------------------------------------------------------" SKIP
              "CUENTA     NOMBRE                   SALDO INICIAL         DEBITO           CREDITO         SALDO FINAL" SKIP
              "--------------------------------------------------------------------------------------------------------------" 
          WITH FRAME F-encabezado2 WIDTH 132 PAGE-TOP NO-LABELS USE-TEXT NO-BOX STREAM-IO.
      DEFINE FRAME F_Mov
        TSCuentas.TS_Cuenta     AT 1
        TSCuentas.TS_Nombre     AT 9
        /*TSCuentas.TS_Nat        AT 42*/
        TSCuentas.TS_Ini        AT 33 /*46*/
        TSCuentas.TS_Db         AT 53 /*61*/
        TSCuentas.TS_Cr         AT 73 /*79*/
        TSCuentas.TS_Fin        AT 93 /*97*/
      WITH DOWN WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
      VIEW FRAME F-Encabezado2.
      VIEW FRAME f-ftr.
      FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel 
      /*FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel EQ Cmb_Nivel*/
      BREAK BY SUBSTRING(TSCuentas.TS_Cuenta,1,1) BY TSCuentas.TS_Cuenta BY TSCuentas.TS_Nivel:
        IF TSCuentas.Ts_Ini NE 0 OR TSCuentas.TS_Db NE 0 OR TSCuentas.TS_Cr NE 0 THEN
          DISPLAY 
            TSCuentas.TS_Cuenta     AT 1  
            TSCuentas.TS_Nombre     AT 9 
            /*TSCuentas.TS_Nat        AT 42 */
            TSCuentas.TS_Ini        AT 33 /*46*/ 
            TSCuentas.TS_Db         AT 53 /*61*/ 
            TSCuentas.TS_Cr         AT 73 /*79*/ 
            TSCuentas.TS_Fin        AT 93 /*97 SKIP(1)*/
          WITH WIDTH 132 FRAME F_Mov USE-TEXT STREAM-IO NO-LABELS.
          IF LAST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,1)) THEN
             DISPLAY SKIP(1).
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
                        /*+ "002" + STRING(TSCuentas.TS_Nat,"99")*/
                        + "015" + STRING(TSCuentas.TS_Ini,"->>>,>>>,>>>,>>>")
                        + "015" + STRING(TSCuentas.TS_Db,"->>,>>>,>>>,>>>")
                        + "015" + STRING(TSCuentas.TS_Cr,"->>,>>>,>>>,>>>")
                        + "015" + STRING(TSCuentas.TS_Fin,"->>>,>>>,>>>,>>>").
            {Incluido\imprimir_Excel.i}
        
  END PROCEDURE.
