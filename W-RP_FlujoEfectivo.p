  DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Flujo de Efectivo".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.
  
  DEFINE VAR W_SubTotalDb  AS DECIMAL FORMAT  "->>>,>>>,>>>,>>9".
  DEFINE VAR W_SubCtaDb    AS DECIMAL FORMAT  "->>>,>>>,>>>,>>9".
  DEFINE VAR W_SubTotalCr  AS DECIMAL FORMAT  "->>>,>>>,>>>,>>9".
  DEFINE VAR W_SubCtaCr    AS DECIMAL FORMAT  "->>>,>>>,>>>,>>9".
  DEFINE VAR W_TotalDb     AS DECIMAL FORMAT  "->>>,>>>,>>>,>>9".
  DEFINE VAR W_TotalCr    AS DECIMAL FORMAT  "->>>,>>>,>>>,>>9".

  
  DEFINE VAR TotDeb  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  DEFINE VAR W_Var1        AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZ9".
  DEFINE VAR TotCre  AS DECIMAL FORMAT      "->>>,>>>,>>>,>>9".
  DEFINE VAR TotActIni  AS DECIMAL FORMAT   "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPasIni  AS DECIMAL FORMAT   "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPtrIni  AS DECIMAL FORMAT   "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotResIni  AS DECIMAL FORMAT   "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotActFin  AS DECIMAL FORMAT   "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPasFin  AS DECIMAL FORMAT   "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPtrFin  AS DECIMAL FORMAT   "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotResFin  AS DECIMAL FORMAT   "->>>,>>>,>>>,>>9.99".
  DEFINE VAR W_SdoAntBco AS DECIMAL FORMAT  "->>>,>>>,>>>,>>9".
  DEFINE VAR W_SdoAntCaj AS DECIMAL FORMAT  "->>>,>>>,>>>,>>9".
  DEFINE VAR W_SdoActBco AS DECIMAL FORMAT  "->>>,>>>,>>>,>>9".
  DEFINE VAR W_SdoActCaj AS DECIMAL FORMAT  "->>>,>>>,>>>,>>9".
  DEFINE VAR W_TSdoAntBco AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".  
  DEFINE VAR W_TSdoAntCaj AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  DEFINE VAR W_TSdoActBco AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  DEFINE VAR W_TSdoActCaj AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  

  DEFINE TEMP-TABLE TSCuentas
      FIELD TS_Cuenta      AS CHARACTER FORMAT "X(14)"
      FIELD TS_Nombre      AS CHARACTER FORMAT "X(25)"
      FIELD TS_Nivel       LIKE Cuentas.Nivel
      FIELD TS_IdCta       LIKE Cuentas.Id_Cuenta
      FIELD TS_CodFlu      LIKE Cuentas.Cod_FlujoEfec
      FIELD TS_Nat         LIKE Cuentas.Naturaleza
      FIELD TS_Ini         AS DECIMAL FORMAT "->>>,>>>,>>>,>>9"
      FIELD TS_Db          AS DECIMAL FORMAT "->>>>,>>>,>>>,>>9"
      FIELD TS_Cr          AS DECIMAL FORMAT "->>>,>>>,>>>,>>9"
      FIELD TS_Fin         AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".

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

  PROCEDURE Grabar_Enc_Temporal:
    CREATE TSCuentas.
    ASSIGN TSCuentas.TS_Cuenta  = Cuentas.Cuenta
           TSCuentas.TS_Nombre  = Cuentas.Nombre
           TSCuentas.TS_Nivel   = Cuentas.Nivel
           TSCuentas.TS_Nat     = Cuentas.Naturaleza
           TSCuentas.TS_IdCta   = Cuentas.Id_Cuenta
           TSCuentas.TS_CodFlu  = Cuentas.Cod_FlujoEfec.
  END PROCEDURE.

  PROCEDURE Buscar_Cuentas:
      DEFINE INPUT PARAMETER Cta LIKE Cuentas.Cuenta.
      FIND Cuentas WHERE Cuentas.Cuenta EQ Cta AND 
                         Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  END PROCEDURE.

  PROCEDURE HallarSdo:
    DEFINE INPUT  PARAMETER SmesI AS INTEGER.
    DEFINE INPUT  PARAMETER SMesF AS INTEGER.
    DEFINE OUTPUT PARAMETER SAnt LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SAct LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR i AS INTEGER.
    SAct = Sal_Cuenta.Sal_Inicial.
    DO i = 1 TO SMesF BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN DO:
           ASSIGN SAct  = SAct + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
           IF i LE SMesI THEN
              SAnt  = SAct - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
        END.
        ELSE DO:
           ASSIGN SAct  = SAct - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
           IF i LE SMesI THEN
              SAnt  = SAct + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
        END.
    END.
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
      ASSIGN W_SdoAntBco  = 0 W_SdoAntCaj  = 0 W_SdoActBco  = 0 W_SdoActCaj  = 0
             W_TSdoAntBco = 0 W_TSdoAntCaj = 0 W_TSdoActBco = 0 W_TSdoActCaj = 0.
      FOR EACH Sal_Cuenta WHERE /*busca saldo anterior y actual de las cuentas de caja*/
               Sal_Cuenta.Agencia     GE W_Ag1     AND
               Sal_Cuenta.Agencia     LE W_Ag2     AND
               Sal_Cuenta.Cen_Costos  GE W_CC1     AND
               Sal_Cuenta.Cen_Costos  LE W_CC2     AND
               Sal_Cuenta.Cuenta      BEGINS "1105" AND
               Sal_Cuenta.Ano         EQ YEAR(W_Fec1) NO-LOCK:
          RUN Buscar_Cuentas (INPUT Sal_Cuenta.Cuenta).
          RUN HallarSdo(INPUT MONTH(W_Fec1), INPUT MONTH(W_Fec2), OUTPUT W_SdoAntCaj, OUTPUT W_SdoActCaj).
          ASSIGN W_TSdoAntCaj = W_TSdoAntCaj + W_SdoAntCaj
                 W_TSdoActCaj = W_TSdoActCaj + W_SdoActCaj.
      END.
      FOR EACH Sal_Cuenta WHERE /*busca saldo anterior y actual de las cuentas de bancos*/
               Sal_Cuenta.Agencia     GE W_Ag1     AND
               Sal_Cuenta.Agencia     LE W_Ag2     AND
               Sal_Cuenta.Cen_Costos  GE W_CC1     AND
               Sal_Cuenta.Cen_Costos  LE W_CC2     AND
               Sal_Cuenta.Cuenta      BEGINS "1110" AND
               Sal_Cuenta.Ano         EQ YEAR(W_Fec1) NO-LOCK:
          RUN Buscar_Cuentas (INPUT Sal_Cuenta.Cuenta).
          RUN HallarSdo(INPUT MONTH(W_Fec1), INPUT MONTH(W_Fec2), OUTPUT W_SdoAntBco, OUTPUT W_SdoActBco).
          ASSIGN W_TSdoAntBco = W_TSdoAntBco + W_SdoAntBco
                 W_TSdoActBco = W_TSdoActBco + W_SdoActBco.
      END.
      /**/
      FOR EACH Agencias FIELDS (Agencias.Agencia)
                        WHERE   Agencias.Agencia  GE W_Ag1
                        AND     Agencias.Agencia  LE W_Ag2 NO-LOCK, 
        EACH Comprobantes FIELDS (Comprobantes.Comprobante)
                            WHERE Comprobantes.Id_Efectivo   EQ TRUE 
                            AND   Comprobantes.Agencia       GE W_Ag1
                            AND   Comprobantes.Agencia       LE W_Ag2 NO-LOCK,
          EACH Mov_Contable FIELDS (Mov_Contable.Cuenta 
                                    Mov_Contable.Db Mov_Contable.Cr)
                            WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                            AND   Mov_Contable.Agencia       GE W_Ag1
                            AND   Mov_Contable.Agencia       LE W_Ag2 
                            AND   Mov_Contable.Cen_Costos    GE W_Cc1 
                            AND   Mov_Contable.Cen_Costos    LE W_Cc2 
                            AND   Mov_Contable.Fec_Contable  GT W_Fec1
                            AND   Mov_Contable.Fec_Contable  LE W_Fec2 NO-LOCK,
               EACH Cuentas FIELDS (Cuentas.Cod_FlujoEfec Cuentas.Cuenta Cuentas.Nombre)
                                    WHERE Cuentas.Cuenta     EQ Mov_Contable.Cuenta NO-LOCK:
     
         FIND TSCuentas WHERE TSCuentas.TS_CodFlu  EQ Cuentas.Cod_FlujoEfec
                         AND  TSCuentas.TS_Cuenta  EQ Mov_Contable.Cuenta NO-LOCK NO-ERROR.
                       
         IF NOT AVAILABLE(TScuentas) THEN
          DO:
            CREATE TSCuentas.
            ASSIGN TSCuentas.TS_CodFlu   = Cuentas.Cod_FlujoEfec
                   TSCuentas.TS_Cuenta   = Cuentas.Cuenta
                   TSCuentas.TS_Nombre   = Cuentas.Nombre.
          END.
          ASSIGN TSCuentas.TS_Db = TSCuentas.TS_Db + Mov_Contable.DB
                 TSCuentas.TS_Cr = TSCuentas.TS_Cr + Mov_Contable.CR.
    END.     
      
      /**/
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
      W_EncColumna = "CUENTA             NOMBRE                                        DEBITO             CREDITO".

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
    FOR EACH TScuentas NO-LOCK BREAK BY TSCuentas.TS_CodFlu
                                     BY TSCuentas.TS_Nat
                                     BY SUBSTRING(TSCuentas.TS_Cuenta,1,4)
                                     BY TSCuentas.TS_Cuenta:
      IF FIRST(TSCuentas.TS_CodFlu) THEN DO:
         DISPLAY "SALDO INICIAL CAJA    : " AT 1
                 W_TSdoAntCaj               AT 30 FORMAT "->>>,>>>,>>>,>>9"
                 "SALDO INICIAL BANCOS  : " AT 1
                 W_TSdoAntBco               AT 30 FORMAT "->>>,>>>,>>>,>>9"
         WITH FRAME FSdoIni WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
      END.
      IF FIRST-OF(TSCuentas.TS_CodFlu) THEN DO:         
          CASE TSCuentas.TS_CodFlu:
            WHEN "A" THEN
              DISPLAY "   ACTIVIDAD DE OPERACION:" @ TSCuentas.TS_Nombre 
              WITH FRAME F-Det1 USE-TEXT STREAM-IO NO-LABELS.
            WHEN "B" THEN
              DISPLAY "   ACTIVIDAD DE INVERSION:" @ TSCuentas.TS_Nombre
              WITH FRAME F-Det2 USE-TEXT STREAM-IO NO-LABELS.
            WHEN "C" THEN 
              DISPLAY "ACTIVIDAD DE FINANCIACION:" @ TSCuentas.TS_Nombre
              WITH FRAME F-Det3 USE-TEXT STREAM-IO NO-LABELS.
            WHEN "D" THEN 
              DISPLAY "    ACTIVIDAD DE EFECTIVO:" @ TSCuentas.TS_Nombre
              WITH FRAME F-Det4 USE-TEXT STREAM-IO NO-LABELS.
            OTHERWISE
              DISPLAY "        ACTIVIDAD (OTRAS):" @ TSCuentas.TS_Nombre
              WITH FRAME F-Det5 USE-TEXT STREAM-IO NO-LABELS.
          END CASE.             
      END.
      
      IF FIRST-OF(TSCuentas.TS_Nat) THEN DO:
         IF TSCuentas.TS_Nat EQ "DB" THEN 
           DISPLAY "                    USADO:" @ TSCuentas.TS_Nombre
           WITH FRAME F-Det6 NO-LABELS.
         ELSE
           DISPLAY "                 GENERADO:" @ TSCuentas.TS_Nombre
           WITH FRAME F-Det7 NO-LABELS.
      END.
      
      IF TSCuentas.TS_Db NE 0 OR TSCuentas.TS_Cr NE 0 THEN DO:
         ASSIGN W_SubCtaDb   = W_SubCtaDb   + TSCuentas.TS_Db
                W_SubtotalDb = W_SubTotalDb + TSCuentas.TS_Db
                W_SubCtaCr   = W_SubCtaCr   + TSCuentas.TS_Cr
                W_SubtotalCr = W_SubTotalCr + TSCuentas.TS_Cr.
         DISPLAY TSCuentas.TS_Cuenta AT 1
                 TSCuentas.TS_Nombre AT 20
                 TSCuentas.TS_Db     AT 60 FORMAT "->>>,>>>,>>>,>>9"
                 TSCuentas.TS_Cr     AT 80 FORMAT "->>>,>>>,>>>,>>9"
         WITH FRAME F-Detalle USE-TEXT STREAM-IO NO-LABELS NO-BOX WIDTH 132.
      END.  
      
      IF LAST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,4)) THEN DO:      
         FIND Cuentas WHERE Cuentas.Cuenta EQ SUBSTRING(TSCuentas.TS_Cuenta,1,4) NO-ERROR.   
         DISPLAY Cuentas.Cuenta               NO-LABEL     AT 1         
                 Cuentas.Nombre               NO-LABEL     AT 20 FORMAT "X(30)" 
                 W_SubCtaDb                   NO-LABEL     AT 60 FORMAT "->>>,>>>,>>>,>>9"
                 W_SubCtaCr                   NO-LABEL     AT 80 FORMAT "->>>,>>>,>>>,>>9"
         WITH FRAME F-Detalle2 USE-TEXT STREAM-IO NO-LABELS NO-BOX WIDTH 132.
         ASSIGN W_SubCtaDb = 0 W_SubCtaCr = 0.
      END.

      IF LAST-OF(TSCuentas.TS_Cuenta) THEN DO:
         IF TSCuentas.TS_Nat EQ "CR" THEN
            ASSIGN W_TotalCr = W_TotalCr - W_SubTotalCr.
         ELSE
            W_TotalDb = W_TotalDb + W_SubTotalDb. 
         DISPLAY "SUBTOTAL   :" AT 45
                 W_TotalDb      AT 60
                 W_TotalCr      AT 80
         WITH FRAME FSubt WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
         ASSIGN W_SubTotalDb = 0 W_SubTotalCr = 0.
      END.
      
      IF LAST-OF(TSCuentas.TS_CodFlu) THEN DO:                   
         DISPLAY "TOTAL NETO ACTIVIDAD  :" AT 30
                 W_TotalDb      AT 60
                 W_TotalCr      AT 80
         WITH FRAME FSubt2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
         ASSIGN W_TotalDb = 0 W_TotalCr = 0 W_SubTotalDb = 0 W_SubTotalCr = 0.
      END.  
      IF LAST(TSCuentas.TS_CodFlu) THEN
         DISPLAY "SALDO FINAL CAJA      : " AT 1
                 W_TSdoActCaj               AT 30
                 "SALDO FINAL BANCOS    : " AT 1
                 W_TSdoActBco               AT 30
         WITH FRAME FSdoIni WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
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

 
