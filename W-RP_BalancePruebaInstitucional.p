  DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Balance de Prueba Institucional Consolidado".
  /*para archivo de excel*/
  DEFINE VAR LisEx AS CHARACTER.
  DEFINE VAR i AS INTEGER.
  DEFINE VARIABLE XANO AS INTEGER.
  DEFINE VAR w_contodas AS LOGICAL INITIAL FALSE.
  DEFINE VAR Ct AS DECIMAL.
  DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".
  DEFINE TEMP-TABLE IEx
      FIELD NLinea AS INTEGER FORMAT "999999"
      FIELD Linea  AS CHARACTER FORMAT "X(150)".

  DEFINE TEMP-TABLE TMP_SALDO LIKE Sal_CtaCon.

  /**/

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.
  
  DEFINE VAR TotDeb  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotCre  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotActIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPasIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPtrIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotResIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotActFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPasFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotPtrFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  DEFINE VAR TotResFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
  

  DEFINE TEMP-TABLE TSCuentas NO-UNDO
      FIELD TS_Cuenta      AS CHARACTER FORMAT "X(14)"
      FIELD TS_Nombre      AS CHARACTER FORMAT "X(25)"
      FIELD TS_Nivel       LIKE Cuentas.Nivel
      FIELD TS_Nat         LIKE Cuentas.Naturaleza
      FIELD TS_Ini         AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
      FIELD TS_Db          AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
      FIELD TS_Cr          AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
      FIELD TS_Fin         AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
      FIELD Niv1           AS CHARACTER FORMAT "X(6)"
      FIELD Niv2           AS CHARACTER FORMAT "X(6)"
      FIELD Niv3           AS CHARACTER FORMAT "X(6)"
      FIELD Niv4           AS CHARACTER FORMAT "X(6)"
      INDEX idxcta ts_cuenta ts_nivel
      INDEX idxcta1  ts_cuenta .

/* incluido de Pantalla con parametros*/
{incluido\Pantalla_Validacion3.i}    
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
      DEFINE VARIABLE SIni LIKE Sal_CtaCon.Sal_Inicial INITIAL 0.
      DEFINE VARIABLE SDb  LIKE Sal_CtaCon.Sal_Inicial.
      DEFINE VARIABLE SCr  LIKE Sal_CtaCon.Sal_Inicial.
      DEFINE VARIABLE SFin LIKE Sal_CtaCon.Sal_Inicial INITIAL 0.
      DEFINE VAR i AS INTEGER.
      DEFINE VAR MCta AS CHARACTER FORMAT "X(14)".
      DEFINE VAR MDb  LIKE Sal_CtaCon.Sal_Inicial.
      DEFINE VAR MCr  LIKE Sal_CtaCon.Sal_Inicial.
      DEFINE VAR MIni LIKE Sal_CtaCon.Sal_Inicial.
      DEFINE VAR MFin LIKE Sal_CtaCon.Sal_Inicial.
      DEFINE VAR MNt  LIKE Cuentas.Naturaleza.
      
      MCta = tmp_saldo.Cuenta.
      RUN HallarSdo (INPUT MONTH(W_Fec1), OUTPUT SIni, OUTPUT SFin).
      FIND TSCuentas WHERE TSCuentas.TS_Cuenta EQ Cuentas.Cuenta NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(TSCuentas) THEN
         RUN Grabar_Enc_Temporal.
      ASSIGN TSCuentas.TS_Ini  = TSCuentas.TS_Ini  + SIni
             MINI              = TSCuentas.TS_Ini
             TSCuentas.TS_Db   = TSCuentas.TS_Db   + tmp_saldo.Db[MONTH(W_Fec1)]
             MDb               = TSCuentas.TS_Db
             TSCuentas.TS_Cr   = TSCuentas.TS_Cr   + tmp_saldo.Cr[MONTH(W_Fec1)]
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
         ASSIGN TSCuentas.TS_Db = TSCuentas.TS_DB + tmp_saldo.Db[MONTH(W_Fec1)] /*MDb*/
                TSCuentas.TS_Cr = TSCuentas.TS_CR + tmp_saldo.Cr[MONTH(W_Fec1)]. /*MCr.*/
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
           TSCuentas.TS_Nat     = Cuentas.Naturaleza
           TSCuentas.Niv1       = SUBSTRING(TSCuentas.TS_Cuenta,1,1)
           TSCuentas.Niv2       = SUBSTRING(TSCuentas.TS_Cuenta,1,2)
           TSCuentas.Niv3       = SUBSTRING(TSCuentas.TS_Cuenta,1,4)
           TSCuentas.Niv4       = SUBSTRING(TSCuentas.TS_Cuenta,1,6).
 END PROCEDURE.

  PROCEDURE Buscar_Cuentas:
      DEFINE INPUT PARAMETER Cta LIKE Cuentas.Cuenta.
      FIND Cuentas WHERE Cuentas.Cuenta EQ Cta AND 
                         Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  END PROCEDURE.

  PROCEDURE HallarSdo:
    DEFINE INPUT  PARAMETER Smes AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini LIKE Sal_CtaCon.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SFin LIKE Sal_CtaCon.Sal_Inicial.
    DEFINE VAR i AS INTEGER.
    SFin = tmp_saldo.Sal_Inicial.
    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
           ASSIGN SFin  = SFin + tmp_saldo.DB[i] - tmp_saldo.Cr[i]
                  SIni  = SFin - tmp_saldo.DB[i] + tmp_saldo.Cr[i].
        ELSE
           ASSIGN SFin  = SFin - tmp_saldo.DB[i] + tmp_saldo.Cr[i]
                  SIni  = SFin + tmp_saldo.DB[i] - tmp_saldo.Cr[i].
         IF i = smes THEN
            ASSIGN TotDeb = TotDeb + tmp_saldo.DB[i]
                   TotCre = TotCre +  tmp_saldo.CR[i].
        /*ASSIGN TotDeb = TotDeb + ROUND(Sal_CtaCon.DB[i],0)
               TotCre = TotCre + ROUND(Sal_CtaCon.CR[i],0).*/
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
      ASSIGN W_Fec1:LABEL IN FRAME F_Valida = "Fecha de Corte"
             W_Fec2:LABEL IN FRAME F_Valida = "Fecha del Día".
      IF NOT L_CC THEN
         DISABLE Cmb_CenCost WITH FRAME F_Valida.
  END PROCEDURE.

  PROCEDURE Tabla_Temporal:  /* TT */
  DEFINE VARIABLE ki AS INTEGER INITIAL 1.
  SESSION:SET-WAIT-STATE("GENERAL").
  EMPTY TEMP-TABLE TMP_saldo.
  ASSIGN XANO = YEAR(W_Fec1).
    IF W_ag1 = W_ag2 THEN DO:
      FOR EACH Sal_CtaCon WHERE
             Sal_CtaCon.Agencia     EQ W_Ag1     AND
             Sal_CtaCon.Ano         EQ XANO NO-LOCK:


          IF Sal_CtaCon.Cen_Costos  EQ 999       AND
             Sal_CtaCon.Cuenta      GE W_Cuenta1 AND
             Sal_CtaCon.Cuenta      LE W_Cuenta2 THEN DO: 
             
             FIND FIRST TMP_SALDO WHERE TMP_SALDO.CUENTA EQ Sal_CtaCon.cuenta NO-ERROR.
             IF NOT AVAILABLE(tmp_saldo) THEN do:
                 CREATE tmp_saldo.
                 BUFFER-COPY Sal_CtaCon TO tmp_saldo.
             END.
             ELSE DO:
                 DO Ki = 1 TO 12:
                    ASSIGN tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + Sal_CtaCon.Cr[Ki]  
                         tmp_saldo.db[Ki]   = tmp_saldo.db[Ki] + Sal_CtaCon.db[Ki].
                 END.
                  ASSIGN tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + Sal_CtaCon.sal_inicial.
             END.
          END.
      END.
    END. /* fin ciclo una */
    ELSE DO:
       FOR EACH agencias NO-LOCK:
         FOR EACH Sal_CtaCon WHERE Sal_CtaCon.Agencia EQ Agencias.agencia AND 
                                   Sal_CtaCon.Ano  EQ XANO NO-LOCK:
           IF Sal_CtaCon.Cuenta GE W_Cuenta1 AND
              Sal_CtaCon.Cuenta  LE W_Cuenta2
              THEN DO:
                 FIND FIRST TMP_SALDO WHERE TMP_SALDO.CUENTA EQ Sal_CtaCon.cuenta NO-ERROR.
             IF NOT AVAILABLE(tmp_saldo) THEN do:
                 CREATE tmp_saldo.
                 BUFFER-COPY Sal_CtaCon TO tmp_saldo.
             END.
             ELSE DO:
                 DO Ki = 1 TO 12:
                    ASSIGN tmp_saldo.Cr[Ki] = tmp_saldo.Cr[Ki] + Sal_CtaCon.Cr[Ki]  
                           tmp_saldo.db[Ki] = tmp_saldo.db[Ki] + Sal_CtaCon.db[Ki].
                 END.
                  ASSIGN tmp_saldo.sal_inicial = tmp_saldo.sal_inicial + Sal_CtaCon.sal_inicial.
             END.

           END.
         END. /* for each */
       END.
     END.
    /* GENERA INFORME */
    FOR EACH TMP_SALDO NO-LOCK:
        RUN Buscar_Cuentas (INPUT TMP_Saldo.Cuenta).
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
      IF w_contodas THEN
        Listado = w_Pathspl + "Blce" + STRING(DAY(W_Fec1),"99") + STRING(MONTH(W_Fec1),"99") + STRING(YEAR(W_Fec1),"9999") + "_Ag" + TRIM(STRING(w_ag1,"999")) + "_Usu" + TRIM(w_usuario).
      ELSE
        Listado = w_Pathspl + "Blce_" + TRIM(w_usuario) + TRIM(STRING(RANDOM(2000, 100000))) + ".lst".
     {incluido\IMPRIMIR_carta_ag.I "listado"}.
  END PROCEDURE.

  PROCEDURE ProcesoImprimir:
     {Incluido\RepEncabezado.i}
      DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
      DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".
      IF SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,1,3)  = "000" THEN 
           W_Reporte   = "REPORTE   : BALANCE DE INSTITUCIONAL CONSOLIDADO - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(20)")
                        + " al " + STRING(W_Fec1) + " Sacado el: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      ELSE DO:
          FIND FIRST agencias WHERE agencia.agencia = w_ag1 NO-LOCK NO-ERROR.
          W_Reporte   = "REPORTE   : BALANCE DE PRUEBA institucional- Agencia: " +  STRING(STRING(agencias.agencia) + " - " + Agencias.nombre,"X(20)") 
                       + " al " + STRING(W_Fec1) + " Sacado el: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      END.
      W_EncColumna = "CUENTA         NOMBRE                     SALDO INICIAL      DEBITO             CREDITO            SALDO FINAL       NAT".

      DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".

      DEFINE VAR  TT_Ini AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
      DEFINE VAR  TT_Db  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
      DEFINE VAR  TT_Fin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
      
      DEFINE VAR TotIni AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>9.99".
      DEFINE VAR TotFin AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>9.99".

      DEFINE FRAME F_Mov
        TSCuentas.TS_Cuenta     
        TSCuentas.TS_Nombre     
        TSCuentas.TS_Ini        
        TSCuentas.TS_Db         
        TSCuentas.TS_Cr         
        TSCuentas.TS_Fin         
        TSCuentas.TS_Nat        
      WITH DOWN WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
      VIEW FRAME F-Encabezado.
      VIEW FRAME f-ftr.
      FOR EACH IEx: DELETE IEx. END.
      CREATE IEx.
      ASSIGN Ct = Ct + 1
             IEx.NLinea = Ct
             IEx.Linea  = "CUENTA" + Cma + "NOMBRE" + Cma + 
                          "INICIAL" + Cma + "DEBITO" + Cma +
                          "CREDITO" + Cma + "FINAL" + Cma + "NAT".
      FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel 
                         BREAK BY TSCuentas.TS_Cuenta BY TSCuentas.TS_Nivel:
                          /*BREAK BY Niv1  BY Niv2 BY Niv3  BY Niv4
                               BY TSCuentas.TS_Cuenta BY TSCuentas.TS_Nivel:*/

          IF TS_Cuenta EQ " " THEN NEXT.
  
         IF TSCuentas.TS_Ini NE 0 OR TSCuentas.TS_Db  NE 0 
         OR TSCuentas.TS_CR  NE 0 OR TSCuentas.TS_Fin NE 0 THEN DO:
            IF SUBSTRING(TSCuentas.TS_Cuenta,2,1) EQ " " THEN                       
               DISPLAY SKIP(1) WITH FRAME F_Mov5 USE-TEXT STREAM-IO NO-LABELS NO-BOX. 

            IF SUBSTRING(TSCuentas.TS_Cuenta,3,1) EQ " " AND SUBSTRING(TSCuentas.TS_Cuenta,2,1) NE " " THEN                       
               DISPLAY SKIP(1) WITH FRAME F_Mov6 USE-TEXT STREAM-IO NO-LABELS NO-BOX. 

             IF SUBSTRING(TSCuentas.TS_Cuenta,4,1) NE " " AND SUBSTRING(TSCuentas.TS_Cuenta,5,1) EQ " " THEN                       
                 DISPLAY SKIP WITH FRAME F_Mov6 USE-TEXT STREAM-IO NO-LABELS NO-BOX.

            DISPLAY                                                 
                TSCuentas.TS_Cuenta                     
                TSCuentas.TS_Nombre                     
                TSCuentas.TS_Ini                        
                TSCuentas.TS_Db                         
                TSCuentas.TS_Cr                         
                TSCuentas.TS_Fin                               
                TSCuentas.TS_Nat    SKIP(0)                    
              WITH WIDTH 132 FRAME F_Mov USE-TEXT STREAM-IO NO-LABELS NO-BOX.

              CREATE IEx.
              ASSIGN Ct = Ct + 1
                     IEx.NLinea = Ct
                     IEx.Linea  = STRING(TSCuentas.TS_Cuenta) + Cma +
                                  STRING(TSCuentas.TS_Nombre) + Cma +
                                  STRING(TSCuentas.TS_Ini,"->>>>>>>>>>>>.99") + Cma +
                                  STRING(TSCuentas.TS_Db,"->>>>>>>>>>>>.99") + Cma +
                                  STRING(TSCuentas.TS_Cr,"->>>>>>>>>>>>.99") + Cma +
                                  STRING(TSCuentas.TS_Fin,"->>>>>>>>>>>>.99") + Cma +
                                  STRING(TSCuentas.TS_Nat).    
             
         END.


          
        /* IF FIRST-OF(SUBSTRING(TSCuentas.TS_Cuenta,1,4)) THEN 
            DISPLAY SKIP WITH FRAME F_Mov3 USE-TEXT STREAM-IO NO-LABELS NO-BOX.  
                */
      END.  
    
      ASSIGN TotIni = TotActIni - (TotPasIni + TotPtrIni) + TotResIni
             TotFin = ROUND((TotActFin - (TotPasFin + TotPtrFin) + TotResFin),0).

      DISPLAY SKIP(1)
              "Totales de Control: " AT 1
              TotIni AT 38
              TotDeb AT 58
              TotCre AT 78 
              TotFin AT 97 WITH FRAME T_Tot WIDTH 132 NO-LABELS STREAM-IO USE-TEXT NO-BOX.
      PAGE.
    OUTPUT CLOSE.
  END PROCEDURE.

  PROCEDURE Imprimir_Excel:
      LisEx = w_Pathspl + "BcePrueba.csv".
      OUTPUT TO VALUE(LisEx).
      FOR EACH IEx BY IEx.NLinea:
          PUT IEx.Linea SKIP.
      END.
      OUTPUT CLOSE.
      MESSAGE "Balance de Prueba para Excel se encuentra en:" SKIP
              LisEx VIEW-AS ALERT-BOX INFORMATION.
      FOR EACH IEx: DELETE IEx. END.
/*      {Incluido\Def_Excel.i}
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
                        + "015" + STRING(TSCuentas.TS_Ini,"->>,>>>,>>>,>>9.99")
                        + "015" + STRING(TSCuentas.TS_Db,"->>,>>>,>>>,>>9.99")
                        + "015" + STRING(TSCuentas.TS_Cr,"->>,>>>,>>>,>>9.99")
                        + "015" + STRING(TSCuentas.TS_Fin,"->>,>>>,>>>,>>9.99").
            {Incluido\imprimir_Excel.i} */      
  END PROCEDURE.
