  DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Balance de Prueba".
  /*para archivo de excel*/
  DEFINE VAR LisEx AS CHARACTER.
  DEFINE VAR vcNomAr AS CHARACTER FORMAT "X(50)".
  DEFINE VAR w_contodas AS LOGICAL INITIAL FALSE.
  DEFINE VAR Ct AS DECIMAL.
  DEFINE VAR vdCt AS DECIMAL INITIAL 0.
  DEFINE VAR XANO LIKE sal_ctacon.ANO.
  DEFINE var wage   LIKE clientes.agencia.
  DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".
  DEFINE TEMP-TABLE IEx
      FIELD NLinea AS INTEGER FORMAT "999999"
      FIELD Linea  AS CHARACTER FORMAT "X(150)".
  DEFINE TEMP-TABLE TTIEx
      FIELD NLinea AS INTEGER FORMAT "999999"
      FIELD Linea  AS CHARACTER FORMAT "X(150)".
  DEFINE VAR Listado AS CHARACTER INITIAL "".
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
  

  DEFINE TEMP-TABLE TSCuentas
      FIELD TS_AGENCIAS    LIKE clientes.agencia
      FIELD TS_Cuenta      AS CHARACTER FORMAT "X(14)"
      FIELD TS_Nombre      AS CHARACTER FORMAT "X(30)"
      FIELD TS_Nivel       LIKE Cuentas.Nivel
      FIELD TS_Nat         LIKE Cuentas.Naturaleza
      FIELD TS_Ini         AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"
      FIELD TS_Db          AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"
      FIELD TS_Cr          AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"
      FIELD TS_Fin         AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"
      INDEX IDX1 TS_CUENTA.

/* incluido de Pantalla con parametros*/
{incluido/Pantalla_Validacion4.i}    
    /* Listado = w_Pathspl + "Blce_" + TRIM(w_usuario) + TRIM(STRING(RANDOM(2000, 100000))) + ".lst". 
    OUTPUT TO VALUE(listado). */
        
/* fin incluido Pantalla parametros */


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

      FOR EACH TTIEx: DELETE TTIEx. END.
      CREATE TTIEx.
      ASSIGN vdCt = vdCt + 1
             TTIEx.NLinea = vdCt
             TTIEx.Linea  = "AGENCIA" + Cma + "CUENTA" + Cma + "NOMBRE" + Cma + 
                            "INICIAL" + Cma + "DEBITO" + Cma +
                            "CREDITO" + Cma + "FINAL" + Cma + "NAT" + Cma.

      ASSIGN vcNomAr = "C:\info_juriscoop\Blce" + STRING(W_fecha,"99999999") + "_Ag999_Usu" + W_Usuario + ".csv".
  END PROCEDURE.

  PROCEDURE Proceso_Imprimir:
      /* DEFINE VAR Listado AS CHARACTER INITIAL "". */
      FOR EACH TSCuentas: DELETE TSCuentas. END.
      ASSIGN TotCre = 0 TotDeb = 0 TotActIni = 0 TotActFin = 0 TotPasIni = 0
             TotPasFin = 0 TotPtrIni = 0 TotPtrFin = 0 TotResIni = 0 TotResFin = 0.
      RUN Tabla_Temporal.
      RUN exparcxls.
      /*IF w_contodas THEN DO:
          Listado = w_Pathspl + "Blce" + STRING(DAY(W_Fec1),"99") + STRING(MONTH(W_Fec1),"99") + STRING(YEAR(W_Fec1),"9999") + "_Ag" + TRIM(STRING(w_ag1,"999")) + "_Usu" + TRIM(w_usuario).
          OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 85.
      END.
      ELSE 
          Listado = w_Pathspl + "Blce_" + TRIM(w_usuario) + TRIM(STRING(RANDOM(2000, 100000))) + ".lst".*/
      {incluido\IMPRIMIR_carta_archivo.I "listado"}..
  END PROCEDURE.

  PROCEDURE Tabla_Temporal:  /* TT */
      SESSION:SET-WAIT-STATE("GENERAL").
      XANO = YEAR(W_Fec1).
      IF w_ag1 = w_ag2 THEN  DO: 
          FOR EACH sal_ctacon WHERE
              sal_ctacon.Agencia     EQ W_Ag1     AND
              sal_ctacon.Cen_Costos  EQ 999     AND
              sal_ctacon.Ano         EQ XANO  NO-LOCK:
              IF sal_ctacon.Cuenta      GE W_Cuenta1 AND
                 sal_ctacon.Cuenta      LE W_Cuenta2 THEN DO:

                  RUN Buscar_Cuentas (INPUT sal_ctacon.Cuenta).
                  RUN Mayorizar.
              END.
          END.
      END.
      ELSE DO:
        DEFINE VARIABLE wxano AS INTEGER.
        wxano = YEAR(W_Fec1).
        FOR EACH agencias:
          FOR EACH sal_ctacon WHERE
              sal_ctacon.Agencia     GE agencias.agencia  AND
              sal_ctacon.Ano         EQ WXANO NO-LOCK:
              
              IF sal_ctacon.Cuenta      GE W_Cuenta1 AND
                 sal_ctacon.Cuenta      LE W_Cuenta2 THEN do: 
                 RUN Buscar_Cuentas (INPUT sal_ctacon.Cuenta).
                 RUN Mayorizar.
              END.
          END.
        END. /* agencia */
      END.
      SESSION:SET-WAIT-STATE("").
  END PROCEDURE.




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
      DEFINE VARIABLE SIni LIKE sal_ctacon.Sal_Inicial INITIAL 0.
      DEFINE VARIABLE SDb  LIKE sal_ctacon.Sal_Inicial.
      DEFINE VARIABLE SCr  LIKE sal_ctacon.Sal_Inicial.
      DEFINE VARIABLE SFin LIKE sal_ctacon.Sal_Inicial INITIAL 0.
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
    ASSIGN TSCuentas.TS_agencia = wage
           TSCuentas.TS_Cuenta  = Cuentas.Cuenta
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
         IF i = smes THEN
            ASSIGN TotDeb = TotDeb + sal_ctacon.DB[i]
                   TotCre = TotCre +  sal_ctacon.CR[i].
        /*ASSIGN TotDeb = TotDeb + ROUND(sal_ctacon.DB[i],0)
               TotCre = TotCre + ROUND(sal_ctacon.CR[i],0).*/
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
    wage = sal_ctacon.agencia.
  END PROCEDURE.



  PROCEDURE ProcesoImprimir:
     {Incluido\RepEncabezado.i} 
      DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
      DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".
/*      IF SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,1,3)  = "000" THEN 
           W_Reporte   = "REPORTE   : BALANCE DE PRUEBA - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(20)")
                        + " al " + STRING(W_Fec1) + " Sacado el: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      ELSE DO:
          FIND FIRST agencias WHERE agencia.agencia = w_ag1 NO-LOCK NO-ERROR.
          W_Reporte   = "REPORTE   : BALANCE DE PRUEBA - Agencia: " +  STRING(STRING(agencias.agencia) + " - " + Agencias.nombre,"X(20)") 
                       + " al " + STRING(W_Fec1) + " Sacado el: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      END.
      W_EncColumna = "CUENTA         NOMBRE                    NAT   SALDO INICIAL      DEBITO            CREDITO     SALDO FINAL". */
      DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".

      DEFINE VAR  TT_Ini AS DECIMAL FORMAT "->>>>>>>>>>>>9.99".
      DEFINE VAR  TT_Db  AS DECIMAL FORMAT "->>>>>>>>>>>>9.99".
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT "->>>>>>>>>>>>9.99".
      DEFINE VAR  TT_Fin AS DECIMAL FORMAT "->>>>>>>>>>>>9.99".
      
      DEFINE VAR TotIni AS DECIMAL FORMAT "->>>>>>>>>>>>9.99".
      DEFINE VAR TotFin AS DECIMAL FORMAT "->>>>>>>>>>>>9.99".

      DEFINE FRAME F_Mov
        TSCuentas.TS_Agencia
        TSCuentas.TS_Cuenta AT 10    
        TSCuentas.TS_Nombre     
        TSCuentas.TS_Ini        
        TSCuentas.TS_Db         
        TSCuentas.TS_Cr 
        TSCuentas.TS_Fin         
        TSCuentas.TS_Nat        
      WITH DOWN WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
 /*   VIEW FRAME F-Encabezado.
      VIEW FRAME f-ftr.  */
/*       FOR EACH IEx: DELETE IEx. END.                                            */
/*       CREATE TTIEx.                                                             */
/*       ASSIGN vdCt = vdCt + 1                                                    */
/*              TTIEx.NLinea = vdCt                                                */
/*              TTIEx.Linea  = "AGENCIA" + Cma + "CUENTA" + Cma + "NOMBRE" + Cma + */
/*                             "INICIAL" + Cma + "DEBITO" + Cma +                  */
/*                             "CREDITO" + Cma + "FINAL" + Cma + "NAT".            */
      FOR EACH TSCuentas WHERE TSCuentas.TS_Nivel LE Cmb_Nivel AND TS_Cuenta NE " "
          BREAK BY SUBSTRING(TSCuentas.TS_Cuenta,1,1)  BY SUBSTRING(TSCuentas.TS_Cuenta,1,2)
                BY SUBSTRING(TSCuentas.TS_Cuenta,1,4)  BY SUBSTRING(TSCuentas.TS_Cuenta,1,6)
                BY TSCuentas.TS_Cuenta BY TSCuentas.TS_Nivel:

         IF TSCuentas.TS_Ini NE 0 OR TSCuentas.TS_Db  NE 0 
         OR TSCuentas.TS_CR  NE 0 OR TSCuentas.TS_Fin NE 0 THEN DO:
            IF SUBSTRING(TSCuentas.TS_Cuenta,2,1) EQ " " THEN                       
               DISPLAY SKIP(0) WITH FRAME F_Mov5 USE-TEXT STREAM-IO NO-LABELS NO-BOX. 

            IF SUBSTRING(TSCuentas.TS_Cuenta,3,1) EQ " " AND SUBSTRING(TSCuentas.TS_Cuenta,2,1) NE " " THEN                       
               DISPLAY SKIP(0) WITH FRAME F_Mov6 USE-TEXT STREAM-IO NO-LABELS NO-BOX. 

             IF SUBSTRING(TSCuentas.TS_Cuenta,4,1) NE " " AND SUBSTRING(TSCuentas.TS_Cuenta,5,1) EQ " " THEN                       
                 DISPLAY SKIP WITH FRAME F_Mov6 USE-TEXT STREAM-IO NO-LABELS NO-BOX.

            DISPLAY   
                TSCuentas.TS_Agencia
                TSCuentas.TS_Cuenta                     
                TSCuentas.TS_Nombre                     
                TSCuentas.TS_Ini                        
                TSCuentas.TS_Db                         
                TSCuentas.TS_Cr                        
                TSCuentas.TS_Fin                               
                TSCuentas.TS_Nat    SKIP(0)                    
              WITH WIDTH 132 FRAME F_Mov USE-TEXT STREAM-IO NO-LABELS NO-BOX.

              CREATE TTIEx.
              ASSIGN vdCt = vdCt + 1
                     TTIEx.NLinea = vdCt
                     TTIEx.Linea  = STRING(TSCuentas.TS_Agencia) + Cma +
                                    STRING(TSCuentas.TS_Cuenta)  + Cma +
                                    STRING(TSCuentas.TS_Nombre)  + Cma +
                                    STRING(TSCuentas.TS_Ini,"->>>>>>>>>>>>.99") + Cma +
                                    STRING(TSCuentas.TS_Db,"->>>>>>>>>>>>.99") + Cma +
                                    STRING(TSCuentas.TS_Cr,"->>>>>>>>>>>>.99") + Cma + 
                                    STRING(TSCuentas.TS_Fin,"->>>>>>>>>>>>.99") + Cma +
                                    STRING(TSCuentas.TS_Nat).    
             
         END.

      END.  
    
      ASSIGN TotIni = TotActIni - (TotPasIni + TotPtrIni) + TotResIni
             TotFin = ROUND((TotActFin - (TotPasFin + TotPtrFin) + TotResFin),0).
    /*
      DISPLAY SKIP(0)
              "Totales de Control: " AT 1
              TotIni AT 43
            /*  TotDeb AT 60
              TotCre AT 78  */
              TotFin AT 60 WITH FRAME T_Tot WIDTH 132 NO-LABELS STREAM-IO USE-TEXT NO-BOX. */
      PAGE.
    OUTPUT CLOSE.
  END PROCEDURE.

/*   PROCEDURE Imprimir_Excel:                */
/*       LisEx = w_Pathspl + "BcePrueba.csv". */
/*       OUTPUT TO VALUE(LisEx).              */
/*       FOR EACH IEx BY IEx.NLinea:          */
/*           PUT IEx.Linea SKIP.              */
/*       END.                                 */
/*       OUTPUT CLOSE.                        */

  PROCEDURE exparcxls:
      
      OUTPUT TO VALUE (vcNomAr).
      FOR EACH TTIEx.
          PUT TTIEx.Linea SKIP.
      END.
      OUTPUT CLOSE.
  END PROCEDURE.





/*       MESSAGE "Balance de Prueba para Excel se encuentra en:" SKIP */
/*               LisEx VIEW-AS ALERT-BOX INFORMATION.                 */
/*       FOR EACH IEx: DELETE IEx. END.                               */
/*   END PROCEDURE.                                                   */
