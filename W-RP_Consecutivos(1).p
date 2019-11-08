  /*recorre tabla de anexos y hace informe igual a balance prueba pero 
    discriminando por nit*/
  DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Informe de Consecutivos de Comprobantes".
  /*para archivo de excel*/
  DEFINE VAR LisEx AS CHARACTER.
  DEFINE VAR Ct AS DECIMAL.
  DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".
  DEFINE TEMP-TABLE IEx
      FIELD NLinea AS INTEGER FORMAT "999999"
      FIELD Linea  AS CHARACTER FORMAT "X(150)".
  /**/

/* {incluido/Variable.i "SHARED"}.  */
/* {incluido/Varcon.i "SHARED"}.    */
/** Temporales*/
  DEFINE VARIABLE W_Usuario   LIKE usuarios.usuario       INITIAL "308". /* 308 - Contabiliza*/
  DEFINE VARIABLE W_Fecha     AS DATE   INITIAL TODAY.
  DEFINE VARIABLE W_PathSpl   AS CHARACTER FORMAT "X(20)" INITIAL "c:\info_juriscoop\".
  DEFINE VARIABLE W_Agencia   LIKE Agencia.Agencia        INITIAL "035". /*"035".*/

    DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion.
/*     DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario. */
    DEFINE {1} VAR W_Clave       LIKE Usuarios.Clave FORMAT "X(16)".
    DEFINE {1} VAR W_Prioridad   LIKE Usuarios.Prioridad INITIAL "".
/*     DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia INITIAL 0.  */
    DEFINE {1} VAR W_Ciudad      LIKE Agencia.Ciudad INITIAL 0.
    DEFINE {1} VAR W_Nom_Agencia   AS CHARACTER FORMAT "X(60)".
    DEFINE {1} VAR W_UbiDatos      AS CHAR INITIAL "D".
    /*DEFINE {1} VAR W_Ninv        LIKE Inversion.Nro_inversion.*/
    DEFINE {1} VAR W_Nom_Entidad   AS CHARACTER FORMAT "X(60)". 
    DEFINE {1} VAR W_Entidad     LIKE Entidad.Entidad INITIAL 1.
    DEFINE {1} VAR W_NitGlobal   LIKE Clientes.Nit INITIAL "".
    DEFINE {1} VAR W_SMLV        LIKE Indicadores.Valor INITIAL 0.
    DEFINE {1} VAR W_Manija        AS HANDLE.
    DEFINE {1} VAR W_ManFin        AS HANDLE.
    DEFINE {1} VAR W_ManTaq        AS HANDLE.
    DEFINE {1} VAR W_Nivel       LIKE Cuentas.Nivel.
    DEFINE {1} VAR W_CtaMay      LIKE Cuentas.Cuenta.
/*     DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.  */
    DEFINE {1} VAR W_ficina        AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1.
    DEFINE {1} VAR W_Path        LIKE Entidad.Dir_Programas.
/*     DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl.  */
    DEFINE {1} VAR W_Eleccion      AS LOGICAL.
    DEFINE {1} VAR W_CedGral     LIKE Clientes.Nit.
    DEFINE {1} VAR W_CenCosGral  LIKE Cen_Costos.Cen_Costos.
    DEFINE {1} VAR W_Cadena        AS CHARACTER FORMAT "X(9)" INITIAL "SIFINCOOP".
    /*DEFINE     VAR Agencia_Cnt     AS INTEGER FORMAT "999".*/
    DEFINE {1} VAR P-Valida        AS LOGICAL.
    DEFINE {1} VAR W_VCodPcto    LIKE Ahorros.Cod_Ahorro.
    DEFINE {1} VAR W_VCueAhorro  LIKE Ahorros.Cue_Ahorros.
    DEFINE {1} VAR W_Solicitud   LIKE Solicitud.Num_Solicitud.
    DEFINE {1} VAR W_PagareS     LIKE Creditos.Pagare.
    DEFINE {1} VAR P_SdoTot      LIKE Creditos.Sdo_Capital.
    DEFINE {1} VAR W_OfiCierre   LIKE Agencias.Agencia.

  /********************************************************************************** 
   Variables globales contables
**********************************************************************************/
  
  DEFINE {1} VAR W_ManCon AS HANDLE.
  DEFINE {1} VAR W_FecIni AS DATE FORMAT "99/99/9999" INITIAL TODAY.
  DEFINE {1} VAR W_FecFin AS DATE FORMAT "99/99/9999" INITIAL TODAY.
  DEFINE {1} VAR W_Mes    AS INTEGER FORMAT "99".
  DEFINE {1} VAR W_MesFin AS INTEGER FORMAT "99".
  DEFINE {1} VAR W_UsuFin LIKE Usuarios.Usuario.
  DEFINE {1} VAR W_UsuIni LIKE Usuarios.Usuario.
  DEFINE {1} VAR W_CtaIni LIKE Cuentas.Cuenta INITIAL "".
  DEFINE {1} VAR W_CtaFin LIKE Cuentas.Cuenta INITIAL "".
  DEFINE {1} VAR W_ComTra LIKE Comprobantes.Comprobante INITIAL 0.
  DEFINE {1} VAR W_ComIni LIKE Comprobantes.Comprobante INITIAL 0.
  DEFINE {1} VAR W_ComFin LIKE Comprobantes.Comprobante INITIAL 0.  
  DEFINE {1} VAR W_CenTra LIKE Cen_Costos.Cen_Costos    INITIAL 0.
  DEFINE {1} VAR W_OfiIni LIKE Agencias.Agencia         INITIAL 0.
  DEFINE {1} VAR W_OfiFin LIKE Agencias.Agencia         INITIAL 0.
  DEFINE {1} VAR W_CenIni LIKE Cen_Costos.Cen_Costos    INITIAL 0.
  DEFINE {1} VAR W_CenFin LIKE Cen_Costos.Cen_Costos    INITIAL 0.  
  DEFINE {1} VAR W_NitIni LIKE Terceros.Nit INITIAL "".
  DEFINE {1} VAR W_NitFin LIKE Terceros.Nit INITIAL "".
  DEFINE {1} VAR W_BaseInf  AS DECIMAL INITIAL 0 FORMAT "->>>,>>>,>>>,>>9".
  DEFINE {1} VAR W_Ope      AS CHARACTER FORMAT "X(13)"  VIEW-AS COMBO-BOX 
        LIST-ITEMS "Sin Seleccion","Mayor Que","Menor Que","Igual" SIZE 13 BY 4.5.
  DEFINE {1} VAR TotDctoDeb AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotDctoCre AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotGralDeb AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotGralCre AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR VlrDeb     AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR VlrCre     AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotOfCre   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotOfDeb   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotCoCre   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotCoDeb   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR W_SaldoAct AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99" .
  DEFINE {1} VAR W_SaldoAnt AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99" .  
  DEFINE {1} VAR W_PorReex  AS DECIMAL INITIAL 0 FORMAT "-ZZ9.99".
  DEFINE {1} VAR W_Raya1    AS CHAR    INITIAL "" FORMAT "X(130)".
  DEFINE {1} VAR W_Raya2    AS CHAR    INITIAL "" FORMAT "X(130)".
  DEFINE {1} VAR W_NomMes   AS CHAR    INITIAL "" FORMAT "X(20)".
  DEFINE {1} VAR W_NomCta   AS CHAR    INITIAL "".        
  DEFINE {1} VAR W_Opcion   AS CHAR    INITIAL "P".
  DEFINE {1} VAR W_Destino  AS CHAR    INITIAL "".
  DEFINE {1} VAR W_NomOfi LIKE Agencias.Nombre.
  DEFINE {1} VAR W_NomCen LIKE Cen_Costos.Nombre INITIAL "Consolidado".
  DEFINE {1} VAR W_NomCom LIKE Comprobantes.Nombre INITIAL "Consolidado".
  DEFINE {1} VAR W_NomIni LIKE Cuentas.Nombre.    
  DEFINE {1} VAR W_NomFin LIKE Cuentas.Nombre.
  DEFINE {1} VAR W_NoInNit  AS CHAR FORMAT "X(40)" INITIAL "".
  DEFINE {1} VAR W_NoFiNit  AS CHAR FORMAT "X(40)" INITIAL "".
  DEFINE {1} VAR W_Rtipo    AS INTEGER INITIAL 1 LABEL  "Tipo Informe " VIEW-AS RADIO-SET HORIZONTAL
                        RADIO-BUTTONS "Detallado/Nit",1,"Resumido/Cuenta",2 SIZE 30 BY 0.81.
  DEFINE {1} VAR W_Ret    AS INTEGER INITIAL 1 VIEW-AS RADIO-SET HORIZONTAL
                        RADIO-BUTTONS "Iva",1,"Retencion fuente",2,"Pagos a Terceros", 3 SIZE 40 BY 0.71.
  DEFINE {1} VAR W_RForma   AS INTEGER INITIAL 0 LABEL "Formato       " VIEW-AS RADIO-SET HORIZONTAL
                        RADIO-BUTTONS "-/+",0,"(",1,"-/+$",2,"($",3 SIZE 25 BY 0.81.
  DEFINE {1} VAR W_Rpta     AS LOGICAL.
  DEFINE {1} VAR W_Validacion AS INTEGER INITIAL -1.
  DEFINE {1} VAR W_Raya     AS CHAR INITIAL "-" FORMAT "X".
  DEFINE {1} VAR W_OfiTra   AS INTEGER FORMAT ">>9" VIEW-AS FILL-IN.
  DEFINE {1} VAR W_Id_Paag  AS LOGICAL INITIAL FALSE LABEL "Sin Ajuste" VIEW-AS TOGGLE-BOX SIZE 10 BY 1.
  DEFINE {1} VAR W_Id_Defla AS LOGICAL INITIAL FALSE LABEL "Deflactado" VIEW-AS TOGGLE-BOX SIZE 10 BY 1.
  DEFINE {1} VAR W_TitRet  AS CHARACTER FORMAT "X(40)".
/********************/


  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.
  DEFINE VARIABLE choice AS LOGICAL.

  DEFINE VAR W_NitEnc LIKE Clientes.Nit.  
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
  

  DEFINE TEMP-TABLE TSDoc
      FIELD TS_Age         LIKE Agencias.Agencia
      FIELD TS_Doc         LIKE Mov_Contable.Num_Documento
      FIELD TS_Com         LIKE Mov_Contable.Comprobante
      FIELD TS_Db          AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
      FIELD TS_Cr          AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
      FIELD TS_Fec         AS DATE
      FIELD TS_Hor         AS INTEGER
      FIELD TS_NO          AS LOGICAL INITIAL YES  
      /* Cambios */  
      INDEX IdxAge TS_Age TS_Com TS_Doc.
      /***********/
    
/* incluido de Pantalla con parametros*/
{incluido/Pantalla_Validacion(1).i} 
 MESSAGE "Vengo de Validacion"
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
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
  
  PROCEDURE Habilita_Deshabilita:
  /* En este procedimiento se habilitan o deshabilitan las variables
     a pedir en pantalla segun el informe que se vaya a ejecutar. */
      ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.
      DISABLE Cmb_CenCost    Cmb_Nivel
              W_Usuario1     W_Usuario2
              W_NomUsuario1  W_NomUsuario2          
              W_Cuenta1      W_Cuenta2
              W_NomCuenta1   W_NomCuenta2         
              W_Nit1         W_NomNit1            
              W_Nit2         W_NomNit2            
              W_Base         W_Porcentaje WITH FRAME F_Valida.
      ASSIGN W_Fec1:LABEL IN FRAME F_Valida = "Desde"
             W_Fec2:LABEL IN FRAME F_Valida = "Hasta".
  END PROCEDURE.

  PROCEDURE Tabla_Temporal:  /* TT */
    DEFINE VAR W_Consecutivo LIKE Mov_Contable.Num_Documento.
    FOR EACH Mov_Contable WHERE
            /* Cambios */
             Mov_Contable.Comprobante  GE W_CB1   AND
             Mov_Contable.Comprobante  LE W_CB2   AND
             Mov_Contable.Fec_Contable GE W_Fec1  AND
             Mov_Contable.Fec_Contable LE W_Fec2  AND          
             Mov_Contable.Agencia      GE W_Ag1   AND
             Mov_Contable.Agencia      LE W_Ag2   NO-LOCK
             /***********/
             BREAK BY Mov_Contable.Agencia
                   BY Mov_Contable.Comprobante
                   BY Mov_Contable.Num_Documento:
        IF FIRST-OF(Mov_Contable.Comprobante) THEN 
           W_Consecutivo = Mov_Contable.Num_Documento.
        ASSIGN TotCre = TotCre + Mov_Contable.CR
               TotDeb = TotDeb + Mov_Contable.DB.
        IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
           REPEAT WHILE W_Consecutivo LT Mov_Contable.Num_Documento:
             CREATE TSDoc.
             ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante
                    TSDoc.TS_Age = Mov_Contable.Agencia
                    TSDoc.TS_Doc = W_Consecutivo
                    TSDoc.TS_No  = NO.
             W_Consecutivo = W_Consecutivo + 1.
           END.
           IF W_Consecutivo EQ Mov_Contable.Num_Documento THEN DO:
             CREATE TSDoc.
             ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante
                    TSDoc.TS_Age = Mov_Contable.Agencia
                    TSDoc.TS_Doc = Mov_Contable.Num_Documento
                    TSDoc.TS_CR  = TotCre
                    TSDoc.TS_DB  = TotDeb
                    TSDoc.TS_Fec = Mov_Contable.Fec_Contable
                    TSDoc.TS_Hor = Mov_Contable.Hora.
             W_Consecutivo = W_Consecutivo + 1.
           END.
           ASSIGN TotCre = 0 TotDeb = 0.
        END.
    END.
  END PROCEDURE.

  PROCEDURE Proceso_Imprimir:
      DEFINE VAR Listado AS CHARACTER INITIAL "".
      MESSAGE "Desea Sacar las inconsistencias?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.
      /* Cambios*/
      EMPTY TEMP-TABLE TSDoc.
      /*********/  
/*       FOR EACH TSDoc: DELETE TSDoc. END. */
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
      W_Reporte   = "REPORTE   : CONSECUTIVOS ENTRE : " + 
                     STRING(W_Fec1) + " y " + STRING(W_Fec2) + 
                     " Para la Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(20)").
                    
                             /*1         2         3         4         5         6         7*/
                   /* 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
      W_EncColumna = "               DOC                  DEBITO           CREDITO         FECHA     HORA".

      DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".

      DEFINE VAR  TT_Db  AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
      DEFINE VAR TotIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
      DEFINE VAR TotFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
      DEFINE VAR Hora AS CHARACTER FORMAT "X(10)".

      VIEW FRAME F-Encabezado.
      VIEW FRAME f-ftr.
      /* Cambios */  
      EMPTY TEMP-TABLE IEx.
      /***********/  
/*       FOR EACH IEx: DELETE IEx. END. */
      CREATE IEx.
      ASSIGN Ct = Ct + 1
             IEx.NLinea = Ct
             IEx.Linea  = "COMPROBANTE" + Cma + "DOCUMENTO" + Cma + 
                          "DEBITO" + Cma + "CREDITO" + Cma + "FECHA" + Cma + "HORA".
      FOR EACH TSDoc 
          BREAK BY TSDoc.TS_Age BY TSDoc.TS_Com BY TSDoc.TS_Doc:
          IF FIRST-OF(TSDoc.TS_Age) THEN DO:
             FIND Agencias WHERE Agencias.Agencia EQ TSDoc.TS_Age NO-LOCK NO-ERROR.
             IF AVAILABLE Agencias THEN DO:
               DISPLAY Agencias.Agencia AT 1
                       Agencias.Nombre  AT 5
               WITH FRAME F_Age WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
               CREATE IEx.
               ASSIGN Ct = Ct + 1
                      IEx.NLinea = Ct
                      IEx.Linea  = STRING(Agencias.Agencia) + Cma + Agencias.Nombre.
             END.
          END.
          IF FIRST-OF(TSDoc.TS_Com) THEN DO:
             FIND Comprobantes WHERE Comprobantes.Agencia EQ TSDoc.TS_Age AND
                  Comprobantes.Comprobante EQ TSDoc.TS_Com NO-LOCK NO-ERROR.
             IF AVAILABLE Comprobantes THEN DO:
               DISPLAY Comprobantes.Comprobante AT 1
                       Comprobantes.Nombre      AT 5
               WITH FRAME F_Com WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
             END.
          END.
          ASSIGN TT_Db = TT_Db + TSDoc.TS_DB
                 TT_CR = TT_CR + TSDoc.TS_CR.
          IF TSDoc.TS_No EQ YES THEN DO:
             Hora = STRING(TSDoc.TS_Hor,"hh:mm:ss").
             DISPLAY 
              TSDoc.TS_Doc        AT 15
              TSDoc.TS_Db         AT 35
              TSDoc.TS_Cr         AT 50
              TSDoc.TS_Fec        AT 70
              Hora                AT 82
             WITH WIDTH 132 FRAME F_Mov USE-TEXT STREAM-IO NO-LABELS NO-BOX.
             /*informe excel*/
             CREATE IEx.
             ASSIGN Ct = Ct + 1
                    IEx.NLinea = Ct
                    IEx.Linea  = STRING(TSDoc.TS_Com) + Cma +
                                 STRING(TSDoc.TS_Doc) + Cma + 
                                 STRING(TSDoc.TS_Db,"->>>>>>>>>>>9.99")  + Cma + 
                                 STRING(TSDoc.TS_Cr,"->>>>>>>>>>>9.99")  + Cma + 
                                 STRING(TSDoc.TS_Fec) + Cma + 
                                 STRING(Hora).
          END.
          ELSE DO:
            IF choice THEN DO:
             Hora = "NoExiste".
             DISPLAY 
              TSDoc.TS_Doc        AT 15
              Hora                AT 27
             WITH WIDTH 132 FRAME F_Mov2 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
            END.
          END.
          IF LAST-OF(TSDoc.TS_Com) THEN DO:
             DISPLAY "Total Comprobante: " AT 1
                     TSDoc.TS_Com          AT 25
                     TT_Db                 AT 35
                     TT_Cr                 AT 50 SKIP(1)
             WITH WIDTH 132 FRAME F_Cbt USE-TEXT STREAM-IO NO-LABELS NO-BOX.
             ASSIGN TT_Db = 0 TT_Cr = 0.
          END.
      END.  
      PAGE.
    OUTPUT CLOSE.
  END PROCEDURE.

  PROCEDURE Imprimir_Excel:
    LisEx = w_Pathspl + "RevConsecutivos.csv".
    OUTPUT TO VALUE(LisEx).
    FOR EACH IEx BY IEx.NLinea:
        PUT IEx.Linea SKIP.
    END.
    OUTPUT CLOSE.
    MESSAGE "Revision Consecutivos para Excel se encuentra en:" SKIP
            LisEx VIEW-AS ALERT-BOX INFORMATION.
    /* Cambios */
    EMPTY TEMP-TABLE IEx.
    /************/     
/*     FOR EACH IEx: DELETE IEx. END. */
  END PROCEDURE.

  /*PROCEDURE Imprimir_Excel:
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
       FOR EACH TSDoc
           BREAK BY TSDoc.TS_Com BY TSDoc.TS_Doc:
            E_Fila2     = "".
/*            E_Fila2     = "014" + STRING(TScuentas.TS_Cuenta,"X(14)")
                        + "025" + STRING(TSCuentas.TS_Nombre,"X(25)")
                        + "002" + STRING(TSCuentas.TS_Nat,"99")
                        + "015" + STRING(TSCuentas.TS_Ini,"->>,>>>,>>>,>>>")
                        + "015" + STRING(TSCuentas.TS_Db,"->>,>>>,>>>,>>>")
                        + "015" + STRING(TSCuentas.TS_Cr,"->>,>>>,>>>,>>>")
                        + "015" + STRING(TSCuentas.TS_Fin,"->>,>>>,>>>,>>>").*/
            {Incluido\imprimir_Excel.i}
        
  END PROCEDURE.*/
