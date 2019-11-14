&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-ProcDif
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-ProcDif 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* oakley */
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

DEFI VAR W_Valor AS DECIMAL INIT 0.
DEFI VAR W_Coment AS CHAR FORM "X(30)" INIT "".
DEFI VAR W_Cta1 LIKE Cuentas.Cuenta.
DEFI VAR W_Cta2 LIKE Cuentas.Cuenta.

DEFI TEMP-TABLE TempLiq
    FIELD Agen LIKE Diferido.Agencia
    FIELD NitD LIKE Clientes.Nit
    FIELD ProD LIKE Diferido.Cod_Pro
    FIELD CodD LIKE Diferido.Codigo
    FIELD Cuot LIKE Diferido.Cuota
    FIELD VAmo LIKE Diferido.Cuota
    FIELD SdoA LIKE Diferido.Saldo
    FIELD SdoD LIKE Diferido.Saldo
    FIELD Ncuo LIKE Diferido.Nro_Cuotas
    FIELD CxD LIKE Diferido.Nro_Cuotas.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_CmbOfi BUTTON-5 Btn_Proc Btn_Done RECT-314 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_FecMes Msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-ProcDif AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.42
     BGCOLOR 8 .

DEFINE BUTTON Btn_Proc 
     LABEL "&Procesar" 
     SIZE 10 BY 1.46 TOOLTIP "Procesar los Diferidos".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U

    /* oakley */

     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 34.86 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49.57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecMes AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Proceso" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .81 TOOLTIP "Fecha Corte para el Proceso"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15.14 BY 6.27.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     W_CmbOfi AT ROW 3.38 COL 14.72 COLON-ALIGNED
     BUTTON-5 AT ROW 3.65 COL 68.29
     Btn_Proc AT ROW 5.62 COL 68.29
     W_FecMes AT ROW 6.38 COL 14.72 COLON-ALIGNED
     Btn_Done AT ROW 7.42 COL 68.14 HELP
          "Sale del proceso de Depreciación y Ajustes"
     Msaje AT ROW 9.65 COL 14.86 COLON-ALIGNED NO-LABEL
     RECT-314 AT ROW 3.12 COL 65.86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.72 BY 11.38
         BGCOLOR 17 FGCOLOR 0 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-ProcDif ASSIGN
         HIDDEN             = YES
         TITLE              = "Proceso Diferidos, Programa W-ProcDif.W"
         HEIGHT             = 11.38
         WIDTH              = 86.72
         MAX-HEIGHT         = 20.77
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 20.77
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-ProcDif
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
                                                                        */
/* SETTINGS FOR FILL-IN Msaje IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecMes IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProcDif)
THEN W-ProcDif:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-ProcDif
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProcDif W-ProcDif
ON END-ERROR OF W-ProcDif /* Proceso Diferidos, Programa W-ProcDif.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProcDif W-ProcDif
ON WINDOW-CLOSE OF W-ProcDif /* Proceso Diferidos, Programa W-ProcDif.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-ProcDif
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proc W-ProcDif
ON CHOOSE OF Btn_Proc IN FRAME F_Proc /* Procesar */
DO:
  DEFI   VAR Listado    AS CHAR FORM "X(40)".
  DEFINE VAR W_Rpta   AS LOGICAL.

  FOR EACH TempLiq: DELETE TempLiq. END.

  FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                      AND Agencias.Agencia GE W_OfiIni 
                      AND Agencias.Agencia LE W_OfiFin NO-LOCK:
      FIND ProcDia WHERE ProcDia.Agencia     EQ Agencias.Agencia
                     AND ProcDia.Cod_Proceso EQ 9
                     AND ProcDia.Fecha_Proc  EQ W_FecMes
                     AND ProcDia.Estado      EQ 1  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(ProcDia) THEN DO:
         MESSAGE "Este proceso Ya Fué Ejecutado para este Día en la Agencia: " Agencias.Agencia SKIP
                 "O no està Matriculado...Revise por favor. No se permite la operación." SKIP
                 VIEW-AS ALERT-BOX /*QUESTION BUTTONS YES-NO*/ TITLE "Confirmar Proceso".     
         RETURN. 
      END.
  END.

  FIND FIRST Varios WHERE Varios.Tipo   EQ 8
                      AND Varios.Codigo EQ 9 NO-LOCK NO-ERROR.
  IF AVAIL (varios) THEN 
     FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                               AND Comprobantes.Comprob EQ Varios.Comprobante
                               AND Comprobantes.Estado  EQ 1 NO-ERROR.

  IF NOT AVAIL(Comprobantes) OR NOT AVAIL(varios) THEN DO:
     MESSAGE "El Comprobante-Fuente Contable para el proceso debe existir en Tipos y en Comprobantes" SKIP
             "                          No se acepta la Operación." VIEW-AS ALERT-BOX.
     RETURN.
  END.
      
  ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
         listado                = W_PathSpl + "CpteDifer.Lst".
                      
  FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
 
  SESSION:SET-WAIT-STATE("GENERAL").
  ASSIGN Msaje:SCREEN-VALUE = "Espere un momento por favor, Procesando Diferidos...".
  
  RUN Proceso NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     SESSION:SET-WAIT-STATE("").

     RETURN.     
  END.
  ELSE DO:              
     /*{Incluido\ImpArch.I "listado"} */
                
     FOR EACH Agencias WHERE Agencias.Estado  NE 3                                     
                         AND Agencias.Agencia GE W_OfiIni 
                         AND Agencias.Agencia LE W_OfiFin NO-LOCK:                            
         FIND ProcDia WHERE ProcDia.Agencia     EQ Agencias.Agencia                    
                        AND ProcDia.Cod_Proceso EQ 9                                   
                        AND ProcDia.Fecha_Proc  EQ W_FecMes                            
                        AND ProcDia.Estado      EQ 1 NO-ERROR.                         
                                                                                       
         ASSIGN ProcDia.Estado = 2.                                                    
     END.                                                                              
                                                                                           
     {Incluido\Imprimir.I "listado"}

     ASSIGN Msaje:SCREEN-VALUE = "        Proceso Diferidos Terminó Exitosamente...".
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-ProcDif
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-ProcDif
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Proc /* Agencia */
DO:
  IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
     FIND FIRST Agencias WHERE Agencias.Agencia GT 0
                           AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiIni = Agencias.Agencia.
        
     FIND LAST Agencias WHERE Agencias.Agencia GT 0
                          AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiFin = Agencias.Agencia.
  END.
  ELSE ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) 
              W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecMes W-ProcDif
ON LEAVE OF W_FecMes IN FRAME F_Proc /* Fecha Proceso */
DO:
  ASSIGN W_FecMes.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-ProcDif 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
  
  ASSIGN W_OfiIni                              = W_Agencia
         W_OfiFin                              = W_Agencia
         W_FecMes:SCREEN-VALUE IN FRAME F_Proc = STRING(TODAY)
         W_FecMes.
                  
  FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                      AND Agencias.Agencia GT 0 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + 
                        "-" + STRING(Agencias.Nombre,"X(25)")).      
  END.              
     
  ASSIGN W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".
     
  APPLY "VALUE-CHANGED" TO W_CmbOfi.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabiliza W-ProcDif 
PROCEDURE Contabiliza :
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Pro_Diferidos.Dif_CtaFuente
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.

    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Diferido.Agencia
           Mov_Contable.destino = W_Agencia
           Mov_Contable.Comprobante = Comprobantes.Comprobante
           Mov_Contable.Num_Documento = Comprobantes.Secuencia
           Mov_Contable.Fec_Contable = w_fecha
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Comentario = "Proc.Amortización"
           Mov_Contable.Usuario = W_Usuario
           Mov_Contable.Hora = TIME
           Mov_Contable.Cen_Costos = W_CenCosGral
           Mov_Contable.Nit = Diferido.Nit
           Mov_Contable.Doc_Refer = STRING(Diferido.Codigo)
           Mov_Contable.Cuenta = Pro_Diferidos.Dif_CtaFuente.

    IF Cuentas.Naturaleza EQ "DB" THEN
       Mov_Contable.Cr = W_Valor.
    ELSE 
       Mov_Contable.Db = W_Valor.
    
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Diferido.Agencia
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Comprobante = Comprobantes.Comprobante
           Mov_Contable.Num_Documento = Comprobantes.Secuencia
           Mov_Contable.Fec_Contable = w_fecha
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Comentario = "Proc.Amortización"
           Mov_Contable.Usuario = W_Usuario
           Mov_Contable.Hora = TIME
           Mov_Contable.Cen_Costos = W_CenCosGral
           Mov_Contable.Nit = Diferido.Nit
           Mov_Contable.Doc_Refer = STRING(Diferido.Codigo)
           Mov_Contable.Cuenta = Pro_Diferidos.Dif_CtaGtoDif.

    IF Cuentas.Naturaleza EQ "DB" THEN
        Mov_Contable.Db = W_Valor.
    ELSE
        Mov_Contable.Cr = W_Valor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-ProcDif  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProcDif)
  THEN DELETE WIDGET W-ProcDif.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-ProcDif  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY W_CmbOfi W_FecMes Msaje 
      WITH FRAME F_Proc IN WINDOW W-ProcDif.
  ENABLE W_CmbOfi BUTTON-5 Btn_Proc Btn_Done RECT-314 
      WITH FRAME F_Proc IN WINDOW W-ProcDif.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W-ProcDif.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabaMovDif W-ProcDif 
PROCEDURE GrabaMovDif :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  CREATE Mov_Diferido.
  ASSIGN Mov_Diferido.Agencia       = Diferido.Agencia
         Mov_Diferido.Codigo        = Diferido.Codigo
         Mov_Diferido.Descrip       = "Proc.Amortización"
         Mov_Diferido.Fecha         = TODAY
         Mov_Diferido.Num_Documento = Comprobantes.Secuencia
         Mov_Diferido.Abono         = W_Valor.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabaTemp W-ProcDif 
PROCEDURE GrabaTemp :
/*------------------------------------------------------------------------------
  Purpose:     
 -----------------------------------------------------------------------------*/
 CREATE TempLiq.
 ASSIGN TempLiq.Agen = Diferido.Agencia
        TempLiq.ProD = Diferido.Cod_Pro
        TempLiq.NitD = Diferido.Nit
        TempLiq.CodD = Diferido.Codigo
        TempLiq.SdoA = Diferido.Sdo_Anterior
        TempLiq.Cuot = Diferido.Cuota
        TempLiq.VAmo = W_Valor 
        TempLiq.SdoD = Diferido.Saldo
        TempLiq.Ncuo = Diferido.Nro_Cuotas
        TempLiq.CxD  = Diferido.Plazo - Diferido.Nro_Cuotas.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpCpte W-ProcDif 
PROCEDURE ImpCpte :
/*------------------------------------------------------------------------------
  Purpose:     
  ------------------------------------------------------------------------------*/
    DEFI VAR TotD   LIKE Mov_Contable.Db INIT 0.
    DEFI VAR TotC   LIKE Mov_Contable.Db INIT 0.
    DEFI VAR TTotD  LIKE Mov_Contable.Db INIT 0.
    DEFI VAR TTotC  LIKE Mov_Contable.Db INIT 0.

    {Incluido\RepEncabezado.I}

    ASSIGN W_Reporte = "Reporte   : Comprobante Resumen Amortizaciòn Diferidos     Fecha del Cpte: " +
                        STRING(W_FecMes,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS").

    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.

    DISPLAY SKIP(0)
              "                   " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                                     STRING(Comprobantes.Secuencia,"99999999") FORMAT "X(120)"  
              SKIP(1)
              "--------------------------------------------------------------------------------------------------------"
         WITH DOWN WIDTH 140 FRAME F1 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 

    FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                            AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                            AND Mov_Contable.Fec_Contable  EQ TODAY NO-LOCK
                                BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta BY Mov_Contable.Nit:
        ASSIGN TotD  = TotD  + Mov_Contable.Db
               TTotD = TTotD + Mov_Contable.Db
               TotC  = TotC  + Mov_Contable.Cr
               TTotC = TTotC + Mov_Contable.Cr.

        IF LAST-OF(Mov_Contable.Nit) THEN DO:
           FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                                AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
           DISPLAY Mov_Contable.Agencia   LABEL "Ag."
                   Mov_Contable.Cuenta    LABEL "Cta-Contable"
                   Cuentas.Nombre         LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas)
                   Mov_Contable.Nit       LABEL "Nit-Diferido"
                   TotD                   LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"
                   TotC                   LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"
               WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

           ASSIGN TotD  = 0
                  TotC  = 0.
        END.

    END.

    DISPLAY SKIP(1)
            "                     TOTAL FINAL------------>                            ---------------- ----------------"
            SKIP
            "                                                                    "
            TTotD      FORM "->>>>>>,>>>,>>9.99"
            TTotC      FORM "->>>>>>,>>>,>>9.99"
               WITH DOWN WIDTH 140 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso W-ProcDif 
PROCEDURE Proceso :
/*------------------------------------------------------------------------------
  Purpose:     
  ------------------------------------------------------------------------------*/
DO TRANSACTION:
  FOR EACH Diferido WHERE Diferido.Agencia GE W_OfiIni
                      AND Diferido.Agencia LE W_OfiFin
                      AND Diferido.Estado  EQ 1
                      AND Diferido.Saldo   GT 0 
           BREAK BY Diferido.Agencia BY Diferido.Cod_producto BY Diferido.Codigo:
                      
      IF FIRST-OF(Diferido.Cod_producto) THEN DO:
         FIND FIRST Pro_Diferidos WHERE Pro_Diferidos.Cod_producto EQ Diferido.Cod_producto
                                        NO-LOCK NO-ERROR.
         IF NOT AVAIL(Pro_Diferidos) THEN DO:
            MESSAGE "Falta configurar Pro_Diferidos.Cod_producto : " Pro_Diferidos.Cod_producto SKIP 
                    "                                          No se acepta la operaciòn..."
                    VIEW-AS ALERT-BOX.
            RETURN ERROR.
         END.
      END.

      ASSIGN W_Valor               = Diferido.Cuota                                                                                             
             Diferido.Sdo_Anterior = Diferido.Saldo.
             
      IF W_Valor GT 0 THEN DO:
         IF Diferido.Saldo GE W_Valor THEN
            Diferido.Saldo = Diferido.Saldo - Diferido.Cuota.
         ELSE 
            ASSIGN W_Valor        = Diferido.Saldo
                   Diferido.Saldo = 0.

         Diferido.Nro_Cuotas = Diferido.Nro_Cuotas + 1. 
          
         RUN GrabaMovDif.

         RUN Contabiliza.
      END.
      ELSE W_Valor = 0.
      
      RUN GrabaTemp.     
  END.
END.  /*Fin Tx*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-ProcDif 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
   DEFI VAR TAmort   LIKE TempLiq.SdoA INIT 0.

   RUN ImpCpte.         
   
   {Incluido\RepEncabezado.I}

   ASSIGN W_Reporte = "Reporte   : Proceso Amortizaciòn Diferidos     Fecha del Informe: " +
            STRING(W_FecMes,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS").

   VIEW FRAME F-Encabezado.
   VIEW FRAME f-ftr.
   
   FOR EACH TempLiq:
       ASSIGN TAmort = TAmort + TempLiq.VAmo.
       
       DISPLAY 
            TempLiq.Agen   LABEL "Ag."
            TempLiq.ProD   LABEL "Pdcto"
            TempLiq.NitD   LABEL "Nit-Diferido"
            TempLiq.CodD   LABEL "Cod-Diferido"
            TempLiq.SdoA   LABEL "Sdo-Anterior"          FORM "->>>>,>>>,>>9.99"
            TempLiq.Cuot   LABEL "Cuota a Diferir"       FORM "->>>>,>>>,>>9.99"
            TempLiq.VAmo   LABEL "Vr.Amortizado"         FORM ">>>>,>>>,>>9.99"
            TempLiq.SdoD   LABEL "Saldo-Actual"          FORM "->>>>,>>>,>>9.99"
            TempLiq.Ncuo   LABEL "Cuo-Diferidas"         FORM "99999"
            TempLiq.CxD    LABEL "X Diferir"             FORM "-99999"
          SKIP(0)
          WITH DOWN WIDTH 240 FRAME F2 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
   END. 
   
   DISPLAY SKIP(1)
           "                                  TOTALES-----> "
           TAmort          FORM "->>>>>,>>>,>>9.99"
      WITH DOWN WIDTH 240 FRAME F3 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

