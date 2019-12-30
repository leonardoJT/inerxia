&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE INPUT PARAMETER P_NomIns AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER P_NitCli LIKE Clientes.Nit.
DEFINE INPUT PARAMETER P_CodCre LIKE Creditos.Cod_Credito.
DEFINE INPUT PARAMETER P_TipCre LIKE Creditos.Tip_Credito.
DEFINE INPUT PARAMETER P_NumCre LIKE Creditos.Num_Credito.
DEFINE INPUT PARAMETER P_CodOpe LIKE Operacion.Cod_Operacion.
DEFINE INPUT PARAMETER P_Progra AS INTEGER FORMAT "9".

DEFINE VAR P_VrInteres LIKE Creditos.Sdo_Capital.
DEFINE VAR P_VrCapital LIKE Creditos.Sdo_Capital.
DEFINE VAR W_Antes LIKE Creditos.Plazo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Cre

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Creditos

/* Definitions for FRAME F_Cre                                          */
&Scoped-define FIELDS-IN-QUERY-F_Cre Creditos.Reestructurado ~
Creditos.Int_Anticipado Creditos.Fec_Desembolso Creditos.Int_Corrientes ~
Creditos.Fec_Pago Creditos.Fec_ProxLiquidacion Creditos.Fec_UltPago ~
Creditos.Sdo_Capital Creditos.Fec_UltLiquidacion Creditos.Plazo ~
Creditos.Int_DifCobro Creditos.Cuota Creditos.Sdo_Proyectado ~
Creditos.Cuo_Pagadas Creditos.Tasa Creditos.Val_Atraso Creditos.Dias_Atraso ~
Creditos.Cuo_Atraso Creditos.Provision Creditos.Fec_Reestructurado 
&Scoped-define QUERY-STRING-F_Cre FOR EACH Creditos SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Cre OPEN QUERY F_Cre FOR EACH Creditos SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Cre Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-F_Cre Creditos


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-296 Btn_liquidar W_NvoPlazo Btn_Salir 
&Scoped-Define DISPLAYED-FIELDS Creditos.Reestructurado ~
Creditos.Int_Anticipado Creditos.Fec_Desembolso Creditos.Int_Corrientes ~
Creditos.Fec_Pago Creditos.Fec_ProxLiquidacion Creditos.Fec_UltPago ~
Creditos.Sdo_Capital Creditos.Fec_UltLiquidacion Creditos.Plazo ~
Creditos.Int_DifCobro Creditos.Cuota Creditos.Sdo_Proyectado ~
Creditos.Cuo_Pagadas Creditos.Tasa Creditos.Val_Atraso Creditos.Dias_Atraso ~
Creditos.Cuo_Atraso Creditos.Provision Creditos.Fec_Reestructurado 
&Scoped-define DISPLAYED-TABLES Creditos
&Scoped-define FIRST-DISPLAYED-TABLE Creditos
&Scoped-Define DISPLAYED-OBJECTS W_NvoSdoCap W_NvoPlazo W_NvaCuota 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Acepta_Terminos 
     LABEL "Aceptar Nuevos Términos" 
     SIZE 21 BY 1.35.

DEFINE BUTTON Btn_liquidar 
     LABEL "Liquidar" 
     SIZE 21 BY 1.35.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 176" 
     SIZE 15 BY 1.62.

DEFINE VARIABLE W_NvaCuota AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Nueva Cuota" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NvoPlazo AS DECIMAL FORMAT "999":U INITIAL 0 
     LABEL "Nuevo Plazo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NvoSdoCap AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Nuevo Saldo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 11.31.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F_Cre FOR 
      Creditos SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cre
     Creditos.Reestructurado AT ROW 1.54 COL 30 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Reestructurado", 1,
"No reestructurado", 2
          SIZE 20 BY 1.62
     Creditos.Int_Anticipado AT ROW 1.54 COL 77 COLON-ALIGNED
          LABEL "Interés Anticipado"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Desembolso AT ROW 2.08 COL 14 COLON-ALIGNED
          LABEL "Aprobación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 2.62 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Pago AT ROW 2.88 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_ProxLiquidacion AT ROW 3.69 COL 14 COLON-ALIGNED
          LABEL "Prox. Liquidación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_UltPago AT ROW 4.5 COL 14 COLON-ALIGNED
          LABEL "Último Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Capital AT ROW 4.5 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NvoSdoCap AT ROW 4.5 COL 60 COLON-ALIGNED
     Btn_liquidar AT ROW 4.5 COL 75
     Creditos.Fec_UltLiquidacion AT ROW 5.31 COL 14 COLON-ALIGNED
          LABEL "Última Liquidación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Plazo AT ROW 5.58 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NvoPlazo AT ROW 5.58 COL 60 COLON-ALIGNED
     Creditos.Int_DifCobro AT ROW 6.12 COL 14 COLON-ALIGNED
          LABEL "Interés Dif. Cobro"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Btn_Acepta_Terminos AT ROW 6.12 COL 75
     Creditos.Cuota AT ROW 6.65 COL 37 COLON-ALIGNED
          LABEL "Cuota"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NvaCuota AT ROW 6.65 COL 60 COLON-ALIGNED
     Creditos.Sdo_Proyectado AT ROW 6.92 COL 14 COLON-ALIGNED
          LABEL "Saldo Proyectado"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuo_Pagadas AT ROW 7.73 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Tasa AT ROW 7.73 COL 37 COLON-ALIGNED
          LABEL "Tasa"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Val_Atraso AT ROW 8.54 COL 14 COLON-ALIGNED
          LABEL "Valor Atraso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Dias_Atraso AT ROW 9.35 COL 14 COLON-ALIGNED
          LABEL "Días de Atraso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Cuo_Atraso AT ROW 10.15 COL 14 COLON-ALIGNED
          LABEL "Cuotas Atrasadas"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.43 BY 12.12
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Cre
     Creditos.Provision AT ROW 10.96 COL 14 COLON-ALIGNED
          LABEL "Provisión"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Salir AT ROW 11.23 COL 78
     Creditos.Fec_Reestructurado AT ROW 11.77 COL 14 COLON-ALIGNED
          LABEL "Reestructuración"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "Información del Crédito" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 1.15 COL 3
          FGCOLOR 7 FONT 5
     RECT-296 AT ROW 1.54 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.43 BY 12.12
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW Wwin ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert SmartWindow title>"
         COLUMN             = 3.57
         ROW                = 10.08
         HEIGHT             = 12.12
         WIDTH              = 97.43
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Wwin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW Wwin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Cre
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn_Acepta_Terminos IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Cuota IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Pagadas IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Dias_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Desembolso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Pago IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Fec_ProxLiquidacion IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltLiquidacion IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Anticipado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Plazo IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Provision IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Creditos.Reestructurado IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Capital IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Proyectado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Tasa IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN W_NvaCuota IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NvoSdoCap IN FRAME F_Cre
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cre
/* Query rebuild information for FRAME F_Cre
     _TblList          = "bdCentral.Creditos"
     _Query            is OPENED
*/  /* FRAME F_Cre */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Acepta_Terminos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Acepta_Terminos Wwin
ON CHOOSE OF Btn_Acepta_Terminos IN FRAME F_Cre /* Aceptar Nuevos Términos */
DO:
  IF DECIMAL(W_NvoPlazo:SCREEN-VALUE IN FRAME F_Cre) EQ 0 OR
     DECIMAL(W_NvaCuota:SCREEN-VALUE IN FRAME F_Cre) EQ 0 OR 
     W_NvaCuota:SCREEN-VALUE IN FRAME F_Cre EQ "?" THEN DO:
     MESSAGE "El nuevo plazo es cero o no se ha liquidado de nuevo" SKIP
             "no se aceptan los nuevos terminos" VIEW-AS ALERT-BOX.
     APPLY "entry" TO W_NvoPlazo IN FRAME F_Cre.
     RETURN NO-APPLY.
  END.
  DO WITH FRAME F_Cre:
      DEFINE VAR Listado     AS CHARACTER INITIAL "".
      Listado = W_PathSpl + "Prorroga.LST".
      {INCLUIDO\Imprimir.I "Listado"} 
      ASSIGN Creditos.Reestructurado:SCREEN-VALUE = "2"
             Creditos.Fec_Reestructurado:SCREEN-VALUE = "?"
             Creditos.Sdo_Capital:SCREEN-VALUE = W_NvoSdoCap:SCREEN-VALUE IN FRAME F_Cre
             Creditos.Plazo:SCREEN-VALUE       = W_NvoPlazo:SCREEN-VALUE IN FRAME F_Cre
             Creditos.Cuota:SCREEN-VALUE       = W_NvaCuota:SCREEN-VALUE IN FRAME F_Cre
             Creditos.Val_Atraso:SCREEN-VALUE  = "0"
             Creditos.Dias_Atraso:SCREEN-VALUE = "0"
             Creditos.Cuo_Atraso:SCREEN-VALUE  = "0".
      ASSIGN FRAME F_Cre 
             Creditos.Sdo_Capital
             Creditos.Plazo
             Creditos.Cuota
             Creditos.Val_Atraso
             Creditos.Dias_Atraso
             Creditos.Cuo_Atraso.
  END.
  RELEASE Creditos.
  APPLY "choose" TO Btn_Salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_liquidar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_liquidar Wwin
ON CHOOSE OF Btn_liquidar IN FRAME F_Cre /* Liquidar */
DO:
  W_NvoSdoCap = DECIMAL(W_NvoSdoCap:SCREEN-VALUE IN FRAME F_Cre) + Creditos.Val_Atraso.  RUN Liquidar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Wwin
ON CHOOSE OF Btn_Salir IN FRAME F_Cre /* Button 176 */
DO: 
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NvoPlazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NvoPlazo Wwin
ON ENTRY OF W_NvoPlazo IN FRAME F_Cre /* Nuevo Plazo */
DO:
  W_Antes = INTEGER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NvoPlazo Wwin
ON LEAVE OF W_NvoPlazo IN FRAME F_Cre /* Nuevo Plazo */
DO:
  IF W_Antes NE INTEGER(SELF:SCREEN-VALUE) THEN DO:
     APPLY "choose" TO Btn_Liquidar IN FRAME F_Cre.
     ENABLE Btn_Acepta_Terminos WITH FRAME F_Cre.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Wwin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects Wwin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Wwin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
  THEN DELETE WIDGET Wwin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Wwin  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-F_Cre}
  GET FIRST F_Cre.
  DISPLAY W_NvoSdoCap W_NvoPlazo W_NvaCuota 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Reestructurado Creditos.Int_Anticipado 
          Creditos.Fec_Desembolso Creditos.Int_Corrientes Creditos.Fec_Pago 
          Creditos.Fec_ProxLiquidacion Creditos.Fec_UltPago Creditos.Sdo_Capital 
          Creditos.Fec_UltLiquidacion Creditos.Plazo Creditos.Int_DifCobro 
          Creditos.Cuota Creditos.Sdo_Proyectado Creditos.Cuo_Pagadas 
          Creditos.Tasa Creditos.Val_Atraso Creditos.Dias_Atraso 
          Creditos.Cuo_Atraso Creditos.Provision Creditos.Fec_Reestructurado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  ENABLE RECT-296 Btn_liquidar W_NvoPlazo Btn_Salir 
      WITH FRAME F_Cre IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cre}
  VIEW Wwin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject Wwin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
RUN SUPER.
  
  FIND Creditos WHERE
       Creditos.Tip_Credito EQ P_TipCre AND
       Creditos.Cod_Credito EQ P_CodCre AND
       Creditos.Num_Credito EQ P_NumCre AND
       Creditos.Nit         EQ P_NitCli EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
     FIND Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito AND
                             Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
     Wwin:TITLE = P_NomIns.
     RUN Mostrar_Credito.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidar Wwin 
PROCEDURE Liquidar :
DEFINE VARIABLE PlazoW LIKE Solicitud.Plazo.
  DEFINE VARIABLE TotPtW LIKE Solicitud.Monto.
  DEFINE VARIABLE CuotaW LIKE Solicitud.Cuota.
  DEFINE VARIABLE TasaW  LIKE Solicitud.Tasa.
  DEFINE VARIABLE TinteW LIKE Solicitud.Monto.
  
  DEFINE VAR W_Resultado AS INTEGER INITIAL 0.
  DEFINE VAR Wimp_Coop   AS INTEGER INITIAL 0.   
   DO WITH FRAME F_Cre:
      ASSIGN TotPtW   = DECIMAL(W_NvoSdoCap:SCREEN-VALUE) 
             PlazoW   = DECIMAL(W_NvoPlazo:SCREEN-VALUE)
             TasaW    = Creditos.Tasa.
      RUN Calculo_Cuota.R (INPUT-OUTPUT TotPtw, INPUT-OUTPUT PlazoW, INPUT-OUTPUT CuotaW,
                      INPUT-OUTPUT TInteW, INPUT-OUTPUT TasaW, INPUT 0,
                      INPUT 0,INPUT Creditos.Per_Pago, INPUT 3,
                      INPUT Creditos.FOR_Interes,
                      INPUT Creditos.Sistema). 
      IF CuotaW LE 0 THEN DO:
         MESSAGE "El Valor de la cuota debe ser mayor a cero. Rectifique!" VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO W_NvoPlazo IN FRAME F_Cre.
         RETURN ERROR.
      END.
      ASSIGN W_NvaCuota:SCREEN-VALUE = STRING(CuotaW).
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito Wwin 
PROCEDURE Mostrar_Credito :
DO WITH FRAME F_Cre:
   ASSIGN Creditos.Fec_Desembolso:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso)
          Creditos.Fec_Pago:SCREEN-VALUE = STRING(Creditos.Fec_Pago)
          Creditos.Fec_ProxLiquidacion:SCREEN-VALUE = STRING(Creditos.Fec_ProxLiquidacion)
          Creditos.Fec_UltLiquidacion:SCREEN-VALUE = STRING(Creditos.Fec_UltLiquidacion)
          Creditos.INT_DifCobro:SCREEN-VALUE = STRING(Creditos.INT_DifCobro)
          Creditos.Sdo_Proyectado:SCREEN-VALUE = STRING(Creditos.Sdo_Proyectado)
          Creditos.Cuo_Pagadas:SCREEN-VALUE = STRING(Creditos.Cuo_Pagadas)
          Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Val_Atraso)
          Creditos.Dias_Atraso:SCREEN-VALUE = STRING(Creditos.Dias_Atraso)
          Creditos.Cuo_Atraso:SCREEN-VALUE = STRING(Creditos.Cuo_Atraso)
          Creditos.Provision:SCREEN-VALUE = STRING(Creditos.Provision)
          Creditos.Fec_Reestructurado:SCREEN-VALUE = STRING(Creditos.Fec_Reestructurado)
          Creditos.Plazo:SCREEN-VALUE = STRING(Creditos.Plazo)
          Creditos.Sdo_Capital:SCREEN-VALUE = STRING(Creditos.Sdo_Capital)
          Creditos.Cuota:SCREEN-VALUE = STRING(Creditos.Cuota)
          Creditos.Reestructurado:SCREEN-VALUE = STRING(Creditos.Reestructurado)
          Creditos.INT_Corrientes:SCREEN-VALUE = STRING(Creditos.INT_Corrientes)
          Creditos.INT_Anticipado:SCREEN-VALUE = STRING(Creditos.INT_Anticipado)
          W_NvoSdoCap:SCREEN-VALUE = STRING(Creditos.Sdo_Capital)
          W_NvaCuota:SCREEN-VALUE = STRING(Creditos.Cuota)
          W_NvoPlazo:SCREEN-VALUE = STRING(Creditos.Plazo).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir Wwin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}
    DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
    DEFINE VAR T_Plazo   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR T_Dedu    AS CHARACTER FORMAT "X(30)".
    FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    ASSIGN W_Cliente = Creditos.Nit + " - " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
 
    W_EncColumna = "Cliente Solicitante         :   " + W_Cliente.
    W_Reporte   = "REESTRUCTURACION   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.

 DO WITH FRAME F_Cre:
   CASE Creditos.Per_Pago:
     WHEN 1 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Semanas".
     WHEN 3 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Quincenas".
     WHEN 4 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Meses".
   END CASE.
   
   DISPLAY 
     "=============================================DATOS GENERALES DE LA SOLICITUD==============================================" AT 1
     "Agencia de Radicación       : " AT 1
     Creditos.Agencia
     "Número del Crédito          : " AT 65
     Creditos.Num_Credito             AT 98
     "Número de Solicitud         : " AT 1
     Creditos.Num_Solicitud           AT 33
     "Fecha de Aprobación         : " AT 65
     Creditos.Fec_Aprobacion          AT 98
     "Producto de Crédito         : " AT 1
     Pro_Creditos.Nom_Producto        AT 33 
     "Tipo de Producto            : " AT 65
     Pro_Creditos.Tip_Credito         AT 98 
     "Instancia Actual            : " AT 1
     P_NomIns                         AT 33 
     "Usuario Actualmente Procesa : " AT 65
     W_Usuario                        AT 98   
     "Forma de Pago de la Cuota   : " AT 1
     Creditos.FOR_Pago                AT 33 
     "=============================================DETALLE DE VALORES DEL CREDITO==============================================" AT 1
     "Monto a Prestar             : " AT 1
     Creditos.Monto                   AT 33 
     "Tasa Efectiva Anual         : " AT 65
     Creditos.Tasa                    AT 98 SKIP(1)
     WITH FRAME F_Sol WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
   
   DISPLAY "CAMBIOS POR REESTRUCTURACION" AT 1
     "Sdo Capital Anterior        : " AT 1
     Creditos.Sdo_Capital             AT 33  FORMAT ">>>,>>>,>>9"
     "Saldo Capital Reestructurado: " AT 65
     W_NvoSdoCap:SCREEN-VALUE IN FRAME F_Cre AT 98
     "Plazo Anterior              : " AT 1
     T_Plazo                          AT 33
     "Nuevo Plazo                 : " AT 65
     W_NvoPlazo:SCREEN-VALUE IN FRAME F_Cre AT 98
     "Cuota Anterior              : " AT 1
     Creditos.Cuota:SCREEN-VALUE IN FRAME F_Cre AT 33
     "Nuevo Cuota                 : " AT 65
     W_NvaCuota:SCREEN-VALUE IN FRAME F_Cre AT 98
   WITH FRAME F_Sol2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
   
     END.
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

