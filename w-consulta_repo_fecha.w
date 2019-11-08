&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}


DEF VAR hSper001 AS HANDLE NO-UNDO.
RUN repo-super001.p PERSISTENT SET hSper001.
SESSION:ADD-SUPER-PROCEDURE(hSper001).
DEF BUFFER brepo FOR repositorio.repositorio.
DEF BUFFER bconf FOR repositorio.conf_repositorio.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEF VAR W_FecMes AS DATE NO-UNDO.
W_FecMes = TODAY.
DEF VAR cLstaSlccion AS CHAR NO-UNDO.

DEFINE TEMP-TABLE TExcel NO-UNDO
    FIELD TE_Ide      AS CHARACTER FORMAT "X(14)".

   DEFINE VARIABLE Listado      AS CHARACTER INITIAL "l-planti.Lst".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-290 faFchai faFchaf ~
SELECT-CmposSlcciondos SELECT-Rpstrio BUTTON-166 Btn_Imp Btn_Ejecutar ~
BUTTON-tdos-drcha BUTTON-uno-derecha BUTTON-uno-izquierda ~
BUTTON-tdos-izquierda BUTTON-14 BUTTON-11 
&Scoped-Define DISPLAYED-OBJECTS faFchai faFchaf SELECT-CmposSlcciondos ~
SELECT-Rpstrio 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fColExcel wWin 
FUNCTION fColExcel RETURNS CHARACTER
  (i AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fL1TOL2 wWin 
FUNCTION fL1TOL2 RETURNS CHARACTER
  (cbuscar AS CHAR,corigen AS CHAR,cdestino AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fToExcelVlor wWin 
FUNCTION fToExcelVlor RETURNS LOGICAL
  (pCol AS char,pval AS char /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_w-repo-info AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ejecutar 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Ejecutar" 
     SIZE 10 BY 1.62 TOOLTIP "Envía Repositorio Directamente A Excel".

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Opciones De Impresión".

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-14 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 14" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-166 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 166" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-tdos-drcha 
     IMAGE-UP FILE "imagenes/sender.bmp":U
     LABEL "->|" 
     SIZE 14 BY 1.35.

DEFINE BUTTON BUTTON-tdos-izquierda 
     IMAGE-UP FILE "imagenes/senizq.bmp":U
     LABEL "|<-" 
     SIZE 14 BY 1.35.

DEFINE BUTTON BUTTON-uno-derecha 
     IMAGE-UP FILE "imagenes/btn_fwd.bmp":U
     LABEL "->" 
     SIZE 14 BY 1.35.

DEFINE BUTTON BUTTON-uno-izquierda 
     IMAGE-UP FILE "imagenes/btn_bck.bmp":U
     LABEL "<-" 
     SIZE 14 BY 1.35.

DEFINE VARIABLE faFchaf AS INTEGER FORMAT "999999":U INITIAL 0 
     LABEL "Final" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY 1 TOOLTIP "Período Final" NO-UNDO.

DEFINE VARIABLE faFchai AS INTEGER FORMAT "999999":U INITIAL 0 
     LABEL "Inicial" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY 1 TOOLTIP "Período Inicial" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 1.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 16 BY 5.88.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE VARIABLE SELECT-CmposSlcciondos AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "item1","1" 
     SIZE 30 BY 15.35 TOOLTIP "Campos Repositorio Disponibles" NO-UNDO.

DEFINE VARIABLE SELECT-Rpstrio AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "item1","1" 
     SIZE 30 BY 15.35 TOOLTIP "Campos Repositorio Disponibles" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     faFchai AT ROW 1.54 COL 8 COLON-ALIGNED WIDGET-ID 12
     faFchaf AT ROW 1.54 COL 24 COLON-ALIGNED WIDGET-ID 16
     SELECT-CmposSlcciondos AT ROW 3.96 COL 53 NO-LABEL WIDGET-ID 8
     SELECT-Rpstrio AT ROW 4.04 COL 3 NO-LABEL WIDGET-ID 4
     BUTTON-166 AT ROW 4.77 COL 94 WIDGET-ID 38
     Btn_Imp AT ROW 6.38 COL 94 WIDGET-ID 54
     Btn_Ejecutar AT ROW 8 COL 94 WIDGET-ID 36
     BUTTON-tdos-drcha AT ROW 9.08 COL 36 WIDGET-ID 26
     BUTTON-uno-derecha AT ROW 10.42 COL 36 WIDGET-ID 24
     BUTTON-uno-izquierda AT ROW 11.77 COL 36 WIDGET-ID 28
     BUTTON-tdos-izquierda AT ROW 13.12 COL 36 WIDGET-ID 30
     BUTTON-14 AT ROW 15.27 COL 95 WIDGET-ID 46
     BUTTON-11 AT ROW 17.15 COL 97 WIDGET-ID 42
     "Período:" VIEW-AS TEXT
          SIZE 8 BY .62 TOOLTIP "Rango Períodos" AT ROW 1 COL 4 WIDGET-ID 20
          FGCOLOR 7 
     "Campos Seleccionados Repositorio:" VIEW-AS TEXT
          SIZE 32 BY .62 TOOLTIP "Campos Disponibles Repositorio" AT ROW 3.15 COL 53 WIDGET-ID 10
          FGCOLOR 7 
     "Campos Disponibles Repositorio:" VIEW-AS TEXT
          SIZE 29 BY .62 TOOLTIP "Campos Disponibles Repositorio" AT ROW 3.15 COL 3 WIDGET-ID 6
          FGCOLOR 7 
     RECT-1 AT ROW 1.27 COL 3 WIDGET-ID 18
     RECT-2 AT ROW 8.77 COL 35 WIDGET-ID 32
     RECT-290 AT ROW 4.5 COL 93 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 145.72 BY 26.19
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "REPOSITORIO - Consulta Por Fechas"
         HEIGHT             = 19.77
         WIDTH              = 113.14
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* REPOSITORIO - Consulta Por Fechas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* REPOSITORIO - Consulta Por Fechas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  /* APPLY "CLOSE":U TO THIS-PROCEDURE.*/
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ejecutar wWin
ON CHOOSE OF Btn_Ejecutar IN FRAME fMain /* Ejecutar */
DO:
    IF faFchaf < faFchai
    THEN DO: 
        MESSAGE "Rango Seleccionado Incorrecto."
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    IF trim(SELECT-CmposSlcciondos:LIST-ITEM-PAIRS,",") = ""
    THEN DO:
        MESSAGE "Seleccione Los Campos A Reportar."
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    SESSION:SET-WAIT("general").
    RUN RprtaRpstrio(SELECT-CmposSlcciondos:LIST-ITEM-PAIRS).
    SESSION:SET-WAIT("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp wWin
ON CHOOSE OF Btn_Imp IN FRAME fMain /* Imprimir */
DO:
    {incluido/Imprimir.i "Listado" 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 wWin
ON CHOOSE OF BUTTON-14 IN FRAME fMain /* Button 14 */
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


&Scoped-define SELF-NAME BUTTON-166
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-166 wWin
ON CHOOSE OF BUTTON-166 IN FRAME fMain /* Button 166 */
DO:
    RUN W-InfDia.w NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-tdos-drcha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-tdos-drcha wWin
ON CHOOSE OF BUTTON-tdos-drcha IN FRAME fMain /* ->| */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        SELECT-CmposSlcciondos:LIST-ITEM-PAIRS = 
            trim(SELECT-CmposSlcciondos:LIST-ITEM-PAIRS + "," + SELECT-Rpstrio:LIST-ITEM-PAIRS,",").
        SELECT-Rpstrio:LIST-ITEM-PAIRS = ",".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-tdos-izquierda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-tdos-izquierda wWin
ON CHOOSE OF BUTTON-tdos-izquierda IN FRAME fMain /* |<- */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        SELECT-Rpstrio:LIST-ITEM-PAIRS = 
            trim(SELECT-Rpstrio:LIST-ITEM-PAIRS + "," + SELECT-CmposSlcciondos:LIST-ITEM-PAIRS,",").
        SELECT-CmposSlcciondos:LIST-ITEM-PAIRS = ",".
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-uno-derecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-uno-derecha wWin
ON CHOOSE OF BUTTON-uno-derecha IN FRAME fMain /* -> */
DO:
    DEF VAR c AS CHAR no-undo.
    DO WITH FRAME {&FRAME-NAME}:
        c = fL1TOL2(SELECT-Rpstrio:SCREEN-VALUE,
                    SELECT-Rpstrio:LIST-ITEM-PAIRS,
                    SELECT-CmposSlcciondos:LIST-ITEM-PAIRS).
        SELECT-Rpstrio:LIST-ITEM-PAIRS = IF NOT ENTRY(1,c,CHR(1)) = "" THEN ENTRY(1,c,CHR(1)) ELSE ",".
        SELECT-CmposSlcciondos:LIST-ITEM-PAIRS = IF NOT ENTRY(2,c,CHR(1)) = "" THEN ENTRY(2,c,CHR(1)) ELSE ",".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-uno-izquierda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-uno-izquierda wWin
ON CHOOSE OF BUTTON-uno-izquierda IN FRAME fMain /* <- */
DO:
    DEF VAR c AS CHAR no-undo.
    DO WITH FRAME {&FRAME-NAME}:
        c = fL1TOL2(SELECT-CmposSlcciondos:SCREEN-VALUE,
                    SELECT-CmposSlcciondos:LIST-ITEM-PAIRS,
                    SELECT-Rpstrio:LIST-ITEM-PAIRS).
        SELECT-CmposSlcciondos:LIST-ITEM-PAIRS = IF NOT ENTRY(1,c,CHR(1)) = "" THEN ENTRY(1,c,CHR(1)) ELSE ",".
        SELECT-Rpstrio:LIST-ITEM-PAIRS = IF NOT ENTRY(2,c,CHR(1)) = "" THEN ENTRY(2,c,CHR(1)) ELSE ",".
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faFchaf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faFchaf wWin
ON LEAVE OF faFchaf IN FRAME fMain /* Final */
DO:
    ASSIGN {&SELF-NAME}.
    IF NOT fafchaf = 0 THEN
    IF NOT can-find(FIRST repositorio WHERE repositorio.fecha = fafchaf)
    THEN do:
        MESSAGE "Período NO Existe" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    IF fafchai > fafchaf 
    THEN do:
        MESSAGE "Rango NO Existe" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO fafchaf.
        RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faFchai
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faFchai wWin
ON LEAVE OF faFchai IN FRAME fMain /* Inicial */
DO:
    ASSIGN {&SELF-NAME}.
    IF NOT fafchai = 0 THEN
    IF NOT can-find(FIRST repositorio WHERE repositorio.fecha = fafchai)
    THEN do:
        MESSAGE "Período NO Existe" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-CmposSlcciondos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-CmposSlcciondos wWin
ON MOUSE-SELECT-DBLCLICK OF SELECT-CmposSlcciondos IN FRAME fMain
DO:
    APPLY "choose" TO BUTTON-uno-izquierda.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-Rpstrio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-Rpstrio wWin
ON MOUSE-SELECT-DBLCLICK OF SELECT-Rpstrio IN FRAME fMain
DO:
    APPLY "choose" TO BUTTON-uno-derecha.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'w-repo-info.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_w-repo-info ).
       /* Position in AB:  ( 1.42 , 39.29 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Armar_TablaExcel wWin 
PROCEDURE Armar_TablaExcel :
/*
FOR EACH TSaldos WHERE TSaldos.TS_Mes EQ INTEGER(TSaldos.TS_Mes:SCREEN-VALUE IN BROWSE B_Saldos):
       CREATE TExcel.
       ASSIGN TExcel.TE_Ide            = W_Cuenta
              TExcel.TE_Inicial        = TSaldos.TS_Inicial
              TExcel.TE_Debito         = TSaldos.TS_Debito
              TExcel.TE_Credito        = TSaldos.TS_Credito
              TExcel.TE_Final          = TSaldos.TS_Final.
    END.
    FOR EACH TAnexos:
       CREATE TExcel.
       ASSIGN TExcel.TE_Age            = TA_Agencia
              TExcel.TE_Ide            = TA_Nit    
              TExcel.TE_CC             = TA_CenCos 
              TExcel.TE_Inicial        = TA_Inicial
              TEXcel.TE_Debito         = TA_Debito 
              TExcel.TE_Credito        = TA_Credito
              TEXcel.TE_Final          = TA_Final.  
       IF Cuentas.Id_Detalle THEN DO:
           FOR EACH Detalle WHERE Detalle.Agencia  EQ  TAnexos.TA_Agencia AND 
                    Detalle.Cen_Costos             EQ  TAnexos.TA_CenCos  AND
                    Detalle.Nit                    EQ  TAnexos.TA_Nit     AND
                    Detalle.Cuenta                 EQ  W_Cuenta:
               IF Cuentas.Naturaleza EQ "DB" THEN
                  IDSdoFin         = Detalle.Db - Detalle.Cr.
               ELSE
                  IDSdoFin         = Detalle.Cr - Detalle.Db.
               CREATE TExcel.
               ASSIGN TExcel.TE_Age      = Detalle.Agencia              
                      TExcel.TE_CC       = Detalle.Cen_Costos           
                      TExcel.TE_Doc      = Detalle.Doc_referencia       
                      TExcel.TE_Fecha    = Detalle.Fec_ultActualizacion 
                      TExcel.TE_Inicial  = Detalle.Plazo                
                      TExcel.TE_Debito   = Detalle.Valor_Inicial        
                      TExcel.TE_Final    = IDSdoFin.
           END.
       END.
    END.
*/        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY faFchai faFchaf SELECT-CmposSlcciondos SELECT-Rpstrio 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-2 RECT-290 faFchai faFchaf SELECT-CmposSlcciondos 
         SELECT-Rpstrio BUTTON-166 Btn_Imp Btn_Ejecutar BUTTON-tdos-drcha 
         BUTTON-uno-derecha BUTTON-uno-izquierda BUTTON-tdos-izquierda 
         BUTTON-14 BUTTON-11 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprimir_excel wWin 
PROCEDURE imprimir_excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        RUN RprtaRpstrio(SELECT-CmposSlcciondos:LIST-ITEM-PAIRS).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Control wWin 
PROCEDURE Imp_Control :
/*------------------------------------------------------------------------------
  Purpose: Imp_Control    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
    
    DISPLAY STRING(W_Nom_Entidad,"X(40)") + "   -  Repositorio - Consulta Por Fechas" SKIP
    "                              Fecha : " + STRING(W_FecMes,"99/99/9999") FORM "X(80)"
    SKIP (1)
    WITH WIDTH 150 NO-LABELS.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
    RUN SUPER.
    RUN RpstrioCnfgrcion.
    DO WITH FRAME {&FRAME-NAME}:
        SELECT-CmposSlcciondos:LIST-ITEM-PAIRS = ",".
        APPLY "entry" TO SELECT-Rpstrio.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:  ProcesoImprimir   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* {Incluido\RepEncabezado.I} */

  /*ASSIGN W_Reporte    = "Reporte   : Detalle Crediticio Por C/Obligaciòn                       Fecha : " +
                         STRING(W_Fecha,"99/99/9999") + "   Hora :" + STRING(TIME,"HH:MM:SS")
  W_EncColumna = */
        
  RUN Imp_Control.  /*Imprime El Informe*/
    
  DEF VAR cl AS CHAR NO-UNDO.
  DEFINE VAR Listado     AS CHARACTER INITIAL "".
    DO WITH FRAME {&FRAME-NAME}:
        RUN RprtaRpstrio-ArchivoPlanoCSV(SELECT-CmposSlcciondos:LIST-ITEM-PAIRS).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RprtaRpstrio wWin 
PROCEDURE RprtaRpstrio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    
    DEF VAR chExcelApplication  AS COM-HANDLE.
    DEF VAR chWorksheet         AS COM-HANDLE.
    DEF VAR chWorkbook          AS COM-HANDLE.
    DEF VAR chPage              AS COM-HANDLE.

    DEF VAR c1 AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR cQuery AS CHAR NO-UNDO.
    DEF VAR iSheetNumber        AS INTEGER INITIAL 1.
    DEF VAR iFla AS INTEGER NO-UNDO.
    DEF VAR iClmna AS INTEGER NO-UNDO.
    DEF VAR itme AS INTEGER NO-UNDO.
    DO i = 2 TO NUM-ENTRIES(c,",") BY 2:
        c1 = c1 + ENTRY(i,c,",") + ",".
    END.
    c = trim(c1,","). /* lista de campos a reportar */

    /*************/
    DEFINE VARIABLE hBufHndle AS HANDLE.
    DEFINE VARIABLE hQHndle AS HANDLE.
    DEFINE VARIABLE hFieldHndle AS HANDLE     NO-UNDO.
    DEF VAR ht AS HANDLE NO-UNDO.

    ht = BUFFER repositorio:HANDLE.
    CREATE QUERY hQHndle.
    hQHndle:SET-BUFFERS(ht).
    IF fafchai = 0 AND fafchaf = 0
    THEN cquery = "for each " + hT:NAME + " no-lock".
    ELSE cquery =  "for each " + hT:NAME + 
                " no-lock where fecha >= " + string(faFchai) + " and fecha <= " +
                           STRING(fafchaf).
    hQHndle:QUERY-PREPARE(cquery).
    hQHndle:QUERY-OPEN.

    
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.
    IF NOT VALID-HANDLE(chExcelApplication) THEN CREATE "Excel.Application" chExcelApplication .
    IF NOT VALID-HANDLE(chExcelApplication)
    THEN DO:
        MESSAGE  "ERROR: Abriendo Excel".
        RETURN.
    END.
    chExcelApplication:VISIBLE = FALSE.
    chExcelApplication:screenupdating = FALSE.
    PUBLISH "DNEElHdeExel"(chExcelApplication).

    chWorkSheet = chExcelApplication:workbooks:add().
    chWorkSheet = chExcelApplication:Sheets:Item(iSheetNumber).
    chWorkSheet = chExcelApplication:workbooks:item(1):worksheets:add().
    chWorkSheet:activate().
    chWorkSheet:Name = "REPO-" + string(faFchai) + "-" + string(faFchaf).

    iFla = 1.
    iClmna = 0.
    REPEAT i = 1 TO ht:NUM-FIELDS:
        hFieldHndle = ht:BUFFER-FIELD(i).
        IF CAN-DO(c,hFieldHndle:NAME) 
        THEN do:
            iClmna = iClmna + 1.
            fToExcelVlor(fcolexcel(iclmna) + string(ifla),hFieldHndle:NAME).
        END.
    END.
    iclmna = 0.    
    ifla = ifla + 1.
    REPEAT i = 1 TO ht:NUM-FIELDS:
        hFieldHndle = ht:BUFFER-FIELD(i).
        IF CAN-DO(c,hFieldHndle:NAME) 
        THEN do:
            iclmna = iclmna + 1.
            fToExcelVlor(fcolexcel(iclmna) + string(ifla),hFieldHndle:LABEL).
        END.
    END.
    
    REPEAT:
        hQHndle:GET-NEXT.
        IF hQHndle:QUERY-OFF-END THEN LEAVE.
        iclmna = 0.
        ifla = ifla + 1.
        REPEAT i = 1 TO ht:NUM-FIELDS:
            hFieldHndle = ht:BUFFER-FIELD(i).
            IF CAN-DO(c,hFieldHndle:NAME) 
            THEN do:
                iclmna = iclmna + 1.
                ftoexcelvlor(fcolexcel(iclmna) + STRING(ifla),hFieldHndle:BUFFER-VALUE).
            END.
        END.
    END.
    hQHndle:QUERY-CLOSE.
    chExcelApplication:VISIBLE = TRUE.
    chExcelApplication:screenupdating = TRUE.
    chExcelApplication:displayalerts = FALSE.

    chExcelApplication:ActiveWorkbook:SaveAs(w_pathspl + "-infrepo-" + STRING(faFchai) + "-" + STRING(faFchaf) + "-" + trim(w_usuario),1,"","",FALSE,FALSE,,).
    chExcelApplication:QUIT(). 
    RELEASE OBJECT chWorksheet.
    RELEASE OBJECT chWorkBook   NO-ERROR.
    RELEASE OBJECT chExcelApplication.
/*************/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RprtaRpstrio-ArchivoPlanoCSV wWin 
PROCEDURE RprtaRpstrio-ArchivoPlanoCSV :
/*------------------------------------------------------------------------------
  Purpose: Envia Los Datos A Archivo Plano    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    

    DEF VAR chExcelApplication  AS COM-HANDLE.
    DEF VAR chWorksheet         AS COM-HANDLE.
    DEF VAR chWorkbook          AS COM-HANDLE.
    DEF VAR chPage              AS COM-HANDLE.
    
    DEF VAR c1 AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR cQuery AS CHAR NO-UNDO.
    DEF VAR iSheetNumber        AS INTEGER INITIAL 1.
    DEF VAR iFla AS INTEGER NO-UNDO.
    DEF VAR iClmna AS INTEGER NO-UNDO.
    DEF VAR cLnea AS CHAR NO-UNDO.
    DEF VAR cVlor AS CHAR NO-UNDO.
    DEF VAR cFleNme AS CHAR NO-UNDO.
    DEF VAR itme AS INTEGER NO-UNDO.

    DO i = 2 TO NUM-ENTRIES(c,",") BY 2:
        c1 = c1 + ENTRY(i,c,",") + ",".
    END.
    c = trim(c1,","). /* lista de campos a reportar */

    /*************/
    DEFINE VARIABLE hBufHndle AS HANDLE.
    DEFINE VARIABLE hQHndle AS HANDLE.
    DEFINE VARIABLE hFieldHndle AS HANDLE     NO-UNDO.
    DEF VAR ht AS HANDLE NO-UNDO.
    ht = BUFFER repositorio:HANDLE.
    CREATE QUERY hQHndle.
    hQHndle:SET-BUFFERS(ht).

    IF fafchai = 0 AND fafchaf = 0
    THEN cquery = "for each " + hT:NAME + " no-lock".
    ELSE cquery =  "for each " + hT:NAME + 
                " no-lock where fecha >= " + string(faFchai) + " and fecha <= " +
                           STRING(fafchaf).
    hQHndle:QUERY-PREPARE(cquery).
    hQHndle:QUERY-OPEN.

    


    iFla = 1.
    iClmna = 0.
    cflenme = w_pathspl + "-infrepo-" + STRING(faFchai) + "-" + STRING(faFchaf) + "-" + trim(w_usuario) + ".csv".
    itme = TIME.
    
    REPEAT i = 1 TO ht:NUM-FIELDS:
        hFieldHndle = ht:BUFFER-FIELD(i).
        IF CAN-DO(c,hFieldHndle:NAME) 
        THEN do:
            clnea = clnea + hFieldHndle:NAME + ",".
        END.
    END.
    PUT UNFORMATTED trim(clnea,",") SKIP.

    iclmna = 0.    
    ifla = ifla + 1.
    clnea = "".
    REPEAT i = 1 TO ht:NUM-FIELDS:
        hFieldHndle = ht:BUFFER-FIELD(i).
        IF CAN-DO(c,hFieldHndle:NAME) 
        THEN do:
            clnea = clnea + hFieldHndle:LABEL  + ",".
        END.
    END.
    PUT UNFORMATTED trim(clnea,",") SKIP.
    REPEAT:
        hQHndle:GET-NEXT.
        IF hQHndle:QUERY-OFF-END THEN LEAVE.
        iclmna = 0.
        ifla = ifla + 1.
        clnea = "".
        REPEAT i = 1 TO ht:NUM-FIELDS:
            hFieldHndle = ht:BUFFER-FIELD(i).
            IF CAN-DO(c,hFieldHndle:NAME) 
            THEN do:
                iclmna = iclmna + 1.
                cvlor = trim(hFieldHndle:BUFFER-VALUE,",").
                IF hFieldHndle:DATA-TYPE = "decimal" 
                THEN cvlor = string(ROUND(DECIMAL(cvlor),0)).
                clnea = clnea + (IF cvlor = ? THEN "" ELSE cvlor) + ",".
            END.
        END.
        PUT UNFORMATTED trim(clnea,",") SKIP.
    END.
    hQHndle:QUERY-CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RpstrioCnfgrcion wWin 
PROCEDURE RpstrioCnfgrcion :
/*------------------------------------------------------------------------------
  Purpose: CARGA LA DEFINICION INICIAL DEL REPOSITORIO    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cLsta AS CHAR NO-UNDO.
    clsta = "Período(fecha),fecha,Año(ano),ano,Mes(mes),mes,Día(dia),dia,Nit(nit),nit,Número Crédito(NumeroCredito),NumeroCredito,".
    clsta = clsta + "Secuencia(SecuencialMes),SecuencialMes,Tipo Novedad(TipoNovedad),TipoNovedad,".
    FOR EACH conf_repositorio NO-LOCK
        WHERE
            conf_repositorio.estado = 1
        BY conf_repositorio.estado:
        cLsta = cLsta + 
        (IF    conf_repositorio.tablaorigen = "CALCULADO" 
        THEN  conf_repositorio.descampocal 
        ELSE  conf_repositorio.campoorigen) + "(" + conf_repositorio.campodestino + ")" + "," + conf_repositorio.campodestino + ",".  
    END.
    clsta = TRIM(clsta,",").
    DO WITH FRAME {&FRAME-NAME}:
         SELECT-Rpstrio:LIST-ITEM-PAIRS = cLsta.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fColExcel wWin 
FUNCTION fColExcel RETURNS CHARACTER
  (i AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(i).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fL1TOL2 wWin 
FUNCTION fL1TOL2 RETURNS CHARACTER
  (cbuscar AS CHAR,corigen AS CHAR,cdestino AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(cbuscar,corigen,cdestino).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fToExcelVlor wWin 
FUNCTION fToExcelVlor RETURNS LOGICAL
  (pCol AS char,pval AS char /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN SUPER(pcol,pval).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

