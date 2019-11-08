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
{incluido\VARIABLE.i "SHARED"}

DEFINE VAR Hora1 AS INTEGER NO-UNDO.
DEFINE VAR Hora2 AS INTEGER NO-UNDO.
ASSIGN Hora1 = TIME.
DEFINE VARIABLE i AS INTEGER NO-UNDO. /*Agencias*/

DEFINE TEMP-TABLE TTCtasInc NO-UNDO
    FIELD Agencia   AS INTEGER
    FIELD Nit       AS CHARACTER
    FIELD Cuenta    AS CHARACTER
    FIELD Fecha     AS DATE
    FIELD Documento AS CHARACTER
    FIELD DB        AS DECIMAL
    FIELD CR        AS DECIMAL
    FIELD Usuario   AS CHARACTER
    FIELD Estado    AS CHARACTER.

DEFINE VARIABLE viAno AS INTEGER NO-UNDO.
DEFINE VARIABLE viMes AS INTEGER NO-UNDO.
DEFINE VARIABLE viAgeIni AS INTEGER NO-UNDO.
DEFINE VARIABLE viAgeFin AS INTEGER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS F-FecIni F-FecFin RS-Opcion BTN-Info ~
BTN-Contabilizar BTN-Cancelar BTN-Salir BTN-Help RECT-1 RECT-274 RECT-321 ~
RECT-322 
&Scoped-Define DISPLAYED-OBJECTS F-FecIni F-FecFin RS-Opcion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 BTN-Info Btn-Consulta BTN-Cancelar BTN-Help 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Cancelar 
     IMAGE-UP FILE "imagenes/borrar.bmp":U
     LABEL "&Cancelar" 
     SIZE 7 BY 1.65 TOOLTIP "Cancelar"
     FONT 4.

DEFINE BUTTON Btn-Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Consultar" 
     SIZE 7 BY 1.65 TOOLTIP "Buscar".

DEFINE BUTTON BTN-Contabilizar 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 7 BY 1.65 TOOLTIP "Contabilizar".

DEFINE BUTTON BTN-Help 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Ayuda" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BTN-Info 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Info" 
     SIZE 7 BY 1.65 TOOLTIP "Información".

DEFINE BUTTON BTN-Salir DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 7 BY 1.65 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON BTN-Titulo 
     LABEL "" 
     SIZE 53.72 BY 1.12.

DEFINE VARIABLE F-FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RS-Opcion AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Movimiento Vs. Saldos", 1,
"Documentos Descuadrados", 2
     SIZE 30 BY 2.69 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 86.86 BY 8.62.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 8.62.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 7.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 7.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     F-FecIni AT ROW 4.77 COL 52 COLON-ALIGNED WIDGET-ID 98
     F-FecFin AT ROW 6.12 COL 52 COLON-ALIGNED WIDGET-ID 100
     RS-Opcion AT ROW 4.77 COL 7 NO-LABEL WIDGET-ID 102
     BTN-Titulo AT ROW 1.31 COL 17.72 WIDGET-ID 68
     BTN-Info AT ROW 1.81 COL 91 WIDGET-ID 58
     Btn-Consulta AT ROW 3.42 COL 91 WIDGET-ID 54
     BTN-Contabilizar AT ROW 5.04 COL 91 WIDGET-ID 56
     BTN-Cancelar AT ROW 6.65 COL 91 WIDGET-ID 52
     BTN-Salir AT ROW 8.27 COL 91 WIDGET-ID 50
     BTN-Help AT ROW 10.42 COL 92 WIDGET-ID 60
     "Reporte Movimientos(Día) Vs. Saldos(Mes)" VIEW-AS TEXT
          SIZE 40 BY .81 AT ROW 1.46 COL 24.57 WIDGET-ID 74
          BGCOLOR 11 FONT 1
     "Reportes" VIEW-AS TEXT
          SIZE 9 BY .81 AT ROW 3.12 COL 17.57 WIDGET-ID 110
          FONT 1
     "Filtros" VIEW-AS TEXT
          SIZE 7 BY .81 AT ROW 3.12 COL 59.57 WIDGET-ID 112
          FONT 1
     RECT-1 AT ROW 2.62 COL 1.14 WIDGET-ID 2
     RECT-274 AT ROW 1.54 COL 90 WIDGET-ID 66
     RECT-321 AT ROW 3.42 COL 41 WIDGET-ID 106
     RECT-322 AT ROW 3.42 COL 4 WIDGET-ID 108
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 1
         SIZE 98 BY 11
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Reporte Movimiento Saldos Documentos - wRptMvtoSldoDoc.w"
         HEIGHT             = 10.96
         WIDTH              = 97.86
         MAX-HEIGHT         = 38.23
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 38.23
         VIRTUAL-WIDTH      = 182.86
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
   FRAME-NAME UNDERLINE Custom                                          */
/* SETTINGS FOR BUTTON BTN-Cancelar IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON Btn-Consulta IN FRAME fMain
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON BTN-Help IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON BTN-Info IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON BTN-Titulo IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Reporte Movimiento Saldos Documentos - wRptMvtoSldoDoc.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Reporte Movimiento Saldos Documentos - wRptMvtoSldoDoc.w */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Contabilizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Contabilizar wWin
ON CHOOSE OF BTN-Contabilizar IN FRAME fMain /* Imprimir */
DO:
    CASE RS-Opcion:SCREEN-VALUE:
        WHEN "1" THEN
            RUN wimprime.w ("prMovContaVsSaldos.p", "Movimientos Vs. Saldos",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            DATE(F-FecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}), /* 8 Campos Fechas - 4 Fechas*/
                            DATE(F-FecFin:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                            ?, ?,
                            ?, ?,
                            ?, ?,
                            ?, ?,  /* 4 Campos Lógicos*/
                            ?, ?).
        WHEN "2" THEN
            RUN wimprime.w ("prDocumDescruadrados.p", "Documentos Descuadrados",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            "", "",
                            DATE(F-FecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}), /* 8 Campos Fechas - 4 Fechas*/
                            DATE(F-FecFin:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                            ?, ?,
                            ?, ?,
                            ?, ?,
                            ?, ?,  /* 4 Campos Lógicos*/
                            ?, ?).
    END CASE.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Info wWin
ON CHOOSE OF BTN-Info IN FRAME fMain /* Info */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Salir wWin
ON CHOOSE OF BTN-Salir IN FRAME fMain
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
  DISPLAY F-FecIni F-FecFin RS-Opcion 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE F-FecIni F-FecFin RS-Opcion BTN-Info BTN-Contabilizar BTN-Cancelar 
         BTN-Salir BTN-Help RECT-1 RECT-274 RECT-321 RECT-322 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

    ASSIGN F-FecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
            F-FecFin:SCREEN-VALUE = STRING(TODAY).
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

