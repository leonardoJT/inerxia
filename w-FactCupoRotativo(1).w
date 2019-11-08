&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME B-PeriodosP

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Per_Facturacion

/* Definitions for BROWSE B-PeriodosP                                   */
&Scoped-define FIELDS-IN-QUERY-B-PeriodosP Per_Facturacion.Per_Factura /*WIDTH 5,43*/ Per_Facturacion.Fec_Inicial Per_Facturacion.Fec_Final Per_Facturacion.Fec_LimPago /*WIDTH 9,43*/ Per_Facturacion.Estado /*WIDTH 6,86*/   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-PeriodosP   
&Scoped-define SELF-NAME B-PeriodosP
&Scoped-define QUERY-STRING-B-PeriodosP FOR EACH Per_Facturacion NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-PeriodosP OPEN QUERY {&SELF-NAME} FOR EACH Per_Facturacion NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-PeriodosP Per_Facturacion
&Scoped-define FIRST-TABLE-IN-QUERY-B-PeriodosP Per_Facturacion


/* Definitions for FRAME F-Periodos                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Periodos ~
    ~{&OPEN-QUERY-B-PeriodosP}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS R-Seleccion 
&Scoped-Define DISPLAYED-OBJECTS R-Seleccion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE R-Seleccion AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Periodos", 1,
"Consultas", 2,
"Generación", 3
     SIZE 85 BY 1.08 NO-UNDO.

DEFINE BUTTON Btn_ConsultaP DEFAULT 
     LABEL "&Consulta" 
     SIZE 9 BY 1.08
     BGCOLOR 8 .

DEFINE BUTTON Btn_ImprimirP 
     IMAGE-UP FILE "Z:/prg/Imagenes/impresora2.bmp":U
     LABEL "&ImprimirP" 
     SIZE 9 BY 1.27.

DEFINE BUTTON Btn_IngresarP DEFAULT 
     LABEL "&Ingresar" 
     SIZE 9 BY 1.08.

DEFINE BUTTON Btn_SalirP DEFAULT 
     LABEL "&Salir" 
     SIZE 9 BY 1.08
     BGCOLOR 8 .

DEFINE VARIABLE F-FecCorP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE F-fecIniP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE F-FecPagoP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .81 NO-UNDO.

DEFINE VARIABLE F-PeriodoP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY 5.38.

DEFINE VARIABLE L-EstadoP AS CHARACTER INITIAL "1" 
     VIEW-AS SELECTION-LIST SINGLE 
     LIST-ITEMS "Vigente","No Vigente","Futuro" 
     SIZE 7 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-PeriodosP FOR 
      Per_Facturacion SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-PeriodosP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-PeriodosP wWin _FREEFORM
  QUERY B-PeriodosP NO-LOCK DISPLAY
      Per_Facturacion.Per_Factura FORMAT ">>>>>>9":U    COLUMN-LABEL "Periodo" /*WIDTH 5,43*/
      Per_Facturacion.Fec_Inicial FORMAT "99/99/9999":U COLUMN-LABEL "F.Inicial" 
      Per_Facturacion.Fec_Final FORMAT "99/99/9999":U   COLUMN-LABEL "F.Final" 
      Per_Facturacion.Fec_LimPago FORMAT "99/99/9999":U COLUMN-LABEL "F.LtePago"  /*WIDTH 9,43*/
      Per_Facturacion.Estado FORMAT "9":U               COLUMN-LABEL "Estado" /*WIDTH 6,86*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 50 BY 4.04
         TITLE "Descripción" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     R-Seleccion AT ROW 3.15 COL 3 NO-LABEL WIDGET-ID 2
     "     CUPO ROTATIVO - FACTURACIÓN" VIEW-AS TEXT
          SIZE 47 BY 1.08 AT ROW 1.27 COL 18 WIDGET-ID 6
          BGCOLOR 18 FGCOLOR 15 FONT 0
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.14 BY 17.15 WIDGET-ID 100.

DEFINE FRAME F-Periodos
     L-EstadoP AT ROW 1.81 COL 1.29 NO-LABEL WIDGET-ID 110
     F-PeriodoP AT ROW 1.81 COL 7 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     F-fecIniP AT ROW 1.81 COL 14.57 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     F-FecCorP AT ROW 1.81 COL 26.29 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     F-FecPagoP AT ROW 1.81 COL 37 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     Btn_IngresarP AT ROW 2.08 COL 52 WIDGET-ID 2
     B-PeriodosP AT ROW 2.96 COL 1 WIDGET-ID 300
     Btn_ConsultaP AT ROW 3.27 COL 52 HELP
          "Sale del proceso de Depreciación y Ajustes" WIDGET-ID 108
     Btn_ImprimirP AT ROW 4.38 COL 52 WIDGET-ID 86
     Btn_SalirP AT ROW 5.69 COL 52 HELP
          "Sale del proceso de Depreciación y Ajustes" WIDGET-ID 84
     "Estado   Periodo   F.Inicial       F.Corte      F.LmtePago" VIEW-AS TEXT
          SIZE 50 BY .81 AT ROW 1 COL 1 WIDGET-ID 106
     RECT-1 AT ROW 1.54 COL 51 WIDGET-ID 88
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 4.23
         SIZE 62 BY 7.54
         TITLE "Periodos de Facturación" WIDGET-ID 200.


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
         TITLE              = "Facturación Cupo Rotativo"
         HEIGHT             = 17.15
         WIDTH              = 91.14
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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
/* REPARENT FRAME */
ASSIGN FRAME F-Periodos:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME F-Periodos
                                                                        */
/* BROWSE-TAB B-PeriodosP Btn_IngresarP F-Periodos */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-PeriodosP
/* Query rebuild information for BROWSE B-PeriodosP
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Per_Facturacion NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B-PeriodosP */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Facturación Cupo Rotativo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Facturación Cupo Rotativo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Periodos
&Scoped-define SELF-NAME Btn_ConsultaP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ConsultaP wWin
ON CHOOSE OF Btn_ConsultaP IN FRAME F-Periodos /* Consulta */
DO:
  DISABLE R-Seleccion WITH FRAME F-Main.
  DISABLE F-PeriodoP
          F-FecIniP
          F-FecCorP
          F-FecPagoP
          WITH FRAME F-Periodos.
  APPLY "Entry" TO L-EstadoP.
/*   DISABLE Btn_Plano        WITH FRAME f-main.  */
/*   DISABLE Btn_Contabilizar WITH FRAME f-main.  */
/*   ENABLE  Btn_Imprimir     WITH FRAME F-main.  */
/*                                                */
/*   EMPTY TEMP-TABLE Rec_Tmp.                    */
/*   &IF DEFINED (adm-panel) <> 0 &THEN           */
/*       RUN dispatch IN THIS-PROCEDURE ('exit'). */
/*   &ELSE                                        */
/*       APPLY "CLOSE":U TO THIS-PROCEDURE.       */
/*   &ENDIF                                       */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_SalirP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalirP wWin
ON CHOOSE OF Btn_SalirP IN FRAME F-Periodos /* Salir */
DO:
/*   EMPTY TEMP-TABLE Rec_Tmp.                     */
/*   &IF DEFINED (adm-panel) <> 0 &THEN            */
/*       RUN dispatch IN THIS-PROCEDURE ('exit').  */
/*   &ELSE                                         */
/*       APPLY "CLOSE":U TO THIS-PROCEDURE.        */
/*   &ENDIF                                        */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME L-EstadoP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL L-EstadoP wWin
ON VALUE-CHANGED OF L-EstadoP IN FRAME F-Periodos

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME B-PeriodosP
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
  DISPLAY R-Seleccion 
      WITH FRAME F-Main IN WINDOW wWin.
  ENABLE R-Seleccion 
      WITH FRAME F-Main IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY L-EstadoP F-PeriodoP F-fecIniP F-FecCorP F-FecPagoP 
      WITH FRAME F-Periodos IN WINDOW wWin.
  ENABLE RECT-1 L-EstadoP F-PeriodoP F-fecIniP F-FecCorP F-FecPagoP 
         Btn_IngresarP B-PeriodosP Btn_ConsultaP Btn_ImprimirP Btn_SalirP 
      WITH FRAME F-Periodos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Periodos}
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

