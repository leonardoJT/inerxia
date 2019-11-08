&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Clientes

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 Clientes.Nit Clientes.Nombre ~
Clientes.Apellido1 Clientes.Apellido2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH Clientes NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH Clientes NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 Clientes


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RADIO-SET-1 busca BUTTON-1 BROWSE-1 TOGGLE-2 ~
TOGGLE-7 TOGGLE-12 TOGGLE-3 TOGGLE-8 TOGGLE-13 TOGGLE-4 TOGGLE-9 TOGGLE-14 ~
TOGGLE-5 TOGGLE-10 TOGGLE-6 TOGGLE-11 EDITOR-1 RECT-1 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-1 busca TOGGLE-2 TOGGLE-7 ~
TOGGLE-12 TOGGLE-3 TOGGLE-8 TOGGLE-13 TOGGLE-4 TOGGLE-9 TOGGLE-14 TOGGLE-5 ~
TOGGLE-10 TOGGLE-6 TOGGLE-11 EDITOR-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Ver Información" 
     SIZE 24 BY .85.

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "Información General: 98556938 - Edwin Gómez Mesa" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 109 BY 12.92
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE busca AS CHARACTER FORMAT "X(256)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cedula", 1,
"Nombre", 2,
"Apellido1", 3,
"Apellido2", 4
     SIZE 48 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 6.19.

DEFINE VARIABLE TOGGLE-10 AS LOGICAL INITIAL no 
     LABEL "Atrasos" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-11 AS LOGICAL INITIAL no 
     LABEL "Hoja de Vida" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-12 AS LOGICAL INITIAL no 
     LABEL "Solo Saldos" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-13 AS LOGICAL INITIAL no 
     LABEL "Familiar" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-14 AS LOGICAL INITIAL no 
     LABEL "Ubicación" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-2 AS LOGICAL INITIAL no 
     LABEL "General" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-3 AS LOGICAL INITIAL no 
     LABEL "Seguros" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-4 AS LOGICAL INITIAL no 
     LABEL "Beneficiarios" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-5 AS LOGICAL INITIAL no 
     LABEL "Autorizados" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-6 AS LOGICAL INITIAL no 
     LABEL "Libretas" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-7 AS LOGICAL INITIAL no 
     LABEL "Creditos" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-8 AS LOGICAL INITIAL no 
     LABEL "Ahorros" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-9 AS LOGICAL INITIAL no 
     LABEL "Especiales" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 wWin _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      Clientes.Nit FORMAT "X(12)":U
      Clientes.Nombre FORMAT "X(20)":U WIDTH 13.86
      Clientes.Apellido1 COLUMN-LABEL "Apellido1" FORMAT "X(15)":U
            WIDTH 14.43
      Clientes.Apellido2 COLUMN-LABEL "Apellido2" FORMAT "X(15)":U
            WIDTH 10.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 56 BY 6.19
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .46 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RADIO-SET-1 AT ROW 1 COL 3 NO-LABEL
     busca AT ROW 2.08 COL 7 COLON-ALIGNED
     BUTTON-1 AT ROW 2.08 COL 35
     BROWSE-1 AT ROW 3.15 COL 3
     TOGGLE-2 AT ROW 3.96 COL 62
     TOGGLE-7 AT ROW 3.96 COL 78
     TOGGLE-12 AT ROW 3.96 COL 93
     TOGGLE-3 AT ROW 5.04 COL 62
     TOGGLE-8 AT ROW 5.04 COL 78
     TOGGLE-13 AT ROW 5.04 COL 93
     TOGGLE-4 AT ROW 6.12 COL 62
     TOGGLE-9 AT ROW 6.12 COL 78
     TOGGLE-14 AT ROW 6.12 COL 93
     TOGGLE-5 AT ROW 7.19 COL 62
     TOGGLE-10 AT ROW 7.19 COL 78
     TOGGLE-6 AT ROW 8.27 COL 62
     TOGGLE-11 AT ROW 8.27 COL 78
     EDITOR-1 AT ROW 9.62 COL 3 NO-LABEL
     RECT-1 AT ROW 3.15 COL 60
     " Escoja el tipo de Información que desea ver" VIEW-AS TEXT
          SIZE 39 BY .77 AT ROW 2.88 COL 62
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 22.19
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
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Prototipo Pantalla de Información"
         HEIGHT             = 22.19
         WIDTH              = 113.57
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
/* SETTINGS FOR FRAME fMain
                                                                        */
/* BROWSE-TAB BROWSE-1 BUTTON-1 fMain */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "bdCentral.Clientes"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = bdCentral.Clientes.Nit
     _FldNameList[2]   > bdCentral.Clientes.Nombre
"Clientes.Nombre" ? ? "character" ? ? ? ? ? ? no ? no no "13.86" yes no no "U" "" ""
     _FldNameList[3]   > bdCentral.Clientes.Apellido1
"Clientes.Apellido1" "Apellido1" ? "character" ? ? ? ? ? ? no ? no no "14.43" yes no no "U" "" ""
     _FldNameList[4]   > bdCentral.Clientes.Apellido2
"Clientes.Apellido2" "Apellido2" ? "character" ? ? ? ? ? ? no ? no no "10.86" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Prototipo Pantalla de Información */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Prototipo Pantalla de Información */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
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
  DISPLAY RADIO-SET-1 busca TOGGLE-2 TOGGLE-7 TOGGLE-12 TOGGLE-3 TOGGLE-8 
          TOGGLE-13 TOGGLE-4 TOGGLE-9 TOGGLE-14 TOGGLE-5 TOGGLE-10 TOGGLE-6 
          TOGGLE-11 EDITOR-1 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RADIO-SET-1 busca BUTTON-1 BROWSE-1 TOGGLE-2 TOGGLE-7 TOGGLE-12 
         TOGGLE-3 TOGGLE-8 TOGGLE-13 TOGGLE-4 TOGGLE-9 TOGGLE-14 TOGGLE-5 
         TOGGLE-10 TOGGLE-6 TOGGLE-11 EDITOR-1 RECT-1 
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

