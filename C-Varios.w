&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Tipos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Tipos 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER WTipo LIKE Varios.Tipo.
DEFINE OUTPUT PARAMETER WCod  LIKE Varios.Codigo.
DEFINE OUTPUT PARAMETER WNom  LIKE Varios.Descripcion.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR_Varios

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Varios

/* Definitions for BROWSE BR_Varios                                     */
&Scoped-define FIELDS-IN-QUERY-BR_Varios Varios.Descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR_Varios   
&Scoped-define SELF-NAME BR_Varios
&Scoped-define QUERY-STRING-BR_Varios FOR EACH Varios NO-LOCK      WHERE  Varios.Tipo EQ WTipo AND Varios.Estado = 1         BY  Varios.Descripcion INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR_Varios OPEN QUERY {&SELF-NAME} FOR EACH Varios NO-LOCK      WHERE  Varios.Tipo EQ WTipo AND Varios.Estado = 1         BY  Varios.Descripcion INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR_Varios Varios
&Scoped-define FIRST-TABLE-IN-QUERY-BR_Varios Varios


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR_Varios}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BR_Varios W_Buscar BUTTON-117 BTn_Salir 
&Scoped-Define DISPLAYED-OBJECTS W_NomTipo W_Buscar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Tipos AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 95" 
     SIZE 10 BY 1.65.

DEFINE BUTTON BUTTON-117 
     LABEL "Ver Todos" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE W_Buscar AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTipo AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1
     FGCOLOR 7  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR_Varios FOR 
      Varios SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR_Varios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR_Varios C-Tipos _FREEFORM
  QUERY BR_Varios NO-LOCK DISPLAY
      Varios.Descripcion FORMAT "X(50)":U WIDTH 25.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 9.15
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .65 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BR_Varios AT ROW 1.27 COL 2
     W_NomTipo AT ROW 10.42 COL 2 NO-LABEL
     W_Buscar AT ROW 11.5 COL 2 NO-LABEL
     BUTTON-117 AT ROW 12.85 COL 2
     BTn_Salir AT ROW 12.85 COL 37
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 46.72 BY 13.81
         BGCOLOR 17 FONT 5.


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
  CREATE WINDOW C-Tipos ASSIGN
         HIDDEN             = YES
         TITLE              = "Escoja el tipo de"
         HEIGHT             = 13.81
         WIDTH              = 46.72
         MAX-HEIGHT         = 19.08
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 19.08
         VIRTUAL-WIDTH      = 80
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
/* SETTINGS FOR WINDOW C-Tipos
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BR_Varios 1 DEFAULT-FRAME */
ASSIGN 
       BR_Varios:SEPARATOR-FGCOLOR IN FRAME DEFAULT-FRAME      = 1.

/* SETTINGS FOR FILL-IN W_Buscar IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_NomTipo IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Tipos)
THEN C-Tipos:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR_Varios
/* Query rebuild information for BROWSE BR_Varios
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Varios NO-LOCK
     WHERE  Varios.Tipo EQ WTipo AND Varios.Estado = 1
        BY  Varios.Descripcion INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Varios.Clase"
     _Query            is OPENED
*/  /* BROWSE BR_Varios */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Tipos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Tipos C-Tipos
ON END-ERROR OF C-Tipos /* Escoja el tipo de */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Tipos C-Tipos
ON WINDOW-CLOSE OF C-Tipos /* Escoja el tipo de */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR_Varios
&Scoped-define SELF-NAME BR_Varios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR_Varios C-Tipos
ON MOUSE-SELECT-DBLCLICK OF BR_Varios IN FRAME DEFAULT-FRAME
DO:
  APPLY 'choose' TO btn_salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTn_Salir C-Tipos
ON CHOOSE OF BTn_Salir IN FRAME DEFAULT-FRAME /* Button 95 */
DO:
  IF Br_Varios:NUM-SELECTED-ROWS > 0 THEN                 
     ASSIGN WNom  = Varios.Descripcion
            WCod  = Varios.Codigo.
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


&Scoped-define SELF-NAME BUTTON-117
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-117 C-Tipos
ON CHOOSE OF BUTTON-117 IN FRAME DEFAULT-FRAME /* Ver Todos */
DO:
  OPEN QUERY Br_Varios FOR EACH Varios NO-LOCK
       WHERE Varios.Tipo = WTipo AND Varios.Estado = 1
          BY Varios.Descripcion INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Buscar C-Tipos
ON LEAVE OF W_Buscar IN FRAME DEFAULT-FRAME
DO:
OPEN QUERY Br_Varios FOR EACH Varios
      WHERE Varios.Tipo EQ WTipo 
      AND Varios.Estado EQ 1
      AND Varios.descripcion BEGINS W_Buscar:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Tipos 


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
  FIND Cfg_Varios WHERE Cfg_Varios.Tipo EQ WTipo NO-LOCK NO-ERROR.
  IF AVAILABLE(Cfg_Varios) THEN
     ASSIGN W_NomTipo:SCREEN-VALUE = "Buscar " + Cfg_Varios.Descripcion
            C-Tipos:TITLE = "Escoja el tipo de " + Cfg_Varios.Descripcion.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Tipos  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Tipos)
  THEN DELETE WIDGET C-Tipos.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Tipos  _DEFAULT-ENABLE
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
  DISPLAY W_NomTipo W_Buscar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Tipos.
  ENABLE BR_Varios W_Buscar BUTTON-117 BTn_Salir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Tipos.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Tipos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

