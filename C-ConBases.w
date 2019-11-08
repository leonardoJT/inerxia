&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/* Parameters Definitions ---                                           */
   DEFINE INPUT-OUTPUT PARAMETER W_Rowid AS ROWID.
           DEFINE        VAR W_Rpta   AS LOGICAL.
/* Local Variable Definitions ---                                       */
  {incluido\variable.i "SHARED"}
  
  DEFINE VARIABLE W_Status     AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Base_Ret

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 Base_Ret.Cod_base Base_Ret.Nombre ~
Base_Ret.Porcentaje 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH Base_Ret ~
      WHERE Base_Ret.Estado = W_Estado NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH Base_Ret ~
      WHERE Base_Ret.Estado = W_Estado NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 Base_Ret
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 Base_Ret


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 RADIO-SET-2 W_Estado W_Valor ~
BUTTON-47 RECT-142 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-2 W_Estado W_Valor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-47 AUTO-GO 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Button 47" 
     SIZE 6 BY 1.62.

DEFINE VARIABLE W_Valor AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.08 TOOLTIP "Valor de Selección"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RADIO-SET-2 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Código", 1,
"Nombre", 2,
"Todos", 3
     SIZE 35 BY .69 TOOLTIP "Opción de Selección" NO-UNDO.

DEFINE VARIABLE W_Estado AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 12 BY 1.62 TOOLTIP "Estado de los Grupos a Consultar" NO-UNDO.

DEFINE RECTANGLE RECT-142
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 2.96.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      Base_Ret
    FIELDS(Base_Ret.Cod_base
      Base_Ret.Nombre
      Base_Ret.Porcentaje) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      Base_Ret.Cod_base COLUMN-LABEL "Código" FORMAT "X(4)":U
      Base_Ret.Nombre COLUMN-LABEL "Nombre" FORMAT "X(30)":U
      Base_Ret.Porcentaje FORMAT "99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 4.85
         BGCOLOR 15 FONT 5 TOOLTIP "Con Doble Click del Mouse se Selecciona el Registro".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-2 AT ROW 1.27 COL 2
     RADIO-SET-2 AT ROW 7.46 COL 16 NO-LABEL
     W_Estado AT ROW 7.73 COL 3 NO-LABEL
     W_Valor AT ROW 8.27 COL 25 COLON-ALIGNED NO-LABEL
     BUTTON-47 AT ROW 10.15 COL 47
     RECT-142 AT ROW 6.92 COL 2
     "  Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 29 BY 1.08 AT ROW 6.38 COL 4
          FGCOLOR 7 
     "Nota:" VIEW-AS TEXT
          SIZE 5 BY .81 AT ROW 10.15 COL 2
          FGCOLOR 7 
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 10.15 COL 7
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 10.96 COL 7
          FONT 4
     SPACE(9.13) SKIP(0.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 17 FONT 5
         TITLE "Consulta de Bases de Retención".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-2 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "bdCentral.Base_Ret"
     _Options          = "NO-LOCK"
     _TblOptList       = "USED"
     _Where[1]         = "Base_Ret.Estado = W_Estado"
     _FldNameList[1]   > bdCentral.Base_Ret.Cod_base
"Base_Ret.Cod_base" "Código" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > bdCentral.Base_Ret.Nombre
"Base_Ret.Nombre" "Nombre" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = bdCentral.Base_Ret.Porcentaje
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Consulta de Bases de Retención */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON ENTRY OF BROWSE-2 IN FRAME Dialog-Frame
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON LEAVE OF BROWSE-2 IN FRAME Dialog-Frame
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME Dialog-Frame
OR RETURN OF {&BROWSE-NAME}
DO:
   ASSIGN W_Rowid = ROWID(Base_Ret).
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-2 Dialog-Frame
ON VALUE-CHANGED OF RADIO-SET-2 IN FRAME Dialog-Frame
DO:
  ASSIGN RADIO-SET-2
         W_Valor:SCREEN-VALUE = "".
  IF NUM-RESULTS("{&BROWSE-NAME}") GT 0 THEN
     CLOSE QUERY {&BROWSE-NAME}.
  
  IF RADIO-SET-2 EQ 3 THEN DO:
     ASSIGN W_Valor:HIDDEN = TRUE.
        {&OPEN-QUERY-{&BROWSE-NAME}}
     RETURN.
  END.
  ELSE DO:
     ASSIGN W_Valor:HIDDEN = FALSE.
     IF RADIO-SET-2 EQ 1 THEN
        next. /*ASSIGN W_Valor:WIDTH-CHARS = 3.*/
     ELSE
     IF RADIO-SET-2 EQ 2 THEN
        ASSIGN W_Valor:FORMAT      = "X(40)".
  END.
  APPLY "ENTRY":U TO W_Valor.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Estado Dialog-Frame
ON VALUE-CHANGED OF W_Estado IN FRAME Dialog-Frame
DO:
  ASSIGN W_Estado.
  IF RADIO-SET-2 EQ 3 THEN DO:
     ASSIGN W_Valor:HIDDEN = TRUE.
     {&OPEN-QUERY-{&BROWSE-NAME}}
  END.
  ELSE RUN Abre_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Valor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Valor Dialog-Frame
ON LEAVE OF W_Valor IN FRAME Dialog-Frame
DO:
   ASSIGN W_Valor.
   RUN Abre_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  APPLY "VALUE-CHANGED":U TO RADIO-SET-2.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Abre_Browser Dialog-Frame 
PROCEDURE Abre_Browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RADIO-SET-2 EQ 1 THEN
         OPEN QUERY BROWSE-2 FOR EACH Base_Ret WHERE Base_Ret.Estado EQ W_Estado
                                               AND Base_Ret.Cod_Base  EQ W_Valor NO-LOCK.
   ELSE
      IF RADIO-SET-2 EQ 2 THEN
         OPEN QUERY BROWSE-2 FOR EACH Base_Ret WHERE Base_Ret.Estado EQ W_Estado 
                                               AND Base_Ret.Nombre BEGINS W_Valor NO-LOCK.
   IF BROWSE BROWSE-2:NUM-ENTRIES EQ 0 THEN RUN MostrarMensaje IN W_Manija (INPUT 64,OUTPUT W_Rpta).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY RADIO-SET-2 W_Estado W_Valor 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-2 RADIO-SET-2 W_Estado W_Valor BUTTON-47 RECT-142 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

