&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT-OUTPUT PARAMETER W_Rowid AS ROWID.

/* Local Variable Definitions ---                                       */

{INCLUIDO\VARIABLE.I "SHARED"}      

DEFINE VARIABLE W_SuperUsu   AS  LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME B_Consulta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Pro_Especiales

/* Definitions for BROWSE B_Consulta                                    */
&Scoped-define FIELDS-IN-QUERY-B_Consulta Pro_Especiales.Agencia Pro_Especiales.Cod_Producto Pro_Especiales.Nom_Producto Pro_Especiales.Estado   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Consulta   
&Scoped-define SELF-NAME B_Consulta
&Scoped-define OPEN-QUERY-B_Consulta IF W_SuperUsu THEN    OPEN QUERY {&SELF-NAME} FOR EACH Pro_Especiales WHERE Pro_Especiales.Estado EQ R_Estado NO-LOCK. ELSE    OPEN QUERY {&SELF-NAME} FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia EQ W_Agencia                                                      AND Pro_Especiales.Estado  EQ R_Estado                                                    NO-LOCK.
&Scoped-define TABLES-IN-QUERY-B_Consulta Pro_Especiales
&Scoped-define FIRST-TABLE-IN-QUERY-B_Consulta Pro_Especiales


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B_Consulta R_seleccion R_estado F_Nombre ~
BUTTON-5 RECT-265 
&Scoped-Define DISPLAYED-OBJECTS R_seleccion R_estado F_Nombre 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-5 AUTO-GO 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 5" 
     SIZE 9 BY 1.62.

DEFINE VARIABLE F_Nombre AS CHARACTER FORMAT "X(31)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .81 TOOLTIP "Item de Consulta"
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE R_estado AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 11 BY 1.88
     FONT 5 NO-UNDO.

DEFINE VARIABLE R_seleccion AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Código", 1,
"Nombre", 2,
"Agencia", 3,
"Todos", 4
     SIZE 50 BY .81
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-265
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 2.96.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_Consulta FOR 
      Pro_Especiales SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Consulta Dialog-Frame _FREEFORM
  QUERY B_Consulta DISPLAY
      Pro_Especiales.Agencia
      Pro_Especiales.Cod_Producto COLUMN-LABEL "Código"
      Pro_Especiales.Nom_Producto FORMAT "X(38)"
      Pro_Especiales.Estado
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 63.29 BY 7.54
         BGCOLOR 15 FGCOLOR 7 FONT 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B_Consulta AT ROW 1.27 COL 1.72
     R_seleccion AT ROW 9.88 COL 14 NO-LABEL
     R_estado AT ROW 10.15 COL 2 NO-LABEL
     F_Nombre AT ROW 10.96 COL 12 COLON-ALIGNED NO-LABEL
     BUTTON-5 AT ROW 12.58 COL 56
     RECT-265 AT ROW 9.35 COL 1
     " Elija el Parámetro de Búsqueda" VIEW-AS TEXT
          SIZE 28 BY 1.04 AT ROW 8.81 COL 3
          FGCOLOR 7 FONT 5
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 41 BY .81 AT ROW 12.58 COL 7
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 39 BY .62 AT ROW 13.38 COL 7
          FONT 4
     "Nota:" VIEW-AS TEXT
          SIZE 4 BY .81 AT ROW 12.58 COL 2
          FGCOLOR 7 
     SPACE(59.71) SKIP(1.06)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 17 FONT 4
         TITLE "Consulta Productos Especiales".


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
/* BROWSE-TAB B_Consulta 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Consulta
/* Query rebuild information for BROWSE B_Consulta
     _START_FREEFORM
IF W_SuperUsu THEN
   OPEN QUERY {&SELF-NAME} FOR EACH Pro_Especiales WHERE Pro_Especiales.Estado EQ R_Estado NO-LOCK.
ELSE
   OPEN QUERY {&SELF-NAME} FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia EQ W_Agencia
                                                     AND Pro_Especiales.Estado  EQ R_Estado
                                                   NO-LOCK.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE B_Consulta */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Consulta Productos Especiales */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Consulta
&Scoped-define SELF-NAME B_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Consulta Dialog-Frame
ON ENTRY OF B_Consulta IN FRAME Dialog-Frame
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Consulta Dialog-Frame
ON LEAVE OF B_Consulta IN FRAME Dialog-Frame
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Consulta Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF B_Consulta IN FRAME Dialog-Frame
OR RETURN OF {&BROWSE-NAME}
DO:
   ASSIGN W_Rowid = ROWID(Pro_Especiales).
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Nombre Dialog-Frame
ON LEAVE OF F_Nombre IN FRAME Dialog-Frame
DO:
  ASSIGN F_Nombre.
  RUN Abre_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_estado Dialog-Frame
ON VALUE-CHANGED OF R_estado IN FRAME Dialog-Frame
DO:
  ASSIGN R_Seleccion
         F_Nombre:SCREEN-VALUE = ""
         R_ESTADO.

  IF NUM-RESULTS("{&BROWSE-NAME}") GT 0 THEN
     CLOSE QUERY {&BROWSE-NAME}.

  IF R_Seleccion EQ 4 THEN DO:
     ASSIGN F_Nombre:VISIBLE = NO.
     IF W_SuperUsu THEN
        OPEN QUERY B_Consulta FOR EACH Pro_Especiales WHERE Pro_Especiales.Estado EQ R_Estado NO-LOCK.
     ELSE
        OPEN QUERY B_Consulta FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia EQ W_Agencia
                                                        AND Pro_Especiales.Estado  EQ R_Estado NO-LOCK.
  END.
  ELSE RUN Abre_Browser.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_seleccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_seleccion Dialog-Frame
ON VALUE-CHANGED OF R_seleccion IN FRAME Dialog-Frame
DO:
  ASSIGN R_Seleccion
         F_Nombre:SCREEN-VALUE = ""
         R_ESTADO.

  IF NUM-RESULTS("{&BROWSE-NAME}") GT 0 THEN
     CLOSE QUERY {&BROWSE-NAME}.

  IF R_Seleccion EQ 4 THEN DO:
     ASSIGN F_Nombre:VISIBLE = NO.
     {&OPEN-QUERY-{&BROWSE-NAME}}
     RETURN.
  END.

  IF R_Seleccion EQ 1 THEN
     ASSIGN F_Nombre:FORMAT      = "X(3)".
  ELSE
     IF R_Seleccion EQ 2 THEN
        ASSIGN F_Nombre:FORMAT      = "X(31)".
     ELSE
        IF R_Seleccion EQ 3 THEN
           ASSIGN F_Nombre:FORMAT      = "X(3)".

  ASSIGN F_NOMBRE:VISIBLE = YES.               
  APPLY "ENTRY":U TO F_Nombre.
  RETURN NO-APPLY.
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
  
  RUN SuperUsuario IN W_Manija (INPUT W_Agencia, INPUT W_Usuario, OUTPUT W_SuperUsu).
  IF W_SuperUsu EQ FALSE THEN 
     R_Seleccion:DELETE("Agencia").

  APPLY "VALUE-CHANGED":U TO R_Seleccion.
    
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
  IF R_Seleccion EQ 1 THEN
     IF W_SuperUsu THEN
        OPEN QUERY B_Consulta FOR EACH Pro_Especiales WHERE Pro_Especiales.Cod_Producto EQ INTEGER(F_Nombre)
                                                        AND Pro_Especiales.Estado       EQ R_Estado
                                                      NO-LOCK.       
     ELSE
        OPEN QUERY B_Consulta FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia      EQ W_Agencia                                                  AND PRO_ESPECIALES.Estado EQ R_Estado
                                                        AND Pro_Especiales.Cod_Producto EQ INTEGER(F_Nombre)
                                                      NO-LOCK.      
  ELSE
     IF R_Seleccion EQ 2 THEN
        IF W_SuperUsu THEN
           OPEN QUERY B_Consulta FOR EACH Pro_Especiales WHERE Pro_Especiales.Nom_Producto CONTAINS F_Nombre
                                                           AND Pro_Especiales.Estado             EQ R_Estado
                                                         NO-LOCK. 
        ELSE         
           OPEN QUERY B_Consulta FOR EACH Pro_Especiales WHERE Pro_Especiales.Nom_Producto CONTAINS F_Nombre
                                                           AND Pro_Especiales.Agencia            EQ W_Agencia 
                                                           AND Pro_Especiales.Estado             EQ R_Estado
                                                         NO-LOCK.
     ELSE
        OPEN QUERY B_Consulta FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia EQ INTEGER(F_Nombre)
                                                        AND Pro_Especiales.Estado  EQ R_Estado
                                                      NO-LOCK.
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
  DISPLAY R_seleccion R_estado F_Nombre 
      WITH FRAME Dialog-Frame.
  ENABLE B_Consulta R_seleccion R_estado F_Nombre BUTTON-5 RECT-265 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

