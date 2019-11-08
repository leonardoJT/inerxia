&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
    
/*DEFINE OUTPUT PARAMETER codFacultad AS CHARACTER.
DEFINE OUTPUT PARAMETER codDepartamento AS CHARACTER.
DEFINE OUTPUT PARAMETER pNombre AS CHARACTER.*/

DEFINE OUTPUT PARAMETER codAgencia AS INTEGER.
DEFINE OUTPUT PARAMETER codOutput AS CHARACTER.
    
DEFINE VAR codFacultad AS CHARACTER.
DEFINE VAR codDepartamento AS CHARACTER.
DEFINE VAR pNombre AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwAgencias

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Agencias Facultades

/* Definitions for BROWSE brwAgencias                                   */
&Scoped-define FIELDS-IN-QUERY-brwAgencias Agencias.Agencia Agencias.Nombre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwAgencias 
&Scoped-define QUERY-STRING-brwAgencias FOR EACH Agencias ~
      WHERE Agencias.Estado = 1 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwAgencias OPEN QUERY brwAgencias FOR EACH Agencias ~
      WHERE Agencias.Estado = 1 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwAgencias Agencias
&Scoped-define FIRST-TABLE-IN-QUERY-brwAgencias Agencias


/* Definitions for BROWSE brwDEPARTAMENTOS                              */
&Scoped-define FIELDS-IN-QUERY-brwDEPARTAMENTOS SUBSTRING(Facultades.codigo,3) Facultades.nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwDEPARTAMENTOS   
&Scoped-define SELF-NAME brwDEPARTAMENTOS
&Scoped-define QUERY-STRING-brwDEPARTAMENTOS FOR EACH Facultades WHERE facultades.agencia = codAgencia                                               AND Facultades.tipo = "D"                                               AND SUBSTRING(facultades.codigo, ~
      1, ~
      2) = codFacultad NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwDEPARTAMENTOS OPEN QUERY {&SELF-NAME} FOR EACH Facultades WHERE facultades.agencia = codAgencia                                               AND Facultades.tipo = "D"                                               AND SUBSTRING(facultades.codigo, ~
      1, ~
      2) = codFacultad NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwDEPARTAMENTOS Facultades
&Scoped-define FIRST-TABLE-IN-QUERY-brwDEPARTAMENTOS Facultades


/* Definitions for BROWSE brwFacultades                                 */
&Scoped-define FIELDS-IN-QUERY-brwFacultades Facultades.codigo ~
Facultades.nombre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwFacultades 
&Scoped-define QUERY-STRING-brwFacultades FOR EACH Facultades ~
      WHERE Facultades.tipo = "F" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwFacultades OPEN QUERY brwFacultades FOR EACH Facultades ~
      WHERE Facultades.tipo = "F" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwFacultades Facultades
&Scoped-define FIRST-TABLE-IN-QUERY-brwFacultades Facultades


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwAgencias}~
    ~{&OPEN-QUERY-brwDEPARTAMENTOS}~
    ~{&OPEN-QUERY-brwFacultades}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwAgencias brwFacultades brwDEPARTAMENTOS ~
btnCancelar btnAceptar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAceptar 
     LABEL "Aceptar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON btnCancelar 
     LABEL "Cancelar" 
     SIZE 15 BY 1.12.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwAgencias FOR 
      Agencias SCROLLING.

DEFINE QUERY brwDEPARTAMENTOS FOR 
      Facultades SCROLLING.

DEFINE QUERY brwFacultades FOR 
      Facultades SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwAgencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwAgencias C-Win _STRUCTURED
  QUERY brwAgencias NO-LOCK DISPLAY
      Agencias.Agencia FORMAT "99":U
      Agencias.Nombre FORMAT "X(40)":U WIDTH 15.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 26.86 BY 9.42 FIT-LAST-COLUMN.

DEFINE BROWSE brwDEPARTAMENTOS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwDEPARTAMENTOS C-Win _FREEFORM
  QUERY brwDEPARTAMENTOS NO-LOCK DISPLAY
      SUBSTRING(Facultades.codigo,3) COLUMN-LABEL "Cod" FORMAT "X(3)":U
      Facultades.nombre COLUMN-LABEL "Departamento" FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 28.86 BY 9.42 FIT-LAST-COLUMN.

DEFINE BROWSE brwFacultades
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwFacultades C-Win _STRUCTURED
  QUERY brwFacultades NO-LOCK DISPLAY
      Facultades.codigo COLUMN-LABEL "Cod" FORMAT "x(2)":U
      Facultades.nombre COLUMN-LABEL "Facultad" FORMAT "x(20)":U
            WIDTH 20.72
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 28 BY 9.42 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwAgencias AT ROW 1.85 COL 1.14 WIDGET-ID 400
     brwFacultades AT ROW 1.85 COL 28 WIDGET-ID 200
     brwDEPARTAMENTOS AT ROW 1.85 COL 56.14 WIDGET-ID 300
     btnCancelar AT ROW 11.54 COL 53.72 WIDGET-ID 10
     btnAceptar AT ROW 11.54 COL 69.72 WIDGET-ID 8
     "             Doble clic para seleccionar el Departamento" VIEW-AS TEXT
          SIZE 84 BY .81 AT ROW 1 COL 1 WIDGET-ID 6
          BGCOLOR 3 FONT 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.14 BY 12 WIDGET-ID 100.


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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Administración de Facultades"
         HEIGHT             = 12
         WIDTH              = 84.14
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 84.14
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 84.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brwAgencias TEXT-1 DEFAULT-FRAME */
/* BROWSE-TAB brwFacultades brwAgencias DEFAULT-FRAME */
/* BROWSE-TAB brwDEPARTAMENTOS brwFacultades DEFAULT-FRAME */
ASSIGN 
       Agencias.Agencia:AUTO-RESIZE IN BROWSE brwAgencias = TRUE
       Agencias.Nombre:AUTO-RESIZE IN BROWSE brwAgencias = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwAgencias
/* Query rebuild information for BROWSE brwAgencias
     _TblList          = "bdcentral.Agencias"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Agencias.Estado = 1"
     _FldNameList[1]   > bdcentral.Agencias.Agencia
"Agencias.Agencia" ? "99" "integer" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdcentral.Agencias.Nombre
"Agencias.Nombre" ? ? "character" ? ? ? ? ? ? no ? no no "15.86" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwAgencias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwDEPARTAMENTOS
/* Query rebuild information for BROWSE brwDEPARTAMENTOS
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Facultades WHERE facultades.agencia = codAgencia
                                              AND Facultades.tipo = "D"
                                              AND SUBSTRING(facultades.codigo,1,2) = codFacultad NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Facultades.tipo = ""F"""
     _Query            is OPENED
*/  /* BROWSE brwDEPARTAMENTOS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwFacultades
/* Query rebuild information for BROWSE brwFacultades
     _TblList          = "bdcentral.Facultades"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Facultades.tipo = ""F"""
     _FldNameList[1]   > bdcentral.Facultades.codigo
"Facultades.codigo" "Cod" "x(2)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdcentral.Facultades.nombre
"Facultades.nombre" "Facultad" ? "character" ? ? ? ? ? ? no ? no no "20.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwFacultades */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Administración de Facultades */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Administración de Facultades */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwAgencias
&Scoped-define SELF-NAME brwAgencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwAgencias C-Win
ON ENTRY OF brwAgencias IN FRAME DEFAULT-FRAME
DO:
    codAgencia = agencias.agencia.

    OPEN QUERY brwFacultades FOR EACH Facultades WHERE facultades.agencia = codAgencia
                                                   AND facultades.tipo = "F" NO-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwAgencias C-Win
ON MOUSE-SELECT-CLICK OF brwAgencias IN FRAME DEFAULT-FRAME
DO:
    codAgencia = agencias.agencia.
    codDepartamento = "00".
    codOutput = "00" + "000".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwAgencias C-Win
ON MOUSE-SELECT-DBLCLICK OF brwAgencias IN FRAME DEFAULT-FRAME
DO:
    codAgencia = agencias.agencia.
    codDepartamento = "00".
    codOutput = "00" + "000".

    APPLY "choose" TO btnAceptar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwAgencias C-Win
ON VALUE-CHANGED OF brwAgencias IN FRAME DEFAULT-FRAME
DO:
    codAgencia = agencias.agencia.

    OPEN QUERY brwFacultades FOR EACH facultades WHERE facultades.agencia = codAgencia
                                                   AND facultades.tipo = "F" NO-LOCK INDEXED-REPOSITION.

    IF AVAILABLE facultades THEN DO:
        codFacultad = facultades.codigo.
        codDepartamento = facultades.codigo.
        codOutput = facultad.codigo + "000".
    END.
    ELSE DO:
        codFacultad = "".
        codDepartamento = "".
        codOutput = "".
    END.

    OPEN QUERY brwDepartamentos FOR EACH Facultades WHERE facultades.agencia = codAgencia
                                                      AND Facultades.tipo = "D"
                                                      AND SUBSTRING(facultades.codigo,1,2) = codFacultad NO-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwDEPARTAMENTOS
&Scoped-define SELF-NAME brwDEPARTAMENTOS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwDEPARTAMENTOS C-Win
ON MOUSE-SELECT-CLICK OF brwDEPARTAMENTOS IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE facultades THEN DO:
        codAgencia = facultades.agencia.
        codDepartamento = facultad.codigo.
        codOutput = facultad.codigo.
        pNombre = facultad.nombre.
    END.
    ELSE DO:
        codDepartamento = "".
        codOutput = "".
        pNombre = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwDEPARTAMENTOS C-Win
ON MOUSE-SELECT-DBLCLICK OF brwDEPARTAMENTOS IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE facultades THEN DO:
        codAgencia = facultades.agencia.
        codDepartamento = facultad.codigo.
        codOutput = facultad.codigo.
        pNombre = facultad.nombre.
    END.
    ELSE DO:
        codDepartamento = "".
        codOutput = "".
        pNombre = "".
    END.

    APPLY "choose" TO btnAceptar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwFacultades
&Scoped-define SELF-NAME brwFacultades
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwFacultades C-Win
ON ENTRY OF brwFacultades IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE facultades THEN
        codFacultad = facultades.codigo.
    ELSE
        codFacultad = "".

    OPEN QUERY brwDepartamentos FOR EACH Facultades WHERE facultades.agencia = codAgencia
                                                      AND Facultades.tipo = "D"
                                                      AND SUBSTRING(facultades.codigo,1,2) = codFacultad NO-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwFacultades C-Win
ON MOUSE-SELECT-CLICK OF brwFacultades IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE facultades THEN DO:
        codDepartamento = facultad.codigo.
        codOutput = facultad.codigo + "000".
    END.
    ELSE DO:
        codDepartamento = "".
        codOutput = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwFacultades C-Win
ON MOUSE-SELECT-DBLCLICK OF brwFacultades IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE facultades THEN DO:
        codDepartamento = facultades.codigo.
        codOutput = facultad.codigo + "000".
    END.
    ELSE DO:
        codDepartamento = "".
        codOutput = "".
    END.

    APPLY "choose" TO btnAceptar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwFacultades C-Win
ON VALUE-CHANGED OF brwFacultades IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE facultades THEN DO:
        codFacultad = facultades.codigo.
        codDepartamento = facultades.codigo.
        codOutput = facultad.codigo + "000".
    END.
    ELSE DO:
        codFacultad = "".
        codDepartamento = "".
        codOutput = "".
    END.
        
    OPEN QUERY brwDepartamentos FOR EACH Facultades WHERE facultades.agencia = codAgencia
                                                      AND Facultades.tipo = "D"
                                                      AND SUBSTRING(facultades.codigo,1,2) = codFacultad NO-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAceptar C-Win
ON CHOOSE OF btnAceptar IN FRAME DEFAULT-FRAME /* Aceptar */
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


&Scoped-define SELF-NAME btnCancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancelar C-Win
ON CHOOSE OF btnCancelar IN FRAME DEFAULT-FRAME /* Cancelar */
DO:
    codOutput = "".

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


&Scoped-define BROWSE-NAME brwAgencias
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE brwAgencias brwFacultades brwDEPARTAMENTOS btnCancelar btnAceptar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

