&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  DEFINE INPUT  PARAMETER P_Agencia  LIKE Agencias.Agencia.
  DEFINE OUTPUT PARAMETER P_Cod      LIKE Empresas.Cod_Empresa NO-UNDO.
  DEFINE OUTPUT PARAMETER P_Nit      LIKE Empresas.Nit.
  DEFINE OUTPUT PARAMETER P_AgeEmp   LIKE Agencias.Agencia.
  DEFINE OUTPUT PARAMETER P_Nom      AS CHARACTER FORMAT "X(30)".
   
  DEFINE SHARED VAR       W_Manija   AS HANDLE.
  DEFINE        VAR       W_Rpta     AS LOGICAL.

  DEFINE TEMP-TABLE TEmpresas
    FIELD TAgencia LIKE Empresas.Agencia
    FIELD TCodigo  LIKE Empresas.Cod_Empresa
    FIELD TNit     LIKE Empresas.Nit
    FIELD TNombre  AS CHARACTER FORMAT "X(30)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_Empresas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TEmpresas

/* Definitions for BROWSE Br_Empresas                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Empresas TAgencia TCodigo TNit TNombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Empresas   
&Scoped-define SELF-NAME Br_Empresas
&Scoped-define OPEN-QUERY-Br_Empresas ASSIGN FRAME {&FRAME-NAME} T_EmpresasAgencia. IF T_EmpresasAgencia THEN   OPEN QUERY Br_Empresas     FOR EACH TEmpresas WHERE Tempresas.TAgencia EQ P_Agencia NO-LOCK         BY TNombre INDEXED-REPOSITION. ELSE   OPEN QUERY Br_Empresas     FOR EACH Tempresas NO-LOCK BY TNombre INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Empresas TEmpresas
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Empresas TEmpresas


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-127 Br_Empresas R_Busca F_Busca ~
T_EmpresasAgencia BUTTON-90 BUTTON-91 
&Scoped-Define DISPLAYED-OBJECTS R_Busca F_Busca T_EmpresasAgencia 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-90 AUTO-END-KEY 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 90" 
     SIZE 7 BY 1.62.

DEFINE BUTTON BUTTON-91 
     LABEL "Ver Todas las Empresas" 
     SIZE 25 BY 1.08.

DEFINE VARIABLE F_Busca AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Codigo", 1,
"Nit", 2,
"Nombre", 3
     SIZE 33 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-127
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 1.88.

DEFINE VARIABLE T_EmpresasAgencia AS LOGICAL INITIAL yes 
     LABEL "Solamente Empresas de esta Agencia" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Empresas FOR 
      TEmpresas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Empresas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Empresas C-Win _FREEFORM
  QUERY Br_Empresas NO-LOCK DISPLAY
      TAgencia FORMAT "999":U COLUMN-LABEL "Agencia"
      TCodigo  FORMAT "9999":U COLUMN-LABEL "Código"
      TNit FORMAT "X(12)":U COLUMN-LABEL "Nit"
      TNombre FORMAT "X(35)":U COLUMN-LABEL "Nombre de la Empresa"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 74 BY 7
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Br_Empresas AT ROW 1.27 COL 2
     R_Busca AT ROW 9.35 COL 4 NO-LABEL
     F_Busca AT ROW 9.35 COL 36 COLON-ALIGNED NO-LABEL
     T_EmpresasAgencia AT ROW 10.96 COL 3
     BUTTON-90 AT ROW 11.23 COL 69
     BUTTON-91 AT ROW 12.04 COL 3
     " Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.54 COL 3
          FGCOLOR 7 
     RECT-127 AT ROW 8.81 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.86 BY 12.5
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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Consulta de Empresas"
         HEIGHT             = 12.5
         WIDTH              = 76.86
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("imagenes/magnify0.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/magnify0.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB Br_Empresas RECT-127 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Empresas
/* Query rebuild information for BROWSE Br_Empresas
     _START_FREEFORM
ASSIGN FRAME {&FRAME-NAME} T_EmpresasAgencia.
IF T_EmpresasAgencia THEN
  OPEN QUERY Br_Empresas
    FOR EACH TEmpresas WHERE Tempresas.TAgencia EQ P_Agencia NO-LOCK
        BY TNombre INDEXED-REPOSITION.
ELSE
  OPEN QUERY Br_Empresas
    FOR EACH Tempresas NO-LOCK BY TNombre INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Empresas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SFG - Consulta de Empresas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SFG - Consulta de Empresas */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Empresas
&Scoped-define SELF-NAME Br_Empresas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Empresas C-Win
ON MOUSE-SELECT-DBLCLICK OF Br_Empresas IN FRAME DEFAULT-FRAME
DO:
  APPLY "Value-Changed" TO SELF.
  APPLY "CHOOSE" TO Button-90.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Empresas C-Win
ON VALUE-CHANGED OF Br_Empresas IN FRAME DEFAULT-FRAME
DO:
  IF AVAIL(Tempresas) THEN
     ASSIGN P_Cod    = Tempresas.TCodigo
            P_Nit    = Tempresas.TNit
            P_AgeEmp = Tempresas.TAgencia
            P_Nom    = Tempresas.TNombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-90
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-90 C-Win
ON CHOOSE OF BUTTON-90 IN FRAME DEFAULT-FRAME /* Button 90 */
DO:  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-91
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-91 C-Win
ON CHOOSE OF BUTTON-91 IN FRAME DEFAULT-FRAME /* Ver Todas las Empresas */
DO:
  ASSIGN FRAME {&FRAME-NAME} T_EmpresasAgencia.
  IF T_EmpresasAgencia EQ YES THEN
    OPEN QUERY Br_Empresas FOR EACH Tempresas WHERE Tempresas.TAgencia EQ P_Agencia NO-LOCK INDEXED-REPOSITION.  
  ELSE
    OPEN QUERY Br_Empresas FOR EACH TEmpresas NO-LOCK BY TNombre INDEXED-REPOSITION.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Busca C-Win
ON LEAVE OF F_Busca IN FRAME DEFAULT-FRAME
DO:
  ASSIGN FRAME {&FRAME-NAME} F_Busca R_Busca T_EmpresasAgencia.
  IF T_EmpresasAgencia THEN
  DO:
    CASE R_Busca:
      WHEN 1 THEN
        OPEN QUERY Br_Empresas FOR EACH TEmpresas  WHERE 
                                        TEmpresas.TAgencia EQ P_Agencia AND
                                        TEmpresas.TCodigo EQ INTEGER(F_Busca)
                                        
                                         NO-LOCK BY TNombre INDEXED-REPOSITION.
      WHEN 2 THEN
        OPEN QUERY Br_Empresas FOR EACH TEmpresas WHERE 
                                        TEmpresas.TAgencia EQ P_Agencia AND
                                        TEmpresas.TNit BEGINS F_Busca
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 3 THEN
        OPEN QUERY Br_Empresas FOR EACH TEmpresas WHERE 
                                        TEmpresas.TAgencia EQ P_Agencia AND
                                        TEmpresas.TNombre BEGINS F_Busca
                                        NO-LOCK BY TNombre INDEXED-REPOSITION.
    END CASE.
  END.
  ELSE
  DO:
    CASE R_Busca:
      WHEN 1 THEN
        OPEN QUERY Br_Empresas FOR EACH TEmpresas  WHERE 
                                         TEmpresas.TCodigo EQ INTEGER(F_Busca)
                                         NO-LOCK BY TNombre INDEXED-REPOSITION.
      WHEN 2 THEN
        OPEN QUERY Br_Empresas FOR EACH TEmpresas WHERE 
                                        TEmpresas.TNit BEGINS F_Busca
                                         NO-LOCK BY TNombre INDEXED-REPOSITION.
      WHEN 3 THEN
        OPEN QUERY Br_Empresas FOR EACH TEmpresas WHERE 
                                        TEmpresas.TNombre BEGINS F_Busca
                                        NO-LOCK BY TNombre INDEXED-REPOSITION.
    END CASE.
  END.

  F_Busca = "".
  DISPLAY F_Busca WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  RUN Inicializar_Temporal.
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
  DISPLAY R_Busca F_Busca T_EmpresasAgencia 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-127 Br_Empresas R_Busca F_Busca T_EmpresasAgencia BUTTON-90 
         BUTTON-91 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Temporal C-Win 
PROCEDURE Inicializar_Temporal :
FOR EACH Empresas NO-LOCK:
  CREATE TEmpresas.
  ASSIGN Tempresas.TAgencia = Empresas.Agencia
         Tempresas.TCodigo  = Empresas.Cod_Empresa
         TEmpresas.TNit     = Empresas.Nit
         TEmpresas.TNombre  = Empresas.Alias_Empresa.
  /*FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN Tempresas.TNombre = Empresas.ALIAS_Empresa.*/
  /*Corregido para mostrar el alias, no el nombre */
      /* Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.*/
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

