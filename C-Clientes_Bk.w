&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  DEFINE INPUT  PARAMETER P_Crear    AS INTEGER FORMAT "9". /*1- Boton Crear ENABLE, 2- Boton Crear DISABLE*/
  DEFINE INPUT  PARAMETER P_Agencia  LIKE Agencias.Agencia.
  DEFINE OUTPUT PARAMETER P_Nit      LIKE Clientes.Nit.
  DEFINE OUTPUT PARAMETER p_Nombre   LIKE Clientes.Nombre.
  DEFINE OUTPUT PARAMETER P_Apellido AS CHARACTER FORMAT "X(25)".
  DEFINE OUTPUT PARAMETER P_AgeCli   LIKE Agencias.Agencia.

  DEFINE SHARED VAR       W_Manija   AS HANDLE.
  DEFINE        VAR       W_Rpta     AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_Clientes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Clientes

/* Definitions for BROWSE Br_Clientes                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Clientes Clientes.Agencia Clientes.Nit Clientes.Nombre Clientes.Apellido1 Clientes.Apellido2   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Clientes   
&Scoped-define SELF-NAME Br_Clientes
&Scoped-define OPEN-QUERY-Br_Clientes ASSIGN FRAME {&FRAME-NAME} T_ClientesAgencia. IF T_ClientesAgencia THEN   OPEN QUERY Br_Clientes     FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia NO-LOCK INDEXED-REPOSITION. ELSE   OPEN QUERY Br_Clientes     FOR EACH Clientes NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Clientes Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Clientes Clientes


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmb-tipocliente F_Busca Br_Clientes R_Busca ~
T_ClientesAgencia REstado Btn_Salir BUTTON-91 Btn_Crear RECT-127 
&Scoped-Define DISPLAYED-OBJECTS cmb-tipocliente F_Busca R_Busca ~
T_ClientesAgencia REstado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Crear 
     LABEL "Crear un Cliente Nuevo" 
     SIZE 25 BY 1.08.

DEFINE BUTTON Btn_Salir AUTO-END-KEY 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 90" 
     SIZE 7 BY 1.62.

DEFINE BUTTON BUTTON-91 
     LABEL "Ver Todos los Clientes" 
     SIZE 25 BY 1.08.

DEFINE VARIABLE cmb-tipocliente AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo de Cliente" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00 - Todas","01 - Natural Mayor de Edad","02 - Natural Menor de Edad","03 - Juridica S.A","04 - Juridica C.A" 
     DROP-DOWN-LIST
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Busca AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE REstado AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activos", 1,
"Retirados", 2
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nit", 1,
"Nombre", 2,
"Apellido1", 3,
"Apellido2", 4
     SIZE 67 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-127
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 2.96.

DEFINE VARIABLE T_ClientesAgencia AS LOGICAL INITIAL yes 
     LABEL "Solamente Clientes de esta Agencia" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Clientes FOR 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Clientes C-Win _FREEFORM
  QUERY Br_Clientes NO-LOCK DISPLAY
      Clientes.Agencia FORMAT "999":U
      Clientes.Nit FORMAT "X(12)":U
      Clientes.Nombre FORMAT "X(20)":U
      Clientes.Apellido1 FORMAT "X(15)":U
      Clientes.Apellido2 FORMAT "X(15)":U WIDTH 17.14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 74 BY 7
         BGCOLOR 15 FONT 5 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmb-tipocliente AT ROW 12.04 COL 15 COLON-ALIGNED
     F_Busca AT ROW 10.35 COL 2 COLON-ALIGNED NO-LABEL
     Br_Clientes AT ROW 1.27 COL 2
     R_Busca AT ROW 9.35 COL 7 NO-LABEL
     T_ClientesAgencia AT ROW 13.38 COL 3
     REstado AT ROW 12.04 COL 51 NO-LABEL
     Btn_Salir AT ROW 14.46 COL 67
     BUTTON-91 AT ROW 14.46 COL 2
     Btn_Crear AT ROW 14.46 COL 27
     RECT-127 AT ROW 8.81 COL 2
     " Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.54 COL 3
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.86 BY 15.46
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
         TITLE              = "SFG - Consulta de Clientes"
         HEIGHT             = 15.46
         WIDTH              = 76.86
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         ALWAYS-ON-TOP      = yes
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
   Custom                                                               */
/* BROWSE-TAB Br_Clientes F_Busca DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Clientes
/* Query rebuild information for BROWSE Br_Clientes
     _START_FREEFORM
ASSIGN FRAME {&FRAME-NAME} T_ClientesAgencia.
IF T_ClientesAgencia THEN
  OPEN QUERY Br_Clientes
    FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia NO-LOCK INDEXED-REPOSITION.
ELSE
  OPEN QUERY Br_Clientes
    FOR EACH Clientes NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Clientes */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SFG - Consulta de Clientes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SFG - Consulta de Clientes */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Clientes
&Scoped-define SELF-NAME Br_Clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Clientes C-Win
ON MOUSE-SELECT-DBLCLICK OF Br_Clientes IN FRAME DEFAULT-FRAME
DO:
  APPLY "choose" TO Btn_Salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Crear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Crear C-Win
ON CHOOSE OF Btn_Crear IN FRAME DEFAULT-FRAME /* Crear un Cliente Nuevo */
DO:
 C-Win:MOVE-TO-BOTTOM().
 ASSIGN C-Win:SENSITIVE = FALSE.
              
 RUN W-Tercero_Nvo.R (INPUT-OUTPUT F_Busca).
    
 /*RUN W-Cliente_Sencillo.w.*/
 OPEN QUERY Br_Clientes FOR EACH Clientes NO-LOCK INDEXED-REPOSITION. 
    
 ASSIGN C-Win:SENSITIVE      = TRUE
        F_Busca:SCREEN-VALUE = F_Busca.
    
 APPLY "LEAVE" TO F_Busca.   
 C-Win:MOVE-TO-TOP(). 
     
 /*MESSAGE "Busque de nuevo el Cliente a Insertar en la Pantalla"
        VIEW-AS ALERT-BOX TITLE "Busqueda".*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir C-Win
ON CHOOSE OF Btn_Salir IN FRAME DEFAULT-FRAME /* Button 90 */
DO:
  IF Br_Clientes:NUM-SELECTED-ROWS > 0 THEN                 
     ASSIGN P_Agencia  = Clientes.Agencia
            P_Nit      = Clientes.Nit
            P_Nombre   = Clientes.Nombre
            P_Apellido = Clientes.Apellido1 + " " + Clientes.Apellido2
            P_AgeCli   = Clientes.Agencia.
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


&Scoped-define SELF-NAME BUTTON-91
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-91 C-Win
ON CHOOSE OF BUTTON-91 IN FRAME DEFAULT-FRAME /* Ver Todos los Clientes */
DO:
  ASSIGN FRAME {&FRAME-NAME} T_ClientesAgencia.
  IF T_ClientesAgencia EQ YES THEN
    OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia NO-LOCK INDEXED-REPOSITION.  
  ELSE
    OPEN QUERY Br_Clientes FOR EACH Clientes NO-LOCK INDEXED-REPOSITION.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Busca C-Win
ON LEAVE OF F_Busca IN FRAME DEFAULT-FRAME
DO:
  ASSIGN FRAME {&FRAME-NAME} F_Busca R_Busca T_ClientesAgencia REstado.
  IF T_ClientesAgencia THEN
  DO:
    CASE R_Busca:
      WHEN 1 THEN
        OPEN QUERY Br_Clientes FOR EACH Clientes  WHERE 
                                        Clientes.Agencia EQ P_Agencia AND
                                        Clientes.Nit EQ F_Busca AND 
                                        Clientes.Estado EQ REstado
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 2 THEN
        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE 
                                        Clientes.Agencia EQ P_Agencia AND
                                        Clientes.Nombre CONTAINS F_Busca AND 
                                        Clientes.Estado EQ REstado 
                                         NO-LOCK BY Clientes.Apellido1 INDEXED-REPOSITION.
      WHEN 3 THEN
        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE 
                                        Clientes.Agencia EQ P_Agencia AND
                                        Clientes.Apellido1 CONTAINS F_Busca AND 
                                        Clientes.Estado EQ REstado
                                         NO-LOCK BY Clientes.Nombre INDEXED-REPOSITION.
      WHEN 4 THEN
        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE 
                                        Clientes.Agencia EQ P_Agencia AND
                                        Clientes.Apellido2 CONTAINS F_Busca AND 
                                        Clientes.Estado EQ REstado
                                         NO-LOCK BY Clientes.Nombre INDEXED-REPOSITION.  
    END CASE.
  END.
  ELSE
  DO:

    CASE R_Busca:
      WHEN 1 THEN
        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE 
                                        Clientes.Nit EQ F_Busca AND 
                                        Clientes.Estado EQ REstado
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 2 THEN
        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE 
                                        Clientes.Nombre CONTAINS F_Busca AND 
                                        Clientes.Estado EQ REstado
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 3 THEN
        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE 
                                        Clientes.Apellido1 CONTAINS F_Busca AND 
                                        Clientes.Estado EQ REstado
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 4 THEN
        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE 
                                        Clientes.Apellido2 CONTAINS F_Busca AND 
                                        Clientes.Estado EQ REstado
                                         NO-LOCK INDEXED-REPOSITION.  
    END CASE.
  END.

  F_Busca = "".
  DISPLAY F_Busca WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME REstado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL REstado C-Win
ON VALUE-CHANGED OF REstado IN FRAME DEFAULT-FRAME
DO:
  APPLY "entry" TO F_Busca.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Busca C-Win
ON MOUSE-SELECT-CLICK OF R_Busca IN FRAME DEFAULT-FRAME
DO:
  ASSIGN R_Busca.
      
  APPLY "ENTRY" TO F_Busca.
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

  RUN enable_UI.
  IF P_Crear EQ 2 THEN
     DISABLE Btn_Crear WITH FRAME {&FRAME-NAME}.
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
  DISPLAY cmb-tipocliente F_Busca R_Busca T_ClientesAgencia REstado 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cmb-tipocliente F_Busca Br_Clientes R_Busca T_ClientesAgencia REstado 
         Btn_Salir BUTTON-91 Btn_Crear RECT-127 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

