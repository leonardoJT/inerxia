&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  DEFINE OUTPUT PARAMETER P_Nit      LIKE Clientes.Nit.
  DEFINE OUTPUT PARAMETER p_id       LIKE Garantias.Identificacion_Bien.
  DEFINE OUTPUT PARAMETER P_tp       LIKE Garantias.Tipo_Garantia.
  DEFINE OUTPUT PARAMETER P_Cd       LIKE Garantias.Cod_Credito.
  DEFINE OUTPUT PARAMETER P_Nc       LIKE Garantias.Num_Credito.
  DEFINE OUTPUT PARAMETER P_NS       LIKE Garantias.Num_Solicitud.
  
  
  DEFINE SHARED VAR       W_Manija   AS HANDLE.
  DEFINE        VAR       W_Rpta     AS LOGICAL.
  DEFINE VAR    W_TipGar AS CHARACTER FORMAT "X(15)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_Garantias

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Garantias

/* Definitions for BROWSE Br_Garantias                                  */
&Scoped-define FIELDS-IN-QUERY-Br_Garantias Garantias.Agencia Garantias.Nit Garantias.Identificacion_Bien Garantias.Nom_Bien Garantias.Num_Solicitud Garantias.Num_Credito W_TipGar   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Garantias   
&Scoped-define SELF-NAME Br_Garantias
&Scoped-define QUERY-STRING-Br_Garantias FOR EACH Garantias WHERE Garantias.Num_Credito GT 0 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Garantias OPEN QUERY Br_Garantias   FOR EACH Garantias WHERE Garantias.Num_Credito GT 0 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Garantias Garantias
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Garantias Garantias


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-Br_Garantias}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Br_Garantias Btn_Salir F_Busca R_Busca ~
RECT-127 
&Scoped-Define DISPLAYED-OBJECTS F_Busca R_Busca 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Salir AUTO-END-KEY 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 90" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE F_Busca AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Id.Garantia", 1,
"Num.Solicitud", 2,
"Num.Credito", 3,
"Nit", 4
     SIZE 47 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-127
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 73 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Garantias FOR 
      Garantias SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Garantias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Garantias C-Win _FREEFORM
  QUERY Br_Garantias NO-LOCK DISPLAY
      Garantias.Agencia COLUMN-LABEL "Age"
  Garantias.Nit     COLUMN-LABEL "Nit"
  Garantias.Identificacion_Bien COLUMN-LABEL "Id.Bien"
  Garantias.Nom_Bien COLUMN-LABEL "Nombre" FORMAT "X(35)"
  Garantias.Num_Solicitud COLUMN-LABEL "Num.Sol"
  Garantias.Num_Credito COLUMN-LABEL "Num.Cre"
  W_TipGar COLUMN-LABEL "Tip"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84 BY 7
         BGCOLOR 15 FONT 4 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Br_Garantias AT ROW 1.27 COL 2
     Btn_Salir AT ROW 8.81 COL 79
     F_Busca AT ROW 9.08 COL 51 COLON-ALIGNED NO-LABEL
     R_Busca AT ROW 9.35 COL 4 NO-LABEL
     RECT-127 AT ROW 8.81 COL 2
     " Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.54 COL 3
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.72 BY 9.58
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
         TITLE              = "SFG - Consulta de Garantias de Créditos Aprobados"
         HEIGHT             = 9.58
         WIDTH              = 86.72
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 104.57
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 104.57
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
                                                                        */
/* BROWSE-TAB Br_Garantias 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Garantias
/* Query rebuild information for BROWSE Br_Garantias
     _START_FREEFORM
OPEN QUERY Br_Garantias
  FOR EACH Garantias WHERE Garantias.Num_Credito GT 0 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Garantias */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SFG - Consulta de Garantias de Créditos Aprobados */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SFG - Consulta de Garantias de Créditos Aprobados */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Garantias
&Scoped-define SELF-NAME Br_Garantias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Garantias C-Win
ON MOUSE-SELECT-DBLCLICK OF Br_Garantias IN FRAME DEFAULT-FRAME
DO:
  APPLY "choose" TO Btn_Salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Garantias C-Win
ON ROW-DISPLAY OF Br_Garantias IN FRAME DEFAULT-FRAME
DO:
  CASE Garantias.Tipo_Garantia:
   WHEN 1 THEN W_TipGar = "Propiedad".
   WHEN 2 THEN W_TipGar = "Vehículo".
   WHEN 3 THEN W_TipGar = "Inversion".
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir C-Win
ON CHOOSE OF Btn_Salir IN FRAME DEFAULT-FRAME /* Button 90 */
DO:
  IF Br_Garantias:NUM-SELECTED-ROWS > 0 THEN                 
     ASSIGN P_Nit      = Garantias.Nit
            P_Id       = Garantias.Identificacion_Bien
            P_TP       = Garantias.Tipo_Garantia
            P_Cd       = Garantias.Cod_Credito
            P_NC       = Garantias.Num_Credito
            P_NS       = Garantias.Num_Solicitud.
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


&Scoped-define SELF-NAME F_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Busca C-Win
ON LEAVE OF F_Busca IN FRAME DEFAULT-FRAME
DO:
  ASSIGN FRAME {&FRAME-NAME} F_Busca R_Busca.
    CASE R_Busca:
      WHEN 1 THEN
        OPEN QUERY Br_Garantias FOR EACH Garantias WHERE 
                                        Garantias.Identificacion_Bien EQ F_Busca AND 
                                        Garantias.Num_Credito GT 0 AND
                                        Garantias.Estado EQ 1    AND 
                                        Garantias.Fec_Retiro EQ ?
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 2 THEN
        OPEN QUERY Br_Garantias FOR EACH Garantias WHERE 
                                        Garantias.Num_Solicitud EQ DECIMAL(F_Busca) AND 
                                        Garantias.Num_Credito GT 0 AND
                                        Garantias.Estado EQ 1             AND
                                        Garantias.Fec_Retiro EQ ?
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 3 THEN
        OPEN QUERY Br_Garantias FOR EACH Garantias WHERE 
                                        Garantias.Num_Credito EQ DECIMAL(F_Busca) AND 
                                        Garantias.Num_Credito GT 0 AND
                                        Garantias.Estado EQ 1             AND
                                        Garantias.Fec_Retiro EQ ? 
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 4 THEN
        OPEN QUERY Br_Garantias FOR EACH Garantias WHERE 
                                        Garantias.Nit EQ F_Busca AND 
                                        Garantias.Num_Credito GT 0 AND
                                        Garantias.Estado EQ 1                 AND
                                        Garantias.Fec_Retiro EQ ?
                                         NO-LOCK INDEXED-REPOSITION.  
    END CASE.
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
  DISPLAY F_Busca R_Busca 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Br_Garantias Btn_Salir F_Busca R_Busca RECT-127 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

