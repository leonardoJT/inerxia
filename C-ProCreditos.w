&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

  DEFINE INPUT  PARAMETER P_Agencia  LIKE Agencias.Agencia.
  DEFINE OUTPUT PARAMETER P_Cod      LIKE Pro_Ahorros.Cod_Ahorro.
  DEFINE OUTPUT PARAMETER p_Nombre   LIKE Pro_Ahorros.Nom_Producto.
  DEFINE OUTPUT PARAMETER P_AgePro   LIKE Agencias.Agencia.

  DEFINE SHARED VAR       W_Manija   AS HANDLE.
  DEFINE        VAR       W_Rpta     AS LOGICAL.
  DEFINE SHARED VAR       W_Agencia  LIKE Agencias.Agencia.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_Productos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Pro_Creditos

/* Definitions for BROWSE Br_Productos                                  */
&Scoped-define FIELDS-IN-QUERY-Br_Productos Pro_Creditos.Cod_Credito Pro_Creditos.Nom_Producto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Productos   
&Scoped-define SELF-NAME Br_Productos
&Scoped-define OPEN-QUERY-Br_Productos ASSIGN FRAME {&FRAME-NAME} T_ProductosAgencia R_Activo. IF T_ProductosAgencia THEN   OPEN QUERY Br_Productos     FOR EACH Pro_Creditos WHERE Pro_Creditos.Agencia EQ P_Agencia AND              Pro_Creditos.Estado EQ R_Activo NO-LOCK INDEXED-REPOSITION. ELSE   OPEN QUERY Br_Productos     FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ R_Activo NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Productos Pro_Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Productos Pro_Creditos


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-127 RECT-228 Br_Productos R_TP R_Busca ~
F_Busca R_Activo T_ProductosAgencia Btn_Salir BUTTON-91 
&Scoped-Define DISPLAYED-OBJECTS R_TP R_Busca F_Busca R_Activo ~
T_ProductosAgencia 

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

DEFINE BUTTON BUTTON-91 
     LABEL "Ver Todos los Productos" 
     SIZE 25 BY 1.08.

DEFINE VARIABLE F_Busca AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Activo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 26 BY 1.12 NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Codigo", 1,
"Nombre", 2
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE R_TP AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Consumo", 1,
"Comercial", 2,
"Hipotec.", 3,
"MicroCred", 4,
"Bien.Y Serv.",5,
"Empleados",6,
"Convenios",7,
"Todos", 8
     SIZE 92.43 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-127
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 1.88.

DEFINE RECTANGLE RECT-228
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 93.72 BY 1.88.

DEFINE VARIABLE T_ProductosAgencia AS LOGICAL INITIAL yes 
     LABEL "Solamente Productos de esta Agencia" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Productos FOR 
      Pro_Creditos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Productos C-Win _FREEFORM
  QUERY Br_Productos NO-LOCK DISPLAY
      Pro_Creditos.Cod_Credito FORMAT "999":U
      Pro_Creditos.Nom_Producto FORMAT "X(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93.86 BY 7
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Br_Productos AT ROW 1.27 COL 2
     R_TP AT ROW 9.35 COL 2.72 NO-LABEL
     R_Busca AT ROW 11.77 COL 7 NO-LABEL
     F_Busca AT ROW 11.77 COL 28 COLON-ALIGNED NO-LABEL
     R_Activo AT ROW 13.12 COL 41 NO-LABEL
     T_ProductosAgencia AT ROW 13.38 COL 3
     Btn_Salir AT ROW 13.38 COL 69
     BUTTON-91 AT ROW 14.35 COL 3
     "Filtrar por Tipo de Productos de Crédito" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 8.54 COL 3
          FGCOLOR 7 
     " Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 10.96 COL 3
          FGCOLOR 7 
     RECT-127 AT ROW 11.23 COL 2
     RECT-228 AT ROW 8.81 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.43 BY 14.58
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
         TITLE              = "SFG - Consulta de Productos de Credito"
         HEIGHT             = 14.58
         WIDTH              = 97.43
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 97.43
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 97.43
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
/* BROWSE-TAB Br_Productos RECT-228 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Productos
/* Query rebuild information for BROWSE Br_Productos
     _START_FREEFORM
ASSIGN FRAME {&FRAME-NAME} T_ProductosAgencia R_Activo.
IF T_ProductosAgencia THEN
  OPEN QUERY Br_Productos
    FOR EACH Pro_Creditos WHERE Pro_Creditos.Agencia EQ P_Agencia AND
             Pro_Creditos.Estado EQ R_Activo NO-LOCK INDEXED-REPOSITION.
ELSE
  OPEN QUERY Br_Productos
    FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ R_Activo NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Productos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SFG - Consulta de Productos de Credito */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SFG - Consulta de Productos de Credito */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Productos
&Scoped-define SELF-NAME Br_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Productos C-Win
ON MOUSE-SELECT-DBLCLICK OF Br_Productos IN FRAME DEFAULT-FRAME
DO:
  APPLY "choose" TO Btn_Salir.
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


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir C-Win
ON CHOOSE OF Btn_Salir IN FRAME DEFAULT-FRAME /* Button 90 */
DO:
  IF Br_Productos:NUM-SELECTED-ROWS > 0 THEN                 
     ASSIGN P_Agencia  = W_Agencia
            P_Cod      = Pro_Creditos.Cod_Credito
            P_Nombre   = Pro_Creditos.Nom_Producto
            P_AgePro   = W_Agencia.
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
ON CHOOSE OF BUTTON-91 IN FRAME DEFAULT-FRAME /* Ver Todos los Productos */
DO:
  ASSIGN FRAME {&FRAME-NAME} T_ProductosAgencia R_Activo.
  IF T_ProductosAgencia EQ YES THEN
    OPEN QUERY Br_Productos FOR EACH Pro_Creditos WHERE 
         Pro_Creditos.Estado  EQ R_Activo NO-LOCK INDEXED-REPOSITION.  
  ELSE
    OPEN QUERY Br_Productos FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ R_Activo NO-LOCK INDEXED-REPOSITION.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Busca C-Win
ON LEAVE OF F_Busca IN FRAME DEFAULT-FRAME
DO:
  ASSIGN FRAME {&FRAME-NAME} R_Activo F_Busca R_Busca T_ProductosAgencia.
  IF T_ProductosAgencia THEN
  DO:
    CASE R_Busca:
      WHEN 1 THEN
        OPEN QUERY Br_Productos FOR EACH Pro_Creditos  WHERE 
                                        Pro_Creditos.Cod_Credito EQ INTEGER(F_Busca) AND 
                                        Pro_Creditos.Estado EQ R_Activo 
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 2 THEN
        OPEN QUERY Br_Productos FOR EACH Pro_Creditos WHERE 
                                        Pro_Creditos.Nom_Producto BEGINS F_Busca AND 
                                        Pro_Creditos.Estado EQ R_Activo
                                        NO-LOCK INDEXED-REPOSITION.
    END CASE.
  END.
  ELSE
  DO:
    CASE R_Busca:
      WHEN 1 THEN
        OPEN QUERY Br_Productos FOR EACH Pro_Creditos WHERE 
                                        Pro_Creditos.Cod_Credito EQ INTEGER(F_Busca) AND 
                                        Pro_Creditos.Estado EQ R_Activo
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 2 THEN
        OPEN QUERY Br_Productos FOR EACH Pro_Creditos WHERE 
                                        Pro_Creditos.Nom_Producto BEGINS F_Busca AND 
                                        Pro_Creditos.Estado EQ R_ACtivo
                                         NO-LOCK INDEXED-REPOSITION.
    END CASE.
  END.

  F_Busca = "".
  DISPLAY F_Busca WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_TP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_TP C-Win
ON VALUE-CHANGED OF R_TP IN FRAME DEFAULT-FRAME
DO:
  ASSIGN FRAME {&FRAME-NAME} R_Tp R_Activo.
  CASE SELF:SCREEN-VALUE:
    WHEN "8" THEN
       OPEN QUERY Br_Productos
           FOR EACH Pro_Creditos WHERE 
                    Pro_Creditos.Estado EQ R_Activo NO-LOCK INDEXED-REPOSITION.
    OTHERWISE
       OPEN QUERY Br_Productos
           FOR EACH Pro_Creditos WHERE 
                    Pro_Creditos.Tip_Credito EQ INTEGER(SELF:SCREEN-VALUE) AND
                    Pro_Creditos.Estado EQ R_Activo NO-LOCK INDEXED-REPOSITION.
  END CASE.
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
  DISPLAY R_TP R_Busca F_Busca R_Activo T_ProductosAgencia 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-127 RECT-228 Br_Productos R_TP R_Busca F_Busca R_Activo 
         T_ProductosAgencia Btn_Salir BUTTON-91 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

