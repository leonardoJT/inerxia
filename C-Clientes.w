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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_Clientes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Clientes

/* Definitions for BROWSE Br_Clientes                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Clientes Clientes.Agencia Clientes.Nit Clientes.Nombre Clientes.Apellido1 Clientes.Apellido2 Clientes.Cod_Anterior Clientes.Fec_Fallecido   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Clientes   
&Scoped-define SELF-NAME Br_Clientes
&Scoped-define OPEN-QUERY-Br_Clientes ASSIGN FRAME {&FRAME-NAME} T_ClientesAgencia.  IF T_ClientesAgencia THEN     OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia = P_Agencia NO-LOCK INDEXED-REPOSITION. ELSE     OPEN QUERY Br_Clientes FOR EACH Clientes NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Clientes Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Clientes Clientes


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TNit TNombre TApellido1 TApellido2 BUTTON-93 ~
W_CodAnt Br_Clientes T_ClientesAgencia REstado Btn_Salir BUTTON-91 ~
Btn_Crear RECT-1 
&Scoped-Define DISPLAYED-OBJECTS CNit TNit TNombre CNombre TApellido1 ~
CApellido1 TApellido2 CApellido2 W_CodAnt T_ClientesAgencia REstado 

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
     SIZE 22.29 BY 1.08.

DEFINE BUTTON Btn_Salir AUTO-END-KEY 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 90" 
     SIZE 9 BY 2.15.

DEFINE BUTTON BUTTON-91 
     LABEL "Ver Todos los Clientes" 
     SIZE 20.57 BY 1.08.

DEFINE BUTTON BUTTON-93 
     LABEL "Buscar" 
     SIZE 10.29 BY 1.12.

DEFINE VARIABLE CApellido1 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE CApellido2 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE CNit AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE CNombre AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE W_CodAnt AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81 TOOLTIP "Teclee el Código en el anterior Sistema para la búsqueda"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE REstado AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activos", 1,
"Retirados", 2
     SIZE 26.14 BY .73 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 4.35.

DEFINE VARIABLE TApellido1 AS LOGICAL INITIAL no 
     LABEL "Apellido1" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TApellido2 AS LOGICAL INITIAL no 
     LABEL "Apellido2" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TNit AS LOGICAL INITIAL yes 
     LABEL "Nit" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .77 NO-UNDO.

DEFINE VARIABLE TNombre AS LOGICAL INITIAL no 
     LABEL "Nombre" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

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
      Clientes.Nombre FORMAT "X(15)":U
      Clientes.Apellido1 FORMAT "X(12)":U
      Clientes.Apellido2 FORMAT "X(12)":U WIDTH 17.14
      Clientes.Cod_Anterior COLUMN-LABEL "Cod.Ant"
      Clientes.Fec_Fallecido COLUMN-LABEL "Fallecido"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 9.42
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CNit AT ROW 11.69 COL 17.43 COLON-ALIGNED NO-LABEL
     TNit AT ROW 11.69 COL 4.43
     TNombre AT ROW 12.62 COL 4.57
     CNombre AT ROW 12.62 COL 17.57 COLON-ALIGNED NO-LABEL
     TApellido1 AT ROW 13.54 COL 4.57
     CApellido1 AT ROW 13.54 COL 17.57 COLON-ALIGNED NO-LABEL
     TApellido2 AT ROW 14.46 COL 4.57
     CApellido2 AT ROW 14.46 COL 17.57 COLON-ALIGNED NO-LABEL
     BUTTON-93 AT ROW 12.58 COL 77
     W_CodAnt AT ROW 12.31 COL 55.43 COLON-ALIGNED NO-LABEL
     Br_Clientes AT ROW 1.27 COL 2
     T_ClientesAgencia AT ROW 15.77 COL 3.43
     REstado AT ROW 16.65 COL 5.86 NO-LABEL
     Btn_Salir AT ROW 15.81 COL 80
     BUTTON-91 AT ROW 14.19 COL 52.29
     Btn_Crear AT ROW 16.35 COL 42.14
     " Elija el o los parámetros de búsqueda" VIEW-AS TEXT
          SIZE 34 BY .5 AT ROW 10.96 COL 4
          FGCOLOR 7 
     "Por Código Anterior" VIEW-AS TEXT
          SIZE 17.72 BY .69 AT ROW 11.58 COL 53.86
          BGCOLOR 7 FGCOLOR 15 
     RECT-1 AT ROW 11.23 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.14 BY 17.12
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
         HEIGHT             = 17.12
         WIDTH              = 89.57
         MAX-HEIGHT         = 31.42
         MAX-WIDTH          = 164.57
         VIRTUAL-HEIGHT     = 31.42
         VIRTUAL-WIDTH      = 164.57
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB Br_Clientes W_CodAnt DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN CApellido1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CApellido2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CNit IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CNombre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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
    OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia = P_Agencia NO-LOCK INDEXED-REPOSITION.
ELSE
    OPEN QUERY Br_Clientes FOR EACH Clientes NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Clientes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
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
    IF clientes.nit <> "" THEN
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
              
 RUN W-Tercero_Nvo.R (INPUT-OUTPUT CNit).
    
 /*RUN W-Cliente_Sencillo.w.*/
 OPEN QUERY Br_Clientes FOR EACH Clientes NO-LOCK INDEXED-REPOSITION. 
    
 ASSIGN C-Win:SENSITIVE      = TRUE.
    
 APPLY "LEAVE" TO CNit.   
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
        ASSIGN P_Agencia = Clientes.Agencia
               P_Nit = Clientes.Nit
               P_Nombre = Clientes.Nombre
               P_Apellido = Clientes.Apellido1 + " " + Clientes.Apellido2
               P_AgeCli = Clientes.Agencia.
    
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


&Scoped-define SELF-NAME BUTTON-93
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-93 C-Win
ON CHOOSE OF BUTTON-93 IN FRAME DEFAULT-FRAME /* Buscar */
DO:
    ASSIGN FRAME {&FRAME-NAME} T_ClientesAgencia
                               REstado
                               TNit
                               TNombre
                               TApellido1
                               TApellido2
                               CNit
                               CNombre
                               CApellido1
                               CApellido2.

    RUN Buscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CApellido1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CApellido1 C-Win
ON LEAVE OF CApellido1 IN FRAME DEFAULT-FRAME
DO:
  APPLY "Choose" TO BUTTON-93.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CApellido2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CApellido2 C-Win
ON LEAVE OF CApellido2 IN FRAME DEFAULT-FRAME
DO:
  APPLY "Choose" TO BUTTON-93.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CNit C-Win
ON LEAVE OF CNit IN FRAME DEFAULT-FRAME
DO:
  APPLY "Choose" TO BUTTON-93.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CNombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CNombre C-Win
ON LEAVE OF CNombre IN FRAME DEFAULT-FRAME
DO:
  APPLY "Choose" TO BUTTON-93.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME REstado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL REstado C-Win
ON VALUE-CHANGED OF REstado IN FRAME DEFAULT-FRAME
DO:
  APPLY "entry" TO CNit.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TApellido1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TApellido1 C-Win
ON MOUSE-SELECT-CLICK OF TApellido1 IN FRAME DEFAULT-FRAME /* Apellido1 */
DO:
  APPLY "value-Changed" TO SELF.
  APPLY "Entry" TO CApellido1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TApellido1 C-Win
ON VALUE-CHANGED OF TApellido1 IN FRAME DEFAULT-FRAME /* Apellido1 */
DO:
    CApellido1:SCREEN-VALUE = "".
    IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
        ASSIGN CApellido1:BGCOLOR = 15.
        ENABLE CApellido1 WITH FRAME {&FRAME-NAME}.
    END.
    ELSE DO:
        CApellido1:BGCOLOR = 17.
        DISABLE CApellido1 WITH FRAME {&FRAME-NAME}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TApellido2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TApellido2 C-Win
ON MOUSE-SELECT-CLICK OF TApellido2 IN FRAME DEFAULT-FRAME /* Apellido2 */
DO:
  APPLY "value-Changed" TO SELF.
  APPLY "Entry" TO CApellido2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TApellido2 C-Win
ON VALUE-CHANGED OF TApellido2 IN FRAME DEFAULT-FRAME /* Apellido2 */
DO:
    CApellido2:SCREEN-VALUE = "".
    IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
        CApellido2:BGCOLOR = 15.
        ENABLE CApellido2 WITH FRAME {&FRAME-NAME}.
    END.
    ELSE DO:
        CApellido2:BGCOLOR = 17.
        DISABLE CApellido2 WITH FRAME {&FRAME-NAME}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TNit C-Win
ON MOUSE-SELECT-CLICK OF TNit IN FRAME DEFAULT-FRAME /* Nit */
DO:
  APPLY "value-Changed" TO SELF.
  APPLY "Entry" TO CNit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TNit C-Win
ON VALUE-CHANGED OF TNit IN FRAME DEFAULT-FRAME /* Nit */
DO:
  CNit:SCREEN-VALUE = "".
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
      ASSIGN CNit:BGCOLOR = 15.
      ENABLE CNit WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO:
      ASSIGN CNit:BGCOLOR = 17.
      DISABLE CNit WITH FRAME {&FRAME-NAME}.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TNombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TNombre C-Win
ON MOUSE-SELECT-CLICK OF TNombre IN FRAME DEFAULT-FRAME /* Nombre */
DO:
  APPLY "value-Changed" TO SELF.
  APPLY "Entry" TO CNombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TNombre C-Win
ON VALUE-CHANGED OF TNombre IN FRAME DEFAULT-FRAME /* Nombre */
DO:
  CNombre:SCREEN-VALUE = "".
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
      ASSIGN CNombre:BGCOLOR = 15.
      ENABLE CNombre WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO:
      ASSIGN CNombre:BGCOLOR = 17.
      DISABLE CNombre WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CodAnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CodAnt C-Win
ON LEAVE OF W_CodAnt IN FRAME DEFAULT-FRAME
DO:
  ASSIGN W_CodAnt
         REstado.
  
  OPEN  QUERY Br_Clientes FOR EACH Clientes  
                                WHERE Clientes.Cod_Anterior EQ W_CodAnt  
                                  AND Clientes.Estado       EQ REstado
                                       NO-LOCK INDEXED-REPOSITION.
  
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

  APPLY "Mouse-select-Click" TO TNit.
 
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar C-Win 
PROCEDURE Buscar :
IF T_ClientesAgencia THEN DO:
    /*todos marcados*/
    IF TNit AND TNombre AND TApellido1 AND TApellido2 THEN
        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                   AND Clientes.Nit EQ CNit
                                                   AND INDEX(Clientes.Nombre,CNombre) > 0
                                                   AND INDEX(clientes.Apellido1,CApellido1) > 0
                                                   AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                   AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
    /*ninguno marcado*/
    ELSE
        IF NOT TNit AND NOT TNombre AND NOT TApellido1 AND NOT TApellido2 THEN
            MESSAGE "Debe elegir algún campo de busqueda".
        /*nit*/
        ELSE
            IF TNit AND NOT TNombre AND NOT TApellido1 AND NOT TApellido2 THEN
                OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                           AND Clientes.Nit EQ CNit
                                                           AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
            ELSE
                IF TNit AND TNombre AND NOT TApellido1 AND NOT TApellido2 THEN
                    OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                               AND Clientes.Nit EQ CNit
                                                               AND INDEX(Clientes.Nombre,CNombre) > 0
                                                               AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                ELSE
                    IF TNit AND TNombre AND TApellido1 AND NOT TApellido2 THEN
                        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                   AND Clientes.Nit EQ CNit
                                                                   AND INDEX(Clientes.Nombre,CNombre) > 0
                                                                   AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                   AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                    ELSE
                        IF NOT TNit AND TNombre AND NOT TApellido1 AND NOT TApellido2 THEN
                            OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                       AND INDEX(Clientes.Nombre,CNombre) > 0
                                                                       AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                        ELSE
                            IF NOT TNit AND NOT TNombre AND TApellido1 AND NOT TApellido2 THEN
                                OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                           AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                           AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                            ELSE
                                IF TNit AND NOT TNombre AND TApellido1 AND NOT TApellido2 THEN
                                    OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                               AND Clientes.Nit EQ CNit
                                                                               AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                               AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                ELSE
                                    IF NOT TNit AND NOT TNombre AND NOT TApellido1 AND TApellido2 THEN
                                        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                                   AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                   AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                    ELSE
                                        IF TNit AND NOT TNombre AND NOT TApellido1 AND TApellido2 THEN
                                            OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                                       AND Clientes.Nit EQ CNit
                                                                                       AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                       AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                        ELSE
                                            IF TNit AND TNombre AND NOT TApellido1 AND TApellido2 THEN
                                                OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                                           AND Clientes.Nit EQ CNit
                                                                                           AND INDEX(Clientes.Nombre,CNombre) > 0
                                                                                           AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                           AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                            ELSE
                                                IF NOT TNit AND TNombre AND TApellido1 AND NOT TApellido2 THEN
                                                    OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                                               AND INDEX(Clientes.Nombre,CNombre) > 0
                                                                                               AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                                               AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                                ELSE
                                                    IF NOT TNit AND NOT TNombre AND TApellido1 AND TApellido2 THEN
                                                        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                                                   AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                                                   AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                                   AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                                    ELSE
                                                        IF NOT TNit AND TNombre AND NOT TApellido1 AND TApellido2 THEN
                                                            OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                                                       AND INDEX(Clientes.Nombre,CNombre) > 0
                                                                                                       AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                                       AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                                        ELSE
                                                            IF NOT TNit AND TNombre AND TApellido1 AND TApellido2 THEN
                                                                OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Agencia EQ P_Agencia
                                                                                                           AND INDEX(Clientes.Nombre,CNombre) > 0
                                                                                                           AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                                                           AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                                           AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
END.
ELSE DO:
    /*todos marcados*/
    IF TNit AND TNombre AND TApellido1 AND TApellido2 THEN
        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Nit EQ CNit
                                                   AND INDEX(Clientes.Nombre,CNombre) > 0
                                                   AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                   AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                   AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
    /*ninguno marcado*/
    ELSE
        IF NOT TNit AND NOT TNombre AND NOT TApellido1 AND NOT TApellido2 THEN
            MESSAGE "Debe elegir algún campo de busqueda".
        /*nit*/
        ELSE
            IF TNit AND NOT TNombre AND NOT TApellido1 AND NOT TApellido2 THEN
                OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Nit EQ CNit
                                                           AND Clientes.Estado  EQ REstado NO-LOCK INDEXED-REPOSITION.
            ELSE
                IF TNit AND TNombre AND NOT TApellido1 AND NOT TApellido2 THEN
                    OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Nit EQ CNit
                                                               AND INDEX(Clientes.Nombre,CNombre) > 0
                                                               AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                ELSE
                    IF TNit AND TNombre AND TApellido1 AND NOT TApellido2 THEN
                        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Nit EQ CNit
                                                                   AND INDEX(Clientes.Nombre,CNombre) > 0
                                                                   AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                   AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                    ELSE
                        IF NOT TNit AND TNombre AND NOT TApellido1 AND NOT TApellido2 THEN
                            OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Nombre CONTAINS CNombre
                                                                       AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                        ELSE
                            IF NOT TNit AND NOT TNombre AND TApellido1 AND NOT TApellido2 THEN
                                OPEN QUERY Br_Clientes FOR EACH Clientes WHERE INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                           AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                            ELSE
                                IF TNit AND NOT TNombre AND TApellido1 AND NOT TApellido2 THEN
                                    OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Nit EQ CNit
                                                                               AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                               AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                ELSE
                                    IF NOT TNit AND NOT TNombre AND NOT TApellido1 AND TApellido2 THEN
                                        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                   AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                    ELSE
                                        IF TNit AND NOT TNombre AND NOT TApellido1 AND TApellido2 THEN
                                            OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Nit EQ CNit
                                                                                       AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                       AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                        ELSE
                                            IF TNit AND TNombre AND NOT TApellido1 AND TApellido2 THEN
                                                OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Nit EQ CNit
                                                                                           AND INDEX(Clientes.Nombre,CNombre) > 0
                                                                                           AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                           AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                            ELSE
                                                IF NOT TNit AND TNombre AND TApellido1 AND NOT TApellido2 THEN
                                                    OPEN QUERY Br_Clientes FOR EACH Clientes WHERE INDEX(Clientes.Nombre,CNombre) > 0
                                                                                               AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                                               AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                                ELSE
                                                    IF NOT TNit AND NOT TNombre AND TApellido1 AND TApellido2 THEN
                                                        OPEN QUERY Br_Clientes FOR EACH Clientes WHERE INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                                                   AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                                   AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                                    ELSE
                                                        IF NOT TNit AND TNombre AND NOT TApellido1 AND TApellido2 THEN
                                                            OPEN QUERY Br_Clientes FOR EACH Clientes WHERE INDEX(Clientes.Nombre,CNombre) > 0
                                                                                                       AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                                       AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
                                                        ELSE
                                                            IF NOT TNit AND TNombre AND TApellido1 AND TApellido2 THEN
                                                                OPEN QUERY Br_Clientes FOR EACH Clientes WHERE INDEX(Clientes.Nombre,CNombre) > 0
                                                                                                           AND INDEX(Clientes.Apellido1,CApellido1) > 0
                                                                                                           AND INDEX(Clientes.Apellido2,CApellido2) > 0
                                                                                                           AND Clientes.Estado EQ REstado NO-LOCK INDEXED-REPOSITION.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY CNit TNit TNombre CNombre TApellido1 CApellido1 TApellido2 CApellido2 
          W_CodAnt T_ClientesAgencia REstado 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE TNit TNombre TApellido1 TApellido2 BUTTON-93 W_CodAnt Br_Clientes 
         T_ClientesAgencia REstado Btn_Salir BUTTON-91 Btn_Crear RECT-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

