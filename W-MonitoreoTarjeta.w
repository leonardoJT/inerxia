&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido\variable.i "shared"}

DEFINE VAR p_Nombre    LIKE Clientes.Nombre     NO-UNDO.
  DEFINE VAR P_Apellido  LIKE Clientes.Apellido1  NO-UNDO.
  DEFINE VAR P_AgeCli    LIKE Clientes.Agencia    NO-UNDO.
  DEFINE VAR w_ok        AS LOGICAL               NO-UNDO.
  DEFINE VAR w-tarjetaDB LIKE Ahorros.TarjetaDb   NO-UNDO.
  DEFINE VAR W-Seleccion AS INTEGER INITIAL 1     NO-UNDO.
  DEFINE VAR Choice      AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE TEMP-TABLE tmp-tarjetadb LIKE tarjetadebito.

DEFINE TEMP-TABLE TT_Movimientos LIKE mov_tarjetas.

/*****************************/
DEFINE TEMP-TABLE TT_MovAhorros
    FIELD nit AS CHARACTER
    FIELD cod_ahorro AS INTEGER
    FIELD cue_ahorros AS CHARACTER
    FIELD num_Tarjeta AS CHARACTER
    FIELD fecha_Transaccion AS DATE
    FIELD hora_transaccion AS INTEGER
    FIELD descripcion AS CHARACTER
    FIELD num_Transaccion AS CHARACTER
    FIELD val_efectivo AS DECIMAL
    FIELD GMF AS DECIMAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BrwMovimientos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_MovAhorros

/* Definitions for BROWSE BrwMovimientos                                */
&Scoped-define FIELDS-IN-QUERY-BrwMovimientos TT_MovAhorros.nit TT_MovAhorros.cod_ahorro TT_MovAhorros.cue_ahorros TT_MovAhorros.num_tarjeta TT_MovAhorros.fecha_transaccion STRING(TT_MovAhorros.Hora_Transaccion,"HH:MM:SS") TT_MovAhorros.num_transaccion TT_MovAhorros.descripcion TT_MovAhorros.val_efectivo TT_MovAhorros.GMF   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwMovimientos   
&Scoped-define SELF-NAME BrwMovimientos
&Scoped-define QUERY-STRING-BrwMovimientos FOR EACH TT_MovAhorros BY TT_MovAhorros.hora_transaccion
&Scoped-define OPEN-QUERY-BrwMovimientos OPEN QUERY {&SELF-NAME} FOR EACH TT_MovAhorros BY TT_MovAhorros.hora_transaccion.
&Scoped-define TABLES-IN-QUERY-BrwMovimientos TT_MovAhorros
&Scoped-define FIRST-TABLE-IN-QUERY-BrwMovimientos TT_MovAhorros


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BrwMovimientos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS txtFechaMov Btn_Imprimir Btn-Procesar ~
RECT-341 BrwMovimientos Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS txtFechaMov 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Imprimir 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Procesar 
     IMAGE-UP FILE "imagenes/proceso_g.bmp":U
     LABEL "Procesar" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/exit01.ico":U
     LABEL "&Salir" 
     SIZE 8 BY 1.62 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE txtFechaMov AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Movimientos" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.08
     BGCOLOR 15 FONT 3 NO-UNDO.

DEFINE RECTANGLE RECT-341
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.29 BY 5.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwMovimientos FOR 
      TT_MovAhorros SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwMovimientos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwMovimientos C-Win _FREEFORM
  QUERY BrwMovimientos DISPLAY
      TT_MovAhorros.nit COLUMN-LABEL "Cédula" FORMAT "X(11)"
     TT_MovAhorros.cod_ahorro COLUMN-LABEL "CodAh" FORMAT "99"
     TT_MovAhorros.cue_ahorros COLUMN-LABEL "CueAh" FORMAT "X(11)"
     TT_MovAhorros.num_tarjeta COLUMN-LABEL "Tarjeta" FORMAT "X(17)"
     TT_MovAhorros.fecha_transaccion COLUMN-LABEL "Fecha" FORMAT "99/99/9999"
     STRING(TT_MovAhorros.Hora_Transaccion,"HH:MM:SS") COLUMN-LABEL "Hora Tr." FORMAT "X(8)"
     TT_MovAhorros.num_transaccion COLUMN-LABEL "Transacción" FORMAT "X(13)"
     TT_MovAhorros.descripcion COLUMN-LABEL "Descripción" FORMAT "X(25)"
     TT_MovAhorros.val_efectivo COLUMN-LABEL "Monto" FORMAT "$>>>,>>>,>>9"
     TT_MovAhorros.GMF COLUMN-LABEL "GMF" FORMAT "$>>>,>>9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 128 BY 18.58
         FGCOLOR 0 FONT 3 ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     txtFechaMov AT ROW 1.54 COL 20 COLON-ALIGNED WIDGET-ID 190
     Btn_Imprimir AT ROW 4.73 COL 131.57 WIDGET-ID 166
     Btn-Procesar AT ROW 3.04 COL 131.72 WIDGET-ID 86
     BrwMovimientos AT ROW 2.88 COL 2 WIDGET-ID 300
     Btn_Done AT ROW 6.46 COL 131.72 WIDGET-ID 20
     RECT-341 AT ROW 2.88 COL 131 WIDGET-ID 170
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 140 BY 20.65
         BGCOLOR 16 FGCOLOR 0  WIDGET-ID 100.


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
         TITLE              = "Monitoreo de Movimientos Tarjeta Débito - W-MonitoreoTarjeta.w"
         HEIGHT             = 20.65
         WIDTH              = 140
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.38
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BrwMovimientos RECT-341 F-Main */
/* SETTINGS FOR BUTTON Btn_Imprimir IN FRAME F-Main
   6                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwMovimientos
/* Query rebuild information for BROWSE BrwMovimientos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_MovAhorros BY TT_MovAhorros.hora_transaccion.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BrwMovimientos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Monitoreo de Movimientos Tarjeta Débito - W-MonitoreoTarjeta.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Monitoreo de Movimientos Tarjeta Débito - W-MonitoreoTarjeta.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Procesar C-Win
ON CHOOSE OF Btn-Procesar IN FRAME F-Main /* Procesar */
DO:
    EMPTY TEMP-TABLE TT_MovAhorros.

    FOR EACH mov_ahorros WHERE mov_ahorros.fecha = TODAY
                           AND mov_ahorros.cpte = 24 NO-LOCK BREAK BY num_doc:
        IF FIRST-OF(num_doc) THEN DO:
            CREATE TT_MovAhorros.
            ASSIGN TT_MovAhorros.nit = mov_ahorros.nit
                   TT_MovAhorros.cod_ahorro = mov_ahorros.cod_ahorro
                   TT_MovAhorros.cue_ahorros = mov_ahorros.cue_ahorros
                   TT_MovAhorros.num_transaccion = mov_ahorros.num_documento
                   TT_MovAhorros.fecha_transaccion = mov_ahorros.fecha
                   TT_MovAhorros.hora_transaccion = mov_ahorros.hora.
            
            FIND FIRST ahorros WHERE ahorros.nit = mov_ahorros.nit
                                 AND ahorros.cod_ahorro = mov_ahorro.cod_ahorro
                                 AND ahorros.cue_ahorros = ahorros.cue_ahorros NO-LOCK NO-ERROR.
            IF AVAILABLE ahorros THEN
                TT_MovAhorros.num_Tarjeta = ahorros.tarjetaDB.
        END.

        IF INDEX(mov_ahorros.descrip,"GMF") > 0 THEN DO:
            TT_MovAhorros.GMF = mov_ahorros.val_efectivo.
        END.
        ELSE DO:
            IF INDEX(mov_ahorros.descrip,"Comis") = 0 THEN DO:
                TT_MovAhorros.val_efectivo = mov_ahorros.val_efectivo.
                TT_MovAhorros.descripcion = mov_ahorros.descrip.
            END.
        END.
    END.

    OPEN QUERY brwMovimientos FOR EACH TT_MovAhorros NO-LOCK BY TT_MovAhorros.hora_transaccion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME F-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir C-Win
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Button 2 */
/* Impresion de formatos TAC y CDAT*/
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
     DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
     listado = W_PathSpl + "L_Usuar.Lst".
     {Incluido\Imprimir.i "Listado" Tamano}
/*                                             */
/* DEFINE VAR Listado AS CHARACTER INITIAL "". */
/* Listado = W_Pathspl + "Lst_Reclamo.lst".    */
/* {incluido/imprimir.i "Listado"}.            */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwMovimientos
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

txtFechaMov:SCREEN-VALUE = STRING(w_fecha).

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
  DISPLAY txtFechaMov 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE txtFechaMov Btn_Imprimir Btn-Procesar RECT-341 BrwMovimientos Btn_Done 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}.  

W_Reporte    = "REPORTE    : MONITOREO DE TRANSACCIONES CON TARJETA DÉBITO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
W_EncColumna = "Cédula     CodAh CueAh      Tarjeta          Fecha      Hora Tr. Transacción         Monto      GMF".

VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.

FOR EACH TT_MovAhorros:
    DISPLAY
        TT_MovAhorros.nit FORMAT "X(10)"
        TT_MovAhorros.cod_ahorro FORMAT "99"
        TT_MovAhorros.cue_ahorros FORMAT "X(10)"
        TT_MovAhorros.num_tarjeta FORMAT "X(16)"
        TT_MovAhorros.fecha_transaccion FORMAT "99/99/9999"
        STRING(TT_MovAhorros.Hora_Transaccion,"HH:MM:SS") FORMAT "X(8)"
        TT_MovAhorros.num_transaccion FORMAT "X(12)"
        TT_MovAhorros.val_efectivo FORMAT "$>>>,>>>,>>9"
        TT_MovAhorros.GMF FORMAT "$>>>,>>9"
    WITH FRAME F-Mov USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 150 NO-LABELS.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

