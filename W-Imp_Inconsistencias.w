&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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

{Incluido\variable.i "shared"}
DEFINE VARIABLE w_ok   AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE Tinc_tarjetaDb LIKE Inc_TarjetaDb
    FIELD Tnit     LIKE Clientes.nit 
    FIELD Tnombre  AS CHARACTER FORMAT "X(80)"
    INDEX IdxFec_tran    Fec_Transac
    INDEX IdxTnit Tnit
    INDEX IdxTnom Tnombre.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Frm-Main
&Scoped-define BROWSE-NAME Brw-Consulta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TInc_TarjetaDB

/* Definitions for BROWSE Brw-Consulta                                  */
&Scoped-define FIELDS-IN-QUERY-Brw-Consulta Tinc_TarjetaDB.Fec_Transac Tinc_TarjetaDB.TarjetaDB Tinc_TarjetaDB.ERROR Tinc_TarjetaDB.detalle Tinc_TarjetaDB.TNit Tinc_TarjetaDB.Tnombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw-Consulta   
&Scoped-define SELF-NAME Brw-Consulta
&Scoped-define QUERY-STRING-Brw-Consulta FOR EACH TInc_TarjetaDB
&Scoped-define OPEN-QUERY-Brw-Consulta OPEN QUERY {&SELF-NAME} FOR EACH TInc_TarjetaDB.
&Scoped-define TABLES-IN-QUERY-Brw-Consulta TInc_TarjetaDB
&Scoped-define FIRST-TABLE-IN-QUERY-Brw-Consulta TInc_TarjetaDB


/* Definitions for FRAME Frm-Main                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frm-Main ~
    ~{&OPEN-QUERY-Brw-Consulta}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-339 RECT-344 Btn-Visualizar W-FecIni ~
R-orden Btn-Procesar W-FecFin Btn_Done Brw-Consulta 
&Scoped-Define DISPLAYED-OBJECTS W-FecIni R-orden W-FecFin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn-Visualizar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Procesar 
     LABEL "Procesar" 
     SIZE 8 BY 1.5.

DEFINE BUTTON Btn-Visualizar 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1.5.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.5 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE VARIABLE W-FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W-FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE R-orden AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Fecha", 1,
"TarjetaDb", 2,
"Cédula", 3,
"Nombres", 4
     SIZE 46.43 BY .81
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE RECTANGLE RECT-339
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 2.65
     FGCOLOR 8 .

DEFINE RECTANGLE RECT-344
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48.14 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw-Consulta FOR 
      TInc_TarjetaDB SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw-Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw-Consulta C-Win _FREEFORM
  QUERY Brw-Consulta DISPLAY
      Tinc_TarjetaDB.Fec_Transac   COLUMN-LABEL "Fec_Transac" 
     Tinc_TarjetaDB.TarjetaDB     COLUMN-LABEL "Tarjeta"        FORMAT "X(20)"
     Tinc_TarjetaDB.ERROR         COLUMN-LABEL "Error"          FORMAT "X(80)"
     Tinc_TarjetaDB.detalle       COLUMN-LABEL "Detalle"        FORMAT "X(120)"
     Tinc_TarjetaDB.TNit          COLUMN-LABEL "Cédula"         FORMAT "X(14)"
     Tinc_TarjetaDB.Tnombre       COLUMN-LABEL "Cliente"        FORMAT "X(30)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 104 BY 9.69
         BGCOLOR 15 FGCOLOR 0  ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm-Main
     Btn-Visualizar AT ROW 1.19 COL 98.29 WIDGET-ID 166
     W-FecIni AT ROW 2.27 COL 10 COLON-ALIGNED WIDGET-ID 64
     R-orden AT ROW 2.31 COL 29.57 NO-LABEL WIDGET-ID 176
     Btn-Procesar AT ROW 2.69 COL 98.43 WIDGET-ID 86
     W-FecFin AT ROW 3.27 COL 10 COLON-ALIGNED WIDGET-ID 66
     Btn_Done AT ROW 4.23 COL 98.43 WIDGET-ID 20
     Brw-Consulta AT ROW 6.12 COL 2 WIDGET-ID 200
     " Fechas:" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 1.27 COL 4.29 WIDGET-ID 68
          FGCOLOR 0 
     "Ordenado por:" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 1.69 COL 29.86 WIDGET-ID 174
     "[dd/mm/aaaa]" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 1.27 COL 13.29 WIDGET-ID 70
          FGCOLOR 12 
     RECT-339 AT ROW 1.77 COL 3 WIDGET-ID 72
     RECT-344 AT ROW 1.96 COL 28.86 WIDGET-ID 172
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.14 BY 14.85 WIDGET-ID 100.


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
         TITLE              = "Histórico de Inconsistencias - W-Imp_Inconsistencias.w"
         HEIGHT             = 14.92
         WIDTH              = 106.72
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
/* SETTINGS FOR FRAME Frm-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB Brw-Consulta Btn_Done Frm-Main */
ASSIGN 
       Brw-Consulta:SEPARATOR-FGCOLOR IN FRAME Frm-Main      = 8.

/* SETTINGS FOR BUTTON Btn-Visualizar IN FRAME Frm-Main
   6                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw-Consulta
/* Query rebuild information for BROWSE Brw-Consulta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TInc_TarjetaDB.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Brw-Consulta */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Histórico de Inconsistencias - W-Imp_Inconsistencias.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Histórico de Inconsistencias - W-Imp_Inconsistencias.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Procesar C-Win
ON CHOOSE OF Btn-Procesar IN FRAME Frm-Main /* Procesar */
DO:
    ASSIGN W-FecIni
           W-FecFin.

    EMPTY TEMP-TABLE Tinc_TarjetaDb. 

    FOR EACH Inc_TarjetaDB WHERE Inc_TarjetaDb.Fec_Transac GE W-FecIni
                             AND Inc_TarjetaDb.Fec_Transac LE W-FecFin NO-LOCK:
        CREATE Tinc_TarjetaDb.
        BUFFER-COPY Inc_TarjetaDb TO Tinc_TarjetaDb.

        FIND FIRST tarjetas WHERE Tarjetas.tarjetaDb EQ Inc_TarjetaDb.tarjetaDb NO-LOCK NO-ERROR.
        IF AVAILABLE(tarjetas) THEN DO:
            FIND FIRST clientes WHERE Clientes.nit EQ Tarjetas.Nit NO-LOCK NO-ERROR.
            IF AVAILABLE(clientes) THEN
                ASSIGN Tinc_TarjetaDb.Tnit = Clientes.nit
                       Tinc_TarjetaDb.TNombre = TRIM(clientes.Apellido1) + " " + TRIM(clientes.apellido2) + " " + TRIM(clientes.nombre).
        END.
    END.

    APPLY "VALUE-CHANGED" TO R-Orden.
    APPLY "CHOOSE" TO Btn-Visualizar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Visualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Visualizar C-Win
ON CHOOSE OF Btn-Visualizar IN FRAME Frm-Main /* Button 2 */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
    DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
    listado = W_PathSpl + "L_Usuar.Lst".
    {Incluido\Imprimir.i "Listado" Tamano}
    APPLY "VALUE-CHANGED" TO R-Orden.
    APPLY "CHOOSE"        TO Btn-Visualizar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME Frm-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-orden
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-orden C-Win
ON VALUE-CHANGED OF R-orden IN FRAME Frm-Main
DO:
  ASSIGN R-Orden.
  CASE R-Orden:
      WHEN 1 THEN OPEN QUERY Brw-Consulta FOR EACH TInc_TarjetaDb NO-LOCK BY TInc_TarjetaDb.Fec_Transac INDEXED-REPOSITION.
      WHEN 2 THEN OPEN QUERY Brw-Consulta FOR EACH TInc_TarjetaDb NO-LOCK BY TInc_TarjetaDb.TarjetaDb   INDEXED-REPOSITION.
      WHEN 3 THEN OPEN QUERY Brw-Consulta FOR EACH TInc_TarjetaDb NO-LOCK BY TInc_TarjetaDb.Tnit        INDEXED-REPOSITION.
      WHEN 4 THEN OPEN QUERY Brw-Consulta FOR EACH TInc_TarjetaDb NO-LOCK BY TInc_TarjetaDb.Tnombre     INDEXED-REPOSITION.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Brw-Consulta
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
  RUN inicializar_variables.
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
  DISPLAY W-FecIni R-orden W-FecFin 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  ENABLE RECT-339 RECT-344 Btn-Visualizar W-FecIni R-orden Btn-Procesar 
         W-FecFin Btn_Done Brw-Consulta 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpLinea C-Win 
PROCEDURE ImpLinea :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
          DISPLAY Tinc_TarjetaDB.Fec_Transac   AT 1 
                  Tinc_TarjetaDB.TarjetaDB     AT 12  FORMAT "X(16)"
                  Tinc_TarjetaDb.ERROR         AT 29  FORMAT "X(80)"
                  Tinc_TarjetaDB.detalle       AT 110 FORMAT "X(120)"
                  Tinc_TarjetaDb.TNit          AT 231 FORMAT "X(35)"
                  Tinc_TarjetaDB.Tnombre       AT 267 FORMAT "X(30)"
                   WITH FRAME F-mov1 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 320 NO-LABELS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
FIND FIRST Cfg_Tarjeta NO-LOCK NO-ERROR.
DISABLE Btn-Visualizar.
ASSIGN W-FecIni:SCREEN-VALUE IN FRAME Frm-main = STRING(W_Fecha - DAY(w_fecha) + 1)
       W-FecFin:SCREEN-VALUE IN FRAME Frm-main = STRING(W_Fecha)
       W-FecIni  W-FecFin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}.  

FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.
DO WITH FRAME Frm-main:
    ASSIGN W-FecIni
           W-FecFin
           R-Orden.
END.

W_Reporte = "REPORTE   : Inconsistencias Tarjeta Débito  - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
W_EncColumna = "Fec_Trans  TarjetaDB        Error                                                                                                               Detalle".

VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.

CASE R-Orden:
    WHEN 1 THEN
        FOR EACH TInc_TarjetaDb NO-LOCK BY TInc_TarjetaDb.Fec_Transac:
            RUN impLinea.
        END.
    
    WHEN 2 THEN
        FOR EACH TInc_TarjetaDb NO-LOCK BY TInc_TarjetaDb.TarjetaDb:
            RUN impLinea.
        END.
    
    WHEN 3 THEN
        FOR EACH TInc_TarjetaDb NO-LOCK BY TInc_TarjetaDb.Tnit:
            RUN impLinea.
        END.

    WHEN 4 THEN
        FOR EACH TInc_TarjetaDb NO-LOCK BY TInc_TarjetaDb.TNombre:
            RUN impLinea.
        END.
END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

