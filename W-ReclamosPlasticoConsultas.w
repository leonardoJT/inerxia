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
/* Local Variable Definitions ---                                       */
  DEFINE VAR P_Nit       LIKE Clientes.Nit        NO-UNDO.
  DEFINE VAR p_Nombre    LIKE Clientes.Nombre     NO-UNDO.
  DEFINE VAR P_Apellido  LIKE Clientes.Apellido1  NO-UNDO.
  DEFINE VAR P_AgeCli    LIKE Clientes.Agencia    NO-UNDO.
  DEFINE VAR w_ok        AS LOGICAL               NO-UNDO.
  DEFINE VAR w-tarjetaDB LIKE Ahorros.TarjetaDb   NO-UNDO.
  DEFINE VAR W-Seleccion AS INTEGER INITIAL 1     NO-UNDO.
  DEFINE VAR Choice      AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE TEMP-TABLE tmp-tarjetadb LIKE tarjetadebito.

DEFINE TEMP-TABLE TT_Movimientos LIKE mov_tarjetas
    FIELD descripcion AS CHARACTER
    FIELD tipoRec AS INTEGER
    FIELD EstadoRec AS INTEGER.

/* Variables Para Imprimir en Excel */
DEFINE VAR InputFile AS CHARACTER NO-UNDO.
DEFINE VAR SwExiste AS CHARACTER NO-UNDO.
DEFINE VAR chExcelApp AS COM-HANDLE NO-UNDO.
DEFINE VAR hWorkBooks AS COM-HANDLE NO-UNDO.
DEFINE VAR ValCol AS CHARACTER NO-UNDO.
DEFINE VAR Dato AS CHARACTER NO-UNDO.
DEFINE VAR PrinterName AS CHARACTER NO-UNDO.
DEFINE VAR chWorksheet AS COM-HANDLE NO-UNDO.

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
&Scoped-define INTERNAL-TABLES TT_Movimientos

/* Definitions for BROWSE BrwMovimientos                                */
&Scoped-define FIELDS-IN-QUERY-BrwMovimientos TT_Movimientos.num_Cuenta TT_Movimientos.lugar TT_Movimientos.fec_transac STRING(TT_Movimientos.Hora_Transac,"HH:MM:SS") TT_Movimientos.Monto TT_Movimientos.comision TT_Movimientos.tipoTransaccion TT_Movimientos.codError TT_Movimientos.tipoRed TT_Movimientos.NroTerminal TT_Movimientos.Naud TT_Movimientos.retCode TT_Movimientos.Aplicado TT_Movimientos.descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwMovimientos   
&Scoped-define SELF-NAME BrwMovimientos
&Scoped-define QUERY-STRING-BrwMovimientos FOR EACH TT_Movimientos NO-LOCK
&Scoped-define OPEN-QUERY-BrwMovimientos OPEN QUERY {&SELF-NAME} FOR EACH TT_Movimientos NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BrwMovimientos TT_Movimientos
&Scoped-define FIRST-TABLE-IN-QUERY-BrwMovimientos TT_Movimientos


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BrwMovimientos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Imprimir Btn_Salir Btn-Procesar w-cedula ~
txtFechaInicial txtFechaFinal Rec-Fondo RECT-340 RECT-341 BrwMovimientos 
&Scoped-Define DISPLAYED-OBJECTS w-FecRec w-HoraRec w-Consecutivo ~
W_NomTitular Cmb-Tipo Cmb-Estado w-Descripcion txtNumTransaccion ~
txtCodTerminal txtMonto txtFechaTransaccion txtHoraTransaccion txtTipoRed ~
w-cedula txtFechaInicial txtFechaFinal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Imprimir Btn_Consulta Btn_Salvar Btn_Cancelar ~
Btn_Modificar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Procesar 
     LABEL "Búsqueda" 
     SIZE 13 BY 2.15.

DEFINE BUTTON Btn-Visualizar 
     LABEL "Visualizar" 
     SIZE 8 BY .69 TOOLTIP "Visualizar por Pantalla / Impresora".

DEFINE BUTTON Btn_Borrar 
     IMAGE-UP FILE "imagenes/error.bmp":U
     LABEL "&Borrar" 
     SIZE 8 BY 1.65
     FONT 4.

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/warning.gif":U
     LABEL "&Cancelar" 
     SIZE 8 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_Modificar 
     IMAGE-UP FILE "imagenes/calendario.bmp":U
     LABEL "&Modificar" 
     SIZE 8 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Salir DEFAULT 
     IMAGE-UP FILE "imagenes/exit01.ico":U
     LABEL "&Salir" 
     SIZE 8 BY 1.62 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Salvar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "&Salvar" 
     SIZE 8 BY 1.62
     FONT 4.

DEFINE VARIABLE w-Descripcion AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 45.29 BY 3.23 NO-UNDO.

DEFINE VARIABLE txtCodTerminal AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE txtFechaFinal AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE txtFechaInicial AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE txtFechaTransaccion AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE txtHoraTransaccion AS CHARACTER FORMAT "X(256)" 
     VIEW-AS FILL-IN 
     SIZE 9.29 BY .81 NO-UNDO.

DEFINE VARIABLE txtMonto AS DECIMAL FORMAT "$>>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE txtNumTransaccion AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE txtTipoRed AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE w-cedula AS CHARACTER FORMAT "X(40)":U 
     LABEL "Cédula" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.08
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w-Consecutivo AS INTEGER FORMAT "99999999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w-FecRec AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE w-HoraRec AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .81
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1.08 TOOLTIP "Nombre del Titular de la Tarjeta Débito"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE Rec-Fondo
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 113 BY 7.54
     BGCOLOR 8 FGCOLOR 15 .

DEFINE RECTANGLE RECT-340
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.29 BY 4.04.

DEFINE RECTANGLE RECT-341
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.29 BY 12.92.

DEFINE VARIABLE Cmb-Estado AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE Cmb-Tipo AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 30 BY .81 NO-UNDO.

DEFINE BUTTON btnBuscarReclamo 
     LABEL "Buscar" 
     SIZE 13 BY 1.12.

DEFINE BUTTON btnCerrarConsultaConsecutivo-2 
     LABEL "Cerrar" 
     SIZE 13 BY 1.12.

DEFINE VARIABLE cmbAgenciasConsulta AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE cmbEstadosConsulta AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estados" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "01 - RADICADO","02 - PENDIENTE","03 - TRANSACCIÓN NO EXITOSA","04 - TRANSACCIÓN EXITOSA" 
     DROP-DOWN-LIST
     SIZE 47 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwMovimientos FOR 
      TT_Movimientos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwMovimientos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwMovimientos C-Win _FREEFORM
  QUERY BrwMovimientos DISPLAY
      TT_Movimientos.num_Cuenta COLUMN-LABEL "Cuenta" FORMAT "X(10)"
     TT_Movimientos.lugar COLUMN-LABEL "Lugar" FORMAT "X(30)"
     TT_Movimientos.fec_transac COLUMN-LABEL "Fecha Tr." FORMAT "99/99/9999"
     STRING(TT_Movimientos.Hora_Transac,"HH:MM:SS") COLUMN-LABEL "Hora Tr." FORMAT "X(8)"
     TT_Movimientos.Monto COLUMN-LABEL "Monto" FORMAT "$>>>,>>>,>>9"
     TT_Movimientos.comision COLUMN-LABEL "Comisión" FORMAT "$>>>,>>9"
     TT_Movimientos.tipoTransaccion COLUMN-LABEL "Tipo Tr." FORMAT "9"
     TT_Movimientos.codError COLUMN-LABEL "Error" FORMAT "99"
     TT_Movimientos.tipoRed COLUMN-LABEL "Red" FORMAT "9"
     TT_Movimientos.NroTerminal COLUMN-LABEL "Nro.Term" FORMAT "X(8)"
     TT_Movimientos.Naud COLUMN-LABEL "Nro.Term" FORMAT "X(8)"
     TT_Movimientos.retCode COLUMN-LABEL "Cod. Terminal" FORMAT ">>>>>>>>"
     TT_Movimientos.Aplicado COLUMN-LABEL "Aplicado" FORMAT "si/no"
     TT_Movimientos.descripcion COLUMN-LABEL "Observaciones" FORMAT "X(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 112 BY 7.54
         FGCOLOR 0  ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     w-FecRec AT ROW 14.23 COL 15.57 COLON-ALIGNED NO-LABEL WIDGET-ID 180
     w-HoraRec AT ROW 15 COL 15.57 COLON-ALIGNED NO-LABEL WIDGET-ID 182
     w-Consecutivo AT ROW 13.38 COL 30 RIGHT-ALIGNED NO-LABEL WIDGET-ID 148
     W_NomTitular AT ROW 4 COL 40 NO-LABEL WIDGET-ID 14
     Cmb-Tipo AT ROW 18.23 COL 84 NO-LABEL WIDGET-ID 96
     Cmb-Estado AT ROW 19.31 COL 84 NO-LABEL WIDGET-ID 98
     w-Descripcion AT ROW 14.46 COL 67 NO-LABEL WIDGET-ID 154
     txtNumTransaccion AT ROW 16.96 COL 15 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     txtCodTerminal AT ROW 19.5 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     txtMonto AT ROW 17.81 COL 15 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     txtFechaTransaccion AT ROW 16.12 COL 15 COLON-ALIGNED HELP
          "Ingrese la Fecha Contable en que se realizó la transacción" NO-LABEL WIDGET-ID 140
     txtHoraTransaccion AT ROW 16.12 COL 27.72 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     Btn-Visualizar AT ROW 9.88 COL 116.57 WIDGET-ID 8
     Btn_Imprimir AT ROW 10.65 COL 116.57 WIDGET-ID 166
     Btn_Consulta AT ROW 1.5 COL 116.57 WIDGET-ID 168
     Btn_Salvar AT ROW 4.81 COL 116.57 WIDGET-ID 18
     Btn_Cancelar AT ROW 6.46 COL 116.72 WIDGET-ID 16
     Btn_Salir AT ROW 12.38 COL 116.57 WIDGET-ID 20
     Btn_Borrar AT ROW 8.12 COL 116.57 WIDGET-ID 164
     Btn_Modificar AT ROW 3.15 COL 116.57 WIDGET-ID 186
     Btn-Procesar AT ROW 1.81 COL 32 WIDGET-ID 86
     txtTipoRed AT ROW 18.65 COL 15 COLON-ALIGNED NO-LABEL WIDGET-ID 194
     w-cedula AT ROW 4 COL 15 COLON-ALIGNED WIDGET-ID 198
     txtFechaInicial AT ROW 1.81 COL 15 COLON-ALIGNED WIDGET-ID 12
     txtFechaFinal AT ROW 2.88 COL 15 COLON-ALIGNED WIDGET-ID 200
     BrwMovimientos AT ROW 5.31 COL 3 WIDGET-ID 300
     "Red:" VIEW-AS TEXT
          SIZE 5.43 BY .81 AT ROW 18.69 COL 11.57 WIDGET-ID 196
          BGCOLOR 8 
     "Fecha reclamo:" VIEW-AS TEXT
          SIZE 14.57 BY .54 AT ROW 14.31 COL 3 WIDGET-ID 176
          BGCOLOR 8 
     " Comentarios" VIEW-AS TEXT
          SIZE 13.43 BY .62 AT ROW 13.65 COL 67 WIDGET-ID 158
          FGCOLOR 1 
     "Transacción:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 16.96 COL 4 WIDGET-ID 126
          BGCOLOR 8 
     "Fecha y Hora:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 16.08 COL 3 WIDGET-ID 130
          BGCOLOR 8 
     "Estado Reclamo" VIEW-AS TEXT
          SIZE 16 BY .54 AT ROW 19.46 COL 66 WIDGET-ID 104
     "Monto:" VIEW-AS TEXT
          SIZE 7.43 BY .81 AT ROW 17.85 COL 9.57 WIDGET-ID 118
          BGCOLOR 8 
     "Código Terminal:" VIEW-AS TEXT
          SIZE 16.72 BY .81 AT ROW 19.54 COL 3.29 WIDGET-ID 116
          BGCOLOR 8 
     "Tipo Reclamo" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 18.38 COL 66 WIDGET-ID 102
     "Consecutivo:" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 13.38 COL 8 WIDGET-ID 174
          BGCOLOR 1 FGCOLOR 15 
     "Hora:" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 15.12 COL 11.29 WIDGET-ID 178
          BGCOLOR 8 
     Rec-Fondo AT ROW 13.12 COL 2 WIDGET-ID 106
     RECT-340 AT ROW 13.92 COL 66 WIDGET-ID 156
     RECT-341 AT ROW 1.27 COL 116 WIDGET-ID 170
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125.14 BY 19.88
         FGCOLOR 0  WIDGET-ID 100.

DEFINE FRAME frmConsultaReclamos
     cmbAgenciasConsulta AT ROW 1.27 COL 11 COLON-ALIGNED WIDGET-ID 8
     cmbEstadosConsulta AT ROW 2.27 COL 11 COLON-ALIGNED WIDGET-ID 10
     txtFechaInicial AT ROW 3.42 COL 15.29 COLON-ALIGNED WIDGET-ID 12
          LABEL "Fecha inicial" FORMAT "99/99/9999":U
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     btnCerrarConsultaConsecutivo-2 AT ROW 3.96 COL 33 WIDGET-ID 6
     btnBuscarReclamo AT ROW 3.96 COL 47 WIDGET-ID 4
     txtFechaFinal AT ROW 4.5 COL 15.29 COLON-ALIGNED WIDGET-ID 14
          LABEL "Fecha final" FORMAT "99/99/9999":U
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 57 ROW 1.54
         SIZE 60 BY 5.92
         TITLE "CONSULTA DE RECLAMOS" WIDGET-ID 500.


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
         TITLE              = "Reclamos de Plástico - W-ReclamosPlastico.w"
         HEIGHT             = 19.88
         WIDTH              = 125.14
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
/* REPARENT FRAME */
ASSIGN FRAME frmConsultaReclamos:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BrwMovimientos RECT-341 F-Main */
/* SETTINGS FOR BUTTON Btn-Visualizar IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn-Visualizar:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON Btn_Borrar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON Btn_Imprimir IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Modificar IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR SELECTION-LIST Cmb-Estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST Cmb-Tipo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtCodTerminal IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       txtCodTerminal:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN txtFechaTransaccion IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       txtFechaTransaccion:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN txtHoraTransaccion IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       txtHoraTransaccion:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN txtMonto IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       txtMonto:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN txtNumTransaccion IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       txtNumTransaccion:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN txtTipoRed IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       txtTipoRed:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-Consecutivo IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       w-Consecutivo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR w-Descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-FecRec IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-FecRec:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-HoraRec IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-HoraRec:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME frmConsultaReclamos
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwMovimientos
/* Query rebuild information for BROWSE BrwMovimientos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Movimientos NO-LOCK.
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
ON END-ERROR OF C-Win /* Reclamos de Plástico - W-ReclamosPlastico.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Reclamos de Plástico - W-ReclamosPlastico.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwMovimientos
&Scoped-define SELF-NAME BrwMovimientos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BrwMovimientos C-Win
ON MOUSE-SELECT-CLICK OF BrwMovimientos IN FRAME F-Main
DO:
    APPLY "value-changed" TO brwMovimientos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BrwMovimientos C-Win
ON VALUE-CHANGED OF BrwMovimientos IN FRAME F-Main
DO:
    txtFechaTransaccion:SCREEN-VALUE = STRING(TT_Movimientos.fec_Trans).
    txtHoraTransaccion:SCREEN-VALUE = STRING(TT_Movimientos.Hora_Transac,"HH:MM:SS").
    txtNumTransaccion:SCREEN-VALUE = TT_Movimientos.Naud.
    txtMonto:SCREEN-VALUE = STRING(TT_Movimientos.Monto,">>>,>>>,>>9").

    CASE TT_Movimientos.tipoRed:
        WHEN 1 THEN txtTipoRed:SCREEN-VALUE = "Servibanca".
        WHEN 2 THEN txtTipoRed:SCREEN-VALUE = "Redeban".
        WHEN 3 THEN txtTipoRed:SCREEN-VALUE = "Credibanco".
        WHEN 4 THEN txtTipoRed:SCREEN-VALUE = "ATH Red Megabanco".
        WHEN 5 THEN txtTipoRed:SCREEN-VALUE = "Red Express".
        WHEN 6 THEN txtTipoRed:SCREEN-VALUE = "ATH Banco de Bogotá".
        WHEN 7 THEN txtTipoRed:SCREEN-VALUE = "ATH Aval".
        WHEN 8 THEN txtTipoRed:SCREEN-VALUE = "Otras Redes recibidas por ATH".
    END CASE.

    txtCodTerminal:SCREEN-VALUE = STRING(TT_Movimientos.RetCode). /* Código Terminal */
    w-descripcion:SCREEN-VALUE = TT_Movimientos.descripcion.

    CASE TT_Movimientos.EstadoRec:
        WHEN 1 THEN cmb-Estado:SCREEN-VALUE = "01 RADICADO".
        WHEN 2 THEN cmb-Estado:SCREEN-VALUE = "02 PENDIENTE".
        WHEN 3 THEN cmb-Estado:SCREEN-VALUE = "03 TRANSACCIÓN NO EXITOSA".
        WHEN 4 THEN cmb-Estado:SCREEN-VALUE = "04 TRANSACCIÓN EXITOSA".
    END CASE.

    CASE TT_Movimientos.TipoRec:
        WHEN 1 THEN cmb-Tipo:SCREEN-VALUE = "01 DÉBITO NO PAGO".
        WHEN 2 THEN cmb-Tipo:SCREEN-VALUE = "02 PAGO PARCIAL RETIRO".
        WHEN 3 THEN cmb-Tipo:SCREEN-VALUE = "03 RETIRO DOBLE O TRIPLE".
        WHEN 4 THEN cmb-Tipo:SCREEN-VALUE = "04 RETIRO NO REALIZADO".
    END CASE.


    ENABLE btn_Modificar WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Procesar C-Win
ON CHOOSE OF Btn-Procesar IN FRAME F-Main /* Búsqueda */
DO:
    EMPTY TEMP-TABLE TT_Movimientos.

    FOR EACH reclamos_tarj WHERE reclamos_tarj.nit = w-cedula:SCREEN-VALUE
                             AND reclamos_tarj.fec_recl >= DATE(txtFechaInicial:SCREEN-VALUE)
                             AND reclamos_tarj.fec_recl <= DATE(txtFechaFinal:SCREEN-VALUE) NO-LOCK:
        FIND FIRST mov_tarjetas WHERE mov_tarjetas.naud = reclamos_tarj.naud NO-LOCK NO-ERROR.
        IF AVAILABLE mov_tarjetas THEN DO:
            CREATE TT_Movimientos.
            BUFFER-COPY mov_tarjetas TO TT_Movimientos.
            TT_Movimientos.descripcion = reclamos_tarj.descripcion.
            TT_Movimientos.estadoRec = reclamos_tarj.estadoRec.
        END.
    END.

    ENABLE btn_Imprimir WITH FRAME F-Main.
        
    OPEN QUERY brwMovimientos FOR EACH TT_Movimientos NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Visualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Visualizar C-Win
ON CHOOSE OF Btn-Visualizar IN FRAME F-Main /* Visualizar */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  listado = W_PathSpl + "L_Usuar.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmConsultaReclamos
&Scoped-define SELF-NAME btnBuscarReclamo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBuscarReclamo C-Win
ON CHOOSE OF btnBuscarReclamo IN FRAME frmConsultaReclamos /* Buscar */
DO:
    EMPTY TEMP-TABLE TT_Movimientos.

    FOR EACH reclamos_tarj WHERE reclamos_tarj.agencia = INTEGER(SUBSTRING(cmbAgenciasConsulta:SCREEN-VALUE,1,2))
                             AND reclamos_tarj.estado = INTEGER(SUBSTRING(cmbestadosConsulta:SCREEN-VALUE,1,2))
                             AND reclamos_tarj.fec_reclamo >= DATE(txtFechaInicial:SCREEN-VALUE IN FRAME frmConsultaReclamos)
                             AND reclamos_tarj.fec_reclamo <= DATE(txtFechaInicial:SCREEN-VALUE IN FRAME frmConsultaReclamos) NO-LOCK:
        FIND FIRST mov_tarjetas WHERE mov_tarjetas.naud = reclamos_tarj.naud NO-LOCK NO-ERROR.
        IF AVAILABLE mov_tarjetas THEN DO:
            CREATE TT_Movimientos.
            BUFFER-COPY mov_tarjetas TO TT_Movimientos.
            TT_Movimientos.descripcion = reclamos_tarj.descripcion.
            TT_Movimientos.estadoRec = reclamos_tarj.estadoRec.
            TT_Movimientos.tipoRec = reclamos_tarj.tipoRec.
        END.
    END.
        
    OPEN QUERY brwMovimientos FOR EACH TT_Movimientos NO-LOCK.

    ENABLE Btn_Imprimir
           Btn_Consulta
           Btn_Salir
           Btn_Cancelar
           txtFechaInicial
           txtFechaFinal
           w-cedula
           brwMovimientos WITH FRAME F-Main.

    ENABLE btn_Imprimir WITH FRAME F-Main.

    HIDE FRAME frmConsultaReclamos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCerrarConsultaConsecutivo-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCerrarConsultaConsecutivo-2 C-Win
ON CHOOSE OF btnCerrarConsultaConsecutivo-2 IN FRAME frmConsultaReclamos /* Cerrar */
DO:
    ASSIGN FRAME F-main:SENSITIVE = TRUE.
    VIEW FRAME F-Main.
    HIDE FRAME frmConsultaReclamos.
    ENABLE Btn_Imprimir
           Btn_Consulta
           Btn_Salir
           Btn_Cancelar
           txtFechaInicial
           txtFechaFinal
           w-cedula
           brwMovimientos WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Borrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Borrar C-Win
ON CHOOSE OF Btn_Borrar IN FRAME F-Main /* Borrar */
DO:
    DEFINE VAR w-contAS AS INTEGER INITIAL 0.

    IF AVAILABLE(Reclamos_Tarj) THEN DO:
        MESSAGE "Esta seguro de Borrar el consecutivo " Reclamos_Tarj.Consecutivo " ?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.

        IF choice THEN DO:
            FIND CURRENT Reclamos_Tarj NO-ERROR.
            DELETE Reclamos_Tarj.
            RELEASE Reclamos_Tarj.

            RUN inicializar_Variables.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME F-Main /* Cancelar */
DO:
  RUN Inicializar_Variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta C-Win
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Button 3 */
DO:
    /*ASSIGN FRAME F-main:SENSITIVE = FALSE.*/
    ASSIGN FRAME frmConsultaReclamos:SENSITIVE = TRUE.

    IF cfg_tarjeta.usuario = w_usuario THEN DO:
        VIEW FRAME frmConsultaReclamos.

        w-cedula:SCREEN-VALUE = "".
        w-cedula.
    END.

    DISABLE Btn_Imprimir
            Btn_Consulta
            Btn_Salvar
            Btn_modificar
            txtFechaInicial
            txtFechaFinal
            w-cedula
            brwMovimientos WITH FRAME F-Main.

    ENABLE Btn_Cancelar Btn_Salir.    
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


&Scoped-define SELF-NAME Btn_Modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Modificar C-Win
ON CHOOSE OF Btn_Modificar IN FRAME F-Main /* Modificar */
DO:
    DO WITH FRAME F-main:
        ASSIGN W-Cedula
               Cmb-Tipo
               Cmb-Estado
               W-Descripcion.
               
        DISABLE Btn_Modificar
                Btn_Consulta
            WITH FRAME F-Main.

        ENABLE Btn_Salvar
               Btn_Borrar
               Btn_CAncelar
            WITH FRAME F-Main.

        FIND FIRST reclamos_tarj WHERE reclamos_tarj.naud = TT_Movimientos.naud NO-ERROR.
        IF AVAILABLE(Reclamos_Tarj) THEN
            ENABLE Cmb-Tipo
                   Cmb-Estado
                   W-Descripcion
            WITH FRAME F-Main.
        
        W-Seleccion = 2.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir C-Win
ON CHOOSE OF Btn_Salir IN FRAME F-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar C-Win
ON CHOOSE OF Btn_Salvar IN FRAME F-Main /* Salvar */
DO:
    DEFI VAR Mens1 AS CHARACTER FORMAT "X(80)" INITIAL "".
    DEFI VAR Mens2 AS CHARACTER FORMAT "X(80)" INITIAL "".
    DEFI VAR Mens3 AS CHARACTER FORMAT "X(80)" INITIAL "".
    DEFI VAR Mens4 AS CHARACTER FORMAT "X(80)" INITIAL "".

    DO WITH FRAME F-main:
        ASSIGN W-Cedula
               Cmb-Tipo
               Cmb-Estado
               W-Descripcion.

        Grabando:
        DO TRANSACTION ON ERROR UNDO Grabando:
            IF W-Descripcion EQ "" THEN
                Mens3 = "Describa el Motivo del Reclamo".

            IF Mens1 NE "" OR Mens2 NE "" OR Mens3 NE "" OR Mens4 NE "" THEN DO:
                MESSAGE "COMPLETAR LA INFORMACION QUE A CONTINUACION SE ENUNCIA "  SKIP(2)
                        Mens1 SKIP
                        Mens2 SKIP
                        Mens3 SKIP
                        Mens4
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN.
            END.
            ELSE DO:
                IF W-Seleccion = 1 THEN DO:
                    CREATE Reclamos_Tarj.
                    ASSIGN Reclamos_Tarj.agencia = w_agencia
                           Reclamos_Tarj.Consecutivo = NEXT-VALUE(Sec_ReclamoTdb)
                           Reclamos_Tarj.Fec_Reclamo = W_Fecha
                           Reclamos_Tarj.Hora_Reclamo = STRING(TIME,"HH:MM:SS")
                           Reclamos_Tarj.Fec_Transac = TT_Movimientos.Fec_Transac
                           Reclamos_Tarj.Hora_Transac = TT_Movimientos.Hora_Transac
                           Reclamos_Tarj.TarjetaDB = TT_Movimientos.TarjetaDb
                           Reclamos_Tarj.Num_cuenta = TT_Movimientos.Num_cuenta
                           Reclamos_Tarj.Nit = TT_Movimientos.Nit
                           Reclamos_Tarj.MontoTransac = TT_Movimientos.Monto
                           Reclamos_Tarj.Comision = TT_Movimientos.Comision
                           Reclamos_Tarj.Naud = TT_Movimientos.Naud
                           Reclamos_Tarj.NroTerminal = /*TT_Movimientos.NroTerminal*/ STRING(TT_Movimientos.retCode)
                           Reclamos_Tarj.Descripcion = W-Descripcion
                           Reclamos_Tarj.Usuario = W_Usuario
                           Reclamos_Tarj.Lugar = TT_Movimientos.Lugar
                           Reclamos_Tarj.TipoCuenta = TT_Movimientos.TipoCuenta
                           Reclamos_Tarj.TipoTransaccion = TT_Movimientos.TipoTransaccion
                           Reclamos_Tarj.TipoRed = TT_Movimientos.TipoRed
                           Reclamos_Tarj.TipoRec = INTEGER(SUBSTRING(Cmb-Tipo,1,2))
                           Reclamos_tarj.EstadoRec = INTEGER(SUBSTRING(cmb-estado,1,2)).

                           /*Reclamos_Tarj.Fec_Reverso       =  */
                           /*Reclamos_Tarj.Reportado         =  */
                           /*Reclamos_Tarj.Reversado         =  */

                    /*APPLY "CHOOSE" TO btn-visualizar.*/
                END.
                ELSE DO: /* Actualizar */
                    FIND CURRENT Reclamos_Tarj NO-ERROR.
                    ASSIGN Reclamos_Tarj.TipoRec = INTEGER(SUBSTRING(Cmb-Tipo:SCREEN-VALUE IN FRAME F-MAIN,1,2))
                           Reclamos_tarj.EstadoRec = INTEGER(SUBSTRING(Cmb-Estado:SCREEN-VALUE IN FRAME F-MAIN,1,2))
                           Reclamos_Tarj.Descripcion = W-Descripcion.
                    /*APPLY "CHOOSE" TO btn-visualizar.*/
                    RELEASE Reclamos_Tarj.
                END.
            END.

            ASSIGN W-Seleccion = 1.
        END.

        DISABLE btn_Imprimir WITH FRAME F-Main.

        RUN inicializar_variables.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w-cedula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cedula C-Win
ON LEAVE OF w-cedula IN FRAME F-Main /* Cédula */
DO:
    ASSIGN w-cedula.
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
  DISPLAY w-FecRec w-HoraRec w-Consecutivo W_NomTitular Cmb-Tipo Cmb-Estado 
          w-Descripcion txtNumTransaccion txtCodTerminal txtMonto 
          txtFechaTransaccion txtHoraTransaccion txtTipoRed w-cedula 
          txtFechaInicial txtFechaFinal 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE Btn_Imprimir Btn_Salir Btn-Procesar w-cedula txtFechaInicial 
         txtFechaFinal Rec-Fondo RECT-340 RECT-341 BrwMovimientos 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY cmbAgenciasConsulta cmbEstadosConsulta txtFechaInicial txtFechaFinal 
      WITH FRAME frmConsultaReclamos IN WINDOW C-Win.
  ENABLE cmbAgenciasConsulta cmbEstadosConsulta txtFechaInicial 
         btnCerrarConsultaConsecutivo-2 btnBuscarReclamo txtFechaFinal 
      WITH FRAME frmConsultaReclamos IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmConsultaReclamos}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TarDebNew C-Win 
PROCEDURE Grabar_TarDebNew :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
  DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wretcode      AS INTEGER INITIAL -1.

  FOR EACH tmp-tarjetadb:
     CREATE tarjetadebito.
     BUFFER-COPY tmp-tarjetadb TO tarjetadebito.
     ASSIGN wvlrmonTD = STRING(tarjetadebito.Monto,'9999999999999').
     RUN TranWebCaja(1,tarjetadebito.TipoTransaccion,TRIM(tarjetadebito.Nit), TRIM(tarjetadebito.Cue_Ahorros), tarjetadebito.TarjetaDB, wvlrmonTD, INPUT-OUTPUT wsecTD, INPUT-OUTPUT wsdoTD, INPUT-OUTPUT wdisTD, OUTPUT wretcode).
     IF wretcode = 0 THEN 
        ASSIGN tarjetadebito.Secuencia       = wsecTD 
               tarjetadebito.SdoTotal        = ROUND(decimal(wsdoTD) / 100,2)
               tarjetadebito.SdoDispon       = ROUND(decimal(wdisTD) / 100,2)
               tarjetadebito.RetCode         = wretcode
               tarjetadebito.Aplicado        = YES.
     ELSE 
        ASSIGN tarjetadebito.RetCode         = wretcode.
     RELEASE tarjetadebito.
  END.
  /* Barrido de la tabla tarjetadb */
  FOR EACH tmp-tarjetadb:
      DELETE tmp-tarjetadb.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TmpTarDeb C-Win 
PROCEDURE Grabar_TmpTarDeb :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER wvlrTrans  LIKE ahorros.sdo_disponible.
DEFINE INPUT PARAMETER wdesTrans  LIKE Mov_contable.comentario.
DEFINE INPUT PARAMETER wtipoTrans AS INTEGER.
DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wretcode      AS INTEGER INITIAL -1.

IF wtipotrans NE 0 THEN DO:
   FIND CURRENT ahorros  NO-ERROR.
   CREATE Tmp-tarjetadb.
   ASSIGN Tmp-tarjetadb.Agencia         =  w_agencia                                                                                                                 
          Tmp-tarjetadb.Usuario         =  w_usuario                                                                                                                 
          Tmp-tarjetadb.Comprobante     =  Comprobante.Comprobante                                                                                                   
          Tmp-tarjetadb.Num_Documento   =  Comprobantes.Secuencia                                                                                                    
          Tmp-tarjetadb.Fec_Contable    =  TODAY                                                                                                                     
          Tmp-tarjetadb.Hora            =  TIME                                                                                                                      
          Tmp-tarjetadb.Comentario      =  wdesTrans                                                                                                                 
          Tmp-tarjetadb.Aplicado        =  NO                                                                                                                        
          Tmp-tarjetadb.ManBiometrico   =  1                                                                                                                         
          Tmp-tarjetadb.TipoTransaccion =  wtipotrans                                                                                                                
          Tmp-tarjetadb.Nit             =  Ahorros.nit                                                                                                               
          Tmp-tarjetadb.Cue_Ahorros     =  Ahorros.cue_ahorros  /* SUBSTRING(Ahorros.TarjetaDB,10,7) */                                                              
          Tmp-tarjetadb.TarjetaDB       =  Ahorros.TarjetaDB                                                                                                         
          Tmp-tarjetadb.Monto           =  wvlrTrans                                                                                                                 
          Tmp-tarjetadb.Secuencia       =  "000000000000"                                                                                                            
          Tmp-tarjetadb.SdoTotal        =  0                                                                                                                         
          Tmp-tarjetadb.SdoDispon       =  0                                                                                                                         
          Tmp-tarjetadb.RetCode         = -1.
   RELEASE Tmp-Tarjetadb.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
HIDE FRAME frmConsultaReclamos.
VIEW FRAME F-Main.

FIND FIRST Cfg_Tarjeta NO-LOCK NO-ERROR.

IF w-seleccion EQ 1 OR W-Seleccion EQ 2 THEN DO:
    ASSIGN W-Cedula:SCREEN-VALUE IN FRAME f-main = ""
           W-cedula
           W-Consecutivo:SCREEN-VALUE = ""
           W-Consecutivo
           w_NomTitular:SCREEN-VALUE = ""
           W_NomTitular
           W-Descripcion:SCREEN-VALUE = ""
           W-Descripcion
           W-HoraRec:SCREEN-VALUE = STRING(TIME,"HH:MM:SS")
           W-HoraRec
           W-FecRec:SCREEN-VALUE = STRING(W_Fecha)
           W-FecRec.

    DISABLE Btn_Salvar
            Btn_Cancelar
            Btn_Borrar
            Btn_Modificar
            Cmb-Tipo
            Cmb-Estado
            W-Descripcion
        WITH FRAME F-Main.

    cmb-estado:LIST-ITEMS = "".
    W_OK = cmb-Estado:ADD-LAST("01 RADICADO").

    IF w_usuario = cfg_tarjetaDB.Usuario THEN DO:
        btn_consulta:SENSITIVE = TRUE.
        W_OK = cmb-Estado:ADD-LAST("02 PENDIENTE").
        w_ok = cmb-Estado:ADD-LAST("03 TRANSACCIÓN NO EXITOSA").
        w_ok = cmb-Estado:ADD-LAST("04 TRANSACCIÓN EXITOSA").
    END.

    cmb-Tipo:LIST-ITEMS = "".
    W_Ok = cmb-tipo:ADD-LAST("01 DÉBITO NO PAGO").
    W_Ok = cmb-tipo:ADD-LAST("02 PAGO PARCIAL RETIRO").
    W_Ok = cmb-tipo:ADD-LAST("03 RETIRO DOBLE O TRIPLE").
    W_Ok = cmb-tipo:ADD-LAST("04 RETIRO NO REALIZADO").
END.
        
ASSIGN w-cedula.
    
CmbAgenciasConsulta:LIST-ITEMS IN FRAME frmConsultaReclamos = "".
FOR EACH agencias NO-LOCK:
    W_Ok = CmbAgenciasConsulta:ADD-LAST(STRING(agencias.agencia,"99") + " - " + agencias.nombre).
END.

txtFechaInicial:SCREEN-VALUE = STRING(w_fecha).
txtFechaFinal:SCREEN-VALUE = STRING(w_fecha).

txtFechaInicial:SCREEN-VALUE IN FRAME frmConsultaReclamos = STRING(w_fecha).
txtFechaFinal:SCREEN-VALUE IN FRAME frmConsultaReclamos = STRING(w_fecha).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
DEFINE VAR nombreTitular AS CHARACTER.
DEFINE VAR motivoReclamo AS CHARACTER.
DEFINE VAR nombreRed AS CHARACTER.
DEFINE VAR cajero AS CHARACTER.
                                      
FOR EACH TT_Movimientos:
    FIND FIRST clientes WHERE clientes.nit = TT_Movimientos.nit NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN
        nombreTitular = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.

    CASE TT_Movimientos.tipoRec:
        WHEN 1 THEN motivoReclamo = "DÉBITO NO PAGO".
        WHEN 2 THEN motivoReclamo = "PAGO PARCIAL RETIRO".
        WHEN 3 THEN motivoReclamo = "RETIRO DOBLE O TRIPLE".
        WHEN 4 THEN motivoReclamo = "RETIRO NO REALIZADO".
    END CASE.

    CASE TT_Movimientos.tipoRed:
        WHEN 1 THEN nombreRed = "Servibanca".
        WHEN 2 THEN nombreRed = "Redeban".
        WHEN 3 THEN nombreRed = "Credibanco".
        WHEN 4 THEN nombreRed = "ATH Red Megabanco".
        WHEN 5 THEN nombreRed = "Red Express".
        WHEN 6 THEN nombreRed = "ATH Banco de Bogotá".
        WHEN 7 THEN nombreRed = "ATH Aval".
        WHEN 8 THEN nombreRed = "Otras Redes recibidas por ATH".
    END CASE.

    cajero = STRING(TT_Movimientos.retCode) + " - " + nombreRed.

    DISPLAY "NOMBRE TITULAR     :   " nombreTitular FORMAT "X(50)" AT 30 SKIP
            "CÉDULA             :   " TT_Movimientos.nit FORMAT "X(12)" AT 30 SKIP
            "TARJETA            :   " TT_Movimientos.tarjeta FORMAT "X(16)" AT 30 SKIP
            "CAJERO             :   " cajero FORMAT "X(50)" AT 30 SKIP
            "FECHA              :   " STRING(TT_Movimientos.fec_trans,"99/99/9999") AT 30 SKIP
            "HORA               :   " STRING(TT_Movimientos.hora_trans,"HH:MM:SS") AT 30 SKIP
            "OPERACIÓN          :   " STRING(TT_Movimientos.naud) FORMAT "X(20)" AT 30 SKIP
            "MONTO TRANSACCIÓN  :   " STRING(TT_Movimientos.monto) AT 30 SKIP
            "COMISIÓN           :   " STRING(TT_Movimientos.comision) AT 30 SKIP
            "VALOR A RECLAMAR   :   " STRING(TT_Movimientos.monto) AT 30 SKIP
            "CONCEPTO           :   " motivoReclamo FORMAT "X(30)" AT 30 SKIP(2)
        WITH FRAME F-Reclamos USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 100 NO-LABELS.
    

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

