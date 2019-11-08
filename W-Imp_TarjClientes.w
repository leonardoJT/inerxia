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

{Incluido\variable.i "shared"}
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE w_ok     AS LOGICAL NO-UNDO.
DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0   NO-UNDO.
DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999 NO-UNDO.

DEFINE VAR w_sw  AS LOGICAL EXTENT 7 INITIAL FALSE.
DEFINE TEMP-TABLE Tmp-Tarjetas LIKE Tarjetas
    FIELD TdetEst  AS CHARACTER FORMAT "X(10)" INITIAL ""
    INDEX tTNom IS PRIMARY Nombres ASCENDING
    INDEX tTtarjDb    TarjetaDb    ASCENDING
    INDEX tTCtaAhorro Cue_Ahorros  ASCENDING
    INDEX tTCtaCupo   Num_credito  ASCENDING.

DEFINE TEMP-TABLE Tmp-Sintar 
    FIELD TCue_Ahorros LIKE ahorros.cue_ahorros 
    FIELD Tusuario     LIKE Tarjetas.usuario
    FIELD Tnombres     AS   CHARACTER FORMAT "X(80)"
    FIELD TAgencia     LIKE ahorros.agencia
    FIELD TNit         LIKE ahorros.nit
    FIELD TNum_Credito LIKE creditos.num_credito
    FIELD Tsdo_dispon  LIKE Ahorros.Sdo_disponible
    FIELD Tsdo_canje   LIKE Ahorros.Sdo_canje
    FIELD Tsdo_total   LIKE Ahorros.Sdo_disponible
    INDEX idxage Tagencia Tsdo_Total DESCENDING.

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
&Scoped-define FRAME-NAME Frm-Main
&Scoped-define BROWSE-NAME Brw-Consulta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Tmp-Tarjetas Tarjetas

/* Definitions for BROWSE Brw-Consulta                                  */
&Scoped-define FIELDS-IN-QUERY-Brw-Consulta Tmp-Tarjetas.TarjetaDB Tmp-Tarjetas.Nombres Tmp-Tarjetas.estado Tmp-Tarjetas.TdetEst Tmp-Tarjetas.Hora_Bloqueo Tmp-Tarjetas.Fec_Bloqueo Tmp-Tarjetas.Agencia Tmp-tarjetas.Usuario Tmp-Tarjetas.Nit Tmp-Tarjetas.Cue_Ahorros Tmp-Tarjetas.Num_Credito   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw-Consulta   
&Scoped-define SELF-NAME Brw-Consulta
&Scoped-define QUERY-STRING-Brw-Consulta FOR EACH Tmp-Tarjetas
&Scoped-define OPEN-QUERY-Brw-Consulta OPEN QUERY {&SELF-NAME} FOR EACH Tmp-Tarjetas.
&Scoped-define TABLES-IN-QUERY-Brw-Consulta Tmp-Tarjetas
&Scoped-define FIRST-TABLE-IN-QUERY-Brw-Consulta Tmp-Tarjetas


/* Definitions for FRAME Frm-Main                                       */
&Scoped-define FIELDS-IN-QUERY-Frm-Main Tarjetas.TarjetaDB 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Frm-Main Tarjetas.TarjetaDB 
&Scoped-define ENABLED-TABLES-IN-QUERY-Frm-Main Tarjetas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Frm-Main Tarjetas
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frm-Main ~
    ~{&OPEN-QUERY-Brw-Consulta}
&Scoped-define QUERY-STRING-Frm-Main FOR EACH Tarjetas SHARE-LOCK
&Scoped-define OPEN-QUERY-Frm-Main OPEN QUERY Frm-Main FOR EACH Tarjetas SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Frm-Main Tarjetas
&Scoped-define FIRST-TABLE-IN-QUERY-Frm-Main Tarjetas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Tarjetas.TarjetaDB 
&Scoped-define ENABLED-TABLES Tarjetas
&Scoped-define FIRST-ENABLED-TABLE Tarjetas
&Scoped-Define ENABLED-OBJECTS RECT-338 RECT-339 RECT-340 Btn-Procesar ~
W-FecIni R-tipo s-EstadosIni W-FecFin s-EstadosFin Btn-Visualizar W-Cedula ~
Btn_Cancelar Btn_Done Cmb_Agencias Brw-Consulta 
&Scoped-Define DISPLAYED-FIELDS Tarjetas.TarjetaDB 
&Scoped-define DISPLAYED-TABLES Tarjetas
&Scoped-define FIRST-DISPLAYED-TABLE Tarjetas
&Scoped-Define DISPLAYED-OBJECTS W-FecIni R-tipo s-EstadosIni W-FecFin ~
s-EstadosFin W-Cedula W-Rango Cmb_Agencias 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn-Visualizar Btn_Cancelar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE Cmb-CtaAhoFin AS CHARACTER FORMAT "X(20)":U 
     LABEL "Final" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 68 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb-CtaAhoIni AS CHARACTER FORMAT "X(20)":U 
     LABEL "Inicial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 68 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-343
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.72 BY 2.54.

DEFINE VARIABLE Cmb-CtaCupoFin AS CHARACTER FORMAT "X(20)":U 
     LABEL "Final" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 68 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb-CtaCupoIni AS CHARACTER FORMAT "X(20)":U 
     LABEL "Inicial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 68 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-344
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.29 BY 2.62.

DEFINE BUTTON Btn-Procesar 
     IMAGE-UP FILE "imagenes/proceso_g.bmp":U
     LABEL "Procesar" 
     SIZE 8 BY 1.88.

DEFINE BUTTON Btn-Visualizar 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1.88.

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/borrar.bmp":U
     LABEL "&Cancelar" 
     SIZE 8 BY 1.88
     FONT 4.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/exit01.ico":U
     LABEL "&Salir" 
     SIZE 8 BY 1.88 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(40)":U 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W-Cedula LIKE Tarjetas.Nit
     VIEW-AS FILL-IN 
     SIZE 16.43 BY .81 NO-UNDO.

DEFINE VARIABLE W-FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W-FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W-Rango AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Valores superiores a" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE R-tipo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Una Cédula    ", 1,
"Una Tarjeta   ", 2,
"Nombre Cliente", 3,
"Tarjeta", 4,
"Cuenta Ahorro", 5,
"Cuenta Cupo", 6,
"Sin Tarjeta", 7,
"Cambios de Monto", 8
     SIZE 20.29 BY 4.85 NO-UNDO.

DEFINE RECTANGLE RECT-338
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22.29 BY 5.38
     FGCOLOR 8 .

DEFINE RECTANGLE RECT-339
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 23.29 BY 2.38
     FGCOLOR 8 .

DEFINE RECTANGLE RECT-340
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 2.42.

DEFINE VARIABLE s-EstadosFin AS CHARACTER INITIAL "62 Bloqueada" 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "00 Disponible","01 Activa","02 Cancelada","41 Bloqueada","62 Bloqueada" 
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE s-EstadosIni AS CHARACTER INITIAL "00 Disponible" 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "00 Disponible","01 Activa","02 Cancelada   ","41 Bloqueada ","62 Bloqueada" 
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE Cmb-NomFin AS CHARACTER FORMAT "X(256)":U 
     LABEL "Final" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 68 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb-NomIni AS CHARACTER FORMAT "X(256)":U 
     LABEL "Inicial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 68 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-341
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.72 BY 2.5.

DEFINE VARIABLE Cmb-TarFin AS CHARACTER FORMAT "X(256)":U 
     LABEL "Final" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 68 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb-TarIni AS CHARACTER FORMAT "X(256)":U 
     LABEL "Inicial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 68 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-342
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw-Consulta FOR 
      Tmp-Tarjetas SCROLLING.

DEFINE QUERY Frm-Main FOR 
      Tarjetas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw-Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw-Consulta C-Win _FREEFORM
  QUERY Brw-Consulta DISPLAY
      Tmp-Tarjetas.TarjetaDB       COLUMN-LABEL "Tarjeta"       FORMAT "X(20)"
     Tmp-Tarjetas.Nombres         COLUMN-LABEL "Nombres"        FORMAT "X(35)"
     Tmp-Tarjetas.estado          COLUMN-LABEL "Estado"  
     Tmp-Tarjetas.TdetEst         COLUMN-LABEL "DescEstado"
     Tmp-Tarjetas.Hora_Bloqueo    COLUMN-LABEL "Hora Bloqueo"
     Tmp-Tarjetas.Fec_Bloqueo     COLUMN-LABEL "Fec Bloqueo"
     Tmp-Tarjetas.Agencia         COLUMN-LABEL "Agencia"
     Tmp-tarjetas.Usuario         COLUMN-LABEL "Usuario"
     Tmp-Tarjetas.Nit             COLUMN-LABEL "Cédula"
     Tmp-Tarjetas.Cue_Ahorros     COLUMN-LABEL "CtaAho"
     Tmp-Tarjetas.Num_Credito     COLUMN-LABEL "CtaCupo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 110 BY 10.62
         FGCOLOR 0  ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm-Main
     Btn-Procesar AT ROW 1.27 COL 104.29 WIDGET-ID 86
     W-FecIni AT ROW 2 COL 52.14 COLON-ALIGNED WIDGET-ID 64
     R-tipo AT ROW 2.08 COL 2.72 NO-LABEL WIDGET-ID 56
     s-EstadosIni AT ROW 2.08 COL 78 NO-LABEL WIDGET-ID 76
     W-FecFin AT ROW 2.92 COL 52.14 COLON-ALIGNED WIDGET-ID 66
     s-EstadosFin AT ROW 3 COL 78 NO-LABEL WIDGET-ID 78
     Btn-Visualizar AT ROW 3.19 COL 104.29 WIDGET-ID 166
     W-Cedula AT ROW 4.77 COL 35.14 COLON-ALIGNED HELP
          "Nit del tercero" NO-LABEL WIDGET-ID 180
     Btn_Cancelar AT ROW 5.12 COL 104.29 WIDGET-ID 16
     W-Rango AT ROW 5.73 COL 80.43 COLON-ALIGNED WIDGET-ID 168
     Tarjetas.TarjetaDB AT ROW 5.77 COL 35.14 COLON-ALIGNED WIDGET-ID 178
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
     Btn_Done AT ROW 7.04 COL 104.43 WIDGET-ID 20
     Cmb_Agencias AT ROW 7.38 COL 10.43 COLON-ALIGNED WIDGET-ID 170
     Brw-Consulta AT ROW 11.92 COL 2.29 WIDGET-ID 200
     " Estados" VIEW-AS TEXT
          SIZE 9 BY .81 AT ROW 1.19 COL 70.57 WIDGET-ID 74
     "[dd/mm/aaaa]" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 1.19 COL 55.43 WIDGET-ID 70
          FGCOLOR 12 
     "Final:" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 3 COL 70.86 WIDGET-ID 82
     "Cédula:" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 4.77 COL 26.86 WIDGET-ID 182
     " Consulta por:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 1.27 COL 3 WIDGET-ID 62
     "Inicial:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 2.19 COL 70.86 WIDGET-ID 80
     " Fechas:" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 1.19 COL 46.43 WIDGET-ID 68
          FGCOLOR 12 
     RECT-338 AT ROW 1.81 COL 1.72 WIDGET-ID 60
     RECT-339 AT ROW 1.65 COL 46 WIDGET-ID 72
     RECT-340 AT ROW 1.65 COL 69.86 WIDGET-ID 84
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.86 BY 22.42 WIDGET-ID 100.

DEFINE FRAME Frm-CtaCupo
     Cmb-CtaCupoIni AT ROW 1.88 COL 8 COLON-ALIGNED WIDGET-ID 2
     Cmb-CtaCupoFin AT ROW 2.96 COL 8 COLON-ALIGNED WIDGET-ID 4
     " Cuentas Cupo Rotativo:" VIEW-AS TEXT
          SIZE 23 BY .81 AT ROW 1.04 COL 2 WIDGET-ID 8
     RECT-344 AT ROW 1.46 COL 1.72 WIDGET-ID 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 8.46
         SIZE 79 BY 3.23
         BGCOLOR 8  WIDGET-ID 600.

DEFINE FRAME Frm-CtaAhorro
     Cmb-CtaAhoIni AT ROW 1.81 COL 8 COLON-ALIGNED WIDGET-ID 6
     Cmb-CtaAhoFin AT ROW 2.88 COL 8 COLON-ALIGNED WIDGET-ID 8
     " Cuentas de Ahorro:" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 1 COL 2 WIDGET-ID 2
     RECT-343 AT ROW 1.42 COL 1.43 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 8.46
         SIZE 79 BY 3.23
         BGCOLOR 8  WIDGET-ID 500.

DEFINE FRAME Frm-Nombres
     Cmb-NomIni AT ROW 1.77 COL 8 COLON-ALIGNED WIDGET-ID 6
     Cmb-NomFin AT ROW 2.81 COL 8 COLON-ALIGNED WIDGET-ID 8
     " Clientes:" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 1.19 COL 2.57 WIDGET-ID 2
     RECT-341 AT ROW 1.46 COL 1.29 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 2 ROW 8.46
         SIZE 79 BY 3.23
         BGCOLOR 8  WIDGET-ID 300.

DEFINE FRAME Frm-Tarjeta
     Cmb-TarIni AT ROW 1.88 COL 8 COLON-ALIGNED WIDGET-ID 6
     Cmb-TarFin AT ROW 2.96 COL 8 COLON-ALIGNED WIDGET-ID 8
     " Tarjeta:" VIEW-AS TEXT
          SIZE 8.43 BY .81 AT ROW 1 COL 2.57 WIDGET-ID 4
     RECT-342 AT ROW 1.62 COL 1.29 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 8.46
         SIZE 79 BY 3.23
         BGCOLOR 8  WIDGET-ID 400.


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
         TITLE              = "Listado de Tarjetas/Clientes - W-Imp_TarjClientes.w"
         HEIGHT             = 22.54
         WIDTH              = 112.57
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
ASSIGN FRAME Frm-CtaAhorro:FRAME = FRAME Frm-Main:HANDLE
       FRAME Frm-CtaCupo:FRAME = FRAME Frm-Main:HANDLE
       FRAME Frm-Nombres:FRAME = FRAME Frm-Main:HANDLE
       FRAME Frm-Tarjeta:FRAME = FRAME Frm-Main:HANDLE.

/* SETTINGS FOR FRAME Frm-CtaAhorro
                                                                        */
ASSIGN 
       FRAME Frm-CtaAhorro:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME Frm-CtaCupo
                                                                        */
ASSIGN 
       FRAME Frm-CtaCupo:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME Frm-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB Brw-Consulta Frm-CtaCupo Frm-Main */
/* SETTINGS FOR BUTTON Btn-Visualizar IN FRAME Frm-Main
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME Frm-Main
   6                                                                    */
/* SETTINGS FOR FILL-IN W-Cedula IN FRAME Frm-Main
   LIKE = bdcentral.Tarjetas.Nit EXP-SIZE                               */
/* SETTINGS FOR FILL-IN W-Rango IN FRAME Frm-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME Frm-Nombres
   UNDERLINE                                                            */
/* SETTINGS FOR FRAME Frm-Tarjeta
                                                                        */
ASSIGN 
       FRAME Frm-Tarjeta:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw-Consulta
/* Query rebuild information for BROWSE Brw-Consulta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Tmp-Tarjetas.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Brw-Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frm-CtaAhorro
/* Query rebuild information for FRAME Frm-CtaAhorro
     _Query            is NOT OPENED
*/  /* FRAME Frm-CtaAhorro */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frm-CtaCupo
/* Query rebuild information for FRAME Frm-CtaCupo
     _Query            is NOT OPENED
*/  /* FRAME Frm-CtaCupo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frm-Main
/* Query rebuild information for FRAME Frm-Main
     _TblList          = "bdcentral.Tarjetas"
     _Query            is OPENED
*/  /* FRAME Frm-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frm-Nombres
/* Query rebuild information for FRAME Frm-Nombres
     _Query            is NOT OPENED
*/  /* FRAME Frm-Nombres */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frm-Tarjeta
/* Query rebuild information for FRAME Frm-Tarjeta
     _Query            is NOT OPENED
*/  /* FRAME Frm-Tarjeta */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Listado de Tarjetas/Clientes - W-Imp_TarjClientes.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Listado de Tarjetas/Clientes - W-Imp_TarjClientes.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Brw-Consulta
&Scoped-define SELF-NAME Brw-Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-Consulta C-Win
ON MOUSE-SELECT-DBLCLICK OF Brw-Consulta IN FRAME Frm-Main
DO:
    IF R-Tipo <> 8 THEN DO:
        InputFile = "Formatos\AV - 319.xls".
        
        SwExiste = SEARCH(InputFile).

        IF SwExiste EQ ? THEN DO:
            MESSAGE InputFile "no encontrado."
                VIEW-AS ALERT-BOX.
            RETURN.
        END.

        FIND FIRST Usuarios WHERE Usuarios.Usuario = W_Usuario NO-LOCK NO-ERROR.
        FIND FIRST Clientes WHERE Clientes.Nit = Tmp-Tarjetas.Nit NO-LOCK NO-ERROR.
        FIND FIRST Agencias WHERE Agencias.Agencia = w_agencia NO-LOCK NO-ERROR.

        CREATE "Excel.Application" chExcelApp.

        hWorkBooks = chExcelApp:WorkBooks:OPEN(SwExiste,,TRUE,).

        IF hWorkBooks THEN DO:
            chExcelApp:Visible = TRUE.
            chWorkSheet = chExcelApp:Sheets:Item(1).
        END.
        ELSE
            SwExiste = ?.

        ASSIGN ValCol = "A5"
               Dato = Agencias.Nombre
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "O5"
               Dato = Tmp-Tarjetas.TarjetaDB
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "V5"
               Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "Y26"
               Dato = Clientes.nit
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "B27"
               Dato = usuarios.nombre
               chWorkSheet:Range(ValCol):VALUE = Dato.
    END.
    ELSE DO:
        InputFile = "Formatos\AV - 323.xls".

        SwExiste = SEARCH(InputFile).

        IF SwExiste EQ ? THEN DO:
            MESSAGE InputFile "no encontrado."
                VIEW-AS ALERT-BOX.
            RETURN.
        END.

        FIND FIRST Usuarios WHERE Usuarios.Usuario = Tmp-tarjetas.Usuario NO-LOCK NO-ERROR.
        FIND FIRST Clientes WHERE Clientes.Nit = Tmp-tarjetas.nit NO-LOCK NO-ERROR.
        FIND FIRST Agencias WHERE Agencias.Agencia = Tmp-Tarjetas.agencia NO-LOCK NO-ERROR.
        FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Tmp-Tarjetas.tarjetaDB NO-ERROR.

        CREATE "Excel.Application" chExcelApp.

        hWorkBooks = chExcelApp:WorkBooks:OPEN(SwExiste,,TRUE,).

        IF hWorkBooks THEN DO:
            chExcelApp:Visible = TRUE.
            chWorkSheet = chExcelApp:Sheets:Item(1).
        END.
        ELSE
            SwExiste = ?.

        ASSIGN ValCol = "D5"
               Dato = Agencias.Nombre
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "B15"
               Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "AE15"
               Dato = Clientes.nit
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "D26"
               Dato = Clientes.dir_residencia
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "AA17"
               Dato = tarjetas.tarjetaDB
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "W21"
               Dato = STRING(tarjetas.MontoMaxCaj)
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "AF21"
               Dato = STRING(tarjetas.OperMaxCaj)
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "K23"
               Dato = STRING(tarjetas.MontoMaxPos)
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "T23"
               Dato = STRING(tarjetas.OperMaxPos)
               chWorkSheet:Range(ValCol):VALUE = Dato.

        ASSIGN ValCol = "AI31"
               Dato = usuarios.nombre
               chWorkSheet:Range(ValCol):VALUE = Dato.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Procesar C-Win
ON CHOOSE OF Btn-Procesar IN FRAME Frm-Main /* Procesar */
DO:
    ASSIGN R-Tipo
           W-FecIni
           W-FecFin
           S-EstadosIni
           s-estadosFin
           W-Cedula.

    EMPTY TEMP-TABLE tmp-Tarjetas.

    CASE R-Tipo:
        WHEN 1 THEN DO: /* Cédula */
            RUN _SetCurs.r ("Wait").
            FOR EACH Tarjetas WHERE Tarjetas.Nit EQ W-cedula
                                AND Tarjetas.Fec_Activacion GE w-FecIni
                                AND Tarjetas.Fec_Activacion LE w-FecFin
                                AND Tarjetas.Estado GE SUBSTRING(s-EstadosIni,1,2)
                                AND Tarjetas.Estado LE SUBSTRING(s-EstadosFin,1,2)
                                AND (Tarjetas.Agencia GE AgeIni AND Tarjetas.Agencia LE AgeFin) NO-LOCK BY Tarjetas.Nit:
                CREATE Tmp-Tarjetas.
                BUFFER-COPY tarjetas TO Tmp-Tarjetas.
                RUN DetEstado.
            END.

            OPEN QUERY Brw-Consulta FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.Nit INDEXED-REPOSITION.
            
            RUN _SetCurs.r ("No-wait").
        END.
        WHEN 2 THEN DO: /* Tarjeta Débito */
            RUN _SetCurs.r ("Wait").
            FOR EACH Tarjetas WHERE Tarjetas.TarjetaDb EQ Tarjetas.TarjetaDb:SCREEN-VALUE IN FRAME Frm-Main
                                AND Tarjetas.Fec_Activacion GE w-FecIni
                                AND Tarjetas.Fec_Activacion LE w-FecFin
                                AND Tarjetas.Estado GE SUBSTRING(s-EstadosIni,1,2)
                                AND Tarjetas.Estado LE SUBSTRING(s-EstadosFin,1,2)
                                AND (Tarjetas.Agencia GE AgeIni AND Tarjetas.Agencia LE AgeFin) NO-LOCK BY Tarjetas.Nit:
                CREATE Tmp-Tarjetas.
                BUFFER-COPY tarjetas TO Tmp-Tarjetas.
                RUN DetEstado.
            END.

            OPEN QUERY Brw-Consulta FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.TarjetaDb INDEXED-REPOSITION.
            RUN _SetCurs.r("No-wait").
        END.
        WHEN 3 THEN DO:
            RUN _SetCurs.r ("Wait").
            FOR EACH Tarjetas WHERE Tarjetas.Nombres GE Cmb-NomIni:SCREEN-VALUE IN FRAME frm-Nombres
                                AND Tarjetas.nombres LE Cmb-NomFin:SCREEN-VALUE IN FRAME frm-Nombres
                                AND Tarjetas.Fec_Activacion GE w-FecIni
                                AND Tarjetas.Fec_Activacion LE w-FecFin
                                AND Tarjetas.Estado GE SUBSTRING(s-EstadosIni,1,2)
                                AND Tarjetas.Estado LE SUBSTRING(s-EstadosFin,1,2)
                                AND (Tarjetas.Agencia GE AgeIni AND Tarjetas.Agencia LE AgeFin) NO-LOCK BY Tarjetas.Nombres:
                CREATE Tmp-Tarjetas.
                BUFFER-COPY tarjetas TO Tmp-Tarjetas.
                RUN DetEstado.
            END.

            OPEN QUERY Brw-Consulta FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.Nombres INDEXED-REPOSITION.
            RUN _SetCurs.r("No-wait").
        END.
        WHEN 4 THEN DO:
            RUN _SetCurs.r ("Wait").
            
            FOR EACH Tarjetas WHERE Tarjetas.TarjetaDB GE Cmb-TarIni:SCREEN-VALUE IN FRAME frm-Tarjeta
                                AND Tarjetas.TarjetaDb LE Cmb-TarFin:SCREEN-VALUE IN FRAME frm-Tarjeta
                                AND Tarjetas.Fec_Activacion GE w-FecIni
                                AND Tarjetas.Fec_Activacion LE w-FecFin
                                AND Tarjetas.Estado GE SUBSTRING(s-EstadosIni,1,2)
                                AND Tarjetas.Estado LE SUBSTRING(s-EstadosFin,1,2)
                                AND (Tarjetas.Agencia GE AgeIni AND Tarjetas.Agencia LE AgeFin) NO-LOCK BY Tarjetas.TarjetaDB:
                CREATE Tmp-Tarjetas.
                BUFFER-COPY Tarjetas TO Tmp-Tarjetas.
                RUN DetEstado.
            END.

            OPEN QUERY Brw-Consulta FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.TarjetaDB INDEXED-REPOSITION.
            RUN _SetCurs.r ("No-wait").
        END.

        WHEN 5 THEN DO:
            RUN _SetCurs.r ("Wait").
            FOR EACH Tarjetas WHERE tarjetas.Cue_ahorros GE Cmb-CtaAhoIni:SCREEN-VALUE IN FRAME frm-CtaAhorro
                                AND Tarjetas.Cue_Ahorros LE Cmb-CtaAhoFin:SCREEN-VALUE IN FRAME frm-CtaAhorro
                                AND Tarjetas.Fec_Activacion GE w-FecIni
                                AND Tarjetas.Fec_Activacion LE w-FecFin
                                AND Tarjetas.Estado GE SUBSTRING(s-EstadosIni,1,2)
                                AND Tarjetas.Estado LE SUBSTRING(s-EstadosFin,1,2)
                                AND (Tarjetas.Agencia GE AgeIni AND Tarjetas.Agencia LE AgeFin) NO-LOCK BY tarjetas.Cue_ahorros:
                CREATE Tmp-Tarjetas.
                BUFFER-COPY Tarjetas TO Tmp-Tarjetas.
                RUN DetEstado.
            END.

            OPEN QUERY Brw-Consulta FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.Cue_Ahorros INDEXED-REPOSITION.
            RUN _SetCurs.r ("No-wait").
        END.

        WHEN 6 THEN DO:
            RUN _SetCurs.r ("Wait").
            FOR EACH Tarjetas WHERE Tarjetas.Num_credito GE INT64(CMB-CTACUPOINI:SCREEN-VALUE IN FRAME frm-CtaCupo)
                                AND Tarjetas.Num_Credito LE INT64(CMB-CTACUPOFIN:SCREEN-VALUE IN FRAME frm-CtaCupo)
                                AND Tarjetas.Fec_ActCupo GE w-FecIni
                                AND Tarjetas.Fec_ActCupo LE w-FecFin
                                AND Tarjetas.Estado GE SUBSTRING(s-EstadosIni,1,2)
                                AND Tarjetas.Estado LE SUBSTRING(s-EstadosFin,1,2)
                                AND (Tarjetas.Agencia GE AgeIni AND Tarjetas.Agencia LE AgeFin) NO-LOCK BY Tarjetas.Num_credito :
                CREATE Tmp-Tarjetas.
                BUFFER-COPY Tarjetas TO Tmp-Tarjetas.
                RUN DetEstado.
            END.

            OPEN QUERY Brw-Consulta FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.Num_credito INDEXED-REPOSITION.
            RUN _SetCurs.r ("No-wait").
        END.

        WHEN 7 THEN DO:
            EMPTY TEMP-TABLE Tmp-Sintar.
            RUN _SetCurs.r ("Wait").

            FOR EACH ahorros WHERE ahorros.cod_ahorro EQ cfg_tarjeta.cod_ahorro
                               AND ahorros.estado EQ 1 NO-LOCK:
                IF (Ahorros.sdo_Disponible + Sdo_canje) LE W-Rango THEN
                    NEXT.

                IF (Ahorros.Agencia GE AgeIni AND Ahorros.Agencia LE AgeFin) THEN DO:
                    FIND FIRST tarjetas WHERE tarjetas.nit = ahorros.nit AND tarjetas.estado = "01" NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE(tarjetas) THEN DO:
                        CREATE Tmp-Sintar.
                        ASSIGN Tmp-Sintar.TCue_Ahorros = ahorros.cue_ahorros
                               Tmp-Sintar.TAgencia = Ahorros.agencia
                               Tmp-Sintar.TNit = Ahorros.nit
                               Tmp-Sintar.TSdo_dispon = Ahorros.Sdo_disponible
                               Tmp-Sintar.TSdo_canje = Ahorros.Sdo_canje
                               Tmp-Sintar.Tsdo_total = Ahorros.Sdo_disponible + Ahorros.Sdo_canje.

                        FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
                        IF AVAILABLE(clientes) THEN
                            ASSIGN Tmp-Sintar.Tnombres = TRIM(clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(clientes.nombre).
                        ELSE
                            Tmp-Sintar.Tnombres = "Sin nombre".
                    END.
                END.
            END.

            RUN _SetCurs.r ("No-wait").
        END.
            
        WHEN 8 THEN DO: /* Cédula */
            RUN _SetCurs.r ("Wait").
                
            FOR EACH Hoja_Vida WHERE Hoja_vida.nit = w-cedula
                                 AND hoja_vida.fec_grabacion >= w-fecIni
                                 AND hoja_vida.fec_grabacion <= w-fecFin
                                 AND Hoja_Vida.Asunto_Cumplido = TRUE
                                 AND Hoja_Vida.Codigo = 10
                                 AND Hoja_Vida.Tipo = 50 NO-LOCK:
                FIND FIRST tarjetas WHERE tarjetas.nit = w-cedula
                                      AND tarjetas.tarjetaDB = SUBSTRING(Hoja_Vida.observacion,19) NO-LOCK NO-ERROR.
                IF AVAILABLE tarjetas THEN DO:
                    CREATE Tmp-Tarjetas.
                    Tmp-tarjetas.tarjetaDB = tarjetas.tarjetaDB.
                    Tmp-Tarjetas.Nombres = tarjetas.nombres.
                    Tmp-Tarjetas.estado = Tarjetas.estado.
                    /*Tmp-Tarjetas.Hora_Bloqueo = Tarjetas.Hora_ActMonto.*/
                    Tmp-Tarjetas.Fec_Bloqueo = Tarjetas.Fec_ActMonto.
                    Tmp-Tarjetas.Agencia = Tarjetas.Agencia.
                    Tmp-tarjetas.Usuario = tarjetas.usuario.
                    Tmp-Tarjetas.Nit = tarjetas.nit.
                    Tmp-Tarjetas.Cue_Ahorros = tarjetas.cue_ahorros.
                    Tmp-Tarjetas.Num_Credito = tarjetas.num_credito.
                    Tmp-Tarjetas.usuario = Hoja_vida.usuario.
                    
                    RUN DetEstado.
                END.
            END.
        
            OPEN QUERY Brw-Consulta FOR EACH Tmp-Tarjetas NO-LOCK /*BY Tmp-Tarjetas.Nit*/ INDEXED-REPOSITION.
                    
            RUN _SetCurs.r ("No-wait").
        END.
    END CASE.

    /*APPLY "CHOOSE" TO btn-Visualizar.*/
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME Frm-Main /* Cancelar */
DO:
  RUN Inicializar_Variables.
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


&Scoped-define FRAME-NAME Frm-CtaAhorro
&Scoped-define SELF-NAME Cmb-CtaAhoFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-CtaAhoFin C-Win
ON VALUE-CHANGED OF Cmb-CtaAhoFin IN FRAME Frm-CtaAhorro /* Final */
DO:
  ASSIGN Cmb-CtaAhoFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-CtaAhoIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-CtaAhoIni C-Win
ON VALUE-CHANGED OF Cmb-CtaAhoIni IN FRAME Frm-CtaAhorro /* Inicial */
DO:
  ASSIGN Cmb-CtaAhoIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm-CtaCupo
&Scoped-define SELF-NAME Cmb-CtaCupoFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-CtaCupoFin C-Win
ON VALUE-CHANGED OF Cmb-CtaCupoFin IN FRAME Frm-CtaCupo /* Final */
DO:
  ASSIGN Cmb-CtaCupoFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-CtaCupoIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-CtaCupoIni C-Win
ON VALUE-CHANGED OF Cmb-CtaCupoIni IN FRAME Frm-CtaCupo /* Inicial */
DO:
  ASSIGN Cmb-CtaCupoIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm-Nombres
&Scoped-define SELF-NAME Cmb-NomFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-NomFin C-Win
ON VALUE-CHANGED OF Cmb-NomFin IN FRAME Frm-Nombres /* Final */
DO:
  ASSIGN Cmb-NomFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-NomIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-NomIni C-Win
ON VALUE-CHANGED OF Cmb-NomIni IN FRAME Frm-Nombres /* Inicial */
DO:
  ASSIGN Cmb-NomIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm-Tarjeta
&Scoped-define SELF-NAME Cmb-TarFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-TarFin C-Win
ON VALUE-CHANGED OF Cmb-TarFin IN FRAME Frm-Tarjeta /* Final */
DO:
  ASSIGN Cmb-TarFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-TarIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-TarIni C-Win
ON VALUE-CHANGED OF Cmb-TarIni IN FRAME Frm-Tarjeta /* Inicial */
DO:
  ASSIGN Cmb-TarIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm-Main
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias C-Win
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME Frm-Main /* Agencias */
DO:
  ASSIGN Cmb_Agencias.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-tipo C-Win
ON VALUE-CHANGED OF R-tipo IN FRAME Frm-Main
DO:
    ASSIGN R-Tipo.

    CASE R-Tipo:
        WHEN 1 THEN DO: 
            ASSIGN W-Cedula:VISIBLE            = TRUE
                   Tarjetas.TarjetaDb:VISIBLE  = FALSE
                   FRAME Frm-Nombres:VISIBLE   = FALSE   /* Nombre Clientes */
                   FRAME Frm-Tarjeta:VISIBLE   = FALSE
                   FRAME Frm-CtaAhorro:VISIBLE = FALSE 
                   FRAME Frm-CtaCupo:VISIBLE   = FALSE
                   W-Rango:VISIBLE             = FALSE
                   W-Rango:SENSITIVE           = FALSE.
        END.
        
        WHEN 2 THEN DO: 
            ASSIGN W-Cedula:VISIBLE            = FALSE
                   Tarjetas.TarjetaDb:VISIBLE  = TRUE
                   FRAME Frm-Nombres:VISIBLE   = FALSE   /* Nombre Clientes */
                   FRAME Frm-Tarjeta:VISIBLE   = FALSE
                   FRAME Frm-CtaAhorro:VISIBLE = FALSE 
                   FRAME Frm-CtaCupo:VISIBLE   = FALSE
                   W-Rango:VISIBLE             = FALSE
                   W-Rango:SENSITIVE           = FALSE.
        END.
        
        WHEN 3 THEN DO: 
            ASSIGN W-Cedula:VISIBLE            = FALSE
                   Tarjetas.TarjetaDb:VISIBLE  = FALSE
                   FRAME Frm-Nombres:VISIBLE   = TRUE   /* Nombre Clientes */
                   FRAME Frm-Tarjeta:VISIBLE   = FALSE
                   FRAME Frm-CtaAhorro:VISIBLE = FALSE 
                   FRAME Frm-CtaCupo:VISIBLE   = FALSE
                   W-Rango:VISIBLE             = FALSE
                   W-Rango:SENSITIVE           = FALSE.
            
            IF NOT w_sw[R-Tipo] THEN DO:
                RUN _SetCurs.r ("Wait").
                
                FOR EACH Tarjetas WHERE Tarjetas.nombres NE " " BREAK BY Tarjetas.Nombres:
                    ASSIGN w_ok = Cmb-NomIni:ADD-LAST(Tarjetas.Nombres) IN FRAME Frm-Nombres
                           w_ok = Cmb-NomFin:ADD-LAST(Tarjetas.Nombres) IN FRAME Frm-Nombres.
                    IF FIRST(Tarjetas.Nombres) THEN
                        ASSIGN Cmb-NomIni:SCREEN-VALUE IN FRAME Frm-Nombres = Tarjetas.Nombres.
                        
                    IF LAST(Tarjetas.Nombres) THEN
                        ASSIGN Cmb-NomFin:SCREEN-VALUE IN FRAME Frm-Nombres = Tarjetas.Nombres.
                END.
                
                ASSIGN w_sw[R-Tipo] = TRUE.
                RUN _SetCurs.r ("No-wait").
            END.
        END.
        
        WHEN 4 THEN DO:
            ASSIGN W-Cedula:VISIBLE            = FALSE 
                   Tarjetas.TarjetaDb:VISIBLE  = FALSE 
                   FRAME Frm-Nombres:VISIBLE   = FALSE   /* Tarjetas */ 
                   FRAME Frm-Tarjeta:VISIBLE   = TRUE
                   FRAME Frm-CtaAhorro:VISIBLE = FALSE 
                   FRAME Frm-CtaCupo:VISIBLE   = FALSE
                   W-Rango:VISIBLE             = FALSE
                   W-Rango:SENSITIVE           = FALSE.
            
            IF NOT w_sw[R-Tipo] THEN DO:
                RUN _SetCurs.r ("Wait").
                FOR EACH Tarjetas WHERE Tarjetas.TarjetaDb NE "" NO-LOCK BREAK BY Tarjetas.TarjetaDB:
                    ASSIGN w_ok = Cmb-TarIni:ADD-LAST(Tarjetas.TarjetaDB) IN FRAME Frm-Tarjeta
                           w_ok = Cmb-TarFin:ADD-LAST(Tarjetas.TarjetaDB) IN FRAME Frm-Tarjeta.
                    
                    IF FIRST(Tarjetas.TarjetaDB) THEN
                        ASSIGN Cmb-TarIni:SCREEN-VALUE IN FRAME Frm-Tarjeta = Tarjetas.TarjetaDb.
                    
                    IF LAST(Tarjetas.TarjetaDB) THEN                          
                        ASSIGN Cmb-TarFin:SCREEN-VALUE IN FRAME Frm-Tarjeta = Tarjetas.TarjetaDb.
                END.
                
                ASSIGN w_sw[R-Tipo] = TRUE.
                RUN _SetCurs.r ("No-wait").
            END.
        END.
        
        WHEN 5 THEN DO:
            ASSIGN W-Cedula:VISIBLE            = FALSE 
                   Tarjetas.TarjetaDb:VISIBLE  = FALSE 
                   FRAME Frm-Nombres:VISIBLE   = FALSE   /* Cuenta Ahorros */
                   FRAME Frm-Tarjeta:VISIBLE   = FALSE
                   FRAME Frm-CtaAhorro:VISIBLE = TRUE 
                   FRAME Frm-CtaCupo:VISIBLE   = FALSE
                   W-Rango:VISIBLE             = FALSE
                   W-Rango:SENSITIVE           = FALSE.
            IF NOT w_sw[R-Tipo] THEN DO:
                RUN _SetCurs.r ("Wait").
                FOR EACH Tarjetas WHERE Tarjetas.Cue_Ahorros NE ? BREAK BY Tarjetas.Cue_Ahorros:                                   
                   IF tarjetas.cue_ahorros NE "" THEN Cmb-CtaAhoFin = tarjetas.cue_ahorros.
                   ASSIGN w_ok = Cmb-CtaAhoIni:ADD-LAST(Tarjetas.Cue_Ahorros) IN FRAME Frm-CtaAhorro                        
                          w_ok = Cmb-CtaAhoFin:ADD-LAST(Tarjetas.Cue_Ahorros) IN FRAME Frm-CtaAhorro.                       
                   IF FIRST(Tarjetas.Cue_Ahorros) THEN                                                                      
                      ASSIGN Cmb-CtaAhoIni:SCREEN-VALUE IN FRAME Frm-CtaAhorro = Tarjetas.Cue_Ahorros.                      
                   IF LAST(Tarjetas.Cue_Ahorros) THEN  
                      ASSIGN Cmb-CtaAhoFin:SCREEN-VALUE IN FRAME Frm-CtaAhorro = Tarjetas.Cue_Ahorros. 
                END.                                                                                                    
                ASSIGN w_sw[R-Tipo] = TRUE
                       Cmb-CtaAhoFin:SCREEN-VALUE  IN FRAME Frm-CtaAhorro = Cmb-CtaAhoFin.
                RUN _SetCurs.r ("No-wait").
            END.
        END.
        
        WHEN 6 THEN DO:
            ASSIGN W-Cedula:VISIBLE            = FALSE  
                   Tarjetas.TarjetaDb:VISIBLE  = FALSE   
                   FRAME Frm-Nombres:VISIBLE   = FALSE  /* Cuenta Cupo Rotativo */ 
                   FRAME Frm-Tarjeta:VISIBLE   = FALSE
                   FRAME Frm-CtaAhorro:VISIBLE = FALSE 
                   FRAME Frm-CtaCupo:VISIBLE   = TRUE
                   W-Rango:VISIBLE             = FALSE
                   W-Rango:SENSITIVE           = FALSE.

            IF NOT w_sw[R-Tipo] THEN DO:
                RUN _SetCurs.r ("Wait").
                FOR EACH Tarjetas WHERE Tarjetas.Num_credito NE 0 BREAK BY Tarjetas.Num_credito:                                    
                    ASSIGN w_ok = Cmb-CtaCupoIni:ADD-LAST(STRING(Tarjetas.Num_credito)) IN FRAME Frm-CtaCupo                 
                           w_ok = Cmb-CtaCupoFin:ADD-LAST(STRING(Tarjetas.Num_credito)) IN FRAME Frm-CtaCupo.                
                    
                    IF FIRST(TArjetas.Num_Credito) THEN                                                                      
                       ASSIGN Cmb-CtaCupoIni:SCREEN-VALUE IN FRAME Frm-CtaCupo = TRIM(STRING(Tarjetas.Num_credito,"zzzzzzzzz9")).  
                    
                    IF LAST(Tarjetas.Num_credito) THEN                                                                       
                       ASSIGN Cmb-CtaCupoFin:SCREEN-VALUE IN FRAME Frm-CtaCupo = TRIM(STRING(Tarjetas.Num_credito,"zzzzzzzzz9")).  
                END.                                                                                                    
                
                ASSIGN w_sw[R-Tipo] = TRUE.
                RUN _SetCurs.r ("No-wait").
            END.
        END.
        
        WHEN 7 THEN DO:
            ASSIGN W-Cedula:VISIBLE            = FALSE 
                   Tarjetas.TarjetaDb:VISIBLE  = FALSE 
                   FRAME Frm-Nombres:VISIBLE   = FALSE  /* Cuentas sin Tarjeta */ 
                   FRAME Frm-Tarjeta:VISIBLE   = FALSE
                   FRAME Frm-CtaAhorro:VISIBLE = FALSE 
                   FRAME Frm-CtaCupo:VISIBLE   = FALSE
                   W-Rango:VISIBLE             = TRUE
                   W-Rango:SENSITIVE           = TRUE.
        END.
            
        WHEN 8 THEN DO:
            ASSIGN W-Cedula:VISIBLE = TRUE
                   Tarjetas.TarjetaDb:VISIBLE = FALSE
                   FRAME Frm-Nombres:VISIBLE = FALSE   /* Cambios de Monto */
                   FRAME Frm-Tarjeta:VISIBLE = FALSE
                   FRAME Frm-CtaAhorro:VISIBLE = FALSE
                   FRAME Frm-CtaCupo:VISIBLE = FALSE
                   W-Rango:VISIBLE = FALSE
                   W-Rango:SENSITIVE = FALSE.
        END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W-Rango
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Rango C-Win
ON LEAVE OF W-Rango IN FRAME Frm-Main /* Valores superiores a */
DO:
  ASSIGN W-Rango.
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
  ASSIGN FRAME Frm-Nombres:VISIBLE   = TRUE   
         FRAME Frm-Tarjeta:VISIBLE   = FALSE  
         FRAME Frm-CtaAhorro:VISIBLE = FALSE  
         FRAME Frm-CtaCupo:VISIBLE   = FALSE. 
  FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
     W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
     IF Agencias.Agencia EQ W_Agencia THEN 
       ASSIGN Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre
              AgeIni = Agencias.agencia   AgeFin = Agencias.agencia.
  END.
  RUN inicializar_variables.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DetEstado C-Win 
PROCEDURE DetEstado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CASE Tarjetas.Estado:
    WHEN "00" THEN ASSIGN Tmp-Tarjetas.TdetEst = 'Disponible'.
    WHEN "01" THEN ASSIGN Tmp-Tarjetas.TdetEst = 'Activa    '.
    WHEN "98" THEN ASSIGN Tmp-Tarjetas.TdetEst = 'Bloq. Temp'.
    OTHERWISE
       ASSIGN Tmp-Tarjetas.TdetEst = 'Bloqueada '.
END CASE.
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

  {&OPEN-QUERY-Frm-Main}
  GET FIRST Frm-Main.
  DISPLAY W-FecIni R-tipo s-EstadosIni W-FecFin s-EstadosFin W-Cedula W-Rango 
          Cmb_Agencias 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  IF AVAILABLE Tarjetas THEN 
    DISPLAY Tarjetas.TarjetaDB 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  ENABLE RECT-338 RECT-339 RECT-340 Btn-Procesar W-FecIni R-tipo s-EstadosIni 
         W-FecFin s-EstadosFin Btn-Visualizar W-Cedula Btn_Cancelar 
         Tarjetas.TarjetaDB Btn_Done Cmb_Agencias Brw-Consulta 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Main}
  DISPLAY Cmb-CtaAhoIni Cmb-CtaAhoFin 
      WITH FRAME Frm-CtaAhorro IN WINDOW C-Win.
  ENABLE RECT-343 Cmb-CtaAhoIni Cmb-CtaAhoFin 
      WITH FRAME Frm-CtaAhorro IN WINDOW C-Win.
  VIEW FRAME Frm-CtaAhorro IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-CtaAhorro}
  DISPLAY Cmb-CtaCupoIni Cmb-CtaCupoFin 
      WITH FRAME Frm-CtaCupo IN WINDOW C-Win.
  ENABLE RECT-344 Cmb-CtaCupoIni Cmb-CtaCupoFin 
      WITH FRAME Frm-CtaCupo IN WINDOW C-Win.
  VIEW FRAME Frm-CtaCupo IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-CtaCupo}
  DISPLAY Cmb-NomIni Cmb-NomFin 
      WITH FRAME Frm-Nombres IN WINDOW C-Win.
  ENABLE RECT-341 Cmb-NomIni Cmb-NomFin 
      WITH FRAME Frm-Nombres IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Nombres}
  DISPLAY Cmb-TarIni Cmb-TarFin 
      WITH FRAME Frm-Tarjeta IN WINDOW C-Win.
  ENABLE RECT-342 Cmb-TarIni Cmb-TarFin 
      WITH FRAME Frm-Tarjeta IN WINDOW C-Win.
  VIEW FRAME Frm-Tarjeta IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Tarjeta}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
FIND FIRST Cfg_Tarjeta NO-LOCK NO-ERROR.
DISABLE Btn-Visualizar.
ASSIGN Cmb-NomIni:LIST-ITEMS     IN FRAME Frm-Nombres   = "" Cmb-NomIni
       Cmb-NomFin:LIST-ITEMS     IN FRAME Frm-Nombres   = "" Cmb-NomFin
       Cmb-TarIni:LIST-ITEMS     IN FRAME Frm-Tarjeta   = "" Cmb-TarIni
       Cmb-TarFin:LIST-ITEMS     IN FRAME Frm-Tarjeta   = "" Cmb-TarFin
       Cmb-CtaAhoIni:LIST-ITEMS  IN FRAME Frm-CtaAhorro = "" Cmb-CtaAhoIni
       Cmb-CtaAhoFin:LIST-ITEMS  IN FRAME Frm-CtaAhorro = "" Cmb-CtaAhoFin
       Cmb-CtaCupoIni:LIST-ITEMS IN FRAME Frm-CtaCupo   = "" Cmb-CtaCupoIni
       Cmb-CtaCupoFin:LIST-ITEMS IN FRAME Frm-CtaCupo   = "" Cmb-CtaCupoFin
       W-Cedula:VISIBLE          IN FRAME Frm-Main = TRUE   
       Tarjetas.TarjetaDb:VISIBLE IN FRAME Frm-Main = FALSE  
       FRAME Frm-Nombres:VISIBLE   = FALSE  
       FRAME Frm-Tarjeta:VISIBLE   = FALSE  
       FRAME Frm-CtaAhorro:VISIBLE = FALSE  
       FRAME Frm-CtaCupo:VISIBLE   = FALSE  
       W-Rango:VISIBLE             = FALSE  
       W-Rango:SENSITIVE           = FALSE
       W-FecIni:SCREEN-VALUE IN FRAME Frm-main = STRING(W_Fecha - DAY(w_fecha) + 1)
       W-FecFin:SCREEN-VALUE IN FRAME Frm-main = STRING(W_Fecha)
       W-FecIni  W-FecFin W-Rango = 0 w-Rango:SCREEN-VALUE = "0"
       W-Rango:VISIBLE = FALSE   w-Rango:SENSITIVE = FALSE.  
     /*  IF NOT w_sw[R-Tipo] THEN DO:
           RUN _SetCurs.r ("Wait").
           FOR EACH Tarjetas WHERE Tarjetas.nombres NE " " BREAK BY Tarjetas.Nombres:
              ASSIGN w_ok = Cmb-NomIni:ADD-LAST(Tarjetas.Nombres) IN FRAME Frm-Nombres
                     w_ok = Cmb-NomFin:ADD-LAST(Tarjetas.Nombres) IN FRAME Frm-Nombres.
              IF FIRST(Tarjetas.Nombres) THEN
                 ASSIGN Cmb-NomIni:SCREEN-VALUE IN FRAME Frm-Nombres = Tarjetas.Nombres.
              IF LAST(Tarjetas.Nombres) THEN
                 ASSIGN Cmb-NomFin:SCREEN-VALUE IN FRAME Frm-Nombres = Tarjetas.Nombres.
           END.
           ASSIGN w_sw[R-Tipo] = TRUE.
           RUN _SetCurs.r ("No-wait").
       END. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
 {INCLUIDO\RepEncabezado.I}.  
/*  DEBUGGER:INITIATE().   */
/*  DEBUGGER:SET-BREAK().  */
 FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.
 DO WITH FRAME Frm-main:
     ASSIGN R-Tipo  W-FecIni  W-FecFin  S-EstadosIni  s-estadosFin.
 END.
 CASE R-Tipo:
     WHEN 1 THEN DO: 
         W_Reporte    = "REPORTE: Tarjetas/Clientes : Clas.x Cédula - " + STRING(W-FecIni,"99/99/9999") + 
                        " Hasta " + STRING(W-FecFin,"99/99/9999") + " - " + s-estadosIni + " Al " + S-EstadosFin +
                        " - Reporte al " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
         W_EncColumna = "Cédula          Tarjeta          Nombres                        Estado          HoraBl FecBloqueo Age  CtaAhorro     CtaCupo    Usuario".
         VIEW FRAME F-Encabezado.
         VIEW FRAME F-Ftr.
         FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.TarjetaDb :
           DISPLAY Tmp-Tarjetas.Nit            AT 1
                   Tmp-Tarjetas.TarjetaDb      AT 16  FORMAT "X(16)"
                   Tmp-Tarjetas.Nombres        AT 34  FORMAT "X(30)"
                   Tmp-Tarjetas.estado         AT 65 ":"
                   Tmp-Tarjetas.TdetEst        AT 70
                   Tmp-Tarjetas.Hora_Bloqueo   AT 81  FORMAT "zzzzzz"
                   Tmp-Tarjetas.Fec_Bloqueo    AT 88
                   Tmp-Tarjetas.Agencia        AT 99
                   Tmp-Tarjetas.Cue_Ahorros    AT 104
                   Tmp-Tarjetas.Num_Credito    AT 118 FORMAT "zzzzzzzzz"
                   Tmp-Tarjetas.Usuario        AT 129
                   WITH FRAME F-mov1 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 150 NO-LABELS.
         END.
     END.
     WHEN 2 THEN DO: 
         W_Reporte    = "REPORTE: Tarjetas/Clientes : Clas.x Tarjetas - " + STRING(W-FecIni,"99/99/9999") + 
                        " Hasta " + STRING(W-FecFin,"99/99/9999") + " - " + s-estadosIni + " Al " + S-EstadosFin +
                        " - Reporte al " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
         W_EncColumna = "Tarjeta          Nombres                        Estado          HoraBl FecBloqueo Age  Cédula          CtaAhorro     CtaCupo    Usuario".
         VIEW FRAME F-Encabezado.
         VIEW FRAME F-Ftr.
         FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.TarjetaDb :
           DISPLAY Tmp-Tarjetas.TarjetaDb      AT 1   FORMAT "X(16)"
                   Tmp-Tarjetas.Nombres        AT 18  FORMAT "X(30)"
                   Tmp-Tarjetas.estado         AT 49 ":"
                   Tmp-Tarjetas.TdetEst        AT 53
                   Tmp-Tarjetas.Hora_Bloqueo   AT 65  FORMAT "zzzzzz"
                   Tmp-Tarjetas.Fec_Bloqueo    AT 72  
                   Tmp-Tarjetas.Agencia        AT 83  
                   Tmp-Tarjetas.Nit            AT 88  
                   Tmp-Tarjetas.Cue_Ahorros    AT 104 
                   Tmp-Tarjetas.Num_Credito    AT 118 FORMAT "zzzzzzzzz"
                   Tmp-Tarjetas.Usuario        AT 129 
                   WITH FRAME F-mov2 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 150 NO-LABELS.
         END.
     END.
     WHEN 3 THEN DO: 
        W_Reporte    = "REPORTE: Tarjetas/Clientes : Clas.x Nombres - " + STRING(W-FecIni,"99/99/9999") + 
                       " Hasta " + STRING(W-FecFin,"99/99/9999") + " - " + s-estadosIni + " Al " + S-EstadosFin +
                       " - Reporte al " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
        W_EncColumna = "Nombres                        Tarjeta          Estado          HoraBl FecBloqueo Age  Cédula          CtaAhorro     CtaCupo    Usuario".
        VIEW FRAME F-Encabezado.
        VIEW FRAME F-Ftr.
        FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.Nombres :
          DISPLAY Tmp-Tarjetas.Nombres        AT 1  FORMAT "X(30)"
                  Tmp-Tarjetas.TarjetaDb      AT 32 FORMAT "X(16)"
                  Tmp-Tarjetas.estado         AT 49 FORMAT "X(2)" ":"
                  Tmp-Tarjetas.TdetEst        AT 53
                  Tmp-Tarjetas.Hora_Bloqueo   AT 64 FORMAT "zzzzzz"
                  Tmp-Tarjetas.Fec_Bloqueo    AT 72 FORMAT "99/99/9999"
                  Tmp-Tarjetas.Agencia        AT 83 
                  Tmp-Tarjetas.Nit            AT 88 
                  Tmp-Tarjetas.Cue_Ahorros    AT 104
                  Tmp-Tarjetas.Num_Credito    AT 118 FORMAT "zzzzzzzzz"
                  Tmp-Tarjetas.usuario        AT 129
                  WITH FRAME F-mov3 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 150 NO-LABELS.
        END.
     END.
     WHEN 4 THEN DO:
        W_Reporte    = "REPORTE: Tarjetas/Clientes : Clas.x Tarjetas - " + STRING(W-FecIni,"99/99/9999") + 
                       " Hasta " + STRING(W-FecFin,"99/99/9999") + " - " + s-estadosIni + " Al " + S-EstadosFin +
                       " - Reporte al " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
        W_EncColumna = "Tarjeta          Nombres                        Estado         HoraBl FecBloqueo Age  Cédula          CtaAhorro     CtaCupo    Usuario".
        VIEW FRAME F-Encabezado.
        VIEW FRAME F-Ftr.
        FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.TarjetaDb :
          DISPLAY Tmp-Tarjetas.TarjetaDb      AT 1   FORMAT "X(16)"
                  Tmp-Tarjetas.Nombres        AT 18  FORMAT "X(30)"
                  Tmp-Tarjetas.estado         AT 49 ":"
                  Tmp-Tarjetas.TdetEst        AT 53
                  Tmp-Tarjetas.Hora_Bloqueo   AT 64  FORMAT "zzzzzz"
                  Tmp-Tarjetas.Fec_Bloqueo    AT 71  
                  Tmp-Tarjetas.Agencia        AT 82  
                  Tmp-Tarjetas.Nit            AT 87  
                  Tmp-Tarjetas.Cue_Ahorros    AT 103 
                  Tmp-Tarjetas.Num_Credito    AT 117 FORMAT "zzzzzzzzz"
                  Tmp-Tarjetas.Usuario        AT 128  
                  WITH FRAME F-mov4 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 150 NO-LABELS.
        END.
     END.
     WHEN 5 THEN DO:
       DEF VAR W-SdoAho  AS DECIMAL INITIAL 0 NO-UNDO.
        W_Reporte    = "REPORTE: Tarjetas/Clientes : Clas.x Cta Ahorros - " + STRING(W-FecIni,"99/99/9999") + 
                       " Hasta " + STRING(W-FecFin,"99/99/9999") + " - " + s-estadosIni + " Al " + S-EstadosFin +
                       " - Reporte al " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
        W_EncColumna = "CtaAhorro      Tarjeta          Nombres                        Estado          HoraBl FecBloqueo Age  Cédula          CtaCupo     Usuario  SdoAhorro".
        VIEW FRAME F-Encabezado.
        VIEW FRAME F-Ftr.
        FIND FIRST ahorros WHERE ahorros.nit         EQ Tmp-Tarjetas.Nit        AND
                                 ahorros.cod_ahorro  EQ Cfg_tarjetadb.cod_ahorro AND
                                 ahorros.cue_ahorros EQ Tmp-Tarjetas.Cue_Ahorros NO-LOCK NO-ERROR.
        IF AVAILABLE(ahorros) THEN
             ASSIGN W-sdoAho = Ahorros.sdo_disponible + Ahorros.sdo_canje.
        ELSE ASSIGN W-SdoAho = 0.

        FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.Cue_ahorros :
          DISPLAY Tmp-Tarjetas.Cue_Ahorros    AT 1
                  Tmp-Tarjetas.TarjetaDb      AT 16  FORMAT "X(16)"
                  Tmp-Tarjetas.Nombres        AT 33  FORMAT "X(30)"
                  Tmp-Tarjetas.estado         AT 64 ":"
                  Tmp-Tarjetas.TdetEst        AT 68
                  Tmp-Tarjetas.Hora_Bloqueo   AT 80  FORMAT "zzzzzz"
                  Tmp-Tarjetas.Fec_Bloqueo    AT 87
                  Tmp-Tarjetas.Agencia        AT 98
                  Tmp-Tarjetas.Nit            AT 103
                  Tmp-Tarjetas.Num_Credito    AT 119 FORMAT "zzzzzzzzz"
                  Tmp-Tarjetas.Usuario        AT 131
                  W-SdoAho                    AT 140 FORMAT "-zzz,zzz,zz9"
                  WITH FRAME F-mov5 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 160 NO-LABELS.
        END.
     END.
     WHEN 6 THEN DO:
       DEF VAR w_sdocupo  AS DECIMAL INITIAL 0.
        W_Reporte    = "REPORTE: Tarjetas/Clientes : Clas.x Cupo Rotativo - " + STRING(W-FecIni,"99/99/9999") + 
                       " Hasta " + STRING(W-FecFin,"99/99/9999") + " - " + s-estadosIni + " Al " + S-EstadosFin +
                       " - Reporte al " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
        W_EncColumna = "CtaCupo    Tarjeta          Nombres                        Estado         HoraBl FecBloqueo Age  Cédula          CtaAhorro Usuario Sdo_Cupo".
        VIEW FRAME F-Encabezado.
        VIEW FRAME F-Ftr.
        FOR EACH Tmp-Tarjetas NO-LOCK BY Tmp-Tarjetas.Num_credito :
            FIND FIRST Creditos   WHERE Creditos.nit          EQ Tmp-Tarjetas.Nit           AND 
                                        Creditos.cod_credito  EQ Cfg_tarjetadb.cod_credito  AND
                                        Creditos.num_credito  EQ Tmp-Tarjetas.Num_Credito   NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(Creditos) THEN
            FIND FIRST Creditos   WHERE Creditos.nit          EQ Tmp-Tarjetas.Nit           AND 
                                        Creditos.cod_credito  EQ Cfg_tarjetadb.cod_credito2 AND
                                        Creditos.num_credito  EQ Tmp-Tarjetas.Num_Credito   NO-LOCK NO-ERROR.
            IF AVAILABLE(Creditos) THEN
                W_SdoCupo  = ROUND(Creditos.Val_Desembolso - Creditos.Sdo_capital, 0).
            ELSE W_SdoCupo = 0.
            DISPLAY Tmp-Tarjetas.Num_Credito    AT 1
                    Tmp-Tarjetas.TarjetaDb      AT 12  FORMAT "X(16)"
                    Tmp-Tarjetas.Nombres        AT 29  FORMAT "X(30)"
                    Tmp-Tarjetas.estado         AT 60 ":"
                    Tmp-Tarjetas.TdetEst        AT 64
                    Tmp-Tarjetas.Hora_Bloqueo   AT 75  FORMAT "zzzzzz"
                    Tmp-Tarjetas.Fec_Bloqueo    AT 82  
                    Tmp-Tarjetas.Agencia        AT 93  
                    Tmp-Tarjetas.Nit            AT 98  
                    Tmp-Tarjetas.Cue_Ahorros    AT 113 FORMAT "X(11)"
                    Tmp-Tarjetas.Usuario        AT 124 
                    W_SdoCupo                   AT 135 FORMAT "-zzz,zzz,zz9"
                  WITH FRAME F-mov6 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 160 NO-LABELS.
        END.
     END.
     WHEN 7 THEN DO:
        W_Reporte    = "REPORTE: Tarjetas/Clientes : Sin Tarjeta - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
        W_EncColumna = "Cta-Ahorro     Age  Cédula            Nombres                          Sdo.Disponible        Sdo.Canje       Sdo.Total".
        VIEW FRAME F-Encabezado.
        VIEW FRAME F-Ftr.
        FOR EACH Tmp-Sintar NO-LOCK  :
          DISPLAY Tmp-Sintar.TCue_Ahorros   AT 1
                  Tmp-Sintar.TAgencia       AT 16  
                  Tmp-Sintar.TNit           AT 21 
                  Tmp-Sintar.Tnombres       AT 39  FORMAT "X(30)"
                  Tmp-Sintar.TSdo_dispon    AT 71  FORMAT "->>>,>>>,>>9.99"
                  Tmp-Sintar.TSdo_Canje     AT 88  FORMAT "->>>,>>>,>>9.99"
                  Tmp-sintar.TSdo_Total     AT 104 FORMAT "->>>,>>>,>>9.99"
                  WITH FRAME F-mov7 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
        END.
     END.
 END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

