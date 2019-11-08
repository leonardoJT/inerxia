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
DEFINE VARIABLE w_ok   AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE Tmp-Transa LIKE Mov_Tarjetas
    FIELD Tnombres   AS CHARACTER FORMAT "X(80)"
    FIELD Tdetalle   AS CHARACTER FORMAT "X(40)"
    FIELD TdetError  LIKE Cod_Tarjetas.Descripcion
    INDEX TtNom IS PRIMARY TNombres         ASCENDING
    INDEX TttarjDb         TarjetaDb        ASCENDING
    INDEX Tfectra          Fec_Transac      ASCENDING
    INDEX THorTra          Hora_Transac     ASCENDING
    INDEX Tlugar           lugar            ASCENDING
    INDEX Ttipoper         TipoTransaccion  ASCENDING
    INDEX Tmonto           Monto            ASCENDING.

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
&Scoped-define INTERNAL-TABLES Tmp-Transa Mov_Tarjetas

/* Definitions for BROWSE Brw-Consulta                                  */
&Scoped-define FIELDS-IN-QUERY-Brw-Consulta Tmp-Transa.NroTerminal + Tmp-Transa.naud Tmp-Transa.Tnombre Tmp-Transa.TarjetaDB Tmp-Transa.Num_Cuenta Tmp-Transa.TipoRed Tmp-Transa.NroTerminal Tmp-Transa.lugar Tmp-Transa.Hora_Transac Tmp-Transa.Fec_Transac Tmp-Transa.Monto Tmp-Transa.Comision Tmp-Transa.ComAdicional Tmp-Transa.OtrosCargos Tmp-Transa.TipoTransac Tmp-Transa.Tdetalle Tmp-Transa.TdetError   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw-Consulta   
&Scoped-define SELF-NAME Brw-Consulta
&Scoped-define QUERY-STRING-Brw-Consulta FOR EACH Tmp-Transa
&Scoped-define OPEN-QUERY-Brw-Consulta OPEN QUERY {&SELF-NAME} FOR EACH Tmp-Transa.
&Scoped-define TABLES-IN-QUERY-Brw-Consulta Tmp-Transa
&Scoped-define FIRST-TABLE-IN-QUERY-Brw-Consulta Tmp-Transa


/* Definitions for FRAME Frm-Main                                       */
&Scoped-define FIELDS-IN-QUERY-Frm-Main Mov_Tarjetas.TarjetaDB ~
Mov_Tarjetas.Nit 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Frm-Main Mov_Tarjetas.TarjetaDB ~
Mov_Tarjetas.Nit 
&Scoped-define ENABLED-TABLES-IN-QUERY-Frm-Main Mov_Tarjetas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Frm-Main Mov_Tarjetas
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frm-Main ~
    ~{&OPEN-QUERY-Brw-Consulta}
&Scoped-define QUERY-STRING-Frm-Main FOR EACH Mov_Tarjetas SHARE-LOCK
&Scoped-define OPEN-QUERY-Frm-Main OPEN QUERY Frm-Main FOR EACH Mov_Tarjetas SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Frm-Main Mov_Tarjetas
&Scoped-define FIRST-TABLE-IN-QUERY-Frm-Main Mov_Tarjetas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Mov_Tarjetas.TarjetaDB Mov_Tarjetas.Nit 
&Scoped-define ENABLED-TABLES Mov_Tarjetas
&Scoped-define FIRST-ENABLED-TABLE Mov_Tarjetas
&Scoped-Define ENABLED-OBJECTS RECT-338 RECT-339 RECT-340 RECT-344 R-tipo2 ~
R-tipo W-FecIni Btn-Visualizar W-FecFin Btn-Procesar Btn_Done R-orden ~
Brw-Consulta 
&Scoped-Define DISPLAYED-FIELDS Mov_Tarjetas.TarjetaDB Mov_Tarjetas.Nit 
&Scoped-define DISPLAYED-TABLES Mov_Tarjetas
&Scoped-define FIRST-DISPLAYED-TABLE Mov_Tarjetas
&Scoped-Define DISPLAYED-OBJECTS R-tipo2 R-tipo W-FecIni W-FecFin R-orden ~
w_totMonto w_totComision w_totComAdi w_totOtrosC w_totTransacc 

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
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn-Visualizar 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE VARIABLE W-FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W-FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE w_totComAdi AS DECIMAL FORMAT "->>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.57 BY .81
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE w_totComision AS DECIMAL FORMAT "->>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.29 BY .81
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE w_totMonto AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE w_totOtrosC AS DECIMAL FORMAT "->>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.57 BY .81
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE w_totTransacc AS DECIMAL FORMAT "->>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE R-orden AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nombres", 1,
"Fecha", 2,
"Lugar", 3,
"Monto", 4,
"Hora", 5,
"Operacion ", 6,
"Nro Tarjeta", 7
     SIZE 86.43 BY .81
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE R-tipo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Una Cédula    ", 1,
"Una Tarjeta   ", 2,
"Nombre Cliente", 3,
"Número de Tarjeta", 4
     SIZE 23.72 BY 2.42 NO-UNDO.

DEFINE VARIABLE R-tipo2 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "General", 1,
"Tipo Ahorros", 2,
"Tipo Rotativo", 3,
"Cobro de Tarjetas", 4,
"Cobro de Cuotas", 5
     SIZE 22.72 BY 2.96 NO-UNDO.

DEFINE RECTANGLE RECT-338
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.72 BY 2.96
     FGCOLOR 8 .

DEFINE RECTANGLE RECT-339
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 2.65
     FGCOLOR 8 .

DEFINE RECTANGLE RECT-340
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24.72 BY 3.5.

DEFINE RECTANGLE RECT-344
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 1.35.

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
      Tmp-Transa SCROLLING.

DEFINE QUERY Frm-Main FOR 
      Mov_Tarjetas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw-Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw-Consulta C-Win _FREEFORM
  QUERY Brw-Consulta DISPLAY
      Tmp-Transa.NroTerminal + Tmp-Transa.naud
                               COLUMN-LABEL "IdentOperación" FORMAT "X(16)"
     Tmp-Transa.Tnombre        COLUMN-LABEL "Cliente"        FORMAT "X(35)"
     Tmp-Transa.TarjetaDB      COLUMN-LABEL "Tarjeta"        FORMAT "X(20)"
     Tmp-Transa.Num_Cuenta     COLUMN-LABEL "Cuenta"
     Tmp-Transa.TipoRed        COLUMN-LABEL "Red"
     Tmp-Transa.NroTerminal    COLUMN-LABEL "Terminal"       FORMAT "X(12)"
     Tmp-Transa.lugar          COLUMN-LABEL "Lugar"
     Tmp-Transa.Hora_Transac   COLUMN-LABEL "Hora"
     Tmp-Transa.Fec_Transac    COLUMN-LABEL "Fecha"
     Tmp-Transa.Monto          COLUMN-LABEL "Monto"
     Tmp-Transa.Comision       COLUMN-LABEL "Comisión"
     Tmp-Transa.ComAdicional   COLUMN-LABEL "Comisión Adic"
     Tmp-Transa.OtrosCargos    COLUMN-LABEL "Cuota/CobroPlas"
     Tmp-Transa.TipoTransac    COLUMN-LABEL "Operación"
     Tmp-Transa.Tdetalle       COLUMN-LABEL "Nombre Oper"
     Tmp-Transa.TdetError      COLUMN-LABEL "Detalle_Error"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 10.23
         BGCOLOR 15 FGCOLOR 0  ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm-Main
     R-tipo2 AT ROW 1.88 COL 56.29 NO-LABEL WIDGET-ID 168
     R-tipo AT ROW 2.08 COL 3.72 NO-LABEL WIDGET-ID 56
     W-FecIni AT ROW 2.35 COL 37.72 COLON-ALIGNED WIDGET-ID 64
     Btn-Visualizar AT ROW 2.46 COL 81.72 WIDGET-ID 166
     W-FecFin AT ROW 3.35 COL 37.72 COLON-ALIGNED WIDGET-ID 66
     Btn-Procesar AT ROW 4.46 COL 81.72 WIDGET-ID 86
     Mov_Tarjetas.TarjetaDB AT ROW 5.12 COL 32.43 COLON-ALIGNED WIDGET-ID 202
          VIEW-AS FILL-IN 
          SIZE 20 BY .81
     Mov_Tarjetas.Nit AT ROW 5.15 COL 3.72 COLON-ALIGNED WIDGET-ID 200
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
     Btn_Done AT ROW 6.42 COL 81.86 WIDGET-ID 20
     R-orden AT ROW 10.5 COL 2.57 NO-LABEL WIDGET-ID 176
     Brw-Consulta AT ROW 11.88 COL 2 WIDGET-ID 200
     w_totMonto AT ROW 22.96 COL 20 RIGHT-ALIGNED NO-LABEL WIDGET-ID 188
     w_totComision AT ROW 22.96 COL 37.29 RIGHT-ALIGNED NO-LABEL WIDGET-ID 190
     w_totComAdi AT ROW 22.96 COL 55 RIGHT-ALIGNED NO-LABEL WIDGET-ID 192
     w_totOtrosC AT ROW 22.96 COL 72.43 RIGHT-ALIGNED NO-LABEL WIDGET-ID 198
     w_totTransacc AT ROW 22.96 COL 88.57 RIGHT-ALIGNED NO-LABEL WIDGET-ID 196
     "Cuota/Cobro Plas  Total Transacc" VIEW-AS TEXT
          SIZE 32 BY .54 AT ROW 22.38 COL 57 WIDGET-ID 186
     "Total Monto                 Comisión                Com.Adicional" VIEW-AS TEXT
          SIZE 54 BY .54 AT ROW 22.38 COL 2 WIDGET-ID 184
     "Tipo Cuenta" VIEW-AS TEXT
          SIZE 12.72 BY .62 AT ROW 1.19 COL 56.29 WIDGET-ID 74
     "[dd/mm/aaaa]" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 1.35 COL 41 WIDGET-ID 70
          FGCOLOR 12 
     " Consulta por:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 1.27 COL 3.57 WIDGET-ID 62
     "Ordenado por:" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 9.96 COL 3 WIDGET-ID 174
     " Fechas:" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 1.35 COL 32 WIDGET-ID 68
          FGCOLOR 12 
     RECT-338 AT ROW 1.81 COL 2.29 WIDGET-ID 60
     RECT-339 AT ROW 1.85 COL 30.72 WIDGET-ID 72
     RECT-340 AT ROW 1.54 COL 55.57 WIDGET-ID 84
     RECT-344 AT ROW 10.23 COL 2 WIDGET-ID 172
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.29 BY 23 WIDGET-ID 100.

DEFINE FRAME Frm-Tarjeta
     Cmb-TarIni AT ROW 1.88 COL 8 COLON-ALIGNED WIDGET-ID 6
     Cmb-TarFin AT ROW 2.96 COL 8 COLON-ALIGNED WIDGET-ID 8
     " Tarjeta:" VIEW-AS TEXT
          SIZE 8.43 BY .81 AT ROW 1 COL 2.57 WIDGET-ID 4
     RECT-342 AT ROW 1.62 COL 1.29 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 6.5
         SIZE 79 BY 3.23
         BGCOLOR 8  WIDGET-ID 400.

DEFINE FRAME Frm-Nombres
     Cmb-NomIni AT ROW 1.77 COL 8 COLON-ALIGNED WIDGET-ID 6
     Cmb-NomFin AT ROW 2.81 COL 8 COLON-ALIGNED WIDGET-ID 8
     " Clientes:" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 1.19 COL 2.57 WIDGET-ID 2
     RECT-341 AT ROW 1.46 COL 1.29 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 2 ROW 6.5
         SIZE 79 BY 3.23
         BGCOLOR 8  WIDGET-ID 300.


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
         TITLE              = "Listado de Operaciones - W-Imp_Operaciones.w"
         HEIGHT             = 22.92
         WIDTH              = 89.29
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
ASSIGN FRAME Frm-Nombres:FRAME = FRAME Frm-Main:HANDLE
       FRAME Frm-Tarjeta:FRAME = FRAME Frm-Main:HANDLE.

/* SETTINGS FOR FRAME Frm-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB Brw-Consulta R-orden Frm-Main */
ASSIGN 
       Brw-Consulta:SEPARATOR-FGCOLOR IN FRAME Frm-Main      = 8.

/* SETTINGS FOR BUTTON Btn-Visualizar IN FRAME Frm-Main
   6                                                                    */
/* SETTINGS FOR FILL-IN w_totComAdi IN FRAME Frm-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       w_totComAdi:READ-ONLY IN FRAME Frm-Main        = TRUE.

/* SETTINGS FOR FILL-IN w_totComision IN FRAME Frm-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       w_totComision:READ-ONLY IN FRAME Frm-Main        = TRUE.

/* SETTINGS FOR FILL-IN w_totMonto IN FRAME Frm-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       w_totMonto:READ-ONLY IN FRAME Frm-Main        = TRUE.

/* SETTINGS FOR FILL-IN w_totOtrosC IN FRAME Frm-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       w_totOtrosC:READ-ONLY IN FRAME Frm-Main        = TRUE.

/* SETTINGS FOR FILL-IN w_totTransacc IN FRAME Frm-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       w_totTransacc:READ-ONLY IN FRAME Frm-Main        = TRUE.

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
OPEN QUERY {&SELF-NAME} FOR EACH Tmp-Transa.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Brw-Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frm-Main
/* Query rebuild information for FRAME Frm-Main
     _TblList          = "bdcentral.Mov_Tarjetas"
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
ON END-ERROR OF C-Win /* Listado de Operaciones - W-Imp_Operaciones.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Listado de Operaciones - W-Imp_Operaciones.w */
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
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
DO:
  ASSIGN R-Tipo R-Tipo2 R-Orden W-FecIni  W-FecFin
         W_TotMonto = 0   W_TotComision = 0   W_TotComAdi = 0   W_TotTransacc = 0  W_TotOtrosC = 0.
  EMPTY TEMP-TABLE tmp-Transa.
  CASE R-Tipo:
      WHEN 1 THEN DO: /* Cédula   */
        FOR EACH Tarjetas WHERE Tarjetas.Nit EQ Mov_Tarjetas.Nit:SCREEN-VALUE IN FRAME Frm-Main 
                                  NO-LOCK BY Tarjetas.Nit :
           RUN ConsxCedula.
        END.
      END.
      WHEN 2 THEN DO: /* Tarjeta  */
        FOR EACH Tarjetas WHERE Tarjetas.TarjetaDb EQ Mov_Tarjetas.TarjetaDb:SCREEN-VALUE IN FRAME Frm-Main 
                                  NO-LOCK BY Tarjetas.TarjetaDb:
           RUN ConsxCedula.
        END.
      END.
      WHEN 3 THEN DO: /* Nombre de Clientes */
         FOR EACH Tarjetas WHERE Tarjetas.Nombres GE Cmb-NomIni:SCREEN-VALUE IN FRAME frm-Nombres AND 
                                 Tarjetas.Nombres LE Cmb-NomFin:SCREEN-VALUE IN FRAME frm-Nombres NO-LOCK BY Tarjetas.Nombres :
             CASE R-tipo2:
                 WHEN 1 THEN DO:
                    FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                                 Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                                 Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                                 Mov_Tarjetas.tipoTransa  NE 0 NO-LOCK:
                       CREATE Tmp-Transa.
                       BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
                       RUN DetalleOper.
                       IF Mov_Tarjetas.CodError NE 0 THEN
                         RUN DetalleError.
                       ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                              W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                              W_TotComision       = W_TotComision  + Tmp-transa.Comision
                              W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                              W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                              W_TotTransacc       = W_TotTransacc  + 1.
                    END.
                 END.
                 WHEN 2 THEN DO:
                     FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                                  Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                                  Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                                  Mov_Tarjetas.TipoCuenta  EQ 10       AND 
                                                  Mov_Tarjetas.tipoTransa  NE 0 NO-LOCK:
                        CREATE Tmp-Transa.
                        BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
                        RUN DetalleOper.
                        IF Mov_Tarjetas.CodError NE 0 THEN
                          RUN DetalleError.
                        ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                               W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                               W_TotComision       = W_TotComision  + Tmp-transa.Comision
                               W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                               W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                               W_TotTransacc       = W_TotTransacc  + 1.
                     END.
                 END.
                 WHEN 3 THEN DO:
                     FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                                  Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                                  Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                                  Mov_Tarjetas.TipoCuenta  EQ 20       AND 
                                                  Mov_Tarjetas.tipoTransa  NE 0 NO-LOCK:
                        CREATE Tmp-Transa.
                        BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
                        RUN DetalleOper.
                        IF Mov_Tarjetas.CodError NE 0 THEN
                          RUN DetalleError.
                        ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                               W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                               W_TotComision       = W_TotComision  + Tmp-transa.Comision
                               W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                               W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                               W_TotTransacc       = W_TotTransacc  + 1.
                     END.
                 END.
                 WHEN 4 THEN DO: /* Cobro de Tarjetas */
                     FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                                  Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                                  Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                                  Mov_Tarjetas.tipoTransa  EQ 0 NO-LOCK:
                        IF SUBSTRING(Mov_Tarjetas.Lugar,1,15) EQ "Cuota de Manejo" THEN NEXT.
                        CREATE Tmp-Transa.
                        BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
                        RUN DetalleOper.
                        IF Mov_Tarjetas.CodError NE 0 THEN
                          RUN DetalleError.
                        ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                               W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                               W_TotComision       = W_TotComision  + Tmp-transa.Comision
                               W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                               W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                               W_TotTransacc       = W_TotTransacc  + 1.
                     END.
                 END.
                 WHEN 5 THEN DO: /* Cobro de Cuotas */
                     FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                                  Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                                  Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                                  Mov_Tarjetas.tipoTransa  EQ 0 NO-LOCK:
                        IF SUBSTRING(Mov_Tarjetas.Lugar,1,15) NE "Cuota de Manejo" THEN NEXT.
                        CREATE Tmp-Transa.
                        BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
                        RUN DetalleOper.
                        IF Mov_Tarjetas.CodError NE 0 THEN
                          RUN DetalleError.
                        ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                               W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                               W_TotComision       = W_TotComision  + Tmp-transa.Comision
                               W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                               W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                               W_TotTransacc       = W_TotTransacc  + 1.
                     END.
                 END.
             END CASE.
         END.
      END.
      WHEN 4 THEN DO: /* Número de Tarjetas */
         FOR EACH Tarjetas WHERE Tarjetas.TarjetaDB GE Cmb-TarIni:SCREEN-VALUE IN FRAME Frm-Tarjeta AND 
                                 Tarjetas.TarjetaDb LE Cmb-TarFin:SCREEN-VALUE IN FRAME Frm-Tarjeta NO-LOCK BY Tarjetas.TarjetaDB :
            CASE R-tipo2:
                WHEN 1 THEN DO:
                   FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                                Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                                Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                                Mov_Tarjetas.tipoTransa  NE 0 NO-LOCK:
                      CREATE Tmp-Transa.
                      BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
                      RUN DetalleOper.
                      IF Mov_Tarjetas.CodError NE 0 THEN
                        RUN DetalleError.
                      ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                             W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                             W_TotComision       = W_TotComision  + Tmp-transa.Comision
                             W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                             W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                             W_TotTransacc       = W_TotTransacc  + 1.
                   END.
                END.
                WHEN 2 THEN DO:
                   FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                                Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                                Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                                Mov_Tarjetas.Tipocuenta  EQ 10       AND 
                                                Mov_Tarjetas.tipoTransa  NE 0 NO-LOCK:
                      CREATE Tmp-Transa.
                      BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
                      RUN DetalleOper.
                      IF Mov_Tarjetas.CodError NE 0 THEN
                        RUN DetalleError.
                      ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                             W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                             W_TotComision       = W_TotComision  + Tmp-transa.Comision
                             W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                             W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                             W_TotTransacc       = W_TotTransacc  + 1.
                   END.
                END.
                WHEN 3 THEN DO:
                   FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                                Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                                Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                                Mov_Tarjetas.Tipocuenta  EQ 20       AND 
                                                Mov_Tarjetas.tipoTransa  NE 0 NO-LOCK:
                      CREATE Tmp-Transa.
                      BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
                      RUN DetalleOper.
                      IF Mov_Tarjetas.CodError NE 0 THEN
                        RUN DetalleError.
                      ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                             W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                             W_TotComision       = W_TotComision  + Tmp-transa.Comision
                             W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                             W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                             W_TotTransacc       = W_TotTransacc  + 1.
                   END.
                END.
                WHEN 4 THEN DO: /* Cobro de Tarjeta */
                   FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                                Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                                Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                                Mov_Tarjetas.tipoTransa  EQ 0 NO-LOCK:
                      IF SUBSTRING(Mov_Tarjetas.Lugar,1,15) EQ "Cuota de Manejo" THEN NEXT.
                      CREATE Tmp-Transa.
                      BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
                      RUN DetalleOper.
                      IF Mov_Tarjetas.CodError NE 0 THEN
                        RUN DetalleError.
                      ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                             W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                             W_TotComision       = W_TotComision  + Tmp-transa.Comision
                             W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                             W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                             W_TotTransacc       = W_TotTransacc  + 1.
                   END.
                END.
                WHEN 5 THEN DO:  /* Cuota de Manejo */
                   FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                                Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                                Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                                Mov_Tarjetas.tipoTransa  EQ 0 NO-LOCK:
                      IF SUBSTRING(Mov_Tarjetas.Lugar,1,15) NE "Cuota de Manejo" THEN NEXT.
                      CREATE Tmp-Transa.
                      BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
                      RUN DetalleOper.
                      IF Mov_Tarjetas.CodError NE 0 THEN
                        RUN DetalleError.
                      ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                             W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                             W_TotComision       = W_TotComision  + Tmp-transa.Comision
                             W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                             W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                             W_TotTransacc       = W_TotTransacc  + 1.
                   END.
                END.
            END CASE.
          END.
      END.
  END CASE.
  ASSIGN W_TotMonto:SCREEN-VALUE    = STRING(W_TotMonto,"-zzz,zzz,zzz,zz9") 
         W_TotComision:SCREEN-VALUE = STRING(W_TotComision,"-zzz,zzz,zzz,zz9")
         W_TotComAdi:SCREEN-VALUE   = STRING(W_TotComAdi,"-zzz,zzz,zzz,zz9")
         W_TotOtrosC:SCREEN-VALUE   = STRING(W_TotOtrosC,"-zzz,zzz,zzz,zz9")
         W_TotTransacc:SCREEN-VALUE = STRING(W_TotTransacc,"-zzz,zzz,zzz,zz9").
  APPLY "value-changed" TO R-Orden.
  APPLY "CHOOSE" TO btn-Visualizar.
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
      WHEN 1 THEN OPEN QUERY Brw-Consulta FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.Tnombres         INDEXED-REPOSITION.
      WHEN 2 THEN OPEN QUERY Brw-Consulta FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.Fec_Transac      INDEXED-REPOSITION.
      WHEN 3 THEN OPEN QUERY Brw-Consulta FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.Lugar            INDEXED-REPOSITION.
      WHEN 4 THEN OPEN QUERY Brw-Consulta FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.Monto            INDEXED-REPOSITION.
      WHEN 5 THEN OPEN QUERY Brw-Consulta FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.Hora_transac     INDEXED-REPOSITION.
      WHEN 6 THEN OPEN QUERY Brw-Consulta FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.TipoTransaccion  INDEXED-REPOSITION.
      WHEN 7 THEN OPEN QUERY Brw-Consulta FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.TarjetaDB        INDEXED-REPOSITION.
  END CASE.
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
          ASSIGN FRAME Frm-Nombres:VISIBLE      = FALSE
                 FRAME Frm-Tarjeta:VISIBLE      = FALSE
                 Mov_Tarjetas.TarjetaDb:VISIBLE = FALSE
                 Mov_Tarjetas.Nit:VISIBLE       = TRUE.
      END.
      WHEN 2 THEN DO:
          ASSIGN FRAME Frm-Nombres:VISIBLE   = FALSE
                 FRAME Frm-Tarjeta:VISIBLE   = FALSE
                 Mov_Tarjetas.TarjetaDb:VISIBLE = TRUE
                 Mov_Tarjetas.Nit:VISIBLE       = FALSE.
      END.
      WHEN 3 THEN DO: 
          ASSIGN Mov_Tarjetas.TarjetaDb:VISIBLE = FALSE
                 Mov_Tarjetas.Nit:VISIBLE       = FALSE
                 FRAME Frm-Nombres:VISIBLE      = TRUE 
                 FRAME Frm-Tarjeta:VISIBLE      = FALSE.
          ASSIGN Cmb-NomIni:LIST-ITEMS = "" Cmb-NomFin:LIST-ITEMS = "".
          FOR EACH Tarjetas WHERE Tarjetas.Nombres NE " " BREAK BY Tarjetas.Nombres:
             ASSIGN w_ok = Cmb-NomIni:ADD-LAST(Tarjetas.Nombres) IN FRAME Frm-Nombres
                    w_ok = Cmb-NomFin:ADD-LAST(Tarjetas.Nombres) IN FRAME Frm-Nombres.
             IF FIRST(Tarjetas.Nombres) THEN
                ASSIGN Cmb-NomIni:SCREEN-VALUE IN FRAME Frm-Nombres = Tarjetas.Nombres.
             IF LAST(Tarjetas.Nombres) THEN
                ASSIGN Cmb-NomFin:SCREEN-VALUE IN FRAME Frm-Nombres = Tarjetas.Nombres.
          END.

      END.
      WHEN 4 THEN DO:
          ASSIGN Mov_Tarjetas.TarjetaDb:VISIBLE = FALSE
                 Mov_Tarjetas.Nit:VISIBLE       = FALSE
                 FRAME Frm-Nombres:VISIBLE      = FALSE    
                 FRAME Frm-Tarjeta:VISIBLE      = TRUE.
          ASSIGN Cmb-TarIni:LIST-ITEMS = "" Cmb-TarFin:LIST-ITEMS = "".
          FOR EACH Tarjetas WHERE Tarjetas.TarjetaDb NE "" NO-LOCK BREAK BY Tarjetas.TarjetaDB :
              ASSIGN w_ok = Cmb-TarIni:ADD-LAST(Tarjetas.TarjetaDB) IN FRAME Frm-Tarjeta
                     w_ok = Cmb-TarFin:ADD-LAST(Tarjetas.TarjetaDB) IN FRAME Frm-Tarjeta.
              IF FIRST(Tarjetas.TarjetaDB) THEN
                 ASSIGN Cmb-TarIni:SCREEN-VALUE IN FRAME Frm-Tarjeta = Tarjetas.TarjetaDb.
              IF LAST(Tarjetas.TarjetaDB) THEN
                 ASSIGN Cmb-TarFin:SCREEN-VALUE IN FRAME Frm-Tarjeta = Tarjetas.TarjetaDb.
          END.
      END.
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
  ASSIGN FRAME Frm-Nombres:VISIBLE   = TRUE   
         FRAME Frm-Tarjeta:VISIBLE   = FALSE. 
  RUN inicializar_variables.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConsxCedula C-Win 
PROCEDURE ConsxCedula :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CASE R-tipo2:
    WHEN 1 THEN DO:
       FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                    Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                    Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                    Mov_Tarjetas.tipoTransa  NE 0 NO-LOCK:
          CREATE Tmp-Transa.
          BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
          RUN DetalleOper.
          IF Mov_Tarjetas.CodError NE 0 THEN
            RUN DetalleError.
          ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                 W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                 W_TotComision       = W_TotComision  + Tmp-transa.Comision
                 W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                 W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                 W_TotTransacc       = W_TotTransacc  + 1.
       END.
    END.
    WHEN 2 THEN DO:
        FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                     Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                     Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                     Mov_Tarjetas.TipoCuenta  EQ 10       AND 
                                     Mov_Tarjetas.tipoTransa  NE 0 NO-LOCK:
           CREATE Tmp-Transa.
           BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
           RUN DetalleOper.
           IF Mov_Tarjetas.CodError NE 0 THEN
             RUN DetalleError.
           ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                  W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                  W_TotComision       = W_TotComision  + Tmp-transa.Comision
                  W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                  W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                  W_TotTransacc       = W_TotTransacc  + 1.
        END.
    END.
    WHEN 3 THEN DO:
        FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                     Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                     Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                     Mov_Tarjetas.TipoCuenta  EQ 20       AND 
                                     Mov_Tarjetas.tipoTransa  NE 0 NO-LOCK:
           CREATE Tmp-Transa.
           BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
           RUN DetalleOper.
           IF Mov_Tarjetas.CodError NE 0 THEN
             RUN DetalleError.
           ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                  W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                  W_TotComision       = W_TotComision  + Tmp-transa.Comision
                  W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                  W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                  W_TotTransacc       = W_TotTransacc  + 1.
        END.
    END.
    WHEN 4 THEN DO: /* Cobro de Tarjetas */
        FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                     Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                     Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                     Mov_Tarjetas.tipoTransa  EQ 0 NO-LOCK:
           IF SUBSTRING(Mov_Tarjetas.Lugar,1,15) EQ "Cuota de Manejo" THEN NEXT.
           CREATE Tmp-Transa.
           BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
           RUN DetalleOper.
           IF Mov_Tarjetas.CodError NE 0 THEN
             RUN DetalleError.
           ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                  W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                  W_TotComision       = W_TotComision  + Tmp-transa.Comision
                  W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                  W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                  W_TotTransacc       = W_TotTransacc  + 1.
        END.
    END.
    WHEN 5 THEN DO: /* Cobro de Cuotas */
        FOR EACH Mov_Tarjetas  WHERE Mov_Tarjetas.TarjetaDb   EQ Tarjetas.TarjetaDb AND 
                                     Mov_Tarjetas.Fec_Transac GE w-FecIni AND 
                                     Mov_Tarjetas.Fec_Transac LE w-FecFin AND 
                                     Mov_Tarjetas.tipoTransa  EQ 0 NO-LOCK:
           IF SUBSTRING(Mov_Tarjetas.Lugar,1,15) NE "Cuota de Manejo" THEN NEXT.
           CREATE Tmp-Transa.
           BUFFER-COPY Mov_Tarjetas TO Tmp-Transa.
           RUN DetalleOper.
           IF Mov_Tarjetas.CodError NE 0 THEN
             RUN DetalleError.
           ASSIGN Tmp-Transa.Tnombres = Tarjetas.Nombres
                  W_TotMonto          = W_TotMonto     + Tmp-transa.Monto
                  W_TotComision       = W_TotComision  + Tmp-transa.Comision
                  W_TotComAdi         = W_TotComAdi    + Tmp-Transa.ComAdicional
                  W_TotOtrosC         = W_TotOtrosC    + Tmp-Transa.OtrosCargos
                  W_TotTransacc       = W_TotTransacc  + 1.
        END.
    END.
END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DetalleError C-Win 
PROCEDURE DetalleError :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR W-Codbusq  AS CHAR FORMAT "X(3)".
     ASSIGN W-Codbusq = STRING(Mov_Tarjetas.TipoRed,"9") + STRING(Mov_Tarjetas.CodError,"99").
     FIND FIRST Cod_Tarjetas WHERE Cod_Tarjetas.TipoCodigo EQ STRING(Mov_Tarjetas.TipoTransa,"9") AND
                                   Cod_Tarjetas.Codigo     EQ W-CodBusq NO-LOCK NO-ERROR.
     IF AVAILABLE Cod_Tarjetas THEN  
        ASSIGN Tmp-Transa.TdetError = Cod_Tarjetas.TipoCodigo + W-CodBusq + " - " + Cod_Tarjetas.Descripcion.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DetalleOper C-Win 
PROCEDURE DetalleOper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFI VAR w_detalle   AS CHARACTER FORMAT "X(80)" INITIAL "".    
     CASE Mov_Tarjetas.TipoTransaccion:
         WHEN 0 THEN ASSIGN w_detalle = "Cobro Cuota/Plástico      ".
         WHEN 1 THEN ASSIGN w_detalle = "Consulta Cajero           ".
         WHEN 2 THEN ASSIGN w_detalle = "Retiro Cajero             ".
         WHEN 3 THEN ASSIGN w_detalle = "Pago en POS               ".
         WHEN 4 THEN ASSIGN w_detalle = "Declinada Cajero          ".
         WHEN 5 THEN ASSIGN w_detalle = "Reversa Credito           ".
         WHEN 6 THEN ASSIGN w_detalle = "Reversa Débito            ".
         WHEN 7 THEN ASSIGN w_detalle = "Transacc.Datáf Cooperativa".
         WHEN 8 THEN ASSIGN w_detalle = "Consignaciones MegaExpress".
         WHEN 9 THEN ASSIGN w_detalle = "Pago Servicios Públicos   ".
         OTHERWISE   ASSIGN w_detalle = "No Identificada           ".
     END CASE.
     FIND FIRST redes    WHERE Redes.Red    = Mov_Tarjetas.TipoRed  NO-LOCK NO-ERROR.
     IF AVAILABLE redes THEN  w_detalle = TRIM(w_detalle) + " - " + TRIM(Redes.nombre).
     ASSIGN Tmp-Transa.Tdetalle = w_detalle.
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
  DISPLAY R-tipo2 R-tipo W-FecIni W-FecFin R-orden w_totMonto w_totComision 
          w_totComAdi w_totOtrosC w_totTransacc 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  IF AVAILABLE Mov_Tarjetas THEN 
    DISPLAY Mov_Tarjetas.TarjetaDB Mov_Tarjetas.Nit 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  ENABLE RECT-338 RECT-339 RECT-340 RECT-344 R-tipo2 R-tipo W-FecIni 
         Btn-Visualizar W-FecFin Btn-Procesar Mov_Tarjetas.TarjetaDB 
         Mov_Tarjetas.Nit Btn_Done R-orden Brw-Consulta 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Main}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpLinea C-Win 
PROCEDURE ImpLinea :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
          DISPLAY Tmp-Transa.Tnombre        AT 1   FORMAT "X(30)"
                  Tmp-Transa.TarjetaDB      AT 32  FORMAT "X(16)"
                  Tmp-Transa.Num_Cuenta     AT 49  FORMAT "X(12)"
                  Tmp-Transa.TipoRed        AT 62 
                  Tmp-Transa.NroTerminal    AT 65
                  Tmp-Transa.lugar          AT 74  FORMAT "X(30)"
                  Tmp-Transa.Hora_Transac   AT 104
                  Tmp-Transa.Fec_Transac    AT 113
                  Tmp-Transa.Monto          AT 124 FORMAT "zz,zzz,zz9"
                  Tmp-Transa.Comision       AT 136 FORMAT "zzz,zz9"
                  Tmp-Transa.ComAdicional   AT 144 FORMAT "zzz,zz9"
                  Tmp-Transa.OtrosCargos    AT 153 FORMAT "zzz,zz9"
                  Tmp-Transa.TipoTransac    AT 162
                  Tmp-Transa.Tdetalle       AT 167 FORMAT "X(30)"
                  SUBSTRING(Tmp-Transa.TdetError,8,30)  AT 199 FORMAT "X(30)"
                   WITH FRAME F-mov1 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 250 NO-LABELS.

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
ASSIGN Cmb-NomIni:LIST-ITEMS     IN FRAME Frm-Nombres   = "" 
       Cmb-NomFin:LIST-ITEMS     IN FRAME Frm-Nombres   = ""
       Cmb-TarIni:LIST-ITEMS     IN FRAME Frm-Tarjeta   = ""
       Cmb-TarFin:LIST-ITEMS     IN FRAME Frm-Tarjeta   = ""
       W-FecIni:SCREEN-VALUE IN FRAME Frm-main = STRING(W_Fecha - DAY(w_fecha) + 1)
       W-FecFin:SCREEN-VALUE IN FRAME Frm-main = STRING(W_Fecha)
       W-FecIni  W-FecFin
       Mov_Tarjetas.Nit:SCREEN-VALUE       = ""
       Mov_Tarjetas.TarjetaDb:SCREEN-VALUE = ""
       Mov_Tarjetas.TarjetaDb:VISIBLE      = FALSE
       Mov_Tarjetas.Nit:VISIBLE            = TRUE
       FRAME Frm-Nombres:VISIBLE           = FALSE
       FRAME Frm-Tarjeta:VISIBLE           = FALSE.


IF R-tipo:SCREEN-VALUE = "3" THEN DO:
    FOR EACH Tarjetas WHERE Tarjetas.TarjetaDb NE "" NO-LOCK BREAK BY Tarjetas.TarjetaDB :
        ASSIGN w_ok = Cmb-TarIni:ADD-LAST(Tarjetas.TarjetaDB) IN FRAME Frm-Tarjeta
               w_ok = Cmb-TarFin:ADD-LAST(Tarjetas.TarjetaDB) IN FRAME Frm-Tarjeta.
        IF FIRST(Tarjetas.TarjetaDB) THEN
           ASSIGN Cmb-TarIni:SCREEN-VALUE IN FRAME Frm-Tarjeta = Tarjetas.TarjetaDb.
        IF LAST(Tarjetas.TarjetaDB) THEN
           ASSIGN Cmb-TarFin:SCREEN-VALUE IN FRAME Frm-Tarjeta = Tarjetas.TarjetaDb.
    END.
END.
IF R-tipo:SCREEN-VALUE = "4" THEN DO:
    FOR EACH Tarjetas WHERE Tarjetas.Nombres NE " " BREAK BY Tarjetas.Nombres:
       ASSIGN w_ok = Cmb-NomIni:ADD-LAST(Tarjetas.Nombres) IN FRAME Frm-Nombres
              w_ok = Cmb-NomFin:ADD-LAST(Tarjetas.Nombres) IN FRAME Frm-Nombres.
       IF FIRST(Tarjetas.Nombres) THEN
          ASSIGN Cmb-NomIni:SCREEN-VALUE IN FRAME Frm-Nombres = Tarjetas.Nombres.
       IF LAST(Tarjetas.Nombres) THEN
          ASSIGN Cmb-NomFin:SCREEN-VALUE IN FRAME Frm-Nombres = Tarjetas.Nombres.
    END.
END.
ASSIGN W_totMonto:SCREEN-VALUE    = "0" W_TotMonto
       W_TotComision:SCREEN-VALUE = "0" W_TotComision
       W_TotComAdi:SCREEN-VALUE   = "0" W_TotComAdi
       W_TotTransacc:SCREEN-VALUE = "0" W_TotTransacc.
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
 FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.
 DO WITH FRAME Frm-main:
     ASSIGN R-Tipo  W-FecIni  W-FecFin  r-tipo R-Orden.
 END.
 CASE R-Tipo2:  /* Tipo Cuenta */
     WHEN 1 THEN W_Reporte = "REPORTE : Listado de Operaciones : TipoCta General  ".
     WHEN 2 THEN W_Reporte = "REPORTE : Listado de Operaciones : TipoCta Ahorros  ".
     WHEN 3 THEN W_Reporte = "REPORTE : Listado de Operaciones : TipoCta Rotativo ".
     WHEN 4 THEN W_Reporte = "REPORTE : Listado de Operaciones : TipoCta Cobro Tarjetas ".
     WHEN 5 THEN W_Reporte = "REPORTE : Listado de Operaciones : TipoCta Cobro Cuotas   ".
 END CASE.
 CASE R-Orden:  
   WHEN 1 THEN  W_Reporte = TRIM(W_Reporte) + " - Clasificado por Nombres ".
   WHEN 2 THEN  W_Reporte = TRIM(W_Reporte) + " - Clasificado por Fecha Transacción ".
   WHEN 3 THEN  W_Reporte = TRIM(W_Reporte) + " - Clasificado por Lugar ".
   WHEN 4 THEN  W_Reporte = TRIM(W_Reporte) + " - Clasificado por Monto ".
   WHEN 5 THEN  W_Reporte = TRIM(W_Reporte) + " - Clasificado por Hora Transacción ".
   WHEN 6 THEN  W_Reporte = TRIM(W_Reporte) + " - Clasificado por Tipo Transacción ".
   WHEN 7 THEN  W_Reporte = TRIM(W_Reporte) + " - Clasificado por TarjetaDb ".
 END CASE.      
 W_Reporte =  TRIM(W_Reporte) + " -  " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

 W_EncColumna = "Nombres                        TarjetaDB        Num_Cuenta  Red NroTerm  lugar                   Hora_Transac  Fec_Transac      Monto Comision ComAdic Cuota/Co  TTra Detalle  Errores".    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.
 CASE R-Orden:
     WHEN 1 THEN  FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.Tnombres         : RUN impLinea. END.
     WHEN 2 THEN  FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.Fec_Transac      : RUN impLinea. END.
     WHEN 3 THEN  FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.Lugar            : RUN impLinea. END.
     WHEN 4 THEN  FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.Monto            : RUN impLinea. END.
     WHEN 5 THEN  FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.Hora_transac     : RUN impLinea. END.
     WHEN 6 THEN  FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.TipoTransaccion  : RUN impLinea. END.
     WHEN 7 THEN  FOR EACH Tmp-Transa NO-LOCK BY Tmp-Transa.TarjetaDB        : RUN impLinea. END.
 END CASE.
 PUT "" SKIP(2).
 DISPLAY "Tot. Monto"      AT 1   
         W_TotMonto        AT 12  FORMAT "-zzz,zzz,zzz,zz9.99"
         "Comisión"        AT 33 
         W_TotComision     AT 43  FORMAT "-zzz,zzz,zzz,zz9"
         "Com.Adicional"   AT 64
         W_TotComAdi       AT 78  FORMAT "-zzz,zzz,zzz,zz9"
         "Cuota/CobroPlas" AT 99
         W_TotOtrosC       AT 116 FORMAT "-zzz,zzz,zzz,zz9"
         "Total Transacciones" AT 137
         W_TotTransacc    AT 158 FORMAT "-zzz,zzz,zz9"
          WITH FRAME F-mov2 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 200 NO-LABELS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

