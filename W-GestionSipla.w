&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
 {Incluido/Variable.I "SHARED"}
 DEFINE VAR W_Ok AS LOGICAL.
 DEFINE BUFFER iclientes FOR Clientes.
 
 DEFINE TEMP-TABLE TIC
     FIELD TNum AS INTEGER
     FIELD TDes AS CHARACTER FORMAT "X(100)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FSipla
&Scoped-define BROWSE-NAME Bicliente

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TIC BorradorSipla

/* Definitions for BROWSE Bicliente                                     */
&Scoped-define FIELDS-IN-QUERY-Bicliente TIC.TDes   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Bicliente   
&Scoped-define SELF-NAME Bicliente
&Scoped-define QUERY-STRING-Bicliente FOR EACH TIC
&Scoped-define OPEN-QUERY-Bicliente OPEN QUERY {&SELF-NAME} FOR EACH TIC.
&Scoped-define TABLES-IN-QUERY-Bicliente TIC
&Scoped-define FIRST-TABLE-IN-QUERY-Bicliente TIC


/* Definitions for FRAME FSipla                                         */
&Scoped-define FIELDS-IN-QUERY-FSipla BorradorSipla.Id_RepROSS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FSipla BorradorSipla.Id_RepROSS 
&Scoped-define ENABLED-TABLES-IN-QUERY-FSipla BorradorSipla
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FSipla BorradorSipla
&Scoped-define QUERY-STRING-FSipla FOR EACH BorradorSipla SHARE-LOCK
&Scoped-define OPEN-QUERY-FSipla OPEN QUERY FSipla FOR EACH BorradorSipla SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FSipla BorradorSipla
&Scoped-define FIRST-TABLE-IN-QUERY-FSipla BorradorSipla


/* Definitions for FRAME F_InfoCliente                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_InfoCliente ~
    ~{&OPEN-QUERY-Bicliente}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS BorradorSipla.Id_RepROSS 
&Scoped-define ENABLED-TABLES BorradorSipla
&Scoped-define FIRST-ENABLED-TABLE BorradorSipla
&Scoped-Define ENABLED-OBJECTS PNit TId_AhoCre PTipoTransaccion PValor ~
PDescripcion PExonerada PSospechosa PRepfiscalia BUTTON-2 BUTTON-181 ~
BtnDone BUTTON-182 BUTTON-183 BUTTON-184 RECT-285 RECT-286 RECT-287 ~
RECT-288 RECT-289 RECT-290 RECT-291 RECT-321 RECT-323 RECT-324 
&Scoped-Define DISPLAYED-FIELDS BorradorSipla.Id_RepROSS 
&Scoped-define DISPLAYED-TABLES BorradorSipla
&Scoped-define FIRST-DISPLAYED-TABLE BorradorSipla
&Scoped-Define DISPLAYED-OBJECTS AcumDia_ABCT AcumDia_ABST AcumMes_ABCT ~
AcumMes_ABST PNit TId_AhoCre PNombre PTipoTransaccion PValor AcumDia_CSST ~
AcumDia_RTST AcumMes_RTST AcumMes_CSST PNUD AcumDia_CSCT AcumDia_RTCT ~
AcumMes_RTCT AcumMes_CSCT PNUM PUltActualizacion FechaDia PDescripcion ~
PExonerada PSospechosa PRepfiscalia 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 PNit PNombre PTipoTransaccion PValor AcumDia_CSST ~
AcumDia_RTST AcumMes_RTST AcumMes_CSST PNUD AcumDia_CSCT AcumDia_RTCT ~
AcumMes_RTCT AcumMes_CSCT PNUM PUltActualizacion PDescripcion PExonerada ~
PSospechosa PRepfiscalia 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-181 
     LABEL "Informacion del cliente" 
     SIZE 29 BY 1.35.

DEFINE BUTTON BUTTON-182 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 182" 
     SIZE 15 BY 1.88.

DEFINE BUTTON BUTTON-183 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 183" 
     SIZE 15 BY 1.88.

DEFINE BUTTON BUTTON-184 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 184" 
     SIZE 15 BY 1.88.

DEFINE BUTTON BUTTON-2 
     LABEL "Grabar Borrador y Generar Cod. Autorizacion" 
     SIZE 51 BY 1.35.

DEFINE VARIABLE PDescripcion AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 88 BY 4.04
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumDia_ABCT AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumDia_ABST AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumDia_CSCT AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumDia_CSST AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumDia_RTCT AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumDia_RTST AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumMes_ABCT AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumMes_ABST AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumMes_CSCT AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumMes_CSST AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumMes_RTCT AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE AcumMes_RTST AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE FechaDia AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE PNit AS CHARACTER FORMAT "X(12)":U 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE PNombre AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 68 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE PUltActualizacion AS DATE FORMAT "99/99/99":U 
     LABEL "Ultima Actualizacion de datos personales" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE PValor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor Transaccion" 
     VIEW-AS FILL-IN 
     SIZE 18.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Seis AS CHARACTER FORMAT "X(50)":U INITIAL " Mas de seis meses, Actualizar!!!" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .92
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE PTipoTransaccion AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Consignacion", 1,
"Retiro", 2
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TId_AhoCre AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ahorro", 1,
"Credito", 2
     SIZE 24 BY 1.08
     BGCOLOR 18 FGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 36 BY 3.23
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-286
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 36 BY 2.42
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 16 BY 2.42
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-288
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 87 BY 1.04
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-289
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 1.35.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 16 BY 3.23
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 17 BY 5.65
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 27 BY 1.62
     BGCOLOR 18 .

DEFINE RECTANGLE RECT-323
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 18 BY 2.42
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-324
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 18 BY 3.23
     BGCOLOR 8 .

DEFINE VARIABLE PExonerada AS LOGICAL INITIAL yes 
     LABEL "Normal" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .77 NO-UNDO.

DEFINE VARIABLE PNUD AS LOGICAL INITIAL no 
     LABEL "No usual dia" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .77
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE PNUM AS LOGICAL INITIAL no 
     LABEL "No usual mes" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .77
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE PRepfiscalia AS LOGICAL INITIAL no 
     LABEL "Reportar a UIAF" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.14 BY .77 NO-UNDO.

DEFINE VARIABLE PSospechosa AS LOGICAL INITIAL no 
     LABEL "Sospechosa" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.86 BY .77 NO-UNDO.

DEFINE BUTTON BUTTON-123 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 123" 
     SIZE 9 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Bicliente FOR 
      TIC SCROLLING.

DEFINE QUERY FSipla FOR 
      BorradorSipla SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Bicliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Bicliente wWin _FREEFORM
  QUERY Bicliente DISPLAY
      TIC.TDes
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 72 BY 5.38
         BGCOLOR 15 FONT 2 ROW-HEIGHT-CHARS .45 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FSipla
     BorradorSipla.Id_RepROSS AT ROW 18.88 COL 63
          LABEL "Reportar a ROSS"
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .77
     AcumDia_ABCT AT ROW 10.23 COL 57 COLON-ALIGNED NO-LABEL
     AcumDia_ABST AT ROW 7.08 COL 57 COLON-ALIGNED NO-LABEL
     AcumMes_ABCT AT ROW 11.15 COL 57 COLON-ALIGNED NO-LABEL
     AcumMes_ABST AT ROW 8 COL 57 COLON-ALIGNED NO-LABEL
     PNit AT ROW 1.27 COL 5 COLON-ALIGNED
     TId_AhoCre AT ROW 2.62 COL 17 NO-LABEL
     PNombre AT ROW 1.27 COL 22 COLON-ALIGNED NO-LABEL
     PTipoTransaccion AT ROW 4.23 COL 17 NO-LABEL
     PValor AT ROW 4.23 COL 56 COLON-ALIGNED
     AcumDia_CSST AT ROW 7.08 COL 23.57 COLON-ALIGNED NO-LABEL
     AcumDia_RTST AT ROW 7.08 COL 39.29 COLON-ALIGNED NO-LABEL
     AcumMes_RTST AT ROW 8 COL 39.29 COLON-ALIGNED NO-LABEL
     AcumMes_CSST AT ROW 8.08 COL 23.57 COLON-ALIGNED NO-LABEL
     PNUD AT ROW 10.15 COL 75
     AcumDia_CSCT AT ROW 10.23 COL 23.43 COLON-ALIGNED NO-LABEL
     AcumDia_RTCT AT ROW 10.23 COL 39.14 COLON-ALIGNED NO-LABEL
     AcumMes_RTCT AT ROW 11.15 COL 39.14 COLON-ALIGNED NO-LABEL
     AcumMes_CSCT AT ROW 11.23 COL 23.43 COLON-ALIGNED NO-LABEL
     PNUM AT ROW 11.23 COL 75
     Seis AT ROW 12.58 COL 60 COLON-ALIGNED NO-LABEL
     PUltActualizacion AT ROW 12.62 COL 48 COLON-ALIGNED
     FechaDia AT ROW 13.58 COL 48 COLON-ALIGNED NO-LABEL
     PDescripcion AT ROW 14.58 COL 3 NO-LABEL
     PExonerada AT ROW 18.88 COL 6
     PSospechosa AT ROW 18.88 COL 21
     PRepfiscalia AT ROW 18.88 COL 40.86
     BUTTON-2 AT ROW 19.96 COL 3
     BUTTON-181 AT ROW 19.96 COL 55
     BtnDone AT ROW 19.58 COL 96
     BUTTON-182 AT ROW 1.27 COL 96
     BUTTON-183 AT ROW 3.15 COL 96
     BUTTON-184 AT ROW 5.04 COL 96
     RECT-285 AT ROW 9.08 COL 4
     RECT-286 AT ROW 6.65 COL 4
     RECT-287 AT ROW 6.65 COL 40
     RECT-288 AT ROW 5.58 COL 4
     RECT-289 AT ROW 3.96 COL 15
     RECT-290 AT ROW 9.08 COL 40
     RECT-291 AT ROW 6.65 COL 74
     RECT-321 AT ROW 2.35 COL 15
     RECT-323 AT ROW 6.65 COL 56
     RECT-324 AT ROW 9.08 COL 56
     "Acumulado Dia" VIEW-AS TEXT
          SIZE 14 BY 1.08 AT ROW 7 COL 11.72
          BGCOLOR 8 
     "Acumulado Mes" VIEW-AS TEXT
          SIZE 14 BY .88 AT ROW 8.08 COL 11.14
          BGCOLOR 8 
     "Consignaciones" VIEW-AS TEXT
          SIZE 15 BY .81 AT ROW 5.73 COL 24.57
          BGCOLOR 8 FGCOLOR 7 
     "Retiros" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 5.81 COL 44.57
          BGCOLOR 8 FGCOLOR 7 
     "Con nueva transaccion" VIEW-AS TEXT
          SIZE 19.86 BY .62 AT ROW 9.35 COL 5
          BGCOLOR 8 FGCOLOR 7 
     "Informacion recolectada en la entrevista al cliente hoy" VIEW-AS TEXT
          SIZE 46 BY .62 AT ROW 13.77 COL 3
          BGCOLOR 17 FGCOLOR 7 
     "Sin esta transaccion" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 5.77 COL 5.43
          BGCOLOR 8 FGCOLOR 7 
     "Acumulado Dia" VIEW-AS TEXT
          SIZE 14 BY 1.08 AT ROW 10.15 COL 11.57
          BGCOLOR 8 
     "Acumulado Mes" VIEW-AS TEXT
          SIZE 14 BY .88 AT ROW 11.23 COL 11
          BGCOLOR 8 
     "Banderas" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 5.85 COL 77.72
          BGCOLOR 8 FGCOLOR 7 
     "Abono Creditos" VIEW-AS TEXT
          SIZE 14.43 BY .62 AT ROW 5.77 COL 58.57
          BGCOLOR 8 FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.72 BY 20.96
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F_InfoCliente
     Bicliente AT ROW 1.27 COL 3
     BUTTON-123 AT ROW 6.92 COL 66
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 12.58
         SIZE 76 BY 8.62
         BGCOLOR 17 FONT 5
         TITLE "Informacion especifica del cliente".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Generacion Codigo Autorizacion Gestion SIPLA"
         HEIGHT             = 20.96
         WIDTH              = 112.72
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_InfoCliente:FRAME = FRAME FSipla:HANDLE.

/* SETTINGS FOR FRAME FSipla
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_InfoCliente:MOVE-AFTER-TAB-ITEM (PValor:HANDLE IN FRAME FSipla)
       XXTABVALXX = FRAME F_InfoCliente:MOVE-BEFORE-TAB-ITEM (AcumDia_CSST:HANDLE IN FRAME FSipla)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN AcumDia_ABCT IN FRAME FSipla
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AcumDia_ABST IN FRAME FSipla
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AcumDia_CSCT IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN AcumDia_CSST IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN AcumDia_RTCT IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN AcumDia_RTST IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN AcumMes_ABCT IN FRAME FSipla
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AcumMes_ABST IN FRAME FSipla
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AcumMes_CSCT IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN AcumMes_CSST IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN AcumMes_RTCT IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN AcumMes_RTST IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FechaDia IN FRAME FSipla
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX BorradorSipla.Id_RepROSS IN FRAME FSipla
   EXP-LABEL                                                            */
/* SETTINGS FOR EDITOR PDescripcion IN FRAME FSipla
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX PExonerada IN FRAME FSipla
   1                                                                    */
/* SETTINGS FOR FILL-IN PNit IN FRAME FSipla
   1                                                                    */
/* SETTINGS FOR FILL-IN PNombre IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX PNUD IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX PNUM IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX PRepfiscalia IN FRAME FSipla
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX PSospechosa IN FRAME FSipla
   1                                                                    */
/* SETTINGS FOR RADIO-SET PTipoTransaccion IN FRAME FSipla
   1                                                                    */
/* SETTINGS FOR FILL-IN PUltActualizacion IN FRAME FSipla
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN PValor IN FRAME FSipla
   1                                                                    */
/* SETTINGS FOR FILL-IN Seis IN FRAME FSipla
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Seis:HIDDEN IN FRAME FSipla           = TRUE.

/* SETTINGS FOR FRAME F_InfoCliente
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Bicliente 1 F_InfoCliente */
ASSIGN 
       FRAME F_InfoCliente:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Bicliente
/* Query rebuild information for BROWSE Bicliente
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TIC.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Bicliente */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FSipla
/* Query rebuild information for FRAME FSipla
     _TblList          = "bdcentral.BorradorSipla"
     _Query            is OPENED
*/  /* FRAME FSipla */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Generacion Codigo Autorizacion Gestion SIPLA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Generacion Codigo Autorizacion Gestion SIPLA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME FSipla /* Salir */
DO:
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


&Scoped-define FRAME-NAME F_InfoCliente
&Scoped-define SELF-NAME BUTTON-123
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-123 wWin
ON CHOOSE OF BUTTON-123 IN FRAME F_InfoCliente /* Button 123 */
DO:
  HIDE FRAME F_InfoCliente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FSipla
&Scoped-define SELF-NAME BUTTON-181
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-181 wWin
ON CHOOSE OF BUTTON-181 IN FRAME FSipla /* Informacion del cliente */
DO:
  ASSIGN FRAME FSipla PNit.
  IF PNit NE "" AND PNombre NE "" THEN VIEW FRAME F_InfoCliente.
  ELSE DO: 
     MESSAGE "Debe digitarse un nit valido" VIEW-AS ALERT-BOX INFORMATION.
     APPLY "entry" TO PNit.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME FSipla /* Grabar Borrador y Generar Cod. Autorizacion */
DO:
  ASSIGN FRAME FSipla PNit PValor PDescripcion PSospechosa PExonerada PRepFiscalia TId_AhoCre.
  IF PNit NE "" AND PValor NE 0 THEN DO:
     CREATE BorradorSipla.
     ASSIGN BorradorSipla.Agencia       = W_Agencia
            BorradorSipla.Nit           = PNit
            BorradorSipla.Fecha         = W_Fecha
            BorradorSipla.Hora          = TIME
            BorradorSipla.Valor         = PValor
            BorradorSipla.Usuario       = W_Usuario
            BorradorSipla.Descripcion   = PDescripcion
            BorradorSipla.Id_Nud        = PNud
            BorradorSipla.Id_Num         = PNum
            BorradorSipla.Id_Sospechosa  = PSospechosa
            BorradorSipla.Id_Exonerada   = PExonerada
            BorradorSipla.Id_RepFiscalia = PRepFiscalia
            BorradorSipla.CodAutoriza   = NEXT-VALUE(Sec_Autorizacion)
            BorradorSipla.Id_AhoCre     = TId_AhoCre.
     MESSAGE "Codigo de Autorizacion del Deposito: " BorradorSipla.CodAutoriza
              VIEW-AS ALERT-BOX INFORMATION.
     RUN Inicializar_Pantalla.
  END.
  ELSE DO:
    MESSAGE "Para grabar un borrador, debe digitarse" SKIP
            "tanto el nit como el valor a depositar en" SKIP
            "taquilla. rectifique de nuevo la informacion" VIEW-AS ALERT-BOX.
    APPLY "entry" TO PNit.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PNit wWin
ON LEAVE OF PNit IN FRAME FSipla /* Nit */
DO:
  HIDE Seis IN FRAME FSipla.
  FOR EACH TIC: DELETE TIC. END.
  FIND iClientes WHERE 
       iClientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE iClientes THEN DO:
     PNombre = iClientes.Nombre + " " + iClientes.Apellido1 + " " + iClientes.Apellido2.
     PUltActualizacion = iClientes.Fec_UltActualiza.
     IF ((W_Fecha - PUltActualizacion) / 30) GT 6 THEN
         DISPLAY Seis WITH FRAME FSipla.
     RUN Llenar_ECliente.
     FIND LAST ControlSipla WHERE
               ControlSipla.Nit EQ iClientes.Nit AND 
               MONTH(ControlSipla.Fecha) EQ MONTH(W_Fecha) AND
               YEAR(ControlSipla.Fecha)  EQ YEAR(W_Fecha) NO-LOCK NO-ERROR.
     IF AVAILABLE ControlSipla THEN
        ASSIGN AcumDia_CSST = ControlSipla.CS_TotalDia
               AcumDia_RTST = ControlSipla.RT_TotalDia
               AcumMes_CSST = ControlSipla.CS_TotalMes
               AcumMes_RTST = ControlSipla.RT_TotalMes
               AcumDia_ABST = ControlSipla.AB_TotalDia
               AcumMes_ABST = ControlSipla.AB_TotalMes.
     DISPLAY AcumDia_CSST AcumDia_RTST AcumMes_CSST AcumMes_RTST PNombre AcumDia_ABST AcumMes_ABST 
             PUltActualizacion WITH FRAME FSipla.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PValor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PValor wWin
ON LEAVE OF PValor IN FRAME FSipla /* Valor Transaccion */
DO:
  ASSIGN FRAME FSipla PTipoTransaccion PValor TId_AhoCre.
  IF AVAILABLE ControlSipla THEN DO:
     IF PTipoTransaccion EQ 1 THEN DO:
        IF TId_AhoCre EQ 1 THEN
           ASSIGN AcumDia_CSCT = ControlSipla.CS_TotalDia + PValor
                  AcumMes_CSCT = ControlSipla.CS_TotalMes + PValor
                  AcumDia_ABCT = ControlSipla.AB_TotalDia
                  AcumMes_ABCT = ControlSipla.AB_TotalMes.
        ELSE
           ASSIGN AcumDia_ABCT = ControlSipla.AB_TotalDia + PValor
                  AcumMes_ABCT = ControlSipla.AB_TotalMes + PValor
                  AcumDia_CSCT = ControlSipla.CS_TotalDia
                  AcumMes_CSCT = ControlSipla.CS_TotalMes.
     END.
     ELSE
        ASSIGN AcumDia_RTCT = ControlSipla.RT_TotalDia + PValor
               AcumMes_RTCT = ControlSipla.RT_TotalMes + PValor.
  END.
  ELSE DO:
     IF PTipoTransaccion EQ 1 THEN DO:
         IF TId_AhoCre EQ 1 THEN
            ASSIGN AcumDia_CSCT = PValor
                   AcumMes_CSCT = PValor.
         ELSE
            ASSIGN AcumDia_ABCT = PValor
                   AcumMes_ABCT = PValor.
     END.
     ELSE
        ASSIGN AcumDia_RTCT = PValor
               AcumMes_RTCT = PValor.
  END.
  ASSIGN PNUD = NO PNUM = NO.
  IF AcumDia_CSCT GT Entidad.MaxOp_Efectivo_Dia OR AcumDia_RTCT GT MaxOp_Efectivo_Dia THEN PNUD = YES.
  IF AcumMes_CSCT GT Entidad.MaxOp_Efectivo_Mes OR AcumMes_RTCT GT MaxOp_Efectivo_Mes THEN PNUM = YES.
  DISPLAY AcumDia_CSCT AcumMes_CSCT AcumDia_RTCT AcumMes_RTCT PNUD PNUM
          AcumDia_ABST AcumMes_ABST AcumDia_ABCT AcumMes_ABCT WITH FRAME FSipla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Bicliente
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-FSipla}
  GET FIRST FSipla.
  DISPLAY AcumDia_ABCT AcumDia_ABST AcumMes_ABCT AcumMes_ABST PNit TId_AhoCre 
          PNombre PTipoTransaccion PValor AcumDia_CSST AcumDia_RTST AcumMes_RTST 
          AcumMes_CSST PNUD AcumDia_CSCT AcumDia_RTCT AcumMes_RTCT AcumMes_CSCT 
          PNUM PUltActualizacion FechaDia PDescripcion PExonerada PSospechosa 
          PRepfiscalia 
      WITH FRAME FSipla IN WINDOW wWin.
  IF AVAILABLE BorradorSipla THEN 
    DISPLAY BorradorSipla.Id_RepROSS 
      WITH FRAME FSipla IN WINDOW wWin.
  ENABLE BorradorSipla.Id_RepROSS PNit TId_AhoCre PTipoTransaccion PValor 
         PDescripcion PExonerada PSospechosa PRepfiscalia BUTTON-2 BUTTON-181 
         BtnDone BUTTON-182 BUTTON-183 BUTTON-184 RECT-285 RECT-286 RECT-287 
         RECT-288 RECT-289 RECT-290 RECT-291 RECT-321 RECT-323 RECT-324 
      WITH FRAME FSipla IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FSipla}
  ENABLE Bicliente BUTTON-123 
      WITH FRAME F_InfoCliente IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_InfoCliente}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Info wWin 
PROCEDURE Grabar_Info :
DEFINE INPUT PARAMETER Des AS CHARACTER FORMAT "X(80)".
DEFINE VAR i AS INTEGER.
FIND LAST TIC NO-ERROR.
IF NOT AVAILABLE TIC THEN DO:
    CREATE TIC.
    ASSIGN TIC.TNUM = 1.
END.
ELSE DO:
    i = TIC.TNUM.
    CREATE TIC.
    ASSIGN TIC.TNUM = i + 1.
END.
TIC.TDES = Des.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Pantalla wWin 
PROCEDURE Inicializar_Pantalla :
FOR EACH Tic: DELETE Tic. END.
OPEN QUERY BiCliente FOR EACH TIC.
ASSIGN PNit              = ""
       PNombre           = ""
       PValor            = 0
       PDescripcion      = ""
       PNum              = NO
       PNud              = NO
       PSospechosa       = NO
       PExonerada        = NO
       PRepFiscalia      = NO
       PUltActualizacion = ?
       PTipoTransaccion  = 1
       AcumDia_CSST      = 0
       AcumMes_CSST      = 0
       AcumDia_RTST      = 0
       AcumMes_RTST      = 0
       AcumDia_CSCT      = 0
       AcumMes_CSCT      = 0
       AcumDia_RTCT      = 0
       AcumMes_RTCT      = 0.
DISPLAY {&List-1} WITH FRAME FSipla.
HIDE Seis IN FRAME FSipla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DEFINE VAR Xok AS LOGICAL INITIAL YES.
  FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  FechaDia = W_Fecha.

  FIND Cfg_Instancias WHERE
       Cfg_Instancias.Agencia        EQ W_Agencia AND
       Cfg_Instancias.Tipo_Instancia EQ 6         AND
       Cfg_Instancias.Orden          EQ 1         AND
       Cfg_Instancias.Usuario        EQ W_Usuario AND
       Cfg_Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Cfg_Instancia THEN DO:
     MESSAGE "Su usuario no tiene la instancia necesaria" SKIP
             "que le permite trabajar en este programa" VIEW-AS ALERT-BOX INFORMATION.
     XOk = NO.
  END.
       
  RUN SUPER.

  IF NOT XOk THEN APPLY "choose" TO BtnDone IN FRAME FSipla.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_ECliente wWin 
PROCEDURE Llenar_ECliente :
DEFINE VAR XTT AS CHARACTER FORMAT "X(30)".
DEFINE VAR XTT2 AS CHARACTER FORMAT "X(30)".
DEFINE VAR VTT AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
XTT = ">> SEGMENTACION <<". 
RUN Grabar_Info(INPUT XTT).
FIND Agencias WHERE Agencias.Agencia EQ iClientes.Agencia NO-LOCK NO-ERROR.
XTT = "Agencia origen           : " + STRING(iClientes.Agencia) + " - " + Agencias.Nombre. 
RUN Grabar_Info(INPUT XTT).
FIND Empresas WHERE Empresas.Cod_Empresa EQ iClientes.Cod_Empresa NO-LOCK NO-ERROR.
IF AVAILABLE Empresas THEN DO:
   FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN DO:
       XTT = "Empresa donde labora     : " + STRING(iClientes.Cod_Empresa) + " - " + Clientes.Nombre. 
       RUN Grabar_Info(INPUT XTT).
   END.
END.
FIND Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ iClientes.Cod_Profesion NO-LOCK NO-ERROR.
IF AVAILABLE Varios THEN DO:
    XTT = "Profesion                : " + STRING(Varios.Descripcion).
    RUN Grabar_Info(INPUT XTT).
END.

CASE iclientes.Tip_Contrato:
    WHEN 0 THEN XTT2 = "Ninguno".
    WHEN 1 THEN XTT2 = "Indefinido".
    WHEN 2 THEN XTT2 = "Fijo".
    WHEN 3 THEN XTT2 = "Labor Prestada".
    WHEN 4 THEN XTT2 = "Prestacion Servicios".
    WHEN 5 THEN XTT2 = "Pensionado".
END CASE.
XTT = "Tipo de Contrato         : " + XTT2.
RUN Grabar_Info(INPUT XTT).

FIND Varios WHERE Varios.Tipo EQ 6 AND Varios.Codigo EQ iclientes.Cod_Segmento NO-LOCK NO-ERROR.
IF AVAILABLE Varios THEN DO:
    XTT = "Segmento                 : " + STRING(Varios.Descripcion).
    RUN Grabar_Info(INPUT XTT).
END.
XTT = "Sujeto a retencion       : " + STRING(iClientes.Id_Retencion). 
RUN Grabar_Info(INPUT XTT).
XTT = "Es gran contribuyente    : " + STRING(iClientes.Gran_Contribuyente). 
RUN Grabar_Info(INPUT XTT).

XTT = "". 
RUN Grabar_Info(INPUT XTT).
XTT = ">> ECONOMICA <<". 
RUN Grabar_Info(INPUT XTT).

VTT = iClientes.Salario + iClientes.Ing_Financieros + iClientes.Ing_Honorarios + iClientes.Ing_Otros.
XTT = "Ing.x Salarios           : " + STRING(iClientes.Salario,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
XTT = "Ingresos Financieros     : " + STRING(iClientes.Ing_Financieros,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
XTT = "Ing.x Honorarios         : " + STRING(iClientes.Ing_Honorarios,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
XTT = "Otros Ingresos           : " + STRING(iClientes.Ing_Otros,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
XTT = "       >> Total Ingresos : " + STRING(VTT,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).

XTT = "". 
RUN Grabar_Info(INPUT XTT).
VTT = iclientes.Gto_familiar + iclientes.Gto_Arriendo + iclientes.Gto_Obligacion.
XTT = "Gastos Familiares        : " + STRING(iClientes.Gto_Familiar,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
XTT = "Gastos de Arriendo       : " + STRING(iClientes.Gto_Arriendo,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
XTT = "Gastos Obligaciones      : " + STRING(iClientes.Gto_Obligacion,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
XTT = "         >> Total Gastos : " + STRING(VTT,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).

XTT = "". 
RUN Grabar_Info(INPUT XTT).
VTT = iclientes.Act_Casa + iclientes.Act_Vehiculo + iclientes.Act_Inversion.
XTT = "Activos: Propiedad Raiz  : " + STRING(iClientes.Act_Casa,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
XTT = "Activos: Vehiculo        : " + STRING(iClientes.Act_Vehiculo,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
XTT = "Activos: Inversiones     : " + STRING(iClientes.Act_Inversion,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
XTT = "        >> Total Activos : " + STRING(VTT,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).

XTT = "". 
RUN Grabar_Info(INPUT XTT).
XTT = "Saldo Obligaciones       : " + STRING(iClientes.Sdo_Obligaciones). 
RUN Grabar_Info(INPUT XTT).

XTT = "". 
RUN Grabar_Info(INPUT XTT).
XTT = ">> TOTALES DE AHORRO <<". 
RUN Grabar_Info(INPUT XTT).
FOR EACH Ahorros WHERE 
         Ahorros.Tip_Ahorro EQ 4 AND
         Ahorros.Nit        EQ iclientes.Nit AND 
         Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje GT 0 NO-LOCK:
    VTT = VTT + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
END.
XTT = "Aportes                  : " + STRING(VTT,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
VTT = 0.
FOR EACH Ahorros WHERE 
         Ahorros.Tip_Ahorro EQ 1 AND
         Ahorros.Nit        EQ iclientes.Nit AND 
         Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje GT 0 NO-LOCK:
    VTT = VTT + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
END.
XTT = "A la vista               : " + STRING(VTT,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
VTT = 0.
FOR EACH Ahorros WHERE 
         Ahorros.Tip_Ahorro EQ 2 AND
         Ahorros.Nit        EQ iclientes.Nit AND 
         Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje GT 0 NO-LOCK:
    VTT = VTT + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
END.
XTT = "Contractual              : " + STRING(VTT,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
VTT = 0.
FOR EACH Ahorros WHERE 
         Ahorros.Tip_Ahorro EQ 4 AND
         Ahorros.Nit        EQ iclientes.Nit AND 
         Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje GT 0 NO-LOCK:
    VTT = VTT + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
END.
XTT = "A Termino                : " + STRING(VTT,">>>,>>>,>>>,>>9"). 
RUN Grabar_Info(INPUT XTT).
OPEN QUERY Bicliente FOR EACH TIC.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

