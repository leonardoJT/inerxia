&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

  /* {Incluido\VARIABLE.I "SHARED"}*/
DEFINE VAR W_ok AS LOGICAL.
DEFINE VAR W_New AS LOGICAL INITIAL NO.
DEFINE VAR Puntero AS ROWID.
DEFINE SHARED VAR W_Nom_Agencia    AS   CHARACTER FORMAT "X(60)".
DEFINE SHARED VAR W_Nom_Entidad    AS   CHARACTER FORMAT "X(60)".
DEFINE SHARED VAR W_Usuario        LIKE Usuarios.Usuario.
DEFINE SHARED VAR W_Estacion       LIKE Estaciones.Estacion.
DEFINE SHARED VAR W_Manija         AS   HANDLE.
DEFINE SHARED VAR W_PathSpl        LIKE Entidad.Dir_Spl.

DEFINE VAR WNomCodFila AS CHARACTER FORMAT "X(50)".
DEFINE VAR WNomEntFila AS CHARACTER FORMAT "X(50)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME Bcpub

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PUB Ind_PUB Cfg_PUB

/* Definitions for BROWSE Bcpub                                         */
&Scoped-define FIELDS-IN-QUERY-Bcpub Pub.Ente Pub.Clase Pub.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Bcpub   
&Scoped-define SELF-NAME Bcpub
&Scoped-define QUERY-STRING-Bcpub FOR EACH PUB WHERE PUB.Estado EQ 1 BY PUB.Ente INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Bcpub OPEN QUERY Bcpub FOR EACH PUB WHERE PUB.Estado EQ 1 BY PUB.Ente INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Bcpub PUB
&Scoped-define FIRST-TABLE-IN-QUERY-Bcpub PUB


/* Definitions for BROWSE BIndicadores                                  */
&Scoped-define FIELDS-IN-QUERY-BIndicadores Ind_PUB.Codigo Ind_PUB.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BIndicadores   
&Scoped-define SELF-NAME BIndicadores
&Scoped-define QUERY-STRING-BIndicadores FOR EACH Ind_PUB WHERE Ind_PUB.Estado EQ 1 BY Ind_PUB.Codigo INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BIndicadores OPEN QUERY BIndicadores FOR EACH Ind_PUB WHERE Ind_PUB.Estado EQ 1 BY Ind_PUB.Codigo INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BIndicadores Ind_PUB
&Scoped-define FIRST-TABLE-IN-QUERY-BIndicadores Ind_PUB


/* Definitions for BROWSE B_PUB                                         */
&Scoped-define FIELDS-IN-QUERY-B_PUB Cfg_PUB.Codigo WNomCodFila Cfg_PUB.Ente WNomEntFila   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_PUB   
&Scoped-define SELF-NAME B_PUB
&Scoped-define QUERY-STRING-B_PUB FOR EACH Cfg_PUB BY Cfg_PUB.Codigo BY Cfg_PUB.Ente INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_PUB OPEN QUERY {&SELF-NAME} FOR EACH Cfg_PUB BY Cfg_PUB.Codigo BY Cfg_PUB.Ente INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_PUB Cfg_PUB
&Scoped-define FIRST-TABLE-IN-QUERY-B_PUB Cfg_PUB


/* Definitions for FRAME FCPUB                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FCPUB ~
    ~{&OPEN-QUERY-Bcpub}

/* Definitions for FRAME FIndicadores                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FIndicadores ~
    ~{&OPEN-QUERY-BIndicadores}

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain Cfg_PUB.Ente Cfg_PUB.Codigo ~
Cfg_PUB.Fec_Ingreso Cfg_PUB.Fec_Retiro Cfg_PUB.Usuario Cfg_PUB.Estado ~
Cfg_PUB.Id_Alimentacion Cfg_PUB.Usa_Limites Cfg_PUB.Cuenta 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain Cfg_PUB.Estado ~
Cfg_PUB.Id_Alimentacion Cfg_PUB.Usa_Limites Cfg_PUB.Cuenta 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain Cfg_PUB
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain Cfg_PUB
&Scoped-define QUERY-STRING-fMain FOR EACH Cfg_PUB SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH Cfg_PUB SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain Cfg_PUB
&Scoped-define FIRST-TABLE-IN-QUERY-fMain Cfg_PUB


/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-B_PUB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Cfg_PUB.Estado Cfg_PUB.Id_Alimentacion ~
Cfg_PUB.Usa_Limites Cfg_PUB.Cuenta 
&Scoped-define ENABLED-TABLES Cfg_PUB
&Scoped-define FIRST-ENABLED-TABLE Cfg_PUB
&Scoped-Define ENABLED-OBJECTS BFInd BFPUB EDes Btn_Metas BUTTON-49 ~
BUTTON-1 BUTTON-2 B_CrSalvar B_CRIngresar B_CRCancelar B_CRBorrar BtnDone ~
RECT-301 
&Scoped-Define DISPLAYED-FIELDS Cfg_PUB.Ente Cfg_PUB.Codigo ~
Cfg_PUB.Fec_Ingreso Cfg_PUB.Fec_Retiro Cfg_PUB.Usuario Cfg_PUB.Estado ~
Cfg_PUB.Id_Alimentacion Cfg_PUB.Usa_Limites Cfg_PUB.Cuenta 
&Scoped-define DISPLAYED-TABLES Cfg_PUB
&Scoped-define FIRST-DISPLAYED-TABLE Cfg_PUB
&Scoped-Define DISPLAYED-OBJECTS EDes NomUsuario Nom_CodPUB Nom_IndPUB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 EDes Cfg_PUB.Ente Cfg_PUB.Codigo 
&Scoped-define List-2 Cfg_PUB.Metas[1] Cfg_PUB.Metas[2] Cfg_PUB.Metas[3] ~
Cfg_PUB.Metas[4] Cfg_PUB.Metas[5] Cfg_PUB.Metas[6] Cfg_PUB.Metas[7] ~
Cfg_PUB.Metas[8] Cfg_PUB.Metas[9] Cfg_PUB.Metas[10] Cfg_PUB.Metas[11] ~
Cfg_PUB.Metas[12] 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-68 
     LABEL "Ocultar" 
     SIZE 10 BY 1.12.

DEFINE VARIABLE RBuscaP AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Perspectivas", 1,
"Subgerencias", 2,
"Grupos", 3,
"Usuarios", 4,
"Todos", 5
     SIZE 59 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 1.35.

DEFINE BUTTON BUTTON-67 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE Cpers AS CHARACTER FORMAT "X(256)":U INITIAL "00 - Todas las perspectivas" 
     LABEL "Filtro x Perspectiva" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00 - Todas las perspectivas" 
     DROP-DOWN-LIST
     SIZE 27 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RBuscaI AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impacto", 1,
"Eficiencia", 2,
"Eficacia", 3,
"Todos", 4
     SIZE 41.72 BY .81
     FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46 BY 1.35.

DEFINE BUTTON BFInd 
     LABEL "C" 
     SIZE 3 BY .85.

DEFINE BUTTON BFPUB 
     LABEL "C" 
     SIZE 3 BY .85.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Metas 
     LABEL "Metas" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 1" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 2" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-49 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 49" 
     SIZE 13 BY 1.54.

DEFINE BUTTON B_CRBorrar 
     LABEL "Borrar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON B_CRCancelar 
     LABEL "Cancelar" 
     SIZE 13 BY 1.65.

DEFINE BUTTON B_CRIngresar 
     LABEL "Ingresar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON B_CrSalvar 
     LABEL "Salvar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON C_CRDeshacer 
     LABEL "Deshacer" 
     SIZE 13 BY 1.62.

DEFINE VARIABLE EDes AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 48 BY 3.23
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomUsuario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_CodPUB AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_IndPUB AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-301
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 1.35.

DEFINE BUTTON BUTTON-63 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE IMetas AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-60 
     LABEL "Ocultar" 
     SIZE 14 BY 1.12.

DEFINE VARIABLE Corg1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Organizar por" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Indicador","Ente" 
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WBusca AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RBusca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 0,
"Ente", 1,
"Indicador", 2
     SIZE 35 BY .81
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 56 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Bcpub FOR 
      PUB SCROLLING.

DEFINE QUERY BIndicadores FOR 
      Ind_PUB SCROLLING.

DEFINE QUERY B_PUB FOR 
      Cfg_PUB SCROLLING.

DEFINE QUERY fMain FOR 
      Cfg_PUB SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Bcpub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Bcpub wWin _FREEFORM
  QUERY Bcpub DISPLAY
      Pub.Ente Pub.Clase Pub.Nombre
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87.57 BY 9.15
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE BIndicadores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BIndicadores wWin _FREEFORM
  QUERY BIndicadores NO-LOCK DISPLAY
      Ind_PUB.Codigo Ind_PUB.Nombre
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 63 BY 7.27
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE B_PUB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_PUB wWin _FREEFORM
  QUERY B_PUB NO-LOCK DISPLAY
      Cfg_PUB.Codigo   COLUMN-LABEL "Codigo"
      WNomCodFila         COLUMN-LABEL "Nom.Codigo"
      Cfg_PUB.Ente     COLUMN-LABEL "Ente PUB" 
      WNomEntFila         COLUMN-LABEL "Nom.Ente"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 6.19
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BFInd AT ROW 5.31 COL 32
     BFPUB AT ROW 4.23 COL 32
     EDes AT ROW 10.69 COL 23 NO-LABEL
     Btn_Metas AT ROW 6.65 COL 23
     NomUsuario AT ROW 16.88 COL 26 COLON-ALIGNED NO-LABEL
     Nom_CodPUB AT ROW 4.23 COL 33 COLON-ALIGNED NO-LABEL
     Nom_IndPUB AT ROW 5.31 COL 33 COLON-ALIGNED NO-LABEL
     BUTTON-49 AT ROW 3.42 COL 96
     BUTTON-1 AT ROW 5.35 COL 96
     BUTTON-2 AT ROW 6.96 COL 96
     B_CrSalvar AT ROW 9.12 COL 96
     C_CRDeshacer AT ROW 10.73 COL 96
     B_CRIngresar AT ROW 12.35 COL 96
     B_CRCancelar AT ROW 13.96 COL 96
     B_CRBorrar AT ROW 15.58 COL 96
     BtnDone AT ROW 17.19 COL 96
     Cfg_PUB.Ente AT ROW 4.23 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Cfg_PUB.Codigo AT ROW 5.31 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Cfg_PUB.Fec_Ingreso AT ROW 14.19 COL 59 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Cfg_PUB.Fec_Retiro AT ROW 15.27 COL 59 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Cfg_PUB.Usuario AT ROW 16.88 COL 22 COLON-ALIGNED
          LABEL "Usuario"
          VIEW-AS FILL-IN 
          SIZE 4.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Cfg_PUB.Estado AT ROW 6.65 COL 41 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 19 BY 1.08
     Cfg_PUB.Id_Alimentacion AT ROW 8.27 COL 48 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Automática":U, 1,
"Manual":U, 2
          SIZE 24 BY .81
     Cfg_PUB.Usa_Limites AT ROW 18.5 COL 16
          LABEL "Usa Limites"
          VIEW-AS TOGGLE-BOX
          SIZE 12.72 BY .65
     Cfg_PUB.Cuenta AT ROW 14.19 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     "  Descripción de la configuración" VIEW-AS TEXT
          SIZE 48 BY .81 AT ROW 9.88 COL 23
          BGCOLOR 18 FGCOLOR 15 
     " Forma de ingreso de información" VIEW-AS TEXT
          SIZE 24 BY .81 AT ROW 8.27 COL 23
          BGCOLOR 18 FGCOLOR 15 
     RECT-301 AT ROW 16.62 COL 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.29 BY 18.85
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Consulta
     B_PUB AT ROW 1.27 COL 2
     RBusca AT ROW 8 COL 7 NO-LABEL
     WBusca AT ROW 8 COL 41 COLON-ALIGNED NO-LABEL
     BUTTON-60 AT ROW 8 COL 70
     Corg1 AT ROW 9.35 COL 27 COLON-ALIGNED
     RECT-302 AT ROW 7.73 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14 ROW 1
         SIZE 85 BY 10.5
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Indicadores PUB".

DEFINE FRAME FMetas
     Cfg_PUB.Metas[1] AT ROW 1.54 COL 11 COLON-ALIGNED
          LABEL "Enero"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[2] AT ROW 2.62 COL 11 COLON-ALIGNED
          LABEL "Febrero"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[3] AT ROW 3.69 COL 11 COLON-ALIGNED
          LABEL "Marzo"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[4] AT ROW 4.77 COL 11 COLON-ALIGNED
          LABEL "Abril"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[5] AT ROW 5.85 COL 11 COLON-ALIGNED
          LABEL "Mayo"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[6] AT ROW 6.92 COL 11 COLON-ALIGNED
          LABEL "Junio"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[7] AT ROW 1.54 COL 39 COLON-ALIGNED
          LABEL "Julio"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[8] AT ROW 2.62 COL 39 COLON-ALIGNED
          LABEL "Agosto"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[9] AT ROW 3.69 COL 39 COLON-ALIGNED
          LABEL "Septiembre"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[10] AT ROW 4.77 COL 39 COLON-ALIGNED
          LABEL "Octubre"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[11] AT ROW 5.85 COL 39 COLON-ALIGNED
          LABEL "Noviembre"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Cfg_PUB.Metas[12] AT ROW 6.92 COL 39 COLON-ALIGNED
          LABEL "Diciembre"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     IMetas AT ROW 8.08 COL 3 COLON-ALIGNED NO-LABEL
     BUTTON-63 AT ROW 8.08 COL 40
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 12 ROW 1
         SIZE 62 BY 9.42
         BGCOLOR 17 FONT 4
         TITLE "Metas del Año".

DEFINE FRAME FIndicadores
     BIndicadores AT ROW 1.54 COL 3
     BUTTON-67 AT ROW 9.08 COL 51
     RBuscaI AT ROW 9.35 COL 6.29 NO-LABEL
     Cpers AT ROW 10.69 COL 21 COLON-ALIGNED
     RECT-303 AT ROW 9.08 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19 ROW 1
         SIZE 67 BY 10.5
         BGCOLOR 17 FONT 4
         TITLE "Indicadores Existentes".

DEFINE FRAME FCPUB
     Bcpub AT ROW 1.27 COL 2.43
     RBuscaP AT ROW 10.96 COL 3 NO-LABEL
     BUTTON-68 AT ROW 10.96 COL 79
     RECT-304 AT ROW 10.69 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 8 ROW 1.27
         SIZE 91 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "Entes PUB Existentes".


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
         TITLE              = "Configuración de los Entes PUB"
         HEIGHT             = 18.85
         WIDTH              = 114.29
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
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
ASSIGN FRAME FCPUB:FRAME = FRAME fMain:HANDLE
       FRAME FIndicadores:FRAME = FRAME fMain:HANDLE
       FRAME FMetas:FRAME = FRAME fMain:HANDLE
       FRAME F_Consulta:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME FCPUB
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Bcpub RECT-304 FCPUB */
ASSIGN 
       FRAME FCPUB:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FIndicadores
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BIndicadores RECT-303 FIndicadores */
ASSIGN 
       FRAME FIndicadores:HIDDEN           = TRUE.

ASSIGN 
       BIndicadores:SEPARATOR-FGCOLOR IN FRAME FIndicadores      = 1.

/* SETTINGS FOR FRAME fMain
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FIndicadores:MOVE-BEFORE-TAB-ITEM (BFInd:HANDLE IN FRAME fMain)
       XXTABVALXX = FRAME FMetas:MOVE-AFTER-TAB-ITEM (BUTTON-1:HANDLE IN FRAME fMain)
       XXTABVALXX = FRAME FMetas:MOVE-BEFORE-TAB-ITEM (BUTTON-2:HANDLE IN FRAME fMain)
       XXTABVALXX = FRAME F_Consulta:MOVE-BEFORE-TAB-ITEM (FRAME FIndicadores:HANDLE)
       XXTABVALXX = FRAME FCPUB:MOVE-BEFORE-TAB-ITEM (FRAME F_Consulta:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN Cfg_PUB.Codigo IN FRAME fMain
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON C_CRDeshacer IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDes IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN Cfg_PUB.Ente IN FRAME fMain
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Fec_Ingreso IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_PUB.Fec_Retiro IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomUsuario IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_CodPUB IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_IndPUB IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Cfg_PUB.Usa_Limites IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Cfg_PUB.Usuario IN FRAME fMain
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME FMetas
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME FMetas:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN IMetas IN FRAME FMetas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[10] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[11] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[12] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[1] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[2] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[3] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[4] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[5] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[6] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[7] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[8] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_PUB.Metas[9] IN FRAME FMetas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FRAME F_Consulta
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_PUB RECT-302 F_Consulta */
ASSIGN 
       FRAME F_Consulta:HIDDEN           = TRUE
       FRAME F_Consulta:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Bcpub
/* Query rebuild information for BROWSE Bcpub
     _START_FREEFORM
OPEN QUERY Bcpub FOR EACH PUB WHERE PUB.Estado EQ 1 BY PUB.Ente INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Bcpub */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BIndicadores
/* Query rebuild information for BROWSE BIndicadores
     _START_FREEFORM
OPEN QUERY BIndicadores FOR EACH Ind_PUB WHERE Ind_PUB.Estado EQ 1 BY Ind_PUB.Codigo INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BIndicadores */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_PUB
/* Query rebuild information for BROWSE B_PUB
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cfg_PUB BY Cfg_PUB.Codigo BY Cfg_PUB.Ente INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_PUB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "bdcentral.Cfg_PUB"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Configuración de los Entes PUB */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Configuración de los Entes PUB */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Bcpub
&Scoped-define FRAME-NAME FCPUB
&Scoped-define SELF-NAME Bcpub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bcpub wWin
ON MOUSE-SELECT-DBLCLICK OF Bcpub IN FRAME FCPUB
DO:
  ASSIGN Cfg_PUB.Ente:SCREEN-VALUE IN FRAME FMain = Pub.Ente
         Nom_CodPUB:SCREEN-VALUE = Pub.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BFInd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BFInd wWin
ON CHOOSE OF BFInd IN FRAME fMain /* C */
DO:
    OPEN QUERY BIndicadores FOR EACH Ind_PUB WHERE Ind_PUB.Estado EQ 1 BY Ind_PUB.Codigo INDEXED-REPOSITION.
  /*IF Cfg_PUB.Codigo:SENSITIVE THEN*/
     VIEW FRAME FIndicadores.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BFPUB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BFPUB wWin
ON CHOOSE OF BFPUB IN FRAME fMain /* C */
DO:
  OPEN QUERY Bcpub FOR EACH PUB WHERE PUB.Estado EQ 1 BY PUB.Ente INDEXED-REPOSITION.
  /*IF Cfg_PUB.Codigo:SENSITIVE THEN*/
     VIEW FRAME FCPUB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BIndicadores
&Scoped-define FRAME-NAME FIndicadores
&Scoped-define SELF-NAME BIndicadores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BIndicadores wWin
ON MOUSE-SELECT-DBLCLICK OF BIndicadores IN FRAME FIndicadores
DO:
DO WITH FRAME FMain:
  ASSIGN Cfg_PUB.Codigo:SCREEN-VALUE = STRING(Ind_PUB.Codigo,"99999999")
         Nom_IndPUB:SCREEN-VALUE = Ind_PUB.Nombre.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain /* Salir */
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


&Scoped-define SELF-NAME Btn_Metas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Metas wWin
ON CHOOSE OF Btn_Metas IN FRAME fMain /* Metas */
DO:
  VIEW FRAME FMetas.
  ENABLE {&List-2} WITH FRAME FMetas.
  APPLY "entry" TO Cfg_PUB.Metas[1] IN FRAME FMetas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".    
  listado = W_PathSpl + "Instancias.Lst".
  {Incluido\Imprimir.I "listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Button 2 */
DO:
    OPEN QUERY B_PUB FOR EACH Cfg_PUB 
     NO-LOCK BY Cfg_PUB.Codigo BY Cfg_PUB.Ente INDEXED-REPOSITION.
  VIEW FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-49
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-49 wWin
ON CHOOSE OF BUTTON-49 IN FRAME fMain /* Button 49 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME BUTTON-60
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-60 wWin
ON CHOOSE OF BUTTON-60 IN FRAME F_Consulta /* Ocultar */
DO:
  HIDE FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMetas
&Scoped-define SELF-NAME BUTTON-63
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-63 wWin
ON CHOOSE OF BUTTON-63 IN FRAME FMetas /* Ocultar */
DO:
  HIDE FRAME FMetas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FIndicadores
&Scoped-define SELF-NAME BUTTON-67
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-67 wWin
ON CHOOSE OF BUTTON-67 IN FRAME FIndicadores /* Ocultar */
DO:
  HIDE FRAME FIndicadores.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCPUB
&Scoped-define SELF-NAME BUTTON-68
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-68 wWin
ON CHOOSE OF BUTTON-68 IN FRAME FCPUB /* Ocultar */
DO:
  HIDE FRAME FCPUB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME B_CRBorrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRBorrar wWin
ON CHOOSE OF B_CRBorrar IN FRAME fMain /* Borrar */
DO:
   FIND CURRENT Cfg_PUB.
   IF AVAILABLE CFG_PUB THEN DELETE cfg_pub.
   ELSE MESSAGE "No se econtro el registro a borrar".
   /*ASSIGN Cfg_PUB.Fec_Retiro = TODAY
          Cfg_PUB.Estado     = 2.*/
   FIND FIRST Cfg_Pub NO-LOCK.
   IF AVAILABLE Cfg_Pub THEN RUN Mostrar_Registro.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_CRCancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRCancelar wWin
ON CHOOSE OF B_CRCancelar IN FRAME fMain /* Cancelar */
DO:
  IF Puntero NE ? THEN DO:
     FIND Cfg_PUB WHERE ROWID(Cfg_PUB) EQ Puntero NO-LOCK NO-ERROR.
     IF AVAILABLE Cfg_PUB THEN DO:
        RUN Mostrar_Registro.
        DISABLE Cfg_PUB.Codigo WITH FRAME FMain.
     END.
  END.
  ELSE DISABLE {&List-1} WITH FRAME FMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_CRIngresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRIngresar wWin
ON CHOOSE OF B_CRIngresar IN FRAME fMain /* Ingresar */
DO:
  W_New = YES.
 /* IF AVAILABLE Ind_PUB THEN DO:
      Puntero = ROWID(Ind_PUB).*/
      DO WITH FRAME FMain:
         ASSIGN  Cfg_PUB.Codigo:SCREEN-VALUE      = "0"
                 Cfg_PUB.Ente:SCREEN-VALUE        = "0"
                 EDes:SCREEN-VALUE                = ""
                 Cfg_PUB.Estado:SCREEN-VALUE      = "1"
                 Cfg_PUB.Fec_Ingreso:SCREEN-VALUE = STRING(TODAY)
                 Cfg_PUB.Fec_Retiro:SCREEN-VALUE  = "?"
                 Cfg_PUB.Usuario:SCREEN-VALUE     = W_Usuario
                 Cfg_PUB.Usa_Limites:SCREEN-VALUE = "YES"
                 Cfg_Pub.Cuenta:SCREEN-VALUE      = "".
      END.
      DO WITH FRAME FMetas:
         ASSIGN Cfg_PUB.Metas[1]:SCREEN-VALUE    = "0"
                Cfg_PUB.Metas[2]:SCREEN-VALUE    = "0"
                Cfg_PUB.Metas[3]:SCREEN-VALUE    = "0"
                Cfg_PUB.Metas[4]:SCREEN-VALUE    = "0"
                Cfg_PUB.Metas[5]:SCREEN-VALUE    = "0"
                Cfg_PUB.Metas[6]:SCREEN-VALUE    = "0"
                Cfg_PUB.Metas[7]:SCREEN-VALUE    = "0"
                Cfg_PUB.Metas[8]:SCREEN-VALUE    = "0"
                Cfg_PUB.Metas[9]:SCREEN-VALUE    = "0"
                Cfg_PUB.Metas[10]:SCREEN-VALUE   = "0"
                Cfg_PUB.Metas[11]:SCREEN-VALUE   = "0"
                Cfg_PUB.Metas[12]:SCREEN-VALUE   = "0".
      END.
  /*END.*/
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Usuarios THEN NomUsuario:SCREEN-VALUE = Usuarios.Nombre.
  ENABLE {&List-1} BfPub BfInd WITH FRAME FMain.
  APPLY "entry" TO Cfg_PUB.Ente.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_CrSalvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CrSalvar wWin
ON CHOOSE OF B_CrSalvar IN FRAME fMain /* Salvar */
DO: 
IF W_New THEN DO:
   FIND Cfg_PUB WHERE
        Cfg_PUB.Codigo EQ INTEGER(Cfg_PUB.Codigo:SCREEN-VALUE) AND
        Cfg_PUB.Ente EQ Cfg_PUB.Ente:SCREEN-VALUE NO-ERROR.
   IF AVAILABLE Cfg_PUB THEN DO:
      MESSAGE "Ya existe la Configuracion con este Código" SKIP
              "y este Ente" SKIP 
              "se cancela la operacion de salvado del" SKIP
              "nuevo registro" VIEW-AS ALERT-BOX.
      RUN Mostrar_Registro.
      APPLY "entry" TO EDes.
   END.
   ELSE DO:
     CREATE Cfg_PUB.
     W_New = NO.
   END.
END.
ELSE 
  FIND CURRENT Cfg_PUB.
ASSIGN FRAME FMain Cfg_PUB.Codigo Cfg_PUB.Ente 
        Cfg_PUB.Estado Cfg_PUB.Fec_Ingreso Cfg_PUB.Fec_Retiro
        Cfg_PUB.Usuario Cfg_Pub.Id_Alimentacion Cfg_Pub.Usa_Limites Cfg_Pub.Cuenta.
Cfg_PUB.Descripcion = EDes:SCREEN-VALUE.
ASSIGN FRAME FMetas Cfg_PUB.Metas[1] Cfg_PUB.Metas[2] Cfg_PUB.Metas[3]
        Cfg_PUB.Metas[4] Cfg_PUB.Metas[5] Cfg_PUB.Metas[6]
        Cfg_PUB.Metas[7] Cfg_PUB.Metas[8] Cfg_PUB.Metas[9]
        Cfg_PUB.Metas[10] Cfg_PUB.Metas[11] Cfg_PUB.Metas[12].
DISABLE Bfpub BfInd WITH FRAME Fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_PUB
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME B_PUB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_PUB wWin
ON MOUSE-SELECT-DBLCLICK OF B_PUB IN FRAME F_Consulta
DO:
 RUN Mostrar_Registro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_PUB wWin
ON ROW-DISPLAY OF B_PUB IN FRAME F_Consulta
DO:
  FIND PUB WHERE PUB.Ente EQ Cfg_PUB.Ente NO-LOCK NO-ERROR.
  IF AVAILABLE PUB THEN
     WNomEntFila = PUB.Nombre.
  ELSE
     WNomEntFila = "No Existe en el PUB".
  FIND Ind_PUB WHERE Ind_PUB.Codigo EQ Cfg_PUB.Codigo NO-LOCK NO-ERROR.
  IF AVAILABLE Ind_PUB THEN
     WNomCodFila = Ind_PUB.Nombre.
  ELSE
     WNomCodFila = "No Existe en  Indicadores PUB".
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME Cfg_PUB.Codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_PUB.Codigo wWin
ON LEAVE OF Cfg_PUB.Codigo IN FRAME fMain /* Código */
DO:
  FIND Ind_PUB WHERE
       Ind_PUB.Codigo EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Ind_PUB THEN
     Nom_IndPUB:SCREEN-VALUE = Ind_PUB.Nombre.
  ELSE DO:
     MESSAGE "El indicador entrado no existe en la tabla" SKIP
             "de configuracion de indicadores PUB" SKIP(1)
             "escoja un indicador de la tabla que" SKIP
             "a continuación aparecera" VIEW-AS ALERT-BOX WARNING.
     APPLY "choose" TO BFInd.
  END.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Corg1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Corg1 wWin
ON VALUE-CHANGED OF Corg1 IN FRAME F_Consulta /* Organizar por */
DO:
  CASE SELF:SCREEN-VALUE:
      WHEN "Indicador" THEN
          OPEN QUERY B_Pub FOR EACH Cfg_PUB BY Cfg_PUB.Codigo BY Cfg_PUB.Ente INDEXED-REPOSITION.
      WHEN "Ente" THEN
          OPEN QUERY B_Pub FOR EACH Cfg_PUB BY Cfg_Pub.Ente INDEXED-REPOSITION.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FIndicadores
&Scoped-define SELF-NAME Cpers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cpers wWin
ON VALUE-CHANGED OF Cpers IN FRAME FIndicadores /* Filtro x Perspectiva */
DO:
  CASE SELF:SCREEN-VALUE:
      WHEN "00 - Todas las perspectivas" THEN
          OPEN QUERY BIndicadores FOR EACH Ind_PUB WHERE 
            Ind_PUB.Estado EQ 1 BY Ind_PUB.Codigo INDEXED-REPOSITION.
      OTHERWISE
          OPEN QUERY BIndicadores FOR EACH Ind_PUB WHERE 
            Ind_Pub.Perspectiva EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) AND
            Ind_PUB.Estado EQ 1 BY Ind_PUB.Codigo INDEXED-REPOSITION.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME C_CRDeshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C_CRDeshacer wWin
ON CHOOSE OF C_CRDeshacer IN FRAME fMain /* Deshacer */
DO:
 /* RUN Deshacer_Cambios_Creacion.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_PUB.Ente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_PUB.Ente wWin
ON LEAVE OF Cfg_PUB.Ente IN FRAME fMain /* Ente */
DO:
  FIND PUB WHERE
       PUB.Ente EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE PUB THEN
     Nom_CodPUB:SCREEN-VALUE = PUB.Nombre.
  ELSE DO:
     MESSAGE "El Ente entrada no existe en la tabla" SKIP
             "de configuracion de Ente PUB" SKIP(1)
             "escoja un indicador de la tabla que" SKIP
             "a continuación aparecera" VIEW-AS ALERT-BOX WARNING.
     APPLY "choose" TO BFPUB.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_PUB.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_PUB.Estado wWin
ON VALUE-CHANGED OF Cfg_PUB.Estado IN FRAME fMain /* Estado */
DO:
  IF SELF:SCREEN-VALUE EQ "1" THEN
     Cfg_PUB.Fec_Retiro:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME RBusca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RBusca wWin
ON VALUE-CHANGED OF RBusca IN FRAME F_Consulta
DO:
  IF INTEGER(SELF:SCREEN-VALUE) EQ 0 THEN DO:
     DISABLE WBusca WITH FRAME F_Consulta.
     WBusca:SCREEN-VALUE = "".
     OPEN QUERY B_PUB FOR EACH Cfg_PUB
          BY Cfg_PUB.Codigo BY Cfg_PUB.Ente INDEXED-REPOSITION.
  END.
  ELSE DO:
     ENABLE WBusca WITH FRAME F_Consulta.
     APPLY "Entry" TO WBusca.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FIndicadores
&Scoped-define SELF-NAME RBuscaI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RBuscaI wWin
ON VALUE-CHANGED OF RBuscaI IN FRAME FIndicadores
DO:
  ASSIGN FRAME FIndicadores RBuscaI.
  IF RBuscaI EQ 4 THEN
     OPEN QUERY BIndicadores 
     FOR EACH Ind_PUB WHERE Ind_PUB.Estado EQ 1 BY Ind_PUB.Codigo INDEXED-REPOSITION.
  ELSE
     OPEN QUERY BIndicadores 
     FOR EACH Ind_PUB WHERE Ind_PUB.Tipo EQ RBuscaI AND 
              Ind_PUB.Estado EQ 1 BY Ind_PUB.Codigo INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCPUB
&Scoped-define SELF-NAME RBuscaP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RBuscaP wWin
ON VALUE-CHANGED OF RBuscaP IN FRAME FCPUB
DO:
  ASSIGN FRAME FCPUB RBuscaP.
  IF RBuscaP EQ 5 THEN
      OPEN QUERY Bcpub FOR EACH PUB WHERE PUB.Estado EQ 1 BY PUB.Ente INDEXED-REPOSITION.
  ELSE 
      OPEN QUERY Bcpub FOR EACH PUB WHERE PUB.Clase EQ RBuscaP AND 
                 PUB.Estado EQ 1 BY PUB.Ente INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME WBusca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WBusca wWin
ON LEAVE OF WBusca IN FRAME F_Consulta
DO:
  ASSIGN FRAME F_Consulta RBusca.
  IF RBusca EQ 1 THEN
     OPEN QUERY B_PUB FOR EACH Cfg_PUB WHERE Cfg_PUB.Ente BEGINS SELF:SCREEN-VALUE
          BY Cfg_PUB.Ente BY Cfg_PUB.Codigo INDEXED-REPOSITION.
  ELSE
     OPEN QUERY B_PUB FOR EACH Cfg_PUB WHERE Cfg_PUB.Codigo EQ INTEGER(SELF:SCREEN-VALUE)
          BY Cfg_PUB.Codigo BY Cfg_PUB.Ente INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME Bcpub
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

  {&OPEN-QUERY-fMain}
  GET FIRST fMain.
  DISPLAY EDes NomUsuario Nom_CodPUB Nom_IndPUB 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE Cfg_PUB THEN 
    DISPLAY Cfg_PUB.Ente Cfg_PUB.Codigo Cfg_PUB.Fec_Ingreso Cfg_PUB.Fec_Retiro 
          Cfg_PUB.Usuario Cfg_PUB.Estado Cfg_PUB.Id_Alimentacion 
          Cfg_PUB.Usa_Limites Cfg_PUB.Cuenta 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE BFInd BFPUB EDes Btn_Metas BUTTON-49 BUTTON-1 BUTTON-2 B_CrSalvar 
         B_CRIngresar B_CRCancelar B_CRBorrar BtnDone Cfg_PUB.Estado 
         Cfg_PUB.Id_Alimentacion Cfg_PUB.Usa_Limites RECT-301 Cfg_PUB.Cuenta 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY IMetas 
      WITH FRAME FMetas IN WINDOW wWin.
  IF AVAILABLE Cfg_PUB THEN 
    DISPLAY Cfg_PUB.Metas[1] Cfg_PUB.Metas[2] Cfg_PUB.Metas[3] Cfg_PUB.Metas[4] 
          Cfg_PUB.Metas[5] Cfg_PUB.Metas[6] Cfg_PUB.Metas[7] Cfg_PUB.Metas[8] 
          Cfg_PUB.Metas[9] Cfg_PUB.Metas[10] Cfg_PUB.Metas[11] Cfg_PUB.Metas[12] 
      WITH FRAME FMetas IN WINDOW wWin.
  ENABLE Cfg_PUB.Metas[1] Cfg_PUB.Metas[2] Cfg_PUB.Metas[3] Cfg_PUB.Metas[4] 
         Cfg_PUB.Metas[5] Cfg_PUB.Metas[6] Cfg_PUB.Metas[7] Cfg_PUB.Metas[8] 
         Cfg_PUB.Metas[9] Cfg_PUB.Metas[10] Cfg_PUB.Metas[11] Cfg_PUB.Metas[12] 
         BUTTON-63 
      WITH FRAME FMetas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FMetas}
  DISPLAY RBusca WBusca Corg1 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE RECT-302 B_PUB RBusca WBusca BUTTON-60 Corg1 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  DISPLAY RBuscaI Cpers 
      WITH FRAME FIndicadores IN WINDOW wWin.
  ENABLE RECT-303 BIndicadores BUTTON-67 RBuscaI Cpers 
      WITH FRAME FIndicadores IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FIndicadores}
  DISPLAY RBuscaP 
      WITH FRAME FCPUB IN WINDOW wWin.
  ENABLE RECT-304 Bcpub RBuscaP BUTTON-68 
      WITH FRAME FCPUB IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FCPUB}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
FOR EACH Pub WHERE Pub.Clase EQ 1 AND Pub.Estado EQ 1 BREAK BY Pub.Ente:
    W_Ok = CPers:ADD-LAST(STRING(Pub.Ente,"99") + " - " + Pub.Nombre) IN FRAME Findicadores.
END.
FIND FIRST Cfg_PUB NO-LOCK NO-ERROR.
IF AVAILABLE Cfg_PUB THEN DO:
   RUN Mostrar_Registro.
   DISABLE Cfg_PUB.Codigo WITH FRAME FMain.
   APPLY "entry" TO Cfg_PUB.Ente IN FRAME FMain.
END.
ELSE DO:
  DISABLE {&List-1} WITH FRAME FMain.
  DISABLE {&List-2} WITH FRAME FMetas.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Registro wWin 
PROCEDURE Mostrar_Registro :
DEFINE VAR Ctg AS CHARACTER FORMAT "X(12)".
  DISPLAY Cfg_PUB.Codigo Cfg_PUB.Ente 
              Cfg_PUB.Estado Cfg_PUB.Fec_Ingreso Cfg_PUB.Fec_Retiro
              Cfg_PUB.Usuario Cfg_Pub.Id_Alimentacion Cfg_Pub.Usa_Limites
              Cfg_Pub.Cuenta WITH FRAME FMain.
  Edes:SCREEN-VALUE = Cfg_PUB.Descripcion.
  FIND Ind_PUB WHERE
       Ind_PUB.Codigo EQ INTEGER(Cfg_PUB.Codigo:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Ind_PUB THEN DO:
     ASSIGN Nom_IndPUB:SCREEN-VALUE = Ind_PUB.Nombre.
     IF Ind_PUB.Unidad EQ 1 THEN
       ASSIGN IMetas:SCREEN-VALUE IN FRAME FMetas = "Unidad: CANTIDADES".
     ELSE
       ASSIGN IMetas:SCREEN-VALUE IN FRAME FMetas = "Unidad: VALORES".
  END.
  FIND PUB WHERE
       PUB.Ente EQ Cfg_PUB.Ente:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE PUB THEN DO:
     CASE LENGTH(Cfg_PUB.Ente):
         WHEN 2 THEN Ctg = "Perspectiva:".
         WHEN 4 THEN Ctg = "Subgerencia:".
         WHEN 6 THEN Ctg = "Agencia    :".
         WHEN 8 THEN Ctg = "Usuario    :".
     END CASE.
     Nom_CodPUB:SCREEN-VALUE = Ctg + " " + PUB.Nombre.
  END.
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Usuarios THEN NomUsuario:SCREEN-VALUE = Usuarios.Nombre.

  DISPLAY Cfg_PUB.Metas[1] Cfg_PUB.Metas[2] Cfg_PUB.Metas[3]
          Cfg_PUB.Metas[4] Cfg_PUB.Metas[5] Cfg_PUB.Metas[6]
          Cfg_PUB.Metas[7] Cfg_PUB.Metas[8] Cfg_PUB.Metas[9]
          Cfg_PUB.Metas[10] Cfg_PUB.Metas[11] Cfg_PUB.Metas[12] WITH FRAME FMetas.

  DISABLE Bfpub BfInd WITH FRAME Fmain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}
 W_Reporte   = "REPORTE   : CONFIGURACION BALANCE SCORE CARD - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "Cod Nombre Indicador                                   Ente     Nombre Ente".
 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.
 FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 NO-LOCK BREAK BY Cfg_pub.Codigo BY Cfg_pub.Ente:
   FIND PUB WHERE PUB.Ente EQ Cfg_PUB.Ente NO-LOCK NO-ERROR.
   IF AVAILABLE PUB THEN
      WNomEntFila = PUB.Nombre.
   ELSE
      WNomEntFila = "No Existe en el PUB".
   FIND Ind_PUB WHERE Ind_PUB.Codigo EQ Cfg_PUB.Codigo NO-LOCK NO-ERROR.
   IF AVAILABLE Ind_PUB THEN
      WNomCodFila = Ind_PUB.Nombre.
   ELSE
      WNomCodFila = "No Existe en  Indicadores PUB".
   DISPLAY Cfg_Pub.Codigo
           WNomCodFila
           Cfg_Pub.Ente
           WNomEntFila
       WITH FRAME Fmov WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
 END.
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

