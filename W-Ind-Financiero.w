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
{incluido\Variable.i "SHARED"}
/* Parameters Definitions ---                                           */
DEFINE VARIABLE W_Operador1 AS CHARACTER.
DEFINE VARIABLE W_Operador2 AS CHARACTER.
DEFINE VARIABLE W_Operando  AS CHARACTER.
DEFINE VARIABLE W_Cuenta    AS CHARACTER.
DEFINE VARIABLE W_Resultado AS CHARACTER.
DEFINE VARIABLE W_Fin       AS LOGICAL.
DEFINE VARIABLE W_Nindice   LIKE Varios.Tipo.
DEFINE VARIABLE W_Ofini    LIKE Agencias.Agencia.
DEFINE VARIABLE W_Offin    LIKE Agencias.Agencia.
DEFINE VARIABLE W_AnSel    AS INTEGER.
DEFINE VARIABLE W_ConAux   AS INTEGER.    
DEFINE VARIABLE Mano AS LOGICAL EXTENT 12. 
DEFINE VARIABLE W_Cal AS INTEGER.
DEFINE VARIABLE W_TotCal AS DECIMAL.   
DEFINE VARIABLE W_RenAct AS DECIMAL.
DEFINE VARIABLE W_CarVen AS DECIMAL.
DEFINE VARIABLE W_MarBru AS DECIMAL.
DEFINE VARIABLE W_PatAct AS DECIMAL.
DEFINE VARIABLE W_DisAct AS DECIMAL.
DEFINE VARIABLE W_PunQui AS DECIMAL.
DEFINE VARIABLE W_TotFin AS DECIMAL.
DEFINE TEMP-TABLE T-Resultado
   FIELD AgIni   AS INTEGER
   FIELD AgFin   AS INTEGER
   FIELD Indicador AS CHARACTER FORMAT 'X(20)'
   FIELD Tipo    LIKE Varios.Tipo
   FIELD Indice  LIKE Cfg_Varios.Tipo
   FIELD Operador1 AS DECIMAL
   FIELD Operador2 AS DECIMAL
   FIELD Operando  AS CHARACTER
   FIELD Porcen    AS DECIMAL
   FIELD Formula   AS CHARACTER FORMAT 'X(20)'
   FIELD Resultado AS CHARACTER FORMAT 'X(50)'
   FIELD comentario AS CHARACTER FORMAT 'X(200)'
   FIELD Mes       AS INTEGER
   FIELD Ano       AS INTEGER.
   
DEFINE TEMP-TABLE Listos
   FIELD OK   AS CHARACTER FORMAT 'XX'
   FIELD Cod  LIKE Varios.Codigo.

DEFINE TEMP-TABLE TotalInd
   FIELD Cod         LIKE Varios.Codigo
   FIELD Indicador   AS CHARACTER
   FIELD Porc        AS CHARACTER
   FIELD Calf        AS CHARACTER.   
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Mayor
&Scoped-define BROWSE-NAME B-Indicadores

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cfg_Varios Varios T-Resultado TotalInd ~
Listos

/* Definitions for BROWSE B-Indicadores                                 */
&Scoped-define FIELDS-IN-QUERY-B-Indicadores Cfg_Varios.Descripcion 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-Indicadores 
&Scoped-define QUERY-STRING-B-Indicadores FOR EACH Cfg_Varios ~
      WHERE Cfg_Varios.Tipo >= 13 ~
 AND Cfg_Varios.Tipo <= 16 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-Indicadores OPEN QUERY B-Indicadores FOR EACH Cfg_Varios ~
      WHERE Cfg_Varios.Tipo >= 13 ~
 AND Cfg_Varios.Tipo <= 16 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-Indicadores Cfg_Varios
&Scoped-define FIRST-TABLE-IN-QUERY-B-Indicadores Cfg_Varios


/* Definitions for BROWSE B-Indices                                     */
&Scoped-define FIELDS-IN-QUERY-B-Indices Varios.Codigo Varios.Descripcion 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-Indices 
&Scoped-define QUERY-STRING-B-Indices FOR EACH Varios NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-Indices OPEN QUERY B-Indices FOR EACH Varios NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-Indices Varios
&Scoped-define FIRST-TABLE-IN-QUERY-B-Indices Varios


/* Definitions for BROWSE B-Resultado                                   */
&Scoped-define FIELDS-IN-QUERY-B-Resultado T-Resultado.AgIni T-Resultado.AgFin T-Resultado.Indicador T-Resultado.Formula T-Resultado.Resultado T-Resultado.Porcen T-Resultado.Mes T-Resultado.Ano   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-Resultado   
&Scoped-define SELF-NAME B-Resultado
&Scoped-define QUERY-STRING-B-Resultado FOR EACH T-Resultado NO-LOCK
&Scoped-define OPEN-QUERY-B-Resultado OPEN QUERY {&SELF-NAME} FOR EACH T-Resultado NO-LOCK.
&Scoped-define TABLES-IN-QUERY-B-Resultado T-Resultado
&Scoped-define FIRST-TABLE-IN-QUERY-B-Resultado T-Resultado


/* Definitions for BROWSE General                                       */
&Scoped-define FIELDS-IN-QUERY-General TotalInd.Indicador TotalInd.Porc TotalInd.Calf   
&Scoped-define ENABLED-FIELDS-IN-QUERY-General   
&Scoped-define SELF-NAME General
&Scoped-define QUERY-STRING-General FOR EACH TotalInd
&Scoped-define OPEN-QUERY-General OPEN QUERY {&SELF-NAME} FOR EACH TotalInd.
&Scoped-define TABLES-IN-QUERY-General TotalInd
&Scoped-define FIRST-TABLE-IN-QUERY-General TotalInd


/* Definitions for BROWSE Listos                                        */
&Scoped-define FIELDS-IN-QUERY-Listos Listos.OK   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Listos   
&Scoped-define SELF-NAME Listos
&Scoped-define QUERY-STRING-Listos FOR EACH Listos NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Listos OPEN QUERY {&SELF-NAME} FOR EACH Listos NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Listos Listos
&Scoped-define FIRST-TABLE-IN-QUERY-Listos Listos


/* Definitions for FRAME F-Mayor                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Mayor ~
    ~{&OPEN-QUERY-B-Indicadores}~
    ~{&OPEN-QUERY-B-Indices}~
    ~{&OPEN-QUERY-B-Resultado}~
    ~{&OPEN-QUERY-Listos}

/* Definitions for FRAME F-Presenta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Presenta ~
    ~{&OPEN-QUERY-General}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Listos Anos Ofi-i Ofi-f BUTTON-9 BtnDone ~
BUTTON-1 BUTTON-2 RECT-2 RECT-4 
&Scoped-Define DISPLAYED-OBJECTS Anos Ofi-i Ofi-f Ed 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 10.43 BY 1.85
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10.43 BY 1.54 TOOLTIP "Información".

DEFINE BUTTON BUTTON-10 
     LABEL "&Cancelar" 
     SIZE 10.43 BY 1.85.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 10.43 BY 1.54 TOOLTIP "Imprimir".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 5" 
     SIZE 10.43 BY 1.54 TOOLTIP "Consulta".

DEFINE BUTTON BUTTON-6 
     LABEL "&Todos" 
     SIZE 10.43 BY 1.85.

DEFINE BUTTON BUTTON-7 
     LABEL "&Deshacer" 
     SIZE 10.43 BY 1.85.

DEFINE BUTTON BUTTON-8 
     LABEL "&Ingresar" 
     SIZE 10.43 BY 1.85.

DEFINE BUTTON BUTTON-9 
     LABEL "&Borrar" 
     SIZE 10.43 BY 1.85.

DEFINE VARIABLE Anos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Año" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 9 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Ofi-f AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Oficina Final" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 Ninguno" 
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Ofi-i AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Oficina Inicial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 Ninguno" 
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Ed AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 96 BY 4.19
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12.86 BY 5.62.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 12.08.

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 11" 
     SIZE 10.43 BY 1.54.

DEFINE BUTTON B1 
     LABEL "Año Completo" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 4" 
     SIZE 10.43 BY 1.54.

DEFINE VARIABLE uno-1 AS LOGICAL INITIAL no 
     LABEL "Enero" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE uno-10 AS LOGICAL INITIAL no 
     LABEL "Octubre" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .77 NO-UNDO.

DEFINE VARIABLE uno-11 AS LOGICAL INITIAL no 
     LABEL "Noviembre" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.29 BY .77 NO-UNDO.

DEFINE VARIABLE uno-12 AS LOGICAL INITIAL no 
     LABEL "Diciembre" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.29 BY .77 NO-UNDO.

DEFINE VARIABLE uno-2 AS LOGICAL INITIAL no 
     LABEL "Febrero" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .77 NO-UNDO.

DEFINE VARIABLE uno-3 AS LOGICAL INITIAL no 
     LABEL "Marzo" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .77 NO-UNDO.

DEFINE VARIABLE uno-4 AS LOGICAL INITIAL no 
     LABEL "Abril" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .77 NO-UNDO.

DEFINE VARIABLE uno-5 AS LOGICAL INITIAL no 
     LABEL "Mayo" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE uno-6 AS LOGICAL INITIAL no 
     LABEL "Junio" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE uno-7 AS LOGICAL INITIAL no 
     LABEL "Julio" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE uno-8 AS LOGICAL INITIAL no 
     LABEL "Agosto" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .77 NO-UNDO.

DEFINE VARIABLE uno-9 AS LOGICAL INITIAL no 
     LABEL "Septiembre" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.29 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-Indicadores FOR 
      Cfg_Varios SCROLLING.

DEFINE QUERY B-Indices FOR 
      Varios SCROLLING.

DEFINE QUERY B-Resultado FOR 
      T-Resultado SCROLLING.

DEFINE QUERY General FOR 
      TotalInd SCROLLING.

DEFINE QUERY Listos FOR 
      Listos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-Indicadores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-Indicadores wWin _STRUCTURED
  QUERY B-Indicadores NO-LOCK DISPLAY
      Cfg_Varios.Descripcion COLUMN-LABEL "               Indicadores Économicos" FORMAT "X(40)":U
            COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 15 COLUMN-FONT 4 LABEL-FGCOLOR 0 LABEL-BGCOLOR 17 LABEL-FONT 4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 32.14 BY 6.92
         BGCOLOR 15 FGCOLOR 0 FONT 4 ROW-HEIGHT-CHARS .54 EXPANDABLE.

DEFINE BROWSE B-Indices
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-Indices wWin _STRUCTURED
  QUERY B-Indices NO-LOCK DISPLAY
      Varios.Codigo FORMAT "99999":U COLUMN-FONT 4 LABEL-FGCOLOR 0 LABEL-BGCOLOR 17 LABEL-FONT 4
      Varios.Descripcion FORMAT "X(50)":U COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 15 COLUMN-FONT 4
            LABEL-FGCOLOR 0 LABEL-BGCOLOR 17 LABEL-FONT 4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 53 BY 6.96
         BGCOLOR 15 FGCOLOR 0 FONT 4 ROW-HEIGHT-CHARS .46 EXPANDABLE.

DEFINE BROWSE B-Resultado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-Resultado wWin _FREEFORM
  QUERY B-Resultado NO-LOCK DISPLAY
      T-Resultado.AgIni   FORMAT "999":U
      T-Resultado.AgFin   FORMAT "999":U
      T-Resultado.Indicador FORMAT "X(30)":U
      T-Resultado.Formula   FORMAT "X(30)":U
      T-Resultado.Resultado FORMAT "X(40)":U
      T-Resultado.Porcen  FORMAT "->>9.99":U
      T-Resultado.Mes       FORMAT "99":U
      T-Resultado.Ano       FORMAT "9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 96 BY 7.77
         BGCOLOR 15 FONT 4 EXPANDABLE.

DEFINE BROWSE General
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS General wWin _FREEFORM
  QUERY General DISPLAY
      TotalInd.Indicador FORMAT "X(50)"
      TotalInd.Porc FORMAT "X(10)"
      TotalInd.Calf FORMAT "X(10)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 57.57 BY 9.15
         BGCOLOR 15 FONT 4 EXPANDABLE.

DEFINE BROWSE Listos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Listos wWin _FREEFORM
  QUERY Listos NO-LOCK DISPLAY
      Listos.OK FORMAT "xx":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 7 BY 6.96
         BGCOLOR 15 FGCOLOR 0 FONT 2 ROW-HEIGHT-CHARS .46 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Mayor
     Listos AT ROW 2.77 COL 91
     Anos AT ROW 1.27 COL 4 COLON-ALIGNED
     Ofi-i AT ROW 1.27 COL 24.72 COLON-ALIGNED
     Ofi-f AT ROW 1.31 COL 65.57 COLON-ALIGNED
     BUTTON-10 AT ROW 16.19 COL 102.29
     BUTTON-6 AT ROW 8.69 COL 102.29
     BUTTON-7 AT ROW 10.58 COL 102.29
     BUTTON-8 AT ROW 12.42 COL 102.29
     BUTTON-9 AT ROW 14.31 COL 102.29
     BtnDone AT ROW 18.08 COL 102.29
     Ed AT ROW 18.08 COL 1 NO-LABEL
     B-Indices AT ROW 2.77 COL 38
     B-Indicadores AT ROW 2.85 COL 3
     B-Resultado AT ROW 10.08 COL 1
     BUTTON-1 AT ROW 1.92 COL 102.29
     BUTTON-2 AT ROW 3.62 COL 102.43
     BUTTON-5 AT ROW 5.27 COL 102.43
     RECT-2 AT ROW 1.54 COL 101
     RECT-4 AT ROW 8.31 COL 100.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 22.08
         BGCOLOR 17 FGCOLOR 0 FONT 4
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F-Presenta
     General AT ROW 1 COL 1.57
     BUTTON-11 AT ROW 10.69 COL 43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 26 ROW 2.88
         SIZE 59 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "".

DEFINE FRAME Meses
     uno-1 AT ROW 1.46 COL 25.86
     uno-7 AT ROW 1.46 COL 41.57
     B1 AT ROW 2.08 COL 3
     uno-2 AT ROW 2.38 COL 25.86
     uno-8 AT ROW 2.38 COL 41.57
     uno-3 AT ROW 3.23 COL 25.86
     uno-9 AT ROW 3.23 COL 41.57
     uno-4 AT ROW 4.12 COL 25.86
     uno-10 AT ROW 4.12 COL 41.57
     BUTTON-4 AT ROW 4.77 COL 5
     uno-5 AT ROW 5.04 COL 25.86
     uno-11 AT ROW 5.04 COL 41.57
     uno-6 AT ROW 6 COL 25.86
     uno-12 AT ROW 6 COL 41.57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 16 ROW 8.81
         SIZE 59 BY 7.27
         BGCOLOR 17 
         TITLE BGCOLOR 17 "Selección de Meses".


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
         TITLE              = "Indicadores Financieros"
         HEIGHT             = 22.08
         WIDTH              = 113.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 15
         FGCOLOR            = 0
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
ASSIGN FRAME F-Presenta:FRAME = FRAME F-Mayor:HANDLE
       FRAME Meses:FRAME = FRAME F-Mayor:HANDLE.

/* SETTINGS FOR FRAME F-Mayor
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME Meses:MOVE-AFTER-TAB-ITEM (Ofi-f:HANDLE IN FRAME F-Mayor)
       XXTABVALXX = FRAME Meses:MOVE-BEFORE-TAB-ITEM (BUTTON-10:HANDLE IN FRAME F-Mayor)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB Listos 1 F-Mayor */
/* BROWSE-TAB B-Indices Ed F-Mayor */
/* BROWSE-TAB B-Indicadores B-Indices F-Mayor */
/* BROWSE-TAB B-Resultado B-Indicadores F-Mayor */
/* SETTINGS FOR BROWSE B-Indicadores IN FRAME F-Mayor
   NO-ENABLE                                                            */
/* SETTINGS FOR BROWSE B-Indices IN FRAME F-Mayor
   NO-ENABLE                                                            */
/* SETTINGS FOR BROWSE B-Resultado IN FRAME F-Mayor
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-10 IN FRAME F-Mayor
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-5 IN FRAME F-Mayor
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME F-Mayor
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-7 IN FRAME F-Mayor
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-8 IN FRAME F-Mayor
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR Ed IN FRAME F-Mayor
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F-Presenta
                                                                        */
/* BROWSE-TAB General 1 F-Presenta */
ASSIGN 
       FRAME F-Presenta:MOVABLE          = TRUE.

ASSIGN 
       General:SEPARATOR-FGCOLOR IN FRAME F-Presenta      = 0.

/* SETTINGS FOR FRAME Meses
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-Indicadores
/* Query rebuild information for BROWSE B-Indicadores
     _TblList          = "bdcentral.Cfg_Varios"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Cfg_Varios.Tipo >= 13
 AND Cfg_Varios.Tipo <= 16"
     _FldNameList[1]   > bdcentral.Cfg_Varios.Descripcion
"Cfg_Varios.Descripcion" "               Indicadores Économicos" ? "character" 15 0 4 17 0 4 no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE B-Indicadores */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-Indices
/* Query rebuild information for BROWSE B-Indices
     _TblList          = "bdcentral.Varios"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Varios.Codigo
"Varios.Codigo" ? ? "integer" ? ? 4 17 0 4 no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > bdcentral.Varios.Descripcion
"Varios.Descripcion" ? ? "character" 15 0 4 17 0 4 no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE B-Indices */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-Resultado
/* Query rebuild information for BROWSE B-Resultado
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH T-Resultado NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE B-Resultado */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE General
/* Query rebuild information for BROWSE General
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TotalInd.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE General */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Listos
/* Query rebuild information for BROWSE Listos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Listos NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Listos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Indicadores Financieros */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Indicadores Financieros */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Anos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anos wWin
ON VALUE-CHANGED OF Anos IN FRAME F-Mayor /* Año */
DO:
  W_AnSel = INTEGER(SELF:SCREEN-VALUE).
  VIEW FRAME Meses.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-Indicadores
&Scoped-define SELF-NAME B-Indicadores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Indicadores wWin
ON MOUSE-SELECT-DBLCLICK OF B-Indicadores IN FRAME F-Mayor
DO:
  OPEN QUERY B-Indices FOR EACH Varios WHERE Cfg_Varios.Tipo = Varios.Tipo NO-LOCK.
  W_Nindice = Varios.Tipo.
  FOR EACH Listos:
   DELETE Listos.
  END.
  FOR EACH TotalInd:
   DELETE TotalInd.
  END.
  FOR EACH Varios WHERE Varios.Tipo = W_Nindice:
   CREATE TotalInd.
   ASSIGN TotalInd.Cod = Varios.Codigo.
  END.
  FOR EACH Varios WHERE Varios.Tipo = W_Nindice:
   CREATE Listos.
   ASSIGN Listos.Cod = Varios.Codigo.
  END.
  RUN Generar.
  ENABLE B-Indices Button-6 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-Indices
&Scoped-define SELF-NAME B-Indices
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Indices wWin
ON MOUSE-SELECT-DBLCLICK OF B-Indices IN FRAME F-Mayor
DO:
  DEFINE VAR W_CodVar LIKE Varios.Codigo.
  W_CodVar = INTEGER(Varios.Codigo:SCREEN-VALUE IN BROWSE B-Indices).
  FIND FIRST Varios WHERE W_Nindice = Varios.Tipo AND W_CodVar = Varios.Codigo AND Varios.Estado = 1 NO-LOCK NO-ERROR.
  IF AVAILABLE Varios THEN DO:
    FIND FIRST Listos WHERE Listos.Cod = Varios.Codigo NO-LOCK NO-ERROR.
    FIND FIRST Programas WHERE Programas.Programa = Varios.Programa NO-LOCK NO-ERROR.
    IF AVAILABLE Programas THEN 
     IF SEARCH(Programas.Ejecutable) EQ ? THEN
        ASSIGN Listos.OK = '  '.     
     ELSE DO:
      DO W_ConAux = 1 TO 12:
      IF Mano[W_ConAux] = YES THEN DO:
       RUN VALUE(Programas.Ejecutable) (INPUT W_Ofini,INPUT W_Offin,INPUT W_ConAux ,INPUT W_AnSel, OUTPUT W_Operador1 ,OUTPUT W_Operador2 ,OUTPUT W_Operando ,OUTPUT W_Cuenta ,OUTPUT W_Resultado).
       FIND T-Resultado WHERE T-Resultado.Tipo = W_Nindice AND T-Resultado.Indice = W_CodVar AND T-Resultado.Mes = W_ConAux AND T-Resultado.Ano = W_AnSel NO-ERROR.
       IF NOT AVAILABLE T-Resultado THEN
          CREATE T-Resultado.
       ASSIGN T-Resultado.Indicador = Varios.Descripcion:SCREEN-VALUE IN BROWSE B-Indices
           T-Resultado.Formula = W_Cuenta 
           T-Resultado.Tipo = W_Nindice
           T-Resultado.Indice = W_CodVar
           T-Resultado.Resultado = TRIM(W_Operador1) + ' ' + TRIM(W_Operando) + ' ' + TRIM(W_Operador2)
           T-Resultado.Mes = W_ConAux
           T-Resultado.Ano = W_AnSel
           T-Resultado.Comentario = Varios.Comentario
           T-Resultado.AgIni = W_Ofini
           T-Resultado.AgFin = W_Offin
           T-Resultado.Operador1 = DECIMAL(W_Operador1)
           T-Resultado.Operador2 = DECIMAL(W_Operador2)
           T-Resultado.Porcen = DECIMAL(W_Resultado)
           T-Resultado.Operando = TRIM(W_Operando).
        ASSIGN Listos.OK = 'Ok'.    
      END.
     END.
     OPEN QUERY Listos FOR EACH Listos NO-LOCK. 
     OPEN QUERY B-Resultado FOR EACH T-Resultado NO-LOCK. 
     ENABLE B-Resultado WITH FRAME {&FRAME-NAME}.
   END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-Resultado
&Scoped-define SELF-NAME B-Resultado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Resultado wWin
ON MOUSE-SELECT-DBLCLICK OF B-Resultado IN FRAME F-Mayor
DO:
  Ed:SCREEN-VALUE = T-Resultado.Comentario.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Meses
&Scoped-define SELF-NAME B1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B1 wWin
ON CHOOSE OF B1 IN FRAME Meses /* Año Completo */
DO:
 DEFINE VARIABLE rta AS LOGICAL.
 rta = uno-1:INPUT-VALUE.
  IF rta = YES THEN
   uno-1:SCREEN-VALUE = 'no'.
  ELSE
   uno-1:SCREEN-VALUE = 'yes'.
 Mano[1] = uno-1:INPUT-VALUE. 
 rta = uno-2:INPUT-VALUE.
 IF rta = YES THEN
   uno-2:SCREEN-VALUE = 'no'.
  ELSE
   uno-2:SCREEN-VALUE = 'yes'.
 Mano[2] = uno-2:INPUT-VALUE.  
 rta = uno-3:INPUT-VALUE.
 IF rta = YES THEN
   uno-3:SCREEN-VALUE = 'no'.
  ELSE
   uno-3:SCREEN-VALUE = 'yes'.
 Mano[3] = uno-3:INPUT-VALUE.  
 rta = uno-4:INPUT-VALUE.
 IF rta = YES THEN
   uno-4:SCREEN-VALUE = 'no'.
  ELSE
   uno-4:SCREEN-VALUE = 'yes'.
 Mano[4] = uno-4:INPUT-VALUE.  
 rta = uno-5:INPUT-VALUE.
 IF rta = YES THEN
   uno-5:SCREEN-VALUE = 'no'.
  ELSE
   uno-5:SCREEN-VALUE = 'yes'.
 Mano[5] = uno-5:INPUT-VALUE.  
 rta = uno-6:INPUT-VALUE.
 IF rta = YES THEN
   uno-6:SCREEN-VALUE = 'no'.
  ELSE
   uno-6:SCREEN-VALUE = 'yes'.
 Mano[6] = uno-6:INPUT-VALUE.  
 rta = uno-7:INPUT-VALUE.
 IF rta = YES THEN
   uno-7:SCREEN-VALUE = 'no'.
  ELSE
   uno-7:SCREEN-VALUE = 'yes'.
 rta = uno-8:INPUT-VALUE.
 Mano[7] = uno-7:INPUT-VALUE.
 IF rta = YES THEN
   uno-8:SCREEN-VALUE = 'no'.
  ELSE
   uno-8:SCREEN-VALUE = 'yes'.
 Mano[8] = uno-8:INPUT-VALUE.  
 rta = uno-9:INPUT-VALUE.
 IF rta = YES THEN
   uno-9:SCREEN-VALUE = 'no'.
  ELSE
   uno-9:SCREEN-VALUE = 'yes'.
 Mano[9] = uno-9:INPUT-VALUE.  
 rta = uno-10:INPUT-VALUE.
 IF rta = YES THEN
   uno-10:SCREEN-VALUE = 'no'.
  ELSE
   uno-10:SCREEN-VALUE = 'yes'.
 Mano[10] = uno-10:INPUT-VALUE.  
 rta = uno-11:INPUT-VALUE.
 IF rta = YES THEN
   uno-11:SCREEN-VALUE = 'no'.
  ELSE
   uno-11:SCREEN-VALUE = 'yes'.
 Mano[11] = uno-11:INPUT-VALUE.  
 rta = uno-12:INPUT-VALUE.
 IF rta = YES THEN
   uno-12:SCREEN-VALUE = 'no'.
  ELSE
   uno-12:SCREEN-VALUE = 'yes'.
 Mano[12] = uno-12:INPUT-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Mayor
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F-Mayor /* Salir */
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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F-Mayor /* Button 1 */
DO:
  RUN w-infdia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Presenta
&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 wWin
ON CHOOSE OF BUTTON-11 IN FRAME F-Presenta /* Button 11 */
DO:
  HIDE FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Mayor
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F-Mayor /* Button 2 */
DO:
   DEFINE VAR Listado AS CHARACTER INITIAL "".
   
   Listado = W_PathSpl + "Linver.LST".
  {Incluido\Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Meses
&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON CHOOSE OF BUTTON-4 IN FRAME Meses /* Button 4 */
DO:
  HIDE FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Mayor
&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 wWin
ON CHOOSE OF BUTTON-5 IN FRAME F-Mayor /* Button 5 */
DO:
  VIEW FRAME F-CINV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 wWin
ON CHOOSE OF BUTTON-6 IN FRAME F-Mayor /* Todos */
DO:
  DEFINE VAR Cont AS INTEGER INITIAL 0.  
  OPEN QUERY B-Indices FOR EACH Varios WHERE Cfg_Varios.Tipo = Varios.Tipo NO-LOCK.
  FOR EACH Varios WHERE Cfg_Varios.Tipo = Varios.Tipo NO-LOCK:  
    Cont = Cont + 1.
  END.
  Cont = Cont + 1.
  FIND FIRST Varios NO-LOCK NO-ERROR.
  GET FIRST B-Indices.
  DO WHILE Cont NE 0: 
    APPLY 'Mouse-Select-Dblclick' TO B-Indices. 
    REPOSITION B-Indices FORWARDS -1.    
    GET NEXT B-Indices.
    cont = cont - 1.    
  END.
  MESSAGE "Proceso Terminado."
   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON CHOOSE OF BUTTON-9 IN FRAME F-Mayor /* Borrar */
DO:
  FOR EACH T-Resultado:
   DELETE T-Resultado.
  END.
  FOR EACH Listos:
   ASSIGN Listos.OK = '  '.
  END.
  OPEN QUERY B-Resultado FOR EACH T-Resultado NO-LOCK. 
  OPEN QUERY Listos FOR EACH Listos NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ofi-f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ofi-f wWin
ON VALUE-CHANGED OF Ofi-f IN FRAME F-Mayor /* Oficina Final */
DO:
  W_Offin = INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,3,'CHARACTER')).
  IF W_Offin < W_Ofini THEN DO:
   MESSAGE 'Cambie de Oficina...'.
   RETURN NO-APPLY.
  END.
  ENABLE B-Indicadores WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ofi-i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ofi-i wWin
ON VALUE-CHANGED OF Ofi-i IN FRAME F-Mayor /* Oficina Inicial */
DO:
  W_Ofini = INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,3,'CHARACTER')).
  IF W_Ofini = 0 THEN
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Meses
&Scoped-define SELF-NAME uno-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-1 wWin
ON VALUE-CHANGED OF uno-1 IN FRAME Meses /* Enero */
DO:
  Mano[1] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-10 wWin
ON VALUE-CHANGED OF uno-10 IN FRAME Meses /* Octubre */
DO:
    Mano[10] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-11 wWin
ON VALUE-CHANGED OF uno-11 IN FRAME Meses /* Noviembre */
DO:
    Mano[11] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-12 wWin
ON VALUE-CHANGED OF uno-12 IN FRAME Meses /* Diciembre */
DO:
    Mano[12] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-2 wWin
ON VALUE-CHANGED OF uno-2 IN FRAME Meses /* Febrero */
DO:
    Mano[2] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-3 wWin
ON VALUE-CHANGED OF uno-3 IN FRAME Meses /* Marzo */
DO:
    Mano[3] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-4 wWin
ON VALUE-CHANGED OF uno-4 IN FRAME Meses /* Abril */
DO:
    Mano[4] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-5 wWin
ON VALUE-CHANGED OF uno-5 IN FRAME Meses /* Mayo */
DO:
    Mano[5] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-6 wWin
ON VALUE-CHANGED OF uno-6 IN FRAME Meses /* Junio */
DO:
    Mano[6] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-7 wWin
ON VALUE-CHANGED OF uno-7 IN FRAME Meses /* Julio */
DO:
    Mano[7] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-8 wWin
ON VALUE-CHANGED OF uno-8 IN FRAME Meses /* Agosto */
DO:
    Mano[8] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME uno-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL uno-9 wWin
ON VALUE-CHANGED OF uno-9 IN FRAME Meses /* Septiembre */
DO:
    Mano[9] = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Mayor
&Scoped-define BROWSE-NAME B-Indicadores
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Camels wWin 
PROCEDURE Camels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH T-Resultado:
  FIND FIRST TotalInd WHERE TotalInd.Cod = T-Resultado.Indice NO-LOCK NO-ERROR.
  IF AVAILABLE TotalInd THEN DO:
    ASSIGN TotalInd.Indicador = T-Resultado.Indicador
           TotalInd.Porc = STRING(T-Resultado.Porcen,'->>>9.99').
       W_TotCal = W_TotCal + 1.
    CASE T-Resultado.Indice: 
      WHEN 1 THEN DO:
         IF T-Resultado.Porcen > 1.333 THEN DO:
          ASSIGN totalInd.Calf = '1'.
          W_Cal = W_Cal + 1.
         END.
         IF T-Resultado.Porcen LE 1.333 AND T-Resultado.Porcen GE 1.222 THEN DO:
           ASSIGN totalInd.Calf = '2'.
           W_Cal = W_Cal + 2.
         END.
         IF T-Resultado.Porcen < 1.222 AND T-Resultado.Porcen GE 1.111 THEN DO:
          ASSIGN totalInd.Calf = '3'.
          W_Cal = W_Cal + 3.
         END.
         IF T-Resultado.Porcen < 1.111 AND T-Resultado.Porcen GE 1 THEN DO:
          ASSIGN totalInd.Calf = '4'.
          W_Cal = W_Cal + 4.
         END.
         IF T-Resultado.Porcen < 1 THEN DO:
          ASSIGN totalInd.Calf = '5'.
          W_Cal = W_Cal + 5.
         END.
      END.
      WHEN 2 THEN DO:
         IF T-Resultado.Porcen < 5 THEN DO:
           W_Cal = W_Cal + 1.
           ASSIGN totalInd.Calf = '1'.
         END.
         IF T-Resultado.Porcen GE 5 AND T-Resultado.Porcen LE 8 THEN DO:
           W_Cal = W_Cal + 2.
           ASSIGN totalInd.Calf = '2'.
         END.
         IF T-Resultado.Porcen > 8 AND T-Resultado.Porcen LE 9 THEN DO:
           W_Cal = W_Cal + 3.
           ASSIGN totalInd.Calf = '3'.
         END.
         IF T-Resultado.Porcen > 9 AND T-Resultado.Porcen LE 10 THEN DO:
           W_Cal = W_Cal + 4. 
           ASSIGN totalInd.Calf = '4'.
         END.
         IF T-Resultado.Porcen > 10 THEN DO:
           ASSIGN totalInd.Calf = '5'.
           W_Cal = W_Cal + 5.
         END.
      END.
      WHEN 3 THEN DO:
         IF T-Resultado.Porcen > 4 THEN DO:
           W_Cal = W_Cal + 1.
           ASSIGN totalInd.Calf = '1'.
         END.
         IF T-Resultado.Porcen LE 4 AND T-Resultado.Porcen GE 2 THEN DO:
           W_Cal = W_Cal + 2.
           ASSIGN totalInd.Calf = '2'.
         END.
         IF T-Resultado.Porcen < 2 AND T-Resultado.Porcen GE -5 THEN DO:
           W_Cal = W_Cal + 3.
           ASSIGN totalInd.Calf = '3'.
         END.
         IF T-Resultado.Porcen < -5 AND T-Resultado.Porcen GE -25 THEN DO:
           W_Cal = W_Cal + 4.
           ASSIGN totalInd.Calf = '4'.
         END.
         IF T-Resultado.Porcen < -25 THEN DO:
           W_Cal = W_Cal + 5.
           ASSIGN totalInd.Calf = '5'.
         END.
      END.
     WHEN 4 THEN 
         CASE INTEGER(T-Resultado.Operador2):
             WHEN 1 THEN DO:
               W_Cal = W_Cal + 1.
               ASSIGN totalInd.Calf = '1'.
             END.
             WHEN 2 THEN DO:
               W_Cal = W_Cal + 2.
               ASSIGN totalInd.Calf = '2'.
             END.
             WHEN 3 THEN DO:
               W_Cal = W_Cal + 3.
               ASSIGN totalInd.Calf = '3'.
             END.
             WHEN 4 THEN DO:
               W_Cal = W_Cal + 4.
               ASSIGN totalInd.Calf = '4'.
             END.
             WHEN 5 THEN DO:
               W_Cal = W_Cal + 5.
               ASSIGN totalInd.Calf = '5'.
             END.
      END CASE.
      WHEN 5 THEN DO:
         IF T-Resultado.Porcen > 175 THEN DO:
           W_Cal = W_Cal + 1.
           ASSIGN totalInd.Calf = '1'.
         END.
         IF T-Resultado.Porcen LE 175 AND T-Resultado.Porcen GE 140 THEN DO:
           W_Cal = W_Cal + 2.
           ASSIGN totalInd.Calf = '2'.
         END.
         IF T-Resultado.Porcen < 140 AND T-Resultado.Porcen GE 125 THEN DO:
           W_Cal = W_Cal + 3.
           ASSIGN totalInd.Calf = '3'.
         END.
         IF T-Resultado.Porcen < 125 AND T-Resultado.Porcen GE 120 THEN DO:
           W_Cal = W_Cal + 4.
           ASSIGN totalInd.Calf = '4'.
         END.
         IF T-Resultado.Porcen < 120 THEN DO:
           W_Cal = W_Cal + 5.
           ASSIGN totalInd.Calf = '5'.
         END.
      END.
     END CASE.
   END.
   ELSE
    MESSAGE 'no encontrado' T-Resultado.Indice.
END.
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
  DISPLAY Anos Ofi-i Ofi-f Ed 
      WITH FRAME F-Mayor IN WINDOW wWin.
  ENABLE Listos Anos Ofi-i Ofi-f BUTTON-9 BtnDone BUTTON-1 BUTTON-2 RECT-2 
         RECT-4 
      WITH FRAME F-Mayor IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Mayor}
  ENABLE General BUTTON-11 
      WITH FRAME F-Presenta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Presenta}
  DISPLAY uno-1 uno-7 uno-2 uno-8 uno-3 uno-9 uno-4 uno-10 uno-5 uno-11 uno-6 
          uno-12 
      WITH FRAME Meses IN WINDOW wWin.
  ENABLE uno-1 uno-7 B1 uno-2 uno-8 uno-3 uno-9 uno-4 uno-10 BUTTON-4 uno-5 
         uno-11 uno-6 uno-12 
      WITH FRAME Meses IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-Meses}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar wWin 
PROCEDURE Generar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
W_Cal = 0.
W_PunQui = 0.
W_TotCal = 0.
APPLY 'choose' TO button-6 IN FRAME {&FRAME-NAME}.
FIND FIRST Programas WHERE Programas.Programa = Cfg_Varios.Programa NO-LOCK NO-ERROR.
IF AVAILABLE Programas THEN DO:
  IF SEARCH(Programas.Ejecutable) NE ? THEN 
    RUN camels.
END.
  ELSE DO:
   FOR EACH T-Resultado:
     FIND FIRST TotalInd WHERE TotalInd.Cod = T-Resultado.Indice NO-LOCK NO-ERROR.
     IF AVAILABLE TotalInd THEN DO:
       ASSIGN TotalInd.Indicador = T-Resultado.Indicador
              TotalInd.Porc = STRING(T-Resultado.Porcen,'->>>>>9.99').
       W_TotCal = W_TotCal + T-Resultado.Porcen.
       W_Cal = W_Cal + 1.
       CASE T-Resultado.Indice:
           WHEN 1 THEN
             W_RenAct = T-Resultado.Porcen.
           WHEN 2 THEN
             W_CarVen = T-Resultado.Porcen.
           WHEN 3 THEN
             W_MarBru = T-Resultado.Porcen.
           WHEN 4 THEN
             W_PatAct = T-Resultado.Porcen.
           WHEN 5 THEN
             W_DisAct = T-Resultado.Porcen.
       END CASE.
     END.
   END.
   RUN Mat(OUTPUT W_PunQui).
   W_TotFin = W_TotCal / W_Cal.
END.
CREATE TotalInd.  
  IF W_Cal NE 0 THEN
   W_TotFin = W_Cal / W_TotCal.
  ELSE
   W_TotFin = 0. 


CREATE TotalInd.
ASSIGN TotalInd.Indicador = FILL(' ',20) + 'Calificacion Total'
       TotalInd.Porc = STRING(W_TotFin,'->>>>>9.99').
 IF W_PunQui NE 0  THEN DO:
   CREATE TotalInd.
   CREATE TotalInd.
   IF W_PunQui <= 20 THEN
     ASSIGN TotalInd.Indicador = 'Probabilidad Baja de Quiebra'.
    ELSE
     IF W_PunQui <= 70 THEN
       ASSIGN TotalInd.Indicador = 'Probabilidad Media de Quiebra'.
      ELSE 
       ASSIGN TotalInd.Indicador = 'Probabilidad Alta de Quiebra'.

   ASSIGN TotalInd.Porc = STRING(W_PunQui,'->>>>>9.99').
 END.
  OPEN QUERY General FOR EACH TotalInd NO-LOCK. 
  FRAME F-Presenta:TITLE = Cfg_Varios.Descripcion.
  VIEW FRAME F-Presenta.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
/*------------------------------------------------------------------------------
  Purpose: Envia A Excel Los Datos Necesarios Para el Informe.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 7.
 E_Fila      = "007" + "AgenIni"
             + "007" + "AgenFin"
             + "020" + "Formula             "
             + "020" + "Numerador           "   
             + "009" + "Operación"
             + "020" + "Denominador         "
             + "010" + "Porcentaje".  
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

  FOR EACH T-Resultado NO-LOCK:
      E_Fila2     = "".
      E_Fila2     = "007" + STRING(T-Resultado.AgIni,"999") + '    '
                  + "007" + STRING(T-Resultado.AgFin,"999") + '    '
                  + "020" + STRING(T-Resultado.Formula,"X(20)")
                  + "020" + STRING(T-Resultado.Operador1,'->>>>>>>>>>>>>999999')
                  + "009" + STRING(TRIM(T-Resultado.Operando),'X') + FILL(' ',8)
                  + "020" + STRING(T-Resultado.Operador2,'->>>>>>>>>>>>>999999')
                  + "010" + STRING(T-Resultado.Porcen,"->>>>>.99") + '%'.                 
{Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  OPEN QUERY B-Indices FOR EACH Varios WHERE Varios.Tipo = 0 NO-LOCK.
  FOR EACH Agencia BY Agencia:
   Ofi-i:ADD-LAST(STRING(Agencia,'999') + ' ' + TRIM(Agencia.Nombre)) IN FRAME {&FRAME-NAME}.
  END.
  
 FOR EACH Agencia BY Agencia:
   Ofi-f:ADD-LAST(STRING(Agencia,'999') + ' ' + TRIM(Agencia.Nombre)) IN FRAME {&FRAME-NAME}.
  END.
  DO W_ConAux = YEAR(W_Fecha) TO 1995 BY -1:  
   Anos:ADD-LAST(STRING(W_ConAux,'9999')) IN FRAME {&FRAME-NAME}.
  END.
  
  DISABLE ALL EXCEPT Ofi-i Ofi-f Anos.
  HIDE FRAME Meses.
  HIDE FRAME F-Presenta.
 
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mat wWin 
PROCEDURE Mat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER W_ResMat AS DECIMAL.

W_ResMat = (EXP(-0.87296791 + (-40.17984251 * (W_RenAct + W_CarVen) - (0.00044593 * W_MarBru) - (2.00465396 * W_PatAct) - (3.82778115 * W_DisAct)),2) / (1 + EXP(-0.87296791 + ((-40.17984251 * W_RenAct) + (0.005055825 * W_CarVen) - (0.00044593 * W_MarBru) - (2.00465396 * W_PatAct) - (3.82778115 * W_DisAct)),2))) / 100.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
  DEFINE VARIABLE ti AS INTEGER INITIAL 0.
  DEFINE VARIABLE NomCompleto AS CHARACTER.
  
  W_Reporte    = "REPORTE   : INDICADORES FINANCIEROS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "Of.Ini Of.Fin Formula               Numerador           Oper     Denominador           Porcen Año".
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  
  FOR EACH T-Resultado BY T-Resultado.Tipo BY T-Resultado.Indice:
    IF ti NE T-Resultado.Indice THEN DO:
     ti = T-Resultado.Indice.
     FIND FIRST Varios WHERE Cfg_Varios.Tipo = Varios.Tipo AND Varios.Codigo = ti NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN
       NomCompleto = STRING(Varios.Codigo,'99999') + ' ' + SUBSTRING(Varios.Descripcion,1,30).
      ELSE
       NomCompleto = STRING(Ti,'99999') + ' Indice No Existe...'. 
     DISPLAY NomCompleto AT 1 NO-LABEL FORMAT "X(50)".  
    END.
    DISPLAY  AgIni           AT 2   NO-LABEL FORMAT "999"
             AgFin           AT 9   NO-LABEL FORMAT "999"
             Formula         AT 14  NO-LABEL FORMAT 'X(20)'
             Operador1       AT 36  NO-LABEL FORMAT '->>>,>>>,>>>,>99,999'
             Operando        AT 58  NO-LABEL FORMAT 'X'
             Operador2       AT 60  NO-LABEL FORMAT '->>>,>>>,>>>,>99,999'
             '='             AT 82  NO-LABEL FORMAT 'X'
             Porcen          AT 84  NO-LABEL FORMAT "->>>9.99"
             T-Resultado.Ano AT 94  NO-LABEL FORMAT "9999"
      WITH FRAME F-reporte DOWN WIDTH 132 USE-TEXT STREAM-IO NO-BOX.
      DOWN WITH FRAME F-reporte.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

