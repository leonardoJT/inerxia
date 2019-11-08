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

  

DEFINE VAR W_New AS LOGICAL INITIAL NO.
DEFINE VAR Puntero AS ROWID.

DEFINE VAR li AS INTEGER FORMAT 9.
DEFINE VAR lf AS INTEGER FORMAT 9.
DEFINE VARIABLE W_ok AS LOGICAL.
DEFINE SHARED VAR W_Nom_Agencia    AS   CHARACTER FORMAT "X(60)".
DEFINE SHARED VAR W_Nom_Entidad    AS   CHARACTER FORMAT "X(60)".
DEFINE SHARED VAR W_Usuario        LIKE Usuarios.Usuario.
DEFINE SHARED VAR W_Estacion       LIKE Estaciones.Estacion.
DEFINE SHARED VAR W_Manija         AS   HANDLE.
DEFINE SHARED VAR W_PathSpl        LIKE Entidad.Dir_Spl.

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
&Scoped-define BROWSE-NAME B_PUB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Ind_PUB

/* Definitions for BROWSE B_PUB                                         */
&Scoped-define FIELDS-IN-QUERY-B_PUB Ind_PUB.Codigo Ind_PUB.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_PUB   
&Scoped-define SELF-NAME B_PUB
&Scoped-define QUERY-STRING-B_PUB FOR EACH Ind_PUB BY Ind_PUB.Codigo INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_PUB OPEN QUERY {&SELF-NAME} FOR EACH Ind_PUB BY Ind_PUB.Codigo INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_PUB Ind_PUB
&Scoped-define FIRST-TABLE-IN-QUERY-B_PUB Ind_PUB


/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain Ind_PUB.Ind_Prg Ind_PUB.Codigo ~
Ind_PUB.Fec_Ingreso Ind_PUB.Fec_Retiro Ind_PUB.Nombre Ind_PUB.Prg ~
Ind_PUB.Usuario Ind_PUB.Estado Ind_PUB.Unidad Ind_PUB.Tipo Ind_PUB.Campo ~
Ind_PUB.Id_Bueno Ind_PUB.Id_Exitoso Ind_PUB.Id_Malo Ind_PUB.Id_Regular ~
Ind_PUB.Mi Ind_PUB.Mf Ind_PUB.Ri Ind_PUB.Rf Ind_PUB.Bi Ind_PUB.Bf ~
Ind_PUB.Ei Ind_PUB.Ef 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain Ind_PUB.Ind_Prg Ind_PUB.Codigo ~
Ind_PUB.Nombre Ind_PUB.Prg Ind_PUB.Estado Ind_PUB.Unidad Ind_PUB.Tipo ~
Ind_PUB.Campo Ind_PUB.Id_Bueno Ind_PUB.Id_Exitoso Ind_PUB.Id_Malo ~
Ind_PUB.Id_Regular Ind_PUB.Mi Ind_PUB.Mf Ind_PUB.Ri Ind_PUB.Rf Ind_PUB.Bi ~
Ind_PUB.Bf Ind_PUB.Ei Ind_PUB.Ef 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain Ind_PUB
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain Ind_PUB
&Scoped-define QUERY-STRING-fMain FOR EACH Ind_PUB SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH Ind_PUB SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain Ind_PUB
&Scoped-define FIRST-TABLE-IN-QUERY-fMain Ind_PUB


/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-B_PUB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Ind_PUB.Ind_Prg Ind_PUB.Codigo Ind_PUB.Nombre ~
Ind_PUB.Prg Ind_PUB.Estado Ind_PUB.Unidad Ind_PUB.Tipo Ind_PUB.Campo ~
Ind_PUB.Id_Bueno Ind_PUB.Id_Exitoso Ind_PUB.Id_Malo Ind_PUB.Id_Regular ~
Ind_PUB.Mi Ind_PUB.Mf Ind_PUB.Ri Ind_PUB.Rf Ind_PUB.Bi Ind_PUB.Bf ~
Ind_PUB.Ei Ind_PUB.Ef 
&Scoped-define ENABLED-TABLES Ind_PUB
&Scoped-define FIRST-ENABLED-TABLE Ind_PUB
&Scoped-Define ENABLED-OBJECTS CPerspectiva BUTTON-61 EDes BUTTON-49 ~
BUTTON-1 BUTTON-2 B_CrSalvar B_CRIngresar B_CRCancelar B_CRBorrar BtnDone ~
RECT-301 RECT-302 RECT-303 RECT-304 RECT-305 RECT-306 RECT-307 RECT-309 ~
RECT-310 RECT-311 RECT-312 RECT-313 RECT-314 RECT-315 RECT-316 RECT-317 
&Scoped-Define DISPLAYED-FIELDS Ind_PUB.Ind_Prg Ind_PUB.Codigo ~
Ind_PUB.Fec_Ingreso Ind_PUB.Fec_Retiro Ind_PUB.Nombre Ind_PUB.Prg ~
Ind_PUB.Usuario Ind_PUB.Estado Ind_PUB.Unidad Ind_PUB.Tipo Ind_PUB.Campo ~
Ind_PUB.Id_Bueno Ind_PUB.Id_Exitoso Ind_PUB.Id_Malo Ind_PUB.Id_Regular ~
Ind_PUB.Mi Ind_PUB.Mf Ind_PUB.Ri Ind_PUB.Rf Ind_PUB.Bi Ind_PUB.Bf ~
Ind_PUB.Ei Ind_PUB.Ef 
&Scoped-define DISPLAYED-TABLES Ind_PUB
&Scoped-define FIRST-DISPLAYED-TABLE Ind_PUB
&Scoped-Define DISPLAYED-OBJECTS CPerspectiva EDes NomUsuario 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 EDes Ind_PUB.Ind_Prg Ind_PUB.Codigo Ind_PUB.Nombre ~
Ind_PUB.Prg Ind_PUB.Estado Ind_PUB.Unidad Ind_PUB.Tipo Ind_PUB.Campo ~
Ind_PUB.Id_Bueno Ind_PUB.Id_Exitoso Ind_PUB.Id_Malo Ind_PUB.Id_Regular ~
Ind_PUB.Mi Ind_PUB.Mf Ind_PUB.Ri Ind_PUB.Rf Ind_PUB.Bi Ind_PUB.Bf ~
Ind_PUB.Ei Ind_PUB.Ef 
&Scoped-define List-2 Ind_PUB.Mi Ind_PUB.Mf Ind_PUB.Ri Ind_PUB.Rf ~
Ind_PUB.Bi Ind_PUB.Bf Ind_PUB.Ei Ind_PUB.Ef 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-62 
     LABEL "Salvar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-63 
     LABEL "Revizar Sintaxis" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-64 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE Eprg AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 80 BY 11.58
     BGCOLOR 11 FONT 2 NO-UNDO.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

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

DEFINE BUTTON BUTTON-61 
     LABEL "Ver Codigo de este Indicador" 
     SIZE 25 BY 1.12.

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

DEFINE VARIABLE CPerspectiva AS CHARACTER FORMAT "X(256)":U INITIAL "00 - Sin perspectiva matriculada" 
     LABEL "Perspectiva" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00 - Sin perspectiva matriculada" 
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE EDes AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 34 BY 2.38
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomUsuario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 62 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-301
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 1.35.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 34 BY 1.85.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 34 BY 1.62.

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 34 BY 2.15.

DEFINE RECTANGLE RECT-305
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 16 BY 1.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-306
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 16 BY 1.35
     BGCOLOR 4 .

DEFINE RECTANGLE RECT-307
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 16 BY 1.31
     BGCOLOR 14 .

DEFINE RECTANGLE RECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 16 BY 1.35
     BGCOLOR 10 .

DEFINE RECTANGLE RECT-310
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 12 BY 1.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-311
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 10 BY 1.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-312
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 12 BY 1.35
     BGCOLOR 4 .

DEFINE RECTANGLE RECT-313
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 10 BY 1.35
     BGCOLOR 4 .

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 12 BY 1.35
     BGCOLOR 14 .

DEFINE RECTANGLE RECT-315
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 10 BY 1.35
     BGCOLOR 14 .

DEFINE RECTANGLE RECT-316
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 12 BY 1.35
     BGCOLOR 10 .

DEFINE RECTANGLE RECT-317
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 10 BY 1.35
     BGCOLOR 10 .

DEFINE BUTTON BUTTON-60 
     LABEL "Ocultar" 
     SIZE 10 BY 1.12.

DEFINE VARIABLE CPerspectiva2 AS CHARACTER FORMAT "X(256)":U INITIAL "00 - Todas las perspectivas" 
     LABEL "Filtro x Perspectiva" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00 - Todas las perspectivas" 
     DROP-DOWN-LIST
     SIZE 26 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE REstado AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE RTipos AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impacto", 1,
"Eficiencia", 2,
"Eficacia", 3,
"Todos", 4
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-326
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47 BY 1.62.

DEFINE RECTANGLE RECT-327
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_PUB FOR 
      Ind_PUB SCROLLING.

DEFINE QUERY fMain FOR 
      Ind_PUB SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_PUB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_PUB wWin _FREEFORM
  QUERY B_PUB NO-LOCK DISPLAY
      Ind_PUB.Codigo Ind_PUB.Nombre
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 48 BY 11.58
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .42 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     CPerspectiva AT ROW 2.88 COL 58 COLON-ALIGNED
     BUTTON-61 AT ROW 15.81 COL 13
     EDes AT ROW 12.69 COL 54 NO-LABEL
     Ind_PUB.Ind_Prg AT ROW 15.5 COL 59
          LABEL "Usa programa?"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .77
     NomUsuario AT ROW 18.23 COL 23 COLON-ALIGNED NO-LABEL
     BUTTON-49 AT ROW 2.08 COL 97
     BUTTON-1 AT ROW 4 COL 97
     BUTTON-2 AT ROW 5.62 COL 97
     B_CrSalvar AT ROW 10.15 COL 97
     C_CRDeshacer AT ROW 11.77 COL 97
     B_CRIngresar AT ROW 13.38 COL 97
     B_CRCancelar AT ROW 15 COL 97
     B_CRBorrar AT ROW 16.62 COL 97
     BtnDone AT ROW 18.23 COL 97
     Ind_PUB.Codigo AT ROW 3.69 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          BGCOLOR 15 
     Ind_PUB.Fec_Ingreso AT ROW 4.77 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ind_PUB.Fec_Retiro AT ROW 5.85 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ind_PUB.Nombre AT ROW 4.77 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY .81
          BGCOLOR 15 
     Ind_PUB.Prg AT ROW 16.31 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25 BY .81
          BGCOLOR 15 
     Ind_PUB.Usuario AT ROW 18.23 COL 19 COLON-ALIGNED
          LABEL "Usuario"
          VIEW-AS FILL-IN 
          SIZE 4.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Ind_PUB.Estado AT ROW 3.69 COL 17 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 16 BY .81
     Ind_PUB.Unidad AT ROW 10.65 COL 60 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Valores":U, 1,
"Cantidades":U, 2
          SIZE 26 BY .62
     Ind_PUB.Tipo AT ROW 8.27 COL 56 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Impacto":U, 1,
"Eficiencia":U, 2,
"Eficacia":U, 3
          SIZE 30 BY .81
     Ind_PUB.Campo AT ROW 5.85 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY .81
          BGCOLOR 15 
     Ind_PUB.Id_Bueno AT ROW 12.58 COL 9
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .65
     Ind_PUB.Id_Exitoso AT ROW 14.19 COL 9
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .65
     Ind_PUB.Id_Malo AT ROW 9.88 COL 9
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .65
     Ind_PUB.Id_Regular AT ROW 11.23 COL 9
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .65
     Ind_PUB.Mi AT ROW 9.88 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Ind_PUB.Mf AT ROW 9.88 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Ind_PUB.Ri AT ROW 11.23 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Ind_PUB.Rf AT ROW 11.23 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Ind_PUB.Bi AT ROW 12.58 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 19.88
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     Ind_PUB.Bf AT ROW 12.58 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Ind_PUB.Ei AT ROW 13.92 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Ind_PUB.Ef AT ROW 13.92 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     "  Descripción" VIEW-AS TEXT
          SIZE 34 BY .77 AT ROW 11.88 COL 54
          BGCOLOR 18 FGCOLOR 15 
     "HASTA" VIEW-AS TEXT
          SIZE 7 BY .5 AT ROW 8.65 COL 43.57
          FGCOLOR 7 FONT 1
     "RANGO" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 8.54 COL 18
          FGCOLOR 7 FONT 1
     "  Unidad en que trabaja el indicador" VIEW-AS TEXT
          SIZE 34 BY .81 AT ROW 9.62 COL 54
          BGCOLOR 18 FGCOLOR 15 
     " Semaforos para este Indicador expresados en porcentajes" VIEW-AS TEXT
          SIZE 40 BY .81 AT ROW 7.19 COL 12
          BGCOLOR 18 FGCOLOR 15 
     "  Tipo de Indicador" VIEW-AS TEXT
          SIZE 34 BY .77 AT ROW 7.19 COL 54
          BGCOLOR 18 FGCOLOR 15 
     "      Malo" VIEW-AS TEXT
          SIZE 13 BY .85 AT ROW 9.88 COL 15
          BGCOLOR 12 FGCOLOR 15 FONT 1
     "    Regular" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 11.27 COL 15
          BGCOLOR 4 FONT 1
     "     Bueno" VIEW-AS TEXT
          SIZE 12 BY .77 AT ROW 12.65 COL 15
          BGCOLOR 14 FONT 1
     "     Exitoso" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 13.96 COL 15
          BGCOLOR 10 FONT 1
     "DESDE" VIEW-AS TEXT
          SIZE 7 BY .5 AT ROW 8.65 COL 32
          FGCOLOR 7 FONT 1
     RECT-301 AT ROW 17.96 COL 13
     RECT-302 AT ROW 9.88 COL 54
     RECT-303 AT ROW 7.73 COL 54
     RECT-304 AT ROW 15.23 COL 54
     RECT-305 AT ROW 9.62 COL 13
     RECT-306 AT ROW 11 COL 13
     RECT-307 AT ROW 12.38 COL 13
     RECT-309 AT ROW 13.69 COL 13
     RECT-310 AT ROW 9.62 COL 29
     RECT-311 AT ROW 9.62 COL 41
     RECT-312 AT ROW 11 COL 29
     RECT-313 AT ROW 11 COL 41
     RECT-314 AT ROW 12.35 COL 29
     RECT-315 AT ROW 12.35 COL 41
     RECT-316 AT ROW 13.69 COL 29
     RECT-317 AT ROW 13.69 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 19.88
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Consulta
     B_PUB AT ROW 1.27 COL 2
     RTipos AT ROW 13.38 COL 5 NO-LABEL
     BUTTON-60 AT ROW 15 COL 40
     REstado AT ROW 15.15 COL 4 NO-LABEL
     CPerspectiva2 AT ROW 16.62 COL 16 COLON-ALIGNED
     RECT-326 AT ROW 13.12 COL 3
     RECT-327 AT ROW 14.88 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 17 ROW 1.54
         SIZE 52 BY 18.04
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Indicadores PUB".

DEFINE FRAME FCodigo
     Eprg AT ROW 1.54 COL 5 NO-LABEL
     BUTTON-62 AT ROW 13.65 COL 35
     BUTTON-63 AT ROW 13.65 COL 52
     BUTTON-64 AT ROW 13.65 COL 70
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 6 ROW 3.42
         SIZE 87 BY 15.08
         BGCOLOR 17 FONT 4
         TITLE "Codigo de Programación del Indicador".


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
         TITLE              = "Configuración de las INDICADORES PUB"
         HEIGHT             = 19.88
         WIDTH              = 113.72
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
ASSIGN FRAME FCodigo:FRAME = FRAME fMain:HANDLE
       FRAME F_Consulta:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME FCodigo
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FCodigo:HIDDEN           = TRUE
       FRAME FCodigo:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME fMain
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Consulta:MOVE-AFTER-TAB-ITEM (BUTTON-61:HANDLE IN FRAME fMain)
       XXTABVALXX = FRAME F_Consulta:MOVE-BEFORE-TAB-ITEM (EDes:HANDLE IN FRAME fMain)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN Ind_PUB.Bf IN FRAME fMain
   1 2                                                                  */
/* SETTINGS FOR FILL-IN Ind_PUB.Bi IN FRAME fMain
   1 2                                                                  */
/* SETTINGS FOR FILL-IN Ind_PUB.Campo IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN Ind_PUB.Codigo IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR BUTTON C_CRDeshacer IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDes IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN Ind_PUB.Ef IN FRAME fMain
   1 2                                                                  */
/* SETTINGS FOR FILL-IN Ind_PUB.Ei IN FRAME fMain
   1 2                                                                  */
/* SETTINGS FOR RADIO-SET Ind_PUB.Estado IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN Ind_PUB.Fec_Ingreso IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ind_PUB.Fec_Retiro IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Ind_PUB.Id_Bueno IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Ind_PUB.Id_Exitoso IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Ind_PUB.Id_Malo IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Ind_PUB.Id_Regular IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Ind_PUB.Ind_Prg IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Ind_PUB.Mf IN FRAME fMain
   1 2                                                                  */
/* SETTINGS FOR FILL-IN Ind_PUB.Mi IN FRAME fMain
   1 2                                                                  */
/* SETTINGS FOR FILL-IN Ind_PUB.Nombre IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN NomUsuario IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ind_PUB.Prg IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN Ind_PUB.Rf IN FRAME fMain
   1 2                                                                  */
/* SETTINGS FOR FILL-IN Ind_PUB.Ri IN FRAME fMain
   1 2                                                                  */
/* SETTINGS FOR RADIO-SET Ind_PUB.Tipo IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR RADIO-SET Ind_PUB.Unidad IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN Ind_PUB.Usuario IN FRAME fMain
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME F_Consulta
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_PUB RECT-327 F_Consulta */
ASSIGN 
       FRAME F_Consulta:HIDDEN           = TRUE
       FRAME F_Consulta:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_PUB
/* Query rebuild information for BROWSE B_PUB
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Ind_PUB BY Ind_PUB.Codigo INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_PUB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "bdcentral.Ind_PUB"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Configuración de las INDICADORES PUB */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Configuración de las INDICADORES PUB */
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
  ASSIGN li = 1 lf = 3.
  OPEN QUERY B_PUB FOR EACH Ind_PUB WHERE Ind_PUB.Tipo GE li AND
             Ind_PUB.Tipo LE lf AND Ind_PUB.Estado EQ 1
     NO-LOCK BY Ind_PUB.Codigo INDEXED-REPOSITION.
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


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-61
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-61 wWin
ON CHOOSE OF BUTTON-61 IN FRAME fMain /* Ver Codigo de este Indicador */
DO:
  IF Ind_Pub.Ind_Prg THEN DO:
      W_ok = Eprg:READ-FILE(Ind_Pub.Prg) IN FRAME FCodigo.
      W_Ok = Eprg:SEARCH("ind" + STRING(Ind_Pub.Codigo,"999"),33) IN FRAME FCodigo.
  END.
  VIEW FRAME FCodigo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCodigo
&Scoped-define SELF-NAME BUTTON-62
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-62 wWin
ON CHOOSE OF BUTTON-62 IN FRAME FCodigo /* Salvar */
DO:
 w_ok = Eprg:SAVE-FILE(Ind_Pub.Prg) IN FRAME FCodigo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-64
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-64 wWin
ON CHOOSE OF BUTTON-64 IN FRAME FCodigo /* Ocultar */
DO:
  HIDE FRAME FCodigo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME B_CRBorrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRBorrar wWin
ON CHOOSE OF B_CRBorrar IN FRAME fMain /* Borrar */
DO:
  /* ASSIGN Ind_PUB.Fec_Retiro = TODAY
          Ind_PUB.Estado     = 2.*/
    FIND CURRENT Ind_pub.
    DELETE Ind_PUB.
    FIND FIRST Ind_Pub NO-LOCK NO-ERROR.
    IF AVAILABLE Ind_Pub THEN RUN Mostrar_Registro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_CRCancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRCancelar wWin
ON CHOOSE OF B_CRCancelar IN FRAME fMain /* Cancelar */
DO:
  IF Puntero NE ? THEN DO:
     FIND Ind_PUB WHERE ROWID(Ind_PUB) EQ Puntero NO-LOCK NO-ERROR.
     IF AVAILABLE Ind_PUB THEN DO:
        DISPLAY Ind_PUB.Codigo  Ind_PUB.Fec_Ingreso
                Ind_PUB.Fec_Retiro Ind_PUB.Ind_Prg Ind_PUB.Nombre 
                Ind_PUB.Prg Ind_PUB.Usuario Ind_PUB.Unidad
                WITH FRAME FMain.
        EDes:SCREEN-VALUE = Ind_PUB.Descripcion.
        DISABLE Ind_PUB.Codigo WITH FRAME FMain.
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
  IF AVAILABLE Ind_PUB THEN Puntero = ROWID(Ind_PUB).
  ASSIGN Ind_PUB.Codigo:SCREEN-VALUE      = "0"
         Ind_PUB.Fec_Ingreso:SCREEN-VALUE = STRING(TODAY)
         Ind_PUB.Fec_Retiro:SCREEN-VALUE  = "?"
         Ind_PUB.Nombre:SCREEN-VALUE      = ""
         Ind_PUB.Ind_Prg:SCREEN-VALUE     = "NO"
         Ind_PUB.Prg:SCREEN-VALUE         = ""
         EDes:SCREEN-VALUE                = ""
         Ind_PUB.Usuario:SCREEN-VALUE     = "10"
         NomUsuario:SCREEN-VALUE          = W_Usuario
         Ind_PUB.Estado:SCREEN-VALUE      = "1"
         Ind_PUB.Unidad:SCREEN-VALUE      = "1"
         Ind_PUB.Tipo:SCREEN-VALUE        = "1"
         Ind_PUB.Mi:SCREEN-VALUE = "0"
         Ind_PUB.Mf:SCREEN-VALUE = "0"
         Ind_PUB.Ri:SCREEN-VALUE = "0"
         Ind_PUB.Rf:SCREEN-VALUE = "0"
         Ind_PUB.Bi:SCREEN-VALUE = "0"
         Ind_PUB.Bf:SCREEN-VALUE = "0"
         Ind_PUB.Ei:SCREEN-VALUE = "0"
         Ind_PUB.Ef:SCREEN-VALUE = "0"
         Ind_PUB.Id_Malo:SCREEN-VALUE = "No"
         Ind_PUB.Id_Regular:SCREEN-VALUE = "No"
         Ind_PUB.Id_Bueno:SCREEN-VALUE = "No"
         Ind_PUB.Id_Exitoso:SCREEN-VALUE = "No".
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Usuarios THEN NomUsuario:SCREEN-VALUE = Usuarios.Nombre.
  ENABLE {&List-1} WITH FRAME FMain.
  DISABLE {&List-2} WITH FRAME FMain.
  APPLY "entry" TO Ind_PUB.Codigo.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_CrSalvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CrSalvar wWin
ON CHOOSE OF B_CrSalvar IN FRAME fMain /* Salvar */
DO: 
RUN Verificar_Datos.
ASSIGN FRAME FMain CPerspectiva.
IF W_New THEN DO:
   FIND Ind_PUB WHERE
        Ind_PUB.Codigo EQ INTEGER(Ind_PUB.Codigo:SCREEN-VALUE) NO-ERROR.
   IF AVAILABLE Ind_PUB THEN DO:
      MESSAGE "Ya existe una Código Ind_PUB con este código" SKIP
              "se cancela la operacion de salvado del" SKIP
              "nuevo registro" VIEW-AS ALERT-BOX.
      RUN Mostrar_Registro.
      APPLY "entry" TO Ind_PUB.Nombre.
   END.
   ELSE DO:
     CREATE Ind_PUB.
     W_New = NO.
   END.
END.
ELSE
  FIND CURRENT Ind_PUB.
ASSIGN FRAME {&FRAME-NAME} Ind_PUB.Codigo Ind_PUB.Fec_Ingreso
         Ind_PUB.Fec_Retiro Ind_PUB.Ind_Prg Ind_PUB.Nombre Ind_PUB.Prg Ind_PUB.Usuario
         Ind_PUB.Estado Ind_PUB.Unidad Ind_PUB.Bf Ind_PUB.Bi
         Ind_PUB.Ef Ind_PUB.Ei Ind_PUB.Mf Ind_PUB.Mi Ind_PUB.Rf Ind_PUB.Ri Ind_PUB.Tipo
         Ind_PUB.Id_Malo Ind_PUB.Id_Regular Ind_PUB.Id_Bueno Ind_PUB.Id_Exitoso
         Ind_Pub.Usuario = W_Usuario. 
ASSIGN Ind_Pub.Perspectiva = INTEGER(SUBSTRING(CPerspectiva,1,2)).
Ind_PUB.Descripcion = EDes:SCREEN-VALUE.

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


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME Ind_PUB.Codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ind_PUB.Codigo wWin
ON LEAVE OF Ind_PUB.Codigo IN FRAME fMain /* Código */
DO:
  FIND Ind_PUB WHERE
       Ind_PUB.Codigo EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Ind_PUB THEN DO:
     MESSAGE "El Código Ind_PUB ya existe, a continuación" SKIP
             "se mostrara la información de esta cuenta" VIEW-AS ALERT-BOX INFORMATION.
     DISPLAY Ind_PUB.Codigo Ind_PUB.Fec_Ingreso
              Ind_PUB.Fec_Retiro Ind_PUB.Ind_Prg Ind_PUB.Nombre
              Ind_PUB.Prg Ind_PUB.Usuario Ind_PUB.Estado Ind_PUB.Unidad WITH FRAME FMain.
     EDes:SCREEN-VALUE = Ind_PUB.Descripcion.
     DISABLE Ind_PUB.Codigo WITH FRAME FMain.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME CPerspectiva2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CPerspectiva2 wWin
ON VALUE-CHANGED OF CPerspectiva2 IN FRAME F_Consulta /* Filtro x Perspectiva */
DO:
    CASE SELF:SCREEN-VALUE:
      WHEN "00 - Todas las perspectivas" THEN
          OPEN QUERY B_Pub FOR EACH Ind_PUB WHERE 
            Ind_PUB.Estado EQ 1 BY Ind_PUB.Codigo INDEXED-REPOSITION.
      OTHERWISE
          OPEN QUERY B_Pub FOR EACH Ind_PUB WHERE 
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


&Scoped-define SELF-NAME Ind_PUB.Id_Bueno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ind_PUB.Id_Bueno wWin
ON VALUE-CHANGED OF Ind_PUB.Id_Bueno IN FRAME fMain
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ENABLE Ind_PUB.Bi Bf WITH FRAME Fmain.
  ELSE DO:
     DISABLE Ind_PUB.Bi Bf WITH FRAME Fmain.
     ASSIGN  Ind_PUB.Bi:SCREEN-VALUE = ""
             Ind_PUB.Bf:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ind_PUB.Id_Exitoso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ind_PUB.Id_Exitoso wWin
ON VALUE-CHANGED OF Ind_PUB.Id_Exitoso IN FRAME fMain
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ENABLE Ind_PUB.Ei Ef WITH FRAME Fmain.
  ELSE DO:
      DISABLE Ind_PUB.Ei Ef WITH FRAME Fmain.
      ASSIGN  Ind_PUB.Ei:SCREEN-VALUE = ""
              Ind_PUB.Ef:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ind_PUB.Id_Malo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ind_PUB.Id_Malo wWin
ON VALUE-CHANGED OF Ind_PUB.Id_Malo IN FRAME fMain
DO:

  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ENABLE Ind_PUB.Mi Mf WITH FRAME Fmain.
  ELSE DO:
      DISABLE Ind_PUB.Mi Mf WITH FRAME Fmain.
      ASSIGN  Ind_PUB.Mi:SCREEN-VALUE = ""
              Ind_PUB.Mf:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ind_PUB.Id_Regular
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ind_PUB.Id_Regular wWin
ON VALUE-CHANGED OF Ind_PUB.Id_Regular IN FRAME fMain
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ENABLE Ind_PUB.Ri Rf WITH FRAME Fmain.
  ELSE DO:
      DISABLE Ind_PUB.Ri Rf WITH FRAME Fmain.
      ASSIGN  Ind_PUB.Ri:SCREEN-VALUE = ""
              Ind_PUB.Rf:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME REstado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL REstado wWin
ON VALUE-CHANGED OF REstado IN FRAME F_Consulta
DO:
  APPLY "value-changed" TO RTipos IN FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RTipos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RTipos wWin
ON VALUE-CHANGED OF RTipos IN FRAME F_Consulta
DO:
  ASSIGN FRAME F_Consulta REstado RTipos.
  IF RTipos EQ 4 THEN ASSIGN li = 1 lf = 3.
  ELSE ASSIGN li = RTipos lf = RTipos.

  OPEN QUERY B_PUB FOR EACH Ind_PUB WHERE Ind_PUB.Tipo GE li AND
     Ind_PUB.Tipo LE lf AND Ind_PUB.Estado EQ REstado
  NO-LOCK BY Ind_PUB.Codigo INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
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
  DISPLAY CPerspectiva EDes NomUsuario 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE Ind_PUB THEN 
    DISPLAY Ind_PUB.Ind_Prg Ind_PUB.Codigo Ind_PUB.Fec_Ingreso Ind_PUB.Fec_Retiro 
          Ind_PUB.Nombre Ind_PUB.Prg Ind_PUB.Usuario Ind_PUB.Estado 
          Ind_PUB.Unidad Ind_PUB.Tipo Ind_PUB.Campo Ind_PUB.Id_Bueno 
          Ind_PUB.Id_Exitoso Ind_PUB.Id_Malo Ind_PUB.Id_Regular Ind_PUB.Mi 
          Ind_PUB.Mf Ind_PUB.Ri Ind_PUB.Rf Ind_PUB.Bi Ind_PUB.Bf Ind_PUB.Ei 
          Ind_PUB.Ef 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE CPerspectiva BUTTON-61 EDes Ind_PUB.Ind_Prg BUTTON-49 BUTTON-1 
         BUTTON-2 B_CrSalvar B_CRIngresar B_CRCancelar B_CRBorrar BtnDone 
         Ind_PUB.Codigo Ind_PUB.Nombre Ind_PUB.Prg Ind_PUB.Estado 
         Ind_PUB.Unidad Ind_PUB.Tipo Ind_PUB.Campo Ind_PUB.Id_Bueno 
         Ind_PUB.Id_Exitoso Ind_PUB.Id_Malo Ind_PUB.Id_Regular Ind_PUB.Mi 
         Ind_PUB.Mf Ind_PUB.Ri Ind_PUB.Rf Ind_PUB.Bi Ind_PUB.Bf Ind_PUB.Ei 
         Ind_PUB.Ef RECT-301 RECT-302 RECT-303 RECT-304 RECT-305 RECT-306 
         RECT-307 RECT-309 RECT-310 RECT-311 RECT-312 RECT-313 RECT-314 
         RECT-315 RECT-316 RECT-317 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY RTipos REstado CPerspectiva2 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE RECT-326 RECT-327 B_PUB RTipos BUTTON-60 REstado CPerspectiva2 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  DISPLAY Eprg 
      WITH FRAME FCodigo IN WINDOW wWin.
  ENABLE Eprg BUTTON-62 BUTTON-63 BUTTON-64 
      WITH FRAME FCodigo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FCodigo}
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
FOR EACH Pub WHERE Pub.Clase EQ 1 NO-LOCK BREAK BY Pub.Ente:
     W_Ok = CPerspectiva:ADD-LAST(STRING(Pub.Ente,"99") + " - " + Pub.Nombre) IN FRAME FMain.
     W_Ok = CPerspectiva2:ADD-LAST(STRING(Pub.Ente,"99") + " - " + Pub.Nombre) IN FRAME F_Consulta.
 END.

  RUN SUPER.
FIND LAST Ind_PUB NO-LOCK NO-ERROR.
IF AVAILABLE Ind_PUB THEN DO:
   RUN Mostrar_Registro.
   DISABLE Ind_PUB.Codigo WITH FRAME FMain.
   APPLY "entry" TO Ind_PUB.Nombre IN FRAME FMain.
END.
ELSE
   DISABLE {&List-1} WITH FRAME FMain.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Registro wWin 
PROCEDURE Mostrar_Registro :
DISPLAY Ind_PUB.Codigo Ind_PUB.Fec_Ingreso
        Ind_PUB.Fec_Retiro Ind_PUB.Ind_Prg Ind_PUB.Nombre
        Ind_PUB.Prg Ind_PUB.Usuario Ind_PUB.Estado
        Ind_PUB.Bf Ind_PUB.Bi Ind_PUB.Ef Ind_PUB.Ei
        Ind_PUB.Mf Ind_PUB.Mi Ind_PUB.Rf Ind_PUB.Ri
        Ind_PUB.Tipo Ind_PUB.Id_Malo
        Ind_PUB.Id_Regular Ind_PUB.Id_Bueno Ind_PUB.Id_Exitoso
     WITH FRAME FMain.
   EDes:SCREEN-VALUE = Ind_PUB.Descripcion.
   ENABLE Ind_PUB.Mi Ind_PUB.Mf WITH FRAME FMain.
   IF NOT Ind_PUB.Id_Malo THEN DO:
      DISABLE Ind_PUB.Mi Ind_PUB.Mf WITH FRAME FMain.
      ASSIGN  Ind_PUB.Mi:SCREEN-VALUE = ""
              Ind_PUB.Mf:SCREEN-VALUE = "".
   END.
   ENABLE Ind_PUB.Ri Ind_PUB.Rf WITH FRAME FMain.
   IF NOT Ind_PUB.Id_Regular THEN DO:
      DISABLE Ind_PUB.Ri Ind_PUB.Rf WITH FRAME FMain.
      ASSIGN  Ind_PUB.Ri:SCREEN-VALUE = ""
              Ind_PUB.Rf:SCREEN-VALUE = "".
   END.
   ENABLE Ind_PUB.Bi Ind_PUB.Bf WITH FRAME FMain.
   IF NOT Ind_PUB.Id_Bueno THEN DO:
      DISABLE Ind_PUB.Bi Ind_PUB.Bf WITH FRAME FMain.
      ASSIGN  Ind_PUB.Bi:SCREEN-VALUE = ""
              Ind_PUB.Bf:SCREEN-VALUE = "".
   END.
   ENABLE Ind_PUB.Ei Ind_PUB.Ef WITH FRAME FMain.
   IF NOT Ind_PUB.Id_Exitoso THEN DO:
      DISABLE Ind_PUB.Ei Ind_PUB.Ef WITH FRAME FMain.
      ASSIGN  Ind_PUB.Ei:SCREEN-VALUE = ""
              Ind_PUB.Ef:SCREEN-VALUE = "".
   END.
   
   IF ind_Pub.Perspectiva EQ 0 THEN
      CPerspectiva:SCREEN-VALUE = CPerspectiva:ENTRY(1).
   ELSE
      CPerspectiva:SCREEN-VALUE = CPerspectiva:ENTRY(ind_pub.perspectiva + 1).
   FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
   IF AVAILABLE Usuarios THEN NomUsuario:SCREEN-VALUE = Usuarios.Nombre.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}
 W_Reporte   = "REPORTE   : INDICADORES PUB - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "COD NOMBRE                                              TIPO        M.I   M.F   R.I  R.F   B.I  B.F     E.I   E.F".

 W_Linea = FILL(W_Raya,132).
 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.
 
 

DEFINE VAR wper AS CHARACTER FORMAT "X(10)".
DEFINE VAR wtip AS CHARACTER FORMAT "X(10)".
          
FOR EACH ind_pub BREAK BY perspectiva BY codigo:
    IF FIRST-OF(perspectiva) THEN DO:
       CASE ind_pub.perspectiva:
           WHEN 1 THEN wper = "Financiera".
           WHEN 2 THEN wper = "Clientes".
           WHEN 3 THEN wper = "Procesos".
           WHEN 4 THEN wper = "Aprendizaje".
       END CASE.
       DISPLAY wper WITH NO-LABELS FRAME fe.

    END.
    CASE ind_pub.tipo:
      WHEN 1 THEN wtip = "Impacto".
      WHEN 2 THEN wtip = "Eficiencia".
      WHEN 3 THEN wtip = "Eficacia".
    END CASE.
    DISPLAY Ind_PUB.Codigo Ind_PUB.Nombre wtip
            Ind_PUB.Mi Ind_PUB.Mf Ind_PUB.Ri
            Ind_PUB.Rf Ind_PUB.Bi Ind_PUB.Bf
            Ind_PUB.Ei Ind_PUB.Ef
            WITH WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO FRAME fmov.
END.

 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar_Datos wWin 
PROCEDURE Verificar_Datos :
IF INTEGER(Ind_PUB.Codigo:SCREEN-VALUE IN FRAME FMain) EQ 0 THEN DO:
    MESSAGE "No se puede crear un Codigo en 0" SKIP(1)
            "Rectifique la información!!!" VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO Ind_PUB.Mf.
    RETURN NO-APPLY.
 END.
 
 IF Ind_PUB.Nombre:SCREEN-VALUE IN FRAME FMain EQ "" THEN DO:
    MESSAGE "No se puede crear un Codigo con Nombre" SKIP 
            "en blancos" SKIP(1)
            "Rectifique la información!!!" VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO Ind_PUB.Mf.
    RETURN NO-APPLY.
 END.
     
/* IF INTEGER(Ind_PUB.Mf:SCREEN-VALUE IN FRAME FMain) EQ 0 THEN DO:
    MESSAGE "El limite final del rango de SEMAFORIZACION" SKIP
            "para los resultados MALOS, no puede ser Cero" SKIP(1)
            "Rectifique la información!!!" VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO Ind_PUB.Mf.
    RETURN NO-APPLY.
 END.

 IF INTEGER(Ind_PUB.Ri:SCREEN-VALUE IN FRAME FMain) LE INTEGER(Ind_PUB.Mf:SCREEN-VALUE) THEN DO:
    MESSAGE "El limite inicial del rango de SEMAFORIZACION" SKIP
            "para los resultados REGULARES, no puede ser menor" SKIP
            "o igual al limite final de los Resultados MALOS" SKIP(1)
            "Rectifique la información!!!." VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO Ind_PUB.Ri.
    RETURN NO-APPLY.
 END.
 IF INTEGER(Ind_PUB.Rf:SCREEN-VALUE IN FRAME FMain) LE INTEGER(Ind_PUB.Ri:SCREEN-VALUE) THEN DO:
    MESSAGE "El limite final del rango de SEMAFORIZACION" SKIP
            "para los resultados REGULARES, no puede ser menor" SKIP
            "o igual al limite inicial de los Resultados REGULARES" SKIP(1)
            "Rectifique la información!!!." VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO Ind_PUB.RF.
    RETURN NO-APPLY.
 END.
 IF INTEGER(Ind_PUB.Bi:SCREEN-VALUE IN FRAME FMain) LE INTEGER(Ind_PUB.Rf:SCREEN-VALUE) THEN DO:
    MESSAGE "El limite inicial del rango de SEMAFORIZACION" SKIP
            "para los resultados BUENOS, no puede ser menor" SKIP
            "o igual al limite final de los Resultados REGULARES" SKIP(1)
            "Rectifique la información!!!." VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO Ind_PUB.Bi.
    RETURN NO-APPLY.
 END.
 IF INTEGER(Ind_PUB.Ei:SCREEN-VALUE IN FRAME FMain) LE INTEGER(Ind_PUB.Bf:SCREEN-VALUE) THEN DO:
    MESSAGE "El limite inicial del rango de SEMAFORIZACION" SKIP
            "para los resultados Exitosos, no puede ser menor" SKIP
            "o igual al limite final de los Resultados BUENOS" SKIP(1)
            "Rectifique la información!!!." VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO Ind_PUB.Ei.
    RETURN NO-APPLY.
 END.
 IF INTEGER(Ind_PUB.Ef:SCREEN-VALUE IN FRAME FMain) LE INTEGER(Ind_PUB.Ei:SCREEN-VALUE) THEN DO:
    MESSAGE "El limite final del rango de SEMAFORIZACION" SKIP
            "para los resultados EXITOSOS, no puede ser menor" SKIP
            "o igual al limite inicial de los Resultados EXITOSOS" SKIP(1)
            "Rectifique la información!!!." VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO Ind_PUB.Ef.
    RETURN NO-APPLY.
 END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

