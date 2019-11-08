&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

{incluido/Variable.i "SHARED"}.
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_NitAnt AS CHARACTER.
DEFINE VAR W_Inf AS CHARACTER FORMAT "X(10)".
DEFINE VAR gTexto AS CHARACTER FORMAT "X(50)".
DEFINE VAR Wk_Edad AS INTEGER FORMAT "999".
DEFINE VAR Tel_Numerico AS DECIMAL FORMAT "99999999999999".
DEFINE VAR P_Ubi AS CHARACTER.
DEFINE VAR P_NUbi AS CHARACTER FORMAT "X(80)".
DEFINE VARIABLE V_Nom AS CHARACTER.
DEFINE VARIABLE V_Cod AS INTEGER.
DEFINE VAR P_cod AS INTEGER.
DEFINE VAR P_AgeEmp AS INTEGER.
DEFINE VAR P_Nom AS CHARACTER FORMAT "X(30)".
DEFINE VAR P_Nit AS CHARACTER.
DEFINE VAR P_Nombre AS CHARACTER.
DEFINE VAR P_Apellido AS CHARACTER.
DEFINE VAR P_AgeCli AS INTEGER.

DEFINE TEMP-TABLE T_Relaciones
    FIELD R_Relacion AS CHARACTER
    FIELD R_AgeObjeto AS INTEGER
    FIELD R_NitObjeto AS CHARACTER
    FIELD R_NomObjeto AS CHARACTER FORMAT "X(35)"
    FIELD R_TelObjeto AS CHARACTER FORMAT "X(30)"
    FIELD R_NomDescri AS CHARACTER FORMAT "X(15)"
    FIELD R_TelComerc AS CHARACTER FORMAT "X(30)".

/* oakley */

  DEFINE TEMP-TABLE TFalta
     FIELD TCampo AS CHARACTER FORMAT "X(55)"
     FIELD TDonde AS CHARACTER FORMAT "X(35)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Aportes
&Scoped-define BROWSE-NAME Br_Relaciones

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES T_Relaciones TFalta

/* Definitions for BROWSE Br_Relaciones                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Relaciones T_Relaciones.R_Relacion T_Relaciones.R_AgeObjeto T_Relaciones.R_NitObjeto T_Relaciones.R_NomObjeto T_Relaciones.R_NomDescri T_Relaciones.R_TelObjeto T_Relaciones.R_TelComerc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Relaciones   
&Scoped-define SELF-NAME Br_Relaciones
&Scoped-define QUERY-STRING-Br_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Relaciones OPEN QUERY {&SELF-NAME} FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Relaciones T_Relaciones
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Relaciones T_Relaciones


/* Definitions for BROWSE B_Falta                                       */
&Scoped-define FIELDS-IN-QUERY-B_Falta TFalta.TCampo TFalta.TDonde   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Falta   
&Scoped-define SELF-NAME B_Falta
&Scoped-define QUERY-STRING-B_Falta FOR EACH TFalta NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Falta OPEN QUERY {&SELF-NAME} FOR EACH TFalta NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Falta TFalta
&Scoped-define FIRST-TABLE-IN-QUERY-B_Falta TFalta


/* Definitions for FRAME F_Falta                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Falta ~
    ~{&OPEN-QUERY-B_Falta}

/* Definitions for FRAME F_Relaciones                                   */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Bt_FinAporte 
&Scoped-Define DISPLAYED-OBJECTS Edit_msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_SC 
     LABEL "SC" 
     SIZE 15 BY .81.

DEFINE BUTTON BUTTON-205 
     LABEL "Grabar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-206 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE R_Apellido1 AS CHARACTER FORMAT "X(15)" 
     LABEL "Primer Apellido" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Apellido2 AS CHARACTER FORMAT "X(15)" 
     LABEL "Segundo Apellido" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Nit AS CHARACTER FORMAT "X(12)" 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Nombre AS CHARACTER FORMAT "X(40)" 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Relacion AS CHARACTER FORMAT "X(50)":U 
     LABEL "Relacion o Parentesco" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Tel_Comercial AS CHARACTER FORMAT "X(15)":U 
     LABEL "Telefono Comercial" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Tel_Residencia AS CHARACTER FORMAT "X(20)" 
     LABEL "Teléfono de la Residencia" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE BUTTON Bt_FinAporte 
     LABEL "Fin Creaciòn Aportes" 
     SIZE 22 BY 1.12.

DEFINE VARIABLE Edit_msaje AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 42.29 BY 3.81 NO-UNDO.

DEFINE BUTTON Btn_Borrar  NO-FOCUS
     LABEL "Borrar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Cancelar  NO-FOCUS
     LABEL "Cancelar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U NO-FOCUS
     LABEL "Button 3" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Deshacer  NO-FOCUS
     LABEL "Deshacer" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Ingresar  NO-FOCUS
     LABEL "Ingresar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Salir DEFAULT  NO-FOCUS
     LABEL "&Salir" 
     SIZE 11 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Salvar  NO-FOCUS
     LABEL "Salvar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U NO-FOCUS
     LABEL "Button 11" 
     SIZE 5 BY 1.08.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U NO-FOCUS
     LABEL "Button 2" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE Cmb_Agencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Titulo_Ape1 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE Titulo_Ape2 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE Titulo_Nombre AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE IMAGE Foto
     FILENAME "imagenes/fotos/0.jpg":U
     SIZE 18 BY 5.38.

DEFINE VARIABLE RSeleccion AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Segmentación", 1,
"Ubicación", 2,
"Económica", 3,
"Relaciones", 4,
"Fechas", 5,
"Controles", 6
     SIZE 87.43 BY 1.08
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 95 BY 1.35
     BGCOLOR 18 .

DEFINE RECTANGLE RECT-216
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 5.38.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 1.62.

DEFINE VARIABLE Tot_Activos AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Activos" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tot_Egresos AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Egresos" 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tot_Ingresos AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Ingresos" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-213
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 7.04.

DEFINE RECTANGLE RECT-214
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 7.08.

DEFINE RECTANGLE RECT-219
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 4.85.

DEFINE RECTANGLE RECT-220
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 4.85.

DEFINE BUTTON BUTTON-179 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 179" 
     SIZE 9 BY 1.62.

DEFINE BUTTON BUTTON-180 
     LABEL "Habilitar Fec.Fallecido" 
     SIZE 18 BY 1.12
     FONT 4.

DEFINE BUTTON Btn_Codseg 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_coding 2" 
     SIZE 3 BY .54.

DEFINE VARIABLE W_NomSegmento AS CHARACTER FORMAT "X(30)":U 
     LABEL "Segmentación Interna" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-221
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 1.35.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 1.35.

DEFINE RECTANGLE RECT-301
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.43 BY 1.04.

DEFINE BUTTON Btn_Activas 
     LABEL "Borrar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_CanRel 
     LABEL "Cancelar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_CreRel 
     LABEL "Crear" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_SalRel 
     LABEL "Salvar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-116 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 116" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE Cmb_Relaciones AS CHARACTER FORMAT "X(50)":U 
     LABEL "Consulta Relaciones" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 49 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_MenRel AS CHARACTER FORMAT "X(256)":U INITIAL "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar" 
     VIEW-AS FILL-IN 
     SIZE 91 BY .81
     FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE RActivas AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activas", 1,
"Borradas", 2
     SIZE 19 BY .81 NO-UNDO.

DEFINE BUTTON Btn_Cargo 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 12" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Ciiu 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 99" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_CodIng 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 1" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Empresa 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 98" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Profesion 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 13" 
     SIZE 3 BY .54.

DEFINE VARIABLE Cmb_TipAct AS CHARACTER FORMAT "X(25)":U 
     LABEL "Tipo Actividad" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "","Empleado","Estudiante","Ama de Casa","Jubilado","Independiente" 
     DROP-DOWN-LIST
     SIZE 29.86 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NomCargo AS CHARACTER FORMAT "X(50)":U 
     LABEL "Cargo" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCIIU AS CHARACTER FORMAT "X(80)":U 
     LABEL "CIIU" 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomEmpresa AS CHARACTER FORMAT "X(50)":U 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomIngreso AS CHARACTER FORMAT "X(30)":U 
     LABEL "Causal de Ingreso" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomProfesion AS CHARACTER FORMAT "X(50)":U 
     LABEL "Profesión" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomRetiro AS CHARACTER FORMAT "X(30)":U 
     LABEL "Causal de Retiro" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomUsuario AS CHARACTER FORMAT "X(60)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 1.62.

DEFINE RECTANGLE RECT-209
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 5.23.

DEFINE RECTANGLE RECT-215
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 1.62.

DEFINE RECTANGLE RECT-217
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 2.35.

DEFINE BUTTON Btn_Documento 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 109" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Nacimiento 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 108" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Residencia 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 103" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-105 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 105" 
     SIZE 3 BY .54.

DEFINE VARIABLE Cmb_Zonas AS CHARACTER FORMAT "X(256)":U 
     LABEL "Zona" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CiuExpedicion AS CHARACTER FORMAT "X(50)":U 
     LABEL "Ubicación Exp. Documento" 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CiuNacimiento AS CHARACTER FORMAT "X(50)":U 
     LABEL "Ubicación de Nacimiento" 
     VIEW-AS FILL-IN 
     SIZE 37.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_UbicacionComercial AS CHARACTER FORMAT "X(80)":U 
     LABEL "Dpto-Mpio" 
     VIEW-AS FILL-IN 
     SIZE 73 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_UbicacionResidencia AS CHARACTER FORMAT "X(80)":U 
     LABEL "Dpto-Mpio" 
     VIEW-AS FILL-IN 
     SIZE 73 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-210
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 4.04.

DEFINE RECTANGLE RECT-211
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 3.35.

DEFINE RECTANGLE RECT-212
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 2.42.

DEFINE RECTANGLE RECT-218
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 2.15.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Relaciones FOR 
      T_Relaciones SCROLLING.

DEFINE QUERY B_Falta FOR 
      TFalta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Relaciones wWin _FREEFORM
  QUERY Br_Relaciones NO-LOCK DISPLAY
      T_Relaciones.R_Relacion  FORMAT "X(18)" LABEL "Relacion"
      T_Relaciones.R_AgeObjeto FORMAT "999"   LABEL "Age"
      T_Relaciones.R_NitObjeto FORMAT "X(14)" LABEL "Nit"
      T_Relaciones.R_NomObjeto FORMAT "X(35)" LABEL "Nombre"
      T_Relaciones.R_NomDescri FORMAT "X(15)" LABEL "Descripción"
      T_Relaciones.R_TelObjeto FORMAT "X(15)" LABEL "Tel.Residencia"
      T_Relaciones.R_TelComerc FORMAT "X(15)" LABEL "Tel.Comercial"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 8.62
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.

DEFINE BROWSE B_Falta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Falta wWin _FREEFORM
  QUERY B_Falta NO-LOCK DISPLAY
      TFalta.TCampo COLUMN-LABEL "Campo"
  TFalta.TDonde COLUMN-LABEL "Donde encontrar para llenar"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 68 BY 6.46
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Aportes
     Edit_msaje AT ROW 1.46 COL 2.57 NO-LABEL
     Bt_FinAporte AT ROW 5.62 COL 12.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 7.85
         SIZE 45.14 BY 6.69
         BGCOLOR 17 
         TITLE "Creaciòn de Aportes".

DEFINE FRAME F_Clientes
     Btn_Consulta AT ROW 4.77 COL 101
     Btn_Borrar AT ROW 15 COL 101
     Btn_Cancelar AT ROW 16.62 COL 101
     Btn_Deshacer AT ROW 11.77 COL 101
     Btn_Ingresar AT ROW 13.38 COL 101
     Btn_Salir AT ROW 18.23 COL 101
     Btn_Salvar AT ROW 10.15 COL 101
     BUTTON-11 AT ROW 20.65 COL 104
     Cmb_Agencia AT ROW 1.27 COL 26 COLON-ALIGNED
     BUTTON-1 AT ROW 1.54 COL 101
     BUTTON-2 AT ROW 3.15 COL 101
     Clientes.Estado AT ROW 1.81 COL 79 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 18 BY .81
          FONT 5
     Clientes.Tipo_Identificacion AT ROW 2.12 COL 26 COLON-ALIGNED
          LABEL "Tipo"
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "C.C","C.E","T.I","NIT","R.C","NUI" 
          DROP-DOWN-LIST
          SIZE 6 BY 1
          BGCOLOR 15 
     Clientes.Nit AT ROW 2.15 COL 39 COLON-ALIGNED
          LABEL "Número"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Titulo_Nombre AT ROW 3.15 COL 20 COLON-ALIGNED NO-LABEL
     Titulo_Ape1 AT ROW 3.15 COL 54 COLON-ALIGNED NO-LABEL
     Titulo_Ape2 AT ROW 3.15 COL 75 COLON-ALIGNED NO-LABEL
     Clientes.Nombre AT ROW 4 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 34 BY .81
          BGCOLOR 15 FONT 5
     Clientes.Apellido1 AT ROW 4 COL 54 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21 BY .81
          BGCOLOR 15 FONT 5
     Clientes.Apellido2 AT ROW 4 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21 BY .81
          BGCOLOR 15 FONT 5
     Clientes.Tipo_Cliente AT ROW 5.04 COL 22 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Natural Mayor de Edad", 1,
"Natural Menor de Edad", 2,
"Juridica S.L", 3,
"Juridica C.L", 4
          SIZE 76 BY 1.08
          FONT 6
     RSeleccion AT ROW 6.54 COL 9.57 NO-LABEL
     "Estado Actual" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 1.81 COL 65
          FGCOLOR 7 FONT 5
     Foto AT ROW 1 COL 3
     RECT-2 AT ROW 6.38 COL 3
     RECT-216 AT ROW 1.27 COL 100
     RECT-4 AT ROW 1.27 COL 64
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 21.27
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON Btn_Salir.

DEFINE FRAME F_Relaciones
     Cmb_Relaciones AT ROW 1.27 COL 23 COLON-ALIGNED
     RActivas AT ROW 1.27 COL 75 NO-LABEL
     Btn_CreRel AT ROW 2.35 COL 25
     Btn_CanRel AT ROW 2.35 COL 42
     Btn_Activas AT ROW 2.35 COL 59
     Btn_SalRel AT ROW 2.35 COL 76
     BUTTON-116 AT ROW 3.15 COL 3
     Br_Relaciones AT ROW 4.77 COL 3
     W_MenRel AT ROW 13.65 COL 1 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.73
         SIZE 95 BY 14.54
         BGCOLOR 17 FONT 4
         TITLE "RELACIONES".

DEFINE FRAME FRelNva
     R_Tel_Comercial AT ROW 5.5 COL 20 COLON-ALIGNED
     R_Nit AT ROW 1.27 COL 20 COLON-ALIGNED HELP
          "Número documento de identificación"
     R_Nombre AT ROW 2.12 COL 20 COLON-ALIGNED HELP
          "Nombre del cliente"
     R_Apellido1 AT ROW 2.96 COL 20 COLON-ALIGNED HELP
          "Primer apellido del cliente"
     R_Apellido2 AT ROW 3.81 COL 20 COLON-ALIGNED HELP
          "Segundo apellido del cliente"
     R_Tel_Residencia AT ROW 4.65 COL 20 COLON-ALIGNED HELP
          "Teléfono de la residencia del cliente"
     R_Relacion AT ROW 6.35 COL 20 COLON-ALIGNED
     BUTTON-205 AT ROW 2.08 COL 53
     BUTTON-206 AT ROW 6.12 COL 53
     Btn_SC AT ROW 1.27 COL 53
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 17 ROW 3.42
         SIZE 69 BY 7.54
         BGCOLOR 17 FONT 4
         TITLE "Nueva Relacion".

DEFINE FRAME F_Falta
     B_Falta AT ROW 2.08 COL 3
     BUTTON-179 AT ROW 8.81 COL 62
     "La siguiente información se necesita para salvar el registro" VIEW-AS TEXT
          SIZE 51 BY .62 AT ROW 1.27 COL 4
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 15 ROW 7.73
         SIZE 72 BY 10.5
         BGCOLOR 17 
         TITLE "Información que falta por llenar".

DEFINE FRAME F_Otros
     Clientes.Id_PuedeCodeudar AT ROW 1.77 COL 68 HELP
          "Id_PuedeCodeudar"
          LABEL "Id_PuedeCodeudar"
          VIEW-AS TOGGLE-BOX
          SIZE 18.14 BY .65
     Clientes.Reestructurado AT ROW 1.92 COL 4 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "No aplica", 0,
"Reestructurado", 1,
"No ReEstructurado", 2
          SIZE 50 BY .69
     Clientes.Id_Privilegiado AT ROW 3.04 COL 66.86 HELP
          "Id_Privilegiado" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "No Privilegio", 0,
"Si Privilegiado", 1
          SIZE 26.43 BY .92
     Clientes.Sancionado AT ROW 3.15 COL 4
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .77
     Clientes.Dias_Sancion AT ROW 3.15 COL 39 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.57 BY .81
          BGCOLOR 15 
     W_NomSegmento AT ROW 4.5 COL 21 COLON-ALIGNED
     Btn_Codseg AT ROW 4.5 COL 55
     Clientes.Cod_Segmento AT ROW 4.5 COL 56 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          FGCOLOR 17 
     Clientes.Reportado_Super AT ROW 5.58 COL 23
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .77
     Clientes.Aut_CentralRiesgo AT ROW 6.38 COL 23
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .77
     Clientes.Id_Preexistentes AT ROW 7.19 COL 23
          LABEL "Ha tenido enfermedades preexistentes?"
          VIEW-AS TOGGLE-BOX
          SIZE 32 BY .77
     Clientes.Reportado_fiscalia AT ROW 8 COL 23
          LABEL "Reportado a Fiscalia"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .77
     Clientes.Reportado_Procredito AT ROW 8.81 COL 23
          LABEL "Reportado Procrédito"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .77
     Clientes.Calificacion AT ROW 9.88 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     Clientes.Con_Sospechosas AT ROW 10.96 COL 21 COLON-ALIGNED
          LABEL "Operaciones Sospechosas"
          VIEW-AS FILL-IN 
          SIZE 2.86 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "  Reestructurado" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 1.27 COL 6
          FGCOLOR 7 FONT 5
     RECT-221 AT ROW 2.88 COL 3
     RECT-3 AT ROW 1.54 COL 3
     RECT-301 AT ROW 1.54 COL 66.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.73
         SIZE 95 BY 14.54
         BGCOLOR 17 FONT 4
         TITLE "OTROS".

DEFINE FRAME F_Ubicacion
     W_UbicacionResidencia AT ROW 1.81 COL 12 COLON-ALIGNED
     Btn_Residencia AT ROW 1.96 COL 87
     Clientes.Dir_Residencia AT ROW 2.81 COL 12 COLON-ALIGNED
          LABEL "Dirección"
          VIEW-AS FILL-IN 
          SIZE 73 BY .81
          BGCOLOR 15 
     Clientes.Lugar_Residencia AT ROW 2.88 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Tel_Residencia AT ROW 3.69 COL 12 COLON-ALIGNED
          LABEL "Teléfono" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 23 BY .81
          BGCOLOR 15 
     Clientes.Celular AT ROW 4.58 COL 12 COLON-ALIGNED FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 23 BY .81
          BGCOLOR 15 
     Clientes.Cod_Zona AT ROW 4.5 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Cmb_Zonas AT ROW 4.54 COL 50 COLON-ALIGNED
     Clientes.Tipo_Vivienda AT ROW 6.38 COL 7 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Familiar", 3,
"Propia", 1,
"Arrendada", 2
          SIZE 29.57 BY .92
     Clientes.Nom_Arrendatario AT ROW 7.19 COL 47 COLON-ALIGNED
          LABEL "Arrendatario"
          VIEW-AS FILL-IN 
          SIZE 27 BY .81
          BGCOLOR 15 
     Clientes.Tel_Arrendatario AT ROW 7.19 COL 74 COLON-ALIGNED NO-LABEL FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 15 
     BUTTON-105 AT ROW 8.92 COL 87
     W_UbicacionComercial AT ROW 9 COL 12 COLON-ALIGNED
     Clientes.Dir_comercial AT ROW 9.92 COL 12 COLON-ALIGNED
          LABEL "Dirección"
          VIEW-AS FILL-IN 
          SIZE 73 BY .81
          BGCOLOR 15 
     Clientes.Tel_comercial AT ROW 10.81 COL 12 COLON-ALIGNED
          LABEL "Teléfono" FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 26 BY .81
          BGCOLOR 15 
     Clientes.Lugar_comercial AT ROW 10.81 COL 81 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 17 FGCOLOR 17 
     W_CiuNacimiento AT ROW 12.42 COL 47 COLON-ALIGNED
     Btn_Nacimiento AT ROW 12.42 COL 87
     Clientes.Lugar_Nacimiento AT ROW 12.42 COL 88 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Dir_Correspondencia AT ROW 13.38 COL 6 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Oficina", yes,
"Residencia", no
          SIZE 21 BY .81
     W_CiuExpedicion AT ROW 13.5 COL 47 COLON-ALIGNED
     Btn_Documento AT ROW 13.5 COL 87
     Clientes.Lugar_expedicion AT ROW 13.5 COL 88 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Estrato AT ROW 7.19 COL 24 COLON-ALIGNED
          LABEL "Estrato"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "0","1","2","3","4","5","6","7" 
          DROP-DOWN-LIST
          SIZE 9 BY 1
          BGCOLOR 15 
     Clientes.Email AT ROW 3.69 COL 50 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 35 BY .81
          BGCOLOR 15 
     "Tipo de Vivienda" VIEW-AS TEXT
          SIZE 15 BY .81 AT ROW 5.73 COL 5
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.73
         SIZE 95 BY 14.54
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Ubicacion
     "Teléfono" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 6.65 COL 76
          FGCOLOR 7 
     "Nombre" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 6.65 COL 49.43
          FGCOLOR 7 
     "  Empresa" VIEW-AS TEXT
          SIZE 9.86 BY .5 AT ROW 8.27 COL 6
          FGCOLOR 7 FONT 5
     "Enviar Correspondencia a" VIEW-AS TEXT
          SIZE 23 BY .77 AT ROW 12.42 COL 5
          FGCOLOR 7 FONT 5
     "  Residencia" VIEW-AS TEXT
          SIZE 12 BY .5 AT ROW 1.27 COL 6
          FGCOLOR 7 FONT 5
     RECT-210 AT ROW 1.54 COL 4
     RECT-211 AT ROW 8.54 COL 4
     RECT-212 AT ROW 12.15 COL 4
     RECT-218 AT ROW 6.12 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.73
         SIZE 95 BY 14.54
         BGCOLOR 17 FONT 4
         TITLE "UBICACIÓN".

DEFINE FRAME F_Economica
     Clientes.Salario AT ROW 2.35 COL 22 COLON-ALIGNED FORMAT ">>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_arriendos AT ROW 3.23 COL 22 COLON-ALIGNED
          LABEL "Arriendos" FORMAT "->>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_financieros AT ROW 4.08 COL 22 COLON-ALIGNED
          LABEL "Financieros" FORMAT ">>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_Honorarios AT ROW 4.92 COL 22 COLON-ALIGNED
          LABEL "Honorarios" FORMAT ">>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_Otros AT ROW 5.85 COL 22 COLON-ALIGNED FORMAT ">>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Gto_Familiar AT ROW 2.35 COL 72.57 COLON-ALIGNED
          LABEL "Familiares" FORMAT ">>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     Clientes.Gto_Arriendo AT ROW 3.27 COL 72.57 COLON-ALIGNED
          LABEL "Arrendamientos" FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     Clientes.Gto_obligacion AT ROW 4.27 COL 72.57 COLON-ALIGNED
          LABEL "Deducciones Colilla" FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     Tot_Ingresos AT ROW 6.92 COL 22 COLON-ALIGNED
     Tot_Egresos AT ROW 7.04 COL 73 COLON-ALIGNED
     Clientes.Act_casa AT ROW 9.19 COL 22 COLON-ALIGNED FORMAT "->>>>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Act_vehiculo AT ROW 10.12 COL 22 COLON-ALIGNED
          LABEL "Valor Vehículo" FORMAT "->>>>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Act_inversion AT ROW 11.08 COL 22 COLON-ALIGNED
          LABEL "Inversiones" FORMAT ">>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Sdo_Obligaciones AT ROW 5.27 COL 72.72 COLON-ALIGNED
          LABEL "Deudas DataCrédito" FORMAT ">>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     Tot_Activos AT ROW 12.42 COL 22 COLON-ALIGNED
     " Activos" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 8.54 COL 7
          FGCOLOR 7 FONT 5
     "Pasivos" VIEW-AS TEXT
          SIZE 8 BY 1.08 AT ROW 8.27 COL 50
          FGCOLOR 7 FONT 5
     "  Egresos Mensuales" VIEW-AS TEXT
          SIZE 19 BY .77 AT ROW 1.04 COL 50
          FGCOLOR 7 FONT 5
     "  Ingresos Mensuales" VIEW-AS TEXT
          SIZE 19 BY 1.04 AT ROW 1 COL 6
          FGCOLOR 7 FONT 5
     RECT-213 AT ROW 1.23 COL 4
     RECT-214 AT ROW 1.27 COL 47.86
     RECT-219 AT ROW 8.81 COL 4
     RECT-220 AT ROW 8.81 COL 48
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.73
         SIZE 95 BY 14.54
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "ECONÓMICA".

DEFINE FRAME F_Segmentacion
     Clientes.Tipo_Vinculo AT ROW 1.81 COL 5 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Asociado", 1,
"Cliente No Asociado", 2,
"Tercero", 3,
"Proveedor", 4
          SIZE 64 BY .81
          FONT 5
     Clientes.Sexo AT ROW 1.81 COL 74 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Hombre", 1,
"Mujer", 2
          SIZE 19 BY .77
          FONT 5
     Clientes.Est_Civil AT ROW 3 COL 13 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "No Aplica","Soltero","Casado","Separado","Viudo","Unión Libre" 
          DROP-DOWN-LIST
          SIZE 27 BY 1
          BGCOLOR 15 
     W_NomIngreso AT ROW 3.15 COL 58 COLON-ALIGNED
     Btn_CodIng AT ROW 3.15 COL 92
     Clientes.Per_Acargo AT ROW 3.96 COL 32.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     W_NomRetiro AT ROW 4.04 COL 58 COLON-ALIGNED
     Clientes.Num_Hijos AT ROW 4.77 COL 32.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Clientes.Id_Retencion AT ROW 5.88 COL 53
          VIEW-AS TOGGLE-BOX
          SIZE 19.14 BY .81
     Clientes.Gran_Contribuyente AT ROW 5.88 COL 75
          LABEL "Gran Contribuyente"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .77
     Clientes.Niv_Educativo AT ROW 6.73 COL 12.86 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 6
          LIST-ITEMS "Ninguno","Bachiller","Tecnico","Tecnologo","Profesional","Especializacion","Maestria","Doctorado" 
          DROP-DOWN-LIST
          SIZE 29 BY 1
          BGCOLOR 15 
     W_NomCIIU AT ROW 6.92 COL 51 COLON-ALIGNED
     Btn_Ciiu AT ROW 6.92 COL 92
     Btn_Profesion AT ROW 7.5 COL 41
     W_NomProfesion AT ROW 7.62 COL 13 COLON-ALIGNED
     W_NomUsuario AT ROW 7.73 COL 51 COLON-ALIGNED
     Btn_Empresa AT ROW 8.46 COL 41
     W_NomEmpresa AT ROW 8.54 COL 13 COLON-ALIGNED
     Clientes.Med_Publicitario AT ROW 8.62 COL 66 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 10
          LIST-ITEMS "Ninguno","Asociados","Televisión","Radio","Medio Escrito","Evento","Otro" 
          DROP-DOWN-LIST
          SIZE 22 BY 1
          BGCOLOR 15 
     Btn_Cargo AT ROW 9.38 COL 41
     W_NomCargo AT ROW 9.46 COL 13 COLON-ALIGNED
     Clientes.Carnet AT ROW 10.42 COL 13 COLON-ALIGNED
          LABEL "Carnet"
          VIEW-AS FILL-IN 
          SIZE 26 BY .81
          BGCOLOR 15 
     Clientes.Cod_Retiro AT ROW 10.42 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Cod_Ingreso AT ROW 10.42 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Codigo_CIIU AT ROW 10.42 COL 67 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Grupo AT ROW 10.42 COL 70 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2.86 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Subgrupo AT ROW 10.42 COL 73 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2.86 BY .81
          BGCOLOR 17 FGCOLOR 17 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.73
         SIZE 95 BY 14.54
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Segmentacion
     Clientes.Cod_Cargo AT ROW 10.42 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          FGCOLOR 17 
     Clientes.Cod_Profesion AT ROW 10.42 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Cod_Empresa AT ROW 10.42 COL 84 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Usuario AT ROW 10.42 COL 88 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Cmb_TipAct AT ROW 11.81 COL 4.57
     Clientes.Tip_Contrato AT ROW 12.88 COL 12.29 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ninguno", 0,
"Indefinido", 1,
"Fijo", 2,
"Labor Contratada", 3,
"Prestación de Servicios", 4,
"Pensionado", 5
          SIZE 81 BY .81
          FONT 5
     "  Sexo" VIEW-AS TEXT
          SIZE 6 BY .77 AT ROW 1 COL 73
          FGCOLOR 7 FONT 5
     "Contrato" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 12.88 COL 3.29
          FGCOLOR 7 FONT 5
     "  Tipo de Vínculo" VIEW-AS TEXT
          SIZE 16 BY .77 AT ROW 1 COL 4
          FGCOLOR 7 FONT 5
     "  Informacion Empresarial y Estudios" VIEW-AS TEXT
          SIZE 31 BY .81 AT ROW 5.85 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-1 AT ROW 1.27 COL 71
     RECT-209 AT ROW 6.31 COL 2
     RECT-215 AT ROW 1.27 COL 3
     RECT-217 AT ROW 11.58 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.73
         SIZE 95 BY 14.54
         BGCOLOR 17 FONT 4
         TITLE "SEGMENTACION".

DEFINE FRAME F_Fechas
     Clientes.Fec_Ingreso AT ROW 2.08 COL 78 COLON-ALIGNED
          LABEL "Ingreso a la Cooperativa"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Fec_Retiro AT ROW 3.15 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Fec_UltActualiza AT ROW 4.23 COL 78 COLON-ALIGNED
          LABEL "Ultima Actualización"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Fec_IngEmpresa AT ROW 6.92 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Clientes.Fec_Calificacion AT ROW 8.27 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Fec_IniSancion AT ROW 9.35 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Fec_expedicion AT ROW 10.96 COL 78 COLON-ALIGNED
          LABEL "Fecha de Expedicion del Documento de Identidad" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     Clientes.Fec_Nacimiento AT ROW 12.04 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     BUTTON-180 AT ROW 13.12 COL 43
     Clientes.Fec_fallecido AT ROW 13.12 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     "Formato de Fecha dd/mm/aaaa" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 1.27 COL 63
          FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.73
         SIZE 95 BY 14.54
         BGCOLOR 17 FONT 5
         TITLE "FECHAS".


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
         TITLE              = "SFG - Central de Clientes, Programa W-ProClientes.w"
         HEIGHT             = 21.04
         WIDTH              = 112.86
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
ASSIGN FRAME FRelNva:FRAME = FRAME F_Relaciones:HANDLE
       FRAME F_Economica:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Falta:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Fechas:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Otros:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Relaciones:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Segmentacion:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Ubicacion:FRAME = FRAME F_Clientes:HANDLE.

/* SETTINGS FOR FRAME FRelNva
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME FRelNva:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Aportes
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Aportes:HIDDEN           = TRUE
       FRAME F_Aportes:MOVABLE          = TRUE.

/* SETTINGS FOR EDITOR Edit_msaje IN FRAME F_Aportes
   NO-ENABLE                                                            */
ASSIGN 
       Edit_msaje:READ-ONLY IN FRAME F_Aportes        = TRUE.

/* SETTINGS FOR FRAME F_Clientes
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Segmentacion:MOVE-BEFORE-TAB-ITEM (FRAME F_Otros:HANDLE)
       XXTABVALXX = FRAME F_Relaciones:MOVE-BEFORE-TAB-ITEM (FRAME F_Segmentacion:HANDLE)
       XXTABVALXX = FRAME F_Economica:MOVE-BEFORE-TAB-ITEM (FRAME F_Relaciones:HANDLE)
       XXTABVALXX = FRAME F_Fechas:MOVE-BEFORE-TAB-ITEM (FRAME F_Economica:HANDLE)
       XXTABVALXX = FRAME F_Falta:MOVE-BEFORE-TAB-ITEM (FRAME F_Fechas:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR BUTTON Btn_Borrar IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Deshacer IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Agencia IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Clientes.Estado IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Nit IN FRAME F_Clientes
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR COMBO-BOX Clientes.Tipo_Identificacion IN FRAME F_Clientes
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Titulo_Ape1 IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Titulo_Ape2 IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Titulo_Nombre IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Economica
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Economica:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Clientes.Act_casa IN FRAME F_Economica
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Clientes.Act_inversion IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Act_vehiculo IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Gto_Arriendo IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Gto_Familiar IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Gto_obligacion IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Ing_arriendos IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Ing_financieros IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Ing_Honorarios IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Ing_Otros IN FRAME F_Economica
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Clientes.Salario IN FRAME F_Economica
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Clientes.Sdo_Obligaciones IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Tot_Activos IN FRAME F_Economica
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tot_Egresos IN FRAME F_Economica
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tot_Ingresos IN FRAME F_Economica
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Falta
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_Falta TEXT-18 F_Falta */
ASSIGN 
       FRAME F_Falta:HIDDEN           = TRUE
       FRAME F_Falta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Fechas
                                                                        */
/* SETTINGS FOR FILL-IN Clientes.Fec_Calificacion IN FRAME F_Fechas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_expedicion IN FRAME F_Fechas
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Fec_fallecido IN FRAME F_Fechas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_Ingreso IN FRAME F_Fechas
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_IniSancion IN FRAME F_Fechas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_Retiro IN FRAME F_Fechas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_UltActualiza IN FRAME F_Fechas
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME F_Otros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Otros:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Clientes.Cod_Segmento IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Con_Sospechosas IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Clientes.Id_Preexistentes IN FRAME F_Otros
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET Clientes.Id_Privilegiado IN FRAME F_Otros
   EXP-HELP                                                             */
/* SETTINGS FOR TOGGLE-BOX Clientes.Id_PuedeCodeudar IN FRAME F_Otros
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX Clientes.Reportado_fiscalia IN FRAME F_Otros
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Clientes.Reportado_Procredito IN FRAME F_Otros
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_NomSegmento IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Relaciones
   NOT-VISIBLE                                                          */
ASSIGN XXTABVALXX = FRAME FRelNva:MOVE-AFTER-TAB-ITEM (BUTTON-116:HANDLE IN FRAME F_Relaciones)
       XXTABVALXX = FRAME FRelNva:MOVE-BEFORE-TAB-ITEM (Br_Relaciones:HANDLE IN FRAME F_Relaciones)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB Br_Relaciones FRelNva F_Relaciones */
ASSIGN 
       FRAME F_Relaciones:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_CanRel IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_SalRel IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_MenRel IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Segmentacion
                                                                        */
/* SETTINGS FOR FILL-IN Clientes.Carnet IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_TipAct IN FRAME F_Segmentacion
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Clientes.Codigo_CIIU IN FRAME F_Segmentacion
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN Clientes.Cod_Cargo IN FRAME F_Segmentacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Cod_Empresa IN FRAME F_Segmentacion
   NO-DISPLAY NO-ENABLE EXP-LABEL                                       */
/* SETTINGS FOR FILL-IN Clientes.Cod_Ingreso IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Cod_Profesion IN FRAME F_Segmentacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Cod_Retiro IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Clientes.Gran_Contribuyente IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Grupo IN FRAME F_Segmentacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Subgrupo IN FRAME F_Segmentacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR RADIO-SET Clientes.Tipo_Vinculo IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Usuario IN FRAME F_Segmentacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN W_NomCargo IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCIIU IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomEmpresa IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomIngreso IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomProfesion IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomRetiro IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomUsuario IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Ubicacion
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Ubicacion:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Clientes.Celular IN FRAME F_Ubicacion
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Clientes.Cod_Zona IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Dir_comercial IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Dir_Residencia IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Clientes.Estrato IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Lugar_comercial IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Lugar_expedicion IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Lugar_Nacimiento IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Lugar_Residencia IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Nom_Arrendatario IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Tel_Arrendatario IN FRAME F_Ubicacion
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Clientes.Tel_comercial IN FRAME F_Ubicacion
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Tel_Residencia IN FRAME F_Ubicacion
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN W_CiuExpedicion IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CiuNacimiento IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_UbicacionComercial IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_UbicacionResidencia IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Relaciones
/* Query rebuild information for BROWSE Br_Relaciones
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Relaciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Falta
/* Query rebuild information for BROWSE B_Falta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TFalta NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Falta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRelNva
/* Query rebuild information for FRAME FRelNva
     _Query            is NOT OPENED
*/  /* FRAME FRelNva */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Aportes
/* Query rebuild information for FRAME F_Aportes
     _Query            is NOT OPENED
*/  /* FRAME F_Aportes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Clientes
/* Query rebuild information for FRAME F_Clientes
     _Query            is NOT OPENED
*/  /* FRAME F_Clientes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Economica
/* Query rebuild information for FRAME F_Economica
     _Query            is NOT OPENED
*/  /* FRAME F_Economica */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Falta
/* Query rebuild information for FRAME F_Falta
     _Query            is NOT OPENED
*/  /* FRAME F_Falta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Fechas
/* Query rebuild information for FRAME F_Fechas
     _Query            is NOT OPENED
*/  /* FRAME F_Fechas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Otros
/* Query rebuild information for FRAME F_Otros
     _Query            is NOT OPENED
*/  /* FRAME F_Otros */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Relaciones
/* Query rebuild information for FRAME F_Relaciones
     _Query            is NOT OPENED
*/  /* FRAME F_Relaciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Segmentacion
/* Query rebuild information for FRAME F_Segmentacion
     _Query            is NOT OPENED
*/  /* FRAME F_Segmentacion */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Ubicacion
/* Query rebuild information for FRAME F_Ubicacion
     _Query            is NOT OPENED
*/  /* FRAME F_Ubicacion */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F_Clientes:HANDLE
       ROW             = 1
       COLUMN          = 1
       HEIGHT          = 5.65
       WIDTH           = .14
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {93330F00-7CA6-101B-874B-0020AF109266} type: CSComboBox */
      CtrlFrame:MOVE-AFTER(FRAME F_Otros:HANDLE).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Central de Clientes, Programa W-ProClientes.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Central de Clientes, Programa W-ProClientes.w */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Clientes.Act_casa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_casa wWin
ON LEAVE OF Clientes.Act_casa IN FRAME F_Economica /* Valor Propiedad */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_casa wWin
ON VALUE-CHANGED OF Clientes.Act_casa IN FRAME F_Economica /* Valor Propiedad */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Act_inversion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_inversion wWin
ON LEAVE OF Clientes.Act_inversion IN FRAME F_Economica /* Inversiones */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_inversion wWin
ON VALUE-CHANGED OF Clientes.Act_inversion IN FRAME F_Economica /* Inversiones */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Act_vehiculo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_vehiculo wWin
ON LEAVE OF Clientes.Act_vehiculo IN FRAME F_Economica /* Valor Vehículo */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_vehiculo wWin
ON VALUE-CHANGED OF Clientes.Act_vehiculo IN FRAME F_Economica /* Valor Vehículo */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Clientes.Apellido1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Apellido1 wWin
ON LEAVE OF Clientes.Apellido1 IN FRAME F_Clientes /* Primer Apellido */
DO:
  SELF:SCREEN-VALUE IN FRAME F_Clientes = CAPS(SELF:SCREEN-VALUE IN FRAME F_Clientes).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Apellido1 wWin
ON VALUE-CHANGED OF Clientes.Apellido1 IN FRAME F_Clientes /* Primer Apellido */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Apellido2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Apellido2 wWin
ON LEAVE OF Clientes.Apellido2 IN FRAME F_Clientes /* Segundo Apellido */
DO:
  SELF:SCREEN-VALUE IN FRAME F_Clientes = CAPS(SELF:SCREEN-VALUE IN FRAME F_Clientes).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Apellido2 wWin
ON VALUE-CHANGED OF Clientes.Apellido2 IN FRAME F_Clientes /* Segundo Apellido */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Aut_CentralRiesgo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Aut_CentralRiesgo wWin
ON VALUE-CHANGED OF Clientes.Aut_CentralRiesgo IN FRAME F_Otros /* Autoriza Consulta C.Riesgo */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_Activas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Activas wWin
ON CHOOSE OF Btn_Activas IN FRAME F_Relaciones /* Borrar */
DO:
  DO WITH FRAME F_Relaciones:
  W_MenRel:SCREEN-VALUE = "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar".
  IF Br_Relaciones:NUM-SELECTED-ROWS EQ 0 THEN DO:
     MESSAGE "Debe posicionarse en la relación a Borrar" SKIP
             "mediante el mouse. Rectifique!!!" VIEW-AS ALERT-BOX INFORMATION.
  END.
  ELSE
  DO:
    FIND Relaciones WHERE Relaciones.Nit EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes AND
                          Relaciones.Cod_Relacion EQ INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5)) AND
                          Relaciones.Nit_Relacion EQ T_Relaciones.R_NitObjeto NO-ERROR.
    IF AVAILABLE(Relaciones) THEN DO:
       IF RActivas:SCREEN-VALUE EQ "1" THEN
          ASSIGN Relaciones.Estado = 2
                 Relaciones.Fec_Inactividad = W_Fecha.
       ELSE
          ASSIGN Relaciones.Estado = 1
                 Relaciones.Fec_Inactividad = ?.
    END.
    APPLY 'value-changed' TO Cmb_Relaciones.
    RETURN NO-APPLY.
  END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Clientes /* Cancelar */
DO:
  DISABLE Cmb_Agencia WITH FRAME F_Clientes.
  ENABLE Btn_Consulta Btn_Ingresar Btn_Borrar WITH FRAME F_Clientes.
  DISABLE Btn_Salvar Btn_Cancelar Btn_Deshacer Btn_Borrar WITH FRAME F_Clientes.  
  W_Ok = RSeleccion:ENABLE("Relaciones").
  Clientes.Nit:BGCOLOR = 18.
  Clientes.Nit:FGCOLOR = 15.
  DISABLE Clientes.Nit Clientes.Tipo_Identificacion WITH FRAME F_Clientes.
  FIND Clientes WHERE Clientes.Agencia EQ INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Clientes,1,3)) AND
                      Clientes.Nit     EQ W_NitAnt NO-ERROR.
  IF AVAILABLE(Clientes) THEN RUN Mostrar_Cliente.  
  ELSE MESSAGE "No se encontro ningún Cliente para mostrar" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_CanRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CanRel wWin
ON CHOOSE OF Btn_CanRel IN FRAME F_Relaciones /* Cancelar */
DO:
  DO WITH FRAME F_Relaciones:
      W_MenRel:SCREEN-VALUE = "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar".
      ENABLE Btn_CreRel Btn_Activas.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Btn_Cargo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cargo wWin
ON CHOOSE OF Btn_Cargo IN FRAME F_Segmentacion /* Button 12 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Varios.r (INPUT 2, OUTPUT V_Cod, OUTPUT V_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF V_Cod EQ 0 THEN
     MESSAGE "No se ha elegido ningun CARGO" VIEW-AS ALERT-BOX.
  ELSE
     ASSIGN W_NomCargo:SCREEN-VALUE = V_Nom
            Clientes.Cod_Cargo:SCREEN-VALUE = STRING(V_Cod).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ciiu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ciiu wWin
ON CHOOSE OF Btn_Ciiu IN FRAME F_Segmentacion /* Button 99 */
DO:
  DEFINE VAR P_Gru LIKE Ciiu.Grupo.
  DEFINE VAR P_Sub LIKE Ciiu.Subgrupo.
  DEFINE VAR P_Cod LIKE Ciiu.Codigo.
  DEFINE VAR P_Nom AS CHARACTER FORMAT "X(50)".
 ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ciiu.r (OUTPUT P_Gru, OUTPUT P_Sub, OUTPUT P_Cod, OUTPUT P_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF P_Gru EQ 0 THEN MESSAGE "No fue escogido ningún código" VIEW-AS ALERT-BOX.
  ELSE
    ASSIGN Clientes.Codigo_Ciiu:SCREEN-VALUE = STRING(P_Cod)
           Clientes.Grupo:SCREEN-VALUE       = STRING(P_Gru)
           Clientes.Subgrupo:SCREEN-VALUE    = STRING(P_Sub)
           W_NomCiiu:SCREEN-VALUE = LOWER(P_Nom).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_CodIng
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CodIng wWin
ON CHOOSE OF Btn_CodIng IN FRAME F_Segmentacion /* Button 1 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Varios.r (INPUT 4, OUTPUT V_Cod, OUTPUT V_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF V_Cod EQ 0 THEN
     MESSAGE "No se ha elegido ningun CAUSAL" VIEW-AS ALERT-BOX.
  ELSE
     ASSIGN W_NomIngreso:SCREEN-VALUE = V_Nom
            Clientes.Cod_Ingreso:SCREEN-VALUE = STRING(V_Cod).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Btn_Codseg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Codseg wWin
ON CHOOSE OF Btn_Codseg IN FRAME F_Otros /* Btn_coding 2 */
DO:
DO WITH FRAME F_Otros:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  RUN C-Varios.r (INPUT 6, OUTPUT V_Cod, OUTPUT V_Nom).
  IF V_Cod EQ 0 THEN
     MESSAGE "No se ha elegido ningun Segmento" VIEW-AS ALERT-BOX.
  ELSE
     ASSIGN W_Nomsegmento:SCREEN-VALUE = V_Nom
            Clientes.Cod_Segmento:SCREEN-VALUE = STRING(V_Cod).
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME F_Clientes /* Button 3 */
DO:
  DO WITH FRAME F_Clientes:
      ASSIGN WWin:SENSITIVE = FALSE.
      RUN C-Clientes.R(INPUT 2, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
      ASSIGN WWin:SENSITIVE = TRUE.
      WWin:MOVE-TO-TOP().

     IF P_Nit NE "" THEN DO:
        FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit AND
             Clientes.Tipo_Vinculo LT 3 NO-ERROR.
        IF AVAILABLE(Clientes) THEN DO: 
           Wk_Edad = (DECIMAL(TODAY) - DECIMAL(Clientes.Fec_Nacimiento)) / 365.
           IF Clientes.Tipo_Cliente EQ 2 AND 
              Wk_Edad GE 18 THEN DO:
                  MESSAGE "El Cliente esta identificado como menor de edad" SKIP
                   "sin embargo por fecha de nacimiento ya es mayor de edad" SKIP
                   "Favor solicitar actualizar documento de identificación." SKIP
                    VIEW-AS ALERT-BOX ERROR.
           END.
           RUN Mostrar_Cliente. 
           FIND Agencias WHERE Agencias.Agencia EQ P_AgeCli NO-LOCK NO-ERROR.
           IF AVAILABLE(Agencias) THEN Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
        END.
        ELSE DO:
           MESSAGE "Este persona o empresa no es un cliente de" SKIP
                   "la cooperativa." SKIP(1)
                   "Si se desea cambiar su estado a cliente. deberá" SKIP
                   "hacerse como un ingreso normal" VIEW-AS ALERT-BOX INFORMATION TITLE "Persona No vinculada".
        END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_CreRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CreRel wWin
ON CHOOSE OF Btn_CreRel IN FRAME F_Relaciones /* Crear */
DO:
  DO WITH FRAME F_Relaciones:
      IF Cmb_Relaciones:SCREEN-VALUE BEGINS "00000" OR 
         Cmb_Relaciones:SCREEN-VALUE BEGINS "99999" THEN DO:
         MESSAGE "Para crear una relacion se debe escoger el tipo" VIEW-AS ALERT-BOX.
         APPLY "entry" TO Cmb_Relaciones.
         RETURN NO-APPLY.
      END.
      W_MenRel:SCREEN-VALUE = "Escoja del Combo de consulta la relación que se le asignará a la persona o empresa".
     RActivas:SCREEN-VALUE = "1".
     ENABLE Cmb_Relaciones Btn_SalRel.
     DISABLE Btn_CreRel Btn_Activas.
     VIEW FRAME FRelNva.
     RETURN NO-APPLY.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Clientes /* Deshacer */
DO:
  DISABLE Cmb_Agencia WITH FRAME F_Clientes.
  ENABLE Btn_Consulta Btn_Ingresar Btn_Borrar WITH FRAME F_Clientes.
  DISABLE Btn_Salvar Btn_Cancelar Btn_Deshacer WITH FRAME F_Clientes.  
  W_Ok = RSeleccion:ENABLE("Relaciones").
  Clientes.Nit:BGCOLOR = 18.
  Clientes.Nit:FGCOLOR = 15.
  DISABLE Clientes.Nit Clientes.Tipo_Identificacion WITH FRAME F_Clientes.
  FIND Clientes WHERE Clientes.Agencia EQ INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Clientes,1,3)) AND
                      (Clientes.Nit     EQ W_NitAnt OR 
                       Clientes.Nit     EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes)NO-ERROR.
  IF AVAILABLE(Clientes) THEN RUN Mostrar_Cliente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Btn_Documento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Documento wWin
ON CHOOSE OF Btn_Documento IN FRAME F_Ubicacion /* Button 109 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  ASSIGN W_CiuExpedicion:SCREEN-VALUE = LC(P_NUbi)
         Clientes.Lugar_Expedicion:SCREEN-VALUE   = P_Ubi.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Btn_Empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Empresa wWin
ON CHOOSE OF Btn_Empresa IN FRAME F_Segmentacion /* Button 98 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Empresas.r (INPUT W_Agencia, OUTPUT P_Cod, OUTPUT P_Nit, OUTPUT P_AgeEmp, OUTPUT P_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF P_Cod NE 0 THEN DO:
     ASSIGN W_NomEmpresa:SCREEN-VALUE = P_Nom
            Clientes.Cod_Empresa:SCREEN-VALUE = STRING(P_Cod).
  END.
  ELSE MESSAGE "No se ha elegido ninguna EMPRESA" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_Clientes /* Ingresar */
DO:
  W_NitAnt = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes.
  RELEASE Clientes.
  
  DISABLE Btn_Consulta Btn_Ingresar Btn_Borrar WITH FRAME F_Clientes.
  ENABLE Btn_Salvar Btn_Cancelar Btn_Deshacer WITH FRAME F_Clientes.
  W_Ok = RSeleccion:DISABLE("Relaciones").
  ENABLE Clientes.Nit Clientes.Tipo_Identificacion WITH FRAME F_Clientes.
  FIND Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
  APPLY 'entry' TO Clientes.Tipo_Identificacion IN FRAME F_Clientes.
  RUN Inicializar_Campos.
  
  ASSIGN Clientes.Nit:BGCOLOR = 15
         Clientes.Nit:FGCOLOR = 0
         Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
  APPLY "ENTRY" TO Clientes.Tipo_Identif.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Btn_Nacimiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Nacimiento wWin
ON CHOOSE OF Btn_Nacimiento IN FRAME F_Ubicacion /* Button 108 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.r (OUTPUT P_Ubi, OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  ASSIGN W_CiuNacimiento:SCREEN-VALUE = LC(P_NUbi)
         Clientes.Lugar_Nacimiento:SCREEN-VALUE = P_Ubi.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Btn_Profesion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Profesion wWin
ON CHOOSE OF Btn_Profesion IN FRAME F_Segmentacion /* Button 13 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  
  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Varios.r (INPUT 1, OUTPUT V_Cod, OUTPUT V_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF V_Cod EQ 0 THEN
     MESSAGE "No se ha elegido ninguna PROFESIÓN" VIEW-AS ALERT-BOX.
  ELSE
     ASSIGN W_NomProfesion:SCREEN-VALUE = V_Nom
            Clientes.Cod_Profesion:SCREEN-VALUE = STRING(V_Cod).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Btn_Residencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Residencia wWin
ON CHOOSE OF Btn_Residencia IN FRAME F_Ubicacion /* Button 103 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
  IF INTEGER(P_Ubi) EQ 0 THEN DO:
     MESSAGE "No fue encontrada ninguna ubicación" VIEW-AS ALERT-BOX.
  END.

  ASSIGN W_UbicacionResidencia:SCREEN-VALUE = LC(P_NUbi)
         Clientes.Lugar_Residencia:SCREEN-VALUE = P_Ubi.
    /*MESSAGE clientes.lugar_residencia:SCREEN-VALUE p_ubi VIEW-AS ALERT-BOX.    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Clientes /* Salir */
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


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_SalRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalRel wWin
ON CHOOSE OF Btn_SalRel IN FRAME F_Relaciones /* Salvar */
DO:
  DO WITH FRAME F_Relaciones:
    W_MenRel:SCREEN-VALUE = "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar".
    FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN DO:
        CREATE T_Relaciones.
        UPDATE T_Relaciones.R_Relacion =  SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,8,15)
               T_Relaciones.R_AgeObjeto = Clientes.Agencia
               T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
               T_Relaciones.R_NomDescri = Relaciones.Descripcion
               T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
               T_Relaciones.R_TelObjeto = Clientes.Tel_Residencia
               T_Relaciones.R_TelComerc = Clientes.Tel_Comercial.
    END.
    DISABLE Btn_SalRel.
     OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
     ENABLE Btn_CreRel Btn_Activas.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Clientes /* Salvar */
DO:
  DEFINE VAR XAportes AS LOGICAL INITIAL NO.
  DEFI VAR W_CodPro  LIKE Ahorros.Cod_Ahorro INIT 5.  /*Inicia con 5 Mayores*/

  FOR EACH TFalta: DELETE TFalta. END.
  RUN Validacion_Informacion.
  FIND FIRST TFalta NO-ERROR.
  IF AVAILABLE TFalta THEN DO:
     OPEN QUERY B_Falta FOR EACH TFalta NO-LOCK INDEXED-REPOSITION.
     VIEW FRAME F_Falta.
     APPLY "entry" TO Clientes.Nombre IN FRAME F_Clientes.
     RETURN NO-APPLY.
  END.
  
  IF Clientes.Estado:SCREEN-VALUE IN FRAME F_Clientes EQ "2"
  OR AVAIL(Clientes) AND Clientes.Tipo_Vinculo NE 1  THEN DO:
     IF AVAIL(Clientes) AND Clientes.Cod_Retiro EQ 12 THEN DO:
        MESSAGE "Cliente con retiro EXLUIDO...No se acepta el ReIngreso."
            VIEW-AS ALERT-BOX ERROR.
        DISABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
        APPLY "entry" TO Btn_Salir IN FRAME F_Clientes.
        RETURN NO-APPLY.
     END.

     MESSAGE "El Cliente NO es ASOCIADO..."  SKIP
             "Continue SOLO Si desea Actualizarlo..." 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "SI ASOCIAR" 
         UPDATE W_SiAsoc AS LOGICAL.
     IF NOT W_SiAsoc THEN DO:
        DISABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
        APPLY "entry" TO Btn_Salir IN FRAME F_Clientes.
        RETURN NO-APPLY.
     END.
  END.
  
  DISABLE Cmb_Agencia WITH FRAME F_Clientes.
  ENABLE Btn_Consulta Btn_Ingresar WITH FRAME F_Clientes.
  DISABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  
  FIND Clientes WHERE Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes NO-ERROR.
  IF NOT AVAILABLE(Clientes) THEN DO: 
     W_Ok = RSeleccion:ENABLE("Relaciones").
     CREATE Clientes.
     ASSIGN Clientes.Agencia      = W_Agencia
            Clientes.Tipo_Vinculo = 2
            XAportes              = TRUE.
  END.
  ELSE DO:
     IF Clientes.Estado:SCREEN-VALUE IN FRAME F_Clientes EQ "2"
     OR AVAIL(Clientes) AND Clientes.Tipo_Vinculo NE 1 THEN DO:
        IF AVAIL(Ahorros) AND Ahorros.Sdo_Dispon + Sdo_Canje GT 0 THEN
           ASSIGN Clientes.Estado       = 1
                  Clientes.Tipo_Vinculo = 1.
                  /*Clientes.Agencia      = W_Agencia.*/
        ELSE
           ASSIGN Clientes.Estado       = 1
                  Clientes.Tipo_Vinculo = 2.
                 /* Clientes.Agencia      = W_Agencia.*/
     END.
  END.

  IF Clientes.Agencia EQ 0 THEN
     Clientes.Agencia = W_Agencia. 
  
  RUN Grabar.
  RUN Verifica_Remanente.

  /***** Nuevo 27/sep/2007 ********/
/*   FIND CURRENT Clientes NO-ERROR.                                                     */
/*   IF (clientes.tipo_vinculo EQ 2) AND ((clientes.fec_ingreso LE clientes.fec_retiro)  */
/*      OR clientes.fec_ingreso = ? OR clientes.agencia = 24) THEN                       */
/*      /*ASSIGN Clientes.Agencia     = W_agencia*/                                      */
/*        ASSIGN Clientes.Fec_Ingr]eso = W_Fecha.                                         */
/*                                                                                       */
/*   FIND ahorros WHERE                                                                  */
/*        ahorros.nit EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes AND               */
/*        ahorros.cod_ahorro EQ 5 AND ahorros.estado EQ 1 AND                            */
/*        ahorros.sdo_disponible + ahorros.sdo_canje GT 0 NO-ERROR.                      */
/*   IF AVAILABLE(ahorros) THEN DO:                                                      */
/*      FIND CURRENT Clientes NO-ERROR.                                                  */
/*      ASSIGN Clientes.Agencia     = ahorros.agencia.                                   */
/*   END.                                                                                */
  /***************/
  
  FIND CURRENT Clientes NO-LOCK NO-ERROR.
  
  IF W_Fecha - Clientes.Fec_Nacimiento LT 5110 THEN
     RUN Verifica_Tutor.

  RUN Verifica_RepLegal.

  IF XAportes THEN DO:
     Xaportes = FALSE.
     RUN Grabar_Aportes NO-ERROR.
     IF ERROR-STATUS:ERROR THEN
        MESSAGE "El producto de aportes, debe estar configurado Id_Consecutivo...PARA" SKIP
                "la creación automática de la cuenta." SKIP
                "La cuenta se deberá crear manualmente" VIEW-AS ALERT-BOX WARNING.
     ELSE DO:
         FRAME F_Clientes:SENSITIVE = FALSE.
         VIEW FRAME F_Aportes.
         ASSIGN FRAME F_Aportes:SENSITIVE = TRUE
                Edit_Msaje:SCREEN-VALUE   = " ".

         IF Pro_Ahorros.Id_Consecutivo THEN 
            ASSIGN Edit_Msaje:SCREEN-VALUE = "La cuenta de aportes fue creada, OK".         
     END.
  END.  

  DISABLE Clientes.Fec_Fallecido WITH FRAME F_Fechas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME Btn_SC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SC wWin
ON CHOOSE OF Btn_SC IN FRAME FRelNva /* SC */
DO:
  R_Nit:SCREEN-VALUE = "SC_" + STRING(NEXT-VALUE(Sec_NitAuto)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Aportes
&Scoped-define SELF-NAME Bt_FinAporte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bt_FinAporte wWin
ON CHOOSE OF Bt_FinAporte IN FRAME F_Aportes /* Fin Creaciòn Aportes */
DO:
  /*IF Pro_Ahorros.Id_Consecutivo  THEN 
     MESSAGE "La cuenta de aportes fue creada" SKIP
             "el monto de apertura debe ser: $ " Ahorros.Monto_Apertura SKIP
             "El valor de la admisión es   : $ " Admision SKIP
             "La cuota mensual para el " YEAR(TODAY) " es " Ahorros.Cuota
             VIEW-AS ALERT-BOX INFORMATION.*/

  HIDE FRAME F_Aportes.
  FRAME F_Clientes:SENSITIVE = TRUE.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Clientes /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME BUTTON-105
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-105 wWin
ON CHOOSE OF BUTTON-105 IN FRAME F_Ubicacion /* Button 105 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  ASSIGN W_UbicacionComercial:SCREEN-VALUE = LC(P_NUbi)
         Clientes.Lugar_Comercial:SCREEN-VALUE = P_Ubi.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME BUTTON-116
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-116 wWin
ON CHOOSE OF BUTTON-116 IN FRAME F_Relaciones /* Button 116 */
DO:
  W_Inf = "Relaciones".     
  IF Br_Relaciones:NUM-ENTRIES EQ 0 THEN
  DO:
     MESSAGE "La lista Actual no contiene ninguna información para imprimir" SKIP
             "Rectifique la consulta por medio del combo: CONSULTA DE RELACIONES" 
             VIEW-AS ALERT-BOX INFORMATION.
  END.
  ELSE DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
    Listado = W_Pathspl + "Lst_Relaciones.lst".
    {incluido/imprimir.i "Listado"}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Falta
&Scoped-define SELF-NAME BUTTON-179
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-179 wWin
ON CHOOSE OF BUTTON-179 IN FRAME F_Falta /* Button 179 */
DO:
  HIDE FRAME F_Falta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Fechas
&Scoped-define SELF-NAME BUTTON-180
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-180 wWin
ON CHOOSE OF BUTTON-180 IN FRAME F_Fechas /* Habilitar Fec.Fallecido */
DO:
  ENABLE Clientes.Fec_Fallecido WITH FRAME F_Fechas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Clientes /* Button 2 */
DO:
  W_Inf = "Cliente".     
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_Pathspl + "Lst_Relaciones.lst".
  {incluido/imprimir.i "Listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME BUTTON-205
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-205 wWin
ON CHOOSE OF BUTTON-205 IN FRAME FRelNva /* Grabar */
DO:
  ASSIGN FRAME FRelNva R_Nit R_Nombre R_Apellido1 R_Apellido2 R_Tel_Residencia R_Relacion
               R_Tel_Comercial.
  IF R_Nit EQ "" OR R_Nombre EQ "" OR R_Apellido1 EQ "" OR R_Relacion EQ "" THEN DO:
     MESSAGE "Debe entrarse minimo: Nit, Nombre, apellido1 y el tipo de relacion" SKIP
             "o parentesco de la persona. rectifique la informacion" VIEW-AS ALERT-BOX.
     APPLY "entry" TO R_Nit.
     RETURN NO-APPLY.
  END.
  ELSE DO:
      FIND Clientes WHERE Clientes.Nit EQ R_Nit NO-ERROR.
      IF NOT AVAILABLE Clientes THEN DO:
          IF INDEX(R_nit,"-") > 0 THEN DO:
              MESSAGE "El formato para el número de documento es inválido." SKIP
                      "Revise por favor..."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.

              RETURN NO-APPLY.
          END.

         CREATE Clientes.
         ASSIGN Clientes.Agencia   = W_Agencia
                Clientes.Nit       = R_Nit
                Clientes.Nombre    = R_Nombre
                Clientes.Apellido1 = R_Apellido1
                Clientes.Apellido2 = R_Apellido2
                Clientes.Tel_Residencia = R_Tel_Residencia
                Clientes.Tel_Comercial  = R_Tel_Comercial.
      END.
      DO WITH FRAME F_Relaciones:
         CREATE Relaciones.
         ASSIGN Relaciones.Nit             = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes
                Relaciones.Cod_Relacion    = INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5))
                Relaciones.Nit_Relacion    = R_Nit
                Relaciones.Usuario         = W_Usuario
                Relaciones.Fec_Ingreso     = W_Fecha
                Relaciones.Descripcion     = R_Relacion:SCREEN-VALUE
                Relaciones.Estado          = 1.
      END.
      DO WITH FRAME FNvaRel:
        ASSIGN R_Nit:SCREEN-VALUE = ""
               R_Nombre:SCREEN-VALUE = ""
               R_Apellido1:SCREEN-VALUE = ""
               R_Apellido2:SCREEN-VALUE = ""
               R_Tel_Residencia:SCREEN-VALUE = ""
               R_Tel_Comercial:SCREEN-VALUE  = "".
      END.
      HIDE FRAME FRelNva.
      APPLY "choose" TO Btn_SalRel IN FRAME F_Relaciones.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-206
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-206 wWin
ON CHOOSE OF BUTTON-206 IN FRAME FRelNva /* Ocultar */
DO:
    ASSIGN R_Nit:SCREEN-VALUE = ""
         R_Nombre:SCREEN-VALUE = ""
         R_Apellido1:SCREEN-VALUE = ""
         R_Apellido2:SCREEN-VALUE = ""
         R_Tel_Residencia:SCREEN-VALUE = "".
    APPLY "choose" TO Btn_CanRel IN FRAME F_Relaciones.
    APPLY "choose" TO Btn_SalRel IN FRAME F_Relaciones.
    HIDE FRAME FRelNva.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Calificacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Calificacion wWin
ON VALUE-CHANGED OF Clientes.Calificacion IN FRAME F_Otros /* Calificación del Cliente */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Carnet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Carnet wWin
ON LEAVE OF Clientes.Carnet IN FRAME F_Segmentacion /* Carnet */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Clientes.Celular
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Celular wWin
ON LEAVE OF Clientes.Celular IN FRAME F_Ubicacion /* Celular */
DO:
  DEFI VAR Tel_Nume AS DEC FORM "999999999999".
  ASSIGN Tel_Nume = DEC(Clientes.Celular:SCREEN-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "Solo Numèrico...Corrija por favor."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO SELF.
     RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Celular wWin
ON VALUE-CHANGED OF Clientes.Celular IN FRAME F_Ubicacion /* Celular */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Cmb_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Relaciones wWin
ON VALUE-CHANGED OF Cmb_Relaciones IN FRAME F_Relaciones /* Consulta Relaciones */
DO:
  DO WITH FRAME F_Relaciones:
  FOR EACH T_Relaciones: DELETE T_Relaciones. END.
  CASE Cmb_Relaciones:SCREEN-VALUE:
  WHEN "99999 - Todas las Relaciones" THEN DO:
      FOR EACH Relaciones WHERE Relaciones.Nit    EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes AND 
                                Relaciones.Estado EQ INTEGER(RActivas:SCREEN-VALUE) NO-LOCK 
                          BREAK BY Relaciones.Cod_Relacion:
          IF FIRST-OF(Relaciones.Cod_Relacion) THEN
             FIND Varios WHERE Varios.Tipo EQ 3 AND Varios.Codigo EQ Relaciones.Cod_Relacion NO-LOCK NO-ERROR.
          FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          IF AVAILABLE(Clientes) THEN DO:
              CREATE T_Relaciones.
              UPDATE T_Relaciones.R_Relacion = Varios.Descripcion
                     T_Relaciones.R_AgeObjeto = Clientes.Agencia
                     T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
                     T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                     T_Relaciones.R_NomDescri = Relaciones.Descripcion
                     T_Relaciones.R_TelObjeto = Clientes.Tel_Residencia
                     T_Relaciones.R_TelComerc = Clientes.Tel_Comercial.
          END.
      END.
  END.
  WHEN "00000 - Ninguna Relacion" THEN DO:
      FOR EACH T_Relaciones: DELETE T_Relaciones. END.
  END.
  OTHERWISE DO:
      FIND Varios WHERE Varios.Tipo EQ 3 AND Varios.Codigo EQ INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5)) NO-LOCK NO-ERROR.
      FOR EACH Relaciones WHERE Relaciones.Nit EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes AND 
                                Relaciones.Cod_Relacion  EQ INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5)) AND 
                                Relaciones.Estado EQ INTEGER(RActivas:SCREEN-VALUE) NO-LOCK:
         FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
         IF AVAILABLE(Clientes) THEN DO:
            CREATE T_Relaciones.
            UPDATE T_Relaciones.R_Relacion = Varios.Descripcion
                   T_Relaciones.R_AgeObjeto = Clientes.Agencia
                   T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
                   T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                   T_Relaciones.R_NomDescri = Relaciones.Descripcion
                   T_Relaciones.R_TelObjeto = Clientes.Tel_Residencia
                   T_Relaciones.R_TelComerc = Clientes.Tel_Comercial.
         END.
       END.
  END.
  END CASE.
  OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.  
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Cmb_TipAct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TipAct wWin
ON LEAVE OF Cmb_TipAct IN FRAME F_Segmentacion /* Tipo Actividad */
DO:
  ASSIGN Cmb_TipAct.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TipAct wWin
ON VALUE-CHANGED OF Cmb_TipAct IN FRAME F_Segmentacion /* Tipo Actividad */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Cmb_Zonas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Zonas wWin
ON VALUE-CHANGED OF Cmb_Zonas IN FRAME F_Ubicacion /* Zona */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
DO WITH FRAME F_Ubicacion:
  ASSIGN Clientes.Cod_Zona:SCREEN-VALUE = SUBSTRING(Cmb_Zonas:SCREEN-VALUE,1,4).
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Dias_Sancion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Dias_Sancion wWin
ON VALUE-CHANGED OF Clientes.Dias_Sancion IN FRAME F_Otros /* Número de Días de Sanción */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Clientes.Dir_comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Dir_comercial wWin
ON VALUE-CHANGED OF Clientes.Dir_comercial IN FRAME F_Ubicacion /* Dirección */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Dir_Correspondencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Dir_Correspondencia wWin
ON VALUE-CHANGED OF Clientes.Dir_Correspondencia IN FRAME F_Ubicacion
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Dir_Residencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Dir_Residencia wWin
ON VALUE-CHANGED OF Clientes.Dir_Residencia IN FRAME F_Ubicacion /* Dirección */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Estrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Estrato wWin
ON VALUE-CHANGED OF Clientes.Estrato IN FRAME F_Ubicacion /* Estrato */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Est_Civil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Est_Civil wWin
ON VALUE-CHANGED OF Clientes.Est_Civil IN FRAME F_Segmentacion /* Estado Civil */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Fechas
&Scoped-define SELF-NAME Clientes.Fec_expedicion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_expedicion wWin
ON LEAVE OF Clientes.Fec_expedicion IN FRAME F_Fechas /* Fecha de Expedicion del Documento de Identidad */
DO:
  IF YEAR(DATE(Clientes.Fec_Expedicion:SCREEN-VALUE)) LT 1900 OR
     YEAR(DATE(Clientes.Fec_Expedicion:SCREEN-VALUE)) GT YEAR(TODAY) THEN DO:
     MESSAGE "Año fuera de Rango" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  IF DATE(SELF:SCREEN-VALUE) GT W_Fecha THEN DO:
     MESSAGE "La Fecha de Expedición" SKIP
             "no puede ser mayor a la fecha actual" SKIP
             "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX ERROR.
     SELF:SCREEN-VALUE = STRING(W_Fecha).
     APPLY "ENTRY" TO Clientes.Fec_Expedicion IN FRAME F_Fechas.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_expedicion wWin
ON VALUE-CHANGED OF Clientes.Fec_expedicion IN FRAME F_Fechas /* Fecha de Expedicion del Documento de Identidad */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Fec_fallecido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_fallecido wWin
ON LEAVE OF Clientes.Fec_fallecido IN FRAME F_Fechas /* Fecha de Fallecido */
DO:
  IF YEAR(DATE(Clientes.Fec_fallecido:SCREEN-VALUE)) LT 1900 OR
     YEAR(DATE(Clientes.Fec_fallecido:SCREEN-VALUE)) GT YEAR(TODAY) THEN DO:
            MESSAGE "Año fuera de Rango" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
  END.
  IF DATE(SELF:SCREEN-VALUE) GT W_Fecha THEN DO:
     MESSAGE "La Fecha de Fallecimiento" SKIP
             "no puede ser mayor a la fecha actual" SKIP
             "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX ERROR.
     SELF:SCREEN-VALUE = STRING(W_Fecha).
     APPLY "ENTRY" TO Clientes.Fec_fallecido IN FRAME F_Fechas.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_fallecido wWin
ON VALUE-CHANGED OF Clientes.Fec_fallecido IN FRAME F_Fechas /* Fecha de Fallecido */
DO:
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Fec_IngEmpresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_IngEmpresa wWin
ON LEAVE OF Clientes.Fec_IngEmpresa IN FRAME F_Fechas /* Fecha Ingreso Empresa */
DO:
  IF YEAR(DATE(Clientes.Fec_IngEmpresa:SCREEN-VALUE)) LT 1900 OR
     YEAR(DATE(Clientes.Fec_IngEmpresa:SCREEN-VALUE)) GT YEAR(TODAY) THEN DO:
     MESSAGE "Año fuera de Rango" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  IF DATE(SELF:SCREEN-VALUE) GT W_Fecha THEN DO:
     MESSAGE "La Fecha de Ingreso a la Empresa" SKIP
             "no puede ser mayor a la fecha actual" SKIP
             "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX ERROR.
     SELF:SCREEN-VALUE = STRING(W_Fecha).
     APPLY "ENTRY" TO Clientes.Fec_IngEmpresa IN FRAME F_Fechas.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_IngEmpresa wWin
ON VALUE-CHANGED OF Clientes.Fec_IngEmpresa IN FRAME F_Fechas /* Fecha Ingreso Empresa */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Fec_Nacimiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_Nacimiento wWin
ON LEAVE OF Clientes.Fec_Nacimiento IN FRAME F_Fechas /* Fecha de Nacimiento */
DO:
  IF YEAR(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) LT 1900 OR
     YEAR(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) GT YEAR(TODAY) THEN DO:
     MESSAGE "Año fuera de Rango" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  IF DATE(SELF:SCREEN-VALUE) GT W_Fecha THEN DO:
     MESSAGE "La Fecha de Nacimiento o de Constitución" SKIP
             "no puede ser mayor a la fecha actual" SKIP
             "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX ERROR.
     SELF:SCREEN-VALUE = STRING(W_Fecha).
     APPLY "ENTRY" TO Clientes.Fec_Nacimiento IN FRAME F_Fechas.
     RETURN NO-APPLY.
  END.
  IF DATE(SELF:SCREEN-VALUE) GT DATE(Clientes.Fec_Expedicion:SCREEN-VALUE) THEN DO:
     MESSAGE "La Fecha de Nacimiento no puede ser mayor a la Fecha" SKIP
             "de expedición del documento de identidad." SKIP(1)
             "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX WARNING.
     APPLY "ENTRY" TO Clientes.Fec_Nacimiento IN FRAME F_Fechas.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_Nacimiento wWin
ON VALUE-CHANGED OF Clientes.Fec_Nacimiento IN FRAME F_Fechas /* Fecha de Nacimiento */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Gran_Contribuyente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Gran_Contribuyente wWin
ON VALUE-CHANGED OF Clientes.Gran_Contribuyente IN FRAME F_Segmentacion /* Gran Contribuyente */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Clientes.Gto_Arriendo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Gto_Arriendo wWin
ON LEAVE OF Clientes.Gto_Arriendo IN FRAME F_Economica /* Arrendamientos */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Gto_Arriendo wWin
ON VALUE-CHANGED OF Clientes.Gto_Arriendo IN FRAME F_Economica /* Arrendamientos */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Gto_Familiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Gto_Familiar wWin
ON LEAVE OF Clientes.Gto_Familiar IN FRAME F_Economica /* Familiares */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Gto_Familiar wWin
ON VALUE-CHANGED OF Clientes.Gto_Familiar IN FRAME F_Economica /* Familiares */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Gto_obligacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Gto_obligacion wWin
ON LEAVE OF Clientes.Gto_obligacion IN FRAME F_Economica /* Deducciones Colilla */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Gto_obligacion wWin
ON VALUE-CHANGED OF Clientes.Gto_obligacion IN FRAME F_Economica /* Deducciones Colilla */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Id_Preexistentes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Id_Preexistentes wWin
ON VALUE-CHANGED OF Clientes.Id_Preexistentes IN FRAME F_Otros /* Ha tenido enfermedades preexistentes? */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Id_Privilegiado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Id_Privilegiado wWin
ON VALUE-CHANGED OF Clientes.Id_Privilegiado IN FRAME F_Otros /* Id_Privilegiado */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Id_PuedeCodeudar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Id_PuedeCodeudar wWin
ON VALUE-CHANGED OF Clientes.Id_PuedeCodeudar IN FRAME F_Otros /* Id_PuedeCodeudar */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Id_Retencion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Id_Retencion wWin
ON VALUE-CHANGED OF Clientes.Id_Retencion IN FRAME F_Segmentacion /* Retención en la Fuente */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Clientes.Ing_arriendos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_arriendos wWin
ON LEAVE OF Clientes.Ing_arriendos IN FRAME F_Economica /* Arriendos */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_arriendos wWin
ON VALUE-CHANGED OF Clientes.Ing_arriendos IN FRAME F_Economica /* Arriendos */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Ing_financieros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_financieros wWin
ON LEAVE OF Clientes.Ing_financieros IN FRAME F_Economica /* Financieros */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_financieros wWin
ON VALUE-CHANGED OF Clientes.Ing_financieros IN FRAME F_Economica /* Financieros */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Ing_Honorarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_Honorarios wWin
ON LEAVE OF Clientes.Ing_Honorarios IN FRAME F_Economica /* Honorarios */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_Honorarios wWin
ON VALUE-CHANGED OF Clientes.Ing_Honorarios IN FRAME F_Economica /* Honorarios */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Ing_Otros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_Otros wWin
ON LEAVE OF Clientes.Ing_Otros IN FRAME F_Economica /* Otros Ingresos */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_Otros wWin
ON VALUE-CHANGED OF Clientes.Ing_Otros IN FRAME F_Economica /* Otros Ingresos */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Med_Publicitario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Med_Publicitario wWin
ON VALUE-CHANGED OF Clientes.Med_Publicitario IN FRAME F_Segmentacion /* Medio Publicitario */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Clientes.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nit wWin
ON LEAVE OF Clientes.Nit IN FRAME F_Clientes /* Número */
DO:
    DO WITH FRAME F_Clientes:
        RUN Validar_Tamano NO-ERROR.

        IF ERROR-STATUS:ERROR THEN DO:
            APPLY "entry" TO Clientes.Nit IN FRAME F_Clientes.
            RETURN NO-APPLY.
        END.

        IF Clientes.Nit:SCREEN-VALUE EQ "" THEN DO:
            MESSAGE "Debe entrar el Número de Identifcación del Cliente." SKIP
                    "Si no entra un Número. la operacion de Ingreso"
                    "se cancela"
                VIEW-AS ALERT-BOX.

            APPLY 'choose' TO Btn_Cancelar.
        END.
        ELSE DO:
            FIND FIRST ListaNegra WHERE ListaNegra.Nit EQ Clientes.Nit:SCREEN-VALUE NO-ERROR.
            IF AVAILABLE ListaNegra THEN DO:
                ASSIGN ListaNegra.Id_HaVenido = YES
                       ListaNegra.Fec_HaVenido = W_Fecha.

                /* IF ListaNegra.Codigo EQ 12 THEN DO:*/
                MESSAGE "Este Número de Identificación ha sido encontrado" SKIP
                        "en las listas de Control de la cooperativa." SKIP
                        "con el nombre : " ListaNegra.Nombre " " ListaNegra.Apellido1 " " ListaNegra.Apellido2 SKIP(1)
                        "Se cancela la operación de afiliación!" SKIP
                        "Reporte esta situación al departamento" SKIP
                        "de control interno, Por estar EXCLUIDO"
                    VIEW-AS ALERT-BOX WARNING TITLE "Activo en Lista Negra".

                APPLY 'choose' TO Btn_Cancelar.
            END.

            FIND Clientes WHERE Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE NO-ERROR.
            IF AVAILABLE(Clientes) THEN DO:
                ASSIGN Clientes.Estado = 1.
                
                MESSAGE "Este Cliente ya Existe."
                        "A continuacion se mostrara la informacion Existente"
                    VIEW-AS ALERT-BOX.

                /*** Nuevo **/
                IF Clientes.Tipo_Vinculo = 2 AND clientes.fec_retiro <> ? THEN /* Cliente No Asociado */
                    Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
                ELSE DO:
                    FIND Agencias WHERE Agencias.Agencia = Clientes.Agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE Agencias THEN
                        Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
                    ELSE DO:
                        MESSAGE "La Agencia: " Clientes.Agencia SKIP
                                "No esta matriculada en la tabla de Agencias" SKIP
                                "Se cancela la operación de creación del Registro" SKIP
                                "de información, consulte con el Administrador del" SKIP
                                "Sistema este error!"
                            VIEW-AS ALERT-BOX ERROR.

                        APPLY "choose" TO Btn_Cancelar.
                    END.
                END.

                DISABLE Clientes.Nit.
                Clientes.Nit:BGCOLOR = 18.
                Clientes.Nit:FGCOLOR = 15.
                
                RUN Mostrar_Cliente.
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Niv_Educativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Niv_Educativo wWin
ON VALUE-CHANGED OF Clientes.Niv_Educativo IN FRAME F_Segmentacion /* Nivel Educativo */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Clientes.Nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nombre wWin
ON LEAVE OF Clientes.Nombre IN FRAME F_Clientes /* Nombre */
DO:
  Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes = CAPS(Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes).
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nombre wWin
ON VALUE-CHANGED OF Clientes.Nombre IN FRAME F_Clientes /* Nombre */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Clientes.Nom_Arrendatario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nom_Arrendatario wWin
ON VALUE-CHANGED OF Clientes.Nom_Arrendatario IN FRAME F_Ubicacion /* Arrendatario */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Num_Hijos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Num_Hijos wWin
ON VALUE-CHANGED OF Clientes.Num_Hijos IN FRAME F_Segmentacion /* Número de Hijos */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Per_Acargo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Per_Acargo wWin
ON VALUE-CHANGED OF Clientes.Per_Acargo IN FRAME F_Segmentacion /* Personas a Cargo */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME RActivas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RActivas wWin
ON VALUE-CHANGED OF RActivas IN FRAME F_Relaciones
DO:
  DO WITH FRAME F_Relaciones:
  IF RActivas:SCREEN-VALUE EQ "1" THEN
    Btn_Activas:LABEL = "Borrar".
  ELSE
    Btn_Activas:LABEL = "Activar".
  END.
  APPLY 'value-changed' TO Cmb_Relaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Reestructurado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Reestructurado wWin
ON VALUE-CHANGED OF Clientes.Reestructurado IN FRAME F_Otros /* Reestructurado */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Reportado_fiscalia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Reportado_fiscalia wWin
ON VALUE-CHANGED OF Clientes.Reportado_fiscalia IN FRAME F_Otros /* Reportado a Fiscalia */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Reportado_Procredito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Reportado_Procredito wWin
ON VALUE-CHANGED OF Clientes.Reportado_Procredito IN FRAME F_Otros /* Reportado Procrédito */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Reportado_Super
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Reportado_Super wWin
ON VALUE-CHANGED OF Clientes.Reportado_Super IN FRAME F_Otros /* Reportado Superbancaria */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME RSeleccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RSeleccion wWin
ON VALUE-CHANGED OF RSeleccion IN FRAME F_Clientes
DO:
  ASSIGN FRAME F_Clientes RSeleccion.
  CASE RSeleccion:
    WHEN 1 THEN DO:
        HIDE FRAME F_Ubicacion FRAME F_Fechas FRAME F_Economica FRAME F_Otros FRAME F_Relaciones.
        VIEW FRAME F_Segmentacion.
    END.
    WHEN 2 THEN DO:
        HIDE FRAME F_segmentacion FRAME F_Fechas FRAME F_Economica FRAME F_Otros FRAME F_Relaciones.
        VIEW FRAME F_Ubicacion.
    END.
    WHEN 3 THEN DO:
        HIDE FRAME F_segmentacion FRAME F_Fechas FRAME F_Ubicacion FRAME F_Otros FRAME F_Relaciones.
        VIEW FRAME F_Economica.
    END.
    WHEN 4 THEN DO:
        IF Clientes.Estado:SCREEN-VALUE IN FRAME F_Clientes EQ "2" THEN DO:
           MESSAGE "No se puede actualizar la información de Relaciones" SKIP
                   "ya que el cliente se encuentra retirado." VIEW-AS ALERT-BOX INFORMATION.
           DISABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
           APPLY "entry" TO Btn_Salir IN FRAME F_Clientes.
           RETURN NO-APPLY.
        END.
        FOR EACH T_Relaciones: DELETE T_Relaciones. END.
        OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
        HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Economica FRAME F_Fechas FRAME F_Otros.
        Cmb_Relaciones:SCREEN-VALUE = Cmb_Relaciones:ENTRY(1).
        VIEW FRAME F_Relaciones.
    END.
    WHEN 5 THEN DO:
        HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Economica FRAME F_Otros FRAME F_Relaciones.
        VIEW FRAME F_Fechas.
    END.
    WHEN 6 THEN DO:
        HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Economica FRAME F_Fechas FRAME F_Relaciones.
        VIEW FRAME F_Otros.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Clientes.Salario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Salario wWin
ON LEAVE OF Clientes.Salario IN FRAME F_Economica /* Salario */
DO:
  RUN Totales_Economica.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Salario wWin
ON VALUE-CHANGED OF Clientes.Salario IN FRAME F_Economica /* Salario */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Sancionado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Sancionado wWin
ON VALUE-CHANGED OF Clientes.Sancionado IN FRAME F_Otros /* Sancionado */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Clientes.Sdo_Obligaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Sdo_Obligaciones wWin
ON VALUE-CHANGED OF Clientes.Sdo_Obligaciones IN FRAME F_Economica /* Deudas DataCrédito */
DO:
    RUN Totales_Economica. 
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Sexo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Sexo wWin
ON VALUE-CHANGED OF Clientes.Sexo IN FRAME F_Segmentacion /* Sexo */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Clientes.Tel_Arrendatario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_Arrendatario wWin
ON LEAVE OF Clientes.Tel_Arrendatario IN FRAME F_Ubicacion /* Telefono Arrendatario */
DO:
  IF Clientes.Tel_Arrendatario:SCREEN-VALUE GT " "  THEN DO:
     ASSIGN Tel_Numerico = DEC(Clientes.Tel_Arrendatario:SCREEN-VALUE) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Solo Numèrico...Corrija por favor."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO SELF.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_Arrendatario wWin
ON VALUE-CHANGED OF Clientes.Tel_Arrendatario IN FRAME F_Ubicacion /* Telefono Arrendatario */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Tel_comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_comercial wWin
ON LEAVE OF Clientes.Tel_comercial IN FRAME F_Ubicacion /* Teléfono */
DO:
  DEFI VAR K AS INTEG FORM 99.

  ASSIGN Tel_Numerico = DEC(Clientes.Tel_Comercial:SCREEN-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "Solo Numèrico ...Corrija por favor."
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO SELF.
     RETURN NO-APPLY.
  END.

  /*DO K = 1 TO 20:
        IF  SUBSTRING(Clientes.Tel_Comercial:SCREEN-VALUE,K,1)     EQ " " AND K GT 5
        AND SUBSTRING(Clientes.Tel_Comercial:SCREEN-VALUE,K + 1,1) EQ " " THEN 
            NEXT.

        IF SUBSTRING(Clientes.Tel_Comercial:SCREEN-VALUE,K,1) EQ "-" THEN.
        ELSE DO:
             IF  SUBSTRING(Clientes.Tel_Comercial:SCREEN-VALUE,K,1) GE "0"
             AND SUBSTRING(Clientes.Tel_Comercial:SCREEN-VALUE,K,1) LE "9" THEN.
             ELSE DO: 
                 MESSAGE "Solo Numèrico y -(Guión Ext.)...Corrija por favor."
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
                 APPLY "ENTRY" TO SELF.
                 RETURN NO-APPLY.
             END.
        END.
     END.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_comercial wWin
ON VALUE-CHANGED OF Clientes.Tel_comercial IN FRAME F_Ubicacion /* Teléfono */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Tel_Residencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_Residencia wWin
ON LEAVE OF Clientes.Tel_Residencia IN FRAME F_Ubicacion /* Teléfono */
DO:
  DEFI VAR K AS INTEG FORM "99".

  ASSIGN Tel_Numerico = DEC(Clientes.Tel_Resid:SCREEN-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "Solo Numèrico ...Corrija por favor."
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO SELF.
     RETURN NO-APPLY.
  END.
     
  /*DO K = 1 TO 20:
        IF  SUBSTRING(Clientes.Tel_Resid:SCREEN-VALUE,K,1) EQ " " AND K GT 5
        AND SUBSTRING(Clientes.Tel_Resid:SCREEN-VALUE,K + 1,1) EQ " " THEN 
            NEXT.

        IF SUBSTRING(Clientes.Tel_Resid:SCREEN-VALUE,K,1) EQ "-" THEN.
        ELSE DO:
             IF  SUBSTRING(Clientes.Tel_Resid:SCREEN-VALUE,K,1) GE "0"
             AND SUBSTRING(Clientes.Tel_Resid:SCREEN-VALUE,K,1) LE "9" THEN.
             ELSE DO: 
                 MESSAGE "Solo Numèrico y -(Guión Ext.)...Corrija por favor."
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
                 APPLY "ENTRY" TO SELF.
                 RETURN NO-APPLY.
             END.
        END.
     END.
  END.*/
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_Residencia wWin
ON VALUE-CHANGED OF Clientes.Tel_Residencia IN FRAME F_Ubicacion /* Teléfono */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Clientes.Tipo_Cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Cliente wWin
ON VALUE-CHANGED OF Clientes.Tipo_Cliente IN FRAME F_Clientes /* Tipo de Cliente */
DO:
  RUN TipoCliente.
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Tipo_Identificacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Identificacion wWin
ON LEAVE OF Clientes.Tipo_Identificacion IN FRAME F_Clientes /* Tipo */
DO:
  DO WITH FRAME F_Clientes:
      CASE Clientes.Tipo_Identificacion:SCREEN-VALUE:
         WHEN "NIT" THEN DO:
             Clientes.Tipo_Cliente:SCREEN-VALUE = "3".
             W_Ok = Clientes.Tipo_Cliente:DISABLE("Natural Mayor de Edad").
             W_Ok = Clientes.Tipo_Cliente:DISABLE("Natural Menor de Edad").
             W_Ok = Clientes.Tipo_Cliente:ENABLE("Juridica S.L").
             W_Ok = Clientes.Tipo_Cliente:ENABLE("Juridica C.L").
             ENABLE Clientes.Id_Retencio Clientes.Gran_Contribuyente WITH FRAME F_Segmentacion.
         END.
         OTHERWISE DO:
             Clientes.Tipo_Cliente:SCREEN-VALUE = "1".
             W_Ok = Clientes.Tipo_Cliente:ENABLE("Natural Mayor de Edad").
             W_Ok = Clientes.Tipo_Cliente:ENABLE("Natural Menor de Edad").
             W_Ok = Clientes.Tipo_Cliente:DISABLE("Juridica C.L").
             W_Ok = Clientes.Tipo_Cliente:DISABLE("Juridica S.L").
             DISABLE Clientes.Id_Retencio Clientes.Gran_Contribuyente WITH FRAME F_Segmentacion.
             IF Clientes.Tipo_Identificacion:SCREEN-VALUE EQ "T.I" OR
                Clientes.Tipo_Identificacion:SCREEN-VALUE EQ "R.C" THEN
                 Clientes.Tipo_Cliente:SCREEN-VALUE = "2".
         END.
      END CASE.
      RUN TipoCliente.
  END.
  
  APPLY "ENTRY" TO Clientes.Nit.
  RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Identificacion wWin
ON RETURN OF Clientes.Tipo_Identificacion IN FRAME F_Clientes /* Tipo */
DO:
  APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Identificacion wWin
ON TAB OF Clientes.Tipo_Identificacion IN FRAME F_Clientes /* Tipo */
DO:
  APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Identificacion wWin
ON VALUE-CHANGED OF Clientes.Tipo_Identificacion IN FRAME F_Clientes /* Tipo */
DO:
  DO WITH FRAME F_Clientes:
      CASE Clientes.Tipo_Identificacion:SCREEN-VALUE:
         WHEN "NIT" THEN DO:
             Clientes.Tipo_Cliente:SCREEN-VALUE = "3".
             W_Ok = Clientes.Tipo_Cliente:DISABLE("Natural Mayor de Edad").
             W_Ok = Clientes.Tipo_Cliente:DISABLE("Natural Menor de Edad").
             W_Ok = Clientes.Tipo_Cliente:ENABLE("Juridica S.L").
             W_Ok = Clientes.Tipo_Cliente:ENABLE("Juridica C.L").
             ENABLE Clientes.Id_Retencio Clientes.Gran_Contribuyente WITH FRAME F_Segmentacion.
         END.
         OTHERWISE DO:
             Clientes.Tipo_Cliente:SCREEN-VALUE = "1".
             W_Ok = Clientes.Tipo_Cliente:ENABLE("Natural Mayor de Edad").
             W_Ok = Clientes.Tipo_Cliente:ENABLE("Natural Menor de Edad").
             W_Ok = Clientes.Tipo_Cliente:DISABLE("Juridica C.L").
             W_Ok = Clientes.Tipo_Cliente:DISABLE("Juridica S.L").
             DISABLE Clientes.Id_Retencio Clientes.Gran_Contribuyente WITH FRAME F_Segmentacion.
             IF Clientes.Tipo_Identificacion:SCREEN-VALUE EQ "T.I" OR
                Clientes.Tipo_Identificacion:SCREEN-VALUE EQ "R.C" THEN
                 Clientes.Tipo_Cliente:SCREEN-VALUE = "2".
         END.
      END CASE.
      RUN TipoCliente.
  END.
  
  APPLY "ENTRY" TO Clientes.Nit.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Tipo_Vinculo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Vinculo wWin
ON VALUE-CHANGED OF Clientes.Tipo_Vinculo IN FRAME F_Segmentacion /* codigo Vinculo */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Clientes.Tipo_Vivienda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Vivienda wWin
ON VALUE-CHANGED OF Clientes.Tipo_Vivienda IN FRAME F_Ubicacion /* Tipo de Vivienda */
DO:
DO WITH FRAME F_Ubicacion:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  IF DECIMAL(SELF:SCREEN-VALUE) NE 1 THEN
    ENABLE Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario.
  ELSE DO:
     ASSIGN Clientes.Nom_Arrendatario:SCREEN-VALUE = ""
            Clientes.Tel_Arrendatario:SCREEN-VALUE = "".
     DISABLE Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario.
  END.

END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Tip_Contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tip_Contrato wWin
ON VALUE-CHANGED OF Clientes.Tip_Contrato IN FRAME F_Segmentacion /* Tipo Contrato */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Aportes
&Scoped-define BROWSE-NAME Br_Relaciones
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CodigoCiiu wWin 
PROCEDURE CodigoCiiu :
DO WITH FRAME F_Segmentacion:
  W_NomCiiu = "".
  FIND Ciiu WHERE Ciiu.Tipo EQ 1 AND Ciiu.Grupo EQ Clientes.Grupo NO-LOCK NO-ERROR.
  IF AVAILABLE Ciiu THEN 
     ASSIGN W_NomCiiu = W_NomCiiu + LOWER(TRIM(STRING(Ciiu.Descripcion,"X(15)"))).
  ELSE
     ASSIGN W_NomCiiu = W_NomCiiu + "Sin Grupo".

  FIND Ciiu WHERE Ciiu.Tipo EQ 2 
       AND Ciiu.Grupo EQ Clientes.Grupo
       AND Ciiu.SubGrupo EQ Clientes.SubGrupo NO-LOCK NO-ERROR.
  IF AVAILABLE Ciiu THEN 
     ASSIGN W_NomCiiu = W_NomCiiu + " - " + LOWER(TRIM(STRING(Ciiu.Descripcion,"X(15)"))).
  ELSE
     ASSIGN W_NomCiiu = W_NomCiiu + " - Sin SubGrupo".

  FIND Ciiu WHERE Ciiu.Tipo EQ 3 
      AND Ciiu.Grupo EQ Clientes.Grupo
      AND Ciiu.Subgrupo EQ Clientes.SubGrupo
      AND Ciiu.Codigo_Ciiu EQ Clientes.Codigo_Ciiu NO-LOCK NO-ERROR.
  IF AVAILABLE Ciiu THEN 
     ASSIGN W_NomCiiu = W_NomCiiu + " - " + LOWER(TRIM(STRING(Ciiu.Descripcion,"X(30)"))).
  ELSE
     ASSIGN W_NomCiiu = W_NomCiiu + " - Sin Codigo".
  DISPLAY W_NomCiiu.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "W-ProClientes.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "W-ProClientes.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea_Faltante wWin 
PROCEDURE Crea_Faltante :
DEFINE INPUT PARAMETER BCampo AS CHARACTER FORMAT "X(35)".
DEFINE INPUT PARAMETER BDonde AS CHARACTER FORMAT "X(35)".
CREATE TFalta.
ASSIGN TFalta.TCampo = BCampo
       TFalta.TDonde = BDonde.
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
  DISPLAY Cmb_Agencia Titulo_Nombre Titulo_Ape1 Titulo_Ape2 RSeleccion 
      WITH FRAME F_Clientes IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Estado Clientes.Tipo_Identificacion Clientes.Nit 
          Clientes.Nombre Clientes.Apellido1 Clientes.Apellido2 
          Clientes.Tipo_Cliente 
      WITH FRAME F_Clientes IN WINDOW wWin.
  ENABLE Btn_Consulta Btn_Ingresar Btn_Salir BUTTON-11 BUTTON-1 BUTTON-2 
         Clientes.Nombre Clientes.Apellido1 Clientes.Apellido2 
         Clientes.Tipo_Cliente RSeleccion Foto RECT-2 RECT-216 RECT-4 
      WITH FRAME F_Clientes IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Clientes}
  DISPLAY R_Tel_Comercial R_Nit R_Nombre R_Apellido1 R_Apellido2 
          R_Tel_Residencia R_Relacion 
      WITH FRAME FRelNva IN WINDOW wWin.
  ENABLE R_Tel_Comercial R_Nit R_Nombre R_Apellido1 R_Apellido2 
         R_Tel_Residencia R_Relacion BUTTON-205 BUTTON-206 Btn_SC 
      WITH FRAME FRelNva IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRelNva}
  DISPLAY Tot_Ingresos Tot_Egresos Tot_Activos 
      WITH FRAME F_Economica IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Salario Clientes.Ing_arriendos Clientes.Ing_financieros 
          Clientes.Ing_Honorarios Clientes.Ing_Otros Clientes.Gto_Familiar 
          Clientes.Gto_Arriendo Clientes.Gto_obligacion Clientes.Act_casa 
          Clientes.Act_vehiculo Clientes.Act_inversion Clientes.Sdo_Obligaciones 
      WITH FRAME F_Economica IN WINDOW wWin.
  ENABLE Clientes.Salario Clientes.Ing_arriendos Clientes.Ing_financieros 
         Clientes.Ing_Honorarios Clientes.Ing_Otros Clientes.Gto_Familiar 
         Clientes.Gto_Arriendo Clientes.Gto_obligacion Clientes.Act_casa 
         Clientes.Act_vehiculo Clientes.Act_inversion Clientes.Sdo_Obligaciones 
         RECT-213 RECT-214 RECT-219 RECT-220 
      WITH FRAME F_Economica IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Economica}
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Fec_Ingreso Clientes.Fec_Retiro Clientes.Fec_UltActualiza 
          Clientes.Fec_IngEmpresa Clientes.Fec_Calificacion 
          Clientes.Fec_IniSancion Clientes.Fec_expedicion 
          Clientes.Fec_Nacimiento Clientes.Fec_fallecido 
      WITH FRAME F_Fechas IN WINDOW wWin.
  ENABLE Clientes.Fec_IngEmpresa Clientes.Fec_expedicion 
         Clientes.Fec_Nacimiento BUTTON-180 
      WITH FRAME F_Fechas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Fechas}
  DISPLAY W_NomSegmento 
      WITH FRAME F_Otros IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Id_PuedeCodeudar Clientes.Reestructurado 
          Clientes.Id_Privilegiado Clientes.Sancionado Clientes.Dias_Sancion 
          Clientes.Cod_Segmento Clientes.Reportado_Super 
          Clientes.Aut_CentralRiesgo Clientes.Id_Preexistentes 
          Clientes.Reportado_fiscalia Clientes.Reportado_Procredito 
          Clientes.Calificacion Clientes.Con_Sospechosas 
      WITH FRAME F_Otros IN WINDOW wWin.
  ENABLE RECT-221 RECT-3 RECT-301 Clientes.Id_PuedeCodeudar 
         Clientes.Reestructurado Clientes.Id_Privilegiado Clientes.Sancionado 
         Clientes.Dias_Sancion Btn_Codseg Clientes.Reportado_Super 
         Clientes.Aut_CentralRiesgo Clientes.Id_Preexistentes 
         Clientes.Reportado_fiscalia Clientes.Reportado_Procredito 
         Clientes.Calificacion 
      WITH FRAME F_Otros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Otros}
  DISPLAY Cmb_Relaciones RActivas W_MenRel 
      WITH FRAME F_Relaciones IN WINDOW wWin.
  ENABLE Cmb_Relaciones RActivas Btn_CreRel Btn_Activas BUTTON-116 
         Br_Relaciones 
      WITH FRAME F_Relaciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Relaciones}
  DISPLAY W_NomIngreso W_NomRetiro W_NomCIIU W_NomProfesion W_NomUsuario 
          W_NomEmpresa W_NomCargo Cmb_TipAct 
      WITH FRAME F_Segmentacion IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Tipo_Vinculo Clientes.Sexo Clientes.Est_Civil 
          Clientes.Per_Acargo Clientes.Num_Hijos Clientes.Id_Retencion 
          Clientes.Gran_Contribuyente Clientes.Niv_Educativo 
          Clientes.Med_Publicitario Clientes.Carnet Clientes.Cod_Retiro 
          Clientes.Cod_Ingreso Clientes.Tip_Contrato 
      WITH FRAME F_Segmentacion IN WINDOW wWin.
  ENABLE RECT-1 RECT-209 RECT-215 RECT-217 Clientes.Sexo Clientes.Est_Civil 
         Btn_CodIng Clientes.Per_Acargo Clientes.Num_Hijos 
         Clientes.Id_Retencion Clientes.Gran_Contribuyente 
         Clientes.Niv_Educativo Btn_Ciiu Btn_Profesion Btn_Empresa 
         Clientes.Med_Publicitario Btn_Cargo Clientes.Carnet Cmb_TipAct 
         Clientes.Tip_Contrato 
      WITH FRAME F_Segmentacion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Segmentacion}
  DISPLAY W_UbicacionResidencia Cmb_Zonas W_UbicacionComercial W_CiuNacimiento 
          W_CiuExpedicion 
      WITH FRAME F_Ubicacion IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Dir_Residencia Clientes.Lugar_Residencia 
          Clientes.Tel_Residencia Clientes.Celular Clientes.Tipo_Vivienda 
          Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario 
          Clientes.Dir_comercial Clientes.Tel_comercial Clientes.Lugar_comercial 
          Clientes.Lugar_Nacimiento Clientes.Dir_Correspondencia 
          Clientes.Lugar_expedicion Clientes.Estrato Clientes.Email 
      WITH FRAME F_Ubicacion IN WINDOW wWin.
  ENABLE Btn_Residencia Clientes.Dir_Residencia Clientes.Tel_Residencia 
         Clientes.Celular Cmb_Zonas Clientes.Tipo_Vivienda 
         Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario BUTTON-105 
         Clientes.Dir_comercial Clientes.Tel_comercial Btn_Nacimiento 
         Clientes.Dir_Correspondencia Btn_Documento Clientes.Estrato 
         Clientes.Email RECT-210 RECT-211 RECT-212 RECT-218 
      WITH FRAME F_Ubicacion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Ubicacion}
  ENABLE B_Falta BUTTON-179 
      WITH FRAME F_Falta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Falta}
  DISPLAY Edit_msaje 
      WITH FRAME F_Aportes IN WINDOW wWin.
  ENABLE Bt_FinAporte 
      WITH FRAME F_Aportes IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Aportes}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar wWin 
PROCEDURE Grabar :
DO WITH FRAME F_Clientes:
  ASSIGN Clientes.Agencia = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3)). 
  ASSIGN FRAME F_Clientes 
        Clientes.Nit Clientes.Tipo_Identificacion Clientes.Tipo_Cliente 
        Clientes.Nombre Clientes.Apellido1 Clientes.Apellido2.  
END.
DO WITH FRAME F_Segmentacion:
 ASSIGN FRAME F_Segmentacion 
        Clientes.Tip_Contrato Clientes.Niv_Educativo Clientes.Carnet Clientes.Id_Retencion 
        Clientes.Gran_Contribuyente Clientes.Est_Civil Clientes.Num_Hijos 
        Clientes.Per_ACargo = INTEGER(Per_Acargo:SCREEN-VALUE)
        Clientes.Cod_Profesion Clientes.Cod_Cargo Clientes.Cod_Empresa. 
 ASSIGN Clientes.Usuario       = Clientes.Usuario:SCREEN-VALUE
        Clientes.Codigo_Ciiu Clientes.Grupo Clientes.Subgrupo Clientes.Sexo /*Clientes.Tipo_Vinculo */
        Clientes.Cod_Ingreso Clientes.Med_Publicitario
        Clientes.Tipo_Actividad = Cmb_TipAct:SCREEN-VALUE.
END.
DO WITH FRAME F_Ubicacion:
/* MESSAGE "nacimiento: " Clientes.Lugar_Nacimiento:SCREEN-VALUE W_UbicacionResidencia skip
         "residencia: " clientes.lugar_residencia:SCREEN-VALUE SKIP
         "comercial : " clientes.lugar_comercial:SCREEN-VALUE SKIP
         "expedicion: " clientes.lugar_expedicion:SCREEN-VALUE VIEW-AS ALERT-BOX.*/
 ASSIGN FRAME F_Ubicacion 
        Clientes.Lugar_Residencia Clientes.Dir_Residencia 
        Clientes.Tel_Residencia Clientes.Estrato Clientes.Email Clientes.Lugar_Comercial
        Clientes.Dir_Comercial Clientes.Tel_Comercial 
        Clientes.Lugar_Expedicion /*= Clientes.Lugar_Expedicion:SCREEN-VALUE*/
        Clientes.Lugar_Nacimiento /*= Clientes.Lugar_Nacimiento:SCREEN-VALUE*/
        Clientes.Celular Clientes.Dir_Correspondencia 
        Clientes.Cod_Zona Clientes.Tipo_Vivienda Clientes.Nom_Arrendatario
        Clientes.Tel_Arrendatario Clientes.Usuario. 

END.
DO WITH FRAME F_Fechas:
    ASSIGN Clientes.Fec_UltActualiza:SCREEN-VALUE = STRING(w_fecha).
    ASSIGN FRAME F_Fechas Clientes.Fec_Calificacion Clientes.Fec_expedicion
        Clientes.Fec_IngEmpresa Clientes.Fec_Ingreso Clientes.Fec_IniSancion
        Clientes.Fec_Nacimiento Clientes.Fec_fallecido Clientes.Fec_Retiro
        Clientes.Fec_UltActualiza.
END.
DO WITH FRAME F_Economica:
 ASSIGN FRAME F_Economica Clientes.Act_casa Clientes.Act_inversion Clientes.Act_vehiculo 
        Clientes.Gto_arriendo Clientes.Gto_Familiar Clientes.Gto_obligacion Clientes.Ing_arriendos
        Clientes.Ing_financieros Clientes.Ing_Honorarios Clientes.Ing_otros Clientes.Salario
        Clientes.Sdo_Obligaciones.
END.
DO WITH FRAME F_Otros:
 ASSIGN FRAME F_Otros Clientes.Calificacion Clientes.Aut_CentralRiesgo Clientes.Reportado_Super 
        Clientes.Reportado_Fiscalia Clientes.Reportado_Procredito 
        Clientes.Sancionado = LOGICAL(Clientes.Sancionado:SCREEN-VALUE)
        Clientes.Cod_Segmento Clientes.Dias_Sancion Clientes.Id_Preexistentes
        Clientes.Id_Privilegiado Clientes.Id_PuedeCodeudar Clientes.Reestructurado.
        
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Aportes wWin 
PROCEDURE Grabar_Aportes :
/*Feb.24/05 GAER, Se agregó control para crear a los menores el Pdcto 10.
  
  -------------------------------------------------------------------------*/
  DEFI VAR W_CodPro LIKE Ahorros.Cod_Ahorro INIT 5.  /*Inicia con 5 Mayores*/
  
  FIND Pro_Ahorros WHERE 
       Pro_Ahorros.Tip_Ahorro EQ 4 AND
       Pro_Ahorros.Cod_ahorro EQ 1 NO-LOCK NO-ERROR.
  IF AVAILABLE Pro_Ahorros THEN DO:
     IF Pro_Ahorros.Id_Consecutivo  THEN DO TRANSACTION:   
         /*FIND Ahorros WHERE
             Ahorros.Tip_Ahorro EQ 4            AND
             Ahorros.Cod_Ahorro EQ W_CodPro     AND
             Ahorros.Nit        EQ Clientes.Nit NO-ERROR.
        IF NOT AVAILABLE Ahorros THEN*/
           
        FIND CURRENT Pro_Ahorros NO-ERROR.
        CREATE Ahorros.

        ASSIGN Pro_Ahorros.Num_Consecutivo = Pro_Ahorros.Num_Consecutivo + 1
               Ahorros.Cue_Ahorros     = STRING(Pro_Ahorros.Num_Consecutivo)
               Ahorros.Detalle_Estado  = 1
               Ahorros.Agencia         = W_Agencia
               Ahorros.Tip_Ahorro      = 4
               Ahorros.Cod_ahorro      = 1
               Ahorros.FOR_Pago        = 1
               Ahorros.Nit             = Clientes.Nit
               Ahorros.Per_Liquidacion = 5
               Ahorros.For_Liquidacion = Pro_Ahorros.FOR_Liquidacion
               Ahorros.Estado          = 1
               Ahorros.Tasa            = 0
               Ahorros.Usu_Creacion    = W_Usuario
               Ahorros.IdNombre        = Clientes.Nombre
               Ahorros.IdApellido1     = Clientes.Apellido1
               Ahorros.Cuota           = 0
               Ahorros.Plazo           = 9999
               Ahorros.Per_Deduccion   = 4.

        FIND CURRENT Pro_Ahorros NO-LOCK NO-ERROR.
        FIND CURRENT Ahorros     NO-LOCK NO-ERROR.
     END.   /*fin TRANS.*/
     ELSE RETURN ERROR.            
  END.
  ELSE RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Cliente wWin 
PROCEDURE Imprimir_Cliente :
{Incluido\RepEncabezado.i}
  
  DEFINE VAR W_NomTipoCliente AS CHARACTER FORMAT "X(20)".
  DEFINE VAR W_NomEstado      AS CHARACTER FORMAT "X(10)".
  DEFINE VAR W_NomTipoContrat AS CHARACTER FORMAT "X(14)".
  DEFINE VAR W_NomVinculo     AS CHARACTER FORMAT "X(20)".
  DEFINE VAR W_NomSexo        AS CHARACTER FORMAT "X(6)".
  DEFINE VAR W_NomRet         AS CHARACTER FORMAT "X(2)".
  DEFINE VAR W_NomGran        AS CHARACTER FORMAT "X(2)".

  W_Reporte   = "REPORTE   : INFORMACION DEL CLIENTE - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "CLIENTE: " + Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes + " - " + 
                  Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes + " " +
                  Clientes.Apellido1:SCREEN-VALUE IN FRAME F_Clientes + " " +
                  Clientes.Apellido2:SCREEN-VALUE IN FRAME F_Clientes.
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.
    DO WITH FRAME F_Clientes:
        CASE Clientes.Tipo_Cliente:SCREEN-VALUE:
            WHEN "1" THEN W_NomTipoCliente = "Natural Mayor".
            WHEN "2" THEN W_NomTipoCliente = "Natural Menor".
            WHEN "3" THEN W_NomTipoCliente = "Juridica S.L".
            WHEN "4" THEN W_NomTipoCliente = "Juridica C.L".
        END CASE.
        IF Clientes.Estado:SCREEN-VALUE EQ "1" THEN W_NomEstado = "Activo".
        ELSE W_NomEstado = "Inactivo".
        DISPLAY "INFORMACION GENERAL----------------------------------------------------------------------------------------" AT 1 SKIP(1)
                "Agencia             : "  AT 1 
                 Cmb_Agencia:SCREEN-VALUE FORMAT "X(30)" AT 25
                "Tipo Identificacion : "  AT 1
                 Clientes.Tipo_Identificacion:SCREEN-VALUE FORMAT "X(3)" AT 25
                ": " AT 29
                 Clientes.Nit:SCREEN-VALUE FORMAT "X(14)" AT 33
                "Tipo Cliente        : " AT 1
                 W_NomTipoCliente        AT 25
                "Estado Cliente      : " AT 1
                 W_NomEstado             AT 25 SKIP(2) 
       WITH FRAME a WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
    END.

    DO WITH FRAME F_Segmentacion:
        IF Tip_Contrato:SCREEN-VALUE EQ "1" THEN W_NomtipoContrat = "Indefinido".
        ELSE W_NomtipoContrat = "A Termino Fijo".
        CASE Clientes.Tipo_Vinculo:SCREEN-VALUE:
            WHEN "1" THEN
                W_NomVinculo = "Asociado".
            WHEN "2" THEN
                W_NomVinculo = "Cliente no Asociado".
            WHEN "3" THEN
                W_NomVinculo = "Tercero".
            WHEN "4" THEN
                W_NomVinculo = "Proveedor".
        END CASE.
        IF Clientes.Sexo:SCREEN-VALUE EQ "1" THEN W_NomSexo = "Hombre".
        ELSE W_NomSexo = "Mujer".
        IF Clientes.Id_Retencion:SCREEN-VALUE EQ "YES" THEN W_NomRet = "SI".
        ELSE W_NomRet = "NO".
        IF Clientes.Gran_Contribuyente:SCREEN-VALUE EQ "YES" THEN W_NomGran = "SI".
        ELSE W_NomGran = "NO".
        DISPLAY "INFORMACION SEGMENTACION-------------------------------------------------------------------------------------" AT 1 SKIP(1)
                "Nivel Educativo     : "                        AT 1
                 Niv_Educativo:SCREEN-VALUE FORMAT "X(30)"      AT 25 
                "Profesión           : "                        AT 60 
                 W_NomProfesion:SCREEN-VALUE FORMAT "X(30)"     AT 85
                "Empresa             : "                        AT 1
                 W_NomEmpresa:SCREEN-VALUE FORMAT "X(30)"       AT 25
                "Cargo               : "                        AT 60 
                 W_NomCargo:SCREEN-VALUE FORMAT "X(30)"         AT 85
                "Carnet              : "                        AT 1
                 Clientes.Carnet:SCREEN-VALUE FORMAT "X(30)"    AT 25
                "Tipo Contrato       : "                        AT 60
                 W_NomTipoContrat FORMAT "X(30)"                AT 85 SKIP(2)
                "Tipo de Vinculo     : "                        AT 1
                 W_NomVinculo                                   AT 25
                "Sexo                : "                        AT 60
                 W_NomSexo                                      AT 85
                "Estado Civil        : "                        AT 1
                 Clientes.Est_Civil:SCREEN-VALUE FORMAT "X(30)" AT 25
                "Personas a Cargo    : "                        AT 60
                 Clientes.Per_Acargo:SCREEN-VALUE               AT 85 
                "Número de Hijos     : "                        AT 1
                 Clientes.Num_Hijos:SCREEN-VALUE                AT 25
                "Medio Publicitario  : "                        AT 60
                 Clientes.Med_Publicitario:SCREEN-VALUE FORMAT "X(30)" AT 85
                "Retiene en la Fuente: "                        AT 1
                 W_NomRet                                       AT 25
                "Gran Contribuyente  : "                        AT 60
                 W_NomGran                                      AT 85
                "Código CIIU         : "                        AT 1
                 W_NomCiiu:SCREEN-VALUE FORMAT "X(30)"          AT 25 SKIP
                "Causa de Ingreso    : "                        AT 1
                 W_NomIngreso:SCREEN-VALUE FORMAT "X(30)"       AT 25
                "Causa de Retiro     : "                        AT 1
                 W_NomRetiro:SCREEN-VALUE FORMAT "X(30)"        AT 25 SKIP
                "Usuario Afilio      : "                        AT 1
                 W_NomUsuario:SCREEN-VALUE FORMAT "X(30)"       AT 25 SKIP
            WITH FRAME b WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
     END.
     DEFINE VAR W_Correspondencia AS CHARACTER FORMAT "X(30)".
     DO WITH FRAME F_Ubicacion:
        IF Clientes.DIR_Correspondencia:SCREEN-VALUE EQ "YES" THEN
           W_Correspondencia = "Oficina".
        ELSE
           W_Correspondencia = "Residencia".
        DISPLAY "INFORMACION UBICACIÓN----------------------------------------------------------------------------------------" AT 1 SKIP(1)
                "Ubicación Residencia: "                              AT 1
                W_UbicacionResidencia:SCREEN-VALUE FORMAT "X(30)"     AT 25
                "Dirección Residencia: "                              AT 60
                Clientes.DIR_Residencia:SCREEN-VALUE FORMAT "X(30)"   AT 85
                "Teléfono Residencia : "                              AT 1
                Clientes.Tel_Residencia:SCREEN-VALUE                  AT 25
                "Teléfono Celular    : "                              AT 60
                Clientes.Celular:SCREEN-VALUE                         AT 85
                "Correo electrónico  : "                              AT 1
                Clientes.Email:SCREEN-VALUE FORMAT "X(30)"            AT 25 SKIP(2)
                "Estrato Vivienda    : "                        AT 60
                 Clientes.Estrato:SCREEN-VALUE                  AT 85
                
                "Ubicación Comercial : "                              AT 1
                W_UbicacionComercial:SCREEN-VALUE FORMAT "X(30)"      AT 25
                "Dirección Comercial : "                              AT 60
                Clientes.DIR_Comercial:SCREEN-VALUE FORMAT "X(30)"    AT 85
                "Teléfono Oficina    : "                              AT 1
                Clientes.Tel_Comercial:SCREEN-VALUE                   AT 25 SKIP(2)
                
                "Expedición Documento: "                              AT 1
                W_CiuExpedicion:SCREEN-VALUE FORMAT "X(30)"           AT 25
                "Ciudad Nacimiento   : "                              AT 60
                W_CiuNacimiento:SCREEN-VALUE FORMAT "X(30)"           AT 85
                "Zona                : "                              AT 1
                Cmb_Zonas:SCREEN-VALUE FORMAT "X(30)"                 AT 25
                "Enviar Correo a     : "                              AT 60
                W_Correspondencia                                     AT 85 SKIP
                "Tipo de Vivienda    : "                              AT 1
                Clientes.Tipo_Vivienda:SCREEN-VALUE                   AT 25
           WITH FRAME c WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
       END.
     DO WITH FRAME F_Fechas:
        DISPLAY "FECHAS ------------------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                "Fecha Calificación  : "                              AT 1
                Clientes.Fec_Calificacion:SCREEN-VALUE                AT 25
                "Expedición Documento: "                              AT 60
                Clientes.Fec_expedicion:SCREEN-VALUE                  AT 85
                "Ingreso a la Empresa: "                              AT 1
                Clientes.Fec_IngEmpresa:SCREEN-VALUE                  AT 25
                "Ingreso Cooperativa : "                              AT 60
                Clientes.Fec_Ingreso:SCREEN-VALUE                     AT 85
                "Inicio de Sanción   : "                              AT 1
                Clientes.Fec_IniSancion:SCREEN-VALUE                  AT 25
                "Fecha Nacimiento    : "                              AT 60
                Clientes.Fec_Nacimiento:SCREEN-VALUE                  AT 85
                "Fecha Retiro        : "                              AT 1
                Clientes.Fec_Retiro:SCREEN-VALUE                      AT 25
                "Ultima Actualización: "                              AT 60
                Clientes.Fec_UltActualiza:SCREEN-VALUE                AT 85
                "Fecha fallecido     : "                              AT 1
                Clientes.Fec_fallecido:SCREEN-VALUE                   AT 25
            WITH FRAME d WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
     END.
     DO WITH FRAME F_Economica:
         DISPLAY "FINANCIERA---------------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                 "INGRESOS"              AT 1                                            
                 "EGRESOS"               AT 65 SKIP
                 "Arriendos           : "                             AT 1
                 Clientes.Ing_arriendos:SCREEN-VALUE  FORMAT "X(30)"  AT 25
                 "Arriendos           : "                             AT 60
                 Clientes.Gto_arriendo:SCREEN-VALUE   FORMAT "X(30)"  AT 85
                 "Financieros         : "                             AT 1
                 Clientes.Ing_financieros:SCREEN-VALUE  FORMAT "X(30)" AT 25
                 "Gastos Familiares   : "                             AT 60
                 Clientes.Gto_Familiar:SCREEN-VALUE  FORMAT "X(30)"   AT 85
                 "Salarios            : "                             AT 1
                 Clientes.Salario:SCREEN-VALUE   FORMAT "X(30)"       AT 25
                 "Obligaciones        : "                             AT 60
                 Clientes.Gto_obligacion:SCREEN-VALUE   FORMAT "X(30)" AT 85
                 "Otros/Honorarios,etc: "                             AT 1
                 Clientes.Ing_Honorarios:SCREEN-VALUE   FORMAT "X(30)" AT 25 SKIP
                 Clientes.Ing_Otros:SCREEN-VALUE FORMAT "X(30)"       AT 25 SKIP(2)
                 "     ACTIVOS" AT 1 SKIP
                 "Valor Propiedad     : "                             AT 1
                 Clientes.Act_casa:SCREEN-VALUE  FORMAT "X(30)"       AT 25
                 "Inversiones         : "                             AT 1
                 Clientes.Act_inversion:SCREEN-VALUE  FORMAT "X(30)"  AT 25
                 "Vehiculo            : "                             AT 1
                 Clientes.Act_vehiculo:SCREEN-VALUE  FORMAT "X(30)"   AT 25
             WITH FRAME e WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
     END.
     DO WITH FRAME F_Otros:
         DISPLAY "OTRA INFORMACION---------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                 "Calificación        : "                             AT 1
                 Clientes.Calificacion:SCREEN-VALUE                   AT 25
                 "Aut.Central Riesgo  : "                             AT 60
                 Clientes.Aut_CentralRiesgo:SCREEN-VALUE              AT 85
                 "Reestructurado      : "                             AT 1
                 Clientes.Reestructurado:SCREEN-VALUE                 AT 25
                 "Reportado Super     : "                             AT 60
                 Clientes.Reportado_Super:SCREEN-VALUE                AT 85
                 "Reportado Fiscalía  : "                             AT 1
                 Clientes.Reportado_Fiscalia:SCREEN-VALUE             AT 25
                 "Reportado Procredito: "                             AT 60
                 Clientes.Reportado_Procredito:SCREEN-VALUE           AT 85
                 "Sancionado          : "                             AT 1
                 Clientes.Sancionado:SCREEN-VALUE                     AT 25
                 "Dias de Sanción     : "                             AT 60
                 Clientes.Dias_Sancion:SCREEN-VALUE                   AT 85
             WITH FRAME f WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
     END.

    /**/
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
MESSAGE "La opción de impresión hacia una hoja de calculo" SKIP
        "no esta habilitada para este programa" VIEW-AS ALERT-BOX INFORMATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Relaciones wWin 
PROCEDURE Imprimir_Relaciones :
{Incluido\RepEncabezado.i}

  W_Reporte   = "REPORTE   : RELACIONES :" + STRING(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE IN FRAME F_Relaciones,8,20)) + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "CLIENTE: " + Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes + " - " + 
                  Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes + " " +
                  Clientes.Apellido1:SCREEN-VALUE IN FRAME F_Clientes + " " +
                  Clientes.Apellido2:SCREEN-VALUE IN FRAME F_Clientes.
     
    W_Linea = FILL(W_Raya,132).
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.
    DISPLAY "NOMBRE RELACION        AGE NIT            NOMBRE                             DESCRIPCION     TELÉFONOS" SKIP(1) WITH WIDTH 132.
    FOR EACH T_Relaciones NO-LOCK:
      DISPLAY T_Relaciones.R_Relacion  FORMAT "X(22)"
              T_Relaciones.R_AgeObjeto FORMAT "999"  
              T_Relaciones.R_NitObjeto FORMAT "X(14)"
              T_Relaciones.R_NomObjeto FORMAT "X(35)"
              T_Relaciones.R_NomDescri FORMAT "X(15)"
              T_Relaciones.R_TelObjeto FORMAT "X(30)"
    WITH WIDTH 132 FRAME F-Relaciones NO-BOX USE-TEXT STREAM-IO NO-LABELS.
    END.  
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Campos wWin 
PROCEDURE Inicializar_Campos :
DO WITH FRAME F_Clientes:
        FIND Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
        W_Ok = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Clientes.
        ASSIGN Clientes.Nit:SCREEN-VALUE                 = ""
               Clientes.Tipo_Identificacion:SCREEN-VALUE = "C.C"
               Clientes.Tipo_Cliente:SCREEN-VALUE        = "1"
               Clientes.Nombre:SCREEN-VALUE              = ""
               Clientes.Apellido1:SCREEN-VALUE           = ""
               Clientes.Apellido2:SCREEN-VALUE           = ""
               Clientes.Estado:SCREEN-VALUE              = "1".
     END.
     
     DO WITH FRAME F_Segmentacion:
        ASSIGN Tip_Contrato:SCREEN-VALUE       = "0"
               Niv_Educativo:SCREEN-VALUE      = "Ninguno"
               Carnet:SCREEN-VALUE             = ""
               Codigo_Ciiu:SCREEN-VALUE        = "0"
               Id_Retencion:SCREEN-VALUE       = "NO"
               Gran_Contribuyente:SCREEN-VALUE = "NO"
               Est_Civil:SCREEN-VALUE          = "No Aplica"
               Num_Hijos:SCREEN-VALUE          = "0"
               Per_Acargo:SCREEN-VALUE         = "0"
               Cod_Profesion:SCREEN-VALUE      = "0"
               Cod_Cargo:SCREEN-VALUE          = "0"
               Cod_Empresa:SCREEN-VALUE        = "0"
               Clientes.Usuario:SCREEN-VALUE   = W_Usuario
               Clientes.Codigo_Ciiu:SCREEN-VALUE = "0"
               Clientes.Grupo:SCREEN-VALUE     = "0"
               Clientes.Subgrupo:SCREEN-VALUE  = "0"
               Sexo:SCREEN-VALUE               = "1"
               Tipo_Vinculo:SCREEN-VALUE       = "2"
               Cod_Ingreso:SCREEN-VALUE        = "0"
               Cod_Retiro:SCREEN-VALUE         = "0"
               W_NomIngreso:SCREEN-VALUE       = ""
               W_NomRetiro:SCREEN-VALUE        = "Persona aun Activa"
               W_NomProfesion:SCREEN-VALUE     = ""
               W_NomCargo:SCREEN-VALUE         = ""
               W_NomEmpresa:SCREEN-VALUE       = ""
               W_NomCiiu:SCREEN-VALUE          = ""
               Cmb_TipAct:SCREEN-VALUE         = " ".
        FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
        IF AVAILABLE(Usuarios) THEN DO:
           W_NomUsuario:SCREEN-VALUE = W_Usuario + " : " + Usuarios.Nombre + " - Nit: " + Usuarios.Nit.
           IF Usuarios.Id_OpeOfi THEN ENABLE Cmb_Agencia WITH FRAME F_Clientes.
           ELSE DISABLE Cmb_Agencia WITH FRAME F_Clientes.
        END.
        ELSE W_NomUsuario:SCREEN-VALUE = "Falta Usuario".
     END.
     DO WITH FRAME F_Ubicacion:
        ASSIGN Lugar_Residencia:SCREEN-VALUE    = "0"
               DIR_Residencia:SCREEN-VALUE      = ""
               Tel_Residencia:SCREEN-VALUE      = ""
               Clientes.Email:SCREEN-VALUE      = ""
               Lugar_Comercial:SCREEN-VALUE     = "0"
               Estrato:SCREEN-VALUE            = "1"
               DIR_Comercial:SCREEN-VALUE       = ""
               Tel_Comercial:SCREEN-VALUE       = ""
               Lugar_Expedicion:SCREEN-VALUE    = "0"
               Lugar_Nacimiento:SCREEN-VALUE    = "0"
               Clientes.Celular:SCREEN-VALUE    = ""
               DIR_Correspondencia:SCREEN-VALUE = ""
               W_UbicacionResidencia:SCREEN-VALUE = "No se ha Escogido"
               W_UbicacionComercial:SCREEN-VALUE  = "No se ha Escogido"
               W_CiuNacimiento:SCREEN-VALUE     = "Ciudad No asignado"
               W_CiuExpedicion:SCREEN-VALUE     = "Ciudad No asignada"
               Clientes.Tipo_Vivienda:SCREEN-VALUE    = "1"
               Clientes.Nom_Arrendatario:SCREEN-VALUE = ""
               Clientes.Tel_Arrendatario:SCREEN-VALUE = "".
         DISABLE Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario.
     END.
     DO WITH FRAME F_Fechas:
        ASSIGN Clientes.Fec_Calificacion:SCREEN-VALUE = "?"
               Clientes.Fec_expedicion:SCREEN-VALUE   = "?"
               Clientes.Fec_IngEmpresa:SCREEN-VALUE   = "?"
               Clientes.Fec_Ingreso:SCREEN-VALUE      = STRING(TODAY)
               Clientes.Fec_IniSancion:SCREEN-VALUE   = "?"
               Clientes.Fec_Nacimiento:SCREEN-VALUE   = "?"
               Clientes.Fec_fallecido:SCREEN-VALUE    = "?"
               Clientes.Fec_Retiro:SCREEN-VALUE       = "?"
               Clientes.Fec_UltActualiza:SCREEN-VALUE = STRING(TODAY).
     END.
     DO WITH FRAME F_Economica:
        ASSIGN Clientes.Act_casa:SCREEN-VALUE        = "0"
               Clientes.Act_inversion:SCREEN-VALUE   = "0"
               Clientes.Act_vehiculo:SCREEN-VALUE    = "0"
               Clientes.Gto_arriendo:SCREEN-VALUE    = "0"
               Clientes.Gto_Familiar:SCREEN-VALUE    = "0"
               Clientes.Gto_obligacion:SCREEN-VALUE  = "0"
               Clientes.Ing_arriendos:SCREEN-VALUE   = "0"
               Clientes.Ing_financieros:SCREEN-VALUE = "0"
               Clientes.Ing_Honorarios:SCREEN-VALUE  = "0"
               Clientes.Ing_otros:SCREEN-VALUE       = "0"
               Clientes.Salario:SCREEN-VALUE         = "0"
               Clientes.Sdo_Obligaciones:SCREEN-VALUE   = "0"
               Tot_Ingresos:SCREEN-VALUE             = "0"
               Tot_Egresos:SCREEN-VALUE              = "0"
               Tot_Activos:SCREEN-VALUE              = "0".
     END.
     DO WITH FRAME F_Otros:
        ASSIGN Clientes.Calificacion:SCREEN-VALUE         = ""
               Clientes.Aut_CentralRiesgo:SCREEN-VALUE    = "NO"
               Clientes.Reportado_Super:SCREEN-VALUE      = "NO"
               Clientes.Reportado_Fiscalia:SCREEN-VALUE   = "NO"
               Cod_Segmento:SCREEN-VALUE                  = "0"
               Clientes.Reportado_Procredito:SCREEN-VALUE = "NO"
               Clientes.Sancionado:SCREEN-VALUE           = "NO"
               Clientes.Id_Preexistentes:SCREEN-VALUE     = "NO"
               Clientes.Id_Privilegiado:SCREEN-VALUE      = "0" 
               Clientes.Id_PuedeCodeudar:SCREEN-VALUE     = "Yes".
     END.
     HIDE FRAME F_Ubicacion FRAME F_Fechas FRAME F_Economica FRAME F_Otros FRAME F_Relaciones.
     VIEW FRAME F_Segmentacion.
     ASSIGN RSeleccion:SCREEN-VALUE = "1".

     HIDE FRAME F_Aportes.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
  W_Ok = Clientes.Med_Publicitario:ADD-LAST("WEB") IN FRAME F_Segmentacion.
  Cmb_Zonas:ADD-LAST("0000 - Zona no Escogida") IN FRAME F_Ubicacion.
  FOR EACH Zonas: 
      W_Ok = Cmb_Zonas:ADD-LAST(STRING(Zonas.Cod_zona,"9999") + " - " + Zonas.Nombre) IN FRAME F_Ubicacion.
  END.
  
  W_Ok = Cmb_Relaciones:ADD-LAST("00000 - Ninguna Relacion") IN FRAME F_Relaciones.
  W_Ok = Cmb_Relaciones:ADD-LAST("99999 - Todas las Relaciones") IN FRAME F_Relaciones.
  FOR EACH Varios WHERE Varios.Tipo EQ 3 AND 
           Varios.Codigo NE 11 NO-LOCK:
      W_Ok = Cmb_Relaciones:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME F_Relaciones.
  END.
  
  FIND FIRST Clientes WHERE Clientes.Agencia EQ W_Agencia NO-ERROR.
  IF AVAILABLE(Clientes) THEN 
     RUN Mostrar_Cliente.

  FOR EACH Agencias NO-LOCK:
    W_Ok = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Clientes.
    IF AVAILABLE(Clientes) AND Agencias.Agencia EQ Clientes.Agencia THEN
       Cmb_Agencia:SCREEN-VALUE IN FRAME F_Clientes = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
  END.
/*  W_Ok = Clientes.Tipo_Vinculo:DISABLE("Tercero") IN FRAME F_Segmentacion.
  W_Ok = Clientes.Tipo_Vinculo:DISABLE("Proveedor") IN FRAME F_Segmentacion.*/
  HIDE FRAME F_Falta.
  ENABLE Btn_Ingresar Btn_Borrar Btn_Salir WITH FRAME F_Clientes.
  DISABLE Btn_Salvar Btn_Cancelar Btn_Deshacer Btn_Borrar WITH FRAME F_Clientes.
  HIDE FRAME F_Aportes.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Cliente wWin 
PROCEDURE Mostrar_Cliente :
DO WITH FRAME F_Clientes:
     ASSIGN Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes = Clientes.Nit
            Clientes.Tipo_Identificacion:SCREEN-VALUE = STRING(Clientes.Tipo_Identificacion)
            Clientes.Tipo_Cliente:SCREEN-VALUE = STRING(Clientes.Tipo_Cliente)
            Clientes.Estado:SCREEN-VALUE = STRING(Clientes.Estado).
     W_Ok = Foto:LOAD-IMAGE("imagenes\fotos\0.jpg") NO-ERROR.
     IF Clientes.Fotografia THEN DO:
         gTexto = SEARCH("imagenes\fotos\" + TRIM(Clientes.Nit:SCREEN-VALUE) + ".jpg").
         IF gTexto EQ ? THEN DO:
            MESSAGE "No ha sido capturada la fotografia" SKIP
                    "del cliente." VIEW-AS ALERT-BOX INFORMATION.
            W_Ok = Foto:LOAD-IMAGE("imagenes\fotos\0.jpg") NO-ERROR.
         END.
         ELSE 
            W_Ok = Foto:LOAD-IMAGE(gTexto) NO-ERROR.
     END.
     RUN TipoCliente.
     IF Clientes.Tipo_Identificacion = "NIT" THEN DO:
            W_Ok = Clientes.Tipo_Cliente:DISABLE("Natural Mayor de Edad").
            W_Ok = Clientes.Tipo_Cliente:DISABLE("Natural Menor de Edad").
            W_Ok = Clientes.Tipo_Cliente:ENABLE("Juridica S.L").
            W_Ok = Clientes.Tipo_Cliente:ENABLE("Juridica C.L").
            ENABLE Clientes.Id_Retencio Clientes.Gran_Contribuyente WITH FRAME F_Segmentacion.
     END.
     ELSE DO:
         W_Ok = Clientes.Tipo_Cliente:ENABLE("Natural Mayor de Edad").
         W_Ok = Clientes.Tipo_Cliente:ENABLE("Natural Menor de Edad").
         W_Ok = Clientes.Tipo_Cliente:DISABLE("Juridica C.L").
            W_Ok = Clientes.Tipo_Cliente:DISABLE("Juridica S.L").
            DISABLE Clientes.Id_Retencio Clientes.Gran_Contribuyente WITH FRAME F_Segmentacion.
            IF Clientes.Tipo_Identificacion:SCREEN-VALUE EQ "T.I" OR
               Clientes.Tipo_Identificacion:SCREEN-VALUE EQ "R.C" THEN
                Clientes.Tipo_Cliente:SCREEN-VALUE = "2".
     END.
END.
DO WITH FRAME F_Segmentacion:
   DISPLAY Clientes.Tip_Contrato Clientes.Niv_Educativo Clientes.Carnet Clientes.Codigo_Ciiu
           Clientes.Id_Retencion Clientes.Gran_Contribuyente Clientes.Est_Civil
           Clientes.Num_Hijos Clientes.Per_ACargo Clientes.Cod_Profesion Clientes.Cod_Cargo
           Clientes.Cod_Empresa Clientes.Usuario Clientes.Grupo Clientes.Subgrupo Clientes.Sexo
           Clientes.Tipo_Vinculo Clientes.Cod_Ingreso Clientes.Cod_Retiro
           Clientes.Med_Publicitario WITH FRAME F_Segmentacion.
   IF Clientes.Med_Publicitario LE " " THEN
      Clientes.Med_Publicitario:SCREEN-VALUE = "".

   Cmb_TipAct:SCREEN-VALUE = Clientes.Tipo_Actividad.

   IF Clientes.Tipo_Vinculo GT 2 THEN
      Clientes.Tipo_Vinculo:SCREEN-VALUE IN FRAME F_Segmentacion = "2".
   
   FIND Varios WHERE Varios.Tipo EQ 4 AND Varios.Codigo EQ Clientes.Cod_Ingreso NO-LOCK NO-ERROR.
   IF AVAILABLE(Varios) THEN W_NomIngreso:SCREEN-VALUE = Varios.Descripcion.
                        ELSE W_NomIngreso:SCREEN-VALUE = "Causal aun no escogida".
   FIND Varios WHERE Varios.Tipo EQ 5 AND Varios.Codigo EQ Clientes.Cod_Retiro NO-LOCK NO-ERROR.
   IF AVAILABLE(Varios) THEN W_NomRetiro:SCREEN-VALUE = Varios.Descripcion.
                        ELSE W_NomRetiro:SCREEN-VALUE = "Persona aun Activa".
   FIND Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ Clientes.Cod_Profesion NO-LOCK NO-ERROR.
   IF AVAILABLE(Varios) THEN W_NomProfesion:SCREEN-VALUE = Varios.Descripcion.
                        ELSE W_NomProfesion:SCREEN-VALUE = "Profesión aun no escogida".
   FIND Varios WHERE Varios.Tipo EQ 2 AND Varios.Codigo EQ Clientes.Cod_Cargo NO-LOCK NO-ERROR.        
   IF AVAILABLE(Varios) THEN W_NomCargo:SCREEN-VALUE = Varios.Descripcion.
                        ELSE W_NomCargo:SCREEN-VALUE = "Cargo aun no escogido".
   FIND Usuarios WHERE Usuarios.Usuario EQ Clientes.Usuario NO-LOCK NO-ERROR.
   IF AVAILABLE(Usuarios) THEN W_NomUsuario:SCREEN-VALUE = Clientes.Usuario + " : " +
                                                           Usuarios.Nombre + " - Nit: " + Usuarios.Nit.
   ELSE DO:
     FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
     IF AVAILABLE Usuarios THEN
        ASSIGN W_NomUsuario:SCREEN-VALUE = W_Usuario + " : " +
                                           Usuarios.Nombre + " - Nit: " + Usuarios.Nit
               Clientes.Usuario:SCREEN-VALUE = W_Usuario.
     ELSE W_NomUsuario:SCREEN-VALUE = "Falta Usuario".
   END.
   RUN CodigoCiiu.
   
   FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
   IF AVAILABLE(Empresas) THEN  
      W_NomEmpresa:SCREEN-VALUE = Empresas.Alias_Empresa.
     /*FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN
        W_NomEmpresa:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
   END.
   ELSE W_NomEmpresa:SCREEN-VALUE = "Empresa Aun no Escogida".*/
     
END.
DO WITH FRAME F_Ubicacion:
   DISPLAY Clientes.Lugar_Residencia Clientes.DIR_Residencia Clientes.Tel_Residencia
           Clientes.Email Clientes.Lugar_Comercial Clientes.DIR_Comercial Clientes.Estrato
           Clientes.Tel_Comercial 
           Clientes.Lugar_Expedicion 
           Clientes.Lugar_Nacimiento 
           Clientes.DIR_Correspondencia Clientes.Cod_Zona Clientes.Celular
           Clientes.Tipo_Vivienda Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario WITH FRAME F_Ubicacion.
   IF Clientes.Tipo_Vivienda:SENSITIVE EQ YES THEN DO:
      IF Clientes.Tipo_Vivienda EQ 2 THEN 
         ENABLE Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario.
      ELSE DISABLE Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario.
   END.
   ELSE
      DISABLE Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario.
   RUN Ubicacion.
END.
DO WITH FRAME F_Fechas:
/*   IF (clientes.tipo_vinculo EQ 2) AND ((clientes.fec_ingreso LE clientes.fec_retiro)  */
/*       OR clientes.fec_ingreso = ?) THEN                                               */
/*      ASSIGN Clientes.Agencia     = w_agencia                                          */
/*             Clientes.Fec_Ingreso = w_fecha.                                           */
  DISPLAY Clientes.Fec_Calificacion Clientes.Fec_expedicion Clientes.Fec_IngEmpresa
       Clientes.Fec_Ingreso Clientes.Fec_IniSancion Clientes.Fec_Nacimiento Clientes.Fec_fallecido
       Clientes.Fec_Retiro Clientes.Fec_UltActualiza WITH FRAME F_Fechas.
END.
DO WITH FRAME F_Economica:
  Clientes.Sdo_Obligaciones:SCREEN-VALUE = "0".
  DISPLAY Clientes.Act_casa Clientes.Act_inversion Clientes.Act_vehiculo
          Clientes.Gto_arriendo Clientes.Gto_Familiar Clientes.Gto_obligacion
          Clientes.Ing_arriendos Clientes.Ing_financieros Clientes.Ing_Honorarios
          Clientes.Ing_Otros Clientes.Salario Clientes.Sdo_Obligaciones WITH FRAME F_Economica.
   ASSIGN Tot_Ingresos:SCREEN-VALUE = STRING(Clientes.Salario + Clientes.Ing_Arriendos + Clientes.Ing_Financieros + Clientes.Ing_Honorarios + Clientes.Ing_Otros)
          Tot_Egresos:SCREEN-VALUE = STRING(Clientes.Gto_Arriendo + Clientes.Gto_Familiar + Clientes.Gto_Obligacion)
          Tot_Activos:SCREEN-VALUE = STRING(Clientes.Act_Casa + Clientes.Act_Vehiculo + Clientes.Act_Inversion).
   IF Clientes.Tipo_Cliente GT 2 THEN
      ASSIGN Clientes.Gto_Familiar:LABEL = "Gastos Empresariales"
             Clientes.Salario:LABEL = "Ingresos Empresa".
   ELSE
      ASSIGN Clientes.Gto_Familiar:LABEL = "Familiares"
             Clientes.Salario:LABEL = "Salario".
END.
DO WITH FRAME F_Otros:
  DISPLAY Clientes.Calificacion Clientes.Aut_CentralRiesgo Clientes.Reestructurado
          Clientes.Reportado_Super Clientes.Reportado_Fiscalia Clientes.Reportado_Procredito
          Clientes.Sancionado Clientes.Dias_Sancion Clientes.Id_Preexistentes 
          Clientes.Id_Privilegiado Clientes.Id_PuedeCodeudar WITH FRAME F_Otros.

   FIND Varios WHERE Varios.Tipo EQ 6 AND Varios.Codigo EQ Clientes.Cod_Segmento NO-LOCK NO-ERROR.
   IF AVAILABLE(Varios) THEN W_NomSegmento:SCREEN-VALUE = Varios.Descripcion.
                            ELSE W_NomSegmento:SCREEN-VALUE = "00000 - Segmento aun no escogida".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
IF W_Inf EQ "Relaciones" THEN RUN Imprimir_Relaciones.
IF W_Inf EQ "Cliente"    THEN RUN Imprimir_Cliente.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TipoCliente wWin 
PROCEDURE TipoCliente :
DO WITH FRAME F_Clientes:
 CASE INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE):
   WHEN 1 OR WHEN 2 THEN DO:
    ASSIGN Titulo_Nombre:SCREEN-VALUE         = "Nombre"
           Titulo_Ape1:SCREEN-VALUE           = "Primer Apellido"
           Titulo_Ape2:SCREEN-VALUE           = "Segundo Apellido"
           Clientes.Apellido1:BGCOLOR         = 15
           Clientes.Apellido2:BGCOLOR         = 15.
    IF Clientes.Tipo_Cliente:SCREEN-VALUE EQ "2" THEN DO:
      IF Clientes.Tipo_identificacion:SCREEN-VALUE NE "T.I" THEN 
       IF Clientes.Tipo_Identificacion:SCREEN-VALUE NE "R.C" THEN DO:
         MESSAGE "Un Cliente menor de edad debe identificarse con" SKIP
                 "Tarjeta de identidad o Registro Civil." SKIP(1)
                 "El sistema pondrá automáticamente T.I como" SKIP
                 "Configuración tentativa, la cual usted como" SKIP
                 "usuario podrá variar según el documento real"
                 VIEW-AS ALERT-BOX WARNING.
         Clientes.Tipo_Identificacion:SCREEN-VALUE = "T.I".
         IF AVAILABLE Clientes THEN Clientes.Tipo_Identificacion = "T.I".
         ENABLE Btn_Salvar Clientes.Tipo_Identificacion WITH FRAME F_Clientes.
       END.
    END.
    IF Clientes.Tipo_Cliente:SCREEN-VALUE EQ "1" THEN DO:
      IF Clientes.Tipo_identificacion:SCREEN-VALUE NE "C.C" THEN
         IF Clientes.Tipo_Identificacion:SCREEN-VALUE NE "C.E" THEN DO:
           MESSAGE "Un Cliente mayor de edad debe identificarse con" SKIP
                   "Cédula de Ciudadanía o Cédula de Extranjería." SKIP(1)
                   "El sistema pondrá automáticamente C.C como" SKIP
                   "Configuración tentativa, la cual usted como" SKIP
                   "usuario podrá variar según el documento real"
                   VIEW-AS ALERT-BOX WARNING.
           Clientes.Tipo_Identificacion:SCREEN-VALUE = "C.C".
           IF AVAILABLE Clientes THEN Clientes.Tipo_Identificacion = "C.C".
           ENABLE Btn_Salvar WITH FRAME F_Clientes.
         END.
    END.
    ENABLE Clientes.Apellido1 Clientes.Apellido2 WITH FRAME F_Clientes.
       /*campos habilitados*/
       ENABLE Clientes.Est_Civil Clientes.Per_Acargo Clientes.Num_Hijos
       /*seg*/ Clientes.Niv_Educativo Btn_Profesion Btn_Empresa Btn_Cargo
               Clientes.Med_Publicitario Clientes.Tip_Contrato Clientes.Sexo
               WITH FRAME F_Segmentacion.
       Clientes.Est_Civil:SCREEN-VALUE IN FRAME F_Segmentacion = "Soltero".
       ENABLE Btn_Residencia Clientes.DIR_Residencia Clientes.Tel_Residencia
       /*Ubi*/ Clientes.Celular Clientes.Tipo_Vivienda Clientes.Estrato
               Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario
               Btn_Nacimiento Btn_Documento WITH FRAME F_Ubicacion.
       ENABLE Clientes.Fec_IngEmpresa Clientes.Fec_Expedicion
       /*fec*/ WITH FRAME F_Fechas.
       Clientes.Gto_Familiar:LABEL IN FRAME F_Economica = "Familiares".
       Clientes.Salario:LABEL IN FRAME F_Economica = "Salario".
       Clientes.Fec_Nacimiento:LABEL IN FRAME F_Fechas = "Fecha de Nacimiento".
       ENABLE Clientes.Id_Preexistentes WITH FRAME F_Otros.
    IF AVAILABLE(Clientes) THEN
       ASSIGN Clientes.Nombre:SCREEN-VALUE       = Clientes.Nombre
              Clientes.Apellido1:SCREEN-VALUE    = Clientes.Apellido1
              Clientes.Apellido2:SCREEN-VALUE    = Clientes.Apellido2.
    END.
   OTHERWISE DO:
    ASSIGN Titulo_Nombre:SCREEN-VALUE         = "Razón Social"
           Titulo_Ape1:SCREEN-VALUE           = ""
           Titulo_Ape2:SCREEN-VALUE           = ""
           Clientes.Apellido1:SCREEN-VALUE    = ""
           Clientes.Apellido2:SCREEN-VALUE    = ""
           Clientes.Apellido1:BGCOLOR         = 17
           Clientes.Apellido2:BGCOLOR         = 17.
    IF DECIMAL(Clientes.Tipo_Cliente:SCREEN-VALUE) GT 2 AND
        Clientes.Tipo_identificacion:SCREEN-VALUE NE "NIT" THEN DO:
       MESSAGE "Un Cliente Jurídico debe identificarse con NIT" SKIP(1)
               "El sistema pondrá automáticamente NIT como" SKIP
               "Configuración definitiva"
               VIEW-AS ALERT-BOX WARNING.
       Clientes.Tipo_Identificacion:SCREEN-VALUE = "NIT".
       IF AVAILABLE Clientes THEN Clientes.Tipo_Identificacion = "NIT".
    END.
    DISABLE Clientes.Apellido1 Clientes.Apellido2 WITH FRAME F_Clientes.
       /*campos deshabilitados*/
       DISABLE Clientes.Est_Civil Clientes.Per_Acargo Clientes.Num_Hijos
       /*seg*/ Clientes.Niv_Educativo Btn_Profesion /* Btn_Empresa */ Btn_Cargo
               Clientes.Med_Publicitario Clientes.Tip_Contrato Clientes.Sexo
               WITH FRAME F_Segmentacion.
       Clientes.Est_Civil:SCREEN-VALUE IN FRAME F_Segmentacion = "No Aplica".
       Clientes.Niv_Educativo:SCREEN-VALUE IN FRAME F_Segmentacion = "Ninguno".
       Clientes.Tip_Contrato:SCREEN-VALUE IN FRAME F_Segmentacion = "0".
       DISABLE Btn_Residencia Clientes.DIR_Residencia Clientes.Tel_Residencia
       /*Ubi*/ Clientes.Celular Clientes.Tipo_Vivienda Clientes.Estrato
               Clientes.Nom_Arrendatario Clientes.Tel_Arrendatario
               Btn_Nacimiento Btn_Documento WITH FRAME F_Ubicacion.
       DISABLE Clientes.Fec_IngEmpresa Clientes.Fec_Expedicion
       /*fec*/ WITH FRAME F_Fechas.
       Clientes.Gto_Familiar:LABEL IN FRAME F_Economica = "Gastos Empresariales".
       Clientes.Salario:LABEL IN FRAME F_Economica = "Ingresos Empresa".
       Clientes.Fec_Nacimiento:LABEL IN FRAME F_Fechas = "Fecha de Constitución de la Empresa".
       DISABLE Clientes.Id_Preexistentes WITH FRAME F_Otros.
    IF AVAILABLE(Clientes) THEN Clientes.Nombre:SCREEN-VALUE       = Clientes.Nombre.
   END.
 END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Totales_Economica wWin 
PROCEDURE Totales_Economica :
DO WITH FRAME F_Economica:
  ASSIGN Tot_Ingresos = 0
         Tot_Ingresos:SCREEN-VALUE = STRING(Tot_Ingresos)
         Tot_Egresos = 0
         Tot_Egresos:SCREEN-VALUE = STRING(Tot_Egresos)
         Tot_Activos = 0
         Tot_Activos:SCREEN-VALUE = STRING(Tot_Activos).
  ASSIGN Tot_Ingresos:SCREEN-VALUE = STRING(DECIMAL(Clientes.Salario:SCREEN-VALUE) + 
                                            DECIMAL(Clientes.Ing_Arriendos:SCREEN-VALUE) + 
                                            DECIMAL(Clientes.Ing_Financieros:SCREEN-VALUE) + 
                                            DECIMAL(Clientes.Ing_Honorarios:SCREEN-VALUE) + 
                                            DECIMAL(Clientes.Ing_Otros:SCREEN-VALUE)).
 ASSIGN  Tot_Egresos:SCREEN-VALUE = STRING(DECIMAL(Clientes.Gto_Arriendo:SCREEN-VALUE) + 
                                           DECIMAL(Clientes.Gto_Familiar:SCREEN-VALUE) + DECIMAL(Clientes.Sdo_Obligacion:SCREEN-VALUE) +
                                           DECIMAL(Clientes.Gto_Obligacion:SCREEN-VALUE)).
 ASSIGN  Tot_Activos:SCREEN-VALUE = STRING(DECIMAL(Clientes.Act_Casa:SCREEN-VALUE) + 
                                           DECIMAL(Clientes.Act_Vehiculo:SCREEN-VALUE) + 
                                           DECIMAL(Clientes.Act_Inversion:SCREEN-VALUE)).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ubicacion wWin 
PROCEDURE Ubicacion :
/*residencia*/
     DEFINE VARIABLE W_NUbicacion AS CHARACTER FORMAT "X(50)".
     /*residencia*/
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Clientes.Lugar_Residencia NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Residencia,1,2) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.

/*     MESSAGE W_Nubicacion clientes.lugar_residencia VIEW-AS ALERT-BOX.*/
     W_UbicacionResidencia = LC(W_NUbicacion).
     
     DISPLAY W_UbicacionResidencia WITH FRAME F_Ubicacion.
     
     /*comercial*/
     W_NUbicacion = "".
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Clientes.Lugar_Comercial NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Comercial,1,5) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Comercial,1,2) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
     
     W_UbicacionComercial:SCREEN-VALUE IN FRAME F_Ubicacion = LC(W_NUbicacion).

     /*nacimiento*/
     ASSIGN W_NUbicacion = ""
            W_CiuNacimiento:SCREEN-VALUE = "".
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Clientes.Lugar_Nacimiento NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN W_NUbicacion  = Ubicacion.Nombre.     

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Nacimiento,1,5) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Nacimiento,1,2) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
     W_CiuNacimiento:SCREEN-VALUE IN FRAME F_Ubicacion = LC(W_NUbicacion).

     /*expedicion*/
     ASSIGN W_NUbicacion = ""
            W_CiuExpedicion:SCREEN-VALUE = "".
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Clientes.Lugar_Expedicion NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN W_NUbicacion  = Ubicacion.Nombre.
     
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Expedicion,1,5) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Expedicion,1,2) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
     W_CiuExpedicion:SCREEN-VALUE = LC(W_NUbicacion).

/*zonas*/
  FIND Zonas WHERE Zonas.Cod_Zona EQ Clientes.Cod_Zona NO-LOCK NO-ERROR.
  IF AVAILABLE(Zonas) THEN ASSIGN Cmb_Zonas:SCREEN-VALUE IN FRAME F_Ubicacion = (STRING(Zonas.Cod_Zona,"9999") + " - " + Zonas.Nombre).
  ELSE ASSIGN Cmb_Zonas:SCREEN-VALUE IN FRAME F_Ubicacion = "0000 - Zona no Escogida".

 /* IF Clientes.Tipo_Cliente GT 2 THEN
     ASSIGN W_UbicacionResidencia:SCREEN-VALUE = ""
            W_CiuNacimiento:SCREEN-VALUE = ""
            W_CiuExpedicion:SCREEN-VALUE = "".*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validacion_Informacion wWin 
PROCEDURE Validacion_Informacion :
/*DEFINE VAR Wk_Edad AS INTEGER FORMAT "99.99".*/
DO WITH FRAME F_Clientes:
   IF Clientes.Nombre:SCREEN-VALUE EQ "" THEN
      RUN Crea_Faltante(INPUT "Nombre", INPUT "Principal").
   IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) LT 3 THEN DO:
      IF Clientes.Apellido1:SCREEN-VALUE EQ "" THEN
         RUN Crea_Faltante(INPUT "Primer Apellido", INPUT "Principal").
   END.
END.
DO WITH FRAME F_Segmentacion:
   IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) LT 3 THEN DO:
      IF Clientes.Est_Civil:SCREEN-VALUE EQ "No Aplica" THEN
         RUN Crea_Faltante(INPUT "Estado Civil", INPUT "Segmentacion").
   END.
   /*comunes a todos*/
   IF W_NomCiiu:SCREEN-VALUE EQ "" OR
      W_NomCiiu:SCREEN-VALUE BEGINS "Sin grupo" THEN
      RUN Crea_Faltante(INPUT "Codigo Ciiu", INPUT "Segmentacion").
   IF W_NomIngreso:SCREEN-VALUE EQ "Causal aun no escogida" OR 
      W_NomIngreso:SCREEN-VALUE EQ "" THEN
      RUN Crea_Faltante(INPUT "Causal de Ingreso", INPUT "Segmentacion").
END.
DO WITH FRAME F_Ubicacion:
   IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) LT 3 THEN DO:
      IF W_UbicacionResidencia:SCREEN-VALUE = "" THEN
         RUN Crea_Faltante(INPUT "Depto y Municipio. Residencia", INPUT "Ubicación").
      IF Clientes.DIR_Residencia:SCREEN-VALUE EQ "" THEN
         RUN Crea_Faltante(INPUT "Dirección Residencia", INPUT "Ubicación").
      IF Clientes.Tel_Residencia:SCREEN-VALUE EQ "" THEN
         RUN Crea_Faltante(INPUT "Teléfono Residencia", INPUT "Ubicación").
      /*IF Clientes.Tipo_Vivienda:SCREEN-VALUE EQ "2" AND 
         (Clientes.Nom_Arrendatario:SCREEN-VALUE EQ "" OR
          Clientes.Tel_Arrendatario:SCREEN-VALUE EQ "") THEN
         RUN Crea_Faltante(INPUT "Información Arrendatario", INPUT "Ubicación").*/
   END.
   ELSE DO:
      IF Clientes.DIR_Comercial:SCREEN-VALUE EQ "" THEN
         RUN Crea_Faltante(INPUT "Dirección Comercial", INPUT "Ubicación").
      IF Clientes.Tel_Comercial:SCREEN-VALUE EQ "" THEN
         RUN Crea_Faltante(INPUT "Teléfono Comercial", INPUT "Ubicación").
      IF W_UbicacionComercial:SCREEN-VALUE EQ "" THEN
         RUN Crea_Faltante(INPUT "Depto y Municipio Comercial", INPUT "Ubicación").
   END.

   IF Clientes.DIR_Correspondencia:SCREEN-VALUE EQ "yes" AND 
      Clientes.Dir_Comercial:SCREEN-VALUE EQ "" THEN DO:
      RUN Crea_Faltante(INPUT "Dir.Comercial x Envio de Correspondencia", INPUT "Ubicación").
   END.
END.

DO WITH FRAME F_Economica:
   IF  INTEGER(Clientes.Tip_Contrato:SCREEN-VALUE IN FRAME F_Segmentacion) LE 2
   AND INTEGER(Clientes.Tip_Contrato:SCREEN-VALUE) GT 0 THEN DO:
       IF DECIMAL(Clientes.Salario:SCREEN-VALUE IN FRAME F_Economica) LE 0 THEN 
          RUN Crea_Faltante(INPUT "Salario", INPUT "Económica").  

       IF W_NomEmpresa:SCREEN-VALUE IN FRAME F_Segmentacion EQ "" OR 
          W_NomEmpresa:SCREEN-VALUE IN FRAME F_Segmentacion EQ "Empresa aun no escogida"  THEN
          RUN Crea_Faltante(INPUT "Empresa", INPUT "Segmentacion").

       IF Clientes.Fec_IngEmpresa:SCREEN-VALUE IN FRAME F_Fechas EQ "" THEN
          RUN Crea_Faltante(INPUT "Fecha Ingreso a Empresa", INPUT "Fechas").
   END.

   IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) LE 2 THEN DO:
      IF  Clientes.Tipo_Vivienda:SCREEN-VALUE IN FRAME F_Ubicacion EQ "1" 
      AND DECIMAL(Clientes.Act_Casa:SCREEN-VALUE IN FRAME F_Economica) LE 0 THEN
          RUN Crea_Faltante(INPUT "Valor Propiedad", INPUT "Económica").
   END.
END.

DO WITH FRAME F_Fechas:
   IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) LE 2 THEN DO:
      IF Clientes.Fec_Nacimiento:SCREEN-VALUE EQ "" THEN
         RUN Crea_Faltante(INPUT "Fecha Nacimiento", INPUT "Fechas").

      Wk_Edad = YEAR(W_FEcha) - YEAR(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)).

      IF MONTH(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) LT MONTH(W_Fecha) THEN.
      ELSE DO:
         IF MONTH(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) EQ MONTH(W_fecha) AND
            DAY(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) GT DAY(W_fecha) THEN 
               Wk_edad = Wk_edad - 1.
         ELSE 
           IF MONTH(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) GT MONTH(W_fecha) THEN 
               Wk_edad = Wk_edad - 1.
      END.

      IF Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes EQ "2" AND 
         Wk_Edad GE 18 THEN DO:
         MESSAGE "El Cliente esta identificado como menor de edad" SKIP
                 "sin embargo la fecha de nacimiento no coincide" SKIP
                 "con esta identificación, según la fecha de" SKIP
                 "nacimiento el cliente tendría: " Wk_Edad " años" VIEW-AS ALERT-BOX ERROR.
         RUN Crea_Faltante(INPUT "Fecha nacimiento", INPUT "Fechas (Fecha inconsistente)").
      END.

      IF Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes EQ "1" AND 
         Wk_Edad LT 18 THEN DO:
         MESSAGE "El Cliente esta identificado como mayor de edad" SKIP
                 "sin embargo la fecha de nacimiento no coincide" SKIP
                 "con esta identificación, según la fecha de" SKIP
                 "nacimiento el cliente tendría: " Wk_Edad " años" VIEW-AS ALERT-BOX ERROR.
         RUN Crea_Faltante(INPUT "Fecha nacimiento", INPUT "Fechas (Fecha inconsistente)").
      END.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Tamano wWin 
PROCEDURE Validar_Tamano :
DEFINE VAR XTemp AS DECIMAL FORMAT "999999999999999".
DEFINE VAR XPos  AS INTEGER FORMAT "99".
DO WITH FRAME F_Clientes:
  CASE Clientes.Tipo_Identificacion:SCREEN-VALUE:
    WHEN "C.C" OR WHEN "C.E" OR WHEN "R.C" THEN DO:
      IF LENGTH(Clientes.Nit:SCREEN-VALUE) LT 5 OR LENGTH(Clientes.Nit:SCREEN-VALUE) GT 11 THEN DO:
         MESSAGE "El documento de identidad debe contener entre 5 y 11 caracteres" SKIP
                 "digite de nuevo el número de documento." VIEW-AS ALERT-BOX INFORMATION.
         RETURN ERROR.
      END.
      ASSIGN XTemp = DECIMAL(Clientes.Nit:SCREEN-VALUE) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "El documento de identidad no puede contener ningún caracter diferente" SKIP
                 "a los números del 0 al 9. Rectifique" VIEW-AS ALERT-BOX INFORMATION.
         APPLY "entry" TO Clientes.Tipo_Identificacion.
         RETURN ERROR.
      END.
    END.
    WHEN "NIT" THEN DO:
      IF LENGTH(Clientes.Nit:SCREEN-VALUE) LT 7 OR LENGTH(Clientes.Nit:SCREEN-VALUE) GT 12 THEN DO:
         MESSAGE "El Numero de documento debe contener entre 7 y 12 caracteres" 
                 VIEW-AS ALERT-BOX INFORMATION.
         RETURN ERROR.
      END.
      XPos = (LENGTH(Clientes.Nit:SCREEN-VALUE)) - 1.
      IF SUBSTRING(Clientes.Nit:SCREEN-VALUE,XPos,1) NE "-" THEN DO:
         MESSAGE "El documento de identidad debe tener como penultimo digito un guion" SKIP
                 "digite de nuevo el número de documento." VIEW-AS ALERT-BOX INFORMATION.
         RETURN ERROR.
      END.
    END.
    WHEN "T.I" THEN DO:
      IF LENGTH(Clientes.Nit:SCREEN-VALUE) LT 10 THEN DO:
         MESSAGE "El documento de identidad debe contener entre más de 9 caracteres" SKIP
                 "digite de nuevo el número de documento." VIEW-AS ALERT-BOX INFORMATION.
         RETURN ERROR.
      END.
      ASSIGN XTemp = DECIMAL(Clientes.Nit:SCREEN-VALUE) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "El documento de identidad no puede contener ningún caracter diferente" SKIP
                 "a los números del 0 al 9. Rectifique" VIEW-AS ALERT-BOX INFORMATION.
         APPLY "entry" TO Clientes.Tipo_Identificacion.
         RETURN ERROR.
      END.
    END.
  END CASE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica_Remanente wWin 
PROCEDURE Verifica_Remanente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST Ahorros WHERE Ahorros.Nit        EQ Clientes.Nit AND
                         Ahorros.cod_ahorro EQ 216          AND
                         Ahorros.estado     EQ 1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE(Ahorros) THEN DO:
   FIND Pro_Ahorros WHERE 
        Pro_Ahorros.Tip_Ahorro EQ 1   AND
        Pro_Ahorros.Cod_ahorro EQ 216 NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Ahorros THEN DO:
      IF Pro_Ahorros.Id_Consecutivo  THEN 
         DO TRANSACTION:   
             FIND CURRENT Pro_Ahorros NO-ERROR.
             CREATE Ahorros.
             ASSIGN Pro_Ahorros.Num_Consecutivo = Pro_Ahorros.Num_Consecutivo + 1
                    Ahorros.Cue_Ahorros     = STRING(Pro_Ahorros.Num_Consecutivo)
                    Ahorros.Detalle_Estado  = 1
                    Ahorros.Agencia         = W_Agencia
                    Ahorros.Tip_Ahorro      = 1
                    Ahorros.Cod_ahorro      = 216
                    Ahorros.For_Pago        = 1 /* Caja*/
                    Ahorros.Nit             = Clientes.Nit
                    Ahorros.Per_Liquidacion = 2 /* Mensual*/
                    Ahorros.For_Liquidacion = Pro_Ahorros.For_Liquidacion
                    Ahorros.Estado          = 1
                    Ahorros.Tasa            = 0
                    Ahorros.Usu_Creacion    = W_Usuario
                    Ahorros.IdNombre        = Clientes.Nombre
                    Ahorros.IdApellido1     = Clientes.Apellido1
                    Ahorros.Cuota           = 0
                    Ahorros.Plazo           = 9999
                    Ahorros.Per_Deduccion   = 4 /* Mensual*/
                    Ahorros.Monto_Apertura  = 10000.
         END.   /*fin TRANS.*/
      ELSE RETURN ERROR.            
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica_RepLegal wWin 
PROCEDURE Verifica_RepLegal :
IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) GT 2 THEN DO:
     FIND FIRST Relaciones WHERE Relaciones.Cod_Relacion EQ 5            AND 
                                 Relaciones.Nit          EQ Clientes.Nit AND
                                 Relaciones.Estado       EQ 1            NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Relaciones) THEN DO:
        MESSAGE "La EMPRESA debe tener un REPRESENTANTE LEGAL" SKIP(1)
                "Utilice la ventana de Relaciones para establecer este vinvulo" SKIP
                "con la persona que cumple con esta condicion!" VIEW-AS ALERT-BOX WARNING.
        FOR EACH T_Relaciones: DELETE T_Relaciones. END.
        OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
        HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Economica FRAME F_Fechas FRAME F_Otros.
        RSeleccion:SCREEN-VALUE IN FRAME F_Clientes = "4".
        VIEW FRAME F_Relaciones.
        APPLY 'choose' TO Btn_CreRel IN FRAME F_Relaciones.
        Cmb_Relaciones:SCREEN-VALUE IN FRAME F_Relaciones = "00005 - Representante Legal".
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica_Tutor wWin 
PROCEDURE Verifica_Tutor :
IF Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes EQ "2" THEN DO:
     FIND FIRST Relaciones WHERE Relaciones.Cod_Relacion EQ 3            AND 
                                 Relaciones.Nit          EQ Clientes.Nit AND
                                 Relaciones.Estado       EQ 1            NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Relaciones) THEN DO:
        MESSAGE "El MENOR debe tener un TUTOR!" SKIP(1)
                "Utilice la ventana de Relaciones para establecer este vinvulo" SKIP
                "con la persona que cumple con esta condicion!" VIEW-AS ALERT-BOX WARNING.
        FOR EACH T_Relaciones: DELETE T_Relaciones. END.
        OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
        HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Economica FRAME F_Fechas FRAME F_Otros.
        VIEW FRAME F_Relaciones.
        APPLY 'choose' TO Btn_CreRel IN FRAME F_Relaciones.
        ASSIGN  RSeleccion:SCREEN-VALUE IN FRAME F_Clientes = "4"
                Cmb_Relaciones:SCREEN-VALUE IN FRAME F_Relaciones = "00003 - Tutor".
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

