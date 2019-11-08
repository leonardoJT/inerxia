&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* oakley */

/* ***************************  Definitions  ************************** */
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_Rw AS ROWID.
DEFINE VAR W_Cr AS LOGICAL.
/*Temporales para Creacion de Instancias*/
DEFINE VAR T_Nom_Instancia  LIKE Instancias.Nom_Instancia.
DEFINE VAR T_Tipo_Producto  LIKE Instancias.Tipo_Producto.
/*DEFINE VAR T_Cod_Producto   LIKE Instancias.Cod_Producto.*/
DEFINE VAR T_Estado         LIKE Instancias.Estado.
DEFINE VAR T_Instancia      LIKE Instancias.Instancia.
DEFINE VAR T_Orden          LIKE Instancias.Orden.
DEFINE VAR T_Tipo_Instancia LIKE Instancias.Tipo_Instancia.
DEFINE VAR T_Fec_Creacion   LIKE Instancias.Fec_Creacion.
DEFINE VAR T_Fec_Retiro     LIKE Instancias.Fec_Retiro.
DEFINE VAR T_CTipos         AS CHARACTER FORMAT "X(25)".
DEFINE VAR T_CProductos     AS CHARACTER FORMAT "X(25)".

/*Temporales para Asignacion de Instancias*/
DEFINE VAR T_ATipo_Instancia LIKE CFG_Instancias.Tipo_Instancia.
DEFINE VAR T_AAgencia        LIKE Cfg_Instancias.Agencia.
DEFINE VAR T_AOrden          LIKE CFG_Instancias.Orden.
DEFINE VAR T_AInstancia      LIKE CFG_Instancias.Instancia.
DEFINE VAR T_AMonMax         LIKE Cfg_Instancias.Monto_Maximo.
DEFINE VAR T_AMonMin         LIKE Cfg_Instancias.Monto_Minimo.
DEFINE VAR T_APlaMin         LIKE Cfg_Instancias.Plazo_Maximo.
DEFINE VAR T_APlaMax         LIKE Cfg_Instancias.Plazo_Minimo.
DEFINE VAR T_AUsuario        LIKE Cfg_Instancias.Usuario.
DEFINE VAR T_AEstado         LIKE Cfg_Instancias.Estado.
DEFINE VAR T_AFecCrea        LIKE Cfg_Instancias.Fec_Creacion.
DEFINE VAR T_AFecRet         LIKE Cfg_Instancias.Fec_Retiro.

DEFINE VAR i AS INTEGER.
DEFINE VAR j AS INTEGER.

DEFINE VAR W_If AS INTEGER.

DEFINE SHARED VAR W_Nom_Agencia    AS   CHARACTER FORMAT "X(60)".
DEFINE SHARED VAR W_Nom_Entidad    AS   CHARACTER FORMAT "X(60)".
DEFINE SHARED VAR W_Usuario        LIKE Usuarios.Usuario.
DEFINE SHARED VAR W_Estacion       LIKE Estaciones.Estacion.
DEFINE SHARED VAR W_Manija         AS   HANDLE.
DEFINE SHARED VAR W_PathSpl        LIKE Entidad.Dir_Spl.
DEFINE VAR W_Rpta                  AS LOGICAL.
DEFINE VAR W_TamOfi                AS INTEGER INITIAL 0.
DEFINE VAR W_OfiTra                LIKE Agencias.Agencia INITIAL 0.

DEFINE VAR W_Asigna1 AS LOGICAL INITIAL YES.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Br_Asignacion

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cfg_Instancias Instancias

/* Definitions for BROWSE Br_Asignacion                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Asignacion Cfg_Instancias.Agencia Cfg_Instancias.Tipo_Instancia Cfg_Instancias.Orden Cfg_Instancias.Instancia Cfg_Instancias.Usuario Cfg_Instancias.Monto_Maximo Cfg_Instancias.Monto_Minimo Cfg_Instancias.Plazo_Maximo Cfg_Instancias.Plazo_Minimo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Asignacion   
&Scoped-define SELF-NAME Br_Asignacion
&Scoped-define QUERY-STRING-Br_Asignacion FOR EACH Cfg_Instancias      SHARE-LOCK BY Cfg_Instancias.Tipo_Instancia                 BY Cfg_Instancias.Orden                 BY Cfg_Instancias.Usuario                 BY Cfg_Instancias.Agencia INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Asignacion OPEN QUERY {&SELF-NAME} FOR EACH Cfg_Instancias      SHARE-LOCK BY Cfg_Instancias.Tipo_Instancia                 BY Cfg_Instancias.Orden                 BY Cfg_Instancias.Usuario                 BY Cfg_Instancias.Agencia INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Asignacion Cfg_Instancias
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Asignacion Cfg_Instancias


/* Definitions for BROWSE Br_Creacion                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Creacion Instancias.Tipo_Instancia Instancias.Instancia Instancias.Orden_Instancia Instancias.Nom_Instancia   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Creacion   
&Scoped-define SELF-NAME Br_Creacion
&Scoped-define QUERY-STRING-Br_Creacion FOR EACH Instancias SHARE-LOCK     BY Instancias.Tipo_Instancia BY Instancias.Orden INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Creacion OPEN QUERY {&SELF-NAME} FOR EACH Instancias SHARE-LOCK     BY Instancias.Tipo_Instancia BY Instancias.Orden INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Creacion Instancias
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Creacion Instancias


/* Definitions for FRAME F_Asignacion                                   */
&Scoped-define FIELDS-IN-QUERY-F_Asignacion Cfg_Instancias.Agencia ~
Cfg_Instancias.Estado Cfg_Instancias.Fec_Creacion ~
Cfg_Instancias.Tipo_Instancia Cfg_Instancias.Fec_Retiro ~
Cfg_Instancias.Orden Cfg_Instancias.Instancia Cfg_Instancias.Usuario ~
Cfg_Instancias.Plazo_Minimo Cfg_Instancias.Plazo_Maximo ~
Cfg_Instancias.Monto_Minimo Cfg_Instancias.Monto_Maximo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Asignacion Cfg_Instancias.Estado ~
Cfg_Instancias.Usuario Cfg_Instancias.Plazo_Minimo ~
Cfg_Instancias.Plazo_Maximo Cfg_Instancias.Monto_Minimo ~
Cfg_Instancias.Monto_Maximo 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Asignacion Cfg_Instancias
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Asignacion Cfg_Instancias
&Scoped-define QUERY-STRING-F_Asignacion FOR EACH Cfg_Instancias SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Asignacion OPEN QUERY F_Asignacion FOR EACH Cfg_Instancias SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Asignacion Cfg_Instancias
&Scoped-define FIRST-TABLE-IN-QUERY-F_Asignacion Cfg_Instancias


/* Definitions for FRAME F_ConsAsignaciones                             */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_ConsAsignaciones ~
    ~{&OPEN-QUERY-Br_Asignacion}

/* Definitions for FRAME F_ConsInstancias                               */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_ConsInstancias ~
    ~{&OPEN-QUERY-Br_Creacion}

/* Definitions for FRAME F_Creacion                                     */
&Scoped-define FIELDS-IN-QUERY-F_Creacion Instancias.Tipo_Instancia ~
Instancias.Nom_Instancia Instancias.Instancia Instancias.Fec_Creacion ~
Instancias.Estado Instancias.Id_Abogado Instancias.Id_Concepto ~
Instancias.Fec_Retiro Instancias.Orden_Instancia Instancias.Primera ~
Instancias.Id_Negadas Instancias.TMI Instancias.Ultima ~
Instancias.Id_Scoring Instancias.Honorarios Instancias.Tipo_Producto 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Creacion Instancias.Nom_Instancia ~
Instancias.Estado Instancias.Id_Abogado Instancias.Id_Concepto ~
Instancias.Orden_Instancia Instancias.Primera Instancias.Id_Negadas ~
Instancias.TMI Instancias.Ultima Instancias.Id_Scoring ~
Instancias.Honorarios Instancias.Tipo_Producto 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Creacion Instancias
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Creacion Instancias
&Scoped-define QUERY-STRING-F_Creacion FOR EACH Instancias SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Creacion OPEN QUERY F_Creacion FOR EACH Instancias SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Creacion Instancias
&Scoped-define FIRST-TABLE-IN-QUERY-F_Creacion Instancias


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BUTTON-49 R_Opcion 
&Scoped-Define DISPLAYED-OBJECTS R_Opcion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-49 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 49" 
     SIZE 13 BY 1.54.

DEFINE VARIABLE R_Opcion AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Creación de Pasos para una Instancia", 1,
"Asignación de Instancias a los Usuarios", 2
     SIZE 84 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 94 BY 1.88
     BGCOLOR 18 .

DEFINE BUTTON BtnDone-2 DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BT_Asignar 
     LABEL "Escoger Instancia a Asignar" 
     SIZE 26 BY 1.12.

DEFINE BUTTON BUTTON-50 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 50" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-51 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 51" 
     SIZE 13 BY 1.62.

DEFINE BUTTON B_CRBorrar-2 
     LABEL "Inactivar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON B_CRCancelar-2 
     LABEL "Cancelar" 
     SIZE 13 BY 1.65.

DEFINE BUTTON B_CRIngresar-2 
     LABEL "Ingresar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON B_CrSalvar-2 
     LABEL "Salvar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON C_CRDeshacer-2 
     LABEL "Deshacer" 
     SIZE 13 BY 1.62.

DEFINE VARIABLE ACmb_Agencia AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todas las Agencias" 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomInstancia AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY .92
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomUsuario AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY .92
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Info_CFG AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 91 BY 8.08
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_OutConAsi 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 53" 
     SIZE 9 BY 1.65.

DEFINE VARIABLE W_AgeAsi AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Ver  Agencia" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_UsuAsi AS CHARACTER FORMAT "X(18)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VerAsi AS DECIMAL FORMAT ">>>>9":U INITIAL 0 
     LABEL "Ver solo las de Tipo" 
     VIEW-AS FILL-IN 
     SIZE 9.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_OutConIns 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 10" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE W_VerCon AS DECIMAL FORMAT ">>>>9":U INITIAL 0 
     LABEL "Ver solo las de Tipo" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 15  NO-UNDO.

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
     LABEL "Button 1" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-52 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 52" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-59 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 59" 
     SIZE 3 BY .54.

DEFINE BUTTON B_CRBorrar 
     LABEL "Inactivar" 
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

DEFINE VARIABLE CCmb_Tipos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Instancias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 61 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Operacion AS CHARACTER FORMAT "X(80)":U 
     LABEL "Operación" 
     VIEW-AS FILL-IN 
     SIZE 61 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Programa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Programa Adjunto" 
     VIEW-AS FILL-IN 
     SIZE 61 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-300
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19.43 BY 1.

DEFINE VARIABLE Info_Instancia AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 91 BY 7.54
     BGCOLOR 15 FONT 2 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Asignacion FOR 
      Cfg_Instancias SCROLLING.

DEFINE QUERY Br_Creacion FOR 
      Instancias SCROLLING.

DEFINE QUERY F_Asignacion FOR 
      Cfg_Instancias SCROLLING.

DEFINE QUERY F_Creacion FOR 
      Instancias SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Asignacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Asignacion wWin _FREEFORM
  QUERY Br_Asignacion SHARE-LOCK NO-WAIT DISPLAY
      Cfg_Instancias.Agencia COLUMN-LABEL "Age" FORMAT "999":U
            WIDTH 5.43
      Cfg_Instancias.Tipo_Instancia FORMAT "99999":U
      Cfg_Instancias.Orden COLUMN-LABEL "Paso" FORMAT "999":U
      Cfg_Instancias.Instancia COLUMN-LABEL "Instancia" FORMAT "99999":U
      Cfg_Instancias.Usuario COLUMN-LABEL "Usua" FORMAT "X(18)":U
            WIDTH 18
      Cfg_Instancias.Monto_Maximo COLUMN-LABEL "Monto Máx" FORMAT "->>>,>>>,>>>,>>9":U
            WIDTH 14
      Cfg_Instancias.Monto_Minimo COLUMN-LABEL "Monto Mín" FORMAT "->>>,>>>,>>>,>>9":U
            WIDTH 12.86
      Cfg_Instancias.Plazo_Maximo COLUMN-LABEL "PlaMax.Días" FORMAT "99999":U
            WIDTH 11.43
      Cfg_Instancias.Plazo_Minimo COLUMN-LABEL "PlaMin.Días" FORMAT "99999":U
            WIDTH 12.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 5.65
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Creacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Creacion wWin _FREEFORM
  QUERY Br_Creacion SHARE-LOCK NO-WAIT DISPLAY
      Instancias.Tipo_Instancia FORMAT "99999":U
      Instancias.Instancia FORMAT "99999":U LABEL "Paso"
      Instancias.Orden_Instancia FORMAT "999":U 
      Instancias.Nom_Instancia FORMAT "X(30)":U WIDTH 63.43
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 5.38
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-49 AT ROW 1.5 COL 98
     R_Opcion AT ROW 1.81 COL 9 NO-LABEL
     RECT-1 AT ROW 1.27 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 21.42
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Creacion
     CCmb_Tipos AT ROW 1.54 COL 18 COLON-ALIGNED
     Instancias.Tipo_Instancia AT ROW 1.54 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .92
          BGCOLOR 17 FGCOLOR 17 
     BUTTON-1 AT ROW 1.81 COL 97
     Instancias.Nom_Instancia AT ROW 2.62 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 61 BY .92
          BGCOLOR 15 
     BUTTON-2 AT ROW 3.42 COL 97
     Instancias.Instancia AT ROW 3.69 COL 18 COLON-ALIGNED
          LABEL "Paso"
          VIEW-AS FILL-IN 
          SIZE 7 BY .92
          BGCOLOR 18 FGCOLOR 15 
     Instancias.Fec_Creacion AT ROW 3.69 COL 79 COLON-ALIGNED
          LABEL "Fec-Apertura"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Instancias.Estado AT ROW 3.88 COL 28 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 18 BY .58
     Instancias.Id_Abogado AT ROW 3.96 COL 49 HELP
          "Id_Abogado"
          LABEL "Id_Abogado"
          VIEW-AS TOGGLE-BOX
          SIZE 14.57 BY .77 TOOLTIP "Marque solo si es Instancia de Abogado"
     Instancias.Id_Concepto AT ROW 4.5 COL 49 HELP
          "Id_Concepto"
          LABEL "Ver Concepto"
          VIEW-AS TOGGLE-BOX
          SIZE 15.72 BY .85
     Instancias.Fec_Retiro AT ROW 4.5 COL 79 COLON-ALIGNED
          LABEL "Fecha Retiro"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Instancias.Orden_Instancia AT ROW 4.77 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY .92
          BGCOLOR 15 FGCOLOR 0 
     Instancias.Primera AT ROW 4.77 COL 28
          LABEL "Primera Instancia"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .77
     Instancias.Id_Negadas AT ROW 5.19 COL 49
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .77
     Instancias.TMI AT ROW 5.31 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 15 
     Instancias.Ultima AT ROW 5.58 COL 28
          LABEL "Ultima Instancia"
          VIEW-AS TOGGLE-BOX
          SIZE 17.72 BY .88
     Instancias.Id_Scoring AT ROW 5.85 COL 49 HELP
          "Id_Scoring"
          LABEL "Ver Scoring"
          VIEW-AS TOGGLE-BOX
          SIZE 14.57 BY .77
     Instancias.Honorarios AT ROW 6.31 COL 79.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     Instancias.Tipo_Producto AT ROW 6.77 COL 20 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ahorro", 1,
"Credito", 2,
"Especial", 3
          SIZE 33 BY .88
     W_Programa AT ROW 7.65 COL 18 COLON-ALIGNED
     BUTTON-52 AT ROW 7.65 COL 81
     B_CrSalvar AT ROW 8 COL 97
     W_Operacion AT ROW 8.46 COL 18 COLON-ALIGNED
     BUTTON-59 AT ROW 8.46 COL 81
     C_CRDeshacer AT ROW 9.62 COL 97
     Info_Instancia AT ROW 10.42 COL 3 NO-LABEL
     B_CRIngresar AT ROW 11.23 COL 97
     B_CRCancelar AT ROW 12.85 COL 97
     B_CRBorrar AT ROW 14.46 COL 97
     BtnDone AT ROW 16.08 COL 97
     " Pasos pertenecientes a la Instancia Actual" VIEW-AS TEXT
          SIZE 91 BY .81 AT ROW 9.62 COL 3
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.69
         SIZE 111 BY 18.04
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON BtnDone.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Creacion
     RECT-300 AT ROW 3.65 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.69
         SIZE 111 BY 18.04
         BGCOLOR 17 FONT 5
         TITLE "Creación"
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F_ConsInstancias
     Br_Creacion AT ROW 1.27 COL 2
     W_VerCon AT ROW 6.92 COL 19 COLON-ALIGNED
     Btn_OutConIns AT ROW 7.46 COL 82
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 12.31
         SIZE 91 BY 9.15
         BGCOLOR 17 FONT 5
         TITLE "Consulta de Instancias".

DEFINE FRAME F_Asignacion
     ACmb_Agencia AT ROW 6.42 COL 10 COLON-ALIGNED
     Cfg_Instancias.Agencia AT ROW 6.42 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.43 BY .92
          BGCOLOR 17 FGCOLOR 17 
     Cfg_Instancias.Estado AT ROW 1.54 COL 61 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 22 BY .81
     BUTTON-50 AT ROW 1.81 COL 97
     Cfg_Instancias.Fec_Creacion AT ROW 2.54 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .92
          BGCOLOR 18 FGCOLOR 15 
     Cfg_Instancias.Tipo_Instancia AT ROW 2.08 COL 10 COLON-ALIGNED
          LABEL "Instancia"
          VIEW-AS FILL-IN 
          SIZE 14 BY .92
          BGCOLOR 18 FGCOLOR 15 
     BT_Asignar AT ROW 2.65 COL 27
     BUTTON-51 AT ROW 3.42 COL 97
     Cfg_Instancias.Fec_Retiro AT ROW 3.58 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .92
          BGCOLOR 18 FGCOLOR 15 
     Cfg_Instancias.Orden AT ROW 3.12 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .92
          BGCOLOR 18 FGCOLOR 15 
     Cfg_Instancias.Instancia AT ROW 4.19 COL 10 COLON-ALIGNED
          LABEL "Paso"
          VIEW-AS FILL-IN 
          SIZE 14 BY .92
          BGCOLOR 18 FGCOLOR 15 
     W_NomInstancia AT ROW 4.19 COL 25 COLON-ALIGNED NO-LABEL
     Cfg_Instancias.Usuario AT ROW 5.23 COL 10 COLON-ALIGNED FORMAT "X(18)"
          VIEW-AS FILL-IN 
          SIZE 14 BY .92
          BGCOLOR 15 
     Cfg_Instancias.Plazo_Minimo AT ROW 4.58 COL 73 COLON-ALIGNED
          LABEL "Mínimo en días"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .92
          BGCOLOR 15 
     Cfg_Instancias.Plazo_Maximo AT ROW 5.58 COL 73 COLON-ALIGNED
          LABEL "Máximo en días"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .92
          BGCOLOR 15 
     Cfg_Instancias.Monto_Minimo AT ROW 6.54 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.43 BY .92
          BGCOLOR 15 
     Cfg_Instancias.Monto_Maximo AT ROW 7.54 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.43 BY .92
          BGCOLOR 15 
     W_NomUsuario AT ROW 5.23 COL 25 COLON-ALIGNED NO-LABEL
     B_CrSalvar-2 AT ROW 8 COL 97
     Info_CFG AT ROW 9.62 COL 3 NO-LABEL
     C_CRDeshacer-2 AT ROW 9.62 COL 97
     B_CRIngresar-2 AT ROW 11.23 COL 97
     B_CRCancelar-2 AT ROW 12.85 COL 97
     B_CRBorrar-2 AT ROW 14.46 COL 97
     BtnDone-2 AT ROW 16.08 COL 97
     "  Instancias Asignadas al Usuario Actual" VIEW-AS TEXT
          SIZE 91 BY .81 AT ROW 8.81 COL 3
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.69
         SIZE 111 BY 18.04
         BGCOLOR 17 FONT 5
         TITLE "Asignación".

DEFINE FRAME F_ConsAsignaciones
     Br_Asignacion AT ROW 1.27 COL 2
     W_VerAsi AT ROW 7.19 COL 18.14 COLON-ALIGNED
     W_UsuAsi AT ROW 7.19 COL 41 COLON-ALIGNED
     W_AgeAsi AT ROW 7.19 COL 73.43 COLON-ALIGNED WIDGET-ID 2
     Btn_OutConAsi AT ROW 7.19 COL 81.57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 12.31
         SIZE 91 BY 8.88
         BGCOLOR 17 FONT 5
         TITLE "Consulta de Asignaciones".


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
         TITLE              = "Configuración de Instancias"
         HEIGHT             = 21.42
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
ASSIGN FRAME F_Asignacion:FRAME = FRAME F-Main:HANDLE
       FRAME F_ConsAsignaciones:FRAME = FRAME F-Main:HANDLE
       FRAME F_ConsInstancias:FRAME = FRAME F-Main:HANDLE
       FRAME F_Creacion:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME F_Asignacion
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Asignacion:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX ACmb_Agencia IN FRAME F_Asignacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_Instancias.Agencia IN FRAME F_Asignacion
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BT_Asignar IN FRAME F_Asignacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_Instancias.Fec_Creacion IN FRAME F_Asignacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_Instancias.Fec_Retiro IN FRAME F_Asignacion
   NO-ENABLE                                                            */
ASSIGN 
       Info_CFG:AUTO-RESIZE IN FRAME F_Asignacion      = TRUE.

/* SETTINGS FOR FILL-IN Cfg_Instancias.Instancia IN FRAME F_Asignacion
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Cfg_Instancias.Orden IN FRAME F_Asignacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_Instancias.Plazo_Maximo IN FRAME F_Asignacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Cfg_Instancias.Plazo_Minimo IN FRAME F_Asignacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Cfg_Instancias.Tipo_Instancia IN FRAME F_Asignacion
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Cfg_Instancias.Usuario IN FRAME F_Asignacion
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN W_NomInstancia IN FRAME F_Asignacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomUsuario IN FRAME F_Asignacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_ConsAsignaciones
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Asignacion 1 F_ConsAsignaciones */
ASSIGN 
       FRAME F_ConsAsignaciones:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_ConsInstancias
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Creacion 1 F_ConsInstancias */
ASSIGN 
       FRAME F_ConsInstancias:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Creacion
                                                                        */
/* SETTINGS FOR COMBO-BOX CCmb_Tipos IN FRAME F_Creacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Instancias.Fec_Creacion IN FRAME F_Creacion
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Instancias.Fec_Retiro IN FRAME F_Creacion
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Instancias.Id_Abogado IN FRAME F_Creacion
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX Instancias.Id_Concepto IN FRAME F_Creacion
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX Instancias.Id_Scoring IN FRAME F_Creacion
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Instancias.Instancia IN FRAME F_Creacion
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Instancias.Primera IN FRAME F_Creacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Instancias.Tipo_Instancia IN FRAME F_Creacion
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Instancias.Ultima IN FRAME F_Creacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_Operacion IN FRAME F_Creacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Programa IN FRAME F_Creacion
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Asignacion
/* Query rebuild information for BROWSE Br_Asignacion
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cfg_Instancias
     SHARE-LOCK BY Cfg_Instancias.Tipo_Instancia
                BY Cfg_Instancias.Orden
                BY Cfg_Instancias.Usuario
                BY Cfg_Instancias.Agencia INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Asignacion */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Creacion
/* Query rebuild information for BROWSE Br_Creacion
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Instancias SHARE-LOCK
    BY Instancias.Tipo_Instancia BY Instancias.Orden INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Creacion */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Asignacion
/* Query rebuild information for FRAME F_Asignacion
     _TblList          = "bdCentral.Cfg_Instancias"
     _Query            is OPENED
*/  /* FRAME F_Asignacion */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Creacion
/* Query rebuild information for FRAME F_Creacion
     _TblList          = "bdCentral.Instancias"
     _Query            is OPENED
*/  /* FRAME F_Creacion */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Configuración de Instancias */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Configuración de Instancias */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Asignacion
&Scoped-define FRAME-NAME F_ConsAsignaciones
&Scoped-define SELF-NAME Br_Asignacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Asignacion wWin
ON MOUSE-SELECT-DBLCLICK OF Br_Asignacion IN FRAME F_ConsAsignaciones
DO:
  RUN Mostrar_Campos_Asignacion.
  RUN Save_Temp_Asignacion.  
  RUN Cambio_Asig_Creacion.
  APPLY "choose" TO Btn_OutConAsi IN FRAME F_ConsAsignaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Creacion
&Scoped-define FRAME-NAME F_ConsInstancias
&Scoped-define SELF-NAME Br_Creacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Creacion wWin
ON MOUSE-SELECT-DBLCLICK OF Br_Creacion IN FRAME F_ConsInstancias
DO:
  RUN Mostrar_Campos_Creacion.
  RUN Save_Temp.  
  RUN Cambio_Inst_Creacion.
  APPLY "choose" TO Btn_OutConIns IN FRAME F_ConsInstancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creacion
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F_Creacion /* Salir */
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


&Scoped-define FRAME-NAME F_Asignacion
&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 wWin
ON CHOOSE OF BtnDone-2 IN FRAME F_Asignacion /* Salir */
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


&Scoped-define FRAME-NAME F_ConsAsignaciones
&Scoped-define SELF-NAME Btn_OutConAsi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConAsi wWin
ON CHOOSE OF Btn_OutConAsi IN FRAME F_ConsAsignaciones /* Button 53 */
DO:
  HIDE FRAME F_ConsAsignaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConsInstancias
&Scoped-define SELF-NAME Btn_OutConIns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConIns wWin
ON CHOOSE OF Btn_OutConIns IN FRAME F_ConsInstancias /* Button 10 */
DO:
  DO WITH FRAME F_Asignacion:
      ASSIGN CFG_Instancias.Tipo_Instancia:SCREEN-VALUE = Instancias.Tipo_Instancia:SCREEN-VALUE IN BROWSE BR_Creacion
             CFG_Instancias.Orden:SCREEN-VALUE = Instancias.Orden:SCREEN-VALUE IN BROWSE Br_Creacion
             CFG_Instancias.Instancia:SCREEN-VALUE = Instancias.Instancia:SCREEN-VALUE IN BROWSE Br_Creacion
             W_NomInstancia:SCREEN-VALUE = Instancias.Nom_Instancia:SCREEN-VALUE IN BROWSE Br_Creacion.
  END.
  HIDE FRAME F_ConsInstancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asignacion
&Scoped-define SELF-NAME BT_Asignar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT_Asignar wWin
ON CHOOSE OF BT_Asignar IN FRAME F_Asignacion /* Escoger Instancia a Asignar */
DO:
  VIEW FRAME F_ConsInstancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creacion
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Creacion /* Button 1 */
DO:
  W_If = 1.
    DEFINE VAR Listado AS CHARACTER INITIAL "".    
  listado = W_PathSpl + "Instancias.Lst".
  {Incluido\Imprimir.I "listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Creacion /* Button 1 */
DO:
    OPEN QUERY Br_Creacion FOR EACH Instancias 
     NO-LOCK BY Instancias.Tipo_Instancia BY Instancias.Orden INDEXED-REPOSITION.
  VIEW FRAME F_ConsInstancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-49
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-49 wWin
ON CHOOSE OF BUTTON-49 IN FRAME F-Main /* Button 49 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asignacion
&Scoped-define SELF-NAME BUTTON-50
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-50 wWin
ON CHOOSE OF BUTTON-50 IN FRAME F_Asignacion /* Button 50 */
DO:
  W_If = 2.
  DEFINE VAR Listado AS CHARACTER INITIAL "".    
  listado = W_PathSpl + "Instancias.Lst".
  {Incluido\Imprimir.I "listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-51
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-51 wWin
ON CHOOSE OF BUTTON-51 IN FRAME F_Asignacion /* Button 51 */
DO:
  OPEN QUERY Br_Asignacion FOR EACH CFG_Instancias 
     SHARE-LOCK BY Cfg_Instancias.Tipo_Instancia BY CFG_Instancias.Orden INDEXED-REPOSITION.
  VIEW FRAME F_ConsAsignaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creacion
&Scoped-define SELF-NAME BUTTON-52
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-52 wWin
ON CHOOSE OF BUTTON-52 IN FRAME F_Creacion /* Button 52 */
DO:
  DEFINE VAR P_Cod LIKE Programas.Programa.
  DEFINE VAR P_Nom LIKE Programas.Descripcion.
  RUN C-Programas.r (OUTPUT P_Cod, OUTPUT P_Nom).
  W_Programa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(P_Cod,"9999999999") + " - " + P_Nom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-59
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-59 wWin
ON CHOOSE OF BUTTON-59 IN FRAME F_Creacion /* Button 59 */
DO:
  DEFINE VAR P_Cod LIKE Operacion.Cod_Operacion.
  DEFINE VAR P_Nom LIKE Operacion.Nom_Operacion.
  RUN C-Operaciones.r (OUTPUT P_Cod, OUTPUT P_Nom).
  IF P_Cod EQ 0 THEN W_Operacion:SCREEN-VALUE = "".
  ELSE W_Operacion:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(P_Cod,"999999999") + " - " + P_Nom.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_CRBorrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRBorrar wWin
ON CHOOSE OF B_CRBorrar IN FRAME F_Creacion /* Inactivar */
DO:
    DO WITH FRAME F_Creacion:
        MESSAGE Instancias.Tipo_instancia INTEGER(SUBSTRING(CCmb_Tipos:SCREEN-VALUE,1,5))
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        MESSAGE Instancias.Instancia INTEGER(Instancias.Instancia:SCREEN-VALUE)
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        FIND FIRST Instancias WHERE Instancias.Tipo_instancia EQ INTEGER(SUBSTRING(CCmb_Tipos:SCREEN-VALUE,1,5))
                                AND Instancias.Instancia EQ INTEGER(Instancias.Instancia:SCREEN-VALUE) NO-ERROR.
        IF AVAILABLE(Instancias) THEN DO:
            IF SELF:LABEL EQ "Inactivar" THEN DO:
                ASSIGN Instancias.Fec_Retiro:SCREEN-VALUE = STRING(TODAY)
                       Instancias.Estado:SCREEN-VALUE = "2".

                SELF:LABEL = "Activar".
                DISABLE Instancias.Estado.
                RUN Salvar_Creacion.
            END.
            ELSE DO:
                IF SELF:LABEL EQ "Activar" THEN DO:
                    ASSIGN Instancias.Fec_Retiro:SCREEN-VALUE = ""
                           Instancias.Estado:SCREEN-VALUE = "1".

                    SELF:LABEL = "Inactivar".
                    ENABLE Instancias.Estado.
                    RUN Salvar_Creacion.
                END.
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asignacion
&Scoped-define SELF-NAME B_CRBorrar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRBorrar-2 wWin
ON CHOOSE OF B_CRBorrar-2 IN FRAME F_Asignacion /* Inactivar */
DO:
 DO WITH FRAME F_Asignacion:

   FIND CFG_Instancias WHERE 
        CFG_Instancias.Tipo_instancia EQ INTEGER(CFG_Instancias.Tipo_Instancia:SCREEN-VALUE) AND
        CFG_Instancias.Instancia EQ INTEGER(CFG_Instancias.Instancia:SCREEN-VALUE) NO-ERROR.
   IF AVAILABLE(CFG_Instancias) THEN
   DO:
      IF SELF:LABEL EQ "Inactivar" THEN DO:
        ASSIGN CFG_Instancias.Fec_Retiro:SCREEN-VALUE = STRING(TODAY)
               CFG_Instancias.Estado:SCREEN-VALUE = "2".
        APPLY 'choose' TO B_CrSalvar-2.
        SELF:LABEL = "Activar".
        DISABLE CFG_Instancias.Estado.
      END.
      ELSE DO:
        IF SELF:LABEL EQ "Activar" THEN DO:
          ASSIGN CFG_Instancias.Fec_Retiro:SCREEN-VALUE = ""
                 CFG_Instancias.Estado:SCREEN-VALUE = "1".
          APPLY 'choose' TO B_CrSalvar-2.
          SELF:LABEL = "Inactivar".
          ENABLE CFG_Instancias.Estado.
        END.
      END.
   END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creacion
&Scoped-define SELF-NAME B_CRCancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRCancelar wWin
ON CHOOSE OF B_CRCancelar IN FRAME F_Creacion /* Cancelar */
DO:
  DISABLE CCmb_Tipos WITH FRAME F_Creacion.
  RUN Deshacer_Cambios_Creacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asignacion
&Scoped-define SELF-NAME B_CRCancelar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRCancelar-2 wWin
ON CHOOSE OF B_CRCancelar-2 IN FRAME F_Asignacion /* Cancelar */
DO:
  DISABLE ACmb_Agencia Bt_Asignar WITH FRAME F_Asignacion.
  RUN Deshacer_Cambios_Asignacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creacion
&Scoped-define SELF-NAME B_CRIngresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRIngresar wWin
ON CHOOSE OF B_CRIngresar IN FRAME F_Creacion /* Ingresar */
DO:
DO WITH FRAME F_Creacion:
  RUN Save_Temp.
  W_cr = YES.
  ENABLE CCmb_Tipos WITH FRAME F_Creacion.
  ASSIGN Instancias.Nom_Instancia:SCREEN-VALUE   = ""
         Instancias.Estado:SCREEN-VALUE          = "1"
         Instancias.Tipo_Producto:SCREEN-VALUE   = "1"
         Instancias.Tipo_Instancia:SCREEN-VALUE  = SUBSTRING(Ccmb_Tipos:SCREEN-VALUE,1,5)
         Instancias.Instancia:SCREEN-VALUE       = STRING(CURRENT-VALUE(Sec_Instancias) + 10)
         Instancias.Fec_Retiro:SCREEN-VALUE      = ""
         Instancias.Fec_Creacion:SCREEN-VALUE    = STRING(TODAY)
         Instancias.TMI:SCREEN-VALUE             = "0"
         Instancias.Id_Scoring:SCREEN-VALUE      = "No" 
         Instancias.Id_Concepto:SCREEN-VALUE     = "No"
         Instancias.Id_Abogado:SCREEN-VALUE      = "No".
         
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asignacion
&Scoped-define SELF-NAME B_CRIngresar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRIngresar-2 wWin
ON CHOOSE OF B_CRIngresar-2 IN FRAME F_Asignacion /* Ingresar */
DO:
DO WITH FRAME F_Asignacion:
  RUN Save_Temp_Asignacion.
  W_cr = YES.
  ENABLE ACmb_Agencia BT_Asignar WITH FRAME F_Asignacion.
  ASSIGN CFG_Instancias.Tipo_Instancia:SCREEN-VALUE   = ""
         CFG_Instancias.Orden:SCREEN-VALUE            = "000"
         CFG_Instancias.Instancia:SCREEN-VALUE        = "00000"
         Cfg_Instancias.Monto_Maximo:SCREEN-VALUE     = "0"
         Cfg_Instancias.Monto_Minimo:SCREEN-VALUE     = "0"
         Cfg_Instancias.Plazo_Maximo:SCREEN-VALUE     = "0"
         Cfg_Instancias.Plazo_Minimo:SCREEN-VALUE     = "0"
         Cfg_Instancias.Usuario:SCREEN-VALUE          = ""
         Cfg_Instancias.Estado:SCREEN-VALUE           = "1" 
         Cfg_Instancias.Fec_Creacion:SCREEN-VALUE     = STRING(TODAY)
         Cfg_Instancias.Fec_Retiro:SCREEN-VALUE       = "".
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creacion
&Scoped-define SELF-NAME B_CrSalvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CrSalvar wWin
ON CHOOSE OF B_CrSalvar IN FRAME F_Creacion /* Salvar */
DO: 
DO WITH FRAME F_Creacion:
  ASSIGN CCmb_Tipos.
  IF W_Cr THEN
  DO:
    CREATE Instancias.
    W_Cr = NO.
  END.
  ELSE
    FIND Instancias WHERE 
         Instancias.Tipo_instancia EQ INTEGER(SUBSTRING(CCmb_Tipos,1,5)) AND
         Instancias.Instancia EQ INTEGER(Instancias.Instancia:SCREEN-VALUE) NO-ERROR.
       
IF AVAILABLE(Instancias) THEN
    RUN Salvar_Creacion.
 OPEN QUERY Br_Creacion FOR EACH Instancias 
   NO-LOCK BY Instancias.Tipo_Instancia BY Instancias.Orden INDEXED-REPOSITION.
 RUN Cambio_Inst_creacion.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asignacion
&Scoped-define SELF-NAME B_CrSalvar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CrSalvar-2 wWin
ON CHOOSE OF B_CrSalvar-2 IN FRAME F_Asignacion /* Salvar */
DO: 
DO WITH FRAME F_Asignacion:
  
  ASSIGN ACmb_Agencia.
  IF CFG_Instancias.Tipo_Instancia:SCREEN-VALUE  = "" OR
     ACmb_Agencia:SCREEN-VALUE                   = "" OR
     CFG_Instancias.Orden:SCREEN-VALUE           = "" OR
     CFG_Instancias.Instancia:SCREEN-VALUE       = "" OR
     Cfg_Instancias.Monto_Maximo:SCREEN-VALUE    = "" OR
     Cfg_Instancias.Monto_Minimo:SCREEN-VALUE    = "" OR
     Cfg_Instancias.Plazo_Maximo:SCREEN-VALUE    = "" OR
     Cfg_Instancias.Plazo_Minimo:SCREEN-VALUE    = "" OR
     Cfg_Instancias.Usuario:SCREEN-VALUE         = "" OR
     Cfg_Instancias.Estado:SCREEN-VALUE          = "" OR
     Cfg_Instancias.Fec_Creacion:SCREEN-VALUE    = "" THEN
  DO:
     MESSAGE "Falta información por llenar para que" SKIP
             "el registro quede completo. Rectifique" VIEW-AS ALERT-BOX.
  END.
  
  IF W_Cr THEN
  DO:
    CREATE CFG_Instancias.
    DISABLE BT_Asignar WITH FRAME F_Asignaciones.
  END.
  ELSE
    FIND CFG_Instancias WHERE 
         CFG_Instancias.Agencia EQ INTEGER(SUBSTRING(ACmb_Agencia,1,3)) AND
         CFG_Instancias.Tipo_instancia EQ INTEGER(CFG_Instancias.Tipo_Instancia:SCREEN-VALUE) AND
         CFG_Instancias.Orden EQ INTEGER(CFG_Instancias.Orden:SCREEN-VALUE) AND
         CFG_Instancias.Instancia EQ INTEGER(CFG_Instancias.Instancia:SCREEN-VALUE) AND
         Cfg_Instancias.Usuario EQ Cfg_Instancias.Usuario:SCREEN-VALUE NO-ERROR.
       

IF AVAILABLE(CFG_Instancias) THEN
DO:
   RUN Salvar_Asignacion.
END.
   OPEN QUERY Br_Asignacion FOR EACH CFG_Instancias 
    SHARE-LOCK BY CFG_Instancias.Tipo_Instancia BY CFG_Instancias.Orden INDEXED-REPOSITION.
   RUN Cambio_Asig_Creacion.
END.
W_Cr = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creacion
&Scoped-define SELF-NAME CCmb_Tipos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CCmb_Tipos wWin
ON VALUE-CHANGED OF CCmb_Tipos IN FRAME F_Creacion /* Instancias */
Do:
 ASSIGN FRAME F_Creacion CCmb_Tipos.
 RUN Cambio_Inst_Creacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME C_CRDeshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C_CRDeshacer wWin
ON CHOOSE OF C_CRDeshacer IN FRAME F_Creacion /* Deshacer */
DO:
  RUN Deshacer_Cambios_Creacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asignacion
&Scoped-define SELF-NAME C_CRDeshacer-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C_CRDeshacer-2 wWin
ON CHOOSE OF C_CRDeshacer-2 IN FRAME F_Asignacion /* Deshacer */
DO:
  RUN Deshacer_Cambios_Asignacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME R_Opcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Opcion wWin
ON VALUE-CHANGED OF R_Opcion IN FRAME F-Main
DO:
  ASSIGN FRAME F-Main R_Opcion.
  IF R_Opcion EQ 1 THEN
  DO:
      VIEW FRAME F_Creacion.
      HIDE FRAME F_Asignacion.
  END.
  ELSE
  DO:
    IF W_Asigna1 THEN
    DO:
       FIND FIRST Cfg_Instancias WHERE Cfg_Instancias.Usuario NE "" NO-ERROR.
       IF AVAILABLE Cfg_Instancias THEN DO:
         RUN Mostrar_Campos_Asignacion.
         RUN Save_Temp_Asignacion.
         RUN Cambio_Asig_Creacion.
       END.
    END.
    W_Asigna1 = NO.
    VIEW FRAME F_Asignacion.
    HIDE FRAME F_Creacion.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asignacion
&Scoped-define SELF-NAME Cfg_Instancias.Usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_Instancias.Usuario wWin
ON LEAVE OF Cfg_Instancias.Usuario IN FRAME F_Asignacion /* Usuario */
DO:
DO WITH FRAME F_Asignaciones.
 FIND Usuarios WHERE Usuarios.Usuario EQ CFG_Instancias.Usuario:SCREEN-VALUE NO-LOCK NO-ERROR.
 IF NOT AVAILABLE(Usuarios) THEN
 DO:
     RUN C-Usuarios.r (INPUT-OUTPUT W_Rw).
     FIND Usuarios WHERE ROWID(Usuarios) EQ W_Rw.
     IF AVAILABLE(Usuarios) THEN
        ASSIGN CFG_Instancias.Usuario:SCREEN-VALUE = Usuarios.Usuario
               W_NomUsuario:SCREEN-VALUE = Usuarios.Nombre.
     ELSE
     DO:
       MESSAGE "No se eligio ningún usuario." SKIP
               "escoja algun usuario de la lista" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
     END.
 END.
 ELSE
     ASSIGN CFG_Instancias.Usuario:SCREEN-VALUE = Usuarios.Usuario
            W_NomUsuario:SCREEN-VALUE = Usuarios.Nombre.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConsAsignaciones
&Scoped-define SELF-NAME W_AgeAsi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_AgeAsi wWin
ON LEAVE OF W_AgeAsi IN FRAME F_ConsAsignaciones /* Ver  Agencia */
DO:
  OPEN QUERY Br_Asignacion 
     FOR EACH Cfg_Instancias WHERE 
              Cfg_Instancias.agencia EQ INT(SELF:SCREEN-VALUE) AND
              (IF (W_UsuAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones) <> '' THEN
                 Cfg_Instancias.Usuario = (W_UsuAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones)
               ELSE TRUE) AND
              (IF STRING(INT(W_VerAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones)) <> '0' THEN
                 Cfg_Instancias.Tipo_Instancia =INT(W_VerAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones)
               ELSE TRUE)
     SHARE-LOCK BY Cfg_Instancias.Tipo_Instancia 
                BY Cfg_Instancias.Orden 
                BY Cfg_Instancias.Usuario 
                 INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_UsuAsi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_UsuAsi wWin
ON LEAVE OF W_UsuAsi IN FRAME F_ConsAsignaciones /* Usuario */
DO:
  OPEN QUERY Br_Asignacion 
     FOR EACH Cfg_Instancias WHERE 
              Cfg_Instancias.Usuario EQ SELF:SCREEN-VALUE AND
              (IF STRING(INT(W_AgeAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones)) <> '0' THEN
                 Cfg_Instancias.Agencia = INT(W_AgeAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones)
               ELSE TRUE) AND 
              (IF STRING(INT(W_VerAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones)) <> '0' THEN
                 Cfg_Instancias.Tipo_Instancia = INT(W_VerAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones) 
               ELSE TRUE)
     SHARE-LOCK BY Cfg_Instancias.Tipo_Instancia 
                BY Cfg_Instancias.Orden 
                BY Cfg_Instancias.Usuario 
                BY Cfg_Instancias.Agencia INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_VerAsi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VerAsi wWin
ON LEAVE OF W_VerAsi IN FRAME F_ConsAsignaciones /* Ver solo las de Tipo */
DO:
  OPEN QUERY Br_Asignacion 
     FOR EACH Cfg_Instancias WHERE 
              Cfg_Instancias.Tipo_Instancia EQ INT(SELF:SCREEN-VALUE) AND
              (IF STRING(INT(W_AgeAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones)) <> '0' THEN
                 Cfg_Instancias.Agencia EQ INT(W_AgeAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones)
               ELSE TRUE) AND 
              (IF (W_UsuAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones) <> '' THEN
                 Cfg_Instancias.Usuario EQ (W_UsuAsi:SCREEN-VALUE IN FRAME F_ConsAsignaciones) 
               ELSE TRUE) 
     SHARE-LOCK BY Cfg_Instancias.Tipo_Instancia 
                BY Cfg_Instancias.Orden 
                BY Cfg_Instancias.Usuario 
                BY Cfg_Instancias.Agencia INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConsInstancias
&Scoped-define SELF-NAME W_VerCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VerCon wWin
ON LEAVE OF W_VerCon IN FRAME F_ConsInstancias /* Ver solo las de Tipo */
DO:
 OPEN QUERY Br_Creacion FOR EACH Instancias WHERE Instancias.Tipo_Instancia EQ INTEGER(SELF:SCREEN-VALUE) SHARE-LOCK
    BY Instancias.Tipo_Instancia BY Instancias.Orden INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Br_Asignacion
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cambio_Asig_Creacion wWin 
PROCEDURE Cambio_Asig_Creacion :
DO WITH FRAME F_Asignacion:
  j = INFO_CFG:NUM-ITEMS.
  DO i = 1 TO 30 BY 1:
      W_Ok = INFO_CFG:DELETE(1).
  END.
  DISPLAY INFO_CFG WITH FRAME F_Asignacion.
  RUN PInfo_Asignacion.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cambio_Inst_Creacion wWin 
PROCEDURE Cambio_Inst_Creacion :
DO WITH FRAME F_Creacion:
  Instancias.Tipo_Instancia:SCREEN-VALUE = SUBSTRING(CCmb_Tipos,1,5).
  j = INFO_Instancia:NUM-ITEMS.
  DO i = 1 TO 30 BY 1:
      W_Ok = INFO_Instancia:DELETE(1).
  END.
  DISPLAY INFO_instancia WITH FRAME F_Creacion.
  RUN PInfo_Instancia.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Deshacer_Cambios_Asignacion wWin 
PROCEDURE Deshacer_Cambios_Asignacion :
DO WITH FRAME F_Asignacion:
  ASSIGN CFG_Instancias.Tipo_Instancia:SCREEN-VALUE = STRING(T_ATipo_Instancia)
         Cfg_Instancias.Agencia:SCREEN-VALUE        = STRING(T_AAgencia)
         CFG_Instancias.Orden:SCREEN-VALUE          = STRING(T_AOrden)
         CFG_Instancias.Instancia:SCREEN-VALUE      = STRING(T_AInstancia)
         Cfg_Instancias.Monto_Maximo:SCREEN-VALUE   = STRING(T_AMonMax)
         Cfg_Instancias.Monto_Minimo:SCREEN-VALUE   = STRING(T_AMonMin)
         Cfg_Instancias.Plazo_Maximo:SCREEN-VALUE   = STRING(T_APlaMin)
         Cfg_Instancias.Plazo_Minimo:SCREEN-VALUE   = STRING(T_APlaMax)
         Cfg_Instancias.Usuario:SCREEN-VALUE        = STRING(T_AUsuario) 
         Cfg_Instancias.Estado:SCREEN-VALUE         = STRING(T_AEstado)
         Cfg_Instancias.Fec_Creacion:SCREEN-VALUE   = STRING(T_AFecCrea)
         Cfg_Instancias.Fec_Retiro:SCREEN-VALUE     = STRING(T_AFecRet).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Deshacer_Cambios_Creacion wWin 
PROCEDURE Deshacer_Cambios_Creacion :
DO WITH FRAME F_Creacion:
  ASSIGN Instancias.Nom_Instancia:SCREEN-VALUE   = STRING(T_Nom_Instancia)
         Instancias.Estado:SCREEN-VALUE          = STRING(T_Estado)
         Instancias.Tipo_Producto:SCREEN-VALUE   = STRING(T_Tipo_Producto)
         Instancias.Tipo_Instancia:SCREEN-VALUE  = STRING(T_Tipo_Instancia)
         Instancias.Instancia:SCREEN-VALUE       = STRING(T_Instancia)
         Instancias.Estado:SCREEN-VALUE          = STRING(T_Estado)
         Instancias.Fec_Retiro:SCREEN-VALUE      = STRING(T_Fec_Retiro)
         Instancias.Fec_Creacion:SCREEN-VALUE    = STRING(T_Fec_Creacion)
         Ccmb_Tipos:SCREEN-VALUE                 = T_CTipos.
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
  DISPLAY R_Opcion 
      WITH FRAME F-Main IN WINDOW wWin.
  ENABLE RECT-1 BUTTON-49 R_Opcion 
      WITH FRAME F-Main IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}

  {&OPEN-QUERY-F_Asignacion}
  GET FIRST F_Asignacion.
  DISPLAY ACmb_Agencia W_NomInstancia W_NomUsuario Info_CFG 
      WITH FRAME F_Asignacion IN WINDOW wWin.
  IF AVAILABLE Cfg_Instancias THEN 
    DISPLAY Cfg_Instancias.Agencia Cfg_Instancias.Estado 
          Cfg_Instancias.Fec_Creacion Cfg_Instancias.Tipo_Instancia 
          Cfg_Instancias.Fec_Retiro Cfg_Instancias.Orden 
          Cfg_Instancias.Instancia Cfg_Instancias.Usuario 
          Cfg_Instancias.Plazo_Minimo Cfg_Instancias.Plazo_Maximo 
          Cfg_Instancias.Monto_Minimo Cfg_Instancias.Monto_Maximo 
      WITH FRAME F_Asignacion IN WINDOW wWin.
  ENABLE Cfg_Instancias.Estado BUTTON-50 BUTTON-51 Cfg_Instancias.Usuario 
         Cfg_Instancias.Plazo_Minimo Cfg_Instancias.Plazo_Maximo 
         Cfg_Instancias.Monto_Minimo Cfg_Instancias.Monto_Maximo B_CrSalvar-2 
         Info_CFG C_CRDeshacer-2 B_CRIngresar-2 B_CRCancelar-2 B_CRBorrar-2 
         BtnDone-2 
      WITH FRAME F_Asignacion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Asignacion}

  {&OPEN-QUERY-F_Creacion}
  GET FIRST F_Creacion.
  DISPLAY CCmb_Tipos W_Programa W_Operacion Info_Instancia 
      WITH FRAME F_Creacion IN WINDOW wWin.
  IF AVAILABLE Instancias THEN 
    DISPLAY Instancias.Tipo_Instancia Instancias.Nom_Instancia 
          Instancias.Instancia Instancias.Fec_Creacion Instancias.Estado 
          Instancias.Id_Abogado Instancias.Id_Concepto Instancias.Fec_Retiro 
          Instancias.Orden_Instancia Instancias.Primera Instancias.Id_Negadas 
          Instancias.TMI Instancias.Ultima Instancias.Id_Scoring 
          Instancias.Honorarios Instancias.Tipo_Producto 
      WITH FRAME F_Creacion IN WINDOW wWin.
  ENABLE RECT-300 BUTTON-1 Instancias.Nom_Instancia BUTTON-2 Instancias.Estado 
         Instancias.Id_Abogado Instancias.Id_Concepto 
         Instancias.Orden_Instancia Instancias.Primera Instancias.Id_Negadas 
         Instancias.TMI Instancias.Ultima Instancias.Id_Scoring 
         Instancias.Honorarios Instancias.Tipo_Producto BUTTON-52 B_CrSalvar 
         BUTTON-59 C_CRDeshacer Info_Instancia B_CRIngresar B_CRCancelar 
         B_CRBorrar BtnDone 
      WITH FRAME F_Creacion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Creacion}
  DISPLAY W_VerAsi W_UsuAsi W_AgeAsi 
      WITH FRAME F_ConsAsignaciones IN WINDOW wWin.
  ENABLE Br_Asignacion W_VerAsi W_UsuAsi W_AgeAsi Btn_OutConAsi 
      WITH FRAME F_ConsAsignaciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConsAsignaciones}
  DISPLAY W_VerCon 
      WITH FRAME F_ConsInstancias IN WINDOW wWin.
  ENABLE Br_Creacion W_VerCon Btn_OutConIns 
      WITH FRAME F_ConsInstancias IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConsInstancias}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 ASSIGN FRAME F-Main R_Opcion.
 E_NumFila = 1.
 IF R_Opcion EQ 1 THEN
 DO:
    E_NumColumn = 6.
    E_Fila      = "005" + "Tipo "
                + "003" + "Ord"
                + "005" + "Insta"
                + "030" + "Nombre Instancia              "
                + "001" + "P"
                + "003" + "Cod".
    RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).
 END.
ELSE
 DO:
    E_NumColumn = 5.
    E_Fila      = "005" + "Tipo "
                + "003" + "Ord"
                + "005" + "Insta"
                + "005" + "Usuar"
                + "005" + "Agen ".
    RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).
 END.

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

IF R_Opcion EQ 1 THEN
 DO:
  FOR EACH Instancias NO-LOCK
      BREAK BY Instancias.Tipo_Instancia BY Instancias.Orden:
      E_Fila2     = "".
      E_Fila2     = "005" + STRING(Instancias.Tipo_Instancia,"99999")
                  + "003" + STRING(Instancias.Orden,"999")
                  + "005" + STRING(Instancias.Instancia,"99999")
                  + "030" + STRING(Instancias.Nom_Instancia,"X(30)")
                  + "001" + STRING(Instancias.Tipo_Producto,"9")
                  + "003" + STRING(Instancias.Cod_Producto,"999").
      {Incluido\imprimir_Excel.i}

  END.
 ELSE
 DO:
  FOR EACH CFG_Instancias NO-LOCK
      BREAK BY CFG_Instancias.Tipo_Instancia BY CFG_Instancias.Orden:
      E_Fila2     = "".
      E_Fila2     = "005" + STRING(CFG_Instancias.Tipo_Instancia,"99999")
                  + "003" + STRING(CFG_Instancias.Orden,"999")
                  + "005" + STRING(CFG_Instancias.Instancia,"99999")
                  + "004" + STRING(CFG_Instancias.Usuario,"X(18)")
                  + "004" + STRING(CFG_Instancias.Agencia,"9999").
     {Incluido\imprimir_Excel.i}
 END.

 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Informe wWin 
PROCEDURE Imprimir_Informe :
DEFINE VAR Listado AS CHARACTER INITIAL "".    
  listado = W_PathSpl + "Instancia.Lst".
  {Incluido\Imprimir.I "listado"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.                                         
DO WITH FRAME F_Creacion:
  RUN Save_Temp.
  FOR EACH Varios WHERE Varios.Tipo EQ 9:
      W_Ok = CCmb_Tipos:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME F_Creacion.
  END.
  FIND Varios WHERE Varios.Tipo EQ 9 AND
                  Varios.Codigo EQ INTEGER(Instancias.Tipo_Instancia:SCREEN-VALUE) NO-ERROR.
  IF AVAILABLE(Varios) THEN DO:
    CCmb_Tipos:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion NO-ERROR.
  END.
  W_Rw = ROWID(Instancias).
  FIND Instancias WHERE ROWID(Instancias) EQ W_Rw.
  RUN Cambio_Inst_Creacion.
  IF Instancias.Fec_Retiro:SCREEN-VALUE NE "" THEN
     DISABLE Instancias.Estado WITH FRAME F_Creacion. 
  ELSE
     ENABLE Instancias.Estado WITH FRAME F_Creacion.
END.

DO WITH FRAME F_Asignacion:
    FOR EACH Agencias WHERE Agencias.Estado EQ 1 AND Agencias.Fec_Retiro EQ ?:
        W_Ok = ACmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
    END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Campos_Asignacion wWin 
PROCEDURE Mostrar_Campos_Asignacion :
DO WITH FRAME F_Asignacion:
IF AVAILABLE(Cfg_instancias) THEN
 DO:
   ASSIGN Cfg_Instancias.Tipo_Instancia:SCREEN-VALUE = STRING(Cfg_Instancias.Tipo_Instancia)
          Cfg_Instancias.Instancia:SCREEN-VALUE      = STRING(Cfg_Instancias.Instancia)
          Cfg_Instancias.Usuario:SCREEN-VALUE        = CfG_Instancias.Usuario
          Cfg_Instancias.Orden:SCREEN-VALUE          = STRING(Cfg_Instancias.Orden,"999")
          Cfg_Instancias.Plazo_Minimo:SCREEN-VALUE   = STRING(Cfg_Instancias.Plazo_Minimo,"99999")
          Cfg_Instancias.Plazo_Maximo:SCREEN-VALUE   = STRING(Cfg_Instancias.Plazo_Maximo,"99999")
          Cfg_Instancias.Monto_Minimo:SCREEN-VALUE   = STRING(Cfg_Instancias.Monto_Minimo,">>>,>>>,>>>,>>9")
          Cfg_Instancias.Monto_Maximo:SCREEN-VALUE   = STRING(Cfg_Instancias.Monto_Maximo,">>>,>>>,>>>,>>9")
          Cfg_Instancias.Estado:SCREEN-VALUE         = STRING(Cfg_Instancias.Estado)
          Cfg_Instancias.Fec_Retiro:SCREEN-VALUE     = STRING(Cfg_Instancias.Fec_Retiro)
          Cfg_Instancias.Fec_Creacion:SCREEN-VALUE     = STRING(Cfg_Instancias.Fec_Creacion).
    FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
    IF AVAILABLE(Usuarios) THEN
        W_NomUsuario:SCREEN-VALUE = Usuarios.Nombre.
    FIND Agencias WHERE Agencias.Agencia EQ Cfg_Instancias.Agencia NO-LOCK NO-ERROR.
    IF AVAILABLE(Agencias) THEN
        ACmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
    
    IF Cfg_Instancias.Estado:SCREEN-VALUE EQ "2" AND Cfg_Instancias.Fec_Retiro:SCREEN-VALUE NE "" THEN
        DISABLE Cfg_Instancias.Estado.
    ELSE
        ENABLE Cfg_Instancias.Estado.
    IF Cfg_Instancias.Estado EQ 1 THEN B_CRBorrar-2:LABEL IN FRAME F_Asignacion = "Inactivar".
    ELSE  B_CRCancelar-2:LABEL IN FRAME F_Asignacion = "Activar".
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Campos_Creacion wWin 
PROCEDURE Mostrar_Campos_Creacion :
DO WITH FRAME F_Creacion:
    DISPLAY 
        Instancias.Tipo_Instancia
        Instancias.Nom_Instancia 
        Instancias.Instancia
        Instancias.Estado
        Instancias.Tipo_Producto
        Instancias.Fec_Retiro
        Instancias.Fec_Creacion
        Instancias.Orden 
        Instancias.Ultima
        Instancias.Primera
        Instancias.TMI 
        Instancias.Id_Scoring 
        Instancias.Id_Concepto
        Instancias.Id_Abogado
        Instancias.Id_Negadas
        Instancias.Honorarios WITH FRAME F_Creacion.

  IF Instancias.Estado EQ 1 THEN 
     B_CRBorrar:LABEL IN FRAME F_Creacion = "Inactivar".
  ELSE  B_CRBorrar:LABEL IN FRAME F_Creacion = "Activar".
       
  W_Programa:SCREEN-VALUE = "".
  IF Instancias.Programa NE 0 THEN DO:
     FIND Programas WHERE Programas.Programa EQ Instancias.Programa NO-LOCK NO-ERROR.
     IF AVAILABLE Programas THEN
        W_Programa:SCREEN-VALUE = STRING(Programas.Programa,"9999999999") + " - " + Programas.Descripcion.
  END.
  
  W_Operacion:SCREEN-VALUE = "".
  
  IF Instancias.Cod_Operacion NE 0 THEN DO:
     FIND Operacion WHERE Operacion.Cod_Operacion EQ Instancias.Cod_Operacion NO-LOCK NO-ERROR.
     IF AVAILABLE Operacion THEN
       W_Operacion:SCREEN-VALUE = STRING(Operacion.Cod_Operacion,"999999999") + " - " + Operacion.Nom_Operacion.
  END.
  
  FIND Varios WHERE Varios.Tipo EQ 9 AND
                  Varios.Codigo EQ INTEGER(Instancias.Tipo_Instancia:SCREEN-VALUE) NO-ERROR.
  IF AVAILABLE(Varios) THEN DO:
    CCmb_Tipos:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion NO-ERROR.
  END.
  IF Instancias.Fec_Retiro NE ? THEN
     DISABLE Instancias.Estado WITH FRAME F_Creacion. 
  ELSE
     ENABLE Instancias.Estado WITH FRAME F_Creacion.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PInfo_Asignacion wWin 
PROCEDURE PInfo_Asignacion :
W_Ok = INFO_CFG:ADD-LAST("Insta   Ord   Codigo  Usuario   PlaMin  PlaMax      MonMin      MonMax") IN FRAME F_Asignacion.
  FOR EACH CFG_Instancias WHERE 
      CFG_Instancias.Usuario EQ CFG_Instancias.Usuario:SCREEN-VALUE:
      W_Ok = INFO_CFG:ADD-LAST(
             STRING(CFG_Instancias.Tipo_Instancia,"99999") + " - " + 
             STRING(CFG_Instancias.Orden,"999") +  " - " +
             STRING(CFG_Instancias.Instancia,"99999") + " - " +
             STRING(CFG_Instancias.Usuario,"X(12)")  + "     - " +
             STRING(CFG_Instancias.Plazo_Minimo,"99999") + " - " +
             STRING(CFG_Instancias.Plazo_Maximo,"99999") + " - " +
             STRING(CFG_Instancias.Monto_Minimo,">>>>>>>>9") + " - " +
             STRING(CFG_Instancias.Monto_Maximo,">>>>>>>>9")).
  END.
  DISPLAY INFO_CFG WITH FRAME F_Asignacion.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PInfo_Instancia wWin 
PROCEDURE PInfo_Instancia :
W_Ok = INFO_Instancia:ADD-LAST("Insta   Ord   Codigo  Nombre                          Tpd  Pdt") IN FRAME F_Creacion.
  FOR EACH Instancias WHERE Instancias.Tipo_Instancia EQ INTEGER(SUBSTRING(CCmb_Tipos:SCREEN-VALUE,1,5))
      BREAK BY Instancias.Tipo_Instancia BY Instancias.Orden:
      W_Ok = INFO_Instancia:ADD-LAST(
             STRING(Instancias.Tipo_Instancia,"99999") + " - " + 
             STRING(Instancias.Orden_Instancia,"999") +  " - " +
             STRING(Instancias.Instancia,"99999") + " - " +
             STRING(Instancias.Nom_Instancia,"x(30)") + " - " +
             STRING(Instancias.Tipo_Producto,"9")).
  END.
  DISPLAY INFO_Instancia WITH FRAME F_Creacion.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}

  CASE W_If:
      WHEN 1 THEN 
      DO:    
         W_Reporte   = "REPORTE   : INSTANCIAS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
         W_EncColumna = "Tipo    Orden    Instancia     Nombre                         TipPdt   Producto".
         DEFINE FRAME F-Instancias
            Instancias.Tipo_Instancia   AT  2
            Instancias.Orden            AT 10
            Instancias.Instancia        AT 19
            Instancias.Nom_Instancia    AT 31 FORMAT "X(30)"
            Instancias.Tipo_Producto    AT 67
         WITH WIDTH 132 FRAME F-Instancias USE-TEXT STREAM-IO NO-LABELS NO-BOX.
         W_Linea = FILL(W_Raya,132).
         VIEW FRAME F-Encabezado.
         VIEW FRAME f-ftr.
         FOR EACH Instancias BREAK BY Instancias.Tipo_Instancia BY Instancias.Orden:
            DISPLAY
             Instancias.Tipo_Instancia   AT  2
             Instancias.Orden            AT 10
             Instancias.Instancia        AT 19
             Instancias.Nom_Instancia    AT 31 FORMAT "X(30)"
             Instancias.Tipo_Producto    AT 67
            WITH WIDTH 132 FRAME F-Instancias USE-TEXT STREAM-IO NO-LABELS NO-BOX.
         END.  
         PAGE.
      END.
      WHEN 2 THEN 
      DO:
          W_Reporte   = "REPORTE   : CFG_INSTANCIAS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
          W_EncColumna = "Tipo    Orden    Instancia     Usuario".
          DEFINE FRAME F-CFGInstancias
             CFG_Instancias.Tipo_Instancia   AT  2
             CFG_Instancias.Orden            AT 10
             CFG_Instancias.Instancia        AT 19
             CFG_Instancias.Usuario          AT 31
             CFG_Instancias.Agencia          AT 40
          WITH WIDTH 132 FRAME F-CFGInstancias USE-TEXT STREAM-IO NO-LABELS NO-BOX.
          W_Linea = FILL(W_Raya,132).
          VIEW FRAME F-Encabezado.
          VIEW FRAME f-ftr.
          FOR EACH CFG_Instancias BREAK BY CFG_Instancias.Tipo_Instancia BY CFG_Instancias.Orden:
             DISPLAY
              CFG_Instancias.Tipo_Instancia   AT  2
              CFG_Instancias.Orden            AT 10
              CFG_Instancias.Instancia        AT 19
              CFG_Instancias.Usuario          AT 31
              CFG_Instancias.Agencia          AT 40
             WITH WIDTH 132 FRAME F-CFGInstancias USE-TEXT STREAM-IO NO-LABELS NO-BOX.
          END.  
          PAGE.
       END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salvar_Asignacion wWin 
PROCEDURE Salvar_Asignacion :
DO WITH FRAME F_Asignacion:
    ASSIGN CFG_Instancias.Tipo_Instancia = INTEGER(CFG_Instancias.Tipo_Instancia:SCREEN-VALUE)
     Cfg_Instancias.Agencia        = INTEGER(SUBSTRING(ACmb_Agencia,1,3))
     CFG_Instancias.Orden          = INTEGER(CFG_Instancias.Orden:SCREEN-VALUE)
     CFG_Instancias.Instancia      = INTEGER(CFG_Instancias.Instancia:SCREEN-VALUE)
     Cfg_Instancias.Monto_Maximo   = DECIMAL(Cfg_Instancias.Monto_Maximo:SCREEN-VALUE)
     Cfg_Instancias.Monto_Minimo   = DECIMAL(Cfg_Instancias.Monto_Minimo:SCREEN-VALUE)  
     Cfg_Instancias.Plazo_Maximo   = INTEGER(Cfg_Instancias.Plazo_Maximo:SCREEN-VALUE)  
     Cfg_Instancias.Plazo_Minimo   = INTEGER(Cfg_Instancias.Plazo_Minimo:SCREEN-VALUE)
     Cfg_Instancias.Usuario        = Cfg_Instancias.Usuario:SCREEN-VALUE       
     Cfg_Instancias.Estado         = INTEGER(Cfg_Instancias.Estado:SCREEN-VALUE)
     Cfg_Instancias.Fec_Creacion   = DATE(Cfg_Instancias.Fec_Creacion:SCREEN-VALUE)  
     Cfg_Instancias.Fec_Retiro     = DATE(Cfg_Instancias.Fec_Retiro:SCREEN-VALUE).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salvar_Creacion wWin 
PROCEDURE Salvar_Creacion :
DO WITH FRAME F_Creacion:
     ASSIGN Instancias.Tipo_Instancia = INTEGER(SUBSTRING(CCmb_Tipos:SCREEN-VALUE,1,5))
            Instancias.Nom_Instancia  = Instancias.Nom_Instancia:SCREEN-VALUE
            Instancias.Tipo_Producto  = INTEGER(Instancias.Tipo_Producto:SCREEN-VALUE)
            Instancias.Fec_Creacion   = DATE(STRING(Instancias.Fec_Creacion:SCREEN-VALUE))
            Instancias.Estado         = INTEGER(Instancias.Estado:SCREEN-VALUE)
            Instancias.Fec_Retiro     = DATE(STRING(Instancias.Fec_Retiro:SCREEN-VALUE))
            Instancias.Orden          = INTEGER(Instancias.Orden:SCREEN-VALUE)
            Instancias.Instancia      = INTEGER(Instancias.Instancia:SCREEN-VALUE)
            Instancias.Ultima         = LOGICAL(Instancias.Ultima:SCREEN-VALUE)
            Instancias.Primera         = LOGICAL(Instancias.Primera:SCREEN-VALUE)
            Instancias.TMI            = INTEGER(Instancias.TMI:SCREEN-VALUE)
            Instancias.Programa       = INTEGER(SUBSTRING(W_Programa:SCREEN-VALUE,1,10))
            Instancias.Cod_Operacion  = INTEGER(SUBSTRING(W_Operacion:SCREEN-VALUE,1,9))
            Instancias.Honorarios     = DECIMAL(Instancias.Honorarios:SCREEN-VALUE)
            Instancias.Id_Scoring 
            Instancias.Id_Concepto
            Instancias.Id_Abogado
            Instancias.Id_Negadas.
            
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save_Temp wWin 
PROCEDURE Save_Temp :
DO WITH FRAME F_Creacion:
  ASSIGN T_Nom_Instancia   = Instancias.Nom_Instancia:SCREEN-VALUE
         T_Estado          = INTEGER(Instancias.Estado:SCREEN-VALUE)
         T_Tipo_Producto   = INTEGER(Instancias.Tipo_Producto:SCREEN-VALUE)
         T_Tipo_Instancia  = INTEGER(Instancias.Tipo_Instancia:SCREEN-VALUE)
         T_CTipos          = CCmb_Tipos:SCREEN-VALUE
         T_Instancia       = INTEGER(Instancias.Instancia:SCREEN-VALUE)
         T_Estado          = INTEGER(Instancias.Estado:SCREEN-VALUE)
         T_Fec_Retiro      = DATE(STRING(Instancias.Fec_Retiro:SCREEN-VALUE))
         T_Fec_Creacion    = DATE(STRING(Instancias.Fec_Creacion:SCREEN-VALUE)).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save_Temp_Asignacion wWin 
PROCEDURE Save_Temp_Asignacion :
DO WITH FRAME F_Asignacion:
  ASSIGN T_ATipo_Instancia = INTEGER(CFG_Instancias.Tipo_Instancia:SCREEN-VALUE)
         T_AAgencia        = INTEGER(Cfg_Instancias.Agencia:SCREEN-VALUE)
         T_AOrden          = INTEGER(CFG_Instancias.Orden:SCREEN-VALUE)
         T_AInstancia      = INTEGER(CFG_Instancias.Instancia:SCREEN-VALUE)
         T_AMonMax         = DECIMAL(Cfg_Instancias.Monto_Maximo:SCREEN-VALUE)
         T_AMonMin         = DECIMAL(Cfg_Instancias.Monto_Minimo:SCREEN-VALUE)  
         T_APlaMin         = INTEGER(Cfg_Instancias.Plazo_Maximo:SCREEN-VALUE)  
         T_APlaMax         = INTEGER(Cfg_Instancias.Plazo_Minimo:SCREEN-VALUE)
         T_AUsuario        = Cfg_Instancias.Usuario:SCREEN-VALUE       
         T_AEstado         = INTEGER(Cfg_Instancias.Estado:SCREEN-VALUE)
         T_AFecCrea        = DATE(Cfg_Instancias.Fec_Creacion:SCREEN-VALUE)  
         T_AFecRet         = DATE(Cfg_Instancias.Fec_Retiro:SCREEN-VALUE).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

