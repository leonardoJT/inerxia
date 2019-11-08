&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
ON RETURN TAB.

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR W_NomApe AS CHARACTER.
DEFINE VAR W_Age AS INTEGER.
DEFINE VAR W_Ok AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Activos
&Scoped-define BROWSE-NAME Br_Gtia

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Garantias

/* Definitions for BROWSE Br_Gtia                                       */
&Scoped-define FIELDS-IN-QUERY-Br_Gtia Garantias.Agencia Garantias.Nom_Bien Garantias.Identificacion_Bien Garantias.Nit Garantias.Val_Bien Garantias.Num_Credito Garantias.Num_Solicitud   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Gtia   
&Scoped-define SELF-NAME Br_Gtia
&Scoped-define OPEN-QUERY-Br_Gtia /*OPEN QUERY {&SELF-NAME} FOR EACH Garantias NO-LOCK INDEXED-REPOSITION.*/.
&Scoped-define TABLES-IN-QUERY-Br_Gtia Garantias
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Gtia Garantias

/* Definitions for FRAME F_Activos                                      */
&Scoped-define FIELDS-IN-QUERY-F_Activos Garantias.Contabilizada ~
Garantias.Descripcion_Bien Garantias.Estado Garantias.Val_Bien ~
Garantias.Fec_Creacion Garantias.Fec_FinSeguro Garantias.Fec_IniSeguro ~
Garantias.Fec_ProxAvaluo Garantias.Fec_Retiro Garantias.Fec_UltAvaluo ~
Garantias.Fec_VctoImpuesto Garantias.Nit_Aseguradora Garantias.Nom_Impuesto ~
Garantias.Nro_Seguro Garantias.Num_Credito Garantias.Num_Solicitud ~
Garantias.Tipo_Garantia Garantias.Usuario Garantias.Val_Asegurado ~
Garantias.Val_Impuesto Garantias.Val_UltAvaluo Garantias.Cod_Credito ~
Garantias.Agencia 

    /* oakley */
&Scoped-define QUERY-STRING-F_Activos FOR EACH Garantias SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Activos OPEN QUERY F_Activos FOR EACH Garantias SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Activos Garantias
&Scoped-define FIRST-TABLE-IN-QUERY-F_Activos Garantias


/* Definitions for FRAME F_Cons                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Cons ~
    ~{&OPEN-QUERY-Br_Gtia}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_NitUsu W_CodGtia RECT-1 
&Scoped-Define DISPLAYED-FIELDS Garantias.Contabilizada ~
Garantias.Descripcion_Bien Garantias.Estado Garantias.Val_Bien ~
Garantias.Fec_Creacion Garantias.Fec_FinSeguro Garantias.Fec_IniSeguro ~
Garantias.Fec_ProxAvaluo Garantias.Fec_Retiro Garantias.Fec_UltAvaluo ~
Garantias.Fec_VctoImpuesto Garantias.Nit_Aseguradora Garantias.Nom_Impuesto ~
Garantias.Nro_Seguro Garantias.Num_Credito Garantias.Num_Solicitud ~
Garantias.Tipo_Garantia Garantias.Usuario Garantias.Val_Asegurado ~
Garantias.Val_Impuesto Garantias.Val_UltAvaluo Garantias.Cod_Credito ~
Garantias.Agencia 
&Scoped-define DISPLAYED-TABLES Garantias
&Scoped-define FIRST-DISPLAYED-TABLE Garantias
&Scoped-Define DISPLAYED-OBJECTS W_NitUsu W_CodGtia W_NomGtia W_NomUsu 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 RBusca F_Busca 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE W_CodGtia AS CHARACTER FORMAT "X(12)" 
     LABEL "No. de la Garantía" 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY .81 TOOLTIP "No. de la Garantía"
     BGCOLOR 15 .

DEFINE VARIABLE W_NitUsu AS CHARACTER FORMAT "X(12)":U 
     LABEL "C.C / Nit del Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .81 TOOLTIP "C.C / Nit del Cliente"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NomGtia AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 43.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomUsu AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 87 BY 14.54.

DEFINE BUTTON Btn_Salir-2 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 12.43 BY 1.62.

DEFINE VARIABLE F_Busca AS CHARACTER FORMAT "X(256)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RBusca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Id.Bien", 1,
"Nit", 2,
"Nombre", 3,
"Activas", 4,
"Inactivas", 5,
"Contabilizadas", 6
     SIZE 70 BY .81
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 73 BY 2.69.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 164" 
     SIZE 15 BY 1.88.

DEFINE BUTTON Btn_Grabar 
     LABEL "Grabar Liberación" 
     SIZE 15 BY 1.62 TOOLTIP "Graba el Traslado".

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 15 BY 1.62.

DEFINE BUTTON BUTTON-166 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 166" 
     SIZE 15 BY 1.88.

DEFINE BUTTON BUTTON-167 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 167" 
     SIZE 15 BY 1.88.

DEFINE BUTTON BUTTON-168 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 168" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE WCre AS CHARACTER FORMAT "X(14)":U 
     LABEL "Crédito" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WDeb AS CHARACTER FORMAT "X(14)":U 
     LABEL "Débito" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Mens AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Gtia FOR 
      Garantias SCROLLING.

DEFINE QUERY F_Activos FOR 
      Garantias SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Gtia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Gtia Wwin _FREEFORM
  QUERY Br_Gtia NO-LOCK DISPLAY
      Garantias.Agencia FORMAT "999":U COLUMN-LABEL "Age"
      Garantias.Nom_Bien FORMAT "X(40)":U COLUMN-LABEL "Nombre del Bien"
      Garantias.Identificacion_Bien FORMAT "X(12)":U  COLUMN-LABEL "Identificación"
      Garantias.Nit FORMAT "X(14)" COLUMN-LABEL "Nit Deudor"
      Garantias.Val_Bien FORMAT ">>,>>>,>>>,>>9":U COLUMN-LABEL "Valor Garantía"
      Garantias.Num_Credito FORMAT "999999999":U COLUMN-LABEL "Núm.Crédito"
      Garantias.Num_Solicitud FORMAT "99999999" COLUMN-LABEL "Núm.Solicitud"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 6.81
         BGCOLOR 15 FGCOLOR 0 FONT 4 EXPANDABLE TOOLTIP "Con Dobler-Click captura la selecciòn".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Recibos
     BUTTON-167 AT ROW 1.54 COL 98
     BUTTON-166 AT ROW 3.42 COL 98
     Btn_Consulta AT ROW 5.31 COL 98
     Btn_Grabar AT ROW 17.42 COL 97
     Btn_Salir AT ROW 19.58 COL 97
     WDeb AT ROW 21.46 COL 10 COLON-ALIGNED
     WCre AT ROW 21.46 COL 34 COLON-ALIGNED
     W_Mens AT ROW 21.46 COL 51 COLON-ALIGNED NO-LABEL
     BUTTON-168 AT ROW 21.46 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 22.23
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Activos
     Garantias.Contabilizada AT ROW 6.38 COL 35
          LABEL "Contabilizada"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .77
     Garantias.Descripcion_Bien AT ROW 8.81 COL 6 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 79 BY 2.96
          BGCOLOR 15 
     Garantias.Estado AT ROW 5.58 COL 35 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 18 BY .81
     W_NitUsu AT ROW 1.27 COL 18 COLON-ALIGNED
     W_CodGtia AT ROW 2.42 COL 18 COLON-ALIGNED
     Garantias.Val_Bien AT ROW 7.73 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.86 BY .81
          BGCOLOR 7 FGCOLOR 15 
     W_NomGtia AT ROW 2.42 COL 33.72 COLON-ALIGNED NO-LABEL
     W_NomUsu AT ROW 1.27 COL 33.57 COLON-ALIGNED NO-LABEL
     Garantias.Fec_Creacion AT ROW 7.73 COL 46 COLON-ALIGNED
          LABEL "Fecha de Creación"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_FinSeguro AT ROW 15.81 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_IniSeguro AT ROW 14.96 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_ProxAvaluo AT ROW 17.35 COL 71 COLON-ALIGNED
          LABEL "Fec.Próx.Avaluo"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_Retiro AT ROW 7.73 COL 72 COLON-ALIGNED
          LABEL "Fecha de Retiro"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_UltAvaluo AT ROW 15.54 COL 71 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_VctoImpuesto AT ROW 14.19 COL 71 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Nit_Aseguradora AT ROW 13.19 COL 19 COLON-ALIGNED
          LABEL "Nit Aseguradora"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Nom_Impuesto AT ROW 12.31 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Nro_Seguro AT ROW 12.31 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Num_Credito AT ROW 5.31 COL 72 COLON-ALIGNED
          LABEL "Número de Credito"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .85
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Num_Solicitud AT ROW 6.38 COL 72 COLON-ALIGNED
          LABEL "Número de Solicitud"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Tipo_Garantia AT ROW 4.5 COL 17 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Propiedad", 1,
"Vehiculo", 2,
"Inversión", 3
          SIZE 32 BY .81
     Garantias.Usuario AT ROW 16.88 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Val_Asegurado AT ROW 14.08 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.86 ROW 1.54
         SIZE 91.14 BY 19.12
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Activos
     Garantias.Val_Impuesto AT ROW 13.23 COL 71 COLON-ALIGNED
          LABEL "Valor del Impuesto"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Val_UltAvaluo AT ROW 16.42 COL 71 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Cod_Credito AT ROW 6.46 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Agencia AT ROW 5.5 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RECT-1 AT ROW 4.12 COL 3
     "  Información de la Garantia" VIEW-AS TEXT
          SIZE 24 BY .81 AT ROW 3.69 COL 5
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.86 ROW 1.54
         SIZE 91.14 BY 19.12
         BGCOLOR 17 FONT 4
         TITLE "Liberación de Garantías".

DEFINE FRAME F_Cons
     Br_Gtia AT ROW 1.19 COL 3
     RBusca AT ROW 8.81 COL 5 NO-LABEL
     Btn_Salir-2 AT ROW 8.81 COL 77
     F_Busca AT ROW 9.88 COL 42 COLON-ALIGNED
     RECT-2 AT ROW 8.27 COL 3
     "  Buscar Garantías por" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 8 COL 5
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4.29 ROW 5.04
         SIZE 91.14 BY 11.04
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Garantías Vigentes del Cliente".


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
  CREATE WINDOW Wwin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Liberación de Garantías"
         HEIGHT             = 22.08
         WIDTH              = 114.29
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Wwin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW Wwin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Activos:FRAME = FRAME F_Recibos:HANDLE.

/* SETTINGS FOR FRAME F_Activos
   Custom                                                               */
/* SETTINGS FOR FILL-IN Garantias.Agencia IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Cod_Credito IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Garantias.Contabilizada IN FRAME F_Activos
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR EDITOR Garantias.Descripcion_Bien IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Garantias.Estado IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_Creacion IN FRAME F_Activos
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_FinSeguro IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_IniSeguro IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_ProxAvaluo IN FRAME F_Activos
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_Retiro IN FRAME F_Activos
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_UltAvaluo IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_VctoImpuesto IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Nit_Aseguradora IN FRAME F_Activos
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Nom_Impuesto IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Nro_Seguro IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Num_Credito IN FRAME F_Activos
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Num_Solicitud IN FRAME F_Activos
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Garantias.Tipo_Garantia IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Usuario IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Val_Asegurado IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Val_Bien IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Val_Impuesto IN FRAME F_Activos
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Val_UltAvaluo IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomGtia IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomUsu IN FRAME F_Activos
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Cons
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Gtia 1 F_Cons */
ASSIGN 
       FRAME F_Cons:HIDDEN           = TRUE
       FRAME F_Cons:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN F_Busca IN FRAME F_Cons
   1                                                                    */
/* SETTINGS FOR RADIO-SET RBusca IN FRAME F_Cons
   1                                                                    */
/* SETTINGS FOR FRAME F_Recibos
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Activos:MOVE-BEFORE-TAB-ITEM (BUTTON-167:HANDLE IN FRAME F_Recibos)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR BUTTON BUTTON-166 IN FRAME F_Recibos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WCre IN FRAME F_Recibos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WDeb IN FRAME F_Recibos
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Mens IN FRAME F_Recibos
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Gtia
/* Query rebuild information for BROWSE Br_Gtia
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Garantias NO-LOCK INDEXED-REPOSITION.*/
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Gtia */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Activos
/* Query rebuild information for FRAME F_Activos
     _TblList          = "bdCentral.Garantias"
     _Query            is NOT OPENED
*/  /* FRAME F_Activos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cons
/* Query rebuild information for FRAME F_Cons
     _Query            is NOT OPENED
*/  /* FRAME F_Cons */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Recibos
/* Query rebuild information for FRAME F_Recibos
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME F_Recibos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* SFG - Liberación de Garantías */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* SFG - Liberación de Garantías */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Gtia
&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME Br_Gtia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Gtia Wwin
ON MOUSE-SELECT-DBLCLICK OF Br_Gtia IN FRAME F_Cons
DO:
  W_CodGtia:SCREEN-VALUE IN FRAME F_Activos = Garantias.Identificacion_Bien.
  IF AVAIL(Garantias) THEN DO:
     FIND FIRST Creditos WHERE Creditos.Nit         EQ W_NitUsu
                           AND Creditos.Num_Credito EQ Garantias.Num_Credito
                           AND Creditos.Sdo_Capital GT 0 NO-LOCK NO-ERROR.
     IF AVAILABLE Creditos THEN DO:
        DISABLE Btn_Grabar WITH FRAME F_Recibos.
        W_Mens:SCREEN-VALUE IN FRAME F_Recibos = "Garantia Con Creditos Vigentes. No se permite descargar".
     END.
     ELSE DO:
        ENABLE Btn_Grabar WITH FRAME F_Recibos.
        W_Mens:SCREEN-VALUE IN FRAME F_Recibos = "".
     END.
        FIND FIRST CortoLargo WHERE CortoLargo.Agencia        EQ Garantias.Agencia
                                AND CortoLargo.Clase_Producto EQ 2                                           
                                AND CortoLargo.Cod_Producto   EQ Garantias.Cod_Credito NO-LOCK NO-ERROR.           

        ASSIGN W_CodGtia = Garantias.Identificacion_Bien
               W_NomGtia = Garantias.Nom_Bien
               W_NitUsu  = Garantias.Nit.
        RUN Mostrar_Garantia.
        ASSIGN WDeb:SCREEN-VALUE IN FRAME F_Recibos = CortoLargo.Cta_VigGarAd WHEN AVAIL(CortoLargo).
        ASSIGN WCre:SCREEN-VALUE                    = CortoLargo.Cta_GarPenCancel WHEN AVAIL(CortoLargo).

        CLOSE QUERY Br_Gtia.
        IF Garantias.Contabilizada AND NOT AVAILABLE Creditos THEN
           ENABLE Btn_Grabar WITH FRAME F_Recibos.
        ELSE 
           DISABLE Btn_Grabar WITH FRAME F_Recibos.
        ASSIGN FRAME F_Cons:VISIBLE      = FALSE
               FRAME F_Activos:SENSITIVE = TRUE
               W_CodGtia:SCREEN-VALUE    = W_CodGtia
               W_NomGtia:SCREEN-VALUE    = W_NomGtia.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Recibos
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta Wwin
ON CHOOSE OF Btn_Consulta IN FRAME F_Recibos /* Button 164 */
DO:
   FRAME F_Cons:TITLE = "Consulta General de Garantías".
   VIEW FRAME F_Cons.
   CLOSE QUERY Br_Gtia.
   OPEN QUERY Br_Gtia FOR EACH Garantias WHERE Garantias.Nit NE "" NO-LOCK.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar Wwin
ON CHOOSE OF Btn_Grabar IN FRAME F_Recibos /* Grabar Liberación */
DO:
   FIND FIRST Garantias WHERE Garantias.Nit                 EQ W_NitUsu
                          AND Garantias.Estado              EQ 1
                          AND Garantias.Identificacion_Bien EQ W_CodGtia
                          AND Garantias.Contabil               NO-LOCK NO-ERROR. 
   IF AVAIL(Garantias) THEN DO:
      FIND FIRST Creditos WHERE Creditos.Nit      EQ W_NitUsu
                         AND Creditos.Num_Credito EQ Garantias.Num_Credito
                         AND Creditos.Sdo_Capital GT 0 NO-LOCK NO-ERROR.
      IF NOT AVAIL(Creditos) THEN DO: 
         FIND FIRST CortoLargo WHERE CortoLargo.Agencia        EQ Garantias.Agencia
                                 AND CortoLargo.Clase_Producto EQ 2                                           
                                 AND CortoLargo.Cod_Producto   EQ  Garantias.Cod_Credito NO-LOCK NO-ERROR.
         IF AVAIL(CortoLargo) THEN DO:
            IF DECIMAL(Garantias.Val_Bien:SCREEN-VALUE IN FRAME F_Activos) GT 0 THEN DO:
               RUN ContabTras NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  FIND CURRENT Comprobantes NO-LOCK NO-ERROR. 
           
                  MESSAGE "   La Contabilizaciòn de la Liberación tiene errores, no se permite la operaciòn."
                      VIEW-AS ALERT-BOX ERROR.
                  RETURN.
               END.
           END.
           ELSE MESSAGE "No se hizo Liberación contable porque el Valor a Trasladar debe ser mayor que 0..."
               VIEW-AS ALERT-BOX TITLE "Informativo".    
        END.
        ELSE MESSAGE "Falta en CortoLargo para Garantías. Cod_Credito : " Garantias.Cod_Credito SKIP
                      "                                 de la Agencia : " Garantias.Agencia     SKIP
                      "           La configuraciòn para Contabilizar...Operaciòn Negada."
                VIEW-AS ALERT-BOX ERROR.                                      
      END.
   END.
   DISABLE Btn_Grabar WITH FRAME F_Recibos. 
   IF NOT AVAIL(Garantias) OR AVAIL(Creditos) THEN
      MESSAGE "No existe garantía para Liberar...Revise por favor." VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Wwin
ON CHOOSE OF Btn_Salir IN FRAME F_Recibos /* Salir */
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


&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME Btn_Salir-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir-2 Wwin
ON CHOOSE OF Btn_Salir-2 IN FRAME F_Cons /* Btn_Salir */
DO: 
  CLOSE QUERY Br_Gtia.

  HIDE FRAME F_Cons.

  ASSIGN FRAME F_Activos:SENSITIVE = TRUE.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Recibos
&Scoped-define SELF-NAME BUTTON-167
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-167 Wwin
ON CHOOSE OF BUTTON-167 IN FRAME F_Recibos /* Button 167 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME F_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Busca Wwin
ON LEAVE OF F_Busca IN FRAME F_Cons /* Buscar */
DO:
  ASSIGN FRAME F_Cons F_Busca RBusca.
  CASE RBusca:
    WHEN 1 THEN
        OPEN QUERY Br_Gtia FOR EACH Garantias NO-LOCK
                            WHERE Garantias.Identificacion_Bien EQ F_Busca. 
    WHEN 2 THEN
        OPEN QUERY Br_Gtia FOR EACH Garantias NO-LOCK
                            WHERE Garantias.Nit    EQ F_Busca. 
    WHEN 3 THEN
        OPEN QUERY Br_Gtia FOR EACH Garantias NO-LOCK
                            WHERE Garantias.Nom_Bien BEGINS F_Busca. 
  END CASE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RBusca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RBusca Wwin
ON VALUE-CHANGED OF RBusca IN FRAME F_Cons
DO:
  CASE SELF:SCREEN-VALUE:
    WHEN "4" THEN
      OPEN QUERY Br_Gtia FOR EACH Garantias NO-LOCK
                            WHERE Garantias.Estado EQ 1.
    WHEN "5" THEN
        OPEN QUERY Br_Gtia FOR EACH Garantias NO-LOCK
                            WHERE Garantias.Estado EQ 2. 
    WHEN "6" THEN
        OPEN QUERY Br_Gtia FOR EACH Garantias NO-LOCK
                            WHERE Garantias.Contabilizada. 
    OTHERWISE 
    DO: APPLY "Entry" TO F_Busca.
        RETURN NO-APPLY.
    END.

  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Activos
&Scoped-define SELF-NAME W_CodGtia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CodGtia Wwin
ON LEAVE OF W_CodGtia IN FRAME F_Activos /* No. de la Garantía */
DO:
   ASSIGN FRAME F_Activos W_CodGtia. 
IF SELF:SCREEN-VALUE NE "" THEN DO:
   FIND FIRST Garantias WHERE Garantias.Nit                 EQ W_NitUsu
                          AND Garantias.Estado              EQ 1
                          AND Garantias.Identificacion_Bien EQ W_CodGtia NO-LOCK NO-ERROR. 
   IF AVAIL(Garantias) THEN DO:
      IF NOT Contabilizada THEN DO:
         MESSAGE "No esta permitido Reversar una Garantia" SKIP
                 "que no ha sido contabilizada" VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.
      FIND FIRST Creditos WHERE Creditos.Nit      EQ W_NitUsu
                         AND Creditos.Num_Credito EQ Garantias.Num_Credito
                         AND Creditos.Sdo_Capital GT 0 NO-LOCK NO-ERROR.
      IF NOT AVAIL(Creditos) THEN DO:
         FIND FIRST CortoLargo WHERE CortoLargo.Agencia       EQ Garantias.Agencia
                                AND CortoLargo.Clase_Producto EQ 2                                           
                                AND CortoLargo.Cod_Producto   EQ Garantias.Cod_Credito NO-LOCK NO-ERROR.

         ASSIGN W_CodGtia = Garantias.Identificacion_Bien
                W_NomGtia = Garantias.Nom_Bien
                W_CodGtia:SCREEN-VALUE = W_CodGtia
                W_NomGtia:SCREEN-VALUE = W_NomGtia.
         RUN Mostrar_Garantia.
         ENABLE Btn_Grabar WITH FRAME F_Recibos.
         ASSIGN WDeb:SCREEN-VALUE IN FRAME F_Recibos = CortoLargo.Cta_VigGarAd WHEN AVAIL(CortoLargo).
         ASSIGN WCre:SCREEN-VALUE                    = CortoLargo.Cta_GarPenCancel WHEN AVAIL(CortoLargo).

      END.
      ELSE DO:
         MESSAGE "La Garantía tiene Pagarés Vigentes..., No se permite la Liberación."
              VIEW-AS ALERT-BOX.
         ASSIGN FRAME F_Activos:SENSITIVE = FALSE
                FRAME F_Cons:VISIBLE      = TRUE.
         CLOSE QUERY Br_Gtia.
         OPEN QUERY Br_Gtia FOR EACH Garantias NO-LOCK
                            WHERE Garantias.Nit    EQ W_NitUsu
                              AND Garantias.Estado EQ 1
                              AND Garantias.Contabil. 
      END.
   END.
   ELSE  MESSAGE "No existe la Garantía solicitada..., No se permite la Liberación."
              VIEW-AS ALERT-BOX.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitUsu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitUsu Wwin
ON LEAVE OF W_NitUsu IN FRAME F_Activos /* C.C / Nit del Cliente */
DO:
  ASSIGN W_NitUsu
         W_NomUsu:SCREEN-VALUE = "".

  FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitUsu NO-LOCK NO-ERROR.
  IF NOT AVAIL(Clientes) THEN DO:
     ASSIGN FRAME F_Activos:SENSITIVE = FALSE.
     /*RUN C-Clientes.R (INPUT  1,W_Agencia,
                       OUTPUT W_NitUsu, OUTPUT W_NomUsu, OUTPUT W_NomApe, OUTPUT W_Age).
     ASSIGN FRAME F_Activos:SENSITIVE = TRUE.
     FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitUsu NO-LOCK NO-ERROR.
     IF AVAIL(Clientes) THEN
        ASSIGN W_NitUsu:SCREEN-VALUE = W_NitUsu
               W_NomUsu:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + 
                                       " " + TRIM(Clientes.Nombre).
     ELSE
        MESSAGE "El Nit debe existir en Clientes" VIEW-AS ALERT-BOX.        */
     APPLY "choose" TO Btn_Consulta IN FRAME F_Recibos.
     RETURN NO-APPLY.
  END.
  ELSE ASSIGN W_NomUsu:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + 
                                      " " + TRIM(Clientes.Nombre).

  IF AVAIL(Clientes) THEN DO:
     FIND FIRST Garantias WHERE Garantias.Nit    EQ W_NitUsu
                            AND Garantias.Estado EQ 1
                            AND Garantias.Contabilizada NO-LOCK NO-ERROR. 
     IF AVAIL(Garantias) THEN DO:
        ASSIGN FRAME F_Activos:SENSITIVE = FALSE.
        FRAME F_Cons:TITLE = "Garantías Vigentes del Cliente".
        VIEW FRAME F_Cons.

        CLOSE QUERY Br_Gtia.

        OPEN QUERY Br_Gtia FOR EACH Garantias NO-LOCK
                            WHERE Garantias.Nit    EQ W_NitUsu
                              AND Garantias.Estado EQ 1
                              AND Garantias.Contabilizada. 
     END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Wwin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects Wwin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContabTras Wwin 
PROCEDURE ContabTras :
/*------------------------------------------------------------------------------
  Purpose:     
 ------------------------------------------------------------------------------*/
  FIND Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_GarPenCancel
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Cuentas) THEN DO:
     MESSAGE "No se encuentra la Cuenta CortoLargo.Cta_GarPenCancel." SKIP
                    "consulte con el administrador!" VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.

  FIND Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_VigGarAd
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Cuentas) THEN DO:
     MESSAGE "No se encuentra la Cuenta CortoLargo.Cta_VigGarAd." SKIP
                    "consulte con el administrador!" VIEW-AS ALERT-BOX.
     RETURN ERROR.
  END.
    
  FIND Comprobantes WHERE Comprobantes.Agencia     EQ Garantias.Agencia
                      AND Comprobantes.Comprobante EQ CortoLargo.Comprobante NO-ERROR.
  IF NOT AVAILABLE Comprobantes THEN DO:
     MESSAGE "No se ha encontrado el Comprobante del CortoLargo.Comprobante." SKIP
             " para la contabilización... Rectifique con el Administrador!" 
           VIEW-AS ALERT-BOX ERROR. 
    RETURN ERROR.
  END.  
    
  ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1. 
    
  FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Garantias.Agencia
           Mov_Contable.Cuenta = CortoLargo.Cta_GarPenCancel
           Mov_Contable.Nit = W_NitUsu
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = "Liberación Garantía"
           Mov_Contable.Usuario = W_Usuario
           Mov_Contable.Cen_Costos = W_Cencosgral
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Comprobante = Comprobantes.Comprobante
           Mov_Contable.Num_Documento = Comprobantes.Secuencia
           Mov_Contable.Doc_Referencia = W_CodGtia
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.Cr = DECIMAL(Garantias.Val_Bien:SCREEN-VALUE IN FRAME F_Activos).

    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Garantias.Agencia
           Mov_Contable.Cuenta = CortoLargo.Cta_VigGarAd
           Mov_Contable.Nit = W_NitUsu
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = "Liberación Garantía"
           Mov_Contable.Usuario = W_Usuario
           Mov_Contable.Cen_Costos = W_Cencosgral
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Comprobante = Comprobantes.Comprobante
           Mov_Contable.Num_Documento = Comprobantes.Secuencia
           Mov_Contable.Doc_Referencia = W_CodGtia
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.Db = DECIMAL(Garantias.Val_Bien:SCREEN-VALUE IN FRAME F_Activos).

    FIND CURRENT Garantias NO-ERROR.

    ASSIGN Garantias.Estado = 2
           Garantias.Fec_Retiro = TODAY.
            
  FIND CURRENT Garantias NO-LOCK NO-ERROR.

  FIND Formatos WHERE Formatos.Agencia     EQ Garantias.Agencia                
                  AND Formatos.Cod_Formato EQ Comprobantes.Cod_Formato  NO-LOCK NO-ERROR.                                          
  IF AVAILABLE(Formatos) THEN DO:                                             
     RUN VALUE(Formatos.Nom_Proceso) (INPUT Comprobantes.Comprobante,                            
                                      INPUT Comprobantes.Secuencia, INPUT Comprobantes.Secuencia,          
                                      INPUT Garantias.Agencia) NO-ERROR.                              
     IF ERROR-STATUS:ERROR THEN
        MESSAGE "Formato Impresiòn con error" VIEW-AS ALERT-BOX.

  END.
  ELSE                                                                      
     MESSAGE "No se hallò el Formato de Impresión" SKIP                    
              "consulte con el administrador!...,La contabilizaciòn està Ok." VIEW-AS ALERT-BOX.                     
             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Wwin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
  THEN DELETE WIDGET Wwin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Wwin  _DEFAULT-ENABLE
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
  DISPLAY WDeb WCre W_Mens 
      WITH FRAME F_Recibos IN WINDOW Wwin.
  ENABLE BUTTON-167 Btn_Consulta Btn_Grabar Btn_Salir BUTTON-168 
      WITH FRAME F_Recibos IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Recibos}
  DISPLAY W_NitUsu W_CodGtia W_NomGtia W_NomUsu 
      WITH FRAME F_Activos IN WINDOW Wwin.
  IF AVAILABLE Garantias THEN 
    DISPLAY Garantias.Contabilizada Garantias.Descripcion_Bien Garantias.Estado 
          Garantias.Val_Bien Garantias.Fec_Creacion Garantias.Fec_FinSeguro 
          Garantias.Fec_IniSeguro Garantias.Fec_ProxAvaluo Garantias.Fec_Retiro 
          Garantias.Fec_UltAvaluo Garantias.Fec_VctoImpuesto 
          Garantias.Nit_Aseguradora Garantias.Nom_Impuesto Garantias.Nro_Seguro 
          Garantias.Num_Credito Garantias.Num_Solicitud Garantias.Tipo_Garantia 
          Garantias.Usuario Garantias.Val_Asegurado Garantias.Val_Impuesto 
          Garantias.Val_UltAvaluo Garantias.Cod_Credito Garantias.Agencia 
      WITH FRAME F_Activos IN WINDOW Wwin.
  ENABLE W_NitUsu W_CodGtia RECT-1 
      WITH FRAME F_Activos IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Activos}
  DISPLAY RBusca F_Busca 
      WITH FRAME F_Cons IN WINDOW Wwin.
  ENABLE Br_Gtia RBusca Btn_Salir-2 F_Busca RECT-2 
      WITH FRAME F_Cons IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cons}
  VIEW Wwin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject Wwin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Campos Wwin 
PROCEDURE Inicializar_Campos :
DO WITH FRAME F_Activos:
    
        
  END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
RUN SUPER.

 /* Wwin:TITLE = P_NomIns.*/
  
  
  RUN Inicializar_Campos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Garantia Wwin 
PROCEDURE Mostrar_Garantia :
W_NitUsu:SCREEN-VALUE IN FRAME F_Activos = Garantias.Nit.
  FIND Clientes WHERE Clientes.Nit EQ Garantias.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN
     W_NomUsu:SCREEN-VALUE IN FRAME F_Activos = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  DISPLAY Garantias.Agencia 
          Garantias.Cod_Credito
          Garantias.Contabilizada
          Garantias.Descripcion_Bien
          Garantias.Estado
          Garantias.Fec_Creacion
          Garantias.Fec_FinSeguro
          Garantias.Fec_IniSeguro
          Garantias.Fec_ProxAvaluo
          Garantias.Fec_Retiro
          Garantias.Fec_UltAvaluo
          Garantias.Fec_VctoImpuesto
          Garantias.Nit_Aseguradora
          Garantias.Nom_Impuesto
          Garantias.Nro_Seguro
          Garantias.Num_Credito
          Garantias.Num_Solicitud
          Garantias.Tipo_Garantia
          Garantias.Usuario
          Garantias.Val_Asegurado
          Garantias.Val_Bien
          Garantias.Val_Impuesto
          Garantias.Val_UltAvaluo WITH FRAME F_Activos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

