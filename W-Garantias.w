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
{Incluido\VARIABLE.I "SHARED"}
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_Nuevo    AS LOGICAL.
DEFINE VAR W_Puntero  AS ROWID.

/*para llamar consulta de garantias*/
  DEFINE VAR g_Nit      LIKE Clientes.Nit.
  DEFINE VAR g_id       LIKE Garantias.Identificacion_Bien.
  DEFINE VAR g_tp       LIKE Garantias.Tipo_Garantia.
  DEFINE VAR g_Cd       LIKE Garantias.Cod_Credito.
  DEFINE VAR g_Nc       LIKE Garantias.Num_Credito.
  DEFINE VAR g_NS       LIKE Garantias.Num_Solicitud.

/*para buscar cliente*/
DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
  

DEFINE TEMP-TABLE TGar LIKE Garantias.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Gar
&Scoped-define BROWSE-NAME B_GarExi

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TGar Garantias

/* Definitions for BROWSE B_GarExi                                      */
&Scoped-define FIELDS-IN-QUERY-B_GarExi TGar.Identificacion_Bien TGar.Nom_Bien TGar.Nit TGar.Num_Solicitud TGar.Cod_Credito TGar.Val_Bien   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_GarExi   
&Scoped-define SELF-NAME B_GarExi
&Scoped-define QUERY-STRING-B_GarExi FOR EACH TGar NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_GarExi OPEN QUERY {&SELF-NAME} FOR EACH TGar NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_GarExi TGar
&Scoped-define FIRST-TABLE-IN-QUERY-B_GarExi TGar


/* Definitions for FRAME F_Gar                                          */
&Scoped-define FIELDS-IN-QUERY-F_Gar Garantias.Estado ~
Garantias.Tipo_Garantia Garantias.Identificacion_Bien Garantias.Nom_Bien ~
Garantias.Nit Garantias.Fec_Creacion Garantias.Tip_Credito ~
Garantias.Cod_Credito Garantias.Fec_Retiro Garantias.Num_Credito ~
Garantias.Pagare Garantias.Contabilizada Garantias.Num_Solicitud ~
Garantias.Descripcion_Bien Garantias.Val_Bien Garantias.Nit_Aseguradora ~
Garantias.Nro_Seguro Garantias.Val_Asegurado Garantias.Fec_IniSeguro ~
Garantias.Fec_FinSeguro Garantias.Nom_Impuesto Garantias.Val_Impuesto ~
Garantias.Fec_VctoImpuesto Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo ~
Garantias.Val_UltAvaluo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Gar Garantias.Tipo_Garantia ~
Garantias.Identificacion_Bien Garantias.Nom_Bien Garantias.Nit ~
Garantias.Tip_Credito Garantias.Cod_Credito Garantias.Num_Credito ~
Garantias.Contabilizada Garantias.Descripcion_Bien Garantias.Val_Bien ~
Garantias.Nit_Aseguradora Garantias.Nro_Seguro Garantias.Val_Asegurado ~
Garantias.Fec_IniSeguro Garantias.Fec_FinSeguro Garantias.Nom_Impuesto ~
Garantias.Val_Impuesto Garantias.Fec_VctoImpuesto Garantias.Fec_ProxAvaluo ~
Garantias.Fec_UltAvaluo Garantias.Val_UltAvaluo 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Gar Garantias
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Gar Garantias
&Scoped-define QUERY-STRING-F_Gar FOR EACH Garantias SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Gar OPEN QUERY F_Gar FOR EACH Garantias SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Gar Garantias
&Scoped-define FIRST-TABLE-IN-QUERY-F_Gar Garantias


/* Definitions for FRAME F_GarExi                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_GarExi ~
    ~{&OPEN-QUERY-B_GarExi}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Garantias.Tipo_Garantia ~
Garantias.Identificacion_Bien Garantias.Nom_Bien Garantias.Nit ~
Garantias.Tip_Credito Garantias.Cod_Credito Garantias.Num_Credito ~
Garantias.Contabilizada Garantias.Descripcion_Bien Garantias.Val_Bien ~
Garantias.Nit_Aseguradora Garantias.Nro_Seguro Garantias.Val_Asegurado ~
Garantias.Fec_IniSeguro Garantias.Fec_FinSeguro Garantias.Nom_Impuesto ~
Garantias.Val_Impuesto Garantias.Fec_VctoImpuesto Garantias.Fec_ProxAvaluo ~
Garantias.Fec_UltAvaluo Garantias.Val_UltAvaluo 
&Scoped-define ENABLED-TABLES Garantias
&Scoped-define FIRST-ENABLED-TABLE Garantias
&Scoped-Define ENABLED-OBJECTS BUTTON-2 Cmb_Agencias BUTTON-3 BUTTON-4 ~
BUTTON-1 Btn_Salvar Btn_Deshacer Btn_ingresar Btn_Cancelar Btn_Descargar ~
Btn_Salir BUTTON-11 RECT-1 RECT-2 RECT-297 RECT-3 RECT-4 
&Scoped-Define DISPLAYED-FIELDS Garantias.Estado Garantias.Tipo_Garantia ~
Garantias.Identificacion_Bien Garantias.Nom_Bien Garantias.Nit ~
Garantias.Fec_Creacion Garantias.Tip_Credito Garantias.Cod_Credito ~
Garantias.Fec_Retiro Garantias.Num_Credito Garantias.Pagare ~
Garantias.Contabilizada Garantias.Num_Solicitud Garantias.Descripcion_Bien ~
Garantias.Val_Bien Garantias.Nit_Aseguradora Garantias.Nro_Seguro ~
Garantias.Val_Asegurado Garantias.Fec_IniSeguro Garantias.Fec_FinSeguro ~
Garantias.Nom_Impuesto Garantias.Val_Impuesto Garantias.Fec_VctoImpuesto ~
Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo Garantias.Val_UltAvaluo 
&Scoped-define DISPLAYED-TABLES Garantias
&Scoped-define FIRST-DISPLAYED-TABLE Garantias
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencias W_NomDeu W_NomPro W_NomAse ~
W_NomUsu 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 BUTTON-2 BUTTON-3 BUTTON-4 Btn_ingresar Btn_Salir ~
BUTTON-11 
&Scoped-define List-2 Garantias.Estado W_NomDeu Garantias.Fec_Creacion ~
W_NomPro Garantias.Fec_Retiro Garantias.Pagare Garantias.Num_Solicitud ~
W_NomAse W_NomUsu 
&Scoped-define List-5 Cmb_Agencias Garantias.Tipo_Garantia ~
Garantias.Identificacion_Bien Garantias.Nit Garantias.Tip_Credito ~
Garantias.Cod_Credito Garantias.Num_Credito 
&Scoped-define List-6 Garantias.Contabilizada Garantias.Descripcion_Bien ~
Garantias.Val_Bien Garantias.Nit_Aseguradora Garantias.Nro_Seguro ~
Garantias.Val_Asegurado Garantias.Fec_IniSeguro Garantias.Fec_FinSeguro ~
Garantias.Nom_Impuesto Garantias.Val_Impuesto Garantias.Fec_VctoImpuesto 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancelar 
     LABEL "Cancelar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Descargar 
     LABEL "Descargar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Deshacer 
     LABEL "Deshacer" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_ingresar 
     LABEL "Ingresar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-1 
     LABEL "i" 
     SIZE 3 BY .81
     FONT 0.

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 2" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 3" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 4" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomAse AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomDeu AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomPro AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomUsu AS CHARACTER FORMAT "X(50)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 78 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 1.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 3.5.

DEFINE RECTANGLE RECT-297
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 4.31.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 4.04.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 10.23.

DEFINE BUTTON Btn_OutExi 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 179" 
     SIZE 6 BY 1.38.

DEFINE BUTTON BUTTON-178 
     LABEL "Asumir Información Principal" 
     SIZE 29 BY 1.12.

DEFINE BUTTON BUTTON-179 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 179" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE S_InfoCre AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 39 BY 5.12
     BGCOLOR 15 FONT 2 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_GarExi FOR 
      TGar SCROLLING.

DEFINE QUERY F_Gar FOR 
      Garantias SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_GarExi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_GarExi wWin _FREEFORM
  QUERY B_GarExi NO-LOCK DISPLAY
      TGar.Identificacion_Bien COLUMN-LABEL "Id.Bien"
  TGar.Nom_Bien       COLUMN-LABEL "Nombre"
  TGar.Nit            COLUMN-LABEL "Nit"
  TGar.Num_Solicitud  COLUMN-LABEL "Nro.Cred."
  TGar.Cod_Credito    COLUMN-LABEL "Cod.Pdt"
  TGar.Val_Bien       COLUMN-LABEL "Valor"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93 BY 5.92
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Gar
     Garantias.Estado AT ROW 1.15 COL 16 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 19 BY .81
     BUTTON-2 AT ROW 1.54 COL 101
     Cmb_Agencias AT ROW 2.08 COL 14 COLON-ALIGNED
     Garantias.Tipo_Garantia AT ROW 2.08 COL 58 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Propiedad", 1,
"Vehiculo", 2,
"Inversión", 3
          SIZE 38 BY .81
     BUTTON-3 AT ROW 3.15 COL 101
     Garantias.Identificacion_Bien AT ROW 3.69 COL 14 COLON-ALIGNED
          LABEL "Id"
          VIEW-AS FILL-IN 
          SIZE 28 BY .81
          BGCOLOR 15 
     Garantias.Nom_Bien AT ROW 3.69 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 52 BY .81
          BGCOLOR 15 
     Garantias.Nit AT ROW 4.62 COL 14 COLON-ALIGNED
          LABEL "Nit Deudor"
          VIEW-AS FILL-IN 
          SIZE 28 BY .81
          BGCOLOR 15 
     W_NomDeu AT ROW 4.62 COL 43 COLON-ALIGNED NO-LABEL
     BUTTON-4 AT ROW 4.77 COL 101
     Garantias.Fec_Creacion AT ROW 5.54 COL 81.29 COLON-ALIGNED
          LABEL "Creacion"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Tip_Credito AT ROW 5.58 COL 16 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Consumo", 1,
"Comercial", 2,
"Hipotecario", 3,
"Microcredito", 4
          SIZE 52 BY .81
     Garantias.Cod_Credito AT ROW 6.38 COL 14 COLON-ALIGNED
          LABEL "Producto"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     W_NomPro AT ROW 6.38 COL 25 COLON-ALIGNED NO-LABEL
     Garantias.Fec_Retiro AT ROW 6.46 COL 81.29 COLON-ALIGNED
          LABEL "Retiro"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Num_Credito AT ROW 7.31 COL 14 COLON-ALIGNED
          LABEL "Nro.Crédito"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     BUTTON-1 AT ROW 7.31 COL 27
     Garantias.Pagare AT ROW 7.31 COL 50.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Contabilizada AT ROW 7.46 COL 79
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .77
     Garantias.Num_Solicitud AT ROW 8.27 COL 14 COLON-ALIGNED
          LABEL "Nro.Solicitud"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Descripcion_Bien AT ROW 9.77 COL 16 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 77 BY 1.73
          BGCOLOR 15 
     Btn_Salvar AT ROW 9.88 COL 101
     Btn_Deshacer AT ROW 11.5 COL 101
     Garantias.Val_Bien AT ROW 11.62 COL 14 COLON-ALIGNED
          LABEL "Valor"
          VIEW-AS FILL-IN 
          SIZE 15.43 BY .81
          BGCOLOR 15 
     Btn_ingresar AT ROW 13.12 COL 101
     Garantias.Nit_Aseguradora AT ROW 13.38 COL 31 COLON-ALIGNED
          LABEL "Nit Aseuguradora"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 
     W_NomAse AT ROW 13.38 COL 45 COLON-ALIGNED NO-LABEL
     Garantias.Nro_Seguro AT ROW 14.27 COL 31 COLON-ALIGNED
          LABEL "Id"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 21.38
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Gar
     Garantias.Val_Asegurado AT ROW 14.27 COL 73.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 15 
     Btn_Cancelar AT ROW 14.73 COL 101
     Garantias.Fec_IniSeguro AT ROW 15.15 COL 31 COLON-ALIGNED
          LABEL "Inicio Seguro"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 
     Garantias.Fec_FinSeguro AT ROW 15.15 COL 73.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 15 
     Btn_Descargar AT ROW 16.35 COL 101
     Garantias.Nom_Impuesto AT ROW 17.15 COL 31 COLON-ALIGNED
          LABEL "Nombre"
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .81
          BGCOLOR 15 
     Garantias.Val_Impuesto AT ROW 18.12 COL 31 COLON-ALIGNED
          LABEL "Valor"
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .81
          BGCOLOR 15 
     Garantias.Fec_VctoImpuesto AT ROW 19.31 COL 31 COLON-ALIGNED
          LABEL "Vencimiento"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 
     Btn_Salir AT ROW 17.96 COL 101
     Garantias.Fec_ProxAvaluo AT ROW 17.15 COL 74 COLON-ALIGNED
          LABEL "Fec.Próx.Avaluo"
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .81
          BGCOLOR 15 
     Garantias.Fec_UltAvaluo AT ROW 18.23 COL 74 COLON-ALIGNED
          LABEL "Fec.Últ.Avaluo"
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .81
          BGCOLOR 15 
     Garantias.Val_UltAvaluo AT ROW 19.31 COL 74 COLON-ALIGNED
          LABEL "Valor Últ.Avaluo"
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .81
          BGCOLOR 15 
     BUTTON-11 AT ROW 20.38 COL 105
     W_NomUsu AT ROW 21.19 COL 13 COLON-ALIGNED
     RECT-1 AT ROW 1.54 COL 57
     RECT-2 AT ROW 12.85 COL 15
     RECT-297 AT ROW 16.62 COL 58
     RECT-3 AT ROW 16.62 COL 15
     RECT-4 AT ROW 9.62 COL 100
     "Tipo de Garantía" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.27 COL 59
          FGCOLOR 7 
     " Avaluos" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 16.35 COL 60
          FGCOLOR 7 
     "Descripción de la Garantía" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 9.08 COL 16
          FGCOLOR 7 
     "Seguros" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.58 COL 16
          FGCOLOR 7 
     "Impuestos" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 16.35 COL 16
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 21.38
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_GarExi
     B_GarExi AT ROW 2.08 COL 3
     Btn_OutExi AT ROW 8.27 COL 90
     BUTTON-178 AT ROW 8.54 COL 3
     "Esta Garantía se encuentra respaldando los siguientes créditos o solicitudes" VIEW-AS TEXT
          SIZE 52 BY .62 AT ROW 1.27 COL 3
          FGCOLOR 7 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 8.81
         SIZE 97 BY 9.69
         BGCOLOR 17 FONT 4
         TITLE "Garantias".

DEFINE FRAME F_InfoCre
     S_InfoCre AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-179 AT ROW 6.65 COL 34
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 22 ROW 10.42
         SIZE 41 BY 8.35
         BGCOLOR 17 FONT 4
         TITLE "Información del Crédito".


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
         TITLE              = "Mantenimiento de Garantias Admisibles"
         HEIGHT             = 21.38
         WIDTH              = 114
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
ASSIGN FRAME F_GarExi:FRAME = FRAME F_Gar:HANDLE
       FRAME F_InfoCre:FRAME = FRAME F_Gar:HANDLE.

/* SETTINGS FOR FRAME F_Gar
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_InfoCre:MOVE-BEFORE-TAB-ITEM (Garantias.Estado:HANDLE IN FRAME F_Gar)
       XXTABVALXX = FRAME F_GarExi:MOVE-BEFORE-TAB-ITEM (FRAME F_InfoCre:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR BUTTON Btn_ingresar IN FRAME F_Gar
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Salir IN FRAME F_Gar
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-11 IN FRAME F_Gar
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-2 IN FRAME F_Gar
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-3 IN FRAME F_Gar
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-4 IN FRAME F_Gar
   1                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_Agencias IN FRAME F_Gar
   5                                                                    */
/* SETTINGS FOR FILL-IN Garantias.Cod_Credito IN FRAME F_Gar
   5 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Garantias.Contabilizada IN FRAME F_Gar
   6                                                                    */
/* SETTINGS FOR EDITOR Garantias.Descripcion_Bien IN FRAME F_Gar
   6                                                                    */
/* SETTINGS FOR RADIO-SET Garantias.Estado IN FRAME F_Gar
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN Garantias.Fec_Creacion IN FRAME F_Gar
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Garantias.Fec_FinSeguro IN FRAME F_Gar
   6                                                                    */
/* SETTINGS FOR FILL-IN Garantias.Fec_IniSeguro IN FRAME F_Gar
   6 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Garantias.Fec_ProxAvaluo IN FRAME F_Gar
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_Retiro IN FRAME F_Gar
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Garantias.Fec_UltAvaluo IN FRAME F_Gar
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_VctoImpuesto IN FRAME F_Gar
   6 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Garantias.Identificacion_Bien IN FRAME F_Gar
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Garantias.Nit IN FRAME F_Gar
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Garantias.Nit_Aseguradora IN FRAME F_Gar
   6 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Garantias.Nom_Bien IN FRAME F_Gar
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Nom_Impuesto IN FRAME F_Gar
   6 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Garantias.Nro_Seguro IN FRAME F_Gar
   6 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Garantias.Num_Credito IN FRAME F_Gar
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Garantias.Num_Solicitud IN FRAME F_Gar
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Garantias.Pagare IN FRAME F_Gar
   NO-ENABLE 2                                                          */
/* SETTINGS FOR RADIO-SET Garantias.Tipo_Garantia IN FRAME F_Gar
   5                                                                    */
/* SETTINGS FOR RADIO-SET Garantias.Tip_Credito IN FRAME F_Gar
   5                                                                    */
/* SETTINGS FOR FILL-IN Garantias.Val_Asegurado IN FRAME F_Gar
   6                                                                    */
/* SETTINGS FOR FILL-IN Garantias.Val_Bien IN FRAME F_Gar
   6 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Garantias.Val_Impuesto IN FRAME F_Gar
   6 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Garantias.Val_UltAvaluo IN FRAME F_Gar
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_NomAse IN FRAME F_Gar
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN W_NomDeu IN FRAME F_Gar
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN W_NomPro IN FRAME F_Gar
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN W_NomUsu IN FRAME F_Gar
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FRAME F_GarExi
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_GarExi 1 F_GarExi */
ASSIGN 
       FRAME F_GarExi:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_InfoCre
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_InfoCre:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_GarExi
/* Query rebuild information for BROWSE B_GarExi
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TGar NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_GarExi */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Gar
/* Query rebuild information for FRAME F_Gar
     _TblList          = "bdCentral.Garantias"
     _Query            is OPENED
*/  /* FRAME F_Gar */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Mantenimiento de Garantias Admisibles */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Mantenimiento de Garantias Admisibles */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Gar /* Cancelar */
DO:
  FIND Garantias WHERE ROWID(Garantias) EQ W_Puntero NO-ERROR.
  IF AVAILABLE Garantias THEN RUN Mostrar_Gar.
  ELSE DO:
    RUN Inicializar_Gar.
    DISABLE ALL WITH FRAME F_Gar.
    ENABLE {&List-1} WITH FRAME F_Gar.
  END.
  DISABLE {&List-5} WITH FRAME F_Gar.
  W_Nuevo = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ingresar wWin
ON CHOOSE OF Btn_ingresar IN FRAME F_Gar /* Ingresar */
DO:
  ENABLE ALL WITH FRAME F_Gar.
  DISABLE {&List-2} WITH FRAME F_Gar.
  W_Puntero = ROWID(Garantias).
  RUN Inicializar_Gar.
  W_Nuevo = YES.
  APPLY "entry" TO Cmb_Agencias.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_GarExi
&Scoped-define SELF-NAME Btn_OutExi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutExi wWin
ON CHOOSE OF Btn_OutExi IN FRAME F_GarExi /* Button 179 */
DO:
  HIDE FRAME F_GarExi.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Gar
&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Gar /* Salir */
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


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Gar /* Salvar */
DO:
  IF W_Nuevo THEN CREATE Garantias.
  ASSIGN FRAME F_Gar
         Garantias.Identificacion_Bien
         Garantias.Nom_Bien
         Garantias.Contabilizada
         Garantias.Nit
         Garantias.Tipo_Garantia
         Garantias.Estado
         Garantias.Tip_Credito
         Garantias.Cod_Credito
         Garantias.Num_Credito
         Garantias.Num_Solicitud
         Garantias.Pagare
         Garantias.Descripcion_Bien
         Garantias.Val_Bien
         Garantias.Nit_Aseguradora
         Garantias.Nro_Seguro
         Garantias.Fec_IniSeguro
         Garantias.Fec_FinSeguro
         Garantias.Val_Asegurado
         Garantias.Nom_Impuesto
         Garantias.Val_Impuesto
         Garantias.Fec_VctoImpuesto.
   ASSIGN Garantias.Agencia = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Gar,1,3))
          W_Nuevo = NO.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Gar /* i */
DO:
  VIEW FRAME F_InfoCre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_GarExi
&Scoped-define SELF-NAME BUTTON-178
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-178 wWin
ON CHOOSE OF BUTTON-178 IN FRAME F_GarExi /* Asumir Información Principal */
DO:
DO WITH FRAME F_Gar:
  IF W_Nuevo THEN DO:
     ASSIGN Garantias.Descripcion_Bien:SCREEN-VALUE = TGar.Descripcion_Bien
            Garantias.Contabilizada:SCREEN-VALUE    = STRING(TGar.Contabilizada)
            Garantias.Nom_Bien:SCREEN-VALUE         = TGar.Nom_Bien
            Garantias.Val_Bien:SCREEN-VALUE         = STRING(TGar.Val_Bien)
            Garantias.Nit:SCREEN-VALUE              = TGar.Nit
            Garantias.Nit_Aseguradora:SCREEN-VALUE  = TGar.Nit_Aseguradora
            Garantias.Nro_Seguro:SCREEN-VALUE       = TGar.Nro_Seguro
            Garantias.Fec_IniSeguro:SCREEN-VALUE    = STRING(TGar.Fec_IniSeguro)
            Garantias.Fec_FinSeguro:SCREEN-VALUE    = STRING(TGar.Fec_FinSeguro)
            Garantias.Val_Asegurado:SCREEN-VALUE    = STRING(TGar.Val_Asegurado)
            Garantias.Nom_Impuesto:SCREEN-VALUE     = TGar.Nom_Impuesto
            Garantias.Val_Impuesto:SCREEN-VALUE     = STRING(TGar.Val_Impuesto)
            Garantias.Fec_VctoImpuesto:SCREEN-VALUE = STRING(TGar.Fec_VctoImpuesto).
     FIND Clientes WHERE Clientes.Nit EQ TGar.Nit_Aseguradora NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN 
        ASSIGN W_NomAse:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     FIND Clientes WHERE Clientes.Nit EQ TGar.Nit NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN 
        ASSIGN W_NomDeu:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ENABLE {&List-6} WITH FRAME F_Gar.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfoCre
&Scoped-define SELF-NAME BUTTON-179
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-179 wWin
ON CHOOSE OF BUTTON-179 IN FRAME F_InfoCre /* Button 179 */
DO:
  HIDE FRAME F_InfoCre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Gar
&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON CHOOSE OF BUTTON-4 IN FRAME F_Gar /* Button 4 */
DO:
  RUN C-Garantias.r (OUTPUT g_Nit, OUTPUT g_id, OUTPUT g_tp, OUTPUT g_Cd,
                    OUTPUT g_Nc, OUTPUT g_NS).
  FIND Garantias WHERE
       Garantias.Identificacion_Bien EQ g_id AND
       Garantias.Tipo_Garantia       EQ g_tp AND
       Garantias.Cod_Credito         EQ g_cd AND
       Garantias.Num_Credito         EQ g_nc AND
       Garantias.Num_Solicitud       EQ g_ns AND
       Garantias.Nit                 EQ G_nit NO-ERROR.
  IF AVAILABLE Garantias THEN DO:
    RUN Mostrar_Gar.
    ENABLE {&List-6} WITH FRAME F_Gar.
    DISABLE {&List-5} WITH FRAME F_Gar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_FinSeguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_FinSeguro wWin
ON LEAVE OF Garantias.Fec_FinSeguro IN FRAME F_Gar /* Vencimiento Seguro */
DO:
  IF DATE(SELF:SCREEN-VALUE) LT DATE(Garantias.Fec_IniSeguro:SCREEN-VALUE) AND 
     DATE(Garantias.Fec_IniSeguro:SCREEN-VALUE) NE ? THEN DO:
     MESSAGE "La fecha de vencimiento del seguro no puede ser" SKIP
             "menor a la fecha de inicio de seguro. Rectifique!" VIEW-AS ALERT-BOX WARNING.
     SELF:SCREEN-VALUE = STRING(DATE(Garantias.Fec_IniSeguro:SCREEN-VALUE) + 365).
     APPLY "entry" TO Garantias.Fec_FinSeguro.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_ProxAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_ProxAvaluo wWin
ON LEAVE OF Garantias.Fec_ProxAvaluo IN FRAME F_Gar /* Fec.Próx.Avaluo */
DO:
  IF DATE(SELF:SCREEN-VALUE) LT DATE(Garantias.Fec_UltAvaluo:SCREEN-VALUE) AND 
     DATE(Garantias.Fec_UltAvaluo:SCREEN-VALUE) NE ? THEN DO:
     MESSAGE "La fecha de Próximo Avaluo no puede ser menor" SKIP
             "a la fecha de último avaluo. Rectifique!" VIEW-AS ALERT-BOX WARNING.
     SELF:SCREEN-VALUE = STRING(DATE(Garantias.Fec_UltAvaluo:SCREEN-VALUE) + 365).
     APPLY "entry" TO Garantias.Fec_ProxAvaluo.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Identificacion_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Identificacion_Bien wWin
ON LEAVE OF Garantias.Identificacion_Bien IN FRAME F_Gar /* Id */
DO:
  DEFINE VAR W_EXiExi AS LOGICAL.
  IF W_nuevo THEN DO:
     FOR EACH Garantias WHERE Garantias.Identificacion_Bien EQ SELF:SCREEN-VALUE BREAK BY Garantias.Identificacion_Bien:
        IF FIRST-OF(Garantias.Identificacion_Bien) THEN W_ExiExi = YES.
        CREATE TGar.
        BUFFER-COPY Garantias TO TGar.
     END.
     OPEN QUERY B_GarExi FOR EACH TGar WHERE TGar.Identificacion_Bien EQ SELF:SCREEN-VALUE NO-LOCK INDEXED-REPOSITION.
     IF W_ExiExi THEN DO:
       VIEW FRAME F_GarExi.   
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nit wWin
ON LEAVE OF Garantias.Nit IN FRAME F_Gar /* Nit Deudor */
DO:
  IF SELF:SCREEN-VALUE EQ "" THEN DO:
     RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
     ASSIGN W_NomDeu:SCREEN-VALUE = P_Nombre + " " + P_Apellido
            SELF:SCREEN-VALUE   = P_Nit.
     FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  ELSE DO:
    FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN 
       W_NomDeu:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE DO:
       RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
       ASSIGN W_NomDeu:SCREEN-VALUE = P_Nombre + " " + P_Apellido
              SELF:SCREEN-VALUE   = P_Nit.
       FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
    END.
  END.
  IF AVAILABLE Clientes THEN DO:
     IF Clientes.Tipo_Vinculo GT 2 THEN DO:
        MESSAGE "La persona o Empresa no esta matriculada" SKIP
                "Como cliente de la cooperativa." SKIP
                "No se podrá crear una Garantia." SKIP
       VIEW-AS ALERT-BOX WARNING.
       ASSIGN Garantias.Nit:SCREEN-VALUE = ""
              W_NomDeu:SCREEN-VALUE = "".
       APPLY "entry" TO Cmb_Agencias.
       RETURN NO-APPLY.
     END.
  END.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nit wWin
ON MOUSE-SELECT-DBLCLICK OF Garantias.Nit IN FRAME F_Gar /* Nit Deudor */
DO:
 RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
 ASSIGN W_NomDeu:SCREEN-VALUE = P_Nombre + " " + P_Apellido
        SELF:SCREEN-VALUE   = P_Nit.
 FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Nit_Aseguradora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nit_Aseguradora wWin
ON LEAVE OF Garantias.Nit_Aseguradora IN FRAME F_Gar /* Nit Aseuguradora */
DO:
  IF SELF:SCREEN-VALUE EQ "" THEN DO:
     RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
     ASSIGN W_NomAse:SCREEN-VALUE = P_Nombre + " " + P_Apellido
            SELF:SCREEN-VALUE   = P_Nit.
     FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  ELSE DO:
    FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN 
       W_NomDeu:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE DO:
       RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
       ASSIGN W_NomDeu:SCREEN-VALUE = P_Nombre + " " + P_Apellido
              SELF:SCREEN-VALUE   = P_Nit.
       FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
    END.
  END.
  IF AVAILABLE Clientes THEN DO:
     IF Clientes.Tipo_Vinculo GT 2 THEN DO:
        MESSAGE "La persona o Empresa no esta matriculada" SKIP
                "Como cliente de la cooperativa." SKIP
                "No se podrá crear una Garantia." SKIP
       VIEW-AS ALERT-BOX WARNING.
       ASSIGN Garantias.Nit_Aseguradora:SCREEN-VALUE = ""
              W_NomAse:SCREEN-VALUE = "".
       APPLY "entry" TO Cmb_Agencias.
       RETURN NO-APPLY.
     END.
     ASSIGN Garantias.Nit_Aseguradora:SCREEN-VALUE = P_Nit
            W_NomAse:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nit_Aseguradora wWin
ON MOUSE-SELECT-DBLCLICK OF Garantias.Nit_Aseguradora IN FRAME F_Gar /* Nit Aseuguradora */
DO:
 RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
 ASSIGN W_NomAse:SCREEN-VALUE = P_Nombre + " " + P_Apellido
        SELF:SCREEN-VALUE   = P_Nit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Num_Credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Num_Credito wWin
ON LEAVE OF Garantias.Num_Credito IN FRAME F_Gar /* Nro.Crédito */
DO:
DO WITH FRAME F_Gar:
  FIND Creditos WHERE
       Creditos.Num_Credito EQ INTEGER(SELF:SCREEN-VALUE) AND
       Creditos.Nit         EQ Garantias.Nit:SCREEN-VALUE AND
       Creditos.Tip_Credito EQ INTEGER(Garantias.Tip_Credito:SCREEN-VALUE) AND
       Creditos.Cod_Credito EQ INTEGER(Garantias.Cod_Credito:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
    RUN Llenar_InfoCre.
    Garantias.Num_Solicitud:SCREEN-VALUE = STRING(Creditos.Num_Solicitud).
  END.
  ELSE DO:
    MESSAGE "No existe ningún crédito que se identifique" SKIP
            "con este número. para este nit y producto" SKIP(1)
            "digite de nuevo el número del crédito" VIEW-AS ALERT-BOX.
    S_InfoCre:LIST-ITEMS IN FRAME F_InfoCre = "".
    APPLY "entry" TO Garantias.Cod_Credito.
    RETURN NO-APPLY.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_GarExi
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

  {&OPEN-QUERY-F_Gar}
  GET FIRST F_Gar.
  DISPLAY Cmb_Agencias W_NomDeu W_NomPro W_NomAse W_NomUsu 
      WITH FRAME F_Gar IN WINDOW wWin.
  IF AVAILABLE Garantias THEN 
    DISPLAY Garantias.Estado Garantias.Tipo_Garantia Garantias.Identificacion_Bien 
          Garantias.Nom_Bien Garantias.Nit Garantias.Fec_Creacion 
          Garantias.Tip_Credito Garantias.Cod_Credito Garantias.Fec_Retiro 
          Garantias.Num_Credito Garantias.Pagare Garantias.Contabilizada 
          Garantias.Num_Solicitud Garantias.Descripcion_Bien Garantias.Val_Bien 
          Garantias.Nit_Aseguradora Garantias.Nro_Seguro Garantias.Val_Asegurado 
          Garantias.Fec_IniSeguro Garantias.Fec_FinSeguro Garantias.Nom_Impuesto 
          Garantias.Val_Impuesto Garantias.Fec_VctoImpuesto 
          Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo 
          Garantias.Val_UltAvaluo 
      WITH FRAME F_Gar IN WINDOW wWin.
  ENABLE BUTTON-2 Cmb_Agencias Garantias.Tipo_Garantia BUTTON-3 
         Garantias.Identificacion_Bien Garantias.Nom_Bien Garantias.Nit 
         BUTTON-4 Garantias.Tip_Credito Garantias.Cod_Credito 
         Garantias.Num_Credito BUTTON-1 Garantias.Contabilizada 
         Garantias.Descripcion_Bien Btn_Salvar Btn_Deshacer Garantias.Val_Bien 
         Btn_ingresar Garantias.Nit_Aseguradora Garantias.Nro_Seguro 
         Garantias.Val_Asegurado Btn_Cancelar Garantias.Fec_IniSeguro 
         Garantias.Fec_FinSeguro Btn_Descargar Garantias.Nom_Impuesto 
         Garantias.Val_Impuesto Garantias.Fec_VctoImpuesto Btn_Salir 
         Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo 
         Garantias.Val_UltAvaluo BUTTON-11 RECT-1 RECT-2 RECT-297 RECT-3 RECT-4 
      WITH FRAME F_Gar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Gar}
  ENABLE B_GarExi Btn_OutExi BUTTON-178 
      WITH FRAME F_GarExi IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_GarExi}
  DISPLAY S_InfoCre 
      WITH FRAME F_InfoCre IN WINDOW wWin.
  ENABLE S_InfoCre BUTTON-179 
      WITH FRAME F_InfoCre IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_InfoCre}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Gar wWin 
PROCEDURE Inicializar_Gar :
DO WITH FRAME F_Gar:
  FIND Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
  IF AVAILABLE Agencias THEN
     Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
  ELSE MESSAGE "Error en la Agencia de la Garantia" VIEW-AS ALERT-BOX TITLE "Llenando CMB_Agencias".
  
  ASSIGN Garantias.Tipo_Garantia:SCREEN-VALUE       = "1"
         Garantias.Identificacion_Bien:SCREEN-VALUE = ""
         Garantias.Nom_Bien:SCREEN-VALUE            = ""
         Garantias.Nit:SCREEN-VALUE                 = ""
         Garantias.Num_Solicitud:SCREEN-VALUE = "0"
         Garantias.Num_Credito:SCREEN-VALUE   = "0"
         Garantias.Pagare:SCREEN-VALUE        = ""
         Garantias.Tip_Credito:SCREEN-VALUE   = "1"
         Garantias.Cod_Credito:SCREEN-VALUE   = "0"
         Garantias.Fec_Creacion:SCREEN-VALUE  = ""
         Garantias.Fec_Retiro:SCREEN-VALUE    = ""
         Garantias.Descripcion_Bien:SCREEN-VALUE = ""
         Garantias.Val_Bien:SCREEN-VALUE      = "0"
         Garantias.Estado:SCREEN-VALUE        = "1"
         Garantias.Contabilizada:SCREEN-VALUE = "NO"
         Garantias.Nit_Aseguradora:SCREEN-VALUE = ""
         Garantias.Nro_Seguro:SCREEN-VALUE       = ""
         Garantias.Val_Asegurado:SCREEN-VALUE    = "0"
         Garantias.Fec_IniSeguro:SCREEN-VALUE    = ""
         Garantias.Fec_FinSeguro:SCREEN-VALUE    = ""
         Garantias.Nom_Impuesto:SCREEN-VALUE     = ""
         Garantias.Val_Impuesto:SCREEN-VALUE     = "0"
         Garantias.Fec_VctoImpuesto:SCREEN-VALUE = ""
         W_NomAse:SCREEN-VALUE = ""
         W_NomDeu:SCREEN-VALUE = ""
         W_NomPro:SCREEN-VALUE = "".
     
   FIND Usuarios WHERE Usuarios.Usuario EQ Garantias.Usuario NO-LOCK NO-ERROR.
   IF AVAILABLE Usuarios THEN W_NomUsu:SCREEN-VALUE = Usuarios.Nombre.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FOR EACH Agencias NO-LOCK:
    W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Gar.
  END.
  RUN SUPER.
  
  FIND FIRST Garantias WHERE 
             Garantias.Agencia EQ W_Agencia AND
             Garantias.Num_Credito NE 0 NO-ERROR.
  IF AVAILABLE Garantias THEN DO: 
    RUN Mostrar_Gar.
    DISABLE {&List-5} WITH FRAME F_Gar.
  END.
  ELSE DO:
    FIND FIRST Garantias WHERE Garantias.Num_Credito NE 0 NO-ERROR.
    IF AVAILABLE Garantias THEN DO: 
      RUN Mostrar_Gar.
      DISABLE {&List-5} WITH FRAME F_Gar.
    END.
    ELSE DO:
      RUN Inicializar_Gar.
      DISABLE ALL WITH FRAME F_Gar.
      ENABLE {&List-1} WITH FRAME F_Gar.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoCre wWin 
PROCEDURE Llenar_InfoCre :
DEFINE VAR W_Per AS CHARACTER FORMAT "X(15)".
DEFINE VAR W_Int AS CHARACTER FORMAT "X(15)".
DEFINE VAR W_Pag AS CHARACTER FORMAT "X(15)".

DO WITH FRAME F_InfoCre:
   IF Creditos.FOR_Interes EQ 1 THEN W_Int = "Vencido".
   ELSE W_Int = "Anticipado".
   CASE Creditos.Per_Pago:
     WHEN 1 THEN W_Per = "Semanal".
     WHEN 3 THEN W_Per = "Quicenal".
     WHEN 4 THEN W_Per = "Mensual".
   END CASE.
   CASE Creditos.FOR_Pago:
     WHEN 1 THEN W_Pag = "Caja".
     WHEN 2 THEN W_Pag = "Nómina".
     WHEN 3 THEN W_Pag = "DB.Automatico".
   END CASE.
   W_Ok = S_InfoCre:ADD-LAST("Monto         : " + STRING(Creditos.Monto)).
   W_Ok = S_InfoCre:ADD-LAST("Tasa          : " + STRING(Creditos.Tasa)).
   W_Ok = S_InfoCre:ADD-LAST("Plazo         : " + STRING(Creditos.Plazo)).
   W_Ok = S_InfoCre:ADD-LAST("Periodo Cuota : " + W_Per).
   W_Ok = S_InfoCre:ADD-LAST("Liq.Interes   : " + W_Int).
   W_Ok = S_InfoCre:ADD-LAST("Cuota         : " + STRING(Creditos.Cuota)).
   W_Ok = S_InfoCre:ADD-LAST("Forma de Pago : " + W_Pag).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Gar wWin 
PROCEDURE Mostrar_Gar :
DO WITH FRAME F_Gar:
  FIND Agencias WHERE Agencias.Agencia EQ Garantias.Agencia NO-LOCK NO-ERROR.
  IF AVAILABLE Agencias THEN
     Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
  ELSE MESSAGE "Error en la Agencia de la Garantia" VIEW-AS ALERT-BOX TITLE "Llenando CMB_Agencias".
  
  ASSIGN Garantias.Tipo_Garantia:SCREEN-VALUE = STRING(Garantias.Tipo_Garantia)
         Garantias.Identificacion_Bien:SCREEN-VALUE = Garantias.Identificacion_Bien
         Garantias.Nom_Bien:SCREEN-VALUE = Garantias.Nom_Bien
         Garantias.Nit:SCREEN-VALUE      = Garantias.Nit
         Garantias.Num_Solicitud:SCREEN-VALUE = STRING(Garantias.Num_Solicitud)
         Garantias.Num_Credito:SCREEN-VALUE   = STRING(Garantias.Num_Credito)
         Garantias.Pagare:SCREEN-VALUE        = Garantias.Pagare
         Garantias.Tip_Credito:SCREEN-VALUE   = STRING(Garantias.Tip_Credito)
         Garantias.Cod_Credito:SCREEN-VALUE   = STRING(Garantias.Cod_Credito)
         Garantias.Fec_Creacion:SCREEN-VALUE  = STRING(Garantias.Fec_Creacion)
         Garantias.Fec_Retiro:SCREEN-VALUE    = STRING(Garantias.Fec_Retiro)
         Garantias.Descripcion_Bien:SCREEN-VALUE = Garantias.Descripcion_Bien
         Garantias.Val_Bien:SCREEN-VALUE      = STRING(Garantias.Val_Bien)
         Garantias.Estado:SCREEN-VALUE        = STRING(Garantias.Estado)
         Garantias.Contabilizada:SCREEN-VALUE = STRING(Garantias.Contabilizada)
         Garantias.Nit_Aseguradora:SCREEN-VALUE = Garantias.Nit_Aseguradora
         Garantias.Nro_Seguro:SCREEN-VALUE    = Garantias.Nro_Seguro
         Garantias.Val_Asegurado:SCREEN-VALUE = STRING(Garantias.Val_Asegurado)
         Garantias.Fec_IniSeguro:SCREEN-VALUE = STRING(Garantias.Fec_IniSeguro)
         Garantias.Fec_FinSeguro:SCREEN-VALUE = STRING(Garantias.Fec_FinSeguro)
         Garantias.Nom_Impuesto:SCREEN-VALUE  = Garantias.Nom_Impuesto
         Garantias.Val_Impuesto:SCREEN-VALUE  = STRING(Garantias.Val_Impuesto)
         Garantias.Fec_VctoImpuesto:SCREEN-VALUE = STRING(Garantias.Fec_VctoImpuesto).
   
   FIND Clientes WHERE Clientes.Nit EQ Garantias.Nit_Aseguradora NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN
      W_NomAse:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
   ELSE
      W_NomAse:SCREEN-VALUE = "".
   
   FIND Clientes WHERE Clientes.Nit EQ Garantias.Nit NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN 
      W_NomDeu:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
   ELSE
      W_NomDeu:SCREEN-VALUE = "Inconsistencia en Cliente".
      
   FIND Pro_Creditos WHERE
        Pro_Creditos.Cod_Credito EQ Garantias.Cod_Credito AND
        Pro_Creditos.Tip_Credito EQ Garantias.Tip_Credito NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Creditos THEN W_NomPro:SCREEN-VALUE = Pro_Creditos.Nom_Producto.
   ELSE W_NomPro:SCREEN-VALUE = "Producto de Creditos No dsponible".
     
   FIND Usuarios WHERE Usuarios.Usuario EQ Garantias.Usuario NO-LOCK NO-ERROR.
   IF AVAILABLE Usuarios THEN W_NomUsu:SCREEN-VALUE = Usuarios.Nombre.
         
  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

