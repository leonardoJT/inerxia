&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR P_Nit AS CHARACTER.
DEFINE VAR P_Nombre AS CHARACTER.
DEFINE VAR P_Apellido AS CHARACTER FORMAT "X(30)".
DEFINE VAR P_AgeCli AS INTEGER.
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_StrCam AS CHARACTER FORMAT "X(300)".
DEFINE VAR W_AccAdm AS CHARACTER FORMAT "X". /*M-modifica,C-Crea*/
DEFINE VAR W_AccNoAdm AS CHARACTER FORMAT "X". /*M-modifica,C-Crea*/
DEFINE VAR W_Puntero AS ROWID.
DEFINE VAR A_Age AS INTEGER.
DEFINE VAR A_Pro AS INTEGER.
DEFINE VAR A_NitW AS CHARACTER.
DEFINE VAR A_Cue AS CHARACTER.

DEFINE TEMP-TABLE TmpCre
    FIELD AgeCre AS INTEGER
    FIELD TipCre AS INTEGER
    FIELD CodCre LIKE Creditos.Cod_Credito
    FIELD NumCre LIKE Creditos.Num_Credito
    FIELD NumSol LIKE Creditos.Num_Solicitud
    FIELD Pagare LIKE Creditos.Pagare
    FIELD Monto  LIKE Creditos.Monto

    /* oakley */

    FIELD SdoCap LIKE Creditos.Sdo_Capital
    FIELD NitCre LIKE Clientes.Nit
    FIELD ModOk  AS LOGICAL INITIAL YES.

DEFINE TEMP-TABLE TmpGar
    FIELD AgeGar LIKE Creditos.Agencia
    FIELD TipGar LIKE Garantias.Tipo_Garantia
    FIELD IdeGar LIKE Garantias.Identificacion_Bien
    FIELD NomGar LIKE Garantias.Nom_Bien
    FIELD ValGar LIKE Garantias.Val_Bien
    FIELD TipCre LIKE Creditos.Tip_Credito
    FIELD CodCre LIKE Creditos.Cod_Credito
    FIELD NumCre LIKE Creditos.Num_Credito
    FIELD NumSol LIKE Creditos.Num_Solicitud
    FIELD NomTip AS CHARACTER FORMAT "X(15)"
    FIELD EstGar LIKE Garantias.Estado.

DEFINE TEMP-TABLE TmpCod
    FIELD AgeCod LIKE Creditos.Agencia
    FIELD ClaPro LIKE Relaciones.Clase_Producto
    FIELD CodPro LIKE Relaciones.Cod_Producto
    FIELD NumCre LIKE Relaciones.Cuenta
    FIELD CodRel LIKE Relaciones.Cod_relacion
    FIELD NitCli LIKE Relaciones.Nit
    FIELD NitRel LIKE Relaciones.Nit_relacion
    FIELD NomCod AS CHARACTER FORMAT "X(50)"
    FIELD DesCod LIKE Relaciones.Descripcion
    FIELD EstGar LIKE Relaciones.Estado
    FIELD TelRes LIKE Clientes.Tel_Residencia
    FIELD TelCom LIKE Clientes.Tel_Residencia.

DEF VAR GNIT AS CHAR NO-UNDO.
DEF VAR GHPARENT AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FGar
&Scoped-define BROWSE-NAME BCod

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TmpCod TmpCre TmpGar

/* Definitions for BROWSE BCod                                          */
&Scoped-define FIELDS-IN-QUERY-BCod TmpCod.CodPro TmpCod.NumCre TmpCod.NitRel TmpCod.NomCod   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BCod   
&Scoped-define SELF-NAME BCod
&Scoped-define QUERY-STRING-BCod FOR EACH TmpCod
&Scoped-define OPEN-QUERY-BCod OPEN QUERY {&SELF-NAME} FOR EACH TmpCod.
&Scoped-define TABLES-IN-QUERY-BCod TmpCod
&Scoped-define FIRST-TABLE-IN-QUERY-BCod TmpCod


/* Definitions for BROWSE BCre                                          */
&Scoped-define FIELDS-IN-QUERY-BCre TmpCre.TipCre TmpCre.CodCre TmpCre.NumCre TmpCre.NumSol TmpCre.Pagare TmpCre.Monto TmpCre.SdoCap   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BCre   
&Scoped-define SELF-NAME BCre
&Scoped-define QUERY-STRING-BCre FOR EACH TmpCre
&Scoped-define OPEN-QUERY-BCre OPEN QUERY {&SELF-NAME} FOR EACH TmpCre.
&Scoped-define TABLES-IN-QUERY-BCre TmpCre
&Scoped-define FIRST-TABLE-IN-QUERY-BCre TmpCre


/* Definitions for BROWSE BGar                                          */
&Scoped-define FIELDS-IN-QUERY-BGar TmpGar.NomTip TmpGar.IdeGar TmpGar.NomGar TmpGar.ValGar   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BGar   
&Scoped-define SELF-NAME BGar
&Scoped-define QUERY-STRING-BGar FOR EACH TmpGar
&Scoped-define OPEN-QUERY-BGar OPEN QUERY {&SELF-NAME} FOR EACH TmpGar.
&Scoped-define TABLES-IN-QUERY-BGar TmpGar
&Scoped-define FIRST-TABLE-IN-QUERY-BGar TmpGar


/* Definitions for FRAME FGar                                           */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FGar ~
    ~{&OPEN-QUERY-BCod}~
    ~{&OPEN-QUERY-BCre}~
    ~{&OPEN-QUERY-BGar}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_NitCli Btn_ModNoAdm Btn_NueNoAdm BCod ~
BUTTON-168 BUTTON-170 BCre Btn_NueGar Btn_ModGar BGar BtnDone ~
R_TipoGarantias BUTTON-169 RECT-307 
&Scoped-Define DISPLAYED-OBJECTS W_NitCli W_NomCli R_TipoGarantias 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Garantias.Contabilizada Garantias.Id_Interna ~
Cmb_AhorrosNA Garantias.Nit_Aseguradora Garantias.Nro_Seguro ~
Garantias.Val_Asegurado Garantias.Fec_IniSeguro Garantias.Fec_FinSeguro ~
Garantias.Val_UltAvaluo Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo ~
Garantias.Nom_Impuesto Garantias.Fec_VctoImpuesto Garantias.Val_Impuesto 
&Scoped-define List-2 Garantias.Id_Interna Cmb_AhorrosNA ~
Garantias.Nit_Aseguradora Garantias.Nro_Seguro Garantias.Val_Asegurado ~
Garantias.Fec_IniSeguro Garantias.Fec_FinSeguro Garantias.Val_UltAvaluo ~
Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo Garantias.Nom_Impuesto ~
Garantias.Fec_VctoImpuesto Garantias.Val_Impuesto 
&Scoped-define List-3 Garantias.Nit_Aseguradora Garantias.Nro_Seguro ~
Garantias.Val_Asegurado Garantias.Fec_IniSeguro Garantias.Fec_FinSeguro ~
Garantias.Val_UltAvaluo Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo ~
Garantias.Nom_Impuesto Garantias.Fec_VctoImpuesto Garantias.Val_Impuesto 
&Scoped-define List-5 Relaciones.Estado Relaciones.Descripcion 
&Scoped-define List-6 Relaciones.Nit_relacion Relaciones.Estado ~
Relaciones.Descripcion 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 11.57 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_ModGar 
     LABEL "Modificar Garantia" 
     SIZE 18 BY 1.62.

DEFINE BUTTON Btn_ModNoAdm 
     LABEL "Modificar Codeudor" 
     SIZE 18 BY 1.62.

DEFINE BUTTON Btn_NueGar 
     LABEL "Nueva Garantia" 
     SIZE 18 BY 1.62.

DEFINE BUTTON Btn_NueNoAdm 
     LABEL "Nuevo Codeudor" 
     SIZE 18 BY 1.62.

DEFINE BUTTON BUTTON-168 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 168" 
     SIZE 14 BY 1.62.

DEFINE BUTTON BUTTON-169 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 169" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-170 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 170" 
     SIZE 14 BY 1.62.

DEFINE VARIABLE W_NitCli AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit Cliente" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCli AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_TipoGarantias AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Garantias Activas", 1,
"Garantias Inactivas", 2
     SIZE 39 BY .81
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-307
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 1.35.

DEFINE BUTTON Btn_SalirNoAdm 
     LABEL "Salir" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_SaveCod 
     LABEL "Salvar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE W_NomCodeudor AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomUsuario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE E_Code AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 77 BY 8.62
     BGCOLOR 15 FONT 3 NO-UNDO.

DEFINE BUTTON Btn_OutAdm 
     LABEL "Regresar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_Save 
     LABEL "Salvar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE Cmb_Agencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_AhorrosNA AS CHARACTER FORMAT "X(35)":U 
     LABEL "Productos de Ahorro" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 32.72 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomAse AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomUsu AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61.14 BY 1.35.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY .92.

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 3.23.

DEFINE RECTANGLE RECT-305
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 3.23.

DEFINE RECTANGLE RECT-306
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 3.23.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BCod FOR 
      TmpCod SCROLLING.

DEFINE QUERY BCre FOR 
      TmpCre SCROLLING.

DEFINE QUERY BGar FOR 
      TmpGar SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BCod wWin _FREEFORM
  QUERY BCod DISPLAY
      TmpCod.CodPro COLUMN-LABEL "Cod.Producto"
 TmpCod.NumCre COLUMN-LABEL "Cuenta.Credito"
 TmpCod.NitRel COLUMN-LABEL "Nit"
 TmpCod.NomCod COLUMN-LABEL "Descripción"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 78 BY 4.5
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BCre wWin _FREEFORM
  QUERY BCre DISPLAY
      TmpCre.TipCre COLUMN-LABEL "TipoCredito"
 TmpCre.CodCre COLUMN-LABEL "Cod.Producto"
 TmpCre.NumCre COLUMN-LABEL "Num.Crédito"
 TmpCre.NumSol COLUMN-LABEL "Num.Solicitud"
 TmpCre.Pagare COLUMN-LABEL "Pagare"
 TmpCre.Monto  COLUMN-LABEL "MontoInicial"
 TmpCre.SdoCap COLUMN-LABEL "Saldo Capital"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 78 BY 3.23
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .46 FIT-LAST-COLUMN.

DEFINE BROWSE BGar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BGar wWin _FREEFORM
  QUERY BGar DISPLAY
      TmpGar.NomTip  COLUMN-LABEL "Tipo Garantia"
TmpGar.IdeGar  COLUMN-LABEL "Identificación"
TmpGar.NomGar  COLUMN-LABEL "Descripcion de la Garantia"
TmpGar.ValGar  COLUMN-LABEL "Valor Garantia"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 78 BY 4.31
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FGar
     W_NitCli AT ROW 1.54 COL 16 COLON-ALIGNED
     Btn_ModNoAdm AT ROW 16.08 COL 92
     Btn_NueNoAdm AT ROW 14.46 COL 92
     BCod AT ROW 13.92 COL 8
     BUTTON-168 AT ROW 1.54 COL 93.43
     W_NomCli AT ROW 1.54 COL 29 COLON-ALIGNED NO-LABEL
     BUTTON-170 AT ROW 3.96 COL 93.43
     BCre AT ROW 4.77 COL 8
     Btn_NueGar AT ROW 9.35 COL 92
     Btn_ModGar AT ROW 10.96 COL 92
     BGar AT ROW 8.81 COL 8
     BtnDone AT ROW 18.23 COL 95
     R_TipoGarantias AT ROW 19.58 COL 9 NO-LABEL
     BUTTON-169 AT ROW 20.08 COL 98.57
     "Garantias del Crédito" VIEW-AS TEXT
          SIZE 30 BY .81 AT ROW 8 COL 8
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Codeudores del Crédito" VIEW-AS TEXT
          SIZE 47 BY .81 AT ROW 13.12 COL 8
          FGCOLOR 7 FONT 5
     " Créditos Vigentes del Cliente" VIEW-AS TEXT
          SIZE 31 BY .81 AT ROW 3.96 COL 8
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Mostrar" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 19.04 COL 10
          FGCOLOR 7 FONT 5
     RECT-307 AT ROW 19.31 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.29 BY 21.08
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F_AdmGar
     Garantias.Descripcion_Bien2 AT ROW 7.46 COL 6 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 39 BY 1.62
          BGCOLOR 15 
     Garantias.Fec_Creacion AT ROW 1.27 COL 72 COLON-ALIGNED
          LABEL "Fecha Creación"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Tipo_Garantia AT ROW 2 COL 1.43 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Propiedad", 1,
"Prenda", 2,
"Inversión", 3,
"Cdat-Contrac No-Ad", 4,
"Otras No-Ad", 5
          SIZE 59.86 BY .62
          FONT 4
     Garantias.Fec_Retiro AT ROW 2.08 COL 72 COLON-ALIGNED
          LABEL "Fecha de Retiro"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Cmb_Agencia AT ROW 3.15 COL 16 COLON-ALIGNED
     Garantias.Aprobada AT ROW 3.15 COL 50
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .77
     Garantias.Estado AT ROW 3.31 COL 67 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 18 BY .65
     Garantias.Identificacion_Bien AT ROW 4.04 COL 16 COLON-ALIGNED
          LABEL "Identificación del Bien"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Garantias.Nom_Bien AT ROW 4.04 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 54.86 BY .81
          BGCOLOR 15 
     Garantias.Val_Bien AT ROW 4.92 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Garantias.Contabilizada AT ROW 5.04 COL 8
          LABEL "Contabilizar?"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .77
     Garantias.Descripcion_Bien AT ROW 7.46 COL 46 NO-LABEL
          VIEW-AS EDITOR MAX-CHARS 500 SCROLLBAR-VERTICAL LARGE
          SIZE 40 BY 1.58
          BGCOLOR 15 FONT 4
     Garantias.Id_Interna AT ROW 5.85 COL 8
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .65
     Cmb_AhorrosNA AT ROW 5.85 COL 44 COLON-ALIGNED
     Garantias.Nit_Aseguradora AT ROW 9.92 COL 17 COLON-ALIGNED
          LABEL "Nit Aseguradora"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     W_NomAse AT ROW 9.92 COL 29 COLON-ALIGNED NO-LABEL
     Garantias.Nro_Seguro AT ROW 10.77 COL 17 COLON-ALIGNED
          LABEL "Número Seguro"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Garantias.Val_Asegurado AT ROW 11.62 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Garantias.Fec_IniSeguro AT ROW 10.77 COL 70.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Garantias.Fec_FinSeguro AT ROW 11.65 COL 70.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Garantias.Val_UltAvaluo AT ROW 13.58 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Garantias.Fec_ProxAvaluo AT ROW 14.5 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Garantias.Fec_UltAvaluo AT ROW 15.38 COL 24 COLON-ALIGNED
          LABEL "Fecha Último Avaluo"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.62
         SIZE 87 BY 18.85
         BGCOLOR 17 FGCOLOR 0 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_AdmGar
     Garantias.Nom_Impuesto AT ROW 13.5 COL 59 COLON-ALIGNED
          LABEL "Nombre Impuesto"
          VIEW-AS FILL-IN 
          SIZE 23 BY .81
          BGCOLOR 15 
     Garantias.Fec_VctoImpuesto AT ROW 14.42 COL 59 COLON-ALIGNED
          LABEL "Fec. Vencimiento"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Garantias.Val_Impuesto AT ROW 15.35 COL 59 COLON-ALIGNED
          LABEL "Valor del Impuesto"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Garantias.Usuario AT ROW 16.62 COL 20 COLON-ALIGNED
          LABEL "Usuario que actualiza"
          VIEW-AS FILL-IN 
          SIZE 4.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NomUsu AT ROW 16.62 COL 24 COLON-ALIGNED NO-LABEL
     Btn_Save AT ROW 17.69 COL 54
     Btn_OutAdm AT ROW 17.69 COL 70
     "  Tipo de Garantia" VIEW-AS TEXT
          SIZE 16 BY .54 AT ROW 1.27 COL 3.43
          FGCOLOR 7 FONT 5
     "  Estado" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 2.85 COL 65
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Información del Bien" VIEW-AS TEXT
          SIZE 20 BY .5 AT ROW 6.77 COL 6
          FGCOLOR 7 FONT 5
     "Concepto del Abogado" VIEW-AS TEXT
          SIZE 21 BY .65 AT ROW 6.77 COL 46
          FGCOLOR 7 FONT 5
     " Información del Seguro" VIEW-AS TEXT
          SIZE 21 BY .81 AT ROW 9.12 COL 7
          FGCOLOR 7 FONT 5
     " Información Impuestos" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 12.69 COL 44
          FGCOLOR 7 FONT 5
     " Información del Avaluo" VIEW-AS TEXT
          SIZE 21 BY .65 AT ROW 12.85 COL 7
          FGCOLOR 7 FONT 5
     RECT-302 AT ROW 1.54 COL 1
     RECT-303 AT ROW 3.15 COL 65
     RECT-304 AT ROW 9.38 COL 6
     RECT-305 AT ROW 13.12 COL 42
     RECT-306 AT ROW 13.12 COL 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.62
         SIZE 87 BY 18.85
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Administrar Garantias".

DEFINE FRAME F_AdmCod
     Relaciones.Nit_relacion AT ROW 1.54 COL 12 COLON-ALIGNED
          LABEL "Nit Codeudor"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     W_NomCodeudor AT ROW 1.54 COL 24 COLON-ALIGNED NO-LABEL
     Relaciones.Estado AT ROW 2.77 COL 14.14 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 21 BY .54
     Relaciones.Descripcion AT ROW 3.69 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 67 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Relaciones.Usuario AT ROW 5.04 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NomUsuario AT ROW 5.04 COL 16 COLON-ALIGNED NO-LABEL
     E_Code AT ROW 7.73 COL 6 NO-LABEL
     Btn_SaveCod AT ROW 17.42 COL 52
     Btn_SalirNoAdm AT ROW 17.42 COL 69
     "Este codeudor a demás coadeuda los siguientes créditos" VIEW-AS TEXT
          SIZE 50 BY .81 AT ROW 6.92 COL 6
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.62
         SIZE 87 BY 18.85
         BGCOLOR 17 FONT 4
         TITLE "Administración de Garantias No Admisibles".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Administración de Garantias"
         HEIGHT             = 21.08
         WIDTH              = 112.43
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
ASSIGN FRAME F_AdmCod:FRAME = FRAME FGar:HANDLE
       FRAME F_AdmGar:FRAME = FRAME FGar:HANDLE.

/* SETTINGS FOR FRAME FGar
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_AdmGar:MOVE-AFTER-TAB-ITEM (BUTTON-169:HANDLE IN FRAME FGar)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BCod Btn_NueNoAdm FGar */
/* BROWSE-TAB BCre BUTTON-170 FGar */
/* BROWSE-TAB BGar Btn_ModGar FGar */
/* SETTINGS FOR FILL-IN W_NomCli IN FRAME FGar
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_AdmCod
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_AdmCod:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_SaveCod IN FRAME F_AdmCod
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Relaciones.Descripcion IN FRAME F_AdmCod
   5 6                                                                  */
/* SETTINGS FOR RADIO-SET Relaciones.Estado IN FRAME F_AdmCod
   5 6                                                                  */
/* SETTINGS FOR FILL-IN Relaciones.Nit_relacion IN FRAME F_AdmCod
   NO-ENABLE 6 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Relaciones.Usuario IN FRAME F_AdmCod
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCodeudor IN FRAME F_AdmCod
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomUsuario IN FRAME F_AdmCod
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_AdmGar
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_AdmGar:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_Save IN FRAME F_AdmGar
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Agencia IN FRAME F_AdmGar
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_AhorrosNA IN FRAME F_AdmGar
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX Garantias.Contabilizada IN FRAME F_AdmGar
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR EDITOR Garantias.Descripcion_Bien IN FRAME F_AdmGar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_Creacion IN FRAME F_AdmGar
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_FinSeguro IN FRAME F_AdmGar
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN Garantias.Fec_IniSeguro IN FRAME F_AdmGar
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN Garantias.Fec_ProxAvaluo IN FRAME F_AdmGar
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN Garantias.Fec_Retiro IN FRAME F_AdmGar
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_UltAvaluo IN FRAME F_AdmGar
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN Garantias.Fec_VctoImpuesto IN FRAME F_AdmGar
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN Garantias.Identificacion_Bien IN FRAME F_AdmGar
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Garantias.Id_Interna IN FRAME F_AdmGar
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN Garantias.Nit_Aseguradora IN FRAME F_AdmGar
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN Garantias.Nom_Impuesto IN FRAME F_AdmGar
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN Garantias.Nro_Seguro IN FRAME F_AdmGar
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN Garantias.Usuario IN FRAME F_AdmGar
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Val_Asegurado IN FRAME F_AdmGar
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN Garantias.Val_Impuesto IN FRAME F_AdmGar
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN Garantias.Val_UltAvaluo IN FRAME F_AdmGar
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN W_NomAse IN FRAME F_AdmGar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomUsu IN FRAME F_AdmGar
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BCod
/* Query rebuild information for BROWSE BCod
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TmpCod.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BCod */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BCre
/* Query rebuild information for BROWSE BCre
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TmpCre.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BCre */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BGar
/* Query rebuild information for BROWSE BGar
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TmpGar.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BGar */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FGar
/* Query rebuild information for FRAME FGar
     _Query            is NOT OPENED
*/  /* FRAME FGar */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_AdmCod
/* Query rebuild information for FRAME F_AdmCod
     _Query            is NOT OPENED
*/  /* FRAME F_AdmCod */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_AdmGar
/* Query rebuild information for FRAME F_AdmGar
     _Query            is NOT OPENED
*/  /* FRAME F_AdmGar */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Administración de Garantias */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Administración de Garantias */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmGar
&Scoped-define SELF-NAME Garantias.Aprobada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Aprobada wWin
ON VALUE-CHANGED OF Garantias.Aprobada IN FRAME F_AdmGar /* Aprobada */
DO:
  ENABLE Btn_Save WITH FRAME F_AdmGar.
  DO WITH FRAME F_AdmGar:
     IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
        ENABLE Garantias.Contabilizada.
        Garantias.Contabilizada:LABEL = "Contabilizar?".
     END.
     ELSE DO:
        IF W_AccAdm EQ "M" AND AVAILABLE Garantias AND Garantias.Contabilizada THEN DO:
           MESSAGE "La garantia ya se encuentra contabilizada" SKIP
                   "lo que significa que anteriormente fue aprobada" SKIP
                   "Para quitar esta garantia, cambie su estado a inactiva" SKIP
                   "y luego salve. esta acción reversará la contabilización" SKIP
                   "contable de la garantia. (LIBERAR GARANTIA)." VIEW-AS ALERT-BOX INFORMATION.
           ASSIGN Garantias.Aprobada:SCREEN-VALUE = STRING(Garantias.Aprobada).
        END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BCod
&Scoped-define FRAME-NAME FGar
&Scoped-define SELF-NAME BCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCod wWin
ON MOUSE-SELECT-DBLCLICK OF BCod IN FRAME FGar
DO:
  APPLY "choose" TO Btn_ModNoadm IN FRAME FGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BCre
&Scoped-define SELF-NAME BCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCre wWin
ON MOUSE-SELECT-CLICK OF BCre IN FRAME FGar
DO:
  RUN MostrarGarantiasCliente.
  APPLY "Entry" TO BROWSE BGar. 
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCre wWin
ON MOUSE-SELECT-DBLCLICK OF BCre IN FRAME FGar
DO:
  RUN MostrarGarantiasCliente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BGar
&Scoped-define SELF-NAME BGar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BGar wWin
ON MOUSE-SELECT-DBLCLICK OF BGar IN FRAME FGar
DO:
  APPLY "choose" TO Btn_ModGar IN FRAME FGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME FGar /* Salir */
DO:
    IF NOT GHPARENT = ? 
    THEN DO:
        RUN hideObject IN THIS-PROCEDURE.    
        RETURN NO-APPLY.
    END.
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


&Scoped-define SELF-NAME Btn_ModGar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ModGar wWin
ON CHOOSE OF Btn_ModGar IN FRAME FGar /* Modificar Garantia */
DO:
  DISABLE Btn_NueGar Btn_NueNoAdm Btn_ModNoAdm Btn_ModGar WITH FRAME FGar.
  W_AccAdm = "M".
  ASSIGN FRAME F_AdmGar:TITLE = "Administrar Garantias (Modificación de Garantias Existentes)".
  RUN MostrarInfoGarantia.
  ENABLE {&List-2} WITH FRAME F_AdmGar.
  /*IF Garantias.Tipo_Garantia EQ 4 THEN ENABLE {&List-3} WITH FRAME F_AdmGar.
  ELSE DISABLE {&List-3} WITH FRAME F_AdmGar.*/
  APPLY "entry" TO Garantias.Tipo_Garantia IN FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ModNoAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ModNoAdm wWin
ON CHOOSE OF Btn_ModNoAdm IN FRAME FGar /* Modificar Codeudor */
DO:
    DISABLE Btn_ModNoAdm Btn_NueNoAdm Btn_ModGar Btn_NueGar WITH FRAME FGar.
    W_AccNoAdm = "M".
    ASSIGN FRAME F_AdmCod:TITLE = "Administrar Garantias No Admisibles (Modificación)".
    RUN MostrarInfoGarantiaNoAdm.
    ENABLE {&List-5} WITH FRAME F_AdmCod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_NueGar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_NueGar wWin
ON CHOOSE OF Btn_NueGar IN FRAME FGar /* Nueva Garantia */
DO:
  ENABLE {&List-1} WITH FRAME F_AdmGar.
  W_AccAdm = "C".
  ASSIGN FRAME F_AdmGar:TITLE = "Administrar Garantias (Creación de una Nueva Garantia)".
  DISABLE Btn_NueGar Btn_ModGar Btn_ModNoAdm Btn_NueNoAdm  WITH FRAME FGar.
  DO WITH FRAME F_AdmGar:
      ASSIGN Garantias.Fec_Creacion:SCREEN-VALUE = STRING(W_Fecha)
             Garantias.Usuario:SCREEN-VALUE      = STRING(W_Usuario).
  END.
  RUN NuevaGarantia.
  APPLY "ENTRY" TO Garantias.Identificacion_Bien IN FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_NueNoAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_NueNoAdm wWin
ON CHOOSE OF Btn_NueNoAdm IN FRAME FGar /* Nuevo Codeudor */
DO:
  ENABLE {&List-6} WITH FRAME F_AdmCod. 
  W_AccNoAdm = "C".
  ASSIGN FRAME F_AdmCod:TITLE = "Administrar Codeudores (Creación)".
  DISABLE Btn_ModNoAdm Btn_ModGar Btn_NueGar Btn_NueNoAdm WITH FRAME FGar.
  DO WITH FRAME F_AdmCod:
      RUN NuevaGarantiaNoAdm. 
      APPLY "entry" TO Relaciones.Nit_Relacion IN FRAME F_AdmCod.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmGar
&Scoped-define SELF-NAME Btn_OutAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutAdm wWin
ON CHOOSE OF Btn_OutAdm IN FRAME F_AdmGar /* Regresar */
DO:
  DISABLE Btn_Save WITH FRAME F_AdmGar.
  ENABLE Btn_NueGar Btn_ModGar Btn_NueNoAdm Btn_ModNoAdm WITH FRAME FGar.
  HIDE FRAME F_AdmGar.
  APPLY "Mouse-Select-Click" TO BROWSE BCre.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmCod
&Scoped-define SELF-NAME Btn_SalirNoAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalirNoAdm wWin
ON CHOOSE OF Btn_SalirNoAdm IN FRAME F_AdmCod /* Salir */
DO:
  ENABLE Btn_NueNoAdm Btn_ModNoAdm Btn_NueGar Btn_ModGar WITH FRAME FGar.
  HIDE FRAME F_AdmCod.
  APPLY "Mouse-Select-Click" TO BROWSE BCre.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmGar
&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save wWin
ON CHOOSE OF Btn_Save IN FRAME F_AdmGar /* Salvar */
DO:
  DEFINE VAR Err AS LOGICAL INITIAL NO.

  IF Garantias.Identificacion_Bien:SCREEN-VALUE LE " " THEN DO:
     MESSAGE "Identificacion bien indispensable."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
  END.

  IF Garantias.Tipo_Garantia:SCREEN-VALUE EQ "4"  THEN DO:
     FIND FIRST Ahorros WHERE Ahorros.Nit        EQ Garantias.Nit_Aseguradora:SCREEN-VALUE     AND
                              Ahorros.Cue_Ahorro EQ Garantias.Identificacion_Bien:SCREEN-VALUE AND
                             (Ahorros.Tip_Ahorro EQ 2 OR Ahorros.Tip_Ahorro EQ 3) AND
                              Ahorros.Estado     EQ 1 AND
                              Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje GT 0 NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Ahorros) OR 
        (Ahorros.Tip_Ahorro EQ 1 OR Ahorros.Tip_Ahorro EQ 4) THEN DO:
        MESSAGE "La cuenta seleccionada Debe ser Cdat/Contractual y Activa," SKIP
                "Escoja una cuenta de este Tipo para la Garantia!" VIEW-AS ALERT-BOX WARNING.
        ASSIGN Garantias.Identificacion_Bien:SCREEN-VALUE = ""
               Garantias.Val_Bien:SCREEN-VALUE            = "0".
        RETURN.
     END.
  END.

  /*RUN Validaciones_Admisibles(OUTPUT Err).*/
 IF Err THEN DO:
     Err = NO.
     /*RUN MostrarInfoGarantia.*/
  END.
  ELSE DO:
      IF W_AccAdm EQ "M" THEN 
         RUN GrabarMod_Admisibles.
      IF W_AccAdm EQ "C" THEN 
         RUN GrabarCre_Admisibles.

      APPLY "choose" TO Btn_OutAdm IN FRAME F_AdmGar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmCod
&Scoped-define SELF-NAME Btn_SaveCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SaveCod wWin
ON CHOOSE OF Btn_SaveCod IN FRAME F_AdmCod /* Salvar */
DO:
  DEFINE VAR Err AS LOGICAL INITIAL NO.
  RUN Validaciones_NoAdmisibles(OUTPUT Err).
  IF Err THEN Err = NO.
  ELSE DO:
      IF W_AccNoAdm EQ "M" THEN 
         RUN GrabarMod_NoAdmisibles.
      IF W_AccNoAdm EQ "C" THEN 
         RUN GrabarCre_NoAdmisibles.

      APPLY "Choose" TO Btn_SalirNoAdm.      
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FGar
&Scoped-define SELF-NAME BUTTON-170
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-170 wWin
ON CHOOSE OF BUTTON-170 IN FRAME FGar /* Button 170 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Garantias.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
  /*{Imprimir.I "listado"}*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmGar
&Scoped-define SELF-NAME Cmb_Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencia wWin
ON VALUE-CHANGED OF Cmb_Agencia IN FRAME F_AdmGar /* Agencia */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_AhorrosNA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_AhorrosNA wWin
ON LEAVE OF Cmb_AhorrosNA IN FRAME F_AdmGar /* Productos de Ahorro */
DO:
/*valida si existe la cuenta de ahorros donde cuenta sea igual a la identifiacion del bien
  y el producto sea igual al de este combo, esto si es una garantia no admisible interna*/
DO WITH FRAME F_AdmGar:
   FIND FIRST Ahorros WHERE 
        Ahorros.Nit            EQ Garantias.Nit_Aseguradora:SCREEN-VALUE     AND
       (Ahorros.Tip_Ahorro EQ 2 OR Ahorros.Tip_Ahorro EQ 3)                  AND
        Ahorros.Cue_Ahorro     EQ Garantias.Identificacion_Bien:SCREEN-VALUE AND
        Ahorros.Estado         EQ 1 AND
        Ahorros.Sdo_Disponible + SDo_Canje GT 0 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Ahorros THEN DO:
      MESSAGE "No se ha encontrado la cuenta de ahorros con" SKIP
              "Número de Cuenta : " Garantias.Identificacion_Bien:SCREEN-VALUE SKIP
              "Producto         : " Cmb_AhorrosNA:SCREEN-VALUE SKIP(1)
              "Escoja otra cuenta de Ahorros para respaldar el crédito"
      VIEW-AS ALERT-BOX WARNING.
      ASSIGN Garantias.Id_Interna:SCREEN-VALUE = "No".
             /*Cmb_AhorrosNA:SCREEN-VALUE = "000 - Pdcto Ahorro no escogido".*/
      DISABLE Cmb_AhorrosNA Garantias.Id_Interna.
   END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_AhorrosNA wWin
ON VALUE-CHANGED OF Cmb_AhorrosNA IN FRAME F_AdmGar /* Productos de Ahorro */
DO:
  ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Contabilizada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Contabilizada wWin
ON VALUE-CHANGED OF Garantias.Contabilizada IN FRAME F_AdmGar /* Contabilizar? */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmCod
&Scoped-define SELF-NAME Relaciones.Descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Relaciones.Descripcion wWin
ON VALUE-CHANGED OF Relaciones.Descripcion IN FRAME F_AdmCod /* Descripcion */
DO:
  ENABLE Btn_SaveCod WITH FRAME F_AdmCod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmGar
&Scoped-define SELF-NAME Garantias.Descripcion_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Descripcion_Bien wWin
ON ENTRY OF Garantias.Descripcion_Bien IN FRAME F_AdmGar /* Descripcion_Bien */
DO:
  W_StrCam = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Descripcion_Bien wWin
ON LEAVE OF Garantias.Descripcion_Bien IN FRAME F_AdmGar /* Descripcion_Bien */
DO:
  IF W_StrCam NE SELF:SCREEN-VALUE THEN ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Descripcion_Bien2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Descripcion_Bien2 wWin
ON ENTRY OF Garantias.Descripcion_Bien2 IN FRAME F_AdmGar /* Descripcion_Bien2 */
DO:
  W_StrCam = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Descripcion_Bien2 wWin
ON LEAVE OF Garantias.Descripcion_Bien2 IN FRAME F_AdmGar /* Descripcion_Bien2 */
DO:
  IF W_StrCam NE SELF:SCREEN-VALUE THEN ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Estado wWin
ON VALUE-CHANGED OF Garantias.Estado IN FRAME F_AdmGar /* Estado */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmCod
&Scoped-define SELF-NAME Relaciones.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Relaciones.Estado wWin
ON VALUE-CHANGED OF Relaciones.Estado IN FRAME F_AdmCod /* Estado */
DO:
    ENABLE Btn_SaveCod WITH FRAME F_AdmCod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmGar
&Scoped-define SELF-NAME Garantias.Fec_Creacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_Creacion wWin
ON VALUE-CHANGED OF Garantias.Fec_Creacion IN FRAME F_AdmGar /* Fecha Creación */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_FinSeguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_FinSeguro wWin
ON LEAVE OF Garantias.Fec_FinSeguro IN FRAME F_AdmGar /* Vencimiento Seguro */
DO:
  IF DATE(SELF:SCREEN-VALUE) LE DATE(Garantias.Fec_IniSeguro:SCREEN-VALUE) 
  OR DATE(SELF:SCREEN-VALUE) LE W_Fecha THEN DO:
     MESSAGE "La fecha de vencimiento del seguro no puede ser" SKIP
             "menor o igual a la de inicio, O estar Vencida" SKIP(1)
             "Rectifique la fecha entrada" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Garantias.Fec_IniSeguro IN FRAME F_AdmGar.
     SELF:SCREEN-VALUE = ?.    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_FinSeguro wWin
ON VALUE-CHANGED OF Garantias.Fec_FinSeguro IN FRAME F_AdmGar /* Vencimiento Seguro */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_IniSeguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_IniSeguro wWin
ON VALUE-CHANGED OF Garantias.Fec_IniSeguro IN FRAME F_AdmGar /* Fecha Inicio Seguro */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_ProxAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_ProxAvaluo wWin
ON LEAVE OF Garantias.Fec_ProxAvaluo IN FRAME F_AdmGar /* Fecha de Próximo Avaluo */
DO:
   IF DATE(SELF:SCREEN-VALUE) LT W_Fecha THEN DO:
       MESSAGE "F.Prox-Avaluo debe ser Posterior a Hoy"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       SELF:SCREEN-VALUE = ?.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_ProxAvaluo wWin
ON VALUE-CHANGED OF Garantias.Fec_ProxAvaluo IN FRAME F_AdmGar /* Fecha de Próximo Avaluo */
DO:
   ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_Retiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_Retiro wWin
ON VALUE-CHANGED OF Garantias.Fec_Retiro IN FRAME F_AdmGar /* Fecha de Retiro */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_UltAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_UltAvaluo wWin
ON LEAVE OF Garantias.Fec_UltAvaluo IN FRAME F_AdmGar /* Fecha Último Avaluo */
DO:
   IF DATE(SELF:SCREEN-VALUE) GT W_Fecha THEN DO:
       MESSAGE "F.Ult-Avaluo Posterior a Hoy...?"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       SELF:SCREEN-VALUE = ?.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_UltAvaluo wWin
ON VALUE-CHANGED OF Garantias.Fec_UltAvaluo IN FRAME F_AdmGar /* Fecha Último Avaluo */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_VctoImpuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_VctoImpuesto wWin
ON VALUE-CHANGED OF Garantias.Fec_VctoImpuesto IN FRAME F_AdmGar /* Fec. Vencimiento */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Identificacion_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Identificacion_Bien wWin
ON LEAVE OF Garantias.Identificacion_Bien IN FRAME F_AdmGar /* Identificación del Bien */
DO:
  ENABLE Btn_Save WITH FRAME F_AdmGar.

  IF Garantias.Tipo_Garantia:SCREEN-VALUE EQ "4"  THEN DO:
     MESSAGE "Seleccione El Cdat/Contractual de Garantia."  
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
     FRAME FGar:SENSITIVE = FALSE.
     RUN C-Ahorros.r (INPUT W_NitCli:SCREEN-VALUE, OUTPUT A_Age, OUTPUT A_Pro, OUTPUT A_NitW, OUTPUT A_Cue).
     FIND Ahorros WHERE Ahorros.Agencia      EQ A_Age  AND
                        Ahorros.Cod_Ahorro   EQ A_Pro  AND
                        Ahorros.Nit          EQ A_NitW AND
                        Ahorros.Cue_Ahorros  EQ A_Cue  AND
                        Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje GT 0 NO-LOCK NO-ERROR.
     FRAME FGar:SENSITIVE = TRUE.

     IF (AVAILABLE(Ahorros) AND (Ahorros.Tip_Ahorro EQ 1 OR Ahorros.Tip_Ahorro EQ 4))
     OR NOT AVAILABLE(Ahorros) THEN DO:
        MESSAGE "La cuenta seleccionada Debe ser Cdat/Contractual y Activa," SKIP
                "Escoja una cuenta de este Tipo para la Garantia!" VIEW-AS ALERT-BOX WARNING.
        ASSIGN Garantias.Identificacion_Bien:SCREEN-VALUE = ""
               Garantias.Val_Bien:SCREEN-VALUE            = "0".
     END.
     ELSE DO:
        ASSIGN Garantias.Identificacion_Bien:SCREEN-VALUE = Ahorros.Cue_Ahorros
               Garantias.Val_Bien:SCREEN-VALUE            = STRING(Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)
               Garantias.Nom_Bien:SCREEN-VALUE            = "Cod.Ahorro " + STRING(Ahorros.Cod_Ahorro,"99") +
                                                            " del Asociado con Ced/Nit : " + A_NitW
               Garantias.Fec_FinSeguro:SCREEN-VALUE       = STRING(Ahorros.Fec_Vencimiento)
               Garantias.Nit_Aseguradora:SCREEN-VALUE     = Ahorros.Nit
               Garantias.Id_Interna:SCREEN-VALUE          = "Yes".        
               
        APPLY "Entry" TO Garantias.Val_Bien.
        RETURN NO-APPLY.
     END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Id_Interna
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Id_Interna wWin
ON VALUE-CHANGED OF Garantias.Id_Interna IN FRAME F_AdmGar /* Id_Interna */
DO:
DO WITH FRAME F_AdmGar:
   ENABLE Btn_Save WITH FRAME F_AdmGar.
   IF SELF:SCREEN-VALUE EQ "yes" THEN
      ENABLE Cmb_AhorrosNA.
   ELSE
      DISABLE Cmb_AhorrosNA.
  /* ASSIGN Cmb_AhorrosNA:SCREEN-VALUE = "000 - Pdcto Ahorro no escogido".*/
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Nit_Aseguradora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nit_Aseguradora wWin
ON LEAVE OF Garantias.Nit_Aseguradora IN FRAME F_AdmGar /* Nit Aseguradora */
DO:
    DEFINE VAR P_NitG LIKE Clientes.Nit.
    IF SELF:SCREEN-VALUE EQ "" THEN DO:
        RUN C-Clientes.r (INPUT 2, INPUT W_Agencia,
                          OUTPUT P_NitG, OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        FIND Clientes WHERE Clientes.Nit EQ P_NitG NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN
           ASSIGN P_Nombre   = Clientes.Nombre
                  P_Apellido = Clientes.Apellido1 + " " + Clientes.Apellido2
                  P_NitG     = Clientes.Nit.
    END.
    IF NOT AVAILABLE Clientes THEN DO:
       MESSAGE "No existe el nit de la aseguradora, reintente digitando un nit valido"
           VIEW-AS ALERT-BOX INFORMATION.
       APPLY "entry" TO SELF.
    END.
    ELSE DO:
        ASSIGN W_NomAse = P_Nombre + " " + P_Apellido
               Garantias.Nit_Aseguradora:SCREEN-VALUE = P_NitG.
        DISPLAY W_NomAse WITH FRAME F_AdmGar.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nit_Aseguradora wWin
ON VALUE-CHANGED OF Garantias.Nit_Aseguradora IN FRAME F_AdmGar /* Nit Aseguradora */
DO:
   ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmCod
&Scoped-define SELF-NAME Relaciones.Nit_relacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Relaciones.Nit_relacion wWin
ON LEAVE OF Relaciones.Nit_relacion IN FRAME F_AdmCod /* Nit Codeudor */
DO:
DO WITH FRAME F_AdmCod:
    DEFINE VAR P_NitC LIKE Clientes.Nit.
    IF SELF:SCREEN-VALUE EQ "" THEN DO:
        RUN C-Clientes.r (INPUT 2, INPUT W_Agencia,
                          OUTPUT P_NitC, OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        FIND Clientes WHERE Clientes.Nit EQ P_NitC NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN
           ASSIGN P_Nombre   = Clientes.Nombre
                  P_Apellido = Clientes.Apellido1 + " " + Clientes.Apellido2
                  P_NitC     = Clientes.Nit.
    END.
    IF NOT AVAILABLE Clientes THEN DO:
       MESSAGE "No existe el nit de la aseguradora, reintente digitando un nit valido"
           VIEW-AS ALERT-BOX INFORMATION.
       APPLY "entry" TO SELF.
    END.
    ELSE DO:
        ASSIGN W_NomCodeudor = P_Nombre + " " + P_Apellido
               Relaciones.Nit_Relacion:SCREEN-VALUE = P_NitC.
        DISPLAY W_NomCodeudor WITH FRAME F_AdmCod.
    END.  
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Relaciones.Nit_relacion wWin
ON VALUE-CHANGED OF Relaciones.Nit_relacion IN FRAME F_AdmCod /* Nit Codeudor */
DO:
  ENABLE Btn_SaveCod WITH FRAME F_AdmCod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmGar
&Scoped-define SELF-NAME Garantias.Nom_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nom_Bien wWin
ON VALUE-CHANGED OF Garantias.Nom_Bien IN FRAME F_AdmGar /* Nom_Bien */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Nom_Impuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nom_Impuesto wWin
ON VALUE-CHANGED OF Garantias.Nom_Impuesto IN FRAME F_AdmGar /* Nombre Impuesto */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Nro_Seguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nro_Seguro wWin
ON VALUE-CHANGED OF Garantias.Nro_Seguro IN FRAME F_AdmGar /* Número Seguro */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FGar
&Scoped-define SELF-NAME R_TipoGarantias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_TipoGarantias wWin
ON VALUE-CHANGED OF R_TipoGarantias IN FRAME FGar
DO:
    ASSIGN FRAME FGar R_TipoGarantias.
    OPEN QUERY BGar FOR EACH TmpGar WHERE TmpGar.EstGar EQ R_TipoGarantias INDEXED-REPOSITION.  
    OPEN QUERY BCod FOR EACH TmpCod WHERE TmpCod.EstGar EQ R_TipoGarantias INDEXED-REPOSITION.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmGar
&Scoped-define SELF-NAME Garantias.Tipo_Garantia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Tipo_Garantia wWin
ON ENTRY OF Garantias.Tipo_Garantia IN FRAME F_AdmGar
DO:
  DO WITH FRAME F_AdmGar:
        IF SELF:SCREEN-VALUE EQ "4" THEN DO:
            ENABLE Garantias.Id_Interna.
            DISABLE {&List-3}.
        END.
        ELSE DO:
          DISABLE Garantias.Id_Interna Cmb_AhorrosNA.
          ENABLE {&List-3}.
          ASSIGN Garantias.Id_Interna:SCREEN-VALUE = "no".
                 /*Cmb_AhorrosNA:SCREEN-VALUE = "000 - Pdcto Ahorro no escogido".*/
        END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Tipo_Garantia wWin
ON VALUE-CHANGED OF Garantias.Tipo_Garantia IN FRAME F_AdmGar
DO:
  ENABLE Btn_Save WITH FRAME F_AdmGar.
  DO WITH FRAME F_AdmGar:
/*     IF W_AccAdm = "C" THEN DO:
     END.
     ELSE DO:*/
      /*  IF W_TipGarAnt EQ 4 AND INTEGER(SELF:SCREEN-VALUE) LT 4 THEN DO:
           MESSAGE "No se puede convertir una garantia no admisible" SKIP
                   "a una admisible, ya que estas se deben contabilzar" SKIP
                   "se niega el cambio de tipo de garantia!!!" VIEW-AS ALERT-BOX ERROR.
           SELF:SCREEN-VALUE = STRING(W_TipGarAnt).
        END.
        IF W_TipGarAnt LT 4 AND INTEGER(SELF:SCREEN-VALUE) EQ 4 THEN DO:
           MESSAGE "No se puede convertir una garantia admisible a una" SKIP
                   "NO admisible, ya que estas se deben contabilzar" SKIP
                   "se niega el cambio de tipo de garantia!!!" VIEW-AS ALERT-BOX ERROR.
           SELF:SCREEN-VALUE = STRING(W_TipGarAnt).
        END.
        */

        IF SELF:SCREEN-VALUE EQ "4" THEN DO:
            ENABLE Garantias.Id_Interna.
            DISABLE {&List-3}.
        END.
        ELSE DO:
          DISABLE Garantias.Id_Interna Cmb_AhorrosNA.
          ENABLE {&List-3}.
          ASSIGN Garantias.Id_Interna:SCREEN-VALUE = "no".
                 /*Cmb_AhorrosNA:SCREEN-VALUE = "000 - Pdcto Ahorro no escogido".*/
        END.
/*      END.*/
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Usuario wWin
ON VALUE-CHANGED OF Garantias.Usuario IN FRAME F_AdmGar /* Usuario que actualiza */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Val_Asegurado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Asegurado wWin
ON LEAVE OF Garantias.Val_Asegurado IN FRAME F_AdmGar /* Valor Asegurado */
DO:
DO WITH FRAME F_AdmGar:
    IF Garantias.Tipo_Garantia:SCREEN-VALUE EQ "1" AND
       DECIMAL(Garantias.Val_Bien:SCREEN-VALUE) GT DECIMAL(SELF:SCREEN-VALUE) THEN DO:
       MESSAGE "El valor de la propiedad no puede ser menor que" SKIP
               "su valor de seguro. No se permitirá grabar esta" SKIP
               "garantia hasta que no se corrija esta inconsistencia" VIEW-AS ALERT-BOX INFORMATION.
       ASSIGN Garantias.Val_Asegurado:SCREEN-VALUE = STRING(Garantias.Val_Asegurado).
       DISABLE Btn_Save WITH FRAME F_AdmGar.
    END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Asegurado wWin
ON VALUE-CHANGED OF Garantias.Val_Asegurado IN FRAME F_AdmGar /* Valor Asegurado */
DO:
   ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Val_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Bien wWin
ON VALUE-CHANGED OF Garantias.Val_Bien IN FRAME F_AdmGar /* Valor del Bien */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Val_Impuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Impuesto wWin
ON VALUE-CHANGED OF Garantias.Val_Impuesto IN FRAME F_AdmGar /* Valor del Impuesto */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Val_UltAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_UltAvaluo wWin
ON VALUE-CHANGED OF Garantias.Val_UltAvaluo IN FRAME F_AdmGar /* Valor Último Avaluo */
DO:
    ENABLE Btn_Save WITH FRAME F_AdmGar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FGar
&Scoped-define SELF-NAME W_NitCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCli wWin
ON LEAVE OF W_NitCli IN FRAME FGar /* Nit Cliente */
DO:
  ASSIGN FRAME FGar W_NitCli.
  IF W_NitCli NE "" THEN DO:
      FIND Clientes WHERE Clientes.Nit EQ W_NitCli NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN DO:
         ASSIGN P_Nombre   = Clientes.Nombre
                P_Apellido = Clientes.Apellido1 + " " + Clientes.Apellido2
                P_Nit      = Clientes.Nit
                W_NomCli   = P_Nombre + " " + P_Apellido.
         DISPLAY W_NomCli WITH FRAME FGar.
         RUN BuscarCreditosCliente.
      END.
      ELSE DO:
          IF NOT AVAILABLE Clientes  THEN DO:
             MESSAGE "No existe el nit a consultar, reintente digitando un nit valido"
                 VIEW-AS ALERT-BOX INFORMATION.
             APPLY "entry" TO SELF.
          END.
          ELSE DO:
              ASSIGN W_NomCli = P_Nombre + " " + P_Apellido.
              DISPLAY W_NomCli WITH FRAME FGar.
              RUN BuscarCreditosCliente.
          END.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BCod
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuscarCreditosCliente wWin 
PROCEDURE BuscarCreditosCliente :
ASSIGN FRAME FGar R_TipoGarantias.
FOR EACH TmpCre: DELETE TmpCre. END.
FOR EACH Creditos WHERE Creditos.Nit    EQ P_Nit
                    AND Creditos.Estado EQ 2 NO-LOCK:
    CREATE TmpCre.
    ASSIGN TmpCre.AgeCre = Creditos.Agencia
           TmpCre.TipCre = Creditos.Tip_Credito
           TmpCre.CodCre = Creditos.Cod_Credito
           TmpCre.NumCre = Creditos.Num_Credito
           TmpCre.NumSol = Creditos.Num_Solicitud
           TmpCre.Pagare = Creditos.Pagare
           TmpCre.Monto  = Creditos.Monto
           TmpCre.SdoCap = Creditos.Sdo_Capital
           TmpCre.NitCre = Creditos.Nit.
    IF Creditos.Cuo_Atraso GT 0 OR Creditos.Dias_Atraso GT 0 OR
       Creditos.Int_DifCobro GT 0 OR Creditos.Int_MoraDifCob GT 0 OR
       Creditos.Int_MorCobrar GT 0 OR Creditos.Sdo_IntMor GT 0 OR
       Creditos.Val_Atraso GT 0 THEN 
       TmpCre.ModOk = NO.
END.
OPEN QUERY BCre FOR EACH TmpCre INDEXED-REPOSITION.
RUN MostrarGarantiasCliente.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Admisible wWin 
PROCEDURE Contabilizar_Admisible :
DEFINE VAR W_NumCbt     LIKE Comprobantes.Secuencia.
  DEFINE VAR TotGarantias LIKE Creditos.Sdo_Capital INIT 0.
  DEFINE VAR WRw AS ROWID.

  FIND CortoLargo WHERE 
       CortoLargo.Agencia        EQ Garantias.Agencia AND
       CortoLargo.Clase_Producto EQ 2 AND
       CortoLargo.Cod_Producto   EQ Garantias.Cod_Credito AND
       CortoLargo.Cta_ContingenteDB NE "" AND
       CortoLargo.Cta_ContingenteCR NE "" AND
       CortoLargo.Comprobante    NE 0
       NO-LOCK NO-ERROR.
  IF NOT AVAILABLE CortoLargo THEN DO:
     MESSAGE "No se ha encontrado la configuración de Corto y largo" SKIP
             "o existe algun tipo de inconsistencia en su configuración" SKIP
             "Comunique esta inconsistencia al Administrador del Sistema" SKIP(1)
             "Se cancela la contabilizacion de la garantia"
             VIEW-AS ALERT-BOX ERROR.
     ASSIGN Garantias.Contabilizada:SCREEN-VALUE IN FRAME F_AdmGar = "no".
     RETURN ERROR.
  END.
  FIND Comprobantes WHERE 
         Comprobantes.Agencia     EQ Garantias.Agencia AND
         Comprobantes.Comprobante EQ CortoLargo.Comprobante NO-ERROR.
  IF NOT AVAILABLE Comprobantes THEN DO:
     MESSAGE "No se ha encontrado el comprobante para la contabilización" SKIP
             "para la contabilización de la garantia. el cual debe estar" SKIP 
             "Configurado en corto y largo. Rectifique con el Administrador!"
             VIEW-AS ALERT-BOX ERROR. 
     RETURN ERROR.
  END.
  ASSIGN W_NumCbt               = Comprobantes.Secuencia + 1
         Comprobantes.Secuencia = Comprobantes.Secuencia + 1.

IF Garantias.Val_Bien GT 0 THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia        = Garantias.Agencia
           Mov_Contable.Comprobante    = CortoLargo.Comprobante
           Mov_Contable.Cuenta         = CortoLargo.Cta_ContrapartidaGar
           Mov_Contable.Fec_Contable   = W_Fecha
           Mov_Contable.Comentario     = "Garantía Admisible"
           Mov_Contable.Usuario        = W_Usuario
           Mov_contable.Nit            = TmpCre.NitCre
           Mov_Contable.Cen_Costos     = 999
           Mov_Contable.Destino        = W_Agencia
           Mov_Contable.Num_Documento  = INTEGER(W_NumCbt)
           Mov_Contable.Doc_Referencia = Garantias.Identificacion_Bien
           Mov_Contable.Enlace         = string(Garantias.Num_Credito)
           Mov_Contable.Fec_Grabacion  = TODAY
           Mov_Contable.Hora           = TIME
           Mov_Contable.Estacion       = W_Estacion 
           Mov_Contable.CR             = Garantias.Val_Bien.
    WRw = ROWID(Mov_Contable).

    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia        = Garantias.Agencia
           Mov_Contable.Comprobante    = CortoLargo.Comprobante
           Mov_Contable.Cuenta         = CortoLargo.Cta_VigGarAd
           Mov_Contable.Fec_Contable   = W_Fecha
           Mov_Contable.Comentario     = "Garantía Admisible"
           Mov_Contable.Usuario        = W_Usuario
           Mov_contable.Nit            = TmpCre.NitCre
           Mov_Contable.Cen_Costos     = 999
           Mov_Contable.Destino        = W_Agencia
           Mov_Contable.Num_Documento  = INTEGER(W_NumCbt)
           Mov_Contable.Doc_Referencia = Garantias.Identificacion_Bien
           Mov_Contable.Enlace         = string(Garantias.Num_Credito)
           Mov_Contable.Fec_Grabacion  = TODAY
           Mov_Contable.Hora           = TIME
           Mov_Contable.Estacion       = W_Estacion 
           Mov_Contable.DB             = Garantias.Val_Bien.
END.
RUN formatos.r (INPUT "NOTA2MUL", Wrw, 0, 0, "Contab.Garantia Admisible", W_NumCbt, 0).

/*impresion del comprobante*/
/*FIND Formatos WHERE Formatos.Agencia     EQ Garantias.Agencia                
                AND Formatos.Cod_Formato EQ Comprobantes.Cod_Formato  NO-LOCK NO-ERROR.                                          
IF AVAILABLE(Formatos) THEN DO:                                             
   RUN VALUE(Formatos.Nom_Proceso) (INPUT Comprobantes.Comprobante,
                                    INPUT W_NumCbt, INPUT W_NumCbt,
                                    INPUT Garantias.Agencia) NO-ERROR.                              
   IF ERROR-STATUS:ERROR THEN
      MESSAGE "Formato Impresiòn con error" VIEW-AS ALERT-BOX.

END.
ELSE                                                                      
   MESSAGE "No se hallò el Formato de Impresión" SKIP                    
            "consulte con el administrador!...,La contabilizaciòn està Ok." VIEW-AS ALERT-BOX.*/

RELEASE Comprobantes.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Reversion wWin 
PROCEDURE Contabilizar_Reversion :
DEFINE VAR W_NumCbt     LIKE Comprobantes.Secuencia.
  DEFINE VAR TotGarantias LIKE Creditos.Sdo_Capital INIT 0.
  DEFINE VAR WRw AS ROWID.

  FIND CortoLargo WHERE 
       CortoLargo.Agencia        EQ Garantias.Agencia AND
       CortoLargo.Clase_Producto EQ 2 AND
       CortoLargo.Cod_Producto   EQ Garantias.Cod_Credito AND
       CortoLargo.Cta_ContingenteDB NE "" AND
       CortoLargo.Cta_ContingenteCR NE "" AND
       CortoLargo.Comprobante    NE 0
       NO-LOCK NO-ERROR.
  IF NOT AVAILABLE CortoLargo THEN DO:
     MESSAGE "No se ha encontrado la configuración de Corto y largo" SKIP
             "o existe algun tipo de inconsistencia en su configuración" SKIP
             "Comunique esta inconsistencia al Administrador del Sistema" SKIP(1)
             "Se cancela la contabilizacion de la garantia"
             VIEW-AS ALERT-BOX ERROR.
     ASSIGN Garantias.Contabilizada:SCREEN-VALUE IN FRAME F_AdmGar = "no".
     RETURN ERROR.
  END.
  FIND Comprobantes WHERE 
         Comprobantes.Agencia     EQ Garantias.Agencia AND
         Comprobantes.Comprobante EQ CortoLargo.Comprobante NO-ERROR.
  IF NOT AVAILABLE Comprobantes THEN DO:
     MESSAGE "No se ha encontrado el comprobante para la contabilización" SKIP
             "para la contabilización de la garantia. el cual debe estar" SKIP 
             "Configurado en corto y largo. Rectifique con el Administrador!"
             VIEW-AS ALERT-BOX ERROR. 
     RETURN ERROR.
  END.
  ASSIGN W_NumCbt               = Comprobantes.Secuencia + 1
         Comprobantes.Secuencia = Comprobantes.Secuencia + 1.

IF Garantias.Val_Bien GT 0 THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia        = Garantias.Agencia
           Mov_Contable.Comprobante    = CortoLargo.Comprobante
           Mov_Contable.Cuenta         = CortoLargo.Cta_GarPenCancel
           Mov_Contable.Fec_Contable   = W_Fecha
           Mov_Contable.Comentario     = "Reversion Gar.Admisible"
           Mov_Contable.Usuario        = W_Usuario
           Mov_contable.Nit            = TmpCre.NitCre
           Mov_Contable.Cen_Costos     = 999
           Mov_Contable.Destino        = W_Agencia
           Mov_Contable.Num_Documento  = INTEGER(W_NumCbt)
           Mov_Contable.Doc_Referencia = Garantias.Identificacion_Bien
           Mov_Contable.Enlace         = string(Garantias.Num_Credito)
           Mov_Contable.Fec_Grabacion  = TODAY
           Mov_Contable.Hora           = TIME
           Mov_Contable.Estacion       = W_Estacion 
           Mov_Contable.CR             = Garantias.Val_Bien NO-ERROR.
    WRw = ROWID(Mov_Contable).

    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia        = Garantias.Agencia
           Mov_Contable.Comprobante    = CortoLargo.Comprobante
           Mov_Contable.Cuenta         = CortoLargo.Cta_VigGarAd
           Mov_Contable.Fec_Contable   = W_Fecha
           Mov_Contable.Comentario     = "Reversion.Gar.Admisible"
           Mov_Contable.Usuario        = W_Usuario
           Mov_contable.Nit            = TmpCre.NitCre
           Mov_Contable.Cen_Costos     = 999
           Mov_Contable.Destino        = W_Agencia
           Mov_Contable.Num_Documento  = INTEGER(W_NumCbt)
           Mov_Contable.Doc_Referencia = Garantias.Identificacion_Bien
           Mov_Contable.Enlace         = string(Garantias.Num_Credito)
           Mov_Contable.Fec_Grabacion  = TODAY
           Mov_Contable.Hora           = TIME
           Mov_Contable.Estacion       = W_Estacion 
           Mov_Contable.DB             = Garantias.Val_Bien NO-ERROR.

END.

RUN formatos.r (INPUT "NOTA2MUL", Wrw, 0, 0, "Reversion Garantia Admisible", W_NumCbt, 0).

/*impresion del comprobante*/
/* FIND Formatos WHERE Formatos.Agencia     EQ Garantias.Agencia                
                 AND Formatos.Cod_Formato EQ Comprobantes.Cod_Formato  NO-LOCK NO-ERROR.                                          
 IF AVAILABLE(Formatos) THEN DO:                                             
    RUN VALUE(Formatos.Nom_Proceso) (INPUT Comprobantes.Comprobante,                            
                                     INPUT W_NumCbt, INPUT W_NumCbt,          
                                     INPUT Garantias.Agencia) NO-ERROR.                              
    IF ERROR-STATUS:ERROR THEN
       MESSAGE "Formato Impresiòn con error" VIEW-AS ALERT-BOX.
 END.
 ELSE                                                                      
    MESSAGE "No se hallò el Formato de Impresión" SKIP                    
            "consulte con el administrador!...,La contabilizaciòn està Ok." VIEW-AS ALERT-BOX.                     */
RELEASE Comprobantes.
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
  DISPLAY W_NitCli W_NomCli R_TipoGarantias 
      WITH FRAME FGar IN WINDOW wWin.
  ENABLE W_NitCli Btn_ModNoAdm Btn_NueNoAdm BCod BUTTON-168 BUTTON-170 BCre 
         Btn_NueGar Btn_ModGar BGar BtnDone R_TipoGarantias BUTTON-169 RECT-307 
      WITH FRAME FGar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FGar}
  DISPLAY W_NomCodeudor W_NomUsuario E_Code 
      WITH FRAME F_AdmCod IN WINDOW wWin.
  IF AVAILABLE Relaciones THEN 
    DISPLAY Relaciones.Nit_relacion Relaciones.Estado Relaciones.Descripcion 
          Relaciones.Usuario 
      WITH FRAME F_AdmCod IN WINDOW wWin.
  ENABLE Relaciones.Estado Relaciones.Descripcion E_Code Btn_SalirNoAdm 
      WITH FRAME F_AdmCod IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_AdmCod}
  DISPLAY Cmb_Agencia Cmb_AhorrosNA W_NomAse W_NomUsu 
      WITH FRAME F_AdmGar IN WINDOW wWin.
  IF AVAILABLE Garantias THEN 
    DISPLAY Garantias.Descripcion_Bien2 Garantias.Fec_Creacion 
          Garantias.Tipo_Garantia Garantias.Fec_Retiro Garantias.Aprobada 
          Garantias.Estado Garantias.Identificacion_Bien Garantias.Nom_Bien 
          Garantias.Val_Bien Garantias.Contabilizada Garantias.Descripcion_Bien 
          Garantias.Id_Interna Garantias.Nit_Aseguradora Garantias.Nro_Seguro 
          Garantias.Val_Asegurado Garantias.Fec_IniSeguro 
          Garantias.Fec_FinSeguro Garantias.Val_UltAvaluo 
          Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo 
          Garantias.Nom_Impuesto Garantias.Fec_VctoImpuesto 
          Garantias.Val_Impuesto Garantias.Usuario 
      WITH FRAME F_AdmGar IN WINDOW wWin.
  ENABLE Garantias.Descripcion_Bien2 Garantias.Tipo_Garantia Garantias.Aprobada 
         Garantias.Estado Garantias.Identificacion_Bien Garantias.Nom_Bien 
         Garantias.Val_Bien Garantias.Nit_Aseguradora Garantias.Nro_Seguro 
         Garantias.Val_Asegurado Garantias.Fec_IniSeguro 
         Garantias.Fec_FinSeguro Garantias.Val_UltAvaluo 
         Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo 
         Garantias.Nom_Impuesto Garantias.Fec_VctoImpuesto 
         Garantias.Val_Impuesto Btn_OutAdm RECT-302 RECT-303 RECT-304 RECT-305 
         RECT-306 
      WITH FRAME F_AdmGar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_AdmGar}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabarCre_Admisibles wWin 
PROCEDURE GrabarCre_Admisibles :
CREATE Garantias.
DO WITH FRAME F_AdmGar:
   ASSIGN {&List-1} Garantias.Usuario.
   ASSIGN Garantias.Nit = W_NitCli:SCREEN-VALUE IN FRAME FGar
          Garantias.Identificacion_Bien
          Garantias.Descripcion_Bien
          Garantias.Descripcion_Bien2
          Garantias.Agencia       = TmpCre.AgeCre  
          Garantias.Cod_Credito   = TmpCre.CodCre                  
          Garantias.Tip_Credito   = TmpCre.TipCre                  
          Garantias.Num_Solicitud = TmpCre.NumSol                  
          Garantias.Num_Credito   = TmpCre.NumCre  
          Garantias.Cod_Credito   = TmpCre.CodCre 
          Garantias.Tip_Credito   = TmpCre.TipCre
          Garantias.Fec_Creacion  = W_Fecha
          Garantias.Aprobada
          Garantias.Val_Bien
          Garantias.Nom_Bien
          Garantias.Tipo_Garantia.

   IF Garantias.Tipo_Garantia:SCREEN-VALUE EQ "4"  THEN
      Garantias.Cod_AhorroNA  = Ahorros.Cod_Ahorro. 
   /*puede contabilizar si esta viendo las garantias activas*/
   IF R_TipoGarantias EQ 1 THEN DO:
       IF Garantias.Aprobada AND Garantias.Contabilizada AND 
          Garantias.Tipo_Garantia NE 4 AND Garantias.Estado EQ 1 THEN DO:
          /*RUN Contabilizar_Admisible.*/
          Garantias.Contabilizada:LABEL = "Contabilizada".
       END.
      /* ELSE
          Garantias.Contabilizada:LABEL = "Contabilizar?".*/
   END.
   IF Garantias.Estado EQ 2 THEN Garantias.Fec_Retiro = W_Fecha.
   DISABLE Btn_Save.
END.
RUN MostrarGarantiasCliente.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabarCre_NoAdmisibles wWin 
PROCEDURE GrabarCre_NoAdmisibles :
CREATE Relaciones.
DO WITH FRAME F_AdmCod:
   ASSIGN {&List-6}.
   ASSIGN Relaciones.Cod_Producto   = TmpCre.CodCre
          Relaciones.Nit            = TmpCre.NitCre
          Relaciones.Cuenta         = STRING(TmpCre.NumCre) 
          Relaciones.Clase_Producto = 2
          Relaciones.Cod_Relacion   = 11
          Relaciones.Usuario        = W_Usuario.
   DISABLE Btn_SaveCod.
END.
RUN MostrarGarantiasCliente.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabarMod_Admisibles wWin 
PROCEDURE GrabarMod_Admisibles :
DEFINE VAR Id_Cont AS LOGICAL.
FIND Garantias WHERE ROWID(Garantias) EQ W_Puntero NO-ERROR.
IF AVAILABLE Garantias AND Garantias.Contabilizada THEN Id_Cont = YES.
IF AVAILABLE Garantias THEN
  DO WITH FRAME F_AdmGar:
    ASSIGN FRAME F_AdmGar {&List-2}
           Garantias.Nit = W_NitCli:SCREEN-VALUE IN FRAME FGar
           Garantias.Identificacion_Bien
           Garantias.Aprobada
           Garantias.Val_Bien
           Garantias.Descripcion_Bien2
           Garantias.Descripcion_Bien
           Garantias.Nom_Bien
           Garantias.Tipo_Garantia
           Garantias.Cod_Credito   = TmpCre.CodCre 
           Garantias.Tip_Credito   = TmpCre.TipCre
           Garantias.Estado
           Garantias.Agencia = TmpCre.AgeCre. 

    /*puede contabilizar si esta en garantias activas y la actual no se ha contabilizado*/
/*    IF NOT Id_Cont AND R_TipoGarantias EQ 1 THEN DO:
        IF Garantias.Aprobada AND Garantias.Contabilizada AND
           Garantias.Tipo_Garantia NE 4 AND Garantias.Estado EQ 1 THEN DO:
           RUN Contabilizar_Admisible.
           Garantias.Contabilizada:LABEL = "Contabilizada".
        END.
        ELSE DO:
            IF Garantias.Estado EQ 1 AND Garantias.Contabilizada EQ NO THEN
            Garantias.Contabilizada:LABEL = "Contabilizar?".
        END.
    END.
    IF Garantias.Estado EQ 2 THEN DO: 
       Garantias.Fec_Retiro = W_Fecha.
       RUN Contabilizar_Reversion.
    END.*/

    IF Garantias.Tipo_Garantia:SCREEN-VALUE EQ "4"  THEN
       Garantias.Cod_AhorroNA = Ahorros.Cod_Ahorro.
    IF Garantias.Id_Interna THEN ENABLE {&List-3}.
    ELSE ENABLE {&List-3}.
    DISABLE Btn_Save.
  END.
ELSE
  MESSAGE "No encontro garantias admisibles para grabar" VIEW-AS ALERT-BOX.
RELEASE Garantias.
DISABLE Btn_Save WITH FRAME F_AdmGar.
RUN MostrarGarantiasCliente.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabarMod_NoAdmisibles wWin 
PROCEDURE GrabarMod_NoAdmisibles :
FIND CURRENT Relaciones NO-ERROR.
IF AVAILABLE Relaciones THEN
  DO WITH FRAME F_AdmCod:
    ASSIGN {&List-6}.
  END.
ELSE
  MESSAGE "No encontro la relación del codeudor" VIEW-AS ALERT-BOX.
RELEASE Relaciones.
DISABLE Btn_SaveCod WITH FRAME F_AdmCod.
RUN MostrarGarantiasCliente.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FOR EACH Agencias NO-LOCK:
      W_Ok = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_AdmGar.
  END.

  FOR EACH Pro_Ahorros WHERE (Pro_Ahorros.Tip_Ahorro EQ 2 OR Pro_Ahorros.Tip_Ahorro EQ 3) AND
           Pro_Ahorros.Estado EQ 1 NO-LOCK:
      W_Ok = Cmb_AhorrosNA:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).
      Cmb_AhorrosNA:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
  END.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MostrarGarantiasCliente wWin 
PROCEDURE MostrarGarantiasCliente :
FOR EACH TmpGar: DELETE TmpGar. END.
FOR EACH Garantias WHERE
         /*Garantias.Cod_Credito   EQ TmpCre.CodCre  AND*/
        Garantias.Num_Credito   EQ TmpCre.NumCre AND
         Garantias.Tip_Credito   EQ TmpCre.TipCre  AND
         Garantias.Num_Solicitud EQ TmpCre.NumSol 
           NO-LOCK:
    CREATE TmpGar.
    ASSIGN TmpGar.AgeGar = TmpCre.AgeCre
           TmpGar.TipGar = Garantias.Tipo_Garantia
           TmpGar.NomGar = Garantias.Nom_Bien
           TmpGar.IdeGar = Garantias.Identificacion_Bien
           TmpGar.ValGar = Garantias.Val_Bien
           TmpGar.TipCre = Garantias.Tip_Credito
           TmpGar.CodCre = Garantias.Cod_Credito
           TmpGar.NumCre = Garantias.Num_Credito
           TmpGar.NumSol = Garantias.Num_Solicitud
           TmpGar.EstGar = Garantias.Estado.
    CASE Garantias.Tipo_Garantia:
        WHEN 1 THEN TmpGar.NomTip = "Propiedad".
        WHEN 2 THEN TmpGar.NomTip = "Vehículo".
        WHEN 3 THEN TmpGar.NomTip = "Inversión".
        WHEN 4 THEN TmpGar.NomTip = "Cdat No-Ad".
        WHEN 5 THEN TmpGar.NomTip = "Otras No-Ad".
    END CASE.
END.
OPEN QUERY BGar FOR EACH TmpGar WHERE TmpGar.EstGar EQ R_TipoGarantias INDEXED-REPOSITION.

FOR EACH TmpCod: DELETE TmpCod. END.
FOR EACH Relaciones WHERE 
         Relaciones.Nit            EQ TmpCre.NitCre AND
         INTEG(Relaciones.Cuenta)  EQ TmpCre.NumCre AND
         Relaciones.Clase_Producto EQ 2             AND
         /*Relaciones.Cod_Producto   EQ TmpCre.CodCre AND */
         Relaciones.Cod_Relacion   EQ 11:
    FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
      CREATE TmpCod.
      ASSIGN TmpCod.AgeCod      = TmpCre.AgeCre
             TmpCod.ClaPro      = 2
             TmpCod.NitCli      = TmpCre.Nit
             TmpCod.NitRel      = Relaciones.Nit_Relacion
             TmpCod.NumCre      = Relaciones.Cuenta
             TmpCod.NomCod      = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
             TmpCod.EstGar      = Relaciones.Estado
             TmpCod.CodPro      = Relaciones.Cod_Producto
             TmpCod.TelRes      = Clientes.Tel_Residencia
             TmpCod.TelCom      = Clientes.Tel_Comercial.
    END.
END.
OPEN QUERY BCod FOR EACH TmpCod WHERE TmpCod.EstGar EQ R_TipoGarantias INDEXED-REPOSITION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MostrarInfoGarantia wWin 
PROCEDURE MostrarInfoGarantia :
DO WITH FRAME F_AdmGar:
  FIND Garantias WHERE
       Garantias.Cod_Credito   EQ TmpGar.CodCre  AND
       Garantias.Tip_Credito   EQ TmpGar.TipCre  AND
       Garantias.Num_Solicitud EQ TmpGar.NumSol  AND
       Garantias.Num_Credito   EQ TmpGar.NumCre  AND 
       Garantias.Identificacion_Bien EQ TmpGar.IdeGar NO-LOCK NO-ERROR.
  IF AVAILABLE Garantias THEN DO:
     W_Puntero = ROWID(Garantias).
     /*DISABLE {&List-1} WITH FRAME F_AdmGar.*/
     ASSIGN  Garantias.Aprobada:SCREEN-VALUE              = STRING(Garantias.Aprobada)
             Garantias.Contabilizada:SCREEN-VALUE         = STRING(Garantias.Contabilizada)
             Garantias.Estado:SCREEN-VALUE                = STRING(Garantias.Estado)
             Garantias.Fec_Creacion:SCREEN-VALUE          = STRING(Garantias.Fec_Creacion)
             Garantias.Fec_FinSeguro:SCREEN-VALUE         = STRING(Garantias.Fec_FinSeguro)
             Garantias.Fec_IniSeguro:SCREEN-VALUE         = STRING(Garantias.Fec_IniSeguro)
             Garantias.Fec_ProxAvaluo:SCREEN-VALUE        = STRING(Garantias.Fec_ProxAvaluo)
             Garantias.Fec_Retiro:SCREEN-VALUE            = STRING(Garantias.Fec_Retiro)
             Garantias.Fec_UltAvaluo:SCREEN-VALUE         = STRING(Garantias.Fec_UltAvaluo)
             Garantias.Fec_VctoImpuesto:SCREEN-VALUE      = STRING(Garantias.Fec_VctoImpuesto)
             Garantias.Identificacion_Bien:SCREEN-VALUE   = Garantias.Identificacion_Bien
             Garantias.Nit_Aseguradora:SCREEN-VALUE       = Garantias.Nit_Aseguradora
             Garantias.Nom_Bien:SCREEN-VALUE              = Garantias.Nom_Bien
             Garantias.Nom_Impuesto:SCREEN-VALUE          = Garantias.Nom_Impuesto
             Garantias.Nro_Seguro:SCREEN-VALUE            = Garantias.Nro_Seguro
             Garantias.Tipo_Garantia:SCREEN-VALUE         = STRING(Garantias.Tipo_Garantia)
             Garantias.Usuario:SCREEN-VALUE               = Garantias.Usuario
             Garantias.Val_Asegurado:SCREEN-VALUE         = STRING(Garantias.Val_Asegurado)
             Garantias.Val_Bien:SCREEN-VALUE              = STRING(Garantias.Val_Bien)
             Garantias.Val_Impuesto:SCREEN-VALUE          = STRING(Garantias.Val_Impuesto)
             Garantias.Val_UltAvaluo:SCREEN-VALUE         = STRING(Garantias.Val_UltAvaluo)
             Garantias.Id_Interna:SCREEN-VALUE            = STRING(Garantias.Id_Interna). 
     IF Garantias.Tipo_Garantia LT 4 THEN DO:
         ENABLE Garantias.Id_Interna.
         ENABLE {&List-3}.
     END.
     ELSE DO: 
         DISABLE Garantias.Id_Interna.
         DISABLE {&List-3}.
     END.
     IF Garantias.Id_Interna THEN DO:
        FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Garantias.Cod_AhorroNA NO-LOCK NO-ERROR.
        IF AVAILABLE Pro_Ahorros THEN
           Cmb_AhorrosNA:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_producto.
        /*ELSE
           Cmb_AhorrosNA:SCREEN-VALUE = "000 - Pdcto Ahorro no escogido".*/
     END.
     /*ELSE Cmb_AhorrosNA:SCREEN-VALUE = "000 - Pdcto Ahorro no escogido".*/
     
     ASSIGN Garantias.Descripcion_Bien:SCREEN-VALUE = Garantias.Descripcion_Bien
            Garantias.Descripcion_Bien2:SCREEN-VALUE = Garantias.Descripcion_Bien2.
     FIND Clientes WHERE Clientes.Nit EQ Garantias.Nit_Aseguradora NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN
        W_NomAse:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     FIND Usuarios WHERE Usuarios.Usuario EQ Garantias.Usuario NO-LOCK NO-ERROR.
     IF AVAILABLE Usuarios THEN
        W_NomUsu:SCREEN-VALUE = Usuarios.Nombre.
     ELSE
        W_NomUsu:SCREEN-VALUE = "Usuario no existe".
     FIND Agencias WHERE Agencias.Agencia EQ TmpGar.AgeGar NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN
        Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

/*     IF Garantias.Aprobada AND NOT Garantias.Contabilizada THEN DO:
        ENABLE Garantias.Contabilizada.
        Garantias.Contabilizada:LABEL = "Contabilizar?".
     END.
     IF Garantias.Aprobada AND Garantias.Contabilizada THEN DO:
        DISABLE Garantias.Contabilizada.
        ASSIGN Garantias.Contabilizada:LABEL = "Contabilizada"
               Garantias.Contabilizada:SCREEN-VALUE = "yes".
     END.
     IF Garantias.Aprobada THEN DISABLE Garantias.Aprobada.*/
     VIEW FRAME F_AdmGar.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MostrarInfoGarantiaNoAdm wWin 
PROCEDURE MostrarInfoGarantiaNoAdm :
DEFINE VAR W_Str AS CHARACTER FORMAT "X(60)" INITIAL "CodPro   NumCre    NitDeudor     NomDeudor".
DEFINE VAR W_Nom AS CHARACTER FORMAT "X(30)".
DO WITH FRAME F_AdmCod:
   ASSIGN E_Code:LIST-ITEMS = ""
          W_Ok = E_Code:ADD-LAST(W_Str)
          W_Ok = E_Code:ADD-LAST("-----------------------------------------------------------------------------------------").
   /*ciclo que llena el editor donde se muestra de quien mas es codeudor*/
   FOR EACH Relaciones WHERE 
            Relaciones.Clase_Producto EQ 2   AND
            Relaciones.Cod_Relacion   EQ 11  AND
            Relaciones.Nit_Relacion   EQ TmpCod.NitRel AND 
            Relaciones.Estado         EQ 1 NO-LOCK BREAK BY Relaciones.Cod_Producto:
       FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit NO-LOCK NO-ERROR.
       W_Nom = "Cliente No Existe".
       IF AVAILABLE Clientes THEN
          W_Nom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
       W_Str = STRING(Relaciones.Cod_Producto).
       W_Ok = E_Code:ADD-LAST(STRING(Relaciones.Cod_Producto,"999") + "      " +
                              STRING(Relaciones.Cuenta,"999999999") + " " +
                              Relaciones.Nit                  + "      " +
                              W_Nom).
   END.
   /*busca en relaciones el codeudor para mostrarlo en pantalla*/
   FIND Relaciones WHERE 
        Relaciones.Nit            EQ TmpCod.NitCli AND
        Relaciones.Cuenta         EQ STRING(TmpCod.NumCre) AND
        Relaciones.Nit_Relacion   EQ TmpCod.NitRel AND
        Relaciones.Clase_Producto EQ 2             AND
        Relaciones.Cod_Producto   EQ TmpCod.CodPro AND
        Relaciones.Cod_Relacion   EQ 11            AND
        Relaciones.Estado         EQ R_TipoGarantias NO-LOCK.
   IF AVAILABLE Relaciones THEN DO:
      FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
      W_NomCodeudor = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
      FIND Usuarios WHERE Usuario.Usuario EQ Relaciones.Usuario NO-LOCK NO-ERROR.
      W_NomUsuario = "Usuario Inconsistente".
      IF AVAILABLE Usuarios THEN W_NomUsuario = Usuarios.Nombre.
      DISPLAY W_NomUsuario W_NomCodeudor.
      ASSIGN Relaciones.Descripcion:SCREEN-VALUE   = Relaciones.Descripcion
             Relaciones.Nit_Relacion:SCREEN-VALUE  = Relaciones.Nit_Relacion
             Relaciones.Estado:SCREEN-VALUE        = STRING(Relaciones.Estado)
             Relaciones.Usuario:SCREEN-VALUE       = STRING(Relaciones.Usuario).
   END.
END.
VIEW FRAME F_AdmCod.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NuevaGarantia wWin 
PROCEDURE NuevaGarantia :
DO WITH FRAME F_AdmGar:
     ASSIGN  Garantias.Aprobada:SCREEN-VALUE              = "NO".
             Garantias.Contabilizada:SCREEN-VALUE         = "NO".
             Garantias.Estado:SCREEN-VALUE                = "1".
             Garantias.Fec_Creacion:SCREEN-VALUE          = STRING(W_Fecha).
             Garantias.Fec_FinSeguro:SCREEN-VALUE         = "?".
             Garantias.Fec_IniSeguro:SCREEN-VALUE         = "?".
             Garantias.Fec_ProxAvaluo:SCREEN-VALUE        = "?".
             Garantias.Fec_Retiro:SCREEN-VALUE            = "?".
             Garantias.Fec_UltAvaluo:SCREEN-VALUE         = "?".
             Garantias.Fec_VctoImpuesto:SCREEN-VALUE      = "?".
             Garantias.Identificacion_Bien:SCREEN-VALUE   = "".
             Garantias.Nit_Aseguradora:SCREEN-VALUE       = "".
             Garantias.Nom_Bien:SCREEN-VALUE              = "".
             Garantias.Nom_Impuesto:SCREEN-VALUE          = "".
             Garantias.Nro_Seguro:SCREEN-VALUE            = "".
             Garantias.Tipo_Garantia:SCREEN-VALUE         = "1".
             Garantias.Usuario:SCREEN-VALUE               = W_Usuario.
             Garantias.Val_Asegurado:SCREEN-VALUE         = "0".
             Garantias.Val_Bien:SCREEN-VALUE              = "0".
             Garantias.Val_Impuesto:SCREEN-VALUE          = "0".
             Garantias.Val_UltAvaluo:SCREEN-VALUE         = "0". 
         
     Garantias.Descripcion_Bien:SCREEN-VALUE = "".
     W_NomAse:SCREEN-VALUE = "".
     FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
     IF AVAILABLE Usuarios THEN
        W_NomUsu:SCREEN-VALUE = Usuarios.Nombre.
     ELSE
        W_NomUsu:SCREEN-VALUE = "Usuario no existe".
     FIND Agencias WHERE Agencias.Agencia EQ TmpCre.AgeCre NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN
        Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
     VIEW FRAME F_AdmGar.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NuevaGarantiaNoAdm wWin 
PROCEDURE NuevaGarantiaNoAdm :
DO WITH FRAME F_AdmCod:
   ASSIGN Relaciones.Nit_Relacion:SCREEN-VALUE = ""
          W_NomCodeudor:SCREEN-VALUE           = ""
          Relaciones.Estado:SCREEN-VALUE       = "1"
          Relaciones.Descripcion:SCREEN-VALUE  = "CODEUDOR".
   ENABLE {&List-6}.
END.
VIEW FRAME F_AdmCod.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParentHandle wWin 
PROCEDURE pParentHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    pParentHandle   
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER hParent AS HANDLE NO-UNDO.
    GHPARENT = hParent.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
    W_Reporte    = "REPORTE   : GARANTIAS ADMISIBLES Y NO ADMISIBLES - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna =     "".
          
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ TmpCre.CodCre NO-LOCK NO-ERROR.
  DISPLAY "Cliente      : " W_NitCli SKIP
          "Nombre       : " W_NomCli SKIP
          "Producto     : " TmpCre.CodCre " - " Pro_Creditos.Nom_Producto SKIP
          "Num.Credito  : " TmpCre.NumCre SKIP
          "Pagare       : " TmpCre.Pagare SKIP
          "Monto Inicial: " TmpCre.Monto FORMAT "->>>,>>>,>>9" SKIP
          "Saldo Actual : " TmpCre.SdoCap FORMAT "->>>,>>>,>>9" SKIP
          "--------------------------------------------------------------------------"
  WITH FRAME F_Enc1 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  FIND FIRST TmpGar NO-ERROR.
  IF AVAILABLE TmpGar THEN DO:
     DISPLAY "GARANTIAS ADMISIBLES" SKIP
             "--------------------------------------------------------------------------------" SKIP
             "TipoGarantia   Identificación    Descripcion Garantia             Valor Garantia" SKIP
             "--------------------------------------------------------------------------------"
     WITH FRAME F_EncGar WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
     FOR EACH TmpGar:
         DISPLAY TmpGar.NomTip TmpGar.IdeGar TmpGar.NomGar FORMAT "X(33)" TmpGar.ValGar 
         WITH FRAME F_MovGar WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
     END.
  END.
  FIND FIRST TmpCod NO-ERROR.
  IF AVAILABLE TmpCod THEN DO:
     DISPLAY "--------------------------------------------------------------------------------" SKIP
             "GARANTIAS NO ADMISIBLES" SKIP
             "--------------------------------------------------------------------------------" SKIP
             "Nit Codeudor      Nombre Codeudor                   Tel.Residencia Tel.Comercial" SKIP
             "--------------------------------------------------------------------------------"
     WITH FRAME F_EncCod WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
     FOR EACH TmpCod:
         DISPLAY TmpCod.NitRel TmpCod.NomCod FORMAT "X(38)" TmpCod.TelRes TmpCod.TelCom
         WITH FRAME F_MovCod WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
     END.
  END.
    
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProSolNit wWin 
PROCEDURE ProSolNit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:   ProSolNit    
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-unDO.
    GNIT = c. 
    W_NitCli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = GNIT.
    APPLY "leave" TO W_NitCli IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validaciones_Admisibles wWin 
PROCEDURE Validaciones_Admisibles :
DEFIN OUTPUT PARAMETER Err AS LOGICAL.
/*validaciones*/
DO WITH FRAME F_AdmGar:
   /* valida si se va a inactiva una garantia, que esta pueda ser inactivada
    IF Garantias.Estado:SCREEN-VALUE EQ "2" AND NOT TmpCre.ModOk THEN DO:
        Err = YES.
        MESSAGE "La garantia protege a un crédito que al momento" SKIP
                "se encuentra atrasado." SKIP(1)
                "No se permite inactivar esta garantia!!!"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Cmb_Agencia.
        RETURN NO-APPLY.
    END.*/

   /*valida que la garantia no se grabe en la agencia administrativa*/
    IF SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3) EQ "010" THEN DO:
       Err = YES.
       MESSAGE "No se pueden crear garantias admisibles en" SKIP
               "la agencia administrativa." SKIP(1)
               "Rectifique la Agencia de la Garantia!!!"
               VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO Cmb_Agencia.
       RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias con identificacion en blanco*/
    IF Garantias.Identificacion_Bien:SCREEN-VALUE EQ "" THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias sin su" SKIP
                "correspondiente código de identificación" SKIP(1)
                "Digite el número de identificación"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Identificacion_Bien.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias con nombre del bien en blanco*/
    IF Garantias.Nom_Bien:SCREEN-VALUE EQ "" THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias sin el nombre" SKIP
                "correspondiente para el bien " SKIP(1)
                "Digite el nombre del bien o garantia"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Nom_Bien.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin valor de la garantia*/
    IF DECIMAL(Garantias.Val_Bien:SCREEN-VALUE) LT 1 THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias sin llenar" SKIP
                "el campo valor de el bien " SKIP(1)
                "Digite el valor de la garantia"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Val_Bien.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin su respectiva descripcion*/
    IF Garantias.Descripcion_Bien2:SCREEN-VALUE EQ "" THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias sin llenar" SKIP
                "el campo de descripción del bien" SKIP(1)
                "Digite la descripción de la garantia"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Descripcion_Bien2.
        RETURN NO-APPLY.
    END.

    /*valida si la garantia no esta aprobada no se puede contabilizar*/
    IF Garantias.Aprobada:SCREEN-VALUE EQ "no" AND Garantias.Estado:SCREEN-VALUE EQ "1" THEN DO:
       MESSAGE "Si la garantia queda vigente(Activa) debe marcar si esta Aprobada?"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       Err = YES.
       RETURN NO-APPLY.
    END.

    IF Garantias.Tipo_Garantia:SCREEN-VALUE GT "3" THEN   /*Fin validar No-Adm*/
       RETURN.

   /*Sigen validaciones para Admisibles*/
   /*valida que no se graben garantias el nit de la aseguradora*/
    IF  Garantias.Nit_Aseguradora:SCREEN-VALUE EQ ""  
    AND Garantias.Tipo_Garantia:SCREEN-VALUE NE "3" THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias sin digitar" SKIP
                "el nit de la aseguradora" SKIP(1)
                "Digite el nit de la aseguradora"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Nit_Aseguradora.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias el numero del seguro*/
    IF Garantias.Nro_Seguro:SCREEN-VALUE EQ "" 
    AND Garantias.Tipo_Garantia:SCREEN-VALUE NE "3"   THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias sin digitar" SKIP
                "el número de seguro" SKIP(1)
                "Digite el número de seguro"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Nro_Seguro.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin el valor del seguro*/
    IF DECIMAL(Garantias.Val_Asegurado:SCREEN-VALUE) LT 1 
    AND Garantias.Tipo_Garantia:SCREEN-VALUE NE "3"  THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias con el" SKIP
                "valor del seguro en cero" SKIP(1)
                "Digite el valor de seguro"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Val_Asegurado.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin la fecha de inicio del seguro*/
    IF DATE(Garantias.Fec_IniSeguro:SCREEN-VALUE) EQ ?  
    AND Garantias.Tipo_Garantia:SCREEN-VALUE NE "3"  THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias donde no" SKIP
                "se especifique la fecha de inicio del seguro" SKIP(1)
                "Digite la fecha de inicio de seguro"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Fec_IniSeguro.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin la fecha de finalizacion del seguro*/
    IF DATE(Garantias.Fec_FinSeguro:SCREEN-VALUE) EQ ? 
    AND Garantias.Tipo_Garantia:SCREEN-VALUE NE "3"  THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias donde no" SKIP
                "se especifique la fecha de finalización del seguro" SKIP(1)
                "Digite la fecha de Finalización de seguro"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Fec_FinSeguro.
        RETURN NO-APPLY.
    END.
    IF DATE(Garantias.Fec_FinSeguro:SCREEN-VALUE) LE DATE(Garantias.Fec_IniSeguro:SCREEN-VALUE) THEN DO:
        Err = YES.
        MESSAGE "La fecha de vencimiento del seguro no puede se" SKIP
               "menor o igual a la de inicio." SKIP(1)
               "Rectifique la fecha entrada" VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO Garantias.Fec_FinSeguro IN FRAME F_AdmGar.
       RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin el valor del ultimo avaluo*/
    IF DECIMAL(Garantias.Val_UltAvaluo:SCREEN-VALUE) LT 1 THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias donde no" SKIP
                "se especifique el valor del último avaluo" SKIP(1)
                "Digite el valor del último avaluo"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Val_UltAvaluo.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin la fecha de proximo avaluo*/
    IF DATE(Garantias.Fec_ProxAvaluo:SCREEN-VALUE) EQ ? AND Garantias.Tipo_Garantia:SCREEN-VALUE NE "3" THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias donde no" SKIP
                "se especifique la fecha del proximo avaluo" SKIP(1)
                "Digite la fecha del proximo avaluo"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Fec_ProxAvaluo.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin la fecha de ultimo avaluo*/
    IF DATE(Garantias.Fec_UltAvaluo:SCREEN-VALUE) EQ ? AND Garantias.Tipo_Garantia:SCREEN-VALUE NE "3" THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias donde no" SKIP
                "se especifique la fecha del último avaluo" SKIP(1)
                "Digite la fecha del último avaluo"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Fec_UltAvaluo.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin el nombre del impuesto*/
/*    IF Garantias.Nom_Impuesto:SCREEN-VALUE EQ "" AND Garantias.Tipo_Garantia:SCREEN-VALUE NE "4" THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias donde no" SKIP
                "se especifique el nombre del impuesto" SKIP(1)
                "Digite el nombre del impuesto"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Nom_Impuesto.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin la fecha de vencimiento del impuesto*/
    IF DATE(Garantias.Fec_VctoImpuesto:SCREEN-VALUE) EQ ? AND Garantias.Tipo_Garantia:SCREEN-VALUE NE "4" THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias donde no" SKIP
                "se especifique la fecha del vencimiento del impuesto" SKIP(1)
                "Digite la fecha del vencimiento del impuesto"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Fec_VctoImpuesto.
        RETURN NO-APPLY.
    END.
    IF DATE(Garantias.Fec_VctoImpuesto:SCREEN-VALUE) LE W_Fecha THEN DO:
        Err = YES.
        MESSAGE "El impuesto de esta garantia se encuentra vencido" SKIP
                "se deberá hacer labor de actualización de la información" SKIP
                "Comuniquese con el cliente para que suministre la nueva" SKIP
                "fecha o en su defecto para que renueve el pago del impuesto"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Fec_VctoImpuesto.
        RETURN NO-APPLY.
    END.
   /*valida que no se graben garantias sin el valor del impuesto*/
    IF DECIMAL(Garantias.Val_Impuesto:SCREEN-VALUE) LT 1 AND Garantias.Tipo_Garantia:SCREEN-VALUE NE "4"  THEN DO:
        Err = YES.
        MESSAGE "No se pueden crear garantias donde no" SKIP
                "se especifique el valor del impuesto" SKIP(1)
                "Digite el valor del impuesto"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Garantias.Val_Impuesto.
        RETURN NO-APPLY.
    END.*/
    /*valida cuando es una garantia no admisible y es interna para que se haya escogiso el pdto*/
    IF Garantias.Tipo_Garantia:SCREEN-VALUE EQ "4" AND
       Garantias.Id_Interna:SCREEN-VALUE   EQ "yes" AND
       SUBSTRING(Cmb_AhorrosNA:SCREEN-VALUE,1,3) EQ "000" THEN DO:
       Err = YES.
       MESSAGE "Se ha definido la garantia como NO ADMISIBLE" SKIP
               "a demás de haber marcado esta como una cuenta" SKIP
               "interna de la cooperativa. sinembargo no se ha" SKIP
               "escogido el producto de ahorros para ligar la cuenta" SKIP(1)
               "Escoja el producto de ahorros de la cuenta entrada" SKIP
               "en el campo IDENTIFIACION DEL BIEN" VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO Cmb_AhorrosNA.
    END.
    
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validaciones_NoAdmisibles wWin 
PROCEDURE Validaciones_NoAdmisibles :
DEFINE OUTPUT PARAMETER Err AS LOGICAL.
/*validaciones*/
DO WITH FRAME F_AdmCod:
   /* valida si se va a inactiva una garantia, que esta pueda ser inactivada
    IF Relaciones.Estado:SCREEN-VALUE EQ "2" AND NOT TmpCre.ModOk THEN DO:
        Err = YES.
        MESSAGE "El Codeudor protege a un crédito que al momento" SKIP
                "se encuentra atrasado." SKIP(1)
                "No se permite inactivar este codeudor!!!"
                VIEW-AS ALERT-BOX ERROR.
    END.*/
   /*valida que el codeudor no se entre con nit en blanco*/
    IF Relaciones.Nit_Relacion:SCREEN-VALUE EQ "" THEN DO:
       Err = YES.
       MESSAGE "No se pueden crear Codeudores si no se" SKIP
               "a digitado el respectivo nit." SKIP(1)
               "Ingrese el nit del codeudor!!!"
               VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO Relaciones.Nit_Relacion.
       RETURN NO-APPLY.
    END.
   /*valida que se grabe la descripcion del codeudor*/
    IF Relaciones.Descripcion:SCREEN-VALUE EQ "" THEN DO:
        Err = YES.
        MESSAGE "No se pueden Actualizar Codeudores sin su" SKIP
                "correspondiente Descripción" SKIP(1)
                "Digite la descripción"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Relaciones.Descripcion.
        RETURN NO-APPLY.
    END.
END.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

