&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

{Incluido\Variable.i "SHARED"}

DEFINE VARIABLE W_Ofi AS INTEGER.
DEFINE VARIABLE W_Metodo AS LOGICAL.
DEFINE VARIABLE W_Val AS LOGICAL.
DEFINE VARIABLE W_Interes AS LOGICAL INITIAL TRUE.
DEFINE VARIABLE W_Asociado AS INTEGER.
DEFINE VARIABLE W_ConsCta AS CHARACTER.
DEFINE VARIABLE W_DispCta AS CHARACTER.
DEFINE VARIABLE W_Pcuenta AS CHARACTER.
DEFINE VARIABLE W_Pnombre AS CHARACTER.
DEFINE VARIABLE W_Naturaleza AS CHARACTER.
DEFINE VARIABLE W_CtrNat AS LOGICAL.
DEFINE VARIABLE crowid AS ROWID.
DEFINE VARIABLE W_Rpta AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES CarteraVencida
&Scoped-define FIRST-EXTERNAL-TABLE CarteraVencida


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR CarteraVencida.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS CarteraVencida.Cta_IntContingCr ~
CarteraVencida.Cta_IntContingDb CarteraVencida.Id_Interes ~
CarteraVencida.Cta_AsoAdDb CarteraVencida.Cta_NoaAdDb ~
CarteraVencida.Cta_AsoNaDb CarteraVencida.Cta_NoaNaDb ~
CarteraVencida.Cta_AsoIntAdDb CarteraVencida.Cta_NoaIntAdDb ~
CarteraVencida.Cta_AsoIntNaDb CarteraVencida.Cta_NoaIntNaDb ~
CarteraVencida.Cta_AsoIntAdCr CarteraVencida.Cta_NoaIntAdCr ~
CarteraVencida.Cta_AsoIntNaCr CarteraVencida.Cta_NoaIntNaCr ~
CarteraVencida.Cta_AsoPrvAdDb CarteraVencida.Cta_NoaPrvAdDb ~
CarteraVencida.Cta_AsoPrvNaDb CarteraVencida.Cta_NoaPrvNaDb ~
CarteraVencida.Cta_AsoPrvAdCr CarteraVencida.Cta_NoaPrvAdCr ~
CarteraVencida.Cta_AsoPrvNaCr CarteraVencida.Cta_NoaPrvNaCr ~
CarteraVencida.Por_ProvNoAdm CarteraVencida.Porc_DefNoHipoteca ~
CarteraVencida.Porc_Admisible CarteraVencida.Porc_DefGarantia ~
CarteraVencida.Categoria CarteraVencida.Cta_CostasCR ~
CarteraVencida.Cta_CostasDB CarteraVencida.CtaGto_ProvCos ~
CarteraVencida.CtaGto_ProvInt CarteraVencida.CtaIng_provCos ~
CarteraVencida.CtaIng_ProvInt CarteraVencida.CtaCal_Costas ~
CarteraVencida.CtaCal_Interes 
&Scoped-define ENABLED-TABLES CarteraVencida
&Scoped-define FIRST-ENABLED-TABLE CarteraVencida
&Scoped-Define ENABLED-OBJECTS Cmb_Aportes RECT-147 RECT-148 RECT-149 ~
RECT-292 RECT-293 RECT-294 
&Scoped-Define DISPLAYED-FIELDS CarteraVencida.Cta_IntContingCr ~
CarteraVencida.Cta_IntContingDb CarteraVencida.Per_Inicial ~
CarteraVencida.Per_Final CarteraVencida.Id_Interes ~
CarteraVencida.Cta_AsoAdDb CarteraVencida.Cta_NoaAdDb ~
CarteraVencida.Cta_AsoNaDb CarteraVencida.Cta_NoaNaDb ~
CarteraVencida.Cta_AsoIntAdDb CarteraVencida.Cta_NoaIntAdDb ~
CarteraVencida.Cta_AsoIntNaDb CarteraVencida.Cta_NoaIntNaDb ~
CarteraVencida.Cta_AsoIntAdCr CarteraVencida.Cta_NoaIntAdCr ~
CarteraVencida.Cta_AsoIntNaCr CarteraVencida.Cta_NoaIntNaCr ~
CarteraVencida.Cta_AsoPrvAdDb CarteraVencida.Cta_NoaPrvAdDb ~
CarteraVencida.Cta_AsoPrvNaDb CarteraVencida.Cta_NoaPrvNaDb ~
CarteraVencida.Cta_AsoPrvAdCr CarteraVencida.Cta_NoaPrvAdCr ~
CarteraVencida.Cta_AsoPrvNaCr CarteraVencida.Cta_NoaPrvNaCr ~
CarteraVencida.Por_ProvNoAdm CarteraVencida.Porc_DefNoHipoteca ~
CarteraVencida.Porc_Admisible CarteraVencida.Porc_DefGarantia ~
CarteraVencida.Categoria CarteraVencida.Cta_CostasCR ~
CarteraVencida.Cta_CostasDB CarteraVencida.CtaGto_ProvCos ~
CarteraVencida.CtaGto_ProvInt CarteraVencida.CtaIng_provCos ~
CarteraVencida.CtaIng_ProvInt CarteraVencida.CtaCal_Costas ~
CarteraVencida.CtaCal_Interes 
&Scoped-define DISPLAYED-TABLES CarteraVencida
&Scoped-define FIRST-DISPLAYED-TABLE CarteraVencida
&Scoped-Define DISPLAYED-OBJECTS Cmb_Pcto Cmb_Califica W_NomCta Cmb_Aportes 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Cmb_Pcto CarteraVencida.Per_Inicial ~
CarteraVencida.Per_Final Cmb_Califica 
&Scoped-define ADM-ASSIGN-FIELDS CarteraVencida.Cod_Producto ~
CarteraVencida.Cod_Califica CarteraVencida.Cod_Aportes 
&Scoped-define List-3 CarteraVencida.Cta_AsoAdDb CarteraVencida.Cta_AsoNaDb ~
CarteraVencida.Cta_AsoPrvAdDb CarteraVencida.Cta_AsoPrvNaDb ~
CarteraVencida.Cta_AsoPrvAdCr CarteraVencida.Cta_AsoPrvNaCr 
&Scoped-define List-4 CarteraVencida.Cta_NoaAdDb CarteraVencida.Cta_NoaNaDb ~
CarteraVencida.Cta_NoaPrvAdDb CarteraVencida.Cta_NoaPrvNaDb ~
CarteraVencida.Cta_NoaPrvAdCr CarteraVencida.Cta_NoaPrvNaCr 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
Cod_Producto|y|y|Datos.CarteraVencida.Cod_Producto
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "Cod_Producto",
     Keys-Supplied = "Cod_Producto"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE Cmb_Aportes AS CHARACTER FORMAT "X(20)":U 
     LABEL "Cod. Aportes" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 31 BY 1 TOOLTIP "Codigo Aportes"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Califica AS CHARACTER FORMAT "X(30)":U 
     LABEL "Calificacion" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1 TOOLTIP "Calificación de la Cartera"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Pcto AS CHARACTER FORMAT "X(40)":U 
     LABEL "Productos de Crédito" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1 TOOLTIP "Productos de Créditos Disponibles"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCta AS CHARACTER FORMAT "X(40)":U 
     LABEL "Nombre Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-147
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 6.96.

DEFINE RECTANGLE RECT-148
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 6.92.

DEFINE RECTANGLE RECT-149
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 4.04.

DEFINE RECTANGLE RECT-292
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 3.58.

DEFINE RECTANGLE RECT-293
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44.86 BY 4.04.

DEFINE RECTANGLE RECT-294
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 2.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CarteraVencida.Cta_IntContingCr AT ROW 17.77 COL 20.72 COLON-ALIGNED NO-LABEL FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81 TOOLTIP "Interes Contingente Credito"
          BGCOLOR 15 
     CarteraVencida.Cta_IntContingDb AT ROW 17.77 COL 2.86 COLON-ALIGNED NO-LABEL FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 13.72 BY .81
          BGCOLOR 15 
     Cmb_Pcto AT ROW 1.27 COL 1
     CarteraVencida.Cod_Producto AT ROW 2.35 COL 86 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
     CarteraVencida.Per_Inicial AT ROW 1.27 COL 56 COLON-ALIGNED
          LABEL "Periodo Inicial"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     CarteraVencida.Per_Final AT ROW 1.27 COL 77 COLON-ALIGNED
          LABEL "Periodo Final"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Cmb_Califica AT ROW 2.15 COL 14 COLON-ALIGNED
     CarteraVencida.Id_Interes AT ROW 2.35 COL 46
          LABEL "Provisiona Intereses?"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .77
     CarteraVencida.Cod_Califica AT ROW 2.35 COL 68 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
     CarteraVencida.Cta_AsoAdDb AT ROW 5.08 COL 11 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_NoaAdDb AT ROW 5.08 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_AsoNaDb AT ROW 5.08 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_NoaNaDb AT ROW 5.08 COL 70 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_AsoIntAdDb AT ROW 6.65 COL 9 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_NoaIntAdDb AT ROW 6.65 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_AsoIntNaDb AT ROW 6.73 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_NoaIntNaDb AT ROW 6.73 COL 70 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_AsoIntAdCr AT ROW 7.54 COL 9 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_NoaIntAdCr AT ROW 7.54 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_AsoIntNaCr AT ROW 7.65 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_NoaIntNaCr AT ROW 7.62 COL 70 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_AsoPrvAdDb AT ROW 9.19 COL 9 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_NoaPrvAdDb AT ROW 9.15 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     CarteraVencida.Cta_AsoPrvNaDb AT ROW 9.35 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_NoaPrvNaDb AT ROW 9.35 COL 70 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_AsoPrvAdCr AT ROW 10.08 COL 9 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_NoaPrvAdCr AT ROW 10.04 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_AsoPrvNaCr AT ROW 10.23 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.Cta_NoaPrvNaCr AT ROW 10.23 COL 70 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     W_NomCta AT ROW 19.08 COL 13.14 COLON-ALIGNED
     Cmb_Aportes AT ROW 20.12 COL 13 COLON-ALIGNED
     CarteraVencida.Por_ProvNoAdm AT ROW 17.08 COL 79 COLON-ALIGNED
          LABEL "% Pro. No Adm."
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     CarteraVencida.Porc_DefNoHipoteca AT ROW 18.96 COL 79 COLON-ALIGNED
          LABEL "% Deducible.No Hipoteca"
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     CarteraVencida.Cod_Aportes AT ROW 20.12 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
     CarteraVencida.Porc_Admisible AT ROW 18.04 COL 79 COLON-ALIGNED
          LABEL "% Provisión"
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     CarteraVencida.Porc_DefGarantia AT ROW 19.85 COL 79 COLON-ALIGNED
          LABEL "% Deducible Hipoteca"
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     CarteraVencida.Categoria AT ROW 3 COL 14 COLON-ALIGNED
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "A","B","C","D","E" 
          DROP-DOWN-LIST
          SIZE 4.72 BY 1
          BGCOLOR 15 
     CarteraVencida.Cta_CostasCR AT ROW 12.08 COL 16.14 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 14 BY .77
          BGCOLOR 15 
     CarteraVencida.Cta_CostasDB AT ROW 13.12 COL 16.14 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     CarteraVencida.CtaGto_ProvCos AT ROW 12.85 COL 70 COLON-ALIGNED
          LABEL "Gasto Prov. Costas/Honorarios/Polizas"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.CtaGto_ProvInt AT ROW 13.65 COL 70 COLON-ALIGNED
          LABEL "Gasto Provision Interes"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.CtaIng_provCos AT ROW 14.46 COL 70 COLON-ALIGNED
          LABEL "Ingreso.Prov.Costas/Honorarios/Poliz"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     CarteraVencida.CtaIng_ProvInt AT ROW 15.27 COL 70 COLON-ALIGNED
          LABEL "Ingreso Provision Interes"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     CarteraVencida.CtaCal_Costas AT ROW 14.19 COL 16.14 COLON-ALIGNED
          LABEL "Calificación"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     CarteraVencida.CtaCal_Interes AT ROW 15.38 COL 16 COLON-ALIGNED
          LABEL "Calificación Interes"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     "Garantía No Admisible" VIEW-AS TEXT
          SIZE 20 BY 1.08 AT ROW 3.69 COL 50
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Débitos" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 5.08 COL 49
          BGCOLOR 17 FGCOLOR 0 
     "Prov. Intereses Garantía No Admisible" VIEW-AS TEXT
          SIZE 33 BY .81 AT ROW 5.92 COL 55
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Créditos" VIEW-AS TEXT
          SIZE 6.14 BY .54 AT ROW 17.19 COL 26
          BGCOLOR 17 FGCOLOR 0 
     "Prov. Intereses Garantía Admisible" VIEW-AS TEXT
          SIZE 34 BY .54 AT ROW 6 COL 11
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Gasto e Ingreso Intereses y Otras Prov" VIEW-AS TEXT
          SIZE 33.86 BY .81 AT ROW 11.96 COL 45.72
          FGCOLOR 7 FONT 5
     "Débitos" VIEW-AS TEXT
          SIZE 7 BY .81 AT ROW 6.65 COL 4
          BGCOLOR 17 FGCOLOR 0 
     "Débitos" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 6.77 COL 49
          BGCOLOR 17 FGCOLOR 0 
     "Ctas-Contingentes de Intereses" VIEW-AS TEXT
          SIZE 27.29 BY .73 AT ROW 16.42 COL 4.72
          FGCOLOR 7 FONT 5
     "Créditos" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 7.58 COL 49
          BGCOLOR 17 FGCOLOR 0 
     "Débitos" VIEW-AS TEXT
          SIZE 7 BY .81 AT ROW 9.15 COL 4
          BGCOLOR 17 FGCOLOR 0 
     "Débitos" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 9.35 COL 49
          BGCOLOR 17 FGCOLOR 0 
     "Provision K Garantía Admisible" VIEW-AS TEXT
          SIZE 28 BY .54 AT ROW 8.5 COL 11
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Provision K Garantía No Admisible" VIEW-AS TEXT
          SIZE 33 BY .81 AT ROW 8.54 COL 55
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Provisión Costas/Polizas/Honorarios" VIEW-AS TEXT
          SIZE 32 BY .73 AT ROW 11.38 COL 5
          FGCOLOR 7 FONT 5
     "Débitos" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 5.08 COL 4
          BGCOLOR 17 FGCOLOR 0 
     "Créditos" VIEW-AS TEXT
          SIZE 7 BY .81 AT ROW 7.54 COL 4
          BGCOLOR 17 FGCOLOR 0 
     "Créditos" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 10.19 COL 49
          BGCOLOR 17 FGCOLOR 0 
     "Débitos" VIEW-AS TEXT
          SIZE 6 BY .65 AT ROW 17.12 COL 8.72
          BGCOLOR 17 FGCOLOR 0 
     "Créditos" VIEW-AS TEXT
          SIZE 7 BY .81 AT ROW 10.08 COL 4
          BGCOLOR 17 FGCOLOR 0 
     " Porcentajes" VIEW-AS TEXT
          SIZE 11 BY 1.08 AT ROW 16.35 COL 58.72
          FGCOLOR 7 FONT 5
     "Garantía Admisible" VIEW-AS TEXT
          SIZE 17 BY .54 AT ROW 3.96 COL 7
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Con Libranza                Sin Libranza" VIEW-AS TEXT
          SIZE 33 BY .54 AT ROW 4.5 COL 11
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Con Libranza              Sin Libranza" VIEW-AS TEXT
          SIZE 30 BY .54 AT ROW 4.5 COL 55
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-147 AT ROW 4.23 COL 48
     RECT-148 AT ROW 4.23 COL 3
     RECT-149 AT ROW 16.88 COL 58
     RECT-292 AT ROW 11.65 COL 3
     RECT-293 AT ROW 12.31 COL 44.14
     RECT-294 AT ROW 16.62 COL 3.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: Datos.CarteraVencida
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 20.12
         WIDTH              = 89.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Califica IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX Cmb_Pcto IN FRAME F-Main
   NO-ENABLE ALIGN-L 1                                                  */
/* SETTINGS FOR FILL-IN CarteraVencida.Cod_Aportes IN FRAME F-Main
   NO-DISPLAY NO-ENABLE 2                                               */
ASSIGN 
       CarteraVencida.Cod_Aportes:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN CarteraVencida.Cod_Califica IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L 2                                       */
ASSIGN 
       CarteraVencida.Cod_Califica:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN CarteraVencida.Cod_Producto IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L 2                                       */
ASSIGN 
       CarteraVencida.Cod_Producto:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN CarteraVencida.CtaCal_Costas IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.CtaCal_Interes IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.CtaGto_ProvCos IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.CtaGto_ProvInt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.CtaIng_provCos IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.CtaIng_ProvInt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_AsoAdDb IN FRAME F-Main
   ALIGN-L 3                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_AsoNaDb IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_AsoPrvAdCr IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_AsoPrvAdDb IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_AsoPrvNaCr IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_AsoPrvNaDb IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_CostasCR IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_CostasDB IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_IntContingCr IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_IntContingDb IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_NoaAdDb IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_NoaIntAdCr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_NoaIntAdDb IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_NoaIntNaCr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_NoaIntNaDb IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_NoaNaDb IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_NoaPrvAdCr IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_NoaPrvAdDb IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_NoaPrvNaCr IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN CarteraVencida.Cta_NoaPrvNaDb IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX CarteraVencida.Id_Interes IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Per_Final IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN CarteraVencida.Per_Inicial IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN CarteraVencida.Porc_Admisible IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Porc_DefGarantia IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Porc_DefNoHipoteca IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CarteraVencida.Por_ProvNoAdm IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_NomCta IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Cmb_Aportes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Aportes V-table-Win
ON VALUE-CHANGED OF Cmb_Aportes IN FRAME F-Main /* Cod. Aportes */
DO:
    CarteraVencida.Cod_Aportes:SCREEN-VALUE = SUBSTRING(Cmb_Aportes:SCREEN-VALUE,1,3).
    APPLY "LEAVE":U TO CarteraVencida.Cod_Aportes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Califica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Califica V-table-Win
ON ENTRY OF Cmb_Califica IN FRAME F-Main /* Calificacion */
DO:
    APPLY "VALUE-CHANGED":U TO Cmb_Califica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Califica V-table-Win
ON VALUE-CHANGED OF Cmb_Califica IN FRAME F-Main /* Calificacion */
DO:
    CarteraVencida.Cod_Califica:SCREEN-VALUE = SUBSTRING(Cmb_Califica:SCREEN-VALUE,1,5). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Pcto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Pcto V-table-Win
ON ENTRY OF Cmb_Pcto IN FRAME F-Main /* Productos de Crédito */
DO:
    APPLY "VALUE-CHANGED":U TO Cmb_Pcto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Pcto V-table-Win
ON VALUE-CHANGED OF Cmb_Pcto IN FRAME F-Main /* Productos de Crédito */
DO:
    IF AVAILABLE(CarteraVencida) THEN DO:
        ASSIGN CarteraVencida.Cod_Producto:SCREEN-VALUE = SUBSTRING(Cmb_Pcto:SCREEN-VALUE,1,3)
               W_NomCta:SCREEN-VALUE = "".

        FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ INTEGER(CarteraVencida.Cod_Producto:SCREEN-VALUE)
                                  AND Pro_Creditos.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Pro_Creditos THEN
            W_Asociado = Pro_Creditos.Id_Asociado.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cod_Aportes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cod_Aportes V-table-Win
ON LEAVE OF CarteraVencida.Cod_Aportes IN FRAME F-Main /* Código Aportes */
DO:
    IF INTEGER(CarteraVencida.Cod_Aportes:SCREEN-VALUE) EQ 0 THEN
       ASSIGN CarteraVencida.Porc_DefNoHipoteca:SENSITIVE    = FALSE
              CarteraVencida.Porc_DefNoHipoteca:SCREEN-VALUE = "0".
    ELSE
       CarteraVencida.Porc_DefNoHipoteca:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.CtaCal_Costas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaCal_Costas V-table-Win
ON ENTRY OF CarteraVencida.CtaCal_Costas IN FRAME F-Main /* Calificación */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaCal_Costas V-table-Win
ON LEAVE OF CarteraVencida.CtaCal_Costas IN FRAME F-Main /* Calificación */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaCal_Costas V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.CtaCal_Costas IN FRAME F-Main /* Calificación */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.CtaCal_Interes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaCal_Interes V-table-Win
ON ENTRY OF CarteraVencida.CtaCal_Interes IN FRAME F-Main /* Calificación Interes */
DO:
   W_ConsCta = SELF:SCREEN-VALUE. 
   RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaCal_Interes V-table-Win
ON LEAVE OF CarteraVencida.CtaCal_Interes IN FRAME F-Main /* Calificación Interes */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaCal_Interes V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.CtaCal_Interes IN FRAME F-Main /* Calificación Interes */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.CtaGto_ProvCos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaGto_ProvCos V-table-Win
ON ENTRY OF CarteraVencida.CtaGto_ProvCos IN FRAME F-Main /* Gasto Prov. Costas/Honorarios/Polizas */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaGto_ProvCos V-table-Win
ON LEAVE OF CarteraVencida.CtaGto_ProvCos IN FRAME F-Main /* Gasto Prov. Costas/Honorarios/Polizas */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaGto_ProvCos V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.CtaGto_ProvCos IN FRAME F-Main /* Gasto Prov. Costas/Honorarios/Polizas */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.CtaGto_ProvInt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaGto_ProvInt V-table-Win
ON ENTRY OF CarteraVencida.CtaGto_ProvInt IN FRAME F-Main /* Gasto Provision Interes */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaGto_ProvInt V-table-Win
ON LEAVE OF CarteraVencida.CtaGto_ProvInt IN FRAME F-Main /* Gasto Provision Interes */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaGto_ProvInt V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.CtaGto_ProvInt IN FRAME F-Main /* Gasto Provision Interes */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.CtaIng_provCos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaIng_provCos V-table-Win
ON ENTRY OF CarteraVencida.CtaIng_provCos IN FRAME F-Main /* Ingreso.Prov.Costas/Honorarios/Poliz */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaIng_provCos V-table-Win
ON LEAVE OF CarteraVencida.CtaIng_provCos IN FRAME F-Main /* Ingreso.Prov.Costas/Honorarios/Poliz */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaIng_provCos V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.CtaIng_provCos IN FRAME F-Main /* Ingreso.Prov.Costas/Honorarios/Poliz */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.CtaIng_ProvInt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaIng_ProvInt V-table-Win
ON ENTRY OF CarteraVencida.CtaIng_ProvInt IN FRAME F-Main /* Ingreso Provision Interes */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaIng_ProvInt V-table-Win
ON LEAVE OF CarteraVencida.CtaIng_ProvInt IN FRAME F-Main /* Ingreso Provision Interes */
DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.CtaIng_ProvInt V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.CtaIng_ProvInt IN FRAME F-Main /* Ingreso Provision Interes */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_AsoAdDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoAdDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_AsoAdDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoAdDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoAdDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_AsoAdDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoAdDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoAdDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_AsoAdDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoAdDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_AsoAdDb:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_AsoIntAdCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntAdCr V-table-Win
ON ENTRY OF CarteraVencida.Cta_AsoIntAdCr IN FRAME F-Main /* Crédito P.Interes Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoIntAdCr:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntAdCr V-table-Win
ON LEAVE OF CarteraVencida.Cta_AsoIntAdCr IN FRAME F-Main /* Crédito P.Interes Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoIntAdCr:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntAdCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_AsoIntAdCr IN FRAME F-Main /* Crédito P.Interes Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoIntAdCr:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_AsoIntAdCr:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_AsoIntAdDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntAdDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_AsoIntAdDb IN FRAME F-Main /* Débito P.Interes Aso. G.Admisible */
DO:
    ASSIGN W_ConsCta = CarteraVencida.Cta_AsoIntAdDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntAdDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_AsoIntAdDb IN FRAME F-Main /* Débito P.Interes Aso. G.Admisible */
DO:
    ASSIGN W_ConsCta = CarteraVencida.Cta_AsoIntAdDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntAdDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_AsoIntAdDb IN FRAME F-Main /* Débito P.Interes Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoIntAdDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_AsoIntAdDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_AsoIntNaCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntNaCr V-table-Win
ON ENTRY OF CarteraVencida.Cta_AsoIntNaCr IN FRAME F-Main /* Crédito P.Interes Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoIntNaCr:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntNaCr V-table-Win
ON LEAVE OF CarteraVencida.Cta_AsoIntNaCr IN FRAME F-Main /* Crédito P.Interes Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoIntNaCr:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntNaCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_AsoIntNaCr IN FRAME F-Main /* Crédito P.Interes Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoIntNaCr:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_AsoIntNaCr:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_AsoIntNaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntNaDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_AsoIntNaDb IN FRAME F-Main /* Débito P.Interes Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoIntNaDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntNaDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_AsoIntNaDb IN FRAME F-Main /* Débito P.Interes Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoIntNaDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoIntNaDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_AsoIntNaDb IN FRAME F-Main /* Débito P.Interes Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoIntNaDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_AsoIntNaDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_AsoNaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoNaDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_AsoNaDb IN FRAME F-Main /* Débito Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoNaDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoNaDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_AsoNaDb IN FRAME F-Main /* Débito Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoNaDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoNaDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_AsoNaDb IN FRAME F-Main /* Débito Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoNaDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_AsoNaDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_AsoPrvAdCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvAdCr V-table-Win
ON ENTRY OF CarteraVencida.Cta_AsoPrvAdCr IN FRAME F-Main /* Cuenta Crédito */
DO:
    ASSIGN W_ConsCta = CarteraVencida.Cta_AsoPrvAdCr:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvAdCr V-table-Win
ON LEAVE OF CarteraVencida.Cta_AsoPrvAdCr IN FRAME F-Main /* Cuenta Crédito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvAdCr:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvAdCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_AsoPrvAdCr IN FRAME F-Main /* Cuenta Crédito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvAdCr:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_AsoPrvAdCr:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_AsoPrvAdDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvAdDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_AsoPrvAdDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvAdDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvAdDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_AsoPrvAdDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvAdDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvAdDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_AsoPrvAdDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvAdDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_AsoPrvAdDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_AsoPrvNaCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvNaCr V-table-Win
ON ENTRY OF CarteraVencida.Cta_AsoPrvNaCr IN FRAME F-Main /* Cuenta Crédito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvNaCr:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvNaCr V-table-Win
ON LEAVE OF CarteraVencida.Cta_AsoPrvNaCr IN FRAME F-Main /* Cuenta Crédito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvNaCr:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvNaCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_AsoPrvNaCr IN FRAME F-Main /* Cuenta Crédito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvNaCr:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_AsoPrvNaCr:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_AsoPrvNaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvNaDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_AsoPrvNaDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvNaDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvNaDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_AsoPrvNaDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvNaDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_AsoPrvNaDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_AsoPrvNaDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_AsoPrvNaDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_AsoPrvNaDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_CostasCR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_CostasCR V-table-Win
ON ENTRY OF CarteraVencida.Cta_CostasCR IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = CarteraVencida.Cta_CostasCr:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_CostasCR V-table-Win
ON LEAVE OF CarteraVencida.Cta_CostasCR IN FRAME F-Main /* Crédito */
DO:
    W_ConsCta = CarteraVencida.Cta_CostasCR:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_CostasCR V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_CostasCR IN FRAME F-Main /* Crédito */
DO:
    W_ConsCta = CarteraVencida.Cta_CostasCr:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_CostasCr:SCREEN-VALUE = W_DispCta.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_CostasDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_CostasDB V-table-Win
ON ENTRY OF CarteraVencida.Cta_CostasDB IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = CarteraVencida.Cta_CostasDB:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_CostasDB V-table-Win
ON LEAVE OF CarteraVencida.Cta_CostasDB IN FRAME F-Main /* Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_CostasDB:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_CostasDB V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_CostasDB IN FRAME F-Main /* Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_CostasDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_CostasDb:SCREEN-VALUE = W_DispCta.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_IntContingCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_IntContingCr V-table-Win
ON ENTRY OF CarteraVencida.Cta_IntContingCr IN FRAME F-Main /* Cta_IntContingCr */
DO:
   ASSIGN W_ConsCta = CarteraVencida.Cta_IntContingCR:SCREEN-VALUE. 
   RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_IntContingCr V-table-Win
ON LEAVE OF CarteraVencida.Cta_IntContingCr IN FRAME F-Main /* Cta_IntContingCr */
DO:
   ASSIGN W_ConsCta = CarteraVencida.Cta_IntContingCr:SCREEN-VALUE. 
   RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_IntContingCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_IntContingCr IN FRAME F-Main /* Cta_IntContingCr */
DO:
   W_ConsCta = CarteraVencida.Cta_IntContingCr:SCREEN-VALUE.
   RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
   CarteraVencida.Cta_IntContingCr:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_IntContingDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_IntContingDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_IntContingDb IN FRAME F-Main /* Cta_IntContingDb */
DO:
   ASSIGN W_ConsCta = CarteraVencida.Cta_IntContingDb:SCREEN-VALUE. 
   RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_IntContingDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_IntContingDb IN FRAME F-Main /* Cta_IntContingDb */
DO:
   ASSIGN W_ConsCta = CarteraVencida.Cta_IntContingDb:SCREEN-VALUE. 
   RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_IntContingDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_IntContingDb IN FRAME F-Main /* Cta_IntContingDb */
DO:
   W_ConsCta = CarteraVencida.Cta_IntContingDb:SCREEN-VALUE.
   RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
   CarteraVencida.Cta_IntContingDb:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_NoaAdDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaAdDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_NoaAdDb IN FRAME F-Main /* Débito No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaAdDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaAdDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_NoaAdDb IN FRAME F-Main /* Débito No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaAdDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaAdDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_NoaAdDb IN FRAME F-Main /* Débito No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaAdDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_NoaAdDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_NoaIntAdCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntAdCr V-table-Win
ON ENTRY OF CarteraVencida.Cta_NoaIntAdCr IN FRAME F-Main /* Crédito P.Interes No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntAdCr:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntAdCr V-table-Win
ON LEAVE OF CarteraVencida.Cta_NoaIntAdCr IN FRAME F-Main /* Crédito P.Interes No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntAdCr:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntAdCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_NoaIntAdCr IN FRAME F-Main /* Crédito P.Interes No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntAdCr:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_NoaIntAdCr:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_NoaIntAdDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntAdDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_NoaIntAdDb IN FRAME F-Main /* Débito P.Interes No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntAdDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntAdDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_NoaIntAdDb IN FRAME F-Main /* Débito P.Interes No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntAdDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntAdDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_NoaIntAdDb IN FRAME F-Main /* Débito P.Interes No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntAdDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_NoaIntAdDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_NoaIntNaCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntNaCr V-table-Win
ON ENTRY OF CarteraVencida.Cta_NoaIntNaCr IN FRAME F-Main /* Crédito P.Interes No Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntNaCr:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntNaCr V-table-Win
ON LEAVE OF CarteraVencida.Cta_NoaIntNaCr IN FRAME F-Main /* Crédito P.Interes No Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntNaCr:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntNaCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_NoaIntNaCr IN FRAME F-Main /* Crédito P.Interes No Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntNaCr:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_NoaIntNaCr:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_NoaIntNaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntNaDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_NoaIntNaDb IN FRAME F-Main /* Débito P.Interes No Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntNaDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntNaDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_NoaIntNaDb IN FRAME F-Main /* Débito P.Interes No Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntNaDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaIntNaDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_NoaIntNaDb IN FRAME F-Main /* Débito P.Interes No Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaIntNaDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_NoaIntNaDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_NoaNaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaNaDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_NoaNaDb IN FRAME F-Main /* Débito No Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaNaDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaNaDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_NoaNaDb IN FRAME F-Main /* Débito No Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaNaDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaNaDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_NoaNaDb IN FRAME F-Main /* Débito No Aso. G.No Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaNaDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_NoaNaDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_NoaPrvAdCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvAdCr V-table-Win
ON ENTRY OF CarteraVencida.Cta_NoaPrvAdCr IN FRAME F-Main /* Crédito Provisión No Aso. G. Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvAdCr:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvAdCr V-table-Win
ON LEAVE OF CarteraVencida.Cta_NoaPrvAdCr IN FRAME F-Main /* Crédito Provisión No Aso. G. Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvAdCr:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvAdCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_NoaPrvAdCr IN FRAME F-Main /* Crédito Provisión No Aso. G. Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvAdCr:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_NoaPrvAdCr:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_NoaPrvAdDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvAdDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_NoaPrvAdDb IN FRAME F-Main /* Débito Provisión No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvAdDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvAdDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_NoaPrvAdDb IN FRAME F-Main /* Débito Provisión No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvAdDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvAdDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_NoaPrvAdDb IN FRAME F-Main /* Débito Provisión No Aso. G.Admisible */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvAdDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_NoaPrvAdDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_NoaPrvNaCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvNaCr V-table-Win
ON ENTRY OF CarteraVencida.Cta_NoaPrvNaCr IN FRAME F-Main /* Cuenta Crédito */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvNaCr:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvNaCr V-table-Win
ON LEAVE OF CarteraVencida.Cta_NoaPrvNaCr IN FRAME F-Main /* Cuenta Crédito */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvNaCr:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvNaCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_NoaPrvNaCr IN FRAME F-Main /* Cuenta Crédito */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvNacr:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_NoaPrvNaCr:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Cta_NoaPrvNaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvNaDb V-table-Win
ON ENTRY OF CarteraVencida.Cta_NoaPrvNaDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvNaDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvNaDb V-table-Win
ON LEAVE OF CarteraVencida.Cta_NoaPrvNaDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvNaDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Cta_NoaPrvNaDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CarteraVencida.Cta_NoaPrvNaDb IN FRAME F-Main /* Cuenta Débito */
DO:
    W_ConsCta = CarteraVencida.Cta_NoaPrvNaDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    CarteraVencida.Cta_NoaPrvNaDb:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Id_Interes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Id_Interes V-table-Win
ON VALUE-CHANGED OF CarteraVencida.Id_Interes IN FRAME F-Main /* Provisiona Intereses? */
DO:
   RUN Activa_Ctas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Per_Final
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Per_Final V-table-Win
ON LEAVE OF CarteraVencida.Per_Final IN FRAME F-Main /* Periodo Final */
DO:
    IF CarteraVencida.Per_Final:SCREEN-VALUE EQ "?" THEN
       CarteraVencida.Per_Final:SCREEN-VALUE = "0". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CarteraVencida.Per_Inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CarteraVencida.Per_Inicial V-table-Win
ON LEAVE OF CarteraVencida.Per_Inicial IN FRAME F-Main /* Periodo Inicial */
DO:
    IF CarteraVencida.Per_Inicial:SCREEN-VALUE EQ "?" THEN
       CarteraVencida.Per_Inicial:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Activa_Ctas V-table-Win 
PROCEDURE Activa_Ctas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&Frame-Name} :
       IF CarteraVencida.Id_Interes:SCREEN-VALUE EQ "Si" THEN
          ASSIGN CarteraVencida.Cta_AsoIntAdDb:SENSITIVE = TRUE
                 CarteraVencida.Cta_NoaIntAdDb:SENSITIVE = TRUE
                 CarteraVencida.Cta_AsoIntNaDb:SENSITIVE = TRUE
                 CarteraVencida.Cta_NoaIntNaDb:SENSITIVE = TRUE
                 CarteraVencida.Cta_AsoIntAdCr:SENSITIVE = TRUE
                 CarteraVencida.Cta_NoaIntAdCr:SENSITIVE = TRUE
                 CarteraVencida.Cta_AsoIntNaCr:SENSITIVE = TRUE
                 CarteraVencida.Cta_NoaIntNaCr:SENSITIVE = TRUE.
       ELSE
          ASSIGN CarteraVencida.Cta_AsoIntAdDb:SCREEN-VALUE = ""
                 CarteraVencida.Cta_NoaIntAdDb:SCREEN-VALUE = ""
                 CarteraVencida.Cta_AsoIntNaDb:SCREEN-VALUE = ""
                 CarteraVencida.Cta_NoaIntNaDb:SCREEN-VALUE = ""
                 CarteraVencida.Cta_AsoIntAdCr:SCREEN-VALUE = ""
                 CarteraVencida.Cta_NoaIntAdCr:SCREEN-VALUE = ""
                 CarteraVencida.Cta_AsoIntNaCr:SCREEN-VALUE = ""
                 CarteraVencida.Cta_NoaIntNaCr:SCREEN-VALUE = ""
                 CarteraVencida.Cta_AsoIntAdDb:SENSITIVE    = FALSE
                 CarteraVencida.Cta_NoaIntAdDb:SENSITIVE    = FALSE
                 CarteraVencida.Cta_AsoIntNaDb:SENSITIVE    = FALSE
                 CarteraVencida.Cta_NoaIntNaDb:SENSITIVE    = FALSE
                 CarteraVencida.Cta_AsoIntAdCr:SENSITIVE    = FALSE
                 CarteraVencida.Cta_NoaIntAdCr:SENSITIVE    = FALSE
                 CarteraVencida.Cta_AsoIntNaCr:SENSITIVE    = FALSE
                 CarteraVencida.Cta_NoaIntNaCr:SENSITIVE    = FALSE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "CarteraVencida"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "CarteraVencida"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConCtaCble V-table-Win 
PROCEDURE ConCtaCble :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER T_ConsCta LIKE Cuentas.Cuenta.
 
    DO WITH FRAME {&FRAME-NAME}:
       IF T_ConsCta NE "" THEN DO:
          FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta
                         AND Cuentas.Tipo   EQ 2 
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
          IF NOT AVAILABLE(Cuentas) THEN
             APPLY "MOUSE-SELECT-DBLCLICK":U TO SELF.
       END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cuentasentry V-table-Win 
PROCEDURE Cuentasentry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER T_ConsCta LIKE Cuentas.Cuenta.
    DO WITH FRAME {&FRAME-NAME}:
       FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta 
                      AND Cuentas.Tipo   EQ 2 
                      AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
       IF AVAILABLE(Cuentas) THEN
          W_NomCta:SCREEN-VALUE = Cuentas.Nombre.
       ELSE
          W_NomCta:SCREEN-VALUE = "".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ) .
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
    ASSIGN Cmb_Pcto:SENSITIVE IN FRAME {&FRAME-NAME}  = TRUE
           CarteraVencida.Per_Inicial:SENSITIVE       = TRUE
           CarteraVencida.Per_Final:SENSITIVE         = TRUE
           Cmb_Califica:SENSITIVE                     = TRUE.
    APPLY "ENTRY":U TO Cmb_Pcto.
    ASSIGN CarteraVencida.Id_Interes:SCREEN-VALUE     = "si"
           CarteraVencida.Cta_NoaIntAdDb:SCREEN-VALUE = "" 
           CarteraVencida.Cta_NoaIntNaDb:SCREEN-VALUE = ""
           CarteraVencida.Cta_NoaIntAdCr:SCREEN-VALUE = ""
           CarteraVencida.Cta_NoaIntNaCr:SCREEN-VALUE = "" 
           CarteraVencida.Cta_AsoIntAdDb:SCREEN-VALUE = "" 
           CarteraVencida.Cta_AsoIntNaDb:SCREEN-VALUE = "" 
           CarteraVencida.Cta_AsoIntAdCr:SCREEN-VALUE = "" 
           CarteraVencida.Cta_AsoIntNaCr:SCREEN-VALUE = ""
           CarteraVencida.Per_Inicial:SCREEN-VALUE    = ""
           CarteraVencida.Per_Final:SCREEN-VALUE      = "".
    RUN Activa_Ctas.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
       ASSIGN CarteraVencida.Cod_Producto:SCREEN-VALUE = SUBSTRING(Cmb_Pcto:SCREEN-VALUE,1,3)
              CarteraVencida.Cod_Califica:SCREEN-VALUE = SUBSTRING(Cmb_Califica:SCREEN-VALUE,1,5)
              Crowid                                   = ROWID(CarteraVencida).
       FIND CarteraVencida 
            WHERE (CarteraVencida.Cod_Producto             EQ INTEGER(CarteraVencida.Cod_Producto:SCREEN-VALUE) 
              AND  INTEGER(CarteraVencida.Per_Inicial:SCREEN-VALUE) GE CarteraVencida.Per_Inicial
              AND  INTEGER(CarteraVencida.Per_Inicial:SCREEN-VALUE) LE CarteraVencida.Per_Final
              AND  ROWID(CarteraVencida) NE Crowid)
               OR (CarteraVencida.Cod_Producto             EQ INTEGER(CarteraVencida.Cod_Producto:SCREEN-VALUE)
              AND  INTEGER(CarteraVencida.Per_Final:SCREEN-VALUE) GE CarteraVencida.Per_Inicial
              AND  INTEGER(CarteraVencida.Per_Final:SCREEN-VALUE) LE CarteraVencida.Per_Final
              AND  ROWID(CarteraVencida) NE Crowid)
               OR (CarteraVencida.Cod_Producto             EQ INTEGER(CarteraVencida.Cod_Producto:SCREEN-VALUE)
              AND  INTEGER(CarteraVencida.Per_Inicial:SCREEN-VALUE) LE CarteraVencida.Per_Inicial
              AND  INTEGER(CarteraVencida.Per_Final:SCREEN-VALUE) GE CarteraVencida.Per_Final
              AND  ROWID(CarteraVencida) NE Crowid)
            NO-LOCK NO-ERROR.
       IF AVAILABLE(CarteraVencida) THEN DO:  
          RUN MostrarMensaje IN W_Manija (INPUT 390, OUTPUT W_Rpta).
          RETURN ERROR.
       END.

       FIND CarteraVencida 
            WHERE CarteraVencida.Cod_Producto EQ INTEGER(CarteraVencida.Cod_Producto:SCREEN-VALUE)
              AND CarteraVencida.Cod_Califica EQ INTEGER(CarteraVencida.Cod_Califica:SCREEN-VALUE)
              AND ROWID(CarteraVencida) NE Crowid
            NO-LOCK NO-ERROR.
       IF AVAILABLE(CarteraVencida) THEN DO:
          RUN MostrarMensaje IN W_Manija (INPUT 391, OUTPUT W_Rpta).  
          FIND CarteraVencida WHERE ROWID(CarteraVencida) EQ crowid 
                              NO-LOCK NO-ERROR.
          RETURN ERROR.
       END.

       FIND CarteraVencida WHERE ROWID(CarteraVencida) EQ crowid 
                           NO-LOCK NO-ERROR.
       
       RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.*/
  
    FIND CURRENT CarteraVencida SHARE-LOCK NO-ERROR NO-WAIT.
         IF AVAILABLE(CarteraVencida) THEN DO:    
            RUN MostrarMensaje IN W_Manija (INPUT 25, OUTPUT W_Val).
            IF W_Val THEN DO:
              /*RUN P-GraLog IN W_Manija (INPUT "ADV: BORRA Registro, cf_vencida. CodPro: " 
                  + Cuentas.Cuenta:SCREEN-VALUE).*/
              RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ).
            END.
            FIND CURRENT CarteraVencida NO-LOCK NO-ERROR.
         END.  

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy V-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ) .
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) . 

IF AVAILABLE(CarteraVencida) THEN
    DO WITH FRAME {&Frame-Name} :
        FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ CarteraVencida.Cod_Producto
                            AND Pro_Creditos.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Pro_Creditos THEN
            ASSIGN W_Asociado = Pro_Creditos.Id_Asociado
                   Cmb_Pcto:SCREEN-VALUE =(STRING(Pro_Creditos.Cod_Credito,"999") + "-" + Pro_Creditos.Nom_Producto).

        FIND FIRST Varios WHERE Varios.Tipo EQ 10
                            AND Varios.Estado EQ 1
                            AND Varios.Codigo EQ CarteraVencida.Cod_Califica NO-LOCK NO-ERROR.
        IF AVAILABLE Varios THEN
            Cmb_Califica:SCREEN-VALUE = (STRING(Varios.Codigo,"99999")+ "-" + Varios.Descripcion).
        ELSE
            Cmb_Califica:SCREEN-VALUE = Cmb_Califica:ENTRY(1).

        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro EQ 4
                                 AND Pro_Ahorros.Cod_Ahorro EQ CarteraVencida.Cod_Aportes
                                 AND Pro_Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Pro_Ahorros THEN
            Cmb_Aportes:SCREEN-VALUE = (STRING(Pro_Ahorros.Cod_Ahorro,"999") + "-" + Pro_Ahorros.Nom_Producto).
        ELSE
            ASSIGN Cmb_Aportes:SCREEN-VALUE = Cmb_Aportes:ENTRY(1).

        RUN GET-ATTRIBUTE IN THIS-PROCEDURE("ADM-NEW-RECORD").

        IF RETURN-VALUE EQ "NO" THEN DO:
            W_Interes = CarteraVencida.Id_Interes.
            RUN Activa_Ctas.
        END.

        APPLY "VALUE-CHANGED":U TO Cmb_Aportes.
    END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
FIND FIRST agencias WHERE agencias.Tip_Agencia EQ "C" NO-LOCK NO-ERROR.
IF AVAILABLE agencias THEN
    W_Ofi = agencias.agencia.

ASSIGN Cmb_Pcto:LIST-ITEMS IN FRAME {&frame-name} = ""
       Cmb_Califica:LIST-ITEMS = ""
       Cmb_Aportes:LIST-ITEMS = "".

FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK BREAK BY Pro_Creditos.Cod_Credito:
    W_Metodo = Cmb_Pcto:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + "-" + Pro_Creditos.Nom_Producto).
END.

FOR EACH Varios WHERE Varios.Tipo EQ 10
                  AND Varios.Estado EQ 1 NO-LOCK BREAK BY Varios.Codigo:
    W_Metodo = Cmb_Califica:ADD-LAST(STRING(Varios.Codigo,"99999")+ "-" + Varios.Descripcion).
END.

W_Metodo = Cmb_Aportes:ADD-LAST("00000 - NO MANEJA").

Cmb_Aportes:SCREEN-VALUE = "00000 - NO MANEJA".

FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro EQ 4
                       AND Pro_Ahorros.Estado EQ 1 NO-LOCK BREAK BY Pro_Ahorros.Cod_Ahorro:
    W_Metodo = Cmb_Aportes:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + "-" + Pro_Ahorros.Nom_Producto).
END.

RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ).

IF Cmb_Aportes:LIST-ITEMS NE "" THEN
    APPLY "VALUE-CHANGED":U TO Cmb_Aportes.

IF Cmb_Pcto:LIST-ITEMS NE "" THEN
    APPLY "VALUE-CHANGED":U TO Cmb_Pcto.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ) .
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ModbClkCta V-table-Win 
PROCEDURE ModbClkCta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  T_ConsCta  LIKE Cuentas.Cuenta.
    DEFINE OUTPUT PARAMETER T_ConsCta1 LIKE Cuentas.Cuenta.
    
    T_ConsCta1 = T_ConsCta.
    
    DO WITH FRAME {&FRAME-NAME}:
       RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, OUTPUT W_CtrNat, INPUT "M").
           IF W_PCuenta EQ ? THEN DO:
              FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta
                             AND Cuentas.Tipo   EQ 2 
                             AND Cuentas.Estado EQ 1
                           NO-LOCK NO-ERROR NO-WAIT.
              IF NOT AVAILABLE(Cuentas) THEN
                 ASSIGN T_ConsCta1           = ""
                        W_NomCta:SCREEN-VALUE = "".           
           END.
           ELSE
              ASSIGN T_ConsCta1                = W_Pcuenta
                     W_NomCta:SCREEN-VALUE     = W_Pnombre.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "Cod_Producto" "CarteraVencida" "Cod_Producto"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "CarteraVencida"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

