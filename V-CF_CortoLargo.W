&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
{Incluido/Variable.I "SHARED"}

DEFINE VARIABLE W_Pcuenta AS CHARACTER.
DEFINE VARIABLE W_Pnombre AS CHARACTER.
DEFINE VARIABLE W_Naturaleza AS CHARACTER.
DEFINE VARIABLE W_CtrNat AS LOGICAL.
DEFINE VARIABLE W_ConsCta AS CHARACTER.
DEFINE VARIABLE W_DispCta AS CHARACTER.
DEFINE VARIABLE crowid AS ROWID.
DEFINE VARIABLE W_Val AS LOGICAL.
DEFINE VARIABLE W_Ok AS LOGICAL.
DEFINE VAR W_Nuevo AS LOGICAL INITIAL NO.

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
&Scoped-define EXTERNAL-TABLES CortoLargo
&Scoped-define FIRST-EXTERNAL-TABLE CortoLargo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR CortoLargo.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS CortoLargo.Agencia CortoLargo.Cod_Producto ~
CortoLargo.Clase_Producto CortoLargo.Plazo_Inicial CortoLargo.Plazo_Final ~
CortoLargo.Cta_AsoAd CortoLargo.Cta_AsoNa CortoLargo.Cta_NoaAd ~
CortoLargo.Cta_NoaNa CortoLargo.Cta_VigGarAd CortoLargo.Cta_ContingenteDb ~
CortoLargo.Cta_ContrapartidaGar CortoLargo.Cta_ContingenteCr ~
CortoLargo.Cta_FutGantia CortoLargo.Cta_GarPenCancel ~
CortoLargo.Cta_CostasDB CortoLargo.Cta_VigGarNa CortoLargo.Cta_CostasCR ~
CortoLargo.Cta_PolizasDB CortoLargo.Cta_OrdCasCR CortoLargo.Cta_PolizasCR ~
CortoLargo.Cta_OrdCasDB CortoLargo.Cta_Castigo CortoLargo.Cta_HonorariosDB ~
CortoLargo.Cta_HonorariosCR CortoLargo.Cta_SYA 
&Scoped-define ENABLED-TABLES CortoLargo
&Scoped-define FIRST-ENABLED-TABLE CortoLargo
&Scoped-Define ENABLED-OBJECTS RECT-199 RECT-200 RECT-202 RECT-228 RECT-229 ~
RECT-230 RECT-231 RECT-232 Cmb_Comprobantes 
&Scoped-Define DISPLAYED-FIELDS CortoLargo.Agencia CortoLargo.Cod_Producto ~
CortoLargo.Clase_Producto CortoLargo.Plazo_Inicial CortoLargo.Plazo_Final ~
CortoLargo.Cta_AsoAd CortoLargo.Cta_AsoNa CortoLargo.Cta_NoaAd ~
CortoLargo.Cta_NoaNa CortoLargo.Cta_VigGarAd CortoLargo.Cta_ContingenteDb ~
CortoLargo.Cta_ContrapartidaGar CortoLargo.Cta_ContingenteCr ~
CortoLargo.Cta_FutGantia CortoLargo.Cta_GarPenCancel ~
CortoLargo.Cta_CostasDB CortoLargo.Cta_VigGarNa CortoLargo.Cta_CostasCR ~
CortoLargo.Cta_PolizasDB CortoLargo.Cta_OrdCasCR CortoLargo.Cta_PolizasCR ~
CortoLargo.Cta_OrdCasDB CortoLargo.Cta_Castigo CortoLargo.Cta_HonorariosDB ~
CortoLargo.Cta_HonorariosCR CortoLargo.Cta_SYA 
&Scoped-define DISPLAYED-TABLES CortoLargo
&Scoped-define FIRST-DISPLAYED-TABLE CortoLargo
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencia Cmb_Producto F_Garantia ~
Cmb_Comprobantes Nombre1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Cmb_Agencia Cmb_Producto 
&Scoped-define ADM-ASSIGN-FIELDS Cmb_Producto 
&Scoped-define List-3 Cmb_Comprobantes 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE Cmb_Agencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Cmb_Comprobantes AS CHARACTER FORMAT "X(256)":U INITIAL "00 - No Asignado" 
     LABEL "Comprobante" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00 - No Asignado" 
     DROP-DOWN-LIST
     SIZE 24 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Producto AS CHARACTER FORMAT "X(50)":U 
     LABEL "Ahorros" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 30 BY 1 TOOLTIP "Productos Disponibles"
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F_Garantia AS CHARACTER FORMAT "X(80)":U INITIAL "Garantía Admisible" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 17 FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE Nombre1 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Nombre Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1.08
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-199
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 2.42.

DEFINE RECTANGLE RECT-200
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 2.69.

DEFINE RECTANGLE RECT-202
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 5.38.

DEFINE RECTANGLE RECT-228
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 1.35.

DEFINE RECTANGLE RECT-229
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 2.15.

DEFINE RECTANGLE RECT-230
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 2.15.

DEFINE RECTANGLE RECT-231
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 2.15.

DEFINE RECTANGLE RECT-232
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 2.15.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CortoLargo.Agencia AT ROW 1.27 COL 93 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          FGCOLOR 17 
     CortoLargo.Cod_Producto AT ROW 1.27 COL 93 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
     Cmb_Agencia AT ROW 1.54 COL 7 COLON-ALIGNED
     Cmb_Producto AT ROW 1.54 COL 62 COLON-ALIGNED
     CortoLargo.Clase_Producto AT ROW 1.65 COL 40.43 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ahorro", 1,
"Credito", 2
          SIZE 17 BY .81
          FONT 4
     CortoLargo.Plazo_Inicial AT ROW 2.88 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     CortoLargo.Plazo_Final AT ROW 2.88 COL 85 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     F_Garantia AT ROW 3.96 COL 5 NO-LABEL
     CortoLargo.Cta_AsoAd AT ROW 4.77 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_AsoNa AT ROW 4.77 COL 62 COLON-ALIGNED
          LABEL "Cta Asociados"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 FONT 5
     CortoLargo.Cta_NoaAd AT ROW 5.58 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_NoaNa AT ROW 5.58 COL 62 COLON-ALIGNED
          LABEL "Cta No Asociados"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_VigGarAd AT ROW 7.46 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_ContingenteDb AT ROW 7.73 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_ContrapartidaGar AT ROW 8.42 COL 74 COLON-ALIGNED
          LABEL "Cta Contrapartida de Garantías"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_ContingenteCr AT ROW 8.62 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_FutGantia AT ROW 9.35 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_GarPenCancel AT ROW 10.31 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_CostasDB AT ROW 10.42 COL 22 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_VigGarNa AT ROW 11.23 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_CostasCR AT ROW 11.31 COL 22 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_PolizasDB AT ROW 12.96 COL 22 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_OrdCasCR AT ROW 13.08 COL 74 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_PolizasCR AT ROW 13.85 COL 22 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     CortoLargo.Cta_OrdCasDB AT ROW 13.92 COL 74 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_Castigo AT ROW 15.27 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_HonorariosDB AT ROW 15.46 COL 22 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_HonorariosCR AT ROW 16.35 COL 22 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     CortoLargo.Cta_SYA AT ROW 16.35 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Cmb_Comprobantes AT ROW 17.96 COL 65 COLON-ALIGNED
     Nombre1 AT ROW 19.58 COL 29 COLON-ALIGNED
     "Cuentas de Orden para el Castigo de Cartera" VIEW-AS TEXT
          SIZE 40 BY .81 AT ROW 12.31 COL 48
          FGCOLOR 7 FONT 5
     "Honorarios" VIEW-AS TEXT
          SIZE 12 BY .5 AT ROW 15 COL 4
          FGCOLOR 7 FONT 5
     "Costas" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 9.88 COL 4
          FGCOLOR 7 FONT 5
     "Polizas" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 12.31 COL 4.57
          FGCOLOR 7 FONT 5
     "Cuentas Para Garantías" VIEW-AS TEXT
          SIZE 21 BY .81 AT ROW 6.65 COL 51
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Créditos Aprobados No Desembolsados" VIEW-AS TEXT
          SIZE 35 BY .81 AT ROW 6.65 COL 5
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Garantía No Admisible" VIEW-AS TEXT
          SIZE 21 BY .54 AT ROW 3.96 COL 59
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-199 AT ROW 4.23 COL 3
     RECT-200 AT ROW 6.92 COL 3
     RECT-202 AT ROW 6.92 COL 47
     RECT-228 AT ROW 1.27 COL 2
     RECT-229 AT ROW 12.69 COL 3
     RECT-230 AT ROW 10.15 COL 3
     RECT-231 AT ROW 15.27 COL 3
     RECT-232 AT ROW 12.73 COL 47
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: Datos.CortoLargo
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
         HEIGHT             = 19.88
         WIDTH              = 98.14.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Agencia IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX Cmb_Comprobantes IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_Producto IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_AsoNa IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_ContrapartidaGar IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_CostasCR IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_CostasDB IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_HonorariosCR IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_HonorariosDB IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_NoaNa IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_OrdCasCR IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_OrdCasDB IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_PolizasCR IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CortoLargo.Cta_PolizasDB IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN F_Garantia IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN Nombre1 IN FRAME F-Main
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

&Scoped-define SELF-NAME CortoLargo.Clase_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Clase_Producto V-table-Win
ON VALUE-CHANGED OF CortoLargo.Clase_Producto IN FRAME F-Main /* Clase de Producto */
OR RETURN OF CortoLargo.Clase_Producto OR TAB OF CortoLargo.Clase_Producto DO:
    ASSIGN Cmb_Producto:LIST-ITEMS                      = "".
    IF INTEGER(CortoLargo.Clase_Producto:SCREEN-VALUE) EQ 1 THEN DO:
       ASSIGN Cmb_Producto:LABEL                        = "Ahorros"
              F_Garantia:HIDDEN                         = TRUE
              CortoLargo.Cta_AsoNa:SENSITIVE            = FALSE
              CortoLargo.Cta_ContingenteCr:SENSITIVE    = FALSE
              CortoLargo.Cta_ContingenteDb:SENSITIVE    = FALSE
              CortoLargo.Cta_ContrapartidaGar:SENSITIVE = FALSE
              CortoLargo.Cta_FutGantia:SENSITIVE        = FALSE
              CortoLargo.Cta_GarPenCancel:SENSITIVE     = FALSE
              CortoLargo.Cta_NoaNa:SENSITIVE            = FALSE
              CortoLargo.Cta_VigGarAd:SENSITIVE         = FALSE
              CortoLargo.Cta_VigGarNa:SENSITIVE         = FALSE
              CortoLargo.Cta_CostasDB:SENSITIVE         = FALSE
              CortoLargo.Cta_CostasCR:SENSITIVE         = FALSE
              CortoLargo.Cta_PolizasDB:SENSITIVE        = FALSE
              CortoLargo.Cta_PolizasCR:SENSITIVE        = FALSE
              CortoLargo.Cta_HonorariosDB:SENSITIVE     = FALSE
              CortoLargo.Cta_HonorariosCR:SENSITIVE     = FALSE
              CortoLargo.Cta_Castigo:SENSITIVE          = FALSE
              CortoLargo.Cta_OrdCasDB:SENSITIVE         = FALSE
              CortoLargo.Cta_OrdCasCR:SENSITIVE         = FALSE.
       
       FOR EACH Pro_Ahorros 
           FIELDS(Pro_ahorros.cod_ahorro Pro_Ahorros.Nom_Producto Pro_Ahorros.Estado 
                                                              Pro_ahorros.tip_ahorro)
           WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK
           BREAK BY Pro_ahorros.cod_ahorro:
           IF FIRST-OF(Pro_ahorros.cod_ahorro) THEN
              Cmb_Producto:ADD-LAST(STRING(Pro_ahorros.cod_ahorro,"999")
                                         + "-" + Pro_Ahorros.Nom_Producto).
       END.
    END.
    ELSE DO:  
       ASSIGN Cmb_Producto:LABEL                        = "Créditos"
              F_Garantia:HIDDEN                         = FALSE
              F_Garantia:SCREEN-VALUE                   = "Garantía Admisible"
              CortoLargo.Cta_AsoNa:SENSITIVE            = TRUE
              CortoLargo.Cta_ContingenteCr:SENSITIVE    = TRUE
              CortoLargo.Cta_ContingenteDb:SENSITIVE    = TRUE
              CortoLargo.Cta_ContrapartidaGar:SENSITIVE = TRUE
              CortoLargo.Cta_FutGantia:SENSITIVE        = TRUE
              CortoLargo.Cta_GarPenCancel:SENSITIVE     = TRUE
              CortoLargo.Cta_NoaNa:SENSITIVE            = TRUE
              CortoLargo.Cta_VigGarAd:SENSITIVE         = TRUE
              CortoLargo.Cta_VigGarNa:SENSITIVE         = TRUE
              CortoLargo.Cta_CostasDB:SENSITIVE         = TRUE
              CortoLargo.Cta_CostasCR:SENSITIVE         = TRUE
              CortoLargo.Cta_PolizasDB:SENSITIVE        = TRUE
              CortoLargo.Cta_PolizasCR:SENSITIVE        = TRUE
              CortoLargo.Cta_HonorariosDB:SENSITIVE     = TRUE
              CortoLargo.Cta_HonorariosCR:SENSITIVE     = TRUE
              CortoLargo.Cta_Castigo:SENSITIVE          = TRUE
              CortoLargo.Cta_OrdCasDB:SENSITIVE         = TRUE
              CortoLargo.Cta_OrdCasCR:SENSITIVE         = TRUE.

       FOR EACH Pro_Creditos FIELDS(Pro_creditos.cod_credito Pro_Creditos.Nom_Producto Pro_Creditos.Estado)
                             WHERE Pro_Creditos.Estado EQ 1 NO-LOCK
                             BREAK BY Pro_creditos.cod_credito:
           IF FIRST-OF(Pro_creditos.cod_credito) THEN
              Cmb_Producto:ADD-LAST(STRING(Pro_creditos.cod_credito,"999")
                                         + "-" + Pro_Creditos.Nom_Producto).
       END.
    END.
    ASSIGN Cmb_Producto:SCREEN-VALUE = Cmb_Producto:ENTRY(1).
    APPLY "VALUE-CHANGED" TO Cmb_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencia V-table-Win
ON VALUE-CHANGED OF Cmb_Agencia IN FRAME F-Main /* Agencia */
DO:
  Cmb_Producto:LIST-ITEMS = "".
  IF CortoLargo.Clase_Producto:SCREEN-VALUE EQ "1" THEN DO:
    FOR EACH Pro_Ahorros 
        FIELDS(Pro_ahorros.cod_ahorro Pro_Ahorros.Nom_Producto Pro_Ahorros.Estado 
                                                           Pro_ahorros.tip_ahorro)
        WHERE Pro_Ahorros.Estado EQ 1 
        BREAK BY Pro_ahorros.cod_ahorro:
        IF FIRST-OF(Pro_Ahorros.Cod_Ahorro) THEN DO:
           Cmb_Producto:ADD-LAST(STRING(Pro_ahorros.cod_ahorro,"999")
                                      + "-" + Pro_Ahorros.Nom_Producto).

/*           IF Pro_ahorros.cod_ahorro EQ CortoLargo.Cod_Producto THEN DO:
              ASSIGN W_Codigo = Pro_ahorros.cod_ahorro
                     W_Nom    = Pro_Ahorros.Nom_Producto.
           END.*/
        END.
    END.
  END.
  ELSE DO:
     Cmb_Comprobantes:LIST-ITEMS = "".
     FOR EACH Comprobantes WHERE 
              Comprobantes.Agencia EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,3)) AND
              Comprobantes.Id_Consecutivo EQ 1 AND
              Comprobantes.Id_Efectivo EQ NO NO-LOCK:
       W_Ok = Cmb_Comprobantes:ADD-LAST(STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre).
       IF CortoLargo.Clase_Producto EQ 2 AND 
         Comprobantes.Comprobante EQ CortoLargo.Comprobante THEN
         Cmb_Comprobantes:SCREEN-VALUE = STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Producto V-table-Win
ON VALUE-CHANGED OF Cmb_Producto IN FRAME F-Main /* Ahorros */
DO:
    ASSIGN CortoLargo.Cod_Producto:SCREEN-VALUE = SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_AsoAd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_AsoAd V-table-Win
ON ENTRY OF CortoLargo.Cta_AsoAd IN FRAME F-Main /* Cta Asociados Gtia Admisible */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_AsoAd:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_AsoAd V-table-Win
ON LEAVE OF CortoLargo.Cta_AsoAd IN FRAME F-Main /* Cta Asociados Gtia Admisible */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_AsoAd:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_AsoAd V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_AsoAd IN FRAME F-Main /* Cta Asociados Gtia Admisible */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_AsoAd:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN CortoLargo.Cta_AsoAd:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_AsoNa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_AsoNa V-table-Win
ON ENTRY OF CortoLargo.Cta_AsoNa IN FRAME F-Main /* Cta Asociados */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_AsoNa:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_AsoNa V-table-Win
ON LEAVE OF CortoLargo.Cta_AsoNa IN FRAME F-Main /* Cta Asociados */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_AsoNa:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_AsoNa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_AsoNa IN FRAME F-Main /* Cta Asociados */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_AsoNa:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN CortoLargo.Cta_AsoNa:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_Castigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_Castigo V-table-Win
ON ENTRY OF CortoLargo.Cta_Castigo IN FRAME F-Main /* Cuenta para Castigo de Cartera */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_Castigo V-table-Win
ON LEAVE OF CortoLargo.Cta_Castigo IN FRAME F-Main /* Cuenta para Castigo de Cartera */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_Castigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_Castigo IN FRAME F-Main /* Cuenta para Castigo de Cartera */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_ContingenteCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_ContingenteCr V-table-Win
ON ENTRY OF CortoLargo.Cta_ContingenteCr IN FRAME F-Main /* Cta Contingente Crétido */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_ContingenteCr:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_ContingenteCr V-table-Win
ON LEAVE OF CortoLargo.Cta_ContingenteCr IN FRAME F-Main /* Cta Contingente Crétido */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_ContingenteCr:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_ContingenteCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_ContingenteCr IN FRAME F-Main /* Cta Contingente Crétido */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_ContingenteCr:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN CortoLargo.Cta_ContingenteCr:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_ContingenteDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_ContingenteDb V-table-Win
ON ENTRY OF CortoLargo.Cta_ContingenteDb IN FRAME F-Main /* Cta Contingente Débito */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_ContingenteDb:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_ContingenteDb V-table-Win
ON LEAVE OF CortoLargo.Cta_ContingenteDb IN FRAME F-Main /* Cta Contingente Débito */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_ContingenteDb:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_ContingenteDb V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_ContingenteDb IN FRAME F-Main /* Cta Contingente Débito */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_ContingenteDb:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN CortoLargo.Cta_ContingenteDb:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_ContrapartidaGar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_ContrapartidaGar V-table-Win
ON ENTRY OF CortoLargo.Cta_ContrapartidaGar IN FRAME F-Main /* Cta Contrapartida de Garantías */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_ContrapartidaGar:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_ContrapartidaGar V-table-Win
ON LEAVE OF CortoLargo.Cta_ContrapartidaGar IN FRAME F-Main /* Cta Contrapartida de Garantías */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_ContrapartidaGar:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_ContrapartidaGar V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_ContrapartidaGar IN FRAME F-Main /* Cta Contrapartida de Garantías */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_ContrapartidaGar:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN CortoLargo.Cta_ContrapartidaGar:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_CostasCR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_CostasCR V-table-Win
ON ENTRY OF CortoLargo.Cta_CostasCR IN FRAME F-Main /* Crédito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_CostasCR V-table-Win
ON LEAVE OF CortoLargo.Cta_CostasCR IN FRAME F-Main /* Crédito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_CostasCR V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_CostasCR IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_CostasDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_CostasDB V-table-Win
ON ENTRY OF CortoLargo.Cta_CostasDB IN FRAME F-Main /* Débito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_CostasDB V-table-Win
ON LEAVE OF CortoLargo.Cta_CostasDB IN FRAME F-Main /* Débito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_CostasDB V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_CostasDB IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_FutGantia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_FutGantia V-table-Win
ON ENTRY OF CortoLargo.Cta_FutGantia IN FRAME F-Main /* Cta Futuras Garantías */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_FutGantia:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_FutGantia V-table-Win
ON LEAVE OF CortoLargo.Cta_FutGantia IN FRAME F-Main /* Cta Futuras Garantías */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_FutGantia:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_FutGantia V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_FutGantia IN FRAME F-Main /* Cta Futuras Garantías */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_FutGantia:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN CortoLargo.Cta_FutGantia:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_GarPenCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_GarPenCancel V-table-Win
ON ENTRY OF CortoLargo.Cta_GarPenCancel IN FRAME F-Main /* Cta Garantías Pendientes Cancelación */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_GarPenCancel:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_GarPenCancel V-table-Win
ON LEAVE OF CortoLargo.Cta_GarPenCancel IN FRAME F-Main /* Cta Garantías Pendientes Cancelación */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_GarPenCancel:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_GarPenCancel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_GarPenCancel IN FRAME F-Main /* Cta Garantías Pendientes Cancelación */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_GarPenCancel:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN CortoLargo.Cta_GarPenCancel:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_HonorariosCR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_HonorariosCR V-table-Win
ON ENTRY OF CortoLargo.Cta_HonorariosCR IN FRAME F-Main /* Crédito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_HonorariosCR V-table-Win
ON LEAVE OF CortoLargo.Cta_HonorariosCR IN FRAME F-Main /* Crédito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_HonorariosCR V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_HonorariosCR IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_HonorariosDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_HonorariosDB V-table-Win
ON ENTRY OF CortoLargo.Cta_HonorariosDB IN FRAME F-Main /* Débito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_HonorariosDB V-table-Win
ON LEAVE OF CortoLargo.Cta_HonorariosDB IN FRAME F-Main /* Débito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_HonorariosDB V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_HonorariosDB IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_NoaAd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_NoaAd V-table-Win
ON ENTRY OF CortoLargo.Cta_NoaAd IN FRAME F-Main /* Cuenta Débito */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_NoaAd:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_NoaAd V-table-Win
ON LEAVE OF CortoLargo.Cta_NoaAd IN FRAME F-Main /* Cuenta Débito */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_NoaAd:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_NoaAd V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_NoaAd IN FRAME F-Main /* Cuenta Débito */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_NoaAd:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN CortoLargo.Cta_NoaAd:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_NoaNa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_NoaNa V-table-Win
ON ENTRY OF CortoLargo.Cta_NoaNa IN FRAME F-Main /* Cta No Asociados */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_NoaNa:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_NoaNa V-table-Win
ON LEAVE OF CortoLargo.Cta_NoaNa IN FRAME F-Main /* Cta No Asociados */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_NoaNa:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_NoaNa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_NoaNa IN FRAME F-Main /* Cta No Asociados */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_NoaNa:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN CortoLargo.Cta_NoaNa:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_OrdCasCR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_OrdCasCR V-table-Win
ON ENTRY OF CortoLargo.Cta_OrdCasCR IN FRAME F-Main /* Crédito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_OrdCasCR V-table-Win
ON LEAVE OF CortoLargo.Cta_OrdCasCR IN FRAME F-Main /* Crédito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_OrdCasCR V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_OrdCasCR IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_OrdCasDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_OrdCasDB V-table-Win
ON ENTRY OF CortoLargo.Cta_OrdCasDB IN FRAME F-Main /* Débito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_OrdCasDB V-table-Win
ON LEAVE OF CortoLargo.Cta_OrdCasDB IN FRAME F-Main /* Débito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_OrdCasDB V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_OrdCasDB IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_PolizasCR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_PolizasCR V-table-Win
ON ENTRY OF CortoLargo.Cta_PolizasCR IN FRAME F-Main /* Crédito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_PolizasCR V-table-Win
ON LEAVE OF CortoLargo.Cta_PolizasCR IN FRAME F-Main /* Crédito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_PolizasCR V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_PolizasCR IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_PolizasDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_PolizasDB V-table-Win
ON ENTRY OF CortoLargo.Cta_PolizasDB IN FRAME F-Main /* Débito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_PolizasDB V-table-Win
ON LEAVE OF CortoLargo.Cta_PolizasDB IN FRAME F-Main /* Débito */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_PolizasDB V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_PolizasDB IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_SYA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_SYA V-table-Win
ON ENTRY OF CortoLargo.Cta_SYA IN FRAME F-Main /* Cuenta Fuente */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_SYA V-table-Win
ON LEAVE OF CortoLargo.Cta_SYA IN FRAME F-Main /* Cuenta Fuente */
DO:
  ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
  RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_SYA V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_SYA IN FRAME F-Main /* Cuenta Fuente */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Cta_VigGarAd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_VigGarAd V-table-Win
ON ENTRY OF CortoLargo.Cta_VigGarAd IN FRAME F-Main /* Cta Garantías Vigentes Admisible */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_VigGarAd:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_VigGarAd V-table-Win
ON LEAVE OF CortoLargo.Cta_VigGarAd IN FRAME F-Main /* Cta Garantías Vigentes Admisible */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_VigGarAd:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Cta_VigGarAd V-table-Win
ON MOUSE-SELECT-DBLCLICK OF CortoLargo.Cta_VigGarAd IN FRAME F-Main /* Cta Garantías Vigentes Admisible */
DO:
    ASSIGN W_ConsCta = CortoLargo.Cta_VigGarAd:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN CortoLargo.Cta_VigGarAd:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Plazo_Final
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Plazo_Final V-table-Win
ON LEAVE OF CortoLargo.Plazo_Final IN FRAME F-Main /* Plazo Final */
DO:
    IF CortoLargo.Plazo_Final:SCREEN-VALUE EQ "?" THEN
       CortoLargo.Plazo_Final:SCREEN-VALUE = "0".  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CortoLargo.Plazo_Inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CortoLargo.Plazo_Inicial V-table-Win
ON LEAVE OF CortoLargo.Plazo_Inicial IN FRAME F-Main /* Plazo Inicial */
DO:
    IF CortoLargo.Plazo_Inicial:SCREEN-VALUE EQ "?" THEN
       CortoLargo.Plazo_Inicial:SCREEN-VALUE = "0".  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

/*  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
 *     RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
 *   &ENDIF   */      
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "CortoLargo"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "CortoLargo"}

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
             APPLY "MOUSE-SELECT-DBLCLICK" TO SELF.
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
          ASSIGN Nombre1:SCREEN-VALUE = Cuentas.Nombre.
       ELSE
          ASSIGN Nombre1:SCREEN-VALUE = "".
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
    DO WITH FRAME {&FRAME-NAME}:
       ASSIGN F_Garantia:SCREEN-VALUE = "Garantía Admisible".
              Cmb_Producto:SENSITIVE  = TRUE.

       IF CortoLargo.Clase_Producto:SCREEN-VALUE EQ "1" THEN DO:
          ASSIGN F_Garantia:HIDDEN = TRUE.
       END.
       ELSE DO:
          ASSIGN F_Garantia:HIDDEN = FALSE.
       END.
       RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
       RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ).
       ENABLE Cmb_Agencia.
       ASSIGN CortoLargo.Plazo_Inicial:SCREEN-VALUE = ""
              CortoLargo.Plazo_Final:SCREEN-VALUE   = ""
              Cmb_Comprobantes:SCREEN-VALUE = Cmb_Comprobantes:ENTRY(1).
       
       APPLY "VALUE-CHANGED":U TO CortoLargo.Clase_Producto.
       CortoLargo.Agencia:SCREEN-VALUE = SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3).
       CortoLargo.Cod_Producto:SCREEN-VALUE = SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3).
       W_Nuevo = YES.
    END.
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
         ASSIGN CortoLargo.Agencia:SCREEN-VALUE = SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3)
                CortoLargo.Cod_Producto:SCREEN-VALUE = SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3).
    
       Crowid = ROWID(CortoLargo).
       FIND CortoLargo 
            WHERE  (CortoLargo.Agencia   EQ integer(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
              AND  CortoLargo.Cod_Producto   EQ INTEGER(CortoLargo.Cod_Producto:SCREEN-VALUE)
              AND  CortoLargo.Clase_Producto EQ INTEGER(CortoLargo.Clase_Producto:SCREEN-VALUE)
              AND  INTEGER(CortoLargo.Plazo_Inicial:SCREEN-VALUE) GE CortoLargo.Plazo_Inicial 
              AND  INTEGER(CortoLargo.Plazo_Inicial:SCREEN-VALUE) LE CortoLargo.Plazo_Final
              AND  ROWID(CortoLargo) NE Crowid)                                              
               OR (CortoLargo.Agencia   EQ integer(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
              AND  CortoLargo.Cod_Producto EQ INTEGER(CortoLargo.Cod_Producto:SCREEN-VALUE)
              AND  CortoLargo.Clase_Producto EQ INTEGER(CortoLargo.Clase_Producto:SCREEN-VALUE)
              AND  INTEGER(CortoLargo.Plazo_Final:SCREEN-VALUE)   GE CortoLargo.Plazo_Inicial    
              AND  INTEGER(CortoLargo.Plazo_Final:SCREEN-VALUE)   LE CortoLargo.Plazo_Final
              AND  ROWID(CortoLargo) NE Crowid)
               OR (CortoLargo.Agencia   EQ integer(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
              AND  CortoLargo.Cod_Producto EQ INTEGER(CortoLargo.Cod_Producto:SCREEN-VALUE)
              AND  CortoLargo.Clase_Producto EQ INTEGER(CortoLargo.Clase_Producto:SCREEN-VALUE)
              AND  INTEGER(CortoLargo.Plazo_Inicial:SCREEN-VALUE) LE CortoLargo.Plazo_Inicial
              AND  INTEGER(CortoLargo.Plazo_Final:SCREEN-VALUE) GE CortoLargo.Plazo_Final
              AND  ROWID(CortoLargo) NE Crowid) EXCLUSIVE-LOCK NO-ERROR.
 
       IF AVAILABLE(CortoLargo) THEN DO:  
          MESSAGE "El Rango ya esta contenido, o Contiene" SKIP
                  "a otro para el producto. Verifique."
                  VIEW-AS ALERT-BOX INFORMATION TITLE "ERROR AL SALVAR".
          RETURN ERROR.
       END.
       FIND CortoLargo WHERE ROWID(CortoLargo) EQ crowid NO-ERROR.
       IF AVAILABLE CortoLargo THEN
         ASSIGN CortoLargo.Agencia:SCREEN-VALUE = SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3)
                CortoLargo.Cod_Producto:SCREEN-VALUE = SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3).
              
       RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).
       
       ASSIGN CortoLargo.Agencia = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
              CortoLargo.Cod_Producto = INTEGER(SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3))
              CortoLargo.Comprobante = INTEGER(SUBSTRIN(Cmb_Comprobantes:SCREEN-VALUE,1,2)).
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
  DISABLE Cmb_Agencia.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
  DO WITH FRAME F-Main:
     ASSIGN CortoLargo.Agencia:SCREEN-VALUE = SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3)
            CortoLargo.Cod_Producto:SCREEN-VALUE = SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3).
  END.
              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
FIND CURRENT  CortoLargo SHARE-LOCK NO-ERROR NO-WAIT.
     IF AVAILABLE(CortoLargo) THEN DO:    
        RUN MostrarMensaje IN W_Manija (INPUT 25, OUTPUT W_Val).
        IF W_Val THEN DO:
          RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ).
          RUN P-GraLog IN W_Manija (INPUT "ADV: BORRA Registro, CortoLargo. Age: " + 
              STRING(CortoLargo.Agencia) + " - ClasePro: " + STRING(CortoLargo.Clase_Producto) +
              " - CodPro: " + STRING(CortoLargo.Cod_Producto) + " - PI:" + STRING(CortoLargo.Plazo_Inicial) + 
              " PF:" + STRING(CortoLargo.Plazo_Final)).
        END.
        FIND CURRENT CortoLargo NO-LOCK NO-ERROR.
     END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy V-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR W_Codigo AS INTEGER   FORMAT "999".
    DEFINE VAR W_Nom    AS CHARACTER FORMAT "X(40)".
  
    DO WITH FRAME {&FRAME-NAME}:
       RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
       
       APPLY "VALUE-CHANGED" TO CortoLargo.Clase_Producto.
       ASSIGN Cmb_Producto:LIST-ITEMS = "".
       IF AVAILABLE(CortoLargo) THEN DO :
          FIND Agencias WHERE Agencias.Agencia EQ CortoLargo.Agencia NO-LOCK NO-ERROR.
          IF AVAILABLE Agencias THEN
              Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
          ELSE DO:
              MESSAGE "No se encontro la Agencia del producto" VIEW-AS ALERT-BOX ERROR.
              RETURN ERROR.
          END.
          APPLY "value-changed" TO Cmb_Agencia.
          IF CortoLargo.Clase_Producto GT 0 THEN
             ASSIGN CortoLargo.Clase_Producto:SCREEN-VALUE = STRING(CortoLargo.Clase_Producto).

          IF CortoLargo.Clase_Producto EQ 1 THEN DO:
             ASSIGN Cmb_Producto:LABEL      = "Ahorros"
                    F_Garantia:HIDDEN       = TRUE.
             FOR EACH Pro_Ahorros 
                 FIELDS(Pro_ahorros.Cod_Ahorro Pro_Ahorros.Nom_Producto Pro_Ahorros.Estado 
                                                                    Pro_ahorros.tip_ahorro)
                 WHERE Pro_Ahorros.Estado EQ 1  NO-LOCK
                 BREAK BY Pro_ahorros.cod_ahorro:
                 IF FIRST-OF(Pro_ahorros.cod_ahorro) THEN DO:
                    Cmb_Producto:ADD-LAST(STRING(Pro_ahorros.cod_ahorro,"999")
                                               + "-" + Pro_Ahorros.Nom_Producto).
                    IF Pro_ahorros.cod_ahorro EQ CortoLargo.Cod_Producto THEN DO:
                       ASSIGN W_Codigo = Pro_ahorros.cod_ahorro
                              W_Nom    = Pro_Ahorros.Nom_Producto.
                    END.
                 END.
             END.
          END.
          ELSE DO:
             ASSIGN Cmb_Producto:LABEL      = "Créditos"
                    F_Garantia:HIDDEN       = FALSE.
             FOR EACH Pro_Creditos 
                      FIELDS(Pro_creditos.cod_credito Pro_Creditos.Nom_Producto Pro_Creditos.Estado)
                                   WHERE Pro_Creditos.Estado EQ 1 NO-LOCK
                                   BREAK BY Pro_creditos.cod_credito:
                 IF FIRST-OF(Pro_creditos.cod_credito) THEN DO:
                    Cmb_Producto:ADD-LAST(STRING(Pro_creditos.cod_credito,"999")
                                               + "-" + Pro_Creditos.Nom_Producto).
                    IF Pro_creditos.cod_credito EQ CortoLargo.Cod_Producto THEN DO:
                       ASSIGN W_Codigo = Pro_creditos.cod_credito
                              W_Nom    = Pro_Creditos.Nom_Producto.
                    END.
                 END.
             END.
          END.
          Cmb_Comprobantes:LIST-ITEMS = "".
          W_Ok = Cmb_Comprobantes:ADD-LAST("00 - No Asignado").
          Cmb_Comprobantes:SCREEN-VALUE = "00 - No Asignado".
          FOR EACH Comprobantes WHERE Comprobantes.Agencia EQ CortoLargo.Agencia AND 
                   Comprobantes.Id_Efectivo EQ NO NO-LOCK:
            W_Ok = Cmb_Comprobantes:ADD-LAST(STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre).
            IF Comprobantes.Comprobante EQ CortoLargo.Comprobante THEN
               Cmb_Comprobantes:SCREEN-VALUE = STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre.
          END.
          IF W_Codigo GT 0 THEN
             ASSIGN Cmb_Producto:SCREEN-VALUE            = STRING(W_Codigo,"999") + "-" + W_Nom
                    CortoLargo.Cod_Producto:SCREEN-VALUE = SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3).  
          ELSE
             APPLY "VALUE-CHANGED":U TO CortoLargo.Clase_Producto.           
       END.
     
       FIND Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd:SCREEN-VALUE 
                      AND Cuentas.Tipo   EQ 2 
                      AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
       IF AVAILABLE(Cuentas) THEN 
          ASSIGN Nombre1:SCREEN-VALUE = Cuentas.Nombre.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
      W_Ok = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME {&FRAME-NAME}.
  END.
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
    FIND FIRST CortoLargo NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CortoLargo THEN DO:
       CortoLargo.Clase_Producto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
       APPLY "VALUE-CHANGED":U TO CortoLargo.Clase_Producto.
    END.
    ELSE DO:
      FOR EACH Comprobantes WHERE 
               Comprobantes.Agencia EQ CortoLargo.Agencia AND
               Comprobantes.Id_Consecutivo EQ 1 AND
               Comprobantes.Id_Efectivo EQ NO NO-LOCK:
          W_Ok = Cmb_Comprobantes:ADD-LAST(STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre).
          IF Comprobantes.Comprobante EQ CortoLargo.Comprobante THEN
             Cmb_Comprobantes:SCREEN-VALUE = STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre.
      END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  IF W_Nuevo THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, CortoLargo. Age: " + 
         STRING(CortoLargo.Agencia) + " - ClasePro: " + STRING(CortoLargo.Clase_Producto) +
         " - CodPro: " + STRING(CortoLargo.Cod_Producto) + " - PI:" + STRING(CortoLargo.Plazo_Inicial) + 
         " PF:" + STRING(CortoLargo.Plazo_Final)).
  ELSE
     RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, CortoLargo. Age: " + 
         STRING(CortoLargo.Agencia) + " - ClasePro: " + STRING(CortoLargo.Clase_Producto) +
         " - CodPro: " + STRING(CortoLargo.Cod_Producto) + " - PI:" + STRING(CortoLargo.Plazo_Inicial) + 
         " PF:" + STRING(CortoLargo.Plazo_Final)).
  W_Nuevo = NO.
  IF RETURN-VALUE EQ "ADM-ERROR":U AND
     CortoLargo.Clase_Producto:SENSITIVE IN FRAME {&FRAME-NAME} EQ FALSE THEN
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ).
  DISABLE Cmb_Agencia.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ModbClkcta V-table-Win 
PROCEDURE ModbClkcta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER T_ConsCta   LIKE Cuentas.Cuenta.
    DEFINE OUTPUT PARAMETER T_ConsCta1 LIKE Cuentas.Cuenta.

    ASSIGN T_ConsCta1 = T_ConsCta.

    DO WITH FRAME {&FRAME-NAME}:
       RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                      OUTPUT W_CtrNat, INPUT "M").
           IF W_PCuenta EQ ? THEN DO:
              FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta
                             AND Cuentas.Tipo   EQ 2 
                             AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
              IF NOT AVAILABLE(Cuentas) THEN
                 ASSIGN T_ConsCta1           = ""
                        Nombre1:SCREEN-VALUE = "".           
           END.
           ELSE
              ASSIGN T_ConsCta1               = W_Pcuenta
                     Nombre1:SCREEN-VALUE     = W_Pnombre.
    END.
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
  {src/adm/template/snd-list.i "CortoLargo"}

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

