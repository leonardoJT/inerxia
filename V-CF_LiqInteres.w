&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/* ***************************  Definitions  ************************** */
    {incluido\variable.i "shared"}
    DEFINE VARIABLE W_Val          AS LOGICAL.
    DEFINE VARIABLE W_Codbase      AS CHARACTER FORMAT "X(4)".
    DEFINE VARIABLE W_Pcuenta    LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_Pnombre    LIKE Cuentas.Nombre.
    DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
    DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
    DEFINE VARIABLE W_Consulta     AS LOGICAL INITIAL FALSE.
    DEFINE VARIABLE W_ConsCta    LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_DispCta    LIKE Cuentas.Cuenta.

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
&Scoped-define EXTERNAL-TABLES Liqui_int
&Scoped-define FIRST-EXTERNAL-TABLE Liqui_int


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Liqui_int.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Liqui_Int.CtaDb_LiqAso Liqui_Int.CtaDb_Liq ~
Liqui_Int.CtaCr_LiqAso Liqui_Int.CtaCr_Liq Liqui_Int.CtaDb_Ret ~
Liqui_Int.CtaInt_AntAso Liqui_Int.CtaCr_Ret Liqui_Int.CtaInt_Ant ~
Liqui_Int.CtaDb_MoraAso Liqui_Int.CtaDb_Mora Liqui_Int.CtaDb_DifCobAso ~
Liqui_Int.CtaDb_DifCob Liqui_Int.CtaCr_MoraAso Liqui_Int.CtaCr_Mora ~
Liqui_Int.CtaCr_DifCobAso Liqui_Int.CtaCr_DifCob Liqui_Int.Cta_SucyAge ~
Liqui_Int.Cod_base Liqui_Int.Base Liqui_Int.Cta_CauCr 
&Scoped-define ENABLED-TABLES Liqui_Int
&Scoped-define FIRST-ENABLED-TABLE Liqui_Int
&Scoped-Define ENABLED-OBJECTS RECT-147 RECT-79 RECT-85 RECT-86 
&Scoped-Define DISPLAYED-FIELDS Liqui_Int.Clase_Producto ~
Liqui_Int.CtaDb_LiqAso Liqui_Int.CtaDb_Liq Liqui_Int.CtaCr_LiqAso ~
Liqui_Int.CtaCr_Liq Liqui_Int.CtaDb_Ret Liqui_Int.CtaInt_AntAso ~
Liqui_Int.CtaCr_Ret Liqui_Int.CtaInt_Ant Liqui_Int.CtaDb_MoraAso ~
Liqui_Int.CtaDb_Mora Liqui_Int.CtaDb_DifCobAso Liqui_Int.CtaDb_DifCob ~
Liqui_Int.CtaCr_MoraAso Liqui_Int.CtaCr_Mora Liqui_Int.CtaCr_DifCobAso ~
Liqui_Int.CtaCr_DifCob Liqui_Int.Cta_SucyAge Liqui_Int.Cod_base ~
Liqui_Int.Base Liqui_Int.Cta_CauCr 
&Scoped-define DISPLAYED-TABLES Liqui_Int
&Scoped-define FIRST-DISPLAYED-TABLE Liqui_Int
&Scoped-Define DISPLAYED-OBJECTS F_Titcxc F_Titcxc-2 F_SobMor W_NomCuenta ~
W_Porcent W-nomret 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Liqui_Int.Clase_Producto Cmb_Producto 
&Scoped-define ADM-ASSIGN-FIELDS Liqui_Int.Cod_Producto 
&Scoped-define List-4 Liqui_Int.Cod_base Liqui_Int.Base 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE Cmb_Producto AS CHARACTER FORMAT "X(50)":U 
     LABEL "Ahorros" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 42 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_SobMor AS CHARACTER FORMAT "X(50)":U INITIAL "Cuentas Liquidación Por Mora" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1.08
     BGCOLOR 17 FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE F_Titcxc AS CHARACTER FORMAT "X(50)":U INITIAL "Ctas. X Cobrar Dev. Cheques" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.08
     BGCOLOR 17 FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE F_Titcxc-2 AS CHARACTER FORMAT "X(50)":U INITIAL "Cuentas Retención" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.08
     BGCOLOR 17 FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE W-nomret AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_NomCuenta AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre de la Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Porcent AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Porcentaje" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-147
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 4.81.

DEFINE RECTANGLE RECT-79
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 4.85.

DEFINE RECTANGLE RECT-85
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 4.31.

DEFINE RECTANGLE RECT-86
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 4.31.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Liqui_Int.Clase_Producto AT ROW 1.27 COL 27 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ahorro", 1,
"Credito", 2
          SIZE 21 BY .62
          FONT 5
     Cmb_Producto AT ROW 2.35 COL 25 COLON-ALIGNED HELP
          "Seleccione el Producto para el Proceso de Corto y Largo Plazo"
     Liqui_Int.Cod_Producto AT ROW 2.35 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
     Liqui_Int.CtaDb_LiqAso AT ROW 6.38 COL 6 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaDb_Liq AT ROW 6.38 COL 29 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaCr_LiqAso AT ROW 7.46 COL 6 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaCr_Liq AT ROW 7.46 COL 29 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     F_Titcxc AT ROW 4.23 COL 47 NO-LABEL
     F_Titcxc-2 AT ROW 4.23 COL 47 NO-LABEL
     Liqui_Int.CtaDb_Ret AT ROW 6.12 COL 54 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaInt_AntAso AT ROW 6.12 COL 79 COLON-ALIGNED
          LABEL "Asociados"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaCr_Ret AT ROW 7.19 COL 54 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaInt_Ant AT ROW 7.19 COL 79 COLON-ALIGNED
          LABEL "No Asociados"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     F_SobMor AT ROW 9.08 COL 2 NO-LABEL
     Liqui_Int.CtaDb_MoraAso AT ROW 11.5 COL 6 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaDb_Mora AT ROW 11.5 COL 28 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaDb_DifCobAso AT ROW 11.5 COL 54 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaDb_DifCob AT ROW 11.5 COL 78 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaCr_MoraAso AT ROW 12.58 COL 6 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaCr_Mora AT ROW 12.58 COL 28 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaCr_DifCobAso AT ROW 12.58 COL 54 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.CtaCr_DifCob AT ROW 12.58 COL 78 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Liqui_Int.Cta_SucyAge AT ROW 14.19 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     W_NomCuenta AT ROW 18.77 COL 5.86
     Liqui_Int.Cod_base AT ROW 15.27 COL 19 COLON-ALIGNED
          LABEL "Código Base Retención" FORMAT "X(4)"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     W_Porcent AT ROW 16.35 COL 44 COLON-ALIGNED
     Liqui_Int.Base AT ROW 16.35 COL 19 COLON-ALIGNED
          LABEL "Valor Base de Retención"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     W-nomret AT ROW 15.27 COL 34 COLON-ALIGNED NO-LABEL
     Liqui_Int.Cta_CauCr AT ROW 17.42 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     "Cuentas de Liquidación de Intereses" VIEW-AS TEXT
          SIZE 32 BY 1.04 AT ROW 4.23 COL 2
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Cuenta Int.Anticipado" VIEW-AS TEXT
          SIZE 19 BY 1.08 AT ROW 4.23 COL 75
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Con libranza" VIEW-AS TEXT
          SIZE 11 BY .81 AT ROW 5.31 COL 5
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Sin Libranza" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 5.31 COL 25
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Cuentas para Difícil Cobro" VIEW-AS TEXT
          SIZE 23 BY 1.08 AT ROW 9.08 COL 49
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Sin Libranza" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 10.15 COL 72
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Con Libranza" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 10.42 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Sin Libranza" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 10.42 COL 24
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Con libranza" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 10.42 COL 51
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-147 AT ROW 4.77 COL 46
     RECT-79 AT ROW 4.77 COL 1
     RECT-85 AT ROW 9.62 COL 1
     RECT-86 AT ROW 9.62 COL 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: Datos.Liqui_int
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
         HEIGHT             = 19.5
         WIDTH              = 94.86.
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

/* SETTINGS FOR FILL-IN Liqui_Int.Base IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR RADIO-SET Liqui_Int.Clase_Producto IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX Cmb_Producto IN FRAME F-Main
   NO-DISPLAY NO-ENABLE 1                                               */
/* SETTINGS FOR FILL-IN Liqui_Int.Cod_base IN FRAME F-Main
   4 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN Liqui_Int.Cod_Producto IN FRAME F-Main
   NO-DISPLAY NO-ENABLE 2                                               */
ASSIGN 
       Liqui_Int.Cod_Producto:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN Liqui_Int.CtaCr_DifCob IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaCr_DifCobAso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaCr_Liq IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaCr_LiqAso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaCr_Mora IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaCr_MoraAso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaCr_Ret IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaDb_DifCob IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaDb_DifCobAso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaDb_Liq IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaDb_LiqAso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaDb_Mora IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaDb_MoraAso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaDb_Ret IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaInt_Ant IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Liqui_Int.CtaInt_AntAso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN F_SobMor IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F_Titcxc IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F_Titcxc-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W-nomret IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCuenta IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_Porcent IN FRAME F-Main
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

&Scoped-define SELF-NAME Liqui_Int.Clase_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.Clase_Producto V-table-Win
ON VALUE-CHANGED OF Liqui_Int.Clase_Producto IN FRAME F-Main /* Clase de Producto */
OR RETURN OF Liqui_Int.Clase_Producto OR TAB OF Liqui_Int.Clase_Producto DO:
    ASSIGN Cmb_Producto:LIST-ITEMS = "".

    IF INTEGER(Liqui_Int.Clase_Producto:SCREEN-VALUE) = 1 THEN DO:
        ASSIGN Cmb_Producto:LABEL = "Ahorros"
               F_SobMor:SCREEN-VALUE = "Cuentas Liquidación Interes Sobregiro"
               Liqui_Int.CtaCr_Ret:SENSITIVE = TRUE
               Liqui_Int.CtaDb_Ret:SENSITIVE = TRUE
               Liqui_Int.Base:SENSITIVE = TRUE
               Liqui_Int.Cod_base:SENSITIVE = TRUE
               Liqui_Int.CtaDb_DifcobAso:SENSITIVE = FALSE
               Liqui_Int.CtaCr_DifcobAso:SENSITIVE = FALSE
               Liqui_Int.CtaDb_DifCob:SENSITIVE = FALSE
               Liqui_Int.CtaCr_DifCob:SENSITIVE = FALSE
               Liqui_Int.CtaDb_MoraAso:SENSITIVE = FALSE
               Liqui_Int.CtaCr_MoraAso:SENSITIVE = FALSE
               Liqui_Int.CtaDb_Mora:SENSITIVE = FALSE
               Liqui_Int.CtaCr_Mora:SENSITIVE = FALSE.

        FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 AND Pro_ahorros.tip_ahorro NE 4 NO-LOCK BREAK BY Pro_ahorros.cod_ahorro:
            IF FIRST-OF(Pro_ahorros.cod_ahorro) THEN
                Cmb_Producto:ADD-LAST(STRING(Pro_ahorros.cod_ahorro,"999") + "-" + Pro_Ahorros.Nom_Producto).
        END.
    END.
    ELSE DO:
        ASSIGN Cmb_Producto:LABEL = "Créditos"
               F_SobMor:SCREEN-VALUE = "Cuentas Liquidación Por Mora"
               W_Porcent:SCREEN-VALUE = ""
               W-Nomret:SCREEN-VALUE = ""
               Liqui_Int.Cod_base:SCREEN-VALUE = ""
               Liqui_Int.Base:SCREEN-VALUE = ""
               Liqui_Int.CtaCr_Ret:SENSITIVE = FALSE
               Liqui_Int.CtaDb_Ret:SENSITIVE = TRUE
               Liqui_Int.Base:SENSITIVE = FALSE
               Liqui_Int.Cod_base:SENSITIVE = FALSE
               Liqui_Int.CtaInt_AntAso:SENSITIVE = TRUE
               Liqui_Int.CtaInt_Ant:SENSITIVE = TRUE
               Liqui_Int.CtaCr_LiqAso:SENSITIVE = TRUE
               Liqui_Int.CtaDb_LiqAso:SENSITIVE = TRUE
               Liqui_Int.CtaCr_Liq:SENSITIVE = TRUE
               Liqui_Int.CtaDb_Liq:SENSITIVE = TRUE
               Liqui_Int.CtaDb_MoraAso:SENSITIVE = TRUE
               Liqui_Int.CtaCr_MoraAso:SENSITIVE = TRUE
               Liqui_Int.CtaDb_Mora:SENSITIVE = TRUE
               Liqui_Int.CtaCr_Mora:SENSITIVE = TRUE
               Liqui_Int.CtaDb_DifcobAso:SENSITIVE = TRUE
               Liqui_Int.CtaCr_DifcobAso:SENSITIVE = TRUE
               Liqui_Int.CtaDb_DifCob:SENSITIVE = TRUE
               Liqui_Int.CtaCr_DifCob:SENSITIVE = TRUE.

        FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK BREAK BY Pro_creditos.cod_credito:
            IF FIRST-OF(Pro_creditos.cod_credito) THEN
                Cmb_Producto:ADD-LAST(STRING(Pro_creditos.cod_credito,"999") + "-" + Pro_Creditos.Nom_Producto).
        END.
    END.

    IF AVAILABLE(Liqui_Int) AND Cmb_Producto:LIST-ITEMS NE "" THEN DO:
        ASSIGN Cmb_Producto:SCREEN-VALUE = Cmb_Producto:ENTRY(1).
        APPLY "VALUE-CHANGED" TO Cmb_Producto.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Producto V-table-Win
ON VALUE-CHANGED OF Cmb_Producto IN FRAME F-Main /* Ahorros */
DO:
  IF AVAILABLE(Liqui_Int) THEN
    ASSIGN Liqui_Int.Cod_Producto:SCREEN-VALUE = SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.Cod_base
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.Cod_base V-table-Win
ON ENTRY OF Liqui_Int.Cod_base IN FRAME F-Main /* Código Base Retención */
DO:
    DO WITH FRAME {&FRAME-NAME}:
       FIND Base_Ret WHERE Base_Ret.Cod_base EQ Liqui_Int.Cod_Base:SCREEN-VALUE 
            NO-ERROR.
       IF AVAILABLE(Base_Ret) THEN
          ASSIGN W-nomret:SCREEN-VALUE = Base_Ret.Nombre.
       ELSE
          ASSIGN W-nomret:SCREEN-VALUE = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.Cod_base V-table-Win
ON LEAVE OF Liqui_Int.Cod_base IN FRAME F-Main /* Código Base Retención */
DO:
    IF Liqui_Int.cod_Base:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
       FIND Base_Ret WHERE Base_Ret.Cod_base EQ Liqui_Int.Cod_Base:SCREEN-VALUE NO-ERROR.
       IF AVAILABLE Base_Ret THEN DO:
          ASSIGN W-nomret:SCREEN-VALUE  = Base_Ret.Nombre
                 W_Porcent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Base_Ret.Porcentaje)
                 W_CodBase              = Liqui_Int.Cod_Base:SCREEN-VALUE.
       END.
       ELSE
          APPLY "MOUSE-SELECT-DBLCLICK" TO SELF.          
    END.
    ELSE
        ASSIGN W-nomret:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.Cod_base V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.Cod_base IN FRAME F-Main /* Código Base Retención */
DO:
  
   RUN C-Bases.r (OUTPUT W_Codbase, OUTPUT W-nomret, OUTPUT W_Porcent).
    IF W_Codbase EQ "" THEN DO:
       FIND Base_Ret WHERE Base_Ret.Cod_base EQ Liqui_Int.Cod_Base:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
       IF NOT AVAILABLE(Base_Ret) THEN
          ASSIGN Liqui_Int.Cod_Base:SCREEN-VALUE = ""
                 W-nomret:SCREEN-VALUE           = "".
    END.
    ELSE
       ASSIGN Liqui_Int.Cod_Base:SCREEN-VALUE     = W_Codbase
              W-nomret:SCREEN-VALUE               = W-nomret.

    DISPLAY W_Porcent WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaCr_DifCob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_DifCob V-table-Win
ON ENTRY OF Liqui_Int.CtaCr_DifCob IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_DifCob:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_DifCob V-table-Win
ON LEAVE OF Liqui_Int.CtaCr_DifCob IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_DifCob:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_DifCob V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaCr_DifCob IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_DifCob:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaCr_DifCob:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaCr_DifCobAso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_DifCobAso V-table-Win
ON ENTRY OF Liqui_Int.CtaCr_DifCobAso IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_DifCobAso:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_DifCobAso V-table-Win
ON LEAVE OF Liqui_Int.CtaCr_DifCobAso IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_DifcobAso:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_DifCobAso V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaCr_DifCobAso IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_DifCobAso:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaCr_DifCobAso:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaCr_Liq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_Liq V-table-Win
ON ENTRY OF Liqui_Int.CtaCr_Liq IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_Liq:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_Liq V-table-Win
ON LEAVE OF Liqui_Int.CtaCr_Liq IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_Liq:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_Liq V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaCr_Liq IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_Liq:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaCr_Liq:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaCr_LiqAso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_LiqAso V-table-Win
ON ENTRY OF Liqui_Int.CtaCr_LiqAso IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_LiqAso:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_LiqAso V-table-Win
ON LEAVE OF Liqui_Int.CtaCr_LiqAso IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_LiqAso:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_LiqAso V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaCr_LiqAso IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_LiqAso:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaCr_LiqAso:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaCr_Mora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_Mora V-table-Win
ON ENTRY OF Liqui_Int.CtaCr_Mora IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_Mora:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_Mora V-table-Win
ON LEAVE OF Liqui_Int.CtaCr_Mora IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_Mora:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_Mora V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaCr_Mora IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_Mora:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaCr_Mora:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaCr_MoraAso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_MoraAso V-table-Win
ON ENTRY OF Liqui_Int.CtaCr_MoraAso IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_MoraAso:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_MoraAso V-table-Win
ON LEAVE OF Liqui_Int.CtaCr_MoraAso IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_MoraAso:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_MoraAso V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaCr_MoraAso IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_MoraAso:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaCr_MoraAso:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaCr_Ret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_Ret V-table-Win
ON ENTRY OF Liqui_Int.CtaCr_Ret IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_Ret:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_Ret V-table-Win
ON LEAVE OF Liqui_Int.CtaCr_Ret IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_Ret:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaCr_Ret V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaCr_Ret IN FRAME F-Main /* Crédito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaCr_Ret:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaCr_Ret:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaDb_DifCob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_DifCob V-table-Win
ON ENTRY OF Liqui_Int.CtaDb_DifCob IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_DifCob:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_DifCob V-table-Win
ON LEAVE OF Liqui_Int.CtaDb_DifCob IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_DifCob:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_DifCob V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaDb_DifCob IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_DifCob:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaDb_DifCob:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaDb_DifCobAso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_DifCobAso V-table-Win
ON ENTRY OF Liqui_Int.CtaDb_DifCobAso IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_DifCobAso:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_DifCobAso V-table-Win
ON LEAVE OF Liqui_Int.CtaDb_DifCobAso IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_DifcobAso:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_DifCobAso V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaDb_DifCobAso IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_DifCobAso:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaDb_DifCobAso:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaDb_Liq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_Liq V-table-Win
ON ENTRY OF Liqui_Int.CtaDb_Liq IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_Liq:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_Liq V-table-Win
ON LEAVE OF Liqui_Int.CtaDb_Liq IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_Liq:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_Liq V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaDb_Liq IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_Liq:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaDb_Liq:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaDb_LiqAso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_LiqAso V-table-Win
ON ENTRY OF Liqui_Int.CtaDb_LiqAso IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_LiqAso:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_LiqAso V-table-Win
ON LEAVE OF Liqui_Int.CtaDb_LiqAso IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_LiqAso:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_LiqAso V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaDb_LiqAso IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_LiqAso:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaDb_LiqAso:SCREEN-VALUE = W_DispCta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaDb_Mora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_Mora V-table-Win
ON ENTRY OF Liqui_Int.CtaDb_Mora IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_Mora:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_Mora V-table-Win
ON LEAVE OF Liqui_Int.CtaDb_Mora IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_Mora:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_Mora V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaDb_Mora IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_Mora:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaDb_Mora:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaDb_MoraAso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_MoraAso V-table-Win
ON ENTRY OF Liqui_Int.CtaDb_MoraAso IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_MoraAso:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_MoraAso V-table-Win
ON LEAVE OF Liqui_Int.CtaDb_MoraAso IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_MoraAso:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_MoraAso V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaDb_MoraAso IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_MoraAso:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaDb_MoraAso:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaDb_Ret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_Ret V-table-Win
ON ENTRY OF Liqui_Int.CtaDb_Ret IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_Ret:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_Ret V-table-Win
ON LEAVE OF Liqui_Int.CtaDb_Ret IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_Ret:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaDb_Ret V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaDb_Ret IN FRAME F-Main /* Débito */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaDb_Ret:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaDb_Ret:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaInt_Ant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaInt_Ant V-table-Win
ON ENTRY OF Liqui_Int.CtaInt_Ant IN FRAME F-Main /* No Asociados */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaInt_Ant:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaInt_Ant V-table-Win
ON LEAVE OF Liqui_Int.CtaInt_Ant IN FRAME F-Main /* No Asociados */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaInt_Ant:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaInt_Ant V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaInt_Ant IN FRAME F-Main /* No Asociados */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaInt_Ant:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaInt_Ant:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.CtaInt_AntAso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaInt_AntAso V-table-Win
ON ENTRY OF Liqui_Int.CtaInt_AntAso IN FRAME F-Main /* Asociados */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaInt_AntAso:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaInt_AntAso V-table-Win
ON LEAVE OF Liqui_Int.CtaInt_AntAso IN FRAME F-Main /* Asociados */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaInt_AntAso:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.CtaInt_AntAso V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.CtaInt_AntAso IN FRAME F-Main /* Asociados */
DO:
    ASSIGN W_ConsCta = Liqui_Int.CtaInt_AntAso:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.CtaInt_AntAso:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.Cta_CauCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.Cta_CauCr V-table-Win
ON ENTRY OF Liqui_Int.Cta_CauCr IN FRAME F-Main /* Cuenta Crédito Causaciòn */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.Cta_CauCr V-table-Win
ON LEAVE OF Liqui_Int.Cta_CauCr IN FRAME F-Main /* Cuenta Crédito Causaciòn */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.Cta_CauCr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.Cta_CauCr IN FRAME F-Main /* Cuenta Crédito Causaciòn */
DO:
    ASSIGN W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN SELF:SCREEN-VALUE = W_DispCta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Liqui_Int.Cta_SucyAge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.Cta_SucyAge V-table-Win
ON ENTRY OF Liqui_Int.Cta_SucyAge IN FRAME F-Main /* Sucursales y Agencias */
DO:
    ASSIGN W_ConsCta = Liqui_Int.Cta_SucyAge:SCREEN-VALUE. 
    RUN Cuentasentry(INPUT W_ConsCta). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.Cta_SucyAge V-table-Win
ON LEAVE OF Liqui_Int.Cta_SucyAge IN FRAME F-Main /* Sucursales y Agencias */
DO:
    ASSIGN W_ConsCta = Liqui_Int.Cta_SucyAge:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Liqui_Int.Cta_SucyAge V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Liqui_Int.Cta_SucyAge IN FRAME F-Main /* Sucursales y Agencias */
DO:
    ASSIGN W_ConsCta = Liqui_Int.Cta_SucyAge:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    ASSIGN Liqui_Int.Cta_SucyAge:SCREEN-VALUE = W_DispCta.    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Activar V-table-Win 
PROCEDURE Activar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER T_Consu AS LOGICAL.
    ASSIGN W_Consulta = T_Consu.
    IF Liqui_Int.Clase_Producto EQ 2 THEN DO:
      Liqui_Int.CtaDb_Ret:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
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
  {src/adm/template/row-list.i "Liqui_int"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Liqui_int"}

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
          ASSIGN W_NomCuenta:SCREEN-VALUE = Cuentas.Nombre.
       ELSE
          ASSIGN W_NomCuenta:SCREEN-VALUE = "".
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
   RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ).

   Cmb_Producto:SENSITIVE = TRUE.
   IF Liqui_Int.Clase_Producto:SCREEN-VALUE EQ "1" THEN
      ASSIGN F_SobMor:SCREEN-VALUE = "Cuentas Liquidación Interes Sobregiro".
   ELSE
      ASSIGN F_SobMor:SCREEN-VALUE = "Cuentas Liquidación Por Mora".
         
   APPLY "VALUE-CHANGED":U TO Liqui_Int.Clase_Producto.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */

  DEFINE VARIABLE W_CtaTrab    LIKE Cuentas.Cuenta.

  IF INTEGER(Liqui_Int.Clase_Producto:SCREEN-VALUE IN FRAME {&FRAME-NAME}) EQ 2 THEN DO:
     W_CtaTrab = Liqui_Int.CtaDb_Ret:SCREEN-VALUE.
     FIND Cuentas WHERE Cuentas.Cuenta EQ W_CtaTrab 
                    AND Cuentas.Estado EQ 1
                  NO-LOCK NO-ERROR.
     /*IF AVAILABLE(Cuentas) THEN DO:
       IF NOT Cuentas.Id_Nit THEN DO:
          MESSAGE "La cuenta debe manejar Nit. Se deshacen los cambios realizados. " VIEW-AS ALERT-BOX
          TITLE "Validación de Cuentas".
          APPLY "ENTRY" TO Liqui_Int.CtaDb_Ret.
          RETURN NO-APPLY.
       END.
     END.*/
  END.
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
FIND CURRENT Liqui_int SHARE-LOCK NO-ERROR NO-WAIT.
     IF AVAILABLE(Liqui_int) THEN DO:    
        RUN MostrarMensaje IN W_Manija (INPUT 25, OUTPUT W_Val).
        IF W_Val THEN DO:
          RUN P-GraLog IN W_Manija (INPUT "ADV: BORRA Registro, LiqInteres. Clase_Producto: " + STRING(Liqui_Int.Clase_Producto) + 
              " - Cod_Producto: " + STRING(Liqui_Int.Cod_Producto)).
          RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ).
        END.
        FIND CURRENT Liqui_int NO-LOCK NO-ERROR.
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
    RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ) .
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
DEFINE VAR W_CodPto AS INTEGER FORMAT "999".
DEFINE VAR W_NomPto AS CHARACTER FORMAT "X(40)".

DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).

    APPLY "VALUE-CHANGED":U TO Liqui_Int.Clase_Producto.

    IF AVAILABLE(Liqui_Int) THEN DO:
        IF Liqui_Int.Clase_Producto GT 0 THEN
            ASSIGN Liqui_Int.Clase_Producto:SCREEN-VALUE = STRING(Liqui_Int.Clase_Producto).

        IF W_Consulta EQ FALSE THEN DO:
            IF Liqui_Int.Clase_Producto EQ 1 THEN
                ASSIGN Liqui_Int.CtaCr_Ret:SENSITIVE = TRUE
                       Liqui_Int.CtaDb_Ret:SENSITIVE = TRUE
                       Liqui_Int.Base:SENSITIVE = TRUE
                       Liqui_Int.Cod_base:SENSITIVE = TRUE
                       Liqui_Int.CtaDb_DifcobAso:SENSITIVE = FALSE
                       Liqui_Int.CtaCr_DifcobAso:SENSITIVE = FALSE
                       Liqui_Int.CtaDb_DifCob:SENSITIVE = FALSE
                       Liqui_Int.CtaCr_DifCob:SENSITIVE = FALSE
                       F_Titcxc:VISIBLE = FALSE
                       F_Titcxc-2:VISIBLE = TRUE.
            ELSE
                ASSIGN Liqui_Int.CtaCr_Ret:SENSITIVE = FALSE
                       Liqui_Int.CtaDb_Ret:SENSITIVE = TRUE
                       Liqui_Int.Base:SENSITIVE = FALSE
                       Liqui_Int.Cod_base:SENSITIVE = FALSE
                       Liqui_Int.CtaInt_AntAso:SENSITIVE = TRUE
                       Liqui_Int.CtaInt_Ant:SENSITIVE = TRUE
                       Liqui_Int.CtaCr_LiqAso:SENSITIVE = TRUE
                       Liqui_Int.CtaDb_LiqAso:SENSITIVE = TRUE
                       Liqui_Int.CtaCr_Liq:SENSITIVE = TRUE
                       Liqui_Int.CtaDb_Liq:SENSITIVE = TRUE
                       Liqui_Int.CtaDb_MoraAso:SENSITIVE = TRUE
                       Liqui_Int.CtaCr_MoraAso:SENSITIVE = TRUE
                       Liqui_Int.CtaDb_Mora:SENSITIVE = TRUE
                       Liqui_Int.CtaCr_Mora:SENSITIVE = TRUE
                       Liqui_Int.CtaDb_DifcobAso:SENSITIVE = TRUE
                       Liqui_Int.CtaCr_DifcobAso:SENSITIVE = TRUE
                       Liqui_Int.CtaDb_DifCob:SENSITIVE = TRUE
                       Liqui_Int.CtaCr_DifCob:SENSITIVE = TRUE
                       F_Titcxc:VISIBLE = TRUE
                       F_Titcxc-2:VISIBLE = FALSE.
        END.

        ASSIGN Cmb_Producto:LIST-ITEMS = "".

        IF Liqui_Int.Clase_Producto EQ 1 THEN DO:
            ASSIGN Cmb_Producto:LABEL = "Ahorros"
                   F_SobMor:SCREEN-VALUE = "Cuentas Liquidación Interes Sobregiro"
                   F_Titcxc:VISIBLE = FALSE
                   F_Titcxc-2:VISIBLE = TRUE.

            FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 AND Pro_ahorros.tip_ahorro NE 4 NO-LOCK BREAK BY Pro_ahorros.cod_ahorro:
                IF FIRST-OF(Pro_ahorros.cod_ahorro) THEN DO:
                    IF Pro_ahorros.cod_ahorro EQ Liqui_Int.Cod_Producto THEN
                        ASSIGN W_CodPto = Pro_ahorros.cod_ahorro
                               W_NomPto = Pro_Ahorros.Nom_Producto.

                    Cmb_Producto:ADD-LAST(STRING(Pro_ahorros.cod_ahorro,"999") + "-" + Pro_Ahorros.Nom_Producto).
                END.
            END.

            FIND FIRST Base_Ret WHERE Base_Ret.Cod_base EQ Liqui_Int.Cod_Base NO-LOCK NO-ERROR.
            IF AVAILABLE(Base_Ret) THEN
                ASSIGN W-nomret:SCREEN-VALUE = Base_Ret.Nombre
                       W_porcent:SCREEN-VALUE = STRING(Base_Ret.Porcentaje).
        END.
        ELSE DO:
            ASSIGN Cmb_Producto:LABEL = "Créditos"
                   F_SobMor:SCREEN-VALUE = "Cuentas Liquidación Por Mora"
                   F_Titcxc:VISIBLE = TRUE
                   F_Titcxc-2:VISIBLE = FALSE.

            FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK BREAK BY Pro_creditos.cod_credito:
                IF FIRST-OF(Pro_creditos.cod_credito) THEN DO:
                    IF Pro_creditos.cod_credito EQ Liqui_Int.Cod_Producto THEN
                        ASSIGN W_CodPto = Pro_creditos.cod_credito
                               W_NomPto = Pro_Creditos.Nom_Producto.

                    Cmb_Producto:ADD-LAST(STRING(Pro_creditos.cod_credito,"999") + "-" + Pro_Creditos.Nom_Producto).
                END.
            END.

            ASSIGN W_Porcent:SCREEN-VALUE = ""
                   W-nomret:SCREEN-VALUE  = "".
        END.

        IF W_CodPto GT 0 THEN
            ASSIGN Cmb_Producto:SCREEN-VALUE = STRING(W_CodPto,"999") + "-" + W_NomPto
                   Liqui_Int.Cod_Producto:SCREEN-VALUE  = SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  /* Code placed here will execute AFTER standard behavior.    */

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
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ) .
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

  /* Dispatch standard ADM method.  */

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  IF RETURN-VALUE EQ "ADM-ERROR":U AND 
                                       Liqui_Int.Clase_Producto:SENSITIVE IN FRAME {&FRAME-NAME} EQ FALSE THEN
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ).                            
  
  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN
   RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, LiqInteres. Clase_Producto: " + STRING(Liqui_Int.Clase_Producto) + 
       " - Cod_Producto: " + STRING(Liqui_Int.Cod_Producto)).
  ELSE
    RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, LiqInteres. Clase_Producto: " + STRING(Liqui_Int.Clase_Producto) + 
        " - Cod_Producto: " + STRING(Liqui_Int.Cod_Producto)).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Modbclkcta V-table-Win 
PROCEDURE Modbclkcta :
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
         ASSIGN T_ConsCta1               = ""
                W_NomCuenta:SCREEN-VALUE = "".           
      END.
      ELSE
         ASSIGN T_ConsCta1               = W_Pcuenta
                W_NomCuenta:SCREEN-VALUE = W_Pnombre.
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
  {src/adm/template/snd-list.i "Liqui_int"}

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

