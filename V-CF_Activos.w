&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
   DEFINE VAR W_Ok AS LOGICAL.
   DEFINE VAR W_Pcuenta    LIKE Cuenta.Cuenta.
   DEFINE VAR W_Pnombre    LIKE Cuenta.Nombre.
   DEFINE VAR W_Naturaleza LIKE Cuenta.Naturaleza.
   DEFINE VAR W_Ptipo      LIKE Varios.Tipo.
   DEFINE VAR W_Pcodigo    LIKE Varios.Codigo.
   DEFINE VAR W_CtrNat     LIKE Cuenta.Ctr_Naturaleza.
   DEFINE VAR W_IdDep      LIKE Cuenta.Id_Base.
   DEFINE VAR W_Error      AS   LOGICAL.
   DEFINE VAR W_Metodo     AS   LOGICAL.
   DEFINE VAR W_CbStr      AS   CHARACTER.
   DEFINE VAR W_HayError   AS   LOGICAL.
   DEFINE VAR W_SiHayError AS   LOGICAL.
   DEFINE VAR J            AS   INTEGER.
   DEFINE VAR W_CtaSal     LIKE Cuentas.Cuenta.
   DEFINE VAR W_PnomCta    LIKE Cuenta.Nombre.
   DEFINE VAR W_Rowid      AS   ROWID.
   DEFINE VAR W_CtaProvAux LIKE Cuentas.Cuenta.
   DEFINE VAR W_CtaValoAux LIKE Cuentas.Cuenta.
   DEFINE VAR W_NoHayCtas  AS   LOGICAL.
   DEFINE VAR W_Adicion    AS   LOGICAL.

  {incluido\variable.i "SHARED"}

    DEFINE VARIABLE P_Codbase LIKE Base_Ret.Cod_Base.
    DEFINE VARIABLE P_Nombre  LIKE Base_Ret.Nombre.
    DEFINE VARIABLE P_Porcent LIKE Base_Ret.Porcentaje.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES cfg_activosFijos
&Scoped-define FIRST-EXTERNAL-TABLE cfg_activosFijos


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cfg_activosFijos.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS cfg_activosFijos.Cta_Fuente cfg_activosFijos.CtaDepAcum ~
cfg_activosFijos.CtaDepMes cfg_activosFijos.CtaOrdFDepDb cfg_activosFijos.CtaOrdFDepCr ~
cfg_activosFijos.CtaDbPig cfg_activosFijos.CtaCrPig cfg_activosFijos.CtaDbPre ~
cfg_activosFijos.CtaCrPre cfg_activosFijos.CtaProvision cfg_activosFijos.CtaGanancia ~
cfg_activosFijos.CtaPerdida cfg_activosFijos.Iva cfg_activosFijos.IvaRetenido ~
cfg_activosFijos.IvaTransitorioRC cfg_activosFijos.IvatransitorioRs cfg_activosFijos.RteFte ~
cfg_activosFijos.CtaSucAge 
&Scoped-define ENABLED-TABLES cfg_activosFijos
&Scoped-define FIRST-ENABLED-TABLE cfg_activosFijos
&Scoped-Define ENABLED-OBJECTS Cmb_Comprobantes NomIvaRet cta1 ~
W_CtaValorizacion cta2 cta3 cta8 cta9 cta6 cta7 cta5 cta4 cta10 cta11 cta12 ~
cta13 RECT-166 RECT-167 RECT-169 RECT-18 RECT-3 
&Scoped-Define DISPLAYED-FIELDS cfg_activosFijos.Cta_Fuente cfg_activosFijos.CtaDepAcum ~
cfg_activosFijos.CtaDepMes cfg_activosFijos.CtaOrdFDepDb cfg_activosFijos.CtaOrdFDepCr ~
cfg_activosFijos.CtaDbPig cfg_activosFijos.CtaCrPig cfg_activosFijos.CtaDbPre ~
cfg_activosFijos.CtaCrPre cfg_activosFijos.CtaProvision cfg_activosFijos.CtaGanancia ~
cfg_activosFijos.CtaPerdida cfg_activosFijos.Iva cfg_activosFijos.IvaRetenido ~
cfg_activosFijos.IvaTransitorioRC cfg_activosFijos.IvatransitorioRs cfg_activosFijos.RteFte ~
cfg_activosFijos.CtaSucAge 
&Scoped-define DISPLAYED-TABLES cfg_activosFijos
&Scoped-define FIRST-DISPLAYED-TABLE cfg_activosFijos
&Scoped-Define DISPLAYED-OBJECTS NomCtaSucAge Cmb_Comprobantes NomCodIva ~
NomCtaRet NomIvaRet NomRC NomRS cta1 W_CmbCod Tg_IdDeprec W_CtaValorizacion ~
cta2 cta3 cta8 cta9 cta6 cta7 cta5 cta4 cta10 cta11 cta12 cta13 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS W_CmbCod Tg_IdDeprec 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
Grupo|y|y|BdCentral.cfg_activosFijos.Grupo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "Grupo",
     Keys-Supplied = "Grupo"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE Cmb_Comprobantes AS CHARACTER FORMAT "X(256)":U INITIAL "00 - No Asignado" 
     LABEL "Comprobante" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00 - No Asignado" 
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CmbCod AS CHARACTER FORMAT "X(40)":U 
     LABEL "Grupo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta10 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta11 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta12 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta13 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta3 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta4 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta5 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta6 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 27 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta7 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 27 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta8 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cta9 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCodIva AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCtaRet AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCtaSucAge AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomIvaRet AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 28.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomRC AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomRS AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaValorizacion AS CHARACTER FORMAT "X(14)" 
     LABEL "Valorización" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-166
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30 BY 9.42.

DEFINE RECTANGLE RECT-167
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33 BY 4.31.

DEFINE RECTANGLE RECT-169
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30 BY 4.31.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 29 BY 4.31.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 2.42.

DEFINE VARIABLE Tg_IdDeprec AS LOGICAL INITIAL yes 
     LABEL "Maneja Depreciación" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .77.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     NomCtaSucAge AT ROW 18.31 COL 2.14 COLON-ALIGNED NO-LABEL
     Cmb_Comprobantes AT ROW 20.12 COL 10 COLON-ALIGNED
     NomCodIva AT ROW 13.23 COL 61 COLON-ALIGNED NO-LABEL
     NomCtaRet AT ROW 11.08 COL 61 COLON-ALIGNED NO-LABEL
     NomIvaRet AT ROW 15.35 COL 61 COLON-ALIGNED NO-LABEL
     NomRC AT ROW 17.54 COL 61 COLON-ALIGNED NO-LABEL
     NomRS AT ROW 19.65 COL 61 COLON-ALIGNED NO-LABEL
     cta1 AT ROW 4.04 COL 2 COLON-ALIGNED NO-LABEL
     W_CmbCod AT ROW 1.27 COL 9
     Tg_IdDeprec AT ROW 1.27 COL 62
     cfg_activosFijos.Cta_Fuente AT ROW 3.15 COL 17 COLON-ALIGNED
          LABEL "Cta Fuente"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaDepAcum AT ROW 3.15 COL 46 COLON-ALIGNED
          LABEL "Dep.Acumulada"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaDepMes AT ROW 3.15 COL 77 COLON-ALIGNED
          LABEL "Gto.Depreciación"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaOrdFDepDb AT ROW 5.85 COL 16 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaOrdFDepCr AT ROW 7.73 COL 16 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaDbPig AT ROW 5.85 COL 45 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaCrPig AT ROW 7.65 COL 45 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaDbPre AT ROW 5.85 COL 76 COLON-ALIGNED
          LABEL "Débito"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaCrPre AT ROW 7.73 COL 76 COLON-ALIGNED
          LABEL "Crédito"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaProvision AT ROW 10.42 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.86 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaGanancia AT ROW 13.85 COL 16 COLON-ALIGNED
          LABEL "Ganancia por venta"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     W_CtaValorizacion AT ROW 12.12 COL 16 COLON-ALIGNED HELP
          "Ingrese la Cuenta de Valorización del activo"
     cfg_activosFijos.CtaPerdida AT ROW 15.62 COL 16 COLON-ALIGNED
          LABEL "Perdidas por venta"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cta2 AT ROW 4.04 COL 31 COLON-ALIGNED NO-LABEL
     cta3 AT ROW 4.04 COL 61 COLON-ALIGNED NO-LABEL
     cta8 AT ROW 6.73 COL 61 COLON-ALIGNED NO-LABEL
     cta9 AT ROW 8.62 COL 61 COLON-ALIGNED NO-LABEL
     cta6 AT ROW 6.65 COL 32 COLON-ALIGNED NO-LABEL
     cta7 AT ROW 8.58 COL 32 COLON-ALIGNED NO-LABEL
     cta5 AT ROW 8.62 COL 2 COLON-ALIGNED NO-LABEL
     cta4 AT ROW 6.73 COL 2 COLON-ALIGNED NO-LABEL
     cta10 AT ROW 11.23 COL 2 COLON-ALIGNED NO-LABEL
     cta11 AT ROW 14.73 COL 2 COLON-ALIGNED NO-LABEL
     cta12 AT ROW 12.96 COL 2 COLON-ALIGNED NO-LABEL
     cta13 AT ROW 16.46 COL 2 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cfg_activosFijos.Iva AT ROW 12.31 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.IvaRetenido AT ROW 14.46 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.IvaTransitorioRC AT ROW 16.62 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.IvatransitorioRs AT ROW 18.77 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.RteFte AT ROW 10.15 COL 76 COLON-ALIGNED
          LABEL "Cuenta Base Retención"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     cfg_activosFijos.CtaSucAge AT ROW 17.42 COL 16 COLON-ALIGNED
          LABEL "Cta.Suc y Agencias"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     RECT-166 AT ROW 10.15 COL 3
     RECT-167 AT ROW 5.31 COL 62
     RECT-169 AT ROW 5.31 COL 3
     RECT-18 AT ROW 5.31 COL 33
     RECT-3 AT ROW 2.62 COL 3
     " Cuentas de Depreciación" VIEW-AS TEXT
          SIZE 24 BY 1.04 AT ROW 2.08 COL 5
          FGCOLOR 7 FONT 5
     "Orden Depreciación" VIEW-AS TEXT
          SIZE 17.86 BY .65 AT ROW 5.04 COL 6
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Pignoración" VIEW-AS TEXT
          SIZE 11 BY .65 AT ROW 5.04 COL 36
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Préstamo" VIEW-AS TEXT
          SIZE 9 BY .65 AT ROW 5.04 COL 67
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Otras Cuentas" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 9.62 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: BdCentral.cfg_activosFijos
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
         HEIGHT             = 20.23
         WIDTH              = 95.86.
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
   Size-to-Fit Custom                                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaCrPig IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaCrPre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaDbPig IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaDbPre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaDepAcum IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaDepMes IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaGanancia IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaOrdFDepCr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaOrdFDepDb IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaPerdida IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.CtaSucAge IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.Cta_Fuente IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN NomCodIva IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCtaRet IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCtaSucAge IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomRC IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomRS IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cfg_activosFijos.RteFte IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Tg_IdDeprec IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX W_CmbCod IN FRAME F-Main
   NO-ENABLE ALIGN-L 1                                                  */
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

&Scoped-define SELF-NAME cfg_activosFijos.CtaCrPig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaCrPig V-table-Win
ON ENTRY OF cfg_activosFijos.CtaCrPig IN FRAME F-Main /* Crédito */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaCrPig, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaCrPig:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta7 = W_PnomCta.
  DISPLAY cta7 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaCrPig V-table-Win
ON TAB OF cfg_activosFijos.CtaCrPig IN FRAME F-Main /* Crédito */
OR RETURN OF cfg_activosFijos.CtaCrPig DO:
  RUN Buscar_CuentaOrd (INPUT INPUT cfg_activosFijos.CtaCrPig, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.CtaCrPig:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta7 = W_PnomCta.
  DISPLAY cta7 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaCrPre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaCrPre V-table-Win
ON ENTRY OF cfg_activosFijos.CtaCrPre IN FRAME F-Main /* Crédito */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaCrPre, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaCrPre:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta9 = W_PnomCta.
  DISPLAY cta9 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaCrPre V-table-Win
ON TAB OF cfg_activosFijos.CtaCrPre IN FRAME F-Main /* Crédito */
OR RETURN OF cfg_activosFijos.CtaCrPre DO:
  RUN Buscar_CuentaOrd (INPUT INPUT cfg_activosFijos.CtaCrPre, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.CtaCrPre:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta9 = W_PnomCta.
  DISPLAY cta9 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaDbPig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaDbPig V-table-Win
ON ENTRY OF cfg_activosFijos.CtaDbPig IN FRAME F-Main /* Débito */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaDbPig, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaDbPig:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta6 = W_PnomCta.
  DISPLAY cta6 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaDbPig V-table-Win
ON TAB OF cfg_activosFijos.CtaDbPig IN FRAME F-Main /* Débito */
OR RETURN OF cfg_activosFijos.CtaDbPig DO:
  RUN Buscar_CuentaOrd (INPUT INPUT cfg_activosFijos.CtaDbPig, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.CtaDbPig:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta6 = W_PnomCta.
  DISPLAY cta6 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaDbPre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaDbPre V-table-Win
ON ENTRY OF cfg_activosFijos.CtaDbPre IN FRAME F-Main /* Débito */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaDbPre, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaDbPre:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta8 = W_PnomCta.
  DISPLAY cta8 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaDbPre V-table-Win
ON TAB OF cfg_activosFijos.CtaDbPre IN FRAME F-Main /* Débito */
OR RETURN OF cfg_activosFijos.CtaDbPRe DO:
  RUN Buscar_CuentaOrd (INPUT INPUT cfg_activosFijos.CtaDbPre, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.CtaDbPre:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta8 = W_PnomCta.
  DISPLAY cta8 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaDepAcum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaDepAcum V-table-Win
ON ENTRY OF cfg_activosFijos.CtaDepAcum IN FRAME F-Main /* Dep.Acumulada */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaDepAcum, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaDepAcum:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta2 = W_PnomCta.
 DISPLAY cta2 WITH FRAME {&FRAME-NAME}.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaDepAcum V-table-Win
ON TAB OF cfg_activosFijos.CtaDepAcum IN FRAME F-Main /* Dep.Acumulada */
OR RETURN OF cfg_activosFijos.CtaDepAcum DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaDepAcum, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.CtaDepAcum:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta2 = W_PnomCta.
  DISPLAY cta2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaDepMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaDepMes V-table-Win
ON ENTRY OF cfg_activosFijos.CtaDepMes IN FRAME F-Main /* Gto.Depreciación */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaDepMes, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaDepMes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta3 = W_PnomCta.
  DISPLAY cta3 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaDepMes V-table-Win
ON TAB OF cfg_activosFijos.CtaDepMes IN FRAME F-Main /* Gto.Depreciación */
OR RETURN OF cfg_activosFijos.CtaDepMes DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaDepMes, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.CtaDepMes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta3 = W_PnomCta.
  DISPLAY cta3 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaGanancia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaGanancia V-table-Win
ON ENTRY OF cfg_activosFijos.CtaGanancia IN FRAME F-Main /* Ganancia por venta */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.Ctaganancia, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.Ctaganancia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta11 = W_PnomCta.
  DISPLAY cta11 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaGanancia V-table-Win
ON TAB OF cfg_activosFijos.CtaGanancia IN FRAME F-Main /* Ganancia por venta */
OR RETURN OF cfg_activosFijos.CtaGanancia DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaGanancia, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.CtaGanancia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta11 = W_PnomCta.
  DISPLAY cta11 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaOrdFDepCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaOrdFDepCr V-table-Win
ON ENTRY OF cfg_activosFijos.CtaOrdFDepCr IN FRAME F-Main /* Crédito */
DO:
  RUN Buscar_CuentaOrd (INPUT INPUT cfg_activosFijos.CtaOrdFDepCr, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaOrdFDepCr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta5 = W_PnomCta.
  DISPLAY cta5 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaOrdFDepCr V-table-Win
ON TAB OF cfg_activosFijos.CtaOrdFDepCr IN FRAME F-Main /* Crédito */
OR RETURN OF cfg_activosFijos.CtaOrdFDepCr DO:
  RUN Buscar_CuentaOrd (INPUT INPUT cfg_activosFijos.CtaOrdFDepCr, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.CtaOrdFDepCr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta7 = W_PnomCta.
  DISPLAY cta7 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaOrdFDepDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaOrdFDepDb V-table-Win
ON ENTRY OF cfg_activosFijos.CtaOrdFDepDb IN FRAME F-Main /* Débito */
DO:
  RUN Buscar_CuentaOrd (INPUT INPUT cfg_activosFijos.CtaOrdFDepDB, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaOrdFDepDB:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta4 = W_PnomCta.
  DISPLAY cta4 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaOrdFDepDb V-table-Win
ON TAB OF cfg_activosFijos.CtaOrdFDepDb IN FRAME F-Main /* Débito */
OR RETURN OF cfg_activosFijos.CtaOrdFDepDB DO:
  RUN Buscar_CuentaOrd (INPUT INPUT cfg_activosFijos.CtaOrdFDepDB, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.CtaOrdFDepDB:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta4 = W_PnomCta.
  DISPLAY cta4 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaPerdida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaPerdida V-table-Win
ON ENTRY OF cfg_activosFijos.CtaPerdida IN FRAME F-Main /* Perdidas por venta */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaPerdida, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaPerdida:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
      Cta13 = W_PnomCta.
  DISPLAY cta13 WITH FRAME {&FRAME-NAME}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaPerdida V-table-Win
ON TAB OF cfg_activosFijos.CtaPerdida IN FRAME F-Main /* Perdidas por venta */
OR RETURN OF cfg_activosFijos.CtaPerdida DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaPerdida, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.CtaPerdida:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta13 = W_PnomCta.
  DISPLAY cta13 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaProvision
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaProvision V-table-Win
ON ENTRY OF cfg_activosFijos.CtaProvision IN FRAME F-Main /* Cta.Provision */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.Ctaprovision, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaProvision:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta10 = W_PnomCta.
  DISPLAY cta10 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaProvision V-table-Win
ON TAB OF cfg_activosFijos.CtaProvision IN FRAME F-Main /* Cta.Provision */
OR RETURN OF cfg_activosFijos.CtaProvision DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaProvision, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

 ASSIGN cfg_activosFijos.CtaProvision:SCREEN-VALUE = W_CtaSal
         Cta10 = W_PnomCta.
 DISPLAY cta10 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.CtaSucAge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.CtaSucAge V-table-Win
ON LEAVE OF cfg_activosFijos.CtaSucAge IN FRAME F-Main /* Cta.Suc y Agencias */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.CtaSucAge, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.CtaSucAge:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         NomCtaSucAge = W_PnomCta.
  DISPLAY NomCtaSucAge WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.Cta_Fuente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.Cta_Fuente V-table-Win
ON ENTRY OF cfg_activosFijos.Cta_Fuente IN FRAME F-Main /* Cta Fuente */
DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.Cta_Fuente, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN cfg_activosFijos.Cta_Fuente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta1 = W_PnomCta.
 DISPLAY cta1 WITH FRAME {&FRAME-NAME}.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.Cta_Fuente V-table-Win
ON TAB OF cfg_activosFijos.Cta_Fuente IN FRAME F-Main /* Cta Fuente */
OR RETURN OF cfg_activosFijos.Cta_Fuente DO:
  RUN Buscar_Cuenta (INPUT INPUT cfg_activosFijos.Cta_Fuente, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN cfg_activosFijos.Cta_Fuente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta1 = W_PnomCta.
 DISPLAY cta1 WITH FRAME {&FRAME-NAME}.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.Iva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.Iva V-table-Win
ON LEAVE OF cfg_activosFijos.Iva IN FRAME F-Main /* Código Base IVA */
DO:
  IF SELF:SCREEN-VALUE EQ "" THEN
    RUN C-Bases.r (OUTPUT P_Codbase, OUTPUT P_Nombre, OUTPUT P_Porcent).
    NomCodIva:SCREEN-VALUE = P_Nombre + " - Porcentaje: " + STRING(P_Porcent).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.Iva V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cfg_activosFijos.Iva IN FRAME F-Main /* Código Base IVA */
DO:
  IF SELF:SCREEN-VALUE EQ "" THEN
    RUN C-Bases.r (OUTPUT P_Codbase, OUTPUT P_Nombre, OUTPUT P_Porcent).
    NomCodIva:SCREEN-VALUE = P_Nombre + " - Porcentaje: " + STRING(P_Porcent).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.IvaRetenido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.IvaRetenido V-table-Win
ON ENTRY OF cfg_activosFijos.IvaRetenido IN FRAME F-Main /* Cta.Iva Retenido */
DO:
  RUN Buscar_Cuenta (INPUT SELF:SCREEN-VALUE, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         NomIvaRet = W_PnomCta.
  DISPLAY NomIvaRet WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.IvaRetenido V-table-Win
ON LEAVE OF cfg_activosFijos.IvaRetenido IN FRAME F-Main /* Cta.Iva Retenido */
DO:
  IF SELF:SCREEN-VALUE EQ "?" OR SELF:SCREEN-VALUE EQ "" THEN DO:
     APPLY "entry" TO SELF.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.IvaRetenido V-table-Win
ON TAB OF cfg_activosFijos.IvaRetenido IN FRAME F-Main /* Cta.Iva Retenido */
OR RETURN OF SELF DO:
  RUN Buscar_Cuenta (INPUT SELF:SCREEN-VALUE, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

 ASSIGN SELF:SCREEN-VALUE = W_CtaSal
         NomIvaRet = W_PnomCta.
 DISPLAY NomIvaRet WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.IvaTransitorioRC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.IvaTransitorioRC V-table-Win
ON ENTRY OF cfg_activosFijos.IvaTransitorioRC IN FRAME F-Main /* IvaTrans RC */
DO:
  RUN Buscar_Cuenta (INPUT SELF:SCREEN-VALUE, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         NomRC = W_PnomCta.
  DISPLAY NomRC WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.IvaTransitorioRC V-table-Win
ON TAB OF cfg_activosFijos.IvaTransitorioRC IN FRAME F-Main /* IvaTrans RC */
OR RETURN OF SELF DO:
  RUN Buscar_Cuenta (INPUT SELF:SCREEN-VALUE, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

 ASSIGN SELF:SCREEN-VALUE = W_CtaSal
         NomRC = W_PnomCta.
 DISPLAY NomRC WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.IvatransitorioRs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.IvatransitorioRs V-table-Win
ON ENTRY OF cfg_activosFijos.IvatransitorioRs IN FRAME F-Main /* IvaTrans RS */
DO:
  RUN Buscar_Cuenta (INPUT SELF:SCREEN-VALUE, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         NomRS = W_PnomCta.
  DISPLAY NomRS WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.IvatransitorioRs V-table-Win
ON TAB OF cfg_activosFijos.IvatransitorioRs IN FRAME F-Main /* IvaTrans RS */
OR RETURN OF SELF DO:
  RUN Buscar_Cuenta (INPUT SELF:SCREEN-VALUE, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

 ASSIGN SELF:SCREEN-VALUE = W_CtaSal
         NomRS = W_PnomCta.
 DISPLAY NomRS WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfg_activosFijos.RteFte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.RteFte V-table-Win
ON ENTRY OF cfg_activosFijos.RteFte IN FRAME F-Main /* Cuenta Base Retención */
DO:
  RUN Buscar_Cuenta (INPUT SELF:SCREEN-VALUE, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         NomCtaRet = W_PnomCta.
  DISPLAY NomCtaRet WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfg_activosFijos.RteFte V-table-Win
ON TAB OF cfg_activosFijos.RteFte IN FRAME F-Main /* Cuenta Base Retención */
OR RETURN OF SELF DO:
  RUN Buscar_Cuenta (INPUT SELF:SCREEN-VALUE, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

 ASSIGN SELF:SCREEN-VALUE = W_CtaSal
         NomCtaRet = W_PnomCta.
 DISPLAY NomCtaRet WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_IdDeprec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_IdDeprec V-table-Win
ON LEAVE OF Tg_IdDeprec IN FRAME F-Main /* Maneja Depreciación */
DO:
  APPLY "VALUE-CHANGED" TO Tg_IdDeprec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_IdDeprec V-table-Win
ON VALUE-CHANGED OF Tg_IdDeprec IN FRAME F-Main /* Maneja Depreciación */
DO:
  ASSIGN Tg_IdDeprec.
  RUN Verif_Togle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbCod V-table-Win
ON LEAVE OF W_CmbCod IN FRAME F-Main /* Grupo */
DO:
/*  {incluido\btcancel.i}
    RUN MostrarMensaje IN W_Manija (INPUT 365, OUTPUT W_Error).
    APPLY "ENTRY" TO W_CmbCod IN FRAME {&FRAME-NAME}.
  W_Adicion = FALSE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbCod V-table-Win
ON VALUE-CHANGED OF W_CmbCod IN FRAME F-Main /* Grupo */
DO:
  W_Pcodigo =  INTEGER(SUBSTRING(w_CmbCod:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,5)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaValorizacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaValorizacion V-table-Win
ON ENTRY OF W_CtaValorizacion IN FRAME F-Main /* Valorización */
DO:
  RUN Buscar_Cuenta (INPUT INPUT W_CtaValorizacion, INPUT "ENTRY", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  ASSIGN W_CtaValorizacion:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal.
         Cta12 = W_PnomCta.
  DISPLAY cta12 WITH FRAME {&FRAME-NAME}.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaValorizacion V-table-Win
ON TAB OF W_CtaValorizacion IN FRAME F-Main /* Valorización */
OR RETURN OF W_CtaValorizacion DO:
  ASSIGN W_CtaValorizacion.
  RUN Buscar_Cuenta (INPUT W_CtaValorizacion, INPUT "LEAVE", OUTPUT W_CtaSal, 
                     OUTPUT w_PNomCta, OUTPUT W_HayError).
  IF W_HayError THEN 
    RETURN NO-APPLY.

  ASSIGN W_CtaValorizacion:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CtaSal
         W_CtaValorizacion = W_CtaSal
         Cta12 = W_PnomCta.
  DISPLAY cta12 WITH FRAME {&FRAME-NAME}.            
  IF W_CtaValoAux NE "" THEN DO:
    IF W_CtaValorizacion NE W_CtaValoAux THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 369, OUTPUT W_Eleccion).
      MESSAGE "La Cuenta del Grupo es: " W_CtaValoAux VIEW-AS ALERT-BOX WARNING
              TITLE "Cuenta de Valorización".
      RETURN NO-APPLY.
    END.
  END.  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'Grupo':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = cfg_activosFijos
           &WHERE = "WHERE cfg_activosFijos.Grupo eq INTEGER(key-value)"
       }
  END CASE.

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
  {src/adm/template/row-list.i "cfg_activosFijos"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cfg_activosFijos"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_CtaGrabar V-table-Win 
PROCEDURE Buscar_CtaGrabar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER W_Pcta      LIKE Cuentas.Cuenta.
  DEFINE INPUT  PARAMETER W_deOrden     AS LOGICAL.
  DEFINE OUTPUT PARAMETER W_PhayError   AS LOGICAL INITIAL FALSE.
  
  IF NOT W_DeOrden THEN
     FIND Cuentas WHERE Cuentas.Cuenta EQ W_Pcta
                    AND Cuentas.Tipo   EQ 2
                  NO-LOCK NO-ERROR.
  ELSE
     FIND Cuentas WHERE Cuentas.Cuenta EQ W_Pcta
                    AND Cuentas.Tipo   EQ 2
                    AND Cuentas.Id_Cuenta EQ 5 NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Cuenta) THEN
        W_PhayError = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Cuenta V-table-Win 
PROCEDURE Buscar_Cuenta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER W_Pcta       LIKE Cuentas.Cuenta.
  DEFINE INPUT  PARAMETER W_Pevento      AS CHARACTER FORMAT "X(5)".
  DEFINE OUTPUT PARAMETER W_PSalCta    LIKE Cuentas.Cuenta.
  DEFINE OUTPUT PARAMETER W_PnomCuenta LIKE Cuentas.Nombre INITIAL "".
  DEFINE OUTPUT PARAMETER W_PhayError    AS LOGICAL INITIAL FALSE.
  
  IF W_Pevento = "ENTRY" AND W_Pcta = "" THEN DO:
     W_PhayError = TRUE.
     RETURN.
  END.
  
  W_PsalCta = W_Pcta.
  FIND Cuentas WHERE Cuentas.Cuenta EQ W_Pcta
                 AND Cuentas.Tipo   EQ 2
                 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Cuenta) THEN DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     IF W_Pcuenta = "" THEN
        W_PhayError = TRUE.
     ELSE
        ASSIGN W_Psalcta    = W_Pcuenta
               W_PnomCuenta = W_Pnombre.
  END. 
  ELSE W_PnomCuenta = Cuentas.nombre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_CuentaOrd V-table-Win 
PROCEDURE Buscar_CuentaOrd :
/*------------------------------------------------------------------------------
  Purpose: busca la cuenta de orden
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER W_Pcta       LIKE Cuentas.Cuenta.
  DEFINE INPUT  PARAMETER W_Pevento      AS CHARACTER FORMAT "X(5)".
  DEFINE OUTPUT PARAMETER W_PSalCta    LIKE Cuentas.Cuenta.
  DEFINE OUTPUT PARAMETER W_PnomCuenta LIKE Cuentas.Nombre INITIAL "".
  DEFINE OUTPUT PARAMETER W_PhayError    AS LOGICAL INITIAL FALSE.
  
  IF W_Pevento = "ENTRY" AND W_Pcta = "" THEN DO:
     W_PhayError = TRUE.
     RETURN.
  END.
  
  W_PsalCta = W_Pcta.
  FIND Cuentas WHERE Cuentas.Cuenta    EQ W_Pcta
                 AND Cuentas.Tipo      EQ 2
                 AND Cuentas.Id_Cuenta EQ 5 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Cuenta) THEN DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "R").
     IF W_Pcuenta = "" THEN
        W_PhayError = TRUE.
     ELSE
        ASSIGN W_Psalcta    = W_Pcuenta
               W_PnomCuenta = W_Pnombre.
  END.
  ELSE W_PnomCuenta = Cuentas.nombre.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Chequeo_OtrasCtas V-table-Win 
PROCEDURE Chequeo_OtrasCtas :
/*------------------------------------------------------------------------------
  Purpose:  Verifica que sean cuentas de orden   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME F-MAIN:
     RUN Buscar_CtaGrabar (INPUT INPUT cfg_activosFijos.CtaDbPig, INPUT TRUE, OUTPUT W_SiHayError).
     IF W_SiHayError THEN DO:
        APPLY "ENTRY" TO cfg_activosFijos.CtaDbPig.
        RETURN.
     END.
     RUN Buscar_CtaGrabar (INPUT INPUT cfg_activosFijos.CtaCrPig, INPUT TRUE, OUTPUT W_SiHayError).
     IF W_SiHayError THEN DO:
        APPLY "ENTRY" TO cfg_activosFijos.CtaCrPig.
        RETURN.
     END.
     RUN Buscar_CtaGrabar (INPUT INPUT cfg_activosFijos.CtaDbPre, INPUT TRUE, OUTPUT W_SiHayError).
     IF W_SiHayError THEN DO:
        APPLY "ENTRY" TO cfg_activosFijos.CtaDbPre.
        RETURN.
     END.
     RUN Buscar_CtaGrabar (INPUT INPUT cfg_activosFijos.CtaCrPre, INPUT TRUE, OUTPUT W_SiHayError).
     IF W_SiHayError THEN DO:
        APPLY "ENTRY" TO cfg_activosFijos.CtaCrPre.
        RETURN.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Chequeo_Total V-table-Win 
PROCEDURE Chequeo_Total :
/*------------------------------------------------------------------------------
  Purpose: Chequea la existencia de todas las cuentas.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME F-Main:
 /*   RUN Buscar_CtaGrabar (INPUT INPUT cfg_activosFijos.Cta_Fuente, INPUT FALSE, OUTPUT W_SiHayError).
    IF W_SiHayError THEN DO:
       APPLY "ENTRY" TO cfg_activosFijos.Cta_Fuente.
       RETURN.
    END.
    
    IF Tg_IdDeprec:SCREEN-VALUE EQ "yes" THEN DO:
       RUN Buscar_CtaGrabar (INPUT INPUT cfg_activosFijos.CtaDepAcum, INPUT FALSE, OUTPUT W_SiHayError).
       IF W_SiHayError THEN DO:
          MESSAGE "Cuenta Depreciación Acumulada" SKIP
                  "no puede omitirse. Verifique."
                  VIEW-AS ALERT-BOX INFORMATION TITLE "Error Configuración".       
          APPLY "ENTRY" TO cfg_activosFijos.CtaDepAcum.
          RETURN.
       END.  
       RUN Buscar_CtaGrabar (INPUT INPUT cfg_activosFijos.CtaDepMes, INPUT FALSE, OUTPUT W_SiHayError).
       IF W_SiHayError THEN DO:
          MESSAGE "Cuenta Gasto Depreciación" SKIP
                  "no puede omitirse. Verifique."
                  VIEW-AS ALERT-BOX INFORMATION TITLE "Error Configuración".       
          APPLY "ENTRY" TO cfg_activosFijos.CtaDepMes.
          RETURN.
       END.         
       RUN Buscar_CtaGrabar (INPUT INPUT cfg_activosFijos.CtaGanancia, INPUT FALSE, OUTPUT W_SiHayError).
       IF W_SiHayError THEN DO:
          MESSAGE "Cuenta Ganancia X Venta" SKIP
                  "no puede omitirse. Verifique."
                  VIEW-AS ALERT-BOX INFORMATION TITLE "Error Configuración".
          APPLY "ENTRY" TO cfg_activosFijos.CtaGanancia.
          RETURN.
       END.
       RUN Buscar_CtaGrabar (INPUT INPUT cfg_activosFijos.CtaPerdida, INPUT FALSE, OUTPUT W_SiHayError).
       IF W_SiHayError THEN DO:
          MESSAGE "Cuenta Perdida x Venta" SKIP
                  "no puede omitirse. Verifique."
                  VIEW-AS ALERT-BOX INFORMATION TITLE "Error Configuración".       
          APPLY "ENTRY" TO cfg_activosFijos.CtaPerdida.
          RETURN.
       END.      
    END.*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LlenarCodigos V-table-Win 
PROCEDURE LlenarCodigos :
/*------------------------------------------------------------------------------
  Purpose: Llena los codigos dependiendo de la clase    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    W_CmbCod:LIST-ITEMS = "".            
    FOR EACH Varios FIELDS(Varios.Codigo Varios.Tipo Varios.Estado Varios.Descripcion)
                    WHERE Varios.Tipo   EQ 7
                      AND Varios.Estado EQ 1
                  NO-LOCK:
       W_CbStr = STRING(Varios.Codigo,"99999") + " " +  TRIM(Varios.Descripcion).
       W_Metodo = W_CmbCod:ADD-LAST(W_CbStr).
    END.
    IF W_CmbCod:NUM-ITEMS GT 0 THEN
      ASSIGN  W_CmbCod:SCREEN-VALUE = W_CmbCod:ENTRY(1).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  W_Adicion = TRUE.

  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  ASSIGN W_CtaValorizacion = ""
         cta1 = ' '
         cta2 = ' '   
         cta3 = ' '
         cta4 = ' '   
         cta5 = ' '
         cta6 = ' '   
         cta7 = ' '
         cta8 = ' '   
         cta9 = ' '
         cta10 = ' '   
         cta11 = ' '
         cta12 = ' '     
         cta13 = ' '
         NomCtaSucAge = " "
         W_CtaValoAux = W_CtaValorizacion.
  DISPLAY W_CtaValorizacion WITH FRAME {&FRAME-NAME}.
  ASSIGN Tg_IdDeprec = TRUE.
  RUN Verif_Togle.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 
  RUN Chequeo_Total.

  IF W_SiHayError THEN
     RETURN "ADM-ERROR".

  /*RUN Chequeo_OtrasCtas.*/
  
  IF W_SiHayError THEN
     RETURN "ADM-ERROR".
  

  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").

  IF RETURN-VALUE = "YES" THEN DO:
     FIND cfg_activosFijos WHERE cfg_activosFijos.Grupo EQ W_Pcodigo
                     NO-LOCK NO-ERROR.
     IF AVAILABLE(cfg_activosFijos) THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 365, OUTPUT W_Error).
        APPLY "ENTRY" TO W_CmbCod IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
     W_Adicion = FALSE.
  END.

 /* IF W_NoHayCtas = FALSE THEN DO:
     IF W_CtaValorizacion NE W_CtaValoAux AND W_CtaValoAux NE "" THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 369, OUTPUT W_Eleccion).
        MESSAGE "La Cuenta del Grupo es: " W_CtaValoAux VIEW-AS ALERT-BOX WARNING
                TITLE "Cuenta de Valorización".
        APPLY "ENTRY" TO W_CtaValorizacion IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        RUN Buscar_CtaGrabar (INPUT INPUT W_CtaValorizacion, INPUT FALSE, OUTPUT W_SiHayError).
        IF W_SiHayError THEN DO:
           APPLY "ENTRY" TO W_CtaValorizacion.
           RETURN "ADM-ERROR".
        END.
     END.
     IF W_CtaProvAux EQ "" THEN DO: /*NE*/
        RUN MostrarMensaje IN W_Manija (INPUT 370, OUTPUT W_Eleccion).
        MESSAGE "La Cuenta del Grupo es: " W_CtaProvAux VIEW-AS ALERT-BOX WARNING
                TITLE "Cuenta de Provision".
        APPLY "ENTRY" TO cfg_activosFijos.CtaProvision IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR".
     END.                                               
     ELSE DO:
        RUN Buscar_CtaGrabar (INPUT INPUT cfg_activosFijos.CtaProvision, INPUT FALSE, OUTPUT W_SiHayError).
        IF W_SiHayError THEN DO:
          APPLY "ENTRY" TO cfg_activosFijos.CtaProvision.
          RETURN "ADM-ERROR".
        END.
     END.
  END.*/
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  IF W_Adicion THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Activos. Codigo: " + STRING(W_PCodigo)).
  ELSE
     RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Activos. Codigo: " + STRING(W_PCodigo)).
  ASSIGN cfg_activosFijos.Grupo           = W_Pcodigo
         cfg_activosFijos.Id_Deprec       = Tg_IdDeprec
         cfg_activosFijos.CtaValorizacion = W_CtaValorizacion.
  W_Adicion = NO.
  cfg_activosFijos.Comprobante = INTEGER(SUBSTRING(Cmb_Comprobantes:SCREEN-VALUE,1,2)).
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
  DEFINE VAR W_NomCta AS CHARACTER FORMAT "X(35)".
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  IF AVAILABLE(cfg_activosFijos) THEN
     DO WITH FRAME {&FRAME-NAME}:
        RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
        IF RETURN-VALUE = "NO" THEN DO:
           ASSIGN Tg_IdDeprec       = cfg_activosFijos.Id_Deprec
                  W_Pcodigo         = cfg_activosFijos.Grupo
                  W_CtaValorizacion = cfg_activosFijos.CtaValorizacion
                  W_CtaProvAux      = cfg_activosFijos.CtaProvision
                  W_CtaValoAux      = W_CtaValorizacion.         
           DISPLAY W_CtaValorizacion.    
        END.
        FIND Varios WHERE Varios.Tipo   EQ 7
                      AND Varios.Codigo EQ cfg_activosFijos.Grupo
                      AND Varios.Estado EQ 1
                    NO-LOCK NO-ERROR.
       IF AVAILABLE(Varios) THEN
          W_CmbCod:SCREEN-VALUE IN FRAME F-Main = STRING(Varios.Codigo,"99999") + " "  + TRIM(Varios.Descripcion).
                                
       RUN Verif_Togle.    
       
       RUN NombreCuenta(INPUT cfg_activosFijos.CtaDepAcum, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta2:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.CtaDepMes, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta3:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.CtaOrdFDepDB, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta4:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.CtaOrdFDepCR, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta5:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.CtaDBPig, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta6:SCREEN-VALUE = W_NomCta.
        
       RUN NombreCuenta(INPUT cfg_activosFijos.CtaCRPig, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta7:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.CtaDBPre, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta8:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.CtaCRPre, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta9:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.CtaGanancia, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta11:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT W_CtaValorizacion, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta12:SCREEN-VALUE = W_NomCta.

       RUN NombreCuenta(INPUT cfg_activosFijos.CtaPerdida, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN Cta13:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.RteFte, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN NomCtaRet:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.IvaRetenido, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN NomIvaRet:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.IvaTransitorioRC, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN NomRC:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.IvaTransitorioRS, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN NomRS:SCREEN-VALUE = W_NomCta.
       
       RUN NombreCuenta(INPUT cfg_activosFijos.CtaSucAge, OUTPUT W_NomCta).
       IF W_NomCta NE "" THEN NomCtaSucAge:SCREEN-VALUE = W_NomCta.
       
       IF cfg_activosFijos.IVA NE "" THEN DO:
          FIND Base_Ret WHERE Base_Ret.Cod_Base EQ cfg_activosFijos.IVA NO-LOCK NO-ERROR.
          IF AVAILABLE Base_Ret THEN NomCodIVA:SCREEN-VALUE = Base_Ret.Nombre.
       END.
       FIND Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia AND
            Comprobantes.Comprobante EQ cfg_activosFijos.Comprobante NO-LOCK NO-ERROR.
       IF AVAILABLE Comprobantes THEN
          Cmb_Comprobantes:SCREEN-VALUE = STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre.
       ELSE
          Cmb_Comprobantes:SCREEN-VALUE = Cmb_Comprobantes:ENTRY(1).
       
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
  FOR EACH Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia AND Comprobantes.Estado EQ 1 NO-LOCK:
    W_Ok = Cmb_Comprobantes:ADD-LAST(STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre) IN FRAME {&FRAME-NAME}. 
  END.
  FOR EACH Varios WHERE Varios.Tipo   EQ 7 AND Varios.Estado EQ 1:
    W_CmbCod:ADD-LAST(STRING(Varios.Codigo,"99999") + " " +  TRIM(Varios.Descripcion)) IN FRAME {&FRAME-NAME}. 
  END.  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
 
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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NombreCuenta V-table-Win 
PROCEDURE NombreCuenta :
DEFINE INPUT PARAMETER WCta LIKE Cuentas.Cuenta.
DEFINE OUTPUT PARAMETER WNom AS CHARACTER FORMAT "X(35)".
FIND Cuentas WHERE Cuentas.Cuenta EQ WCta NO-LOCK NO-ERROR.
IF AVAILABLE Cuentas THEN WNom = Cuentas.Nombre.
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
  {src/adm/template/sndkycas.i "Grupo" "cfg_activosFijos" "Grupo"}

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
  {src/adm/template/snd-list.i "cfg_activosFijos"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verif_Togle V-table-Win 
PROCEDURE Verif_Togle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
   IF Tg_IdDeprec EQ NO THEN
      ASSIGN cfg_activosFijos.CtaDepAcum:SENSITIVE       = FALSE
             cfg_activosFijos.CtaDepMes:SENSITIVE        = FALSE
             cfg_activosFijos.CtaOrdFDepDB:SENSITIVE     = FALSE
             cfg_activosFijos.CtaOrdFDepCR:SENSITIVE     = FALSE
             Tg_IdDeprec:CHECKED                   = FALSE
             cfg_activosFijos.CtaDepAcum:SCREEN-VALUE    = ""
             cfg_activosFijos.CtaDepMes:SCREEN-VALUE     = "".
   ELSE
      ASSIGN cfg_activosFijos.CtaDepMes:SENSITIVE        = TRUE
             cfg_activosFijos.CtaDepAcum:SENSITIVE       = TRUE
             cfg_activosFijos.CtaOrdFDepDB:SENSITIVE     = TRUE
             cfg_activosFijos.CtaOrdFDepCR:SENSITIVE     = TRUE 
             Tg_IdDeprec:CHECKED                   = TRUE.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

