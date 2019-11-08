&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"d-pro_inversiones.i"}.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
  DEFINE VARIABLE W_ConsCta    LIKE Cuentas.Cuenta.
  DEFINE VARIABLE W_DispCta    LIKE Cuentas.Cuenta.
  DEFINE VARIABLE W_Pcuenta    LIKE Cuenta.Cuenta.
  DEFINE VARIABLE W_Pnombre    LIKE Cuenta.Nombre.
  DEFINE VARIABLE W_nomcta    LIKE Cuenta.Nombre.
  DEFINE VARIABLE W_Naturaleza LIKE Cuenta.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuenta.Ctr_Naturaleza.
  
  DEFINE VAR W_Rowid AS ROWID.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "d-pro_inversiones.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-VInversiones

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Id_TasaUnid RowObject.Categoria ~
RowObject.Cod_Producto RowObject.Nom_Producto RowObject.Tiempo_Valoracion ~
RowObject.Cuenta_Inversion_DB RowObject.CtaRendimiento_DB ~
RowObject.CtaValoracion_DB RowObject.CtaRendimiento_CR ~
RowObject.CtaValoracion_CR RowObject.Cpte_Egr RowObject.Cpte_Ingr ~
RowObject.Cpte_Tras RowObject.CtaProvision_Cr RowObject.CtaProvision_DB ~
RowObject.CtaRetFte_DB RowObject.Porc_RetFte 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS Btn_Cbe Btn_Cbi Btn_Cbt Indicador ctainv ~
ctarenc ctarend ctavalc ctavald RECT-11 RECT-12 RECT-13 RECT-14 RECT-15 
&Scoped-Define DISPLAYED-FIELDS RowObject.Id_TasaUnid RowObject.Indice ~
RowObject.Categoria RowObject.Cod_Producto RowObject.Nom_Producto ~
RowObject.Tiempo_Valoracion RowObject.Cuenta_Inversion_DB ~
RowObject.CtaRendimiento_DB RowObject.CtaValoracion_DB ~
RowObject.CtaRendimiento_CR RowObject.CtaValoracion_CR RowObject.Cpte_Egr ~
RowObject.Cpte_Ingr RowObject.Cpte_Tras RowObject.CtaProvision_Cr ~
RowObject.CtaProvision_DB RowObject.CtaRetFte_DB RowObject.Porc_RetFte 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS Cbt_Egresos Cbt_Ingresos Cbt_Traslados ~
NomCr_Provision NomDB_Provision NomDB_RetFte Indicador ctainv ctarenc ~
ctarend ctavalc ctavald 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cbe 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 7" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Cbi 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 6" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Cbt 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 8" 
     SIZE 3 BY .54.

DEFINE VARIABLE Indicador AS CHARACTER FORMAT "X(256)":U 
     LABEL "Indice Económico" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 Ninguno" 
     DROP-DOWN-LIST
     SIZE 25 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cbt_Egresos AS CHARACTER FORMAT "X(35)":U 
      VIEW-AS TEXT 
     SIZE 19 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cbt_Ingresos AS CHARACTER FORMAT "X(35)":U 
      VIEW-AS TEXT 
     SIZE 19 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cbt_Traslados AS CHARACTER FORMAT "X(35)":U 
      VIEW-AS TEXT 
     SIZE 19 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ctainv AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE ctarenc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE ctarend AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE ctavalc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE ctavald AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE NomCr_Provision AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomDB_Provision AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomDB_RetFte AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44 BY 1.62.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 3.23.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 3.23.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 2.96.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-VInversiones
     Btn_Cbe AT ROW 10.96 COL 41
     Btn_Cbi AT ROW 9.88 COL 41
     Btn_Cbt AT ROW 12.04 COL 41
     Cbt_Egresos AT ROW 10.96 COL 20 COLON-ALIGNED NO-LABEL
     Cbt_Ingresos AT ROW 9.88 COL 20 COLON-ALIGNED NO-LABEL
     Cbt_Traslados AT ROW 12.04 COL 20 COLON-ALIGNED NO-LABEL
     RowObject.Id_TasaUnid AT ROW 14.19 COL 7 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Tasa", 0,
"Unidades", 1
          SIZE 33 BY .81
          FONT 5
     NomCr_Provision AT ROW 17.96 COL 66 COLON-ALIGNED NO-LABEL
     NomDB_Provision AT ROW 16.88 COL 66 COLON-ALIGNED NO-LABEL
     NomDB_RetFte AT ROW 19.58 COL 66 COLON-ALIGNED NO-LABEL
     RowObject.Indice AT ROW 2.69 COL 92 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 3 BY .62
          FGCOLOR 17 
     RowObject.Categoria AT ROW 2.62 COL 4 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Negociables", 1,
"Permanentes", 2,
"Disponibles", 3
          SIZE 42 BY .81
          FONT 5
     RowObject.Cod_Producto AT ROW 2.08 COL 53 COLON-ALIGNED
          LABEL "Codigo"
          VIEW-AS FILL-IN 
          SIZE 37 BY .81
          BGCOLOR 15 
     RowObject.Nom_Producto AT ROW 3.15 COL 53 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37 BY .81
          BGCOLOR 15 
     RowObject.Tiempo_Valoracion AT ROW 7.73 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25 BY .81
          BGCOLOR 15 
     Indicador AT ROW 8.81 COL 17 COLON-ALIGNED
     RowObject.Cuenta_Inversion_DB AT ROW 6.65 COL 54 COLON-ALIGNED
          LABEL "Cta.Inversion"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     RowObject.CtaRendimiento_DB AT ROW 9.35 COL 54 COLON-ALIGNED
          LABEL "Cta.Debito"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     RowObject.CtaValoracion_DB AT ROW 13.12 COL 54 COLON-ALIGNED
          LABEL "Cta.Débito"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     RowObject.CtaRendimiento_CR AT ROW 10.42 COL 54 COLON-ALIGNED
          LABEL "Cta.Crédito"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     RowObject.CtaValoracion_CR AT ROW 14.19 COL 54 COLON-ALIGNED
          LABEL "Cta.Crédito"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     ctainv AT ROW 6.65 COL 66 COLON-ALIGNED NO-LABEL
     ctarenc AT ROW 10.42 COL 66 COLON-ALIGNED NO-LABEL
     ctarend AT ROW 9.35 COL 66 COLON-ALIGNED NO-LABEL
     ctavalc AT ROW 14.19 COL 66 COLON-ALIGNED NO-LABEL
     ctavald AT ROW 13.12 COL 66 COLON-ALIGNED NO-LABEL
     RowObject.Cpte_Egr AT ROW 10.96 COL 3.28
          LABEL "Comprobante Egresos"
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 15 
     RowObject.Cpte_Ingr AT ROW 9.88 COL 9.71
          LABEL "Cbt.Ingresos"
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 15 
     RowObject.Cpte_Tras AT ROW 12.04 COL 2.14
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 15 
     RowObject.CtaProvision_Cr AT ROW 17.96 COL 54 COLON-ALIGNED
          LABEL "Cta.Crédito"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FGCOLOR 0 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-VInversiones
     RowObject.CtaProvision_DB AT ROW 16.88 COL 54 COLON-ALIGNED
          LABEL "Cta.Débito"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     RowObject.CtaRetFte_DB AT ROW 19.58 COL 54 COLON-ALIGNED
          LABEL "Cta. Débito Ret.Fuente"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     RowObject.Porc_RetFte AT ROW 6.65 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25 BY .81
          BGCOLOR 15 
     RECT-11 AT ROW 2.08 COL 3
     RECT-12 AT ROW 8.54 COL 46
     RECT-13 AT ROW 12.31 COL 46
     RECT-14 AT ROW 16.35 COL 46
     RECT-15 AT ROW 13.65 COL 3
     "Categoria Inversión:" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 1.81 COL 5
          FGCOLOR 7 FONT 5
     "Cuentas Rendimiento" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 8.27 COL 47
          FGCOLOR 7 FONT 5
     "Cuentas Valoración" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 12.04 COL 47
          FGCOLOR 7 FONT 5
     "Cuentas Provisión" VIEW-AS TEXT
          SIZE 17 BY .81 AT ROW 16.08 COL 47
          FGCOLOR 7 FONT 5
     "Configuración Contable" VIEW-AS TEXT
          SIZE 22 BY .81 AT ROW 5.31 COL 46
          FGCOLOR 7 FONT 5
     "Forma en la que se representa la Inversión" VIEW-AS TEXT
          SIZE 38 BY 1.08 AT ROW 13.12 COL 4
          FGCOLOR 7 FONT 5
     "Otras Configuraciones" VIEW-AS TEXT
          SIZE 28 BY .81 AT ROW 5.31 COL 5
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FGCOLOR 0 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "d-pro_inversiones.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {d-pro_inversiones.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 19.81
         WIDTH              = 96.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-VInversiones
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-VInversiones:SCROLLABLE       = FALSE
       FRAME F-VInversiones:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Cbt_Egresos IN FRAME F-VInversiones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cbt_Ingresos IN FRAME F-VInversiones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cbt_Traslados IN FRAME F-VInversiones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Cod_Producto IN FRAME F-VInversiones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Cpte_Egr IN FRAME F-VInversiones
   ALIGN-L EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN RowObject.Cpte_Ingr IN FRAME F-VInversiones
   ALIGN-L EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN RowObject.Cpte_Tras IN FRAME F-VInversiones
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.CtaProvision_Cr IN FRAME F-VInversiones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.CtaProvision_DB IN FRAME F-VInversiones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.CtaRendimiento_CR IN FRAME F-VInversiones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.CtaRendimiento_DB IN FRAME F-VInversiones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.CtaRetFte_DB IN FRAME F-VInversiones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.CtaValoracion_CR IN FRAME F-VInversiones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.CtaValoracion_DB IN FRAME F-VInversiones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Cuenta_Inversion_DB IN FRAME F-VInversiones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Indice IN FRAME F-VInversiones
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.Indice:HIDDEN IN FRAME F-VInversiones           = TRUE.

/* SETTINGS FOR FILL-IN NomCr_Provision IN FRAME F-VInversiones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomDB_Provision IN FRAME F-VInversiones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomDB_RetFte IN FRAME F-VInversiones
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-VInversiones
/* Query rebuild information for FRAME F-VInversiones
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-VInversiones */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Cbe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cbe vTableWin
ON CHOOSE OF Btn_Cbe IN FRAME F-VInversiones /* Button 7 */
DO:
  RUN C-Comprobantes.r(INPUT-OUTPUT W_Rowid).
  FIND FIRST Comprobantes WHERE ROWID(Comprobantes) EQ W_Rowid NO-LOCK NO-ERROR.
  IF AVAILABLE Comprobantes THEN DO:
     ASSIGN RowObject.Cpte_Egr:SCREEN-VALUE = STRING(Comprobantes.Comprobante)
            Cbt_Egresos:SCREEN-VALUE = Comprobantes.Nombre.
     APPLY "Value-Changed" TO Cpte_Egr.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cbi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cbi vTableWin
ON CHOOSE OF Btn_Cbi IN FRAME F-VInversiones /* Button 6 */
DO:
  RUN C-Comprobantes.r(INPUT-OUTPUT W_Rowid).
  FIND FIRST Comprobantes WHERE ROWID(Comprobantes) EQ W_Rowid NO-LOCK NO-ERROR.
  IF AVAILABLE Comprobantes THEN DO:
     ASSIGN RowObject.Cpte_Ingr:SCREEN-VALUE = STRING(Comprobantes.Comprobante)
            Cbt_Ingresos:SCREEN-VALUE = STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre.
     APPLY "value-Changed" TO RowObject.Cpte_Ingr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cbt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cbt vTableWin
ON CHOOSE OF Btn_Cbt IN FRAME F-VInversiones /* Button 8 */
DO:
  RUN C-Comprobantes.r(INPUT-OUTPUT W_Rowid).
  FIND FIRST Comprobantes WHERE ROWID(Comprobantes) EQ W_Rowid NO-LOCK NO-ERROR.
  IF AVAILABLE Comprobantes THEN DO:
     ASSIGN RowObject.Cpte_Tras:SCREEN-VALUE = STRING(Comprobantes.Comprobante)
            Cbt_Traslados:SCREEN-VALUE = Comprobantes.Nombre.
     APPLY "value-changed" TO Cpte_Tras.    
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Cod_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Cod_Producto vTableWin
ON LEAVE OF RowObject.Cod_Producto IN FRAME F-VInversiones /* Codigo */
DO:
  IF INTEGER(SELF:SCREEN-VALUE) > 0 THEN DO:
    FIND Pro_Inversiones WHERE Pro_Inversiones.Cod_Producto = INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Inversiones THEN DO:
       MESSAGE 'Inversión Ya Existe...' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN NO-APPLY.
      END.
    END.
   ELSE DO:
     MESSAGE 'Código en Blanco...' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Cpte_Egr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Cpte_Egr vTableWin
ON LEAVE OF RowObject.Cpte_Egr IN FRAME F-VInversiones /* Comprobante Egresos */
DO:
  FIND Comprobantes WHERE Comprobantes.Comprobante EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Comprobantes THEN
     Cbt_Egresos:SCREEN-VALUE = Comprobantes.Nombre.
  ELSE APPLY "choose" TO Btn_Cbe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Cpte_Ingr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Cpte_Ingr vTableWin
ON LEAVE OF RowObject.Cpte_Ingr IN FRAME F-VInversiones /* Cbt.Ingresos */
DO:
  FIND Comprobantes WHERE Comprobantes.Comprobante EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Comprobantes THEN
     Cbt_Ingresos:SCREEN-VALUE = Comprobantes.Nombre.
  ELSE APPLY "choose" TO Btn_CbI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Cpte_Tras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Cpte_Tras vTableWin
ON LEAVE OF RowObject.Cpte_Tras IN FRAME F-VInversiones /* Comprobante Traslados */
DO:
  FIND Comprobantes WHERE Comprobantes.Comprobante EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Comprobantes THEN
     Cbt_Traslados:SCREEN-VALUE = Comprobantes.Nombre.
  ELSE APPLY "choose" TO Btn_CbT.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.CtaProvision_Cr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaProvision_Cr vTableWin
ON LEAVE OF RowObject.CtaProvision_Cr IN FRAME F-VInversiones /* Cta.Crédito */
DO:
  ASSIGN NomCR_Provision:SCREEN-VALUE = "".

  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.

  IF SELF:SCREEN-VALUE = ' ':U OR NOT avail(Cuentas) THEN DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).      
  END.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF AVAIL(Cuentas) THEN
     ASSIGN NomCR_Provision:SCREEN-VALUE = Cuentas.Nombre.
  ELSE ASSIGN NomCR_Provision:SCREEN-VALUE = " "
              SELF:SCREEN-VALUE    = "".  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaProvision_Cr vTableWin
ON LEFT-MOUSE-DBLCLICK OF RowObject.CtaProvision_Cr IN FRAME F-VInversiones /* Cta.Crédito */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.
    RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.CtaProvision_DB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaProvision_DB vTableWin
ON LEAVE OF RowObject.CtaProvision_DB IN FRAME F-VInversiones /* Cta.Débito */
DO:
  ASSIGN NomDb_Provision:SCREEN-VALUE = "".

  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.

  IF SELF:SCREEN-VALUE = ' ':U OR NOT avail(Cuentas) THEN DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).      
  END.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF AVAIL(Cuentas) THEN
     ASSIGN NomDb_Provision:SCREEN-VALUE = Cuentas.Nombre.
  ELSE ASSIGN NomDb_Provision:SCREEN-VALUE = " "
              SELF:SCREEN-VALUE    = "".  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaProvision_DB vTableWin
ON LEFT-MOUSE-DBLCLICK OF RowObject.CtaProvision_DB IN FRAME F-VInversiones /* Cta.Débito */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ctarend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ctarend vTableWin
ON LEAVE OF ctarend IN FRAME F-VInversiones
DO:
    IF INTEGER(SELF:SCREEN-VALUE) > 0 THEN DO:
     FIND Cuentas WHERE Cuentas.Cuenta = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Cuentas THEN DO:
        MESSAGE 'Cuenta no Existe en el Maestro...' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
       END.
      ELSE DO:
        ctarend = TRIM(Cuentas.Nombre) + ' ' + TRIM(Cuentas.Naturaleza).
        DISPLAY ctarend WITH FRAME F-VInversiones.
      END.
    END.
   ELSE DO:
     MESSAGE 'Cuenta vacia...' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.CtaRendimiento_CR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaRendimiento_CR vTableWin
ON LEAVE OF RowObject.CtaRendimiento_CR IN FRAME F-VInversiones /* Cta.Crédito */
DO:
  ASSIGN ctarenc:SCREEN-VALUE = "".

  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.

  IF SELF:SCREEN-VALUE = ' ':U OR NOT avail(Cuentas) THEN DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).      
  END.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF AVAIL(Cuentas) THEN
     ASSIGN ctarenc:SCREEN-VALUE = Cuentas.Nombre.
  ELSE ASSIGN ctarenc:SCREEN-VALUE = " "
              SELF:SCREEN-VALUE   = "".         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaRendimiento_CR vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.CtaRendimiento_CR IN FRAME F-VInversiones /* Cta.Crédito */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.CtaRendimiento_DB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaRendimiento_DB vTableWin
ON LEAVE OF RowObject.CtaRendimiento_DB IN FRAME F-VInversiones /* Cta.Debito */
DO:
  ASSIGN ctarend:SCREEN-VALUE = "".

  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.

  IF SELF:SCREEN-VALUE = ' ':U OR NOT avail(Cuentas) THEN DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).      
  END.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF AVAIL(Cuentas) THEN
     ASSIGN ctarend:SCREEN-VALUE = Cuentas.Nombre.
  ELSE ASSIGN ctarend:SCREEN-VALUE = " "
              SELF:SCREEN-VALUE   = "".         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaRendimiento_DB vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.CtaRendimiento_DB IN FRAME F-VInversiones /* Cta.Debito */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.CtaRetFte_DB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaRetFte_DB vTableWin
ON LEAVE OF RowObject.CtaRetFte_DB IN FRAME F-VInversiones /* Cta. Débito Ret.Fuente */
DO:
  ASSIGN NomDb_RetFte:SCREEN-VALUE = "".

  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.

  IF SELF:SCREEN-VALUE = ' ':U OR NOT avail(Cuentas) THEN DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).      
  END.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF AVAIL(Cuentas) THEN
     ASSIGN NomDb_RetFte:SCREEN-VALUE = Cuentas.Nombre.
  ELSE ASSIGN NomDb_RetFte:SCREEN-VALUE = " "
              SELF:SCREEN-VALUE    = "".    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaRetFte_DB vTableWin
ON LEFT-MOUSE-DBLCLICK OF RowObject.CtaRetFte_DB IN FRAME F-VInversiones /* Cta. Débito Ret.Fuente */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.
    RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ctavalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ctavalc vTableWin
ON LEAVE OF ctavalc IN FRAME F-VInversiones
DO:
    IF INTEGER(SELF:SCREEN-VALUE) > 0 THEN DO:
     FIND Cuentas WHERE Cuentas.Cuenta = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Cuentas THEN DO:
        MESSAGE 'Cuenta no Existe en el Maestro...' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
       END.
      ELSE DO:
        ctarenc = TRIM(Cuentas.Nombre) + ' ' + TRIM(Cuentas.Naturaleza).
        DISPLAY ctarenc WITH FRAME F-VInversiones.
      END.
    END.
   ELSE DO:
     MESSAGE 'Cuenta vacia...' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.CtaValoracion_CR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaValoracion_CR vTableWin
ON LEAVE OF RowObject.CtaValoracion_CR IN FRAME F-VInversiones /* Cta.Crédito */
DO:
  ASSIGN ctavalc:SCREEN-VALUE = "".

  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.

  IF SELF:SCREEN-VALUE = ' ':U OR NOT avail(Cuentas) THEN DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).      
  END.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF AVAIL(Cuentas) THEN
     ASSIGN ctavalc:SCREEN-VALUE = Cuentas.Nombre.
  ELSE ASSIGN ctavalc:SCREEN-VALUE = " "
              SELF:SCREEN-VALUE    = "".            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaValoracion_CR vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.CtaValoracion_CR IN FRAME F-VInversiones /* Cta.Crédito */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.CtaValoracion_DB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaValoracion_DB vTableWin
ON LEAVE OF RowObject.CtaValoracion_DB IN FRAME F-VInversiones /* Cta.Débito */
DO:
  ASSIGN ctavald:SCREEN-VALUE = "".

  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.

  IF SELF:SCREEN-VALUE = ' ':U OR NOT avail(Cuentas) THEN DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).      
  END.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF AVAIL(Cuentas) THEN
     ASSIGN ctavald:SCREEN-VALUE = Cuentas.Nombre.
  ELSE ASSIGN ctavald:SCREEN-VALUE = " "
              SELF:SCREEN-VALUE   = "".            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaValoracion_DB vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.CtaValoracion_DB IN FRAME F-VInversiones /* Cta.Débito */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Cuenta_Inversion_DB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Cuenta_Inversion_DB vTableWin
ON LEAVE OF RowObject.Cuenta_Inversion_DB IN FRAME F-VInversiones /* Cta.Inversion */
DO:
  ctainv:SCREEN-VALUE = " ".

  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.

  IF SELF:SCREEN-VALUE = ' ':U OR NOT avail(Cuentas) THEN DO:
    W_ConsCta = SELF:SCREEN-VALUE. 
    RUN ConCtaCble(INPUT W_ConsCta).      
  END.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                 AND Cuentas.Tipo   EQ 2 
                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF AVAIL(Cuentas) THEN
     ASSIGN ctainv:SCREEN-VALUE = Cuentas.Nombre.
  ELSE ASSIGN ctainv:SCREEN-VALUE = " "
              SELF:SCREEN-VALUE   = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Cuenta_Inversion_DB vTableWin
ON LEFT-MOUSE-DBLCLICK OF RowObject.Cuenta_Inversion_DB IN FRAME F-VInversiones /* Cta.Inversion */
DO:
    W_ConsCta = SELF:SCREEN-VALUE.
    RUN MoDbClkCta(INPUT W_ConsCta, OUTPUT W_DispCta).
    SELF:SCREEN-VALUE = W_DispCta.
    RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Indicador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Indicador vTableWin
ON VALUE-CHANGED OF Indicador IN FRAME F-VInversiones /* Indice Económico */
DO:
  ASSIGN RowObject.Indice:SCREEN-VALUE = SUBSTRING(SELF:SCREEN-VALUE,1,5). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */
 &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConCtaCble vTableWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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
  HIDE FRAME F-VInversiones.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields vTableWin 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.
 DEFINE VARIABLE Vt AS CHARACTER.        
  RUN SUPER( INPUT pcColValues).
   /* Code placed here will execute PRIOR to standard behavior. */

   /************* Indice ************/
    ASSIGN Vt = RowObject.Indice:SCREEN-VALUE IN FRAME {&FRAME-NAME}. 
    IF INTEGER(Vt) > 0 THEN DO:
     FIND FIRST Indicadores WHERE Indicadores.Indicador = INTEGER(Vt) NO-LOCK NO-ERROR.
     IF AVAILABLE Indicadores THEN
       Indicador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Vt,'99999') + ' ' + TRIM(Indicadores.Nombre).   
      ELSE 
       IF TRIM(Vt) = '' THEN
         Indicador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '00000 Ninguno'.    
        ELSE DO:
          Indicador:ADD-LAST(TRIM(Vt) + ' No Existe.') IN FRAME {&FRAME-NAME}.  
          Indicador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = TRIM(Vt) + ' No Existe.'.    
        END.
  
   /*********************************/   
   /******** Cuenta Inversion ********/
    
    ASSIGN Vt = RowObject.Cuenta_Inversion_DB:SCREEN-VALUE IN FRAME {&FRAME-NAME}.     
    FIND FIRST Cuentas WHERE Cuentas.Cuenta = TRIM(Vt) NO-LOCK NO-ERROR.
    IF AVAILABLE Cuentas THEN
      ctainv = Cuentas.Nombre.
     ELSE
      ctainv = 'Cuenta No Existe..'.
    DISPLAY ctainv WITH FRAME {&FRAME-NAME}.
   /*********************************/
   /******** Cuenta Rendimiento Debito ********/
    ASSIGN Vt = RowObject.CtaRendimiento_DB:SCREEN-VALUE IN FRAME {&FRAME-NAME}.         
    FIND FIRST Cuentas WHERE Cuentas.Cuenta = TRIM(Vt) NO-LOCK NO-ERROR.
    IF AVAILABLE Cuentas THEN
      ctarend = Cuentas.Nombre.
     ELSE
      ctarend = 'Cuenta No Existe..'.
    DISPLAY ctarend WITH FRAME {&FRAME-NAME}.
   /*******************************************/
   /******** Cuenta Rendimiento Credito ********/
    ASSIGN Vt = RowObject.CtaRendimiento_CR:SCREEN-VALUE IN FRAME {&FRAME-NAME}.             
    FIND FIRST Cuentas WHERE Cuentas.Cuenta = TRIM(Vt) NO-LOCK NO-ERROR.
    IF AVAILABLE Cuentas THEN
      ctarenc = Cuentas.Nombre.
     ELSE
      ctarenc = 'Cuenta No Existe..'.
    DISPLAY ctarenc WITH FRAME {&FRAME-NAME}.
   /******** Cuenta de Valorizacion Debito ********/
    ASSIGN Vt = RowObject.CtaValoracion_DB:SCREEN-VALUE IN FRAME {&FRAME-NAME}.             
    FIND FIRST Cuentas WHERE Cuentas.Cuenta = TRIM(Vt) NO-LOCK NO-ERROR.
    IF AVAILABLE Cuentas THEN
      ctavald = Cuentas.Nombre.
     ELSE
      ctavald = 'Cuenta No Existe..'.
    DISPLAY ctavald WITH FRAME {&FRAME-NAME}.
   /***********************************************/
   /******** Cuenta de Valorizacion Credito ********/
    ASSIGN Vt = RowObject.CtaValoracion_CR:SCREEN-VALUE IN FRAME {&FRAME-NAME}.             
    FIND FIRST Cuentas WHERE Cuentas.Cuenta = TRIM(Vt) NO-LOCK NO-ERROR.
    IF AVAILABLE Cuentas THEN
      ctavalc = Cuentas.Nombre.
     ELSE
      ctavalc = 'Cuenta No Existe..'.
    DISPLAY ctavalc WITH FRAME {&FRAME-NAME}.
   /***********************************************/
 /* Code placed here will execute AFTER standard behavior.    */
 
    FIND Comprobantes WHERE Comprobantes.Comprobante EQ RowObject.Cpte_Ingr NO-LOCK NO-ERROR.
    IF AVAILABLE Comprobantes THEN
     ASSIGN RowObject.Cpte_Ingr:SCREEN-VALUE = STRING(Comprobantes.Comprobante)
            Cbt_Ingresos:SCREEN-VALUE = STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre.
    
    FIND Comprobantes WHERE Comprobantes.Comprobante EQ RowObject.Cpte_Egr NO-LOCK NO-ERROR.
    IF AVAILABLE Comprobantes THEN
     ASSIGN RowObject.Cpte_Egr:SCREEN-VALUE = STRING(Comprobantes.Comprobante)
            Cbt_Egresos:SCREEN-VALUE = STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre.

    FIND Comprobantes WHERE Comprobantes.Comprobante EQ RowObject.Cpte_Tras NO-LOCK NO-ERROR.
    IF AVAILABLE Comprobantes THEN
     ASSIGN RowObject.Cpte_Tras:SCREEN-VALUE = STRING(Comprobantes.Comprobante)
            Cbt_Traslados:SCREEN-VALUE = STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre.

    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
    FOR EACH Indicadores BY Indicador:
      Indicador:ADD-LAST(TRIM(STRING(Indicadores.Indicador,'99999')) + ' ' + TRIM(Indicadores.Nombre)) IN FRAME F-VInversiones.
    END.
    RUN SUPER. 
/*    Indicador:ADD-LAST('00000 Ninguno') IN FRAME F-VInversiones.*/
    

    ENABLE ALL WITH FRAME {&FRAME-NAME}.
   
 /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ModbClkCta vTableWin 
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
       RUN C-Cuentas.R (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, OUTPUT W_CtrNat, INPUT "M").
           IF W_PCuenta EQ ? THEN DO:
              FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta
                             AND Cuentas.Tipo   EQ 2 
                             AND Cuentas.Estado EQ 1
                           NO-LOCK NO-ERROR NO-WAIT.
              IF NOT AVAILABLE(Cuentas) THEN
                 ASSIGN T_ConsCta1           = ""
                        W_NomCta             = " ".
           END.
           ELSE
              ASSIGN T_ConsCta1                = LOWER(W_Pcuenta)
                     W_NomCta                  = LOWER(W_Pnombre).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  APPLY 'VALUE-CHANGED' TO Indicador IN FRAME F-VInversiones.
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

