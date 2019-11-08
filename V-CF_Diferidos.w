&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"d-cf_diferidos.i"}.


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

/* Local Variable Definitions ---                    */   
   DEFINE VAR W_Pcodigo    LIKE Varios.Codigo.
   DEFINE VAR W_Pcuenta    LIKE Cuenta.Cuenta.
   DEFINE VAR W_Pnombre    LIKE Cuenta.Nombre.
   DEFINE VAR W_Pnomcta    LIKE Cuenta.Nombre.
   DEFINE VAR W_Naturaleza LIKE Cuenta.Naturaleza.
   DEFINE VAR W_ctrnat     LIKE Cuentas.Ctr_Naturaleza.
   DEFINE VAR W_HayError   AS   LOGICAL.
   DEFINE VAR W_ok         AS   LOGICAL.  /* Items del combo-box */ 
   DEFINE VAR W_ctasal     LIKE Cuentas.Cuenta.
   DEFINE VAR W_posicion   AS INTEGER INITIAL 0.
   DEFINE VAR W_nroreg     AS INTEGER INITIAL 0.    
  {incluido\variable.i "SHARED"}

    DEFINE VARIABLE P_Codbase LIKE Base_Ret.Cod_Base.
    DEFINE VARIABLE P_Nombre  LIKE Base_Ret.Nombre.
    DEFINE VARIABLE P_Porcent LIKE Base_Ret.Porcentaje.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "d-cf_diferidos.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Dif_PlaMax RowObject.Dif_CostoMin ~
RowObject.Dif_CtaFuente RowObject.Dif_CtaGtoDif RowObject.Iva ~
RowObject.IvaRetenido RowObject.IvaTransitorioRC RowObject.IvatransitorioRs ~
RowObject.RteFte RowObject.CtaSucAge 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS Cmb_Comprobantes w_cmbcod cta_fte cta_Gtodif ~
RECT-174 
&Scoped-Define DISPLAYED-FIELDS RowObject.Dif_PlaMax RowObject.Dif_CostoMin ~
RowObject.Dif_CtaFuente RowObject.Dif_CtaGtoDif RowObject.Cod_Producto ~
RowObject.Iva RowObject.IvaRetenido RowObject.IvaTransitorioRC ~
RowObject.IvatransitorioRs RowObject.RteFte RowObject.Comprobante ~
RowObject.CtaSucAge 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS NomCtaSucAge Cmb_Comprobantes NomIva ~
NomIvaRet NOMRC NomRet NOMRS w_cmbcod cta_fte cta_Gtodif 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */
&Scoped-define ADM-ASSIGN-FIELDS RowObject.Comprobante 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE Cmb_Comprobantes AS CHARACTER FORMAT "X(256)":U INITIAL "00 - No Asignado" 
     LABEL "Comprobante" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00 - No Asignado" 
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_cmbcod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Grupo Diferidos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 33 BY 1
     BGCOLOR 15 FGCOLOR 0 FONT 5 NO-UNDO.

DEFINE VARIABLE w_cmbnew AS CHARACTER FORMAT "X(256)":U 
     LABEL "Grupo Diferidos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cta_fte AS CHARACTER FORMAT "X(60)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE cta_Gtodif AS CHARACTER FORMAT "X(60)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE NomCtaSucAge AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomIva AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomIvaRet AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NOMRC AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomRet AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NOMRS AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-174
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 10.23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     NomCtaSucAge AT ROW 16.88 COL 38 COLON-ALIGNED NO-LABEL
     Cmb_Comprobantes AT ROW 4.5 COL 17 COLON-ALIGNED
     NomIva AT ROW 15.27 COL 38 COLON-ALIGNED NO-LABEL
     NomIvaRet AT ROW 9.88 COL 38 COLON-ALIGNED NO-LABEL
     NOMRC AT ROW 11.23 COL 38 COLON-ALIGNED NO-LABEL
     NomRet AT ROW 13.92 COL 38 COLON-ALIGNED NO-LABEL
     NOMRS AT ROW 12.58 COL 38 COLON-ALIGNED NO-LABEL
     w_cmbcod AT ROW 1.27 COL 17 COLON-ALIGNED
     w_cmbnew AT ROW 1.27 COL 17 COLON-ALIGNED
     RowObject.Dif_PlaMax AT ROW 2.35 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY .81 TOOLTIP "Plazo Máximo (en Días)"
          BGCOLOR 15 FONT 4
     RowObject.Dif_CostoMin AT ROW 3.42 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.72 BY .81 TOOLTIP "Costo a diferir diariamente"
          BGCOLOR 15 FONT 4
     RowObject.Dif_CtaFuente AT ROW 7.19 COL 22 COLON-ALIGNED
          LABEL "Cuenta Fuente"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81 TOOLTIP "Cuenta contable donde se origina el costo"
          BGCOLOR 15 FONT 4
     cta_fte AT ROW 7.19 COL 38 COLON-ALIGNED NO-LABEL
     RowObject.Dif_CtaGtoDif AT ROW 8.54 COL 22 COLON-ALIGNED
          LABEL "Cta de Gasto Diferido"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 FONT 4
     RowObject.Cod_Producto AT ROW 1.27 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 17 FGCOLOR 17 
     cta_Gtodif AT ROW 8.54 COL 38 COLON-ALIGNED NO-LABEL
     RowObject.Iva AT ROW 15.27 COL 22 COLON-ALIGNED
          LABEL "Codigo Base de IVA"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     RowObject.IvaRetenido AT ROW 9.88 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     RowObject.IvaTransitorioRC AT ROW 11.23 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     RowObject.IvatransitorioRs AT ROW 12.58 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     RowObject.RteFte AT ROW 13.92 COL 22 COLON-ALIGNED
          LABEL "Cuenta Retención"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     RowObject.Comprobante AT ROW 4.5 COL 54 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          FGCOLOR 17 
     RowObject.CtaSucAge AT ROW 16.88 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     RECT-174 AT ROW 6.38 COL 3
     "Cuentas Diferidos" VIEW-AS TEXT
          SIZE 16 BY 1.08 AT ROW 5.85 COL 5
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "d-cf_diferidos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {d-cf_diferidos.i}
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
         HEIGHT             = 17.88
         WIDTH              = 81.43.
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
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Cod_Producto IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Cod_Producto:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Comprobante IN FRAME F-Main
   NO-ENABLE 1                                                          */
ASSIGN 
       RowObject.Comprobante:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Dif_CtaFuente IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Dif_CtaGtoDif IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Iva IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN NomCtaSucAge IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomIva IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomIvaRet IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NOMRC IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomRet IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NOMRS IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.RteFte IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       w_cmbcod:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR COMBO-BOX w_cmbnew IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       w_cmbnew:HIDDEN IN FRAME F-Main           = TRUE.

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

&Scoped-define SELF-NAME Cmb_Comprobantes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Comprobantes vTableWin
ON VALUE-CHANGED OF Cmb_Comprobantes IN FRAME F-Main /* Comprobante */
DO:
  APPLY "value-changed" TO RowObject.Dif_CtaFuente  IN FRAME {&FRAME-NAME}.
  RowObject.Comprobante:SCREEN-VALUE = SUBSTRING(Cmb_Comprobantes:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.CtaSucAge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CtaSucAge vTableWin
ON LEAVE OF RowObject.CtaSucAge IN FRAME F-Main /* Cta.Suc.Age */
DO:
  DO WITH FRAME {&FRAME-NAME}:
  FIND Cuentas WHERE Cuentas.Cuenta EQ RowObject.CtaSucAge:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN 
     NomCtaSucAge:SCREEN-VALUE = Cuentas.Nombre.
  ELSE DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     ASSIGN RowObject.CtaSucAge:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
            NomCtaSucAge:SCREEN-VALUE = W_Pnombre.
  END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Dif_CostoMin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Dif_CostoMin vTableWin
ON LEAVE OF RowObject.Dif_CostoMin IN FRAME F-Main /* Valor Mínimo */
DO:
  IF INTEGER(RowObject.Dif_costoMin:SCREEN-VALUE) LE 0 THEN DO:
     RUN MostrarMensaje IN w_manija (INPUT 10, OUTPUT w_eleccion).
     ON RETURN RETURN.
     RETURN.
/*     RETURN NO-APPLY. */
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Dif_CtaFuente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Dif_CtaFuente vTableWin
ON LEAVE OF RowObject.Dif_CtaFuente IN FRAME F-Main /* Cuenta Fuente */
DO:
 DO WITH FRAME {&FRAME-NAME}:
  FIND Cuentas WHERE Cuentas.Cuenta EQ RowObject.Dif_CtaFuente:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN 
     Cta_Fte:SCREEN-VALUE = cuentas.nombre.
  ELSE DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     ASSIGN RowObject.Dif_Ctafuente:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
            Cta_Fte:SCREEN-VALUE = W_Pnombre.
  END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Dif_CtaGtoDif
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Dif_CtaGtoDif vTableWin
ON LEAVE OF RowObject.Dif_CtaGtoDif IN FRAME F-Main /* Cta de Gasto Diferido */
DO:
 DO WITH FRAME {&FRAME-NAME}:
  FIND Cuentas WHERE Cuentas.Cuenta EQ RowObject.Dif_Ctagtodif:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN 
     Cta_Gtodif:SCREEN-VALUE = cuentas.nombre.
  ELSE DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     ASSIGN RowObject.Dif_Ctagtodif:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
            Cta_Gtodif:SCREEN-VALUE = W_Pnombre.
  END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Dif_PlaMax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Dif_PlaMax vTableWin
ON LEAVE OF RowObject.Dif_PlaMax IN FRAME F-Main /* Plazo Máximo */
DO:
  IF INTEGER(RowObject.Dif_PlaMax:SCREEN-VALUE) EQ 0 THEN DO:
     RUN MostrarMensaje IN w_manija (INPUT 10, OUTPUT w_eleccion).
     ON RETURN RETURN.
     RETURN.
/*      RETURN NO-APPLY. */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Iva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Iva vTableWin
ON LEAVE OF RowObject.Iva IN FRAME F-Main /* Codigo Base de IVA */
DO:
  FIND Base_Ret WHERE Base_Ret.Cod_Base EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Base_Ret THEN
     NomIva:SCREEN-VALUE = Base_Ret.Nombre.
  ELSE DO:
    RUN C-Bases.r (OUTPUT P_Codbase, OUTPUT P_Nombre, OUTPUT P_Porcent).
    NomIva:SCREEN-VALUE = P_Nombre + " - Porcentaje: " + STRING(P_Porcent).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.IvaRetenido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.IvaRetenido vTableWin
ON LEAVE OF RowObject.IvaRetenido IN FRAME F-Main /* IVA Retenido */
DO:
 DO WITH FRAME {&FRAME-NAME}:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN 
     NomIvaRet:SCREEN-VALUE = cuentas.nombre.
  ELSE DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
            NomIvaRet:SCREEN-VALUE = W_Pnombre.
  END.
 END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.IvaTransitorioRC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.IvaTransitorioRC vTableWin
ON LEAVE OF RowObject.IvaTransitorioRC IN FRAME F-Main /* Cta.IVA.TransRC */
DO:
 DO WITH FRAME {&FRAME-NAME}:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN 
     NomRC:SCREEN-VALUE = cuentas.nombre.
  ELSE DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
            NomRC:SCREEN-VALUE = W_Pnombre.
  END.
 END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.IvatransitorioRs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.IvatransitorioRs vTableWin
ON LEAVE OF RowObject.IvatransitorioRs IN FRAME F-Main /* Cta.IVA.TransRS */
DO:
 DO WITH FRAME {&FRAME-NAME}:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN 
     NomRS:SCREEN-VALUE = cuentas.nombre.
  ELSE DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
            NomRS:SCREEN-VALUE = W_Pnombre.
  END.
 END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.RteFte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.RteFte vTableWin
ON LEAVE OF RowObject.RteFte IN FRAME F-Main /* Cuenta Retención */
DO:
 DO WITH FRAME {&FRAME-NAME}:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN 
     NomRet:SCREEN-VALUE = cuentas.nombre.
  ELSE DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
            NomRet:SCREEN-VALUE = W_Pnombre.
  END.
 END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w_cmbcod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_cmbcod vTableWin
ON VALUE-CHANGED OF w_cmbcod IN FRAME F-Main /* Grupo Diferidos */
DO:
  RowObject.Cod_Producto:SCREEN-VALUE =  SUBSTRING(w_CmbCod:SCREEN-VALUE IN FRAME {&FRAME-NAME},3,3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w_cmbnew
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_cmbnew vTableWin
ON VALUE-CHANGED OF w_cmbnew IN FRAME F-Main /* Grupo Diferidos */
DO:
  RowObject.Cod_Producto:SCREEN-VALUE =  SUBSTRING(w_Cmbnew:SCREEN-VALUE IN FRAME {&FRAME-NAME},3,3).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Lista de cod_producto disponibles para configurar como diferidos
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DO w_posicion = 1 TO w_nroreg by 1 : 
     w_ok = W_Cmbnew:DELETE(1) IN FRAME {&FRAME-NAME}.
  END.
  W_nroreg = 0.  
  ENABLE W_Cmbnew  WITH FRAME  {&FRAME-NAME}.
  FOR EACH Varios WHERE Varios.Tipo   EQ 11 AND Varios.Estado EQ 1:
    FIND FIRST pro_diferidos WHERE pro_diferidos.cod_producto  EQ   Varios.codigo NO-ERROR.
    IF NOT AVAILABLE pro_diferidos THEN DO:
        W_ok = W_Cmbnew:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " +  TRIM(Varios.Descripcion)) IN FRAME {&FRAME-NAME}. 
        W_nroreg = W_nroreg + 1.    
    END.
  END. 
  IF W_nroreg GT 0 THEN 
       ENABLE W_Cmbnew  WITH FRAME  {&FRAME-NAME}.
  ELSE DO:
     HIDE W_Cmbnew  IN FRAME  {&FRAME-NAME}. 
     RUN MostrarMensaje IN w_manija (INPUT 41, OUTPUT w_eleccion).
     ON RETURN RETURN.
     RETURN.
  END.
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SUPER.
  IF W_nroreg EQ 0 THEN    
     RUN cancelrecord.
  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  HIDE W_Cmbnew in FRAME {&FRAME-NAME}.
  RUN SUPER.
  /* Code placed here will execute AFTER standard behavior.    */

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
  HIDE FRAME F-Main.
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
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).
    DO WITH FRAME f-MAIN:
    FIND FIRST cuentas WHERE cuentas.cuenta EQ RowObject.Dif_CtaGtoDif:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN
       Cta_Gtodif:SCREEN-VALUE = cuentas.nombre.
    FIND FIRST cuentas WHERE cuentas.cuenta EQ RowObject.Dif_CtaFuente:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN
       Cta_Fte:SCREEN-VALUE = cuentas.nombre.
       
    FIND FIRST cuentas WHERE cuentas.cuenta EQ RowObject.CtaSucAge:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN
       NomCtaSucAge:SCREEN-VALUE = cuentas.nombre.

    FIND FIRST Cuentas WHERE Cuentas.cuenta EQ RowObject.IVARetenido:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN
       NomIvaRet:SCREEN-VALUE = Cuentas.nombre.
    
    FIND FIRST Cuentas WHERE Cuentas.cuenta EQ RowObject.IVATransitorioRC:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN
       NomRC:SCREEN-VALUE = Cuentas.nombre.
    
    FIND FIRST Cuentas WHERE Cuentas.cuenta EQ RowObject.IVATransitorioRS:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN
       NomRS:SCREEN-VALUE = Cuentas.nombre.
    
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ RowObject.RteFte:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN
       NomRet:SCREEN-VALUE = Cuentas.nombre.
       
    FIND Base_Ret WHERE Base_Ret.Cod_Base EQ RowObject.IVA NO-LOCK NO-ERROR.
    IF AVAILABLE Base_Ret THEN
       NomIVA:SCREEN-VALUE = Base_Ret.Nombre.
    
    FIND FIRST Pro_Diferidos WHERE Pro_Diferidos.Cod_Producto EQ INTEGER(RowObject.Cod_Producto:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE Pro_Diferidos THEN
       W_CmbCod:SCREEN-VALUE = W_CmbCod:ENTRY(INTEGER(RowObject.Cod_producto:SCREEN-VALUE)).
/*       FIND Varios WHERE Varios.Tipo EQ 11 AND Varios.Codigo EQ Pro_Diferidos.Cod_Producto NO-LOCK NO-ERROR.
           W_CmbCod:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " +  TRIM(Varios.Descripcion).
    END.*/
    
    FIND Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia AND
                            Comprobantes.Comprobante EQ INTEGER(RowObject.Comprobante:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE Comprobantes THEN
       Cmb_Comprobantes:SCREEN-VALUE = STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre.
    ELSE
       Cmb_Comprobantes:SCREEN-VALUE = Cmb_Comprobantes:ENTRY(1).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
FOR EACH Varios WHERE Varios.Tipo EQ 11 AND Varios.Estado EQ 1:
     W_ok = W_Cmbcod:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " +  TRIM(Varios.Descripcion)) IN FRAME {&FRAME-NAME}. 
  END.   
  FOR EACH Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia AND
           Comprobantes.Estado EQ 1 NO-LOCK:
     W_Ok = Cmb_Comprobantes:ADD-LAST(STRING(Comprobantes.comprobante,"99") + " - " + Comprobantes.Nombre). 
  END.
  RUN SUPER.

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

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, CF_Diferidos. Cod: " + STRING(RowObject.Cod_Producto:SCREEN-VALUE IN FRAME F-Main)).
  /*Pro_Diferidos.Comprobante = INTEGER(SUBSTRING(Cmb_Comprobantes:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2)).*/
  
  DO w_posicion = 1 TO w_nroreg by 1 : 
     w_ok = W_Cmbnew:DELETE(1) IN FRAME {&FRAME-NAME}.
  END.
  W_nroreg = 0.  
  HIDE W_Cmbnew in FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewRecord vTableWin 
PROCEDURE viewRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

