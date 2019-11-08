&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

   {incluido/variable.i "SHARED"}

    DEFINE SHARED VARIABLE W_CodOperacion LIKE Operacion.Cod_Operacion.
    DEFINE VARIABLE W_Pcuenta    LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_Pnombre    LIKE Cuentas.Nombre.
    DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
    DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
    DEFINE VARIABLE W_ConsCta    LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_DispCta    LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_Val          AS LOGICAL.

   DEFINE VAR W_Ok AS LOGICAL.
   DEFINE VARIABLE W_Status        AS LOGICAL.
   DEFINE VARIABLE W_String        AS CHARACTER FORMAT "X(43)".
   DEFINE VARIABLE W_String2       AS CHARACTER FORMAT "X(55)".
   
   DEFINE VARIABLE Wk_Agencia    LIKE Agencias.Agencia.
   DEFINE VARIABLE W_Codigo      LIKE Operacion.Cod_Operacion.
   DEFINE VARIABLE W_Nit         LIKE Clientes.Nit.
   DEFINE VARIABLE W_Apellido    LIKE Clientes.Apellido1.
   DEFINE VARIABLE W_AgeCli      LIKE Clientes.Agencia.
   DEFINE VARIABLE W_Cuenta      LIKE Cuentas.Cuenta.
   DEFINE VARIABLE W_TipoPro     LIKE Operacion.Tipo_Producto.
   DEFINE VARIABLE W_Clase       LIKE Operacion.Clase_Operacion.
   DEFINE VARIABLE W_TipoOpe     LIKE Operacion.Tipo_Operacion.
   DEFINE VARIABLE W_Secuencia     AS INTEGER FORMAT "999".
   DEFINE VARIABLE W_Comprobante LIKE Operacion.Comprobante.

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
&Scoped-define EXTERNAL-TABLES Operacion
&Scoped-define FIRST-EXTERNAL-TABLE Operacion


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Operacion.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Operacion.Nom_Operacion ~
Operacion.Tipo_Producto Operacion.Id_SyA Operacion.Cuenta ~
Operacion.Cta_XPagarGMF Operacion.Cod_Deducible Operacion.Comision ~
Operacion.Cta_GtoGMF Operacion.Observaciones Operacion.Cta_SYA 
&Scoped-define ENABLED-TABLES Operacion
&Scoped-define FIRST-ENABLED-TABLE Operacion
&Scoped-Define ENABLED-OBJECTS RECT-204 RECT-205 RECT-206 RECT-207 RECT-208 ~
Cmb_TipoProductos Btn_Cuenta W_CmbCbte 
&Scoped-Define DISPLAYED-FIELDS Operacion.Cod_Operacion ~
Operacion.Nom_Operacion Operacion.Estado Operacion.Id_Clave ~
Operacion.Prioridad Operacion.Abreviado Operacion.Tipo_Producto ~
Operacion.Id_SyA Operacion.Nit_Predefinido Operacion.Cod_Compensa ~
Operacion.Fec_Creacion Operacion.Clase_Operacion Operacion.Ctrl_EfeChe ~
Operacion.Tipo_Validacion Operacion.Tipo_Operacion Operacion.Fec_Retiro ~
Operacion.Cta_XPagarGMF Operacion.Cod_Deducible Operacion.Comision ~
Operacion.Cta_GtoGMF Operacion.Observaciones Operacion.Cta_SYA 
&Scoped-define DISPLAYED-TABLES Operacion
&Scoped-define FIRST-DISPLAYED-TABLE Operacion
&Scoped-Define DISPLAYED-OBJECTS Cmb_TipoProductos Cuenta_Contable ~
W_Predefine W_Nombre Nom_Compensacion W_CmbCbte NomCxPGMF F_NomDed F_NomCom ~
NomGtoGMF NomCtaSyA 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Operacion.Abreviado 
&Scoped-define ADM-ASSIGN-FIELDS Operacion.Cod_Operacion Operacion.Estado ~
Operacion.Cuenta Operacion.Fec_Creacion Operacion.Fec_Retiro ~
Operacion.Comprobante 

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
DEFINE BUTTON Btn_Cuenta 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 53" 
     SIZE 3 BY .54.

DEFINE VARIABLE Cmb_TipoProductos AS CHARACTER FORMAT "X(50)":U 
     LABEL "Tipos de Produto" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CmbCbte AS CHARACTER FORMAT "X(43)":U 
     LABEL "Comprobante" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cuenta_Contable AS CHARACTER FORMAT "X(50)":U 
     LABEL "Cuenta Contable" 
     VIEW-AS FILL-IN 
     SIZE 70 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_NomCom AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_NomDed AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCtaSyA AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 28.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCxPGMF AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 28.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomGtoGMF AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 28.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Compensacion AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Nombre AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-204
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 95 BY 5.92.

DEFINE RECTANGLE RECT-205
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 4 BY 3.77.

DEFINE RECTANGLE RECT-206
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 4 BY 3.77.

DEFINE RECTANGLE RECT-207
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 4 BY 3.77.

DEFINE RECTANGLE RECT-208
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 4 BY 3.77.

DEFINE VARIABLE W_Predefine AS LOGICAL INITIAL no 
     LABEL "Nit Predefinido" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Operacion.Cod_Operacion AT ROW 1.77 COL 6 COLON-ALIGNED
          LABEL "Código"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Operacion.Nom_Operacion AT ROW 1.77 COL 21 COLON-ALIGNED HELP
          "Nombre de la Operación" NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 62 BY .81 TOOLTIP "Nombre de la Operación"
          BGCOLOR 15 
     Operacion.Estado AT ROW 2.62 COL 8 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 17 BY .81
     Operacion.Id_Clave AT ROW 2.62 COL 26
          VIEW-AS TOGGLE-BOX
          SIZE 14.29 BY .81
     Operacion.Prioridad AT ROW 2.62 COL 56 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20" 
          DROP-DOWN-LIST
          SIZE 6.72 BY 1
          BGCOLOR 15 
     Operacion.Abreviado AT ROW 2.62 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
          BGCOLOR 15 
     Operacion.Tipo_Producto AT ROW 2.88 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2.86 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Operacion.Id_SyA AT ROW 3.69 COL 26
          VIEW-AS TOGGLE-BOX
          SIZE 43 BY .77
          FONT 4
     Operacion.Cuenta AT ROW 5.31 COL 72 COLON-ALIGNED
          LABEL "Cuenta Contable" FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Cmb_TipoProductos AT ROW 5.46 COL 21 COLON-ALIGNED
     Cuenta_Contable AT ROW 6.38 COL 21 COLON-ALIGNED
     Btn_Cuenta AT ROW 6.38 COL 93
     W_Predefine AT ROW 7.46 COL 6
     Operacion.Nit_Predefinido AT ROW 7.46 COL 21 COLON-ALIGNED
          LABEL "Nit"
          VIEW-AS FILL-IN 
          SIZE 28 BY .81
          BGCOLOR 15 
     W_Nombre AT ROW 7.46 COL 49 COLON-ALIGNED NO-LABEL
     Operacion.Cod_Compensa AT ROW 8.54 COL 21 COLON-ALIGNED
          LABEL "Código Compensación"
          VIEW-AS FILL-IN 
          SIZE 28 BY .81
          BGCOLOR 15 
     Nom_Compensacion AT ROW 8.54 COL 49 COLON-ALIGNED NO-LABEL
     W_CmbCbte AT ROW 9.62 COL 21 COLON-ALIGNED
     Operacion.Fec_Creacion AT ROW 11.58 COL 79 COLON-ALIGNED
          LABEL "Fecha Creación"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Operacion.Clase_Operacion AT ROW 11.73 COL 6.43 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Taquilla", 1,
"Nómina", 2,
"Procesos", 3
          SIZE 11 BY 3.23
     Operacion.Ctrl_EfeChe AT ROW 11.73 COL 36.43 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Efectivo", 1,
"Cheque", 2,
"Ninguno", 3
          SIZE 10 BY 3.23
     Operacion.Tipo_Validacion AT ROW 11.73 COL 58.72 RIGHT-ALIGNED NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Ninguno", 1,
"Validadora", 2,
"Impresora", 3
          SIZE 10.29 BY 3.23
     Operacion.Tipo_Operacion AT ROW 12 COL 20.43 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Consignación", 1,
"Retiro", 2,
"Traslado", 3,
"Corrección", 4
          SIZE 13 BY 2.96
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     Operacion.Fec_Retiro AT ROW 12.38 COL 79 COLON-ALIGNED
          LABEL "Fecha Retiro"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     Operacion.Comprobante AT ROW 14 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
     Operacion.Cta_XPagarGMF AT ROW 15.73 COL 50 COLON-ALIGNED NO-LABEL FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 11.86 BY .81 TOOLTIP "Cta X Pagar GMF"
          BGCOLOR 15 
     NomCxPGMF AT ROW 15.73 COL 62 COLON-ALIGNED NO-LABEL
     Operacion.Cod_Deducible AT ROW 15.77 COL 11.57 COLON-ALIGNED
          LABEL "Cod. Deducible"
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     F_NomDed AT ROW 15.77 COL 17.57 COLON-ALIGNED NO-LABEL
     Operacion.Comision AT ROW 17.23 COL 11.43 COLON-ALIGNED
          LABEL "Comisión"
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     F_NomCom AT ROW 17.23 COL 17.43 COLON-ALIGNED NO-LABEL
     Operacion.Cta_GtoGMF AT ROW 17.23 COL 49.86 COLON-ALIGNED NO-LABEL FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     NomGtoGMF AT ROW 17.27 COL 61.86 COLON-ALIGNED NO-LABEL
     Operacion.Observaciones AT ROW 18.27 COL 13.29 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 37.29 BY 1.35
          BGCOLOR 15 FGCOLOR 7 FONT 5
     NomCtaSyA AT ROW 18.69 COL 61.86 COLON-ALIGNED NO-LABEL
     Operacion.Cta_SYA AT ROW 18.73 COL 49.86 COLON-ALIGNED NO-LABEL FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .81
          BGCOLOR 15 
     " Validación" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 11.19 COL 47.43
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Datos Generales" VIEW-AS TEXT
          SIZE 16 BY 1.08 AT ROW 4.5 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Clase" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 11.19 COL 5.43
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Cta-Contable S.y A. Coomeva" VIEW-AS TEXT
          SIZE 25.57 BY .5 AT ROW 18.19 COL 51.86
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Identificación de la Transacción" VIEW-AS TEXT
          SIZE 28 BY .77 AT ROW 1 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Control" VIEW-AS TEXT
          SIZE 7 BY .5 AT ROW 11.19 COL 34.43
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Tipo" VIEW-AS TEXT
          SIZE 5 BY .58 AT ROW 11.19 COL 18.43
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Cta-Contable X Pagar G.M.F." VIEW-AS TEXT
          SIZE 24 BY .5 AT ROW 15.23 COL 52.29
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Cta-Contable Gasto G.M.F." VIEW-AS TEXT
          SIZE 24 BY .5 AT ROW 16.73 COL 52.14
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Observ.:" VIEW-AS TEXT
          SIZE 7.86 BY .65 AT ROW 18.15 COL 5.14
          FGCOLOR 7 FONT 5
     RECT-204 AT ROW 5.04 COL 2
     RECT-205 AT ROW 11.46 COL 3.43
     RECT-206 AT ROW 11.46 COL 17.43
     RECT-207 AT ROW 11.46 COL 33.43
     RECT-208 AT ROW 11.46 COL 46.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Operacion
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
         HEIGHT             = 18.81
         WIDTH              = 96.29.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Operacion.Abreviado IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RADIO-SET Operacion.Clase_Operacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Operacion.Cod_Compensa IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Operacion.Cod_Deducible IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Operacion.Cod_Operacion IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Operacion.Comision IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Operacion.Comprobante IN FRAME F-Main
   NO-DISPLAY NO-ENABLE 2                                               */
ASSIGN 
       Operacion.Comprobante:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN Operacion.Cta_GtoGMF IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Operacion.Cta_SYA IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Operacion.Cta_XPagarGMF IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR RADIO-SET Operacion.Ctrl_EfeChe IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Operacion.Cuenta IN FRAME F-Main
   NO-DISPLAY 2 EXP-LABEL EXP-FORMAT                                    */
/* SETTINGS FOR FILL-IN Cuenta_Contable IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Operacion.Estado IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN Operacion.Fec_Creacion IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Operacion.Fec_Retiro IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN F_NomCom IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_NomDed IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Operacion.Id_Clave IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Operacion.Nit_Predefinido IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN NomCtaSyA IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCxPGMF IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomGtoGMF IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Compensacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Operacion.Nom_Operacion IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR COMBO-BOX Operacion.Prioridad IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Operacion.Tipo_Operacion IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Operacion.Tipo_Producto:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RADIO-SET Operacion.Tipo_Validacion IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN W_Nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX W_Predefine IN FRAME F-Main
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

&Scoped-define SELF-NAME Btn_Cuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cuenta V-table-Win
ON CHOOSE OF Btn_Cuenta IN FRAME F-Main /* Button 53 */
DO:
    RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                      OUTPUT W_CtrNat, INPUT "M").
    Cuenta_Contable:SCREEN-VALUE IN FRAME F-Main = STRING(W_PCuenta,"X(14)") + " - " + W_PNombre.

  /**/
  IF W_TipoPro EQ 1 OR W_TipoPro EQ 2 THEN DO:
   ASSIGN Operacion.Cod_Compensa:SENSITIVE = FALSE.
   RETURN.
  END.
  ASSIGN W_Cuenta = W_PCuenta
         Operacion.Cuenta:SCREEN-VALUE = STRING(W_Cuenta).
  FIND Cuentas WHERE Cuentas.Cuenta EQ W_Cuenta NO-LOCK NO-ERROR.
  IF AVAILABLE(Cuentas) THEN DO:
    IF Cuentas.Id_Caja AND Cuentas.Id_Nit THEN
       ASSIGN W_Predefine:SENSITIVE = TRUE.
    ELSE
       ASSIGN W_Predefine:SENSITIVE = FALSE.
    IF  Cuentas.Cod_FlujoEfec EQ "D" THEN DO:
       IF Cuentas.Car_Efectivo  EQ 2 THEN DO:
          ASSIGN Operacion.Cod_Compensa:SCREEN-VALUE    = ""
                 Operacion.Cod_Compensa:SENSITIVE       = FALSE
                 Operacion.Id_Clave:SENSITIVE           = TRUE
                 Operacion.Clase_Operacion:SCREEN-VALUE = "1"
                 Operacion.Clase_Operacion:SENSITIVE    = TRUE
                 Operacion.Ctrl_EfeChe:SENSITIVE        = TRUE
                 /*Operacion.Tipo_Operacion:SENSITIVE     = FALSE*/
                 Operacion.Tipo_Operacion:SCREEN-VALUE  = "1".
       END.
       ELSE
       IF Cuentas.Car_Efectivo  EQ 3 THEN DO:
          ASSIGN Operacion.Cod_Compensa:SENSITIVE       = TRUE
                 Operacion.Id_Clave:SENSITIVE           = TRUE
                 Operacion.Ctrl_EfeChe:SENSITIVE        = FALSE
                 /*Operacion.Tipo_Operacion:SENSITIVE     = FALSE*/
                 Operacion.Clase_Operacion:SCREEN-VALUE = "1"
                 Operacion.Clase_Operacion:SENSITIVE    = TRUE
                 Operacion.Ctrl_EfeChe:SCREEN-VALUE     = "2"
                 Operacion.Tipo_Operacion:SCREEN-VALUE  = "2".
       END.
   END.
   ELSE DO:
     IF Cuentas.Id_Caja THEN DO:
        ASSIGN Operacion.Clase_Operacion:SCREEN-VALUE = "1"
               Operacion.Ctrl_EfeChe:SCREEN-VALUE     = "3"
               Operacion.Clase_Operacion:SENSITIVE    = FALSE
               Operacion.Ctrl_EfeChe:SENSITIVE        = FALSE
              /* Operacion.Tipo_Operacion:SENSITIVE     = FALSE*/
               Operacion.Cod_Compensa:SENSITIVE       = FALSE
               Operacion.Id_Clave:SENSITIVE           = TRUE.
        IF Cuentas.Cod_Caja EQ 1 THEN
           ASSIGN Operacion.Tipo_Operacion:SCREEN-VALUE = "1".
        ELSE
           ASSIGN Operacion.Tipo_Operacion:SCREEN-VALUE = "2".
     END.
   END.
   APPLY "VALUE-CHANGED" TO Operacion.Clase_Operacion.
   RETURN NO-APPLY.
END.

  /**/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Operacion.Clase_Operacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Clase_Operacion V-table-Win
ON VALUE-CHANGED OF Operacion.Clase_Operacion IN FRAME F-Main /* Clase de Operación */
DO:
 DO WITH FRAME {&FRAME-NAME}:
    ASSIGN W_Clase = INTEGER(SELF:SCREEN-VALUE).
    IF Operacion.Tipo_Producto:SCREEN-VALUE EQ "1" OR
       Operacion.Tipo_Producto:SCREEN-VALUE EQ "2" THEN DO:
       IF W_Clase EQ 1 THEN
          ASSIGN Operacion.Ctrl_EfeChe:SENSITIVE     = TRUE
                 Operacion.Ctrl_EfeChe:SCREEN-VALUE  = "1"
                 Operacion.Prioridad:SENSITIVE       = FALSE
                 Operacion.Id_Clave:SENSITIVE        = TRUE
                 Operacion.Tipo_Validacion:SENSITIVE = TRUE.
       ELSE
       IF W_Clase EQ 2 THEN
          ASSIGN Operacion.Ctrl_EfeChe:SENSITIVE     = FALSE
                 Operacion.Ctrl_EfeChe:SCREEN-VALUE  = "3"
                 Operacion.Prioridad:SENSITIVE       = TRUE
                 Operacion.Id_Clave:SENSITIVE        = FALSE
                 Operacion.Tipo_Validacion:SENSITIVE = FALSE.
       ELSE
         ASSIGN Operacion.Ctrl_EfeChe:SENSITIVE     = FALSE
                Operacion.Ctrl_EfeChe:SCREEN-VALUE  = "3"
                Operacion.Prioridad:SENSITIVE       = FALSE
                Operacion.Id_Clave:SENSITIVE        = FALSE
                Operacion.Tipo_Validacion:SENSITIVE = FALSE.
    END.   
    ELSE DO:
       IF W_Clase EQ 1 THEN
          ASSIGN Operacion.Id_Clave:SENSITIVE = TRUE
                 Operacion.Tipo_Validacion:SENSITIVE = TRUE.
       ELSE
       IF W_Clase EQ 2 THEN
          ASSIGN Operacion.Id_Clave:SENSITIVE = FALSE
                 Operacion.Tipo_Validacion:SENSITIVE = FALSE.
       ELSE
       IF W_Clase EQ 3 THEN
          ASSIGN SELF:SCREEN-VALUE            = "1"
                 Operacion.Id_Clave:SENSITIVE = TRUE
                 Operacion.Tipo_Validacion:SENSITIVE = FALSE.
    END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_TipoProductos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TipoProductos V-table-Win
ON VALUE-CHANGED OF Cmb_TipoProductos IN FRAME F-Main /* Tipos de Produto */
DO:
   ASSIGN W_TipoPro = INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F-main,1,5))
          Operacion.Tipo_Producto:SCREEN-VALUE = STRING(W_TipoPro).

  RUN Habilitar_Deshabilitar.
/*     IF W_CmbCta:SCREEN-VALUE NE ? OR W_CmbCta:SCREEN-VALUE NE "" THEN
        ASSIGN W_CmbCta:SCREEN-VALUE = W_CmbCta:ENTRY(1).   
     APPLY "VALUE-CHANGED" TO W_CmbCta.*/
  IF W_TipoPro NE 1 AND W_TipoPro NE 2 THEN DO:
     APPLY "choose" TO Btn_Cuenta.
     /*RETURN NO-APPLY.*/
  END.
  ELSE SELF:SCREEN-VALUE = "".
  
  APPLY "VALUE-CHANGED" TO Operacion.Clase_Operacion.
  /*RETURN NO-APPLY.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Operacion.Cod_Compensa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Cod_Compensa V-table-Win
ON LEAVE OF Operacion.Cod_Compensa IN FRAME F-Main /* Código Compensación */
DO:
  DEFINE VAR P_CodCom LIKE Bancos.Cod_Compensa.
  DEFINE VAR P_Nombre LIKE Bancos.Nombre.
  IF SELF:SCREEN-VALUE EQ "00" OR SELF:SCREEN-VALUE EQ "?" THEN
     RUN C-Bancos.r (OUTPUT P_CodCom, OUTPUT P_Nombre).
     IF P_CodCom NE 0 THEN ASSIGN SELF:SCREEN-VALUE = STRING(P_CodCom)
                                  Nom_Compensacion:SCREEN-VALUE = P_Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Operacion.Cod_Deducible
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Cod_Deducible V-table-Win
ON TAB OF Operacion.Cod_Deducible IN FRAME F-Main /* Cod. Deducible */
OR RETURN OF Operacion.Cod_Deducible DO:
   DEFINE VAR W_Codigo LIKE Deducible.Cod_Deducible.
   DEFINE VAR W_Nomded LIKE Deducible.Nom_Deducible.
   
   DO WITH FRAME {&FRAME-NAME}:
      IF Operacion.Cod_Deducible:SCREEN-VALUE NE "" THEN DO:
         FIND Deducible WHERE Deducible.Cod_Deducible EQ Operacion.Cod_Deducible:SCREEN-VALUE 
                        NO-LOCK NO-ERROR.
         IF AVAILABLE(Deducible) THEN DO:
            ASSIGN F_NomDed:SCREEN-VALUE = Deducible.Nom_Deducible.
         END.
         ELSE DO:
           RUN C-Deducibles.r(OUTPUT W_Codigo,OUTPUT W_Nomded).
           ASSIGN Operacion.Cod_Deducible:SCREEN-VALUE = W_Codigo
                  F_NomDed:SCREEN-VALUE                = W_Nomded.
         END. 
      END. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Operacion.Comision
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Comision V-table-Win
ON TAB OF Operacion.Comision IN FRAME F-Main /* Comisión */
OR RETURN OF Operacion.Comision DO:
   DEFINE VAR W_Codigo LIKE Deducible.Cod_Deducible.
   DEFINE VAR W_Nomded LIKE Deducible.Nom_Deducible.
   
   DO WITH FRAME {&FRAME-NAME}:
      IF Operacion.Comision:SCREEN-VALUE NE "" THEN DO:
         FIND Deducible WHERE Deducible.Cod_Deducible EQ Operacion.Comision:SCREEN-VALUE 
                        NO-LOCK NO-ERROR.
         IF AVAILABLE(Deducible) THEN DO:
            ASSIGN F_NomCom:SCREEN-VALUE = Deducible.Nom_Deducible.
         END.
         ELSE DO:
           RUN C-Deducibles.r(OUTPUT W_Codigo,OUTPUT W_Nomded).
           ASSIGN Operacion.Comision:SCREEN-VALUE = W_Codigo
                  F_NomCom:SCREEN-VALUE           = W_Nomded.
         END. 
      END. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Operacion.Cta_GtoGMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Cta_GtoGMF V-table-Win
ON LEAVE OF Operacion.Cta_GtoGMF IN FRAME F-Main /* Cuenta */
DO:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE(Cuentas) THEN
     APPLY "MOUSE-SELECT-DBLCLICK" TO SELF.   
  ELSE ASSIGN NomGtoGMF:SCREEN-VALUE = Cuentas.Nombre. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Cta_GtoGMF V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Operacion.Cta_GtoGMF IN FRAME F-Main /* Cuenta */
DO:
   RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                    OUTPUT W_CtrNat, INPUT "M").
   IF W_PCuenta EQ ? THEN DO:
      FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE(Cuentas) THEN
         ASSIGN SELF:SCREEN-VALUE   = ""
                NomGtoGMF:SCREEN-VALUE = "".           
   END.
   ELSE
      ASSIGN SELF:SCREEN-VALUE      = W_Pcuenta
             NomGtoGMF:SCREEN-VALUE = W_Pnombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Operacion.Cta_SYA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Cta_SYA V-table-Win
ON LEAVE OF Operacion.Cta_SYA IN FRAME F-Main /* Cuenta Fuente */
DO:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE(Cuentas) THEN
     APPLY "MOUSE-SELECT-DBLCLICK" TO SELF.   
  ELSE ASSIGN NomCtaSyA:SCREEN-VALUE = Cuentas.Nombre. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Cta_SYA V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Operacion.Cta_SYA IN FRAME F-Main /* Cuenta Fuente */
DO:
   RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                    OUTPUT W_CtrNat, INPUT "M").
   IF W_PCuenta EQ ? THEN DO:
      FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE(Cuentas) THEN
         ASSIGN SELF:SCREEN-VALUE   = ""
                NomCtaSyA:SCREEN-VALUE = "".           
   END.
   ELSE
      ASSIGN SELF:SCREEN-VALUE      = W_Pcuenta
             NomCtaSyA:SCREEN-VALUE = W_Pnombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Operacion.Cta_XPagarGMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Cta_XPagarGMF V-table-Win
ON LEAVE OF Operacion.Cta_XPagarGMF IN FRAME F-Main /* Cuenta */
DO:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE(Cuentas) THEN
     APPLY "MOUSE-SELECT-DBLCLICK" TO SELF.   
  ELSE ASSIGN NomCxPGMF:SCREEN-VALUE = Cuentas.Nombre. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Cta_XPagarGMF V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Operacion.Cta_XPagarGMF IN FRAME F-Main /* Cuenta */
DO:
   RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                    OUTPUT W_CtrNat, INPUT "M").
   IF W_PCuenta EQ ? THEN DO:
      FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE(Cuentas) THEN
         ASSIGN SELF:SCREEN-VALUE   = ""
                NomCxPGMF:SCREEN-VALUE = "".           
   END.
   ELSE
      ASSIGN SELF:SCREEN-VALUE      = W_Pcuenta
             NomCxPGMF:SCREEN-VALUE = W_Pnombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Operacion.Nit_Predefinido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Nit_Predefinido V-table-Win
ON LEAVE OF Operacion.Nit_Predefinido IN FRAME F-Main /* Nit */
DO:
  ASSIGN W_Nit = Operacion.Nit_Predefinido:SCREEN-VALUE.

  IF W_Nit EQ "?"
  OR W_Nit EQ ? THEN
     RETURN.
  
  IF W_Nit NE "" THEN DO:
     FIND Clientes WHERE Clientes.Nit        EQ W_Nit
                     AND Clientes.Fec_Retiro EQ ?
                     AND Clientes.Estado     EQ 1 NO-LOCK NO-ERROR.
  END.
  IF NOT AVAILABLE Clientes
  OR W_Nit EQ "" THEN DO:
     RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT W_Nit,OUTPUT W_Nombre, OUTPUT W_Apellido, OUTPUT W_AgeCli).
     IF W_Nit EQ ""
     OR W_Nit EQ ? THEN DO:
        APPLY "ENTRY":U TO SELF.
        RETURN NO-APPLY.
     END.
  END.
  ELSE DO:
     FIND Clientes WHERE Clientes.Nit        EQ W_Nit
                     AND Clientes.Fec_Retiro EQ ?
                     AND Clientes.Estado     EQ 1 NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Clientes THEN DO:
        APPLY "ENTRY":U TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN W_Nombre   = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
  END.

  ASSIGN SELF:SCREEN-VALUE = W_Nit
         W_Nombre:SCREEN-VALUE = W_Nombre + W_Apellido.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Nit_Predefinido V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Operacion.Nit_Predefinido IN FRAME F-Main /* Nit */
DO:
  ASSIGN SELF:SCREEN-VALUE = "".
  APPLY "LEAVE":U TO SELF.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Operacion.Tipo_Operacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Operacion.Tipo_Operacion V-table-Win
ON VALUE-CHANGED OF Operacion.Tipo_Operacion IN FRAME F-Main /* Tipo de Operación */
DO:
  ASSIGN W_TipoOpe = INTEGER(SELF:SCREEN-VALUE).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbCbte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbCbte V-table-Win
ON VALUE-CHANGED OF W_CmbCbte IN FRAME F-Main /* Comprobante */
DO:
  IF SELF:SCREEN-VALUE EQ ? THEN
     RETURN.
     
  ASSIGN W_Comprobante = INTEGER(ENTRY(1,SELF:SCREEN-VALUE, "-"))
         Operacion.Comprobante:SCREEN-VALUE = STRING(W_Comprobante).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Predefine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Predefine V-table-Win
ON VALUE-CHANGED OF W_Predefine IN FRAME F-Main /* Nit Predefinido */
DO:
  IF W_Predefine:CHECKED THEN
     IF Operacion.Nit_Predefinido:SCREEN-VALUE EQ ? THEN
        ASSIGN Operacion.Nit_Predefinido:SCREEN-VALUE = "".
     ELSE
        ASSIGN Operacion.Nit_Predefinido:SENSITIVE    = TRUE.
  ELSE
     ASSIGN Operacion.Nit_Predefinido:SCREEN-VALUE = ?
            Operacion.Nit_Predefinido:SENSITIVE    = FALSE.
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
  {src/adm/template/row-list.i "Operacion"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Operacion"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna_Codigo V-table-Win 
PROCEDURE Asigna_Codigo :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Programa que Genera el Código de Operación para el Sistema.       
------------------------------------------------------------------------------*/
  DEFINE BUFFER Tmp_Operacion FOR Operacion.
  ASSIGN W_Secuencia = 1.
  REPEAT:
    ASSIGN W_Codigo    = (W_TipoPro * 10000000) + (W_Clase * 100000) + (W_TipoOpe * 1000) + W_Secuencia.
    FIND Tmp_Operacion WHERE Tmp_Operacion.Cod_Operacion EQ W_Codigo NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Tmp_Operacion THEN LEAVE.
    ASSIGN W_Secuencia = W_Secuencia + 1.
    IF W_Secuencia EQ 999 THEN DO:
       MESSAGE "No se permite definir mas Operaciones con estas caracteristicas"
         VIEW-AS ALERT-BOX.
       RETURN ERROR.
    END.
  END.
  
  ASSIGN Operacion.Cod_Operacion:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(W_Codigo).
  IF W_TipoPro NE 3 THEN
     ASSIGN Operacion.Cuenta:SCREEN-VALUE = "".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Habilitar_Deshabilitar V-table-Win 
PROCEDURE Habilitar_Deshabilitar :
DO WITH FRAME F-Main:
  IF W_TipoPro EQ 1
  OR W_TipoPro EQ 2 THEN DO:
     ASSIGN Btn_Cuenta:SENSITIVE                  = FALSE
            W_Predefine:CHECKED                 = FALSE
            W_Predefine:SENSITIVE               = FALSE
            Operacion.Clase_Operacion:SENSITIVE = TRUE
            Operacion.Tipo_Operacion:SENSITIVE  = TRUE.
  END.
  ELSE DO:
     ASSIGN Btn_Cuenta:SENSITIVE                  = TRUE
            Operacion.Clase_Operacion:SENSITIVE = FALSE
            Operacion.Ctrl_EfeChe:SENSITIVE     = FALSE.
            /*Operacion.Tipo_Operacion:SENSITIVE  = FALSE.*/
  END.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN Operacion.Tipo_Producto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(1)
         Operacion.Fec_Creacion:SCREEN-VALUE  = STRING(TODAY)
         Operacion.Cod_Operacion:SCREEN-VALUE = ""
         W_TipoPro = 1
         W_Clase   = 1
         W_TipoOpe = 1
         Operacion.Clase_Operacion:SENSITIVE = TRUE
         Operacion.Ctrl_EfeChe:SENSITIVE     = TRUE
         Operacion.Tipo_Operacion:SENSITIVE  = TRUE.
  APPLY "VALUE-CHANGED":U TO Operacion.Tipo_Producto.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
 OBSERVACIONES : Grabar Operación.       
------------------------------------------------------------------------------*/
  DEFINE VAR W_Evento AS   CHARACTER.
  DEFINE VAR A_Clase  LIKE Operacion.Clase_Operacion.
  DEFINE VAR A_Ctrl   LIKE Operacion.Ctrl_EfeChe.
  DEFINE VAR A_Tipo   LIKE Operacion.Tipo_Operacion.
  DEFINE VAR A_Cod    LIKE Operacion.Cod_Compensa.
  DEFINE VAR A_Nit    LIKE Operacion.Nit_Predefinido.
  DEFINE VAR A_Pri    LIKE Operacion.Prioridad.
  DEFINE VAR A_Cla    LIKE Operacion.Id_Clave.
  DEFINE VAR A_Val    LIKE Operacion.Tipo_Validacion.


  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Operacion.Clase_Operacion:SENSITIVE = TRUE
            Operacion.Ctrl_EfeChe:SENSITIVE     = TRUE
            Operacion.Tipo_Operacion:SENSITIVE  = TRUE
            Operacion.Cod_Compensa:SENSITIVE    = TRUE
            Operacion.Nit_Predefinido:SENSITIVE = TRUE
            W_Cuenta = TRIM(SUBSTRING(Cuenta_Contable:SCREEN-VALUE,1,14))
            W_Comprobante = INTEGER(ENTRY(1,W_CmbCbte:SCREEN-VALUE, "-")).

     IF Operacion.Nom_Operacion:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ ?
     OR Operacion.Nom_Operacion:SCREEN-VALUE EQ "?"
     OR Operacion.Nom_Operacion:SCREEN-VALUE EQ "" THEN DO:
        RUN MostrarMensaje IN W_Manija(INPUT 58,OUTPUT W_Status).
        APPLY "ENTRY" TO Operacion.Nom_Operacion.
        RETURN ERROR. 
     END.      

     IF Operacion.Abreviado:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ ?
     OR Operacion.Abreviado:SCREEN-VALUE EQ "?"
     OR Operacion.Abreviado:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "Requiere de un codigo Abreviado" VIEW-AS ALERT-BOX.
        APPLY "ENTRY" TO Operacion.Abreviado.
        RETURN ERROR. 
     END.      
     IF W_Predefine:SCREEN-VALUE EQ "SI" THEN DO:
        IF Operacion.Nit_Predefinido EQ ?
        OR Operacion.Nit_Predefinido EQ "" THEN DO:
           MESSAGE "Debe Especificar un Nit" VIEW-AS ALERT-BOX.
           APPLY "ENTRY" TO Operacion.Nit_Predefinido.
           RETURN ERROR.
        END.
     END.
     ASSIGN W_Evento = "SALVAR".
     RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
     IF RETURN-VALUE = "YES" THEN DO:
        RUN Asigna_Codigo NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          RETURN ERROR.
     END.
     ASSIGN A_Clase = INTEGER(Operacion.Clase_Operacion:SCREEN-VALUE)
            A_Ctrl  = INTEGER(Operacion.Ctrl_EfeChe:SCREEN-VALUE)
            A_Tipo  = INTEGER(Operacion.Tipo_Operacion:SCREEN-VALUE)
            A_Cod   = INTEGER(Operacion.Cod_Compensa:SCREEN-VALUE)
            A_Nit   = Operacion.Nit_Predefinido:SCREEN-VALUE
            A_Pri   = INTEGER(Operacion.Prioridad:SCREEN-VALUE)
            A_Val   = INTEGER(Operacion.Tipo_Validacion:SCREEN-VALUE).
     IF Operacion.Id_Clave:SCREEN-VALUE EQ "YES" THEN
        ASSIGN A_Cla = TRUE.
     ELSE
        ASSIGN A_Cla = FALSE.
  
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).
     ASSIGN Operacion.Clase_Operacion = A_Clase
            Operacion.Ctrl_EfeChe     = A_Ctrl
            Operacion.Tipo_Operacion  = A_Tipo
            Operacion.Cod_Compensa    = A_Cod
            Operacion.Nit_Predefinido = A_Nit
            Operacion.Cuenta          = W_Cuenta
            Operacion.Prioridad       = A_Pri
            Operacion.Id_Clave        = A_Cla
            Operacion.Tipo_Validacion = A_Val
            Operacion.Comprobante     = W_Comprobante
            Operacion.Cuenta          = W_Cuenta
            Operacion.Tipo_Producto   = INTEGER(Operacion.Tipo_Producto:SCREEN-VALUE).
            
     ASSIGN Operacion.Clase_Operacion:SENSITIVE = FALSE
            Operacion.Ctrl_EfeChe:SENSITIVE     = FALSE
            Operacion.Tipo_Operacion:SENSITIVE  = FALSE
            Operacion.Cod_Compensa:SENSITIVE    = FALSE
            Operacion.Nit_Predefinido:SENSITIVE = FALSE
            Btn_Cuenta:SENSITIVE                  = FALSE
            W_Predefine:SENSITIVE               = FALSE
            Operacion.Tipo_Validacion:SENSITIVE = FALSE.
     IF W_Evento EQ "SALVAR" THEN
        RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Operacion. Cod_Operacion: " + 
            STRING(Operacion.Cod_Operacion)).
     ELSE
        RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Operacion. Cod_Operacion: " + 
            STRING(Operacion.Cod_Operacion)).

    IF Operacion.Cuenta:SCREEN-VALUE NE " " AND Operacion.Cuenta:SCREEN-VALUE NE ? THEN
       ASSIGN Operacion.Cuenta = Operacion.Cuenta:SCREEN-VALUE.
  
  END.
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
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
  DO WITH FRAME {&FRAME-NAME}:
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ).
     ASSIGN Operacion.Clase_Operacion:SENSITIVE = FALSE
            Operacion.Ctrl_EfeChe:SENSITIVE     = FALSE
            Operacion.Tipo_Operacion:SENSITIVE  = FALSE
            Operacion.Cod_Compensa:SENSITIVE    = FALSE
            Operacion.Nit_Predefinido:SENSITIVE = FALSE
            Btn_Cuenta:SENSITIVE                  = FALSE
            W_Predefine:SENSITIVE               = FALSE
            Operacion.Tipo_Validacion:SENSITIVE = FALSE.
  END.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  OBSERVACIONES :        
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
     IF Operacion.Tipo_Producto GE 1 AND Operacion.Tipo_Producto LE 2 THEN DO:
        MESSAGE "No Se Permite Borrar Las Operaciones de Los Productos."
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN "ADM-ERROR".
     END.
     ELSE DO:
        IF Operacion.Fec_Retiro NE ? THEN
           RETURN "ADM-ERROR".
        RUN dispatch IN THIS-PROCEDURE ('update-record':U).
        IF AVAILABLE(Operacion) THEN DO:
           ASSIGN Operacion.Fec_Retiro = TODAY
                  Operacion.Estado     = 2.
           RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Operacion. Cod_Operacion: " + 
               STRING(Operacion.Cod_Operacion)).
        END.
     END.
  END.
  RUN dispatch IN THIS-PROCEDURE ('end-update':U).
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
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
  OBSERVACIONES :        
------------------------------------------------------------------------------*/

   DO WITH FRAME {&FRAME-NAME}:
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
      IF NOT AVAILABLE Operacion THEN RETURN.
   
      ASSIGN W_TipoPro = Operacion.Tipo_Producto
             W_Clase   = Operacion.Clase_Operacion.
      FIND Varios WHERE Varios.Tipo EQ 12 AND Varios.Codigo EQ W_TipoPro NO-LOCK NO-ERROR.
      IF AVAILABLE Varios THEN 
         Cmb_TipoProductos:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion.
      IF Operacion.Nit_Predefinido EQ "" OR Operacion.Nit_Predefinido EQ ? THEN DO:
         ASSIGN W_Predefine:SCREEN-VALUE = "NO"
                W_Nombre:SCREEN-VALUE    = "".
      END.
      ELSE DO:
         ASSIGN W_Predefine:SCREEN-VALUE = "YES".
         FIND Clientes WHERE Clientes.Nit        EQ Operacion.Nit_Predefinido
                         AND Clientes.Fec_Retiro EQ ?
                         AND Clientes.Estado     EQ 1 NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN DO:
            ASSIGN W_Nombre:SCREEN-VALUE = Clientes.Nombre + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
         END.
      END.
      FIND Deducible WHERE Deducible.Cod_Deducible EQ Operacion.Cod_Deducible NO-LOCK NO-ERROR.
      IF AVAILABLE(Deducible) THEN
         ASSIGN F_NomDed:SCREEN-VALUE = Deducible.Nom_Deducible.
      ELSE
         ASSIGN F_NomDed:SCREEN-VALUE = "".
      FIND Deducible WHERE Deducible.Cod_Deducible EQ Operacion.Comision NO-LOCK NO-ERROR.
      IF AVAILABLE(Deducible) THEN
         ASSIGN F_NomCom:SCREEN-VALUE = Deducible.Nom_Deducible.
      ELSE
         ASSIGN F_NomCom:SCREEN-VALUE = "".
         
      IF Operacion.Comprobante EQ 0 THEN DO:
         ASSIGN W_CmbCbte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00-NINGUNO".
      END.
      ELSE DO:
         FIND Comprobantes WHERE Comprobantes.Agencia EQ Wk_Agencia
                             AND Comprobantes.Comprobante EQ Operacion.Comprobante NO-LOCK NO-ERROR.
         IF AVAILABLE Comprobantes THEN DO:
            ASSIGN W_String = STRING(Comprobantes.Comprobante, "99") + "-" + STRING(Comprobantes.Nombre,"X(40)")
                   W_CmbCbte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_String.
         END.
      END.
      
      IF Operacion.Tipo_Producto EQ 1 OR Operacion.Tipo_Producto EQ 2 THEN DO:
         ASSIGN Cuenta_Contable:SCREEN-VALUE = "".
      END.
      ELSE DO:
         FIND Cuentas WHERE Cuentas.Cuenta EQ Operacion.Cuenta NO-LOCK NO-ERROR.
         IF AVAILABLE(Cuentas) THEN DO:
            ASSIGN Cuenta_Contable:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Cuentas.Cuenta,"X(14)") + " - " + Cuentas.Nombre.
         END.
      END.
                                                      
      FIND Cuentas WHERE Cuentas.Cuenta EQ Operacion.Cta_XPagarGMF
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
      IF AVAILABLE(Cuentas) THEN
         ASSIGN NomCxPGMF:SCREEN-VALUE = Cuentas.Nombre. 
         
      FIND Cuentas WHERE Cuentas.Cuenta EQ Operacion.Cta_GtoGMF
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
      IF AVAILABLE(Cuentas) THEN
         ASSIGN NomGtoGMF:SCREEN-VALUE = Cuentas.Nombre. 

      FIND Cuentas WHERE Cuentas.Cuenta EQ Operacion.Cta_SyA
                     AND Cuentas.Tipo   EQ 2 
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
      IF AVAILABLE(Cuentas) THEN
         ASSIGN NomCtaSyA:SCREEN-VALUE = Cuentas.Nombre. 
      
      
      W_CodOperacion = Operacion.Cod_Operacion.
      RUN Habilitar_Deshabilitar.
      ASSIGN Operacion.Cuenta:SCREEN-VALUE = Operacion.Cuenta
             Operacion.Cuenta:VISIBLE      = TRUE
             Operacion.Cuenta:SENSITIVE    = TRUE
             Operacion.Tipo_Operacion:SENSITIVE  = TRUE.

      IF Operacion.Tipo_Producto EQ 4 THEN
         Operacion.Tipo_Producto:SCREEN-VALUE = "4".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  OBSERVACIONES :        
------------------------------------------------------------------------------*/  
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN W_TipoPro = 1
         W_Clase   = 1
         W_TipoOpe = 1
         Operacion.Ctrl_EfeChe:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
  FOR EACH Varios WHERE Varios.Tipo EQ 12 NO-LOCK:
    W_Ok = Cmb_TipoProductos:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
  END.
  FIND FIRST Agencias WHERE Agencias.Tip_Agencia EQ "C" NO-LOCK NO-ERROR.
  IF AVAILABLE Agencias THEN DO:
     ASSIGN Wk_Agencia = Agencias.Agencia.
            W_CmbCbte:LIST-ITEMS = "00-NINGUNO".
     FOR EACH Comprobantes WHERE Comprobantes.Agencia EQ Wk_Agencia
                             AND Comprobantes.Estado  EQ 1
                             AND Comprobantes.Fec_Retiro EQ ? NO-LOCK
                              BY Comprobantes.Comprobante:
         ASSIGN W_String = STRING(Comprobantes.Comprobante, "99") + "-" + STRING(Comprobantes.Nombre,"X(40)").
         IF W_CmbCbte:ADD-LAST(W_String) THEN.
     END.
  END.  
  /* Dispatch standard ADM method.                             */
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
  {src/adm/template/snd-list.i "Operacion"}

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

