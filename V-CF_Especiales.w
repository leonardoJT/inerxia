&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */


{incluido\variable.i "SHARED"}.
DEFINE SHARED VAR W_CodProE LIKE Pro_Especiales.Cod_Producto.

DEFINE VARIABLE W_NomCli        LIKE Clientes.Nombre.
DEFINE VARIABLE W_ApeCli        LIKE Clientes.Apellido1.
DEFINE VARIABLE W_AgeCli        LIKE Clientes.Agencia.
DEFINE VARIABLE W_NitCli        LIKE Clientes.Nit.
DEFINE VARIABLE W_Cuenta         LIKE Cuentas.Cuenta.
DEFINE VARIABLE W_Nombre         LIKE Cuentas.Nombre.
DEFINE VARIABLE W_NatTra         LIKE Cuentas.Naturaleza.
DEFINE VARIABLE W_CtrNat         LIKE Cuentas.Ctr_Naturaleza.
DEFINE VARIABLE W_ProdPrior      LIKE Pro_Especiales.Prioridad.
DEFINE VARIABLE W_TipCta           AS CHARACTER INITIAL "M".
DEFINE VARIABLE W_Rowid            AS ROWID.
DEFINE VARIABLE W_Procmto          AS CHARACTER.
DEFINE VARIABLE W_SuperUsu         AS LOGICAL.
DEFINE VARIABLE W_Rpta             AS LOGICAL.
DEFINE VARIABLE W_AuxStrLogico1    AS CHARACTER FORMAT "X(2)".
DEFINE VARIABLE W_AuxStrLogico2    AS CHARACTER FORMAT "X(2)".

DEFINE BUFFER Tmp_Pro_Espe        FOR Pro_Especiales.

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
&Scoped-define EXTERNAL-TABLES PRO_ESPECIALES
&Scoped-define FIRST-EXTERNAL-TABLE PRO_ESPECIALES


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR PRO_ESPECIALES.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Pro_Especiales.Id_Proceso ~
Pro_Especiales.Per_Proceso Pro_Especiales.Cuo_FijaVar ~
Pro_Especiales.Id_Linea Pro_Especiales.Pro_Asociado ~
Pro_Especiales.Plazo_Minimo Pro_Especiales.Plazo_Maximo ~
Pro_Especiales.Ran_IniCuota Pro_Especiales.Ran_FinCuota Pro_Especiales.Nit ~
Pro_Especiales.Id_Contabiliza Pro_Especiales.Id_CtrSaldo ~
Pro_Especiales.Cta_Cargos Pro_Especiales.Cta_Recaudos ~
Pro_Especiales.Cta_SucyAge 
&Scoped-define ENABLED-TABLES Pro_Especiales
&Scoped-define FIRST-ENABLED-TABLE Pro_Especiales
&Scoped-Define ENABLED-OBJECTS RECT-199 RECT-200 RECT-218 RECT-219 RECT-263 ~
RECT-264 
&Scoped-Define DISPLAYED-FIELDS Pro_Especiales.Id_Proceso ~
Pro_Especiales.Per_Proceso Pro_Especiales.Cuo_FijaVar ~
Pro_Especiales.Id_Linea Pro_Especiales.agencia Pro_Especiales.Estado ~
Pro_Especiales.Cod_Producto Pro_Especiales.Nom_Producto ~
Pro_Especiales.Pro_Asociado Pro_Especiales.Prioridad ~
Pro_Especiales.Plazo_Minimo Pro_Especiales.Plazo_Maximo ~
Pro_Especiales.Ran_IniCuota Pro_Especiales.Ran_FinCuota ~
Pro_Especiales.Fec_Creacion Pro_Especiales.Fec_Retiro Pro_Especiales.Nit ~
Pro_Especiales.Id_Contabiliza Pro_Especiales.Id_CtrSaldo ~
Pro_Especiales.Cta_Cargos Pro_Especiales.Cta_Recaudos ~
Pro_Especiales.Cta_SucyAge 
&Scoped-define DISPLAYED-TABLES Pro_Especiales
&Scoped-define FIRST-DISPLAYED-TABLE Pro_Especiales
&Scoped-Define DISPLAYED-OBJECTS W_NomOfi W_NomTercero W_NomCar W_NomReca ~
W_NomSucAge 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Pro_Especiales.Nom_Producto 
&Scoped-define ADM-ASSIGN-FIELDS Pro_Especiales.agencia ~
Pro_Especiales.Estado Pro_Especiales.Cod_Producto Pro_Especiales.Prioridad ~
Pro_Especiales.Fec_Creacion 

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
DEFINE VARIABLE W_NomCar AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomOfi AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomReca AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomSucAge AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTercero AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 66 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-199
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 2.69.

DEFINE RECTANGLE RECT-200
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 4 BY 3.23.

DEFINE RECTANGLE RECT-218
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 2.65.

DEFINE RECTANGLE RECT-219
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 3.77.

DEFINE RECTANGLE RECT-263
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 5 BY 3.23.

DEFINE RECTANGLE RECT-264
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 1.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Pro_Especiales.Id_Proceso AT ROW 6.38 COL 73
          LABEL "Mtto. por Proceso"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .77
     Pro_Especiales.Per_Proceso AT ROW 9.35 COL 13 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Mensual", 4,
"Bimestral", 5,
"Trimestral", 6,
"Semestral", 8,
"Anual", 9,
"Unica", 11
          SIZE 77 BY 1.08
     Pro_Especiales.Cuo_FijaVar AT ROW 5.31 COL 39 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Fija", yes,
"Rango", no
          SIZE 14 BY 2.15
     Pro_Especiales.Id_Linea AT ROW 4.5 COL 73
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .77
     Pro_Especiales.agencia AT ROW 1.27 COL 9 COLON-ALIGNED
          LABEL "Agencia"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NomOfi AT ROW 1.27 COL 23 COLON-ALIGNED NO-LABEL
     Pro_Especiales.Estado AT ROW 3.42 COL 11 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 16 BY .81
          BGCOLOR 17 FGCOLOR 0 FONT 4
     Pro_Especiales.Cod_Producto AT ROW 2.35 COL 9 COLON-ALIGNED
          LABEL "Producto"
          VIEW-AS FILL-IN 
          SIZE 14.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Pro_Especiales.Nom_Producto AT ROW 2.35 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 41 BY .81
          BGCOLOR 15 
     Pro_Especiales.Pro_Asociado AT ROW 5.31 COL 15 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Todos", 1,
"Asociado", 2,
"No Asociado", 3
          SIZE 13 BY 2.15
     Pro_Especiales.Prioridad AT ROW 3.42 COL 85 COLON-ALIGNED
          LABEL "Prioridad"
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Pro_Especiales.Plazo_Minimo AT ROW 12.85 COL 23 COLON-ALIGNED
          LABEL "Mínimo en días"
          VIEW-AS FILL-IN 
          SIZE 23 BY .81
          BGCOLOR 15 
     Pro_Especiales.Plazo_Maximo AT ROW 13.92 COL 23 COLON-ALIGNED
          LABEL "Máximo en días"
          VIEW-AS FILL-IN 
          SIZE 23 BY .81
          BGCOLOR 15 
     Pro_Especiales.Ran_IniCuota AT ROW 12.85 COL 65 COLON-ALIGNED
          LABEL "Inicial"
          VIEW-AS FILL-IN 
          SIZE 23 BY .81
          BGCOLOR 15 
     Pro_Especiales.Ran_FinCuota AT ROW 13.92 COL 65 COLON-ALIGNED
          LABEL "Final"
          VIEW-AS FILL-IN 
          SIZE 22.57 BY .81
          BGCOLOR 15 
     Pro_Especiales.Fec_Creacion AT ROW 1.27 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Pro_Especiales.Fec_Retiro AT ROW 2.35 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Pro_Especiales.Nit AT ROW 10.96 COL 9 COLON-ALIGNED
          LABEL "Nit"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     W_NomTercero AT ROW 10.96 COL 23 COLON-ALIGNED NO-LABEL
     Pro_Especiales.Id_Contabiliza AT ROW 4.5 COL 56
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .77
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     Pro_Especiales.Id_CtrSaldo AT ROW 6.38 COL 56
          VIEW-AS TOGGLE-BOX
          SIZE 13 BY .77
     Pro_Especiales.Cta_Cargos AT ROW 16.35 COL 31 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17.43 BY .81
          BGCOLOR 15 
     W_NomCar AT ROW 16.35 COL 49 COLON-ALIGNED NO-LABEL
     Pro_Especiales.Cta_Recaudos AT ROW 17.42 COL 31 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17.43 BY .81
          BGCOLOR 15 
     W_NomReca AT ROW 17.42 COL 49 COLON-ALIGNED NO-LABEL
     Pro_Especiales.Cta_SucyAge AT ROW 18.5 COL 31 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17.43 BY .81
          BGCOLOR 15 
     W_NomSucAge AT ROW 18.54 COL 49 COLON-ALIGNED NO-LABEL
     RECT-199 AT ROW 12.31 COL 51
     RECT-200 AT ROW 4.77 COL 36
     RECT-218 AT ROW 12.31 COL 11
     RECT-219 AT ROW 15.81 COL 11
     RECT-263 AT ROW 4.77 COL 11
     RECT-264 AT ROW 8.81 COL 11
     " Periodicidad para Realizar el Proceso" VIEW-AS TEXT
          SIZE 34 BY 1.08 AT ROW 8.27 COL 13
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Configuración Contable" VIEW-AS TEXT
          SIZE 21 BY 1.08 AT ROW 15.27 COL 14
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Este producto aplica a..." VIEW-AS TEXT
          SIZE 22 BY 1.08 AT ROW 4.23 COL 13
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Cuenta Suc. y Agencias" VIEW-AS TEXT
          SIZE 21 BY .81 AT ROW 18.5 COL 12
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Fecha de Retiro" VIEW-AS TEXT
          SIZE 11 BY .77 AT ROW 2.35 COL 67
     "Fecha Creación" VIEW-AS TEXT
          SIZE 11 BY .77 AT ROW 1.27 COL 67
          FGCOLOR 0 
     "Cuenta  Cargos" VIEW-AS TEXT
          SIZE 14 BY .77 AT ROW 16.35 COL 19
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Cuenta  Recaudos" VIEW-AS TEXT
          SIZE 17 BY .81 AT ROW 17.42 COL 16
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Tipo Cuota" VIEW-AS TEXT
          SIZE 10 BY 1.08 AT ROW 4.23 COL 38
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Plazos" VIEW-AS TEXT
          SIZE 8 BY 1.04 AT ROW 11.77 COL 14
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Rangos de la Cuota" VIEW-AS TEXT
          SIZE 18 BY 1.08 AT ROW 11.77 COL 54
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ESPECIAL.PRO_ESPECIALES
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
         WIDTH              = 94.57.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Pro_Especiales.agencia IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Pro_Especiales.Cod_Producto IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR RADIO-SET Pro_Especiales.Estado IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN Pro_Especiales.Fec_Creacion IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN Pro_Especiales.Fec_Retiro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Pro_Especiales.Id_Proceso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Especiales.Nit IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Especiales.Nom_Producto IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Pro_Especiales.Plazo_Maximo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Especiales.Plazo_Minimo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Especiales.Prioridad IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Pro_Especiales.Ran_FinCuota IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Pro_Especiales.Ran_IniCuota IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_NomCar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomOfi IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomReca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomSucAge IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomTercero IN FRAME F-Main
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

&Scoped-define SELF-NAME Pro_Especiales.Cod_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Especiales.Cod_Producto V-table-Win
ON LEAVE OF Pro_Especiales.Cod_Producto IN FRAME F-Main /* Producto */
DO:
  W_CodProE = INPUT Pro_Especiales.Cod_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Especiales.Cta_Cargos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Especiales.Cta_Cargos V-table-Win
ON LEAVE OF Pro_Especiales.Cta_Cargos IN FRAME F-Main /* Cuenta Cargos */
DO:
  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Especiales.Cta_Cargos:SCREEN-VALUE
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Id_Nit EQ TRUE
               NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Cuentas THEN DO:
     APPLY "MOUSE-SELECT-DBLCLICK":U TO SELF.
     IF W_Cuenta EQ ? THEN DO:
        APPLY "ENTRY":U TO SELF.
        RETURN NO-APPLY.
     END.
  END.
  ELSE
     ASSIGN W_NomCar:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Especiales.Cta_Cargos V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Pro_Especiales.Cta_Cargos IN FRAME F-Main /* Cuenta Cargos */
DO:
  ASSIGN W_Cuenta = ?.
  RUN P-BCtNit.r (OUTPUT W_Cuenta, OUTPUT W_Nombre, OUTPUT W_NatTra, 
                  OUTPUT W_CtrNat, INPUT W_TipCta).
                  
  IF W_Cuenta NE ? THEN
     ASSIGN Pro_Especiales.Cta_Cargos:SCREEN-VALUE = W_Cuenta
            W_NomCar:SCREEN-VALUE                  = W_Nombre.
  ELSE
     RUN MostrarMensaje IN W_Manija (INPUT 414, OUTPUT W_Rpta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Especiales.Cta_Recaudos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Especiales.Cta_Recaudos V-table-Win
ON LEAVE OF Pro_Especiales.Cta_Recaudos IN FRAME F-Main /* Cuenta Recaudos */
DO:
  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Especiales.Cta_Recaudos:SCREEN-VALUE
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2
                 AND Cuentas.Id_Nit EQ TRUE  
               NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Cuentas THEN DO:
    APPLY "MOUSE-SELECT-DBLCLICK":U TO SELF.
    IF W_Cuenta EQ ? THEN DO:
       APPLY "ENTRY":U TO SELF.
       RETURN NO-APPLY.
    END.
  END.
  ELSE
    ASSIGN W_NomReca:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Especiales.Cta_Recaudos V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Pro_Especiales.Cta_Recaudos IN FRAME F-Main /* Cuenta Recaudos */
DO:
  ASSIGN W_Cuenta = ?.
  RUN P-BCtNit.r (OUTPUT W_Cuenta, OUTPUT W_Nombre, OUTPUT W_NatTra, 
                  OUTPUT W_CtrNat, INPUT W_TipCta).
                  
  IF W_Cuenta NE ? THEN
     ASSIGN Pro_Especiales.Cta_Recaudos:SCREEN-VALUE = W_Cuenta
            W_NomReca:SCREEN-VALUE                   = W_Nombre.
  ELSE
     RUN MostrarMensaje IN W_Manija (INPUT 415, OUTPUT W_Rpta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Especiales.Cta_SucyAge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Especiales.Cta_SucyAge V-table-Win
ON LEAVE OF Pro_Especiales.Cta_SucyAge IN FRAME F-Main /* Cuenta Sucursales y Agencias */
DO:
  IF Pro_Especiales.Cta_SucyAge:SCREEN-VALUE EQ ""
  OR Pro_Especiales.Cta_SucyAge:SCREEN-VALUE EQ ? THEN DO:
     ASSIGN Pro_Especiales.Cta_SucyAge:SCREEN-VALUE = ""
            W_NomSucAge:SCREEN-VALUE                = "".       
     RETURN.
  END.

  FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Especiales.Cta_SucyAge:SCREEN-VALUE
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2
               NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Cuentas THEN 
    APPLY "MOUSE-SELECT-DBLCLICK":U TO SELF.
  ELSE
    ASSIGN W_NomSucAge:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Especiales.Cta_SucyAge V-table-Win
ON MOUSE-SELECT-DBLCLICK OF Pro_Especiales.Cta_SucyAge IN FRAME F-Main /* Cuenta Sucursales y Agencias */
DO:
  ASSIGN W_Cuenta = ?.
  RUN P-brwcta.r (OUTPUT W_Cuenta, OUTPUT W_Nombre, OUTPUT W_NatTra, 
                  OUTPUT W_CtrNat, INPUT W_TipCta).
  IF W_Cuenta NE ? THEN
     ASSIGN Pro_Especiales.Cta_SucyAge:SCREEN-VALUE = W_Cuenta
            W_NomSucAge:SCREEN-VALUE                = W_Nombre.
  ELSE
     ASSIGN Pro_Especiales.Cta_SucyAge:SCREEN-VALUE = ""
            W_NomSucAge:SCREEN-VALUE                = "".       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Especiales.Id_Contabiliza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Especiales.Id_Contabiliza V-table-Win
ON VALUE-CHANGED OF Pro_Especiales.Id_Contabiliza IN FRAME F-Main /* Contabiliza */
DO:
  IF Pro_Especiales.Id_Contabiliza:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "Si" THEN
     ASSIGN Pro_Especiales.Cta_Cargos:SENSITIVE    = TRUE
            W_NomCar:VISIBLE                       = TRUE.
  ELSE
     ASSIGN Pro_Especiales.Cta_Cargos:SENSITIVE    = FALSE
            Pro_Especiales.Cta_Cargos:SCREEN-VALUE = ""
            W_NomCar:SCREEN-VALUE                  = ""
            W_NomCar:VISIBLE                       = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pro_Especiales.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_Especiales.Nit V-table-Win
ON LEAVE OF Pro_Especiales.Nit IN FRAME F-Main /* Nit */
DO:
  IF Pro_Especiales.Nit:SCREEN-VALUE EQ ""
  OR Pro_Especiales.Nit:SCREEN-VALUE EQ ? THEN DO:
     ASSIGN Pro_Especiales.Nit:SCREEN-VALUE = ""
            W_NomTercero:SCREEN-VALUE       = "".
     RETURN.       
  END.
  
  FIND Clientes WHERE Clientes.Nit EQ Pro_Especiales.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Clientes THEN
  DO:
    ASSIGN W_NomCli = ?
           W_ApeCli = ?
           W_NitCli = ?.
    RUN C-Clientes.R (INPUT W_Agencia, OUTPUT W_NitCli, OUTPUT W_NomCli, OUTPUT W_ApeCli, OUTPUT W_AgeCli).
 
    IF W_NitCli NE ? THEN 
       ASSIGN W_NomTercero:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
              TRIM(W_NomCli) + " " + TRIM(W_ApeCli)
              Pro_Especiales.Nit:SCREEN-VALUE = W_NitCli.
     ELSE
       ASSIGN W_NomTercero:SCREEN-VALUE       = ""
              Pro_Especiales.Nit:SCREEN-VALUE = "".
  END.
  ELSE
     ASSIGN W_NomTercero:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
            TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
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
  {src/adm/template/row-list.i "PRO_ESPECIALES"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "PRO_ESPECIALES"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */

  RUN Superusuario IN W_Manija (W_Agencia, W_Usuario, OUTPUT W_SuperUsu).
  FIND FIRST Agencias WHERE Agencias.Agencia     EQ W_Agencia
                        AND Agencias.Tip_Agencia EQ "C" NO-LOCK NO-ERROR.
                  
  IF NOT AVAILABLE Agencias
  OR NOT W_SuperUsu
  OR W_Prioridad LT 4 THEN DO:
     RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT "TABLEIO", OUTPUT W_Procmto).
     RUN local-enable IN WIDGET-HANDLE(W_Procmto).
     RUN MostrarMensaje IN W_Manija (INPUT 296,OUTPUT W_Eleccion).
     APPLY "ENTRY" TO Pro_Especiales.Nom_Producto IN FRAME {&FRAME-NAME}.
     RETURN.
  END.  

  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT "CONTAINER", OUTPUT W_Procmto).
  RUN Estado_Btn IN WIDGET-HANDLE(W_Procmto) (FALSE).

  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) . 
  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN Pro_Especiales.Agencia:SCREEN-VALUE           = STRING(W_Agencia)
         W_NomOfi:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = Agencias.Nombre
         Pro_Especiales.Fec_Creacion:SCREEN-VALUE      = STRING(TODAY)
         W_NomTercero:SCREEN-VALUE                     = ""
         W_CodProE                                     = 1
         W_NomCar:SCREEN-VALUE                         = ""
         W_NomReca:SCREEN-VALUE                        = ""
         W_NomSucAge:SCREEN-VALUE                      = ""
         W_Rowid                                       = ROWID(Pro_Especiales).
             
  IF Pro_Especiales.Id_Contabiliza:SCREEN-VALUE EQ "Si" THEN
     ASSIGN Pro_Especiales.Cta_Cargos:SENSITIVE    = TRUE
            W_NomCar:VISIBLE                       = TRUE.
  ELSE
     ASSIGN Pro_Especiales.Cta_Cargos:SENSITIVE    = FALSE
            W_NomCar:VISIBLE                       = FALSE.

  FIND LAST Pro_Especiales WHERE Pro_Especiales.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
  IF AVAILABLE Pro_Especiales THEN
     ASSIGN W_CodProE = Pro_Especiales.Cod_Producto + 1.
  ELSE
     ASSIGN W_CodProE = 1.
         
  FIND Pro_Especiales WHERE W_Rowid EQ ROWID(Pro_Especiales) NO-ERROR.
  ASSIGN Pro_Especiales.Cod_Producto:SCREEN-VALUE = STRING(W_CodProE).
  RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Pro_Especiales. Cod_Producto: " + 
      STRING(Pro_Especiales.Cod_Producto)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN DO:
     /* BUSQUEDA DE LA PRIORIDAD MAYOR EN UN PRODUCTO (Pro_Especiales, Pro_Ahorros y Pro_Creditos) */  
    ASSIGN W_ProdPrior = 1.

    FIND LAST Tmp_Pro_Espe WHERE Tmp_Pro_Espe.Prioridad GE W_ProdPrior NO-LOCK NO-ERROR.
    IF AVAILABLE Tmp_Pro_Espe THEN
       ASSIGN W_ProdPrior = Tmp_Pro_Espe.Prioridad + 1.

    FIND LAST Pro_Ahorros WHERE Pro_Ahorros.Prioridad GE W_ProdPrior NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Ahorros THEN
       ASSIGN W_ProdPrior = Pro_Ahorros.Prioridad + 1.

    FIND LAST Pro_Creditos WHERE Pro_Creditos.Prioridad GE W_ProdPrior NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Creditos THEN
       ASSIGN W_ProdPrior = Pro_Creditos.Prioridad + 1.

    ASSIGN Pro_Especiales.Prioridad = W_ProdPrior
           Pro_Especiales.Prioridad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(W_ProdPrior).
   /* FIN BUSQUEDA. */
   END.

  ELSE DO:
     ASSIGN W_AuxStrLogico1 = IF Pro_Especiales.Id_Contabiliza THEN "Si"
                              ELSE "No"
            W_AuxStrLogico2 = IF Pro_Especiales.Id_CtrSaldo THEN "Si"
                              ELSE "No".

     IF W_AuxStrLogico1 NE Pro_Especiales.Id_Contabiliza:SCREEN-VALUE 
     OR W_AuxStrLogico2 NE Pro_Especiales.Id_CtrSaldo:SCREEN-VALUE 
     THEN DO:
        FIND FIRST Especiales WHERE Especiales.Cod_Producto EQ Pro_Especiales.Cod_Producto
                                AND Especiales.Estado       EQ 1 NO-LOCK NO-ERROR.

        IF AVAILABLE Especiales THEN DO:
           RUN MostrarMensaje IN W_Manija (INPUT 389, OUTPUT W_Rpta).
           RETURN ERROR.        
        END.
     END.
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
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT "CONTAINER", OUTPUT W_Procmto).
  RUN Estado_Btn IN WIDGET-HANDLE(W_Procmto) (TRUE).

  IF AVAILABLE(Pro_Especiales) THEN DO:
     ASSIGN W_CodProE = Pro_Especiales.Cod_Producto.
     
     FIND Agencias WHERE Agencias.Agencia EQ Pro_Especiales.Agencia NO-LOCK NO-ERROR.
     IF AVAILABLE(Agencias) THEN 
        ASSIGN W_NomOfi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = Agencias.Nombre.

     FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Especiales.Cta_Cargos NO-LOCK NO-ERROR.
     IF AVAILABLE(Cuentas) THEN
        ASSIGN W_NomCar:SCREEN-VALUE = Cuentas.Nombre.
        
     FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Especiales.Cta_Recaudos NO-LOCK NO-ERROR.
     IF AVAILABLE(Cuentas) THEN
        ASSIGN W_NomReca:SCREEN-VALUE = Cuentas.Nombre.

     FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Especiales.Cta_SucyAge NO-LOCK NO-ERROR.
     IF AVAILABLE(Cuentas) THEN
        ASSIGN W_NomSucAge:SCREEN-VALUE = Cuentas.Nombre.

     IF Pro_Especiales.Id_Contabiliza:SCREEN-VALUE EQ "Si" THEN
        ASSIGN Pro_Especiales.Cta_Cargos:SENSITIVE    = TRUE
               W_NomCar:VISIBLE                       = TRUE.
     ELSE
        ASSIGN Pro_Especiales.Cta_Cargos:SENSITIVE    = FALSE
               Pro_Especiales.Cta_Cargos:SCREEN-VALUE = ""
               W_NomCar:SCREEN-VALUE                  = ""
               W_NomCar:VISIBLE                       = FALSE.
               
     FIND Clientes WHERE Clientes.Nit EQ Pro_Especiales.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN
        ASSIGN W_NomTercero:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
            TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
     ELSE
        ASSIGN W_NomTercero:SCREEN-VALUE                        = "".               
               
  END.
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
  {src/adm/template/snd-list.i "PRO_ESPECIALES"}

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

