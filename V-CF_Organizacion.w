&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.
/* Local Variable Definitions ---                                       */
 DEFINE VARIABLE W_liscencosto AS LOGICAL.
 DEFINE VARIABLE W_cencostos   AS INTEGER FORMAT "999" INITIAL 0.
 DEFINE VARIABLE Stat          AS INTEGER.
 DEFINE VARIABLE W_Dirprogra   AS CHAR FORMAT "X(60)".
 DEFINE VARIABLE W_Dirspl      AS CHAR FORMAT "X(60)".
 DEFINE VARIABLE W_Tamano      AS INTEGER.
 {INCLUIDO\VARIABLE.I "shared"}

DEFINE VAR W_Creando AS LOGICAL INITIAL NO.
     DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
     DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
     DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
     DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
     DEFINE VARIABLE P_TipDoc   LIKE Clientes.Tipo_Identificacion.

/*para indicadores*/
    DEFINE VAR P_CodInd  LIKE Indicadores.Indicador.
    DEFINE VAR P_NomInd  LIKE Indicadores.Nombre.
    DEFINE VAR P_ValTas  LIKE Indicadores.Tasa.
    DEFINE VAR P_Valor   LIKE Indicadores.Valor.

   DEFINE VAR W_RowidCbt AS ROWID.

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
&Scoped-define EXTERNAL-TABLES Entidad
&Scoped-define FIRST-EXTERNAL-TABLE Entidad


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Entidad.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Entidad.Nit Entidad.Nit_RepLegal ~
Entidad.Direccion Entidad.Telefono Entidad.email Entidad.Fax ~
Entidad.Dir_Spl Entidad.Id_CenCosto Entidad.Mensaje ~
Entidad.Cod_EntidadControl Entidad.Clave_EntControl Entidad.Cod_Datacredito ~
Entidad.Clave_Datacredito Entidad.Cod_Cifin Entidad.Clave_Cifin ~
Entidad.Nit_RevFiscal Entidad.TarProf_RevFiscal Entidad.Nit_EntidadRevisora ~
Entidad.Nit_Contador Entidad.TarProf_Contador Entidad.Nit_RepLegal_Suplente ~
Entidad.MaxOp_Efectivo_Dia Entidad.MaxOp_Efectivo_Mes ~
Entidad.ValMax_ConcentracionCre Entidad.ValMax_FondoLiquidez ~
Entidad.ValMax_Inversiones Entidad.Cpte_RecNom Entidad.Cpte_NomEmp 
&Scoped-define ENABLED-TABLES Entidad
&Scoped-define FIRST-ENABLED-TABLE Entidad
&Scoped-Define ENABLED-OBJECTS BUTTON-167 BUTTON-163 BUTTON-164 ~
Btn_Ubicacion RECT-123 RECT-124 RECT-126 RECT-128 
&Scoped-Define DISPLAYED-FIELDS Entidad.Entidad Entidad.Nit ~
Entidad.Nit_RepLegal Entidad.Direccion Entidad.Telefono Entidad.email ~
Entidad.Fax Entidad.Dir_Spl Entidad.Id_CenCosto Entidad.Mensaje ~
Entidad.Cod_EntidadControl Entidad.Clave_EntControl Entidad.Nombre ~
Entidad.Cod_Datacredito Entidad.Clave_Datacredito Entidad.Cod_Cifin ~
Entidad.Clave_Cifin Entidad.Nit_RevFiscal Entidad.TarProf_RevFiscal ~
Entidad.Nit_EntidadRevisora Entidad.Nit_Contador Entidad.TarProf_Contador ~
Entidad.Nit_RepLegal_Suplente Entidad.Ubicacion Entidad.Fec_Creacion ~
Entidad.Fec_Retiro Entidad.MaxOp_Efectivo_Dia Entidad.MaxOp_Efectivo_Mes ~
Entidad.ValMax_ConcentracionCre Entidad.ValMax_FondoLiquidez ~
Entidad.ValMax_Inversiones Entidad.Cpte_RecNom Entidad.Cpte_NomEmp 
&Scoped-define DISPLAYED-TABLES Entidad
&Scoped-define FIRST-DISPLAYED-TABLE Entidad
&Scoped-Define DISPLAYED-OBJECTS NomCbt W_GMF Sal_Minimo Usura W_Estado ~
W_NomUbicacion W_NomGer Nom_Revisor Nom_Contador Nom_Suplente ~
Nom_EntRevisora NomCbtEmp 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Entidad.Entidad Entidad.Nit 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ubicacion 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 117" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-163 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 163" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-164 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 164" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-167 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 167" 
     SIZE 3 BY .54.

DEFINE VARIABLE NomCbt AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCbtEmp AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Contador AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_EntRevisora AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Revisor AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Suplente AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Sal_Minimo AS CHARACTER FORMAT "X(50)":U 
     LABEL "S.M.L.V" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Usura AS CHARACTER FORMAT "X(50)":U 
     LABEL "Usura" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_GMF AS CHARACTER FORMAT "X(256)":U 
     LABEL "G.M.F" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomGer AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomUbicacion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ubicación" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Estado AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activa", 1,
"Inactiva", 2
     SIZE 19 BY 1.08
     BGCOLOR 17 FONT 5.

DEFINE RECTANGLE RECT-123
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY 6.46.

DEFINE RECTANGLE RECT-124
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47 BY 10.23.

DEFINE RECTANGLE RECT-126
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY 3.5.

DEFINE RECTANGLE RECT-128
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     NomCbt AT ROW 5.31 COL 23.14 COLON-ALIGNED NO-LABEL
     BUTTON-167 AT ROW 16.08 COL 46
     W_GMF AT ROW 16.08 COL 12 COLON-ALIGNED
     BUTTON-163 AT ROW 14.19 COL 46
     BUTTON-164 AT ROW 15.08 COL 46
     Sal_Minimo AT ROW 14.19 COL 12 COLON-ALIGNED
     Usura AT ROW 15.12 COL 12 COLON-ALIGNED
     Entidad.Entidad AT ROW 1.08 COL 9 COLON-ALIGNED
          LABEL "Organización"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 FGCOLOR 7 
     Entidad.Nit AT ROW 1.96 COL 9.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 FGCOLOR 7 
     Entidad.Nit_RepLegal AT ROW 2.88 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     W_Estado AT ROW 1 COL 25 HELP
          "Seleccione el Estado para la Entidad" NO-LABEL
     Entidad.Direccion AT ROW 7.46 COL 12 COLON-ALIGNED
          LABEL "Dirección"
          VIEW-AS FILL-IN 
          SIZE 32 BY .81
          BGCOLOR 15 
     Entidad.Telefono AT ROW 8.38 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY .81
          BGCOLOR 15 
     Entidad.email AT ROW 9.31 COL 12 COLON-ALIGNED
          LABEL "e-mail"
          VIEW-AS FILL-IN 
          SIZE 32.29 BY .81
          BGCOLOR 15 
     Entidad.Fax AT ROW 10.23 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY .81
          BGCOLOR 15 
     Btn_Ubicacion AT ROW 11.15 COL 46
     W_NomUbicacion AT ROW 11.15 COL 12 COLON-ALIGNED
     Entidad.Dir_Spl AT ROW 12.04 COL 12 COLON-ALIGNED
          LABEL "Dir. Informes"
          VIEW-AS FILL-IN 
          SIZE 32 BY .81
          BGCOLOR 15 
     Entidad.Id_CenCosto AT ROW 13.12 COL 14
          LABEL "Maneja Centros de Costos?"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .77
     Entidad.Mensaje AT ROW 18.23 COL 3 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 47 BY 2.96
          BGCOLOR 15 
     Entidad.Cod_EntidadControl AT ROW 5.58 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Entidad.Clave_EntControl AT ROW 5.58 COL 83 COLON-ALIGNED
          LABEL "Clave"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Entidad.Nombre AT ROW 1.96 COL 23.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Entidad.Cod_Datacredito AT ROW 6.46 COL 63 COLON-ALIGNED
          LABEL "Datacredito"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Entidad.Clave_Datacredito AT ROW 6.46 COL 83 COLON-ALIGNED
          LABEL "Clave"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Entidad.Cod_Cifin AT ROW 7.35 COL 63 COLON-ALIGNED
          LABEL "CIFIN"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Entidad.Clave_Cifin AT ROW 7.38 COL 83 COLON-ALIGNED
          LABEL "Clave"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Entidad.Nit_RevFiscal AT ROW 8.27 COL 63 COLON-ALIGNED
          LABEL "Revisor Fiscal"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     Entidad.TarProf_RevFiscal AT ROW 8.27 COL 83 COLON-ALIGNED
          LABEL "T.Profesional"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Entidad.Nit_EntidadRevisora AT ROW 10.31 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Entidad.Nit_Contador AT ROW 12.62 COL 62 COLON-ALIGNED
          LABEL "Contador"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Entidad.TarProf_Contador AT ROW 12.58 COL 84 COLON-ALIGNED
          LABEL "T.Profesional"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Entidad.Nit_RepLegal_Suplente AT ROW 14.46 COL 62 COLON-ALIGNED
          LABEL "Rep. Suplente"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     W_NomGer AT ROW 2.88 COL 23.14 COLON-ALIGNED NO-LABEL
     Entidad.Ubicacion AT ROW 13.12 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.14 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Nom_Revisor AT ROW 9.23 COL 63 COLON-ALIGNED NO-LABEL
     Nom_Contador AT ROW 13.54 COL 62 COLON-ALIGNED NO-LABEL
     Entidad.Fec_Creacion AT ROW 1.27 COL 66 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25 BY .85
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Entidad.Fec_Retiro AT ROW 2.35 COL 66 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25 BY .85
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Nom_Suplente AT ROW 14.46 COL 73 COLON-ALIGNED NO-LABEL
     Nom_EntRevisora AT ROW 10.31 COL 74 COLON-ALIGNED NO-LABEL
     Entidad.MaxOp_Efectivo_Dia AT ROW 16.62 COL 78 COLON-ALIGNED
          LABEL "Máximo Operaciones en Efectivo (Día)"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Entidad.MaxOp_Efectivo_Mes AT ROW 17.5 COL 78 COLON-ALIGNED
          LABEL "Máximo Operaciones en Efectivo (Mes)"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Entidad.ValMax_ConcentracionCre AT ROW 18.38 COL 78 COLON-ALIGNED
          LABEL "Máximo Concentración Crédito"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Entidad.ValMax_FondoLiquidez AT ROW 19.31 COL 78 COLON-ALIGNED
          LABEL "Máximo Fondo Liquidez"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Entidad.ValMax_Inversiones AT ROW 20.19 COL 78 COLON-ALIGNED
          LABEL "Máximo de Inversión"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Entidad.Cpte_RecNom AT ROW 5.23 COL 19.14 COLON-ALIGNED
          LABEL "Comprobante Recaudo"
          VIEW-AS FILL-IN 
          SIZE 2.86 BY .81
          BGCOLOR 15 
     Entidad.Cpte_NomEmp AT ROW 4.38 COL 19.14 COLON-ALIGNED
          LABEL "Cbte Nomina Empleados"
          VIEW-AS FILL-IN 
          SIZE 2.86 BY .81
          BGCOLOR 15 
     NomCbtEmp AT ROW 4.38 COL 23.14 COLON-ALIGNED NO-LABEL
     " Control y Revisoria" VIEW-AS TEXT
          SIZE 17 BY 1.04 AT ROW 4.5 COL 53
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     " Información General" VIEW-AS TEXT
          SIZE 19 BY 1.08 AT ROW 6.31 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Observaciones" VIEW-AS TEXT
          SIZE 15 BY 1.08 AT ROW 17.15 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Controles" VIEW-AS TEXT
          SIZE 9 BY 1.04 AT ROW 15.54 COL 53
          FGCOLOR 7 FONT 5
     " Datos para para Informes" VIEW-AS TEXT
          SIZE 31 BY 1.08 AT ROW 11.5 COL 53
          FGCOLOR 7 FONT 5
     RECT-123 AT ROW 5.04 COL 52
     RECT-124 AT ROW 6.92 COL 3
     RECT-126 AT ROW 12.04 COL 52
     RECT-128 AT ROW 16.08 COL 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Entidad
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
         HEIGHT             = 20.54
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Entidad.Clave_Cifin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Clave_Datacredito IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Clave_EntControl IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Cod_Cifin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Cod_Datacredito IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Cpte_NomEmp IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Cpte_RecNom IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Direccion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Dir_Spl IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.email IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Entidad IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Entidad.Fec_Creacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Entidad.Fec_Retiro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Entidad.Id_CenCosto IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.MaxOp_Efectivo_Dia IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.MaxOp_Efectivo_Mes IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Nit IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN Entidad.Nit_Contador IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Nit_RepLegal_Suplente IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Nit_RevFiscal IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Nombre IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN NomCbt IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCbtEmp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Contador IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_EntRevisora IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Revisor IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Suplente IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Sal_Minimo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Entidad.TarProf_Contador IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.TarProf_RevFiscal IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.Ubicacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Usura IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Entidad.ValMax_ConcentracionCre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.ValMax_FondoLiquidez IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Entidad.ValMax_Inversiones IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET W_Estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_GMF IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomGer IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomUbicacion IN FRAME F-Main
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

&Scoped-define SELF-NAME Btn_Ubicacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ubicacion V-table-Win
ON CHOOSE OF Btn_Ubicacion IN FRAME F-Main /* Button 117 */
DO:
  DEFINE VAR P_Ubi LIKE Ubicacion.Ubicacion.
  DEFINE VAR P_NUbi AS CHARACTER FORMAT "X(80)".
  RUN C-Ubicacion.r (OUTPUT P_Ubi, OUTPUT P_NUbi).
  ASSIGN W_NomUbicacion:SCREEN-VALUE = LC(P_NUbi)
         Entidad.Ubicacion:SCREEN-VALUE = P_Ubi.
  APPLY 'entry' TO Entidad.DIR_Spl IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-163
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-163 V-table-Win
ON CHOOSE OF BUTTON-163 IN FRAME F-Main /* Button 163 */
DO:
  RUN C-Indicadores.r (OUTPUT P_CodInd, OUTPUT P_NomInd,
                       OUTPUT P_ValTas, OUTPUT P_Valor).
  Sal_Minimo:SCREEN-VALUE = STRING(P_CodInd,"99999") + " - " + P_NomInd + " : " + STRING(P_Valor,">>,>>>,>>9").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-164
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-164 V-table-Win
ON CHOOSE OF BUTTON-164 IN FRAME F-Main /* Button 164 */
DO:
  RUN C-Indicadores.r (OUTPUT P_CodInd, OUTPUT P_NomInd,
                       OUTPUT P_ValTas, OUTPUT P_Valor).
  Usura:SCREEN-VALUE = STRING(P_CodInd,"99999") + " - " + P_NomInd + " : " + STRING(P_ValTas).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-167
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-167 V-table-Win
ON CHOOSE OF BUTTON-167 IN FRAME F-Main /* Button 167 */
DO:
  DEFINE VAR W_CodDed LIKE Deducible.Cod_Deducible.
  DEFINE VAR W_NomDed LIKE Deducible.Nom_Deducible.
  RUN C-Deducibles.r (OUTPUT W_CodDed, OUTPUT W_NomDed).
  IF W_CodDed NE "" THEN
     W_GMF:SCREEN-VALUE = STRING(W_CodDed,"X(4)") + " - " + W_NomDed.
  ELSE W_GMF:SCREEN-VALUE = "0000 - No Escogido".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Entidad.Cpte_NomEmp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Entidad.Cpte_NomEmp V-table-Win
ON LEAVE OF Entidad.Cpte_NomEmp IN FRAME F-Main /* Cbte Nomina Empleados */
DO:
  FIND FIRST Comprobantes WHERE Comprobantes.Comprobante EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Comprobantes THEN
     NomCbtEmp:SCREEN-VALUE = Comprobantes.Nombre.
  ELSE DO:
     RUN C-Comprobantes.r (INPUT-OUTPUT W_RowidCbt).
     FIND Comprobantes WHERE ROWID(Comprobantes) EQ W_RowidCbt NO-LOCK NO-ERROR.
     IF AVAILABLE Comprobantes THEN
        ASSIGN SELF:SCREEN-VALUE = STRING(Comprobantes.Comprobante) 
               NomCbtEmp:SCREEN-VALUE = Comprobantes.Nombre.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Entidad.Cpte_RecNom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Entidad.Cpte_RecNom V-table-Win
ON LEAVE OF Entidad.Cpte_RecNom IN FRAME F-Main /* Comprobante Recaudo */
DO:
  FIND FIRST Comprobantes WHERE Comprobantes.Comprobante EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Comprobantes THEN
     NomCbt:SCREEN-VALUE = Comprobantes.Nombre.
  ELSE DO:
     RUN C-Comprobantes.r (INPUT-OUTPUT W_RowidCbt).
     FIND Comprobantes WHERE ROWID(Comprobantes) EQ W_RowidCbt NO-LOCK NO-ERROR.
     IF AVAILABLE Comprobantes THEN
        ASSIGN SELF:SCREEN-VALUE = STRING(Comprobantes.Comprobante) 
               NomCbt:SCREEN-VALUE = Comprobantes.Nombre.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Entidad.Entidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Entidad.Entidad V-table-Win
ON LEAVE OF Entidad.Entidad IN FRAME F-Main /* Organización */
DO:
  FIND Entidad WHERE Entidad.Entidad EQ INPUT Entidad.Entidad NO-ERROR.
  IF AVAILABLE(Entidad) THEN DO:
     RUN MostrarMensaje IN W_Manija (iNPUT 33, OUTPUT W_Eleccion).
     ASSIGN Entidad.Entidad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(0).
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Entidad.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Entidad.Nit V-table-Win
ON LEAVE OF Entidad.Nit IN FRAME F-Main /* Nit */
DO:
  FIND Clientes WHERE Clientes.Nit EQ INPUT Entidad.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE (Clientes) THEN
     ASSIGN Entidad.Nombre:SCREEN-VALUE IN FRAME F-Main = Clientes.Nombre.
  ELSE
  DO:
    RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
    ASSIGN Entidad.Nombre:SCREEN-VALUE IN FRAME {&FRAME-NAME} = P_Nombre
           Entidad.Nit:SCREEN-VALUE    IN FRAME {&FRAME-NAME} = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion NE "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Jurídica" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Entidad.Nit_Contador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Entidad.Nit_Contador V-table-Win
ON LEAVE OF Entidad.Nit_Contador IN FRAME F-Main /* Contador */
DO:
DO WITH FRAME {&FRAME-NAME}:
     FIND Clientes WHERE Clientes.Nit EQ Entidad.Nit_Contador:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Nom_Contador:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        ASSIGN Nom_Contador:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
               Entidad.Nit_Contador:SCREEN-VALUE = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion EQ "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Natural" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Entidad.Nit_EntidadRevisora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Entidad.Nit_EntidadRevisora V-table-Win
ON LEAVE OF Entidad.Nit_EntidadRevisora IN FRAME F-Main /* Entidad Fiscal */
DO:
DO WITH FRAME {&FRAME-NAME}:
     FIND Clientes WHERE Clientes.Nit EQ Entidad.Nit_EntidadRevisora:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Nom_Entrevisora:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        ASSIGN Nom_EntRevisora:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
               Entidad.Nit_EntidadRevisora:SCREEN-VALUE = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion NE "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Juridica" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Entidad.Nit_RepLegal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Entidad.Nit_RepLegal V-table-Win
ON LEAVE OF Entidad.Nit_RepLegal IN FRAME F-Main /* Gerente */
DO:

IF NOT W_Creando THEN DO:
  FIND Clientes WHERE Clientes.Nit EQ Entidad.Nit_RepLegal:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN
     W_NomGer:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  ELSE DO:
     RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
     ASSIGN W_NomGer:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
            Entidad.Nit_RepLegal:SCREEN-VALUE = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion EQ "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Natural" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Entidad.Nit_RepLegal_Suplente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Entidad.Nit_RepLegal_Suplente V-table-Win
ON LEAVE OF Entidad.Nit_RepLegal_Suplente IN FRAME F-Main /* Rep. Suplente */
DO:
DO WITH FRAME {&FRAME-NAME}:
  FIND Clientes WHERE Clientes.Nit EQ Entidad.Nit_RepLegal_Suplente:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN
     Nom_Suplente:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  ELSE DO:
     RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
     ASSIGN Nom_Suplente:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
            Entidad.Nit_RepLegal_Suplente:SCREEN-VALUE = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion EQ "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Natural" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Entidad.Nit_RevFiscal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Entidad.Nit_RevFiscal V-table-Win
ON LEAVE OF Entidad.Nit_RevFiscal IN FRAME F-Main /* Revisor Fiscal */
DO:
DO WITH FRAME {&FRAME-NAME}:
     FIND Clientes WHERE Clientes.Nit EQ Entidad.Nit_RevFiscal:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) AND Entidad.Nit_RevFiscal:SCREEN-VALUE NE "" THEN
        Nom_Revisor:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
          RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
          ASSIGN Nom_Revisor:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
               Entidad.Nit_RevFiscal:SCREEN-VALUE = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion EQ "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Natural" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Estado V-table-Win
ON MOUSE-SELECT-CLICK OF W_Estado IN FRAME F-Main
DO:
  APPLY "VALUE-CHANGED":U TO W_Estado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Estado V-table-Win
ON VALUE-CHANGED OF W_Estado IN FRAME F-Main
OR RETURN OF W_Estado DO:
  ASSIGN W_Estado.
  IF INTEGER (W_Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME}) EQ 2 THEN
     DISPLAY TODAY @ Entidad.Fec_Retiro WITH FRAME {&FRAME-NAME}.
  ELSE
     DISPLAY "" @ Entidad.Fec_Retiro WITH FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "Entidad"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Entidad"}

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
  W_Creando = YES.
  /* Dispatch standard ADM method.                             */
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .


  FIND LAST Entidad NO-LOCK NO-ERROR.
  IF AVAILABLE(Entidad) THEN
     Entidad.Entidad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Entidad.Entidad + 1).
  ELSE
     Entidad.Entidad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN W_Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(1)
         W_Estado    = 1.
  DISABLE Entidad.Entidad WITH FRAME {&FRAME-NAME}.
  DISPLAY TODAY @ Entidad.Fec_Creacion WITH FRAME {&FRAME-NAME}.
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
   IF Entidad.Fec_Retiro:SCREEN-VALUE NE "" OR W_estado:SCREEN-VALUE EQ "2" THEN DO:
      MESSAGE "La Organizacion ya se encuentra inactiva" SKIP
              "no se pueden salvar los cambios realizados" VIEW-AS ALERT-BOX ERROR.
      RUN local-display-fields.
      RETURN ERROR.
   END.

   DEFINE VAR W_Rpta AS LOGICAL.
   DEFINE VAR W_Evento AS CHAR INITIAL " MODIFICAR ". 
   IF INTEGER(Entidad.Entidad:SCREEN-VALUE) = 0 OR
             (Entidad.Nit:SCREEN-VALUE) = "" OR 
             (Entidad.Nit_RepLegal:SCREEN-VALUE) = "" THEN
    DO:
      MESSAGE "Los siguientes campos son necesarios para poder grabar el registro:" SKIP
              "- CODIGO DE LA ORGANIZACION" SKIP
              "- NIT DE LA ORGANIZACION" SKIP
              "- NIT DEL REPRESENTANTE LEGAL" SKIP(1)
              "Rectifique la información de estos campos!!!" VIEW-AS ALERT-BOX ERROR.

      RUN notify ('cancel-record':U).
      RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
      RETURN ERROR.
    END.     
   ASSIGN W_Dirspl    = Entidad.Dir_Spl:SCREEN-VALUE IN FRAME {&FRAME-NAME}
          STAT        = 1.
   IF TRIM(W_Dirspl) <> "" THEN
    DO WITH FRAME {&FRAME-NAME}:
       OS-CREATE-DIR VALUE(W_Dirspl).
       STAT = OS-ERROR.
       IF STAT <> 0 THEN
        DO:
          RUN MostrarMensaje IN W_Manija (INPUT 264,OUTPUT W_Rpta).
          APPLY "ENTRY" TO Entidad.dir_Spl.
          RETURN ERROR.
        END.
    END.   
   ELSE
     DO:
       RUN MostrarMensaje IN W_Manija (INPUT 263,OUTPUT W_Rpta).
       APPLY "ENTRY" TO Entidad.dir_Spl IN FRAME {&FRAME-NAME}.
       RETURN ERROR.
     END.
  IF Entidad.Nit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " " OR 
     Entidad.Entidad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " "THEN
    DO:
      RUN MostrarMensaje IN W_Manija (INPUT 289, OUTPUT W_Rpta).
      APPLY "ENTRY" TO Entidad.Nit.
      RETURN ERROR.
    END.
  IF STAT = 0 THEN
   DO:
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).
     ASSIGN Entidad.Ubicacion   = Entidad.Ubicacion:SCREEN-VALUE.  
     RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
     IF RETURN-VALUE = "YES" THEN
       RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Entidad. Entidad : " + STRING(Entidad.Entidad)).
     ELSE
       RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Entidad. Entidad : " + STRING(Entidad.Entidad)).
   END.
   ASSIGN Entidad.Nombre = Entidad.Nombre:SCREEN-VALUE
          Entidad.Ind_SMLV = INTEGER(SUBSTRING(Sal_Minimo:SCREEN-VALUE,1,5))
          Entidad.Ind_Usura = INTEGER(SUBSTRING(Usura:SCREEN-VALUE,1,5))
          Entidad.Deduc_GMF = SUBSTRING(W_GMF:SCREEN-VALUE,1,4).

END.
W_Creando = NO.
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
  W_Creando = NO.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   IF Entidad.Entidad > 0 THEN
    DO:    
      FIND FIRST Agencias WHERE Agencia.Entidad = Entidad.Entidad AND 
                                Agencia.Estado = 1 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(Agencias) THEN
       DO:
        RUN MostrarMensaje IN W_Manija (iNPUT 25, OUTPUT W_Eleccion).
        IF W_Eleccion THEN
         DO:
           FIND CURRENT Entidad NO-ERROR.
           IF AVAILABLE(Entidad) THEN DO:
             ASSIGN Entidad.Estado     = 2
                     Entidad.Fec_Retiro = TODAY
                     Entidad.Fec_Retiro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
                     W_Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "2".
             RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Entidad. Entidad : " + STRING(Entidad.Entidad)).
           END.
           FIND CURRENT Entidad NO-LOCK NO-ERROR.
         END.
       END.
      ELSE
        RUN MostrarMensaje IN W_Manija (iNPUT 261, OUTPUT W_Eleccion).
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
  RUN check-modified IN THIS-PROCEDURE (INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  IF AVAILABLE (Entidad) THEN
   DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_Estado                                       = Entidad.Estado
            W_Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(Entidad.Estado).
     FIND Clientes WHERE Clientes.nit EQ Entidad.Nit_RepLegal NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        W_NomGer:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
     ELSE
        W_NomGer:SCREEN-VALUE = "No Existe en Clientes".
     FIND Clientes WHERE Clientes.Nit EQ Entidad.Nit NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Entidad.Nombre:SCREEN-VALUE = TRIM(Clientes.Nombre).
     ELSE
        Entidad.Nombre:SCREEN-VALUE = "No Existe en Clientes".

     DEFINE VARIABLE W_NUbicacion AS CHARACTER FORMAT "X(50)".

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Entidad.Ubicacion NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Entidad.Ubicacion,1,5) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Entidad.Ubicacion,1,2) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.

     W_NomUbicacion:SCREEN-VALUE = LC(W_NUbicacion).

     FIND FIRST Comprobantes WHERE Comprobantes.Comprobante EQ Entidad.Cpte_RecNom NO-LOCK NO-ERROR.
     IF AVAILABLE Comprobantes THEN
        ASSIGN NomCbt:SCREEN-VALUE = Comprobantes.Nombre.

     FIND FIRST Comprobantes WHERE Comprobantes.Comprobante EQ Entidad.Cpte_NomEmp NO-LOCK NO-ERROR.
     IF AVAILABLE Comprobantes THEN
        ASSIGN NomCbtEmp:SCREEN-VALUE = Comprobantes.Nombre.

     FIND Clientes WHERE Clientes.Nit EQ Entidad.Nit_RevFiscal NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Nom_Revisor:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE
        Nom_Revisor:SCREEN-VALUE = "Cliente No Existe".

     FIND Clientes WHERE Clientes.Nit EQ Entidad.Nit_EntidadRevisora NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Nom_EntRevisora:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE
        Nom_EntRevisora:SCREEN-VALUE = "Cliente No Existe".
     
     FIND Clientes WHERE Clientes.Nit EQ Entidad.Nit_RepLegal_Suplente NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Nom_Suplente:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE
        Nom_Suplente:SCREEN-VALUE = "Cliente No Existe".
     
     FIND Clientes WHERE Clientes.Nit EQ Entidad.Nit_Contador NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Nom_Contador:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE
        Nom_Contador:SCREEN-VALUE = "Cliente No Existe".
        
     FIND Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_SMLV NO-LOCK NO-ERROR.
     IF AVAILABLE Indicadores THEN
        Sal_Minimo:SCREEN-VALUE = STRING(Indicadores.Indicador,"99999") + " - " + Indicadores.Nombre + " : " + STRING(Indicadores.Valor,">>,>>>,>>9").
     ELSE Sal_Minimo:SCREEN-VALUE = "00000 - No Asignado".
     
     FIND Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_Usura AND 
          Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
     IF AVAILABLE Indicadores THEN
        Usura:SCREEN-VALUE = STRING(Indicadores.Indicador,"99999") + " - " + Indicadores.Nombre + " : " + STRING(Indicadores.Tasa).
     ELSE Usura:SCREEN-VALUE = "00000 - No Asignado".
     
     FIND Deducible WHERE Deducible.Cod_Deducible EQ Entidad.Deduc_GMF AND
          Deducible.Estado EQ 1 NO-LOCK NO-ERROR.
     W_GMF:SCREEN-VALUE = "".
     IF AVAILABLE Deducible THEN
        W_GMF:SCREEN-VALUE = STRING(Deducible.Cod_Deducible,"X(4)") + " - " + Deducible.Nom_Deducible. 
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
   DEFINE VAR OK AS LOGICAL.
/*   FOR EACH Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Estado EQ 1
                      NO-LOCK BY Ubicacion.Nombre:
      OK = W_ComCiudad:ADD-LAST(Ubicacion.Ubicacion + " - " + Ubicacion.Nombre) IN FRAME F-Main.
   END.
   FIND Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion EQ Entidad.ciudad NO-LOCK NO-ERROR.
   IF AVAILABLE(Ubicacion) THEN
       W_ComCiudad:SCREEN-VALUE IN FRAME F-Main = Ubicacion.Ubicacion + "-" + Ubicacion.Nombre.*/
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
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .
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
  {src/adm/template/snd-list.i "Entidad"}

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

