&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{Incluido\VARIABLE.I "SHARED"}.

DEFINE VARIABLE W_Rpta AS LOGICAL.
DEFINE VARIABLE W_TipPro LIKE Pro_Ahorros.Cod_Ahorro.
DEFINE VARIABLE W-Cod LIKE Formatos.Cod_Formato.
DEFINE VARIABLE W_CtrBalance AS LOGICAL.
DEFINE VARIABLE P_Nit LIKE Clientes.Nit.
DEFINE VARIABLE p_Nombre LIKE Clientes.Nombre.
DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
DEFINE VARIABLE P_AgeCli LIKE Clientes.Agencia.
DEFINE VARIABLE W_Puntero AS ROWID.

DEFINE VAR W_Activar AS LOGICAL INITIAL NO.
DEFINE VAR W_Nuevo AS LOGICAL INITIAL NO.

DEFINE VAR p_cuenta AS CHARACTER.

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
&Scoped-define EXTERNAL-TABLES Cuentas
&Scoped-define FIRST-EXTERNAL-TABLE Cuentas


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Cuentas.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Cuentas.Nombre Cuentas.Id_Cuenta ~
Cuentas.Naturaleza Cuentas.Tipo Cuentas.Estado Cuentas.Id_Presupuesto ~
Cuentas.Id_Base Cuentas.Id_Enlace Cuentas.Cam_Posicion Cuentas.Id_Caja ~
Cuentas.Cod_Caja Cuentas.Id_NoMvto Cuentas.Ctr_Naturaleza Cuentas.Id_Nit ~
Cuentas.Id_Doc Cuentas.Id_CenCostos Cuentas.Id_detalle ~
Cuentas.Id_Excendentes Cuentas.Cod_FlujoEfec Cuentas.id_nodlle ~
Cuentas.Car_Efectivo Cuentas.Cam_Captrabajo Cuentas.Id_FinAno ~
Cuentas.Id_Total Cuentas.Id_Renuncia 
&Scoped-define ENABLED-TABLES Cuentas
&Scoped-define FIRST-ENABLED-TABLE Cuentas
&Scoped-Define ENABLED-OBJECTS Cmb_Base Rs_IdProducto Cmb_Producto RECT-15 ~
RECT-16 RECT-17 RECT-18 RECT-19 RECT-20 
&Scoped-Define DISPLAYED-FIELDS Cuentas.Cuenta Cuentas.Nombre Cuentas.Nivel ~
Cuentas.Id_Cuenta Cuentas.Naturaleza Cuentas.Tipo Cuentas.Estado ~
Cuentas.Id_Presupuesto Cuentas.Id_Base Cuentas.Id_Enlace ~
Cuentas.Cam_Posicion Cuentas.Id_Caja Cuentas.Cod_Caja Cuentas.Id_NoMvto ~
Cuentas.Ctr_Naturaleza Cuentas.Id_Nit Cuentas.Id_Doc Cuentas.Id_CenCostos ~
Cuentas.Id_detalle Cuentas.Id_Excendentes Cuentas.Cod_FlujoEfec ~
Cuentas.id_nodlle Cuentas.Car_Efectivo Cuentas.Cam_Captrabajo ~
Cuentas.Fec_Creacion Cuentas.Fec_Retiro Cuentas.Id_FinAno Cuentas.Id_Total ~
Cuentas.Id_Renuncia 
&Scoped-define DISPLAYED-TABLES Cuentas
&Scoped-define FIRST-DISPLAYED-TABLE Cuentas
&Scoped-Define DISPLAYED-OBJECTS Cmb_Base Rs_IdProducto Cmb_Producto ~
W_Cformato W_FExtracto Rs_FExtracto 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Cuentas.Cuenta Cuentas.Estado 
&Scoped-define ADM-ASSIGN-FIELDS Cuentas.Estado Cuentas.Id_Presupuesto ~
Cuentas.Id_Caja Cuentas.Cod_Caja Cuentas.Car_Efectivo 
&Scoped-define List-3 Cuentas.Id_Presupuesto Cuentas.Id_Base ~
Cuentas.Id_Enlace Rs_IdProducto Cuentas.Cam_Posicion Cuentas.Id_Caja ~
Cuentas.Id_NoMvto Cuentas.Ctr_Naturaleza Cuentas.Id_Nit Cuentas.Id_Doc ~
Cuentas.Id_CenCostos Cuentas.Id_detalle Cuentas.Id_Excendentes ~
Cuentas.Cod_FlujoEfec Cuentas.id_nodlle Cuentas.Cam_Captrabajo ~
Cuentas.Id_FinAno Cuentas.Id_Total Cuentas.Id_Renuncia 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Activar 
     LABEL "Activar Cuenta Inactiva" 
     SIZE 27 BY .85.

DEFINE VARIABLE Cmb_Base AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Cmb_Presupuesto AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Cmb_Producto AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_Cformato AS CHARACTER FORMAT "X(40)":U 
     LABEL "Formato" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_FExtracto AS CHARACTER FORMAT "X(25)":U 
     LABEL "Extracto" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE WNomNit AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomNit-2 AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_FExtracto AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Extracto", 1,
"No-Ext.", 2
     SIZE 10 BY 1.19
     FONT 4 NO-UNDO.

DEFINE VARIABLE Rs_IdProducto AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ahorro", yes,
"Crédito", no
     SIZE 16.29 BY 1.04 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 4.31.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 4.31.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 4.85.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 3.77.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 1.88.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 4.23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Activar AT ROW 20.12 COL 2
     WNomNit AT ROW 13.23 COL 70.14 COLON-ALIGNED NO-LABEL
     Cuentas.Cuenta AT ROW 1 COL 13 COLON-ALIGNED FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 24 BY .81
          BGCOLOR 15 FONT 5
     Cuentas.Nombre AT ROW 1 COL 51 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY .81
          BGCOLOR 15 FONT 5
     Cuentas.Nivel AT ROW 1 COL 94 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Cuentas.Id_Cuenta AT ROW 3.15 COL 5 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Pasivo", 2,
"Patrimonio", 3,
"Resultado", 4,
"De Orden", 5
          SIZE 13 BY 3.5
          BGCOLOR 17 FONT 5
     Cuentas.Naturaleza AT ROW 3.04 COL 36 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Debito", "DB":U,
"Credito", "CR":U
          SIZE 10 BY 1.19
          BGCOLOR 17 FONT 5
     Cuentas.Tipo AT ROW 3.04 COL 58 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Mayor", 1,
"Movimiento", 2
          SIZE 14 BY 1.19
          BGCOLOR 17 FONT 5
     Cuentas.Estado AT ROW 3.04 COL 81 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 12 BY 1.19
          BGCOLOR 17 FONT 5
     Cuentas.Id_Presupuesto AT ROW 4.65 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY 1.08
          FONT 4
     Cmb_Presupuesto AT ROW 4.65 COL 55 COLON-ALIGNED NO-LABEL
     Cuentas.Id_Base AT ROW 5.46 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY 1.04
          FONT 4
     Cmb_Base AT ROW 5.54 COL 55 COLON-ALIGNED NO-LABEL
     Cuentas.Id_Enlace AT ROW 6.35 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1.04
          FONT 4
     Rs_IdProducto AT ROW 6.38 COL 57 NO-LABEL
     Cuentas.Cam_Posicion AT ROW 8 COL 5 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Utilidad", 1,
"Fuente", 2,
"Aplicaciones", 3,
"Ninguna", 4
          SIZE 17 BY 2.96
          BGCOLOR 17 FONT 5
     Cmb_Producto AT ROW 7.27 COL 57 NO-LABEL
     Cuentas.Id_Caja AT ROW 8.08 COL 35
          LABEL "Sin ser del portafolio se puede manejar por caja?"
          VIEW-AS TOGGLE-BOX
          SIZE 36 BY 1.08
     Cuentas.Cod_Caja AT ROW 8.08 COL 72 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ingreso", 1,
"Egreso", 2,
"Ambos", 3
          SIZE 25 BY 1.08
          BGCOLOR 17 FONT 4
     Cuentas.Id_NoMvto AT ROW 8.96 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .77
     Cuentas.Ctr_Naturaleza AT ROW 8.96 COL 72
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .77
     Cuentas.Id_Nit AT ROW 11.23 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 29 BY .54
     Cuentas.Id_Doc AT ROW 10.35 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .88
     Cuentas.Id_CenCostos AT ROW 9.62 COL 72
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .85
     Cuentas.Id_detalle AT ROW 9.65 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 30 BY .77
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     Cuentas.Id_Excendentes AT ROW 10.38 COL 72
          LABEL "Cuenta de Excedentes o perdida?"
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .77
     Cuentas.Cod_FlujoEfec AT ROW 12.31 COL 5 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Operación", "A":U,
"Inversión", "B":U,
"Financiación", "C":U,
"Efectivo", "D":U,
"Ninguno", "E":U
          SIZE 17 BY 3.5
          BGCOLOR 17 FONT 5
     Cuentas.id_nodlle AT ROW 11.81 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 32 BY .77
     Cuentas.Car_Efectivo AT ROW 14.5 COL 35 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Ninguno", 1,
"Caja", 2,
"Banco", 3
          SIZE 10 BY 2
          BGCOLOR 17 
     W_Cformato AT ROW 15.85 COL 51.86 COLON-ALIGNED
     W_FExtracto AT ROW 16.77 COL 51.86 COLON-ALIGNED
     Rs_FExtracto AT ROW 17.04 COL 35 NO-LABEL
     Cuentas.Cam_Captrabajo AT ROW 17.15 COL 5 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo Corriente", 1,
"Pasivo Corriente", 2,
"Ninguno", 3
          SIZE 19 BY 2.42
          BGCOLOR 17 FONT 5
     Cuentas.Fec_Creacion AT ROW 20.12 COL 56 COLON-ALIGNED
          LABEL "Creación"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Cuentas.Fec_Retiro AT ROW 20.12 COL 80 COLON-ALIGNED
          LABEL "Retiro"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Cuentas.Id_FinAno AT ROW 13.42 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .65
     Cuentas.Id_Total AT ROW 12.62 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .65
     Cuentas.Nit_FinAno AT ROW 13.23 COL 59 COLON-ALIGNED
          LABEL "Nit"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     Cuentas.Id_Renuncia AT ROW 11.15 COL 72
          LABEL "Cuenta para Renuncia de Clientes?"
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .77
     WNomNit-2 AT ROW 13.23 COL 70.14 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     "Naturaleza" VIEW-AS TEXT
          SIZE 10 BY 1.04 AT ROW 2.08 COL 36
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Tipo" VIEW-AS TEXT
          SIZE 6 BY 1.04 AT ROW 2.08 COL 58
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Estado" VIEW-AS TEXT
          SIZE 7 BY 1.08 AT ROW 2.08 COL 81
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Balance - Estado de Resultados" VIEW-AS TEXT
          SIZE 28 BY 1.08 AT ROW 2.08 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Cambio Posición Financiera" VIEW-AS TEXT
          SIZE 25 BY .77 AT ROW 7.19 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Flujo de Efectivo" VIEW-AS TEXT
          SIZE 15 BY .77 AT ROW 11.5 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Cambio en Capital de Trabajo" VIEW-AS TEXT
          SIZE 27 BY .81 AT ROW 16.35 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-15 AT ROW 2.62 COL 2
     RECT-16 AT ROW 6.92 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RECT-17 AT ROW 11.23 COL 2
     RECT-18 AT ROW 16.08 COL 2
     RECT-19 AT ROW 2.62 COL 32
     RECT-20 AT ROW 14.27 COL 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Cuentas
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
         HEIGHT             = 20.08
         WIDTH              = 99.72.
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

/* SETTINGS FOR BUTTON Btn_Activar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Cuentas.Cam_Captrabajo IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR RADIO-SET Cuentas.Cam_Posicion IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR RADIO-SET Cuentas.Car_Efectivo IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_Presupuesto IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_Presupuesto:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Producto IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR RADIO-SET Cuentas.Cod_Caja IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR RADIO-SET Cuentas.Cod_FlujoEfec IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Ctr_Naturaleza IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN Cuentas.Cuenta IN FRAME F-Main
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR RADIO-SET Cuentas.Estado IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR FILL-IN Cuentas.Fec_Creacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Cuentas.Fec_Retiro IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_Base IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_Caja IN FRAME F-Main
   2 3 EXP-LABEL                                                        */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_CenCostos IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_detalle IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_Doc IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_Enlace IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_Excendentes IN FRAME F-Main
   3 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_FinAno IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_Nit IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cuentas.id_nodlle IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_NoMvto IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_Presupuesto IN FRAME F-Main
   2 3                                                                  */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_Renuncia IN FRAME F-Main
   3 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Cuentas.Id_Total IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN Cuentas.Nit_FinAno IN FRAME F-Main
   NO-DISPLAY NO-ENABLE EXP-LABEL                                       */
ASSIGN 
       Cuentas.Nit_FinAno:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN Cuentas.Nivel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Rs_FExtracto IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Rs_FExtracto:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RADIO-SET Rs_IdProducto IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN WNomNit IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       WNomNit:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN WNomNit-2 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       WNomNit-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR COMBO-BOX W_Cformato IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX W_FExtracto IN FRAME F-Main
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON GO OF FRAME F-Main
DO:
    ENABLE Cuentas.Estado WITH FRAME {&FRAME-NAME}.
    ASSIGN Cuentas.Estado:SCREEN-VALUE = "1".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Activar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Activar V-table-Win
ON CHOOSE OF Btn_Activar IN FRAME F-Main /* Activar Cuenta Inactiva */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cuentas.Cuenta:SCREEN-VALUE NO-ERROR.
    IF AVAILABLE Cuentas THEN
        ASSIGN Cuentas.Fec_Retiro = ?
               Cuentas.Estado = 1.

    RUN Local-Display-Fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Car_Efectivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Car_Efectivo V-table-Win
ON VALUE-CHANGED OF Cuentas.Car_Efectivo IN FRAME F-Main /* Caracteristica */
OR RETURN OF cuentas.Car_Efectivo OR LEAVE OF cuentas.Car_Efectivo OR 
   MOUSE-SELECT-CLICK OF cuentas.Car_Efectivo DO:
   DO WITH FRAME {&FRAME-NAME}:
      IF Cuentas.Car_Efectivo:SCREEN-VALUE EQ "2" THEN
         ASSIGN W_CFormato:VISIBLE       = FALSE
                W_CFormato:SCREEN-VALUE  = ""
                W_FExtracto:VISIBLE      = FALSE
                W_FExtracto:SCREEN-VALUE = ""
                Rs_FExtracto:VISIBLE     = FALSE.
      ELSE DO:  
         IF Cuentas.Car_Efectivo:SCREEN-VALUE = "1" THEN DO:
            IF Cuentas.Cod_FlujoEfec:SCREEN-VALUE EQ "D" THEN DO:
               ASSIGN W_CFormato:SENSITIVE     = FALSE
                      W_CFormato:VISIBLE       = FALSE
                      W_CFormato:SCREEN-VALUE  = ""
                      Rs_FExtracto:SENSITIVE   = TRUE                   
                      Rs_FExtracto:VISIBLE     = TRUE
                      W_FExtracto:SENSITIVE    = FALSE 
                      W_FExtracto:VISIBLE      = FALSE 
                      W_FExtracto:SCREEN-VALUE = "".
               APPLY "VALUE-CHANGED":U TO Rs_FExtracto.
            END.
            ELSE
               ASSIGN W_CFormato:SENSITIVE     = FALSE
                      W_CFormato:VISIBLE       = FALSE
                      W_CFormato:SCREEN-VALUE  = ""
                      Rs_FExtracto:SENSITIVE   = FALSE
                      Rs_FExtracto:VISIBLE     = FALSE
                      W_FExtracto:SENSITIVE    = FALSE 
                      W_FExtracto:VISIBLE      = FALSE 
                      W_FExtracto:SCREEN-VALUE = "".
         END.
         ELSE DO:
            ASSIGN Rs_FExtracto:VISIBLE     = FALSE
                   W_CFormato:VISIBLE       = TRUE
                   W_FExtracto:VISIBLE      = TRUE
                   W_CFormato:SENSITIVE     = TRUE
                   W_FExtracto:SENSITIVE    = TRUE.
            APPLY "ENTRY":U TO W_CFormato.
            RETURN NO-APPLY.
         END.  
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Base
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Base V-table-Win
ON VALUE-CHANGED OF Cmb_Base IN FRAME F-Main
DO:
  /*ASSIGN Cuentas.Cod_base = SUBSTRING(Cmb_Base:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,4).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Producto V-table-Win
ON VALUE-CHANGED OF Cmb_Producto IN FRAME F-Main
DO:
  /*W_TipPro = SELF:LOOKUP(SELF:SCREEN-VALUE) IN FRAME {&FRAME-NAME}.
 *   ASSIGN W_TipPro = INTEGER(SUBSTRING(SELF:ENTRY(W_TipPro),1,2)).  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Cod_FlujoEfec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Cod_FlujoEfec V-table-Win
ON VALUE-CHANGED OF Cuentas.Cod_FlujoEfec IN FRAME F-Main /* Código Flujo Efectivo */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     IF Cuentas.Cod_FlujoEfec:SCREEN-VALUE EQ "D" THEN DO:
        ASSIGN Cuentas.Car_Efectivo:SENSITIVE = TRUE
               Cuentas.Car_Efectivo:VISIBLE   = TRUE.
        APPLY "VALUE-CHANGED":U TO Cuentas.Car_Efectivo. /*Cuentas.Cod_FlujoEfec.*/
        RETURN NO-APPLY.
     END.
     ELSE
        ASSIGN W_CFormato:VISIBLE             = FALSE
               W_FExtracto:VISIBLE            = FALSE
               Cuentas.Car_Efectivo:VISIBLE   = FALSE 
               Rs_FExtracto:VISIBLE           = FALSE
               W_CFormato:SENSITIVE           = FALSE 
               W_FExtracto:SENSITIVE          = FALSE  
               Rs_FExtracto:SENSITIVE         = FALSE  
               Cuentas.Car_Efectivo:SENSITIVE = FALSE .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Cuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Cuenta V-table-Win
ON LEAVE OF Cuentas.Cuenta IN FRAME F-Main /* Código de Cuenta */
OR RETURN OF Cuentas.Cuenta DO:
    IF Cuentas.Cuenta:SCREEN-VALUE EQ "" OR Cuentas.Cuenta:SCREEN-VALUE EQ ? THEN
        RETURN.

    APPLY "TAB":U TO SELF.
    ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Cuenta V-table-Win
ON TAB OF Cuentas.Cuenta IN FRAME F-Main /* Código de Cuenta */
OR MOUSE-SELECT-DBLCLICK OF Cuentas.Cuenta
OR RETURN OF Cuentas.Cuenta DO:
    ON RETURN RETURN.

    DO WITH FRAME {&Frame-Name}:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cuentas.Cuenta:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE(Cuentas) OR TRIM(Cuentas.Cuenta:SCREEN-VALUE) = "" THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 57,OUTPUT W_Rpta).
            APPLY "ENTRY" TO Cuentas.Cuenta.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            /*Busca si la cuenta tiene una cuenta mayor*/
            W_Nivel = -1.

            RUN hallarCtaMayor IN W_Manija(INPUT Cuentas.Cuenta:SCREEN-VALUE,
                                           OUTPUT W_CtaMay,
                                           INPUT-OUTPUT W_Nivel).

            IF W_CtaMay = "" AND LENGTH(Cuentas.Cuenta:SCREEN-VALUE) > 1 THEN DO:
                RUN MostrarMensaje IN W_Manija (INPUT 84, OUTPUT W_Eleccion).
                RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
                RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply_btnCancel':U ) .
            END.
            ELSE
                IF W_Nivel = 2 THEN DO:
                    RUN MostrarMensaje IN W_Manija (INPUT 85, OUTPUT W_Eleccion).
                    APPLY "ENTRY" TO Cuentas.Cuenta.
                    RETURN NO-APPLY.
                END.
                ELSE
                    IF W_CtrBalance THEN DO:
                        CASE SUBSTRING(Cuentas.Cuenta:SCREEN-VALUE,1,1):
                            WHEN "1" THEN
                                ASSIGN Cuentas.Id_Cuenta:SCREEN-VALUE = "1"
                                       Cuentas.Naturaleza:SCREEN-VALUE = "DB".

                            WHEN "2" THEN
                                ASSIGN Cuentas.Id_Cuenta:SCREEN-VALUE = "2"
                                       Cuentas.Naturaleza:SCREEN-VALUE = "CR".

                            WHEN "3" THEN
                                ASSIGN Cuentas.Id_Cuenta:SCREEN-VALUE = "3"
                                       Cuentas.Naturaleza:SCREEN-VALUE = "CR".

                            WHEN "4" THEN
                                ASSIGN Cuentas.Id_Cuenta:SCREEN-VALUE = "4"
                                       Cuentas.Naturaleza:SCREEN-VALUE = "CR".

                            WHEN "5" THEN
                                ASSIGN Cuentas.Id_Cuenta:SCREEN-VALUE = "4"
                                       Cuentas.Naturaleza:SCREEN-VALUE = "DB".

                            WHEN "8" THEN
                                ASSIGN Cuentas.Id_Cuenta:SCREEN-VALUE = "5".
                        END CASE.

                        W_CtrBalance = No.
                    END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Id_Base
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Id_Base V-table-Win
ON VALUE-CHANGED OF Cuentas.Id_Base IN FRAME F-Main /* Maneja Base */
DO:
  IF Cuentas.Id_Base:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "No" THEN
     ASSIGN Cmb_Base:VISIBLE      = FALSE
            Cmb_Base:SCREEN-VALUE = ""
            Cmb_Base:SENSITIVE    = FALSE.
  ELSE DO:
     ASSIGN Cmb_Base:VISIBLE = TRUE
            Cmb_Base:SENSITIVE = TRUE.
     RUN ffBaseRetencion.w PERSISTENT.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Id_Caja
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Id_Caja V-table-Win
ON VALUE-CHANGED OF Cuentas.Id_Caja IN FRAME F-Main /* Sin ser del portafolio se puede manejar por caja? */
DO:
  IF Cuentas.Id_Caja:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No" THEN
     Cuentas.Cod_Caja:VISIBLE = FALSE.
  ELSE
     Cuentas.Cod_Caja:VISIBLE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Id_detalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Id_detalle V-table-Win
ON VALUE-CHANGED OF Cuentas.Id_detalle IN FRAME F-Main /* Maneja Saldos por pagar o cobrar */
DO:
  IF SELF:SCREEN-VALUE IN FRAME F-Main EQ "YES" THEN
     ASSIGN Cuentas.Id_Nit:SCREEN-VALUE IN FRAME F-Main = "YES"
            Cuentas.Id_Doc:SCREEN-VALUE IN FRAME F-Main = "YES".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Id_Enlace
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Id_Enlace V-table-Win
ON VALUE-CHANGED OF Cuentas.Id_Enlace IN FRAME F-Main /* Requiere Producto */
DO:
  IF Cuentas.Id_Enlace:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "NO" THEN
     ASSIGN Rs_IdProducto:VISIBLE     = FALSE 
            Cmb_Producto:VISIBLE      = FALSE
            Cmb_Producto:SCREEN-VALUE = ""
            Rs_IdProducto:SENSITIVE   = FALSE
            Cmb_Producto:SENSITIVE    = FALSE.
  ELSE
     ASSIGN Rs_IdProducto:VISIBLE   = TRUE 
            Cmb_Producto:VISIBLE    = TRUE
            Cmb_Producto:SENSITIVE  = TRUE
            Rs_IdProducto:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Id_FinAno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Id_FinAno V-table-Win
ON VALUE-CHANGED OF Cuentas.Id_FinAno IN FRAME F-Main /* Cuenta de Cierre de Año? */
DO:
DO WITH FRAME f-main:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE Cuentas.Nit_FinAno.
     ASSIGN Cuentas.Nit_FinAno:HIDDEN = NO
            WNomNit:HIDDEN = NO.
     APPLY "entry" TO Cuentas.Nit_FinAno.
     RETURN NO-APPLY.
  END.
  ELSE DO:
     DISABLE Cuentas.Nit_FinAno.
     ASSIGN Cuentas.Nit_FinAno:HIDDEN = YES
            WNomNit:HIDDEN = YES.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Id_Presupuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Id_Presupuesto V-table-Win
ON VALUE-CHANGED OF Cuentas.Id_Presupuesto IN FRAME F-Main /* Maneja Presupuesto */
DO:
  /*IF Cuentas.Id_Presupuesto:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "No" THEN
     ASSIGN Cmb_Presupuesto:VISIBLE      = FALSE
            Cmb_Presupuesto:SCREEN-VALUE = ""
            Cmb_Presupuesto:SENSITIVE    = FALSE.
  ELSE
     ASSIGN Cmb_Presupuesto:VISIBLE   = TRUE
            Cmb_Presupuesto:SENSITIVE = TRUE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Nit_FinAno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Nit_FinAno V-table-Win
ON LEAVE OF Cuentas.Nit_FinAno IN FRAME F-Main /* Nit */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN
            WNomNit:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        ELSE DO:
            RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
            ASSIGN WNomNit:SCREEN-VALUE = TRIM(P_Nombre) + " " + P_Apellido
                   SELF:SCREEN-VALUE = P_Nit.

            FIND FIRST Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_FExtracto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_FExtracto V-table-Win
ON LEAVE OF Rs_FExtracto IN FRAME F-Main
DO:
  APPLY "VALUE-CHANGED" TO Rs_FEXTRACTO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_FExtracto V-table-Win
ON MOUSE-SELECT-CLICK OF Rs_FExtracto IN FRAME F-Main
DO:
  APPLY "VALUE-CHANGED" TO Rs_FEXTRACTO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_FExtracto V-table-Win
ON VALUE-CHANGED OF Rs_FExtracto IN FRAME F-Main
OR RETURN OF Rs_FExtracto DO:

 DO WITH FRAME {&FRAME-NAME}:
  IF Rs_FExtracto:SCREEN-VALUE = "1" THEN
     DO: 
       VIEW W_FExtracto.
       ENABLE W_FExtracto.
       ASSIGN W_FExtracto:SCREEN-VALUE = W_FExtracto:ENTRY(1)
              W_FExtracto = SUBSTRING(W_FExtracto:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2).
       APPLY "ENTRY" TO W_FExtracto.
       RETURN NO-APPLY.
     END.
  ELSE
     DO:
       ASSIGN W_FExtracto = "00".
       HIDE W_FExtracto.
       DISABLE W_FExtracto.
     END.
 END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_IdProducto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_IdProducto V-table-Win
ON VALUE-CHANGED OF Rs_IdProducto IN FRAME F-Main
DO:
  ASSIGN Rs_IdProducto
         Cmb_Producto:LIST-ITEMS     = "".
  IF Rs_IdProducto THEN
     FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK
                          BREAK BY Pro_Ahorros.Cod_Ahorro:
         IF FIRST-OF(Pro_Ahorros.Cod_Ahorro) THEN DO:
            ASSIGN W_Eleccion = Cmb_Producto:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " " + 
                   STRING(Pro_Ahorros.Nom_Producto,"X(26)")) IN FRAME {&FRAME-NAME}.
         END.          
     END.
  ELSE
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK
                            BREAK BY Pro_Creditos.Cod_Credito:
          IF FIRST-OF(Pro_Creditos.Cod_Credito) THEN DO:
             ASSIGN W_Eleccion = Cmb_Producto:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " " + 
                    STRING(Pro_Creditos.Nom_Producto,"X(26)")) IN FRAME {&FRAME-NAME}.
          END.                       
       END.
  IF Cmb_Producto:NUM-ITEMS GT 0 THEN
     Cmb_Producto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = Cmb_Producto:ENTRY(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuentas.Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuentas.Tipo V-table-Win
ON VALUE-CHANGED OF Cuentas.Tipo IN FRAME F-Main /* Tipo */
DO:
    DEFINE BUFFER Tmp_Cuentas FOR Cuentas.
    DEFINE VARIABLE Cta_Hijo LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_Sdofinal LIKE Sal_Cuenta.Sal_inicial.
    DEFINE VARIABLE W_MesAnt AS INTEGER FORMAT "99".
    DEFINE VARIABLE WMesi AS INTEGER FORMAT "99".
    DEFINE VARIABLE W_AnoAnt AS INTEGER FORMAT "9999".
    DEFINE VARIABLE P_Error AS LOGICAL.

    IF Cuentas.Tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "1" THEN DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST CortoLargo WHERE CortoLargo.Cta_AsoAd EQ Cuentas.Cuenta:SCREEN-VALUE
                                 OR CortoLargo.Cta_AsoNa EQ Cuentas.Cuenta:SCREEN-VALUE
                                 OR CortoLargo.Cta_ContingenteCr EQ Cuentas.Cuenta:SCREEN-VALUE
                                 OR CortoLargo.Cta_ContingenteDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                 OR CortoLargo.Cta_ContrapartidaGar EQ Cuentas.Cuenta:SCREEN-VALUE
                                 OR CortoLargo.Cta_FutGantia EQ Cuentas.Cuenta:SCREEN-VALUE
                                 OR CortoLargo.Cta_GarPenCancel EQ Cuentas.Cuenta:SCREEN-VALUE
                                 OR CortoLargo.Cta_NoaAd EQ Cuentas.Cuenta:SCREEN-VALUE
                                 OR CortoLargo.Cta_NoaNa EQ Cuentas.Cuenta:SCREEN-VALUE
                                 OR CortoLargo.Cta_VigGarAd EQ Cuentas.Cuenta:SCREEN-VALUE
                                 OR CortoLargo.Cta_VigGarNa EQ Cuentas.Cuenta:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE CortoLargo THEN DO:
            MESSAGE "Cuenta no puede Ser Cambiada a Mayor" SKIP(0)
                    "Configuraciones de Corto y Largo Asignadas."
                VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".

            RETURN NO-APPLY.
        END.

        FIND FIRST Liqui_Int WHERE Liqui_Int.CtaCr_DifCob EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaCr_DifCobAso EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaCr_Liq EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaCr_LiqAso EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaCr_Mora EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaCr_MoraAso EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaCr_Ret EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaDb_DifCob EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaDb_DifCobAso EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaDb_Liq EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaDb_LiqAso EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaDb_Mora EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaDb_MoraAso EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaDb_Ret EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaInt_Ant EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.CtaInt_AntAso EQ Cuentas.Cuenta:SCREEN-VALUE
                                OR Liqui_Int.Cta_SucyAge EQ Cuentas.Cuenta:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE Liqui_Int THEN DO:
            MESSAGE "Cuenta no puede Ser Cambiada a Mayor" SKIP(0)
                    "Configuración Liquidación Intereses Asignados."
                VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".

            RETURN NO-APPLY.
        END.

        FIND FIRST CarteraVencida WHERE CarteraVencida.Cta_AsoAdDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_AsoIntAdCr EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_AsoIntAdDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_AsoIntNaCr EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_AsoIntNaDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_AsoNaDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_AsoPrvAdCr EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_AsoPrvAdDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_AsoPrvNaCr EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_AsoPrvNaDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_NoaAdDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_NoaIntAdCr EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_NoaIntAdDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_NoaIntNaCr EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_NoaIntNaDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_NoaNaDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_NoaPrvAdCr EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_NoaPrvAdDb EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_NoaPrvNaCr EQ Cuentas.Cuenta:SCREEN-VALUE
                                     OR CarteraVencida.Cta_NoaPrvNaDb EQ Cuentas.Cuenta:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE Liqui_Int THEN DO:
            MESSAGE "Cuenta no puede Ser Cambiada a Mayor" SKIP(0)
                    "Configuración Cartera Vencida Asignada."
                VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".

            RETURN NO-APPLY.
        END.

        W_Sdofinal = 0.

        FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Cuenta = Cuentas.Cuenta:SCREEN-VALUE
                              AND Sal_Cuenta.Ano = YEAR(TODAY) NO-LOCK:
            DO Wmesi = 1 TO 12:
                W_Sdofinal = W_Sdofinal + Sal_Cuenta.db[Wmesi] - Sal_Cuenta.cr[Wmesi].
            END.
        END.

        IF W_Sdofinal NE 0 THEN DO:
            MESSAGE "La Cuenta Tiene Saldo Diferente de Cero." SKIP
                    "Verifique Antes de Cambiar el Tipo ."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

            Cuentas.Tipo:SCREEN-VALUE = "2".
            APPLY "ENTRY" TO Cuentas.Tipo.
            RETURN NO-APPLY.
        END.

        ASSIGN W_MesAnt = MONTH(TODAY) - 1
               W_AnoAnt = YEAR(TODAY).

        IF W_MesAnt EQ 0 THEN
            ASSIGN W_MesAnt = 12
                   W_AnoAnt = YEAR(TODAY) - 1.

        DISABLE {&list-3}.

        ASSIGN Cuentas.Id_CenCostos:SCREEN-VALUE = STRING(FALSE)
               Cuentas.Id_Doc:SCREEN-VALUE = STRING(FALSE)
               Cuentas.Id_Nit:SCREEN-VALUE = STRING(FALSE)
               Cuentas.Id_Presupuesto:SCREEN-VALUE = STRING(FALSE)
               Cuentas.Id_Base:SCREEN-VALUE = STRING(FALSE)
               Cuentas.Id_Enlace:SCREEN-VALUE = STRING(FALSE)
               Cuentas.Id_NoMvto:SCREEN-VALUE = STRING(FALSE)
               Cuentas.Ctr_Naturaleza:SCREEN-VALUE = STRING(FALSE)
               Cuentas.Id_Caja:SCREEN-VALUE = STRING(FALSE)
               Cmb_Presupuesto:VISIBLE = FALSE
               Cmb_Base:VISIBLE = FALSE
               Cmb_Producto:VISIBLE = FALSE
               Rs_IdProducto:VISIBLE = FALSE
               Cuentas.cod_Caja:VISIBLE = FALSE.
    END.
    ELSE DO WITH FRAME {&FRAME-NAME}:
        IF Cuentas.Tipo:SCREEN-VALUE EQ "2" THEN DO:
            Cta_Hijo = Cuentas.Cuenta:SCREEN-VALUE + "99".

            FIND FIRST Tmp_Cuentas WHERE Tmp_Cuentas.Cuenta GT Cuentas.Cuenta:SCREEN-VALUE
                                     AND Tmp_Cuentas.Cuenta LE Cta_Hijo NO-LOCK NO-ERROR.
            IF AVAILABLE(Tmp_Cuentas) THEN DO:
                RUN MostrarMensaje IN W_Manija (INPUT 83,OUTPUT W_Rpta).
                Cuentas.Tipo:SCREEN-VALUE = "1".
                APPLY "ENTRY" TO Cuentas.Tipo.
                RETURN NO-APPLY.
            END.

            ENABLE {&list-3}.

            Rs_IdProducto:VISIBLE = FALSE.

            FIND FIRST Entidad WHERE Entidad.Estado EQ 1 NO-LOCK NO-ERROR.
            IF Entidad.Id_CenCosto EQ NO THEN
                Cuentas.Id_CenCostos:SENSITIVE = FALSE.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FExtracto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FExtracto V-table-Win
ON VALUE-CHANGED OF W_FExtracto IN FRAME F-Main /* Extracto */
DO:
  ASSIGN W_FExtracto.
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
  {src/adm/template/row-list.i "Cuentas"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Cuentas"}

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
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  W_Nuevo = YES.
  ASSIGN W_CtrBalance                                     = YES
         Cuentas.tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1"
         Cuentas.Estado:SCREEN-VALUE                      = "1"
         Cuentas.Id_Cuenta:SCREEN-VALUE                   = "1"
         Cuentas.Id_CenCostos:SCREEN-VALUE                = "No"
         Cuentas.Id_Doc:SCREEN-VALUE                      = "No"
         Cuentas.Id_Nit:SCREEN-VALUE                      = "No"
         Cuentas.Ctr_Naturaleza:SCREEN-VALUE              = "No"
         Cuentas.Id_Presupuesto:SCREEN-VALUE              = "No"
         Cuentas.Id_Base:SCREEN-VALUE                     = "No"
         Cuentas.Id_Enlace:SCREEN-VALUE                   = "No"
         Cuentas.Naturaleza:SCREEN-VALUE                  = "DB"
         Cuentas.Cam_Posicion:SCREEN-VALUE                = "4"
         Cuentas.Cam_Captrabajo:SCREEN-VALUE              = "3"
         Cuentas.Cod_FlujoEfec:SCREEN-VALUE               = "E"
         Cmb_Presupuesto:SCREEN-VALUE                     = ""
         Cmb_Base:SCREEN-VALUE                            = ""
         Cmb_Producto:SCREEN-VALUE                        = ""
         Cmb_Presupuesto:VISIBLE                          = FALSE
         Cmb_Base Cmb_Producto:VISIBLE                    = FALSE
         Cuentas.Id_Presupuesto:SENSITIVE                 = TRUE.
  APPLY "VALUE-CHANGED":U TO Cuentas.Id_Base.

  DISABLE {&list-3} WITH FRAME {&Frame-Name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Dispatch standard ADM method.                             */

  IF Cuentas.Nombre:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN DO:
     MESSAGE "No puede Omitir el Nombre de la Cuenta. Verifique" 
     VIEW-AS ALERT-BOX INFORMATION TITLE "Error al Salvar".
     RETURN ERROR.
  END.
  
  IF AVAILABLE(CUENTAS) THEN DO:
     IF Cuentas.Cod_FlujoEfec EQ "D" AND Cuentas.Cod_FlujoEfec:SCREEN-VALUE NE "D" THEN DO:
        FIND Operacion WHERE Operacion.Cuenta EQ Cuentas.Cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE Operacion THEN DO:
           Cuentas.Cod_FlujoEfec:SCREEN-VALUE = Cuentas.Cod_FlujoEfec.
           MESSAGE "La Cuenta Contable esta asignada en una Operación, no" SKIP
                   "se realizará la Modificación (Parámetro: Flujo de Efectivo)"
                   VIEW-AS ALERT-BOX INFORMATION TITLE "Error al Salvar".
           RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
           RETURN ERROR.
        END.
     END.  
     
     IF (Cuentas.Car_Efectivo EQ 2 AND INTEGER(Cuentas.Car_Efectivo:SCREEN-VALUE) NE 2) OR
        (Cuentas.Car_Efectivo EQ 3 AND INTEGER(Cuentas.Car_Efectivo:SCREEN-VALUE) NE 3) THEN DO:
        FIND Operacion WHERE Operacion.Cuenta EQ Cuentas.Cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE Operacion THEN DO:
           Cuentas.Car_Efectivo:SCREEN-VALUE = STRING(Cuentas.Car_Efectivo).
           MESSAGE "La Cuenta Contable esta asignada en una Operación, no" SKIP
                   "se realizará la Modificación (Parámetro: Tipo)"
                   VIEW-AS ALERT-BOX INFORMATION TITLE "Error al Salvar".
           RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .                
           RETURN ERROR.
        END.
     END.
     
     IF Cuentas.Id_Caja AND Cuentas.Id_Caja:SCREEN-VALUE NE "Yes" THEN DO:
       /* FIND Operacion WHERE Operacion.Cuenta EQ Cuentas.Cuenta NO-LOCK NO-ERROR.
             IF AVAILABLE Operacion THEN DO:
                Cuentas.Id_Caja:SCREEN-VALUE = STRING(Cuentas.Id_Caja).
                MESSAGE "La Cuenta Contable esta asignada en una Operación, no" SKIP
                        "se realizará la Modificación (Parámetro: Maneja Caja)"
                        VIEW-AS ALERT-BOX INFORMATION TITLE "Error al Salvar".
                RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).                     
                RETURN ERROR.
             END.*/
     END.  

     IF Cuentas.Cod_Caja NE INTEGER(Cuentas.Cod_Caja:SCREEN-VALUE) THEN DO:
       /* FIND Operacion WHERE Operacion.Cuenta EQ Cuentas.Cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE Operacion THEN DO:
           Cuentas.Cod_Caja:SCREEN-VALUE = STRING(Cuentas.Cod_Caja).
           MESSAGE "La Cuenta Contable esta asignada en una Operación, no" SKIP
                   "se realizará la Modificación (Parámetro: Ingreso/Egreso)"
                   VIEW-AS ALERT-BOX INFORMATION TITLE "Error al Salvar".
           RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).                
           RETURN ERROR.
        END.*/
     END.
     
     IF Cuentas.Id_FinAno:SCREEN-VALUE EQ "yes" AND Cuentas.Nit_FinAno:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "Aunque la cuenta esta definida como cuenta de cierre de" SKIP
                "fin de año, aun no se ha entrado el nit para la cuenta." SKIP
                VIEW-AS ALERT-BOX INFORMATION TITLE "Error al Salvar".
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).                     
        RETURN ERROR.
     END.  
  END.


  RUN dispatch IN THIS-PROCEDURE (INPUT 'assign-record':U ).

  IF TRIM(Cuentas.Cuenta) EQ "" THEN
     RETURN ERROR.
  /*Asigna el nivel a la cuenta creada*/  
  W_Nivel = -2.
  RUN hallarCtaMayor IN W_Manija(INPUT Cuentas.Cuenta:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                 OUTPUT W_CtaMay,
                                 INPUT-OUTPUT W_Nivel).
  Cuentas.Nivel = W_Nivel.

  IF Cuentas.Id_Enlace EQ YES THEN
     Cuentas.Cod_Enlace = W_TipPro.

  IF Cuentas.Fec_Retiro:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN
     IF Cuentas.Fec_Retiro EQ ? THEN DO:
        ASSIGN Cuentas.Fec_Retiro = W_Fecha.
               Cuentas.Estado     = 2.
     END.
  IF Cmb_Presupuesto:SCREEN-VALUE NE "" AND Cuentas.Id_Presupuesto:SCREEN-VALUE EQ "YES" THEN
     Cuentas.Cod_Presupuesto = SUBSTRING(Cmb_Presupuesto:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,14).
  ELSE
     IF Cuentas.Id_Presupuesto:SCREEN-VALUE EQ "NO" THEN
        Cuentas.Cod_Presupuesto = "".
  
  IF Cuentas.Cod_FlujoEfec EQ "D" AND Cuentas.Car_Efectivo EQ 1 THEN
     Cuentas.Cod_FormatoExt = INTEGER(SUBSTRING(W_FExtracto,1,2)). 

  IF Cuentas.Cod_FlujoEfec EQ "D" AND Cuentas.Car_Efectivo GT 1 THEN
     ASSIGN Cuentas.Cod_Formato    = INTEGER(SUBSTRING(W_CFormato:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2))
            Cuentas.Cod_FormatoExt = INTEGER(SUBSTRING(W_FExtracto:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2)). 

  IF Cmb_Base:SCREEN-VALUE NE "" AND Cuentas.Id_Base:SCREEN-VALUE EQ "YES" THEN
     ASSIGN Cuentas.Cod_base = SUBSTRING(Cmb_Base:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,4)
            Cuentas.Id_Nit   = TRUE.
  ELSE
     IF Cuentas.Id_Base:SCREEN-VALUE EQ "No" THEN
        Cuentas.Cod_Base = "".

  IF Cmb_Producto:SCREEN-VALUE NE "" AND Cuentas.Id_Enlace:SCREEN-VALUE EQ "YES" THEN
     Cuentas.Cod_Enlace = INTEGER(SUBSTRING(Cmb_Producto:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,3)).
  ELSE
     ASSIGN Cuentas.Cod_Enlace = 0
            Cuentas.Id_Enlace  = FALSE.

  Cuentas.Id_Pcto = Rs_IdProducto.
  Cuentas.Nit_FinAno = Cuentas.Nit_FinAno:SCREEN-VALUE IN FRAME f-main.
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Objetivo:     No borra el registro fisicamente, solamente agrega la fecha del
                sistema a la fecha de retiro de la cuenta.
------------------------------------------------------------------------------*/
  W_Puntero = ROWID(Cuentas).
  IF Cuentas.Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "1" THEN DO:
   
     FIND FIRST Mov_Contable WHERE Mov_Contable.Cuenta 
          BEGINS Cuentas.Cuenta:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
     IF AVAILABLE(Mov_Contable) THEN
        RUN MostrarMensaje IN W_Manija (INPUT 88, OUTPUT W_Eleccion).
     ELSE DO:
        FIND Cuentas WHERE Cuentas.Cuenta BEGINS Cuentas.Cuenta:SCREEN-VALUE AND
             LENGTH(Cuentas.Cuenta) GT LENGTH(Cuentas.Cuenta:SCREEN-VALUE) AND
             Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Cuentas THEN DO:
           MESSAGE "La Cuenta no se puede inactivar ya que tiene" SKIP
                   "Subcuentas Activas. Rectifique!" VIEW-AS ALERT-BOX WARNING.
        END.
        ELSE DO:
          RUN MostrarMensaje IN W_Manija (INPUT 89, OUTPUT W_Eleccion).
          W_Rpta = W_Eleccion.
        END.
        FIND Cuentas WHERE ROWID(Cuentas) EQ W_Puntero.
     END.
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ).
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
     IF RETURN-VALUE EQ "ADM-ERROR":U THEN
        RETURN ERROR.
  END.
  ELSE
    RUN MostrarMensaje IN W_Manija (INPUT 90, OUTPUT W_Eleccion).   
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
ASSIGN Cuentas.Car_Efectivo:VISIBLE IN FRAME {&FRAME-NAME} = TRUE
       Cuentas.Car_Efectivo:SENSITIVE = TRUE.
    
RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).

IF AVAILABLE(Cuentas) THEN DO WITH FRAME {&Frame-Name}:
    IF Cuentas.Estado NE 1 AND Cuentas.Fec_Retiro NE ? THEN
        ENABLE Btn_Activar.
    ELSE
        DISABLE Btn_Activar.
    
    IF Cuentas.Id_Base EQ FALSE THEN
        Cmb_Base:VISIBLE = FALSE.
    ELSE DO:
        Cmb_Base:SENSITIVE = TRUE.
            
        FIND Base_Ret WHERE Base_Ret.Cod_Base EQ Cuentas.Cod_Base NO-ERROR.
        IF AVAILABLE(Base_Ret) THEN
            ASSIGN Cmb_Base:SCREEN-VALUE = STRING(Base_Ret.Cod_base,"X(4)") + " - " + STRING(Base_Ret.Nombre, "X(35)").
    END.
        
    IF Cuentas.Id_FinAno THEN DO:
        ENABLE Cuentas.Nit_FinAno.
        WNomNit:HIDDEN = NO.
        Cuentas.Nit_FinAno:SCREEN-VALUE = Cuentas.Nit_FinAno.
        FIND Clientes WHERE Clientes.Nit EQ Cuentas.Nit_FinAno NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN WNomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     END.
     ELSE DO:
       ASSIGN Cuentas.Nit_FinAno:HIDDEN = YES
              WNomNit:HIDDEN = YES.
     END.
        
     IF Cuentas.Tipo EQ 2 THEN
        ENABLE {&List-3}.
     ELSE
        DISABLE {&List-3}.
     
     IF Cuentas.Id_Caja:SCREEN-VALUE EQ "No":U THEN
        Cuentas.Cod_Caja:VISIBLE = FALSE.
     ELSE DO:
        ASSIGN Cuentas.Cod_Caja:SCREEN-VALUE = STRING(Cuentas.Cod_Caja)
               Cuentas.Cod_Caja:VISIBLE      = TRUE.
     END.
 
     IF Cuentas.Cod_FlujoEfec NE "D":U OR Cuentas.Tipo NE 2 THEN
        ASSIGN W_CFormato:VISIBLE             = FALSE
               W_FExtracto:VISIBLE            = FALSE 
               Cuentas.Car_Efectivo:VISIBLE   = FALSE
               Rs_FExtracto:VISIBLE           = FALSE
               W_CFormato:SENSITIVE           = FALSE
               W_FExtracto:SENSITIVE          = FALSE
               Cuentas.Car_Efectivo:SENSITIVE = FALSE
               Rs_FExtracto:SENSITIVE         = FALSE.
     ELSE DO:
        ASSIGN Cuentas.Car_Efectivo:VISIBLE   = TRUE
               Cuentas.Car_Efectivo:SENSITIVE = TRUE.
        IF Cuentas.Cod_FlujoEfec EQ "D" AND Cuentas.Car_Efectivo EQ 1  THEN DO:
           ASSIGN W_CFormato:VISIBLE     = FALSE
                  Rs_FExtracto:SENSITIVE = TRUE.
           IF Cuentas.Cod_FormatoExt EQ 0 THEN
              ASSIGN Rs_FExtracto:SCREEN-VALUE = "2" 
                     W_FExtracto:VISIBLE       = FALSE. 
           ELSE DO:
              ASSIGN Rs_FExtracto:SCREEN-VALUE = "1"
                     W_FExtracto:SENSITIVE     = TRUE.
              FIND Formatos WHERE Formatos.agencia     EQ W_agencia 
                              AND Formatos.Cod_Formato EQ Cuentas.Cod_FormatoExt 
                            NO-ERROR.
              IF AVAILABLE(Formatos) THEN
                 ASSIGN W_FExtracto:SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + "-" +
                                                   STRING(Formatos.Nom_Formato,"X(18)").
           END.
        END.
        ELSE DO:
           ASSIGN Rs_FExtracto:VISIBLE  = FALSE
                  W_CFormato:SENSITIVE  = TRUE
                  W_FExtracto:SENSITIVE = TRUE.
           FIND Formatos WHERE Formatos.agencia     EQ W_agencia 
                           AND Formatos.Cod_Formato EQ Cuentas.Cod_Formato 
                         NO-ERROR.
           IF AVAILABLE(Formatos) THEN DO:
              ASSIGN W_CFormato:SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + "-" +
                                               STRING(Formatos.Nom_Formato,"X(18)").
              FIND Formatos WHERE Formatos.agencia     EQ W_agencia 
                              AND Formatos.Cod_Formato EQ Cuentas.Cod_FormatoExt 
                            NO-ERROR.
              IF AVAILABLE(Formatos) THEN
                 ASSIGN W_FExtracto:SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + "-" + 
                                                   STRING(Formatos.Nom_Formato,"X(18)").
           END.
           ELSE DO:
              IF Cuentas.Cod_Formato EQ 0 THEN
                 ASSIGN W_CFormato:SCREEN-VALUE = "00" + "-" + "NINGUNO".
           END.
        END.
        FIND FIRST Entidad WHERE Entidad.Estado EQ 1 NO-LOCK NO-ERROR.
        IF Entidad.Id_CenCosto EQ NO THEN
           Cuentas.Id_CenCostos:SENSITIVE = FALSE.

        IF Cuentas.Id_Presupuesto:SCREEN-VALUE EQ "No" THEN
           Cmb_Presupuesto:VISIBLE = FALSE.
        ELSE DO:
           Cmb_Presupuesto:VISIBLE = TRUE.
           FIND Presupuesto WHERE Presupuesto.Cod_Presupuesto EQ Cuentas.Cod_Presupuesto AND
                                  Presupuesto.Estado          EQ 1
                            NO-ERROR.
           IF AVAILABLE(Presupuesto) THEN
              Cmb_Presupuesto:SCREEN-VALUE = STRING(Presupuesto.Cod_Presupuesto,"X(14)") + " - " + 
                                             STRING(Presupuesto.Nombre, "X(18)").
           IF Cuentas.Id_Enlace EQ FALSE THEN
              ASSIGN Rs_IdProducto:VISIBLE = FALSE 
                     Cmb_Producto:VISIBLE  = FALSE.
           ELSE DO: 
              ASSIGN Rs_IdProducto Cmb_Producto:SENSITIVE = TRUE
                     W_TipPro                             = Cuentas.Cod_Enlace
                     Cmb_Producto:LIST-ITEMS              = "".
              IF Cuentas.Id_Pcto THEN DO: 
                 ASSIGN Rs_IdProducto:SCREEN-VALUE = STRING(YES)
                        Rs_IdProducto = Cuentas.Id_Pcto.
                 FOR EACH Pro_Ahorros FIELDS(Pro_Ahorros.Estado Pro_Ahorros.Cod_Ahorro
                                             Pro_Ahorros.Nom_Producto)
                                      WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK
                                      BREAK BY Pro_Ahorros.Cod_Ahorro:
                    IF FIRST-OF(Pro_Ahorros.Cod_Ahorro) THEN
                       ASSIGN W_Eleccion = Cmb_Producto:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " " + 
                                           STRING(Pro_Ahorros.Nom_Producto,"X(26)")) IN FRAME {&FRAME-NAME}.
                 END.
                 FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Cuentas.Cod_Enlace 
                                        NO-LOCK NO-ERROR.
                 IF AVAILABLE(Pro_Ahorros) THEN 
                    ASSIGN Cmb_Producto:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " " + 
                                                       STRING(Pro_Ahorros.Nom_Producto,"X(26)").
                 ELSE 
                    ASSIGN Cmb_Producto:SCREEN-VALUE      = "" 
                           Cuentas.Id_Enlace:SCREEN-VALUE = STRING(FALSE).
              END.
              ELSE DO:
                 ASSIGN Rs_IdProducto:SCREEN-VALUE = STRING(NO)
                        Rs_IdProducto = Cuentas.Id_Pcto.
                 FOR EACH Pro_Creditos FIELDS(Pro_Creditos.Estado Pro_Creditos.Cod_Credito
                                              Pro_Creditos.Nom_Producto)
                                       WHERE Pro_Creditos.Estado EQ 1 NO-LOCK
                                       BREAK BY Pro_Creditos.Cod_Credito:
                   IF FIRST-OF(Pro_Creditos.Cod_Credito) THEN
                      ASSIGN W_Eleccion = Cmb_Producto:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " " + 
                                          STRING(Pro_Creditos.Nom_Producto,"X(26)")) IN FRAME {&FRAME-NAME}.
                 END.
                 FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Cuentas.Cod_Enlace 
                                         NO-LOCK NO-ERROR.
                 IF AVAILABLE(Pro_Creditos) THEN 
                    ASSIGN Cmb_Producto:SCREEN-VALUE = STRING(Pro_Creditos.Cod_Credito,"999") + " " + 
                                                       STRING(Pro_Creditos.Nom_Producto,"X(26)").
                 ELSE
                    ASSIGN Cmb_Producto:SCREEN-VALUE      = ""
                           Cuentas.Id_Enlace:SCREEN-VALUE = STRING(FALSE).
              END.
           END.                                
            
           IF Cuentas.Tipo EQ 1 THEN
              Cuentas.Id_Presupuesto:SENSITIVE = FALSE.
           ELSE
              Cuentas.Id_Presupuesto:SENSITIVE = TRUE.

           Cuentas.Estado:SENSITIVE = FALSE.

        END.
     END.
     APPLY "VALUE-CHANGED":U TO Cuentas.Id_Presupuesto.
     APPLY "VALUE-CHANGED":U TO Cuentas.Id_Base.
     APPLY "VALUE-CHANGED":U TO Cuentas.Id_Enlace.
     APPLY "VALUE-CHANGED":U TO Cuentas.Car_Efectivo.
     APPLY "VALUE-CHANGED":U TO Cuentas.Cod_FlujoEfec.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
  IF Cuentas.Tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "1" THEN
     DISABLE {&list-3} WITH FRAME {&Frame-Name}.
  ELSE
     IF Cuentas.Tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "2" THEN
        ENABLE {&list-3} WITH FRAME {&Frame-Name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
    
  FOR EACH Base_Ret WHERE Base_Ret.Estado EQ 1:
      W_Eleccion = Cmb_Base:ADD-LAST(STRING(Cod_Base, "X(4)") + " - " +
                   STRING(Base_Ret.Nombre, "X(35)")) IN FRAME {&FRAME-NAME}.
  END.
  
  IF Cmb_Base:NUM-ITEMS GT 0 THEN
     Cmb_Base:SCREEN-VALUE IN FRAME {&FRAME-NAME} = Cmb_Base:ENTRY(1).

  FOR EACH Presupuesto WHERE Presupuesto.Estado EQ 1:
     W_Eleccion = Cmb_Presupuesto:ADD-LAST(STRING(Presupuesto.Cod_Presupuesto, "X(14)") + " - " +
                  STRING(Presupuesto.Nombre, "X(18)")) IN FRAME {&FRAME-NAME}.
  END.
  
  IF Cmb_Presupuesto:NUM-ITEMS GT 0 THEN
     Cmb_Presupuesto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = Cmb_Presupuesto:ENTRY(1).

  FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK
                       BREAK BY Pro_Ahorros.Cod_Ahorro:
      IF FIRST-OF(Pro_Ahorros.Cod_Ahorro) THEN
         W_Eleccion = Cmb_Producto:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " " + 
                             STRING(Pro_Ahorros.Nom_Producto,"X(26)")) IN FRAME {&FRAME-NAME}.
  END.
  
  IF Cmb_Producto:NUM-ITEMS GT 0 THEN
     Cmb_Producto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = Cmb_Producto:ENTRY(1).
  
  W_Eleccion = W_CFormato:ADD-LAST("00" + "-" + "NINGUNO").
  FOR EACH Formatos WHERE Formatos.Estado EQ 1 
                      AND Formatos.Id_Formato EQ "PI"  
                      AND Formatos.agencia EQ W_agencia NO-LOCK:
      W_Eleccion = W_CFormato:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + "-" + 
                   STRING(Formatos.Nom_Formato,"X(18)")).
  END.
  
  FOR EACH Formatos WHERE Formatos.Estado EQ 1 
                      AND Formatos.Id_Formato EQ "AC" 
                      AND Formatos.agencia EQ W_agencia
                    NO-LOCK BREAK BY Formatos.Cod_Formato:                 
      IF Formatos.Cod_Formato NE W-Cod THEN 
         ASSIGN W_Eleccion = W_FExtracto:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + "-" +
                             STRING(Formatos.Nom_Formato,"X(18)"))
                W-Cod      = Formatos.Cod_Formato.                                        
      ELSE 
         W-Cod = Formatos.Cod_Formato.       
  END.
  
  IF W_CFormato:NUM-ITEMS GT 0 THEN
     W_CFormato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_CFormato:ENTRY(1).
  
  IF W_FExtracto:NUM-ITEMS GT 0 THEN
     W_FExtracto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_FExtracto:ENTRY(1).

  cuentas.id_nit:LABEL = "Maneja Nit (Tercero)".
  
/*  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .*/
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
  /*Verifica que si la cuenta es de nivel 1,2 o 3 sea entrada como de mayor */
   IF LENGTH(Cuentas.Cuenta:SCREEN-VALUE IN FRAME {&FRAME-NAME}) LT 5 AND
      INTEGER(Cuentas.Tipo:SCREEN-VALUE)                         EQ 2 THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 86, OUTPUT W_Eleccion).
      Cuentas.Tipo:SCREEN-VALUE = "1".
      RETURN ERROR. 
   END.

   IF LENGTH(Cuentas.Cuenta:SCREEN-VALUE IN FRAME {&FRAME-NAME}) EQ 16  AND 
      Cuentas.Tipo:SCREEN-VALUE EQ "1" THEN DO:
      MESSAGE "Las cuentas de ultimo nivel: 16 digitos" SKIP
              "Deben ser definidas como de movimiento" VIEW-AS ALERT-BOX.
      Cuentas.Tipo:SCREEN-VALUE = "2".
      RETURN ERROR.
   END.

  IF W_Rpta THEN DO:
     ASSIGN Cuentas.Fec_Retiro:SCREEN-VALUE = STRING(W_Fecha)
            Cuentas.Estado:SCREEN-VALUE     = "2"
            Cuentas.Estado:SENSITIVE        = FALSE
            W_Rpta                          = NO.
     RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Cuentas. Cod: " + Cuentas.Cuenta:SCREEN-VALUE).
  END.

  /* hace el dispatch a GRABAR */
  IF AVAILABLE(Cuentas) THEN DO:
     IF Cuentas.Fec_Retiro EQ ? THEN DO:
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ).
     END.
     ELSE
        RUN MostrarMensaje IN W_Manija (INPUT 87, OUTPUT W_Eleccion).
  END.
  ELSE
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ).
  IF W_Nuevo THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Cuentas. Cod: " + Cuentas.Cuenta:SCREEN-VALUE).
  ELSE
     RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Cuentas. Cod: " + Cuentas.Cuenta:SCREEN-VALUE).
     
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
  {src/adm/template/snd-list.i "Cuentas"}

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

