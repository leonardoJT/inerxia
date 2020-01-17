&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido/Variable.I "SHARED"}

DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR rowIdActivo AS ROWID.
DEFINE VAR vAgencia AS INTEGER.
DEFINE VAR vTipoActivo AS INTEGER.
DEFINE VAR vCenCostos AS INTEGER.
DEFINE VAR vIdActivo AS CHARACTER.
DEFINE VAR vAvaluo AS DECIMAL.
DEFINE VAR vValorizacion AS DECIMAL.
/* ----------------------- */

DEFINE TEMP-TABLE tt_agencias
    FIELD cod_agencia AS INTEGER
    FIELD prefijo AS CHARACTER.

DEFINE VAR pComprobante AS INTEGER INITIAL 10.
DEFINE VAR pCuentaSyA AS CHARACTER INITIAL "19040501".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Usuarios

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME Usuarios.Pedir_Clave ~
Usuarios.Estado Usuarios.Id_OpeOfi 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DEFAULT-FRAME Usuarios.Estado 
&Scoped-define ENABLED-TABLES-IN-QUERY-DEFAULT-FRAME Usuarios
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME Usuarios
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH Usuarios SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH Usuarios SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME Usuarios
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME Usuarios


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Usuarios.Estado 
&Scoped-define ENABLED-TABLES Usuarios
&Scoped-define FIRST-ENABLED-TABLE Usuarios
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 RECT-6 RECT-9 ~
cNombre txtAnotaciones btnBuscar btnAgregar btnSalir 
&Scoped-Define DISPLAYED-FIELDS Usuarios.Pedir_Clave Usuarios.Estado ~
Usuarios.Id_OpeOfi 
&Scoped-define DISPLAYED-TABLES Usuarios
&Scoped-define FIRST-DISPLAYED-TABLE Usuarios
&Scoped-Define DISPLAYED-OBJECTS txtMesesA_Depreciar cId txtValorDepreciado ~
cNombre txtFecUltDep cEmail cUsuario cmbPerfiles txtMarca cmbAgencia ~
txtSerial rsEstado cmbPrivilegios txtUbicacion tgPermiteCambioDeFecha ~
avaluo tgPermiteGestionDeCobranza fechaAvaluo txtAnotaciones ~
txtNitProveedor txtValorCompra 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAgregar 
     IMAGE-UP FILE "imagenes/agregar.jpg":U
     LABEL "Button 231" 
     SIZE 7 BY 1.77.

DEFINE BUTTON btnBuscar 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 1" 
     SIZE 7 BY 1.77.

DEFINE BUTTON btnEditar 
     IMAGE-UP FILE "imagenes/modify.jpg":U
     LABEL "btnagregar 2" 
     SIZE 7 BY 1.77.

DEFINE BUTTON btnEliminar 
     IMAGE-UP FILE "imagenes/eliminar.jpg":U
     LABEL "Button 1" 
     SIZE 7 BY 1.77.

DEFINE BUTTON btnGuardar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "btnagregar 2" 
     SIZE 7 BY 1.77.

DEFINE BUTTON btnSalir 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "btnagregar 2" 
     SIZE 7 BY 1.77.

DEFINE VARIABLE cmbAgencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38.86 BY 1 NO-UNDO.

DEFINE VARIABLE cmbPerfiles AS CHARACTER FORMAT "X(256)":U 
     LABEL "Perfil" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 38.86 BY 1 NO-UNDO.

DEFINE VARIABLE cmbPrivilegios AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Privilegios" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "1","2","3","4","5","6" 
     DROP-DOWN-LIST
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE txtAnotaciones AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 41 BY 4.85 NO-UNDO.

DEFINE VARIABLE avaluo AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Avaluo" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY 1 NO-UNDO.

DEFINE VARIABLE cEmail AS CHARACTER FORMAT "X(100)":U 
     LABEL "E-mail" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .92
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE cId AS CHARACTER FORMAT "X20)":U 
     LABEL "Identificación" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .92 NO-UNDO.

DEFINE VARIABLE cNombre AS CHARACTER FORMAT "X(70)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 42 BY .92
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE cUsuario AS CHARACTER FORMAT "X(30)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .92 NO-UNDO.

DEFINE VARIABLE fechaAvaluo AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de Avaluo" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .92 NO-UNDO.

DEFINE VARIABLE txtFecUltDep AS DATE FORMAT "99/99/99":U 
     LABEL "Fec. depreciación" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY .92
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE txtMarca AS CHARACTER FORMAT "X(256)":U 
     LABEL "Marca" 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY .92 NO-UNDO.

DEFINE VARIABLE txtMesesA_Depreciar AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Meses a depreciar" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE txtNitProveedor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nit Proveedor" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY .92 NO-UNDO.

DEFINE VARIABLE txtSerial AS CHARACTER FORMAT "X(256)":U 
     LABEL "Serial #" 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY .92 NO-UNDO.

DEFINE VARIABLE txtUbicacion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ubicación" 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY .92 NO-UNDO.

DEFINE VARIABLE txtValorCompra AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY 1 NO-UNDO.

DEFINE VARIABLE txtValorDepreciado AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Depreciado" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY 1
     BGCOLOR 3  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "imagenes/lupa.bmp":U
     SIZE 5.29 BY 1.31.

DEFINE VARIABLE rsEstado AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Activo", 1,
"Inactivo (De baja)", 2
     SIZE 20 BY 1.77 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 2.15.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 11.58.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 8.08.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 4.85.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22.43 BY 2.15.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22.43 BY 1.35.

DEFINE VARIABLE tgPermiteCambioDeFecha AS LOGICAL INITIAL no 
     LABEL "Permite cambiar fecha de trabajo" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .77 NO-UNDO.

DEFINE VARIABLE tgPermiteGestionDeCobranza AS LOGICAL INITIAL no 
     LABEL "Permite gestión de cobranza" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      Usuarios SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     txtMesesA_Depreciar AT ROW 3.27 COL 152.57 COLON-ALIGNED WIDGET-ID 74
     Usuarios.Pedir_Clave AT ROW 3.42 COL 77.86 HELP
          "" WIDGET-ID 118
          LABEL "Resetear contraseña"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .77
     cId AT ROW 3.65 COL 4.57 WIDGET-ID 124
     txtValorDepreciado AT ROW 4.31 COL 138 COLON-ALIGNED WIDGET-ID 76
     cNombre AT ROW 4.65 COL 9.57 WIDGET-ID 84
     Usuarios.Estado AT ROW 5.04 COL 78 NO-LABEL WIDGET-ID 128
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Bloqueado", 2,
"Retirado", 3
          SIZE 14 BY 2.15
     txtFecUltDep AT ROW 5.38 COL 122.57 WIDGET-ID 96
     cEmail AT ROW 5.62 COL 57 RIGHT-ALIGNED WIDGET-ID 86
     cUsuario AT ROW 6.62 COL 9.57 WIDGET-ID 30
     cmbPerfiles AT ROW 7.62 COL 16 COLON-ALIGNED WIDGET-ID 92
     txtMarca AT ROW 7.81 COL 118.14 WIDGET-ID 32
     cmbAgencia AT ROW 8.58 COL 16 COLON-ALIGNED WIDGET-ID 24
     txtSerial AT ROW 8.77 COL 116.71 WIDGET-ID 34
     rsEstado AT ROW 9.27 COL 82.43 NO-LABEL WIDGET-ID 98
     cmbPrivilegios AT ROW 9.62 COL 16 COLON-ALIGNED WIDGET-ID 28
     Usuarios.Id_OpeOfi AT ROW 10.69 COL 18 WIDGET-ID 136
          LABEL "Maneja Sucursales y Agencias"
          VIEW-AS TOGGLE-BOX
          SIZE 32 BY .77
     txtUbicacion AT ROW 10.69 COL 114.71 WIDGET-ID 122
     tgPermiteCambioDeFecha AT ROW 11.58 COL 18 WIDGET-ID 72
     avaluo AT ROW 11.77 COL 79 COLON-ALIGNED WIDGET-ID 104
     tgPermiteGestionDeCobranza AT ROW 12.46 COL 18 WIDGET-ID 138
     fechaAvaluo AT ROW 12.77 COL 72.14 WIDGET-ID 106
     txtAnotaciones AT ROW 16.23 COL 63 NO-LABEL WIDGET-ID 80
     txtNitProveedor AT ROW 17.73 COL 23.28 WIDGET-ID 48
     txtValorCompra AT ROW 20.08 COL 35.14 COLON-ALIGNED WIDGET-ID 50
     btnBuscar AT ROW 21.92 COL 62.57 WIDGET-ID 12
     btnAgregar AT ROW 21.92 COL 69.43 WIDGET-ID 14
     btnEliminar AT ROW 21.92 COL 76.29 WIDGET-ID 22
     btnEditar AT ROW 21.92 COL 83 WIDGET-ID 16
     btnGuardar AT ROW 21.92 COL 89.86 WIDGET-ID 18
     btnSalir AT ROW 21.92 COL 96.72 WIDGET-ID 20
     " Estado y contraseña" VIEW-AS TEXT
          SIZE 20.43 BY .62 AT ROW 2.62 COL 76.57 WIDGET-ID 70
     "Anotaciones:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 15.54 COL 76.43 RIGHT-ALIGNED WIDGET-ID 82
     "Datos Básicos" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.62 COL 3 WIDGET-ID 40
     "              ADMINISTRACIÓN DE USUARIOS" VIEW-AS TEXT
          SIZE 102.29 BY 1.08 AT ROW 1.27 COL 1.72 WIDGET-ID 8
          BGCOLOR 3 
     RECT-1 AT ROW 21.73 COL 61.57 WIDGET-ID 26
     RECT-2 AT ROW 2.88 COL 2 WIDGET-ID 38
     RECT-3 AT ROW 15.27 COL 2 WIDGET-ID 42
     RECT-4 AT ROW 2.88 COL 75 WIDGET-ID 68
     RECT-6 AT ROW 9.08 COL 81 WIDGET-ID 102
     RECT-9 AT ROW 21.62 COL 37 WIDGET-ID 112
     IMAGE-1 AT ROW 3.35 COL 43.72 WIDGET-ID 126
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 169.14 BY 23 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Administración de Activos Fijos"
         HEIGHT             = 23
         WIDTH              = 169.14
         MAX-HEIGHT         = 36.62
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 36.62
         VIRTUAL-WIDTH      = 194.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN avaluo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnEditar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnEliminar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnGuardar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cEmail IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       cEmail:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN cId IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR COMBO-BOX cmbAgencia IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbPerfiles IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbPrivilegios IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cNombre IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       cNombre:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN cUsuario IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fechaAvaluo IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR TOGGLE-BOX Usuarios.Id_OpeOfi IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Usuarios.Pedir_Clave IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR RADIO-SET rsEstado IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tgPermiteCambioDeFecha IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tgPermiteGestionDeCobranza IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       txtAnotaciones:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtFecUltDep IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       txtFecUltDep:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtMarca IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtMesesA_Depreciar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtNitProveedor IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtSerial IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtUbicacion IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtValorCompra IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtValorDepreciado IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       txtValorDepreciado:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR TEXT-LITERAL "Anotaciones:"
          SIZE 14 BY .62 AT ROW 15.54 COL 76.43 RIGHT-ALIGNED           */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "bdcentral.Usuarios"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Administración de Activos Fijos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Administración de Activos Fijos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME avaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL avaluo C-Win
ON LEAVE OF avaluo IN FRAME DEFAULT-FRAME /* Avaluo */
DO:
    IF DECIMAL(avaluo:SCREEN-VALUE) > 0 THEN
        txtValorCompra:SCREEN-VALUE = STRING(DECIMAL(avaluo:SCREEN-VALUE) + DECIMAL(txtValorDepreciado:SCREEN-VALUE)).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAgregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAgregar C-Win
ON CHOOSE OF btnAgregar IN FRAME DEFAULT-FRAME /* Button 231 */
DO:
    DEFINE VAR cont AS INTEGER.

    cmbAgencia:SENSITIVE = TRUE.
    cmbAgencia:SCREEN-VALUE = cmbAgencia:ENTRY(1).
    cmbPrivilegios:SENSITIVE = TRUE.
    CmbPrivilegios:SCREEN-VALUE = CmbPrivilegios:ENTRY(1).
    cUsuario:SENSITIVE = TRUE.
    cUsuario:SCREEN-VALUE = "".
    txtMarca:SENSITIVE = TRUE.
    txtMarca:SCREEN-VALUE = "".
    txtSerial:SENSITIVE = TRUE.
    txtSerial:SCREEN-VALUE = "".
    cNombre:SENSITIVE = TRUE.
    cNombre:SCREEN-VALUE = "".
    txtUbicacion:SENSITIVE = TRUE.
    txtUbicacion:SCREEN-VALUE = "".
    cId:SENSITIVE = TRUE.
    cId:SCREEN-VALUE = "".
    txtNitProveedor:SENSITIVE = TRUE.
    txtNitProveedor:SCREEN-VALUE = "".
    cEmail:SCREEN-VALUE = "".
    txtValorCompra:SENSITIVE = TRUE.
    txtValorCompra:SCREEN-VALUE = "0.00".
    avaluo:SENSITIVE = FALSE.
    avaluo:SCREEN-VALUE = "0.00".
    fechaAvaluo:SENSITIVE = FALSE.
    fechaAvaluo:SCREEN-VALUE = "".
    cmbPerfiles:SENSITIVE = TRUE.
    tgPermiteCambioDeFecha:SENSITIVE = TRUE.
    tgPermiteCambioDeFecha:SCREEN-VALUE = "no".
    tgPermiteGestionDeCobranza:SENSITIVE = TRUE.
    tgPermiteGestionDeCobranza:SCREEN-VALUE = "no".
    usuarios.pedir_clave:SCREEN-VALUE = "no".
    usuarios.id_opeOfi:SCREEN-VALUE = "no".
    txtMesesA_Depreciar:SENSITIVE = TRUE.
    txtMesesA_Depreciar:SCREEN-VALUE = "0".
    txtValorDepreciado:SENSITIVE = FALSE.
    txtValorDepreciado:SCREEN-VALUE = "0.00".
    txtAnotaciones:SENSITIVE = FALSE.
    txtAnotaciones:SCREEN-VALUE = "".
    rsEstado:SCREEN-VALUE = "1".

    btnGuardar:SENSITIVE = TRUE.
    btnEditar:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBuscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBuscar C-Win
ON CHOOSE OF btnBuscar IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    DEFINE VAR cont AS INTEGER.

    ASSIGN C-Win:SENSITIVE = FALSE.

    RUN C-ActivosFijos.R(OUTPUT rowIdActivo).

    ASSIGN C-Win:SENSITIVE = TRUE.

    FIND FIRST activosFijos WHERE ROWID(activosFijos) = rowIdActivo NO-ERROR.
    IF AVAILABLE activosFijos THEN DO:
        FIND FIRST agencias WHERE agencias.agencia = activosFijos.agencia NO-LOCK NO-ERROR.
        /*cmbAgencia:SCREEN-VALUE = STRING(agencias.agencia,"99") + " - " + agencias.nombre.*/
        cmbAgencia:SCREEN-VALUE = cmbAgencia:ENTRY(agencias.agencia + 1).
        cmbAgencia:SENSITIVE = FALSE.
        CmbPrivilegios:SCREEN-VALUE = STRING(usuarios.prioridad).
        cmbPrivilegios:SENSITIVE = FALSE.
        cUsuario:SCREEN-VALUE = activosFijos.nombre.
        cUsuario:SENSITIVE = FALSE.
        txtMarca:SCREEN-VALUE = activosFijos.marca.
        txtMarca:SENSITIVE = FALSE.
        txtSerial:SCREEN-VALUE = activosFijos.serial.
        txtSerial:SENSITIVE = FALSE.
        cNombre:SCREEN-VALUE = activosFijos.idActivo.
        cNombre:SENSITIVE = TRUE.
        txtUbicacion:SCREEN-VALUE = activosFijos.ubicacion.
        txtUbicacion:SENSITIVE = FALSE.
        cId:SCREEN-VALUE = activosFijos.responsable.
        cId:SENSITIVE = FALSE.
        txtNitProveedor:SCREEN-VALUE = activosFijos.nitProveedor.
        txtNitProveedor:SENSITIVE = FALSE.
        txtfecUltDep:SCREEN-VALUE = STRING(activosFijos.fecUltDepreciacion).
        
        APPLY "leave" TO txtNitProveedor.

        /*FIND FIRST clientes WHERE clientes.nit = activosFijos.nitProveedor NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            txtProveedor:SCREEN-VALUE = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
            txtProveedor:SENSITIVE = FALSE.
        END.*/

        txtValorCompra:SCREEN-VALUE = STRING(activosFijos.valorCompra,">>>,>>>,>>>,>>9.99").
        txtValorCompra:SENSITIVE = FALSE.
        rsEstado:SCREEN-VALUE = STRING(activos.estado).
        tgPermiteCambioDeFecha:SCREEN-VALUE = STRING(usuarios.permiteCambiarFecha).
        tgPermiteCambioDeFecha:SENSITIVE = FALSE.
        tgPermiteGestionDeCobranza:SCREEN-VALUE = STRING(usuarios.permiteGestionDeCobranza).
        tgPermiteGestionDeCobranza:SENSITIVE = FALSE.
        usuarios.pedir_clave:SCREEN-VALUE = STRING(usuarios.pedir_clave).
        usuarios.id_opeOfi:SCREEN-VALUE = STRING(usuarios.id_opeOfi).
        txtMesesA_Depreciar:SCREEN-VALUE = STRING(activosFijos.mesesDepreciar,">>>9").
        txtMesesA_Depreciar:SENSITIVE = FALSE.
        txtValorDepreciado:SCREEN-VALUE = STRING(activosFijos.valorDepreciado,">>>,>>>,>>>,>>9.99").
        txtValorDepreciado:SENSITIVE = FALSE.
        avaluo:SCREEN-VALUE = STRING(activosFijos.avaluo,">>>,>>>,>>>,>>9.99").
        avaluo:SENSITIVE = FALSE.
        
        IF activosFijos.avaluo = 0 THEN
            vAvaluo = activosFijos.valorActual.
        ELSE
            vAvaluo = activosFijos.avaluo.

        fechaAvaluo:SCREEN-VALUE = STRING(activosFijos.fechaAvaluo,"99/99/9999").
        fechaAvaluo:SENSITIVE = FALSE.
        txtAnotaciones:SCREEN-VALUE = activosFijos.anotacion.
        txtAnotaciones:SENSITIVE = FALSE.

        btnEditar:SENSITIVE = TRUE.
        btnEliminar:SENSITIVE = TRUE.
        btnAgregar:SENSITIVE = TRUE.

        IF activosFijos.estado = 2 THEN DO:
            btnEditar:SENSITIVE = FALSE.
            btnEliminar:SENSITIVE = FALSE.
        END.
    END.
    ELSE DO:
        cmbAgencia:SENSITIVE = FALSE.
        cmbAgencia:SCREEN-VALUE = "".
        cmbPrivilegios:SENSITIVE = FALSE.
        cmbPrivilegios:SCREEN-VALUE = "".
        cUsuario:SENSITIVE = FALSE.
        cUsuario:SCREEN-VALUE = "".
        txtMarca:SENSITIVE = FALSE.
        txtMarca:SCREEN-VALUE = "".
        txtSerial:SENSITIVE = FALSE.
        txtSerial:SCREEN-VALUE = "".
        cNombre:SENSITIVE = FALSE.
        cNombre:SCREEN-VALUE = "".
        txtUbicacion:SENSITIVE = FALSE.
        txtUbicacion:SCREEN-VALUE = "".
        cId:SENSITIVE = FALSE.
        cId:SCREEN-VALUE = "".
        txtNitProveedor:SENSITIVE = FALSE.
        txtNitProveedor:SCREEN-VALUE = "".
        cEmail:SCREEN-VALUE = "".
        txtValorCompra:SENSITIVE = FALSE.
        txtValorCompra:SCREEN-VALUE = "0.00".
        cmbPerfiles:SENSITIVE = FALSE.
        cmbPerfiles:SCREEN-VALUE = "".
        tgPermiteCambioDeFecha:SENSITIVE = FALSE.
        tgPermiteCambioDeFecha:SCREEN-VALUE = "no".
        tgPermiteGestionDeCobranza:SENSITIVE = FALSE.
        tgPermiteGestionDeCobranza:SCREEN-VALUE = "no".
        usuarios.pedir_clave:SCREEN-VALUE = "no".
        usuarios.id_OpeOfi:SCREEN-VALUE = "no".
        txtMesesA_Depreciar:SENSITIVE = FALSE.
        txtMesesA_Depreciar:SCREEN-VALUE = "0".
        txtValorDepreciado:SENSITIVE = FALSE.
        txtValorDepreciado:SCREEN-VALUE = "0.00".
        avaluo:SENSITIVE = FALSE.
        avaluo:SCREEN-VALUE = "0.00".
        fechaAvaluo:SENSITIVE = FALSE.
        fechaAvaluo:SCREEN-VALUE = "".
        txtAnotaciones:SENSITIVE = FALSE.
        txtAnotaciones:SCREEN-VALUE = "".
        rsEstado:SCREEN-VALUE = "1".

        btnGuardar:SENSITIVE = FALSE.
        btnEditar:SENSITIVE = FALSE.
        btnEliminar:SENSITIVE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEditar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEditar C-Win
ON CHOOSE OF btnEditar IN FRAME DEFAULT-FRAME /* btnagregar 2 */
DO:
    cmbAgencia:SENSITIVE = TRUE.
    cmbPrivilegios:SENSITIVE = TRUE.
    cUsuario:SENSITIVE = TRUE.
    txtMarca:SENSITIVE = TRUE.
    txtSerial:SENSITIVE = TRUE.
    cNombre:SENSITIVE = TRUE.
    txtUbicacion:SENSITIVE = TRUE.
    cId:SENSITIVE = TRUE.
    txtNitProveedor:SENSITIVE = TRUE.
    txtValorCompra:SENSITIVE = FALSE.
    cmbPerfiles:SENSITIVE = TRUE.
    tgPermiteCambioDeFecha:SENSITIVE = TRUE.
    tgPermiteGestionDeCobranza:SENSITIVE = TRUE.
    txtMesesA_Depreciar:SENSITIVE = TRUE.
    txtValorDepreciado:SENSITIVE = FALSE.
    avaluo:SENSITIVE = FALSE.
    fechaAvaluo:SENSITIVE = FALSE.
    
    btnGuardar:SENSITIVE = TRUE.

    FIND FIRST usuarios WHERE usuarios.usuario = w_usuario NO-LOCK NO-ERROR.
    IF AVAILABLE usuarios THEN DO:
        IF usuarios.prioridad = 6 THEN
            ASSIGN txtValorCompra:SENSITIVE = TRUE
                   avaluo:SENSITIVE = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEliminar C-Win
ON CHOOSE OF btnEliminar IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    DEFINE VAR flagBorrar AS LOGICAL.

    MESSAGE "Está seguro que desea dar de baja este Activo?"
        VIEW-AS ALERT-BOX BUTTONS YES-NO SET flagBorrar.

    IF flagBorrar = TRUE THEN DO:
        /* Falta hacer la contabilidad respectiva */

        activosFijos.estado = 2.

        cmbAgencia:SENSITIVE = FALSE.
        cmbAgencia:SCREEN-VALUE = cmbAgencia:ENTRY(1).
        cmbPrivilegios:SENSITIVE = FALSE.
        cmbPrivilegios:SCREEN-VALUE = CmbPrivilegios:ENTRY(1).
        cUsuario:SENSITIVE = FALSE.
        cUsuario:SCREEN-VALUE = "".
        txtMarca:SENSITIVE = FALSE.
        txtMarca:SCREEN-VALUE = "".
        txtSerial:SENSITIVE = FALSE.
        txtSerial:SCREEN-VALUE = "".
        cNombre:SENSITIVE = FALSE.
        cNombre:SCREEN-VALUE = "".
        txtUbicacion:SENSITIVE = FALSE.
        txtUbicacion:SCREEN-VALUE = "".
        cId:SENSITIVE = FALSE.
        cId:SCREEN-VALUE = "".
        txtNitProveedor:SENSITIVE = FALSE.
        txtNitProveedor:SCREEN-VALUE = "".
        cEmail:SCREEN-VALUE = "".
        txtValorCompra:SENSITIVE = FALSE.
        txtValorCompra:SCREEN-VALUE = "0.00".
        cmbPerfiles:SENSITIVE = FALSE.
        tgPermiteCambioDeFecha:SENSITIVE = FALSE.
        tgPermiteCambioDeFecha:SCREEN-VALUE = "no".
        tgPermiteGestionDeCobranza:SENSITIVE = FALSE.
        tgPermiteGestionDeCobranza:SCREEN-VALUE = "no".
        usuarios.pedir_clave:SCREEN-VALUE = "no".
        usuarios.id_OpeOfi:SCREEN-VALUE = "no".
        txtMesesA_Depreciar:SENSITIVE = FALSE.
        txtMesesA_Depreciar:SCREEN-VALUE = "0".
        txtValorDepreciado:SENSITIVE = FALSE.
        txtValorDepreciado:SCREEN-VALUE = "0.00".
        avaluo:SENSITIVE = FALSE.
        avaluo:SCREEN-VALUE = "0.00".
        fechaAvaluo:SENSITIVE = FALSE.
        fechaAvaluo:SCREEN-VALUE = "".
        txtAnotaciones:SENSITIVE = FALSE.
        txtAnotaciones:SCREEN-VALUE = "".
        
        btnBuscar:SENSITIVE = TRUE.
        btnAgregar:SENSITIVE = TRUE.
        btnEliminar:SENSITIVE = FALSE.
        btnEditar:SENSITIVE = FALSE.
        btnGuardar:SENSITIVE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGuardar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGuardar C-Win
ON CHOOSE OF btnGuardar IN FRAME DEFAULT-FRAME /* btnagregar 2 */
DO:
    IF cmbPrivilegios:SCREEN-VALUE <> ""     AND cmbPrivilegios:SCREEN-VALUE <> ?     AND
       cmbAgencia:SCREEN-VALUE <> ""        AND cmbAgencia:SCREEN-VALUE <> ?        AND
       cUsuario:SCREEN-VALUE <> ""          AND cUsuario:SCREEN-VALUE <> ?         AND
       txtNitProveedor:SCREEN-VALUE <> ""   AND txtNitProveedor:SCREEN-VALUE <> ?   AND
       txtValorCompra:SCREEN-VALUE <> ""    AND txtValorCompra:SCREEN-VALUE <> ?    AND DECIMAL(txtValorCompra:SCREEN-VALUE) > 0 AND
       cmbPerfiles:SCREEN-VALUE <> ""       AND cmbPerfiles:SCREEN-VALUE <> ? THEN DO:

        IF (INTEGER(SUBSTRING(cmbPrivilegios:SCREEN-VALUE IN FRAME default-frame,1,2)) <> 1 AND 
            INTEGER(SUBSTRING(cmbPrivilegios:SCREEN-VALUE IN FRAME default-frame,1,2)) <> 2 AND
            INTEGER(SUBSTRING(cmbPrivilegios:SCREEN-VALUE IN FRAME default-frame,1,2)) <> 3 AND
            INTEGER(SUBSTRING(cmbPrivilegios:SCREEN-VALUE IN FRAME default-frame,1,2)) <> 4) AND
           (txtMarca:SCREEN-VALUE = ""      OR txtMarca:SCREEN-VALUE = ?   OR
            txtSerial:SCREEN-VALUE = ""     OR txtSerial:SCREEN-VALUE = ?) THEN DO:
            MESSAGE "Todos los campos deben estar diligenciados." SKIP
                    "Revise la información y vuelva a intentar."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN NO-APPLY.
        END.

        RUN GrabarActivoFijo.
        
        cmbAgencia:SENSITIVE = FALSE.
        cmbPrivilegios:SENSITIVE = FALSE.
        cUsuario:SENSITIVE = FALSE.
        txtMarca:SENSITIVE = FALSE.
        txtSerial:SENSITIVE = FALSE.
        cNombre:SENSITIVE = TRUE.
        txtUbicacion:SENSITIVE = FALSE.
        cId:SENSITIVE = FALSE.
        cId:SENSITIVE = FALSE.
        txtNitProveedor:SENSITIVE = FALSE.
        txtValorCompra:SENSITIVE = FALSE.
        cmbPerfiles:SENSITIVE = FALSE.
        tgPermiteCambioDeFecha:SENSITIVE = FALSE.
        tgPermiteGestionDeCobranza:SENSITIVE = FALSE.
        txtMesesA_Depreciar:SENSITIVE = FALSE.
        txtValorDepreciado:SENSITIVE = FALSE.
        avaluo:SENSITIVE = FALSE.
        fechaAvaluo:SENSITIVE = FALSE.
        txtAnotaciones:SENSITIVE = FALSE.

        btnGuardar:SENSITIVE = FALSE.
        btnEditar:SENSITIVE = TRUE.
        btnEliminar:SENSITIVE = TRUE.
    END.
    ELSE
        MESSAGE "Todos los campos deben estar diligenciados." SKIP
                "Revise la información y vuelva a intentar."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSalir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSalir C-Win
ON CHOOSE OF btnSalir IN FRAME DEFAULT-FRAME /* btnagregar 2 */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cId C-Win
ON VALUE-CHANGED OF cId IN FRAME DEFAULT-FRAME /* Identificación */
DO:
    FIND FIRST clientes WHERE clientes.nit = cId:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN
        RUN mostrarDatos (INPUT clientes.nit) NO-ERROR.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 C-Win
ON MOUSE-SELECT-CLICK OF IMAGE-1 IN FRAME DEFAULT-FRAME
DO:
    DEFINE VAR pId AS CHARACTER.
    DEFINE VAR pNombre AS CHARACTER.
    DEFINE VAR pApellido AS CHARACTER.
    DEFINE VAR pAgenciaCliente AS INTEGER.

    RUN C-Clientes.R(INPUT 1,
                     INPUT W_Agencia,
                     OUTPUT pId,
                     OUTPUT pNombre,
                     OUTPUT pApellido,
                     OUTPUT pAgenciaCliente).

    cId:SCREEN-VALUE = pId.
    cNombre:SCREEN-VALUE = pNombre + " " + pApellido.

    FIND FIRST Clientes WHERE clientes.nit = pId NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FIND FIRST usuarios WHERE usuarios.usuario = w_usuario NO-LOCK NO-ERROR.

    /* Se llena el combo de Perfiles */
    W_Ok = CmbPerfiles:ADD-LAST("").

    FOR EACH grupos WHERE grupos.estado = 1 NO-LOCK BY grupos.grupo:
        W_Ok = CmbPerfiles:ADD-LAST(STRING(grupos.grupo,"99") + " - " + grupos.nombre).
    END.

    /* Se llena el combo de agencias */
    W_Ok = cmbAgencia:ADD-LAST("").

    FOR EACH Agencias WHERE Agencias.Estado = 1 NO-LOCK:
        W_Ok = CmbAgencia:ADD-LAST(STRING(Agencias.Agencia,"99") + " - " + Agencias.Nombre).

        IF agencias.agencia = w_agencia THEN
            CmbAgencia:SCREEN-VALUE = STRING(Agencias.Agencia,"99") + " - " + Agencias.Nombre.
    END.

    RUN mostrarDatosUsuario (INPUT w_usuario) NO-ERROR.
    RUN mostrarDatosCliente (INPUT usuarios.nit) NO-ERROR.

    RUN enable_UI.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE contabilizar C-Win 
PROCEDURE contabilizar :
DEFINE INPUT PARAMETER vAgencia AS INTEGER.
DEFINE INPUT PARAMETER vCenCostos AS INTEGER.
DEFINE INPUT PARAMETER vCuenta AS CHARACTER.
DEFINE INPUT PARAMETER vNit AS CHARACTER.
DEFINE INPUT PARAMETER vDb AS DECIMAL.
DEFINE INPUT PARAMETER vCr AS DECIMAL.

CREATE mov_contable.
ASSIGN Mov_Contable.agencia = vAgencia
       Mov_Contable.Cen_Costos = vCenCostos
       Mov_Contable.Comentario = "Activos Fijos"
       Mov_Contable.Comprobante = pComprobante
       Mov_Contable.Cr = vCr
       Mov_Contable.Cuenta = vCuenta
       Mov_Contable.Db = vDb
       mov_Contable.Estacion = w_estacion
       Mov_Contable.Fec_Contable = w_fecha
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Nit = vNit
       Mov_Contable.Num_Documento = comprobantes.secuencia
       Mov_Contable.Usuario = w_usuario.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  DISPLAY txtMesesA_Depreciar cId txtValorDepreciado cNombre txtFecUltDep cEmail 
          cUsuario cmbPerfiles txtMarca cmbAgencia txtSerial rsEstado 
          cmbPrivilegios txtUbicacion tgPermiteCambioDeFecha avaluo 
          tgPermiteGestionDeCobranza fechaAvaluo txtAnotaciones txtNitProveedor 
          txtValorCompra 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE Usuarios THEN 
    DISPLAY Usuarios.Pedir_Clave Usuarios.Estado Usuarios.Id_OpeOfi 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 RECT-6 RECT-9 cNombre Usuarios.Estado 
         txtAnotaciones btnBuscar btnAgregar btnSalir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabarActivoFijo C-Win 
PROCEDURE GrabarActivoFijo :
DEFINE VAR flgNuevo AS LOGICAL.
DEFINE VAR vDB AS DECIMAL.
DEFINE VAR vCR AS DECIMAL.
DEFINE VAR flgSecuencia AS LOGICAL INITIAL FALSE.
DEFINE VAR vValorDepreciadoLastYear AS DECIMAL.
DEFINE VAR vValorDepreciadoThisYear AS DECIMAL.
DEFINE VAR valorReversarDepreciacion AS DECIMAL.
DEFINE VAR valorAjusteAvaluo AS DECIMAL.

IF NOT AVAILABLE activosFijos THEN DO:
    CREATE activosFijos.
    cfg_activosFijos.consecutivo = cfg_activosFijos.consecutivo + 1.
    flgNuevo = TRUE.

    CREATE clientes.
    clientes.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE IN FRAME default-frame,1,2)).
    usuarios.usuario = cUsuario:SCREEN-VALUE.
    clientes.fec_ingreso = w_fecha.
    clientes.fec_ultActualiza = w_fecha.
    clientes.tipo_vinculo = 6.
    clientes.usuario = w_usuario.
END.
ELSE DO:
    FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = INTEGER(SUBSTRING(cmbPrivilegios:SCREEN-VALUE IN FRAME default-frame,1,2)) NO-LOCK NO-ERROR.

    vAgencia = activosFijos.agencia.
    vTipoActivo = activosFijos.tipoActivo.
    vCenCostos = activosFijos.cen_costos.
    vIdActivo = activosFijos.idActivo.
    vValorizacion = activosFijos.valorizacion.

    /*IF vIdActivo <> txtId:SCREEN-VALUE THEN DO:
        CREATE clientes.
        clientes.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE IN FRAME default-frame,1,2)).
        clientes.nit = txtId:SCREEN-VALUE.
        clientes.nombre = txtNombre:SCREEN-VALUE.
        clientes.fec_ingreso = w_fecha.
        clientes.fec_ultActualiza = w_fecha.
        clientes.tipo_vinculo = 6.
        clientes.usuario = w_usuario.
    END.*/
END.

activosFijos.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE IN FRAME default-frame,1,2)).
usuarios.prioridad = INTEGER(cmbPrivilegios:SCREEN-VALUE).
usuario.usuario = cUsuario:SCREEN-VALUE.
activosFijos.marca = txtMarca:SCREEN-VALUE.
usuarios.grupo = INTEGER(SUBSTRING(cmbPerfiles:SCREEN-VALUE,1,2)).

IF tgPermiteCambioDeFecha:SCREEN-VALUE = "yes" THEN
    usuarios.permiteCambiarFecha = TRUE.
ELSE
    usuarios.permiteCambiarFecha = FALSE.

IF tgPermiteGestionDeCobranza:SCREEN-VALUE = "yes" THEN
    usuarios.permiteGestionDeCobranza = TRUE.
ELSE
    usuarios.permiteGestionDeCobranza = FALSE.

activosFijos.estado = 1.
activosFijos.mesesDepreciar = INTEGER(txtMesesA_Depreciar:SCREEN-VALUE).
activosFijos.nitProveedor = txtNitProveedor:SCREEN-VALUE.
usuarios.usuario = cUsuario:SCREEN-VALUE.
activosFijos.serial = txtSerial:SCREEN-VALUE.
activosFijos.ubicacion = txtUbicacion:SCREEN-VALUE.
usuarios.nit = cId:SCREEN-VALUE.
activosFijos.valorCompra = DECIMAL(txtValorCompra:SCREEN-VALUE).
activosFijos.avaluo = DECIMAL(avaluo:SCREEN-VALUE).
activosFijos.fechaAvaluo = DATE(fechaAvaluo:SCREEN-VALUE).

FIND FIRST comprobantes WHERE comprobantes.agencia = activosfijos.agencia
                          AND comprobantes.comprobante = pComprobante NO-ERROR.

/*
CONTABILIZAR:
- Agencia
- Cen_costos
- Cuenta
- Nit
- DB
- CR
*/

IF flgNuevo = FALSE THEN DO:
    /* Cambia el tipo de activo */
    IF activosFijos.tipoActivo <> vTipoActivo THEN DO:
        IF flgSecuencia = FALSE THEN
            ASSIGN comprobantes.secuencia = comprobantes.secuencia + 1
                   flgSecuencia = TRUE.

        FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = vTipoActivo NO-LOCK NO-ERROR.

        RUN contabilizar(INPUT vAgencia, INPUT vCenCostos, INPUT cfg_activosFijos.activoFijo, INPUT vIdActivo, INPUT 0, INPUT activosFijos.valorActual + activosFijos.valorDepreciado).
        
        vIdActivo = activosFijos.idActivo.

        FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = activosFijos.tipoActivo NO-LOCK NO-ERROR.

        RUN contabilizar(INPUT vAgencia, INPUT vCenCostos, INPUT cfg_activosFijos.activoFijo, INPUT activosFijos.idActivo, INPUT activosFijos.valorActual + activosFijos.valorDepreciado, INPUT 0).
    END.

    
    /* Cambia la agencia */
    IF activosFijos.agencia <> vAgencia THEN DO:
        IF flgSecuencia = FALSE THEN
            ASSIGN comprobantes.secuencia = comprobantes.secuencia + 1
                   flgSecuencia = TRUE.

        FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = activosFijos.tipoActivo NO-LOCK NO-ERROR.

        RUN contabilizar(INPUT vAgencia, INPUT vCenCostos, INPUT cfg_activosFijos.activoFijo, INPUT vIdActivo,  INPUT 0, INPUT activosFijos.valorActual + activosFijos.valorDepreciado).
        RUN contabilizar(INPUT vAgencia, INPUT 999, INPUT pCuentaSyA, INPUT STRING(activosFijos.agencia,"999"), INPUT activosFijos.valorActual + activosFijos.valorDepreciado, INPUT 0).

        FIND FIRST comprobantes WHERE comprobantes.comprobante = pComprobante AND comprobantes.agencia = activosFijos.agencia NO-ERROR.
        
        comprobantes.secuencia = comprobantes.secuencia + 1.

        RUN contabilizar(INPUT activosFijos.agencia, INPUT 999, INPUT pCuentaSyA, INPUT STRING(vAgencia,"999"), INPUT 0, INPUT activosFijos.valorActual + activosFijos.valorDepreciado).
        RUN contabilizar(INPUT activosFijos.agencia, INPUT activosFijos.cen_costos, INPUT cfg_activosFijos.activoFijo, INPUT activosFijos.idActivo, INPUT activosFijos.valorActual + activosFijos.valorDepreciado, INPUT 0).

        vCenCostos = activosFijos.cen_costos.
    END.

    /* Cambia el centro de costos */
    IF activosFijos.cen_costos <> vCenCostos THEN DO:
        IF flgSecuencia = FALSE THEN
            ASSIGN comprobantes.secuencia = comprobantes.secuencia + 1
                   flgSecuencia = TRUE.

        FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = activosFijos.tipoActivo NO-LOCK NO-ERROR.

        RUN contabilizar(INPUT activosFijos.agencia, INPUT vCenCostos, INPUT cfg_activosFijos.activoFijo, INPUT vIdActivo, INPUT 0, INPUT activosFijos.valorActual + activosFijos.valorDepreciado).
        RUN contabilizar(INPUT activosFijos.agencia, INPUT activosFijos.cen_costos, INPUT cfg_activosFijos.activoFijo, INPUT activosFijos.idActivo, INPUT activosFijos.valorActual + activosFijos.valorDepreciado, INPUT 0).
    END.

    /* Cambia el avaluo */
    IF activosFijos.avaluo > 0 AND activosFijos.avaluo <> vAvaluo THEN DO:
        FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = activosFijos.tipoActivo NO-LOCK NO-ERROR.

        valorAjusteAvaluo = activosFijos.avaluo - activosFijos.valorActual.

        IF flgSecuencia = FALSE THEN DO:
            comprobantes.secuencia = comprobantes.secuencia + 1.
            flgSecuencia = TRUE.
        END.

        /* Si el avaluo aumenta se devuelve lo que sea posible devolver del gasto de depreciación para el año que corre */
        IF valorAjusteAvaluo > 0 THEN DO:
            /* La primera opción es devolver la depreciación */
            MESSAGE "Desea reversar la depreciación para realizar el ajuste" SKIP
                    "del avalúo...?"
                VIEW-AS ALERT-BOX BUTTONS YES-NO SET flagReversarDepreciacion AS LOGICAL.

            IF flagReversarDepreciacion = YES THEN DO:
                FIND FIRST rep_activosFijos WHERE rep_activosFijos.fecCorte = DATE(12,31,YEAR(w_fecha) - 1)
                                              AND rep_activosFijos.id = activosFijos.idActivo NO-LOCK NO-ERROR.
                IF AVAILABLE rep_activosFijos THEN
                    vValorDepreciadoLastYear = rep_activosFijos.valorDepreciado.

                vValorDepreciadoThisYear = activosFijos.valorDepreciado - vValorDepreciadoLastYear.
            END.

            IF vValorDepreciadoThisYear >= valorAjusteAvaluo THEN DO:
                valorReversarDepreciacion = valorAjusteAvaluo.
                valorAjusteAvaluo = 0.
            END.
            ELSE DO:
                valorReversarDepreciacion = vValorDepreciadoThisYear.
                valorAjusteAvaluo = valorAjusteAvaluo - valorReversarDepreciacion.
            END.

            IF valorReversarDepreciacion > 0 THEN DO:
                RUN contabilizar(INPUT activosFijos.agencia, INPUT activosFijos.cen_costos, INPUT cfg_activosFijos.gasto, INPUT activosFijos.idActivo, INPUT 0, INPUT valorReversarDepreciacion).
                activosFijos.valorDepreciado = activosFijos.valorDepreciado - valorReversarDepreciacion.
            END.

            IF valorAjusteAvaluo > 0 THEN
                RUN contabilizar(INPUT activosFijos.agencia, INPUT activosFijos.cen_costos, INPUT cfg_activosFijos.ingreso_xAvaluo, INPUT activosFijos.idActivo, INPUT 0, INPUT valorAjusteAvaluo).
            
            RUN contabilizar(INPUT activosFijos.agencia, INPUT activosFijos.cen_costos, INPUT cfg_activosFijos.activoFijo, INPUT activosFijos.idActivo, INPUT activosFijos.avaluo - activosFijos.valorActual, INPUT 0).
        END.
        ELSE DO:
            activosFijos.valorDepreciado = activosFijos.valorDepreciado + ABS(valorAjusteAvaluo).
            RUN contabilizar(INPUT activosFijos.agencia, INPUT activosFijos.cen_costos, INPUT cfg_activosFijos.gasto, INPUT activosFijos.idActivo, INPUT ABS(valorAjusteAvaluo), 0).
            RUN contabilizar(INPUT activosFijos.agencia, INPUT activosFijos.cen_costos, INPUT cfg_activosFijos.activoFijo, INPUT activosFijos.idActivo, INPUT 0, INPUT ABS(valorAjusteAvaluo)).
        END.

        activosFijos.valorActual = activosFijos.avaluo.
        activosFijos.valorCompra = activosFijos.valorActual + activosFijos.valorDepreciado.
    END.
END.

MESSAGE "El Activo Fijo fue grabado con éxito!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mostrarDatosCliente C-Win 
PROCEDURE mostrarDatosCliente :
DEFINE INPUT PARAMETER pId AS CHARACTER.

cNombre:SCREEN-VALUE IN FRAME default-frame = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
cUsuario:SCREEN-VALUE = usuarios.usuario.
cEmail:SCREEN-VALUE = clientes.email.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mostrarDatosUsuario C-Win 
PROCEDURE mostrarDatosUsuario :
DEFINE INPUT PARAMETER pUsuario AS CHARACTER.

FIND FIRST usuarios WHERE usuarios.usuario = pUsuario NO-LOCK NO-ERROR.

cUsuario:SCREEN-VALUE IN FRAME default-frame = usuarios.usuario.

FIND FIRST grupos WHERE grupos.grupo = usuario.grupo AND grupo.estado = 1 NO-LOCK NO-ERROR.
IF AVAILABLE grupos THEN
    CmbPerfiles:SCREEN-VALUE = STRING(grupos.grupo,"99") + " - " + grupos.nombre.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

