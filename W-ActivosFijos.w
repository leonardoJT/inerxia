&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 RECT-6 RECT-7 ~
RECT-8 RECT-9 txtId txtAnotaciones btnBuscar btnAgregar btnSalir 
&Scoped-Define DISPLAYED-OBJECTS tgDepreciable cmbAgencia ~
txtMesesA_Depreciar cmbCentroDeCostos txtValorDepreciado cmbTipoActivo ~
txtNombre txtFecUltDep txtMarca txtValorActual txtSerial txtId rsEstado ~
txtUbicacion txtResponsable avaluo fechaAvaluo txtFechaDeCompra ~
txtNumFactura txtNitProveedor txtAnotaciones txtProveedor txtValorCompra ~
chkContabilizado 

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

DEFINE VARIABLE cmbCentroDeCostos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Centro de Costos" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 38.86 BY 1 NO-UNDO.

DEFINE VARIABLE cmbTipoActivo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo Activo" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 38.86 BY 1 NO-UNDO.

DEFINE VARIABLE txtAnotaciones AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 41 BY 4.85 NO-UNDO.

DEFINE VARIABLE avaluo AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Avaluo" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY 1 NO-UNDO.

DEFINE VARIABLE fechaAvaluo AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de Avaluo" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .92 NO-UNDO.

DEFINE VARIABLE txtFechaDeCompra AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de Compra" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .92 NO-UNDO.

DEFINE VARIABLE txtFecUltDep AS DATE FORMAT "99/99/99":U 
     LABEL "Fec. depreciación" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY .92
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE txtId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Id de Activo" 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY .92
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

DEFINE VARIABLE txtNombre AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY .92 NO-UNDO.

DEFINE VARIABLE txtNumFactura AS CHARACTER FORMAT "X(256)":U 
     LABEL "Factura #" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY .92 NO-UNDO.

DEFINE VARIABLE txtProveedor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Razón Social" 
     VIEW-AS FILL-IN 
     SIZE 38.72 BY .92
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE txtResponsable AS CHARACTER FORMAT "X(256)":U 
     LABEL "Responsable" 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY .92 NO-UNDO.

DEFINE VARIABLE txtSerial AS CHARACTER FORMAT "X(256)":U 
     LABEL "Serial #" 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY .92 NO-UNDO.

DEFINE VARIABLE txtUbicacion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ubicación" 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY .92 NO-UNDO.

DEFINE VARIABLE txtValorActual AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Actual" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY 1 NO-UNDO.

DEFINE VARIABLE txtValorCompra AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY 1 NO-UNDO.

DEFINE VARIABLE txtValorDepreciado AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Depreciado" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY 1
     BGCOLOR 3  NO-UNDO.

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
     SIZE 59 BY 9.96.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 7.81.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42.72 BY 18.58.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22.43 BY 2.15.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY .12.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY .12.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22.43 BY 1.35.

DEFINE VARIABLE chkContabilizado AS LOGICAL INITIAL no 
     LABEL "Contabilizado" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .77 NO-UNDO.

DEFINE VARIABLE tgDepreciable AS LOGICAL INITIAL no 
     LABEL "Depreciable" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tgDepreciable AT ROW 3.35 COL 88.86 WIDGET-ID 72
     cmbAgencia AT ROW 3.5 COL 18.14 COLON-ALIGNED WIDGET-ID 24
     txtMesesA_Depreciar AT ROW 4.27 COL 93.43 COLON-ALIGNED WIDGET-ID 74
     cmbCentroDeCostos AT ROW 4.46 COL 18.14 COLON-ALIGNED WIDGET-ID 92
     txtValorDepreciado AT ROW 5.31 COL 78.86 COLON-ALIGNED WIDGET-ID 76
     cmbTipoActivo AT ROW 5.42 COL 18.14 COLON-ALIGNED WIDGET-ID 28
     txtNombre AT ROW 6.38 COL 11.71 WIDGET-ID 30
     txtFecUltDep AT ROW 6.38 COL 63.43 WIDGET-ID 96
     txtMarca AT ROW 7.35 COL 13.28 WIDGET-ID 32
     txtValorActual AT ROW 7.96 COL 78.86 COLON-ALIGNED WIDGET-ID 78
     txtSerial AT ROW 8.31 COL 11.85 WIDGET-ID 34
     txtId AT ROW 9.27 COL 8.28 WIDGET-ID 84
     rsEstado AT ROW 9.27 COL 82.43 NO-LABEL WIDGET-ID 98
     txtUbicacion AT ROW 10.23 COL 9.85 WIDGET-ID 122
     txtResponsable AT ROW 11.23 COL 6.71 WIDGET-ID 124
     avaluo AT ROW 11.77 COL 79 COLON-ALIGNED WIDGET-ID 104
     fechaAvaluo AT ROW 12.77 COL 72.14 WIDGET-ID 106
     txtFechaDeCompra AT ROW 14.12 COL 27.43 WIDGET-ID 36
     txtNumFactura AT ROW 15.08 COL 27.42 WIDGET-ID 46
     txtNitProveedor AT ROW 16.04 COL 23.28 WIDGET-ID 48
     txtAnotaciones AT ROW 16.23 COL 63 NO-LABEL WIDGET-ID 80
     txtProveedor AT ROW 17.04 COL 58.72 RIGHT-ALIGNED WIDGET-ID 86
     txtValorCompra AT ROW 18.38 COL 35.14 COLON-ALIGNED WIDGET-ID 50
     chkContabilizado AT ROW 20.19 COL 38 WIDGET-ID 118
     btnBuscar AT ROW 21.92 COL 62.57 WIDGET-ID 12
     btnAgregar AT ROW 21.92 COL 69.43 WIDGET-ID 14
     btnEliminar AT ROW 21.92 COL 76.29 WIDGET-ID 22
     btnEditar AT ROW 21.92 COL 83 WIDGET-ID 16
     btnGuardar AT ROW 21.92 COL 89.86 WIDGET-ID 18
     btnSalir AT ROW 21.92 COL 96.72 WIDGET-ID 20
     "Datos Básicos" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.62 COL 3 WIDGET-ID 40
     "Estado" VIEW-AS TEXT
          SIZE 7.14 BY .62 AT ROW 2.62 COL 63.86 WIDGET-ID 70
     "            ADMINISTRACIÓN DE ACTIVOS FIJOS" VIEW-AS TEXT
          SIZE 102.29 BY 1.08 AT ROW 1.27 COL 1.72 WIDGET-ID 8
          BGCOLOR 3 
     "Detalles de la compra" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 13.35 COL 3 WIDGET-ID 44
     "Anotaciones:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 15.54 COL 76.43 RIGHT-ALIGNED WIDGET-ID 82
     RECT-1 AT ROW 21.73 COL 61.57 WIDGET-ID 26
     RECT-2 AT ROW 2.88 COL 2 WIDGET-ID 38
     RECT-3 AT ROW 13.65 COL 2 WIDGET-ID 42
     RECT-4 AT ROW 2.88 COL 62 WIDGET-ID 68
     RECT-6 AT ROW 9.08 COL 81 WIDGET-ID 102
     RECT-7 AT ROW 11.42 COL 63 WIDGET-ID 108
     RECT-8 AT ROW 7.58 COL 63 WIDGET-ID 110
     RECT-9 AT ROW 19.92 COL 37 WIDGET-ID 112
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.86 BY 23 WIDGET-ID 100.


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
         WIDTH              = 103.86
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
/* SETTINGS FOR TOGGLE-BOX chkContabilizado IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbAgencia IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbCentroDeCostos IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbTipoActivo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fechaAvaluo IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR RADIO-SET rsEstado IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tgDepreciable IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       txtAnotaciones:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtFechaDeCompra IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtFecUltDep IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       txtFecUltDep:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtId IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       txtId:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtMarca IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtMesesA_Depreciar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtNitProveedor IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtNombre IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtNumFactura IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtProveedor IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       txtProveedor:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtResponsable IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtSerial IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtUbicacion IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtValorActual IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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

    RELEASE activosFijos.

    DO cont = 1 to cmbCentroDeCostos:NUM-ITEMS:
        cmbCentroDeCostos:DELETE(cont).
    END.

    W_Ok = cmbCentroDeCostos:ADD-LAST("").

    cmbAgencia:SENSITIVE = TRUE.
    cmbAgencia:SCREEN-VALUE = cmbAgencia:ENTRY(1).
    cmbTipoActivo:SENSITIVE = TRUE.
    CmbTipoActivo:SCREEN-VALUE = CmbTipoActivo:ENTRY(1).
    txtNombre:SENSITIVE = TRUE.
    txtNombre:SCREEN-VALUE = "".
    txtMarca:SENSITIVE = TRUE.
    txtMarca:SCREEN-VALUE = "".
    txtSerial:SENSITIVE = TRUE.
    txtSerial:SCREEN-VALUE = "".
    txtId:SENSITIVE = TRUE.
    txtId:SCREEN-VALUE = "".
    txtUbicacion:SENSITIVE = TRUE.
    txtUbicacion:SCREEN-VALUE = "".
    txtResponsable:SENSITIVE = TRUE.
    txtResponsable:SCREEN-VALUE = "".
    txtFechaDeCompra:SENSITIVE = TRUE.
    txtFechaDeCompra:SCREEN-VALUE = "".
    txtNumFactura:SENSITIVE = TRUE.
    txtNumFactura:SCREEN-VALUE = "".
    txtNitProveedor:SENSITIVE = TRUE.
    txtNitProveedor:SCREEN-VALUE = "".
    txtProveedor:SCREEN-VALUE = "".
    txtValorCompra:SENSITIVE = TRUE.
    txtValorCompra:SCREEN-VALUE = "0.00".
    avaluo:SENSITIVE = FALSE.
    avaluo:SCREEN-VALUE = "0.00".
    fechaAvaluo:SENSITIVE = FALSE.
    fechaAvaluo:SCREEN-VALUE = "".
    cmbCentroDeCostos:SENSITIVE = TRUE.
    cmbCentroDeCostos:SCREEN-VALUE = cmbCentroDeCostos:ENTRY(1).
    tgDepreciable:SENSITIVE = TRUE.
    tgDepreciable:SCREEN-VALUE = "yes".
    chkContabilizado:SCREEN-VALUE = "no".
    txtMesesA_Depreciar:SENSITIVE = TRUE.
    txtMesesA_Depreciar:SCREEN-VALUE = "0".
    txtValorDepreciado:SENSITIVE = FALSE.
    txtValorDepreciado:SCREEN-VALUE = "0.00".
    txtValorActual:SENSITIVE = FALSE.
    txtValorActual:SCREEN-VALUE = "0.00".
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

        FIND FIRST varios WHERE varios.tipo = 7 AND varios.codigo = activosFijos.tipoActivo NO-LOCK NO-ERROR.
        CmbTipoActivo:SCREEN-VALUE = STRING(varios.codigo,"99") + " - " + varios.descripcion.
        CmbTipoActivo:SENSITIVE = FALSE.

        txtNombre:SCREEN-VALUE = activosFijos.nombre.
        txtNombre:SENSITIVE = FALSE.
        txtMarca:SCREEN-VALUE = activosFijos.marca.
        txtMarca:SENSITIVE = FALSE.
        txtSerial:SCREEN-VALUE = activosFijos.serial.
        txtSerial:SENSITIVE = FALSE.
        txtId:SCREEN-VALUE = activosFijos.idActivo.
        txtId:SENSITIVE = TRUE.
        txtUbicacion:SCREEN-VALUE = activosFijos.ubicacion.
        txtUbicacion:SENSITIVE = FALSE.
        txtResponsable:SCREEN-VALUE = activosFijos.responsable.
        txtResponsable:SENSITIVE = FALSE.
        txtFechaDeCompra:SCREEN-VALUE = STRING(activosFijos.fechaCompra,"99/99/9999").
        txtFechaDeCompra:SENSITIVE = FALSE.
        txtNumFactura:SCREEN-VALUE = activosFijos.numFactura.
        txtNumFactura:SENSITIVE = FALSE.
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

        DO cont = 1 to cmbCentroDeCostos:NUM-ITEMS:
            cmbCentroDeCostos:DELETE(cont).
        END.

        W_Ok = cmbCentroDeCostos:ADD-LAST("").
        
        FOR EACH cen_costos WHERE cen_costos.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                              AND cen_costos.estado = 1 NO-LOCK:
           W_Ok = cmbCentroDeCostos:ADD-LAST(STRING(cen_costos.cen_costos,"999") + " - " + cen_costos.Nombre).
        END.

        FIND FIRST cen_costos WHERE cen_costos.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                                AND cen_costos.cen_costos = activosFijos.cen_costos NO-LOCK NO-ERROR.
        
        cmbCentroDeCostos:SCREEN-VALUE = STRING(cen_costos.cen_costos,"999") + " - " + cen_costos.nombre.
        cmbCentroDeCostos:SENSITIVE = FALSE.

        tgDepreciable:SCREEN-VALUE = STRING(activosFijos.depreciable).
        tgDepreciable:SENSITIVE = FALSE.
        chkContabilizado:SCREEN-VALUE = STRING(activosFijos.contabilizado).
        txtMesesA_Depreciar:SCREEN-VALUE = STRING(activosFijos.mesesDepreciar,">>>9").
        txtMesesA_Depreciar:SENSITIVE = FALSE.
        txtValorDepreciado:SCREEN-VALUE = STRING(activosFijos.valorDepreciado,">>>,>>>,>>>,>>9.99").
        txtValorDepreciado:SENSITIVE = FALSE.
        txtValorActual:SCREEN-VALUE = STRING(activosFijos.valorActual,">>>,>>>,>>>,>>9.99").
        txtValorActual:SENSITIVE = FALSE.
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
        cmbTipoActivo:SENSITIVE = FALSE.
        CmbTipoActivo:SCREEN-VALUE = "".
        txtNombre:SENSITIVE = FALSE.
        txtNombre:SCREEN-VALUE = "".
        txtMarca:SENSITIVE = FALSE.
        txtMarca:SCREEN-VALUE = "".
        txtSerial:SENSITIVE = FALSE.
        txtSerial:SCREEN-VALUE = "".
        txtId:SENSITIVE = FALSE.
        txtId:SCREEN-VALUE = "".
        txtUbicacion:SENSITIVE = FALSE.
        txtUbicacion:SCREEN-VALUE = "".
        txtResponsable:SENSITIVE = FALSE.
        txtResponsable:SCREEN-VALUE = "".
        txtFechaDeCompra:SENSITIVE = FALSE.
        txtFechaDeCompra:SCREEN-VALUE = "".
        txtNumFactura:SENSITIVE = FALSE.
        txtNumFactura:SCREEN-VALUE = "".
        txtNitProveedor:SENSITIVE = FALSE.
        txtNitProveedor:SCREEN-VALUE = "".
        txtProveedor:SCREEN-VALUE = "".
        txtValorCompra:SENSITIVE = FALSE.
        txtValorCompra:SCREEN-VALUE = "0.00".
        cmbCentroDeCostos:SENSITIVE = FALSE.
        cmbCentroDeCostos:SCREEN-VALUE = "".
        tgDepreciable:SENSITIVE = FALSE.
        tgDepreciable:SCREEN-VALUE = "yes".
        chkContabilizado:SCREEN-VALUE = "no".
        txtMesesA_Depreciar:SENSITIVE = FALSE.
        txtMesesA_Depreciar:SCREEN-VALUE = "0".
        txtValorDepreciado:SENSITIVE = FALSE.
        txtValorDepreciado:SCREEN-VALUE = "0.00".
        txtValorActual:SENSITIVE = FALSE.
        txtValorActual:SCREEN-VALUE = "0.00".
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
    IF txtId:SCREEN-VALUE = "" THEN DO:
        MESSAGE "El ID para el Activo fijo no se ha cargado de forma correcta."
                "Por favor, informe al Administrador del Sistema."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    cmbAgencia:SENSITIVE = TRUE.
    cmbTipoActivo:SENSITIVE = TRUE.
    txtNombre:SENSITIVE = TRUE.
    txtMarca:SENSITIVE = TRUE.
    txtSerial:SENSITIVE = TRUE.
    txtId:SENSITIVE = TRUE.
    txtUbicacion:SENSITIVE = TRUE.
    txtResponsable:SENSITIVE = TRUE.
    txtFechaDeCompra:SENSITIVE = TRUE.
    txtNumFactura:SENSITIVE = TRUE.
    txtNitProveedor:SENSITIVE = TRUE.
    txtValorCompra:SENSITIVE = FALSE.
    cmbCentroDeCostos:SENSITIVE = TRUE.
    
    IF ActivosFijos.depreciable = NO THEN
        tgDepreciable:SENSITIVE = TRUE.
    
    txtMesesA_Depreciar:SENSITIVE = TRUE.
    txtValorDepreciado:SENSITIVE = FALSE.
    txtValorActual:SENSITIVE = FALSE.
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
        cmbTipoActivo:SENSITIVE = FALSE.
        CmbTipoActivo:SCREEN-VALUE = CmbTipoActivo:ENTRY(1).
        txtNombre:SENSITIVE = FALSE.
        txtNombre:SCREEN-VALUE = "".
        txtMarca:SENSITIVE = FALSE.
        txtMarca:SCREEN-VALUE = "".
        txtSerial:SENSITIVE = FALSE.
        txtSerial:SCREEN-VALUE = "".
        txtId:SENSITIVE = FALSE.
        txtId:SCREEN-VALUE = "".
        txtUbicacion:SENSITIVE = FALSE.
        txtUbicacion:SCREEN-VALUE = "".
        txtResponsable:SENSITIVE = FALSE.
        txtResponsable:SCREEN-VALUE = "".
        txtFechaDeCompra:SENSITIVE = FALSE.
        txtFechaDeCompra:SCREEN-VALUE = "".
        txtNumFactura:SENSITIVE = FALSE.
        txtNumFactura:SCREEN-VALUE = "".
        txtNitProveedor:SENSITIVE = FALSE.
        txtNitProveedor:SCREEN-VALUE = "".
        txtProveedor:SCREEN-VALUE = "".
        txtValorCompra:SENSITIVE = FALSE.
        txtValorCompra:SCREEN-VALUE = "0.00".
        cmbCentroDeCostos:SENSITIVE = FALSE.
        cmbCentroDeCostos:SCREEN-VALUE = cmbCentroDeCostos:ENTRY(1).
        tgDepreciable:SENSITIVE = FALSE.
        tgDepreciable:SCREEN-VALUE = "yes".
        chkContabilizado:SCREEN-VALUE = "no".
        txtMesesA_Depreciar:SENSITIVE = FALSE.
        txtMesesA_Depreciar:SCREEN-VALUE = "0".
        txtValorDepreciado:SENSITIVE = FALSE.
        txtValorDepreciado:SCREEN-VALUE = "0.00".
        txtValorActual:SENSITIVE = FALSE.
        txtValorActual:SCREEN-VALUE = "0.00".
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
    IF cmbTipoActivo:SCREEN-VALUE <> ""     AND cmbTipoActivo:SCREEN-VALUE <> ?     AND
       cmbAgencia:SCREEN-VALUE <> ""        AND cmbAgencia:SCREEN-VALUE <> ?        AND
       txtNombre:SCREEN-VALUE <> ""         AND txtNombre:SCREEN-VALUE <> ?         AND
       txtFechaDeCompra:SCREEN-VALUE <> ""  AND txtFechaDeCompra:SCREEN-VALUE <> ?  AND
       txtNitProveedor:SCREEN-VALUE <> ""   AND txtNitProveedor:SCREEN-VALUE <> ?   AND
       txtValorCompra:SCREEN-VALUE <> ""    AND txtValorCompra:SCREEN-VALUE <> ?    AND DECIMAL(txtValorCompra:SCREEN-VALUE) > 0 AND
       cmbCentroDeCostos:SCREEN-VALUE <> "" AND cmbCentroDeCostos:SCREEN-VALUE <> ? THEN DO:

        IF (INTEGER(SUBSTRING(cmbTipoActivo:SCREEN-VALUE IN FRAME default-frame,1,2)) <> 1 AND 
            INTEGER(SUBSTRING(cmbTipoActivo:SCREEN-VALUE IN FRAME default-frame,1,2)) <> 2 AND
            INTEGER(SUBSTRING(cmbTipoActivo:SCREEN-VALUE IN FRAME default-frame,1,2)) <> 3 AND
            INTEGER(SUBSTRING(cmbTipoActivo:SCREEN-VALUE IN FRAME default-frame,1,2)) <> 4) AND
           (txtMarca:SCREEN-VALUE = ""      OR txtMarca:SCREEN-VALUE = ?   OR
            txtSerial:SCREEN-VALUE = ""     OR txtSerial:SCREEN-VALUE = ?  OR
            txtNumFactura:SCREEN-VALUE = "" OR txtNumFactura:SCREEN-VALUE = ?) THEN DO:
            MESSAGE "Todos los campos deben estar diligenciados." SKIP
                    "Revise la información y vuelva a intentar."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN NO-APPLY.
        END.

        IF tgDepreciable:SCREEN-VALUE = "yes" THEN DO:
            IF txtMesesA_Depreciar:SCREEN-VALUE <> "" THEN
                RUN GrabarActivoFijo.
        END.
        ELSE
            RUN GrabarActivoFijo.

        cmbAgencia:SENSITIVE = FALSE.
        cmbTipoActivo:SENSITIVE = FALSE.
        txtNombre:SENSITIVE = FALSE.
        txtMarca:SENSITIVE = FALSE.
        txtSerial:SENSITIVE = FALSE.
        txtId:SENSITIVE = TRUE.
        txtUbicacion:SENSITIVE = FALSE.
        txtResponsable:SENSITIVE = FALSE.
        txtFechaDeCompra:SENSITIVE = FALSE.
        txtNumFactura:SENSITIVE = FALSE.
        txtNitProveedor:SENSITIVE = FALSE.
        txtValorCompra:SENSITIVE = FALSE.
        cmbCentroDeCostos:SENSITIVE = FALSE.
        tgDepreciable:SENSITIVE = FALSE.
        txtMesesA_Depreciar:SENSITIVE = FALSE.
        txtValorDepreciado:SENSITIVE = FALSE.
        txtValorActual:SENSITIVE = FALSE.
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


&Scoped-define SELF-NAME cmbAgencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbAgencia C-Win
ON VALUE-CHANGED OF cmbAgencia IN FRAME DEFAULT-FRAME /* Agencia */
DO:
    DEFINE VAR cont AS INTEGER.

    IF cmbAgencia:SCREEN-VALUE <> "" THEN DO:
        DO cont = 1 to cmbCentroDeCostos:NUM-ITEMS:
            cmbCentroDeCostos:DELETE(cont).
        END.

        W_Ok = cmbCentroDeCostos:ADD-LAST("").

        FOR EACH cen_costos WHERE cen_costos.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                              AND cen_costos.estado = 1 NO-LOCK:
           W_Ok = cmbCentroDeCostos:ADD-LAST(STRING(cen_costos.cen_costos,"999") + " - " + cen_costos.Nombre).
        END.
    END.
    
    FIND FIRST tt_agencias WHERE tt_agencias.cod_agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.

    APPLY "value-changed" TO cmbCentroDeCostos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCentroDeCostos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCentroDeCostos C-Win
ON VALUE-CHANGED OF cmbCentroDeCostos IN FRAME DEFAULT-FRAME /* Centro de Costos */
DO:
    IF cmbTipoActivo:SCREEN-VALUE <> "" AND cmbTipoActivo:SCREEN-VALUE <> ? AND
       cmbAgencia:SCREEN-VALUE <> "" AND cmbAgencia:SCREEN-VALUE <> ? AND
       cmbCentroDeCostos:SCREEN-VALUE <> "" AND cmbCentroDeCostos:SCREEN-VALUE <> ? THEN DO:
        IF AVAILABLE ActivosFijos THEN DO:
            IF activosFijos.depreciable = NO THEN
                tgDepreciable:SENSITIVE = TRUE.
        END.

        FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = INTEGER(SUBSTRING(cmbTipoActivo:SCREEN-VALUE,1,2)) NO-ERROR.
        IF AVAILABLE cfg_activosFijos THEN DO:
            FIND FIRST tt_agencias WHERE tt_agencias.cod_agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
            txtId:SCREEN-VALUE = tt_agencias.prefijo + SUBSTRING(cmbCentroDeCostos:SCREEN-VALUE,1,3) + SUBSTRING(cmbTipoActivo:SCREEN-VALUE,1,2) + STRING(cfg_ActivosFijos.consecutivo + 1,"99999").
        END.
        ELSE DO:
            MESSAGE "No existe la configuración para este tipo de Activos." SKIP
                    "Proceda primero a realizar la configuración contable" SKIP
                    "antes de ingresar Activos de este tipo."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.

    END.
    /*ELSE
        tgDepreciable:SENSITIVE = FALSE.*/
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbTipoActivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbTipoActivo C-Win
ON VALUE-CHANGED OF cmbTipoActivo IN FRAME DEFAULT-FRAME /* Tipo Activo */
DO:
    APPLY "value-changed" TO cmbCentroDeCostos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgDepreciable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgDepreciable C-Win
ON VALUE-CHANGED OF tgDepreciable IN FRAME DEFAULT-FRAME /* Depreciable */
DO:
    IF tgDepreciable:SCREEN-VALUE = "yes" THEN DO:
        txtMesesA_Depreciar:SENSITIVE = TRUE.
    END.
    ELSE
        txtMesesA_Depreciar:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtNitProveedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtNitProveedor C-Win
ON LEAVE OF txtNitProveedor IN FRAME DEFAULT-FRAME /* Nit Proveedor */
DO:
    FIND FIRST clientes WHERE clientes.nit = txtNitProveedor:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN
        txtProveedor:SCREEN-VALUE = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
    ELSE DO:
        MESSAGE "Este Proveedor no se encuentra matriculado en el Sistema." SKIP
                "Debe primero realizar esta operación antes de poder utilizarlo" SKIP
                "en el módulo de Activos Fijos."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        txtProveedor:SCREEN-VALUE = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtResponsable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtResponsable C-Win
ON VALUE-CHANGED OF txtResponsable IN FRAME DEFAULT-FRAME /* Responsable */
DO:
    IF cmbTipoActivo:SCREEN-VALUE <> "" AND
       cmbAgencia:SCREEN-VALUE <> "" AND
       txtNombre:SCREEN-VALUE <> "" AND
       txtMarca:SCREEN-VALUE <> "" AND
       txtSerial:SCREEN-VALUE <> "" THEN
        txtFechaDeCompra:SENSITIVE = TRUE.
    ELSE
        txtFechaDeCompra:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtSerial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtSerial C-Win
ON VALUE-CHANGED OF txtSerial IN FRAME DEFAULT-FRAME /* Serial # */
DO:
    IF cmbTipoActivo:SCREEN-VALUE <> "" AND
       cmbAgencia:SCREEN-VALUE <> "" AND
       txtNombre:SCREEN-VALUE <> "" AND
       txtMarca:SCREEN-VALUE <> "" AND
       txtSerial:SCREEN-VALUE <> "" THEN
        txtFechaDeCompra:SENSITIVE = TRUE.
    ELSE
        txtFechaDeCompra:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtUbicacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtUbicacion C-Win
ON VALUE-CHANGED OF txtUbicacion IN FRAME DEFAULT-FRAME /* Ubicación */
DO:
    IF cmbTipoActivo:SCREEN-VALUE <> "" AND
       cmbAgencia:SCREEN-VALUE <> "" AND
       txtNombre:SCREEN-VALUE <> "" AND
       txtMarca:SCREEN-VALUE <> "" AND
       txtSerial:SCREEN-VALUE <> "" THEN
        txtFechaDeCompra:SENSITIVE = TRUE.
    ELSE
        txtFechaDeCompra:SENSITIVE = FALSE.
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

    /* Se llena el combo de agencias */
    W_Ok = cmbAgencia:ADD-LAST("").

    FOR EACH Agencias WHERE Agencias.Estado = 1 NO-LOCK:
        W_Ok = CmbAgencia:ADD-LAST(STRING(Agencias.Agencia,"99") + " - " + Agencias.Nombre).
    END.

    /* Se llena el combo de tipo de activo */
    W_Ok = CmbTipoActivo:ADD-LAST("").

    FOR EACH varios WHERE varios.tipo = 7
                      AND varios.estado = 1 NO-LOCK:
        W_Ok = CmbTipoActivo:ADD-LAST(STRING(varios.codigo,"99") + " - " + varios.descripcion).
    END.

    /* Llenar la tabla temporal para los prefijos de los idActivos con las iniciales de las agencias */
    FOR EACH agencias NO-LOCK:
        CREATE tt_agencias.
        tt_agencias.cod_agencia = agencias.agencia.
        tt_agencias.prefijo = SUBSTRING(agencias.nombre,1,3).
    END.

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
  DISPLAY tgDepreciable cmbAgencia txtMesesA_Depreciar cmbCentroDeCostos 
          txtValorDepreciado cmbTipoActivo txtNombre txtFecUltDep txtMarca 
          txtValorActual txtSerial txtId rsEstado txtUbicacion txtResponsable 
          avaluo fechaAvaluo txtFechaDeCompra txtNumFactura txtNitProveedor 
          txtAnotaciones txtProveedor txtValorCompra chkContabilizado 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 RECT-6 RECT-7 RECT-8 RECT-9 txtId 
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
    clientes.nit = txtId:SCREEN-VALUE.
    clientes.nombre = txtNombre:SCREEN-VALUE.
    clientes.fec_ingreso = w_fecha.
    clientes.fec_ultActualiza = w_fecha.
    clientes.tipo_vinculo = 6.
    clientes.usuario = w_usuario.
END.
ELSE DO:
    FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = INTEGER(SUBSTRING(cmbTipoActivo:SCREEN-VALUE IN FRAME default-frame,1,2)) NO-LOCK NO-ERROR.

    vAgencia = activosFijos.agencia.
    vTipoActivo = activosFijos.tipoActivo.
    vCenCostos = activosFijos.cen_costos.
    vIdActivo = activosFijos.idActivo.
    vValorizacion = activosFijos.valorizacion.

    IF vIdActivo <> txtId:SCREEN-VALUE THEN DO:
        CREATE clientes.
        clientes.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE IN FRAME default-frame,1,2)).
        clientes.nit = txtId:SCREEN-VALUE.
        clientes.nombre = txtNombre:SCREEN-VALUE.
        clientes.fec_ingreso = w_fecha.
        clientes.fec_ultActualiza = w_fecha.
        clientes.tipo_vinculo = 6.
        clientes.usuario = w_usuario.
    END.
END.

activosFijos.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE IN FRAME default-frame,1,2)).
activosFijos.tipoActivo = INTEGER(SUBSTRING(cmbTipoActivo:SCREEN-VALUE IN FRAME default-frame,1,2)).
activosFijos.nombre = txtNombre:SCREEN-VALUE.
activosFijos.marca = txtMarca:SCREEN-VALUE.
activosFijos.cen_costos = INTEGER(SUBSTRING(cmbCentroDeCostos:SCREEN-VALUE IN FRAME default-frame,1,3)).

IF tgDepreciable:SCREEN-VALUE = "yes" THEN
    activosFijos.depreciable = TRUE.
ELSE
    activosFijos.depreciable = FALSE.

activosFijos.estado = 1.
activosFijos.fechaCompra = DATE(txtFechaDeCompra:SCREEN-VALUE).
activosFijos.idActivo = txtId:SCREEN-VALUE.
activosFijos.mesesDepreciar = INTEGER(txtMesesA_Depreciar:SCREEN-VALUE).
activosFijos.nitProveedor = txtNitProveedor:SCREEN-VALUE.
activosFijos.nombre = txtNombre:SCREEN-VALUE.
activosFijos.numFactura = txtNumFactura:SCREEN-VALUE.
activosFijos.serial = txtSerial:SCREEN-VALUE.
activosFijos.ubicacion = txtUbicacion:SCREEN-VALUE.
activosFijos.responsable = txtResponsable:SCREEN-VALUE.
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

