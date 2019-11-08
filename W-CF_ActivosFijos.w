&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR codAgencia AS INTEGER.
DEFINE VAR rowIdCfgActivo AS ROWID.

/* Para almacenar los valores actuales del activo, y comparar contra lo que guarde, */
/* para el caso en que se realice alguna modificación. */
DEFINE VAR vAgencia AS INTEGER.
DEFINE VAR vTipoActivo AS INTEGER.
DEFINE VAR vId AS CHARACTER.
DEFINE VAR vValorActual AS DECIMAL.
DEFINE VAR vMesesDepreciar AS INTEGER.
/* ----------------------- */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 btnBuscar btnAgregar btnSalir 
&Scoped-Define DISPLAYED-OBJECTS cmbTipoActivo activoFijo txtActivoFijo ~
Gasto txtGasto Depreciacion txtDepreciacion CxP txtCxP ingreso_xAvaluo ~
txtIngreso_xAvaluo 

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

DEFINE VARIABLE cmbTipoActivo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo Activo" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 38.86 BY 1 NO-UNDO.

DEFINE VARIABLE activoFijo AS CHARACTER FORMAT "X(14)":U 
     LABEL "Activo Fijo" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .92 NO-UNDO.

DEFINE VARIABLE CxP AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta x Pagar" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .92 NO-UNDO.

DEFINE VARIABLE Depreciacion AS CHARACTER FORMAT "X(14)":U 
     LABEL "Depreciación" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .92 NO-UNDO.

DEFINE VARIABLE Gasto AS CHARACTER FORMAT "X(14)":U 
     LABEL "Gasto" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .92 NO-UNDO.

DEFINE VARIABLE ingreso_xAvaluo AS CHARACTER FORMAT "X(14)":U 
     LABEL "Ingreso x Avalúo" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .92 NO-UNDO.

DEFINE VARIABLE txtActivoFijo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .92
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE txtCxP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .92
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE txtDepreciacion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .92
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE txtGasto AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .92
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE txtIngreso_xAvaluo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .92
     BGCOLOR 3  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 2.15.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbTipoActivo AT ROW 3.42 COL 14.86 COLON-ALIGNED WIDGET-ID 28
     activoFijo AT ROW 5.5 COL 8.71 WIDGET-ID 30
     txtActivoFijo AT ROW 5.5 COL 44.57 NO-LABEL WIDGET-ID 88
     Gasto AT ROW 6.42 COL 12.86 WIDGET-ID 114
     txtGasto AT ROW 6.42 COL 44.57 NO-LABEL WIDGET-ID 116
     Depreciacion AT ROW 7.35 COL 6.14 WIDGET-ID 118
     txtDepreciacion AT ROW 7.35 COL 44.57 NO-LABEL WIDGET-ID 120
     CxP AT ROW 8.27 COL 4.14 WIDGET-ID 122
     txtCxP AT ROW 8.27 COL 44.57 NO-LABEL WIDGET-ID 128
     ingreso_xAvaluo AT ROW 9.19 COL 2.85 WIDGET-ID 130
     txtIngreso_xAvaluo AT ROW 9.19 COL 44.57 NO-LABEL WIDGET-ID 132
     btnBuscar AT ROW 10.88 COL 60.72 WIDGET-ID 12
     btnAgregar AT ROW 10.88 COL 67.57 WIDGET-ID 14
     btnEliminar AT ROW 10.88 COL 74.43 WIDGET-ID 22
     btnEditar AT ROW 10.88 COL 81.14 WIDGET-ID 16
     btnGuardar AT ROW 10.88 COL 88 WIDGET-ID 18
     btnSalir AT ROW 10.88 COL 94.86 WIDGET-ID 20
     " Datos Básicos" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 4.73 COL 3 WIDGET-ID 40
     "                 CONFIGURACIÓN CONTABLE PARA LOS ACTIVOS FIJOS" VIEW-AS TEXT
          SIZE 101.29 BY 1.08 AT ROW 1.27 COL 1.72 WIDGET-ID 8
          BGCOLOR 3 
     RECT-1 AT ROW 10.69 COL 59.72 WIDGET-ID 26
     RECT-2 AT ROW 5.04 COL 2 WIDGET-ID 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.86 BY 12.12 WIDGET-ID 100.


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
         HEIGHT             = 12.12
         WIDTH              = 102.86
         MAX-HEIGHT         = 36.62
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 36.62
         VIRTUAL-WIDTH      = 182.86
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
/* SETTINGS FOR FILL-IN activoFijo IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR BUTTON btnEditar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnEliminar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnGuardar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbTipoActivo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CxP IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN Depreciacion IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN Gasto IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN ingreso_xAvaluo IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtActivoFijo IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       txtActivoFijo:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtCxP IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       txtCxP:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtDepreciacion IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       txtDepreciacion:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtGasto IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       txtGasto:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtIngreso_xAvaluo IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       txtIngreso_xAvaluo:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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


&Scoped-define SELF-NAME activoFijo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL activoFijo C-Win
ON LEAVE OF activoFijo IN FRAME DEFAULT-FRAME /* Activo Fijo */
DO:
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR vNombreCuenta AS CHARACTER.
    DEFINE VAR vNaturalezaCuenta AS CHARACTER.
    DEFINE VAR vCtrNaturalezaCuenta AS LOG.
    DEFINE VAR vTipoCuenta AS CHARACTER INITIAL "M".

    IF activoFijo:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = activoFijo:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cuentas THEN DO:
            RUN C-Cuentas.r (OUTPUT vCuenta,
                             OUTPUT vNombreCuenta,
                             OUTPUT vNaturalezaCuenta,
                             OUTPUT vCtrNaturalezaCuenta,
                             INPUT vTipoCuenta).

            activoFijo:SCREEN-VALUE = vCuenta.
            FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
        END.

        IF NOT AVAILABLE cuentas THEN DO:
            MESSAGE "Cuenta inexistente"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            APPLY "entry" TO activoFijo.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL activoFijo C-Win
ON MOUSE-SELECT-DBLCLICK OF activoFijo IN FRAME DEFAULT-FRAME /* Activo Fijo */
DO:
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR vNombreCuenta AS CHARACTER.
    DEFINE VAR vNaturalezaCuenta AS CHARACTER.
    DEFINE VAR vCtrNaturalezaCuenta AS LOG.
    DEFINE VAR vTipoCuenta AS CHARACTER INITIAL "M".

    FIND FIRST cuentas WHERE cuentas.cuenta = activoFijo:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN DO:
        RUN C-Cuentas.r (OUTPUT vCuenta,
                         OUTPUT vNombreCuenta,
                         OUTPUT vNaturalezaCuenta,
                         OUTPUT vCtrNaturalezaCuenta,
                         INPUT vTipoCuenta).

        activoFijo:SCREEN-VALUE = vCuenta.
        FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
    END.

    IF NOT AVAILABLE cuentas THEN DO:
        MESSAGE "Cuenta inexistente"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "entry" TO activoFijo.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL activoFijo C-Win
ON VALUE-CHANGED OF activoFijo IN FRAME DEFAULT-FRAME /* Activo Fijo */
DO:
    FIND FIRST cuentas WHERE cuentas.cuenta = activoFijo:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        txtActivoFijo:SCREEN-VALUE = cuentas.nombre.
    END.
    ELSE
        txtActivoFijo:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAgregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAgregar C-Win
ON CHOOSE OF btnAgregar IN FRAME DEFAULT-FRAME /* Button 231 */
DO:
    RELEASE cfg_activosFijos.

    cmbTipoActivo:SENSITIVE = TRUE.
    cmbTipoACtivo:SCREEN-VALUE = cmbTipoActivo:ENTRY(1).

    activoFijo:SENSITIVE = TRUE.
    activoFijo:SCREEN-VALUE = "".
    txtActivoFijo:SCREEN-VALUE = "".
    
    gasto:SENSITIVE = TRUE.
    gasto:SCREEN-VALUE = "".
    txtGasto:SCREEN-VALUE = "".
    
    depreciacion:SENSITIVE = TRUE.
    depreciacion:SCREEN-VALUE = "".
    txtDepreciacion:SCREEN-VALUE = "".

    CxP:SENSITIVE = TRUE.
    CxP:SCREEN-VALUE = "".
    txtCxP:SCREEN-VALUE = "".

    ingreso_xAvaluo:SENSITIVE = TRUE.
    ingreso_xAvaluo:SCREEN-VALUE = "".
    txtIngreso_xAvaluo:SCREEN-VALUE = "".

    btnAgregar:SENSITIVE = FALSE.
    btnGuardar:SENSITIVE = TRUE.
    btnEditar:SENSITIVE = FALSE.
    btnEliminar:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBuscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBuscar C-Win
ON CHOOSE OF btnBuscar IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    /*ASSIGN C-Win:SENSITIVE = FALSE.*/

    RUN C-cfgActivosFijos.R(OUTPUT rowIdCfgActivo).

    ASSIGN C-Win:SENSITIVE = TRUE.

    FIND FIRST cfg_activosFijos WHERE ROWID(cfg_activosFijos) = rowIdCfgActivo NO-ERROR.
    IF AVAILABLE cfg_activosFijos THEN DO:
        FIND FIRST varios WHERE varios.tipo = 7 AND varios.codigo = cfg_activosFijos.tipoActivo NO-LOCK NO-ERROR.
        CmbTipoActivo:SCREEN-VALUE = STRING(varios.codigo,"99") + " - " + varios.descripcion.
        CmbTipoActivo:SENSITIVE = FALSE.

        activoFijo:SCREEN-VALUE = cfg_activosfijos.activoFijo.
        APPLY "value-changed" TO activoFijo.
        activoFijo:SENSITIVE = FALSE.

        gasto:SCREEN-VALUE = cfg_activosFijos.gasto.
        APPLY "value-changed" TO gasto.
        gasto:SENSITIVE = FALSE.

        depreciacion:SCREEN-VALUE = cfg_activosFijos.depreciacion.
        APPLY "value-changed" TO depreciacion.
        depreciacion:SENSITIVE = FALSE.

        CxP:SCREEN-VALUE = cfg_activosfijos.CxP.
        APPLY "value-changed" TO CxP.
        CxP:SENSITIVE = FALSE.

        ingreso_xAvaluo:SCREEN-VALUE = cfg_activosfijos.ingreso_xAvaluo.
        APPLY "value-changed" TO ingreso_xAvaluo.
        ingreso_xAvaluo:SENSITIVE = FALSE.

        btnEditar:SENSITIVE = TRUE.
        btnEliminar:SENSITIVE = TRUE.
        btnAgregar:SENSITIVE = TRUE.
    END.
    ELSE DO:
        cmbTipoActivo:SENSITIVE = FALSE.
        CmbTipoActivo:SCREEN-VALUE = cmbTipoActivo:ENTRY(1).
        
        activoFijo:SENSITIVE = FALSE.
        activoFijo:SCREEN-VALUE = "".
        txtActivoFijo:SCREEN-VALUE = "".

        gasto:SENSITIVE = FALSE.
        gasto:SCREEN-VALUE = "".
        txtGasto:SCREEN-VALUE = "".

        depreciacion:SENSITIVE = FALSE.
        depreciacion:SCREEN-VALUE = "".
        txtDepreciacion:SCREEN-VALUE = "".

        CxP:SENSITIVE = FALSE.
        CxP:SCREEN-VALUE = "".
        txtCxP:SCREEN-VALUE = "".

        ingreso_xAvaluo:SENSITIVE = FALSE.
        ingreso_xAvaluo:SCREEN-VALUE = "".
        txtIngreso_xAvaluo:SCREEN-VALUE = "".

        btnGuardar:SENSITIVE = FALSE.
        btnEditar:SENSITIVE = FALSE.
        btnEliminar:SENSITIVE = FALSE.
        btnAgregar:SENSITIVE = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEditar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEditar C-Win
ON CHOOSE OF btnEditar IN FRAME DEFAULT-FRAME /* btnagregar 2 */
DO:
    cmbTipoActivo:SENSITIVE = FALSE.

    activoFijo:SENSITIVE = TRUE.
    gasto:SENSITIVE = TRUE.
    depreciacion:SENSITIVE = TRUE.
    CxP:SENSITIVE = TRUE.
    ingreso_xAvaluo:SENSITIVE = TRUE.
    
    btnGuardar:SENSITIVE = TRUE.
    btnEditar:SENSITIVE = FALSE.
    btnEliminar:SENSITIVE = TRUE.
    btnAgregar:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEliminar C-Win
ON CHOOSE OF btnEliminar IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    DEFINE VAR flagBorrar AS LOGICAL.
    
    MESSAGE "Está seguro que desea eliminar esta configuración?"
        VIEW-AS ALERT-BOX BUTTONS YES-NO SET flagBorrar.

    FIND FIRST activosFijos WHERE activosFijos.tipoActivo = cfg_activosFijos.tipo
                              AND activosFijos.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE activosFijos THEN DO:
        MESSAGE "No se permite el borrado de esta configuración," SKIP
                "ya que existen Activos Fijos registrados y" SKIP
                "en estado activo. Revise por favor..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.
    
    IF flagBorrar = TRUE THEN DO:
        /* Falta hacer la contabilidad respectiva */
        DELETE cfg_activosFijos.

        cmbTipoActivo:SENSITIVE = FALSE.
        CmbTipoActivo:SCREEN-VALUE = cmbTipoActivo:ENTRY(1).
        
        activoFijo:SENSITIVE = FALSE.
        activoFijo:SCREEN-VALUE = "".
        txtActivoFijo:SCREEN-VALUE = "".

        gasto:SENSITIVE = FALSE.
        gasto:SCREEN-VALUE = "".
        txtGasto:SCREEN-VALUE = "".

        depreciacion:SENSITIVE = FALSE.
        depreciacion:SCREEN-VALUE = "".
        txtDepreciacion:SCREEN-VALUE = "".

        CxP:SENSITIVE = FALSE.
        CxP:SCREEN-VALUE = "".
        txtCxP:SCREEN-VALUE = "".

        ingreso_xAvaluo:SENSITIVE = FALSE.
        ingreso_xAvaluo:SCREEN-VALUE = "".
        txtIngreso_xAvaluo:SCREEN-VALUE = "".
        
        btnGuardar:SENSITIVE = FALSE.
        btnEditar:SENSITIVE = FALSE.
        btnEliminar:SENSITIVE = FALSE.
        btnAgregar:SENSITIVE = TRUE.

        MESSAGE "La configuración fue eliminada con éxito..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGuardar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGuardar C-Win
ON CHOOSE OF btnGuardar IN FRAME DEFAULT-FRAME /* btnagregar 2 */
DO:
    IF cmbTipoActivo:SCREEN-VALUE <> "" AND cmbTipoActivo:SCREEN-VALUE <> ? AND
       activoFijo:SCREEN-VALUE <> ""    AND activoFijo:SCREEN-VALUE <> ? THEN
        RUN GrabarConfiguracion.
    ELSE
        MESSAGE "Los campos Tipo Activo y Activo Fijo deben estar diligenciados." SKIP
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


&Scoped-define SELF-NAME cmbTipoActivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbTipoActivo C-Win
ON VALUE-CHANGED OF cmbTipoActivo IN FRAME DEFAULT-FRAME /* Tipo Activo */
DO:
    FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = INTEGER(SUBSTRING(cmbTipoActivo:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
    IF AVAILABLE cfg_activosFijos THEN DO:
        activoFijo:SCREEN-VALUE = cfg_activosfijos.activoFijo.
        APPLY "value-changed" TO activoFijo.
        gasto:SCREEN-VALUE = cfg_activosFijos.gasto.
        APPLY "value-changed" TO gasto.
        depreciacion:SCREEN-VALUE = cfg_activosFijos.depreciacion.
        APPLY "value-changed" TO depreciacion.

        btnEditar:SENSITIVE = TRUE.
        btnEliminar:SENSITIVE = TRUE.
        btnAgregar:SENSITIVE = TRUE.
        btnGuardar:SENSITIVE = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CxP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CxP C-Win
ON LEAVE OF CxP IN FRAME DEFAULT-FRAME /* Cuenta x Pagar */
DO:
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR vNombreCuenta AS CHARACTER.
    DEFINE VAR vNaturalezaCuenta AS CHARACTER.
    DEFINE VAR vCtrNaturalezaCuenta AS LOG.
    DEFINE VAR vTipoCuenta AS CHARACTER INITIAL "M".

    IF CxP:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = CxP:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cuentas THEN DO:
            RUN C-Cuentas.r (OUTPUT vCuenta,
                             OUTPUT vNombreCuenta,
                             OUTPUT vNaturalezaCuenta,
                             OUTPUT vCtrNaturalezaCuenta,
                             INPUT vTipoCuenta).

            CxP:SCREEN-VALUE = vCuenta.
            FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
        END.

        IF NOT AVAILABLE cuentas THEN DO:
            MESSAGE "Cuenta inexistente"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            APPLY "entry" TO CxP.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CxP C-Win
ON MOUSE-SELECT-DBLCLICK OF CxP IN FRAME DEFAULT-FRAME /* Cuenta x Pagar */
DO:
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR vNombreCuenta AS CHARACTER.
    DEFINE VAR vNaturalezaCuenta AS CHARACTER.
    DEFINE VAR vCtrNaturalezaCuenta AS LOG.
    DEFINE VAR vTipoCuenta AS CHARACTER INITIAL "M".

    FIND FIRST cuentas WHERE cuentas.cuenta = CxP:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN DO:
        RUN C-Cuentas.r (OUTPUT vCuenta,
                         OUTPUT vNombreCuenta,
                         OUTPUT vNaturalezaCuenta,
                         OUTPUT vCtrNaturalezaCuenta,
                         INPUT vTipoCuenta).

        CxP:SCREEN-VALUE = vCuenta.
        FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
    END.

    IF NOT AVAILABLE cuentas THEN DO:
        MESSAGE "Cuenta inexistente"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "entry" TO CxP.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CxP C-Win
ON VALUE-CHANGED OF CxP IN FRAME DEFAULT-FRAME /* Cuenta x Pagar */
DO:
    FIND FIRST cuentas WHERE cuentas.cuenta = CxP:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        txtCxP:SCREEN-VALUE = cuentas.nombre.
    END.
    ELSE
        txtCxP:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Depreciacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Depreciacion C-Win
ON LEAVE OF Depreciacion IN FRAME DEFAULT-FRAME /* Depreciación */
DO:
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR vNombreCuenta AS CHARACTER.
    DEFINE VAR vNaturalezaCuenta AS CHARACTER.
    DEFINE VAR vCtrNaturalezaCuenta AS LOG.
    DEFINE VAR vTipoCuenta AS CHARACTER INITIAL "M".

    IF depreciacion:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = depreciacion:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cuentas THEN DO:
            RUN C-Cuentas.r (OUTPUT vCuenta,
                             OUTPUT vNombreCuenta,
                             OUTPUT vNaturalezaCuenta,
                             OUTPUT vCtrNaturalezaCuenta,
                             INPUT vTipoCuenta).

            depreciacion:SCREEN-VALUE = vCuenta.
            FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
        END.

        IF NOT AVAILABLE cuentas THEN DO:
            MESSAGE "Cuenta inexistente"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            APPLY "entry" TO depreciacion.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Depreciacion C-Win
ON MOUSE-SELECT-DBLCLICK OF Depreciacion IN FRAME DEFAULT-FRAME /* Depreciación */
DO:
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR vNombreCuenta AS CHARACTER.
    DEFINE VAR vNaturalezaCuenta AS CHARACTER.
    DEFINE VAR vCtrNaturalezaCuenta AS LOG.
    DEFINE VAR vTipoCuenta AS CHARACTER INITIAL "M".

    FIND FIRST cuentas WHERE cuentas.cuenta = depreciacion:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN DO:
        RUN C-Cuentas.r (OUTPUT vCuenta,
                         OUTPUT vNombreCuenta,
                         OUTPUT vNaturalezaCuenta,
                         OUTPUT vCtrNaturalezaCuenta,
                         INPUT vTipoCuenta).

        depreciacion:SCREEN-VALUE = vCuenta.
        FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
    END.

    IF NOT AVAILABLE cuentas THEN DO:
        MESSAGE "Cuenta inexistente"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "entry" TO depreciacion.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Depreciacion C-Win
ON VALUE-CHANGED OF Depreciacion IN FRAME DEFAULT-FRAME /* Depreciación */
DO:
    FIND FIRST cuentas WHERE cuentas.cuenta = depreciacion:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        txtDepreciacion:SCREEN-VALUE = cuentas.nombre.
    END.
    ELSE
        txtDepreciacion:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Gasto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Gasto C-Win
ON LEAVE OF Gasto IN FRAME DEFAULT-FRAME /* Gasto */
DO:
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR vNombreCuenta AS CHARACTER.
    DEFINE VAR vNaturalezaCuenta AS CHARACTER.
    DEFINE VAR vCtrNaturalezaCuenta AS LOG.
    DEFINE VAR vTipoCuenta AS CHARACTER INITIAL "M".

    IF gasto:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = gasto:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cuentas THEN DO:
            RUN C-Cuentas.r (OUTPUT vCuenta,
                             OUTPUT vNombreCuenta,
                             OUTPUT vNaturalezaCuenta,
                             OUTPUT vCtrNaturalezaCuenta,
                             INPUT vTipoCuenta).

            gasto:SCREEN-VALUE = vCuenta.
            FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
        END.

        IF NOT AVAILABLE cuentas THEN DO:
            MESSAGE "Cuenta inexistente"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            APPLY "entry" TO gasto.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Gasto C-Win
ON MOUSE-SELECT-DBLCLICK OF Gasto IN FRAME DEFAULT-FRAME /* Gasto */
DO:
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR vNombreCuenta AS CHARACTER.
    DEFINE VAR vNaturalezaCuenta AS CHARACTER.
    DEFINE VAR vCtrNaturalezaCuenta AS LOG.
    DEFINE VAR vTipoCuenta AS CHARACTER INITIAL "M".

    FIND FIRST cuentas WHERE cuentas.cuenta = gasto:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN DO:
        RUN C-Cuentas.r (OUTPUT vCuenta,
                         OUTPUT vNombreCuenta,
                         OUTPUT vNaturalezaCuenta,
                         OUTPUT vCtrNaturalezaCuenta,
                         INPUT vTipoCuenta).

        gasto:SCREEN-VALUE = vCuenta.
        FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
    END.

    IF NOT AVAILABLE cuentas THEN DO:
        MESSAGE "Cuenta inexistente"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "entry" TO gasto.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Gasto C-Win
ON VALUE-CHANGED OF Gasto IN FRAME DEFAULT-FRAME /* Gasto */
DO:
    FIND FIRST cuentas WHERE cuentas.cuenta = gasto:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        txtGasto:SCREEN-VALUE = cuentas.nombre.
    END.
    ELSE
        txtGasto:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ingreso_xAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ingreso_xAvaluo C-Win
ON LEAVE OF ingreso_xAvaluo IN FRAME DEFAULT-FRAME /* Ingreso x Avalúo */
DO:
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR vNombreCuenta AS CHARACTER.
    DEFINE VAR vNaturalezaCuenta AS CHARACTER.
    DEFINE VAR vCtrNaturalezaCuenta AS LOG.
    DEFINE VAR vTipoCuenta AS CHARACTER INITIAL "M".

    IF ingreso_xAvaluo:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = ingreso_xAvaluo:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cuentas THEN DO:
            RUN C-Cuentas.r (OUTPUT vCuenta,
                             OUTPUT vNombreCuenta,
                             OUTPUT vNaturalezaCuenta,
                             OUTPUT vCtrNaturalezaCuenta,
                             INPUT vTipoCuenta).

            ingreso_xAvaluo:SCREEN-VALUE = vCuenta.
            FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
        END.

        IF NOT AVAILABLE cuentas THEN DO:
            MESSAGE "Cuenta inexistente"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            APPLY "entry" TO ingreso_xAvaluo.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ingreso_xAvaluo C-Win
ON MOUSE-SELECT-DBLCLICK OF ingreso_xAvaluo IN FRAME DEFAULT-FRAME /* Ingreso x Avalúo */
DO:
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR vNombreCuenta AS CHARACTER.
    DEFINE VAR vNaturalezaCuenta AS CHARACTER.
    DEFINE VAR vCtrNaturalezaCuenta AS LOG.
    DEFINE VAR vTipoCuenta AS CHARACTER INITIAL "M".

    FIND FIRST cuentas WHERE cuentas.cuenta = ingreso_xAvaluo:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN DO:
        RUN C-Cuentas.r (OUTPUT vCuenta,
                         OUTPUT vNombreCuenta,
                         OUTPUT vNaturalezaCuenta,
                         OUTPUT vCtrNaturalezaCuenta,
                         INPUT vTipoCuenta).

        ingreso_xAvaluo:SCREEN-VALUE = vCuenta.
        FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
        IF AVAILABLE cuentas THEN DO:
            txtingreso_xAvaluo:SCREEN-VALUE = cuentas.nombre.
        END.
        ELSE
            txtingreso_xAvaluo:SCREEN-VALUE = "".
    END.

    IF NOT AVAILABLE cuentas THEN DO:
        MESSAGE "Cuenta inexistente"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "entry" TO ingreso_xAvaluo.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ingreso_xAvaluo C-Win
ON VALUE-CHANGED OF ingreso_xAvaluo IN FRAME DEFAULT-FRAME /* Ingreso x Avalúo */
DO:
    FIND FIRST cuentas WHERE cuentas.cuenta = ingreso_xAvaluo:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        txtingreso_xAvaluo:SCREEN-VALUE = cuentas.nombre.
    END.
    ELSE
        txtingreso_xAvaluo:SCREEN-VALUE = "".
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

    /* Se llena el combo de tipo de activo */
    W_Ok = CmbTipoActivo:ADD-LAST("").

    FOR EACH varios WHERE varios.tipo = 7
                      AND varios.estado = 1 NO-LOCK:
        W_Ok = CmbTipoActivo:ADD-LAST(STRING(varios.codigo,"99") + " - " + varios.descripcion).
    END.

    DISABLE txtActivoFijo.

    RUN enable_UI.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY cmbTipoActivo activoFijo txtActivoFijo Gasto txtGasto Depreciacion 
          txtDepreciacion CxP txtCxP ingreso_xAvaluo txtIngreso_xAvaluo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 btnBuscar btnAgregar btnSalir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabarConfiguracion C-Win 
PROCEDURE GrabarConfiguracion :
IF NOT AVAILABLE cfg_activosFijos THEN
    CREATE cfg_activosFijos.

cfg_activosFijos.activoFijo = activoFijo:SCREEN-VALUE IN FRAME default-frame.
cfg_activosFijos.tipoActivo = INTEGER(SUBSTRING(cmbTipoActivo:SCREEN-VALUE,1,3)).
cfg_activosFijos.gasto = gasto:SCREEN-VALUE.
cfg_activosFijos.depreciacion = depreciacion:SCREEN-VALUE.
cfg_activosFijos.CxP = CxP:SCREEN-VALUE.
cfg_activosFijos.ingreso_xAvaluo = ingreso_xAvaluo:SCREEN-VALUE.

MESSAGE "El registro ha sigo Guardado"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

CmbTipoActivo:SENSITIVE = FALSE.
activoFijo:SENSITIVE = FALSE.
gasto:SENSITIVE = FALSE.
depreciacion:SENSITIVE = FALSE.
CxP:SENSITIVE = FALSE.
ingreso_xAvaluo:SENSITIVE = FALSE.

btnEditar:SENSITIVE = TRUE.
btnEliminar:SENSITIVE = TRUE.
btnAgregar:SENSITIVE = TRUE.
btnGuardar:SENSITIVE = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

