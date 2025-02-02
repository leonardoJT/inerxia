&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

/* Local Variable Definitions ---                                       */

DEFINE VAR codFacultad AS CHARACTER.
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR codAgencia AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnBuscar btnAgregar btnSalir 
&Scoped-Define DISPLAYED-OBJECTS cmbAgencia txtFacultad nombreFacultad ~
txtDepartamento nombreDepartamento 

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

DEFINE VARIABLE nombreDepartamento AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE nombreFacultad AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE txtDepartamento AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Departamento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE txtFacultad AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Facultad" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbAgencia AT ROW 2.54 COL 14.14 COLON-ALIGNED WIDGET-ID 24
     txtFacultad AT ROW 3.62 COL 14.14 COLON-ALIGNED WIDGET-ID 2
     nombreFacultad AT ROW 3.62 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     txtDepartamento AT ROW 4.77 COL 14.29 COLON-ALIGNED WIDGET-ID 4
     nombreDepartamento AT ROW 4.77 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     btnBuscar AT ROW 6.31 COL 13.72 WIDGET-ID 12
     btnAgregar AT ROW 6.31 COL 20.57 WIDGET-ID 14
     btnEliminar AT ROW 6.31 COL 27.43 WIDGET-ID 22
     btnEditar AT ROW 6.31 COL 34.14 WIDGET-ID 16
     btnGuardar AT ROW 6.31 COL 41 WIDGET-ID 18
     btnSalir AT ROW 6.31 COL 47.86 WIDGET-ID 20
     "ADMINISTRACION DE FACULTADES Y DEPARTAMENTOS" VIEW-AS TEXT
          SIZE 53.29 BY 1.08 AT ROW 1.27 COL 1.72 WIDGET-ID 8
          BGCOLOR 3 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 55 BY 7.35 WIDGET-ID 100.


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
         TITLE              = "Facultades / Departamentos"
         HEIGHT             = 7.35
         WIDTH              = 55
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* SETTINGS FOR BUTTON btnEditar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnEliminar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnGuardar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbAgencia IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombreDepartamento IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombreFacultad IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtDepartamento IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtFacultad IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Facultades / Departamentos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Facultades / Departamentos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAgregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAgregar C-Win
ON CHOOSE OF btnAgregar IN FRAME DEFAULT-FRAME /* Button 231 */
DO:
    MESSAGE "Se va a crear una nueva Facultad (Haga clic en no en caso que desee agregar un Departamento)?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO TITLE "Crear Facultad/Departamento" UPDATE flagCreaFacultad AS LOGICAL.

    IF flagCreaFacultad = YES THEN DO:
        cmbAgencia:SENSITIVE = TRUE.
        cmbAgencia:SCREEN-VALUE = "".

        txtFacultad:SENSITIVE = FALSE.
        txtFacultad:SCREEN-VALUE = "".
        
        nombreFacultad:SENSITIVE = FALSE.
        nombreFacultad:SCREEN-VALUE = "".

        txtDepartamento:SENSITIVE = FALSE.
        txtDepartamento:SCREEN-VALUE = "".
        nombreDepartamento:SENSITIVE = FALSE.
        nombreDepartamento:SCREEN-VALUE = "".
    END.
    ELSE DO:
        IF INTEGER(txtFacultad:SCREEN-VALUE) > 0 THEN DO:
            cmbAgencia:SENSITIVE = FALSE.
            txtFacultad:SENSITIVE = FALSE.
            nombreFacultad:SENSITIVE = FALSE.
            /*txtDepartamento:SENSITIVE = TRUE.*/
            txtDepartamento:SCREEN-VALUE = "1".
            nombreDepartamento:SENSITIVE = TRUE.
            nombreDepartamento:SCREEN-VALUE = "".

            FOR EACH facultades WHERE facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                                  AND facultades.tipo = "D"
                                  AND SUBSTRING(facultades.codigo,1,2) = txtFacultad:SCREEN-VALUE NO-LOCK BY INTEGER(facultades.codigo) DESCENDING:
                txtDepartamento:SCREEN-VALUE = STRING(INTEGER(SUBSTRING(facultades.codigo,3)) + 1,"999").
                LEAVE.
            END.
        END.
        ELSE DO:
            MESSAGE "Debe primero seleccionar una facultad"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN.
        END.
    END.

    btnGuardar:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBuscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBuscar C-Win
ON CHOOSE OF btnBuscar IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    ASSIGN C-Win:SENSITIVE = FALSE.
    
    RUN C-Facultades.R(OUTPUT codAgencia,
                       OUTPUT codFacultad).

    ASSIGN C-Win:SENSITIVE = TRUE.

    FIND FIRST facultades WHERE facultades.agencia = codAgencia
                            AND facultades.codigo = SUBSTRING(codFacultad,1,2)
                            AND facultades.tipo = "F" NO-LOCK NO-ERROR.
    IF AVAILABLE facultades THEN DO:
        cmbAgencia:SCREEN-VALUE = cmbAgencia:ENTRY(codAgencia).
        txtFacultad:SCREEN-VALUE = facultades.codigo.
        nombreFacultad:SCREEN-VALUE = facultades.nombre.

        FIND FIRST facultades WHERE facultades.agencia = codAgencia
                                AND facultades.codigo = codFacultad
                                AND facultades.tipo = "D" NO-LOCK NO-ERROR.
        IF AVAILABLE facultades THEN DO:
            txtDepartamento:SCREEN-VALUE = SUBSTRING(codFacultad,3).
            nombreDepartamento:SCREEN-VALUE = facultades.nombre.
        END.
        ELSE DO:
            txtDepartamento:SCREEN-VALUE = "".
            nombreDepartamento:SCREEN-VALUE = "".
        END.

        cmbAgencia:SENSITIVE = FALSE.
        txtFacultad:SENSITIVE = FALSE.
        nombreFacultad:SENSITIVE = FALSE.
        txtDepartamento:SENSITIVE = FALSE.
        nombreDepartamento:SENSITIVE = FALSE.
        
        btnEditar:SENSITIVE = TRUE.
        btnEliminar:SENSITIVE = TRUE.
    END.
    ELSE DO:
        cmbAgencia:SENSITIVE = FALSE.
        txtFacultad:SENSITIVE = FALSE.
        nombreFacultad:SENSITIVE = FALSE.
        txtDepartamento:SENSITIVE = FALSE.
        nombreDepartamento:SENSITIVE = FALSE.

        cmbAgencia:SCREEN-VALUE = "".
        txtFacultad:SCREEN-VALUE = "".
        nombreFacultad:SCREEN-VALUE = "".
        txtDepartamento:SCREEN-VALUE = "".
        nombreDepartamento:SCREEN-VALUE = "".

        btnEditar:SENSITIVE = FALSE.
        btnEliminar:SENSITIVE = FALSE.
        btnGuardar:SENSITIVE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEditar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEditar C-Win
ON CHOOSE OF btnEditar IN FRAME DEFAULT-FRAME /* btnagregar 2 */
DO:
    IF txtFacultad:SCREEN-VALUE <> "00" THEN
        nombreFacultad:SENSITIVE = TRUE.

    IF txtDepartamento:SCREEN-VALUE <> "000" THEN
        nombreDepartamento:SENSITIVE = TRUE.

    btnGuardar:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEliminar C-Win
ON CHOOSE OF btnEliminar IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    DEFINE VAR tempFacultad AS CHARACTER.

    FIND FIRST clientes WHERE clientes.facultad = INTEGER(txtFacultad:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        MESSAGE "No se puede eliminar esta facultad ya que existen Clientes que la tienen matriculada"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    FIND FIRST facultades WHERE facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                            AND facultades.tipo = "D"
                            AND SUBSTRING(facultades.codigo,1,2) = txtFacultad:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE facultades AND txtDepartamento:SCREEN-VALUE = "000" THEN DO:
        MESSAGE "No se puede eliminar esta facultad ya que existen Departamentos creados dentro de ella"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    IF txtDepartamento:SCREEN-VALUE <> "000" THEN DO:
        FIND FIRST facultades WHERE facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                                AND facultades.codigo = txtFacultad:SCREEN-VALUE + txtDepartamento:SCREEN-VALUE
                                AND facultades.tipo = "D" NO-ERROR.
        IF AVAILABLE facultades THEN DO:
            DELETE facultades.

            MESSAGE "El Departamento fue borrado con �xito"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            btnEliminar:SENSITIVE = FALSE.
            txtDepartamento:SCREEN-VALUE = "".
            nombreDepartamento:SCREEN-VALUE = "".
        END.
        ELSE
            MESSAGE "No hay Departamento para eliminar"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
        FIND FIRST facultades WHERE facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                                AND facultades.codigo = txtFacultad:SCREEN-VALUE
                                AND facultades.tipo = "F" NO-ERROR.
        IF AVAILABLE facultades THEN DO:
            DELETE facultades.

            MESSAGE "La Facultad fue borrada con �xito"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            btnEliminar:SENSITIVE = FALSE.
            txtFacultad:SCREEN-VALUE = "".
            nombreFacultad:SCREEN-VALUE = "".
        END.
        ELSE
            MESSAGE "No hay Facultades para eliminar"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGuardar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGuardar C-Win
ON CHOOSE OF btnGuardar IN FRAME DEFAULT-FRAME /* btnagregar 2 */
DO:
    RELEASE facultades.

    IF INTEGER(txtFacultad:SCREEN-VALUE) > 0 AND nombreFacultad:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST facultades WHERE facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                                AND facultades.tipo = "F"
                                AND facultades.codigo = txtFacultad:SCREEN-VALUE NO-ERROR.
        IF NOT AVAILABLE facultades THEN
            CREATE facultades.

        ASSIGN facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
               facultades.tipo = "F"
               facultades.codigo = txtFacultad:SCREEN-VALUE
               facultades.nombre = nombreFacultad:SCREEN-VALUE.

        txtFacultad:SENSITIVE = FALSE.
        nombreFacultad:SENSITIVE = FALSE.
        btnGuardar:SENSITIVE = FALSE.
        cmbAgencia:SENSITIVE = FALSE.
        btnEliminar:SENSITIVE = TRUE.
    END.

    IF INTEGER(txtDepartamento:SCREEN-VALUE) > 0 AND nombreDepartamento:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST facultades WHERE facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                                AND facultades.tipo = "D"
                                AND facultades.codigo = txtFacultad:SCREEN-VALUE + txtDepartamento:SCREEN-VALUE NO-ERROR.
        IF NOT AVAILABLE facultades THEN
            CREATE facultades.

        ASSIGN facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
               facultades.tipo = "D"
               facultades.codigo = txtFacultad:SCREEN-VALUE + txtDepartamento:SCREEN-VALUE
               facultades.nombre = nombreDepartamento:SCREEN-VALUE.

        txtDepartamento:SENSITIVE = FALSE.
        nombreDepartamento:SENSITIVE = FALSE.
        btnGuardar:SENSITIVE = FALSE.
        cmbAgencia:SENSITIVE = FALSE.
        btnEliminar:SENSITIVE = TRUE.
    END.

    IF AVAILABLE facultades THEN
        MESSAGE "Los cambios fueron realizados con �xito"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE
        MESSAGE "No hay cambios para aplicar"
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
    IF cmbAgencia:SCREEN-VALUE <> "" THEN DO:
        txtFacultad:SCREEN-VALUE = "1".
        txtFacultad:SENSITIVE = FALSE.
        nombreFacultad:SENSITIVE = TRUE.

        FOR EACH facultades WHERE facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                              AND facultades.tipo = "F" BY INTEGER(facultades.codigo) DESCENDING:
            txtFacultad:SCREEN-VALUE = STRING(INTEGER(facultades.codigo) + 1).
            LEAVE.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtFacultad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtFacultad C-Win
ON LEAVE OF txtFacultad IN FRAME DEFAULT-FRAME /* Facultad */
DO:
    IF txtFacultad:SCREEN-VALUE <> "" AND INTEGER(txtFacultad:SCREEN-VALUE) > 0 THEN DO:
        FIND FIRST facultades WHERE facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                                AND facultades.tipo = "F"
                                AND facultades.codigo = txtFacultad:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE facultad THEN DO:
            MESSAGE "Esta facultad ya existe"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            nombreFacultad:SCREEN-VALUE = facultades.nombre.

            FOR EACH facultades WHERE facultades.agencia = INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2))
                                  AND facultades.tipo = "D"
                                  AND SUBSTRING(facultades.codigo,1,2) = txtFacultad:SCREEN-VALUE NO-LOCK BY INTEGER(facultades.codigo) DESCENDING:
                txtDepartamento:SCREEN-VALUE = STRING(INTEGER(SUBSTRING(facultades.codigo,3)) + 1,"999").
                LEAVE.
            END.
        END.
        ELSE DO:
            txtDepartamento:SCREEN-VALUE = "001".
        END.
    END.
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

    FOR EACH Agencias WHERE Agencias.Estado = 1 NO-LOCK:
        W_Ok = CmbAgencia:ADD-LAST(STRING(Agencias.Agencia,"99") + " - " + Agencias.Nombre).
    END.

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
  DISPLAY cmbAgencia txtFacultad nombreFacultad txtDepartamento 
          nombreDepartamento 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnBuscar btnAgregar btnSalir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

