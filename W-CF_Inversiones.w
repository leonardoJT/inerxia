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
&Scoped-Define DISPLAYED-OBJECTS tipoInversion nombreTipoInversion ~
claseInversion nombreClaseInversion 

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

DEFINE VARIABLE claseInversion AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Clase de Inversión" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE nombreClaseInversion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE nombreTipoInversion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tipoInversion AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Tipo de Inversión" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tipoInversion AT ROW 2.5 COL 17.57 COLON-ALIGNED WIDGET-ID 2
     nombreTipoInversion AT ROW 2.5 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     claseInversion AT ROW 3.65 COL 17.57 COLON-ALIGNED WIDGET-ID 4
     nombreClaseInversion AT ROW 3.65 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     btnBuscar AT ROW 4.85 COL 21 WIDGET-ID 12
     btnAgregar AT ROW 4.85 COL 27.86 WIDGET-ID 14
     btnEliminar AT ROW 4.85 COL 34.72 WIDGET-ID 22
     btnEditar AT ROW 4.85 COL 41.43 WIDGET-ID 16
     btnGuardar AT ROW 4.85 COL 48.29 WIDGET-ID 18
     btnSalir AT ROW 4.85 COL 55.14 WIDGET-ID 20
     " ADMINISTRACIÓN DE INVERSIONES" VIEW-AS TEXT
          SIZE 60.29 BY 1.08 AT ROW 1.27 COL 1.72 WIDGET-ID 8
          BGCOLOR 3 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 61.86 BY 5.81 WIDGET-ID 100.


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
         TITLE              = "Configuración de Inversiones"
         HEIGHT             = 5.81
         WIDTH              = 61.86
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
/* SETTINGS FOR FILL-IN claseInversion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombreClaseInversion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombreTipoInversion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tipoInversion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Configuración de Inversiones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Configuración de Inversiones */
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
        tipoInversion:SENSITIVE = FALSE.
        tipoInversion:SCREEN-VALUE = "".
        
        nombreTipoInversion:SENSITIVE = FALSE.
        nombreTipoInversion:SCREEN-VALUE = "".

        claseInversion:SENSITIVE = FALSE.
        claseInversion:SCREEN-VALUE = "".
        nombreClaseInversion:SENSITIVE = FALSE.
        nombreClaseInversion:SCREEN-VALUE = "".
    END.
    ELSE DO:
        IF INTEGER(tipoInversion:SCREEN-VALUE) > 0 THEN DO:
            tipoInversion:SENSITIVE = FALSE.
            nombreTipoInversion:SENSITIVE = FALSE.
            /*claseInversion:SENSITIVE = TRUE.*/
            claseInversion:SCREEN-VALUE = "1".
            nombreClaseInversion:SENSITIVE = TRUE.
            nombreClaseInversion:SCREEN-VALUE = "".

            FOR EACH facultades WHERE facultades.tipo = "D"
                                  AND SUBSTRING(facultades.codigo,1,2) = tipoInversion:SCREEN-VALUE NO-LOCK BY INTEGER(facultades.codigo) DESCENDING:
                claseInversion:SCREEN-VALUE = STRING(INTEGER(SUBSTRING(facultades.codigo,3)) + 1,"999").
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
        tipoInversion:SCREEN-VALUE = facultades.codigo.
        nombreTipoInversion:SCREEN-VALUE = facultades.nombre.

        FIND FIRST facultades WHERE facultades.agencia = codAgencia
                                AND facultades.codigo = codFacultad
                                AND facultades.tipo = "D" NO-LOCK NO-ERROR.
        IF AVAILABLE facultades THEN DO:
            claseInversion:SCREEN-VALUE = SUBSTRING(codFacultad,3).
            nombreClaseInversion:SCREEN-VALUE = facultades.nombre.
        END.
        ELSE DO:
            claseInversion:SCREEN-VALUE = "".
            nombreClaseInversion:SCREEN-VALUE = "".
        END.

        tipoInversion:SENSITIVE = FALSE.
        nombreTipoInversion:SENSITIVE = FALSE.
        claseInversion:SENSITIVE = FALSE.
        nombreClaseInversion:SENSITIVE = FALSE.
        
        btnEditar:SENSITIVE = TRUE.
        btnEliminar:SENSITIVE = TRUE.
    END.
    ELSE DO:
        tipoInversion:SENSITIVE = FALSE.
        nombreTipoInversion:SENSITIVE = FALSE.
        claseInversion:SENSITIVE = FALSE.
        nombreClaseInversion:SENSITIVE = FALSE.

        tipoInversion:SCREEN-VALUE = "".
        nombreTipoInversion:SCREEN-VALUE = "".
        claseInversion:SCREEN-VALUE = "".
        nombreClaseInversion:SCREEN-VALUE = "".

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
    IF tipoInversion:SCREEN-VALUE <> "00" THEN
        nombreTipoInversion:SENSITIVE = TRUE.

    IF claseInversion:SCREEN-VALUE <> "000" THEN
        nombreClaseInversion:SENSITIVE = TRUE.

    btnGuardar:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEliminar C-Win
ON CHOOSE OF btnEliminar IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    DEFINE VAR tempFacultad AS CHARACTER.

    FIND FIRST clientes WHERE clientes.facultad = INTEGER(tipoInversion:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        MESSAGE "No se puede eliminar esta facultad ya que existen Clientes que la tienen matriculada"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    FIND FIRST facultades WHERE facultades.tipo = "D"
                            AND SUBSTRING(facultades.codigo,1,2) = tipoInversion:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE facultades AND claseInversion:SCREEN-VALUE = "000" THEN DO:
        MESSAGE "No se puede eliminar esta facultad ya que existen Departamentos creados dentro de ella"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    IF claseInversion:SCREEN-VALUE <> "000" THEN DO:
        FIND FIRST facultades WHERE facultades.codigo = tipoInversion:SCREEN-VALUE + claseInversion:SCREEN-VALUE
                                AND facultades.tipo = "D" NO-ERROR.
        IF AVAILABLE facultades THEN DO:
            DELETE facultades.

            MESSAGE "El Departamento fue borrado con éxito"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            btnEliminar:SENSITIVE = FALSE.
            claseInversion:SCREEN-VALUE = "".
            nombreClaseInversion:SCREEN-VALUE = "".
        END.
        ELSE
            MESSAGE "No hay Departamento para eliminar"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
        FIND FIRST facultades WHERE facultades.codigo = tipoInversion:SCREEN-VALUE
                                AND facultades.tipo = "F" NO-ERROR.
        IF AVAILABLE facultades THEN DO:
            DELETE facultades.

            MESSAGE "La Facultad fue borrada con éxito"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            btnEliminar:SENSITIVE = FALSE.
            tipoInversion:SCREEN-VALUE = "".
            nombreTipoInversion:SCREEN-VALUE = "".
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

    IF INTEGER(tipoInversion:SCREEN-VALUE) > 0 AND nombreTipoInversion:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST facultades WHERE facultades.tipo = "F"
                                AND facultades.codigo = tipoInversion:SCREEN-VALUE NO-ERROR.
        IF NOT AVAILABLE facultades THEN
            CREATE facultades.

        ASSIGN facultades.tipo = "F"
               facultades.codigo = tipoInversion:SCREEN-VALUE
               facultades.nombre = nombreTipoInversion:SCREEN-VALUE.

        tipoInversion:SENSITIVE = FALSE.
        nombreTipoInversion:SENSITIVE = FALSE.
        btnGuardar:SENSITIVE = FALSE.
        btnEliminar:SENSITIVE = TRUE.
    END.

    IF INTEGER(claseInversion:SCREEN-VALUE) > 0 AND nombreClaseInversion:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST facultades WHERE facultades.tipo = "D"
                                AND facultades.codigo = tipoInversion:SCREEN-VALUE + claseInversion:SCREEN-VALUE NO-ERROR.
        IF NOT AVAILABLE facultades THEN
            CREATE facultades.

        ASSIGN facultades.tipo = "D"
               facultades.codigo = tipoInversion:SCREEN-VALUE + claseInversion:SCREEN-VALUE
               facultades.nombre = nombreClaseInversion:SCREEN-VALUE.

        claseInversion:SENSITIVE = FALSE.
        nombreClaseInversion:SENSITIVE = FALSE.
        btnGuardar:SENSITIVE = FALSE.
        btnEliminar:SENSITIVE = TRUE.
    END.

    IF AVAILABLE facultades THEN
        MESSAGE "Los cambios fueron realizados con éxito"
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


&Scoped-define SELF-NAME tipoInversion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipoInversion C-Win
ON LEAVE OF tipoInversion IN FRAME DEFAULT-FRAME /* Tipo de Inversión */
DO:
    IF tipoInversion:SCREEN-VALUE <> "" AND INTEGER(tipoInversion:SCREEN-VALUE) > 0 THEN DO:
        FIND FIRST facultades WHERE facultades.tipo = "F"
                                AND facultades.codigo = tipoInversion:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE facultad THEN DO:
            MESSAGE "Esta facultad ya existe"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            nombreTipoInversion:SCREEN-VALUE = facultades.nombre.

            FOR EACH facultades WHERE facultades.tipo = "D"
                                  AND SUBSTRING(facultades.codigo,1,2) = tipoInversion:SCREEN-VALUE NO-LOCK BY INTEGER(facultades.codigo) DESCENDING:
                claseInversion:SCREEN-VALUE = STRING(INTEGER(SUBSTRING(facultades.codigo,3)) + 1,"999").
                LEAVE.
            END.
        END.
        ELSE DO:
            claseInversion:SCREEN-VALUE = "001".
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
  DISPLAY tipoInversion nombreTipoInversion claseInversion nombreClaseInversion 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnBuscar btnAgregar btnSalir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

