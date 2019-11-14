&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-RP_Visor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-RP_Visor 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

   DEFINE SHARED VAR W_Manija  AS   HANDLE.
   DEFINE SHARED VAR W_Pathspl LIKE Entidad.Dir_Spl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Subir

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Buscar Btn_Salir W_Path Btn_Impresora ~
Btn_Pantalla Bnt_Ayuda RECT-12 
&Scoped-Define DISPLAYED-OBJECTS W_Path 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-RP_Visor AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Bnt_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "&Ayuda" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda".

DEFINE BUTTON Btn_Buscar 
     IMAGE-UP FILE "Imagenes/open-d":U
     LABEL "&Buscar" 
     SIZE 6 BY 1.38 TOOLTIP "Buscar Archivo"
     FONT 1.

DEFINE BUTTON Btn_Impresora 
     IMAGE-UP FILE "Imagenes/impresora2.bmp":U
     LABEL "&Impresora" 
     SIZE 5.43 BY 1.54 TOOLTIP "Envío a Impresora"
     FONT 4.

DEFINE BUTTON Btn_Pantalla 
     IMAGE-UP FILE "Imagenes/monitor.ico":U
     LABEL "&Pantalla" 
     SIZE 5.43 BY 1.54 TOOLTIP "Envío a Pantalla"
     FONT 4.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 6 BY 1.38 TOOLTIP "Regresar al Menú".

DEFINE VARIABLE W_Path AS CHARACTER FORMAT "X(80)":U 
     LABEL "Nombre Archivo" 
     VIEW-AS FILL-IN 
     SIZE 67 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 2.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Subir
     Btn_Buscar AT ROW 1.27 COL 86
     Btn_Salir AT ROW 1.27 COL 92 HELP
          "Sale del Programa"
     W_Path AT ROW 1.54 COL 16 COLON-ALIGNED HELP
          "Ingrese el nombre del Archivo o presione el boton buscar"
     Btn_Impresora AT ROW 3.96 COL 40 HELP
          "Imprime el Archivo por Impresora"
     Btn_Pantalla AT ROW 3.96 COL 50 HELP
          "Imprime el Archivo por pantalla"
     Bnt_Ayuda AT ROW 4.5 COL 93 HELP
          "Obtiene ayuda acerca del programa"
     RECT-12 AT ROW 3.42 COL 38
     " Salida del Reporte" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 3.15 COL 39
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.86 BY 16.92
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-RP_Visor ASSIGN
         HIDDEN             = YES
         TITLE              = "Visor de Reportes"
         HEIGHT             = 5.46
         WIDTH              = 97.86
         MAX-HEIGHT         = 16.92
         MAX-WIDTH          = 97.86
         VIRTUAL-HEIGHT     = 16.92
         VIRTUAL-WIDTH      = 97.86
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-RP_Visor:LOAD-ICON("imagenes/e_graficar.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/e_graficar.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-RP_Visor
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Subir
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-RP_Visor)
THEN W-RP_Visor:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-RP_Visor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-RP_Visor W-RP_Visor
ON END-ERROR OF W-RP_Visor /* Visor de Reportes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-RP_Visor W-RP_Visor
ON WINDOW-CLOSE OF W-RP_Visor /* Visor de Reportes */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bnt_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bnt_Ayuda W-RP_Visor
ON CHOOSE OF Bnt_Ayuda IN FRAME F-Subir /* Ayuda */
OR HELP OF {&WINDOW-NAME}
DO:
   SYSTEM-HELP "ayudas/utilidad" CONTEXT 6.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Buscar W-RP_Visor
ON CHOOSE OF Btn_Buscar IN FRAME F-Subir /* Buscar */
DO:
  DEFINE VAR W_Tecla   AS LOGICAL INITIAL TRUE.
  DEFINE VAR W_NomArc  AS CHAR INITIAL "".
  
  SYSTEM-DIALOG GET-FILE W_NomArc
  TITLE "Escoja el archivo"
  FILTERS "Todos los Archivos (*.*)" "*.*"
  INITIAL-DIR W_PathSpl
  RETURN-TO-START-DIR
  UPDATE W_Tecla.
  IF W_Tecla = TRUE THEN 
     ASSIGN W_Path:SCREEN-VALUE IN FRAME F-Subir = W_NomArc
            W_Path = W_NomArc.
  ELSE
    DO: 
      APPLY "ENTRY" TO W_Path.
      RETURN NO-APPLY.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresora W-RP_Visor
ON CHOOSE OF Btn_Impresora IN FRAME F-Subir /* Impresora */
DO:
  DEFINE VAR W_Sw AS LOGICAL.
/*el tercer parametro tenia un valor de ?, y debe ser de Dos. */
  IF W_Path = ? OR TRIM(W_Path) = "" THEN
   DO:
     MESSAGE "El Archivo a Mostrar NO ES VALIDO..verifique" VIEW-AS ALERT-BOX ERROR TITLE "Validaci¢n Imprimir Archivo".
     APPLY "ENTRY" TO W_Path.
     RETURN NO-APPLY.
   END.  
  RUN adecomm/_osprint.r ( INPUT  ?, INPUT W_Path,INPUT  2,INPUT  1,INPUT  1,
                                                           INPUT  99999,OUTPUT W_sw).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Pantalla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Pantalla W-RP_Visor
ON CHOOSE OF Btn_Pantalla IN FRAME F-Subir /* Pantalla */
DO:
  IF W_Path = ? OR TRIM(W_Path) = "" THEN
   DO:
     MESSAGE "El Archivo a Mostrar NO ES VALIDO..verifique" VIEW-AS ALERT-BOX ERROR TITLE "Validaci¢n Imprimir Archivo".
     APPLY "ENTRY" TO W_Path.
     RETURN NO-APPLY.
   END.  
   RUN Pantalla IN W_Manija (INPUT W_Path).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir W-RP_Visor
ON CHOOSE OF Btn_Salir IN FRAME F-Subir /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Path
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Path W-RP_Visor
ON LEAVE OF W_Path IN FRAME F-Subir /* Nombre Archivo */
DO: 
  ASSIGN W_Path.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-RP_Visor 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-RP_Visor  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-RP_Visor)
  THEN DELETE WIDGET W-RP_Visor.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-RP_Visor  _DEFAULT-ENABLE
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
  DISPLAY W_Path 
      WITH FRAME F-Subir IN WINDOW W-RP_Visor.
  ENABLE Btn_Buscar Btn_Salir W_Path Btn_Impresora Btn_Pantalla Bnt_Ayuda 
         RECT-12 
      WITH FRAME F-Subir IN WINDOW W-RP_Visor.
  {&OPEN-BROWSERS-IN-QUERY-F-Subir}
  VIEW W-RP_Visor.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

