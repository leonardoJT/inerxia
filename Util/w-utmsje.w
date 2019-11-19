&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  ARCHIVO. W-UtMsje.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
CREATE WIDGET-POOL.

  DEFINE VAR W_CCosGral LIKE Cen_Costos.Cen_Costo.
  DEFINE VAR W_Cod      LIKE Men_Sistema.Codigo.
  DEFINE VAR W_Tip      LIKE Men_Sistema.Tipo.
  DEFINE VAR W_Cla      LIKE Men_Sistema.Clase.
  DEFINE VAR W_Est      LIKE Men_Sistema.Estado.
  DEFINE VAR W_Men      LIKE Men_Sistema.Mensaje.
  DEFINE VAR W_Tit      LIKE Men_Sistema.Titulo.
  DEFINE VAR W_PathFile AS   CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Umvto

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-142 Btn_File RECT-143 RECT-144 ~
Btn_Ejecutar Btn_Salir 
&Scoped-Define DISPLAYED-OBJECTS W_Ruta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ejecutar 
     LABEL "Ejecutar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_File 
     IMAGE-UP FILE "open-d":U
     LABEL "Button 1" 
     SIZE 5 BY 1.35 TOOLTIP "Buscar Archivo para importar".

DEFINE BUTTON Btn_Salir AUTO-END-KEY 
     LABEL "Salir" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE w_contador AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Ruta AS CHARACTER FORMAT "X(50)":U 
     LABEL "Ubicación del Archivo" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-142
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 6.73.

DEFINE RECTANGLE RECT-143
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 1.62.

DEFINE RECTANGLE RECT-144
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Umvto
     Btn_File AT ROW 3.15 COL 60 HELP
          "Permite Buscar el Archivo para importar"
     W_Ruta AT ROW 3.42 COL 23 COLON-ALIGNED HELP
          "Ingrese el path donde se encuentra el archivo a importar"
     w_contador AT ROW 4.77 COL 26 COLON-ALIGNED NO-LABEL
     Btn_Ejecutar AT ROW 6.12 COL 19
     Btn_Salir AT ROW 6.12 COL 36
     RECT-142 AT ROW 1 COL 1
     "    Actualización de Mensajes." VIEW-AS TEXT
          SIZE 30 BY .81 AT ROW 1.81 COL 23
          BGCOLOR 7 FGCOLOR 15 
     RECT-143 AT ROW 5.85 COL 18
     RECT-144 AT ROW 5.85 COL 35
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.


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
         TITLE              = "Actualización de Mensajes"
         COLUMN             = 13.72
         ROW                = 5.85
         HEIGHT             = 6.96
         WIDTH              = 69.72
         MAX-HEIGHT         = 17.73
         MAX-WIDTH          = 91.43
         VIRTUAL-HEIGHT     = 17.73
         VIRTUAL-WIDTH      = 91.43
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Umvto
                                                                        */
/* SETTINGS FOR FILL-IN w_contador IN FRAME F_Umvto
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       w_contador:HIDDEN IN FRAME F_Umvto           = TRUE.

/* SETTINGS FOR FILL-IN W_Ruta IN FRAME F_Umvto
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Actualización de Mensajes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Actualización de Mensajes */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ejecutar C-Win
ON CHOOSE OF Btn_Ejecutar IN FRAME F_Umvto /* Ejecutar */
DO:
 RUN _Setcurs.P ("WAIT").
 FIND LAST Men_sistema NO-ERROR.
 IF AVAILABLE (Men_Sistema) THEN DO:
   IF Men_sistema.Codigo NE CURRENT-VALUE(Sec_Mensajes) THEN   
     CURRENT-VALUE(Sec_Mensajes) = Men_Sistema.Codigo.
 END.
 
 INPUT FROM VALUE(W_Ruta).
 REPEAT:
   IMPORT 
      w_Cod W_Tip W_Cla W_Est W_Men W_Tit NO-ERROR.
   FIND Men_Sistema WHERE Men_Sistema.Codigo = W_Cod NO-ERROR.
   IF NOT AVAILABLE (Men_Sistema) THEN DO:
    CREATE Men_sistema.
    ASSIGN Men_Sistema.Codigo  = W_Cod
           Men_Sistema.Tipo    = W_Tip
           Men_Sistema.Clase   = W_Cla
           Men_Sistema.Estado  = W_Est
           Men_Sistema.Mensaje = W_Men
           Men_Sistema.Titulo  = W_tit.
   END.
   W_contador = w_contador + 1.
   DISPLAY W_Contador WITH FRAME {&FRAME-NAME}.         
 END.
 FIND LAST Men_sistema NO-ERROR.
 IF AVAILABLE (Men_Sistema) THEN 
   CURRENT-VALUE(Sec_Mensajes) = Men_Sistema.Codigo.
 RUN _Setcurs.P ("ARROW").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_File
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_File C-Win
ON CHOOSE OF Btn_File IN FRAME F_Umvto /* Button 1 */
DO:
  SYSTEM-DIALOG GET-FILE W_pathfile
     TITLE      "Buscar Archivo ..."
     FILTERS    "Archivos Tipo Texto (*.txt)"   "*.txt",
                "Archivos de Listado (*.Lst)"   "*.Lst",
                "Archivos de Datos   (*.Dat)"   "*.Dat",
                "Archivos de Progress (*.d)"     "*.D",
                "Todos los Archivos (*.*)"      "*.*"
     MUST-EXIST
     USE-FILENAME.
        
  W_Ruta = W_pathfile.
  DISPLAY W_ruta WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Ruta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Ruta C-Win
ON LEAVE OF W_Ruta IN FRAME F_Umvto /* Ubicación del Archivo */
DO:
  ASSIGN W_Ruta.
  IF W_Ruta = "" THEN
    RETURN NO-APPLY.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  DISPLAY W_Ruta 
      WITH FRAME F_Umvto IN WINDOW C-Win.
  ENABLE RECT-142 Btn_File RECT-143 RECT-144 Btn_Ejecutar Btn_Salir 
      WITH FRAME F_Umvto IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Umvto}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


