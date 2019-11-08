&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES coincidencias_ListasSarlaft

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ~
coincidencias_ListasSarlaft.Coincidencia coincidencias_ListasSarlaft.fecha ~
coincidencias_ListasSarlaft.idlista coincidencias_ListasSarlaft.linea ~
coincidencias_ListasSarlaft.Usuario coincidencias_ListasSarlaft.agencia 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH coincidencias_ListasSarlaft NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH coincidencias_ListasSarlaft NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 coincidencias_ListasSarlaft
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 coincidencias_ListasSarlaft


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 BtnDone 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone 
     IMAGE-UP FILE "D:/SFG/Desarrollo/Obj/imagenes/volver.bmp":U
     LABEL "" 
     SIZE 7 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Salir 
     IMAGE-UP FILE "D:/SFG/Desarrollo/Obj/imagenes/volver.bmp":U
     LABEL "" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE DetalleText AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 99.57 BY 16.96 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      coincidencias_ListasSarlaft SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      coincidencias_ListasSarlaft.Coincidencia FORMAT "x(30)":U
            WIDTH 12.43
      coincidencias_ListasSarlaft.fecha FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 24
      coincidencias_ListasSarlaft.idlista FORMAT "x(8)":U WIDTH 14.43
      coincidencias_ListasSarlaft.linea FORMAT "x(320)":U WIDTH 55.43
      coincidencias_ListasSarlaft.Usuario FORMAT "x(60)":U WIDTH 15.86
      coincidencias_ListasSarlaft.agencia FORMAT "->,>>>,>>9":U
            WIDTH 8.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 137 BY 25.62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-2 AT ROW 1.42 COL 2.43 WIDGET-ID 200
     BtnDone AT ROW 25.31 COL 140.43 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 147 BY 27.73 WIDGET-ID 100.

DEFINE FRAME DetalleCoincidencia
     DetalleText AT ROW 1.27 COL 2.43 NO-LABEL WIDGET-ID 8
     Salir AT ROW 18.5 COL 95 WIDGET-ID 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23 ROW 2.62
         SIZE 103 BY 20.19
         TITLE "Detalle de coincidencia en listas SARLAFT" WIDGET-ID 300.


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
         TITLE              = "Coincidencias en listas SARLAFT"
         HEIGHT             = 26.85
         WIDTH              = 147.29
         MAX-HEIGHT         = 27.73
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 27.73
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
/* REPARENT FRAME */
ASSIGN FRAME DetalleCoincidencia:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME DetalleCoincidencia:MOVE-AFTER-TAB-ITEM (BROWSE-2:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME DetalleCoincidencia:MOVE-BEFORE-TAB-ITEM (BtnDone:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BROWSE-2 1 DEFAULT-FRAME */
/* SETTINGS FOR FRAME DetalleCoincidencia
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DetalleCoincidencia:HIDDEN           = TRUE.

ASSIGN 
       DetalleText:READ-ONLY IN FRAME DetalleCoincidencia        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "bdcentral.coincidencias_ListasSarlaft"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.coincidencias_ListasSarlaft.Coincidencia
"coincidencias_ListasSarlaft.Coincidencia" ? ? "character" ? ? ? ? ? ? no ? no no "12.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdcentral.coincidencias_ListasSarlaft.fecha
"coincidencias_ListasSarlaft.fecha" ? ? "datetime" ? ? ? ? ? ? no ? no no "24" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > bdcentral.coincidencias_ListasSarlaft.idlista
"coincidencias_ListasSarlaft.idlista" ? ? "character" ? ? ? ? ? ? no ? no no "14.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > bdcentral.coincidencias_ListasSarlaft.linea
"coincidencias_ListasSarlaft.linea" ? "x(320)" "character" ? ? ? ? ? ? no ? no no "55.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > bdcentral.coincidencias_ListasSarlaft.Usuario
"coincidencias_ListasSarlaft.Usuario" ? ? "character" ? ? ? ? ? ? no ? no no "15.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > bdcentral.coincidencias_ListasSarlaft.agencia
"coincidencias_ListasSarlaft.agencia" ? ? "integer" ? ? ? ? ? ? no ? no no "8.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Coincidencias en listas SARLAFT */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Coincidencias en listas SARLAFT */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME
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



&Scoped-define FRAME-NAME DetalleCoincidencia
&Scoped-define SELF-NAME Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Salir C-Win
ON CHOOSE OF Salir IN FRAME DetalleCoincidencia
DO:
  ASSIGN 
       FRAME DetalleCoincidencia:HIDDEN           = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2
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
  
  DEFINE VAR NombreLista AS CHARACTER INIT "Lista no encontrada".
  DEFINE VAR NombreUsuario AS CHARACTER INIT "Usuario no encontrado".
  DEFINE VAR NombreAgencia AS CHARACTER INIT "Agencia no encontrada".

  ON MOUSE-SELECT-DBLCLICK OF BROWSE-2
    DO:
        IF AVAILABLE coincidencias_listasSarlaft THEN DO:
            ASSIGN FRAME DetalleCoincidencia:HIDDEN           = FALSE.
            FIND FIRST Agencias WHERE Agencias.Agencia = coincidencias_ListasSarlaft.agencia NO-LOCK NO-ERROR.
            FIND FIRST Usuarios WHERE Usuario.usuario = coincidencias_ListasSarlaft.usuario NO-LOCK NO-ERROR.
            FIND FIRST cfg_listasSarlaft WHERE STRING(ROWID(cfg_listasSarlaft)) = coincidencias_ListasSarlaft.idlista NO-LOCK NO-ERROR.
            IF AVAILABLE cfg_listasSarlaft THEN NombreLista = cfg_listasSarlaft.lista.
            IF AVAILABLE Agencias THEN NombreAgencia = Agencias.Nombre.
            IF AVAILABLE Usuarios THEN NombreUsuario = Usuarios.nombre.
            ASSIGN DetalleText:SCREEN-VALUE = "Identificación: " + STRING(coincidencias_listasSarlaft.coincidencia) + CHR(10) +
                 "Lista: " + NombreLista + CHR(10) +
                 "Línea: " + STRING(coincidencias_listasSarlaft.linea) + CHR(10) +
                 "Fecha: " + STRING(coincidencias_ListasSarlaft.fecha) + CHR(10) +
                 "Agencia: " + NombreAgencia + CHR(10) +
                 "Usuario que reporta: " + NombreUsuario.
            ASSIGN NombreLista = "Lista no encontrada".
        END.
    END.


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
  ENABLE BROWSE-2 BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY DetalleText 
      WITH FRAME DetalleCoincidencia IN WINDOW C-Win.
  ENABLE DetalleText Salir 
      WITH FRAME DetalleCoincidencia IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DetalleCoincidencia}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

