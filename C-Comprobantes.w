&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

   DEFINE INPUT-OUTPUT PARAMETER W_Rowid AS ROWID.
   
/* Local Variable Definitions ---                                       */
  {incluido\variable.i "SHARED"}
  
  DEFINE VARIABLE Wk_agencia LIKE Comprobantes.agencia.
  DEFINE VARIABLE Wk_SuperUsr  AS LOGICAL.
  DEFINE VARIABLE W_Status     AS LOGICAL.

  DEFINE BUFFER Tmp_Usuarios FOR Usuarios.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Comprobantes

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 Comprobantes.agencia Comprobantes.Comprobante Comprobantes.Nombre Comprobantes.Secuencia   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH Comprobantes       WHERE Comprobantes.Estado EQ W_Estado NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH Comprobantes       WHERE Comprobantes.Estado EQ W_Estado NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 Comprobantes
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 Comprobantes


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 RADIO-SET-2 W_Estado W_Valor ~
BUTTON-48 RECT-143 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-2 W_Estado W_Valor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-48 AUTO-GO 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Button 48" 
     SIZE 9 BY 1.62 TOOLTIP "Vuelve a la pantalla anterior".

DEFINE VARIABLE W_Valor AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1.08 TOOLTIP "Valor de Selección"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RADIO-SET-2 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Código", 1,
"Nombre", 2,
"Agencia", 3,
"Todos", 4
     SIZE 47 BY .69 TOOLTIP "Opción de Selección" NO-UNDO.

DEFINE VARIABLE W_Estado AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 11 BY 1.62 TOOLTIP "Estado de los Comprobantes a Consultar" NO-UNDO.

DEFINE RECTANGLE RECT-143
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 2.96.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      Comprobantes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      Comprobantes.agencia COLUMN-LABEL "Age" FORMAT "999":U
      Comprobantes.Comprobante COLUMN-LABEL "Código" FORMAT "99":U
      Comprobantes.Nombre FORMAT "X(37)":U
      Comprobantes.Secuencia FORMAT "9999999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 64 BY 4.85
         BGCOLOR 15 FONT 5 TOOLTIP "Con Doble Click del Mouse se Selecciona el Registro".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-2 AT ROW 1.27 COL 2
     RADIO-SET-2 AT ROW 7.46 COL 18 NO-LABEL
     W_Estado AT ROW 7.73 COL 5 NO-LABEL
     W_Valor AT ROW 8.27 COL 16 COLON-ALIGNED NO-LABEL
     BUTTON-48 AT ROW 9.88 COL 57
     RECT-143 AT ROW 6.65 COL 2
     "  Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 30 BY 1.08 AT ROW 6.12 COL 4
          FGCOLOR 7 
     "Nota:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 10.15 COL 3
          FGCOLOR 7 
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 10.15 COL 9
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 10.96 COL 9
          FONT 4
     SPACE(19.85) SKIP(0.29)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 17 FONT 5
         TITLE "Consulta de Comprobantes".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-2 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Comprobantes
      WHERE Comprobantes.Estado EQ W_Estado NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _TblOptList       = "USED,"
     _Where[1]         = "Comprobantes.Estado EQ W_Estado"
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Consulta de Comprobantes */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON ENTRY OF BROWSE-2 IN FRAME Dialog-Frame
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON LEAVE OF BROWSE-2 IN FRAME Dialog-Frame
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME Dialog-Frame
OR RETURN OF {&BROWSE-NAME}
DO:
   ASSIGN W_Rowid = ROWID(Comprobantes).
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-48
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-48 Dialog-Frame
ON CHOOSE OF BUTTON-48 IN FRAME Dialog-Frame /* Button 48 */
DO:
   ASSIGN W_Rowid = ROWID(Comprobantes).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-2 Dialog-Frame
ON VALUE-CHANGED OF RADIO-SET-2 IN FRAME Dialog-Frame
DO:
  ASSIGN RADIO-SET-2
         W_Valor:SCREEN-VALUE = "".
  IF NUM-RESULTS("{&BROWSE-NAME}") GT 0 THEN
     CLOSE QUERY {&BROWSE-NAME}.
  
  IF RADIO-SET-2 EQ 4 THEN DO:
     DISABLE W_Valor WITH FRAME {&FRAME-NAME}.
     IF Wk_SuperUsr THEN
        {&OPEN-QUERY-{&BROWSE-NAME}}
     ELSE
        OPEN QUERY BROWSE-2 FOR EACH Comprobantes WHERE Comprobantes.agencia EQ W_agencia
                                                    AND Comprobantes.Estado  EQ W_Estado NO-LOCK.
     RETURN.
  END.
  ELSE
    ENABLE W_Valor WITH FRAME {&FRAME-NAME}.
  APPLY "ENTRY":U TO W_Valor.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Estado Dialog-Frame
ON VALUE-CHANGED OF W_Estado IN FRAME Dialog-Frame
DO:
  ASSIGN W_Estado.
  IF RADIO-SET-2 EQ 4 THEN DO:
     ASSIGN W_Valor:HIDDEN = TRUE.
        OPEN QUERY BROWSE-2 FOR EACH Comprobantes WHERE Comprobantes.agencia EQ W_agencia
                                                    AND Comprobantes.Estado  EQ W_Estado NO-LOCK.
  END.
  ELSE RUN Abre_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Valor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Valor Dialog-Frame
ON LEAVE OF W_Valor IN FRAME Dialog-Frame
DO:
   ASSIGN W_Valor.
   RUN Abre_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  ASSIGN Wk_agencia  = W_agencia.
         Wk_SuperUsr = YES.
  FIND Tmp_Usuarios WHERE Tmp_Usuarios.agencia   EQ W_agencia 
                      AND Tmp_Usuarios.Id_OpeOfi EQ TRUE 
                      AND Tmp_Usuarios.Usuario   EQ W_Usuario 
                      AND Tmp_Usuarios.Prioridad GT 2 NO-LOCK NO-ERROR.

  IF NOT AVAILABLE Tmp_Usuarios THEN
     ASSIGN Wk_SuperUsr = FALSE
            W_Status    = RADIO-SET-2:DELETE("agencia").

  APPLY "VALUE-CHANGED":U TO RADIO-SET-2.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Abre_Browser Dialog-Frame 
PROCEDURE Abre_Browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RADIO-SET-2 EQ 1 THEN DO:
      IF Wk_SuperUsr THEN
         OPEN QUERY BROWSE-2 FOR EACH Comprobantes WHERE Comprobantes.Estado      EQ W_Estado
                                                     AND Comprobantes.Comprobante EQ INTEGER(W_Valor) NO-LOCK.
      ELSE
         OPEN QUERY BROWSE-2 FOR EACH Comprobantes WHERE Comprobantes.agencia     EQ W_agencia
                                                     AND Comprobantes.Estado      EQ W_Estado
                                                     AND Comprobantes.Comprobante EQ INTEGER(W_Valor) NO-LOCK.
   END.
   ELSE DO:
      IF RADIO-SET-2 EQ 2 THEN DO:
         IF Wk_SuperUsr THEN
            OPEN QUERY BROWSE-2 FOR EACH Comprobantes WHERE Comprobantes.Estado EQ W_Estado 
                                                        AND Comprobantes.Nombre     CONTAINS W_Valor NO-LOCK.
                                                    
         ELSE         
            OPEN QUERY BROWSE-2 FOR EACH Comprobantes WHERE Comprobantes.agencia EQ W_agencia
                                                        AND Comprobantes.Estado  EQ W_Estado 
                                                        AND Comprobantes.Nombre  CONTAINS W_Valor NO-LOCK.                                                        
      END.
      ELSE
         OPEN QUERY BROWSE-2 FOR EACH Comprobantes WHERE Comprobantes.Estado EQ W_Estado 
                                                     AND Comprobantes.agencia    EQ INTEGER(W_Valor) NO-LOCK.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY RADIO-SET-2 W_Estado W_Valor 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-2 RADIO-SET-2 W_Estado W_Valor BUTTON-48 RECT-143 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

