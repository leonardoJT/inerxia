&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* P_TipCta toma valores de T = todas las ctas, M = Ctas de movimiento, 
                            A = Ctas de Ajustes, R = Ctas de Orden */
  DEFINE OUTPUT PARAMETER P_Cuenta LIKE Cuentas.Cuenta.
  DEFINE OUTPUT PARAMETER P_Nombre LIKE Cuentas.Nombre.
  DEFINE OUTPUT PARAMETER P_NatTra LIKE Cuentas.Naturaleza.
  DEFINE OUTPUT PARAMETER P_CtrNat LIKE Cuentas.Ctr_Naturaleza.
  DEFINE INPUT  PARAMETER P_TipCta AS CHAR.  
  DEFINE SHARED VAR       W_Manija AS HANDLE.
  DEFINE        VAR       W_Rowid  AS ROWID.
  DEFINE        VAR       W_Rpta   AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cuentas

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 Cuentas.Cuenta Cuentas.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define OPEN-QUERY-BROWSE-2 CASE P_TipCta:     WHEN "T" THEN         OPEN QUERY {&SELF-NAME} FOR EACH Cuentas NO-LOCK.     WHEN "M" THEN         OPEN QUERY {&SELF-NAME} FOR EACH Cuentas WHERE Cuentas.Tipo EQ 2 NO-LOCK.     WHEN "A" THEN         OPEN QUERY BROWSE-2         FOR EACH Cuentas WHERE Cuentas.Tipo    EQ 2                            AND Cuentas.Estado  EQ 1                            AND Cuentas.Id_Paag EQ TRUE NO-LOCK.     WHEN "R" THEN         OPEN QUERY BROWSE-2         FOR EACH Cuentas WHERE Cuentas.Tipo      EQ 2                            AND Cuentas.Estado    EQ 1                            AND Cuentas.Id_Cuenta EQ 5 NO-LOCK.     WHEN "D" THEN         OPEN QUERY BROWSE-2         FOR EACH Cuentas WHERE Cuentas.Tipo      EQ 2                            AND Cuentas.Estado    EQ 1                            AND Cuentas.Id_Cuenta EQ 4 NO-LOCK.  END CASE.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 Cuentas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 Cuentas


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-143 BROWSE-2 R_Buscar W_Valor BT_FIN 
&Scoped-Define DISPLAYED-OBJECTS R_Buscar W_Valor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BT_FIN 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Button 60" 
     SIZE 9 BY 1.65.

DEFINE VARIABLE W_Valor AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1.08 TOOLTIP "Valor de Selección"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Buscar AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cuenta", 1,
"Nombre", 2,
"Todos", 3
     SIZE 55 BY .69 TOOLTIP "Opción de Selección" NO-UNDO.

DEFINE RECTANGLE RECT-143
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 2.96.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      Cuentas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      Cuentas.Cuenta FORMAT "X(14)":U
      Cuentas.Nombre FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 58 BY 12.38
         BGCOLOR 15 FGCOLOR 7 FONT 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-2 AT ROW 1 COL 3
     R_Buscar AT ROW 15.27 COL 5 NO-LABEL
     W_Valor AT ROW 16.08 COL 3 COLON-ALIGNED NO-LABEL
     BT_FIN AT ROW 17.96 COL 52
     "  Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 30 BY 1.08 AT ROW 13.92 COL 5
          FGCOLOR 7 
     "Nota:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 17.96 COL 4
          FGCOLOR 7 
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 17.96 COL 10
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 18.77 COL 10
          FONT 4
     RECT-143 AT ROW 14.46 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 18.96
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta de Cuentas Contables"
         HEIGHT             = 18.96
         WIDTH              = 62.29
         MAX-HEIGHT         = 22.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.35
         VIRTUAL-WIDTH      = 114.29
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 RECT-143 F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
CASE P_TipCta:
    WHEN "T" THEN
        OPEN QUERY {&SELF-NAME} FOR EACH Cuentas NO-LOCK.
    WHEN "M" THEN
        OPEN QUERY {&SELF-NAME} FOR EACH Cuentas WHERE Cuentas.Tipo EQ 2 NO-LOCK.
    WHEN "A" THEN
        OPEN QUERY BROWSE-2
        FOR EACH Cuentas WHERE Cuentas.Tipo    EQ 2
                           AND Cuentas.Estado  EQ 1
                           AND Cuentas.Id_Paag EQ TRUE NO-LOCK.
    WHEN "R" THEN
        OPEN QUERY BROWSE-2
        FOR EACH Cuentas WHERE Cuentas.Tipo      EQ 2
                           AND Cuentas.Estado    EQ 1
                           AND Cuentas.Id_Cuenta EQ 5 NO-LOCK.
    WHEN "D" THEN
        OPEN QUERY BROWSE-2
        FOR EACH Cuentas WHERE Cuentas.Tipo      EQ 2
                           AND Cuentas.Estado    EQ 1
                           AND Cuentas.Id_Cuenta EQ 4 NO-LOCK.

END CASE.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Consulta de Cuentas Contables */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Consulta de Cuentas Contables */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 W-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME F-Main
DO:
    IF AVAILABLE(Cuentas) THEN 
       ASSIGN P_Cuenta = Cuentas.Cuenta
              P_Nombre   = Cuentas.Nombre.
    APPLY "CHOOSE" TO Bt_Fin.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 W-Win
ON VALUE-CHANGED OF BROWSE-2 IN FRAME F-Main
DO:
    IF AVAILABLE(Cuentas) THEN 
       ASSIGN P_Cuenta = Cuentas.Cuenta
              P_Nombre   = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT_FIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT_FIN W-Win
ON CHOOSE OF BT_FIN IN FRAME F-Main /* Button 60 */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT_FIN W-Win
ON ENTRY OF BT_FIN IN FRAME F-Main /* Button 60 */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT_FIN W-Win
ON LEAVE OF BT_FIN IN FRAME F-Main /* Button 60 */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Buscar W-Win
ON VALUE-CHANGED OF R_Buscar IN FRAME F-Main
DO:
  ASSIGN R_Buscar
         W_Valor:SCREEN-VALUE = "".

  IF NUM-RESULTS("{&BROWSE-NAME}") GT 0 THEN
     CLOSE QUERY {&BROWSE-NAME}.
  
  IF R_Buscar EQ 3 THEN DO:
     DISABLE W_Valor WITH FRAME {&FRAME-NAME}.
     RUN Abre_Browser.
     RETURN.
  END.
  ELSE
    ENABLE W_Valor WITH FRAME {&FRAME-NAME}.

  APPLY "ENTRY":U TO W_Valor.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Valor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Valor W-Win
ON LEAVE OF W_Valor IN FRAME F-Main
OR RETURN OF W_Valor DO:
    IF LASTKEY = KEYCODE("ENTER") THEN
      DO:
        ASSIGN W_Valor.
               W_Rowid = ROWID(Cuentas).
        RUN Abre_Browser.
        IF AVAILABLE (Cuentas) THEN
          DO:
           ASSIGN P_Cuenta = Cuentas.Cuenta
                  P_Nombre = Cuentas.Nombre.
          END.
        ELSE
          DO:
            RUN MostrarMensaje IN W_Manija (INPUT 64, OUTPUT W_Rpta).
            FIND Cuentas WHERE W_Rowid = ROWID(Cuentas) NO-LOCK NO-ERROR.
          END.
        APPLY "ENTRY" TO W_Valor IN FRAME F-MAIN.
        RETURN NO-APPLY.
      END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Abre_Browser W-Win 
PROCEDURE Abre_Browser :
IF P_TipCta = "T" THEN DO:
     CASE R_Buscar:
      WHEN 1 THEN
       OPEN QUERY BROWSE-2 
       FOR EACH Cuentas WHERE 
                Cuentas.Cuenta BEGINS W_Valor AND
                Cuentas.Estado  EQ 1 NO-LOCK.
      WHEN 2 THEN
       OPEN QUERY BROWSE-2 
       FOR EACH Cuentas WHERE 
                Cuentas.Nombre BEGINS W_Valor AND
                Cuentas.Estado  EQ 1 NO-LOCK.
      WHEN 3 THEN
       OPEN QUERY BROWSE-2 
       FOR EACH Cuentas WHERE Cuentas.Estado  EQ 1 NO-LOCK.
     END CASE.
  END.
  IF P_TipCta = "A" THEN DO:
     CASE R_Buscar:
      WHEN 1 THEN
        OPEN QUERY BROWSE-2 
        FOR EACH Cuentas WHERE Cuentas.Tipo    EQ 2    AND
                               Cuentas.Estado  EQ 1    AND
                               Cuentas.Id_Paag EQ TRUE AND 
                               Cuentas.Cuenta BEGINS W_Valor NO-LOCK.
      WHEN 2 THEN
        OPEN QUERY BROWSE-2 
        FOR EACH Cuentas WHERE Cuentas.Tipo    EQ 2 
                           AND Cuentas.Estado  EQ 1
                           AND Cuentas.Id_Paag EQ TRUE 
                           AND Cuentas.Nombre BEGINS W_Valor NO-LOCK.
      WHEN 3 THEN
        OPEN QUERY BROWSE-2
        FOR EACH Cuentas WHERE Cuentas.Tipo    EQ 2 
                           AND Cuentas.Estado  EQ 1
                           AND Cuentas.Id_Paag EQ TRUE NO-LOCK.
     END CASE.
  END.
  IF P_TipCta = "R" THEN DO:
     CASE R_Buscar:
       WHEN 1 THEN
           OPEN QUERY BROWSE-2 
           FOR EACH Cuentas WHERE Cuentas.Tipo      EQ 2 
                              AND Cuentas.Estado    EQ 1
                              AND Cuentas.Id_Cuenta EQ 5 
                                                    AND Cuentas.Cuenta BEGINS W_Valor NO-LOCK.
       WHEN 2 THEN
           OPEN QUERY BROWSE-2 
           FOR EACH Cuentas WHERE Cuentas.Tipo      EQ 2 
                              AND Cuentas.Estado    EQ 1
                              AND Cuentas.Id_Cuenta EQ 5 
                              AND Cuentas.Nombre BEGINS W_Valor NO-LOCK.
       WHEN 3 THEN
           OPEN QUERY BROWSE-2 
           FOR EACH Cuentas WHERE Cuentas.Tipo      EQ 2 
                              AND Cuentas.Estado    EQ 1
                              AND Cuentas.Id_Cuenta EQ 5 NO-LOCK.
     END CASE.
  END.
  IF P_TipCta = "D" THEN DO:
     CASE R_Buscar:
       WHEN 1 THEN
             OPEN QUERY BROWSE-2
             FOR EACH Cuentas WHERE Cuentas.Tipo      EQ 2 
                                AND Cuentas.Estado    EQ 1
                                AND Cuentas.Id_Cuenta EQ 4 
                                AND Cuentas.Nombre BEGINS W_Valor NO-LOCK.
       WHEN 2 THEN
             OPEN QUERY BROWSE-2 
             FOR EACH Cuentas WHERE Cuentas.Tipo      EQ 2 
                                AND Cuentas.Estado    EQ 1
                                AND Cuentas.Id_Cuenta EQ 4 
                                AND Cuentas.Nombre BEGINS W_Valor NO-LOCK.
       WHEN 3 THEN
             OPEN QUERY BROWSE-2 
             FOR EACH Cuentas WHERE Cuentas.Tipo      EQ 2 
                                AND Cuentas.Estado    EQ 1
                                AND Cuentas.Id_Cuenta EQ 4 NO-LOCK.
     END CASE.
  END.
  ELSE DO:
    CASE R_Buscar:
      WHEN 1 THEN
            OPEN QUERY BROWSE-2 
            FOR EACH Cuentas WHERE Cuentas.Tipo   EQ 2 
                               AND Cuentas.Estado EQ 1
                               AND Cuentas.Cuenta BEGINS W_Valor NO-LOCK.
      WHEN 2 THEN
            OPEN QUERY BROWSE-2
            FOR EACH Cuentas WHERE Cuentas.Tipo   EQ 2 
                               AND Cuentas.Estado EQ 1
                               AND Cuentas.Nombre BEGINS W_Valor NO-LOCK.
      WHEN 3 THEN
            OPEN QUERY BROWSE-2 
            FOR EACH Cuentas WHERE Cuentas.Tipo   EQ 2 
                               AND Cuentas.Estado EQ 1 NO-LOCK.
    END CASE.         
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY R_Buscar W_Valor 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-143 BROWSE-2 R_Buscar W_Valor BT_FIN 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  APPLY "Value-changed" TO R_Buscar IN FRAME F-Main.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

