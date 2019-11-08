&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  DEFINE OUTPUT PARAMETER P_Cuenta LIKE Presupuesto.Cod_presupuesto.
  DEFINE OUTPUT PARAMETER P_Nombre LIKE Presupuesto.Nombre.
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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Presupuesto

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 Presupuesto.Cod_presupuesto Presupuesto.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH Presupuesto NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH Presupuesto NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 Presupuesto
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 Presupuesto


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 R_Buscar W_Valor BT_FIN RECT-143 
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
          "Codigo", 1,
"Nombre", 2,
"Todos", 3
     SIZE 55 BY .69 TOOLTIP "Opción de Selección" NO-UNDO.

DEFINE RECTANGLE RECT-143
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58 BY 2.96.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      Presupuesto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      Presupuesto.Cod_presupuesto FORMAT "X(14)":U
      Presupuesto.Nombre FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 58 BY 6.73
         BGCOLOR 15 FGCOLOR 7 FONT 5 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-2 AT ROW 1 COL 1
     R_Buscar AT ROW 9.88 COL 5 NO-LABEL
     W_Valor AT ROW 10.69 COL 3 COLON-ALIGNED NO-LABEL
     BT_FIN AT ROW 12.58 COL 52
     RECT-143 AT ROW 9.08 COL 3
     "  Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 30 BY 1.08 AT ROW 8.54 COL 5
          FGCOLOR 7 
     "Nota:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 12.58 COL 4
          FGCOLOR 7 
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 12.58 COL 10
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 13.38 COL 10
          FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.5
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
         TITLE              = "Consulta Cuentas de Presupuestos"
         HEIGHT             = 13.5
         WIDTH              = 62.29
         MAX-HEIGHT         = 22.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.35
         VIRTUAL-WIDTH      = 114.29
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
                                                                        */
/* BROWSE-TAB BROWSE-2 1 F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "bdcentral.Presupuesto"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = bdcentral.Presupuesto.Cod_presupuesto
     _FldNameList[2]   = bdcentral.Presupuesto.Nombre
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Consulta Cuentas de Presupuestos */
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
    IF AVAILABLE(Presupuesto) THEN 
       ASSIGN P_Cuenta =  Presupuesto.Cod_presupuesto
              P_Nombre   = Presupuesto.Nombre.
    APPLY "CHOOSE" TO Bt_Fin.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 W-Win
ON VALUE-CHANGED OF BROWSE-2 IN FRAME F-Main
DO:
    IF AVAILABLE(Presupuesto) THEN 
       ASSIGN P_Cuenta =  Presupuesto.Cod_presupuesto
              P_Nombre   = Presupuesto.Nombre.
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
               W_Rowid = ROWID(Presupuesto).
        RUN Abre_Browser.
        IF AVAILABLE (Presupuesto) THEN
          DO:
           ASSIGN P_Cuenta = Presupuesto.Cod_presupuesto
                  P_Nombre = Presupuesto.Nombre.
          END.
        ELSE
          DO:
            RUN MostrarMensaje IN W_Manija (INPUT 64, OUTPUT W_Rpta).
            FIND Presupuesto WHERE W_Rowid = ROWID(Presupuesto) NO-LOCK NO-ERROR.
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
CASE R_Buscar:
      WHEN 1 THEN DO:
           OPEN QUERY BROWSE-2 
           FOR EACH Presupuesto WHERE 
                Presupuesto.Cod_presupuesto EQ W_Valor AND
                Cuentas.Estado  EQ 1 NO-LOCK. 
       END.
       WHEN 2 THEN DO:
           OPEN QUERY BROWSE-2 
           FOR EACH Presupuesto WHERE 
                Presupuesto.Nombre BEGINS W_Valor AND
                Presupuesto.Estado  EQ 1 NO-LOCK. 
       END.
       WHEN 3 THEN DO:
           OPEN QUERY BROWSE-2 
           FOR EACH Presupuesto WHERE Presupuesto.Estado  EQ 1 NO-LOCK. 
       END.
END CASE.
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
  ENABLE BROWSE-2 R_Buscar W_Valor BT_FIN RECT-143 
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
  {src/adm/template/snd-list.i "Presupuesto"}

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

