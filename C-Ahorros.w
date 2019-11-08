&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER W_Nit LIKE Ahorros.Nit.
DEFINE OUTPUT PARAMETER W_Age LIKE Ahorros.Agencia.
DEFINE OUTPUT PARAMETER W_Pro LIKE Ahorros.Cod_Ahorro.
DEFINE OUTPUT PARAMETER W_NitW LIKE Ahorros.Nit.
DEFINE OUTPUT PARAMETER W_Cue LIKE Ahorros.Cue_Ahorros.

DEFINE VAR NomPro AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_DetalleEstado AS CHARACTER FORMAT "X(20)".

DEFI VAR W_RowIdAho AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME B_Ahorros

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Ahorros

/* Definitions for BROWSE B_Ahorros                                     */
&Scoped-define FIELDS-IN-QUERY-B_Ahorros Ahorros.Agencia Ahorros.Cod_Ahorro NomPro Ahorros.Cue_Ahorros Ahorros.num_formato Ahorros.Nit Ahorros.IdNombre Ahorros.IdApellido1 W_DetalleEstado Ahorros.Sdo_Disponible Ahorros.Sdo_Minimo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Ahorros   
&Scoped-define SELF-NAME B_Ahorros
&Scoped-define OPEN-QUERY-B_Ahorros /*OPEN QUERY {&SELF-NAME} FOR EACH Ahorros WHERE     Ahorros.Nit EQ W_Nit AND Ahorros.Estado EQ 1 AND W_Nit GT "0" NO-LOCK INDEXED-REPOSITION.*/  RUN QUERY_Aho.
&Scoped-define TABLES-IN-QUERY-B_Ahorros Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-B_Ahorros Ahorros


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-B_Ahorros}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS R_Busca B_Ahorros Btn_Salir W_Busca RECT-282 
&Scoped-Define DISPLAYED-OBJECTS R_Busca W_Busca 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 96" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE W_Busca AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nit", 1,
"Cuenta", 2,
"Nombre", 3,
"Apellido1", 4
     SIZE 41 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_Ahorros FOR 
      Ahorros SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_Ahorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Ahorros wWin _FREEFORM
  QUERY B_Ahorros NO-LOCK DISPLAY
      Ahorros.Agencia COLUMN-LABEL "Age" FORMAT "999":U 
      Ahorros.Cod_Ahorro COLUMN-LABEL "Pto" FORMAT "999":U
      NomPro  COLUMN-LABEL "Nombre Producto" FORMAT "X(21)"
      Ahorros.Cue_Ahorros FORMAT "X(14)":U
      Ahorros.num_formato FORMAT "X(14)":U
      Ahorros.Nit FORMAT "X(12)":U
      Ahorros.IdNombre    FORMAT "X(15)"
      Ahorros.IdApellido1 FORMAT "X(15)"
      W_DetalleEstado     FORMAT "X(13)" COLUMN-LABEL "Estado" 
      Ahorros.Sdo_Disponible FORMAT "->>>>>,>>>,>>9":U COLUMN-LABEL "Sdo-Disponible"
      Ahorros.Sdo_Minimo     FORMAT "->>>>>,>>>,>>9":U COLUMN-LABEL "Saldo-Mínimo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 100 BY 4.85
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     R_Busca AT ROW 7 COL 3 NO-LABEL
     B_Ahorros AT ROW 1.27 COL 2
     Btn_Salir AT ROW 6.38 COL 94
     W_Busca AT ROW 7 COL 43 COLON-ALIGNED NO-LABEL
     "Digite el contenido a Consultar dependiendo de la Selección" VIEW-AS TEXT
          SIZE 52 BY .62 AT ROW 6.31 COL 3.29
          FGCOLOR 7 
     RECT-282 AT ROW 6.65 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.43 BY 7.5
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta de Cuentas de Ahorro"
         COLUMN             = 4.29
         ROW                = 6.5
         HEIGHT             = 7.5
         WIDTH              = 103.43
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/desktop.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/desktop.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB B_Ahorros R_Busca fMain */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Ahorros
/* Query rebuild information for BROWSE B_Ahorros
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Ahorros WHERE
    Ahorros.Nit EQ W_Nit AND Ahorros.Estado EQ 1 AND W_Nit GT "0" NO-LOCK INDEXED-REPOSITION.*/
 RUN QUERY_Aho.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Ahorros */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de Cuentas de Ahorro */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de Cuentas de Ahorro */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME fMain /* Button 96 */
DO:
  IF B_Ahorros:NUM-SELECTED-ROWS > 0 THEN                 
     ASSIGN W_Age  = Ahorros.Agencia
            W_Pro  = Ahorros.Cod_Ahorro
            W_Cue  = Ahorros.Cue_Ahorros
            W_NitW = Ahorros.Nit.
            
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


&Scoped-define BROWSE-NAME B_Ahorros
&Scoped-define SELF-NAME B_Ahorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Ahorros wWin
ON MOUSE-SELECT-CLICK OF B_Ahorros IN FRAME fMain
DO:
  ASSIGN W_RowIdAho = ROWID(Ahorros).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Ahorros wWin
ON MOUSE-SELECT-DBLCLICK OF B_Ahorros IN FRAME fMain
DO:
  APPLY "choose" TO Btn_Salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Ahorros wWin
ON ROW-DISPLAY OF B_Ahorros IN FRAME fMain
DO:
  FIND Pro_Ahorros WHERE 
       Pro_Ahorros.Tip_Ahorro EQ Ahorros.Tip_Ahorro AND
       Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro AND 
       Pro_Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAILABLE Pro_Ahorros THEN
     NomPro = Pro_Ahorros.Nom_Producto.
  ELSE NomPro = "No Disponible".
  
  FIND Varios WHERE Varios.Tipo EQ 21 AND Varios.Codigo EQ Ahorros.Detalle_Estado NO-LOCK NO-ERROR.
  IF AVAILABLE Varios THEN
     W_DetalleEstado = Varios.Descripcion.
  ELSE W_DetalleEstado = "Detalle No Econtrado".
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Busca wWin
ON LEAVE OF W_Busca IN FRAME fMain
DO:
    ASSIGN FRAME {&FRAME-NAME} R_Busca W_Busca.

    IF R_Busca EQ 1 THEN
        FOR EACH Ahorros WHERE Ahorros.Nit EQ W_Busca /*AND Ahorros.Estado EQ 1*/:
            FIND FIRST Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
            IF AVAIL(Clientes) THEN
                ASSIGN Ahorros.IdApellido1 = Clientes.Apellido1
                       Ahorros.IdNombre = Clientes.Nombre.
        END.
    ELSE
        IF R_Busca EQ 2 THEN
            FOR EACH Ahorros WHERE Ahorros.Cue_Ahorros EQ W_Busca /*AND Ahorros.Estado EQ 1*/:
                FIND FIRST Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
                IF AVAIL(Clientes) THEN
                    ASSIGN Ahorros.IdApellido1 = Clientes.Apellido1
                           Ahorros.IdNombre = Clientes.Nombre.
            END.

    CASE R_Busca:
        WHEN 1 THEN OPEN QUERY B_Ahorros FOR EACH Ahorros WHERE Ahorros.Nit EQ W_Busca /*AND Ahorros.Estado EQ 1*/ NO-LOCK.
        WHEN 2 THEN OPEN QUERY B_Ahorros FOR EACH Ahorros WHERE Ahorros.Cue_Ahorro EQ W_Busca /*AND Ahorros.Estado EQ 1*/ NO-LOCK.
        WHEN 3 THEN OPEN QUERY B_Ahorros FOR EACH Ahorros WHERE Ahorros.IdNombre BEGINS W_Busca /*AND Ahorros.Estado EQ 1*/ NO-LOCK.
        WHEN 4 THEN OPEN QUERY B_Ahorros FOR EACH Ahorros WHERE Ahorros.IdApellido1 BEGINS W_Busca /*AND Ahorros.Estado EQ 1*/ NO-LOCK.
    END CASE.

    /*REPOSITION B_Ahorros TO ROWID W_RowIdAho.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY R_Busca W_Busca 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE R_Busca B_Ahorros Btn_Salir W_Busca RECT-282 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  W_Busca:SCREEN-VALUE IN FRAME FMain = W_Nit.

  IF W_Nit LE "0" THEN DO:
     APPLY "Entry" TO W_Busca IN FRAME FMain.
     RETURN NO-APPLY.
  END.
/*APPLY "entry" TO W_Busca IN FRAME Fmain.
RETURN NO-APPLY.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Query_Aho wWin 
PROCEDURE Query_Aho :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OPEN QUERY B_Ahorros FOR EACH Ahorros WHERE
       Ahorros.Nit EQ W_Nit AND Ahorros.Estado EQ 1 AND W_Nit GT "0" NO-LOCK.

  IF W_Nit LE "0" THEN DO:
     APPLY "Entry" TO W_Busca IN FRAME FMain.
     RETURN NO-APPLY.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

