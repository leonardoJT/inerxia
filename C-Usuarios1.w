&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER ipcUsu  LIKE Usuarios.Usuario.
DEFINE INPUT  PARAMETER ipiAge  LIKE Usuarios.Agencia.
DEFINE OUTPUT PARAMETER opcGrp  LIKE Usuarios.Grupo.
DEFINE OUTPUT PARAMETER opcUsu  LIKE Usuarios.usuario.
DEFINE OUTPUT PARAMETER opcNom  LIKE Usuarios.Nombre.


/* DEFINE OUTPUT PARAMETER W_Cue LIKE Ahorros.Cue_Ahorros. */

/* DEFINE VAR NomPro AS CHARACTER FORMAT "X(30)".           */
/* DEFINE VAR W_DetalleEstado AS CHARACTER FORMAT "X(20)".  */
/*                                                          */
/* DEFI VAR W_RowIdAho AS ROWID.                            */

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
&Scoped-define BROWSE-NAME B_Usuarios

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Ahorros

/* Definitions for BROWSE B_Usuarios                                    */
&Scoped-define FIELDS-IN-QUERY-B_Usuarios usuarios.Agencia Usuarios.Usuario Usuarios.Nit Usuarios.Nombre Usuarios.Grupo Usuarios.Fec_Creacion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Usuarios   
&Scoped-define SELF-NAME B_Usuarios
&Scoped-define OPEN-QUERY-B_Usuarios /*OPEN QUERY {&SELF-NAME} FOR EACH Ahorros WHERE     Ahorros.Nit EQ W_Nit AND Ahorros.Estado EQ 1 AND W_Nit GT "0" NO-LOCK INDEXED-REPOSITION.*/ RUN QUERY_Usuarios.
&Scoped-define TABLES-IN-QUERY-B_Usuarios Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-B_Usuarios Ahorros


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-B_Usuarios}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B_Usuarios R_Busca Btn_Salir W_Busca ~
RECT-282 
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
     SIZE 7 BY 1.62.

DEFINE VARIABLE W_Busca AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Usuario", 1,
"Nit", 2,
"Nombre", 3
     SIZE 29 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_Usuarios FOR 
      Ahorros SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_Usuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Usuarios wWin _FREEFORM
  QUERY B_Usuarios DISPLAY
      usuarios.Agencia        COLUMN-LABEL "Age" FORMAT "999":U 
    Usuarios.Usuario        COLUMN-LABEL "Usu" 
    Usuarios.Nit            COLUMN-LABEL "Nit" 
    Usuarios.Nombre         COLUMN-LABEL "Nombres" 
    Usuarios.Grupo          COLUMN-LABEL "Grp" 
    Usuarios.Fec_Creacion   COLUMN-LABEL "Fec.Creado"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97 BY 4.58
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B_Usuarios AT ROW 1.54 COL 3.72 WIDGET-ID 200
     R_Busca AT ROW 7 COL 3 NO-LABEL
     Btn_Salir AT ROW 6.65 COL 93
     W_Busca AT ROW 7 COL 32 COLON-ALIGNED NO-LABEL
     "Digite el contenido a Consultar dependiendo de la Selección" VIEW-AS TEXT
          SIZE 52 BY .62 AT ROW 6.31 COL 3.29
          FGCOLOR 7 
     RECT-282 AT ROW 6.65 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.43 BY 7.62
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
         TITLE              = "Consulta de Usuarios"
         COLUMN             = 3.57
         ROW                = 13.46
         HEIGHT             = 7.58
         WIDTH              = 103.14
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 15
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
/* BROWSE-TAB B_Usuarios 1 fMain */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Usuarios
/* Query rebuild information for BROWSE B_Usuarios
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Ahorros WHERE
    Ahorros.Nit EQ W_Nit AND Ahorros.Estado EQ 1 AND W_Nit GT "0" NO-LOCK INDEXED-REPOSITION.*/
RUN QUERY_Usuarios.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE B_Usuarios */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de Usuarios */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de Usuarios */
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
/*   IF B_Usuarios:NUM-SELECTED-ROWS > 0 THEN */
     ASSIGN opiAge  = Usuarios.Agencia
            opcGrp  = Usuarios.Grupo
            opcUsu  = Usuarios.Nit
            opcNom  = Usuarios.Nombre.
            
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


&Scoped-define SELF-NAME W_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Busca wWin
ON LEAVE OF W_Busca IN FRAME fMain
DO:
  ASSIGN FRAME {&FRAME-NAME} R_Busca W_Busca.
  CASE R_Busca:
      WHEN 1 THEN
          MESSAGE 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END CASE.



  IF R_Busca EQ 1 THEN FOR EACH Usuarios WHERE STRING(Usuarios.Grupo) EQ W_Busca AND Usuarios.Estado EQ 1:
     FIND FIRST Clientes WHERE Clientes.Nit EQ Usuarios.Nit NO-LOCK NO-ERROR.
     IF AVAIL(Clientes) THEN
        ASSIGN Usuarios.Nombre = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
  END.
  ELSE
   IF R_Busca EQ 2 THEN FOR EACH Usuarios WHERE Usuarios.Nit EQ W_Busca AND Usuarios.Estado EQ 1:
      FIND FIRST Clientes WHERE Clientes.Nit EQ Usuarios.Nit NO-LOCK NO-ERROR.
      IF AVAIL(Clientes) THEN
         ASSIGN Usuarios.Nombre = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
  END.
  CASE R_Busca:
          WHEN 1 THEN
              OPEN QUERY B_Usuarios FOR EACH Usuarios WHERE
                  STRING(Usuarios.Grupo) EQ W_Busca AND Usuarios.Estado EQ 1 NO-LOCK.
          WHEN 2 THEN
              OPEN QUERY B_Usuarios FOR EACH Usuarios WHERE
                  Usuarios.Nit EQ W_Busca AND Usuarios.Estado EQ 1 NO-LOCK.
          WHEN 3 THEN
              OPEN QUERY B_Usuarios FOR EACH Usuarios WHERE
                 Usuarios.Nombre BEGINS W_Busca AND Usuarios.Estado EQ 1 NO-LOCK.
  END CASE.
  /*REPOSITION B_Usuarios TO ROWID W_RowIdAho.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Usuarios
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
  ENABLE B_Usuarios R_Busca Btn_Salir W_Busca RECT-282 
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
  W_Busca:SCREEN-VALUE IN FRAME FMain = ipcUsu.

  IF ipcUsu LE "0" THEN DO:
     APPLY "Entry" TO W_Busca IN FRAME FMain.
     RETURN NO-APPLY.
  END.
/*APPLY "entry" TO W_Busca IN FRAME Fmain.
RETURN NO-APPLY.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QUERY_Usuarios wWin 
PROCEDURE QUERY_Usuarios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  OPEN QUERY B_Usuarios FOR EACH Usuarios WHERE
        usuarios.agencia EQ ipiAge AND Usuarios.usuario EQ ipcUsu AND 
        usuarios.estado EQ 1 NO-LOCK.

  IF ipcUsu LE "0" THEN DO:
     APPLY "Entry" TO W_Busca IN FRAME FMain.
     RETURN NO-APPLY.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

