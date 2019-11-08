&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

{src/adm2/widgetprto.i}
        
{incluido\Variable.i "SHARED"}
DEFINE INPUT PARAMETER WUsu LIKE Usuarios.Usuario.
DEFINE INPUT PARAMETER WNomUsu LIKE Usuarios.Nombre.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cajero

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain Cajero.fecha Cajero.salini 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain Cajero.fecha Cajero.salini 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain Cajero
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain Cajero
&Scoped-define QUERY-STRING-fMain FOR EACH Cajero SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH Cajero SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain Cajero
&Scoped-define FIRST-TABLE-IN-QUERY-fMain Cajero


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Cajero.fecha Cajero.salini 
&Scoped-define ENABLED-TABLES Cajero
&Scoped-define FIRST-ENABLED-TABLE Cajero
&Scoped-Define ENABLED-OBJECTS RECT-232 BGra BtnDone 
&Scoped-Define DISPLAYED-FIELDS Cajero.fecha Cajero.salini 
&Scoped-define DISPLAYED-TABLES Cajero
&Scoped-define FIRST-DISPLAYED-TABLE Cajero
&Scoped-Define DISPLAYED-OBJECTS Nombre WEstado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Cajero.fecha Cajero.salini 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BGra 
     LABEL "Grabar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "Salir" 
     SIZE 15 BY 1.12
     BGCOLOR 8 .

DEFINE VARIABLE Nombre AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE WEstado AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-232
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 42 BY 2.96.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      Cajero SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Nombre AT ROW 1.27 COL 2 NO-LABEL
     Cajero.fecha AT ROW 2.62 COL 20 COLON-ALIGNED
          LABEL "Fecha"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Cajero.salini AT ROW 3.69 COL 20 COLON-ALIGNED
          LABEL "Saldo Inicial del Día"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     WEstado AT ROW 5.04 COL 2 NO-LABEL
     BGra AT ROW 6.12 COL 6
     BtnDone AT ROW 6.12 COL 25
     RECT-232 AT ROW 2.08 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 43.86 BY 6.69
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.


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
         TITLE              = "Grabar la Base para el Cajero"
         HEIGHT             = 6.58
         WIDTH              = 43.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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
                                                                        */
/* SETTINGS FOR FILL-IN Cajero.fecha IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Nombre IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN Cajero.salini IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN WEstado IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "bdcentral.Cajero"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Grabar la Base para el Cajero */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Grabar la Base para el Cajero */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BGra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BGra wWin
ON CHOOSE OF BGra IN FRAME fMain /* Grabar */
DO:
  FIND Cajero WHERE Cajero.Usuario EQ WUsu AND
                    Cajero.Fecha   EQ DATE(Cajero.Fecha:SCREEN-VALUE) NO-ERROR.
  IF NOT AVAILABLE Cajero THEN CREATE Cajero.
  ASSIGN WEstado:SCREEN-VALUE = "La caja para este dia esta abierta"
         Cajero.Estado    = 1
         Cajero.Fecha     = DATE(Cajero.Fecha:SCREEN-VALUE)
         Cajero.salini    = DECIMAL(Cajero.SalIni:SCREEN-VALUE)
         Cajero.Usuario   = WUsu.
  DISABLE BGra WITH FRAME Fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain /* Salir */
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

  {&OPEN-QUERY-fMain}
  GET FIRST fMain.
  DISPLAY Nombre WEstado 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE Cajero THEN 
    DISPLAY Cajero.fecha Cajero.salini 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-232 Cajero.fecha Cajero.salini BGra BtnDone 
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
RUN SUPER.

  ENABLE {&List-1} WITH FRAME Fmain.  
  Cajero.Fecha:SCREEN-VALUE IN FRAME FMain = STRING(W_Fecha).
  Nombre:SCREEN-VALUE = WNomUsu.
  FIND Cajero WHERE Cajero.Usuario EQ WUsu AND
                    Cajero.Fecha   EQ W_Fecha NO-ERROR.
  IF AVAILABLE Cajero THEN DO:
     ASSIGN Cajero.Fecha:SCREEN-VALUE = STRING(Cajero.Fecha)
            Cajero.SalIni:SCREEN-VALUE = STRING(Cajero.SalIni).
     WEstado:SCREEN-VALUE = "La caja para este dia esta abierta".
     IF Cajero.Estado EQ 2 THEN DO:
        DISABLE {&List-1} BGra WITH FRAME Fmain.
        WEstado:SCREEN-VALUE = "La caja para este dia esta Cerrada".
     END.
  END.
  ELSE 
     ASSIGN WEstado:SCREEN-VALUE = "Aun no se ha creado la base para este día".
            Cajero.SalIni:SCREEN-VALUE = "0".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

