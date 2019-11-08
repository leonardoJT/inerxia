&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
  
    DEFINE OUTPUT PARAMETER P_Codbase LIKE Base_Ret.Cod_Base.
    DEFINE OUTPUT PARAMETER P_Nombre  LIKE Base_Ret.Nombre.
    DEFINE OUTPUT PARAMETER P_Porcent LIKE Base_Ret.Porcentaje.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

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
&Scoped-define BROWSE-NAME BR_Bases

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Base_Ret

/* Definitions for BROWSE BR_Bases                                      */
&Scoped-define FIELDS-IN-QUERY-BR_Bases Base_Ret.Cod_base Base_Ret.Nombre ~
Base_Ret.Porcentaje 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR_Bases 
&Scoped-define QUERY-STRING-BR_Bases FOR EACH Base_Ret ~
      WHERE Base_Ret.Estado = 1 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR_Bases OPEN QUERY BR_Bases FOR EACH Base_Ret ~
      WHERE Base_Ret.Estado = 1 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR_Bases Base_Ret
&Scoped-define FIRST-TABLE-IN-QUERY-BR_Bases Base_Ret


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BR_Bases}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BR_Bases R_Busca Btn_Salir 
&Scoped-Define DISPLAYED-OBJECTS R_Busca Busca 

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
     LABEL "Button 98" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE Busca AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Código", 1,
"Nombre", 2,
"Todos", 3
     SIZE 31 BY .81
     FONT 5 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR_Bases FOR 
      Base_Ret SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR_Bases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR_Bases wWin _STRUCTURED
  QUERY BR_Bases NO-LOCK DISPLAY
      Base_Ret.Cod_base FORMAT "X(4)":U
      Base_Ret.Nombre FORMAT "X(30)":U
      Base_Ret.Porcentaje FORMAT "99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 49 BY 6.46
         BGCOLOR 15 FONT 4 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BR_Bases AT ROW 1.27 COL 2
     R_Busca AT ROW 8 COL 2 NO-LABEL
     Btn_Salir AT ROW 8.27 COL 41
     Busca AT ROW 9.08 COL 1 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50.57 BY 9.23
         BGCOLOR 17 FGCOLOR 0 FONT 4.


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
         TITLE              = "Consula de Bases de Retención"
         HEIGHT             = 9.23
         WIDTH              = 50.57
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
/* BROWSE-TAB BR_Bases 1 fMain */
/* SETTINGS FOR FILL-IN Busca IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR_Bases
/* Query rebuild information for BROWSE BR_Bases
     _TblList          = "bdCentral.Base_Ret"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Base_Ret.Estado = 1"
     _FldNameList[1]   = bdCentral.Base_Ret.Cod_base
     _FldNameList[2]   = bdCentral.Base_Ret.Nombre
     _FldNameList[3]   = bdCentral.Base_Ret.Porcentaje
     _Query            is OPENED
*/  /* BROWSE BR_Bases */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consula de Bases de Retención */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consula de Bases de Retención */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR_Bases
&Scoped-define SELF-NAME BR_Bases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR_Bases wWin
ON MOUSE-SELECT-DBLCLICK OF BR_Bases IN FRAME fMain
DO:
  APPLY 'choose' TO Btn_Salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME fMain /* Button 98 */
DO:
  IF Br_Bases:NUM-SELECTED-ROWS > 0 THEN
      ASSIGN P_Codbase = Base_Ret.Cod_Base
             P_Nombre  = Base_Ret.Nombre
             P_Porcent = Base_Ret.Porcentaje.
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


&Scoped-define SELF-NAME Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Busca wWin
ON LEAVE OF Busca IN FRAME fMain
DO:
   CASE R_Busca:SCREEN-VALUE:
     WHEN "1" THEN DO:
         OPEN QUERY BR_Bases FOR EACH Base_Ret
             WHERE Base_Ret.Cod_Base EQ SELF:SCREEN-VALUE AND
                   Base_Ret.Estado EQ 1
                   NO-LOCK.
     END.
     WHEN "2" THEN DO:
         OPEN QUERY BR_Bases FOR EACH Base_Ret
             WHERE Base_Ret.Nombre EQ SELF:SCREEN-VALUE AND
                   Base_Ret.Estado EQ 1
                   NO-LOCK.
     END.
     WHEN "3" THEN DO:
         OPEN QUERY BR_Bases FOR EACH Base_Ret
             WHERE Base_Ret.Estado EQ 1
                   NO-LOCK.
     END.
 END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Busca wWin
ON VALUE-CHANGED OF R_Busca IN FRAME fMain
DO:
DO WITH FRAME {&FRAME-NAME}:
  IF R_Busca:SCREEN-VALUE EQ "3" THEN DO:
      DISABLE Busca.
      OPEN QUERY BR_Bases FOR EACH Base_Ret
           WHERE Base_Ret.Estado EQ 1
           NO-LOCK.
  END.
  ELSE DO:
      ENABLE Busca.
  END.
  END.
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
  DISPLAY R_Busca Busca 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE BR_Bases R_Busca Btn_Salir 
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
  ENABLE Busca WITH FRAME FMain.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

