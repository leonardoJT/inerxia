&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-2 FILL-IN-3 BUTTON-3 BUTTON-4 ~
BtnDone BUTTON-2 EDITOR-2 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-2 FILL-IN-3 EDITOR-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY .81
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-2 
     LABEL "Tipos de Información" 
     SIZE 23 BY .81.

DEFINE BUTTON BUTTON-3 
     LABEL "Ver" 
     SIZE 6 BY .81.

DEFINE BUTTON BUTTON-4 
     LABEL "Imprimir" 
     SIZE 16 BY .81.

DEFINE VARIABLE COMBO-BOX-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Cedula" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Cedula","Nombre","Apellido1","Apellido2" 
     DROP-DOWN-LIST
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE EDITOR-2 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 110 BY 20.19 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Identificacion" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-5 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE TOGGLE-18 AS LOGICAL INITIAL yes 
     LABEL "General" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-19 AS LOGICAL INITIAL yes 
     LABEL "Ahorros" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-20 AS LOGICAL INITIAL yes 
     LABEL "Creditos" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-21 AS LOGICAL INITIAL yes 
     LABEL "Especiales" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-22 AS LOGICAL INITIAL yes 
     LABEL "Atrasos" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-23 AS LOGICAL INITIAL yes 
     LABEL "Familiar" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-24 AS LOGICAL INITIAL yes 
     LABEL "Garantias" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-25 AS LOGICAL INITIAL yes 
     LABEL "Beneficiarios" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-26 AS LOGICAL INITIAL yes 
     LABEL "Seguros" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-27 AS LOGICAL INITIAL yes 
     LABEL "Hoja Vida" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-28 AS LOGICAL INITIAL no 
     LABEL "Solo Saldos" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .77 NO-UNDO.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 6" 
     SIZE 9 BY 1.88.

DEFINE BUTTON BUTTON-7 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 7" 
     SIZE 9 BY 1.88.

DEFINE VARIABLE RADIO-SET-2 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Adicionar Encabezado", 1,
"Hoja Encabezado Preimpreso", 2
     SIZE 26 BY 1.62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     COMBO-BOX-2 AT ROW 1.27 COL 2 NO-LABEL
     FILL-IN-3 AT ROW 1.27 COL 24 COLON-ALIGNED
     BUTTON-3 AT ROW 1.27 COL 49
     BUTTON-4 AT ROW 1.27 COL 55
     BtnDone AT ROW 1.27 COL 71
     BUTTON-2 AT ROW 1.27 COL 89
     EDITOR-2 AT ROW 2.35 COL 2 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 22.04
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME FOpciones
     TOGGLE-18 AT ROW 1.27 COL 6
     TOGGLE-19 AT ROW 2.08 COL 6
     TOGGLE-20 AT ROW 2.88 COL 6
     TOGGLE-21 AT ROW 3.69 COL 6
     TOGGLE-22 AT ROW 4.5 COL 6
     TOGGLE-23 AT ROW 5.31 COL 6
     TOGGLE-24 AT ROW 6.12 COL 6
     TOGGLE-25 AT ROW 6.92 COL 6
     TOGGLE-26 AT ROW 7.73 COL 6
     TOGGLE-27 AT ROW 8.54 COL 6
     TOGGLE-28 AT ROW 9.35 COL 6
     BUTTON-5 AT ROW 10.42 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 89 ROW 2.08
         SIZE 23 BY 11.85
         FONT 4
         TITLE "Tipos de Información".

DEFINE FRAME Gimp
     RADIO-SET-2 AT ROW 1.27 COL 3 NO-LABEL
     BUTTON-6 AT ROW 3.15 COL 12
     BUTTON-7 AT ROW 3.15 COL 21
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 55 ROW 2.08
         SIZE 31 BY 5.12
         FONT 4
         TITLE "Imprimir".


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
         TITLE              = "Prototipo Consulta Clientes"
         HEIGHT             = 22.04
         WIDTH              = 114
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
/* REPARENT FRAME */
ASSIGN FRAME FOpciones:FRAME = FRAME fMain:HANDLE
       FRAME Gimp:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME Gimp:MOVE-AFTER-TAB-ITEM (BUTTON-2:HANDLE IN FRAME fMain)
       XXTABVALXX = FRAME FOpciones:MOVE-BEFORE-TAB-ITEM (EDITOR-2:HANDLE IN FRAME fMain)
       XXTABVALXX = FRAME Gimp:MOVE-BEFORE-TAB-ITEM (FRAME FOpciones:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR COMBO-BOX COMBO-BOX-2 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FOpciones
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FOpciones:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME Gimp
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Gimp:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Prototipo Consulta Clientes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Prototipo Consulta Clientes */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Tipos de Información */
DO:
  VIEW FRAME FOpciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON CHOOSE OF BUTTON-4 IN FRAME fMain /* Imprimir */
DO:
  VIEW FRAME gImp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FOpciones
&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 wWin
ON CHOOSE OF BUTTON-5 IN FRAME FOpciones /* Ocultar */
DO:
  HIDE FRAME FOpciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Gimp
&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON CHOOSE OF BUTTON-7 IN FRAME Gimp /* Button 7 */
DO:
  HIDE FRAME gImp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
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
  DISPLAY COMBO-BOX-2 FILL-IN-3 EDITOR-2 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE COMBO-BOX-2 FILL-IN-3 BUTTON-3 BUTTON-4 BtnDone BUTTON-2 EDITOR-2 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY RADIO-SET-2 
      WITH FRAME Gimp IN WINDOW wWin.
  ENABLE RADIO-SET-2 BUTTON-6 BUTTON-7 
      WITH FRAME Gimp IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-Gimp}
  DISPLAY TOGGLE-18 TOGGLE-19 TOGGLE-20 TOGGLE-21 TOGGLE-22 TOGGLE-23 TOGGLE-24 
          TOGGLE-25 TOGGLE-26 TOGGLE-27 TOGGLE-28 
      WITH FRAME FOpciones IN WINDOW wWin.
  ENABLE TOGGLE-18 TOGGLE-19 TOGGLE-20 TOGGLE-21 TOGGLE-22 TOGGLE-23 TOGGLE-24 
         TOGGLE-25 TOGGLE-26 TOGGLE-27 TOGGLE-28 BUTTON-5 
      WITH FRAME FOpciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FOpciones}
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

