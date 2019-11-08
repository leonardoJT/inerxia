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
   
   {incluido\Variable.i "SHARED"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FBuscar

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Opc Buscar BUTTON-118 RECT-217 
&Scoped-Define DISPLAYED-OBJECTS Opc Buscar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cliente_sencillo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d-Cliente_Sencillo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav91 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cliente_sencillo AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-118 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 118" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Opc AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Agencia", 1,
"Nit", 2,
"Nombre", 3,
"Apellido1", 4,
"Apellido2", 5
     SIZE 59 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-217
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 1.62.

DEFINE BUTTON BUTTON-117 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 117" 
     SIZE 11 BY 1.65.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-117 AT ROW 1.27 COL 71
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83.14 BY 12.35
         BGCOLOR 17 FONT 4.

DEFINE FRAME FBuscar
     Opc AT ROW 8.54 COL 4 NO-LABEL
     Buscar AT ROW 10.15 COL 2 COLON-ALIGNED NO-LABEL
     BUTTON-118 AT ROW 10.15 COL 56
     RECT-217 AT ROW 8.27 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 16 ROW 1.27
         SIZE 66 BY 11.85
         BGCOLOR 17 FONT 5
         TITLE "Buscar Cliente".


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
         TITLE              = "Creación de Personas y Entidades (Información Básica)"
         HEIGHT             = 12.35
         WIDTH              = 83.14
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/keybrd02.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/keybrd02.ico"
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
/* REPARENT FRAME */
ASSIGN FRAME FBuscar:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME FBuscar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FBuscar:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME fMain
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Creación de Personas y Entidades (Información Básica) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Creación de Personas y Entidades (Información Básica) */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Buscar wWin
ON LEAVE OF Buscar IN FRAME FBuscar
OR RETURN OF Buscar DO:
  DEFINE VARIABLE palabra AS CHARACTER NO-UNDO.
  IF SELF:SCREEN-VALUE NE "":U THEN DO:
     DEFINE VARIABLE op      AS INTEGER NO-UNDO.
     ASSIGN op = opc:INPUT-VALUE.
     CASE op:
        WHEN 1 THEN DO:
          FIND FIRST Clientes WHERE STRING(Clientes.Agencia) BEGINS SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
          ASSIGN palabra = "STRING(Clientes.Agencia) BEGINS '" + SELF:SCREEN-VALUE + "'".
        END.
        WHEN 2 THEN DO:
          FIND FIRST Clientes WHERE STRING(Clientes.Nit) BEGINS SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
          ASSIGN palabra = "STRING(Clientes.Nit) BEGINS '" + SELF:SCREEN-VALUE + "'".
        END.
        WHEN 3 THEN DO:
          FIND FIRST Clientes WHERE Clientes.Nombre BEGINS SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
          ASSIGN palabra = "Clientes.Nombre BEGINS '" + SELF:SCREEN-VALUE + "'".
        END.
        WHEN 4 THEN DO:
          FIND FIRST Clientes WHERE Clientes.Apellido1 BEGINS SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
          ASSIGN palabra = "Clientes.Apellido1 BEGINS '" + SELF:SCREEN-VALUE + "'".
        END.
         WHEN 5 THEN DO:
           FIND FIRST Clientes WHERE Clientes.Apellido2 BEGINS SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
           ASSIGN palabra = "Clientes.Apellido2 BEGINS '" + SELF:SCREEN-VALUE + "'".
         END.
     END CASE.
    END.
    IF AVAILABLE Clientes THEN DO:
      {SET QueryWhere palabra h_d-Cliente_Sencillo}.
      {FN OpenQuery h_d-Cliente_Sencillo}.
     END.
   ELSE DO:
     FIND FIRST Clientes NO-LOCK NO-ERROR.
     {FN OpenQuery h_d-Cliente_Sencillo}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-117
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-117 wWin
ON CHOOSE OF BUTTON-117 IN FRAME fMain /* Button 117 */
DO:
  VIEW FRAME FBuscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FBuscar
&Scoped-define SELF-NAME BUTTON-118
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-118 wWin
ON CHOOSE OF BUTTON-118 IN FRAME FBuscar /* Button 118 */
DO:
  HIDE FRAME FBuscar.
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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'd-Cliente_Sencillo.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed-Cliente_SencilloUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d-Cliente_Sencillo ).
       RUN repositionObject IN h_d-Cliente_Sencillo ( 1.00 , 38.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'b-cliente_sencillo.w':U ,
             INPUT  FRAME FBuscar:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b-cliente_sencillo ).
       RUN repositionObject IN h_b-cliente_sencillo ( 1.27 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_b-cliente_sencillo ( 6.65 , 64.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'v-cliente_sencillo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_v-cliente_sencillo ).
       RUN repositionObject IN h_v-cliente_sencillo ( 1.54 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 11.04 , 64.29 ) */

       RUN constructObject (
             INPUT  'p-updsav91.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordEdgePixels2PanelTypeSaveDeactivateTargetOnHidenoDisabledActionsHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_p-updsav91 ).
       RUN repositionObject IN h_p-updsav91 ( 3.15 , 70.00 ) NO-ERROR.
       RUN resizeObject IN h_p-updsav91 ( 9.42 , 13.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_b-cliente_sencillo. */
       RUN addLink ( h_d-Cliente_Sencillo , 'Data':U , h_b-cliente_sencillo ).

       /* Links to SmartDataViewer h_v-cliente_sencillo. */
       RUN addLink ( h_d-Cliente_Sencillo , 'Data':U , h_v-cliente_sencillo ).
       RUN addLink ( h_v-cliente_sencillo , 'Update':U , h_d-Cliente_Sencillo ).
       RUN addLink ( h_p-updsav91 , 'TableIO':U , h_v-cliente_sencillo ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_b-cliente_sencillo ,
             Opc:HANDLE IN FRAME FBuscar , 'BEFORE':U ).
       RUN adjustTabOrder ( h_v-cliente_sencillo ,
             BUTTON-117:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_p-updsav91 ,
             h_v-cliente_sencillo , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  ENABLE BUTTON-117 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY Opc Buscar 
      WITH FRAME FBuscar IN WINDOW wWin.
  ENABLE Opc Buscar BUTTON-118 RECT-217 
      WITH FRAME FBuscar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FBuscar}
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
  HIDE FRAME FBuscar.
  
  WWin:MOVE-TO-TOP().  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

