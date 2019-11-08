&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
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


DEFINE VARIABLE W_pag AS INTEGER    NO-UNDO.

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_orden B_pagina BtnDone BUTTON-143 RECT-1 ~
RECT-3 RECT-4 RECT-5 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS W_orden 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dregalos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dregalos_entrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dregalos_rango AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-Regalos_Rango AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar_regalos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar_Regalos_Entr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vregalos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vregalos_entrega_2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vregalos_rango AS HANDLE NO-UNDO.
DEFINE VARIABLE h_wfregalos AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 6 BY 1.5 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 6 BY 1.5.

DEFINE BUTTON B_pagina DEFAULT 
     IMAGE-UP FILE "imagenes/135.ico":U
     LABEL "&Consulta" 
     SIZE 6 BY 1.5 TOOLTIP "Consulta".

DEFINE VARIABLE W_orden AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Regalo","regalo",
                     "Nombre","nombre"
     DROP-DOWN-LIST
     SIZE 24.14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 127.86 BY 19.04.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 3.15.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 78.86 BY 3.15.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.14 BY 1.92.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 75.14 BY 1.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     W_orden AT ROW 2.12 COL 10.86 COLON-ALIGNED HELP
          "Seleccione opción de ordenación" NO-LABEL
     B_pagina AT ROW 1.85 COL 106
     BtnDone AT ROW 1.85 COL 112
     BUTTON-143 AT ROW 1.85 COL 100 WIDGET-ID 68
     "Ordenar Por:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.27 COL 13
          FGCOLOR 2 FONT 6
     RECT-1 AT ROW 4.23 COL 1
     RECT-3 AT ROW 1 COL 9
     RECT-4 AT ROW 1 COL 42
     RECT-5 AT ROW 1.62 COL 10.86
     RECT-7 AT ROW 1.62 COL 43.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127.86 BY 22.31
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 3
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Entrega de Regalos"
         HEIGHT             = 22.31
         WIDTH              = 127.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 160
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Entrega de Regalos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Entrega de Regalos */
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
ON CHOOSE OF BtnDone IN FRAME fMain
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


&Scoped-define SELF-NAME BUTTON-143
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-143 wWin
ON CHOOSE OF BUTTON-143 IN FRAME fMain /* Button 143 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_pagina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_pagina wWin
ON CHOOSE OF B_pagina IN FRAME fMain /* Consulta */
DO:
    IF SELF:LABEL = "&Consulta" THEN DO:
        ASSIGN SELF:LABEL = "&Edición"
            SELF:TOOLTIP = "Edición".
        RUN SelectPage(2).
        RUN opcion_sdb.
    END.
    ELSE DO:
        ASSIGN SELF:LABEL = "&Consulta"
            SELF:TOOLTIP = "Consulta".
        RUN SelectPage(1).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_pagina wWin
ON ENTRY OF B_pagina IN FRAME fMain /* Consulta */
DO:
  ASSIGN W_pag = getCurrentPage().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_orden
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_orden wWin
ON VALUE-CHANGED OF W_orden IN FRAME fMain
DO:
  DYNAMIC-FUNCTION('setQuerySort':U IN h_dregalos, INPUT SELF:SCREEN-VALUE).
  DYNAMIC-FUNCTION('OpenQuery':U IN h_dregalos).
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
             INPUT  'dregalos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedregalosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dregalos ).
       RUN repositionObject IN h_dregalos ( 1.27 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dregalos_rango.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsregalos_rango.regalo,regaloObjectNamedregalos_rangoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dregalos_rango ).
       RUN repositionObject IN h_dregalos_rango ( 2.88 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dregalos_entrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsregalos_entrega.regalo,regalo,regalos_entrega.agencia,agenciaObjectNamedregalos_entregaOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dregalos_entrega ).
       RUN repositionObject IN h_dregalos_entrega ( 4.50 , 9.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Mnto. Regalos|Programación Regalos|Entrega Regalos' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder-2 ).
       RUN repositionObject IN h_folder-2 ( 5.69 , 6.14 ) NO-ERROR.
       RUN resizeObject IN h_folder-2 ( 16.15 , 119.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1,2,3':U) NO-ERROR.

       /* Links to SmartDataObject h_dregalos. */
       RUN addLink ( h_dyntoolbar_regalos , 'Navigation':U , h_dregalos ).
       RUN addLink ( h_wfregalos , 'Filter':U , h_dregalos ).

       /* Links to SmartDataObject h_dregalos_rango. */
       RUN addLink ( h_dregalos , 'Data':U , h_dregalos_rango ).
       RUN addLink ( h_dyntoolbar-Regalos_Rango , 'Navigation':U , h_dregalos_rango ).

       /* Links to SmartDataObject h_dregalos_entrega. */
       RUN addLink ( h_dregalos_rango , 'Data':U , h_dregalos_entrega ).
       RUN addLink ( h_dyntoolbar_Regalos_Entr , 'Navigation':U , h_dregalos_entrega ).

       /* Links to SmartFolder h_folder-2. */
       RUN addLink ( h_folder-2 , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder-2 ,
             W_orden:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'wfregalos.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInityesDisableOnInityesObjectLayout':U ,
             OUTPUT h_wfregalos ).
       /* Position in AB:  ( 4.73 , 2.00 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'vregalos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vregalos ).
       RUN repositionObject IN h_vregalos ( 8.54 , 26.00 ) NO-ERROR.
       /* Size in AB:  ( 12.12 , 79.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,FunctionTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar_regalos ).
       RUN repositionObject IN h_dyntoolbar_regalos ( 2.08 , 44.86 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar_regalos ( 1.00 , 44.86 ) NO-ERROR.

       /* Links to SmartWindow h_wfregalos. */
       RUN addLink ( h_dregalos , 'Data':U , h_wfregalos ).

       /* Links to SmartDataViewer h_vregalos. */
       RUN addLink ( h_dregalos , 'Data':U , h_vregalos ).
       RUN addLink ( h_vregalos , 'Update':U , h_dregalos ).
       RUN addLink ( h_dyntoolbar_regalos , 'TableIO':U , h_vregalos ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar_regalos ,
             BUTTON-143:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_vregalos ,
             h_dyntoolbar_regalos , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'vregalos_rango.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vregalos_rango ).
       RUN repositionObject IN h_vregalos_rango ( 8.54 , 25.86 ) NO-ERROR.
       /* Size in AB:  ( 12.12 , 79.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,FunctionTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-Regalos_Rango ).
       RUN repositionObject IN h_dyntoolbar-Regalos_Rango ( 2.08 , 45.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-Regalos_Rango ( 1.08 , 46.29 ) NO-ERROR.

       /* Links to SmartDataViewer h_vregalos_rango. */
       RUN addLink ( h_dregalos_rango , 'Data':U , h_vregalos_rango ).
       RUN addLink ( h_vregalos_rango , 'Update':U , h_dregalos_rango ).
       RUN addLink ( h_dyntoolbar-Regalos_Rango , 'TableIO':U , h_vregalos_rango ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar-Regalos_Rango ,
             BUTTON-143:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_vregalos_rango ,
             h_dyntoolbar-Regalos_Rango , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'vregalos_entrega_2.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vregalos_entrega_2 ).
       RUN repositionObject IN h_vregalos_entrega_2 ( 8.73 , 26.14 ) NO-ERROR.
       /* Size in AB:  ( 11.04 , 79.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,FunctionTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar_Regalos_Entr ).
       RUN repositionObject IN h_dyntoolbar_Regalos_Entr ( 2.08 , 45.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar_Regalos_Entr ( 1.08 , 46.29 ) NO-ERROR.

       /* Links to SmartDataViewer h_vregalos_entrega_2. */
       RUN addLink ( h_dregalos_entrega , 'Data':U , h_vregalos_entrega_2 ).
       RUN addLink ( h_vregalos_entrega_2 , 'Update':U , h_dregalos_entrega ).
       RUN addLink ( h_dyntoolbar_Regalos_Entr , 'TableIO':U , h_vregalos_entrega_2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vregalos_entrega_2 ,
             h_folder-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar_Regalos_Entr ,
             BUTTON-143:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

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
  DISPLAY W_orden 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE W_orden B_pagina BtnDone BUTTON-143 RECT-1 RECT-3 RECT-4 RECT-5 RECT-7 
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

  /* Code placed here will execute AFTER standard behavior.    */


  ASSIGN W_orden:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "regalo".

  DYNAMIC-FUNCTION('setBGColor':U IN h_folder-2, INPUT 11 /* INTEGER */).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opcion_sdb wWin 
PROCEDURE opcion_sdb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*     DEF VAR qhdl AS HANDLE NO-UNDO.                                      */
/*                                                                          */
/*     ASSIGN qhdl = DYNAMIC-FUNCTION('getbrowsehandle':U in h_dynbrowser). */
/*                                                                          */
/*     ASSIGN                                                               */
/*         qhdl:COLUMN-RESIZABLE = TRUE                                     */
/*         qhdl:ROW-RESIZABLE = TRUE                                        */
/*         qhdl:COLUMN-MOVABLE = TRUE.                                      */
/*                                                                          */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

