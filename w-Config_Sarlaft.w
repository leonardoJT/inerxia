&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
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

{Incluido\variable.i "shared"}

DEF VAR W_Nuevo        AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_ConfigSarlaft

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Salir RECT-2 RECT-291 BUTTON-1 ~
Btn_Imprimir Btn_Consulta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dcfg_Sarlaft1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcfg_Sarlaft2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcfg_Sarlaft3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcontroles_sarlaft AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dsegmentacion_sarlaft AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcontroles_sarlaftwi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vSarlaftanalisis AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vSarlaftprobabilidad AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vSarlaftseveridad AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vsegmentacion_sarlaftwi AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 10" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 8" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Salir DEFAULT  NO-FOCUS
     LABEL "&Salir" 
     SIZE 9 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-155 
     LABEL "" 
     SIZE 50 BY 1.12.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.92.

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_ConfigSarlaft
     Btn_Salir AT ROW 20.04 COL 97.86 WIDGET-ID 12
     BUTTON-155 AT ROW 1.54 COL 27.57 WIDGET-ID 64
     BUTTON-1 AT ROW 1.81 COL 97 WIDGET-ID 6
     Btn_Imprimir AT ROW 3.69 COL 97 WIDGET-ID 4
     Btn_Consulta AT ROW 5.58 COL 97 WIDGET-ID 2
     "DESCRIPCION ESTADISTICAS SARLAFT" VIEW-AS TEXT
          SIZE 37.86 BY .62 AT ROW 1.81 COL 35.14 WIDGET-ID 66
          FONT 1
     RECT-2 AT ROW 1.54 COL 96 WIDGET-ID 8
     RECT-291 AT ROW 1.27 COL 26.57 WIDGET-ID 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.29 BY 21.69
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 5
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Configuración Sarlaft"
         HEIGHT             = 21.69
         WIDTH              = 111.29
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
/* SETTINGS FOR FRAME F_ConfigSarlaft
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON BUTTON-155 IN FRAME F_ConfigSarlaft
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Configuración Sarlaft */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Configuración Sarlaft */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME F_ConfigSarlaft /* Button 10 */
DO:
  IF W_Nuevo THEN
     RETURN.
    
  RUN wconscfgSarlaft.w.  
    
  /*APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Sarlaft. */
/*                                                            */
/*   FRAME F_Consulta:HIDDEN = NO.                            */
     /*RUN Consulta IN h_vpSarlaft
    ( INPUT v-cod-factor1 /* INTEGER */).*/
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME F_ConfigSarlaft /* Button 8 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_Pathspl + "L-ConfSarlaft.lst".
  {Incluido\IMPRIMIR.i "Listado"}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_ConfigSarlaft /* Salir */
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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_ConfigSarlaft /* Button 1 */
DO:
  RUN W-InfDia.w NO-ERROR.
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
             INPUT  'dcfg_Sarlaft.wDB-AWARE':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcfg_SarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcfg_Sarlaft1 ).
       RUN repositionObject IN h_dcfg_Sarlaft1 ( 19.85 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 2.69 , 14.00 ) */

       RUN constructObject (
             INPUT  'dcfg_Sarlaft.wDB-AWARE':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcfg_SarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcfg_Sarlaft2 ).
       RUN repositionObject IN h_dcfg_Sarlaft2 ( 19.85 , 15.00 ) NO-ERROR.
       /* Size in AB:  ( 2.69 , 17.00 ) */

       RUN constructObject (
             INPUT  'dcfg_Sarlaft.wDB-AWARE':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcfg_SarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcfg_Sarlaft3 ).
       RUN repositionObject IN h_dcfg_Sarlaft3 ( 19.85 , 32.00 ) NO-ERROR.
       /* Size in AB:  ( 2.85 , 13.72 ) */

       RUN constructObject (
             INPUT  'dsegmentacion_sarlaft.wDB-AWARE':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedsegmentacion_sarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dsegmentacion_sarlaft ).
       RUN repositionObject IN h_dsegmentacion_sarlaft ( 19.85 , 60.00 ) NO-ERROR.
       /* Size in AB:  ( 2.69 , 14.00 ) */

       RUN constructObject (
             INPUT  'dcontroles_sarlaft.wDB-AWARE':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcontroles_sarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcontroles_sarlaft ).
       RUN repositionObject IN h_dcontroles_sarlaft ( 20.12 , 46.00 ) NO-ERROR.
       /* Size in AB:  ( 2.58 , 14.00 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'FolderLabels':U + 'Severidad|Probabilidad|Análisis|Controles|Segmentación' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 3.19 , 3.72 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 18.35 , 91.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             BUTTON-1:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vSarlaftseveridad.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vSarlaftseveridad ).
       RUN repositionObject IN h_vSarlaftseveridad ( 4.12 , 3.86 ) NO-ERROR.
       /* Size in AB:  ( 10.35 , 90.43 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'DisplayedFieldsCod_Nivel,Nombre,Cod_RASoc,Descripcion,EstadoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdcfg_SarlaftUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 14.50 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 7.00 , 91.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeSaveAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav1 ).
       RUN repositionObject IN h_pupdsav1 ( 8.00 , 96.43 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav1 ( 11.58 , 12.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vSarlaftseveridad. */
       RUN addLink ( h_dcfg_Sarlaft1 , 'Data':U , h_vSarlaftseveridad ).
       RUN addLink ( h_vSarlaftseveridad , 'Update':U , h_dcfg_Sarlaft1 ).
       RUN addLink ( h_pupdsav1 , 'TableIO':U , h_vSarlaftseveridad ).

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_dcfg_Sarlaft1 , 'Data':U , h_dynbrowser ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vSarlaftseveridad ,
             Btn_Imprimir:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_pupdsav1 ,
             Btn_Consulta:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser ,
             h_pupdsav1 , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'vSarlaftprobabilidad.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vSarlaftprobabilidad ).
       RUN repositionObject IN h_vSarlaftprobabilidad ( 4.12 , 3.86 ) NO-ERROR.
       /* Size in AB:  ( 10.15 , 90.57 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'DisplayedFieldsCod_Nivel,Nombre,Val_DetalladaP,Descripcion,EstadoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdcfg_SarlaftUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-2 ).
       RUN repositionObject IN h_dynbrowser-2 ( 14.38 , 3.72 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-2 ( 7.12 , 91.29 ) NO-ERROR.

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeSaveAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav2 ).
       RUN repositionObject IN h_pupdsav2 ( 7.73 , 96.43 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav2 ( 11.85 , 12.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vSarlaftprobabilidad. */
       RUN addLink ( h_dcfg_Sarlaft2 , 'Data':U , h_vSarlaftprobabilidad ).
       RUN addLink ( h_vSarlaftprobabilidad , 'Update':U , h_dcfg_Sarlaft2 ).
       RUN addLink ( h_pupdsav2 , 'TableIO':U , h_vSarlaftprobabilidad ).

       /* Links to SmartDataBrowser h_dynbrowser-2. */
       RUN addLink ( h_dcfg_Sarlaft2 , 'Data':U , h_dynbrowser-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vSarlaftprobabilidad ,
             Btn_Imprimir:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_pupdsav2 ,
             Btn_Consulta:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser-2 ,
             h_pupdsav2 , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'vSarlaftanalisis.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vSarlaftanalisis ).
       RUN repositionObject IN h_vSarlaftanalisis ( 4.12 , 3.86 ) NO-ERROR.
       /* Size in AB:  ( 8.35 , 90.43 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'DisplayedFieldsFSeveridad,FProbabilidad,Cla_Imp,Cta_Db,Por_uno,Cta_Cr,Por_dosEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdcfg_SarlaftUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-3 ).
       RUN repositionObject IN h_dynbrowser-3 ( 13.04 , 3.72 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-3 ( 8.46 , 91.29 ) NO-ERROR.

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeSaveAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav3 ).
       RUN repositionObject IN h_pupdsav3 ( 7.92 , 96.29 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav3 ( 11.85 , 12.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vSarlaftanalisis. */
       RUN addLink ( h_dcfg_Sarlaft3 , 'Data':U , h_vSarlaftanalisis ).
       RUN addLink ( h_vSarlaftanalisis , 'Update':U , h_dcfg_Sarlaft3 ).
       RUN addLink ( h_pupdsav3 , 'TableIO':U , h_vSarlaftanalisis ).

       /* Links to SmartDataBrowser h_dynbrowser-3. */
       RUN addLink ( h_dcfg_Sarlaft3 , 'Data':U , h_dynbrowser-3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vSarlaftanalisis ,
             Btn_Imprimir:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_pupdsav3 ,
             Btn_Consulta:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser-3 ,
             h_pupdsav3 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'vcontroles_sarlaftwi.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vcontroles_sarlaftwi ).
       RUN repositionObject IN h_vcontroles_sarlaftwi ( 4.23 , 3.72 ) NO-ERROR.
       /* Size in AB:  ( 9.19 , 90.86 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'DisplayedFieldscod_control,nom_controls,des_control,fec_control,Cod_FactorEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-4 ).
       RUN repositionObject IN h_dynbrowser-4 ( 13.38 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-4 ( 8.08 , 90.29 ) NO-ERROR.

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeSaveAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav ).
       RUN repositionObject IN h_pupdsav ( 7.46 , 96.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav ( 12.38 , 12.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vcontroles_sarlaftwi. */
       RUN addLink ( h_dcontroles_sarlaft , 'Data':U , h_vcontroles_sarlaftwi ).
       RUN addLink ( h_vcontroles_sarlaftwi , 'Update':U , h_dcontroles_sarlaft ).
       RUN addLink ( h_pupdsav , 'TableIO':U , h_vcontroles_sarlaftwi ).

       /* Links to SmartDataBrowser h_dynbrowser-4. */
       RUN addLink ( h_dcontroles_sarlaft , 'Data':U , h_dynbrowser-4 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vcontroles_sarlaftwi ,
             Btn_Imprimir:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_pupdsav ,
             Btn_Consulta:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser-4 ,
             h_pupdsav , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN constructObject (
             INPUT  'vsegmentacion_sarlaftwi.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vsegmentacion_sarlaftwi ).
       RUN repositionObject IN h_vsegmentacion_sarlaftwi ( 4.15 , 3.72 ) NO-ERROR.
       /* Size in AB:  ( 8.00 , 91.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'DisplayedFieldscod_segmento,Cod_variables,nom_segmento,pun_Final,Pun_inicial,Val_Max_tran_dia,Val_Max_tran_MesEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-5 ).
       RUN repositionObject IN h_dynbrowser-5 ( 12.19 , 3.72 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-5 ( 9.27 , 91.29 ) NO-ERROR.

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME F_ConfigSarlaft:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeSaveAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav-2 ).
       RUN repositionObject IN h_pupdsav-2 ( 7.73 , 96.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav-2 ( 11.85 , 12.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vsegmentacion_sarlaftwi. */
       RUN addLink ( h_dsegmentacion_sarlaft , 'Data':U , h_vsegmentacion_sarlaftwi ).
       RUN addLink ( h_vsegmentacion_sarlaftwi , 'Update':U , h_dsegmentacion_sarlaft ).
       RUN addLink ( h_pupdsav-2 , 'TableIO':U , h_vsegmentacion_sarlaftwi ).

       /* Links to SmartDataBrowser h_dynbrowser-5. */
       RUN addLink ( h_dsegmentacion_sarlaft , 'Data':U , h_dynbrowser-5 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vsegmentacion_sarlaftwi ,
             Btn_Imprimir:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_pupdsav-2 ,
             Btn_Consulta:HANDLE IN FRAME F_ConfigSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser-5 ,
             h_pupdsav-2 , 'AFTER':U ).
    END. /* Page 5 */

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
  ENABLE Btn_Salir RECT-2 RECT-291 BUTTON-1 Btn_Imprimir Btn_Consulta 
      WITH FRAME F_ConfigSarlaft IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConfigSarlaft}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{Incluido\Def_Excel.i}
        
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 3.
 E_Fila      = "003" + "Nivel"
             + "005" + "Nombre"
             + "030" + "Descripcion                   ".
 
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

 DEFI VAR W_IdCC AS CHARACTER FORMAT "X(2)".
 FOR EACH cfg_Sarlaft NO-LOCK:
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(cfg_Sarlaft.Cod_Nivel,"999999")
                  + "005" + STRING(cfg_Sarlaft.Nombre,"x(50)")
                  + "030" + STRING(cfg_Sarlaft.Descripcion,"x(80)").
      {Incluido\imprimir_Excel.i}

          

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
  
  /*Inicializo las paginas*/
  RUN initPages ('1,2,3':U).

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  /*Se incorporan las rutinas del los OSD*/
  RUN Tipo_Codigo IN h_dcfg_Sarlaft1 ( INPUT 1).
  RUN Tipo_Codigo IN h_dcfg_Sarlaft2 ( INPUT 2).
  RUN Tipo_Codigo IN h_dcfg_Sarlaft3 ( INPUT 3).

  /*cerrando los query principal y abriendo uno por cada pagina*/
  DYNAMIC-FUNCTION('closeQuery':U IN h_dcfg_Sarlaft1).
  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dcfg_Sarlaft1,
     INPUT "Tipo = 1").
  DYNAMIC-FUNCTION('openQuery':U IN h_dcfg_Sarlaft1).

  DYNAMIC-FUNCTION('closeQuery':U IN h_dcfg_Sarlaft2).
  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dcfg_Sarlaft2,
     INPUT "Tipo = 2").
  DYNAMIC-FUNCTION('openQuery':U IN h_dcfg_Sarlaft2).

  DYNAMIC-FUNCTION('closeQuery':U IN h_dcfg_Sarlaft3).
  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dcfg_Sarlaft3,
     INPUT "Tipo = 3").
  DYNAMIC-FUNCTION('openQuery':U IN h_dcfg_Sarlaft3).

 
  RUN selectpage(3).
  RUN selectpage(2).
  RUN selectpage(1).



  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Linea         AS CHAR FORMAT "X(120)" INITIAL "" NO-UNDO.
  DEFINE VAR W_EstadoInf   AS CHAR FORMAT "X(8)"   INITIAL "" NO-UNDO.
  DEFINE VAR Raya          AS CHAR INITIAL "-" NO-UNDO.
   
  DEFINE FRAME F-Encabezado
    HEADER
        "ENTIDAD : "                         AT 2 
        W_Nom_Entidad                        AT 14 FORMAT "X(30)"
        "Página:"                            AT 75 PAGE-NUMBER FORMAT ">>>9"
        "Agencia : "                         AT 2
        TRIM(W_Nom_Agencia)                  AT 14 FORMAT "X(30)" SKIP
        "LISTADO DE CONFIGURACION DE TIPOS"  AT 35
        "____________________________________________________________________________________________________________________________" AT 1
    WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.
          
    DEFINE FRAME F-Piepagina
    HEADER 
        "FECHA:"                AT 2 
        TODAY                   AT 10 FORMAT "99/99/9999"
        W_Nom_Agencia           AT 40 FORMAT "X(30)" FONT 9
        "HORA:"                 AT 70 STRING(TIME,"HH:MM AM")
    WITH DOWN WIDTH 132 FRAME F-Piepagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
               
  DEFINE FRAME F-Severidad
     HEADER   
        "NIVEL"                              AT 2
        "NOMBRE"                             AT 10
        "VALORACIÓN DETALLADA"               AT 27
        "DESCRIPCION DETALLADA"              AT 80
        "ESTADO"                             AT 130
        SKIP(0)
        "____________________________________________________________________________________________________________________________" AT 1
     WITH DOWN WIDTH 250 USE-TEXT PAGE-TOP FRAME F-Severidad STREAM-IO.
               
   FORM 
     HEADER   
        "NIVEL"                              AT 2
        "NOMBRE"                             AT 10
        "DESCRIPCION DETALLADA"              AT 27
        "ESTADO"                             AT 110
        SKIP(0)
        "____________________________________________________________________________________________________________________________" AT 1
     WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Probabilidad STREAM-IO.  
        
  DEFINE FRAME F-Analisis
     HEADER   
        "SEVERIDAD"                          AT 2
        "PROBABILIDAD"                       AT 8
        "CLASIFICACIÓN IMPACTO DEL RIESGO"   AT 15
        "CUENTA DEBITO"                      AT 23
        "% PORCENTAJE"                       AT 25
        "CUENTA CREDITO"                     AT 30
        "% PORCENTAJE"                       AT 32
        "ESTADO"                             AT 58
        SKIP(0)
        "____________________________________________________________________________________________________________________________" AT 1
     WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Analisis STREAM-IO.
                 
   VIEW FRAME F-Encabezado.
   VIEW FRAME F-Piepagina.
    
   VIEW FRAME F-Severidad.     
   

   FOR EACH Cfg_Sarlaft WHERE tipo = 1 NO-LOCK WITH FRAME a:
      IF Cfg_Sarlaft.Estado = 1 THEN
        ASSIGN W_EstadoInf = "Activo".
      ELSE
        ASSIGN W_EstadoInf = "Inactivo". 
            
      DISPLAY Cfg_Sarlaft.Cod_Nivel    
               Cfg_Sarlaft.Nombre      FORMAT "x(15)"  AT 10
               Cfg_Sarlaft.Descripcion FORMAT "x(48)"  AT 80
               W_Estadoinf                          AT 130
      WITH FRAME a DOWN WIDTH 250 NO-LABELS NO-BOX.
      DOWN WITH FRAME a.
   END.
    
   
   /*HIDE FRAME F-Severidad.*/
    
   
   VIEW FRAME F-Probabilidad.  
   FOR EACH Cfg_Sarlaft WHERE tipo = 2 NO-LOCK WITH FRAME b:
       IF Cfg_Sarlaft.Estado = 1 THEN
         ASSIGN W_EstadoInf = "Activo".
       ELSE
         ASSIGN W_EstadoInf = "Inactivo". 

       DISPLAY Cfg_Sarlaft.Cod_Nivel                      
               Cfg_Sarlaft.Nombre      FORMAT "x(15)"    AT 10 
               Cfg_Sarlaft.Descripcion FORMAT "x(40)"    AT 80 
               W_Estadoinf                            AT 130
       WITH FRAME b DOWN WIDTH 250 NO-LABELS NO-BOX.  
       DOWN WITH FRAME b.
   END.
/*    HIDE FRAME F-Probabilidad. */
    
   VIEW FRAME F-analisis.   
   FOR EACH Cfg_Sarlaft WHERE tipo = 3 NO-LOCK WITH FRAME c:
      IF Cfg_Sarlaft.Estado = 1 THEN
        ASSIGN W_EstadoInf = "Activo".
      ELSE
        ASSIGN W_EstadoInf = "Inactivo". 

       DISPLAY Cfg_Sarlaft.Cod_Analisis  
               Cfg_Sarlaft.Nombre        FORMAT "x(14)"    AT 10
               SUBSTRING(Cfg_Sarlaft.Nombre,16,40) 
               @ Cfg_Sarlaft.Descripcion  FORMAT "x(40)"    AT 80 
               W_Estadoinf                           AT 130
       WITH FRAME c DOWN WIDTH 250 NO-LABELS NO-BOX.
       DOWN WITH FRAME c.
   END.
    
/*    HIDE FRAME F-analisis. */
/*    PAGE.                  */
        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

