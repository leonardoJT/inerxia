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

DEFINE INPUT PARAMETER P_Nit     LIKE Act_Pasivos.Nit.
DEFINE OUTPUT PARAMETER P_Pas    AS DECIMAL.
DEFINE OUTPUT PARAMETER P_VVeh   AS DECIMAL.
DEFINE OUTPUT PARAMETER P_VBie   AS DECIMAL. 
DEFINE OUTPUT PARAMETER P_VOtros AS DECIMAL.
/*
DEF VAR p_dir AS CHAR.
DEF VAR p_nit AS CHAR.

ASSIGN p_nit = '52068909'.

DEFINE VAR P_Pas    AS DECIMAL.
DEFINE VAR P_VVeh   AS DECIMAL.
DEFINE VAR P_VBie   AS DECIMAL. 
DEFINE VAR P_VOtros AS DECIMAL.
*/

DEF VAR cDrccion AS CHAR NO-UNDO.
DEF VAR hDrccion AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS Btn_Salir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dact_pasivos1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dact_pasivos2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dact_pasivos3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dact_pasivos4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vclientesbienes-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vclientesotros AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vclientespasivos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vclientesvehiculos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vpas-act AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Salir DEFAULT  NO-FOCUS
     LABEL "&Salir" 
     SIZE 11 BY 1.62
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_Salir AT ROW 15 COL 92 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129.57 BY 19.77
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Detalle Activos y Pasivos"
         HEIGHT             = 19.77
         WIDTH              = 113.14
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Detalle Activos y Pasivos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Detalle Activos y Pasivos */
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
ON CHOOSE OF Btn_Salir IN FRAME fMain /* Salir */
DO:
  RUN Calculos_bienes.

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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dact_pasivos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedact_pasivosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dact_pasivos1 ).
       RUN repositionObject IN h_dact_pasivos1 ( 1.00 , 84.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'vpas-act.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vpas-act ).
       RUN repositionObject IN h_vpas-act ( 1.00 , 16.00 ) NO-ERROR.
       /* Size in AB:  ( 2.42 , 60.00 ) */

       RUN constructObject (
             INPUT  'dact_pasivos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedact_pasivosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dact_pasivos2 ).
       RUN repositionObject IN h_dact_pasivos2 ( 1.00 , 76.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dact_pasivos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedact_pasivosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dact_pasivos3 ).
       RUN repositionObject IN h_dact_pasivos3 ( 1.00 , 8.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dact_pasivos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedact_pasivosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dact_pasivos4 ).
       RUN repositionObject IN h_dact_pasivos4 ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Pasivos|Vehículos|Bienes|Otros Activos' + 'FolderTabWidth17FolderFont5HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 3.69 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 1.27 , 90.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vpas-act. */
       RUN addLink ( h_dact_pasivos1 , 'Data':U , h_vpas-act ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             h_vpas-act , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vclientespasivos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisableModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vclientespasivos ).
       RUN repositionObject IN h_vclientespasivos ( 5.04 , 24.00 ) NO-ERROR.
       /* Size in AB:  ( 4.58 , 53.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsNombre,Val_Comercial,Val_Cuota,Val_SaldoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser1 ).
       RUN repositionObject IN h_dynbrowser1 ( 9.88 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser1 ( 10.77 , 90.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav ).
       RUN repositionObject IN h_pupdsav ( 1.00 , 91.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav ( 14.00 , 13.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vclientespasivos. */
       RUN addLink ( h_dact_pasivos1 , 'Data':U , h_vclientespasivos ).
       RUN addLink ( h_vclientespasivos , 'Update':U , h_dact_pasivos1 ).
       RUN addLink ( h_pupdsav , 'TableIO':U , h_vclientespasivos ).

       /* Links to SmartDataBrowser h_dynbrowser1. */
       RUN addLink ( h_dact_pasivos1 , 'Data':U , h_dynbrowser1 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_pupdsav ,
             h_dact_pasivos1 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vclientespasivos ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser1 ,
             h_vclientespasivos , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'vclientesvehiculos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisableModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vclientesvehiculos ).
       RUN repositionObject IN h_vclientesvehiculos ( 5.04 , 12.00 ) NO-ERROR.
       /* Size in AB:  ( 4.58 , 67.29 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsNombre,Modelo,Placa,Val_Comercial,Prenda_HipotecaEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser2 ).
       RUN repositionObject IN h_dynbrowser2 ( 9.88 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser2 ( 10.77 , 90.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav-2 ).
       RUN repositionObject IN h_pupdsav-2 ( 1.00 , 91.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav-2 ( 13.73 , 13.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vclientesvehiculos. */
       RUN addLink ( h_dact_pasivos2 , 'Data':U , h_vclientesvehiculos ).
       RUN addLink ( h_vclientesvehiculos , 'Update':U , h_dact_pasivos2 ).
       RUN addLink ( h_pupdsav-2 , 'TableIO':U , h_vclientesvehiculos ).

       /* Links to SmartDataBrowser h_dynbrowser2. */
       RUN addLink ( h_dact_pasivos2 , 'Data':U , h_dynbrowser2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_pupdsav-2 ,
             h_dact_pasivos1 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vclientesvehiculos ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser2 ,
             h_vclientesvehiculos , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'vclientesbienes.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisableModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vclientesbienes-2 ).
       RUN repositionObject IN h_vclientesbienes-2 ( 5.04 , 13.00 ) NO-ERROR.
       /* Size in AB:  ( 5.92 , 67.29 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsNombre,Dir_Bienes,Lugar_Bienes,Val_Comercial,Matricula_inmobiliaria,Prenda_HipotecaEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser3 ).
       RUN repositionObject IN h_dynbrowser3 ( 11.23 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser3 ( 9.42 , 89.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav-3 ).
       RUN repositionObject IN h_pupdsav-3 ( 1.00 , 91.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav-3 ( 14.00 , 13.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vclientesbienes-2. */
       RUN addLink ( h_dact_pasivos3 , 'Data':U , h_vclientesbienes-2 ).
       RUN addLink ( h_vclientesbienes-2 , 'Update':U , h_dact_pasivos3 ).
       RUN addLink ( h_pupdsav-3 , 'TableIO':U , h_vclientesbienes-2 ).

       /* Links to SmartDataBrowser h_dynbrowser3. */
       RUN addLink ( h_dact_pasivos3 , 'Data':U , h_dynbrowser3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_pupdsav-3 ,
             h_dact_pasivos1 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vclientesbienes-2 ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser3 ,
             h_vclientesbienes-2 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'vclientesotros.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisableModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vclientesotros ).
       RUN repositionObject IN h_vclientesotros ( 5.04 , 16.00 ) NO-ERROR.
       /* Size in AB:  ( 3.65 , 60.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsNombre,Val_Comercial,DetalleEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser4 ).
       RUN repositionObject IN h_dynbrowser4 ( 8.81 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser4 ( 11.85 , 89.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav-4 ).
       RUN repositionObject IN h_pupdsav-4 ( 1.00 , 91.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav-4 ( 14.00 , 13.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vclientesotros. */
       RUN addLink ( h_dact_pasivos4 , 'Data':U , h_vclientesotros ).
       RUN addLink ( h_vclientesotros , 'Update':U , h_dact_pasivos4 ).
       RUN addLink ( h_pupdsav-4 , 'TableIO':U , h_vclientesotros ).

       /* Links to SmartDataBrowser h_dynbrowser4. */
       RUN addLink ( h_dact_pasivos4 , 'Data':U , h_dynbrowser4 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_pupdsav-4 ,
             h_dact_pasivos1 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vclientesotros ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser4 ,
             h_vclientesotros , 'AFTER':U ).
    END. /* Page 4 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculos_bienes wWin 
PROCEDURE calculos_bienes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR v-suma AS DECIMAL.
      
  ASSIGN v-suma = 0.
  FOR EACH Act_Pasivos WHERE Act_Pasivos.Nit = p_nit
                         AND Act_Pasivos.Cod_Tipo = 4 
    NO-LOCK:
     
    v-suma = v-suma + Act_Pasivos.Val_Comercial.
  END.
  ASSIGN P_Pas = v-suma.

  ASSIGN v-suma = 0.
  FOR EACH Act_Pasivos WHERE Act_Pasivos.Nit = p_nit
                         AND Act_Pasivos.Cod_Tipo = 2 
    NO-LOCK:
     
    v-suma = v-suma + Act_Pasivos.Val_Comercial.
  END.
  ASSIGN P_VVeh = v-suma.

  ASSIGN v-suma = 0.
  FOR EACH Act_Pasivos WHERE Act_Pasivos.Nit = p_nit
                         AND Act_Pasivos.Cod_Tipo = 1 
    NO-LOCK:
     
    v-suma = v-suma + Act_Pasivos.Val_Comercial.
  END.
  ASSIGN P_VBie  = v-suma.

  ASSIGN v-suma = 0.
  FOR EACH Act_Pasivos WHERE Act_Pasivos.Nit = p_nit
                         AND Act_Pasivos.Cod_Tipo = 3 
    NO-LOCK:
     
    v-suma = v-suma + Act_Pasivos.Val_Comercial.
  END.
  ASSIGN P_VOtros  = v-suma.


   
   
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
  ENABLE Btn_Salir 
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

  
  RUN initPages ('1,2,3,4':U).

  RUN SUPER.
    RUN Busqueda_Nit IN h_vpas-act(P_Nit).
  /* Code placed here will execute AFTER standard behavior.    */

  /*Inicializo las paginas*/
  
  /*se incorporan las rutinas del los OSD*/
  RUN Actualizacion_nit IN h_dact_pasivos1 ( P_Nit ).
  RUN Actualizacion_nit IN h_dact_pasivos2 ( P_Nit ).
  RUN Actualizacion_nit IN h_dact_pasivos3 ( P_Nit ).
  RUN Actualizacion_nit IN h_dact_pasivos4 ( P_Nit ).

  RUN Cod_Relacion IN h_dact_pasivos1 ( INPUT 4 ).
  RUN Cod_Relacion IN h_dact_pasivos2 ( INPUT 2 ).
  RUN Cod_Relacion IN h_dact_pasivos3 ( INPUT 1 ).
  RUN Cod_Relacion IN h_dact_pasivos4 ( INPUT 3 ).
  
  
  
  
  
  
  /*cerrando los query principal y abriendo uno por cada pagina*/
  DYNAMIC-FUNCTION('closeQuery':U IN h_dact_pasivos1).
  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dact_pasivos1,
     INPUT "nit = '" + p_nit + "' AND 
            Cod_Tipo = 4").
  DYNAMIC-FUNCTION('openQuery':U IN h_dact_pasivos1).

  DYNAMIC-FUNCTION('closeQuery':U IN h_dact_pasivos2).
  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dact_pasivos2,
     INPUT "nit = '" + p_nit + "' AND 
            Cod_Tipo = 2").
  DYNAMIC-FUNCTION('openQuery':U IN h_dact_pasivos2).

  DYNAMIC-FUNCTION('closeQuery':U IN h_dact_pasivos3).
  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dact_pasivos3,
     INPUT "nit = '" + p_nit + "' AND 
            Cod_Tipo = 1").
  DYNAMIC-FUNCTION('openQuery':U IN h_dact_pasivos3).

  DYNAMIC-FUNCTION('closeQuery':U IN h_dact_pasivos4).
  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dact_pasivos4,
     INPUT "nit = '" + p_nit + "' AND 
            Cod_Tipo = 3").
  DYNAMIC-FUNCTION('openQuery':U IN h_dact_pasivos4).


  DYNAMIC-FUNCTION('setBrowseColumnLabels':U IN h_dynbrowser1,
     INPUT "'Entidad'" ).
  RUN selectPage(4).
  RUN selectPage(3).
  RUN selectPage(2).
  RUN selectPage(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

