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

DEF VAR lCrrar AS LOGICAL NO-UNDO.
lCrrar = TRUE.

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

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sbosuperformato AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vsuperformatocwi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vsuperformatodwi-2 AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 131 BY 20.88
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 2
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Configuración Formatos"
         HEIGHT             = 20.88
         WIDTH              = 131
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         MIN-BUTTON         = no
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
ON END-ERROR OF wWin /* Configuración Formatos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Configuración Formatos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
    IF NOT lCrrar
    THEN do:
        MESSAGE "ERROR: Termine o Cancele la Transacción Antes de Salir"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
    END.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addrecord wWin 
PROCEDURE addrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    lCrrar = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
             INPUT  'sbosuperformato.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceUpdateOrderdsuperformatod,dsuperformatocCascadeOnBrowseyesOpenOnInityesForeignFieldsLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttribute':U ,
             OUTPUT h_sbosuperformato ).
       RUN repositionObject IN h_sbosuperformato ( 1.00 , 51.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Lista|Configuración' + 'FolderTabWidth0FolderFont5HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 1.35 , 23.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1,2':U) NO-ERROR.

       /* Links to SmartBusinessObject h_sbosuperformato. */
       RUN addLink ( h_dyntoolbar-4 , 'Commit':U , h_sbosuperformato ).
       RUN addLink ( h_dyntoolbar-4 , 'Navigation':U , h_sbosuperformato ).
       RUN addLink ( h_dyntoolbar-5 , 'Navigation':U , h_sbosuperformato ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vsuperformatocwi.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesdsuperformatocUpdateTargetNamesdsuperformatocLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vsuperformatocwi ).
       RUN repositionObject IN h_vsuperformatocwi ( 2.35 , 67.00 ) NO-ERROR.
       /* Size in AB:  ( 19.38 , 62.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsCodigo,Periodicidad,NombreEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdsuperformatocUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 2.35 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 19.38 , 65.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBordernoToolbaryesActionGroupsTableio,Navigation,TransactionTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-source,Commit-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNamedsuperformatocHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-4 ).
       RUN repositionObject IN h_dyntoolbar-4 ( 2.62 , 72.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-4 ( 1.00 , 49.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vsuperformatocwi. */
       RUN addLink ( h_dyntoolbar-4 , 'TableIo':U , h_vsuperformatocwi ).
       RUN addLink ( h_sbosuperformato , 'Data':U , h_vsuperformatocwi ).
       RUN addLink ( h_vsuperformatocwi , 'Update':U , h_sbosuperformato ).

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_sbosuperformato , 'Data':U , h_dynbrowser ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser ,
             h_sbosuperformato , 'AFTER':U ).
       RUN adjustTabOrder ( h_vsuperformatocwi ,
             h_dynbrowser , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar-4 ,
             h_vsuperformatocwi , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'vsuperformatodwi.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesdsuperformatodUpdateTargetNamesdsuperformatodLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vsuperformatodwi-2 ).
       RUN repositionObject IN h_vsuperformatodwi-2 ( 1.00 , 66.00 ) NO-ERROR.
       /* Size in AB:  ( 20.46 , 66.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsUnidadDeCaptura,Subcuenta,Columna,Titulo,CodigoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdsuperformatodUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-2 ).
       RUN repositionObject IN h_dynbrowser-2 ( 2.35 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-2 ( 19.38 , 65.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBordernoToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNamedyntoolbarDisabledActionsHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNamedsuperformatodHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-5 ).
       RUN repositionObject IN h_dyntoolbar-5 ( 1.27 , 78.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-5 ( 0.92 , 40.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vsuperformatodwi-2. */
       RUN addLink ( h_dyntoolbar-5 , 'TableIo':U , h_vsuperformatodwi-2 ).
       RUN addLink ( h_sbosuperformato , 'Data':U , h_vsuperformatodwi-2 ).
       RUN addLink ( h_vsuperformatodwi-2 , 'Update':U , h_sbosuperformato ).

       /* Links to SmartDataBrowser h_dynbrowser-2. */
       RUN addLink ( h_sbosuperformato , 'Data':U , h_dynbrowser-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vsuperformatodwi-2 ,
             h_sbosuperformato , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar-5 ,
             h_vsuperformatodwi-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser-2 ,
             h_dyntoolbar-5 , 'AFTER':U ).
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 2 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelrecord wWin 
PROCEDURE cancelrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    lCrrar = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE committransaction wWin 
PROCEDURE committransaction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    lCrrar = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyrecord wWin 
PROCEDURE copyrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    lCrrar = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleterecord wWin 
PROCEDURE deleterecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    lCrrar = FALSE.
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
  VIEW FRAME fMain IN WINDOW wWin.
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
    SUBSCRIBE "addrecord" ANYWHERE.
    SUBSCRIBE "copyrecord" ANYWHERE.
    SUBSCRIBE "updaterecord" ANYWHERE.
    SUBSCRIBE "cancelrecord" ANYWHERE.
    SUBSCRIBE "updatemode" ANYWHERE.
    SUBSCRIBE "committransaction" ANYWHERE.
    SUBSCRIBE "undotransaction" ANYWHERE.
    SUBSCRIBE "deleterecord" ANYWHERE.

    /* Code placed here will execute AFTER standard behavior.    */
    DYNAMIC-FUNCTION('setBGColor':U IN h_dyntoolbar-4,17).
    DYNAMIC-FUNCTION('setBGColor':U IN h_dyntoolbar-5,17).
    DYNAMIC-FUNCTION('setBGColor':U IN h_dynbrowser,17).
    DYNAMIC-FUNCTION('setBGColor':U IN h_dynbrowser-2,17).
    DYNAMIC-FUNCTION('setBGColor':U IN h_folder,17).
    DYNAMIC-FUNCTION('setFont':U IN h_dynbrowser-2,4).
    DYNAMIC-FUNCTION('setFont':U IN h_dynbrowser,4).
    DYNAMIC-FUNCTION('setFont':U IN h_folder,4).
    RUN selectpage(2).
    RUN selectpage(1).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undotransaction wWin 
PROCEDURE undotransaction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE "Transacción Cancelada"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    lCrrar = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updatemode wWin 
PROCEDURE updatemode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    lCrrar = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updaterecord wWin 
PROCEDURE updaterecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    lCrrar = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

