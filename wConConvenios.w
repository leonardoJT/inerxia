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
DEFINE VARIABLE h_badicionalcredi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bpro_creditoi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_creditos_i AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dsdoconvenios AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dsdocreditos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dsdopro_creditos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sdo_adicionalconv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vconvenios AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.14 BY 23.23
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta de Adicionales en Convenios"
         HEIGHT             = 23.23
         WIDTH              = 82.14
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de Adicionales en Convenios */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de Adicionales en Convenios */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
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
             INPUT  '//192.168.101.9/desarrollo/prg/dsdopro_creditos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedsdopro_creditosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dsdopro_creditos ).
       RUN repositionObject IN h_dsdopro_creditos ( 1.27 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  '//192.168.101.9/desarrollo/prg/bpro_creditoi.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bpro_creditoi ).
       RUN repositionObject IN h_bpro_creditoi ( 1.27 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bpro_creditoi ( 4.31 , 77.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  '//192.168.101.9/desarrollo/prg/dsdocreditos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsCreditos.Cod_Credito,Cod_CreditoObjectNamedsdocreditosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dsdocreditos ).
       RUN repositionObject IN h_dsdocreditos ( 12.04 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  '//192.168.101.9/desarrollo/prg/b_creditos_i.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b_creditos_i ).
       RUN repositionObject IN h_b_creditos_i ( 9.46 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_b_creditos_i ( 5.81 , 77.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  '//192.168.101.9/desarrollo/prg/dsdoconvenios.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsConvenios.Cod_Credito,Cod_Credito,Convenios.Num_Credito,Num_CreditoObjectNamedsdoconveniosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dsdoconvenios ).
       RUN repositionObject IN h_dsdoconvenios ( 15.81 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  '//192.168.101.9/desarrollo/prg/vconvenios.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vconvenios ).
       RUN repositionObject IN h_vconvenios ( 15.38 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 2.69 , 76.86 ) */

       RUN constructObject (
             INPUT  '//192.168.101.9/desarrollo/prg/sdo_adicionalconv.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsAdicional_Convenios.Cod_Credito,Cod_Credito,Adicional_Convenios.Num_Credito,Num_CreditoObjectNamesdo_adicionalconvOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_sdo_adicionalconv ).
       RUN repositionObject IN h_sdo_adicionalconv ( 19.31 , 4.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  '//192.168.101.9/desarrollo/prg/badicionalcredi.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_badicionalcredi ).
       RUN repositionObject IN h_badicionalcredi ( 18.23 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_badicionalcredi ( 5.58 , 77.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsNitOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsDesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 5.65 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 3.77 , 77.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bpro_creditoi. */
       RUN addLink ( h_dsdopro_creditos , 'Data':U , h_bpro_creditoi ).
       RUN addLink ( h_bpro_creditoi , 'Update':U , h_dsdopro_creditos ).

       /* Links to SmartDataObject h_dsdocreditos. */
       RUN addLink ( h_dsdopro_creditos , 'Data':U , h_dsdocreditos ).
       RUN addLink ( h_dynfilter , 'Filter':U , h_dsdocreditos ).

       /* Links to SmartDataBrowser h_b_creditos_i. */
       RUN addLink ( h_dsdocreditos , 'Data':U , h_b_creditos_i ).
       RUN addLink ( h_b_creditos_i , 'Update':U , h_dsdocreditos ).

       /* Links to SmartDataObject h_dsdoconvenios. */
       RUN addLink ( h_dsdocreditos , 'Data':U , h_dsdoconvenios ).

       /* Links to SmartDataViewer h_vconvenios. */
       RUN addLink ( h_dsdoconvenios , 'Data':U , h_vconvenios ).
       RUN addLink ( h_vconvenios , 'Update':U , h_dsdoconvenios ).

       /* Links to SmartDataObject h_sdo_adicionalconv. */
       RUN addLink ( h_dsdocreditos , 'Data':U , h_sdo_adicionalconv ).

       /* Links to SmartDataBrowser h_badicionalcredi. */
       RUN addLink ( h_sdo_adicionalconv , 'Data':U , h_badicionalcredi ).
       RUN addLink ( h_badicionalcredi , 'Update':U , h_sdo_adicionalconv ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynfilter ,
             h_bpro_creditoi , 'AFTER':U ).
       RUN adjustTabOrder ( h_b_creditos_i ,
             h_dynfilter , 'AFTER':U ).
       RUN adjustTabOrder ( h_vconvenios ,
             h_b_creditos_i , 'AFTER':U ).
       RUN adjustTabOrder ( h_badicionalcredi ,
             h_vconvenios , 'AFTER':U ).
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

