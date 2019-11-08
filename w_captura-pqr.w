&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  vars:
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

DEF VAR GHPARENT AS HANDLE NO-UNDO.

DEF VAR P_Nit         AS CHAR NO-UNDO.
DEF VAR W_Usuario     AS CHAR NO-UNDO.
DEF VAR W_Agencia     AS INT  NO-UNDO.
DEF VAR W_Nom_Agencia AS CHAR NO-UNDO FORMAT "X(60)".
DEF VAR W_Ciudad      AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-CapPQR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PQR

/* Definitions for FRAME f-CapPQR                                       */
&Scoped-define QUERY-STRING-f-CapPQR FOR EACH PQR ~
      WHERE PQR.Nit = "51575899" NO-LOCK
&Scoped-define OPEN-QUERY-f-CapPQR OPEN QUERY f-CapPQR FOR EACH PQR ~
      WHERE PQR.Nit = "51575899" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-f-CapPQR PQR
&Scoped-define FIRST-TABLE-IN-QUERY-f-CapPQR PQR


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-322 Btn_Consulta Btn-Salir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dpqr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdpqr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vpqr AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Salir 
     LABEL "&Salir" 
     SIZE 15 BY 1.12
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     LABEL "Consulta" 
     SIZE 15 BY 1.12.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 79.86 BY 2.12.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY f-CapPQR FOR 
      PQR SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-CapPQR
     Btn_Consulta AT ROW 19.58 COL 50.43 WIDGET-ID 2
     Btn-Salir AT ROW 19.58 COL 66 WIDGET-ID 6
     RECT-322 AT ROW 19.08 COL 3.14 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.86 BY 21.81
         BGCOLOR 17 FONT 5
         TITLE "Dato De la Solicitud" WIDGET-ID 100.


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
         TITLE              = ""
         HEIGHT             = 21.81
         WIDTH              = 89.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
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
/* SETTINGS FOR FRAME f-CapPQR
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-CapPQR
/* Query rebuild information for FRAME f-CapPQR
     _TblList          = "bdcentral.PQR"
     _Options          = "NO-LOCK"
     _Where[1]         = "PQR.Nit = ""51575899"""
     _Query            is OPENED
*/  /* FRAME f-CapPQR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Salir wWin
ON CHOOSE OF Btn-Salir IN FRAME f-CapPQR /* Salir */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      IF NOT GHPARENT = ? 
        THEN DO:
        RUN pActvaVntna IN GHPARENT.
        RUN hideObject IN THIS-PROCEDURE.    
        RETURN NO-APPLY.
      END.

      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME f-CapPQR /* Consulta */
DO:
  IF SELF:LABEL = "Consulta" THEN DO:
        ASSIGN SELF:LABEL = "&Edición"
            SELF:TOOLTIP = "Edición".
        RUN SelectPage(2).
    END.
    ELSE DO:
        ASSIGN SELF:LABEL = "Consulta"
            SELF:TOOLTIP = "Consulta".
        RUN SelectPage(1).
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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dpqr.wDB-AWARE':U ,
             INPUT  FRAME f-CapPQR:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedpqrOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch10RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dpqr ).
       RUN repositionObject IN h_dpqr ( 19.04 , 82.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'vpqr.w':U ,
             INPUT  FRAME f-CapPQR:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vpqr ).
       RUN repositionObject IN h_vpqr ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 17.92 , 89.57 ) */

       RUN constructObject (
             INPUT  'pupdpqr.w':U ,
             INPUT  FRAME f-CapPQR:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdpqr ).
       RUN repositionObject IN h_pupdpqr ( 19.31 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdpqr ( 1.77 , 46.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vpqr. */
       RUN addLink ( h_dpqr , 'Data':U , h_vpqr ).
       RUN addLink ( h_vpqr , 'Update':U , h_dpqr ).
       RUN addLink ( h_pupdpqr , 'TableIO':U , h_vpqr ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vpqr ,
             Btn_Consulta:HANDLE IN FRAME f-CapPQR , 'BEFORE':U ).
       RUN adjustTabOrder ( h_pupdpqr ,
             h_vpqr , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME f-CapPQR:HANDLE ,
             INPUT  'DisplayedFieldsNum_PQR,Fec_Solicitud,Tip_PQR,Estado,Agencia,Fnom_producto,Canal_Servicio,Area_Resp,Descrip_RespEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdpqrUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 3.96 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 14.54 , 85.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_dpqr , 'Data':U , h_dynbrowser ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser ,
             h_vpqr , 'AFTER':U ).
    END. /* Page 2 */

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

  {&OPEN-QUERY-f-CapPQR}
  GET FIRST f-CapPQR.
  ENABLE RECT-322 Btn_Consulta Btn-Salir 
      WITH FRAME f-CapPQR IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-f-CapPQR}
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
  RUN Busqueda_Nit     IN h_vpqr(P_Nit).
  RUN Busqueda_Usuario IN h_vpqr(W_Usuario).
  RUN Busqueda_agencia IN h_vpqr(STRING(W_agencia)).
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrntHndle wWin 
PROCEDURE pPrntHndle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER h AS HANDLE NO-UNDO.
    GHPARENT = h.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRcbeDtos wWin 
PROCEDURE pRcbeDtos :
/*------------------------------------------------------------------------------
  Purpose:    pRcbeDtos 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cnit AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cusu AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cage AS CHAR NO-UNDO.



    P_Nit     = ENTRY(1,cnit,CHR(1)).
    W_Usuario = ENTRY(1,cusu,CHR(1)).
    W_Agencia = INT(ENTRY(1,cage,CHR(1))).
    

    /*RUN pRcbeDtos IN h_w_captura-pqr(P_Nit,w_usuario,STRING(w_agencia)).*/
    RUN Busqueda_Nit IN h_vpqr(P_Nit).
    RUN Busqueda_Usuario IN h_vpqr(W_Usuario).
    RUN Busqueda_Agencia IN h_vpqr(STRING(W_Agencia)).
    DYNAMIC-FUNCTION('closeQuery':U IN h_dpqr).
    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dpqr,"pqr.nit = '" + P_Nit + "'").
    DYNAMIC-FUNCTION('openQuery':U IN h_dpqr).
    RUN pRcbeDtos IN h_vpqr ( INPUT cnit /* CHARACTER */,
        INPUT cusu /* CHARACTER */,
        INPUT cage /* CHARACTER */).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

