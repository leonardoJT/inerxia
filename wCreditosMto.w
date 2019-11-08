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

{incluido\VARIABLE.i "SHARED"}

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
&Scoped-Define ENABLED-OBJECTS W_orden B_pagina BtnDone BUTTON-143 ~
Btn_Excel RECT-1 RECT-3 RECT-4 RECT-5 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS W_orden 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dcreditos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcreditosmto-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_wfcreditos AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 6 BY 1.5 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Excel 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Excel" 
     SIZE 6 BY 1.5
     FONT 8.

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
     LIST-ITEM-PAIRS "Producto","cod_credito",
                     "Agencia","Agencia",
                     "Nit","Nit"
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
     Btn_Excel AT ROW 1.85 COL 94 WIDGET-ID 56
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
         SIZE 127.86 BY 22.27
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


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
         TITLE              = "SFG - Mantenimiento de Créditos - wCreditosMto.w"
         HEIGHT             = 22.27
         WIDTH              = 127.86
         MAX-HEIGHT         = 29.54
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 29.54
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
ON END-ERROR OF wWin /* SFG - Mantenimiento de Créditos - wCreditosMto.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Mantenimiento de Créditos - wCreditosMto.w */
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


&Scoped-define SELF-NAME Btn_Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Excel wWin
ON CHOOSE OF Btn_Excel IN FRAME fMain /* Excel */
DO:
    DEFINE VARIABLE viCnt AS INTEGER INITIAL 0 NO-UNDO.


    DYNAMIC-FUNCTION('setRebuildOnRepos':U IN h_dCreditos,
                     INPUT NO /* LOGICAL */).  

    RUN fetchLast IN h_dCreditos.
    ASSIGN viCnt = DYNAMIC-FUNCTION('getLastRowNum':U IN h_dCreditos).

    MESSAGE "Se va a generar archivo Excel con " SKIP
        viCnt " registros." SKIP
        "Esto puede tardar unos segundos." SKIP
        "Desea continuar?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO 
        TITLE "Generar a Excel" UPDATE vlgenerar AS LOGICAL.


    DYNAMIC-FUNCTION('setRebuildOnRepos':U IN h_dCreditos,
                     INPUT YES /* LOGICAL */).  

    IF vlgenerar THEN 
        RUN transferToExcel IN h_dCreditos
        ( INPUT "Agencia,Nit,Cod_Credito,Num_Credito,Estado,Sdo_Capital" /* CHARACTER */, /* "Nit Agencia " */
          INPUT YES /* LOGICAL */,
          INPUT YES /* LOGICAL */,
          INPUT viCnt /* INTEGER */).
    ELSE
        RETURN.


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
        RUN SelectPage(3).
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
  DYNAMIC-FUNCTION('setQuerySort':U IN h_dCreditos, INPUT SELF:SCREEN-VALUE).
  DYNAMIC-FUNCTION('OpenQuery':U IN h_dCreditos).
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
             INPUT  'dcreditos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcreditosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dcreditos ).
       RUN repositionObject IN h_dcreditos ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'wfcreditos.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_wfcreditos ).
       /* Position in AB:  ( 2.35 , 1.00 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'vcreditosmto.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vcreditosmto-3 ).
       RUN repositionObject IN h_vcreditosmto-3 ( 6.92 , 34.86 ) NO-ERROR.
       /* Size in AB:  ( 14.27 , 60.29 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsAdd,Copy,DeleteFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,FunctionTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsAdd,Copy,DeleteHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 2.08 , 44.86 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.00 , 44.86 ) NO-ERROR.

       /* Links to SmartDataObject h_dcreditos. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dcreditos ).
       RUN addLink ( h_wfcreditos , 'Filter':U , h_dcreditos ).

       /* Links to SmartWindow h_wfcreditos. */
       RUN addLink ( h_dcreditos , 'Data':U , h_wfcreditos ).

       /* Links to SmartDataViewer h_vcreditosmto-3. */
       RUN addLink ( h_dcreditos , 'Data':U , h_vcreditosmto-3 ).
       RUN addLink ( h_vcreditosmto-3 , 'Update':U , h_dcreditos ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vcreditosmto-3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             W_orden:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_vcreditosmto-3 ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsFAgencia,Nit,FCredito,Monto,Sdo_CapitalEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdcreditosUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 9.08 , 6.43 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 13.73 , 117.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_dcreditos , 'Data':U , h_dynbrowser ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser ,
             Btn_Excel:HANDLE IN FRAME fMain , 'AFTER':U ).
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
  ENABLE W_orden B_pagina BtnDone BUTTON-143 Btn_Excel RECT-1 RECT-3 RECT-4 
         RECT-5 RECT-7 
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

  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Usuarios THEN DO:
      IF Usuarios.Grupo EQ 21 THEN DO:
          DYNAMIC-FUNCTION('setQueryWhere':U IN h_dcreditos,
                           INPUT "(Cod_Credito EQ 545 or Cod_Credito EQ 546) AND (Estado EQ 2 OR Estado EQ 3)"/* CHARACTER */).
          DYNAMIC-FUNCTION('openQuery':U IN h_dcreditos).
      END.
      ELSE IF Usuarios.Grupo EQ 1 THEN DO:
              DYNAMIC-FUNCTION('setQueryWhere':U IN h_dcreditos,
                               INPUT "(Cod_Credito NE 0)"/* CHARACTER */).
              DYNAMIC-FUNCTION('openQuery':U IN h_dcreditos).  
           END.
           ELSE DO:
               DYNAMIC-FUNCTION('setQueryWhere':U IN h_dcreditos,
                               INPUT "(Cod_Credito EQ 0)"/* CHARACTER */).
              DYNAMIC-FUNCTION('openQuery':U IN h_dcreditos).  
           END.
  END.

  /*DYNAMIC-FUNCTION('setQueryWhere':U IN h_dcreditos,
     INPUT "(Cod_Credito EQ 545 or Cod_Credito EQ 546) AND (Estado EQ 2 OR Estado EQ 3)"/* CHARACTER */).
  DYNAMIC-FUNCTION('openQuery':U IN h_dcreditos).*/

  ASSIGN W_orden:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Cod_Credito".
  RUN cancelObject IN h_wfCreditos.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

