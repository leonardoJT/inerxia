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


DEF VAR hSper001 AS HANDLE NO-UNDO.
RUN repo-super001.p PERSISTENT SET hSper001.
SESSION:ADD-SUPER-PROCEDURE(hSper001).

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEF VAR W_FecMes AS DATE NO-UNDO.
W_FecMes = TODAY.

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
&Scoped-Define ENABLED-OBJECTS RECT-269 BUTTON-1 Btn_Buscar Btn_Termina-2 ~
Btn_Termina BUTTON-54 Btn_Ayuda 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCambiaLabelFilter wWin 
FUNCTION fCambiaLabelFilter RETURNS CHARACTER
  (h_dynfilter AS HANDLE /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fColExcel wWin 
FUNCTION fColExcel RETURNS CHARACTER
  (j AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPuedeNavegar wWin 
FUNCTION fPuedeNavegar RETURNS LOGICAL
  (hDelViewer AS HANDLE /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fToExcelVlor wWin 
FUNCTION fToExcelVlor RETURNS LOGICAL
  (pCol AS char,pval AS char /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_drepo_formulas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vrepo_formulaswi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-repo-info AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "Ayuda" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda".

DEFINE BUTTON Btn_Buscar 
     IMAGE-UP FILE "imagenes\lupa":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "&Consultar" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de Informaciòn de la pantalla en uso".

DEFINE BUTTON Btn_Termina 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Termina-2 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62
     FONT 4.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra interface de salida de información (Reportes)".

DEFINE BUTTON BUTTON-54 
     IMAGE-UP FILE "imagenes\informacion":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U NO-FOCUS
     LABEL "Button 54" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-269
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-1 AT ROW 3.15 COL 103 WIDGET-ID 2
     Btn_Buscar AT ROW 4.77 COL 103 HELP
          "Permite generar la consulta de Cuentas" WIDGET-ID 4
     Btn_Termina-2 AT ROW 15.54 COL 85 HELP
          "Termina la consulta del Plan Contable" WIDGET-ID 16
     Btn_Termina AT ROW 17.42 COL 104.57 HELP
          "Termina la consulta del Plan Contable" WIDGET-ID 12
     BUTTON-54 AT ROW 1.54 COL 103 WIDGET-ID 8
     Btn_Ayuda AT ROW 19.04 COL 106 HELP
          "Permite generar la ayuda de la Pantalla" WIDGET-ID 14
     RECT-269 AT ROW 1.27 COL 102 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 19.77
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Fórmulas Del Modelo"
         HEIGHT             = 19.77
         WIDTH              = 113.14
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         CONTROL-BOX        = no
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
ON END-ERROR OF wWin /* Fórmulas Del Modelo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Fórmulas Del Modelo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda wWin
ON CHOOSE OF Btn_Ayuda IN FRAME fMain /* Ayuda */
OR HELP OF {&WINDOW-NAME}
DO:
    IF NOT fPuedeNavegar(h_vrepo_formulaswi) THEN RETURN NO-APPLY.            
    SYSTEM-HELP "AYUDAS\REDECOOP" CONTEXT 22.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Buscar wWin
ON CHOOSE OF Btn_Buscar IN FRAME fMain /* Consultar */
DO:
    IF NOT fPuedeNavegar(h_vrepo_formulaswi) THEN RETURN NO-APPLY.            
        
    RUN viewObject IN h_dynfilter.        
    DO WITH FRAME {&FRAME-NAME}:
        Btn_Termina-2:VISIBLE = TRUE.
        Btn_Termina-2:MOVE-TO-TOP().
        DISABLE Btn_Buscar Btn_Termina BUTTON-54 BUTTON-1.
        RUN disableObject IN h_dynbrowser.
        RUN hideObject IN h_pupdsav.            
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Termina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Termina wWin
ON CHOOSE OF Btn_Termina IN FRAME fMain /* Terminar Consulta */
DO:
    IF NOT fPuedeNavegar(h_vrepo_formulaswi) THEN RETURN NO-APPLY.            
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Termina-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Termina-2 wWin
ON CHOOSE OF Btn_Termina-2 IN FRAME fMain /* Terminar Consulta */
DO:
    RUN hideObject IN h_dynfilter.        
    DO WITH FRAME {&FRAME-NAME}:
        Btn_Termina-2:VISIBLE = FALSE.
        Btn_Termina-2:MOVE-TO-BOTTOM().
        ENABLE Btn_Buscar Btn_Termina BUTTON-54 BUTTON-1.
        RUN enableObject IN h_dynbrowser.
        RUN viewObject IN h_pupdsav.            
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Imprimir */
DO:
    IF NOT fPuedeNavegar(h_vrepo_formulaswi) THEN RETURN NO-APPLY.            
    DEFINE VAR Listado     AS CHARACTER INITIAL "".
    ASSIGN Listado = W_PathSpl + "-conf-repo-" + W_Usuario.

    {INCLUIDO\Imprimir.I "Listado"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-54
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-54 wWin
ON CHOOSE OF BUTTON-54 IN FRAME fMain /* Button 54 */
DO:
    IF NOT fPuedeNavegar(h_vrepo_formulaswi) THEN RETURN NO-APPLY.            
    RUN W-InfDia.r NO-ERROR.
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
             INPUT  'drepo_formulas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedrepo_formulasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_drepo_formulas ).
       RUN repositionObject IN h_drepo_formulas ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'vrepo_formulaswi.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vrepo_formulaswi ).
       RUN repositionObject IN h_vrepo_formulaswi ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 13.19 , 96.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsvigente,consecutivo,formula,descripcion,NombreModelo,procedimientoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdrepo_formulasUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 14.19 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 6.46 , 96.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'w-repo-info.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_w-repo-info ).
       /* Position in AB:  ( 1.27 , 97.00 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav ).
       RUN repositionObject IN h_pupdsav ( 6.65 , 102.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav ( 10.77 , 12.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsconsecutivo,descripcion,formula,NombreModelo,procedimiento,vigenteOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsformulaProcedimiento?FieldLabelsFieldToolTipsFieldHelpIdsconsecutivo0formulaProcedimiento0vigente0DesignDataObjectFieldColumn30HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 10.15 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 10.50 , 96.00 ) NO-ERROR.

       /* Links to SmartDataObject h_drepo_formulas. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_drepo_formulas ).

       /* Links to SmartDataViewer h_vrepo_formulaswi. */
       RUN addLink ( h_drepo_formulas , 'Data':U , h_vrepo_formulaswi ).
       RUN addLink ( h_vrepo_formulaswi , 'Update':U , h_drepo_formulas ).
       RUN addLink ( h_pupdsav , 'TableIO':U , h_vrepo_formulaswi ).

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_drepo_formulas , 'Data':U , h_dynbrowser ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vrepo_formulaswi ,
             BUTTON-1:HANDLE IN FRAME fMain , 'BEFORE':U ).
       RUN adjustTabOrder ( h_pupdsav ,
             Btn_Buscar:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynfilter ,
             h_pupdsav , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser ,
             h_dynfilter , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE drepoFrmlasReabrirQuery wWin 
PROCEDURE drepoFrmlasReabrirQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DYNAMIC-FUNCTION('refreshQuery':U IN h_drepo_formulas).
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
  ENABLE RECT-269 BUTTON-1 Btn_Buscar Btn_Termina-2 Btn_Termina BUTTON-54 
         Btn_Ayuda 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprimir_excel wWin 
PROCEDURE imprimir_excel :
/*------------------------------------------------------------------------------
  Purpose:    imprimir_excel 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DEF VAR chExcelApplication  AS COM-HANDLE.
    DEF VAR chWorksheet         AS COM-HANDLE.
    DEF VAR chWorkbook          AS COM-HANDLE.
    DEF VAR chPage              AS COM-HANDLE.

    DEF VAR c1 AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR cQuery AS CHAR NO-UNDO.
    DEF VAR iSheetNumber        AS INTEGER INITIAL 1.
    DEF VAR iFla AS INTEGER NO-UNDO.
    DEF VAR iClmna AS INTEGER NO-UNDO.
    DEF VAR itme AS INTEGER NO-UNDO.

    /*************/
    DEFINE VARIABLE hBufHndle AS HANDLE.
    DEFINE VARIABLE hQHndle AS HANDLE.
    DEFINE VARIABLE hFieldHndle AS HANDLE     NO-UNDO.
    DEF VAR ht AS HANDLE NO-UNDO.

    ht = BUFFER formulas:HANDLE.
    CREATE QUERY hQHndle.
    hQHndle:SET-BUFFERS(ht).
    cquery = "for each " + hT:NAME + " no-lock".
    hQHndle:QUERY-PREPARE(cquery).
    hQHndle:QUERY-OPEN.

    
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.
    IF NOT VALID-HANDLE(chExcelApplication) THEN CREATE "Excel.Application" chExcelApplication .
    IF NOT VALID-HANDLE(chExcelApplication)
    THEN DO:
        MESSAGE  "ERROR: Abriendo Excel".
        RETURN.
    END.
    chExcelApplication:VISIBLE = FALSE.
    chExcelApplication:screenupdating = FALSE.
    PUBLISH "DNEElHdeExel"(chExcelApplication).

    chWorkSheet = chExcelApplication:workbooks:add().
    chWorkSheet = chExcelApplication:Sheets:Item(iSheetNumber).
    chWorkSheet = chExcelApplication:workbooks:item(1):worksheets:add().
    chWorkSheet:activate().
    chWorkSheet:Name = "REPO-FORM-MOD".

    iFla = 1.
    iClmna = 0.
    REPEAT i = 1 TO ht:NUM-FIELDS:
        hFieldHndle = ht:BUFFER-FIELD(i).
        iClmna = iClmna + 1.
        fToExcelVlor(fcolexcel(iclmna) + string(ifla),hFieldHndle:NAME).
    END.
    iclmna = 0.    
    ifla = ifla + 1.
    REPEAT i = 1 TO ht:NUM-FIELDS:
        hFieldHndle = ht:BUFFER-FIELD(i).
        iclmna = iclmna + 1.
        fToExcelVlor(fcolexcel(iclmna) + string(ifla),hFieldHndle:LABEL).
    END.
    
    REPEAT:
        hQHndle:GET-NEXT.
        IF hQHndle:QUERY-OFF-END THEN LEAVE.
        iclmna = 0.
        ifla = ifla + 1.
        REPEAT i = 1 TO ht:NUM-FIELDS:
            hFieldHndle = ht:BUFFER-FIELD(i).
            iclmna = iclmna + 1.
            ftoexcelvlor(fcolexcel(iclmna) + STRING(ifla),hFieldHndle:BUFFER-VALUE).
        END.
    END.
    hQHndle:QUERY-CLOSE.
    chExcelApplication:VISIBLE = TRUE.
    chExcelApplication:screenupdating = TRUE.
    chExcelApplication:displayalerts = FALSE.

    chExcelApplication:ActiveWorkbook:SaveAs(w_pathspl + "-infrepo-form-mod"  + trim(w_usuario),1,"","",FALSE,FALSE,,).
    chExcelApplication:QUIT(). 
    RELEASE OBJECT chWorksheet.
    RELEASE OBJECT chWorkBook   NO-ERROR.
    RELEASE OBJECT chExcelApplication.
/*************/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Control wWin 
PROCEDURE Imp_Control :
/*------------------------------------------------------------------------------
  Purpose: Imp_Control    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
    
    
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
    DYNAMIC-FUNCTION('setBGColor':U IN h_dynbrowser,17).
    DYNAMIC-FUNCTION('setFont':U IN h_dynbrowser,5).
    DYNAMIC-FUNCTION('setBGColor':U IN h_dynfilter,17).
    /*     DYNAMIC-FUNCTION('setBGColor':U IN h_dyntoolbar,17). */
    DYNAMIC-FUNCTION('setFont':U IN h_dynfilter,5).
    fCambiaLabelFilter(h_dynfilter).
    RUN initializeobject1 IN h_vrepo_formulaswi.
    
    SUBSCRIBE "vconf_repositorio_estado" IN  h_vrepo_formulaswi.
    SUBSCRIBE "vconf_repositorio_tbla" IN  h_vrepo_formulaswi.
    RUN dataAvailable IN h_drepo_formulas("different").
    RUN hideObject IN h_dynfilter.        
    DO WITH FRAME {&FRAME-NAME}:
    Btn_Termina-2:VISIBLE = FALSE.
    END.
    SUBSCRIBE "updatemode" IN h_pupdsav.   
    SUBSCRIBE "drepoFrmlasReabrirQuery" IN h_drepo_formulas.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:  ProcesoImprimir   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* {Incluido\RepEncabezado.I} */
    
    /*
    ASSIGN W_Reporte    = "Reporte   : Detalle Crediticio Por C/Obligaciòn                       Fecha : " +
            STRING(W_Fecha,"99/99/9999") + "   Hora :" + STRING(TIME,"HH:MM:SS")
    W_EncColumna = */
    

    RUN Imp_Control.  /*Imprime El Informe*/
    {Incluido/RepEncabezado.I}
    DEFINE VAR W_Titulo2 AS CHARACTER FORMAT "X(120)".
    W_Reporte    = "REPORTE   : REPOSITORIO - FORMULAS MODELO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna =  "CONSEC DESCRIPCION          FORMULA              FORMULACION               MODELO               PROCEDIMIENTO        VIGENTE".

    VIEW FRAME F-Encabezado.
    VIEW FRAME F-ftr.
    
    FOR EACH formulas NO-LOCK
        BY 
            formulas.vigente DESC:
        DISPLAY formulas.consecutivo 
                formulas.descripcion VIEW-AS EDITOR SIZE 20 BY 2
                formulas.formula 
                formulas.formulaProcedimiento VIEW-AS EDITOR SIZE 25 BY 3
                formulas.NombreModelo 
                formulas.procedimiento 
                formulas.vigente
            WITH STREAM-IO USE-TEXT WIDTH 200 NO-LABELS.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCambiaLabelFilter wWin 
FUNCTION fCambiaLabelFilter RETURNS CHARACTER
  (h_dynfilter AS HANDLE /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:   fCambiaLabelFilter
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(h_dynfilter).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fColExcel wWin 
FUNCTION fColExcel RETURNS CHARACTER
  (j AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: fColExcel 
    Notes:  
------------------------------------------------------------------------------*/

    RETURN SUPER(j).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPuedeNavegar wWin 
FUNCTION fPuedeNavegar RETURNS LOGICAL
  (hDelViewer AS HANDLE /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fPuedeNavegar
    Notes:  
------------------------------------------------------------------------------*/
    RETURN SUPER(hDelViewer).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fToExcelVlor wWin 
FUNCTION fToExcelVlor RETURNS LOGICAL
  (pCol AS char,pval AS char /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fToExcelVlor
    Notes:  
------------------------------------------------------------------------------*/

    RETURN SUPER(pcol,pval).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

