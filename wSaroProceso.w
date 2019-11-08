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

{Incluido\variable.i "shared"}        

DEF VAR W_Nuevo        AS LOG NO-UNDO.
DEF VAR v-cod-factor1  AS INT NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS Btn_print W_orden Btn_pagina Btn_Done ~
Btn_Info Btn_Excel RECT-1 RECT-3 RECT-4 RECT-5 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS W_orden 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dsaro AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vpsaro AS HANDLE NO-UNDO.
DEFINE VARIABLE h_wfsaroproceso AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 6 BY 1.5 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Excel 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Excel" 
     SIZE 6 BY 1.5
     FONT 8.

DEFINE BUTTON Btn_Info 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 6 BY 1.5.

DEFINE BUTTON Btn_pagina DEFAULT 
     IMAGE-UP FILE "imagenes/135.ico":U
     LABEL "&Consulta" 
     SIZE 6 BY 1.5 TOOLTIP "Consulta".

DEFINE BUTTON Btn_print 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 6 BY 1.5.

DEFINE VARIABLE W_orden AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Proceso","proceso",
                     "Tipo","tipo",
                     "Nombre","nombre",
                     "Estado","estado"
     DROP-DOWN-LIST
     SIZE 24.14 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 127.86 BY 19.04.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 3.15.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 85.86 BY 3.15.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.14 BY 1.92.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 1.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_print AT ROW 1.85 COL 92 WIDGET-ID 70
     W_orden AT ROW 2.19 COL 6.86 COLON-ALIGNED HELP
          "Seleccione opción de ordenación" NO-LABEL
     Btn_pagina AT ROW 1.85 COL 110
     Btn_Done AT ROW 1.85 COL 116
     Btn_Info AT ROW 1.85 COL 104 WIDGET-ID 68
     Btn_Excel AT ROW 1.85 COL 98 WIDGET-ID 56
     "Ordenar Por:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.27 COL 9
          FGCOLOR 2 FONT 6
     RECT-1 AT ROW 4.23 COL 1
     RECT-3 AT ROW 1 COL 5
     RECT-4 AT ROW 1 COL 39
     RECT-5 AT ROW 1.62 COL 6.86
     RECT-7 AT ROW 1.62 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 128.14 BY 22.38
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 2
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Proceso SARO"
         HEIGHT             = 22.38
         WIDTH              = 128.14
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
ASSIGN 
       Btn_print:HIDDEN IN FRAME fMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Proceso SARO */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Proceso SARO */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done wWin
ON CHOOSE OF Btn_Done IN FRAME fMain
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


    DYNAMIC-FUNCTION('setRebuildOnRepos':U IN h_dsaro,
                     INPUT NO /* LOGICAL */).  

    RUN fetchLast IN h_dsaro.
    ASSIGN viCnt = DYNAMIC-FUNCTION('getLastRowNum':U IN h_dsaro).

    MESSAGE "Se va a generar archivo Excel con " SKIP
        viCnt " registros." SKIP
        "Esto puede tardar unos segundos." SKIP
        "Desea continuar?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO 
        TITLE "Generar a Excel" UPDATE vlgenerar AS LOGICAL.


    DYNAMIC-FUNCTION('setRebuildOnRepos':U IN h_dsaro,
                     INPUT YES /* LOGICAL */).  

    IF vlgenerar THEN 
        RUN transferToExcel IN h_dsaro
        ( INPUT " " /* CHARACTER */, /* "Nit Agencia " */
          INPUT YES /* LOGICAL */,
          INPUT YES /* LOGICAL */,
          INPUT viCnt /* INTEGER */).
    ELSE
        RETURN.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Info wWin
ON CHOOSE OF Btn_Info IN FRAME fMain /* Button 143 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_pagina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_pagina wWin
ON CHOOSE OF Btn_pagina IN FRAME fMain /* Consulta */
DO:
    IF SELF:LABEL = "&Consulta" THEN DO:
        ASSIGN SELF:LABEL = "&Edición"
            SELF:TOOLTIP = "Edición".
        RUN SelectPage(2).
    END.
    ELSE DO:
        ASSIGN SELF:LABEL = "&Consulta"
            SELF:TOOLTIP = "Consulta".
        RUN SelectPage(1).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_pagina wWin
ON ENTRY OF Btn_pagina IN FRAME fMain /* Consulta */
DO:
  ASSIGN W_pag = getCurrentPage().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_print wWin
ON CHOOSE OF Btn_print IN FRAME fMain /* Imprimir */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
    Listado = W_Pathspl + "L-FacRiesgo.lst".
    {Incluido\IMPRIMIR.i "Listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_orden
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_orden wWin
ON VALUE-CHANGED OF W_orden IN FRAME fMain
DO:
  DYNAMIC-FUNCTION('setQuerySort':U IN h_dsaro, INPUT SELF:SCREEN-VALUE).
  DYNAMIC-FUNCTION('OpenQuery':U IN h_dsaro).
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
             INPUT  'dsaro.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedsaroOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dsaro ).
       RUN repositionObject IN h_dsaro ( 1.54 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'wfsaroproceso.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_wfsaroproceso ).
       /* Position in AB:  ( 3.31 , 1.00 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'vpsaro.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vpsaro ).
       RUN repositionObject IN h_vpsaro ( 5.31 , 6.86 ) NO-ERROR.
       /* Size in AB:  ( 16.96 , 116.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,FunctionTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 2.08 , 43.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.00 , 44.86 ) NO-ERROR.

       /* Links to SmartDataObject h_dsaro. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dsaro ).
       RUN addLink ( h_wfsaroproceso , 'Filter':U , h_dsaro ).

       /* Links to SmartWindow h_wfsaroproceso. */
       RUN addLink ( h_dsaro , 'Data':U , h_wfsaroproceso ).

       /* Links to SmartDataViewer h_vpsaro. */
       RUN addLink ( h_dsaro , 'Data':U , h_vpsaro ).
       RUN addLink ( h_vpsaro , 'Update':U , h_dsaro ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vpsaro ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vpsaro ,
             Btn_print:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar ,
             W_orden:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsTipo,Cod_Factor,Proceso,Subproceso,Estado,Clase_ROperativo,Fec_Creacion,Fec_Retiro,Actividad,NombreEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdsaroUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 7.73 , 7.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 14.54 , 116.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_dsaro , 'Data':U , h_dynbrowser ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser ,
             h_vpsaro , 'AFTER':U ).
    END. /* Page 2 */

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
  ENABLE Btn_print W_orden Btn_pagina Btn_Done Btn_Info Btn_Excel RECT-1 RECT-3 
         RECT-4 RECT-5 RECT-7 
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


  ASSIGN W_orden:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "proceso".
  RUN cancelObject IN h_wfsaroproceso. /*filtro*/
  
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
    DEFINE VAR Linea         AS CHARACTER FORMAT "X(120)" INITIAL "".
    DEFINE VAR W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
    DEFINE VAR Raya          AS CHARACTER INITIAL "-".
    
    DEFINE FRAME F-Encabezado
        HEADER
        "ENTIDAD : "                         AT 2 
        W_Nom_Entidad                        AT 14 FORMAT "X(30)"
        "Página:"                            AT 75 PAGE-NUMBER FORMAT ">>>9"
        "Agencia : "                         AT 2
        TRIM(W_Nom_Agencia)                  AT 14 FORMAT "X(30)" SKIP
        "Factores SARO"  AT 35
        "____________________________________________________________________________________________" AT 1
        "No."                                AT 4
        "Proceso"                            AT 9
        "SubProceso"                         AT 17
        "Actividad"                          AT 28
        "Factor de"                          AT 37
        "Riesgo Operativos"                  AT 70
        "Descripcion como puede (ó pudo)"    
        "Descripcion de "                     
        "Severidad y/o"
        "Frecuencia y/o"
        "Calificación inicial"
        "Descripción de la"
        "Descripción de"
        "Tipo"    
        "Responsable"
        "Cargo Responsable"
        SKIP 
        "Riesgo"                             AT 2      
        "Riesgo"                          
        "Suceder el riesgo"                  
        "consecuencias"                      
        SKIP(0)
        "____________________________________________________________________________________________" AT 1
         WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.
          
    DEFINE FRAME F-Piepagina
    HEADER 
        "FECHA:"                AT 2 
        TODAY                   AT 10 FORMAT "99/99/9999"
        W_Nom_Agencia           AT 40 FORMAT "X(30)" FONT 9
        "HORA:"                 AT 70 STRING(TIME,"HH:MM AM")
    WITH DOWN WIDTH 132 FRAME F-Piepagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
       
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Piepagina.
        
        /*
    FOR EACH Varios NO-LOCK:
      IF Varios.Estado = 1 THEN
        ASSIGN W_EstadoInf = "Activo".
      ELSE
        ASSIGN W_EstadoInf = "Inactivo".         
      DISPLAY Varios.Tipo         AT 2  
              Varios.Codigo       AT 15 
              Varios.Descripcion  AT 23  FORMAT "X(35)"
              Varios.comprobante  AT 61
              Varios.Programa     AT 70
              W_Estadoinf         AT 85 
      WITH FRAME F-Varios  DOWN WIDTH 250 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
      DOWN WITH FRAME F-Varios.
    END.  
    PAGE.  
      */      
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

