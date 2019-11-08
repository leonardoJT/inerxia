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
DEF BUFFER brepo FOR repositorio.repositorio.
DEF BUFFER bconf FOR repositorio.conf_repositorio.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEF VAR W_FecMes AS DATE NO-UNDO.
W_FecMes = TODAY.
DEF VAR cLstaSlccion AS CHAR NO-UNDO.

DEFINE TEMP-TABLE TExcel NO-UNDO
    FIELD TE_Ide      AS CHARACTER FORMAT "X(14)".

DEFINE VARIABLE Listado      AS CHARACTER INITIAL "l-planti.Lst".

DEF TEMP-TABLE tPrddaEsprda NO-UNDO
    FIELD iAgncia AS INTEGER
    FIELD iCdgoCrdto AS INTEGER
    FIELD dePrdda AS DECIMAL.
DEF VAR ht AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-290 Btn_Termina-2 BUTTON-54 Btn_Imp ~
Btn_Buscar BUTT-ttles Btn_Termina BUTTON-11 

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
  (i AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fL1TOL2 wWin 
FUNCTION fL1TOL2 RETURNS CHARACTER
  (cbuscar AS CHAR,corigen AS CHAR,cdestino AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fprintMnsje wWin 
FUNCTION fprintMnsje RETURNS LOGICAL
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPuedeNavegar wWin 
FUNCTION fPuedeNavegar RETURNS LOGICAL
  (hDelViewer AS HANDLE /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fRsmenAgnciaLnea wWin 
FUNCTION fRsmenAgnciaLnea RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fRsmenAgncias wWin 
FUNCTION fRsmenAgncias RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fRsmenLneaAgncia wWin 
FUNCTION fRsmenLneaAgncia RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fRsmenLneas wWin 
FUNCTION fRsmenLneas RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
DEFINE VARIABLE h_drepo_resultados AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-repo-info AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-repo-resu AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Buscar 
     IMAGE-UP FILE "imagenes\lupa":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "&Consultar" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de Informaciòn de la pantalla en uso".

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Opciones De Impresión".

DEFINE BUTTON Btn_Termina 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 14" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_Termina-2 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62
     FONT 4.

DEFINE BUTTON BUTT-ttles 
     LABEL "TOT AGE" 
     SIZE 10 BY 1.62 TOOLTIP "Total Por Agencias".

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-54 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 166" 
     SIZE 10 BY 1.62.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 7.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_Termina-2 AT ROW 4.77 COL 79 HELP
          "Termina la consulta del Plan Contable" WIDGET-ID 56
     BUTTON-54 AT ROW 4.77 COL 94 WIDGET-ID 38
     Btn_Imp AT ROW 6.38 COL 94 WIDGET-ID 54
     Btn_Buscar AT ROW 8 COL 94 HELP
          "Permite generar la consulta de Cuentas" WIDGET-ID 58
     BUTT-ttles AT ROW 9.62 COL 94 WIDGET-ID 60
     Btn_Termina AT ROW 15.27 COL 95 WIDGET-ID 46
     BUTTON-11 AT ROW 17.15 COL 97 WIDGET-ID 42
     RECT-290 AT ROW 4.5 COL 93 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 145.72 BY 26.19
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
         TITLE              = "REPOSITORIO - Consulta Resultados Modelo"
         HEIGHT             = 19.77
         WIDTH              = 113.14
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
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
ON END-ERROR OF wWin /* REPOSITORIO - Consulta Resultados Modelo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* REPOSITORIO - Consulta Resultados Modelo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  /* APPLY "CLOSE":U TO THIS-PROCEDURE.*/
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Buscar wWin
ON CHOOSE OF Btn_Buscar IN FRAME fMain /* Consultar */
DO:
        
    RUN viewObject IN h_dynfilter.        
    DO WITH FRAME {&FRAME-NAME}:
        Btn_Termina-2:VISIBLE = TRUE.
        Btn_Termina-2:MOVE-TO-TOP().
        DISABLE Btn_Buscar Btn_Termina BUTTON-54 btn_imp.
        RUN disableObject IN h_dynbrowser.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp wWin
ON CHOOSE OF Btn_Imp IN FRAME fMain /* Imprimir */
DO:
    {incluido/Imprimir.i "Listado" 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Termina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Termina wWin
ON CHOOSE OF Btn_Termina IN FRAME fMain /* Button 14 */
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


&Scoped-define SELF-NAME Btn_Termina-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Termina-2 wWin
ON CHOOSE OF Btn_Termina-2 IN FRAME fMain /* Terminar Consulta */
DO:
    RUN hideObject IN h_dynfilter.        
    DO WITH FRAME {&FRAME-NAME}:
        Btn_Termina-2:VISIBLE = FALSE.
        Btn_Termina-2:MOVE-TO-BOTTOM().
        ENABLE Btn_Buscar Btn_Termina BUTTON-54 btn_imp.
        RUN enableObject IN h_dynbrowser.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTT-ttles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTT-ttles wWin
ON CHOOSE OF BUTT-ttles IN FRAME fMain /* TOT AGE */
DO:
    SESSION:SET-WAIT("general").
    RUN Pttlzar.  
    fRsmenAgncias().
    fRsmenLneas().
    DYNAMIC-FUNCTION('fRsmenAgnciaLnea':U).
    DYNAMIC-FUNCTION('fRsmenLneaAgncia':U).
    SESSION:SET-WAIT("").
    RUN viewObject IN h_w-repo-resu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-54
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-54 wWin
ON CHOOSE OF BUTTON-54 IN FRAME fMain /* Button 166 */
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
             INPUT  'drepo_resultados.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedrepo_resultadosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch30RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_drepo_resultados ).
       RUN repositionObject IN h_drepo_resultados ( 1.00 , 92.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsfecha,Nit,Apellido1,Apellido2,Nombre,prestamo,int002,int003,Nom_Producto,Descripcion,PerdidaEsperada,ProbabilidadIncumplimiento,calificacion,CalificacionEquivalente,ExposicionActivo,PerdidaEsperada%,AnoContable,MesContable,int001EnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdrepo_resultadosUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 19.65 , 91.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'w-repo-info.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_w-repo-info ).
       /* Position in AB:  ( 2.73 , 92.29 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'w-repo-resu.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_w-repo-resu ).
       /* Position in AB:  ( 2.69 , 100.72 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsfecha,Nit,prestamo,Apellido1,Apellido2,Nombre,CalificacionEquivalente,PerdidaEsperada,PerdidaEsperada%,int002,int003,int001OperatorStyleRangeOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsint001AgenciaFieldHelpIdscalificacion0fecha0Nit0prestamo0Apellido10PerdidaEsperada0int0020int0030int0010DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 15.35 , 91.00 ) NO-ERROR.

       /* Links to SmartDataObject h_drepo_resultados. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_drepo_resultados ).

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_drepo_resultados , 'Data':U , h_dynbrowser ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser ,
             Btn_Termina-2:HANDLE IN FRAME fMain , 'BEFORE':U ).
       RUN adjustTabOrder ( h_dynfilter ,
             h_dynbrowser , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Armar_TablaExcel wWin 
PROCEDURE Armar_TablaExcel :
/*
FOR EACH TSaldos WHERE TSaldos.TS_Mes EQ INTEGER(TSaldos.TS_Mes:SCREEN-VALUE IN BROWSE B_Saldos):
       CREATE TExcel.
       ASSIGN TExcel.TE_Ide            = W_Cuenta
              TExcel.TE_Inicial        = TSaldos.TS_Inicial
              TExcel.TE_Debito         = TSaldos.TS_Debito
              TExcel.TE_Credito        = TSaldos.TS_Credito
              TExcel.TE_Final          = TSaldos.TS_Final.
    END.
    FOR EACH TAnexos:
       CREATE TExcel.
       ASSIGN TExcel.TE_Age            = TA_Agencia
              TExcel.TE_Ide            = TA_Nit    
              TExcel.TE_CC             = TA_CenCos 
              TExcel.TE_Inicial        = TA_Inicial
              TEXcel.TE_Debito         = TA_Debito 
              TExcel.TE_Credito        = TA_Credito
              TEXcel.TE_Final          = TA_Final.  
       IF Cuentas.Id_Detalle THEN DO:
           FOR EACH Detalle WHERE Detalle.Agencia  EQ  TAnexos.TA_Agencia AND 
                    Detalle.Cen_Costos             EQ  TAnexos.TA_CenCos  AND
                    Detalle.Nit                    EQ  TAnexos.TA_Nit     AND
                    Detalle.Cuenta                 EQ  W_Cuenta:
               IF Cuentas.Naturaleza EQ "DB" THEN
                  IDSdoFin         = Detalle.Db - Detalle.Cr.
               ELSE
                  IDSdoFin         = Detalle.Cr - Detalle.Db.
               CREATE TExcel.
               ASSIGN TExcel.TE_Age      = Detalle.Agencia              
                      TExcel.TE_CC       = Detalle.Cen_Costos           
                      TExcel.TE_Doc      = Detalle.Doc_referencia       
                      TExcel.TE_Fecha    = Detalle.Fec_ultActualizacion 
                      TExcel.TE_Inicial  = Detalle.Plazo                
                      TExcel.TE_Debito   = Detalle.Valor_Inicial        
                      TExcel.TE_Final    = IDSdoFin.
           END.
       END.
    END.
*/        
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
  ENABLE RECT-290 Btn_Termina-2 BUTTON-54 Btn_Imp Btn_Buscar BUTT-ttles 
         Btn_Termina BUTTON-11 
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
  Purpose:     
  Parameters:  <none>
  Notes:  Esta Rutina No Se Terminó Por Orden De Nelsón     
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
    DEF VAR hRsltdos AS HANDLE NO-UNDO.
    DEF VAR hClientes AS HANDLE NO-UNDO.
    DEF VAR hrpstrio AS HANDLE NO-UNDO.
    DEF VAR hpro_crdtos AS HANDLE NO-UNDO.
    DEF VAR hagncias AS HANDLE NO-UNDO.

    hRsltdos = BUFFER resultados:HANDLE.
    hClientes = BUFFER clientes:HANDLE.
    hRpstrio = BUFFER repositorio:HANDLE.
    hpro_crdtos = BUFFER pro_creditos:HANDLE.
    hagncias = BUFFER agencias:HANDLE.
        
    CREATE QUERY hQHndle.
    hQHndle:SET-BUFFERS(hRsltdos,hclientes,hrpstrio,hpro_crdtos,hagncias).
    cquery = DYNAMIC-FUNCTION('getQueryWhere' IN h_drepo_resultados).
    hQHndle:QUERY-PREPARE(cquery).
    hQHndle:QUERY-OPEN.

    
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.
    IF NOT VALID-HANDLE(chExcelApplication) THEN CREATE "Excel.Application" chExcelApplication .
    IF NOT VALID-HANDLE(chExcelApplication)
    THEN DO:
        MESSAGE  "ERROR: Abriendo Excel".
        RETURN.
    END.
    
        
    /* DEFINE SEPARADORES DEL SISTEMA */
/*     chExcelApplication:UseSystemSeparators = FALSE. */
/*     chExcelApplication:DecimalSeparator = ",".      */
/*     chExcelApplication:ThousandsSeparator = ".".    */
    /* fin DEFINE SEPARADORES DEL SISTEMA */
    
    chExcelApplication:VISIBLE = FALSE.
    chExcelApplication:screenupdating = FALSE.
    PUBLISH "DNEElHdeExel"(chExcelApplication).

    chWorkSheet = chExcelApplication:workbooks:add().
    chWorkSheet = chExcelApplication:Sheets:Item(iSheetNumber).
    chWorkSheet = chExcelApplication:workbooks:item(1):worksheets:add().
    chWorkSheet:activate().
    chWorkSheet:Name = "ConResuMod".

    iFla = 1.
    iClmna = 0.
    REPEAT i = 1 TO hRsltdos:NUM-FIELDS:
        hFieldHndle = hRsltdos:BUFFER-FIELD(i).
        iClmna = iClmna + 1.
        fToExcelVlor(fcolexcel(iclmna) + string(ifla),hFieldHndle:NAME).
    END.
    iclmna = 0.    
    ifla = ifla + 1.
    REPEAT i = 1 TO hRsltdos:NUM-FIELDS:
        hFieldHndle = hRsltdos:BUFFER-FIELD(i).
        iclmna = iclmna + 1.
        fToExcelVlor(fcolexcel(iclmna) + string(ifla),hFieldHndle:LABEL).
    END.
    
    REPEAT:
        hQHndle:GET-NEXT.
        IF hQHndle:QUERY-OFF-END THEN LEAVE.
        iclmna = 0.
        ifla = ifla + 1.
        REPEAT i = 1 TO hRsltdos:NUM-FIELDS:
            hFieldHndle = hRsltdos:BUFFER-FIELD(i).
            iclmna = iclmna + 1.
            ftoexcelvlor(fcolexcel(iclmna) + STRING(ifla),hFieldHndle:BUFFER-VALUE).
        END.
    END.
    hQHndle:QUERY-CLOSE.
    chExcelApplication:VISIBLE = TRUE.
    chExcelApplication:screenupdating = TRUE.
    chExcelApplication:displayalerts = FALSE.

    chExcelApplication:ActiveWorkbook:SaveAs(w_pathspl + "-infrepo-con-resu-mod"  + trim(w_usuario),1,"","",FALSE,FALSE,,).
    /* chExcelApplication:UseSystemSeparators = TRUE. */
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
  
/*                                                                                            */
/*     DISPLAY STRING(W_Nom_Entidad,"X(40)") + "   -  Repositorio - Consulta Por Fechas" SKIP */
/*     "                              Fecha : " + STRING(W_FecMes,"99/99/9999") FORM "X(80)"  */
/*     SKIP (1)                                                                               */
/*     WITH WIDTH 150 NO-LABELS.                                                              */
    
    
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

    DYNAMIC-FUNCTION('setBGColor':U IN h_dynbrowser,17).
    DYNAMIC-FUNCTION('setFont':U IN h_dynbrowser,5).
    DYNAMIC-FUNCTION('setBGColor':U IN h_dynfilter,17).
/*     DYNAMIC-FUNCTION('setBGColor':U IN h_dyntoolbar,17). */
    DYNAMIC-FUNCTION('setFont':U IN h_dynfilter,5).
    fCambiaLabelFilter(h_dynfilter).

    RUN dataAvailable IN h_drepo_resultados("different").
    RUN hideObject IN h_dynfilter.        
    DO WITH FRAME {&FRAME-NAME}:
        Btn_Termina-2:VISIBLE = FALSE.
    END.

    SUBSCRIBE "queryopened" IN h_drepo_resultados.
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
    
    /*ASSIGN W_Reporte    = "Reporte   : Detalle Crediticio Por C/Obligaciòn                       Fecha : " +
         STRING(W_Fecha,"99/99/9999") + "   Hora :" + STRING(TIME,"HH:MM:SS")
    W_EncColumna = */
    def var ccmpos            AS CHARACTER  NO-UNDO.
    def var hq             AS HANDLE     NO-UNDO.
    def var hBuffer            AS HANDLE     NO-UNDO.
    def var hcmpo             AS HANDLE     NO-UNDO.
    def var icmpos            AS INTEGER    NO-UNDO.
    def var iRow               AS INTEGER    NO-UNDO.
    def var cRow               AS CHARACTER  NO-UNDO INITIAL "A".
    DEF VAR hRsltdos AS HANDLE NO-UNDO.
    DEF VAR hClientes AS HANDLE NO-UNDO.
    DEF VAR hrpstrio AS HANDLE NO-UNDO.
    DEF VAR hpro_crdtos AS HANDLE NO-UNDO.
    DEF VAR hagncias AS HANDLE NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR cL AS CHAR NO-UNDO.
    DEF VAR cTblas AS CHAR NO-UNDO.
    DEF VAR hTbla AS HANDLE NO-UNDO.
    DEF VAR j AS INTEGER NO-UNDO.
    DEF VAR l AS INTEGER NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO.

    RUN Imp_Control.  /*Imprime El Informe*/
    {Incluido/RepEncabezado.I}

    DEFINE VAR W_Titulo2 AS CHARACTER FORMAT "X(120)".
    W_Reporte    = "REPORTE   : REPOSITORIO - CONSULTA RESULTADOS MODELO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna =  "".
    
    VIEW FRAME F-ftr.
    VIEW FRAME F-Encabezado.
    DEFINE FRAME XX HEADER
        "                         PRIMER          SEGUNDO                                                  NUMERO                  PERDIDA PROBABILID          CALIF         EXPOSICION     PERDIDA AÑO  MES"
        SKIP
        "FECHA    NIT             APELLIDO        APELLIDO        NOMBRES                                  PRESTAMO               ESPERADA INCUMPLIMI CALIFICA EQUIV.            ACTIVO    ESPERADA CONTABLE"
        WITH PAGE-TOP
        WIDTH 250 STREAM-IO.
    VIEW FRAME XX.
    
    DEFINE VAR Listado     AS CHARACTER INITIAL "".
    
    /*  PUT UNFORMATTED FILL("-",128) SKIP. */
    cl = "".
    /*  PUT UNFORMATTED FILL("-",128) SKIP(1). */

    
    
    hrsltdos = BUFFER resultados:HANDLE.
    hclientes = BUFFER clientes:HANDLE.
    hRpstrio = BUFFER repositorio:HANDLE.
    hpro_crdtos = BUFFER pro_creditos:HANDLE.
    hagncias = BUFFER agencias:HANDLE.
    /* LISTA DE LOS CAMPOS A MOSTRAR */
    ccmpos = "fecha,Nit,apellido1,apellido2,nombre,prestamo,PerdidaEsperada,ProbabilidadIncumplimiento,calificacion,CalificacionEquivalente,ExposicionActivo,PerdidaEsperada%,AnoContable,MesContable".
    /* TABLA CORRESPONDIENTE DEFINIDA EN EL QUERY */
    ctblas = "resultados,resultados,clientes,clientes,clientes,resultados,resultados,resultados,resultados,resultados,resultados,resultados,resultados,resultados".

    CREATE QUERY hq.
    hq:SET-BUFFERS(hrsltdos,hclientes,hrpstrio,hpro_crdtos,hagncias).
    hq:QUERY-PREPARE(DYNAMIC-FUNCTION('getQueryWhere' IN h_drepo_resultados)).
    hq:QUERY-OPEN(). 
    hq:GET-FIRST().

    IF AVAILABLE resultados
    THEN DO:
        
        REPEAT:
            cl = "".
            ASSIGN iRow = iRow + 1.
            DO i = 1 TO NUM-ENTRIES(ccmpos,","):
                htbla = hq:GET-BUFFER-HANDLE(ENTRY(i,ctblas,",")).
                hcmpo = htbla:BUFFER-FIELD(ENTRY(i,ccmpos,",")).
                cl = cl + string(hcmpo:BUFFER-VALUE,hcmpo:FORMAT) + " ".
            END.
            PUT UNFORMATTED cl SKIP.
            IF hQ:GET-NEXT() = FALSE 
            THEN LEAVE.
        END.    
        hq:QUERY-CLOSE().
        DELETE OBJECT hq.
        DELETE OBJECT htbla.
        DELETE OBJECT hclientes.
        DELETE OBJECT hrsltdos.
        DELETE OBJECT hcmpo.
        PUT UNFORMATTED skip(2) "R E S U M E N E S" SKIP(2).
        c = DYNAMIC-FUNCTION('fAgncia':U IN h_w-repo-resu).
        fPrintMnsje(c).
        c = DYNAMIC-FUNCTION('flnea':U IN h_w-repo-resu).
        fPrintMnsje(c).
        c = DYNAMIC-FUNCTION('fAgnciaLnea':U IN h_w-repo-resu).
        fPrintMnsje(c).
        c = DYNAMIC-FUNCTION('fLneaAgncia':U IN h_w-repo-resu).
        fPrintMnsje(c).
    END.
    ELSE DO:
        MESSAGE "Subconjunto De Resultados Vacío" VIEW-AS ALERT-BOX INFORMATION TITLE "INFORMACION".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pttlzar wWin 
PROCEDURE Pttlzar :
/*------------------------------------------------------------------------------
  Purpose:  Pttlzar   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var ccmpos            AS CHARACTER  NO-UNDO.
    def var hq             AS HANDLE     NO-UNDO.
    def var hBuffer            AS HANDLE     NO-UNDO.
    def var hcmpo             AS HANDLE     NO-UNDO.
    def var icmpos            AS INTEGER    NO-UNDO.
    def var iRow               AS INTEGER    NO-UNDO.
    def var cRow               AS CHARACTER  NO-UNDO INITIAL "A".
    DEF VAR hRsltdos AS HANDLE NO-UNDO.
    DEF VAR hClientes AS HANDLE NO-UNDO.
    DEF VAR hrpstrio AS HANDLE NO-UNDO.
    DEF VAR hpro_crdtos AS HANDLE NO-UNDO.
    DEF VAR hAgncias AS HANDLE NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR cL AS CHAR NO-UNDO.
    DEF VAR cTblas AS CHAR NO-UNDO.
    DEF VAR hTbla AS HANDLE NO-UNDO.
    DEF VAR j AS INTEGER NO-UNDO.
    DEF VAR l AS INTEGER NO-UNDO.
    DEF VAR deTtal AS DECIMAL NO-UNDO.

    EMPTY TEMP-TABLE tprddaesprda.
    
    hrsltdos = BUFFER resultados:HANDLE.
    hclientes = BUFFER clientes:HANDLE.
    hRpstrio = BUFFER repositorio:HANDLE.
    hpro_crdtos = BUFFER pro_creditos:HANDLE.
    hagncias = BUFFER agencias:HANDLE.

    ccmpos = "PerdidaEsperada,int001,int003".
    /* TABLA CORRESPONDIENTE DEFINIDA EN EL QUERY */
    ctblas = "resultados,repositorio,repositorio".

    CREATE QUERY hq.
    hq:SET-BUFFERS(hrsltdos,hclientes,hrpstrio,hpro_crdtos,hagncias).
    hq:QUERY-PREPARE(DYNAMIC-FUNCTION('getQueryWhere' IN h_drepo_resultados)).
    
    hq:QUERY-OPEN(). 
    hq:GET-FIRST().
    IF AVAILABLE resultados THEN DO:
        REPEAT:
            cl = "".
            ASSIGN iRow = iRow + 1.
            CREATE tPrddaEsprda.

            DO i = 1 TO NUM-ENTRIES(ccmpos,","):
                htbla = hq:GET-BUFFER-HANDLE(ENTRY(i,ctblas,",")).
                hcmpo = htbla:BUFFER-FIELD(ENTRY(i,ccmpos,",")).
                CASE hcmpo:NAME:
                    WHEN "PerdidaEsperada" 
                    THEN do:
                        dettal = dettal + hcmpo:BUFFER-VALUE.
                        tPrddaEsprda.dePrdda = hcmpo:BUFFER-VALUE.
                    END.
                    WHEN "int001" THEN tPrddaEsprda.iagncia = hcmpo:BUFFER-VALUE.
                    WHEN "int003" THEN tPrddaEsprda.iCdgoCrdto = hcmpo:BUFFER-VALUE.
                END CASE.
            END.
            IF hQ:GET-NEXT() = FALSE 
            THEN LEAVE.
        END.    
        hq:QUERY-CLOSE().
        DELETE OBJECT hq.
        DELETE OBJECT htbla.
        DELETE OBJECT hclientes.
        DELETE OBJECT hrsltdos.
        DELETE OBJECT hcmpo.
        RUN pMuestraTtalPrdda IN h_w-repo-resu(STRING(dettal)).
    END.
    ELSE DO:
        RUN pMuestraTtalPrdda IN h_w-repo-resu(STRING(dettal)).
        MESSAGE "Subconjunto De Resultados Vacío" VIEW-AS ALERT-BOX INFORMATION TITLE "INFORMACION".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE queryopened wWin 
PROCEDURE queryopened :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        APPLY "choose" TO Btn_Termina-2.
        APPLY "choose" TO BUTT-ttles.
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
  (i AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(i).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fL1TOL2 wWin 
FUNCTION fL1TOL2 RETURNS CHARACTER
  (cbuscar AS CHAR,corigen AS CHAR,cdestino AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(cbuscar,corigen,cdestino).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fprintMnsje wWin 
FUNCTION fprintMnsje RETURNS LOGICAL
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR cl AS CHAR NO-UNDO.
    PAGE.
    DO I = 1 TO NUM-ENTRIES(c,CHR(10)):
        cl = ENTRY(i,c,CHR(10)).
        IF TRIM(cl) = "" THEN PUT UNFORMATTED SKIP(1).
        PUT UNFORMATTED cl SKIP.
    END.

  RETURN TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fRsmenAgnciaLnea wWin 
FUNCTION fRsmenAgnciaLnea RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fRsmenAgnciaLnea
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR Lneas AS CHAR NO-UNDO.
    DEF VAR cl AS CHAR NO-UNDO.
    
    Lneas = "RESUMEN AGENCIA/LINEA".
    FOR EACH tPrddaEsprda,
        FIRST agencias FIELDS (agencia nombre) NO-LOCK
            WHERE
                agencias.agencia = tprddaesprda.iagncia,
        FIRST pro_credito NO-LOCK
            WHERE
                pro_credito.cod_credito = tprddaesprda.icdgocrdto
        BREAK
            BY tprddaesprda.iagncia
            BY tprddaesprda.icdgocrdto:
        ACCUMULATE tprddaesprda.deprdda (TOTAL BY tprddaesprda.icdgocrdto BY tprddaesprda.iagncia).

        IF FIRST-OF(tprddaesprda.iagncia)
        THEN DO:
            cl = "".
            OVERLAY(cl,1,3) = string(agencias.agencia,"999").
            OVERLAY(cl,5,30) = UPPER(agencias.nombre) + ":".
            lneas = lneas   + chr(10)  + CHR(10) + cl.
        END.


        IF LAST-OF(tprddaesprda.icdgocrdto)
        THEN DO:
            cl = "".
            OVERLAY(cl,1,3) = string(pro_credito.cod_credito,"999").
            OVERLAY(cl,5,30) = pro_credito.nom_producto.
            OVERLAY(cl,36) = string(ACCUM TOTAL BY tprddaesprda.icdgocrdto tprddaesprda.deprdda,"$->>>,>>>,>>>,>>9").
            Lneas = Lneas +  CHR(10) + cl.
        END.

        IF LAST-OF(tprddaesprda.iagncia)
        THEN DO:
            cl = "".
            OVERLAY(cl,1,3) = string(agencias.agencia,"999").
            OVERLAY(cl,5,30) = "TOTAL " + upper(agencias.nombre).
            OVERLAY(cl,36) = string(ACCUM TOTAL BY tprddaesprda.iagncia tprddaesprda.deprdda,"$->>>,>>>,>>>,>>9").
            lneas = lneas +  CHR(10) + cl.
        END.
    END.
    RUN pRsmenAgnciaLnea IN h_w-repo-resu(Lneas).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fRsmenAgncias wWin 
FUNCTION fRsmenAgncias RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fRsmenAgncias
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR cRsmenAgncias AS CHAR NO-UNDO.
    DEF VAR cl AS CHAR NO-UNDO.
    
    cRsmenAgncias = "RESUMEN POR AGENCIAS" + CHR(10).
    FOR EACH tPrddaEsprda,
        FIRST agencias FIELDS (agencia nombre) NO-LOCK
            WHERE
                agencias.agencia = tprddaesprda.iagncia
        BREAK
            BY tprddaesprda.iagncia:
        ACCUMULATE tprddaesprda.deprdda (TOTAL BY tprddaesprda.iagncia).
        IF LAST-OF(tprddaesprda.iagncia)
        THEN DO:
            OVERLAY(cl,1,3) = string(agencias.agencia,"999").
            OVERLAY(cl,5,30) = agencias.nombre.
            OVERLAY(cl,36) = string(ACCUM TOTAL BY tprddaesprda.iagncia tprddaesprda.deprdda,"$->>>,>>>,>>>,>>9").
            cRsmenAgncias = cRsmenAgncias +  CHR(10) + cl.
        END.
    END.
    RUN pRsmenAgncias IN h_w-repo-resu(crsmenAgncias).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fRsmenLneaAgncia wWin 
FUNCTION fRsmenLneaAgncia RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fRsmenLneaAgncia
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR Lneas AS CHAR NO-UNDO.
    DEF VAR cl AS CHAR NO-UNDO.
    
    Lneas = "RESUMEN LINEA/AGENCIA".
    FOR EACH tPrddaEsprda,
        FIRST agencias FIELDS (agencia nombre) NO-LOCK
            WHERE
                agencias.agencia = tprddaesprda.iagncia,
        FIRST pro_credito NO-LOCK
            WHERE
                pro_credito.cod_credito = tprddaesprda.icdgocrdto
        BREAK
            BY tprddaesprda.icdgocrdto
            BY tprddaesprda.iagncia:
        ACCUMULATE tprddaesprda.deprdda (TOTAL BY tprddaesprda.icdgocrdto BY tprddaesprda.iagncia).

        IF FIRST-OF(tprddaesprda.icdgocrdto)
        THEN DO:
            cl = "".
            OVERLAY(cl,1,3) = string(pro_credito.cod_credito,"999").
            OVERLAY(cl,5,30) = upper(pro_credito.nom_producto) + ":".
            Lneas = Lneas +  chr(10) + CHR(10) + cl.
        END.

        IF LAST-OF(tprddaesprda.iagncia)
        THEN DO:
            cl = "".
            OVERLAY(cl,1,3) = string(agencias.agencia,"999").
            OVERLAY(cl,5,30) = agencias.nombre.
            OVERLAY(cl,36) = string(ACCUM TOTAL BY tprddaesprda.iagncia tprddaesprda.deprdda,"$->>>,>>>,>>>,>>9").
            lneas = lneas +  CHR(10) + cl.
        END.

        IF LAST-OF(tprddaesprda.icdgocrdto)
        THEN DO:
            cl = "".
            OVERLAY(cl,1,3) = string(pro_credito.cod_credito,"999").
            OVERLAY(cl,5,30) = "TOTAL " + upper(pro_credito.nom_producto).
            OVERLAY(cl,36) = string(ACCUM TOTAL BY tprddaesprda.icdgocrdto tprddaesprda.deprdda,"$->>>,>>>,>>>,>>9").
            Lneas = Lneas +  CHR(10) + cl.
        END.

    END.
    RUN pRsmenLneaAgncia IN h_w-repo-resu(Lneas).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fRsmenLneas wWin 
FUNCTION fRsmenLneas RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fRsmenLneas
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR cRsmenLneas AS CHAR NO-UNDO.
    DEF VAR cl AS CHAR NO-UNDO.
    
    cRsmenLneas = "RESUMEN POR LINEAS" + CHR(10).
    FOR EACH tPrddaEsprda,
        FIRST pro_credito NO-LOCK
            WHERE
                pro_credito.cod_credito = tprddaesprda.icdgocrdto
        BREAK
            BY tprddaesprda.icdgocrdto:
        ACCUMULATE tprddaesprda.deprdda (TOTAL BY tprddaesprda.icdgocrdto).
        IF LAST-OF(tprddaesprda.icdgocrdto)
        THEN DO:
            OVERLAY(cl,1,3) = string(pro_credito.cod_credito,"999").
            OVERLAY(cl,5,30) = pro_credito.nom_producto.
            OVERLAY(cl,36) = string(ACCUM TOTAL BY tprddaesprda.icdgocrdto tprddaesprda.deprdda,"$->>>,>>>,>>>,>>9").
            cRsmenLneas = cRsmenLneas +  CHR(10) + cl.
        END.
    END.
    RUN pRsmenLneas IN h_w-repo-resu(cRsmenLneas).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fToExcelVlor wWin 
FUNCTION fToExcelVlor RETURNS LOGICAL
  (pCol AS char,pval AS char /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN SUPER(pcol,pval).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

