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
&Scoped-define FRAME-NAME F-Main

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dclientes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dfacturacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dPeriodos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Facturacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Periodos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vclientes_Ubic AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     "            CRÉDITO ROTATIVO - FACTURACIÓN" VIEW-AS TEXT
          SIZE 62 BY 1.08 AT ROW 1.19 COL 22 WIDGET-ID 6
          BGCOLOR 18 FGCOLOR 15 FONT 0
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 131.86 BY 20.04 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Facturación Cupo Rotativo"
         HEIGHT             = 20.04
         WIDTH              = 131.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Facturación Cupo Rotativo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Facturación Cupo Rotativo */
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
             INPUT  'z:/prg/dPeriodos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedPeriodosOpenOnInityesPromptColumnsPer_Factura,Fec_Final,Fec_Inicial,Fec_LimPago,EstadoPromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dPeriodos ).
       RUN repositionObject IN h_dPeriodos ( 2.08 , 124.00 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 8.00 ) */

       RUN constructObject (
             INPUT  'dfacturacion.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsFacturacion.Per_Factura,Per_FacturaObjectNamedFacturacionOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dfacturacion ).
       RUN repositionObject IN h_dfacturacion ( 4.23 , 125.00 ) NO-ERROR.
       /* Size in AB:  ( 2.04 , 7.72 ) */

       RUN constructObject (
             INPUT  'dclientes.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsClientes.Nit,NitObjectNamed-clientes_facturacionOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dclientes ).
       RUN repositionObject IN h_dclientes ( 6.65 , 125.00 ) NO-ERROR.
       /* Size in AB:  ( 2.15 , 7.86 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FolderLabels':U + 'Periodos|Adicionar|Ir a Asociado|Facturación Preliminar|Generar Facturación|Crear Periodos|Imprimir' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 2.62 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 17.23 , 123.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dfacturacion. */
       RUN addLink ( h_dPeriodos , 'Data':U , h_dfacturacion ).

       /* Links to SmartDataObject h_dclientes. */
       RUN addLink ( h_dfacturacion , 'Data':U , h_dclientes ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'DisplayedFieldsNit,Per_Factura,Agencia,Num_factura,Estado,Num_Credito,Tasa_Ic,Tasa_Ea,fec_corte,Fec_LimPago,Sdo_Anterior,Sdo_Mora,Val_atrasokpen,Fec_Mora,Cuota,CupoDisponible,Sobrecupo,CupoTotal,Pago_Minimo,Pago_Total,Nuevo_Saldo,Int_Corrientes,Int_MorCobrar,Seg_cartera,Cargos,OtrosCargos,Pagos,PagoTotal,Val_Recaudo,Rec_Capital,Rec_Honorarios,Rec_IntCorrientes,Rec_IntMora,Rec_SegcarteraEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldNitDataSourceNamesdFacturacionUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Facturacion ).
       RUN repositionObject IN h_dynbrowser-Facturacion ( 4.50 , 55.86 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Facturacion ( 4.85 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  '//172.16.31.149/sfgpruebas/prg/vclientes_Ubicacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vclientes_Ubic ).
       RUN repositionObject IN h_vclientes_Ubic ( 9.62 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 9.42 , 118.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'DisplayedFieldsPer_Factura,Fec_Inicial,Fec_Final,Fec_LimPago,EstadoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndnoUseSortIndicatornoSearchFieldPer_FacturaDataSourceNamesdPeriodosUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Periodos ).
       RUN repositionObject IN h_dynbrowser-Periodos ( 4.50 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Periodos ( 4.85 , 51.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser-Facturacion. */
       RUN addLink ( h_dfacturacion , 'Data':U , h_dynbrowser-Facturacion ).
       RUN addLink ( h_dynbrowser-Facturacion , 'Update':U , h_dfacturacion ).

       /* Links to SmartDataViewer h_vclientes_Ubic. */
       RUN addLink ( h_dclientes , 'Data':U , h_vclientes_Ubic ).

       /* Links to SmartDataBrowser h_dynbrowser-Periodos. */
       RUN addLink ( h_dPeriodos , 'Data':U , h_dynbrowser-Periodos ).
       RUN addLink ( h_dynbrowser-Periodos , 'Update':U , h_dPeriodos ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser-Periodos ,
             h_dfacturacion , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser-Facturacion ,
             h_dynbrowser-Periodos , 'AFTER':U ).
       RUN adjustTabOrder ( h_vclientes_Ubic ,
             h_dclientes , 'AFTER':U ).
    END. /* Page 1 */

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
  VIEW FRAME F-Main IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
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

