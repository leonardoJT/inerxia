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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Excel BUTTON-154 BUTTON-155 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dttcreditos_a AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-2 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Excel 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Excel" 
     SIZE 6 BY 1.5
     FONT 8.

DEFINE BUTTON BUTTON-154 
     LABEL "Button 154" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-155 
     LABEL "columnas" 
     SIZE 15 BY 1.12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_Excel AT ROW 1 COL 56 WIDGET-ID 56
     BUTTON-154 AT ROW 1.27 COL 26 WIDGET-ID 2
     BUTTON-155 AT ROW 1.54 COL 68 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.14 BY 17 WIDGET-ID 100.


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
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 17
         WIDTH              = 99.14
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
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Excel wWin
ON CHOOSE OF Btn_Excel IN FRAME fMain /* Excel */
DO:
    DEFINE VARIABLE viCnt AS INTEGER INITIAL 0 NO-UNDO.

/*     ASSIGN vcFiltro = "((Fecha_Transaccion >= " + FFecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " AND " +                                                       */
/*                       "Fecha_Transaccion <= " + FFecFin:SCREEN-VALUE + ") OR (" + FFecIni:SCREEN-VALUE + " EQ ? AND " + FFecFin:SCREEN-VALUE + " EQ ?)) AND " + */
/*                       "((Agencia >= " + FAgeIni:SCREEN-VALUE + " AND " +                                                                                        */
/*                       "Agencia <= " + FAgeFin:SCREEN-VALUE + ") OR (" + FAgeIni:SCREEN-VALUE + " = 0 AND " + FAgeFin:SCREEN-VALUE + " = 0)) AND " +             */
/*                       "((Instancia >= " + FInsIni:SCREEN-VALUE + " AND " +                                                                                      */
/*                       "Instancia <= " + FInsFin:SCREEN-VALUE + ") OR (" + FInsIni:SCREEN-VALUE + " = 0 AND " + FInsFin:SCREEN-VALUE + " = 0)) AND " +           */
/*                       "((usuGestiona >= '" + FUsuIni:SCREEN-VALUE + "' AND " +                                                                                  */
/*                       "usuGestiona <= '" + FUsuFin:SCREEN-VALUE + "') OR (" + FUsuIni:SCREEN-VALUE + " = 0 AND " + FUsuFin:SCREEN-VALUE + " = 0)) AND " +       */
/*                       "(estado = LOGICAL(" + REstado:SCREEN-VALUE + ") OR LOGICAL(" + REstado:SCREEN-VALUE + ") = ?) AND " +                                    */
/*                       " (TRUE)".                                                                                                                                */

/*     DYNAMIC-FUNCTION('setQueryWhere':U IN h_dttcreditos_a, */
/*      INPUT vcFiltro /* CHARACTER */).                      */
/*     DYNAMIC-FUNCTION('openQuery':U IN h_dttcreditos_a).    */

    RUN fetchLast IN h_dttcreditos_a.
    ASSIGN viCnt = DYNAMIC-FUNCTION('getLastRowNum':U IN h_dttcreditos_a).

/*     RUN exportData IN h_dttcreditos_a */
/*     ( INPUT "Excel" /* CHARACTER */,  */
/*       INPUT " " /* CHARACTER */,      */
/*       INPUT YES /* LOGICAL */,        */
/*       INPUT YES /* LOGICAL */,        */
/*       INPUT viCnt /* INTEGER */).     */

    MESSAGE "Se va a generar archivo Excel con " SKIP
        viCnt " registros." SKIP
        "Esto puede tardar unos segundos." SKIP
        "Desea continuar?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO 
        TITLE "Generar a Excel" UPDATE vlgenerar AS LOGICAL.



    IF vlgenerar THEN 
        RUN transferToExcel IN h_dttcreditos_a
        ( INPUT " "/* "Agencia,Cod_Credito,Cuota,Estado,Nit,Monto,Num_Credito" */ /* CHARACTER */,
          INPUT YES /* LOGICAL */,
          INPUT YES /* LOGICAL */,
          INPUT viCnt /* INTEGER */).
    ELSE
        RETURN.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-154
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-154 wWin
ON CHOOSE OF BUTTON-154 IN FRAME fMain /* Button 154 */
DO:  
    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dttcreditos_a,
     INPUT "agencia EQ 1" /* CHARACTER */).
    DYNAMIC-FUNCTION('openQuery':U IN h_dttcreditos_A).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-155
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-155 wWin
ON CHOOSE OF BUTTON-155 IN FRAME fMain /* columnas */
DO:

/*     RUN displayFields IN h_dynbrowser-2 */
/*     ( INPUT "NIT" /* CHARACTER */).     */

/*     DYNAMIC-FUNCTION('colValues':U IN h_dynbrowser-2, */
/*      INPUT pcViewColList /* CHARACTER */).1           */


/*         RUN displayFields IN h_dynbrowser-2 */
/*     ( INPUT "NIT" /* CHARACTER */).         */


    DYNAMIC-FUNCTION('setDisplayedFields':U IN h_dynbrowser-2, 
                     INPUT "Nit ").

    RUN initializeObject IN h_dynbrowser-2.
/*    RUN viewObject IN h_dynbrowser-2. */


/*     DYNAMIC-FUNCTION('openQuery':U IN h_dttcreditos_A). */

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
             INPUT  'dttcreditos_a.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedttcreditos_aOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dttcreditos_a ).
       RUN repositionObject IN h_dttcreditos_a ( 1.81 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsAbogado,Agencia,Age_DebAutomatico,Age_Desembolso,Capital_Acum,Categoria,CategoriaMes,Cod_Califica,Cod_CalificaMes,Cod_Credito,Cod_DebAutomatico,Cod_Desembolso,Costas,Cta_Contable,Cue_DebAutomatico,Cue_Desembolso,Cuota,Cuo_Atraso,Cuo_Pagadas,Deducible,Desembolso,Destino,Detalle_Estado,Dias_Atraso,Estado,Fec_Aprobacion,Fec_Calificacion,Fec_CanceTotal,Fec_Desembolso,Fec_DifCobro,Fec_PagAnti,Fec_Pago,Fec_ProxLiquidacion,Fec_Reestructurado,Fec_UltLiquidacion,Fec_UltPago,For_Interes,For_Pago,Honorarios,Id_Adicionales,Incremento,Int_AntDesembolso,Int_Anticipado,Int_Corrientes,Int_DifCobro,Int_LiqAcum,Int_MoraDifCob,Int_MorCobrar,Monto,Nit,Nit_Juzgado,Nom_Juzgado,Num_Credito,Num_Solicitud,Observaciones,Pagare,Per_Gracia,Per_Pago,Plazo,Poliza,Polizas,Provision,Provision_Interes,Provision_Otros,Reestructurado,Sdo_Anuales1,Sdo_Anuales2,Sdo_Anuales3,Sdo_Anuales4,Sdo_Anuales5,Sdo_Anuales6,Sdo_Anuales7,Sdo_Anuales8,Sdo_Anuales9,Sdo_Anuales10,Sdo_Anuales11,Sdo_Anuales12,Sdo_Capital,Sdo_CapPag,Sdo_IntMor,Sdo_IntPag,Sdo_Proyectado,Sistema,Tasa,Tip_Credito,Usuario,Val_Atraso,Val_DesembolsoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-2 ).
       RUN repositionObject IN h_dynbrowser-2 ( 3.42 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-2 ( 12.92 , 95.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser-2. */
       RUN addLink ( h_dttcreditos_a , 'Data':U , h_dynbrowser-2 ).
       RUN addLink ( h_dynbrowser-2 , 'Update':U , h_dttcreditos_a ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser-2 ,
             BUTTON-155:HANDLE IN FRAME fMain , 'AFTER':U ).
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
  ENABLE Btn_Excel BUTTON-154 BUTTON-155 
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

