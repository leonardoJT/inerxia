&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dPSarlaft.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dPSarlaft.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_PSarlaft

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Cod_Factor RowObject.perfil ~
RowObject.Nombre RowObject.Estado RowObject.Descripcion_Con ~
RowObject.Des_Controles RowObject.Cargo_Resp 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-291 RECT-2 
&Scoped-Define DISPLAYED-FIELDS RowObject.Cod_Factor RowObject.perfil ~
RowObject.Nombre RowObject.Estado RowObject.Descripcion_Con ~
RowObject.Des_Controles RowObject.Cargo_Resp 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dcfg_Sarlaft AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcfg_Sarlaft2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvarios AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvarios2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvarios3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvarios4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect3 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-155 
     LABEL "" 
     SIZE 50 BY 1.12.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 17.23.

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_PSarlaft
     RowObject.Cod_Factor AT ROW 1.54 COL 23 COLON-ALIGNED WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 7.43 BY 1
     BUTTON-155 AT ROW 1.54 COL 34 WIDGET-ID 64
     RowObject.perfil AT ROW 3.69 COL 5 COLON-ALIGNED WIDGET-ID 72
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Perfil 1",1,
                     "Perfil 2",2,
                     "Perfil 3",3,
                     "Perfil 4",4,
                     "perfil 5",5
          DROP-DOWN-LIST
          SIZE 16 BY 1
     RowObject.Nombre AT ROW 3.69 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 74
          VIEW-AS COMBO-BOX SORT INNER-LINES 9
          LIST-ITEMS "Compra De Permisos Por Parte De Organizaciones Delectivas","Utilización De Cuentas De Ahorro Prestadas Para Ocultar Pagos","Transacciones De Cuentas De Secuestrados","Uso De Documentación Falsa","Operaciones 'Cenicienta' O Paseo Millonario","Cobro De Extorsiones Por Seguridad O Permisos","Utilización De Cuentas Bancarios De Ex Secuestrados Que Se Prestan Para Dar Apariencia De Legalidad Al Dinero De Una Organización De Secuestradores","Transacciones Por Mayor Valor De Acuerdo A La Actividad Económica","Otras De Acuerdo A Las Nuevas Estrategias De Los Lavadores O Financiadores Del Terrorismo" 
          SIMPLE MAX-CHARS 256
          SIZE 92 BY 6.73
     RowObject.Estado AT ROW 12.04 COL 94 NO-LABEL WIDGET-ID 30
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 22 BY .81
     RowObject.Descripcion_Con AT ROW 13.12 COL 24 NO-LABEL WIDGET-ID 84
          VIEW-AS EDITOR LARGE
          SIZE 92 BY 1.62
     RowObject.Des_Controles AT ROW 16.35 COL 14 NO-LABEL WIDGET-ID 90
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 49 BY 4.04
     RowObject.Responsable_Cont AT ROW 17.96 COL 22 COLON-ALIGNED HELP
          "Responsable Controles del Registro Riesgo Sarlaft" WIDGET-ID 54
          LABEL "Responsable Controles" FORMAT "X(120)"
          VIEW-AS FILL-IN 
          SIZE 40 BY .92
          BGCOLOR 15 
     RowObject.Cargo_Resp AT ROW 18.04 COL 79 COLON-ALIGNED HELP
          "Cargo Responsable del Registro Riesgo Sarlaft" WIDGET-ID 56
          LABEL "Cargo Responsable" FORMAT "X(120)"
          VIEW-AS FILL-IN 
          SIZE 35 BY .92
          BGCOLOR 15 
     RowObject.Actividad AT ROW 19.31 COL 10 COLON-ALIGNED HELP
          "Actividad del Registro Riesgo Sarlaft" WIDGET-ID 42
          LABEL "Actividad" FORMAT "X(120)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 100.86 BY .92
          BGCOLOR 15 
     RowObject.Subproceso AT ROW 19.31 COL 38.29 COLON-ALIGNED HELP
          "Subproceso del Registro Riesgo Sarlaft" WIDGET-ID 40
          LABEL "Subproceso" FORMAT "X(80)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 47 BY .92
          BGCOLOR 15 
     RowObject.Proceso AT ROW 19.31 COL 40 COLON-ALIGNED HELP
          "Proceso del Registro Riesgo Sarlaft" WIDGET-ID 36
          LABEL "Proceso :" FORMAT "X(80)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 41 BY .92
          BGCOLOR 15 
     "Estado" VIEW-AS TEXT
          SIZE 6.29 BY .92 AT ROW 12.04 COL 87 WIDGET-ID 48
     "Controles" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 17.15 COL 4 WIDGET-ID 92
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 116 BY 19.65
         BGCOLOR 17 FONT 5 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_PSarlaft
     "Descripción" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 13.38 COL 13 WIDGET-ID 86
     "Tipología Riesgo" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 6.92 COL 8 WIDGET-ID 78
     "Descripción" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 16.35 COL 3 WIDGET-ID 88
     "REGISTRO RIESGOS SARLAFT" VIEW-AS TEXT
          SIZE 29.72 BY .62 AT ROW 1.81 COL 46.29 WIDGET-ID 66
          FONT 1
     "Consecuencias" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 13.92 COL 10 WIDGET-ID 82
     RECT-291 AT ROW 1.27 COL 33 WIDGET-ID 62
     RECT-2 AT ROW 3.42 COL 1 WIDGET-ID 60
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 116 BY 19.65
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dPSarlaft.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dPSarlaft.i}
      END-FIELDS.
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 19.65
         WIDTH              = 116.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_PSarlaft
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_PSarlaft:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Actividad IN FRAME F_PSarlaft
   NO-DISPLAY NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                   */
ASSIGN 
       RowObject.Actividad:HIDDEN IN FRAME F_PSarlaft           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-155 IN FRAME F_PSarlaft
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Cargo_Resp IN FRAME F_PSarlaft
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
ASSIGN 
       RowObject.Cod_Factor:READ-ONLY IN FRAME F_PSarlaft        = TRUE.

/* SETTINGS FOR EDITOR RowObject.Descripcion_Con IN FRAME F_PSarlaft
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.Descripcion_Con:RETURN-INSERTED IN FRAME F_PSarlaft  = TRUE.

/* SETTINGS FOR EDITOR RowObject.Des_Controles IN FRAME F_PSarlaft
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F_PSarlaft
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX RowObject.Nombre IN FRAME F_PSarlaft
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Proceso IN FRAME F_PSarlaft
   NO-DISPLAY NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                   */
ASSIGN 
       RowObject.Proceso:HIDDEN IN FRAME F_PSarlaft           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Responsable_Cont IN FRAME F_PSarlaft
   NO-DISPLAY NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                   */
ASSIGN 
       RowObject.Responsable_Cont:HIDDEN IN FRAME F_PSarlaft           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Subproceso IN FRAME F_PSarlaft
   NO-DISPLAY NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                   */
ASSIGN 
       RowObject.Subproceso:HIDDEN IN FRAME F_PSarlaft           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_PSarlaft
/* Query rebuild information for FRAME F_PSarlaft
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F_PSarlaft */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
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
             INPUT  'dcfg_Sarlaft.wDB-AWARE':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcfg_SarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch20RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dcfg_Sarlaft ).
       RUN repositionObject IN h_dcfg_Sarlaft ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 2.42 , 14.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldNombreDataSourceFilterTipo = 1NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelSeveridadSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_SeveridadDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCod_Severidad':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 15.00 , 24.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 0.92 , 40.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dcfg_Sarlaft.wDB-AWARE':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcfg_SarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch20RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dcfg_Sarlaft2 ).
       RUN repositionObject IN h_dcfg_Sarlaft2 ( 1.00 , 15.00 ) NO-ERROR.
       /* Size in AB:  ( 2.42 , 14.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldNombreDataSourceFilterTipo = 2NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelProbabilidadSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_ProbabilidadDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCod_Probabilidad':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 14.88 , 81.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 0.92 , 35.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dvarios.wDB-AWARE':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvariosSarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvarios ).
       RUN repositionObject IN h_dvarios ( 1.54 , 93.86 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 7.29 ) */

       RUN constructObject (
             INPUT  'dvarios.wDB-AWARE':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvariosSarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvarios2 ).
       RUN repositionObject IN h_dvarios2 ( 1.54 , 101.43 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 7.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDescripcionDataSourceFilterTipo = 40NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelFactor RiesgoSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameClase_ROperativoDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCodigo':U ,
             OUTPUT h_dynselect3 ).
       RUN repositionObject IN h_dynselect3 ( 10.69 , 81.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect3 ( 0.92 , 35.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dvarios.wDB-AWARE':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvariosSarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvarios3 ).
       RUN repositionObject IN h_dvarios3 ( 1.54 , 109.00 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 7.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDescripcionDataSourceFilterTipo = 39NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelRiesgos AsociadosSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_EveFactorDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCodigo':U ,
             OUTPUT h_dynselect2 ).
       RUN repositionObject IN h_dynselect2 ( 10.69 , 24.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect2 ( 0.92 , 40.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dvarios.wDB-AWARE':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvariosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch20RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dvarios4 ).
       RUN repositionObject IN h_dvarios4 ( 1.54 , 85.57 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 8.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_PSarlaft:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDescripcionDataSourceFiltertipo = 41NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelTipo ControlesSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_ControlesDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCodigo':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 16.35 , 81.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 0.92 , 35.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_dcfg_Sarlaft , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_dcfg_Sarlaft2 , 'Data':U , h_dynselect-4 ).

       /* Links to SmartDataField h_dynselect3. */
       RUN addLink ( h_dvarios2 , 'Data':U , h_dynselect3 ).

       /* Links to SmartDataField h_dynselect2. */
       RUN addLink ( h_dvarios3 , 'Data':U , h_dynselect2 ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dvarios4 , 'Data':U , h_dynselect-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect2 ,
             RowObject.Nombre:HANDLE IN FRAME F_PSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect3 ,
             h_dynselect2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-4 ,
             RowObject.Descripcion_Con:HANDLE IN FRAME F_PSarlaft , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect-4 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             RowObject.Des_Controles:HANDLE IN FRAME F_PSarlaft , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Consulta vTableWin 
PROCEDURE Consulta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER v-cod-factor1 AS INT NO-UNDO.

    /*FIND Sarlaft  ehere 
    IF avaiulable  THEN DO:
        nombre:SCREEN-VALUE
        Cmb_UFactores
    END.
      */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F_PSarlaft.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFieldList vTableWin 
PROCEDURE displayFieldList :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcFieldList  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcFromSource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER phDataSource AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER pcColValues  AS CHARACTER NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
    i = LOOKUP("perfil",pcFieldList,",").
    IF NOT i = 0 
    THEN ENTRY(i,pcColValues,CHR(1)) = "1".
  RUN SUPER( INPUT pcFieldList, INPUT pcFromSource, INPUT phDataSource, INPUT pcColValues).

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

