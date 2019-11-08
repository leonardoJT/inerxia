&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dsaro.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dsaro.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_PSaro

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Proceso RowObject.Subproceso ~
RowObject.Actividad RowObject.Nombre RowObject.Estado ~
RowObject.Descripcion_Con RowObject.Des_Controles RowObject.Cod_Controles ~
RowObject.Responsable_Cont RowObject.Cargo_Resp 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-291 RECT-290 
&Scoped-Define DISPLAYED-FIELDS RowObject.Proceso RowObject.Subproceso ~
RowObject.Actividad RowObject.Nombre RowObject.Estado ~
RowObject.Descripcion_Con RowObject.Des_Controles RowObject.Cod_Controles ~
RowObject.Responsable_Cont RowObject.Cargo_Resp 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dcfg_saro AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcfg_saro-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvariossaro AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvariossaro-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvariossaro-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect3 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-155 
     LABEL "" 
     SIZE 50 BY 1.12.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 14.54.

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_PSaro
     BUTTON-155 AT ROW 1.54 COL 34 WIDGET-ID 64
     RowObject.Proceso AT ROW 3.96 COL 10 COLON-ALIGNED HELP
          "Proceso del Registro Riesgo Saro" WIDGET-ID 36
          LABEL "Proceso :" FORMAT "X(80)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 41 BY .92
          BGCOLOR 15 
     RowObject.Subproceso AT ROW 3.96 COL 64 COLON-ALIGNED HELP
          "Subproceso del Registro Riesgo Saro" WIDGET-ID 40
          LABEL "Subproceso" FORMAT "X(80)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 47 BY .92
          BGCOLOR 15 
     RowObject.Actividad AT ROW 5.31 COL 10 COLON-ALIGNED HELP
          "Actividad del Registro Riesgo Saro" WIDGET-ID 42
          LABEL "Actividad" FORMAT "X(120)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 100.86 BY .92
          BGCOLOR 15 
     RowObject.Nombre AT ROW 6.62 COL 31.14 COLON-ALIGNED HELP
          "Nombre de la cuenta del plan contable" NO-LABEL WIDGET-ID 16 FORMAT "X(60)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 79.86 BY .92
          BGCOLOR 15 
     RowObject.Estado AT ROW 9.62 COL 78 NO-LABEL WIDGET-ID 30
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 22 BY .81
     RowObject.Descripcion_Con AT ROW 10.96 COL 26.14 COLON-ALIGNED HELP
          "Descripción Consecuencias del Registro Riesgo Saro" WIDGET-ID 50
          LABEL "Descripción Consecuencias" FORMAT "X(120)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 84.86 BY .92
          BGCOLOR 15 
     RowObject.Des_Controles AT ROW 13.92 COL 21 COLON-ALIGNED HELP
          "Descripción Controles del Registro Riesgo Saro" WIDGET-ID 52
          LABEL "Descripción Controles" FORMAT "X(120)"
          VIEW-AS FILL-IN 
          SIZE 43 BY .92
          BGCOLOR 15 
     RowObject.Cod_Controles AT ROW 13.92 COL 81 COLON-ALIGNED HELP
          "Código Controles" WIDGET-ID 58
          LABEL "Tipo Controles" FORMAT "->,>>>,>>9"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "0" 
          DROP-DOWN-LIST
          SIZE 30 BY 1
          BGCOLOR 15 
     RowObject.Responsable_Cont AT ROW 15.54 COL 21 COLON-ALIGNED HELP
          "Responsable Controles del Registro Riesgo Saro" WIDGET-ID 54
          LABEL "Responsable Controles" FORMAT "X(120)"
          VIEW-AS FILL-IN 
          SIZE 38.14 BY .92
          BGCOLOR 15 
     RowObject.Cargo_Resp AT ROW 15.62 COL 81 COLON-ALIGNED HELP
          "Cargo Responsable del Registro Riesgo Saro" WIDGET-ID 56
          LABEL "Cargo Responsable" FORMAT "X(120)"
          VIEW-AS FILL-IN 
          SIZE 30 BY .92
          BGCOLOR 15 
     "PROCESO SARO" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.81 COL 51 WIDGET-ID 66
          FONT 1
     "Descripción  como puede o Pudo" VIEW-AS TEXT
          SIZE 29.43 BY .92 AT ROW 6.5 COL 3.29 WIDGET-ID 44
     "suceder  el riesgo" VIEW-AS TEXT
          SIZE 26.29 BY .92 AT ROW 7.15 COL 3.43 WIDGET-ID 46
     "Estado" VIEW-AS TEXT
          SIZE 6.29 BY .92 AT ROW 9.62 COL 70.86 WIDGET-ID 48
     RECT-291 AT ROW 1.27 COL 33 WIDGET-ID 62
     RECT-290 AT ROW 3.42 COL 1 WIDGET-ID 60
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 116 BY 16.96
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dsaro.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dsaro.i}
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
         HEIGHT             = 16.96
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
/* SETTINGS FOR FRAME F_PSaro
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_PSaro:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Actividad IN FRAME F_PSaro
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR BUTTON BUTTON-155 IN FRAME F_PSaro
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Cargo_Resp IN FRAME F_PSaro
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR COMBO-BOX RowObject.Cod_Controles IN FRAME F_PSaro
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Descripcion_Con IN FRAME F_PSaro
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Des_Controles IN FRAME F_PSaro
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F_PSaro
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Nombre IN FRAME F_PSaro
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Proceso IN FRAME F_PSaro
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Responsable_Cont IN FRAME F_PSaro
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Subproceso IN FRAME F_PSaro
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_PSaro
/* Query rebuild information for FRAME F_PSaro
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F_PSaro */
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
             INPUT  'dcfg_saro.wDB-AWARE':U ,
             INPUT  FRAME F_PSaro:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcfg_saroOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcfg_saro ).
       RUN repositionObject IN h_dcfg_saro ( 1.54 , 85.43 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_PSaro:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldNombreKeyFieldCod_SeveridadDataSourceFilterTipo = 1NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelSeveridadSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_SeveridadDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 12.58 , 16.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 0.92 , 44.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dvariossaro.wDB-AWARE':U ,
             INPUT  FRAME F_PSaro:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvariossaroOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvariossaro ).
       RUN repositionObject IN h_dvariossaro ( 1.54 , 93.43 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.29 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_PSaro:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDescripcionKeyFieldCodigoDataSourceFiltertipo = 38NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelUbic. FactorSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_UbFactorDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 9.62 , 16.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 0.92 , 36.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dvariossaro.wDB-AWARE':U ,
             INPUT  FRAME F_PSaro:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvariossaroOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvariossaro-3 ).
       RUN repositionObject IN h_dvariossaro-3 ( 1.54 , 101.43 ) NO-ERROR.
       /* Size in AB:  ( 1.35 , 7.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_PSaro:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDescripcionKeyFieldCodigoDataSourceFilterTipo = 35NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelRiesgo OperativoSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameClase_ROperativoDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect3 ).
       RUN repositionObject IN h_dynselect3 ( 8.27 , 78.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect3 ( 0.92 , 35.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dvariossaro.wDB-AWARE':U ,
             INPUT  FRAME F_PSaro:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvariossaroOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvariossaro-2 ).
       RUN repositionObject IN h_dvariossaro-2 ( 1.81 , 109.43 ) NO-ERROR.
       /* Size in AB:  ( 1.35 , 7.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_PSaro:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDescripcionKeyFieldCodigoDataSourceFilterTipo = 37NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelFactor RiesgoSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_EveFactorDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect2 ).
       RUN repositionObject IN h_dynselect2 ( 8.27 , 16.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect2 ( 0.92 , 36.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dcfg_saro.wDB-AWARE':U ,
             INPUT  FRAME F_PSaro:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcfg_saroOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcfg_saro-2 ).
       RUN repositionObject IN h_dcfg_saro-2 ( 3.42 , 108.00 ) NO-ERROR.
       /* Size in AB:  ( 1.35 , 8.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_PSaro:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldNombreKeyFieldCod_ProbabilidadDataSourceFilterTipo = 2NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelProbabilidadSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_ProbabilidadDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 12.46 , 74.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 0.92 , 39.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_dcfg_saro , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dvariossaro , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect3. */
       RUN addLink ( h_dvariossaro-3 , 'Data':U , h_dynselect3 ).

       /* Links to SmartDataField h_dynselect2. */
       RUN addLink ( h_dvariossaro-2 , 'Data':U , h_dynselect2 ).

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_dcfg_saro-2 , 'Data':U , h_dynselect-4 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect2 ,
             RowObject.Nombre:HANDLE IN FRAME F_PSaro , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect3 ,
             h_dynselect2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect ,
             h_dynselect3 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-4 ,
             RowObject.Descripcion_Con:HANDLE IN FRAME F_PSaro , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect-4 , 'AFTER':U ).
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

    /*FIND saro  ehere 
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
  HIDE FRAME F_PSaro.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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

