&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dcfg_saro.i"}.



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


DEF TEMP-TABLE TImpacto NO-UNDO
   FIELD Cod_Impacto  AS CHAR
   FIELD nombre       AS CHAR FORMAT "x(100)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dcfg_saro.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Analisis

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Estado RowObject.Cod_Analisis ~
RowObject.Por_dos RowObject.Por_uno 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS Btn_Matriz 
&Scoped-Define DISPLAYED-FIELDS RowObject.Nombre RowObject.Estado ~
RowObject.Cod_Analisis RowObject.Por_dos RowObject.Por_uno 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS v-cod-analisis 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dcfg_saro1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcfg_saro5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcuentas1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcuentas2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Matriz 
     LABEL "Matriz" 
     SIZE 11 BY 1.12.

DEFINE VARIABLE v-cod-analisis AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .92
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Analisis
     RowObject.Nombre AT ROW 1.04 COL 73.86 COLON-ALIGNED HELP
          "Descripcion" WIDGET-ID 318
          LABEL "Descripcion" FORMAT "X(150)"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 10 
     RowObject.Estado AT ROW 2.65 COL 58.43 HELP
          "Estado del registro de la cuenta" NO-LABEL WIDGET-ID 308
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 22 BY .81
     RowObject.Cod_Analisis AT ROW 5.42 COL 1.14 COLON-ALIGNED HELP
          "Clasificacion Impacto del riesgo Operativo" NO-LABEL WIDGET-ID 332 FORMAT "x(1)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Riesgo Inusual-Administrar mediante registro","I",
                     "Riesgo Bajo-Administrar mediante procedimientos de rutina","B",
                     "Riesgo Medio-Requiere especificaciónde responsabilidad gerencial","M",
                     "Riesgo Alto-Requiere atención de la alta gerencia","A",
                     "Riesgo Extremo-Requiere acción inmediata","E"
          DROP-DOWN-LIST
          SIZE 53 BY 1
          BGCOLOR 15 
     v-cod-analisis AT ROW 5.42 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 340
     RowObject.Por_dos AT ROW 6.88 COL 63.43 COLON-ALIGNED HELP
          "% Porcentaje" WIDGET-ID 20
          LABEL "% Porcentaje" FORMAT "->>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.43 BY 1
          BGCOLOR 15 
     Btn_Matriz AT ROW 8.15 COL 77.29 WIDGET-ID 336
     RowObject.Por_uno AT ROW 8.23 COL 63.72 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 8.43 BY 1
          BGCOLOR 15 
     "Estado" VIEW-AS TEXT
          SIZE 19 BY .92 AT ROW 1.81 COL 58.43 WIDGET-ID 312
          FONT 5
     "Clasificacion Impacto del Riesgo Operativo" VIEW-AS TEXT
          SIZE 47 BY .92 AT ROW 4.5 COL 3.14 WIDGET-ID 316
          FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 90.43 BY 8.35
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dcfg_saro.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dcfg_saro.i}
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
         HEIGHT             = 8.35
         WIDTH              = 90.43.
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
/* SETTINGS FOR FRAME F_Analisis
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Analisis:HIDDEN           = TRUE
       FRAME F_Analisis:PRIVATE-DATA     = 
                "02 Información Cónyuge o Compañero (a) permanente".

/* SETTINGS FOR COMBO-BOX RowObject.Cod_Analisis IN FRAME F_Analisis
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F_Analisis
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.Nombre IN FRAME F_Analisis
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.Nombre:HIDDEN IN FRAME F_Analisis           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Por_dos IN FRAME F_Analisis
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN v-cod-analisis IN FRAME F_Analisis
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Analisis
/* Query rebuild information for FRAME F_Analisis
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F_Analisis */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Matriz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Matriz vTableWin
ON CHOOSE OF Btn_Matriz IN FRAME F_Analisis /* Matriz */
DO:
  RUN dial_saro.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Cod_Analisis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Cod_Analisis vTableWin
ON LEAVE OF RowObject.Cod_Analisis IN FRAME F_Analisis /* Cod_Analisis */
DO:
  
  FOR EACH tImpacto WHERE TImpacto.Cod_Impacto 
                        = RowObject.Cod_Analisis:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    NO-LOCK:
    ASSIGN  RowObject.Nombre:SCREEN-VALUE  = tImpacto.nombre.
            
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Cod_Analisis vTableWin
ON VALUE-CHANGED OF RowObject.Cod_Analisis IN FRAME F_Analisis /* Cod_Analisis */
DO:

  v-cod-analisis:SCREEN-VALUE = RowObject.Cod_Analisis:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  
 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

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
             INPUT  FRAME F_Analisis:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcfg_saroOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcfg_saro1 ).
       RUN repositionObject IN h_dcfg_saro1 ( 3.69 , 82.29 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_Analisis:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldNombreDataSourceFilterTipo = 1NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelSeveridadSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsNombre,Cod_NivelExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_SeveridadDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCod_Nivel':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 1.81 , 14.43 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 36.57 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dcuentas.wDB-AWARE':U ,
             INPUT  FRAME F_Analisis:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcuentasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcuentas2 ).
       RUN repositionObject IN h_dcuentas2 ( 3.69 , 73.29 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_Analisis:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldCuentaDataSourceFilterTipo = 2 and Estado = 1NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelCuenta CréditoSortyesViewAsBrowserToolTipFormatX(14)HelpId0BrowseTitleBrowseFieldsCuenta,NombreExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCta_CrDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCuenta':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 8.27 , 17.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 1.00 , 35.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dcfg_saro.wDB-AWARE':U ,
             INPUT  FRAME F_Analisis:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcfg_saroOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcfg_saro5 ).
       RUN repositionObject IN h_dcfg_saro5 ( 5.58 , 82.29 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_Analisis:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldNombreDataSourceFilterTipo = 2NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelProbabilidadSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsNombre,Cod_NivelExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_ProbabilidadDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCod_Nivel':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 3.15 , 14.72 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 36.29 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dcuentas.wDB-AWARE':U ,
             INPUT  FRAME F_Analisis:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcuentasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcuentas1 ).
       RUN repositionObject IN h_dcuentas1 ( 5.58 , 73.29 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F_Analisis:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldCuentaDataSourceFilterTipo = 2 and Estado = 1NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelCuenta DébitoSortyesViewAsBrowserToolTipFormatX(14)HelpId0BrowseTitleBrowseFieldsCuenta,Nombre,Cta_HomologadaExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCta_DbDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCuenta':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 6.92 , 17.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 35.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dcfg_saro1 , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_dcuentas2 , 'Data':U , h_dynselect-4 ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dcfg_saro5 , 'Data':U , h_dynselect-2 ).

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_dcuentas1 , 'Data':U , h_dynselect-3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.Nombre:HANDLE IN FRAME F_Analisis , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             RowObject.Estado:HANDLE IN FRAME F_Analisis , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             RowObject.Por_dos:HANDLE IN FRAME F_Analisis , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-4 ,
             RowObject.Por_uno:HANDLE IN FRAME F_Analisis , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  HIDE FRAME F_Analisis.
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

  DEFINE VAR i AS INT NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcFieldList, INPUT pcFromSource, INPUT phDataSource, INPUT pcColValues).

  /* Code placed here will execute AFTER standard behavior.    */
  i = LOOKUP("Cod_Analisis",pcFieldList,",").

  v-cod-analisis:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(i,pcColValues,CHR(1)).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Impacto vTableWin 
PROCEDURE Impacto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CREATE TImpacto.
  ASSIGN TImpacto.Cod_Impacto = 'I'
         TImpacto.Nombre      = 'Riesgo Inusual-Administrar mediante registro'.

  CREATE TImpacto.
  ASSIGN TImpacto.Cod_Impacto = 'B'
         TImpacto.Nombre      = 'Riesgo Bajo-Administrar mediante procedimientos de rutina'.

  CREATE TImpacto.
  ASSIGN TImpacto.Cod_Impacto = 'M'
         TImpacto.Nombre      = 'Riesgo Medio-Requiere especificaciónde responsabilidad gerencial'.


  CREATE TImpacto.
  ASSIGN TImpacto.Cod_Impacto = 'A'
         TImpacto.Nombre      = 'Riesgo Alto-Requiere atención de la alta gerencia'.


  CREATE TImpacto.
  ASSIGN TImpacto.Cod_Impacto = 'E'
         TImpacto.Nombre      = 'Riesgo Extremo-Requiere acción inmediata'.


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
  RUN impacto.
  
  ASSIGN v-cod-analisis = ''.
  IF RowObject.Cod_Analisis:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? THEN
     ASSIGN v-cod-analisis = ''.
  ELSE 
     ASSIGN v-cod-analisis = RowObject.Cod_Analisis:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

  DISPLAY v-cod-analisis WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

