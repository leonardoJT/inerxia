&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dact_fijo.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dact_fijo.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Codigo RowObject.Nombre ~
RowObject.Descripcion RowObject.Fec_Venta RowObject.Estado ~
RowObject.Fec_Retiro RowObject.Fec_debaja 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-307 RECT-9 
&Scoped-Define DISPLAYED-FIELDS RowObject.Codigo RowObject.Nombre ~
RowObject.Descripcion RowObject.Fec_Venta RowObject.Estado ~
RowObject.Fec_Retiro RowObject.Fec_debaja 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS cAgncia 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dagencias AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcen_costos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvarios AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-age AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-grupo AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cAgncia AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-307
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 2.42.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 6.46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Codigo AT ROW 2.35 COL 16 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN NATIVE 
          SIZE 11 BY .81
          BGCOLOR 15 
     RowObject.Nombre AT ROW 2.35 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 48
          VIEW-AS FILL-IN NATIVE 
          SIZE 37.72 BY .81
          BGCOLOR 15 
     RowObject.Descripcion AT ROW 2.35 COL 68 COLON-ALIGNED NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN NATIVE 
          SIZE 46 BY .81
          BGCOLOR 15 
     RowObject.Fec_Venta AT ROW 3.42 COL 99 COLON-ALIGNED WIDGET-ID 356
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     RowObject.Estado AT ROW 3.69 COL 71 NO-LABEL WIDGET-ID 14
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Retirado", 2,
"Depreciado", 3
          SIZE 15 BY 1.88
     RowObject.Fec_Retiro AT ROW 4.31 COL 99 COLON-ALIGNED WIDGET-ID 354
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     RowObject.Fec_debaja AT ROW 5.23 COL 99 COLON-ALIGNED WIDGET-ID 352
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     cAgncia AT ROW 3.42 COL 53 COLON-ALIGNED NO-LABEL WIDGET-ID 360
     "Descripción" VIEW-AS TEXT
          SIZE 13 BY .54 AT ROW 1.73 COL 89 WIDGET-ID 134
     "Nombre" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 1.81 COL 42.72 WIDGET-ID 132
     RECT-307 AT ROW 3.42 COL 70 WIDGET-ID 358
     RECT-9 AT ROW 1 COL 1 WIDGET-ID 140
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dact_fijo.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dact_fijo.i}
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
         HEIGHT             = 6.58
         WIDTH              = 124.57.
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
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:PRIVATE-DATA     = 
                "02 Información Cónyuge o Compañero (a) permanente".

/* SETTINGS FOR FILL-IN cAgncia IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
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
             INPUT  'dagencias.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedagenciasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch20RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dagencias ).
       RUN repositionObject IN h_dagencias ( 2.08 , 117.00 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 7.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventCambioAgenciaDisplayedFieldNombreKeyFieldAgenciaDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelAgenciaSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleAGENCIASBrowseFieldsAgencia,NombreExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysFieldNameAgenciaDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-age ).
       RUN repositionObject IN h_dynselect-age ( 3.42 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-age ( 1.00 , 37.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dcencostosaf.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcen_costosOpenOnInityesPromptColumnsagencia,NombrePromptOnDeleteyesRowsToBatch20RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dcen_costos ).
       RUN repositionObject IN h_dcen_costos ( 3.96 , 117.00 ) NO-ERROR.
       /* Size in AB:  ( 1.35 , 7.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldNombreKeyFieldCen_CostosDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleCentro de CostosBrowseFieldsCen_Costos,Nombre,agencia,Nombre_AgenciaExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCen_CostosDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 5.58 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 32.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dvarios.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvariosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch20RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dvarios ).
       RUN repositionObject IN h_dvarios ( 5.58 , 117.00 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 7.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldFVariosKeyFieldCodigoDataSourceFiltertipo = 7NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelGrupoSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleGrupoBrowseFieldsCodigo,FVariosExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysFieldNameGrupoDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-grupo ).
       RUN repositionObject IN h_dynselect-grupo ( 4.50 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-grupo ( 1.00 , 24.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-age. */
       RUN addLink ( h_dagencias , 'Data':U , h_dynselect-age ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dcen_costos , 'Data':U , h_dynselect-2 ).

       /* Links to SmartDataField h_dynselect-grupo. */
       RUN addLink ( h_dvarios , 'Data':U , h_dynselect-grupo ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect-age ,
             RowObject.Descripcion:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-grupo ,
             RowObject.Fec_Retiro:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             RowObject.Fec_debaja:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CambioAgencia vTableWin 
PROCEDURE CambioAgencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER vcchar AS CHARACTER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        cAgncia:SCREEN-VALUE = vcchar.
    END.
    DYNAMIC-FUNCTION('closeQuery':U IN h_dcen_costos).
    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dcen_costos,INPUT "cencostosAF.agencia = " + vcchar ).
    DYNAMIC-FUNCTION('openQuery':U IN h_dcen_costos).

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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

