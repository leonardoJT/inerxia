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
&Scoped-Define ENABLED-FIELDS RowObject.Agencia RowObject.Estado ~
RowObject.Codigo RowObject.Nombre RowObject.Descripcion ~
RowObject.Cen_Costos RowObject.Nit_Seguro RowObject.Nit_Proveedor ~
RowObject.Nro_Seguro RowObject.Nro_Factura RowObject.Vto_Seguro ~
RowObject.Nit_Responsable RowObject.Fec_Compra RowObject.Val_Garantia ~
RowObject.Ord_Compra RowObject.Fec_Garantia RowObject.Val_Compra ~
RowObject.Mejoras RowObject.Fec_Retiro RowObject.Fec_IniDepre ~
RowObject.Fec_debaja RowObject.Anos_Adepreciar RowObject.Fec_Venta ~
RowObject.Per_Depreciado RowObject.ValDepMes RowObject.Cos_Historico ~
RowObject.Neto RowObject.ValDepAcum RowObject.Sdo_Depre ~
RowObject.Fec_Avaluo 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-286 RECT-287 RECT-288 RECT-289 
&Scoped-Define DISPLAYED-FIELDS RowObject.Agencia RowObject.Estado ~
RowObject.Codigo RowObject.Nombre RowObject.Descripcion ~
RowObject.Cen_Costos RowObject.Nit_Seguro RowObject.Nit_Proveedor ~
RowObject.Nro_Seguro RowObject.Nro_Factura RowObject.Vto_Seguro ~
RowObject.Nit_Responsable RowObject.Fec_Compra RowObject.Val_Garantia ~
RowObject.Ord_Compra RowObject.Fec_Garantia RowObject.Val_Compra ~
RowObject.Mejoras RowObject.Fec_Retiro RowObject.Fec_IniDepre ~
RowObject.Fec_debaja RowObject.Anos_Adepreciar RowObject.Fec_Venta ~
RowObject.Per_Depreciado RowObject.ValDepMes RowObject.Cos_Historico ~
RowObject.Neto RowObject.ValDepAcum RowObject.Sdo_Depre ~
RowObject.Fec_Avaluo 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dgrupos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-286
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 5.12.

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 2.42.

DEFINE RECTANGLE RECT-288
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 5.12.

DEFINE RECTANGLE RECT-289
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 3.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Agencia AT ROW 1 COL 28 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     RowObject.Estado AT ROW 1.27 COL 107 NO-LABEL WIDGET-ID 14
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Retirado", 2,
"Depreciado", 3
          SIZE 17 BY 1.88
     RowObject.Codigo AT ROW 2.08 COL 28 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     RowObject.Nombre AT ROW 2.08 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 54 BY .81
     RowObject.Descripcion AT ROW 3.04 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 63 BY .81
     RowObject.Cen_Costos AT ROW 4.23 COL 28 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 7 BY .81
     RowObject.Nit_Seguro AT ROW 5.65 COL 54.86 COLON-ALIGNED NO-LABEL WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 19.86 BY .81
     RowObject.Nit_Proveedor AT ROW 5.85 COL 18 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
     RowObject.Nro_Seguro AT ROW 6.5 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 20 BY .81
     RowObject.Nro_Factura AT ROW 6.69 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
     RowObject.Vto_Seguro AT ROW 7.42 COL 55.14 COLON-ALIGNED NO-LABEL WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
     RowObject.Nit_Responsable AT ROW 7.46 COL 92 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
     RowObject.Fec_Compra AT ROW 7.54 COL 18 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     RowObject.Val_Garantia AT ROW 8.38 COL 55.14 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
     RowObject.Ord_Compra AT ROW 8.42 COL 18 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     RowObject.Fec_Garantia AT ROW 9.27 COL 55.29 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 12.72 BY .81
     RowObject.Val_Compra AT ROW 9.31 COL 18 COLON-ALIGNED WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
     RowObject.Mejoras AT ROW 9.62 COL 94 COLON-ALIGNED NO-LABEL WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 20 BY .81
     RowObject.Fec_Retiro AT ROW 12.62 COL 56 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     RowObject.Fec_IniDepre AT ROW 13.38 COL 93 COLON-ALIGNED NO-LABEL WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 18 BY .96
     RowObject.Fec_debaja AT ROW 13.42 COL 56 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     RowObject.Anos_Adepreciar AT ROW 13.5 COL 19 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 8 BY .96
     RowObject.Fec_Venta AT ROW 14.23 COL 56 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     RowObject.Per_Depreciado AT ROW 14.46 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
     RowObject.ValDepMes AT ROW 16.35 COL 72 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 19.72 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.Cos_Historico AT ROW 16.62 COL 25 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
     RowObject.Neto AT ROW 17.42 COL 25 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 19.72 BY 1
     RowObject.ValDepAcum AT ROW 17.42 COL 72 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 19.72 BY 1
     RowObject.Sdo_Depre AT ROW 18.5 COL 72 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 18.57 BY 1
     RowObject.Fec_Avaluo AT ROW 20.38 COL 66 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     "Nro. Factura:" VIEW-AS TEXT
          SIZE 8.43 BY .88 AT ROW 6.65 COL 11 WIDGET-ID 118
     "Aseguradora:" VIEW-AS TEXT
          SIZE 9.43 BY 1.08 AT ROW 5.58 COL 47 WIDGET-ID 116
     "Nro. Póliza:" VIEW-AS TEXT
          SIZE 8.43 BY 1.08 AT ROW 6.38 COL 48.57 WIDGET-ID 114
     "Fecha de Venci/to:" VIEW-AS TEXT
          SIZE 13.57 BY 1.08 AT ROW 7.31 COL 43 WIDGET-ID 78
     "Meses" VIEW-AS TEXT
          SIZE 7 BY 1.62 AT ROW 13.65 COL 32 WIDGET-ID 104
     "Periodos Depre.:" VIEW-AS TEXT
          SIZE 16 BY 1.08 AT ROW 14.42 COL 5 WIDGET-ID 106
     "Fecha Ini.Depre:" VIEW-AS TEXT
          SIZE 11 BY 1.08 AT ROW 13.38 COL 83 WIDGET-ID 88
     "Mejoras:" VIEW-AS TEXT
          SIZE 9 BY 1.08 AT ROW 9.35 COL 87 WIDGET-ID 82
     RECT-286 AT ROW 5.31 COL 2 WIDGET-ID 90
     RECT-287 AT ROW 13.38 COL 1 WIDGET-ID 108
     RECT-288 AT ROW 5.31 COL 42 WIDGET-ID 110
     RECT-289 AT ROW 12.31 COL 41 WIDGET-ID 112
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


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
         HEIGHT             = 21.08
         WIDTH              = 130.72.
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
       FRAME F-Main:HIDDEN           = TRUE.

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
             INPUT  'dgrupos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedgruposOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dgrupos ).
       RUN repositionObject IN h_dgrupos ( 1.54 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDescripcionKeyFieldGrupoDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleGruposBrowseFieldsGrupo,Descripcion,EstadoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameGrupoDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 2.88 , 30.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 14.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dgrupos , 'Data':U , h_dynselect ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.Nombre:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

