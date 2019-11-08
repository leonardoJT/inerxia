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
&Scoped-Define ENABLED-FIELDS RowObject.Fec_IniDepre ~
RowObject.Cos_Historico RowObject.Neto RowObject.Per_Depreciado ~
RowObject.Anos_Adepreciar RowObject.ValDepMes RowObject.ValDepAcum ~
RowObject.Sdo_Depre 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-286 RECT-287 RECT-288 
&Scoped-Define DISPLAYED-FIELDS RowObject.Fec_IniDepre ~
RowObject.Cos_Historico RowObject.Neto RowObject.Per_Depreciado ~
RowObject.Anos_Adepreciar RowObject.ValDepMes RowObject.ValDepAcum ~
RowObject.Sdo_Depre 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-286
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 3.04.

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 2.96.

DEFINE RECTANGLE RECT-288
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 102 BY 4.04.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Fec_IniDepre AT ROW 2.19 COL 12 COLON-ALIGNED NO-LABEL WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     RowObject.Cos_Historico AT ROW 2.35 COL 55.29 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     RowObject.Neto AT ROW 2.35 COL 83 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     RowObject.Per_Depreciado AT ROW 3.19 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     RowObject.Anos_Adepreciar AT ROW 3.23 COL 12 COLON-ALIGNED NO-LABEL WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     RowObject.ValDepMes AT ROW 6.12 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 19.57 BY .81
          BGCOLOR 15 
     RowObject.ValDepAcum AT ROW 6.12 COL 60.86 COLON-ALIGNED NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 21.14 BY .81
          BGCOLOR 15 
     RowObject.Sdo_Depre AT ROW 7.19 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 21 BY .81
          BGCOLOR 15 
     "Saldo:" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 7.19 COL 55 WIDGET-ID 36
     "Acumulado:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 6.31 COL 52 WIDGET-ID 34
     "Mensual:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 6.31 COL 20 WIDGET-ID 32
     "Costos" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.54 COL 69.29 WIDGET-ID 24
     "Histórico:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 2.54 COL 48 WIDGET-ID 26
     "Fec. Inicial:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 2.42 COL 4 WIDGET-ID 42
     "Vida Util :" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.42 COL 5 WIDGET-ID 18
     "Periodos (Meses)" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.46 COL 13 WIDGET-ID 16
     "Depreciados:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 3.27 COL 22 WIDGET-ID 20
     "Valores Depreciación" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 5.04 COL 43 WIDGET-ID 30
     RECT-286 AT ROW 1.27 COL 3 WIDGET-ID 22
     RECT-287 AT ROW 1.27 COL 45 WIDGET-ID 28
     RECT-288 AT ROW 4.5 COL 3 WIDGET-ID 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17  WIDGET-ID 100.


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
         HEIGHT             = 7.81
         WIDTH              = 105.
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
                "00) Identificación Cliente".

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

