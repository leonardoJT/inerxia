&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"z:/prg/dsolicitud.i"}.



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
&Scoped-define DATA-FIELD-DEFS "z:/prg/dsolicitud.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Num_Solicitud ~
RowObject.Fec_Solicitud RowObject.Agencia RowObject.Estado RowObject.Nit ~
RowObject.Monto RowObject.Usuario 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-316 RECT-317 RECT-318 RECT-319 
&Scoped-Define DISPLAYED-FIELDS RowObject.Num_Solicitud ~
RowObject.Fec_Solicitud RowObject.Nombre-3 RowObject.Agencia ~
RowObject.Estado RowObject.Nit RowObject.Nombre-2 RowObject.Apellido2 ~
RowObject.Apellido1 RowObject.Monto RowObject.Nombre RowObject.Usuario ~
RowObject.Fle 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-316
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 67 BY 9.69.

DEFINE RECTANGLE RECT-317
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65 BY 1.62.

DEFINE RECTANGLE RECT-318
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65 BY 2.69.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Num_Solicitud AT ROW 1.27 COL 12 COLON-ALIGNED WIDGET-ID 6
          LABEL "Solicitud"
          VIEW-AS FILL-IN 
          SIZE 10.57 BY 1
     RowObject.Fec_Solicitud AT ROW 1.27 COL 52 COLON-ALIGNED WIDGET-ID 4
          LABEL "Fecha"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY 1
     RowObject.Nombre-3 AT ROW 2.62 COL 12 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 41.43 BY 1
     RowObject.Agencia AT ROW 2.62 COL 59 COLON-ALIGNED NO-LABEL WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     RowObject.Estado AT ROW 4.23 COL 7 NO-LABEL WIDGET-ID 30
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "En estudio", 1,
"Aprobada", 2,
"Negada", 3,
"Condicionada", 4
          SIZE 53 BY .81
     RowObject.Nit AT ROW 5.31 COL 9 COLON-ALIGNED WIDGET-ID 20
          LABEL "Cliente"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY 1
     RowObject.Nombre-2 AT ROW 6.38 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     RowObject.Apellido2 AT ROW 6.38 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 16.43 BY 1
     RowObject.Apellido1 AT ROW 6.38 COL 48 COLON-ALIGNED NO-LABEL WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 16.43 BY 1
     RowObject.Monto AT ROW 7.73 COL 9 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 18.57 BY 1
     RowObject.Nombre AT ROW 9.08 COL 9 COLON-ALIGNED WIDGET-ID 36
          LABEL "Usuario"
          VIEW-AS FILL-IN 
          SIZE 41.43 BY 1
     RowObject.Usuario AT ROW 9.08 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 8.43 BY 1
     RowObject.Fle AT ROW 10.96 COL 2 NO-LABEL WIDGET-ID 46 FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 65 BY 1
     RECT-316 AT ROW 1 COL 1 WIDGET-ID 28
     RECT-317 AT ROW 2.35 COL 2 WIDGET-ID 40
     RECT-318 AT ROW 5.04 COL 2 WIDGET-ID 42
     RECT-319 AT ROW 8.81 COL 2 WIDGET-ID 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "z:\prg\dsolicitud.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {z:/prg/dsolicitud.i}
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
         HEIGHT             = 10.96
         WIDTH              = 67.
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

/* SETTINGS FOR FILL-IN RowObject.Apellido1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Apellido2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_Solicitud IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fle IN FRAME F-Main
   NO-ENABLE ALIGN-L EXP-FORMAT                                         */
/* SETTINGS FOR FILL-IN RowObject.Nit IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Nombre IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Nombre-2 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Nombre-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Num_Solicitud IN FRAME F-Main
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

