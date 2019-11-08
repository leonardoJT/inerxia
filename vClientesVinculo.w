&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dclientes.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dclientes.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Tipo_Vinculo RowObject.Fec_Ingreso 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-6 
&Scoped-Define DISPLAYED-FIELDS RowObject.Nit RowObject.Agencia ~
RowObject.Nombre RowObject.Apellido1 RowObject.Apellido2 ~
RowObject.Tipo_Vinculo RowObject.Fec_Ingreso RowObject.Fec_UltActualiza 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Titulo 
     LABEL "" 
     SIZE 41 BY 1.12.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 11.85.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BTN-Titulo AT ROW 1.27 COL 14.57 WIDGET-ID 24
     RowObject.Nit AT ROW 4.23 COL 21 COLON-ALIGNED HELP
          "Número documento de identificación" WIDGET-ID 12
          LABEL "Nit" FORMAT "X(12)"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Agencia AT ROW 5.23 COL 21 COLON-ALIGNED HELP
          "Código de agencia" WIDGET-ID 2
          LABEL "Agencia" FORMAT "999"
          VIEW-AS FILL-IN 
          SIZE 4.86 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Nombre AT ROW 6.23 COL 21 COLON-ALIGNED HELP
          "Nombre del cliente" WIDGET-ID 14
          LABEL "Nombre" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 41.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Apellido1 AT ROW 7.23 COL 21 COLON-ALIGNED HELP
          "Primer apellido del cliente" WIDGET-ID 4
          LABEL "Primer Apellido" FORMAT "X(15)"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Apellido2 AT ROW 8.23 COL 21 COLON-ALIGNED HELP
          "Segundo apellido del cliente" WIDGET-ID 6
          LABEL "Segundo Apellido" FORMAT "X(15)"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Tipo_Vinculo AT ROW 9.23 COL 23 HELP
          "Identifica el vínculo que tiene con la Organización" NO-LABEL WIDGET-ID 16
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Asociado", 1,
"Cliente No Asociado", 2,
"Tercero", 3,
"Proveedor", 4
          SIZE 22.14 BY 2.81
          BGCOLOR 15 
     RowObject.Fec_Ingreso AT ROW 12.31 COL 21 COLON-ALIGNED HELP
          "Fecha de ingreso del cliente a la Organización" WIDGET-ID 8
          LABEL "Fecha de Ingreso" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     RowObject.Fec_UltActualiza AT ROW 13.31 COL 21 COLON-ALIGNED HELP
          "Fecha de actualización de datos" WIDGET-ID 10
          LABEL "Fecha Ultima Actualización" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "CLIENTES" VIEW-AS TEXT
          SIZE 10.14 BY .5 AT ROW 1.54 COL 30 WIDGET-ID 30
          BGCOLOR 11 FONT 1
     "Tipo Vinculo:" VIEW-AS TEXT
          SIZE 9 BY .5 AT ROW 9.35 COL 22 RIGHT-ALIGNED WIDGET-ID 22
          FONT 4
     RECT-2 AT ROW 3.15 COL 1 WIDGET-ID 26
     RECT-6 AT ROW 1 COL 13.57 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dclientes.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dclientes.i}
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
         HEIGHT             = 14
         WIDTH              = 68.
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

/* SETTINGS FOR FILL-IN RowObject.Agencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Apellido1 IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Apellido2 IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR BUTTON BTN-Titulo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_Ingreso IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_UltActualiza IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Nit IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Nombre IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR RADIO-SET RowObject.Tipo_Vinculo IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TEXT-LITERAL "Tipo Vinculo:"
          SIZE 9 BY .5 AT ROW 9.35 COL 22 RIGHT-ALIGNED                 */

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

