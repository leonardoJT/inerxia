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

DEF VAR  v-Util-Neta  AS DEC NO-UNDO INITIAL 0.

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
&Scoped-define FRAME-NAME F_Severidad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Cod_Nivel RowObject.Nombre ~
RowObject.Util_Neta RowObject.Val_Inicial RowObject.Estado ~
RowObject.Val_Final RowObject.Descripcion 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.Cod_Nivel RowObject.Nombre ~
RowObject.Util_Neta RowObject.Val_Inicial RowObject.Estado ~
RowObject.Val_Final RowObject.Descripcion 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Severidad
     RowObject.Cod_Nivel AT ROW 1.81 COL 7.43 COLON-ALIGNED HELP
          "Código Nivel" WIDGET-ID 306
          LABEL "Nivel" FORMAT ">>9"
          VIEW-AS COMBO-BOX INNER-LINES 11
          LIST-ITEMS "0","1","2","3","4","5" 
          DROP-DOWN-LIST
          SIZE 6.57 BY 1
          BGCOLOR 15 
     RowObject.Nombre AT ROW 1.81 COL 30.43 COLON-ALIGNED HELP
          "Descripcion" WIDGET-ID 24
          LABEL "Descripcion" FORMAT "X(250)"
          VIEW-AS FILL-IN 
          SIZE 33 BY .92
          BGCOLOR 15 
     RowObject.Util_Neta AT ROW 3.42 COL 67 COLON-ALIGNED WIDGET-ID 324
          VIEW-AS FILL-IN 
          SIZE 7.43 BY 1
     RowObject.Val_Inicial AT ROW 4.35 COL 12.72 COLON-ALIGNED HELP
          "Valoracion Detallada" WIDGET-ID 32
          LABEL "Valor Inicial" FORMAT ">>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17.29 BY .92
          BGCOLOR 15 
     RowObject.Estado AT ROW 5.54 COL 51.86 HELP
          "Estado del registro de la cuenta" NO-LABEL WIDGET-ID 308
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 22 BY .81
     RowObject.Val_Final AT ROW 5.69 COL 12.72 COLON-ALIGNED HELP
          "" WIDGET-ID 30
          LABEL "Valor Final" FORMAT ">>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17.29 BY .92
          BGCOLOR 15 
     RowObject.Descripcion AT ROW 7.35 COL 24.72 HELP
          "Descripción" NO-LABEL WIDGET-ID 34
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 64.29 BY 3.46 TOOLTIP "Descripcion Detallada"
          BGCOLOR 15 FONT 5
     "Descripcion Detallada" VIEW-AS TEXT
          SIZE 19 BY .92 AT ROW 7.31 COL 4.29 WIDGET-ID 36
          FONT 5
     "Aprox." VIEW-AS TEXT
          SIZE 8 BY .92 AT ROW 3.42 COL 78 WIDGET-ID 322
          FONT 5
     "Millones" VIEW-AS TEXT
          SIZE 10 BY .92 AT ROW 5.65 COL 33 WIDGET-ID 320
          FONT 5
     "Millones" VIEW-AS TEXT
          SIZE 10 BY .92 AT ROW 4.35 COL 33 WIDGET-ID 318
          FONT 5
     "Rango Valoracion Detallada" VIEW-AS TEXT
          SIZE 27 BY .92 AT ROW 3.15 COL 4 WIDGET-ID 316
          FONT 5
     "Estado" VIEW-AS TEXT
          SIZE 19 BY .92 AT ROW 4.69 COL 51.57 WIDGET-ID 312
          FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 90.43 BY 10.35
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
         HEIGHT             = 10.35
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
/* SETTINGS FOR FRAME F_Severidad
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Severidad:HIDDEN           = TRUE
       FRAME F_Severidad:PRIVATE-DATA     = 
                "02 Información Cónyuge o Compañero (a) permanente".

/* SETTINGS FOR COMBO-BOX RowObject.Cod_Nivel IN FRAME F_Severidad
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR EDITOR RowObject.Descripcion IN FRAME F_Severidad
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F_Severidad
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.Nombre IN FRAME F_Severidad
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Val_Final IN FRAME F_Severidad
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Val_Inicial IN FRAME F_Severidad
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Severidad
/* Query rebuild information for FRAME F_Severidad
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F_Severidad */
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
  HIDE FRAME F_Severidad.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

