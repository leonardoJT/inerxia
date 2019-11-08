&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dcfg_Sarlaft.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dcfg_Sarlaft.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Probabilidad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Cod_Probabilidad ~
RowObject.Cod_Nivel RowObject.Nombre RowObject.Val_Inicial RowObject.Estado ~
RowObject.Val_Final RowObject.Descripcion 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.Cod_Probabilidad ~
RowObject.Cod_Nivel RowObject.Nombre RowObject.Val_Inicial RowObject.Estado ~
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

DEFINE FRAME F_Probabilidad
     RowObject.Cod_Probabilidad AT ROW 1 COL 22 COLON-ALIGNED WIDGET-ID 328
          VIEW-AS FILL-IN 
          SIZE 7.43 BY 1
     RowObject.Cod_Nivel AT ROW 1.81 COL 7.43 COLON-ALIGNED HELP
          "Código Nivel" WIDGET-ID 306
          LABEL "Nivel" FORMAT ">>9"
          VIEW-AS COMBO-BOX INNER-LINES 11
          LIST-ITEMS "1","2","3","4" 
          DROP-DOWN-LIST
          SIZE 6.57 BY 1
          BGCOLOR 15 
     RowObject.Nombre AT ROW 1.81 COL 30.43 COLON-ALIGNED HELP
          "Descripcion" WIDGET-ID 318
          LABEL "Descripcion" FORMAT "X(250)"
          VIEW-AS FILL-IN 
          SIZE 57.57 BY .92
          BGCOLOR 15 
     RowObject.Val_Inicial AT ROW 4.35 COL 12.72 COLON-ALIGNED HELP
          "Valoracion Detallada" WIDGET-ID 322
          LABEL "Valor Inicial" FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17.29 BY .92
          BGCOLOR 15 
     RowObject.Estado AT ROW 4.5 COL 68 HELP
          "Estado del registro de la cuenta" NO-LABEL WIDGET-ID 308
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 22 BY .81
     RowObject.Val_Final AT ROW 5.69 COL 12.72 COLON-ALIGNED HELP
          "" WIDGET-ID 320
          LABEL "Valor Final" FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17.29 BY .92
          BGCOLOR 15 
     RowObject.Descripcion AT ROW 8 COL 2 HELP
          "Descripción" NO-LABEL WIDGET-ID 34
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 88 BY 2.81 TOOLTIP "Descripcion Detallada"
          BGCOLOR 15 FONT 5
     "Estado" VIEW-AS TEXT
          SIZE 7 BY .92 AT ROW 4.5 COL 60 WIDGET-ID 312
          FONT 5
     "Descripcion Detallada" VIEW-AS TEXT
          SIZE 19 BY .92 AT ROW 6.92 COL 4 WIDGET-ID 36
          FONT 5
     "Veces al Año" VIEW-AS TEXT
          SIZE 13 BY .92 AT ROW 5.65 COL 33 WIDGET-ID 324
          FONT 5
     "Veces al Año" VIEW-AS TEXT
          SIZE 13 BY .92 AT ROW 4.35 COL 33 WIDGET-ID 326
          FONT 5
     "Rango Valoracion Detallada" VIEW-AS TEXT
          SIZE 27 BY .92 AT ROW 3.15 COL 4 WIDGET-ID 316
          FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 90.57 BY 10.15
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dcfg_Sarlaft.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dcfg_Sarlaft.i}
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
         HEIGHT             = 10.15
         WIDTH              = 90.57.
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
/* SETTINGS FOR FRAME F_Probabilidad
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Probabilidad:HIDDEN           = TRUE
       FRAME F_Probabilidad:PRIVATE-DATA     = 
                "02 Información Cónyuge o Compañero (a) permanente".

/* SETTINGS FOR COMBO-BOX RowObject.Cod_Nivel IN FRAME F_Probabilidad
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
ASSIGN 
       RowObject.Cod_Probabilidad:HIDDEN IN FRAME F_Probabilidad           = TRUE.

/* SETTINGS FOR EDITOR RowObject.Descripcion IN FRAME F_Probabilidad
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F_Probabilidad
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.Nombre IN FRAME F_Probabilidad
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Val_Final IN FRAME F_Probabilidad
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Val_Inicial IN FRAME F_Probabilidad
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Probabilidad
/* Query rebuild information for FRAME F_Probabilidad
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F_Probabilidad */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME RowObject.Cod_Nivel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Cod_Nivel vTableWin
ON VALUE-CHANGED OF RowObject.Cod_Nivel IN FRAME F_Probabilidad /* Nivel */
DO:
    DO WITH FRAME {&FRAME-NAME}:
         RowObject.Cod_probabilidad:SCREEN-VALUE = RowObject.Cod_Nivel:SCREEN-VALUE.
    END.
  
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
  HIDE FRAME F_Probabilidad.
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
  i = LOOKUP("cod_nivel",pcFieldList,",").
  IF NOT i = 0 
  THEN DO:
      IF trim(ENTRY(i,pcColValues,CHR(1))) = "0" 
      THEN entry(i,pcColValues,CHR(1)) = "1".
  END.

  RUN SUPER( INPUT pcFieldList, INPUT pcFromSource, INPUT phDataSource, INPUT pcColValues).
  /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        i = LOOKUP("cod_probabilidad",pcFieldList,",").
        IF NOT i = 0 
        THEN DO:
            IF trim(ENTRY(i,pcColValues,CHR(1))) = "0" 
            THEN RowObject.Cod_probabilidad:SCREEN-VALUE = "1".
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

