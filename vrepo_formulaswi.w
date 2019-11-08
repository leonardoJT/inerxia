&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          repositorio      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"drepo_formulas.i"}.



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
&Scoped-define DATA-FIELD-DEFS "drepo_formulas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.consecutivo RowObject.vigente ~
RowObject.NombreModelo RowObject.formula RowObject.procedimiento ~
RowObject.formulaProcedimiento RowObject.descripcion 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS BUTTON-frmlcion RECT-270 
&Scoped-Define DISPLAYED-FIELDS RowObject.consecutivo RowObject.vigente ~
RowObject.NombreModelo RowObject.formula RowObject.procedimiento ~
RowObject.formulaProcedimiento RowObject.descripcion 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fpropath vTableWin 
FUNCTION fpropath RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-frmlcion  NO-FOCUS FLAT-BUTTON
     LABEL "Formulación:" 
     SIZE 12.29 BY .62 TOOLTIP "Leer Programa Fuente"
     FONT 5.

DEFINE RECTANGLE RECT-270
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 13.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-frmlcion AT ROW 4.77 COL 3 WIDGET-ID 28
     RowObject.consecutivo AT ROW 1.27 COL 17 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 8.29 BY 1
     RowObject.vigente AT ROW 1.27 COL 84.43 WIDGET-ID 24
          LABEL "Vigente"
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .77
     RowObject.NombreModelo AT ROW 2.27 COL 17 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 21.43 BY 1
     RowObject.formula AT ROW 2.27 COL 50.14 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 21.86 BY 1
     RowObject.procedimiento AT ROW 3.42 COL 17 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 55 BY 1
     RowObject.formulaProcedimiento AT ROW 5.58 COL 2 NO-LABEL WIDGET-ID 16
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
          SIZE 94 BY 4.58
     RowObject.descripcion AT ROW 10.96 COL 2 NO-LABEL WIDGET-ID 20
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
          SIZE 94 BY 3.12
     "Descripción:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 10.15 COL 3 WIDGET-ID 22
     RECT-270 AT ROW 1 COL 1 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "drepo_formulas.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {drepo_formulas.i}
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
         HEIGHT             = 13.19
         WIDTH              = 96.
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

/* SETTINGS FOR EDITOR RowObject.descripcion IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.descripcion:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR EDITOR RowObject.formulaProcedimiento IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.formulaProcedimiento:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.vigente IN FRAME F-Main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BUTTON-frmlcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-frmlcion vTableWin
ON CHOOSE OF BUTTON-frmlcion IN FRAME F-Main /* Formulación: */
DO:
    DEF VAR l AS LOGICAL NO-UNDO.
    DEF VAR cfle AS CHAR NO-UNDO.
    l = FALSE.
    MESSAGE "Confirme Por Favor" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "Pregunta" UPDATE l.
    IF NOT l THEN RETURN NO-APPLY.
    DO WITH FRAME {&FRAME-NAME}:
        cfle = RowObject.procedimiento:SCREEN-VALUE.
        IF search(cfle) = ?
        THEN do:
            MESSAGE "'" + cfle + "' NO Existe En El PROPATH:" + CHR(10) + fpropath()  VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
        RowObject.formulaProcedimiento:READ-FILE(cfle) NO-ERROR.
        APPLY "value-changed" TO RowObject.formulaProcedimiento.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.consecutivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.consecutivo vTableWin
ON ANY-PRINTABLE OF RowObject.consecutivo IN FRAME F-Main /* Consecutivo */
DO:
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.consecutivo vTableWin
ON ENTRY OF RowObject.consecutivo IN FRAME F-Main /* Consecutivo */
DO:
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.procedimiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.procedimiento vTableWin
ON ENTRY OF RowObject.procedimiento IN FRAME F-Main /* Procedimiento */
DO:
    DEF VAR cFle AS CHAR NO-UNDO.
    DEF VAR l AS LOGICAL NO-UNDO.
    SYSTEM-DIALOG GET-FILE cfle
        FILTERS "*.p" "*.p",
        ".w" "*.w"
        MUST-EXIST 
        TITLE "Procedimientos Disponibles"
        UPDATE l.
    IF l 
    THEN do:
        SELF:SCREEN-VALUE = cFle.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.procedimiento vTableWin
ON HELP OF RowObject.procedimiento IN FRAME F-Main /* Procedimiento */
DO:
    APPLY 'entry' TO SELF.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.procedimiento vTableWin
ON LEAVE OF RowObject.procedimiento IN FRAME F-Main /* Procedimiento */
DO:
    DEF VAR cfle AS CHAR NO-UNDO.

    cfle = SELF:SCREEN-VALUE.
    cfle = ENTRY(NUM-ENTRIES(cfle,"\"),cfle,"\").
    IF NOT cfle MATCHES "*repo*" 
    THEN DO:
        MESSAGE "El Nombre Del Procedimiento Debe Contener La Palabra 'repo'"
            VIEW-AS ALERT-BOX ERROR TITLE "Error".
        RETURN NO-APPLY.
    END.
    IF SEARCH(ENTRY(1,cfle,".") + ".r") = ? AND SEARCH(cfle) = ?
    THEN MESSAGE "Precaución: El Programa '" + cfle + "' NO Se Encuentra En El PROPATH:" SKIP
        fpropath() VIEW-AS ALERT-BOX WARNING TITLE "Precausión:"  .
    SELF:SCREEN-VALUE = cfle.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.procedimiento vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.procedimiento IN FRAME F-Main /* Procedimiento */
DO:
    APPLY 'entry' TO SELF.  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableFields vTableWin 
PROCEDURE disableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcFieldType AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcFieldType).

  /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        BUTTON-frmlcion:SENSITIVE = FALSE.
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields vTableWin 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        BUTTON-frmlcion:SENSITIVE = TRUE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject1 vTableWin 
PROCEDURE initializeObject1 :
DO WITH FRAME {&FRAME-NAME}:
        BUTTON-frmlcion:SENSITIVE = FALSE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fpropath vTableWin 
FUNCTION fpropath RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

