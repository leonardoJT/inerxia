&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"d-cf_ciiu.i"}.


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

  
{INCLUIDO\Variable.i "SHARED"}.
DEFINE VAR W_Nuevo AS LOGICAL INITIAL NO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "d-cf_ciiu.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Tipo RowObject.Grupo ~
RowObject.Descripcion 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.Tipo RowObject.Grupo ~
RowObject.Subgrupo RowObject.Codigo_CIIU RowObject.Descripcion 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Tipo AT ROW 1 COL 3 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Grupo", 1,
"Subrupo", 2,
"Código", 3
          SIZE 11 BY 2.96
          FONT 4
     RowObject.Grupo AT ROW 1 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY .85
          BGCOLOR 15 
     RowObject.Subgrupo AT ROW 2.08 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY .85
          BGCOLOR 15 
     RowObject.Codigo_CIIU AT ROW 3.15 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY .85
          BGCOLOR 15 
     RowObject.Descripcion AT ROW 4.23 COL 2 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 79 BY 1.62
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "d-cf_ciiu.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {d-cf_ciiu.i}
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
         HEIGHT             = 5.08
         WIDTH              = 80.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Codigo_CIIU IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Subgrupo IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME RowObject.Codigo_CIIU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Codigo_CIIU vTableWin
ON LEAVE OF RowObject.Codigo_CIIU IN FRAME F-Main /* Codigo_CIIU */
DO:
  IF RowObject.Grupo:SCREEN-VALUE IN FRAME F-Main EQ "00" THEN
  DO:
        MESSAGE "No se puede crear un Codigo sin antes" SKIP
                "digitar el grupo al que pertenece..."
                VIEW-AS ALERT-BOX WARNING TITLE "Falta información".
        APPLY 'entry' TO RowObject.Grupo IN FRAME F-Main.
        RETURN NO-APPLY.
  END.
  IF RowObject.SubGrupo:SCREEN-VALUE IN FRAME F-Main EQ "00" AND
     RowObject.Tipo:SCREEN-VALUE EQ "3" THEN
  DO:
        MESSAGE "No se puede crear un Código sin antes" SKIP
                "digitar el Subgrupo al que pertenece..."
                VIEW-AS ALERT-BOX WARNING TITLE "Falta información".
        APPLY 'entry' TO RowObject.SubGrupo IN FRAME F-Main.
        RETURN NO-APPLY.
  END.
    
  FIND Ciiu WHERE Ciiu.Tipo     EQ 2                        AND
                  Ciiu.Grupo    EQ INTEGER(RowObject.Grupo:SCREEN-VALUE IN FRAME F-Main) AND
                  Ciiu.SubGrupo EQ INTEGER(RowObject.SubGrupo:SCREEN-VALUE IN FRAME F-Main) AND
                  Ciiu.Codigo   EQ 0000 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Ciiu) THEN
  DO:
          MESSAGE "El Grupo o Subgrupo digitado no existe!" SKIP
                  "debe crear el Grupo o Subgrupo antes de" SKIP
                  "matricular algún Código" 
                  VIEW-AS ALERT-BOX WARNING TITLE "Falta Crear Información".
          ASSIGN RowObject.Grupo:SCREEN-VALUE    IN FRAME F-Main = "00"
                 RowObject.Subgrupo:SCREEN-VALUE IN FRAME F-Main = "00"
                 RowObject.Codigo:SCREEN-VALUE   IN FRAME F-Main = "0000".
          APPLY 'entry' TO RowObject.Grupo IN FRAME F-Main.
          RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Grupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Grupo vTableWin
ON LEAVE OF RowObject.Grupo IN FRAME F-Main /* Grupo */
DO:
    FIND Ciiu WHERE Ciiu.Tipo EQ 1 AND
                    Ciiu.Grupo EQ INTEGER(RowObject.Grupo:SCREEN-VALUE IN FRAME F-Main) AND
                    Ciiu.Subgrupo EQ 0 AND Ciiu.Codigo EQ 0
                    NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Ciiu) THEN
    DO:
       IF  RowObject.Tipo:SCREEN-VALUE IN FRAME F-Main NE "1" THEN
       DO:
            MESSAGE "El grupo digitado no existe!" SKIP
                    "debe crear el grupo antes de" SKIP
                    "matricular algun subgrupo en este" 
                    VIEW-AS ALERT-BOX WARNING TITLE "Falta Crear Información".
            ASSIGN RowObject.Subgrupo:SCREEN-VALUE IN FRAME F-Main = "00"
                   RowObject.Codigo:SCREEN-VALUE IN FRAME F-Main = "0000".
            APPLY 'entry' TO RowObject.Grupo IN FRAME F-Main.
            RETURN NO-APPLY.
       END.
       
       APPLY 'entry' TO RowObject.Descripcion IN FRAME F-Main.
       RETURN NO-APPLY.
    END.
    ELSE
    DO:
       IF RowObject.Tipo:SCREEN-VALUE IN FRAME F-Main EQ "1" THEN
            MESSAGE "El Grupo ya Existe"
                     VIEW-AS ALERT-BOX INFORMATION TITLE "Creación No Valida".
    END.
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Subgrupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Subgrupo vTableWin
ON LEAVE OF RowObject.Subgrupo IN FRAME F-Main /* Subgrupo */
DO:
  IF RowObject.Grupo:SCREEN-VALUE IN FRAME F-Main EQ "00" THEN
  DO:
        MESSAGE "No se puede crear un subgrupo sin antes" SKIP
                "digitar el grupo al que pertenece..."
                VIEW-AS ALERT-BOX WARNING TITLE "Falta información".
        APPLY 'entry' TO RowObject.Grupo IN FRAME F-Main.
        RETURN NO-APPLY.
  END.
  ELSE
  DO:
        FIND Ciiu WHERE Ciiu.Tipo EQ 1 AND
                        Ciiu.Grupo EQ INTEGER(RowObject.Grupo:SCREEN-VALUE IN FRAME F-Main) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Ciiu) THEN
        DO:
                MESSAGE "El grupo digitado no existe!" SKIP(2)
                        "Debe crear el grupo antes de" SKIP
                        "matricular algun subgrupo en este" 
                        VIEW-AS ALERT-BOX WARNING TITLE "Falta Crear Información".
                ASSIGN RowObject.Subgrupo:SCREEN-VALUE IN FRAME F-Main = "00"
                       RowObject.Codigo:SCREEN-VALUE IN FRAME F-Main = "0000".
                APPLY 'entry' TO RowObject.Grupo IN FRAME F-Main.
                RETURN NO-APPLY.
        END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Tipo vTableWin
ON VALUE-CHANGED OF RowObject.Tipo IN FRAME F-Main
DO:
  CASE RowObject.Tipo:SCREEN-VALUE IN FRAME F-Main:
      WHEN "1" THEN
      DO:
            ASSIGN RowObject.Subgrupo:SCREEN-VALUE IN FRAME F-Main = "00"
                   RowObject.Codigo:SCREEN-VALUE IN FRAME F-Main = "0000".
            DISABLE RowObject.Codigo RowObject.Subgrupo WITH FRAME F-Main.
            ENABLE RowObject.Grupo WITH FRAME F-Main.
      END.
      WHEN "2" THEN
      DO:
            ASSIGN RowObject.Codigo:SCREEN-VALUE IN FRAME F-Main = "0000".
            DISABLE RowObject.Codigo WITH FRAME F-Main.
            ENABLE RowObject.Grupo RowObject.Subgrupo WITH FRAME F-Main.
      END.
      WHEN "3" THEN
      DO:
            ENABLE RowObject.Grupo RowObject.Subgrupo RowObject.Codigo WITH FRAME F-Main.
      END.
  END CASE.
  APPLY 'entry' TO RowObject.Grupo IN FRAME F-Main.
  RETURN NO-APPLY.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
RUN SUPER.
  DISABLE RowObject.Subgrupo RowObject.Codigo WITH FRAME F-Main.
  ENABLE RowObject.Grupo WITH FRAME F-Main.
  W_Nuevo = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord vTableWin 
PROCEDURE deleteRecord :
MESSAGE "No se pueden borrar Registros de Esta Tabla" VIEW-AS ALERT-BOX.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields vTableWin 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).


  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
RUN SUPER.
  IF W_Nuevo THEN 
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, CIIU. Codigo: " + 
         STRING(RowObject.Grupo:SCREEN-VALUE IN FRAME F-Main) + 
         STRING(RowObject.SubGrupo:SCREEN-VALUE IN FRAME F-Main) +
         STRING(RowObject.Codigo_CIIU:SCREEN-VALUE IN FRAME F-Main)).
  ELSE
     RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, CIIU. Codigo: " + 
         STRING(RowObject.Grupo:SCREEN-VALUE IN FRAME F-Main) + 
         STRING(RowObject.SubGrupo:SCREEN-VALUE IN FRAME F-Main) +
         STRING(RowObject.Codigo_CIIU:SCREEN-VALUE IN FRAME F-Main)).
  W_Nuevo = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

