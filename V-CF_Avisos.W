&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

   {Incluido\Variable.i "SHARED"}

DEFINE VAR InActiva AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES Men_Sistema
&Scoped-define FIRST-EXTERNAL-TABLE Men_Sistema


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Men_Sistema.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Men_Sistema.Titulo Men_Sistema.Estado ~
Men_Sistema.Tipo Men_Sistema.Mensaje Men_Sistema.Clase 
&Scoped-define ENABLED-TABLES Men_Sistema
&Scoped-define FIRST-ENABLED-TABLE Men_Sistema
&Scoped-Define ENABLED-OBJECTS IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 RECT-148 ~
RECT-19 RECT-20 
&Scoped-Define DISPLAYED-FIELDS Men_Sistema.Codigo Men_Sistema.Titulo ~
Men_Sistema.Estado Men_Sistema.Tipo Men_Sistema.Mensaje Men_Sistema.Clase 
&Scoped-define DISPLAYED-TABLES Men_Sistema
&Scoped-define FIRST-DISPLAYED-TABLE Men_Sistema


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE IMAGE IMAGE-5
     FILENAME "imagenes\error":U
     SIZE 5 BY 1.35.

DEFINE IMAGE IMAGE-6
     FILENAME "imagenes\info":U
     SIZE 5 BY 1.35.

DEFINE IMAGE IMAGE-7
     FILENAME "imagenes\warning1":U
     SIZE 5 BY 1.35.

DEFINE IMAGE IMAGE-8
     FILENAME "imagenes\interr":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE RECT-148
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27 BY 1.35.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27 BY 1.35.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 21 BY 10.23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Men_Sistema.Codigo AT ROW 1.81 COL 7 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Men_Sistema.Titulo AT ROW 2.88 COL 7 COLON-ALIGNED
          LABEL "Título"
          VIEW-AS FILL-IN 
          SIZE 27 BY .81
          BGCOLOR 15 
     Men_Sistema.Estado AT ROW 4.23 COL 11 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 24 BY .81
          BGCOLOR 17 
     Men_Sistema.Tipo AT ROW 6.65 COL 11 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Pantalla", 1,
"Archivo", 2
          SIZE 23 BY .54
          BGCOLOR 17 
     Men_Sistema.Mensaje AT ROW 12.85 COL 8 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 54 BY 2.42
          BGCOLOR 15 
     Men_Sistema.Clase AT ROW 2.62 COL 42 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Mensaje", 1,
"Pregunta", 2,
"Información", 3,
"Error", 4,
"Warning", 5
          SIZE 11 BY 9.15
          BGCOLOR 17 
     IMAGE-5 AT ROW 8.54 COL 56
     IMAGE-6 AT ROW 6.65 COL 56
     IMAGE-7 AT ROW 10.42 COL 56
     IMAGE-8 AT ROW 4.77 COL 56
     RECT-148 AT ROW 3.96 COL 9
     RECT-19 AT ROW 6.12 COL 9
     RECT-20 AT ROW 2.08 COL 41
     "  Iconos Disponibles" VIEW-AS TEXT
          SIZE 18 BY 1.08 AT ROW 1.54 COL 42
          FGCOLOR 7 FONT 5
     "  Objetivo del Mensaje" VIEW-AS TEXT
          SIZE 20 BY 1.08 AT ROW 5.58 COL 11
          FGCOLOR 7 FONT 5
     "Contenido del Mensaje" VIEW-AS TEXT
          SIZE 25 BY 1.08 AT ROW 11.77 COL 8
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Men_Sistema
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 14.85
         WIDTH              = 62.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Men_Sistema.Codigo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Men_Sistema.Titulo IN FRAME F-Main
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

&Scoped-define SELF-NAME Men_Sistema.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Men_Sistema.Estado V-table-Win
ON VALUE-CHANGED OF Men_Sistema.Estado IN FRAME F-Main /* Estado */
DO:
  IF SELF:SCREEN-VALUE EQ "2" THEN
     InActiva = YES.
  ELSE Inactiva = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "Men_Sistema"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Men_Sistema"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  DISPLAY CURRENT-VALUE(Sec_Mensajes) + 1 @ Men_Sistema.Codigo WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR W_Rpta AS LOGICAL.
  
  RUN MostrarMensaje IN W_Manija (INPUT 270,OUTPUT W_Rpta).
  RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy V-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ) .
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ) .
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR W_Evento AS CHAR INITIAL "SALVAR".
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Mensajes. Codigo: " + STRING(Men_Sistema.Codigo)).
  ELSE DO:
    IF Inactiva THEN DO:
       RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Mensajes. Codigo: " + STRING(Men_Sistema.Codigo)).
       Inactiva = NO.
    END.
    ELSE
      RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Mensajes. Codigo: " + STRING(Men_Sistema.Codigo)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Men_Sistema"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

