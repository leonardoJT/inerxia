&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
   DEFINE SHARED VAR W_Manija AS HANDLE.
   DEFINE        VAR W_Rpta   AS LOGICAL.
   DEFINE        VAR W_Rowid  AS ROWID.
   DEFINE        VAR W_Entero AS INTEGER FORMAT 999.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Entidad

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table Entidad.Entidad Entidad.Nit Entidad.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define OPEN-QUERY-br_table /*IF SELF:NAME = "F_Buscar" AND R_Buscar EQ 3 THEN   OPEN QUERY br_table FOR EACH Entidad WHERE Entidad.Nombre BEGINS F_Buscar NO-LOCK. ELSE   IF SELF:NAME = "W_Fnit" THEN     OPEN QUERY br_Table FOR EACH Entidad NO-LOCK.   ELSE     OPEN QUERY br_table FOR EACH Entidad NO-LOCK.*/ ASSIGN FRAME F-MAIN R_Buscar. CASE R_Buscar:  WHEN 1 THEN   OPEN QUERY br_table FOR EACH Entidad NO-LOCK.  WHEN 2 THEN   OPEN QUERY br_table FOR EACH Entidad NO-LOCK.  WHEN 3 THEN   OPEN QUERY br_table FOR EACH Entidad WHERE Entidad.Nombre BEGINS F_Buscar NO-LOCK. END CASE.
&Scoped-define TABLES-IN-QUERY-br_table Entidad
&Scoped-define FIRST-TABLE-IN-QUERY-br_table Entidad


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table R_Buscar F_Buscar RECT-130 
&Scoped-Define DISPLAYED-OBJECTS R_Buscar F_Buscar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE F_Buscar AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE R_Buscar AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Código Organización", 1,
"Nit", 2,
"Nombre", 3
     SIZE 38 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-130
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 1.88.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      Entidad SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      Entidad.Entidad
      Entidad.Nit
      Entidad.Nombre
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 64 BY 5.38
         BGCOLOR 15 FGCOLOR 7 FONT 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     R_Buscar AT ROW 7.46 COL 2 NO-LABEL
     F_Buscar AT ROW 7.46 COL 39 COLON-ALIGNED NO-LABEL
     RECT-130 AT ROW 6.92 COL 1
     "  Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 29 BY 1.08 AT ROW 6.38 COL 4
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FGCOLOR 0 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 8.15
         WIDTH              = 64.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
/*IF SELF:NAME = "F_Buscar" AND R_Buscar EQ 3 THEN
  OPEN QUERY br_table FOR EACH Entidad WHERE Entidad.Nombre BEGINS F_Buscar NO-LOCK.
ELSE
  IF SELF:NAME = "W_Fnit" THEN
    OPEN QUERY br_Table FOR EACH Entidad NO-LOCK.
  ELSE
    OPEN QUERY br_table FOR EACH Entidad NO-LOCK.*/
ASSIGN FRAME F-MAIN R_Buscar.
CASE R_Buscar:
 WHEN 1 THEN
  OPEN QUERY br_table FOR EACH Entidad NO-LOCK.
 WHEN 2 THEN
  OPEN QUERY br_table FOR EACH Entidad NO-LOCK.
 WHEN 3 THEN
  OPEN QUERY br_table FOR EACH Entidad WHERE Entidad.Nombre BEGINS F_Buscar NO-LOCK.
END CASE.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Buscar B-table-Win
ON LEAVE OF F_Buscar IN FRAME F-Main
OR RETURN OF F_Buscar DO:
  ASSIGN FRAME F-MAIN R_Buscar F_Buscar. 
  CASE R_Buscar:
   WHEN 1 THEN  DO:
       IF INTEGER(SUBSTRING(F_Buscar,1,1)) GE 0 OR INTEGER(SUBSTRING(F_Buscar,1,1)) LE 9 THEN
          RUN Buscar_Codigo.
       ELSE DO:
          MESSAGE "Debe entrar caracteres númericos" VIEW-AS ALERT-BOX TITLE "Código".
       END.

   END.
   WHEN 2 THEN
     RUN Buscar_Nit.
   WHEN 3 THEN
     RUN Buscar_Nombre.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Codigo B-table-Win 
PROCEDURE Buscar_Codigo :
IF LASTKEY = KEYCODE("ENTER") THEN
   DO:
     ASSIGN W_Entero = INTEGER(F_Buscar)
            W_Rowid = ROWID(Entidad).
     FIND Entidad WHERE Entidad.Entidad = W_Entero NO-LOCK NO-ERROR.
     IF AVAILABLE (Entidad) THEN
      DO:
        REPOSITION Br_Table TO ROWID ROWID(Entidad).
        {incluido/brschnge.i}.
      END.
     ELSE
      DO:
        RUN MostrarMensaje IN W_Manija (INPUT 64,OUTPUT W_Rpta).
        FIND Entidad WHERE ROWID(Entidad) = W_Rowid NO-LOCK NO-ERROR.        
      END.
     APPLY "ENTRY" TO F_Buscar IN FRAME F-MAIN.
     RETURN NO-APPLY.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Nit B-table-Win 
PROCEDURE Buscar_Nit :
IF LASTKEY = KEYCODE("ENTER") THEN
   DO:
     ASSIGN W_Rowid = ROWID(Entidad).
     FIND Entidad WHERE Entidad.Nit BEGINS F_Buscar NO-LOCK NO-ERROR.
     IF AVAILABLE (Entidad) THEN
      DO:
        REPOSITION Br_Table TO ROWID ROWID(Entidad).
        {incluido/brschnge.i}.
      END.
     ELSE
      DO:
        RUN MostrarMensaje IN W_Manija (INPUT 64,OUTPUT W_Rpta).
        FIND Entidad WHERE ROWID(Entidad) = W_Rowid NO-LOCK NO-ERROR.        
      END.
     APPLY "ENTRY" TO F_Buscar IN FRAME F-MAIN.
     RETURN NO-APPLY.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Nombre B-table-Win 
PROCEDURE Buscar_Nombre :
IF LASTKEY = KEYCODE("ENTER") THEN
   DO:
     ASSIGN W_Rowid = ROWID(Entidad).
     FIND FIRST Entidad WHERE Entidad.Nombre BEGINS F_Buscar NO-LOCK NO-ERROR.
     IF AVAILABLE (Entidad) THEN
      DO:
        REPOSITION Br_Table TO ROWID ROWID(Entidad).
        {incluido/brschnge.i}.
      END.
     ELSE
      DO:
        RUN MostrarMensaje IN W_Manija (INPUT 64,OUTPUT W_Rpta).
        FIND Entidad WHERE ROWID(Entidad) = W_Rowid NO-LOCK NO-ERROR.        
      END.
     APPLY "ENTRY" TO F_Buscar IN FRAME F-MAIN.
     RETURN NO-APPLY.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Entidad"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

