&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE VAR W_Ubicacion AS CHAR FORMAT "X(8)".
DEFINE VAR W_Rowid     AS ROWID.
DEFINE VAR W_AuxUbica  AS DECIMAL FORMAT "99999999".
DEFINE VAR W_StrUbica  LIKE Ubicacion.Ubicacion.
DEFINE SHARED VAR W_Manija AS HANDLE.
DEFINE SHARED VAR W_Eleccion AS LOGICAL.

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
&Scoped-define EXTERNAL-TABLES Ubicacion
&Scoped-define FIRST-EXTERNAL-TABLE Ubicacion


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Ubicacion.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Ubicacion.Nombre 
&Scoped-define ENABLED-TABLES Ubicacion
&Scoped-define FIRST-ENABLED-TABLE Ubicacion
&Scoped-Define ENABLED-OBJECTS W_Estado 
&Scoped-Define DISPLAYED-FIELDS Ubicacion.Tipo Ubicacion.Nombre 
&Scoped-define DISPLAYED-TABLES Ubicacion
&Scoped-define FIRST-DISPLAYED-TABLE Ubicacion
&Scoped-Define DISPLAYED-OBJECTS W_Fdepartamento W_Fnomdpto W_Fciudad ~
W_Fnomciu W_FBarrio W_Fnombarrio W_Estado 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Ubicacion.Tipo W_Fdepartamento W_Fciudad ~
W_FBarrio 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE W_FBarrio AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Fciudad AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Fdepartamento AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Fnombarrio AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Fnomciu AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Fnomdpto AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Estado AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activo", 1,
"Inactivo", 2
     SIZE 21 BY .81
     BGCOLOR 17 FGCOLOR 0 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Ubicacion.Tipo AT ROW 1.27 COL 16 RIGHT-ALIGNED NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Departamento", "D":U,
"Ciudad", "C":U,
"Barrio", "B":U
          SIZE 14 BY 3.5
          BGCOLOR 17 
     W_Fdepartamento AT ROW 1.54 COL 16 COLON-ALIGNED HELP
          "Ingrese el C¢digo del Departamento para su Ubicaci¢n" NO-LABEL
     W_Fnomdpto AT ROW 1.54 COL 27 COLON-ALIGNED NO-LABEL
     W_Fciudad AT ROW 2.62 COL 16 COLON-ALIGNED HELP
          "Ingrese el C¢digo de la Ciudad para su Ubicaci¢n" NO-LABEL
     W_Fnomciu AT ROW 2.62 COL 27 COLON-ALIGNED NO-LABEL
     W_FBarrio AT ROW 3.69 COL 16 COLON-ALIGNED HELP
          "Ingrese el C¢digo del Barrio para su Ubicaci¢n" NO-LABEL
     W_Fnombarrio AT ROW 3.69 COL 27 COLON-ALIGNED NO-LABEL
     Ubicacion.Nombre AT ROW 5.04 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 45 BY .85
          BGCOLOR 15 
     W_Estado AT ROW 6.12 COL 18 HELP
          "Seleccione el Estado para la Ubicaci¢n" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Ubicacion
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
         HEIGHT             = 6.31
         WIDTH              = 64.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET Ubicacion.Tipo IN FRAME F-Main
   NO-ENABLE ALIGN-R 1                                                  */
/* SETTINGS FOR FILL-IN W_FBarrio IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Fciudad IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Fdepartamento IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Fnombarrio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Fnomciu IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Fnomdpto IN FRAME F-Main
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

&Scoped-define SELF-NAME Ubicacion.Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ubicacion.Tipo V-table-Win
ON MOUSE-SELECT-CLICK OF Ubicacion.Tipo IN FRAME F-Main /* Tipo */
DO:
  APPLY "VALUE-CHANGED" TO ubicacion.tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ubicacion.Tipo V-table-Win
ON VALUE-CHANGED OF Ubicacion.Tipo IN FRAME F-Main /* Tipo */
OR RETURN OF Ubicacion.Tipo DO:
  IF INPUT Ubicacion.Tipo = "C" THEN
    DO:
      ENABLE W_Fciudad WITH FRAME {&FRAME-NAME}.
      DISABLE W_Fbarrio WITH FRAME {&FRAME-NAME}.
    END.
  ELSE
    IF INPUT Ubicacion.Tipo = "D" THEN
      DO:
        DISABLE W_Fciudad WITH FRAME {&FRAME-NAME}.
        DISABLE W_Fbarrio WITH FRAME {&FRAME-NAME}.        
      END.
    ELSE
      IF INPUT Ubicacion.Tipo = "B" THEN
        DO:
          ENABLE W_Fdepartamento WITH FRAME {&FRAME-NAME}.
          ENABLE W_Fciudad WITH FRAME {&FRAME-NAME}.
          ENABLE W_Fbarrio WITH FRAME {&FRAME-NAME}.          
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Estado V-table-Win
ON MOUSE-SELECT-CLICK OF W_Estado IN FRAME F-Main
DO:
  APPLY "VALUE-CHANGED" TO W_Estado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Estado V-table-Win
ON VALUE-CHANGED OF W_Estado IN FRAME F-Main
OR RETURN OF W_Estado DO:
  W_Rowid = ROWID (Ubicacion).
  IF INPUT Ubicacion.Tipo = "C" THEN
    DO:
       W_ubicacion = W_Fdepartamento:SCREEN-VALUE IN FRAME {&FRAME-NAME} +
                     W_Fciudad:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "000".
       FIND FIRST Ubicacion WHERE Ubicacion.Ubicacion > W_Ubicacion AND 
                     Ubicacion.Estado = 1 NO-ERROR.
       IF AVAILABLE (Ubicacion) THEN
         DO:
           IF SUBSTRING(Ubicacion.Ubicacion,1,5) = SUBSTRING(W_Ubicacion,1,5) AND
                        Ubicacion.Estado = 1 THEN
             DO:
               FIND Ubicacion WHERE ROWID(Ubicacion) = W_Rowid NO-ERROR.
               RUN MostrarMensaje IN W_Manija (iNPUT 28, OUTPUT W_Eleccion).
               RETURN NO-APPLY.
             END.
          END.
       ELSE
         FIND Ubicacion WHERE ROWID(Ubicacion) = W_Rowid NO-ERROR.
    END.
   ELSE
     DO:
      IF INPUT Ubicacion.Tipo = "D" THEN
        DO:
          W_ubicacion = W_Fdepartamento:SCREEN-VALUE IN FRAME {&FRAME-NAME} +
                        "000" + "000".
          FIND FIRST Ubicacion WHERE Ubicacion.Ubicacion > W_Ubicacion AND 
                     Ubicacion.Estado = 1 NO-ERROR.
          IF AVAILABLE (Ubicacion) THEN
            DO:
              IF SUBSTRING(Ubicacion.Ubicacion,1,2) = SUBSTRING(W_Ubicacion,1,2) AND
                           Ubicacion.Estado = 1 THEN
                DO:
                  FIND Ubicacion WHERE ROWID(Ubicacion) = W_Rowid NO-ERROR.
                  RUN MostrarMensaje IN W_Manija (INPUT 29, OUTPUT W_Eleccion).
                  RETURN NO-APPLY.
                END.
             END.
          ELSE
            FIND Ubicacion WHERE ROWID(Ubicacion) = W_Rowid NO-ERROR.
        END.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FBarrio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FBarrio V-table-Win
ON LEAVE OF W_FBarrio IN FRAME F-Main
DO:
  {Incluido\BTCANCEL.I}
  W_Ubicacion = W_Fdepartamento:SCREEN-VALUE IN FRAME {&FRAME-NAME} +  
                W_Fciudad:SCREEN-VALUE IN FRAME {&FRAME-NAME} +
                W_Fbarrio:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  FIND FIRST Ubicacion WHERE Ubicacion.Ubicacion =  W_Ubicacion NO-LOCK NO-ERROR. 
  IF AVAILABLE (Ubicacion) THEN
    DO:
      DISPLAY Ubicacion.Nombre @ W_Fnombarrio WITH FRAME {&FRAME-NAME}.
      RUN MostrarMensaje IN W_Manija (iNPUT 30, OUTPUT W_Eleccion). 
      APPLY "ENTRY" TO W_FBarrio.
      RETURN NO-APPLY.
    END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Fciudad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Fciudad V-table-Win
ON LEAVE OF W_Fciudad IN FRAME F-Main
DO:
  {Incluido\BTCANCEL.I}  
  W_Ubicacion = W_Fdepartamento:SCREEN-VALUE IN FRAME {&FRAME-NAME} + 
                W_Fciudad:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "000".
  FIND FIRST Ubicacion WHERE Ubicacion.Ubicacion = W_Ubicacion NO-LOCK NO-ERROR.
  IF AVAILABLE (Ubicacion) THEN
     IF INPUT Ubicacion.Tipo = "B" THEN
        DISPLAY Ubicacion.Nombre @ W_Fnomciu WITH FRAME {&FRAME-NAME}.
     ELSE
       DO:
         RUN MostrarMensaje IN W_Manija (INPUT 30, OUTPUT W_Eleccion).
         APPLY "ENTRY" TO W_Fciudad.
         RETURN NO-APPLY.
       END.
  ELSE
    IF INPUT Ubicacion.Tipo = "B" AND NOT AVAILABLE (Ubicacion) THEN
     DO:
       RUN MostrarMensaje IN W_Manija (INPUT 31, OUTPUT W_Eleccion).
       APPLY "ENTRY" TO W_Fciudad.
       RETURN NO-APPLY.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Fdepartamento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Fdepartamento V-table-Win
ON LEAVE OF W_Fdepartamento IN FRAME F-Main
DO:
  {Incluido\BTCANCEL.I}
  FIND FIRST Ubicacion WHERE SUBSTRING(Ubicacion.Ubicacion,1,2) = W_Fdepartamento:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
  IF AVAILABLE (Ubicacion) THEN
     IF INPUT Ubicacion.Tipo = "C" OR INPUT Ubicacion.Tipo = "B" THEN
        DISPLAY Ubicacion.Nombre @ W_FNomdpto WITH FRAME {&FRAME-NAME}.
     ELSE
       DO:
         RUN MostrarMensaje IN W_Manija (iNPUT 30, OUTPUT W_Eleccion).
         APPLY "ENTRY" TO W_Fdepartamento.
         RETURN NO-APPLY.
       END.
  ELSE
    IF (INPUT Ubicacion.Tipo = "C" OR INPUT Ubicacion.Tipo = "B") AND
                                       NOT AVAILABLE (Ubicacion) THEN
     DO:
       RUN MostrarMensaje IN W_Manija (iNPUT 31, OUTPUT W_Eleccion).
       APPLY "ENTRY" TO W_Fdepartamento.
       RETURN NO-APPLY.
     END.
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
  {src/adm/template/row-list.i "Ubicacion"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Ubicacion"}

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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN Ubicacion.Tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "D".
  DISABLE W_Fciudad WITH FRAME {&FRAME-NAME}.
  DISABLE W_Fbarrio WITH FRAME {&FRAME-NAME}.
  ASSIGN W_Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Ubicacion.Estado = INTEGER(W_Estado:SCREEN-VALUE)
            W_Fdepartamento
            W_Fciudad
            W_Fbarrio.
    IF Ubicacion.Tipo = "D" THEN
       ASSIGN Ubicacion.Ubicacion = W_Fdepartamento:SCREEN-VALUE + "000000".
    ELSE
      IF Ubicacion.Tipo = "C" THEN
         ASSIGN Ubicacion.Ubicacion = W_Fdepartamento:SCREEN-VALUE +
                                      W_Fciudad:SCREEN-VALUE       + "000".
      ELSE
        ASSIGN Ubicacion.Ubicacion = W_Fdepartamento:SCREEN-VALUE +
                                     W_Fciudad:SCREEN-VALUE       +
                                     W_Fbarrio:SCREEN-VALUE.
    RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
    IF RETURN-VALUE = "YES" THEN
       RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Ubicacion. Ubicacion: " + STRING(Ubicacion.Ubicacion)).
    ELSE
       RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Ubicacion. Ubicacion: " + STRING(Ubicacion.Ubicacion)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
/*  DEFINE VAR W_Evento AS CHAR INITIAL " Salvar ".
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Ubicacion.Estado = INTEGER(W_Estado:SCREEN-VALUE)
            W_Fdepartamento
            W_Fciudad
            W_Fbarrio.
    IF Ubicacion.Tipo = "D" THEN
       ASSIGN Ubicacion.Ubicacion = W_Fdepartamento:SCREEN-VALUE + "000000".
    ELSE
      IF Ubicacion.Tipo = "C" THEN
         ASSIGN Ubicacion.Ubicacion = W_Fdepartamento:SCREEN-VALUE +
                                      W_Fciudad:SCREEN-VALUE       + "000".
      ELSE
        ASSIGN Ubicacion.Ubicacion = W_Fdepartamento:SCREEN-VALUE +
                                     W_Fciudad:SCREEN-VALUE       +
                                     W_Fbarrio:SCREEN-VALUE.
    RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
    IF RETURN-VALUE = "YES" THEN
       ASSIGN W_Evento = " Modificar ".
    RUN P-GraLog IN W_Manija (INPUT STRING(Ubicacion.Ubicacion) + W_Evento + " Mantenimiento de ubicaciones").
  END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/***********************************************************************************/
  IF W_Estado:SCREEN-VALUE IN FRAME F-Main EQ "2" THEN
    RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Ubicacion. Ubicacion: " + STRING(Ubicacion.Ubicacion)).
  ELSE DO:
    RUN MostrarMensaje IN W_Manija (INPUT 270, OUTPUT W_Eleccion).
    RETURN ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy V-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE (Ubicacion) THEN
    DO:
      W_Rowid = ROWID (Ubicacion).
      ASSIGN W_Fdepartamento = INTEGER(SUBSTRING(Ubicacion.Ubicacion,1,2)).
      ASSIGN W_Fciudad       = INTEGER(SUBSTRING(Ubicacion.Ubicacion,3,3)).
      ASSIGN W_Fbarrio       = INTEGER(SUBSTRING(Ubicacion.Ubicacion,6,3)).
      ASSIGN W_Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Ubicacion.Estado).
      IF INTEGER(SUBSTRING(Ubicacion.Ubicacion,6,3)) > 0 THEN
        DO:
          DISPLAY Ubicacion.Nombre @ W_Fnombarrio WITH FRAME {&FRAME-NAME}.   
          DISPLAY SUBSTRING(Ubicacion.Ubicacion,6,3) @ W_Fbarrio WITH FRAME {&FRAME-NAME}.
          ASSIGN W_Ubicacion = " ".
          W_Ubicacion = SUBSTRING(Ubicacion.Ubicacion,1,2) + 
                        SUBSTRING(Ubicacion.Ubicacion,3,3) + "000".
          FIND FIRST Ubicacion WHERE Ubicacion.Ubicacion = W_Ubicacion NO-LOCK NO-ERROR.
          IF AVAILABLE (Ubicacion) THEN
            DO:
              DISPLAY Ubicacion.Nombre @ W_Fnomciu WITH FRAME {&FRAME-NAME}.
              W_Ubicacion = SUBSTRING(Ubicacion.Ubicacion,1,2) + "000" + "000".
              DISPLAY INTEGER(SUBSTRING(Ubicacion.Ubicacion,3,3)) @ W_Fciudad WITH FRAME {&FRAME-NAME}.
              FIND FIRST Ubicacion WHERE Ubicacion.Ubicacion = W_Ubicacion NO-LOCK NO-ERROR.
              IF AVAILABLE (Ubicacion) THEN
                DO:
                  DISPLAY Ubicacion.Nombre @ W_Fnomdpto WITH FRAME {&FRAME-NAME}.
                  DISPLAY INTEGER(SUBSTRING(Ubicacion.Ubicacion,1,2)) @ W_Fdepartamento WITH FRAME {&FRAME-NAME}.
                END.
              FIND Ubicacion WHERE ROWID(Ubicacion) = W_Rowid NO-ERROR.
            END.
        END.
      ELSE
        DO:
          IF INTEGER(SUBSTRING(Ubicacion.Ubicacion,3,3)) > 0 THEN
            DO:
              DISPLAY Ubicacion.Nombre @ W_Fnomciu WITH FRAME {&FRAME-NAME}.   
              DISPLAY " "  @ W_Fnombarrio WITH FRAME {&FRAME-NAME}.
              DISPLAY " "  @ W_Fbarrio    WITH FRAME {&FRAME-NAME}.
              DISPLAY INTEGER(SUBSTRING(Ubicacion.Ubicacion,3,3)) @ W_Fciudad WITH FRAME {&FRAME-NAME}.
              ASSIGN W_Ubicacion = " ".
              W_Ubicacion = SUBSTRING(Ubicacion.Ubicacion,1,2) + "000" + "000".
              FIND FIRST Ubicacion WHERE Ubicacion.Ubicacion = W_Ubicacion NO-LOCK NO-ERROR.
              IF AVAILABLE (Ubicacion) THEN
                DO:
                  DISPLAY INTEGER(SUBSTRING(Ubicacion.Ubicacion,1,2)) @ W_Fdepartamento WITH FRAME {&FRAME-NAME}.
                  DISPLAY Ubicacion.Nombre @ W_Fnomdpto WITH FRAME {&FRAME-NAME}.
                  FIND Ubicacion WHERE ROWID(Ubicacion) = W_Rowid NO-ERROR.
                END.
            END.
          ELSE        
            DO:
              IF INTEGER(SUBSTRING(Ubicacion.Ubicacion,1,2)) > 0 THEN
                DO:
                  DISPLAY INTEGER(SUBSTRING(Ubicacion.Ubicacion,1,2)) @ W_Fdepartamento WITH FRAME {&FRAME-NAME}.
                  DISPLAY Ubicacion.Nombre @ W_Fnomdpto WITH FRAME {&FRAME-NAME}.   
                  DISPLAY " "  @ W_Fciudad WITH FRAME {&FRAME-NAME}.    
                  DISPLAY " "  @ W_Fnomciu WITH FRAME {&FRAME-NAME}.   
                  DISPLAY " "  @ W_Fbarrio WITH FRAME {&FRAME-NAME}.
                  DISPLAY " "  @ W_Fnombarrio WITH FRAME {&FRAME-NAME}.
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  {src/adm/template/snd-list.i "Ubicacion"}

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

