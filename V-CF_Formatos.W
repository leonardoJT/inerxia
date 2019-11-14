&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{incluido\variable.i "shared"}
DEFINE VAR W_Ofitbajo  LIKE Agencias.Agencia.
DEFINE VAR W_OfStr     AS   CHARACTER FORMAT "X(20)".
DEFINE VAR W_SuperUsu  AS   LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES Formatos
&Scoped-define FIRST-EXTERNAL-TABLE Formatos


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Formatos.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Formatos.Id_Formato Formatos.Nom_Formato ~
Formatos.Estado Formatos.Nom_Proceso 
&Scoped-define ENABLED-TABLES Formatos
&Scoped-define FIRST-ENABLED-TABLE Formatos
&Scoped-Define ENABLED-OBJECTS W_CmbOfi 
&Scoped-Define DISPLAYED-FIELDS Formatos.Id_Formato Formatos.Nom_Formato ~
Formatos.Estado Formatos.Nom_Proceso 
&Scoped-define DISPLAYED-TABLES Formatos
&Scoped-define FIRST-DISPLAYED-TABLE Formatos
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_Agenciap W_Cod 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS W_Cod 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(20)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Agenciap AS CHARACTER FORMAT "X(20)" INITIAL "0" 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE W_Cod AS INTEGER FORMAT "99" INITIAL 0 
     LABEL "Código" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_CmbOfi AT ROW 2.08 COL 9 COLON-ALIGNED
     W_Agenciap AT ROW 2.08 COL 9 COLON-ALIGNED HELP
          "Ingrese la Agencia a la que pertenece el Diferido"
     Formatos.Id_Formato AT ROW 3.15 COL 11 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Comprobante", "CB":U,
"Archivo", "AC":U,
"PreImpreso", "PI":U
          SIZE 36 BY .81
          BGCOLOR 17 
     W_Cod AT ROW 4.5 COL 9 COLON-ALIGNED
     Formatos.Nom_Formato AT ROW 5.58 COL 9 COLON-ALIGNED
          LABEL "Nombre"
          VIEW-AS FILL-IN 
          SIZE 36 BY .81
          BGCOLOR 15 
     Formatos.Estado AT ROW 6.65 COL 11 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 21 BY .81
          BGCOLOR 17 
     Formatos.Nom_Proceso AT ROW 7.73 COL 9 COLON-ALIGNED
          LABEL "Proceso" FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 36 BY .81
          BGCOLOR 15 
     "Información General" VIEW-AS TEXT
          SIZE 45 BY .62 AT ROW 1.27 COL 2
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Formatos
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
         HEIGHT             = 8.65
         WIDTH              = 47.72.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Formatos.Nom_Formato IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Formatos.Nom_Proceso IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN W_Agenciap IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Cod IN FRAME F-Main
   NO-ENABLE 1                                                          */
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

&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi V-table-Win
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F-Main /* Agencia */
DO:
    W_OfiTbajo = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,3)) no-error.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cod V-table-Win
ON LEAVE OF W_Cod IN FRAME F-Main /* Código */
DO:
  ASSIGN W_Cod.
  FIND Formatos WHERE Formatos.Agencia     EQ W_OfiTbajo 
                  AND Formatos.Cod_Formato EQ W_Cod 
                  NO-LOCK NO-ERROR.               
  IF AVAILABLE (Formatos) THEN
    DO:
      MESSAGE "El Código que desea crear ya se encuentra matriculado. Verifique"
               VIEW-AS ALERT-BOX.
      APPLY "Entry" TO W_Cod IN FRAME {&frame-name}.               
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
  {src/adm/template/row-list.i "Formatos"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Formatos"}

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
 DO WITH FRAME {&FRAME-NAME}: 
    IF W_SuperUsu THEN DO:
      ASSIGN W_AgenciaP:HIDDEN = TRUE
             w_Cmbofi:HIDDEN   = FALSE.
      ENABLE W_cmbofi.
      APPLY "ENTRY" TO W_CmbOfi.
    END.
    ELSE
       ASSIGN W_Cmbofi:HIDDEN   IN FRAME F-Main = TRUE
              W_AgenciaP:HIDDEN IN FRAME F-Main = FALSE.
  W_Cod = 0.
  DISPLAY W_Cod WITH FRAME {&FRAME-NAME}.
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  ASSIGN Formatos.Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING (1).
 END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  ASSIGN W_Cmbofi:HIDDEN   IN FRAME F-Main = TRUE
         W_AgenciaP:HIDDEN IN FRAME F-Main = FALSE
         Formatos.Agencia = W_OfiTbajo
         Formatos.Cod_Formato = W_Cod.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_Cmbofi:HIDDEN   IN FRAME {&FRAME-NAME} = TRUE
         W_AgenciaP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF Formatos.Cod_Formato > 0 THEN
    DO:    
      RUN MostrarMensaje IN W_Manija (INPUT 25, OUTPUT W_Eleccion).
      CASE W_Eleccion:
        WHEN TRUE THEN
          DO:
           FIND CURRENT Formatos SHARE-LOCK NO-ERROR NO-WAIT.
           IF AVAILABLE(Formatos) THEN
            DO:    
             IF W_Eleccion THEN
              DO:
               ASSIGN Formatos.Estado     = 2.
               ASSIGN Formatos.Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Formatos.Estado).
               RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Formatos. Cod_Formato: " + STRING(Formatos.Cod_Formato)).
              END.  
              FIND CURRENT Formatos NO-LOCK NO-ERROR.
            END.
          END.
        WHEN FALSE THEN
             RETURN ERROR.
      END CASE.
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
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ) .
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  FIND Agencias WHERE Agencias.Agencia EQ Formato.Agencia 
                NO-LOCK NO-ERROR.
  IF AVAILABLE (Agencias) THEN DO:
     ASSIGN W_AgenciaP:SCREEN-VALUE IN FRAME {&frame-name} = STRING(Formato.Agencia) + "-" +
                                          Agencia.Nombre
            W_Cod = Formatos.Cod_Formato.    
     DISPLAY W_Cod WITH FRAME {&frame-name}.
     ASSIGN W_OfiTbajo =  Agencias.Agencia.                 
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  {incluido\CmbOfi.i}
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN W_Cmbofi:HIDDEN   IN FRAME {&FRAME-NAME} = TRUE
           W_AgenciaP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
           W_OfiTbajo = W_Agencia.
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
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

  /* Code placed here will execute PRIOR to standard behavior. */
 DEFINE VAR W_Evento AS CHAR INITIAL "SA". 
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Formatos. Cod_Formato: " + STRING(Formatos.Cod_Formato)).
  ELSE
     RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Formatos. Cod_Formato: " + STRING(Formatos.Cod_Formato)).

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
  {src/adm/template/snd-list.i "Formatos"}

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

