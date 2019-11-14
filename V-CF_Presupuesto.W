&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

 {Incluido\Variable.i "Shared"}.

 DEFINE SHARED VARIABLE Fcodigo LIKE cuentas.cuenta.

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
&Scoped-define EXTERNAL-TABLES Presupuesto
&Scoped-define FIRST-EXTERNAL-TABLE Presupuesto


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Presupuesto.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Presupuesto.Nombre Presupuesto.Id_CenCostos 
&Scoped-define ENABLED-TABLES Presupuesto
&Scoped-define FIRST-ENABLED-TABLE Presupuesto
&Scoped-Define DISPLAYED-FIELDS Presupuesto.Cod_Presupuesto ~
Presupuesto.Nombre Presupuesto.Id_CenCostos Presupuesto.Fec_Creacion ~
Presupuesto.Fec_Retiro Presupuesto.Estado 
&Scoped-define DISPLAYED-TABLES Presupuesto
&Scoped-define FIRST-DISPLAYED-TABLE Presupuesto


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Presupuesto.Cod_Presupuesto 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Presupuesto.Cod_Presupuesto AT ROW 3.15 COL 17 COLON-ALIGNED HELP
          "Ingrese el Código de Presupuesto"
          VIEW-AS FILL-IN 
          SIZE 62 BY .81
          BGCOLOR 15 
     Presupuesto.Nombre AT ROW 4.23 COL 17 COLON-ALIGNED HELP
          "Ingrese el Nombre para el código de presupuesto"
          VIEW-AS FILL-IN 
          SIZE 62 BY .81
          BGCOLOR 15 
     Presupuesto.Id_CenCostos AT ROW 5.85 COL 19 HELP
          "Indique si el código de Ppto maneja Centro de Costos"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
     Presupuesto.Fec_Creacion AT ROW 5.31 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Presupuesto.Fec_Retiro AT ROW 6.65 COL 60 COLON-ALIGNED
          LABEL "Fecha de Inactividad"
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Presupuesto.Estado AT ROW 6.92 COL 19 HELP
          "Ingrese el Estado del Código de Presupuesto" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 18 BY .81
          BGCOLOR 17 FGCOLOR 0 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Presupuesto
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
         HEIGHT             = 7.08
         WIDTH              = 81.57.
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

/* SETTINGS FOR FILL-IN Presupuesto.Cod_Presupuesto IN FRAME F-Main
   NO-ENABLE 1 EXP-HELP                                                 */
/* SETTINGS FOR RADIO-SET Presupuesto.Estado IN FRAME F-Main
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN Presupuesto.Fec_Creacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Presupuesto.Fec_Retiro IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Presupuesto.Id_CenCostos IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN Presupuesto.Nombre IN FRAME F-Main
   EXP-HELP                                                             */
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

&Scoped-define SELF-NAME Presupuesto.Cod_Presupuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Presupuesto.Cod_Presupuesto V-table-Win
ON LEAVE OF Presupuesto.Cod_Presupuesto IN FRAME F-Main /* Codigo Presupuesto */
OR RETURN OF Presupuesto.Cod_Presupuesto DO:
  IF Presupuesto.Cod_Presupuesto:SCREEN-VALUE EQ ""
  OR Presupuesto.Cod_Presupuesto:SCREEN-VALUE EQ ? THEN    
     RETURN.
  APPLY "TAB" TO SELF. 
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Presupuesto.Cod_Presupuesto V-table-Win
ON TAB OF Presupuesto.Cod_Presupuesto IN FRAME F-Main /* Codigo Presupuesto */
OR MOUSE-SELECT-DBLCLICK OF Presupuesto.Cod_Presupuesto 
OR RETURN OF Presupuesto.Cod_Presupuesto DO:
 ON RETURN RETURN.
 
 FIND Presupuesto WHERE Presupuesto.Cod_Presupuesto EQ FRAME {&FRAME-NAME} Cod_Presupuesto
                  NO-LOCK NO-ERROR.
 IF AVAILABLE (Presupuesto) THEN 
   DO:
     RUN MostrarMensaje IN W_Manija (INPUT 2, OUTPUT W_Eleccion).
     APPLY "ENTRY" TO Presupuesto.Cod_Presupuesto.
     RETURN NO-APPLY.
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Presupuesto.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Presupuesto.Estado V-table-Win
ON MOUSE-SELECT-CLICK OF Presupuesto.Estado IN FRAME F-Main /* Estado */
DO:
  APPLY "VALUE-CHANGED" TO Presupuesto.Estado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Presupuesto.Estado V-table-Win
ON VALUE-CHANGED OF Presupuesto.Estado IN FRAME F-Main /* Estado */
OR RETURN OF Presupuesto.Estado DO:
  IF INPUT Presupuesto.Estado = 2 THEN
    ASSIGN Presupuesto.Fec_Retiro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
  ELSE
    ASSIGN Presupuesto.Fec_Retiro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Presupuesto.Nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Presupuesto.Nombre V-table-Win
ON LEAVE OF Presupuesto.Nombre IN FRAME F-Main /* Nombre cuenta */
DO:
 {incluido\BTCANCEL.I}
 /*IF Presupuesto.Nombre:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN
  DO:
    RUN MostrarMensaje IN W_Manija (INPUT 58, OUTPUT W_Eleccion).
    APPLY "ENTRY" TO Presupuesto.Nombre.
    RETURN NO-APPLY.
  END.*/
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
  {src/adm/template/row-list.i "Presupuesto"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Presupuesto"}

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR W_Evento AS CHARACTER FORMAT "XX".
 IF Presupuesto.Nombre:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN
  DO:
    RUN MostrarMensaje IN W_Manija (iNPUT 204, OUTPUT W_Eleccion).
    RUN notify ('cancel-record':U).
    RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
    IF RETURN-VALUE = "YES" THEN
      APPLY "ENTRY" TO Presupuesto.Cod_Presupuesto.
    ELSE
      APPLY "ENTRY" TO Presupuesto.Nombre.
    RETURN ERROR.
  END.  
 ELSE
   DO:
     IF Presupuesto.Cod_Presupuesto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN
        DO:
          RUN MostrarMensaje IN W_Manija (iNPUT 109, OUTPUT W_Eleccion). 
          RETURN ERROR.
        END.
   END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN
    RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Presupuesto. Cod_Presupuesto : " + STRING(Presupuesto.Cod_Presupuesto)).
  ELSE    
    RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Presupuesto. Cod_Presupuesto : " + STRING(Presupuesto.Cod_Presupuesto)).
     
  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose: permite el borrado fisico del registro del plan de cuentas presupuestal,
     no permitia borrar fisicamente.
  Notes:       
------------------------------------------------------------------------------*/
  RUN P-GraLog IN W_Manija (INPUT "ADV: BORRA FISICO Registro, Presupuesto. Cod_Presupuesto : " + STRING(Presupuesto.Cod_Presupuesto)).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

/*mdc  IF Presupuesto.Cod_Presupuesto > "" THEN
    DO:    
      RUN MostrarMensaje IN W_Manija (INPUT 270, OUTPUT W_Eleccion).
      CASE W_Eleccion:
        WHEN FALSE THEN
             RETURN ERROR.
      END CASE.
    END. */
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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  IF AVAILABLE(Presupuesto) THEN
    DO:
      IF Presupuesto.Estado = 2 THEN
        ASSIGN Presupuesto.Fec_Retiro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
      ELSE
        ASSIGN Presupuesto.Fec_Retiro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
      ASSIGN fcodigo = Presupuesto.cod_presupuesto.
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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ).

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
  {src/adm/template/snd-list.i "Presupuesto"}

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

