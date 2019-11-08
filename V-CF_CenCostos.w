&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.
  {INCLUIDO\Variable.i "SHARED"}.
  DEFINE VAR W_Ofitbajo  LIKE Agencias.Agencia.
  DEFINE VAR W_SuperUsu  AS   LOGICAL INITIAL YES.
  DEFINE VAR W_OfStr     AS   CHARACTER FORMAT "X(20)".
  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR W_Usu AS LOGICAl.

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
&Scoped-define EXTERNAL-TABLES Cen_Costos
&Scoped-define FIRST-EXTERNAL-TABLE Cen_Costos


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Cen_Costos.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Cen_Costos.Cen_Costos Cen_Costos.Nombre 
&Scoped-define ENABLED-TABLES Cen_Costos
&Scoped-define FIRST-ENABLED-TABLE Cen_Costos
&Scoped-Define ENABLED-OBJECTS W_CmbOfi W_Estado 
&Scoped-Define DISPLAYED-FIELDS Cen_Costos.Cen_Costos ~
Cen_Costos.Fec_Creacion Cen_Costos.Nombre Cen_Costos.Fec_Retiro 
&Scoped-define DISPLAYED-TABLES Cen_Costos
&Scoped-define FIRST-DISPLAYED-TABLE Cen_Costos
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_Estado 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Cen_Costos.Cen_Costos 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(20)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 27 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Estado AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activo", 1,
"Inactivo", 2
     SIZE 16 BY .46
     BGCOLOR 17 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_CmbOfi AT ROW 1.54 COL 13 COLON-ALIGNED HELP
          "Seleccione la agencia a la cual pertenecera el Centro de Costos"
     Cen_Costos.Cen_Costos AT ROW 2.62 COL 13 COLON-ALIGNED HELP
          "Digite el Código con el cual se identificara el Centro de Costo"
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     Cen_Costos.Fec_Creacion AT ROW 2.62 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Cen_Costos.Nombre AT ROW 3.69 COL 13 COLON-ALIGNED HELP
          "Digite el Nombre del Centro de Costos"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Cen_Costos.Fec_Retiro AT ROW 3.69 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_Estado AT ROW 5.31 COL 15 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: DATOS.Cen_Costos
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
         HEIGHT             = 5.46
         WIDTH              = 77.14.
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

/* SETTINGS FOR FILL-IN Cen_Costos.Cen_Costos IN FRAME F-Main
   1 EXP-HELP                                                           */
/* SETTINGS FOR FILL-IN Cen_Costos.Fec_Creacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cen_Costos.Fec_Retiro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cen_Costos.Nombre IN FRAME F-Main
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

&Scoped-define SELF-NAME Cen_Costos.Cen_Costos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cen_Costos.Cen_Costos V-table-Win
ON LEAVE OF Cen_Costos.Cen_Costos IN FRAME F-Main /* Centro de Costos */
DO:
 /* {INCLUIDO\BTCANCEL.I}
  FIND Cen_Costos WHERE Cen_Costos.Cen_Costos = FRAME {&FRAME-NAME} Cen_Costos AND 
       Cen_Costos.Agencia = W_OfiTbajo NO-LOCK NO-ERROR.
  IF AVAILABLE (Cen_Costos) THEN 
   DO:
     RUN MostrarMensaje IN W_Manija (INPUT 4, OUTPUT W_Eleccion).
     APPLY "ENTRY" TO Cen_Costos.Cen_Costos.
     RETURN NO-APPLY.
   END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi V-table-Win
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F-Main /* Agencia */
DO:
    ASSIGN W_OfiTbajo = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,3)) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Estado V-table-Win
ON ENTRY OF W_Estado IN FRAME F-Main
DO:
  ON RETURN TAB.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Estado V-table-Win
ON LEAVE OF W_Estado IN FRAME F-Main
OR RETURN OF W_Estado DO:
  IF INTEGER (W_Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 2 THEN  
      DISPLAY TODAY @ Cen_Costos.Fec_Retiro WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  ASSIGN W_Estado.
  IF INTEGER (W_Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 2 THEN
       DISPLAY TODAY @ Cen_Costos.Fec_Retiro WITH FRAME {&FRAME-NAME}.
  ELSE
    DISPLAY "" @ Cen_Costos.Fec_Retiro WITH FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "Cen_Costos"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Cen_Costos"}

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

  DO WITH FRAME {&FRAME-NAME}:
    IF W_SuperUsu THEN DO:
      ENABLE W_cmbofi.
      APPLY "ENTRY" TO W_CmbOfi.
    END.
    ELSE
       ASSIGN W_Cmbofi:HIDDEN   IN FRAME F-Main = TRUE.
    
  
  
  
/*  DO WITH FRAME {&FRAME-NAME}:
     IF W_Usu THEN
      DO:
        Cen_Costos.Agencia:SENSITIVE    = TRUE.
        Cen_Costos.Agencia:BGCOLOR      = ?.
        Cen_Costos.Agencia:FGCOLOR      = ?.
      END.
     ELSE
      DO:
        Cen_Costos.Agencia:SENSITIVE    = FALSE.
        Cen_Costos.Agencia:BGCOLOR      = 3.
        Cen_Costos.Agencia:FGCOLOR      = 15.
      END.*/
  
     ASSIGN W_Estado:SCREEN-VALUE       = STRING(1)
            Cen_Costos.Nombre:SENSITIVE = TRUE
            W_Estado:SENSITIVE          = TRUE.
            
     DISPLAY TODAY         @ Cen_Costos.Fec_Creacion
             ""            @ Cen_Costos.Fec_retiro.
            /* W_Agencia     @ Cen_Costos.Agencia
             W_Nom_Agencia @ W_Nomofi.*/
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
  DEFINE VAR W_Evento AS CHAR INITIAL "Modificar". 

  DO WITH FRAME {&FRAME-NAME}:
     RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
     IF RETURN-VALUE EQ "YES" THEN
        IF Cen_Costos.Cen_Costos:SCREEN-VALUE EQ "000" OR
           Cen_Costos.Nombre:SCREEN-VALUE     EQ ""    THEN DO:
           RUN MostrarMensaje IN W_Manija (INPUT 107, OUTPUT W_Eleccion). 
           RETURN ERROR.
        END.
        ELSE DO:
           FIND FIRST Entidad NO-LOCK NO-ERROR.
           IF AVAILABLE Entidad THEN DO:
              IF Entidad.Id_CenCosto EQ NO THEN DO:
                 RUN MostrarMensaje IN W_Manija (INPUT 4, OUTPUT W_Eleccion). 
                 RETURN ERROR.
              END.
           END.
           ELSE DO:
              MESSAGE "No Existe Entidad Configurada. Verifique"
                      VIEW-AS ALERT-BOX INFORMATION TITLE "Error Configuración".
              RETURN ERROR.
           END.
        END.

     W_Evento = "Ingresar".
     RUN dispatch IN THIS-PROCEDURE (INPUT 'assign-record':U ) .
     
     IF W_Estado:SCREEN-VALUE = "1" AND Cen_Costos.Fec_Retiro <> ? THEN
        ASSIGN Cen_Costos.Fec_Retiro = ?.
     ELSE
        IF W_Estado:SCREEN-VALUE = "2" AND Cen_Costos.Fec_Retiro = ? THEN
           ASSIGN Cen_Costos.Fec_Retiro.
     ASSIGN Cen_Costos.Estado = W_Estado.
     IF  W_Evento                             EQ " Ingresar "
     AND Cen_Costos.Fec_Creacion EQ ? THEN 
         ASSIGN Cen_Costos.Fec_Creacion = TODAY.   
     ASSIGN Cen_Costos.Agencia = W_OfiTbajo.
     DISPLAY  Cen_Costos.Fec_Creacion.
     IF W_Evento EQ "Ingresar" THEN
        RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, CenCostos. Codigo: " + STRING(Cen_Costos.Cen_Costos)).
     ELSE
        RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, CenCostos. Codigo: " + STRING(Cen_Costos.Cen_Costos)).
     RUN dispatch IN THIS-PROCEDURE (INPUT 'DISPLAY-FIELDS':U ) .

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects V-table-Win 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
 RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .
/* FIND FIRST Entidad NO-LOCK NO-ERROR.
 *  IF Entidad.Id_CenCosto   EQ YES THEN DO:
 *      RUN get-link-handle IN adm-broker-hdl
 *                          (INPUT THIS-PROCEDURE,
 *                           INPUT 'costo':U,
 *                          OUTPUT W_Alm).
 *      RUN local-habilita IN WIDGET-HANDLE(W_Alm).
 *    END.
 *  ELSE
 *    DO:
 * message "paso por create" Entidad.entidad view-as alert-box.
 *       RUN get-link-handle IN adm-broker-hdl
 *           (INPUT THIS-PROCEDURE,
 *            INPUT 'costo':U,
 *            OUTPUT W_Alm).
 *       RUN local-deshabilita IN WIDGET-HANDLE(W_Alm).
 *     END.  */

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN MostrarMensaje IN W_Manija (INPUT 132, OUTPUT W_Eleccion).
  IF W_Eleccion THEN
   DO:
     FIND CURRENT Cen_Costos SHARE-LOCK NO-ERROR NO-WAIT.
     IF AVAILABLE(Cen_Costos) THEN
      DO WITH FRAME {&FRAME-NAME}:
         ASSIGN Cen_costos.Estado     = 2
                Cen_costos.Fec_Retiro = Today
                W_Estado:SCREEN-VALUE = STRING(Cen_costos.Estado)
                Cen_costos.Fec_Retiro:SCREEN-VALUE = STRING(TODAY,"99/99/9999").
         RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, CenCostos. Codigo: " + STRING(Cen_Costos.Cen_Costos)).
      END.  
     FIND CURRENT Cen_Costos NO-LOCK NO-ERROR.
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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  IF AVAILABLE (Cen_costos) THEN
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN W_Estado:SCREEN-VALUE = STRING(Cen_Costos.Estado).
      ASSIGN Cen_Costos.Nombre:SENSITIVE = FALSE
             W_Estado:SENSITIVE          = FALSE.
      FIND FIRST Entidad NO-LOCK NO-ERROR. 
      IF  Cen_Costos.Fec_Retiro EQ ? 
      AND Entidad.Id_CenCosto   EQ YES 
      AND Cen_Costos.Cen_Costos NE 999 THEN
          ASSIGN Cen_Costos.Nombre:SENSITIVE = TRUE
                 W_Estado:SENSITIVE          = TRUE.
  /* IF Entidad.Id_CenCosto   EQ YES THEN DO:
 *      RUN get-link-handle IN adm-broker-hdl
 *         (INPUT THIS-PROCEDURE,
 *          INPUT 'costo':U,
 *          OUTPUT W_Alm).
 *     RUN local-habilita IN WIDGET-HANDLE(W_Alm).
 *   END.
 *   ELSE
 *     DO:
 * message "paso por display" Entidad.entidad view-as alert-box.
 *       RUN get-link-handle IN adm-broker-hdl
 *           (INPUT THIS-PROCEDURE,
 *            INPUT 'costo':U,
 *            OUTPUT W_Alm).
 *       RUN local-deshabilita IN WIDGET-HANDLE(W_Alm).
 *     END.*/
    FIND Agencias WHERE Agencias.Agencia = Cen_costos.Agencia NO-LOCK NO-ERROR.
    IF AVAILABLE (Agencias) THEN 
       ASSIGN W_CmbOfi:SCREEN-VALUE = STRING(Cen_costos.Agencia,"999") + " - " + Agencia.Nombre.
       DISABLE W_CmbOfi Cen_Costos.Cen_Costos.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */

  DEF VAR W_CbStr  AS CHAR.
 /*{incluido\CmbOfi.i}*/
  W_OfiTbajo = W_Agencia.
  FOR EACH Agencias NO-LOCK:
    W_Ok = W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME {&FRAME-NAME}.
  END.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  IF AVAILABLE Cen_Costos THEN DO:
     FIND Agencias WHERE Agencias.Agencia EQ Cen_Costos.Agencia NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN
        W_CmbOfi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
  END.
  
   /* Code placed here will execute AFTER standard behavior.    */
  
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

