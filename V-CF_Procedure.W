&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

   DEFINE SHARED VAR W_Nom_Agencia AS CHAR FORMAT "X(40)".
   DEFINE SHARED VAR W_Agencia LIKE Agencias.Agencia INITIAL 001.
   DEFINE SHARED VAR W_Manija AS HANDLE.
   DEFINE        VAR W_Rpta   AS LOGICAL.
   DEFINE        VAR W_Rowid  AS ROWID.

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
&Scoped-define EXTERNAL-TABLES Programas
&Scoped-define FIRST-EXTERNAL-TABLE Programas


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Programas.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Programas.Opcion Programas.Estado ~
Programas.Id_Procesos Programas.Ejecutable Programas.Ubicacion ~
Programas.Nom_Libreria Programas.Id_Clave Programas.Descripcion 
&Scoped-define ENABLED-TABLES Programas
&Scoped-define FIRST-ENABLED-TABLE Programas
&Scoped-Define ENABLED-OBJECTS RECT-144 
&Scoped-Define DISPLAYED-FIELDS Programas.Programa Programas.Opcion ~
Programas.Estado Programas.Nom_Fisico Programas.Tipo Programas.Id_Procesos ~
Programas.Ejecutable Programas.Ubicacion Programas.Nom_Libreria ~
Programas.Id_Clave Programas.Descripcion 
&Scoped-define DISPLAYED-TABLES Programas
&Scoped-define FIRST-DISPLAYED-TABLE Programas
&Scoped-Define DISPLAYED-OBJECTS W_Rproceso 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Programas.Tipo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE W_Rproceso AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Clientes", 1,
"Taquilla", 2,
"Procesos", 3,
"Cierres", 4
     SIZE 10 BY 4.31
     BGCOLOR 17  NO-UNDO.

DEFINE RECTANGLE RECT-144
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 5 BY 5.12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Programas.Programa AT ROW 2.62 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Programas.Opcion AT ROW 3.54 COL 17 COLON-ALIGNED
          LABEL "Nombre"
          VIEW-AS FILL-IN 
          SIZE 32 BY .81
          BGCOLOR 15 
     Programas.Estado AT ROW 2.62 COL 54 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 20 BY .81
          BGCOLOR 17 
     Programas.Nom_Fisico AT ROW 4.54 COL 17 COLON-ALIGNED
          LABEL "Nombre Físico"
          VIEW-AS FILL-IN 
          SIZE 32 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Programas.Tipo AT ROW 6.12 COL 8
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .69
     Programas.Id_Procesos AT ROW 6.12 COL 59
          LABEL "El programa es un proceso de"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .73
     Programas.Ejecutable AT ROW 6.92 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 43 BY .81
          BGCOLOR 15 
     W_Rproceso AT ROW 6.92 COL 61 HELP
          "Seleccione con que proceso se relaciona el Programa" NO-LABEL
     Programas.Ubicacion AT ROW 8.27 COL 8
          LABEL "El programa hace parte de una libreria"
          VIEW-AS TOGGLE-BOX
          SIZE 37 BY .69
     Programas.Nom_Libreria AT ROW 9.08 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 43 BY .81
          BGCOLOR 15 
     Programas.Id_Clave AT ROW 10.42 COL 8
          LABEL "El programa debe pedir clave para su acceso"
          VIEW-AS TOGGLE-BOX
          SIZE 34 BY .73
     Programas.Descripcion AT ROW 12.85 COL 5 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 87 BY 5.38
          BGCOLOR 15 FONT 5
     RECT-144 AT ROW 6.38 COL 57
     "Información General" VIEW-AS TEXT
          SIZE 18 BY .77 AT ROW 1.54 COL 8
          FGCOLOR 7 FONT 5
     "Descripción del programa" VIEW-AS TEXT
          SIZE 26 BY .77 AT ROW 11.77 COL 6
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Programas
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
         HEIGHT             = 17.42
         WIDTH              = 94.
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

/* SETTINGS FOR TOGGLE-BOX Programas.Id_Clave IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Programas.Id_Procesos IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Programas.Nom_Fisico IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Programas.Opcion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Programas.Programa IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Programas.Tipo IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX Programas.Ubicacion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET W_Rproceso IN FRAME F-Main
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

&Scoped-define SELF-NAME Programas.Ejecutable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Programas.Ejecutable V-table-Win
ON LEAVE OF Programas.Ejecutable IN FRAME F-Main /* Ejecutable */
DO:
  ASSIGN W_Rproceso
         W_Rowid    = ROWID(Programas).
      IF W_Rproceso EQ 4 THEN DO WITH FRAME {&FRAME-NAME}:
         FIND FIRST Programas WHERE ROWID(Programas)     NE W_Rowid
                                AND Programas.Ejecutable EQ Programas.Ejecutable:SCREEN-VALUE
                                AND Programas.Proceso    EQ 4
                    NO-LOCK NO-ERROR.
         IF AVAILABLE(Programas) THEN DO:
            MESSAGE "El Ejecutable ya existe para el Programa " Programas.Programa SKIP(1)
                    " con Idt de Cierre ...Revise" VIEW-AS ALERT-BOX.
            /*RUN MostrarMensaje IN W_Manija (INPUT 254, OUTPUT W_Eleccion).*/
            ASSIGN W_RProceso              = 1
                   W_RProceso:SCREEN-VALUE = "1".
         END.
         FIND Programas WHERE ROWID(Programas) = W_Rowid NO-LOCK NO-ERROR.
         APPLY "ENTRY" TO W_RProceso.
         RETURN NO-APPLY.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Programas.Id_Procesos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Programas.Id_Procesos V-table-Win
ON VALUE-CHANGED OF Programas.Id_Procesos IN FRAME F-Main /* El programa es un proceso de */
DO:
  IF Programas.Id_Procesos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes" THEN
     ENABLE W_Rproceso WITH FRAME {&FRAME-NAME}.
  ELSE   
     DISABLE W_Rproceso WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Programas.Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Programas.Tipo V-table-Win
ON VALUE-CHANGED OF Programas.Tipo IN FRAME F-Main /* Programa */
DO:
  IF INPUT FRAME {&FRAME-NAME} Programas.Tipo = TRUE THEN
   DO:
     ENABLE Programa.Ejecutable WITH FRAME {&FRAME-NAME}.
     APPLY "ENTRY" TO Programa.Ejecutable.
     RETURN NO-APPLY.
   END.
  ELSE
   DO:
     ASSIGN Programa.Ejecutable:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
     DISABLE Programa.Ejecutable WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Programas.Ubicacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Programas.Ubicacion V-table-Win
ON VALUE-CHANGED OF Programas.Ubicacion IN FRAME F-Main /* El programa hace parte de una libreria */
DO:
  IF INPUT FRAME {&FRAME-NAME} Programas.Ubicacion = TRUE THEN
   DO:
     ENABLE Programas.Nom_Libreria WITH FRAME {&FRAME-NAME}.
     APPLY "ENTRY" TO Programas.Nom_Libreria.
     RETURN NO-APPLY.
   END.
  ELSE
   DO:
     ASSIGN Programas.Nom_Libreria:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
     DISABLE Programas.Nom_Libreria WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rproceso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rproceso V-table-Win
ON VALUE-CHANGED OF W_Rproceso IN FRAME F-Main
DO:
  ASSIGN W_Rproceso
         W_Rowid    = ROWID(Programas).
      IF W_Rproceso EQ 3 THEN DO WITH FRAME {&FRAME-NAME}:
         FIND FIRST Programas WHERE ROWID(Programas)     NE W_Rowid
                                AND Programas.Ejecutable EQ Programas.Ejecutable:SCREEN-VALUE
                                AND Programas.Proceso    EQ 3
                    NO-LOCK NO-ERROR.
         IF AVAILABLE(Programas) THEN DO:
            MESSAGE "El Ejecutable ya existe para el Programa " Programas.Programa SKIP(1)
                    " con Idt de Proceso ...Revise" VIEW-AS ALERT-BOX.
            /*RUN MostrarMensaje IN W_Manija (INPUT 254, OUTPUT W_Eleccion).*/
            ASSIGN W_RProceso              = 1
                   W_RProceso:SCREEN-VALUE = "1".
         END.
         FIND Programas WHERE ROWID(Programas) = W_Rowid NO-LOCK NO-ERROR.
         APPLY "ENTRY" TO W_RProceso.
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
  {src/adm/template/row-list.i "Programas"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Programas"}

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
  DISPLAY CURRENT-VALUE(Sec_Programas) + 1 @ Programas.Programa WITH FRAME {&FRAME-NAME}.
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

  /* Code placed here will execute PRIOR to standard behavior. */
  IF (Programas.Opcion:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = "" THEN
    DO:
      RUN MostrarMensaje IN W_Manija (iNPUT 204, OUTPUT W_Rpta).
      RUN notify ('cancel-record':U).
      APPLY "ENTRY" TO Programas.Opcion.
      RETURN ERROR.
    END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN
     Programas.Nom_Fisico = Programas.Opcion.


  IF Programas.Id_Procesos = yes THEN
     ASSIGN Programas.Proceso = W_RProceso.
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
  IF Programas.Estado:SCREEN-VALUE IN FRAME F-Main EQ "2" THEN
     RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Programas. Programa : " + STRING(Programas.Programa)).
  ELSE DO:
    RUN MostrarMensaje IN W_Manija (INPUT 270,OUTPUT W_Rpta).
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
  IF AVAILABLE(Programas) THEN
   DO:
     IF Programas.Ubicacion = TRUE THEN
       ENABLE Programas.Nom_Libreria WITH FRAME {&FRAME-NAME}.
     ELSE
       DISABLE Programas.Nom_Libreria WITH FRAME {&FRAME-NAME}.
     IF Programas.Id_Procesos = yes THEN
      DO:
       ENABLE W_Rproceso WITH FRAME {&FRAME-NAME}.
       ASSIGN W_RProceso:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Programas.Proceso).
      END. 
     ELSE   
       DISABLE W_Rproceso WITH FRAME {&FRAME-NAME}.     
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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR W_Evento AS CHAR INITIAL "SA".
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Programas. Programa : " + STRING(Programas.Programa)).
  ELSE
     RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Programas. Programa : " + STRING(Programas.Programa)).
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
  {src/adm/template/snd-list.i "Programas"}

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

