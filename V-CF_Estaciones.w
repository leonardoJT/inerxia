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

 DEFINE VARIABLE W_Ofitbajo    LIKE Agencias.Agencia.
 DEFINE VARIABLE W_CodFormato  LIKE Formatos.Cod_Formato.
 DEFINE VARIABLE W_CodForAux   LIKE Formatos.Cod_Formato.
 DEFINE VARIABLE W_String        AS CHARACTER.
 DEFINE VARIABLE W_Procedimiento AS CHARACTER.
 DEFINE VARIABLE W_SuperUsu      AS LOGICAL.
 DEFINE VARIABLE W_Estado        AS LOGICAL.
 DEFINE VARIABLE W_Primero       AS LOGICAL INITIAL NO.
 DEFINE VARIABLE i               AS INTEGER.


 DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
 DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
 DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.

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
&Scoped-define EXTERNAL-TABLES Estaciones
&Scoped-define FIRST-EXTERNAL-TABLE Estaciones


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Estaciones.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Estaciones.Estacion Estaciones.Descripcion ~
Estaciones.Nit_Responsable Estaciones.Estado ~
Estaciones.Fec_UltMantenimiento Estaciones.Comentarios 
&Scoped-define ENABLED-TABLES Estaciones
&Scoped-define FIRST-ENABLED-TABLE Estaciones
&Scoped-Define ENABLED-OBJECTS IMAGE-3 IMAGE-4 RECT-201 
&Scoped-Define DISPLAYED-FIELDS Estaciones.Estacion Estaciones.Descripcion ~
Estaciones.Nit_Responsable Estaciones.Estado Estaciones.Fec_Creacion ~
Estaciones.Fec_Retiro Estaciones.Fec_UltEntrada Estaciones.Fec_UltSalida ~
Estaciones.Fec_UltMantenimiento Estaciones.Comentarios 
&Scoped-define DISPLAYED-TABLES Estaciones
&Scoped-define FIRST-DISPLAYED-TABLE Estaciones
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_NomResponsable HEntrada HSalida 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS W_CmbOfi 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
Agencia||y|bdcentral.Estaciones.Agencia
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "Agencia"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(20)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 45 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE HEntrada AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE HSalida AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomResponsable AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "imagenes\computador":U
     SIZE 5 BY 1.35.

DEFINE IMAGE IMAGE-4
     FILENAME "imagenes\calendario":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE RECT-201
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_CmbOfi AT ROW 2.35 COL 27 COLON-ALIGNED
     Estaciones.Estacion AT ROW 3.42 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 45 BY .85
          BGCOLOR 15 
     Estaciones.Descripcion AT ROW 4.5 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 45 BY .85
          BGCOLOR 15 
     Estaciones.Nit_Responsable AT ROW 5.58 COL 27 COLON-ALIGNED
          LABEL "Responsable"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     W_NomResponsable AT ROW 5.58 COL 39 COLON-ALIGNED NO-LABEL
     Estaciones.Estado AT ROW 6.65 COL 56 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 18 BY .81
     Estaciones.Fec_Creacion AT ROW 8.81 COL 27 COLON-ALIGNED
          LABEL "Fecha de Creación"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 18 FGCOLOR 15 
     Estaciones.Hora_UltEntrada AT ROW 8.81 COL 56 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7.14 BY 1
     Estaciones.Agencia AT ROW 8.81 COL 63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.86 BY 1.08
     Estaciones.Hora_UltSalida AT ROW 9.88 COL 63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     Estaciones.Fec_Retiro AT ROW 10.15 COL 27 COLON-ALIGNED
          LABEL "Fecha de Desactivación"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 18 FGCOLOR 15 
     Estaciones.Fec_UltEntrada AT ROW 12.04 COL 27 COLON-ALIGNED
          LABEL "Última Entrada al Aplicativo"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 18 FGCOLOR 15 
     HEntrada AT ROW 12.04 COL 51 COLON-ALIGNED NO-LABEL
     Estaciones.Fec_UltSalida AT ROW 13.38 COL 27 COLON-ALIGNED
          LABEL "Última Salida del Aplicativo"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 18 FGCOLOR 15 
     HSalida AT ROW 13.38 COL 51 COLON-ALIGNED NO-LABEL
     Estaciones.Fec_UltMantenimiento AT ROW 15.12 COL 59 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.57 BY .81
          BGCOLOR 15 
     Estaciones.Comentarios AT ROW 16.08 COL 4 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 72 BY 4.35
          BGCOLOR 15 
     IMAGE-3 AT ROW 1 COL 5
     IMAGE-4 AT ROW 7.19 COL 5
     RECT-201 AT ROW 8.54 COL 4
     " Información de Ubicación de la Estación" VIEW-AS TEXT
          SIZE 39 BY .81 AT ROW 1.27 COL 11
          FGCOLOR 7 FONT 5
     "  Comentarios" VIEW-AS TEXT
          SIZE 16 BY .77 AT ROW 15.27 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "  Manejo Histórico y Actual de la Estación" VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 7.46 COL 10
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: bdcentral.Estaciones
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
         HEIGHT             = 19.69
         WIDTH              = 76.43.
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

/* SETTINGS FOR FILL-IN Estaciones.Agencia IN FRAME F-Main
   NO-DISPLAY NO-ENABLE EXP-LABEL                                       */
/* SETTINGS FOR FILL-IN Estaciones.Fec_Creacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Estaciones.Fec_Retiro IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Estaciones.Fec_UltEntrada IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Estaciones.Fec_UltSalida IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN HEntrada IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Estaciones.Hora_UltEntrada IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Estaciones.Hora_UltSalida IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN HSalida IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Estaciones.Nit_Responsable IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX W_CmbOfi IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_NomResponsable IN FRAME F-Main
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

&Scoped-define SELF-NAME Estaciones.Nit_Responsable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Estaciones.Nit_Responsable V-table-Win
ON LEAVE OF Estaciones.Nit_Responsable IN FRAME F-Main /* Responsable */
DO:
    DO WITH FRAME {&FRAME-NAME}:
     DEFINE VAR P_Agecli LIKE clientes.Agencia.
     FIND Clientes WHERE Clientes.Nit EQ Estaciones.Nit_responsable:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        W_NomResponsable:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN c-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        ASSIGN W_NomResponsable:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
               Estaciones.Nit_responsable:SCREEN-VALUE = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion EQ "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Natural" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi V-table-Win
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F-Main /* Agencia */
DO:
   IF SELF:SCREEN-VALUE EQ ? THEN
      RETURN.
      
   ASSIGN W_OfiTbajo = INTEGER(ENTRY(1,SELF:SCREEN-VALUE, "-"))
          Estaciones.Agencia:SCREEN-VALUE = STRING(W_OfiTbajo).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "Estaciones"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Estaciones"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
DEFINE VAR W_Evento  AS   CHARACTER FORMAT "XX".
  DEFINE VAR G_Ciudad  LIKE Agencias.Ciudad.
  DEFINE VAR G_Zona    LIKE Agencias.Zona.
  DEFINE VAR G_Entidad LIKE Agencia.Entidad.
  
  DO WITH FRAME {&FRAME-NAME}:
     
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).

     RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
     IF RETURN-VALUE = "YES" THEN DO:
       ASSIGN Estaciones.Fec_Creacion = TODAY
              Estaciones.Agencia      = INTEGER(Estaciones.Agencia:SCREEN-VALUE).
       RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Estaciones. Estacion: " + STRING(Estaciones.Estacion)).
     END.
     ELSE
       RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Estaciones. Estacion: " + STRING(Estaciones.Estacion)).
        
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
DO WITH FRAME {&FRAME-NAME}:
     IF Estaciones.Fec_Retiro NE ? OR Estaciones.Estado EQ 2 THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 56,OUTPUT W_Eleccion).
        RETURN ERROR.
     END.
     ELSE DO:
        RUN MostrarMensaje IN W_Manija (INPUT 55,OUTPUT W_Eleccion).
        IF W_Eleccion THEN DO:
           RUN dispatch IN THIS-PROCEDURE ('update-record':U).
           IF AVAILABLE(Estaciones) THEN DO:
              ASSIGN Estaciones.Fec_Retiro = TODAY
                     Usuarios.Estado        = 2.
              RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Estaciones. Estaciones.Estacion: " + STRING(Estaciones.Estacion)).
           END.
           RUN dispatch IN THIS-PROCEDURE ('end-update':U).
           RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
        END.
     END.
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
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
FIND Clientes WHERE Clientes.Nit EQ Estaciones.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN
     W_NomResponsable = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  ELSE
     W_NomResponsable = "No Existe en clientes".
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  
  FIND Agencias WHERE Agencia.Agencia EQ Estaciones.Agencia NO-LOCK NO-ERROR.
  IF AVAILABLE Agencias THEN
     ASSIGN W_String = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre
            W_CmbOfi:SCREEN-VALUE IN FRAME F-MAIN = W_String.
  ELSE
     IF W_CmbOfi:NUM-ITEMS GT 0 THEN
        ASSIGN W_CmbOfi:SCREEN-VALUE = W_CmbOfi:ENTRY(1).
      
      
  IF AVAILABLE(Estaciones) THEN DO:
   IF Estaciones.Hora_UltEntrada NE ? THEN
     ASSIGN HEntrada:SCREEN-VALUE = STRING(Estaciones.Hora_UltEntrada,"hh:mm am").
   IF Estaciones.Hora_UltSalida NE ? THEN
     ASSIGN HSalida:SCREEN-VALUE = STRING(Estaciones.Hora_UltSalida,"hh:mm am").

  END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  ASSIGN W_CmbOfi:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK
                       BY Agencia.Agencia:
      ASSIGN W_String = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre
             W_Estado = W_CmbOfi:ADD-LAST(W_String).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "Agencia" "Estaciones" "Agencia"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "Estaciones"}

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

