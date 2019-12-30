&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{incluido\Variable.i "SHARED"}

DEFINE VARIABLE W_OfiUsu AS INTEGER.
DEFINE VARIABLE W_GruUsu AS INTEGER.
DEFINE VARIABLE W_Estado AS LOGICAL.
DEFINE VARIABLE W_String AS CHARACTER.
DEFINE VARIABLE W_Hora AS CHARACTER FORMAT "X(8)".
DEFINE VAR PJI AS CHARACTER FORMAT "X(23)" INITIAL "INICIO PRIMERA JORNADA".
DEFINE VAR PJF AS CHARACTER FORMAT "X(23)" INITIAL "FINAL PRIMERA JORNADA".
DEFINE VAR SJI AS CHARACTER FORMAT "X(23)" INITIAL "INICIO SEGUNDA JORNADA".
DEFINE VAR SJF AS CHARACTER FORMAT "X(23)" INITIAL "FINAL SEGUNDA JORNADA".
DEFINE VAR W_Cod AS CHARACTER.
DEFINE VAR W_Des AS CHARACTER.
DEFINE VARIABLE P_Nit AS CHARACTER.
DEFINE VARIABLE p_Nombre AS CHARACTER.
DEFINE VARIABLE P_Apellido AS CHARACTER.
DEFINE VARIABLE P_AgeCli AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Grupo

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES Usuarios
&Scoped-define FIRST-EXTERNAL-TABLE Usuarios


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Usuarios.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Usuarios.Id_AccSimultaneo ~
Usuarios.permiteCambiarFecha 
&Scoped-define ENABLED-TABLES Usuarios
&Scoped-define FIRST-ENABLED-TABLE Usuarios
&Scoped-Define ENABLED-OBJECTS RECT-326 tgGestionCobranza 
&Scoped-Define DISPLAYED-FIELDS Usuarios.Id_AccSimultaneo ~
Usuarios.permiteCambiarFecha Usuarios.Fec_Creacion Usuarios.Fec_Retiro ~
Usuarios.Fec_UltCam 
&Scoped-define DISPLAYED-TABLES Usuarios
&Scoped-define FIRST-DISPLAYED-TABLE Usuarios
&Scoped-Define DISPLAYED-OBJECTS tgGestionCobranza 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-326
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 10.77.

DEFINE VARIABLE tgGestionCobranza AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.72 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Grupo
     Usuarios.Id_AccSimultaneo AT ROW 9.85 COL 62.86
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 2 BY .77
     Usuarios.permiteCambiarFecha AT ROW 10.58 COL 62.86 WIDGET-ID 22
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 2 BY .77
     tgGestionCobranza AT ROW 11.23 COL 62.86 WIDGET-ID 26
     Usuarios.Fec_Creacion AT ROW 12.54 COL 57.86 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Usuarios.Fec_Retiro AT ROW 13.38 COL 57.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Usuarios.Fec_UltCam AT ROW 14.23 COL 57.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "Permite gestión de cobranza:" VIEW-AS TEXT
          SIZE 20 BY .5 AT ROW 11.35 COL 43 WIDGET-ID 28
     " Información General" VIEW-AS TEXT
          SIZE 18 BY .5 AT ROW 1.27 COL 4
          FGCOLOR 7 FONT 5
     "Fecha de ingreso:" VIEW-AS TEXT
          SIZE 12.14 BY .5 AT ROW 12.69 COL 47.43 WIDGET-ID 16
     "Permitir varias sesiones simultáneas del programa:" VIEW-AS TEXT
          SIZE 33.86 BY .5 AT ROW 9.96 COL 29.14 WIDGET-ID 20
     "Permite cambiar fecha:" VIEW-AS TEXT
          SIZE 15.86 BY .5 AT ROW 10.69 COL 47.14 WIDGET-ID 24
     RECT-326 AT ROW 1.54 COL 3 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: Agencia.Usuarios
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
         HEIGHT             = 14.23
         WIDTH              = 73.
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
/* SETTINGS FOR FRAME F-Grupo
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Grupo:SCROLLABLE       = FALSE
       FRAME F-Grupo:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Usuarios.Fec_Creacion IN FRAME F-Grupo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Usuarios.Fec_Retiro IN FRAME F-Grupo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Usuarios.Fec_UltCam IN FRAME F-Grupo
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Usuarios.Id_AccSimultaneo IN FRAME F-Grupo
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Usuarios.permiteCambiarFecha IN FRAME F-Grupo
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Grupo
/* Query rebuild information for FRAME F-Grupo
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Grupo */
&ANALYZE-RESUME

 


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
  {src/adm/template/row-list.i "Usuarios"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Usuarios"}

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
  HIDE FRAME F-Grupo.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hora V-table-Win 
PROCEDURE Hora :
/* Hace el calculo de los segundos partiendo del valor de pantalla del combo texto */
 DEFINE VARIABLE X AS INTEGER.
 IF SUBSTRING(W_Hora,3,1) EQ ":" THEN
  DO:
    X = (INTEGER(SUBSTRING(W_Hora,1,2)) * 3600) + (INTEGER(SUBSTRING(W_Hora,4,2)) * 60).
    IF SUBSTRING(W_Hora,7,2) = "PM" THEN
       X = X + 43200.
    W_Hora = STRING(X). 
  END.
 IF SUBSTRING(W_Hora,2,1) EQ ":" THEN
  DO:
    X = (INTEGER(SUBSTRING(W_Hora,1,1)) * 3600) + (INTEGER(SUBSTRING(W_Hora,3,2)) * 60).
    IF SUBSTRING(W_Hora,6,2) = "PM" THEN
       X = X + 43200.
    W_Hora = STRING(X). 
  END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE H_Pantalla V-table-Win 
PROCEDURE H_Pantalla :
DEFINE INPUT PARAMETER H AS INTEGER FORMAT "9999".
DEFINE OUTPUT PARAMETER X1 AS INTEGER FORMAT "99".

X1 = INTEGER(SUBSTRING(STRING(H,"HH:MM:SS"),1,2)) * 2.
IF INTEGER(SUBSTRING(STRING(H,"HH:MM:SS"),1,2)) EQ 0 THEN X1 = 47.
IF INTEGER(SUBSTRING(STRING(H,"HH:MM:SS"),4,2)) NE 0 THEN
   X1 = X1 + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Horario V-table-Win 
PROCEDURE Inicializar_Horario :
DISPLAY {&List-3} WITH FRAME {&FRAME-NAME}.
       

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
DEFINE BUFFER Tmp_Usuarios FOR Usuarios.

RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ).

Usuarios.Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
usuarios.usuario:SENSITIVE = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
DEFINE VARIABLE W_Evento AS CHARACTER.

IF Usuarios.Nombre:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" OR Usuarios.Nit:SCREEN-VALUE EQ "" THEN DO:
    MESSAGE "No se ha Digitado el Nit y/o nombre de Usuario" SKIP
            "Rectifique!"
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Usuarios.Usuario:SCREEN-VALUE NO-ERROR.
    
RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).

ASSIGN FRAME {&FRAME-NAME}
    usuarios.usuario
    Usuarios.Estado
    Usuarios.Id_Bloqueo.

ASSIGN Usuarios.Agencia = INTEG(SUBSTRING(W_CAgencia:SCREEN-VALUE,1,3)).

W_Evento = "MODIFICAR".

RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
IF RETURN-VALUE = "YES" THEN
    RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Usuarios. Usuario: " + STRING(Usuarios.Usuario)).
ELSE
    RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Usuarios. Usuario: " + STRING(Usuarios.Usuario)).

IF Usuarios.Estado EQ 1 THEN
    Usuarios.Fec_Retiro = ?.
ELSE
    IF Usuarios.Fec_Retiro = ? THEN
        Usuarios.Fec_Retiro = W_Fecha.
        
/*RUN Local-Row-Available.*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*-----------------------------------------------------------------------------------
   OBSERVACIONES : Permite inactivar un registro.       
  ------------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
     IF Usuarios.Fec_Retiro NE ? OR Usuarios.Estado EQ 2 THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 56,OUTPUT W_Eleccion).
        RETURN ERROR.
     END.
     ELSE DO:
        RUN MostrarMensaje IN W_Manija (INPUT 55,OUTPUT W_Eleccion).
        IF W_Eleccion THEN DO:
           RUN dispatch IN THIS-PROCEDURE ('update-record':U).
           IF AVAILABLE(Usuarios) THEN DO:
              ASSIGN Usuarios.Fec_Retiro = TODAY
                     Usuarios.Estado      = 2.
              RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Usuarios. Usuario: " + 
                  STRING(Usuarios.Usuario)).
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
RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).

DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE Usuarios THEN
        PUBLISH "v-cf_usuarioCntrol"(usuarios.usuario + CHR(1) + w_usuario).

    IF NOT AVAILABLE Usuarios THEN DO:
        IF W_CAgencia:NUM-ITEMS GT 0 THEN
            W_CAgencia:SCREEN-VALUE = W_CAgencia:ENTRY(1).

        APPLY "VALUE-CHANGED":U TO W_CAgencia.

        IF W_CGrupo:NUM-ITEMS GT 0 THEN
            W_CGrupo:SCREEN-VALUE = W_CGrupo:ENTRY(1).

        APPLY "VALUE-CHANGED":U TO W_CGrupo.
        RETURN.
    END.
    ELSE DO:
        FIND FIRST Agencias WHERE Agencias.Agencia EQ Usuarios.Agencia NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
            ASSIGN W_String = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre
                   W_CAgencia:SCREEN-VALUE = W_String.
        ELSE
            IF W_CAgencia:NUM-ITEMS GT 0 THEN
                W_CAgencia:SCREEN-VALUE = W_CAgencia:ENTRY(1).

        APPLY "VALUE-CHANGED":U TO W_CAgencia.

        FIND FIRST Grupos WHERE Grupos.Grupo EQ Usuarios.Grupo NO-LOCK NO-ERROR.
        IF AVAILABLE Grupos THEN
            ASSIGN W_String = STRING(Grupos.Grupo, "99") + "-" + Grupos.Nombre
                   W_CGrupo:SCREEN-VALUE = W_String.
        ELSE
            IF W_CGrupo:NUM-ITEMS GT 0 THEN
                ASSIGN W_CGrupo:SCREEN-VALUE = W_CGrupo:ENTRY(1).

        ASSIGN W_GruUsu = INTEGER(ENTRY(1,W_CGrupo:SCREEN-VALUE, "-"))
               Usuarios.Grupo:SCREEN-VALUE = STRING(W_GruUsu).

        usuarios.nombre:SCREEN-VALUE = usuarios.nombre.
    END.

    IF Usuarios.IdHorario THEN DO:
        /*ENABLE Usuarios.Tip_Menu WITH FRAME F-GRUPO.*/

        IF Usuarios.Tip_Menu EQ 2 THEN
            DISABLE {&List-3} WITH FRAME F-GRUPO.
        ELSE
            ENABLE {&List-4} WITH FRAME F-GRUPO.
    END.

    IF Usuarios.IdHorario EQ YES THEN DO:
        IF Usuarios.Tip_Menu EQ 1 THEN DO:
            DISABLE {&List-3} {&List-5} WITH FRAME {&FRAME-NAME}.
            ENABLE {&List-4} WITH FRAME {&FRAME-NAME}.
            RUN Mostrar_Horario.
        END.
        ELSE DO:
            ENABLE {&List-3} WITH FRAME {&FRAME-NAME}.
            RUN Mostrar_Horario.
        END.
    END.
    ELSE DO:
        RUN Inicializar_Horario.
        DISABLE {&List-3} {&List-5} WITH FRAME {&FRAME-NAME}.
    END.

    IF Usuarios.Id_Estacion THEN DO:
        FIND FIRST Estaciones WHERE Estaciones.Estacion EQ Usuarios.Estacion
                                AND Estaciones.Estado EQ 1 NO-LOCK NO-ERROR.
    END.

    /*usuarios.usuario:SENSITIVE = FALSE.*/
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
  ASSIGN W_CAgencia:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK
                       BY Agencia.Agencia:
      ASSIGN W_String = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre
             W_Estado = W_CAgencia:ADD-LAST(W_String).
  END.
  
  ASSIGN W_CGrupo:LIST-ITEMS = "".
  FOR EACH Grupos WHERE Grupos.Estado EQ 1 NO-LOCK BY Grupo:
      ASSIGN W_String = STRING(Grupos.Grupo, "99") + "-" + Grupos.Nombre
             W_Estado = W_CGrupo:ADD-LAST(W_String).
  END.
  
  ASSIGN W_CAgencia:SENSITIVE = FALSE.
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Usuarios AND Usuarios.Id_OpeOfi
                        AND Usuarios.Prioridad GT 2 THEN           /*W_IdSyA = YES.*/
     ASSIGN W_CAgencia:SENSITIVE = TRUE.
    
  FIND FIRST Usuarios WHERE Usuarios.Agencia EQ W_Agencia NO-LOCK NO-ERROR.  
     
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
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /*RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ).*/
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mensaje_Horario V-table-Win 
PROCEDURE Mensaje_Horario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER H_Dia AS CHARACTER FORMAT "X(9)".
  DEFINE INPUT PARAMETER H_Jor AS CHARACTER FORMAT "X(23)".
  
  MESSAGE "El horario de " H_Jor skip
          "para el dia " H_Dia skip
          "Debe ser configurado. Rectifique!" VIEW-AS ALERT-BOX WARNING TITLE "Falta Configurar".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Horario V-table-Win 
PROCEDURE Mostrar_Horario :
DEFINE VAR X AS INTEGER FORMAT "99". 
DO WITH FRAME {&FRAME-NAME}:
 RUN Inicializar_Horario.
    IF Usuarios.Dia_Permiso[1] OR Usuarios.Tip_Menu EQ 1 THEN DO:
        RUN H_Pantalla (INPUT Usuarios.HIniMan[1], OUTPUT X).
        
        RUN H_Pantalla (INPUT Usuarios.HFinMan[1], OUTPUT X).
        
        RUN H_Pantalla (INPUT Usuarios.HIniTar[1], OUTPUT X).
        
        RUN H_Pantalla (INPUT Usuarios.HFinTar[1], OUTPUT X).
    END.
    IF Usuarios.Tip_Menu EQ 2 THEN
     DO:
       IF Usuarios.Dia_Permiso[2] THEN
        DO:
          RUN H_Pantalla (INPUT Usuarios.HIniMan[2], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinMan[2], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HIniTar[2], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinTar[2], OUTPUT X).
        END.
       IF Usuarios.Dia_Permiso[3] THEN
        DO:
          RUN H_Pantalla (INPUT Usuarios.HIniMan[3], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinMan[3], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HIniTar[3], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinTar[3], OUTPUT X).
        END.
       IF Usuarios.Dia_Permiso[4] THEN
        DO:
          RUN H_Pantalla (INPUT Usuarios.HIniMan[4], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinMan[4], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HIniTar[4], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinTar[4], OUTPUT X).
        END.
       IF Usuarios.Dia_Permiso[5] THEN
        DO:
          RUN H_Pantalla (INPUT Usuarios.HIniMan[5], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinMan[5], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HIniTar[5], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinTar[5], OUTPUT X).
          
        END.
       IF Usuarios.Dia_Permiso[6] THEN
        DO:
          RUN H_Pantalla (INPUT Usuarios.HIniMan[6], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinMan[6], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HIniTar[6], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinTar[6], OUTPUT X).
        END.
       IF Usuarios.Dia_Permiso[7] THEN
        DO:
          RUN H_Pantalla (INPUT Usuarios.HIniMan[7], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinMan[7], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HIniTar[7], OUTPUT X).
          
          RUN H_Pantalla (INPUT Usuarios.HFinTar[7], OUTPUT X).
        END.
     END.
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
  {src/adm/template/snd-list.i "Usuarios"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar_Caracteres&Numeros V-table-Win 
PROCEDURE Verificar_Caracteres&Numeros :
/*DEFINE OUTPUT PARAMETER P_Ok AS LOGICAL.
DEFINE VARIABLE W_Verificar AS INTEGER FORMAT 9.
 ASSIGN P_Ok  = YES
        W_Num = YES
        W_Dig = NO.
 
 DO I = 1 TO LENGTH(W_ClaUsu) BY 1:
    W_Verificar = INTEGER(SUBSTRING(W_ClaUsu,I,1)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
       W_Dig = YES.
    ELSE
       W_Num = YES.
 END.
 IF NOT W_Dig OR NOT W_Num THEN
    P_Ok = NO.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar_Repeticion_Digitos V-table-Win 
PROCEDURE Verificar_Repeticion_Digitos :
/*DEFINE OUTPUT PARAMETER P_Ok AS LOGICAL.
 P_Ok = YES.
 
 DO I = 1 TO LENGTH(W_ClaUsu) BY 1:
   ASSIGN W_dCont = 0
          W_Car1 = SUBSTRING(W_ClaUsu,I,1).
   DO J = 1 TO LENGTH(W_ClaUsu) BY 1:
     W_Car2 = SUBSTRING(W_ClaUsu,J,1).
     IF W_Car1 EQ W_Car2 THEN
        W_dCont = W_dCont + 1.
     IF W_dCont GT 3 THEN
        NEXT.
   END.
   IF W_dCont GT 3 THEN
    DO:
      P_Ok = NO.
      NEXT.
    END.
 END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

