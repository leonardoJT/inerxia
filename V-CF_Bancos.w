&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{Incluido/Variable.I "SHARED"}
  DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.

  DEFINE VAR W_Nuevo AS LOGICAL.

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
&Scoped-define EXTERNAL-TABLES Bancos
&Scoped-define FIRST-EXTERNAL-TABLE Bancos


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Bancos.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Bancos.Cod_Compensa Bancos.Nit ~
Bancos.Nom_Contacto Bancos.Tel_Contacto Bancos.Cod_RemCobro ~
Bancos.Cod_RemNego Bancos.Dia_Canje Bancos.Dia_RemCobro Bancos.Dia_RemNego ~
Bancos.Calif_Emisor Bancos.Porc_Participacion 
&Scoped-define ENABLED-TABLES Bancos
&Scoped-define FIRST-ENABLED-TABLE Bancos
&Scoped-Define ENABLED-OBJECTS IMAGE-1 RECT-198 RECT-278 
&Scoped-Define DISPLAYED-FIELDS Bancos.Cod_Compensa Bancos.Nit ~
Bancos.Nombre Bancos.Nom_Contacto Bancos.Tel_Contacto Bancos.Cod_RemCobro ~
Bancos.Cod_RemNego Bancos.Dia_Canje Bancos.Dia_RemCobro Bancos.Dia_RemNego ~
Bancos.Calif_Emisor Bancos.Porc_Participacion 
&Scoped-define DISPLAYED-TABLES Bancos
&Scoped-define FIRST-DISPLAYED-TABLE Bancos
&Scoped-Define DISPLAYED-OBJECTS F_Nombre1 F_Nombre2 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE F_Nombre1 AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Nombre2 AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "imagenes\dinero":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE RECT-198
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 5.92.

DEFINE RECTANGLE RECT-278
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 2.69.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Bancos.Cod_Compensa AT ROW 1.54 COL 22 COLON-ALIGNED
          LABEL "Banco" FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 37 BY .81
          BGCOLOR 15 
     Bancos.Nit AT ROW 2.62 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37 BY .81
          BGCOLOR 15 
     Bancos.Nombre AT ROW 3.69 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Bancos.Nom_Contacto AT ROW 5.31 COL 22 COLON-ALIGNED
          LABEL "Nombre"
          VIEW-AS FILL-IN 
          SIZE 37 BY .81
          BGCOLOR 15 
     Bancos.Tel_Contacto AT ROW 6.38 COL 22 COLON-ALIGNED
          LABEL "Telefono"
          VIEW-AS FILL-IN 
          SIZE 37 BY .81
          BGCOLOR 15 
     Bancos.Cod_RemCobro AT ROW 9.35 COL 22 COLON-ALIGNED
          LABEL "Deducible Remesa Cobro" FORMAT "X(4)"
          VIEW-AS FILL-IN 
          SIZE 37 BY .81
          BGCOLOR 15 
     F_Nombre1 AT ROW 10.42 COL 22 COLON-ALIGNED NO-LABEL
     Bancos.Cod_RemNego AT ROW 12.04 COL 22 COLON-ALIGNED
          LABEL "Deducible Remesa Negociada"
          VIEW-AS FILL-IN 
          SIZE 37 BY .81
          BGCOLOR 15 
     F_Nombre2 AT ROW 13.12 COL 22 COLON-ALIGNED NO-LABEL
     Bancos.Dia_Canje AT ROW 16.08 COL 22 COLON-ALIGNED
          LABEL "Días de Canje Local"
          VIEW-AS FILL-IN 
          SIZE 37.57 BY .81
          BGCOLOR 15 
     Bancos.Dia_RemCobro AT ROW 17.15 COL 22 COLON-ALIGNED
          LABEL "Días Canje Remesa al Cobro"
          VIEW-AS FILL-IN 
          SIZE 37.57 BY .81
          BGCOLOR 15 
     Bancos.Dia_RemNego AT ROW 18.23 COL 22 COLON-ALIGNED
          LABEL "Días Canje Remesa Negociada"
          VIEW-AS FILL-IN 
          SIZE 37.57 BY .81
          BGCOLOR 15 
     Bancos.Calif_Emisor AT ROW 19.58 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY .81
          BGCOLOR 15 
     Bancos.Porc_Participacion AT ROW 20.65 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY .81
          BGCOLOR 15 
     "Configuración de los días de Canje" VIEW-AS TEXT
          SIZE 31 BY .81 AT ROW 15 COL 5
          FGCOLOR 7 FONT 5
     "Contacto" VIEW-AS TEXT
          SIZE 10 BY 1.08 AT ROW 4.5 COL 4
          FGCOLOR 7 FONT 5
     "  Configuración de los Deducibles" VIEW-AS TEXT
          SIZE 30 BY .81 AT ROW 8 COL 4
          FGCOLOR 7 FONT 5
     IMAGE-1 AT ROW 1.81 COL 6
     RECT-198 AT ROW 8.27 COL 2
     RECT-278 AT ROW 5.04 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Bancos
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
         HEIGHT             = 21.04
         WIDTH              = 63.14.
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

/* SETTINGS FOR FILL-IN Bancos.Cod_Compensa IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Bancos.Cod_RemCobro IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Bancos.Cod_RemNego IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Bancos.Dia_Canje IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Bancos.Dia_RemCobro IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Bancos.Dia_RemNego IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN F_Nombre1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Nombre2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bancos.Nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bancos.Nom_Contacto IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Bancos.Tel_Contacto IN FRAME F-Main
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

&Scoped-define SELF-NAME Bancos.Cod_RemCobro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bancos.Cod_RemCobro V-table-Win
ON LEAVE OF Bancos.Cod_RemCobro IN FRAME F-Main /* Deducible Remesa Cobro */
DO:
  DEFINE VAR W_Codigo LIKE Deducible.Cod_Deducible.
  DEFINE VAR W_Nomded LIKE Deducible.Nom_Deducible.
  
  DO WITH FRAME {&FRAME-NAME}:
     IF Bancos.Cod_RemCobro:SCREEN-VALUE EQ "" THEN DO:
        RUN C-Deducibles.r(OUTPUT W_Codigo,OUTPUT W_Nomded).
        ASSIGN Bancos.Cod_RemCobro:SCREEN-VALUE = W_Codigo
               F_Nombre1:SCREEN-VALUE           = W_Nomded.
     END.
     ELSE DO:
        FIND Deducible WHERE Deducible.Cod_Deducible EQ Bancos.Cod_RemCobro:SCREEN-VALUE 
                       NO-LOCK NO-ERROR.
        IF AVAILABLE(Deducible) THEN DO:
           ASSIGN F_Nombre1:SCREEN-VALUE = Deducible.Nom_Deducible.
        END.
        ELSE DO:
           RUN C-Deducibles.r(OUTPUT W_Codigo,OUTPUT W_Nomded).
           ASSIGN Bancos.Cod_RemCobro:SCREEN-VALUE = W_Codigo
                  F_Nombre1:SCREEN-VALUE           = W_Nomded.
        END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bancos.Cod_RemNego
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bancos.Cod_RemNego V-table-Win
ON LEAVE OF Bancos.Cod_RemNego IN FRAME F-Main /* Deducible Remesa Negociada */
DO:
  DEFINE VAR W_Codigo LIKE Deducible.Cod_Deducible.
  DEFINE VAR W_Nomded LIKE Deducible.Nom_Deducible.
  
  DO WITH FRAME {&FRAME-NAME}:
     IF Bancos.Cod_RemNego:SCREEN-VALUE EQ "" THEN DO:
        RUN C-Deducibles.r(OUTPUT W_Codigo,OUTPUT W_Nomded).
        ASSIGN Bancos.Cod_RemNego:SCREEN-VALUE = W_Codigo
               F_Nombre2:SCREEN-VALUE           = W_Nomded.
     END.
     ELSE DO:
        FIND Deducible WHERE Deducible.Cod_Deducible EQ Bancos.Cod_RemNego:SCREEN-VALUE 
                       NO-LOCK NO-ERROR.
        IF AVAILABLE(Deducible) THEN DO:
           ASSIGN F_Nombre2:SCREEN-VALUE = Deducible.Nom_Deducible.
        END.
        ELSE DO:
           RUN C-Deducibles.r(OUTPUT W_Codigo,OUTPUT W_Nomded).
           ASSIGN Bancos.Cod_RemNego:SCREEN-VALUE = W_Codigo
                  F_Nombre2:SCREEN-VALUE          = W_Nomded.
        END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bancos.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bancos.Nit V-table-Win
ON LEAVE OF Bancos.Nit IN FRAME F-Main /* Nit */
DO:
 DO WITH FRAME {&FRAME-NAME}:
     FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Bancos.Nombre:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        ASSIGN Bancos.Nombre:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
               SELF:SCREEN-VALUE = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion NE "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Juridica" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bancos.Nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bancos.Nombre V-table-Win
ON LEAVE OF Bancos.Nombre IN FRAME F-Main /* Nombre */
DO:
  Bancos.Nombre:SCREEN-VALUE = CAPS(Bancos.Nombre:SCREEN-VALUE).
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
  {src/adm/template/row-list.i "Bancos"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Bancos"}

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
RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  W_Nuevo = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
     IF INTEGER(Bancos.Cod_Compensa:SCREEN-VALUE) = 0 OR
        Bancos.Nombre:SCREEN-VALUE                = "" THEN DO:
        MESSAGE "No Debe Grabar Banco en Cero o Nombre en Blanco."
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
        TITLE "Error al Grabar". 
     END.
     ELSE DO:
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).
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

  DO WITH FRAME {&FRAME-NAME}:
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
     FIND Deducible WHERE Deducible.Cod_Deducible EQ Bancos.Cod_RemCobro NO-LOCK NO-ERROR.
     IF AVAILABLE(Deducible) THEN DO:
        ASSIGN F_Nombre1:SCREEN-VALUE = Deducible.Nom_Deducible.
     END.
     FIND Deducible WHERE Deducible.Cod_Deducible EQ Bancos.Cod_RemNego  NO-LOCK NO-ERROR.
     IF AVAILABLE(Deducible) THEN DO:
        ASSIGN F_Nombre2:SCREEN-VALUE = Deducible.Nom_Deducible.
     END.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  IF W_Nuevo THEN
    RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Bancos. Codigo: " + STRING(Bancos.Cod_Compensa)).
  ELSE
    RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Bancos. Codigo: " + STRING(Bancos.Cod_Compensa)).
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
  {src/adm/template/snd-list.i "Bancos"}

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

