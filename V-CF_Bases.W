&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{Incluido\Variable.I "SHARED"}
   DEFINE VAR W_Pcodigo    LIKE Varios.Codigo.
   DEFINE VAR W_Pcuenta    LIKE Cuenta.Cuenta.
   DEFINE VAR W_Pnombre    LIKE Cuenta.Nombre.
   DEFINE VAR W_Pnomcta    LIKE Cuenta.Nombre.
   DEFINE VAR W_Naturaleza LIKE Cuenta.Naturaleza.
   DEFINE VAR W_ctrnat     LIKE Cuentas.Ctr_Naturaleza.
   DEFINE VAR W_HayError   AS   LOGICAL.
   DEFINE VAR W_ok         AS   LOGICAL.  /* Items del combo-box */ 
   DEFINE VAR W_ctasal     LIKE Cuentas.Cuenta.
   DEFINE VAR W_posicion   AS INTEGER INITIAL 0.
   DEFINE VAR W_nroreg     AS INTEGER INITIAL 0.

   DEFINE VAR Inactiva AS LOGICAL INITIAL NO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Grupo

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES Base_Ret
&Scoped-define FIRST-EXTERNAL-TABLE Base_Ret


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Base_Ret.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Base_Ret.Cod_base Base_Ret.Nombre ~
Base_Ret.Fecha Base_Ret.Porcentaje Base_Ret.Cuenta Base_Ret.Tope ~
Base_Ret.Estado 
&Scoped-define ENABLED-TABLES Base_Ret
&Scoped-define FIRST-ENABLED-TABLE Base_Ret
&Scoped-Define DISPLAYED-FIELDS Base_Ret.Cod_base Base_Ret.Nombre ~
Base_Ret.Fecha Base_Ret.Porcentaje Base_Ret.Cuenta Base_Ret.Tope ~
Base_Ret.Estado 
&Scoped-define DISPLAYED-TABLES Base_Ret
&Scoped-define FIRST-DISPLAYED-TABLE Base_Ret
&Scoped-Define DISPLAYED-OBJECTS NomCta 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE NomCta AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Grupo
     Base_Ret.Cod_base AT ROW 1.54 COL 24 COLON-ALIGNED
          LABEL "Código"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Base_Ret.Nombre AT ROW 2.62 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 43 BY .81
          BGCOLOR 15 
     Base_Ret.Fecha AT ROW 3.69 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Base_Ret.Porcentaje AT ROW 4.77 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Base_Ret.Cuenta AT ROW 5.85 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.43 BY .81
          BGCOLOR 15 
     NomCta AT ROW 5.85 COL 40 COLON-ALIGNED NO-LABEL
     Base_Ret.Tope AT ROW 6.92 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.43 BY .81
          BGCOLOR 15 
     Base_Ret.Estado AT ROW 8 COL 26 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 22 BY 1.08
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: bdCentral.Base_Ret
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
         HEIGHT             = 8.46
         WIDTH              = 70.72.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Grupo:SCROLLABLE       = FALSE
       FRAME F-Grupo:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Base_Ret.Cod_base IN FRAME F-Grupo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN NomCta IN FRAME F-Grupo
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Grupo
/* Query rebuild information for FRAME F-Grupo
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Grupo */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Base_Ret.Cuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Base_Ret.Cuenta V-table-Win
ON LEAVE OF Base_Ret.Cuenta IN FRAME F-Grupo /* Cuenta */
DO:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN
     NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
            NomCta:SCREEN-VALUE = W_Pnombre.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Base_Ret.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Base_Ret.Estado V-table-Win
ON VALUE-CHANGED OF Base_Ret.Estado IN FRAME F-Grupo /* Estado */
DO:
  IF SELF:SCREEN-VALUE EQ "2" THEN Inactiva = YES.
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
  {src/adm/template/row-list.i "Base_Ret"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Base_Ret"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  DISPLAY CURRENT-VALUE(Sec_Grupos) + 1 @ Grupos.Grupo WITH FRAME {&FRAME-NAME}.
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
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN
      ASSIGN W_Evento = "Ingresar".
  IF W_Evento EQ "Ingresar" THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Bases. Codigo: " + STRING(Base_Ret.Cod_Base)).
  ELSE DO:
    IF Inactiva THEN DO:
      RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Bases. Codigo: " + STRING(Base_Ret.Cod_Base)).
      Inactiva = NO.
    END.
    ELSE
      RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Bases. Codigo: " + STRING(Base_Ret.Cod_Base)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
DEFINE VAR W_Rpta AS LOGICAL.
  RUN MostrarMensaje IN W_Manija (INPUT 270,OUTPUT W_Rpta).
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
RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  IF AVAILABLE Base_Ret AND Base_Ret.Cuenta NE "" THEN 
     APPLY "leave" TO Base_Ret.Cuenta IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/snd-list.i "Base_Ret"}

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

