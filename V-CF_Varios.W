&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{INCLUIDO\VARIABLE.I "SHARED"}

    DEFINE VARIABLE W_CodCbte  LIKE Varios.Comprobante.
    DEFINE VARIABLE W_Progra   LIKE Varios.Programa.
    DEFINE VARIABLE W_Evento     AS CHARACTER.
    DEFINE VARIABLE W_Rta        AS LOGICAL.
    DEFINE VARIABLE W_Obliga     AS LOGICAL.
    DEFINE VARIABLE W_Rowid      AS ROWID.
    DEFINE VARIABLE W_SwError    AS LOGICAL.
    DEFINE VARIABLE W_PrioriAnt  AS INTEGER FORMAT "99".

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
&Scoped-define EXTERNAL-TABLES Varios
&Scoped-define FIRST-EXTERNAL-TABLE Varios


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Varios.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Varios.Estado Varios.Descripcion ~
Varios.Id_Gestion Varios.Comentario 
&Scoped-define ENABLED-TABLES Varios
&Scoped-define FIRST-ENABLED-TABLE Varios
&Scoped-Define ENABLED-OBJECTS W_CmbCbte 
&Scoped-Define DISPLAYED-FIELDS Varios.Codigo Varios.Estado ~
Varios.Descripcion Varios.Comprobante Varios.Id_Gestion Varios.Val_Inicial ~
Varios.Val_Final Varios.Comentario 
&Scoped-define DISPLAYED-TABLES Varios
&Scoped-define FIRST-DISPLAYED-TABLE Varios
&Scoped-Define DISPLAYED-OBJECTS W_Cmbtipo Cmb_CodPrograma W_CmbCbte 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS W_Cmbtipo 
&Scoped-define ADM-ASSIGN-FIELDS Varios.Codigo Varios.Tipo ~
Varios.Comprobante 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE Cmb_CodPrograma AS CHARACTER FORMAT "X(40)":U 
     LABEL "Programa" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "0000000000 - No Configurado" 
     DROP-DOWN-LIST
     SIZE 48 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CmbCbte AS CHARACTER FORMAT "X(25)":U 
     LABEL "Comprobantes" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cmbtipo AS CHARACTER FORMAT "X(80)":U 
     LABEL "Tipo" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 48 BY 1
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_Cmbtipo AT ROW 1.27 COL 10 COLON-ALIGNED
     Varios.Codigo AT ROW 2.35 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY .88
          BGCOLOR 18 FGCOLOR 15 
     Varios.Tipo AT ROW 2.35 COL 31 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     Varios.Estado AT ROW 2.35 COL 41 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 17 BY .81
          BGCOLOR 17 
     Varios.Descripcion AT ROW 3.42 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 48 BY .81
          BGCOLOR 15 
     Cmb_CodPrograma AT ROW 4.5 COL 10 COLON-ALIGNED
     W_CmbCbte AT ROW 5.58 COL 10 COLON-ALIGNED
     Varios.Comprobante AT ROW 5.58 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Varios.Id_Gestion AT ROW 7.19 COL 4
          LABEL "Tiene Gestion?"
          VIEW-AS TOGGLE-BOX
          SIZE 13.57 BY .77 TOOLTIP "Tiene Gestion?"
     Varios.Val_Inicial AT ROW 7.73 COL 37 COLON-ALIGNED
          LABEL "Valor Inicial"
          VIEW-AS FILL-IN 
          SIZE 21 BY .81
          BGCOLOR 15 
     Varios.Val_Final AT ROW 8.81 COL 37 COLON-ALIGNED
          LABEL "Valor Final"
          VIEW-AS FILL-IN 
          SIZE 21 BY .81
          BGCOLOR 15 
     Varios.Comentario AT ROW 10.96 COL 3 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 56 BY 3.5
          BGCOLOR 15 
     "Rangos de Valores" VIEW-AS TEXT
          SIZE 19 BY .77 AT ROW 6.92 COL 39
          FGCOLOR 7 FONT 5
     "Comentario" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 9.88 COL 3
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Varios
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
         HEIGHT             = 13.62
         WIDTH              = 62.29.
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

/* SETTINGS FOR COMBO-BOX Cmb_CodPrograma IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Varios.Codigo IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN Varios.Comprobante IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX Varios.Id_Gestion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Varios.Tipo IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L 2                                       */
ASSIGN 
       Varios.Tipo:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN Varios.Val_Final IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Varios.Val_Inicial IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR COMBO-BOX W_Cmbtipo IN FRAME F-Main
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

&Scoped-define SELF-NAME Cmb_CodPrograma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_CodPrograma V-table-Win
ON VALUE-CHANGED OF Cmb_CodPrograma IN FRAME F-Main /* Programa */
DO:
    DO WITH FRAME {&FRAME-NAME}:
       ASSIGN W_Progra = INTEGER(SUBSTRING(Cmb_CodPrograma:SCREEN-VALUE,1,10)).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Varios.Codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Varios.Codigo V-table-Win
ON LEAVE OF Varios.Codigo IN FRAME F-Main /* Codigo */
DO:
    {Incluido\Btcancel.i}
    FIND Varios WHERE Varios.Tipo   EQ INTEGER(Varios.Tipo:SCREEN-VALUE)
                  AND Varios.Codigo EQ INPUT Varios.Codigo 
                NO-LOCK NO-ERROR.
    IF AVAILABLE(Varios) THEN DO:
       RUN MostrarMensaje IN W_Manija (INPUT 60, OUTPUT W_Eleccion).
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbCbte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbCbte V-table-Win
ON VALUE-CHANGED OF W_CmbCbte IN FRAME F-Main /* Comprobantes */
DO:
    W_CodCbte        = SELF:LOOKUP(SELF:SCREEN-VALUE) IN FRAME {&FRAME-NAME}.
    ASSIGN W_CodCbte = INTEGER(SUBSTRING(SELF:ENTRY(W_CodCbte),1,2)).
    DISPLAY W_CodCbte @ Varios.Comprobante WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Cmbtipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cmbtipo V-table-Win
ON VALUE-CHANGED OF W_Cmbtipo IN FRAME F-Main /* Tipo */
DO:
  Varios.Tipo:SCREEN-VALUE = SUBSTRING(W_CmbTipo:SCREEN-VALUE,1,3).
  DO WITH FRAME {&FRAME-NAME}:
       FIND LAST Varios WHERE Varios.Tipo  EQ INTEGER(SUBSTRING(Varios.Tipo:SCREEN-VALUE,1,3))
                        NO-LOCK NO-ERROR.
       IF AVAILABLE(Varios) THEN
          Varios.Codigo:SCREEN-VALUE = STRING(Varios.Codigo + 1).
       ELSE
          Varios.Codigo:SCREEN-VALUE = "1".
    
    FIND Cfg_Varios WHERE Cfg_Varios.Tipo EQ INTEGER(SUBSTRING(W_CmbTipo:SCREEN-VALUE,1,3)) AND 
         Cfg_Varios.Id_Proceso EQ YES NO-LOCK NO-ERROR.
    IF AVAILABLE Cfg_Varios THEN ENABLE Cmb_CodPrograma.
    ELSE DISABLE Cmb_CodPrograma.
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
  {src/adm/template/row-list.i "Varios"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Varios"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar V-table-Win 
PROCEDURE Grabar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAILABLE(Varios) THEN DO WITH FRAME {&FRAME-NAME}:
       IF W_Progra GT 0 THEN 
          ASSIGN Varios.Programa = W_Progra.
       ELSE
          ASSIGN Varios.Programa = 0.
    END.
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
    DO WITH FRAME {&FRAME-NAME}:
       FIND LAST Varios WHERE Varios.Tipo  EQ INTEGER(SUBSTRING(W_CmbTipo:SCREEN-VALUE,1,3))
                        NO-LOCK NO-ERROR.
       IF AVAILABLE(Varios) THEN
          Varios.Codigo:SCREEN-VALUE = STRING(Varios.Codigo + 1).
       ELSE
          Varios.Codigo:SCREEN-VALUE = "1".
    END.
    /*{Incluido\Btcancel.i}*/


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
  ASSIGN FRAME {&FRAME-NAME} W_CmbTipo.
  Varios.Tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(W_CmbTipo,1,3).
  IF INTEGER(Varios.Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME}) EQ 2 THEN DO:
      MESSAGE "Inactivar Registro Puede Causar Errores en Diferentes" SKIP
              "Configuraciones.  Esta Seguro de Inactivar?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Advertencia" UPDATE choice AS LOGICAL.
      IF CHOICE THEN DO:
         RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
         RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Varios. Tipo: " + 
             STRING(W_CmbTipo) + " - Codigo: " + STRING(Varios.Codigo)).
         RUN Grabar.
      END.
      ELSE DO:
          RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
      END.
         
   END.
   ELSE DO:
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) . 
      ASSIGN Varios.Val_Inicial = DECIMAL(Varios.Val_Inicial:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
             Varios.Val_Final = DECIMAL(Varios.Val_Final:SCREEN-VALUE IN FRAME {&FRAME-NAME}). 
      RUN Grabar.
      RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Varios. Tipo: " + 
          STRING(W_CmbTipo) + " - Codigo: " + STRING(Varios.Codigo)).
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    MESSAGE "No se Permite Borrar Registros" SKIP
            "en Tipos. Inactivelos."
            VIEW-AS ALERT-BOX INFORMATION TITLE "Error al Borrar".
            
   /*    DEFINE VAR W_PrioriSig    AS INTEGER FORMAT "99".
 *     W_PrioriAnt = 0.
 *     IF Varios.id_Prioridad = TRUE THEN
 *        W_PrioriAnt = Varios.Nro_Prioridad.
 *     W_PrioriSig = W_PrioriAnt + 1.
 *     RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
 *     W_Rowid = ROWID(Varios).
 *     FOR EACH Varios FIELDS(Varios.Tipo Varios.Id_Prioridad Varios.Nro_Prioridad)
 *                     WHERE Varios.Tipo EQ Varios.Tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
 *                       AND Varios.Id_Prioridad = TRUE SHARE-LOCK:
 *                       IF Varios.Nro_Prioridad > W_PrioriAnt THEN DO:
 *                          ASSIGN Varios.Nro_Prioridad = W_PrioriAnt
 *                                 W_PrioriAnt          = W_PrioriAnt +  1.   
 *                           END.
 *     END.
 *     FIND Varios WHERE ROWID(Varios) = W_Rowid NO-LOCK NO-ERROR.*/            

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
------------------------------------------------------------------------------*/.
  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
    
    IF AVAILABLE(Varios) THEN DO WITH FRAME {&FRAME-NAME}:
       FIND Cfg_Varios WHERE Cfg_Varios.Tipo EQ Varios.Tipo NO-LOCK NO-ERROR.
       IF AVAILABLE(Cfg_Varios) THEN  DO:
          IF Cfg_Varios.Id_Rango THEN DO:
             ENABLE Varios.Val_Inicial Val_Final WITH FRAME {&FRAME-NAME}.
             ASSIGN Varios.Val_Inicial:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Varios.Val_Inicial)
                    Varios.Val_Final:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Varios.Val_Final).
          END.
          ELSE DO:
             DISABLE Varios.Val_Inicial Val_Final WITH FRAME {&FRAME-NAME}.
             ASSIGN Varios.Val_Inicial:SCREEN-VALUE = "0"
                    Varios.Val_Final:SCREEN-VALUE = "0".
          END.
          W_CmbTipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Cfg_Varios.Tipo,"999") + " - " + Cfg_Varios.Descripcion.
          IF Cfg_Varios.Id_Proceso THEN 
             ENABLE Cmb_CodPrograma WITH FRAME {&FRAME-NAME}.
          ELSE 
             DISABLE Cmb_CodPrograma WITH FRAME {&FRAME-NAME}.
          ASSIGN W_CmbTipo:SCREEN-VALUE = STRING(Cfg_Varios.Tipo,"999") + " - " + Cfg_Varios.Descripcion.
       END.
       
 
       FIND Comprobantes WHERE Comprobantes.Comprobante EQ Varios.Comprobante
                           AND Comprobantes.Agencia EQ W_Agencia
                         NO-LOCK NO-ERROR.
       IF AVAILABLE(Comprobantes) THEN 
          ASSIGN W_CmbCbte:SCREEN-VALUE  = (STRING(Comprobantes.Comprobante,"99")
                                           + "-" + STRING(Comprobantes.Nombre,"X(21)")). 
       ELSE
          ASSIGN W_CmbCbte:SCREEN-VALUE  = ("00-NINGUNO").                   
    END.
    IF Varios.Programa NE 0 THEN DO:
       FIND Programas WHERE Programas.Programa EQ Varios.Programa AND Programas.Tipo        EQ TRUE AND
                            Programas.Estado   EQ 1               AND Programas.Id_Procesos EQ TRUE AND
                            Programas.Proceso  EQ 3 NO-LOCK NO-ERROR.
       IF AVAILABLE(Programas) THEN 
          ASSIGN Cmb_CodPrograma:SCREEN-VALUE = STRING(Programas.Programa,"9999999999") + " - " + STRING(Programas.Opcion)
                 W_Progra                     = Varios.Programa.
    END.
    ELSE Cmb_CodPrograma:SCREEN-VALUE = Cmb_CodPrograma:ENTRY(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN W_CmbCbte:LIST-ITEMS  = ""
           W_Cmbtipo:LIST-ITEMS  = "".
        
    W_Rta = W_CmbCbte:ADD-LAST("00-NINGUNO").
        
    FOR EACH Programas FIELDS(Programas.Tipo Programas.Estado Programas.Id_Procesos Programas.Proceso Programas.Programa Programas.Opcion)
        WHERE Programas.Tipo EQ TRUE
          AND Programas.Estado EQ 1
          AND Programas.Id_Procesos EQ TRUE
          AND Programas.Proceso EQ 3 NO-LOCK:
                ASSIGN W_Eleccion = Cmb_CodPrograma:ADD-LAST(STRING(Programas.Programa,"9999999999") + " - " + STRING(Programas.Opcion)).
    END.
        
    FOR EACH Comprobantes FIELDS(Comprobantes.Comprobante Comprobantes.Nombre Comprobantes.Agencia Comprobantes.Estado)
        WHERE Comprobantes.Estado EQ 1
          AND Comprobantes.Agencia EQ W_Agencia NO-LOCK:
                W_Rta = W_CmbCbte:ADD-LAST( STRING(Comprobantes.Comprobante,"99") + "-" + STRING(Comprobantes.Nombre,"X(21)")).
    END.
        
    IF W_CmbCbte:NUM-ITEMS NE 0 THEN
        W_CmbCbte:SCREEN-VALUE = W_CmbCbte:ENTRY(1).
        
    FOR EACH Cfg_Varios FIELDS(Cfg_Varios.Tipo Cfg_Varios.Descripcion) NO-LOCK:
        W_Rta = W_CmbTipo:ADD-LAST(STRING(Cfg_Varios.Tipo,"999") + " - " + Cfg_Varios.Descripcion).
    END.
        
    IF W_CmbTipo:NUM-ITEMS NE 0 THEN
        W_CmbTipo:SCREEN-VALUE = W_CmbTipo:ENTRY(1).
        
    ASSIGN Varios.Tipo:SCREEN-VALUE = SUBSTRING(W_CmbTipo:ENTRY(1),1,3).
END.
    
RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

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
  {src/adm/template/snd-list.i "Varios"}

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

