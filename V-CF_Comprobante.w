&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
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
&Scoped-define EXTERNAL-TABLES Comprobantes
&Scoped-define FIRST-EXTERNAL-TABLE Comprobantes


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Comprobantes.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Comprobantes.Nombre Comprobantes.Estado ~
Comprobantes.ReiniciaCierre Comprobantes.Id_Efectivo ~
Comprobantes.Id_Consecutivo 
&Scoped-define ENABLED-TABLES Comprobantes
&Scoped-define FIRST-ENABLED-TABLE Comprobantes
&Scoped-Define ENABLED-OBJECTS RECT-29 W_Cmbformato 
&Scoped-Define DISPLAYED-FIELDS Comprobantes.Fec_Creacion ~
Comprobantes.Fec_Retiro Comprobantes.Comprobante Comprobantes.Nombre ~
Comprobantes.Estado Comprobantes.ReiniciaCierre Comprobantes.Id_Efectivo ~
Comprobantes.Id_Consecutivo Comprobantes.Num_Inicial Comprobantes.Secuencia 
&Scoped-define DISPLAYED-TABLES Comprobantes
&Scoped-define FIRST-DISPLAYED-TABLE Comprobantes
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_Cmbformato 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS W_CmbOfi Comprobantes.Comprobante ~
Comprobantes.Num_Inicial 
&Scoped-define ADM-ASSIGN-FIELDS Comprobantes.Fec_Creacion ~
Comprobantes.Fec_Retiro Comprobantes.Cod_Formato Comprobantes.agencia ~
Comprobantes.Secuencia 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE W_Cmbformato AS CHARACTER FORMAT "X(40)":U 
     LABEL "Formato" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 34 BY 1
     BGCOLOR 15 FGCOLOR 0 FONT 4 NO-UNDO.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(20)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 34 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 3.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_CmbOfi AT ROW 2.88 COL 15 COLON-ALIGNED
     Comprobantes.Fec_Creacion AT ROW 2.88 COL 68 COLON-ALIGNED
          LABEL "Fecha de Creación"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     Comprobantes.Fec_Retiro AT ROW 3.96 COL 68 COLON-ALIGNED
          LABEL "Fecha de Retiro"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Comprobantes.Comprobante AT ROW 4.04 COL 15 COLON-ALIGNED
          LABEL "Comprobante"
          VIEW-AS FILL-IN 
          SIZE 34 BY .81
          BGCOLOR 15 
     Comprobantes.Nombre AT ROW 5.12 COL 15 COLON-ALIGNED DEBLANK  AUTO-RETURN 
          VIEW-AS FILL-IN 
          SIZE 34 BY .81
          BGCOLOR 15 
     Comprobantes.Estado AT ROW 6.12 COL 30 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 20 BY 1.08
          BGCOLOR 17 
     Comprobantes.ReiniciaCierre AT ROW 7.73 COL 17 WIDGET-ID 2
          LABEL "Reinicia secuencia cada mes"
          VIEW-AS TOGGLE-BOX
          SIZE 34 BY .77
     Comprobantes.Id_Efectivo AT ROW 8.54 COL 17
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .77
     Comprobantes.Cod_Formato AT ROW 9.08 COL 71 COLON-ALIGNED
          LABEL "Formato"
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     W_Cmbformato AT ROW 9.88 COL 15 COLON-ALIGNED
     Comprobantes.agencia AT ROW 10.42 COL 71.14 COLON-ALIGNED
          LABEL "Agencia"
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     Comprobantes.Id_Consecutivo AT ROW 11.23 COL 13 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Automático", 1,
"Consecutivo", 2,
"No Maneja", 3
          SIZE 35 BY 1.73
          BGCOLOR 17 FONT 4
     Comprobantes.Num_Inicial AT ROW 13.38 COL 33 COLON-ALIGNED
          LABEL "Numero Inicial"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 FGCOLOR 0 FONT 4
     Comprobantes.Secuencia AT ROW 14.46 COL 33 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 16 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     "Información General" VIEW-AS TEXT
          SIZE 18 BY .5 AT ROW 1.81 COL 9
          FGCOLOR 7 FONT 5
     RECT-29 AT ROW 12.04 COL 11
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Comprobantes
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
         HEIGHT             = 16.81
         WIDTH              = 91.43.
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

/* SETTINGS FOR FILL-IN Comprobantes.agencia IN FRAME F-Main
   NO-DISPLAY NO-ENABLE 2 EXP-LABEL                                     */
ASSIGN 
       Comprobantes.agencia:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN Comprobantes.Cod_Formato IN FRAME F-Main
   NO-DISPLAY NO-ENABLE 2 EXP-LABEL                                     */
ASSIGN 
       Comprobantes.Cod_Formato:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN Comprobantes.Comprobante IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Comprobantes.Fec_Creacion IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Comprobantes.Fec_Retiro IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Comprobantes.Num_Inicial IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX Comprobantes.ReiniciaCierre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Comprobantes.Secuencia IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR COMBO-BOX W_CmbOfi IN FRAME F-Main
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

&Scoped-define SELF-NAME Comprobantes.Comprobante
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Comprobantes.Comprobante V-table-Win
ON LEAVE OF Comprobantes.Comprobante IN FRAME F-Main /* Comprobante */
DO:
  DEFINE BUFFER Tmp_Comprobantes FOR Comprobantes.
  
  FIND Tmp_Comprobantes WHERE Tmp_Comprobantes.Agencia EQ W_OfiTbajo 
                          AND Tmp_Comprobantes.Comprobante EQ INTEGER(Comprobantes.Comprobante:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.               
  IF AVAILABLE Tmp_Comprobantes THEN DO:
     MESSAGE "El Código que desea crear ya se encuentra matrículado. Verifique"
         VIEW-AS ALERT-BOX.
     APPLY "ENTRY" TO Comprobantes.Comprobante IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Comprobantes.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Comprobantes.Estado V-table-Win
ON VALUE-CHANGED OF Comprobantes.Estado IN FRAME F-Main /* Estado */
OR RETURN OF Comprobantes.Estado DO:

   IF  Comprobantes.Estado:SCREEN-VALUE EQ STRING(2)
   AND Comprobantes.Fec_Retiro          EQ ? THEN DO:
       FIND FIRST Varios WHERE Tipo               EQ 8
                           AND Varios.Comprobante EQ 
                               INTEGER(Comprobantes.Comprobante:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                         NO-LOCK NO-ERROR.
       IF AVAILABLE Varios THEN DO:
          RUN MostrarMensaje IN W_Manija (INPUT 382, OUTPUT W_Estado).
          ASSIGN Comprobantes.Estado:SCREEN-VALUE = STRING(1).
          RETURN NO-APPLY.
       END.  
       ASSIGN Comprobantes.Fec_Retiro:SCREEN-VALUE = STRING(TODAY).
   END.
   ELSE DO:
      IF Comprobantes.Estado:SCREEN-VALUE = STRING(1) THEN
         ASSIGN Comprobantes.Fec_Retiro:SCREEN-VALUE = ?.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Comprobantes.Id_Consecutivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Comprobantes.Id_Consecutivo V-table-Win
ON VALUE-CHANGED OF Comprobantes.Id_Consecutivo IN FRAME F-Main /* Consecutivo */
OR RETURN OF Comprobantes.Id_Consecutivo DO:

   ASSIGN Comprobantes.Secuencia:HIDDEN = FALSE.
   IF Comprobantes.Id_Consecutivo:SCREEN-VALUE EQ STRING(3) THEN
      ASSIGN Comprobantes.Secuencia:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Comprobantes.Num_Inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Comprobantes.Num_Inicial V-table-Win
ON LEAVE OF Comprobantes.Num_Inicial IN FRAME F-Main /* Numero Inicial */
DO:
  IF Comprobantes.Id_Consecutivo:SCREEN-VALUE NE STRING(3) THEN
     ASSIGN Comprobantes.Secuencia:SCREEN-VALUE = Comprobantes.Num_Inicial:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Cmbformato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cmbformato V-table-Win
ON VALUE-CHANGED OF W_Cmbformato IN FRAME F-Main /* Formato */
DO:
  IF SELF:SCREEN-VALUE EQ ? THEN
     RETURN.
     
  ASSIGN W_CodFormato= INTEGER(ENTRY(1,SELF:SCREEN-VALUE, "-"))
         Comprobantes.Cod_Formato:SCREEN-VALUE = STRING(W_CodFormato).
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
          Comprobantes.Agencia:SCREEN-VALUE = STRING(W_OfiTbajo).
  
   ASSIGN W_CmbFormato:LIST-ITEMS = ""
          W_Estado = W_CmbFormato:ADD-LAST("00-NINGUNO") IN FRAME {&FRAME-NAME}.
   FOR EACH Formatos WHERE Formatos.Estado  EQ 1 
                       AND Formatos.Agencia EQ W_OfiTbajo NO-LOCK
                        BY Formatos.Cod_Formato:
       ASSIGN W_String = STRING(Formatos.Cod_Formato,"99") + "-" + Formatos.Nom_formato
              W_Estado = W_CmbFormato:ADD-LAST(W_String).
       IF Formatos.Cod_Formato EQ W_CodFormato THEN
          ASSIGN W_CmbFormato:SCREEN-VALUE = W_String.
   END.

   IF W_CmbFormato:SCREEN-VALUE EQ ""
   OR W_CmbFormato:SCREEN-VALUE EQ ? THEN DO:
      IF W_CmbFormato:NUM-ITEMS GT 0 THEN
         ASSIGN W_CmbFormato:SCREEN-VALUE = W_CmbFormato:ENTRY(1).
   END.
   APPLY "VALUE-CHANGED":U TO W_CmbFormato.
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
  {src/adm/template/row-list.i "Comprobantes"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Comprobantes"}

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
  DEFINE BUFFER Tmp_Usuarios FOR Usuarios.
  
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ).

  FIND Tmp_Usuarios WHERE Tmp_Usuarios.Agencia   EQ W_Agencia 
                      AND Tmp_Usuarios.Id_OpeOfi EQ TRUE 
                      AND Tmp_Usuarios.Usuario   EQ W_Usuario 
                      AND Tmp_Usuarios.Prioridad GT 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Tmp_Usuarios THEN
     ASSIGN W_CmbOfi:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

  ASSIGN Comprobantes.Id_consecutivo:SCREEN-VALUE = STRING(1)
         Comprobantes.Secuencia:SCREEN-VALUE      = STRING(0)
         Comprobantes.Fec_Creacion:SCREEN-VALUE   = STRING(TODAY).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE W_Evento AS CHARACTER.

  IF Comprobantes.Comprobante:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "00" 
  OR Comprobantes.Nombre:SCREEN-VALUE      EQ ""
  OR Comprobantes.Nombre:SCREEN-VALUE      EQ ?   THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 204, OUTPUT W_Eleccion).
     RUN notify ('cancel-record':U).
     RETURN ERROR.
  END.
 
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).
   
  ASSIGN W_Evento = "MODIFICAR".
  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN
     ASSIGN W_Evento = "INGRESAR".
  IF W_Evento EQ "INGRESAR" THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Comprobantes. Age: " + STRING(Comprobantes.Agencia)
         + " - Com: " + STRING(Comprobantes.Comprobante)).
  ELSE
     RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Comprobantes. Age: " + STRING(Comprobantes.Agencia)
         + " - Com: " + STRING(Comprobantes.Comprobante)).
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
  FIND FIRST Varios WHERE Tipo               EQ 8
                      AND Varios.Comprobante EQ 
                          INTEGER(Comprobantes.Comprobante:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                    NO-LOCK NO-ERROR.
  IF AVAILABLE Varios THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 382, OUTPUT W_Estado).
     RUN notify ('cancel-record':U).
     RETURN ERROR.
  END.  

  /* Dispatch standard ADM method.                             */
  
/* RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) . */
     
  /* Code placed here will execute AFTER standard behavior.    */
  IF Comprobantes.Fec_Retiro NE ? THEN DO:
     RETURN.
  END.

  DO TRANSACTION:
     FIND CURRENT Comprobantes EXCLUSIVE-LOCK NO-ERROR.
     IF AVAILABLE Comprobantes THEN
        ASSIGN Comprobantes.Fec_Retiro = TODAY
               Comprobantes.Estado     = 2.
     RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Comprobantes. Age: " + STRING(Comprobantes.Agencia)
         + " - Com: " + STRING(Comprobantes.Comprobante)).
  END.
  FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
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
  ASSIGN W_OfiTbajo = W_Agencia.
  
  IF NOT AVAILABLE Comprobantes THEN DO:
     IF W_CmbOfi:NUM-ITEMS IN FRAME {&FRAME-NAME} GT 0 THEN
        ASSIGN W_CmbOfi:SCREEN-VALUE = W_CmbOfi:ENTRY(1).
     APPLY "VALUE-CHANGED":U TO W_CmbOfi.
     RETURN.
  END.

  FIND Agencias WHERE Agencia.Agencia EQ Comprobantes.Agencia NO-LOCK NO-ERROR.
  IF AVAILABLE Agencias THEN
     ASSIGN W_String = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre
            W_CmbOfi:SCREEN-VALUE = W_String.
  ELSE
     IF W_CmbOfi:NUM-ITEMS GT 0 THEN
        ASSIGN W_CmbOfi:SCREEN-VALUE = W_CmbOfi:ENTRY(1).

  ASSIGN W_CodFormato = Comprobantes.Cod_Formato.
  APPLY "VALUE-CHANGED":U TO W_CmbOfi.
  APPLY "VALUE-CHANGED":U TO Comprobantes.Id_Consecutivo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_CmbOfi:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK
                       BY Agencia.Agencia:
      ASSIGN W_String = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre
             W_Estado = W_CmbOfi:ADD-LAST(W_String).
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
  {src/adm/template/snd-list.i "Comprobantes"}

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

