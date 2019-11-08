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
  DEFINE SHARED VAR W_Ind     LIKE Indicadores.Indicador.
  DEFINE SHARED VAR W_Rango   LIKE Indicadores.Rango.
  DEFINE SHARED VAR W_Est     LIKE Indicadores.Estado.
  DEFINE SHARED VAR W_Fec     LIKE Indicadores.Fecha.
  DEFINE SHARED VAR W_FecVcto LIKE Indicadores.FecVcto.
  DEFINE VAR W_PlaIni         LIKE Ran_Intereses.Pla_Inicial INITIAL 0.
  DEFINE VAR W_PlaFin         LIKE Ran_Intereses.Pla_Final   INITIAL 0.
  DEFINE VAR W_VlrIni         LIKE Ran_Intereses.Val_Inicial INITIAL 0.
  DEFINE VAR W_VlrFin         LIKE Ran_Intereses.Val_Final   INITIAL 0.
  DEFINE VAR W_Rowid1         AS ROWID.
  DEFINE VAR W_Rpta           AS LOGICAL.
  DEFINE VAR W_Nuevo          AS LOGICAL.
  DEFINE VAR W_Ventana        AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-VRanInt

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES Ran_Intereses Indicadores
&Scoped-define FIRST-EXTERNAL-TABLE Ran_Intereses


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Ran_Intereses, Indicadores.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Ran_Intereses.Indicador Ran_Intereses.Estado ~
Ran_Intereses.Val_Inicial Ran_Intereses.Puntos Ran_Intereses.Pla_Inicial ~
Ran_Intereses.Puntos_Asoc Ran_Intereses.Val_Final ~
Ran_Intereses.Pun_Negociables Ran_Intereses.Pla_Final 
&Scoped-define ENABLED-TABLES Ran_Intereses
&Scoped-define FIRST-ENABLED-TABLE Ran_Intereses
&Scoped-Define ENABLED-OBJECTS RECT-126 Btn_ingRang Btn-salvar Btn-desh 
&Scoped-Define DISPLAYED-FIELDS Ran_Intereses.Indicador Indicadores.Nombre ~
Ran_Intereses.Estado Ran_Intereses.Val_Inicial Ran_Intereses.Puntos ~
Ran_Intereses.Pla_Inicial Ran_Intereses.Puntos_Asoc Ran_Intereses.Val_Final ~
Ran_Intereses.Pun_Negociables Ran_Intereses.Pla_Final Ran_Intereses.Fecha 
&Scoped-define DISPLAYED-TABLES Ran_Intereses Indicadores
&Scoped-define FIRST-DISPLAYED-TABLE Ran_Intereses
&Scoped-define SECOND-DISPLAYED-TABLE Indicadores


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
DEFINE BUTTON Btn-desh 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn-salvar 
     LABEL "&Salvar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_ingRang 
     LABEL "&Ingresar" 
     SIZE 10 BY 1.62.

DEFINE RECTANGLE RECT-126
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 4.85.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-VRanInt
     Ran_Intereses.Indicador AT ROW 1 COL 13.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     Indicadores.Nombre AT ROW 1 COL 22 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 57 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     Btn_ingRang AT ROW 1.54 COL 81
     Ran_Intereses.Estado AT ROW 2.08 COL 11 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 17 BY .81
          FONT 4
     Ran_Intereses.Val_Inicial AT ROW 3.15 COL 17 COLON-ALIGNED
          LABEL "Valor Inicial"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 FONT 4
     Ran_Intereses.Puntos AT ROW 3.15 COL 55 COLON-ALIGNED
          LABEL "Nomina"
          VIEW-AS FILL-IN 
          SIZE 22 BY .73
          BGCOLOR 15 FONT 4
     Btn-salvar AT ROW 3.42 COL 81
     Ran_Intereses.Pla_Inicial AT ROW 4.23 COL 17 COLON-ALIGNED
          LABEL "Plazo Inicial"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 FONT 4
     Ran_Intereses.Puntos_Asoc AT ROW 4.23 COL 55 COLON-ALIGNED
          LABEL "Caja"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 FONT 4
     Ran_Intereses.Val_Final AT ROW 5.31 COL 17 COLON-ALIGNED
          LABEL "Valor Final"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 FONT 4
     Ran_Intereses.Pun_Negociables AT ROW 5.31 COL 55 COLON-ALIGNED
          LABEL "Negociables"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 FONT 4
     Btn-desh AT ROW 5.31 COL 81
     Ran_Intereses.Pla_Final AT ROW 6.38 COL 17 COLON-ALIGNED
          LABEL "Plazo Final"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 FONT 4
     Ran_Intereses.Fecha AT ROW 7.46 COL 17 COLON-ALIGNED
          LABEL "Fecha"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 FONT 4
     "Puntos :" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.35 COL 44
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-126 AT ROW 2.62 COL 43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Ran_Intereses,datos.Indicadores
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
         HEIGHT             = 7.42
         WIDTH              = 93.
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
/* SETTINGS FOR FRAME F-VRanInt
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-VRanInt:SCROLLABLE       = FALSE
       FRAME F-VRanInt:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Ran_Intereses.Fecha IN FRAME F-VRanInt
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Indicadores.Nombre IN FRAME F-VRanInt
   NO-ENABLE ALIGN-L EXP-LABEL                                          */
/* SETTINGS FOR FILL-IN Ran_Intereses.Pla_Final IN FRAME F-VRanInt
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Ran_Intereses.Pla_Inicial IN FRAME F-VRanInt
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Ran_Intereses.Puntos IN FRAME F-VRanInt
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Ran_Intereses.Puntos_Asoc IN FRAME F-VRanInt
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Ran_Intereses.Pun_Negociables IN FRAME F-VRanInt
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Ran_Intereses.Val_Final IN FRAME F-VRanInt
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Ran_Intereses.Val_Inicial IN FRAME F-VRanInt
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-VRanInt
/* Query rebuild information for FRAME F-VRanInt
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-VRanInt */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-desh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-desh V-table-Win
ON CHOOSE OF Btn-desh IN FRAME F-VRanInt /* Cancelar */
DO:
  IF W_Nuevo THEN DO:
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinBB':U,OUTPUT W_Ventana).
     RUN P-HabRangos IN WIDGET-HANDLE (W_Ventana) (INPUT FALSE).  
  END.
  RUN dispatch IN THIS-PROCEDURE ('end-update':U).
  RUN notify IN THIS-PROCEDURE ('row-available':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-salvar V-table-Win
ON CHOOSE OF Btn-salvar IN FRAME F-VRanInt /* Salvar */
DO:
  DEFINE BUFFER W_Ran_Intereses FOR Ran_Intereses.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN W_PlaIni = DECIMAL(Ran_Intereses.Pla_Inicial:SCREEN-VALUE)
           W_PlaFin = DECIMAL(Ran_Intereses.Pla_Final:SCREEN-VALUE)
           W_VlrIni = DECIMAL(Ran_Intereses.Val_Inicial:SCREEN-VALUE)
           W_VlrFin = DECIMAL(Ran_Intereses.Val_Final:SCREEN-VALUE)
           W_Rowid1 = ROWID(Ran_Intereses).
    IF (W_PlaIni EQ 0 AND W_PlaFin EQ 0  AND W_VlrIni EQ 0 AND W_VlrFin EQ 0) 
    OR (W_PlaIni GT W_PlaFin)
    OR (W_VlrIni GT W_VlrFin) THEN DO:
       RUN MostrarMensaje IN W_Manija (INPUT 68,OUTPUT W_Rpta).
       APPLY "ENTRY" TO Ran_Intereses.Estado.
       RETURN NO-APPLY.
    END.
    FIND FIRST W_Ran_Intereses    WHERE (    W_Rowid1     NE ROWID(W_Ran_Intereses)
                                         AND W_Agencia    EQ W_Ran_Intereses.Agencia
                                         AND W_Ind        EQ W_Ran_Intereses.Indicador
                                         AND 1            EQ W_Ran_Intereses.Estado)
                                    AND (   (    W_PlaIni GE W_Ran_Intereses.Pla_Inicial     
                                             AND W_PlaIni LE W_Ran_Intereses.Pla_Final)  
                                         OR (    W_PlaFin GE W_Ran_Intereses.Pla_Inicial     
                                             AND W_PlaFin LE W_Ran_Intereses.Pla_Final)  
                                         OR (    W_PlaIni LE W_Ran_Intereses.Pla_Inicial     
                                             AND W_PlaFin GE W_Ran_Intereses.Pla_Final))
                                   AND(     (    W_VlrIni GE W_Ran_Intereses.Val_Inicial     
                                             AND W_VlrIni LE W_Ran_Intereses.Val_Final)
                                         OR (    W_VlrFin GE W_Ran_Intereses.Val_Inicial     
                                             AND W_VlrFin LE W_Ran_Intereses.Val_Final)
                                         OR (    W_VlrIni LE W_Ran_Intereses.Val_Inicial     
                                             AND W_VlrFin GE W_Ran_Intereses.Val_Final))                                
                                  NO-LOCK NO-ERROR.
                                
      IF AVAILABLE(W_Ran_Intereses) THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 305,OUTPUT W_Rpta).
        FIND Ran_Intereses WHERE ROWID(Ran_Intereses) EQ W_Rowid1 NO-LOCK NO-ERROR.
        APPLY "ENTRY" TO Ran_Intereses.Estado.
        RETURN NO-APPLY.
      END.
  END.
  IF W_Nuevo THEN DO:
     CREATE Ran_Intereses.
  END.
  ELSE  
     FIND Ran_Intereses WHERE ROWID(Ran_Intereses) EQ W_Rowid1 NO-ERROR.
  ASSIGN Ran_Intereses.Agencia = W_Agencia
         Ran_Intereses.Indicador  = W_ind
         Ran_Intereses.Fecha   
         Ran_Intereses.Estado  
         Ran_Intereses.Val_Inicial
         Ran_Intereses.Val_Final
         Ran_Intereses.Pla_Final
         Ran_Intereses.Pla_Inicial
         Ran_Intereses.Puntos
         Ran_Intereses.Puntos_Asoc
         Ran_Intereses.Pun_Negociables.
  IF W_Nuevo THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Ran_Intereses. Agencia: " + 
         STRING(Ran_Intereses.Agencia) + " - Indicador: " + STRING(Ran_Intereses.Indicador)).
  ELSE DO:
    IF Ran_Intereses.Estado EQ 2 THEN
     RUN P-GraLog IN W_Manija (INPUT "ADV: INACTIVA Registro, Ran_Intereses. Agencia: " + 
         STRING(Ran_Intereses.Agencia) + " - Indicador: " + STRING(Ran_Intereses.Indicador)).
    ELSE
     RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Ran_Intereses. Agencia: " + 
         STRING(Ran_Intereses.Agencia) + " - Indicador: " + STRING(Ran_Intereses.Indicador)).
  END.

  FIND CURRENT Ran_Intereses NO-LOCK NO-ERROR.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinR':U,OUTPUT W_Ventana).
  RUN P-BrwRango IN WIDGET-HANDLE (W_Ventana).  
  W_Nuevo = FALSE.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinBB':U,OUTPUT W_Ventana).
  RUN P-HabRangos IN WIDGET-HANDLE (W_Ventana) (INPUT FALSE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ingRang
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ingRang V-table-Win
ON CHOOSE OF Btn_ingRang IN FRAME F-VRanInt /* Ingresar */
DO:
  IF W_FecVcto LT TODAY THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 379,OUTPUT W_Rpta).
     APPLY "ENTRY" TO Ran_Intereses.Estado.
     RETURN NO-APPLY.
  END.
  IF W_Est EQ 2 THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 379,OUTPUT W_Rpta).
     APPLY "ENTRY" TO Ran_Intereses.Estado.
     RETURN NO-APPLY.
  END.
  FIND FIRST Indicadores WHERE Indicadores.Indicador EQ W_Ind
                           AND Indicadores.Rangos EQ TRUE 
                           AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAILABLE(Indicadores) THEN 
     DO WITH FRAME {&FRAME-NAME}:
        ASSIGN W_Nuevo = TRUE
               Ran_Intereses.Indicador:SCREEN-VALUE          = STRING(W_ind)
               Ran_Intereses.Estado:SCREEN-VALUE          = "1"
               Ran_Intereses.Val_Inicial:SCREEN-VALUE     = "0"
               Ran_Intereses.Val_Final:SCREEN-VALUE       = "0"
               Ran_Intereses.Pla_Final:SCREEN-VALUE       = "0"
               Ran_Intereses.Pla_Inicial:SCREEN-VALUE     = "0"
               Ran_Intereses.Puntos:SCREEN-VALUE          = "0.00"
               Ran_Intereses.Puntos_Asoc:SCREEN-VALUE     = "0.00"
               Ran_Intereses.Pun_Negociables:SCREEN-VALUE = "0.00"
               Ran_Intereses.Fecha:SCREEN-VALUE           = STRING(TODAY)
               Indicadores.Nombre:SCREEN-VALUE            = Indicadores.Nombre.  
        IF TODAY LT W_Fec THEN
           ASSIGN Ran_Intereses.Fecha:SCREEN-VALUE  = STRING(W_Fec).
    END.
  ELSE DO:
    RUN MostrarMensaje IN W_Manija (INPUT 379,OUTPUT W_Rpta).
    APPLY "ENTRY" TO Ran_Intereses.Estado.
    RETURN NO-APPLY.
  END.
  APPLY "ENTRY" TO Ran_Intereses.Val_Inicial.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinBB':U,OUTPUT W_Ventana).
  RUN P-HabRangos IN WIDGET-HANDLE (W_Ventana) (INPUT TRUE).
  RETURN NO-APPLY.
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
  {src/adm/template/row-list.i "Ran_Intereses"}
  {src/adm/template/row-list.i "Indicadores"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Ran_Intereses"}
  {src/adm/template/row-find.i "Indicadores"}

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
  HIDE FRAME F-VRanInt.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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
  ENABLE ALL EXCEPT Ran_Intereses.Indicador Ran_Intereses.Fecha 
                    Indicadores.Nombre WITH FRAME F-VRanInt.
  ASSIGN Ran_Intereses.Indicador:SCREEN-VALUE = STRING(W_Ind).
  IF W_Nuevo THEN
     ASSIGN    Ran_Intereses.Estado:SCREEN-VALUE          = "1"
               Ran_Intereses.Val_Inicial:SCREEN-VALUE     = "0"
               Ran_Intereses.Val_Final:SCREEN-VALUE       = "0"
               Ran_Intereses.Pla_Final:SCREEN-VALUE       = "0"
               Ran_Intereses.Pla_Inicial:SCREEN-VALUE     = "0"
               Ran_Intereses.Puntos:SCREEN-VALUE          = "0.00"
               Ran_Intereses.Puntos_Asoc:SCREEN-VALUE     = "0.00"
               Ran_Intereses.Pun_Negociables:SCREEN-VALUE = "0.00"
               Ran_Intereses.Fecha:SCREEN-VALUE           = STRING(TODAY)
               Indicadores.Nombre:SCREEN-VALUE            = Indicadores.Nombre
               W_Nuevo = FALSE.
  APPLY "ENTRY" TO Ran_Intereses.Val_Inicial IN FRAME F-VRanInt.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE P-VRangos V-table-Win 
PROCEDURE P-VRangos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "CHOOSE" TO Btn_IngRang IN FRAME F-VRanInt.
  RETURN NO-APPLY.
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
  {src/adm/template/snd-list.i "Ran_Intereses"}
  {src/adm/template/snd-list.i "Indicadores"}

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

