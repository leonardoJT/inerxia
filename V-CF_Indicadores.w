&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/* ***************************  Definitions  ************************** */
   CREATE WIDGET-POOL.
   DEFINE SHARED VARIABLE W_Ind       LIKE Indicadores.Indicador.
   DEFINE SHARED VARIABLE W_Rango     LIKE Indicadores.Rango.
   DEFINE SHARED VARIABLE W_Est       LIKE Indicadores.Estado.
   DEFINE SHARED VARIABLE W_Fec       LIKE Indicadores.Fecha.
   DEFINE SHARED VARIABLE W_FecVcto   LIKE Indicadores.FecVcto.
   DEFINE SHARED VARIABLE W_Agencia   LIKE Agencias.Agencia.
   DEFINE SHARED VARIABLE W_Manija      AS HANDLE.
   DEFINE        VARIABLE W_Rpta        AS LOGICAL.
   DEFINE        VARIABLE W_Ventana     AS CHARACTER.
   DEFINE        VARIABLE W_Rowid       AS ROWID.
   
   DEFINE TEMP-TABLE Tmp_Rangos LIKE Ran_Intereses.

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
&Scoped-define EXTERNAL-TABLES Indicadores
&Scoped-define FIRST-EXTERNAL-TABLE Indicadores


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Indicadores.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Indicadores.Nombre Indicadores.Estado ~
Indicadores.Tasa Indicadores.Fecha Indicadores.Base Indicadores.FecVcto ~
Indicadores.Rangos Indicadores.Id_Economico Indicadores.Valor ~
Indicadores.Hora 
&Scoped-define ENABLED-TABLES Indicadores
&Scoped-define FIRST-ENABLED-TABLE Indicadores
&Scoped-Define DISPLAYED-FIELDS Indicadores.indicador Indicadores.Nombre ~
Indicadores.Estado Indicadores.Tasa Indicadores.Fecha Indicadores.Base ~
Indicadores.FecVcto Indicadores.Rangos Indicadores.Id_Economico ~
Indicadores.Valor 
&Scoped-define DISPLAYED-TABLES Indicadores
&Scoped-define FIRST-DISPLAYED-TABLE Indicadores


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Indicadores.indicador Indicadores.Nombre ~
Indicadores.Estado Indicadores.Tasa Indicadores.Base Indicadores.Valor 
&Scoped-define ADM-ASSIGN-FIELDS Indicadores.Hora 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Indicadores.indicador AT ROW 2.35 COL 7 COLON-ALIGNED
          LABEL "Codigo"
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
          BGCOLOR 15 
     Indicadores.Nombre AT ROW 2.35 COL 21 COLON-ALIGNED
          LABEL "Nombre"
          VIEW-AS FILL-IN 
          SIZE 68 BY .81
          BGCOLOR 15 
     Indicadores.Estado AT ROW 3.42 COL 23 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 18 BY .81
          BGCOLOR 17 
     Indicadores.Tasa AT ROW 3.42 COL 45 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .85
          BGCOLOR 15 
     Indicadores.Fecha AT ROW 3.42 COL 77 COLON-ALIGNED
          LABEL "Fecha Inicial"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Indicadores.Base AT ROW 4.5 COL 45 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Indicadores.FecVcto AT ROW 4.5 COL 77 COLON-ALIGNED
          LABEL "Fecha Vencimiento"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Indicadores.Rangos AT ROW 4.77 COL 21
          LABEL "Maneja Rangos?"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .77
     Indicadores.Id_Economico AT ROW 5.58 COL 21
          LABEL "Indicador Economico?"
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .77
     Indicadores.Valor AT ROW 5.58 COL 45 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     Indicadores.Hora AT ROW 5.58 COL 77 COLON-ALIGNED NO-LABEL FORMAT "99999"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 15 
     "Información General" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 1.27 COL 9
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: datos.Indicadores
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
         HEIGHT             = 5.58
         WIDTH              = 94.86.
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

/* SETTINGS FOR FILL-IN Indicadores.Base IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR RADIO-SET Indicadores.Estado IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN Indicadores.Fecha IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Indicadores.FecVcto IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Indicadores.Hora IN FRAME F-Main
   NO-DISPLAY 2 EXP-FORMAT                                              */
ASSIGN 
       Indicadores.Hora:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX Indicadores.Id_Economico IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Indicadores.indicador IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Indicadores.Nombre IN FRAME F-Main
   1 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Indicadores.Rangos IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Indicadores.Tasa IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN Indicadores.Valor IN FRAME F-Main
   1                                                                    */
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

&Scoped-define SELF-NAME Indicadores.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Indicadores.Estado V-table-Win
ON MOUSE-SELECT-CLICK OF Indicadores.Estado IN FRAME F-Main /* Estado */
DO:
  APPLY "VALUE-CHANGED":U TO Indicadores.Estado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Indicadores.Estado V-table-Win
ON VALUE-CHANGED OF Indicadores.Estado IN FRAME F-Main /* Estado */
OR LEAVE OF Indicadores.Estado DO:
   IF Indicadores.Estado:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "1" THEN DO:
      ASSIGN W_Rowid  = ROWID(Indicadores).
      FIND FIRST Indicadores WHERE Indicadores.Indicador EQ INTEGER(Indicadores.Indicador:SCREEN-VALUE) 
                               AND Indicadores.Estado EQ 1 
                               AND ROWID(Indicadores) NE W_Rowid NO-LOCK NO-ERROR.
      IF AVAILABLE (Indicadores) THEN DO:
         ASSIGN Indicadores.Estado:SCREEN-VALUE = "2".        
         RUN MostrarMensaje IN W_Manija (INPUT 271,OUTPUT W_Rpta).
         APPLY "ENTRY":U TO Indicadores.Estado.
         RETURN NO-APPLY.
      END.  
      FIND Indicadores WHERE ROWID(Indicadores) EQ W_Rowid NO-LOCK NO-ERROR.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Indicadores.Fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Indicadores.Fecha V-table-Win
ON LEAVE OF Indicadores.Fecha IN FRAME F-Main /* Fecha Inicial */
DO:
  IF DATE(Indicadores.Fecha:SCREEN-VALUE) LT TODAY THEN DO:
     ASSIGN Indicadores.Fecha:SCREEN-VALUE = STRING(TODAY).
     MESSAGE "Fecha Inicial no puede" SKIP
             "ser menor que la actual."
             VIEW-AS ALERT-BOX INFORMATION TITLE "Error Configuración".
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Indicadores.FecVcto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Indicadores.FecVcto V-table-Win
ON LEAVE OF Indicadores.FecVcto IN FRAME F-Main /* Fecha Vencimiento */
DO:
/*  {INCLUIDO\BTCANCEL.I}
 *   IF DATE(Indicadores.FecVcto:SCREEN-VALUE IN FRAME {&FRAME-NAME}) LT 
 *      DATE(Indicadores.Fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
 *      DO:
 *        RUN MostrarMensaje IN W_Manija (INPUT 130,OUTPUT W_Rpta).
 *        APPLY "ENTRY" TO Indicadores.FecVcto.
 *        RETURN NO-APPLY.
 *      END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Indicadores.indicador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Indicadores.indicador V-table-Win
ON LEAVE OF Indicadores.indicador IN FRAME F-Main /* Codigo */
DO:
    IF Indicadores.Indicador:SCREEN-VALUE LE "00000" OR Indicadores.Indicador:SCREEN-VALUE EQ "?" THEN
       Indicadores.Indicador:SCREEN-VALUE = "00000".
    APPLY "TAB":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Indicadores.indicador V-table-Win
ON TAB OF Indicadores.indicador IN FRAME F-Main /* Codigo */
OR MOUSE-SELECT-DBLCLICK OF Indicadores.Indicador
OR RETURN OF Indicadores.Indicador DO:
   ON RETURN RETURN.
   DEFINE VAR W_Sino  AS CHARACTER INITIAL "no".
   ASSIGN W_Rowid  = ROWID(Indicadores).
   FIND FIRST Indicadores WHERE Indicadores.Indicador EQ INTEGER(Indicadores.Indicador:SCREEN-VALUE)
                            AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
   IF AVAILABLE (Indicadores) THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 271,OUTPUT W_Rpta).
      Indicadores.Indicador:SCREEN-VALUE = "".      
      APPLY "ENTRY" TO Indicadores.Indicador.
      RETURN NO-APPLY.
   END.
   FIND LAST Indicadores WHERE Indicadores.Indicador EQ INTEGER(Indicadores.Indicador:SCREEN-VALUE)
                           AND Indicadores.Fecha  NE ? NO-LOCK NO-ERROR.
   IF AVAILABLE (Indicadores) THEN DO WITH FRAME {&FRAME-NAME}:
           IF Indicadores.Rangos THEN
              ASSIGN W_Sino = "yes".
           DISPLAY TODAY @ Indicadores.Fecha
                   TODAY @ Indicadores.FecVcto.
           ASSIGN  Indicadores.Nombre:SCREEN-VALUE = Indicadores.Nombre
                   Indicadores.Rangos:SCREEN-VALUE = W_Sino
                   Indicadores.Tasa:SCREEN-VALUE   = STRING(Indicadores.Tasa)
                   Indicadores.Valor:SCREEN-VALUE  = STRING(Indicadores.Valor)
                   Indicadores.Base:SCREEN-VALUE   = STRING(Indicadores.Base)
                   Indicadores.Nombre:SENSITIVE    = FALSE.
   END.
   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinV':U,OUTPUT W_Ventana).
   RUN P-BrwRango IN WIDGET-HANDLE (W_Ventana).
   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinW':U,OUTPUT W_Ventana).
   RUN P-DesHRango IN WIDGET-HANDLE (W_Ventana).
   FIND Indicadores WHERE ROWID(Indicadores) EQ W_Rowid NO-LOCK NO-ERROR.
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
  {src/adm/template/row-list.i "Indicadores"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
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
     DISPLAY TODAY @ Indicadores.Fecha
             TODAY @ Indicadores.FecVcto.
     ASSIGN Indicadores.Rangos:SCREEN-VALUE = "no"
            Indicadores.Estado:SCREEN-VALUE = "1"
            W_Ind                           =  0
            W_Est                           =  1
            W_Rango                         = FALSE.
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinV':U,OUTPUT W_Ventana).
     RUN P-BrwRango IN WIDGET-HANDLE (W_Ventana).
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinW':U,OUTPUT W_Ventana).
     RUN P-DesHRango IN WIDGET-HANDLE (W_Ventana).
     ASSIGN Indicadores.Fecha:SENSITIVE = TRUE.
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
  DEFINE VAR W_Evento AS CHAR INITIAL " Modificar ". 
    W_FecVcto = DATE(Indicadores.FecVcto:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
    IF RETURN-VALUE = "YES" THEN DO:
       IF DATE(Indicadores.Fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}) LT TODAY THEN DO:
          RUN MostrarMensaje IN W_Manija (INPUT 321,OUTPUT W_Rpta).
          APPLY "ENTRY" TO Indicadores.Fecha.
          RETURN ERROR.
       END.
    END.
    
    IF (Indicadores.Fecha:SCREEN-VALUE) EQ "" OR (Indicadores.FecVcto:SCREEN-VALUE) EQ "" THEN DO:
       MESSAGE "Fechas Inicial o Final" SKIP
               "no pueden omitirse." 
               VIEW-AS ALERT-BOX INFORMATION TITLE "Error Configuración".
       RETURN ERROR.
    END.
    
    Indicadores.Hora:SCREEN-VALUE = STRING(TIME).
    
   /* ASSIGN FRAME {&FRAME-NAME} W_Ind             = Indicadores.Indicador.*/
                                /*Indicadores.Rango*/
                               /*W_Rango           = Indicadores.Rango*/
                               
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  
    IF Indicadores.Rango:SCREEN-VALUE EQ "yes" THEN DO:
       RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
       IF RETURN-VALUE = "YES" THEN DO:
          ASSIGN W_Evento                     = " Ingresar "
                 Indicadores.Nombre:SENSITIVE = TRUE.
          IF Indicadores.Rango:SCREEN-VALUE EQ "yes" THEN DO:
             FOR EACH Ran_Intereses FIELDS(Ran_Intereses.Indicador Ran_Intereses.Estado Ran_Intereses.Agencia)
                                    WHERE Ran_Intereses.Indicador EQ INTEGER(Indicadores.Indicador:SCREEN-VALUE) AND
                                          Ran_Intereses.Agencia EQ W_Agencia AND Ran_Intereses.Estado EQ 1:
                 CREATE Tmp_Rangos.
                 BUFFER-COPY Ran_Intereses TO Tmp_Rangos.
                 ASSIGN Ran_Intereses.Estado = 2
                        Tmp_Rangos.Fecha     = W_Fec.
             END.
             FOR EACH Tmp_Rangos:
                 CREATE Ran_Intereses.
                 BUFFER-COPY Tmp_Rangos TO Ran_Intereses. 
                 DELETE Tmp_Rangos.
             END.
          FIND CURRENT Ran_Intereses NO-LOCK NO-ERROR.
          END.
          RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Indicadores. Indicador: " + STRING(Indicadores.Indicador)).
       END.
       ELSE
          RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Indicadores. Indicador: " + STRING(Indicadores.Indicador)).
       
       IF Indicadores.Rango:SCREEN-VALUE EQ "yes" THEN DO:
          FIND FIRST Ran_Intereses WHERE Ran_Intereses.Indicador  EQ INTEGER(Indicadores.Indicador:SCREEN-VALUE)
                                     AND Ran_Intereses.Agencia EQ W_Agencia
                                     AND Ran_Intereses.Estado  EQ 1 NO-LOCK NO-ERROR.
          IF NOT AVAILABLE(Ran_Intereses) THEN DO:
             FRAME F-Main:SENSITIVE = FALSE.
             RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinV':U,OUTPUT W_Ventana).
             RUN P-BrwRango IN WIDGET-HANDLE (W_Ventana).
             MESSAGE "Ingrese Rangos para este Indicador" VIEW-AS ALERT-BOX.        
             RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinW':U,OUTPUT W_Ventana).
             RUN P-IngRangos IN WIDGET-HANDLE (W_Ventana).
             FRAME F-Main:SENSITIVE = TRUE.
/*---------------------------------------------------------------
En P-LlegaRango (Procedimiento interno)........: Asigna Falso a |
Indicadores.Rango si no ingresaron Rangos para este Indicador.  |
---------------------------------------------------------------*/
        END. 
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinV':U,OUTPUT W_Ventana).
        RUN P-BrwRango IN WIDGET-HANDLE (W_Ventana).
     END.
   END.
    
/*    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR W_Rpta AS LOGICAL.
  IF Indicadores.Estado EQ 2 THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: INACTIVA Registro, Indicadores. Indicador: " + 
         STRING(Indicadores.Indicador)).
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
  Notes:  Ejecuta P-BrwRango en B-RanIn1.W (Browser de Rangos).
          Agregó  GAER el 21 sept./98.     
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  ASSIGN Indicadores.Fecha:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
         W_Ind                                              = 1
         Indicadores.Rangos:SCREEN-VALUE                    = "no".
  IF AVAILABLE(Indicadores) THEN DO:
     ASSIGN W_Ind     = Indicadores.Indicador
            W_Rango   = Indicadores.Rango
            W_Est     = Indicadores.Estado
            W_FecVcto = Indicadores.FecVcto
            W_Fec     = Indicadores.Fecha.
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinV':U,OUTPUT W_Ventana).
     RUN P-BrwRango IN WIDGET-HANDLE (W_Ventana).
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinW':U,OUTPUT W_Ventana).
     RUN P-DesHRango IN WIDGET-HANDLE (W_Ventana).
     IF W_rango THEN 
        ASSIGN Indicadores.Rangos:SCREEN-VALUE = "yes".
  END.
  ELSE ASSIGN W_Ind     = 1
              W_Est     = 1
              W_Fec     = TODAY
              W_FecVcto = TODAY
              W_Rango   = FALSE.
              
  /* Code placed here will execute AFTER standard behavior.    */

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

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE P-LlegaRango V-table-Win 
PROCEDURE P-LlegaRango :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST Ran_Intereses WHERE Ran_Intereses.Indicador  EQ W_Ind
                              AND Ran_Intereses.Agencia EQ W_Agencia
                              AND Ran_Intereses.Estado  EQ 1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(Ran_Intereses) THEN DO:
      FIND CURRENT Indicadores NO-ERROR.
      ASSIGN Indicadores.Rangos = FALSE
             W_Rango            = FALSE.
      FIND CURRENT Indicadores NO-LOCK NO-ERROR.
   END.
  ASSIGN Indicadores.Rangos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no".
  IF W_rango THEN 
     ASSIGN Indicadores.Rangos:SCREEN-VALUE = "yes".
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'LinkWinW':U,OUTPUT W_Ventana).
  RUN P-DesHRango IN WIDGET-HANDLE (W_Ventana).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
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

