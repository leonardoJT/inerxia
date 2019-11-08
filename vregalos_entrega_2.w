&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dregalos_entrega.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}


/*     {Incluido/Variable.I} */
/* {Incluido/Variable.I "NEW SHARED"} */
/*     {Incluido/Variable.I} /*giocam*/ */
/*                                      */
/*         ASSIGN W_Usuario = "339"     */
/*                  W_Agencia = 1       */
/*                  W_fecha = TODAY.    */


DEFINE VARIABLE W_Usuario  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE W_Fecha    AS DATE        NO-UNDO.
DEFINE VARIABLE W_Agencia  AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dregalos_entrega.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.nit 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS RowObject.usuario RowObject.hora ~
RowObject.agencia RowObject.regalo RowObject.FRegalo RowObject.FAgencia ~
RowObject.nit RowObject.fecha_entrega RowObject.FHora 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS F-Cliente 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_drelaciones AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Titulo 
     LABEL "" 
     SIZE 41 BY 1.12.

DEFINE VARIABLE F-Cliente AS CHARACTER FORMAT "x(40)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 8.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.usuario AT ROW 1.27 COL 7 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 8.43 BY 1
          BGCOLOR 10 
     BTN-Titulo AT ROW 1.54 COL 20 WIDGET-ID 30
     RowObject.hora AT ROW 2.35 COL 9 COLON-ALIGNED HELP
          "Hora de entrega del regalo" WIDGET-ID 10
          LABEL "Hora Entrega" FORMAT "99999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 7.14 BY .81
          BGCOLOR 10 FONT 4
     RowObject.agencia AT ROW 2.88 COL 33 COLON-ALIGNED HELP
          "Código de la Agenica que entrega el regalo" WIDGET-ID 2
          LABEL "Agencia" FORMAT "999"
          VIEW-AS FILL-IN 
          SIZE 4.86 BY .81
          BGCOLOR 10 FONT 4
     RowObject.regalo AT ROW 4.77 COL 19 COLON-ALIGNED HELP
          "Digite Código del regalo" WIDGET-ID 16
          LABEL "Código Regalo" FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 FONT 4
     RowObject.FRegalo AT ROW 4.77 COL 26 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 24 FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 30 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 1
     RowObject.FAgencia AT ROW 5.85 COL 19 COLON-ALIGNED HELP
          "" WIDGET-ID 36
          LABEL "Agencia" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 37 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 1
     RowObject.nit AT ROW 6.92 COL 19 COLON-ALIGNED HELP
          "Digite el Nit del Asociado" WIDGET-ID 12
          LABEL "Nit Asociado" FORMAT "x(12)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 16.43 BY .81
          BGCOLOR 15 FONT 4
     F-Cliente AT ROW 6.92 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     RowObject.fecha_entrega AT ROW 9.35 COL 19 COLON-ALIGNED HELP
          "Digite la fecha de entrega del regalo" WIDGET-ID 4
          LABEL "Fecha Entrega" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY .81
          BGCOLOR 15 FONT 4
     RowObject.FHora AT ROW 10.31 COL 19 COLON-ALIGNED HELP
          "" WIDGET-ID 6
          LABEL "Hora" FORMAT "x(10)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 13 BY .81
          BGCOLOR 15 
     "ENTREGA DE REGALOS" VIEW-AS TEXT
          SIZE 22.57 BY .5 AT ROW 1.81 COL 29.14 WIDGET-ID 32
          BGCOLOR 11 FONT 1
     RECT-1 AT ROW 3.42 COL 1 WIDGET-ID 26
     RECT-2 AT ROW 1.27 COL 19.14 WIDGET-ID 28
     SPACE(14.86) SKIP(6.11)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dregalos_entrega.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dregalos_entrega.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 11.04
         WIDTH              = 79.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.agencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.agencia:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON BTN-Titulo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Cliente IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.FAgencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.fecha_entrega IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FHora IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FRegalo IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.hora IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.hora:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.nit IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.regalo IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.usuario IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.usuario:HIDDEN IN FRAME F-Main           = TRUE.

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

&Scoped-define SELF-NAME RowObject.nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.nit vTableWin
ON LEAVE OF RowObject.nit IN FRAME F-Main /* Nit Asociado */
DO:
    DEFINE VARIABLE vcFiltro AS CHARACTER   NO-UNDO.

    FIND FIRST clientes WHERE clientes.nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE clientes THEN DO:
        MESSAGE "Asociado no aparece en base de datos."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN cancelRecord.
    END.
    ELSE DO:
        ASSIGN F-Cliente:SCREEN-VALUE = TRIM(clientes.nit) + " - "  + 
            TRIM(clientes.apellido1) + " " + 
            TRIM(clientes.apellido2) + " " + 
            TRIM(Clientes.nombre).
    END.

    ASSIGN vcFiltro = "nit EQ '" + TRIM(SELF:SCREEN-VALUE) + "' AND " + 
        "cod_relacion EQ 4 AND " +  
        "estado EQ 1 AND " +
        "descripcion BEGINS 'HIJ'".

    DYNAMIC-FUNCTION('setQueryWhere':U IN h_drelaciones,
       INPUT vcFiltro).
      DYNAMIC-FUNCTION('openQuery':U IN h_drelaciones).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN RowObject.fecha_entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
      RowObject.hora:SCREEN-VALUE = STRING(TIME)
      RowObject.usuario:SCREEN-VALUE = W_Usuario
      RowObject.agencia:SCREEN-VALUE = TRIM(STRING(W_Agencia)).
  


  FIND FIRST regalos_rango WHERE regalos_rango.fecha_inicial LE W_Fecha AND
      regalos_rango.fecha_final GT W_Fecha NO-LOCK NO-ERROR.
  IF NOT AVAILABLE regalos_rango THEN  DO:
      MESSAGE "No hay regalos por entregar a esta fecha" SKIP
          "en esta agencia." SKIP
          "Por favo revise los datos"
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Regalos fuera de rango".
      RUN cancelRecord.
  END.
  ELSE DO:
      FIND FIRST regalos WHERE regalos.regalo EQ regalos_rango.regalo NO-LOCK NO-ERROR.
      IF AVAILABLE regalos THEN 
          ASSIGN RowObject.regalo:SCREEN-VALUE = TRIM(STRING(regalos.regalo)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'drelaciones.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedrelacionesOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch20RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_drelaciones ).
       RUN repositionObject IN h_drelaciones ( 1.27 , 69.00 ) NO-ERROR.
       /* Size in AB:  ( 1.88 , 8.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldFPersonaRelacionKeyFieldNit_relacionDataSourceFiltercod_relacion = 4NumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBeneficiariosBrowseFieldsNit_relacion,FPersonaRelacion,Cod_relacion,DescripcionExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysFieldNamenit_relacionDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 8.00 , 21.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 55.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_drelaciones , 'Data':U , h_dynselect-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect-2 ,
             F-Cliente:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields vTableWin 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).

  /* Code placed here will execute AFTER standard behavior.    */

  FIND FIRST clientes WHERE clientes.nit EQ RowObject.nit:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
  IF AVAILABLE clientes THEN
      ASSIGN F-Cliente:SCREEN-VALUE = TRIM(clientes.apellido1) + " " + 
          TRIM(clientes.apellido2) + " " + 
          TRIM(Clientes.nombre).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parametros vTableWin 
PROCEDURE parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER ipUsuario   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipFecha     AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER ipAgencia   AS INTEGER     NO-UNDO.
              
ASSIGN 
    W_Usuario = ipUsuario  
    W_Fecha   = ipFecha   
    W_Agencia = ipAgencia .
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

