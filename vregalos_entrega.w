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

/* {Incluido/Variable.I "SHARED"} */


   {Incluido/Variable.I} /*giocam*/

       ASSIGN W_Usuario = "339"
                W_Agencia = 1
                W_fecha = TODAY.

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
&Scoped-Define ENABLED-FIELDS RowObject.nit RowObject.fecha_entrega 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS RowObject.regalo RowObject.hora ~
RowObject.nit RowObject.fecha_entrega RowObject.FHora RowObject.agencia ~
RowObject.FAgencia RowObject.usuario 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS F-Regalo F-Cliente 

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
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE F-Regalo AS CHARACTER FORMAT "x(40)" 
     LABEL "Regalo" 
     VIEW-AS FILL-IN 
     SIZE 55 BY .81
     BGCOLOR 18 FONT 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 9.69.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.regalo AT ROW 1 COL 13 COLON-ALIGNED HELP
          "Digite Código del regalo" WIDGET-ID 34
          LABEL "Código Regalo" FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 4.29 BY .81
          BGCOLOR 10 
     BTN-Titulo AT ROW 1.27 COL 20.14 WIDGET-ID 16
     RowObject.hora AT ROW 2.62 COL 14 COLON-ALIGNED HELP
          "Hora de entrega del regalo" WIDGET-ID 30
          LABEL "Hora Entrega" FORMAT "99999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 10 
     F-Regalo AT ROW 3.69 COL 14 COLON-ALIGNED WIDGET-ID 38
     RowObject.nit AT ROW 4.77 COL 14 COLON-ALIGNED HELP
          "Digite el Nit del Asociado" WIDGET-ID 4
          LABEL "Nit" FORMAT "x(12)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 16 BY .81
          BGCOLOR 15 
     F-Cliente AT ROW 4.77 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     RowObject.fecha_entrega AT ROW 6.92 COL 14 COLON-ALIGNED HELP
          "Digite la fecha de entrega del regalo" WIDGET-ID 28
          LABEL "Fecha Entrega" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     RowObject.FHora AT ROW 8 COL 14 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     RowObject.agencia AT ROW 9.08 COL 14 COLON-ALIGNED HELP
          "Código de la Agenica que entrega el regalo" WIDGET-ID 2
          LABEL "Agencia" FORMAT "999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 4.86 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.FAgencia AT ROW 9.08 COL 19 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 24 FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 50 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.usuario AT ROW 10.15 COL 14 COLON-ALIGNED HELP
          "Usuario que crea registro" WIDGET-ID 36
          LABEL "Usuario" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 4.86 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "ENTREGA DE REGALOS" VIEW-AS TEXT
          SIZE 22.57 BY .5 AT ROW 1.54 COL 29.29 WIDGET-ID 22
          BGCOLOR 11 FONT 1
     RECT-1 AT ROW 3.15 COL 1 WIDGET-ID 18
     RECT-2 AT ROW 1 COL 19.14 WIDGET-ID 20
     SPACE(11.58) SKIP(4.15)
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
         HEIGHT             = 11.92
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
/* SETTINGS FOR BUTTON BTN-Titulo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Cliente IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Regalo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.FAgencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.fecha_entrega IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.FHora IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.hora IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.nit IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.regalo IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.usuario IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
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
ON LEAVE OF RowObject.nit IN FRAME F-Main /* Nit */
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
    ASSIGN RowObject.hora:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INTEGER(TIME))
        RowObject.Fecha_Entrega:SCREEN-VALUE = STRING(W_Fecha)
        RowObject.usuario:SCREEN-VALUE = W_Usuario.

    ASSIGN vcFiltro = "nit EQ " + TRIM(SELF:SCREEN-VALUE).

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

  FIND FIRST regalos WHERE 
      regalos.regalo EQ INTEGER(RowObject.regalo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF AVAILABLE regalos THEN 
      ASSIGN F-Regalo:SCREEN-VALUE = regalos.nombre.
  ELSE
      ASSIGN F-Regalo:SCREEN-VALUE = "No creado regalo.".

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
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedrelacionesOpenOnInitnoPromptColumns(NONE)PromptOnDeleteyesRowsToBatch5RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_drelaciones ).
       RUN repositionObject IN h_drelaciones ( 1.27 , 66.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldFPersonaRelacionKeyFieldNit_relacionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNamenit_relacionDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 5.85 , 16.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 0.92 , 50.00 ) NO-ERROR.

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

  FIND FIRST regalos WHERE 
      regalos.regalo EQ INTEGER(RowObject.regalo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF AVAILABLE regalos THEN 
      ASSIGN F-Regalo:SCREEN-VALUE = regalos.nombre.
  ELSE
      ASSIGN F-Regalo:SCREEN-VALUE = "No creado regalo.".

  FIND FIRST clientes WHERE clientes.nit EQ RowObject.nit:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE clientes THEN
      ASSIGN F-Cliente:SCREEN-VALUE = TRIM(clientes.apellido1) + " " + 
          TRIM(clientes.apellido2) + " " + 
          TRIM(Clientes.nombre).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  DYNAMIC-FUNCTION('setQueryWhere':U IN h_drelaciones,
     INPUT "cod_relacion EQ 4 and estado EQ 1 and descripcion BEGINS 'HIJ'").

    DYNAMIC-FUNCTION('openQuery':U IN h_drelaciones).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

