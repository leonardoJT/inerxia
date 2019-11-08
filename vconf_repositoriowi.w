&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          repositorio      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dconf_repositorio.i"}.



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

DEF VAR cTbla AS CHAR NO-UNDO.

DEF VAR ccmpos AS CHAR NO-UNDO.
DEF VAR iExtnt AS INTEGER NO-UNDO.
DEF VAR cEstdo AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dconf_repositorio.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Codigo RowObject.Estado ~
RowObject.FechaCambioEstado RowObject.BaseOrigen RowObject.BaseDestino ~
RowObject.NomCampoCal RowObject.DesCampoCal RowObject.TablaOrigen ~
RowObject.TablaDestino RowObject.ProcCampoCal RowObject.CampoOrigen ~
RowObject.CampoDestino RowObject.indice RowObject.TipoOrigen 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 FILL-IN-1 
&Scoped-Define DISPLAYED-FIELDS RowObject.Codigo RowObject.Estado ~
RowObject.FechaCambioEstado RowObject.BaseOrigen RowObject.BaseDestino ~
RowObject.NomCampoCal RowObject.DesCampoCal RowObject.TablaOrigen ~
RowObject.TablaDestino RowObject.ProcCampoCal RowObject.CampoOrigen ~
RowObject.CampoDestino RowObject.indice RowObject.TipoOrigen 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCmbnaLbelCmpo vTableWin 
FUNCTION fCmbnaLbelCmpo RETURNS CHARACTER
  (ccmpos AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetCmposDspnbleRpstrio vTableWin 
FUNCTION fGetCmposDspnbleRpstrio RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetCmposTbla vTableWin 
FUNCTION fGetCmposTbla RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTblasYDscrpciones vTableWin 
FUNCTION fGetTblasYDscrpciones RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLabelCmpo vTableWin 
FUNCTION fLabelCmpo RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLabelTbla vTableWin 
FUNCTION fLabelTbla RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Campo Calculado:" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FGCOLOR 7  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 100 BY 6.46.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 37 BY 3.5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 31 BY 3.5 TOOLTIP "Destino".

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    ROUNDED 
     SIZE 30 BY 3.5 TOOLTIP "Detalle Campo Calculado".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Codigo AT ROW 1.27 COL 8 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 8 BY 1 TOOLTIP "Código"
     RowObject.Estado AT ROW 1.27 COL 44.57 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS COMBO-BOX INNER-LINES 3
          LIST-ITEM-PAIRS "Activo",1,
                     "Inactivo",2
          DROP-DOWN-LIST
          SIZE 16 BY 1 TOOLTIP "Estado"
     RowObject.FechaCambioEstado AT ROW 1.27 COL 82 COLON-ALIGNED WIDGET-ID 14
          LABEL "Fecha Cambio"
          VIEW-AS FILL-IN 
          SIZE 10.57 BY 1 TOOLTIP "Fecha Cambio Estado"
     RowObject.BaseOrigen AT ROW 2.88 COL 8 COLON-ALIGNED WIDGET-ID 28
          LABEL "Base"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RowObject.BaseDestino AT ROW 2.88 COL 45 COLON-ALIGNED WIDGET-ID 2
          LABEL "Base"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1 TOOLTIP "Base Datos Destino"
     RowObject.NomCampoCal AT ROW 2.88 COL 80 COLON-ALIGNED WIDGET-ID 44
          LABEL "Nombre"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.DesCampoCal AT ROW 3.88 COL 80 COLON-ALIGNED WIDGET-ID 42
          LABEL "Descripción"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.TablaOrigen AT ROW 3.96 COL 8 COLON-ALIGNED WIDGET-ID 26
          LABEL "Tabla"
          VIEW-AS COMBO-BOX SORT INNER-LINES 30
          LIST-ITEM-PAIRS "Item 1","Item 1"
          DROP-DOWN-LIST
          SIZE 28 BY 1 TOOLTIP "Tablas Disponible"
     RowObject.TablaDestino AT ROW 3.96 COL 45 COLON-ALIGNED WIDGET-ID 16
          LABEL "Tabla"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1 TOOLTIP "Tabla Destino"
     RowObject.ProcCampoCal AT ROW 4.88 COL 80 COLON-ALIGNED WIDGET-ID 46
          LABEL "Proc."
          VIEW-AS FILL-IN 
          SIZE 17 BY 1 TOOLTIP "Procedimiento"
     RowObject.CampoOrigen AT ROW 5.04 COL 8 COLON-ALIGNED WIDGET-ID 32
          LABEL "Campo"
          VIEW-AS COMBO-BOX SORT INNER-LINES 30
          LIST-ITEM-PAIRS "Item 1","Item 1"
          DROP-DOWN-LIST
          SIZE 28 BY 1 TOOLTIP "Campos Disponibles"
     RowObject.CampoDestino AT ROW 5.04 COL 45 COLON-ALIGNED WIDGET-ID 38
          LABEL "Campo"
          VIEW-AS COMBO-BOX INNER-LINES 30
          LIST-ITEM-PAIRS "Item 1","Item 1"
          DROP-DOWN-LIST
          SIZE 22 BY 1 TOOLTIP "Campo Destino"
     RowObject.indice AT ROW 6.12 COL 8 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 3.43 BY 1
     RowObject.TipoOrigen AT ROW 6.12 COL 22 COLON-ALIGNED WIDGET-ID 20
          LABEL "Tipo"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1 TOOLTIP "Tipo Dato Origen"
     FILL-IN-1 AT ROW 2.35 COL 69 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     "Origen:" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 2.35 COL 3 WIDGET-ID 50
          FGCOLOR 7 
     "Destino:" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 2.35 COL 40 WIDGET-ID 52
          FGCOLOR 7 
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 22
     RECT-2 AT ROW 2.62 COL 2 WIDGET-ID 34
     RECT-3 AT ROW 2.62 COL 39 WIDGET-ID 36
     RECT-4 AT ROW 2.62 COL 70 WIDGET-ID 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dconf_repositorio.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dconf_repositorio.i}
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
         HEIGHT             = 6.46
         WIDTH              = 100.
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

/* SETTINGS FOR FILL-IN RowObject.BaseDestino IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.BaseOrigen IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX RowObject.CampoDestino IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX RowObject.CampoOrigen IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.DesCampoCal IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.FechaCambioEstado IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.NomCampoCal IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.ProcCampoCal IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.TablaDestino IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX RowObject.TablaOrigen IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.TipoOrigen IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.BaseDestino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.BaseDestino vTableWin
ON ENTRY OF RowObject.BaseDestino IN FRAME F-Main /* Base */
DO:
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.BaseOrigen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.BaseOrigen vTableWin
ON ENTRY OF RowObject.BaseOrigen IN FRAME F-Main /* Base */
DO:
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.CampoOrigen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.CampoOrigen vTableWin
ON VALUE-CHANGED OF RowObject.CampoOrigen IN FRAME F-Main /* Campo */
DO:
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR cTpoCmpo AS CHAR NO-UNDO.
    DEF VAR cCmposRpstrio AS CHAR NO-UNDO.
    DEF VAR cCmposRpstrio1 AS CHAR NO-UNDO.
    i = lookup(SELF:SCREEN-VALUE,entry(1,ccmpos,CHR(1)),CHR(2)).
    cTpoCmpo = entry(i,ENTRY(3,ccmpos,CHR(1)),CHR(2)).
    
    i = INTEGER(entry(i,ENTRY(5,ccmpos,CHR(1)),CHR(2))).
    iExtnt = i.
    PUBLISH "pextent" (STRING(iextnt)).
    DO WITH FRAME  {&FRAME-NAME}:
        RowObject.indice:SCREEN-VALUE = "0".
        RowObject.TipoOrigen:SCREEN-VALUE = ctpocmpo.
        cCmposRpstrio = fGetCmposDspnbleRpstrio("repositorio" + CHR(1) + ctpocmpo).
        cCmposRpstrio1 = fCmbnaLbelCmpo(cCmposRpstrio).
        RowObject.CampoDestino:LIST-ITEM-PAIRS = cCmposRpstrio1.
        RowObject.CampoDestino:SCREEN-VALUE = ENTRY(2,cCmposRpstrio1,",").
        RowObject.CampoDestino:SENSITIVE = FALSE.
        IF i = 0 THEN RowObject.indice:SENSITIVE = FALSE.
        ELSE do:
            RowObject.indice:SENSITIVE = TRUE.
            APPLY "entry" TO RowObject.indice.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Codigo vTableWin
ON ANY-PRINTABLE OF RowObject.Codigo IN FRAME F-Main /* Código */
DO:
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Codigo vTableWin
ON ENTRY OF RowObject.Codigo IN FRAME F-Main /* Código */
DO:
    RUN initializeobject1.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.indice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.indice vTableWin
ON ENTRY OF RowObject.indice IN FRAME F-Main /* Indice */
DO:
    DO WITH FRAME  {&FRAME-NAME}:
        self:TOOLTIP = "El Indice Debe Ser Meno O Igual Que " + string(iExtnt).  
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.ProcCampoCal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.ProcCampoCal vTableWin
ON ENTRY OF RowObject.ProcCampoCal IN FRAME F-Main /* Proc. */
DO:
    APPLY "help" TO RowObject.ProcCampoCal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.ProcCampoCal vTableWin
ON HELP OF RowObject.ProcCampoCal IN FRAME F-Main /* Proc. */
DO:
    DEF VAR cFle AS CHAR NO-UNDO.
    DEF VAR lsino AS LOGICAL NO-UNDO.
    SYSTEM-DIALOG GET-FILE cfle
        FILTERS "p-prc-repo*.p" "p-prc-repo*.p"
        MUST-EXIST 
        RETURN-TO-START-DIR 
        TITLE "Procedimiento Repositorio"
        USE-FILENAME
        UPDATE lsino.
    IF lsino
    THEN DO:
        cFle = ENTRY(NUM-ENTRIES(cfle,"\"),cFle,"\").
        {&SELF-NAME}:SCREEN-VALUE = cFle.
    END.
    ELSE {&SELF-NAME}:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.ProcCampoCal vTableWin
ON LEAVE OF RowObject.ProcCampoCal IN FRAME F-Main /* Proc. */
DO:
    IF NOT {&SELF-NAME}:SCREEN-VALUE BEGINS "p-prc-repo" 
    THEN DO:
        MESSAGE "El Nombre Del Programa Debe Empezar por 'p-prc-repo'"
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    IF SEARCH({&SELF-NAME}:SCREEN-VALUE) = ? 
    THEN DO:
        MESSAGE "El Programa No Se Encuentra En El PROPATH:"
            SKIP(2) replace(PROPATH,",",CHR(10))
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.TablaDestino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.TablaDestino vTableWin
ON ENTRY OF RowObject.TablaDestino IN FRAME F-Main /* Tabla */
DO:
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.TablaOrigen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.TablaOrigen vTableWin
ON VALUE-CHANGED OF RowObject.TablaOrigen IN FRAME F-Main /* Tabla */
DO:
    cTbla = SELF:SCREEN-VALUE.  
    DEF VAR clsta AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR clsta1 AS CHAR NO-UNDO.
    ccmpos = fGetCmposTbla(cTbla).
    DO WITH FRAME {&FRAME-NAME}:
        clsta1 = fCmbnaLbelCmpo(ccmpos).
        RowObject.CampoOrigen:LIST-ITEM-PAIRS = clsta1.
    END.
    PUBLISH "vconf_repositorio_tbla" FROM THIS-PROCEDURE  (ctbla).
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
    RUN initializeobject1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    
    RUN SUPER.
    
    /* Code placed here will execute AFTER standard behavior.    */
    
    PUBLISH "vconf_repositorio_estado" FROM THIS-PROCEDURE  (cestdo) .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFieldList vTableWin 
PROCEDURE displayFieldList :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pcFieldList  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcFromSource AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER phDataSource AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER pcColValues  AS CHARACTER NO-UNDO.
    DEF VAR ctbla AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR ccmpo AS CHAR NO-UNDO.
    DEF VAR ccmpoD AS CHAR NO-UNDO.
    
    i = LOOKUP("TablaOrigen",pcFieldList,",").
    ctbla = entry(i,pcColValues,CHR(1)).

    PUBLISH "vconf_repositorio_tbla" FROM THIS-PROCEDURE  (ctbla) .

    i = LOOKUP("CampoOrigen",pcFieldList,",").
    ccmpo = entry(i,pcColValues,CHR(1)).
        
    i = LOOKUP("CampoDestino",pcFieldList,",").
    ccmpoD = entry(i,pcColValues,CHR(1)).
    
    DO WITH FRAME  {&FRAME-NAME}:
        RowObject.TablaOrigen:LIST-ITEM-PAIRS = fLabelTbla(ctbla) + "," + ctbla NO-ERROR.
        RowObject.CampoOrigen:LIST-ITEM-PAIRS = flabelcmpo(ctbla + CHR(1) + ccmpo) + "," + ccmpo NO-ERROR.
        RowObject.CampoDestino:LIST-ITEM-PAIRS = ccmpod + "," + ccmpod NO-ERROR.
    END.

    i = LOOKUP("Estado",pcFieldList,",").
    cestdo = entry(i,pcColValues,CHR(1)).
    
    PUBLISH "vconf_repositorio_estado" FROM THIS-PROCEDURE  (cestdo) .

    /* Code placed here will execute PRIOR to standard behavior. */
    
    RUN SUPER( INPUT pcFieldList, INPUT pcFromSource, INPUT phDataSource, INPUT pcColValues).
    
    /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields vTableWin 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
    DEF VAR cAdd AS CHAR NO-UNDO.
    cadd = DYNAMIC-FUNCTION( "getNewRecord" ).
    
    RUN SUPER.
    IF cadd <> "add" 
    THEN DO WITH FRAME {&FRAME-NAME}:
        RowObject.Estado:SENSITIVE = TRUE.
        RowObject.FechaCambioEstado:SENSITIVE = FALSE.

        RowObject.BaseDestino:SENSITIVE = FALSE. 
        RowObject.BaseOrigen:SENSITIVE = FALSE. 
        RowObject.CampoDestino:SENSITIVE = FALSE. 
        RowObject.CampoOrigen:SENSITIVE = FALSE. 
        RowObject.Codigo:SENSITIVE = FALSE. 
        RowObject.indice:SENSITIVE = FALSE. 
        RowObject.TablaDestino:SENSITIVE = FALSE. 
        RowObject.TablaOrigen:SENSITIVE = FALSE. 
        RowObject.TipoOrigen:SENSITIVE = FALSE.
        RETURN.
    END.

  /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        RowObject.indice:SENSITIVE = FALSE.
        RowObject.TipoOrigen:SENSITIVE = FALSE.
        RowObject.Estado:SENSITIVE = FALSE.
        RowObject.FechaCambioEstado:SENSITIVE = FALSE.
    END.
    RUN initializeobject1.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hbltaClcldo vTableWin 
PROCEDURE hbltaClcldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DO WITH FRAME  {&FRAME-NAME}:
        RECT-4:HIDDEN = FALSE.
        RowObject.DesCampoCal:VISIBLE = TRUE.
        RowObject.NomCampoCal:VISIBLE = c = "S". 
        RowObject.ProcCampoCal:VISIBLE = c = "S".
        IF NOT DYNAMIC-FUNCTION('canNavigate':U IN THIS-PROCEDURE) = "yes"
        THEN DO:
            RowObject.DesCampoCal:SENSITIVE = TRUE.
            RowObject.NomCampoCal:SENSITIVE = TRUE. 
            RowObject.ProcCampoCal:SENSITIVE = TRUE. 
        END.
        FILL-IN-1:VISIBLE = c = "S". 
    END.
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
    RUN initializeobject1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeobject1 vTableWin 
PROCEDURE initializeobject1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cTblas AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DO WITH FRAME  {&FRAME-NAME}:
        cTblas = fGetTblasYDscrpciones().

        ctblas = REPLACE(ctblas,CHR(1),",").
        ctblas = REPLACE(ctblas,CHR(2),",").
        RowObject.TablaOrigen:LIST-ITEM-PAIRS = ctblas.
        RowObject.TipoOrigen:SENSITIVE = FALSE.
        RowObject.CampoDestino:DELETE(1).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCmbnaLbelCmpo vTableWin 
FUNCTION fCmbnaLbelCmpo RETURNS CHARACTER
  (ccmpos AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR clsta AS CHAR NO-UNDO.
    DO i = 1 TO NUM-ENTRIES(ENTRY(1,ccmpos,CHR(1)),CHR(2)):
        clsta = clsta + 
                    "[" + ENTRY(i,ENTRY(1,ccmpos,CHR(1)),CHR(2)) + "] " + ENTRY(i,ENTRY(2,ccmpos,CHR(1)),CHR(2)) + 
                "," + ENTRY(i,ENTRY(1,ccmpos,CHR(1)),CHR(2)) + ",".
    END.
    clsta = TRIM(clsta,",").
    RETURN clsta.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetCmposDspnbleRpstrio vTableWin 
FUNCTION fGetCmposDspnbleRpstrio RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetCmposTbla vTableWin 
FUNCTION fGetCmposTbla RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTblasYDscrpciones vTableWin 
FUNCTION fGetTblasYDscrpciones RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER().

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLabelCmpo vTableWin 
FUNCTION fLabelCmpo RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLabelTbla vTableWin 
FUNCTION fLabelTbla RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

