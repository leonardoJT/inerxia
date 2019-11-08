&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE dsuperformatoc NO-UNDO
       {"dSuperFormatoC.i"}.
DEFINE TEMP-TABLE dsuperformatod NO-UNDO
       {"dSuperFormatoD.i"}.



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

DEF VAR iTbla AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "sbosuperformato.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS dsuperformatod.UnidadDeCaptura ~
dsuperformatod.Subcuenta dsuperformatod.Columna dsuperformatod.funcion ~
dsuperformatod.Titulo dsuperformatod.cPrmtro dsuperformatod.cQuery ~
dsuperformatod.Tblas dsuperformatod.CampoFechaParaTRM ~
dsuperformatod.CamposASumar dsuperformatod.CuentasCuadre 
&Scoped-define ENABLED-TABLES dsuperformatod
&Scoped-define FIRST-ENABLED-TABLE dsuperformatod
&Scoped-Define ENABLED-OBJECTS RECT-295 RECT-296 
&Scoped-Define DISPLAYED-FIELDS dsuperformatod.Codigo ~
dsuperformatod.UnidadDeCaptura dsuperformatod.Subcuenta ~
dsuperformatod.Columna dsuperformatod.funcion dsuperformatod.Titulo ~
dsuperformatod.cPrmtro dsuperformatod.cQuery dsuperformatod.Tblas ~
dsuperformatod.CampoFechaParaTRM dsuperformatod.CamposASumar ~
dsuperformatod.CuentasCuadre 
&Scoped-define DISPLAYED-TABLES dsuperformatod
&Scoped-define FIRST-DISPLAYED-TABLE dsuperformatod


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-295
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 66 BY 20.46.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 3.23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     dsuperformatod.Codigo AT ROW 3.42 COL 2 NO-LABEL WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 12.43 BY 1 TOOLTIP "Código Formato"
     dsuperformatod.UnidadDeCaptura AT ROW 3.42 COL 13 COLON-ALIGNED NO-LABEL WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 4.86 BY 1
     dsuperformatod.Subcuenta AT ROW 3.42 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 4.86 BY 1
     dsuperformatod.Columna AT ROW 3.42 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 3.72 BY 1
     dsuperformatod.funcion AT ROW 3.42 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     dsuperformatod.Titulo AT ROW 5.04 COL 3 COLON-ALIGNED WIDGET-ID 8
          LABEL "Tit" FORMAT "x(60)"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     dsuperformatod.cPrmtro AT ROW 5.04 COL 42 NO-LABEL WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 23 BY 1 TOOLTIP "Lista De Parámetros Separados Por Comas"
     dsuperformatod.cQuery AT ROW 6.65 COL 2 NO-LABEL WIDGET-ID 14
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 63 BY 2.42 TOOLTIP "Query COn Sintaxis PROGRESS 4GL"
     dsuperformatod.Tblas AT ROW 9.19 COL 7 COLON-ALIGNED WIDGET-ID 30 FORMAT "x(256)"
          VIEW-AS COMBO-BOX SORT INNER-LINES 5
          LIST-ITEM-PAIRS "","",
                     "t338Creditos","t338Creditos",
                     "t338Ahorros","t338Ahorros",
                     "saldo_diario","saldo_diario"
          DROP-DOWN-LIST
          SIZE 56 BY 1 TOOLTIP "Tabla"
     dsuperformatod.CampoFechaParaTRM AT ROW 10.15 COL 22 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "","",
                     "fecha","fecha"
          DROP-DOWN-LIST
          SIZE 41 BY 1 TOOLTIP "Campo Fecha Para Calcular La TRM"
     dsuperformatod.CamposASumar AT ROW 11.23 COL 10 NO-LABEL WIDGET-ID 22
          VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
          LIST-ITEM-PAIRS "SaldoCapital","SaldoCapital",
                     "InteresCorriente","InteresCorriente",
                     "InteresAnticipado","InteresAnticipado",
                     "InteresMoraCobrar","InteresMoraCobrar",
                     "InteresDificilCobro","InteresDificilCobro",
                     "SaldoDisponible","SaldoDisponible",
                     "SaldoDia","SaldoDia" 
          SIZE 55 BY 6.19 TOOLTIP "Campos A Sumar"
     dsuperformatod.CuentasCuadre AT ROW 18.23 COL 2 NO-LABEL WIDGET-ID 36
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE NO-BOX
          SIZE 63 BY 2.92
     "Función" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 2.88 COL 42 WIDGET-ID 52
     "A" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 13.12 COL 5 WIDGET-ID 26
     "CUENTAS DE CUADRE" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 17.42 COL 2 WIDGET-ID 38
     "Sumar" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 13.65 COL 2.57 WIDGET-ID 28
     "Campos" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.58 COL 2 WIDGET-ID 24
     "Formato" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.88 COL 2 WIDGET-ID 40
     "Unidad" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 2.35 COL 14 WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Captura" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 2.88 COL 14 WIDGET-ID 44
     "Sub Cuenta" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 2.88 COL 22 WIDGET-ID 46
     "Columna" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.88 COL 33 WIDGET-ID 48
     "Parámetros:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 4.5 COL 42 WIDGET-ID 60
     RECT-295 AT ROW 1 COL 1 WIDGET-ID 12
     RECT-296 AT ROW 3.15 COL 41 WIDGET-ID 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "sbosuperformato.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: dsuperformatoc D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {"dSuperFormatoC.i"}
      END-FIELDS.
      TABLE: dsuperformatod D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {"dSuperFormatoD.i"}
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
         HEIGHT             = 20.46
         WIDTH              = 66.
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

ASSIGN 
       dsuperformatod.CampoFechaParaTRM:PRIVATE-DATA IN FRAME F-Main     = 
                "saldo_diario,fecha".

/* SETTINGS FOR SELECTION-LIST dsuperformatod.CamposASumar IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       dsuperformatod.CamposASumar:PRIVATE-DATA IN FRAME F-Main     = 
                ",,SaldoCapital,SaldoCapital,InteresCorriente,InteresCorriente,InteresAnticipado,InteresAnticipado,InteresMoraCobrar,InteresMoraCobrar,InteresDificilCobro,InteresDificilCobro*,,SaldoDisponible,SaldoDisponible*,,SaldoDia,SaldoDia".

/* SETTINGS FOR FILL-IN dsuperformatod.Codigo IN FRAME F-Main
   NO-ENABLE ALIGN-L EXP-LABEL                                          */
/* SETTINGS FOR FILL-IN dsuperformatod.cPrmtro IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR EDITOR dsuperformatod.cQuery IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       dsuperformatod.cQuery:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR EDITOR dsuperformatod.CuentasCuadre IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       dsuperformatod.CuentasCuadre:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR COMBO-BOX dsuperformatod.Tblas IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       dsuperformatod.Tblas:PRIVATE-DATA IN FRAME F-Main     = 
                "t338Creditos,t338Ahorros,saldo_diario".

/* SETTINGS FOR FILL-IN dsuperformatod.Titulo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN dsuperformatod.UnidadDeCaptura IN FRAME F-Main
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

&Scoped-define SELF-NAME dsuperformatod.Tblas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dsuperformatod.Tblas vTableWin
ON VALUE-CHANGED OF dsuperformatod.Tblas IN FRAME F-Main /* Tablas */
DO:
    DEF VAR i AS INTEGER NO-UNDO.
    iTbla = LOOKUP(SELF:SCREEN-VALUE,SELF:PRIVATE-DATA,",").
    dsuperformatod.CamposASumar:LIST-ITEM-PAIRS = ENTRY(iTbla,dsuperformatod.CamposASumar:PRIVATE-DATA,"*").
    i = LOOKUP(SELF:SCREEN-VALUE,campofechaparatrm:PRIVATE-DATA,",").
    IF NOT i = 0 
    THEN DO:
        campofechaparatrm:LIST-ITEM-PAIRS = ENTRY(i + 1 ,campofechaparatrm:PRIVATE-DATA,",") + "," + ENTRY(i + 1 ,campofechaparatrm:PRIVATE-DATA,",").
    END.
    ELSE campofechaparatrm:LIST-ITEM-PAIRS = ",".
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
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    DO WITH FRAME {&FRAME-NAME}:
        DEF VAR i AS INTEGER NO-UNDO.
        DEF VAR cV AS CHAR NO-UNDO.
    
        i = LOOKUP("dsuperformatod.Tblas",pcFieldList,",").

        cV = entry(i,pcColValues,CHR(1)).
        IF NOT TRIM(cv) = ""
        THEN DO:
            iTbla = LOOKUP(cv,dsuperformatod.Tblas:PRIVATE-DATA,",").
            dsuperformatod.CamposASumar:LIST-ITEM-PAIRS = ENTRY(iTbla,dsuperformatod.CamposASumar:PRIVATE-DATA,"*").
        END.
    END. 

    i = LOOKUP("dsuperformatod.CamposASumar",pcFieldList,",").
    cV = entry(i,pcColValues,CHR(1)).
    IF trim(cv) = "?"
    THEN entry(i,pcColValues,CHR(1)) = "".
    
    
    RUN SUPER( INPUT pcFieldList, INPUT pcFromSource, INPUT phDataSource, INPUT pcColValues).
    /* Code placed here will execute AFTER standard behavior.    */

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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

