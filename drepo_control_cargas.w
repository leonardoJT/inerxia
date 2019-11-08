&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
          repositorio      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Global-define DATA-LOGIC-PROCEDURE .p

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF


&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES control_carga Usuarios Agencias

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  periodo fechaCarga horaCarga usuario agencia TotalRegistros
&Scoped-define ENABLED-FIELDS-IN-control_carga periodo fechaCarga horaCarga ~
usuario agencia TotalRegistros 
&Scoped-Define DATA-FIELDS  periodo cHra fechaCarga horaCarga usuario Nombre agencia Nombre-2~
 TotalRegistros
&Scoped-define DATA-FIELDS-IN-control_carga periodo fechaCarga horaCarga ~
usuario agencia TotalRegistros 
&Scoped-define DATA-FIELDS-IN-Usuarios Nombre 
&Scoped-define DATA-FIELDS-IN-Agencias Nombre-2 
&Scoped-Define MANDATORY-FIELDS  Nombre Nombre-2
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Nombre-2 = Agencias.Nombre
&Scoped-Define DATA-FIELD-DEFS "drepo_control_cargas.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH control_carga NO-LOCK, ~
      EACH Usuarios WHERE Usuarios.Usuario = ~
repositorio.control_carga.usuario OUTER-JOIN NO-LOCK, ~
      EACH Agencias WHERE Agencias.Agencia = control_carga.agencia OUTER-JOIN NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH control_carga NO-LOCK, ~
      EACH Usuarios WHERE Usuarios.Usuario = ~
repositorio.control_carga.usuario OUTER-JOIN NO-LOCK, ~
      EACH Agencias WHERE Agencias.Agencia = control_carga.agencia OUTER-JOIN NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main control_carga Usuarios Agencias
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main control_carga
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main Usuarios
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main Agencias


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      control_carga, 
      Usuarios, 
      Agencias SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
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
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 46.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "repositorio.control_carga,bdcentral.Usuarios WHERE repositorio.control_carga ...,bdcentral.Agencias WHERE repositorio.control_carga ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER, OUTER"
     _JoinCode[2]      = "bdcentral.Usuarios.Usuario =
repositorio.control_carga.usuario"
     _JoinCode[3]      = "bdcentral.Agencias.Agencia = repositorio.control_carga.agencia"
     _FldNameList[1]   > repositorio.control_carga.periodo
"periodo" "periodo" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[2]   > "_<CALC>"
"STRING (RowObject.horaCarga,""hh:mm:ss am"")" "cHra" "Hora" "x(11)" "character" ? ? ? ? ? ? no ? no 11 no "Hora"
     _FldNameList[3]   > repositorio.control_carga.fechaCarga
"fechaCarga" "fechaCarga" ? ? "date" ? ? ? ? ? ? yes ? no 11.57 yes ?
     _FldNameList[4]   > repositorio.control_carga.horaCarga
"horaCarga" "horaCarga" ? ? "integer" ? ? ? ? ? ? yes ? no 10.43 yes ?
     _FldNameList[5]   > repositorio.control_carga.usuario
"usuario" "usuario" ? ? "character" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[6]   > bdcentral.Usuarios.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? no ? yes 40 yes ?
     _FldNameList[7]   > repositorio.control_carga.agencia
"agencia" "agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[8]   > bdcentral.Agencias.Nombre
"Nombre" "Nombre-2" "Agencia" ? "character" ? ? ? ? ? ? no ? yes 40 yes ?
     _FldNameList[9]   > repositorio.control_carga.TotalRegistros
"TotalRegistros" "TotalRegistros" ? ? "integer" ? ? ? ? ? ? yes ? no 23.57 yes ?
     _Design-Parent    is WINDOW dTables @ ( 1.15 , 2.57 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.cHra = (STRING (RowObject.horaCarga,"hh:mm:ss am"))
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE periodovalidate dTables  _DB-REQUIRED
PROCEDURE periodovalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    /*IF CAN-FIND(CONTROL_carga WHERE CONTROL_carga.periodo = periodo)
    THEN RETURN "Período Ya Existe".*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE usuariovalidate dTables  _DB-REQUIRED
PROCEDURE usuariovalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    /*c = TRIM(c).
    IF c = ""
    THEN RETURN "Indique El Usuario".*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

