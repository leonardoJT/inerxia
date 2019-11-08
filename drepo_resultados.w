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
&Scoped-define INTERNAL-TABLES repositorio.resultados bdcentral.Clientes ~
repositorio.repositorio bdcentral.Pro_Creditos bdcentral.Agencias

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  fecha Nit Apellido1 Apellido2 Nombre prestamo int002 int003 Nom_Producto~
 PerdidaEsperada ProbabilidadIncumplimiento calificacion~
 CalificacionEquivalente ExposicionActivo PerdidaEsperada% AnoContable~
 MesContable int001
&Scoped-define ENABLED-FIELDS-IN-repositorio.resultados fecha Nit prestamo ~
PerdidaEsperada ProbabilidadIncumplimiento calificacion ~
CalificacionEquivalente ExposicionActivo PerdidaEsperada% AnoContable ~
MesContable 
&Scoped-define ENABLED-FIELDS-IN-bdcentral.Clientes Apellido1 Apellido2 ~
Nombre 
&Scoped-define ENABLED-FIELDS-IN-repositorio.repositorio int002 int003 ~
int001 
&Scoped-define ENABLED-FIELDS-IN-bdcentral.Pro_Creditos Nom_Producto 
&Scoped-Define DATA-FIELDS  fecha Nit Apellido1 Apellido2 Nombre prestamo int002 int003 Nom_Producto~
 PerdidaEsperada ProbabilidadIncumplimiento calificacion~
 CalificacionEquivalente ExposicionActivo PerdidaEsperada% AnoContable~
 MesContable int001
&Scoped-define DATA-FIELDS-IN-repositorio.resultados fecha Nit prestamo ~
PerdidaEsperada ProbabilidadIncumplimiento calificacion ~
CalificacionEquivalente ExposicionActivo PerdidaEsperada% AnoContable ~
MesContable 
&Scoped-define DATA-FIELDS-IN-bdcentral.Clientes Apellido1 Apellido2 Nombre 
&Scoped-define DATA-FIELDS-IN-repositorio.repositorio int002 int003 int001 
&Scoped-define DATA-FIELDS-IN-bdcentral.Pro_Creditos Nom_Producto 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "drepo_resultados.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH repositorio.resultados NO-LOCK, ~
      EACH bdcentral.Clientes WHERE bdcentral.Clientes.Nit = repositorio.resultados.Nit NO-LOCK, ~
      EACH repositorio.repositorio WHERE repositorio.repositorio.NumeroCredito = integer(repositorio.resultados.prestamo) NO-LOCK, ~
      EACH bdcentral.Pro_Creditos WHERE bdcentral.Pro_Creditos.Cod_Credito = repositorio.repositorio.int003 NO-LOCK, ~
      EACH bdcentral.Agencias WHERE bdcentral.Agencias.Agencia = repositorio.repositorio.int001 NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH repositorio.resultados NO-LOCK, ~
      EACH bdcentral.Clientes WHERE bdcentral.Clientes.Nit = repositorio.resultados.Nit NO-LOCK, ~
      EACH repositorio.repositorio WHERE repositorio.repositorio.NumeroCredito = integer(repositorio.resultados.prestamo) NO-LOCK, ~
      EACH bdcentral.Pro_Creditos WHERE bdcentral.Pro_Creditos.Cod_Credito = repositorio.repositorio.int003 NO-LOCK, ~
      EACH bdcentral.Agencias WHERE bdcentral.Agencias.Agencia = repositorio.repositorio.int001 NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main repositorio.resultados ~
bdcentral.Clientes repositorio.repositorio bdcentral.Pro_Creditos ~
bdcentral.Agencias
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main repositorio.resultados
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main bdcentral.Clientes
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main repositorio.repositorio
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main bdcentral.Pro_Creditos
&Scoped-define FIFTH-TABLE-IN-QUERY-Query-Main bdcentral.Agencias


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      repositorio.resultados, 
      bdcentral.Clientes, 
      repositorio.repositorio, 
      bdcentral.Pro_Creditos, 
      bdcentral.Agencias SCROLLING.
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
     _TblList          = "repositorio.resultados,bdcentral.Clientes WHERE repositorio.resultados ...,repositorio.repositorio WHERE repositorio.resultados ...,bdcentral.Pro_Creditos WHERE repositorio.repositorio ...,bdcentral.Agencias WHERE repositorio.repositorio ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ",,,,"
     _JoinCode[2]      = "bdcentral.Clientes.Nit = repositorio.resultados.Nit"
     _JoinCode[3]      = "repositorio.repositorio.NumeroCredito = integer(repositorio.resultados.prestamo)"
     _JoinCode[4]      = "bdcentral.Pro_Creditos.Cod_Credito = repositorio.repositorio.int003"
     _JoinCode[5]      = "bdcentral.Agencias.Agencia = repositorio.repositorio.int001"
     _FldNameList[1]   > repositorio.resultados.fecha
"fecha" "fecha" ? ? "integer" ? ? ? ? ? ? yes ? no 9.14 yes ?
     _FldNameList[2]   > repositorio.resultados.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[3]   > bdcentral.Clientes.Apellido1
"Apellido1" "Apellido1" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[4]   > bdcentral.Clientes.Apellido2
"Apellido2" "Apellido2" ? ? "character" ? ? ? ? ? ? yes ? no 16.14 yes ?
     _FldNameList[5]   > bdcentral.Clientes.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[6]   > repositorio.resultados.prestamo
"prestamo" "prestamo" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[7]   > repositorio.repositorio.int002
"int002" "int002" "Tipo Crédito" ? "integer" ? ? ? ? ? ? yes ? no 18.86 yes "Tipo Crédito"
     _FldNameList[8]   > repositorio.repositorio.int003
"int003" "int003" "Clase Crédito" ? "integer" ? ? ? ? ? ? yes ? no 19.57 yes "Clase Crédito"
     _FldNameList[9]   > bdcentral.Pro_Creditos.Nom_Producto
"Nom_Producto" "Nom_Producto" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[10]   > repositorio.resultados.PerdidaEsperada
"PerdidaEsperada" "PerdidaEsperada" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.57 yes ?
     _FldNameList[11]   > repositorio.resultados.ProbabilidadIncumplimiento
"ProbabilidadIncumplimiento" "ProbabilidadIncumplimiento" ? ? "decimal" ? ? ? ? ? ? yes ? no 26.43 yes ?
     _FldNameList[12]   > repositorio.resultados.calificacion
"calificacion" "calificacion" ? ? "character" ? ? ? ? ? ? yes ? no 10.72 yes ?
     _FldNameList[13]   > repositorio.resultados.CalificacionEquivalente
"CalificacionEquivalente" "CalificacionEquivalente" ? ? "character" ? ? ? ? ? ? yes ? no 22.29 yes ?
     _FldNameList[14]   > repositorio.resultados.ExposicionActivo
"ExposicionActivo" "ExposicionActivo" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[15]   > repositorio.resultados.PerdidaEsperada%
"PerdidaEsperada%" "PerdidaEsperada%" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.72 yes ?
     _FldNameList[16]   > repositorio.resultados.AnoContable
"AnoContable" "AnoContable" ? ? "integer" ? ? ? ? ? ? yes ? no 12.14 yes ?
     _FldNameList[17]   > repositorio.resultados.MesContable
"MesContable" "MesContable" ? ? "integer" ? ? ? ? ? ? yes ? no 12.72 yes ?
     _FldNameList[18]   > repositorio.repositorio.int001
"int001" "int001" "Agencia" "9999" "integer" ? ? ? ? ? ? yes ? no 7.29 yes "Agencia"
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

