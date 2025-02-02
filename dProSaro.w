&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T1Cfg_Instancias NO-UNDO LIKE Cfg_Instancias.
DEFINE TEMP-TABLE T1Instancias NO-UNDO LIKE Instancias.
DEFINE TEMP-TABLE T1Saro NO-UNDO LIKE Saro.
DEFINE TEMP-TABLE T2Saro NO-UNDO LIKE Saro.



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
&Scoped-define INTERNAL-TABLES Pro_Saro

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Actividad Cargo_Resp Clase_ROperativo Cod_Controles Cod_EveFactor~
 Cod_Factor Cod_Probabilidad Cod_Severidad Cod_UbFactor Descripcion_Con~
 Des_Controles Estado Fec_Creacion Fec_Retiro Nombre Proceso~
 Responsable_Cont Subproceso Tipo
&Scoped-define ENABLED-FIELDS-IN-Pro_Saro Actividad Cargo_Resp ~
Clase_ROperativo Cod_Controles Cod_EveFactor Cod_Factor Cod_Probabilidad ~
Cod_Severidad Cod_UbFactor Descripcion_Con Des_Controles Estado ~
Fec_Creacion Fec_Retiro Nombre Proceso Responsable_Cont Subproceso Tipo 
&Scoped-Define DATA-FIELDS  Actividad Cargo_Resp Clase_ROperativo Cod_Controles Cod_EveFactor~
 Cod_Factor Cod_Probabilidad Cod_Severidad Cod_UbFactor Descripcion_Con~
 Des_Controles Estado Fec_Creacion Fec_Retiro Nombre Proceso~
 Responsable_Cont Subproceso Tipo
&Scoped-define DATA-FIELDS-IN-Pro_Saro Actividad Cargo_Resp ~
Clase_ROperativo Cod_Controles Cod_EveFactor Cod_Factor Cod_Probabilidad ~
Cod_Severidad Cod_UbFactor Descripcion_Con Des_Controles Estado ~
Fec_Creacion Fec_Retiro Nombre Proceso Responsable_Cont Subproceso Tipo 
&Scoped-Define MANDATORY-FIELDS  Actividad Cargo_Resp Cod_Controles Cod_Factor Cod_Probabilidad~
 Cod_Severidad Descripcion_Con Des_Controles Estado Fec_Creacion Nombre~
 Proceso Responsable_Cont Subproceso
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dProSaro.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Pro_Saro NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Pro_Saro NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Pro_Saro
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Pro_Saro


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Pro_Saro SCROLLING.
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
   Temp-Tables and Buffers:
      TABLE: T1Cfg_Instancias T "?" NO-UNDO bdcentral Cfg_Instancias
      TABLE: T1Instancias T "?" NO-UNDO bdcentral Instancias
      TABLE: T1Saro T "?" NO-UNDO bdcentral Saro
      TABLE: T2Saro T "?" NO-UNDO bdcentral Saro
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
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 27.72.
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
     _TblList          = "bdcentral.Pro_Saro"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Pro_Saro.Actividad
"Actividad" "Actividad" ? ? "character" ? ? ? ? ? ? yes ? yes 120 yes ?
     _FldNameList[2]   > bdcentral.Pro_Saro.Cargo_Resp
"Cargo_Resp" "Cargo_Resp" ? ? "character" ? ? ? ? ? ? yes ? yes 120 yes ?
     _FldNameList[3]   > bdcentral.Pro_Saro.Clase_ROperativo
"Clase_ROperativo" "Clase_ROperativo" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[4]   > bdcentral.Pro_Saro.Cod_Controles
"Cod_Controles" "Cod_Controles" ? ? "integer" ? ? ? ? ? ? yes ? yes 14.86 yes ?
     _FldNameList[5]   > bdcentral.Pro_Saro.Cod_EveFactor
"Cod_EveFactor" "Cod_EveFactor" ? ? "integer" ? ? ? ? ? ? yes ? no 12.14 yes ?
     _FldNameList[6]   > bdcentral.Pro_Saro.Cod_Factor
"Cod_Factor" "Cod_Factor" ? ? "integer" ? ? ? ? ? ? yes ? yes 12.72 yes ?
     _FldNameList[7]   > bdcentral.Pro_Saro.Cod_Probabilidad
"Cod_Probabilidad" "Cod_Probabilidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 17.86 yes ?
     _FldNameList[8]   > bdcentral.Pro_Saro.Cod_Severidad
"Cod_Severidad" "Cod_Severidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.72 yes ?
     _FldNameList[9]   > bdcentral.Pro_Saro.Cod_UbFactor
"Cod_UbFactor" "Cod_UbFactor" ? ? "integer" ? ? ? ? ? ? yes ? no 21.29 yes ?
     _FldNameList[10]   > bdcentral.Pro_Saro.Descripcion_Con
"Descripcion_Con" "Descripcion_Con" ? ? "character" ? ? ? ? ? ? yes ? yes 120 yes ?
     _FldNameList[11]   > bdcentral.Pro_Saro.Des_Controles
"Des_Controles" "Des_Controles" ? ? "character" ? ? ? ? ? ? yes ? yes 120 yes ?
     _FldNameList[12]   > bdcentral.Pro_Saro.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.14 yes ?
     _FldNameList[13]   > bdcentral.Pro_Saro.Fec_Creacion
"Fec_Creacion" "Fec_Creacion" ? ? "date" ? ? ? ? ? ? yes ? yes 16.29 yes ?
     _FldNameList[14]   > bdcentral.Pro_Saro.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 13.72 yes ?
     _FldNameList[15]   > bdcentral.Pro_Saro.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? yes ? yes 250 yes ?
     _FldNameList[16]   > bdcentral.Pro_Saro.Proceso
"Proceso" "Proceso" ? ? "character" ? ? ? ? ? ? yes ? yes 80 yes ?
     _FldNameList[17]   > bdcentral.Pro_Saro.Responsable_Cont
"Responsable_Cont" "Responsable_Cont" ? ? "character" ? ? ? ? ? ? yes ? yes 120 yes ?
     _FldNameList[18]   > bdcentral.Pro_Saro.Subproceso
"Subproceso" "Subproceso" ? ? "character" ? ? ? ? ? ? yes ? yes 80 yes ?
     _FldNameList[19]   > bdcentral.Pro_Saro.Tipo
"Tipo" "Tipo" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable dTables  _DB-REQUIRED
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).

  /* Code placed here will execute AFTER standard behavior.    */
    PUBLISH "ProSaro" (rowobject.cod_factor).
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

