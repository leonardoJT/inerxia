&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T1Instancias NO-UNDO LIKE Instancias.
DEFINE TEMP-TABLE T1Mov_Instancias NO-UNDO LIKE Mov_Instancias.



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

&glob DATA-LOGIC-PROCEDURE .p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

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
&Scoped-define INTERNAL-TABLES Mov_Instancias

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Cuenta Descripcion Estado Fec_Ingreso Fec_Retiro Hora_Ingreso~
 Hora_Retiro Instancia Nit Num_Solicitud Usuario
&Scoped-define ENABLED-FIELDS-IN-Mov_Instancias Agencia Cuenta Descripcion ~
Estado Fec_Ingreso Fec_Retiro Hora_Ingreso Hora_Retiro Instancia Nit ~
Num_Solicitud Usuario 
&Scoped-Define DATA-FIELDS  Agencia Cuenta Descripcion Estado Fec_Ingreso Fec_Retiro Hora_Ingreso~
 Hora_Retiro Instancia Nit Num_Solicitud Usuario
&Scoped-define DATA-FIELDS-IN-Mov_Instancias Agencia Cuenta Descripcion ~
Estado Fec_Ingreso Fec_Retiro Hora_Ingreso Hora_Retiro Instancia Nit ~
Num_Solicitud Usuario 
&Scoped-Define MANDATORY-FIELDS  Agencia Fec_Ingreso Num_Solicitud Usuario
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "H:\prg\dMov_Instancias.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Mov_Instancias NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Mov_Instancias NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Mov_Instancias
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Mov_Instancias


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Mov_Instancias SCROLLING.
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
      TABLE: T1Instancias T "?" NO-UNDO bdcentral Instancias
      TABLE: T1Mov_Instancias T "?" NO-UNDO bdcentral Mov_Instancias
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
     _TblList          = "bdcentral.Mov_Instancias"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Mov_Instancias.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.29 yes ?
     _FldNameList[2]   > bdcentral.Mov_Instancias.Cuenta
"Cuenta" "Cuenta" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[3]   > bdcentral.Mov_Instancias.Descripcion
"Descripcion" "Descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 500 yes ?
     _FldNameList[4]   > bdcentral.Mov_Instancias.Estado
"Estado" "Estado" ? ? "logical" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[5]   > bdcentral.Mov_Instancias.Fec_Ingreso
"Fec_Ingreso" "Fec_Ingreso" ? ? "date" ? ? ? ? ? ? yes ? yes 13.14 yes ?
     _FldNameList[6]   > bdcentral.Mov_Instancias.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 11.72 yes ?
     _FldNameList[7]   > bdcentral.Mov_Instancias.Hora_Ingreso
"Hora_Ingreso" "Hora_Ingreso" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[8]   > bdcentral.Mov_Instancias.Hora_Retiro
"Hora_Retiro" "Hora_Retiro" ? ? "integer" ? ? ? ? ? ? yes ? no 10.57 yes ?
     _FldNameList[9]   > bdcentral.Mov_Instancias.Instancia
"Instancia" "Instancia" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[10]   > bdcentral.Mov_Instancias.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[11]   > bdcentral.Mov_Instancias.Num_Solicitud
"Num_Solicitud" "Num_Solicitud" ? ? "integer" ? ? ? ? ? ? yes ? yes 11.86 yes ?
     _FldNameList[12]   > bdcentral.Mov_Instancias.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? yes 12 yes ?
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

