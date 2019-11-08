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
&Scoped-define INTERNAL-TABLES Instancias

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Cod_Operacion Cod_Producto Estado Fec_Creacion Fec_Retiro Honorarios~
 Id_Abogado Id_Concepto Id_Negadas Id_Scoring Instancia Nom_Instancia~
 Orden_Instancia Primera Programa Tipo_Instancia Tipo_Producto TMI Ultima
&Scoped-define ENABLED-FIELDS-IN-Instancias Cod_Operacion Cod_Producto ~
Estado Fec_Creacion Fec_Retiro Honorarios Id_Abogado Id_Concepto Id_Negadas ~
Id_Scoring Instancia Nom_Instancia Orden_Instancia Primera Programa ~
Tipo_Instancia Tipo_Producto TMI Ultima 
&Scoped-Define DATA-FIELDS  Cod_Operacion Cod_Producto Estado Fec_Creacion Fec_Retiro Honorarios~
 Id_Abogado Id_Concepto Id_Negadas Id_Scoring Instancia Nom_Instancia~
 Orden_Instancia Primera Programa Tipo_Instancia Tipo_Producto TMI Ultima
&Scoped-define DATA-FIELDS-IN-Instancias Cod_Operacion Cod_Producto Estado ~
Fec_Creacion Fec_Retiro Honorarios Id_Abogado Id_Concepto Id_Negadas ~
Id_Scoring Instancia Nom_Instancia Orden_Instancia Primera Programa ~
Tipo_Instancia Tipo_Producto TMI Ultima 
&Scoped-Define MANDATORY-FIELDS  Tipo_Instancia
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "h:\prg\dinstancia.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Instancias NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Instancias NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Instancias
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Instancias


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Instancias SCROLLING.
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
         WIDTH              = 32.86.
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
     _TblList          = "bdcentral.Instancias"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Instancias.Cod_Operacion
"Cod_Operacion" "Cod_Operacion" ? ? "integer" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[2]   > bdcentral.Instancias.Cod_Producto
"Cod_Producto" "Cod_Producto" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[3]   > bdcentral.Instancias.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[4]   > bdcentral.Instancias.Fec_Creacion
"Fec_Creacion" "Fec_Creacion" ? ? "date" ? ? ? ? ? ? yes ? no 13.86 yes ?
     _FldNameList[5]   > bdcentral.Instancias.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[6]   > bdcentral.Instancias.Honorarios
"Honorarios" "Honorarios" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.29 yes ?
     _FldNameList[7]   > bdcentral.Instancias.Id_Abogado
"Id_Abogado" "Id_Abogado" ? ? "logical" ? ? ? ? ? ? yes ? no 10.86 yes ?
     _FldNameList[8]   > bdcentral.Instancias.Id_Concepto
"Id_Concepto" "Id_Concepto" ? ? "logical" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[9]   > bdcentral.Instancias.Id_Negadas
"Id_Negadas" "Id_Negadas" ? ? "logical" ? ? ? ? ? ? yes ? no 8.29 yes ?
     _FldNameList[10]   > bdcentral.Instancias.Id_Scoring
"Id_Scoring" "Id_Scoring" ? ? "logical" ? ? ? ? ? ? yes ? no 9.86 yes ?
     _FldNameList[11]   > bdcentral.Instancias.Instancia
"Instancia" "Instancia" ? ? "integer" ? ? ? ? ? ? yes ? no 8.43 yes ?
     _FldNameList[12]   > bdcentral.Instancias.Nom_Instancia
"Nom_Instancia" "Nom_Instancia" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[13]   > bdcentral.Instancias.Orden_Instancia
"Orden_Instancia" "Orden_Instancia" ? ? "integer" ? ? ? ? ? ? yes ? no 5.57 yes ?
     _FldNameList[14]   > bdcentral.Instancias.Primera
"Primera" "Primera" ? ? "logical" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[15]   > bdcentral.Instancias.Programa
"Programa" "Programa" ? ? "integer" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[16]   > bdcentral.Instancias.Tipo_Instancia
"Tipo_Instancia" "Tipo_Instancia" ? ? "integer" ? ? ? ? ? ? yes ? yes 5.72 yes ?
     _FldNameList[17]   > bdcentral.Instancias.Tipo_Producto
"Tipo_Producto" "Tipo_Producto" ? ? "integer" ? ? ? ? ? ? yes ? no 12.72 yes ?
     _FldNameList[18]   > bdcentral.Instancias.TMI
"TMI" "TMI" ? ? "integer" ? ? ? ? ? ? yes ? no 4.57 yes ?
     _FldNameList[19]   > bdcentral.Instancias.Ultima
"Ultima" "Ultima" ? ? "logical" ? ? ? ? ? ? yes ? no 6 yes ?
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

