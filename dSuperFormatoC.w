&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
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
&Scoped-define INTERNAL-TABLES SuperFormatoC

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Codigo DocumentoTecnico FormatoExcel Nombre Observaciones Periodicidad~
 Proforma circular TipoYNumero AreaInformacion EnDolares EnMiles~
 NombreArchivoSalida
&Scoped-define ENABLED-FIELDS-IN-SuperFormatoC Codigo DocumentoTecnico ~
FormatoExcel Nombre Observaciones Periodicidad Proforma circular ~
TipoYNumero AreaInformacion EnDolares EnMiles NombreArchivoSalida 
&Scoped-Define DATA-FIELDS  Codigo DocumentoTecnico FormatoExcel Nombre Observaciones Periodicidad~
 Proforma circular TipoYNumero AreaInformacion EnDolares EnMiles~
 NombreArchivoSalida
&Scoped-define DATA-FIELDS-IN-SuperFormatoC Codigo DocumentoTecnico ~
FormatoExcel Nombre Observaciones Periodicidad Proforma circular ~
TipoYNumero AreaInformacion EnDolares EnMiles NombreArchivoSalida 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dsuperformatoc.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH SuperFormatoC NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH SuperFormatoC NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main SuperFormatoC
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main SuperFormatoC


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      SuperFormatoC SCROLLING.
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
     _TblList          = "bdcentral.SuperFormatoC"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.SuperFormatoC.Codigo
"Codigo" "Codigo" ? ? "character" ? ? ? ? ? ? yes ? no 14.57 yes "Código!Formato"
     _FldNameList[2]   > bdcentral.SuperFormatoC.DocumentoTecnico
"DocumentoTecnico" "DocumentoTecnico" ? "x(30)" "character" ? ? ? ? ? ? yes ? no 30 yes "Documento!Técnico"
     _FldNameList[3]   > bdcentral.SuperFormatoC.FormatoExcel
"FormatoExcel" "FormatoExcel" ? "x(30)" "character" ? ? ? ? ? ? yes ? no 30 yes "Formato!Excel"
     _FldNameList[4]   > bdcentral.SuperFormatoC.Nombre
"Nombre" "Nombre" ? "x(60)" "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[5]   > bdcentral.SuperFormatoC.Observaciones
"Observaciones" "Observaciones" ? "x(256)" "character" ? ? ? ? ? ? yes ? no 256 yes ?
     _FldNameList[6]   > bdcentral.SuperFormatoC.Periodicidad
"Periodicidad" "Periodicidad" ? "x(30)" "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[7]   > bdcentral.SuperFormatoC.Proforma
"Proforma" "Proforma" ? "x(30)" "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[8]   > bdcentral.SuperFormatoC.circular
"circular" "circular" ? "x(30)" "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[9]   > bdcentral.SuperFormatoC.TipoYNumero
"TipoYNumero" "TipoYNumero" ? ? "integer" ? ? ? ? ? ? yes ? no 25.14 yes "Tipo!Número!Informe"
     _FldNameList[10]   > bdcentral.SuperFormatoC.AreaInformacion
"AreaInformacion" "AreaInformacion" ? ? "integer" ? ? ? ? ? ? yes ? no 18.72 yes "Area!Información"
     _FldNameList[11]   > bdcentral.SuperFormatoC.EnDolares
"EnDolares" "EnDolares" ? ? "character" ? ? ? ? ? ? yes ? no 10.29 yes ?
     _FldNameList[12]   > bdcentral.SuperFormatoC.EnMiles
"EnMiles" "EnMiles" ? ? "character" ? ? ? ? ? ? yes ? no 13.43 yes "En Miles"
     _FldNameList[13]   > bdcentral.SuperFormatoC.NombreArchivoSalida
"NombreArchivoSalida" "NombreArchivoSalida" ? "x(60)" "character" ? ? ? ? ? ? yes ? no 60 yes ?
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

