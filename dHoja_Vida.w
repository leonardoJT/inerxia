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



{incluido\iGetSdo.i}

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
&Scoped-define INTERNAL-TABLES Hoja_Vida

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Asunto_Cumplido Codigo DoctoRefer Fec_Grabacion Fec_Limite Hora_Grabacion~
 Instancia Nit Observacion Tipo Usuario
&Scoped-define ENABLED-FIELDS-IN-Hoja_Vida Asunto_Cumplido Codigo ~
DoctoRefer Fec_Grabacion Fec_Limite Hora_Grabacion Instancia Nit ~
Observacion Tipo Usuario 
&Scoped-Define DATA-FIELDS  Asunto_Cumplido Codigo DoctoRefer Fec_Grabacion Fec_Limite Hora_Grabacion~
 Instancia Nit Observacion Tipo Usuario FInstancia FUsuario FCliente~
 FHVEncabezado
&Scoped-define DATA-FIELDS-IN-Hoja_Vida Asunto_Cumplido Codigo DoctoRefer ~
Fec_Grabacion Fec_Limite Hora_Grabacion Instancia Nit Observacion Tipo ~
Usuario 
&Scoped-Define MANDATORY-FIELDS  Asunto_Cumplido Codigo Fec_Grabacion Hora_Grabacion Usuario
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dHoja_Vida.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Hoja_Vida NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Hoja_Vida NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Hoja_Vida
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Hoja_Vida


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Hoja_Vida SCROLLING.
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
     _TblList          = "bdcentral.Hoja_Vida"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Hoja_Vida.Asunto_Cumplido
"Asunto_Cumplido" "Asunto_Cumplido" ? ? "logical" ? ? ? ? ? ? yes ? yes 8.72 yes "Cumplido"
     _FldNameList[2]   > bdcentral.Hoja_Vida.Codigo
"Codigo" "Codigo" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.43 yes ?
     _FldNameList[3]   > bdcentral.Hoja_Vida.DoctoRefer
"DoctoRefer" "DoctoRefer" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.57 yes ?
     _FldNameList[4]   > bdcentral.Hoja_Vida.Fec_Grabacion
"Fec_Grabacion" "Fec_Grabacion" ? ? "date" ? ? ? ? ? ? yes ? yes 13.86 yes ?
     _FldNameList[5]   > bdcentral.Hoja_Vida.Fec_Limite
"Fec_Limite" "Fec_Limite" ? ? "date" ? ? ? ? ? ? yes ? no 10.29 yes ?
     _FldNameList[6]   > bdcentral.Hoja_Vida.Hora_Grabacion
"Hora_Grabacion" "Hora_Grabacion" ? ? "integer" ? ? ? ? ? ? yes ? yes 15 yes ?
     _FldNameList[7]   > bdcentral.Hoja_Vida.Instancia
"Instancia" "Instancia" ? ? "integer" ? ? ? ? ? ? yes ? no 8.43 yes ?
     _FldNameList[8]   > bdcentral.Hoja_Vida.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[9]   > bdcentral.Hoja_Vida.Observacion
"Observacion" "Observacion" ? ? "character" ? ? ? ? ? ? yes ? no 400 yes ?
     _FldNameList[10]   > bdcentral.Hoja_Vida.Tipo
"Tipo" "Tipo" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes ?
     _FldNameList[11]   > bdcentral.Hoja_Vida.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? yes 7.29 yes ?
     _FldNameList[12]   > "_<CALC>"
"getInstancia(INPUT RowObject.Instancia)" "FInstancia" "Instancia" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no "Instancia"
     _FldNameList[13]   > "_<CALC>"
"getUsuario(INPUT RowObject.Usuario )" "FUsuario" "Usuario" "x(35)" "character" ? ? ? ? ? ? no ? no 35 no "Usuario"
     _FldNameList[14]   > "_<CALC>"
"getCliente(INPUT RowObject.Nit)" "FCliente" "Cliente" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Cliente"
     _FldNameList[15]   > "_<CALC>"
"getHVEncabezado(INPUT RowObject.Codigo )" "FHVEncabezado" "Encabezado" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no "Encabezado"
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
         rowObject.FCliente = (getCliente(INPUT RowObject.Nit))
         rowObject.FHVEncabezado = (getHVEncabezado(INPUT RowObject.Codigo ))
         rowObject.FInstancia = (getInstancia(INPUT RowObject.Instancia))
         rowObject.FUsuario = (getUsuario(INPUT RowObject.Usuario ))
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

