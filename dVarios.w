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
&Scoped-define INTERNAL-TABLES Varios

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Codigo Descripcion Estado Tipo Comentario Comprobante Id_Gestion~
 Id_Prioridad Nit Nro_Prioridad Obligatorio Programa Val_Final Val_Inicial
&Scoped-define ENABLED-FIELDS-IN-Varios Codigo Descripcion Estado Tipo ~
Comentario Comprobante Id_Gestion Id_Prioridad Nit Nro_Prioridad ~
Obligatorio Programa Val_Final Val_Inicial 
&Scoped-Define DATA-FIELDS  Codigo Descripcion Estado Tipo Comentario Comprobante Id_Gestion~
 Id_Prioridad Nit Nro_Prioridad Obligatorio Programa Val_Final Val_Inicial~
 FVarios
&Scoped-define DATA-FIELDS-IN-Varios Codigo Descripcion Estado Tipo ~
Comentario Comprobante Id_Gestion Id_Prioridad Nit Nro_Prioridad ~
Obligatorio Programa Val_Final Val_Inicial 
&Scoped-Define MANDATORY-FIELDS  Codigo
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dvarios.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Varios NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Varios NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Varios
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Varios


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Varios SCROLLING.
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
         WIDTH              = 37.29.
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
     _TblList          = "bdcentral.Varios"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Varios.Codigo
"Codigo" "Codigo" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.43 yes ?
     _FldNameList[2]   > bdcentral.Varios.Descripcion
"Descripcion" "Descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[3]   > bdcentral.Varios.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[4]   > bdcentral.Varios.Tipo
"Tipo" "Tipo" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes ?
     _FldNameList[5]   > bdcentral.Varios.Comentario
"Comentario" "Comentario" ? ? "character" ? ? ? ? ? ? yes ? no 200 yes ?
     _FldNameList[6]   > bdcentral.Varios.Comprobante
"Comprobante" "Comprobante" ? ? "integer" ? ? ? ? ? ? yes ? no 12.29 yes ?
     _FldNameList[7]   > bdcentral.Varios.Id_Gestion
"Id_Gestion" "Id_Gestion" ? ? "logical" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[8]   > bdcentral.Varios.Id_Prioridad
"Id_Prioridad" "Id_Prioridad" ? ? "logical" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[9]   > bdcentral.Varios.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[10]   > bdcentral.Varios.Nro_Prioridad
"Nro_Prioridad" "Nro_Prioridad" ? ? "integer" ? ? ? ? ? ? yes ? no 8.43 yes ?
     _FldNameList[11]   > bdcentral.Varios.Obligatorio
"Obligatorio" "Obligatorio" ? ? "logical" ? ? ? ? ? ? yes ? no 10.14 yes ?
     _FldNameList[12]   > bdcentral.Varios.Programa
"Programa" "Programa" ? ? "integer" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[13]   > bdcentral.Varios.Val_Final
"Val_Final" "Val_Final" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[14]   > bdcentral.Varios.Val_Inicial
"Val_Inicial" "Val_Inicial" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[15]   > "_<CALC>"
"TRIM( STRING(RowObject.Codigo)) +  "" - "" + RowObject.Descripcion" "FVarios" "Nombre" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
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
         rowObject.FVarios = (TRIM( STRING(RowObject.Codigo)) +  " - " + RowObject.Descripcion)
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

