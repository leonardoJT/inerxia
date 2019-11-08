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


{incluido\igetSdo.i}


    {Incluido/Variable.I "SHARED"}
    /*     {Incluido/Variable.I} /*giocam*/ */
    /*                                      */
    /*         ASSIGN W_Usuario = "339"     */
    /*                  W_Agencia = 1       */
    /*                  W_fecha = TODAY.    */

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
&Scoped-define INTERNAL-TABLES regalos_entrega

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  regalo agencia fecha_entrega hora nit nit_relacion usuario
&Scoped-define ENABLED-FIELDS-IN-regalos_entrega regalo agencia ~
fecha_entrega hora nit nit_relacion usuario 
&Scoped-Define DATA-FIELDS  regalo agencia fecha_entrega hora nit nit_relacion usuario FAgencia~
 FCliente FRelacion FRegalo FHora
&Scoped-define DATA-FIELDS-IN-regalos_entrega regalo agencia fecha_entrega ~
hora nit nit_relacion usuario 
&Scoped-Define MANDATORY-FIELDS  regalo agencia fecha_entrega nit usuario
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dregalos_entrega.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH regalos_entrega NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH regalos_entrega NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main regalos_entrega
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main regalos_entrega


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      regalos_entrega SCROLLING.
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
         WIDTH              = 43.86.
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
     _TblList          = "bdcentral.regalos_entrega"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.regalos_entrega.regalo
"regalo" "regalo" ? ? "integer" ? ? ? ? ? ? yes ? yes 14 yes ?
     _FldNameList[2]   > bdcentral.regalos_entrega.agencia
"agencia" "agencia" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.8 yes ?
     _FldNameList[3]   > bdcentral.regalos_entrega.fecha_entrega
"fecha_entrega" "fecha_entrega" ? ? "date" ? ? ? ? ? ? yes ? yes 14 yes ?
     _FldNameList[4]   > bdcentral.regalos_entrega.hora
"hora" "hora" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 yes ?
     _FldNameList[5]   > bdcentral.regalos_entrega.nit
"nit" "nit" ? ? "character" ? ? ? ? ? ? yes ? yes 12 yes ?
     _FldNameList[6]   > bdcentral.regalos_entrega.nit_relacion
"nit_relacion" "nit_relacion" ? ? "character" ? ? ? ? ? ? yes ? no 14.2 yes ?
     _FldNameList[7]   > bdcentral.regalos_entrega.usuario
"usuario" "usuario" ? ? "character" ? ? ? ? ? ? yes ? yes 7.2 yes ?
     _FldNameList[8]   > "_<CALC>"
"getAgencia(INPUT RowObject.agencia)" "FAgencia" "Agencia" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no "Agencia"
     _FldNameList[9]   > "_<CALC>"
"getCliente(INPUT RowObject.nit)" "FCliente" "Cliente" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Cliente"
     _FldNameList[10]   > "_<CALC>"
"getCliente(INPUT RowObject.nit_relacion)" "FRelacion" "Beneficiario" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Beneficiario"
     _FldNameList[11]   > "_<CALC>"
"getRegalo(INPUT RowObject.regalo)" "FRegalo" "Regalo" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Regalo"
     _FldNameList[12]   > "_<CALC>"
"string(RowObject.hora,""HH:MM:SS"")" "FHora" "Hora" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no "Hora"
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
         rowObject.FAgencia = (getAgencia(INPUT RowObject.agencia))
         rowObject.FCliente = (getCliente(INPUT RowObject.nit))
         rowObject.FHora = (string(RowObject.hora,"HH:MM:SS"))
         rowObject.FRegalo = (getRegalo(INPUT RowObject.regalo))
         rowObject.FRelacion = (getCliente(INPUT RowObject.nit_relacion))
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */


    FOR EACH RowObjUpd WHERE LOOKUP(RowObjUpd.RowMod, "A,C,U,D":U) NE 0:
      IF LOOKUP(RowObjUpd.RowMod, "A,C":U) NE 0 THEN DO:
        FIND FIRST regalos_entrega WHERE regalos_entrega.regalo EQ RowObjUpd.regalo AND
            regalos_entrega.nit EQ RowObjUpd.nit AND
            regalos_entrega.nit_relacion EQ RowObjUpd.nit_relacion NO-LOCK NO-ERROR.
        IF AVAILABLE regalos_entrega THEN DO:
            RETURN "El regalo ya fue entregado en Agencia " + STRING(regalos_entrega.agencia) + " Usuario " + regalos_entrega.usuario.
        END.
      END.
    END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

