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
&Scoped-define INTERNAL-TABLES Mov_Creditos

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  agencia Cod_Credito Cod_Operacion Cpte Descrip Fecha Hora Int_Corrientes~
 Int_MorCobrar Nit Num_Credito Num_Documento Ofi_Destino Ofi_Fuente Pagare~
 Sdo_Capital Usuario Val_Cheque Val_Efectivo
&Scoped-define ENABLED-FIELDS-IN-Mov_Creditos agencia Cod_Credito ~
Cod_Operacion Cpte Descrip Fecha Hora Int_Corrientes Int_MorCobrar Nit ~
Num_Credito Num_Documento Ofi_Destino Ofi_Fuente Pagare Sdo_Capital Usuario ~
Val_Cheque Val_Efectivo 
&Scoped-Define DATA-FIELDS  agencia Cod_Credito Cod_Operacion Cpte Descrip Fecha Hora Int_Corrientes~
 Int_MorCobrar Nit Num_Credito Num_Documento Ofi_Destino Ofi_Fuente Pagare~
 Sdo_Capital Usuario Val_Cheque Val_Efectivo FDeposito FEfeChe
&Scoped-define DATA-FIELDS-IN-Mov_Creditos agencia Cod_Credito ~
Cod_Operacion Cpte Descrip Fecha Hora Int_Corrientes Int_MorCobrar Nit ~
Num_Credito Num_Documento Ofi_Destino Ofi_Fuente Pagare Sdo_Capital Usuario ~
Val_Cheque Val_Efectivo 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dmov_creditos.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Mov_Creditos NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Mov_Creditos NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Mov_Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Mov_Creditos


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDeposito dTables  _DB-REQUIRED
FUNCTION getDeposito RETURNS DECIMAL
    (INPUT piCodOpe AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Mov_Creditos SCROLLING.
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
     _TblList          = "bdcentral.Mov_Creditos"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Mov_Creditos.agencia
"agencia" "agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[2]   > bdcentral.Mov_Creditos.Cod_Credito
"Cod_Credito" "Cod_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[3]   > bdcentral.Mov_Creditos.Cod_Operacion
"Cod_Operacion" "Cod_Operacion" ? ? "integer" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[4]   > bdcentral.Mov_Creditos.Cpte
"Cpte" "Cpte" ? ? "integer" ? ? ? ? ? ? yes ? no 12.29 yes ?
     _FldNameList[5]   > bdcentral.Mov_Creditos.Descrip
"Descrip" "Descrip" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[6]   > bdcentral.Mov_Creditos.Fecha
"Fecha" "Fecha" "Fec. Pago" ? "date" ? ? ? ? ? ? yes ? no 13.72 yes "Fec. Pago"
     _FldNameList[7]   > bdcentral.Mov_Creditos.Hora
"Hora" "Hora" ? ? "integer" ? ? ? ? ? ? yes ? no 5.72 yes ?
     _FldNameList[8]   > bdcentral.Mov_Creditos.Int_Corrientes
"Int_Corrientes" "Int_Corrientes" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[9]   > bdcentral.Mov_Creditos.Int_MorCobrar
"Int_MorCobrar" "Int_MorCobrar" ? ? "decimal" ? ? ? ? ? ? yes ? no 24.86 yes ?
     _FldNameList[10]   > bdcentral.Mov_Creditos.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[11]   > bdcentral.Mov_Creditos.Num_Credito
"Num_Credito" "Num_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[12]   > bdcentral.Mov_Creditos.Num_Documento
"Num_Documento" "Num_Documento" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[13]   > bdcentral.Mov_Creditos.Ofi_Destino
"Ofi_Destino" "Ofi_Destino" ? ? "integer" ? ? ? ? ? ? yes ? no 14.14 yes ?
     _FldNameList[14]   > bdcentral.Mov_Creditos.Ofi_Fuente
"Ofi_Fuente" "Ofi_Fuente" ? ? "integer" ? ? ? ? ? ? yes ? no 13.29 yes ?
     _FldNameList[15]   > bdcentral.Mov_Creditos.Pagare
"Pagare" "Pagare" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[16]   > bdcentral.Mov_Creditos.Sdo_Capital
"Sdo_Capital" "Sdo_Capital" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[17]   > bdcentral.Mov_Creditos.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[18]   > bdcentral.Mov_Creditos.Val_Cheque
"Val_Cheque" "Val_Cheque" ? ? "decimal" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[19]   > bdcentral.Mov_Creditos.Val_Efectivo
"Val_Efectivo" "Val_Efectivo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[20]   > "_<CALC>"
"getDeposito(INPUT RowObject.Cod_Operacion)" "FDeposito" "Deposito" ">>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 14.29 no "Deposito"
     _FldNameList[21]   > "_<CALC>"
"getCtrlEfeChe(INPUT RowObject.Cod_Operacion)" "FEfeChe" "Efec/Cheq" "X(8)" "Character" ? ? ? ? ? ? no ? no 8 no "Efe/Che"
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
         rowObject.FDeposito = (getDeposito(INPUT RowObject.Cod_Operacion))
         rowObject.FEfeChe = (getCtrlEfeChe(INPUT RowObject.Cod_Operacion))
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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDeposito dTables  _DB-REQUIRED
FUNCTION getDeposito RETURNS DECIMAL
    (INPUT piCodOpe AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  Devuelve el Consignado
            Tipo_Operacion = 2 es Credito
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdeValor AS DECIMAL INITIAL 0 NO-UNDO.

    FIND Operacion WHERE Operacion.Cod_Operacion EQ piCodOpe NO-LOCK NO-ERROR.
    IF AVAILABLE Operacion THEN DO:
        IF Operacion.Tipo_Operacion EQ 1 THEN /* Creditos */
            ASSIGN vdeValor = RowObject.Val_Efectivo + RowObject.Val_Cheque.
    END.

    RETURN vdeValor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

