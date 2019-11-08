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
&Scoped-define INTERNAL-TABLES Asesoria

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Clase_Producto Cod_Producto Cuota Estado Fec_Apertura Fec_Asesoria~
 For_Liquidacion Id_Resultado Monto Nit Num_Asesoria Per_Deduccion~
 Per_Liquidacion Plazo Tasa Usuario Val_EgresosMes Val_IngresosMes
&Scoped-define ENABLED-FIELDS-IN-Asesoria Agencia Clase_Producto ~
Cod_Producto Cuota Estado Fec_Apertura Fec_Asesoria For_Liquidacion ~
Id_Resultado Monto Nit Num_Asesoria Per_Deduccion Per_Liquidacion Plazo ~
Tasa Usuario Val_EgresosMes Val_IngresosMes 
&Scoped-Define DATA-FIELDS  Agencia Clase_Producto Cod_Producto Cuota Estado Fec_Apertura Fec_Asesoria~
 For_Liquidacion Id_Resultado Monto Nit Num_Asesoria Per_Deduccion~
 Per_Liquidacion Plazo Tasa Usuario Val_EgresosMes Val_IngresosMes FAgencia~
 FCliente FClase_Producto FProducto FUsuario FPorcEndeud
&Scoped-define DATA-FIELDS-IN-Asesoria Agencia Clase_Producto Cod_Producto ~
Cuota Estado Fec_Apertura Fec_Asesoria For_Liquidacion Id_Resultado Monto ~
Nit Num_Asesoria Per_Deduccion Per_Liquidacion Plazo Tasa Usuario ~
Val_EgresosMes Val_IngresosMes 
&Scoped-Define MANDATORY-FIELDS  Num_Asesoria
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dasesoria.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Asesoria NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Asesoria NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Asesoria
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Asesoria


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Asesoria SCROLLING.
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
     _TblList          = "bdcentral.Asesoria"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Asesoria.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[2]   > bdcentral.Asesoria.Clase_Producto
"Clase_Producto" "Clase_Producto" ? ? "integer" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[3]   > bdcentral.Asesoria.Cod_Producto
"Cod_Producto" "Cod_Producto" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[4]   > bdcentral.Asesoria.Cuota
"Cuota" "Cuota" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.72 yes ?
     _FldNameList[5]   > bdcentral.Asesoria.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[6]   > bdcentral.Asesoria.Fec_Apertura
"Fec_Apertura" "Fec_Apertura" ? ? "date" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[7]   > bdcentral.Asesoria.Fec_Asesoria
"Fec_Asesoria" "Fec_Asesoria" ? ? "date" ? ? ? ? ? ? yes ? no 17.14 yes ?
     _FldNameList[8]   > bdcentral.Asesoria.For_Liquidacion
"For_Liquidacion" "For_Liquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 10.72 yes ?
     _FldNameList[9]   > bdcentral.Asesoria.Id_Resultado
"Id_Resultado" "Id_Resultado" ? ? "integer" ? ? ? ? ? ? yes ? no 9.43 yes ?
     _FldNameList[10]   > bdcentral.Asesoria.Monto
"Monto" "Monto" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[11]   > bdcentral.Asesoria.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[12]   > bdcentral.Asesoria.Num_Asesoria
"Num_Asesoria" "Num_Asesoria" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.86 yes ?
     _FldNameList[13]   > bdcentral.Asesoria.Per_Deduccion
"Per_Deduccion" "Per_Deduccion" ? ? "integer" ? ? ? ? ? ? yes ? no 17.43 yes ?
     _FldNameList[14]   > bdcentral.Asesoria.Per_Liquidacion
"Per_Liquidacion" "Per_Liquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 21.86 yes ?
     _FldNameList[15]   > bdcentral.Asesoria.Plazo
"Plazo" "Plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[16]   > bdcentral.Asesoria.Tasa
"Tasa" "Tasa" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[17]   > bdcentral.Asesoria.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[18]   > bdcentral.Asesoria.Val_EgresosMes
"Val_EgresosMes" "Val_EgresosMes" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.72 yes ?
     _FldNameList[19]   > bdcentral.Asesoria.Val_IngresosMes
"Val_IngresosMes" "Val_IngresosMes" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.14 yes ?
     _FldNameList[20]   > "_<CALC>"
"getAgencia(INPUT RowObject.Agencia)" "FAgencia" "Agencia" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Agencia"
     _FldNameList[21]   > "_<CALC>"
"getCliente(INPUT RowObject.Nit)" "FCliente" "Cliente" "x(50)" "character" ? ? ? ? ? ? no ? no 50 no "Cliente"
     _FldNameList[22]   > "_<CALC>"
"IF (RowObject.Clase_Producto = 1) THEN (""AHORRO"") ELSE ( IF (RowObject.Clase_Producto = 2) THEN (""CREDITO"") ELSE (""???""))" "FClase_Producto" "Clase Prod." "x(12)" "character" ? ? ? ? ? ? no ? no 12 no "Clase Prod."
     _FldNameList[23]   > "_<CALC>"
"getProducto(INPUT RowObject.Clase_Producto,
INPUT RowObject.Cod_Producto)" "FProducto" "Producto" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Producto"
     _FldNameList[24]   > "_<CALC>"
"getUsuario(INPUT RowObject.Usuario)" "FUsuario" "Usuario" "x(45)" "character" ? ? ? ? ? ? no ? no 45 no "Usuario"
     _FldNameList[25]   > "_<CALC>"
"(DECIMAL(RowObject.Val_EgresosMes) + DECIMAL(RowObject.Cuota)) / (DECIMAL(RowObject.Val_IngresosMes)) * 100" "FPorcEndeud" "%Endeudam." ">>9.99" "Decimal" ? ? ? ? ? ? no ? no 12 no "%Endeudam."
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
         rowObject.FAgencia = (getAgencia(INPUT RowObject.Agencia))
         rowObject.FClase_Producto = (IF (RowObject.Clase_Producto = 1) THEN ("AHORRO") ELSE ( IF (RowObject.Clase_Producto = 2) THEN ("CREDITO") ELSE ("???")))
         rowObject.FCliente = (getCliente(INPUT RowObject.Nit))
         rowObject.FPorcEndeud = ((DECIMAL(RowObject.Val_EgresosMes) + DECIMAL(RowObject.Cuota)) / (DECIMAL(RowObject.Val_IngresosMes)) * 100)
         rowObject.FProducto = (getProducto(INPUT RowObject.Clase_Producto,
INPUT RowObject.Cod_Producto))
         rowObject.FUsuario = (getUsuario(INPUT RowObject.Usuario))
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

