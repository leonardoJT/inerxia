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
&Scoped-define INTERNAL-TABLES PlanPagos

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Capital_Acum Capital_Pdo Cargos_Acum Cargos_Pdo Categoria~
 Cod_Credito Cuota Cuo_Pagas Fec_Inic Fec_ProxPago Fec_Vcto Id_PdoMes~
 Int_LiqAcum Int_LiqPdo Int_MoraAcum Int_MoraPdo ITasa_Mora Monto_Actual Nit~
 Nro_Cuota Num_Credito Pagare Pagos_CapitalAcum Pagos_CapitalPdo~
 Pagos_IntAcum Pagos_IntPdo Pagos_MoraAcum Pagos_MoraPdo Pagos_OtrosAcum~
 Pagos_OtrosPdo Plazo Provision Tasa Tip_Credito
&Scoped-define ENABLED-FIELDS-IN-PlanPagos Agencia Capital_Acum Capital_Pdo ~
Cargos_Acum Cargos_Pdo Categoria Cod_Credito Cuota Cuo_Pagas Fec_Inic ~
Fec_ProxPago Fec_Vcto Id_PdoMes Int_LiqAcum Int_LiqPdo Int_MoraAcum ~
Int_MoraPdo ITasa_Mora Monto_Actual Nit Nro_Cuota Num_Credito Pagare ~
Pagos_CapitalAcum Pagos_CapitalPdo Pagos_IntAcum Pagos_IntPdo ~
Pagos_MoraAcum Pagos_MoraPdo Pagos_OtrosAcum Pagos_OtrosPdo Plazo Provision ~
Tasa Tip_Credito 
&Scoped-Define DATA-FIELDS  Agencia Capital_Acum Capital_Pdo Cargos_Acum Cargos_Pdo Categoria~
 Cod_Credito Cuota Cuo_Pagas Fec_Inic Fec_ProxPago Fec_Vcto Id_PdoMes~
 Int_LiqAcum Int_LiqPdo Int_MoraAcum Int_MoraPdo ITasa_Mora Monto_Actual Nit~
 Nro_Cuota Num_Credito Pagare Pagos_CapitalAcum Pagos_CapitalPdo~
 Pagos_IntAcum Pagos_IntPdo Pagos_MoraAcum Pagos_MoraPdo Pagos_OtrosAcum~
 Pagos_OtrosPdo Plazo Provision Tasa Tip_Credito
&Scoped-define DATA-FIELDS-IN-PlanPagos Agencia Capital_Acum Capital_Pdo ~
Cargos_Acum Cargos_Pdo Categoria Cod_Credito Cuota Cuo_Pagas Fec_Inic ~
Fec_ProxPago Fec_Vcto Id_PdoMes Int_LiqAcum Int_LiqPdo Int_MoraAcum ~
Int_MoraPdo ITasa_Mora Monto_Actual Nit Nro_Cuota Num_Credito Pagare ~
Pagos_CapitalAcum Pagos_CapitalPdo Pagos_IntAcum Pagos_IntPdo ~
Pagos_MoraAcum Pagos_MoraPdo Pagos_OtrosAcum Pagos_OtrosPdo Plazo Provision ~
Tasa Tip_Credito 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dplanpagos.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH PlanPagos NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH PlanPagos NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main PlanPagos
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main PlanPagos


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      PlanPagos SCROLLING.
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
     _TblList          = "bdcentral.PlanPagos"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.PlanPagos.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[2]   > bdcentral.PlanPagos.Capital_Acum
"Capital_Acum" "Capital_Acum" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.86 yes "Capital !Acumulado"
     _FldNameList[3]   > bdcentral.PlanPagos.Capital_Pdo
"Capital_Pdo" "Capital_Pdo" "Capital Período" ? "decimal" ? ? ? ? ? ? yes ? no 18.86 yes ?
     _FldNameList[4]   > bdcentral.PlanPagos.Cargos_Acum
"Cargos_Acum" "Cargos_Acum" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.86 yes ?
     _FldNameList[5]   > bdcentral.PlanPagos.Cargos_Pdo
"Cargos_Pdo" "Cargos_Pdo" "Cargos Período" ? "decimal" ? ? ? ? ? ? yes ? no 18.86 yes ?
     _FldNameList[6]   > bdcentral.PlanPagos.Categoria
"Categoria" "Categoria" ? ? "character" ? ? ? ? ? ? yes ? no 8.86 yes ?
     _FldNameList[7]   > bdcentral.PlanPagos.Cod_Credito
"Cod_Credito" "Cod_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes "Código !Producto"
     _FldNameList[8]   > bdcentral.PlanPagos.Cuota
"Cuota" "Cuota" "Cuota" ? "decimal" ? ? ? ? ? ? yes ? no 22.14 yes ?
     _FldNameList[9]   > bdcentral.PlanPagos.Cuo_Pagas
"Cuo_Pagas" "Cuo_Pagas" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[10]   > bdcentral.PlanPagos.Fec_Inic
"Fec_Inic" "Fec_Inic" "FecIni Cuota" ? "date" ? ? ? ? ? ? yes ? no 22.72 yes ?
     _FldNameList[11]   > bdcentral.PlanPagos.Fec_ProxPago
"Fec_ProxPago" "Fec_ProxPago" "FecPróx Pago" ? "date" ? ? ? ? ? ? yes ? no 19.14 yes "Fec Próx !Pago"
     _FldNameList[12]   > bdcentral.PlanPagos.Fec_Vcto
"Fec_Vcto" "Fec_Vcto" "Fec Vcto Cuota" ? "date" ? ? ? ? ? ? yes ? no 21 yes "Fec Vcto !Cuota"
     _FldNameList[13]   > bdcentral.PlanPagos.Id_PdoMes
"Id_PdoMes" "Id_PdoMes" ? ? "integer" ? ? ? ? ? ? yes ? no 10.43 yes ?
     _FldNameList[14]   > bdcentral.PlanPagos.Int_LiqAcum
"Int_LiqAcum" "Int_LiqAcum" "Int_Liq Acum" ? "decimal" ? ? ? ? ? ? yes ? no 23.29 yes ?
     _FldNameList[15]   > bdcentral.PlanPagos.Int_LiqPdo
"Int_LiqPdo" "Int_LiqPdo" "Int_Liq Período" ? "decimal" ? ? ? ? ? ? yes ? no 25.43 yes ?
     _FldNameList[16]   > bdcentral.PlanPagos.Int_MoraAcum
"Int_MoraAcum" "Int_MoraAcum" "Int_Mora Liq.Acum" ? "decimal" ? ? ? ? ? ? yes ? no 22.29 yes ?
     _FldNameList[17]   > bdcentral.PlanPagos.Int_MoraPdo
"Int_MoraPdo" "Int_MoraPdo" "Int_Mora Período" ? "decimal" ? ? ? ? ? ? yes ? no 21 yes ?
     _FldNameList[18]   > bdcentral.PlanPagos.ITasa_Mora
"ITasa_Mora" "ITasa_Mora" "Tasa Mora Per" ? "decimal" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[19]   > bdcentral.PlanPagos.Monto_Actual
"Monto_Actual" "Monto_Actual" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[20]   > bdcentral.PlanPagos.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[21]   > bdcentral.PlanPagos.Nro_Cuota
"Nro_Cuota" "Nro_Cuota" "N.Cuota" ? "integer" ? ? ? ? ? ? yes ? no 24.29 yes ?
     _FldNameList[22]   > bdcentral.PlanPagos.Num_Credito
"Num_Credito" "Num_Credito" "Núm. Crédito" ? "integer" ? ? ? ? ? ? yes ? no 17.29 yes ?
     _FldNameList[23]   > bdcentral.PlanPagos.Pagare
"Pagare" "Pagare" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[24]   > bdcentral.PlanPagos.Pagos_CapitalAcum
"Pagos_CapitalAcum" "Pagos_CapitalAcum" "Cap Pagado Acum" ? "decimal" ? ? ? ? ? ? yes ? no 24.72 yes ?
     _FldNameList[25]   > bdcentral.PlanPagos.Pagos_CapitalPdo
"Pagos_CapitalPdo" "Pagos_CapitalPdo" "Cap Pagado Período" ? "decimal" ? ? ? ? ? ? yes ? no 26.86 yes "Cap Pagado !Período"
     _FldNameList[26]   > bdcentral.PlanPagos.Pagos_IntAcum
"Pagos_IntAcum" "Pagos_IntAcum" "Int_Pag Acum" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[27]   > bdcentral.PlanPagos.Pagos_IntPdo
"Pagos_IntPdo" "Pagos_IntPdo" "Int_Pag Período" ? "decimal" ? ? ? ? ? ? yes ? no 23.29 yes ?
     _FldNameList[28]   > bdcentral.PlanPagos.Pagos_MoraAcum
"Pagos_MoraAcum" "Pagos_MoraAcum" "Int_Mora Pag Acum" ? "decimal" ? ? ? ? ? ? yes ? no 26.43 yes "Int_Mora !Pag Acum"
     _FldNameList[29]   > bdcentral.PlanPagos.Pagos_MoraPdo
"Pagos_MoraPdo" "Pagos_MoraPdo" "Int_Mora Pag Peri" ? "decimal" ? ? ? ? ? ? yes ? no 28.57 yes ?
     _FldNameList[30]   > bdcentral.PlanPagos.Pagos_OtrosAcum
"Pagos_OtrosAcum" "Pagos_OtrosAcum" "Pagos Otros Acum" ? "decimal" ? ? ? ? ? ? yes ? no 23.86 yes ?
     _FldNameList[31]   > bdcentral.PlanPagos.Pagos_OtrosPdo
"Pagos_OtrosPdo" "Pagos_OtrosPdo" "Pagos Otros Período" ? "decimal" ? ? ? ? ? ? yes ? no 24.29 yes ?
     _FldNameList[32]   > bdcentral.PlanPagos.Plazo
"Plazo" "Plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 19.43 yes ?
     _FldNameList[33]   > bdcentral.PlanPagos.Provision
"Provision" "Provision" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[34]   > bdcentral.PlanPagos.Tasa
"Tasa" "Tasa" "Tasa Per" ? "decimal" ? ? ? ? ? ? yes ? no 19.72 yes ?
     _FldNameList[35]   > bdcentral.PlanPagos.Tip_Credito
"Tip_Credito" "Tip_Credito" "Tipo Credito" ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes ?
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

