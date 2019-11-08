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
&Scoped-define INTERNAL-TABLES Ahorros

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Agencia_Destino Age_DebAutomatico Ajuste Clave Cod_ahorro~
 Cod_DebAutomatico Conjuncion Cue_Ahorros Cue_DebAutomatico Cue_Destino~
 Cue_Padre Cuota Des_Intereses Detalle_Estado Dias_Causar Dia_Sobregiro~
 Estado Exento_3xm Fec_Apertura Fec_BloqueoTdb Fec_Cancelacion Fec_CancTdb~
 Fec_CreaTdb Fec_ProLiquidacion Fec_Prorroga Fec_ProxDeb Fec_UltCausacion~
 Fec_UltDeb Fec_UltLiquidacion Fec_Ulttransaccion Fec_Vencimiento~
 For_Liquidacion For_Pago IdApellido1 IdNombre Id_ForLiq Id_Sobregiro~
 Id_TasDiferencial Ind_TasVariable Int_Causado Int_Pagar Int_Sobregiro~
 Monto_Apertura Nit Num_Asesoria Num_DepDia Num_DepMes Num_Formato~
 Num_RetDia Num_RetDiaCheq Num_RetMes Per_Deduccion Per_Liquidacion Plazo~
 Pro_Destino Pun_TasVariable Sal_Intpagados Sdo_Anuales1 Sdo_Anuales2~
 Sdo_Anuales3 Sdo_Anuales4 Sdo_Anuales5 Sdo_Anuales6 Sdo_Anuales7~
 Sdo_Anuales8 Sdo_Anuales9 Sdo_Anuales10 Sdo_Anuales11 Sdo_Anuales12~
 Sdo_AnualPerAnt1 Sdo_AnualPerAnt2 Sdo_AnualPerAnt3 Sdo_AnualPerAnt4~
 Sdo_AnualPerAnt5 Sdo_AnualPerAnt6 Sdo_AnualPerAnt7 Sdo_AnualPerAnt8~
 Sdo_AnualPerAnt9 Sdo_AnualPerAnt10 Sdo_AnualPerAnt11 Sdo_AnualPerAnt12~
 Sdo_Canje Sdo_Disponible Sdo_Inicial Sdo_MinCta Sdo_Minimo Sdo_Revaloriza~
 Sdo_UltLiquidacion TarjetaDB Tasa Tip-Tasa Tip_ahorro TRF TRF_Notas~
 Usu_Creacion Val_DebAutPdo Val_DepDia Val_DepMes Val_Embargo Val_Pignorar~
 Val_RetAcum Val_RetDia Val_RetDiaCheq Val_RetMes Val_RetPeriodo
&Scoped-define ENABLED-FIELDS-IN-Ahorros Agencia Agencia_Destino ~
Age_DebAutomatico Ajuste Clave Cod_ahorro Cod_DebAutomatico Conjuncion ~
Cue_Ahorros Cue_DebAutomatico Cue_Destino Cue_Padre Cuota Des_Intereses ~
Detalle_Estado Dias_Causar Dia_Sobregiro Estado Exento_3xm Fec_Apertura ~
Fec_BloqueoTdb Fec_Cancelacion Fec_CancTdb Fec_CreaTdb Fec_ProLiquidacion ~
Fec_Prorroga Fec_ProxDeb Fec_UltCausacion Fec_UltDeb Fec_UltLiquidacion ~
Fec_Ulttransaccion Fec_Vencimiento For_Liquidacion For_Pago IdApellido1 ~
IdNombre Id_ForLiq Id_Sobregiro Id_TasDiferencial Ind_TasVariable ~
Int_Causado Int_Pagar Int_Sobregiro Monto_Apertura Nit Num_Asesoria ~
Num_DepDia Num_DepMes Num_Formato Num_RetDia Num_RetDiaCheq Num_RetMes ~
Per_Deduccion Per_Liquidacion Plazo Pro_Destino Pun_TasVariable ~
Sal_Intpagados Sdo_Anuales1 Sdo_Anuales2 Sdo_Anuales3 Sdo_Anuales4 ~
Sdo_Anuales5 Sdo_Anuales6 Sdo_Anuales7 Sdo_Anuales8 Sdo_Anuales9 ~
Sdo_Anuales10 Sdo_Anuales11 Sdo_Anuales12 Sdo_AnualPerAnt1 Sdo_AnualPerAnt2 ~
Sdo_AnualPerAnt3 Sdo_AnualPerAnt4 Sdo_AnualPerAnt5 Sdo_AnualPerAnt6 ~
Sdo_AnualPerAnt7 Sdo_AnualPerAnt8 Sdo_AnualPerAnt9 Sdo_AnualPerAnt10 ~
Sdo_AnualPerAnt11 Sdo_AnualPerAnt12 Sdo_Canje Sdo_Disponible Sdo_Inicial ~
Sdo_MinCta Sdo_Minimo Sdo_Revaloriza Sdo_UltLiquidacion TarjetaDB Tasa ~
Tip-Tasa Tip_ahorro TRF TRF_Notas Usu_Creacion Val_DebAutPdo Val_DepDia ~
Val_DepMes Val_Embargo Val_Pignorar Val_RetAcum Val_RetDia Val_RetDiaCheq ~
Val_RetMes Val_RetPeriodo 
&Scoped-Define DATA-FIELDS  Agencia Agencia_Destino Age_DebAutomatico Ajuste Clave Cod_ahorro~
 Cod_DebAutomatico Conjuncion Cue_Ahorros Cue_DebAutomatico Cue_Destino~
 Cue_Padre Cuota Des_Intereses Detalle_Estado Dias_Causar Dia_Sobregiro~
 Estado Exento_3xm Fec_Apertura Fec_BloqueoTdb Fec_Cancelacion Fec_CancTdb~
 Fec_CreaTdb Fec_ProLiquidacion Fec_Prorroga Fec_ProxDeb Fec_UltCausacion~
 Fec_UltDeb Fec_UltLiquidacion Fec_Ulttransaccion Fec_Vencimiento~
 For_Liquidacion For_Pago IdApellido1 IdNombre Id_ForLiq Id_Sobregiro~
 Id_TasDiferencial Ind_TasVariable Int_Causado Int_Pagar Int_Sobregiro~
 Monto_Apertura Nit Num_Asesoria Num_DepDia Num_DepMes Num_Formato~
 Num_RetDia Num_RetDiaCheq Num_RetMes Per_Deduccion Per_Liquidacion Plazo~
 Pro_Destino Pun_TasVariable Sal_Intpagados Sdo_Anuales1 Sdo_Anuales2~
 Sdo_Anuales3 Sdo_Anuales4 Sdo_Anuales5 Sdo_Anuales6 Sdo_Anuales7~
 Sdo_Anuales8 Sdo_Anuales9 Sdo_Anuales10 Sdo_Anuales11 Sdo_Anuales12~
 Sdo_AnualPerAnt1 Sdo_AnualPerAnt2 Sdo_AnualPerAnt3 Sdo_AnualPerAnt4~
 Sdo_AnualPerAnt5 Sdo_AnualPerAnt6 Sdo_AnualPerAnt7 Sdo_AnualPerAnt8~
 Sdo_AnualPerAnt9 Sdo_AnualPerAnt10 Sdo_AnualPerAnt11 Sdo_AnualPerAnt12~
 Sdo_Canje Sdo_Disponible Sdo_Inicial Sdo_MinCta Sdo_Minimo Sdo_Revaloriza~
 Sdo_UltLiquidacion TarjetaDB Tasa Tip-Tasa Tip_ahorro TRF TRF_Notas~
 Usu_Creacion Val_DebAutPdo Val_DepDia Val_DepMes Val_Embargo Val_Pignorar~
 Val_RetAcum Val_RetDia Val_RetDiaCheq Val_RetMes Val_RetPeriodo FAgencia~
 FAgenciaDest FAporte FCliente FProducto
&Scoped-define DATA-FIELDS-IN-Ahorros Agencia Agencia_Destino ~
Age_DebAutomatico Ajuste Clave Cod_ahorro Cod_DebAutomatico Conjuncion ~
Cue_Ahorros Cue_DebAutomatico Cue_Destino Cue_Padre Cuota Des_Intereses ~
Detalle_Estado Dias_Causar Dia_Sobregiro Estado Exento_3xm Fec_Apertura ~
Fec_BloqueoTdb Fec_Cancelacion Fec_CancTdb Fec_CreaTdb Fec_ProLiquidacion ~
Fec_Prorroga Fec_ProxDeb Fec_UltCausacion Fec_UltDeb Fec_UltLiquidacion ~
Fec_Ulttransaccion Fec_Vencimiento For_Liquidacion For_Pago IdApellido1 ~
IdNombre Id_ForLiq Id_Sobregiro Id_TasDiferencial Ind_TasVariable ~
Int_Causado Int_Pagar Int_Sobregiro Monto_Apertura Nit Num_Asesoria ~
Num_DepDia Num_DepMes Num_Formato Num_RetDia Num_RetDiaCheq Num_RetMes ~
Per_Deduccion Per_Liquidacion Plazo Pro_Destino Pun_TasVariable ~
Sal_Intpagados Sdo_Anuales1 Sdo_Anuales2 Sdo_Anuales3 Sdo_Anuales4 ~
Sdo_Anuales5 Sdo_Anuales6 Sdo_Anuales7 Sdo_Anuales8 Sdo_Anuales9 ~
Sdo_Anuales10 Sdo_Anuales11 Sdo_Anuales12 Sdo_AnualPerAnt1 Sdo_AnualPerAnt2 ~
Sdo_AnualPerAnt3 Sdo_AnualPerAnt4 Sdo_AnualPerAnt5 Sdo_AnualPerAnt6 ~
Sdo_AnualPerAnt7 Sdo_AnualPerAnt8 Sdo_AnualPerAnt9 Sdo_AnualPerAnt10 ~
Sdo_AnualPerAnt11 Sdo_AnualPerAnt12 Sdo_Canje Sdo_Disponible Sdo_Inicial ~
Sdo_MinCta Sdo_Minimo Sdo_Revaloriza Sdo_UltLiquidacion TarjetaDB Tasa ~
Tip-Tasa Tip_ahorro TRF TRF_Notas Usu_Creacion Val_DebAutPdo Val_DepDia ~
Val_DepMes Val_Embargo Val_Pignorar Val_RetAcum Val_RetDia Val_RetDiaCheq ~
Val_RetMes Val_RetPeriodo 
&Scoped-Define MANDATORY-FIELDS  Cue_Ahorros Tip_ahorro
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Sdo_Anuales1 = Ahorros.Sdo_Anuales[1]~
  rowObject.Sdo_Anuales2 = Ahorros.Sdo_Anuales[2]~
  rowObject.Sdo_Anuales3 = Ahorros.Sdo_Anuales[3]~
  rowObject.Sdo_Anuales4 = Ahorros.Sdo_Anuales[4]~
  rowObject.Sdo_Anuales5 = Ahorros.Sdo_Anuales[5]~
  rowObject.Sdo_Anuales6 = Ahorros.Sdo_Anuales[6]~
  rowObject.Sdo_Anuales7 = Ahorros.Sdo_Anuales[7]~
  rowObject.Sdo_Anuales8 = Ahorros.Sdo_Anuales[8]~
  rowObject.Sdo_Anuales9 = Ahorros.Sdo_Anuales[9]~
  rowObject.Sdo_Anuales10 = Ahorros.Sdo_Anuales[10]~
  rowObject.Sdo_Anuales11 = Ahorros.Sdo_Anuales[11]~
  rowObject.Sdo_Anuales12 = Ahorros.Sdo_Anuales[12]~
  rowObject.Sdo_AnualPerAnt1 = Ahorros.Sdo_AnualPerAnt[1]~
  rowObject.Sdo_AnualPerAnt2 = Ahorros.Sdo_AnualPerAnt[2]~
  rowObject.Sdo_AnualPerAnt3 = Ahorros.Sdo_AnualPerAnt[3]~
  rowObject.Sdo_AnualPerAnt4 = Ahorros.Sdo_AnualPerAnt[4]~
  rowObject.Sdo_AnualPerAnt5 = Ahorros.Sdo_AnualPerAnt[5]~
  rowObject.Sdo_AnualPerAnt6 = Ahorros.Sdo_AnualPerAnt[6]~
  rowObject.Sdo_AnualPerAnt7 = Ahorros.Sdo_AnualPerAnt[7]~
  rowObject.Sdo_AnualPerAnt8 = Ahorros.Sdo_AnualPerAnt[8]~
  rowObject.Sdo_AnualPerAnt9 = Ahorros.Sdo_AnualPerAnt[9]~
  rowObject.Sdo_AnualPerAnt10 = Ahorros.Sdo_AnualPerAnt[10]~
  rowObject.Sdo_AnualPerAnt11 = Ahorros.Sdo_AnualPerAnt[11]~
  rowObject.Sdo_AnualPerAnt12 = Ahorros.Sdo_AnualPerAnt[12]
&Scoped-Define DATA-FIELD-DEFS "dahorros.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Ahorros NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Ahorros NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Ahorros


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Ahorros SCROLLING.
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
         WIDTH              = 47.86.
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
     _TblList          = "bdcentral.Ahorros"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Ahorros.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[2]   > bdcentral.Ahorros.Agencia_Destino
"Agencia_Destino" "Agencia_Destino" ? ? "integer" ? ? ? ? ? ? yes ? no 14.14 yes ?
     _FldNameList[3]   > bdcentral.Ahorros.Age_DebAutomatico
"Age_DebAutomatico" "Age_DebAutomatico" ? ? "integer" ? ? ? ? ? ? yes ? no 18.43 yes ?
     _FldNameList[4]   > bdcentral.Ahorros.Ajuste
"Ajuste" "Ajuste" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[5]   > bdcentral.Ahorros.Clave
"Clave" "Clave" ? ? "character" ? ? ? ? ? ? yes ? no 5.29 yes ?
     _FldNameList[6]   > bdcentral.Ahorros.Cod_ahorro
"Cod_ahorro" "Cod_ahorro" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[7]   > bdcentral.Ahorros.Cod_DebAutomatico
"Cod_DebAutomatico" "Cod_DebAutomatico" ? ? "integer" ? ? ? ? ? ? yes ? no 18.57 yes ?
     _FldNameList[8]   > bdcentral.Ahorros.Conjuncion
"Conjuncion" "Conjuncion" ? ? "logical" ? ? ? ? ? ? yes ? no 3.43 yes ?
     _FldNameList[9]   > bdcentral.Ahorros.Cue_Ahorros
"Cue_Ahorros" "Cue_Ahorros" ? ? "character" ? ? ? ? ? ? yes ? yes 14 yes ?
     _FldNameList[10]   > bdcentral.Ahorros.Cue_DebAutomatico
"Cue_DebAutomatico" "Cue_DebAutomatico" ? ? "character" ? ? ? ? ? ? yes ? no 18.57 yes ?
     _FldNameList[11]   > bdcentral.Ahorros.Cue_Destino
"Cue_Destino" "Cue_Destino" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[12]   > bdcentral.Ahorros.Cue_Padre
"Cue_Padre" "Cue_Padre" ? ? "character" ? ? ? ? ? ? yes ? no 20.14 yes ?
     _FldNameList[13]   > bdcentral.Ahorros.Cuota
"Cuota" "Cuota" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[14]   > bdcentral.Ahorros.Des_Intereses
"Des_Intereses" "Des_Intereses" ? ? "integer" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[15]   > bdcentral.Ahorros.Detalle_Estado
"Detalle_Estado" "Detalle_Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 14.14 yes ?
     _FldNameList[16]   > bdcentral.Ahorros.Dias_Causar
"Dias_Causar" "Dias_Causar" ? ? "integer" ? ? ? ? ? ? yes ? no 13.14 yes ?
     _FldNameList[17]   > bdcentral.Ahorros.Dia_Sobregiro
"Dia_Sobregiro" "Dia_Sobregiro" ? ? "integer" ? ? ? ? ? ? yes ? no 13.86 yes ?
     _FldNameList[18]   > bdcentral.Ahorros.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[19]   > bdcentral.Ahorros.Exento_3xm
"Exento_3xm" "Exento_3xm" ? ? "logical" ? ? ? ? ? ? yes ? no 11.57 yes ?
     _FldNameList[20]   > bdcentral.Ahorros.Fec_Apertura
"Fec_Apertura" "Fec_Apertura" ? ? "date" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[21]   > bdcentral.Ahorros.Fec_BloqueoTdb
"Fec_BloqueoTdb" "Fec_BloqueoTdb" ? ? "date" ? ? ? ? ? ? yes ? no 11.57 yes ?
     _FldNameList[22]   > bdcentral.Ahorros.Fec_Cancelacion
"Fec_Cancelacion" "Fec_Cancelacion" ? ? "date" ? ? ? ? ? ? yes ? no 20.29 yes ?
     _FldNameList[23]   > bdcentral.Ahorros.Fec_CancTdb
"Fec_CancTdb" "Fec_CancTdb" ? ? "date" ? ? ? ? ? ? yes ? no 11.29 yes ?
     _FldNameList[24]   > bdcentral.Ahorros.Fec_CreaTdb
"Fec_CreaTdb" "Fec_CreaTdb" ? ? "date" ? ? ? ? ? ? yes ? no 21.43 yes ?
     _FldNameList[25]   > bdcentral.Ahorros.Fec_ProLiquidacion
"Fec_ProLiquidacion" "Fec_ProLiquidacion" ? ? "date" ? ? ? ? ? ? yes ? no 25.14 yes ?
     _FldNameList[26]   > bdcentral.Ahorros.Fec_Prorroga
"Fec_Prorroga" "Fec_Prorroga" ? ? "date" ? ? ? ? ? ? yes ? no 17 yes ?
     _FldNameList[27]   > bdcentral.Ahorros.Fec_ProxDeb
"Fec_ProxDeb" "Fec_ProxDeb" ? ? "date" ? ? ? ? ? ? yes ? no 20.43 yes ?
     _FldNameList[28]   > bdcentral.Ahorros.Fec_UltCausacion
"Fec_UltCausacion" "Fec_UltCausacion" ? ? "date" ? ? ? ? ? ? yes ? no 22.43 yes ?
     _FldNameList[29]   > bdcentral.Ahorros.Fec_UltDeb
"Fec_UltDeb" "Fec_UltDeb" ? ? "date" ? ? ? ? ? ? yes ? no 18.72 yes ?
     _FldNameList[30]   > bdcentral.Ahorros.Fec_UltLiquidacion
"Fec_UltLiquidacion" "Fec_UltLiquidacion" ? ? "date" ? ? ? ? ? ? yes ? no 26.14 yes ?
     _FldNameList[31]   > bdcentral.Ahorros.Fec_Ulttransaccion
"Fec_Ulttransaccion" "Fec_Ulttransaccion" ? ? "date" ? ? ? ? ? ? yes ? no 26.86 yes ?
     _FldNameList[32]   > bdcentral.Ahorros.Fec_Vencimiento
"Fec_Vencimiento" "Fec_Vencimiento" ? ? "date" ? ? ? ? ? ? yes ? no 20.29 yes ?
     _FldNameList[33]   > bdcentral.Ahorros.For_Liquidacion
"For_Liquidacion" "For_Liquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 10.72 yes ?
     _FldNameList[34]   > bdcentral.Ahorros.For_Pago
"For_Pago" "For_Pago" ? ? "integer" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[35]   > bdcentral.Ahorros.IdApellido1
"IdApellido1" "IdApellido1" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[36]   > bdcentral.Ahorros.IdNombre
"IdNombre" "IdNombre" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[37]   > bdcentral.Ahorros.Id_ForLiq
"Id_ForLiq" "Id_ForLiq" ? ? "integer" ? ? ? ? ? ? yes ? no 8.72 yes ?
     _FldNameList[38]   > bdcentral.Ahorros.Id_Sobregiro
"Id_Sobregiro" "Id_Sobregiro" ? ? "logical" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[39]   > bdcentral.Ahorros.Id_TasDiferencial
"Id_TasDiferencial" "Id_TasDiferencial" ? ? "integer" ? ? ? ? ? ? yes ? no 22.57 yes ?
     _FldNameList[40]   > bdcentral.Ahorros.Ind_TasVariable
"Ind_TasVariable" "Ind_TasVariable" ? ? "integer" ? ? ? ? ? ? yes ? no 18.43 yes ?
     _FldNameList[41]   > bdcentral.Ahorros.Int_Causado
"Int_Causado" "Int_Causado" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[42]   > bdcentral.Ahorros.Int_Pagar
"Int_Pagar" "Int_Pagar" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[43]   > bdcentral.Ahorros.Int_Sobregiro
"Int_Sobregiro" "Int_Sobregiro" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[44]   > bdcentral.Ahorros.Monto_Apertura
"Monto_Apertura" "Monto_Apertura" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[45]   > bdcentral.Ahorros.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[46]   > bdcentral.Ahorros.Num_Asesoria
"Num_Asesoria" "Num_Asesoria" ? ? "integer" ? ? ? ? ? ? yes ? no 9.14 yes ?
     _FldNameList[47]   > bdcentral.Ahorros.Num_DepDia
"Num_DepDia" "Num_DepDia" ? ? "integer" ? ? ? ? ? ? yes ? no 12.29 yes ?
     _FldNameList[48]   > bdcentral.Ahorros.Num_DepMes
"Num_DepMes" "Num_DepMes" ? ? "integer" ? ? ? ? ? ? yes ? no 13.14 yes ?
     _FldNameList[49]   > bdcentral.Ahorros.Num_Formato
"Num_Formato" "Num_Formato" ? ? "character" ? ? ? ? ? ? yes ? no 18.29 yes ?
     _FldNameList[50]   > bdcentral.Ahorros.Num_RetDia
"Num_RetDia" "Num_RetDia" ? ? "integer" ? ? ? ? ? ? yes ? no 11.72 yes ?
     _FldNameList[51]   > bdcentral.Ahorros.Num_RetDiaCheq
"Num_RetDiaCheq" "Num_RetDiaCheq" ? ? "integer" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[52]   > bdcentral.Ahorros.Num_RetMes
"Num_RetMes" "Num_RetMes" ? ? "integer" ? ? ? ? ? ? yes ? no 12.57 yes ?
     _FldNameList[53]   > bdcentral.Ahorros.Per_Deduccion
"Per_Deduccion" "Per_Deduccion" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[54]   > bdcentral.Ahorros.Per_Liquidacion
"Per_Liquidacion" "Per_Liquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 21.86 yes ?
     _FldNameList[55]   > bdcentral.Ahorros.Plazo
"Plazo" "Plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 5.29 yes ?
     _FldNameList[56]   > bdcentral.Ahorros.Pro_Destino
"Pro_Destino" "Pro_Destino" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[57]   > bdcentral.Ahorros.Pun_TasVariable
"Pun_TasVariable" "Pun_TasVariable" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.72 yes ?
     _FldNameList[58]   > bdcentral.Ahorros.Sal_Intpagados
"Sal_Intpagados" "Sal_Intpagados" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.29 yes ?
     _FldNameList[59]   > bdcentral.Ahorros.Sdo_Anuales[1]
"Sdo_Anuales[1]" "Sdo_Anuales1" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[60]   > bdcentral.Ahorros.Sdo_Anuales[2]
"Sdo_Anuales[2]" "Sdo_Anuales2" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[61]   > bdcentral.Ahorros.Sdo_Anuales[3]
"Sdo_Anuales[3]" "Sdo_Anuales3" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[62]   > bdcentral.Ahorros.Sdo_Anuales[4]
"Sdo_Anuales[4]" "Sdo_Anuales4" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[63]   > bdcentral.Ahorros.Sdo_Anuales[5]
"Sdo_Anuales[5]" "Sdo_Anuales5" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[64]   > bdcentral.Ahorros.Sdo_Anuales[6]
"Sdo_Anuales[6]" "Sdo_Anuales6" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[65]   > bdcentral.Ahorros.Sdo_Anuales[7]
"Sdo_Anuales[7]" "Sdo_Anuales7" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[66]   > bdcentral.Ahorros.Sdo_Anuales[8]
"Sdo_Anuales[8]" "Sdo_Anuales8" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[67]   > bdcentral.Ahorros.Sdo_Anuales[9]
"Sdo_Anuales[9]" "Sdo_Anuales9" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[68]   > bdcentral.Ahorros.Sdo_Anuales[10]
"Sdo_Anuales[10]" "Sdo_Anuales10" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[69]   > bdcentral.Ahorros.Sdo_Anuales[11]
"Sdo_Anuales[11]" "Sdo_Anuales11" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[70]   > bdcentral.Ahorros.Sdo_Anuales[12]
"Sdo_Anuales[12]" "Sdo_Anuales12" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[71]   > bdcentral.Ahorros.Sdo_AnualPerAnt[1]
"Sdo_AnualPerAnt[1]" "Sdo_AnualPerAnt1" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[72]   > bdcentral.Ahorros.Sdo_AnualPerAnt[2]
"Sdo_AnualPerAnt[2]" "Sdo_AnualPerAnt2" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[73]   > bdcentral.Ahorros.Sdo_AnualPerAnt[3]
"Sdo_AnualPerAnt[3]" "Sdo_AnualPerAnt3" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[74]   > bdcentral.Ahorros.Sdo_AnualPerAnt[4]
"Sdo_AnualPerAnt[4]" "Sdo_AnualPerAnt4" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[75]   > bdcentral.Ahorros.Sdo_AnualPerAnt[5]
"Sdo_AnualPerAnt[5]" "Sdo_AnualPerAnt5" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[76]   > bdcentral.Ahorros.Sdo_AnualPerAnt[6]
"Sdo_AnualPerAnt[6]" "Sdo_AnualPerAnt6" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[77]   > bdcentral.Ahorros.Sdo_AnualPerAnt[7]
"Sdo_AnualPerAnt[7]" "Sdo_AnualPerAnt7" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[78]   > bdcentral.Ahorros.Sdo_AnualPerAnt[8]
"Sdo_AnualPerAnt[8]" "Sdo_AnualPerAnt8" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[79]   > bdcentral.Ahorros.Sdo_AnualPerAnt[9]
"Sdo_AnualPerAnt[9]" "Sdo_AnualPerAnt9" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[80]   > bdcentral.Ahorros.Sdo_AnualPerAnt[10]
"Sdo_AnualPerAnt[10]" "Sdo_AnualPerAnt10" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[81]   > bdcentral.Ahorros.Sdo_AnualPerAnt[11]
"Sdo_AnualPerAnt[11]" "Sdo_AnualPerAnt11" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[82]   > bdcentral.Ahorros.Sdo_AnualPerAnt[12]
"Sdo_AnualPerAnt[12]" "Sdo_AnualPerAnt12" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[83]   > bdcentral.Ahorros.Sdo_Canje
"Sdo_Canje" "Sdo_Canje" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[84]   > bdcentral.Ahorros.Sdo_Disponible
"Sdo_Disponible" "Sdo_Disponible" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[85]   > bdcentral.Ahorros.Sdo_Inicial
"Sdo_Inicial" "Sdo_Inicial" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[86]   > bdcentral.Ahorros.Sdo_MinCta
"Sdo_MinCta" "Sdo_MinCta" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[87]   > bdcentral.Ahorros.Sdo_Minimo
"Sdo_Minimo" "Sdo_Minimo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[88]   > bdcentral.Ahorros.Sdo_Revaloriza
"Sdo_Revaloriza" "Sdo_Revaloriza" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[89]   > bdcentral.Ahorros.Sdo_UltLiquidacion
"Sdo_UltLiquidacion" "Sdo_UltLiquidacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.14 yes ?
     _FldNameList[90]   > bdcentral.Ahorros.TarjetaDB
"TarjetaDB" "TarjetaDB" ? ? "character" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[91]   > bdcentral.Ahorros.Tasa
"Tasa" "Tasa" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[92]   > bdcentral.Ahorros.Tip-Tasa
"Tip-Tasa" "Tip-Tasa" ? ? "integer" ? ? ? ? ? ? yes ? no 20.57 yes ?
     _FldNameList[93]   > bdcentral.Ahorros.Tip_ahorro
"Tip_ahorro" "Tip_ahorro" ? ? "integer" ? ? ? ? ? ? yes ? yes 12.72 yes ?
     _FldNameList[94]   > bdcentral.Ahorros.TRF
"TRF" "TRF" ? ? "integer" ? ? ? ? ? ? yes ? no 3.72 yes ?
     _FldNameList[95]   > bdcentral.Ahorros.TRF_Notas
"TRF_Notas" "TRF_Notas" ? ? "character" ? ? ? ? ? ? yes ? no 80 yes ?
     _FldNameList[96]   > bdcentral.Ahorros.Usu_Creacion
"Usu_Creacion" "Usu_Creacion" ? ? "character" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[97]   > bdcentral.Ahorros.Val_DebAutPdo
"Val_DebAutPdo" "Val_DebAutPdo" ? ? "decimal" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[98]   > bdcentral.Ahorros.Val_DepDia
"Val_DepDia" "Val_DepDia" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.14 yes ?
     _FldNameList[99]   > bdcentral.Ahorros.Val_DepMes
"Val_DepMes" "Val_DepMes" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.29 yes ?
     _FldNameList[100]   > bdcentral.Ahorros.Val_Embargo
"Val_Embargo" "Val_Embargo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[101]   > bdcentral.Ahorros.Val_Pignorar
"Val_Pignorar" "Val_Pignorar" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[102]   > bdcentral.Ahorros.Val_RetAcum
"Val_RetAcum" "Val_RetAcum" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[103]   > bdcentral.Ahorros.Val_RetDia
"Val_RetDia" "Val_RetDia" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.43 yes ?
     _FldNameList[104]   > bdcentral.Ahorros.Val_RetDiaCheq
"Val_RetDiaCheq" "Val_RetDiaCheq" ? ? "decimal" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[105]   > bdcentral.Ahorros.Val_RetMes
"Val_RetMes" "Val_RetMes" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.29 yes ?
     _FldNameList[106]   > bdcentral.Ahorros.Val_RetPeriodo
"Val_RetPeriodo" "Val_RetPeriodo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[107]   > "_<CALC>"
"getAgencia(INPUT RowObject.Agencia)" "FAgencia" "Agencia" "x(45)" "character" ? ? ? ? ? ? no ? no 45 no "Agencia"
     _FldNameList[108]   > "_<CALC>"
"getAgencia(INPUT RowObject.Agencia_Destino)" "FAgenciaDest" "Age. Destino" "x(45)" "character" ? ? ? ? ? ? no ? no 45 no "Age. Destino"
     _FldNameList[109]   > "_<CALC>"
"getAhorro(INPUT RowObject.Cod_ahorro)" "FAporte" "Aporte" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no "Aporte"
     _FldNameList[110]   > "_<CALC>"
"getCliente(INPUT RowObject.Nit)" "FCliente" "Cliente" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no "Cliente"
     _FldNameList[111]   > "_<CALC>"
"getProducto(1, INPUT RowObject.Cod_ahorro)" "FProducto" "Producto" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
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
         rowObject.FAgenciaDest = (getAgencia(INPUT RowObject.Agencia_Destino))
         rowObject.FAporte = (getAhorro(INPUT RowObject.Cod_ahorro))
         rowObject.FCliente = (getCliente(INPUT RowObject.Nit))
         rowObject.FProducto = (getProducto(1, INPUT RowObject.Cod_ahorro))
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

