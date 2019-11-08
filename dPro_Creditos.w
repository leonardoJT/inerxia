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
&Scoped-define INTERNAL-TABLES Pro_Creditos

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Cod_Credito Cod_ForChequeo Cod_ForPagare Cod_ForRetencion Cod_ForTalonario~
 Cod_otro Cod_ProAhorro Cod_Tasa Cod_TasaMora Consecutivo Deducible1~
 Deducible2 Deducible3 Deducible4 Deducible5 Deducible6 Deducible7~
 Deducible8 Deducible9 Deducible10 Descripcion Dia_Canje Dia_Gracia Estado~
 Fec_Matricula Fec_Retiro For_Liquidacion Id_AprobAgencia Id_AsociaCta~
 Id_Asociado Id_Consecutivo Id_CreProAhorro Id_CreUnico Id_Debito~
 Id_Extracto Id_Formato Id_IntMora Id_Linea Id_Montomaximo Id_Montominimo~
 Id_NumAlterno Id_PagParcial Id_PerGracia Id_Plazo Id_Rotatorio Id_Sorteos~
 Id_Talonario Id_Tasa Id_TasDiferencial Ind_TasVble Lin_multiplicador~
 Multiplicador Nom_Producto Per_GarPer Per_GarRea Per_Liquidacion Pla_Maximo~
 Pla_Minimo Prioridad Pun_TasaVble Tas_Fija Tas_intmora Tip-Tasa Tip_Credito~
 Tip_Tasainterna Val_Montomaximo Val_Montominimo
&Scoped-define ENABLED-FIELDS-IN-Pro_Creditos Cod_Credito Cod_ForChequeo ~
Cod_ForPagare Cod_ForRetencion Cod_ForTalonario Cod_otro Cod_ProAhorro ~
Cod_Tasa Cod_TasaMora Consecutivo Deducible1 Deducible2 Deducible3 ~
Deducible4 Deducible5 Deducible6 Deducible7 Deducible8 Deducible9 ~
Deducible10 Descripcion Dia_Canje Dia_Gracia Estado Fec_Matricula ~
Fec_Retiro For_Liquidacion Id_AprobAgencia Id_AsociaCta Id_Asociado ~
Id_Consecutivo Id_CreProAhorro Id_CreUnico Id_Debito Id_Extracto Id_Formato ~
Id_IntMora Id_Linea Id_Montomaximo Id_Montominimo Id_NumAlterno ~
Id_PagParcial Id_PerGracia Id_Plazo Id_Rotatorio Id_Sorteos Id_Talonario ~
Id_Tasa Id_TasDiferencial Ind_TasVble Lin_multiplicador Multiplicador ~
Nom_Producto Per_GarPer Per_GarRea Per_Liquidacion Pla_Maximo Pla_Minimo ~
Prioridad Pun_TasaVble Tas_Fija Tas_intmora Tip-Tasa Tip_Credito ~
Tip_Tasainterna Val_Montomaximo Val_Montominimo 
&Scoped-Define DATA-FIELDS  Cod_Credito Cod_ForChequeo Cod_ForPagare Cod_ForRetencion Cod_ForTalonario~
 Cod_otro Cod_ProAhorro Cod_Tasa Cod_TasaMora Consecutivo Deducible1~
 Deducible2 Deducible3 Deducible4 Deducible5 Deducible6 Deducible7~
 Deducible8 Deducible9 Deducible10 Descripcion Dia_Canje Dia_Gracia Estado~
 Fec_Matricula Fec_Retiro For_Liquidacion Id_AprobAgencia Id_AsociaCta~
 Id_Asociado Id_Consecutivo Id_CreProAhorro Id_CreUnico Id_Debito~
 Id_Extracto Id_Formato Id_IntMora Id_Linea Id_Montomaximo Id_Montominimo~
 Id_NumAlterno Id_PagParcial Id_PerGracia Id_Plazo Id_Rotatorio Id_Sorteos~
 Id_Talonario Id_Tasa Id_TasDiferencial Ind_TasVble Lin_multiplicador~
 Multiplicador Nom_Producto Per_GarPer Per_GarRea Per_Liquidacion Pla_Maximo~
 Pla_Minimo Prioridad Pun_TasaVble Tas_Fija Tas_intmora Tip-Tasa Tip_Credito~
 Tip_Tasainterna Val_Montomaximo Val_Montominimo FProducto
&Scoped-define DATA-FIELDS-IN-Pro_Creditos Cod_Credito Cod_ForChequeo ~
Cod_ForPagare Cod_ForRetencion Cod_ForTalonario Cod_otro Cod_ProAhorro ~
Cod_Tasa Cod_TasaMora Consecutivo Deducible1 Deducible2 Deducible3 ~
Deducible4 Deducible5 Deducible6 Deducible7 Deducible8 Deducible9 ~
Deducible10 Descripcion Dia_Canje Dia_Gracia Estado Fec_Matricula ~
Fec_Retiro For_Liquidacion Id_AprobAgencia Id_AsociaCta Id_Asociado ~
Id_Consecutivo Id_CreProAhorro Id_CreUnico Id_Debito Id_Extracto Id_Formato ~
Id_IntMora Id_Linea Id_Montomaximo Id_Montominimo Id_NumAlterno ~
Id_PagParcial Id_PerGracia Id_Plazo Id_Rotatorio Id_Sorteos Id_Talonario ~
Id_Tasa Id_TasDiferencial Ind_TasVble Lin_multiplicador Multiplicador ~
Nom_Producto Per_GarPer Per_GarRea Per_Liquidacion Pla_Maximo Pla_Minimo ~
Prioridad Pun_TasaVble Tas_Fija Tas_intmora Tip-Tasa Tip_Credito ~
Tip_Tasainterna Val_Montomaximo Val_Montominimo 
&Scoped-Define MANDATORY-FIELDS  Cod_Credito Prioridad
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Deducible1 = Pro_Creditos.Deducible[1]~
  rowObject.Deducible2 = Pro_Creditos.Deducible[2]~
  rowObject.Deducible3 = Pro_Creditos.Deducible[3]~
  rowObject.Deducible4 = Pro_Creditos.Deducible[4]~
  rowObject.Deducible5 = Pro_Creditos.Deducible[5]~
  rowObject.Deducible6 = Pro_Creditos.Deducible[6]~
  rowObject.Deducible7 = Pro_Creditos.Deducible[7]~
  rowObject.Deducible8 = Pro_Creditos.Deducible[8]~
  rowObject.Deducible9 = Pro_Creditos.Deducible[9]~
  rowObject.Deducible10 = Pro_Creditos.Deducible[10]
&Scoped-Define DATA-FIELD-DEFS "dpro_creditos.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Pro_Creditos NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Pro_Creditos NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Pro_Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Pro_Creditos


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Pro_Creditos SCROLLING.
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
     _TblList          = "bdcentral.Pro_Creditos"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Pro_Creditos.Cod_Credito
"Cod_Credito" "Cod_Credito" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.14 yes ?
     _FldNameList[2]   > bdcentral.Pro_Creditos.Cod_ForChequeo
"Cod_ForChequeo" "Cod_ForChequeo" ? ? "integer" ? ? ? ? ? ? yes ? no 25.14 yes ?
     _FldNameList[3]   > bdcentral.Pro_Creditos.Cod_ForPagare
"Cod_ForPagare" "Cod_ForPagare" ? ? "integer" ? ? ? ? ? ? yes ? no 14.72 yes ?
     _FldNameList[4]   > bdcentral.Pro_Creditos.Cod_ForRetencion
"Cod_ForRetencion" "Cod_ForRetencion" ? ? "integer" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[5]   > bdcentral.Pro_Creditos.Cod_ForTalonario
"Cod_ForTalonario" "Cod_ForTalonario" ? ? "integer" ? ? ? ? ? ? yes ? no 21.57 yes ?
     _FldNameList[6]   > bdcentral.Pro_Creditos.Cod_otro
"Cod_otro" "Cod_otro" ? ? "integer" ? ? ? ? ? ? yes ? no 10.86 yes ?
     _FldNameList[7]   > bdcentral.Pro_Creditos.Cod_ProAhorro
"Cod_ProAhorro" "Cod_ProAhorro" ? ? "integer" ? ? ? ? ? ? yes ? no 24.57 yes ?
     _FldNameList[8]   > bdcentral.Pro_Creditos.Cod_Tasa
"Cod_Tasa" "Cod_Tasa" ? ? "integer" ? ? ? ? ? ? yes ? no 14.43 yes ?
     _FldNameList[9]   > bdcentral.Pro_Creditos.Cod_TasaMora
"Cod_TasaMora" "Cod_TasaMora" ? ? "integer" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[10]   > bdcentral.Pro_Creditos.Consecutivo
"Consecutivo" "Consecutivo" ? ? "integer" ? ? ? ? ? ? yes ? no 26.29 yes ?
     _FldNameList[11]   > bdcentral.Pro_Creditos.Deducible[1]
"Deducible[1]" "Deducible1" ? ? "character" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[12]   > bdcentral.Pro_Creditos.Deducible[2]
"Deducible[2]" "Deducible2" ? ? "character" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[13]   > bdcentral.Pro_Creditos.Deducible[3]
"Deducible[3]" "Deducible3" ? ? "character" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[14]   > bdcentral.Pro_Creditos.Deducible[4]
"Deducible[4]" "Deducible4" ? ? "character" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[15]   > bdcentral.Pro_Creditos.Deducible[5]
"Deducible[5]" "Deducible5" ? ? "character" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[16]   > bdcentral.Pro_Creditos.Deducible[6]
"Deducible[6]" "Deducible6" ? ? "character" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[17]   > bdcentral.Pro_Creditos.Deducible[7]
"Deducible[7]" "Deducible7" ? ? "character" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[18]   > bdcentral.Pro_Creditos.Deducible[8]
"Deducible[8]" "Deducible8" ? ? "character" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[19]   > bdcentral.Pro_Creditos.Deducible[9]
"Deducible[9]" "Deducible9" ? ? "character" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[20]   > bdcentral.Pro_Creditos.Deducible[10]
"Deducible[10]" "Deducible10" ? ? "character" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[21]   > bdcentral.Pro_Creditos.Descripcion
"Descripcion" "Descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 240 yes ?
     _FldNameList[22]   > bdcentral.Pro_Creditos.Dia_Canje
"Dia_Canje" "Dia_Canje" ? ? "integer" ? ? ? ? ? ? yes ? no 13 yes ?
     _FldNameList[23]   > bdcentral.Pro_Creditos.Dia_Gracia
"Dia_Gracia" "Dia_Gracia" ? ? "integer" ? ? ? ? ? ? yes ? no 10.86 yes ?
     _FldNameList[24]   > bdcentral.Pro_Creditos.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[25]   > bdcentral.Pro_Creditos.Fec_Matricula
"Fec_Matricula" "Fec_Matricula" ? ? "date" ? ? ? ? ? ? yes ? no 14.72 yes ?
     _FldNameList[26]   > bdcentral.Pro_Creditos.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 11.72 yes ?
     _FldNameList[27]   > bdcentral.Pro_Creditos.For_Liquidacion
"For_Liquidacion" "For_Liquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 17.14 yes ?
     _FldNameList[28]   > bdcentral.Pro_Creditos.Id_AprobAgencia
"Id_AprobAgencia" "Id_AprobAgencia" ? ? "logical" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[29]   > bdcentral.Pro_Creditos.Id_AsociaCta
"Id_AsociaCta" "Id_AsociaCta" ? ? "logical" ? ? ? ? ? ? yes ? no 13.14 yes ?
     _FldNameList[30]   > bdcentral.Pro_Creditos.Id_Asociado
"Id_Asociado" "Id_Asociado" ? ? "integer" ? ? ? ? ? ? yes ? no 21.86 yes ?
     _FldNameList[31]   > bdcentral.Pro_Creditos.Id_Consecutivo
"Id_Consecutivo" "Id_Consecutivo" ? ? "logical" ? ? ? ? ? ? yes ? no 18.86 yes ?
     _FldNameList[32]   > bdcentral.Pro_Creditos.Id_CreProAhorro
"Id_CreProAhorro" "Id_CreProAhorro" ? ? "logical" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[33]   > bdcentral.Pro_Creditos.Id_CreUnico
"Id_CreUnico" "Id_CreUnico" ? ? "logical" ? ? ? ? ? ? yes ? no 12.43 yes ?
     _FldNameList[34]   > bdcentral.Pro_Creditos.Id_Debito
"Id_Debito" "Id_Debito" ? ? "logical" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[35]   > bdcentral.Pro_Creditos.Id_Extracto
"Id_Extracto" "Id_Extracto" ? ? "logical" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[36]   > bdcentral.Pro_Creditos.Id_Formato
"Id_Formato" "Id_Formato" ? ? "logical" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[37]   > bdcentral.Pro_Creditos.Id_IntMora
"Id_IntMora" "Id_IntMora" ? ? "logical" ? ? ? ? ? ? yes ? no 19.14 yes ?
     _FldNameList[38]   > bdcentral.Pro_Creditos.Id_Linea
"Id_Linea" "Id_Linea" ? ? "logical" ? ? ? ? ? ? yes ? no 15.57 yes ?
     _FldNameList[39]   > bdcentral.Pro_Creditos.Id_Montomaximo
"Id_Montomaximo" "Id_Montomaximo" ? ? "logical" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[40]   > bdcentral.Pro_Creditos.Id_Montominimo
"Id_Montominimo" "Id_Montominimo" ? ? "logical" ? ? ? ? ? ? yes ? no 20.57 yes ?
     _FldNameList[41]   > bdcentral.Pro_Creditos.Id_NumAlterno
"Id_NumAlterno" "Id_NumAlterno" ? ? "logical" ? ? ? ? ? ? yes ? no 20.86 yes ?
     _FldNameList[42]   > bdcentral.Pro_Creditos.Id_PagParcial
"Id_PagParcial" "Id_PagParcial" ? ? "logical" ? ? ? ? ? ? yes ? no 33.57 yes ?
     _FldNameList[43]   > bdcentral.Pro_Creditos.Id_PerGracia
"Id_PerGracia" "Id_PerGracia" ? ? "logical" ? ? ? ? ? ? yes ? no 26.86 yes ?
     _FldNameList[44]   > bdcentral.Pro_Creditos.Id_Plazo
"Id_Plazo" "Id_Plazo" ? ? "logical" ? ? ? ? ? ? yes ? no 12.72 yes ?
     _FldNameList[45]   > bdcentral.Pro_Creditos.Id_Rotatorio
"Id_Rotatorio" "Id_Rotatorio" ? ? "logical" ? ? ? ? ? ? yes ? no 15.57 yes ?
     _FldNameList[46]   > bdcentral.Pro_Creditos.Id_Sorteos
"Id_Sorteos" "Id_Sorteos" ? ? "logical" ? ? ? ? ? ? yes ? no 13.86 yes ?
     _FldNameList[47]   > bdcentral.Pro_Creditos.Id_Talonario
"Id_Talonario" "Id_Talonario" ? ? "integer" ? ? ? ? ? ? yes ? no 16.14 yes ?
     _FldNameList[48]   > bdcentral.Pro_Creditos.Id_Tasa
"Id_Tasa" "Id_Tasa" ? ? "integer" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[49]   > bdcentral.Pro_Creditos.Id_TasDiferencial
"Id_TasDiferencial" "Id_TasDiferencial" ? ? "integer" ? ? ? ? ? ? yes ? no 22.57 yes ?
     _FldNameList[50]   > bdcentral.Pro_Creditos.Ind_TasVble
"Ind_TasVble" "Ind_TasVble" ? ? "integer" ? ? ? ? ? ? yes ? no 18.43 yes ?
     _FldNameList[51]   > bdcentral.Pro_Creditos.Lin_multiplicador
"Lin_multiplicador" "Lin_multiplicador" ? ? "integer" ? ? ? ? ? ? yes ? no 17.72 yes ?
     _FldNameList[52]   > bdcentral.Pro_Creditos.Multiplicador
"Multiplicador" "Multiplicador" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[53]   > bdcentral.Pro_Creditos.Nom_Producto
"Nom_Producto" "Nom_Producto" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[54]   > bdcentral.Pro_Creditos.Per_GarPer
"Per_GarPer" "Per_GarPer" ? ? "integer" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[55]   > bdcentral.Pro_Creditos.Per_GarRea
"Per_GarRea" "Per_GarRea" ? ? "integer" ? ? ? ? ? ? yes ? no 20.14 yes ?
     _FldNameList[56]   > bdcentral.Pro_Creditos.Per_Liquidacion
"Per_Liquidacion" "Per_Liquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 18.43 yes ?
     _FldNameList[57]   > bdcentral.Pro_Creditos.Pla_Maximo
"Pla_Maximo" "Pla_Maximo" ? ? "integer" ? ? ? ? ? ? yes ? no 13.29 yes ?
     _FldNameList[58]   > bdcentral.Pro_Creditos.Pla_Minimo
"Pla_Minimo" "Pla_Minimo" ? ? "integer" ? ? ? ? ? ? yes ? no 12.72 yes ?
     _FldNameList[59]   > bdcentral.Pro_Creditos.Prioridad
"Prioridad" "Prioridad" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.43 yes ?
     _FldNameList[60]   > bdcentral.Pro_Creditos.Pun_TasaVble
"Pun_TasaVble" "Pun_TasaVble" ? ? "decimal" ? ? ? ? ? ? yes ? no 19.72 yes ?
     _FldNameList[61]   > bdcentral.Pro_Creditos.Tas_Fija
"Tas_Fija" "Tas_Fija" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.86 yes ?
     _FldNameList[62]   > bdcentral.Pro_Creditos.Tas_intmora
"Tas_intmora" "Tas_intmora" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[63]   > bdcentral.Pro_Creditos.Tip-Tasa
"Tip-Tasa" "Tip-Tasa" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[64]   > bdcentral.Pro_Creditos.Tip_Credito
"Tip_Credito" "Tip_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[65]   > bdcentral.Pro_Creditos.Tip_Tasainterna
"Tip_Tasainterna" "Tip_Tasainterna" ? ? "integer" ? ? ? ? ? ? yes ? no 12.72 yes ?
     _FldNameList[66]   > bdcentral.Pro_Creditos.Val_Montomaximo
"Val_Montomaximo" "Val_Montomaximo" ? ? "decimal" ? ? ? ? ? ? yes ? no 19 yes ?
     _FldNameList[67]   > bdcentral.Pro_Creditos.Val_Montominimo
"Val_Montominimo" "Val_Montominimo" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.43 yes ?
     _FldNameList[68]   > "_<CALC>"
"STRING (STRING(RowObject.Cod_Credito) + "" - "" + RowObject.Nom_Producto,""X(60)"")" "FProducto" "Producto" "x(60)" "character" ? ? ? ? ? ? no ? no 60 no "Producto"
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
         rowObject.FProducto = (STRING (STRING(RowObject.Cod_Credito) + " - " + RowObject.Nom_Producto,"X(60)"))
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

