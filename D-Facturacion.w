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
&Scoped-define INTERNAL-TABLES Facturacion

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Per_Factura Agencia Num_factura Nit Num_Credito fec_corte Sdo_Anterior~
 Sdo_Mora Fec_Mora Cuota CupoDisponible CupoTotal Tasa_Ic Int_Corrientes~
 Int_MorCobrar Pagos Cargos OtrosCargos Seg_cartera Nuevo_Saldo Pago_Minimo~
 Pago_Total Sobrecupo Val_Recaudo Rec_Capital Rec_Honorarios~
 Rec_IntCorrientes Rec_IntMora Rec_Segcartera PagoTotal Estado Tasa_Ea~
 Val_Atraso Val_atrasokpen ciudad direccion Fec_Inicial Fec_Final~
 Fec_LimPago nombre Nombre_Agencia Telefono
&Scoped-define ENABLED-FIELDS-IN-Facturacion Per_Factura Agencia ~
Num_factura Nit Num_Credito fec_corte Sdo_Anterior Sdo_Mora Fec_Mora Cuota ~
CupoDisponible CupoTotal Tasa_Ic Int_Corrientes Int_MorCobrar Pagos Cargos ~
OtrosCargos Seg_cartera Nuevo_Saldo Pago_Minimo Pago_Total Sobrecupo ~
Val_Recaudo Rec_Capital Rec_Honorarios Rec_IntCorrientes Rec_IntMora ~
Rec_Segcartera PagoTotal Estado Tasa_Ea Val_Atraso Val_atrasokpen ciudad ~
direccion Fec_Inicial Fec_Final Fec_LimPago nombre Nombre_Agencia Telefono 
&Scoped-Define DATA-FIELDS  Per_Factura Agencia Num_factura Nit Num_Credito fec_corte Sdo_Anterior~
 Sdo_Mora Fec_Mora Cuota CupoDisponible CupoTotal Tasa_Ic Int_Corrientes~
 Int_MorCobrar Pagos Cargos OtrosCargos Seg_cartera Nuevo_Saldo Pago_Minimo~
 Pago_Total Sobrecupo Val_Recaudo Rec_Capital Rec_Honorarios~
 Rec_IntCorrientes Rec_IntMora Rec_Segcartera PagoTotal Estado Tasa_Ea~
 Val_Atraso Val_atrasokpen ciudad direccion Fec_Inicial Fec_Final~
 Fec_LimPago nombre Nombre_Agencia Telefono
&Scoped-define DATA-FIELDS-IN-Facturacion Per_Factura Agencia Num_factura ~
Nit Num_Credito fec_corte Sdo_Anterior Sdo_Mora Fec_Mora Cuota ~
CupoDisponible CupoTotal Tasa_Ic Int_Corrientes Int_MorCobrar Pagos Cargos ~
OtrosCargos Seg_cartera Nuevo_Saldo Pago_Minimo Pago_Total Sobrecupo ~
Val_Recaudo Rec_Capital Rec_Honorarios Rec_IntCorrientes Rec_IntMora ~
Rec_Segcartera PagoTotal Estado Tasa_Ea Val_Atraso Val_atrasokpen ciudad ~
direccion Fec_Inicial Fec_Final Fec_LimPago nombre Nombre_Agencia Telefono 
&Scoped-Define MANDATORY-FIELDS  Per_Factura Agencia Num_factura Nombre_Agencia
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "\\172.16.31.149\sfgpruebas\prg\d-facturacion.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Facturacion NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Facturacion NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Facturacion
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Facturacion


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Facturacion SCROLLING.
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
     _TblList          = "bdcentral.Facturacion"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Facturacion.Per_Factura
"Per_Factura" "Per_Factura" "Periodo" ? "integer" ? ? ? ? ? ? yes ? yes 32 yes "Periodo"
     _FldNameList[2]   > bdcentral.Facturacion.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.29 yes ?
     _FldNameList[3]   > bdcentral.Facturacion.Num_factura
"Num_factura" "Num_factura" "Factura" ? "integer" ? ? ? ? ? ? yes ? yes 19.86 yes "Factura"
     _FldNameList[4]   > bdcentral.Facturacion.Nit
"Nit" "Nit" "Cédula" ? "character" ? ? ? ? ? ? yes ? no 15 yes "Cédula"
     _FldNameList[5]   > bdcentral.Facturacion.Num_Credito
"Num_Credito" "Num_Credito" "Crédito" ? "integer" ? ? ? ? ? ? yes ? no 17.86 yes "Crédito"
     _FldNameList[6]   > bdcentral.Facturacion.fec_corte
"fec_corte" "fec_corte" "Fec.Corte" ? "date" ? ? ? ? ? ? yes ? no 31.57 yes "Fec.Corte"
     _FldNameList[7]   > bdcentral.Facturacion.Sdo_Anterior
"Sdo_Anterior" "Sdo_Anterior" "Sdo.Anterior" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Sdo.Anterior"
     _FldNameList[8]   > bdcentral.Facturacion.Sdo_Mora
"Sdo_Mora" "Sdo_Mora" "Sdo.Mora" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Sdo.Mora"
     _FldNameList[9]   > bdcentral.Facturacion.Fec_Mora
"Fec_Mora" "Fec_Mora" "Fec.Mora" ? "date" ? ? ? ? ? ? yes ? no 13.72 yes "Fec.Mora"
     _FldNameList[10]   > bdcentral.Facturacion.Cuota
"Cuota" "Cuota" "Cuota" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Cuota"
     _FldNameList[11]   > bdcentral.Facturacion.CupoDisponible
"CupoDisponible" "CupoDisponible" "Disponible" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Disponible"
     _FldNameList[12]   > bdcentral.Facturacion.CupoTotal
"CupoTotal" "CupoTotal" "Cupo" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Cupo"
     _FldNameList[13]   > bdcentral.Facturacion.Tasa_Ic
"Tasa_Ic" "Tasa_Ic" "Tasa.Dia" ? "decimal" ? ? ? ? ? ? yes ? no 31.72 yes "Tasa.Dia"
     _FldNameList[14]   > bdcentral.Facturacion.Int_Corrientes
"Int_Corrientes" "Int_Corrientes" "Int.Corriente" "->>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no 14.86 yes "Int.Corriente"
     _FldNameList[15]   > bdcentral.Facturacion.Int_MorCobrar
"Int_MorCobrar" "Int_MorCobrar" "Int. Mora" "->>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no 14.86 yes "Int. Mora"
     _FldNameList[16]   > bdcentral.Facturacion.Pagos
"Pagos" "Pagos" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Pagos"
     _FldNameList[17]   > bdcentral.Facturacion.Cargos
"Cargos" "Cargos" "Retiros" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Retiros"
     _FldNameList[18]   > bdcentral.Facturacion.OtrosCargos
"OtrosCargos" "OtrosCargos" "Comisión" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Comisión"
     _FldNameList[19]   > bdcentral.Facturacion.Seg_cartera
"Seg_cartera" "Seg_cartera" "Seg.Cartera" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Seg.Cartera"
     _FldNameList[20]   > bdcentral.Facturacion.Nuevo_Saldo
"Nuevo_Saldo" "Nuevo_Saldo" "Nvo.Saldo" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Nvo.Saldo"
     _FldNameList[21]   > bdcentral.Facturacion.Pago_Minimo
"Pago_Minimo" "Pago_Minimo" "Pago.Mínimo" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Pago.Mínimo"
     _FldNameList[22]   > bdcentral.Facturacion.Pago_Total
"Pago_Total" "Pago_Total" "Pago.Total" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Pago.Total"
     _FldNameList[23]   > bdcentral.Facturacion.Sobrecupo
"Sobrecupo" "Sobrecupo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[24]   > bdcentral.Facturacion.Val_Recaudo
"Val_Recaudo" "Val_Recaudo" "Tot.Recaudo" ? "decimal" ? ? ? ? ? ? yes ? no 14.29 yes "Tot.Recaudo"
     _FldNameList[25]   > bdcentral.Facturacion.Rec_Capital
"Rec_Capital" "Rec_Capital" "Rec.Capital" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Rec.Capital"
     _FldNameList[26]   > bdcentral.Facturacion.Rec_Honorarios
"Rec_Honorarios" "Rec_Honorarios" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.43 yes ?
     _FldNameList[27]   > bdcentral.Facturacion.Rec_IntCorrientes
"Rec_IntCorrientes" "Rec_IntCorrientes" "Rec.Int.Corrientes" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Rec.Int.Corrientes"
     _FldNameList[28]   > bdcentral.Facturacion.Rec_IntMora
"Rec_IntMora" "Rec_IntMora" "Rec.Int.Mora" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Rec.Int.Mora"
     _FldNameList[29]   > bdcentral.Facturacion.Rec_Segcartera
"Rec_Segcartera" "Rec_Segcartera" "Rec.Seg.Cartera" ? "decimal" ? ? ? ? ? ? yes ? no 28.86 yes "Rec.Seg.Cartera"
     _FldNameList[30]   > bdcentral.Facturacion.PagoTotal
"PagoTotal" "PagoTotal" "Canc.Tot.Cuota" ? "logical" ? ? ? ? ? ? yes ? no 16.86 yes "Canc.Tot.Cuota"
     _FldNameList[31]   > bdcentral.Facturacion.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[32]   > bdcentral.Facturacion.Tasa_Ea
"Tasa_Ea" "Tasa_Ea" "Tasa. EA" ? "decimal" ? ? ? ? ? ? yes ? no 28.86 yes "Tasa. EA"
     _FldNameList[33]   > bdcentral.Facturacion.Val_Atraso
"Val_Atraso" "Val_Atraso" "Val.Atraso.Capital" ? "decimal" ? ? ? ? ? ? yes ? no 14.29 yes "Val.Atraso.Capital"
     _FldNameList[34]   > bdcentral.Facturacion.Val_atrasokpen
"Val_atrasokpen" "Val_atrasokpen" "Val.Atraso.Capital.Pend" ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes "Val.Atraso.Capital.Pend"
     _FldNameList[35]   > bdcentral.Facturacion.ciudad
"ciudad" "ciudad" "Ciudad" ? "character" ? ? ? ? ? ? yes ? no 40 yes "Ciudad"
     _FldNameList[36]   > bdcentral.Facturacion.direccion
"direccion" "direccion" "Dirección" ? "character" ? ? ? ? ? ? yes ? no 60 yes "Dirección"
     _FldNameList[37]   > bdcentral.Facturacion.Fec_Inicial
"Fec_Inicial" "Fec_Inicial" "Fec.Ini" ? "date" ? ? ? ? ? ? yes ? no 11.14 yes "Fec.Ini"
     _FldNameList[38]   > bdcentral.Facturacion.Fec_Final
"Fec_Final" "Fec_Final" "Fec.Fin" ? "date" ? ? ? ? ? ? yes ? no 11.72 yes "Fec.Fin"
     _FldNameList[39]   > bdcentral.Facturacion.Fec_LimPago
"Fec_LimPago" "Fec_LimPago" "Fec.Lte.Pago" ? "date" ? ? ? ? ? ? yes ? no 20 yes "Fec.Lte.Pago"
     _FldNameList[40]   > bdcentral.Facturacion.nombre
"nombre" "nombre" "Nombres" ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[41]   > bdcentral.Facturacion.Nombre_Agencia
"Nombre_Agencia" "Nombre_Agencia" "Nom.Agencia" ? "character" ? ? ? ? ? ? yes ? yes 40 yes "Nom.Agencia"
     _FldNameList[42]   > bdcentral.Facturacion.Telefono
"Telefono" "Telefono" "Teléfono" ? "character" ? ? ? ? ? ? yes ? no 27.14 yes "Teléfono"
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

