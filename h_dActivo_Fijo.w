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

&glob DATA-LOGIC-PROCEDURE .p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

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
&Scoped-define INTERNAL-TABLES Act_Fijo

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Anos_Adepreciar Cen_Costos Codigo Cos_Historico Descripcion Estado~
 Fec_Avaluo Fec_Compra Fec_Contrato Fec_debaja Fec_Garantia Fec_IniDepre~
 Fec_Retiro Fec_Venta Grupo Mejoras Neto Nit_Proveedor Nit_Responsable~
 Nit_Seguro Nombre Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado~
 Sdo_Depre ValDepAcum ValDepMes Val_Compra Val_Garantia Vto_Seguro
&Scoped-define ENABLED-FIELDS-IN-Act_Fijo Agencia Anos_Adepreciar ~
Cen_Costos Codigo Cos_Historico Descripcion Estado Fec_Avaluo Fec_Compra ~
Fec_Contrato Fec_debaja Fec_Garantia Fec_IniDepre Fec_Retiro Fec_Venta ~
Grupo Mejoras Neto Nit_Proveedor Nit_Responsable Nit_Seguro Nombre ~
Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado Sdo_Depre ValDepAcum ~
ValDepMes Val_Compra Val_Garantia Vto_Seguro 
&Scoped-Define DATA-FIELDS  Agencia Anos_Adepreciar Cen_Costos Codigo Cos_Historico Descripcion Estado~
 Fec_Avaluo Fec_Compra Fec_Contrato Fec_debaja Fec_Garantia Fec_IniDepre~
 Fec_Retiro Fec_Venta Grupo Mejoras Neto Nit_Proveedor Nit_Responsable~
 Nit_Seguro Nombre Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado~
 Sdo_Depre ValDepAcum ValDepMes Val_Compra Val_Garantia Vto_Seguro
&Scoped-define DATA-FIELDS-IN-Act_Fijo Agencia Anos_Adepreciar Cen_Costos ~
Codigo Cos_Historico Descripcion Estado Fec_Avaluo Fec_Compra Fec_Contrato ~
Fec_debaja Fec_Garantia Fec_IniDepre Fec_Retiro Fec_Venta Grupo Mejoras ~
Neto Nit_Proveedor Nit_Responsable Nit_Seguro Nombre Nro_Factura Nro_Seguro ~
Ord_Compra Per_Depreciado Sdo_Depre ValDepAcum ValDepMes Val_Compra ~
Val_Garantia Vto_Seguro 
&Scoped-Define MANDATORY-FIELDS  Cen_Costos Codigo Nombre
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "h_dActivo_Fijo.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Act_Fijo NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Act_Fijo NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Act_Fijo
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Act_Fijo


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Act_Fijo SCROLLING.
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
     _TblList          = "bdcentral.Act_Fijo"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Act_Fijo.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[2]   > bdcentral.Act_Fijo.Anos_Adepreciar
"Anos_Adepreciar" "Anos_Adepreciar" ? ? "integer" ? ? ? ? ? ? yes ? no 7.72 yes ?
     _FldNameList[3]   > bdcentral.Act_Fijo.Cen_Costos
"Cen_Costos" "Cen_Costos" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.86 yes ?
     _FldNameList[4]   > bdcentral.Act_Fijo.Codigo
"Codigo" "Codigo" ? ? "character" ? ? ? ? ? ? yes ? yes 12.57 yes ?
     _FldNameList[5]   > bdcentral.Act_Fijo.Cos_Historico
"Cos_Historico" "Cos_Historico" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[6]   > bdcentral.Act_Fijo.Descripcion
"Descripcion" "Descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes ?
     _FldNameList[7]   > bdcentral.Act_Fijo.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[8]   > bdcentral.Act_Fijo.Fec_Avaluo
"Fec_Avaluo" "Fec_Avaluo" ? ? "date" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[9]   > bdcentral.Act_Fijo.Fec_Compra
"Fec_Compra" "Fec_Compra" ? ? "date" ? ? ? ? ? ? yes ? no 16.14 yes ?
     _FldNameList[10]   > bdcentral.Act_Fijo.Fec_Contrato
"Fec_Contrato" "Fec_Contrato" ? ? "date" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[11]   > bdcentral.Act_Fijo.Fec_debaja
"Fec_debaja" "Fec_debaja" ? ? "date" ? ? ? ? ? ? yes ? no 13.29 yes ?
     _FldNameList[12]   > bdcentral.Act_Fijo.Fec_Garantia
"Fec_Garantia" "Fec_Garantia" ? ? "date" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[13]   > bdcentral.Act_Fijo.Fec_IniDepre
"Fec_IniDepre" "Fec_IniDepre" ? ? "date" ? ? ? ? ? ? yes ? no 12.14 yes ?
     _FldNameList[14]   > bdcentral.Act_Fijo.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[15]   > bdcentral.Act_Fijo.Fec_Venta
"Fec_Venta" "Fec_Venta" ? ? "date" ? ? ? ? ? ? yes ? no 14.14 yes ?
     _FldNameList[16]   > bdcentral.Act_Fijo.Grupo
"Grupo" "Grupo" ? ? "integer" ? ? ? ? ? ? yes ? no 5.72 yes ?
     _FldNameList[17]   > bdcentral.Act_Fijo.Mejoras
"Mejoras" "Mejoras" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.86 yes ?
     _FldNameList[18]   > bdcentral.Act_Fijo.Neto
"Neto" "Neto" ? ? "decimal" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[19]   > bdcentral.Act_Fijo.Nit_Proveedor
"Nit_Proveedor" "Nit_Proveedor" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[20]   > bdcentral.Act_Fijo.Nit_Responsable
"Nit_Responsable" "Nit_Responsable" ? ? "character" ? ? ? ? ? ? yes ? no 12.29 yes ?
     _FldNameList[21]   > bdcentral.Act_Fijo.Nit_Seguro
"Nit_Seguro" "Nit_Seguro" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[22]   > bdcentral.Act_Fijo.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? yes ? yes 40 yes ?
     _FldNameList[23]   > bdcentral.Act_Fijo.Nro_Factura
"Nro_Factura" "Nro_Factura" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[24]   > bdcentral.Act_Fijo.Nro_Seguro
"Nro_Seguro" "Nro_Seguro" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[25]   > bdcentral.Act_Fijo.Ord_Compra
"Ord_Compra" "Ord_Compra" ? ? "character" ? ? ? ? ? ? yes ? no 14.43 yes ?
     _FldNameList[26]   > bdcentral.Act_Fijo.Per_Depreciado
"Per_Depreciado" "Per_Depreciado" ? ? "integer" ? ? ? ? ? ? yes ? no 20.29 yes ?
     _FldNameList[27]   > bdcentral.Act_Fijo.Sdo_Depre
"Sdo_Depre" "Sdo_Depre" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[28]   > bdcentral.Act_Fijo.ValDepAcum
"ValDepAcum" "ValDepAcum" ? ? "decimal" ? ? ? ? ? ? yes ? no 25.29 yes ?
     _FldNameList[29]   > bdcentral.Act_Fijo.ValDepMes
"ValDepMes" "ValDepMes" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.14 yes ?
     _FldNameList[30]   > bdcentral.Act_Fijo.Val_Compra
"Val_Compra" "Val_Compra" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[31]   > bdcentral.Act_Fijo.Val_Garantia
"Val_Garantia" "Val_Garantia" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.43 yes ?
     _FldNameList[32]   > bdcentral.Act_Fijo.Vto_Seguro
"Vto_Seguro" "Vto_Seguro" ? ? "date" ? ? ? ? ? ? yes ? no 18.43 yes ?
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

