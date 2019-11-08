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
&Scoped-define INTERNAL-TABLES Cuentas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Cam_Captrabajo Cam_Posicion Car_Efectivo Cod_base Cod_Caja Cod_Enlace~
 Cod_FlujoEfec Cod_Formato Cod_FormatoExt Cod_Presupuesto Ctr_Naturaleza~
 Cuenta Estado Fec_Creacion Fec_Retiro Id_Base Id_Caja Id_CenCostos~
 Id_Cuenta Id_detalle Id_Doc Id_Enlace Id_Excendentes Id_FinAno~
 Id_FlujoEfectivo Id_Nit id_nodlle Id_NoMvto Id_Paag Id_Pcto Id_Presupuesto~
 Id_Renuncia Id_Total Naturaleza Nit_FinAno Nivel Nombre Tipo Cta_Homologada
&Scoped-define ENABLED-FIELDS-IN-Cuentas Cam_Captrabajo Cam_Posicion ~
Car_Efectivo Cod_base Cod_Caja Cod_Enlace Cod_FlujoEfec Cod_Formato ~
Cod_FormatoExt Cod_Presupuesto Ctr_Naturaleza Cuenta Estado Fec_Creacion ~
Fec_Retiro Id_Base Id_Caja Id_CenCostos Id_Cuenta Id_detalle Id_Doc ~
Id_Enlace Id_Excendentes Id_FinAno Id_FlujoEfectivo Id_Nit id_nodlle ~
Id_NoMvto Id_Paag Id_Pcto Id_Presupuesto Id_Renuncia Id_Total Naturaleza ~
Nit_FinAno Nivel Nombre Tipo Cta_Homologada 
&Scoped-Define DATA-FIELDS  Cam_Captrabajo Cam_Posicion Car_Efectivo Cod_base Cod_Caja Cod_Enlace~
 Cod_FlujoEfec Cod_Formato Cod_FormatoExt Cod_Presupuesto Ctr_Naturaleza~
 Cuenta Estado Fec_Creacion Fec_Retiro Id_Base Id_Caja Id_CenCostos~
 Id_Cuenta Id_detalle Id_Doc Id_Enlace Id_Excendentes Id_FinAno~
 Id_FlujoEfectivo Id_Nit id_nodlle Id_NoMvto Id_Paag Id_Pcto Id_Presupuesto~
 Id_Renuncia Id_Total Naturaleza Nit_FinAno Nivel Nombre Tipo Cta_Homologada
&Scoped-define DATA-FIELDS-IN-Cuentas Cam_Captrabajo Cam_Posicion ~
Car_Efectivo Cod_base Cod_Caja Cod_Enlace Cod_FlujoEfec Cod_Formato ~
Cod_FormatoExt Cod_Presupuesto Ctr_Naturaleza Cuenta Estado Fec_Creacion ~
Fec_Retiro Id_Base Id_Caja Id_CenCostos Id_Cuenta Id_detalle Id_Doc ~
Id_Enlace Id_Excendentes Id_FinAno Id_FlujoEfectivo Id_Nit id_nodlle ~
Id_NoMvto Id_Paag Id_Pcto Id_Presupuesto Id_Renuncia Id_Total Naturaleza ~
Nit_FinAno Nivel Nombre Tipo Cta_Homologada 
&Scoped-Define MANDATORY-FIELDS  Ctr_Naturaleza Cuenta Estado Fec_Creacion Id_Base Id_CenCostos Id_Cuenta~
 Id_Doc Id_Enlace Id_Nit Id_Paag Id_Pcto Id_Presupuesto Nivel Nombre
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dcuentas.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Cuentas NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Cuentas NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Cuentas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Cuentas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Cuentas SCROLLING.
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
         WIDTH              = 32.29.
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
     _TblList          = "bdcentral.Cuentas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Cuentas.Cam_Captrabajo
"Cam_Captrabajo" "Cam_Captrabajo" ? ? "integer" ? ? ? ? ? ? yes ? no 21.57 yes ?
     _FldNameList[2]   > bdcentral.Cuentas.Cam_Posicion
"Cam_Posicion" "Cam_Posicion" ? ? "integer" ? ? ? ? ? ? yes ? no 24.57 yes ?
     _FldNameList[3]   > bdcentral.Cuentas.Car_Efectivo
"Car_Efectivo" "Car_Efectivo" ? ? "integer" ? ? ? ? ? ? yes ? no 12.72 yes ?
     _FldNameList[4]   > bdcentral.Cuentas.Cod_base
"Cod_base" "Cod_base" ? ? "character" ? ? ? ? ? ? yes ? no 21.72 yes ?
     _FldNameList[5]   > bdcentral.Cuentas.Cod_Caja
"Cod_Caja" "Cod_Caja" ? ? "integer" ? ? ? ? ? ? yes ? no 11.14 yes ?
     _FldNameList[6]   > bdcentral.Cuentas.Cod_Enlace
"Cod_Enlace" "Cod_Enlace" ? ? "integer" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[7]   > bdcentral.Cuentas.Cod_FlujoEfec
"Cod_FlujoEfec" "Cod_FlujoEfec" ? ? "character" ? ? ? ? ? ? yes ? no 19.57 yes ?
     _FldNameList[8]   > bdcentral.Cuentas.Cod_Formato
"Cod_Formato" "Cod_Formato" ? ? "integer" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[9]   > bdcentral.Cuentas.Cod_FormatoExt
"Cod_FormatoExt" "Cod_FormatoExt" ? ? "integer" ? ? ? ? ? ? yes ? no 11.72 yes ?
     _FldNameList[10]   > bdcentral.Cuentas.Cod_Presupuesto
"Cod_Presupuesto" "Cod_Presupuesto" ? ? "character" ? ? ? ? ? ? yes ? no 19.14 yes ?
     _FldNameList[11]   > bdcentral.Cuentas.Ctr_Naturaleza
"Ctr_Naturaleza" "Ctr_Naturaleza" ? "Si/No" "logical" ? ? ? ? ? ? yes ? yes 32.14 yes ?
     _FldNameList[12]   > bdcentral.Cuentas.Cuenta
"Cuenta" "Cuenta" ? ? "character" ? ? ? ? ? ? yes ? yes 16.29 yes ?
     _FldNameList[13]   > bdcentral.Cuentas.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.43 yes ?
     _FldNameList[14]   > bdcentral.Cuentas.Fec_Creacion
"Fec_Creacion" "Fec_Creacion" ? ? "date" ? ? ? ? ? ? yes ? yes 17.14 yes ?
     _FldNameList[15]   > bdcentral.Cuentas.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[16]   > bdcentral.Cuentas.Id_Base
"Id_Base" "Id_Base" ? "Si/No" "logical" ? ? ? ? ? ? yes ? yes 12.29 yes ?
     _FldNameList[17]   > bdcentral.Cuentas.Id_Caja
"Id_Caja" "Id_Caja" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 19 yes ?
     _FldNameList[18]   > bdcentral.Cuentas.Id_CenCostos
"Id_CenCostos" "Id_CenCostos" ? "Si/No" "logical" ? ? ? ? ? ? yes ? yes 23.29 yes ?
     _FldNameList[19]   > bdcentral.Cuentas.Id_Cuenta
"Id_Cuenta" "Id_Cuenta" ? ? "integer" ? ? ? ? ? ? yes ? yes 11.86 yes ?
     _FldNameList[20]   > bdcentral.Cuentas.Id_detalle
"Id_detalle" "Id_detalle" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 31.43 yes ?
     _FldNameList[21]   > bdcentral.Cuentas.Id_Doc
"Id_Doc" "Id_Doc" ? "Si/No" "logical" ? ? ? ? ? ? yes ? yes 26.43 yes ?
     _FldNameList[22]   > bdcentral.Cuentas.Id_Enlace
"Id_Enlace" "Id_Enlace" ? "Si/No" "logical" ? ? ? ? ? ? yes ? yes 17.14 yes ?
     _FldNameList[23]   > bdcentral.Cuentas.Id_Excendentes
"Id_Excendentes" "Id_Excendentes" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[24]   > bdcentral.Cuentas.Id_FinAno
"Id_FinAno" "Id_FinAno" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 23.43 yes ?
     _FldNameList[25]   > bdcentral.Cuentas.Id_FlujoEfectivo
"Id_FlujoEfectivo" "Id_FlujoEfectivo" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 24 yes ?
     _FldNameList[26]   > bdcentral.Cuentas.Id_Nit
"Id_Nit" "Id_Nit" ? "Si/No" "logical" ? ? ? ? ? ? yes ? yes 10 yes ?
     _FldNameList[27]   > bdcentral.Cuentas.id_nodlle
"id_nodlle" "id_nodlle" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 35.14 yes ?
     _FldNameList[28]   > bdcentral.Cuentas.Id_NoMvto
"Id_NoMvto" "Id_NoMvto" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 24.86 yes ?
     _FldNameList[29]   > bdcentral.Cuentas.Id_Paag
"Id_Paag" "Id_Paag" ? "Si/No" "logical" ? ? ? ? ? ? yes ? yes 14.29 yes ?
     _FldNameList[30]   > bdcentral.Cuentas.Id_Pcto
"Id_Pcto" "Id_Pcto" ? "Si/No" "logical" ? ? ? ? ? ? yes ? yes 8.14 yes ?
     _FldNameList[31]   > bdcentral.Cuentas.Id_Presupuesto
"Id_Presupuesto" "Id_Presupuesto" ? "Si/No" "logical" ? ? ? ? ? ? yes ? yes 19.14 yes ?
     _FldNameList[32]   > bdcentral.Cuentas.Id_Renuncia
"Id_Renuncia" "Id_Renuncia" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 11.57 yes ?
     _FldNameList[33]   > bdcentral.Cuentas.Id_Total
"Id_Total" "Id_Total" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 20.86 yes ?
     _FldNameList[34]   > bdcentral.Cuentas.Naturaleza
"Naturaleza" "Naturaleza" "Nat." ? "character" ? ? ? ? ? ? yes ? no 10.14 yes "Nat."
     _FldNameList[35]   > bdcentral.Cuentas.Nit_FinAno
"Nit_FinAno" "Nit_FinAno" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[36]   > bdcentral.Cuentas.Nivel
"Nivel" "Nivel" ? ? "integer" ? ? ? ? ? ? yes ? yes 4.86 yes ?
     _FldNameList[37]   > bdcentral.Cuentas.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? yes ? yes 40 yes ?
     _FldNameList[38]   > bdcentral.Cuentas.Tipo
"Tipo" "Tipo" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes ?
     _FldNameList[39]   > bdcentral.Cuentas.Cta_Homologada
"Cta_Homologada" "Cta_Homologada" ? ? "character" ? ? ? ? ? ? yes ? no 16.43 yes ?
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

