&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T1Cfg_Instancias NO-UNDO LIKE Cfg_Instancias.
DEFINE TEMP-TABLE T1Instancias NO-UNDO LIKE Instancias.
DEFINE TEMP-TABLE T1Saro NO-UNDO LIKE Saro.
DEFINE TEMP-TABLE T2Saro NO-UNDO LIKE Saro.



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
&Scoped-define INTERNAL-TABLES Saro

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Canal_Servicio Cargo_JC Clase_Proceso Clase_Producto~
 Clase_ROperativo Cod_Evento Cod_Factor1 Cod_Factor2 Cod_Producto~
 Cuantia_Recup_Seg Cuantia_Recup_Tot Cuantia_Riesgo Descrip_Anulado~
 Descrip_Evento Divisa Especifique_Factores Estado_Evento Factores_Riesgo~
 Fec_Anu_Even Fec_Aprobacion Fec_Conta_Even Fec_Des_Even Fec_Fin_Even~
 Fec_Grabacion Fec_Ini_Even Hora_Anu_Even Hora_Aprobacion Hora_Cont_Even~
 Hora_Des_Even Hora_Fin_Even Hora_Grabacion Hora_Ini_Even Lineas_Operat1~
 Lineas_Operat2 Lineas_Operat3 Lineas_Operat4 Lineas_Operat5 Nit Nit_seguro~
 Num_Referencia Porc_Even1 Porc_Even2 Porc_Even3 Porc_Even4 Porc_Even5~
 Tip_Credito Tip_perdida Usuario Usu_Anulacion Zona_Geografica
&Scoped-define ENABLED-FIELDS-IN-Saro Agencia Canal_Servicio Cargo_JC ~
Clase_Proceso Clase_Producto Clase_ROperativo Cod_Evento Cod_Factor1 ~
Cod_Factor2 Cod_Producto Cuantia_Recup_Seg Cuantia_Recup_Tot Cuantia_Riesgo ~
Descrip_Anulado Descrip_Evento Divisa Especifique_Factores Estado_Evento ~
Factores_Riesgo Fec_Anu_Even Fec_Aprobacion Fec_Conta_Even Fec_Des_Even ~
Fec_Fin_Even Fec_Grabacion Fec_Ini_Even Hora_Anu_Even Hora_Aprobacion ~
Hora_Cont_Even Hora_Des_Even Hora_Fin_Even Hora_Grabacion Hora_Ini_Even ~
Lineas_Operat1 Lineas_Operat2 Lineas_Operat3 Lineas_Operat4 Lineas_Operat5 ~
Nit Nit_seguro Num_Referencia Porc_Even1 Porc_Even2 Porc_Even3 Porc_Even4 ~
Porc_Even5 Tip_Credito Tip_perdida Usuario Usu_Anulacion Zona_Geografica 
&Scoped-Define DATA-FIELDS  Agencia Canal_Servicio Cargo_JC Clase_Proceso Clase_Producto~
 Clase_ROperativo Cod_Evento Cod_Factor1 Cod_Factor2 Cod_Producto~
 Cuantia_Recup_Seg Cuantia_Recup_Tot Cuantia_Riesgo Descrip_Anulado~
 Descrip_Evento Divisa Especifique_Factores Estado_Evento Factores_Riesgo~
 Fec_Anu_Even Fec_Aprobacion Fec_Conta_Even Fec_Des_Even Fec_Fin_Even~
 Fec_Grabacion Fec_Ini_Even Hora_Anu_Even Hora_Aprobacion Hora_Cont_Even~
 Hora_Des_Even Hora_Fin_Even Hora_Grabacion Hora_Ini_Even Lineas_Operat1~
 Lineas_Operat2 Lineas_Operat3 Lineas_Operat4 Lineas_Operat5 Nit Nit_seguro~
 Num_Referencia Porc_Even1 Porc_Even2 Porc_Even3 Porc_Even4 Porc_Even5~
 Tip_Credito Tip_perdida Usuario Usu_Anulacion Zona_Geografica
&Scoped-define DATA-FIELDS-IN-Saro Agencia Canal_Servicio Cargo_JC ~
Clase_Proceso Clase_Producto Clase_ROperativo Cod_Evento Cod_Factor1 ~
Cod_Factor2 Cod_Producto Cuantia_Recup_Seg Cuantia_Recup_Tot Cuantia_Riesgo ~
Descrip_Anulado Descrip_Evento Divisa Especifique_Factores Estado_Evento ~
Factores_Riesgo Fec_Anu_Even Fec_Aprobacion Fec_Conta_Even Fec_Des_Even ~
Fec_Fin_Even Fec_Grabacion Fec_Ini_Even Hora_Anu_Even Hora_Aprobacion ~
Hora_Cont_Even Hora_Des_Even Hora_Fin_Even Hora_Grabacion Hora_Ini_Even ~
Lineas_Operat1 Lineas_Operat2 Lineas_Operat3 Lineas_Operat4 Lineas_Operat5 ~
Nit Nit_seguro Num_Referencia Porc_Even1 Porc_Even2 Porc_Even3 Porc_Even4 ~
Porc_Even5 Tip_Credito Tip_perdida Usuario Usu_Anulacion Zona_Geografica 
&Scoped-Define MANDATORY-FIELDS  Num_Referencia
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Lineas_Operat1 = Saro.Lineas_Operat[1]~
  rowObject.Lineas_Operat2 = Saro.Lineas_Operat[2]~
  rowObject.Lineas_Operat3 = Saro.Lineas_Operat[3]~
  rowObject.Lineas_Operat4 = Saro.Lineas_Operat[4]~
  rowObject.Lineas_Operat5 = Saro.Lineas_Operat[5]~
  rowObject.Porc_Even1 = Saro.Porc_Even[1]~
  rowObject.Porc_Even2 = Saro.Porc_Even[2]~
  rowObject.Porc_Even3 = Saro.Porc_Even[3]~
  rowObject.Porc_Even4 = Saro.Porc_Even[4]~
  rowObject.Porc_Even5 = Saro.Porc_Even[5]
&Scoped-Define DATA-FIELD-DEFS "dsaro.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Saro NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Saro NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Saro
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Saro


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Saro SCROLLING.
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
   Temp-Tables and Buffers:
      TABLE: T1Cfg_Instancias T "?" NO-UNDO bdcentral Cfg_Instancias
      TABLE: T1Instancias T "?" NO-UNDO bdcentral Instancias
      TABLE: T1Saro T "?" NO-UNDO bdcentral Saro
      TABLE: T2Saro T "?" NO-UNDO bdcentral Saro
   END-TABLES.
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
         WIDTH              = 27.72.
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
     _TblList          = "bdcentral.Saro"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Saro.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 14.43 yes ?
     _FldNameList[2]   > bdcentral.Saro.Canal_Servicio
"Canal_Servicio" "Canal_Servicio" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[3]   > bdcentral.Saro.Cargo_JC
"Cargo_JC" "Cargo_JC" ? ? "integer" ? ? ? ? ? ? yes ? no 5.43 yes ?
     _FldNameList[4]   > bdcentral.Saro.Clase_Proceso
"Clase_Proceso" "Clase_Proceso" ? ? "integer" ? ? ? ? ? ? yes ? no 13.43 yes ?
     _FldNameList[5]   > bdcentral.Saro.Clase_Producto
"Clase_Producto" "Clase_Producto" ? ? "integer" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[6]   > bdcentral.Saro.Clase_ROperativo
"Clase_ROperativo" "Clase_ROperativo" ? ? "integer" ? ? ? ? ? ? yes ? no 22 yes ?
     _FldNameList[7]   > bdcentral.Saro.Cod_Evento
"Cod_Evento" "Cod_Evento" ? ? "character" ? ? ? ? ? ? yes ? no 11.14 yes ?
     _FldNameList[8]   > bdcentral.Saro.Cod_Factor1
"Cod_Factor1" "Cod_Factor1" ? ? "character" ? ? ? ? ? ? yes ? no 11.57 yes ?
     _FldNameList[9]   > bdcentral.Saro.Cod_Factor2
"Cod_Factor2" "Cod_Factor2" ? ? "character" ? ? ? ? ? ? yes ? no 11.57 yes ?
     _FldNameList[10]   > bdcentral.Saro.Cod_Producto
"Cod_Producto" "Cod_Producto" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[11]   > bdcentral.Saro.Cuantia_Recup_Seg
"Cuantia_Recup_Seg" "Cuantia_Recup_Seg" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[12]   > bdcentral.Saro.Cuantia_Recup_Tot
"Cuantia_Recup_Tot" "Cuantia_Recup_Tot" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.86 yes ?
     _FldNameList[13]   > bdcentral.Saro.Cuantia_Riesgo
"Cuantia_Riesgo" "Cuantia_Riesgo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[14]   > bdcentral.Saro.Descrip_Anulado
"Descrip_Anulado" "Descrip_Anulado" ? ? "character" ? ? ? ? ? ? yes ? no 150 yes ?
     _FldNameList[15]   > bdcentral.Saro.Descrip_Evento
"Descrip_Evento" "Descrip_Evento" ? ? "character" ? ? ? ? ? ? yes ? no 250 yes ?
     _FldNameList[16]   > bdcentral.Saro.Divisa
"Divisa" "Divisa" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[17]   > bdcentral.Saro.Especifique_Factores
"Especifique_Factores" "Especifique_Factores" ? ? "character" ? ? ? ? ? ? yes ? no 80 yes ?
     _FldNameList[18]   > bdcentral.Saro.Estado_Evento
"Estado_Evento" "Estado_Evento" ? ? "integer" ? ? ? ? ? ? yes ? no 13.43 yes ?
     _FldNameList[19]   > bdcentral.Saro.Factores_Riesgo
"Factores_Riesgo" "Factores_Riesgo" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[20]   > bdcentral.Saro.Fec_Anu_Even
"Fec_Anu_Even" "Fec_Anu_Even" ? ? "date" ? ? ? ? ? ? yes ? no 22.14 yes ?
     _FldNameList[21]   > bdcentral.Saro.Fec_Aprobacion
"Fec_Aprobacion" "Fec_Aprobacion" ? ? "date" ? ? ? ? ? ? yes ? no 26.29 yes ?
     _FldNameList[22]   > bdcentral.Saro.Fec_Conta_Even
"Fec_Conta_Even" "Fec_Conta_Even" ? ? "date" ? ? ? ? ? ? yes ? no 27.43 yes ?
     _FldNameList[23]   > bdcentral.Saro.Fec_Des_Even
"Fec_Des_Even" "Fec_Des_Even" ? ? "date" ? ? ? ? ? ? yes ? no 22 yes ?
     _FldNameList[24]   > bdcentral.Saro.Fec_Fin_Even
"Fec_Fin_Even" "Fec_Fin_Even" ? ? "date" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[25]   > bdcentral.Saro.Fec_Grabacion
"Fec_Grabacion" "Fec_Grabacion" ? ? "date" ? ? ? ? ? ? yes ? no 25.43 yes ?
     _FldNameList[26]   > bdcentral.Saro.Fec_Ini_Even
"Fec_Ini_Even" "Fec_Ini_Even" ? ? "date" ? ? ? ? ? ? yes ? no 18.14 yes ?
     _FldNameList[27]   > bdcentral.Saro.Hora_Anu_Even
"Hora_Anu_Even" "Hora_Anu_Even" ? ? "character" ? ? ? ? ? ? yes ? no 21 yes ?
     _FldNameList[28]   > bdcentral.Saro.Hora_Aprobacion
"Hora_Aprobacion" "Hora_Aprobacion" ? ? "character" ? ? ? ? ? ? yes ? no 22.29 yes ?
     _FldNameList[29]   > bdcentral.Saro.Hora_Cont_Even
"Hora_Cont_Even" "Hora_Cont_Even" ? ? "character" ? ? ? ? ? ? yes ? no 26.86 yes ?
     _FldNameList[30]   > bdcentral.Saro.Hora_Des_Even
"Hora_Des_Even" "Hora_Des_Even" ? ? "character" ? ? ? ? ? ? yes ? no 26.57 yes ?
     _FldNameList[31]   > bdcentral.Saro.Hora_Fin_Even
"Hora_Fin_Even" "Hora_Fin_Even" ? ? "character" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[32]   > bdcentral.Saro.Hora_Grabacion
"Hora_Grabacion" "Hora_Grabacion" ? ? "character" ? ? ? ? ? ? yes ? no 21.43 yes ?
     _FldNameList[33]   > bdcentral.Saro.Hora_Ini_Even
"Hora_Ini_Even" "Hora_Ini_Even" ? ? "character" ? ? ? ? ? ? yes ? no 17 yes ?
     _FldNameList[34]   > bdcentral.Saro.Lineas_Operat[1]
"Lineas_Operat[1]" "Lineas_Operat1" ? ? "integer" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[35]   > bdcentral.Saro.Lineas_Operat[2]
"Lineas_Operat[2]" "Lineas_Operat2" ? ? "integer" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[36]   > bdcentral.Saro.Lineas_Operat[3]
"Lineas_Operat[3]" "Lineas_Operat3" ? ? "integer" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[37]   > bdcentral.Saro.Lineas_Operat[4]
"Lineas_Operat[4]" "Lineas_Operat4" ? ? "integer" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[38]   > bdcentral.Saro.Lineas_Operat[5]
"Lineas_Operat[5]" "Lineas_Operat5" ? ? "integer" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[39]   > bdcentral.Saro.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[40]   > bdcentral.Saro.Nit_seguro
"Nit_seguro" "Nit_seguro" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[41]   > bdcentral.Saro.Num_Referencia
"Num_Referencia" "Num_Referencia" ? ? "integer" ? ? ? ? ? ? yes ? yes 13.86 yes ?
     _FldNameList[42]   > bdcentral.Saro.Porc_Even[1]
"Porc_Even[1]" "Porc_Even1" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[43]   > bdcentral.Saro.Porc_Even[2]
"Porc_Even[2]" "Porc_Even2" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[44]   > bdcentral.Saro.Porc_Even[3]
"Porc_Even[3]" "Porc_Even3" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[45]   > bdcentral.Saro.Porc_Even[4]
"Porc_Even[4]" "Porc_Even4" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[46]   > bdcentral.Saro.Porc_Even[5]
"Porc_Even[5]" "Porc_Even5" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[47]   > bdcentral.Saro.Tip_Credito
"Tip_Credito" "Tip_Credito" ? ? "integer" ? ? ? ? ? ? yes ? no 24.14 yes ?
     _FldNameList[48]   > bdcentral.Saro.Tip_perdida
"Tip_perdida" "Tip_perdida" ? ? "integer" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[49]   > bdcentral.Saro.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? no 17.29 yes ?
     _FldNameList[50]   > bdcentral.Saro.Usu_Anulacion
"Usu_Anulacion" "Usu_Anulacion" ? ? "character" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[51]   > bdcentral.Saro.Zona_Geografica
"Zona_Geografica" "Zona_Geografica" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes ?
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

