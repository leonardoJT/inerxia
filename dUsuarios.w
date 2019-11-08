&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
     
/* oakley */     

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
&Scoped-define INTERNAL-TABLES Usuarios

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Clave Dia_Permiso1 Dia_Permiso2 Dia_Permiso3 Dia_Permiso4~
 Dia_Permiso5 Dia_Permiso6 Dia_Permiso7 Estacion Estado Fec_Creacion~
 Fec_Retiro Fec_UltCam Grupo Hentrada Hfinman1 Hfinman2 Hfinman3 Hfinman4~
 Hfinman5 Hfinman6 Hfinman7 Hfintar1 Hfintar2 Hfintar3 Hfintar4 Hfintar5~
 Hfintar6 Hfintar7 HIniMan1 HIniMan2 HIniMan3 HIniMan4 HIniMan5 HIniMan6~
 HIniMan7 Hinitar1 Hinitar2 Hinitar3 Hinitar4 Hinitar5 Hinitar6 Hinitar7~
 IdHorario Id_AccSimultaneo Id_AllAgencias Id_Bloqueo id_cartera~
 id_cobrojuridico Id_Entrada Id_Estacion Id_OpeOfi MaxVal_Efectivo Nit~
 Nombre Pedir_Clave Prioridad proactual Tie_Renovacion Tip_Menu Usuario
&Scoped-define ENABLED-FIELDS-IN-Usuarios Agencia Clave Dia_Permiso1 ~
Dia_Permiso2 Dia_Permiso3 Dia_Permiso4 Dia_Permiso5 Dia_Permiso6 ~
Dia_Permiso7 Estacion Estado Fec_Creacion Fec_Retiro Fec_UltCam Grupo ~
Hentrada Hfinman1 Hfinman2 Hfinman3 Hfinman4 Hfinman5 Hfinman6 Hfinman7 ~
Hfintar1 Hfintar2 Hfintar3 Hfintar4 Hfintar5 Hfintar6 Hfintar7 HIniMan1 ~
HIniMan2 HIniMan3 HIniMan4 HIniMan5 HIniMan6 HIniMan7 Hinitar1 Hinitar2 ~
Hinitar3 Hinitar4 Hinitar5 Hinitar6 Hinitar7 IdHorario Id_AccSimultaneo ~
Id_AllAgencias Id_Bloqueo id_cartera id_cobrojuridico Id_Entrada ~
Id_Estacion Id_OpeOfi MaxVal_Efectivo Nit Nombre Pedir_Clave Prioridad ~
proactual Tie_Renovacion Tip_Menu Usuario 
&Scoped-Define DATA-FIELDS  Agencia Clave Dia_Permiso1 Dia_Permiso2 Dia_Permiso3 Dia_Permiso4~
 Dia_Permiso5 Dia_Permiso6 Dia_Permiso7 Estacion Estado Fec_Creacion~
 Fec_Retiro Fec_UltCam Grupo Hentrada Hfinman1 Hfinman2 Hfinman3 Hfinman4~
 Hfinman5 Hfinman6 Hfinman7 Hfintar1 Hfintar2 Hfintar3 Hfintar4 Hfintar5~
 Hfintar6 Hfintar7 HIniMan1 HIniMan2 HIniMan3 HIniMan4 HIniMan5 HIniMan6~
 HIniMan7 Hinitar1 Hinitar2 Hinitar3 Hinitar4 Hinitar5 Hinitar6 Hinitar7~
 IdHorario Id_AccSimultaneo Id_AllAgencias Id_Bloqueo id_cartera~
 id_cobrojuridico Id_Entrada Id_Estacion Id_OpeOfi MaxVal_Efectivo Nit~
 Nombre Pedir_Clave Prioridad proactual Tie_Renovacion Tip_Menu Usuario
&Scoped-define DATA-FIELDS-IN-Usuarios Agencia Clave Dia_Permiso1 ~
Dia_Permiso2 Dia_Permiso3 Dia_Permiso4 Dia_Permiso5 Dia_Permiso6 ~
Dia_Permiso7 Estacion Estado Fec_Creacion Fec_Retiro Fec_UltCam Grupo ~
Hentrada Hfinman1 Hfinman2 Hfinman3 Hfinman4 Hfinman5 Hfinman6 Hfinman7 ~
Hfintar1 Hfintar2 Hfintar3 Hfintar4 Hfintar5 Hfintar6 Hfintar7 HIniMan1 ~
HIniMan2 HIniMan3 HIniMan4 HIniMan5 HIniMan6 HIniMan7 Hinitar1 Hinitar2 ~
Hinitar3 Hinitar4 Hinitar5 Hinitar6 Hinitar7 IdHorario Id_AccSimultaneo ~
Id_AllAgencias Id_Bloqueo id_cartera id_cobrojuridico Id_Entrada ~
Id_Estacion Id_OpeOfi MaxVal_Efectivo Nit Nombre Pedir_Clave Prioridad ~
proactual Tie_Renovacion Tip_Menu Usuario 
&Scoped-Define MANDATORY-FIELDS  Clave Dia_Permiso1 Dia_Permiso2 Dia_Permiso3 Dia_Permiso4 Dia_Permiso5~
 Dia_Permiso6 Dia_Permiso7 Estado Fec_Creacion Grupo HIniMan1 HIniMan2~
 HIniMan3 HIniMan4 HIniMan5 HIniMan6 HIniMan7 Id_OpeOfi Nombre Prioridad~
 Usuario
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Dia_Permiso1 = Usuarios.Dia_Permiso[1]~
  rowObject.Dia_Permiso2 = Usuarios.Dia_Permiso[2]~
  rowObject.Dia_Permiso3 = Usuarios.Dia_Permiso[3]~
  rowObject.Dia_Permiso4 = Usuarios.Dia_Permiso[4]~
  rowObject.Dia_Permiso5 = Usuarios.Dia_Permiso[5]~
  rowObject.Dia_Permiso6 = Usuarios.Dia_Permiso[6]~
  rowObject.Dia_Permiso7 = Usuarios.Dia_Permiso[7]~
  rowObject.Hfinman1 = Usuarios.Hfinman[1]~
  rowObject.Hfinman2 = Usuarios.Hfinman[2]~
  rowObject.Hfinman3 = Usuarios.Hfinman[3]~
  rowObject.Hfinman4 = Usuarios.Hfinman[4]~
  rowObject.Hfinman5 = Usuarios.Hfinman[5]~
  rowObject.Hfinman6 = Usuarios.Hfinman[6]~
  rowObject.Hfinman7 = Usuarios.Hfinman[7]~
  rowObject.Hfintar1 = Usuarios.Hfintar[1]~
  rowObject.Hfintar2 = Usuarios.Hfintar[2]~
  rowObject.Hfintar3 = Usuarios.Hfintar[3]~
  rowObject.Hfintar4 = Usuarios.Hfintar[4]~
  rowObject.Hfintar5 = Usuarios.Hfintar[5]~
  rowObject.Hfintar6 = Usuarios.Hfintar[6]~
  rowObject.Hfintar7 = Usuarios.Hfintar[7]~
  rowObject.HIniMan1 = Usuarios.HIniMan[1]~
  rowObject.HIniMan2 = Usuarios.HIniMan[2]~
  rowObject.HIniMan3 = Usuarios.HIniMan[3]~
  rowObject.HIniMan4 = Usuarios.HIniMan[4]~
  rowObject.HIniMan5 = Usuarios.HIniMan[5]~
  rowObject.HIniMan6 = Usuarios.HIniMan[6]~
  rowObject.HIniMan7 = Usuarios.HIniMan[7]~
  rowObject.Hinitar1 = Usuarios.Hinitar[1]~
  rowObject.Hinitar2 = Usuarios.Hinitar[2]~
  rowObject.Hinitar3 = Usuarios.Hinitar[3]~
  rowObject.Hinitar4 = Usuarios.Hinitar[4]~
  rowObject.Hinitar5 = Usuarios.Hinitar[5]~
  rowObject.Hinitar6 = Usuarios.Hinitar[6]~
  rowObject.Hinitar7 = Usuarios.Hinitar[7]
&Scoped-Define DATA-FIELD-DEFS "dusuarios.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Usuarios NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Usuarios NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Usuarios
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Usuarios


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Usuarios SCROLLING.
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
     _TblList          = "bdcentral.Usuarios"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Usuarios.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[2]   > bdcentral.Usuarios.Clave
"Clave" "Clave" ? ? "character" ? ? ? ? ? ? yes ? yes 10.57 yes ?
     _FldNameList[3]   > bdcentral.Usuarios.Dia_Permiso[1]
"Dia_Permiso[1]" "Dia_Permiso1" ? ? "logical" ? ? ? ? ? ? yes ? yes 14.14 yes ?
     _FldNameList[4]   > bdcentral.Usuarios.Dia_Permiso[2]
"Dia_Permiso[2]" "Dia_Permiso2" ? ? "logical" ? ? ? ? ? ? yes ? yes 14.14 yes ?
     _FldNameList[5]   > bdcentral.Usuarios.Dia_Permiso[3]
"Dia_Permiso[3]" "Dia_Permiso3" ? ? "logical" ? ? ? ? ? ? yes ? yes 14.14 yes ?
     _FldNameList[6]   > bdcentral.Usuarios.Dia_Permiso[4]
"Dia_Permiso[4]" "Dia_Permiso4" ? ? "logical" ? ? ? ? ? ? yes ? yes 14.14 yes ?
     _FldNameList[7]   > bdcentral.Usuarios.Dia_Permiso[5]
"Dia_Permiso[5]" "Dia_Permiso5" ? ? "logical" ? ? ? ? ? ? yes ? yes 14.14 yes ?
     _FldNameList[8]   > bdcentral.Usuarios.Dia_Permiso[6]
"Dia_Permiso[6]" "Dia_Permiso6" ? ? "logical" ? ? ? ? ? ? yes ? yes 14.14 yes ?
     _FldNameList[9]   > bdcentral.Usuarios.Dia_Permiso[7]
"Dia_Permiso[7]" "Dia_Permiso7" ? ? "logical" ? ? ? ? ? ? yes ? yes 14.14 yes ?
     _FldNameList[10]   > bdcentral.Usuarios.Estacion
"Estacion" "Estacion" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[11]   > bdcentral.Usuarios.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.43 yes ?
     _FldNameList[12]   > bdcentral.Usuarios.Fec_Creacion
"Fec_Creacion" "Fec_Creacion" ? ? "date" ? ? ? ? ? ? yes ? yes 17.72 yes ?
     _FldNameList[13]   > bdcentral.Usuarios.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[14]   > bdcentral.Usuarios.Fec_UltCam
"Fec_UltCam" "Fec_UltCam" ? ? "date" ? ? ? ? ? ? yes ? no 19.43 yes ?
     _FldNameList[15]   > bdcentral.Usuarios.Grupo
"Grupo" "Grupo" ? ? "integer" ? ? ? ? ? ? yes ? yes 5.57 yes ?
     _FldNameList[16]   > bdcentral.Usuarios.Hentrada
"Hentrada" "Hentrada" ? ? "integer" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[17]   > bdcentral.Usuarios.Hfinman[1]
"Hfinman[1]" "Hfinman1" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[18]   > bdcentral.Usuarios.Hfinman[2]
"Hfinman[2]" "Hfinman2" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[19]   > bdcentral.Usuarios.Hfinman[3]
"Hfinman[3]" "Hfinman3" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[20]   > bdcentral.Usuarios.Hfinman[4]
"Hfinman[4]" "Hfinman4" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[21]   > bdcentral.Usuarios.Hfinman[5]
"Hfinman[5]" "Hfinman5" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[22]   > bdcentral.Usuarios.Hfinman[6]
"Hfinman[6]" "Hfinman6" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[23]   > bdcentral.Usuarios.Hfinman[7]
"Hfinman[7]" "Hfinman7" ? ? "integer" ? ? ? ? ? ? yes ? no 15.29 yes ?
     _FldNameList[24]   > bdcentral.Usuarios.Hfintar[1]
"Hfintar[1]" "Hfintar1" ? ? "integer" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[25]   > bdcentral.Usuarios.Hfintar[2]
"Hfintar[2]" "Hfintar2" ? ? "integer" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[26]   > bdcentral.Usuarios.Hfintar[3]
"Hfintar[3]" "Hfintar3" ? ? "integer" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[27]   > bdcentral.Usuarios.Hfintar[4]
"Hfintar[4]" "Hfintar4" ? ? "integer" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[28]   > bdcentral.Usuarios.Hfintar[5]
"Hfintar[5]" "Hfintar5" ? ? "integer" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[29]   > bdcentral.Usuarios.Hfintar[6]
"Hfintar[6]" "Hfintar6" ? ? "integer" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[30]   > bdcentral.Usuarios.Hfintar[7]
"Hfintar[7]" "Hfintar7" ? ? "integer" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[31]   > bdcentral.Usuarios.HIniMan[1]
"HIniMan[1]" "HIniMan1" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.57 yes ?
     _FldNameList[32]   > bdcentral.Usuarios.HIniMan[2]
"HIniMan[2]" "HIniMan2" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.57 yes ?
     _FldNameList[33]   > bdcentral.Usuarios.HIniMan[3]
"HIniMan[3]" "HIniMan3" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.57 yes ?
     _FldNameList[34]   > bdcentral.Usuarios.HIniMan[4]
"HIniMan[4]" "HIniMan4" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.57 yes ?
     _FldNameList[35]   > bdcentral.Usuarios.HIniMan[5]
"HIniMan[5]" "HIniMan5" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.57 yes ?
     _FldNameList[36]   > bdcentral.Usuarios.HIniMan[6]
"HIniMan[6]" "HIniMan6" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.57 yes ?
     _FldNameList[37]   > bdcentral.Usuarios.HIniMan[7]
"HIniMan[7]" "HIniMan7" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.57 yes ?
     _FldNameList[38]   > bdcentral.Usuarios.Hinitar[1]
"Hinitar[1]" "Hinitar1" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[39]   > bdcentral.Usuarios.Hinitar[2]
"Hinitar[2]" "Hinitar2" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[40]   > bdcentral.Usuarios.Hinitar[3]
"Hinitar[3]" "Hinitar3" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[41]   > bdcentral.Usuarios.Hinitar[4]
"Hinitar[4]" "Hinitar4" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[42]   > bdcentral.Usuarios.Hinitar[5]
"Hinitar[5]" "Hinitar5" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[43]   > bdcentral.Usuarios.Hinitar[6]
"Hinitar[6]" "Hinitar6" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[44]   > bdcentral.Usuarios.Hinitar[7]
"Hinitar[7]" "Hinitar7" ? ? "integer" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[45]   > bdcentral.Usuarios.IdHorario
"IdHorario" "IdHorario" ? ? "logical" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[46]   > bdcentral.Usuarios.Id_AccSimultaneo
"Id_AccSimultaneo" "Id_AccSimultaneo" ? ? "logical" ? ? ? ? ? ? yes ? no 27.57 yes ?
     _FldNameList[47]   > bdcentral.Usuarios.Id_AllAgencias
"Id_AllAgencias" "Id_AllAgencias" ? ? "logical" ? ? ? ? ? ? yes ? no 32.29 yes ?
     _FldNameList[48]   > bdcentral.Usuarios.Id_Bloqueo
"Id_Bloqueo" "Id_Bloqueo" ? ? "logical" ? ? ? ? ? ? yes ? no 10.57 yes ?
     _FldNameList[49]   > bdcentral.Usuarios.id_cartera
"id_cartera" "id_cartera" ? ? "integer" ? ? ? ? ? ? yes ? no 9.29 yes ?
     _FldNameList[50]   > bdcentral.Usuarios.id_cobrojuridico
"id_cobrojuridico" "id_cobrojuridico" ? ? "integer" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[51]   > bdcentral.Usuarios.Id_Entrada
"Id_Entrada" "Id_Entrada" ? ? "logical" ? ? ? ? ? ? yes ? no 28.86 yes ?
     _FldNameList[52]   > bdcentral.Usuarios.Id_Estacion
"Id_Estacion" "Id_Estacion" ? ? "logical" ? ? ? ? ? ? yes ? no 43.72 yes ?
     _FldNameList[53]   > bdcentral.Usuarios.Id_OpeOfi
"Id_OpeOfi" "Id_OpeOfi" ? ? "logical" ? ? ? ? ? ? yes ? yes 21.14 yes ?
     _FldNameList[54]   > bdcentral.Usuarios.MaxVal_Efectivo
"MaxVal_Efectivo" "MaxVal_Efectivo" ? ? "decimal" ? ? ? ? ? ? yes ? no 19.43 yes ?
     _FldNameList[55]   > bdcentral.Usuarios.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[56]   > bdcentral.Usuarios.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? yes ? yes 40 yes ?
     _FldNameList[57]   > bdcentral.Usuarios.Pedir_Clave
"Pedir_Clave" "Pedir_Clave" ? ? "logical" ? ? ? ? ? ? yes ? no 11.29 yes ?
     _FldNameList[58]   > bdcentral.Usuarios.Prioridad
"Prioridad" "Prioridad" ? ? "integer" ? ? ? ? ? ? yes ? yes 12 yes ?
     _FldNameList[59]   > bdcentral.Usuarios.proactual
"proactual" "proactual" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[60]   > bdcentral.Usuarios.Tie_Renovacion
"Tie_Renovacion" "Tie_Renovacion" ? ? "integer" ? ? ? ? ? ? yes ? no 26.86 yes ?
     _FldNameList[61]   > bdcentral.Usuarios.Tip_Menu
"Tip_Menu" "Tip_Menu" ? ? "integer" ? ? ? ? ? ? yes ? no 12.57 yes ?
     _FldNameList[62]   > bdcentral.Usuarios.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? yes 10.43 yes ?
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

