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
&Scoped-define INTERNAL-TABLES Mov_InsSipla

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia CodAutoriza Descripcion Estado Fecha_Gestion Fecha_Transaccion~
 Fec_RepROSS Fec_RepUIAF Hora_Gestion Hora_Transaccion Id_Exonerada Id_NUD~
 Id_NUM Id_RepROSS Id_RepUIAF Id_Sospechosa Id_Traslado Instancia~
 Instancia_Anterior Nit Tipo_Registro UsuCajero UsuGestiona UsuReporta~
 Valor_RegManual
&Scoped-define ENABLED-FIELDS-IN-Mov_InsSipla Agencia CodAutoriza ~
Descripcion Estado Fecha_Gestion Fecha_Transaccion Fec_RepROSS Fec_RepUIAF ~
Hora_Gestion Hora_Transaccion Id_Exonerada Id_NUD Id_NUM Id_RepROSS ~
Id_RepUIAF Id_Sospechosa Id_Traslado Instancia Instancia_Anterior Nit ~
Tipo_Registro UsuCajero UsuGestiona UsuReporta Valor_RegManual 
&Scoped-Define DATA-FIELDS  Agencia CodAutoriza Descripcion Estado Fecha_Gestion Fecha_Transaccion~
 Fec_RepROSS Fec_RepUIAF Hora_Gestion Hora_Transaccion Id_Exonerada Id_NUD~
 Id_NUM Id_RepROSS Id_RepUIAF Id_Sospechosa Id_Traslado Instancia~
 Instancia_Anterior Nit Tipo_Registro UsuCajero UsuGestiona UsuReporta~
 Valor_RegManual
&Scoped-define DATA-FIELDS-IN-Mov_InsSipla Agencia CodAutoriza Descripcion ~
Estado Fecha_Gestion Fecha_Transaccion Fec_RepROSS Fec_RepUIAF Hora_Gestion ~
Hora_Transaccion Id_Exonerada Id_NUD Id_NUM Id_RepROSS Id_RepUIAF ~
Id_Sospechosa Id_Traslado Instancia Instancia_Anterior Nit Tipo_Registro ~
UsuCajero UsuGestiona UsuReporta Valor_RegManual 
&Scoped-Define MANDATORY-FIELDS  UsuCajero UsuGestiona UsuReporta
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dmov_insSipla.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Mov_InsSipla NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Mov_InsSipla NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Mov_InsSipla
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Mov_InsSipla


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Mov_InsSipla SCROLLING.
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
     _TblList          = "bdcentral.Mov_InsSipla"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Mov_InsSipla.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[2]   > bdcentral.Mov_InsSipla.CodAutoriza
"CodAutoriza" "CodAutoriza" ? ? "integer" ? ? ? ? ? ? yes ? no 11.14 yes ?
     _FldNameList[3]   > bdcentral.Mov_InsSipla.Descripcion
"Descripcion" "Descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 500 yes ?
     _FldNameList[4]   > bdcentral.Mov_InsSipla.Estado
"Estado" "Estado" ? ? "logical" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[5]   > bdcentral.Mov_InsSipla.Fecha_Gestion
"Fecha_Gestion" "Fecha_Gestion" ? ? "date" ? ? ? ? ? ? yes ? no 13.29 yes ?
     _FldNameList[6]   > bdcentral.Mov_InsSipla.Fecha_Transaccion
"Fecha_Transaccion" "Fecha_Transaccion" ? ? "date" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[7]   > bdcentral.Mov_InsSipla.Fec_RepROSS
"Fec_RepROSS" "Fec_RepROSS" ? ? "date" ? ? ? ? ? ? yes ? no 13.57 yes ?
     _FldNameList[8]   > bdcentral.Mov_InsSipla.Fec_RepUIAF
"Fec_RepUIAF" "Fec_RepUIAF" ? ? "date" ? ? ? ? ? ? yes ? no 12.43 yes ?
     _FldNameList[9]   > bdcentral.Mov_InsSipla.Hora_Gestion
"Hora_Gestion" "Hora_Gestion" ? ? "integer" ? ? ? ? ? ? yes ? no 12.72 yes ?
     _FldNameList[10]   > bdcentral.Mov_InsSipla.Hora_Transaccion
"Hora_Transaccion" "Hora_Transaccion" ? ? "integer" ? ? ? ? ? ? yes ? no 16.86 yes ?
     _FldNameList[11]   > bdcentral.Mov_InsSipla.Id_Exonerada
"Id_Exonerada" "Id_Exonerada" ? ? "logical" ? ? ? ? ? ? yes ? no 21.86 yes ?
     _FldNameList[12]   > bdcentral.Mov_InsSipla.Id_NUD
"Id_NUD" "Id_NUD" ? ? "logical" ? ? ? ? ? ? yes ? no 12.29 yes ?
     _FldNameList[13]   > bdcentral.Mov_InsSipla.Id_NUM
"Id_NUM" "Id_NUM" ? ? "logical" ? ? ? ? ? ? yes ? no 13.14 yes ?
     _FldNameList[14]   > bdcentral.Mov_InsSipla.Id_RepROSS
"Id_RepROSS" "Id_RepROSS" ? ? "logical" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[15]   > bdcentral.Mov_InsSipla.Id_RepUIAF
"Id_RepUIAF" "Id_RepUIAF" ? ? "logical" ? ? ? ? ? ? yes ? no 17.57 yes ?
     _FldNameList[16]   > bdcentral.Mov_InsSipla.Id_Sospechosa
"Id_Sospechosa" "Id_Sospechosa" ? ? "logical" ? ? ? ? ? ? yes ? no 23.29 yes ?
     _FldNameList[17]   > bdcentral.Mov_InsSipla.Id_Traslado
"Id_Traslado" "Id_Traslado" ? ? "logical" ? ? ? ? ? ? yes ? no 11 yes ?
     _FldNameList[18]   > bdcentral.Mov_InsSipla.Instancia
"Instancia" "Instancia" ? ? "integer" ? ? ? ? ? ? yes ? no 8.43 yes ?
     _FldNameList[19]   > bdcentral.Mov_InsSipla.Instancia_Anterior
"Instancia_Anterior" "Instancia_Anterior" ? ? "integer" ? ? ? ? ? ? yes ? no 16.72 yes ?
     _FldNameList[20]   > bdcentral.Mov_InsSipla.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[21]   > bdcentral.Mov_InsSipla.Tipo_Registro
"Tipo_Registro" "Tipo_Registro" ? ? "character" ? ? ? ? ? ? yes ? no 13 yes ?
     _FldNameList[22]   > bdcentral.Mov_InsSipla.UsuCajero
"UsuCajero" "UsuCajero" ? ? "character" ? ? ? ? ? ? yes ? yes 6 yes ?
     _FldNameList[23]   > bdcentral.Mov_InsSipla.UsuGestiona
"UsuGestiona" "UsuGestiona" ? ? "character" ? ? ? ? ? ? yes ? yes 16.14 yes ?
     _FldNameList[24]   > bdcentral.Mov_InsSipla.UsuReporta
"UsuReporta" "UsuReporta" ? ? "character" ? ? ? ? ? ? yes ? yes 15.14 yes ?
     _FldNameList[25]   > bdcentral.Mov_InsSipla.Valor_RegManual
"Valor_RegManual" "Valor_RegManual" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.43 yes ?
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

