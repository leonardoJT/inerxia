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
&Scoped-define INTERNAL-TABLES Pro_Sarlaft

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Cod_Factor Nombre Cod_UbFactor Cod_EveFactor Clase_ROperativo Fec_Creacion~
 Estado Fec_Retiro Actividad Cargo_Resp Cod_Controles Cod_Probabilidad~
 Cod_Severidad Descripcion_Con Des_Controles Proceso Responsable_Cont~
 Subproceso perfil
&Scoped-define ENABLED-FIELDS-IN-Pro_Sarlaft Cod_Factor Nombre Cod_UbFactor ~
Cod_EveFactor Clase_ROperativo Fec_Creacion Estado Fec_Retiro Actividad ~
Cargo_Resp Cod_Controles Cod_Probabilidad Cod_Severidad Descripcion_Con ~
Des_Controles Proceso Responsable_Cont Subproceso perfil 
&Scoped-Define DATA-FIELDS  Cod_Factor Nombre Cod_UbFactor Cod_EveFactor Clase_ROperativo Fec_Creacion~
 Estado Fec_Retiro Actividad Cargo_Resp Cod_Controles Cod_Probabilidad~
 Cod_Severidad Descripcion_Con Des_Controles Proceso Responsable_Cont~
 Subproceso perfil
&Scoped-define DATA-FIELDS-IN-Pro_Sarlaft Cod_Factor Nombre Cod_UbFactor ~
Cod_EveFactor Clase_ROperativo Fec_Creacion Estado Fec_Retiro Actividad ~
Cargo_Resp Cod_Controles Cod_Probabilidad Cod_Severidad Descripcion_Con ~
Des_Controles Proceso Responsable_Cont Subproceso perfil 
&Scoped-Define MANDATORY-FIELDS  Cod_Factor Nombre Fec_Creacion Estado Actividad Cargo_Resp Cod_Controles~
 Cod_Probabilidad Cod_Severidad Descripcion_Con Des_Controles Proceso~
 Responsable_Cont Subproceso
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dPSarlaft.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Pro_Sarlaft NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Pro_Sarlaft NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Pro_Sarlaft
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Pro_Sarlaft


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fReplicaEnSaro dTables  _DB-REQUIRED
FUNCTION fReplicaEnSaro RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Pro_Sarlaft SCROLLING.
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
         WIDTH              = 32.43.
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
     _TblList          = "bdcentral.Pro_Sarlaft"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Pro_Sarlaft.Cod_Factor
"Cod_Factor" "Cod_Factor" ? ? "integer" ? ? ? ? ? ? yes ? yes 19.86 yes "Factor Riesgo"
     _FldNameList[2]   > bdcentral.Pro_Sarlaft.Nombre
"Nombre" "Nombre" ? ? "character" ? ? ? ? ? ? yes ? yes 40 yes ?
     _FldNameList[3]   > bdcentral.Pro_Sarlaft.Cod_UbFactor
"Cod_UbFactor" "Cod_UbFactor" ? ? "integer" ? ? ? ? ? ? yes ? no 22.43 yes ?
     _FldNameList[4]   > bdcentral.Pro_Sarlaft.Cod_EveFactor
"Cod_EveFactor" "Cod_EveFactor" ? ? "integer" ? ? ? ? ? ? yes ? no 19.86 yes ?
     _FldNameList[5]   > bdcentral.Pro_Sarlaft.Clase_ROperativo
"Clase_ROperativo" "Clase_ROperativo" ? ? "integer" ? ? ? ? ? ? yes ? no 22 yes ?
     _FldNameList[6]   > bdcentral.Pro_Sarlaft.Fec_Creacion
"Fec_Creacion" "Fec_Creacion" ? ? "date" ? ? ? ? ? ? yes ? yes 17.14 yes ?
     _FldNameList[7]   > bdcentral.Pro_Sarlaft.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.43 yes ?
     _FldNameList[8]   > bdcentral.Pro_Sarlaft.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[9]   > bdcentral.Pro_Sarlaft.Actividad
"Actividad" "Actividad" ? ? "character" ? ? ? ? ? ? yes ? yes 120 yes ?
     _FldNameList[10]   > bdcentral.Pro_Sarlaft.Cargo_Resp
"Cargo_Resp" "Cargo_Resp" ? ? "character" ? ? ? ? ? ? yes ? yes 120 yes ?
     _FldNameList[11]   > bdcentral.Pro_Sarlaft.Cod_Controles
"Cod_Controles" "Cod_Controles" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.86 yes ?
     _FldNameList[12]   > bdcentral.Pro_Sarlaft.Cod_Probabilidad
"Cod_Probabilidad" "Cod_Probabilidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.72 yes ?
     _FldNameList[13]   > bdcentral.Pro_Sarlaft.Cod_Severidad
"Cod_Severidad" "Cod_Severidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 16.43 yes ?
     _FldNameList[14]   > bdcentral.Pro_Sarlaft.Descripcion_Con
"Descripcion_Con" "Descripcion_Con" ? ? "character" ? ? ? ? ? ? yes ? yes 120 yes ?
     _FldNameList[15]   > bdcentral.Pro_Sarlaft.Des_Controles
"Des_Controles" "Des_Controles" ? ? "character" ? ? ? ? ? ? yes ? yes 120 yes ?
     _FldNameList[16]   > bdcentral.Pro_Sarlaft.Proceso
"Proceso" "Proceso" ? ? "character" ? ? ? ? ? ? yes ? yes 80 yes ?
     _FldNameList[17]   > bdcentral.Pro_Sarlaft.Responsable_Cont
"Responsable_Cont" "Responsable_Cont" ? ? "character" ? ? ? ? ? ? yes ? yes 120 yes ?
     _FldNameList[18]   > bdcentral.Pro_Sarlaft.Subproceso
"Subproceso" "Subproceso" ? ? "character" ? ? ? ? ? ? yes ? yes 80 yes ?
     _FldNameList[19]   > bdcentral.Pro_Sarlaft.perfil
"perfil" "perfil" ? ? "integer" ? ? ? ? ? ? yes ? no 4.29 yes ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beginTransactionValidate dTables  _DB-REQUIRED
PROCEDURE beginTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  /*Creacion*/
  FOR EACH rowobjupd WHERE rowobjupd.rowmod = "A" :
       ASSIGN  rowobjupd.Cod_Factor       = NEXT-VALUE(Sec_ProSarlaft)
               rowobjupd.Fec_Creacion     = TODAY.
       fReplicaEnSaro().
  END.

  FOR EACH rowobjupd WHERE rowobjupd.rowmod = "U":
      IF rowobjupd.estado = 2 THEN
         rowobjupd.Fec_Retiro = TODAY.
      /* fReplicaEnSaro(). La Actualización No Se Debe Replicar....NELSON */
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable dTables  _DB-REQUIRED
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).

  /* Code placed here will execute AFTER standard behavior.    */
    IF NOT pcRelative = "SAME"
    THEN PUBLISH "ProSarlaft" (rowobject.cod_factor).

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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fReplicaEnSaro dTables  _DB-REQUIRED
FUNCTION fReplicaEnSaro RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*     
       ASSIGN  rowobjupd.Cod_Factor       = NEXT-VALUE(Sec_ProSaro) */
    IF NOT CAN-FIND(FIRST pro_saro 
        WHERE 
            Pro_Saro.Cod_Factor_Sarlaft = rowobjupd.cod_factor)
    THEN DO:
        CREATE pro_saro.
        pro_saro.cod_factor = NEXT-VALUE(Sec_ProSaro). 
        pro_saro.cod_factor_sarlaft = rowobjupd.cod_factor.
    END.
    ELSE DO:
        FIND FIRST pro_saro EXCLUSIVE-LOCK
            WHERE
                Pro_Saro.Cod_Factor_Sarlaft = rowobjupd.cod_factor NO-WAIT NO-ERROR.
        IF LOCKED(pro_saro)
        THEN RETURN "ERROR: Registro En Uso Por Otro Usuario." + CHR(10) + 
            "Transacción Abortada".
    END.
    ASSIGN
        Pro_Saro.Actividad           = rowobjupd.Actividad       
        Pro_Saro.Cargo_Resp          = ROWOBJUPD.Cargo_Resp      
        Pro_Saro.Clase_ROperativo    = ROWOBJUPD.Clase_ROperativo
        Pro_Saro.Cod_Controles       = ROWOBJUPD.Cod_Controles   
        Pro_Saro.Cod_EveFactor       = ROWOBJUPD.Cod_EveFactor   
        Pro_Saro.Cod_Probabilidad    = ROWOBJUPD.Cod_Probabilidad
        Pro_Saro.Cod_Severidad       = ROWOBJUPD.Cod_Severidad   
        Pro_Saro.Cod_UbFactor        = ROWOBJUPD.Cod_UbFactor    
        Pro_Saro.Descripcion_Con     = ROWOBJUPD.Descripcion_Con 
        Pro_Saro.Des_Controles       = ROWOBJUPD.Des_Controles   
        Pro_Saro.Des_proceso         = ""
        Pro_Saro.Estado              = 1 
        Pro_Saro.Fec_Creacion        = ROWOBJUPD.Fec_Creacion    
        Pro_Saro.Fec_Retiro          = ROWOBJUPD.Fec_Retiro      
        Pro_Saro.Nombre              = ROWOBJUPD.Nombre          
        Pro_Saro.Proceso             = ROWOBJUPD.Proceso         
        Pro_Saro.Responsable_Cont    = ROWOBJUPD.Responsable_Cont
        Pro_Saro.Subproceso          = ROWOBJUPD.Subproceso
        Pro_Saro.Tipo                = 2
        Pro_Saro.Tipo_proceso        = 0.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

