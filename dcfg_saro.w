&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T1Cfg_Saro NO-UNDO LIKE Cfg_Saro.
DEFINE TEMP-TABLE T2Cfg_Saro NO-UNDO LIKE Cfg_Saro.



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
DEF VAR v-Cod_Tipo AS INT.

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
&Scoped-define INTERNAL-TABLES Cfg_Saro

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Cfg_Cod Cod_Analisis Cod_Nivel Cod_Probabilidad Cod_Severidad Cta_Cr Cta_Db~
 Descripcion Estado Fec_Creacion Fec_Retiro Nombre Tipo Util_Neta Val_Final~
 Val_Inicial Por_dos Por_uno
&Scoped-define ENABLED-FIELDS-IN-Cfg_Saro Cfg_Cod Cod_Analisis Cod_Nivel ~
Cod_Probabilidad Cod_Severidad Cta_Cr Cta_Db Descripcion Estado ~
Fec_Creacion Fec_Retiro Nombre Tipo Util_Neta Val_Final Val_Inicial Por_dos ~
Por_uno 
&Scoped-Define DATA-FIELDS  Cfg_Cod Cod_Analisis Cod_Nivel Cod_Probabilidad Cod_Severidad Cta_Cr Cta_Db~
 Descripcion Estado Fec_Creacion Fec_Retiro Nombre Tipo Util_Neta Val_Final~
 Val_Inicial Por_dos Por_uno Val_Detallada Val_DetalladaP Cla_Imp FSeveridad~
 FProbabilidad Cfg_Nombre
&Scoped-define DATA-FIELDS-IN-Cfg_Saro Cfg_Cod Cod_Analisis Cod_Nivel ~
Cod_Probabilidad Cod_Severidad Cta_Cr Cta_Db Descripcion Estado ~
Fec_Creacion Fec_Retiro Nombre Tipo Util_Neta Val_Final Val_Inicial Por_dos ~
Por_uno 
&Scoped-Define MANDATORY-FIELDS  Cfg_Cod Cod_Probabilidad Cod_Severidad Estado Fec_Creacion Tipo
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dcfg_saro.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Cfg_Saro NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Cfg_Saro NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Cfg_Saro
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Cfg_Saro


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProbabilidad dTables  _DB-REQUIRED
FUNCTION getProbabilidad RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSeveridad dTables  _DB-REQUIRED
FUNCTION getSeveridad RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Cfg_Saro SCROLLING.
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
      TABLE: T1Cfg_Saro T "?" NO-UNDO bdcentral Cfg_Saro
      TABLE: T2Cfg_Saro T "?" NO-UNDO bdcentral Cfg_Saro
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
         WIDTH              = 31.57.
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
     _TblList          = "bdcentral.Cfg_Saro"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Cfg_Saro.Cfg_Cod
"Cfg_Cod" "Cfg_Cod" ? ? "integer" ? ? ? ? ? ? yes ? yes 19.72 yes ?
     _FldNameList[2]   > bdcentral.Cfg_Saro.Cod_Analisis
"Cod_Analisis" "Cod_Analisis" ? ? "character" ? ? ? ? ? ? yes ? no 39.14 yes ?
     _FldNameList[3]   > bdcentral.Cfg_Saro.Cod_Nivel
"Cod_Nivel" "Cod_Nivel" ? ">>9" "integer" ? ? ? ? ? ? yes ? no 4.86 yes ?
     _FldNameList[4]   > bdcentral.Cfg_Saro.Cod_Probabilidad
"Cod_Probabilidad" "Cod_Probabilidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.72 yes ?
     _FldNameList[5]   > bdcentral.Cfg_Saro.Cod_Severidad
"Cod_Severidad" "Cod_Severidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 16.43 yes ?
     _FldNameList[6]   > bdcentral.Cfg_Saro.Cta_Cr
"Cta_Cr" "Cta_Cr" ? ? "character" ? ? ? ? ? ? yes ? no 22 yes ?
     _FldNameList[7]   > bdcentral.Cfg_Saro.Cta_Db
"Cta_Db" "Cta_Db" ? ? "character" ? ? ? ? ? ? yes ? no 22 yes ?
     _FldNameList[8]   > bdcentral.Cfg_Saro.Descripcion
"Descripcion" "Descripcion" "Descripción Detallada" "X(50)" "character" ? ? ? ? ? ? yes ? no 50 yes "Descripción Detallada"
     _FldNameList[9]   > bdcentral.Cfg_Saro.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.43 yes ?
     _FldNameList[10]   > bdcentral.Cfg_Saro.Fec_Creacion
"Fec_Creacion" "Fec_Creacion" ? ? "date" ? ? ? ? ? ? yes ? yes 17.14 yes ?
     _FldNameList[11]   > bdcentral.Cfg_Saro.Fec_Retiro
"Fec_Retiro" "Fec_Retiro" ? ? "date" ? ? ? ? ? ? yes ? no 14.57 yes ?
     _FldNameList[12]   > bdcentral.Cfg_Saro.Nombre
"Nombre" "Nombre" ? "X(20)" "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[13]   > bdcentral.Cfg_Saro.Tipo
"Tipo" "Tipo" ? ? "integer" ? ? ? ? ? ? yes ? yes 4 yes ?
     _FldNameList[14]   > bdcentral.Cfg_Saro.Util_Neta
"Util_Neta" "Util_Neta" ? ">>9,99" "decimal" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[15]   > bdcentral.Cfg_Saro.Val_Final
"Val_Final" "Val_Final" ? ">>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no 19.14 yes ?
     _FldNameList[16]   > bdcentral.Cfg_Saro.Val_Inicial
"Val_Inicial" "Val_Inicial" ? ">>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no 19.14 yes ?
     _FldNameList[17]   > bdcentral.Cfg_Saro.Por_dos
"Por_dos" "Por_dos" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[18]   > bdcentral.Cfg_Saro.Por_uno
"Por_uno" "Por_uno" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[19]   > "_<CALC>"
"""Entre ""  + STRING (RowObject.Val_Inicial,'>>>,>>>,>>9.99') + "" y "" +  STRING (RowObject.Val_Final,'>>>,>>>,>>9.99') + "" Millones""" "Val_Detallada" "Valoración Detallada" "x(47)" "character" ? ? ? ? ? ? no ? no 47 no "Valoración Detallada"
     _FldNameList[20]   > "_<CALC>"
"""De ""  +  SUBSTRING (STRING (RowObject.Val_Inicial,'>>>,>>>,>>9.99'),1,11) + "" A "" +  SUBSTRING (STRING (RowObject.Val_Final,'>>>,>>>,>>9.99'),1,11) + "" Veces al Año""" "Val_DetalladaP" "Valoración Detallada" "x(45)" "character" ? ? ? ? ? ? no ? no 45 no "Valoración Detallada"
     _FldNameList[21]   > "_<CALC>"
"RowObject.Cod_Analisis + ""  "" + RowObject.Nombre" "Cla_Imp" "Clasificación Impacto del Riesgo Operativo" "x(60)" "character" ? ? ? ? ? ? no ? no 60 no "Clasificación Impacto del Riesgo Operativo"
     _FldNameList[22]   > "_<CALC>"
"getSeveridad(INPUT RowObject.Cod_Severidad)" "FSeveridad" "Severidad" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no "Severidad"
     _FldNameList[23]   > "_<CALC>"
"getProbabilidad(INPUT RowObject.Cod_Probabilidad)" "FProbabilidad" "Probabilidad" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no "Probabilidad"
     _FldNameList[24]   > "_<CALC>"
"IF (RowObject.Tipo = 1) THEN ('Severidad') 
 ELSE ( IF (RowObject.Tipo = 2) THEN ('Probabilidad') 
             ELSE ( IF (RowObject.Tipo = 3) THEN ('Analisis') 
                         ELSE ('No-Existe')))" "Cfg_Nombre" "Nombre" "x(12)" "character" ? ? ? ? ? ? no ? no 12 no "Nombre"
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

  /* Code placed here will execute AFTER standard behavior.    */

  FOR EACH rowobjupd WHERE rowobjupd.rowmod = "A" :
     ASSIGN rowobjupd.Cfg_Cod      = NEXT-VALUE(Sec_cfgsaro)
            rowobjupd.Tipo         = v-Cod_Tipo
            rowobjupd.Fec_Creacion = TODAY.

  END.

   FOR EACH rowobjupd WHERE rowobjupd.rowmod = "U":
      IF rowobjupd.estado = 2 THEN
         rowobjupd.Fec_Retiro = TODAY.


   END.


END PROCEDURE.


/* Cod_Analisis 
   Cod_Nivel 
   Cod_Probabilidad 
   Cod_Severidad 
   Fec_Retiro 
   */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.Cfg_Nombre = (IF (RowObject.Tipo = 1) THEN ('Severidad') 
 ELSE ( IF (RowObject.Tipo = 2) THEN ('Probabilidad') 
             ELSE ( IF (RowObject.Tipo = 3) THEN ('Analisis') 
                         ELSE ('No-Existe'))))
         rowObject.Cla_Imp = (RowObject.Cod_Analisis + "  " + RowObject.Nombre)
         rowObject.FProbabilidad = (getProbabilidad(INPUT RowObject.Cod_Probabilidad))
         rowObject.FSeveridad = (getSeveridad(INPUT RowObject.Cod_Severidad))
         rowObject.Val_Detallada = ("Entre "  + STRING (RowObject.Val_Inicial,'>>>,>>>,>>9.99') + " y " +  STRING (RowObject.Val_Final,'>>>,>>>,>>9.99') + " Millones")
         rowObject.Val_DetalladaP = ("De "  +  SUBSTRING (STRING (RowObject.Val_Inicial,'>>>,>>>,>>9.99'),1,11) + " A " +  SUBSTRING (STRING (RowObject.Val_Final,'>>>,>>>,>>9.99'),1,11) + " Veces al Año")
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tipo_Codigo dTables  _DB-REQUIRED
PROCEDURE Tipo_Codigo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER T_Cod AS INT.

    ASSIGN  v-Cod_Tipo = T_Cod.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProbabilidad dTables  _DB-REQUIRED
FUNCTION getProbabilidad RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE vcDescr AS CHARACTER   NO-UNDO.

    FIND FIRST Cfg_Saro WHERE Cfg_Saro.tipo = 2 
                          AND Cfg_Saro.Cod_Nivel = piCodigo NO-LOCK NO-ERROR.
    IF AVAILABLE Cfg_Saro THEN 
        ASSIGN vcDescr = STRING(Cfg_Saro.Cod_Nivel,'9') + " " + Cfg_Saro.nombre.
    ELSE
        ASSIGN vcDescr = "N/A".

  RETURN vcDescr.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSeveridad dTables  _DB-REQUIRED
FUNCTION getSeveridad RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcDescr AS CHARACTER   NO-UNDO.

    FIND FIRST Cfg_Saro WHERE Cfg_Saro.tipo = 1 
                          AND Cfg_Saro.Cod_Nivel = piCodigo NO-LOCK NO-ERROR.
    IF AVAILABLE Cfg_Saro THEN 
        ASSIGN vcDescr = STRING(Cfg_Saro.Cod_Nivel,'9') + " " + Cfg_Saro.nombre.
    ELSE
        ASSIGN vcDescr = "N/A".

  RETURN vcDescr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

