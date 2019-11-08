&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          repositorio      PROGRESS
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

DEF VAR iExtnt AS INTEGER NO-UNDO.
DEF VAR cCmpoClcldo AS CHAR NO-UNDO.

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
&Scoped-define INTERNAL-TABLES conf_repositorio

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  BaseDestino BaseOrigen CampoDestino CampoOrigen Codigo Estado~
 FechaCambioEstado TablaDestino TablaOrigen TipoOrigen indice DesCampoCal~
 NomCampoCal ProcCampoCal
&Scoped-define ENABLED-FIELDS-IN-conf_repositorio BaseDestino BaseOrigen ~
CampoDestino CampoOrigen Codigo Estado FechaCambioEstado TablaDestino ~
TablaOrigen TipoOrigen indice DesCampoCal NomCampoCal ProcCampoCal 
&Scoped-Define DATA-FIELDS  BaseDestino BaseOrigen CampoDestino CampoOrigen Codigo Estado~
 FechaCambioEstado TablaDestino TablaOrigen TipoOrigen indice DesCampoCal~
 NomCampoCal ProcCampoCal
&Scoped-define DATA-FIELDS-IN-conf_repositorio BaseDestino BaseOrigen ~
CampoDestino CampoOrigen Codigo Estado FechaCambioEstado TablaDestino ~
TablaOrigen TipoOrigen indice DesCampoCal NomCampoCal ProcCampoCal 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dconf_repositorio.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH conf_repositorio NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH conf_repositorio NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main conf_repositorio
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main conf_repositorio


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fVldaNmbreCmpo dTables  _DB-REQUIRED
FUNCTION fVldaNmbreCmpo RETURNS CHARACTER
  (C AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      conf_repositorio SCROLLING.
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
     _TblList          = "repositorio.conf_repositorio"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > repositorio.conf_repositorio.BaseDestino
"BaseDestino" "BaseDestino" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[2]   > repositorio.conf_repositorio.BaseOrigen
"BaseOrigen" "BaseOrigen" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[3]   > repositorio.conf_repositorio.CampoDestino
"CampoDestino" "CampoDestino" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[4]   > repositorio.conf_repositorio.CampoOrigen
"CampoOrigen" "CampoOrigen" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[5]   > repositorio.conf_repositorio.Codigo
"Codigo" "Codigo" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[6]   > repositorio.conf_repositorio.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.43 yes ?
     _FldNameList[7]   > repositorio.conf_repositorio.FechaCambioEstado
"FechaCambioEstado" "FechaCambioEstado" ? ? "date" ? ? ? ? ? ? yes ? no 20.14 yes ?
     _FldNameList[8]   > repositorio.conf_repositorio.TablaDestino
"TablaDestino" "TablaDestino" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[9]   > repositorio.conf_repositorio.TablaOrigen
"TablaOrigen" "TablaOrigen" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[10]   > repositorio.conf_repositorio.TipoOrigen
"TipoOrigen" "TipoOrigen" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[11]   > repositorio.conf_repositorio.indice
"indice" "indice" ? ? "integer" ? ? ? ? ? ? yes ? no 5.57 yes ?
     _FldNameList[12]   > repositorio.conf_repositorio.DesCampoCal
"DesCampoCal" "DesCampoCal" ? "x(60)" "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[13]   > repositorio.conf_repositorio.NomCampoCal
"NomCampoCal" "NomCampoCal" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[14]   > repositorio.conf_repositorio.ProcCampoCal
"ProcCampoCal" "ProcCampoCal" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE begintransactionvalidate dTables  _DB-REQUIRED
PROCEDURE begintransactionvalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH rowobjupd
        WHERE
            rowobjupd.rowmod = "A":
        rowobjupd.codigo = NEXT-VALUE(ConRepoCodigo,repositorio).
        IF NOT rowobjupd.TablaOrigen = "CALCULADO"
        THEN DO:
            assign  rowobjupd.ProcCampoCal = ""
                    /* rowobjupd.DesCampoCal  = "" */
                    rowobjupd.NomCampoCal = ""
                    rowobjupd.FechaCambioEstado = ?.
            rowobjupd.DesCampoCal = UPPER(rowobjupd.DesCampoCal).
        END.
    END.
    FOR EACH rowobjupd
        WHERE
            rowobjupd.rowmod = "U":
        rowobjupd.FechaCambioEstado = TODAY.
        rowobjupd.DesCampoCal = UPPER(rowobjupd.DesCampoCal).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE campodestinovalidate dTables  _DB-REQUIRED
PROCEDURE campodestinovalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    IF c = ""
    THEN RETURN "Indique El Campo Destino".
    ELSE RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE campoorigenvalidate dTables  _DB-REQUIRED
PROCEDURE campoorigenvalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    IF c = ""
    THEN RETURN "Indique El Campo De Origen".
    ELSE RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CmpoClcldo dTables  _DB-REQUIRED
PROCEDURE CmpoClcldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    cCmpoClcldo = c.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descampocalvalidate dTables  _DB-REQUIRED
PROCEDURE descampocalvalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    /* IF NOT cCmpoClcldo = "S" THEN RETURN "". */
    IF TRIM(c) = ""
    THEN DO:
        RETURN "Indique La Descripción Del Campo.".
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE endtransactionvalidate dTables  _DB-REQUIRED
PROCEDURE endtransactionvalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH rowobjupd
        WHERE
            rowobjupd.rowmod = "A":
        FOR EACH repositorio._file NO-LOCK
            WHERE
                repositorio._file._file-name = rowobjupd.TablaDestino,
            EACH repositorio._field EXCLUSIVE-LOCK
                WHERE repositorio._field._field-name = rowobjupd.CampoDestino:
            repositorio._field._label = rowobjupd.Tablaorigen + "." + rowobjupd.Campoorigen.
        END.
    END.
    RELEASE repositorio._field.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE indicevalidate dTables  _DB-REQUIRED
PROCEDURE indicevalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    IF iextnt = 0  THEN RETURN "".
    IF INTEGER(c) < 1 OR INTEGER(c) > iextnt 
    THEN RETURN "Ingrese Un Número Entre 1 Y " + STRING(iextnt).
    ELSE RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
    SUBSCRIBE "pextent" ANYWHERE.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nomcampocalvalidate dTables  _DB-REQUIRED
PROCEDURE nomcampocalvalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DEF VAR cError AS CHAR NO-UNDO.

    IF NOT cCmpoClcldo = "S" THEN RETURN "".
    IF TRIM(c) = ""
    THEN DO:
        RETURN "Indique El Nombre Del Campo Calculado".
    END.
    cError = fVldaNmbreCmpo(c).
    IF NOT cError = ""
    THEN DO:
        RETURN cError.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pextent dTables  _DB-REQUIRED
PROCEDURE pextent :
/*------------------------------------------------------------------------------
  Purpose: establece el límite para la validación
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    iextnt = INTEGER(c).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proccampocalvalidate dTables  _DB-REQUIRED
PROCEDURE proccampocalvalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    IF NOT cCmpoClcldo = "S" THEN RETURN "".
    IF TRIM(c) = ""
    THEN DO:
        RETURN "Indique El Nombre Del Programa Para El Campo Calculado".
    END.
    IF SEARCH(c) = ?
    THEN DO:
        MESSAGE c " NO Encontrado." VIEW-AS ALERT-BOX WARNING TITLE "Precaución".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tablaorigenvalidate dTables  _DB-REQUIRED
PROCEDURE tablaorigenvalidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    IF c = ""
    THEN RETURN "Tabla Origen No Puede Ser Vacía".
    ELSE RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fVldaNmbreCmpo dTables  _DB-REQUIRED
FUNCTION fVldaNmbreCmpo RETURNS CHARACTER
  (C AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

