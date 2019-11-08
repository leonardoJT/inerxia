&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
&Scoped-define INTERNAL-TABLES Pro_Inversiones

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Categoria CtaRendimiento_CR CtaRendimiento_DB CtaValoracion_CR~
 CtaValoracion_DB Cuenta_Inversion_DB Indice Nom_Producto Tiempo_Valoracion~
 Cod_Producto Cpte_Egr Cpte_Ingr Cpte_Tras CtaProvision_Cr CtaProvision_DB~
 CtaRetFte_DB Porc_RetFte Id_TasaUnid
&Scoped-define ENABLED-FIELDS-IN-Pro_Inversiones Categoria ~
CtaRendimiento_CR CtaRendimiento_DB CtaValoracion_CR CtaValoracion_DB ~
Cuenta_Inversion_DB Indice Nom_Producto Tiempo_Valoracion Cod_Producto ~
Cpte_Egr Cpte_Ingr Cpte_Tras CtaProvision_Cr CtaProvision_DB CtaRetFte_DB ~
Porc_RetFte Id_TasaUnid 
&Scoped-Define DATA-FIELDS  Categoria CtaRendimiento_CR CtaRendimiento_DB CtaValoracion_CR~
 CtaValoracion_DB Cuenta_Inversion_DB Indice Nom_Producto Tiempo_Valoracion~
 Cod_Producto Cpte_Egr Cpte_Ingr Cpte_Tras CtaProvision_Cr CtaProvision_DB~
 CtaRetFte_DB Porc_RetFte Id_TasaUnid
&Scoped-define DATA-FIELDS-IN-Pro_Inversiones Categoria CtaRendimiento_CR ~
CtaRendimiento_DB CtaValoracion_CR CtaValoracion_DB Cuenta_Inversion_DB ~
Indice Nom_Producto Tiempo_Valoracion Cod_Producto Cpte_Egr Cpte_Ingr ~
Cpte_Tras CtaProvision_Cr CtaProvision_DB CtaRetFte_DB Porc_RetFte ~
Id_TasaUnid 
&Scoped-Define MANDATORY-FIELDS  Categoria Cod_Producto
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "D-Pro_Inversiones.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Pro_Inversiones NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Pro_Inversiones NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Pro_Inversiones
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Pro_Inversiones


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Pro_Inversiones SCROLLING.
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
         HEIGHT             = 1.58
         WIDTH              = 16.43.
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
     _TblList          = "bdcentral.Pro_Inversiones"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Pro_Inversiones.Categoria
"Categoria" "Categoria" ? ? "integer" ? ? ? ? ? ? yes ? yes 12.72 yes
     _FldNameList[2]   > bdcentral.Pro_Inversiones.CtaRendimiento_CR
"CtaRendimiento_CR" "CtaRendimiento_CR" ? ? "character" ? ? ? ? ? ? yes ? no 23 yes
     _FldNameList[3]   > bdcentral.Pro_Inversiones.CtaRendimiento_DB
"CtaRendimiento_DB" "CtaRendimiento_DB" ? ? "character" ? ? ? ? ? ? yes ? no 22.43 yes
     _FldNameList[4]   > bdcentral.Pro_Inversiones.CtaValoracion_CR
"CtaValoracion_CR" "CtaValoracion_CR" ? ? "character" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[5]   > bdcentral.Pro_Inversiones.CtaValoracion_DB
"CtaValoracion_DB" "CtaValoracion_DB" ? ? "character" ? ? ? ? ? ? yes ? no 20.43 yes
     _FldNameList[6]   > bdcentral.Pro_Inversiones.Cuenta_Inversion_DB
"Cuenta_Inversion_DB" "Cuenta_Inversion_DB" ? ? "character" ? ? ? ? ? ? yes ? no 18.57 yes
     _FldNameList[7]   > bdcentral.Pro_Inversiones.Indice
"Indice" "Indice" ? ? "integer" ? ? ? ? ? ? yes ? no 5.57 yes
     _FldNameList[8]   > bdcentral.Pro_Inversiones.Nom_Producto
"Nom_Producto" "Nom_Producto" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[9]   > bdcentral.Pro_Inversiones.Tiempo_Valoracion
"Tiempo_Valoracion" "Tiempo_Valoracion" ? ? "integer" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[10]   > bdcentral.Pro_Inversiones.Cod_Producto
"Cod_Producto" "Cod_Producto" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.14 yes
     _FldNameList[11]   > bdcentral.Pro_Inversiones.Cpte_Egr
"Cpte_Egr" "Cpte_Egr" ? ? "integer" ? ? ? ? ? ? yes ? no 20.57 yes
     _FldNameList[12]   > bdcentral.Pro_Inversiones.Cpte_Ingr
"Cpte_Ingr" "Cpte_Ingr" ? ? "integer" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[13]   > bdcentral.Pro_Inversiones.Cpte_Tras
"Cpte_Tras" "Cpte_Tras" ? ? "integer" ? ? ? ? ? ? yes ? no 22.14 yes
     _FldNameList[14]   > bdcentral.Pro_Inversiones.CtaProvision_Cr
"CtaProvision_Cr" "CtaProvision_Cr" ? ? "character" ? ? ? ? ? ? yes ? no 20.14 yes
     _FldNameList[15]   > bdcentral.Pro_Inversiones.CtaProvision_DB
"CtaProvision_DB" "CtaProvision_DB" ? ? "character" ? ? ? ? ? ? yes ? no 19.57 yes
     _FldNameList[16]   > bdcentral.Pro_Inversiones.CtaRetFte_DB
"CtaRetFte_DB" "CtaRetFte_DB" ? ? "character" ? ? ? ? ? ? yes ? no 20.72 yes
     _FldNameList[17]   > bdcentral.Pro_Inversiones.Porc_RetFte
"Porc_RetFte" "Porc_RetFte" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.29 yes
     _FldNameList[18]   > bdcentral.Pro_Inversiones.Id_TasaUnid
"Id_TasaUnid" "Id_TasaUnid" "Id.Tasa/Unidad" ? "integer" ? ? ? ? ? ? yes ? no 14.57 yes
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

