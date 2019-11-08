&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dclientes.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

{src/adm2/widgetprto.i}

{incluido\iGetSdo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dclientes.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Nit RowObject.Est_Civil ~
RowObject.Niv_Educativo RowObject.Num_Hijos RowObject.Gran_Contribuyente ~
RowObject.Per_Acargo RowObject.Id_Retencion 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS RowObject.Nit RowObject.FCliente ~
RowObject.FEmpresa RowObject.FAgencia RowObject.FProfesion ~
RowObject.Est_Civil RowObject.FCargo RowObject.Niv_Educativo ~
RowObject.FTpVinculo RowObject.Num_Hijos RowObject.Gran_Contribuyente ~
RowObject.FTpContrato RowObject.Per_Acargo RowObject.Id_Retencion 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 8.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Nit AT ROW 1.27 COL 14 COLON-ALIGNED HELP
          "Número documento de identificación" WIDGET-ID 18
          LABEL "Nit" FORMAT "X(12)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 19 BY 1
     RowObject.FCliente AT ROW 2.35 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 38
          LABEL "Nombre" FORMAT "x(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 41 BY 1
     RowObject.FEmpresa AT ROW 2.35 COL 75 COLON-ALIGNED HELP
          "" WIDGET-ID 42
          LABEL "Empresa" FORMAT "x(30)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 40 BY 1
     RowObject.FAgencia AT ROW 3.42 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 12
          LABEL "Agencia" FORMAT "x(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 41 BY 1
     RowObject.FProfesion AT ROW 3.42 COL 75 COLON-ALIGNED HELP
          "" WIDGET-ID 44
          LABEL "Profesión" FORMAT "x(20)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 40 BY 1
     RowObject.Est_Civil AT ROW 4.5 COL 6.57 HELP
          "Seleccione de la lista el estado civil del cliente" WIDGET-ID 10
          LABEL "Est. Civil" FORMAT "X(15)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 19 BY 1
     RowObject.FCargo AT ROW 4.5 COL 75 COLON-ALIGNED HELP
          "" WIDGET-ID 40
          LABEL "Cargo" FORMAT "x(20)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 40 BY 1
     RowObject.Niv_Educativo AT ROW 5.58 COL 5.28 HELP
          "Seleccione el nivel educativo del cliente" WIDGET-ID 20
          LABEL "Nivel Edu." FORMAT "X(14)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 19 BY 1
     RowObject.FTpVinculo AT ROW 5.58 COL 75 COLON-ALIGNED HELP
          "" WIDGET-ID 48
          LABEL "Tp. Vínculo" FORMAT "x(15)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 31 BY 1
     RowObject.Num_Hijos AT ROW 6.65 COL 14 COLON-ALIGNED HELP
          "Número de hijos del cliente" WIDGET-ID 22
          LABEL "# Hijos" FORMAT "99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 3.72 BY 1
     RowObject.Gran_Contribuyente AT ROW 6.65 COL 31 HELP
          "Especifique si el cliente es o no es un gran contribuyente" WIDGET-ID 14
          LABEL "Gran Contribuyente"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY 1
     RowObject.FTpContrato AT ROW 6.65 COL 75 COLON-ALIGNED HELP
          "" WIDGET-ID 46
          LABEL "Tp. Contrato" FORMAT "x(15)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 31 BY 1
     RowObject.Per_Acargo AT ROW 7.73 COL 14 COLON-ALIGNED HELP
          "Número de personas a cargo del cliente" WIDGET-ID 24
          LABEL "Pers. a Cargo" FORMAT "99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 3.72 BY 1
     RowObject.Id_Retencion AT ROW 7.73 COL 31 HELP
          "Marque el campo si es o no es sujeto de retención en la fuente" WIDGET-ID 16
          LABEL "Retención en la Fuente"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY 1
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dclientes.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dclientes.i}
      END-FIELDS.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 8.62
         WIDTH              = 118.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Est_Civil IN FRAME F-Main
   ALIGN-L EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN RowObject.FAgencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FCargo IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FCliente IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FEmpresa IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FProfesion IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FTpContrato IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FTpVinculo IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR TOGGLE-BOX RowObject.Gran_Contribuyente IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_Retencion IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.Nit IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Niv_Educativo IN FRAME F-Main
   ALIGN-L EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN RowObject.Num_Hijos IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Per_Acargo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

