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
&Scoped-Define ENABLED-FIELDS RowObject.Nit RowObject.Dir_Residencia ~
RowObject.Tel_Residencia RowObject.Dir_comercial RowObject.Tel_comercial ~
RowObject.Email RowObject.Fec_UltActualiza RowObject.Fec_Retiro 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS RowObject.Nit RowObject.FCliente ~
RowObject.Dir_Residencia RowObject.FLugResidencia RowObject.Tel_Residencia ~
RowObject.FLugNacimiento RowObject.Dir_comercial RowObject.FLugComercial ~
RowObject.Tel_comercial RowObject.FEnvCorresp RowObject.Email ~
RowObject.Fec_UltActualiza RowObject.Fec_Retiro 
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
     SIZE 118 BY 1.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 7.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Nit AT ROW 1.27 COL 27 COLON-ALIGNED HELP
          "Número documento de identificación" WIDGET-ID 18
          LABEL "Cliente" FORMAT "X(12)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 18 FGCOLOR 15 
     RowObject.FCliente AT ROW 1.27 COL 46 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 38 FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Dir_Residencia AT ROW 3.69 COL 17 COLON-ALIGNED HELP
          "Dirección de residencia del cliente" WIDGET-ID 58
          LABEL "Dir. Residencia" FORMAT "X(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 41.43 BY 1
     RowObject.FLugResidencia AT ROW 3.69 COL 59 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 78 FORMAT "x(50)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 51.43 BY 1
     RowObject.Tel_Residencia AT ROW 4.77 COL 17 COLON-ALIGNED HELP
          "Teléfono de la residencia del cliente" WIDGET-ID 74
          LABEL "Tel. Residencia" FORMAT "X(20)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 21.43 BY 1
     RowObject.FLugNacimiento AT ROW 4.77 COL 59 COLON-ALIGNED HELP
          "" WIDGET-ID 82
          LABEL "Lug. Nacimiento" FORMAT "x(50)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 51.43 BY 1
     RowObject.Dir_comercial AT ROW 5.85 COL 17 COLON-ALIGNED HELP
          "Dirección comercial donde labora el cliente" WIDGET-ID 52
          LABEL "Dir. Comercial" FORMAT "X(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 41.43 BY 1
     RowObject.FLugComercial AT ROW 5.85 COL 59 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 76 FORMAT "x(50)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 51.43 BY 1
     RowObject.Tel_comercial AT ROW 6.92 COL 17 COLON-ALIGNED HELP
          "Teléfono del lugar comercial del cliente" WIDGET-ID 72
          LABEL "Tel Comercial" FORMAT "X(20)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 21.43 BY 1
     RowObject.FEnvCorresp AT ROW 8 COL 17 COLON-ALIGNED HELP
          "" WIDGET-ID 80
          LABEL "Envio Corresp." FORMAT "x(12)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 16.43 BY 1
     RowObject.Email AT ROW 8 COL 59 COLON-ALIGNED HELP
          "Correo electrónico del cliente" WIDGET-ID 60
          LABEL "Correo Electronico" FORMAT "X(50)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 51.43 BY 1
     RowObject.Fec_UltActualiza AT ROW 9.08 COL 17 COLON-ALIGNED HELP
          "Fecha de actualización de datos" WIDGET-ID 64
          LABEL "Ult. Actualiz." FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY 1
     RowObject.Fec_Retiro AT ROW 9.08 COL 59 COLON-ALIGNED HELP
          "Fecha de retiro del cliente de la Organización" WIDGET-ID 62
          LABEL "Fec. Retiro" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY 1
     "UBICACION" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 2.65 COL 47 WIDGET-ID 86
          FGCOLOR 7 
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 50
     RECT-2 AT ROW 2.62 COL 1 WIDGET-ID 84
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17  WIDGET-ID 100.


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
         HEIGHT             = 9.42
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

/* SETTINGS FOR FILL-IN RowObject.Dir_comercial IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Dir_Residencia IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Email IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.FCliente IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Fec_Retiro IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_UltActualiza IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.FEnvCorresp IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FLugComercial IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FLugNacimiento IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FLugResidencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Nit IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Tel_comercial IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Tel_Residencia IN FRAME F-Main
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

