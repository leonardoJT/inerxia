&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dttmov_inssipla.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dttmov_inssipla.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Id_NUD RowObject.Id_NUM ~
RowObject.Id_Exonerada RowObject.Id_Sospechosa RowObject.UsuCajero ~
RowObject.CodAutoriza RowObject.Id_Traslado RowObject.Id_RepROSS ~
RowObject.Id_RepUIAF RowObject.Descripcion RowObject.Estado ~
RowObject.Fec_RepROSS RowObject.Valor_RegManual RowObject.Fec_RepUIAF ~
RowObject.Tipo_Registro 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS RowObject.FInstancia RowObject.FVNUD ~
RowObject.Id_NUD RowObject.FAgencia RowObject.FVNUM RowObject.Id_NUM ~
RowObject.Id_Exonerada RowObject.FUsu_reporta RowObject.Id_Sospechosa ~
RowObject.UsuCajero RowObject.CodAutoriza RowObject.Id_Traslado ~
RowObject.FUsu_Gestiona RowObject.Id_RepROSS RowObject.Id_RepUIAF ~
RowObject.FCliente RowObject.Descripcion RowObject.Estado ~
RowObject.Fec_RepROSS RowObject.Valor_RegManual RowObject.Fec_RepUIAF ~
RowObject.Tipo_Registro 
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
     SIZE 67 BY 10.19
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 10.23
     BGCOLOR 1 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.FInstancia AT ROW 1.27 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 60
          LABEL "Instancia" FORMAT "x(35)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 50 BY 1
     RowObject.FVNUD AT ROW 1.27 COL 97.29 COLON-ALIGNED HELP
          "" WIDGET-ID 56
          LABEL "Val. N.U.Día" FORMAT ">>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 17.43 BY 1
     RowObject.Id_NUD AT ROW 1.38 COL 69 WIDGET-ID 42
          LABEL "No Usual Dia"
          VIEW-AS TOGGLE-BOX
          SIZE 30 BY .77
     RowObject.FAgencia AT ROW 2.35 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 6
          LABEL "Agencia" FORMAT "x(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 50 BY 1
     RowObject.FVNUM AT ROW 2.35 COL 97.29 COLON-ALIGNED HELP
          "" WIDGET-ID 58
          LABEL "Val. N.U.Mes" FORMAT ">>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 17.43 BY 1
     RowObject.Id_NUM AT ROW 2.46 COL 69 WIDGET-ID 44
          LABEL "No Usual Mes"
          VIEW-AS TOGGLE-BOX
          SIZE 30 BY .77
     RowObject.Id_Exonerada AT ROW 3.15 COL 69 WIDGET-ID 40
          LABEL "Transaccion Exonerada"
          VIEW-AS TOGGLE-BOX
          SIZE 30 BY .77
     RowObject.FUsu_reporta AT ROW 3.42 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 30
          LABEL "Usu. Reporta" FORMAT "x(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 50 BY 1
     RowObject.Id_Sospechosa AT ROW 3.96 COL 69 WIDGET-ID 50
          LABEL "Transaccion Sospechosa"
          VIEW-AS TOGGLE-BOX
          SIZE 30 BY .77
     RowObject.UsuCajero AT ROW 4.5 COL 14 COLON-ALIGNED HELP
          "Cédula del usuario que se habilita para el ingreso al sistema" WIDGET-ID 12
          LABEL "Cajero" FORMAT "X(4)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 8.43 BY 1
     RowObject.CodAutoriza AT ROW 4.5 COL 57 COLON-ALIGNED HELP
          "" WIDGET-ID 32
          LABEL "Cod. Autorización" FORMAT "99999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 7.14 BY 1
     RowObject.Id_Traslado AT ROW 4.77 COL 69 WIDGET-ID 52
          LABEL "Id_Traslado"
          VIEW-AS TOGGLE-BOX
          SIZE 30 BY .77
     RowObject.FUsu_Gestiona AT ROW 5.58 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 28
          LABEL "Usu. Gestiona" FORMAT "x(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 50 BY 1
     RowObject.Id_RepROSS AT ROW 5.58 COL 69 HELP
          "" WIDGET-ID 46
          LABEL "Reportar ROS"
          VIEW-AS TOGGLE-BOX
          SIZE 21.14 BY .77
     RowObject.Id_RepUIAF AT ROW 6.38 COL 69 WIDGET-ID 48
          LABEL "Reportar a Fiscalia"
          VIEW-AS TOGGLE-BOX
          SIZE 21.14 BY .77
     RowObject.FCliente AT ROW 6.65 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 8
          LABEL "Cliente" FORMAT "x(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 50 BY 1
     RowObject.Descripcion AT ROW 7.46 COL 69.29 HELP
          "" NO-LABEL WIDGET-ID 24
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 47.43 BY 3.42
          BGCOLOR 15 
     RowObject.Estado AT ROW 7.73 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 4
          LABEL "Estado" FORMAT "Gestionada/No Gestionada"
          VIEW-AS FILL-IN NATIVE 
          SIZE 18 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.Fec_RepROSS AT ROW 7.73 COL 51 COLON-ALIGNED HELP
          "" WIDGET-ID 36
          LABEL "Fec. Rep. ROSS" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY 1
     RowObject.Valor_RegManual AT ROW 8.81 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 34
          LABEL "Val. Reg. Man." FORMAT ">>,>>>,>>>,>>9"
          VIEW-AS FILL-IN NATIVE 
          SIZE 18 BY 1
     RowObject.Fec_RepUIAF AT ROW 8.81 COL 51 COLON-ALIGNED HELP
          "" WIDGET-ID 38
          LABEL "Fec. Rep. UIAF" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY 1
     RowObject.Tipo_Registro AT ROW 9.88 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 54
          LABEL "Tp. Registro" FORMAT "X"
          VIEW-AS FILL-IN NATIVE 
          SIZE 18 BY 1
     RECT-1 AT ROW 1.04 COL 1 WIDGET-ID 22
     RECT-2 AT ROW 1 COL 68 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dttmov_inssipla.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dttmov_inssipla.i}
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
         HEIGHT             = 10.42
         WIDTH              = 117.29.
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

/* SETTINGS FOR FILL-IN RowObject.CodAutoriza IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR EDITOR RowObject.Descripcion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
ASSIGN 
       RowObject.Descripcion:AUTO-INDENT IN FRAME F-Main      = TRUE
       RowObject.Descripcion:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Estado IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.FAgencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FCliente IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Fec_RepROSS IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_RepUIAF IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.FInstancia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FUsu_Gestiona IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FUsu_reporta IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FVNUD IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FVNUM IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_Exonerada IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_NUD IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_NUM IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_RepROSS IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_RepUIAF IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_Sospechosa IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_Traslado IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Tipo_Registro IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.UsuCajero IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Valor_RegManual IN FRAME F-Main
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

