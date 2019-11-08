&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dact_fijo.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dact_fijo.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Agencia RowObject.Estado ~
RowObject.Codigo RowObject.Nombre RowObject.Grupo RowObject.Descripcion ~
RowObject.Cen_Costos RowObject.Nit_Responsable RowObject.Nit_Seguro ~
RowObject.Nit_Proveedor RowObject.Nro_Seguro RowObject.Fec_Compra ~
RowObject.Mejoras RowObject.Vto_Seguro RowObject.Ord_Compra ~
RowObject.Val_Garantia RowObject.Nro_Factura RowObject.Fec_Garantia ~
RowObject.Val_Compra RowObject.Fec_IniDepre RowObject.Fec_Retiro ~
RowObject.Fec_debaja RowObject.Anos_Adepreciar RowObject.Fec_Venta ~
RowObject.Per_Depreciado RowObject.ValDepMes RowObject.Cos_Historico ~
RowObject.Neto RowObject.ValDepAcum RowObject.Sdo_Depre ~
RowObject.Fec_Avaluo 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-286 RECT-287 RECT-288 RECT-289 
&Scoped-Define DISPLAYED-FIELDS RowObject.Agencia RowObject.Estado ~
RowObject.Codigo RowObject.Nombre RowObject.Grupo RowObject.Descripcion ~
RowObject.Cen_Costos RowObject.Nit_Responsable RowObject.Nit_Seguro ~
RowObject.Nit_Proveedor RowObject.Nro_Seguro RowObject.Fec_Compra ~
RowObject.Mejoras RowObject.Vto_Seguro RowObject.Ord_Compra ~
RowObject.Val_Garantia RowObject.Nro_Factura RowObject.Fec_Garantia ~
RowObject.Val_Compra RowObject.Fec_IniDepre RowObject.Fec_Retiro ~
RowObject.Fec_debaja RowObject.Anos_Adepreciar RowObject.Fec_Venta ~
RowObject.Per_Depreciado RowObject.ValDepMes RowObject.Cos_Historico ~
RowObject.Neto RowObject.ValDepAcum RowObject.Sdo_Depre ~
RowObject.Fec_Avaluo 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-286
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 5.92.

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 2.42.

DEFINE RECTANGLE RECT-288
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 5.12.

DEFINE RECTANGLE RECT-289
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 3.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Agencia AT ROW 1 COL 28 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     RowObject.Estado AT ROW 1.27 COL 107 NO-LABEL WIDGET-ID 14
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Retirado", 2,
"Depreciado", 3
          SIZE 17 BY 1.88
     RowObject.Codigo AT ROW 2.08 COL 28 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     RowObject.Nombre AT ROW 2.08 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 54 BY .81
     RowObject.Grupo AT ROW 3 COL 28 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 7 BY .81
     RowObject.Descripcion AT ROW 3.04 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 63 BY .81
     RowObject.Cen_Costos AT ROW 3.96 COL 28 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 7 BY .81
     RowObject.Nit_Responsable AT ROW 7.19 COL 99 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
     RowObject.Nit_Seguro AT ROW 7.54 COL 54.86 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 19.86 BY .81
     RowObject.Nit_Proveedor AT ROW 7.73 COL 18.29 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
     RowObject.Nro_Seguro AT ROW 8.38 COL 55 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 20 BY .81
     RowObject.Fec_Compra AT ROW 8.54 COL 18.29 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     RowObject.Mejoras AT ROW 9.08 COL 99 COLON-ALIGNED NO-LABEL WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 20 BY .81
     RowObject.Vto_Seguro AT ROW 9.31 COL 55.14 COLON-ALIGNED NO-LABEL WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
     RowObject.Ord_Compra AT ROW 9.42 COL 18.29 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     RowObject.Val_Garantia AT ROW 10.27 COL 55.14 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
     RowObject.Nro_Factura AT ROW 10.31 COL 18.29 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     RowObject.Fec_Garantia AT ROW 11.15 COL 55.29 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 12.72 BY .81
     RowObject.Val_Compra AT ROW 11.19 COL 18.29 COLON-ALIGNED WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
     RowObject.Fec_IniDepre AT ROW 12.04 COL 18.29 COLON-ALIGNED NO-LABEL WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 18 BY .96
     RowObject.Fec_Retiro AT ROW 12.62 COL 56 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     RowObject.Fec_debaja AT ROW 13.42 COL 56 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     RowObject.Anos_Adepreciar AT ROW 13.5 COL 19 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 8 BY .96
     RowObject.Fec_Venta AT ROW 14.23 COL 56 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     RowObject.Per_Depreciado AT ROW 14.46 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.ValDepMes AT ROW 16.35 COL 72 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 19.72 BY 1
     RowObject.Cos_Historico AT ROW 16.62 COL 25 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
     RowObject.Neto AT ROW 17.42 COL 25 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 19.72 BY 1
     RowObject.ValDepAcum AT ROW 17.42 COL 72 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 19.72 BY 1
     RowObject.Sdo_Depre AT ROW 18.5 COL 72 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 18.57 BY 1
     RowObject.Fec_Avaluo AT ROW 20.38 COL 66 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     "Mejoras:" VIEW-AS TEXT
          SIZE 9 BY 1.08 AT ROW 8.81 COL 92 WIDGET-ID 82
     "Venci/to:" VIEW-AS TEXT
          SIZE 8 BY 1.08 AT ROW 9.08 COL 48.57 WIDGET-ID 78
     "Meses" VIEW-AS TEXT
          SIZE 7 BY 1.62 AT ROW 13.65 COL 32 WIDGET-ID 104
     "Periodos Depre.:" VIEW-AS TEXT
          SIZE 16 BY 1.08 AT ROW 14.42 COL 5 WIDGET-ID 106
     "Fecha Ini.Depre:" VIEW-AS TEXT
          SIZE 15.43 BY 1.08 AT ROW 12.12 COL 4.43 WIDGET-ID 88
     RECT-286 AT ROW 7.46 COL 1 WIDGET-ID 90
     RECT-287 AT ROW 13.38 COL 1 WIDGET-ID 108
     RECT-288 AT ROW 7.19 COL 41 WIDGET-ID 110
     RECT-289 AT ROW 12.31 COL 41 WIDGET-ID 112
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dact_fijo.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dact_fijo.i}
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
         HEIGHT             = 21.09
         WIDTH              = 130.72.
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

/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F-Main
   EXP-LABEL                                                            */
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

