&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"d-act_fijos.i"}.



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
&Scoped-define DATA-FIELD-DEFS "d-act_fijos.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Agencia RowObject.Codigo ~
RowObject.Nombre RowObject.Grupo RowObject.Descripcion RowObject.Cen_Costos ~
RowObject.Nit_Seguro RowObject.Nit_Responsable RowObject.Nit_Proveedor ~
RowObject.Nro_Seguro RowObject.Fec_Compra RowObject.Fec_Asignacion ~
RowObject.Ord_Compra RowObject.Vto_Seguro RowObject.Mejoras ~
RowObject.Val_Garantia RowObject.Nro_Factura RowObject.Val_Valorizacion ~
RowObject.Val_Compra RowObject.Val_Comercial RowObject.Fec_Retiro ~
RowObject.Fec_Garantia RowObject.Fec_debaja RowObject.Val_Provision ~
RowObject.Fec_Venta RowObject.Sdo_Provision RowObject.Fec_IniDepre ~
RowObject.Cos_Historico RowObject.Neto RowObject.Sdo_Depre ~
RowObject.ValDepMes RowObject.ValDepAcum 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-5 RECT-8 RECT-9 RECT-10 
&Scoped-Define DISPLAYED-FIELDS RowObject.Agencia RowObject.Codigo ~
RowObject.Nombre RowObject.Estado RowObject.Grupo RowObject.Descripcion ~
RowObject.Cen_Costos RowObject.Nit_Seguro RowObject.Nit_Responsable ~
RowObject.Nit_Proveedor RowObject.Nro_Seguro RowObject.Fec_Compra ~
RowObject.Fec_Asignacion RowObject.Ord_Compra RowObject.Vto_Seguro ~
RowObject.Mejoras RowObject.Val_Garantia RowObject.Nro_Factura ~
RowObject.Val_Valorizacion RowObject.Val_Compra RowObject.Val_Comercial ~
RowObject.Fec_Retiro RowObject.Fec_Garantia RowObject.Fec_debaja ~
RowObject.Val_Provision RowObject.Fec_Venta RowObject.Sdo_Provision ~
RowObject.Id_Prestamo RowObject.Anos_Adepreciar RowObject.Per_Depreciado ~
RowObject.Fec_IniDepre RowObject.Cos_Historico RowObject.Neto ~
RowObject.Sdo_Depre RowObject.ValDepMes RowObject.ValDepAcum 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 2.15.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 5.12.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 112 BY 6.62.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 8.62.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 3.5.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 8.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Agencia AT ROW 1.81 COL 17 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     RowObject.Codigo AT ROW 2.77 COL 17 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     RowObject.Nombre AT ROW 2.77 COL 24.57 COLON-ALIGNED NO-LABEL WIDGET-ID 72
          VIEW-AS FILL-IN 
          SIZE 64.43 BY .81
     RowObject.Estado AT ROW 3.04 COL 95 NO-LABEL WIDGET-ID 20
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Retirado", 2,
"Depreciado", 3
          SIZE 13 BY 1.35
     RowObject.Grupo AT ROW 3.69 COL 17 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .81
     RowObject.Descripcion AT ROW 3.69 COL 24.57 COLON-ALIGNED NO-LABEL WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 64.43 BY .81
     RowObject.Cen_Costos AT ROW 4.5 COL 17.14 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     RowObject.Nit_Seguro AT ROW 6.27 COL 51 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 16.43 BY 1
     RowObject.Nit_Responsable AT ROW 6.27 COL 90.86 COLON-ALIGNED WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 16.43 BY 1
     RowObject.Nit_Proveedor AT ROW 6.35 COL 17 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 16.43 BY 1
     RowObject.Nro_Seguro AT ROW 7.35 COL 51 COLON-ALIGNED WIDGET-ID 76
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     RowObject.Fec_Compra AT ROW 7.42 COL 17 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     RowObject.Fec_Asignacion AT ROW 7.42 COL 91 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 16 BY 1.12
     RowObject.Ord_Compra AT ROW 8.5 COL 17 COLON-ALIGNED WIDGET-ID 78
          VIEW-AS FILL-IN 
          SIZE 14.43 BY 1
     RowObject.Vto_Seguro AT ROW 8.5 COL 51 COLON-ALIGNED NO-LABEL WIDGET-ID 112
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
     RowObject.Mejoras AT ROW 8.92 COL 90.86 COLON-ALIGNED NO-LABEL WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
     RowObject.Val_Garantia AT ROW 9.5 COL 51 COLON-ALIGNED WIDGET-ID 102
          VIEW-AS FILL-IN 
          SIZE 18.57 BY 1
     RowObject.Nro_Factura AT ROW 9.58 COL 17 COLON-ALIGNED WIDGET-ID 74
          VIEW-AS FILL-IN 
          SIZE 14.43 BY 1
     RowObject.Val_Valorizacion AT ROW 9.81 COL 90.86 COLON-ALIGNED WIDGET-ID 110
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     RowObject.Val_Compra AT ROW 10.65 COL 17 COLON-ALIGNED WIDGET-ID 100
          VIEW-AS FILL-IN 
          SIZE 18.57 BY 1
     RowObject.Val_Comercial AT ROW 10.88 COL 90.86 COLON-ALIGNED WIDGET-ID 98
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     RowObject.Fec_Retiro AT ROW 11.19 COL 55 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     RowObject.Fec_Garantia AT ROW 11.73 COL 17 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 12.86 BY 1
     RowObject.Fec_debaja AT ROW 12 COL 55 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     RowObject.Val_Provision AT ROW 12 COL 90.86 COLON-ALIGNED WIDGET-ID 104
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.Fec_Venta AT ROW 12.85 COL 55 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     RowObject.Sdo_Provision AT ROW 13.04 COL 90.86 COLON-ALIGNED WIDGET-ID 86
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     RowObject.Id_Prestamo AT ROW 15.27 COL 13 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 5.43 BY 1
     RowObject.Anos_Adepreciar AT ROW 16.62 COL 53 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     RowObject.Per_Depreciado AT ROW 16.62 COL 85 COLON-ALIGNED WIDGET-ID 80
          VIEW-AS FILL-IN 
          SIZE 7.72 BY .81
     RowObject.Fec_IniDepre AT ROW 16.73 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     RowObject.Cos_Historico AT ROW 17.69 COL 29 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     RowObject.Neto AT ROW 17.69 COL 50 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     RowObject.Sdo_Depre AT ROW 17.69 COL 85 COLON-ALIGNED WIDGET-ID 84
          VIEW-AS FILL-IN 
          SIZE 18.57 BY 1
     RowObject.ValDepMes AT ROW 18.77 COL 29 COLON-ALIGNED WIDGET-ID 92
          VIEW-AS FILL-IN 
          SIZE 19.72 BY 1
     RowObject.ValDepAcum AT ROW 18.77 COL 85 COLON-ALIGNED WIDGET-ID 90
          VIEW-AS FILL-IN 
          SIZE 19.72 BY 1
     "Mejoras:" VIEW-AS TEXT
          SIZE 9 BY 1.08 AT ROW 8.92 COL 83.86 WIDGET-ID 124
     "Fec.Ini_Depre.:" VIEW-AS TEXT
          SIZE 14.43 BY 1.08 AT ROW 16.62 COL 16 WIDGET-ID 130
     "Vcto. Seguro:" VIEW-AS TEXT
          SIZE 13 BY 1.08 AT ROW 8.27 COL 40 WIDGET-ID 118
     RECT-6 AT ROW 14.73 COL 1 WIDGET-ID 114
     RECT-7 AT ROW 5.85 COL 1 WIDGET-ID 116
     RECT-5 AT ROW 5.85 COL 39 WIDGET-ID 120
     RECT-8 AT ROW 10.96 COL 39 WIDGET-ID 122
     RECT-9 AT ROW 5.85 COL 73 WIDGET-ID 126
     RECT-10 AT ROW 2.62 COL 94 WIDGET-ID 128
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "d-act_fijos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {d-act_fijos.i}
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
         HEIGHT             = 20.77
         WIDTH              = 113.14.
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

/* SETTINGS FOR FILL-IN RowObject.Anos_Adepreciar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Id_Prestamo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Per_Depreciado IN FRAME F-Main
   NO-ENABLE                                                            */
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

