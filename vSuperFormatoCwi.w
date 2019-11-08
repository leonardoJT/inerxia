&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE dsuperformatoc NO-UNDO
       {"dSuperFormatoC.i"}.
DEFINE TEMP-TABLE dsuperformatod NO-UNDO
       {"dSuperFormatoD.i"}.



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
&Scoped-define DATA-FIELD-DEFS "sbosuperformato.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS dsuperformatoc.Codigo dsuperformatoc.Nombre ~
dsuperformatoc.Observaciones dsuperformatoc.Periodicidad ~
dsuperformatoc.FormatoExcel dsuperformatoc.Proforma ~
dsuperformatoc.DocumentoTecnico dsuperformatoc.circular ~
dsuperformatoc.AreaInformacion dsuperformatoc.TipoYNumero ~
dsuperformatoc.EnDolares dsuperformatoc.EnMiles ~
dsuperformatoc.NombreArchivoSalida 
&Scoped-define ENABLED-TABLES dsuperformatoc
&Scoped-define FIRST-ENABLED-TABLE dsuperformatoc
&Scoped-Define ENABLED-OBJECTS RECT-294 
&Scoped-Define DISPLAYED-FIELDS dsuperformatoc.Codigo dsuperformatoc.Nombre ~
dsuperformatoc.Observaciones dsuperformatoc.Periodicidad ~
dsuperformatoc.FormatoExcel dsuperformatoc.Proforma ~
dsuperformatoc.DocumentoTecnico dsuperformatoc.circular ~
dsuperformatoc.AreaInformacion dsuperformatoc.TipoYNumero ~
dsuperformatoc.EnDolares dsuperformatoc.EnMiles ~
dsuperformatoc.NombreArchivoSalida 
&Scoped-define DISPLAYED-TABLES dsuperformatoc
&Scoped-define FIRST-DISPLAYED-TABLE dsuperformatoc


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-294
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 62 BY 19.38
     BGCOLOR 17 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     dsuperformatoc.Codigo AT ROW 2.62 COL 11 COLON-ALIGNED WIDGET-ID 2
          LABEL "Código" FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 12.43 BY 1 TOOLTIP "Código Formato"
     dsuperformatoc.Nombre AT ROW 3.69 COL 11 COLON-ALIGNED WIDGET-ID 8
          LABEL "Nombre"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     dsuperformatoc.Observaciones AT ROW 5.04 COL 6 NO-LABEL WIDGET-ID 18
          VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
          SIZE 52 BY 3.23
     dsuperformatoc.Periodicidad AT ROW 8.27 COL 22 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS COMBO-BOX SORT INNER-LINES 12
          LIST-ITEM-PAIRS "Anual","Anual",
                     "Bisemanal","Bisemanal",
                     "Diaria","Diaria",
                     "Esporádica","Esporádica",
                     "Mensual","Mensual",
                     "Mensual/Semes","Mensual/Semes",
                     "Semanal","Semanal",
                     "Semestral","Semestral",
                     "Trimes-Mensual","Trimes-Mensual",
                     "Trimestral","Trimestral",
                     "Permanente","Permanente",
                     "Diaria/Mensual","Diaria/Mensual"
          DROP-DOWN-LIST
          SIZE 34 BY 1 TOOLTIP "Periodicidad"
     dsuperformatoc.FormatoExcel AT ROW 9.35 COL 22 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     dsuperformatoc.Proforma AT ROW 10.42 COL 22 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     dsuperformatoc.DocumentoTecnico AT ROW 11.5 COL 22 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS COMBO-BOX SORT INNER-LINES 5
          LIST-ITEM-PAIRS "","",
                     "SBDS003","SBDS003",
                     "SBDS007","SBDS007",
                     "SBDS015-SBDS016","SBDS015-016",
                     "SFC-DTP-001","SFCDTP001"
          DROP-DOWN-LIST
          SIZE 34 BY 1 TOOLTIP "Document Técnico"
     dsuperformatoc.circular AT ROW 12.58 COL 22 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     dsuperformatoc.AreaInformacion AT ROW 13.92 COL 22 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS COMBO-BOX INNER-LINES 3
          LIST-ITEM-PAIRS "",0,
                     "Subsistema Contable Y Estadístico",1,
                     "Composición Arancelaria",2,
                     "Calificación De Activos",3
          DROP-DOWN-LIST
          SIZE 34 BY 1 TOOLTIP "Area De Información"
     dsuperformatoc.TipoYNumero AT ROW 15 COL 22 COLON-ALIGNED WIDGET-ID 34
          LABEL "Tipo Informe"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "",99,
                     "Estados financieros con operaciones de sucursales y agencias en el exterior o estados financieros con operaciones en el territorio nacional para aquellas entidades que no poseen sucursales en el exterior.",0,
                     "Estados financieros con operaciones en el territorio nacional (sólo para aquellas entidades que poseen sucursales y agencias en el exterior).",4,
                     "Bisemanal de encaje",6,
                     "Semanal principales cuentas activas y pasivas",7,
                     "Semanal tasas de interés activas y pasivas",8,
                     "Diario compra - venta de divisas",10,
                     "Semestral - formato no PUC de patrimonio adecuado",11,
                     "Diario tasas de interés",12,
                     "Formatos de inversiones prima media",15,
                     "Valoración diaria de los fondos de pensiones y cesantías",17,
                     "Movimiento de ingresos y egresos de cuentas corrientes en moneda extranjera",18,
                     "Valoración diaria del fondo común ordinario",19,
                     "Gestión de activos y pasivos",20,
                     "Diario posición propia",21,
                     "Información de afiliados y composición de portafolio de inversión",22,
                     "Información afiliados y pensionados",23,
                     "Flujo de caja ácido diario",25,
                     "Reporte de transacciones en efectivo",26,
                     "Exigibilidades y disponibilidades en FINAGRO",27,
                     "Informe sistema general de riesgos profesionales",28,
                     "Inversiones obligatorias en TRD",29,
                     "Reporte del grupo financiero nacional",30,
                     "Reporte grupo financiero nacional con subordinadas en el exterior (sub extranjero)",31,
                     "Financiación de vivienda",32,
                     "Costos servicios financieros",33,
                     "Riesgos de mercado",34,
                     "Estado anual de ingresos y egresos - SOAT",36,
                     "Capital mínimo de funcionamiento",37
          DROP-DOWN-LIST
          SIZE 34 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     dsuperformatoc.EnDolares AT ROW 16.08 COL 22 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS COMBO-BOX INNER-LINES 2
          LIST-ITEM-PAIRS "S","S",
                     "N","N"
          DROP-DOWN-LIST
          SIZE 6 BY 1 TOOLTIP "Reporte En Dólares"
     dsuperformatoc.EnMiles AT ROW 17.15 COL 22 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS COMBO-BOX INNER-LINES 2
          LIST-ITEM-PAIRS "S","S",
                     "N","N"
          DROP-DOWN-LIST
          SIZE 6 BY 1 TOOLTIP "En Miles"
     dsuperformatoc.NombreArchivoSalida AT ROW 18.23 COL 13.43 WIDGET-ID 40
          LABEL "Archivo Salida"
          VIEW-AS FILL-IN 
          SIZE 34 BY 1 TOOLTIP "Nombre Del Archivo De Salida"
     RECT-294 AT ROW 1 COL 1 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "sbosuperformato.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: dsuperformatoc D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {"dSuperFormatoC.i"}
      END-FIELDS.
      TABLE: dsuperformatod D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {"dSuperFormatoD.i"}
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
         HEIGHT             = 19.38
         WIDTH              = 62.29.
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

/* SETTINGS FOR FILL-IN dsuperformatoc.Codigo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN dsuperformatoc.Nombre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN dsuperformatoc.NombreArchivoSalida IN FRAME F-Main
   ALIGN-L EXP-LABEL                                                    */
/* SETTINGS FOR EDITOR dsuperformatoc.Observaciones IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       dsuperformatoc.Observaciones:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR COMBO-BOX dsuperformatoc.TipoYNumero IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       dsuperformatoc.TipoYNumero:PRIVATE-DATA IN FRAME F-Main     = 
                "[0]Estados financieros con operaciones de sucursales y agencias en el exterior o estados financieros con operaciones en el territorio nacional para aquellas entidades que no poseen sucursales en el exterior.
[4]Estados financieros con operaciones en el territorio nacional (sólo para aquellas entidades que poseen sucursales y agencias en el exterior).
[6]Bisemanal de encaje
[7]Semanal principales cuentas activas y pasivas
[8]Semanal tasas de interés activas y pasivas
[10]Diario compra - venta de divisas
[11]Semestral - formato no PUC de patrimonio adecuado
[12]Diario tasas de interés
[15]Formatos de inversiones prima media
[17]Valoración diaria de los fondos de pensiones y cesantías
[18]Movimiento de ingresos y egresos de cuentas corrientes en moneda extranjera
[19]Valoración diaria del fondo común ordinario
[20]Gestión de activos y pasivos
[21]Diario posición propia
[22]Información de afiliados y composición de portafolio de inversión
[23]Información afiliados y pensionados
[25]Flujo de caja ácido diario
[26]Reporte de transacciones en efectivo
[27]Exigibilidades y disponibilidades en FINAGRO
[28]Informe sistema general de riesgos profesionales
[29]Inversiones obligatorias en TRD
[30]Reporte del grupo financiero nacional
[31]Reporte grupo financiero nacional con subordinadas en el exterior (sub extranjero)
[32]Financiación de vivienda
[33]Costos servicios financieros
[34]Riesgos de mercado
[36]Estado anual de ingresos y egresos - SOAT
[37]Capital mínimo de funcionamiento".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME dsuperformatoc.Codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dsuperformatoc.Codigo vTableWin
ON VALUE-CHANGED OF dsuperformatoc.Codigo IN FRAME F-Main /* Código */
DO:
    SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      dsuperformatoc.TipoYNumero:TOOLTIP = replace(dsuperformatoc.TipoYNumero:PRIVATE-DATA,"[",CHR(10) + "[").
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

