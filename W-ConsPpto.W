&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-PptCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-PptCon 
/*------------------------------------------------------------------------
  ARCHIVO    :W-PPTCON.W

  DESCRIPCION:Programa que consulta y compara los saldos de presupuesto con los
              de movimiento.
------------------------------------------------------------------------*/
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{Incluido\VARIABLE.I "SHARED"}

/* Variables para manejo de combo-box */
 DEFINE VAR W_COfi      AS   LOGICAL.
 DEFINE VAR W_COfiCon   AS   LOGICAL.
 DEFINE VAR W_CCenCos   AS   LOGICAL.
 DEFINE VAR W_OfStr     AS   CHAR    FORMAT "X(20)".

/*  Tabla temporal de manejo de partidas contables */
 DEFINE TEMP-TABLE Tmp
   FIELD W_CCosto       LIKE Sal_Presupuesto.Cen_Costos              LABEL "C.C":C
   FIELD W_Cuenta       LIKE Sal_Presupuesto.Cod_Presupuesto         LABEL "Cuenta":C
   FIELD W_Nombre       AS   CHAR    FORMAT "X(25)"                  LABEL "Nombre":C
   FIELD W_Ppto         AS   DECIMAL FORMAT "->,>>>,>>>,>>>,>>9.99"  LABEL "Presupuestado":C
   FIELD W_Mvto         AS   DECIMAL FORMAT "->,>>>,>>>,>>>,>>9.99"  LABEL "Ejecutado":C
   FIELD W_PDif         AS   DECIMAL FORMAT "->>>>.99"               LABEL "%Eje":C
   FIELD W_VDif         AS   DECIMAL FORMAT ">>>>,>>>,>>>,>>9.99"   LABEL "Variación":C.

  DEFINE VAR W_Cco      LIKE Cen_Costos.Cen_Costos.
  DEFINE VAR W_CenIni   LIKE Cen_Costos.Cen_Costos.
  DEFINE VAR W_CenFin   LIKE Cen_Costos.Cen_Costos.
  DEFINE VAR W_CenOfi   LIKE agencias.agencia.
  DEFINE VAR W_Ofi      LIKE agencias.agencia.
  DEFINE VAR W_OfiIni   LIKE agencias.agencia.
  DEFINE VAR W_OfiFin   LIKE agencias.agencia.
  DEFINE VAR W_SalCre   LIKE Sal_Presupuesto.Sdo_Final.
  DEFINE VAR W_SalDeb   LIKE Sal_Presupuesto.Sdo_Final.
  DEFINE VAR W_SdoPpto  LIKE Sal_Presupuesto.Sdo_Final.
  DEFINE VAR W_SdoMvto  LIKE Sal_Presupuesto.Sdo_Final.
  DEFINE VAR W_TotPpto  LIKE Sal_Presupuesto.Sdo_Final.
  DEFINE VAR W_TotMvto  LIKE Sal_Presupuesto.Sdo_Final.
  DEFINE VAR Cont       AS   INTEGER.
  DEFINE VAR Linea      AS   CHARACTER.
  DEFINE VAR Linea2     AS   CHARACTER.
  DEFINE VAR Linea3     AS   CHARACTER.
  DEFINE VAR Linea4     AS   CHARACTER.
  DEFINE VAR Linea5     AS   CHARACTER.
  DEFINE VAR Linea6     AS   CHARACTER.
  DEFINE VAR Linea7     AS   CHARACTER.
  DEFINE VAR Linea8     AS   CHARACTER.
  DEFINE VAR Raya       AS   CHARACTER INITIAL "-".
  DEFINE VAR W_HayInf   AS   LOGICAL   INITIAL FALSE.
  DEFINE VAR W_Mes      AS   LOGICAL   EXTENT 12.
  DEFINE VAR W_MesCont  AS   INTEGER   FORMAT "99".
  DEFINE VAR W_MesUlt   AS   INTEGER   FORMAT "99".
  DEFINE VAR W_Rta      AS   LOGICAL   INITIAL FALSE.
  DEFINE VAR W_SiHaySdo AS   LOGICAL   INITIAL FALSE.
  DEFINE VAR W_Titulo   AS   CHARACTER FORMAT "X(40)".
  DEFINE VAR W_Tipo     AS   CHARACTER FORMAT "X(6)".
  DEFINE VAR W_WidSel   AS   HANDLE.
  DEFINE VAR W_Seleccion AS   LOGICAL   INITIAL FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_PptCon
&Scoped-define BROWSE-NAME Temp

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Tmp

/* Definitions for BROWSE Temp                                          */
&Scoped-define FIELDS-IN-QUERY-Temp Tmp.W_CCosto Tmp.W_Cuenta Tmp.W_Nombre Tmp.W_Ppto Tmp.W_Mvto Tmp.W_PDif Tmp.W_VDif   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Temp   
&Scoped-define SELF-NAME Temp
&Scoped-define QUERY-STRING-Temp FOR EACH Tmp NO-LOCK
&Scoped-define OPEN-QUERY-Temp OPEN QUERY Temp FOR EACH Tmp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Temp Tmp
&Scoped-define FIRST-TABLE-IN-QUERY-Temp Tmp


/* Definitions for FRAME F_PptCon                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_PptCon ~
    ~{&OPEN-QUERY-Temp}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-69 W_OfiPpto W_Cencostos W_Ano ~
Btn_Imprimir Btn_VerCta T_ConCta Btn_Generar Temp Btn_Nueva Btn-Salir ~
BUTTON-68 RECT-122 RECT-123 RECT-42 RECT-43 
&Scoped-Define DISPLAYED-OBJECTS W_OfiPpto W_Nomagencia W_Cencostos ~
W_NomCcos W_Ano T_Ene T_Feb T_Mar T_Abr T_May T_Jun T_Jul T_Ago T_Sep T_Oct ~
T_Nov T_Dic T_ConCta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 T_Ene T_Feb T_Mar T_Abr T_May T_Jun T_Jul T_Ago T_Sep ~
T_Oct T_Nov T_Dic T_ConCta 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-PptCon AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Salir DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.88
     BGCOLOR 8 .

DEFINE BUTTON Btn_Generar 
     LABEL "&Generar" 
     SIZE 10 BY 1.88.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Nueva 
     LABEL "&Nueva" 
     SIZE 10 BY 1.88.

DEFINE BUTTON Btn_VerCta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "&Ver Cuenta" 
     SIZE 10 BY 1.88.

DEFINE BUTTON BUTTON-68 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 68" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda".

DEFINE BUTTON BUTTON-69 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 69" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_Ano AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE W_Cencostos AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cta AS CHARACTER FORMAT "X(14)":U 
     LABEL "Código Presupuesto" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W_Nomagencia AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCcos AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomPpto AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_OfiPpto AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "agencia" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE W_Pro AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Reg. Procesados" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-122
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 1.35.

DEFINE RECTANGLE RECT-123
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 1.62.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26 BY 1.62.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 6.73.

DEFINE VARIABLE T_Abr AS LOGICAL INITIAL no 
     LABEL "Abril" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .5 NO-UNDO.

DEFINE VARIABLE T_Ago AS LOGICAL INITIAL no 
     LABEL "Agosto" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .5 NO-UNDO.

DEFINE VARIABLE T_ConCta AS LOGICAL INITIAL no 
     LABEL "Consolidado por Cuenta" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE T_Dic AS LOGICAL INITIAL no 
     LABEL "Diciembre" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .5 NO-UNDO.

DEFINE VARIABLE T_Ene AS LOGICAL INITIAL no 
     LABEL "Enero" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .5 NO-UNDO.

DEFINE VARIABLE T_Feb AS LOGICAL INITIAL no 
     LABEL "Febrero" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .5 NO-UNDO.

DEFINE VARIABLE T_Jul AS LOGICAL INITIAL no 
     LABEL "Julio" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .5 NO-UNDO.

DEFINE VARIABLE T_Jun AS LOGICAL INITIAL no 
     LABEL "Junio" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .5 NO-UNDO.

DEFINE VARIABLE T_Mar AS LOGICAL INITIAL no 
     LABEL "Marzo" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .5 NO-UNDO.

DEFINE VARIABLE T_May AS LOGICAL INITIAL no 
     LABEL "Mayo" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .5 NO-UNDO.

DEFINE VARIABLE T_Nov AS LOGICAL INITIAL no 
     LABEL "Noviembre" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .5 NO-UNDO.

DEFINE VARIABLE T_Oct AS LOGICAL INITIAL no 
     LABEL "Octubre" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .5 NO-UNDO.

DEFINE VARIABLE T_Sep AS LOGICAL INITIAL no 
     LABEL "Septiembre" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .5 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Temp FOR 
      Tmp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Temp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Temp W-PptCon _FREEFORM
  QUERY Temp NO-LOCK DISPLAY
      Tmp.W_CCosto
  Tmp.W_Cuenta
  Tmp.W_Nombre
  Tmp.W_Ppto
  Tmp.W_Mvto label "Ejecutado":C
  Tmp.W_PDif
  Tmp.W_VDif
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 4.69
         BGCOLOR 15 FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_PptCon
     BUTTON-69 AT ROW 1.81 COL 101
     W_OfiPpto AT ROW 4.5 COL 7 COLON-ALIGNED
     W_Nomagencia AT ROW 4.5 COL 13 COLON-ALIGNED NO-LABEL
     W_Cencostos AT ROW 4.5 COL 47 COLON-ALIGNED NO-LABEL
     W_NomCcos AT ROW 4.5 COL 52 COLON-ALIGNED NO-LABEL
     W_Ano AT ROW 4.5 COL 82 COLON-ALIGNED
     Btn_Imprimir AT ROW 5.58 COL 101
     T_Ene AT ROW 6.38 COL 5
     T_Feb AT ROW 6.38 COL 18
     T_Mar AT ROW 6.38 COL 30
     T_Abr AT ROW 6.38 COL 44
     T_May AT ROW 6.38 COL 58
     T_Jun AT ROW 6.38 COL 74
     T_Jul AT ROW 6.92 COL 5
     T_Ago AT ROW 6.92 COL 18
     T_Sep AT ROW 6.92 COL 30
     T_Oct AT ROW 6.92 COL 44
     T_Nov AT ROW 6.92 COL 58
     T_Dic AT ROW 6.92 COL 74
     Btn_VerCta AT ROW 7.19 COL 101
     T_ConCta AT ROW 8.81 COL 5
     W_Cta AT ROW 8.81 COL 42 COLON-ALIGNED
     W_NomPpto AT ROW 8.81 COL 57 COLON-ALIGNED NO-LABEL
     Btn_Generar AT ROW 10.42 COL 101
     Temp AT ROW 10.96 COL 2
     Btn_Nueva AT ROW 12.31 COL 101
     Btn-Salir AT ROW 14.19 COL 101
     W_Pro AT ROW 16.62 COL 49 COLON-ALIGNED
     BUTTON-68 AT ROW 18.77 COL 104
     RECT-122 AT ROW 8.54 COL 2
     RECT-123 AT ROW 6.12 COL 2
     RECT-42 AT ROW 16.08 COL 64
     RECT-43 AT ROW 9.88 COL 100
     "Datos para la consulta" VIEW-AS TEXT
          SIZE 88 BY .5 AT ROW 3.69 COL 2
          BGCOLOR 18 FGCOLOR 15 
     "Centro de Costos." VIEW-AS TEXT
          SIZE 13 BY .5 AT ROW 3.69 COL 49
          BGCOLOR 18 FGCOLOR 15 
     "Escoja los meses a consultar" VIEW-AS TEXT
          SIZE 88 BY .5 AT ROW 5.58 COL 2
          BGCOLOR 18 FGCOLOR 15 
     "Parámetros de Consolidación" VIEW-AS TEXT
          SIZE 88 BY .5 AT ROW 8 COL 2
          BGCOLOR 18 FGCOLOR 15 
     "Resultado de la consulta" VIEW-AS TEXT
          SIZE 88 BY .5 AT ROW 10.15 COL 2
          BGCOLOR 18 FGCOLOR 15 
     "." VIEW-AS TEXT
          SIZE 88 BY .5 AT ROW 15.54 COL 2
          BGCOLOR 18 FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.43 BY 19.77
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-PptCon ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta Ejecución Presupuestal"
         HEIGHT             = 21.15
         WIDTH              = 113.43
         MAX-HEIGHT         = 21.38
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.38
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = 17
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-PptCon
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_PptCon
                                                                        */
/* BROWSE-TAB Temp Btn_Generar F_PptCon */
/* SETTINGS FOR TOGGLE-BOX T_Abr IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_Ago IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_ConCta IN FRAME F_PptCon
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX T_Dic IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_Ene IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_Feb IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_Jul IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_Jun IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_Mar IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_May IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_Nov IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_Oct IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX T_Sep IN FRAME F_PptCon
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Cta IN FRAME F_PptCon
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_Cta:HIDDEN IN FRAME F_PptCon           = TRUE.

/* SETTINGS FOR FILL-IN W_Nomagencia IN FRAME F_PptCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCcos IN FRAME F_PptCon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomPpto IN FRAME F_PptCon
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_NomPpto:HIDDEN IN FRAME F_PptCon           = TRUE.

/* SETTINGS FOR FILL-IN W_Pro IN FRAME F_PptCon
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_Pro:HIDDEN IN FRAME F_PptCon           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-PptCon)
THEN W-PptCon:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Temp
/* Query rebuild information for BROWSE Temp
     _START_FREEFORM
OPEN QUERY Temp FOR EACH Tmp NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE Temp */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-PptCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-PptCon W-PptCon
ON END-ERROR OF W-PptCon /* Consulta Ejecución Presupuestal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-PptCon W-PptCon
ON WINDOW-CLOSE OF W-PptCon /* Consulta Ejecución Presupuestal */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Salir W-PptCon
ON CHOOSE OF Btn-Salir IN FRAME F_PptCon /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Generar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Generar W-PptCon
ON CHOOSE OF Btn_Generar IN FRAME F_PptCon /* Generar */
DO:
   W_Pro = 0.
   FOR EACH Tmp: DELETE Tmp. END.

   ASSIGN {&List-1}.
   RUN Asignar-Variables.
   
   IF NOT W_Seleccion THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 127, OUTPUT W_Eleccion).
     RETURN NO-APPLY.
   END.
   
   IF W_Ano > YEAR(TODAY) OR W_Ano = 0 THEN DO: 
     RUN MostrarMensaje IN W_Manija (INPUT 96, OUTPUT W_Eleccion).
     APPLY "ENTRY" TO W_Ano.
     RETURN NO-APPLY.
   END.

   IF W_Ano = 0 THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 94, OUTPUT W_Eleccion).
     ENABLE {&List-1} WITH FRAME F_PptCon.
     RETURN NO-APPLY.
   END.

   DO WITH FRAME F_PptCon:
     DISABLE {&List-1}.
     W_OfiPpto:SENSITIVE    = FALSE.
     W_CenCostos:SENSITIVE  = FALSE.
     W_Ano:SENSITIVE        = FALSE.
     DISPLAY W_OfiPPto.
     DISPLAY W_Cencostos.
     DISPLAY W_Ano.
     VIEW W_Pro.
     DISABLE Btn_Generar.
     IF T_ConCta THEN
       W_Cta:SENSITIVE     = FALSE.
   END.
   
   W_HayInf = FALSE.
   RUN _SetCurs.r ("WAIT").
   IF T_ConCta THEN
     RUN Generar-Temporal-Consolidado.
   ELSE
     RUN Generar-Temporal.

   IF NOT W_HayInf THEN
     RUN MostrarMensaje IN W_Manija (INPUT 79, OUTPUT W_Rta ).

   HIDE W_Pro  IN FRAME F_pptCon.
   RUN _SetCurs.r ("ARROW").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-PptCon
ON CHOOSE OF Btn_Imprimir IN FRAME F_PptCon /* Imprimir */
DO:
  DEFINE VAR Listado AS CHAR INITIAL "L_Ppto.Lst".
  
  Listado = W_PathSpl + Listado.
  {Incluido\Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Nueva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Nueva W-PptCon
ON CHOOSE OF Btn_Nueva IN FRAME F_PptCon /* Nueva */
DO:
  DO WITH FRAME F_pptcon:
    W_OfiPpto:SENSITIVE   IN   FRAME F_PptCon = TRUE.
    W_CenCostos:SENSITIVE IN   FRAME F_PptCon = TRUE.
    W_Ano:SENSITIVE       IN   FRAME F_PptCon = TRUE.
    ENABLE Btn_Generar    WITH FRAME F_pptCon.

    ENABLE {&List-1}.
    ASSIGN T_Ene:SCREEN-VALUE = "No"
           T_Ene              = NO
           T_Feb:SCREEN-VALUE = "No"
           T_Feb              = NO
           T_Mar:SCREEN-VALUE = "No"
           T_Mar              = NO
           T_Abr:SCREEN-VALUE = "No"
           T_Abr              = NO
           T_May:SCREEN-VALUE = "No"
           T_May              = NO
           T_Jun:SCREEN-VALUE = "No"
           T_Jun              = NO
           T_Jul:SCREEN-VALUE = "No"
           T_Jul              = NO
           T_Ago:SCREEN-VALUE = "No"
           T_Ago              = NO
           T_Sep:SCREEN-VALUE = "No"
           T_Sep              = NO
           T_Oct:SCREEN-VALUE = "No"
           T_Oct              = NO
           T_Nov:SCREEN-VALUE = "No"
           T_Nov              = NO
           T_Dic:SCREEN-VALUE = "No"
           T_Dic              = NO
           T_ConCta:SCREEN-VALUE  = "No"
           W_Cta:SCREEN-VALUE = "".
    HIDE W_Cta W_NomPPto.
  END.

  RUN HabilitarMes.
  RUN Selec_Mes.

  FOR EACH Tmp: DELETE Tmp. END.
  OPEN QUERY Temp FOR EACH Tmp NO-LOCK.
  APPLY "ENTRY" TO W_OfiPpto IN FRAME F_PptCon.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_VerCta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_VerCta W-PptCon
ON CHOOSE OF Btn_VerCta IN FRAME F_PptCon /* Ver Cuenta */
DO:
  RUN W-GrapptoManual.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-68
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-68 W-PptCon
ON CHOOSE OF BUTTON-68 IN FRAME F_PptCon /* Button 68 */
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "Ayudas\CONTABIL" CONTEXT 4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-69
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-69 W-PptCon
ON CHOOSE OF BUTTON-69 IN FRAME F_PptCon /* Button 69 */
DO:
  RUN  W-infdia.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Abr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Abr W-PptCon
ON VALUE-CHANGED OF T_Abr IN FRAME F_PptCon /* Abril */
DO:
  ASSIGN T_Abr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Ago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Ago W-PptCon
ON VALUE-CHANGED OF T_Ago IN FRAME F_PptCon /* Agosto */
DO:
  ASSIGN T_Ago.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_ConCta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_ConCta W-PptCon
ON VALUE-CHANGED OF T_ConCta IN FRAME F_PptCon /* Consolidado por Cuenta */
DO:
  ASSIGN T_ConCta.
  CASE T_ConCta:
    WHEN TRUE THEN
     DO WITH FRAME F_PptCon:
       ASSIGN W_NomPpto = ""
              W_Cta     = "".
       DISPLAY W_NomPPto WITH FRAME F_pptCon.
       DISPLAY W_Cta     WITH FRAME F_pptCon.
       VIEW W_NomPpto    IN   FRAME F_pptCon.
       ENABLE W_Cta      WITH FRAME F_PptCon.
       APPLY "ENTRY" TO W_Cta.
     END.
    WHEN FALSE THEN
         HIDE W_Cta W_NomPpto IN FRAME F_PptCon.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Dic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Dic W-PptCon
ON VALUE-CHANGED OF T_Dic IN FRAME F_PptCon /* Diciembre */
DO:
  ASSIGN T_Dic.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Ene
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Ene W-PptCon
ON VALUE-CHANGED OF T_Ene IN FRAME F_PptCon /* Enero */
DO:
  ASSIGN T_Ene.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Feb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Feb W-PptCon
ON VALUE-CHANGED OF T_Feb IN FRAME F_PptCon /* Febrero */
DO:
  ASSIGN T_Feb.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Jul
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Jul W-PptCon
ON VALUE-CHANGED OF T_Jul IN FRAME F_PptCon /* Julio */
DO:
  ASSIGN T_Jul.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Jun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Jun W-PptCon
ON VALUE-CHANGED OF T_Jun IN FRAME F_PptCon /* Junio */
DO:
  ASSIGN T_Jun.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Mar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Mar W-PptCon
ON VALUE-CHANGED OF T_Mar IN FRAME F_PptCon /* Marzo */
DO:
  ASSIGN T_Mar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_May
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_May W-PptCon
ON VALUE-CHANGED OF T_May IN FRAME F_PptCon /* Mayo */
DO:
  ASSIGN T_May.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Nov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Nov W-PptCon
ON VALUE-CHANGED OF T_Nov IN FRAME F_PptCon /* Noviembre */
DO:
  ASSIGN T_Nov.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Oct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Oct W-PptCon
ON VALUE-CHANGED OF T_Oct IN FRAME F_PptCon /* Octubre */
DO:
  ASSIGN T_Oct.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T_Sep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Sep W-PptCon
ON VALUE-CHANGED OF T_Sep IN FRAME F_PptCon /* Septiembre */
DO:
  ASSIGN T_Sep.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Ano
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Ano W-PptCon
ON LEAVE OF W_Ano IN FRAME F_PptCon /* Año */
DO:
  ASSIGN W_Ano.
  IF W_Ano > YEAR(W_Fecha) OR W_Ano = 0 THEN
   DO: 
     RUN MostrarMensaje IN W_Manija (INPUT 96, OUTPUT W_Eleccion).
     RETURN NO-APPLY.
   END.
  ENABLE {&List-1} WITH FRAME F_PptCon.
  RUN HabilitarMes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Cencostos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cencostos W-PptCon
ON LEAVE OF W_Cencostos IN FRAME F_PptCon
DO:
  ASSIGN W_CenCostos.
  W_WidSel = LAST-EVENT:WIDGET-ENTER.
  IF W_WidSel:NAME = "Btn_Salir" THEN
     RETURN.
  DO WITH FRAME F_PptCon:
    IF W_CenCostos NE 0 THEN DO:
      FIND Cen_Costos WHERE Cen_Costos.agencia    EQ W_OfiPpto
                        AND Cen_Costos.Estado     EQ 1
                        AND Cen_Costos.Cen_Costos EQ W_CenCostos 
                    NO-LOCK NO-ERROR.
      IF AVAILABLE(Cen_Costos) THEN DO:
        ASSIGN W_NomCcos = Cen_Costos.Nombre.
        DISPLAY Cen_Costos.Nombre @ W_NomCcos.
      END.
      ELSE DO:
       RUN C-Cencosto.r (OUTPUT W_CenCostos, OUTPUT W_NomCcos).
        IF W_CenCostos EQ 0 THEN DO:
          APPLY "ENTRY" TO W_CenCostos.
          RETURN NO-APPLY.
        END.
        DISPLAY W_NomCcos W_CenCostos.
        DISPLAY W_CenCostos.
        ASSIGN W_CenIni = W_CenCostos
               W_CenFin = W_CenCostos.
              
       END.
     END.
    ELSE DO:
      ASSIGN W_CenIni = 000
             W_CenFin = 999
             W_NomCcos = "Consolidado".
      DISPLAY "Consolidada." @ W_NomCcos. 
    END.
  END.
  RUN ConsultarCcostos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Cta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cta W-PptCon
ON LEAVE OF W_Cta IN FRAME F_PptCon /* Código Presupuesto */
DO: 
  DO WITH FRAME F_PptCon:
    W_WidSel = LAST-EVENT:WIDGET-ENTER.
    IF W_WidSel:NAME = "Btn-Salir" OR  W_WidSel:NAME = "Btn-Nueva" OR
       W_WidSel:NAME = "T_ConCta" THEN
     RETURN.
    ASSIGN W_Cta.
    FIND Presupuesto WHERE Presupuesto.Cod_Presupuesto = W_Cta AND
                           Presupuesto.Estado          = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE (Presupuesto) THEN
     DO:
        RUN C-CtaPpto.r (OUTPUT W_Cta, OUTPUT W_NomPPto).
        IF TRIM(W_Cta) = "" THEN
         DO:
           APPLY "ENTRY" TO W_Cta.
           RETURN NO-APPLY.          
         END. 
        ELSE
         FIND Presupuesto WHERE Presupuesto.Cod_Presupuesto = W_Cta
                            AND Presupuesto.Estado          = 1 
                        NO-LOCK NO-ERROR.        
     END.
    ELSE 
      W_NomPPto = Presupuesto.Nombre.
     DISPLAY W_Cta.     
     DISPLAY W_NomPPTO.
  END.    
  IF NOT Presupuesto.Id_Cencostos AND W_CenCostos > 0 THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 343, OUTPUT W_Eleccion).
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_OfiPpto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OfiPpto W-PptCon
ON LEAVE OF W_OfiPpto IN FRAME F_PptCon /* agencia */
DO:
  DEFINE VAR W_OfiNom     AS CHAR FORMAT "X(25)" INITIAL "".
  ASSIGN W_OfiPpto.
  W_WidSel = LAST-EVENT:WIDGET-ENTER.
  IF W_WidSel:NAME = "Btn_Salir" THEN
     RETURN.
  DO WITH FRAME F_PptCon:
    IF W_OfiPpto NE 0 THEN DO:
      FIND agencias WHERE agencias.agencia = W_OfiPpto
                      AND agencias.Fec_Retiro EQ ? 
                  NO-LOCK NO-ERROR.
      IF AVAILABLE(agencias) THEN DO:
        w_Nomagencia = agencias.Nombre.
        DISPLAY agencias.Nombre @ W_Nomagencia.
      END.
      ELSE DO:
        RUN P-BrwOfi.r (OUTPUT W_OfiPpto, OUTPUT W_OfiNom).
        IF W_OfiPpto EQ 0 THEN DO:
          APPLY "ENTRY" TO W_OfiPpto.
          RETURN NO-APPLY.
        END.
        ASSIGN W_Nomagencia = W_OfiNom.
        DISPLAY W_OfiNom @ W_Nomagencia.
        DISPLAY W_OfiPpto.
       END.
     END.
    ELSE DO:
      w_Nomagencia = "Consolidada".
      DISPLAY "Consolidada." @ W_Nomagencia. 
    END.
  END.
  RUN Consultaragencia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Temp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-PptCon 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN Inicializar-Variables.
  RUN enable_UI.
  W_OfiPpto = W_agencia.
DO WITH FRAME F_pptCon:
  
  DISPLAY W_OfiPpto.
  ASSIGN W_CenIni  = 000
         W_CenFin  = 999
         W_NomCcos = "Consolidado"
         W_Ano     = YEAR(W_Fecha).
  DISPLAY W_CenIni @ W_CenCostos.
  DISPLAY "Consolidado" @ W_NomCcos.
  DISPLAY W_Ano.
  RUN Selec_Mes.
 END.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Acumular-Presupuesto W-PptCon 
PROCEDURE Acumular-Presupuesto :
/*------------------------------------------------------------------------------
  Objetivo:     Halla el saldo y acumula (presupuesto)
------------------------------------------------------------------------------*/
  ASSIGN W_TotPpto = W_TotPpto + Sal_presupuesto.Sdo_final
         W_Pro     = W_Pro + 1.
  DISPLAY W_Pro WITH FRAME F_PptCon.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar-Variables W-PptCon 
PROCEDURE Asignar-Variables :
/*------------------------------------------------------------------------------
  Objetivo:     Asigna al vector el resultado de los meses (logicos) ademas
                asigna la agencia ,centro de costos y el contador de registros
                procesados.
------------------------------------------------------------------------------*/
  ASSIGN W_Mes[1]  = T_Ene
         W_Mes[2]  = T_Feb
         W_Mes[3]  = T_Mar
         W_Mes[4]  = T_Abr
         W_Mes[5]  = T_May
         W_Mes[6]  = T_Jun
         W_Mes[7]  = T_Jul
         W_Mes[8]  = T_Ago
         W_Mes[9]  = T_Sep
         W_Mes[10] = T_Oct
         W_Mes[11] = T_Nov
         W_Mes[12] = T_Dic
         W_Pro     = 0.
  W_Seleccion = FALSE.
  DO Cont = 1 TO 12:
    IF W_Mes[cont] = TRUE THEN
      W_Seleccion = TRUE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Consultaragencia W-PptCon 
PROCEDURE Consultaragencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME F_PptCon:
  IF W_OfiPpto NE 0 THEN
    ASSIGN W_OfiIni              = W_OfiPpto
           W_OfiFin              = W_Ofippto
           W_CenCostos:SENSITIVE = TRUE
           W_CenCostos:BGCOLOR   = ?
           W_CenCostos:FGCOLOR   = ?.     
  ELSE DO:
        ASSIGN W_CenCostos:SENSITIVE = FALSE
               W_CenCostos:BGCOLOR   = 3
               W_CenCostos:FGCOLOR   = 15
               W_CenCostos           = 0.
        DISPLAY W_CenCostos.
        
        FIND FIRST agencias WHERE agencias.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(agencias) THEN
           W_OfiIni = agencias.agencia.
        FIND LAST agencias WHERE agencias.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(agencias) THEN
           W_OfiFin = agencias.agencia.
        APPLY "ENTRY" TO W_CenCostos.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConsultarCcostos W-PptCon 
PROCEDURE ConsultarCcostos :
/*------------------------------------------------------------------------------
  Purpose: Busca el primer y ultimo centro de costos, asignado a las respectivas
           vbles de rango    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME F_PptCon:
  IF W_CenCostos NE 0 THEN
    ASSIGN W_CenIni              = W_CenCostos
           W_CenFin              = W_CenCostos.
  ELSE DO:        
    FIND FIRST Cen_Costos WHERE Cen_Costos.agencia    EQ W_OfiPpto
                            AND Cen_Costos.Estado     EQ 1
                            AND Cen_Costos.Cen_Costos EQ W_CenCostos 
                        NO-LOCK NO-ERROR.
    IF AVAILABLE(Cen_Costos) THEN
      W_CenIni = Cen_Costos.Cen_Costos.
    FIND LAST Cen_Costos  WHERE Cen_Costos.agencia    EQ W_OfiPpto
                            AND Cen_Costos.Estado     EQ 1
                            AND Cen_Costos.Cen_Costos EQ W_CenCostos 
                        NO-LOCK NO-ERROR.
    IF AVAILABLE(Cen_Costos) THEN
      W_CenFin = Cen_Costos.Cen_Costos.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-PptCon  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-PptCon)
  THEN DELETE WIDGET W-PptCon.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-PptCon  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY W_OfiPpto W_Nomagencia W_Cencostos W_NomCcos W_Ano T_Ene T_Feb T_Mar 
          T_Abr T_May T_Jun T_Jul T_Ago T_Sep T_Oct T_Nov T_Dic T_ConCta 
      WITH FRAME F_PptCon IN WINDOW W-PptCon.
  ENABLE BUTTON-69 W_OfiPpto W_Cencostos W_Ano Btn_Imprimir Btn_VerCta T_ConCta 
         Btn_Generar Temp Btn_Nueva Btn-Salir BUTTON-68 RECT-122 RECT-123 
         RECT-42 RECT-43 
      WITH FRAME F_PptCon IN WINDOW W-PptCon.
  {&OPEN-BROWSERS-IN-QUERY-F_PptCon}
  VIEW W-PptCon.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar-Temporal W-PptCon 
PROCEDURE Generar-Temporal :
/*------------------------------------------------------------------------------
  Objetivo: Proceso cuando se escoge un centro de costos. Especifico.
------------------------------------------------------------------------------*/    
  IF W_Cencostos = 0 THEN
    RUN Generar_ConsoliCenCos.
  ELSE DO:
    FOR EACH Presupuesto  WHERE Presupuesto.Estado   EQ 1 
                            AND Presupuesto.Id_Cencostos EQ TRUE 
                        NO-LOCK:
      FOR EACH agencias   WHERE ( agencias.agencia  GE W_OfiIni 
                            AND   agencias.agencia  LE W_OfiFin )
                            AND agencias.Estado = 1
                        NO-LOCK:
          ASSIGN W_Ofi      = agencias.agencia
                 W_Cco      = W_CenCostos
                 W_TotPpto  = 0
                 W_TotMvto  = 0
                 W_SiHaySdo = FALSE.

          RUN Hallar_SdoPpto.
  
          IF W_SiHaySdo THEN DO:
            RUN LLenarTemporal.
            RUN GrabarTempo.
          END.
  
      END. /*End de agencias */
    END.   /* End de Ppto. */
  END.     /* End del if */
  OPEN QUERY Temp FOR EACH Tmp NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar-Temporal-Consolidado W-PptCon 
PROCEDURE Generar-Temporal-Consolidado :
/*------------------------------------------------------------------------------
  Objetivo: Saca la consulta consolidada por los parametros de consolidacion por
            cuenta, centro de costos.
------------------------------------------------------------------------------*/
  IF W_CenCostos = 0 THEN
    RUN Generar_ConsoliCta.
  ELSE DO:  
    FOR EACH Presupuesto  WHERE Presupuesto.Estado          EQ 1 
                            AND Presupuesto.Cod_Presupuesto EQ W_Cta
                            AND Presupuesto.Id_Cencostos    EQ TRUE 
                        NO-LOCK:
      FOR EACH agencias   WHERE ( agencias.agencia  GE W_OfiIni 
                            AND   agencias.agencia  LE W_OfiFin )
                            AND agencias.Estado = 1
                        NO-LOCK:
          ASSIGN W_Ofi     = agencias.agencia
                 W_Cco     = W_CenCostos
                 W_TotPpto = 0
                 W_TotMvto = 0
                 W_SiHaySdo = FALSE.

          RUN Hallar_SdoPpto.
  
          IF W_SiHaySdo THEN DO:
            RUN LLenarTemporal.
            RUN GrabarTempo.
          END.
  
      END. /*End de agencias */
    END.   /* End de Ppto. */
  END.     /* End del if */
  OPEN QUERY Temp FOR EACH Tmp NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_ConsoliCencos W-PptCon 
PROCEDURE Generar_ConsoliCencos :
/*------------------------------------------------------------------------------
  Purpose: genera el proceso consolidado por centro de costos     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH Presupuesto  WHERE Presupuesto.Estado   EQ 1 NO-LOCK:
    FOR EACH agencias   WHERE ( agencias.agencia  GE W_OfiIni 
                          AND   agencias.agencia  LE W_OfiFin )
                          AND agencias.Estado = 1
                      NO-LOCK:
        ASSIGN W_Ofi      = agencias.agencia
               W_TotPpto  = 0
               W_TotMvto  = 0
               W_SiHaySdo = FALSE.

      FOR EACH Cen_costos WHERE Cen_Costos.agencia EQ W_Ofi
                            AND Cen_costos.Estado  EQ 1 NO-LOCK:
         W_Cco = Cen_costos.Cen_Costos.                            
         RUN Hallar_SdoPpto.
      END.   /* each cen_costos */

      W_Cco = W_CenCostos.
      RUN Hallar_SdoPpto.    /* llamo nuevamente para el ccos igual a cero */
      IF W_SiHaySdo THEN DO:
         RUN LLenarTmp_Consoli.
         RUN GrabarTempo.
      END.

    END. /*End de agencias */
  END.   /* End de Ppto. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_ConsoliCta W-PptCon 
PROCEDURE Generar_ConsoliCta :
/*------------------------------------------------------------------------------
  Purpose: Consolidado por cuenta y centro de costos.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH Presupuesto  WHERE Presupuesto.Estado          EQ 1 
                          AND Presupuesto.Cod_Presupuesto EQ W_Cta
                      NO-LOCK:
    FOR EACH agencias   WHERE ( agencias.agencia  GE W_OfiIni 
                          AND   agencias.agencia  LE W_OfiFin )
                          AND agencias.Estado = 1
                      NO-LOCK:
        ASSIGN W_Ofi      = agencias.agencia
               W_TotPpto  = 0
               W_TotMvto  = 0
               W_SiHaySdo = FALSE.

      FOR EACH Cen_costos WHERE Cen_Costos.agencia EQ W_Ofi
                            AND Cen_costos.Estado  EQ 1 NO-LOCK:
         W_Cco = Cen_costos.Cen_Costos.                            
         RUN Hallar_SdoPpto.
      END.   /* each cen_costos */

      W_Cco = W_CenCostos.
      RUN Hallar_SdoPpto.    /* llamo nuevamente para el ccos igual a cero */
      IF W_SiHaySdo THEN DO:
         RUN LLenarTmp_Consoli.
         RUN GrabarTempo.
      END.

    END. /*End de agencias */
  END.   /* End de Ppto. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabarTempo W-PptCon 
PROCEDURE GrabarTempo :
/*------------------------------------------------------------------------------
  Purpose: graba informacion al temporal, bien sea porque se consulte consolidado por
       centro de costos o no.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND Tmp WHERE Tmp.W_CCosto EQ W_Cco
               AND Tmp.W_Cuenta EQ Presupuesto.Cod_Presupuesto
          NO-ERROR.
    IF NOT AVAILABLE(Tmp) THEN DO:
      CREATE Tmp.
      ASSIGN Tmp.W_CCosto = W_Cco
             Tmp.W_Cuenta = Presupuesto.Cod_Presupuesto
             Tmp.W_Nombre = Presupuesto.Nombre
             Tmp.W_Ppto   = W_TotPpto
             Tmp.W_Mvto   = W_TotMvto
             Tmp.W_PDif   = (W_TotMvto * 100) / W_TotPpto
             Tmp.W_VDif   = ABS(W_TotPpto - W_TotMvto).
    END.
    ELSE
      ASSIGN Tmp.W_Ppto   = Tmp.W_Ppto + W_TotPpto
             Tmp.W_Mvto   = Tmp.W_Mvto + W_TotMvto
             Tmp.W_PDif   = (Tmp.W_Mvto * 100) / Tmp.W_Ppto
             Tmp.W_VDif   = ABS(Tmp.W_Ppto - Tmp.W_Mvto).
    IF W_totPPto = 0  THEN
      Tmp.W_PDif = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HabilitarMes W-PptCon 
PROCEDURE HabilitarMes :
/*------------------------------------------------------------------------------
  Purpose: habilita y deshabilita los campos de meses
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF W_Ano = YEAR(W_Fecha) THEN
    W_MesCont = MONTH(W_Fecha).
  ELSE
    W_MesCont = 12.
        
    DO Cont = 1 TO 12 BY 1:
      IF Cont LE W_MesCont THEN DO WITH FRAME F_PPTCON:
        CASE Cont:
             WHEN 1 THEN 
                   ENABLE T_Ene.
             WHEN 2 THEN
                   ENABLE T_Feb.
             WHEN 3 THEN
                   ENABLE T_Mar.
             WHEN 4 THEN
                   ENABLE T_Abr.
             WHEN 5 THEN
                   ENABLE T_May.
             WHEN 6 THEN
                   ENABLE T_Jun.
             WHEN 7 THEN
                   ENABLE T_Jul.
             WHEN 8 THEN
                   ENABLE T_Ago.
             WHEN 9 THEN
                   ENABLE T_Sep.
             WHEN 10 THEN
                   ENABLE T_Oct.
             WHEN 11 THEN
                   ENABLE T_Nov.
             WHEN 12 THEN
                   ENABLE T_Dic.
        END CASE.
      END.
      ELSE DO WITH FRAME F_PPTCON:
        CASE Cont:
             WHEN 1 THEN 
                   DISABLE T_Ene.
             WHEN 2 THEN
                   DISABLE T_Feb.
             WHEN 3 THEN
                   DISABLE T_Mar.
             WHEN 4 THEN
                   DISABLE T_Abr.
             WHEN 5 THEN 
                   DISABLE T_May.
             WHEN 6 THEN
                   DISABLE T_Jun.
             WHEN 7 THEN
                   DISABLE T_Jul.
             WHEN 8 THEN
                   DISABLE T_Ago.
             WHEN 9 THEN
                   DISABLE T_Sep.
             WHEN 10 THEN
                   DISABLE T_Oct.
             WHEN 11 THEN
                   DISABLE T_Nov.
             WHEN 12 THEN
                   DISABLE T_Dic.
        END CASE.
      END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_SdoPpto W-PptCon 
PROCEDURE Hallar_SdoPpto :
/*------------------------------------------------------------------------------
  Purpose: busca el saldo de presupuesto para una agencia y un centro de costos.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Sal_Presupuesto WHERE Sal_Presupuesto.agencia         EQ W_Ofi
                               AND Sal_Presupuesto.Ano             EQ W_Ano
                               AND Sal_Presupuesto.Cen_Costos      EQ W_Cco
                               AND Sal_Presupuesto.Cod_Presupuesto EQ Presupuesto.Cod_Presupuesto
                           NO-LOCK:  
      ASSIGN W_SiHaySdo = TRUE
             W_HayInf   = TRUE.
      DO Cont = 1 TO 12 BY 1:
       IF W_Mes[Cont] = TRUE AND Sal_Presupuesto.Mes = Cont THEN DO:
         RUN Acumular-Presupuesto.
         W_MesUlt = Cont.
       END.
      END.
    END.  /* each sal_ppto */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar-Variables W-PptCon 
PROCEDURE Inicializar-Variables :
/*------------------------------------------------------------------------------
  Objetivo:     Llenar las variables de pantalla como: browsers, selectio-list
                combos, etc.
------------------------------------------------------------------------------*/  
  FIND agencias WHERE agencias.agencia = W_agencia NO-LOCK NO-ERROR.
  IF AVAILABLE(agencias) THEN
    DO:
      W_OfiPpto = W_agencia.
      DISPLAY agencias.Nombre @ W_Nomagencia WITH FRAME F_PptCon.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE llenarTemporal W-PptCon 
PROCEDURE llenarTemporal :
/*------------------------------------------------------------------------------
  Purpose: Busca el valor de saldos unicamente para un centro de costos (especifico).    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    FOR EACH Cuentas WHERE Cuentas.Tipo            EQ 2
                       AND Cuentas.Id_Presupuesto  EQ TRUE 
                       AND Cuentas.Cod_Presupuesto EQ Presupuesto.Cod_Presupuesto 
                       AND Cuentas.Estado          EQ 1
                     /*  AND Cuentas.Id_CenCostos    EQ TRUE*/
                   NO-LOCK:
       FIND Sal_Cuenta WHERE Sal_Cuenta.agencia    EQ W_Ofi
                            AND Sal_Cuenta.Cen_Costos EQ W_Cco
                            AND Sal_Cuenta.Cuenta     EQ Cuentas.Cuenta 
                            AND Sal_Cuenta.Ano        EQ W_Ano 
                            NO-LOCK NO-ERROR.
       MESSAGE W_ofi W_cco cuentas.cuenta w_ano cont VIEW-AS ALERT-BOX.   
       IF AVAILABLE(Sal_Cuenta) THEN DO:
             DO Cont = 1 TO 12 BY 1:
                IF W_Mes[Cont] = TRUE THEN DO:
                   IF Cuentas.Naturaleza = "DB" THEN
                      W_TotMvto = W_TotMvto + (Sal_Cuenta.Db[Cont]- Sal_Cuenta.Cr[Cont]).
                   ELSE
                      W_TotMvto = W_TotMvto + (Sal_Cuenta.Cr[Cont] - Sal_Cuenta.Db[Cont]).
                   W_Pro     = W_Pro + 1.
                   DISPLAY W_Pro WITH FRAME F_PptCon.
                END.
             END. /* End del if interno */
          END.  /* end del primer if */
    END.  /* Each cuentas */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LlenarTmp_Consoli W-PptCon 
PROCEDURE LlenarTmp_Consoli :
/*------------------------------------------------------------------------------
  Purpose:  llena el temporal consolidado, no importa el centro de costos   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 

    FOR EACH Cuentas WHERE Cuentas.Tipo            EQ 2
                       AND Cuentas.Id_Presupuesto  EQ TRUE 
                       AND Cuentas.Cod_Presupuesto EQ Presupuesto.Cod_Presupuesto 
                       AND Cuentas.Estado          EQ 1
                   NO-LOCK:
      FOR EACH Cen_Costos WHERE Cen_Costos.agencia EQ W_Ofi
                            AND Cen_costos.Estado  EQ 1 NO-LOCK:
       
            FIND Sal_Cuenta WHERE Sal_Cuenta.agencia    EQ W_Ofi
                              AND Sal_Cuenta.Cuenta     EQ Cuentas.Cuenta 
                              AND Sal_Cuenta.Cen_Costos EQ Cen_costos.Cen_costos
                              AND Sal_Cuenta.Ano        EQ W_Ano 
                          NO-LOCK NO-ERROR.
             MESSAGE W_ofi W_cco cuentas.cuenta w_ano cont VIEW-AS ALERT-BOX.
            IF AVAILABLE(Sal_Cuenta) THEN DO:
               DO Cont = 1 TO 12 BY 1:
                  IF W_Mes[Cont] = TRUE THEN DO:
                     IF Cuentas.Naturaleza = "DB" THEN
                         W_TotMvto = W_TotMvto + (Sal_Cuenta.Db[Cont] - Sal_Cuenta.Cr[Cont]).
                     ELSE
                         W_TotMvto = W_TotMvto + (Sal_Cuenta.Cr[Cont] - Sal_Cuenta.Db[Cont]).
                     W_Pro     = W_Pro + 1.
                     DISPLAY W_Pro WITH FRAME F_PptCon.
                  END.
               END. /* End del do */
            END.
      END. /*each centro de costos */
    END.  /* Each cuentas */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-PptCon 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Objetivo:     Imprimir el informe de presupuesto tomando las caracteristcas
                de la consulta
------------------------------------------------------------------------------*/
  DEFINE VAR W_Rpta         AS LOGICAL.
  DEFINE VAR W_TotPpt       AS DECIMAL FORMAT ">>>,>>>,>>>,>99.99".
  DEFINE VAR W_TotEje       AS DECIMAL FORMAT ">>>,>>>,>>>,>99.99".
  DEFINE VAR W_TotDif       AS DECIMAL FORMAT ">>>,>>>,>>>,>99.99".
  DEFINE VAR W_TitConsolida AS CHARACTER FORMAT "X(18)" INITIAL "".
  DEFINE VAR W_CtaConsolida AS CHARACTER FORMAT "X(14)" INITIAL "".
  DEFINE VAR W_Nommes       AS CHARACTER FORMAT "X(100)".
  DEFINE VAR J              AS INTEGER   FORMAT "99" INITIAL 1.

  DEFINE FRAME f-encabezado
    HEADER
      W_Nom_Entidad                AT 30
      "PAGINA:"                    AT 104 PAGE-NUMBER FORMAT ">>>9"
      "CONSULTA DE PRESUPUESTO"    AT 44 SKIP(1)
      "agencia:"                   AT 1
      W_Nomagencia                 AT 20
      "Año de Consulta: "          AT 1
      W_Ano                        AT 20
      "Centro de Costos: "         AT 1
      w_NomCcos                    AT 20
      w_TitConsolida               AT 1
      w_CtaConsolida               AT 20
      "Meses: "                    AT 1
      W_NomMes                     AT 8
      Linea no-label               AT 1 FORMAT "X(117)" SKIP
    WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME f-encabezado STREAM-IO.

  DEFINE FRAME F_Presupuesto
     Tmp.W_CCosto
     Tmp.W_Cuenta
     Tmp.W_Nombre
     Tmp.W_Ppto
     Tmp.W_Mvto
     Tmp.W_PDif
     Tmp.W_VDif SKIP
    WITH  WIDTH 132 FRAME F_Presupuesto USE-TEXT STREAM-IO font 2.

  DEFINE FRAME f-Tot
    WITH WIDTH 200 NO-LABELS.

  DEFINE FRAME f-PiePagina
    HEADER 
      "FECHA:"                  AT 2 
      TODAY                     AT 10 FORMAT "99/99/9999"
      W_Nom_agencia             AT 38 FORMAT "X(40)"
      "HORA:"                   AT 104 STRING(TIME,"HH:MM:SS")
  WITH WIDTH 132 FRAME F-Piepagina PAGE-BOTTOM USE-TEXT STREAM-IO. 

  IF T_ConCta THEN
    ASSIGN W_TitConsolida = "Consolidada x Cta."
           W_CtaConsolida = W_Cta.
  
  DO Cont = 1 TO 12 BY 1:
    IF W_Mes[Cont] = TRUE THEN DO:
      CASE Cont:
        WHEN  1 THEN W_NomMes = W_NomMes + "Ene ".
        WHEN  2 THEN W_NomMes = W_NomMes + "Feb ".
        WHEN  3 THEN W_NomMes = W_NomMes + "Mar ".
        WHEN  4 THEN W_NomMes = W_NomMes + "Abr ".
        WHEN  5 THEN W_NomMes = W_NomMes + "May ".
        WHEN  6 THEN W_NomMes = W_NomMes + "Jun ".
        WHEN  7 THEN W_NomMes = W_NomMes + "Jul ".
        WHEN  8 THEN W_NomMes = W_NomMes + "Ago ".
        WHEN 9  THEN W_NomMes = W_NomMes + "Sep ".
        WHEN 10 THEN W_NomMes = W_NomMes + "Oct ".
        WHEN 11 THEN W_NomMes = W_NomMes + "Nov ".
        WHEN 12 THEN W_NomMes = W_NomMes + "Dic ".
      END.
      J = J + 1.
    END.
  END.
  
  LINEA = FILL(Raya,117).
  VIEW FRAME f-encabezado.
  VIEW FRAME f-piepagina.
  FOR EACH Tmp NO-LOCK:
     W_TotPpt = W_TotPpt + Tmp.W_Ppto.
     W_TotEje = W_TotEje + Tmp.W_Mvto.
     W_TotDif = W_TotDif + Tmp.W_VDif.
     DISPLAY Tmp
     WITH WIDTH 132 USE-TEXT STREAM-IO font 2 NO-BOX.      
  END.
  DISPLAY " "           AT   1   SKIP(1)
          "Totales :"   AT  15
          W_TotPpt      AT  49   NO-LABEL
          W_TotEje      AT  71   NO-LABEL
          W_TotDif      AT  100  NO-LABEL
     WITH FRAME F_Tot WIDTH 132 USE-TEXT STREAM-IO font 2 NO-BOX.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Selec_Mes W-PptCon 
PROCEDURE Selec_Mes :
/*------------------------------------------------------------------------------
  Purpose: selecciona el mes en que se encuentra W_Fecha como parametro de consulta,
           además habilita los meses transcurridos hasta el mes de w_fecha siempre y 
           cuando el año a consultar sea igual al año de W_fecha, de lo contrario habilita
           todos los meses.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME F_pptCon:
    CASE MONTH(W_Fecha):
       WHEN 1 THEN
         ASSIGN T_Ene = TRUE
                T_Ene:CHECKED = TRUE.
       WHEN 2 THEN
         ASSIGN T_Feb = TRUE
                T_Feb:CHECKED = TRUE.
       WHEN 3 THEN
         ASSIGN T_Mar = TRUE
                T_Mar:CHECKED = TRUE.
       WHEN 4 THEN
         ASSIGN T_Abr = TRUE
                T_abr:CHECKED = true.
       WHEN 5 THEN
         ASSIGN T_May = TRUE
                T_May:CHECKED = TRUE.
       WHEN 6 THEN
         ASSIGN T_Jun = TRUE
                T_Jun:CHECKED = TRUE.
       WHEN 7 THEN
         ASSIGN T_Jul = TRUE
                T_Jul:CHECKED = TRUE.
       WHEN 8 THEN
         ASSIGN T_Ago = TRUE
                T_Ago:CHECKED = TRUE.    
       WHEN 9 THEN
         ASSIGN T_Sep = TRUE
                T_Sep:CHECKED = TRUE.
       WHEN 10 THEN
         ASSIGN T_Oct = TRUE
                T_Oct:CHECKED = TRUE.
       WHEN 11 THEN
         ASSIGN T_Nov = TRUE
                T_Nov:CHECKED = TRUE.
       WHEN 12 THEN
         ASSIGN T_Dic = TRUE
                T_Dic:CHECKED = TRUE.
    END CASE.
    RUN HabilitarMes.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

