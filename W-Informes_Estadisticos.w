&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  {Incluido/Variable.I "SHARED"}
  {Incluido/VARCON.I "SHARED"}

  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR i AS INTEGER.
  
  DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
  DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
  DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
  DEFINE VARIABLE WFec     AS DATE.   
  DEFINE VARIABLE FecIni   AS DATE FORMAT "99/99/9999".
  DEFINE VARIABLE FecFin   AS DATE FORMAT "99/99/9999".
  DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
  DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".
  DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE ProIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE ProFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE TpdIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE TpdFin   AS INTEGER FORMAT "999" INITIAL 9.
  DEFINE VAR Minimo AS DECIMAL FORMAT ">,>>>,>>9".
  DEFINE VARIABLE EstadoC  AS INTEGER FORMAT "9".

  DEFINE VAR Listado AS CHARACTER INITIAL "".

 DEFINE VAR j AS DECIMAL.
 DEFINE VAR k AS INTEGER.

DEFINE VAR W_Nin AS CHARACTER FORMAT "X(61)".
DEFINE VAR W_Vig AS INTEGER FORMAT "9999".
DEFINE VAR W_Nus AS CHARACTER FORMAT "X(20)".

DEFINE VAR W_Compromiso AS CHARACTER FORMAT "X(2)".
DEFINE VAR W_Interes AS DECIMAL FORMAT "->>>,>>>,>>9".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR W_TipPdt AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_Vigen  AS INTEGER FORMAT "9999".
DEFINE VAR W_ForPag AS CHARACTER FORMAT "X(10)".

DEFINE VAR Tot1     AS DECIMAL  FORMAT ">>>,>>>,>>>,>>>".
DEFINE VAR Tot2     AS DECIMAL  FORMAT ">>>,>>>,>>>,>>>".
DEFINE VAR Tot3     AS DECIMAL  FORMAT ">>>,>>>,>>>,>>>".
DEFINE VAR Tot4     AS DECIMAL  FORMAT ">>>,>>>,>>>,>>>".
DEFINE VAR Tot5     AS DECIMAL  FORMAT ">>>,>>>,>>>,>>>".
DEFINE VAR Tot6     AS DECIMAL  FORMAT ">>>,>>>,>>>,>>>".
DEFINE VAR Tot7     AS DECIMAL  FORMAT ">>>,>>>,>>>,>>>".
DEFINE VAR Tot8     AS DECIMAL  FORMAT ">>>,>>>,>>>,>>>".
DEFINE VAR Tot9     AS DECIMAL  FORMAT ">>>,>>>,>>>,>>>".
DEFINE VAR Tot10     AS DECIMAL  FORMAT ">>>,>>>,>>>,>>>".

/*totales filas*/
DEFINE VAR LTot1     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR LTot2     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR LTot3     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR LTot4     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR LTot5     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR LTot6     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR LTot7     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR LTot8     AS DECIMAL  FORMAT ">>>>,>>>,>>>".

/*totales columnas*/
DEFINE VAR TTot1     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR TTot2     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR TTot3     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR TTot4     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR TTot5     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR TTot6     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR TTot7     AS DECIMAL  FORMAT ">>>>,>>>,>>>".
DEFINE VAR TTot8     AS DECIMAL  FORMAT ">>>>,>>>,>>>".

DEFINE TEMP-TABLE Tseg
FIELD TCodGru LIKE Clientes.Grupo
FIELD TCodSub LIKE Clientes.Subgrupo
FIELD TCodCod LIKE Clientes.Codigo_Ciiu
FIELD TNroEst AS INTEGER FORMAT ">>,>>9"
FIELD TValEst AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
FIELD TNroApr AS INTEGER FORMAT ">>,>>9"
FIELD TValApr AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
FIELD TNroNeg AS INTEGER FORMAT ">>,>>9"
FIELD TValNeg AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
FIELD TNroCon AS INTEGER FORMAT ">>,>>9"
FIELD TValCon AS DECIMAL FORMAT ">>,>>>,>>>,>>9".

DEFINE TEMP-TABLE Tmp_TotAho
 FIELD A_Agencia LIKE Agencias.Agencia
 FIELD A_NomAge  LIKE Agencias.Nombre
 FIELD Val_Hombres AS DECIMAL FORMAT ">>>,>>>,>>>,>>9" EXTENT 8
 FIELD Val_Mujeres AS DECIMAL FORMAT ">>>,>>>,>>>,>>9" EXTENT 8
 FIELD Con_Mujeres AS DECIMAL FORMAT ">>>,>>>,>>>,>>9" EXTENT 8
 FIELD Con_Hombres AS DECIMAL FORMAT ">>>,>>>,>>>,>>9" EXTENT 8.
    
DEFINE TEMP-TABLE Yaportes
        FIELD Nit LIKE Clientes.Nit.

DEFI TEMP-TABLE TCorLar
     FIELD Age LIKE Agencias.Agencia
     FIELD Pto LIKE Creditos.Cod_credito
     FIELD Nom LIKE Pro_Creditos.Nom_Prod
     FIELD VCor LIKE Creditos.Sdo_Capital INIT 0
     FIELD Vlar LIKE Creditos.Sdo_Capital INIT 0.

DEFI TEMP-TABLE TTCreditos LIKE Creditos
     FIELD DM  AS INTEG FORM "-9999" INIT 0
     FIELD NM  AS CHAR  FORM "X(40)" INIT "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Basicos

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Tipo Cmb_BCreditos Cmb_BUsuarios ~
Cmb_BClientes BUTTON-143 BUTTON-153 BUTTON-144 BUTTON-149 E1 BUTTON-148 ~
BUTTON-150 BUTTON-152 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Tipo Cmb_BCreditos Cmb_BUsuarios ~
Cmb_BClientes E1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Cmb_TProducto Cmb_Productos TTotales ValIni ValFin ~
UsuIni UsuFin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-144 
     LABEL "Cambiar los Rangos de Filtro de Información" 
     SIZE 40 BY 1.12.

DEFINE BUTTON BUTTON-148 
     LABEL "Ejecutar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-149 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-150 
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-152 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 152" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 153" 
     SIZE 7 BY 1.65.

DEFINE VARIABLE Cmb_BAhorros AS CHARACTER FORMAT "X(256)":U INITIAL "Básico" 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Básico","Fechas de Apertura","Fechas de Cancelación","Fechas de Vencimiento","Canje","Sexo","Edades","Estado Civil","Nivel Educativo","Salario" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BClientes AS CHARACTER FORMAT "X(256)":U INITIAL "Básico" 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Clientes por Agencia","Afiliaciones","Retiros","Habiles","No Habiles","Sexo","Edades","Estado Civil","Nivel Educativo","Salario","Tipos de Cliente" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BCreditos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Cartera","Indice de Morosidad","Por Dias-Mora","Por Corto/Largo Plazo","Por Estados","Por Instancias","Sexo","Edades","Estado Civil","Nivel Educativo","Salario","Estadisticos Acumulados" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BGarantias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "Garantias Admisibles","Vencimientos Poliza","Vencimiento Impuestos","Cupos" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BSolicitudes AS CHARACTER FORMAT "X(256)":U 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Básico","Fechas de Ingreso" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_BUsuarios AS CHARACTER FORMAT "X(256)":U 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Pendientes de Cobro" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Tipo AS CHARACTER FORMAT "X(256)":U INITIAL "Clientes" 
     LABEL "Tipo de Informe" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Clientes","Ahorros","Créditos","Solicitudes","Garantias","Especiales","Usuarios" 
     DROP-DOWN-LIST
     SIZE 18 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE E1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 96 BY 17.5
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_anterior 
     LABEL "Anterior" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_siguiente 
     LABEL "Siguiente" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-154 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 154" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(20)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE BUTTON Btn_FecFin 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_FecIni 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-145 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 145" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_EstCre AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estado del Crédito" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 - Aprobado No Desembolsado","2 - Desembolsado Normal","3 - Cancelado","4 - Retirado Sin Aprobar","5 - Castigado" 
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todos" 
     LABEL "Productos Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todos" 
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_TProducto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipos de Producto" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE UsuFin AS CHARACTER FORMAT "X(4)":U 
     LABEL "Usuario Final" 
     VIEW-AS FILL-IN 
     SIZE 15.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE UsuIni AS CHARACTER FORMAT "X(4)":U 
     LABEL "Usuario Inicial" 
     VIEW-AS FILL-IN 
     SIZE 15.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ValFin AS DECIMAL FORMAT ">>>,>>>,>>9.999":U INITIAL 999999999 
     LABEL "Valor Final" 
     VIEW-AS FILL-IN 
     SIZE 15.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ValIni AS DECIMAL FORMAT "->>>,>>>,>>9.999":U INITIAL 0 
     LABEL "Valor Inicial" 
     VIEW-AS FILL-IN 
     SIZE 15.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaFin AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE W_DiaIni AS DECIMAL FORMAT "99":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE IMAGE Img_MesF
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE IMAGE Img_MesI
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE VARIABLE RFpago AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Caja", 1,
"Nomina", 2,
"Todos", 3
     SIZE 12 BY 2.15 NO-UNDO.

DEFINE VARIABLE RRFechas AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Toda la Información", 1,
"Rangos de Fechas", 2
     SIZE 42 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 1.88.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 1.88.

DEFINE VARIABLE TTotales AS LOGICAL INITIAL no 
     LABEL "Solo Totales?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .77 NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "imagenes/clock05.ico":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R5
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R7
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R8
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R9
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Basicos
     Cmb_Tipo AT ROW 1.54 COL 16 COLON-ALIGNED
     Cmb_BCreditos AT ROW 1.54 COL 54 COLON-ALIGNED
     Cmb_BAhorros AT ROW 1.54 COL 54 COLON-ALIGNED
     Cmb_BGarantias AT ROW 1.54 COL 54 COLON-ALIGNED
     Cmb_BSolicitudes AT ROW 1.54 COL 54 COLON-ALIGNED
     Cmb_BUsuarios AT ROW 1.54 COL 54 COLON-ALIGNED
     Cmb_BClientes AT ROW 1.54 COL 54 COLON-ALIGNED
     BUTTON-143 AT ROW 1.54 COL 102
     BUTTON-153 AT ROW 2.88 COL 3
     BUTTON-144 AT ROW 2.88 COL 56
     BUTTON-149 AT ROW 3.15 COL 102
     E1 AT ROW 4.5 COL 3 NO-LABEL
     BUTTON-148 AT ROW 4.77 COL 102
     BUTTON-150 AT ROW 19.04 COL 102
     BUTTON-152 AT ROW 20.92 COL 105
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 21.38
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Progreso
     IMAGE-2 AT ROW 1.27 COL 4.57
     R1 AT ROW 6.92 COL 3.57
     R2 AT ROW 6.38 COL 3.57
     R3 AT ROW 5.85 COL 3.57
     R4 AT ROW 5.31 COL 3.57
     R5 AT ROW 4.77 COL 3.57
     R6 AT ROW 4.23 COL 3.57
     R7 AT ROW 3.69 COL 3.57
     R8 AT ROW 3.15 COL 3.57
     R9 AT ROW 2.62 COL 3.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 101 ROW 6.65
         SIZE 12 BY 6.73
         BGCOLOR 17 .

DEFINE FRAME F_Filtros
     Cmb_Agencias AT ROW 1.27 COL 9 COLON-ALIGNED
     RRFechas AT ROW 3.42 COL 12 NO-LABEL
     Btn_FecFin AT ROW 5.65 COL 60
     W_DiaIni AT ROW 5.69 COL 11 COLON-ALIGNED NO-LABEL
     AnoIni AT ROW 5.69 COL 26 COLON-ALIGNED NO-LABEL
     Btn_FecIni AT ROW 5.69 COL 34
     W_DiaFin AT ROW 5.69 COL 39 NO-LABEL
     AnoFin AT ROW 5.69 COL 52 COLON-ALIGNED NO-LABEL
     Cmb_TProducto AT ROW 6.92 COL 25 COLON-ALIGNED
     Cmb_Productos AT ROW 8 COL 25 COLON-ALIGNED
     Cmb_EstCre AT ROW 9.08 COL 25 COLON-ALIGNED
     TTotales AT ROW 10.15 COL 48
     ValIni AT ROW 11.23 COL 46 COLON-ALIGNED
     ValFin AT ROW 12.31 COL 46 COLON-ALIGNED
     UsuIni AT ROW 13.38 COL 46 COLON-ALIGNED
     UsuFin AT ROW 14.46 COL 46 COLON-ALIGNED
     RFpago AT ROW 14.73 COL 4 NO-LABEL
     BUTTON-145 AT ROW 15.54 COL 57
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 11 BY .69 AT ROW 4.88 COL 17
          BGCOLOR 17 FGCOLOR 7 
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 4.92 COL 43
          BGCOLOR 17 FGCOLOR 7 
     Img_MesF AT ROW 5.69 COL 43
     Img_MesI AT ROW 5.69 COL 17
     RECT-282 AT ROW 4.77 COL 12
     RECT-283 AT ROW 4.77 COL 38
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 30 ROW 4.5
         SIZE 69 BY 17.5
         BGCOLOR 17 FONT 5
         TITLE "Filtros de Información".

DEFINE FRAME F_Buscar
     Buscar AT ROW 1.27 COL 7 COLON-ALIGNED
     BUTTON-154 AT ROW 1.54 COL 45
     Btn_anterior AT ROW 2.35 COL 9
     Btn_siguiente AT ROW 2.35 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14 ROW 3.69
         SIZE 53 BY 3.5
         BGCOLOR 17 FONT 4
         TITLE "Buscar".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Modulo de Informes Estadísticos"
         HEIGHT             = 21.38
         WIDTH              = 113.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Buscar:FRAME = FRAME F_Basicos:HANDLE
       FRAME F_Filtros:FRAME = FRAME F_Basicos:HANDLE
       FRAME F_Progreso:FRAME = FRAME F_Basicos:HANDLE.

/* SETTINGS FOR FRAME F_Basicos
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Buscar:MOVE-AFTER-TAB-ITEM (BUTTON-149:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Buscar:MOVE-BEFORE-TAB-ITEM (E1:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Filtros:MOVE-AFTER-TAB-ITEM (E1:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Filtros:MOVE-BEFORE-TAB-ITEM (BUTTON-148:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-AFTER-TAB-ITEM (BUTTON-148:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-BEFORE-TAB-ITEM (BUTTON-150:HANDLE IN FRAME F_Basicos)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR COMBO-BOX Cmb_BAhorros IN FRAME F_Basicos
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_BAhorros:HIDDEN IN FRAME F_Basicos           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_BGarantias IN FRAME F_Basicos
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_BGarantias:HIDDEN IN FRAME F_Basicos           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_BSolicitudes IN FRAME F_Basicos
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_BSolicitudes:HIDDEN IN FRAME F_Basicos           = TRUE.

/* SETTINGS FOR FRAME F_Buscar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Buscar:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Filtros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Filtros:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN AnoFin IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_FecFin IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_FecIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_EstCre IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_EstCre:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Productos IN FRAME F_Filtros
   NO-ENABLE 6                                                          */
/* SETTINGS FOR COMBO-BOX Cmb_TProducto IN FRAME F_Filtros
   NO-ENABLE 6                                                          */
/* SETTINGS FOR TOGGLE-BOX TTotales IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE 6                                               */
ASSIGN 
       TTotales:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR FILL-IN UsuFin IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE 6                                               */
ASSIGN 
       UsuFin:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR FILL-IN UsuIni IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE 6                                               */
ASSIGN 
       UsuIni:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR FILL-IN ValFin IN FRAME F_Filtros
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN ValIni IN FRAME F_Filtros
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN W_DiaFin IN FRAME F_Filtros
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_DiaIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Modulo de Informes Estadísticos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Modulo de Informes Estadísticos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME Btn_anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_anterior wWin
ON CHOOSE OF Btn_anterior IN FRAME F_Buscar /* Anterior */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,34) IN FRAME F_Basicos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Btn_FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FecFin wWin
ON CHOOSE OF Btn_FecFin IN FRAME F_Filtros /* Button 121 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         W_DiaFin:SCREEN-VALUE = STRING(DAY(WFec))
         AnoFin:SCREEN-VALUE = STRING(WAno)
         AnoFin = WAno
         MesFin = WMes
         FecFin = WFec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FecIni wWin
ON CHOOSE OF Btn_FecIni IN FRAME F_Filtros /* Button 120 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         AnoIni:SCREEN-VALUE = STRING(WAno)
         W_DiaIni:SCREEN-VALUE = STRING(DAY(WFec))
         AnoIni = WAno
         MesIni = WMes
         FecIni = WFec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME Btn_siguiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_siguiente wWin
ON CHOOSE OF Btn_siguiente IN FRAME F_Buscar /* Siguiente */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,33) IN FRAME F_Basicos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME BUTTON-143
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-143 wWin
ON CHOOSE OF BUTTON-143 IN FRAME F_Basicos /* Button 143 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-144
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-144 wWin
ON CHOOSE OF BUTTON-144 IN FRAME F_Basicos /* Cambiar los Rangos de Filtro de Información */
DO:

  VIEW FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-145
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-145 wWin
ON CHOOSE OF BUTTON-145 IN FRAME F_Filtros /* Button 145 */
DO:
  HIDE FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME BUTTON-148
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-148 wWin
ON CHOOSE OF BUTTON-148 IN FRAME F_Basicos /* Ejecutar */
DO:
  ASSIGN FRAME F_Basicos Cmb_Tipo Cmb_BClientes Cmb_BAhorros.
  ASSIGN FRAME F_Filtros TTotales AnoIni AnoFin Cmb_Agencias W_DiaIni W_DiaFin UsuIni UsuFin
               Cmb_TProducto Cmb_Productos RFPago.
  ASSIGN EstadoC = INTEGER(SUBSTRING(Cmb_EstCre:SCREEN-VALUE IN FRAME F_Filtros,1,1)).
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = AgeIni.
  
  IF SUBSTRING(Cmb_TProducto,1,1) EQ "0" THEN ASSIGN TpdIni = 0 TpdFin = 9.
  ELSE ASSIGN TpdIni = INTEGER(SUBSTRING(Cmb_TProducto,1,1)) TpdFin = TpdIni.
  
  IF SUBSTRING(Cmb_Productos,1,3) EQ "000" THEN ASSIGN ProIni = 0 ProFin = 999.
  ELSE ASSIGN ProIni = INTEGER(SUBSTRING(Cmb_Productos,1,3)) ProFin = ProIni.
  
  
  VIEW FRAME F_Progreso.
  CASE Cmb_Tipo:
    WHEN "Clientes" THEN RUN Informes_Clientes.
    WHEN "Ahorros"  THEN RUN Informes_Ahorros.
    WHEN "Solicitudes" THEN RUN Informes_Solicitudes.
    WHEN "Garantias" THEN RUN Informes_Garantias.
    WHEN "Usuarios" THEN RUN Informes_Usuarios.
    WHEN "Créditos" THEN RUN Informes_Creditos.
  END CASE.
  W_ok = e1:READ-FILE(Listado) IN FRAME F_Basicos.
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-149
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-149 wWin
ON CHOOSE OF BUTTON-149 IN FRAME F_Basicos /* Button 149 */
DO:
  IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
    DEFINE VAR W_sw          AS LOGICAL. 
    RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado,INPUT-OUTPUT W_Dispositivo).
    IF W_Dispositivo = "" THEN
      RETURN.
    IF W_Dispositivo = "P" THEN  
      RUN Pantalla IN W_Manija (INPUT Listado).
    ELSE                                                  
      IF W_Dispositivo = "I" THEN
          RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  ?,INPUT  1,INPUT  1,
                                          INPUT  99999,OUTPUT W_sw).
    IF W_Dispositivo = "E" THEN
      RUN Imprimir_Excel.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-150
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-150 wWin
ON CHOOSE OF BUTTON-150 IN FRAME F_Basicos /* Salir */
DO:
  OS-DELETE VALUE(Listado).
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-153
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON CHOOSE OF BUTTON-153 IN FRAME F_Basicos /* Button 153 */
DO:
  IF E1:SCREEN-VALUE EQ "" THEN
     MESSAGE "No se ha ejecutado ningún Informe" SKIP
             "Escoja el informe y presione clic" SKIP
             "en el boton ejecutar!." VIEW-AS ALERT-BOX INFORMATION.
  ELSE DO:
    Buscar:SCREEN-VALUE IN FRAME F_Buscar = E1:SELECTION-TEXT.
    ASSIGN FRAME F_Buscar Buscar.
    W_Ok = E1:SEARCH(Buscar,32).
    VIEW FRAME F_Buscar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME BUTTON-154
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-154 wWin
ON CHOOSE OF BUTTON-154 IN FRAME F_Buscar /* Button 154 */
DO:
  HIDE FRAME F_Buscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME F_Filtros /* Agencias */
DO:
  ASSIGN FRAME F_Filtros Cmb_Agencias.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME Cmb_BCreditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_BCreditos wWin
ON VALUE-CHANGED OF Cmb_BCreditos IN FRAME F_Basicos /* Informes Disponibles */
DO:
  CASE SELF:SCREEN-VALUE:
    WHEN "Por Instancias" THEN DO:
         ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = YES
                Cmb_EstCre:HIDDEN IN FRAME F_Filtros = NO   
                Cmb_EstCre:LIST-ITEMS IN FRAME F_Filtros = ""
                Cmb_EstCre:LABEL IN FRAME F_Filtros = "Instancias"
                W_Ok = Cmb_EstCre:ADD-LAST("2 - Proceso de Cobros")
                W_Ok = Cmb_EstCre:ADD-LAST("5 - Proceso de Abogados")
                Cmb_EstCre:SCREEN-VALUE = Cmb_EstCre:ENTRY(1)
                TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
                ValIni:LABEL = "Valor Inicial"
                ValFin:LABEL = "Valor Final".
     END.
    WHEN "Por Estados" THEN DO:
         ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = YES
                Cmb_EstCre:HIDDEN IN FRAME F_Filtros = NO
                Cmb_EstCre:LIST-ITEMS IN FRAME F_Filtros = ""
                Cmb_EstCre:LABEL IN FRAME F_Filtros = "Estados"
                W_Ok = Cmb_EstCre:ADD-LAST("1 - Aprobado No Desembolsado")
                W_Ok = Cmb_EstCre:ADD-LAST("2 - Normal Desembolsado")
                W_Ok = Cmb_EstCre:ADD-LAST("3 - Cancelado")
                W_Ok = Cmb_EstCre:ADD-LAST("4 - Retirado Sin Aprobar")
                W_Ok = Cmb_EstCre:ADD-LAST("5 - Castigado")
                Cmb_EstCre:SCREEN-VALUE = Cmb_EstCre:ENTRY(1)
                TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
                ValIni:LABEL = "Valor Inicial"
                ValFin:LABEL = "Valor Final".
    END.
    WHEN "Cartera" THEN DO:
         ASSIGN Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
                Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
                TTotales:SENSITIVE IN FRAME F_Filtros = YES TTotales:HIDDEN IN FRAME F_Filtros = NO
                ValIni:LABEL = "Días de Mora Inicial"
                ValFin:LABEL = "Días de Mora Final".
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_BSolicitudes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_BSolicitudes wWin
ON VALUE-CHANGED OF Cmb_BSolicitudes IN FRAME F_Basicos /* Informes Disponibles */
DO:
  IF SELF:SCREEN-VALUE EQ "Por Tipo de Producto" THEN DO:
     VIEW FRAME F_Filtros.
     APPLY "entry" TO Cmb_TProducto IN FRAME F_Filtros.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Cmb_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON VALUE-CHANGED OF Cmb_Productos IN FRAME F_Filtros /* Productos Disponibles */
DO:
  ASSIGN FRAME F_Filtros Cmb_Productos.
  IF Cmb_Productos EQ "000 - Todos" THEN ASSIGN ProIni = 0 ProFin = 999.
  ELSE ASSIGN ProIni = INTEGER(SUBSTRING(Cmb_Productos,1,3)) ProFin = ProIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME Cmb_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tipo wWin
ON VALUE-CHANGED OF Cmb_Tipo IN FRAME F_Basicos /* Tipo de Informe */
DO:
  ASSIGN FRAME F_Basicos Cmb_Tipo.
  ASSIGN Cmb_TProducto:LIST-ITEMS IN FRAME F_Filtros = ""
         Cmb_Productos:LIST-ITEMS IN FRAME F_Filtros = ""
         W_Ok = Cmb_Productos:ADD-LAST("000 - Todos")
         Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1)
         ValIni:SCREEN-VALUE = "0"
         ValFin:SCREEN-VALUE = "999999999"
         RFPago:SENSITIVE IN FRAME F_Filtros = NO.

  CASE Cmb_Tipo:
    WHEN "Clientes" THEN DO:
      ASSIGN Cmb_BClientes:SENSITIVE = YES
             Cmb_BClientes:HIDDEN    = NO
             Cmb_BAhorros:SENSITIVE  = NO
             Cmb_BAhorros:HIDDEN     = YES
             Cmb_BCreditos:SENSITIVE  = NO
             Cmb_BCreditos:HIDDEN     = YES
             Cmb_BGarantias:SENSITIVE = NO
             Cmb_BGarantias:HIDDEN    = YES
             Cmb_BSolicitudes:SENSITIVE = NO
             Cmb_BUsuarios:SENSITIVE = NO
             Cmb_BUsuarios:HIDDEN    = YES
             Cmb_BSolicitudes:HIDDEN    = YES
             Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN = YES
             ValIni:SENSITIVE = NO
             ValFin:SENSITIVE = NO
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
             DISABLE {&List-6} WITH FRAME F_Filtros.
    END.
    WHEN "Ahorros" THEN DO:
      ASSIGN Cmb_BClientes:SENSITIVE = NO
             Cmb_BClientes:HIDDEN    = YES
             Cmb_BCreditos:SENSITIVE  = NO
             Cmb_BCreditos:HIDDEN     = YES
             Cmb_BSolicitudes:SENSITIVE = NO
             Cmb_BSolicitudes:HIDDEN    = YES
             Cmb_BGarantias:SENSITIVE = NO
             Cmb_BGarantias:HIDDEN    = YES
             Cmb_BAhorros:SENSITIVE  = YES
             Cmb_BAhorros:HIDDEN     = NO
             Cmb_BAhorros:SCREEN-VALUE = Cmb_BAhorros:ENTRY(1)
             Cmb_BUsuarios:SENSITIVE = NO
             Cmb_BUsuarios:HIDDEN    = YES
             Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
             ValIni:SENSITIVE = YES
             ValFin:SENSITIVE = YES
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN = YES
             W_Ok = Cmb_TProducto:ADD-LAST("1 - A la Vista")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Contractual")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - A Término")
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
      FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro   EQ 1 AND
                                 Pro_Ahorros.Estado       EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_producto).
      END.
    END.
    WHEN "Solicitudes" THEN DO:
      ASSIGN Cmb_BClientes:SENSITIVE = NO
             Cmb_BClientes:HIDDEN    = YES
             Cmb_BCreditos:SENSITIVE  = NO
             Cmb_BCreditos:HIDDEN     = YES
             Cmb_BGarantias:SENSITIVE = NO
             Cmb_BGarantias:HIDDEN    = YES
             Cmb_BSolicitudes:SENSITIVE = YES
             Cmb_BSolicitudes:HIDDEN    = NO
             Cmb_BAhorros:SENSITIVE  = NO
             Cmb_BAhorros:HIDDEN     = YES
             Cmb_BUsuarios:SENSITIVE = NO
             Cmb_BUsuarios:HIDDEN    = YES
             Cmb_BSolicitudes:SCREEN-VALUE = Cmb_BSolicitudes:ENTRY(1)
             Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
             ValIni:SENSITIVE = YES
             ValFin:SENSITIVE = YES
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN = YES
             W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos")
             W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")
             W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito   EQ 1 AND
                                  Pro_Creditos.Estado        EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_producto).
      END.
    END.
    WHEN "Garantias" THEN DO:
      ASSIGN Cmb_BClientes:SENSITIVE = NO
             Cmb_BClientes:HIDDEN    = YES
             Cmb_BCreditos:SENSITIVE  = NO
             Cmb_BCreditos:HIDDEN     = YES
             Cmb_BSolicitudes:SENSITIVE = NO
             Cmb_BSolicitudes:HIDDEN    = YES
             Cmb_BGarantias:SENSITIVE = YES
             Cmb_BGarantias:HIDDEN    = NO
             Cmb_BAhorros:SENSITIVE  = NO
             Cmb_BAhorros:HIDDEN     = YES
             Cmb_BUsuarios:SENSITIVE = NO
             Cmb_BUsuarios:HIDDEN    = YES
             Cmb_BGarantias:SCREEN-VALUE = Cmb_BGarantias:ENTRY(1)
             Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             ValIni:SENSITIVE = YES
             ValFin:SENSITIVE = YES
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN = YES
             W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos")
             W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")
             W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito   EQ 1 AND
                                  Pro_Creditos.Estado        EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_producto).
      END.
    END.
    WHEN "Usuarios" THEN DO:
      ASSIGN Cmb_BUsuarios:SENSITIVE = YES
             Cmb_BUsuarios:HIDDEN    = NO
             Cmb_BCreditos:SENSITIVE  = NO
             Cmb_BCreditos:HIDDEN     = YES
             Cmb_BClientes:SENSITIVE = NO
             Cmb_BClientes:HIDDEN    = YES
             Cmb_BSolicitudes:SENSITIVE = NO
             Cmb_BSolicitudes:HIDDEN    = YES
             Cmb_BGarantias:SENSITIVE = NO
             Cmb_BGarantias:HIDDEN    = YES
             Cmb_BAhorros:SENSITIVE  = NO
             Cmb_BAhorros:HIDDEN     = YES
             Cmb_BUsuarios:SCREEN-VALUE = Cmb_BUsuarios:ENTRY(1)
             Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             ValIni:SENSITIVE = YES
             ValFin:SENSITIVE = YES
             W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos")
             W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")
             W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")
             UsuIni:SENSITIVE = YES
             UsuIni:HIDDEN = NO
             UsuFin:SENSITIVE = YES
             UsuFin:HIDDEN = NO
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             TTotales:SENSITIVE IN FRAME F_Filtros = NO TTotales:HIDDEN IN FRAME F_Filtros = YES
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito   EQ 1 AND
                                  Pro_Creditos.Estado        EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_producto).
      END.
    END.
    WHEN "Créditos" THEN DO:
      ASSIGN Cmb_BCreditos:SENSITIVE  = YES
             Cmb_BCreditos:HIDDEN     = NO
             Cmb_BCreditos:SCREEN-VALUE = Cmb_BCreditos:ENTRY(1)             
             Cmb_BUsuarios:SENSITIVE = NO
             Cmb_BUsuarios:HIDDEN    = YES
             Cmb_BClientes:SENSITIVE = NO
             Cmb_BClientes:HIDDEN    = YES
             Cmb_BSolicitudes:SENSITIVE = NO
             Cmb_BSolicitudes:HIDDEN    = YES
             Cmb_BGarantias:SENSITIVE = NO
             Cmb_BGarantias:HIDDEN    = YES
             Cmb_BAhorros:SENSITIVE  = NO
             Cmb_BAhorros:HIDDEN     = YES
             RFPago:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_TProducto:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_Productos:SENSITIVE IN FRAME F_Filtros = YES
             Cmb_EstCre:SENSITIVE IN FRAME F_Filtros = NO
             Cmb_EstCre:HIDDEN IN FRAME F_Filtros = YES
             ValIni:SENSITIVE = YES
             ValFin:SENSITIVE = YES
             W_Ok = Cmb_TProducto:ADD-LAST("0 - Todos")
             W_Ok = Cmb_TProducto:ADD-LAST("1 - Consumo")
             W_Ok = Cmb_TProducto:ADD-LAST("2 - Comercial")
             W_Ok = Cmb_TProducto:ADD-LAST("3 - Hipotecario")
             W_Ok = Cmb_TProducto:ADD-LAST("4 - Microcredito")
             UsuIni:SENSITIVE = NO
             UsuIni:HIDDEN = YES
             UsuFin:SENSITIVE = NO
             UsuFin:HIDDEN = YES
             Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1)
             TTotales:SENSITIVE IN FRAME F_Filtros = YES TTotales:HIDDEN IN FRAME F_Filtros = NO
             ValIni:LABEL = "Dias de Mora Inicial"
             ValFin:LABEL = "Dias de Mora Final".
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito   EQ 1 AND
                                  Pro_Creditos.Estado        EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_producto).
      END.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Cmb_TProducto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TProducto wWin
ON VALUE-CHANGED OF Cmb_TProducto IN FRAME F_Filtros /* Tipos de Producto */
DO:
  ASSIGN FRAME F_Basicos Cmb_Tipo.
  ASSIGN FRAME F_Filtros Cmb_TProducto
         Cmb_Productos:LIST-ITEMS = ""
         W_Ok = Cmb_Productos:ADD-LAST("000 - Todos")
         Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
  IF Cmb_Tipo EQ "Ahorros" THEN DO:
     FOR EACH Pro_Ahorros WHERE 
         Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TProducto,1,1)) AND
         Pro_Ahorros.Estado       EQ 1 NO-LOCK:
         W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).      
     END.
  END.
  IF Cmb_Tipo EQ "Créditos" THEN DO:
     FOR EACH Pro_Creditos WHERE 
         Pro_Creditos.Tip_Credito EQ INTEGER(SUBSTRING(Cmb_TProducto,1,1)) AND
         Pro_Creditos.Estado       EQ 1 NO-LOCK:
         W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Credito.Cod_Credito,"999") + " - " + Pro_Credito.Nom_Producto).      
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RRFechas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RRFechas wWin
ON VALUE-CHANGED OF RRFechas IN FRAME F_Filtros
DO:
  IF SELF:SCREEN-VALUE EQ "1" THEN DO:
     DISABLE Btn_FecIni Btn_FecFin WITH FRAME F_Filtros.
     ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month1.gif")
            AnoIni:SCREEN-VALUE = "1900"
            W_DiaIni:SCREEN-VALUE = "01"
            AnoIni = 1900
            MesIni = 01
            FecIni = DATE("01/01/1900").
     ASSIGN W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(W_Fecha)) + ".gif")
            AnoFin:SCREEN-VALUE = STRING(YEAR(W_Fecha))
            W_DiaFin:SCREEN-VALUE = STRING(DAY(W_fecha))
            AnoFin = YEAR(W_Fecha)
            MesFin = MONTH(W_Fecha)
            FecFin = W_Fecha.
  END.
  ELSE DO: 
     ENABLE Btn_FecIni Btn_FecFin WITH FRAME F_Filtros.
     ASSIGN FecIni = W_Fecha - DAY(W_Fecha) + 1
            FecFin = W_Fecha.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aho_Edades wWin 
PROCEDURE Aho_Edades :
DEFINE VAR Wk        AS INTEGER FORMAT ">>,>>9".
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
FOR EACH Tmp_TotAho: DELETE Tmp_TotAho. END.
  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni AND Ahorros.Agencia      LE AgeFin AND
           Ahorros.Fec_Apertura GE FecIni AND Ahorros.Fec_Apertura LE FecFin
           NO-LOCK BREAK BY Ahorros.Agencia BY Ahorros.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Ahorros.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            FIND Tmp_TotAho WHERE Tmp_TotAho.A_Agencia EQ Ahorros.Agencia NO-ERROR.
            IF NOT AVAILABLE Tmp_TotAho THEN DO:
               CREATE Tmp_TotAho.
               ASSIGN Tmp_TotAho.A_Agencia = Ahorros.Agencia
                      Tmp_TotAho.A_NomAge  = Agencias.Nombre.
            END.
            W_NomAge = Agencias.Nombre.
         END.
      END.
      IF FIRST-OF(Ahorros.Nit) THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN
           IF Clientes.Tipo_Cliente LT 3 THEN DO:
             IF Clientes.Fec_Nacimiento NE ? THEN Wk = (W_Fecha - Clientes.Fec_Nacimiento) / 360.
             ELSE Wk = Clientes.Edad.
           END.
      END.
      IF Clientes.Tipo_Cliente LT 3 THEN DO:
          IF Wk GE 0  AND Wk LT 18 THEN Val_Hombres[1] = Val_Hombres[1] + Ahorros.Sdo_Disponible.
          IF Wk GE 18 AND Wk LT 30 THEN Val_Hombres[2] = Val_Hombres[2] + Ahorros.Sdo_Disponible.
          IF Wk GE 30 AND Wk LT 40 THEN Val_Hombres[3] = Val_Hombres[3] + Ahorros.Sdo_Disponible.
          IF Wk GE 40 AND Wk LT 50 THEN Val_Hombres[4] = Val_Hombres[4] + Ahorros.Sdo_Disponible.
          IF Wk GE 50 AND Wk LT 60 THEN Val_Hombres[5] = Val_Hombres[5] + Ahorros.Sdo_Disponible.
          IF Wk GE 60 AND Wk LT 70 THEN Val_Hombres[6] = Val_Hombres[6] + Ahorros.Sdo_Disponible.
          IF Wk GE 70 THEN Val_Hombres[7] = Val_Hombres[7] + Ahorros.Sdo_Disponible.
      END.
      IF LAST-OF(Ahorros.Nit) THEN DO:
          IF Wk GE 0  AND Wk LT 18 THEN Con_Hombres[1] = Con_Hombres[1] + 1.
          IF Wk GE 18 AND Wk LT 30 THEN Con_Hombres[2] = Con_Hombres[2] + 1.
          IF Wk GE 30 AND Wk LT 40 THEN Con_Hombres[3] = Con_Hombres[3] + 1.
          IF Wk GE 40 AND Wk LT 50 THEN Con_Hombres[4] = Con_Hombres[4] + 1.
          IF Wk GE 50 AND Wk LT 60 THEN Con_Hombres[5] = Con_Hombres[5] + 1.
          IF Wk GE 60 AND Wk LT 70 THEN Con_Hombres[6] = Con_Hombres[6] + 1.
          IF Wk GE 70 THEN Con_Hombres[7] = Con_Hombres[7] + 1.
      END.
  END.    
  
  FOR EACH Tmp_TotAho:
         Val_Hombres[8] = Val_Hombres[1] + Val_Hombres[2] + Val_Hombres[3] + Val_Hombres[4] + Val_Hombres[5]
                        + Val_Hombres[6] + Val_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Val_Hombres[1]    AT 27  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[2]    AT 39  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[3]    AT 51  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[4]    AT 63  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[5]    AT 75  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[6]    AT 87  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[7]    AT 99  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[8]    AT 111 FORMAT ">>>,>>>,>>9"
         WITH FRAME F_AhoEda WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Val_Hombres[1] Tot2 = Tot2 + Val_Hombres[2]
                Tot3 = Tot3 + Val_Hombres[3] Tot4 = Tot4 + Val_Hombres[4]
                Tot5 = Tot5 + Val_Hombres[5] Tot6 = Tot6 + Val_Hombres[6]
                Tot7 = Tot7 + Val_Hombres[7] Tot8 = Tot8 + Val_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Valores :" AT 1
          Tot1  AT 27  FORMAT ">>>,>>>,>>9"
          Tot2  AT 39  FORMAT ">>>,>>>,>>9"
          Tot3  AT 51  FORMAT ">>>,>>>,>>9"
          Tot4  AT 63  FORMAT ">>>,>>>,>>9"
          Tot5  AT 75  FORMAT ">>>,>>>,>>9"
          Tot6  AT 87  FORMAT ">>>,>>>,>>9"
          Tot7  AT 99  FORMAT ">>>,>>>,>>9"
          Tot8  AT 111 FORMAT ">>,>>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotVal WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
  DISPLA SKIP(2)
         "Total de Personas..." AT 1 SKIP(1)
         "AGE NOMBRE AGENCIA                           0 -17    18 - 29   30 - 39   40 - 49  50 - 59    60 - 69   70 o mas   TOTAL" AT 1 
         WITH FRAME F_Inter2 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
  
  FOR EACH Tmp_TotAho:
         Con_Hombres[8] = Con_Hombres[1] + Con_Hombres[2] + Con_Hombres[3] + Con_Hombres[4] + Con_Hombres[5] + Con_Hombres[6] + Con_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Con_Hombres[1]    AT 40 FORMAT ">,>>>,>>9" 
                 Con_Hombres[2]    AT 52 FORMAT ">,>>>,>>9" 
                 Con_Hombres[3]    AT 62 FORMAT ">,>>>,>>9" 
                 Con_Hombres[4]    AT 72 FORMAT ">,>>>,>>9" 
                 Con_Hombres[5]    AT 82 FORMAT ">,>>>,>>9" 
                 Con_Hombres[6]    AT 92 FORMAT ">,>>>,>>9" 
                 Con_Hombres[7]    AT 102 FORMAT ">,>>>,>>9"
                 Con_Hombres[8]    AT 112 FORMAT ">,>>>,>>9"
         WITH FRAME F_AhoEda2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Con_Hombres[1] Tot2 = Tot2 + Con_Hombres[2]
                Tot3 = Tot3 + Con_Hombres[3] Tot4 = Tot4 + Con_Hombres[4]
                Tot5 = Tot5 + Con_Hombres[5] Tot6 = Tot6 + Con_Hombres[6]
                Tot7 = Tot7 + Con_Hombres[7] Tot8 = Tot8 + Con_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Personas:" AT 1
          Tot1  AT 40 FORMAT ">,>>>,>>9" 
          Tot2  AT 52 FORMAT ">,>>>,>>9" 
          Tot3  AT 62 FORMAT ">,>>>,>>9" 
          Tot4  AT 72 FORMAT ">,>>>,>>9" 
          Tot5  AT 82 FORMAT ">,>>>,>>9" 
          Tot6  AT 92 FORMAT ">,>>>,>>9" 
          Tot7  AT 102 FORMAT ">,>>>,>>9"
          Tot8  AT 112 FORMAT ">,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotVal2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aho_EstCivil wWin 
PROCEDURE Aho_EstCivil :
DEFINE VAR Wk        AS INTEGER FORMAT ">>,>>9".
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".

FOR EACH Tmp_TotAho: DELETE Tmp_TotAho. END.
  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni AND Ahorros.Agencia      LE AgeFin AND
           Ahorros.Fec_Apertura GE FecIni AND Ahorros.Fec_Apertura LE FecFin
           NO-LOCK BREAK BY Ahorros.Agencia BY Ahorros.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Ahorros.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            FIND Tmp_TotAho WHERE Tmp_TotAho.A_Agencia EQ Ahorros.Agencia NO-ERROR.
            IF NOT AVAILABLE Tmp_TotAho THEN DO:
               CREATE Tmp_TotAho.
               ASSIGN Tmp_TotAho.A_Agencia = Ahorros.Agencia
                      Tmp_TotAho.A_NomAge  = Agencias.Nombre.
            END.
            W_NomAge = Agencias.Nombre.
         END.
      END.
      IF FIRST-OF(Ahorros.Nit) THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
      END.
      IF Clientes.Tipo_Cliente LT 3 THEN DO:
         CASE Clientes.Est_Civil:
           WHEN "Soltero"     THEN Val_Hombres[1] = Val_Hombres[1] + Ahorros.Sdo_Disponible.
           WHEN "Casado"      THEN Val_Hombres[2] = Val_Hombres[2] + Ahorros.Sdo_Disponible.
           WHEN "Separado"    THEN Val_Hombres[3] = Val_Hombres[3] + Ahorros.Sdo_Disponible.
           WHEN "Viudo"       THEN Val_Hombres[4] = Val_Hombres[4] + Ahorros.Sdo_Disponible.
           WHEN "Unión Libre" THEN Val_Hombres[5] = Val_Hombres[5] + Ahorros.Sdo_Disponible.
         END CASE.
      END.
      IF LAST-OF(Ahorros.Nit) AND Clientes.Tipo_Cliente LT 3 THEN DO:
       IF Clientes.Tipo_Cliente LT 3 THEN DO:
           CASE Clientes.Est_Civil:
            WHEN "Soltero"     THEN Con_Hombres[1] = Con_Hombres[1] + 1.
            WHEN "Casado"      THEN Con_Hombres[2] = Con_Hombres[2] + 1.
            WHEN "Separado"    THEN Con_Hombres[3] = Con_Hombres[3] + 1.
            WHEN "Viudo"       THEN Con_Hombres[4] = Con_Hombres[4] + 1.
            WHEN "Unión Libre" THEN Con_Hombres[5] = Con_Hombres[5] + 1.
          END CASE.
       END.
      END.
  END.    
  
  FOR EACH Tmp_TotAho:
         Val_Hombres[6] = Val_Hombres[1] + Val_Hombres[2] + Val_Hombres[3] + Val_Hombres[4] + Val_Hombres[5].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Val_Hombres[1]    AT 27  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[2]    AT 39  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[3]    AT 51  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[4]    AT 63  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[5]    AT 75  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[6]    AT 87  FORMAT ">>>,>>>,>>9"
         WITH FRAME F_AhoEda WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Val_Hombres[1] Tot2 = Tot2 + Val_Hombres[2]
                Tot3 = Tot3 + Val_Hombres[3] Tot4 = Tot4 + Val_Hombres[4]
                Tot5 = Tot5 + Val_Hombres[5] Tot6 = Tot6 + Val_Hombres[6].
  END.
  DISPLAY SKIP(1)
          "Total Valores :" AT 1
          Tot1  AT 27  FORMAT ">>>,>>>,>>9"
          Tot2  AT 39  FORMAT ">>>,>>>,>>9"
          Tot3  AT 51  FORMAT ">>>,>>>,>>9"
          Tot4  AT 63  FORMAT ">>>,>>>,>>9"
          Tot5  AT 75  FORMAT ">>>,>>>,>>9"
          Tot6  AT 87  FORMAT ">>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotVal WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0.
  DISPLA SKIP(2)
         "Total de Personas..." AT 1 SKIP(1)
         "AGE NOMBRE AGENCIA                          SOLTEROS  CASADOS  SEPARADOS   VIUDOS  UNION LIBRE  TOTAL" AT 1  
         WITH FRAME F_Inter2 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
  
  FOR EACH Tmp_TotAho:
         Con_Hombres[6] = Con_Hombres[1] + Con_Hombres[2] + Con_Hombres[3] + Con_Hombres[4] + Con_Hombres[5].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Con_Hombres[1]    AT 40 FORMAT ">,>>>,>>9" 
                 Con_Hombres[2]    AT 52 FORMAT ">,>>>,>>9" 
                 Con_Hombres[3]    AT 62 FORMAT ">,>>>,>>9" 
                 Con_Hombres[4]    AT 72 FORMAT ">,>>>,>>9" 
                 Con_Hombres[5]    AT 82 FORMAT ">,>>>,>>9" 
                 Con_Hombres[6]    AT 92 FORMAT ">,>>>,>>9" 
         WITH FRAME F_AhoEda2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Con_Hombres[1] Tot2 = Tot2 + Con_Hombres[2]
                Tot3 = Tot3 + Con_Hombres[3] Tot4 = Tot4 + Con_Hombres[4]
                Tot5 = Tot5 + Con_Hombres[5] Tot6 = Tot6 + Con_Hombres[6].
  END.
  DISPLAY SKIP(1)
          "Total Personas:" AT 1
          Tot1  AT 40 FORMAT ">,>>>,>>9" 
          Tot2  AT 52 FORMAT ">,>>>,>>9" 
          Tot3  AT 62 FORMAT ">,>>>,>>9" 
          Tot4  AT 72 FORMAT ">,>>>,>>9" 
          Tot5  AT 82 FORMAT ">,>>>,>>9" 
          Tot6  AT 92 FORMAT ">,>>>,>>9" 
          SKIP(2)
  WITH FRAME F_TotVal2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aho_NivEducativo wWin 
PROCEDURE Aho_NivEducativo :
DEFINE VAR Wk        AS INTEGER FORMAT ">>,>>9".
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
FOR EACH Tmp_TotAho: DELETE Tmp_TotAho. END.
  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni AND Ahorros.Agencia      LE AgeFin AND
           Ahorros.Fec_Apertura GE FecIni AND Ahorros.Fec_Apertura LE FecFin
           NO-LOCK BREAK BY Ahorros.Agencia BY Ahorros.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Ahorros.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            FIND Tmp_TotAho WHERE Tmp_TotAho.A_Agencia EQ Ahorros.Agencia NO-ERROR.
            IF NOT AVAILABLE Tmp_TotAho THEN DO:
               CREATE Tmp_TotAho.
               ASSIGN Tmp_TotAho.A_Agencia = Ahorros.Agencia
                      Tmp_TotAho.A_NomAge  = Agencias.Nombre.
            END.
         END.
      END.
      IF FIRST-OF(Ahorros.Nit) THEN FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
      IF Clientes.Tipo_Cliente LT 3 THEN DO:
         CASE Clientes.Niv_Educativo:
           WHEN "Ninguno"     THEN Val_Hombres[1] = Val_Hombres[1] + Ahorros.Sdo_Disponible.
           WHEN "Primaria"    THEN Val_Hombres[2] = Val_Hombres[2] + Ahorros.Sdo_Disponible.
           WHEN "Bachiller"   THEN Val_Hombres[3] = Val_Hombres[3] + Ahorros.Sdo_Disponible.
           WHEN "Tecnológo"   THEN Val_Hombres[4] = Val_Hombres[4] + Ahorros.Sdo_Disponible.
           WHEN "Técnico"     THEN Val_Hombres[5] = Val_Hombres[5] + Ahorros.Sdo_Disponible.
           WHEN "Profesional" THEN Val_Hombres[6] = Val_Hombres[6] + Ahorros.Sdo_Disponible.
           WHEN "Post-Grado"  THEN Val_Hombres[7] = Val_Hombres[7] + Ahorros.Sdo_Disponible.
         END CASE.
      END.
      IF LAST-OF(Ahorros.Nit) AND Clientes.Tipo_Cliente LT 3 THEN DO:
       IF Clientes.Tipo_Cliente LT 3 THEN DO:
           CASE Clientes.Niv_Educativo:
            WHEN "Ninguno"     THEN Con_Hombres[1] = Con_Hombres[1] + 1.
            WHEN "Primaria"    THEN Con_Hombres[2] = Con_Hombres[2] + 1.
            WHEN "Bachiller"   THEN Con_Hombres[3] = Con_Hombres[3] + 1.
            WHEN "Tecnológo"   THEN Con_Hombres[4] = Con_Hombres[4] + 1.
            WHEN "Técnico"     THEN Con_Hombres[5] = Con_Hombres[5] + 1.
            WHEN "Profesional" THEN Con_Hombres[6] = Con_Hombres[6] + 1.
            WHEN "Post-Grado"  THEN Con_Hombres[7] = Con_Hombres[7] + 1.
          END CASE.
       END.
      END.
  END.    
  
  FOR EACH Tmp_TotAho:
         Val_Hombres[8] = Val_Hombres[1] + Val_Hombres[2] + Val_Hombres[3] + 
         Val_Hombres[4] + Val_Hombres[5] + Val_Hombres[6] + Val_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Val_Hombres[1]    AT 27  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[2]    AT 39  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[3]    AT 51  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[4]    AT 63  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[5]    AT 75  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[6]    AT 87  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[7]    AT 99  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[8]    AT 111  FORMAT ">>>,>>>,>>9"
         WITH FRAME F_AhoNiv WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Val_Hombres[1] Tot2 = Tot2 + Val_Hombres[2]
                Tot3 = Tot3 + Val_Hombres[3] Tot4 = Tot4 + Val_Hombres[4]
                Tot5 = Tot5 + Val_Hombres[5] Tot6 = Tot6 + Val_Hombres[6]
                Tot7 = Tot7 + Val_Hombres[7] Tot8 = Tot8 + Val_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Valores :" AT 1
          Tot1  AT 27  FORMAT ">>>,>>>,>>9"
          Tot2  AT 39  FORMAT ">>>,>>>,>>9"
          Tot3  AT 51  FORMAT ">>>,>>>,>>9"
          Tot4  AT 63  FORMAT ">>>,>>>,>>9"
          Tot5  AT 75  FORMAT ">>>,>>>,>>9"
          Tot6  AT 87  FORMAT ">>>,>>>,>>9"
          Tot7  AT 99  FORMAT ">>>,>>>,>>9"
          Tot8  AT 111  FORMAT ">>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotNiv WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
  DISPLA SKIP(2)
         "Total de Personas..." AT 1 SKIP(1)
         "AGE NOMBRE AGENCIA                      NINGUNO    PRIMARIA   BACHILLER TECNOLÓGO TÉCNICO   PROFESION  POST-GRADO  TOTAL" AT 1  
         WITH FRAME F_Inter2 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
  
  FOR EACH Tmp_TotAho:
         Con_Hombres[8] = Con_Hombres[1] + Con_Hombres[2] + Con_Hombres[3] + 
         Con_Hombres[4] + Con_Hombres[5] + Con_Hombres[6] + Con_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Con_Hombres[1]    AT 40 FORMAT ">,>>>,>>9" 
                 Con_Hombres[2]    AT 52 FORMAT ">,>>>,>>9" 
                 Con_Hombres[3]    AT 62 FORMAT ">,>>>,>>9" 
                 Con_Hombres[4]    AT 72 FORMAT ">,>>>,>>9" 
                 Con_Hombres[5]    AT 82 FORMAT ">,>>>,>>9" 
                 Con_Hombres[6]    AT 92 FORMAT ">,>>>,>>9" 
                 Con_Hombres[7]    AT 102 FORMAT ">,>>>,>>9" 
                 Con_Hombres[8]    AT 112 FORMAT ">,>>>,>>9" 
         WITH FRAME F_AhoNiv2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Con_Hombres[1] Tot2 = Tot2 + Con_Hombres[2]
                Tot3 = Tot3 + Con_Hombres[3] Tot4 = Tot4 + Con_Hombres[4]
                Tot5 = Tot5 + Con_Hombres[5] Tot6 = Tot6 + Con_Hombres[6]
                Tot7 = Tot7 + Con_Hombres[7] Tot8 = Tot8 + Con_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Personas:" AT 1
          Tot1  AT 40 FORMAT ">,>>>,>>9" 
          Tot2  AT 52 FORMAT ">,>>>,>>9" 
          Tot3  AT 62 FORMAT ">,>>>,>>9" 
          Tot4  AT 72 FORMAT ">,>>>,>>9" 
          Tot5  AT 82 FORMAT ">,>>>,>>9" 
          Tot6  AT 92 FORMAT ">,>>>,>>9" 
          Tot7  AT 102 FORMAT ">,>>>,>>9" 
          Tot8  AT 112 FORMAT ">,>>>,>>9" 
          SKIP(2)
  WITH FRAME F_TotNiv2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aho_Salarios wWin 
PROCEDURE Aho_Salarios :
DEFINE VAR Wk        AS INTEGER FORMAT ">>,>>9".
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
FOR EACH Tmp_TotAho: DELETE Tmp_TotAho. END.
  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni AND Ahorros.Agencia      LE AgeFin AND
           Ahorros.Fec_Apertura GE FecIni AND Ahorros.Fec_Apertura LE FecFin
           NO-LOCK BREAK BY Ahorros.Agencia BY Ahorros.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Ahorros.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            FIND Tmp_TotAho WHERE Tmp_TotAho.A_Agencia EQ Ahorros.Agencia NO-ERROR.
            IF NOT AVAILABLE Tmp_TotAho THEN DO:
               CREATE Tmp_TotAho.
               ASSIGN Tmp_TotAho.A_Agencia = Ahorros.Agencia
                      Tmp_TotAho.A_NomAge  = Agencias.Nombre.
            END.
         END.
      END.
      IF FIRST-OF(Ahorros.Nit) THEN FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
      IF Clientes.Tipo_Cliente LT 3 THEN DO:
         IF Clientes.Salario GE 0  AND Clientes.Salario LE Minimo THEN                    
            Val_Hombres[1] = Val_Hombres[1] + Ahorros.Sdo_Disponible.
         IF Clientes.Salario GE (Minimo + 1)     AND Clientes.Salario LE (Minimo * 2) THEN
            Val_Hombres[2] = Val_Hombres[2] + Ahorros.Sdo_Disponible.
         IF Clientes.Salario GE (Minimo * 2) + 1 AND Clientes.Salario LE (Minimo * 3) THEN
            Val_Hombres[3] = Val_Hombres[3] + Ahorros.Sdo_Disponible.
         IF Clientes.Salario GE (Minimo * 3) + 1 AND Clientes.Salario LT (Minimo * 4) THEN
            Val_Hombres[4] = Val_Hombres[4] + Ahorros.Sdo_Disponible.
         IF Clientes.Salario GE (Minimo * 4) + 1 AND Clientes.Salario LT (Minimo * 5) THEN
            Val_Hombres[5] = Val_Hombres[5] + Ahorros.Sdo_Disponible.
         IF Clientes.Salario GE (Minimo * 5) + 1 AND Clientes.Salario LT (Minimo * 6) THEN
            Val_Hombres[6] = Val_Hombres[6] + Ahorros.Sdo_Disponible.
         IF Clientes.Salario GE (Minimo * 6) + 1 THEN                                     
            Val_Hombres[7] = Val_Hombres[7] + Ahorros.Sdo_Disponible.
      END.
      IF LAST-OF(Ahorros.Nit) AND Clientes.Tipo_Cliente LT 3 THEN DO:
       IF Clientes.Tipo_Cliente LT 3 THEN DO:
         IF Clientes.Salario GE 0  AND Clientes.Salario LE Minimo THEN                    
            Con_Hombres[1] = Con_Hombres[1] + 1.
         IF Clientes.Salario GE (Minimo + 1)     AND Clientes.Salario LE (Minimo * 2) THEN
            Con_Hombres[2] = Con_Hombres[2] + 1.
         IF Clientes.Salario GE (Minimo * 2) + 1 AND Clientes.Salario LE (Minimo * 3) THEN
            Con_Hombres[3] = Con_Hombres[3] + 1.
         IF Clientes.Salario GE (Minimo * 3) + 1 AND Clientes.Salario LT (Minimo * 4) THEN
            Con_Hombres[4] = Con_Hombres[4] + 1.
         IF Clientes.Salario GE (Minimo * 4) + 1 AND Clientes.Salario LT (Minimo * 5) THEN
            Con_Hombres[5] = Con_Hombres[5] + 1.
         IF Clientes.Salario GE (Minimo * 5) + 1 AND Clientes.Salario LT (Minimo * 6) THEN
            Con_Hombres[6] = Con_Hombres[6] + 1.
         IF Clientes.Salario GE (Minimo * 6) + 1 THEN                                     
            Con_Hombres[7] = Con_Hombres[7] + 1.
       END.
      END.
  END.    
  
  FOR EACH Tmp_TotAho:
         Val_Hombres[8] = Val_Hombres[1] + Val_Hombres[2] + Val_Hombres[3] + 
         Val_Hombres[4] + Val_Hombres[5] + Val_Hombres[6] + Val_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Val_Hombres[1]    AT 27  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[2]    AT 39  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[3]    AT 51  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[4]    AT 63  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[5]    AT 75  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[6]    AT 87  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[7]    AT 99  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[8]    AT 111  FORMAT ">>>,>>>,>>9"
         WITH FRAME F_AhoNiv WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Val_Hombres[1] Tot2 = Tot2 + Val_Hombres[2]
                Tot3 = Tot3 + Val_Hombres[3] Tot4 = Tot4 + Val_Hombres[4]
                Tot5 = Tot5 + Val_Hombres[5] Tot6 = Tot6 + Val_Hombres[6]
                Tot7 = Tot7 + Val_Hombres[7] Tot8 = Tot8 + Val_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Valores :" AT 1
          Tot1  AT 27  FORMAT ">>>,>>>,>>9"
          Tot2  AT 39  FORMAT ">>>,>>>,>>9"
          Tot3  AT 51  FORMAT ">>>,>>>,>>9"
          Tot4  AT 63  FORMAT ">>>,>>>,>>9"
          Tot5  AT 75  FORMAT ">>>,>>>,>>9"
          Tot6  AT 87  FORMAT ">>>,>>>,>>9"
          Tot7  AT 99  FORMAT ">>>,>>>,>>9"
          Tot8  AT 111  FORMAT ">>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotNiv WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
  DISPLA SKIP(2)
         "Total de Personas..." AT 1 SKIP(1)
         "AGE NOMBRE AGENCIA                        0 - 1       1 - 2      2 - 3     3 - 4    4 - 5      5 - 6    6 - o mas   TOTAL" AT 1  
         WITH FRAME F_Inter2 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
  
  FOR EACH Tmp_TotAho:
         Con_Hombres[8] = Con_Hombres[1] + Con_Hombres[2] + Con_Hombres[3] + 
         Con_Hombres[4] + Con_Hombres[5] + Con_Hombres[6] + Con_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Con_Hombres[1]    AT 40 FORMAT ">,>>>,>>9" 
                 Con_Hombres[2]    AT 52 FORMAT ">,>>>,>>9" 
                 Con_Hombres[3]    AT 62 FORMAT ">,>>>,>>9" 
                 Con_Hombres[4]    AT 72 FORMAT ">,>>>,>>9" 
                 Con_Hombres[5]    AT 82 FORMAT ">,>>>,>>9" 
                 Con_Hombres[6]    AT 92 FORMAT ">,>>>,>>9" 
                 Con_Hombres[7]    AT 102 FORMAT ">,>>>,>>9" 
                 Con_Hombres[8]    AT 112 FORMAT ">,>>>,>>9" 
         WITH FRAME F_AhoNiv2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Con_Hombres[1] Tot2 = Tot2 + Con_Hombres[2]
                Tot3 = Tot3 + Con_Hombres[3] Tot4 = Tot4 + Con_Hombres[4]
                Tot5 = Tot5 + Con_Hombres[5] Tot6 = Tot6 + Con_Hombres[6]
                Tot7 = Tot7 + Con_Hombres[7] Tot8 = Tot8 + Con_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Personas:" AT 1
          Tot1  AT 40 FORMAT ">,>>>,>>9" 
          Tot2  AT 52 FORMAT ">,>>>,>>9" 
          Tot3  AT 62 FORMAT ">,>>>,>>9" 
          Tot4  AT 72 FORMAT ">,>>>,>>9" 
          Tot5  AT 82 FORMAT ">,>>>,>>9" 
          Tot6  AT 92 FORMAT ">,>>>,>>9" 
          Tot7  AT 102 FORMAT ">,>>>,>>9" 
          Tot8  AT 112 FORMAT ">,>>>,>>9" 
          SKIP(2)
  WITH FRAME F_TotNiv2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aho_Sexo wWin 
PROCEDURE Aho_Sexo PRIVATE :
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
FOR EACH Tmp_TotAho: DELETE Tmp_TotAho. END.
  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni AND Ahorros.Agencia      LE AgeFin AND
           Ahorros.Fec_Apertura GE FecIni AND Ahorros.Fec_Apertura LE FecFin
           NO-LOCK BREAK BY Ahorros.Agencia BY Ahorros.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Ahorros.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            FIND Tmp_TotAho WHERE Tmp_TotAho.A_Agencia EQ Ahorros.Agencia NO-ERROR.
            IF NOT AVAILABLE Tmp_TotAho THEN DO:
               CREATE Tmp_TotAho.
               ASSIGN Tmp_TotAho.A_Agencia = Ahorros.Agencia
                      Tmp_TotAho.A_NomAge  = Agencias.Nombre.
            END.
            W_NomAge = Agencias.Nombre.
         END.
      END.
      IF FIRST-OF(Ahorros.Nit) THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
      END.
      IF Clientes.Tipo_Cliente LT 3 THEN DO:
         IF Clientes.Sexo EQ 1 THEN 
            ASSIGN Val_Hombres[Ahorros.Tip_Ahorro] = Val_Hombres[Ahorros.Tip_Ahorro] + Ahorros.Sdo_Disponible
                   Con_Hombres[Ahorros.Tip_Ahorro] = Con_Hombres[Ahorros.Tip_Ahorro] + 1.
         ELSE
            ASSIGN Val_Mujeres[Ahorros.Tip_Ahorro] = Val_Mujeres[Ahorros.Tip_Ahorro] + Ahorros.Sdo_Disponible
                   Con_Mujeres[Ahorros.Tip_Ahorro] = Con_Mujeres[Ahorros.Tip_Ahorro] + 1.
      END.
  END.    
  DISPLA "                          *-------------CLIENTES HOMBRES---------------*  *-------------CLIENTES MUJERES---------------*" AT 1
         "AGE NOMBRE AGENCIA        A LA VISTA  CONTRACTUAL  A TERMINO    APORTES   A LA VISTA  CONTRACTUAL  A TERMINO    APORTES"  AT 1 
         WITH FRAME F_Inter WIDTH 132 NO-LABEL NO-BOX USE-TEXT STREAM-IO.
  FOR EACH Tmp_TotAho:
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Val_Hombres[1]    AT 27  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[2]    AT 39  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[3]    AT 51  FORMAT ">>>,>>>,>>9"
                 Val_Hombres[4]    AT 63  FORMAT ">>>,>>>,>>9"
                 Val_Mujeres[1]    AT 75  FORMAT ">>>,>>>,>>9"
                 Val_Mujeres[2]    AT 87  FORMAT ">>>,>>>,>>9"
                 Val_Mujeres[3]    AT 99  FORMAT ">>>,>>>,>>9"
                 Val_Mujeres[4]    AT 111 FORMAT ">>>,>>>,>>9"
         WITH FRAME F_AhoSex WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Val_Hombres[1] Tot2 = Tot2 + Val_Hombres[2]
                Tot3 = Tot3 + Val_Hombres[3] Tot4 = Tot4 + Val_Hombres[4]
                Tot5 = Tot5 + Val_Mujeres[1] Tot6 = Tot6 + Val_Mujeres[2]
                Tot7 = Tot7 + Val_Mujeres[3] Tot8 = Tot8 + Val_Mujeres[4].
  END.
  DISPLAY SKIP(1)
          "Total Valores :" AT 1
          Tot1  AT  27  FORMAT ">>>,>>>,>>9"
          Tot2  AT  39  FORMAT ">>>,>>>,>>9"
          Tot3  AT  51  FORMAT ">>>,>>>,>>9"
          Tot4  AT  63  FORMAT ">>>,>>>,>>9"
          Tot5  AT  75  FORMAT ">>>,>>>,>>9"
          Tot6  AT  87  FORMAT ">>>,>>>,>>9"
          Tot7  AT  99  FORMAT ">>>,>>>,>>9"
          Tot8  AT  111  FORMAT ">>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotVal WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
  DISPLA SKIP(2)
         "Total de Personas..." AT 1 SKIP(1)
         "                          *-------------CLIENTES HOMBRES---------------*  *-------------CLIENTES MUJERES---------------*" AT 1
         "AGE NOMBRE AGENCIA        A LA VISTA  CONTRACTUAL  A TERMINO    APORTES   A LA VISTA  CONTRACTUAL  A TERMINO    APORTES"  AT 1 
         WITH FRAME F_Inter2 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
  FOR EACH Tmp_TotAho:
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Con_Hombres[1]    AT 27  FORMAT ">>>,>>>,>>9"
                 Con_Hombres[2]    AT 39  FORMAT ">>>,>>>,>>9"
                 Con_Hombres[3]    AT 51  FORMAT ">>>,>>>,>>9"
                 Con_Hombres[4]    AT 63  FORMAT ">>>,>>>,>>9"
                 Con_Mujeres[1]    AT 75  FORMAT ">>>,>>>,>>9"
                 Con_Mujeres[2]    AT 87  FORMAT ">>>,>>>,>>9"
                 Con_Mujeres[3]    AT 99  FORMAT ">>>,>>>,>>9"
                 Con_Mujeres[4]    AT 111 FORMAT ">>>,>>>,>>9"
         WITH FRAME F_AhoSex2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Con_Hombres[1] Tot2 = Tot2 + Con_Hombres[2] 
                Tot3 = Tot3 + Con_Hombres[3] Tot4 = Tot4 + Con_Hombres[4]
                Tot5 = Tot5 + Con_Mujeres[1] Tot6 = Tot6 + Con_Mujeres[2]
                Tot7 = Tot7 + Con_Mujeres[3] Tot8 = Tot8 + Con_Mujeres[4].
  END.
  DISPLAY SKIP(1)
          "Total Personas:" AT 1
          Tot1  AT  27  FORMAT ">>>,>>>,>>9"
          Tot2  AT  39  FORMAT ">>>,>>>,>>9"
          Tot3  AT  51  FORMAT ">>>,>>>,>>9"
          Tot4  AT  63  FORMAT ">>>,>>>,>>9"
          Tot5  AT  75  FORMAT ">>>,>>>,>>9"
          Tot6  AT  87  FORMAT ">>>,>>>,>>9"
          Tot7  AT  99  FORMAT ">>>,>>>,>>9"
          Tot8  AT  111  FORMAT ">>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotVal2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BtnImp_Ahorros wWin 
PROCEDURE BtnImp_Ahorros :
Listado = W_PathSpl + "\Ahorros.LST".

ASSIGN FRAME F_Filtros ValIni ValFin.  
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" 
  INITIAL "AGE NIT           CODPDT CUENTA            TASA   PLAZO   ".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".

ASSIGN J = 0 k = 0.

{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Ahorros: " + Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BClientes:
    WHEN "Básico" THEN  
        W_EncColumna = WEnc1 + " FEC.APERTR  MONTO.APERTURA  SDO.DISPONIBLE".
    WHEN "Fecha de Cancelacion" THEN
        W_EncColumna = WEnc1 + " FEC.APERTR  MONTO.APERTURA  SDO.DISPONIBLE   FEC.CANCELA".
    WHEN "Fecha de Vencimiento" THEN
        W_EncColumna = WEnc1 + " FEC.APERTR  MONTO.APERTURA  SDO.DISPONIBLE   FEC.VENCIMIEN".
    WHEN "Canje" THEN
                             /* 6         7         8         9         1
                                012345678901234567890123456789012345678901234567890*/
        W_EncColumna = WEnc1 + " FEC.APERTR  MONTO.APERTURA  SDO.DISPONIBLE   CANJE".
  END CASE.
  VIEW FRAME F-Encabezado.
  
  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni AND
           Ahorros.Agencia      LE AgeFin AND
           Ahorros.Cod_Ahorro   GE ProIni AND
           Ahorros.Cod_Ahorro   LE ProFin
           NO-LOCK BREAK BY Ahorros.Agencia BY Ahorros.Nit BY Ahorros.Cod_Ahorro:  
      j = j + 1.
      RUN Progreso.
      CASE Cmb_BAhorros:
        WHEN "Básico" THEN DO:
           IF Ahorros.Sdo_Disponible GE ValIni AND
              Ahorros.Sdo_Disponible LE ValFin THEN DO:
              DISPLAY Ahorros.Agencia         AT 1  FORMAT "999"
                      Ahorros.Nit             AT 5  FORMAT "X(14)"
                      Ahorros.Cod_Ahorro    AT 19 FORMAT "999"
                      Ahorros.Cue_Ahorros     AT 26 FORMAT "X(14)"
                      Ahorros.Tasa            AT 44 FORMAT ">9.999"
                      Ahorros.Plazo           AT 51 FORMAT "9999"
                      Ahorros.Fec_Apertura    AT 61 FORMAT "99/99/9999"
                      Ahorros.Monto_Apertura  AT 73 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Disponible + Ahorros.Sdo_canje AT 89 FORMAT ">,>>>,>>>,>>9"
              WITH FRAME F1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Apertura" THEN DO:
           IF Ahorros.Sdo_Disponible GE ValIni AND
              Ahorros.Sdo_Disponible LE ValFin AND 
              Ahorros.Fec_Apertura   GE FecIni AND 
              Ahorros.Fec_Apertura   LE FecFin THEN DO:
              DISPLAY Ahorros.Agencia         AT 1  FORMAT "999"
                      Ahorros.Nit             AT 5  FORMAT "X(14)"
                      Ahorros.Cod_Ahorro    AT 19 FORMAT "999"
                      Ahorros.Cue_Ahorros     AT 26 FORMAT "X(14)"
                      Ahorros.Tasa            AT 44 FORMAT ">9.999"
                      Ahorros.Plazo           AT 51 FORMAT "9999"
                      Ahorros.Fec_Apertura    AT 61 FORMAT "99/99/9999"
                      Ahorros.Monto_Apertura  AT 73 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Disponible + Ahorros.Sdo_canje  AT 89 FORMAT ">,>>>,>>>,>>9"
              WITH FRAME F2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Cancelacion" THEN DO:
           IF Ahorros.Sdo_Disponible  GE ValIni AND
              Ahorros.Sdo_Disponible  LE ValFin AND 
              Ahorros.Fec_Cancelacion GE FecIni AND 
              Ahorros.Fec_Cancelacion LE FecFin THEN DO:
              DISPLAY Ahorros.Agencia         AT 1  FORMAT "999"
                      Ahorros.Nit             AT 5  FORMAT "X(14)"
                      Ahorros.Cod_Ahorro    AT 19 FORMAT "999"
                      Ahorros.Cue_Ahorros     AT 26 FORMAT "X(14)"
                      Ahorros.Tasa            AT 44 FORMAT ">9.999"
                      Ahorros.Plazo           AT 51 FORMAT "9999"
                      Ahorros.Fec_Apertura    AT 61 FORMAT "99/99/9999"
                      Ahorros.Monto_Apertura  AT 73 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Disponible + Ahorros.Sdo_canje  AT 89 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Fec_Cancelacion AT 105 FORMAT "99/99/9999" 
              WITH FRAME F3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Vencimiento" THEN DO:
           IF Ahorros.Sdo_Disponible  GE ValIni AND
              Ahorros.Sdo_Disponible  LE ValFin AND 
              Ahorros.Fec_Cancelacion GE FecIni AND 
              Ahorros.Fec_Cancelacion LE FecFin THEN DO:
              DISPLAY Ahorros.Agencia         AT 1  FORMAT "999"
                      Ahorros.Nit             AT 5  FORMAT "X(14)"
                      Ahorros.Cod_Ahorro    AT 19 FORMAT "999"
                      Ahorros.Cue_Ahorros     AT 26 FORMAT "X(14)"
                      Ahorros.Tasa            AT 44 FORMAT ">9.999"
                      Ahorros.Plazo           AT 51 FORMAT "9999"
                      Ahorros.Fec_Apertura    AT 61 FORMAT "99/99/9999"
                      Ahorros.Monto_Apertura  AT 73 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Disponible + Ahorros.Sdo_canje  AT 89 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Fec_Vencimiento AT 105 FORMAT "99/99/9999" 
              WITH FRAME F4 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Canje" THEN DO:
           IF Ahorros.Sdo_Canje  GE ValIni AND
              Ahorros.Sdo_Canje  LE ValFin THEN DO:
              DISPLAY Ahorros.Agencia         AT 1  FORMAT "999"
                      Ahorros.Nit             AT 5  FORMAT "X(14)"
                      Ahorros.Cod_Ahorro    AT 19 FORMAT "999"
                      Ahorros.Cue_Ahorros     AT 26 FORMAT "X(14)"
                      Ahorros.Tasa            AT 44 FORMAT ">9.999"
                      Ahorros.Plazo           AT 51 FORMAT "9999"
                      Ahorros.Fec_Apertura    AT 61 FORMAT "99/99/9999"
                      Ahorros.Monto_Apertura  AT 73 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Disponible   AT 89 FORMAT ">,>>>,>>>,>>9"
                      Ahorros.Sdo_Canje       AT 105 FORMAT ">,>>>,>>>,>>9"
              WITH FRAME F5 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Ahorros.Sdo_Disponible + Ahorros.Sdo_canje
                     TotReg    = TotReg + 1.
           END.
        END.
      END CASE.
      IF LAST-OF(Ahorros.Agencia) THEN
         DISPLAY SKIP(1)
                "Total de Registros Agencia "    AT 1
                 Ahorros.Agencia                 AT 30 FORMAT "999"
                ": "                             AT 35
                 TotRegAge                       AT 40
                "Tot.Sdo.Disponible : "          AT 60
                 TotSdoAge                       AT 89
        WITH FRAME t1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
  END.
  DISPLAY SKIP(1)
    "Total de Registros        :"    AT 1
     TotReg                          AT 40
    "Total Disponible   : "          AT 60
     TotSdo                          AT 89
  WITH FRAME tt WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 

  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BtnImp_Clientes wWin 
PROCEDURE BtnImp_Clientes :
Listado = W_PathSpl + "\Clientes.LST".

DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" INITIAL "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        ".
DEFINE VAR TotReg AS DECIMAL.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Clientes: " + Cmb_BClientes:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BClientes:
    WHEN "Básico" THEN  
        W_EncColumna = WEnc1 + "TEL.RESIDENCIA   TEL.COMERCIAL".
    WHEN "Actualización de Información" THEN
        W_EncColumna = "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        FEC.ULT.ACT      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Cumpleaños" THEN
        W_EncColumna = WEnc1 + "FEC.NACIMIENTO   TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Afiliaciones" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Retiros" THEN
        W_EncColumna = WEnc1 + "FEC.RETIRO       TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Proveedores" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Solo Asociados" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Solo Clientes No Asociados" THEN
        W_EncColumna = WEnc1 + "FEC.INGRESO      TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "Habiles" THEN
        W_EncColumna = WEnc1 + "FEC.ULT.ACTUALIZ TEL.REDIDENCIA   TEL.COMERCIAL ".
    WHEN "No Habiles" THEN
                             /* 7    8         9         1
                                567890123456789012345678901234567890*/
        W_EncColumna = WEnc1 + "FEC.ULT.ACTUALIZ TEL.REDIDENCIA   TEL.COMERCIAL ".
  END CASE.
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
  FOR EACH Clientes WHERE 
           Clientes.Agencia GE AgeIni AND
           Clientes.Agencia LE AgeFin NO-LOCK BREAK BY Clientes.Agencia BY Clientes.Nit:  
      j = j + 1.
      RUN Progreso.
      CASE Cmb_BClientes:
        WHEN "Básico" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 THEN DO:
              DISPLAY Clientes.Agencia         AT 1  FORMAT "999"
                      Clientes.Nit             AT 5  FORMAT "X(14)"
                      Clientes.Nombre          AT 20 FORMAT "X(20)"
                      Clientes.Apellido1       AT 42 FORMAT "X(15)"
                      Clientes.Apellido2       AT 59 FORMAT "X(15)"
                      Clientes.Tel_Residencia  AT 76 FORMAT "X(15)"
                      Clientes.Tel_Comercial   AT 93 FORMAT "X(15)"
              WITH FRAME F1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Actualización de Información" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_UltActualiza GE FecIni AND
              Clientes.Fec_UltActualiza LE FecFin THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_UltActualiza AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Cumpleaños" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_Nacimiento GE FecIni AND
              Clientes.Fec_Nacimiento LE FecFin THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Nacimiento   AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Afiliaciones" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_Ingreso GE FecIni AND
              Clientes.Fec_Ingreso LE FecFin THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F4 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Retiros" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_Retiro GE FecIni AND
              Clientes.Fec_Retiro LE FecFin THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Retiro       AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F5 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Proveedores" THEN DO:
           IF Clientes.Tipo_Vinculo GT 2 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F6 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Solo Asociados" THEN DO:
           IF Clientes.Tipo_Vinculo EQ 1 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F7 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Solo Clientes No Asociados" THEN DO:
           IF Clientes.Tipo_Vinculo EQ 2 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_Ingreso      AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F8 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Habiles" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              YEAR(Clientes.Fec_UltActualiza) EQ YEAR(TODAY) AND
              Clientes.Estado EQ 1 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_UltActualiza AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F9 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "No Habiles" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              YEAR(Clientes.Fec_UltActualiza) NE YEAR(TODAY) AND
              Clientes.Estado EQ 1 THEN DO:
              DISPLAY Clientes.Agencia          AT 1  FORMAT "999"
                      Clientes.Nit              AT 5  FORMAT "X(14)"
                      Clientes.Nombre           AT 20 FORMAT "X(20)"
                      Clientes.Apellido1        AT 42 FORMAT "X(15)"
                      Clientes.Apellido2        AT 59 FORMAT "X(15)"
                      Clientes.Fec_UltActualiza AT 76 FORMAT "99/99/9999"
                      Clientes.Tel_Residencia   AT 93 FORMAT "X(15)"
                      Clientes.Tel_Comercial    AT 110 FORMAT "X(15)"
              WITH FRAME F10 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              TotReg = TotReg + 1.
           END.
        END.
      END CASE.
  END.
  DISPLAY SKIP "Total de Registros Reportados: " TotReg WITH FRAME FT WIDTH 132 NO-LABELS.
  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cre_Edades wWin 
PROCEDURE Cre_Edades :
DEFINE VAR Wk        AS INTEGER FORMAT ">>,>>9".
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
FOR EACH Tmp_TotAho: DELETE Tmp_TotAho. END.
  FOR EACH Creditos WHERE 
           Creditos.Agencia        GE AgeIni AND Creditos.Agencia      LE AgeFin AND
           Creditos.Fec_Desembolso GE FecIni AND Creditos.Fec_Desembolso LE FecFin
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Creditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            FIND Tmp_TotAho WHERE Tmp_TotAho.A_Agencia EQ Creditos.Agencia NO-ERROR.
            IF NOT AVAILABLE Tmp_TotAho THEN DO:
               CREATE Tmp_TotAho.
               ASSIGN Tmp_TotAho.A_Agencia = Creditos.Agencia
                      Tmp_TotAho.A_NomAge  = Agencias.Nombre.
            END.
            W_NomAge = Agencias.Nombre.
         END.
      END.
      IF FIRST-OF(Creditos.Nit) THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN
           IF Clientes.Tipo_Cliente LT 3 THEN DO:
             IF Clientes.Fec_Nacimiento NE ? THEN Wk = (W_Fecha - Clientes.Fec_Nacimiento) / 360.
             ELSE Wk = Clientes.Edad.
           END.
         ELSE RETURN.
      END.
      IF AVAILABLE Clientes THEN DO:
        IF Clientes.Tipo_Cliente LT 3 THEN DO:
          IF Wk GE 0  AND Wk LT 18 THEN Val_Hombres[1] = Val_Hombres[1] + Creditos.Sdo_Capital.
          IF Wk GE 18 AND Wk LT 30 THEN Val_Hombres[2] = Val_Hombres[2] + Creditos.Sdo_Capital.
          IF Wk GE 30 AND Wk LT 40 THEN Val_Hombres[3] = Val_Hombres[3] + Creditos.Sdo_Capital.
          IF Wk GE 40 AND Wk LT 50 THEN Val_Hombres[4] = Val_Hombres[4] + Creditos.Sdo_Capital.
          IF Wk GE 50 AND Wk LT 60 THEN Val_Hombres[5] = Val_Hombres[5] + Creditos.Sdo_Capital.
          IF Wk GE 60 AND Wk LT 70 THEN Val_Hombres[6] = Val_Hombres[6] + Creditos.Sdo_Capital.
          IF Wk GE 70 THEN Val_Hombres[7] = Val_Hombres[7] + Creditos.Sdo_Capital.
        END.
        IF LAST-OF(Creditos.Nit) THEN DO:
          IF Wk GE 0  AND Wk LT 18 THEN Con_Hombres[1] = Con_Hombres[1] + 1.
          IF Wk GE 18 AND Wk LT 30 THEN Con_Hombres[2] = Con_Hombres[2] + 1.
          IF Wk GE 30 AND Wk LT 40 THEN Con_Hombres[3] = Con_Hombres[3] + 1.
          IF Wk GE 40 AND Wk LT 50 THEN Con_Hombres[4] = Con_Hombres[4] + 1.
          IF Wk GE 50 AND Wk LT 60 THEN Con_Hombres[5] = Con_Hombres[5] + 1.
          IF Wk GE 60 AND Wk LT 70 THEN Con_Hombres[6] = Con_Hombres[6] + 1.
          IF Wk GE 70 THEN Con_Hombres[7] = Con_Hombres[7] + 1.
        END.
      END.
  END.    
  
  FOR EACH Tmp_TotAho:
         Val_Hombres[8] = Val_Hombres[1] + Val_Hombres[2] + Val_Hombres[3] + Val_Hombres[4] + Val_Hombres[5]
                        + Val_Hombres[6] + Val_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Val_Hombres[1]    AT 27   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[2]    AT 40   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[3]    AT 53   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[4]    AT 66   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[5]    AT 79   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[6]    AT 92   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[7]    AT 105  FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[8]    AT 119  FORMAT ">>>>,>>>,>>9"
         WITH FRAME F_AhoEda WIDTH 140 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Val_Hombres[1] Tot2 = Tot2 + Val_Hombres[2]
                Tot3 = Tot3 + Val_Hombres[3] Tot4 = Tot4 + Val_Hombres[4]
                Tot5 = Tot5 + Val_Hombres[5] Tot6 = Tot6 + Val_Hombres[6]
                Tot7 = Tot7 + Val_Hombres[7] Tot8 = Tot8 + Val_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Valores :" AT 1
          Tot1  AT 27   FORMAT ">>>>,>>>,>>9"
          Tot2  AT 40   FORMAT ">>>>,>>>,>>9"
          Tot3  AT 53   FORMAT ">>>>,>>>,>>9"
          Tot4  AT 66   FORMAT ">>>>,>>>,>>9"
          Tot5  AT 79   FORMAT ">>>>,>>>,>>9"
          Tot6  AT 92   FORMAT ">>>>,>>>,>>9"
          Tot7  AT 105  FORMAT ">>>>,>>>,>>9"
          Tot8  AT 119  FORMAT ">>>>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotVal WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
  DISPLA SKIP(2)
         "Total de Personas..." AT 1 SKIP(1)
         "AGE NOMBRE AGENCIA             0 - 17      18 - 29      30 - 39       40 - 49      50 - 59      60 - 69     70 o mas    TOTAL" AT 1 
         WITH FRAME F_Inter2 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
  
  FOR EACH Tmp_TotAho:
         Con_Hombres[8] = Con_Hombres[1] + Con_Hombres[2] + Con_Hombres[3] + Con_Hombres[4] + Con_Hombres[5] + Con_Hombres[6] + Con_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Con_Hombres[1]    AT 27   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[2]    AT 40   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[3]    AT 53   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[4]    AT 66   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[5]    AT 79   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[6]    AT 92   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[7]    AT 105  FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[8]    AT 119  FORMAT ">>>>,>>>,>>9"
         WITH FRAME F_AhoEda2 WIDTH 140 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Con_Hombres[1] Tot2 = Tot2 + Con_Hombres[2]
                Tot3 = Tot3 + Con_Hombres[3] Tot4 = Tot4 + Con_Hombres[4]
                Tot5 = Tot5 + Con_Hombres[5] Tot6 = Tot6 + Con_Hombres[6]
                Tot7 = Tot7 + Con_Hombres[7] Tot8 = Tot8 + Con_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Personas:" AT 1
          Tot1  AT 27   FORMAT ">>>>,>>>,>>9"
          Tot2  AT 40   FORMAT ">>>>,>>>,>>9"
          Tot3  AT 53   FORMAT ">>>>,>>>,>>9"
          Tot4  AT 66   FORMAT ">>>>,>>>,>>9"
          Tot5  AT 79   FORMAT ">>>>,>>>,>>9"
          Tot6  AT 92   FORMAT ">>>>,>>>,>>9"
          Tot7  AT 105  FORMAT ">>>>,>>>,>>9"
          Tot8  AT 119  FORMAT ">>>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotVal2 WIDTH 140 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cre_Estadisticos wWin 
PROCEDURE Cre_Estadisticos :
DEFINE VAR Wk        AS INTEGER FORMAT ">>,>>9".
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".

DEFINE VAR Cantidad    AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR Acumulado   AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99".
DEFINE VAR Promedio    AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TCantidad    AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR TAcumulado   AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TPromedio    AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99".


DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
  FOR EACH Creditos WHERE 
           Creditos.Agencia        GE AgeIni AND Creditos.Agencia        LE AgeFin AND
           Creditos.Fec_Desembolso GE FecIni AND Creditos.Fec_Desembolso LE FecFin AND
           (Creditos.Estado EQ 2)
           NO-LOCK BREAK BY Creditos.Cod_Credito BY Creditos.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Creditos.Cod_Credito) THEN DO:
         FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
      END.
      ASSIGN Cantidad = Cantidad + 1
             Acumulado = Acumulado + Creditos.Monto.
      IF LAST-OF(Creditos.Cod_Credito) THEN DO:
         Promedio = (Acumulado / Cantidad).
         DISPLAY Pro_Creditos.Cod_Credito
                 Pro_Creditos.Nom_Producto
                 Cantidad
                 Acumulado
                 Promedio

         WITH FRAME FDET WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN TCantidad  = TCantidad + Cantidad
                TAcumulado = TAcumulado + Acumulado
                TPromedio  = TPromedio  + Promedio
                Cantidad  = 0
                Acumulado = 0.

      END.
             

  END.    
  
  DISPLAY SKIP(1)
          "Total final               "    
           TCantidad 
           TAcumulado 
           TPromedio
  WITH FRAME F_TotNiv WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cre_EstCivil wWin 
PROCEDURE Cre_EstCivil :
DEFINE VAR Wk        AS INTEGER FORMAT ">>,>>9".
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
FOR EACH Tmp_TotAho: DELETE Tmp_TotAho. END.
  FOR EACH Creditos WHERE 
           Creditos.Agencia        GE AgeIni AND Creditos.Agencia        LE AgeFin AND
           Creditos.Fec_Desembolso GE FecIni AND Creditos.Fec_Desembolso LE FecFin
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Creditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            FIND Tmp_TotAho WHERE Tmp_TotAho.A_Agencia EQ Creditos.Agencia NO-ERROR.
            IF NOT AVAILABLE Tmp_TotAho THEN DO:
               CREATE Tmp_TotAho.
               ASSIGN Tmp_TotAho.A_Agencia = Creditos.Agencia
                      Tmp_TotAho.A_NomAge  = Agencias.Nombre.
            END.
            W_NomAge = Agencias.Nombre.
         END.
      END.
      IF FIRST-OF(Creditos.Nit) THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE Clientes THEN DO:
       IF Clientes.Tipo_Cliente LT 3 THEN DO:
          CASE Clientes.Est_Civil:
            WHEN "Soltero"     THEN Val_Hombres[1] = Val_Hombres[1] + Creditos.Sdo_Capital.
            WHEN "Casado"      THEN Val_Hombres[2] = Val_Hombres[2] + Creditos.Sdo_Capital.
            WHEN "Separado"    THEN Val_Hombres[3] = Val_Hombres[3] + Creditos.Sdo_Capital.
            WHEN "Viudo"       THEN Val_Hombres[4] = Val_Hombres[4] + Creditos.Sdo_Capital.
            WHEN "Unión Libre" THEN Val_Hombres[5] = Val_Hombres[5] + Creditos.Sdo_Capital.
          END CASE.
       END.
       IF LAST-OF(Creditos.Nit) AND Clientes.Tipo_Cliente LT 3 THEN DO:
        IF Clientes.Tipo_Cliente LT 3 THEN DO:
           CASE Clientes.Est_Civil:
             WHEN "Soltero"     THEN Con_Hombres[1] = Con_Hombres[1] + 1.
             WHEN "Casado"      THEN Con_Hombres[2] = Con_Hombres[2] + 1.
             WHEN "Separado"    THEN Con_Hombres[3] = Con_Hombres[3] + 1.
             WHEN "Viudo"       THEN Con_Hombres[4] = Con_Hombres[4] + 1.
             WHEN "Unión Libre" THEN Con_Hombres[5] = Con_Hombres[5] + 1.
           END CASE.
        END.
       END.
      END.
  END.    
  
  FOR EACH Tmp_TotAho:
         Val_Hombres[6] = Val_Hombres[1] + Val_Hombres[2] + Val_Hombres[3] + Val_Hombres[4] + Val_Hombres[5].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Val_Hombres[1]    AT 27   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[2]    AT 40   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[3]    AT 53   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[4]    AT 66   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[5]    AT 79   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[6]    AT 92   FORMAT ">>>>,>>>,>>9"
         WITH FRAME F_AhoEda WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Val_Hombres[1] Tot2 = Tot2 + Val_Hombres[2]
                Tot3 = Tot3 + Val_Hombres[3] Tot4 = Tot4 + Val_Hombres[4]
                Tot5 = Tot5 + Val_Hombres[5] Tot6 = Tot6 + Val_Hombres[6].
  END.
  DISPLAY SKIP(1)
          "Total Valores :" AT 1
          Tot1  AT 27   FORMAT ">>>>,>>>,>>9"
          Tot2  AT 40   FORMAT ">>>>,>>>,>>9"
          Tot3  AT 53   FORMAT ">>>>,>>>,>>9"
          Tot4  AT 66   FORMAT ">>>>,>>>,>>9"
          Tot5  AT 79   FORMAT ">>>>,>>>,>>9"
          Tot6  AT 92   FORMAT ">>>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotVal WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0.
  DISPLA SKIP(2)
         "Total de Personas..." AT 1 SKIP(1)
         "AGE NOMBRE AGENCIA            SOLTEROS      CASADOS    SEPARADOS     VIUDOS     UNION LIBRE    TOTAL" AT 1  
         WITH FRAME F_Inter2 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
  
  FOR EACH Tmp_TotAho:
         Con_Hombres[6] = Con_Hombres[1] + Con_Hombres[2] + Con_Hombres[3] + Con_Hombres[4] + Con_Hombres[5].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Con_Hombres[1]    AT 27   FORMAT ">>>>,>>>,>>9" 
                 Con_Hombres[2]    AT 40   FORMAT ">>>>,>>>,>>9" 
                 Con_Hombres[3]    AT 53   FORMAT ">>>>,>>>,>>9" 
                 Con_Hombres[4]    AT 66   FORMAT ">>>>,>>>,>>9" 
                 Con_Hombres[5]    AT 79   FORMAT ">>>>,>>>,>>9" 
                 Con_Hombres[6]    AT 92   FORMAT ">>>>,>>>,>>9" 
         WITH FRAME F_AhoEda2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Con_Hombres[1] Tot2 = Tot2 + Con_Hombres[2]
                Tot3 = Tot3 + Con_Hombres[3] Tot4 = Tot4 + Con_Hombres[4]
                Tot5 = Tot5 + Con_Hombres[5] Tot6 = Tot6 + Con_Hombres[6].
  END.
  DISPLAY SKIP(1)
          "Total Personas:" AT 1
          Tot1  AT 27   FORMAT ">>>>,>>>,>>9" 
          Tot2  AT 40   FORMAT ">>>>,>>>,>>9" 
          Tot3  AT 53   FORMAT ">>>>,>>>,>>9" 
          Tot4  AT 66   FORMAT ">>>>,>>>,>>9" 
          Tot5  AT 79   FORMAT ">>>>,>>>,>>9" 
          Tot6  AT 92   FORMAT ">>>>,>>>,>>9" 
          SKIP(2)
  WITH FRAME F_TotVal2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cre_NivEducativo wWin 
PROCEDURE Cre_NivEducativo :
DEFINE VAR Wk        AS INTEGER FORMAT ">>,>>9".
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
FOR EACH Tmp_TotAho: DELETE Tmp_TotAho. END.
  FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND Creditos.Agencia      LE AgeFin AND
           Creditos.Fec_Desembolso GE FecIni AND Creditos.Fec_Desembolso LE FecFin
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Creditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            FIND Tmp_TotAho WHERE Tmp_TotAho.A_Agencia EQ Creditos.Agencia NO-ERROR.
            IF NOT AVAILABLE Tmp_TotAho THEN DO:
               CREATE Tmp_TotAho.
               ASSIGN Tmp_TotAho.A_Agencia = Creditos.Agencia
                      Tmp_TotAho.A_NomAge  = Agencias.Nombre.
            END.
         END.
      END.
      IF FIRST-OF(Creditos.Nit) THEN FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN DO:
        IF Clientes.Tipo_Cliente LT 3 THEN DO:
           CASE Clientes.Niv_Educativo:
             WHEN "Ninguno"     THEN Val_Hombres[1] = Val_Hombres[1] + Creditos.Sdo_Capital.
             WHEN "Primaria"    THEN Val_Hombres[2] = Val_Hombres[2] + Creditos.Sdo_Capital.
             WHEN "Bachiller"   THEN Val_Hombres[3] = Val_Hombres[3] + Creditos.Sdo_Capital.
             WHEN "Tecnológo"   THEN Val_Hombres[4] = Val_Hombres[4] + Creditos.Sdo_Capital.
             WHEN "Técnico"     THEN Val_Hombres[5] = Val_Hombres[5] + Creditos.Sdo_Capital.
             WHEN "Profesional" THEN Val_Hombres[6] = Val_Hombres[6] + Creditos.Sdo_Capital.
             WHEN "Post-Grado"  THEN Val_Hombres[7] = Val_Hombres[7] + Creditos.Sdo_Capital.
           END CASE.
        END.
        IF LAST-OF(Creditos.Nit) AND Clientes.Tipo_Cliente LT 3 THEN DO:
          IF Clientes.Tipo_Cliente LT 3 THEN DO:
             CASE Clientes.Niv_Educativo:
              WHEN "Ninguno"     THEN Con_Hombres[1] = Con_Hombres[1] + 1.
              WHEN "Primaria"    THEN Con_Hombres[2] = Con_Hombres[2] + 1.
              WHEN "Bachiller"   THEN Con_Hombres[3] = Con_Hombres[3] + 1.
              WHEN "Tecnológo"   THEN Con_Hombres[4] = Con_Hombres[4] + 1.
              WHEN "Técnico"     THEN Con_Hombres[5] = Con_Hombres[5] + 1.
              WHEN "Profesional" THEN Con_Hombres[6] = Con_Hombres[6] + 1.
              WHEN "Post-Grado"  THEN Con_Hombres[7] = Con_Hombres[7] + 1.
            END CASE.
         END.
        END.
      END.
  END.    
  
  FOR EACH Tmp_TotAho:
         Val_Hombres[8] = Val_Hombres[1] + Val_Hombres[2] + Val_Hombres[3] + 
         Val_Hombres[4] + Val_Hombres[5] + Val_Hombres[6] + Val_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Val_Hombres[1]    AT 27   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[2]    AT 40   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[3]    AT 53   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[4]    AT 66   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[5]    AT 79   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[6]    AT 92   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[7]    AT 105  FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[8]    AT 119  FORMAT ">>>>,>>>,>>9"
         WITH FRAME F_AhoNiv WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Val_Hombres[1] Tot2 = Tot2 + Val_Hombres[2]
                Tot3 = Tot3 + Val_Hombres[3] Tot4 = Tot4 + Val_Hombres[4]
                Tot5 = Tot5 + Val_Hombres[5] Tot6 = Tot6 + Val_Hombres[6]
                Tot7 = Tot7 + Val_Hombres[7] Tot8 = Tot8 + Val_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Valores :" AT 1
          Tot1  AT 27   FORMAT ">>>>,>>>,>>9"
          Tot2  AT 40   FORMAT ">>>>,>>>,>>9"
          Tot3  AT 53   FORMAT ">>>>,>>>,>>9"
          Tot4  AT 66   FORMAT ">>>>,>>>,>>9"
          Tot5  AT 79   FORMAT ">>>>,>>>,>>9"
          Tot6  AT 92   FORMAT ">>>>,>>>,>>9"
          Tot7  AT 105  FORMAT ">>>>,>>>,>>9"
          Tot8  AT 119  FORMAT ">>>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotNiv WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
  DISPLA SKIP(2)
         "Total de Personas..." AT 1 SKIP(1)
         "AGE NOMBRE AGENCIA           NINGUNO       PRIMARIA   BACHILLER    TECNOLÓGO     TÉCNICO      PROFESION    POST-GRADO   TOTAL" AT 1  
         WITH FRAME F_Inter2 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
  
  FOR EACH Tmp_TotAho:
         Con_Hombres[8] = Con_Hombres[1] + Con_Hombres[2] + Con_Hombres[3] + 
         Con_Hombres[4] + Con_Hombres[5] + Con_Hombres[6] + Con_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Con_Hombres[1]    AT 27   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[2]    AT 40   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[3]    AT 53   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[4]    AT 66   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[5]    AT 79   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[6]    AT 92   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[7]    AT 105  FORMAT ">>>>,>>>,>>9" 
                 Con_Hombres[8]    AT 119  FORMAT ">>>>,>>>,>>9" 
         WITH FRAME F_AhoNiv2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Con_Hombres[1] Tot2 = Tot2 + Con_Hombres[2]
                Tot3 = Tot3 + Con_Hombres[3] Tot4 = Tot4 + Con_Hombres[4]
                Tot5 = Tot5 + Con_Hombres[5] Tot6 = Tot6 + Con_Hombres[6]
                Tot7 = Tot7 + Con_Hombres[7] Tot8 = Tot8 + Con_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Personas:" AT 1
          Tot1  AT 27   FORMAT ">>>>,>>>,>>9"
          Tot2  AT 40   FORMAT ">>>>,>>>,>>9"
          Tot3  AT 53   FORMAT ">>>>,>>>,>>9"
          Tot4  AT 66   FORMAT ">>>>,>>>,>>9"
          Tot5  AT 79   FORMAT ">>>>,>>>,>>9"
          Tot6  AT 92   FORMAT ">>>>,>>>,>>9"
          Tot7  AT 105  FORMAT ">>>>,>>>,>>9"
          Tot8  AT 119  FORMAT ">>>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotNiv2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cre_Salarios wWin 
PROCEDURE Cre_Salarios :
DEFINE VAR Wk        AS INTEGER FORMAT ">>,>>9".
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
FOR EACH Tmp_TotAho: DELETE Tmp_TotAho. END.
  FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND Creditos.Agencia      LE AgeFin AND
           Creditos.Fec_Desembolso GE FecIni AND Creditos.Fec_Desembolso LE FecFin
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Creditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            FIND Tmp_TotAho WHERE Tmp_TotAho.A_Agencia EQ Creditos.Agencia NO-ERROR.
            IF NOT AVAILABLE Tmp_TotAho THEN DO:
               CREATE Tmp_TotAho.
               ASSIGN Tmp_TotAho.A_Agencia = Creditos.Agencia
                      Tmp_TotAho.A_NomAge  = Agencias.Nombre.
            END.
         END.
      END.
      IF FIRST-OF(Creditos.Nit) THEN FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN DO:
        IF Clientes.Tipo_Cliente LT 3 THEN DO:
           IF Clientes.Salario GE 0  AND Clientes.Salario LE Minimo THEN                    
              Val_Hombres[1] = Val_Hombres[1] + Creditos.Sdo_Capital.
           IF Clientes.Salario GE (Minimo + 1)     AND Clientes.Salario LE (Minimo * 2) THEN
              Val_Hombres[2] = Val_Hombres[2] + Creditos.Sdo_Capital.
           IF Clientes.Salario GE (Minimo * 2) + 1 AND Clientes.Salario LE (Minimo * 3) THEN
              Val_Hombres[3] = Val_Hombres[3] + Creditos.Sdo_Capital.
           IF Clientes.Salario GE (Minimo * 3) + 1 AND Clientes.Salario LT (Minimo * 4) THEN
              Val_Hombres[4] = Val_Hombres[4] + Creditos.Sdo_Capital.
           IF Clientes.Salario GE (Minimo * 4) + 1 AND Clientes.Salario LT (Minimo * 5) THEN
              Val_Hombres[5] = Val_Hombres[5] + Creditos.Sdo_Capital.
           IF Clientes.Salario GE (Minimo * 5) + 1 AND Clientes.Salario LT (Minimo * 6) THEN
              Val_Hombres[6] = Val_Hombres[6] + Creditos.Sdo_Capital.
           IF Clientes.Salario GE (Minimo * 6) + 1 THEN                                     
              Val_Hombres[7] = Val_Hombres[7] + Creditos.Sdo_Capital.
        END.
        IF LAST-OF(Creditos.Nit) AND Clientes.Tipo_Cliente LT 3 THEN DO:
         IF Clientes.Tipo_Cliente LT 3 THEN DO:
           IF Clientes.Salario GE 0  AND Clientes.Salario LE Minimo THEN                    
              Con_Hombres[1] = Con_Hombres[1] + 1.
           IF Clientes.Salario GE (Minimo + 1)     AND Clientes.Salario LE (Minimo * 2) THEN
              Con_Hombres[2] = Con_Hombres[2] + 1.
           IF Clientes.Salario GE (Minimo * 2) + 1 AND Clientes.Salario LE (Minimo * 3) THEN
              Con_Hombres[3] = Con_Hombres[3] + 1.
           IF Clientes.Salario GE (Minimo * 3) + 1 AND Clientes.Salario LT (Minimo * 4) THEN
              Con_Hombres[4] = Con_Hombres[4] + 1.
           IF Clientes.Salario GE (Minimo * 4) + 1 AND Clientes.Salario LT (Minimo * 5) THEN
              Con_Hombres[5] = Con_Hombres[5] + 1.
           IF Clientes.Salario GE (Minimo * 5) + 1 AND Clientes.Salario LT (Minimo * 6) THEN
              Con_Hombres[6] = Con_Hombres[6] + 1.
           IF Clientes.Salario GE (Minimo * 6) + 1 THEN                                     
              Con_Hombres[7] = Con_Hombres[7] + 1.
         END.
        END.
      END.
  END.    
  
  FOR EACH Tmp_TotAho:
         Val_Hombres[8] = Val_Hombres[1] + Val_Hombres[2] + Val_Hombres[3] + 
         Val_Hombres[4] + Val_Hombres[5] + Val_Hombres[6] + Val_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Val_Hombres[1]    AT 27   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[2]    AT 40   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[3]    AT 53   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[4]    AT 66   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[5]    AT 79   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[6]    AT 92   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[7]    AT 105  FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[8]    AT 119  FORMAT ">>>>,>>>,>>9"
         WITH FRAME F_AhoNiv WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Val_Hombres[1] Tot2 = Tot2 + Val_Hombres[2]
                Tot3 = Tot3 + Val_Hombres[3] Tot4 = Tot4 + Val_Hombres[4]
                Tot5 = Tot5 + Val_Hombres[5] Tot6 = Tot6 + Val_Hombres[6]
                Tot7 = Tot7 + Val_Hombres[7] Tot8 = Tot8 + Val_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Valores :" AT 1
          Tot1  AT 27   FORMAT ">>>>,>>>,>>9"
          Tot2  AT 40   FORMAT ">>>>,>>>,>>9"
          Tot3  AT 53   FORMAT ">>>>,>>>,>>9"
          Tot4  AT 66   FORMAT ">>>>,>>>,>>9"
          Tot5  AT 79   FORMAT ">>>>,>>>,>>9"
          Tot6  AT 92   FORMAT ">>>>,>>>,>>9"
          Tot7  AT 105  FORMAT ">>>>,>>>,>>9"
          Tot8  AT 119  FORMAT ">>>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotNiv WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
  DISPLA SKIP(2)
         "Total de Personas..." AT 1 SKIP(1)
         "AGE NOMBRE AGENCIA              0 - 1       1 - 2        2 - 3       3 - 4        4 - 5         5 - 6      6 - o mas    TOTAL" AT 1  
         WITH FRAME F_Inter2 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
  
  FOR EACH Tmp_TotAho:
         Con_Hombres[8] = Con_Hombres[1] + Con_Hombres[2] + Con_Hombres[3] + 
         Con_Hombres[4] + Con_Hombres[5] + Con_Hombres[6] + Con_Hombres[7].
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Con_Hombres[1]    AT 27   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[2]    AT 40   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[3]    AT 53   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[4]    AT 66   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[5]    AT 79   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[6]    AT 92   FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[7]    AT 105  FORMAT ">>>>,>>>,>>9" 
                 Con_Hombres[8]    AT 119  FORMAT ">>>>,>>>,>>9" 
         WITH FRAME F_AhoNiv2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Con_Hombres[1] Tot2 = Tot2 + Con_Hombres[2]
                Tot3 = Tot3 + Con_Hombres[3] Tot4 = Tot4 + Con_Hombres[4]
                Tot5 = Tot5 + Con_Hombres[5] Tot6 = Tot6 + Con_Hombres[6]
                Tot7 = Tot7 + Con_Hombres[7] Tot8 = Tot8 + Con_Hombres[8].
  END.
  DISPLAY SKIP(1)
          "Total Personas:" AT 1
          Tot1  AT 27   FORMAT ">>>>,>>>,>>9"
          Tot2  AT 40   FORMAT ">>>>,>>>,>>9"
          Tot3  AT 53   FORMAT ">>>>,>>>,>>9"
          Tot4  AT 66   FORMAT ">>>>,>>>,>>9"
          Tot5  AT 79   FORMAT ">>>>,>>>,>>9"
          Tot6  AT 92   FORMAT ">>>>,>>>,>>9"
          Tot7  AT 105  FORMAT ">>>>,>>>,>>9" 
          Tot8  AT 119  FORMAT ">>>>,>>>,>>9" 
          SKIP(2)
  WITH FRAME F_TotNiv2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cre_Sexo wWin 
PROCEDURE Cre_Sexo :
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
FOR EACH Tmp_TotAho: DELETE Tmp_TotAho. END.
  FOR EACH Creditos WHERE 
           Creditos.Agencia        GE AgeIni AND Creditos.Agencia        LE AgeFin AND
           Creditos.Fec_Desembolso GE FecIni AND Creditos.Fec_Desembolso LE FecFin
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Creditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            FIND Tmp_TotAho WHERE Tmp_TotAho.A_Agencia EQ Creditos.Agencia NO-ERROR.
            IF NOT AVAILABLE Tmp_TotAho THEN DO:
               CREATE Tmp_TotAho.
               ASSIGN Tmp_TotAho.A_Agencia = Creditos.Agencia
                      Tmp_TotAho.A_NomAge  = Agencias.Nombre.
            END.
            W_NomAge = Agencias.Nombre.
         END.
      END.
      IF FIRST-OF(Creditos.Nit) THEN DO: 
         FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Clientes THEN NEXT.
      END.
      IF AVAILABLE Clientes THEN DO:
       IF Clientes.Tipo_Cliente LT 3 THEN
         IF Clientes.Sexo EQ 1 THEN 
            ASSIGN Val_Hombres[Creditos.Tip_Credito] = Val_Hombres[Creditos.Tip_Credito] + Creditos.Sdo_Capital
                   Con_Hombres[Creditos.Tip_Credito] = Con_Hombres[Creditos.Tip_Credito] + 1.
         ELSE
            ASSIGN Val_Mujeres[Creditos.Tip_Credito] = Val_Mujeres[Creditos.Tip_Credito] + Creditos.Sdo_Capital
                   Con_Mujeres[Creditos.Tip_Credito] = Con_Mujeres[Creditos.Tip_Credito] + 1.
      END.
  END.    
  DISPLA "                          *----------------CLIENTES HOMBRES-----------------*  *----------------CLIENTES MUJERES---------------*" AT 1
         "AGE NOMBRE AGENCIA           CONSUMO     COMERCIAL    HIPOTECARIO    MICROCRE     CONSUMO     COMERCIAL     HIPOTECAR   MICROCRE"  AT 1 
         WITH FRAME F_Intcre WIDTH 140 NO-LABEL NO-BOX USE-TEXT STREAM-IO.
  FOR EACH Tmp_TotAho:
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Val_Hombres[1]    AT 27   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[2]    AT 40   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[3]    AT 53   FORMAT ">>>>,>>>,>>9"
                 Val_Hombres[4]    AT 66   FORMAT ">>>>,>>>,>>9"
                 Val_Mujeres[1]    AT 79   FORMAT ">>>>,>>>,>>9"
                 Val_Mujeres[2]    AT 92   FORMAT ">>>>,>>>,>>9"
                 Val_Mujeres[3]    AT 105  FORMAT ">>>>,>>>,>>9"
                 Val_Mujeres[4]    AT 119  FORMAT ">>>>,>>>,>>9"
         WITH FRAME F_creSex WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Val_Hombres[1] Tot2 = Tot2 + Val_Hombres[2]
                Tot3 = Tot3 + Val_Hombres[3] Tot4 = Tot4 + Val_Hombres[4]
                Tot5 = Tot5 + Val_Mujeres[1] Tot6 = Tot6 + Val_Mujeres[2]
                Tot7 = Tot7 + Val_Mujeres[3] Tot8 = Tot8 + Val_Mujeres[4].
  END.
  DISPLAY SKIP(1)
          "Total Valores :" AT 1
          Tot1  AT 27   FORMAT ">>>>,>>>,>>9"
          Tot2  AT 40   FORMAT ">>>>,>>>,>>9"
          Tot3  AT 53   FORMAT ">>>>,>>>,>>9"
          Tot4  AT 66   FORMAT ">>>>,>>>,>>9"
          Tot5  AT 79   FORMAT ">>>>,>>>,>>9"
          Tot6  AT 92   FORMAT ">>>>,>>>,>>9"
          Tot7  AT 105  FORMAT ">>>>,>>>,>>9"
          Tot8  AT 119   FORMAT ">>>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotVal WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8 = 0.
  DISPLA SKIP(2)
         "Total de Personas..." AT 1 SKIP(1)
         "                          *----------------CLIENTES HOMBRES-----------------*  *----------------CLIENTES MUJERES-----------------*" AT 1
         "AGE NOMBRE AGENCIA           CONSUMO     COMERCIAL    HIPOTECARIO    MICROCRE     CONSUMO     COMERCIAL     HIPOTECAR     MICROCRE"  AT 1 
         WITH FRAME F_Intcre2 WIDTH 140 NO-LABEL NO-BOX USE-TEXT.
  FOR EACH Tmp_TotAho:
         DISPLAY Tmp_TotAho.A_Agencia        AT 1
                 Tmp_TotAho.A_NomAge         AT 5 FORMAT "X(20)"
                 Con_Hombres[1]   AT 27  FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[2]   AT 40  FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[3]   AT 53  FORMAT ">>>>,>>>,>>9"
                 Con_Hombres[4]   AT 66  FORMAT ">>>>,>>>,>>9"
                 Con_Mujeres[1]   AT 79  FORMAT ">>>>,>>>,>>9"
                 Con_Mujeres[2]   AT 92  FORMAT ">>>>,>>>,>>9"
                 Con_Mujeres[3]   AT 105 FORMAT ">>>>,>>>,>>9"
                 Con_Mujeres[4]   AT 119 FORMAT ">>>>,>>>,>>9"
         WITH FRAME F_creSex2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
         ASSIGN Tot1 = Tot1 + Con_Hombres[1] Tot2 = Tot2 + Con_Hombres[2] 
                Tot3 = Tot3 + Con_Hombres[3] Tot4 = Tot4 + Con_Hombres[4]
                Tot5 = Tot5 + Con_Mujeres[1] Tot6 = Tot6 + Con_Mujeres[2]
                Tot7 = Tot7 + Con_Mujeres[3] Tot8 = Tot8 + Con_Mujeres[4].
  END.
  DISPLAY SKIP(1)
          "Total Personas:" AT 1
          Tot1  AT 27   FORMAT ">>>>,>>>,>>9"
          Tot2  AT 40   FORMAT ">>>>,>>>,>>9"
          Tot3  AT 53   FORMAT ">>>>,>>>,>>9"
          Tot4  AT 66   FORMAT ">>>>,>>>,>>9"
          Tot5  AT 79   FORMAT ">>>>,>>>,>>9"
          Tot6  AT 92   FORMAT ">>>>,>>>,>>9"
          Tot7  AT 105  FORMAT ">>>>,>>>,>>9"
          Tot8  AT 119   FORMAT ">>>>,>>>,>>9"
          SKIP(2)
  WITH FRAME F_TotVal2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Tipo Cmb_BCreditos Cmb_BUsuarios Cmb_BClientes E1 
      WITH FRAME F_Basicos IN WINDOW wWin.
  ENABLE Cmb_Tipo Cmb_BCreditos Cmb_BUsuarios Cmb_BClientes BUTTON-143 
         BUTTON-153 BUTTON-144 BUTTON-149 E1 BUTTON-148 BUTTON-150 BUTTON-152 
      WITH FRAME F_Basicos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Basicos}
  DISPLAY Buscar 
      WITH FRAME F_Buscar IN WINDOW wWin.
  ENABLE Buscar BUTTON-154 Btn_anterior Btn_siguiente 
      WITH FRAME F_Buscar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Buscar}
  DISPLAY Cmb_Agencias RRFechas W_DiaIni AnoIni W_DiaFin AnoFin Cmb_TProducto 
          Cmb_Productos ValIni ValFin RFpago 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE Img_MesF Img_MesI RECT-282 RECT-283 Cmb_Agencias RRFechas RFpago 
         BUTTON-145 
      WITH FRAME F_Filtros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Filtros}
  ENABLE IMAGE-2 R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
MESSAGE "Opción deshabilitada" VIEW-AS ALERT-BOX INFORMATION.
/*CASE Cmb_Tipo:
  WHEN "Clientes" THEN RUN ImpEx_Clientes.
  WHEN "Ahorros"  THEN RUN ImpEx_Ahorros.
END CASE.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InformesCortoLargoCorte wWin 
PROCEDURE InformesCortoLargoCorte :
DEFI VAR DiaPdo AS DEC FORM "9999.9".
DEFI VAR CuoFal AS INTEG FORM "9999".
DEFI VAR NN AS INTEG FORM "9999".
DEFI VAR Sdo LIKE rep_creditos.Sdo_Capital.
DEFI VAR Cap LIKE rep_creditos.Sdo_Capital.
DEFI VAR INT LIKE rep_creditos.Sdo_Capital.
DEFI VAR KCuo LIKE rep_creditos.Cuota.
DEFI VAR W_Tasa LIKE rep_creditos.Tasa.
DEFI VAR Tpdtos LIKE rep_creditos.Sdo_Capital EXTENT 2.
DEFI VAR TGen LIKE rep_creditos.Sdo_Capital EXTENT 2.

EMPTY TEMP-TABLE TCorLar.

FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fecFin
                        AND rep_creditos.Estado EQ 2 /*AND rep_creditos.cod_credito = 123 AND agencia = 1*/
                        AND rep_creditos.Sdo_Capital GT 0 NO-LOCK BREAK BY rep_creditos.Agencia BY rep_creditos.Cod_Credito:
    IF FIRST-OF(Cod_Credito) THEN DO:
        FIND FIRST Pro_Credito WHERE Pro_Credito.Cod_Credito EQ rep_creditos.Cod_Credito NO-LOCK NO-ERROR.
        FIND FIRST TCorLar WHERE TCorLar.Age EQ rep_creditos.Agencia
                             AND TCorLar.Pto EQ rep_creditos.Cod_Credito NO-ERROR.
        IF NOT AVAIL(TCorLar) THEN
            CREATE TCorLar.
    END.

    ASSIGN DiaPdo = rep_creditos.Plazo
           CuoFal = rep_creditos.Plazo - rep_creditos.Cuo_Pagadas
           W_Tasa = rep_creditos.Tasa / 1200
           KCuo = rep_creditos.Cuota.

    IF kCuo = ? THEN
        KCuo = 0.

    IF Per_Pago EQ 1 THEN
        ASSIGN DiaPdo = rep_creditos.Plazo / 4.33
               CuoFal = ROUND((rep_creditos.Plazo - rep_creditos.Cuo_Pagadas) / 4.33,0)
               KCuo = rep_creditos.Cuota * 4.33.
    ELSE
        IF Per_Pago EQ 2 THEN
            ASSIGN DiaPdo = rep_creditos.Plazo / 3
                   CuoFal = ROUND((rep_creditos.Plazo - rep_creditos.Cuo_Pagadas) / 3,0)
                   KCuo = rep_creditos.Cuota * 3.
        ELSE
            IF Per_Pago EQ 3 THEN
                ASSIGN DiaPdo = rep_creditos.Plazo / 2
                       CuoFal = ROUND((rep_creditos.Plazo - rep_creditos.Cuo_Pagadas) / 2,0)
                       KCuo = rep_creditos.Cuota * 2.

    IF DiaPdo LE 12 OR CuoFal LE 12 OR rep_creditos.Sistema NE 1 THEN
        ASSIGN TCorLar.Age = rep_creditos.Agencia
               TCorLar.Pto = rep_creditos.Cod_Credito
               TCorLar.Nom = Pro_creditos.Nom_Prod
               TCorLar.VCor = TCorLar.VCor + rep_creditos.Sdo_Capital.
    ELSE DO:
        Sdo = rep_creditos.Sdo_Capital.

        DO NN = 1 TO CuoFal:
            ASSIGN Int = ROUND(Sdo * W_Tasa,0)
                   Cap = ROUND(KCuo - Int,0).

            IF Cap LE 0 THEN
                Cap = 0.
            ELSE
                IF Cap GT Sdo THEN
                    Cap = Sdo.

            TCorLar.Age = rep_creditos.Agencia.
            TCorLar.Pto = rep_creditos.Cod_Credito.
            TCorLar.Nom = Pro_creditos.Nom_Prod.
                
            IF NN LE 12 THEN
                TCorLar.VCor = TCorLar.VCor + Cap.
            ELSE
                TCorLar.VLar = TCorLar.VLar + Cap.

            Sdo = Sdo - Cap.

            IF Sdo LE 0 THEN
                LEAVE.
            ELSE
                IF NN = CuoFal THEN DO:
                    TCorLar.Age = rep_creditos.Agencia.
                    TCorLar.Pto = rep_creditos.Cod_Credito.
                    TCorLar.Nom = Pro_creditos.Nom_Prod.

                    IF NN LE 12 THEN
                        TCorLar.VCor = TCorLar.VCor + Sdo.
                    ELSE
                        TCorLar.VLar = TCorLar.VLar + Sdo.

                    LEAVE.
                END.
        END.
    END.
END.

FOR EACH TCorLar BREAK BY TCorLar.Age BY TCorLar.Pto:
    DISPLAY TCorLar.Age   LABEL "AGE."
            TCorLar.Pto   LABEL "PDCTO."
            TCorLar.Nom   LABEL "NOMBRE DEL PRODUCTO"
            TCorLar.VCor  LABEL "S-CAPITAL CORTO-PLAZO"
            TCorLar.VLar  LABEL "S-CAPITAL LARGO-PLAZO" SKIP (0)
        WITH DOWN WIDTH 120 FRAME F1 NO-LABEL NO-BOX USE-TEXT STREAM-IO.

    ASSIGN Tpdtos[1] = Tpdtos[1] + TCorLar.VCor
           Tpdtos [2] = Tpdtos [2] + TCorLar.VLar.

    IF LAST-OF(TCorLar.Age) THEN DO:
        DISPLAY "TOTAL AGENCIA                                          -------------------  --------------------" SKIP
                TCorLar.Age
                "       "
                " "          FORM "X(30)"
                "         "
                Tpdtos[1]
                Tpdtos[2]   SKIP (1)
            WITH DOWN WIDTH 120 FRAME F2 NO-LABEL NO-BOX USE-TEXT STREAM-IO.

        ASSIGN Tpdtos[1] = 0
               Tpdtos[2] = 0.
    END.
END.

ASSIGN Tpdtos[1] = 0
       Tpdtos[2] = 0.

FOR EACH TCorLar BREAK BY TCorLar.Pto:
    ASSIGN Tpdtos[1] = Tpdtos[1] + TCorLar.VCor
           Tpdtos[2] = Tpdtos[2] + TCorLar.VLar
           TGen[1] = TGen[1] + TCorLar.VCor
           TGen[2] = TGen[2] + TCorLar.VLar.

    IF LAST-OF(TCorLar.Pto) THEN DO:
        DISPLAY "TOT."
                TCorLar.Pto   LABEL "PDCTO."
                TCorLar.Nom   LABEL "NOMBRE DEL PRODUCTO"
                Tpdtos[1]     LABEL "S-CAPITAL CORTO-PLAZO"
                Tpdtos[2]     LABEL "S-CAPITAL LARGO-PLAZO" SKIP (0)
            WITH DOWN WIDTH 120 FRAME F3 NO-LABEL NO-BOX USE-TEXT STREAM-IO.

        ASSIGN Tpdtos[1] = 0
               Tpdtos[2] = 0.
    END.
END.

DISPLAY "TOTAL GENERAL                                          -------------------  --------------------" SKIP
        "           "
        " "          FORM "X(30)"
        "         "
        TGen[1]
        TGen[2]   SKIP (1)
    WITH DOWN WIDTH 120 FRAME F4 NO-LABEL NO-BOX USE-TEXT STREAM-IO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Ahorros wWin 
PROCEDURE Informes_Ahorros :
Listado = W_PathSpl + "\Ahorros.LST".    

OS-DELETE VALUE(Listado).
ASSIGN Tot1 = 0 TTot1 = 0 Tot2 = 0 TTot2 = 0 Tot3 = 0 TTot3 = 0 Tot4 = 0 TTot4 = 0
       Tot5 = 0 TTot5 = 0 Tot6 = 0 TTot6 = 0 Tot7 = 0 TTot7= 0 Tot8 = 0 TTot8 = 0.

ASSIGN FRAME F_Filtros ValIni ValFin.  
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" 
  INITIAL "AGE NIT           CODPDT CUENTA            TASA     ".
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".

ASSIGN J = 0 k = 0.

OUTPUT TO value(Listado).
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Ahorros: " + Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos
              + " ENTRE " + STRING(FecIni,"99/99/9999") + " y " + STRING(FecFin,"99/99/9999") + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BAhorros:
    WHEN "Básico" THEN  
        W_EncColumna = "AGE  NOMBRE AGENCIA                       NUM.TT.  A LA VISTA   NUM.TT.  CONTRACUAL   NUM.TT   A TERMINO".
    WHEN "Fecha de Cancelacion" THEN
        W_EncColumna = WEnc1 + " FEC.APERTR  MONTO.APERTURA  SDO.DISPONIBLE   FEC.CANCELA".
    WHEN "Fecha de Vencimiento" THEN
        W_EncColumna = WEnc1 + " FEC.APERTR PLAZO FEC.VENCIMIEN MONTO.APERTURA  SDO.DISPONIBLE   ".
    WHEN "Canje" THEN
        W_EncColumna = WEnc1 + " FEC.APERTR  MONTO.APERTURA  SDO.DISPONIBLE   CANJE".
    WHEN "Edades" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA               0 -17     18 - 29     30 - 39    40 - 49     50 - 59      60 - 69    70 o mas     TOTAL".        
    WHEN "Estado Civil" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA            SOLTEROS   CASADOS       SEPARADOS   VIUDOS  UNION LIBRE  TOTAL".  
    WHEN "Nivel Educativo" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA           NINGUNO    PRIMARIA   BACHILLER    TECNOLÓGO     TÉCNICO   PROFESION  POST-GRADO  TOTAL".
    WHEN "Salario" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA              0 - 1       1 - 2      2 - 3      3 - 4      4 - 5      5 - 6    6 - o mas   TOTAL".
  END CASE.
  VIEW FRAME F-Encabezado.
  
IF Cmb_BAhorros:SCREEN-VALUE NE "Sexo" AND 
   Cmb_BAhorros:SCREEN-VALUE NE "Edades" AND 
   Cmb_BAhorros:SCREEN-VALUE NE "Estado Civil" AND
   Cmb_BAhorros:SCREEN-VALUE NE "Nivel Educativo" AND
   Cmb_BAhorros:SCREEN-VALUE NE "Salario" THEN DO:
  FOR EACH Ahorros WHERE 
           Ahorros.Agencia      GE AgeIni AND
           Ahorros.Agencia      LE AgeFin AND
           Ahorros.Cod_Ahorro   GE ProIni AND
           Ahorros.Cod_Ahorro   LE ProFin
           NO-LOCK BREAK BY Ahorros.Agencia BY Ahorros.Nit BY Ahorros.Cod_Ahorro:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Ahorros.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN W_NomAge = Agencias.Nombre.
      END.
      CASE Cmb_BAhorros:
        WHEN "Básico" THEN DO:
           IF Ahorros.Sdo_Disponible GE ValIni AND
              Ahorros.Sdo_Disponible LE ValFin THEN DO:
              CASE Ahorros.Tip_Ahorro:
                WHEN 1 THEN ASSIGN Tot1 = Tot1 + 1 Tot2 = Tot2 + Ahorros.Sdo_Disponible.
                WHEN 2 THEN ASSIGN Tot3 = Tot3 + 1 Tot4 = Tot4 + Ahorros.Sdo_Disponible.
                WHEN 3 THEN ASSIGN Tot5 = Tot5 + 1 Tot6 = Tot6 + Ahorros.Sdo_Disponible.
              END CASE.
              ASSIGN TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Apertura" THEN DO:
           IF Ahorros.Sdo_Disponible GE ValIni AND
              Ahorros.Sdo_Disponible LE ValFin AND 
              Ahorros.Fec_Apertura   GE FecIni AND 
              Ahorros.Fec_Apertura   LE FecFin THEN DO:
              CASE Ahorros.Tip_Ahorro:
                WHEN 1 THEN ASSIGN Tot1 = Tot1 + 1 Tot2 = Tot2 + Ahorros.Sdo_Disponible.
                WHEN 2 THEN ASSIGN Tot3 = Tot3 + 1 Tot4 = Tot4 + Ahorros.Sdo_Disponible.
                WHEN 3 THEN ASSIGN Tot5 = Tot5 + 1 Tot6 = Tot6 + Ahorros.Sdo_Disponible.
              END CASE.
              ASSIGN TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Cancelacion" THEN DO:
           IF Ahorros.Sdo_Disponible  GE ValIni AND
              Ahorros.Sdo_Disponible  LE ValFin AND 
              Ahorros.Fec_Cancelacion GE FecIni AND 
              Ahorros.Fec_Cancelacion LE FecFin THEN DO:
              CASE Ahorros.Tip_Ahorro:
                WHEN 1 THEN ASSIGN Tot1 = Tot1 + 1 Tot2 = Tot2 + Ahorros.Sdo_Disponible.
                WHEN 2 THEN ASSIGN Tot3 = Tot3 + 1 Tot4 = Tot4 + Ahorros.Sdo_Disponible.
                WHEN 3 THEN ASSIGN Tot5 = Tot5 + 1 Tot6 = Tot6 + Ahorros.Sdo_Disponible.
              END CASE.
              ASSIGN TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Vencimiento" THEN DO:
           IF Ahorros.Sdo_Disponible  GE ValIni AND
              Ahorros.Sdo_Disponible  LE ValFin AND 
              Ahorros.Fec_Vencimiento GE FecIni AND 
              Ahorros.Fec_Vencimiento LE FecFin THEN DO:
              CASE Ahorros.Tip_Ahorro:
                WHEN 1 THEN ASSIGN Tot1 = Tot1 + 1 Tot2 = Tot2 + Ahorros.Sdo_Disponible.
                WHEN 2 THEN ASSIGN Tot3 = Tot3 + 1 Tot4 = Tot4 + Ahorros.Sdo_Disponible.
                WHEN 3 THEN ASSIGN Tot5 = Tot5 + 1 Tot6 = Tot6 + Ahorros.Sdo_Disponible.
              END CASE.
              ASSIGN TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Canje" THEN DO:
           IF Ahorros.Sdo_Canje  GE ValIni AND
              Ahorros.Sdo_Canje  LE ValFin THEN DO:
              CASE Ahorros.Tip_Ahorro:
                WHEN 1 THEN ASSIGN Tot1 = Tot1 + 1 Tot2 = Tot2 + Ahorros.Sdo_Disponible.
                WHEN 2 THEN ASSIGN Tot3 = Tot3 + 1 Tot4 = Tot4 + Ahorros.Sdo_Disponible.
                WHEN 3 THEN ASSIGN Tot5 = Tot5 + 1 Tot6 = Tot6 + Ahorros.Sdo_Disponible.
              END CASE.
              ASSIGN TotReg    = TotReg + 1.
           END.
        END.
      END CASE.
      IF LAST-OF(Ahorros.Agencia) THEN DO:
         DISPLAY Ahorros.Agencia  AT 1
                 W_NomAge          AT 5 FORMAT "X(30)"
                 Tot1              AT 40 FORMAT ">>,>>9"
                 Tot2              AT 48 FORMAT ">>>>,>>>,>>9"
                 Tot3              AT 62 FORMAT ">>,>>9"
                 Tot4              AT 70 FORMAT ">>>>,>>>,>>9"
                 Tot5              AT 84 FORMAT ">>,>>9"
                 Tot6              AT 92 FORMAT ">>>>,>>>,>>9"
         WITH FRAME F_T1 WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS.
         ASSIGN TTot1 = TTot1 + Tot1 TTot2 = TTot2 + Tot2
                TTot3 = TTot3 + Tot3 TTot4 = TTot4 + Tot4
                TTot5 = TTot5 + Tot5 TTot6 = TTot6 + Tot6
                Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0.
      END.
  END.
END.  
IF Cmb_BAhorros:SCREEN-VALUE EQ "Sexo"            THEN RUN Aho_Sexo.
IF Cmb_BAhorros:SCREEN-VALUE EQ "Edades"          THEN RUN Aho_Edades.
IF Cmb_BAhorros:SCREEN-VALUE EQ "Estado Civil"    THEN RUN Aho_EstCivil.
IF Cmb_BAhorros:SCREEN-VALUE EQ "Nivel Educativo" THEN RUN Aho_NivEducativo.
IF Cmb_BAhorros:SCREEN-VALUE EQ "Salario"        THEN RUN Aho_Salarios.
  DISPLAY SKIP(1) 
          "Totales: "        AT 1
          TTot1              AT 40 FORMAT ">>,>>>"
          TTot2              AT 48 FORMAT ">>>>,>>>,>>>>"
          TTot3              AT 62 FORMAT ">>,>>>"
          TTot4              AT 70 FORMAT ">>>>,>>>,>>>"
          TTot5              AT 84 FORMAT ">>,>>>"
          TTot6              AT 92 FORMAT ">>>>,>>>,>>>"
  WITH FRAME F_TT1 WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS.

  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Clientes wWin 
PROCEDURE Informes_Clientes :
Listado = W_PathSpl + "\Clientes.LST".

OS-DELETE VALUE(Listado).
DEFINE VAR Wk AS DECIMAL FORMAT ">>,>>9".

IF Cmb_BClientes:SCREEN-VALUE IN FRAME F_Basicos EQ "Salarios" AND Minimo EQ 0 THEN DO:
   MESSAGE "Para poder sacar este informe se tiene que establecer" SKIP
           "el valor del S.M.L.V en la Configuración de la Organización"
           VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.
ASSIGN Tot1 = 0 TTot1 = 0 Tot2 = 0 TTot2 = 0 Tot3 = 0 TTot3 = 0 Tot4 = 0 TTot4 = 0
       Tot5 = 0 TTot5 = 0 Tot6 = 0 TTot6 = 0 Tot7 = 0 TTot7 = 0 Tot8 = 0 TTot8 = 0.
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" INITIAL "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        ".
DEFINE VAR TotReg AS DECIMAL.
DEFINE VAR W_NomAge AS CHARACTER FORMAT "X(30)".
OUTPUT TO value(Listado) PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  IF Cmb_BClientes:SCREEN-VALUE NE "Salario" THEN
    W_Reporte   = "REPORTE   : Clientes: " + Cmb_BClientes:SCREEN-VALUE IN FRAME F_Basicos
                + " ENTRE " + STRING(FecIni,"99/99/9999") + " y " + STRING(FecFin,"99/99/9999") + " - " + STRING(TIME,"hh:mm am").
  ELSE
    W_Reporte   = "REPORTE   : Rangos de Salarios S.M.L.V = " + STRING(Minimo,">,>>>,>>9")
                  + " - ENTRE " + STRING(FecIni,"99/99/9999") + " y " + STRING(FecFin,"99/99/9999") + " - " + STRING(TIME,"hh:mm am").
  
  CASE Cmb_BClientes:
    WHEN "Clientes por Agencia" THEN  
        W_EncColumna = "AGE NOMBRE AGENCIA                    ASOCIADOS   CLI.NO.ASOC  TERCEROS  PROVEEDORES  TOTAL".
    WHEN "Afiliaciones" THEN
        ASSIGN W_EncColumna = "AGE NOMBRE AGENCIA                    ASOCIADOS   CLI.NO.ASOC    TOTAL".
    WHEN "Retiros" THEN
        ASSIGN W_EncColumna = "AGE NOMBRE AGENCIA                    ASOCIADOS   CLI.NO.ASOC    TOTAL".
    WHEN "Habiles" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA                    ASOCIADOS   CLI.NO.ASOC    TOTAL".
    WHEN "No Habiles" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA                    ASOCIADOS   CLI.NO.ASOC    TOTAL".
    WHEN "Sexo" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA                      HOMBRES     MUEJERES     TOTAL".
    WHEN "Nivel Educativo" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA                      NINGUNO    PRIMARIA   BACHILLER TECNOLÓGO TÉCNICO   PROFESION  POST-GRADO  TOTAL".
    WHEN "Edades" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA                        0 -17     18 - 29     30 - 39   40 - 49  50 - 59    60 - 69   70 o mas   TOTAL".
    WHEN "Estado Civil" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA                      SOLTEROS   CASADOS    SEPARADOS   VIUDOS  UNION LIBRE  TOTAL".
    WHEN "Salario" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA                        0 - 1       1 - 2      2 - 3     3 - 4    4 - 5      5 - 6    6 - o mas   TOTAL".
    WHEN "Tipos de Cliente" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA                      MAY.EDAD   MENORES    JURID.S.A  JURID.C.A  TOTAL".
  END CASE.
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
  FOR EACH Clientes WHERE 
           Clientes.Agencia GE AgeIni AND
           Clientes.Agencia LE AgeFin NO-LOCK BREAK BY Clientes.Agencia BY Clientes.Nit:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Clientes.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Clientes.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN W_NomAge = Agencias.Nombre.
      END.
         
      CASE Cmb_BClientes:
        WHEN "Clientes por Agencia" THEN DO:
           CASE Clientes.Tipo_Vinculo:
              WHEN 1 THEN Tot1 = Tot1 + 1.
              WHEN 2 THEN Tot2 = Tot2 + 1.
              WHEN 3 THEN Tot3 = Tot3 + 1.
              WHEN 4 THEN Tot4 = Tot4 + 1.
           END CASE.
           TotReg = TotReg + 1.
        END.
        WHEN "Afiliaciones" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_Ingreso GE FecIni AND
              Clientes.Fec_Ingreso LE FecFin THEN DO:
              IF Clientes.Tipo_Vinculo EQ 1 THEN Tot1 = Tot1 + 1.
              IF Clientes.Tipo_Vinculo EQ 2 THEN Tot2 = Tot2 + 1.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Retiros" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              Clientes.Fec_Retiro GE FecIni AND
              Clientes.Fec_Retiro LE FecFin THEN DO:
              IF Clientes.Tipo_Vinculo EQ 1 THEN Tot1 = Tot1 + 1.
              IF Clientes.Tipo_Vinculo EQ 2 THEN Tot2 = Tot2 + 1.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Habiles" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              YEAR(Clientes.Fec_UltActualiza) EQ YEAR(TODAY) AND
              Clientes.Estado EQ 1 THEN DO:
              IF Clientes.Tipo_Vinculo EQ 1 THEN Tot1 = Tot1 + 1.
              IF Clientes.Tipo_Vinculo EQ 2 THEN Tot2 = Tot2 + 1.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "No Habiles" THEN DO:
           IF Clientes.Tipo_Vinculo LT 3 AND 
              YEAR(Clientes.Fec_UltActualiza) NE YEAR(TODAY) AND
              Clientes.Estado EQ 1 THEN DO:
              IF Clientes.Tipo_Vinculo EQ 1 THEN Tot1 = Tot1 + 1.
              IF Clientes.Tipo_Vinculo EQ 2 THEN Tot2 = Tot2 + 1.
              TotReg = TotReg + 1.
           END.
        END.
        WHEN "Sexo" THEN DO:
          IF Clientes.Tipo_Cliente LT 3 AND 
             Clientes.Fec_Ingreso GE FecIni AND
             Clientes.Fec_Ingreso LE FecFin THEN DO:
           IF Clientes.Sexo EQ 1 THEN Tot1 = Tot1 + 1.
           ELSE Tot2 = Tot2 + 1.
           TotReg = TotReg + 1.
          END.
        END.
        WHEN "Nivel Educativo" THEN DO:
          IF Clientes.Tipo_Cliente LT 3 AND 
             Clientes.Fec_Ingreso GE FecIni AND
             Clientes.Fec_Ingreso LE FecFin THEN DO:
             CASE Clientes.Niv_Educativo:
               WHEN "Ninguno"     THEN Tot1 = Tot1 + 1.
               WHEN "Primaria"    THEN Tot2 = Tot2 + 1.
               WHEN "Bachiller"   THEN Tot3 = Tot3 + 1.
               WHEN "Tecnológo"   THEN Tot4 = Tot4 + 1.
               WHEN "Técnico"     THEN Tot5 = Tot5 + 1.
               WHEN "Profesional" THEN Tot6 = Tot6 + 1.
               WHEN "Post-Grado"  THEN Tot7 = Tot7 + 1.
             END CASE.
             TotReg = TotReg + 1.
          END.
        END.
        WHEN "Estado Civil" THEN DO:
          IF Clientes.Tipo_Cliente LT 3 AND 
             Clientes.Fec_Ingreso GE FecIni AND
             Clientes.Fec_Ingreso LE FecFin THEN DO:
             CASE Clientes.Est_Civil:
               WHEN "Soltero"     THEN Tot1 = Tot1 + 1.
               WHEN "Casado"      THEN Tot2 = Tot2 + 1.
               WHEN "Separado"    THEN Tot3 = Tot3 + 1.
               WHEN "Viudo"       THEN Tot4 = Tot4 + 1.
               WHEN "Unión Libre" THEN Tot5 = Tot5 + 1.
             END CASE.
             TotReg = TotReg + 1.
          END.
        END.
        WHEN "Tipos de Cliente" THEN DO:
             CASE Clientes.Tipo_Cliente:
               WHEN 1    THEN Tot1 = Tot1 + 1.
               WHEN 2    THEN Tot2 = Tot2 + 1.
               WHEN 3    THEN Tot3 = Tot3 + 1.
               WHEN 4    THEN Tot4 = Tot4 + 1.
             END CASE.
             TotReg = TotReg + 1.
        END.
        WHEN "Edades" THEN DO:
         IF Clientes.Tipo_Cliente LT 3 AND 
             Clientes.Fec_Ingreso GE FecIni AND
             Clientes.Fec_Ingreso LE FecFin THEN DO:
          IF Clientes.Fec_Nacimiento NE ? THEN Wk = (W_Fecha - Clientes.Fec_Nacimiento) / 360.
          ELSE Wk = Clientes.Edad.
          IF Wk GE 0  AND Wk LT 18 THEN Tot1 = Tot1 + 1.
          IF Wk GE 18 AND Wk LT 30 THEN Tot2 = Tot2 + 1.
          IF Wk GE 30 AND Wk LT 40 THEN Tot3 = Tot3 + 1.
          IF Wk GE 40 AND Wk LT 50 THEN Tot4 = Tot4 + 1.
          IF Wk GE 50 AND Wk LT 60 THEN Tot5 = Tot5 + 1.
          IF Wk GE 60 AND Wk LT 70 THEN Tot6 = Tot6 + 1.
          IF Wk GE 70 THEN Tot7 = Tot7 + 1.
          TotReg = TotReg + 1.
         END.
        END.
        WHEN "Salario" THEN DO:
         IF Clientes.Tipo_Cliente LT 3 AND 
             Clientes.Fec_Ingreso GE FecIni AND
             Clientes.Fec_Ingreso LE FecFin THEN DO:
          IF Clientes.Salario GE 0  AND Clientes.Salario LE Minimo THEN 
             Tot1 = Tot1 + 1.
          IF Clientes.Salario GE (Minimo + 1)     AND Clientes.Salario LE (Minimo * 2) THEN 
             Tot2 = Tot2 + 1.
          IF Clientes.Salario GE (Minimo * 2) + 1 AND Clientes.Salario LE (Minimo * 3) THEN 
             Tot3 = Tot3 + 1.
          IF Clientes.Salario GE (Minimo * 3) + 1 AND Clientes.Salario LT (Minimo * 4) THEN 
             Tot4 = Tot4 + 1.
          IF Clientes.Salario GE (Minimo * 4) + 1 AND Clientes.Salario LT (Minimo * 5) THEN
             Tot5 = Tot5 + 1.
          IF Clientes.Salario GE (Minimo * 5) + 1 AND Clientes.Salario LT (Minimo * 6) THEN
             Tot6 = Tot6 + 1.
          IF Clientes.Salario GE (Minimo * 6) + 1 THEN
             Tot7 = Tot7 + 1.
          TotReg = TotReg + 1.
         END.
        END.
      END CASE.
      IF LAST-OF(Clientes.Agencia) THEN DO:
         CASE Cmb_BClientes:
            WHEN "Clientes por Agencia" OR WHEN "Tipos de Cliente" THEN DO:
               Tot5 = Tot1 + Tot2 + Tot3 + Tot4.
               DISPLAY Clientes.Agencia  AT 1
                       W_NomAge          AT 5 FORMAT "X(30)"
                       Tot1              AT 40 FORMAT ">,>>>,>>9"
                       Tot2              AT 52 FORMAT ">,>>>,>>9"
                       Tot3              AT 62 FORMAT ">,>>>,>>9"
                       Tot4              AT 72 FORMAT ">,>>>,>>9"
                       Tot5              AT 82 FORMAT ">,>>>,>>9"
               WITH FRAME F_T1 WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS.
            END.
            WHEN "Estado Civil" THEN DO:
               Tot6 = Tot1 + Tot2 + Tot3 + Tot4 + Tot5.
               DISPLAY Clientes.Agencia  AT 1
                       W_NomAge          AT 5 FORMAT "X(30)"
                       Tot1              AT 40 FORMAT ">,>>>,>>9"
                       Tot2              AT 52 FORMAT ">,>>>,>>9"
                       Tot3              AT 62 FORMAT ">,>>>,>>9"
                       Tot4              AT 72 FORMAT ">,>>>,>>9"
                       Tot5              AT 82 FORMAT ">,>>>,>>9"
                       Tot6              AT 92 FORMAT ">,>>>,>>9"
               WITH FRAME F_T2 WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS.
            END.
            WHEN "Afiliaciones" OR WHEN "Retiros" OR 
                 WHEN "Habiles" OR WHEN "No Habiles" OR WHEN "Sexo" THEN DO:
               Tot3 = Tot1 + Tot2.
               DISPLAY Clientes.Agencia  AT 1
                       W_NomAge          AT 5 FORMAT "X(30)"
                       Tot1              AT 40 FORMAT ">,>>>,>>9"
                       Tot2              AT 52 FORMAT ">,>>>,>>9"
                       Tot3              AT 62 FORMAT ">,>>>,>>9"
               WITH FRAME F_T3 WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS.
            END.
            WHEN "Nivel Educativo" OR WHEN "Edades" OR WHEN "Salario" THEN DO:
               Tot8 = Tot1 + Tot2 + Tot3 + Tot4 + Tot5 + Tot6 + Tot7.
               DISPLAY Clientes.Agencia  AT 1
                       W_NomAge          AT 5 FORMAT "X(30)"
                       Tot1              AT 40 FORMAT ">,>>>,>>9"
                       Tot2              AT 52 FORMAT ">,>>>,>>9"
                       Tot3              AT 62 FORMAT ">,>>>,>>9"
                       Tot4              AT 72 FORMAT ">,>>>,>>9"
                       Tot5              AT 82 FORMAT ">,>>>,>>9"
                       Tot6              AT 92 FORMAT ">,>>>,>>9"
                       Tot7              AT 102 FORMAT ">,>>>,>>9"
                       Tot8              AT 112 FORMAT ">,>>>,>>9"
               WITH FRAME F_T4 WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS.
            END.
         END CASE.
         ASSIGN TTot1 = TTot1 + Tot1
                TTot2 = TTot2 + Tot2
                TTot3 = TTot3 + Tot3
                TTot4 = TTot4 + Tot4
                TTot5 = TTot5 + Tot5
                TTot6 = TTot6 + Tot6
                TTot7 = TTot7 + Tot7
                TTot8 = TTot8 + Tot8
                Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0 Tot5 = 0 Tot6 = 0 Tot7 = 0 Tot8.
      END.
  END.
  DISPLAY SKIP(1) 
          "Totales: "        AT 1
          TTot1              AT 40 FORMAT ">,>>>,>>>"
          TTot2              AT 52 FORMAT ">,>>>,>>>"
          TTot3              AT 62 FORMAT ">,>>>,>>>"
          TTot4              AT 72 FORMAT ">,>>>,>>>"
          TTot5              AT 82 FORMAT ">,>>>,>>>"
          TTot6              AT 92 FORMAT ">,>>>,>>>"
          TTot7              AT 102 FORMAT ">,>>>,>>>"
          TTot8              AT 112 FORMAT ">,>>>,>>>"
  WITH FRAME F_TT1 WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS.

  DISPLAY SKIP "Total de Registros Reportados: " TotReg FORMAT ">>>,>>9" WITH FRAME FT WIDTH 132 NO-LABELS.
  VIEW FRAME F-Ftr.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_CortoLargo wWin 
PROCEDURE Informes_CortoLargo :
DEFI VAR DiaPdo AS DEC FORM "9999.9".
DEFI VAR CuoFal AS INTEG FORM "9999".
DEFI VAR NN AS INTEG FORM "9999".
DEFI VAR Sdo LIKE Creditos.Sdo_Capital.
DEFI VAR Cap LIKE Creditos.Sdo_Capital.
DEFI VAR INT LIKE Creditos.Sdo_Capital.
DEFI VAR KCuo LIKE Creditos.Cuota.
DEFI VAR W_Tasa LIKE Creditos.Tasa.
DEFI VAR Tpdtos LIKE Creditos.Sdo_Capital EXTENT 2.
DEFI VAR TGen LIKE Creditos.Sdo_Capital EXTENT 2.

IF FecFin < w_fecha AND DAY(fecFin + 1) = 1 THEN
    RUN InformesCortoLargoCorte.
ELSE DO:
    EMPTY TEMP-TABLE TCorLar.

    FOR EACH Creditos WHERE Creditos.Estado EQ 2 /*AND creditos.cod_credito = 123 AND agencia = 1*/
                        AND Creditos.Sdo_Capital GT 0 NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Cod_Credito:
        IF FIRST-OF(Cod_Credito) THEN DO:
            FIND FIRST Pro_Credito WHERE Pro_Credito.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
            FIND FIRST TCorLar WHERE TCorLar.Age EQ Creditos.Agencia
                             AND TCorLar.Pto EQ Creditos.Cod_Credito NO-ERROR.
            IF NOT AVAIL(TCorLar) THEN
                CREATE TCorLar.
        END.

        ASSIGN DiaPdo = Creditos.Plazo
               CuoFal = Creditos.Plazo - Creditos.Cuo_Pagadas
               W_Tasa = Creditos.Tasa / 1200
               KCuo = Creditos.Cuota.

        IF kCuo = ? THEN
            KCuo = 0.

        IF Per_Pago EQ 1 THEN
            ASSIGN DiaPdo = Creditos.Plazo / 4.33
                   CuoFal = ROUND((Creditos.Plazo - Creditos.Cuo_Pagadas) / 4.33,0)
                   KCuo = Creditos.Cuota * 4.33.
        ELSE
            IF Per_Pago EQ 2 THEN
                ASSIGN DiaPdo = Creditos.Plazo / 3
                       CuoFal = ROUND((Creditos.Plazo - Creditos.Cuo_Pagadas) / 3,0)
                       KCuo = Creditos.Cuota * 3.
            ELSE
                IF Per_Pago EQ 3 THEN
                    ASSIGN DiaPdo = Creditos.Plazo / 2
                           CuoFal = ROUND((Creditos.Plazo - Creditos.Cuo_Pagadas) / 2,0)
                           KCuo = Creditos.Cuota * 2.

        IF DiaPdo LE 12 OR CuoFal LE 12 OR Creditos.Sistema NE 1 THEN
            ASSIGN TCorLar.Age = Creditos.Agencia
                   TCorLar.Pto = Creditos.Cod_Credito
                   TCorLar.Nom = Pro_Creditos.Nom_Prod
                   TCorLar.VCor = TCorLar.VCor + Creditos.Sdo_Capital.
        ELSE DO:
            Sdo = Creditos.Sdo_Capital.

            DO NN = 1 TO CuoFal:
                ASSIGN Int = ROUND(Sdo * W_Tasa,0)
                       Cap = ROUND(KCuo - Int,0).

                IF Cap LE 0 THEN
                    Cap = 0.
                ELSE
                    IF Cap GT Sdo THEN
                        Cap = Sdo.

                TCorLar.Age = Creditos.Agencia.
                TCorLar.Pto = Creditos.Cod_Credito.
                TCorLar.Nom = Pro_Creditos.Nom_Prod.
                
                IF NN LE 12 THEN
                    TCorLar.VCor = TCorLar.VCor + Cap.
                ELSE
                    TCorLar.VLar = TCorLar.VLar + Cap.

                Sdo = Sdo - Cap.

                IF Sdo LE 0 THEN
                    LEAVE.
                ELSE
                    IF NN = CuoFal THEN DO:
                        TCorLar.Age = Creditos.Agencia.
                        TCorLar.Pto = Creditos.Cod_Credito.
                        TCorLar.Nom = Pro_Creditos.Nom_Prod.

                        IF NN LE 12 THEN
                            TCorLar.VCor = TCorLar.VCor + Sdo.
                        ELSE
                            TCorLar.VLar = TCorLar.VLar + Sdo.

                        LEAVE.
                    END.
            END.
        END.
    END.

    FOR EACH TCorLar BREAK BY TCorLar.Age BY TCorLar.Pto:
        DISPLAY TCorLar.Age   LABEL "AGE."
                TCorLar.Pto   LABEL "PDCTO."
                TCorLar.Nom   LABEL "NOMBRE DEL PRODUCTO"
                TCorLar.VCor  LABEL "S-CAPITAL CORTO-PLAZO"
                TCorLar.VLar  LABEL "S-CAPITAL LARGO-PLAZO" SKIP (0)
            WITH DOWN WIDTH 120 FRAME F1 NO-LABEL NO-BOX USE-TEXT STREAM-IO.

        ASSIGN Tpdtos[1] = Tpdtos[1] + TCorLar.VCor
               Tpdtos [2] = Tpdtos [2] + TCorLar.VLar.

        IF LAST-OF(TCorLar.Age) THEN DO:
            DISPLAY "TOTAL AGENCIA                                          -------------------  --------------------" SKIP
                    TCorLar.Age
                    "       "
                    " "          FORM "X(30)"
                    "         "
                    Tpdtos[1]
                    Tpdtos[2]   SKIP (1)
                WITH DOWN WIDTH 120 FRAME F2 NO-LABEL NO-BOX USE-TEXT STREAM-IO.

            ASSIGN Tpdtos[1] = 0
                   Tpdtos[2] = 0.
        END.
    END.

    ASSIGN Tpdtos[1] = 0
           Tpdtos[2] = 0.

    FOR EACH TCorLar BREAK BY TCorLar.Pto:
        ASSIGN Tpdtos[1] = Tpdtos[1] + TCorLar.VCor
               Tpdtos[2] = Tpdtos[2] + TCorLar.VLar
               TGen[1] = TGen[1] + TCorLar.VCor
               TGen[2] = TGen[2] + TCorLar.VLar.

        IF LAST-OF(TCorLar.Pto) THEN DO:
            DISPLAY "TOT."
                    TCorLar.Pto   LABEL "PDCTO."
                    TCorLar.Nom   LABEL "NOMBRE DEL PRODUCTO"
                    Tpdtos[1]     LABEL "S-CAPITAL CORTO-PLAZO"
                    Tpdtos[2]     LABEL "S-CAPITAL LARGO-PLAZO" SKIP (0)
                WITH DOWN WIDTH 120 FRAME F3 NO-LABEL NO-BOX USE-TEXT STREAM-IO.

            ASSIGN Tpdtos[1] = 0
                   Tpdtos[2] = 0.
        END.
    END.

    DISPLAY "TOTAL GENERAL                                          -------------------  --------------------" SKIP
            "           "
            " "          FORM "X(30)"
            "         "
            TGen[1]
            TGen[2]   SKIP (1)
        WITH DOWN WIDTH 120 FRAME F4 NO-LABEL NO-BOX USE-TEXT STREAM-IO.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos wWin 
PROCEDURE Informes_Creditos :
Listado = W_PathSpl + "\creditos.lst".

OS-DELETE VALUE(Listado).
DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(30)".
ASSIGN J = 0 k = 0
       Tot1 = 0 TTot1 = 0
       Tot2 = 0 TTot2 = 0
       Tot3 = 0 TTot3 = 0
       Tot4 = 0 TTot4 = 0
       Tot5 = 0 TTot5 = 0
       Tot6 = 0 TTot6 = 0.
OUTPUT TO value(Listado).
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : " + Cmb_BCreditos:SCREEN-VALUE IN FRAME F_Basicos   
         + " ENTRE " + STRING(FecIni,"99/99/9999") + " y " + STRING(FecFin,"99/99/9999") + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BCreditos:SCREEN-VALUE IN FRAME F_Basicos:
    WHEN "Por Estados" THEN
      ASSIGN   W_Reporte   = "REPORTE   : Creditos Por Estado: " + Cmb_EstCre:SCREEN-VALUE IN FRAME F_Filtros
                           + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
               W_EncColumna = "AGE NOMBRE                   APROB.NO.DESEMBOLSA NOMRAL.DESEMBOLSADO CREDITOS.CANCELADOS RETIRADOS.NO.APROBA CREDIT.CASTIGADOS".
    WHEN "Por Instancias" THEN
      ASSIGN   W_Reporte   = "REPORTE   : Creditos Instancias: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
               W_EncColumna = "CODINS  NOMBRE INSTANCIA               NUM.CRE       SDO.CAPITAL         SDO.INTERES".
    WHEN "Cartera" THEN
      ASSIGN   W_Reporte   = "REPORTE   : Cartera de Créditos: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
               W_EncColumna = "".
    WHEN "Indice de Morosidad" THEN
      ASSIGN   W_Reporte   = "REPORTE   : INDICE DE MOROSIDAD: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
               W_EncColumna = "".
    WHEN "Por Dias-Mora" THEN
        ASSIGN   W_Reporte   = "REPORTE   : DIAS  DE  MOROSIDAD: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                 W_EncColumna = "".
    WHEN "Por Corto/Largo Plazo" THEN
        ASSIGN   W_Reporte   = "REPORTE   : Por Corto/Largo Plazo: " + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am")
                 W_EncColumna = "".
        
    WHEN "Edades" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA             0 - 17      18 - 29      30 - 39       40 - 49      50 - 59      60 - 69     70 o mas    TOTAL".        
    WHEN "Estado Civil" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA            SOLTEROS      CASADOS    SEPARADOS     VIUDOS     UNION LIBRE    TOTAL".  
    WHEN "Nivel Educativo" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA           NINGUNO       PRIMARIA   BACHILLER    TECNOLÓGO     TÉCNICO      PROFESION    POST-GRADO   TOTAL".
    WHEN "Salario" THEN
        W_EncColumna = "AGE NOMBRE AGENCIA              0 - 1       1 - 2        2 - 3       3 - 4        4 - 5         5 - 6      6 - o mas    TOTAL".
    WHEN "Estadisticos Acumulados" THEN
        W_EncColumna = "CON NOMBRE CONCEPTO                                     CANTIDAD          ACUMULADO AÑO               PROMEDIO".
  END CASE.
  
  VIEW FRAME F-Encabezado.

/*IF Cmb_BCreditos:SCREEN-VALUE EQ "Cartera" THEN DO:
  DISPLAY "                                                                                                                                   PAG C" AT 1 
     WITH FRAME F_TitCartera WIDTH 230 USE-TEXT NO-BOX.
END.*/
IF Cmb_BCreditos:SCREEN-VALUE EQ "Por Dias-Mora" THEN
   RUN Informes_Creditos_DiaMora.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Por Corto/Largo Plazo" THEN
   RUN Informes_CortoLargo.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Por Estados" THEN
   RUN Informes_Creditos_Estados.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Indice de Morosidad" THEN
   RUN Informes_Creditos_IndiceMor.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Por Instancias" THEN 
   RUN Informes_Creditos_Instancias.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Cartera" THEN 
   RUN Informes_Creditos_Cartera.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Sexo" THEN
   RUN Cre_Sexo.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Edades" THEN
   RUN Cre_Edades.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Estado Civil" THEN
   RUN Cre_EstCivil.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Nivel Educativo" THEN
   RUN Cre_NivEducativo.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Salario" THEN
   RUN Cre_Salarios.
ELSE IF Cmb_BCreditos:SCREEN-VALUE EQ "Estadisticos Acumulados" THEN
   RUN Cre_Estadisticos.
        
  VIEW FRAME F-Ftr.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Cartera wWin 
PROCEDURE Informes_Creditos_Cartera :
DEFINE VAR CTotMon AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotSdo AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotInt AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotMon  AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".  
DEFINE VAR TotInt  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". 
DEFINE VAR PorMor  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". 
DEFINE VAR TotCarNM AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR TaCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR TeCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR W_Garantias AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".  
DEFINE VAR TApo LIKE Ahorros.Sdo_Disponible.

/*polizas*/
DEFINE VAR TCrePol  AS DECIMAL FORMAT ">,>>>,>>>,>>9".  
DEFINE VAR TaCrePol AS DECIMAL FORMAT ">,>>>,>>>,>>9".  
DEFINE VAR TeCrePol AS DECIMAL FORMAT ">,>>>,>>>,>>9".

/*costas*/
DEFINE VAR TCreCos  AS DECIMAL FORMAT ">,>>>,>>>,>>9".  
DEFINE VAR TaCreCos AS DECIMAL FORMAT ">,>>>,>>>,>>9".  
DEFINE VAR TeCreCos AS DECIMAL FORMAT ">,>>>,>>>,>>9".

/*Aportes*/
DEFINE VAR TCreApo  AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".  
DEFINE VAR TaCreApo AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".  
DEFINE VAR TeCreApo AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".

/*Neto*/
DEFINE VAR TCreNet  AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".  
DEFINE VAR TaCreNet AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".  
DEFINE VAR TeCreNet AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".

/*Provision*/
DEFINE VAR TCrePro  AS DECIMAL FORMAT ">,>>>,>>>,>>9".  
DEFINE VAR TaCrePro AS DECIMAL FORMAT ">,>>>,>>>,>>9".  
DEFINE VAR TeCrePro AS DECIMAL FORMAT ">,>>>,>>>,>>9".

/*Garantias*/
DEFINE VAR TCreGar  AS DECIMAL FORMAT ">,>>>,>>>,>>9".  
DEFINE VAR TaCreGar AS DECIMAL FORMAT ">,>>>,>>>,>>9".  
DEFINE VAR TeCreGar AS DECIMAL FORMAT ">,>>>,>>>,>>9".

EMPTY TEMP-TABLE Yaportes.

ASSIGN tot1 = 0
       tot2 = 0
       tot3 = 0
       tot4 = 0
       tot5 = 0
       tot6 = 0
       tot7 = 0
       tot8 = 0
       tot9 = 0
       tot10 = 0
       TotCarNM = 0
       TCreCla = 0
       TaCreCla = 0
       TeCreCla = 0
       TotSdo = 0.

DISPLAY "Filtros del Informe" SKIP
        "Agencia      : " STRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Filtros,"X(40)") FORMAT "X(30)" SKIP
        "Tipo Producto: " STRING(Cmb_TProducto:SCREEN-VALUE IN FRAME F_Filtros,"X(40)") FORMAT "X(30)" SKIP
        "Prod  Credito: " STRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Filtros,"X(40)") FORMAT "X(30)"
    WITH FRAME Fenc3 WIDTH 250 NO-LABELS USE-TEXT NO-BOX STREAM-IO.

DISPLAY "               CODIGO     NOMBRE                                MONTO      SDO.CAPITAL      INTERESES    NUM.CREDITOS       POLIZAS  COSTAS JURID     PROVISION  GAR.ADMISIBLE              APORTES" WITH FRAME Fti WIDTH 250.

FOR EACH Creditos WHERE Creditos.Agencia GE AgeIni
                    AND Creditos.Agencia LE AgeFin
                    AND Creditos.Tip_Credito GE TPdIni
                    AND Creditos.Tip_Credito LE TpdFin
                    AND Creditos.Cod_Credito GE ProIni
                    AND Creditos.Cod_Credito LE ProFin
                    AND Creditos.Dias_Atraso GE ValIni
                    AND Creditos.Dias_Atraso LE ValFin
                    AND creditos.tip_credito <= 4 NO-LOCK BREAK BY Creditos.Agencia
                                                                BY Creditos.Cod_Califica
                                                                BY Creditos.Cod_Credito:
    IF FIRST-OF(Creditos.Agencia) THEN DO:
        FIND FIRST Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
            DISPLAY CAPS(Agencias.Nombre) AT 1  FORMAT "X(30)" WITH FRAME TitCartAge WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
        ELSE
            DISPLAY "Tipo de Cartera no definida" AT 1 WITH FRAME TitCartAge2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
    END.

    IF FIRST-OF(Creditos.Cod_Califica) THEN
        ASSIGN CTotMon = 0
               CTotSdo = 0
               CTotInt = 0.

    j = j + 1.

    RUN Progreso.

    W_Garantias = 0.

    FOR EACH Garantias WHERE Garantias.Cod_Credito EQ Creditos.Cod_Credito
                         AND Garantias.Tip_Credito EQ Creditos.Tip_Credito
                         AND Garantias.Num_Credito EQ Creditos.Num_Credito
                         AND Garantias.Estado EQ 1 NO-LOCK:
        W_Garantias = W_Garantias + Garantias.Val_Bien.
    END.

    W_Interes = Creditos.INT_Corriente + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.

    FIND FIRST Yaportes WHERE Yaportes.Nit EQ Creditos.Nit NO-ERROR.
    IF NOT AVAILABLE Yaportes THEN DO:
        FOR EACH Ahorros WHERE Ahorros.Tip_Ahorro EQ 1
                           AND Ahorros.Nit EQ Creditos.Nit NO-LOCK:
            TApo = TApo + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
        END.

        ASSIGN TCreApo = TCreApo + TApo
               TApo = 0.

        CREATE Yaportes.
        ASSIGN Yaportes.Nit = Creditos.Nit.
    END.

    ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto
           TotRegAge = TotReg + 1
           TotMon = TotMon + Creditos.Monto
           TotSdo = TotSdo + Creditos.Sdo_Capital
           TotInt = TotInt + W_Interes
           TotReg = TotReg + 1
           CTotMon = CTotMon + Creditos.Monto
           CTotSdo = CTotSdo + Creditos.Sdo_Capital
           CTotInt = CTotInt + W_Interes
           Tot1 = Tot1 + Creditos.Monto
           Tot2 = Tot2 + Creditos.Sdo_Capital
           Tot3 = Tot3 + W_Interes
           Tot4 = Tot4 + INT_MorCobrar
           TCreCla = TCreCla + 1
           TCrePol = TCrePol + Creditos.Polizas
           TCreCos = TCreCos + Creditos.Costas  
           TCrePro = TCrePro + Creditos.Provision
           TCreGar = TCreGar + W_Garantias.

    IF LAST-OF(Creditos.Cod_Califica) THEN DO:
        FIND FIRST Varios WHERE Varios.Tipo EQ 10
                            AND Varios.Codigo EQ Creditos.Cod_Califica NO-LOCK NO-ERROR.
        IF AVAILABLE Varios THEN
            DISPLAY Varios.Codigo AT 15
                    CAPS(Varios.Descripcion) AT 23 FORMAT "X(30)"
                    CTotMon AT 61 FORMAT "->,>>>,>>>,>>9"
                    CTotSdo AT 76 FORMAT "->,>>>,>>>,>>9"
                    CTotInt AT 92 FORMAT "->,>>>,>>>,>>9"
                    TCreCla AT 108 FORMAT ">>,>>>,>>9"
                    TCrePol AT 119 FORMAT ">,>>>,>>>,>>9"
                    TCreCos AT 133 FORMAT ">,>>>,>>>,>>9"
                    TCrePro AT 147 FORMAT ">,>>>,>>>,>>9"
                    TCreGar AT 162 FORMAT ">,>>>,>>>,>>9"
                    TCreApo AT 176 FORMAT ">>>,>>>,>>>,>>>,>>9"
                WITH FRAME F_TotCar WIDTH 250 USE-TEXT NO-BOX NO-LABELS STREAM-IO.

        ASSIGN TaCreCla = TaCreCla + TCreCla
               TaCrePol = TaCrePol + TCrePol
               TaCreCos = TaCreCos + TCreCos
               TaCrePro = TaCrePro + TCrePro
               TaCreGar = TaCreGar + TCreGar
               TaCreApo = TaCreApo + TCreApo.

        IF Creditos.cod_califica LE 2 THEN
            TotCarNM = TotCarNM + CTotSdo.

        ASSIGN CTotMon = 0
               CTotSdo = 0
               CTotInt = 0
               TCreCla = 0
               TCrePol = 0
               TCreCos = 0
               TCrePro = 0
               TCreNet = 0
               TCreGar = 0
               TCreApo = 0.
    END.

    IF LAST-OF(Creditos.Agencia) THEN DO:
        FIND FIRST Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
            DISPLAY SKIP(1) "TOTALES:" AT 1 CAPS(Agencias.Nombre) AT 15  FORMAT "X(30)"
                    Tot1 AT 61  FORMAT "->,>>>,>>>,>>9"
                    Tot2 AT 76  FORMAT "->,>>>,>>>,>>9"
                    Tot3 AT 93  FORMAT "->,>>>,>>>,>>9"
                    TaCreCla AT 108 FORMAT ">>,>>>,>>9"
                    TaCrePol AT 119 FORMAT ">,>>>,>>>,>>9"
                    TaCreCos AT 133 FORMAT ">,>>>,>>>,>>9"
                    TaCrePro AT 147 FORMAT ">,>>>,>>>,>>9"
                    TaCreGar AT 162 FORMAT ">,>>>,>>>,>>9"
                    TaCreApo  AT 176 FORMAT ">>>,>>>,>>>,>>>,>>9"
                    SKIP(1)
                WITH FRAME F_TotCar2 WIDTH 250 USE-TEXT NO-BOX NO-LABELS STREAM-IO.

        ASSIGN TeCreCla = TeCreCla + TaCreCla
               TeCrePol = TeCrePol + TaCrePol
               TeCreCos = TeCreCos + TaCreCos
               TeCrePro = TeCrePro + TaCrePro
               TeCreGar = TeCreGar + TaCreGar
               TeCreApo = TeCreApo + TaCreApo.

        ASSIGN Tot1 = 0
               Tot2 = 0
               Tot3 = 0 
               TaCreCla = 0
               TaCrePol = 0
               TaCreCos = 0
               TaCrePro = 0
               TaCreNet = 0
               TaCreGar = 0
               TaCreApo = 0.
    END.
END.

DISPLAY SKIP(1)
        "Total de Registros  :" AT 1
        TotReg AT 25
        "Total x Entidad  : " AT 42
        TotMon AT 60
        TotSdo AT 74
        TotInt AT 93  FORMAT "->,>>>,>>>,>>9"
        TeCreCla AT 108 FORMAT ">>,>>>,>>9"
        TeCrePol AT 119 FORMAT ">,>>>,>>>,>>9"
        TeCreCos AT 133 FORMAT ">,>>>,>>>,>>9"
        TeCrePro AT 147 FORMAT ">,>>>,>>>,>>9"
        TeCreGar AT 162 FORMAT ">,>>>,>>>,>>9"
        TeCreApo AT 176 FORMAT ">>>,>>>,>>>,>>>,>>9"
    WITH FRAME ttCartera WIDTH 250 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.

DISPLAY "Porcentaje de Morosidad: "    ROUND((((TotSdo - TotCarNM) / TotSdo) * 100),2) format ">>9.99%".

IF AgeIni NE AgeFin THEN
    RUN TOTAL_CarteraAg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_DiaMora wWin 
PROCEDURE Informes_Creditos_DiaMora :
/*------------------------------------------------------------------------------
  Purpose:   Resumen de Cartera por Dias-Mora
  Notes:     Agosto 17/06 - GAER  
------------------------------------------------------------------------------*/
DEFINE VAR CTotMon AS DECIMAL FORMAT ">,>>>,>>>,>>>,>>9".
DEFINE VAR CTotSdo AS DECIMAL FORMAT ">,>>>,>>>,>>>,>>9".
DEFINE VAR CTotInt AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotMon  AS DECIMAL FORMAT ">,>>>,>>>,>>>,>>9".  
DEFINE VAR TotInt  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". 
DEFINE VAR TotVdo  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". 
DEFINE VAR CTotVdo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TTotVdo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR PorMor  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". 
DEFINE VAR TotCarNM AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR TaCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR TeCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR W_Garantias AS DECIMAL FORMAT ">>>,>>>,>>>,>>9". 
DEFI   VAR Dias_Vdos  LIKE creditos.dias_atraso.
    
DEFINE VAR TApo LIKE Ahorros.Sdo_Disponible.

FOR EACH TTCreditos: DELETE TTCreditos. END.

ASSIGN tot1 = 0 tot2 = 0 tot3 = 0 tot4 = 0 tot5 = 0 tot5 = 0 tot6 = 0
       tot7 = 0 tot8 = 0 tot9 = 0 tot10 = 0 TotCarNM = 0
       TCreCla = 0 TaCreCla = 0 TeCreCla = 0.

ASSIGN TotSdo = 0
       TotReg = 0
       TotMon = 0
       TotInt = 0
       TotVdo = 0.

DEFINE VAR FP1 LIKE Creditos.FOR_Pago.
DEFINE VAR FP2 LIKE Creditos.FOR_Pago.
DEFINE VAR FPN AS CHARACTER FORMAT "X(20)".

CASE RFPago:
    WHEN 1 THEN ASSIGN FP1 = 1 FP2 = 1 FPN = "Caja".
    WHEN 2 THEN ASSIGN FP1 = 2 FP2 = 2 FPN = "Nomina".
    WHEN 3 THEN ASSIGN FP1 = 1 FP2 = 2 FPN = "Todos".
END CASE.


DISPLAY "Filtros del Informe" SKIP
        "Agencia      : " STRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Filtros,"X(40)") FORMAT "X(30)" SKIP
        "Tipo Producto: " STRING(Cmb_TProducto:SCREEN-VALUE IN FRAME F_Filtros,"X(40)") FORMAT "X(30)" SKIP
        "Forma de Pago de los Creditos: " FPN
       /* "Prod  Credito: " STRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Filtros,"X(40)") FORMAT "X(30)"*/
    WITH FRAME Fenc3 WIDTH 250 NO-LABELS USE-TEXT NO-BOX STREAM-IO.

DISPLAY "               CODIGO     NOMBRE                                MONTO      SDO.CAPITAL      TOTAL VENCIDO       INTERESES    NUM.CREDITOS"
    WITH FRAME Fti WIDTH 250.
/*TpdFin = 5. */
FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND           
           Creditos.Dias_Atraso  GE ValIni AND
           Creditos.Dias_Atraso  LE ValFin AND
           Creditos.Sdo_Capital  GT 0                    NO-LOCK:
    Dias_Vdos = creditos.dias_atraso.
    IF creditos.estado NE 2   THEN NEXT.
    IF creditos.tip_credito GT 4 THEN NEXT.
    FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    IF Creditos.FOR_Pago EQ 2 AND AVAIL (Clientes) AND Creditos.Val_Atraso GT 0 THEN DO:
       FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
       IF AVAIL(Empresas) THEN 
          ASSIGN Dias_Vdos = Dias_Vdos - Empresas.Dias_Gracia.
    END. 

    CREATE TTCreditos.
    BUFFER-COPY Creditos TO TTCreditos.

    TTCreditos.Dias_Atraso = Dias_Vdos.

    IF fp1 = 1 AND fp2 = 1 AND creditos.FOR_pago NE 1 THEN TTCreditos.dias_atraso = 0.
    IF fp1 = 2 AND fp2 = 2 AND creditos.FOR_pago NE 2 THEN TTCreditos.dias_atraso = 0.

    
    IF     TTCreditos.Dias_Atraso GT 1080 THEN
       ASSIGN DM = 1080
              NM = "Superior a 3 Años".
    ELSE IF TTCreditos.Dias_Atraso GT 720 THEN
       ASSIGN DM = 720
              NM = "Superior a 2 Años Hasta 1080 Dias".
    ELSE IF TTCreditos.Dias_Atraso GT 540 THEN
       ASSIGN DM = 540
              NM = "Superior a 18 Meses Hasta 720 Dias".
    ELSE IF TTCreditos.Dias_Atraso GT 360 THEN
       ASSIGN DM = 360
              NM = "Superior a Un Año Hasta 540 Dias".
    ELSE IF TTCreditos.Dias_Atraso GT 180 THEN
       ASSIGN DM = 180
              NM = "Superior a 6 Meses Hasta 360 Dias".
    ELSE IF TTCreditos.Dias_Atraso GT 120 THEN
       ASSIGN DM = 120
              NM = "Desde 121 Dias Hasta 180 Dias".
    ELSE IF TTCreditos.Dias_Atraso GT 90 THEN
       ASSIGN DM = 90
              NM = "Desde 91 Dias Hasta 120 Dias".
    ELSE IF TTCreditos.Dias_Atraso GT 60 THEN
       ASSIGN DM = 60
              NM = "Desde 61 Dias Hasta 90 Dias".
    ELSE IF TTCreditos.Dias_Atraso GT 30 THEN
       ASSIGN DM = 30
              NM = "Desde 31 Dias Hasta 60 Dias".
    ELSE 
       ASSIGN DM = 0
              NM = "Desde 0 Dias Hasta 30 Dias".
END.


FOR EACH TTCreditos NO-LOCK BREAK BY TTCreditos.Agencia BY TTCreditos.DM: 
      IF FIRST-OF(TTCreditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ TTCreditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN
            DISPLAY CAPS(Agencias.Nombre) AT 1  FORMAT "X(30)"
            WITH FRAME TitCartAge WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
         ELSE
            DISPLAY "Tipo de Cartera no definida" AT 1 WITH FRAME TitCartAge2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
      END.
      
      IF FIRST-OF(TTCreditos.DM) THEN
         ASSIGN CTotMon = 0 CTotSdo = 0 CTotInt = 0 CTotVdo = 0.

      j = j + 1.
      RUN Progreso.

      IF TTCreditos.Dias_Atraso LE 30 THEN
         TotCarNM = TotCarNM + TTCreditos.Sdo_Capital.

      ASSIGN W_Interes = TTCreditos.INT_Corriente + TTCreditos.INT_DifCobro + TTCreditos.INT_MorCobrar - TTCreditos.INT_Anticipado
             TotVdo    = TotVdo  + TTCreditos.Val_Atraso + W_Interes
             CTotVdo   = CTotVdo + TTCreditos.Val_Atraso + W_Interes
             TTotVdo   = TTotVdo + TTCreditos.Val_Atraso + W_Interes.
      ASSIGN TotSdoAge = TotSdoAge + TTCreditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + TTCreditos.Monto
             TotSdo    = TotSdo + TTCreditos.Sdo_Capital
             TotInt    = TotInt + W_Interes
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + TTCreditos.Monto
             CTotSdo    = CTotSdo + TTCreditos.Sdo_Capital
             CTotInt    = CTotInt + W_Interes
             Tot1       = Tot1 + TTCreditos.Monto
             Tot2       = Tot2 + TTCreditos.Sdo_Capital
             Tot3       = Tot3 + W_Interes
             Tot4       = Tot4 + TTCreditos.INT_MorCobrar
             TCreCla    = TCreCla + 1.

      IF LAST-OF(TTCreditos.DM)  THEN DO:
         DISPLAY TTCreditos.NM   AT 16 FORMAT "X(40)"
                 CTotMon   AT 61 FORMAT "->,>>>,>>>,>>9"
                 CTotSdo   AT 76 FORMAT "->,>>>,>>>,>>9"
                 CTotInt   AT 91 FORMAT "->,>>>,>>>,>>9"
                 CTotVdo   AT 106 FORMAT "->,>>>,>>>,>>9"
                 TCreCla   AT 121 FORMAT ">>,>>>,>>9"
           WITH FRAME F_TotCar WIDTH 250 USE-TEXT NO-BOX NO-LABELS STREAM-IO.

         ASSIGN TaCreCla = TaCreCla + TCreCla.

         ASSIGN CTotMon = 0 
                CTotSdo = 0 
                CTotInt = 0 
                TCreCla = 0
                CTotVdo = 0.         
      END.

      IF LAST-OF(TTCreditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ TTCreditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN
            DISPLAY SKIP(1) "TOTALES:" AT 1 CAPS(Agencias.Nombre) AT 15  FORMAT "X(30)"
                 Tot1     AT 61  FORMAT "->,>>>,>>>,>>9"
                 Tot2     AT 76  FORMAT "->,>>>,>>>,>>9"
                 Tot3     AT 91  FORMAT "->,>>>,>>>,>>9" 
                 TTotVdo   AT 106 FORMAT "->,>>>,>>>,>>9"
                 TaCreCla  AT 121 FORMAT ">>,>>>,>>9"
                 SKIP(1)
         WITH FRAME F_TotCar2 WIDTH 250 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
         ASSIGN TeCreCla = TeCreCla + TaCreCla.
         /*ASSIGN CTotMon = 0 CTotSdo = 0 CTotInt = 0.*/
         ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 TaCreCla = 0 TTotVdo = 0.
      END.
  END.

  DISPLAY SKIP(1)
    "Total de Registros  :"   AT 1
     TotReg                   AT 25
    "Total x Entidad  : "     AT 42
     TotMon                   AT 60
     TotSdo                   AT 76
     TotInt                   AT 92  FORMAT "->,>>>,>>>,>>9"
     TotVdo                   AT 108
     TeCreCla                 AT 124 FORMAT ">>,>>>,>>9"
  WITH FRAME ttCartera WIDTH 250 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO. 
  
  DISPLAY "Porcentaje de Morosidad: "    ROUND((((TotSdo - TotCarNM) / TotSdo) * 100),2) format ">>9.99%".


  /*IF AgeIni NE AgeFin THEN
     RUN TOTAL_CarteraAg.*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Estados wWin 
PROCEDURE Informes_Creditos_Estados :
DEFINE VAR CTotMon AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotSdo AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotInt AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR WNomAge AS CHARACTER FORMAT "X(25)".
ASSIGN tot1 = 0 tot2 = 0 tot3 = 0 tot4 = 0 tot5 = 0 tot5 = 0 tot6 = 0 tot7 = 0 tot8 = 0 tot9 = 0 tot10 = 0.
ASSIGN TotSdo = 0.

DEFINE VAR W_Nus AS CHARACTER FORMAT "X(20)".  
DISPLAY                                 "                               #CRED    VAL.TOT    #CRED    VAL.TOT    #CRED    VAL.TOT    #CRED    VAL.TOT    #CRED    VAL.TOT" AT 1
  WITH FRAME F_Enc2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.        
  
  FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND
           Creditos.Cod_Credito  GE ProIni AND
           Creditos.Cod_Credito  LE ProFin
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Estado: 
      j = j + 1.
      RUN Progreso.
      CASE Creditos.Estado:
        WHEN 1 THEN ASSIGN Tot1 = Tot1 + 1 Tot2 = Tot2 + Creditos.Sdo_Capital.
        WHEN 2 THEN ASSIGN Tot3 = Tot3 + 1 Tot4 = Tot4 + Creditos.Sdo_Capital.
        WHEN 3 THEN ASSIGN Tot5 = Tot5 + 1 Tot6 = Tot6 + Creditos.Sdo_Capital.
        WHEN 4 THEN ASSIGN Tot7 = Tot7 + 1 Tot8 = Tot8 + Creditos.Sdo_Capital.
        WHEN 5 THEN ASSIGN Tot9 = Tot9 + 1 Tot10 = Tot10 + Creditos.Sdo_Capital.
      END CASE.
      IF LAST-OF(Creditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN WNomAge = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
         DISPLAY WNomAge AT 1
                 Tot1    AT 30 FORMAT ">>,>>9"
                 Tot2    AT 37 FORMAT ">>>>,>>>,>>9" 
                 Tot3    AT 50 FORMAT ">>,>>9"
                 Tot4    AT 57 FORMAT ">,>>>,>>>,>>9" 
                 Tot5    AT 70 FORMAT ">>,>>9"
                 Tot6    AT 77 FORMAT ">,>>>,>>>,>>9" 
                 Tot7    AT 90 FORMAT ">>,>>9"
                 Tot8    AT 97 FORMAT ">,>>>,>>>,>>9" 
                 Tot9    AT 110 FORMAT ">>,>>9"
                 Tot10   AT 117 FORMAT ">,>>>,>>>,>>9" 
          WITH FRAME totagencia WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS.          
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_IndiceMor wWin 
PROCEDURE Informes_Creditos_IndiceMor :
DEFINE VAR CTotMon AS DECIMAL FORMAT ">,>>>,>>>,>>>,>>9".
DEFINE VAR CTotSdo AS DECIMAL FORMAT ">,>>>,>>>,>>>,>>9".
DEFINE VAR CTotInt AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotMon  AS DECIMAL FORMAT ">,>>>,>>>,>>>,>>9".  
DEFINE VAR TotInt  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". 
DEFINE VAR PorMor  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". 
DEFINE VAR TotCarNM AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR TaCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR TeCreCla AS DECIMAL FORMAT ">>,>>>,>>9".  
DEFINE VAR W_Garantias AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".  
    
DEFINE VAR TApo LIKE Ahorros.Sdo_Disponible.


ASSIGN tot1 = 0 tot2 = 0 tot3 = 0 tot4 = 0 tot5 = 0 tot5 = 0 tot6 = 0
       tot7 = 0 tot8 = 0 tot9 = 0 tot10 = 0 TotCarNM = 0
       TCreCla = 0 TaCreCla = 0 TeCreCla = 0. 
ASSIGN TotSdo = 0
       TotReg = 0
       TotMon = 0
       TotInt = 0.

DISPLAY "Filtros del Informe" SKIP
        "Agencia      : " STRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Filtros,"X(40)") FORMAT "X(30)" SKIP
        "Tipo Producto: " STRING(Cmb_TProducto:SCREEN-VALUE IN FRAME F_Filtros,"X(40)") FORMAT "X(30)" SKIP
        "Prod  Credito: " STRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Filtros,"X(40)") FORMAT "X(30)"
    WITH FRAME Fenc3 WIDTH 250 NO-LABELS USE-TEXT NO-BOX STREAM-IO.

DISPLAY "               CODIGO     NOMBRE                                MONTO      SDO.CAPITAL      INTERESES    NUM.CREDITOS"
    WITH FRAME Fti WIDTH 250.
  FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND
           Creditos.Cod_Credito  GE ProIni AND
           Creditos.Cod_Credito  LE ProFin AND
           Creditos.Dias_Atraso  GE ValIni AND
           Creditos.Dias_Atraso  LE ValFin AND 
           Creditos.Sdo_Capital  GT 0      
           NO-LOCK BREAK BY Creditos.Agencia BY Creditos.Cod_Califica BY Creditos.Cod_Credito: 
      IF creditos.estado NE 2 THEN NEXT.
      IF creditos.tip_credito GT 4 THEN NEXT.
      IF FIRST-OF(Creditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN
            DISPLAY CAPS(Agencias.Nombre) AT 1  FORMAT "X(30)"
            WITH FRAME TitCartAge WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
         ELSE
            DISPLAY "Tipo de Cartera no definida" AT 1 WITH FRAME TitCartAge2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
      END.
      IF FIRST-OF(Creditos.Cod_Califica) THEN
         ASSIGN CTotMon = 0 CTotSdo = 0 CTotInt = 0.
      j = j + 1.
      RUN Progreso.
      W_Interes = Creditos.INT_Corriente + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.
      ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Monto
             TotSdo    = TotSdo + Creditos.Sdo_Capital
             TotInt    = TotInt + W_Interes
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Monto
             CTotSdo    = CTotSdo + Creditos.Sdo_Capital
             CTotInt    = CTotInt + W_Interes
             Tot1       = Tot1 + Creditos.Monto
             Tot2       = Tot2 + Creditos.Sdo_Capital
             Tot3       = Tot3 + W_Interes
             Tot4       = Tot4 + INT_MorCobrar
             TCreCla    = TCreCla + 1.
      IF LAST-OF(Creditos.Cod_Califica) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Creditos.Cod_Califica NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN
            DISPLAY Varios.Codigo            AT 15
                    CAPS(Varios.Descripcion) AT 23 FORMAT "X(30)"
                    CTotMon   AT 61 FORMAT "->,>>>,>>>,>>9"
                    CTotSdo   AT 76 FORMAT "->,>>>,>>>,>>9"
                    CTotInt   AT 92 FORMAT "->,>>>,>>>,>>9"
                    TCreCla   AT 108 FORMAT ">>,>>>,>>9"
         WITH FRAME F_TotCar WIDTH 250 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
         ASSIGN TaCreCla = TaCreCla + TCreCla.
         IF Creditos.cod_califica LE 2 THEN
             TotCarNM = TotCarNM + CTotSdo.
         ASSIGN CTotMon = 0 CTotSdo = 0 CTotInt = 0 TCreCla = 0.
         
      END.
      IF LAST-OF(Creditos.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN
            DISPLAY SKIP(1) "TOTALES:" AT 1 CAPS(Agencias.Nombre) AT 15  FORMAT "X(30)"
                 Tot1     AT 61  FORMAT "->,>>>,>>>,>>9"
                 Tot2     AT 76  FORMAT "->,>>>,>>>,>>9"
                 Tot3     AT 93  FORMAT "->,>>>,>>>,>>9" 
                 TaCreCla AT 108 FORMAT ">>,>>>,>>9"
                 SKIP(1)
         WITH FRAME F_TotCar2 WIDTH 250 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
         ASSIGN TeCreCla = TeCreCla + TaCreCla.
         /*ASSIGN CTotMon = 0 CTotSdo = 0 CTotInt = 0.*/
         ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 TaCreCla = 0.
      END.
  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"   AT 1
     TotReg                   AT 25
    "Total x Entidad  : "     AT 42
     TotMon                   AT 60
     TotSdo                   AT 74
     TotInt                   AT 93  FORMAT "->,>>>,>>>,>>9"
     TeCreCla                 AT 108 FORMAT ">>,>>>,>>9"
  WITH FRAME ttCartera WIDTH 250 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO. 
  DISPLAY "Porcentaje de Morosidad: "    ROUND((((TotSdo - TotCarNM) / TotSdo) * 100),2) format ">>9.99%".

  IF AgeIni NE AgeFin THEN
     RUN TOTAL_CarteraAg.  /*Marzo 17/05 GAER*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Creditos_Instancias wWin 
PROCEDURE Informes_Creditos_Instancias :
DEFINE VAR CTotMon AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotSdo AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR CTotInt AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
DEFINE VAR TotMon  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".  
DEFINE VAR TotInt  AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
ASSIGN tot1 = 0 tot2 = 0 tot3 = 0 tot4 = 0 tot5 = 0 tot5 = 0 tot6 = 0 tot7 = 0 tot8 = 0 tot9 = 0 tot10 = 0.
ASSIGN TotSdo = 0.
   FOR EACH Instancias WHERE 
            Instancias.Tipo_Instancia EQ INTEGER(SUBSTRING(Cmb_EstCre:SCREEN-VALUE IN FRAME F_Filtros,1,1)) AND
            Instancias.Estado EQ 1 NO-LOCK BREAK BY Instancias.Instancia:
       FOR EACH Mov_Instancias WHERE 
                Mov_Instancias.Instancia EQ Instancias.Instancia AND
                Mov_Instancias.Estado    EQ NO NO-LOCK 
                BREAK BY Mov_Instancias.Instancia BY Mov_Instancias.Fec_Ingreso:
           FIND Creditos WHERE 
                Creditos.Num_Credito EQ DECIMAL(Mov_Instancias.Cuenta) AND
                Creditos.Nit EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
           IF AVAILABLE Creditos THEN DO:
              RUN INFO_Creditos(OUTPUT W_TipPdt, OUTPUT W_ForPag).
              W_Interes = Creditos.INT_Corrientes + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.
              j = j + 1.
              RUN Progreso.
              ASSIGN Tot1 = Tot1 + 1
                     Tot2 = Tot2 + Creditos.Sdo_Capital
                     Tot3 = Tot3 + W_Interes
                     TotSdoAge = TotSdoAge + Creditos.Monto 
                     TotRegAge = TotReg + 1
                     TotMon    = TotMon + Creditos.Monto
                     TotSdo    = TotSdo + Creditos.Sdo_Capital
                     TotInt    = TotInt + W_Interes
                     TotReg    = TotReg + 1.
           END.
       END.
       IF LAST-OF(Instancias.Instancia) THEN DO:
          W_NIn = STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia.
          DISPLAY W_Nin     AT 1 FORMAT "X(35)"
                  Tot1      AT 40 FORMAT ">>,>>9"
                  Tot2      AT 50 FORMAT ">>,>>>,>>>,>>9"
                  Tot3      AT 70 FORMAT "->,>>>,>>>,>>9"
          WITH FRAME F_TitIns WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
       END.
       ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0.
   END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad     : "         AT 42
     TotSdo                          AT 77
     TotInt                          AT 92
  WITH FRAME ttInstancias WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Garantias wWin 
PROCEDURE Informes_Garantias :
Listado = W_PathSpl + "\Garantias.LST".

OS-DELETE VALUE(Listado).
ASSIGN FRAME F_Filtros ValIni ValFin.  
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(61)"
/*                  1         2         3         4         5         6         7         8         9         1
           12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
  INITIAL "AGE NIT                 ".
DEFINE VAR W_Cupo  LIKE Creditos.Monto.
DEFINE VAR W_Monto LIKE Creditos.Monto.
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR W_TipGar AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_Vigen  AS INTEGER FORMAT "9999".

ASSIGN J = 0 k = 0.

OUTPUT TO value(Listado).
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Solicitudes: " + Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  
  CASE Cmb_BGarantias:SCREEN-VALUE IN FRAME F_Basicos:
    WHEN "Garantias Admisibles" THEN
        W_EncColumna = WEnc1 + "NUM.SOLIC   NUM.CREDIT   ID.GARANTIA    NOM.GARANTIA                 VALOR.GARANTIA    TIPO".
    WHEN "Vencimiento Impuestos" THEN
       W_EncColumna = WEnc1 + "NUM.SOLIC   NUM.CREDIT   ID.GARANTIA    NOM.GARANTIA                 VALOR.GARANTIA    FEC.VEN.IMPUESTO".
    WHEN "Vencimientos Poliza" THEN
       W_EncColumna = WEnc1 + "NUM.SOLIC   NUM.CREDIT   ID.GARANTIA    NOM.GARANTIA                 VALOR.GARANTIA    FEC.VENCIMIENTO".
    WHEN "Cupos" THEN          
       W_EncColumna = WEnc1 + "ID.GARANTIA    NOM.GARANTIA                 VALOR.GARANTIA      MONTO.CREDITO   CUPO DISPONIBLE.".
       
  END CASE.
  VIEW FRAME F-Encabezado.
  FOR EACH Garantias WHERE 
           Garantias.Agencia      GE AgeIni AND
           Garantias.Agencia      LE AgeFin
           NO-LOCK BREAK BY Garantias.Agencia BY Garantias.Nit BY Garantias.Cod_Credito: 
      j = j + 1.
      CASE Garantias.Tipo_Garantia:
        WHEN 1 THEN W_TipGar = "Propiedad".
        WHEN 2 THEN W_TipGar = "Vehículo".
        WHEN 3 THEN W_TipGar = "Inversión".
      END CASE.
      RUN Progreso.
      CASE Cmb_BGarantias:SCREEN-VALUE IN FRAME F_Basicos:
        WHEN "Garantias Admisibles" THEN DO:
           IF Garantias.Estado EQ 1 THEN DO:
              DISPLAY Garantias.Agencia              AT 1  FORMAT "999"
                      Garantias.Nit                  AT 5  FORMAT "X(14)"
                      Garantias.Num_Solicitud        AT 25 FORMAT "999999999"
                      Garantias.Num_Credito          AT 37 FORMAT "999999999"
                      Garantias.Identificacion_Bien  AT 50 FORMAT "X(12)"
                      Garantias.Nom_Bien             AT 65 FORMAT "X(25)"
                      Garantias.Val_Bien             AT 94 FORMAT ">>>,>>>,>>>,>>9"
                      W_TipGar                       AT 112 FORMAT "X(10)"
              WITH FRAME F_G1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Garantias.Val_Bien
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Garantias.Val_Bien
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Vencimientos Poliza" THEN DO:
           IF Garantias.Fec_FinSeguro NE ? AND
              Garantias.Fec_FinSeguro GE FecIni AND
              Garantias.Fec_FinSeguro LE FecFin AND
              Garantias.Estado EQ 1 THEN DO:
              DISPLAY Garantias.Agencia              AT 1  FORMAT "999"
                      Garantias.Nit                  AT 5  FORMAT "X(14)"
                      Garantias.Num_Solicitud        AT 25 FORMAT "999999999"
                      Garantias.Num_Credito          AT 37 FORMAT "999999999"
                      Garantias.Identificacion_Bien  AT 50 FORMAT "X(12)"
                      Garantias.Nom_Bien             AT 65 FORMAT "X(25)"
                      Garantias.Val_Bien             AT 94 FORMAT ">>>,>>>,>>>,>>9"
                      GarantIas.Fec_FinSeguro        AT 112 FORMAT "99/99/9999"
              WITH FRAME F_G2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Garantias.Val_Bien
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Garantias.Val_Bien
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Cupos" THEN DO:
           IF Garantias.Estado EQ 1 THEN DO:
              IF Garantias.Num_Credito GT 0 THEN DO:
                 FIND Creditos WHERE Creditos.Num_Credito EQ Garantias.Num_Credito AND
                                     Creditos.Cod_Credito EQ Garantias.Cod_Credito AND
                                     Creditos.Tip_Credito EQ Garantias.Tip_Credito AND
                                     Creditos.Nit         EQ Garantias.Nit NO-LOCK NO-ERROR.
                 IF AVAILABLE Creditos  THEN DO:
                    ASSIGN W_Monto = Creditos.Monto
                           W_Cupo  = Garantias.Val_Bien - Creditos.Monto.
                    DISPLAY Garantias.Agencia              AT 1  FORMAT "999"
                            Garantias.Nit                  AT 5  FORMAT "X(14)"
                            Garantias.Identificacion_Bien  AT 25 FORMAT "X(12)"
                            Garantias.Nom_Bien             AT 40 FORMAT "X(25)"
                            Garantias.Val_Bien             AT 70 FORMAT ">>>,>>>,>>>,>>9"
                            W_Monto                        AT 87 FORMAT ">>>,>>>,>>>,>>9"
                            W_Cupo                         AT 105 FORMAT ">>>,>>>,>>>,>>9"
                    WITH FRAME F_G3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
                    ASSIGN TotSdoAge = TotSdoAge + Garantias.Val_Bien
                           TotRegAge = TotReg + 1
                           TotSdo    = TotSdo + Garantias.Val_Bien
                           TotReg    = TotReg + 1.
                 END.
                 ELSE 
                   DISPLAY Garantias.Nit Garantias.Cod_Credito Garantias.Tip_Credito
                   WITH FRAME F_Gi WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              END.
           END.
        END.
        WHEN "Vencimiento Impuestos" THEN DO:
           IF Garantias.Fec_FinSeguro NE ? AND
              Garantias.Fec_VctoImpuesto GE FecIni AND
              Garantias.Fec_VctoImpuesto LE FecFin AND
              Garantias.Estado EQ 1 THEN DO:
              DISPLAY Garantias.Agencia              AT 1  FORMAT "999"
                      Garantias.Nit                  AT 5  FORMAT "X(14)"
                      Garantias.Num_Solicitud        AT 25 FORMAT "999999999"
                      Garantias.Num_Credito          AT 37 FORMAT "999999999"
                      Garantias.Identificacion_Bien  AT 50 FORMAT "X(12)"
                      Garantias.Nom_Bien             AT 65 FORMAT "X(25)"
                      Garantias.Val_Bien             AT 94 FORMAT ">>>,>>>,>>>,>>9"
                      Garantias.Fec_VctoImpuesto     AT 112 FORMAT "99/99/9999"
              WITH FRAME F_G4 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.
              ASSIGN TotSdoAge = TotSdoAge + Garantias.Val_Bien
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Garantias.Val_Bien
                     TotReg    = TotReg + 1.
           END.
        END.
      END CASE.
      IF LAST-OF(Garantias.Agencia) THEN
         DISPLAY SKIP(1)
                "Total de Registros Agencia "    AT 1
                 Garantias.Agencia                 AT 30 FORMAT "999"
                ": "                             AT 35
                 TotRegAge                       AT 40
                "Tot.Valor de las Garantias:"    AT 60
                 TotSdoAge                       AT 89
        WITH FRAME t1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
  END.
  DISPLAY SKIP(1)
    "Total de Registros        :"    AT 1
     TotReg                          AT 40
    "Tot.Valor de las Garantias:"    AT 60
     TotSdo                          AT 89
  WITH FRAME tt WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Solicitudes wWin 
PROCEDURE Informes_Solicitudes :
Listado = W_PathSpl + "\Solicitudes.LST".

OS-DELETE VALUE(Listado).
ASSIGN FRAME F_Filtros ValIni ValFin.  
DEFINE VAR NomCiiu AS CHARACTER FORMAT "X(25)". 
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(61)" 
/*                  1         2         3         4         5         6
           1234567890123456789012345678901234567890123456789012345678901234567890*/
  INITIAL "AGE NIT            PDT TIP.PDT        NUM.SOLIC  TASA  PLAZO ".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR W_TipPdt AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_Vigen  AS INTEGER FORMAT "9999".
DEFINE VAR W_ForPag AS CHARACTER FORMAT "X(10)".
ASSIGN tot1 = 0 tot2 = 0 tot3 = 0 tot4 = 0 tot5 = 0 tot5 = 0 tot6 = 0 tot7 = 0 tot8 = 0 tot9 = 0 tot10 = 0.
ASSIGN J = 0 k = 0.

OUTPUT TO value(Listado).
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Solicitudes: " + Cmb_BAhorros:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BSolicitudes:SCREEN-VALUE IN FRAME F_Basicos:
    WHEN "Básico" THEN       /*  1         2         3         4         5         6         7         8         9         0         1         2         3         4      
                        123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
        W_EncColumna = "AGENCIA              CREDITOS DE CONSUMO    CREDITOS.COMERCIAL       CREDITOS.HIPOTECARI          MICROCREDITO".
    WHEN "Fechas de Ingreso" THEN    /*  1         2         3         4         5         6         7         8         9         0         1         2         3         4      
                                123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
        W_EncColumna = "AGENCIA            SOLICITUD.EN.ESTUDIO   SOLICITUD.APROBADA         SOLICITUD.NEGADA              SOLIC.CONDICIONADAS". 
  END CASE.
  VIEW FRAME F-Encabezado.
  
  FOR EACH Solicitud WHERE 
           Solicitud.Agencia      GE AgeIni AND
           Solicitud.Agencia      LE AgeFin AND
           Solicitud.Cod_Credito  GE ProIni AND
           Solicitud.Cod_Credito  LE ProFin 
           NO-LOCK BREAK BY Solicitud.Agencia BY Solicitud.Tip_Credito BY Solicitud.Cod_Credito: 
      W_Vigen = TODAY - Solicitud.Fec_Solicitud.
      j = j + 1.
      RUN Progreso.
      CASE Cmb_BSolicitudes:SCREEN-VALUE IN FRAME F_Basicos:
        WHEN "Básico" THEN DO:
           IF Solicitud.Monto GE ValIni AND
              Solicitud.Monto LE ValFin AND 
              Solicitud.Fec_Aprobacion EQ ? THEN DO:
              CASE Solicitud.Tip_Credito:
                WHEN 1 THEN ASSIGN Tot1 = Tot1 + 1
                                   Tot2 = Tot2 + Solicitud.Monto.
                WHEN 2 THEN ASSIGN Tot3 = Tot3 + 1
                                   Tot4 = Tot4 + Solicitud.Monto.
                WHEN 3 THEN ASSIGN Tot5 = Tot5 + 1
                                   Tot6 = Tot6 + Solicitud.Monto.
                WHEN 4 THEN ASSIGN Tot7 = Tot7 + 1
                                   Tot8 = Tot8 + Solicitud.Monto.
              END CASE.
              ASSIGN TotSdoAge = TotSdoAge + Solicitud.Monto 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Solicitud.Monto
                     TotReg    = TotReg + 1.
           END.
        END.
        WHEN "Fechas de Ingreso" THEN DO:
           IF Solicitud.Monto         GE ValIni AND
              Solicitud.Monto         LE ValFin AND 
              Solicitud.Fec_Solicitud GE FecIni AND 
              Solicitud.Fec_Solicitud LE FecFin AND
              Solicitud.Fec_Aprobacion EQ ? THEN DO:
              CASE Solicitud.Estado:
                WHEN 1 THEN ASSIGN Tot1 = Tot1 + 1
                                   Tot2 = Tot2 + Solicitud.Monto.
                WHEN 2 THEN ASSIGN Tot3 = Tot3 + 1
                                   Tot4 = Tot4 + Solicitud.Monto.
                WHEN 3 THEN ASSIGN Tot5 = Tot5 + 1
                                   Tot6 = Tot6 + Solicitud.Monto.
                WHEN 4 THEN ASSIGN Tot7 = Tot7 + 1
                                   Tot8 = Tot8 + Solicitud.Monto.
              END CASE.
              ASSIGN TotSdoAge = TotSdoAge + Solicitud.Monto 
                     TotRegAge = TotReg + 1
                     TotSdo    = TotSdo + Solicitud.Monto
                     TotReg    = TotReg + 1.
           END.
        END.
      END CASE.
      IF LAST-OF(Solicitud.Agencia) THEN DO:
         DISPLAY "Agencia :"                   AT 1
                 Solicitud.Agencia            AT 12 FORMAT "999"
                 Tot1                         AT 20 FORMAT ">>,>>9"
                 Tot2                         AT 28 FORMAT ">>>>,>>>,>>9"
                 Tot3                         AT 45 FORMAT ">>,>>9"
                 Tot4                         AT 53 FORMAT ">>>>,>>>,>>9"
                 Tot5                         AT 70 FORMAT ">>,>>9"
                 Tot6                         AT 79 FORMAT ">>>>,>>>,>>9"
                 Tot7                         AT 95 FORMAT ">>,>>9"
                 Tot8                         AT 105 FORMAT ">>>>,>>>,>>9"
        WITH FRAME TSOLt1 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO. 
        ASSIGN tot1 = 0 tot2 = 0 tot3 = 0 tot4 = 0 tot5 = 0 tot5 = 0 tot6 = 0 tot7 = 0 tot8 = 0 tot9 = 0 tot10 = 0.
      END.
  END.
  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Usuarios wWin 
PROCEDURE Informes_Usuarios :
Listado = W_PathSpl + "\Usuarios.LST".

OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)".
DEFINE VAR WEnc2 AS CHARACTER FORMAT "X(2)".
DEFINE VAR TotReg AS DECIMAL.
OUTPUT TO value(Listado) PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Usuarios: " + Cmb_BClientes:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_BUsuarios:
    WHEN "Pendientes de Cobro" THEN  
        W_EncColumna = "Num.Credito Nit               Detalle                                                        Fec.Ingreso  Fec.Limite Cumplido".
  END CASE.
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
  FOR EACH Usuarios WHERE 
           DECIMAL(Usuarios.Usuario) GE DECIMAL(UsuIni) AND
           DECIMAL(Usuarios.Usuario) LE DECIMAL(UsuFin)
           NO-LOCK BREAK BY Usuarios.Usuario:  
      j = j + 1.
      RUN Progreso.
      CASE Cmb_BUsuarios:SCREEN-VALUE IN FRAME F_Basicos:
        WHEN "Pendientes de Cobro" THEN DO:
          FOR EACH Hoja_Vida WHERE
                   Hoja_Vida.Tipo   EQ 9 AND
                   Hoja_Vida.Codigo EQ 2 AND
                   Hoja_Vida.Fec_Grabacion GE FecIni AND
                   Hoja_Vida.Fec_Grabacion LE FecFin AND
                   Hoja_Vida.Usuario EQ Usuarios.Usuario NO-LOCK BREAK BY 
                   Hoja_Vida.Usuario BY Hoja_Vida.DoctoRefer:
              IF FIRST-OF(Hoja_Vida.Usuario) THEN DO:
                 WEnc1 = "Notas pendientes de Cobro del Usuario: " + Usuarios.Usuario + " - " + CAPS(Usuarios.Nombre).
                 DISPLAY SKIP(1)
                         "------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                         WEnc1 AT 1 SKIP(1)
                         "Num.Credito Nit               Detalle                                                        Fec.Ingreso  Fec.Limite Cumplido" AT 1 SKIP(1)
                         "------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                 WITH FRAME F_U1 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
              END.
              IF Hoja_Vida.Asunto_Cumplido THEN WEnc2 = "Si".
              ELSE WEnc2 = "No".
              DISPLAY 
                 Hoja_Vida.DoctoRefer  AT 1 FORMAT "999999999"
                 Hoja_Vida.Nit         AT 12 FORMAT "X(14)"
                 SUBSTRING(Hoja_Vida.Observacion,1,100) AT 30 FORMAT "x(60)"
                 Hoja_Vida.Fec_Grabacion AT 95
                 Hoja_Vida.Fec_Limite    AT 107
                 WEnc2 AT 120 FORMAT "X(2)"
                 WITH FRAME F_U3 WIDTH 132 NO-LABEL NO-BOX USE-TEXT.
          END.
        END.
      END CASE.
  END.
  DISPLAY SKIP "Total de Registros Reportados: " TotReg WITH FRAME FT WIDTH 132 NO-LABELS.
  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Info_Creditos wWin 
PROCEDURE Info_Creditos :
DEFINE OUTPUT PARAMETER W_Tip AS CHARACTER FORMAT "X(15)".      
DEFINE OUTPUT PARAMETER W_For AS CHARACTER FORMAT "X(15)".      
      CASE Creditos.Tip_Credito:
        WHEN 1 THEN W_Tip = "Consumo".
        WHEN 2 THEN W_Tip = "Comercial".
        WHEN 3 THEN W_Tip = "Hipotecario".
        WHEN 4 THEN W_Tip = "MicroCredito".
      END CASE.
      CASE Creditos.FOR_Pago:
        WHEN 1 THEN W_For = "Nomina".
        WHEN 2 THEN W_For = "Caja".
        WHEN 3 THEN W_For = "DB.Automatic".
      END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DO WITH FRAME F_Filtros:
     FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
        W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
        IF Agencias.Agencia EQ W_Agencia THEN
           Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
     END.
  END.
  FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  IF AVAILABLE Entidad THEN DO:
     FIND Indicadores WHERE Indicadores.Indicador EQ Entidad.ind_SMLV NO-LOCK NO-ERROR.
     IF AVAILABLE Indicadores THEN Minimo = Indicadores.Valor.
  END.
  
  W_DiaFin = DAY(TODAY).
  RUN SUPER.
  DO WITH FRAME F_Filtros:
    ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month1.gif")
           W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
           AnoIni:SCREEN-VALUE = "1900"
           W_DiaIni:SCREEN-VALUE = "01"
           AnoFin:SCREEN-VALUE = STRING(YEAR(TODAY))
           MesIni = 1
           MesFin = MONTH(TODAY)
           AgeIni = W_Agencia
           AgeFin = W_Agencia
           FecIni = DATE("01/01/1900")
           FecFin = W_Fecha.
   END.
   Cmb_Tipo:SCREEN-VALUE IN FRAME F_Basicos = Cmb_Tipo:ENTRY(1).
   Cmb_BClientes:SCREEN-VALUE IN FRAME F_Basicos = Cmb_BClientes:ENTRY(1).
   APPLY "Value-changed" TO Cmb_Tipo IN FRAME F_Basicos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Progreso wWin 
PROCEDURE Progreso :
DO WITH FRAME F_Progreso:
  IF j EQ 250 THEN DO:
        j = 0.
        k = k + 1.
        ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
               R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15.
               R9:BGCOLOR = 15.
        CASE k:
          WHEN 1 THEN
             R1:BGCOL = 18.
          WHEN 2 THEN
             R2:BGCOL = 18.
          WHEN 3 THEN
             R3:BGCOL = 18.
          WHEN 4 THEN
             R4:BGCOL = 18.
          WHEN 5 THEN
             R5:BGCOL = 18.
          WHEN 6 THEN
             R6:BGCOL = 18.
          WHEN 7 THEN
             R7:BGCOL = 18.
          WHEN 8 THEN
             R8:BGCOL = 18.
          WHEN 9 THEN
             R9:BGCOL = 18.
        END CASE.
        IF k = 9 THEN k = 0.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TOTAL_CarteraAg wWin 
PROCEDURE TOTAL_CarteraAg :
/*------------------------------------------------------------------------------
  Purpose:  Total genral por cada calificacion.
  Notes:    Marzo 17/05 GAER.
------------------------------------------------------------------------------*/
  DEFINE VAR CTotMon AS DECIMAL FORMAT ">,>>>,>>>,>>>,>>9".
  DEFINE VAR CTotSdo AS DECIMAL FORMAT ">,>>>,>>>,>>>,>>9".  
  DEFINE VAR CTotInt AS DECIMAL FORMAT "->,>>>,>>>,>>9".  
  DEFINE VAR TotMon  AS DECIMAL FORMAT ">,>>>,>>>,>>>,>>9". 
  DEFINE VAR TotInt  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  DEFINE VAR PorMor  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  DEFINE VAR TotCarNM AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". 

  ASSIGN tot1 = 0 tot2 = 0 tot3 = 0 tot4 = 0 tot5 = 0 tot5 = 0 tot6 = 0
         tot7 = 0 tot8 = 0 tot9 = 0 tot10 = 0 TotCarNM = 0 
         TotSdo = 0 CTotMon = 0 CTotSdo = 0 CTotInt = 0
         TotMon = 0 TotSdo = 0  TotInt = 0.

  DISPLAY "---------------------------------------------------------------------------------------------------------------" SKIP(1)
          "                              TOTALES GENERALES"  SKIP(1)
      WITH WIDTH 150 FRAME FTT5.

  FOR EACH Creditos WHERE 
           Creditos.Agencia      GE AgeIni AND
           Creditos.Agencia      LE AgeFin AND
           Creditos.Tip_Credito  GE TPdIni AND
           Creditos.Tip_Credito  LE TpdFin AND
           Creditos.Cod_Credito  GE ProIni AND
           Creditos.Cod_Credito  LE ProFin AND
           Creditos.Estado       EQ 2      AND
           Creditos.Dias_Atraso  GE ValIni AND
           Creditos.Dias_Atraso  LE ValFin 
           NO-LOCK BREAK BY Creditos.Cod_Califica BY Creditos.Cod_Credito: 
      
      IF FIRST-OF(Creditos.Cod_Califica) THEN
         ASSIGN CTotMon = 0 CTotSdo = 0 CTotInt = 0.
      j = j + 1.
      RUN Progreso.
      W_Interes = Creditos.INT_Corriente + Creditos.INT_DifCobro + Creditos.INT_MorCobrar - Creditos.INT_Anticipado.
      ASSIGN TotSdoAge = TotSdoAge + Creditos.Monto 
             TotRegAge = TotReg + 1
             TotMon    = TotMon + Creditos.Monto
             TotSdo    = TotSdo + Creditos.Sdo_Capital
             TotInt    = TotInt + W_Interes
             TotReg    = TotReg + 1
             CTotMon    = CTotMon + Creditos.Monto
             CTotSdo    = CTotSdo + Creditos.Sdo_Capital
             CTotInt    = CTotInt + W_Interes
             Tot1       = Tot1 + Creditos.Monto
             Tot2       = Tot2 + Creditos.Sdo_Capital
             Tot3       = Tot3 + W_Interes
             Tot4       = Tot4 + INT_MorCobrar.

      IF LAST-OF(Creditos.Cod_Califica) THEN DO:         
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Creditos.Cod_Califica NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN
            DISPLAY Varios.Codigo            AT 15
                    CAPS(Varios.Descripcion) AT 23 FORMAT "X(30)"
                    CTotMon   AT 61 FORMAT "->,>>>,>>>,>>9"
                    CTotSdo   AT 76 FORMAT "->,>>>,>>>,>>9"
                    CTotInt   AT 92 FORMAT "->,>>>,>>>,>>9"
         WITH FRAME F_TotCar WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
         IF Creditos.cod_califica LE 2 THEN
             TotCarNM = TotCarNM + CTotSdo.
         ASSIGN CTotMon = 0 CTotSdo = 0 CTotInt = 0.
         
      END.

  END.
  DISPLAY SKIP(1)
    "Total de Registros  :"          AT 1
     TotReg                          AT 25
    "Total x Entidad  : "            AT 42
     TotMon                          AT 60
     TotSdo                          AT 74
     TotInt                          AT 93 FORMAT "->,>>>,>>>,>>9" SKIP(2)
  WITH FRAME ttCartera WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE. 
  DISPLAY "Porcentaje de Morosidad: "    ROUND((((TotSdo - TotCarNM) / TotSdo) * 100),2) format ">>9.99%".

  ASSIGN TotMon = 0
         TotSdo = 0
         TotInt = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

