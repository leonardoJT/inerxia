&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnAgncia btnBis btnCrrra RECT-306 RECT-307 ~
RECT-308 RECT-309 cDrccion BUTTON-BLANCO BUTTON-rtrcso BUTTON-RETURN ~
BUTTON-limpiar btncal BUTTONCE cTxtoABrrar btnIntrior btnKlmtro btnLte ~
btnMnzna btnPrque btnL btnR btnS btna btnAprtdo btnAprtmnto btnAutpsta ~
btnAvnda btnAvndaCrrra btnb btnBlque btnc btnCiuddla btnClle btnCnjnto ~
btnCnjntoRsdncial btnCsa btnd btnDiagnal btnE btnEdfcio btnEntrda btnEste ~
btnEtpa btnF btnMncpio btnNrte btnOeste btnOfcna btnPenthouse btnPseo ~
btnPso btnSctor btnSperMnzna btnSuite btnSur btnTrnsvrsal btnTrre btnUndad ~
btnUnidadRsdncial btnUrbnzcion btnZna btnCero btnCinco btnCuatro btnDos ~
btnG btnH btnI btnJ btnK btnM btnN btnNueve btnO btnOcho btnP btnQ btnSeis ~
btnSiete btnT btnTres btnU btnUno btnV btnW btnX btnY btnZ btnCntroCmrcial ~
btnDpstoStno btnAdmnstrcion btnAgrpcion btnAlmcen btnAltllo btnBdga ~
btnBlvar btnClla btnCmno btnCnsltrio btnCrclar btnCrcnvlar btnCrrgmiento ~
btnCrrtra btnDprtmnto btnDpsto btnEsquina btnExtrior btnFnca btnGrge ~
btnOccidnte btnOriente btnPlnta btnPrcla btnPrdio btnPrqueadro btnPrtria ~
btnPsje btnPuente btnPuesto btnSlar btnSlon btnSlonCmnal btnSmsctor btnStno ~
btnTrmnal btnTrrza btnVariante btnVrda btnZnaFrnca 
&Scoped-Define DISPLAYED-OBJECTS cDrccion cTxtoABrrar FILL-IN-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btna  NO-FOCUS
     LABEL "A" 
     SIZE 3 BY 1.12 TOOLTIP "A"
     FONT 8.

DEFINE BUTTON btnAdmnstrcion  NO-FOCUS FLAT-BUTTON
     LABEL "ADMINISTRACION" 
     SIZE 15 BY 1.12 TOOLTIP "Administración"
     FONT 8.

DEFINE BUTTON btnAgncia  NO-FOCUS
     LABEL "AGENCIA" 
     SIZE 15 BY 1.12 TOOLTIP "Agencia"
     FONT 8.

DEFINE BUTTON btnAgrpcion  NO-FOCUS FLAT-BUTTON
     LABEL "AGRUPACION" 
     SIZE 15 BY 1.12 TOOLTIP "Agrupación"
     FONT 8.

DEFINE BUTTON btnAlmcen  NO-FOCUS FLAT-BUTTON
     LABEL "ALMACEN" 
     SIZE 15 BY 1.12 TOOLTIP "Almacén"
     FONT 8.

DEFINE BUTTON btnAltllo  NO-FOCUS FLAT-BUTTON
     LABEL "ALTILLO" 
     SIZE 15 BY 1.12 TOOLTIP "Altillo"
     FONT 8.

DEFINE BUTTON btnAprtdo  NO-FOCUS
     LABEL "APARTADO" 
     SIZE 15 BY 1.12 TOOLTIP "Apartado"
     FONT 8.

DEFINE BUTTON btnAprtmnto  NO-FOCUS
     LABEL "APARTAMENTO" 
     SIZE 15 BY 1.12 TOOLTIP "Apartamento".

DEFINE BUTTON btnAutpsta  NO-FOCUS
     LABEL "AUTOPISTA" 
     SIZE 15 BY 1.12 TOOLTIP "Autopista".

DEFINE BUTTON btnAvnda  NO-FOCUS
     LABEL "AVENIDA" 
     SIZE 15 BY 1.12 TOOLTIP "Avenida".

DEFINE BUTTON btnAvndaCrrra  NO-FOCUS
     LABEL "AVENIDA CARRERA" 
     SIZE 15 BY 1.12 TOOLTIP "AVENIDA CARRERA"
     FONT 15.

DEFINE BUTTON btnb  NO-FOCUS
     LABEL "B" 
     SIZE 3 BY 1.12 TOOLTIP "B"
     FONT 8.

DEFINE BUTTON btnBdga  NO-FOCUS FLAT-BUTTON
     LABEL "BODEGA" 
     SIZE 15 BY 1.12 TOOLTIP "BODEGA"
     FONT 8.

DEFINE BUTTON btnBis  NO-FOCUS
     LABEL "BIS" 
     SIZE 15 BY 1.12 TOOLTIP "BIS"
     FONT 8.

DEFINE BUTTON btnBlque  NO-FOCUS
     LABEL "BLOQUE" 
     SIZE 15 BY 1.12 TOOLTIP "BLOQUE"
     FONT 8.

DEFINE BUTTON btnBlvar  NO-FOCUS FLAT-BUTTON
     LABEL "BULEVAR" 
     SIZE 15 BY 1.12 TOOLTIP "BULEVAR"
     FONT 8.

DEFINE BUTTON btnc  NO-FOCUS
     LABEL "C" 
     SIZE 3 BY 1.12 TOOLTIP "C"
     FONT 8.

DEFINE BUTTON btncal  NO-FOCUS FLAT-BUTTON
     LABEL "LOCAL" 
     SIZE 15 BY 1.12 TOOLTIP "Local"
     FONT 8.

DEFINE BUTTON btnCero  NO-FOCUS FLAT-BUTTON
     LABEL "0" 
     SIZE 3 BY 1.12 TOOLTIP "0"
     FONT 8.

DEFINE BUTTON btnCinco  NO-FOCUS FLAT-BUTTON
     LABEL "5" 
     SIZE 3 BY 1.12 TOOLTIP "5"
     FONT 8.

DEFINE BUTTON btnCiuddla  NO-FOCUS
     LABEL "CIUDADELA" 
     SIZE 15 BY 1.12 TOOLTIP "Ciudadela"
     FONT 8.

DEFINE BUTTON btnClla  NO-FOCUS FLAT-BUTTON
     LABEL "CELULA" 
     SIZE 15 BY 1.12 TOOLTIP "Célula"
     FONT 8.

DEFINE BUTTON btnClle  NO-FOCUS
     LABEL "CALLE" 
     SIZE 15 BY 1.12 TOOLTIP "Calle".

DEFINE BUTTON btnCmno  NO-FOCUS FLAT-BUTTON
     LABEL "CAMINO" 
     SIZE 15 BY 1.12 TOOLTIP "CAMINO"
     FONT 8.

DEFINE BUTTON btnCnjnto  NO-FOCUS
     LABEL "CONJUNTO" 
     SIZE 15 BY 1.12 TOOLTIP "Conjunto"
     FONT 8.

DEFINE BUTTON btnCnjntoRsdncial  NO-FOCUS
     LABEL "CONJUNTO RESIDENCIAL" 
     SIZE 15 BY 1.12 TOOLTIP "Conjunto Residencial"
     FONT 15.

DEFINE BUTTON btnCnsltrio  NO-FOCUS FLAT-BUTTON
     LABEL "CONSULTORIO" 
     SIZE 15 BY 1.12 TOOLTIP "Consultorio"
     FONT 8.

DEFINE BUTTON btnCntroCmrcial  NO-FOCUS FLAT-BUTTON
     LABEL "CEN COMERCIAL" 
     SIZE 15 BY 1.12 TOOLTIP "Centro Comercial"
     FONT 15.

DEFINE BUTTON btnCrclar  NO-FOCUS FLAT-BUTTON
     LABEL "CIRCULAR" 
     SIZE 15 BY 1.12 TOOLTIP "Circular"
     FONT 8.

DEFINE BUTTON btnCrcnvlar  NO-FOCUS FLAT-BUTTON
     LABEL "CIRCUNVALAR" 
     SIZE 15 BY 1.12 TOOLTIP "Circunvalar"
     FONT 8.

DEFINE BUTTON btnCrrgmiento  NO-FOCUS FLAT-BUTTON
     LABEL "CORREGIMIENTO" 
     SIZE 15 BY 1.12 TOOLTIP "Corregimiento"
     FONT 8.

DEFINE BUTTON btnCrrra  NO-FOCUS
     LABEL "CARRERA" 
     SIZE 15 BY 1.12 TOOLTIP "Carrera".

DEFINE BUTTON btnCrrtra  NO-FOCUS FLAT-BUTTON
     LABEL "CARRETERA" 
     SIZE 15 BY 1.12 TOOLTIP "CARRETERA"
     FONT 8.

DEFINE BUTTON btnCsa  NO-FOCUS
     LABEL "CASA" 
     SIZE 15 BY 1.12 TOOLTIP "CASA"
     FONT 8.

DEFINE BUTTON btnCuatro  NO-FOCUS FLAT-BUTTON
     LABEL "4" 
     SIZE 3 BY 1.12 TOOLTIP "4"
     FONT 8.

DEFINE BUTTON btnd  NO-FOCUS
     LABEL "D" 
     SIZE 3 BY 1.12 TOOLTIP "D"
     FONT 8.

DEFINE BUTTON btnDiagnal  NO-FOCUS
     LABEL "DIAGONAL" 
     SIZE 15 BY 1.12 TOOLTIP "Diagonal".

DEFINE BUTTON btnDos  NO-FOCUS FLAT-BUTTON
     LABEL "2" 
     SIZE 3 BY 1.12 TOOLTIP "2"
     FONT 8.

DEFINE BUTTON btnDprtmnto  NO-FOCUS FLAT-BUTTON
     LABEL "DEPARTAMENTO" 
     SIZE 15 BY 1.12 TOOLTIP "Departamento"
     FONT 8.

DEFINE BUTTON btnDpsto  NO-FOCUS FLAT-BUTTON
     LABEL "DEPOSITO" 
     SIZE 15 BY 1.12 TOOLTIP "Depósito"
     FONT 8.

DEFINE BUTTON btnDpstoStno  NO-FOCUS FLAT-BUTTON
     LABEL "DEPOSITO SOTANO" 
     SIZE 15 BY 1.12 TOOLTIP "DEPOSITO SOTANO"
     FONT 15.

DEFINE BUTTON btnE  NO-FOCUS
     LABEL "E" 
     SIZE 3 BY 1.12 TOOLTIP "E"
     FONT 8.

DEFINE BUTTON btnEdfcio  NO-FOCUS
     LABEL "EDIFICIO" 
     SIZE 15 BY 1.12 TOOLTIP "Edificio".

DEFINE BUTTON btnEntrda  NO-FOCUS
     LABEL "ENTRADA" 
     SIZE 15 BY 1.12 TOOLTIP "ENTRADA"
     FONT 8.

DEFINE BUTTON btnEsquina  NO-FOCUS FLAT-BUTTON
     LABEL "ESQUINA" 
     SIZE 15 BY 1.12 TOOLTIP "ESQUINA"
     FONT 8.

DEFINE BUTTON btnEste  NO-FOCUS
     LABEL "ESTE" 
     SIZE 15 BY 1.12 TOOLTIP "ESTE".

DEFINE BUTTON btnEtpa  NO-FOCUS
     LABEL "ETAPA" 
     SIZE 15 BY 1.12 TOOLTIP "ETAPA"
     FONT 8.

DEFINE BUTTON btnExtrior  NO-FOCUS FLAT-BUTTON
     LABEL "EXTERIOR" 
     SIZE 15 BY 1.12 TOOLTIP "EXTERIOR"
     FONT 8.

DEFINE BUTTON btnF  NO-FOCUS
     LABEL "F" 
     SIZE 3 BY 1.12 TOOLTIP "F"
     FONT 8.

DEFINE BUTTON btnFnca  NO-FOCUS FLAT-BUTTON
     LABEL "FINCA" 
     SIZE 15 BY 1.12 TOOLTIP "FINCA"
     FONT 8.

DEFINE BUTTON btnG  NO-FOCUS FLAT-BUTTON
     LABEL "G" 
     SIZE 3 BY 1.12 TOOLTIP "G"
     FONT 8.

DEFINE BUTTON btnGrge  NO-FOCUS FLAT-BUTTON
     LABEL "GARAGE" 
     SIZE 15 BY 1.12 TOOLTIP "GARAGE"
     FONT 8.

DEFINE BUTTON btnGuion  NO-FOCUS
     LABEL "-" 
     SIZE 3 BY 1.12 TOOLTIP "Guión"
     FONT 8.

DEFINE BUTTON btnH  NO-FOCUS FLAT-BUTTON
     LABEL "H" 
     SIZE 3 BY 1.12 TOOLTIP "H"
     FONT 8.

DEFINE BUTTON btnI  NO-FOCUS FLAT-BUTTON
     LABEL "I" 
     SIZE 3 BY 1.12 TOOLTIP "I"
     FONT 8.

DEFINE BUTTON btnIntrior  NO-FOCUS
     LABEL "INTERIOR" 
     SIZE 15 BY 1.12 TOOLTIP "INTERIOR"
     FONT 8.

DEFINE BUTTON btnJ  NO-FOCUS FLAT-BUTTON
     LABEL "J" 
     SIZE 3 BY 1.12 TOOLTIP "J"
     FONT 8.

DEFINE BUTTON btnK  NO-FOCUS FLAT-BUTTON
     LABEL "K" 
     SIZE 3 BY 1.12 TOOLTIP "K"
     FONT 8.

DEFINE BUTTON btnKlmtro  NO-FOCUS FLAT-BUTTON
     LABEL "KILOMETRO" 
     SIZE 15 BY 1.12 TOOLTIP "Kilómetro"
     FONT 8.

DEFINE BUTTON btnL  NO-FOCUS FLAT-BUTTON
     LABEL "L" 
     SIZE 3 BY 1.12 TOOLTIP "L"
     FONT 8.

DEFINE BUTTON btnLte  NO-FOCUS FLAT-BUTTON
     LABEL "LOTE" 
     SIZE 15 BY 1.12 TOOLTIP "Lote"
     FONT 8.

DEFINE BUTTON btnM  NO-FOCUS FLAT-BUTTON
     LABEL "M" 
     SIZE 3 BY 1.12 TOOLTIP "M"
     FONT 8.

DEFINE BUTTON btnMncpio  NO-FOCUS
     LABEL "MUNICIPIO" 
     SIZE 15 BY 1.12 TOOLTIP "MUNICIPIO"
     FONT 8.

DEFINE BUTTON btnMnzna  NO-FOCUS
     LABEL "MANZANA" 
     SIZE 15 BY 1.12 TOOLTIP "MANZANA"
     FONT 8.

DEFINE BUTTON btnN  NO-FOCUS FLAT-BUTTON
     LABEL "N" 
     SIZE 3 BY 1.12 TOOLTIP "N"
     FONT 8.

DEFINE BUTTON btnNmral  NO-FOCUS
     LABEL "#" 
     SIZE 3 BY 1.12 TOOLTIP "Numeral"
     FONT 8.

DEFINE BUTTON btnNrte  NO-FOCUS
     LABEL "NORTE" 
     SIZE 15 BY 1.12 TOOLTIP "Norte".

DEFINE BUTTON btnNueve  NO-FOCUS FLAT-BUTTON
     LABEL "9" 
     SIZE 3 BY 1.12 TOOLTIP "9"
     FONT 8.

DEFINE BUTTON btnO  NO-FOCUS FLAT-BUTTON
     LABEL "O" 
     SIZE 3 BY 1.12 TOOLTIP "O"
     FONT 8.

DEFINE BUTTON btnOccidnte  NO-FOCUS FLAT-BUTTON
     LABEL "OCCIDENTE" 
     SIZE 15 BY 1.12 TOOLTIP "OCCIDENTE"
     FONT 8.

DEFINE BUTTON btnOcho  NO-FOCUS FLAT-BUTTON
     LABEL "8" 
     SIZE 3 BY 1.12 TOOLTIP "8"
     FONT 8.

DEFINE BUTTON btnOeste  NO-FOCUS
     LABEL "OESTE" 
     SIZE 15 BY 1.12 TOOLTIP "OESTE".

DEFINE BUTTON btnOfcna  NO-FOCUS
     LABEL "OFICINA" 
     SIZE 15 BY 1.12 TOOLTIP "OFICINA"
     FONT 8.

DEFINE BUTTON btnOriente  NO-FOCUS FLAT-BUTTON
     LABEL "ORIENTE" 
     SIZE 15 BY 1.12 TOOLTIP "ORIENTE"
     FONT 8.

DEFINE BUTTON btnP  NO-FOCUS FLAT-BUTTON
     LABEL "P" 
     SIZE 3 BY 1.12 TOOLTIP "P"
     FONT 8.

DEFINE BUTTON btnPenthouse  NO-FOCUS
     LABEL "PENTHOUSE" 
     SIZE 15 BY 1.12 TOOLTIP "PentHouse"
     FONT 8.

DEFINE BUTTON btnPlnta  NO-FOCUS FLAT-BUTTON
     LABEL "PLANTA" 
     SIZE 15 BY 1.12 TOOLTIP "PLANTA"
     FONT 8.

DEFINE BUTTON btnPrcla  NO-FOCUS FLAT-BUTTON
     LABEL "PARCELA" 
     SIZE 15 BY 1.12 TOOLTIP "PARCELA"
     FONT 8.

DEFINE BUTTON btnPrdio  NO-FOCUS FLAT-BUTTON
     LABEL "PREDIO" 
     SIZE 15 BY 1.12 TOOLTIP "Predio"
     FONT 8.

DEFINE BUTTON btnPrque  NO-FOCUS FLAT-BUTTON
     LABEL "PARQUE" 
     SIZE 15 BY 1.12 TOOLTIP "PARQUE"
     FONT 8.

DEFINE BUTTON btnPrqueadro  NO-FOCUS FLAT-BUTTON
     LABEL "PARQUEADERO" 
     SIZE 15 BY 1.12 TOOLTIP "PARQUEADERO"
     FONT 8.

DEFINE BUTTON btnPrtria  NO-FOCUS FLAT-BUTTON
     LABEL "PORTERIA" 
     SIZE 15 BY 1.12 TOOLTIP "Portería"
     FONT 8.

DEFINE BUTTON btnPseo  NO-FOCUS
     LABEL "PASEO" 
     SIZE 15 BY 1.12 TOOLTIP "PASEO"
     FONT 8.

DEFINE BUTTON btnPsje  NO-FOCUS FLAT-BUTTON
     LABEL "PASAJE" 
     SIZE 15 BY 1.12 TOOLTIP "Pasaje"
     FONT 8.

DEFINE BUTTON btnPso  NO-FOCUS
     LABEL "PISO" 
     SIZE 15 BY 1.12 TOOLTIP "PISO"
     FONT 8.

DEFINE BUTTON btnPuente  NO-FOCUS FLAT-BUTTON
     LABEL "PUENTE" 
     SIZE 15 BY 1.12 TOOLTIP "Puente"
     FONT 8.

DEFINE BUTTON btnPuesto  NO-FOCUS FLAT-BUTTON
     LABEL "PUESTO" 
     SIZE 15 BY 1.12 TOOLTIP "PUESTO"
     FONT 8.

DEFINE BUTTON btnQ  NO-FOCUS FLAT-BUTTON
     LABEL "Q" 
     SIZE 3 BY 1.12 TOOLTIP "Q"
     FONT 8.

DEFINE BUTTON btnR  NO-FOCUS FLAT-BUTTON
     LABEL "R" 
     SIZE 3 BY 1.12 TOOLTIP "R"
     FONT 8.

DEFINE BUTTON btnS  NO-FOCUS FLAT-BUTTON
     LABEL "S" 
     SIZE 3 BY 1.12 TOOLTIP "S"
     FONT 8.

DEFINE BUTTON btnSctor  NO-FOCUS
     LABEL "SECTOR" 
     SIZE 15 BY 1.12 TOOLTIP "Sector"
     FONT 8.

DEFINE BUTTON btnSeis  NO-FOCUS FLAT-BUTTON
     LABEL "6" 
     SIZE 3 BY 1.12 TOOLTIP "6"
     FONT 8.

DEFINE BUTTON btnSiete  NO-FOCUS FLAT-BUTTON
     LABEL "7" 
     SIZE 3 BY 1.12 TOOLTIP "7"
     FONT 8.

DEFINE BUTTON btnSlar  NO-FOCUS FLAT-BUTTON
     LABEL "SOLAR" 
     SIZE 15 BY 1.12 TOOLTIP "SOLAR"
     FONT 8.

DEFINE BUTTON btnSlon  NO-FOCUS FLAT-BUTTON
     LABEL "SALON" 
     SIZE 15 BY 1.12 TOOLTIP "SALON"
     FONT 8.

DEFINE BUTTON btnSlonCmnal  NO-FOCUS FLAT-BUTTON
     LABEL "SALON COMUNAL" 
     SIZE 15 BY 1.12 TOOLTIP "Salón Comunal"
     FONT 8.

DEFINE BUTTON btnSmsctor  NO-FOCUS FLAT-BUTTON
     LABEL "SEMISECTOR" 
     SIZE 15 BY 1.12 TOOLTIP "Semisector"
     FONT 8.

DEFINE BUTTON btnSperMnzna  NO-FOCUS
     LABEL "SUPER MANZANA" 
     SIZE 15 BY 1.12 TOOLTIP "Super Manzana"
     FONT 8.

DEFINE BUTTON btnStno  NO-FOCUS FLAT-BUTTON
     LABEL "SOTANO" 
     SIZE 15 BY 1.12 TOOLTIP "SOTANO"
     FONT 8.

DEFINE BUTTON btnSuite  NO-FOCUS
     LABEL "SUITE" 
     SIZE 15 BY 1.12 TOOLTIP "SUITE"
     FONT 8.

DEFINE BUTTON btnSur  NO-FOCUS
     LABEL "SUR" 
     SIZE 15 BY 1.12 TOOLTIP "Sur".

DEFINE BUTTON btnT  NO-FOCUS FLAT-BUTTON
     LABEL "T" 
     SIZE 3 BY 1.12 TOOLTIP "T"
     FONT 8.

DEFINE BUTTON btnTres  NO-FOCUS FLAT-BUTTON
     LABEL "3" 
     SIZE 3 BY 1.12 TOOLTIP "3"
     FONT 8.

DEFINE BUTTON btnTrmnal  NO-FOCUS FLAT-BUTTON
     LABEL "TERMINAL" 
     SIZE 15 BY 1.12 TOOLTIP "Terminal"
     FONT 8.

DEFINE BUTTON btnTrnsvrsal  NO-FOCUS
     LABEL "TRANSVERSAL" 
     SIZE 15 BY 1.12 TOOLTIP "Transversal".

DEFINE BUTTON btnTrre  NO-FOCUS
     LABEL "TORRE" 
     SIZE 15 BY 1.12 TOOLTIP "Torre"
     FONT 8.

DEFINE BUTTON btnTrrza  NO-FOCUS FLAT-BUTTON
     LABEL "TERRAZA" 
     SIZE 15 BY 1.12 TOOLTIP "Terraza"
     FONT 8.

DEFINE BUTTON btnU  NO-FOCUS FLAT-BUTTON
     LABEL "U" 
     SIZE 3 BY 1.12 TOOLTIP "U"
     FONT 8.

DEFINE BUTTON btnUndad  NO-FOCUS
     LABEL "UNIDAD" 
     SIZE 15 BY 1.12 TOOLTIP "Unidad"
     FONT 8.

DEFINE BUTTON btnUnidadRsdncial  NO-FOCUS
     LABEL "UNIDAD RESIDENCIAL" 
     SIZE 15 BY 1.12 TOOLTIP "Unidad Residencial"
     FONT 15.

DEFINE BUTTON btnUno  NO-FOCUS FLAT-BUTTON
     LABEL "1" 
     SIZE 3 BY 1.12 TOOLTIP "1"
     FONT 8.

DEFINE BUTTON btnUrbnzcion  NO-FOCUS
     LABEL "URBANIZACION" 
     SIZE 15 BY 1.12 TOOLTIP "Urbanización"
     FONT 8.

DEFINE BUTTON btnV  NO-FOCUS FLAT-BUTTON
     LABEL "V" 
     SIZE 3 BY 1.12 TOOLTIP "V"
     FONT 8.

DEFINE BUTTON btnVariante  NO-FOCUS FLAT-BUTTON
     LABEL "VARIANTE" 
     SIZE 15 BY 1.12 TOOLTIP "Variante"
     FONT 8.

DEFINE BUTTON btnVrda  NO-FOCUS FLAT-BUTTON
     LABEL "VEREDA" 
     SIZE 15 BY 1.12 TOOLTIP "Vereda"
     FONT 8.

DEFINE BUTTON btnW  NO-FOCUS FLAT-BUTTON
     LABEL "W" 
     SIZE 3 BY 1.12 TOOLTIP "W"
     FONT 8.

DEFINE BUTTON btnX  NO-FOCUS FLAT-BUTTON
     LABEL "X" 
     SIZE 3 BY 1.12 TOOLTIP "X"
     FONT 8.

DEFINE BUTTON btnY  NO-FOCUS FLAT-BUTTON
     LABEL "Y" 
     SIZE 3 BY 1.12 TOOLTIP "Y"
     FONT 8.

DEFINE BUTTON btnZ  NO-FOCUS FLAT-BUTTON
     LABEL "Z" 
     SIZE 3 BY 1.12 TOOLTIP "Z"
     FONT 8.

DEFINE BUTTON btnZna  NO-FOCUS
     LABEL "ZONA" 
     SIZE 15 BY 1.12 TOOLTIP "Zona"
     FONT 8.

DEFINE BUTTON btnZnaFrnca  NO-FOCUS FLAT-BUTTON
     LABEL "ZONA FRANCA" 
     SIZE 15 BY 1.12 TOOLTIP "Zona Franca"
     FONT 8.

DEFINE BUTTON BUTTON-BLANCO 
     LABEL "" 
     SIZE 15 BY 1.12 TOOLTIP "Blanco".

DEFINE BUTTON BUTTON-limpiar 
     LABEL "C" 
     SIZE 4 BY 1.12 TOOLTIP "Limpiar".

DEFINE BUTTON BUTTON-RETURN 
     LABEL "RETORNO" 
     SIZE 14 BY 8.88 TOOLTIP "RETORNO".

DEFINE BUTTON BUTTON-rtrcso 
     LABEL "RETROCESO" 
     SIZE 14 BY 1.12 TOOLTIP "Limpiar".

DEFINE BUTTON BUTTONCE 
     LABEL "CE" 
     SIZE 4 BY 1.12 TOOLTIP "CE".

DEFINE VARIABLE cDrccion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dirección" 
     VIEW-AS FILL-IN 
     SIZE 65 BY 1 TOOLTIP "Dirección" NO-UNDO.

DEFINE VARIABLE cTxtoABrrar AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.86 BY 1.12 TOOLTIP "Texto A Borrar" NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "NOMENCLATURAS" 
      VIEW-AS TEXT 
     SIZE 18 BY .62 TOOLTIP "Nomenclaturas Más Usadas" NO-UNDO.

DEFINE RECTANGLE RECT-306
     EDGE-PIXELS 2 GRAPHIC-EDGE    ROUNDED 
     SIZE 62 BY 4.58
     BGCOLOR 17 .

DEFINE RECTANGLE RECT-307
     EDGE-PIXELS 2 GRAPHIC-EDGE    ROUNDED 
     SIZE 92 BY 14.54
     BGCOLOR 17 .

DEFINE RECTANGLE RECT-308
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 16 BY 10.5.

DEFINE RECTANGLE RECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 35 BY 4.85.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnAgncia AT ROW 7.46 COL 18 WIDGET-ID 34
     btnBis AT ROW 8.54 COL 33 WIDGET-ID 48
     btnCrrra AT ROW 4.77 COL 3 WIDGET-ID 12
     btnGuion AT ROW 5.04 COL 109 WIDGET-ID 168
     cDrccion AT ROW 1.42 COL 11 COLON-ALIGNED HELP
          "Dirección" WIDGET-ID 2
     BUTTON-BLANCO AT ROW 5.04 COL 79 WIDGET-ID 260
     BUTTON-rtrcso AT ROW 8.81 COL 95 WIDGET-ID 268
     btnNmral AT ROW 3.96 COL 109 WIDGET-ID 166
     BUTTON-RETURN AT ROW 9.88 COL 95 WIDGET-ID 262
     BUTTON-limpiar AT ROW 20.46 COL 63 WIDGET-ID 254
     btncal AT ROW 12.85 COL 48 WIDGET-ID 278
     BUTTONCE AT ROW 20.46 COL 67 WIDGET-ID 258
     cTxtoABrrar AT ROW 20.46 COL 69 COLON-ALIGNED NO-LABEL WIDGET-ID 256
     btnIntrior AT ROW 12.85 COL 33 WIDGET-ID 270
     btnKlmtro AT ROW 12.85 COL 78 WIDGET-ID 276
     btnLte AT ROW 12.85 COL 63 WIDGET-ID 274
     btnMnzna AT ROW 13.92 COL 3 WIDGET-ID 272
     btnPrque AT ROW 15 COL 3 WIDGET-ID 108
     btnL AT ROW 2.88 COL 79 WIDGET-ID 224
     btnR AT ROW 2.88 COL 97 WIDGET-ID 236
     btnS AT ROW 2.88 COL 100 WIDGET-ID 238
     btna AT ROW 1.81 COL 79 WIDGET-ID 200
     btnAprtdo AT ROW 7.46 COL 78 WIDGET-ID 42
     btnAprtmnto AT ROW 3.69 COL 3 WIDGET-ID 4
     btnAutpsta AT ROW 3.69 COL 33 WIDGET-ID 8
     btnAvnda AT ROW 3.69 COL 18 WIDGET-ID 6
     btnAvndaCrrra AT ROW 8.54 COL 3 WIDGET-ID 44
     btnb AT ROW 1.81 COL 82 WIDGET-ID 204
     btnBlque AT ROW 8.54 COL 18 WIDGET-ID 46
     btnc AT ROW 1.81 COL 85 WIDGET-ID 206
     btnCiuddla AT ROW 10.69 COL 3 WIDGET-ID 70
     btnClle AT ROW 3.69 COL 48 WIDGET-ID 10
     btnCnjnto AT ROW 10.69 COL 18 WIDGET-ID 72
     btnCnjntoRsdncial AT ROW 10.69 COL 33 WIDGET-ID 74
     btnCsa AT ROW 9.62 COL 18 WIDGET-ID 58
     btnd AT ROW 1.81 COL 88 WIDGET-ID 208
     btnDiagnal AT ROW 4.77 COL 18 WIDGET-ID 14
     btnE AT ROW 1.81 COL 91 WIDGET-ID 210
     btnEdfcio AT ROW 4.77 COL 33 WIDGET-ID 16
     btnEntrda AT ROW 11.77 COL 33 WIDGET-ID 86
     btnEste AT ROW 4.77 COL 48 WIDGET-ID 24
     btnEtpa AT ROW 11.77 COL 63 WIDGET-ID 90
     btnF AT ROW 1.81 COL 94 WIDGET-ID 212
     btnMncpio AT ROW 13.92 COL 18 WIDGET-ID 98
     btnNrte AT ROW 5.85 COL 3 WIDGET-ID 18
     btnOeste AT ROW 5.85 COL 18 WIDGET-ID 26
     btnOfcna AT ROW 13.92 COL 48 WIDGET-ID 102
     btnPenthouse AT ROW 15 COL 63 WIDGET-ID 116
     btnPseo AT ROW 15 COL 48 WIDGET-ID 114
     btnPso AT ROW 15 COL 78 WIDGET-ID 118
     btnSctor AT ROW 17.15 COL 18 WIDGET-ID 132
     btnSperMnzna AT ROW 18.23 COL 3 WIDGET-ID 148
     btnSuite AT ROW 17.15 COL 78 WIDGET-ID 146
     btnSur AT ROW 5.85 COL 33 WIDGET-ID 20
     btnTrnsvrsal AT ROW 5.85 COL 48 WIDGET-ID 22
     btnTrre AT ROW 18.23 COL 48 WIDGET-ID 144
     btnUndad AT ROW 18.23 COL 63 WIDGET-ID 154
     btnUnidadRsdncial AT ROW 18.23 COL 78 WIDGET-ID 156
     btnUrbnzcion AT ROW 19.31 COL 3 WIDGET-ID 152
     btnZna AT ROW 19.31 COL 48 WIDGET-ID 160
     btnCero AT ROW 5.04 COL 106 WIDGET-ID 196
     btnCinco AT ROW 3.96 COL 106 WIDGET-ID 186
     btnCuatro AT ROW 3.96 COL 103 WIDGET-ID 184
     btnDos AT ROW 3.96 COL 97 WIDGET-ID 180
     btnG AT ROW 1.81 COL 97 WIDGET-ID 214
     btnH AT ROW 1.81 COL 100 WIDGET-ID 216
     btnI AT ROW 1.81 COL 103 WIDGET-ID 218
     btnJ AT ROW 1.81 COL 106 WIDGET-ID 220
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112 BY 20.73 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     btnK AT ROW 1.81 COL 109 WIDGET-ID 222
     btnM AT ROW 2.88 COL 82 WIDGET-ID 226
     btnN AT ROW 2.88 COL 85 WIDGET-ID 228
     btnNueve AT ROW 5.04 COL 103 WIDGET-ID 194
     btnO AT ROW 2.88 COL 88 WIDGET-ID 230
     btnOcho AT ROW 5.04 COL 100 WIDGET-ID 192
     btnP AT ROW 2.88 COL 91 WIDGET-ID 232
     btnQ AT ROW 2.88 COL 94 WIDGET-ID 234
     btnSeis AT ROW 5.04 COL 94 WIDGET-ID 188
     btnSiete AT ROW 5.04 COL 97 WIDGET-ID 190
     btnT AT ROW 2.88 COL 103 WIDGET-ID 240
     btnTres AT ROW 3.96 COL 100 WIDGET-ID 182
     btnU AT ROW 2.88 COL 106 WIDGET-ID 242
     btnUno AT ROW 3.96 COL 94 WIDGET-ID 178
     btnV AT ROW 2.88 COL 109 WIDGET-ID 244
     btnW AT ROW 3.96 COL 79 WIDGET-ID 246
     btnX AT ROW 3.96 COL 82 WIDGET-ID 248
     btnY AT ROW 3.96 COL 85 WIDGET-ID 250
     btnZ AT ROW 3.96 COL 88 WIDGET-ID 252
     btnCntroCmrcial AT ROW 9.62 COL 48 WIDGET-ID 62
     btnDpstoStno AT ROW 11.77 COL 18 WIDGET-ID 84
     btnAdmnstrcion AT ROW 7.46 COL 3 WIDGET-ID 32
     btnAgrpcion AT ROW 7.46 COL 33 WIDGET-ID 36
     btnAlmcen AT ROW 7.46 COL 48 WIDGET-ID 38
     btnAltllo AT ROW 7.46 COL 63 WIDGET-ID 40
     btnBdga AT ROW 8.54 COL 48 WIDGET-ID 50
     btnBlvar AT ROW 8.54 COL 63 WIDGET-ID 52
     btnClla AT ROW 9.62 COL 33 WIDGET-ID 60
     btnCmno AT ROW 8.54 COL 78 WIDGET-ID 54
     btnCnsltrio AT ROW 10.69 COL 48 WIDGET-ID 76
     btnCrclar AT ROW 9.62 COL 63 WIDGET-ID 64
     btnCrcnvlar AT ROW 9.62 COL 78 WIDGET-ID 66
     btnCrrgmiento AT ROW 10.69 COL 63 WIDGET-ID 78
     btnCrrtra AT ROW 9.62 COL 3 WIDGET-ID 56
     btnDprtmnto AT ROW 10.69 COL 78 WIDGET-ID 80
     btnDpsto AT ROW 11.77 COL 3 WIDGET-ID 82
     btnEsquina AT ROW 11.77 COL 48 WIDGET-ID 88
     btnExtrior AT ROW 11.77 COL 78 WIDGET-ID 92
     btnFnca AT ROW 12.85 COL 3 WIDGET-ID 94
     btnGrge AT ROW 12.85 COL 18 WIDGET-ID 96
     btnOccidnte AT ROW 13.92 COL 33 WIDGET-ID 100
     btnOriente AT ROW 13.92 COL 63 WIDGET-ID 104
     btnPlnta AT ROW 16.08 COL 3 WIDGET-ID 120
     btnPrcla AT ROW 13.92 COL 78 WIDGET-ID 106
     btnPrdio AT ROW 16.08 COL 33 WIDGET-ID 126
     btnPrqueadro AT ROW 15 COL 18 WIDGET-ID 110
     btnPrtria AT ROW 16.08 COL 18 WIDGET-ID 128
     btnPsje AT ROW 15 COL 33 WIDGET-ID 112
     btnPuente AT ROW 16.08 COL 48 WIDGET-ID 122
     btnPuesto AT ROW 16.08 COL 63 WIDGET-ID 130
     btnSlar AT ROW 17.15 COL 48 WIDGET-ID 138
     btnSlon AT ROW 16.08 COL 78 WIDGET-ID 124
     btnSlonCmnal AT ROW 17.15 COL 3 WIDGET-ID 134
     btnSmsctor AT ROW 17.15 COL 33 WIDGET-ID 136
     btnStno AT ROW 17.15 COL 63 WIDGET-ID 140
     btnTrmnal AT ROW 18.23 COL 18 WIDGET-ID 142
     btnTrrza AT ROW 18.23 COL 33 WIDGET-ID 150
     btnVariante AT ROW 19.31 COL 18 WIDGET-ID 158
     btnVrda AT ROW 19.31 COL 33 WIDGET-ID 162
     btnZnaFrnca AT ROW 19.31 COL 63 WIDGET-ID 164
     FILL-IN-2 AT ROW 2.88 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     RECT-306 AT ROW 2.62 COL 2 WIDGET-ID 30
     RECT-307 AT ROW 7.19 COL 2 WIDGET-ID 176
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112 BY 20.73 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     RECT-308 AT ROW 8.54 COL 94 WIDGET-ID 198
     RECT-309 AT ROW 1.54 COL 78 WIDGET-ID 202
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112 BY 20.73 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Generador De Direcciones"
         HEIGHT             = 20.73
         WIDTH              = 112
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
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
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
ASSIGN 
       btna:PRIVATE-DATA IN FRAME fMain     = 
                "A".

ASSIGN 
       btnAdmnstrcion:PRIVATE-DATA IN FRAME fMain     = 
                "ADMON".

ASSIGN 
       btnAgncia:PRIVATE-DATA IN FRAME fMain     = 
                "AGENCIA".

ASSIGN 
       btnAgrpcion:PRIVATE-DATA IN FRAME fMain     = 
                "AG".

ASSIGN 
       btnAlmcen:PRIVATE-DATA IN FRAME fMain     = 
                "ALMACEN".

ASSIGN 
       btnAltllo:PRIVATE-DATA IN FRAME fMain     = 
                "ALTILLO".

ASSIGN 
       btnAprtdo:PRIVATE-DATA IN FRAME fMain     = 
                "APARTADO".

ASSIGN 
       btnAprtmnto:PRIVATE-DATA IN FRAME fMain     = 
                "AP".

ASSIGN 
       btnAutpsta:PRIVATE-DATA IN FRAME fMain     = 
                "AUTOP".

ASSIGN 
       btnAvnda:PRIVATE-DATA IN FRAME fMain     = 
                "AV".

ASSIGN 
       btnAvndaCrrra:PRIVATE-DATA IN FRAME fMain     = 
                "AK".

ASSIGN 
       btnb:PRIVATE-DATA IN FRAME fMain     = 
                "B".

ASSIGN 
       btnBdga:PRIVATE-DATA IN FRAME fMain     = 
                "BG".

ASSIGN 
       btnBis:PRIVATE-DATA IN FRAME fMain     = 
                "BIS".

ASSIGN 
       btnBlque:PRIVATE-DATA IN FRAME fMain     = 
                "BL".

ASSIGN 
       btnBlvar:PRIVATE-DATA IN FRAME fMain     = 
                "BULEVAR".

ASSIGN 
       btnc:PRIVATE-DATA IN FRAME fMain     = 
                "C".

ASSIGN 
       btncal:PRIVATE-DATA IN FRAME fMain     = 
                "LOCAL".

ASSIGN 
       btnCero:PRIVATE-DATA IN FRAME fMain     = 
                "0".

ASSIGN 
       btnCinco:PRIVATE-DATA IN FRAME fMain     = 
                "5".

ASSIGN 
       btnCiuddla:PRIVATE-DATA IN FRAME fMain     = 
                "CIUDADELA".

ASSIGN 
       btnClla:PRIVATE-DATA IN FRAME fMain     = 
                "CEL".

ASSIGN 
       btnClle:PRIVATE-DATA IN FRAME fMain     = 
                "CL".

ASSIGN 
       btnCmno:PRIVATE-DATA IN FRAME fMain     = 
                "CN".

ASSIGN 
       btnCnjnto:PRIVATE-DATA IN FRAME fMain     = 
                "CONJ".

ASSIGN 
       btnCnjntoRsdncial:PRIVATE-DATA IN FRAME fMain     = 
                "CONJ RESIDENCIAL".

ASSIGN 
       btnCnsltrio:PRIVATE-DATA IN FRAME fMain     = 
                "CS".

ASSIGN 
       btnCntroCmrcial:PRIVATE-DATA IN FRAME fMain     = 
                "CC".

ASSIGN 
       btnCrclar:PRIVATE-DATA IN FRAME fMain     = 
                "CIRCULAR".

ASSIGN 
       btnCrcnvlar:PRIVATE-DATA IN FRAME fMain     = 
                "CIRCUNVALAR".

ASSIGN 
       btnCrrgmiento:PRIVATE-DATA IN FRAME fMain     = 
                "CORREG".

ASSIGN 
       btnCrrra:PRIVATE-DATA IN FRAME fMain     = 
                "CR".

ASSIGN 
       btnCrrtra:PRIVATE-DATA IN FRAME fMain     = 
                "CT".

ASSIGN 
       btnCsa:PRIVATE-DATA IN FRAME fMain     = 
                "CA".

ASSIGN 
       btnCuatro:PRIVATE-DATA IN FRAME fMain     = 
                "4".

ASSIGN 
       btnd:PRIVATE-DATA IN FRAME fMain     = 
                "D".

ASSIGN 
       btnDiagnal:PRIVATE-DATA IN FRAME fMain     = 
                "DG".

ASSIGN 
       btnDos:PRIVATE-DATA IN FRAME fMain     = 
                "2".

ASSIGN 
       btnDprtmnto:PRIVATE-DATA IN FRAME fMain     = 
                "DPTO".

ASSIGN 
       btnDpsto:PRIVATE-DATA IN FRAME fMain     = 
                "DP".

ASSIGN 
       btnDpstoStno:PRIVATE-DATA IN FRAME fMain     = 
                "DP SOTANO".

ASSIGN 
       btnE:PRIVATE-DATA IN FRAME fMain     = 
                "E".

ASSIGN 
       btnEdfcio:PRIVATE-DATA IN FRAME fMain     = 
                "ED".

ASSIGN 
       btnEntrda:PRIVATE-DATA IN FRAME fMain     = 
                "EN".

ASSIGN 
       btnEsquina:PRIVATE-DATA IN FRAME fMain     = 
                "ESQ".

ASSIGN 
       btnEste:PRIVATE-DATA IN FRAME fMain     = 
                "ES".

ASSIGN 
       btnEtpa:PRIVATE-DATA IN FRAME fMain     = 
                "ET".

ASSIGN 
       btnExtrior:PRIVATE-DATA IN FRAME fMain     = 
                "EXT".

ASSIGN 
       btnF:PRIVATE-DATA IN FRAME fMain     = 
                "F".

ASSIGN 
       btnFnca:PRIVATE-DATA IN FRAME fMain     = 
                "FINCA".

ASSIGN 
       btnG:PRIVATE-DATA IN FRAME fMain     = 
                "G".

ASSIGN 
       btnGrge:PRIVATE-DATA IN FRAME fMain     = 
                "GJ".

/* SETTINGS FOR BUTTON btnGuion IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       btnGuion:PRIVATE-DATA IN FRAME fMain     = 
                "-".

ASSIGN 
       btnH:PRIVATE-DATA IN FRAME fMain     = 
                "H".

ASSIGN 
       btnI:PRIVATE-DATA IN FRAME fMain     = 
                "I".

ASSIGN 
       btnIntrior:PRIVATE-DATA IN FRAME fMain     = 
                "IN".

ASSIGN 
       btnJ:PRIVATE-DATA IN FRAME fMain     = 
                "J".

ASSIGN 
       btnK:PRIVATE-DATA IN FRAME fMain     = 
                "K".

ASSIGN 
       btnKlmtro:PRIVATE-DATA IN FRAME fMain     = 
                "KM".

ASSIGN 
       btnL:PRIVATE-DATA IN FRAME fMain     = 
                "L".

ASSIGN 
       btnLte:PRIVATE-DATA IN FRAME fMain     = 
                "LOTE".

ASSIGN 
       btnM:PRIVATE-DATA IN FRAME fMain     = 
                "M".

ASSIGN 
       btnMncpio:PRIVATE-DATA IN FRAME fMain     = 
                "MUNICIPIO".

ASSIGN 
       btnMnzna:PRIVATE-DATA IN FRAME fMain     = 
                "MANZANA".

ASSIGN 
       btnN:PRIVATE-DATA IN FRAME fMain     = 
                "N".

/* SETTINGS FOR BUTTON btnNmral IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       btnNmral:PRIVATE-DATA IN FRAME fMain     = 
                "#".

ASSIGN 
       btnNrte:PRIVATE-DATA IN FRAME fMain     = 
                "N".

ASSIGN 
       btnNueve:PRIVATE-DATA IN FRAME fMain     = 
                "9".

ASSIGN 
       btnO:PRIVATE-DATA IN FRAME fMain     = 
                "O".

ASSIGN 
       btnOccidnte:PRIVATE-DATA IN FRAME fMain     = 
                "OCC".

ASSIGN 
       btnOcho:PRIVATE-DATA IN FRAME fMain     = 
                "8".

ASSIGN 
       btnOeste:PRIVATE-DATA IN FRAME fMain     = 
                "OE".

ASSIGN 
       btnOfcna:PRIVATE-DATA IN FRAME fMain     = 
                "OF".

ASSIGN 
       btnOriente:PRIVATE-DATA IN FRAME fMain     = 
                "ORIENTE".

ASSIGN 
       btnP:PRIVATE-DATA IN FRAME fMain     = 
                "P".

ASSIGN 
       btnPenthouse:PRIVATE-DATA IN FRAME fMain     = 
                "PH".

ASSIGN 
       btnPlnta:PRIVATE-DATA IN FRAME fMain     = 
                "PLANTA".

ASSIGN 
       btnPrcla:PRIVATE-DATA IN FRAME fMain     = 
                "PA".

ASSIGN 
       btnPrdio:PRIVATE-DATA IN FRAME fMain     = 
                "PD".

ASSIGN 
       btnPrque:PRIVATE-DATA IN FRAME fMain     = 
                "PQ".

ASSIGN 
       btnPrqueadro:PRIVATE-DATA IN FRAME fMain     = 
                "PARQUEADERO".

ASSIGN 
       btnPrtria:PRIVATE-DATA IN FRAME fMain     = 
                "PORTERIA".

ASSIGN 
       btnPseo:PRIVATE-DATA IN FRAME fMain     = 
                "PO".

ASSIGN 
       btnPsje:PRIVATE-DATA IN FRAME fMain     = 
                "PJ".

ASSIGN 
       btnPso:PRIVATE-DATA IN FRAME fMain     = 
                "PI".

ASSIGN 
       btnPuente:PRIVATE-DATA IN FRAME fMain     = 
                "PN".

ASSIGN 
       btnPuesto:PRIVATE-DATA IN FRAME fMain     = 
                "PT".

ASSIGN 
       btnQ:PRIVATE-DATA IN FRAME fMain     = 
                "Q".

ASSIGN 
       btnR:PRIVATE-DATA IN FRAME fMain     = 
                "R".

ASSIGN 
       btnS:PRIVATE-DATA IN FRAME fMain     = 
                "S".

ASSIGN 
       btnSctor:PRIVATE-DATA IN FRAME fMain     = 
                "SR".

ASSIGN 
       btnSeis:PRIVATE-DATA IN FRAME fMain     = 
                "6".

ASSIGN 
       btnSiete:PRIVATE-DATA IN FRAME fMain     = 
                "7".

ASSIGN 
       btnSlar:PRIVATE-DATA IN FRAME fMain     = 
                "SL".

ASSIGN 
       btnSlon:PRIVATE-DATA IN FRAME fMain     = 
                "SALON".

ASSIGN 
       btnSlonCmnal:PRIVATE-DATA IN FRAME fMain     = 
                "SC".

ASSIGN 
       btnSmsctor:PRIVATE-DATA IN FRAME fMain     = 
                "SEMISECTOR".

ASSIGN 
       btnSperMnzna:PRIVATE-DATA IN FRAME fMain     = 
                "SM".

ASSIGN 
       btnStno:PRIVATE-DATA IN FRAME fMain     = 
                "SOTANO".

ASSIGN 
       btnSuite:PRIVATE-DATA IN FRAME fMain     = 
                "SUITE".

ASSIGN 
       btnSur:PRIVATE-DATA IN FRAME fMain     = 
                "SUR".

ASSIGN 
       btnT:PRIVATE-DATA IN FRAME fMain     = 
                "T".

ASSIGN 
       btnTres:PRIVATE-DATA IN FRAME fMain     = 
                "3".

ASSIGN 
       btnTrmnal:PRIVATE-DATA IN FRAME fMain     = 
                "TERMINAL".

ASSIGN 
       btnTrnsvrsal:PRIVATE-DATA IN FRAME fMain     = 
                "TV".

ASSIGN 
       btnTrre:PRIVATE-DATA IN FRAME fMain     = 
                "TO".

ASSIGN 
       btnTrrza:PRIVATE-DATA IN FRAME fMain     = 
                "TERRAZA".

ASSIGN 
       btnU:PRIVATE-DATA IN FRAME fMain     = 
                "U".

ASSIGN 
       btnUndad:PRIVATE-DATA IN FRAME fMain     = 
                "UN".

ASSIGN 
       btnUnidadRsdncial:PRIVATE-DATA IN FRAME fMain     = 
                "UR".

ASSIGN 
       btnUno:PRIVATE-DATA IN FRAME fMain     = 
                "1".

ASSIGN 
       btnUrbnzcion:PRIVATE-DATA IN FRAME fMain     = 
                "URB".

ASSIGN 
       btnV:PRIVATE-DATA IN FRAME fMain     = 
                "V".

ASSIGN 
       btnVariante:PRIVATE-DATA IN FRAME fMain     = 
                "VARIANTE".

ASSIGN 
       btnVrda:PRIVATE-DATA IN FRAME fMain     = 
                "VDA".

ASSIGN 
       btnW:PRIVATE-DATA IN FRAME fMain     = 
                "W".

ASSIGN 
       btnX:PRIVATE-DATA IN FRAME fMain     = 
                "X".

ASSIGN 
       btnY:PRIVATE-DATA IN FRAME fMain     = 
                "Y".

ASSIGN 
       btnZ:PRIVATE-DATA IN FRAME fMain     = 
                "Z".

ASSIGN 
       btnZna:PRIVATE-DATA IN FRAME fMain     = 
                "ZN".

ASSIGN 
       btnZnaFrnca:PRIVATE-DATA IN FRAME fMain     = 
                "ZN FRANCA".

/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-2:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Generador De Direcciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Generador De Direcciones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-BLANCO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-BLANCO wWin
ON CHOOSE OF BUTTON-BLANCO IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        IF LENGTH(trim(cdrccion:SCREEN-VALUE)) = 0 THEN RETURN NO-APPLY.
        IF NOT SUBSTRING(cdrccion:SCREEN-VALUE,LENGTH(cdrccion:SCREEN-VALUE),1) = "_"
        THEN cdrccion:SCREEN-VALUE = cdrccion:SCREEN-VALUE + "_".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-limpiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-limpiar wWin
ON CHOOSE OF BUTTON-limpiar IN FRAME fMain /* C */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        cdrccion:SCREEN-VALUE = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-RETURN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-RETURN wWin
ON CHOOSE OF BUTTON-RETURN IN FRAME fMain /* RETORNO */
DO:
    DEF VAR C AS CHAR NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        c = cDrccion:SCREEN-VALUE.
        DO WHILE INDEX(c,"_") <> 0:
            c = REPLACE(c,"_"," ").
        END.
        c = TRIM(c).
    END.
    PUBLISH "DrccionCmbiada"(c).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-rtrcso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-rtrcso wWin
ON CHOOSE OF BUTTON-rtrcso IN FRAME fMain /* RETROCESO */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        IF LENGTH(trim(cdrccion:SCREEN-VALUE)) < 2 THEN RETURN NO-APPLY.
        cDrccion = SUBSTRING(cdrccion:SCREEN-VALUE,1,LENGTH(cdrccion:SCREEN-VALUE) - 1).
        cdrccion:SCREEN-VALUE = cDrccion.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTONCE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTONCE wWin
ON CHOOSE OF BUTTONCE IN FRAME fMain /* CE */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        cdrccion:SCREEN-VALUE = REPLACE(cdrccion:SCREEN-VALUE,cTxtoABrrar:SCREEN-VALUE,"").
        APPLY "u1" TO FRAME fmain.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cDrccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDrccion wWin
ON ANY-KEY OF cDrccion IN FRAME fMain /* Dirección */
DO:
    DEF VAR c AS CHAR NO-UNDO.
    c = "127,8".
    IF CAN-DO(c,STRING(LASTKEY)) 
    THEN RETURN NO-APPLY.
    ELSE APPLY LASTKEY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDrccion wWin
ON ANY-PRINTABLE OF cDrccion IN FRAME fMain /* Dirección */
DO:
    RETURN NO-APPLY.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}
ON 'choose':U OF btna ,btnAdmnstrcion ,btnAgncia ,btnAgrpcion ,btnAlmcen ,btnAltllo ,btnAprtdo ,btnAprtmnto ,btnAutpsta ,btnAvnda ,btnAvndaCrrra ,btnb ,btnBdga ,btnBis ,btnBlque ,btnBlvar ,btnc ,btnCero ,btnCinco ,btnCiuddla ,btnClla ,btnClle ,btnCmno ,btnCnjnto ,btnCnjntoRsdncial ,btnCnsltrio ,btnCntroCmrcial ,btnCrclar ,btnCrcnvlar ,btnCrrgmiento ,btnCrrra ,btnCrrtra ,btnCsa ,btnCuatro ,btnd ,btnDiagnal ,btnDos ,btnDprtmnto ,btnDpsto ,btnDpstoStno ,btnE ,btnEdfcio ,btnEntrda ,btnEsquina ,btnEste ,btnEtpa ,btnExtrior ,btnF ,btnFnca ,btnG ,btnGrge ,btnGuion ,btnH ,btnI ,btnJ ,btnK ,btnL ,btnM ,btnMncpio ,btnN ,btnNmral ,btnNrte ,btnNueve ,btnO ,btnOccidnte ,btnOcho ,btnOeste ,btnOfcna ,btnOriente ,btnP ,btnPenthouse ,btnPlnta ,btnPrcla ,btnPrdio ,btnPrque ,btnPrqueadro ,btnPrtria ,btnPseo ,btnPsje ,btnPso ,btnPuente ,btnPuesto ,btnQ ,btnR ,btnS ,btnSctor ,btnSeis ,btnSiete ,btnSlar ,btnSlon ,btnSlonCmnal ,btnSmsctor ,btnSperMnzna ,btnStno ,btnSuite ,btnSur ,btnT ,btnTres ,btnTrmnal ,btnTrnsvrsal ,btnTrre ,btnTrrza ,btnU ,btnUndad ,btnUnidadRsdncial ,btnUno ,btnUrbnzcion ,btnV ,btnVariante ,btnVrda ,btnW ,btnX ,btnY ,btnZ ,btnZna ,btnZnaFrnca,btnIntrior IN FRAME {&FRAME-NAME},
    btnMnzna,btnLte,btnKlmtro,btncal
DO:
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT length(SELF:LABEL) = 1 
        THEN
        IF LOOKUP(SELF:PRIVATE-DATA,cDrccion:SCREEN-VALUE,"_") <> 0 THEN RETURN NO-APPLY.
        cDrccion:SCREEN-VALUE = 
            trim(cDrccion:SCREEN-VALUE +  
                 (IF SELF:PRIVATE-DATA = ? THEN SELF:LABEL ELSE SELF:PRIVATE-DATA)) +
            (IF NOT LENGTH(SELF:LABEL) = 1 THEN "_" ELSE "").
        APPLY "u1" TO FRAME fmain.
    END.
END.
ON 'right-mouse-click':U OF btna ,btnAdmnstrcion ,btnAgncia ,btnAgrpcion ,btnAlmcen ,btnAltllo ,btnAprtdo ,btnAprtmnto ,btnAutpsta ,btnAvnda ,btnAvndaCrrra ,btnb ,btnBdga ,btnBis ,btnBlque ,btnBlvar ,btnc ,btnCero ,btnCinco ,btnCiuddla ,btnClla ,btnClle ,btnCmno ,btnCnjnto ,btnCnjntoRsdncial ,btnCnsltrio ,btnCntroCmrcial ,btnCrclar ,btnCrcnvlar ,btnCrrgmiento ,btnCrrra ,btnCrrtra ,btnCsa ,btnCuatro ,btnd ,btnDiagnal ,btnDos ,btnDprtmnto ,btnDpsto ,btnDpstoStno ,btnE ,btnEdfcio ,btnEntrda ,btnEsquina ,btnEste ,btnEtpa ,btnExtrior ,btnF ,btnFnca ,btnG ,btnGrge ,btnGuion ,btnH ,btnI ,btnJ ,btnK ,btnL ,btnM ,btnMncpio ,btnN ,btnNmral ,btnNrte ,btnNueve ,btnO ,btnOccidnte ,btnOcho ,btnOeste ,btnOfcna ,btnOriente ,btnP ,btnPenthouse ,btnPlnta ,btnPrcla ,btnPrdio ,btnPrque ,btnPrqueadro ,btnPrtria ,btnPseo ,btnPsje ,btnPso ,btnPuente ,btnPuesto ,btnQ ,btnR ,btnS ,btnSctor ,btnSeis ,btnSiete ,btnSlar ,btnSlon ,btnSlonCmnal ,btnSmsctor ,btnSperMnzna ,btnStno ,btnSuite ,btnSur ,btnT ,btnTres ,btnTrmnal ,btnTrnsvrsal ,btnTrre ,btnTrrza ,btnU ,btnUndad ,btnUnidadRsdncial ,btnUno ,btnUrbnzcion ,btnV ,btnVariante ,btnVrda ,btnW ,btnX ,btnY ,btnZ ,btnZna ,btnZnaFrnca,btnIntrior IN FRAME {&FRAME-NAME},
    btnMnzna,btnLte,btnKlmtro,btncal
DO:
    DO WITH FRAME {&FRAME-NAME}:
        cDrccion:SCREEN-VALUE = trim(replace(cDrccion:SCREEN-VALUE,SELF:PRIVATE-DATA,"")).
        cDrccion:SCREEN-VALUE = trim(replace(cDrccion:SCREEN-VALUE,"  "," ")).
        APPLY "u1" TO FRAME fmain.
    END.
END.

ON U1 OF FRAME fMain
DO:
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR c1 AS CHAR NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        c = cDrccion:SCREEN-VALUE.
        c1 = IF length(c) > 1 THEN SUBSTRING(c,LENGTH(c)) ELSE c1.
        c = IF length(c) > 1 THEN SUBSTRING(c,1,LENGTH(c) - 1) ELSE c.
        DO WHILE INDEX(c,"__") <> 0:
            c = REPLACE(c,"__","_").
        END.
        cDrccion:SCREEN-VALUE = replace(c + c1,"__","_").
    END.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CmbiarDrccion wWin 
PROCEDURE CmbiarDrccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        cDrccion:SCREEN-VALUE = c.
    END.
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
  DISPLAY cDrccion cTxtoABrrar FILL-IN-2 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE btnAgncia btnBis btnCrrra RECT-306 RECT-307 RECT-308 RECT-309 cDrccion 
         BUTTON-BLANCO BUTTON-rtrcso BUTTON-RETURN BUTTON-limpiar btncal 
         BUTTONCE cTxtoABrrar btnIntrior btnKlmtro btnLte btnMnzna btnPrque 
         btnL btnR btnS btna btnAprtdo btnAprtmnto btnAutpsta btnAvnda 
         btnAvndaCrrra btnb btnBlque btnc btnCiuddla btnClle btnCnjnto 
         btnCnjntoRsdncial btnCsa btnd btnDiagnal btnE btnEdfcio btnEntrda 
         btnEste btnEtpa btnF btnMncpio btnNrte btnOeste btnOfcna btnPenthouse 
         btnPseo btnPso btnSctor btnSperMnzna btnSuite btnSur btnTrnsvrsal 
         btnTrre btnUndad btnUnidadRsdncial btnUrbnzcion btnZna btnCero 
         btnCinco btnCuatro btnDos btnG btnH btnI btnJ btnK btnM btnN btnNueve 
         btnO btnOcho btnP btnQ btnSeis btnSiete btnT btnTres btnU btnUno btnV 
         btnW btnX btnY btnZ btnCntroCmrcial btnDpstoStno btnAdmnstrcion 
         btnAgrpcion btnAlmcen btnAltllo btnBdga btnBlvar btnClla btnCmno 
         btnCnsltrio btnCrclar btnCrcnvlar btnCrrgmiento btnCrrtra btnDprtmnto 
         btnDpsto btnEsquina btnExtrior btnFnca btnGrge btnOccidnte btnOriente 
         btnPlnta btnPrcla btnPrdio btnPrqueadro btnPrtria btnPsje btnPuente 
         btnPuesto btnSlar btnSlon btnSlonCmnal btnSmsctor btnStno btnTrmnal 
         btnTrrza btnVariante btnVrda btnZnaFrnca 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
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
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

