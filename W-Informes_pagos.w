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
  DEFINE VARIABLE FecIni   AS DATE.
  DEFINE VARIABLE FecFin   AS DATE.
  DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
  DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".
  DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE ProIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE ProFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE TpdIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE TpdFin   AS INTEGER FORMAT "999" INITIAL 9.
  
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
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR W_TipPdt AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_Vigen  AS INTEGER FORMAT "9999".
DEFINE VAR W_ForPag AS CHARACTER FORMAT "X(10)".

    DEFINE VARIABLE W_Val          AS LOGICAL.
    DEFINE VARIABLE W_Codbase      AS CHARACTER FORMAT "X(4)".
    DEFINE VARIABLE W_Pcuenta    LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_Pnombre    LIKE Cuentas.Nombre.
    DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
    DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
    DEFINE VARIABLE W_Consulta     AS LOGICAL INITIAL FALSE.
    DEFINE VARIABLE W_ConsCta    LIKE Cuentas.Cuenta.
    DEFINE VARIABLE W_DispCta    LIKE Cuentas.Cuenta.
    
  DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.

DEFINE TEMP-TABLE TR
    FIELD TAge LIKE Agencias.Agencia
    FIELD TCue LIKE Cuentas.Cuenta
    FIELD TNCt LIKE Cuentas.Nombre
    FIELD TPor LIKE Base_Ret.Porcentaje
    FIELD TNit LIKE Clientes.Nit
    FIELD TNom AS CHARACTER FORMAT "X(30)"
    FIELD TBDb LIKE Mov_Contable.Db
    FIELD TRDb LIKE Mov_Contable.Db
    FIELD TBCr LIKE Mov_Contable.Db
    FIELD TRCr LIKE Mov_Contable.Db
    FIELD TTot LIKE Mov_Contable.Db.

DEFINE TEMP-TABLE IT
    FIELD I_Cue LIKE Cuentas.Cuenta
    FIELD I_NCt LIKE Cuentas.Nombre
    FIELD I_Tot AS DECIMAL FORMAT "->>>,>>>,>>9"  EXTENT 11
    FIELD I_Pag AS DECIMAL FORMAT "->>>,>>>,>>9"
    FIELD I_Aju AS DECIMAL FORMAT "->>>,>>>,>>9"
    FIELD I_Bas AS DECIMAL FORMAT "->>>,>>>,>>9"
    FIELD I_AjT AS CHARACTER FORMAT "X(15)".

DEFINE VAR TT1  AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT2  AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT3  AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT4  AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT5  AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT6  AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT7  AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT8  AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT9  AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT10 AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT11 AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT12 AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT13 AS DECIMAL FORMAT  "->>>,>>>,>>9".
DEFINE VAR TT14 AS DECIMAL FORMAT  "->>>,>>>,>>9".

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
&Scoped-Define ENABLED-OBJECTS Cmb_Tipo BUTTON-143 BUTTON-153 BUTTON-144 ~
BUTTON-149 E1 btn_ejecutar BUTTON-150 BUTTON-152 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Tipo E1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_ejecutar 
     LABEL "Ejecutar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-144 
     LABEL "Cambiar los Rangos de Filtro de Información" 
     SIZE 40 BY 1.12.

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

DEFINE VARIABLE Cmb_Tipo AS CHARACTER FORMAT "X(256)":U INITIAL "Por Agencia/Cuenta/Nit" 
     LABEL "Tipo de Informe" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Por Agencia/Cuenta/Nit","Total por Entidad/Cuentas" 
     DROP-DOWN-LIST
     SIZE 35 BY 1
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

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 4 BY .54.

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

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CueFin AS CHARACTER FORMAT "X(14)":U INITIAL "99999999999999" 
     LABEL "Cuenta Final" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CueIni AS CHARACTER FORMAT "X(14)":U INITIAL "0" 
     LABEL "Cuenta Inicial" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE NitFin AS CHARACTER FORMAT "X(14)":U INITIAL "ZZZZZZZZZZZZZZ" 
     LABEL "Nit Final" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NitIni AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit Inicial" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCueFin AS CHARACTER FORMAT "X(35)":U INITIAL "Todas las Cuentas" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCueIni AS CHARACTER FORMAT "X(35)":U INITIAL "Todas las Cuentas" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomNitFin AS CHARACTER FORMAT "X(50)":U INITIAL "Todos los Nits" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomNitIni AS CHARACTER FORMAT "X(50)":U INITIAL "Todos los Nits" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ValFin AS DECIMAL FORMAT ">>>,>>>,>>9.999":U INITIAL 999999999 
     LABEL "Valor Final" 
     VIEW-AS FILL-IN 
     SIZE 15.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ValIni AS DECIMAL FORMAT ">>>,>>>,>>9.999":U INITIAL 0 
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

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 1.88.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 1.88.

DEFINE VARIABLE TTotales AS LOGICAL INITIAL no 
     LABEL "Solo Totales?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .77 NO-UNDO.

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
     BUTTON-143 AT ROW 1.54 COL 102
     BUTTON-153 AT ROW 2.88 COL 3
     BUTTON-144 AT ROW 2.88 COL 56
     BUTTON-149 AT ROW 3.15 COL 102
     E1 AT ROW 4.5 COL 3 NO-LABEL
     btn_ejecutar AT ROW 4.77 COL 102
     BUTTON-150 AT ROW 19.04 COL 102
     BUTTON-152 AT ROW 20.92 COL 105
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 21.38
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Buscar
     Buscar AT ROW 1.27 COL 7 COLON-ALIGNED
     BUTTON-154 AT ROW 1.54 COL 45
     Btn_anterior AT ROW 2.35 COL 9
     Btn_siguiente AT ROW 2.35 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 45 ROW 1.54
         SIZE 53 BY 3.5
         BGCOLOR 17 FONT 4
         TITLE "Buscar".

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
     BUTTON-121 AT ROW 3.12 COL 60
     W_DiaIni AT ROW 3.27 COL 11 COLON-ALIGNED NO-LABEL
     AnoIni AT ROW 3.27 COL 26 COLON-ALIGNED NO-LABEL
     BUTTON-120 AT ROW 3.27 COL 34
     W_DiaFin AT ROW 3.27 COL 39 NO-LABEL
     AnoFin AT ROW 3.27 COL 52 COLON-ALIGNED NO-LABEL
     CueIni AT ROW 4.77 COL 13 COLON-ALIGNED
     NomCueIni AT ROW 4.77 COL 29 COLON-ALIGNED NO-LABEL
     CueFin AT ROW 5.85 COL 13 COLON-ALIGNED
     NomCueFin AT ROW 5.85 COL 29 COLON-ALIGNED NO-LABEL
     NitIni AT ROW 7.46 COL 13 COLON-ALIGNED
     NomNitIni AT ROW 7.46 COL 29 COLON-ALIGNED NO-LABEL
     NitFin AT ROW 8.54 COL 13 COLON-ALIGNED
     NomNitFin AT ROW 8.54 COL 29 COLON-ALIGNED NO-LABEL
     TTotales AT ROW 9.88 COL 8
     ValIni AT ROW 10.96 COL 47 COLON-ALIGNED
     ValFin AT ROW 12.04 COL 47 COLON-ALIGNED
     BUTTON-145 AT ROW 13.12 COL 54
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 11 BY .69 AT ROW 2.46 COL 17
          BGCOLOR 17 FGCOLOR 7 
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.5 COL 43
          BGCOLOR 17 FGCOLOR 7 
     Img_MesF AT ROW 3.27 COL 43
     Img_MesI AT ROW 3.27 COL 17
     RECT-282 AT ROW 2.35 COL 12
     RECT-283 AT ROW 2.35 COL 38
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 30 ROW 4.5
         SIZE 69 BY 15.62
         BGCOLOR 17 FONT 5
         TITLE "Filtros de Información".


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
         TITLE              = "SFG - Modulo de Informes Básicos"
         HEIGHT             = 21.38
         WIDTH              = 113.57
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

ASSIGN XXTABVALXX = FRAME F_Buscar:MOVE-AFTER-TAB-ITEM (Cmb_Tipo:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Buscar:MOVE-BEFORE-TAB-ITEM (BUTTON-143:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Filtros:MOVE-AFTER-TAB-ITEM (E1:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Filtros:MOVE-BEFORE-TAB-ITEM (btn_ejecutar:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-AFTER-TAB-ITEM (btn_ejecutar:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-BEFORE-TAB-ITEM (BUTTON-150:HANDLE IN FRAME F_Basicos)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME F_Buscar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Buscar:HIDDEN           = TRUE
       FRAME F_Buscar:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Filtros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Filtros:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN AnoFin IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCueFin IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCueIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomNitFin IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomNitIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
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
ON END-ERROR OF wWin /* SFG - Modulo de Informes Básicos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Modulo de Informes Básicos */
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


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME btn_ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ejecutar wWin
ON CHOOSE OF btn_ejecutar IN FRAME F_Basicos /* Ejecutar */
DO:
  ASSIGN FRAME F_Basicos Cmb_Tipo.
  ASSIGN FRAME F_Filtros AnoIni AnoFin Cmb_Agencias W_DiaIni W_DiaFin
               CueIni CueFin NitIni NitFin ValIni ValFin TTotales.
  
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = AgeIni.
  
  VIEW FRAME F_Progreso.
  RUN Informes_Retencion.
  W_ok = e1:READ-FILE(Listado) IN FRAME F_Basicos.
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.
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


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 wWin
ON CHOOSE OF BUTTON-120 IN FRAME F_Filtros /* Button 120 */
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


&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 wWin
ON CHOOSE OF BUTTON-121 IN FRAME F_Filtros /* Button 121 */
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
          RUN _osprint.r ( INPUT  ?, INPUT Listado,INPUT  ?,INPUT  1,INPUT  1,
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
&Scoped-define SELF-NAME Cmb_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tipo wWin
ON VALUE-CHANGED OF Cmb_Tipo IN FRAME F_Basicos /* Tipo de Informe */
DO:
  ASSIGN FRAME F_Basicos Cmb_Tipo.
  ASSIGN ValIni:SCREEN-VALUE IN FRAME F_Filtros = "0"
         ValFin:SCREEN-VALUE IN FRAME F_Filtros = "999999999".
  CASE Cmb_Tipo:
    WHEN "Retenciones" THEN DO:
      ASSIGN ValIni:SENSITIVE = NO
             ValFin:SENSITIVE = NO
             ValIni:LABEL = "Valor Inicial"
             ValFin:LABEL = "Valor Final".
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME CueFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CueFin wWin
ON LEAVE OF CueFin IN FRAME F_Filtros /* Cuenta Final */
DO:
  IF SELF:SCREEN-VALUE NE "99999999999999" THEN DO:
     FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE Cuentas THEN
        NomCueFin:SCREEN-VALUE = Cuentas.Nombre.
     ELSE DO:
       RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                  OUTPUT W_CtrNat, INPUT "M").
       ASSIGN SELF:SCREEN-VALUE = W_PCuenta
              NomCueFin:SCREEN-VALUE = W_PNombre.
     END.
  END.
  ELSE ASSIGN NomCueFin:SCREEN-VALUE = "Todas las Cuentas".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CueIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CueIni wWin
ON LEAVE OF CueIni IN FRAME F_Filtros /* Cuenta Inicial */
DO:
  IF SELF:SCREEN-VALUE NE "0" THEN DO:
     FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE Cuentas THEN
        NomCueIni:SCREEN-VALUE = Cuentas.Nombre.
     ELSE DO:
       RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                  OUTPUT W_CtrNat, INPUT "M").
       ASSIGN SELF:SCREEN-VALUE = W_PCuenta
              NomCueIni:SCREEN-VALUE = W_PNombre.
     END.
  END.
  ELSE ASSIGN NomCueIni:SCREEN-VALUE = "Todas las Cuentas".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME NitFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NitFin wWin
ON LEAVE OF NitFin IN FRAME F_Filtros /* Nit Final */
DO:
  IF SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) EQ "000" THEN ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) AgeFin = AgeIni.
    
  IF SELF:SCREEN-VALUE NE "99999999999999" THEN DO:
     FIND Clientes WHERE 
          Clientes.Agencia GE AgeIni AND
          Clientes.Agencia LE AgeFin AND
          Clientes.Nit EQ SELF:SCREEN-VALUE 
          NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN
        NomNitFin:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        ASSIGN NomNitFin:SCREEN-VALUE = P_Nombre + " " + P_Apellido 
               SELF:SCREEN-VALUE      = P_Nit.
     END.
  END.
  ELSE ASSIGN NomNitFin:SCREEN-VALUE = "Todos los Nits".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME NitIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NitIni wWin
ON LEAVE OF NitIni IN FRAME F_Filtros /* Nit Inicial */
DO:
  IF SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3) EQ "000" THEN ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) AgeFin = AgeIni.
  
  IF SELF:SCREEN-VALUE NE "0" THEN DO:
     FIND Clientes WHERE 
          Clientes.Agencia GE AgeIni AND
          Clientes.Agencia LE AgeFin AND
          Clientes.Nit EQ SELF:SCREEN-VALUE 
          NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN
        NomNitIni:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        ASSIGN NomNitIni:SCREEN-VALUE = P_Nombre + " " + P_Apellido
               SELF:SCREEN-VALUE      = P_Nit.
     END.
  END.
  ELSE ASSIGN NomNitIni:SCREEN-VALUE = "Todos los Nits".
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
  DISPLAY Cmb_Tipo E1 
      WITH FRAME F_Basicos IN WINDOW wWin.
  ENABLE Cmb_Tipo BUTTON-143 BUTTON-153 BUTTON-144 BUTTON-149 E1 btn_ejecutar 
         BUTTON-150 BUTTON-152 
      WITH FRAME F_Basicos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Basicos}
  DISPLAY Buscar 
      WITH FRAME F_Buscar IN WINDOW wWin.
  ENABLE Buscar BUTTON-154 Btn_anterior Btn_siguiente 
      WITH FRAME F_Buscar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Buscar}
  DISPLAY Cmb_Agencias W_DiaIni AnoIni W_DiaFin AnoFin CueIni NomCueIni CueFin 
          NomCueFin NitIni NomNitIni NitFin NomNitFin TTotales ValIni ValFin 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE Img_MesF Img_MesI RECT-282 RECT-283 Cmb_Agencias BUTTON-121 BUTTON-120 
         CueIni CueFin NitIni NitFin TTotales ValIni ValFin BUTTON-145 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Retencion wWin 
PROCEDURE Informes_Retencion :
Listado = W_PathSpl + "Info_Pagos.LST".  
OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)".
DEFINE VAR WEnc2 AS CHARACTER FORMAT "X(2)".
DEFINE VAR NomEmp AS CHARACTER FORMAT "X(50)".
DEFINE VAR NomCli AS CHARACTER FORMAT "X(45)".
DEFINE VAR Nombre AS CHARACTER FORMAT "X(50)".

DEFINE VAR NomCta AS CHARACTER FORMAT "X(50)".
DEFINE VAR Bs_Deb  AS DECIMAL FORMAT "->,>>>,>>>,>>>".
DEFINE VAR Bs_Cre  AS DECIMAL FORMAT "->,>>>,>>>,>>9".
DEFINE VAR Vr_Deb  AS DECIMAL FORMAT "->,>>>,>>>,>>>".
DEFINE VAR Vr_Cre  AS DECIMAL FORMAT "->,>>>,>>>,>>9".
DEFINE VAR CBs_Deb AS DECIMAL FORMAT "->,>>>,>>>,>>>".
DEFINE VAR CBs_Cre AS DECIMAL FORMAT "->,>>>,>>>,>>9".
DEFINE VAR CVr_Deb AS DECIMAL FORMAT "->,>>>,>>>,>>>".
DEFINE VAR CVr_Cre AS DECIMAL FORMAT "->,>>>,>>>,>>9".
DEFINE VAR ABs_Deb AS DECIMAL FORMAT "->,>>>,>>>,>>>".
DEFINE VAR ABs_Cre AS DECIMAL FORMAT "->,>>>,>>>,>>9".
DEFINE VAR AVr_Deb AS DECIMAL FORMAT "->,>>>,>>>,>>>".
DEFINE VAR AVr_Cre AS DECIMAL FORMAT "->,>>>,>>>,>>9".
DEFINE VAR TBs_Deb AS DECIMAL FORMAT "->,>>>,>>>,>>>".
DEFINE VAR TBs_Cre AS DECIMAL FORMAT "->,>>>,>>>,>>9".
DEFINE VAR TVr_Deb AS DECIMAL FORMAT "->,>>>,>>>,>>>".
DEFINE VAR TVr_Cre AS DECIMAL FORMAT "->,>>>,>>>,>>9".

DEFINE VAR CDif AS DECIMAL FORMAT "->,>>>,>>>,>>9".
DEFINE VAR ADif AS DECIMAL FORMAT "->,>>>,>>>,>>9".
DEFINE VAR TDif AS DECIMAL FORMAT "->,>>>,>>>,>>9".


DEFINE VAR Valor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR AcumValor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".

DEFINE VAR TotReg AS DECIMAL.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : INFORME DE RETENCION EN LA FUENTE " 
              + " - Fec.Inicial: " + STRING(FecIni) + " - Fec.Final: " + STRING(FecFin) + " - " + STRING(TIME,"hh:mm am").
               /* 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
                           1         2         3         4         5         6         7         8         9         1         2         3     */
  ASSIGN j = 0 k = 0.
  FOR EACH Tr: DELETE Tr. END.
  /*arma tabla para informe*/
  FOR EACH Mov_Contable WHERE 
           Mov_Contable.Agencia GE AgeIni AND
           Mov_Contable.Agencia LE AgeFin AND
           Mov_Contable.Cuenta  GE CueIni AND
           Mov_Contable.Cuenta  LE CueFin AND
           Mov_Contable.Cuenta  BEGINS "24" AND
           Mov_Contable.Nit     NE ""     AND
           Mov_Contable.Nit     GE NitIni AND
           Mov_Contable.Nit     LE NitFin AND
           /*mov_contable.nit     EQ "71654028" AND*/
           Mov_Contable.Fec_Contable GE FecIni AND
           Mov_Contable.Fec_Contable LE FecFin AND
         ((Mov_Contable.Db      GE ValIni AND Mov_Contable.Db LE ValFin) OR
          (Mov_Contable.Cr      GE ValIni AND Mov_Contable.Cr LE ValFin)) NO-LOCK
           BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta BY Mov_Contable.Nit:
     j = j + 1.
     RUN Progreso.
     FIND Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta AND 
          Cuentas.Id_Nit AND Cuentas.Id_Base NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Cuentas THEN NEXT.
     IF FIRST-OF(Mov_Contable.Nit) THEN DO:
        FIND Clientes WHERE Clientes.Nit EQ Mov_Contable.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN
           NomCli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        CREATE Tr.
     END.
     
     ASSIGN Tr.TAge = Mov_Contable.Agencia
            Tr.TCue = Mov_Contable.Cuenta
            Tr.TNCt = Cuentas.Nombre
            Tr.TNit = Mov_Contable.Nit
            Tr.TNom = NomCli.
     IF Mov_Contable.Nit EQ "800197384-0" /*AND Mov_Contable.Db GT 0*/ THEN
        NEXT.
     ASSIGN Tr.TRDb = Tr.TRDb + Mov_Contable.Db
            Tr.TRCr = Tr.TRCr + Mov_Contable.Cr.
            /*Tr.TTot = Tr.TTot + (Tr.TRCr - Tr.TRDb).*/
     FIND Base_Ret WHERE Base_Ret.Cod_Base EQ Cuentas.Cod_Base NO-LOCK NO-ERROR.
     IF AVAILABLE Base_Ret THEN DO:
        Tr.TPor = Base_Ret.Porcentaje.
        IF Mov_Contable.Db GT 0 THEN
           Tr.TBDb = Tr.TBDb + Mov_Contable.Db * (100 / Base_Ret.Porcentaje).
        IF Mov_Contable.Cr GT 0 THEN
           Tr.TBCr = Tr.TBCr + Mov_Contable.Cr * (100 / Base_Ret.Porcentaje).
     END.
  END.

/*primer informe*/
IF Cmb_Tipo:SCREEN-VALUE IN FRAME F_Basicos EQ "Por Agencia/Cuenta/Nit" THEN DO: 
    W_EncColumna = "    Nit             Nombre                           Base Debito    Ret.Deb   Base Credito   Ret.Cred          Neto".
    VIEW FRAME F-Encabezado.
    FOR EACH Tr BREAK BY Tr.TAge BY Tr.TCue BY Tr.TNit:
        IF FIRST-OF(Tr.TAge) THEN DO:
           FIND Agencias WHERE Agencias.Agencia EQ Tr.TAge NO-LOCK NO-ERROR.
           IF AVAILABLE Agencias THEN DO:
              DISPLAY Agencias.Agencia AT 1 FORMAT "999"
                      Agencias.Nombre  AT 5 
              WITH FRAME ST_NomAge WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
           END.
        END.
        IF NOT TTotales THEN DO:
            IF FIRST-OF(Tr.TCue) THEN
               DISPLAY SKIP(1) Tr.TCue AT 3  FORMAT "X(14)"
                       Tr.TNCt AT 19 FORMAT "X(30)"
                       "Porcentaje Retencion: "
                       Tr.TPor AT 75 SKIP(1)
               WITH FRAME ST_Cue WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            DISPLAY Tr.TNit           AT 5   FORMAT "X(14)"
                    Tr.TNom           AT 21  FORMAT "X(25)"
                    Tr.TBDb           AT 51  FORMAT "->>>,>>>,>>9"
                    Tr.TRDb           AT 66  FORMAT "->,>>>,>>9"
                    Tr.TBCr           AT 77  FORMAT "->>>,>>>,>>9"
                    Tr.TRCr           AT 92  FORMAT "->,>>>,>>9"
                    Tr.TRCr - Tr.TRDb AT 103 FORMAT "->>>,>>>,>>9"
            WITH FRAME ST_Mov0 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        END.
        ASSIGN    CBs_Deb = CBs_Deb + Tr.TBDb
                  CBs_Cre = CBs_Cre + Tr.TBCr
                  CVr_Deb = CVr_Deb + Tr.TRDb
                  CVr_Cre = CVr_Cre + Tr.TRCr
                  CDif    = CVr_Cre - CVr_Deb.
        IF LAST-OF(Tr.TCue) THEN DO:
              DISPLAY "-----------------------------------------------------------------------------------------------------" AT 1 SKIP
                      Tr.TCue         AT 3
                      Tr.TNct         AT 19 FORMAT "X(20)"
                      "%"             AT 41
                      Tr.TPor         AT 43 FORMAT ">9.999"
                      CBs_Deb         AT 51  FORMAT "->>>,>>>,>>9"
                      CVr_Deb         AT 66  FORMAT "->,>>>,>>9"
                      CBs_Cre         AT 77  FORMAT "->>>,>>>,>>9"
                      CVr_Cre         AT 92  FORMAT "->,>>>,>>9"
                      CDif            AT 103 FORMAT "->>>,>>>,>>9"
                      "-----------------------------------------------------------------------------------------------------" AT 1 
              WITH FRAME ST_Cuentas WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO NO-UNDERLINE.
              ASSIGN ABs_Deb = ABs_Deb + CBs_Deb
                     ABs_Cre = ABs_Cre + CBs_Cre
                     AVr_Deb = AVr_Deb + CVr_Deb
                     AVr_Cre = AVr_Cre + CVr_Cre
                     ADif    = AVr_Cre - AVr_Deb.
              ASSIGN CBs_Deb = 0 CBs_Cre = 0 CVr_Deb = 0 CVr_Cre = 0 CDif = 0.
        END.

        IF LAST-OF(Tr.TAge) THEN DO:
            DISPLAY "Tot Agencia: "  AT 5
                    Tr.TAge         AT 23   FORMAT "999"
                    ABs_Deb         AT 51   FORMAT "->>>,>>>,>>9"
                    AVr_Deb         AT 66   FORMAT "->,>>>,>>9"
                    ABs_Cre         AT 77   FORMAT "->>>,>>>,>>9"
                    AVr_Cre         AT 92   FORMAT "->,>>>,>>9"
                    ADif            AT 103  FORMAT "->>>,>>>,>>9" SKIP(1)
            WITH FRAME TT_Age WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            ASSIGN TBs_Deb = TBs_Deb + ABs_Deb
                   TBs_Cre = TBs_Cre + ABs_Cre
                   TVr_Deb = TVr_Deb + AVr_Deb
                   TVr_Cre = TVr_Cre + AVr_Cre
                   TDif    = TVr_Cre - TVr_Deb.
            ASSIGN ABs_Deb = 0 ABs_Cre = 0 AVr_Deb = 0 AVr_Cre = 0 ADif = 0.
        END.
    END.
    DEFIN VAR WK  AS INTEGER   FORMAT ">>9".
    DEFIN VAR WK2 AS INTEGER   FORMAT "->>>>,>>>,>>9".
    DEFIN VAR TPI AS CHARACTER FORMAT "X(7)".

    IF INTEGER(SUBSTRING(STRING(Tdif,"->>>>,>>>,>>9"),11,3)) GE 500 THEN
       ASSIGN Wk = 1000 - INTEGER(SUBSTRING(STRING(Tdif,"->>>>,>>>,>>9"),11,3))
              Wk2 = TDif + Wk
              TPi = "Inc:" + STRING(Wk).
    ELSE
       ASSIGN Wk = INTEGER(SUBSTRING(STRING(Tdif,"->>>>,>>>,>>9"),11,3))
              Wk2 = TDif - Wk
              Tpi = "Dec:" + STRING(Wk).

    DISPLAY "Tot Entidad: "  AT 5
             TBs_Deb         AT 51   FORMAT ">>>,>>>,>>9"
             TVr_Deb         AT 66   FORMAT ">>,>>>,>>9"  
             TBs_Cre         AT 77   FORMAT ">>>,>>>,>>9"
             TVr_Cre         AT 92   FORMAT ">>,>>>,>>9"  
             TDif            AT 103  FORMAT ">>>,>>>,>>9"
             Wk2             AT 116  FORMAT ">>>,>>>,>>9"
             Tpi             AT 129  FORMAT "X(7)" SKIP(1)
    WITH FRAME TT_Tot2 WIDTH 145 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

    VIEW FRAME F-Ftr.
    PAGE.
END.

DEFINE VAR TmpVal AS CHARACTER FORMAT "X(14)".
FOR EACH it: DELETE it. END.
/*Segundo informe*/
IF Cmb_Tipo:SCREEN-VALUE IN FRAME F_Basicos EQ "Total por Entidad/Cuentas" THEN DO:  
    W_EncColumna = "".
    VIEW FRAME F-Encabezado.
    FOR EACH Tr BREAK BY Tr.TCue:
      IF FIRST-OF(Tr.TCue) THEN DO:
          CREATE It.
          ASSIGN It.I_Cue = Tr.TCue
                 It.I_Nct = Tr.TNCt.
      END.
      ASSIGN It.I_Tot[Tr.TAge] = It.I_Tot[Tr.TAge] + (Tr.TRCr - Tr.TRDb)
             It.I_Tot[11] = It.I_Tot[11] + (Tr.TRCr - Tr.TRDb).
      IF LAST-OF(Tr.TCue) THEN DO:
          TmpVal = STRING(It.I_Tot[11],"->>>>,>>>,>>9").
          IF DECIMAL(SUBSTRING(TmpVal,11,3)) GE 500 THEN
             ASSIGN Wk = 1000 - INTEGER(SUBSTRING(TmpVal,11,3))
                    It.I_Pag = DECIMAL(TmpVal) + Wk
                    It.I_Aju = DECIMAL(Wk).
          ELSE
             ASSIGN Wk = INTEGER(SUBSTRING(TmpVal,11,3))
                    It.I_Pag = DECIMAL(TmpVal) - Wk
                    It.I_Aju = Wk - Wk * 2.
      END.
    END.
  ASSIGN TT1 = 0 TT2 = 0 TT3 = 0 TT4 = 0 TT5 = 0
        TT6 = 0 TT7 = 0 TT8 = 0 TT9 = 0 TT10 = 0
        TT11 = 0 TT12 = 0 TT13 = 0.
        /*123456789012  123456789012 123456789012 123456789012 123456789012 123456789012 123456789012 123456789012 123456789012 123456789012 123456789012 123456789012 123456789012 123456789012*/
  DEFINE VAR AjuInf AS CHARACTER FORMAT "X(15)".
  DISPLAY "Cuenta                         Age.Belen    Age.Centro  Age.Sabaneta  Age.Caldas  Age.Envigado  Age.Itagui  Age.Floresta Age.Rionegro Age.AltaVist   Age.Admon      Total         Pago        Ajuste"
          WITH FRAME F_TitIt WIDTH 240.
  FOR EACH It BREAK BY It.I_Cue:
      DISPLAY It.I_Cue It.I_Nct FORMAT "X(15)" It.I_Tot It.I_Pag It.I_Aju
          WITH FRAME F_MovIt WIDTH 240 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
      ASSIGN TT1 = TT1 + It.I_Tot[1] TT2 = TT2 + It.I_Tot[2]
             TT3 = TT3 + It.I_Tot[3] TT4 = TT4 + It.I_Tot[4]
             TT5 = TT5 + It.I_Tot[5] TT6 = TT6 + It.I_Tot[6]
             TT7 = TT7 + It.I_Tot[7] TT8 = TT8 + It.I_Tot[8]
             TT9 = TT9 + It.I_Tot[9] TT10 = TT10 + It.I_Tot[10]
             TT11 = TT11 + It.I_Tot[11]
             TT12 = TT12 + It.I_Pag
             TT13 = TT13 + It.I_Aju.
  END.
  DISPLAY skip(1) "Totales                       " TT1 TT2 TT3 TT4 TT5 TT6 TT7 TT8 TT9 TT10 TT11 TT12 TT13
      WITH FRAME FTot2 WIDTH 240 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
END. 


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
  W_DiaFin = DAY(TODAY).
  RUN SUPER.
  DO WITH FRAME F_Filtros:
    ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
           W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
           AnoIni:SCREEN-VALUE = STRING(YEAR(TODAY))
           AnoFin:SCREEN-VALUE = STRING(YEAR(TODAY))
           MesIni = MONTH(TODAY)
           MesFin = MONTH(TODAY)
           AgeIni = W_Agencia
           AgeFin = W_Agencia
           FecIni = (TODAY - DAY(TODAY)) + 1
           FecFin = TODAY.
   END.

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

