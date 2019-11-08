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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


                                   
/*  {Incluido\variable.i "SHARED"}  */
/*  {Incluido\varcon.i "SHARED"}    */


/********************************/
  DEFINE VARIABLE W_Usuario   LIKE usuarios.usuario       INITIAL "339". /* 308 - Contabiliza*/
  DEFINE VARIABLE W_Fecha     AS DATE   INITIAL TODAY.
  DEFINE VARIABLE W_PathSpl   AS CHARACTER FORMAT "X(20)" INITIAL "c:\info_juriscoop\".
  DEFINE VARIABLE W_Agencia   LIKE Agencia.Agencia        INITIAL "024". /*"035".*/

    DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion INITIAL "5".
/*     DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario. */
    DEFINE {1} VAR W_Clave       LIKE Usuarios.Clave FORMAT "X(16)".
    DEFINE {1} VAR W_Prioridad   LIKE Usuarios.Prioridad INITIAL "".
/*     DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia INITIAL 0.  */
    DEFINE {1} VAR W_Ciudad      LIKE Agencia.Ciudad INITIAL 0.
    DEFINE {1} VAR W_Nom_Agencia   AS CHARACTER FORMAT "X(60)".
    DEFINE {1} VAR W_UbiDatos      AS CHAR INITIAL "D".
    /*DEFINE {1} VAR W_Ninv        LIKE Inversion.Nro_inversion.*/
    DEFINE {1} VAR W_Nom_Entidad   AS CHARACTER FORMAT "X(60)".
    DEFINE {1} VAR W_Entidad     LIKE Entidad.Entidad.
    DEFINE {1} VAR W_NitGlobal   LIKE Clientes.Nit INITIAL "".
    DEFINE {1} VAR W_SMLV        LIKE Indicadores.Valor INITIAL 0.
    DEFINE {1} VAR W_Manija        AS HANDLE.
    DEFINE {1} VAR W_ManFin        AS HANDLE.
    DEFINE {1} VAR W_ManTaq        AS HANDLE.
    DEFINE {1} VAR W_Nivel       LIKE Cuentas.Nivel.
    DEFINE {1} VAR W_CtaMay      LIKE Cuentas.Cuenta.
/*     DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.  */
    DEFINE {1} VAR W_ficina        AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1.
    DEFINE {1} VAR W_Path        LIKE Entidad.Dir_Programas.
/*     DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl.  */
    DEFINE {1} VAR W_Eleccion      AS LOGICAL.
    DEFINE {1} VAR W_CedGral     LIKE Clientes.Nit.
    DEFINE {1} VAR W_CenCosGral  LIKE Cen_Costos.Cen_Costos.
    DEFINE {1} VAR W_Cadena        AS CHARACTER FORMAT "X(9)" INITIAL "SIFINCOOP".
    /*DEFINE     VAR Agencia_Cnt     AS INTEGER FORMAT "999".*/
    DEFINE {1} VAR P-Valida        AS LOGICAL.
    DEFINE {1} VAR W_VCodPcto    LIKE Ahorros.Cod_Ahorro.
    DEFINE {1} VAR W_VCueAhorro  LIKE Ahorros.Cue_Ahorros.
    DEFINE {1} VAR W_Solicitud   LIKE Solicitud.Num_Solicitud.
    DEFINE {1} VAR W_PagareS     LIKE Creditos.Pagare.
    DEFINE {1} VAR P_SdoTot      LIKE Creditos.Sdo_Capital.
    DEFINE {1} VAR W_OfiCierre   LIKE Agencias.Agencia.
  /********************************************************************************** 
   Variables globales contables
**********************************************************************************/
  
  DEFINE {1} VAR W_ManCon AS HANDLE.
  DEFINE {1} VAR W_FecIni AS DATE FORMAT "99/99/9999" INITIAL TODAY.
  DEFINE {1} VAR W_FecFin AS DATE FORMAT "99/99/9999" INITIAL TODAY.
  DEFINE {1} VAR W_Mes    AS INTEGER FORMAT "99".
  DEFINE {1} VAR W_MesFin AS INTEGER FORMAT "99".
  DEFINE {1} VAR W_UsuFin LIKE Usuarios.Usuario.
  DEFINE {1} VAR W_UsuIni LIKE Usuarios.Usuario.
  DEFINE {1} VAR W_CtaIni LIKE Cuentas.Cuenta INITIAL "".
  DEFINE {1} VAR W_CtaFin LIKE Cuentas.Cuenta INITIAL "".
  DEFINE {1} VAR W_ComTra LIKE Comprobantes.Comprobante INITIAL 0.
  DEFINE {1} VAR W_ComIni LIKE Comprobantes.Comprobante INITIAL 0.
  DEFINE {1} VAR W_ComFin LIKE Comprobantes.Comprobante INITIAL 0.  
  DEFINE {1} VAR W_CenTra LIKE Cen_Costos.Cen_Costos    INITIAL 0.
  DEFINE {1} VAR W_OfiIni LIKE Agencias.Agencia         INITIAL 0.
  DEFINE {1} VAR W_OfiFin LIKE Agencias.Agencia         INITIAL 0.
  DEFINE {1} VAR W_CenIni LIKE Cen_Costos.Cen_Costos    INITIAL 0.
  DEFINE {1} VAR W_CenFin LIKE Cen_Costos.Cen_Costos    INITIAL 0.  
  DEFINE {1} VAR W_NitIni LIKE Terceros.Nit INITIAL "".
  DEFINE {1} VAR W_NitFin LIKE Terceros.Nit INITIAL "".
  DEFINE {1} VAR W_BaseInf  AS DECIMAL INITIAL 0 FORMAT "->>>,>>>,>>>,>>9".
  DEFINE {1} VAR W_Ope      AS CHARACTER FORMAT "X(13)"  VIEW-AS COMBO-BOX 
        LIST-ITEMS "Sin Seleccion","Mayor Que","Menor Que","Igual" SIZE 13 BY 4.5.
  DEFINE {1} VAR TotDctoDeb AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotDctoCre AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotGralDeb AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotGralCre AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR VlrDeb     AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR VlrCre     AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotOfCre   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotOfDeb   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotCoCre   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotCoDeb   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR W_SaldoAct AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99" .
  DEFINE {1} VAR W_SaldoAnt AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99" .  
  DEFINE {1} VAR W_PorReex  AS DECIMAL INITIAL 0 FORMAT "-ZZ9.99".
  DEFINE {1} VAR W_Raya1    AS CHAR    INITIAL "" FORMAT "X(130)".
  DEFINE {1} VAR W_Raya2    AS CHAR    INITIAL "" FORMAT "X(130)".
  DEFINE {1} VAR W_NomMes   AS CHAR    INITIAL "" FORMAT "X(20)".
  DEFINE {1} VAR W_NomCta   AS CHAR    INITIAL "".        
  DEFINE {1} VAR W_Opcion   AS CHAR    INITIAL "P".
  DEFINE {1} VAR W_Destino  AS CHAR    INITIAL "".
  DEFINE {1} VAR W_NomOfi LIKE Agencias.Nombre.
  DEFINE {1} VAR W_NomCen LIKE Cen_Costos.Nombre INITIAL "Consolidado".
  DEFINE {1} VAR W_NomCom LIKE Comprobantes.Nombre INITIAL "Consolidado".
  DEFINE {1} VAR W_NomIni LIKE Cuentas.Nombre.    
  DEFINE {1} VAR W_NomFin LIKE Cuentas.Nombre.
  DEFINE {1} VAR W_NoInNit  AS CHAR FORMAT "X(40)" INITIAL "".
  DEFINE {1} VAR W_NoFiNit  AS CHAR FORMAT "X(40)" INITIAL "".
  DEFINE {1} VAR W_Rtipo    AS INTEGER INITIAL 1 LABEL  "Tipo Informe " VIEW-AS RADIO-SET HORIZONTAL
                        RADIO-BUTTONS "Detallado/Nit",1,"Resumido/Cuenta",2 SIZE 30 BY 0.81.
  DEFINE {1} VAR W_Ret    AS INTEGER INITIAL 1 VIEW-AS RADIO-SET HORIZONTAL
                        RADIO-BUTTONS "Iva",1,"Retencion fuente",2,"Pagos a Terceros", 3 SIZE 40 BY 0.71.
  DEFINE {1} VAR W_RForma   AS INTEGER INITIAL 0 LABEL "Formato       " VIEW-AS RADIO-SET HORIZONTAL
                        RADIO-BUTTONS "-/+",0,"(",1,"-/+$",2,"($",3 SIZE 25 BY 0.81.
  DEFINE {1} VAR W_Rpta     AS LOGICAL.
  DEFINE {1} VAR W_Validacion AS INTEGER INITIAL -1.
  DEFINE {1} VAR W_Raya     AS CHAR INITIAL "-" FORMAT "X".
  DEFINE {1} VAR W_OfiTra   AS INTEGER FORMAT ">>9" VIEW-AS FILL-IN.
  DEFINE {1} VAR W_Id_Paag  AS LOGICAL INITIAL FALSE LABEL "Sin Ajuste" VIEW-AS TOGGLE-BOX SIZE 10 BY 1.
  DEFINE {1} VAR W_Id_Defla AS LOGICAL INITIAL FALSE LABEL "Deflactado" VIEW-AS TOGGLE-BOX SIZE 10 BY 1.
  DEFINE {1} VAR W_TitRet  AS CHARACTER FORMAT "X(40)".
/********************/
/*** Nuevo */
DEFINE VARIABLE vcEstado    AS CHARACTER FORMAT "X(15)"      INITIAL "" NO-UNDO.
DEFINE VARIABLE vcNom       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcDesc      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcNomAct    AS CHARACTER FORMAT "X(40)"      NO-UNDO.
DEFINE VARIABLE vdFecCompra AS DATE      FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE vcCodigo    AS CHARACTER FORMAT "X(10)"      NO-UNDO.
DEFINE VARIABLE vcFactura   AS CHARACTER FORMAT "X(15)"      NO-UNDO.
DEFINE VARIABLE vcCodAge    AS CHARACTER FORMAT "X(5)"       NO-UNDO.
DEFINE VARIABLE vdTotCompra AS DECIMAL   FORMAT "->>>,>>>,>>>,>>9.99"   NO-UNDO.
DEFINE VARIABLE vdTotDepre  AS DECIMAL   FORMAT "->>>,>>>,>>>,>>9.99"   NO-UNDO.
DEFINE VARIABLE vcEspacio   AS CHARACTER FORMAT "X(40)"      INITIAL "" NO-UNDO.
DEFINE VARIABLE vcEspacio1  AS CHARACTER FORMAT "X(15)"      INITIAL "" NO-UNDO.

/**************/
/*para busqueda de cuentas*/
  DEFINE VAR P_Cuenta LIKE Cuentas.Cuenta.
  DEFINE VAR P_Nombre LIKE Cuentas.Nombre.
  DEFINE VAR P_NatTra LIKE Cuentas.Naturaleza.
  DEFINE VAR P_CtrNat LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VAR P_TipCta AS CHAR.  
  
/*edwin gomez 11/02/2004*/
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR i AS INTEGER.
  DEFINE VAR j AS INTEGER.
  DEFINE VAR k AS INTEGER.
  
  DEFINE VARIABLE W_Puntero AS ROWID.
  DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
  DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
  DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
  DEFINE VARIABLE WFec     AS DATE.   
  DEFINE VARIABLE FecIni   AS DATE.
  DEFINE VARIABLE FecFin   AS DATE.
  DEFINE VARIABLE vcResActIni AS CHARACTER FORMAT "X(14)".
  DEFINE VARIABLE vcResActFin AS CHARACTER FORMAT "X(14)".
  DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
  DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".
  DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE ProIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE ProFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE TpdIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE TpdFin   AS INTEGER FORMAT "999" INITIAL 9.


  DEFINE VAR Tot1 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  DEFINE VAR Tot2 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  DEFINE VAR Tot3 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  DEFINE VAR Tot4 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  
  DEFINE VAR TotA1 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  DEFINE VAR TotA2 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  DEFINE VAR TotA3 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  DEFINE VAR TotA4 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  
  DEFINE VAR TotT1 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  DEFINE VAR TotT2 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  DEFINE VAR TotT3 AS DECIMAL FORMAT "->>>>,>>>,>>9".
  DEFINE VAR TotT4 AS DECIMAL FORMAT "->>>>,>>>,>>9".

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
&Scoped-Define ENABLED-OBJECTS RECT-176 RECT-5 RTipo Cmb_Activos ~
Cmb_Diferidos BUTTON-1 Btn_Filtros Btn_Imprimir BUTTON-153 Btn_Ejecutar E1 ~
Btn_Salir Btn_Ayuda 
&Scoped-Define DISPLAYED-OBJECTS RTipo Cmb_Activos E1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Cmb_Activos Cmb_Proveedores Cmb_Diferidos 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "" 
     SIZE 5 BY 1.08 TOOLTIP "Accesa la Ayuda a la Ventana".

DEFINE BUTTON Btn_Ejecutar 
     LABEL "Ejecutar" 
     SIZE 13 BY 1.88.

DEFINE BUTTON Btn_Filtros 
     LABEL "Cambiar los Rangos de Filtro de Información" 
     SIZE 40 BY 1.12.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 13 BY 1.88 TOOLTIP "Permite Ejecutar el Proceso de Impresión.".

DEFINE BUTTON Btn_Salir AUTO-END-KEY 
     LABEL "&Salir" 
     SIZE 13 BY 1.88 TOOLTIP "Cierra la Ventana Actual y Retorna al Menú".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 12.72 BY 1.88.

DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 153" 
     SIZE 7 BY 1.65.

DEFINE VARIABLE Cmb_Activos AS CHARACTER FORMAT "X(256)":U INITIAL "Relación de Activos" 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Relación de Activos","Listado de Responsables","Resumen por Grupos de Activos" 
     DROP-DOWN-LIST
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Diferidos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Listado General de Diferidos","Resumen por Grupos" 
     DROP-DOWN-LIST
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Proveedores AS CHARACTER FORMAT "X(256)":U 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Descuentos por Pronto Pago","Pagos","Causaciones" 
     DROP-DOWN-LIST
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE E1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 93 BY 15.88
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE RTipo AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Activos Fijos", 1,
"Contabilidad", 2
     SIZE 16 BY 2.15
     FGCOLOR 7  NO-UNDO.

DEFINE RECTANGLE RECT-176
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 2.69.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 6.46.

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

DEFINE BUTTON BUTTON-145 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 145" 
     SIZE 10 BY 1.35.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todas las Agencias" 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CuentaIni AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-FecFin AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-FecIni AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCtaIni AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomResponde AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Responde AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-284
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55.43 BY 2.96.

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
     RTipo AT ROW 1.54 COL 8 NO-LABEL
     Cmb_Activos AT ROW 1.81 COL 47 COLON-ALIGNED
     Cmb_Proveedores AT ROW 1.81 COL 47 COLON-ALIGNED
     Cmb_Diferidos AT ROW 1.81 COL 47 COLON-ALIGNED
     BUTTON-1 AT ROW 1.81 COL 99
     Btn_Filtros AT ROW 2.88 COL 55
     Btn_Imprimir AT ROW 3.69 COL 99 HELP
          "Permite Ejecutar el Proceso de Impresión."
     BUTTON-153 AT ROW 4.23 COL 3
     Btn_Ejecutar AT ROW 5.58 COL 99
     E1 AT ROW 5.85 COL 3 NO-LABEL
     Btn_Salir AT ROW 17.96 COL 99 HELP
          "Cierra la Ventana Actual y Retorna al Menú"
     Btn_Ayuda AT ROW 20.38 COL 103 HELP
          "Accesa la Ayuda a la Ventana"
     RECT-176 AT ROW 1.27 COL 5
     RECT-5 AT ROW 1.54 COL 98
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 22.27
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
         AT COL 99 ROW 8.65
         SIZE 12 BY 6.73
         BGCOLOR 17 .

DEFINE FRAME F_Buscar
     Buscar AT ROW 1.27 COL 7 COLON-ALIGNED
     BUTTON-154 AT ROW 1.54 COL 45
     Btn_anterior AT ROW 2.35 COL 9
     Btn_siguiente AT ROW 2.35 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19 ROW 4.77
         SIZE 53 BY 3.5
         BGCOLOR 17 FONT 4
         TITLE "Buscar".

DEFINE FRAME F_Filtros
     Cmb_Agencias AT ROW 1.27 COL 9 COLON-ALIGNED
     BUTTON-145 AT ROW 3.69 COL 65.86
     F-FecIni AT ROW 4.12 COL 20.72 COLON-ALIGNED HELP
          "Digite Fecha Inicial" NO-LABEL WIDGET-ID 92
     F-FecFin AT ROW 4.15 COL 38 COLON-ALIGNED HELP
          "Digite Fecha Final" NO-LABEL WIDGET-ID 94
     Responde AT ROW 6.12 COL 2 NO-LABEL WIDGET-ID 98
     NomResponde AT ROW 6.12 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     CuentaIni AT ROW 7.15 COL 8 COLON-ALIGNED
     NomCtaIni AT ROW 7.19 COL 24.86 COLON-ALIGNED NO-LABEL
     "Responsable" VIEW-AS TEXT
          SIZE 15 BY .69 AT ROW 5.31 COL 2 WIDGET-ID 104
          BGCOLOR 17 FGCOLOR 5 
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 15 BY .69 AT ROW 3.31 COL 22.72
          BGCOLOR 17 FGCOLOR 7 
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 14.29 BY .62 AT ROW 3.38 COL 40.14
          BGCOLOR 17 FGCOLOR 7 
     "Compra / Asignación" VIEW-AS TEXT
          SIZE 23 BY .69 AT ROW 2.65 COL 11 WIDGET-ID 102
          BGCOLOR 17 FGCOLOR 5 
     RECT-284 AT ROW 2.35 COL 9.57 WIDGET-ID 96
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 22 ROW 4.77
         SIZE 76 BY 8.08
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
         TITLE              = "SFG - Informes de Activos"
         HEIGHT             = 22.27
         WIDTH              = 114
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* REPARENT FRAME */
ASSIGN FRAME F_Buscar:FRAME = FRAME F_Basicos:HANDLE
       FRAME F_Filtros:FRAME = FRAME F_Basicos:HANDLE
       FRAME F_Progreso:FRAME = FRAME F_Basicos:HANDLE.

/* SETTINGS FOR FRAME F_Basicos
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR COMBO-BOX Cmb_Activos IN FRAME F_Basicos
   1                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_Diferidos IN FRAME F_Basicos
   NO-DISPLAY 1                                                         */
ASSIGN 
       Cmb_Diferidos:HIDDEN IN FRAME F_Basicos           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Proveedores IN FRAME F_Basicos
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       Cmb_Proveedores:HIDDEN IN FRAME F_Basicos           = TRUE.

/* SETTINGS FOR FRAME F_Buscar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Buscar:HIDDEN           = TRUE
       FRAME F_Buscar:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Filtros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Filtros:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN CuentaIni IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CuentaIni:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR FILL-IN NomCtaIni IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       NomCtaIni:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR FILL-IN Responde IN FRAME F_Filtros
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME F_Progreso
                                                                        */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Informes de Activos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Informes de Activos */
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
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda wWin
ON CHOOSE OF Btn_Ayuda IN FRAME F_Basicos
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "ayudas/contabil" CONTEXT 42.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ejecutar wWin
ON CHOOSE OF Btn_Ejecutar IN FRAME F_Basicos /* Ejecutar */
DO:
  ASSIGN FRAME F_Basicos RTipo Cmb_Activos Cmb_Diferidos Cmb_Proveedores.
  ASSIGN FRAME F_Filtros F-FecIni F-FecFin.

  ASSIGN F-FecIni
         F-FecFin.
  ASSIGN FecIni = date(F-FecIni:SCREEN-VALUE)
         FecIni = date(F-FecIni:SCREEN-VALUE).
  MESSAGE "Fecha Inicial: " FecIni SKIP
          "Fecha Final  : " FecFin
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  ASSIGN Tot1  = 0 Tot2  = 0 Tot3  = 0 Tot4 = 0
         TotA1 = 0 TotA2 = 0 TotA3 = 0 TotA4 = 0
         TotT1 = 0 TotT2 = 0 TotT3 = 0 TotT4 = 0.
  
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = AgeIni.
  
  IF Responde:SCREEN-VALUE EQ "" THEN
     ASSIGN vcResActIni = "0"  vcResActFin = "99999999999999".
  ELSE
     ASSIGN vcResActIni = Responde:SCREEN-VALUE   vcResActFin = Responde:SCREEN-VALUE.

  MESSAGE "Responde:" vcResActIni
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  VIEW FRAME F_Progreso.
/*   RUN Informes_Activos.  */

  CASE RTipo:
    WHEN 1 THEN RUN Informes_Activos.
    WHEN 2 THEN RUN Informes_Diferidos.
    WHEN 3 THEN RUN Informes_Proveedores.
  END CASE.
  W_ok = e1:READ-FILE(Listado) IN FRAME F_Basicos.
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.
          
  APPLY "entry" TO RTipo IN FRAME F_Basicos.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Filtros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Filtros wWin
ON CHOOSE OF Btn_Filtros IN FRAME F_Basicos /* Cambiar los Rangos de Filtro de Información */
DO:
  DISABLE {&List-1} WITH FRAME F_Basicos.
  CASE Cmb_Activos:
    WHEN string(1) THEN DO:
        DISABLE Responde NomResponde.
        F-FecIni:SCREEN-VALUE IN FRAME F_Filtros = "".
        F-FecFin:SCREEN-VALUE IN FRAME F_Filtros = "".
        NomCtaIni:SCREEN-VALUE IN FRAME F_Filtros = "".
        CuentaIni:SCREEN-VALUE IN FRAME F_Filtros = "".
    END.
    WHEN string(2) THEN DO:
        DISABLE UNLESS-HIDDEN F-FecIni
               F-FecFin
               CuentaIni
               NomCtaIni.
        Responde:SCREEN-VALUE IN FRAME F_Filtros = "".
        NomResponde:SCREEN-VALUE IN FRAME F_Filtros = "".
    END.
    OTHERWISE
        DISABLE F-FecIni
               F-FecFin
               CuentaIni
               NomCtaIni
               Responde
               NomResponde.
  END CASE.
/*   F-FecIni:SCREEN-VALUE IN FRAME F_Filtros = "".                                                         */
/*   F-FecFin:SCREEN-VALUE IN FRAME F_Filtros = "".                                                         */
/*   NomCtaIni:SCREEN-VALUE IN FRAME F_Filtros = "".                                                        */
/*   CuentaIni:SCREEN-VALUE IN FRAME F_Filtros = "".                                                        */
/*   IF RTipo:SCREEN-VALUE EQ "3" AND Cmb_Proveedores:SCREEN-VALUE EQ "Descuentos por Pronto Pago" THEN DO: */
/*       CuentaIni:SENSITIVE IN FRAME F_Filtros = NO.                                                       */
/*       CuentaIni:HIDDEN IN FRAME F_Filtros = YES.                                                         */
/*       NomCtaIni:HIDDEN IN FRAME F_Filtros = YES.                                                         */
/*   END.                                                                                                   */
  VIEW FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME F_Basicos /* Imprimir */
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


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Basicos /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      PUBLISH "wrActivosFijosActvarVntna".
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF  
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
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Basicos /* Button 1 */
DO:
  RUN W-InfDia.R NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-145
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-145 wWin
ON CHOOSE OF BUTTON-145 IN FRAME F_Filtros /* Button 145 */
DO:
  HIDE FRAME F_Filtros.
  ENABLE {&List-1} WITH FRAME F_Basicos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
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


&Scoped-define SELF-NAME CuentaIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CuentaIni wWin
ON LEAVE OF CuentaIni IN FRAME F_Filtros /* Cuenta */
DO:
/*   FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.               */
/*   IF AVAILABLE Cuentas THEN NomCtaIni:SCREEN-VALUE IN FRAME F_Filtros = Cuentas.Nombre.  */
/*   ELSE DO:                                                                               */
/*      RUN C-Cuentas.r (OUTPUT P_Cuenta, OUTPUT P_Nombre, OUTPUT P_NatTra,                 */
/*                       OUTPUT P_CtrNat, INPUT "T").                                       */
/*      ASSIGN SELF:SCREEN-VALUE = P_Cuenta                                                 */
/*             NomCtaIni:SCREEN-VALUE = P_Nombre.                                           */
/*   END.                                                                                   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Responde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Responde wWin
ON LEAVE OF Responde IN FRAME F_Filtros
DO:
  FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN DO:
     NomResponde:SCREEN-VALUE IN FRAME F_Filtros = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  END.

/*   ELSE DO:                                                                */
/*      RUN C-Cuentas.r (OUTPUT P_Cuenta, OUTPUT P_Nombre, OUTPUT P_NatTra,  */
/*                       OUTPUT P_CtrNat, INPUT "T").                        */
/*      ASSIGN SELF:SCREEN-VALUE = P_Cuenta                                  */
/*             NomCtaIni:SCREEN-VALUE = P_Nombre.                            */
/*   END.                                                                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME RTipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RTipo wWin
ON VALUE-CHANGED OF RTipo IN FRAME F_Basicos
DO:
DO WITH FRAME F_Basicos:
  CASE SELF:SCREEN-VALUE:
    WHEN "1" THEN DO:
      Cmb_Diferidos:HIDDEN = YES.
      Cmb_Proveedores:HIDDEN = YES.
      Cmb_Activos:HIDDEN = NO.
      Cmb_Activos:SCREEN-VALUE = Cmb_Activos:ENTRY(1).
      CuentaIni:SENSITIVE IN FRAME F_Filtros = NO.
      CuentaIni:HIDDEN IN FRAME F_Filtros = YES.
      NomCtaIni:HIDDEN IN FRAME F_Filtros = YES.
    END.
    WHEN "2" THEN DO:
      Cmb_Diferidos:HIDDEN = NO.
      Cmb_Proveedores:HIDDEN = YES.
      Cmb_Activos:HIDDEN = YES.
      Cmb_Diferidos:SCREEN-VALUE = Cmb_Diferidos:ENTRY(1).
      CuentaIni:SENSITIVE IN FRAME F_Filtros = NO.
      CuentaIni:HIDDEN IN FRAME F_Filtros = YES.
      NomCtaIni:HIDDEN IN FRAME F_Filtros = YES.
    END.
    WHEN "3" THEN DO:
      Cmb_Diferidos:HIDDEN = YES.
      Cmb_Proveedores:HIDDEN = NO.
      Cmb_Activos:HIDDEN = YES.
      Cmb_Proveedores:SCREEN-VALUE = Cmb_Proveedores:ENTRY(1).
      CuentaIni:SENSITIVE IN FRAME F_Filtros = YES.
      CuentaIni:HIDDEN IN FRAME F_Filtros = NO.
      NomCtaIni:HIDDEN IN FRAME F_Filtros = NO.
    END.
  END CASE.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY RTipo Cmb_Activos E1 
      WITH FRAME F_Basicos IN WINDOW wWin.
  ENABLE RECT-176 RECT-5 RTipo Cmb_Activos Cmb_Diferidos BUTTON-1 Btn_Filtros 
         Btn_Imprimir BUTTON-153 Btn_Ejecutar E1 Btn_Salir Btn_Ayuda 
      WITH FRAME F_Basicos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Basicos}
  DISPLAY Buscar 
      WITH FRAME F_Buscar IN WINDOW wWin.
  ENABLE Buscar BUTTON-154 Btn_anterior Btn_siguiente 
      WITH FRAME F_Buscar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Buscar}
  DISPLAY Cmb_Agencias F-FecIni F-FecFin Responde NomResponde 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE RECT-284 Cmb_Agencias BUTTON-145 F-FecIni F-FecFin Responde 
         NomResponde 
      WITH FRAME F_Filtros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Filtros}
  ENABLE IMAGE-2 R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW wWin.
  VIEW FRAME F_Progreso IN WINDOW wWin.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE General_Diferidos wWin 
PROCEDURE General_Diferidos :
FOR EACH Diferido WHERE 
         Diferido.Agencia GE AgeIni AND
         Diferido.Agencia LE AgeFin NO-LOCK BREAK BY Diferido.Agencia BY Diferido.Cod_Producto BY Diferido.Codigo:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Diferido.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Diferido.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN
           DISPLAY SKIP(1)
                   "--------------------------------------------------------------------------------------------------------------------------------" AT 1
                   "Agencia:"     AT 1
                   Agencias.Agencia AT 10 FORMAT "999"
                   CAPS(Agencias.Nombre)  AT 17 FORMAT "X(50)"
           WITH FRAME TAA WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      END.
      IF FIRST-OF(Diferido.Cod_Producto) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 11 AND Varios.Codigo EQ Diferido.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN
            DISPLAY "-------------------------------------------------------------------------------------------------------------------------------" AT 1
                    "Grupo : "         AT 1
                    Varios.Codigo      AT 10 FORMAT "99999"
                    CAPS(Varios.Descripcion) AT 17 FORMAT "X(50)"
                    "--------------------------------------------------------------------------------------------------------------------------------" AT 1
            WITH FRAME TVA WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      END.
      DISPLAY Diferido.Codigo           AT 1 FORMAT "999999999"
              Diferido.Descripcion      AT 13 FORMAT "X(28)"
              Diferido.Fec_Compra       AT 45 FORMAT "99/99/99"
              Diferido.Nit              AT 55 FORMAT "X(14)"
              Diferido.Plazo            AT 71 FORMAT "999"
              Diferido.Cuota            AT 76 FORMAT ">>,>>>,>>9"
              Diferido.Nro_Factura      AT 88 FORMAT "X(10)"
              Diferido.Costo            AT 100 FORMAT "->>>,>>>,>>9"
              Diferido.Saldo            AT 117 FORMAT "->>>,>>>,>>9"
      WITH FRAME DDA1 WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
      ASSIGN TotA1 = TotA1 + Diferido.Costo
             TotA2 = TotA2 + Diferido.Saldo
             TotT1 = TotT1 + Diferido.Costo
             TotT2 = TotT2 + Diferido.Saldo.
      IF LAST-OF(Diferido.Agencia) THEN DO:
         DISPLAY "Totales Agencia: " AT 20
                 Diferido.Agencia    AT 40
                 TotA1               AT 99
                 TotA2               AT 116
         WITH FRAME TA1 WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
         ASSIGN TotA1 = 0 TotA2 = 0.
      END.
  END.
  DISPLAY SKIP "Total : "  AT 20
               TotT1       AT 99
               TotT2       AT 116
  WITH FRAME FRD WIDTH 135 NO-LABELS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
MESSAGE "Opción no disponible" VIEW-AS ALERT-BOX INFORMATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Activos wWin 
PROCEDURE Informes_Activos :
Listado = /*W_PathSpl +*/ "c:\info\Clientes.LST".  
OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" INITIAL "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        ".
DEFINE VAR TotReg AS DECIMAL.
OUTPUT TO value(Listado) PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : " + Cmb_Activos:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_Activos:
    WHEN "Relación de Activos" THEN
        W_EncColumna = "Codigo     C.C          Nombre                            Estado          Fec.Compra       Valor.Compra  Nro.Factura    Proveedor      Responsable    Años   Peri.   Deprec.Mes    Sdo.Depreciacion".
    WHEN "Resumen por Grupos de Activos" THEN  
        W_EncColumna = "Cod.Grupo   Nombre                                        Sdo.Depre        Val.Compra".
    WHEN "Listado de Responsables" THEN  
                    /*           1         2         3         4         5         6         7         8         9         0         1         2
                        123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
        W_EncColumna = "Codigo     C.C          Nombre                            Fec.Asigna".
/*         W_EncColumna = "Codigo C.C Nombre                           Fec.Comp  Nit.Proveedor   NroFactura  Sdo.Deprecia   Valor.Compra". */
  END CASE.
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
  CASE Cmb_Activos:
     WHEN "Relación de Activos" THEN RUN Relacion_Activos.
     WHEN "Resumen por Grupos de Activos" THEN RUN Resumen_Activos.
     WHEN "Listado de Responsables" THEN RUN Responsables_Activos.
  END CASE.

/*   FORM                                           */
/*     vcCodAge    FORMAT "X(20)"                   */
/*     vdTotCompra FORMAT "->>>,>>>,>>>,>>9.99"     */
/*     vdTotDepre  FORMAT "->>>,>>>,>>>,>>9.99"     */
/*   WITH FRAME FAgeTotG NO-BOX NO-LABEL WIDTH 80.  */
/*                                                  */
/*   DISPLAY "Total General: " @ vcCodAge           */
/*           TotT2 @ vdTotCompra                    */
/*           TotT1 @ vdTotDepre                     */
/*     WITH FRAME FAgeTotG.                         */
/*   DOWN WITH FRAME FAgeTotG.                      */

/*   DISPLAY SKIP "Total : "  AT 20      */
/*                TotT1       AT 100     */
/*                TotT2       AT 117     */
/*   WITH FRAME FT WIDTH 135 NO-LABELS.  */
  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Diferidos wWin 
PROCEDURE Informes_Diferidos :
Listado = /*W_PathSpl +*/ "c:\info\Clientes.LST".  
OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" INITIAL "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        ".
DEFINE VAR TotReg AS DECIMAL.
OUTPUT TO value(Listado) PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : " + Cmb_Activos:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_Diferidos:
    WHEN "Listado General de Diferidos" THEN  
        W_EncColumna = "Codigo     Nombre                           Fec.Comp  Nit              Plazo  Cuota    NroFactura  Sdo.Deprecia   Valor.Compra".
    WHEN "Resumen por Grupos" THEN  
        W_EncColumna = "Cod.Grupo   Nombre                                        Costo Difer.     Sdo.Difer.".
  END CASE.
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
  CASE Cmb_Diferidos:
     WHEN "Listado General de Diferidos" THEN RUN General_Diferidos.
     WHEN "Resumen por Grupos" THEN RUN Resumen_Diferidos.
  END CASE.


  VIEW FRAME F-Ftr.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informes_Proveedores wWin 
PROCEDURE Informes_Proveedores :
Listado = /*W_PathSpl +*/ "c:\info\cxc.LST".  
OS-DELETE VALUE(Listado).
DEFINE VAR valor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR NomPro AS CHARACTER FORMAT "X(25)".
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" INITIAL "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        ".
 
DEFINE VAR TotReg AS DECIMAL.
OUTPUT TO value(Listado) PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : " + Cmb_Proveedores:SCREEN-VALUE IN FRAME F_Basicos
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  CASE Cmb_Proveedores:
                    /*           1         2         3         4         5         6         7         8         9         0         1         2
                        123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
    WHEN "Causaciones" THEN  
        W_EncColumna = "Nit                Nombre Proveedor       DocRefer       FecUlPago          Val.Inicial      Debito       Credito        Restante".
    WHEN "Descuentos por Pronto Pago" THEN  
        W_EncColumna = "Nit                Nombre Proveedor                   DocRefer       FecPronto      Porc          ValFactura      ValDescuento".
    WHEN "Pagos" THEN  
        W_EncColumna = "                   Nro.Factura    Fec.Pago            Debito                   Credito".
  END CASE.
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
CASE Cmb_Proveedores:
  WHEN "Descuentos por Pronto Pago" THEN RUN InfPro_DescuentosPronto.
  WHEN "Pagos" THEN RUN InfPro_Pagos.
  WHEN "Causaciones" THEN RUN InfPro_Causaciones.
END CASE.
IF TotT1 GT 0 OR TotT2 GT 0 THEN
  DISPLAY SKIP "Total : "  AT 20
               TotT1       AT 100
               TotT2       AT 117
  WITH FRAME FT WIDTH 135 NO-LABELS.
  VIEW FRAME F-Ftr.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfPro_Causaciones wWin 
PROCEDURE InfPro_Causaciones :
DEFINE VAR NomPro AS CHARACTER FORMAT "X(25)".
DEFINE VAR valor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR CtaIni LIKE Cuentas.Cuenta.
DEFINE VAR CtaFin LIKE Cuentas.Cuenta.
ASSIGN FRAME F_Filtros CuentaIni.
IF CuentaIni EQ "" THEN ASSIGN CtaIni = "00000000000000" CtaFin = "99999999999999".
ELSE ASSIGN CtaIni = CuentaIni CtaFin = CtaIni.
  
  FOR EACH Detalle WHERE 
           Detalle.Agencia GE AgeIni AND
           Detalle.Agencia LE AgeFin AND 
           Detalle.Cuenta  GE CtaIni AND 
           Detalle.Cuenta  LE CtaFin
           NO-LOCK BREAK BY Detalle.Agencia BY Detalle.Cuenta:  
        j = j + 1.
        RUN Progreso.
            IF FIRST-OF(Detalle.Agencia) THEN DO:
               FIND Agencias WHERE Agencias.Agencia EQ Detalle.Agencia NO-LOCK NO-ERROR.
               IF AVAILABLE Agencias THEN
                 DISPLAY SKIP(1)
                         "********************************************************************************************************************************" AT 1
                         "Agencia:"     AT 1
                         Agencias.Agencia AT 10 FORMAT "999"
                         CAPS(Agencias.Nombre)  AT 17 FORMAT "X(50)"
                         "********************************************************************************************************************************" AT 1
                 WITH FRAME TTCAAge WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
            END.
            IF FIRST-OF(Detalle.Cuenta) THEN DO:
               FIND Cuentas WHERE Cuentas.Cuenta EQ Detalle.Cuenta NO-LOCK NO-ERROR.
               IF AVAILABLE Cuentas THEN
                 DISPLAY SKIP(1)
                         "Cuenta:"     AT 1
                         Cuentas.Cuenta AT 10 FORMAT "X(14)"
                         CAPS(Cuentas.Nombre)  AT 30 FORMAT "X(50)"
                 WITH FRAME TTCACta WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
            END.
            NomPro = "No existe en clientes".
            FIND Clientes WHERE Clientes.Nit EQ Detalle.Nit NO-LOCK NO-ERROR.
            IF AVAILABLE Clientes THEN NomPro = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
            IF Cuentas.Naturaleza EQ "db" THEN
               Valor = Detalle.Db - Detalle.Cr.
            ELSE
               Valor = Detalle.Cr - Detalle.Db.
            DISPLAY Detalle.Nit             AT 1
                    NomPro                  AT 17 FORMAT "X(25)"
                    Detalle.Doc_referencia  AT 45
                    Detalle.Fec_ultActualizacion AT 57
                    Detalle.Valor_Inicial   AT 70 FORMAT ">,>>>,>>>,>>9"
                    Detalle.Db              AT 85 FORMAT ">,>>>,>>>,>>9"
                    Detalle.Cr              AT 100 FORMAT ">,>>>,>>>,>>9"
                    Valor                   AT 115  FORMAT "->,>>>,>>>,>>9"
            WITH FRAME DAA1 WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
            ASSIGN TotA1 = TotA1 + Detalle.Valor_Inicial
                   TotA2 = TotA2 + Detalle.Db
                   TotA3 = TotA3 + Detalle.Cr
                   TotA4 = TotA4 + Valor
                   Tot1  = Tot1 + Detalle.Valor_Inicial 
                   Tot2  = Tot2 + Detalle.Db            
                   Tot3  = Tot3 + Detalle.Cr            
                   Tot4  = Tot4 + Valor.
            IF LAST-OF(Detalle.Cuenta) THEN DO:
               DISPLAY SKIP(1) "TOTAL CUENTA: " AT 20
                       Detalle.Cuenta      AT 40
                       Tot1               AT 70
                       Tot2               AT 85
                       Tot3               AT 100
                       Tot4               AT 115
                       "-------------------------------------------------------------------------------------------------------------------------------" AT 1
               WITH FRAME TTCta WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
               ASSIGN Tot1 = 0 Tot2 = 0 Tot3 = 0 Tot4 = 0.
            END.
            IF LAST-OF(Detalle.Agencia) THEN DO:
               DISPLAY SKIP(1) "Totales Agencia: " AT 20
                       Detalle.Agencia     AT 40
                       TotA1               AT 70
                       TotA2               AT 85
                       TotA3               AT 100
                       TotA4               AT 115
               WITH FRAME TTAge WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
               ASSIGN TotA1 = 0 TotA2 = 0 TotA3 = 0 TotA4 = 0.
            END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfPro_DescuentosPronto wWin 
PROCEDURE InfPro_DescuentosPronto :
DEFINE VAR NomPro AS CHARACTER FORMAT "X(25)".
DEFINE VAR NomCta AS CHARACTER FORMAT "X(50)".
DEFINE VAR valor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
  
  FOR EACH Detalle WHERE 
           Detalle.Agencia GE AgeIni AND
           Detalle.Agencia LE AgeFin AND 
           Detalle.Fec_ProntoPago NE ? AND
           Detalle.Fec_ProntoPago GE FecIni AND 
           Detalle.Fec_ProntoPago LE FecFin
           NO-LOCK BREAK BY Detalle.Agencia BY Detalle.Cuenta:  
        j = j + 1.
        RUN Progreso.
        IF FIRST-OF(Detalle.Agencia) THEN DO:
           FIND Agencias WHERE Agencias.Agencia EQ Detalle.Agencia NO-LOCK NO-ERROR.
           IF AVAILABLE Agencias THEN DO:
             DISPLAY SKIP(1)
                     "********************************************************************************************************************************" AT 1
                     "AGENCIA:"     AT 1
                     Agencias.Agencia AT 10 FORMAT "999"
                     CAPS(Agencias.Nombre)  AT 17 FORMAT "X(50)" 
                     "********************************************************************************************************************************" AT 1
             WITH FRAME TTPPAge WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
           END.
        END.
        IF FIRST-OF(Detalle.Cuenta) THEN DO:
           FIND Cuentas WHERE Cuentas.Cuenta EQ Detalle.Cuenta NO-LOCK NO-ERROR.
           IF AVAILABLE Cuentas THEN DO:
             NomCta = TRIM(Cuentas.Cuenta) + " - " + CAPS(Cuentas.Nombre).
             DISPLAY "        CUENTA  :"     AT 1
                     NomCta                AT 20 SKIP(1)
             WITH FRAME TTPPCta WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
           END.
        END.
        NomPro = "No existe en clientes".
        FIND Clientes WHERE Clientes.Nit EQ Detalle.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN NomPro = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        Valor = Detalle.Valor_Inicial - ((Detalle.Por_ProntoPago / 100) * Detalle.Valor_inicial).
        DISPLAY Detalle.Nit             AT 1
                NomPro                  AT 20
                Detalle.Doc_referencia  AT 55
                Detalle.Fec_ProntoPago  AT 70
                Detalle.Por_ProntoPago  AT 85
                Detalle.Valor_Inicial   AT 93
                Valor                   AT 110
        WITH FRAME DTPP WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
        ASSIGN TotA1 = TotA1 + Detalle.Valor_Inicial
               TotA2 = TotA2 + Valor
               Tot1  = Tot1  + Detalle.Valor_Inicial
               Tot2  = Tot2  + Valor.
        IF LAST-OF(Detalle.Cuenta) THEN DO:
           DISPLAY "     TOT.CUENTA: " AT 1
                   NomCta     AT 20
                   Tot1               AT 96
                   Tot2               AT 113
                   "--------------------------------------------------------------------------------------------------------------------------------" AT 1
           WITH FRAME TotPPCta WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
           ASSIGN Tot1 = 0 Tot2 = 0.
        END.
         
        IF LAST-OF(Detalle.Agencia) THEN DO:
           DISPLAY SKIP(1)
                   "Totales Agencia: " AT 1
                   Detalle.Agencia     AT 20
                   TotA1               AT 96
                   TotA2               AT 113
           WITH FRAME TotPPAge WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
           ASSIGN TotA1 = 0 TotA2 = 0.
        END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfPro_Pagos wWin 
PROCEDURE InfPro_Pagos :
DEFINE VAR NomPro AS CHARACTER FORMAT "X(25)".
DEFINE VAR valor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR CtaIni LIKE Cuentas.Cuenta.
DEFINE VAR CtaFin LIKE Cuentas.Cuenta.
ASSIGN FRAME F_Filtros CuentaIni.
IF CuentaIni EQ "" THEN ASSIGN CtaIni = "00000000000000" CtaFin = "99999999999999".
ELSE ASSIGN CtaIni = CuentaIni CtaFin = CtaIni.
  
  FOR EACH Detalle WHERE 
           Detalle.Agencia GE AgeIni AND
           Detalle.Agencia LE AgeFin AND 
           Detalle.Cuenta  GE CtaIni AND 
           Detalle.Cuenta  LE CtaFin AND
           Detalle.Fec_UltActualizacion NE ? AND
           Detalle.Fec_UltActualizacion GE FecIni AND 
           Detalle.Fec_UltActualizacion LE FecFin NO-LOCK BREAK BY Detalle.Agencia BY Detalle.Nit BY Detalle.Cuenta:  
         FIND FIRST Mov_Contable WHERE
                 Mov_Contable.Cuenta EQ Detalle.Cuenta AND
                 Mov_Contable.Nit    EQ Detalle.Nit    AND
                 Mov_Contable.Doc_Referencia EQ Detalle.Doc_referencia AND
                 Mov_Contable.Fec_Contable GE FecIni AND
                 Mov_Contable.Fec_Contable LE FecFin NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Mov_Contable THEN NEXT.
        j = j + 1.
        RUN Progreso.
        IF FIRST-OF(Detalle.Agencia) THEN DO:
           FIND Agencias WHERE Agencias.Agencia EQ Detalle.Agencia NO-LOCK NO-ERROR.
           IF AVAILABLE Agencias THEN
             DISPLAY "********************************************************************************************************************************" AT 1
                     "AGENCIA :"     AT 1
                     Agencias.Agencia     AT 14 FORMAT "999"
                     CAPS(Agencias.Nombre)  AT 40 FORMAT "X(50)"
                     "********************************************************************************************************************************" AT 1
            WITH FRAME TTPGAge WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
        END.
        IF FIRST-OF(Detalle.Nit) THEN DO:
           FIND Clientes WHERE Clientes.Nit EQ Detalle.Nit NO-LOCK NO-ERROR.
           IF AVAILABLE Clientes THEN
             DISPLAY "--------------------------------------------------------------------------------------------------------------------------------" AT 1
                     "Proveedor:"     AT 1
                     Clientes.Nit     AT 14 FORMAT "X(14)"
                     CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)  AT 40 FORMAT "X(50)"
             WITH FRAME TTPGPro WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
        END.
        IF FIRST-OF(Detalle.Cuenta) THEN DO:
           FIND Cuentas WHERE Cuentas.Cuenta EQ Detalle.Cuenta NO-LOCK NO-ERROR.
           IF AVAILABLE Cuentas THEN
             DISPLAY "Cuenta:"       AT 1
                     Cuentas.Cuenta  AT 14 FORMAT "X(14)"
                     CAPS(Cuentas.Nombre)  AT 40 FORMAT "X(50)"
             WITH FRAME TTPGCta WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
        END.
        FOR EACH Mov_Contable WHERE
                 Mov_Contable.Cuenta EQ Detalle.Cuenta AND
                 Mov_Contable.Nit    EQ Detalle.Nit    AND
                 Mov_Contable.Doc_Referencia EQ Detalle.Doc_referencia AND
                 Mov_Contable.Fec_Contable GE FecIni AND
                 Mov_Contable.Fec_Contable LE FecFin NO-LOCK BREAK BY Mov_Contable.Doc_Referencia:
            Valor = Detalle.Valor_Inicial - ((Detalle.Por_ProntoPago / 100) * Detalle.Valor_inicial).
            DISPLAY Mov_Contable.Doc_Referencia  AT 17
                    Mov_Contable.Fec_Contable    AT 35
                    Mov_Contable.Db              AT 50
                    Mov_Contable.Cr              AT 75
            WITH FRAME DAA1 WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
            ASSIGN TotA1 = TotA1 + Mov_Contable.Db
                   TotA2 = TotA2 + Mov_Contable.Cr
                   Tot1  = Tot1  + Mov_Contable.Db
                   Tot2  = Tot2  + Mov_Contable.Cr.
            IF LAST-OF(Mov_Contable.Doc_Referencia) THEN DO:
               DISPLAY  "----------------------------------------------------------------------------" AT 15 SKIP(1)
                        "Total Factura: " AT 15
                        Detalle.Doc_Referencia AT 35
                        TotA1            AT 53
                        TotA2            AT 78 SKIP(1)
               WITH FRAME TotFac WIDTH 132 USE-TEXT NO-BOX NO-LABELS.
               ASSIGN TotA1 = 0 TotA2 = 0.
            END.
        END.
        IF LAST-OF(Detalle.Agencia) THEN DO:
          DISPLAY  "----------------------------------------------------------------------------" AT 15 SKIP(1)
                   "TOTAL AGENCIA: " AT 15
                   Detalle.Agencia   AT 35
                   Tot1              AT 53
                   Tot2              AT 78 SKIP(1)
          WITH FRAME TotAge WIDTH 132 USE-TEXT NO-BOX NO-LABELS.
          ASSIGN Tot1 = 0 Tot2 = 0.
        END.
  END.
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
  DO WITH FRAME F_Filtros:
    FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
      W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
    END.
    ASSIGN 
           AgeIni = W_Agencia
           AgeFin = W_Agencia
           FecIni = TODAY
           FecFin = TODAY.
   END.
  
HIDE Cmb_Diferidos IN FRAME F_Basicos.
HIDE Cmb_Proveedores IN FRAME F_Basicos.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Relacion_Activos wWin 
PROCEDURE Relacion_Activos :
FOR EACH Act_Fijo WHERE 
         Act_Fijo.Agencia GE AgeIni    AND
         Act_Fijo.Agencia LE AgeFin    AND
         Act_Fijo.Fec_Compra GE FecIni AND
         Act_Fijo.Fec_Compra LE FecFin
         NO-LOCK BREAK BY Act_Fijo.Agencia BY Act_Fijo.Grupo BY Act_Fijo.Codigo:  
      j = j + 1.
      ASSIGN vcCodigo    = act_fijo.codigo
             vcNomAct    = act_fijo.nombre
             vdFecCompra = act_fijo.fec_compra
             vcFactura   = act_fijo.Nro_Factura.

      CASE Act_Fijo.Estado:
         WHEN 1 THEN ASSIGN vcEstado = "1-Activo".
         WHEN 2 THEN ASSIGN vcEstado = "2-Retirado".
         WHEN 3 THEN ASSIGN vcEstado = "3-Depreciado".
         OTHERWISE ASSIGN vcEstado = "0-Desconocido".
      END CASE.

      FORM
        vcCodigo                  /* COLUMN-LABEL "Código"       */  FORMAT "X(10)"
        Act_Fijo.Cen_Costos       /* COLUMN-LABEL "C.Costos"     */  FORMAT "999"
/*         Act_Fijo.Nombre           /* COLUMN-LABEL "Nombre"       */  FORMAT "X(10)" */
        vcNomAct                  /* COLUMN-LABEL "Nombre"       */  FORMAT "X(40)"
        vcEstado                                                     FORMAT "X(15)"
        vdFecCompra                                                  FORMAT "99/99/9999"
        Act_Fijo.Val_Compra       /* COLUMN-LABEL "Val.Compra"   */  FORMAT "->>,>>>,>>>,>>9.99"
        vcFactura                                                    FORMAT "X(15)"
/*         Act_Fijo.Nro_Factura      /* COLUMN-LABEL "Nro.Factura"  */  FORMAT "X(10)" */
        Act_Fijo.Nit_Proveedor    /* COLUMN-LABEL "Nit.Provee."  */  FORMAT "X(14)"
        Act_Fijo.Nit_Responsable  /* COLUMN-LABEL "Nit.Respons." */  FORMAT "X(14)"
        Act_Fijo.Anos_Adepreciar                                     FORMAT "ZZZZ"
        Act_Fijo.Per_Depreciado                                      FORMAT "ZZZZ"
        Act_Fijo.ValDepMes        /* COLUMN-LABEL "Sdo.Deprecia" */  FORMAT "->>,>>>,>>9.99"
        Act_Fijo.Sdo_Depre        /* COLUMN-LABEL "Sdo.Deprecia" */  FORMAT "->>,>>>,>>>,>>9.99"

        WITH FRAME FRep DOWN COLUMN 1 WIDTH 210
            NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT NO-LABEL STREAM-IO.
/*       RUN Progreso.  */
          IF FIRST-OF(Act_Fijo.Agencia) THEN DO:
             FIND Agencias WHERE Agencias.Agencia EQ Act_Fijo.Agencia NO-LOCK NO-ERROR.
             IF AVAILABLE Agencias THEN
               FORM                                          
                 vcNom   FORMAT "X(60)"
                 WITH FRAME FAge NO-BOX NO-LABEL WIDTH 60.
               DISPLAY ("Agencia: " + (STRING(Agencias.Agencia,"999")) + " - "+ CAPS(Agencias.Nombre)) @ vcNom  
                 WITH FRAME FAge.                                                     
               DOWN WITH FRAME FAge.
          END.
          IF FIRST-OF(Act_Fijo.Grupo) THEN DO:
             FIND Varios WHERE Varios.Tipo EQ 7 AND Varios.Codigo EQ Act_Fijo.Grupo NO-LOCK NO-ERROR.
             IF AVAILABLE Varios THEN
                FORM                                          
                  vcNom   FORMAT "X(50)"
                  WITH FRAME FAge NO-BOX NO-LABEL WIDTH 60.
                DISPLAY ("Grupo: " + (STRING(Varios.Codigo,"99999")) + " - "+ CAPS(Varios.Descripcion)) @ vcNom         
                  WITH FRAME FAge.                                                     
                DOWN WITH FRAME FAge.
          END.

          DISPLAY vcCodigo                   /* /* AT 1   */  FORMAT "X(10)"         */
                  Act_Fijo.Cen_Costos       /* /* AT 12  */  FORMAT "999"           */
                  vcNomAct                  
                  vcEstado
/*                   Act_Fijo.Nombre           FORMAT "X(30)"/* /* AT 17  */  FORMAT "X(28)"         */  */
                  vdFecCompra
/*                   Act_Fijo.Fec_Compra       /* /* AT 49  */  FORMAT "99/99/9999"    */  */
                  Act_Fijo.Val_Compra       /* /* AT 123 */  FORMAT "->>>,>>>,>>9"  */
                  vcFactura
/*                   Act_Fijo.Nro_Factura      /* /* AT 94  */  FORMAT "X(10)"         */ */
                  Act_Fijo.Nit_Proveedor    /* /* AT 61  */  FORMAT "X(14)"         */
                  Act_Fijo.Nit_Responsable  /* /* AT 78  */  FORMAT "X(14)"         */
                  Act_Fijo.Anos_Adepreciar
                  Act_Fijo.Per_Depreciado 
                  Act_Fijo.Sdo_Depre        /* /* AT 106 */  FORMAT "->>>,>>>,>>9"  */
                  Act_Fijo.ValDepMes
             WITH FRAME FRep.
          DOWN WITH FRAME FRep.

          ASSIGN TotA1 = TotA1 + Act_Fijo.Sdo_Depre 
                 TotA2 = TotA2 + Act_Fijo.Val_Compra
                 TotT1 = TotT1 + Act_Fijo.Sdo_Depre
                 TotT2 = TotT2 + Act_Fijo.Val_Compra.
          IF LAST-OF(Act_Fijo.Grupo) THEN DO:
             DISPLAY SKIP(1) 
                WITH FRAME FAge.                                                     
             DOWN WITH FRAME FAge.
          END.

/*           IF LAST-OF(Act_Fijo.Agencia) THEN DO:                                       */
/*              FORM                                                                     */
/*                vcCodAge    FORMAT "X(20)"                                             */
/*                vdTotCompra FORMAT "->>>,>>>,>>>,>>9.99"                               */
/*                vdTotDepre  FORMAT "->>>,>>>,>>>,>>9.99"                               */
/*              WITH FRAME FAgeTot NO-BOX NO-LABEL WIDTH 60.                             */
/*                                                                                       */
/*              DISPLAY ("Total Agencia: " + STRING(Act_Fijo.Agencia,"999")) @ vcCodAge  */
/*                      TotA2 @ vdTotCompra                                              */
/*                      TotA1 @ vdTotDepre                                               */
/*                WITH FRAME FAgeTot.                                                    */
/*              DOWN WITH FRAME FAgeTot.                                                 */
/*                                                                                       */
/* /*              DISPLAY "Totales Agencia: " AT 20                             */      */
/* /*                      Act_Fijo.Agencia    AT 40                             */      */
/* /*                      TotA1               AT 99                             */      */
/* /*                      TotA2               AT 116                            */      */
/* /*              WITH FRAME TA1 WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO. */      */
/*              ASSIGN TotA1 = 0 TotA2 = 0.                                              */
/*           END.                                                                        */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Responsables_Activos wWin 
PROCEDURE Responsables_Activos :
FOR EACH Act_Fijo WHERE 
         Act_Fijo.Agencia GE AgeIni    AND
         Act_Fijo.Agencia LE AgeFin    AND
/*          Act_Fijo.Fec_Compra GE FecIni AND  */
/*          Act_Fijo.Fec_Compra LE FecFin AND  */
         Act_Fijo.Nit_Responsable      GE vcResActIni AND
         Act_Fijo.Nit_Responsable      LE vcResActFin
           NO-LOCK BREAK BY Act_Fijo.Nit_Responsable BY Act_Fijo.Grupo:  
      j = j + 1.
      RUN Progreso.
      ASSIGN vcCodigo    = act_fijo.codigo
             vcNomAct    = act_fijo.nombre
             vdFecCompra = act_fijo.fec_asignacion.
   
      ASSIGN vdFecCompra = TODAY.

      IF FIRST-OF(Act_Fijo.Nit_Responsable) THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Act_Fijo.Nit_Responsable NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN
            DISPLAY "-------------------------------------------------------------------------------------------------------------------------------" AT 1
                    "Responsable : "   AT 1
                    Clientes.Nit       AT 15 FORMAT "X(14)"
                    CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2) AT 35 FORMAT "X(50)"
            WITH FRAME TVA WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
         ELSE
            DISPLAY "-------------------------------------------------------------------------------------------------------------------------------" AT 1
                    "Responsable : "   AT 1
                    Clientes.Nit       AT 15 FORMAT "X(14)"
                    "No Existe en Clientes" AT 35 FORMAT "X(50)"
            WITH FRAME TVA WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      END.
      IF FIRST-OF(Act_Fijo.Grupo) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 7 AND Varios.Codigo EQ Act_Fijo.Grupo NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN
            DISPLAY "-------------------------------------------------------------------------------------------------------------------------------" AT 1
                    "Grupo : "         AT 1
                    Varios.Codigo      AT 10 FORMAT "99999"
                    CAPS(Varios.Descripcion) AT 17 FORMAT "X(50)"
                    "--------------------------------------------------------------------------------------------------------------------------------" AT 1
            WITH FRAME TVA WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
      END.
      DISPLAY vcCodigo 
              Act_Fijo.Cen_Costos    FORMAT "999"
              vcNomAct 
              vdFecCompra 
/*               Act_Fijo.Nit_Proveedor    AT 55 FORMAT "X(14)"          */
/*               Act_Fijo.Nro_Factura      AT 72 FORMAT "X(10)"          */
/*               Act_Fijo.Sdo_Depre        AT 84 FORMAT "->>>,>>>,>>9"   */
/*               Act_Fijo.Val_Compra       AT 100 FORMAT "->>>,>>>,>>9"  */
      WITH FRAME DAA1 WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
      ASSIGN TotA1 = TotA1 + Act_Fijo.Sdo_Depre
             TotA2 = TotA2 + Act_Fijo.Val_Compra
             TotT1 = TotT1 + Act_Fijo.Sdo_Depre
             TotT2 = TotT2 + Act_Fijo.Val_Compra.
      IF LAST-OF(Act_Fijo.Nit_Responsable) THEN 
         PAGE.

/*       IF LAST-OF(Act_Fijo.Nit_Responsable) THEN DO:  */
/*          DISPLAY "Totales Responsable: " AT 20                          */
/*                  Act_Fijo.Agencia    AT 40                              */
/*                  TotA1               AT 99                              */
/*                  TotA2               AT 116                             */
/*          WITH FRAME TA1 WIDTH 135 USE-TEXT NO-BOX NO-LABELS STREAM-IO.  */
/*          ASSIGN TotA1 = 0 TotA2 = 0.                                    */
/*       END.  */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_Activos wWin 
PROCEDURE Resumen_Activos :
FOR EACH Act_Fijo WHERE 
         Act_Fijo.Agencia GE AgeIni    AND
         Act_Fijo.Agencia LE AgeFin    AND
         Act_Fijo.Fec_Compra GE FecIni AND
         Act_Fijo.Fec_Compra LE FecFin NO-LOCK BREAK BY Act_Fijo.Grupo:  
      j = j + 1.
      RUN Progreso.
      ASSIGN TotA1 = TotA1 + Act_Fijo.Sdo_Depre 
             TotA2 = TotA2 + Act_Fijo.Val_Compra
             TotT1 = TotT1 + Act_Fijo.Sdo_Depre
             TotT2 = TotT2 + Act_Fijo.Val_Compra.
      
      IF LAST-OF(Act_Fijo.Grupo) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 7 AND Varios.Codigo EQ Act_Fijo.Grupo NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN DO:
            DISPLAY Varios.Codigo      AT 1 FORMAT "99999"
                    CAPS(Varios.Descripcion) AT 13 FORMAT "X(30)"
                    TotA1             AT 59
                    TotA2             AT 76
            WITH FRAME TRes WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
            ASSIGN TotA1 = 0 TotA2 = 0.
         END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_Diferidos wWin 
PROCEDURE Resumen_Diferidos :
FOR EACH Diferido WHERE 
         Diferido.Agencia GE AgeIni AND
         Diferido.Agencia LE AgeFin NO-LOCK BREAK BY Diferido.Cod_Producto:  
        
      j = j + 1.
      RUN Progreso.
      ASSIGN TotA1 = TotA1 + Diferido.Costo 
             TotA2 = TotA2 + Diferido.Saldo
             TotT1 = TotT1 + Diferido.Costo 
             TotT2 = TotT2 + Diferido.Saldo.
      IF LAST-OF(Diferido.Cod_Producto) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 11 AND Varios.Codigo EQ Diferido.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN DO:
            DISPLAY Varios.Codigo      AT 1 FORMAT "99999"
                    CAPS(Varios.Descripcion) AT 13 FORMAT "X(30)"
                    TotA1             AT 59
                    TotA2             AT 76
            WITH FRAME TResD WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE STREAM-IO.
            ASSIGN TotA1 = 0 TotA2 = 0.
         END.
      END.
  END.
  DISPLAY SKIP "Total : "  AT 20
               TotT1       AT 59
               TotT2       AT 76
  WITH FRAME FRD WIDTH 135 NO-LABELS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

