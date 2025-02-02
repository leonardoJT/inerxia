&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-Rec_XLibranza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Rec_XLibranza 
CREATE WIDGET-POOL.

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEFINE VAR P_Poliza AS DECIMAL.
DEFINE VAR P_Honora AS DECIMAL.
DEFINE VAR P_Costas AS DECIMAL.
DEFINE VAR P_IMora AS DECIMAL.
DEFINE VAR P_IMorDifC AS DECIMAL.
DEFINE VAR P_IDifCob AS DECIMAL.
DEFINE VAR P_ICte AS DECIMAL.
DEFINE VAR P_IAntic AS DECIMAL.
DEFINE VAR P_Capit AS DECIMAL.
DEFINE VAR P_VlrNoDist AS DECIMAL.
DEFI VAR W_OpAboAho AS INTEGER INITIAL 010301001.

/* oakley */

DEFI VAR W_CodEmp LIKE Empresas.Cod_Empresa INIT 0.
DEFI VAR W_CodEmpI LIKE Empresas.Cod_Empresa INIT 0.
DEFI VAR W_NroPagI LIKE Rec_Nomina.Nro_Pago INIT 0.
DEFI VAR W_Age LIKE Empresas.Agencia.
DEFI VAR W_VrADist LIKE Rec_Nomina.Val_Deduc INIT 0.
DEFI VAR TDist LIKE Rec_Nomina.Val_Deduc INIT 0.
DEFI VAR TTDist LIKE Rec_Nomina.Val_Deduc INIT 0.
DEFI VAR TSdo LIKE Rec_Nomina.Val_Deduc INIT 0.
DEFI VAR TTSdo LIKE Rec_Nomina.Val_Deduc INIT 0.
DEFI VAR TRec LIKE Rec_Nomina.Val_Deduc INIT 0.
DEFI VAR w_arc AS INTEGER INITIAL 0.
DEFI VAR TDif LIKE Rec_Nomina.Val_Deduc INIT 0.
DEFI VAR TTDif LIKE Rec_Nomina.Val_Deduc INIT 0.
DEFI VAR W_ContabDistSi AS DECIMAL INIT 0.
DEFI VAR W_ContabDistNo AS DECIMAL INIT 0.
DEFI VAR TImp AS DECIMAL INIT 0.
DEFI VAR TNoImp AS DECIMAL INIT 0 NO-UNDO.
DEFI VAR NNoImp AS INTEG FORM "999999" INIT 0 NO-UNDO.
DEFI VAR W_RowId AS ROWID.
DEFI VAR W_Inf AS LOG INIT FALSE.
DEFI VAR W_SiContab AS LOG INIT FALSE.
DEFI VAR W_SiCpte AS LOG INIT FALSE.
DEFI VAR W_SiImp AS LOG INIT FALSE.
DEFI VAR W_SiImpF AS LOG INIT FALSE.
DEFINE VARIABLE W_exasoc AS INTEGER INITIAL 0 NO-UNDO.

   /* oakley */

   DEFINE VARIABLE W_sinrema AS INTEGER INITIAL 0 NO-UNDO.
   DEFINE VARIABLE W_estado  AS CHARACTER FORMAT "X(12)" INITIAL "" NO-UNDO.
   DEFINE VARIABLE W_nomcli  AS CHARACTER FORMAT "X(60)" INITIAL "" NO-UNDO.
   DEFINE VARIABLE W_vinculo AS CHARACTER FORMAT "X(25)" INITIAL "" NO-UNDO.
   DEFINE VARIABLE w_fecha1  AS DATE FORMAT "99/99/9999" INITIAL "" NO-UNDO.
   DEFINE VARIABLE w_fecha2  AS DATE FORMAT "99/99/9999" INITIAL "" NO-UNDO.
   DEFINE VARIABLE w_fecha3  AS DATE FORMAT "99/99/9999" INITIAL "" NO-UNDO.
   DEFINE VARIABLE w_totrema AS INTEGER INITIAL 0.
   DEFINE VARIABLE w_grarema AS INTEGER INITIAL 0.
   DEFINE VARIABLE w_conseaho AS INTEGER INITIAL 0.
   /* Nuevo 07/Feb/2008*/
   DEFINE VARIABLE vdfecpago  AS DATE INITIAL ?  NO-UNDO.
   DEFINE VARIABLE vimesini   AS INTEGER INITIAL 0 NO-UNDO.
   DEFINE VARIABLE vimesfin   AS INTEGER INITIAL 0 NO-UNDO.
   DEFINE VARIABLE viano      AS INTEGER INITIAL 0 NO-UNDO.
   /*********************/
/****** Plan de Pagos****/

   DEFI VAR W_Proyectado LIKE Creditos.Sdo_Proyectado.
   DEFI VAR W_Tasa       LIKE Creditos.Tasa.
   DEFI VAR W_PdoLiq     AS INTEG FORM "99".
   DEFI VAR W_DiaPdo     AS INTEG FORM "99".
   DEFI VAR KSdo      LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KInt      LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KCap      LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KIAcu     LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KCAcu     LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KCuo         LIKE Creditos.Cuota.              
   DEFI VAR NN           AS INTEG FORM "99999".
   DEFI VAR CuoFalt      AS INTEG FORM "99999".  
   DEFI VAR PdoTrans     AS INTEG FORM "99999".
   DEFI VAR W_FecIniP    AS DATE.                      
   DEFI VAR xFecIni      AS DATE.                      
   DEFI VAR W_FecTra     AS DATE.
   DEFI VAR W_FecIniCont AS DATE.
   DEFI VAR W_SiPdoTr    AS LOG INIT FALSE.
   DEFINE VARIABLE iax   AS INTEGER.
   DEFINE VARIABLE xtas  LIKE creditos.tasa.
/******************/

   DEFI TEMP-TABLE TPdctos
        FIELD Agen    LIKE Ahorros.Agencia
        FIELD Prior   LIKE Pro_Ahorros.Prioridad
        FIELD FApert  LIKE Ahorros.Fec_Apert
        FIELD TP      AS   CHAR FORM "X(1)"
        FIELD CodP    LIKE Pro_Ahorros.Cod_Ahorro
        FIELD CtaP    LIKE Ahorros.Cue_Ahorro
        FIELD NomP    LIKE Pro_Ahorros.Nom_Produc
        FIELD Cuota   LIKE Ahorros.Cuota
        FIELD CuoOrig LIKE Ahorros.Cuota
        FIELD VrDist  LIKE Ahorros.Cuota
        FIELD VrSdo   LIKE Ahorros.Sdo_Dispon
        FIELD NC      LIKE Rec_Nomina.Num_Cuotas
        FIELD Abonos  LIKE Ahorros.Cuota EXTENT 9
        FIELD Vint  LIKE Creditos.Int_corrientes
              INDEX PF Prior FApert
              INDEX id1 tp codp ctap.
   
   DEFI TEMP-TABLE CopTPdctos LIKE TPdctos
        FIELD NitP  LIKE Ahorros.Nit            
        FIELD NomC  LIKE Clientes.Nombre
        FIELD VRec  LIKE Ahorros.Sdo_Dispon
              INDEX NP NitP Prior
              INDEX Nit NitP.

   DEFI TEMP-TABLE TempCtas
        FIELD Agen   LIKE Ahorros.Agencia
        FIELD TipP   AS CHAR FORM "X(1)"
        FIELD Pto    LIKE Ahorros.Cod_Ahorro
        FIELD CtaPro LIKE Cuentas.Cuenta
        FIELD CtaIng LIKE Cuentas.Cuenta
        FIELD CtaLiq LIKE Cuentas.Cuenta
        FIELD IntAnt LIKE Cuentas.Cuenta
        FIELD IntMor LIKE Cuentas.Cuenta
        FIELD DifCoD LIKE Cuentas.Cuenta
        FIELD DifCoH LIKE Cuentas.Cuenta
        FIELD CtaPol LIKE Cuentas.Cuenta
        FIELD CtaHon LIKE Cuentas.Cuenta
        FIELD CtaCos LIKE Cuentas.Cuenta
        FIELD Oper   LIKE Liqui_Int.Cod_Operacion
        FIELD CtaSyA LIKE Cuentas.Cuenta
        INDEX x3 agen tipp pto .

   DEFI TEMP-TABLE CopMov_Contable LIKE Mov_Contable
        INDEX X1 AGENCIA CUENTA DB
        INDEX x2 agencia cuenta cr.

      DEFI TEMP-TABLE Tmp        
        FIELD Ced   AS CHAR FORM "X(12)"
        FIELD Vr    LIKE ahorros.Sdo_Dispon INIT 0     
        FIELD Nom   AS CHAR FORM "X(40)"
        FIELD Age   AS INTE FORM 999            
        FIELD Err   AS CHAR FORM "X(25)".

      DEFINE TEMP-TABLE tmp-deducc
            FIELD agepro   LIKE Creditos.Agencia
            FIELD cod_emp  AS CHARACTER FORMAT "x(80)" 
            FIELD cedula   AS CHARACTER FORMAT "x(12)"
            FIELD nombre   AS CHARACTER FORMAT "x(60)"
            FIELD nroobli  AS CHARACTER FORMAT "x(16)"
            FIELD nompro   AS CHARACTER FORMAT "X(70)"
            FIELD nrocuo   LIKE creditos.plazo
            FIELD fecdesc  LIKE creditos.fec_desembolso
            FIELD vdeduc   AS DECIMAL   FORMAT ">>>>>>>>>>>>"
            FIELD codnoved AS CHARACTER FORMAT "X(10)"
            INDEX Idxded cedula nroobli.

      DEFINE TEMP-TABLE Tmp_SinRema
          FIELD agencia  LIKE rec_nomina.agencia
          FIELD cedula   AS CHARACTER FORMAT "x(12)"
          FIELD codempre LIKE rec_nomina.cod_empresa
          FIELD valdist  AS DECIMAL   FORMAT ">>>>>>>>>>>>"
          INDEX IdxCed cedula.

      DEFINE TEMP-TABLE tmp-exasoc
            FIELD cod_age  LIKE clientes.agencia
            FIELD nit      AS CHARACTER FORMAT "x(12)"
            FIELD nombre   AS CHARACTER FORMAT "x(60)"
            FIELD codempre LIKE clientes.cod_empresa
            FIELD estado   LIKE clientes.estado
            FIELD fecing   AS DATE      FORMAT "99/99/9999"
            FIELD fecret   AS DATE      FORMAT "99/99/9999"
            FIELD fecont   AS DATE      FORMAT "99/99/9999"
            FIELD valrec   AS DECIMAL   FORMAT ">>>>>>>>>>>>"
            INDEX IdxEmpNit codempre nit.

      DEFINE TEMP-TABLE tmp-exasocSyA
            FIELD cod_age  LIKE clientes.agencia
            FIELD nit      AS CHARACTER FORMAT "x(12)"
            FIELD CtaPro   LIKE Cuentas.Cuenta
            FIELD valrec   AS DECIMAL   FORMAT ">>>>>>>>>>>>"
            INDEX IdxageSyA cod_age.

      DEFINE TEMP-TABLE Rec_Tmp 
            FIELD Agencia     AS CHARACTER FORMAT "X(3)"
            FIELD Cod_Empresa LIKE empresas.cod_empresa
            FIELD Empresa     AS CHARACTER FORMAT "X(15)"
            FIELD Nro_Pago    LIKE rec_nomina.nro_pago
            FIELD Nit         AS CHARACTER FORMAT "X(12)"
            FIELD Nombre      AS CHARACTER FORMAT "X(40)"
            FIELD Estado      AS CHARACTER FORMAT "X(12)"
            FIELD fecing      LIKE clientes.fec_ingreso
            FIELD fecret      LIKE clientes.fec_retiro
            FIELD fecpag      LIKE clientes.fec_ingreso
            FIELD Vr          LIKE ahorros.Sdo_Dispon INIT 0
            FIELD Err         AS CHARACTER FORMAT "X(25)"
            INDEX Idxrectmp Agencia Nit.

    DEFINE BUFFER BAhorros FOR Ahorros. /*Gcamacho Abr 12 - Ahorros Permanetes*/

DEFINE VAR periodoAplicar AS INTEGER.
DEFINE VAR anoAplicar AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Imp
&Scoped-define BROWSE-NAME Br_Pdctos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TPdctos Rec_Nomina Clientes

/* Definitions for BROWSE Br_Pdctos                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Pdctos TPdctos.CodP "999" TPdctos.NomP TPdctos.CuoOrig TPdctos.VrDist TPdctos.NC TPdctos.VrSdo TPdctos.Abono[1] TPdctos.Abono[3] TPdctos.Abono[2] TPdctos.Abono[4] TPdctos.Abono[5] TPdctos.Abono[6] TPdctos.Abono[7] TPdctos.Abono[8] TPdctos.Prior "9999"   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Pdctos   
&Scoped-define SELF-NAME Br_Pdctos
&Scoped-define QUERY-STRING-Br_Pdctos FOR EACH TPdctos NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Pdctos OPEN QUERY Br_Pdctos FOR EACH TPdctos NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Pdctos TPdctos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Pdctos TPdctos


/* Definitions for BROWSE Br_Rec                                        */
&Scoped-define FIELDS-IN-QUERY-Br_Rec Rec_Nomina.Nit Rec_Nomina.Val_Deducido Rec_Nomina.Num_Cuotas TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Rec   
&Scoped-define SELF-NAME Br_Rec
&Scoped-define OPEN-QUERY-Br_Rec /*   OPEN QUERY Br_Rec FOR EACH Rec_Nomina WHERE                      */ /*                              Rec_Nomina.Cod_Empresa EQ 0 NO-LOCK, ~
        */ /*        EACH Clientes WHERE Clientes.Nit    EQ Rec_Nomina.Nit       */ /*                        AND Clientes.Estado EQ 1 NO-LOCK.           */  /* Nuevo*/   OPEN QUERY Br_Rec FOR EACH Rec_Nomina WHERE                              Rec_Nomina.Cod_Empresa EQ W_CodEmp  AND                              Rec_Nomina.Nro_Pago    EQ W_NroPago AND                              Rec_Nomina.Fec_Contab  EQ ?         AND                              Rec_Nomina.Num_Cuotas  NE 99 NO-LOCK, ~
              EACH Clientes WHERE Clientes.Nit    EQ Rec_Nomina.Nit                        AND Clientes.Estado EQ 1 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Br_Rec Rec_Nomina Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Rec Rec_Nomina
&Scoped-define SECOND-TABLE-IN-QUERY-Br_Rec Clientes


/* Definitions for FRAME F_Proc                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Proc ~
    ~{&OPEN-QUERY-Br_Pdctos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-317 Rs_OpInf W_CmbEI Cmb_PagI W_FecIniI ~
W_FecFinI Btn_FinImp Btn_Imp-2 
&Scoped-Define DISPLAYED-OBJECTS Rs_OpInf W_CmbEI Cmb_PagI W_FecIniI ~
W_FecFinI 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Rec_XLibranza AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_SalirFec 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Regresar" 
     SIZE 8 BY 1.69 TOOLTIP "Regresar a la ventana principal".

DEFINE VARIABLE w-fecfin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Fin novedad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE w-fecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial  corte" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE BUTTON Btn_FinImp 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Regresar" 
     SIZE 9.57 BY 1.69 TOOLTIP "Regresar a la ventana principal".

DEFINE BUTTON Btn_Imp-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 9.57 BY 1.73 TOOLTIP "Informe por vencimientos y Extracto por Inversi�n.".

DEFINE VARIABLE Cmb_PagI AS CHARACTER FORMAT "X(36)":U 
     LABEL "Recaudos" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 33 BY 1 TOOLTIP "Pagos de esta Empresa"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CmbEI AS CHARACTER 
     LABEL "Pagadur�a" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN MAX-CHARS 36
     SIZE 35.72 BY 1 TOOLTIP "Empresas Afiliadas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_FecFinI AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .85
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_FecIniI AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .85
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Rs_OpInf AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Detalle Grabaci�n", 1,
"Disponible 1", 2,
"Disponible 2", 3
     SIZE 18.86 BY 2.42 TOOLTIP "Seleccione el informe"
     BGCOLOR 17 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-317
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.57 BY 2.92.

DEFINE BUTTON btn-Cuotas 
     LABEL "Novedad Pagadur�a" 
     SIZE 21 BY 1.12.

DEFINE BUTTON Btn_BorraPagos 
     LABEL "Borrar Cargue No Conta." 
     SIZE 24 BY 1.08.

DEFINE BUTTON Btn_Contabilizar 
     LABEL "&Contabilizar" 
     SIZE 11 BY 1.54 TOOLTIP "Abona la Distribuci�n y Contabiliza".

DEFINE BUTTON Btn_CreCancelado 
     LABEL "Cr�ditos Cancelados" 
     SIZE 21 BY 1.08.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 11 BY 1.35
     BGCOLOR 8 .

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 11.29 BY 1.62 TOOLTIP "Informe por vencimientos y Extracto por Inversi�n.".

DEFINE BUTTON Btn_Preliminar 
     LABEL "&Preliminar" 
     SIZE 11 BY 1.58 TOOLTIP "Informe Preliminar de la Distribuci�n".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 11.29 BY 1.38.

DEFINE VARIABLE Cmb_pagos AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 24.72 BY 1 TOOLTIP "Pagos de esta Empresa pendientes por Contabilizar"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CmbEmp AS CHARACTER FORMAT "X(50)":U 
     LABEL "Pagadur�a" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 36.14 BY 1 TOOLTIP "Empresas Afiliadas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(50)":U 
     LABEL "Oficina" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 47.57 BY 1 TOOLTIP "Agencias Disponibles"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 50.72 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE txt_contrapartida AS CHARACTER FORMAT "X(12)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_anoAplicar AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "A�o:" 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cont AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Dist AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .77 TOOLTIP "Vr.Distribuido"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NCuotas AS INTEGER FORMAT ">9":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 3 BY .85 TOOLTIP "Nro de Cuotas"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NitCte AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY .85 TOOLTIP "Ced/Nit del Cliente"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NomCte AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 48.86 BY .85 TOOLTIP "Nombre del Cliente"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroPago AS INTEGER FORMAT "999999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 TOOLTIP "Nro.del Recaudo para esta Empresa"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PorD AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .73 TOOLTIP "Valor por Distribuir"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_sem AS CHARACTER FORMAT "X(2)":U 
     LABEL "Periodo:" 
     VIEW-AS FILL-IN 
     SIZE 3.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotNoD AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.29 BY .81 TOOLTIP "Total de este Recaudo no Distribuido"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotRec AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.29 BY .81 TOOLTIP "Total de este Recaudo"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrRec AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .85 TOOLTIP "Valor del Recaudo"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_Opcrea AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Manual", 1,
"Archivo", 2,
"Desde Pdtos.", 3,
"Desde As400.", 4
     SIZE 14.43 BY 2.69 TOOLTIP "Marque la opci�n para capturar los recaudos"
     BGCOLOR 17 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 14 BY 8.08.

DEFINE RECTANGLE RECT-316
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.72 BY 3.73.

DEFINE VARIABLE Tg_Nuevo AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.29 BY .54 TOOLTIP "Marque para generar un nuevo recaudo"
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE Tg_SemP AS LOGICAL INITIAL no 
     LABEL "Semana de Pago?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.29 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Pdctos FOR 
      TPdctos SCROLLING.

DEFINE QUERY Br_Rec FOR 
      Rec_Nomina, 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Pdctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Pdctos W-Rec_XLibranza _FREEFORM
  QUERY Br_Pdctos NO-LOCK DISPLAY
      TPdctos.CodP        COLUMN-LABEL "Pto"                         FORM   "999"
      TPdctos.NomP        COLUMN-LABEL "Descripc.del Producto"       FORMAT "X(20)"
      TPdctos.CuoOrig     COLUMN-LABEL "Valor Cuota"          FORMAT ">>>>>>,>>9.99"
      TPdctos.VrDist      COLUMN-LABEL "Vr.Distribuido"       FORMAT ">>>>>>>,>>9.99"
      TPdctos.NC          COLUMN-LABEL "NC"
      TPdctos.VrSdo       COLUMN-LABEL "Sdo.Ant.Pdcto"        FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[1]    COLUMN-LABEL "Abono Capital"        FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[3]    COLUMN-LABEL "Int.Corrientes"       FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[2]    COLUMN-LABEL "Int.Anticipado"       FORMAT "->>>>>>,>>9.99"
      TPdctos.Abono[4]    COLUMN-LABEL "Int.Dif.Cobro"        FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[5]    COLUMN-LABEL "Inter�s x Mora"       FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[6]    COLUMN-LABEL "Abono Costas"         FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[7]    COLUMN-LABEL "Honorarios"           FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[8]    COLUMN-LABEL "Polizas"              FORMAT ">>>>>>>,>>9.99"
      TPdctos.Prior       COLUMN-LABEL "Prio"                 FORM   "9999"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 110 BY 5.65
         BGCOLOR 15 FGCOLOR 0 FONT 5
         TITLE BGCOLOR 15 FGCOLOR 0 "Distribuci�n del Recaudo Seleccionado" ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN TOOLTIP "Distribuci�n Preliminar".

DEFINE BROWSE Br_Rec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Rec W-Rec_XLibranza _FREEFORM
  QUERY Br_Rec NO-LOCK DISPLAY
      Rec_Nomina.Nit FORMAT "X(12)":U      COLUMN-LABEL "C�dula/Nit"
      Rec_Nomina.Val_Deducido              COLUMN-LABEL "Valor Recaudo" FORMAT ">,>>>,>>>,>>9.99":U
      Rec_Nomina.Num_Cuotas FORMAT "99":U  COLUMN-LABEL "N.C"
      TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)
                                           COLUMN-LABEL "Nombre del Cliente" FORMAT "X(35)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 70 BY 8.08
         BGCOLOR 15 FGCOLOR 0 FONT 5 ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN TOOLTIP "Seleccione el Recaudo del Cliente a modificar".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     Btn_BorraPagos AT ROW 3.42 COL 85 WIDGET-ID 6
     W_CmbEmp AT ROW 1.23 COL 71.86 COLON-ALIGNED
     W_CmbOfi AT ROW 1.27 COL 9.14 COLON-ALIGNED
     Cmb_pagos AT ROW 2.35 COL 83 COLON-ALIGNED NO-LABEL
     Tg_Nuevo AT ROW 2.62 COL 4
     Rs_Opcrea AT ROW 3.15 COL 5 NO-LABEL
     W_NitCte AT ROW 3.15 COL 22 COLON-ALIGNED NO-LABEL
     W_VrRec AT ROW 3.15 COL 41 COLON-ALIGNED NO-LABEL
     W_NCuotas AT ROW 3.15 COL 67.72 COLON-ALIGNED NO-LABEL
     W_NroPago AT ROW 3.15 COL 73 COLON-ALIGNED NO-LABEL
     W_NomCte AT ROW 4.23 COL 22 COLON-ALIGNED NO-LABEL
     BUTTON-5 AT ROW 5.12 COL 97.29
     Br_Rec AT ROW 6.38 COL 3.14
     txt_contrapartida AT ROW 5.35 COL 73 COLON-ALIGNED
     W_sem AT ROW 5.31 COL 54.29 COLON-ALIGNED
     Btn_Imp AT ROW 6.54 COL 97.29
     W_TotRec AT ROW 6.92 COL 73 COLON-ALIGNED NO-LABEL
     Btn_Preliminar AT ROW 8.12 COL 97.57
     W_TotNoD AT ROW 8.35 COL 73 COLON-ALIGNED NO-LABEL
     Btn_Contabilizar AT ROW 9.73 COL 97.57 HELP
          "Permite Realizar la contabilizaci�n de Depreciaci�n"
     Tg_SemP AT ROW 15.54 COL 78
     Btn_Done AT ROW 11.31 COL 97.57 HELP
          "Sale del proceso de Depreciaci�n y Ajustes"
     Br_Pdctos AT ROW 14.65 COL 3
     W_Cont AT ROW 10.08 COL 73.14 COLON-ALIGNED NO-LABEL
     Msaje AT ROW 20.73 COL 40.29 COLON-ALIGNED NO-LABEL
     W_Dist AT ROW 20.88 COL 21.57 COLON-ALIGNED NO-LABEL
     W_PorD AT ROW 20.92 COL 1.43 COLON-ALIGNED NO-LABEL
     Btn_CreCancelado AT ROW 12.31 COL 74 WIDGET-ID 2
     btn-Cuotas AT ROW 11.08 COL 74 WIDGET-ID 4
     w_anoAplicar AT ROW 5.31 COL 65 COLON-ALIGNED WIDGET-ID 8
     "Nuevo Recaudo" VIEW-AS TEXT
          SIZE 14.72 BY .54 AT ROW 2.62 COL 6
          BGCOLOR 17 FGCOLOR 7 
     "Total x Contabilizar" VIEW-AS TEXT
          SIZE 17 BY .54 AT ROW 6.35 COL 75
          BGCOLOR 17 FGCOLOR 7 
     "Regist.Procesado" VIEW-AS TEXT
          SIZE 16 BY .77 AT ROW 9.23 COL 75.14
          BGCOLOR 1 
     "Tot. NO Distribuido" VIEW-AS TEXT
          SIZE 16.29 BY .54 AT ROW 7.81 COL 75
          BGCOLOR 17 FGCOLOR 7 
     "Contrapartida" VIEW-AS TEXT
          SIZE 14.86 BY .65 AT ROW 4.62 COL 75
          BGCOLOR 17 FGCOLOR 7 
     "Valor Distribuido" VIEW-AS TEXT
          SIZE 16 BY .54 AT ROW 20.35 COL 23.57
          BGCOLOR 18 FGCOLOR 15 
     "Cargue:" VIEW-AS TEXT
          SIZE 8.57 BY .81 AT ROW 2.35 COL 75
          BGCOLOR 17 FGCOLOR 7 
     "Ced./Nit Cliente                   Valor Recaudo             Nro.Cuotas" VIEW-AS TEXT
          SIZE 50 BY .62 AT ROW 2.35 COL 24
          BGCOLOR 18 FGCOLOR 15 
     "Vlr. este Recaudo" VIEW-AS TEXT
          SIZE 16 BY .58 AT ROW 20.35 COL 3.43
          BGCOLOR 18 FGCOLOR 15 
     RECT-314 AT ROW 4.85 COL 96
     RECT-316 AT ROW 2.38 COL 2.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.29 BY 21.15
         BGCOLOR 17 FGCOLOR 0 FONT 5.

DEFINE FRAME F_Imp
     Rs_OpInf AT ROW 1.42 COL 17.14 NO-LABEL
     W_CmbEI AT ROW 4.31 COL 11.43 COLON-ALIGNED
     Cmb_PagI AT ROW 6.08 COL 11.43 COLON-ALIGNED
     W_FecIniI AT ROW 7.12 COL 11.57 COLON-ALIGNED
     W_FecFinI AT ROW 7.12 COL 36 COLON-ALIGNED
     Btn_FinImp AT ROW 8.35 COL 39
     Btn_Imp-2 AT ROW 8.38 COL 3.86
     "No.Pago  Fec-Pago  Fec-Contabilizac." VIEW-AS TEXT
          SIZE 32.72 BY .62 AT ROW 5.54 COL 13.57
          BGCOLOR 18 FGCOLOR 15 
     RECT-317 AT ROW 1 COL 19
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 20.57 ROW 8.12
         SIZE 51.57 BY 10.23
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Opciones de Informes".

DEFINE FRAME frm-FPagaduria
     Btn_SalirFec AT ROW 1 COL 40 WIDGET-ID 10
     w-fecIni AT ROW 1.27 COL 22 COLON-ALIGNED WIDGET-ID 2
     w-fecfin AT ROW 2.35 COL 22 COLON-ALIGNED WIDGET-ID 8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.62
         SIZE 50 BY 3.65
         BGCOLOR 17 FGCOLOR 0 
         TITLE "Fechas  a validar" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Rec_XLibranza ASSIGN
         HIDDEN             = YES
         TITLE              = "Distribuci�n Recaudos De aportes"
         HEIGHT             = 19.08
         WIDTH              = 112.43
         MAX-HEIGHT         = 30.46
         MAX-WIDTH          = 164.57
         VIRTUAL-HEIGHT     = 30.46
         VIRTUAL-WIDTH      = 164.57
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
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
/* SETTINGS FOR WINDOW W-Rec_XLibranza
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME frm-FPagaduria:FRAME = FRAME F_Imp:HANDLE.

/* SETTINGS FOR FRAME frm-FPagaduria
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME frm-FPagaduria:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Imp
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Imp:HIDDEN           = TRUE
       FRAME F_Imp:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Proc
   Custom                                                               */
/* BROWSE-TAB Br_Rec BUTTON-5 F_Proc */
/* BROWSE-TAB Br_Pdctos Btn_Done F_Proc */
/* SETTINGS FOR FILL-IN Msaje IN FRAME F_Proc
   NO-ENABLE 2                                                          */
ASSIGN 
       Msaje:HIDDEN IN FRAME F_Proc           = TRUE.

/* SETTINGS FOR RADIO-SET Rs_Opcrea IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX W_CmbOfi IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Cont IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Dist IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCte IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NroPago IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_PorD IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TotNoD IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TotRec IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Rec_XLibranza)
THEN W-Rec_XLibranza:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Pdctos
/* Query rebuild information for BROWSE Br_Pdctos
     _START_FREEFORM
OPEN QUERY Br_Pdctos FOR EACH TPdctos NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Pdctos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Rec
/* Query rebuild information for BROWSE Br_Rec
     _START_FREEFORM
/*   OPEN QUERY Br_Rec FOR EACH Rec_Nomina WHERE                      */
/*                              Rec_Nomina.Cod_Empresa EQ 0 NO-LOCK,  */
/*        EACH Clientes WHERE Clientes.Nit    EQ Rec_Nomina.Nit       */
/*                        AND Clientes.Estado EQ 1 NO-LOCK.           */

/* Nuevo*/
  OPEN QUERY Br_Rec FOR EACH Rec_Nomina WHERE
                             Rec_Nomina.Cod_Empresa EQ W_CodEmp  AND
                             Rec_Nomina.Nro_Pago    EQ W_NroPago AND
                             Rec_Nomina.Fec_Contab  EQ ?         AND
                             Rec_Nomina.Num_Cuotas  NE 99 NO-LOCK,
       EACH Clientes WHERE Clientes.Nit    EQ Rec_Nomina.Nit
                       AND Clientes.Estado EQ 1 NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Rec */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Imp
/* Query rebuild information for FRAME F_Imp
     _Query            is NOT OPENED
*/  /* FRAME F_Imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Rec_XLibranza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Rec_XLibranza W-Rec_XLibranza
ON END-ERROR OF W-Rec_XLibranza /* Distribuci�n Recaudos De aportes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Rec_XLibranza W-Rec_XLibranza
ON WINDOW-CLOSE OF W-Rec_XLibranza /* Distribuci�n Recaudos De aportes */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Rec
&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Br_Rec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Rec W-Rec_XLibranza
ON MOUSE-SELECT-CLICK OF Br_Rec IN FRAME F_Proc
DO:
   IF AVAIL(Rec_Nomina) THEN DO:
      ASSIGN W_NitCte:SCREEN-VALUE  = Rec_Nomina.Nit
             W_NitCte
             W_VrRec:SCREEN-VALUE   = "0.00"
             W_VrRec                = 0
             W_NCuotas:SCREEN-VALUE = STRING(Rec_Nomina.Num_Cuotas)
             W_NCuotas.

      CLOSE QUERY Br_Pdctos.

      RUN DistRecaudo.    
            
      OPEN QUERY Br_Pdctos FOR EACH TPdctos NO-LOCK INDEXED-REPOSITION.
     
      APPLY "LEAVE" TO W_NitCte.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Cuotas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Cuotas W-Rec_XLibranza
ON CHOOSE OF btn-Cuotas IN FRAME F_Proc /* Novedad Pagadur�a */
DO:
  ASSIGN w-fecini = w_fecha  w-fecfin = w_fecha   w_arc = 1.
  ASSIGN w-fecini:SCREEN-VALUE IN FRAME frm-fpagaduria = string(w_fecha)  w-fecfin:SCREEN-VALUE = string(w_fecha).
  ASSIGN FRAME F_Proc:SENSITIVE = FALSE
         FRAME Frm-Fpagaduria:VISIBLE    = TRUE.
  APPLY "Entry" TO w-fecini IN FRAME frm-fpagaduria.
 /* RUN cuotasnomina.r.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_BorraPagos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_BorraPagos W-Rec_XLibranza
ON CHOOSE OF Btn_BorraPagos IN FRAME F_Proc /* Borrar Cargue No Conta. */
DO:
  ASSIGN W_CodEmp  = INTEGER(SUBSTRING(W_CmbEmp :SCREEN-VALUE,1,4))
         W_NroPago = INTEGER(SUBSTRING(Cmb_Pagos:SCREEN-VALUE,1,6))
         W_NroPago:SCREEN-VALUE = STRING(W_NroPago).

  IF W_CodEmp NE 0 AND W_NroPago NE 0 THEN DO:
     FIND FIRST Rec_Nomina WHERE Rec_Nomina.Cod_Empr     EQ W_CodEmp
                             AND Rec_Nomina.Nro_Pago     EQ W_NroPago
                             NO-ERROR.
     IF AVAILABLE(Rec_Nomina) AND Rec_Nomina.Fec_Contabilizacion EQ ? THEN DO:
        MESSAGE "Esta Seguro(a) de Eliminar"                         SKIP
                "La N�mina de la Empresa : " STRING(W_CodEmp,"9999") SKIP
                "Y Con Nro. de Pago         : " STRING(W_NroPago,"999999") 
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice AS LOGICAL.
        CASE choice:
            WHEN TRUE THEN DO:
                ASSIGN Msaje:VISIBLE = TRUE.
                RUN EliPagosNoConta.
                ASSIGN Msaje:VISIBLE = FALSE.
                CLOSE QUERY Br_Rec.
                RUN ComboPagos.
                ASSIGN W_NroPago              = 0
                       W_NroPago:SCREEN-VALUE = "0".
                ASSIGN W_CodEmp  = 0.
                APPLY "Entry" TO W_CmbEmp.
            END.
            WHEN FALSE THEN RETURN.
        END CASE.
     END.
     ELSE
      IF AVAILABLE(Rec_Nomina) AND Rec_Nomina.Fec_Contabilizacion NE ? THEN
         MESSAGE "La N�mina de la Empresa : " STRING(W_CodEmp,"9999")    SKIP
             "Y Con Nro. de Pago      : " STRING(W_NroPago,"999999") SKIP
             "Fue Contabilizada el d�a: " STRING(Rec_Nomina.Fec_Contabilizacion,"99/99/9999") SKIP(1)
             "Por Lo Tanto No Puede ser Eliminada.." 
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ELSE
          MESSAGE "No Existe el Recaudo de N�mina."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  ELSE
     MESSAGE "Debe Seleccionar La Empresa y " SKIP
             "El Nro de Pago a Eliminar los Registros.. " SKIP
             "Recuerde Solo los Nro de Pago NO Contabilizados" SKIP
             "Se Eliminaran..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Contabilizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Contabilizar W-Rec_XLibranza
ON CHOOSE OF Btn_Contabilizar IN FRAME F_Proc /* Contabilizar */
DO:
    DEFINE VAR flagAplicar AS LOGICAL.
    
    MESSAGE "Est� seguro de recaudar para el mes" W_sem:SCREEN-VALUE
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO SET flagAplicar.

    IF NOT flagAplicar THEN
        RETURN NO-APPLY.

    W_Inf = TRUE.
    W_SiContab = TRUE.
    Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Distrib.Contable desde Pdctos de C/Cliente...".
    Msaje:VISIBLE = TRUE.
    W_TotNoD = 0.
    W_TotNoD:SCREEN-VALUE = STRING(W_TotNoD).
    W_ContabDistSi = 0.
    W_ContabDistNo = 0.

    EMPTY TEMP-TABLE CopTPdctos.
    EMPTY TEMP-TABLE TPdctos.
    EMPTY TEMP-TABLE CopMov_Contable.
    EMPTY TEMP-TABLE TempCtas.
    
    CLOSE QUERY Br_Rec.
    CLOSE QUERY Br_Pdctos.

    IF w_sem:SCREEN-VALUE <> "" AND txt_contrapartida:SCREEN-VALUE <> "" THEN DO:
        RUN CambioExaso NO-ERROR.

        MESSAGE "Seguro(a) de contablizar el periodo de pago:" w_sem "?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Contabilizar" UPDATE flagDistribuir AS LOGICAL.

        IF NOT flagDistribuir THEN
            RETURN.

        RUN Valida NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "La configuraci�n contable present� errores...Proceso cancelado."
                VIEW-AS ALERT-BOX ERROR.

            Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor".
            W_Inf = FALSE.
            W_SiContab = FALSE.

            RETURN.
        END.

        IF txt_contrapartida:SCREEN-VALUE = "" THEN DO:
            MESSAGE "Debe ingresar la cuenta de contrapartida... Proceso cancelado."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor".
            W_Inf = FALSE.
            W_SiContab = FALSE.

            RETURN.
        END.

        FIND FIRST Cuentas WHERE Cuentas.Tipo = 2
                             AND Cuentas.Cuenta = txt_contrapartida:SCREEN-VALUE
                             AND Cuentas.Estado = 1 NO-LOCK NO-ERROR.

        FIND FIRST Empresas WHERE Empresas.Cod_Empresa = W_CodEmp NO-ERROR.

        IF NOT AVAILABLE(Cuentas) OR NOT AVAILABLE(Empresas) THEN DO:
            MESSAGE "La Cuenta de Caja y la Empresa deben existir Activas..." SKIP
                    "No se acepta la operaci�n."
                VIEW-AS ALERT-BOX ERROR.

            Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor".
            W_Inf = FALSE.
            W_SiContab = FALSE.

            RETURN.
        END.

        FIND FIRST Entidad WHERE Entidad.Entidad = W_Entidad NO-LOCK NO-ERROR.
        IF AVAILABLE(Entidad) THEN DO:
            FIND FIRST Comprobantes WHERE Comprobantes.Agencia = W_Agencia
                                      AND Comprobantes.Comprob = Entidad.Cpte_REcNom
                                      AND Comprobantes.Estado = 1 NO-ERROR.
        END.

        IF NOT AVAILABLE(Comprobantes) OR NOT AVAILABLE(Entidad) THEN DO:
            MESSAGE "El Comprobante Contable para el proceso debe existir en Entidad y en Comprobantes." SKIP
                    "No se acepta la operaci�n."
                VIEW-AS ALERT-BOX ERROR.

            Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor".
            W_Inf = FALSE.
            W_SiContab = FALSE.

            RETURN.
        END.

        SESSION:SET-WAIT-STATE("GENERAL").

        RUN Contabilizar NO-ERROR. /* oakley */
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "La Contabilizaci�n Presento Errores...Proceso cancelado."
                VIEW-AS ALERT-BOX ERROR.

            ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor"
                   W_Inf = FALSE
                   W_SiContab = FALSE.

            SESSION:SET-WAIT-STATE("").

            RETURN.
        END.
        ELSE DO:
            DEFI VAR Listado AS CHAR FORM "X(40)".

            Listado = W_PathSpl + "DRContab-" + STRING(W_CodEmp) + "-" + STRING(W_NroPago)  + ".Lst".
            {Incluido\ImpArch.I "listado"}

            ASSIGN W_Inf = FALSE
                   W_SiContab = FALSE
                   Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generada la Distrib.Contable desde Pdctos de C/Cliente..."
                   Msaje:VISIBLE = FALSE
                   W_Cont = 0
                   W_Cont:SCREEN-VALUE = STRING(W_Cont).

            EMPTY TEMP-TABLE CopTPdctos.
            EMPTY TEMP-TABLE TPdctos.
            EMPTY TEMP-TABLE CopMov_Contable.
            EMPTY TEMP-TABLE TempCtas.

            IF W_SiContab THEN
                RUN ComboPagos NO-ERROR.

            SESSION:SET-WAIT-STATE("").
        END.
    END.
    ELSE DO:
       MESSAGE "Debe ingresar el mes y la cuenta de contrapartida ..Proceso cancelado."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

       ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor"
              W_Inf = FALSE
              W_SiContab = FALSE.

       RETURN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_CreCancelado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CreCancelado W-Rec_XLibranza
ON CHOOSE OF Btn_CreCancelado IN FRAME F_Proc /* Cr�ditos Cancelados */
DO:
    ASSIGN w-fecini = w_fecha  w-fecfin = w_fecha w_arc = 2.
    ASSIGN w-fecini:SCREEN-VALUE IN FRAME frm-fpagaduria = string(w_fecha)  w-fecfin:SCREEN-VALUE = string(w_fecha).
    ASSIGN FRAME F_Proc:SENSITIVE = FALSE
           FRAME Frm-Fpagaduria:VISIBLE    = TRUE.
    APPLY "Entry" TO w-fecini IN FRAME frm-fpagaduria.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Rec_XLibranza
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  EMPTY TEMP-TABLE Rec_Tmp.
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME Btn_FinImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FinImp W-Rec_XLibranza
ON CHOOSE OF Btn_FinImp IN FRAME F_Imp /* Regresar */
DO:
   ASSIGN FRAME F_Proc:SENSITIVE = TRUE
          FRAME F_Imp:VISIBLE    = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp W-Rec_XLibranza
ON CHOOSE OF Btn_Imp IN FRAME F_Proc /* Imprimir */
DO:
  ASSIGN FRAME F_Proc:SENSITIVE    = FALSE
         FRAME F_Imp:VISIBLE       = TRUE
         W_FecIniI:SCREEN-VALUE    = STRING(TODAY)
         W_FecIniI
         W_FecFinI:SCREEN-VALUE    = STRING(TODAY)
         W_FecFinI
         Rs_OpInf
         Cmb_PagI:LIST-ITEMS       = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME Btn_Imp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp-2 W-Rec_XLibranza
ON CHOOSE OF Btn_Imp-2 IN FRAME F_Imp /* Imprimir */
DO:
  DEFI VAR Listado AS CHAR FORM "X(40)".
  
  ASSIGN listado = W_PathSpl + "InfDRec-" + STRING(W_CodEmpI) + "-" + STRING(W_NroPagI) + ".Lst".
/*   ASSIGN Listado  = "C:\InfDRec.Lst" */
         W_SiImpF = TRUE.
  
  {Incluido\Imprimir.I "listado"}

  W_SiImpF = FALSE.
END.

/*-----------------*/
PROCEDURE InfFrame:
  {Incluido\RepEncabezado.I}

  FIND FIRST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmpI
                          AND Rec_Nomina.Nro_Pago    EQ W_NroPagI
                          AND Rec_Nomina.Fec_Contab  EQ ? NO-LOCK NO-ERROR.

  ASSIGN W_Reporte    = "Reporte   : Detalle-Grabaci�n Recaudo       Fecha del Informe: " +
                        STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
         W_EncColumna = "Empresa: " + STRING(W_CodEmpI) + " - Nro.Pago: " + STRING(W_NroPagI)
         TImp         = 0
         W_Cont       = 0. 

  IF NOT AVAIL(Rec_Nomina) THEN
     W_EncColumna = W_EncColumna + "     Recaudo Ya Contabilizado.".
  ELSE 
     W_EncColumna = W_EncColumna + "     Recaudo Pendiente X Contabilizar.".

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.

  FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmpI
                        AND Rec_Nomina.Nro_Pago    EQ W_NroPagI NO-LOCK BY Rec_Nomina.Nit:
      ASSIGN TImp   = TImp + Rec_Nomina.Val_Deduc
             W_Cont = W_Cont + 1.

      FIND FIRST Clientes WHERE Clientes.Nit EQ Rec_Nomina.Nit
                            AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.    

      DISPLAY Rec_Nomina.Nit             LABEL "C�dula/Nit"
              TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)   
                                         LABEL "Nombre del Cliente" FORM "X(40)" WHEN AVAIL(Clientes)
              Rec_Nomina.Val_Deduc       LABEL "Valor Recaudo"              
         WITH DOWN WIDTH 140 FRAME FImp USE-TEXT NO-LABELS STREAM-IO NO-BOX.
  END.
    
  DISPLAY SKIP(1)
          "TOTALES : Registros : " + STRING(W_Cont,"999999") + " Por valor de $ " +
           STRING(Timp,"->>>>,>>>,>>9.99")                 FORMAT "X(120)"          
      WITH DOWN WIDTH 140 FRAME FTtImp USE-TEXT NO-LABELS STREAM-IO NO-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Btn_Preliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Preliminar W-Rec_XLibranza
ON CHOOSE OF Btn_Preliminar IN FRAME F_Proc /* Preliminar */
DO:
    DEFINE VARIABLE vlOp AS LOGICAL INITIAL NO NO-UNDO.  
    MESSAGE "Esta seguro de Recaudar" SKIP
            "para le Mes " W_sem:SCREEN-VALUE
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO SET vlOp.
    IF NOT vlOP THEN RETURN NO-APPLY.

   /* Nuevo*/
   IF w_sem:SCREEN-VALUE NE "" THEN DO:
       RUN CambioExaso.
       ASSIGN W_Inf                              = TRUE
              Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Distrib.Preliminar desde Pdctos de C/Cliente..."
              Msaje:VISIBLE                      = TRUE
              W_TotNoD                           = 0
              W_TotNoD:SCREEN-VALUE              = STRING(W_TotNoD).
    
    /*    FOR EACH CopTPdctos: DELETE CopTPdctos. END. */
       EMPTY TEMP-TABLE CopTPdctos.
        
       SESSION:SET-WAIT-STATE("GENERAL").
    
       FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp
                             AND Rec_Nomina.Nro_Pago    EQ W_NroPago
                             AND Rec_Nomina.Fec_Contab  EQ ? NO-LOCK:
           IF Rec_Nomina.Val_Deduc GT 0 AND Rec_Nomina.Num_Cuotas  NE 99 THEN DO:
              ASSIGN W_NitCte            = Rec_Nomina.Nit
                     W_Cont              = W_Cont + 1
                     W_Cont:SCREEN-VALUE = STRING(W_Cont).
              
              RUN DistRecaudo NO-ERROR.
              ASSIGN W_TotNoD              = W_TotNoD + W_VrADist
                     W_TotNoD:SCREEN-VALUE = STRING(W_TotNoD).
           END.
       END.
       /* Nuevo */
       /*OUTPUT CLOSE.*/
       /*RUN Exasociados NO-ERROR.*/
       RUN ImpExaso.
       RUN InfSinRema.
       EMPTY TEMP-TABLE Tmp_SinRema.
       /*RUN InfExasoc   NO-ERROR.
       FOR EACH tmp-exasoc: DELETE tmp-exasoc. END.*/
    
       DEFINE VARIABLE Listado AS CHAR FORM "X(40)".
       ASSIGN listado = W_PathSpl + "DistRPrel-" + STRING(W_CodEmp) + "-" + STRING(W_NroPago) + ".Lst".
/*        listado = W_PathSpl + "DistRPrel.Lst".  */       
       {Incluido\Imprimir.I "listado"}
       ASSIGN W_Inf                              = FALSE
              Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generada la Distrib.Preliminar desde Pdctos de C/Cliente..."
              Msaje:VISIBLE                      = FALSE
              W_Cont                             = 0
              W_Cont:SCREEN-VALUE                = STRING(W_Cont).
    /*    FOR EACH CopTPdctos: DELETE CopTPdctos. END. */
       EMPTY TEMP-TABLE CopTPdctos.
       APPLY "MOUSE-SELECT-CLICK" TO Br_Rec.
       SESSION:SET-WAIT-STATE("").
   END. /* Fin de w_sem*/
   ELSE DO:
       /** Variables en Blanco */
        MESSAGE "Debe ingresar el Mes a Procesar...."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distribuci�n de Recaudo Cancelada...Revise por favor".
        ASSIGN W_Inf = FALSE.
        RETURN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frm-FPagaduria
&Scoped-define SELF-NAME Btn_SalirFec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalirFec W-Rec_XLibranza
ON CHOOSE OF Btn_SalirFec IN FRAME frm-FPagaduria /* Regresar */
DO:
    MESSAGE w-fecini w-fecfin
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

   ASSIGN FRAME F_Proc:SENSITIVE = TRUE
          FRAME Frm-Fpagaduria:VISIBLE    = FALSE.
 DEFINE VAR zpath   AS CHARACTER FORMAT "X(70)".
 DEFINE VARIABLE REGISTRO AS CHARACTER FORMAT "X(180)".
 DEFINE VARIABLE W_fecnov AS DATE.
 
 IF w_arc = 1 THEN 
 DO:
   zpath = W_PathSpl + "nomina_CodEmp" + STRING(w_codEmp) + "_" + STRING(day(w_fecha),"99") + STRING(MONTH(w_fecha),'99') + STRING(YEAR(w_fecha),'9999').
   ASSIGN w-fecini w-fecfin.
   
   /*fecha de inicio */
/*    FOR EACH Tmp-deducc: DELETE Tmp-deducc.    END. */
   EMPTY TEMP-TABLE Tmp-deducc.

   MESSAGE w-fecini w-fecfin
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   FOR EACH clientes WHERE clientes.cod_empresa EQ W_CodEmp NO-LOCK:
       FOR EACH creditos WHERE creditos.nit EQ clientes.nit 
                           AND creditos.sdo_capital GT 0
                           AND (creditos.FOR_pago    EQ 2) NO-LOCK:
          ASSIGN W_fecnov  = Creditos.Fec_Desembolso.
          IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desembolso THEN
             W_fecnov = Creditos.Fec_PagAnti.
              /*W_fecnov = Creditos.Fec_Desembolso.*/
          
          IF (W_fecnov GE w-fecini AND W_fecnov LE w-fecfin) THEN DO:
              
              FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
              FIND FIRST tmp-deducc   WHERE tmp-deducc.cedula  EQ clientes.nit NO-ERROR.
              IF NOT AVAILABLE (tmp-deducc) THEN CREATE tmp-deducc.
              ASSIGN tmp-deducc.cod_emp = W_CmbEmp:SCREEN-VALUE  IN FRAME F_Proc
                     tmp-deducc.agepro  = creditos.agencia
                     tmp-deducc.cedula  = clientes.nit
                     tmp-deducc.nombre  = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)  
                     tmp-deducc.nroobli = TRIM(STRING(Creditos.pagare))
                     tmp-deducc.nompro  = TRIM(Pro_Creditos.Nom_Producto) 
                     tmp-deducc.nrocuo  = creditos.plazo
                     tmp-deducc.fecdesc = W_Fecnov
                     tmp-deducc.vdeduc  = tmp-deducc.vdeduc + creditos.cuota.
               ASSIGN tmp-deducc.codnoved  = "510".
               
              
          END. /* Fin Valida Fecha de Movimiento*/
       END. /* For de Creditos*/
  /*
       FOR EACH ahorros WHERE ahorros.nit      EQ clientes.nit
                          AND AHORROS.FOR_PAGO EQ 2 
                          AND ahorros.estado   EQ 1  
                          AND (ahorros.fec_apertura GE w-fecini 
                          AND  ahorros.Fec_apertura LE w-fecfin)
                          AND cod_ahorro NE 216 NO-LOCK:
           FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
           FIND FIRST tmp-deducc   WHERE tmp-deducc.cedula  EQ clientes.nit AND
                                         tmp-deducc.nroobli EQ Ahorros.cue_ahorros NO-ERROR.
           IF NOT AVAILABLE (tmp-deducc) THEN CREATE tmp-deducc.
           ASSIGN tmp-deducc.cod_emp = W_CmbEmp:SCREEN-VALUE  IN FRAME F_Proc
                  tmp-deducc.agepro  = Ahorros.agencia
                  tmp-deducc.cedula  = clientes.nit
                  tmp-deducc.nombre  = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)  
                  tmp-deducc.nroobli = Ahorros.cue_ahorros
                  tmp-deducc.nompro  = Pro_Ahorros.Nom_Producto
                  tmp-deducc.nrocuo  = ahorros.plazo / 30
                  tmp-deducc.fecdesc = w_fecha
                  tmp-deducc.vdeduc  = ahorros.cuota.
           IF clientes.cod_empresa = 1 THEN DO: /* procuraduria */
              IF Ahorros.tip_ahorro = 4 THEN
                 ASSIGN tmp-deducc.codnoved  = "847".
              ELSE
                 IF Ahorros.tip_ahorro = 2 THEN
                    ASSIGN tmp-deducc.codnoved  = "1292".
           END.
           ELSE 
              IF clientes.cod_empresa = 4 THEN DO: /* INPEC */ 
                IF Ahorros.tip_ahorro = 2 THEN 
                   ASSIGN tmp-deducc.codnoved  = "JURD1".
                  IF Ahorros.tip_ahorro = 4 THEN 
                     ASSIGN tmp-deducc.codnoved  = "JURAP".
              END.
              ELSE 
                 IF clientes.cod_empresa = 5 THEN DO: /* Defensoria del Pueblo */ 
                   IF Ahorros.tip_ahorro = 2 THEN 
                      ASSIGN tmp-deducc.codnoved  = "409".
                     IF Ahorros.tip_ahorro = 4 THEN 
                        ASSIGN tmp-deducc.codnoved  = "263".
                 END.
       END. /* For de Ahorros*/
       */
   END. /* For de Clientes*/

   OUTPUT TO VALUE(zpath).
   DEFINE VARIABLE totcuo AS DECIMAL.

   FOR EACH tmp-deducc: /* Vigentes*/
       ASSIGN totcuo = 0.
       FOR EACH creditos WHERE  creditos.nit = tmp-deducc.cedula AND
                               (creditos.FOR_pago = 2 ) NO-LOCK:
            ASSIGN totcuo = totcuo + creditos.cuota.
            FIND FIRST ahorros WHERE ahorros.nit = creditos.nit AND
                                     ahorros.cod_ahorro = 217 AND
                                     ahorros.cue_ahorro = creditos.pagare NO-LOCK NO-ERROR.
            IF AVAILABLE(ahorros)  THEN totcuo = totcuo + ahorros.cuota.
       END.

       ASSIGN tmp-deducc.vdeduc = totcuo.
       REGISTRO =  TRIM(STRING(tmp-deducc.cod_emp)) + ";".
       ASSIGN REGISTRO = TRIM(REGISTRO) + tmp-deducc.codnoved + ";".
       REGISTRO =  TRIM(REGISTRO) +
                   TRIM(tmp-deducc.cedula)          + ";" +
                   TRIM(tmp-deducc.nombre)          + ";" +
                   TRIM(STRING(tmp-deducc.nrocuo))  + ";" +
                   TRIM(STRING(tmp-deducc.vdeduc))  + ";" +
                   TRIM(STRING(tmp-deducc.fecdesc)).
       PUT REGISTRO SKIP.

   END. /* For de tmp-deducc*/

   FIND FIRST Empresas WHERE Empresas.Estado    EQ 1
                       AND Empresas.Cod_empresa EQ W_CodEmp
                       AND Empresas.Agencia     EQ w_agencia  NO-ERROR.
   IF AVAILABLE(Empresas) THEN
      ASSIGN empresas.fec_constitucion = w-fecfin.
   FIND CURRENT empresas NO-LOCK NO-ERROR.
   MESSAGE "GENERACION DE CUOTAS TERMINO EXITOSAMENTE" SKIP
           "UBICACION DEL ARCHIVO: " zpath  VIEW-AS ALERT-BOX.
 END. /* Fin de w_arc = 1*/
 ELSE
 DO:
   zpath = W_PathSpl + "CreCancelado_CodEmp" + STRING(w_codEmp) + "_" + STRING(day(w_fecha),"99") + STRING(MONTH(w_fecha),'99') + STRING(YEAR(w_fecha),'9999').
   ASSIGN w-fecini w-fecfin.
/*    FOR EACH Tmp-deducc: DELETE Tmp-deducc.    END. */
   EMPTY TEMP-TABLE Tmp-deducc.

   FOR EACH clientes WHERE clientes.cod_empresa EQ W_CodEmp:
       FOR EACH creditos WHERE creditos.nit EQ clientes.nit 
                           AND creditos.sdo_capital EQ 0
                           AND creditos.FOR_pago    EQ 2 
                           AND ( creditos.fec_cancetotal GE w-fecini AND
                                 creditos.Fec_cancetotal LE w-fecfin)
           NO-LOCK:
           FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
           FIND FIRST tmp-deducc   WHERE tmp-deducc.cedula  EQ clientes.nit AND
                                         tmp-deducc.nroobli EQ TRIM(STRING(Creditos.num_credito)) NO-ERROR.
           IF NOT AVAILABLE (tmp-deducc) THEN CREATE tmp-deducc.
           ASSIGN tmp-deducc.cod_emp = W_CmbEmp:SCREEN-VALUE  IN FRAME F_Proc
                  tmp-deducc.agepro  = creditos.agencia
                  tmp-deducc.cedula  = clientes.nit
                  tmp-deducc.nombre  = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)  
                  tmp-deducc.nroobli = TRIM(STRING(Creditos.num_credito))
                  tmp-deducc.nompro  = TRIM(Pro_Creditos.Nom_Producto) 
                  tmp-deducc.nrocuo  = creditos.plazo
                  tmp-deducc.fecdesc = Creditos.Fec_Pago
                  tmp-deducc.vdeduc  = creditos.cuota.
       END. /* For de Creditos*/

       FOR EACH ahorros WHERE ahorros.nit      EQ clientes.nit
                          AND AHORROS.FOR_PAGO EQ 2 
           AND ( ahorros.fec_cancela GE w-fecini  AND
                 ahorros.Fec_cancela LE w-fecfin) AND
           ahorros.estado = 2 NO-LOCK:
           FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
           FIND FIRST tmp-deducc   WHERE tmp-deducc.cedula  EQ clientes.nit AND
                                         tmp-deducc.nroobli EQ Ahorros.cue_ahorros NO-ERROR.
           IF NOT AVAILABLE (tmp-deducc) THEN CREATE tmp-deducc.
           ASSIGN tmp-deducc.cod_emp = W_CmbEmp:SCREEN-VALUE  IN FRAME F_Proc
                  tmp-deducc.agepro  = Ahorros.Agencia
                  tmp-deducc.cedula  = clientes.nit
                  tmp-deducc.nombre  = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)  
                  tmp-deducc.nroobli = Ahorros.cue_ahorros
                  tmp-deducc.nompro  = Pro_Ahorros.Nom_Producto
                  tmp-deducc.nrocuo  = 0
                  tmp-deducc.fecdesc = w_fecha
                  tmp-deducc.vdeduc  = ahorros.cuota.
       END. /* For de Ahorros*/
   END. /* For de Clientes*/

   OUTPUT TO VALUE(zpath).
   FOR EACH tmp-deducc: /* Vigentes*/
       REGISTRO =  TRIM(STRING(tmp-deducc.cod_emp)) + ";". 
       IF DECIMAL(SUBSTRING(tmp-deducc.cod_emp,1,4)) = 1 OR 
          DECIMAL(SUBSTRING(tmp-deducc.cod_emp,1,4)) = 4 OR 
          DECIMAL(SUBSTRING(tmp-deducc.cod_emp,1,4)) = 5 THEN
            ASSIGN REGISTRO = TRIM(REGISTRO) + tmp-deducc.codnoved + ";".
       REGISTRO =  TRIM(REGISTRO) +
                   TRIM(tmp-deducc.cedula)          + ";" + 
                   TRIM(tmp-deducc.nombre)          + ";" + 
                   TRIM(STRING(tmp-deducc.agepro))  + ";" +
                   TRIM(STRING(tmp-deducc.nroobli)) + ";" + 
                   TRIM(tmp-deducc.nompro)          + ";" +                                          
                   TRIM(STRING(tmp-deducc.nrocuo))  + ";" + 
                   TRIM(STRING(tmp-deducc.vdeduc))  + ";" + 
                   TRIM(STRING(tmp-deducc.fecdesc)).     
       PUT REGISTRO SKIP.
   END.
   MESSAGE "GENERACION DE CANCELACION DE CREDITOS/CUOTAS AHORRO TERMINO EXITOSAMENTE" SKIP
             "UBICACION DEL ARCHIVO: " zpath  VIEW-AS ALERT-BOX.
  END. /* For de tmp-deducc*/
 END. /* Fin de w_arc NE 1*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Rec_XLibranza
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME Cmb_PagI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_PagI W-Rec_XLibranza
ON VALUE-CHANGED OF Cmb_PagI IN FRAME F_Imp /* Recaudos */
DO:
   ASSIGN W_NroPagI = INTEG(SUBSTRING(Cmb_PagI:SCREEN-VALUE,1,6)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Cmb_pagos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_pagos W-Rec_XLibranza
ON VALUE-CHANGED OF Cmb_pagos IN FRAME F_Proc
DO:
   ASSIGN W_NroPago              = INTEG(SUBSTRING(Cmb_Pagos:SCREEN-VALUE,1,6))
          W_NroPago:SCREEN-VALUE = STRING(W_NroPago)
          W_TotNoD               = 0
          W_TotNoD:SCREEN-VALUE  = STRING(W_TotNoD).


   RUN QueryRec.                                        
END                                                               .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_Opcrea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Opcrea W-Rec_XLibranza
ON MOUSE-SELECT-CLICK OF Rs_Opcrea IN FRAME F_Proc
DO:
    ASSIGN Rs_OpCrea.
    W_NroPago = 1.
    W_NroPago:SCREEN-VALUE = "000001".
    Rs_OpCrea:SENSITIVE = FALSE.

    FIND LAST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa = W_CodEmp
                           AND Rec_Nomina.Nro_Pago > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE(Rec_Nomina) THEN DO:
        W_NroPago = Rec_Nomina.Nro_Pago + 1.
        W_NroPago:SCREEN-VALUE = STRING(Rec_Nomina.Nro_Pago + 1).
    END.

    IF Rs_OpCrea = 1 THEN
        APPLY "ENTRY" TO W_NitCte.
    ELSE DO:
        IF Rs_OpCrea = 2 THEN
            RUN HallaArchivo.
        ELSE DO:
            IF Rs_OpCrea = 3 THEN
                RUN HallaPdctos.
            ELSE DO:
                IF W_CodEmp = 1
                    THEN RUN Hallaas400n.

                IF W_CodEmp = 2 THEN
                    RUN Hallaas400l.
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_Nuevo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_Nuevo W-Rec_XLibranza
ON MOUSE-SELECT-CLICK OF Tg_Nuevo IN FRAME F_Proc
DO:
  ASSIGN Tg_Nuevo
         Rs_OpCrea:SENSITIVE = FALSE.

  IF Tg_Nuevo AND W_CodEmp GT 0 THEN DO:
     ASSIGN Rs_OpCrea:SENSITIVE    = TRUE
            W_NitCte               = " "
            W_NitCte:SCREEN-VALUE  = " "
            W_VrRec                = 0
            W_VrRec:SCREEN-VALUE   = "0.00"
            /*            W_NCuotas              = 1 */
            /*            W_NCuotas:SCREEN-VALUE = "1"  */
            W_TotNoD               = 0
            W_TotNoD:SCREEN-VALUE  = STRING(W_TotNoD).
    
/*      FOR EACH TPdctos. DELETE TPdctos. END.  */
     EMPTY TEMP-TABLE TPdctos.
     CLOSE QUERY Br_Rec.
     CLOSE QUERY Br_Pdctos.
  END.
  ELSE IF Tg_Nuevo THEN 
    MESSAGE "Seleccione una Empresa para el Nuevo Recaudo..." VIEW-AS ALERT-BOX.

  ASSIGN Tg_Nuevo              = FALSE
         Tg_Nuevo:SCREEN-VALUE = "No".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_SemP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_SemP W-Rec_XLibranza
ON VALUE-CHANGED OF Tg_SemP IN FRAME F_Proc /* Semana de Pago? */
DO:
  ASSIGN Tg_SemP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txt_contrapartida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txt_contrapartida W-Rec_XLibranza
ON LEAVE OF txt_contrapartida IN FRAME F_Proc
DO:
  FIND FIRST Cuentas    WHERE Cuentas.Tipo      EQ 2
                          AND Cuentas.Estado    EQ 1
                          AND Cuentas.Cuenta    EQ txt_contrapartida:SCREEN-VALUE NO-LOCK NO-ERROR.


    
    IF NOT AVAIL(Cuentas)  THEN DO:
       MESSAGE "La Cuenta debe existir y estar activa..." SKIP
             "               No se acepta la Operaci�n." VIEW-AS ALERT-BOX ERROR.
       
       RETURN.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frm-FPagaduria
&Scoped-define SELF-NAME w-fecfin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-fecfin W-Rec_XLibranza
ON LEAVE OF w-fecfin IN FRAME frm-FPagaduria /* Fecha Fin novedad */
DO:
  ASSIGN w-fecfin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w-fecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-fecIni W-Rec_XLibranza
ON LEAVE OF w-fecIni IN FRAME frm-FPagaduria /* Fecha Inicial  corte */
DO:
  ASSIGN w-fecini.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME w_anoAplicar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_anoAplicar W-Rec_XLibranza
ON LEAVE OF w_anoAplicar IN FRAME F_Proc /* A�o: */
DO:
    ASSIGN w_anoAplicar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME W_CmbEI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEI W-Rec_XLibranza
ON VALUE-CHANGED OF W_CmbEI IN FRAME F_Imp /* Pagadur�a */
DO:
  ASSIGN W_CodEmpI           = INTEGER(SUBSTRING(W_CmbEI:SCREEN-VALUE,1,4)) 
         Cmb_PagI:LIST-ITEMS = ""
         W_NroPagI           = 0.
 
  FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empr     EQ W_CodEmpI
                            BREAK BY Rec_Nomina.Nro_pago:
      IF FIRST-OF(Rec_Nomina.Nro_pago) THEN 
         Cmb_PagI:ADD-LAST(STRING(Rec_Nomina.Nro_pago,"999999")     + " - " +
                           STRING(Rec_Nomina.Fec_Pago,"99/99/9999") + " - " +
                           STRING(Rec_Nomina.Fec_Contab,"99/99/9999")).
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME W_CmbEmp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEmp W-Rec_XLibranza
ON VALUE-CHANGED OF W_CmbEmp IN FRAME F_Proc /* Pagadur�a */
DO:
    ASSIGN W_CodEmp = INTEGER(SUBSTRING(W_CmbEmp:SCREEN-VALUE,1,4))
           Cmb_Pagos:LIST-ITEMS = ""
           W_NroPago = 0
           W_NroPago:SCREEN-VALUE = "0".

    RUN CalculaPeriodoAnoAplicar.

    RUN ComboPagos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NCuotas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NCuotas W-Rec_XLibranza
ON LEAVE OF W_NCuotas IN FRAME F_Proc
DO:
    W_NCuotas = INTEGER(SELF:SCREEN-VALUE).

    IF W_NCuotas EQ 99 THEN DO:
        MESSAGE "N�mero de Cuotas Inferior a 99"
            VIEW-AS ALERT-BOX.

        RETURN NO-APPLY.
    END.
  ELSE DO:
     /*ASSIGN W_NCuotas.*/
     IF W_NCuotas LE 0 THEN
        W_NCuotas = 1. 
           
     IF W_CodEmp GT 0 AND W_NroPago GT 0 THEN DO:
        FIND FIRST Clientes WHERE Clientes.Nit         EQ W_NitCte
                              AND Clientes.Cod_Empresa EQ W_CodEmp
                              AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.                            
        IF AVAIL(Clientes) THEN DO:                    
           FIND LAST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp
                                  AND Rec_Nomina.Nro_Pago    EQ W_NroPago
                                  AND Rec_Nomina.Nit         EQ W_NitCte
                                  AND Rec_Nomina.Fec_Contab  EQ ? 
                                  AND Rec_Nomina.Num_Cuotas  NE 99 NO-ERROR.
           IF NOT AVAIL(Rec_Nomina) AND W_VrRec GT 0 THEN DO:
              CREATE Rec_Nomina.
              ASSIGN Rec_Nomina.Agencia      = W_Agencia
                     Rec_Nomina.Cod_Empresa  = W_CodEmp       
                     Rec_Nomina.Nro_Pago     = W_NroPago
                     Rec_Nomina.Nit          = W_NitCte 
                     Rec_Nomina.Num_Cuotas   = W_NCuotas
                     Rec_Nomina.Fec_Contab   = ? 
                     Rec_Nomina.Fec_Grabac   = W_Fecha
                     Rec_Nomina.Fec_Pago     = W_Fecha
                     Rec_Nomina.Val_Deducido = W_VrRec
                     W_VrRec                 = 0
                     W_VrRec:SCREEN-VALUE    = "0.00"
                     W_RowId                 = ROWID(Rec_Nomina).                
           END.
           ELSE IF AVAIL(Rec_Nomina) AND (W_VrRec + Rec_Nomina.Val_Deducido) GE 0 THEN
              ASSIGN Rec_Nomina.Num_Cuotas   = W_NCuotas
                     Rec_Nomina.Val_Deducido = Rec_Nomina.Val_Deducido + W_VrRec
                     W_VrRec                 = 0
                     W_VrRec:SCREEN-VALUE    = "0.00"
                     W_RowId                 = ROWID(Rec_Nomina).
           ELSE MESSAGE "El valor para Distribuir debe ser Mayor que 0(Cero)..." SKIP
                        "                         Revise por favor."  VIEW-AS ALERT-BOX.
    
           IF AVAIL(Rec_Nomina) THEN
              W_RowId = ROWID(Rec_Nomina).
    
           APPLY "ENTRY" TO W_NitCte.
               
           RUN QueryRec.
               
           REPOSITION Br_Rec TO ROWID W_RowId.
               
           RUN DistRecaudo. 
        END.
        ELSE MESSAGE "El Nit debe existir Activo en Clientes para esta Empresa" VIEW-AS ALERT-BOX.      
     END.
     ELSE MESSAGE "Debe existir la Empresa y Nro.de Recaudo...Revise por favor."
                   VIEW-AS ALERT-BOX.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitCte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCte W-Rec_XLibranza
ON LEAVE OF W_NitCte IN FRAME F_Proc
DO:
   ASSIGN W_NitCte
          W_NomCte:SCREEN-VALUE = "".

   FIND FIRST Clientes WHERE Clientes.Nit         EQ W_NitCte 
                         AND Clientes.Cod_Empresa EQ W_CodEmp NO-LOCK NO-ERROR.
   IF NOT AVAIL(Clientes) THEN DO:                                                                    
      ASSIGN FRAME F_Proc:SENSITIVE = FALSE.                                                           
                                                                                                    
      RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                        OUTPUT W_NitCte, OUTPUT W_NomCte, OUTPUT W_NomCte, OUTPUT W_Age).       
                                                                                                    
      ASSIGN FRAME F_Proc:SENSITIVE = TRUE.                                                            
                                                                                                    
      FIND FIRST Clientes WHERE Clientes.Nit         EQ W_NitCte
                            AND Clientes.Cod_Empresa EQ W_CodEmp
                            AND Clientes.Estado      EQ 1 /* Nuevo*/ NO-LOCK NO-ERROR.                            
      IF AVAIL(Clientes) THEN                                                                         
         ASSIGN W_NitCte:SCREEN-VALUE = W_NitCte                                                      
                W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +   
                                        " " + TRIM(Clientes.Nombre).                                  
      ELSE                                                                                            
         MESSAGE "El Nit debe existir Activo en Clientes para esta Empresa" VIEW-AS ALERT-BOX.                                 
   END.                                                                                               
   ELSE ASSIGN W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +    
                                       " " + TRIM(Clientes.Nombre).                                   
                                                                                                       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_sem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_sem W-Rec_XLibranza
ON LEAVE OF W_sem IN FRAME F_Proc /* Periodo: */
DO:
    ASSIGN W_Sem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_VrRec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrRec W-Rec_XLibranza
ON LEAVE OF W_VrRec IN FRAME F_Proc
DO:
   ASSIGN W_VrRec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define BROWSE-NAME Br_Pdctos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Rec_XLibranza 


ASSIGN CURRENT-WINDOW = {&WINDOW-NAME}
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

ON CLOSE OF THIS-PROCEDURE
    RUN disable_UI.

PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.

    FIND FIRST Agencias WHERE Agencias.Agencia EQ W_Agencia

      /* oakley */

                        AND Agencias.Estado  EQ 1  NO-LOCK NO-ERROR.
  IF NOT AVAIL(Agencias) THEN DO:
     MESSAGE "La Agencia no est� disponible para realizar Transacciones..." VIEW-AS ALERT-BOX ERROR.

     APPLY "CLOSE":U TO THIS-PROCEDURE.
     RETURN NO-APPLY.
  END.
/*   ELSE W_CmbOfi:SCREEN-VALUE = (STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(40)")).  */

  W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
  
  ASSIGN W_OfiIni       = W_Agencia
         W_OfiFin       = W_Agencia
         Msaje:VISIBLE  = FALSE.
                  
  FOR EACH Agencias WHERE Agencias.Estado  EQ 1
                      AND Agencias.Agencia GT 0 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(20)")).
      

      IF Agencias.Agencia EQ W_Agencia THEN
         W_CmbOfi:SCREEN-VALUE = (STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(20)")).
  END.              
    
  
  FOR EACH Empresas WHERE Empresas.Estado      EQ 1
                      AND Empresas.Cod_empresa GT 0 
                      AND Empresas.Agencia     EQ w_agencia NO-LOCK:
     FIND FIRST Clientes WHERE Clientes.Nit    EQ Empresas.Nit
                            AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
        
      IF AVAIL(Clientes) THEN DO: 
         W_CmbEmp:ADD-LAST(STRING(Empresas.Cod_empresa,"9999") + "-" +
                           STRING(Empresas.ALIAS_Empresa,"X(45)")).                   
         W_CmbEI:ADD-LAST(STRING(Empresas.Cod_empresa,"9999") + "-" +
                           STRING(Empresas.ALIAS_Empresa,"X(45)")).
     END.
  END. 


     
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
     WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Abonar W-Rec_XLibranza 
PROCEDURE Abonar :
IF TPdctos.TP = "A" THEN DO:
    FIND FIRST Ahorros WHERE Ahorros.Nit = W_NitCte
                         AND Ahorros.Cod_Ahorro = TPdctos.CodP
                         AND Ahorros.Cue_Ahorro = TPdctos.CtaP
                         AND Ahorros.Estado = 1 NO-ERROR.
    IF AVAILABLE(Ahorros) THEN
        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
                                 AND Pro_Ahorros.Estado = 1 NO-LOCK NO-ERROR.

    IF NOT AVAILABLE(Ahorros) OR NOT AVAILABLE(Pro_Ahorros) THEN DO:
        MESSAGE "Falta Pro_Ahorros:" TPdctos.CodP SKIP
                "O falta la cuenta:" TPdctos.CtaP ", del Cliente con C�dula.:" W_NitCte SKIP
                "para realizar el abono del recaudo..."
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    FIND FIRST TempCtas WHERE TempCtas.Agen = Ahorros.Agencia
                          AND TempCtas.TipP = "A"
                          AND TempCtas.Pto = Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(TempCtas) THEN DO:
        MESSAGE "Falta configuraci�n con producto_Ahorro:" Ahorros.Cod_Ahorro SKIP
                "para la Agencia:" Ahorros.Agencia
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + TPdctos.VrDist.
    ahorros.cuota = ROUND(TPdctos.VrDist / W_Ncuotas,0).
    Ahorros.Fec_UltTrans = W_Fecha.
    Ahorros.Num_DepMes = Ahorros.Num_DepMes + 1.
    Ahorros.Val_DepMes = Ahorros.Val_DepMes + TPdctos.VrDist.
    Ahorros.Val_DepDia = Ahorros.Val_DepDia + TPdctos.VrDist.
    Ahorros.Num_DepDia = Ahorros.Num_DepDia + 1.

    RUN MovAhorro.
    RUN ContablesAhorro NO-ERROR. /* oakley */
    
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
  END.
  ELSE DO:   
     FIND Creditos WHERE Creditos.Nit         EQ W_NitCte
                     AND Creditos.Cod_Credito EQ TPdctos.CodP
                     AND Creditos.Num_Credito EQ INTEG(TPdctos.CtaP)
                     AND Creditos.Sdo_Capital GT 0 
                     AND Creditos.Estado = 2 NO-LOCK NO-ERROR.
     /* para abonar prima extraordinaria */
    /* IF AVAIL(Creditos) THEN DO:
        RUN DRAboCredito.R         /*Distribuye abonos en Cr�ditos,graba Mov_creditos,Mov_Contab y PlanPagos*/
            (INPUT TRUE,
             INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
             INPUT Creditos.Num_Credito,TPdctos.VrDist,
             INPUT Comprobantes.Comprobante,Comprobantes.Secuencia,0,
             OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IMorDifC, OUTPUT P_IMora,
             OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capit,
             OUTPUT P_VlrNoDist) NO-ERROR.*/

     IF AVAIL(Creditos) THEN DO:
        /* F�lix Vargas - Nov/29/2007 */
        FIND LAST PlanPagos WHERE 
                  PlanPagos.Agencia      EQ Creditos.Agencia    
              AND PlanPagos.Nit          EQ Creditos.Nit        
              AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
              AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
              AND PlanPagos.Id_PdoMes    EQ 1 NO-ERROR.
        IF NOT AVAILABLE(PlanPagos) THEN DO:
           FIND CURRENT Creditos EXCLUSIVE-LOCK NO-ERROR.
           RUN creaplanpagos NO-ERROR.
           FIND CURRENT Creditos NO-LOCK NO-ERROR.
        END.
        /******************/

        RUN AboCredito.R         /*Distribuye abonos en Cr�ditos,graba Mov_creditos,Mov_Contab y PlanPagos*/
            (INPUT TRUE,
             INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
             INPUT Creditos.Num_Credito,TPdctos.VrDist,
             INPUT Comprobantes.Comprobante,Comprobantes.Secuencia,0,1,
             OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IMorDifC, OUTPUT P_IMora,
             OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capit,
             OUTPUT P_VlrNoDist) NO-ERROR.

     IF ERROR-STATUS:ERROR THEN
           RETURN ERROR.
     END.
     ELSE DO:
         MESSAGE "No se hall� el Cr�dito vigente para Distribuir el Pago..." SKIP
                 "Del Nit. : " W_NitCte ", Cod_producto : " TPdctos.CodP ", Nro-Cr�dito : " TPdctos.CtaP SKIP
                 "                        Revise por favor...Distribuci�n cancelada."
                 VIEW-AS ALERT-BOX ERROR.

         RETURN ERROR.
     END.

     IF P_VlrNoDist GT 0 OR P_VlrNoDist LT 0 THEN DO:
        MESSAGE "El Programa AboCreditos.P...Retorn� valor no distribuido para" SKIP
                "el Nit. : " W_NitCte ", Cod_producto : " TPdctos.CodP ", Nro-Cr�dito : " TPdctos.CtaP SKIP
                "                        Revise por favor...Distribuci�n cancelada."
                VIEW-AS ALERT-BOX ERROR.
        
        RETURN ERROR.
     END.

     FIND FIRST TempCtas WHERE TempCtas.Agen EQ Creditos.Agencia                      
                           AND TempCtas.TipP EQ "C"                                       
                           AND TempCtas.Pto  EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.    
     IF NOT AVAIL(TempCtas) THEN DO:
        MESSAGE "Falta Configuraci�n con Producto_Credito: " Creditos.Cod_Credito SKIP
                "Para la Agencia : " Creditos.Agencia
                VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
     END.
     
     IF TempCtas.Agen NE W_Agencia THEN 
        RUN CompletaSyA.                                                                                              
                                                              
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalculaPeriodoAnoAPlicar W-Rec_XLibranza 
PROCEDURE CalculaPeriodoAnoAPlicar :
/* Cargo el periodo y a�o a distribuir */
FOR EACH ctr_ProcDistribucion WHERE ctr_ProcDistribucion.agencia = w_agencia
                                AND ctr_ProcDistribucion.cod_empresa = W_CodEmp NO-LOCK BY ctr_ProcDistribucion.ano DESCENDING
                                                                                        BY ctr_ProcDistribucion.periodo DESCENDING:
    periodoAplicar = ctr_ProcDistribucion.periodo + 1.
    anoAplicar = ctr_procDistribucion.ano.
        
    FIND FIRST empresas WHERE empresas.cod_empresa = W_CodEmp NO-LOCK NO-ERROR.
    IF (empresas.FOR_pago = 4 AND periodoAplicar > 12) OR
       (empresas.FOR_pago = 3 AND periodoAplicar > 24) OR
       (empresas.FOR_pago = 2 AND periodoAplicar > 36) OR
       (empresas.FOR_pago = 1 AND periodoAplicar > 52) THEN
        ASSIGN periodoAplicar = 1
               anoAplicar = ctr_procDistribucion.ano + 1.

    w_sem:SCREEN-VALUE IN FRAME F_Proc = STRING(periodoAplicar).
    w_anoAplicar:SCREEN-VALUE = STRING(anoAplicar).

    LEAVE.
END.

IF w_sem:SCREEN-VALUE = "" THEN DO:
    w_sem:SCREEN-VALUE = "1".
    periodoAplicar = 1.
END.

IF w_anoAplicar:SCREEN-VALUE = "" THEN DO:
    w_anoAplicar:SCREEN-VALUE = STRING(YEAR(w_fecha)).
    anoAPlicar = YEAR(w_fecha).
END.

ASSIGN w_sem.
ASSIGN w_anoAplicar.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CambioExaso W-Rec_XLibranza 
PROCEDURE CambioExaso :
FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa = W_CodEmp
                      AND Rec_Nomina.Nro_Pago = W_NroPago
                      AND Rec_Nomina.Num_Cuotas = 99 BY Rec_Nomina.Nit:
    FIND FIRST Clientes WHERE clientes.nit = Rec_Nomina.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) AND Clientes.Estado = 1 AND Clientes.Cod_Empresa = Rec_Nomina.Cod_Empresa THEN
        Rec_Nomina.Num_Cuotas  = 1.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ComboPagos W-Rec_XLibranza 
PROCEDURE ComboPagos :
Cmb_Pagos:LIST-ITEMS IN FRAME F_Proc = "".

FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empr = W_CodEmp
                      AND Rec_Nomina.Fec_Contabilizacion = ?
                      AND rec_nomina.carnet = "1" NO-LOCK BREAK BY Rec_Nomina.Nro_pago:
    IF FIRST-OF(Rec_Nomina.Nro_pago) THEN DO:
        Cmb_Pagos:ADD-LAST(STRING(Rec_Nomina.Nro_pago,"999999") + " - " + STRING(Rec_Nomina.Fec_Pago,"99/99/9999")).
        
        IF Rec_Nomina.Nro_Pago = W_NroPago THEN
            Cmb_Pagos:SCREEN-VALUE = STRING(Rec_Nomina.Nro_pago,"999999") + " - " + STRING(Rec_Nomina.Fec_Pago,"99/99/9999").
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompletaExasoSyA W-Rec_XLibranza 
PROCEDURE CompletaExasoSyA :
/*------------------------------------------------------------------------------
  Purpose: Graba Sucursales y Agencias - de Exasociados     
  Fecha  : 19 de Octubre de 2007
------------------------------------------------------------------------------*/
DEFINE VARIABLE w_totrecsya    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE w_totgenrecsya AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.

FIND FIRST Cuentas WHERE
    Cuentas.Tipo   EQ 2 AND
    Cuentas.Cuenta EQ "1904159930" AND
    Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp                             
                    AND Rec_Nomina.Nro_Pago      EQ W_NroPago                            
                    AND Rec_Nomina.Num_Cuotas    EQ 99 
                    BY Rec_Nomina.Nit.
    FOR EACH Clientes WHERE Clientes.nit EQ Rec_Nomina.Nit NO-LOCK:
       IF Clientes.Agencia NE W_Agencia THEN DO: /* Sucur y Agencia*/
           FIND FIRST tmp-exasocSyA WHERE 
                tmp-exasocSyA.cod_age EQ Clientes.Agencia AND
                tmp-exasocSyA.Nit     EQ Rec_Nomina.Nit /*STRING(W_Agencia,"999")*/ NO-ERROR.
           IF NOT AVAIL(tmp-exasocSyA) THEN DO:
              CREATE tmp-exasocSyA.
              ASSIGN tmp-exasocSyA.cod_age  = Clientes.Agencia
                     tmp-exasocSyA.nit      = Clientes.Nit /*STRING(W_Agencia,"999")*/ 
                     tmp-exasocSyA.CtaPro   = Cuentas.Cuenta
                     tmp-exasocSyA.valrec   = Rec_Nomina.Val_Deducido.
           END.
           ELSE
              ASSIGN tmp-exasocSyA.valrec   = tmp-exasocSyA.valrec + Rec_Nomina.Val_Deducido.
       END. /* Fin Sucur y Agencia*/
    END.
END.

ASSIGN w_totrecsya    = 0
       w_totgenrecsya = 0.

FOR EACH tmp-exasocSyA BREAK BY tmp-exasocSyA.cod_age :
    IF FIRST-OF(tmp-exasocSyA.cod_age) THEN
       ASSIGN w_totrecsya = 0.

    ASSIGN w_totrecsya = w_totrecsya + tmp-exasocSyA.valrec.
    CREATE Mov_Contable.                
    ASSIGN Mov_Contable.Agencia        = tmp-exasocSyA.cod_age
           Mov_Contable.Cuenta         = Cuentas.Cuenta
           Mov_Contable.Nit            = tmp-exasocSyA.nit
           Mov_Contable.Fec_Contable   = W_Fecha                            
           Mov_Contable.Comentario     = "Recaudo N�mina de Exasociados SyA - Mes." + w_sem
           Mov_Contable.Usuario        = W_Usuario                          
           Mov_Contable.Cen_Costos     = W_Cencosgral                    
           Mov_Contable.Destino        = tmp-exasocSyA.cod_age                          
           Mov_Contable.Comprobante    = Comprobantes.Comprobante           
           Mov_Contable.Num_Documento  = Comprobantes.Secuencia             
           Mov_Contable.Fec_Grabacion  = TODAY                              
           Mov_Contable.Hora           = TIME                               
           Mov_Contable.Estacion       = W_Estacion                         
           Mov_Contable.Db             = tmp-exasocSyA.valrec. /*w_totrecsya.*/

    IF LAST-OF(tmp-exasocSyA.cod_age) THEN DO:
       ASSIGN w_totgenrecsya = w_totgenrecsya + w_totrecsya.
       CREATE Mov_Contable.                                                                                 
       ASSIGN Mov_Contable.Agencia        = W_Agencia
              Mov_Contable.Cuenta         = Cuentas.Cuenta
              Mov_Contable.Nit            = STRING(tmp-exasocSyA.cod_age,"999")
              Mov_Contable.Fec_Contable   = W_Fecha                                                         
              Mov_Contable.Comentario     = "Recaudo N�mina de Exasociados SyA - Mes." + w_sem
              Mov_Contable.Usuario        = W_Usuario                                                      
              Mov_Contable.Cen_Costos     = W_Cencosgral                                                   
              Mov_Contable.Destino        = W_Agencia                                                       
              Mov_Contable.Comprobante    = Comprobantes.Comprobante                                        
              Mov_Contable.Num_Documento  = Comprobantes.Secuencia                                          
              Mov_Contable.Fec_Grabacion  = TODAY                                                           
              Mov_Contable.Hora           = TIME                                                           
              Mov_Contable.Estacion       = W_Estacion                                                      
              Mov_Contable.Cr             = w_totrecsya.
    END.
END.
/* IF w_totgenrecsya GT 0 THEN DO:                                                            */
/*    CREATE Mov_Contable.                                                                    */
/*    ASSIGN Mov_Contable.Agencia        = W_Agencia                                          */
/*           Mov_Contable.Cuenta         = Cuentas.Cuenta                                     */
/*           Mov_Contable.Nit            = tmp-exasocSyA.nit                                  */
/*           Mov_Contable.Fec_Contable   = W_Fecha                                            */
/*           Mov_Contable.Comentario     = "Recaudo N�mina de Exasociados SyA - Mes." + w_sem */
/*           Mov_Contable.Usuario        = W_Usuario                                          */
/*           Mov_Contable.Cen_Costos     = W_Cencosgral                                       */
/*           Mov_Contable.Destino        = W_Agencia                                          */
/*           Mov_Contable.Comprobante    = Comprobantes.Comprobante                           */
/*           Mov_Contable.Num_Documento  = Comprobantes.Secuencia                             */
/*           Mov_Contable.Fec_Grabacion  = TODAY                                              */
/*           Mov_Contable.Hora           = TIME                                               */
/*           Mov_Contable.Estacion       = W_Estacion                                         */
/*           Mov_Contable.Cr             = w_totgenrecsya.                                    */
/* END.                                                                                       */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompletaSyA W-Rec_XLibranza 
PROCEDURE CompletaSyA :
DEFINE VAR W_OtraAg AS INTEGER.

/* Contabilidad en la agencia del producto */
FIND FIRST CopMov_Contable WHERE CopMov_Contable.Agencia = TempCtas.Agen
                             AND CopMov_Contable.Cuenta = TempCtas.CtaSyA
                             AND CopMov_Contable.Db > 0 NO-ERROR.
IF NOT AVAILABLE(CopMov_Contable) THEN DO:
    CREATE CopMov_Contable.
    CopMov_Contable.Agencia = TempCtas.Agen.
    CopMov_Contable.Cuenta = TempCtas.CtaSyA.
    CopMov_Contable.Nit = STRING(W_Agencia,"999").
    CopMov_Contable.Fec_Contable = W_Fecha.
    CopMov_Contable.Comentario = "Distribuci�n de Recaudo".
    CopMov_Contable.Usuario = W_Usuario.
    CopMov_Contable.Cen_Costos = W_Cencosgral.
    CopMov_Contable.Destino = W_Agencia.
    CopMov_Contable.Comprobante = Comprobantes.Comprobante.
    CopMov_Contable.Num_Documento = Comprobantes.Secuencia.
    CopMov_Contable.Fec_Grabacion = TODAY.
    CopMov_Contable.Hora = TIME.
    CopMov_Contable.Estacion = W_Estacion.
END.

CopMov_Contable.Db = CopMov_Contable.Db + TPdctos.VrDist.

W_OtraAg = TempCtas.Agen.


/* Contabilidad en la agencia donde se est� aplicando la N�mina */
FIND FIRST TempCtas WHERE TempCtas.Agen = W_Agencia
                      AND TempCtas.TipP = TPdctos.TP
                      AND TempCtas.Pto = TPdctos.CodP NO-LOCK NO-ERROR.

FIND FIRST CopMov_Contable WHERE CopMov_Contable.Agencia = W_Agencia
                             AND CopMov_Contable.Cuenta = TempCtas.CtaSyA
                             AND CopMov_Contable.Nit = STRING(W_OtraAg,"999")
                             AND CopMov_Contable.Cr > 0 NO-ERROR.
IF NOT AVAILABLE(CopMov_Contable) THEN DO:
    CREATE CopMov_Contable.
    CopMov_Contable.Agencia = W_Agencia.
    CopMov_Contable.Cuenta = TempCtas.CtaSyA.
    CopMov_Contable.Nit = STRING(W_OtraAg,"999").
    CopMov_Contable.Fec_Contable = W_Fecha.
    CopMov_Contable.Comentario = "Distribuci�n de Recaudo".
    CopMov_Contable.Usuario = W_Usuario.
    CopMov_Contable.Cen_Costos = W_Cencosgral.
    CopMov_Contable.Destino = W_OtraAg.
    CopMov_Contable.Comprobante = Comprobantes.Comprobante.
    CopMov_Contable.Num_Documento = Comprobantes.Secuencia.
    CopMov_Contable.Fec_Grabacion = TODAY.
    CopMov_Contable.Hora = TIME.
    CopMov_Contable.Estacion = W_Estacion.
END.

CopMov_Contable.Cr = CopMov_Contable.Cr + TPdctos.VrDist.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar W-Rec_XLibranza 
PROCEDURE Contabilizar :
DEFINE VAR TotT AS DECIMAL.
DEFINE VAR Listado AS CHARACTER FORMAT "X(40)".
DEFINE VAR W_SiErr AS LOGICAL.

W_Cont = 0.
W_Cont:SCREEN-VALUE IN FRAME F_Proc = "0".

TranConta:
REPEAT TRANSACTION ON ERROR UNDO TranConta, LEAVE TranConta:
    Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
    Empresas.Consecutivo_Pago = Empresas.Consecutivo_Pago + 1.
    Empresas.Fec_UltPago[1] = W_Fecha.

    FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
    FIND CURRENT Empresas NO-LOCK NO-ERROR.

    FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa = W_CodEmp
                          AND Rec_Nomina.Nro_Pago = W_NroPago
                          AND Rec_Nomina.Num_Cuotas <> 99
                          AND Rec_Nomina.Fec_Contab = ?:
        IF rec_nomina.carnet <> "1" THEN
            NEXT.

        IF Rec_Nomina.Val_Deduc > 0 THEN DO:
            W_NitCte = Rec_Nomina.Nit.
            W_Cont = W_Cont + 1.

            W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).

            RUN DistRecaudo NO-ERROR. /* oakley */
            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "Error en distribucion recuado"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.

            ASSIGN W_TotNoD = W_TotNoD + W_VrADist
                   W_TotNoD:SCREEN-VALUE = STRING(W_TotNoD)
                   Rec_Nomina.Fec_Contab = W_Fecha.

            Rec_Nomina.Fec_Contabilizacion = W_Fecha.
        END.
    END.

    FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa = W_CodEmp
                          AND Rec_Nomina.Nro_Pago = W_NroPago
                          AND Rec_Nomina.Num_Cuotas = 99
                          AND Rec_Nomina.Fec_Contab = ?:
        IF rec_nomina.carnet <> "1" THEN
            NEXT.

        Rec_Nomina.Fec_Contabilizacion = W_Fecha.
    END.

    RUN ImpExaso.
    RUN InfSinRema.
    EMPTY TEMP-TABLE Tmp_SinRema.

    FIND FIRST Cuentas WHERE Cuentas.Tipo = 2
                         AND Cuentas.Cuenta = txt_contrapartida:SCREEN-VALUE
                         AND Cuentas.Estado = 1 NO-LOCK NO-ERROR.

    FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ W_CodEmp NO-ERROR.

    IF NOT AVAILABLE(Cuentas) OR NOT AVAILABLE(Empresas) THEN DO:
        MESSAGE "La Cuenta de Caja y la Empresa deben existir Activas..." SKIP
                "               No se acepta la Operaci�n." VIEW-AS ALERT-BOX ERROR.

        ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor"
               W_Inf = FALSE
               W_SiContab = FALSE.

        RETURN.
    END.
    
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = W_Agencia
           Mov_Contable.Cuenta = Cuentas.Cuenta
           Mov_Contable.Nit = Empresas.Nit
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = "Recaudo Mes.." + w_sem:SCREEN-VALUE
           Mov_Contable.Usuario = W_Usuario
           Mov_Contable.Cen_Costos = W_Cencosgral
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Comprobante = Comprobantes.Comprobante
           Mov_Contable.Num_Documento = Comprobantes.Secuencia
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.Db = W_ContabDistSi + W_ContabDistNo.

    IF W_ContabDistNo > 0 THEN DO:
        CREATE Mov_Contable.
        ASSIGN Mov_Contable.Agencia = W_Agencia
               Mov_Contable.Cuenta = Cuentas.Cuenta
               Mov_Contable.Nit = Empresas.Nit
               Mov_Contable.Fec_Contable = W_Fecha
               Mov_Contable.Comentario = "NO-Distrib. Mes." + w_sem:SCREEN-VALUE
               Mov_Contable.Usuario = W_Usuario
               Mov_Contable.Cen_Costos = W_Cencosgral
               Mov_Contable.Destino = W_Agencia
               Mov_Contable.Comprobante = Comprobantes.Comprobante
               Mov_Contable.Num_Documento = Comprobantes.Secuencia
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Hora = TIME
               Mov_Contable.Estacion = W_Estacion
               Mov_Contable.Cr = W_ContabDistNo.
    END.

    FOR EACH CopMov_Contable:
        CREATE Mov_Contable.
        BUFFER-COPY CopMov_Contable TO Mov_Contable.

        DELETE CopMov_Contable.
    END.

    FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante = Comprobantes.Comprobante
                            AND Mov_Contable.Num_Documento = Comprobantes.Secuencia
                            AND Mov_Contable.Fec_Contable = W_Fecha NO-LOCK BREAK BY Mov_Contable.Agencia:
        TotT = TotT + (Mov_Contable.Db - Mov_Contable.Cr).

        IF LAST-OF(Mov_Contable.Agencia) AND TotT <> 0 THEN DO:
            MESSAGE "Los DEBE - HABER en la Agencia : " Mov_Contable.Agencia SKIP
                    "                 Est�n Diferentes...Revise por favor."
                VIEW-AS ALERT-BOX ERROR.

            W_SiErr = TRUE.
        END.
    END.

END.

ASSIGN listado = W_PathSpl + "CpteDRContab-" + STRING(W_CodEmp) + "-" + STRING(W_NroPago)  + ".Lst"
       W_Sicpte = TRUE.

{Incluido\Imprimir.I "listado"}

W_Sicpte = FALSE.

IF W_SiErr THEN DO:
    MESSAGE "Error por Debe - Haber diferentes...."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    RETURN ERROR.
END.

/*LEAVE.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContablesAhorro W-Rec_XLibranza 
PROCEDURE ContablesAhorro :
CREATE Mov_Contable.
Mov_Contable.Agencia = TempCtas.Agen.
Mov_Contable.Cuenta = TempCtas.CtaPro.
Mov_Contable.Nit = W_NitCte.
Mov_Contable.Fec_Contable = W_Fecha.
Mov_Contable.Comentario = "Recaudo periodo " + w_sem.
Mov_Contable.Usuario = W_Usuario.
Mov_Contable.Cen_Costos = W_Cencosgral.
Mov_Contable.Destino = W_Agencia.
Mov_Contable.Comprobante = Comprobantes.Comprobante.
Mov_Contable.Num_Documento = Comprobantes.Secuencia.
Mov_Contable.Fec_Grabacion = TODAY.
Mov_Contable.Hora = TIME.
Mov_Contable.Estacion = W_Estacion.
Mov_Contable.Cr = TPdctos.VrDist.

IF TempCtas.Agen <> W_Agencia THEN
    RUN CompletaSyA.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreaPlanPagos W-Rec_XLibranza 
PROCEDURE CreaPlanPagos :
/*------------------------------------------------------------------------------
  Purpose:  Crear el Plan de Pagos de un Cr�dito en Particular   
  Parameters: F�lix Vargas 
  Notes: 29/Nov/2007.       
------------------------------------------------------------------------------*/
    ASSIGN xFecIni = Creditos.Fec_Desembolso.
    IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desembolso THEN
      ASSIGN xFecIni = Creditos.Fec_PagAnti.
    
    ASSIGN Per_pago = 4.
          /* Creditos.Fec_Desembolso = Creditos.Fec_Aprobac
           Creditos.Fec_PagAnt     = Creditos.Fec_Aprobac.*/
    
/*     IF Creditos.Monto LT Creditos.Sdo_Capital THEN  */
/*        Creditos.Monto = Creditos.Sdo_Capital.       */
    
    IF Creditos.Sistema NE 1 AND (Creditos.Cuota LT Creditos.Monto) THEN
       Creditos.Sistema = 1.
    
    ASSIGN W_FecTra   = xfecini
           W_FecIniP   = xfecini
/*            Creditos.Sdo_CapPag     = Creditos.Monto - Creditos.Sdo_Capital  */
/*            Creditos.Val_Desembolso = Creditos.Monto                         */
/*            Creditos.Sdo_Proyectado = Creditos.Monto                         */
/*            Creditos.Capital_Acum   = 0                                      */
/*            Creditos.Int_LiqAcum    = 0                                      */
/*            Creditos.Sdo_Intpag     = 0                                      */
/*            Creditos.Cuo_Pagadas    = 0                                      */
/*            Creditos.Cuo_Atraso     = 0                                      */
/*            Creditos.Dias_Atraso    = 0                                      */
/*            Creditos.Val_Atraso     = 0                                      */
/*            Creditos.Estado         = 2                                      */
/*            /*Creditos.Fec_Aprobacion = Creditos.Fec_Desembolso*/            */
/*            Creditos.For_Interes    = 1                                      */
           W_PdoLiq = 12    /*Inicia mensual*/
           W_DiaPdo = 30.
    
    IF Creditos.Per_Pago EQ 1 THEN
       ASSIGN W_DiaPdo = 7
              W_PdoLiq = 52.
    ELSE IF Creditos.Per_Pago EQ 2 THEN
       ASSIGN W_DiaPdo = 10
              W_PdoLiq = 36.
    ELSE IF Creditos.Per_Pago EQ 3 THEN
       ASSIGN W_DiaPdo = 15
              W_PdoLiq = 24.
    
    ASSIGN /*Creditos.Fec_Pago = xfecini + W_DiaPdo*/
           W_Tasa            = xtas / (W_PdoLiq * 100). 
    
    IF Creditos.Sistema         EQ 2 
    OR Creditos.Plazo           EQ 1
    OR (Creditos.Sdo_Capital GE Creditos.Monto) THEN DO:
    
/*        ASSIGN Creditos.Cuo_Pagadas = 0.  */
    
       IF Creditos.Sistema EQ 2 AND Creditos.Plazo GT 1 THEN DO
          NN = 1 TO Creditos.Plazo:
    
          RUN Halla_FecVcto.R (INPUT  W_FecIniP,W_DiaPdo,W_FecTra,   
                               OUTPUT W_FecTra).  
/*           Creditos.Fec_Pago = W_FecTra.  */
       END.
       ELSE DO: /* Sale */
          RUN Halla_FecVcto.R (INPUT  W_FecIniP,W_DiaPdo,W_FecTra,   
                               OUTPUT W_FecTra).  
/*           Creditos.Fec_Pago = W_FecTra.  */
       END.            
    END.
    
    IF Creditos.Sistema EQ 2 OR Creditos.Plazo   EQ 1 THEN DO:
       RUN CrearPlan.
       ASSIGN PlanPagos.Id_PdoMes         = 1                      
              PlanPagos.Nro_Cuota         = 1                      
              PlanPagos.Fec_Ini           = xfecini
              PlanPagos.Fec_Vcto          = Creditos.Fec_Pago      
              PlanPagos.Fec_ProxPag       = Creditos.Fec_Pago      
              PlanPagos.Cuo_Pagas         = 0
              PlanPagos.Pagos_CapitalAcum = Creditos.Sdo_CapPag
              PlanPagos.Pagos_CapitalPdo  = Creditos.Sdo_CapPag
              PlanPagos.Int_LiqPdo        = Creditos.Int_Corrientes + Creditos.Int_DifCobr.
    
       IF TODAY GE Creditos.Fec_Pago THEN    /*Ya est� vencido totalmente*/
          ASSIGN /*Creditos.Sdo_Proyectado = 0
                 Creditos.Capital_Acum   = Creditos.Monto
                 Creditos.Int_LiqAcum    = Creditos.Int_Corrientes + Creditos.Int_DifCobro  */
                 PlanPagos.Capital_Acum  = Creditos.Monto
                 PlanPagos.Int_LiqAcum   = Creditos.Int_Corrientes + Creditos.Int_DifCobr.
                 /*Creditos.Cuo_Atraso     = 1
                 Creditos.Dias_Atraso    = TODAY - Creditos.Fec_Pago
                 Creditos.Val_Atraso     = Creditos.Sdo_Capital.         */
       NEXT.
    END.
    ELSE DO:
        ASSIGN KSdo     = Creditos.Sdo_Capital
               KCuo     = Creditos.Cuota      
               CuoFalt  = Creditos.Plazo
               W_FecTra   = xfecini
               W_FecIniP   = xfecini.
    END.
    IF (Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
        Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
        Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado) LE Creditos.Cuota THEN 
        ASSIGN /*Creditos.Cuo_Pagadas = Creditos.Plazo - 1*/
               CuoFalt              = 1.
    ELSE IF Creditos.Sdo_Capital LT Creditos.Monto THEN DO:
        DO NN = 1 TO Creditos.Plazo:   /*Con Sdo-Capital halla Faltantes*/
           ASSIGN KInt = ROUND(KSdo * W_Tasa,0)
                  KCap = ROUND(KCuo - KInt,0)
                  KSdo = KSdo - KCap.
    
           IF KSdo LE 0 THEN DO:  
              CuoFalt = NN.       
              LEAVE.
           END.
    
           CuoFalt = NN.
        END.
    
/*         Creditos.Cuo_Pagadas = Creditos.Plazo - CuoFalt.   /*Plazo menos faltantes son pagadas*/  */
    END.
    
    DO NN = 1 TO (Creditos.Cuo_Pagadas):  /*Halla Fecha pr�ximo pago con base en Pagadas*/ 
       RUN Halla_FecVcto.R (INPUT  W_FecIniP,W_DiaPdo,W_FecTra,   
                            OUTPUT W_FecTra).  
/*        Creditos.Fec_Pago = W_FecTra.  */
    END.
    
    ASSIGN W_FecTra   = xfecini  
           W_FecIniP   = xfecini
           /*KSdo       = Creditos.Sdo_Capital*/
           KSdo       = Creditos.Monto
           KCuo       = Creditos.Cuota
           KIAcu      = 0
           KCAcu      = 0
           PdoTrans   = 0
           KInt       = 0
           KCap       = 0.
    
    DO NN = 1 TO Creditos.Plazo:    /*Halla la Cuota(Pdo.) que transcurre y Sdo_proy con base Monto*/ 
       ASSIGN KInt  = ROUND(KSdo * W_Tasa,0)
              KCap  = ROUND(KCuo - KInt,0) 
              KIAcu = KIAcu + KInt
              KCAcu = KCAcu + KCap
              KSdo  = KSdo  - KCap
              W_FecIniCont = W_FecTra.
    
       RUN Halla_FecVcto.R (INPUT  W_FecIniP,W_DiaPdo,W_FecTra,   
                            OUTPUT W_FecTra). 
    
       IF W_FecTra GE TODAY THEN DO:
          PdoTrans = NN.
          LEAVE.
       END.  
    
       /*ASSIGN KInt  = ROUND(KSdo * W_Tasa,0)
              KCap  = ROUND(KCuo - KInt,0) 
              KIAcu = KIAcu + KInt
              KCAcu = KCAcu + KCap
              KSdo  = KSdo  - KCap.*/
    END.
    
    IF PdoTrans LE 0 AND xfecini LT (TODAY - W_DiaPdo) THEN
       ASSIGN PdoTrans = Creditos.Plazo + 1.
    ELSE IF PdoTrans LE 0 THEN
       PdoTrans = 1.
    
    ASSIGN W_FecTra   = xfecini  
           W_FecIniP   = xfecini.             
    
    IF PdoTrans GT 1 THEN DO NN = 1 TO PdoTrans - 1:     /*Halla la Fec-Ini del que transcurre - 1*/ 
       ASSIGN W_FecIniCont = W_FecTra.
    
       RUN Halla_FecVcto.R (INPUT  W_FecIniP,W_DiaPdo,W_FecTra,   
                            OUTPUT W_FecTra). 
    END.            
    
    ASSIGN W_SiPdoTr  = FALSE
           KSdo       = Creditos.Sdo_Capital
           KCuo       = Creditos.Cuota
           W_FecIniP   = W_FecIniCont
           W_FecTra   = W_FecIniCont. 
    
    DO NN = (PdoTrans - 1) TO Creditos.Plazo + 1:  /*Genera los Reg.del PlanPagos*/ 
       RUN CrearPlan.
       ASSIGN PlanPagos.Nro_Cuota = NN                      
              PlanPagos.Fec_Ini   = W_FecTra
              PlanPagos.Fec_Vcto  = W_FecTra.
    
       IF NN GT 0 THEN DO:
          RUN Halla_FecVcto.R (INPUT  W_FecIniP,W_DiaPdo,W_FecTra,   
                               OUTPUT W_FecTra).
          PlanPagos.Fec_Vcto = W_FecTra.
       END.
    
       IF NN LT PdoTrans THEN
          ASSIGN PlanPagos.Id_PdoMes         = 2   /*Ya Cumplido*/
                 /*Creditos.Capital_Acum       = KCAcu - KCap
                 Creditos.Int_LiqAcum        = KIAcu - KInt*/
                 PlanPagos.Capital_Acum      = KCAcu - KCap
                 /*Creditos.Sdo_Proyectado     = Creditos.Monto - Creditos.Capital_Acum*/
                 PlanPagos.Int_LiqAcum       = KIAcu - KInt
                 PlanPagos.Int_LiqPdo        = KInt
                 PlanPagos.Capital_Pdo       = KCap
                 PlanPagos.Pagos_CapitalPdo  = Creditos.Sdo_CapPag.
       ELSE IF NN EQ PdoTrans THEN DO:
          ASSIGN PlanPagos.Id_PdoMes         = 1   /*Transcurre*/
                 /*Creditos.Capital_Acum       = KCAcu - KCap
                 Creditos.Sdo_Proyectado     = Creditos.Monto - Creditos.Capital_Acum
                 Creditos.Int_LiqAcum        = KIAcu - KInt*/
                 PlanPagos.Capital_Acum      = KCAcu - KCap
                 PlanPagos.Int_LiqAcum       = KIAcu - KInt
                 PlanPagos.Int_LiqPdo        = ROUND(((Creditos.Sdo_Proyectado * W_Tasa) / W_DiaPdo)
                                                     * (TODAY - PlanPagos.Fec_Ini),0)
                 /*PlanPagos.Int_LiqPdo        = (KInt / W_DiaPdo) * (TODAY - PlanPagos.Fec_Ini + 1)*/
                 PlanPagos.Capital_Pdo       = 0.
          IF NN EQ Creditos.Cuo_Pagadas AND Creditos.Sdo_Proyectado LE Creditos.Sdo_Capital THEN
             ASSIGN /*Creditos.Cuo_Pagadas  = Creditos.Cuo_Pagadas - 1
                    Creditos.Fec_Pago     = PlanPagos.Fec_Ini*/
                    PlanPagos.Fec_ProxPag = Creditos.Fec_Pago.
          ELSE IF  NN GT Creditos.Cuo_Pagadas AND Creditos.Sdo_Proyectado LE Creditos.Sdo_Capital
               AND (Creditos.Sdo_Capital - Creditos.Sdo_Proyectado) LT Creditos.Cuota THEN
             ASSIGN /*Creditos.Cuo_Pagadas  = Creditos.Cuo_Pagadas + 1
                    Creditos.Fec_Pago     = PlanPagos.Fec_Inic */
                    PlanPagos.Fec_ProxPag = Creditos.Fec_Pago.
       END.
    
       IF NN LE PdoTrans THEN
          ASSIGN PlanPagos.Fec_ProxPag       = Creditos.Fec_Pago
                 PlanPagos.Cuo_Pagas         = Creditos.Cuo_Pagadas
                 PlanPagos.Pagos_CapitalAcum = Creditos.Sdo_CapPag.                   
    END.
    
    FIND LAST PlanPagos WHERE PlanPagos.Agencia         EQ Creditos.Agencia    
                             AND PlanPagos.Nit          EQ Creditos.Nit                                    
                             AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito                            
                             AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito                            
                             AND PlanPagos.Id_PdoMes    EQ 1   /*Transc.*/  NO-ERROR.          
    IF NOT AVAIL(PlanPagos) THEN DO:
       RUN CrearPlan.                        
       ASSIGN PlanPagos.Id_PdoMes     = 1
              PlanPagos.Nro_Cuota     = Creditos.Plazo + 1       
              PlanPagos.Fec_Ini       = TODAY
              PlanPagos.Fec_Vcto      = Creditos.Fec_Pago
              PlanPagos.Fec_ProxPago  = Creditos.Fec_Pago.
              /*Creditos.Sdo_Proyectado = 0.*/
    END.
    
    ASSIGN Creditos.Sdo_IntPag     = Creditos.Int_LiqAcum - (Creditos.Int_Corrientes + Creditos.Int_DifCobro)
           PlanPagos.Pagos_IntAcum = Creditos.Sdo_IntPag.
    
    IF PlanPagos.Nro_Cuota EQ Creditos.Cuo_Pagadas AND Creditos.Sdo_Capital GT Creditos.Sdo_Proyectado THEN
       ASSIGN /*Creditos.Cuo_Pagadas = Creditos.Cuo_Pagadas - 1*/
              PlanPagos.Cuo_Pagas  = Creditos.Cuo_Pagadas.
    
    IF PlanPagos.Nro_Cuota GT Creditos.Plazo THEN
       ASSIGN /*Creditos.Sdo_Proyectado = 0
              Creditos.Capital_Acum   = Creditos.Monto
              Creditos.Cuo_Atraso     = Creditos.Plazo - Creditos.Cuo_Pagadas*/
              PlanPagos.Capital_Acum  = Creditos.Monto.
    ELSE IF PlanPagos.Nro_Cuota - 1 LE Creditos.Cuo_Pagadas THEN
        Creditos.Cuo_Atraso = Creditos.Cuo_Atraso.
       /*Creditos.Cuo_Atraso = 0.*/
    ELSE IF PlanPagos.Nro_Cuota - 1 GT Creditos.Cuo_Pagadas THEN
       /*Creditos.Cuo_Atraso = (PlanPagos.Nro_Cuota - 1) - Creditos.Cuo_Pagadas.*/
    
/*     ASSIGN Creditos.Val_Atraso = Creditos.Sdo_Capital - Creditos.Sdo_Proyectado WHEN */
/*                                  Creditos.Sdo_Capital GT Creditos.Sdo_Proyectado.    */
/*                                                                                      */
/*     ASSIGN Creditos.Dias_Atraso = TODAY - Creditos.Fec_Pago WHEN                     */
/*                                   TODAY GT Creditos.Fec_Pago.                        */
END.

  /*------------------------*/
PROCEDURE CrearPlan:
     CREATE PlanPagos.
     ASSIGN PlanPagos.Agencia      = Creditos.Agencia
            PlanPAgos.Nit          = Creditos.Nit
            PlanPagos.Num_Credito  = Creditos.Num_credito
            PlanPagos.Pagare       = Creditos.Pagare
            PlanPagos.Cod_Credito  = Creditos.Cod_Credito
            PlanPagos.Tip_Credito  = Creditos.Tip_Credito
            PlanPagos.Cuota        = Creditos.Cuota
            PlanPagos.Tasa         = Creditos.Tasa
            PlanPagos.Plazo        = Creditos.Plazo
            PlanPagos.Monto_Actual = Creditos.Monto
            PlanPagos.Id_PdoMes    = 0.                 /*Inicia en futuro*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Rec_XLibranza  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Rec_XLibranza)
  THEN DELETE WIDGET W-Rec_XLibranza.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DistRecaudo W-Rec_XLibranza 
PROCEDURE DistRecaudo :
DEFINE VAR J AS INTEGER.
DEFINE VARIABLE vCuota AS DECIMAL INITIAL 0.

EMPTY TEMP-TABLE TPdctos.

W_PorD = 0.
W_PorD:SCREEN-VALUE IN FRAME F_Proc = "0.00".
W_Dist = 0.
W_Dist:SCREEN-VALUE = "0.00".

W_Cont:SCREEN-VALUE = STRING(W_Cont).

IF NOT W_Inf THEN
    FIND LAST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa = W_CodEmp
                           AND Rec_Nomina.Nro_Pago = W_NroPago
                           AND Rec_Nomina.Fec_Contab = ?
                           AND Rec_Nomina.Nit = W_NitCte
                           AND Rec_Nomina.Num_Cuotas <> 99
                           AND rec_nomina.carnet = "1" NO-LOCK NO-ERROR.

IF NOT AVAILABLE(Rec_Nomina) THEN
    RETURN.

IF Rec_Nomina.Val_Deduc <= 0 THEN
    RETURN.

w_fecha1 = DATE(INTEGER(w_sem),1,viano).
w_fecha2 = DATE(INTEGER(w_sem),1,viano) - 1.

IF INTEGER(w_sem) = 1 OR INTEGER(w_sem) = 3 OR INTEGER(w_sem) = 5  OR INTEGER(w_sem) = 7 OR INTEGER(w_sem) = 8 OR INTEGER(w_sem) = 10 OR INTEGER(w_sem) = 12 THEN
    ASSIGN w_fecha3 = w_fecha1 + 31.
ELSE
    IF INTEGER(w_sem) = 2 THEN
        w_fecha3 = w_fecha1 + DAY(DATE(3,1,viano) - 1).
    ELSE
        w_fecha3 = w_fecha1 + 30.
        
DEFINE VARIABLE cuoper AS DECIMAL INITIAL 0.

FOR EACH Ahorros WHERE Ahorros.Nit = W_NitCte
                   AND (Ahorros.FOR_Pago = 2 OR Ahorros.FOR_Pago = 4)
                   AND Ahorros.Estado = 1
                   AND (ahorros.cod_ahorro = 2 OR ahorros.cod_ahorro = 3) NO-LOCK BREAK BY Ahorros.Cod_Ahorro:
    IF FIRST-OF(ahorros.cod_ahorro) THEN DO:
        IF cod_ahorro = 2 THEN
            vCuota = round(Rec_Nomina.Val_Deduc * 0.2,0).
        ELSE
            vCuota = round(Rec_Nomina.Val_Deduc * 0.8,0).

        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
                                 AND Pro_Ahorros.Estado = 1 NO-LOCK NO-ERROR.

        CREATE TPdctos.
        ASSIGN TPdctos.Prior = Pro_Ahorros.Prioridad
               TPdctos.CodP = Ahorros.Cod_Ahorro
               TPdctos.TP = "A"
               TPdctos.Agen = Ahorros.Agencia
               TPdctos.CtaP = Ahorros.Cue_Ahorro
               TPdctos.FApert = Ahorros.Fec_Apertura
               TPdctos.Cuota = vCuota
               TPdctos.CuoOrig = vCuota
               TPdctos.VrSdo = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
               TPdctos.VrDist = 0
               TPdctos.NC = 0
               TPdctos.NomP = Pro_Ahorros.Nom_Produc.
    END.
END.

W_VrADist = Rec_Nomina.Val_Deduc.

DO J = 1 TO Rec_Nomina.Num_Cuotas:
    FOR EACH TPdctos BY TPdctos.Prior
                     BY TPdctos.FApert:
        IF W_VrADist >= TPdctos.Cuota THEN DO:
            TPdctos.VrDist = TPdctos.VrDist + TPdctos.Cuota.
            TPdctos.NC = TPdctos.NC + 1.
            W_VrADist = W_VrADist - TPdctos.Cuota.
        END.
        ELSE DO:
            TPdctos.VrDist = TPdctos.VrDist + W_VrADist.
            TPdctos.NC = TPdctos.NC + 1.
            W_VrADist = 0.
        END.

        IF W_VrADist <= 0 THEN
            LEAVE.
    END.

    IF W_VrADist <= 0 THEN
        LEAVE.
END.

IF W_VrADist > 0 THEN DO:
    IF NOT W_SiContab THEN
        RUN Verifica_Remanente NO-ERROR.

    FOR EACH Pro_Ahorros WHERE pro_ahorros.cod_ahorro = 8
                           AND Pro_Ahorros.Estado = 1 NO-LOCK BY pro_ahorros.tip_ahorro:
        FIND FIRST Ahorros WHERE Ahorros.Nit = W_NitCte
                             AND Ahorros.Cod_Ahorro = 8
                             AND Ahorros.Estado = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Ahorros) THEN
            FIND FIRST Ahorros WHERE Ahorros.Nit = W_NitCte
                                 AND Ahorros.Cod_Ahorro = 8
                                 AND Ahorros.Estado = 1 NO-LOCK NO-ERROR.

        IF AVAILABLE(Ahorros) THEN DO:
            FIND FIRST TPdctos WHERE TPdctos.TP = "A"
                                 AND TPdctos.CodP = Ahorros.Cod_Ahorro
                                 AND TPdctos.CtaP = Ahorros.Cue_Ahorro NO-ERROR.
            IF NOT AVAILABLE(TPdctos) THEN
                CREATE TPdctos.

            TPdctos.Prior = Pro_Ahorros.Prioridad.
            TPdctos.CodP = Ahorros.Cod_Ahorro.
            TPdctos.CtaP = Ahorros.Cue_Ahorro.
            TPdctos.TP = "A".
            TPdctos.Agen = Ahorros.Agencia.
            TPdctos.FApert = Ahorros.Fec_Apertura.
            TPdctos.Cuota = Ahorros.Cuota.
            TPdctos.CuoOrig = Ahorros.Cuota.
            TPdctos.VrSdo = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje.
            TPdctos.VrDist = TPdctos.VrDist + W_VrADist.
            TPdctos.NomP = Pro_Ahorros.Nom_Produc.
            W_VrADist = 0.

            LEAVE.
        END.
        ELSE DO:
            FIND FIRST Tmp_SinRema WHERE Tmp_SinRema.cedula = W_NitCte NO-ERROR.
            IF NOT AVAILABLE(Tmp_SinRema) THEN DO:
                CREATE Tmp_SinRema.
                
                Tmp_SinRema.agencia = rec_nomina.agencia.
                Tmp_SinRema.cedula = W_NitCte.
                Tmp_SinRema.codempre = rec_nomina.cod_empresa.
                Tmp_SinRema.valdist = W_VrADist.
            END.
        END.
    END.
END.

IF NOT W_Inf THEN DO:
    IF W_VrADist > 0 THEN
        MESSAGE "El Cliente/Asociado no tiene productos de Ahorro para Abonar el sobrante de la Distribuci�n..." SKIP
                "Revise por favor..."
            VIEW-AS ALERT-BOX.

    ASSIGN W_PorD = Rec_Nomina.Val_Deduc
           W_PorD:SCREEN-VALUE = STRING(W_PorD)
           W_Dist:SCREEN-VALUE = STRING(W_Dist).

    OPEN QUERY Br_Pdctos FOR EACH TPdctos NO-LOCK INDEXED-REPOSITION.
END.
ELSE DO:
    IF W_SiContab AND W_VrADist > 0 THEN
        W_ContabDistNo = W_ContabDistNo + W_VrADist.

    FIND FIRST Clientes WHERE Clientes.Nit = W_NitCte
                          AND Clientes.Estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN DO:
        FOR EACH TPdctos:
            CREATE CopTPdctos.
            BUFFER-COPY TPdctos TO CopTPdctos.

            CopTPdctos.NitP = W_NitCte.
            CopTPdctos.NomC = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre).
            CopTPdctos.VRec = Rec_Nomina.Val_Deduc.

            IF W_SiContab AND TPdctos.VrDist > 0 THEN DO:
                W_ContabDistSi = W_ContabDistSi + TPdctos.VrDist.

                RUN Abonar NO-ERROR. /* oakley */
                IF ERROR-STATUS:ERROR THEN DO:
                    MESSAGE "Error en Abonar"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    RETURN ERROR.
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EliPagosNoConta W-Rec_XLibranza 
PROCEDURE EliPagosNoConta :
/*------------------------------------------------------------------------------
  Purpose: Eliminar las n�mina que fueron cargadas y No contabilizadas de la empresa respectiva      
------------------------------------------------------------------------------*/
FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empr            EQ W_CodEmp  AND
                          Rec_Nomina.Nro_Pago            EQ W_NroPago AND
                          Rec_Nomina.Fec_Contabilizacion EQ ? :
    DELETE rec_nomina.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Rec_XLibranza  _DEFAULT-ENABLE
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
  DISPLAY W_CmbEmp W_CmbOfi Cmb_pagos Tg_Nuevo Rs_Opcrea W_NitCte W_VrRec 
          W_NCuotas W_NroPago W_NomCte txt_contrapartida W_sem W_TotRec W_TotNoD 
          Tg_SemP W_Cont Msaje W_Dist W_PorD w_anoAplicar 
      WITH FRAME F_Proc IN WINDOW W-Rec_XLibranza.
  ENABLE Btn_BorraPagos W_CmbEmp Cmb_pagos Tg_Nuevo W_NitCte W_VrRec W_NCuotas 
         BUTTON-5 Br_Rec txt_contrapartida W_sem Btn_Imp Btn_Preliminar 
         Btn_Contabilizar Tg_SemP Btn_Done Br_Pdctos Btn_CreCancelado 
         btn-Cuotas w_anoAplicar RECT-314 RECT-316 
      WITH FRAME F_Proc IN WINDOW W-Rec_XLibranza.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  DISPLAY w-fecIni w-fecfin 
      WITH FRAME frm-FPagaduria IN WINDOW W-Rec_XLibranza.
  ENABLE Btn_SalirFec w-fecIni w-fecfin 
      WITH FRAME frm-FPagaduria IN WINDOW W-Rec_XLibranza.
  {&OPEN-BROWSERS-IN-QUERY-frm-FPagaduria}
  DISPLAY Rs_OpInf W_CmbEI Cmb_PagI W_FecIniI W_FecFinI 
      WITH FRAME F_Imp IN WINDOW W-Rec_XLibranza.
  ENABLE RECT-317 Rs_OpInf W_CmbEI Cmb_PagI W_FecIniI W_FecFinI Btn_FinImp 
         Btn_Imp-2 
      WITH FRAME F_Imp IN WINDOW W-Rec_XLibranza.
  {&OPEN-BROWSERS-IN-QUERY-F_Imp}
  VIEW W-Rec_XLibranza.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Exasociados W-Rec_XLibranza 
PROCEDURE Exasociados :
/*------------------------------------------------------------------------------
  Purpose: Alimenta la tabla de Exasociados Para su Posterior Contabilizacion     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH tmp-exasoc.  DELETE tmp-exasoc. END.

FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp AND
                          Rec_Nomina.Nro_Pago    EQ W_NroPago AND
                          Rec_Nomina.Num_Cuotas  EQ 99 NO-LOCK.
    FIND FIRST Clientes WHERE
         Clientes.Nit         EQ Rec_Nomina.nit         AND
         Clientes.Cod_Empresa EQ Rec_Nomina.Cod_Empresa NO-LOCK NO-ERROR.
    IF AVAILABLE (clientes) AND clientes.estado NE 1 THEN DO:
       FIND tmp-exasoc WHERE tmp-exasoc.nit EQ Rec_nomina.Nit NO-ERROR.
       IF NOT AVAILABLE(tmp-exasoc) THEN DO:
          CREATE tmp-exasoc.
          ASSIGN tmp-exasoc.cod_age  = clientes.agencia
                 tmp-exasoc.nit      = clientes.nit
                 tmp-exasoc.nombre   = TRIM(clientes.nombre) + " " + TRIM(clientes.apellido1) + " " + TRIM(clientes.apellido2)
                 tmp-exasoc.codempre = clientes.cod_empresa
                 tmp-exasoc.estado   = clientes.estado
                 tmp-exasoc.fecing   = clientes.fec_ingreso
                 tmp-exasoc.fecret   = clientes.fec_retiro               
                 tmp-exasoc.fecont   = Rec_Nomina.Fec_Pago            WHEN Fec_Contabilizacion EQ ?
                 tmp-exasoc.fecont   = Rec_Nomina.Fec_Contabilizacion WHEN Fec_Contabilizacion NE ?
                 tmp-exasoc.valrec   = Rec_nomina.Val_Deducido.
         END.
       ELSE
          ASSIGN tmp-exasoc.valrec   = tmp-exasoc.valrec + Rec_nomina.Val_Deducido.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaArchivo W-Rec_XLibranza 
PROCEDURE HallaArchivo :
DEFINE VAR Archivo AS CHARACTER FORMAT "X(40)".
DEFINE VAR Datos AS CHARACTER FORMAT "X(80)".

SESSION:SET-WAIT-STATE("GENERAL").

Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Nuevo-Recaudo desde Archivo...".
Msaje:VISIBLE = TRUE.
W_Cont:SCREEN-VALUE = "0".
W_Cont = 0.
TImp = 0.
TNoImp = 0.
NNoImp = 0.

SYSTEM-DIALOG GET-FILE Archivo
    TITLE "Selecc.el Archivo...Dise�o : C�dula-X(12);Valor-9999999999.99"
    FILTERS "Archivos Txt (*.Txt)"   "*.*"
    MUST-EXIST
    USE-FILENAME.

INPUT FROM VALUE(Archivo).
REPEAT:
    CREATE tmp.
    IMPORT DELIMITER ";" tmp.

    FIND FIRST Clientes WHERE Clientes.Nit = Tmp.Ced
                          AND Clientes.Estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN DO:
        IF Tmp.Vr > 0 THEN DO:
            Tmp.Age = clientes.agencia.
            Tmp.Nom = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
            
            FIND FIRST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa = W_CodEmp
                                    AND Rec_Nomina.Nro_Pago = W_NroPago
                                    AND Rec_Nomina.Nit = Tmp.Ced
                                    AND Rec_Nomina.Fec_Contab = ? NO-ERROR.
            IF NOT AVAILABLE(Rec_Nomina) THEN
                CREATE Rec_Nomina.

            Rec_Nomina.Agencia = W_Agencia.
            Rec_Nomina.Cod_Empresa = W_CodEmp.
            Rec_Nomina.Nro_Pago = W_NroPago.
            Rec_Nomina.Nit = Tmp.Ced.
            Rec_Nomina.Num_Cuotas = integer(W_NCuotas:SCREEN-VALUE in FRAME F_Proc).
            Rec_Nomina.Fec_Contab = ?.
            Rec_Nomina.Fec_Grabac = W_Fecha.
            Rec_Nomina.Fec_Pago = W_Fecha.
            Rec_Nomina.Val_Deducido = Rec_Nomina.Val_Deducido + Tmp.Vr.
            Rec_Nomina.carnet = "1".
            W_Cont = W_Cont + 1.
            W_Cont:SCREEN-VALUE = STRING(W_Cont).
            TImp = TImp + Tmp.Vr.
        END.
    END.
    ELSE DO:
        MESSAGE "La C�dula:" Tmp.Ced "de:" Tmp.Nom SKIP
                "por valor de $" Tmp.Vr "...No aparece activa en Clientes..." SKIP
                "Registro Ignorado."
            VIEW-AS ALERT-BOX.

        NNoImp = NNoImp + 1.
        TNoImp = TNoImp + Tmp.Vr.
        Tmp.Err = "Cliente No Existe-inactivo o de otra empresa..".

        FIND FIRST Clientes WHERE Clientes.Nit = Tmp.Ced NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN DO:
            FIND FIRST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa = W_CodEmp
                                    AND Rec_Nomina.Nro_Pago = W_NroPago
                                    AND Rec_Nomina.Nit = Tmp.Ced
                                    AND Rec_Nomina.Fec_Contab = ? NO-ERROR.
            IF NOT AVAILABLE(Rec_Nomina) THEN
                CREATE Rec_Nomina.

            Rec_Nomina.Agencia = W_Agencia.
            Rec_Nomina.Cod_Empresa = W_CodEmp.
            Rec_Nomina.Nro_Pago = W_NroPago.
            Rec_Nomina.Nit = Tmp.Ced.
            Rec_Nomina.Num_Cuotas = 99.
            Rec_Nomina.Fec_Contab = ?.
            Rec_Nomina.Fec_Grabac = W_Fecha.
            Rec_Nomina.Fec_Pago = W_Fecha.
            Rec_Nomina.Val_Deducido = Rec_Nomina.Val_Deducido + Tmp.Vr.
            rec_nomina.carnet = "1".
        END.
        ELSE DO:
            FIND FIRST Rec_Tmp WHERE Rec_Tmp.Cod_Empresa = W_CodEmp
                                 AND Rec_Tmp.Nro_Pago = W_NroPago
                                 AND Rec_Tmp.Nit = Tmp.Ced NO-ERROR.
            IF NOT AVAILABLE(Rec_Tmp) THEN
                CREATE Rec_Tmp.

            Rec_Tmp.Agencia = "000".
            Rec_Tmp.Cod_Empresa = W_CodEmp.
            Rec_Tmp.Empresa = "0-No Existe".
            Rec_Tmp.Nro_Pago = W_NroPago.
            Rec_Tmp.Nit = Tmp.Ced.
            Rec_Tmp.Nombre = "No Existe en Clientes".
            Rec_Tmp.Esta = "0-No Existe".
            Rec_Tmp.fecing = ?.
            Rec_Tmp.fecret = ?.
            Rec_Tmp.fecpag = W_Fecha.
            Rec_Tmp.Vr = Rec_Tmp.Vr + Tmp.Vr.
            Rec_Tmp.Err = "Cliente No Existe..".
        END.
    END.
END.
INPUT CLOSE.

MESSAGE "Se Importaron:" W_cont "registros, por valor de $" TImp SKIP
        "Se Omitieron :" NNoImp "registros, por valor de $" TNoImp
    VIEW-AS ALERT-BOX.

RUN QueryRec.
RUN ImpExaso.

DEFINE VAR Listado AS CHARACTER FORMAT "X(40)".

W_SiImp = TRUE.
listado = W_PathSpl + "ImpRec-" + STRING(W_CodEmp) + "-" + STRING(W_NroPago) + ".Lst".

{Incluido\Imprimir.I "listado"}

W_SiImp = FALSE.

EMPTY TEMP-TABLE Tmp.

SESSION:SET-WAIT-STATE("").

Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generado Nuevo-Recaudo desde Archivo...".
Msaje:VISIBLE = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hallaas400l W-Rec_XLibranza 
PROCEDURE hallaas400l :
/* halla el proceso de leche. */
/*SESSION:SET-WAIT-STATE("GENERAL").


 ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Novedad desde AS400"
        Msaje:VISIBLE                      = TRUE
        W_Cont:SCREEN-VALUE                = "0"
        W_Cont                             = 0
        TImp                               = 0
        TNoImp                             = 0
        NNoImp                             = 0.
FIND FIRST ldescleche NO-LOCK NO-ERROR.
MESSAGE "Semana a Procesar " ldescleche.lulsen VIEW-AS ALERT-BOX.

FOR EACH ldescleche WHERE ldicob = "1":
     CREATE Tmp NO-ERROR.
       ASSIGN tmp.ced = ldescleche.ltecod
              tmp.vr  = ldescleche.ldcuot.
       IF ERROR-STATUS:ERROR THEN DO:
          MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
          LEAVE.
       END.
   FIND FIRST Clientes WHERE Clientes.Nit         EQ Tmp.Ced
                        /* AND Clientes.Cod_Empresa EQ W_CodEmp*/
                         AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.                            
   IF AVAIL(Clientes) AND Tmp.Vr GT 0 THEN DO:                                                                  
      FIND FIRST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp                             
                              AND Rec_Nomina.Nro_Pago    EQ W_NroPago                            
                              AND Rec_Nomina.Nit         EQ Tmp.Ced                              
                              AND Rec_Nomina.Fec_Contab  EQ ? NO-ERROR.                          
      IF NOT AVAIL(Rec_Nomina) THEN                                                              
         CREATE Rec_Nomina.                                                                      

       ASSIGN Rec_Nomina.Agencia      = W_Agencia                                                
              Rec_Nomina.Cod_Empresa  = W_CodEmp                                                 
              Rec_Nomina.Nro_Pago     = W_NroPago                                                
              Rec_Nomina.Nit          = Tmp.Ced                                                  
              Rec_Nomina.Num_Cuotas   = 1                                                        
              Rec_Nomina.Fec_Contab   = ?                                                        
              Rec_Nomina.Fec_Grabac   = W_Fecha                                                  
              Rec_Nomina.Fec_Pago     = W_Fecha                                                  
              Rec_Nomina.Val_Deducido = Rec_Nomina.Val_Deducido + Tmp.Vr
              W_Cont                  = W_Cont + 1
              W_Cont:SCREEN-VALUE     = STRING(W_Cont)
              TImp                    = TImp + Tmp.Vr.

   END.                                                                                          
   ELSE DO:
       MESSAGE "nit " tmp.ced skip
               "valor " tmp.vr  VIEW-AS ALERT-BOX.
       ASSIGN NNoImp  = NNoImp + 1
              TNoImp  = TNoImp + Tmp.Vr
              Tmp.Err = "Cliente no existe Activo".
   END.
 END.

 RUN QueryRec.

 MESSAGE "Se Importaron : " W_cont " Registros, por valor de $ " TImp SKIP
         "Se Omitieron  : " NNoImp " Registros, por valor de $ " TNoImp
         VIEW-AS ALERT-BOX.

 DEFI VAR Listado AS CHAR FORM "X(40)".

 ASSIGN W_SiImp = TRUE
        listado = W_PathSpl + "ImpRec.Lst".

 {Incluido\Imprimir.I "listado"}

 W_SiImp = FALSE.

 FOR EACH Tmp: DELETE Tmp. END.

 SESSION:SET-WAIT-STATE("").

 ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generado Novedades desde AS400"
        Msaje:VISIBLE                      = FALSE.*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hallaas400n W-Rec_XLibranza 
PROCEDURE hallaas400n :
/* halla el proceso de nomina. */
/*SESSION:SET-WAIT-STATE("GENERAL").

/*CONNECT C:\SPS\DATOS\interfase\interfase -1.
CONNECT ayc400 -ld "ayc400" -dt ODBC -U "sps04" -P "adm321" .*/

 ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Novedad desde AS400"
        Msaje:VISIBLE                      = TRUE
        W_Cont:SCREEN-VALUE                = "0"
        W_Cont                             = 0
        TImp                               = 0
        TNoImp                             = 0
        NNoImp                             = 0.

FIND FIRST fdescnomin NO-ERROR.
MESSAGE "Periodo a Abonar " fdescnomin.semapa VIEW-AS ALERT-BOX.

FOR EACH fdescnomin :
     CREATE Tmp NO-ERROR.
       ASSIGN tmp.ced = fdescnomin.tercero
              tmp.vr  = fdescnomin.valcuo.
       IF ERROR-STATUS:ERROR THEN DO:
          MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
          LEAVE.
       END.
   FIND FIRST Clientes WHERE Clientes.Nit         EQ Tmp.Ced
                         /*AND Clientes.Cod_Empresa EQ W_CodEmp*/
                         AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.                            
   IF NOT AVAIL(clientes) THEN DO:
       FIND FIRST Clientes WHERE decimal(Clientes.cod_anterior) EQ decimal(fdescnomin.ceduln)
                          AND Clientes.Cod_Empresa EQ W_CodEmp
                          AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.                            
       IF AVAILABLE(clientes) THEN ASSIGN tmp.ced = clientes.nit.
   END.
   IF AVAIL(Clientes) AND Tmp.Vr GT 0 THEN DO:                                                                  
      FIND FIRST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp                             
                              AND Rec_Nomina.Nro_Pago    EQ W_NroPago                            
                              AND Rec_Nomina.Nit         EQ Tmp.Ced                              
                              AND Rec_Nomina.Fec_Contab  EQ ? NO-ERROR.                          
      IF NOT AVAIL(Rec_Nomina) THEN                                                              
         CREATE Rec_Nomina.                                                                      

       ASSIGN Rec_Nomina.Agencia      = W_Agencia                                                
              Rec_Nomina.Cod_Empresa  = W_CodEmp                                                 
              Rec_Nomina.Nro_Pago     = W_NroPago                                                
              Rec_Nomina.Nit          = Tmp.Ced                                                  
              Rec_Nomina.Num_Cuotas   = 1                                                        
              Rec_Nomina.Fec_Contab   = ?                                                        
              Rec_Nomina.Fec_Grabac   = W_Fecha                                                  
              Rec_Nomina.Fec_Pago     = W_Fecha                                                  
              Rec_Nomina.Val_Deducido = Rec_Nomina.Val_Deducido + Tmp.Vr
              W_Cont                  = W_Cont + 1
              W_Cont:SCREEN-VALUE     = STRING(W_Cont)
              TImp                    = TImp + Tmp.Vr.

   END.                                                                                          
   ELSE DO:
       ASSIGN NNoImp  = NNoImp + 1
              TNoImp  = TNoImp + Tmp.Vr
              Tmp.Err = "Cliente no existe Activo".
   END.
 END.

 RUN QueryRec.

 MESSAGE "Se Importaron : " W_cont " Registros, por valor de $ " TImp SKIP
         "Se Omitieron  : " NNoImp " Registros, por valor de $ " TNoImp
         VIEW-AS ALERT-BOX.

 DEFI VAR Listado AS CHAR FORM "X(40)".

 ASSIGN W_SiImp = TRUE
        listado = W_PathSpl + "ImpRec.Lst".

 {Incluido\Imprimir.I "listado"}

 W_SiImp = FALSE.

 FOR EACH Tmp: DELETE Tmp. END.

 SESSION:SET-WAIT-STATE("").

 ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generado Novedades desde AS400"
        Msaje:VISIBLE                      = FALSE.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaPdctos W-Rec_XLibranza 
PROCEDURE HallaPdctos :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR T_VrRec LIKE Rec_Nomina.Val_Deducido INIT 0.
  DEFINE VAR v_forma_pago like empresas.for_pago.
                                            
  SESSION:SET-WAIT-STATE("GENERAL").
    
  ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Nuevo-Recaudo desde Pdctos de C/Cliente..."
         Msaje:VISIBLE                      = TRUE.
    
   /*27/01/2005 Edisson Alexander Moncada  Hallamos periocidad de la empresa*/
  v_forma_pago = 4.
  FIND FIRST empresas WHERE empresas.cod_empresa = W_CodEmp  NO-LOCK NO-ERROR.
  IF AVAIL empresas THEN DO:
     v_forma_pago = Empresas.For_Pago.
  END.  
    
  FOR EACH Clientes WHERE Clientes.Cod_Empresa EQ W_CodEmp
                      AND Clientes.Estado      EQ 1 NO-LOCK BY Clientes.Nit:
      
      FOR EACH Ahorros WHERE Ahorros.Nit       EQ Clientes.Nit
                         AND Ahorros.Cuota     GE 0
                         AND Ahorros.FOR_Pago  EQ 2
                         AND Ahorros.Estado    EQ 1 NO-LOCK BY Ahorros.Cod_Ahorro:
            IF cod_ahorro NE 2 THEN NEXT.
           /*buscamos la pereiocidad del pago de los creditos, y si los creditos tienen
            periocidad diferente a la empresa respetamos la periocidad de los creditos*/
            ASSIGN T_VrRec = T_VrRec + round(Ahorros.Cuota,0).
            /*IF ahorros.Per_Deduccion EQ 4 AND v_forma_pago EQ 3 THEN
                ASSIGN T_VrRec = T_VrRec + round(Ahorros.Cuota / 2,0).              
            ELSE                              
                ASSIGN T_VrRec = T_VrRec + round(Ahorros.Cuota / 2,0).              */
      END.
/*
      FOR EACH Creditos WHERE Creditos.Nit         EQ Clientes.Nit
                          AND Creditos.Cuota       GT 0
                          AND Creditos.FOR_Pago    EQ 2
                          AND Creditos.Sdo_capital GT 0 NO-LOCK BY Creditos.Cod_Credito:
              /* Alexander Moncada adiccion 27/01/2005*/
                IF creditos.Per_pago EQ 4 AND v_forma_pago EQ 3 THEN
                   ASSIGN T_VrRec = T_VrRec + round(Creditos.Cuota / 2,0).              
                else
                   ASSIGN T_VrRec = T_VrRec + Creditos.Cuota.                     
      END.
 */
      IF T_VrRec GT 0 THEN DO:
         CREATE Rec_Nomina.                           
         ASSIGN Rec_Nomina.Agencia      = W_Agencia       
                Rec_Nomina.Cod_Empresa  = W_CodEmp           
                Rec_Nomina.Nro_Pago     = W_NroPago     
                Rec_Nomina.Nit          = Clientes.Nit      
                Rec_Nomina.Num_Cuotas   = 1            
                Rec_Nomina.Fec_Contab   = ?             
                Rec_Nomina.Fec_Grabac   = W_Fecha       
                Rec_Nomina.Fec_Pago     = W_Fecha       
                Rec_Nomina.Val_Deducido = T_VrRec      
                T_VrRec                 = 0
                W_Cont                  = W_Cont + 1
                W_Cont:SCREEN-VALUE     = STRING(W_Cont).  
      END.
  END.

  RUN QueryRec.

  SESSION:SET-WAIT-STATE("").     

  MESSAGE "El Nuevo Recaudo fue generado desde productos de C/Cliente...Puede continuar."
          VIEW-AS ALERT-BOX.
        
  ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "El Nuevo-Recaudo Ya fue generado..."
         Msaje:VISIBLE                      = FALSE
         W_Cont                             = 0
         W_Cont:SCREEN-VALUE                = STRING(W_Cont).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpCpte W-Rec_XLibranza 
PROCEDURE ImpCpte :
DEFI VAR TotD   LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TotC   LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TTotD  LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TTotC  LIKE Mov_Contable.Db INIT 0.

  {Incluido\RepEncabezado.I}

  ASSIGN W_Reporte = "Cpte Resumen : Distribuci�n Contabilizada de Recaudo      Fecha del Informe: " +
                      STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
         W_EncColumna = "Comprobante: " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                        STRING(Comprobantes.Secuencia,"99999999") + "      Empresa: " + 
                        STRING(W_CodEmp) + " - Nro.Pago: " + STRING(W_NroPago).

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.

  FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                          AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                          AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK
                              BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta:
      ASSIGN TotD  = TotD  + Mov_Contable.Db
             TTotD = TTotD + Mov_Contable.Db
             TotC  = TotC  + Mov_Contable.Cr
             TTotC = TTotC + Mov_Contable.Cr.

      IF LAST-OF(Mov_Contable.Cuenta) THEN DO:
         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                              AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
         DISPLAY Mov_Contable.Agencia   LABEL "Ag."
                 Mov_Contable.Cuenta    LABEL "Cta-Contable"
                 Cuentas.Nombre         LABEL "Descripci�n de la Cuenta" WHEN AVAIL(Cuentas)
                 TotD                   LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"
                 TotC                   LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

         ASSIGN TotD  = 0
                TotC  = 0.
      END.

  END.

  DISPLAY SKIP(1)
          "                     TOTAL FINAL------------>               ------------------ ------------------"
          SKIP
          "                                                           "
          TTotD      FORM "->>>>>>,>>>,>>9.99"
          TTotC      FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpExaso W-Rec_XLibranza 
PROCEDURE ImpExaso :
DEFINE VARIABLE WTotRec AS DECIMAL.
DEFINE VARIABLE vNombreEmpresa AS CHARACTER.

RUN _SetCurs.r ("WAIT").

DEFINE VARIABLE Listado1 AS CHARACTER FORMAT "X(40)".

listado1 = W_PathSpl + "ImpExaso-" + STRING(W_CodEmp) + "-" + STRING(W_NroPago) + ".Lst".

OS-DELETE VALUE(Listado1).
OUTPUT TO value(Listado1).

{Incluido\RepEncabezado.I}

W_Reporte = "Exasociado: Informe de Exasociados de Recaudo      Fecha del Informe: " + STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS").
W_EncColumna = "Empresa     : " + STRING(W_CodEmp) + " - Nro.Pago: " + STRING(W_NroPago).

VIEW FRAME F-Encabezado.

FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa = W_CodEmp
                      AND Rec_Nomina.Nro_Pago = W_NroPago
                      AND Rec_Nomina.Num_Cuotas = 99 NO-LOCK BY Rec_Nomina.Nit:
    WTotRec = WTotRec + Rec_Nomina.Val_Deducido.
    W_estado = "0-No Existe".
    W_nomcli = "No Existe".

    FIND FIRST Clientes WHERE clientes.nit = Rec_Nomina.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN DO:
        W_nomcli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

        CASE Clientes.Estado:
            WHEN 1 THEN W_estado = "1-Activo".
            WHEN 2 THEN W_estado = "2-Inactivo".
            OTHERWISE W_estado = "No Asignado".
        END CASE.

        FIND FIRST Empresas WHERE Empresas.Cod_Empresa = Clientes.Cod_Empresa NO-LOCK NO-ERROR.
        IF AVAILABLE(Empresas) THEN
            vNombreEmpresa = STRING(empresa.cod_empresa) + "-" + Empresas.Alias_Empresa.
        ELSE
            vNombreEmpresa = STRING(clientes.cod_empresa) + "-No Existe".

        DISPLAY Rec_Nomina.Nit          LABEL "C�dula/Nit"
                W_nomcli                LABEL "Nombres" FORMAT "X(40)"
                Clientes.Agencia        LABEL "Agencia"
                vNombreEmpresa          LABEL "Empresa" FORMAT "X(20)"
                W_estado                LABEL "Estado"
                Clientes.Fec_Ingreso    LABEL "Fec_Ingreso"
                Clientes.Fec_Retiro     LABEL "Fec_Retiro"
                Rec_Nomina.Fec_Pago     LABEL "Fec_Pago"
                Rec_Nomina.Val_Deducido LABEL "TOTAL RECAUDO" FORMAT "->>>,>>>,>>>,>>9.99"
            WITH DOWN WIDTH 180 FRAME FImpExaso USE-TEXT NO-LABELS STREAM-IO NO-BOX.

        W_exasoc = W_exasoc + 1.
    END.
END.

FOR EACH Rec_Tmp BY Rec_Tmp.Cod_Empresa
                 BY Rec_Tmp.Nit:
    WTotRec = WTotRec + Rec_Tmp.Vr.

    DISPLAY Rec_Tmp.Nit         LABEL "C�dula/Nit"
            Rec_Tmp.Nombre      LABEL "Nombres" FORMAT "X(40)"
            Rec_Tmp.Agencia     LABEL "Agencia"
            Rec_Tmp.Empresa     LABEL "Empresa" FORMAT "X(20)"
            Rec_Tmp.Estado      LABEL "Estado"
            Rec_Tmp.fecing      LABEL "Fec_Ingreso"
            Rec_Tmp.fecret      LABEL "Fec_Retiro"
            Rec_Tmp.fecpag      LABEL "Fec_Pago"
            Rec_Tmp.Vr          LABEL "TOTAL RECAUDO" FORMAT "->>>,>>>,>>>,>>9.99"
        WITH DOWN WIDTH 180 FRAME FImpExaso1 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    W_exasoc = W_exasoc + 1.
END.

DISPLAY SKIP(1)
        "    TOTAL RECAUDO------------>   "
        SPACE(98)
        WTotRec FORMAT "->>>>>>,>>>,>>9.99"
    WITH DOWN WIDTH 180 FRAME FTotImpExaso USE-TEXT NO-LABELS STREAM-IO NO-BOX.

EMPTY TEMP-TABLE Rec_Tmp.
OUTPUT CLOSE.

MESSAGE "El archivo plano est� en" listado1
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfExasoc W-Rec_XLibranza 
PROCEDURE InfExasoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR Listado AS CHARACTER INITIAL "".                                                            
 RUN _SetCurs.r ("WAIT").
 ASSIGN listado = W_PathSpl + "InfExasociados-" + STRING(W_CodEmp) + "-" + STRING(W_NroPago) + ".csv".
/*  Listado = W_PathSpl + "InfExasociados-" + W_Usuario + ".csv". */
 OS-DELETE VALUE(Listado).
 OUTPUT TO value(Listado). /* NO-ECHO PAGED PAGE-SIZE 65 */
/*   {Incluido\RepEncabezado.I}                                                                             */
/*   ASSIGN W_Reporte    = "Reporte   : Clientes No Asociados de la Importacion     Fecha del Informe: " +  */
/*                         STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")         */
/*          W_EncColumna = "Empresa: " + STRING(W_CodEmp) + " - Nro.Pago: " + STRING(W_NroPago).            */
/*                                                                                                          */
/* /*  VIEW FRAME F-Encabezado.                                                                             */
/*   VIEW FRAME f-ftr.*/                                                                                    */
 PUT "Cod_Empre;Cedula;Nombres;Agencia;Estado;Fec_Ingreso;Fec_Retiro;Fec_Pago/Contab.;Vlr.Recaudo" SKIP(0).
 ASSIGN W_exasoc = 0.
 FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp                             
                       AND Rec_Nomina.Nro_Pago    EQ W_NroPago                            
                       AND Rec_Nomina.Num_Cuotas  EQ 99 NO-LOCK 
                       BY Rec_Nomina.Cod_Empresa BY Rec_Nomina.Nit:
     FIND FIRST Clientes WHERE clientes.nit EQ Rec_Nomina.Nit NO-ERROR.
     IF AVAILABLE(Clientes) THEN DO:
        CASE Clientes.Estado:
            WHEN 1 THEN ASSIGN W_estado = "1- Activo".    
            WHEN 2 THEN ASSIGN W_estado = "2- Inactivo".  
            OTHERWISE                                     
               ASSIGN W_estado = "No Asignado".    
        END CASE.
        PUT Rec_Nomina.Cod_Empresa ";"
            Rec_Nomina.Nit         ";"
            Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2 ";"
            Clientes.Agencia ";"
            W_estado ";"
            Clientes.Fec_Ingreso    FORMAT "99/99/9999" ";"
            Clientes.Fec_Retiro     FORMAT "99/99/9999" ";"
            Rec_Nomina.Fec_Pago     FORMAT "99/99/9999" ";" 
            Rec_Nomina.Val_Deducido FORMAT "->>>,>>>,>>>,>>9.99" SKIP(0).
      END.
      ELSE
         PUT "No Existe el Cliente...." SKIP(0).
 END.

/*  FOR EACH tmp-exasoc BY codempre BY nit :              */
/*      CASE tmp-exasoc.estado:                           */
/*          WHEN 1 THEN ASSIGN W_estado = "1- Activo".    */
/*          WHEN 2 THEN ASSIGN W_estado = "2- Inactivo".  */
/*          OTHERWISE                                     */
/*              ASSIGN W_estado = "No Asignado".          */
/*      END CASE.                                         */
/*      PUT /** Nuevo */                                  */
/*         tmp-exasoc.codempre ";"                        */
/*         tmp-exasoc.nit      ";"                        */
/*         tmp-exasoc.nombre   ";"                        */
/*         tmp-exasoc.cod_age  ";"                        */
/*         W_vinculo           ";"                        */
/*         W_estado            ";"                        */
/*         tmp-exasoc.fecing   ";"                        */
/*         tmp-exasoc.fecont   ";"                        */
/*         tmp-exasoc.valrec   SKIP(0).                   */
/*       ASSIGN W_exasoc = W_exasoc + 1.                  */
/*   END.                                                 */

  PUT "TOTAL...Registros : " + STRING(W_exasoc,"zzzzzz") FORMAT "X(120)" SKIP(0).
      /*WITH DOWN WIDTH 140 FRAME FTtImp USE-TEXT NO-LABELS STREAM-IO NO-BOX.*/

/*   DEFI VAR Listado AS CHAR FORM "X(40)".   */
/*                                            */
/*   listado = W_PathSpl + "Exasociado.Lst".  */
/*                                            */
/*   {Incluido\Imprimir.I "listado"}          */

  MESSAGE "El archivo plano esta en " listado VIEW-AS ALERT-BOX INFO BUTTONS OK.
  OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfImport W-Rec_XLibranza 
PROCEDURE InfImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {Incluido\RepEncabezado.I}
  ASSIGN W_Reporte    = "Reporte   : Importaci�n Recaudo desde Archivo      Fecha del Informe: " +
                        STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
         W_EncColumna = "Empresa: " + STRING(W_CodEmp) + " - Nro.Pago: " + STRING(W_NroPago). 

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.

  FOR EACH Tmp:
      DISPLAY /** Nuevo */
              Tmp.Age    LABEL "Agencia"
              /**************/
              Tmp.Ced    LABEL "C�dula/Nit"
              Tmp.Nom    LABEL "Nombre del Cliente"
              Tmp.Vr     LABEL "Valor Recaudo"
              Tmp.Err    LABEL "Inconsistencia"
         WITH DOWN WIDTH 140 FRAME FImp USE-TEXT NO-LABELS STREAM-IO NO-BOX.
  END.

  DISPLAY SKIP(1)
          "TOTALES : Registros Importados : " + STRING(W_Cont,"999999") + " Por valor de $ " +
           STRING(Timp,"->>>>,>>>,>>9.99")                 FORMAT "X(120)" SKIP
          "-------   Registros Omitidos   : " + STRING(NNoImp,"999999") + " Por valor de $ " + 
           STRING(TNoImp,"->>>>,>>>,>>9.99")               FORMAT "X(120)"
      WITH DOWN WIDTH 140 FRAME FTtImp USE-TEXT NO-LABELS STREAM-IO NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfSinRema W-Rec_XLibranza 
PROCEDURE InfSinRema :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFI VAR WTotInco  LIKE Mov_Contable.Db INIT 0.
RUN _SetCurs.r ("WAIT").
DEFINE VARIABLE Listado AS CHAR FORM "X(40)".
ASSIGN listado = W_PathSpl + "InfAsoSinRema-" + STRING(W_CodEmp) + "-" + STRING(W_NroPago) + ".Lst".
/* ASSIGN Listado = W_PathSpl + "InfAsoSinRema-" + W_Usuario + ".Lst". */
OUTPUT TO VALUE(Listado).

  {Incluido\RepEncabezado.I}

  ASSIGN W_Reporte    = "Remanentes: Informe de Remanentes No Creados      Fecha del Informe: " +
                        STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
         W_EncColumna = "Empresa     : " + STRING(W_CodEmp) + " - Nro.Pago: " + STRING(W_NroPago).

  VIEW FRAME F-Encabezado.
  /*VIEW FRAME f-ftr.*/

  FOR EACH Tmp_SinRema BY cedula:
      ASSIGN WTotInco  = WTotInco + Tmp_SinRema.valdist.
      FIND FIRST Clientes WHERE clientes.nit EQ Tmp_SinRema.cedula NO-ERROR.
      IF AVAILABLE(Clientes) THEN DO:
         DISPLAY Tmp_SinRema.agencia  LABEL "Agencia"
                 Tmp_SinRema.cedula   LABEL "Cedula/Nit"
                 Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2 LABEL "Nombres" FORMAT "X(40)"
                 Tmp_SinRema.valdist  LABEL "TOTAL DISTRIBUIDO" FORMAT "->>>,>>>,>>>,>>9.99"
         WITH DOWN WIDTH 120 FRAME FImpSinRema USE-TEXT NO-LABELS STREAM-IO NO-BOX.  
         ASSIGN W_sinrema = W_sinrema + 1.
      END.
      ELSE DO:
         DISPLAY Tmp_SinRema.agencia        LABEL "Agencia"
                 Tmp_SinRema.cedula         LABEL "Cedula/Nit"
                 "No Existe el Cliente...." LABEL "Nombres" 
                 Tmp_SinRema.valdist        LABEL "TOTAL DISTRIBUIDO" FORMAT "->>>,>>>,>>>,>>9.99"
         WITH DOWN WIDTH 160 FRAME FImpSinRema1 USE-TEXT NO-LABELS STREAM-IO NO-BOX.      
       END.
  END.
      
  DISPLAY SKIP(1)
  "    TOTAL -->   "
  SPACE(47)
  WTotInco    FORM "->>>>>>,>>>,>>9.99"
    WITH DOWN WIDTH 120 FRAME FTotImpSinRema USE-TEXT NO-LABELS STREAM-IO NO-BOX.

  /* Nuevo */
  OUTPUT CLOSE.
  MESSAGE "El archivo plano esta en " listado
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
  /********/

/* NO-ECHO PAGED PAGE-SIZE 65 */
/*   {Incluido\RepEncabezado.I}                                                                      */
/*   ASSIGN W_Reporte    = "Reporte   : Clientes Sin Remanentes    Fecha del Informe: " +            */
/*                         STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")  */
/*       W_EncColumna = "Empresa: " + STRING(W_CodEmp) + " - Nro.Pago: " + STRING(W_NroPago).        */
/*                                                                                                   */
/*   VIEW FRAME f-ftr.                                                                               */

/* PUT "Agencia;Cedula;Cod_Empre;Vlr.Distri." SKIP(0).                              */
/* ASSIGN W_sinrema = 0.                                                            */
/*                                                                                  */
/* FOR EACH Tmp_SinRema BY cedula:                                                  */
/*     PUT /** Nuevo */                                                             */
/*        Tmp_SinRema.agencia  ";"                                                  */
/*        Tmp_SinRema.cedula   ";"                                                  */
/*        Tmp_SinRema.codempre ";"                                                  */
/*        Tmp_SinRema.valdist FORMAT "->>>,>>>,>>>,>>9.99" SKIP(0).                 */
/*     ASSIGN W_sinrema = W_sinrema + 1.                                            */
/* END.                                                                             */
/* PUT "TOTAL...Registros : " + STRING(W_sinrema,"zzzzzz") FORMAT "X(120)" SKIP(0). */
/*                                                                                  */
/* FOR EACH Tmp_SinRema: DELETE Tmp_SinRema. END.                                   */
/* MESSAGE "El archivo plano esta en " listado VIEW-AS ALERT-BOX INFO BUTTONS OK.   */
/* OUTPUT CLOSE.                                                                    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovAhorro W-Rec_XLibranza 
PROCEDURE MovAhorro :
CREATE Mov_Ahorros.
Mov_Ahorros.Agencia = Ahorros.Agencia.
Mov_Ahorros.Age_Destino = Ahorros.Agencia.
Mov_Ahorros.Age_Fuente = W_Agencia.
Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro.
Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro.
Mov_Ahorros.Fecha = W_Fecha.
Mov_Ahorros.Hora = TIME.
Mov_Ahorros.Nit = Ahorros.Nit.
Mov_Ahorros.Num_Documento = STRING(Comprobantes.Secuencia).
Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.
Mov_Ahorros.Usuario = W_Usuario.
Mov_Ahorros.Val_Efectivo = TPdctos.VrDist.
Mov_Ahorros.Cpte = Comprobantes.Comprobante.
Mov_Ahorros.Cod_Operacion = W_OpAboAho.
Mov_Ahorros.Descrip = "Consignaci�n por distribuci�n de N�mina".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Rec_XLibranza 
PROCEDURE ProcesoImprimir :
IF W_SiCpte THEN DO:
    MESSAGE "El proceso de distribuci�n ha finalizado con �xito"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    CREATE ctr_ProcDistribucion.
    ASSIGN ctr_ProcDistribucion.agencia = w_agencia
           ctr_ProcDistribucion.cod_empresa = W_CodEmp
           ctr_ProcDistribucion.periodo = integer(w_sem)
           ctr_ProcDistribucion.ano = YEAR(w_fecha)
           ctr_ProcDistribucion.fec_distribucion = w_fecha.

    IF MONTH(w_fecha) = 1 AND integer(w_sem) >= 12 THEN
        ctr_ProcDistribucion.ano = ctr_ProcDistribucion.ano - 1.

    RUN ImpCpte.
    RETURN.
END.

IF W_SiImp THEN DO:
    RUN InfImport.
    OUTPUT CLOSE.
    RETURN.
END.

IF W_SiImpF THEN DO:
    RUN InfFrame.
    RETURN.
END.

{Incluido\RepEncabezado.I}

DO WITH FRAME F_Proc.
END.

ASSIGN W_Reporte = "Reporte   : Distribuci�n Preliminar de Recaudo      Fecha del Informe: " + STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
       W_EncColumna = "AgeAso Nit        Cuentas     AgePro Pdcto-Descripci�n                      Cuota     Vr.Distribuido    NC    Sdo.Ant.Pdcto.   Sdo.Actual/Difer."
       TDist = 0
       TTDist = 0
       TSdo = 0
       TTSdo = 0
       TRec = 0
       TDif = 0.

DEFINE VARIABLE znro AS INTEGER INITIAL 0.
DEFINE VARIABLE zcompr LIKE comprobantes.comprobante INITIAL 7.

IF W_SiContab THEN DO:
    W_Reporte = "Reporte   : Distribuci�n Contabilizada de Recaudo      Fecha del Informe: " + STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS").

    ASSIGN znro = Comprobantes.Secuencia
           zcompr = comprobante.comprobante.
END.

VIEW FRAME F-Encabezado.
VIEW FRAME f-ftr.

DISPLAY SKIP(0)
        "Empresa : " + W_CmbEmp:SCREEN-VALUE + " Nro.de Pago : " + W_NroPago:SCREEN-VALUE FORMAT "X(120)" SKIP(1)
    WITH DOWN WIDTH 160 FRAME F1 NO-LABELS.

DEFINE VARIABLE XSdoFin LIKE CREDITOS.SDO_CAPITAL.

DEFINE VARIABLE w_acliente LIKE clientes.agencia.

FOR EACH CopTPdctos USE-INDEX Nit BREAK BY CopTPdctos.Nit:
    ASSIGN TDist = TDist + CopTPdctos.VrDist
           TSdo = TSdo + CopTPdctos.VrSdo.

    IF CopTPdctos.TP = "A" THEN
        XSdoFin = CopTPdctos.VrSdo + CopTPdctos.VrDist.
    ELSE
        XSdoFin = CopTPdctos.VrSdo - CopTPdctos.VrDist.

    TDif = TDif + XSdoFin.

    FIND FIRST clientes WHERE clientes.nit EQ NitP NO-ERROR.
    IF AVAILABLE(clientes) THEN
        w_acliente = clientes.agencia.
    ELSE
        w_acliente = 0.

    DISPLAY w_acliente
            CopTPdctos.NitP
            CopTPdctos.CtaP        
            CopTPdctos.Agen
            CopTPdctos.CodP        
            "-"
            CopTPdctos.NomP FORMAT "X(22)"
            SPACE(3)
            CopTPdctos.CuoOrig FORMAT "->>>,>>>,>>9.99"
            SPACE(4)
            CopTPdctos.VrDist FORMAT "->>>,>>>,>>9.99"
            SPACE(3)
            CopTPdctos.NC
            SPACE(3)
            CopTPdctos.VrSdo FORMAT "->>>,>>>,>>9.99"
            SPACE(4)
            XSdoFin FORMAT "->>>,>>>,>>9.99"
        WITH DOWN WIDTH 160 FRAME Det NO-BOX NO-LABELS STREAM-IO USE-TEXT.

    CREATE Mov_Distrib.
    ASSIGN Mov_Distrib.Agencia = CopTPdctos.Agen
           Mov_Distrib.Ano = YEAR(W_Fecha)
           Mov_Distrib.Sem = W_Sem
           Mov_Distrib.ApellyNomb = CopTPdctos.NomC 
           Mov_Distrib.Nit = CopTPdctos.NitP 
           Mov_Distrib.Cod_Empresa = W_CodEmp 
           Mov_Distrib.Tip_Pdcto = CopTPdctos.TP
           Mov_Distrib.Nom_Producto = CopTPdctos.NomP
           Mov_Distrib.Cod_Pdcto = CopTPdctos.CodP
           Mov_Distrib.Cpte = zcompr   /* Comprobantes.Comprobante */
           Mov_Distrib.Cta_Pdcto = CopTPdctos.CtaP
           Mov_Distrib.Cuota = CopTPdctos.CuoOrig
           Mov_Distrib.Val_Distrib = CopTPdctos.VrDist
           Mov_Distrib.Fec_Dist = W_Fecha
           Mov_Distrib.Nro_Pago = W_NroPago
           Mov_Distrib.Num_Cuotas = CopTPdctos.NC
           Mov_Distrib.Num_Document = string(znro) /* STRING(Comprobantes.Secuencia) */
           Mov_Distrib.Sdo_Actual = XSdoFin
           Mov_Distrib.Sdo_Anterior = CopTPdctos.VrSdo 
           Mov_Distrib.Val_TAbono = CopTPdctos.VRec.

    IF INTEG(W_SEm) GE 12 AND MONTH(W_Fecha) EQ 1 THEN
        Mov_Distrib.Ano = Mov_Distrib.Ano - 1.

    IF LAST-OF(CopTPdctos.Nit) THEN
        RUN TotXNit.
END.

DISPLAY SKIP(5)
        "Total General   :"
        SPACE(46) /*48*/
        TRec FORMAT "->>>,>>>,>>>,>>9.99"
        SPACE(0)
        TTDist FORMAT "->>>,>>>,>>>,>>9.99"
        SPACE(4)
        TTSdo FORMAT "->>>,>>>,>>>,>>9.99"
        SPACE(0)
        (TTDist - TRec) FORMAT "->>>,>>>,>>>,>>9.99"
    WITH DOWN WIDTH 160 FRAME TotGral NO-BOX NO-LABELS STREAM-IO USE-TEXT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QueryRec W-Rec_XLibranza 
PROCEDURE QueryRec :
W_TotRec = 0.
W_TotRec:SCREEN-VALUE IN FRAME F_Proc = "0.00".

CLOSE QUERY Br_Rec.
CLOSE QUERY Br_Pdctos.

vdfecpago = ?.
viano = 0.

FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa = W_CodEmp
                      AND Rec_Nomina.Nro_Pago = W_NroPago
                      AND Rec_Nomina.Fec_Contab = ?
                      AND Rec_Nomina.Num_Cuotas <> 99 NO-LOCK:
    IF rec_nomina.carnet <> "1" THEN
        NEXT.

    W_TotRec = W_TotRec + Rec_Nomina.Val_Deduc.
    W_TotRec:SCREEN-VALUE = STRING(W_TotRec).

    IF vdfecpago = ? THEN DO:
        vdfecpago = rec_nomina.fec_pago.
        viano = YEAR(vdfecpago).

        IF MONTH(vdfecpago) = 1 THEN DO:
            vimesini = 12.
            vimesfin = MONTH(vdfecpago).
        END.
        ELSE DO:
            vimesini = MONTH(vdfecpago) - 1.
            vimesfin = MONTH(vdfecpago).
        END.
    END.
END.

OPEN QUERY Br_Rec FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa = W_CodEmp
                                        AND Rec_Nomina.Nro_Pago = W_NroPago
                                        AND Rec_Nomina.Fec_Contab = ?
                                        AND Rec_Nomina.Num_Cuotas <> 99
                                        AND rec_nomina.carnet = "1" NO-LOCK,
                      EACH Clientes WHERE Clientes.Nit = Rec_Nomina.Nit
                                       AND Clientes.Estado = 1 NO-LOCK.
RUN ComboPagos.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TotXNit W-Rec_XLibranza 
PROCEDURE TotXNit :
/*------------------------------------------------------------------------------
  Purpose:     
  ------------------------------------------------------------------------------*/
  DISPLAY SPACE(4)
          CopTPdctos.NitP
          SPACE(1)
          CopTPdctos.NomC                       FORMAT "X(47)"
          SPACE(3)
          CopTPdctos.VRec                       FORMAT "->>>,>>>,>>9.99"
          SPACE(4)
          TDist                                 FORMAT "->>>,>>>,>>9.99"
          SPACE(8)
          TSdo                                  FORMAT "->>>,>>>,>>9.99"
          SPACE(4)
          /*TDif                                  FORMAT "->>>,>>>,>>9.99" SKIP(1)*/
          (TDist - CopTPdctos.VRec)             FORMAT "->>>,>>>,>>9.99" SKIP(1) 
        WITH DOWN WIDTH 160 FRAME Tot1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  ASSIGN TTDist = TTDist + TDist
         TTSdo  = TTSdo  + TSdo
         TRec   = TRec   + CopTPdctos.VRec
         TTDif  = TTDif  + TDif
         /*TDif   = TDif   + (TDist - CopTPdctos.VRec) */
         TDist  = 0
         TSdo   = 0
         TDif   = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida W-Rec_XLibranza 
PROCEDURE Valida :
FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado = 1 NO-LOCK BY Pro_Ahorros.Cod_Ahorro:
    FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto = 1
                          AND CortoLargo.Cod_Producto = Pro_Ahorros.Cod_Ahorro
                          AND CortoLargo.Plazo_Inicial >= 0 NO-LOCK BREAK BY CortoLargo.Agencia
                                                                          BY CortoLargo.Cod_Producto
                                                                          BY CortoLargo.Plazo_Inicial:
        IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
            FIND FIRST Cuentas WHERE Cuentas.Cuenta = CortoLargo.Cta_AsoAd
                                 AND Cuentas.Tipo = 2 NO-LOCK NO-ERROR.
            IF AVAILABLE(Cuentas) THEN
                FIND FIRST Cuentas WHERE Cuentas.Cuenta = CortoLargo.Cta_SyA
                                     AND Cuentas.Tipo = 2 NO-LOCK NO-ERROR.

            IF NOT AVAILABLE(Cuentas) THEN DO:
                MESSAGE "En CortoLargo.Cta_AsoAd y CortoLargo.Cta_SyA deben existir en Cuentas..." SKIP
                        "Para el Pro_Ahorros.Cod_Ahorro:" Pro_Ahorros.Cod_Ahorro SKIP
                        "De la Agencia:" CortoLargo.Agencia
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            CREATE TempCtas.
            TempCtas.Age = CortoLargo.Agencia.
            TempCtas.TipP = "A".
            TempCtas.Pto = CortoLargo.Cod_Producto.
            TempCtas.CtaPro = CortoLargo.Cta_AsoAd.
            TempCtas.CtaSyA = CortoLargo.Cta_SyA.
        END.
    END.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica_Remanente W-Rec_XLibranza 
PROCEDURE Verifica_Remanente :
w_totrema  = 0.
w_grarema  = 0.
w_conseaho = 0.

DO:
    FOR EACH Ahorros WHERE Ahorros.Nit = W_NitCte
                       AND Ahorros.cod_ahorro = 8 NO-LOCK:
        w_totrema = w_totrema + 1.

        IF Ahorros.estado = 1 THEN
            w_grarema = 1.
    END.

    IF w_grarema = 0 AND w_totrema = 0 THEN DO:
        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro = 1
                                 AND Pro_Ahorros.Cod_ahorro = 8 EXCLUSIVE-LOCK NO-ERROR.
        
        DO WHILE LOCKED(pro_Ahorro):
            MESSAGE "El registro de remanente est� siendo usado" SKIP
                    "por otro usuario.. Espera o Aborta. Nit:" Ahorros.Nit
                VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE wespera AS LOGICAL.

            IF wespera THEN
                FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro = 1
                                         AND Pro_Ahorros.Cod_ahorro = 8 EXCLUSIVE-LOCK NO-ERROR.
            ELSE DO:
                MESSAGE "Error por registro pro_ahorro bloqueado" SKIP
                        "Nit" Ahorros.Nit
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.
        END.

        IF AVAILABLE Pro_Ahorros THEN DO:
            IF Pro_Ahorros.Id_Consecutivo THEN DO:
                w_conseaho = Pro_Ahorros.Num_Consecutivo + 1.

                FIND FIRST Clientes WHERE Clientes.Nit = W_NitCte
                                      AND Clientes.Cod_Empresa = W_CodEmp
                                      AND Clientes.Estado = 1 NO-LOCK NO-ERROR.
                IF AVAILABLE(Clientes) THEN DO:
                    CREATE Ahorros.
                    Ahorros.Cue_Ahorros = STRING(w_conseaho).
                    Ahorros.Detalle_Estado = 2.
                    Ahorros.Agencia = clientes.agencia.
                    Ahorros.Tip_Ahorro = 1.
                    Ahorros.Cod_ahorro = 8.
                    Ahorros.For_Pago = 1.
                    Ahorros.Nit = Clientes.Nit.
                    Ahorros.Per_Liquidacion = 2.
                    Ahorros.For_Liquidacion = Pro_Ahorros.For_Liquidacion.
                    Ahorros.Estado = 1.
                    Ahorros.Tasa = 0.
                    Ahorros.Usu_Creacion = W_Usuario.
                    Ahorros.IdNombre = Clientes.Nombre.
                    Ahorros.IdApellido1 = Clientes.Apellido1.
                    Ahorros.Cuota = 0.
                    Ahorros.Plazo = 9999.
                    Ahorros.Per_Deduccion = 4.
                    Ahorros.Monto_Apertura = 0.

                    FIND CURRENT Pro_Ahorros NO-ERROR.
                    Pro_Ahorros.Num_Consecutivo = w_conseaho.

                    FIND CURRENT Pro_Ahorros NO-LOCK.
                END.
                ELSE DO:
                    MESSAGE "Registro de cliente no encontrado o estado Inactivo" SKIP
                            "Nit:" W_NitCte SKIP
                            "Empresa:" W_CodEmp SKIP
                            "Cambiar de Pagadur�a"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    RETURN ERROR.
                END.
            END.
            ELSE DO:
                MESSAGE "No tiene consecutivo" SKIP
                        "Nit:" Ahorros.Nit
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.
        END.
    END.
    ELSE DO:
        IF w_grarema = 0 AND w_totrema >= 1 THEN DO:
            FIND FIRST Ahorros WHERE Ahorros.Nit = W_NitCte
                                 AND Ahorros.cod_ahorro = 8
                                 AND Ahorros.estado = 2 EXCLUSIVE-LOCK NO-ERROR.
            DO WHILE LOCKED(Ahorros):
                MESSAGE "El registro de Remanente est� siendo usado" SKIP
                        "por otro Usuario... Espera o Aborta"
                    VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE wespera1 AS LOGICAL.

                IF wespera1 THEN
                    FIND FIRST Ahorros WHERE Ahorros.Nit = W_NitCte
                                         AND Ahorros.cod_ahorro = 8
                                         AND Ahorros.estado = 2 EXCLUSIVE-LOCK NO-ERROR.
                ELSE DO:
                    MESSAGE "Registro ahorros lookeado"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    RETURN ERROR.
                END.
            END.

            IF AVAILABLE(Ahorros) THEN DO:
                Ahorros.Estado = 1.

                FIND CURRENT Ahorros NO-LOCK.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

