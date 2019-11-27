&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE VAR CtrLavado AS LOGICAL INITIAL YES.
DEFINE VAR T_OpeD AS INTEGER.
DEFINE VAR F_Seg AS INTEGER.
DEFINE VAR T_Ope LIKE Operacion.Cod_Operacion.
DEFINE VAR T_Ded LIKE Operacion.Cod_deducible.
DEFINE VAR T_Cld LIKE Deducible.Cla_Deducible.
DEFINE VAR T_Ndd LIKE Deducible.Nom_Deducible.
DEFINE VAR T_Ctd LIKE Deducible.Cuenta.
DEFINE VAR T_VIm LIKE Deducible.Valor_Impuesto.
DEFINE VAR T_CIm LIKE Deducible.Cuenta_Impuesto.
DEFINE VAR T_Imp AS LOGICAL.
DEFINE VAR W_MaxDia LIKE Ahorros.Sdo_Disponible.
DEFINE VAR W_Cta4 LIKE Cuentas.Cuenta.
DEFI VAR P_ImpAplic LIKE Ahorros.Sdo_Disponible INIT 0.
DEFI VAR W_Rev AS LOG INIT FALSE.
DEFI VAR WCpteTx LIKE Comprobantes.Comprobante.
DEFI VAR W_SiChGir AS LOG INIT FALSE.
DEFI VAR W_SiAplGMF AS LOG INIT FALSE.
DEFINE VARIABLE RutaFoto AS CHARACTER FORMAT "x(80)" INITIAL "imagenes\fotos\0.jpg"  NO-UNDO.
DEFI VAR W_AgAportes LIKE Ahorros.Agencia.
DEFI VAR W_CtaSyACMV LIKE Cuentas.Cuenta.
DEFI VAR W_SiAfilia AS LOG INIT FALSE.
DEFI VAR W_DocContab AS INTEGER.
DEFINE VAR numDocSyA AS INTEGER.
DEFI VAR T_SdoF LIKE Ahorros.Sdo_disponible.
DEFI VAR T_SdoI LIKE Ahorros.Sdo_disponible.
DEFI VAR NroCpte LIKE Taquilla.Nro_Transaccion.
DEFI VAR Si_Reval AS LOG INIT FALSE.
DEFINE VARIABLE W_Ok AS LOGICAL.
DEFINE VARIABLE F_Tipo AS INTEGER FORMAT "99".
DEFINE VARIABLE X_Nit LIKE Clientes.NIT.
DEFINE VARIABLE Xdb LIKE Anexos.Sdo_Final.
DEFINE VARIABLE Xcr LIKE Anexos.Sdo_Final.
DEFINE VARIABLE Xsd AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VARIABLE Xms AS INTEGER FORMAT "99".
DEFINE VARIABLE Xan AS INTEGER FORMAT "9999".
DEFINE VARIABLE Vr_Deducible LIKE Ahorros.Sdo_disponible.
 
DEFINE VAR ValFac AS DECIMAL FORMAT ">>>,>>>,>>9".

{Incluido/Variable.I "SHARED"}

DEFINE VARIABLE W_NumSeq AS INTEGER FORMAT "9999999".
DEFINE VARIABLE W_SecuenciaImprimir AS INTEGER FORMAT "9999999".
DEFINE VARIABLE W_CodPto LIKE ahorros.cod_ahorro.
DEFINE VARIABLE Open-Recid AS RECID.
DEFINE VARIABLE Open-On-Row AS INTEGER.
DEFINE VARIABLE Cont AS INTEGER.
DEFINE VARIABLE W_Error AS LOGICAL.
DEFINE VARIABLE W_Autorizo AS CHARACTER.
DEFINE VARIABLE Sw_Estado AS LOGICAL.
DEFINE VARIABLE AuxCta AS CHARACTER.
DEFINE VARIABLE CtaCble AS CHARACTER.
DEFINE VARIABLE CtaSYA_Des AS CHARACTER.
DEFINE VARIABLE CtaSYA_FTe AS CHARACTER.
DEFINE VARIABLE T_OfiIni AS INTEGER.
DEFINE VARIABLE T_OfiFin AS INTEGER.
DEFINE VARIABLE V_Operacion AS INTEGER.
DEFINE VARIABLE W_Grupo AS INTEGER.
DEFINE VARIABLE W_Nomoper AS CHARACTER.
DEFINE VARIABLE ValCheq AS DECIMAL.
DEFINE VARIABLE W_LiqIntDb AS CHARACTER.
DEFINE VARIABLE W_LiqIntCr AS CHARACTER.
DEFINE VARIABLE W_LiqMor AS CHARACTER.
DEFINE VARIABLE W_DifCdb AS CHARACTER.
DEFINE VARIABLE W_DifCcr AS CHARACTER.
DEFINE VARIABLE Cbte AS INTEGER.
DEFINE VARIABLE Docto AS CHARACTER.

   /* oakley */

   DEFINE VARIABLE Val2XMIl    LIKE Ahorros.Sdo_Disponible.
   DEFINE VARIABLE ValAnt      LIKE Ahorros.Sdo_Disponible.
   DEFINE VARIABLE LibChe        AS LOGICAL.
   DEFINE VARIABLE Cta_Caja    LIKE Cuentas.Cuenta.
   DEFINE VARIABLE Cta_Banco   LIKE Cuentas.Cuenta.
   DEFINE VARIABLE AuxCaja     LIKE Cuentas.Cuenta.
   DEFINE VARIABLE AuxBanco    LIKE Cuentas.Cuenta.
   DEFINE VARIABLE AuxTrasl    LIKE Cuentas.Cuenta.
   DEFINE VARIABLE CtaSyA      LIKE Cuentas.Cuenta.
   DEFINE VARIABLE T_Valor     LIKE Creditos.Sdo_Capital.
   DEFINE VARIABLE T_Difer     LIKE Creditos.Sdo_Capital.
   DEFINE VARIABLE T_Inter     LIKE Creditos.Sdo_Capital.
   DEFINE VARIABLE W_CtaBco    LIKE Cuentas.Cuenta.
   DEFINE VARIABLE W_SYA         AS LOGICAL.
   DEFINE VARIABLE IDCaja      LIKE Cuentas.Cod_Caja.

   /* Variables Rutina de Distribución de Créditos. */
   
   DEFINE VARIABLE W_AboMora    LIKE Creditos.Cuota.
   DEFINE VARIABLE W_IntXcobrar LIKE Creditos.Cuota.
   DEFINE VARIABLE W_IntDifCob  LIKE Creditos.Cuota.
   DEFINE VARIABLE W_VrCapMor   LIKE Creditos.Cuota.
   DEFINE VARIABLE W_IntPagAnti LIKE Creditos.Cuota.
   DEFINE VARIABLE W_IntPagCorr LIKE Creditos.Cuota.
   DEFINE VARIABLE W_VlrPagCap  LIKE Creditos.Cuota.
   DEFINE VARIABLE W_IntAntiDis LIKE Creditos.Cuota.
   DEFINE VARIABLE W_VlrCapDis  LIKE Creditos.Cuota.
   DEFINE VARIABLE W_CuoPagadas   AS INTEGER FORMAT "9999".
   DEFINE VARIABLE W_TotPDist   LIKE Creditos.Cuota.
   DEFINE VARIABLE W_VrSobra    LIKE Creditos.Cuota.
   DEFINE VARIABLE W_PlaPer       AS INTEGER FORMAT "999".
   DEFINE VARIABLE WXnat       LIKE Taquilla.naturaleza INITIAL "" NO-UNDO.

   DEFI TEMP-TABLE CopMov LIKE Mov_Contable.
   
   DEFINE TEMP-TABLE Cheques
      FIELD W_Secue    AS   INTEGER
      FIELD W_Orden    AS   DECIMAL DECIMALS 2
      FIELD W_CodPto   LIKE ahorros.cod_ahorro
      FIELD W_Cuenta   LIKE Cuentas.Cuenta
      FIELD W_Dto      AS   CHARACTER FORMAT "X(11)" 
      FIELD W_Banco    LIKE Che_Transito.Cod_Compensa  LABEL " Banco "
      FIELD W_Cheque   LIKE Che_Transito.Cheque LABEL "Nro.Cheque"
      FIELD W_Canje    AS   INTEGER FORMAT "9" INITIAL 1 LABEL "T . C":C
      FIELD W_Valor    LIKE Che_Transito.Valor  LABEL "Valor":C
      INDEX Idx_Cheq   IS PRIMARY W_Secue  ASCENDING
      INDEX Idx_Cheq1  W_CodPto W_Cuenta ASCENDING.
   
   DEFINE TEMP-TABLE Producto
      FIELD W_Order  AS   DECIMAL DECIMALS 2 
      FIELD OfiTem   LIKE Agencias.Agencia            LABEL "Ofi ":C
      FIELD OperaAux LIKE Taquilla.Cod_Operacion      LABEL "OP"
      FIELD TipPto   AS   CHARACTER FORMAT "X(3)"     LABEL "Tipo"       INITIAL ""    
      FIELD CodPto   LIKE ahorros.cod_ahorro        LABEL "Código"     FORMAT ">>>"
      FIELD NomPto   LIKE Pro_Ahorros.Nom_Producto    LABEL "Producto/Cuenta":C
      FIELD Id_Tal   LIKE Pro_Ahorros.Id_Talonario    INITIAL 0
      FIELD DtoRef   AS   CHARACTER FORMAT "X(11)"    LABEL "Doc.Referencia":C INITIAL ""
      FIELD EC       AS   CHARACTER FORMAT "X(1)"     LABEL "E/C"
      FIELD Cuenta   AS   CHARACTER FORMAT "X(14)"    LABEL "Cuenta/Pagare":C
      FIELD CtaAnt   LIKE Ahorros.Num_Formato
      FIELD CueBco   AS   CHARACTER FORMAT "X(14)"
      FIELD Canje    LIKE Che_Transito.Tip_Remesa
      FIELD D_Cheq   AS   INTEGER                     LABEL "DC":C FORMAT ">9"
      FIELD Formato  LIKE Cuentas.Cod_Formato
      FIELD NitCta   LIKE Clientes.Nit
      FIELD Banco    LIKE Che_Transito.Cod_Compensa   LABEL "Banco"
      FIELD Cheque   LIKE Che_Transito.Cheque         LABEL "Nro.Cheque"
      FIELD Benefi   AS   CHARACTER                   FORMAT "X(50)" INITIAL ""
      FIELD ConsigEf LIKE Ahorros.Sdo_Disponible      LABEL "Consig/Efectivo":C FORMAT ">>>>,>>>,>>9.99"
      FIELD Debe     LIKE Ahorros.Sdo_Disponible      LABEL "Tot.Consig/Pago":C FORMAT "->>>>,>>>,>>9.99"
      FIELD Haber    LIKE Ahorros.Sdo_Disponible      LABEL "Retirar":C         FORMAT "->>>>,>>>,>>9.99"
      FIELD UsuAut   LIKE Usuarios.Usuario            INITIAL ""
      FIELD Retiro   AS   LOGICAL                     LABEL "I.C " INITIAL FALSE
      FIELD MinCta   AS DECIMAL FORMAT ">>>,>>>,>>9"
      FIELD EstCta   LIKE Ahorros.Detalle_Estado
        INDEX Idx_Pto IS PRIMARY W_Order ASCENDING.
      
   DEFINE BUFFER Tmp_Producto FOR Producto.
   DEFINE BUFFER Bpro_ahorros FOR pro_ahorros.
   DEFINE BUFFER BAhorros FOR Ahorros.
      
   DEFINE BUFFER Tmp_Cuentas  FOR Cuentas.
   DEFINE BUFFER Tmp_Cheques  FOR Cheques.
   DEFINE BUFFER Tmp_Pto      FOR Producto.

  DEFI VAR W_NroBcoIgual LIKE Producto.CueBco.
  DEFI VAR W_CtaOpe    LIKE Operacion.Cuenta.            
   DEFI VAR W_CodDed    LIKE Operacion.Cod_Deducible.
   DEFI VAR W_Comision  LIKE Operacion.Comision.        
   DEFI VAR W_Operacion LIKE Operacion.Cod_Operacion. 
   DEFI VAR W_DedCta    LIKE Deducible.Cuenta.              
   DEFI VAR W_DedCla    LIKE Deducible.Cla_Deducible.
   DEFI VAR W_DedVal    LIKE Deducible.Valor.      
   DEFI VAR W_DedValImp LIKE Deducible.Valor_Impuesto.
   DEFI VAR W_DedCtaImp LIKE Deducible.Cuenta_Impuesto.  
   DEFI VAR W_DedNom    LIKE Deducible.Nom_Deducible.
   DEFI VAR W_ComVal    LIKE Pro_Ahorro.Val_Talonario. 
   DEFI VAR W_ComCta    LIKE Deducible.Cuenta.
   DEFI VAR W_ComCla    LIKE Deducible.Cla_Deducible.
   DEFI VAR W_ComValImp LIKE Deducible.Valor_Impuesto.
   DEFI VAR W_ComCtaImp LIKE Deducible.Cuenta_Impuesto.
   DEFI VAR W_ComNom    LIKE Deducible.Nom_Deducible. 
   DEFI VAR W_CtaCor    LIKE CortoLargo.Cta_AsoAd.
   DEFI VAR W_Total     LIKE Pro_Ahorro.Val_Talonario.

DEFINE TEMP-TABLE tmp-tarjetadb LIKE tarjetadebito.

/*giocam Oct 11/07 Implementacion OCX PSTimer*/
DEFINE VAR vCon LIKE Ahorros.Sdo_Disponible INITIAL 0.
DEFINE VAR vRet LIKE Ahorros.Sdo_Disponible  INITIAL 0.
DEFINE VARIABLE vdeEfectivo AS DECIMAL INITIAL 0 NO-UNDO. /*calcula efectivo en caja*/
DEFINE VARIABLE vcMensaje AS CHARACTER   NO-UNDO.

/* 23-Abril-2008  Félix Vargas*/
DEFINE VAR viagencia  AS INTEGER   FORMAT "999"   NO-UNDO.
DEFINE VAR vcnit      AS CHARACTER FORMAT "X(12)" NO-UNDO.
DEFINE VAR vcnombre   AS CHARACTER FORMAT "X(30)" NO-UNDO.  /*Falta*/
DEFINE VAR vinotran   AS INTEGER   INITIAL 0 NO-UNDO.
/* DEFINE VAR vthora     AS INTEGER   INITIAL ?      NO-UNDO. */
DEFINE VAR vdfecha    AS DATE      INITIAL ?      NO-UNDO.
DEFINE VAR viagen     AS INTEGER   INITIAL 0      NO-UNDO.
DEFINE VAR vinrocpte  AS INTEGER   FORMAT "99999999" NO-UNDO.
DEFINE VAR vcdetalle  AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VAR vccheque   AS CHARACTER FORMAT "X(12)" INITIAL "" NO-UNDO.
DEFINE VAR vcctadb    AS CHARACTER FORMAT "X(14)" NO-UNDO.
DEFINE VAR vcctacr    AS CHARACTER FORMAT "X(14)" NO-UNDO.
DEFINE VAR vdnomctadb AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VAR vdnomctacr AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VAR vddebito   AS DECIMAL   FORMAT "->>>,>>>,>>>,>>9" NO-UNDO.
DEFINE VAR vdcredito  AS DECIMAL   FORMAT "->>>,>>>,>>>,>>9" NO-UNDO.
DEFINE VAR vcimpcpte  AS CHARACTER NO-UNDO.
DEFINE VAR flagCtaSucyAg AS LOGICAL INITIAL FALSE.

DEFINE TEMP-TABLE ttProducto LIKE producto.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-10

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cuentas Operacion Taquilla Producto Cheques ~
Detalle

/* Definitions for BROWSE BROWSE-10                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-10 Operacion.Cod_Compensa ~
Cuentas.Cuenta Cuentas.Nombre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-10 
&Scoped-define QUERY-STRING-BROWSE-10 FOR EACH Cuentas ~
      WHERE Cuentas.Cod_FlujoEfec = "D" ~
 AND Cuentas.Car_Efectivo = 3 ~
 AND Cuentas.Estado = 1 ~
 AND INTEGER(Cuentas.Cta_Homologada) = w_agencia NO-LOCK, ~
      EACH Operacion OF Cuentas ~
      WHERE Operacion.Clase_Operacion = 1 ~
 AND Operacion.Ctrl_EfeChe = 2 ~
 AND Operacion.Tipo_Operacion = 2 NO-LOCK ~
    BY Operacion.Cod_Compensa
&Scoped-define OPEN-QUERY-BROWSE-10 OPEN QUERY BROWSE-10 FOR EACH Cuentas ~
      WHERE Cuentas.Cod_FlujoEfec = "D" ~
 AND Cuentas.Car_Efectivo = 3 ~
 AND Cuentas.Estado = 1 ~
 AND INTEGER(Cuentas.Cta_Homologada) = w_agencia NO-LOCK, ~
      EACH Operacion OF Cuentas ~
      WHERE Operacion.Clase_Operacion = 1 ~
 AND Operacion.Ctrl_EfeChe = 2 ~
 AND Operacion.Tipo_Operacion = 2 NO-LOCK ~
    BY Operacion.Cod_Compensa.
&Scoped-define TABLES-IN-QUERY-BROWSE-10 Cuentas Operacion
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-10 Cuentas
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-10 Operacion


/* Definitions for BROWSE BROWSE-12                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-12 Taquilla.Nit Taquilla.Nro_Transaccion Taquilla.Val_Cheque Taquilla.Val_Efectivo Taquilla.Tip_Producto Taquilla.Cod_Producto Taquilla.Cuenta Taquilla.Nro_Cuenta   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-12   
&Scoped-define SELF-NAME BROWSE-12
&Scoped-define OPEN-QUERY-BROWSE-12 /*OPEN QUERY {&SELF-NAME} FOR EACH Taquilla WHERE Taquilla.Nit EQ "00" NO-LOCK                                             BY  Taquilla.Nit  INDEXED-REPOSITION.*/.
&Scoped-define TABLES-IN-QUERY-BROWSE-12 Taquilla
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-12 Taquilla


/* Definitions for BROWSE BROWSE-5                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-5 Producto.OfiTem Producto.NomPto Producto.Cuenta Producto.CtaAnt Producto.D_Cheq Producto.DtoRef Producto.Debe Producto.Haber Producto.EC Producto.Retiro " " Producto.ConsigEf   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-5 Producto.EC Producto.Cuenta Producto.DtoRef Producto.Debe Producto.Haber Producto.Retiro   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-5 Producto
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-5 Producto
&Scoped-define SELF-NAME BROWSE-5
&Scoped-define QUERY-STRING-BROWSE-5 FOR EACH Producto
&Scoped-define OPEN-QUERY-BROWSE-5 OPEN QUERY BROWSE-5 FOR EACH Producto.
&Scoped-define TABLES-IN-QUERY-BROWSE-5 Producto
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-5 Producto


/* Definitions for BROWSE BROWSE-7                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-7 Cheques.W_Canje Cheques.W_Banco Cheques.W_Cheque Cheques.W_Valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-7 Cheques.W_Banco Cheques.W_Canje Cheques.W_Cheque Cheques.W_Valor   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-7 Cheques
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-7 Cheques
&Scoped-define SELF-NAME BROWSE-7
&Scoped-define QUERY-STRING-BROWSE-7 FOR EACH Cheques
&Scoped-define OPEN-QUERY-BROWSE-7 OPEN QUERY BROWSE-7 FOR EACH Cheques.
&Scoped-define TABLES-IN-QUERY-BROWSE-7 Cheques
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-7 Cheques


/* Definitions for BROWSE BROWSE-9                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-9 Cuentas.Cuenta TRIM(Operacion.Nom_Oper) Cuentas.Naturaleza Cuentas.Id_detalle   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-9   
&Scoped-define SELF-NAME BROWSE-9
&Scoped-define QUERY-STRING-BROWSE-9 FOR EACH Cuentas       WHERE Cuentas.Id_Caja = TRUE  AND Cuentas.Estado = 1 NO-LOCK, ~
             EACH Operacion OF Cuentas       WHERE Operacion.Tipo_Producto = 4  AND Operacion.Clase_Operacion = 1  AND Operacion.Ctrl_EfeChe = 3 NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-9 OPEN QUERY {&SELF-NAME} FOR EACH Cuentas       WHERE Cuentas.Id_Caja = TRUE  AND Cuentas.Estado = 1 NO-LOCK, ~
             EACH Operacion OF Cuentas       WHERE Operacion.Tipo_Producto = 4  AND Operacion.Clase_Operacion = 1  AND Operacion.Ctrl_EfeChe = 3 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-9 Cuentas Operacion
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-9 Cuentas
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-9 Operacion


/* Definitions for BROWSE BROWSE-Detalle                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Detalle Detalle.Doc_referencia Detalle.Plazo Detalle.Fec_ProntoPago Detalle.Valor_amortizacion Detalle.Valor_inicial Detalle.Fec_Grabacion Detalle.Cr Detalle.Db ValFac   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Detalle   
&Scoped-define SELF-NAME BROWSE-Detalle
&Scoped-define QUERY-STRING-BROWSE-Detalle FOR EACH Detalle NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Detalle OPEN QUERY {&SELF-NAME} FOR EACH Detalle NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Detalle Detalle
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Detalle Detalle


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-5}

/* Definitions for FRAME Frame-Cuentas                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frame-Cuentas ~
    ~{&OPEN-QUERY-BROWSE-9}

/* Definitions for FRAME Frame-Cuentas-Bancos                           */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frame-Cuentas-Bancos ~
    ~{&OPEN-QUERY-BROWSE-10}

/* Definitions for FRAME FRAME-Detalle                                  */

/* Definitions for FRAME Frame_Cheques                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frame_Cheques ~
    ~{&OPEN-QUERY-BROWSE-7}

/* Definitions for FRAME F_ReVal                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_ReVal ~
    ~{&OPEN-QUERY-BROWSE-12}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_ReValid Imp_Cdat btn_autorizados ~
Btn_ImpSaldo BUTTON-1 CMB_Tipo Btn_Descripcion BROWSE-5 Btn_Grabar ~
Btn_Cancelar Btn_Salir Btn_Ayuda BUTTON-58 RECT-139 RECT-227 
&Scoped-Define DISPLAYED-OBJECTS CMB_Tipo F_Nit F_Agencia Com_Producto ~
F_NomAgencia F_Cuenta F_Chequeo F_Descripcion F_Nombre F_ValCheqCon ~
F_ValCheqret F_Consigna F_Retiro F_ValEfecCon F_ValEfeRet F_Total 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 F_SDisponible1 F_Vlrsobregiro1 F_SCanje1 ~
F_FecCancela1 F_SdoTot F_FecProxLiq1 F_SIntPagados1 F_FecUltLiq1 F_Cuota1 ~
F_Plazo1 F_Intsobregiro1 F_Intporpagar1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-Frame_Cheques 
       MENU-ITEM m_Salir        LABEL "Salir Sin &Procesar".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_autorizados 
     LABEL "Ver Autorizados" 
     SIZE 17 BY .81.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Help" 
     SIZE 5 BY 1.23.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.35.

DEFINE BUTTON Btn_Descripcion 
     IMAGE-UP FILE "imagenes/btn_fwd.bmp":U
     LABEL "Button 59" 
     SIZE 12 BY 1.08.

DEFINE BUTTON Btn_Grabar 
     LABEL "&Grabar" 
     SIZE 10 BY 1.35.

DEFINE BUTTON Btn_ImpSaldo  NO-FOCUS
     LABEL "&Imprimir Saldo" 
     SIZE 17.29 BY .88.

DEFINE BUTTON Btn_ReValid 
     LABEL "Re-Validar" 
     SIZE 14.29 BY 1.12.

DEFINE BUTTON Btn_Reversa 
     LABEL "&Reversar Transacc." 
     SIZE 3.29 BY 1.

DEFINE BUTTON Btn_Salir DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.35
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1  NO-FOCUS
     LABEL "Ver Firmas" 
     SIZE 12.86 BY .88
     FGCOLOR 12 .

DEFINE BUTTON BUTTON-173 
     LABEL "i" 
     SIZE 3 BY .81
     FONT 0.

DEFINE BUTTON BUTTON-58 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 58" 
     SIZE 10 BY 1.35.

DEFINE BUTTON Imp_Cdat 
     LABEL "Impresión CDAT" 
     SIZE 14.43 BY 1.08.

DEFINE VARIABLE CMB_Tipo AS CHARACTER FORMAT "X(256)":U INITIAL "00 - Todos los Productos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 33 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Com_Producto AS CHARACTER FORMAT "X(55)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 43 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_Agencia AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Chequeo AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 5 NO-UNDO.

DEFINE VARIABLE F_Consigna AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_Cuenta AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_Descripcion AS CHARACTER FORMAT "X(80)":U 
     LABEL "Descripción" 
     VIEW-AS FILL-IN 
     SIZE 88 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Nit AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_NomAgencia AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .88
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_Nombre AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Retiro AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21.43 BY .81
     BGCOLOR 5 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_Total AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Pago/Recaudo" 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .81
     BGCOLOR 1 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_ValCheqCon AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_ValCheqret AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21.43 BY .81
     BGCOLOR 5 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_ValEfecCon AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_ValEfeRet AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21.43 BY .81
     BGCOLOR 5 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-139
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 4.58.

DEFINE RECTANGLE RECT-227
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 1.35.

DEFINE BUTTON SalirPDF 
     IMAGE-UP FILE "D:/SFG/Desarrollo/Obj/imagenes/volver.bmp":U
     LABEL "" 
     SIZE 6.72 BY 1.65.

DEFINE VARIABLE F_Cuota1 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Cuota" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_DetalleEstado AS CHARACTER FORMAT "X(30)":U 
     LABEL "Estado" 
     VIEW-AS FILL-IN 
     SIZE 37.29 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_FecCancela1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Vencimiento" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_FecProxLiq1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Prox.Liq." 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_FecUltLiq1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Ult.Liq." 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Intporpagar1 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Int x Pagar" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Intsobregiro1 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Int.Sobregiro" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Plazo1 AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Plazo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_SCanje1 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "En canje" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_SDisponible1 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Disponible" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_SdoTot AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Sdo Total" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_SIntPagados1 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Int Pag" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Vlrsobregiro1 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Sdo.Mínimo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_NomCuenta AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rad-Nat AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Nat. Débito", 1,
"Nat. Crédito", 2,
"Nat. Deb/Cr", 3
     SIZE 12 BY 1.88 NO-UNDO.

DEFINE VARIABLE Rad-TipoCaja AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ingreso", 1,
"Egreso", 2,
"Ingr/Egr", 3
     SIZE 9 BY 1.88 NO-UNDO.

DEFINE VARIABLE T_Caja AS LOGICAL INITIAL no 
     LABEL "Maneja Caja" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .5 NO-UNDO.

DEFINE VARIABLE T_Dto AS LOGICAL INITIAL no 
     LABEL "Maneja Documento" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .77 NO-UNDO.

DEFINE VARIABLE T_ManNit AS LOGICAL INITIAL no 
     LABEL "Maneja Nit" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .5 NO-UNDO.

DEFINE BUTTON BUTTON-59 
     IMAGE-UP FILE "IMAGENES/volver.bmp":U
     LABEL "SALIR" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE F_NitCuenta AS CHARACTER FORMAT "X(12)":U 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_NomNit AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_CtaCC AS INTEGER FORMAT "999":U INITIAL 999 
     LABEL "Centro de Costos" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Beneficiario AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre del Beneficiario" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_NroCheque AS CHARACTER FORMAT "X(10)":U 
     LABEL "Número de Cheque" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-60 
     IMAGE-UP FILE "IMAGENES/volver.bmp":U
     LABEL "Button 60" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE F-Cantidad3 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Cantidad" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-CargoInicial3 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Car.Inicial" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Cuopagadas3 AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Cuotas Pagadas" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Cuota3 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cuota" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-FecCargo3 AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Cargo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Fecultpag3 AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Ult.Pago" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Forpago3 AS CHARACTER FORMAT "X(20)":U 
     LABEL "Pago" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Periodo3 AS CHARACTER FORMAT "X(20)":U 
     LABEL "Período" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Plazo3 AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Plazo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Sdopendiente3 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Sdo.Pdte" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Secuencia3 AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Secuencia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Vlracumpagos3 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Vlr.Pagos" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .69
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_SalPro 
     LABEL "Salir Sin &Procesar" 
     SIZE 17 BY .81.

DEFINE BUTTON BUTTON-174 
     IMAGE-UP FILE "IMAGENES/volver.bmp":U
     LABEL "Button 174" 
     SIZE 8.57 BY 1.58 TOOLTIP "Valida y regresa al Browser principal".

DEFINE VARIABLE F_Sumatoria AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_VrEfectivo AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     LABEL "Consigna en Efectivo" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .81 TOOLTIP "Vr.diferencia con Total consignado"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-230 
     LABEL "Cerrar" 
     SIZE 11.29 BY .81.

DEFINE IMAGE P_Foto
     FILENAME "adeicon/blank":U
     SIZE 18 BY 5.12.

DEFINE BUTTON Btn_ReVal 
     LABEL "Validadora" 
     SIZE 15 BY 1.12 TOOLTIP "Valida la Transacc.seleccionada".

DEFINE BUTTON BUTTON-183 
     IMAGE-UP FILE "IMAGENES/volver.bmp":U
     LABEL "Button 183" 
     SIZE 8.57 BY 1.88 TOOLTIP "Valida y regresa al Browser principal".

DEFINE BUTTON BUTTON-123 
     IMAGE-UP FILE "IMAGENES/volver.bmp":U
     LABEL "Button 123" 
     SIZE 10 BY 1.62 TOOLTIP "Regresa a la Ventana principal".

DEFINE VARIABLE W_NroTx AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Nro.de la Transacciòn" 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "IMAGENES/volver.bmp":U
     LABEL "Button 121" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_CedTrans AS CHARACTER FORMAT "X(12)":U 
     LABEL "Cèdula/Nit" 
     VIEW-AS FILL-IN 
     SIZE 16.29 BY 1 TOOLTIP "Cèdula/Nit  de quien està realizando la Tx."
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTx AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 TOOLTIP "Nombres y Apellidos"
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-10 FOR 
      Cuentas, 
      Operacion SCROLLING.

DEFINE QUERY BROWSE-12 FOR 
      Taquilla SCROLLING.

DEFINE QUERY BROWSE-5 FOR 
      Producto SCROLLING.

DEFINE QUERY BROWSE-7 FOR 
      Cheques SCROLLING.

DEFINE QUERY BROWSE-9 FOR 
      Cuentas, 
      Operacion SCROLLING.

DEFINE QUERY BROWSE-Detalle FOR 
      Detalle SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-10 C-Win _STRUCTURED
  QUERY BROWSE-10 NO-LOCK DISPLAY
      Operacion.Cod_Compensa COLUMN-LABEL "Banco" FORMAT "99":U
            WIDTH 5.43
      Cuentas.Cuenta FORMAT "X(14)":U WIDTH 14
      Cuentas.Nombre FORMAT "X(35)":U WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 5.38
         BGCOLOR 15 FONT 5 TOOLTIP "Doble Clic para número de cheque".

DEFINE BROWSE BROWSE-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-12 C-Win _FREEFORM
  QUERY BROWSE-12 NO-LOCK DISPLAY
      Taquilla.Nit FORMAT "X(12)":U                             COLUMN-LABEL "Ced./Nit"
      Taquilla.Nro_Transaccion FORMAT ">>>>>>9":U               COLUMN-LABEL "No.Transac"
      Taquilla.Val_Cheque FORMAT "->>>>>,>>>,>>>,>>9.99":U      COLUMN-LABEL "Valor-Cheque"
      Taquilla.Val_Efectivo FORMAT "->>>>>,>>>,>>>,>>9.99":U    COLUMN-LABEL "Valor-Efectivo"
      Taquilla.Tip_Producto FORMAT "9":U                        COLUMN-LABEL "TipP"
      Taquilla.Cod_Producto FORMAT "999":U                      COLUMN-LABEL "CodP"
      Taquilla.Cuenta FORMAT "X(14)":U                          COLUMN-LABEL "Cuenta 1"
      Taquilla.Nro_Cuenta FORMAT "X(14)":U                      COLUMN-LABEL "Cuenta 2"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 55.72 BY 12.92
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .65 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-5 C-Win _FREEFORM
  QUERY BROWSE-5 DISPLAY
      Producto.OfiTem COLUMN-LABEL "Age"
        Producto.NomPto FORMAT "X(21)" WIDTH 15
        Producto.Cuenta COLUMN-LABEL "Cta/Pagare" WIDTH 10
        Producto.CtaAnt COLUMN-LABEL "CtaAnterior" WIDTH 10
        Producto.D_Cheq
        Producto.DtoRef COLUMN-LABEL "Docmto.Ref"
        Producto.Debe
        Producto.Haber
        Producto.EC
        Producto.Retiro  FORMAT "S/N"
        " "
        Producto.ConsigEf
        ENABLE Producto.EC Producto.Cuenta Producto.DtoRef Producto.Debe Producto.Haber Producto.Retiro
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 110 BY 8.08
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .85.

DEFINE BROWSE BROWSE-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-7 C-Win _FREEFORM
  QUERY BROWSE-7 DISPLAY
      Cheques.W_Canje
        Cheques.W_Banco       
        Cheques.W_Cheque    
        Cheques.W_Valor
        ENABLE Cheques.W_Banco Cheques.W_Canje Cheques.W_Cheque Cheques.W_Valor
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 46.72 BY 5.5
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .62.

DEFINE BROWSE BROWSE-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-9 C-Win _FREEFORM
  QUERY BROWSE-9 NO-LOCK DISPLAY
      Cuentas.Cuenta     FORMAT "X(14)":U 
      TRIM(Operacion.Nom_Oper)     FORMAT "X(50)":U 
      Cuentas.Naturaleza                      COLUMN-LABEL "Nat" FORMAT "X(2)":U 
      Cuentas.Id_detalle FORMAT "yes/no":U    COLUMN-LABEL "SxD"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 73 BY 9.31
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5.

DEFINE BROWSE BROWSE-Detalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Detalle C-Win _FREEFORM
  QUERY BROWSE-Detalle NO-LOCK DISPLAY
      Detalle.Doc_referencia COLUMN-LABEL "DocReferencia" FORMAT "X(10)":U
            WIDTH 12
      Detalle.Plazo COLUMN-LABEL "Plazo" FORMAT "999":U WIDTH 7
      Detalle.Fec_ProntoPago COLUMN-LABEL "ProntoPago" FORMAT "99/99/9999":U
            WIDTH 10
      Detalle.Valor_amortizacion COLUMN-LABEL "Cuota" FORMAT ">,>>>,>>>,>>9.99":U
            WIDTH 11
      Detalle.Valor_inicial COLUMN-LABEL "Valor Inicial" FORMAT "->>>,>>>,>>>,>>9":U
            WIDTH 10
      Detalle.Fec_Grabacion COLUMN-LABEL "Fecha" FORMAT "99/99/99":U
            WIDTH 10
      Detalle.Cr FORMAT "->>>>,>>>,>>9":U WIDTH 10
      Detalle.Db FORMAT "->>>,>>>,>>>,>>9":U WIDTH 10
      ValFac     FORMAT "->>>>,>>>,>>9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 101 BY 8.35
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_ReVal
     BROWSE-12 AT ROW 1.19 COL 1.57
     BUTTON-183 AT ROW 14.46 COL 46.14
     Btn_ReVal AT ROW 14.92 COL 12.43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 42 ROW 2.35
         SIZE 58 BY 16.46
         BGCOLOR 17 FONT 4
         TITLE "Re-Validación de Transacciones - Seleccione la Transacc.".

DEFINE FRAME F_Trans
     BUTTON-121 AT ROW 1.27 COL 68
     W_CedTrans AT ROW 1.54 COL 8 COLON-ALIGNED
     W_NomTx AT ROW 1.54 COL 25 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14.72 ROW 2.96
         SIZE 81.43 BY 3.12
         BGCOLOR 17 FONT 4
         TITLE "IDENTIFICACIÒN QUIEN TRAMITA LA TRANSACCIÒN".

DEFINE FRAME DEFAULT-FRAME
     Btn_ReValid AT ROW 20.85 COL 51.14
     Imp_Cdat AT ROW 19.31 COL 51
     btn_autorizados AT ROW 21.27 COL 83
     Btn_ImpSaldo AT ROW 20.38 COL 82.72
     BUTTON-1 AT ROW 20.38 COL 69.86
     Btn_Reversa AT ROW 20.38 COL 109
     CMB_Tipo AT ROW 2.08 COL 1 COLON-ALIGNED NO-LABEL
     F_Nit AT ROW 2.08 COL 35 COLON-ALIGNED NO-LABEL
     F_Agencia AT ROW 4.23 COL 3 NO-LABEL
     Com_Producto AT ROW 4.23 COL 35 COLON-ALIGNED NO-LABEL
     F_NomAgencia AT ROW 4.23 COL 5 COLON-ALIGNED NO-LABEL
     F_Cuenta AT ROW 4.23 COL 79 COLON-ALIGNED NO-LABEL
     F_Chequeo AT ROW 4.23 COL 95 COLON-ALIGNED NO-LABEL
     Btn_Descripcion AT ROW 5.58 COL 2
     F_Descripcion AT ROW 5.85 COL 22 COLON-ALIGNED
     F_Nombre AT ROW 2.08 COL 52 COLON-ALIGNED NO-LABEL
     BROWSE-5 AT ROW 6.92 COL 2
     F_ValCheqCon AT ROW 17.15 COL 55.57 COLON-ALIGNED NO-LABEL
     F_ValCheqret AT ROW 17.15 COL 76.57 COLON-ALIGNED NO-LABEL
     F_Consigna AT ROW 18.23 COL 57.57 NO-LABEL
     F_Retiro AT ROW 18.23 COL 78.57 NO-LABEL
     F_ValEfecCon AT ROW 16.08 COL 55.57 COLON-ALIGNED NO-LABEL
     F_ValEfeRet AT ROW 16.08 COL 76.57 COLON-ALIGNED NO-LABEL
     F_Total AT ROW 19.31 COL 83.57 COLON-ALIGNED
     Btn_Grabar AT ROW 15.73 COL 101.14
     Btn_Cancelar AT ROW 17.15 COL 101.29
     Btn_Salir AT ROW 18.5 COL 101.29
     Btn_Ayuda AT ROW 21.19 COL 108
     BUTTON-58 AT ROW 1.5 COL 101.86
     BUTTON-173 AT ROW 2.08 COL 51
     "Agencia" VIEW-AS TEXT
          SIZE 8 BY .77 AT ROW 3.15 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Identificación y Nombre del Cliente" VIEW-AS TEXT
          SIZE 31 BY .81 AT ROW 1 COL 37
          FGCOLOR 7 FONT 5
     "Cuenta/Pagare" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 3.15 COL 81
          FGCOLOR 7 FONT 5
     "Tipo de Producto" VIEW-AS TEXT
          SIZE 16 BY .77 AT ROW 1 COL 3
          FGCOLOR 7 FONT 5
     "Productos Disponibles" VIEW-AS TEXT
          SIZE 20 BY .77 AT ROW 3.15 COL 37
          FGCOLOR 7 FONT 5
     "Cheque" VIEW-AS TEXT
          SIZE 7 BY .77 AT ROW 17.12 COL 49.57
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Efectivo" VIEW-AS TEXT
          SIZE 8 BY .77 AT ROW 16.08 COL 49.57
          FGCOLOR 7 FONT 5
     "Total" VIEW-AS TEXT
          SIZE 5 BY .77 AT ROW 18.15 COL 51.57
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "DC" VIEW-AS TEXT
          SIZE 3 BY .81 AT ROW 3.15 COL 97
          FGCOLOR 7 FONT 5
     RECT-139 AT ROW 15.54 COL 100.29
     RECT-227 AT ROW 3.96 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112 BY 21.42
         BGCOLOR 17 FONT 4.

DEFINE FRAME Frame-Cuentas
     BROWSE-9 AT ROW 1.27 COL 2.72
     W_CtaCC AT ROW 10.69 COL 15.43 COLON-ALIGNED
     BUTTON-59 AT ROW 10.96 COL 63.43
     F_NitCuenta AT ROW 11.77 COL 2.43 COLON-ALIGNED
     F_NomNit AT ROW 11.77 COL 15.43 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 11 ROW 3.69
         SIZE 75.86 BY 12.69
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Cuentas".

DEFINE FRAME Frame-Ahorros
     F_SDisponible1 AT ROW 1.5 COL 8.72 COLON-ALIGNED
     F_Vlrsobregiro1 AT ROW 1.65 COL 33.72 COLON-ALIGNED
     F_SCanje1 AT ROW 2.27 COL 8.72 COLON-ALIGNED
     F_FecCancela1 AT ROW 2.46 COL 33.72 COLON-ALIGNED
     F_SdoTot AT ROW 3 COL 8.57 COLON-ALIGNED WIDGET-ID 2
     F_FecProxLiq1 AT ROW 3.27 COL 33.72 COLON-ALIGNED
     F_SIntPagados1 AT ROW 3.77 COL 8.72 COLON-ALIGNED
     F_FecUltLiq1 AT ROW 4.08 COL 33.72 COLON-ALIGNED
     F_Cuota1 AT ROW 4.54 COL 8.72 COLON-ALIGNED
     F_Plazo1 AT ROW 4.88 COL 33.72 COLON-ALIGNED
     F_Intsobregiro1 AT ROW 5.31 COL 8.72 COLON-ALIGNED
     F_Intporpagar1 AT ROW 6.08 COL 8.72 COLON-ALIGNED
     F_DetalleEstado AT ROW 6.88 COL 8.72 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 15.27
         SIZE 47 BY 6.73
         BGCOLOR 17 FGCOLOR 0 FONT 4.

DEFINE FRAME Frame-Cuentas-Bancos
     BROWSE-10 AT ROW 1.27 COL 2
     F_NroCheque AT ROW 6.92 COL 6
     F_Beneficiario AT ROW 8 COL 3.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23 ROW 6.12
         SIZE 56 BY 8.88
         BGCOLOR 17 FONT 4
         TITLE "Cuentas de Bancos".

DEFINE FRAME Frame_Cheques
     BROWSE-7 AT ROW 1.27 COL 6.57
     W_VrEfectivo AT ROW 6.92 COL 34.86 COLON-ALIGNED
     BUTTON-174 AT ROW 8.92 COL 49.57
     Btn_SalPro AT ROW 9.54 COL 3.43
     F_Sumatoria AT ROW 9.54 COL 21.29 COLON-ALIGNED NO-LABEL
     "         Tipos de Canje: 1 - Local   2 - Al Cobro   3 - Negociable" VIEW-AS TEXT
          SIZE 55 BY .81 AT ROW 7.81 COL 3
          BGCOLOR 18 FGCOLOR 15 FONT 5
     "Tot.Consigna en Cheques" VIEW-AS TEXT
          SIZE 16.86 BY .5 AT ROW 9 COL 23.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 8 ROW 6.65
         SIZE 59 BY 10.5
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Relación de Cheques".

DEFINE FRAME F_Foto
     BUTTON-230 AT ROW 6.12 COL 5 WIDGET-ID 4
     P_Foto AT ROW 1 COL 1 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 92 ROW 1
         SIZE 19 BY 6.73
         TITLE "Foto del Asociado"
         CANCEL-BUTTON BUTTON-230 WIDGET-ID 100.

DEFINE FRAME FRAME-Detalle
     BROWSE-Detalle AT ROW 2.08 COL 3
     BUTTON-60 AT ROW 9.88 COL 93
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.88
         SIZE 104 BY 11.58
         BGCOLOR 17 
         TITLE "Detalle Facturas".

DEFINE FRAME Frame-Especiales
     F-Vlracumpagos3 AT ROW 1.42 COL 7 COLON-ALIGNED
     F-Forpago3 AT ROW 1.42 COL 29 COLON-ALIGNED
     F-Sdopendiente3 AT ROW 2.23 COL 7 COLON-ALIGNED
     F-Periodo3 AT ROW 2.23 COL 29 COLON-ALIGNED
     F-Cuota3 AT ROW 3.04 COL 7 COLON-ALIGNED
     F-CargoInicial3 AT ROW 3.04 COL 29 COLON-ALIGNED
     F-Cantidad3 AT ROW 3.85 COL 14 COLON-ALIGNED
     F-FecCargo3 AT ROW 3.85 COL 32 COLON-ALIGNED
     F-Cuopagadas3 AT ROW 4.65 COL 14 COLON-ALIGNED
     F-Fecultpag3 AT ROW 4.65 COL 32 COLON-ALIGNED
     F-Plazo3 AT ROW 5.46 COL 14 COLON-ALIGNED
     F-Secuencia3 AT ROW 5.46 COL 32 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 16.08
         SIZE 46 BY 5.92
         BGCOLOR 17 FONT 4.

DEFINE FRAME Frame-CarCue
     F_NomCuenta AT ROW 1.27 COL 2 NO-LABEL
     Rad-Nat AT ROW 2.08 COL 3 NO-LABEL
     Rad-TipoCaja AT ROW 2.08 COL 23 NO-LABEL
     T_Dto AT ROW 4.35 COL 3
     T_Caja AT ROW 5.15 COL 3
     T_ManNit AT ROW 5.85 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 16.08
         SIZE 45 BY 5.65
         BGCOLOR 17 FONT 4.

DEFINE FRAME FormatoEfectivo
     SalirPDF AT ROW 16.46 COL 89 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 8 ROW 2.62
         SIZE 97 BY 18.85
         TITLE "Formato transacciones mayores al límite" WIDGET-ID 200.

DEFINE FRAME F_RevTx
     W_NroTx AT ROW 1.96 COL 23.29 COLON-ALIGNED
     BUTTON-123 AT ROW 4.19 COL 27.57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 6.96
         SIZE 41 BY 6.65
         BGCOLOR 17 
         TITLE "Reversar Transacciòn".


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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Taquilla"
         HEIGHT             = 21.42
         WIDTH              = 112
         MAX-HEIGHT         = 26.27
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 26.27
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FormatoEfectivo:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME Frame-Ahorros:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME Frame-CarCue:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME Frame-Cuentas:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME Frame-Cuentas-Bancos:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Detalle:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME Frame-Especiales:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME Frame_Cheques:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME F_Foto:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME Frame-Ahorros:MOVE-AFTER-TAB-ITEM (Btn_Ayuda:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME Frame-Especiales:MOVE-BEFORE-TAB-ITEM (BUTTON-58:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME Frame-Cuentas-Bancos:MOVE-BEFORE-TAB-ITEM (FRAME Frame-Especiales:HANDLE)
       XXTABVALXX = FRAME Frame_Cheques:MOVE-BEFORE-TAB-ITEM (FRAME Frame-Cuentas-Bancos:HANDLE)
       XXTABVALXX = FRAME Frame-Cuentas:MOVE-BEFORE-TAB-ITEM (FRAME Frame_Cheques:HANDLE)
       XXTABVALXX = FRAME Frame-CarCue:MOVE-BEFORE-TAB-ITEM (FRAME Frame-Cuentas:HANDLE)
       XXTABVALXX = FRAME Frame-Ahorros:MOVE-BEFORE-TAB-ITEM (FRAME Frame-CarCue:HANDLE)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BROWSE-5 F_Nombre DEFAULT-FRAME */
/* SETTINGS FOR BUTTON Btn_Reversa IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Reversa:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-173 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Com_Producto IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Agencia IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F_Chequeo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Consigna IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F_Cuenta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Descripcion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Nit IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_NomAgencia IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Nombre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Retiro IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F_Total IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_ValCheqCon IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_ValCheqret IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_ValEfecCon IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_ValEfeRet IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FormatoEfectivo
                                                                        */
ASSIGN 
       FRAME FormatoEfectivo:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME Frame-Ahorros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frame-Ahorros:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F_Cuota1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_DetalleEstado IN FRAME Frame-Ahorros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_FecCancela1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_FecProxLiq1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_FecUltLiq1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_Intporpagar1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_Intsobregiro1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_Plazo1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_SCanje1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_SDisponible1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_SdoTot IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_SIntPagados1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F_Vlrsobregiro1 IN FRAME Frame-Ahorros
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FRAME Frame-CarCue
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frame-CarCue:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F_NomCuenta IN FRAME Frame-CarCue
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR RADIO-SET Rad-Nat IN FRAME Frame-CarCue
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Rad-TipoCaja IN FRAME Frame-CarCue
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T_Caja IN FRAME Frame-CarCue
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T_Dto IN FRAME Frame-CarCue
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T_ManNit IN FRAME Frame-CarCue
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME Frame-Cuentas
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-9 1 Frame-Cuentas */
ASSIGN 
       FRAME Frame-Cuentas:HIDDEN           = TRUE
       FRAME Frame-Cuentas:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN F_NomNit IN FRAME Frame-Cuentas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CtaCC IN FRAME Frame-Cuentas
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME Frame-Cuentas-Bancos
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-10 1 Frame-Cuentas-Bancos */
ASSIGN 
       FRAME Frame-Cuentas-Bancos:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F_Beneficiario IN FRAME Frame-Cuentas-Bancos
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN F_NroCheque IN FRAME Frame-Cuentas-Bancos
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-Detalle
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-Detalle 1 FRAME-Detalle */
ASSIGN 
       FRAME FRAME-Detalle:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME Frame-Especiales
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frame-Especiales:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F-Cantidad3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-CargoInicial3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Cuopagadas3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Cuota3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-FecCargo3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Fecultpag3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Forpago3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Periodo3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Plazo3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Sdopendiente3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Secuencia3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Vlracumpagos3 IN FRAME Frame-Especiales
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME Frame_Cheques
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-7 TEXT-11 Frame_Cheques */
ASSIGN 
       FRAME Frame_Cheques:HIDDEN           = TRUE
       FRAME Frame_Cheques:MOVABLE          = TRUE
       FRAME Frame_Cheques:POPUP-MENU       = MENU POPUP-MENU-Frame_Cheques:HANDLE.

/* SETTINGS FOR FILL-IN F_Sumatoria IN FRAME Frame_Cheques
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrEfectivo IN FRAME Frame_Cheques
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Foto
   NOT-VISIBLE                                                          */
/* SETTINGS FOR FRAME F_ReVal
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-12 1 F_ReVal */
ASSIGN 
       FRAME F_ReVal:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-12:SEPARATOR-FGCOLOR IN FRAME F_ReVal      = 17.

/* SETTINGS FOR FRAME F_RevTx
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_RevTx:HIDDEN           = TRUE
       FRAME F_RevTx:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Trans
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Trans:HIDDEN           = TRUE
       FRAME F_Trans:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-10
/* Query rebuild information for BROWSE BROWSE-10
     _TblList          = "bdcentral.Cuentas,bdcentral.Operacion OF bdcentral.Cuentas"
     _Options          = "NO-LOCK"
     _OrdList          = "bdcentral.Operacion.Cod_Compensa|yes"
     _Where[1]         = "Cuentas.Cod_FlujoEfec = ""D""
 AND Cuentas.Car_Efectivo = 3
 AND Cuentas.Estado = 1
 AND INTEGER(Cuentas.Cta_Homologada) = w_agencia"
     _Where[2]         = "Operacion.Clase_Operacion = 1
 AND Operacion.Ctrl_EfeChe = 2
 AND Operacion.Tipo_Operacion = 2"
     _FldNameList[1]   > bdCentral.Operacion.Cod_Compensa
"Operacion.Cod_Compensa" "Banco" ? "integer" ? ? ? ? ? ? no ? no no "5.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdCentral.Cuentas.Cuenta
"Cuentas.Cuenta" ? ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > bdCentral.Cuentas.Nombre
"Cuentas.Nombre" ? "X(35)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-10 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-12
/* Query rebuild information for BROWSE BROWSE-12
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Taquilla WHERE Taquilla.Nit EQ "00" NO-LOCK
                                            BY  Taquilla.Nit  INDEXED-REPOSITION.*/
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-12 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-5
/* Query rebuild information for BROWSE BROWSE-5
     _START_FREEFORM
OPEN QUERY BROWSE-5 FOR EACH Producto.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-7
/* Query rebuild information for BROWSE BROWSE-7
     _START_FREEFORM
OPEN QUERY BROWSE-7 FOR EACH Cheques.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-7 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-9
/* Query rebuild information for BROWSE BROWSE-9
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cuentas
      WHERE Cuentas.Id_Caja = TRUE
 AND Cuentas.Estado = 1 NO-LOCK,
      EACH Operacion OF Cuentas
      WHERE Operacion.Tipo_Producto = 4
 AND Operacion.Clase_Operacion = 1
 AND Operacion.Ctrl_EfeChe = 3 NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _TblOptList       = ","
     _Where[1]         = "Cuentas.Id_Caja = TRUE
 AND Cuentas.Estado = 1"
     _Where[2]         = "Operacion.Tipo_Producto = 4
 AND Operacion.Clase_Operacion = 1
 AND Operacion.Ctrl_EfeChe = 3"
     _Query            is OPENED
*/  /* BROWSE BROWSE-9 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Detalle
/* Query rebuild information for BROWSE BROWSE-Detalle
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Detalle NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-Detalle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frame-Cuentas
/* Query rebuild information for FRAME Frame-Cuentas
     _Query            is NOT OPENED
*/  /* FRAME Frame-Cuentas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frame-Cuentas-Bancos
/* Query rebuild information for FRAME Frame-Cuentas-Bancos
     _Query            is NOT OPENED
*/  /* FRAME Frame-Cuentas-Bancos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Detalle
/* Query rebuild information for FRAME FRAME-Detalle
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Detalle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_ReVal
/* Query rebuild information for FRAME F_ReVal
     _Query            is NOT OPENED
*/  /* FRAME F_ReVal */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_RevTx
/* Query rebuild information for FRAME F_RevTx
     _Query            is NOT OPENED
*/  /* FRAME F_RevTx */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Trans
/* Query rebuild information for FRAME F_Trans
     _Query            is NOT OPENED
*/  /* FRAME F_Trans */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 83.86
       HEIGHT          = 2.42
       WIDTH           = 10
       WIDGET-ID       = 2
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME FormatoEfectivo:HANDLE
       ROW             = 1.54
       COLUMN          = 4
       HEIGHT          = 16.69
       WIDTH           = 83
       WIDGET-ID       = 2
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {CA8A9780-280D-11CF-A24D-444553540000} type: AcroPDF */
      CtrlFrame-2:MOVE-BEFORE(SalirPDF:HANDLE IN FRAME FormatoEfectivo).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Taquilla */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Taquilla */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Frame-Cuentas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Frame-Cuentas C-Win
ON GO OF FRAME Frame-Cuentas /* Consulta de Cuentas */
DO:
  IF F_NitCuenta:SCREEN-VALUE IN FRAME FRAME-Cuentas EQ "" THEN DO:
     MESSAGE "Digite el Nit" VIEW-AS ALERT-BOX INFORMATION.
     APPLY "entry" TO F_NitCuenta IN FRAME frame-Cuentas.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-10
&Scoped-define FRAME-NAME Frame-Cuentas-Bancos
&Scoped-define SELF-NAME BROWSE-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-10 C-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-10 IN FRAME Frame-Cuentas-Bancos
DO:
    DEFINE VAR numCheque AS INTEGER INITIAL 0.
    DEFINE VAR aux AS INTEGER.
    
    F_NroCheque:SCREEN-VALUE = "".
        
    FOR EACH mov_contable WHERE mov_contable.agencia = w_agencia
                            AND mov_contable.fec_contable >= 03/01/2011
                            AND mov_contable.cuenta = cuentas.cuenta
                            AND mov_contable.fec_contable <= w_fecha NO-LOCK BY mov_contable.num_documento:
        aux = INTEGER(mov_contable.doc_referencia) NO-ERROR.

        IF aux > numCheque THEN
            numCheque = aux.
    END.

    F_NroCheque:SCREEN-VALUE = STRING(numCheque + 1).

    ASSIGN F_NroCheque.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-5
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BROWSE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-5 C-Win
ON F8 OF BROWSE-5 IN FRAME DEFAULT-FRAME
OR F8 OF Producto.Cuenta IN BROWSE BROWSE-5 
OR F8 OF Producto.DtoRef IN BROWSE BROWSE-5 
OR F8 OF Producto.Debe IN BROWSE BROWSE-5 
OR F8 OF Producto.Haber IN BROWSE BROWSE-5 
OR F8 OF Producto.EC IN BROWSE BROWSE-5 
OR F8 OF  Producto.Retiro IN BROWSE BROWSE-5 DO:
   APPLY "ENTRY":U TO Btn_Grabar IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-5 C-Win
ON VALUE-CHANGED OF BROWSE-5 IN FRAME DEFAULT-FRAME
DO:
  RUN MBBrwVc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-9
&Scoped-define FRAME-NAME Frame-Cuentas
&Scoped-define SELF-NAME BROWSE-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-9 C-Win
ON VALUE-CHANGED OF BROWSE-9 IN FRAME Frame-Cuentas
DO:
  W_Cta4 = Cuentas.Cuenta:SCREEN-VALUE IN BROWSE  browse-9.
  APPLY "LEAVE" TO W_CtaCC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Detalle
&Scoped-define FRAME-NAME FRAME-Detalle
&Scoped-define SELF-NAME BROWSE-Detalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Detalle C-Win
ON ROW-DISPLAY OF BROWSE-Detalle IN FRAME FRAME-Detalle
DO:
  IF Cuentas.Naturaleza:SCREEN-VALUE IN BROWSE browse-9 EQ "db" THEN
     ValFac = Detalle.Db - Detalle.Cr.
  ELSE
     ValFac =  Detalle.Cr - Detalle.Db.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btn_autorizados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_autorizados C-Win
ON CHOOSE OF btn_autorizados IN FRAME DEFAULT-FRAME /* Ver Autorizados */
DO:
  IF F_Nit:SCREEN-VALUE EQ "" OR Producto.CodPto EQ ? OR Producto.Cuenta EQ "" THEN
     MESSAGE "No se ha elegido ninguna cuenta o nit de trabajo" SKIP
             VIEW-AS ALERT-BOX INFORMATION.
  ELSE DO:
     ASSIGN C-Win:SENSITIVE = FALSE.
     
     RUN C-Autorizados.r (INPUT F_Nit:SCREEN-VALUE, INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT 7).
     
     ASSIGN C-Win:SENSITIVE = TRUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda C-Win
ON CHOOSE OF Btn_Ayuda IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF {&WINDOW-NAME}
DO:
   SYSTEM-HELP "ayudas/tesoreri" CONTEXT 23.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME DEFAULT-FRAME /* Cancelar */
DO:
  RUN Borrar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON MOUSE-SELECT-CLICK OF Btn_Cancelar IN FRAME DEFAULT-FRAME /* Cancelar */
DO:
  RUN Borrar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Descripcion C-Win
ON CHOOSE OF Btn_Descripcion IN FRAME DEFAULT-FRAME /* Button 59 */
DO:
  IF F_Descripcion:SENSITIVE EQ YES THEN DO:
     W_Ok = Btn_Descripcion:LOAD-IMAGE-UP("imagenes\Btn_Fwd.bmp").
     DISABLE F_Descripcion WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO:
     W_Ok = Btn_Descripcion:LOAD-IMAGE-UP("Imagenes\Btn_Bck.bmp").
     ENABLE F_Descripcion WITH FRAME {&FRAME-NAME}.
     APPLY "entry" TO F_Descripcion.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar C-Win
ON CHOOSE OF Btn_Grabar IN FRAME DEFAULT-FRAME /* Grabar */
DO:
    DEFI VAR T_VrCheque LIKE Producto.Haber.
    DEFI VAR T_Benef LIKE Producto.Benefi.
    DEFI VAR T_Age LIKE Producto.Ofitem.
    DEFI VAR T_For LIKE Producto.Formato.

    flagCtaSucyAg = FALSE.

    IF BROWSE-5:NUM-ITERATIONS EQ 0 THEN DO:
        MESSAGE "No existen transacciones para procesar"
            VIEW-AS ALERT-BOX ERROR.

        APPLY "choose" TO Btn_Cancelar.
        RETURN NO-APPLY.
    END.

    RUN Grabar NO-ERROR.

    /* oakley */

    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Transacción Rechazada..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    FIND FIRST Taquilla WHERE Taquilla.Usuario EQ W_Usuario
                          AND Taquilla.Contabiliza EQ NO NO-LOCK NO-ERROR.
    IF AVAIL(Taquilla) THEN
        MESSAGE "Por favor revise esta Transacción, Quedaron registros no Contabilizados..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN T_VrCheque = 0.

    FOR EACH Producto WHERE Producto.EC NE "" NO-LOCK:
        CASE SUBSTRING(Producto.TipPto,1,1):
            WHEN ""  THEN RUN Imp_Cuentas.
            WHEN "1" THEN DO:
                IF Producto.EC EQ "C" AND Producto.Haber GT 0 THEN
                    ASSIGN T_VrCheque = T_VrCheque + Producto.Haber
                           T_Age = Producto.Ofitem
                           T_Benef = Producto.Benefi
                           T_For = Producto.Formato.
            END.
        END CASE.
    END.

    IF T_VrCheque GT 0 THEN DO:
        FIND FIRST Formatos WHERE Formatos.Agencia EQ T_Age
                              AND Formatos.Cod_Formato EQ T_For NO-LOCK NO-ERROR.
        IF AVAILABLE(Formatos) THEN DO:
            RUN Imp_Cheque(INPUT T_VrCheque,
                           INPUT T_Benef,
                           INPUT W_Ciudad).
        END.
        ELSE
            MESSAGE "No se halló El Formato para Imprimir Cheque...Revise por favor."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    W_SiChGir = FALSE.

    IF W_Rev THEN
        F_Descripcion:SCREEN-VALUE = "Reversa Tx No : " + STRING(W_NroTx).

    RUN Impresion.

    ASSIGN F_Descripcion:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           W_SecuenciaImprimir = 0
           W_CedTrans = " "
           W_NomTx = " "
           W_NroTx = 0
           W_NroTx:SCREEN-VALUE IN FRAME F_RevTx = "0"
           W_Rev = FALSE.

    RUN Borrar.

    CtrLavado = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar C-Win
ON ENTRY OF Btn_Grabar IN FRAME DEFAULT-FRAME /* Grabar */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ImpSaldo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ImpSaldo C-Win
ON CHOOSE OF Btn_ImpSaldo IN FRAME DEFAULT-FRAME /* Imprimir Saldo */
DO:
  DEFINE VAR W_Tipo   AS INTEGER   FORMAT "9".
  DEFINE VAR W_CtaPto AS CHARACTER FORMAT "X(30)".
  
  ASSIGN W_Tipo = INTEGER(SUBSTRING(Producto.TipPto,1,1)).
  IF W_Tipo EQ 1 THEN DO:
     ASSIGN W_CtaPto = STRING(Producto.Ofitem,"999") + "-" + SUBSTRING(Producto.TipPto,3,1) + "-" +
                       STRING(Producto.CodPto,"999") + "-" + TRIM(Producto.Cuenta) + "-" + STRING(Producto.D_Cheq,"9").
     ASSIGN FRAME Frame-Ahorros F_FecCancela1 F_FecUltLiq1 F_Intporpagar1 F_SCanje1 F_SDisponible1.
     RUN Saldo(INPUT 1,INPUT TRIM(W_Nom_Agencia),INPUT W_CtaPto,INPUT Producto.NomPto,
               INPUT F_SDisponible1,INPUT F_SCanje1,INPUT F_Intporpagar1,INPUT 0,
               INPUT F_FecUltLiq1,INPUT F_FecCancela1) NO-ERROR.
  END.
  IF W_Tipo EQ 4 THEN DO:
     ASSIGN W_CtaPto = STRING(Producto.Ofitem,"999") + "-" + STRING(Producto.CodPto,"999") + "-" + 
                       TRIM(Producto.NitCta) + "-" + STRING(Producto.D_Cheq,"9").
     ASSIGN FRAME Frame-Especiales F-Vlracumpagos3 F-Sdopendiente3 F-Cuopagadas3 F-Fecultpag3 F-FecCargo3.
     RUN Saldo(INPUT 4,INPUT TRIM(W_Nom_Agencia),INPUT W_CtaPto,INPUT Producto.NomPto,
               INPUT F-Vlracumpagos3,INPUT F-Sdopendiente3,INPUT F-Cuopagadas3,INPUT 0,
               INPUT F-Fecultpag3,INPUT F-FecCargo3) NO-ERROR.
  END.
  APPLY "ENTRY":U TO Producto.Cuenta IN BROWSE BROWSE-5.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ReVal
&Scoped-define SELF-NAME Btn_ReVal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ReVal C-Win
ON CHOOSE OF Btn_ReVal IN FRAME F_ReVal /* Validadora */
DO:
  DEFI VAR NroTx   LIKE Taquilla.Nro_Transaccion.
  
  IF AVAIL(Taquilla) THEN DO:
     ASSIGN NroCpte  = 0
            Si_Reval = TRUE.
     FIND FIRST Mov_contable WHERE Mov_contable.Fec_Cont      EQ W_Fecha 
                               AND Mov_contable.Cuenta        EQ Taquilla.Cta_Contra
                               AND INTEG(Mov_contable.Enlace) EQ Taquilla.Nro_Transaccion NO-LOCK NO-ERROR.
     IF AVAIL(Mov_contable) THEN
        ASSIGN NroCpte = Mov_contable.Num_Docum.

     IF Taquilla.Tip_Producto EQ 2 THEN DO:
        RUN Imp_REvalidCreditos (INPUT Taquilla.Nro_Transaccion, "ReValidac-AboCréditos").
        Si_Reval   = FALSE.
        RETURN.
     END.

     ASSIGN NroTx      = Taquilla.Nro_Transaccion
            W_SiAfilia = FALSE.

     IF Taquilla.Naturaleza EQ "Db" THEN DO:
         RUN Imp_Validadora (INPUT Taquilla.Nro_Transaccion, INPUT "ReValidación").  
         Si_Reval   = FALSE.
         RETURN.
     END.

     FIND FIRST Taquilla WHERE Taquilla.Nro_Transaccion EQ NroTx  
                           AND Taquilla.Naturaleza      EQ "Db" NO-LOCK NO-ERROR.
     IF NOT AVAIL(Taquilla) THEN DO:  /*<-------Solo Consignaciones de Ahorros*/
        FIND FIRST Taquilla WHERE Taquilla.Nro_Transaccion EQ NroTx
                              AND  (Taquilla.Cod_operac EQ 040101013 OR
                                    Taquilla.Cod_operac EQ 040101051) NO-LOCK NO-ERROR.
        IF AVAIL(Taquilla) THEN      /*<---Solo Cuota Admón y Consignaciones de Ahorros*/
           W_SiAfilia = TRUE.

        RUN Imp_ValidConsigAho(INPUT NroTx, INPUT "ReValidación").    
        ASSIGN W_SiAfilia = FALSE
               Si_Reval   = FALSE.
        RETURN.
     END.
     
     /*Si hay retiros o son otro pdcto*/
     RUN Imp_Validadora (INPUT NroTx, INPUT "ReValidación").
     ASSIGN W_SiAfilia = FALSE
            Si_Reval   = FALSE.
  END.
  ELSE
     MESSAGE "No hay Tx Seleccionada."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Btn_ReValid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ReValid C-Win
ON CHOOSE OF Btn_ReValid IN FRAME DEFAULT-FRAME /* Re-Validar */
DO:
  ASSIGN FRAME Default-Frame:SENSITIVE = FALSE
         FRAME F_ReVal:VISIBLE         = TRUE.

  OPEN QUERY BROWSE-12 FOR EACH Taquilla WHERE Fec_Transac       EQ W_Fecha
                                            AND Taquilla.Usuario EQ W_Usuario NO-LOCK
                                             BY Hora DESCEND INDEXED-REPOSITION.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Reversa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Reversa C-Win
ON CHOOSE OF Btn_Reversa IN FRAME DEFAULT-FRAME /* Reversar Transacc. */
DO:
  RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
  IF NOT W_Error THEN 
     RETURN.
  
  FOR EACH Producto: DELETE Producto. END.
  CLOSE QUERY BROWSE-5.
  
  ASSIGN FRAME Default-Frame:SENSITIVE = FALSE                                   
         FRAME F_RevTx:SENSITIVE       = TRUE                                    
         FRAME F_RevTx:VISIBLE         = TRUE
         W_NroTx                       = 0
         W_NroTx:SCREEN-VALUE          = "0"
         W_Rev                         = TRUE.
         
  APPLY "ENTRY" TO W_NroTX.
  RETURN NO-APPLY.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir C-Win
ON CHOOSE OF Btn_Salir IN FRAME DEFAULT-FRAME /* Salir */
DO:
  &IF DEFINED (adm-panel) NE 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Cheques
&Scoped-define SELF-NAME Btn_SalPro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalPro C-Win
ON CHOOSE OF Btn_SalPro IN FRAME Frame_Cheques /* Salir Sin Procesar */
DO:
  HIDE FRAME Frame_Cheques.
  FOR EACH Tmp_Cheques WHERE Tmp_Cheques.W_Cuenta EQ Producto.Cuenta
                         AND Tmp_Cheques.W_CodPto EQ Producto.CodPto EXCLUSIVE-LOCK:
      DELETE Tmp_Cheques.
  END.
  RELEASE Tmp_Cheques.
  RUN Activar.
  ASSIGN Producto.EC:SCREEN-VALUE IN BROWSE BROWSE-5 = "".
  APPLY "ENTRY":U TO Producto.EC IN BROWSE BROWSE-5.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Ver Firmas */
DO:
  IF F_Nit:SCREEN-VALUE EQ "" OR Producto.CodPto EQ ? OR Producto.Cuenta EQ "" THEN
     MESSAGE "No se ha elegido ninguna cuenta o nit de trabajo" SKIP
             VIEW-AS ALERT-BOX INFORMATION.
  ELSE DO:
     ASSIGN C-Win:SENSITIVE = FALSE.
     
     RUN C-Firma.r (INPUT F_Nit:SCREEN-VALUE, INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT 7).
     
     ASSIGN C-Win:SENSITIVE = TRUE.
     APPLY "entry" TO BUTTON-1 IN FRAME DEFAULT-FRAME.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Trans
&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 C-Win
ON CHOOSE OF BUTTON-121 IN FRAME F_Trans /* Button 121 */
DO:
  IF W_CedTrans LE "0" OR W_NomTx LE " " THEN DO:
     APPLY "ENTRY" TO W_CedTrans.
     RETURN NO-APPLY.
  END.

  ASSIGN FRAME F_Trans:VISIBLE         = FALSE
         FRAME Default-Frame:SENSITIVE = TRUE.
         
  APPLY "Entry" TO Producto.EC IN BROWSE BROWSE-5.
  RETURN NO-APPLY.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 C-Win
ON LEAVE OF BUTTON-121 IN FRAME F_Trans /* Button 121 */
DO:
  APPLY "CHOOSE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_RevTx
&Scoped-define SELF-NAME BUTTON-123
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-123 C-Win
ON CHOOSE OF BUTTON-123 IN FRAME F_RevTx /* Button 123 */
DO:
  ASSIGN FRAME F_RevTx:VISIBLE         = FALSE
         FRAME Default-Frame:SENSITIVE = TRUE
         BROWSE-5:SENSITIVE            = FALSE. 
         
  IF NOT AVAIL(Producto) THEN
     ASSIGN BROWSE-5:SENSITIVE = TRUE
            W_Rev              = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-173
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-173 C-Win
ON CHOOSE OF BUTTON-173 IN FRAME DEFAULT-FRAME /* i */
DO:
  DEFINE VARIABLE W_nitw LIKE clientes.nit.

  APPLY "ENTRY" TO Cmb_Tipo.

  RUN p-Buscanit.r (OUTPUT W_NitW).

  IF W_NitW NE " " THEN DO:
     F_Nit:SCREEN-VALUE = W_nitW.

     APPLY "ENTRY" TO Cmb_Tipo.
          
     F_Nit:SCREEN-VALUE = W_nitW.

     APPLY "ENTRY" TO F_Nit.
  END.
  ELSE
     APPLY "ENTRY" TO Cmb_Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Cheques
&Scoped-define SELF-NAME BUTTON-174
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-174 C-Win
ON CHOOSE OF BUTTON-174 IN FRAME Frame_Cheques /* Button 174 */
DO:
  RUN CONTROL_Cheques.
  RUN Actualizar_Valor.
  IF Producto.Debe - (ValCheq + W_VrEfectivo) EQ 0 THEN DO:
     HIDE FRAME Frame_Cheques.
     RUN Activar.     
  END.
  ELSE 
     MESSAGE "Revise los valores con el Total a Consignar..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ReVal
&Scoped-define SELF-NAME BUTTON-183
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-183 C-Win
ON CHOOSE OF BUTTON-183 IN FRAME F_ReVal /* Button 183 */
DO:
   CLOSE QUERY BROWSE-12.

   ASSIGN FRAME F_ReVal:VISIBLE         = FALSE
          FRAME Default-Frame:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Foto
&Scoped-define SELF-NAME BUTTON-230
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-230 C-Win
ON CHOOSE OF BUTTON-230 IN FRAME F_Foto /* Cerrar */
DO:
  HIDE FRAME F_Foto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-58
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-58 C-Win
ON CHOOSE OF BUTTON-58 IN FRAME DEFAULT-FRAME /* Button 58 */
DO:
   RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame-Cuentas
&Scoped-define SELF-NAME BUTTON-59
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-59 C-Win
ON CHOOSE OF BUTTON-59 IN FRAME Frame-Cuentas /* SALIR */
DO:
  HIDE FRAME Frame-Cuentas.
  RUN Activar.
  APPLY "choose" TO Btn_Cancelar IN FRAME DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Detalle
&Scoped-define SELF-NAME BUTTON-60
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-60 C-Win
ON CHOOSE OF BUTTON-60 IN FRAME FRAME-Detalle /* Button 60 */
DO:


  HIDE FRAME frame-Detalle.
  RUN Activar.
  Producto.Cuenta:SCREEN-VALUE IN BROWSE BROWSE-5 = W_Cta4.
  IF Tmp_Cuentas.Id_Doc EQ TRUE THEN DO:
      Producto.DtoRef:SCREEN-VALUE IN BROWSE Browse-5 = Detalle.Doc_Referencia.
      APPLY "ENTRY" TO Producto.DtoRef IN BROWSE BROWSE-5.
      HIDE FRAME Frame-Cuentas.
      RETURN NO-APPLY.
  END.
  ELSE DO:
      APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
      HIDE FRAME Frame-Cuentas.
      RETURN NO-APPLY.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME CMB_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_Tipo C-Win
ON ENTRY OF CMB_Tipo IN FRAME DEFAULT-FRAME
DO:
    RUN Borrar.
  DO WITH FRAME {&FRAME-NAME}:
     ON RETURN TAB.
     IF NUM-RESULTS("BROWSE-5") GT 0 THEN DO:
        ASSIGN Cmb_Tipo:SCREEN-VALUE       = "00 - Todos los Productos"
               Com_Producto:LIST-ITEMS   = ""
               F_Cuenta:SCREEN-VALUE     = ""
               F_Nit:SCREEN-VALUE        = ""
               F_Nombre:SCREEN-VALUE     = "".
        FOR EACH Producto:
            DELETE Producto.
        END.
        FOR EACH Cheques:
            DELETE Cheques.
        END.
        OPEN QUERY BROWSE-5 FOR EACH Producto.
        OPEN QUERY BROWSE-7 FOR EACH Cheques.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_Tipo C-Win
ON LEAVE OF CMB_Tipo IN FRAME DEFAULT-FRAME
DO:
    ASSIGN F_Tipo = INTEGER(SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,2)).
    
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_Tipo C-Win
ON TAB OF CMB_Tipo IN FRAME DEFAULT-FRAME
OR RETURN OF F_Nit DO:
DO WITH FRAME {&FRAME-NAME}:
  F_Tipo = INTEGER(SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,2)).

  IF F_Tipo NE 0 THEN 
     ASSIGN Cmb_Tipo:SCREEN-VALUE     = "00 - Todos los Productos"
            Com_Producto:LIST-ITEMS   = ""
            F_Cuenta:SCREEN-VALUE     = ""
            F_Nit:SCREEN-VALUE        = ""
            F_Nombre:SCREEN-VALUE     = ""
            F_Tipo = INTEGER(SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,2)).

  IF F_Tipo EQ 2 THEN
     RETURN NO-APPLY.

  IF F_Tipo EQ 0 THEN DO:
         ASSIGN F_Cuenta:SCREEN-VALUE   = ""
                Com_Producto:LIST-ITEMS = "".
         DISABLE F_Cuenta Com_Producto F_Chequeo. 
         ENABLE  F_Nit.
         APPLY "ENTRY" TO F_Nit.
         RETURN NO-APPLY.
   END.
   ELSE DO:
      ENABLE F_Agencia.
      APPLY "ENTRY" TO F_Agencia.
      RETURN NO-APPLY.
   END.
END.
  /*RUN MBTabFTipo.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_Tipo C-Win
ON VALUE-CHANGED OF CMB_Tipo IN FRAME DEFAULT-FRAME
DO:
  ASSIGN F_Tipo = INTEGER(SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,2)).

  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Com_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Com_Producto C-Win
ON ENTRY OF Com_Producto IN FRAME DEFAULT-FRAME
DO:
  IF SELF:SCREEN-VALUE EQ "" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Com_Producto C-Win
ON LEAVE OF Com_Producto IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Com_Producto Cmb_Tipo.
     F_Tipo = INTEGER(SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,2)).
     IF Com_Producto EQ "" AND
        F_Tipo       GE 0  AND
        F_Tipo       LE 2  THEN DO:
        APPLY "choose" TO Btn_Cancelar.
        ENABLE  F_Nit.
        DISABLE F_Cuenta Com_Producto F_Chequeo.
        APPLY "ENTRY" TO F_Nit.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Com_Producto C-Win
ON VALUE-CHANGED OF Com_Producto IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Cmb_Tipo
            W_CodPto = INTEGER(SUBSTRING(Com_Producto:SCREEN-VALUE,42,3))
            F_Tipo = INTEGER(SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,2)).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     Mostrar mensajes al cajero cuando el efectivo que tiene es mayor al permitido
  Parameters:  None required for OCX.
  Notes:       Creado giocam Oct 11/07 - 
------------------------------------------------------------------------------*/
    ASSIGN vdeEfectivo = 0
        vCon = 0
        vRet = 0
        vcmensaje = "El valor en caja supera lo máximo permitido. " + 
                    "Actualmente tiene en efectivo ".
    FIND FIRST Cajero WHERE Cajero.Usuario EQ W_Usuario AND Cajero.Fecha EQ TODAY  NO-LOCK NO-ERROR.
    IF AVAILABLE Cajero THEN DO:
       FOR EACH Mov_Contable WHERE
                Mov_Contable.Agencia EQ W_Agencia AND
                Mov_Contable.Fec_Contable EQ TODAY AND
                Mov_Contable.Usuario EQ Cajero.Usuario AND 
               (mov_contable.cuenta GE '11050501' AND mov_contable.cuenta LE "110510") AND
               (Mov_Contable.Db GT 0 OR Mov_Contable.Cr GT 0)
                NO-LOCK
           BY Mov_Contable.Agencia
           BY Mov_Contable.Fec_Contable
           BY Mov_Contable.Usuario:

           ASSIGN vCon = vCon + Mov_Contable.Db
               vRet = vRet + Mov_Contable.Cr.
       END.
       ASSIGN vdeEfectivo = Cajero.SalIni + vCon - vRet.
       FIND FIRST Usuarios WHERE Usuarios.Usuario = W_usuario NO-LOCK NO-ERROR.
       IF AVAILABLE Usuarios THEN DO:
           IF vdeEfectivo > Usuarios.MaxVal_Efectivo THEN DO:
               ASSIGN vcmensaje = vcmensaje + TRIM(STRING(vdeEfectivo,"$->>,>>>,>>9.99")) + ".".
               MESSAGE vcmensaje
                   VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Monto Max. Superado".
                   FIND Current Usuarios.
                   CREATE Logs.
                   UPDATE Logs.agencia     = Usuarios.Agencia
                       Logs.estado      = TRUE
                       Logs.Fecha       = TODAY
                       Logs.HoraE       = TIME
                       Logs.Observacion = vcMensaje + " Usu: " + W_Usuario + " - Age: " + TRIM(STRING(W_Agencia)) + " - " + usuario.nombre
                       Logs.Usuario     = "454" /*giocam - el codigo es temporal, mientras se define funcionalidad por parte de Auditoría*/
                       Logs.IdRegistro  = 1 NO-ERROR.
                   ASSIGN vcmensaje = "".
           END. /*IF vdeEfectivo > Usuarios.MaxVal_Efectivo */
       END. /*IF AVAILABLE Usuarios */
       ELSE
           MESSAGE "No se encuentra Usuario"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
       MESSAGE "No se ha entrado la base del día." SKIP
               "comuniquese con el administrador" VIEW-AS ALERT-BOX INFORMATION.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Agencia C-Win
ON LEAVE OF F_Agencia IN FRAME DEFAULT-FRAME
DO:
 DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_Agencia.
     IF F_Agencia EQ 0 THEN DO:
        ASSIGN T_OfiIni = 0
               T_OfiFin = 999.
     END.
     ELSE DO:
        FIND Agencias WHERE Agencias.Agencia EQ F_Agencia 
                      AND   Agencias.Estado  EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE (Agencias) THEN DO:
           ASSIGN T_OfiIni = F_Agencia
                  T_OfiFin = F_Agencia
                  F_NomAgencia:SCREEN-VALUE = Agencias.Nombre.
           RUN MBTabFTipo.
           APPLY "entry" TO Com_Producto IN FRAME {&FRAME-NAME}.
           RETURN NO-APPLY. /*Cmb_Tipo.*/
        END.
        ELSE DO:
           MESSAGE "La Agencia Debe Estar en Estado Normal. Verifique...".
           ASSIGN F_Agencia:SCREEN-VALUE = STRING(W_Agencia).
           APPLY "ENTRY" TO F_Agencia.
           RETURN NO-APPLY.
        END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame-Cuentas-Bancos
&Scoped-define SELF-NAME F_Beneficiario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Beneficiario C-Win
ON ENTRY OF F_Beneficiario IN FRAME Frame-Cuentas-Bancos /* Nombre del Beneficiario */
DO:
  DO WITH FRAME Frame_NroCheque.
     ASSIGN F_NroCheque.
     IF F_NroCheque EQ "" THEN DO:
        APPLY "ENTRY":U TO F_NroCheque.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Beneficiario C-Win
ON LEAVE OF F_Beneficiario IN FRAME Frame-Cuentas-Bancos /* Nombre del Beneficiario */
DO:
  DO WITH FRAME Frame-Cuentas-Bancos.
     ASSIGN F_Beneficiario = CAPS(F_Beneficiario:SCREEN-VALUE)
            W_SiChGir      = FALSE.

     IF F_Beneficiario EQ "" THEN DO:
        MESSAGE "El Beneficiario No Debe Ser Campos en Blanco."
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
        TITLE "Validación del Cheque.".
        APPLY "ENTRY":U TO F_NroCheque.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        IF Producto.CueBco LE " " THEN DO:
           MESSAGE "Debe seleccionar Banco..."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "ENTRY":U TO F_NroCheque.
           RETURN NO-APPLY.
        END.

        ASSIGN Producto.Cheque = F_NroCheque:SCREEN-VALUE
               F_NroCheque
               Producto.Benefi = F_Beneficiario:SCREEN-VALUE
               W_NroBcoIgual   = Producto.CueBco
               W_SiChGir       = TRUE.

        HIDE FRAME Frame-Cuentas-Bancos.

        RUN Activar.

        ASSIGN Producto.EC:SCREEN-VALUE IN BROWSE BROWSE-5 = "C"
               Producto.EC.
        ASSIGN BROWSE BROWSE-5 Producto.EC.
        RUN Actualizar_Valor.
        APPLY "Leave" TO Producto.Retiro IN BROWSE BROWSE-5.
        /*RETURN NO-APPLY.*/
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME F_Chequeo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Chequeo C-Win
ON ENTRY OF F_Chequeo IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_Cuenta.
     IF F_Cuenta EQ "" THEN DO:
        APPLY "ENTRY":U TO F_Cuenta.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Chequeo C-Win
ON LEAVE OF F_Chequeo IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE SW AS LOGICAL INITIAL FALSE.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN Cmb_Tipo F_Agencia F_Cuenta Cont = 0.
    F_Tipo = INTEGER(SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,2)).
    DISABLE Com_Producto F_Chequeo F_Cuenta.
    CASE F_Tipo:
      WHEN 1 OR WHEN 4 THEN DO:
        W_CodPto = INTEGER(SUBSTRING(Com_Producto:SCREEN-VALUE,1,3)).
        FOR FIRST Pro_Ahorros WHERE pro_ahorros.cod_ahorro EQ W_CodPto 
                              AND   Pro_Ahorros.Estado       EQ 1 NO-LOCK,
            FIRST Ahorros     WHERE ahorros.cod_ahorro     EQ pro_ahorros.cod_ahorro
                              AND   Ahorros.Cue_Ahorros      EQ F_Cuenta NO-LOCK:
            IF Ahorros.Detalle_Estado GT 02 THEN DO:
               /* Comentariado Mayo 26/05 GAER, Validación solo por Retiros
               FIND Varios WHERE 
                    Varios.Tipo EQ 21 AND Varios.Codigo EQ Ahorros.Detalle_Estado NO-LOCK NO-ERROR.
               IF AVAILABLE Varios THEN DO:
                  MESSAGE "No se pueden hacer transacciones para esta cuenta" SKIP
                          "El estado de esta cuenta es de bloqueo por: " Varios.Descripcion 
                          VIEW-AS ALERT-BOX WARNING.
                  APPLY "choose" TO Btn_Cancelar IN FRAME {&FRAME-NAME}.
                  RETURN NO-APPLY.
               END.*/
            END.
              IF (Ahorros.Agencia EQ W_Agencia) OR ((Ahorros.Agencia NE W_Agencia) AND (Pro_Ahorros.Id_Linea EQ TRUE)  ) THEN DO: /* jjmp 13 marzo 2007 */
               Cont = Cont + 1.
               /* Se incluye aviso en taquilla */
               FOR EACH atrasos where Atrasos.nit = Ahorros.nit:
                   if atrasos.vlr_capmor > 0 then
                      MESSAGE "AVISO PELIGROSO : Asociado Atrasado ".
               END.
                /*arreglar cambio de garantias*/
/*               FOR EACH Garantias WHERE Garantias.Nit_Codeudor = Ahorros.Nit:
                 FIND FIRST Atrasos WHERE Atrasos.Pagare     EQ Garantias.Pagare AND
                                          Atrasos.Vlr_CapMor GT 0                AND
                                          Atrasos.Estado     EQ 1 NO-LOCK NO-ERROR.
                 IF AVAILABLE(Atrasos) THEN
                    MESSAGE "Codeudor del Moroso : " Atrasos.Nit " !!! CUIDADO !!!"
                            VIEW-AS ALERT-BOX.
               END.*/
               CREATE Producto.
               ASSIGN Producto.W_Order = Cont
                      Producto.OfiTem  = Ahorros.Agencia  
                      Producto.TipPto  = "1-" + STRING(pro_ahorros.tip_ahorro,"9")
                      Producto.CodPto  = ahorros.cod_ahorro
                      Producto.Nit     = Ahorros.Nit
                      Producto.NomPto  = Pro_Ahorros.Nom_Producto
                      Producto.Id_Tal  = Pro_Ahorros.Id_Talonario
                      Producto.Cuenta  = Ahorros.Cue_Ahorros
                      Producto.CtaAnt  = Ahorros.Num_Formato
                      Producto.EstCta  = Ahorros.Detalle_Estado.

               FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
               IF AVAILABLE (Clientes) THEN DO:
                  ASSIGN F_Nombre:SCREEN-VALUE  = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)
                         F_Nit:SCREEN-VALUE     = Clientes.Nit
                         F_Seg = Clientes.Cod_Segmento.
               END.
               ASSIGN SW = TRUE.
            END.
        END.
      END.
     END CASE.
     OPEN QUERY BROWSE-5 FOR EACH Producto.
     IF NUM-RESULTS("BROWSE-5") EQ 0  AND F_Agencia NE W_Agencia AND SW EQ FALSE 
        AND F_Tipo GE 1 AND F_Tipo LE 4 THEN DO:
        ASSIGN W_SYA = TRUE.
        DISABLE F_Cuenta Com_Producto F_Chequeo. 
        ENABLE  F_Nit.
        APPLY "ENTRY" TO F_Nit.
        RETURN NO-APPLY.
     END.
     ELSE DO:
       IF NUM-RESULTS("BROWSE-5") EQ 0 THEN DO:
          MESSAGE "La Cuenta No Pertenece a la Agencia. Verifique..."
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
          TITLE "Error En Taquilla".
          ASSIGN F_Nit:SCREEN-VALUE    = "" 
                 F_Nombre:SCREEN-VALUE = "".
          APPLY "ENTRY" TO F_Agencia.
          RETURN NO-APPLY.
       END.
       ELSE DO:
          APPLY "ENTRY" TO Producto.DtoRef IN BROWSE BROWSE-5.
          RETURN NO-APPLY.
       END.
     END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Nit C-Win
ON LEAVE OF F_Nit IN FRAME DEFAULT-FRAME
OR RETURN OF F_Nit OR TAB OF F_Nit DO:

    /*IF F_Nit:SCREEN-VALUE = "10249571" THEN DO:
        MESSAGE "Para este número de identificación no se permite" SKIP
                "ningún tipo de operación. Por favor, consulte con" SKIP
                "el Administrador del Sistema."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        F_Nit:SCREEN-VALUE = ''.
        F_Nombre:SCREEN-VALUE = ''.

        LEAVE.
    END.*/

    
    RUN VerFoto.

    ASSIGN F_Nit
          W_CedTrans = " "
          W_NomTx    = " "
          W_SiChGir = FALSE.
          
   RUN MBTabNit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Nit C-Win
ON MOUSE-SELECT-DBLCLICK OF F_Nit IN FRAME DEFAULT-FRAME
DO:
  DEFI VAR W_NomCte AS CHAR FORM "X(35)".  
  DEFI VAR W_Age    LIKE Agencias.Agencia.
        
  ASSIGN C-Win:SENSITIVE = FALSE.                                                           
                                                                                                    
  RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                    OUTPUT F_Nit, OUTPUT W_NomCte, OUTPUT W_NomCte, OUTPUT W_Age).       
                                                                                                    
  ASSIGN F_Nit:SCREEN-VALUE = F_Nit
         C-Win:SENSITIVE    = TRUE.

  C-Win:MOVE-TO-TOP().

  APPLY "ENTRY" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame-Cuentas
&Scoped-define SELF-NAME F_NitCuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_NitCuenta C-Win
ON LEAVE OF F_NitCuenta IN FRAME Frame-Cuentas /* Nit */
OR TAB OF F_NitCuenta DO:
    DO WITH FRAME Frame-Cuentas:
        IF F_NitCuenta:SCREEN-VALUE EQ "" AND Cuentas.Id_Nit THEN DO:
            APPLY "value-changed" TO BROWSE browse-9.
            APPLY "entry" TO F_NitCuenta IN FRAME Frame-cuentas.
            RETURN NO-APPLY.
        END.

        FIND FIRST Clientes WHERE Clientes.Nit EQ F_NitCuenta:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAIL(Clientes) AND Cuentas.Id_Nit THEN DO:
            MESSAGE "El Nit No Esta Matriculado en Clientes."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Error en Taquilla".

            APPLY "ENTRY" TO F_NitCuenta.
            RETURN NO-APPLY.
        END.

        IF AVAILABLE(Clientes) THEN DO:
            ASSIGN Producto.NitCta = F_NitCuenta:SCREEN-VALUE
                   F_NomNit:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                   F_nombre:SCREEN-VALUE IN FRAME default-frame = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                   X_Nit = Clientes.Nit
                   F_Seg = Clientes.Cod_Segmento.

            IF Cuentas.Id_Detalle:SCREEN-VALUE IN BROWSE browse-9 EQ "yes" THEN DO:
                OPEN QUERY Browse-Detalle FOR EACH Detalle WHERE Detalle.Nit EQ F_NitCuenta:SCREEN-VALUE
                                                             AND Detalle.Cuenta EQ Cuentas.Cuenta:SCREEN-VALUE IN BROWSE browse-9
                                                             AND Detalle.Cr NE Detalle.Db
                                                             /*AND (Detalle.fec_contable) LE W_fecha*/ NO-LOCK INDEXED-REPOSITION.
                VIEW FRAME frame-Detalle.
            END.
        END.
        
        Producto.Cuenta:SCREEN-VALUE IN BROWSE BROWSE-5 = W_Cta4.

        IF Operacion.Cod_Operac EQ 040101051 THEN
            ASSIGN Producto.Ofitem:SCREEN-VALUE IN BROWSE BROWSE-5 = "011"
                   Producto.OperaAux = 040101051.

        RUN Activar.

        IF Tmp_Cuentas.Id_Doc EQ TRUE THEN DO:
            APPLY "ENTRY" TO Producto.DtoRef IN BROWSE BROWSE-5.
            HIDE FRAME Frame-Cuentas.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
            HIDE FRAME Frame-Cuentas.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame-Cuentas-Bancos
&Scoped-define SELF-NAME F_NroCheque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_NroCheque C-Win
ON TAB OF F_NroCheque IN FRAME Frame-Cuentas-Bancos /* Número de Cheque */
DO:
  APPLY "entry" TO F_Beneficiario IN FRAME Frame-Cuentas-Bancos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Imp_Cdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Imp_Cdat C-Win
ON CHOOSE OF Imp_Cdat IN FRAME DEFAULT-FRAME /* Impresión CDAT */
DO:
   FIND FIRST Ahorros WHERE Ahorros.Nit         EQ Producto.NitCta
                     AND   ahorros.cod_ahorro   EQ Producto.CodPto
                     AND   Ahorros.Cue_Ahorros  EQ Producto.Cuenta
                     AND   Ahorros.Agencia      EQ Producto.Ofitem
                     AND Ahorros.Sdo_dispon + Sdo_Canje GT 0 NO-LOCK NO-ERROR.
   IF AVAILABLE(Ahorros) THEN DO:
      IF Ahorros.Tip_Ahorro EQ 3 AND Ahorros.Fec_Apertura EQ W_Fecha THEN DO:
         MESSAGE "Desea Imprimir el Título CDAT" VIEW-AS ALERT-BOX 
             QUESTION BUTTONS YES-NO UPDATE Ask AS LOGICAL.
         IF Ask THEN 
            RUN F-Atermino.R (INPUT Ahorros.Nit, INPUT Ahorros.Cue_Ahorros).
     END.
   END.
   ELSE
     MESSAGE "Solo luego de Grabar la Transacción, debe seleccionar nuevamente el Título," SKIP
             "                                     Para poder Imprimirlo."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Salir C-Win
ON CHOOSE OF MENU-ITEM m_Salir /* Salir Sin Procesar */
DO:
  HIDE FRAME Frame_Cheques.
  FOR EACH Tmp_Cheques WHERE Tmp_Cheques.W_Cuenta EQ Producto.Cuenta
                       AND   Tmp_Cheques.W_CodPto EQ Producto.CodPto EXCLUSIVE-LOCK:
       DELETE Tmp_Cheques.
   END.
  RELEASE Tmp_Cheques.
  RUN Activar.
  ASSIGN Producto.EC:SCREEN-VALUE IN BROWSE BROWSE-5 = "".
  APPLY "ENTRY" TO Producto.EC IN BROWSE BROWSE-5.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FormatoEfectivo
&Scoped-define SELF-NAME SalirPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SalirPDF C-Win
ON CHOOSE OF SalirPDF IN FRAME FormatoEfectivo
DO:
  HIDE FRAME FormatoEfectivo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Trans
&Scoped-define SELF-NAME W_CedTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CedTrans C-Win
ON LEAVE OF W_CedTrans IN FRAME F_Trans /* Cèdula/Nit */
DO:
  DEFI VAR ROWID_Cte AS ROWID.
  
  ASSIGN W_CedTrans
         ROWID_Cte  = ROWID(Clientes).
         
  FIND FIRST Clientes WHERE Clientes.Nit EQ W_CedTrans NO-LOCK NO-ERROR.
  IF AVAIL(Clientes) THEN
     ASSIGN W_NomTx = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)
            W_NomTx:SCREEN-VALUE = W_NomTx
            W_NomTx:SENSITIVE    = FALSE.
  ELSE DO:            
     ASSIGN W_NomTx = " "
            W_NomTx:SCREEN-VALUE = W_NomTx
            W_NomTx:SENSITIVE    = TRUE.
            
     APPLY "ENTRY" TO W_NomTx.
  END.
                   
  FIND FIRST Clientes WHERE ROWID(Clientes) EQ ROWID_Cte NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame-Cuentas
&Scoped-define SELF-NAME W_CtaCC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaCC C-Win
ON LEAVE OF W_CtaCC IN FRAME Frame-Cuentas /* Centro de Costos */
DO:
   IF Cuentas.Id_Nit THEN DO:
       RUN MbCtaTab.
       APPLY "entry" TO F_nitcuenta IN FRAME Frame-Cuentas.
       RETURN NO-APPLY.
   END.
   ELSE DO:
     HIDE FRAME Frame-Cuentas.
     RUN Activar.
     APPLY "ENTRY" TO Producto.Cuenta IN BROWSE BROWSE-5.
     RETURN NO-APPLY.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Trans
&Scoped-define SELF-NAME W_NomTx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NomTx C-Win
ON LEAVE OF W_NomTx IN FRAME F_Trans
DO:
  ASSIGN W_NomTx.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_RevTx
&Scoped-define SELF-NAME W_NroTx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NroTx C-Win
ON LEAVE OF W_NroTx IN FRAME F_RevTx /* Nro.de la Transacciòn */
DO:
  ASSIGN W_NroTx.
  
  FOR EACH Taquilla WHERE Taquilla.Nro_Transaccion EQ W_NroTx
                      AND Taquilla.Fec_Transac     EQ W_Fecha
                      AND Taquilla.Usuario         EQ W_Usuario
                      AND Taquilla.Contabilizar    EQ TRUE NO-LOCK:
                      
      CREATE Producto.
      ASSIGN Cta_Caja               = Taquilla.Cta_Contra
             F_Nit                  = Taquilla.Nit
             Producto.OfiTem        = Taquilla.Age_Destino
             Producto.TipPto        = STRING(Taquilla.Tip_Producto) + "-"
             Producto.OperaAux      = Taquilla.Cod_Operacion 
             Producto.CodPto        = Taquilla.Cod_Producto                                  
             Producto.Nit           = Taquilla.Nit
             Producto.DtoRef        = STRING (Taquilla.Nro_Transaccion)
             Producto.Cuenta        = Taquilla.Nro_cuenta 
             Producto.CueBco        = Taquilla.Cta_Contra
            /* Producto.D_Cheq        = 
             Producto.NitCta*/
             Producto.Banco         = Taquilla.Cod_Compensa    
             Producto.Cheque        = Taquilla.Num_Retcheque.  
             
      IF Taquilla.Val_Cheque GT 0 AND Taquilla.Naturaleza EQ "DB" THEN             
         ASSIGN Producto.EC   = "C"                                                
                Producto.Debe = Taquilla.Val_Cheque.                               
      ELSE IF Taquilla.Val_Cheque GT 0 AND Taquilla.Naturaleza EQ "CR" THEN        
         ASSIGN Producto.Haber = Taquilla.Val_Cheque                               
                Producto.EC    = "C".                                              
      ELSE IF Taquilla.Val_Efectivo GT 0 AND Taquilla.Naturaleza EQ "CR" THEN      
         ASSIGN Producto.Haber = Taquilla.Val_Efectivo                             
                Producto.EC    = "E".                                              
      ELSE IF Taquilla.Val_Efectivo GT 0 AND Taquilla.Naturaleza EQ "DB" THEN      
         ASSIGN Producto.Debe = Taquilla.Val_Efectivo                              
                Producto.EC   = "E".  
                
      IF Taquilla.Tip_Producto EQ 1 THEN DO:
         FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Taquilla.Cod_Producto NO-LOCK NO-ERROR. 
         ASSIGN Producto.NomPto  = Pro_Ahorros.Nom_Producto
                Producto.Id_Tal  = Pro_Ahorros.Id_Talonario
               /* Producto.EstCta  = Ahorros.Detalle_Estado.*/
               Producto.TipPto = Producto.TipPto + STRING(Pro_Ahorros.Tip_Ahorro,"9").
      END.
      ELSE 
  END.
  
  IF NOT AVAIL(Producto) THEN
     MESSAGE "No se hallò el nùmero de la Transacciòn para la reversa" VIEW-AS ALERT-BOX.
  ELSE DO:
     FIND Clientes WHERE Clientes.Nit EQ F_Nit NO-LOCK NO-ERROR.
     IF AVAILABLE (Clientes) THEN DO:
        ASSIGN F_Nombre:SCREEN-VALUE IN FRAME Default-Frame = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)
               F_Nit:SCREEN-VALUE                           = F_Nit
               Cmb_Tipo:SCREEN-VALUE                        = "00 - Todos los Productos"
               F_Descripcion:SCREEN-VALUE                   = "Reversa de Transacc."
               F_Descripcion
               F_Seg = Clientes.Cod_Segmento.
     END.
  END.
  
  OPEN QUERY BROWSE-5 FOR EACH Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-10
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

RUN Inicializar.

ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.
PAUSE 0 BEFORE-HIDE.
ON RETURN TAB.
ON ENTRY OF Producto.Cuenta IN BROWSE BROWSE-5 DO:
   RUN MBBrwVc.
   ASSIGN AuxCta = Producto.Cuenta:SCREEN-VALUE IN BROWSE BROWSE-5
          IDCaja = 0.
END.
ON TAB OF Producto.Cuenta IN BROWSE BROWSE-5 DO:
    RUN MBCtaTab.
END.
ON LEAVE OF Producto.Cuenta IN BROWSE BROWSE-5 DO:
   IF Producto.TipPto NE "" AND
      AuxCta          NE Producto.Cuenta:SCREEN-VALUE IN BROWSE BROWSE-5 THEN DO:
      ASSIGN Producto.Cuenta:SCREEN-VALUE IN BROWSE BROWSE-5 = AuxCta.
      ASSIGN BROWSE BROWSE-5 Producto.Cuenta.
   END.
END.
ON ENTRY OF Producto.DtoRef IN BROWSE BROWSE-5 DO:
   IF F_Tipo EQ 3 THEN
      ASSIGN Producto.DtoRef:SCREEN-VALUE IN BROWSE BROWSE-5  = "".
   IF Producto.Cuenta EQ "" AND Producto.TipPto EQ "" THEN DO:
      APPLY "ENTRY" TO Producto.Cuenta IN BROWSE BROWSE-5.
      RETURN NO-APPLY.
   END.
END.
ON LEAVE OF Producto.DtoRef IN BROWSE BROWSE-5 DO:   
    RUN Documento.
END.
ON ENTRY OF Producto.Debe IN BROWSE BROWSE-5 DO:
   IF Producto.DtoRef EQ "" THEN DO:
      APPLY "ENTRY" TO Producto.DtoRef IN BROWSE BROWSE-5.
      RETURN NO-APPLY.
   END.
   ELSE DO:
     IF Producto.TipPto EQ "" THEN DO:
        FIND Tmp_Cuentas WHERE Tmp_Cuentas.Estado  EQ 1 
                         AND   Tmp_Cuentas.Cuenta  EQ Producto.Cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE(Tmp_Cuentas) AND Tmp_Cuentas.Cod_Caja EQ 2 THEN DO:
           APPLY KEYFUNCTION(LASTKEY) TO SELF.
           RETURN NO-APPLY.
        END.
        ASSIGN IDCaja = Tmp_Cuentas.Cod_Caja.       /*1*/
     END.
     ELSE DO:
        IF F_Tipo EQ 3 THEN DO:
           APPLY KEYFUNCTION(LASTKEY) TO SELF.
           RETURN NO-APPLY.
        END.
     END.
   END.
END.
ON LEAVE OF Producto.Debe IN BROWSE BROWSE-5 DO:
   ASSIGN BROWSE BROWSE-5 Producto.Debe.
   IF Producto.Debe EQ 0 THEN DO:
      IF IDCaja EQ 1 THEN DO:
         APPLY "ENTRY" TO Producto.Cuenta IN BROWSE BROWSE-5.
         RETURN NO-APPLY.
      END.
   END.
   ELSE DO:
     IF Producto.TipPto NE "" THEN DO:
         IF Producto.EstCta GT 2 AND
            Producto.EstCta EQ 10 THEN DO:
            FIND Varios WHERE Varios.Tipo EQ 21 AND Varios.Codigo EQ Producto.EstCta NO-LOCK NO-ERROR.
            IF AVAILABLE Varios THEN DO:
               MESSAGE "La cuenta se encuentra bloqueada." SKIP
                       "Tipo de Bloqueo: " Varios.Descripcion VIEW-AS ALERT-BOX ERROR.
               ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
               APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
               RETURN NO-APPLY.
            END.
            ELSE DO:
               MESSAGE "No se encuentra el estado de la cuenta" SKIP
                       "tal ves no se encuentra matriculado" SKIP
                       "el estado actual es: " Ahorros.Detalle_Estado VIEW-AS ALERT-BOX ERROR.
               ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
               APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
               RETURN NO-APPLY.
            END.
         END.

         RUN Verificar_Estado.        
        CASE SUBSTRING(Producto.TipPto,1,1):
          WHEN "1" THEN DO:
           IF SUBSTRING(Producto.TipPto,3,1) EQ "1" THEN DO:
               RUN Val_ConVista NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                 ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
                 APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
                 RETURN NO-APPLY.
              END.
           END.
           IF SUBSTRING(Producto.TipPto,3,1) EQ "2" THEN DO:
               RUN Val_ConContractual NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                 ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
                 APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
                 RETURN NO-APPLY.
              END.
           END.
           IF SUBSTRING(Producto.TipPto,3,1) EQ "3" THEN DO:
               RUN Val_ConTermino NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                 ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
                 APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
                 RETURN NO-APPLY.
              END.
           END.
           IF SUBSTRING(Producto.TipPto,3,1) EQ "4" THEN DO:
               RUN Val_ConAportes NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                 ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
                 APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
                 RETURN NO-APPLY.
              END.
           END.
          END.
          WHEN "4" THEN DO:
              RUN Val_Especiales NO-ERROR.
           IF ERROR-STATUS:ERROR THEN DO:
              ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
              APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
              RETURN NO-APPLY.
           END.
          END.
        END CASE.
        ASSIGN W_VrEfectivo               = 0
               Producto.ConsigEf          = 0
               Producto.ConsigEf:SCREEN-VALUE IN BROWSE Browse-5 = STRING(W_VrEfectivo).
        RUN Cedula_Transacc.
     END.     
     ASSIGN Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
     APPLY "ENTRY" TO Producto.EC IN BROWSE BROWSE-5.
     RETURN NO-APPLY.
   END.
END.

ON ENTRY OF Producto.Haber IN BROWSE BROWSE-5 DO:

    /*GCamacho May04/08 - Implementación Ahorro Permanente*/
    IF producto.TipPto EQ "1-2" AND CodPto EQ 221 THEN DO:
        MESSAGE "No se pueden hacer Retiros para esta cuenta" SKIP
            "Por esta opción"
                      VIEW-AS ALERT-BOX ERROR TITLE "Retiro Bloqueado".
        ASSIGN Producto.Haber = 0
               Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
        APPLY "ENTRY":U TO Producto.Debe.
        RETURN NO-APPLY.
    END.
    /************************************************************/

   IF Producto.TipPto EQ "" THEN DO:
      FIND Tmp_Cuentas WHERE Tmp_Cuentas.Estado  EQ 1 
                       AND   Tmp_Cuentas.Cuenta  EQ Producto.Cuenta NO-LOCK NO-ERROR.
      IF AVAILABLE(Tmp_Cuentas) AND Tmp_Cuentas.Cod_Caja EQ 1 THEN DO:
         APPLY KEYFUNCTION(LASTKEY) TO SELF.
         RETURN NO-APPLY.
      END.
      ASSIGN IDCaja = Tmp_Cuentas.Cod_Caja. /*2.*/
   END.
   ELSE DO:
      IF SUBSTRING(Producto.TipPto,1,1) EQ "2" OR SUBSTRING(Producto.TipPto,1,1) EQ "4" THEN DO:
         APPLY KEYFUNCTION(LASTKEY) TO SELF.
         RETURN NO-APPLY.
      END.
   END.
END.
ON LEAVE OF Producto.Haber IN BROWSE BROWSE-5 DO:
   ASSIGN BROWSE BROWSE-5 Producto.Haber
          Producto.Debe:SCREEN-VALUE = "0"
          W_VrEfectivo               = 0
          Producto.ConsigEf          = 0
          Producto.ConsigEf:SCREEN-VALUE IN BROWSE Browse-5 = STRING(W_VrEfectivo).
   RUN Verificar_Estado.
   IF Producto.TipPto BEGINS "1" AND
      Producto.CodPto NE 5       AND
      Producto.Haber  GT DECIMAL(F_SDisponible1:SCREEN-VALUE IN FRAME FRAME-AHORROS) + 
                         DEC(F_Intporpagar1:SCREEN-VALUE) THEN DO:
      MESSAGE "El Valor del Retiro No Puede ser Mayor" skip
              "Que el Saldo Disponible + Int.X Pagar. RECTIFIQUE!" SKIP(2)
              "Retiro: " Producto.Haber " Saldo: " DECIMAL(F_SDisponible1:SCREEN-VALUE IN FRAME FRAME-AHORROS)   VIEW-AS ALERT-BOX
              TITLE "INCONSISTENCIA".
      RETURN NO-APPLY.
   END.
   
   /*giocam - Feb-02-08 - entra a buscar productos de ahorro con cargo GMF del cliente*/
   FIND FIRST Bpro_ahorros WHERE Pro_Ahorros.Id_GMF EQ 1 AND 
        Pro_Ahorros.VrTope_ExentoEE GT 0 NO-LOCK NO-ERROR.
   IF AVAILABLE Bpro_ahorros THEN DO:
       IF  Producto.CodPto NE 23 AND Producto.Haber EQ DECIMAL(F_SDisponible1:SCREEN-VALUE) + DEC(F_Intporpagar1:SCREEN-VALUE)
       AND SUBSTRING(Producto.TipPto,3,1) NE "3" THEN
           MESSAGE "El RETIRO - el GMF es $" Producto.Haber - ROUND(DECIMAL(F_SDisponible1:SCREEN-VALUE) * .004,0)
                 VIEW-AS ALERT-BOX.
   END.

        
   IF Producto.Haber EQ 0 AND IDCaja EQ 2 THEN DO:
      APPLY "ENTRY" TO Producto.Cuenta IN BROWSE BROWSE-5.
      RETURN NO-APPLY.
   END.
   ELSE DO:
     IF Producto.Haber NE 0 THEN DO:
        RUN Documento NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:                                    
           APPLY "ENTRY" TO Producto.DtoRef IN BROWSE BROWSE-5.            
           RETURN NO-APPLY.                                               
        END.                                                           
        IF W_CedTrans LE "0" AND Producto.Haber NE 0 THEN DO:
           ASSIGN FRAME Default-Frame:SENSITIVE = FALSE                                   
                  FRAME F_Trans:SENSITIVE       = TRUE                                    
                  FRAME F_Trans:VISIBLE         = TRUE
                  W_NomTx:SCREEN-VALUE    = F_Nombre:SCREEN-VALUE IN FRAME Default-Frame  
                  W_NomTx                 = F_Nombre:SCREEN-VALUE IN FRAME Default-Frame  
                  W_CedTrans:SCREEN-VALUE = F_Nit                                         
                  W_CedTrans              = F_Nit.  
            APPLY "Entry" TO W_CedTrans.
            RETURN NO-APPLY.                         
        END.        
        RUN ValidaAutoriz NO-ERROR.                                     
        IF ERROR-STATUS:ERROR THEN DO:                                                                                     
           ASSIGN Producto.Haber = 0                                          
                  Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".       
           APPLY "ENTRY" TO Producto.Haber IN BROWSE BROWSE-5.                
           RETURN NO-APPLY.                                                   
        END.                                                                  
     END.
   END.
END.
ON ANY-PRINTABLE OF Producto.EC IN BROWSE BROWSE-5 DO:
   IF ASC(CAPS(CHR(LASTKEY))) NE 32 THEN DO:
      IF ASC(CAPS(CHR(LASTKEY))) NE 69 THEN DO:
         IF ASC(CAPS(CHR(LASTKEY))) NE 67 THEN DO:
            RETURN NO-APPLY.
         END.
      END.
   END.
END.   
ON ENTRY OF Producto.EC IN BROWSE BROWSE-5 DO:
   IF F_Tipo EQ 3 OR F_Tipo EQ 5 THEN DO: 
     ASSIGN Producto.EC:SCREEN-VALUE IN BROWSE BROWSE-5 = "E".
     APPLY KEYFUNCTION(LASTKEY) TO SELF.
     RETURN NO-APPLY.
   END.
END.
ON LEAVE OF Producto.EC IN BROWSE BROWSE-5 DO:
    Producto.EC:SCREEN-VALUE IN BROWSE BROWSE-5 = CAPS(Producto.EC:SCREEN-VALUE).
        
    IF Producto.TipPto BEGINS "1" AND Producto.Haber GT 0 THEN DO:
        RUN Documento NO-ERROR.
            
        IF NOT ERROR-STATUS:ERROR THEN DO:
            RUN ValidaAutoriz NO-ERROR.
                
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN Producto.Haber = 0
                       Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
                    
                APPLY "ENTRY" TO Producto.Haber IN BROWSE BROWSE-5.
                RETURN NO-APPLY.
            END.
        END.
        ELSE DO:
            APPLY "ENTRY" TO Producto.DtoRef IN BROWSE BROWSE-5.
            RETURN NO-APPLY.
        END.
    END.
        
    IF Producto.TipPto EQ "" AND Producto.Haber GT 0 AND (SUBSTRING(Producto.Cuenta,1,2) NE "19" AND SUBSTRING(Producto.Cuenta,1,2) NE "27") THEN
        RUN Find_SdoFinal.
        
    ASSIGN BROWSE BROWSE-5 Producto.EC.
        
    IF Producto.Debe EQ 0 AND Producto.Haber EQ 0 THEN
        Producto.EC:SCREEN-VALUE IN BROWSE BROWSE-5 = "".
    ELSE DO:
        IF Producto.EC EQ "" THEN
            Producto.EC:SCREEN-VALUE IN BROWSE BROWSE-5 = "E".
        ELSE DO:
            IF Producto.EC EQ "C" AND Producto.Debe NE 0 THEN DO:
                ValCheq = 0.
                    
                RUN Desactivar.
                    
                CLOSE QUERY BROWSE-7.
                    
                FIND FIRST Tmp_Cheques WHERE Tmp_Cheques.W_Cuenta EQ Producto.Cuenta
                                         AND Tmp_Cheques.W_CodPto EQ Producto.CodPto
                                         AND Tmp_Cheques.W_CodPto NE 0 NO-LOCK NO-ERROR.
                IF AVAILABLE(Tmp_Cheques) THEN DO:
                    VIEW FRAME Frame_Cheques.
                    OPEN QUERY BROWSE-7 FOR EACH Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta
                                                           AND Cheques.W_CodPto EQ Producto.CodPto.
                    APPLY "ENTRY" TO Cheques.W_Canje IN BROWSE BROWSE-7.
                    RETURN NO-APPLY.
                END.
                ELSE DO:
                    VIEW FRAME Frame_Cheques.
                    RUN Insert_Cheque.
                        
                    ASSIGN Cheques.W_Valor:SCREEN-VALUE IN BROWSE BROWSE-7 = "0"
                           F_Sumatoria:SCREEN-VALUE IN FRAME Frame_Cheques = "0".
                        
                    APPLY "ENTRY" TO Cheques.W_Canje IN BROWSE BROWSE-7.
                    RETURN NO-APPLY.
                END.
            END.

            IF Producto.EC EQ "C" AND Producto.Haber NE 0 THEN DO:
                IF NUM-RESULTS("BROWSE-10") NE 0 THEN DO:
                    RUN Desactivar.

                    {&OPEN-QUERY-BROWSE-10}

                    VIEW FRAME Frame-Cuentas-Bancos.
                    APPLY "ENTRY" TO BROWSE-10.
                    RETURN NO-APPLY.
                END.
                ELSE DO:
                    MESSAGE "No Hay Cuentas de Banco Configuradas."
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Error en Taquilla".

                    RETURN NO-APPLY.
                END.
            END.
        END.
    END.

    IF Producto.EC EQ "C" AND Producto.Haber NE 0 THEN.
    ELSE DO:
        APPLY "ENTRY" TO Producto.Retiro IN BROWSE BROWSE-5.
        RETURN NO-APPLY.
    END.
END.

ON ENTRY OF Producto.Retiro IN BROWSE BROWSE-5 DO:
   RUN Actualizar_Valor.
   IF Producto.Debe NE 0 OR Producto.TipPto EQ "" THEN DO:
      APPLY KEYFUNCTION(LASTKEY) TO SELF.
      RETURN NO-APPLY.
   END.
   IF F_Tipo EQ 3 THEN DO: 
     APPLY KEYFUNCTION(LASTKEY) TO SELF.
     RETURN NO-APPLY.
   END.
END.
ON LEAVE OF Producto.Retiro IN BROWSE BROWSE-5 DO:
   Producto.Retiro:SCREEN-VALUE IN BROWSE BROWSE-5 = CAPS(Producto.Retiro:SCREEN-VALUE).
   IF (DECIMAL(F_Sdisponible1:SCREEN-VALUE IN FRAME frame-ahorros) + 
      DECIMAL(F_intporpagar1:SCREEN-VALUE IN FRAME frame-ahorros)) EQ 
      DECIMAL(Producto.Haber:SCREEN-VALUE IN BROWSE browse-5) AND
      DECIMAL(Producto.Haber:SCREEN-VALUE IN BROWSE browse-5) GT 0 AND
      Producto.Retiro:SCREEN-VALUE IN BROWSE BROWSE-5 NE "S" THEN DO:
      IF (ahorros.tarjetadb EQ "" AND ahorros.cod_ahorro NE 3) AND (ahorros.cod_ahorro NE 216) THEN DO: /* Convenios por nomina */
         MESSAGE "Se trata de retirar el total de disponible más los intereses por pagar" SKIP
                 "El Producto quedará cancelado" VIEW-AS ALERT-BOX.
         Producto.Retiro:SCREEN-VALUE IN BROWSE BROWSE-5 = "S".
      END.
   END.
   IF Producto.Debe EQ 0 AND Producto.Haber EQ 0 THEN
      ASSIGN Producto.Retiro:SCREEN-VALUE IN BROWSE BROWSE-5 = "N".
   ELSE DO:
     RUN Validar_Reg NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0"
               Producto.EC:SCREEN-VALUE IN BROWSE BROWSE-5    = "".
        APPLY "ENTRY" TO Producto.Haber IN BROWSE BROWSE-5.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        IF RETURN-VALUE EQ "ERROR" THEN DO:
           ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5  = "0"
                  Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0"
                  Producto.EC:SCREEN-VALUE IN BROWSE BROWSE-5    = "".
           APPLY "ENTRY" TO Producto.Haber IN BROWSE BROWSE-5.
           RETURN NO-APPLY.
        END.
     END.
   END.
   IF NUM-RESULTS("BROWSE-5") EQ Producto.W_Order THEN DO:
      IF NUM-RESULTS("BROWSE-9") NE 0 THEN DO:
         FIND FIRST Tmp_Cuentas WHERE Tmp_Cuentas.Id_Caja EQ TRUE NO-LOCK NO-ERROR.
         IF AVAILABLE(Tmp_Cuentas) THEN DO:
            RUN Insert_Row.
            ASSIGN  Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5   = "0"
                    Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5  = "0"
                    Producto.Retiro:SCREEN-VALUE IN BROWSE BROWSE-5 = "N".

            APPLY "ENTRY" TO Producto.Cuenta IN BROWSE BROWSE-5.
            RETURN NO-APPLY.
         END.
      END.
   END.
END.
ON ANY-PRINTABLE OF Cheques.W_Canje IN BROWSE BROWSE-7 DO:
     IF ASC(CHR(LASTKEY)) NE 49 THEN DO:
        IF ASC(CHR(LASTKEY)) NE 50 THEN DO:
           IF ASC(CHR(LASTKEY)) NE 51 THEN DO:
              RETURN NO-APPLY.
           END.
        END.
     END.
END.
ON ENTRY OF Cheques.W_Cheque IN BROWSE BROWSE-7 DO:
   ASSIGN BROWSE BROWSE-7 Cheques.W_Banco.
   FIND Bancos WHERE Bancos.Cod_Compensa EQ Cheques.W_Banco NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(Bancos) THEN DO:
      MESSAGE "Banco No Existe en Tabla de Bancos."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      APPLY "ENTRY" TO Cheques.W_Banco IN BROWSE BROWSE-7.
      RETURN NO-APPLY.
   END.
END.
ON ENTRY OF Cheques.W_Valor IN BROWSE BROWSE-7 DO:
   ASSIGN BROWSE BROWSE-7 Cheques.W_Banco Cheques.W_Cheque.
   IF Cheques.W_Banco EQ ? OR Cheques.W_Cheque EQ "" THEN DO:
      APPLY "ENTRY" TO Cheques.W_Banco IN BROWSE BROWSE-7.
      RETURN NO-APPLY.
   END. 
END.
ON TAB, RETURN OF Cheques.W_Valor IN BROWSE BROWSE-7 DO:
   ASSIGN BROWSE BROWSE-7 Cheques.W_Canje  Cheques.W_Banco 
                          Cheques.W_Cheque Cheques.W_Valor.
   RUN CONTROL_Cheques.
   IF ValCheq EQ Producto.Debe THEN DO:
       HIDE FRAME Frame_Cheques.
       RUN Activar.     
       APPLY "entry" TO Producto.Retiro IN BROWSE Browse-5.
    END.
    ELSE DO:
      IF ValCheq GT Producto.Debe THEN DO:
         Cheques.W_Valor:SCREEN-VALUE IN BROWSE BROWSE-7 = "0".
         MESSAGE "La Sumatoria de Valores No Coincide con el Valor a Consignar."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Error en Cheques en Canje".
         RETURN NO-APPLY.                  
      END.
      ELSE DO:
         RUN Diferencia_VrEfectivo.        
      END.
   END.
END.
ON TAB, RETURN, MOUSE-SELECT-CLICK OF BROWSE-9 IN FRAME  Frame-Cuentas DO:
   ASSIGN Producto.Cuenta:SCREEN-VALUE IN BROWSE BROWSE-5 = Cuentas.Cuenta.
          
   APPLY "entry" TO W_CtaCC IN FRAME Frame-cuentas.
   IF Cuentas.Id_Nit THEN 
       RUN MbCtaTab.
   ELSE DO:
     HIDE FRAME Frame-Cuentas.
     RUN Activar.
     APPLY "ENTRY" TO Producto.Cuenta IN BROWSE BROWSE-5.
     RETURN NO-APPLY.
   END.
END.
ON TAB, RETURN, MOUSE-SELECT-CLICK OF BROWSE-10 IN FRAME Frame-Cuentas-Bancos DO:
   ASSIGN Producto.Banco    = INTEGER(Operacion.Cod_Compensa:SCREEN-VALUE IN BROWSE BROWSE-10)
          Producto.CueBco   = Cuentas.Cuenta:SCREEN-VALUE IN BROWSE BROWSE-10
          Producto.OperaAux = Operacion.Cod_Operacion
          Producto.Formato  = Cuentas.Cod_Formato
          F_Beneficiario:SCREEN-VALUE IN FRAME Frame-Cuentas-Bancos = CAPS(F_Nombre:SCREEN-VALUE IN FRAME DEFAULT-FRAME).
   APPLY "ENTRY" TO F_NroCheque IN FRAME Frame-Cuentas-Bancos.
   RETURN NO-APPLY.
END.
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:                          

      /*giocam - Oct 10/07 - Implementación PSTimer para desplegar mensajes al cajero avisando de existencias de efectivo*/
      ASSIGN chCtrlFrame:PSTimer:Interval = 1800000. /*60000 es un + o - 1 minuto*/

     FIND Agencias WHERE Agencias.Agencia  EQ W_Agencia 
                   AND   Agencias.Estado   EQ 1 NO-LOCK NO-ERROR.
     IF AVAILABLE (Agencias) THEN DO:
        ASSIGN F_Agencia:SCREEN-VALUE    = STRING(W_Agencia)
               W_Autorizo                = W_Usuario.
     END.
     ELSE DO:
        MESSAGE "La Agencia Debe Estar en Estado Normal. Verifique...".
        APPLY "CLOSE":U TO THIS-PROCEDURE.
     END.
     FIND Usuarios WHERE Usuarios.Agencia EQ W_Agencia
                   AND   Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
     IF AVAILABLE(Usuarios) THEN DO:
        ASSIGN W_Grupo = Usuarios.Grupo.
     END.

     RUN Cuentas NO-ERROR.

     IF ERROR-STATUS:ERROR THEN
        APPLY "CLOSE":U TO THIS-PROCEDURE.
     C-Win:MOVE-TO-TOP().
     APPLY "leave" TO Cmb_tipo.
     RETURN NO-APPLY.   
  END. /* DO FRAME */


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Activar C-Win 
PROCEDURE Activar :
/*------------------------------------------------------------------------------
  OBSERVACIONES :       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN BROWSE-5:SENSITIVE     = TRUE
            Btn_Cancelar:SENSITIVE = TRUE
            Btn_Grabar:SENSITIVE   = TRUE
            Btn_ImpSaldo:SENSITIVE = TRUE
            Btn_Salir:SENSITIVE    = TRUE
            F_Agencia:SENSITIVE    = TRUE
            Cmb_Tipo:SENSITIVE       = TRUE.
  END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualizar_Valor C-Win 
PROCEDURE Actualizar_Valor :
/*------------------------------------------------------------------------------
  Observaciones : Permite Actualizar los valores de la Sumatoria de los Totales.       
------------------------------------------------------------------------------*/
   DEFINE VAR C_ValCheqCon AS DECIMAL DECIMALS 2 FORMAT "->>>,>>>,>>>,>>>,>>9.99".
   DEFINE VAR C_ValCheqret AS DECIMAL DECIMALS 2 FORMAT "->>>,>>>,>>>,>>>,>>9.99".
   DEFINE VAR C_ValEfecCon AS DECIMAL DECIMALS 2 FORMAT "->>>,>>>,>>>,>>>,>>9.99".
   DEFINE VAR C_ValEfeRet  AS DECIMAL DECIMALS 2 FORMAT "->>>,>>>,>>>,>>>,>>9.99".

   ASSIGN  BROWSE BROWSE-5  Producto.Debe Producto.Haber Producto.EC.
           
   ASSIGN C_ValCheqCon    = 0
          C_ValCheqret    = 0
          C_ValEfecCon    = 0
          C_ValEfeRet     = 0.

   FOR EACH Tmp_Pto NO-LOCK:
       IF Tmp_Pto.EC EQ "C" THEN DO:
          ASSIGN C_ValCheqCon = C_ValCheqCon + Tmp_Pto.Debe
                 C_ValCheqret = C_ValCheqret + Tmp_Pto.Haber. 
          IF C_ValCheqCon GT DECIMAL(F_Sumatoria:SCREEN-VALUE IN FRAME FRAME_cheques) THEN
             ASSIGN C_ValEfecCon = C_ValCheqCon - DECIMAL(F_Sumatoria:SCREEN-VALUE IN FRAME FRAME_cheques)
                    C_ValCheqCon  = C_ValCheqCon - C_ValEfecCon.
       END.
       IF Tmp_Pto.EC EQ "E" THEN DO:
          ASSIGN C_ValEfecCon = C_ValEfecCon + Tmp_Pto.Debe
                 C_ValEfeRet  = C_ValEfeRet  + Tmp_Pto.Haber.
       END.
   END.

   ASSIGN F_ValCheqCon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(C_ValCheqCon)
          F_ValCheqret:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(C_ValCheqret)
          F_ValEfecCon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(C_ValEfecCon)
          F_ValEfeRet:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = STRING(C_ValEfeRet)
          F_Consigna = C_ValEfecCon + C_ValCheqCon
          F_Consigna:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(F_Consigna)
          F_Retiro   = C_ValEfeRet + C_ValCheqret
          F_Retiro:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(F_Retiro)
          F_Total    = F_Consigna - F_Retiro
          F_Total:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(F_Total).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Armar_Operacion C-Win 
PROCEDURE Armar_Operacion :
DEFINE OUTPUT PARAMETER T_Op AS INTEGER FORMAT "999999999".
DEFINE OUTPUT PARAMETER T_Dd LIKE Operacion.Cod_Deducible.

DEFINE VARIABLE Aux_Ope AS CHARACTER FORMAT "X(9)".
DEFINE VARIABLE Aux_EfeChe AS CHARACTER FORMAT "X(2)".
DEFINE VARIABLE Aux_ConRet AS CHARACTER FORMAT "X(2)".

IF Producto.EC = "e" THEN
    Aux_EfeChe = "01".
    
IF Producto.EC = "c" THEN
    Aux_EfeChe = "02".
    
IF Producto.Debe GT 0 THEN
    Aux_ConRet = "01".
    
IF Producto.Haber GT 0 THEN
    Aux_ConRet = "02".
    
/*El codigo de _Operacion se compone de:
  99 - Tipo de Producto:(01-Ahorros, 02- Creditos, etc)
  99 - Clase: 01-Taquilla, 02- Nomina, 03-Procesos
  99 - Tipo: 01-Retiro, 02-Consignacion, 03-Procesos, 04-Correccion
  999- Consecutivo de la operracion*/
    
Aux_Ope = STRING(INTEGER(SUBSTRING(Producto.TipPto,1,1)),"99") + "01" + Aux_ConRet.

FIND FIRST Operacion WHERE STRING(Operacion.Cod_Operacion,"999999999") BEGINS Aux_Ope
                       AND Operacion.Ctrl_EfeChe EQ INTEGER(Aux_EfeChe)
                       AND Operacion.Estado EQ 1
                       AND Operacion.Id_SYA EQ NO NO-LOCK NO-ERROR.
IF AVAILABLE Operacion THEN DO:
    ASSIGN T_Op = Operacion.Cod_Operacion
           T_Dd = Operacion.Cod_Deducible
           WCpteTx = Operacion.Comprobante.

    RUN Validar_Operacion(INPUT W_Agencia,
                          INPUT W_Grupo,
                          INPUT W_Usuario,
                          INPUT T_Op,
                          INPUT Operacion.Id_Clave,
                          INPUT Operacion.Nom_Operacion) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
END.
ELSE DO:
    MESSAGE "No fue encontrada ninguna operación:" Aux_Ope SKIP
            "Para realizar la transacción." SKIP
            "Transacción CANCELADA"
        VIEW-AS ALERT-BOX.

    RETURN ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Auditoria C-Win 
PROCEDURE Auditoria :
/*------------------------------------------------------------------------------
   OBSERVACIONES : Imprimir en la Validadora La Operacion para la Auditoria.     
  ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER P_Cuenta     AS CHAR    FORMAT "X(26)".
    DEFINE INPUT PARAMETER P_TipTra     AS CHAR    FORMAT "X(40)". 
    DEFINE INPUT PARAMETER P_Docto      AS CHAR    FORMAT "X(10)".
    DEFINE INPUT PARAMETER P_NomPto     AS CHAR    FORMAT "X(28)".
    DEFINE INPUT PARAMETER P_ValorEf    AS DECIMAL FORMAT "-***,***,***,**9.99".
    DEFINE INPUT PARAMETER P_ValorCh    AS DECIMAL FORMAT "-***,***,***,**9.99".
    
    DEFINE VAR W_FecTra AS CHAR FORMAT "X(30)".

    ASSIGN W_FecTra = STRING(TODAY,"99/99/9999") + "  -  " + STRING(TIME,"HH:MM:SS AM").

    OUTPUT TO COM1.
    PUT CONTROL CHR(27) + "@". 
    PUT CONTROL CHR(27) + "SP".
    PUT CONTROL CHR(27) + "T" + CHR(0).
      
    PUT W_FecTra SKIP.
    PUT P_TipTra SKIP.
    PUT "Agencia  : " TRIM(W_Nom_Agencia) SKIP.
    PUT "Pcto     : " P_NomPto  SKIP.
    PUT "Cuenta   : " P_Cuenta  SKIP.
    PUT "Dto.Ref  : " P_Docto   SKIP.
    PUT "Valor Ef.: " P_ValorEf SKIP.
    PUT "Valor Ch.: " P_ValorCh SKIP.
      
    PUT CONTROL CHR(12).
    PUT CONTROL CHR(27) + "q".
    OUTPUT CLOSE.
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borrar C-Win 
PROCEDURE Borrar :
/*------------------------------------------------------------------------------
  Observaciones : Permite Limpiar la Pantalla Y Arrancar de Nuevo.    
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_Agencia:SCREEN-VALUE    = STRING(W_Agencia)
            Cmb_Tipo:SCREEN-VALUE       = "00 - Todos los Productos"
            F_Tipo                    = 0
            F_Cuenta:SCREEN-VALUE     = ""
            F_Nit:SCREEN-VALUE        = ""
            F_Nombre:SCREEN-VALUE     = ""
            F_Consigna:SCREEN-VALUE   = "0"
            F_Retiro:SCREEN-VALUE     = "0"
            F_Total:SCREEN-VALUE      = "0"
            F_ValCheqCon:SCREEN-VALUE = "0"
            F_ValCheqret:SCREEN-VALUE = "0"
            F_ValEfecCon:SCREEN-VALUE = "0"
            F_ValEfeRet:SCREEN-VALUE  = "0"
            F_Consigna                = 0
            F_Retiro                  = 0
            F_Total                   = 0
            F_ValCheqCon              = 0
            F_ValCheqret              = 0
            F_ValEfecCon              = 0
            F_ValEfeRet               = 0
            Cont                      = 0.
     ASSIGN Com_Producto:SENSITIVE = FALSE 
            F_Cuenta:SENSITIVE     = FALSE
            F_Nit:SENSITIVE        = FALSE
            F_Chequeo:SENSITIVE    = FALSE
            F_Agencia:SENSITIVE    = FALSE.
     HIDE FRAME Frame_Cheques.
     HIDE FRAME Frame-Nit.
     HIDE FRAME Frame-Cuentas.
     HIDE FRAME Frame-Ahorros.
     FOR EACH Producto:
         DELETE Producto.
     END.
     FOR EACH Cheques:
         DELETE Cheques.
     END.
     IF Com_Producto:NUM-ITEMS GT 0 THEN 
            Com_Producto:LIST-ITEMS   = "".

     OPEN QUERY BROWSE-5 FOR EACH Producto.
     OPEN QUERY BROWSE-7 FOR EACH Cheques.
     ON RETURN TAB.
     APPLY "ENTRY" TO Cmb_Tipo.
     RETURN NO-APPLY.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borrar_Ahorros C-Win 
PROCEDURE Borrar_Ahorros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DO WITH FRAME Frame-Ahorros:
        ASSIGN F_Cuota1:SCREEN-VALUE        = ""
               F_DetalleEstado:SCREEN-VALUE = ""
               F_FecProxLiq1:SCREEN-VALUE   = ""
               F_FecUltLiq1:SCREEN-VALUE    = ""
               F_FecCancela1:SCREEN-VALUE   = ""
               F_Intporpagar1:SCREEN-VALUE  = ""
               F_Intsobregiro1:SCREEN-VALUE = ""
               F_Plazo1:SCREEN-VALUE        = ""
               F_SCanje1:SCREEN-VALUE       = ""
               F_SDisponible1:SCREEN-VALUE  = ""
               F_SIntPagados1:SCREEN-VALUE  = ""
               F_Vlrsobregiro1:SCREEN-VALUE = "".
     END.
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_CtaEsp C-Win 
PROCEDURE Buscar_CtaEsp :
/*------------------------------------------------------------------------------
  OBSERVACIONES :        
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER E_Agencias LIKE Pro_Especiales.Agencia.
  DEFINE INPUT  PARAMETER E_CodPro   LIKE Pro_Especiales.Cod_Producto.
  DEFINE OUTPUT PARAMETER E_CtaPato  LIKE Cuentas.Cuenta.
  DEFINE OUTPUT PARAMETER E_CtaSya   LIKE Cuentas.Cuenta.
  
  FIND FIRST Pro_Especiales WHERE Pro_Especiales.Agencia      EQ E_Agencias
                            AND   Pro_Especiales.Cod_Producto EQ E_CodPro 
                            AND   Pro_Especiales.Estado       EQ 1 NO-LOCK NO-ERROR.
  IF AVAILABLE(Pro_Especiales) THEN
       ASSIGN E_CtaPato = Pro_Especiales.Cta_Recaudos
              E_CtaSya  = Pro_Especiales.Cta_SucyAge.
  ELSE RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Cuenta C-Win 
PROCEDURE Buscar_Cuenta :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Cuentas de los Productos.       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT  PARAMETER B_Pto    LIKE pro_ahorros.tip_ahorro.
   DEFINE INPUT  PARAMETER B_CodPto LIKE ahorros.cod_ahorro.
   DEFINE INPUT  PARAMETER B_Plazo  LIKE Ahorros.Plazo.
   DEFINE INPUT  PARAMETER B_Nit    LIKE Clientes.Nit.
   DEFINE OUTPUT PARAMETER B_Cuenta LIKE Cuentas.Cuenta.
   DEFINE OUTPUT PARAMETER B_CtaSYA_DES LIKE Cuentas.Cuenta.
   DEFINE OUTPUT PARAMETER B_CtaSYA_FTE LIKE Cuentas.Cuenta.
   
   FIND Clientes WHERE Clientes.Nit EQ B_Nit NO-LOCK NO-ERROR.
   IF AVAILABLE(Clientes) THEN DO:
       IF W_Error OR B_Pto EQ 1 THEN DO:
          FIND FIRST CortoLargo WHERE CortoLargo.Agencia        EQ Producto.OfiTem /*Busca Oficina destino*/
                                  AND CortoLargo.Clase_Producto EQ B_Pto 
                                  AND CortoLargo.Cod_Producto   EQ B_CodPto
                                  AND CortoLargo.Plazo_Inicial  LE B_Plazo
                                  AND CortoLargo.Plazo_Final    GE B_Plazo NO-LOCK NO-ERROR.
          IF AVAILABLE(CortoLargo) THEN DO:
             B_CtaSYA_DES = CortoLargo.Cta_SYA.
             IF Clientes.Tipo_Vinculo EQ 1 THEN ASSIGN B_Cuenta = CortoLargo.Cta_AsoAd.
             ELSE ASSIGN B_Cuenta = CortoLargo.Cta_NoaAd.
          END.
          ELSE do:
              MESSAGE "1-Falta Configurar en CortoLargo cta CortoLargo.Cta_AsoAd de Pdto Ahorros:"
                      SKIP "Agencia: " Producto.OfiTem SKIP "Clase: " B_Pto  SKIP "Producto :"
                      B_CodPto SKIP "Para el plazo : " B_Plazo SKIP VIEW-AS ALERT-BOX ERROR.
              RETURN ERROR.
          END.
          IF Producto.OfiTem NE W_Agencia THEN DO:
              FIND CortoLargo WHERE CortoLargo.Agencia        EQ W_Agencia /*Busca Oficina Fuente*/
                              AND   CortoLargo.Clase_Producto EQ B_Pto 
                              AND   CortoLargo.Cod_Producto   EQ B_CodPto
                              AND   CortoLargo.Plazo_Inicial  LE B_Plazo
                              AND   CortoLargo.Plazo_Final    GE B_Plazo
                              NO-LOCK NO-ERROR.
              IF AVAILABLE(CortoLargo) THEN do:
                  /*MESSAGE "CortoLargo.Cta_SYA : " CortoLargo.Cta_SYA VIEW-AS ALERT-BOX.*/
                  B_CtaSYA_FTE = CortoLargo.Cta_SYA.
              END.
              ELSE do:
                  MESSAGE "2-Falta Configurar en CortoLargo.Cta_SYA de Pdto Ahorros: " B_CodPto SKIP
                      "Para el plazo : " B_Plazo SKIP
                      "de la agencia : " W_Agencia
                  VIEW-AS ALERT-BOX ERROR.
                  RETURN ERROR.
              END.
          END.
       END.
   END.
   ELSE RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_OpRetiro C-Win 
PROCEDURE Buscar_OpRetiro :
DEFINE OUTPUT PARAMETER OpRet      LIKE Operacion.Cod_Operacion.

 /*El codigo de _Operacion se compone de:
   99 - Tipo de Producto:(01-Ahorros, 02- Creditos, etc)
   99 - Clase: 01-Taquilla, 02- Nomina, 03-Procesos
   99 - Tipo: 01-Consignacion, 02-Retiro, 03-Procesos, 04-Correccion
   999- Consecutivo de la operracion*/
   
   FIND FIRST Operacion WHERE STRING(Operacion.Cod_Operacion,"999999999") BEGINS "010102" AND
                                   Operacion.Ctrl_EfeChe    EQ     1 AND 
                                   Operacion.Estado         EQ     1 AND 
                                   Operacion.Id_SYA         EQ NO NO-LOCK NO-ERROR.
 IF AVAILABLE Operacion THEN
    ASSIGN OpRet = Operacion.Cod_Operacion.
 ELSE RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cedula_Transacc C-Win 
PROCEDURE Cedula_Transacc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF W_CedTrans LE "0" THEN DO:                                                    
     ASSIGN FRAME Default-Frame:SENSITIVE = FALSE                                  
            FRAME F_Trans:SENSITIVE       = TRUE                                   
            FRAME F_Trans:VISIBLE         = TRUE                                   
            W_NomTx:SCREEN-VALUE    = F_Nombre:SCREEN-VALUE IN FRAME Default-Frame 
            W_NomTx                 = F_Nombre:SCREEN-VALUE IN FRAME Default-Frame 
            W_CedTrans:SCREEN-VALUE = F_Nit                                        
            W_CedTrans              = F_Nit.                                       
                                                                                   
     APPLY "Entry" TO W_CedTrans.                                                  
     RETURN NO-APPLY.                                                              
  END.                                                                             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CobraCheque C-Win 
PROCEDURE CobraCheque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   FIND Operacion WHERE Operacion.Cod_Operacion EQ Pro_Ahorros.Cod_CobroCheq NO-LOCK NO-ERROR.
   IF AVAILABLE Operacion THEN DO:
      IF Operacion.Cuenta EQ "" THEN DO:
         MESSAGE "No se encuentra configurada la cuenta en la operación" SKIP
                 "se cancela la operacion de cobro Cheque" VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      END.
      ELSE DO:
         IF Operacion.Cuenta EQ "" THEN DO:
            MESSAGE "No se encuentra la cuenta en la transacción Cobro-Cheque." SKIP
                    "informe al adminsitrador para que rectifique"
                    VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
         END.
         
         ASSIGN W_CtaOpe = Operacion.Cuenta
                W_CodDed = Operacion.Cod_Deducible
                W_Comision = Operacion.Comision
                W_Operacion = Operacion.Cod_Operacion.
                
         IF W_CodDed NE "" THEN DO:
           FIND Deducible WHERE Deducible.Cod_Deducible = W_CodDed NO-LOCK NO-ERROR.
           IF NOT AVAILABLE Deducible THEN DO:
              MESSAGE "No existe el deducible en la Transaccion Cobro-Cheque." SKIP
                      "Informe al Administrador para que rectifique la Transacción" SKIP
                      "para el cobro del Cheque!"
                      VIEW-AS ALERT-BOX ERROR.
              RETURN ERROR. 
           END.
           ELSE DO: 
             ASSIGN W_DedCta    = Deducible.Cuenta
                    W_DedCla    = Deducible.Cla_Deducible
                    W_DedVal    = Deducible.Valor
                    W_DedValImp = Deducible.Valor_Impuesto
                    W_DedCtaImp = Deducible.Cuenta_Impuesto.
             IF Deducible.Cod_Deducible NE "2" THEN
                W_DedNom    = Deducible.Nom_Deducible.
             ELSE
                W_DedNom    = Operacion.Nom_Operacion.
             IF W_DedCla EQ 1 THEN
                W_DedVal = Pro_Ahorro.Val_CadaCheque - (Pro_Ahorro.Val_CadaCheque / (W_DedVal + 1)).
           END.
         END.
         
         IF W_Comision NE "" THEN DO:
           FIND Deducible WHERE Deducible.Cod_Deducible = W_Comision NO-LOCK NO-ERROR.
           IF NOT AVAILABLE Deducible THEN DO:
              MESSAGE "No existe un deducible con Codigo igual a la Comisión configurada" SKIP
                      "en la Transaccion Cobro-Cheque." SKIP(1)
                      "Informe al Administrador para que rectifique la Transacción" SKIP
                      "para el cobro del Cheque!"
                      VIEW-AS ALERT-BOX ERROR.
              RETURN ERROR. 
           END.
           ELSE DO:
             ASSIGN W_ComCta    = Deducible.Cuenta
                    W_ComCla    = Deducible.Cla_Deducible
                    W_ComVal    = Deducible.Valor
                    W_ComValImp = Deducible.Valor_Impuesto
                    W_ComCtaImp = Deducible.Cuenta_Impuesto.
             IF Deducible.Cod_Deducible NE "2" THEN
                W_ComNom    = Deducible.Nom_Deducible.
             ELSE
                W_ComNom    = Operacion.Nom_Operacion.
             IF W_DedCla EQ 1 THEN
                W_DedVal = Pro_Ahorro.Val_CadaCheque - (Pro_Ahorro.Val_CadaCheque / (W_DedVal + 1)).
             IF W_ComCla EQ 1 THEN
                W_ComVal = Pro_Ahorro.Val_CadaCheque * W_ComVal.
           END.
         END.
         
         FIND FIRST CortoLargo WHERE CortoLargo.Agencia  EQ Ahorros.Agencia /*Busca Oficina destino*/
                         AND   CortoLargo.Clase_Producto EQ 1 
                         AND   CortoLargo.Cod_Producto   EQ Ahorros.Cod_Ahorro
                         NO-LOCK NO-ERROR.
         IF AVAILABLE(CortoLargo) THEN DO:
            ASSIGN W_CtaCor = CortoLargo.Cta_AsoAd
                   W_Total  = Pro_Ahorros.Val_CadaCheq + 
                              ROUND(W_DedVal,0) + ROUND(Pro_Ahorros.Val_CadaCheq * W_DedValImp,0) + 
                              ROUND(W_ComVal,0) + ROUND(Pro_Ahorros.Val_CadaCheq * W_ComValImp,0).
            IF Ahorros.Sdo_Disponible LT W_Total THEN DO:
               MESSAGE "La cuenta no tiene efectivo suficiente para el debito," SKIP
                       "del valor del cheque." SKIP
                       "Se cancela la operaciòn"
                       VIEW-AS ALERT-BOX.
               RETURN ERROR.
            END.
            
            RUN Contab_DebCheq NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                  MESSAGE "La contabilizaciòn del Dèbito del Cheque presentò error..." SKIP
                          "Rectifique por favor." VIEW-AS ALERT-BOX ERROR.
                  RETURN ERROR.
            END.
         END.
         ELSE DO:
           MESSAGE "No ha encontrado la configuración de corto y largo" SKIP
                   "para el producto de ahorros actual. Rectifique" SKIP
                   "esta configuracion." VIEW-AS ALERT-BOX ERROR.
           RETURN ERROR.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Consignacion_Ahorro C-Win 
PROCEDURE Consignacion_Ahorro :
Consig:
DO TRANSACTION ON ERROR UNDO Consig:
    IF Ahorros.Detalle_Estado EQ 2 AND Ahorros.Sdo_Disponible LT 0 AND Producto.EC EQ "E" THEN DO: /*Esta sobregirado*/
        IF Pro_Ahorros.Id_Sobregiro THEN DO:
            RUN Sob_ConEfec NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                RELEASE Ahorros NO-ERROR.
                RETURN ERROR.
            END.
        END.
        ELSE DO:
            IF Ahorros.tarjetadb EQ '' THEN DO:
                RELEASE Ahorros NO-ERROR.

                MESSAGE "El Producto relacionado a la cuenta" SKIP
                        "no maneja la modalidad de sobregiro"
                    VIEW-AS ALERT-BOX ERROR TITLE "Error en Taquilla".

                RETURN ERROR.
            END.
        END.
    END.
    ELSE DO:
        Ahorros.Fec_UltTransaccion = TODAY.

        IF Ahorros.Detalle_Estado EQ 1 THEN DO:
            ASSIGN Ahorros.Detalle_Estado = 2
                   Ahorros.Sdo_Inicial = Producto.Debe
                   Ahorros.Monto_Apertura = Producto.Debe
                   Ahorros.Fec_Apertura = W_Fecha.

            IF Ahorros.Tip_Ahorro EQ 3 OR Ahorros.Tip_Ahorro EQ 2 THEN DO: /*asigna fecvencimiento para atermino y contract*/
                IF Ahorros.Per_Liquidacion EQ 6 AND Ahorros.Tip_Ahorro NE 2 THEN
                    ASSIGN Ahorros.Fec_ProLiquidacion = W_Fecha + Ahorros.Plazo - 1
                           Ahorros.Fec_Vencimiento = W_Fecha + Ahorros.Plazo.
                ELSE DO:
                    IF Ahorros.Per_Liquidacion EQ 1 THEN
                        ASSIGN Ahorros.Fec_ProLiquidacion = W_Fecha + 1
                               Ahorros.Fec_Vencimiento = W_Fecha + Ahorros.Plazo.
                    ELSE DO:
                        IF Ahorros.Tip_Ahorro EQ 2 THEN
                            Ahorros.Fec_Vencimiento = Ahorros.Fec_ProLiquidacion + 1.
                        ELSE DO:
                            RUN Halla_FVcto.
                    END.
                END.
            END.
        END.
    END.

    IF Producto.EC EQ "E" THEN DO:
        ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Producto.Debe
               Ahorros.Num_DepDia = Ahorros.Num_DepDia + 1
               Ahorros.Val_DepDia = Ahorros.Val_DepDia + Producto.Debe
               Ahorros.Num_DepMes = Ahorros.Num_DepMes + 1
               Ahorros.Val_DepMes = Ahorros.Val_DepMes + Producto.Debe.

        RUN reportarVisionamosAh(INPUT 1,
                                 INPUT producto.debe).

        IF ahorros.tarjetaDB NE " " THEN
            RUN grabar_TmpTarDeb(INPUT Producto.Debe,
                                 INPUT "Consignacion en Efectivo",
                                 INPUT 3).
    END.
    ELSE DO:
        ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Producto.ConsigEf + (Producto.Debe - Producto.ConsigEf)
               Ahorros.Num_DepDia = Ahorros.Num_DepDia + 1
               Ahorros.Val_DepDia = Ahorros.Val_DepDia + (Producto.Debe - Producto.ConsigEf)
               Ahorros.Num_DepMes = Ahorros.Num_DepMes + 1
               Ahorros.Val_DepMes = Ahorros.Val_DepMes + (Producto.Debe - Producto.ConsigEf).

        RUN reportarVisionamosAh(INPUT 1,
                                 INPUT Producto.Debe - Producto.ConsigEf).

        IF ahorros.tarjetaDB NE " " THEN
    END.

    RUN Gra_MovAhorros(INPUT T_Ope,
                       INPUT Producto.CodPto,
                       INPUT Producto.Cuenta,
                       INPUT Producto.DtoRef,
                       INPUT Producto.OfiTem,
                       INPUT W_Agencia,
                       INPUT Producto.OfiTem,
                       INPUT Producto.UsuAut,
                       INPUT 0,
                       INPUT Producto.Debe,
                       INPUT Ahorros.Nit).

    movProductos.sdo_disponible = producto.debe.

    Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
    movProductos.tipo_transaccion = 1.

    IF Ahorros.Tip_Ahorro EQ 3 THEN
        Ahorros.Monto_Apertura = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.

    IF Producto.EC EQ "C" AND Producto.ConsigEf EQ 0 THEN
        ASSIGN Mov_Ahorros.Val_Cheque = Producto.Debe
               Mov_Ahorros.Val_Efectivo = 0
               movProductos.sdo_canje = producto.debe
               movProductos.sdo_disponible = 0.

    IF Producto.EC EQ "C" AND Producto.ConsigEf GT 0 THEN DO:
        ASSIGN Mov_Ahorros.Sdo_Disponible = Mov_Ahorros.Sdo_Disponible - Producto.ConsigEf
               Mov_Ahorros.Val_Cheque = Producto.Debe - Producto.ConsigEf
               Mov_Ahorros.Val_Efectivo = 0
               movProducto.sdo_canje = producto.debe - producto.consigEf
               movProducto.sdo_disponible = 0.

        RUN Gra_MovAhorros(INPUT T_Ope,
                           INPUT Producto.CodPto,
                           INPUT Producto.Cuenta,
                           INPUT Producto.DtoRef,
                           INPUT Producto.OfiTem,
                           INPUT W_Agencia,
                           INPUT Producto.OfiTem,
                           INPUT Producto.UsuAut,
                           INPUT 0,
                           INPUT Producto.Debe,
                           INPUT Ahorros.Nit).

        ASSIGN Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
               Mov_Ahorros.Val_Cheque = 0
               Mov_Ahorros.Cod_Operac = 010101001
               Mov_Ahorros.Val_Efectivo = Producto.ConsigEf
               Mov_Ahorros.Descrip = "Consig.en Efectivo".

        ASSIGN movProductos.sdo_disponible = producto.consigEf
               movProductos.tipo_transaccion = 1.
    END.

    IF Producto.EC EQ "C" THEN DO: /*Consignacion en Cheque recorrido cheques entrados*/
        FOR EACH Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta
                           AND Cheques.W_CodPto EQ Producto.CodPto
                           AND Cheques.W_Valor GT 0 NO-LOCK:
            IF Producto.OfiTem EQ W_Agencia THEN DO:
                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco,      INPUT T_Ope,            INPUT Producto.CodPto,  INPUT CtaCble,      INPUT Cta_Banco,        INPUT "CR",
                                 INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Cheques.W_Cheque, INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                                 INPUT 1,               INPUT W_Usuario,            INPUT Cheques.W_Valor,  INPUT 0,                INPUT F_Seg) NO-ERROR.
            END.
            ELSE DO:
                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco,      INPUT T_Ope,            INPUT Producto.CodPto,  INPUT CtaSYA_Des,   INPUT Cta_Banco,        INPUT "CR",
                                 INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Cheques.W_Cheque, INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                                 INPUT 1,               INPUT W_Usuario,            INPUT Cheques.W_Valor,  INPUT 0,                INPUT F_Seg) NO-ERROR.

                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco,      INPUT T_Ope,            INPUT Producto.CodPto,  INPUT CtaCble,          INPUT CtaSYA_Fte,       INPUT "CR",
                                 INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Cheques.W_Cheque, INPUT Producto.OfiTem,  INPUT Producto.OfiTem,  INPUT W_Agencia,
                                 INPUT 1,               INPUT W_Usuario,            INPUT Cheques.W_Valor,  INPUT 0,                INPUT F_Seg) NO-ERROR.
            END.

            IF ERROR-STATUS:ERROR OR NOT AVAIL(Taquilla) THEN DO:
                MESSAGE "Error o La Tabla Taquilla no fue Grabada, Revise por favor." SKIP
                        "La transaccion no debe quedar registrada..."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                MESSAGE "Recuerde revisar la transaccion, si no fue anulada," SKIP
                        "                                 Por favor reporte a Sistemas."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.

            RUN Gra_CheqTransito(INPUT Cheques.W_Banco,
                                 INPUT Cheques.W_Cheque,
                                 INPUT Producto.CodPto,
                                 INPUT 1,
                                 INPUT Producto.Cuenta,
                                 INPUT Producto.Ofitem,
                                 INPUT "1",
                                 INPUT Cheques.W_Valor,
                                 INPUT Cheques.W_Canje) NO-ERROR.
            IF ERROR-STATUS:ERROR OR NOT AVAIL(Che_Transito) THEN DO:
                MESSAGE "Error grabando Cheques-Canje...Revise por favor."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                MESSAGE "Recuerde revisar la transaccion, si no fue anulada," SKIP
                        "                                 Por favor reporte a Sistemas."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.
        END.
    END.

    IF Producto.EC EQ "E" OR Producto.ConsigEf GT 0 THEN DO:
        /*Graba taquilla cuando se hace consignacion en efectivo*/
        IF Producto.OfiTem EQ W_Agencia THEN DO:
           RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT T_Ope, 
                            INPUT Producto.CodPto, INPUT CtaCble,         INPUT Cta_Caja,
                            INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                            INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                            INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT 1,            
                            INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).
           IF Producto.ConsigEf GT 0 THEN
              ASSIGN Taquilla.Val_Cheque   = 0
                     Taquilla.Val_Efectivo = Producto.ConsigEf. 

        END.
        ELSE DO:                                                                          /* *ultimo */
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT T_Ope, 
                             INPUT Producto.CodPto, INPUT CtaSYA_Des,      INPUT  AuxCaja /*Cta_Banco */, 
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT 1,            
                             INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).
            IF Producto.ConsigEf GT 0 THEN
               ASSIGN Taquilla.Val_Cheque   = 0
                      Taquilla.Val_Efectivo = Producto.ConsigEf. 

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT T_Ope, 
                             INPUT Producto.CodPto, INPUT CtaCble,         INPUT CtaSYA_Fte,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT 1,            
                             INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).
            IF Producto.ConsigEf GT 0 THEN
               ASSIGN Taquilla.Val_Cheque   = 0
                      Taquilla.Val_Efectivo = Producto.ConsigEf.
        END.
        /*RELEASE Taquilla.*/
     END.
  END.
END.   /*Fin TRANS.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar C-Win 
PROCEDURE Contabilizar :
Contables:
DO TRANSACTION ON ERROR UNDO Contables:
    FOR EACH Taquilla WHERE Taquilla.Usuario EQ W_Usuario
                        AND Taquilla.Contabiliza EQ NO BREAK BY Taquilla.Nro_Transaccion:
        IF FIRST-OF(Taquilla.Nro_Transaccion) THEN DO:
            FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
            IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN
                ASSIGN Cbte = Operacion.Comprobante.
            ELSE DO:
                RUN MostrarMensaje IN W_Manija (INPUT 143,
                                                OUTPUT W_Error).

                RETURN ERROR.
            END.
        END.

        RUN Contabilizar_Partidas NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.
        ELSE
            Taquilla.Contabiliza = YES.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Partidas C-Win 
PROCEDURE Contabilizar_Partidas :
DEFINE VAR W_CCostos AS INTEGER.
DEFINE VAR W_DRef AS CHARACTER.
DEFINE VAR WComentario AS CHARACTER FORMAT "X(30)".
DEFINE VAR WComent1 AS CHARACTER FORMAT "X(30)".
DEFINE VAR numDocAux AS INTEGER.

ContabPartidas:
DO TRANSACTION ON ERROR UNDO ContabPartidas:
    IF F_Descripcion:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN DO:
        IF Taquilla.Descripcion NE "" THEN
            ASSIGN WComentario = Taquilla.Descripcion
                   WComent1 = WComentario.
        ELSE DO:
            FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
            IF AVAILABLE(Operacion) THEN
                ASSIGN WComentario = SUBSTRING(Producto.NomPto,1,15) + "," + TRIM(Operacion.Nom_Operacion)
                       WComent1 = WComentario.

            IF Operacion.Clase_Operacion EQ 1 AND Operacion.Tipo_Operacion EQ 2 AND Operacion.Ctrl_EfeChe EQ 2 THEN
                ASSIGN WComentario = "Ret.Che: " + F_Beneficiario:SCREEN-VALUE IN FRAME Frame-cuentas-bancos
                       Taquilla.descripcion = F_Beneficiario:SCREEN-VALUE.
        END.
    END.
    ELSE
        ASSIGN WComentario = F_Descripcion:SCREEN-VALUE
               WComent1 = WComentario.

    ASSIGN FRAME frame-cuentas W_CtaCC.

    W_CCostos = 999.

    IF W_CtaCC NE 0 THEN
        W_CCostos = W_CtaCC.

    W_DRef = Taquilla.Num_Retcheque.

    FIND Cuentas WHERE Cuentas.Cuenta EQ Taquilla.Cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Cuentas) THEN DO:
        MESSAGE "No existe Taquilla.Cuenta : " Taquilla.Cuenta
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN ERROR.
    END.

    IF W_DRef EQ ? OR W_DRef EQ " " THEN
        W_DRef = Taquilla.Num_Documento.

    numDocAux = W_DocContab.
    
    IF Taquilla.Agencia <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
        FIND FIRST comprobantes WHERE comprobantes.agencia = Taquilla.Agencia
                                  AND comprobantes.comprobante = Cbte NO-ERROR.
        IF AVAILABLE comprobantes THEN DO:
            comprobantes.secuencia = comprobantes.secuencia + 1.
            numDocSyA = comprobantes.secuencia.
            W_DocContab = numDocSyA.
            flagCtaSucyAg = TRUE.
        END.
    END.
    ELSE DO:
        IF Taquilla.Agencia <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
            FIND FIRST comprobantes WHERE comprobantes.agencia = Taquilla.Agencia
                                      AND comprobantes.comprobante = Cbte NO-ERROR.
            IF AVAILABLE comprobantes THEN DO:
                numDocSyA = comprobantes.secuencia.
                W_DocContab = numDocSyA.
            END.
        END.
    END.

    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Taquilla.Agencia
           Mov_Contable.Comprobante = Cbte
           Mov_Contable.Cuenta = Taquilla.Cuenta
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = WComent1
           Mov_Contable.Usuario = Taquilla.Usuario
           Mov_contable.Nit = Taquilla.Nit
           Mov_Contable.Cen_Costos = W_CCostos
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Num_Documento = W_DocContab
           Mov_Contable.Doc_Referencia = Taquilla.Num_Documento
           Mov_contable.Enlace = STRING(Taquilla.Nro_Transaccion)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion NO-ERROR.

    IF taquilla.agencia <> w_agencia THEN
        ASSIGN mov_contable.enlace = STRING(numDocAux).
    
    IF Taquilla.Naturaleza EQ "DB" THEN DO:
        Mov_Contable.DB = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.
    END.
    ELSE DO:
        Mov_Contable.CR = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.
    END.

    IF SUBSTRING(Taquilla.Cuenta,1,4) EQ "1904" OR SUBSTRING(Taquilla.Cuenta,1,4) EQ "2705" THEN DO:
        Mov_contable.Nit = STRING(W_Agencia,"999").

        IF Taquilla.Agencia EQ W_Agencia THEN DO:
            Mov_contable.Nit = STRING(Taquilla.Age_Destino,"999").
        END.
    END.

    ASSIGN viagen = Mov_Contable.Agencia
           vdfecha = Mov_Contable.Fec_Contable.
    
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar Mov_Contable...Revise por favor."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN ERROR.
    END.

    IF W_Rev THEN DO:
        Mov_Contable.Doc_Refer = STRING (W_NroTx).
    END.

    /*contrapartida*/
    W_DRef = Taquilla.Num_Retcheque.

    FIND Cuentas WHERE Cuentas.Cuenta EQ Taquilla.Cta_Contra NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Cuentas) THEN DO:
        MESSAGE "No existe Taquilla.Cta_Contra : " Taquilla.Cta_Contra
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN ERROR.
    END.

    IF W_DRef EQ ? OR W_DRef EQ " " THEN
        W_DRef = Taquilla.Num_Documento.

    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Taquilla.Agencia
           Mov_Contable.Comprobante = Cbte
           Mov_Contable.Cuenta = Taquilla.Cta_Contra
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = WComentario
           Mov_Contable.Usuario = Taquilla.Usuario
           Mov_contable.Nit = Taquilla.Nit
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Cen_Costos = W_CCostos
           Mov_Contable.Num_Documento = W_DocContab
           Mov_Contable.Doc_Referencia = W_DRef
           Mov_contable.Enlace = STRING(Taquilla.Nro_Transaccion)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion NO-ERROR.

    IF taquilla.agencia <> w_agencia THEN
        ASSIGN mov_contable.enlace = STRING(numDocAux).

    IF Taquilla.Naturaleza EQ "DB" THEN DO:
        Mov_Contable.CR = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.
    END.
    ELSE DO:
        Mov_Contable.DB = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.
    END.

    IF SUBSTRING(Taquilla.Cta_Contra,1,4) EQ "1904" OR SUBSTRING(Taquilla.Cta_Contra,1,4) EQ "2705" THEN DO:
        Mov_contable.Nit = STRING(W_Agencia,"999").
        
        IF Taquilla.Agencia EQ W_Agencia THEN DO:
            Mov_contable.Nit = STRING(Taquilla.Age_Destino,"999").
        END.
    END.

    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar Mov_Contable...Revise por favor."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN ERROR.
    END.

    IF W_Rev THEN DO:
        Mov_Contable.Doc_Refer = STRING (W_NroTx).
    END.
END.

W_DocContab = numDocAux.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_DebCheq C-Win 
PROCEDURE Contab_DebCheq :
DEFI VAR P_ImpAplic LIKE Pro_Ahorros.Val_Talonario INIT 0.
DEFINE VAR numDocAux AS INTEGER.

FIND FIRST Operacion WHERE STRING(Operacion.Cod_Operacion,"999999999") BEGINS "010302"
                       AND Operacion.Estado EQ 1
                       AND Operacion.Id_SYA EQ NO NO-LOCK NO-ERROR.
IF AVAIL(Operacion) AND Operacion.Cod_Deducible GT " " THEN DO:
    numDocAux = W_DocContab.
    
    IF ahorros.agencia <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
        FIND FIRST comprobantes WHERE comprobantes.agencia = ahorros.agencia
                                  AND comprobantes.comprobante = Cbte NO-ERROR.
        IF AVAILABLE comprobantes THEN DO:
            comprobantes.secuencia = comprobantes.secuencia + 1.
            numDocSyA = comprobantes.secuencia.
            W_DocContab = numDocSyA.
            flagCtaSucyAg = TRUE.
        END.
    END.
    ELSE DO:
        IF ahorros.agencia <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
            FIND FIRST comprobantes WHERE comprobantes.agencia = ahorros.agencia
                                      AND comprobantes.comprobante = Cbte NO-ERROR.
            IF AVAILABLE comprobantes THEN DO:
                numDocSyA = comprobantes.secuencia.
                W_DocContab = numDocSyA.
            END.
        END.
    END.

    RUN RutGMF.r(INPUT TRUE,
                 INPUT W_Agencia,
                 INPUT Ahorros.Agencia,
                 INPUT 1,
                 INPUT Ahorros.Cod_Ahorro,
                 INPUT Ahorros.Nit,
                 INPUT Ahorros.Cue_Ahorro,
                 INPUT Operacion.Cod_Operacion,
                 INPUT (W_Total - W_DedVal),
                 INPUT Cbte,
                 INPUT STRING(W_DocContab),
                 INPUT "Débito X CheqGirado",
                 INPUT 0,
                 INPUT 3,
                 OUTPUT P_ImpAplic) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "El Prog: RutGMF.P...Retorno ERROR(Salvando) no se permite la operaciòn..."
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.
END.

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
       Mov_Contable.Comprobante = Cbte
       Mov_Contable.Cuenta = W_CtaOpe
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = "Dèbito Cheque-Girado"
       Mov_Contable.Usuario = W_Usuario
       Mov_contable.Nit = Ahorros.Nit
       Mov_Contable.Cen_Costos = 999
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Num_Documento = W_DocContab
       Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion
       Mov_Contable.CR = Pro_Ahorros.Val_CadaCheq - ROUND(W_DedVal,0) NO-ERROR.

IF ahorros.agencia <> w_agencia THEN
    ASSIGN mov_contable.enlace = STRING(numDocAux).

/*Deducible*/
IF W_DedVal NE 0 THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
           Mov_Contable.Comprobante = Cbte
           Mov_Contable.Cuenta = W_DedCta
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = W_DedNom
           Mov_Contable.Usuario = W_Usuario
           Mov_contable.Nit = Ahorros.Nit
           Mov_Contable.Cen_Costos = 999
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Num_Documento = W_DocContab
           Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
           Mov_Contable.Fec_Grabacion  = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.CR = ROUND(W_DedVal,0) NO-ERROR.

    IF ahorros.agencia <> w_agencia THEN
        ASSIGN mov_contable.enlace = STRING(numDocAux).
END.

/*impuesto al Deducible*/
IF W_DedValImp NE 0 THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
           Mov_Contable.Comprobante = Cbte
           Mov_Contable.Cuenta = W_DedCtaImp
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = "Impuesto: " + STRING(W_DedValImp)
           Mov_Contable.Usuario = W_Usuario
           Mov_contable.Nit = Ahorros.Nit
           Mov_Contable.Cen_Costos = 999
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Num_Documento = W_DocContab
           Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.CR = ROUND(Pro_Ahorros.Val_CadaCheq * W_DedValImp,0) NO-ERROR.

    IF ahorros.agencia <> w_agencia THEN
        ASSIGN mov_contable.enlace = STRING(numDocAux).
END.

/*Comision*/
IF W_ComVal NE 0 THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
           Mov_Contable.Comprobante = Cbte
           Mov_Contable.Cuenta = W_ComCta
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = W_ComNom
           Mov_Contable.Usuario = W_Usuario
           Mov_contable.Nit = Ahorros.Nit
           Mov_Contable.Cen_Costos = 999
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Num_Documento = W_DocContab
           Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.CR = ROUND(W_ComVal,0) NO-ERROR.

    IF ahorros.agencia <> w_agencia THEN
        ASSIGN mov_contable.enlace = STRING(numDocAux).
END.

/*impuesto a la comision*/
IF W_ComValImp NE 0 THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
           Mov_Contable.Comprobante = Cbte
           Mov_Contable.Cuenta = W_ComCtaImp
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = "Impuesto: " + STRING(W_ComValImp)
           Mov_Contable.Usuario = W_Usuario
           Mov_contable.Nit = Ahorros.Nit
           Mov_Contable.Cen_Costos = 999
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Num_Documento = W_DocContab
           Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.CR = ROUND(Pro_Ahorros.Val_CadaCheq * W_ComValImp,0) NO-ERROR.

    IF ahorros.agencia <> w_agencia THEN
        ASSIGN mov_contable.enlace = STRING(numDocAux).
END.

/*copntrapartida*/
CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
       Mov_Contable.Comprobante = Cbte
       Mov_Contable.Cuenta = W_CtaCor
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = "Débito x Cheque-Girado"
       Mov_Contable.Usuario = W_Usuario
       Mov_contable.Nit = Ahorros.Nit
       Mov_Contable.Cen_Costos = 999
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Num_Documento = W_DocContab        
       Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion
       Mov_Contable.DB = ROUND(W_Total,0) - ROUND(W_DedVal,0) NO-ERROR.

IF ahorros.agencia <> w_agencia THEN
    ASSIGN mov_contable.enlace = STRING(numDocAux).


/*registro en mov_ahorros*/
CREATE Mov_Ahorros.
ASSIGN Mov_Ahorros.Cod_Operacion = Operacion.Cod_Operacion
       Mov_ahorros.cod_ahorro = Ahorros.Cod_Ahorro
       Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorros
       Mov_ahorros.nit = Ahorros.Nit
       Mov_Ahorros.Fecha = w_fecha
       Mov_Ahorros.Hora = TIME
       Mov_Ahorros.Cpte = Cbte
       Mov_Ahorros.Num_Documento = STRING(W_DocContab)
       Mov_Ahorros.Agencia = Ahorros.Agencia
       Mov_Ahorros.Age_Fuente = W_Agencia
       Mov_Ahorros.Age_Destino = W_Agencia
       Mov_Ahorros.Usuario = W_Usuario
       Mov_Ahorros.Val_Efectivo = ROUND(W_Total,0) - ROUND(W_DedVal,0)   /*Mayo 10/05 GAER*/
       Mov_Ahorros.Descrip = "Débito x Cheque-Girado".

CREATE movProductos.
ASSIGN movProductos.agencia = ahorros.agencia
       movProductos.comprobante = Cbte
       movProductos.estado = 1
       movProductos.fecha = w_fecha
       movProductos.id_producto = ahorros.cue_ahorros
       movProductos.nit = ahorros.nit
       movProductos.num_documento = W_DocContab
       movProductos.sdo_disponible = ROUND(W_Total,0) - ROUND(W_DedVal,0)
       movProductos.tipo_producto = 1
       movProductos.tipo_transaccion = 2.

/*Saca del sdo disponible el valor debitado*/
ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - (ROUND(W_Total,0) - ROUND(W_DedVal,0))
       Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.   /*Mayo 10/05 GAER*/

RUN reportarVisionamosAh(INPUT 2,
                         INPUT ROUND(W_Total,0) - ROUND(W_DedVal,0)).

/* Creamos la transacción en la tabla de transacciones */
FIND FIRST transAhorros WHERE transAhorros.cod_ahorro = ahorros.cod_ahorro
                          AND transAhorros.cue_ahorros = ahorros.cue_ahorros
                          AND transAhorros.nit = ahorros.nit
                          AND transAhorros.fec_contable = w_fecha
                          AND transAhorros.comprobante = Cbte
                          AND transAhorros.num_documento = W_DocContab
                          AND transAhorros.agencia = w_agencia NO-ERROR.
IF NOT AVAILABLE transAhorros THEN DO:
    CREATE transAhorros.
    ASSIGN transAhorros.cod_ahorro = ahorros.cod_ahorro
           transAhorros.cue_ahorros = ahorros.cue_ahorros
           transAhorros.nit = ahorros.nit
           transAhorros.fec_contable = w_fecha
           transAhorros.comprobante = Cbte
           transAhorros.num_documento = W_DocContab
           transAhorros.agencia = w_agencia.
END.

transAhorros.sdo_disponible = transAhorros.sdo_disponible - (ROUND(W_Total,0) - ROUND(W_DedVal,0)).
/* --------------------------------------------------- */

IF ahorros.tarjetaDB NE " " THEN
    RUN grabar_TmpTarDeb(INPUT ROUND(W_Total,0) - ROUND(W_DedVal,0),
                         INPUT "Débito x Cheque-Girado",
                         INPUT 2).
W_DocContab = numDocAux.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ControlLavado C-Win 
PROCEDURE ControlLavado :
ASSIGN FRAME {&FRAME-NAME} F_Consigna F_Retiro F_ValEfecCon F_ValCheqCon.
DEFINE VAR RT_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR CS_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR AB_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR RT_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR CS_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR AB_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR PCod LIKE BorradorSipla.CodAutoriza.
DEFINE VAR PUsu LIKE W_Usuario.
DEFINE VAR MenSipla  AS CHARACTER FORMAT "X(5)".
DEFINE VAR MenSiplaM AS CHARACTER FORMAT "X(5)".
FIND LAST ControlSipla WHERE
          ControlSipla.Nit          EQ Ahorros.Nit AND
          MONTH(ControlSipla.Fecha) EQ MONTH(W_Fecha) NO-ERROR.
IF AVAILABLE ControlSipla THEN DO:
   IF ControlSipla.Fecha EQ W_Fecha THEN
      ASSIGN CS_ValSiplaDia = ControlSipla.CS_TotalDia
             AB_ValSiplaDia = ControlSipla.AB_TotalDia
             RT_ValSiplaDia = ControlSipla.RT_TotalDia.
   ASSIGN CS_ValSiplaMes = ControlSipla.CS_TotalMes
          AB_ValSiplaMes = ControlSipla.AB_TotalMes
          RT_ValSiplaMes = ControlSipla.RT_TotalDia.
END.
ELSE DO:
    CREATE ControlSipla.
    ASSIGN ControlSipla.Nit         = Ahorros.Nit
           ControlSipla.Fecha       = W_Fecha.
END.
IF F_Consigna GT 0 THEN DO: /*es consignacion*/
/*    IF CS_ValSiplaDia + AB_ValSiplaDia + F_Consigna GT Entidad.MaxOp_Efectivo_Dia THEN */
    IF CS_ValSiplaDia + AB_ValSiplaDia + DECIMAL(F_ValEfecCon:SCREEN-VALUE IN FRAME {&FRAME-NAME}) GT Entidad.MaxOp_Efectivo_Dia THEN /*Giocam Oct16/07 - Solo tiene en cuenta consig. en efectivo*/
        ASSIGN MenSipla  = "CSDIA". 
/*     IF CS_ValSiplaMes + AB_ValSiplaMes + F_Consigna  GT Entidad.MaxOp_Efectivo_Mes THEN */
    IF CS_ValSiplaMes + AB_ValSiplaMes + DECIMAL(F_ValEfecCon:SCREEN-VALUE IN FRAME {&FRAME-NAME}) GT Entidad.MaxOp_Efectivo_Mes THEN /*Giocam Oct16/07 - Solo tiene en cuenta consig. en efectivo*/
        ASSIGN MenSiplaM = "CSMES".
END.
IF F_Retiro GT 0 THEN DO: /* es retiro */
   IF RT_ValSiplaDia + F_Retiro GT Entidad.MaxOp_Efectivo_Dia THEN
      ASSIGN MenSipla  = "RTDIA".
   IF RT_ValSiplaMes + F_Retiro GT Entidad.MaxOp_Efectivo_Mes THEN
      ASSIGN MenSiplaM = "RTMES".
END.
ASSIGN PCod = 0
       CtrLavado = NO.

IF MenSipla NE "" OR MenSiplaM NE "" THEN 
   RUN C-ControlSipla.w (INPUT Ahorros.Nit, INPUT MenSipla, INPUT MenSiplaM, OUTPUT PCod).
ELSE DO: 
   CtrLavado = YES.
   /*MESSAGE controlsipla.fecha w_fecha VIEW-AS ALERT-BOX.*/
   IF ControlSipla.Fecha NE W_Fecha THEN DO:
       CREATE ControlSipla.
       ASSIGN ControlSipla.Nit         = Ahorros.Nit
              ControlSipla.Fecha       = W_Fecha.
   END.
   ASSIGN ControlSipla.CS_TotalMes = ControlSipla.CS_TotalMes + F_ValEfecCon + F_ValCheqCon
          ControlSipla.RT_TotalMes = ControlSipla.RT_TotalMes + F_ValEfeRet  + F_ValCheqRet
          ControlSipla.CS_TotalDia = ControlSipla.CS_TotalDia + F_ValEfecCon + F_ValCheqCon
          ControlSipla.RT_TotalDia = ControlSipla.RT_TotalDia + F_ValEfeRet  + F_ValCheqRet.
END.
IF PCod NE 0 THEN DO: /*graba registro del dia de la transaccion*/
   CtrLavado = YES.
   IF ControlSipla.Fecha LT W_Fecha THEN DO:
       FIND ControlSipla       WHERE
            ControlSipla.Nit   EQ Ahorros.Nit AND
            ControlSipla.Fecha EQ W_Fecha NO-ERROR.
       IF NOT AVAILABLE ControlSipla THEN DO:
          CREATE ControlSipla.
          ASSIGN ControlSipla.Nit         = Ahorros.Nit
                 ControlSipla.Fecha       = W_Fecha
                 ControlSipla.CS_TotalMes = ControlSipla.CS_TotalMes + CS_ValSiplaMes
                 ControlSipla.RT_TotalMes = ControlSipla.RT_TotalMes + RT_ValSiplaMes.
       END.
   END.
   IF MenSipla  NE "" THEN ControlSipla.Id_NUD = YES.
   IF MenSiplaM NE "" THEN ControlSipla.Id_NUM = YES.
   ASSIGN ControlSipla.CS_TotalMes = ControlSipla.CS_TotalMes + F_ValEfecCon + F_ValCheqCon
          ControlSipla.RT_TotalMes = ControlSipla.RT_TotalMes + F_ValEfeRet  + F_ValCheqRet
          ControlSipla.CS_TotalDia = ControlSipla.CS_TotalDia + F_ValEfecCon + F_ValCheqCon
          ControlSipla.RT_TotalDia = ControlSipla.RT_TotalDia + F_ValEfeRet  + F_ValCheqRet.
   /*crea la instancia de mov_inssipla para el encargado de la agencia*/
   FIND Instancias WHERE
        Instancias.Tipo_Instancia EQ 6   AND 
        Instancias.Primera        EQ YES AND
        Instancias.Estado         EQ 1   NO-LOCK NO-ERROR.
   IF AVAILABLE Instancias THEN DO:
      FIND FIRST Cfg_Instancias WHERE 
           Cfg_Instancias.Agencia   EQ Ahorros.Agencia /*W_Agencia*/ AND /*para que la instancia quede*/
           Cfg_Instancias.Instancia EQ Instancias.Instancia AND          /*en la agencia de la cuenta*/
           Cfg_Instancias.Estado    EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE Cfg_Instancias THEN DO:
         FIND BorradorSipla WHERE
              BorradorSipla.Agencia      EQ W_Agencia   AND
              BorradorSipla.Nit          EQ Ahorros.Nit AND
              BorradorSipla.Fecha        EQ W_Fecha     AND
              BorradorSipla.Id_AhoCre    EQ 1           AND
              BorradorSipla.CodAutoriza  EQ PCod NO-LOCK NO-ERROR.
         FIND FIRST Mov_InsSipla WHERE
              Mov_InsSipla.Agencia           EQ Ahorros.Agencia AND
              Mov_InsSipla.Fecha_Transaccion EQ W_Fecha         AND
              Mov_InsSipla.Instancia         EQ Cfg_Instancias.Instancia AND
              Mov_InsSipla.Nit               EQ Ahorros.Nit NO-ERROR.
         IF NOT AVAILABLE Mov_InsSipla THEN DO:
             CREATE Mov_InsSipla.
             ASSIGN Mov_InsSipla.Agencia           = Ahorros.Agencia /*W_Agencia*/
                    Mov_InsSipla.Fecha_Transaccion = W_Fecha
                    Mov_InsSipla.Hora_Transaccion  = TIME
                    Mov_InsSipla.Instancia         = Cfg_Instancias.Instancia
                    Mov_InsSipla.Nit               = Ahorros.Nit
                    Mov_InsSipla.UsuCajero         = W_Usuario
                    Mov_InsSipla.UsuGestiona       = Cfg_Instancias.Usuario.
                    /*Mov_InsSipla.AgeOrigen         = W_Agencia Ahorros.Agencia*/
         END.
         IF AVAILABLE BorradorSipla THEN DO:
             ASSIGN Mov_InsSipla.UsuReporta        = BorradorSipla.Usuario
                    Mov_InsSipla.Id_Exonerada      = BorradorSipla.Id_Exonerada
                    Mov_InsSipla.Id_NUD            = BorradorSipla.Id_NUD
                    Mov_InsSipla.Id_NUM            = BorradorSipla.Id_NUM
                    Mov_InsSipla.Id_RepUIAF        = BorradorSipla.Id_RepFiscalia
                    Mov_InsSipla.Id_Sospechosa     = BorradorSipla.Id_Sospechosa
                    Mov_InsSipla.CodAutoriza       = PCod.
             IF Ahorros.Agencia NE W_Agencia THEN 
                Mov_InsSipla.Descripcion       = Mov_InsSipla.Descripcion + 
                                                 " Ag:" + STRING(W_Agencia,"99") + " - " +
                                                 BorradorSipla.Descripcion.
             ELSE
                 Mov_InsSipla.Descripcion       = Mov_InsSipla.Descripcion + " . " +
                                                  BorradorSipla.Descripcion.
         END.
         ELSE DO:
            MESSAGE "No se encontrador el borrador sipla que permita" SKIP
                    "identificar quien manejo al cliente en la agencia"
                    VIEW-AS ALERT-BOX.
            RETURN ERROR.
         END.
         IF MenSipla  NE "" THEN Mov_InsSipla.Id_NUD = YES.
         IF MenSiplaM NE "" THEN Mov_InsSipla.Id_NUM = YES.
      END.
      ELSE DO:
         MESSAGE "No se ha encontrado un usuario para asignarle" SKIP
                 "la primera instancia del control de lavado de" SKIP
                 "activos.  No  se podra realizar la operacion," SKIP
                 "hasta que no se asigne la instancia a un usuario" VIEW-AS ALERT-BOX.
         CtrLavado = NO.
      END.
   END.
   ELSE DO:
       MESSAGE "No se ha encontrado la primera instancia" SKIP
               "del control de lavado de activos." SKIP
               "la transaccion no se podra realizar!!!" SKIP(1)
               "comuniquese con el depto de sistemas" VIEW-AS ALERT-BOX ERROR.
       CtrLavado = NO.
   END.
END.
RELEASE Mov_InsSipla.
RELEASE ControlSipla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Control_Cheques C-Win 
PROCEDURE Control_Cheques :
/*------------------------------------------------------------------------------
 Purpose: Invocado desde Main-block    
 Notes:  Mayo 16/05 GAER.
------------------------------------------------------------------------------*/
   ASSIGN ValCheq = 0.
   FOR EACH Tmp_Cheques WHERE Tmp_Cheques.W_Cuenta EQ Producto.Cuenta
                        AND   Tmp_Cheques.W_CodPto EQ Producto.CodPto 
                        AND   Tmp_Cheques.W_Dto    EQ Producto.DtoRef NO-LOCK:
       ASSIGN ValCheq = ValCheq + Tmp_Cheques.W_Valor
              W_VrEfectivo:SCREEN-VALUE IN FRAME FRAME_Cheques = STRING(Producto.Debe - ValCheq)
              W_VrEfectivo
              F_Sumatoria:SCREEN-VALUE                          = STRING(ValCheq)
              Producto.ConsigEf                                 = W_VrEfectivo
              Producto.ConsigEf:SCREEN-VALUE IN BROWSE Browse-5 = STRING(W_VrEfectivo).
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-taquilla.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    CtrlFrame-2:NAME = "CtrlFrame-2":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-taquilla.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ctas_Coomeva C-Win 
PROCEDURE Ctas_Coomeva :
/*------------------------------------------------------------------------------
  Purpose: Halla la agencia del cliente que consigna coomeva desde Aportes.
  Notes:  Feb.14/05 GAER.     
------------------------------------------------------------------------------*/
  W_AgAportes = W_Agencia.  /*Si no los halla toma la agencia donde consigna*/
  FIND FIRST Ahorros WHERE Ahorros.Nit         EQ Producto.Nit 
                       AND Ahorros.Cod_Ahorro  EQ 5    /*5-Pdcto aportes*/
                       AND Ahorros.Estado      EQ 1
                       AND Ahorros.Sdo_Disponib + Ahorros.Sdo_Canje GT 0 NO-LOCK NO-ERROR.
  IF AVAIL(Ahorros) THEN
     W_AgAportes = Ahorros.Agencia.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cta_IntTermino C-Win 
PROCEDURE Cta_IntTermino :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE INPUT  PARAMETER I_Pto    LIKE pro_ahorros.tip_ahorro.
   DEFINE INPUT  PARAMETER I_CodPto LIKE ahorros.cod_ahorro.
   DEFINE INPUT  PARAMETER I_Nit    LIKE Clientes.Nit.
   DEFINE OUTPUT PARAMETER I_Cuenta LIKE Cuentas.Cuenta.

   FIND Clientes WHERE Clientes.Nit EQ I_Nit NO-LOCK NO-ERROR.
   IF AVAILABLE(Clientes) THEN DO:
      FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ I_Pto
                     AND   Liqui_Int.Cod_Producto   EQ I_CodPto NO-LOCK NO-ERROR.
      IF AVAILABLE(Liqui_Int) AND Liqui_Int.CtaCr_LiqAso GT " " THEN
         ASSIGN I_Cuenta = Liqui_Int.CtaCr_LiqAso. 
      ELSE DO:
         MESSAGE "Falta Liqui_int con CtaCr_LiqAso."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN ERROR.
      END.
   END.
   ELSE
      RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cta_Sobregiro C-Win 
PROCEDURE Cta_Sobregiro :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Devolver las cuentas de liquidacion de intereses 
                  para el sobregiro.       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT  PARAMETER I_Pto      LIKE pro_ahorros.tip_ahorro.
   DEFINE INPUT  PARAMETER I_CodPto   LIKE ahorros.cod_ahorro.
   DEFINE INPUT  PARAMETER I_Nit      LIKE Clientes.Nit.
   DEFINE OUTPUT PARAMETER C_LiqIntDb LIKE Cuentas.Cuenta.
   DEFINE OUTPUT PARAMETER C_LiqIntCr LIKE Cuentas.Cuenta.
   DEFINE OUTPUT PARAMETER C_LiqMor   LIKE Cuentas.Cuenta.
   DEFINE OUTPUT PARAMETER C_DifCdb   LIKE Cuentas.Cuenta.
   DEFINE OUTPUT PARAMETER C_DifCcr   LIKE Cuentas.Cuenta.

   FIND Clientes WHERE Clientes.Nit EQ I_Nit NO-LOCK NO-ERROR.
   IF AVAILABLE(Clientes) THEN DO:
      FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ I_Pto
                     AND   Liqui_Int.Cod_Producto   EQ I_CodPto NO-LOCK NO-ERROR.
      IF AVAILABLE(Liqui_Int) THEN DO:
/*         IF Clientes.Asociado THEN DO:
            ASSIGN C_LiqIntDb = Liqui_Int.CtaDb_LiqAso
                   C_LiqIntCr = Liqui_Int.CtaCr_LiqAso              
                   C_LiqMor   = Liqui_Int.CtaDb_MoraAso
                   C_DifCdb   = Liqui_Int.CtaDb_DifCobAso
                   C_DifCcr   = Liqui_Int.CtaCr_DifCobAso.
         END.
         ELSE DO:
            ASSIGN C_LiqIntDb = Liqui_Int.CtaDb_Liq
                   C_LiqIntCr = Liqui_Int.CtaCr_Liq           
                   C_LiqMor   = Liqui_Int.CtaDb_Mora
                   C_DifCdb   = Liqui_Int.CtaDb_DifCob
                   C_DifCcr   = Liqui_Int.CtaCr_DifCob.
         END.*/
      END.
      ELSE DO:
         RETURN ERROR.
      END.
   END.
   ELSE DO:
      RETURN ERROR.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cta_Sucursales C-Win 
PROCEDURE Cta_Sucursales :
/*------------------------------------------------------------------------------
  Observaciones :       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT  PARAMETER S_Pto    LIKE pro_ahorros.tip_ahorro.
   DEFINE INPUT  PARAMETER S_CodPto LIKE ahorros.cod_ahorro.
   DEFINE OUTPUT PARAMETER S_Cuenta LIKE Cuentas.Cuenta.
   
   IF S_Pto EQ 3 THEN ASSIGN S_Pto = 1.
   FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ S_Pto
                  AND   Liqui_Int.Cod_Producto   EQ S_CodPto NO-LOCK NO-ERROR.
   IF AVAILABLE(Liqui_Int) THEN
      ASSIGN S_Cuenta = Liqui_Int.Cta_SucyAge.
   ELSE 
      RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cuentas C-Win 
PROCEDURE Cuentas :
DEFINE VAR SW AS LOGICAL.

SW = FALSE.

FOR EACH Cuentas WHERE Cuentas.Cod_FlujoEfec EQ "D"
                   AND Cuentas.Car_Efectivo EQ 2
                   AND Cuentas.Estado EQ  1 NO-LOCK,
    FIRST Operacion WHERE Operacion.Cuenta EQ Cuentas.Cuenta
                      AND Operacion.Clase_Operacion EQ 1
                      AND Operacion.Tipo_Operacion EQ 1
                      AND Operacion.Ctrl_EfeChe EQ 1 
                      AND Operacion.Estado EQ 1 NO-LOCK:
    ASSIGN AuxCaja = Cuentas.Cuenta
           SW = TRUE.
END.

IF SW EQ FALSE THEN DO:
    MESSAGE "No Se Ha Definido la Cuenta de Efectivo Caja Para el Proceso."
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK TITLE "Error en Taquilla.".
END.

SW = FALSE.

FIND FIRST Cuentas WHERE Cuentas.Cod_FlujoEfec EQ "D"
                     AND Cuentas.Car_Efectivo EQ  3
                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
IF AVAIL(Cuentas) THEN DO:
    FIND FIRST Operacion WHERE Operacion.Cuenta EQ Cuentas.Cuenta
                           AND Operacion.Clase_Operacion EQ 1
                           AND Operacion.Tipo_Operacion EQ 1
                           AND Operacion.Ctrl_EfeChe EQ 2
                           AND Operacion.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE operacion THEN
        ASSIGN AuxBanco = Operacion.Cuenta /* "11050505"*/
               SW = TRUE.
END.

  IF SW EQ FALSE THEN DO:
     MESSAGE "No Se Ha Definido la Cuenta de Caja-Cheque Para el Proceso."
     VIEW-AS ALERT-BOX QUESTION BUTTONS OK
     TITLE "Error en Taquilla.".
  END.

  FIND FIRST Cuentas WHERE Cuentas.Id_Caja  EQ TRUE
                     AND   Cuentas.Cod_Caja EQ 3 
                     AND   Cuentas.Estado   EQ 1 NO-LOCK NO-ERROR.
  IF AVAILABLE(Cuentas) THEN DO:
     ASSIGN AuxTrasl = Cuentas.Cuenta.
  END.
  ELSE DO:
     MESSAGE "No Se Ha Definido la Cuenta de Transferencias Para el Proceso."
     VIEW-AS ALERT-BOX QUESTION BUTTONS OK
     TITLE "Error en Taquilla.".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Desactivar C-Win 
PROCEDURE Desactivar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN BROWSE-5:SENSITIVE     = FALSE
            Btn_Cancelar:SENSITIVE = FALSE
            Btn_Grabar:SENSITIVE   = FALSE
            Btn_ImpSaldo:SENSITIVE = FALSE
            Btn_Salir:SENSITIVE    = FALSE
            F_Agencia:SENSITIVE    = FALSE
            Cmb_Tipo:SENSITIVE       = FALSE.
  END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Diferencia_VrEfectivo C-Win 
PROCEDURE Diferencia_VrEfectivo :
/*------------------------------------------------------------------------------
  Purpose: Invocado desde Main-block    
  Notes:  Mayo 16/05 GAER.
------------------------------------------------------------------------------*/
  ASSIGN W_VrEfectivo:SCREEN-VALUE IN FRAME FRAME_Cheques = STRING(Producto.Debe - ValCheq)
         W_VrEfectivo.
      
  IF ValCheq LT Producto.Debe AND Cheques.W_Valor NE 0 THEN DO:
     FIND LAST Tmp_Cheques NO-LOCK NO-ERROR.
     IF Cheques.W_Secue EQ Tmp_Cheques.W_Secue THEN DO:
        RUN Insert_Cheque.                                            
        ASSIGN Cheques.W_Valor:SCREEN-VALUE IN BROWSE BROWSE-7 = "0". 
        APPLY "ENTRY" TO Cheques.W_Canje IN BROWSE BROWSE-7.                 
        RETURN NO-APPLY.                                                     
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dig_Chequeo C-Win 
PROCEDURE Dig_Chequeo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE INPUT  PARAMETER C_Agencia LIKE Ahorros.Agencia.
   DEFINE INPUT  PARAMETER C_ProDig  LIKE Pro_Ahorros.Pro_Digito.
   DEFINE INPUT  PARAMETER C_Cuenta  LIKE Ahorros.Cue_Ahorros.
   DEFINE OUTPUT PARAMETER C_Digito  AS   INTEGER.
   
   ASSIGN C_Digito = 0.
   IF C_ProDig GT 0 THEN DO:
      FIND Formatos WHERE Formatos.Agencia     EQ C_Agencia
                    AND   Formatos.Cod_Formato EQ C_ProDig 
                    AND   Formatos.Id_Formato  EQ "AC" NO-LOCK NO-ERROR.
      IF AVAILABLE(Formatos) THEN DO:
         RUN VALUE(Formatos.Nom_Proceso) (INPUT-OUTPUT C_Cuenta, INPUT-OUTPUT C_Digito).
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Documento C-Win 
PROCEDURE Documento :
/*------------------------------------------------------------------------------
  Observaciones : Rutina Para el Documento Referencia...       
------------------------------------------------------------------------------*/
   ASSIGN BROWSE BROWSE-5 Producto.DtoRef.

   /*GCamacho May04/08 - Implementación Ahorro Permanente*/
   IF producto.TipPto EQ "1-2" AND CodPto EQ 221 THEN DO:
       MESSAGE "No se pueden hacer Retiros para esta cuenta" SKIP
           "Por esta opción"
                     VIEW-AS ALERT-BOX ERROR TITLE "Retiro Bloqueado".
       ASSIGN Producto.Haber = 0
              Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
       RETURN ERROR.
   END.
   /************************************************************/
    
   IF Producto.Haber GT 0 AND Producto.EstCta GT 2 AND Producto.EstCta NE 10 THEN DO:
      FIND Varios WHERE Varios.Tipo   EQ 21                                               
                    AND Varios.Codigo EQ Producto.EstCta NO-LOCK NO-ERROR.                   
      IF AVAILABLE Varios THEN DO:                                                           
         MESSAGE "No se pueden hacer Retiros para esta cuenta" SKIP                          
                       "El estado de esta cuenta es de bloqueo por: " Varios.Descripcion     
                       VIEW-AS ALERT-BOX ERROR.                                            
         ASSIGN Producto.Haber = 0                                                           
                Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".                        
         RETURN ERROR.                                                                       
      END.                                                                                         
   END.

   IF Producto.Id_Tal GE 1 AND Producto.Id_Tal LE 2 AND Producto.DtoRef NE ""
   AND Producto.Haber GT 0 THEN DO:
      ASSIGN LibChe = FALSE.
      FIND FIRST Lib_Chequera WHERE Lib_Chequera.Agencia    EQ Producto.OfiTem 
                            AND   Lib_Chequera.Cod_Producto EQ Producto.CodPto 
                            AND   Lib_Chequera.Cue_Ahorros  EQ Producto.Cuenta
                            AND   Lib_Chequera.Estado       EQ 1 NO-LOCK NO-ERROR.
      ASSIGN LibChe = TRUE. /*quitar en produccion temporal */
      IF AVAIL(Lib_Chequera) AND INTEGER(Producto.DtoRef) GE Lib_Chequera.Num_Inicial AND
                                 INTEGER(Producto.DtoRef) LE Lib_Chequera.Num_Final   AND
                                 INTEGER(Producto.DtoRef) GT Lib_Chequera.Ult_Consec      THEN 
         ASSIGN LibChe = TRUE.
      
      IF LibChe = FALSE THEN DO:
         MESSAGE "El Documento Referencia Digitado No Corresponde Con" SKIP
                 "El Rango de Valores Permitidos Para la Libreta o "   SKIP
                 "Chequera del Producto."
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
         TITLE "Error En Taquilla".
         ASSIGN Producto.DtoRef = " "
                Producto.DtoRef:SCREEN-VALUE IN BROWSE BROWSE-5 = " "
                Producto.Haber = 0
                Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
          RETURN ERROR.      
      END.
   END.   
   
   IF F_Tipo EQ 3 THEN DO:
      ASSIGN Producto.DtoRef:SCREEN-VALUE IN BROWSE BROWSE-5  = "C" + TRIM(Producto.DtoRef).
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Duplicado C-Win 
PROCEDURE Duplicado :
/*------------------------------------------------------------------------------
  OBSERVACIONES :        
------------------------------------------------------------------------------*/
  DEFINE VAR Old-Num   AS   INTEGER NO-UNDO.
  
  DEFINE VAR P_TipPto  AS   CHARACTER FORMAT "X(3)".
  DEFINE VAR P_CodPto  LIKE ahorros.cod_ahorro.
  DEFINE VAR P_NomPto  LIKE Pro_Ahorros.Nom_Producto.
  DEFINE VAR P_OfiTem  LIKE Agencias.Agencia.
  DEFINE VAR P_Cuenta  AS   CHARACTER FORMAT "X(14)".
  
  IF Producto.TipPto EQ "" AND Producto.Debe EQ 0 AND Producto.Haber EQ 0 THEN DO:
     APPLY "ENTRY" TO Producto.Cuenta IN BROWSE BROWSE-5.
     RETURN NO-APPLY.
  END.
  ELSE DO:
    ASSIGN BROWSE BROWSE-5 
           Producto.NomPto
           Producto.OfiTem
           Producto.Cuenta.
    ASSIGN P_TipPto = Producto.TipPto
           P_CodPto = Producto.CodPto
           P_NomPto = Producto.NomPto
           P_OfiTem = Producto.OfiTem
           P_Cuenta = Producto.Cuenta.
    IF NOT AVAILABLE Producto THEN 
       FIND LAST Producto NO-ERROR.
    IF AVAILABLE Producto THEN 
       ASSIGN Old-Num = Producto.W_Order.
    CREATE Producto.
    ASSIGN Producto.W_Order = Old-Num
           Producto.TipPto  = P_TipPto
           Producto.CodPto  = P_CodPto
           Producto.NomPto  = P_NomPto
           Producto.OfiTem  = P_OfiTem
           Producto.Cuenta  = P_Cuenta.
    RUN Reorder-Browse.
    ASSIGN Open-Recid  = RECID(Producto)
           Open-On-Row = Open-On-Row + 1. 
    RUN Reopen-Query.
    APPLY "ENTRY" TO Producto.Cuenta IN BROWSE BROWSE-5.
    RETURN NO-APPLY.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY CMB_Tipo F_Nit F_Agencia Com_Producto F_NomAgencia F_Cuenta F_Chequeo 
          F_Descripcion F_Nombre F_ValCheqCon F_ValCheqret F_Consigna F_Retiro 
          F_ValEfecCon F_ValEfeRet F_Total 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Btn_ReValid Imp_Cdat btn_autorizados Btn_ImpSaldo BUTTON-1 CMB_Tipo 
         Btn_Descripcion BROWSE-5 Btn_Grabar Btn_Cancelar Btn_Salir Btn_Ayuda 
         BUTTON-58 RECT-139 RECT-227 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE P_Foto BUTTON-230 
      WITH FRAME F_Foto IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Foto}
  ENABLE BROWSE-12 BUTTON-183 Btn_ReVal 
      WITH FRAME F_ReVal IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_ReVal}
  ENABLE SalirPDF 
      WITH FRAME FormatoEfectivo IN WINDOW C-Win.
  VIEW FRAME FormatoEfectivo IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FormatoEfectivo}
  ENABLE BROWSE-Detalle BUTTON-60 
      WITH FRAME FRAME-Detalle IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Detalle}
  DISPLAY W_CedTrans W_NomTx 
      WITH FRAME F_Trans IN WINDOW C-Win.
  ENABLE BUTTON-121 W_CedTrans W_NomTx 
      WITH FRAME F_Trans IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Trans}
  DISPLAY W_CtaCC F_NitCuenta F_NomNit 
      WITH FRAME Frame-Cuentas IN WINDOW C-Win.
  ENABLE BROWSE-9 BUTTON-59 F_NitCuenta 
      WITH FRAME Frame-Cuentas IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame-Cuentas}
  DISPLAY F_NroCheque F_Beneficiario 
      WITH FRAME Frame-Cuentas-Bancos IN WINDOW C-Win.
  ENABLE BROWSE-10 F_NroCheque F_Beneficiario 
      WITH FRAME Frame-Cuentas-Bancos IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame-Cuentas-Bancos}
  DISPLAY W_VrEfectivo F_Sumatoria 
      WITH FRAME Frame_Cheques IN WINDOW C-Win.
  ENABLE BROWSE-7 BUTTON-174 Btn_SalPro 
      WITH FRAME Frame_Cheques IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame_Cheques}
  DISPLAY W_NroTx 
      WITH FRAME F_RevTx IN WINDOW C-Win.
  ENABLE W_NroTx BUTTON-123 
      WITH FRAME F_RevTx IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_RevTx}
  DISPLAY F_SDisponible1 F_Vlrsobregiro1 F_SCanje1 F_FecCancela1 F_SdoTot 
          F_FecProxLiq1 F_SIntPagados1 F_FecUltLiq1 F_Cuota1 F_Plazo1 
          F_Intsobregiro1 F_Intporpagar1 F_DetalleEstado 
      WITH FRAME Frame-Ahorros IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame-Ahorros}
  DISPLAY F-Vlracumpagos3 F-Forpago3 F-Sdopendiente3 F-Periodo3 F-Cuota3 
          F-CargoInicial3 F-Cantidad3 F-FecCargo3 F-Cuopagadas3 F-Fecultpag3 
          F-Plazo3 F-Secuencia3 
      WITH FRAME Frame-Especiales IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame-Especiales}
  DISPLAY F_NomCuenta Rad-Nat Rad-TipoCaja T_Dto T_Caja T_ManNit 
      WITH FRAME Frame-CarCue IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame-CarCue}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Estado_Pto C-Win 
PROCEDURE Estado_Pto :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permite Validar el Estado de los Productos de Ahorros Antes
                  del Proceso de Grabación.
------------------------------------------------------------------------------*/
   /*
   DEFINE VAR E_ValDif AS DECIMAL DECIMALS 0 FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99".
   
   FIND FIRST Ahorros WHERE Ahorros.Agencia      EQ Producto.OfiTem 
                      AND   ahorros.cod_ahorro EQ Producto.CodPto
                      AND   Ahorros.Cue_Ahorros  EQ Producto.Cuenta 
                      AND   Ahorros.Nit          EQ F_Nit 
                      AND   Ahorros.Estado       EQ 1 NO-LOCK NO-ERROR.
   IF AVAILABLE (Ahorros) THEN DO:
      IF Ahorros.Clase_Operacion  EQ "BLOQ" AND
         Ahorros.Codigo_Operacion EQ 1      THEN DO:
         IF Producto.Debe GT 0 THEN DO:
            MESSAGE "Importante... La Cuenta Esta Bloqueada Por Inactividad."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Información Para la Taquilla".
         END.
         ELSE
         IF Producto.Haber GT 0 THEN DO:
            MESSAGE "Importante... La Cuenta Esta Bloqueada Por Inactividad." SKIP
                    "Desea Autorizar la Transacción ...?"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
            IF Sw_Estado EQ TRUE THEN DO:
               RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
               IF W_Error EQ FALSE THEN DO:
                  RETURN ERROR.
               END.
            END.
            ELSE DO:
               RETURN ERROR.
            END.
         END.
      END.   
      ELSE
      IF Ahorros.Clase_Operacion  EQ "BLOQ" AND
         Ahorros.Codigo_Operacion EQ 2      THEN DO:
         IF Producto.Debe GT 0 THEN DO:
            MESSAGE "Importante... La Cuenta Esta Bloqueada Por Morosidad."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Información Para la Taquilla".
         END.
         ELSE
         IF Producto.Haber GT 0 THEN DO:
            MESSAGE "Importante... La Cuenta Esta Bloqueada Por Morosidad." SKIP
                    "Desea Autorizar la Transacción ...?"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
            TITLE "Error En Taquilla" UPDATE Sw_Estado.
            IF Sw_Estado EQ TRUE THEN DO:
               RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
               IF W_Error EQ FALSE THEN DO:
                  RETURN ERROR.
               END.
            END.
            ELSE DO:
               RETURN ERROR.
            END.
         END.   
      END.      
      ELSE
      IF Ahorros.Clase_Operacion  EQ "BLOQ" AND
         Ahorros.Codigo_Operacion EQ 3      THEN DO:
         IF Producto.Debe GT 0 THEN DO:
            MESSAGE "Importante... La Cuenta Esta Bloqueada " SKIP
                    "Por Perdida de Libreta o Chequera."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Información Para la Taquilla".
         END.
         ELSE
         IF Producto.Haber GT 0 THEN DO:
            MESSAGE "Importante... La Cuenta Esta Bloqueada " SKIP
                    "Por Perdida de Libreta o Chequera."      SKIP
                    "Desea Autorizar la Transacción ...?"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
            TITLE "Error En Taquilla" UPDATE Sw_Estado.
            IF Sw_Estado EQ TRUE THEN DO:
               RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
               IF W_Error EQ FALSE THEN DO:
                  RETURN ERROR.
               END.
            END.
            ELSE DO:
               RETURN ERROR.
            END.
         END.   
      END.
      ELSE
      IF Ahorros.Clase_Operacion  EQ "EMBG" AND
         Ahorros.Codigo_Operacion EQ 1      THEN DO:
         IF Producto.Debe GT 0 THEN DO:
            MESSAGE "Importante... La Cuenta Esta Embargada" SKIP
                    "Por Oficio de Juzgado."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Información Para la Taquilla".
         END.
         ELSE
         IF Producto.Haber GT 0 THEN DO:
            FIND Hoja_Pro WHERE Hoja_Pro.Agencia        EQ Ahorros.Agencia
                          AND   Hoja_Pro.Nit            EQ Ahorros.Nit
                          AND   Hoja_Pro.Cta_Producto   EQ Ahorros.Cue_Ahorros
                          AND   Hoja_Pro.Clase_Producto EQ 1
                          AND   Hoja_Pro.Cod_Producto   EQ ahorros.cod_ahorro
                          AND   Hoja_Pro.Clase_Asunto   EQ Ahorros.Clase_Operacion
                          AND   Hoja_Pro.Codigo_Asunto  EQ Ahorros.Codigo_Operacion
                          NO-LOCK NO-ERROR.
            IF AVAILABLE(Hoja_Pro) THEN DO:
               ASSIGN E_ValDif = Ahorros.Sdo_Disponible - Hoja_Pro.Valor - Producto.Haber.
               IF E_ValDif GE 0 THEN DO:
                  MESSAGE "Importante... La Cuenta Esta Embargada" SKIP
                          "Por Oficio de Juzgado."
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                  TITLE "Información Para la Taquilla".
               END.
               ELSE DO:
                  ASSIGN E_ValDif = Ahorros.Sdo_Disponible - Hoja_Pro.Valor.
                  IF E_ValDif LT 0 THEN ASSIGN E_ValDif = 0.
                  MESSAGE "Importante... La Cuenta Esta Embargada" SKIP
                          "Por Oficio de Juzgado. El Valor Máximo" SKIP
                          "Que Puede Retirar Es: " E_ValDif
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                  TITLE "Error en Taquilla".
                  RETURN ERROR.
               END.
            END.
            ELSE DO:
               MESSAGE "Importante... La Cuenta Esta Embargada" SKIP
                       "Por Oficio de Juzgado."
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
               TITLE "Error en Taquilla".
               RETURN ERROR.
            END.
         END.
      END.
      ELSE
      IF Ahorros.Clase_Operacion  EQ "ENDS" AND
         Ahorros.Codigo_Operacion EQ 1      THEN DO:
         IF Producto.Debe GT 0 THEN DO:
            MESSAGE "Importante... La Cuenta Esta Endosada."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Información Para la Taquilla".
         END.
         ELSE
         IF Producto.Haber GT 0 THEN DO:
            MESSAGE "Importante... La Cuenta Esta Endosada."
                    "Desea Autorizar la Transacción ...?"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
            TITLE "Error En Taquilla" UPDATE Sw_Estado.
            IF Sw_Estado EQ TRUE THEN DO:
               RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
               IF W_Error THEN DO:
                  FIND Hoja_Pro WHERE Hoja_Pro.Agencia        EQ Ahorros.Agencia
                                AND   Hoja_Pro.Nit            EQ Ahorros.Nit
                                AND   Hoja_Pro.Cta_Producto   EQ Ahorros.Cue_Ahorros
                                AND   Hoja_Pro.Clase_Producto EQ 1
                                AND   Hoja_Pro.Cod_Producto   EQ ahorros.cod_ahorro
                                AND   Hoja_Pro.Clase_Asunto   EQ Ahorros.Clase_Operacion
                                AND   Hoja_Pro.Codigo_Asunto  EQ Ahorros.Codigo_Operacion
                                NO-LOCK NO-ERROR.
                  IF AVAILABLE(Hoja_Pro) THEN DO:
                     ASSIGN E_ValDif = Ahorros.Sdo_Disponible - Hoja_Pro.Valor - Producto.Haber.
                     IF E_ValDif LT 0 THEN DO:
                        ASSIGN E_ValDif = Ahorros.Sdo_Disponible - Hoja_Pro.Valor.
                        IF E_ValDif LT 0 THEN ASSIGN E_ValDif = 0.
                        MESSAGE "Importante... El Valor Máximo Que" SKIP
                                "Puede Retirar de la Cuenta Es: " E_ValDif
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                        TITLE "Error en Taquilla".
                        RETURN ERROR.
                     END.
                  END.
                  ELSE DO:
                     RETURN ERROR.
                  END.
               END.
               ELSE DO:
                  RETURN ERROR.
               END.
            END.
            ELSE DO:
               RETURN ERROR.
            END.
         END.
      END.
      ELSE
      IF Ahorros.Clase_Operacion  EQ "ENDS" AND
         Ahorros.Codigo_Operacion EQ 2      THEN DO:
         IF Producto.Debe GT 0 THEN DO:
            MESSAGE "Importante... La Cuenta Esta Pignorada" SKIP
                    "Endoso En Garantía."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Información Para la Taquilla".
         END.
         ELSE
         IF Producto.Haber GT 0 THEN DO:
            FIND Hoja_Pro WHERE Hoja_Pro.Agencia        EQ Ahorros.Agencia
                          AND   Hoja_Pro.Nit            EQ Ahorros.Nit
                          AND   Hoja_Pro.Cta_Producto   EQ Ahorros.Cue_Ahorros
                          AND   Hoja_Pro.Clase_Producto EQ 1
                          AND   Hoja_Pro.Cod_Producto   EQ ahorros.cod_ahorro
                          AND   Hoja_Pro.Clase_Asunto   EQ Ahorros.Clase_Operacion
                          AND   Hoja_Pro.Codigo_Asunto  EQ Ahorros.Codigo_Operacion
                          NO-LOCK NO-ERROR.
            IF AVAILABLE(Hoja_Pro) THEN DO:
               IF Producto.Retiro THEN DO: /* Es cancelacion total cap + int */
                 MESSAGE "Importante... La Cuenta Esta Pignorada - Endoso En Garantía."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                    TITLE "Información En Taquilla".
                 IF SUBSTRING(Producto.TipPto,1,1) = "1" AND
                    SUBSTRING(Producto.TipPto,3,3) EQ "3" THEN DO: /* Si es <> Termino */
                   MESSAGE "El tipo de producto es un ahorro a Término, por lo tanto únicamente " SKIP
                           "se puede retirar los intereses. por Favor rectifique su opción."
                     VIEW-AS ALERT-BOX TITLE "Validación. (Estado_Pto)".
                   RETURN ERROR.
                 END.
                 ASSIGN E_ValDif = Ahorros.Sdo_Disponible - Hoja_Pro.Valor - Producto.Haber.
                 IF E_ValDif LT 0 THEN DO:
                    ASSIGN E_ValDif = Ahorros.Sdo_Disponible - Hoja_Pro.Valor.
                    IF E_ValDif LT 0 THEN ASSIGN E_ValDif = 0.
                    MESSAGE "Importante... El Valor Máximo Que" SKIP
                            "Puede Retirar de la Cuenta Es: " E_ValDif
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                    TITLE "Error en Taquilla".
                    RETURN ERROR.
                 END.
               END.
               ELSE DO:
                 IF SUBSTRING(Producto.TipPto,1,1) = "1" THEN DO: /* Si es Ahorro */
                    IF SUBSTRING(Producto.TipPto,3,3) NE "3" THEN DO: /* Si es <> Termino */
                        MESSAGE "Importante... La Cuenta Esta Pignorada - Endoso En Garantía."
                          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                          TITLE "Información En Taquilla".
                        ASSIGN E_ValDif = Ahorros.Sdo_Disponible - Hoja_Pro.Valor - Producto.Haber.
                        IF E_ValDif LT 0 THEN DO:
                           ASSIGN E_ValDif = Ahorros.Sdo_Disponible - Hoja_Pro.Valor.
                           IF E_ValDif LT 0 THEN ASSIGN E_ValDif = 0.
                           MESSAGE "Importante... El Valor Máximo Que" SKIP
                                   "Puede Retirar de la Cuenta Es: " E_ValDif
                           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                           TITLE "Error en Taquilla".
                           RETURN ERROR.
                        END.
                    END.
                 END.
               END.
            END.
            ELSE DO:
               RETURN ERROR.
            END.
         END.
      END.
      ELSE
      IF Ahorros.Clase_Operacion  EQ "ENDS" AND
         Ahorros.Codigo_Operacion EQ 3      THEN DO:
         IF Producto.Debe GT 0 THEN DO:
            MESSAGE "La Cuenta Esta Endosada Como Fuente de Pago."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Información Para la Taquilla".
         END.
         ELSE
         IF Producto.Haber GT 0 THEN DO:
            MESSAGE "La Cuenta Esta Endosada Como Fuente de Pago." SKIP
                    "Desea Autorizar la Transacción ...?"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
            TITLE "Error En Taquilla" UPDATE Sw_Estado.
            IF Sw_Estado EQ TRUE THEN DO:
               RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
               IF W_Error EQ FALSE THEN DO:
                  RETURN ERROR.
               END.
            END.
            ELSE DO:
               RETURN ERROR.
            END.
         END.
      END.
      ELSE
      IF Ahorros.Clase_Operacion  EQ "NORM" AND
         Ahorros.Codigo_Operacion EQ 1      AND 
         Producto.Haber           NE 0      THEN DO:
         MESSAGE "La Cuenta Esta en Estado No Consignado." SKIP
                 "Se Debe Efectuar la Apertura Para Poder" SKIP
                 "Hacer el Retiro."
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
         TITLE "Error En Taquilla".
         RETURN ERROR.
      END.
   END.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Find2_SdoFinal C-Win 
PROCEDURE Find2_SdoFinal :
RUN HallarSdoTercero IN W_Manija (INPUT X_Nit,
                                  INPUT Producto.OfiTem,
                                  INPUT Producto.OfiTem,
                                  INPUT 0,
                                  INPUT 999,
                                  INPUT Producto.Cuenta,
                                  INPUT Xan,
                                  INPUT Xms,
                                  OUTPUT Xdb,
                                  OUTPUT Xcr,
                                  OUTPUT Xsd).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Find_SdoFinal C-Win 
PROCEDURE Find_SdoFinal :
DEFINE VAR X_Cerrado AS LOGICAL INITIAL FALSE.

ASSIGN Xms = MONTH(W_Fecha)
       Xan =  YEAR(W_Fecha)
       Xsd = 0.
    
ASSIGN FRAME {&FRAME-NAME} F_Agencia.
    
FIND FIRST Cuentas WHERE Cuentas.Cuenta = Producto.Cuenta NO-LOCK NO-ERROR.
    
RUN Find2_SdoFinal.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gabar_TarDeb_old C-Win 
PROCEDURE Gabar_TarDeb_old :
/*------------------------------------------------------------------------------
  Purpose:   Graba movimientos de Tarjeta debito  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER wvlrTrans  LIKE ahorros.sdo_disponible.
DEFINE INPUT PARAMETER wdesTrans AS CHARACTER.
DEFINE INPUT PARAMETER wtipoTrans AS INTEGER.
DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wretcode      AS INTEGER INITIAL -1.

IF wtipotrans NE 0 THEN DO:
   FIND CURRENT ahorros  NO-ERROR.
   CREATE tarjetadebito.
   ASSIGN tarjetadebito.Agencia         = w_agencia        /* mov_contable.agencia */
          tarjetadebito.Usuario         = w_usuario        /* mov_contable.usuario      */
          tarjetadebito.Comprobante     = cbte             /* mov_contable.comprobante   */
          tarjetadebito.Num_Documento   = w_docContab      /* mov_contable.Num_Documento */
          tarjetadebito.Fec_Contable    = TODAY            /* mov_contable.Fec_Contable */
          tarjetadebito.Hora            = TIME             /* mov_contable.Hora */
          tarjetadebito.Comentario      = wdesTrans
          tarjetadebito.Aplicado        = NO
          tarjetadebito.ManBiometrico   = 1
          tarjetadebito.TipoTransaccion = wtipotrans
          tarjetadebito.Nit             = Ahorros.nit
          tarjetadebito.Cue_Ahorros     = Ahorros.cue_ahorros  /* SUBSTRING(Ahorros.TarjetaDB,10,7) */
          tarjetadebito.TarjetaDB       = Ahorros.TarjetaDB
          tarjetadebito.Monto           = wvlrTrans
          tarjetadebito.Secuencia       = "000000000000"
          tarjetadebito.SdoTotal        = 0
          tarjetadebito.SdoDispon       = 0
          tarjetadebito.RetCode         = -1.
   wvlrmonTD = STRING(wvlrtrans,'9999999999999').
   RUN TranWebCaja.P(1,wtipotrans,TRIM(ahorros.nit), TRIM(ahorros.cue_ahorros), ahorros.TarjetaDB, wvlrmonTD, INPUT-OUTPUT wsecTD, INPUT-OUTPUT wsdoTD, INPUT-OUTPUT wdisTD, OUTPUT wretcode).
   IF wretcode = 0 THEN 
      ASSIGN tarjetadebito.Secuencia       = wsecTD 
             tarjetadebito.SdoTotal        = ROUND(decimal(wsdoTD) / 100,2)
             tarjetadebito.SdoDispon       = ROUND(decimal(wdisTD) / 100,2)
             tarjetadebito.RetCode         = wretcode
             tarjetadebito.Aplicado        = YES.
   ELSE 
      ASSIGN tarjetadebito.RetCode         = wretcode.
   RELEASE tarjetadebito.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar C-Win 
PROCEDURE Grabar :
DEFINE VAR I AS INTEGER.
DEFI VAR W_Sw1o AS LOG INIT FALSE.

EMPTY TEMP-TABLE ttProducto.

DO WITH FRAME {&FRAME-NAME}:
    W_NumSeq = NEXT-VALUE(Sec_Taquilla).

    Trantaq:
    DO TRANSACTION ON ERROR UNDO TranTaq
                   ON ENDKEY UNDO TranTaq
                   ON STOP UNDO TranTaq:
        ASSIGN FRAME DEFAULT-FRAME Cmb_Tipo
                                   F_Consigna
                                   F_Retiro.

        F_Tipo = INTEGER(SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,2)).

        IF F_Tipo = 5 THEN DO:
            Cta_Caja = AuxTrasl.

            IF F_Consigna <> F_Retiro THEN DO:
                MESSAGE "El total de consignaciones y retiros deben" SKIP
                        "ser iguales para efectuar el traslado."
                    VIEW-AS ALERT-BOX QUESTION BUTTONS OK TITLE "Error en Cuentas".

                RETURN ERROR.
            END.
        END.
        ELSE 
            ASSIGN Cta_Caja = AuxCaja
                   Cta_Banco = AuxBanco.

        /* Revisamos si es una consignación a Aportes, para partir la consignación entre Aportes y Ahorro Permanente */
        FOR EACH Producto WHERE Producto.EC <> " "
                            AND Producto.Debe > 0
                            AND ((producto.tipPto = "1-4" AND producto.codPto = 2) OR (Producto.tipPto = "1-2" AND producto.codPto = 3)):
            IF producto.tipPto = "1-4" THEN DO:
                FIND FIRST ahorros WHERE ahorros.nit = producto.nitCta
                                     AND ahorros.tip_ahorro = 2
                                     AND ahorros.cod_ahorro = 3
                                     AND ahorros.estado = 1 NO-LOCK NO-ERROR.
                IF AVAILABLE ahorros THEN DO:
                    CREATE ttProducto.
                    BUFFER-COPY producto TO ttProducto.
                    ttProducto.tipPto = "1-2".
                    ttProducto.codPto = 3.
                    ttProducto.cuenta = ahorros.cue_ahorro.

                    ttProducto.debe = ROUND(producto.debe * 0.2,0).
                    producto.debe = producto.debe - ttProducto.debe.
                END.
            END.

            IF producto.tipPto = "1-2" THEN DO:
                FIND FIRST ahorros WHERE ahorros.nit = producto.nitCta
                                     AND ahorros.tip_ahorro = 4
                                     AND ahorros.cod_ahorro = 2
                                     AND ahorros.estado = 1 NO-LOCK NO-ERROR.
                IF AVAILABLE ahorros THEN DO:
                    CREATE ttProducto.
                    BUFFER-COPY producto TO ttProducto.
                    ttProducto.tipPto = "1-4".
                    ttProducto.codPto = 2.
                    ttProducto.cuenta = ahorros.cue_ahorro.

                    ttProducto.debe = ROUND(producto.debe * 0.8,0).
                    producto.debe = producto.debe - ttProducto.debe.
                END.
            END.
        END.

        FOR EACH ttProducto NO-LOCK:
            CREATE producto.
            BUFFER-COPY ttProducto TO producto.
        END.
        /* --------------------------------------------------------------------------------------------------------- */
        

        FOR EACH Producto WHERE Producto.EC <> " "
                            AND (Producto.Debe > 0 OR Producto.Haber > 0) NO-LOCK BREAK BY Producto.OfiTem:
            IF Producto.Cuenta <= " " OR
               (Cta_Caja <= " " AND Producto.EC = "E") OR
               (Cta_Banco <= " " AND Producto.EC = "C") OR
               (Cta_Banco = ? AND Producto.EC = "C") THEN DO:
                MESSAGE "Falta alguna de las Cuentas Contables, " SKIP
                        "Cuenta prinicipal  :" Producto.Cuenta SKIP
                        "Contrapartida Caja :" Cta_Caja SKIP
                        "Contrapartida Banco:" Cta_Banco SKIP
                        "para la contabilización. Revise por favor."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.

            IF FIRST-OF(Producto.OfiTem) AND NOT W_Sw1o THEN DO:
                Cbte = 1.

                IF Producto.Haber > 0 THEN
                    Cbte = 2.

                FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                                          AND Comprobantes.Comprobante EQ Cbte NO-ERROR.
                IF AVAIL(Comprobantes) THEN DO:
                    FIND FIRST mov_contable WHERE mov_contable.agencia = comprobantes.agencia
                                              AND mov_contable.comprobante = comprobantes.comprobante
                                              AND mov_contable.num_documento = comprobantes.secuencia NO-LOCK NO-ERROR.
                    IF AVAILABLE mov_contable THEN
                        Comprobantes.Secuencia = Comprobantes.Secuencia + 1.

                    ASSIGN W_DocContab = Comprobantes.Secuencia
                           W_Sw1o = TRUE.

                    FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
                END.
                ELSE DO:
                    RUN MostrarMensaje IN W_Manija (INPUT 143,
                                                    OUTPUT W_Error).

                    RETURN ERROR.
                END.
            END.

            ASSIGN T_SdoI = 0
                   T_SdoF = 0.

            CASE SUBSTRING(Producto.TipPto,1,1):
                WHEN "" THEN DO:
                    RUN Gra_Cuentas NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        RETURN ERROR.
                END.

                WHEN "1" THEN DO:
                    RUN Operacion_Ahorros NO-ERROR.

                    /* oakley */

                    IF ERROR-STATUS:ERROR THEN
                        RETURN ERROR.
                     
                    T_SdoF = Ahorros.Sdo_disponible.
                END.

                WHEN "4" THEN DO:
                    RUN Gra_Especiales NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        RETURN ERROR.
                END.
            END CASE.
        END.

        RUN Contabilizar NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            FOR EACH Tmp-tarjetadb:
                DELETE Tmp-tarjetadb.
            END.

            MESSAGE "Ha habido un error en la contabilización" SKIP
                    "del Documento. Se devuelve la transaccion" SKIP
                    "De contabilización de la Taquilla"
                VIEW-AS ALERT-BOX.

            RETURN ERROR.
        END.

        W_SecuenciaImprimir = W_NumSeq.
    END.

    W_NumSeq = 0.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Deducible C-Win 
PROCEDURE Grabar_Deducible :
DEFINE VAR Vr_Impuesto LIKE Ahorros.Sdo_Disponible.
DEFINE VAR OpRet LIKE Operacion.Cod_Operacion.

IF Vr_Deducible GT 0 THEN DO:
    RUN Buscar_Cuenta(INPUT 1,
                      INPUT Producto.CodPto,
                      INPUT Ahorros.Plazo,
                      INPUT Ahorros.Nit,
                      OUTPUT CtaCble,
                      OUTPUT CtaSYA_Des,
                      OUTPUT CtaSYA_FTE) NO-ERROR.
    IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).

        RETURN ERROR.
    END.

    RUN Buscar_OpRetiro (OUTPUT OpRet).
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "No se encontro la Operación de Retiro" SKIP
                "Para el correspondiente asiento de los" SKIP
                "Deducibles e impuestos" SKIP
                "Se cancela la operacion"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    RUN Gra_MovAhorros(INPUT OpRet,
                       INPUT Producto.CodPto,
                       INPUT Producto.Cuenta,
                       INPUT Producto.DtoRef,
                       INPUT Producto.OfiTem,
                       INPUT W_Agencia,
                       INPUT Producto.OfiTem,
                       INPUT Producto.UsuAut,
                       INPUT 0,
                       INPUT Vr_Deducible,
                       INPUT Ahorros.Nit).

    Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.

    movProductos.sdo_disponible = Vr_Deducible.
    movProductos.tipo_transaccion = 2.

    IF PRoducto.OfiTem EQ W_Agencia THEN
        RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT OpRet,            INPUT Producto.CodPto,  INPUT CtaCble,      INPUT T_Ctd,            INPUT "DB",
                         INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                         INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT Vr_Deducible,     INPUT F_Seg).
    ELSE DO:
        RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT OpRet,            INPUT Producto.CodPto,  INPUT CtaSYA_FTE,   INPUT T_Ctd,            INPUT "DB",
                         INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                         INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT Vr_Deducible,     INPUT F_Seg).

        RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT OpRet,            INPUT Producto.CodPto,  INPUT CtaCble,          INPUT CtaSYA_DES,       INPUT "DB",
                         INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT Producto.OfiTem,  INPUT Producto.OfiTem,  INPUT W_Agencia,
                         INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT Vr_Deducible,     INPUT F_Seg).
    END.

    IF T_CIm NE "" AND T_VIm NE 0 THEN DO:
        T_Imp = YES.
        Vr_Impuesto = Vr_Deducible * T_VIm.

        RUN Gra_MovAhorros(INPUT OpRet,
                           INPUT Producto.CodPto,
                           INPUT Producto.Cuenta,
                           INPUT Producto.DtoRef,
                           INPUT Producto.OfiTem,
                           INPUT W_Agencia,
                           INPUT Producto.OfiTem,
                           INPUT Producto.UsuAut,
                           INPUT 0,
                           INPUT Vr_Impuesto,
                           INPUT Ahorros.Nit).

        Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
        
        DELETE movProductos.

        IF Producto.OfiTem EQ W_Agencia THEN
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT OpRet,            INPUT Producto.CodPto,  INPUT CtaCble,      INPUT T_CIm,            INPUT "DB",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,       INPUT "1",                           
                             INPUT W_Usuario,       INPUT 0,                    INPUT Vr_Impuesto,      INPUT F_Seg).
        ELSE DO:
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT OpRet,            INPUT Producto.CodPto,  INPUT CtaSYA_FTE,   INPUT T_CIm,            INPUT "DB",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT Vr_Impuesto,      INPUT F_Seg).

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT OpRet,                INPUT Producto.CodPto,  INPUT CtaCble,          INPUT CtaSYA_DES,
                             INPUT "DB",            INPUT Ahorros.Nit,          INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT Producto.OfiTem,
                             INPUT Producto.OfiTem, INPUT W_Agencia,            INPUT "1",                  INPUT W_Usuario,        INPUT 0,                INPUT Vr_Impuesto,
                             INPUT F_Seg).
        END.

        T_Imp = NO.
    END.
END.

RELEASE Taquilla.

Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - Vr_Impuesto.

RUN reportarVisionamosAh(INPUT 2,
                         INPUT Vr_Impuesto).

IF ahorros.tarjetaDB NE " " AND Vr_Impuesto GT 0 THEN
    RUN grabar_TmpTarDeb(INPUT Vr_Impuesto,
                         INPUT "Deducibles e Impuestos",
                         INPUT 1).

ASSIGN Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje WHEN AVAIL(Mov_Ahorros).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_DeducibleOtros C-Win 
PROCEDURE Grabar_DeducibleOtros :
DEFINE INPUT PARAMETER CuentaOtros LIKE Cuentas.Cuenta.
DEFINE VAR Vr_Impuesto LIKE Ahorros.Sdo_Disponible.
DEFINE VAR OpRet LIKE Operacion.Cod_Operacion.
IF Vr_Deducible GT 0 THEN DO:
   IF Producto.OfiTem NE W_Agencia THEN
      CuentaOtros = W_CtaSyACMV.

   RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT T_OpeD, 
                    INPUT Producto.CodPto, INPUT T_Ctd,           INPUT CuentaOtros, 
                    INPUT "CR",            INPUT F_NitCuenta:SCREEN-VALUE IN FRAME Frame-Cuentas, 
                    INPUT T_Ctd,           
                    INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,                     
                    INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                           
                    INPUT W_Usuario,       INPUT 0,               INPUT Vr_Deducible, INPUT F_Seg) NO-ERROR.    
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "No se pudo grabar la taquilla para el deducible" VIEW-AS ALERT-BOX.
      RETURN ERROR.
   END. 

   IF Producto.EC EQ "C" THEN
      ASSIGN Taquilla.Val_Cheque   = Vr_Deducible
             Taquilla.Val_Efectivo = 0.
END.
RELEASE Taquilla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TarDeb C-Win 
PROCEDURE Grabar_TarDeb :
/*------------------------------------------------------------------------------
  Purpose:   Graba movimientos de Tarjeta debito  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER wvlrTrans  LIKE ahorros.sdo_disponible.
DEFINE INPUT PARAMETER wdesTrans AS CHARACTER.
DEFINE INPUT PARAMETER wtipoTrans AS INTEGER.
DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wretcode      AS INTEGER INITIAL -1.

IF wtipotrans NE 0 THEN DO:
   FIND CURRENT ahorros  NO-ERROR.
   CREATE tarjetadebito.
   ASSIGN tarjetadebito.Agencia         = w_agencia        /* mov_contable.agencia */
          tarjetadebito.Usuario         = w_usuario        /* mov_contable.usuario      */
          tarjetadebito.Comprobante     = cbte             /* mov_contable.comprobante   */
          tarjetadebito.Num_Documento   = w_docContab      /* mov_contable.Num_Documento */
          tarjetadebito.Fec_Contable    = TODAY            /* mov_contable.Fec_Contable */
          tarjetadebito.Hora            = TIME             /* mov_contable.Hora */
          tarjetadebito.Comentario      = wdesTrans
          tarjetadebito.Aplicado        = NO
          tarjetadebito.ManBiometrico   = 1
          tarjetadebito.TipoTransaccion = wtipotrans
          tarjetadebito.Nit             = Ahorros.nit
          tarjetadebito.Cue_Ahorros     = Ahorros.cue_ahorros  /* SUBSTRING(Ahorros.TarjetaDB,10,7) */
          tarjetadebito.TarjetaDB       = Ahorros.TarjetaDB
          tarjetadebito.Monto           = wvlrTrans
          tarjetadebito.Secuencia       = "000000000000"
          tarjetadebito.SdoTotal        = 0
          tarjetadebito.SdoDispon       = 0
          tarjetadebito.RetCode         = -1.
   wvlrmonTD = STRING(wvlrtrans,'9999999999999').
   RUN TranWebCaja.P(1,wtipotrans,TRIM(ahorros.nit), TRIM(ahorros.cue_ahorros), ahorros.TarjetaDB, wvlrmonTD, INPUT-OUTPUT wsecTD, INPUT-OUTPUT wsdoTD, INPUT-OUTPUT wdisTD, OUTPUT wretcode).
   IF wretcode = 0 THEN 
      ASSIGN tarjetadebito.Secuencia       = wsecTD 
             tarjetadebito.SdoTotal        = ROUND(decimal(wsdoTD) / 100,2)
             tarjetadebito.SdoDispon       = ROUND(decimal(wdisTD) / 100,2)
             tarjetadebito.RetCode         = wretcode
             tarjetadebito.Aplicado        = YES.
   ELSE 
      ASSIGN tarjetadebito.RetCode         = wretcode.
   RELEASE tarjetadebito.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TarDebNew C-Win 
PROCEDURE Grabar_TarDebNew :
/*------------------------------------------------------------------------------
  Purpose:   Graba movimientos de Tarjeta debito  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wretcode      AS INTEGER INITIAL -1.

  FOR EACH tmp-tarjetadb:
     CREATE tarjetadebito.
     BUFFER-COPY tmp-tarjetadb TO tarjetadebito.
     ASSIGN wvlrmonTD = STRING(tarjetadebito.Monto,'9999999999999').
     RUN TranWebCaja.P(1,tarjetadebito.TipoTransaccion,TRIM(tarjetadebito.Nit), TRIM(tarjetadebito.Cue_Ahorros), tarjetadebito.TarjetaDB, wvlrmonTD, INPUT-OUTPUT wsecTD, INPUT-OUTPUT wsdoTD, INPUT-OUTPUT wdisTD, OUTPUT wretcode).
     IF wretcode = 0 THEN 
        ASSIGN tarjetadebito.Secuencia       = wsecTD 
               tarjetadebito.SdoTotal        = ROUND(decimal(wsdoTD) / 100,2)
               tarjetadebito.SdoDispon       = ROUND(decimal(wdisTD) / 100,2)
               tarjetadebito.RetCode         = wretcode
               tarjetadebito.Aplicado        = YES.
     ELSE 
        ASSIGN tarjetadebito.RetCode         = wretcode.
     RELEASE tarjetadebito.
  END.
  /* Barrido de la tabla tarjetadb */
  FOR EACH tmp-tarjetadb:
      DELETE tmp-tarjetadb.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TmpTarDeb C-Win 
PROCEDURE Grabar_TmpTarDeb :
/*------------------------------------------------------------------------------
  Purpose:   Graba movimientos de Tarjeta debito  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER wvlrTrans  LIKE ahorros.sdo_disponible.
DEFINE INPUT PARAMETER wdesTrans AS CHARACTER.
DEFINE INPUT PARAMETER wtipoTrans AS INTEGER.
DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wretcode      AS INTEGER INITIAL -1.

IF wtipotrans NE 0 THEN DO:
   FIND CURRENT ahorros  NO-ERROR.
   CREATE Tmp-tarjetadb.
   ASSIGN Tmp-tarjetadb.Agencia         = w_agencia        /* mov_contable.agencia */
          Tmp-tarjetadb.Usuario         = w_usuario        /* mov_contable.usuario      */
          Tmp-tarjetadb.Comprobante     = cbte             /* mov_contable.comprobante   */
          Tmp-tarjetadb.Num_Documento   = w_docContab      /* mov_contable.Num_Documento */
          Tmp-tarjetadb.Fec_Contable    = TODAY            /* mov_contable.Fec_Contable */
          Tmp-tarjetadb.Hora            = TIME             /* mov_contable.Hora */
          Tmp-tarjetadb.Comentario      = wdesTrans
          Tmp-tarjetadb.Aplicado        = NO
          Tmp-tarjetadb.ManBiometrico   = 1
          Tmp-tarjetadb.TipoTransaccion = wtipotrans
          Tmp-tarjetadb.Nit             = Ahorros.nit
          Tmp-tarjetadb.Cue_Ahorros     = Ahorros.cue_ahorros  /* SUBSTRING(Ahorros.TarjetaDB,10,7) */
          Tmp-tarjetadb.TarjetaDB       = Ahorros.TarjetaDB
          Tmp-tarjetadb.Monto           = wvlrTrans
          Tmp-tarjetadb.Secuencia       = "000000000000"
          Tmp-tarjetadb.SdoTotal        = 0
          Tmp-tarjetadb.SdoDispon       = 0
          Tmp-tarjetadb.RetCode         = -1.
   RELEASE Tmp-Tarjetadb.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_CheqTransito C-Win 
PROCEDURE Gra_CheqTransito :
/*------------------------------------------------------------------------------
  OBSERVACIONES :       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER C_Banco   LIKE Che_Transito.Cod_Compensa.
   DEFINE INPUT PARAMETER C_Cheque  LIKE Che_Transito.Cheque.
   DEFINE INPUT PARAMETER C_CodPto  LIKE Che_Transito.Cod_Producto.
   DEFINE INPUT PARAMETER C_Estado  LIKE Che_Transito.Estado.
   DEFINE INPUT PARAMETER C_Cuenta  LIKE Che_Transito.Num_Cuenta.
   DEFINE INPUT PARAMETER C_Agencia LIKE Che_Transito.Agencia.
   DEFINE INPUT PARAMETER C_TipPto  LIKE Che_Transito.Tip_Producto.
   DEFINE INPUT PARAMETER C_Valor   LIKE Che_Transito.Valor.
   DEFINE INPUT PARAMETER C_Canje   LIKE Che_Transito.Tip_Remesa.
   
   FIND FIRST Che_Transito WHERE Che_Transito.Agencia EQ C_Agencia
                      AND   Che_Transito.Cod_Compensa EQ C_Banco
                      AND   Che_Transito.Cheque       EQ C_Cheque
                      AND   Che_Transito.Cod_Producto EQ C_CodPto
                      AND   Che_Transito.Tip_Producto EQ C_TipPto /*<---Abril 19/05 GAER*/
                      AND   Che_Transito.Num_Cuenta   EQ C_Cuenta EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE(Che_Transito) THEN DO:
      IF Che_Transito.Estado = C_Estado THEN /*<---Abril 19/05 GAER*/
         ASSIGN Che_Transito.Valor            = Che_Transito.Valor + C_Valor
                Che_Transito.Fec_Confirmacion = ?.
      ELSE /*<---Abril 19/05 GAER*/
         ASSIGN Che_Transito.Valor        = C_Valor
                Che_Transito.Estado       = C_Estado
                Che_Transito.Fec_Canje    = W_Fecha
                Che_Transito.Fec_Confirmacion = ?
                Che_Transito.Int_Generado     = 0.
   END.
   ELSE DO:
      CREATE Che_Transito.
      ASSIGN Che_Transito.Cod_Compensa     = C_Banco
             Che_Transito.Cheque           = C_Cheque
             Che_Transito.Cod_Producto     = C_CodPto
             Che_Transito.Estado           = C_Estado
             Che_Transito.Fec_Canje        = W_Fecha
             Che_Transito.Fec_Confirmacion = ?
             Che_Transito.Int_Generado     = 0
             Che_Transito.Num_Cuenta       = C_Cuenta
             Che_Transito.Agencia          = C_Agencia
             Che_Transito.Ofi_Destino      = W_Agencia
             Che_Transito.Tip_Producto     = C_TipPto
             Che_Transito.Valor            = C_Valor
             Che_Transito.Tip_Remesa       = C_Canje NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Error al Grabar en Cheques en Transito... "
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
         TITLE "Error en Taquilla".
         RETURN ERROR.
      END.
   END.

   IF ROUND(TIME / 3600,4) GE 16.25 THEN
      ASSIGN Che_Transito.Fec_Canje = W_Fecha + 1.

   /*RELEASE Che_Transito.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_Cheques C-Win 
PROCEDURE Gra_Cheques :
/*------------------------------------------------------------------------------
  OBSERVACION : Permite grabar los cheques en el producto respectivo.       
------------------------------------------------------------------------------*/
  DEFINE VAR X_Order AS DECIMAL DECIMALS 2.
  
  ASSIGN X_Order = Producto.W_Order.
  FOR EACH Cheques WHERE Cheques.W_Banco NE ? NO-LOCK:
      ASSIGN X_Order = X_Order + 0.05.
      CREATE Tmp_Producto.
      ASSIGN Tmp_Producto.W_Order  = X_Order
             Tmp_Producto.OfiTem   = Producto.OfiTem
             Tmp_Producto.OperaAux = Producto.OperaAux
             Tmp_Producto.TipPto   = Producto.TipPto 
             Tmp_Producto.CodPto   = Producto.CodPto
             Tmp_Producto.NomPto   = Producto.NomPto
             Tmp_Producto.Id_Tal   = Producto.Id_Tal
             Tmp_Producto.DtoRef   = Producto.DtoRef
             Tmp_Producto.EC       = "C"
             Tmp_Producto.Cuenta   = Producto.Cuenta
             Tmp_Producto.CueBco   = Producto.CueBco
             Tmp_Producto.Canje    = Cheques.W_Canje
             Tmp_Producto.D_Cheq   = Producto.D_Cheq
             Tmp_Producto.NitCta   = Producto.NitCta
             Tmp_Producto.Banco    = Cheques.W_Banco
             Tmp_Producto.Cheque   = Cheques.W_Cheque
             Tmp_Producto.Benefi   = Producto.Benefi
             Tmp_Producto.Debe     = Cheques.W_Valor
             Tmp_Producto.Haber    = 0
             Tmp_Producto.UsuAut   = Producto.UsuAut
             Tmp_Producto.Retiro   = FALSE.
  END.
  ASSIGN Open-Recid  = RECID(Tmp_Producto).
  FIND Tmp_Producto WHERE Tmp_Producto.W_Order EQ Producto.W_Order EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE(Tmp_Producto) THEN
     DELETE Tmp_Producto.
  RELEASE Tmp_Producto.
  FOR EACH Cheques:
      DELETE Cheques.
  END.
  RELEASE Cheques.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_Cuentas C-Win 
PROCEDURE Gra_Cuentas :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Graba consig.y retiros a las Cuentas contables.
------------------------------------------------------------------------------*/
  DEFINE VAR CuentaOtros LIKE Cuentas.Cuenta.
  DEFI   VAR P_Base      LIKE Ahorros.Sdo_Disponible INIT 0.
  DEFI   VAR EfCh        AS INTEG FORM "9" INIT 0.  /*Inicia en efectivo*/ 
  
  IF Producto.Debe NE 0 THEN DO:   /*Consignaciones*/
     IF Producto.EC EQ "E" THEN DO:  /*Efectivo*/
        RUN Operacion_DC(INPUT Producto.Cuenta,INPUT 1,INPUT 1,INPUT 3,OUTPUT V_Operacion) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
           RETURN ERROR.

        IF W_Error THEN DO:
           MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP
                   W_Nomoper 
           VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
           RETURN ERROR.
        END.
/**/
        RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT V_Operacion,OUTPUT W_Error,OUTPUT W_Nomoper).

        IF AVAILABLE Operacion THEN 
           ASSIGN T_Ded  = Operacion.Cod_deducible
                  T_OpeD = Operacion.Cod_Operacion
                  W_CtaSyACMV  = Operacion.Cta_SyA
                  Vr_Deducible = 0.
        ELSE DO:
            MESSAGE "No hay una Operación disponible para el deducible".
            RETURN ERROR.
        END.

        IF T_Ded GT " " THEN DO:
           RUN Validar_Deducible (INPUT T_Ded, INPUT Producto.Debe, 
                                  OUTPUT Vr_Deducible, OUTPUT T_Cld, OUTPUT T_NDd,
                                  OUTPUT T_CtD, OUTPUT T_Cim, OUTPUT T_VIm) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN DO:
              MESSAGE "En el Proc.Validar_Deducible se presentó error...Verifique por favor." 
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN ERROR.
           END.
        END.     
        
        ASSIGN Producto.Debe = Producto.Debe - Vr_Deducible
               CuentaOtros   = Cta_Caja.

        RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT V_Operacion, 
                         INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT Cta_Caja,
                         INPUT "CR",            INPUT Producto.NitCta, INPUT Producto.Cuenta,     
                         INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                         INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT 0,                  
                         INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).

        RELEASE Taquilla. 
        
     END.
     ELSE DO:     /*Consignaciones en cheque*/
        RUN Operacion_DC(INPUT Producto.Cuenta,INPUT 1,INPUT 1,INPUT 3,OUTPUT V_Operacion) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
           RETURN ERROR.
       
        RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT V_Operacion,OUTPUT W_Error,OUTPUT W_Nomoper).
        IF W_Error THEN DO:
           MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP
                   W_Nomoper 
           VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
           RETURN ERROR.
        END.

        IF AVAILABLE Operacion THEN 
           ASSIGN T_Ded  = Operacion.Cod_deducible
                  T_OpeD = Operacion.Cod_Operacion
                  W_CtaSyACMV  = Operacion.Cta_SyA
                  Vr_Deducible = 0.
        ELSE DO:
            MESSAGE "No hay una operación disponible para el deducible".
            RETURN ERROR.
        END.

        IF T_Ded GT " " THEN DO:
           RUN Validar_Deducible (INPUT T_Ded, INPUT Producto.Debe, 
                                  OUTPUT Vr_Deducible, OUTPUT T_Cld, OUTPUT T_NDd, 
                                  OUTPUT T_CtD, OUTPUT T_Cim, OUTPUT T_VIm) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN DO:
              MESSAGE "En el Proc.Validar_Deducible se presentó error...Verifique por favor." 
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN ERROR.
           END.
        END.

        FIND FIRST Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta    /*Enero 31/06 GAER*/                                                       
                           AND   Cheques.W_CodPto EQ Producto.CodPto                                                           
                           AND   Cheques.W_Dto    EQ Producto.DtoRef NO-ERROR. 
        
        ASSIGN Producto.Debe   = Producto.Debe   - Vr_Deducible
               Cheques.W_Valor = Cheques.W_Valor - Vr_Deducible
               CuentaOtros     = Cta_Banco.

        FOR EACH Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta                                                           
                            AND   Cheques.W_CodPto EQ Producto.CodPto                                                           
                            AND   Cheques.W_Dto    EQ Producto.DtoRef NO-LOCK:                                                  
               RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco, INPUT V_Operacion,                                
                                INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT Cta_Banco,                                  
                                INPUT "CR",            INPUT Producto.NitCta, INPUT Producto.Cuenta,                            
                                INPUT Producto.DtoRef, INPUT Cheques.W_Cheque,INPUT W_Agencia,                                  
                                INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT 0,                                          
                                INPUT W_Usuario,       INPUT Cheques.W_Valor, INPUT 0, INPUT F_Seg).  

               RUN Gra_CheqTransito(INPUT Cheques.W_Banco, INPUT Cheques.W_Cheque, INPUT Producto.CodPto,                        
                                   INPUT 1,               INPUT Producto.Cuenta, INPUT Producto.Ofitem,                         
                                   INPUT 0,               INPUT Cheques.W_Valor + Vr_Deducible, INPUT Cheques.W_Canje) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN                                                                                    
                  RETURN ERROR.                                                                                                  
               RELEASE Taquilla.                                                                                                                                                                                                                            
        END.
     END.
  END.
  ELSE DO:   /*Retiros*/    
     IF Producto.EC EQ "E" THEN DO:
        RUN Operacion_DC(INPUT Producto.Cuenta,INPUT 1,INPUT 2,INPUT 3,OUTPUT V_Operacion) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
           RETURN ERROR.
        
        RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT V_Operacion,OUTPUT W_Error,OUTPUT W_Nomoper).
        IF AVAILABLE Operacion THEN ASSIGN T_Ded  = Operacion.Cod_deducible
                                           T_OpeD = Operacion.Cod_Operacion
                                           WCpteTx = Operacion.Comprobante.
        IF W_Error THEN DO:
           MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP
                   W_Nomoper 
           VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
           RETURN ERROR.
        END.

        IF Operacion.Cod_deducible NE ? AND Operacion.Cod_deducible NE " " THEN DO:
           RUN Validar_Deducible (INPUT T_Ded, INPUT Producto.Haber, 
                                  OUTPUT Vr_Deducible, OUTPUT T_Cld, OUTPUT T_NDd, OUTPUT T_CtD, OUTPUT T_Cim, OUTPUT T_VIm) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN DO:
              MESSAGE "En el Proc.Validar_Deducible se presentó error...Verifique por favor." 
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN ERROR.
           END.
        END.

        /*llamado gmg para cuentas contables  17/01/2004 Alex moncada*/
            
            
        ASSIGN P_Base = Producto.Haber.
        IF P_Base GT 0 AND T_Ded GT " " AND Operacion.Cod_deducible NE ? AND NOT W_Rev THEN DO:
           RUN RutGMF.P (INPUT TRUE,W_Agencia,W_Agencia,3,"",Producto.NitCta,producto.cuenta,T_OpeD,P_Base,Cbte,
                         INPUT STRING(W_DocContab),"Por Taquilla",0,EfCh,OUTPUT P_ImpAplic) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN DO:                                                        
              MESSAGE "El Prog: RutGMF.P...Retornò ERROR(Salvando) no se permite la operaciòn..."       
                       VIEW-AS ALERT-BOX ERROR.                                                                         
              RETURN ERROR.                                                                                       
           END.                                                                                                   
        END.
        /*Fin llamado*/
        RUN Gra_Taquilla(INPUT Producto.UsuAut,     INPUT Producto.Banco,  INPUT V_Operacion, 
                         INPUT Producto.CodPto,     INPUT Producto.Cuenta, INPUT Cta_Caja,
                         INPUT "DB",                INPUT Producto.NitCta, INPUT Producto.Cuenta,     
                         INPUT Producto.DtoRef,     INPUT Producto.Cheque, INPUT W_Agencia,           
                         INPUT Producto.OfiTem,     INPUT W_Agencia,       INPUT 0,                  
                         INPUT W_Usuario,           INPUT 0,               INPUT Producto.Haber, INPUT F_Seg).
        RELEASE Taquilla.                                    
     END.
     ELSE DO:
        RUN Operacion_DC(INPUT Producto.Cuenta,INPUT 1,INPUT 2,INPUT 3,OUTPUT V_Operacion) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
           RETURN ERROR.
    
        FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ Producto.OperaAux NO-LOCK NO-ERROR.
        IF AVAILABLE(Operacion) THEN 
           ASSIGN W_CtaBco = Operacion.Cuenta.
        
        RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT V_Operacion,OUTPUT W_Error,OUTPUT W_Nomoper).
        IF W_Error THEN DO:
           MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP
                   W_Nomoper 
           VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
           RETURN ERROR.
        END.

        RUN Gra_Taquilla(INPUT Producto.UsuAut,     INPUT Producto.Banco,  INPUT V_Operacion, 
                         INPUT Producto.CodPto,     INPUT Producto.Cuenta, INPUT W_CtaBco,
                         INPUT "DB",                INPUT Producto.NitCta, INPUT Producto.Cuenta,     
                         INPUT Producto.DtoRef,     INPUT Producto.Cheque, INPUT W_Agencia,           
                         INPUT Producto.OfiTem,     INPUT W_Agencia,       INPUT 0,                  
                         INPUT W_Usuario,           INPUT Producto.Haber,  INPUT 0, INPUT F_Seg).
        RUN Gra_CheqTransito(INPUT Producto.Banco,  INPUT Producto.Cheque, INPUT Producto.CodPto,
                             INPUT 4,               INPUT Producto.CueBco, INPUT Producto.Ofitem, 
                             INPUT 0,               INPUT Producto.Haber,  INPUT 0) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
           RETURN ERROR.
        END.
        RELEASE Taquilla.                                    
     END.
  END.

  IF Producto.Debe GT 0 THEN DO:
     RUN Grabar_DeducibleOtros(INPUT CuentaOtros) NO-ERROR.  
     IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error Al Grabar los Deducibles. Se cancela la operacion" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_Especiales C-Win 
PROCEDURE Gra_Especiales :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permita Grabar los Productos Epeciales.
------------------------------------------------------------------------------*/
  DEFINE VAR T_Cuota LIKE Especiales.Cuota.
   
  FOR FIRST Pro_Especiales WHERE Pro_Especiales.Agencia      EQ Producto.OfiTem
                           AND   Pro_Especiales.Cod_Producto EQ Producto.CodPto
                           AND   Pro_Especiales.Estado       EQ 1 NO-LOCK,
      FIRST Especiales     WHERE Especiales.Agencia      EQ Pro_Especiales.Agencia
                           AND   Especiales.Cod_Producto EQ Pro_Especiales.Cod_Producto
                           AND   Especiales.Nit          EQ F_Nit EXCLUSIVE-LOCK:
      RUN Buscar_CtaEsp(INPUT Producto.OfiTem,INPUT Producto.CodPto,OUTPUT CtaCble,OUTPUT CtaSyA) NO-ERROR.
      IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
         RELEASE Especiales.
         RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
         RETURN ERROR.
      END.
      IF Producto.EC EQ "E" THEN DO:
         IF Especiales.Clase_Operacion   EQ "NORM" AND 
            Especiales.Codigo_Operacion  EQ 1      THEN DO:
            RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT 040101001,OUTPUT W_Error,OUTPUT W_Nomoper).
            IF W_Error THEN DO:
               RELEASE Especiales.
               MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP W_Nomoper 
               VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
               RETURN ERROR.
            END.
            RUN Gra_MovEspeciales(INPUT  040101001,     INPUT Producto.CodPto,INPUT Producto.OfiTem, INPUT W_Agencia,
                                  INPUT Producto.OfiTem,INPUT Especiales.Nit, INPUT Producto.DtoRef, INPUT Especiales.Secuencia,
                                  INPUT 0,              INPUT Producto.Debe).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 040101001, 
                             INPUT Producto.CodPto, INPUT CtaCble,         INPUT Cta_Caja,
                             INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                             INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).
            IF Pro_Especiales.Id_CtrSaldo EQ TRUE THEN DO:
               ASSIGN Especiales.Sdo_Pendiente = Especiales.Sdo_Pendiente - Producto.Debe.
               IF Especiales.Sdo_Pendiente LT 0 THEN ASSIGN Especiales.Sdo_Pendiente = 0.
            END.
            ASSIGN Especiales.Codigo_Operacion = 2
                   Especiales.Fec_UltPago      = TODAY 
                   Especiales.Vlr_AcumPagos    = Especiales.Vlr_AcumPagos + Producto.Debe
                   T_Cuota = ROUND(Especiales.Vlr_AcumPagos / (Especiales.Cuota * Especiales.Cantidad),0)
                   Especiales.Cuo_Pagadas      = T_Cuota.
            RELEASE Taquilla.
         END.
         ELSE DO:
            RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT 040101005,OUTPUT W_Error,OUTPUT W_Nomoper).
            IF W_Error THEN DO:
               RELEASE Especiales.
               MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP W_Nomoper 
               VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
               RETURN ERROR.
            END.
            RUN Gra_MovEspeciales(INPUT 040101005,      INPUT Producto.CodPto,INPUT Producto.OfiTem, INPUT W_Agencia,
                                  INPUT Producto.OfiTem,INPUT Especiales.Nit, INPUT Producto.DtoRef, INPUT Especiales.Secuencia,
                                  INPUT 0,              INPUT Producto.Debe).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 040101005, 
                             INPUT Producto.CodPto, INPUT CtaCble,         INPUT Cta_Caja,
                             INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                             INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).
            IF Pro_Especiales.Id_CtrSaldo EQ TRUE THEN DO:
               ASSIGN Especiales.Sdo_Pendiente = Especiales.Sdo_Pendiente - Producto.Debe.
               IF Especiales.Sdo_Pendiente LT 0 THEN ASSIGN Especiales.Sdo_Pendiente = 0.
            END.
            ASSIGN Especiales.Fec_UltPago      = TODAY 
                   Especiales.Vlr_AcumPagos    = Especiales.Vlr_AcumPagos + Producto.Debe
                   T_Cuota = ROUND(Especiales.Vlr_AcumPagos / (Especiales.Cuota * Especiales.Cantidad),0)
                   Especiales.Cuo_Pagadas      = T_Cuota.
            RELEASE Taquilla.
         END.
      END.
      ELSE DO:
         IF Especiales.Clase_Operacion   EQ "NORM" AND 
            Especiales.Codigo_Operacion  EQ 1      THEN DO:
            RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT 040101002,OUTPUT W_Error,OUTPUT W_Nomoper).
            IF W_Error THEN DO:
               RELEASE Especiales.
               MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP W_Nomoper 
               VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
               RETURN ERROR.
            END.
            RUN Gra_MovEspeciales(INPUT  040101002,     INPUT Producto.CodPto,INPUT Producto.OfiTem, INPUT W_Agencia,
                                  INPUT Producto.OfiTem,INPUT Especiales.Nit, INPUT Producto.DtoRef, INPUT Especiales.Secuencia,
                                  INPUT Producto.Debe,  INPUT 0).
            FOR EACH Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta
                             AND   Cheques.W_CodPto EQ Producto.CodPto NO-LOCK:
                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco, INPUT 040101002, 
                                 INPUT Producto.CodPto, INPUT CtaCble,         INPUT Cta_Banco,
                                 INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                                 INPUT Producto.DtoRef, INPUT Cheques.W_Cheque,INPUT W_Agencia,           
                                 INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                                 INPUT W_Usuario,       INPUT Cheques.W_Valor, INPUT 0, INPUT F_Seg).
                RUN Gra_CheqTransito(INPUT Cheques.W_Banco, INPUT Cheques.W_Cheque, INPUT Producto.CodPto,
                                     INPUT 1,               INPUT Producto.Cuenta,  INPUT Producto.Ofitem, 
                                     INPUT 4,               INPUT Cheques.W_Valor,  INPUT Cheques.W_Canje) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                   RETURN ERROR.
            END.
            IF Pro_Especiales.Id_CtrSaldo EQ TRUE THEN DO:
               ASSIGN Especiales.Sdo_Pendiente = Especiales.Sdo_Pendiente - Producto.Debe.
               IF Especiales.Sdo_Pendiente LT 0 THEN ASSIGN Especiales.Sdo_Pendiente = 0.
            END.
            ASSIGN Especiales.Codigo_Operacion = 2
                   Especiales.Fec_UltPago      = TODAY 
                   Especiales.Vlr_AcumPagos    = Especiales.Vlr_AcumPagos + Producto.Debe
                   T_Cuota = ROUND(Especiales.Vlr_AcumPagos / (Especiales.Cuota * Especiales.Cantidad),0)
                   Especiales.Cuo_Pagadas      = T_Cuota.
            RELEASE Taquilla.
         END.
         ELSE DO:
            RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT 040101006,OUTPUT W_Error,OUTPUT W_Nomoper).
            IF W_Error THEN DO:
               RELEASE Especiales.
               MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP W_Nomoper 
               VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
               RETURN ERROR.
            END.
            RUN Gra_MovEspeciales(INPUT  040101006,     INPUT Producto.CodPto,INPUT Producto.OfiTem, INPUT W_Agencia,
                                  INPUT Producto.OfiTem,INPUT Especiales.Nit, INPUT Producto.DtoRef, INPUT Especiales.Secuencia,
                                  INPUT Producto.Debe,  INPUT 0).
            FOR EACH Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta
                             AND   Cheques.W_CodPto EQ Producto.CodPto NO-LOCK:
                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco, INPUT 040101006, 
                                 INPUT Producto.CodPto, INPUT CtaCble,         INPUT Cta_Banco,
                                 INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                                 INPUT Producto.DtoRef, INPUT Cheques.W_Cheque,INPUT W_Agencia,           
                                 INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                                 INPUT W_Usuario,       INPUT Cheques.W_Valor, INPUT 0, INPUT F_Seg).
                RUN Gra_CheqTransito(INPUT Cheques.W_Banco, INPUT Cheques.W_Cheque, INPUT Producto.CodPto,
                                     INPUT 1,               INPUT Producto.Cuenta,  INPUT Producto.Ofitem, 
                                     INPUT 4,               INPUT Cheques.W_Valor,  INPUT Cheques.W_Canje) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                   RETURN ERROR.
            END.
            IF Pro_Especiales.Id_CtrSaldo EQ TRUE THEN DO:
               ASSIGN Especiales.Sdo_Pendiente = Especiales.Sdo_Pendiente - Producto.Debe.
               IF Especiales.Sdo_Pendiente LT 0 THEN ASSIGN Especiales.Sdo_Pendiente = 0.
            END.
            ASSIGN Especiales.Fec_UltPago      = TODAY 
                   Especiales.Vlr_AcumPagos    = Especiales.Vlr_AcumPagos + Producto.Debe
                   T_Cuota = ROUND(Especiales.Vlr_AcumPagos / (Especiales.Cuota * Especiales.Cantidad),0)
                   Especiales.Cuo_Pagadas      = T_Cuota.
            RELEASE Taquilla.
         END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_EspecialesSyA C-Win 
PROCEDURE Gra_EspecialesSyA :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permita Grabar los Productos Epeciales en SYA.
------------------------------------------------------------------------------*/
  DEFINE VAR T_Cuota LIKE Especiales.Cuota.
  DEFINE VAR SW      AS   LOGICAL INITIAL FALSE.
   
  RUN Buscar_CtaEsp(INPUT Producto.OfiTem,INPUT Producto.CodPto,OUTPUT CtaCble,OUTPUT CtaSyA) NO-ERROR.
  IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
     RETURN ERROR.
  END. 
  FOR FIRST Pro_Especiales WHERE Pro_Especiales.Agencia      EQ Producto.OfiTem
                           AND   Pro_Especiales.Cod_Producto EQ Producto.CodPto
                           AND   Pro_Especiales.Estado       EQ 1 NO-LOCK,
      FIRST Especiales     WHERE Especiales.Agencia      EQ Pro_Especiales.Agencia
                           AND   Especiales.Cod_Producto EQ Pro_Especiales.Cod_Producto
                           AND   Especiales.Nit          EQ F_Nit EXCLUSIVE-LOCK:
      ASSIGN SW = TRUE.
      IF Producto.EC EQ "E" THEN DO:
         IF Especiales.Clase_Operacion   EQ "NORM" AND 
            Especiales.Codigo_Operacion  EQ 1      THEN DO:
            RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT 040101003,OUTPUT W_Error,OUTPUT W_Nomoper).
            IF W_Error THEN DO:
               RELEASE Especiales.
               MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP W_Nomoper 
               VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
               RETURN ERROR.
            END.
            RUN Gra_MovEspeciales(INPUT 040101003,      INPUT Producto.CodPto,INPUT Producto.OfiTem, INPUT W_Agencia,
                                  INPUT Producto.OfiTem,INPUT Especiales.Nit, INPUT Producto.DtoRef, INPUT Especiales.Secuencia,
                                  INPUT 0,              INPUT Producto.Debe).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 040101003, 
                             INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                             INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                             INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 040101003, 
                             INPUT Producto.CodPto, INPUT CtaCble,         INPUT CtaSyA,
                             INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                             INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).
            IF Pro_Especiales.Id_CtrSaldo EQ TRUE THEN DO:
               ASSIGN Especiales.Sdo_Pendiente = Especiales.Sdo_Pendiente - Producto.Debe.
               IF Especiales.Sdo_Pendiente LT 0 THEN ASSIGN Especiales.Sdo_Pendiente = 0.
            END.
            ASSIGN Especiales.Codigo_Operacion = 2
                   Especiales.Fec_UltPago      = TODAY 
                   Especiales.Vlr_AcumPagos    = Especiales.Vlr_AcumPagos + Producto.Debe
                   T_Cuota = ROUND(Especiales.Vlr_AcumPagos / (Especiales.Cuota * Especiales.Cantidad),0)
                   Especiales.Cuo_Pagadas      = T_Cuota.
            RELEASE Taquilla.
         END.
         ELSE DO:
            RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT 040101007,OUTPUT W_Error,OUTPUT W_Nomoper).
            IF W_Error THEN DO:
               RELEASE Especiales.
               MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP W_Nomoper 
               VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
               RETURN ERROR.
            END.
            RUN Gra_MovEspeciales(INPUT 040101007,      INPUT Producto.CodPto,INPUT Producto.OfiTem, INPUT W_Agencia,
                                  INPUT Producto.OfiTem,INPUT Especiales.Nit, INPUT Producto.DtoRef, INPUT Especiales.Secuencia,
                                  INPUT 0,              INPUT Producto.Debe).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 040101007, 
                             INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                             INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                             INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 040101007, 
                             INPUT Producto.CodPto, INPUT CtaCble,         INPUT CtaSyA,
                             INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                             INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).
            IF Pro_Especiales.Id_CtrSaldo EQ TRUE THEN DO:
               ASSIGN Especiales.Sdo_Pendiente = Especiales.Sdo_Pendiente - Producto.Debe.
               IF Especiales.Sdo_Pendiente LT 0 THEN ASSIGN Especiales.Sdo_Pendiente = 0.
            END.
            ASSIGN Especiales.Fec_UltPago      = TODAY 
                   Especiales.Vlr_AcumPagos    = Especiales.Vlr_AcumPagos + Producto.Debe
                   T_Cuota = ROUND(Especiales.Vlr_AcumPagos / (Especiales.Cuota * Especiales.Cantidad),0)
                   Especiales.Cuo_Pagadas      = T_Cuota.
            RELEASE Taquilla.
         END.
      END.
      ELSE DO:
         IF Especiales.Clase_Operacion   EQ "NORM" AND 
            Especiales.Codigo_Operacion  EQ 1      THEN DO:
            RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT 040101004,OUTPUT W_Error,OUTPUT W_Nomoper).
            IF W_Error THEN DO:
               RELEASE Especiales.
               MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP W_Nomoper 
               VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
               RETURN ERROR.
            END.
            RUN Gra_MovEspeciales(INPUT  040101004,     INPUT Producto.CodPto,INPUT Producto.OfiTem, INPUT W_Agencia,
                                  INPUT Producto.OfiTem,INPUT Especiales.Nit, INPUT Producto.DtoRef, INPUT Especiales.Secuencia,
                                  INPUT Producto.Debe,  INPUT 0).
            FOR EACH Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta
                             AND   Cheques.W_CodPto EQ Producto.CodPto NO-LOCK:
                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco, INPUT 040101004, 
                                 INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Banco,
                                 INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                                 INPUT Producto.DtoRef, INPUT Cheques.W_Cheque,INPUT W_Agencia,           
                                 INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                                 INPUT W_Usuario,       INPUT Cheques.W_Valor, INPUT 0, INPUT F_Seg).
                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco, INPUT 040101004, 
                                 INPUT Producto.CodPto, INPUT CtaCble,         INPUT CtaSyA,
                                 INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                                 INPUT Producto.DtoRef, INPUT Cheques.W_Cheque,INPUT Producto.OfiTem,           
                                 INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                                 INPUT W_Usuario,       INPUT Cheques.W_Valor, INPUT 0).
                RUN Gra_CheqTransito(INPUT Cheques.W_Banco, INPUT Cheques.W_Cheque, INPUT Producto.CodPto,
                                     INPUT 1,               INPUT Producto.Cuenta,  INPUT Producto.Ofitem, 
                                     INPUT 4,               INPUT Cheques.W_Valor,  INPUT Cheques.W_Canje) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                   RETURN ERROR.
            END.
            IF Pro_Especiales.Id_CtrSaldo EQ TRUE THEN DO:
               ASSIGN Especiales.Sdo_Pendiente = Especiales.Sdo_Pendiente - Producto.Debe.
               IF Especiales.Sdo_Pendiente LT 0 THEN ASSIGN Especiales.Sdo_Pendiente = 0.
            END.
            ASSIGN Especiales.Codigo_Operacion = 2
                   Especiales.Fec_UltPago      = TODAY 
                   Especiales.Vlr_AcumPagos    = Especiales.Vlr_AcumPagos + Producto.Debe
                   T_Cuota = ROUND(Especiales.Vlr_AcumPagos / (Especiales.Cuota * Especiales.Cantidad),0)
                   Especiales.Cuo_Pagadas      = T_Cuota.
            RELEASE Taquilla.
         END.
         ELSE DO:
            RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT 040101008,OUTPUT W_Error,OUTPUT W_Nomoper).
            IF W_Error THEN DO:
               RELEASE Especiales.
               MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP W_Nomoper 
               VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
               RETURN ERROR.
            END.
            RUN Gra_MovEspeciales(INPUT 040101008,      INPUT Producto.CodPto,INPUT Producto.OfiTem, INPUT W_Agencia,
                                  INPUT Producto.OfiTem,INPUT Especiales.Nit, INPUT Producto.DtoRef, INPUT Especiales.Secuencia,
                                  INPUT Producto.Debe,  INPUT 0).
            FOR EACH Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta
                             AND   Cheques.W_CodPto EQ Producto.CodPto NO-LOCK:
                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco, INPUT 040101008, 
                                 INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Banco,
                                 INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                                 INPUT Producto.DtoRef, INPUT Cheques.W_Cheque,INPUT W_Agencia,           
                                 INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                                 INPUT W_Usuario,       INPUT Cheques.W_Valor, INPUT 0, INPUT F_Seg).
                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco, INPUT 040101008, 
                                 INPUT Producto.CodPto, INPUT CtaCble,         INPUT CtaSyA,
                                 INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                                 INPUT Producto.DtoRef, INPUT Cheques.W_Cheque,INPUT Producto.OfiTem,           
                                 INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                                 INPUT W_Usuario,       INPUT Cheques.W_Valor, INPUT 0, INPUT F_Seg).
                RUN Gra_CheqTransito(INPUT Cheques.W_Banco, INPUT Cheques.W_Cheque, INPUT Producto.CodPto,
                                     INPUT 1,               INPUT Producto.Cuenta,  INPUT Producto.Ofitem, 
                                     INPUT 4,               INPUT Cheques.W_Valor,  INPUT Cheques.W_Canje) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                   RETURN ERROR.
            END.
            IF Pro_Especiales.Id_CtrSaldo EQ TRUE THEN DO:
               ASSIGN Especiales.Sdo_Pendiente = Especiales.Sdo_Pendiente - Producto.Debe.
               IF Especiales.Sdo_Pendiente LT 0 THEN ASSIGN Especiales.Sdo_Pendiente = 0.
            END.
            ASSIGN Especiales.Fec_UltPago      = TODAY 
                   Especiales.Vlr_AcumPagos    = Especiales.Vlr_AcumPagos + Producto.Debe
                   T_Cuota = ROUND(Especiales.Vlr_AcumPagos / (Especiales.Cuota * Especiales.Cantidad),0)
                   Especiales.Cuo_Pagadas      = T_Cuota.
            RELEASE Taquilla.
         END.
      END.
      RELEASE Especiales.
  END.
  IF SW EQ FALSE THEN DO:
     IF Producto.EC EQ "E" THEN DO:
        RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT 040101007,OUTPUT W_Error,OUTPUT W_Nomoper).
        IF W_Error THEN DO:
           MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP W_Nomoper 
           VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
           RETURN ERROR.
        END.
        RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 040101007, 
                         INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                         INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                         INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                         INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                         INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).
     END.
     ELSE DO:
        RUN Val_Operacion(INPUT W_Agencia,INPUT W_Grupo,INPUT W_Usuario,INPUT 040101008,OUTPUT W_Error,OUTPUT W_Nomoper).
        IF W_Error THEN DO:
           MESSAGE "No Esta Permitida Por Taquilla La Operación..." SKIP W_Nomoper 
           VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
           RETURN ERROR.
        END.
        FOR EACH Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta
                         AND   Cheques.W_CodPto EQ Producto.CodPto NO-LOCK:
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco, INPUT 040101008, 
                             INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Banco,
                             INPUT "CR",            INPUT Especiales.Nit,  INPUT Especiales.Nit, 
                             INPUT Producto.DtoRef, INPUT Cheques.W_Cheque,INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "4",            
                             INPUT W_Usuario,       INPUT Cheques.W_Valor, INPUT 0, INPUT F_Seg).
            RUN Gra_CheqTransito(INPUT Cheques.W_Banco, INPUT Cheques.W_Cheque, INPUT Producto.CodPto,
                                 INPUT 1,               INPUT Producto.Cuenta,  INPUT Producto.Ofitem, 
                                 INPUT 4,               INPUT Cheques.W_Valor,  INPUT Cheques.W_Canje) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               RETURN ERROR.
        END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_MovAhorros C-Win 
PROCEDURE Gra_MovAhorros :
DEFINE INPUT PARAMETER M_CodOper LIKE Mov_Ahorros.Cod_Operacion.
DEFINE INPUT PARAMETER M_CodPto LIKE Mov_ahorros.cod_ahorro.
DEFINE INPUT PARAMETER M_Cuenta LIKE Mov_Ahorros.Cue_Ahorros.
DEFINE INPUT PARAMETER M_Dto LIKE Mov_Ahorros.Num_Documento.
DEFINE INPUT PARAMETER M_Agencia LIKE Mov_Ahorros.Agencia.
DEFINE INPUT PARAMETER M_OfiFte LIKE Mov_Ahorros.Age_Fuente.
DEFINE INPUT PARAMETER M_OfiDest LIKE Mov_Ahorros.Age_Destino.
DEFINE INPUT PARAMETER M_Usuario LIKE Mov_Ahorros.Usuario.
DEFINE INPUT PARAMETER M_VlrChe LIKE Mov_Ahorros.Val_Cheque.
DEFINE INPUT PARAMETER M_VlrEfe LIKE Mov_Ahorros.Val_Efectivo.
DEFINE INPUT PARAMETER M_Nit LIKE Clientes.Nit.

CREATE Mov_Ahorros.
ASSIGN Mov_Ahorros.Cod_Operacion = M_CodOper
       Mov_ahorros.cod_ahorro = M_CodPto
       Mov_Ahorros.Cue_Ahorros = M_Cuenta
       Mov_ahorros.nit = M_Nit
       Mov_Ahorros.Fecha = w_fecha
       Mov_Ahorros.Hora = TIME
       Mov_Ahorros.Num_Documento = STRING(W_DocContab)
       Mov_Ahorros.Cpte = Cbte
       Mov_Ahorros.Agencia = M_Agencia
       Mov_Ahorros.Age_Fuente = M_OfiFte
       Mov_Ahorros.Age_Destino = M_OfiDest
       Mov_Ahorros.Usuario = M_Usuario
       Mov_Ahorros.Val_Cheque = M_VlrChe
       Mov_Ahorros.Val_Efectivo = M_VlrEfe
       Mov_Ahorros.NomApell = W_NomTx
       Mov_Ahorros.Cedula_Trans = W_CedTrans.

IF W_Rev THEN
    Mov_Ahorros.Descrip = "Reversar TX".

CREATE movProductos.
ASSIGN movProductos.agencia = M_agencia
       movProductos.comprobante = Cbte
       movProductos.estado = 1
       movProductos.fecha = w_fecha
       movProductos.id_producto = M_Cuenta
       movProductos.nit = M_Nit
       movProductos.num_documento = w_docContab
       movProductos.tipo_producto = 1.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_MovEspeciales C-Win 
PROCEDURE Gra_MovEspeciales :
/*------------------------------------------------------------------------------
  Observación : 
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER E_Opera  LIKE Mov_Especiales.Cod_Operacion.
    DEFINE INPUT PARAMETER E_CodPto LIKE Mov_Especiales.Cod_Producto.
    DEFINE INPUT PARAMETER E_Ofi    LIKE Mov_Especiales.Agencia.
    DEFINE INPUT PARAMETER E_OfiFte LIKE Mov_Especiales.Ofi_Fuente.
    DEFINE INPUT PARAMETER E_OfiDno LIKE Mov_Especiales.Ofi_Destino.
    DEFINE INPUT PARAMETER E_Nit    LIKE Mov_Especiales.Nit.
    DEFINE INPUT PARAMETER E_Ndto   LIKE Mov_Especiales.Num_Documento.
    DEFINE INPUT PARAMETER E_Scia   LIKE Mov_Especiales.Secuencia.
    DEFINE INPUT PARAMETER E_Vche   LIKE Mov_Especiales.Vlr_Cheque.
    DEFINE INPUT PARAMETER E_Vefe   LIKE Mov_Especiales.Vlr_Efectivo.
    
    CREATE Mov_Especiales.
    ASSIGN Mov_Especiales.Cod_Operacion = E_Opera
           Mov_Especiales.Cod_Producto  = E_CodPto
           Mov_Especiales.Agencia       = E_Ofi
           Mov_Especiales.Ofi_Fuente    = E_OfiFte
           Mov_Especiales.Ofi_Destino   = E_OfiDno
           Mov_Especiales.Nit           = E_Nit
           Mov_Especiales.Num_Documento = E_Ndto
           Mov_Especiales.Secuencia     = E_Scia
           Mov_Especiales.Vlr_Cheque    = E_Vche
           Mov_Especiales.Vlr_Efectivo  = E_Vefe
           Mov_Especiales.Usuario       = W_Usuario
           Mov_Especiales.Fecha         = TODAY
           Mov_Especiales.Hora          = TIME.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_Movimientos C-Win 
PROCEDURE Gra_Movimientos :
DEFINE INPUT PARAMETER P_Agencia LIKE Agencias.Agencia.
DEFINE INPUT PARAMETER P_Cbte AS INTEGER.
DEFINE INPUT PARAMETER P_Cuenta LIKE Cuentas.Cuenta.
DEFINE INPUT PARAMETER P_Fecha1 AS DATE.
DEFINE INPUT PARAMETER P_Nat LIKE Cuentas.Naturaleza.
DEFINE INPUT PARAMETER P_Coment AS CHARACTER.
DEFINE INPUT PARAMETER P_Usuario LIKE Usuarios.Usuario.
DEFINE INPUT PARAMETER P_DB AS DECIMAL.
DEFINE INPUT PARAMETER P_CR AS DECIMAL.
DEFINE INPUT PARAMETER P_CC AS INTEGER.
DEFINE INPUT PARAMETER P_Docto AS INTEGER.
DEFINE INPUT PARAMETER P_Fecha2 AS DATE.
DEFINE INPUT PARAMETER P_Hora AS INTEGER.

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = P_Agencia
       Mov_Contable.Comprobante = Cbte
       Mov_Contable.Cuenta = P_Cuenta
       Mov_Contable.Fec_Contable = P_Fecha1
       Mov_Contable.DB = P_DB
       Mov_Contable.CR = P_CR
       Mov_Contable.Comentario = P_Coment
       Mov_Contable.Usuario = P_Usuario
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Cen_Costos = P_CC
       Mov_Contable.Num_Documento = W_DocContab
       Mov_Contable.Fec_Grabacion = P_Fecha2
       Mov_Contable.Hora = P_Hora NO-ERROR.

IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.

IF W_Rev THEN
    ASSIGN Mov_Contable.Doc_Refer = STRING (W_NroTx).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_TaqCoomevaSyA C-Win 
PROCEDURE Gra_TaqCoomevaSyA :
/*------------------------------------------------------------------------------
  Purpose:  Graba taquilla con SyA consignaciones coomeva Efectivo.
  Feb.15/05 GAER.   
------------------------------------------------------------------------------*/
  RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT V_Operacion, 
                   INPUT Producto.CodPto, INPUT W_CtaSyACMV,     INPUT Cta_Caja,
                   INPUT "CR",            INPUT Producto.NitCta, INPUT Producto.Cuenta,     
                   INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                   INPUT W_AgAportes,     INPUT W_Agencia,       INPUT 0,                  
                   INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).

  IF Producto.Ofitem EQ 11 AND Vr_Deducible GT 0 THEN
     ASSIGN Taquilla.Val_Efectivo = Producto.Debe + Vr_Deducible.

  RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT V_Operacion, 
                   INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT W_CtaSyACMV,
                   INPUT "CR",            INPUT Producto.NitCta, INPUT Producto.Cuenta,     
                   INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_AgAportes,           
                   INPUT W_AgAportes,     INPUT W_Agencia,       INPUT 0,                  
                   INPUT W_Usuario,       INPUT 0,               INPUT Producto.Debe, INPUT F_Seg).


  RELEASE Taquilla. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_TaqCoomevaSyACheq C-Win 
PROCEDURE Gra_TaqCoomevaSyACheq :
/*------------------------------------------------------------------------------
  Purpose:  Graba taquilla con SyA consignaciones coomeva en Cheque.
  Feb.15/05 GAER.   
------------------------------------------------------------------------------*/
  FOR EACH Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta                                                           
                   AND   Cheques.W_CodPto EQ Producto.CodPto                                                           
                   AND   Cheques.W_Dto    EQ Producto.DtoRef NO-LOCK:                                                  
      RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco, INPUT V_Operacion,                                         
                       INPUT Producto.CodPto, INPUT W_CtaSyACMV,     INPUT Cta_Banco,                                           
                       INPUT "CR",            INPUT Producto.NitCta, INPUT Producto.Cuenta,                                     
                       INPUT Producto.DtoRef, INPUT Cheques.W_Cheque,INPUT W_Agencia,                                           
                       INPUT W_AgAportes,     INPUT W_Agencia,       INPUT 0,                                                   
                       INPUT W_Usuario,       INPUT Cheques.W_Valor, INPUT 0, INPUT F_Seg).   

      IF Producto.Ofitem EQ 11 AND Vr_Deducible GT 0 THEN
         ASSIGN Taquilla.Val_Cheque = Cheques.W_Valor + Vr_Deducible.         
      
      RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Cheques.W_Banco, INPUT V_Operacion,                                         
                       INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT W_CtaSyACMV,                                           
                       INPUT "CR",            INPUT Producto.NitCta, INPUT Producto.Cuenta,                                     
                       INPUT Producto.DtoRef, INPUT Cheques.W_Cheque,INPUT W_AgAportes,                                           
                       INPUT W_AgAportes,     INPUT W_Agencia,       INPUT 0,                                                   
                       INPUT W_Usuario,       INPUT Cheques.W_Valor, INPUT 0, INPUT F_Seg). 
                                                                                                                                
      RELEASE Taquilla.  

      RUN Gra_CheqTransito(INPUT Cheques.W_Banco, INPUT Cheques.W_Cheque, INPUT Producto.CodPto,                                 
                          INPUT 1,               INPUT Producto.Cuenta, INPUT W_AgAportes,                                  
                          INPUT 0,               INPUT Cheques.W_Valor + Vr_Deducible, INPUT Cheques.W_Canje) NO-ERROR.         
      IF ERROR-STATUS:ERROR THEN                                                                                                
         RETURN ERROR.  
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_Taquilla C-Win 
PROCEDURE Gra_Taquilla :
/*------------------------------------------------------------------------------
  OBSERVACIONES: Permite Almacenar el Registro en Taquilla.       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER T_Autorizo  LIKE Taquilla.Autorizo.
   DEFINE INPUT PARAMETER T_Banco     LIKE Taquilla.Cod_Compensa.
   DEFINE INPUT PARAMETER T_CodOper   LIKE Taquilla.Cod_Operacion.
   DEFINE INPUT PARAMETER T_CodPto    LIKE Taquilla.Cod_Producto.
   DEFINE INPUT PARAMETER T_Cuenta    LIKE Taquilla.Cuenta.
   DEFINE INPUT PARAMETER T_CtraCta   LIKE Taquilla.Cuenta.
   DEFINE INPUT PARAMETER T_Nat       LIKE Taquilla.Naturaleza.
   DEFINE INPUT PARAMETER T_Nit       LIKE Taquilla.Nit.
   DEFINE INPUT PARAMETER T_Nrocuenta LIKE Taquilla.Nro_cuenta.
   DEFINE INPUT PARAMETER T_NumDto    LIKE Taquilla.Num_Documento.
   DEFINE INPUT PARAMETER T_NumRetche LIKE Taquilla.Num_Retcheque.
   DEFINE INPUT PARAMETER T_Agencia   LIKE Taquilla.Agencia.
   DEFINE INPUT PARAMETER T_OfiDes    LIKE Taquilla.Age_Destino.
   DEFINE INPUT PARAMETER T_OfiFue    LIKE Taquilla.Age_Fuente.
   DEFINE INPUT PARAMETER T_TipPto    LIKE Taquilla.Tip_Producto.
   DEFINE INPUT PARAMETER T_Usuario   LIKE Taquilla.Usuario.
   DEFINE INPUT PARAMETER T_ValChe    LIKE Taquilla.Val_Cheque.
   DEFINE INPUT PARAMETER T_ValEfec   LIKE Taquilla.Val_Efectivo.
   DEFINE INPUT PARAMETER T_Segmento  LIKE Clientes.Cod_Segmento.

   CREATE Taquilla.
   ASSIGN Taquilla.Autorizo         = T_Autorizo
          Taquilla.Nro_Transaccion  = W_NumSeq
          Taquilla.Cod_Compensa     = T_Banco
          Taquilla.Cod_Operacion    = T_CodOper
          Taquilla.Cod_Producto     = T_CodPto
          Taquilla.Contabilizar     = FALSE
          Taquilla.Cuenta           = T_Cuenta
          Taquilla.Cta_Contra       = T_CtraCta
          Taquilla.Duracion         = 0
          Taquilla.Est_Linea        = 0
          Taquilla.Fec_Transaccion  = W_Fecha
          Taquilla.Hora_Transaccion = TIME
          Taquilla.Naturaleza       = T_Nat
          Taquilla.Nit              = T_Nit
          Taquilla.Nro_cuenta       = T_Nrocuenta
          Taquilla.Num_Documento    = T_NumDto
          Taquilla.Num_Retcheque    = T_NumRetche.

   ASSIGN Taquilla.Agencia          = T_Agencia
          Taquilla.Age_Destino      = T_OfiDes
          Taquilla.Age_Fuente       = T_OfiFue
          Taquilla.Tip_Producto     = T_TipPto
          Taquilla.Usuario          = T_Usuario
          Taquilla.Val_Cheque       = T_ValChe
          Taquilla.Val_Efectivo     = T_ValEfec
          Taquilla.Estacion         = W_Estacion
          Taquilla.Cod_Segmento     = T_Segmento.

   IF T_NDd NE "" THEN                    
      Taquilla.Descripcion = T_NDd.              
   ELSE                                          
      Taquilla.Descripcion = "".     

   IF T_Imp THEN                                 
      Taquilla.Descripcion = "Impuestos".        
    
   IF T_ValEfec GT W_MaxDia THEN DO:
      ASSIGN Taquilla.Id_NUD = YES.
      MESSAGE "Transaccion Supera Monto Efectivo Dia" VIEW-AS ALERT-BOX.
   END.                                      
   /* Nuevo*/
   ASSIGN vinotran  = Taquilla.Nro_Transaccion
          vcnit     = Taquilla.Nit
          wxnat     = Taquilla.Naturaleza.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hallaFecProxLiq C-Win 
PROCEDURE hallaFecProxLiq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR FProxLiq AS DATE.
  DEFI VAR NMes     AS INTEG FORMAT "99".
 
  ASSIGN NMes     = MONTH(W_Fecha) + 1        /*Inicia en mensual*/         
         FProxLiq = W_Fecha.

  IF DAY(FProxLiq) EQ 31 THEN
     FProxLiq = W_Fecha - 1.

  IF Ahorros.Per_liquidacion EQ 3 THEN          /*Liq.Trimestral*/ 
     NMes = MONTH(W_Fecha) + 3.
  ELSE IF Ahorros.Per_liquidacion EQ 4 THEN     /*Liq.Semestral*/       
     NMes  = MONTH(W_Fecha) + 6.
  ELSE IF Ahorros.Per_liquidacion EQ 5 THEN       /*Liq.Anual*/  
     NMes = MONTH(W_Fecha) + 12.
  ELSE IF Ahorros.Per_liquidacion EQ 6 THEN       /*Liq.al vcto contractuales*/  
     NMes = MONTH(W_Fecha) + ROUND(Ahorros.Plazo / 30,0).

  IF NMes GE 13 THEN DO:
     ASSIGN NMes = NMes - 12.

     IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
        Ahorros.Fec_ProLiquidacion = DATE(NMes,28,YEAR(FProxLiq) + 1) - 1.
     ELSE
        Ahorros.Fec_ProLiquidacion = DATE(NMes,DAY(FProxLiq),YEAR(FProxLiq) + 1) - 1.
  END.
  ELSE DO:
     IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
        Ahorros.Fec_ProLiquidacion = DATE(NMes,28,YEAR(FProxLiq)) - 1.       
     ELSE
        Ahorros.Fec_ProLiquidacion = DATE(NMes,DAY(FProxLiq),YEAR(FProxLiq)) - 1.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_FVcto C-Win 
PROCEDURE Halla_FVcto :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR NMes      AS INTEG FORMAT "99"  INIT 0.
  DEFI VAR FProxVcto AS DATE.
  DEFI VAR NroM      AS INTEG FORMAT "99"  INIT 0.
  DEFI VAR FContVcto AS DATE.
 
  ASSIGN FProxVcto = W_Fecha + Ahorros.Plazo 
         FContVcto = W_Fecha 
         NMes      = MONTH(W_Fecha)
         NroM      = ROUND(Ahorros.Plazo / 30,0). 

  IF DAY(W_Fecha) EQ 31 THEN
     FContVcto = W_Fecha - 1.

  IF Ahorros.Plazo GT 31 THEN DO:        
     ASSIGN NMes = Nmes + NroM.

     IF NMes GE 25 THEN DO:                                                            
        ASSIGN NMes = NMes - 24.

        IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
           FProxVcto = DATE(NMes,28,YEAR(W_Fecha) + 2).
        ELSE 
           FProxVcto = DATE(NMes,DAY(FContVcto),YEAR(W_Fecha) + 2). 
     END.
     ELSE IF NMes GE 13 THEN DO:                                                            
        ASSIGN NMes = NMes - 12.

        IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
           FProxVcto = DATE(NMes,28,YEAR(W_Fecha) + 1).  
        ELSE 
           FProxVcto = DATE(NMes,DAY(FContVcto),YEAR(W_Fecha) + 1).
     END.
     ELSE DO:                                                                          
        IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
           FProxVcto = DATE(NMes,28,YEAR(W_Fecha)). 
        ELSE 
           FProxVcto = DATE(NMes,DAY(FContVcto),YEAR(W_Fecha)).
     END.

     ASSIGN Ahorros.Fec_Vencimiento = FProxVcto.
  END.
  ELSE DO:
     NMes = Nmes + 1.

     IF NMes GE 13 THEN DO:                                                            
        ASSIGN NMes = NMes - 12.

        IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
           Ahorros.Fec_Vencimiento = DATE(NMes,28,YEAR(W_Fecha) + 1).  
        ELSE 
           Ahorros.Fec_Vencimiento = DATE(NMes,DAY(FContVcto),YEAR(W_Fecha) + 1).
     END.
     ELSE DO:                                                                          
        IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
           Ahorros.Fec_Vencimiento = DATE(NMes,28,YEAR(W_Fecha)). 
        ELSE
           Ahorros.Fec_Vencimiento = DATE(NMes,DAY(FContVcto),YEAR(W_Fecha)). 
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Impresion C-Win 
PROCEDURE Impresion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VAR RpaVI AS LOGICAL INITIAL FALSE.
     
     W_SiAfilia = FALSE.

     FIND FIRST taquilla WHERE Taquilla.Nro_Transaccion EQ W_SecuenciaImprimir NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Taquilla) THEN DO:
        MESSAGE "No es posible la Impresión...no está disponible Tabla taquilla...?" SKIP
                "W_SecuenciaImprimir : " W_SecuenciaImprimir
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR.
     END.

     /*
     MESSAGE "Impresora(SI)   Ò    Validadora(NO)" VIEW-AS ALERT-BOX QUESTION BUTTONS 
                YES-NO  TITLE "Medio de Impresiòn" UPDATE RpaVI AS LOGICAL.*/
     RpaVI = NO.

     /*FIND FIRST Taquilla WHERE Taquilla.Nro_Transaccion EQ W_SecuenciaImprimir  /* Mayo 13/05 GAER*/
                              AND Taquilla.Naturaleza   EQ "Db" 
                              AND Taquilla.Val_Cheque   GT 0 NO-LOCK NO-ERROR.
     IF NOT AVAIL(Taquilla) THEN*/

     

     IF RpaVI THEN DO:
          
         /*NOT AVAIL(Taquilla) AND*/ 
        /*
        RUN F-Taquilla.r (INPUT W_SecuenciaImprimir, INPUT F_Descripcion:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
        MESSAGE "Aliste papel para Segunda Copia ".
        RUN F-Taquilla.r (INPUT W_SecuenciaImprimir, INPUT F_Descripcion:SCREEN-VALUE).
        */
       /* Impresion en los nuevos formatos de Poscript */
       /*  RUN formatos.r (INPUT "NOTA2MUL", 0, 0, F_Descripcion:SCREEN-VALUE IN FRAME default-frame, w_secuenciaimprimir, 0).*/
         APPLY "entry" TO cmb_tipo IN FRAME {&FRAME-NAME}.
     END.
     ELSE DO:
        FIND FIRST Taquilla WHERE Taquilla.Nro_Transaccion EQ W_SecuenciaImprimir  /* Mayo 13/05 GAER*/
                              AND Taquilla.Tip_Producto    EQ 1
                              AND Taquilla.Naturaleza      EQ "CR" NO-LOCK NO-ERROR.
        IF AVAIL(Taquilla) THEN DO:
           W_SiAfilia = FALSE.
           FIND FIRST Taquilla WHERE Taquilla.Nro_Transaccion EQ W_SecuenciaImprimir  
                                 AND Taquilla.Naturaleza      EQ "Db" NO-LOCK NO-ERROR.
           IF NOT AVAIL(Taquilla) THEN DO:  /*<-------Solo Consignaciones de Ahorros*/
              FIND FIRST Taquilla WHERE Taquilla.Nro_Transaccion EQ W_SecuenciaImprimir
                                  AND  (Taquilla.Cod_operac EQ 040101013 OR
                                        Taquilla.Cod_operac EQ 040101051) NO-LOCK NO-ERROR.
              IF AVAIL(Taquilla) THEN      /*<---Solo Cuota Admón y Consignaciones de Ahorros*/
                 W_SiAfilia = TRUE.
                 RUN Imp_ValidConsigAho(INPUT W_SecuenciaImprimir, INPUT F_Descripcion:SCREEN-VALUE IN FRAME default-frame).
                 RETURN.
           END.
        END.  /*<---Hasta acá, Mayo 13/05 GAER*/

        /*Si hay retiros o son otro pdcto*/
        RUN Imp_Validadora (INPUT W_SecuenciaImprimir, INPUT F_Descripcion:SCREEN-VALUE IN FRAME default-frame).
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir C-Win 
PROCEDURE Imprimir :
/*************************************************************************************** 
   Observaciones : Imprimir el Contenido de la temporal en la validadora despues de
                   no encontrar errores. 
***************************************************************************************/
   FOR EACH Producto WHERE Producto.EC NE "" NO-LOCK:
         CASE SUBSTRING(Producto.TipPto,1,1):
/*           WHEN ""  THEN RUN Imp_Cuentas.*/
             WHEN "1" THEN RUN Imp_Ahorros.
/*           WHEN "2" THEN RUN Imp_Creditos.
             WHEN "4" THEN RUN Imp_Especiales.*/
         END CASE.
   END.
 
/*   RUN F-Taquil.r (INPUT W_SecuenciaImprimir, INPUT W_Comentario).*/
/*   RUN Imp_Taquilla.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Ahorros C-Win 
PROCEDURE Imp_Ahorros :
/*------------------------------------------------------------------------------
  Observaciones : Imprimir las Cuentas de Ahorros.       
------------------------------------------------------------------------------*/
  DEFINE VAR W_CtaPto AS CHARACTER FORMAT "X(30)".
  
  FIND FIRST Ahorros WHERE Ahorros.Nit          EQ Producto.NitCta
                     AND   ahorros.cod_ahorro   EQ Producto.CodPto
                     AND   Ahorros.Cue_Ahorros  EQ Producto.Cuenta
                     AND   Ahorros.Agencia      EQ Producto.Ofitem NO-LOCK NO-ERROR.
  IF AVAILABLE(Ahorros) THEN DO:
     /*IF Ahorros.Tip_Ahorro EQ 3 AND Ahorros.Fec_Apertura EQ W_Fecha THEN DO:
        MESSAGE "Desea imprimir el titulo CDT" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ask AS LOGICAL.
        IF ask THEN 
            RUN F-Atermino.p (INPUT Ahorros.Nit, INPUT Ahorros.Cue_Ahorros).
     END.*/
     ASSIGN W_CtaPto = STRING(Producto.Ofitem,"999") + "-" + SUBSTRING(Producto.TipPto,3,1) + "-" +
                       STRING(Producto.CodPto,"999") + "-" + TRIM(Producto.Cuenta) + "-" + STRING(Producto.D_Cheq,"9").
     IF Producto.EC EQ "E" THEN DO:     /* Efectivo */
        IF Producto.Debe NE 0 THEN DO:  /* Consignación */
/*           RUN Auditoria(INPUT W_CtaPto,INPUT "Consigna Cta.Efectivo",INPUT Producto.DtoRef,
                         INPUT Producto.NomPto,INPUT Producto.Debe, INPUT 0) NO-ERROR.
           RUN Reg_Opera(INPUT 1,INPUT W_CtaPto,INPUT "Consigna Cta.Efectivo",
                         INPUT TRIM(W_Nom_Agencia),INPUT Producto.NomPto,
                         INPUT Ahorros.Sdo_Disponible,INPUT Producto.Debe) NO-ERROR.*/
        END.
        ELSE DO:                        /* Retiro */
/*           RUN Auditoria(INPUT W_CtaPto,INPUT "Retiro Cta. en Efectivo.",INPUT Producto.DtoRef,
                         INPUT Producto.NomPto,INPUT Producto.Haber, INPUT 0) NO-ERROR.
           RUN Reg_Opera(INPUT 1,INPUT W_CtaPto,INPUT "Retiro Cta. en Efectivo.",
                         INPUT TRIM(W_Nom_Agencia),INPUT Producto.NomPto,
                         INPUT Ahorros.Sdo_Disponible,INPUT Producto.Haber) NO-ERROR.*/
        END.
     END.
     ELSE DO:                           /* Cheque */
        IF Producto.Debe NE 0 THEN DO:  /* Consignación */
/*           RUN Auditoria(INPUT W_CtaPto,INPUT "Consigna Cta. en Cheque.",INPUT Producto.DtoRef,
                         INPUT Producto.NomPto,INPUT 0, INPUT Producto.Debe) NO-ERROR.
           RUN Reg_Opera(INPUT 1,INPUT W_CtaPto,INPUT "Consigna Cta. en Cheque.",
                         INPUT TRIM(W_Nom_Agencia),INPUT Producto.NomPto,
                         INPUT Ahorros.Sdo_Disponible,INPUT Producto.Debe) NO-ERROR.*/
        END.
        ELSE DO:                        /* Retiro */           
           /*Comentariado Nov.27/06 GAER, se imprime el total en el Triggers Btn_Grabar*/
           FIND FIRST Formatos WHERE Formatos.Agencia     EQ Producto.Ofitem 
                               AND   Formatos.Cod_Formato EQ Producto.Formato NO-LOCK NO-ERROR.
           IF AVAILABLE(Formatos) THEN DO:
              RUN Imp_Cheque(INPUT Producto.Haber,INPUT Producto.Benefi,INPUT W_Ciudad).
           END.
        END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Cheque C-Win 
PROCEDURE Imp_Cheque :
DEFINE INPUT PARAMETER C_Valor LIKE Ahorros.Sdo_Disponible.
DEFINE INPUT PARAMETER C_Benef AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER C_Ciudad LIKE Ubicacion.Nombre.

DEFINE VAR W_Cadena AS CHARACTER FORMAT "X(150)".
DEFINE VAR W_Monto1 AS CHARACTER FORMAT "X(70)".
DEFINE VAR W_Monto2 AS CHARACTER FORMAT "X(70)".
DEFINE VAR W_Monto3 AS CHARACTER FORMAT "X(70)".
DEFINE VAR W_Rpta AS LOGICAL.
DEFINE VAR vctipo AS CHARAC FORMAT "X(1)".

MESSAGE "Desea Imprimir Ahora el Cheque?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Impresión de Cheque" UPDATE choice2 AS LOGICAL.

CHOICE2 = TRUE.

IF CHOICE2 THEN DO:
    ASSIGN vctipo = "1".

    RUN MontoEsc.r (INPUT C_Valor,
                    INPUT 0,
                    OUTPUT W_Cadena).

    RUN PartirValor IN W_Manija (INPUT W_Cadena,
                                 INPUT 60,
                                 OUTPUT W_Monto1,
                                 OUTPUT W_Monto2,
                                 OUTPUT W_Monto3).

    RUN f-cheque.r(INPUT W_monto1,
                   INPUT W_monto2,
                   INPUT C_benef,
                   INPUT c_ciudad,
                   INPUT C_valor,
                   INPUT F_Descripcion:SCREEN-VALUE IN FRAME default-frame ,
                   INPUT STRING (Cbte),
                   INPUT wxnat,
                   INPUT STRING (W_docContab)).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_CpteEgreso C-Win 
PROCEDURE Imp_CpteEgreso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* ASSIGN vcimpcpte = "                                                               " +                                      */
/*                    STRING(vinrocpte) + "         " + STRING(viagencia,"999") + "    " +                                     */
/*                    STRING(DAY(vdfecha), "99") + "-" + STRING(MONTH(vdfecha), "99") + "-" + STRING(YEAR(vdfecha), "9999") +  */
/*                    CHR(10) + CHR(10) +                                                                                      */
/*                    "               " + TRIM(vcnombre,"X(30)") + CHR(10) +                                                   */
/*                    "          " + TRIM(vcdetalle) + CHR(10) + CHR(10) + CHR(10) +                                           */
/*                    "  " + STRING(vcctadb, "X(14)") + " " + STRING(vdnomctadb, "X(30)") + "   " + "T" +                      */
/*                    "    " + STRING(vcnit,"X(12)") + STRING(vddebito, "$>,>>>,>>>,>>9") + CHR(10) +                          */
/*                    "  " + STRING(vcctacr, "X(14)") + " " + STRING(vdnomctacr, "X(30)") + "   " + "T" +                      */
/*                    "    " + STRING(vcnit,"X(12)") + "              " + STRING(vdcredito, "$>,>>>,>>>,>>9") +                */
/*                    CHR(10) + CHR(10) + CHR(10) + CHR(10) + CHR(10) + CHR(10) +                                              */
/*                    "                                                                   " +                                  */
/*                    STRING(vddebito, "$>,>>>,>>>,>>9") + STRING(vdcredito, "$>,>>>,>>>,>>9") + CHR(10) +                     */
/*                    "      " + STRING(vccheque).                                                                             */
/*                                                                                                                             */
/* PUT UNFORMATTED vcimpcpte SKIP.                                                                                             */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Cuentas C-Win 
PROCEDURE Imp_Cuentas :
/*------------------------------------------------------------------------------
  Observaciones : Imprimir las Cuentas Contables.       
------------------------------------------------------------------------------*/
  IF Producto.EC EQ "E" THEN DO:     /* Efectivo */
     IF Producto.Debe NE 0 THEN DO:  /* Consignación */
/*        RUN Auditoria(INPUT Producto.Cuenta,INPUT "Pago Cta.de Ingresos Efect.", 
                      INPUT Producto.DtoRef,INPUT Producto.NomPto,INPUT Producto.Debe, INPUT 0) NO-ERROR.
        RUN Reg_Opera(INPUT 0,INPUT Producto.Cuenta,INPUT "Pago Cta.de Ingresos Efect.",
                      INPUT TRIM(W_Nom_Agencia),INPUT Producto.NomPto,
                      INPUT 0,INPUT Producto.Debe) NO-ERROR.*/
     END.
     ELSE DO:                        /* Retiro */
/*        RUN Auditoria(INPUT Producto.Cuenta,INPUT "Retirar Cta.de Egresos Efect.", 
                      INPUT Producto.DtoRef,INPUT Producto.NomPto,INPUT Producto.Haber, INPUT 0) NO-ERROR.
        RUN Reg_Opera(INPUT 0,INPUT Producto.Cuenta,INPUT "Retirar Cta.de Egresos Efect.",
                      INPUT TRIM(W_Nom_Agencia),INPUT Producto.NomPto,
                      INPUT 0,INPUT Producto.Haber) NO-ERROR.*/
     END.
  END.
  ELSE DO:                           /* Cheque */
     IF Producto.Debe NE 0 THEN DO:  /* Consignación */
/*        RUN Auditoria(INPUT Producto.Cuenta,INPUT "Pago Cta.de Ingresos Cheque.", 
                      INPUT Producto.DtoRef,INPUT Producto.NomPto,INPUT 0, INPUT Producto.Debe) NO-ERROR.
        RUN Reg_Opera(INPUT 0,INPUT Producto.Cuenta,INPUT "Pago Cta.de Ingresos Cheque.",
                      INPUT TRIM(W_Nom_Agencia),INPUT Producto.NomPto,
                      INPUT 0,INPUT Producto.Debe) NO-ERROR.*/
     END.
     ELSE DO:                        /* Retiro */
/*        RUN Auditoria(INPUT Producto.Cuenta,INPUT "Retirar Cta.de Egresos Cheque.", 
                      INPUT Producto.DtoRef,INPUT Producto.NomPto,INPUT 0, INPUT Producto.Haber) NO-ERROR.
        RUN Reg_Opera(INPUT 0,INPUT Producto.Cuenta,INPUT "Retirar Cta.de Egresos Cheque.",
                      INPUT TRIM(W_Nom_Agencia),INPUT Producto.NomPto,
                      INPUT 0,INPUT Producto.Haber) NO-ERROR.*/
           FIND FIRST Formatos WHERE Formatos.Agencia     EQ Producto.Ofitem 
                               AND   Formatos.Cod_Formato EQ Producto.Formato NO-LOCK NO-ERROR.
           IF AVAILABLE(Formatos) THEN DO:
              /*RUN Imp_ChequeCpte.*/
              RUN Imp_Cheque(INPUT Producto.Haber,INPUT Producto.Benefi,INPUT W_Ciudad).
           END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Especiales C-Win 
PROCEDURE Imp_Especiales :
/*------------------------------------------------------------------------------
  Observaciones : Imprimir las Cuentas especiales.       
------------------------------------------------------------------------------*/
  MESSAGE "ENTRA ESPECIALES."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  DEFINE VAR W_CtaPto AS CHARACTER FORMAT "X(30)".
  
  ASSIGN W_CtaPto = STRING(Producto.Ofitem,"999") + "-" + STRING(Producto.CodPto,"999") + "-" + 
                    Producto.NitCta + "-" + STRING(Producto.D_Cheq,"9").
  
  IF Producto.EC EQ "E" THEN DO:     /* Efectivo */
     IF Producto.Debe NE 0 THEN DO:  /* Consignación */
        RUN Auditoria(INPUT W_CtaPto,INPUT "Pago Pto. Especial Efect.", 
                      INPUT Producto.DtoRef,INPUT Producto.NomPto,INPUT Producto.Debe, INPUT 0) NO-ERROR.
        RUN Reg_Opera(INPUT 4,INPUT W_CtaPto,INPUT "Pago Pto. Especial Efect.",
                      INPUT TRIM(W_Nom_Agencia),INPUT Producto.NomPto,
                      INPUT 0,INPUT Producto.Debe) NO-ERROR.
     END.
  END.
  ELSE DO:                           /* Cheque */
     IF Producto.Debe NE 0 THEN DO:  /* Consignación */
        RUN Auditoria(INPUT W_CtaPto,INPUT "Pago Pto. Especial en Cheque.", 
                      INPUT Producto.DtoRef,INPUT Producto.NomPto,INPUT 0, INPUT Producto.Debe) NO-ERROR.
        RUN Reg_Opera(INPUT 4,INPUT W_CtaPto,INPUT "Pago Pto. Especial en Cheque.",
                      INPUT TRIM(W_Nom_Agencia),INPUT Producto.NomPto,
                      INPUT 0,INPUT Producto.Debe) NO-ERROR.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_REvalidCreditos C-Win 
PROCEDURE Imp_REvalidCreditos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER P_transa   LIKE Taquilla.Nro_Transaccion.
 DEFINE INPUT PARAMETER P_Comentario AS CHARACTER FORMAT "X(62)".

 DEFINE VAR W_NomPcto          AS   CHARACTER FORMAT "X(15)".
 DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NitEnti          LIKE Clientes.Nit.
 DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".
 DEFINE VAR W_TVrCheque        AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99" INIT 0.
 DEFINE VAR W_TValEfe          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99" INIT 0.
 DEFINE VAR W_DescOpe          AS   CHARACTER FORMAT "X(40)".
 DEFINE VAR W_VrOpera          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99" INIT 0.
 DEFINE VAR W_VrConsig         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_VrRetiro         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_PrimerCom        AS   CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario       AS   CHARACTER FORMAT "X(32)" INITIAL "".
 DEFINE VAR W_Nomofi           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NomCli           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_totopera         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR Total_Debito AS DECIMAL.
 DEFINE VAR Total_Credito AS DECIMAL.
 DEFINE VAR W_NomUsu           LIKE Usuarios.Nombre.
 DEFINE VAR W_Rpta             AS   LOGICAL. 
 DEFI   VAR T_MK             LIKE Creditos.Sdo_Capital.
 DEFI   VAR T_MI             LIKE Creditos.Sdo_Capital.
 DEFI   VAR T_SdoF             LIKE Creditos.Sdo_Capital.
 DEFI   VAR T_Sdo0             LIKE Creditos.Sdo_Capital.

FOR EACH Taquilla WHERE Taquilla.Nro_Transaccion EQ P_Transa 
                    AND Taquilla.Fec_Transaccion EQ W_Fecha NO-LOCK BREAK BY Taquilla.Cod_Producto:
      IF  Taquilla.Naturaleza            EQ "Cr" 
      AND SUBSTRING(Taquilla.Cuenta,1,4) NE "2705" 
      AND SUBSTRING(Taquilla.Cuenta,1,4) NE "1904"   THEN
         ASSIGN W_TValEfe   = W_TValEfe   + Taquilla.Val_Efectivo
                W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.

      IF LAST-OF(Taquilla.Cod_Producto) THEN DO:
         ASSIGN W_DescOpe = "Operación no Existe"
                W_NomPcto = ""
                W_NomCli  = "".

         FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
         IF AVAILABLE(Operacion) THEN
            W_DescOpe = Operacion.Nom_Operacion.

         FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Taquilla.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Creditos) THEN 
            W_NomPcto = TRIM(Pro_Creditos.Nom_Producto).

         FIND FIRST Creditos WHERE Creditos.Nit         EQ Taquilla.Nit
                               AND Creditos.Num_Credito EQ INTEG(Taquilla.Nro_Cuenta) NO-LOCK NO-ERROR.
         IF AVAIL(Creditos) THEN DO:
            ASSIGN T_SdoF = Creditos.Sdo_Capital. 
            FIND FIRST Mov_Contable WHERE Mov_Contable.Nit        EQ Creditos.Nit
                                      AND Mov_Contable.Fec_Contab EQ Taquilla.Fec_Transac
                                      AND Mov_Contable.Doc_Refer  EQ STRING(Creditos.Num_Credito)
                                      AND Mov_Contable.Usuario    EQ W_Usuario NO-LOCK NO-ERROR.
            IF AVAIL(Mov_Contable) THEN
               FOR EACH Mov_Creditos WHERE Mov_Creditos.Nit   EQ Taquilla.Nit
                                 AND Mov_Creditos.Num_Credito EQ Creditos.Num_Credito
                                 AND Mov_Creditos.Fecha       EQ Taquilla.Fec_Transac
                                 AND Mov_Creditos.Usuario     EQ W_Usuario
                                 AND Mov_Creditos.Num_Docum   EQ STRING(Mov_Contable.Num_Docum) NO-LOCK:
                   IF Mov_Creditos.Cod_Operac EQ 020101001 THEN
                      ASSIGN T_MK   = T_MK + (Mov_Creditos.Val_Cheq + Mov_Creditos.Val_Efec)
                             T_Sdo0 = Creditos.Sdo_Capital + (Mov_Creditos.Val_Cheq + Mov_Creditos.Val_Efec).
                   ELSE ASSIGN T_MI = T_MI + (Mov_Creditos.Val_Cheq + Mov_Creditos.Val_Efec).
            END.
         END.

         FIND FIRST Clientes WHERE Clientes.nit EQ Taquilla.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN 
            W_Nomcli = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
     
         /*W_Comentario = CAPS(SUBSTRING(TRIM(W_DescOpe),1,16) + " " + TRIM(W_NomPcto)).*/
         IF Taquilla.Descripcion NE "" THEN
            W_Comentario = Taquilla.Descripcion.

         RUN Imp_Valida.P (INPUT 0,STRING(Taquilla.Age_Destino), Taquilla.Tip_Producto,1,Taquilla.Nit,
                           T_Sdo0,T_SdoF,T_MK,T_MI,            
                           Taquilla.Nro_Cuenta,                          
                           W_Comentario, W_Nomcli, W_NomPcto,            
/*Taquilla.Num_Documento*/ NroCpte, W_TValEfe,W_TVrCheque," ",0).      

         ASSIGN W_TValEfe   = 0
                W_TVrCheque = 0
                T_Sdo0      = 0
                T_SdoF      = 0
                T_MK        = 0
                T_MI        = 0
                Si_Reval    = FALSE.
      END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Validadora C-Win 
PROCEDURE Imp_Validadora :
DEFINE INPUT PARAMETER P_transa   LIKE Taquilla.Nro_Transaccion.
 DEFINE INPUT PARAMETER P_Comentario AS CHARACTER FORMAT "X(62)".

 DEFI VAR NroTxImp LIKE Taquilla.Nro_Transaccion.
 
 DEFI VAR T_NoVlr LIKE Ahorros.Sdo_Dispon INIT 0.
 DEFI VAR P_IdtAhV AS INTEG FORM "9".

 DEFINE VAR W_NomPcto          AS   CHARACTER FORMAT "X(15)".
 DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NitEnti          LIKE Clientes.Nit.
 DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".
 DEFINE VAR W_TvrCheque        AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99".
 DEFINE VAR W_TvrEfec          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99".
 DEFINE VAR W_DescOpe          AS   CHARACTER FORMAT "X(40)".
 DEFINE VAR W_VrOpera          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_VrConsig         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_VrRetiro         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_PrimerCom        AS   CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario       AS   CHARACTER FORMAT "X(32)" INITIAL "".
 DEFINE VAR W_Nomofi           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NomCli           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_totopera         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR Total_Debito AS DECIMAL.
 DEFINE VAR Total_Credito AS DECIMAL.
 DEFINE VAR W_Rpta             AS   LOGICAL. 

 FOR EACH Taquilla WHERE Taquilla.Nro_Transaccion EQ P_Transa
                     AND Taquilla.Fec_Transaccion EQ W_Fecha NO-LOCK:
      ASSIGN W_DescOpe = "Operación no Existe"
             W_NomPcto = ""
             W_NomCli  = ""
             P_IdtAhV  = 1.   /*Inicia 1 = Diferente de a la vista*/

      FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
      IF AVAILABLE(Operacion) THEN
         W_DescOpe = Operacion.Nom_Operacion.
                  
     IF Taquilla.Tip_Producto = 1 THEN DO:  /* Ahorros */
        FIND Pro_ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Taquilla.Cod_Producto NO-LOCK NO-ERROR.
        IF AVAILABLE(Pro_Ahorros) THEN DO:
          W_NomPcto = TRIM(Pro_Ahorros.Nom_Producto).

          IF Pro_Ahorros.Tip_Ahorro EQ 1 THEN
             P_IdtAhV = 0.
        END.

        IF Taquilla.Naturaleza = "DB" THEN DO:  /* Es un Retiro */
          ASSIGN W_VrOpera = 0.
          IF Taquilla.Val_Cheque GT 0 THEN 
            ASSIGN W_VrOpera = Taquilla.Val_Cheque
                   W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.
          ELSE
            ASSIGN W_VrOpera = Taquilla.Val_Efectivo
                   W_TvrEfec  = W_TvrEfec + Taquilla.Val_Efectivo.
          Total_Credito  = Total_Credito + w_VrOpera.
        END.
        ELSE DO:
          IF Taquilla.Naturaleza = "CR" THEN DO:  /* Es una Consignacion */
            ASSIGN W_VrOpera = 0.
            IF Taquilla.Val_Cheque GT 0 THEN 
              ASSIGN W_VrOpera = Taquilla.Val_Cheque
                     W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.
            ELSE
              ASSIGN W_VrOpera = Taquilla.Val_Efectivo
                     W_TvrEfec  = W_TvrEfec + Taquilla.Val_Efectivo.
            Total_Debito = Total_Debito + w_VrOpera.
          END.
        END.
      END.
      ELSE DO:  /* Otros Pctos */
          IF Taquilla.Naturaleza = "Cr" THEN DO:
            ASSIGN W_VrOpera = 0.
            IF Taquilla.Val_Cheque GT 0 THEN 
              ASSIGN W_VrOpera = Taquilla.Val_Cheque
                     W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.
            ELSE
              ASSIGN W_VrOpera = Taquilla.Val_Efectivo
                     W_TvrEfec  = W_TvrEfec + Taquilla.Val_Efectivo.
            Total_Credito  = Total_Credito + w_VrOpera.
          END.
          ELSE DO:
            IF Taquilla.Naturaleza = "Db" THEN DO:
              ASSIGN W_VrOpera = 0.
              IF Taquilla.Val_Cheque GT 0 THEN 
                ASSIGN W_VrOpera = Taquilla.Val_Cheque
                       W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.
              ELSE
                ASSIGN W_VrOpera = Taquilla.Val_Efectivo
                       W_TvrEfec  = W_TvrEfec + Taquilla.Val_Efectivo.
              Total_Debito  = Total_Debito + w_VrOpera.
            END.
          END.
      END.

      
      FIND FIRST Clientes WHERE Clientes.nit EQ Taquilla.Nit NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN 
        W_Nomcli = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
      FIND FIRST Ahorros WHERE Ahorros.Nit        EQ Taquilla.Nit
                           AND Ahorros.Cod_Ahorro EQ Taquilla.Cod_Producto
                           AND Ahorros.Cue_Ahorro EQ Taquilla.Nro_Cuenta NO-LOCK NO-ERROR.
      ASSIGN T_SdoF = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje WHEN AVAIL(Ahorros).
     
      ASSIGN W_Comentario = CAPS(SUBSTRING(TRIM(W_DescOpe),1,16) + " " + TRIM(W_NomPcto))
             NroTxImp     = W_DocContab.

      IF Taquilla.Descripcion NE "" THEN
         W_Comentario = Taquilla.Descripcion.

      IF Si_Reval AND NroCpte GT 0 THEN
         NroTxImp = NroCpte.


      RUN Imp_Valida.R (INPUT 0,STRING(Taquilla.Age_Destino), Taquilla.Tip_Producto,P_IdtAhV,Taquilla.Nit,
                              T_SdoI,T_SdoF,T_NoVlr,T_NoVlr,                              
                              Taquilla.Nro_Cuenta,
                              W_Comentario, W_Nomcli, W_NomPcto,
                              NroTxImp, W_VrOpera,Taquilla.Val_Cheque," ",0).
      
 END.
 Si_Reval = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_ValidConsigAho C-Win 
PROCEDURE Imp_ValidConsigAho :
/*------------------------------------------------------------------------------
  Purpose:   Valida en una sola Impresión Consignaciones de Ahorros.
             (Efectivo y cheques sumarizados) .
  Notes:     Mayo 13/05 GAER.
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER P_transa   LIKE Taquilla.Nro_Transaccion.
  DEFINE INPUT PARAMETER P_Comentario AS CHARACTER FORMAT "X(62)". 

  DEFI VAR NroTxImp LIKE Taquilla.Nro_Transaccion.

  DEFI VAR T_NoVlr LIKE Ahorros.Sdo_Dispon INIT 0.
  DEFI VAR P_IdtAhV AS INTEG FORM "9".
                                                                                 
  DEFINE VAR W_NomPcto          AS   CHARACTER FORMAT "X(15)".                   
  DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".                   
  DEFINE VAR W_NitEnti          LIKE Clientes.Nit.                               
  DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".                   
  DEFINE VAR W_TVrCheque        AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99" INIT 0.
  DEFINE VAR W_TVrEfec          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99" INIT 0.
  DEFINE VAR W_DescOpe          AS   CHARACTER FORMAT "X(40)".                   
  DEFINE VAR W_VrOpera          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99" INIT 0.
  DEFINE VAR W_VrConsig         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".       
  DEFINE VAR W_VrRetiro         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".       
  DEFINE VAR W_PrimerCom        AS   CHARACTER FORMAT "X(100)" INITIAL "".       
  DEFINE VAR W_Comentario       AS   CHARACTER FORMAT "X(32)" INITIAL "".        
  DEFINE VAR W_Nomofi           AS   CHARACTER FORMAT "X(30)".                   
  DEFINE VAR W_NomCli           AS   CHARACTER FORMAT "X(30)".                   
  DEFINE VAR W_totopera         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".       
  DEFINE VAR Total_Debito AS DECIMAL.
  DEFINE VAR Total_Credito AS DECIMAL.
  DEFINE VAR W_NomUsu           LIKE Usuarios.Nombre.                            
  DEFINE VAR W_Rpta             AS   LOGICAL.   

  T_SdoF = 0.
  FOR EACH Taquilla WHERE Taquilla.Nro_Transaccion EQ P_Transa
                      AND Taquilla.Fec_Transaccion EQ W_Fecha NO-LOCK
                    BREAK BY Taquilla.Nit BY Taquilla.Cod_Producto BY Taquilla.Nro_Cuenta:
      IF  Taquilla.Naturaleza            EQ "Cr" 
      AND SUBSTRING(Taquilla.Cuenta,1,4) NE "2705" 
      AND SUBSTRING(Taquilla.Cuenta,1,4) NE "1904"   THEN
         ASSIGN W_TVrEfec   = W_TVrEfec   + Taquilla.Val_Efectivo
                W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.

      IF LAST-OF(Taquilla.Nro_Cuenta) THEN DO:
         ASSIGN W_DescOpe = "Operación no Existe"
                W_NomPcto = ""
                W_NomCli  = ""
                P_IdtAhV  = 1.   /*Inicia 1 = Diferente de a la vista*/

         FIND FIRST Ahorros WHERE Ahorros.Nit     EQ Taquilla.Nit
                           AND Ahorros.Cod_Ahorro EQ Taquilla.Cod_Producto
                           AND Ahorros.Cue_Ahorro EQ Taquilla.Nro_Cuenta NO-LOCK NO-ERROR.
         ASSIGN T_SdoF = (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) WHEN AVAIL(Ahorros).

         FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
         IF AVAILABLE(Operacion) THEN
            W_DescOpe = Operacion.Nom_Operacion.

         FIND Pro_ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Taquilla.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Ahorros) THEN DO:
            W_NomPcto = TRIM(Pro_Ahorros.Nom_Producto).
            IF Pro_Ahorros.Tip_Ahorro EQ 1 THEN
               P_IdtAhV = 0.            
         END.

         FIND FIRST Clientes WHERE Clientes.nit EQ Taquilla.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN 
            W_Nomcli = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).



         ASSIGN W_Comentario = CAPS(SUBSTRING(TRIM(W_DescOpe),1,16) + " " + TRIM(W_NomPcto))
                NroTxImp     = W_DocContab.

         IF Taquilla.Descripcion NE "" THEN
            W_Comentario = Taquilla.Descripcion.

         IF Si_Reval AND NroCpte GT 0 THEN
            NroTxImp = NroCpte.



         IF NOT W_SiAfilia THEN DO:
            RUN Imp_Valida.R (INPUT 0,STRING(Taquilla.Age_Destino), Taquilla.Tip_Producto,P_IdtAhV,Taquilla.Nit,
                           T_SdoI,T_SdoF,T_NoVlr,T_NoVlr,                                                                                   
                           Taquilla.Nro_Cuenta,
                           W_Comentario, W_Nomcli, W_NomPcto,
                           W_DocContab, W_TVrEfec,W_TVrCheque,"ConsigAho",0).
                        /* P_transa = codig transaccion se cambia a nro documento desde el 23 ene 2006 jjmp */
            ASSIGN W_TVrEfec   = 0
                   W_TVrCheque = 0
                   T_SdoF      = 0.
         END.
      END.

      IF W_SiAfilia AND LAST-OF(Taquilla.Nit) THEN DO:
         W_Comentario = "Afiliación Nuevo Asociado".
         RUN Imp_Valida.R (INPUT 0,STRING(Taquilla.Age_Destino), Taquilla.Tip_Producto,P_IdtAhV,Taquilla.Nit,
                           T_SdoI,T_SdoF,T_NoVlr,T_NoVlr,                                                         
                           Taquilla.Nro_Cuenta,
                           W_Comentario, W_Nomcli, W_NomPcto,
                           NroTxImp, W_TVrEfec,W_TVrCheque,"ConsigAho",0).
         ASSIGN W_TVrEfec   = 0
                W_TVrCheque = 0
                T_SdoF      = 0.
      END.
  END.
  Si_Reval = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar C-Win 
PROCEDURE Inicializar :
FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
    IF AVAILABLE Entidad THEN ASSIGN W_MaxDia = Entidad.MaxOp_Efectivo_Dia.
    
    Cmb_Tipo:ADD-LAST("00 - Todos los Productos") IN FRAME {&FRAME-NAME}.
    FOR EACH Varios WHERE Varios.Tipo EQ 12 AND Varios.Codigo LT 5 AND Varios.Estado EQ 1:
        IF Varios.Codigo NE 2 THEN
           Cmb_Tipo:ADD-LAST(STRING(Varios.Codigo,"99") + " - " + Varios.Descripcion).
    END.
    Cmb_Tipo:SCREEN-VALUE = "00 - Todos los Productos".

    /* ADICIONADO POR WILLIAM MARTINEZ 07-11-2008 */
    
    HIDE FRAME F_Foto.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Insert_Cheque C-Win 
PROCEDURE Insert_Cheque :
/*------------------------------------------------------------------------------
  Observaciones: Permite Insertar Una Fila el el Browser Sin Nececidad de 
                 Utilizar los Metodos y con Mayor Control del Proceso.       
  ------------------------------------------------------------------------------*/
  
  DEFINE VAR Old-Num AS INTEGER NO-UNDO.
  
  IF NOT AVAILABLE Cheques THEN 
     FIND LAST Cheques NO-ERROR.
  IF AVAILABLE Cheques THEN 
     ASSIGN Old-Num = Cheques.W_Secue.
  CREATE Cheques.
  ASSIGN Cheques.W_Secue  = Old-Num + 1
         Cheques.W_Orden  = Producto.W_Order
         Cheques.W_CodPto = Producto.CodPto
         Cheques.W_Cuenta = Producto.Cuenta
         Cheques.W_Dto    = Producto.DtoRef
         Cheques.W_Canje  = 1
         Cheques.W_Banco  = ?    
         Cheques.W_Cheque = "" 
         Cheques.W_Valor  = 0. 
  RUN Reorder_BCheq.
  ASSIGN Open-Recid  = RECID(Cheques)
         Open-On-Row = Open-On-Row + 1. 
  RUN Reopen_QCheq.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Insert_Row C-Win 
PROCEDURE Insert_Row :
/*------------------------------------------------------------------------------
  Observaciones: Permite Insertar Una Fila el el Browser Sin Nececidad de 
                 Utilizar los Metodos y con Mayor Control del Proceso.       
   ------------------------------------------------------------------------------*/
  
  DEFINE VAR Old-Num AS DECIMAL DECIMALS 2 NO-UNDO.
  
  IF NOT AVAILABLE Producto THEN 
     FIND LAST Producto NO-ERROR.
  IF AVAILABLE Producto THEN 
     ASSIGN Old-Num = Producto.W_Order.
  CREATE Producto.
  ASSIGN Producto.W_Order = Old-Num + 1
         Producto.TipPto  = ""
         Producto.Cuenta  = Producto.Cuenta:SCREEN-VALUE IN BROWSE BROWSE-5
         Producto.Ofitem  = W_Agencia
         Producto.NomPto  = "Otros Pagos"
         Producto.Debe    = 0
         Producto.Haber   = 0
         Producto.Retiro  = FALSE.
         /*Producto.TipPto  = "04".*/
  RUN Reorder-Browse.
  ASSIGN Open-Recid  = RECID(Producto)
         Open-On-Row = Open-On-Row + 1. 
  RUN Reopen-Query.
  APPLY "ENTRY" TO Producto.Cuenta IN BROWSE BROWSE-5.
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MBBrwVc C-Win 
PROCEDURE MBBrwVc :
DEFINE VAR P_TipPto AS CHARACTER FORMAT "X".
DEFINE VAR W_ForInt LIKE Creditos.For_Interes.
DEFINE VAR W_SdoIntPag LIKE Creditos.Sdo_IntPag.
DEFINE VAR W_Cuota LIKE Creditos.Sdo_IntPag.
DEFINE VAR W_AcuInt LIKE Creditos.Sdo_intpag.
DEFINE VAR W_SalCapMor AS INTEGER.
DEFINE VAR xgmf AS DECIMAL.

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN BROWSE BROWSE-5
        Producto.OfiTem
        Producto.Cuenta.

    ASSIGN F_Nit
           P_TipPto = SUBSTRING(Producto.TipPto,1,1).

    IF P_TipPto EQ "" THEN DO:
        HIDE FRAME Frame-Ahorros.
        HIDE FRAME Frame-Especiales.

        FIND FIRST Tmp_Cuentas WHERE Tmp_Cuentas.Estado EQ 1
                                 AND Tmp_Cuentas.Id_Caja EQ TRUE
                                 AND Tmp_Cuentas.Cuenta EQ Producto.Cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE(Tmp_Cuentas) THEN DO:
            VIEW FRAME Frame-CarCue.

            ASSIGN F_NomCuenta:SCREEN-VALUE IN FRAME Frame-CarCue = Tmp_Cuentas.Nombre
                   Rad-TipoCaja:SCREEN-VALUE IN FRAME Frame-CarCue = STRING(Tmp_Cuentas.Cod_Caja)
                   T_Caja:SCREEN-VALUE IN FRAME Frame-CarCue = STRING(Tmp_Cuentas.Id_Caja)
                   T_Dto:SCREEN-VALUE IN FRAME Frame-CarCue = STRING(Tmp_Cuentas.Id_Doc)
                   T_ManNit:SCREEN-VALUE IN FRAME Frame-CarCue = STRING(Id_Nit).

            IF Tmp_Cuentas.Naturaleza EQ "DB" THEN
                Rad-Nat:SCREEN-VALUE IN FRAME Frame-CarCue = "1".
            ELSE
                IF Tmp_Cuentas.Naturaleza EQ "Cr" THEN
                    Rad-Nat:SCREEN-VALUE IN FRAME Frame-CarCue = "2".
                ELSE
                    Rad-Nat:SCREEN-VALUE IN FRAME Frame-CarCue = "3".
        END.
    END.
    ELSE DO:
        IF P_TipPto EQ "1" THEN DO:
            HIDE FRAME Frame-CarCue.
            HIDE FRAME Frame-Especiales.

            FOR FIRST Pro_Ahorros WHERE pro_ahorros.cod_ahorro EQ Producto.CodPto
                                    AND Pro_Ahorros.Estado EQ 1 NO-LOCK,
                FIRST Ahorros WHERE ahorros.cod_ahorro EQ pro_ahorros.cod_ahorro
                                AND Ahorros.Cue_Ahorros EQ Producto.Cuenta
                                AND Ahorros.Nit EQ F_Nit NO-LOCK:
                
                VIEW FRAME Frame-Ahorros.

                IF Pro_Ahorros.Id_VerSdoTaq EQ TRUE THEN DO:
                    IF Pro_Ahorros.Tip_Ahorro = 1 THEN
                        xgmf = 250 / 251.
                    ELSE
                        xgmf = 1.
                    ASSIGN F_SDisponible1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(xgmf * Ahorros.Sdo_Disponible,"->>>,>>>,>>9.99")
                           F_SdoTot:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.sdo_canje + Ahorros.Sdo_disponible,"->>>,>>>,>>9.99").
                END.
                ELSE
                    ASSIGN F_SDisponible1:SCREEN-VALUE IN FRAME Frame-Ahorros = "0"
                           F_SdoTot:SCREEN-VALUE IN FRAME Frame-Ahorros = "0".

                ASSIGN F_Cuota1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Cuota)
                       F_FecProxLiq1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Fec_ProLiquidacion)
                       F_FecUltLiq1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Fec_UltLiquidacion)
                       F_FecCancela1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Fec_Vencimiento)
                       F_Intporpagar1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Int_Pagar)
                       F_Intsobregiro1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Int_Sobregiro)
                       F_Plazo1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Plazo)
                       F_SCanje1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Sdo_Canje)
                       F_SIntPagados1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Sal_Intpagados)
                       F_Vlrsobregiro1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Sdo_MinCta).

                IF ahorros.tip_ahorro EQ 4 AND ahorros.cod_ahorro EQ 5 THEN DO: /* Producto es aporte obligatorio */
                    F_Cuota1:LABEL = "Cuota Cobro".

                    FIND FIRST Bahorros WHERE BAhorros.nit EQ F_Nit
                                          AND Bahorros.tip_ahorro EQ 2
                                          AND Bahorros.cod_ahorro EQ 221 NO-LOCK NO-ERROR. /* Ahorro Permanente */
                    IF AVAILABLE BAhorros THEN
                        F_Cuota1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Cuota - BAhorros.Cuota).
                    ELSE
                        F_Cuota1:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Ahorros.Cuota).
                END.
                ELSE
                    F_Cuota1:LABEL = "Cuota".

                FIND FIRST Varios WHERE Varios.Tipo EQ 21
                                    AND Varios.Codigo EQ Ahorros.Detalle_Estado NO-LOCK NO-ERROR.
                IF AVAILABLE Varios THEN
                    F_DetalleEstado:SCREEN-VALUE IN FRAME Frame-Ahorros = STRING(Varios.Codigo,"99") + " - " + Varios.Descripcion.
                ELSE
                    F_DetalleEstado:SCREEN-VALUE IN FRAME frame-ahorros = STRING(Ahorros.Detalle_Estado,"99") + " - " + "No Encontrado".

                APPLY "ENTRY" TO Producto.DtoRef IN BROWSE BROWSE-5.
                RETURN NO-APPLY.
            END.
        END.
        ELSE DO:
            IF P_TipPto EQ "4" THEN DO:
                HIDE FRAME Frame-Ahorros.
                HIDE FRAME Frame-CarCue.

                FOR FIRST Pro_Especiales WHERE Pro_Especiales.Cod_Producto EQ Producto.CodPto
                                           AND Pro_Especiales.Estado EQ 1 NO-LOCK,
                    FIRST Especiales WHERE Especiales.Cod_Producto EQ Pro_Especiales.Cod_Producto
                                       AND Especiales.Nit EQ F_Nit NO-LOCK:
                    IF Especiales.For_Pago EQ 1 THEN
                        F-Forpago3:SCREEN-VALUE IN FRAME Frame-Especiales = "CAJA".

                    IF Especiales.For_Pago EQ 2 THEN
                        F-Forpago3:SCREEN-VALUE IN FRAME Frame-Especiales = "NOMINA".

                    IF Especiales.For_Pago EQ 3 THEN
                        F-Forpago3:SCREEN-VALUE IN FRAME Frame-Especiales = "DB.AUTOMATICO".

                    IF Especiales.Per_Pago EQ 1 THEN
                        F-Periodo3:SCREEN-VALUE IN FRAME Frame-Especiales = "SEMANAL".

                    IF Especiales.Per_Pago EQ 2 THEN
                        F-Periodo3:SCREEN-VALUE IN FRAME Frame-Especiales = "DECADAL".

                    IF Especiales.Per_Pago EQ 3 THEN
                        F-Periodo3:SCREEN-VALUE IN FRAME Frame-Especiales = "QUINCENAL".

                    IF Especiales.Per_Pago EQ 4 THEN
                        F-Periodo3:SCREEN-VALUE IN FRAME Frame-Especiales = "MENSUAL".

                    VIEW FRAME Frame-Especiales.

                    ASSIGN F-Cantidad3:SCREEN-VALUE IN FRAME Frame-Especiales = STRING(Especiales.Cantidad)
                           F-CargoInicial3:SCREEN-VALUE IN FRAME Frame-Especiales = STRING(Especiales.Cargo_Inicial)
                           F-Cuopagadas3:SCREEN-VALUE IN FRAME Frame-Especiales = STRING(Especiales.Cuo_Pagadas)
                           F-Cuota3:SCREEN-VALUE IN FRAME Frame-Especiales = STRING(Especiales.Cuota)
                           F-FecCargo3:SCREEN-VALUE IN FRAME Frame-Especiales = STRING(Especiales.Fec_Cargo)
                           F-Fecultpag3:SCREEN-VALUE IN FRAME Frame-Especiales = STRING(Especiales.Fec_UltPago)
                           F-Plazo3:SCREEN-VALUE IN FRAME Frame-Especiales = STRING(Especiales.Plazo)
                           F-Sdopendiente3:SCREEN-VALUE IN FRAME Frame-Especiales = STRING(Especiales.Sdo_Pendiente)
                           F-Secuencia3:SCREEN-VALUE IN FRAME Frame-Especiales = STRING(Especiales.Secuencia)
                           F-Vlracumpagos3:SCREEN-VALUE IN FRAME Frame-Especiales = STRING(Especiales.Vlr_AcumPagos).
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MBCtaTab C-Win 
PROCEDURE MBCtaTab :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permite validar el tab del campo cuenta.       
------------------------------------------------------------------------------*/
   ASSIGN BROWSE BROWSE-5 Producto.Cuenta.
   IF Producto.Cuenta NE "" AND Producto.TipPto  EQ "" THEN DO:
      FIND Tmp_Cuentas WHERE Tmp_Cuentas.Estado  EQ 1
                       AND   Tmp_Cuentas.Id_Caja EQ TRUE 
                       AND   Tmp_Cuentas.Cuenta  EQ Producto.Cuenta NO-LOCK NO-ERROR.
      IF AVAILABLE(Tmp_Cuentas) THEN DO:
         ASSIGN Producto.NomPto:SCREEN-VALUE IN BROWSE BROWSE-5 = Tmp_Cuentas.Nombre.
         IF Cuentas.Id_Doc THEN DO:
            ASSIGN Producto.Id_Tal = 4. 
         END.
         IF Tmp_Cuentas.Id_Nit THEN DO:
            RUN Desactivar.
            ASSIGN F_NitCuenta:SCREEN-VALUE IN FRAME Frame-Cuentas = ""
                   F_NomNit:SCREEN-VALUE IN FRAME Frame-Cuentas = "".
            VIEW FRAME Frame-Cuentas.
            APPLY "Leave" TO F_NitCuenta IN FRAME Frame-Cuentas.
            RETURN NO-APPLY.
         END.
      END.
      ELSE DO:
         IF Producto.Cuenta:SCREEN-VALUE IN BROWSE BROWSE-5 NE "" THEN DO:
            IF NUM-RESULTS("BROWSE-9") NE 0 THEN DO:
               RUN Desactivar.
               {&OPEN-QUERY-BROWSE-9}
               ASSIGN F_NitCuenta:SCREEN-VALUE IN FRAME Frame-Cuentas = ""
                      F_NomNit:SCREEN-VALUE IN FRAME Frame-Cuentas = "".
               VIEW FRAME Frame-Cuentas.
               APPLY "ENTRY" TO BROWSE-9.
               RETURN NO-APPLY.
            END.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MBTabFTipo C-Win 
PROCEDURE MBTabFTipo :
/*------------------------------------------------------------------------------
  OBSERVACIONES :       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Cmb_Tipo.
     F_Tipo = INTEGER(SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,2)).
     CASE F_Tipo:
       /*WHEN 0 THEN DO:
         ASSIGN F_Cuenta:SCREEN-VALUE   = ""
                Com_Producto:LIST-ITEMS = "".
         DISABLE F_Cuenta Com_Producto F_Chequeo. 
         ENABLE  F_Nit.
         APPLY "ENTRY" TO F_Nit.
         RETURN NO-APPLY.
       END.*/
       WHEN 1 THEN DO:
         ASSIGN F_Nit:SCREEN-VALUE      = ""
                F_Nombre:SCREEN-VALUE   = ""
                Com_Producto:LIST-ITEMS = "".
         DISABLE F_Nit.
         ENABLE  Com_Producto F_Cuenta F_Chequeo.
         FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado  EQ 1 
                               NO-LOCK
                              BREAK BY pro_ahorros.cod_ahorro:
             IF FIRST-OF(pro_ahorros.cod_ahorro) THEN DO:
                W_Ok = Com_Producto:ADD-LAST(STRING(pro_ahorros.cod_ahorro,"999") + " - " + STRING(Pro_Ahorros.Nom_Producto,"X(40)")).
             END.
         END.
         APPLY "ENTRY" TO Com_Producto.
         RETURN NO-APPLY.
       END.
/*mdb    WHEN 3 THEN DO:
         ASSIGN F_Nit:SCREEN-VALUE      = ""
                F_Nombre:SCREEN-VALUE   = ""
                Com_Producto:LIST-ITEMS = "".
         DISABLE F_Nit.
         ENABLE  Com_Producto F_Cuenta F_Chequeo.
         FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado       EQ 1 
                              AND   Pro_Ahorros.Agencia      GE T_OfiIni 
                              AND   Pro_Ahorros.Agencia      LE T_OfiFin 
                              AND   Pro_Ahorros.Id_Talonario EQ 2 NO-LOCK
                              BREAK BY pro_ahorros.cod_ahorro:
             IF FIRST-OF(pro_ahorros.cod_ahorro) THEN DO:
                Com_Producto:ADD-LAST(STRING(Pro_Ahorros.Nom_Producto,"X(40)") + " " + STRING(pro_ahorros.cod_ahorro,"999")).
             END.
         END.
         ASSIGN Com_Producto:SCREEN-VALUE = Com_Producto:ENTRY(1)
                W_CodPto = INTEGER(SUBSTRING(Com_Producto:SCREEN-VALUE,42,3)).
         APPLY "ENTRY" TO Com_Producto.
         RETURN NO-APPLY.
       END.*/
       WHEN 4 THEN DO:
         ASSIGN F_Nit:SCREEN-VALUE      = ""
                F_Nombre:SCREEN-VALUE   = ""
                Com_Producto:LIST-ITEMS = "".
         DISABLE F_Cuenta F_Chequeo.
         ENABLE  Com_Producto F_Nit.
         FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia GE T_OfiIni
                                 AND   Pro_Especiales.Agencia LE T_OfiFin
                                 AND   Pro_Especiales.Estado  EQ 1 NO-LOCK
                                 BREAK BY Pro_Especiales.Cod_Producto:
             IF FIRST-OF(Pro_Especiales.Cod_Producto) THEN DO:
                Com_Producto:ADD-LAST(STRING(Pro_Especiales.Nom_Producto,"X(40)") + " " + STRING(Pro_Especiales.Cod_Producto,"999")).
             END.
         END.
         APPLY "ENTRY" TO Com_Producto.
         RETURN NO-APPLY.
       END.
       OTHERWISE DO:
         APPLY "ENTRY" TO F_Agencia.
         RETURN NO-APPLY.
       END.
     END CASE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MBTabNit C-Win 
PROCEDURE MBTabNit :
/*------------------------------------------------------------------------------
  Observaciones : Definir Parametros del Nit.       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE Age_Cliente LIKE Agencias.Agencia.
  DEFINE VARIABLE Wk_Edad AS DECIMAL.
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_Nit Cmb_Tipo Com_Producto.
     F_Tipo = INTEGER(SUBSTRING(Cmb_Tipo:SCREEN-VALUE,1,2)).
     /* Se incluye aviso en taquilla */
     IF F_Nit EQ "" THEN DO:
        IF NUM-RESULTS("BROWSE-5") EQ 0 THEN DO:
           DISABLE F_Nit.
           ASSIGN F_Nombre:SCREEN-VALUE = "".
           IF NUM-RESULTS("BROWSE-9") NE 0 THEN DO:
              FIND FIRST Tmp_Cuentas WHERE Tmp_Cuentas.Id_Caja EQ TRUE NO-LOCK NO-ERROR.
              IF AVAILABLE(Tmp_Cuentas) THEN DO:
                 RUN Insert_Row.
              END.
              ELSE DO:
                 APPLY "ENTRY" TO F_Agencia.
                 RETURN NO-APPLY.
              END.
           END.
           ELSE DO:
              APPLY "ENTRY" TO F_Agencia.
              RETURN NO-APPLY.
           END.
        END.
     END.
     ELSE DO:
        FIND Clientes WHERE Clientes.Nit EQ F_Nit NO-LOCK NO-ERROR.
        IF AVAILABLE (Clientes) THEN DO:
           ASSIGN F_Nombre:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)
                  F_Seg = Clientes.Cod_Segmento.
           IF Clientes.Fec_fallecido NE ? THEN 
              MESSAGE "Este cliente aparece como FALLECIDO fecha: " clientes.Fec_fallecido
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RUN Verifica_CobroJuridico.
        END.
        /* CONTROL DE EDAD */
        Wk_Edad = YEAR(W_Fecha) - year(Clientes.Fec_Nacimiento).
        IF MONTH(Clientes.Fec_Nacimiento) LT MONTH(W_fecha) THEN .
        ELSE 
           IF MONTH(Clientes.Fec_Nacimiento) EQ MONTH(W_fecha) AND
            day(Clientes.Fec_Nacimiento) GT day(W_fecha) THEN 
               Wk_edad = Wk_edad - 1.
        IF Clientes.Tipo_Cliente EQ 2 AND 
              Wk_Edad GE 18 THEN DO:
                  MESSAGE "El Cliente esta identificado como menor de edad" SKIP
                   "sin embargo por fecha de nacimiento ya es mayor de edad" SKIP
                   "Favor solicitar actualizar documento de identificación." SKIP
                    VIEW-AS ALERT-BOX ERROR.
        END.
        IF W_SYA EQ TRUE THEN DO:
           MESSAGE "Desea Ingresar la Transacción por Sucursales y Agencias...?"
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK-CANCEL
           TITLE "Error En Taquilla" UPDATE Choice AS LOGICAL.
           IF Choice THEN DO:
             Cont = Cont + 1.
             CREATE Producto.
             ASSIGN Producto.W_Order = Cont   
                    Producto.OfiTem  = F_Agencia  
                    Producto.TipPto  = STRING(F_Tipo)
                    Producto.Nit     = F_Nit
                    Producto.CodPto  = INTEGER(SUBSTRING(Com_Producto:SCREEN-VALUE,42,45))
                    Producto.NomPto  = SUBSTRING(Com_Producto:SCREEN-VALUE,1,40)
                    Producto.Cuenta  = F_Cuenta.
           END.
           ELSE DO:
             ASSIGN F_Nit:SCREEN-VALUE    = "" 
                    F_Nombre:SCREEN-VALUE = "".
             APPLY "ENTRY" TO F_Agencia.
             RETURN NO-APPLY.
           END.
        END.
        ELSE DO:
          CASE F_Tipo:
            WHEN 0 OR WHEN 5 THEN DO:
              FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado  EQ 1 NO-LOCK, 
                 EACH Ahorros WHERE ahorros.cod_ahorro   EQ pro_ahorros.cod_ahorro
                               AND   Ahorros.Nit             EQ F_Nit                 
                               AND   Ahorros.Estado          EQ 1                     
                               AND   Ahorros.Fec_cancelacion EQ ? NO-LOCK:             
                                   /*AND   (Ahorros.Detalle_estado  = 1 or
                                          Ahorros.Detalle_estado  = 0 or
                                          Ahorros.Detalle_estado  = 2)*/ 
                  IF AVAILABLE(Agencias) THEN DO:
                    IF (Ahorros.Agencia EQ W_Agencia) OR ((Ahorros.Agencia NE W_Agencia) AND (Pro_Ahorros.Id_Linea EQ TRUE)) THEN DO:
                       /*RUN Dig_Chequeo(INPUT Ahorros.Agencia, INPUT Pro_Ahorros.Pro_Digito, INPUT Ahorros.Cue_Ahorros, OUTPUT W_Digito).*/
                       Cont = Cont + 1.
                       CREATE Producto.
                       ASSIGN Producto.W_Order = Cont
                              Producto.OfiTem  = Ahorros.Agencia  
                              Producto.TipPto  = "1-" + STRING(pro_ahorros.tip_ahorro,"9")
                              Producto.CodPto  = ahorros.cod_ahorro
                              Producto.Nit     = Ahorros.Nit
                              Producto.NomPto  = Pro_Ahorros.Nom_Producto
                              Producto.Id_Tal  = Pro_Ahorros.Id_Talonario
                              Producto.Cuenta  = Ahorros.Cue_Ahorros
                              Producto.CtaAnt  = Ahorros.Num_Formato
                              Producto.EstCta  = Ahorros.Detalle_Estado.
                    END.
                 END.
              END.
              FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia  GE T_OfiIni
                                      AND   Pro_Especiales.Agencia  LE T_OfiFin
                                      AND   Pro_Especiales.Estado   EQ 1 NO-LOCK,
                  EACH Especiales     WHERE Especiales.Agencia      EQ Pro_Especiales.Agencia
                                      AND   Especiales.Cod_Producto EQ Pro_Especiales.Cod_Producto
                                      AND   Especiales.Nit          EQ F_Nit 
                                      AND   Especiales.Estado       EQ 1 NO-LOCK:
                  FIND Agencias WHERE Agencias.Agencia EQ Especiales.Agencia AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
                  IF AVAILABLE(Agencias) THEN DO:
                    Cont = Cont + 1.
                    CREATE Producto.
                    ASSIGN Producto.W_Order = Cont
                           Producto.OfiTem  = Especiales.Agencia  
                           Producto.TipPto  = "4-E"
                           Producto.Nit     = Especiales.Nit
                           Producto.CodPto  = Especiales.Cod_Producto
                           Producto.NomPto  = Pro_Especiales.Nom_Producto
                           Producto.Cuenta  = "".
                  END.
              END.
            END.
            WHEN 1 THEN DO:
              FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado      EQ 1 NO-LOCK,
                  EACH Ahorros     WHERE ahorros.cod_ahorro    EQ pro_ahorros.cod_ahorro
                                   AND   Ahorros.Nit             EQ F_Nit 
                                   AND   Ahorros.Estado          EQ 1
                                   AND   Ahorros.Fec_cancelacion EQ ? NO-LOCK:
                  FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
                  IF AVAILABLE(Agencias) THEN DO:
                    IF (Ahorros.Agencia EQ W_Agencia) OR ((Ahorros.Agencia NE W_Agencia) AND (Pro_Ahorros.Id_Linea EQ TRUE)) THEN DO:
                       /*RUN Dig_Chequeo(INPUT Ahorros.Agencia, INPUT Pro_Ahorros.Pro_Digito, INPUT Ahorros.Cue_Ahorros, OUTPUT W_Digito).*/
                       Cont = Cont + 1.
                       CREATE Producto.
                       ASSIGN Producto.W_Order = Cont
                              Producto.OfiTem  = Ahorros.Agencia  
                              Producto.TipPto  = "1-" + STRING(pro_ahorros.tip_ahorro,"9")
                              Producto.CodPto  = ahorros.cod_ahorro
                              Producto.Nit     = Ahorros.Nit
                              Producto.NomPto  = Pro_Ahorros.Nom_Producto
                              Producto.Id_Tal  = Pro_Ahorros.Id_Talonario
                              Producto.Cuenta  = Ahorros.Cue_Ahorros 
                              Producto.CtaAnt  = Ahorros.Num_Formato
                              Producto.EstCta  = Ahorros.Detalle_Estado.
                    END.
                  END.
              END.
            END.
            WHEN 4 THEN DO:
              IF Com_Producto EQ "" THEN DO:
                 FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia      GE T_OfiIni
                                         AND   Pro_Especiales.Agencia      LE T_OfiFin
                                         AND   Pro_Especiales.Estado       EQ 1 NO-LOCK,
                     EACH Especiales     WHERE Especiales.Agencia      EQ Pro_Especiales.Agencia
                                         AND   Especiales.Cod_Producto EQ Pro_Especiales.Cod_Producto
                                         AND   Especiales.Nit          EQ F_Nit 
                                         AND   Especiales.Estado       EQ 1 NO-LOCK:
                     FIND Agencias WHERE Agencias.Agencia EQ Especiales.Agencia AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
                     IF AVAILABLE(Agencias) THEN DO:
                       Cont = Cont + 1.
                       CREATE Producto.
                       ASSIGN Producto.W_Order = Cont
                              Producto.OfiTem  = Especiales.Agencia  
                              Producto.TipPto  = "4-E"
                              Producto.Nit     = Especiales.Nit
                              Producto.CodPto  = Especiales.Cod_Producto
                              Producto.NomPto  = Pro_Especiales.Nom_Producto
                              Producto.Cuenta  = "".
                     END.
                 END.
              END.
              ELSE DO:
                 FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia      GE T_OfiIni
                                         AND   Pro_Especiales.Agencia      LE T_OfiFin
                                         AND   Pro_Especiales.Cod_Producto EQ W_CodPto
                                         AND   Pro_Especiales.Estado       EQ 1 NO-LOCK,
                     EACH Especiales     WHERE Especiales.Agencia      EQ Pro_Especiales.Agencia
                                         AND   Especiales.Cod_Producto EQ Pro_Especiales.Cod_Producto
                                         AND   Especiales.Nit          EQ F_Nit 
                                         AND   Especiales.Estado       EQ 1 NO-LOCK:
                     FIND Agencias WHERE Agencias.Agencia EQ Especiales.Agencia AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
                     IF AVAILABLE(Agencias) THEN DO:
                       Cont = Cont + 1.
                       CREATE Producto.
                       ASSIGN Producto.W_Order = Cont
                              Producto.OfiTem  = Especiales.Agencia  
                              Producto.TipPto  = "4-E"
                              Producto.Nit     = Especiales.Nit
                              Producto.CodPto  = Especiales.Cod_Producto
                              Producto.NomPto  = Pro_Especiales.Nom_Producto
                              Producto.Cuenta  = "".
                     END.
                 END.
              END.
            END.
          END CASE.
        END.
        OPEN QUERY BROWSE-5 FOR EACH Producto.
        IF NUM-RESULTS("BROWSE-5") EQ 0 THEN DO:
           MESSAGE "El Cliente No Tiene Ningun Producto. Verifique.. "
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
           TITLE "Error En Taquilla".
           ASSIGN F_Nit:SCREEN-VALUE    = "" 
                  F_Nombre:SCREEN-VALUE = "".
           APPLY "ENTRY" TO F_Agencia.
           RETURN NO-APPLY.
        END.
        ELSE DO:
          DISABLE F_Nit.
          APPLY "ENTRY" TO Producto.DtoRef IN BROWSE BROWSE-5.
          APPLY "VALUE-CHANGED" TO BROWSE-5.
          RETURN NO-APPLY.
        END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mismo_Cheque C-Win 
PROCEDURE Mismo_Cheque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT W_SiChGir THEN DO: 
     RUN Desactivar. 
     {&OPEN-QUERY-BROWSE-10}
     VIEW FRAME Frame-Cuentas-Bancos.
     APPLY "ENTRY" TO BROWSE-10.
     RETURN NO-APPLY.
  END.
  ELSE DO:
     ASSIGN Producto.Cheque = F_NroCheque:SCREEN-VALUE IN FRAME Frame-Cuentas-Bancos
            Producto.Benefi = F_Beneficiario
            Producto.CueBco = W_NroBcoIgual.

     RUN Activar.

     ASSIGN Producto.EC:SCREEN-VALUE IN BROWSE BROWSE-5 = "C"
            Producto.EC.

     ASSIGN BROWSE BROWSE-5 Producto.EC.
     RUN Actualizar_Valor.
     APPLY "Leave" TO Producto.Retiro IN BROWSE BROWSE-5.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Operacion_Ahorros C-Win 
PROCEDURE Operacion_Ahorros :
DEFI VAR P_ImpAplic LIKE Ahorros.Sdo_Disponible INIT 0.
DEFI VAR EfCh AS INTEG FORM "9" INIT 0.  /*Inicia en efectivo*/ 
DEFI VAR P_Base LIKE Ahorros.Sdo_Disponible INIT 0.
DEFI VAR W_RowifTx AS ROWID.
DEFINE VAR T_Saldo LIKE Ahorros.Sdo_Disponible.
DEFINE VAR T_Difer LIKE Ahorros.Sdo_Disponible.
DEFINE VAR T_Int LIKE Ahorros.Int_Pagar.
DEFINE VAR T_Cap LIKE Ahorros.Sdo_Disponible.

ASSIGN T_Ndd = ""
       T_Ctd = ""
       Vr_Deducible = 0
       T_VIm = 0
       T_CIm = "".

P_ImpAplic = 0.

RUN Armar_Operacion (OUTPUT T_Ope,
                     OUTPUT T_Ded) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.

FIND FIRST Pro_Ahorros WHERE pro_ahorros.tip_ahorro EQ INTEGER(SUBSTRING(Producto.TipPto,3,1))
                         AND pro_ahorros.cod_ahorro EQ Producto.CodPto
                         AND Pro_Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
FIND FIRST Ahorros WHERE Ahorros.Nit EQ Producto.Nit
                     AND ahorros.cod_ahorro EQ pro_ahorros.cod_ahorro
                     AND Ahorros.Cue_Ahorros EQ Producto.Cuenta NO-ERROR.
IF NOT AVAIL(Ahorros) OR AVAIL(Ahorros) AND LOCKED(Ahorros) THEN DO:
    MESSAGE "La Cuenta de ahorros no-existe o Está siendo Accesada por otro Usuario..." SKIP
            "             Continue, Cancele e Intente nuevamente luego del DESBLOQUEO."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    RETURN ERROR.
END.
ELSE DO:
    T_SdoI = Ahorros.Sdo_disponible.

    RUN Buscar_Cuenta(INPUT 1,
                      INPUT Producto.CodPto,
                      INPUT Ahorros.Plazo,
                      INPUT Ahorros.Nit,
                      OUTPUT CtaCble,
                      OUTPUT CtaSYA_Des,
                      OUTPUT CtaSYA_FTE) NO-ERROR.
    IF ERROR-STATUS:ERROR OR CtaCble EQ ? THEN DO:
        RELEASE Ahorros NO-ERROR.
        
        RUN MostrarMensaje IN W_Manija (INPUT 66,
                                        OUTPUT W_Error).

        RETURN ERROR.
    END.

    IF Producto.Debe GT 0 THEN DO:
        RUN Consignacion_Ahorro NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "En el Proc.Consignacion_Ahorro se presentó error...Verfifique por favor."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN ERROR.
        END.
    END.
    ELSE DO:
        FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ T_Ope NO-LOCK NO-ERROR.
        
        IF Producto.EC EQ "C" THEN
            EfCh = 1.

        IF Producto.EC EQ "C" AND NOT W_Rev AND (Producto.CueBco LE " " OR Producto.CueBco EQ ?) THEN DO:
            MESSAGE "Falta seleccionar Banco, Operación Cancelada."
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.

        IF (Ahorros.Sdo_Disponible + Ahorros.Int_Pagar) EQ Producto.Haber THEN DO:
            IF Ahorros.Sdo_Canje NE 0 THEN DO:
                MESSAGE "No es posible cancelar la Cuenta, Tiene Sdo.en Canje."
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            IF INT_Causado NE 0 AND SUBSTRING(Producto.TipPto,3,1) NE "1" THEN DO:
                MESSAGE "No es posible cancelar la Cuenta, Tiene Intereses Causados."
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            IF SUBSTRING(Producto.TipPto,3,1) EQ "3" AND Ahorros.Fec_Vencimiento GT W_Fecha THEN DO:
                IF Ahorros.Fec_Prorroga = ? OR (Ahorros.Fec_Prorroga NE ? AND (W_Fecha - Ahorros.Fec_Prorroga) GT 3) THEN DO:
                    MESSAGE "No es posible cancelar la Cuenta, Vencimiento no Cumplido."
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.
            END.
        END.

        ASSIGN P_Base = Producto.Haber
               W_SiAplGMF = FALSE.

        IF Producto.Retiro AND Ahorros.INT_Pagar GT 0 THEN   /*Solo el capital*/
            P_Base = Producto.Haber - Ahorros.INT_Pagar.
        ELSE
            IF Ahorros.INT_Pagar GT 0 THEN  /*No hay retiro de capital*/
                P_Base = 0.

        IF P_Base GT 0 AND T_Ded GT " " AND Ahorros.Cod_Ahorro NE 23 THEN DO:
            RUN RutGMF.P (INPUT TRUE,
                          INPUT W_Agencia,
                          INPUT Ahorros.Agencia,
                          INPUT 1,
                          INPUT Ahorros.Cod_Ahorro,
                          INPUT Ahorros.Nit,
                          INPUT Ahorros.Cue_Ahorro,
                          INPUT T_Ope,
                          INPUT P_Base,
                          INPUT Cbte,
                          INPUT STRING(W_DocContab),
                          INPUT "Por Taquilla",
                          INPUT 0,
                          INPUT EfCh,
                          OUTPUT P_ImpAplic) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "El Prog: RutGMF.P...Retorno ERROR(Salvando) no se permite la operaciòn..."
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.
        END.

        IF Ahorros.Tip_Ahorro EQ 3 AND Ahorros.INT_Pagar GT 0 AND T_Ded GT " " THEN DO:
            RUN RutGMF.P (INPUT TRUE,
                          INPUT W_Agencia,
                          INPUT Ahorros.Agencia,
                          INPUT 1,
                          INPUT Ahorros.Cod_Ahorro,
                          INPUT Ahorros.Nit,
                          INPUT Ahorros.Cue_Ahorro,
                          INPUT T_Ope,
                          INPUT Ahorros.INT_Pagar,
                          INPUT Cbte,
                          INPUT STRING(W_DocContab),
                          INPUT "Por Taquilla",
                          INPUT 1,
                          INPUT EfCh,
                          OUTPUT P_ImpAplic) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "El Prog: RutGMF.P...Retorno ERROR(Salvando) no se permite la operaciòn..."
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.
        END.

        IF Ahorros.Sdo_Disponible - Producto.Haber LT 0 THEN DO:
            FIND CURRENT Producto NO-ERROR.

            ASSIGN Producto.Haber = Producto.Haber - (Producto.Haber - Ahorros.Sdo_Disponible)
                   W_SiAplGMF = TRUE.

            IF Producto.Retiro AND Ahorros.Int_Pagar GT 0 THEN
                Producto.Haber = Producto.Haber + Ahorros.Int_Pagar.
        END.

        IF Producto.Retiro THEN DO:
            IF Producto.Haber  NE (Ahorros.Sdo_Disponib + Ahorros.Sdo_Canje) + Ahorros.Int_Pagar THEN DO:
                MESSAGE "El sdo-disponible + el sdo-canje+ Ahorros.Int_Pagar no quedan en CERO(0) " SKIP
                        "Oper.Rechazada." SKIP
                        Producto.Haber (Ahorros.Sdo_Disponib + Ahorros.Sdo_Canje) Ahorros.Int_Pagar
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            IF SUBSTRING(Producto.TipPto,3,1) EQ "1" AND Ahorros.INT_Causado GT 0 THEN DO:
                RUN Reversa_Causado NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    MESSAGE "La Reversa del Int-Causado retornó Error...Operación rechazada."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    RETURN ERROR.
                END.
            END.
        END.

        IF Producto.EC EQ "C" AND NOT W_Rev AND Ahorros.Tip_Ahorro NE 3 AND (Producto.Haber LT 500000) THEN DO:
            W_RowifTx = ROWID(Operacion).

            RUN CobraCheque NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                RETURN ERROR.

            FIND FIRST Operacion WHERE ROWID(Operacion) EQ W_RowifTx NO-LOCK NO-ERROR.
        END.

        IF (Pro_Ahorros.Id_Salminimo EQ TRUE AND Pro_Ahorros.Val_SdoMinimo GT (Ahorros.Sdo_Dispon - Producto.Haber) AND NOT Producto.Retiro) AND
           Ahorros.ajuste NE 1 THEN DO:
            MESSAGE "El retiro con GMF y Cobro por cheques, Deja el Sdo-Mínimo Inferior al Configurado,"  SKIP
                    "Error en el Proc.Operacion_Ahorros...Se cancela la operacion" SKIP
                    "Quedaría con Sdo-Mínimo : $" (Ahorros.Sdo_Dispon - Producto.Haber)
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.

        RUN Retiro_Ahorro NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Error en el Proc.Retiro_Ahorro...Se cancela la operacion"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.
END.

IF Producto.Debe GT 0 THEN DO:
    RUN Grabar_Deducible NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error Al Grabar los Deducibles. Se cancela la operacion"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Operacion_Cuenta C-Win 
PROCEDURE Operacion_Cuenta :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permite Definir la Operacion que Corresponde a la Cuenta
                  Contable de Ingreso o Egreso.       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER P_Cuenta LIKE Cuentas.Cuenta.
   DEFINE INPUT  PARAMETER P_Clase  AS   INTEGER FORMAT "9".
   DEFINE INPUT  PARAMETER P_Tipo   AS   INTEGER FORMAT "9".
   DEFINE INPUT  PARAMETER P_EC     AS   INTEGER FORMAT "9".
   DEFINE OUTPUT PARAMETER P_Opera  LIKE Operacion.Cod_Opera.
   
   FIND FIRST Operacion WHERE Operacion.Cuenta    EQ P_Cuenta
                  AND   Operacion.Clase_Operacion EQ 1
                  AND   Operacion.Tipo_Operacion  EQ P_Tipo  /*1Consig, 2 Retiros*/
                  AND   Operacion.Tipo_Producto   EQ 4  /*Contables*/ NO-LOCK NO-ERROR.
   IF AVAILABLE(Operacion) THEN 
      ASSIGN P_Opera = Operacion.Cod_Opera.
   ELSE DO:
      MESSAGE "No se Halló Operacion para Cuenta : " P_Cuenta SKIP
              "Operacion.Clase_Operacion         : " 1        SKIP
              "Operacion.Tipo_Operacion          : " P_Tipo   SKIP
              "Operacion.Tipo_Producto           : " 4        SKIP
              "De Operacion Contable"
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Operacion_DC C-Win 
PROCEDURE Operacion_DC :
/*------------------------------------------------------------------------------
  Purpose:       
  Notes:   Sept.26/06 GAER    
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER P_Cuenta LIKE Cuentas.Cuenta.
   DEFINE INPUT  PARAMETER P_Clase  AS   INTEGER FORMAT "9".
   DEFINE INPUT  PARAMETER P_Tipo   AS   INTEGER FORMAT "9".
   DEFINE INPUT  PARAMETER P_EC     AS   INTEGER FORMAT "9".
   DEFINE OUTPUT PARAMETER P_Opera  LIKE Operacion.Cod_Opera.
   
   FIND FIRST Operacion WHERE Operacion.Cuenta    EQ P_Cuenta
                  AND   Operacion.Clase_Operacion EQ 1
                  AND   Operacion.Tipo_Operacion  EQ P_Tipo  /*1Consig, 2 Retiros, 3 D/C*/
                  AND   Operacion.Tipo_Producto   EQ 4  /*Contables*/ NO-LOCK NO-ERROR.
   IF AVAILABLE(Operacion) THEN 
      ASSIGN P_Opera = Operacion.Cod_Opera.
   ELSE DO:
      FIND FIRST Operacion WHERE Operacion.Cuenta EQ P_Cuenta
                  AND   Operacion.Clase_Operacion EQ 1
                  AND   Operacion.Tipo_Operacion  EQ 3  /*1Consig, 2 Retiros, 3 D/C*/
                  AND   Operacion.Tipo_Producto   EQ 4  /*Contables*/ NO-LOCK NO-ERROR.
      IF AVAILABLE(Operacion) THEN 
         ASSIGN P_Opera = Operacion.Cod_Opera.
      ELSE DO:
         MESSAGE "No se Halló Operacion para Cuenta : " P_Cuenta SKIP
                 "Operacion.Clase_Operacion         : " 1        SKIP
                 "Operacion.Tipo_Operacion          : " 3        SKIP
                 "Operacion.Tipo_Producto           : " 4        SKIP
                 "De Operacion Contable"
             VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pdfTransaccionesEfectivo C-Win 
PROCEDURE pdfTransaccionesEfectivo :
DEFINE VAR hPdf AS COM-HANDLE NO-UNDO.
DEFINE VAR l-file AS CHARACTER.

VIEW FRAME FormatoEfectivo.

/*RUN Rp_TransaccionEfectivo.R (). 

l-file = "TransaccionesEfectivo\" + STRING(TODAY) + "_" + "" + ".pdf".
hPdf = CtrlFrame-2:AcroPDF.
hPdf:LoadFile(l-file).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Periodo C-Win 
PROCEDURE Periodo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT  PARAMETER P_Dato  AS INTEGER.
  DEFINE OUTPUT PARAMETER P_Valor AS INTEGER.
  
  CASE P_Dato:
    WHEN 1 THEN DO:
       ASSIGN P_Valor = 1.
    END.
    WHEN 2 THEN DO:
       ASSIGN P_Valor = 30.
    END.
    WHEN 3 THEN DO:
       ASSIGN P_Valor = 90.
    END.
    WHEN 4 THEN DO:
       ASSIGN P_Valor = 180.
    END.
    WHEN 5 THEN DO:
       ASSIGN P_Valor = 360.
    END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reg_Opera C-Win 
PROCEDURE Reg_Opera :
/*------------------------------------------------------------------------------
   Observaciones : Permite Imprimir Por Validadora el Registro de la Transacción
                   en el Comprobante o en la Libreta.      
  ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER P_TipPro   AS INTEGER FORMAT "9".
    DEFINE INPUT PARAMETER P_Cuenta   AS CHAR    FORMAT "X(26)".
    DEFINE INPUT PARAMETER P_Descrip  AS CHAR    FORMAT "X(30)".
    DEFINE INPUT PARAMETER P_Agencia  AS CHAR    FORMAT "X(15)".
    DEFINE INPUT PARAMETER P_NomPto   AS CHAR    FORMAT "X(26)".
    DEFINE INPUT PARAMETER P_SdoDispo AS DECIMAL FORMAT "-***,***,***,**9.99".
    DEFINE INPUT PARAMETER P_Valor    AS DECIMAL FORMAT "-***,***,***,**9.99".
    
    DEFINE VAR   W_Rpta    AS LOGICAL.
    DEFINE VAR   W_FecTra  AS CHAR FORMAT "X(30)".
    
    ASSIGN W_FecTra  = STRING(TODAY,"99/99/9999") + "  -  " + STRING(TIME,"HH:MM:SS AM").
           
    MESSAGE "Inserte Documento Para Validar la Operación" SKIP
            "Del Producto : " P_NomPto
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
    TITLE "Validar Transacción".
    REPEAT:
       OUTPUT TO COM1.
       PUT CONTROL CHR(27) + "@". 
       PUT CONTROL CHR(27) + "f" + CHR(1) + CHR(37) + CHR(10). 
       PUT CONTROL CHR(27) + "!" + CHR(1). 
       PUT CONTROL CHR(27) + "c" + "0" + CHR(4).
       PUT CONTROL CHR(27) + "L".
       PUT CONTROL CHR(27) + "V" + CHR(1).
       PUT CONTROL CHR(27) + "T" + CHR(48).

       PUT SKIP(1).
       PUT W_FecTra             SKIP.
       PUT P_Descrip            SKIP.
       PUT "Agencia:" P_Agencia SKIP.
       PUT "Pcto  :"  P_NomPto  SKIP.
       PUT "Cuenta:"  P_Cuenta  SKIP.
       IF P_TipPro EQ 0 THEN DO:
          PUT "Valor : " P_Valor.
       END.
       IF P_TipPro EQ 1 THEN DO:
          PUT "Valor     : " P_Valor    SKIP.
          PUT "Disponible: " P_SdoDispo SKIP.
       END.
       IF P_TipPro EQ 2 THEN DO:
          PUT "Vlr.Pagado:" P_Valor.
       END.
       IF P_TipPro EQ 4 THEN DO:
          PUT "Vlr.Pagado:" P_Valor.
       END.
       PUT CONTROL CHR(12).
       PUT CONTROL CHR(27) + "q".
       OUTPUT CLOSE.
       RUN MostrarMensaje IN W_Manija(INPUT 262,OUTPUT W_Rpta).
       IF NOT W_Rpta THEN
          LEAVE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reopen-Query C-Win 
PROCEDURE Reopen-Query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR I AS INTEGER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    OPEN QUERY BROWSE-5 FOR EACH Producto.
    IF Open-On-Row > 0 THEN 
       BROWSE-5:SET-REPOSITIONED-ROW (Open-On-Row, "CONDITIONAL":U).
    REPOSITION BROWSE-5 TO RECID Open-Recid NO-ERROR.
    BROWSE-5:SELECT-ROW(BROWSE-5:FOCUSED-ROW) NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reopen_QCheq C-Win 
PROCEDURE Reopen_QCheq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR I AS INTEGER NO-UNDO.
  
  DO WITH FRAME Frame_Cheques:
       OPEN QUERY BROWSE-7 FOR EACH Cheques WHERE Cheques.W_Cuenta EQ Producto.Cuenta
                                            AND   Cheques.W_CodPto EQ Producto.CodPto
                                            AND   Cheques.W_Dto    EQ Producto.DtoRef.
       IF Open-On-Row > 0 THEN 
          BROWSE-7:SET-REPOSITIONED-ROW (Open-On-Row, "CONDITIONAL":U).
       REPOSITION BROWSE-7 TO RECID Open-Recid NO-ERROR.
       BROWSE-7:SELECT-ROW(BROWSE-7:FOCUSED-ROW) NO-ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reorder-Browse C-Win 
PROCEDURE Reorder-Browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR I AS INTEGER INITIAL 0 NO-UNDO.
  DEFINE BUFFER Tmp_Producto FOR Producto.
  
  REPEAT PRESELECT EACH Tmp_Producto BY Tmp_Producto.W_Order:
    FIND NEXT Tmp_Producto.
    ASSIGN I                    = I + 1
           Tmp_Producto.W_Order = I.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reorder_Bcheq C-Win 
PROCEDURE Reorder_Bcheq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR I AS INTEGER NO-UNDO.
  
  DEFINE BUFFER Tmp_Cheques FOR Cheques.
  
  REPEAT PRESELECT EACH Tmp_Cheques BY Tmp_Cheques.W_Orden:
    FIND NEXT Tmp_Cheques.
    ASSIGN I = I + 1
           Tmp_Cheques.W_Secue = I.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reportarVisionamosAh C-Win 
PROCEDURE reportarVisionamosAh :
DEFINE INPUT PARAMETER tipo AS INTEGER.
DEFINE INPUT PARAMETER vValor AS DECIMAL.

FIND FIRST tarjetas WHERE tarjetas.nit = ahorros.nit
                      AND tarjetas.estado = '01'
                      AND tarjetas.cue_ahorros = ahorros.cue_ahorros NO-LOCK NO-ERROR.
IF AVAILABLE tarjetas THEN DO:
    FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.

    CREATE reportarVisionamos.
    ASSIGN reportarVisionamos.fecha = w_fecha
           reportarVisionamos.agencia = w_agencia
           reportarVisionamos.nit = ahorros.nit
           reportarVisionamos.numCuenta = ahorros.cue_ahorros
           reportarVisionamos.tarjeta = tarjetas.tarjetaDB
           reportarVisionamos.tipoCuenta = 'AH'
           reportarVisionamos.tipoDoc = clientes.tipo_identificacion
           reportarVisionamos.valor = vValor
           reportarVisionamos.usuario = w_usuario
           reportarVisionamos.estado = 1.

    IF tipo = 1 THEN
        reportarVisionamos.tipoTrans = 'C'.
    ELSE
        reportarVisionamos.tipoTrans = 'R'.

    RELEASE reportarVisionamos.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reportarVisionamosCr C-Win 
PROCEDURE reportarVisionamosCr :
DEFINE INPUT PARAMETER tipo AS INTEGER.
DEFINE INPUT PARAMETER vValor AS DECIMAL.

FIND FIRST tarjetas WHERE tarjetas.nit = creditos.nit
                      AND tarjetas.estado = '01'
                      AND tarjetas.num_credito = creditos.num_credito NO-LOCK NO-ERROR.
IF AVAILABLE tarjetas THEN DO:
    FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.

    CREATE reportarVisionamos.
    ASSIGN reportarVisionamos.fecha = w_fecha
           reportarVisionamos.agencia = w_agencia
           reportarVisionamos.nit = creditos.nit
           reportarVisionamos.numCuenta = STRING(creditos.num_credito)
           reportarVisionamos.tarjeta = tarjetas.tarjetaDB
           reportarVisionamos.tipoCuenta = 'CR'
           reportarVisionamos.tipoDoc = clientes.tipo_identificacion
           reportarVisionamos.valor = vValor
           reportarVisionamos.usuario = w_usuario
           reportarVisionamos.estado = 1.

    IF tipo = 1 THEN
        reportarVisionamos.tipoTrans = 'C'.
    ELSE
        reportarVisionamos.tipoTrans = 'R'.

    RELEASE reportarVisionamos.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Retiro_Ahorro C-Win 
PROCEDURE Retiro_Ahorro :
DEFINE VAR V_Sobre LIKE Ahorros.Sdo_Disponible INITIAL 0.
DEFINE VAR V_Saldo LIKE Ahorros.Sdo_Disponible INITIAL 0.
DEFINE VAR V_Efectivo LIKE Ahorros.Sdo_Disponible.
DEFINE VAR V_Cheque LIKE Ahorros.Sdo_Disponible.
DEFI VAR Cta_Contra LIKE Cuentas.Cuenta.

FIND FIRST Operacion WHERE Operacion.Cod_operacion EQ T_Ope NO-LOCK NO-ERROR.
IF AVAILABLE Operacion THEN
    W_CtaBco = Operacion.Cuenta.
ELSE DO:
    MESSAGE "No existe la operaciòn : " Producto.OperaAux SKIP
            "Revise por favor."
        VIEW-AS ALERT-BOX.

    RETURN ERROR.
END.

IF Producto.Retiro THEN DO:
    IF SUBSTRING(Producto.TipPto,3,1) EQ "2" OR SUBSTRING(Producto.TipPto,3,1) EQ "3" THEN DO: /*si es contractual o a termino*/
        IF Ahorros.Sdo_Disponible + Ahorros.INT_Pagar NE Producto.Haber THEN DO:
            IF NOT W_SiAplGMF THEN DO:
                MESSAGE "Disponible mas Int-Pagar Diferente del Vr.del Retiro." SKIP
                        "Revise por favor...No se acepta la operación"
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.
        END.

        ASSIGN T_Difer = Producto.Haber - Ahorros.INT_Pagar
               Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - T_Difer
               Ahorros.Num_RetDia = Ahorros.Num_RetDia + 1
               Ahorros.Val_RetDia = Ahorros.Val_RetDia + T_Difer
               Ahorros.Num_RetMes = Ahorros.Num_RetMes + 1
               Ahorros.Val_RetMes = Ahorros.Val_RetMes + T_Difer.

        RUN reportarVisionamosAh(INPUT 2,
                                 INPUT T_Difer).
    END.
    ELSE DO:
        ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - Producto.Haber
               T_Difer = Producto.Haber
               Ahorros.Num_RetDia = Ahorros.Num_RetDia + 1
               Ahorros.Val_RetDia = Ahorros.Val_RetDia + T_Difer
               Ahorros.Num_RetMes = Ahorros.Num_RetMes + 1
               Ahorros.Val_RetMes = Ahorros.Val_RetMes + T_Difer.

        RUN reportarVisionamosAh(INPUT 2,
                                 INPUT producto.haber).

        IF ahorros.tarjetaDB NE " " THEN DO:
            IF Producto.EC EQ "C" THEN
                RUN grabar_TmpTarDeb(INPUT Producto.Haber,
                                     INPUT "Cancelacion de cuenta",
                                     INPUT 2).
            ELSE
                RUN grabar_TmpTarDeb(INPUT Producto.Haber,
                                     INPUT "Cancelacion de cuenta",
                                     INPUT 1).
        
        END.
    END.

    Cta_Contra = Cta_Caja.

    IF Producto.EC EQ "C" THEN
        ASSIGN V_Efectivo = 0
               V_Cheque = T_Difer
               Cta_Contra = Producto.CueBco.
    ELSE
        ASSIGN V_Efectivo = T_Difer
               V_Cheque = 0.

    ASSIGN Ahorros.Fec_Ulttransaccion = TODAY
           Ahorros.Fec_Cancelacion = TODAY
           Ahorros.Estado = 2
           Ahorros.Detalle_Estado = 9. /*ESTADO DE CANCELADO*/

    RUN Gra_MovAhorros(INPUT T_Ope,
                       INPUT Producto.CodPto,
                       INPUT Producto.Cuenta, 
                       INPUT Producto.DtoRef,
                       INPUT Producto.OfiTem,
                       INPUT W_Agencia,
                       INPUT Producto.OfiTem,
                       INPUT Producto.UsuAut,
                       INPUT 0,
                       INPUT T_Difer,
                       INPUT Ahorros.Nit).

    Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.

    IF v_efectivo > 0 THEN
        movProductos.sdo_disponible = T_Difer.

    IF v_cheque > 0 THEN
        movProductos.sdo_canje = T_Difer.

    movProductos.tipo_transaccion = 2.

    IF PRoducto.OfiTem EQ W_Agencia THEN DO:
        RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT T_Ope,            INPUT Producto.CodPto,  INPUT CtaCble,      INPUT Cta_Contra,       INPUT "DB",
                         INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                         INPUT "1",             INPUT W_Usuario,            INPUT V_Cheque,         INPUT V_Efectivo,       INPUT F_Seg).
    END.
    ELSE DO:
        RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT T_Ope,            INPUT Producto.CodPto,  INPUT CtaSYA_Des,   INPUT Cta_Contra,       INPUT "DB",
                         INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                         INPUT "1",             INPUT W_Usuario,            INPUT V_Cheque,         INPUT V_Efectivo,       INPUT F_Seg).

        RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT T_Ope,            INPUT Producto.CodPto,  INPUT CtaCble,          INPUT CtaSYA_Fte,       INPUT "DB",
                         INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT Producto.OfiTem,  INPUT Producto.OfiTem,  INPUT W_Agencia,
                         INPUT "1",             INPUT W_Usuario,            INPUT V_Cheque,         INPUT V_Efectivo,       INPUT F_Seg).
    END.

    RELEASE Taquilla.

    IF (SUBSTRING(Producto.TipPto,3,1) EQ "2" OR SUBSTRING(Producto.TipPto,3,1) EQ "3") AND Ahorros.INT_Pagar GT 0 THEN DO:
        RUN Gra_MovAhorros(INPUT T_Ope,
                           INPUT Producto.CodPto,
                           INPUT Producto.Cuenta,
                           INPUT Producto.DtoRef,
                           INPUT Producto.OfiTem,
                           INPUT W_Agencia,
                           INPUT Producto.OfiTem,
                           INPUT Producto.UsuAut,
                           INPUT 0,
                           INPUT Ahorros.Int_Pagar,
                           INPUT Ahorros.Nit).

        Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.

        movProductos.INT_pagar = ahorros.INT_pagar.
        movProductos.tipo_transaccion = 2.

        RUN Cta_IntTermino(INPUT 1,
                           INPUT Producto.CodPto,
                           INPUT Ahorros.Nit,
                           OUTPUT CtaCble) NO-ERROR.
        IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 66,
                                            OUTPUT W_Error).
            
            RETURN ERROR.
        END.

        T_NDd = "Canc.Intereses".

        IF Producto.OfiTem EQ W_Agencia THEN DO:
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,   INPUT T_Ope,                INPUT Producto.CodPto,  INPUT CtaCble,          INPUT Cta_Contra,
                             INPUT "DB",            INPUT Ahorros.Nit,      INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,
                             INPUT Producto.OfiTem, INPUT W_Agencia,        INPUT "1",                  INPUT W_Usuario,        INPUT 0,                INPUT Ahorros.Int_Pagar,
                             INPUT F_Seg).

            IF Producto.EC EQ "C" THEN
                ASSIGN Taquilla.Val_Cheque = Ahorros.Int_Pagar
                       Taquilla.Val_Efectivo = 0.
        END.
        ELSE DO:
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT T_Ope,                 INPUT Producto.CodPto,  INPUT CtaSYA_Des,       INPUT Cta_Contra,
                             INPUT "DB",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,   INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                   INPUT W_Usuario,        INPUT 0,                INPUT Ahorros.Int_Pagar,
                             INPUT F_Seg).

            IF Producto.EC EQ "C" THEN
                ASSIGN Taquilla.Val_Cheque = Ahorros.Int_Pagar
                       Taquilla.Val_Efectivo = 0.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT T_Ope,                 INPUT Producto.CodPto,  INPUT CtaCble,          INPUT CtaSYA_Fte,
                             INPUT "DB",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,   INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT Producto.OfiTem,
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                   INPUT W_Usuario,        INPUT 0,                INPUT Ahorros.Int_Pagar,
                             INPUT F_Seg).

            IF Producto.EC EQ "C" THEN
                ASSIGN Taquilla.Val_Cheque = Ahorros.Int_Pagar
                       Taquilla.Val_Efectivo = 0.
        END.

        RELEASE Taquilla.

        ASSIGN Ahorros.Int_Pagar = 0
               Ahorros.Fec_UltTransaccion = W_Fecha
               T_NDd = "".
    END.
END.
ELSE DO:
    ASSIGN Ahorros.Fec_Ulttransaccion = W_Fecha
           Cta_Contra = Cta_Caja.

    IF SUBSTRING(Producto.TipPto,3,1) EQ "3" THEN DO:
        ASSIGN Ahorros.INT_Pagar = Ahorros.INT_Pagar - Producto.Haber
               T_NDd = "Canc.Intereses".

        IF Producto.EC EQ "C" THEN
            ASSIGN V_Efectivo = 0
                   V_Cheque = Producto.Haber
                   Cta_Contra = Producto.CueBco.
        ELSE
            ASSIGN V_Efectivo = Producto.Haber
                   V_Cheque = 0.

        RUN Cta_IntTermino(INPUT 1,
                           INPUT Producto.CodPto,
                           INPUT Ahorros.Nit,
                           OUTPUT CtaCble) NO-ERROR.
        IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 66,
                                            OUTPUT W_Error).

            RETURN ERROR.
        END.
    END.
    ELSE DO:
        V_Saldo = Producto.Haber.

        IF Producto.EC EQ "C" THEN
            ASSIGN V_Efectivo = 0
                   V_Cheque = V_Saldo
                   Cta_Contra = Producto.CueBco.
        ELSE
            ASSIGN V_Efectivo = V_Saldo
                   V_Cheque = 0.

        ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - Producto.Haber
               Ahorros.Num_RetDia = Ahorros.Num_RetDia + 1
               Ahorros.Val_RetDia = Ahorros.Val_RetDia + Producto.Haber
               Ahorros.Num_RetMes = Ahorros.Num_RetMes + 1
               Ahorros.Val_RetMes = Ahorros.Val_RetMes + Producto.Haber.

        RUN reportarVisionamosAh(INPUT 2,
                                 INPUT producto.haber).

        IF ahorros.tarjetaDB NE " " THEN DO:
            IF Producto.EC EQ "C" THEN
                RUN grabar_TmpTarDeb(INPUT Producto.Haber,
                                     INPUT "Retiro en Cheque",
                                     INPUT 2).
            ELSE
                RUN grabar_TmpTarDeb(INPUT Producto.Haber,
                                     INPUT "Retiro en Efectivo",
                                     INPUT 1).
        END.
    END.

    IF V_Efectivo GT 0 OR V_Cheque GT 0 THEN DO:
        IF Producto.OfiTem EQ W_Agencia THEN
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT T_ope,            INPUT Producto.CodPto,  INPUT CtaCble,      INPUT Cta_Contra,       INPUT "DB",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT V_Cheque,         INPUT V_Efectivo,       INPUT F_Seg).
        ELSE DO:
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT T_ope,            INPUT Producto.CodPto,  INPUT CtaSYA_Des,   INPUT Cta_Contra,       INPUT "DB",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT V_Cheque,         INPUT V_Efectivo,       INPUT F_Seg).

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,   INPUT T_ope,                INPUT Producto.CodPto,  INPUT CtaCble,          INPUT CtaSYA_Fte,
                             INPUT "DB",            INPUT Ahorros.Nit,      INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT Producto.OfiTem,
                             INPUT Producto.OfiTem, INPUT W_Agencia,        INPUT "1",                  INPUT W_Usuario,        INPUT V_Cheque,         INPUT V_Efectivo,
                             INPUT F_Seg).
        END.

        T_NDd = "".

        RELEASE Taquilla.
    END.

    IF V_Sobre GT 0 AND SUBSTRING(Producto.TipPto,3,1) EQ "1" THEN DO:
        IF Producto.EC EQ "C" THEN
            ASSIGN V_Efectivo = 0
                   V_Cheque = V_Sobre.
        ELSE
            ASSIGN V_Efectivo = V_Sobre
                   V_Cheque = 0.

        FIND FIRST Creditos WHERE Creditos.Agencia EQ Ahorros.Agencia
                              AND creditos.cod_credito EQ Pro_Ahorros.ProCre_Asociado
                              AND Creditos.Pagare EQ Ahorros.Cue_Ahorros EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE(Creditos) THEN DO:
            RUN Buscar_Cuenta(INPUT 2,
                              INPUT creditos.cod_credito,
                              INPUT Creditos.Plazo,
                              INPUT Ahorros.Nit,
                              OUTPUT CtaCble,
                              OUTPUT CtaSYA_Des,
                              OUTPUT CtaSYA_FTE) NO-ERROR.
            IF ERROR-STATUS:ERROR OR CtaCble EQ ? THEN DO:
                RUN MostrarMensaje IN W_Manija (INPUT 66,
                                                OUTPUT W_Error).

                RETURN ERROR.
            END.

            IF Producto.OfiTem EQ W_Agencia THEN
                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,   INPUT T_ope,            INPUT Producto.CodPto,  INPUT CtaCble,          INPUT Cta_Caja,
                                 INPUT "DB",            INPUT Creditos.Nit,     INPUT Creditos.Pagare,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,
                                 INPUT Producto.OfiTem, INPUT W_Agencia,        INPUT "1",              INPUT W_Usuario,        INPUT V_Cheque,         INPUT V_Efectivo,
                                 INPUT F_Seg).
            ELSE DO:
                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,   INPUT T_ope,            INPUT Producto.CodPto,  INPUT CtaSYA_Des,       INPUT Cta_Caja,
                                 INPUT "DB",            INPUT Creditos.Nit,     INPUT Creditos.Pagare,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,
                                 INPUT Producto.OfiTem, INPUT W_Agencia,        INPUT "1",              INPUT W_Usuario,        INPUT V_Cheque,         INPUT V_Efectivo,
                                 INPUT F_Seg).

                RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,   INPUT T_ope,            INPUT Producto.CodPto,  INPUT CtaCble,          INPUT CtaSYA_Fte,
                                 INPUT "DB",            INPUT Creditos.Nit,     INPUT Creditos.Pagare,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT Producto.OfiTem,
                                 INPUT Producto.OfiTem, INPUT W_Agencia,        INPUT "1",              INPUT W_Usuario,        INPUT V_Cheque,         INPUT V_Efectivo,
                                 INPUT F_Seg).
            END.

            Creditos.Sdo_Capital = Creditos.Sdo_Capital + V_Sobre.

            IF creditos.cod_credito = 123 THEN
                RUN reportarVisionamosCr(INPUT 2,
                                         INPUT v_sobre).
        END.
    END.

    RELEASE Creditos.

    RUN Gra_MovAhorros(INPUT T_Ope,
                       INPUT Producto.CodPto,
                       INPUT Producto.Cuenta,
                       INPUT Producto.DtoRef,
                       INPUT Producto.OfiTem,
                       INPUT W_Agencia,
                       INPUT Producto.OfiTem,
                       INPUT Producto.UsuAut,
                       INPUT 0,
                       INPUT Producto.Haber,
                       INPUT Ahorros.Nit).

    Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.

    movProductos.sdo_disponible = producto.haber.
    movProductos.tipo_transaccion = 2.
END.

IF Producto.EC EQ "C" AND NOT W_Rev THEN DO:
    RUN Gra_CheqTransito(INPUT Producto.Banco,
                         INPUT Producto.Cheque,
                         INPUT Producto.CodPto,
                         INPUT 4,
                         INPUT Producto.Cuenta,
                         INPUT Producto.Ofitem,
                         INPUT 1,
                         INPUT Producto.Haber,
                         INPUT 0) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        RELEASE Ahorros NO-ERROR.
        
        RETURN ERROR.
    END.
END.

IF Producto.Haber GT 0 THEN DO:
    FIND FIRST Lib_Chequera WHERE Lib_Chequera.Agencia EQ Producto.OfiTem
                              AND Lib_Chequera.Cod_Producto EQ Producto.CodPto
                              AND Lib_Chequera.Cue_Ahorros EQ Producto.Cuenta
                              AND Lib_Chequera.Estado EQ 1 NO-ERROR.
    IF AVAIL(Lib_Chequera) THEN
        Lib_Chequera.Ult_Consec = INTEG (Producto.DtoRef).

    FIND CURRENT Lib_Chequera NO-LOCK NO-ERROR.
END.

IF Producto.Haber GT 0 AND Producto.EC EQ "C" AND NOT W_Rev THEN
    ASSIGN Ahorros.Num_RetDiaCheq = Ahorros.Num_RetDiaCheq + 1
           Ahorros.Val_RetDiaCheq = Ahorros.Val_RetDiaCheq + Producto.Haber.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reversa_Causado C-Win 
PROCEDURE Reversa_Causado :
DEFINE VAR numDocAux AS INTEGER.

FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 1
                       AND Liqui_Int.Cod_Producto EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
IF NOT AVAIL(Liqui_Int) OR (AVAIL(Liqui_Int) AND (Liqui_Int.Cta_CauCr LE "0" OR Liqui_Int.CtaDb_LiqAso LE "0"))  THEN DO:
    MESSAGE "Falta Liqui_Int para el producto:" Ahorros.Cod_Ahorro SKIP
            "con cuentas para reversar los intereses causados."
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

numDocAux = W_DocContab.

IF ahorros.agencia <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
    FIND FIRST comprobantes WHERE comprobantes.agencia = ahorros.agencia
                              AND comprobantes.comprobante = Cbte NO-ERROR.
    IF AVAILABLE comprobantes THEN DO:
        comprobantes.secuencia = comprobantes.secuencia + 1.
        numDocSyA = comprobantes.secuencia.
        W_DocContab = numDocSyA.
        flagCtaSucyAg = TRUE.
    END.
END.
ELSE DO:
    IF ahorros.agencia <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
        FIND FIRST comprobantes WHERE comprobantes.agencia = ahorros.agencia
                                  AND comprobantes.comprobante = Cbte NO-ERROR.
        IF AVAILABLE comprobantes THEN DO:
            numDocSyA = comprobantes.secuencia.
            W_DocContab = numDocSyA.
        END.
    END.
END.

CREATE Mov_Ahorros.
ASSIGN Mov_Ahorros.Cod_Operacion = 010102001
       Mov_ahorros.cod_ahorro = Ahorros.cod_ahorro
       Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorros
       Mov_ahorros.nit = ahorros.nit
       Mov_Ahorros.Fecha = W_Fecha
       Mov_Ahorros.Hora = TIME
       Mov_Ahorros.Num_Documento = STRING(W_DocContab)
       Mov_Ahorros.Cpte = Cbte
       Mov_Ahorros.Agencia = Ahorros.Agencia
       Mov_Ahorros.Age_Fuente = W_Agencia
       Mov_Ahorros.Age_Destino = W_Agencia
       Mov_Ahorros.Usuario = W_Usuario
       Mov_Ahorros.Val_Efectivo = Ahorros.INT_Causado
       Mov_Ahorros.NomApell = W_NomTx
       Mov_Ahorros.Cedula_Trans = W_CedTrans
       Mov_Ahorros.Descrip = "Reversa Causado por Cancelación"
       Mov_Ahorros.Sdo_Dispon = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.

CREATE movProductos.
ASSIGN movProductos.agencia = ahorros.agencia
       movProductos.comprobante = Cbte
       movProductos.estado = 1
       movProductos.fecha = w_fecha
       movProductos.id_producto = ahorros.cue_ahorros
       movProductos.int_causado = ahorros.int_causado
       movProductos.nit = ahorros.nit
       movProductos.num_documento = w_docContab
       movProductos.tipo_producto = 1
       movProductos.tipo_transaccion = 2.

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
       Mov_Contable.Comprobante = Cbte
       Mov_Contable.Cuenta = Liqui_Int.Cta_CauCr
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = "Reversa Causado por Cancelación"
       Mov_Contable.Usuario = W_Usuario
       Mov_contable.Nit = Ahorros.Nit
       Mov_Contable.Cen_Costos = 999
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Num_Documento = W_DocContab
       Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion
       Mov_Contable.Db = Ahorros.INT_Causado.

IF ahorros.agencia <> w_agencia THEN
    ASSIGN mov_Contable.enlace = STRING(numDocAux).

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
       Mov_Contable.Comprobante = Cbte
       Mov_Contable.Cuenta = Liqui_Int.CtaDb_LiqAso
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = "Reversa Causado por Cancelación"
       Mov_Contable.Usuario = W_Usuario
       Mov_contable.Nit = Ahorros.Nit
       Mov_Contable.Cen_Costos = 999
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Num_Documento = W_DocContab
       Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion
       Mov_Contable.Cr = Ahorros.INT_Causado
       Ahorros.INT_Causado = 0.

IF ahorros.agencia <> w_agencia THEN
    ASSIGN mov_Contable.enlace = STRING(numDocAux).

W_DocContab = numDocAux.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SalCapPagar C-Win 
PROCEDURE SalCapPagar :
/*---------------------------------------------------------------------------------
  OBSERVACIONES : Permite Definir el Valor de la Cuota a Cancelar por el Producto.       
  ----------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER S_AcumCap    LIKE Creditos.Sdo_capital.
  DEFINE INPUT  PARAMETER S_AcumCapPag LIKE Creditos.sdo_capPag.
  DEFINE INPUT  PARAMETER S_IntMorCob  LIKE Creditos.Int_MorCobrar.
  DEFINE INPUT  PARAMETER S_AcumInt    LIKE Creditos.int_Corrientes. 
  DEFINE INPUT  PARAMETER S_SdoIntPag  LIKE Creditos.Sdo_IntPag. 
  DEFINE INPUT  PARAMETER S_IntDifCo   LIKE Creditos.Int_difCobro. 
  DEFINE INPUT  PARAMETER S_IntCor     LIKE Creditos.Int_Corrientes. 
  DEFINE INPUT  PARAMETER S_IntAnt     LIKE Creditos.Int_Anticipado.
  DEFINE INPUT  PARAMETER S_ForLiqInt  LIKE Creditos.For_Interes.
  DEFINE INPUT  PARAMETER S_IdPagPar   LIKE Pro_Creditos.Id_PagParcial.
  DEFINE OUTPUT PARAMETER S_ValPagar   LIKE Creditos.Sdo_capital.
  
  DEFINE VAR CapPag LIKE Creditos.Sdo_capital.
  DEFINE VAR IntPag LIKE Creditos.Sdo_capital.
  
  ASSIGN CapPag     = S_AcumCap - S_AcumCapPag
         IntPag     = S_AcumInt - S_SdoIntPag
         S_ValPagar = 0.
  IF CapPag   LT 0 THEN ASSIGN CapPag   = 0.
  IF IntPag   LT 0 THEN ASSIGN IntPag   = 0.
  IF S_IntAnt LT 0 THEN ASSIGN S_IntAnt = 0.
  IF S_IdPagPar EQ TRUE THEN DO:
     ASSIGN S_ValPagar = CapPag + IntPag + S_IntMorCob.
  END.
  ELSE DO:
     ASSIGN S_ValPagar = CapPag + S_IntMorCob + S_IntDifCo + S_IntCor + S_IntAnt.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saldo C-Win 
PROCEDURE Saldo :
/*************************************************************************************
 OBSERVACIONES : Permite Imprimir el Saldo Por La Validadora.
**************************************************************************************/
    DEFINE INPUT PARAMETER P_TipPro AS INTEGER   FORMAT "9".
    DEFINE INPUT PARAMETER P_OfiTjo AS CHARACTER FORMAT "X(26)".
    DEFINE INPUT PARAMETER P_Cuenta AS CHAR      FORMAT "X(26)".
    DEFINE INPUT PARAMETER P_NomPto AS CHARACTER FORMAT "X(26)".
    DEFINE INPUT PARAMETER P_Sdo1   AS DECIMAL   FORMAT "-***,***,***,**9.99".
    DEFINE INPUT PARAMETER P_Sdo2   AS DECIMAL   FORMAT "-***,***,***,**9.99".
    DEFINE INPUT PARAMETER P_Sdo3   AS DECIMAL   FORMAT "-***,***,***,**9.99".
    DEFINE INPUT PARAMETER P_Sdo4   AS DECIMAL   FORMAT "-***,***,***,**9.99".
    DEFINE INPUT PARAMETER P_Fec1   AS DATE      FORMAT "99/99/9999".
    DEFINE INPUT PARAMETER P_Fec2   AS DATE      FORMAT "99/99/9999".
    DEFINE VAR   W_Rpta    AS LOGICAL.
    DEFINE VAR   W_FecTra  AS CHAR FORMAT "X(30)".

    ASSIGN W_FecTra = STRING(TODAY,"99/99/9999") + " - " + STRING(TIME,"HH:MM:SS AM").
           
    REPEAT:
      OUTPUT TO PRINTER.
      PUT CONTROL CHR(27) + "@". 
      PUT CONTROL CHR(27) + "f" + CHR(1) + CHR(57) + CHR(10).
      PUT CONTROL CHR(27) + "!" + CHR(1). 
      PUT CONTROL CHR(27) + "c" + "0" + CHR(4).
      PUT CONTROL CHR(27) + "V" + CHR(1).
      
      PUT SKIP(1).
      PUT "CONSULTA DE SALDOS" SKIP.
      PUT W_FecTra             SKIP.
      PUT "Agencia:" P_OfiTjo  SKIP.
      PUT "Pcto   :" P_NomPto  SKIP.
      PUT "Cuenta :" P_Cuenta  SKIP.

      IF P_TipPro EQ 1 THEN DO: 
         PUT "Disp : "   P_Sdo1 SKIP.
         PUT "Canje: "   P_Sdo2 SKIP.
         PUT "I x P: "   P_Sdo3 SKIP.
         PUT "Ult.Liq: " P_Fec1 SKIP.
         PUT "Fec.Vto: " P_Fec2 SKIP.
      END.

      IF P_TipPro EQ 2 THEN DO:
         PUT "Sal.Capital  : "  P_Sdo1 SKIP.
         PUT "Sal.Interes  : "  P_Sdo2 SKIP.
         PUT "Sal.Int.Mora : "  P_Sdo3 SKIP.
         PUT "S.P.D        : "  P_Sdo4 SKIP.
         PUT "Fec.Ult. Pago: "  P_Fec1 SKIP.
         PUT "Fec.Prox.Pago: "  P_Fec2 SKIP.
      END.

      IF P_TipPro = 4 THEN DO:
         PUT "Vlr.Pagos    : "  P_Sdo1 SKIP.
         PUT "Sdo.Pendiente: "  P_Sdo2 SKIP.
         PUT "Cuotas Pag.  : "  P_Sdo3 FORMAT "9999" SKIP.
         PUT "Fec.Ult.Pago : "  P_Fec1 SKIP.
      END.

      PUT CONTROL CHR(27) + "q".
      OUTPUT CLOSE.

      RUN MostrarMensaje IN W_Manija(INPUT 262,OUTPUT W_Rpta).
      IF NOT W_Rpta THEN
         LEAVE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sdo_Minimo C-Win 
PROCEDURE Sdo_Minimo :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permite Validar la Afectación del Saldo Mínimo del Producto. 
------------------------------------------------------------------------------*/
  IF Pro_Ahorros.Id_Sobregiro EQ FALSE THEN DO:
     IF (Ahorros.Sdo_Disponible - Producto.Haber) LT 0 THEN DO:
        MESSAGE "La Cuenta No Tiene Saldo "  SKIP
                "Disponible Para el Retiro"  SKIP
                "Sdo_disponible = " Ahorros.Sdo_Disponible SKIP
                "Valor Total del Retiro = " Producto.Haber SKIP
                "Tiene una diferencia   = " (Ahorros.Sdo_Disponible - Producto.Haber)
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
        TITLE "Error En Taquilla".
        RETURN ERROR.
     END.
     ELSE DO:
        IF (Ahorros.Sdo_Disponible - Producto.Haber - Val2xmil) LT 0 THEN DO:
           ASSIGN Val2xmil = Producto.Haber / 1.004.
           MESSAGE "No Es Posible Hacer el Retiro. Se Debe Dejar" SKIP
                   "La Cantidad Que Cubra El Impuesto Del 4XMIL." SKIP
                   "El Valor Máximo a Retirar Debe Ser: " Val2xmil
           VIEW-AS ALERT-BOX QUESTION BUTTONS OK
           TITLE "Error En Taquilla".
           RETURN ERROR.
        END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sob_ConEfec C-Win 
PROCEDURE Sob_ConEfec :
DEFINE VAR T_Valor LIKE Creditos.Sdo_Capital INITIAL 0.
DEFINE VAR T_Difer LIKE Creditos.Sdo_Capital INITIAL 0.
DEFINE VAR T_Inter LIKE Creditos.Sdo_Capital INITIAL 0.
DEFINE VAR numDocAux AS INTEGER.

RUN Val_Operacion(INPUT W_Agencia,
                  INPUT W_Grupo,
                  INPUT W_Usuario,
                  INPUT 010101003,
                  OUTPUT W_Error,
                  OUTPUT W_Nomoper).

IF W_Error THEN DO:
    MESSAGE "No está permitida por taquilla la operación..." SKIP
            W_Nomoper
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK.

    RETURN ERROR.
END.

T_Valor = Producto.Debe.

numDocAux = W_DocContab.

IF Producto.OfiTem <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
    FIND FIRST comprobantes WHERE comprobantes.agencia = Producto.OfiTem
                              AND comprobantes.comprobante = Cbte NO-ERROR.
    IF AVAILABLE comprobantes THEN DO:
        comprobantes.secuencia = comprobantes.secuencia + 1.
        numDocSyA = comprobantes.secuencia.
        W_DocContab = numDocSyA.
        flagCtaSucyAg = TRUE.
    END.
END.
ELSE DO:
    IF Producto.OfiTem <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
        FIND FIRST comprobantes WHERE comprobantes.agencia = Producto.OfiTem
                                  AND comprobantes.comprobante = Cbte NO-ERROR.
        IF AVAILABLE comprobantes THEN DO:
            numDocSyA = comprobantes.secuencia.
            W_DocContab = numDocSyA.
        END.
    END.
END.

FIND FIRST Creditos WHERE Creditos.Agencia EQ Ahorros.Agencia
                      AND creditos.cod_credito EQ Pro_Ahorros.ProCre_Asociado
                      AND Creditos.Pagare EQ Ahorros.Cue_Ahorros EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE(Creditos) THEN DO:
    RUN Cta_Sobregiro(INPUT 2,
                      INPUT creditos.cod_credito,
                      INPUT Creditos.Nit,
                      OUTPUT W_LiqIntDb,
                      OUTPUT W_LiqIntCr,
                      OUTPUT W_LiqMor,
                      OUTPUT W_DifCdb,
                      OUTPUT W_DifCcr).
    IF ERROR-STATUS:ERROR THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 66,
                                        OUTPUT W_Error).
        RETURN ERROR.
    END.

    IF Creditos.Int_MorCobrar GT 0 AND T_Valor NE 0 THEN DO:
        IF T_Valor GT Creditos.Int_MorCobrar THEN DO:
            RUN Gra_MovAhorros(INPUT 010102013,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0,
                               INPUT Creditos.Int_MorCobrar,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = creditos.INT_morCobrar.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,   INPUT 010101006,            INPUT Producto.CodPto,  INPUT W_LiqMor,         INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,      INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,
                             INPUT Producto.OfiTem, INPUT W_Agencia,        INPUT "1",                  INPUT W_Usuario,        INPUT 0,                INPUT Creditos.Int_MorCobrar,
                             INPUT F_Seg).

            ASSIGN T_Valor = T_Valor - Creditos.Int_MorCobrar
                   T_Inter = T_Inter + Creditos.Int_MorCobrar
                   Creditos.Int_MorCobrar = 0.
        END.
        ELSE DO:
            RUN Gra_MovAhorros(INPUT 010102013,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0,
                               INPUT T_Valor,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = T_Valor.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,   INPUT 010101006,            INPUT Producto.CodPto,  INPUT W_LiqMor,         INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,      INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,
                             INPUT Producto.OfiTem, INPUT W_Agencia,        INPUT "1",                  INPUT W_Usuario,        INPUT 0,                INPUT T_Valor,
                             INPUT F_Seg).

            ASSIGN Creditos.Int_MorCobrar = Creditos.Int_MorCobrar - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
        END.
    END.

    IF Creditos.Int_difCobro GT 0 AND T_Valor NE 0 THEN DO:
        FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ 010101008 NO-LOCK NO-ERROR.
        IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN
            Cbte = Operacion.Comprobante.
        ELSE DO:
            RUN MostrarMensaje IN W_Manija (INPUT 143,
                                            OUTPUT W_Error).

            RETURN ERROR.
        END.

        RUN Cbte_Agencia IN W_Manija (INPUT Cbte,
                                      INPUT Producto.OfiTem,
                                      OUTPUT Docto,
                                      OUTPUT W_Error).

        IF W_Error THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 144,
                                            OUTPUT W_Eleccion).

            RETURN ERROR.
        END.

        IF T_Valor GT Creditos.Int_difCobro THEN DO:
            RUN Gra_MovAhorros(INPUT 010102014,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0,
                               INPUT Creditos.Int_difCobro,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = creditos.INT_difCobro.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,   INPUT 010101008,            INPUT Producto.CodPto,  INPUT W_LiqIntCr,       INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,      INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,
                             INPUT Producto.OfiTem, INPUT W_Agencia,        INPUT "1",                  INPUT W_Usuario,        INPUT 0,                INPUT Creditos.Int_difCobro,
                             INPUT F_Seg).

            RUN Gra_Movimientos(INPUT Producto.OfiTem,          INPUT Cbte, INPUT W_DifCdb, INPUT W_Fecha,  INPUT "CR", INPUT "Cerrar Cuenta Difícil Cobro",    INPUT W_Usuario,
                                INPUT Creditos.Int_difCobro,    INPUT 999,  INPUT Docto,    INPUT TODAY,    INPUT TIME) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                RETURN ERROR.

            RUN Gra_Movimientos(INPUT Producto.OfiTem,          INPUT Cbte, INPUT W_DifCcr, INPUT W_Fecha,  INPUT "DB", INPUT "Cerrar cuenta Difícil Cobro",    INPUT W_Usuario,
                                INPUT Creditos.Int_difCobro,    INPUT 999,  INPUT Docto,    INPUT TODAY,    INPUT TIME) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                RETURN ERROR.

            ASSIGN T_Valor = T_Valor - Creditos.Int_difCobro
                   T_Inter = T_Inter + Creditos.Int_difCobro
                   Creditos.Int_difCobro = 0.
        END.
        ELSE DO:
            RUN Gra_MovAhorros(INPUT 010102014,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0,
                               INPUT T_Valor,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = T_Valor.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,   INPUT 010101008,            INPUT Producto.CodPto,  INPUT W_LiqIntCr,       INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,      INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,
                             INPUT Producto.OfiTem, INPUT W_Agencia,        INPUT "1",                  INPUT W_Usuario,        INPUT 0,                INPUT T_Valor,
                             INPUT F_Seg).

            RUN Gra_Movimientos(INPUT Producto.OfiTem,  INPUT Cbte, INPUT W_DifCdb, INPUT W_Fecha,  INPUT "CR", INPUT "Cerrar Cuenta Dificil Cobro",    INPUT W_Usuario,
                                INPUT T_Valor,          INPUT 999,  INPUT Docto,    INPUT TODAY,    INPUT TIME) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                RETURN ERROR.

            RUN Gra_Movimientos(INPUT Producto.OfiTem,  INPUT Cbte, INPUT W_DifCcr, INPUT W_Fecha,  INPUT "DB", INPUT "Cerrar Cuenta Difícil Cobro",    INPUT W_Usuario,
                                INPUT T_Valor,          INPUT 999,  INPUT Docto,    INPUT TODAY,    INPUT TIME) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                RETURN ERROR.

            ASSIGN Creditos.Int_difCobro = Creditos.Int_difCobro - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
        END.
    END.

    IF Creditos.Int_MorCobrar GT 0 AND T_Valor NE 0 THEN DO:
        IF T_Valor GT Creditos.Int_MorCobrar THEN DO:
            RUN Gra_MovAhorros(INPUT 010102015,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0,
                               INPUT Creditos.Int_MorCobrar,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = creditos.INT_morCobrar.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,   INPUT 010101007,            INPUT Producto.CodPto,  INPUT W_LiqIntDb,       INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,      INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,
                             INPUT Producto.OfiTem, INPUT W_Agencia,        INPUT "1",                  INPUT W_Usuario,        INPUT 0,                INPUT Creditos.Int_MorCobrar,
                             INPUT F_Seg).

            ASSIGN T_Valor = T_Valor - Creditos.Int_MorCobrar
                   T_Inter = T_Inter + Creditos.Int_morCobrar
                   Creditos.Int_morCobrar = 0.
        END.
        ELSE DO:
            RUN Gra_MovAhorros(INPUT 010102015,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0,
                               INPUT T_Valor,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = T_Valor.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101007,            INPUT Producto.CodPto,  INPUT W_LiqIntDb,   INPUT Cta_Caja,         INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,      INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                    INPUT T_Valor,          INPUT F_Seg).

            ASSIGN Creditos.Int_morCobrar = Creditos.Int_morCobrar - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
        END.
    END.

    IF Creditos.Int_Corrientes GT 0 AND T_Valor NE 0 THEN DO:
        IF T_Valor GT Creditos.Int_Corrientes THEN DO:
            RUN Gra_MovAhorros(INPUT 010102016,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0,
                               INPUT Creditos.Int_Corrientes,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = creditos.INT_corrientes.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101005,        INPUT Producto.CodPto,          INPUT W_LiqIntDb,   INPUT Cta_Caja,         INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,          INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT Creditos.Int_Corrientes,  INPUT F_Seg).

            ASSIGN T_Valor = T_Valor - Creditos.Int_Corrientes
                   T_Inter = T_Inter + Creditos.Int_Corrientes
                   Creditos.Int_Corrientes = 0.
        END.
        ELSE DO:
            RUN Gra_MovAhorros(INPUT 010102016,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0,
                               INPUT T_Valor,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = T_Valor.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101005,        INPUT Producto.CodPto,  INPUT W_LiqIntDb,   INPUT Cta_Caja,         INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT T_Valor,          INPUT F_Seg).

            ASSIGN Creditos.Int_Corrientes = Creditos.Int_Corrientes - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
        END.
    END.

    IF Creditos.Sdo_Capital GT 0 AND T_Valor NE 0 THEN DO:
        IF T_Valor GT Creditos.Sdo_Capital THEN DO:
            T_Difer = T_Valor - Creditos.Sdo_Capital.

            RUN Buscar_Cuenta(INPUT 2,
                              INPUT creditos.cod_credito,
                              INPUT Creditos.Plazo,
                              INPUT Ahorros.Nit,
                              OUTPUT CtaCble) NO-ERROR.
            IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
                RUN MostrarMensaje IN W_Manija (INPUT 66,
                                                OUTPUT W_Error).

                RETURN ERROR.
            END.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101009,        INPUT Producto.CodPto,      INPUT CtaCble,      INPUT Cta_Caja,         INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,      INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT Creditos.Sdo_Capital, INPUT F_Seg).

            RUN Buscar_Cuenta(INPUT 1,
                              INPUT Producto.CodPto,
                              INPUT Ahorros.Plazo,
                              INPUT Ahorros.Nit,
                              OUTPUT CtaCble) NO-ERROR.
            IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
                RUN MostrarMensaje IN W_Manija (INPUT 66,
                                                 OUTPUT W_Error).
                RETURN ERROR.
            END.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101003,        INPUT Producto.CodPto,  INPUT CtaCble,      INPUT Cta_Caja,         INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT T_Difer,          INPUT F_Seg).

            IF creditos.cod_credito = 123 THEN
                RUN reportarVisionamosCr(INPUT 1,
                                         INPUT creditos.sdo_capital).

            Creditos.Sdo_Capital = 0.
        END.
        ELSE DO:
            RUN Buscar_Cuenta(INPUT 2,
                              INPUT creditos.cod_credito,
                              INPUT Creditos.Plazo,
                              INPUT Ahorros.Nit,
                              OUTPUT CtaCble) NO-ERROR.
            IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
                RUN MostrarMensaje IN W_Manija (INPUT 66,
                                                 OUTPUT W_Error).
                RETURN ERROR.
            END.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101009,        INPUT Producto.CodPto,  INPUT CtaCble,      INPUT Cta_Caja,         INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT T_Valor,          INPUT F_Seg).

            Creditos.Sdo_Capital = Creditos.Sdo_Capital - T_Valor.

            IF creditos.cod_credito = 123 THEN
                RUN reportarVisionamos(INPUT 1,
                                       INPUT T_Valor).
        END.
    END.

    ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + (Producto.Debe - T_Inter)
           Ahorros.Fec_Ulttransaccion = TODAY.

    RUN reportarVisionamosAh(INPUT 1,
                             INPUT Producto.Debe - T_Inter).

    IF ahorros.tarjetaDB NE " " THEN
        RUN grabar_TmpTarDeb(Producto.Debe - T_Inter, "Consigna sobregiro en efectivo", 3).
     
    RUN Gra_MovAhorros(INPUT 010101003,
                       INPUT Producto.CodPto,
                       INPUT Producto.Cuenta,
                       INPUT Producto.DtoRef,
                       INPUT Producto.OfiTem,
                       INPUT W_Agencia,
                       INPUT Producto.OfiTem,
                       INPUT Producto.UsuAut,
                       INPUT 0,
                       INPUT Producto.Debe,
                       INPUT Ahorros.Nit).

    Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

    movProductos.sdo_disponible = producto.debe.
    movProductos.tipo_transaccion = 1.
END.
ELSE DO:
    MESSAGE "No Hay Producto de Crédito Asociado a la Cuenta."
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Error en Taquilla.".

    RETURN ERROR.
END.

RELEASE Creditos.
RELEASE Taquilla.

W_DocContab = numDocAux.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sob_ConefecSYA C-Win 
PROCEDURE Sob_ConefecSYA :
DEFINE VAR numDocAux AS INTEGER.
    
ASSIGN T_Valor = 0
       T_Difer = 0
       T_Inter = 0.

RUN Val_Operacion(INPUT W_Agencia,
                  INPUT W_Grupo,
                  INPUT W_Usuario,
                  INPUT 010101003,
                  OUTPUT W_Error,
                  OUTPUT W_Nomoper).

IF W_Error THEN DO:
    MESSAGE "No está permitida por taquilla la operación..." SKIP
            W_Nomoper
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK.

    RETURN ERROR.
END.

T_Valor = Producto.Debe.

numDocAux = W_DocContab.

IF ahorros.agencia <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
    FIND FIRST comprobantes WHERE comprobantes.agencia = ahorros.agencia
                              AND comprobantes.comprobante = Cbte NO-ERROR.
    IF AVAILABLE comprobantes THEN DO:
        comprobantes.secuencia = comprobantes.secuencia + 1.
        numDocSyA = comprobantes.secuencia.
        W_DocContab = numDocSyA.
        flagCtaSucyAg = TRUE.
    END.
END.
ELSE DO:
    IF ahorros.agencia <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
        FIND FIRST comprobantes WHERE comprobantes.agencia = ahorros.agencia
                                  AND comprobantes.comprobante = Cbte NO-ERROR.
        IF AVAILABLE comprobantes THEN DO:
            numDocSyA = comprobantes.secuencia.
            W_DocContab = numDocSyA.
        END.
    END.
END.


FIND FIRST Creditos WHERE Creditos.Agencia EQ Ahorros.Agencia
                      AND creditos.cod_credito EQ Pro_Ahorros.ProCre_Asociado
                      AND Creditos.Pagare EQ Ahorros.Cue_Ahorros EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE(Creditos) THEN DO:
    RUN Cta_Sobregiro(INPUT 2,
                      INPUT creditos.cod_credito,
                      INPUT Creditos.Nit,
                      OUTPUT W_LiqIntDb,
                      OUTPUT W_LiqIntCr,
                      OUTPUT W_LiqMor,
                      OUTPUT W_DifCdb,
                      OUTPUT W_DifCcr).
    IF ERROR-STATUS:ERROR THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 66,
                                        OUTPUT W_Error).

        RETURN ERROR.
    END.

    IF Creditos.Int_MorCobrar GT 0 AND T_Valor NE 0 THEN DO:
        IF T_Valor GT Creditos.Int_MorCobrar THEN DO:
            RUN Gra_MovAhorros(INPUT 010102013,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0,
                               INPUT Creditos.Int_MorCobrar,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProducto.sdo_disponible = creditos.INT_morCobrar.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101006,        INPUT Producto.CodPto,          INPUT CtaSyA,       INPUT Cta_Caja,         INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,          INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT Creditos.Int_MorCobrar,   INPUT F_Seg).

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101006,        INPUT Producto.CodPto,          INPUT W_LiqMor,         INPUT CtaSyA,           INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,          INPUT Producto.OfiTem,  INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT Creditos.Int_MorCobrar,   INPUT F_Seg).

            ASSIGN T_Valor = T_Valor - Creditos.Int_MorCobrar
                   T_Inter = T_Inter + Creditos.Int_MorCobrar
                   Creditos.Int_MorCobrar = 0.
        END.
        ELSE DO:
            RUN Gra_MovAhorros(INPUT 010102013,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0, 
                               INPUT T_Valor,
                               input creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProducto.sdo_disponible = T_Valor.
            movProductos.tipo_transaccion = 2.
            
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101006,        INPUT Producto.CodPto,  INPUT CtaSyA,       INPUT Cta_Caja,         INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT T_Valor,          INPUT F_Seg).

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101006,        INPUT Producto.CodPto,  INPUT W_LiqMor,         INPUT CtaSyA,           INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,  INPUT Producto.OfiTem,  INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT T_Valor,          INPUT F_Seg).

            ASSIGN Creditos.Int_MorCobrar = Creditos.Int_MorCobrar - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
        END.
    END.
    
    IF Creditos.Int_difCobro GT 0 AND T_Valor NE 0 THEN DO:
        FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ 010101008 NO-LOCK NO-ERROR.
        IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN
            Cbte = Operacion.Comprobante.
        ELSE DO:
            RUN MostrarMensaje IN W_Manija (INPUT 143,
                                            OUTPUT W_Error).

            RETURN ERROR.
        END.

        RUN Cbte_Agencia IN W_Manija (INPUT Cbte,
                                      INPUT Producto.OfiTem,
                                      OUTPUT Docto,
                                      OUTPUT W_Error).

        IF W_Error THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 144,
                                            OUTPUT W_Eleccion).

            RETURN ERROR.
        END.

        IF T_Valor GT Creditos.Int_difCobro THEN DO:
            RUN Gra_MovAhorros(INPUT 010102014,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0, 
                               INPUT Creditos.Int_difCobro,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = creditos.INT_difCobro.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101008,        INPUT Producto.CodPto,          INPUT CtaSyA,       INPUT Cta_Caja,         INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,          INPUT W_Agencia,    INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT Creditos.Int_difCobro,    INPUT F_Seg).

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,       INPUT 010101008,        INPUT Producto.CodPto,          INPUT W_LiqIntCr,       INPUT CtaSyA,           INPUT "CR",
                             INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros,  INPUT Producto.DtoRef,  INPUT Producto.Cheque,          INPUT Producto.OfiTem,  INPUT Producto.OfiTem,  INPUT W_Agencia,
                             INPUT "1",             INPUT W_Usuario,            INPUT 0,                INPUT Creditos.Int_difCobro,    INPUT F_Seg).

            RUN Gra_Movimientos(INPUT Producto.OfiTem,          INPUT Cbte,     INPUT W_DifCdb, INPUT W_Fecha,  INPUT "CR", INPUT "Cerrar Cuenta Dificil Cobro",    INPUT W_Usuario,
                                INPUT Creditos.Int_difCobro,    INPUT 999,      INPUT Docto,    INPUT TODAY,    INPUT TIME) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                RETURN ERROR.

            RUN Gra_Movimientos(INPUT Producto.OfiTem,          INPUT Cbte, INPUT W_DifCcr, INPUT W_Fecha,  INPUT "DB", INPUT "Cerrar Cuenta Dificil Cobro",    INPUT W_Usuario,
                                INPUT Creditos.Int_difCobro,    INPUT 999,  INPUT Docto,    INPUT TODAY,    INPUT TIME) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                RETURN ERROR.
            
            ASSIGN T_Valor = T_Valor - Creditos.Int_difCobro
                   T_Inter = T_Inter + Creditos.Int_difCobro
                   Creditos.Int_difCobro = 0.
        END.
        ELSE DO:
            RUN Gra_MovAhorros(INPUT 010102014,
                               INPUT Producto.CodPto,
                               INPUT Producto.Cuenta,
                               INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,
                               INPUT W_Agencia,
                               INPUT Producto.OfiTem,
                               INPUT Producto.UsuAut,
                               INPUT 0, 
                               INPUT T_Valor,
                               INPUT Creditos.Nit).

            Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = T_Valor.
            movProductos.tipo_transaccion = 2.
            
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101008, 
                             INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT T_Valor, INPUT F_Seg).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101008, 
                             INPUT Producto.CodPto, INPUT W_LiqIntCr,      INPUT CtaSyA,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT T_Valor, INPUT F_Seg).
            RUN Gra_Movimientos(INPUT Producto.OfiTem, INPUT Cbte,INPUT W_DifCdb,  INPUT W_Fecha, 
                                INPUT "CR",    INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT T_Valor,    INPUT 999,       INPUT Docto,    
                                INPUT TODAY, INPUT TIME) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               RETURN ERROR.
            END.
            RUN Gra_Movimientos(INPUT Producto.OfiTem, INPUT Cbte,INPUT W_DifCcr,  INPUT W_Fecha, 
                                INPUT "DB",    INPUT "Cerrar Cuenta Dificil Cobro",INPUT W_Usuario,   
                                INPUT T_Valor,    INPUT 999,         INPUT Docto,    
                                INPUT TODAY, INPUT TIME) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               RETURN ERROR.
            END.
            ASSIGN Creditos.Int_difCobro = Creditos.Int_difCobro - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_morCobrar GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_morCobrar THEN DO:
            RUN Gra_MovAhorros(INPUT 010102015, INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,INPUT W_Agencia, INPUT Producto.OfiTem, INPUT Producto.UsuAut, INPUT 0, 
                               INPUT Creditos.Int_morCobrar, INPUT Creditos.Nit).
/*0203*/    Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = creditos.INT_morCobrar.
            movProductos.tipo_transaccion = 2.
            
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101007, 
                             INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT Creditos.Int_morCobrar, INPUT F_Seg).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101007, 
                             INPUT Producto.CodPto, INPUT W_LiqIntDb,      INPUT CtaSyA,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT Creditos.Int_morCobrar, INPUT F_Seg).
            ASSIGN T_Valor = T_Valor - Creditos.Int_morCobrar
                   T_Inter = T_Inter + Creditos.Int_morCobrar
                   Creditos.Int_morCobrar = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010102015,INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,INPUT W_Agencia, INPUT Producto.OfiTem, INPUT Producto.UsuAut, INPUT 0, 
                               INPUT T_Valor, INPUT Creditos.Nit).
/*0203*/    Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.

            movProductos.sdo_disponible = T_Valor.
            movProductos.tipo_transaccion = 2.
            
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101007, 
                             INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT T_Valor, INPUT F_Seg).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101007, 
                             INPUT Producto.CodPto, INPUT W_LiqIntDb,      INPUT CtaSyA,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT T_Valor, INPUT F_Seg).
            ASSIGN Creditos.Int_morCobrar = Creditos.Int_morCobrar - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Int_Corrientes GT 0 AND T_Valor NE 0 THEN DO:
         IF T_Valor GT Creditos.Int_Corrientes THEN DO:
            RUN Gra_MovAhorros(INPUT 010102016, INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,INPUT W_Agencia, INPUT Producto.OfiTem, INPUT Producto.UsuAut, INPUT 0, 
                               INPUT Creditos.Int_Corrientes, INPUT Creditos.Nit).
/*0203*/    Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.
            movProductos.sdo_disponible = creditos.INT_corrientes.
            movProductos.tipo_transaccion = 2.
            
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101005, 
                             INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT Creditos.Int_Corrientes, INPUT F_Seg).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101005, 
                             INPUT Producto.CodPto, INPUT W_LiqIntDb,      INPUT CtaSyA,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT Creditos.Int_Corrientes, INPUT F_Seg).
            ASSIGN T_Valor = T_Valor - Creditos.Int_Corrientes
                   T_Inter = T_Inter + Creditos.Int_Corrientes
                   Creditos.Int_Corrientes = 0.
         END.
         ELSE DO:
            RUN Gra_MovAhorros(INPUT 010102016, INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT Producto.DtoRef,
                               INPUT Producto.OfiTem,INPUT W_Agencia, INPUT Producto.OfiTem, INPUT Producto.UsuAut, INPUT 0, 
                               INPUT T_Valor, INPUT Creditos.Nit).
/*0203*/    Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.
            movProductos.sdo_disponible = T_Valor.
            movProductos.tipo_transaccion = 2.

            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101005, 
                             INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT T_Valor, INPUT F_Seg).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101005, 
                             INPUT Producto.CodPto, INPUT W_LiqIntDb,      INPUT CtaSyA,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT T_Valor, INPUT F_Seg).
            ASSIGN Creditos.Int_Corrientes = Creditos.Int_Corrientes - T_Valor
                   T_Inter = T_Inter + T_Valor
                   T_Valor = 0.
         END.
      END.
      IF Creditos.Sdo_Capital GT 0 AND T_Valor NE 0 THEN DO:
         RUN Sob_ConEfecSYA2.
      END.
      ASSIGN Ahorros.Sdo_Disponible     = Ahorros.Sdo_Disponible + (Producto.Debe - T_Inter)
             Ahorros.Fec_Ulttransaccion = TODAY.

      RUN reportarVisionamosAh(INPUT 1,
                               INPUT Producto.Debe - T_Inter).
      IF ahorros.tarjetaDB NE " " THEN      /* jjmp  24 de marzo de 2007 */
         RUN grabar_TmpTarDeb(Producto.Debe - T_Inter, "Consigna sobregiro en efectivo", 3).
      /* RUN grabar_TarDeb(Producto.Debe - T_Inter, "Consigna sobregiro en efectivo", 3). */

      RUN Gra_MovAhorros(INPUT 010101003, INPUT Producto.CodPto, INPUT Producto.Cuenta, INPUT Producto.DtoRef,
                         INPUT Producto.OfiTem,INPUT W_Agencia, INPUT Producto.OfiTem, INPUT Producto.UsuAut, INPUT 0, 
                         INPUT Producto.Debe, INPUT Creditos.nit).
/*0203*/  Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible.
          movProductos.sdo_disponible = producto.debe.
          movProductos.tipo_transaccion = 2.
   
   END.
   ELSE DO:
      MESSAGE "No Hay Producto de Crédito Asociado a la Cuenta."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
      TITLE "Error en Taquilla.".
      RETURN ERROR.
   END.
   RELEASE Creditos.
   RELEASE Taquilla.

W_DocContab = numDocAux.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sob_ConEfecSYA2 C-Win 
PROCEDURE Sob_ConEfecSYA2 :
/*-------------------------------------------------------------------------------
    Observaciones : Consigna Sobregiro en Efectivo.
    -------------------------------------------------------------------------------*/
         IF T_Valor GT Creditos.Sdo_Capital THEN DO:
            ASSIGN T_Difer = T_Valor - Creditos.Sdo_Capital.
            RUN Buscar_Cuenta(INPUT 2, INPUT creditos.cod_credito, INPUT Creditos.Plazo, INPUT Ahorros.Nit, OUTPUT CtaCble) NO-ERROR.
            IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
               RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
               RETURN ERROR.
            END.
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101009, 
                             INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT Creditos.Sdo_Capital, INPUT F_Seg).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101009, 
                             INPUT Producto.CodPto, INPUT CtaCble,         INPUT CtaSyA,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT Creditos.Sdo_Capital, INPUT F_Seg).
            RUN Buscar_Cuenta(INPUT 1, INPUT Producto.CodPto, INPUT Ahorros.Plazo, INPUT Ahorros.Nit, OUTPUT CtaCble) NO-ERROR.
            IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
               RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
               RETURN ERROR.
            END.
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101003, 
                             INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT T_Difer, INPUT F_Seg).
            RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101003, 
                             INPUT Producto.CodPto, INPUT CtaCble,         INPUT CtaSyA,
                             INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                             INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                             INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                             INPUT W_Usuario,       INPUT 0,               INPUT T_Difer, INPUT F_Seg).

            IF creditos.cod_credito = 123 THEN
                RUN reportarVisionamosCr(INPUT 1,
                                         INPUT creditos.sdo_capital).

            ASSIGN Creditos.Sdo_Capital = 0.
         END.
         ELSE DO:
           RUN Buscar_Cuenta(INPUT 2, INPUT creditos.cod_credito, INPUT Creditos.Plazo, INPUT Ahorros.Nit, OUTPUT CtaCble) NO-ERROR.
           IF ERROR-STATUS:ERROR OR CtaCble EQ "" THEN DO:
              RUN MostrarMensaje IN W_Manija (INPUT 66,OUTPUT W_Error).
              RETURN ERROR.
           END.
           RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101009, 
                            INPUT Producto.CodPto, INPUT CtaSyA,          INPUT Cta_Caja,
                            INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                            INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT W_Agencia,           
                            INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                            INPUT W_Usuario,       INPUT 0,               INPUT T_Valor, INPUT F_Seg).
           RUN Gra_Taquilla(INPUT Producto.UsuAut, INPUT Producto.Banco,  INPUT 010101009, 
                            INPUT Producto.CodPto, INPUT CtaCble,         INPUT CtaSyA,
                            INPUT "CR",            INPUT Ahorros.Nit,     INPUT Ahorros.Cue_Ahorros, 
                            INPUT Producto.DtoRef, INPUT Producto.Cheque, INPUT Producto.OfiTem,           
                            INPUT Producto.OfiTem, INPUT W_Agencia,       INPUT "1",                 
                            INPUT W_Usuario,       INPUT 0,               INPUT T_Valor, INPUT F_Seg).
           ASSIGN Creditos.Sdo_Capital = Creditos.Sdo_Capital - T_Valor.

           IF creditos.cod_credito = 123 THEN
               RUN reportarVisionamosCr(INPUT 1,
                                        INPUT T_Valor).
         END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidaAutoriz C-Win 
PROCEDURE ValidaAutoriz :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF W_CedTrans NE F_Nit THEN DO:
     FIND FIRST Relaciones WHERE Relaciones.Nit            EQ F_Nit
                             AND Relaciones.Nit_relacion   EQ W_CedTrans
                             AND Relaciones.Cod_relacion   EQ 7
                             AND Relaciones.Cuenta         EQ Producto.Cuenta
                             AND Relaciones.Clase_Producto EQ 1
                             AND Relaciones.Cod_Producto   EQ Producto.CodPto
                             AND Relaciones.Estado         EQ 1
                             AND Relaciones.Val_Autorizado GE Producto.Haber NO-LOCK NO-ERROR.
     IF NOT AVAIL(Relaciones) THEN DO:
        MESSAGE "El AUTORIZADO NO cumple para Retirar ese MONTO"
           VIEW-AS ALERT-BOX ERROR.
        ASSIGN Producto.Haber = 0
               Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0"
               W_CedTrans = " "
               W_CedTrans:SCREEN-VALUE IN FRAME F_Trans = " "
               W_NomTx:SCREEN-VALUE = " "
               W_NomTx.
        RETURN ERROR.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Cheques C-Win 
PROCEDURE Validar_Cheques :
/*------------------------------------------------------------------------------
  Observaciones : Permite Validar Los Cheques Consignados a una Cuenta.       
------------------------------------------------------------------------------*/
   ASSIGN BROWSE BROWSE-7 Cheques.W_Canje 
                          Cheques.W_Banco 
                          Cheques.W_Cheque 
                          Cheques.W_Valor.
   ASSIGN ValCheq = ValCheq + Cheques.W_Valor - ValAnt.
      
   IF ValCheq EQ Producto.Debe THEN DO:
      HIDE FRAME Frame_Cheques.
      RUN Activar.
      APPLY "ENTRY" TO Producto.Retiro IN BROWSE BROWSE-5.
      RETURN NO-APPLY.
   END.
   ELSE
   IF ValCheq GT Producto.Debe THEN DO:
      ASSIGN ValCheq = ValCheq - Cheques.W_Valor.
             Cheques.W_Valor:SCREEN-VALUE IN BROWSE BROWSE-7 = "0".
      MESSAGE "La Sumatoria de Valores No Coincide con el Valor a Consignar."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
      TITLE "Error en Cheques en Canje".
      RETURN NO-APPLY.
   END.
   ELSE
   IF ValCheq LT Producto.Debe AND Cheques.W_Valor NE 0 THEN DO:
      IF NUM-RESULTS("BROWSE-7") EQ Cheques.W_Secue THEN DO:
         RUN Insert_Cheque.
         ASSIGN Cheques.W_Valor:SCREEN-VALUE IN BROWSE BROWSE-7 = "0".
         APPLY "ENTRY" TO Cheques.W_Canje IN BROWSE BROWSE-7.
         RETURN NO-APPLY.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Deducible C-Win 
PROCEDURE Validar_Deducible :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permita Validar Si el Saldo del Producto Es Suficiente Para
                  el Retiro Más el Impuesto.       
--------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER E_Ded      LIKE Operacion.Cod_Deducible.
  DEFINE INPUT  PARAMETER E_Valor    LIKE Ahorros.Sdo_Disponible.
  DEFINE OUTPUT PARAMETER Resultado  AS   DECIMAL DECIMALS 2 FORMAT ">>>,>>>,>>>,>>9.99".
  DEFINE OUTPUT PARAMETER Clase      AS   INTEGER FORMAT 9.
  DEFINE OUTPUT PARAMETER Nombre     LIKE Deducible.Nom_Deducible.
  DEFINE OUTPUT PARAMETER Cuenta     LIKE Deducible.Cuenta.
  DEFINE OUTPUT PARAMETER Cta_Imp    LIKE Deducible.Cuenta_Impuesto.
  DEFINE OUTPUT PARAMETER Val_Imp    LIKE Deducible.Valor_Impuesto.
  
  DEFINE VAR Valor     LIKE Deducible.Valor.
  FIND Deducible WHERE Deducible.Cod_Deducible EQ E_Ded NO-LOCK NO-ERROR.
  IF AVAILABLE (Deducible) THEN DO:
     ASSIGN Valor  = Deducible.Valor
            Cuenta = Deducible.Cuenta
            Cta_Imp = Deducible.Cuenta_Impuesto
            Val_Imp = Deducible.Valor_Impuesto.
     IF Deducible.Cod_Deducible NE "2" THEN
        Nombre = Deducible.Nom_Deducible.
     ELSE     
        Nombre = Operacion.Nom_Operacion.
  END.
  ELSE RETURN ERROR.

  IF Valor GT 0 THEN DO:
     IF Deducible.Cla_Deducible EQ 1 THEN
        ASSIGN Resultado = E_Valor - (E_Valor / (Valor + 1)) /*antes: E_Valor * Valor*/
               Clase     = 1.
     ELSE ASSIGN Resultado = Valor
               Clase     = 2.
  END.
  ELSE DO:
    ASSIGN Resultado = 0.
    RETURN.
  END.
  
  IF SUBSTRING(Producto.TipPto,1,1) EQ "3" THEN /*si es un producto de ahorro a termino*/
     Resultado = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Operacion C-Win 
PROCEDURE Validar_Operacion :
DEFINE INPUT    PARAMETER T_OfiVal  LIKE Agencias.Agencia.
  DEFINE INPUT  PARAMETER T_GrpVal  LIKE Grupos.Grupo.
  DEFINE INPUT  PARAMETER T_UsuVal  LIKE Usuarios.Usuario.
  DEFINE INPUT  PARAMETER T_OpeVal  LIKE Operacion.Cod_Operacion.
  DEFINE INPUT  PARAMETER T_Clave   LIKE Operacion.id_Clave.
  DEFINE INPUT  PARAMETER T_NomOpe  LIKE Operacion.Nom_Operacion.

  DEFINE VAR T_Validar AS LOGICAL INITIAL NO.

  IF T_Clave THEN DO:
     MESSAGE "La Operación "  T_NomOpe   SKIP
             "Requiere Clave de SuperUsuario."
     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
     TITLE "Validación En Taquilla".
     RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
     IF W_Error EQ FALSE THEN DO:
        ASSIGN T_Validar = TRUE.
     END.
  END.
  
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 3
                           AND   Res_Operacion.Usuario       EQ T_UsuVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 2
                           AND   Res_Operacion.Grupo         EQ T_GrpVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 1
                           AND   Res_Operacion.Agencia       EQ T_OfiVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.

  IF T_Validar THEN DO:
   MESSAGE "Hay restricción para esta Operación...Cancelada" SKIP
            T_Nomope 
   VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
   RETURN ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Reg C-Win 
PROCEDURE Validar_Reg :
/*------------------------------------------------------------------------------
  OBSERVACIONES: Procedimiento de Validacion del BROWSER.       
------------------------------------------------------------------------------*/
      DEFI   VAR EfCh       AS INTEG FORM "9" INIT 0.  /*Inicia en efectivo*/ 

      DEFINE VAR T_DtoRef AS CHARACTER.
      
      P_ImpAplic = 0.
      
      ASSIGN  BROWSE BROWSE-5  Producto.OfiTem Producto.NomPto  Producto.Cuenta
                               Producto.DtoRef Producto.Debe    Producto.Haber
                               Producto.EC     Producto.Retiro.
      /* Validar si el cheque a pagar ha sido contraordenado */
         IF F_Tipo EQ 3 THEN DO:
            ASSIGN T_DtoRef = TRIM(SUBSTRING(Producto.DtoRef,2,11)).
            RUN Val_Cheque(INPUT Producto.OfiTem,INPUT W_CodPto,INPUT F_Cuenta,INPUT T_DtoRef) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               MESSAGE "El Cheque Esta Contraordenado. Verifique...?"
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
               RETURN ERROR.
            END.
         END.
         
      /* Validar Los Productos en su Estado*/
       IF Producto.TipPto NE "" AND (Producto.Debe NE 0 OR Producto.Haber NE 0) THEN DO:
            RUN Estado_Pto NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               RETURN "ERROR".
            END.
       END.
    
      /* Validar Los Retiros En Efectivo o Cheque. */
      IF Producto.Haber NE 0 AND Producto.TipPto NE "" THEN DO:
         IF Producto.EstCta GT 2 AND
            Producto.EstCta NE 10 THEN DO:
            FIND Varios WHERE Varios.Tipo EQ 21 AND Varios.Codigo EQ Producto.EstCta NO-LOCK NO-ERROR.
            IF AVAILABLE Varios THEN DO:
               MESSAGE "La cuenta se encuentra bloqueada." SKIP
                       "Tipo de Bloqueo: " Varios.Descripcion VIEW-AS ALERT-BOX ERROR.
               RETURN ERROR.
            END.
            ELSE DO:
               MESSAGE "No se encuentra el estado de la cuenta" SKIP
                       "tal ves no se encuentra matriculado" SKIP
                       "el estado actual es: " Producto.EstCta VIEW-AS ALERT-BOX ERROR.
               RETURN ERROR.
            END.
         END.

         IF SUBSTRING(Producto.TipPto,1,1) EQ "1" THEN DO:
            RUN Armar_Operacion (OUTPUT T_Ope, OUTPUT T_Ded) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN ERROR.
            IF T_Ded NE "" THEN DO:
               RUN Validar_Deducible (INPUT T_Ded, INPUT Producto.Haber, 
                   OUTPUT Vr_Deducible, OUTPUT T_Cld, OUTPUT T_NDd, OUTPUT T_CtD, OUTPUT T_CIm, OUTPUT T_VIm).
               IF ERROR-STATUS:ERROR THEN RETURN ERROR.
            END.
            
            IF Producto.Haber GT 0 AND T_Ded GT " " THEN DO:
               IF (SUBSTRING(Producto.TipPto,3,1) EQ "1" OR SUBSTRING(Producto.TipPto,3,1) EQ "4") AND 
/*                     EfCh EQ 0 AND /*GCamacho - 03/14/2011 - Operaciones en cheque deben generar GMF*/ */
                    TRUE THEN DO:                                                                                      
                /*Con false retorna el valor a cargo del cliente y no actualiza*/                     
                  RUN RutGMF.P (INPUT FALSE,W_Agencia,Ahorros.Agencia,1,Ahorros.Cod_Ahorro,Ahorros.Nit, 
                                INPUT Ahorros.Cue_Ahorro,T_Ope,Producto.Haber,Operacion.Comprobante,          
                                INPUT STRING(Producto.DtoRef),"Por Taquilla",0,EfCh,OUTPUT P_ImpAplic) NO-ERROR.                                              
                /*IF ERROR-STATUS:ERROR THEN DO:                                                        
                     MESSAGE "El Prog: RutGMF.P...Retorno ERROR(Informativo sin Salvar) no se permite la operaciòn..."          
                              VIEW-AS ALERT-BOX ERROR.                                                        
                     RETURN ERROR.                                                                      
                  END.*/                                                                                  
               END.
            END.
          
            IF SUBSTRING(Producto.TipPto,3,1) EQ "1" THEN DO:
               RUN Val_RetVista NO-ERROR.
               IF ERROR-STATUS:ERROR THEN RETURN ERROR.         
            END.
            
            IF SUBSTRING(Producto.TipPto,3,1) EQ "2" THEN DO:
               RUN Val_RetContractual NO-ERROR.
               IF ERROR-STATUS:ERROR THEN RETURN ERROR.
            END.
            
            IF SUBSTRING(Producto.TipPto,3,1) EQ "3" THEN DO:
               RUN Val_RetTermino NO-ERROR.
               IF ERROR-STATUS:ERROR THEN RETURN ERROR.
            END.
            
            IF SUBSTRING(Producto.TipPto,3,1) EQ "4" THEN DO:
               RUN Val_RetAportes NO-ERROR.
               IF ERROR-STATUS:ERROR THEN RETURN ERROR.
            END.
         END.
      END.
   /* Fin Validar */
   ASSIGN Producto.UsuAut = W_Autorizo
          W_Autorizo      = W_Usuario.
   ASSIGN  BROWSE BROWSE-5  Producto.OfiTem Producto.NomPto  Producto.Cuenta
                            Producto.DtoRef Producto.Debe    Producto.Haber
                            Producto.EC     Producto.Retiro.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_Cheque C-Win 
PROCEDURE Val_Cheque :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Permite Validar Que el Cheque no sea Contraordenado.       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER B_Agencia LIKE Che_Blo.Agencia.
  DEFINE INPUT  PARAMETER B_CodPto  LIKE Che_Blo.Cod_Producto.
  DEFINE INPUT  PARAMETER B_CueAho  LIKE Che_Blo.Cue_Ahorros.
  DEFINE INPUT  PARAMETER B_NumChe  LIKE Che_Blo.Numero_Cheque.
  
  FIND FIRST Che_Blo WHERE Che_Blo.Agencia       EQ B_Agencia
                     AND   Che_Blo.Cod_Producto  EQ B_CodPto
                     AND   Che_Blo.Cue_Ahorros   EQ B_CueAho
                     AND   Che_Blo.Numero_Cheque EQ B_NumChe NO-LOCK NO-ERROR.
  IF AVAILABLE(Che_Blo) THEN
     RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_ConAportes C-Win 
PROCEDURE Val_ConAportes :
/*------------------------------------------------------------------------------
  Observaciones : Validar la Consignación en Aportes.       
------------------------------------------------------------------------------*/
    
   FOR FIRST Pro_Ahorros WHERE pro_ahorros.tip_ahorro EQ INTEGER(SUBSTRING(Producto.TipPto,3,1))
                         AND   pro_ahorros.cod_ahorro EQ Producto.CodPto 
                         AND   Pro_Ahorros.Estado     EQ 1 NO-LOCK,
       FIRST Ahorros     WHERE Ahorros.Nit            EQ Producto.Nit
                         AND   Ahorros.Cod_Ahorro     EQ Pro_Ahorros.cod_ahorro
                         AND   Ahorros.Cue_Ahorros    EQ Producto.Cuenta NO-LOCK:
        
       /*GCamacho - May04/08 - implementacion Ahorro permanente - Validacion aporte obligatorio*/
       /*
       IF pro_ahorros.cod_ahorro EQ 5 AND Ahorros.Estado EQ 1 THEN DO:
               FIND FIRST indicadores WHERE indicadores.indicador EQ 21 NO-LOCK NO-ERROR.
               IF indicadores.valor * 20 < (Ahorros.Sdo_Disponible + producto.debe) THEN DO:
                   MESSAGE "Abonos + Aportes superan los 20 SMMLV." SKIP
                       "Aportes = $" Ahorros.Sdo_Disponible SKIP
                       "Por favor revisar"
                       VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Aportes Mayores a 20SMMLV".
                   ASSIGN producto.debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
                   APPLY "ENTRY":U TO producto.debe IN BROWSE BROWSE-5.
                   RETURN NO-APPLY.
               END.
       END.
       */
       /********************************************************************/

       IF Ahorros.Estado EQ 1 AND 
          Ahorros.Detalle_Estado EQ 1 THEN DO:
          IF Pro_Ahorros.Id_MonApertura THEN DO:
             IF Ahorros.Monto_Apertura GT Producto.Debe THEN DO:
                MESSAGE "El Monto de Apertura es Inferior al Valor "SKIP
                        "Registrado Para la Cuenta o el Producto."  SKIP
                        "Desea Autorizar la Transacción ...?"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                TITLE "Error En Taquilla" UPDATE Sw_Estado.
                IF Sw_Estado EQ TRUE THEN DO:
                   RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                   IF W_Error EQ FALSE THEN
                      RETURN ERROR.
                END.
                ELSE
                  RETURN ERROR.
             END.       
             IF Pro_Ahorros.Val_MaxConsignacion GT 0 AND
                Pro_Ahorros.Val_MaxConsignacion LT Producto.Debe THEN DO:
                MESSAGE "La Consignación Excede el Valor "    SKIP
                        "Maximo Permitido Para el Producto."  SKIP
                        "Desea Autorizar la Transacción ...?"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                TITLE "Error En Taquilla" UPDATE Sw_Estado.
                IF Sw_Estado EQ TRUE THEN DO:
                   RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                   IF W_Error EQ FALSE THEN
                      RETURN ERROR.
                END.
                ELSE
                  RETURN ERROR.
             END.
          END.
          ELSE DO:
             IF Pro_Ahorros.Val_MaxConsignacion GT 0 AND
                Pro_Ahorros.Val_MaxConsignacion LT Producto.Debe THEN DO:
                MESSAGE "La Consignación Excede el Valor "    SKIP
                        "Maximo Permitido Para el Producto."  SKIP
                        "Desea Autorizar la Transacción ...?"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                TITLE "Error En Taquilla" UPDATE Sw_Estado.
                IF Sw_Estado EQ TRUE THEN DO:
                   RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                   IF W_Error EQ FALSE THEN
                      RETURN ERROR.
                END.
                ELSE
                  RETURN ERROR.
             END.
          END.
       END.
       ELSE DO:
          IF Pro_Ahorros.Val_Minconsignacion GT 0 AND
             Pro_Ahorros.Val_Minconsignacion GT Producto.Debe THEN DO:
             MESSAGE "El Valor a Consignar No Supera el" SKIP
                     "Monto Mínimo de Consignación."     SKIP
                     "Desea Autorizar la Transacción ...?"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
             TITLE "Error En Taquilla" UPDATE Sw_Estado.
             IF Sw_Estado EQ TRUE THEN DO:
                RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                IF W_Error EQ FALSE THEN
                  RETURN ERROR.
             END.
             ELSE
               RETURN ERROR.
          END.
          ELSE DO:
             IF Pro_Ahorros.Val_MaxConsignacion GT 0 AND
                Pro_Ahorros.Val_MaxConsignacion LT Producto.Debe THEN DO:
                MESSAGE "La Consignación Excede el Valor "    SKIP
                        "Maximo Permitido Para el Producto."  SKIP
                        "Desea Autorizar la Transacción ...?"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                TITLE "Error En Taquilla" UPDATE Sw_Estado.
                IF Sw_Estado EQ TRUE THEN DO:
                   RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                   IF W_Error EQ FALSE THEN RETURN ERROR.
                END.
                ELSE RETURN ERROR.
             END.
          END.
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_ConContractual C-Win 
PROCEDURE Val_ConContractual :
/*------------------------------------------------------------------------------
  Observaciones :       
------------------------------------------------------------------------------*/
   DEFINE VAR W_Mod  AS INTEGER.
   DEFI   VAR W_ValC AS DECIMAL.
   
   FOR FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Producto.TipPto,3,1))
                         AND   Pro_Ahorros.Cod_Ahorro EQ Producto.CodPto 
                         AND   Pro_Ahorros.Estado     EQ 1 NO-LOCK,
       FIRST Ahorros     WHERE Ahorros.Nit            EQ Producto.Nit
                         AND   Ahorros.Cod_Ahorro     EQ Pro_Ahorros.Cod_Ahorro
                         AND   Ahorros.Cue_Ahorros    EQ Producto.Cuenta NO-LOCK:
       IF Producto.Debe GT 0 THEN DO: 
          W_ValC = Producto.Debe / Ahorros.Cuota. 
/*          IF W_ValC NE ROUND(W_ValC,0)  THEN DO:
             MESSAGE "La consignación debe ser Múltiplo de la cuota."
                 VIEW-AS ALERT-BOX ERROR.
             RETURN ERROR. 
          END.*/
       END.
       
        /* Validamos para Ahorros permamnetes, que Asociado ya hubiese hecho su aporte mensual */
        IF producto.CodPto EQ 221 THEN DO:
            FIND FIRST Bahorros WHERE BAhorros.tip_ahorro EQ 4 AND 
                    BAhorros.cod_ahorro EQ 5 AND 
                    BAhorros.nit EQ Producto.Nit NO-LOCK NO-ERROR.
            IF AVAILABLE BAhorros THEN DO:
                    IF MONTH(BAhorros.Fec_Ulttransaccion) NE MONTH(W_Fecha) THEN DO:
                            MESSAGE "El Asociado no ha hecho el aporte este mes" SKIP
                                    "Por favor revisar"
                                VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Asociado sin aporte en el mes".
                    END.
            END.
        END.

       IF Ahorros.Estado EQ 1 AND 
          Ahorros.Detalle_Estado EQ 1      THEN DO:
          IF Ahorros.Monto_Apertura LE Producto.Debe THEN DO:
             IF Pro_Ahorros.Val_MaxConsignacion GT 0 AND
                Pro_Ahorros.Val_MaxConsignacion LT Producto.Debe THEN DO:
                MESSAGE "La Consignación Excede el Valor "    SKIP
                        "Maximo Permitido Para el Producto."  SKIP
                        "Desea Autorizar la Transacción ...?" SKIP(1)
                        "El valor maximo de Apertura es: $" STRING(Pro_Ahorros.Val_MaxConsignacion,">>>,>>>,>>9")
                VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                TITLE "Error En Taquilla" UPDATE Sw_Estado.
                IF Sw_Estado EQ TRUE THEN DO:
                   RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                   IF W_Error EQ FALSE THEN
                      RETURN ERROR.
                END.
                ELSE
                   RETURN ERROR.
             END. 
          END.
          ELSE DO:
             MESSAGE "El Monto de Apertura es Inferior al Valor "SKIP
                     "de apertura pactado para la Cuenta."  SKIP
                     "el cual fue: $ " STRING(Ahorros.Monto,">>,>>>,>>9") SKIP(1)
                     "Desea Autorizar la Transacción ...?"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
             TITLE "Error En Taquilla" UPDATE Sw_Estado.
             IF Sw_Estado EQ TRUE THEN DO:
                RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                IF W_Error EQ FALSE THEN
                   RETURN ERROR.
             END.
             ELSE
                RETURN ERROR.
          END.
       END.
       ELSE DO:
          /*IF Pro_Ahorros.Id_Cuota AND Ahorros.Cuota NE Producto.Debe THEN DO:
             MESSAGE "La Cuota minima permitida es: $ " Ahorros.Cuota SKIP
                     "O El valor de Consignacion es Mayor que la Cuota pactada...?"  SKIP(1)
                     "                         Desea Autorizar la Transacción ...?"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
             TITLE "Error En Taquilla" UPDATE Sw_Estado.
             IF Sw_Estado EQ TRUE THEN DO:
                RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                IF W_Error EQ FALSE THEN RETURN ERROR.
             END.
             ELSE RETURN ERROR.
          END.*/
          IF Pro_Ahorros.Tip_Anualidad EQ TRUE THEN DO: /* Cuota Fija */
             ASSIGN W_Mod = Producto.Debe MODULO Ahorros.Cuota. 
             IF Pro_Ahorros.Id_Cuota EQ TRUE AND W_Mod EQ 0 THEN DO:
                IF Pro_Ahorros.Val_MaxConsignacion GT 0 AND
                   Pro_Ahorros.Val_MaxConsignacion LT Producto.Debe THEN DO:
                   MESSAGE "La Consignación Excede el Valor "    SKIP
                           "Maximo Permitido Para el Producto."  SKIP
                           "El cual es: $ " STRING(Pro_Ahorros.Val_MaxConsignacion,">>,>>>,>>9") SKIP(1)
                           "Desea Autorizar la Transacción ...?"
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                   TITLE "Error En Taquilla" UPDATE Sw_Estado.
                   IF Sw_Estado EQ TRUE THEN DO:
                      RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                      IF W_Error EQ FALSE THEN RETURN ERROR.
                   END.
                   ELSE RETURN ERROR.
                END.
             END.
             ELSE DO:
                /*MESSAGE "El Producto Maneja Cuota Fija Y el Valor a " SKIP
                        "Consignar No Corresponde Con la(s) Cuota(s)" SKIP
                        "Para el Producto."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                TITLE "Error En Taquilla".
                RETURN ERROR.   Abril 26/05 GAER*/
             END.
          END.
          ELSE DO:                                      /* Cuota Variable */
             IF Pro_Ahorros.Val_Minconsignacion GT 0 AND
                Pro_Ahorros.Val_Minconsignacion GT Producto.Debe THEN DO:
                MESSAGE "El Valor a Consignar No Supera el" SKIP
                        "Monto Mínimo de Consignación."     SKIP
                        "El cual es de: $ " STRING(Pro_Ahorros.Val_Minconsignacion,">>,>>>,>>9") SKIP(1)
                        "Desea Autorizar la Transacción ...?"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                TITLE "Error En Taquilla" UPDATE Sw_Estado.
                IF Sw_Estado EQ TRUE THEN DO:
                   RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                   IF W_Error EQ FALSE THEN DO:
                      RETURN ERROR.
                   END.
                END.
                ELSE DO:
                   RETURN ERROR.
                END.
             END.
             ELSE DO:
                IF Pro_Ahorros.Val_MaxConsignacion GT 0 AND
                   Pro_Ahorros.Val_Maxconsignacion LT Producto.Debe THEN DO:
                   MESSAGE "El Valor a Consignar es Superior al Valor Maximo" SKIP
                           "Permitido Para la Consignación en Este Producto." SKIP
                           "El cual es: $ " STRING(Pro_Ahorros.Val_Maxconsignacion,">>,>>>,>>9") SKIP(1)
                           "Desea Autorizar la Transacción ...?"
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                   TITLE "Error En Taquilla" UPDATE Sw_Estado.
                   IF Sw_Estado EQ TRUE THEN DO:
                      RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                      IF W_Error EQ FALSE THEN DO:
                         RETURN ERROR.
                      END.
                   END.
                   ELSE DO:
                     RETURN ERROR.
                   END.  
                END.
             END.
          END.
       END.
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_ConTermino C-Win 
PROCEDURE Val_ConTermino :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Validar la Apertura de los A Termino.       
------------------------------------------------------------------------------*/
    /*15/01/2005 se agrego al find de ahorros el nit de la persona*/
    FOR FIRST Pro_Ahorros WHERE pro_ahorros.tip_ahorro EQ INTEGER(SUBSTRING(Producto.TipPto,3,1))
                         AND   pro_ahorros.cod_ahorro EQ Producto.CodPto 
                         AND   Pro_Ahorros.Estado       EQ 1 NO-LOCK,
       FIRST Ahorros     WHERE Ahorros.Nit              EQ Producto.Nit
                         AND   ahorros.cod_ahorro       EQ pro_ahorros.cod_ahorro
                         AND   Ahorros.Cue_Ahorros      EQ Producto.Cuenta NO-LOCK:
       IF (Ahorros.Sdo_Canje + Ahorros.Sdo_Disponible + Producto.Debe) NE Ahorros.Monto_Apertura THEN DO:
           MESSAGE "El Valor Debe Ser Menor o Igual al Monto de" SKIP
            "Apertura Registrado Para la Cuenta ." SKIP
            "EL monto pactado fue: $ " STRING(Ahorros.Monto_Apertura,">>,>>>,>>>,>>9") SKIP
            "Desea Autorizar la Transacción ...?"
           VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
           TITLE "Error En Taquilla" UPDATE Sw_Estado.
           IF Sw_Estado EQ TRUE THEN DO:
              RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
              IF W_Error EQ FALSE THEN
                 RETURN ERROR.
           END.
           ELSE
             RETURN ERROR.
       END.
       IF (Ahorros.Sdo_Canje + Ahorros.Sdo_Disponible + Producto.Debe) > Ahorros.Monto_Apertura THEN DO:
           MESSAGE "El Valor a Consignar Supera el Monto de Apertura"  SKIP
                    "Establecido Para el Producto. el cual fue: $ " STRING(Ahorros.Monto_Apertura,">>,>>>,>>>,>>9")
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
           TITLE "Error En Taquilla".
           RETURN ERROR.
       END.
       IF Pro_Ahorros.Id_Montomaximo      EQ TRUE          AND
                Pro_Ahorros.Val_Maxconsignacion LT Producto.Debe THEN DO:
                MESSAGE "El Valor a Consignar es Superior al Valor Maximo" SKIP
                        "Permitido Para la Consignación en Este Producto." SKIP
                        "Desea Autorizar la Transacción ...?"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                TITLE "Error En Taquilla" UPDATE Sw_Estado.
                IF Sw_Estado EQ TRUE THEN DO:
                   RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                   IF W_Error EQ FALSE THEN DO:
                     RETURN ERROR.
                   END.
                END.
                ELSE DO:
                  RETURN ERROR.
                END.  
       END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_ConVista C-Win 
PROCEDURE Val_ConVista :
/*-----------------------------------------------------------------------------------
  OBSERVACIONES : Validar Consignaciones a la Vista.       
------------------------------------------------------------------------------------*/
   
   FOR FIRST Pro_Ahorros WHERE pro_ahorros.tip_ahorro EQ INTEGER(SUBSTRING(Producto.TipPto,3,1))
                         AND   pro_ahorros.cod_ahorro EQ Producto.CodPto 
                         AND   Pro_Ahorros.Estado       EQ 1 NO-LOCK,
       FIRST Ahorros     WHERE Ahorros.Nit            EQ Producto.Nit
                         AND   ahorros.cod_ahorro     EQ pro_ahorros.cod_ahorro
                         AND   Ahorros.Cue_Ahorros    EQ Producto.Cuenta NO-LOCK:
       IF Ahorros.Estado EQ 1 AND 
          Ahorros.Detalle_Estado EQ 1      THEN DO:
          IF Pro_Ahorros.Id_MonApertura EQ TRUE THEN DO: 
             IF Ahorros.Monto_Apertura  LE Producto.Debe THEN DO:
                IF Pro_Ahorros.Id_Montomaximo      EQ TRUE AND
                   Pro_Ahorros.Val_MaxConsignacion LT Producto.Debe THEN DO:
                   MESSAGE "La Consignación Excede el Valor "    SKIP
                           "Maximo Permitido Para el Producto."  SKIP
                           "Desea Autorizar la Transacción ...?"
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                   TITLE "Error En Taquilla" UPDATE Sw_Estado.
                   IF Sw_Estado EQ TRUE THEN DO:
                      RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                      IF W_Error EQ FALSE THEN DO:
                        RETURN ERROR.
                      END.
                   END.
                   ELSE DO:
                     RETURN ERROR.
                   END.
                END.
             END.
             ELSE DO:
                MESSAGE "El Monto de Apertura es Inferior al Valor "SKIP
                        "Registrado Para la Cuenta o el Producto."  SKIP
                        "Desea Autorizar la Transacción ...?"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                TITLE "Error En Taquilla" UPDATE Sw_Estado.
                IF Sw_Estado EQ TRUE THEN DO:
                   RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                   IF W_Error EQ FALSE THEN DO:
                     RETURN ERROR.
                   END.
                END.
                ELSE DO:
                  RETURN ERROR.
                END.
             END.
          END.
       END.
       ELSE DO:
          IF Pro_Ahorros.Id_Montominimo      EQ TRUE          AND
             Pro_Ahorros.Val_Minconsignacion GT Producto.Debe THEN DO:
             MESSAGE "El Valor a Consignar no Supera el" SKIP
                     "Monto Mínimo de Consignación."     SKIP
                     "Desea Autorizar la Transacción ...?"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
             TITLE "Error En Taquilla" UPDATE Sw_Estado.
             IF Sw_Estado EQ TRUE THEN DO:
                RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                IF W_Error EQ FALSE THEN DO:
                  RETURN ERROR.
                END.
             END.
             ELSE DO:
               RETURN ERROR.
             END.
          END.
          ELSE DO:
             IF Pro_Ahorros.Id_Montomaximo      EQ TRUE          AND
                Pro_Ahorros.Val_Maxconsignacion LT Producto.Debe THEN DO:
                MESSAGE "El Valor a Consignar es Superior al Valor Maximo" SKIP
                        "Permitido Para la Consignación en Este Producto." SKIP
                        "Desea Autorizar la Transacción ...?"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                TITLE "Error En Taquilla" UPDATE Sw_Estado.
                IF Sw_Estado EQ TRUE THEN DO:
                   RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                   IF W_Error EQ FALSE THEN DO:
                     RETURN ERROR.
                   END.
                END.
                ELSE DO:
                  RETURN ERROR.
                END.  
             END.
          END.
       END.
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_CpteEgreso C-Win 
PROCEDURE Val_CpteEgreso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_Especiales C-Win 
PROCEDURE Val_Especiales :
/*------------------------------------------------------------------------------
  Observaciones : Validar los ingresos a los productos especiales.       
------------------------------------------------------------------------------*/
  DEFINE VAR T_Cuota LIKE Especiales.Cuota.
  DEFINE VAR T_Rdo   AS   DECIMAL DECIMALS 2 FORMAT "->>>,>>>.99".
  
  FOR FIRST Pro_Especiales WHERE Pro_Especiales.Agencia      EQ Producto.OfiTem
                           AND   Pro_Especiales.Cod_Producto EQ Producto.CodPto
                           AND   Pro_Especiales.Estado       EQ 1 NO-LOCK,
      FIRST Especiales     WHERE Especiales.Agencia      EQ Pro_Especiales.Agencia
                           AND   Especiales.Cod_Producto EQ Pro_Especiales.Cod_Producto
                           AND   Especiales.Nit          EQ F_Nit NO-LOCK:
      IF Pro_Especiales.Id_CtrSaldo EQ TRUE THEN DO:
         IF Especiales.Sdo_Pendiente EQ 0 THEN DO:
            MESSAGE "El Producto No Tiene Saldo Pendiente."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Error en Especiales".
            RETURN ERROR.
         END.
         ELSE DO:
            IF (Especiales.Sdo_Pendiente - Producto.Debe) LT 0 THEN DO:
               MESSAGE "El Valor a Cancelar Supera el Saldo Pendiente."
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
               TITLE "Error en Especiales".
               RETURN ERROR.
            END.
         END.
      END.
      ELSE DO:
         IF Especiales.Cuo_Pagadas GE Especiales.Plazo THEN DO:
            MESSAGE "El Producto No Tiene Cuotas Pendientes de Pago."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Error en Especiales".
            RETURN ERROR.
         END.
         ELSE DO:
            ASSIGN T_Cuota = Producto.Debe / (Especiales.Cuota * Especiales.Cantidad).
            IF (Especiales.Cuo_Pagadas + T_Cuota) GT Especiales.Plazo THEN DO:
               MESSAGE "El Valor de Pago Supera el Número de Cuotas Pendientes."
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
               TITLE "Error en Especiales".
               RETURN ERROR.
            END.
         END.
      END.
      IF Pro_Especiales.Cuo_FijaVar EQ TRUE THEN DO:
         ASSIGN T_Cuota = Especiales.Cuota * Especiales.Cantidad
                T_Rdo   = Producto.Debe MODULO T_Cuota.
         IF T_Rdo NE 0 THEN DO:
            MESSAGE "La Cuota del Producto Debe Ser el Valor" SKIP 
                    "Exacto o un Multiplo del Valor."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            TITLE "Error en Especiales".
            RETURN ERROR.
         END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_Operacion C-Win 
PROCEDURE Val_Operacion :
/*------------------------------------------------------------------------------
  OBSERVACION : Validar la autorizacion de la operacion.       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER T_OfiVal  LIKE Agencias.Agencia.
  DEFINE INPUT  PARAMETER T_GrpVal  LIKE Grupos.Grupo.
  DEFINE INPUT  PARAMETER T_UsuVal  LIKE Usuarios.Usuario.
  DEFINE INPUT  PARAMETER T_OpeVal  LIKE Operacion.Cod_Operacion.
  DEFINE OUTPUT PARAMETER T_Validar AS LOGICAL.
  DEFINE OUTPUT PARAMETER T_NomOpe  LIKE Operacion.Nom_Operacion.
  
  DEFINE VAR T_Clave AS LOGICAL.
  
  ASSIGN T_Validar = FALSE T_Clave = FALSE.
  FIND Operacion WHERE Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Operacion) THEN DO:
     ASSIGN T_NomOpe = Operacion.Nom_Operacion
            T_Clave  = Operacion.Id_Clave.
  END.
  ELSE ASSIGN T_NomOpe = "".
  
  IF T_Clave THEN DO:
     MESSAGE "La Operación "  T_NomOpe   SKIP
             "Requiere Clave de SuperUsuario."
     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
     TITLE "Validación En Taquilla".
     RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
     IF W_Error EQ FALSE THEN DO:
        ASSIGN T_Validar = TRUE.
     END.
  END.
  
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 3
                           AND   Res_Operacion.Usuario       EQ T_UsuVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 2
                           AND   Res_Operacion.Grupo         EQ T_GrpVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 1
                           AND   Res_Operacion.Agencia       EQ T_OfiVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_RetAportes C-Win 
PROCEDURE Val_RetAportes :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Validar Retiro en los Aportes.       
------------------------------------------------------------------------------*/
   DEFIN VAR W_VrDeducible LIKE Ahorros.Sdo_Disponible.   
  
   FOR FIRST Pro_Ahorros WHERE pro_ahorros.tip_ahorro EQ INTEGER(SUBSTRING(Producto.TipPto,3,1))
                         AND   pro_ahorros.cod_ahorro EQ Producto.CodPto 
                         AND   Pro_Ahorros.Estado       EQ 1 NO-LOCK,
       FIRST Ahorros     WHERE Ahorros.Nit            EQ Producto.Nit
                         AND   ahorros.cod_ahorro     EQ pro_ahorros.cod_ahorro
                         AND   Ahorros.Cue_Ahorros    EQ Producto.Cuenta NO-LOCK:
       IF Producto.Retiro THEN DO:
          IF Ahorros.Sdo_Disponible LT Producto.Haber THEN DO:
             MESSAGE "El Valor a Retirar Debe Ser Igual al" SKIP
                     "Saldo Disponible Para La Cuenta: " Ahorros.Sdo_Disponible 
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
             TITLE "Error En Taquilla".
             RETURN ERROR.
          END.

          IF Ahorros.Agencia NE W_Agencia THEN DO:
             MESSAGE "La cancelación de la cuenta de Aportes solo puede realizarla" SKIP
                     "En la Agencia de la Cuenta."
                 VIEW-AS ALERT-BOX ERROR.
             RETURN ERROR.
          END.

       END.
       ELSE DO:
          IF Ahorros.Sdo_Disponible LT Producto.Haber THEN DO:
                MESSAGE "El Valor a Retirar Es Superior al" SKIP
                        "Saldo Disponible Para La Cuenta"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                TITLE "Error En Taquilla".
                RETURN ERROR.
          END.
          
          IF (Ahorros.Sdo_Disponible - Producto.Haber - P_ImpAplic) LT Pro_Ahorros.Val_SdoMinimo THEN DO:
                MESSAGE "No Es Posible Hacer el Retiro. Se Debe Dejar" SKIP
                        "La Cantidad Que Cubra el Sdo-Mìnimo + el GMF" SKIP                  
                VIEW-AS ALERT-BOX QUESTION BUTTONS OK
                TITLE "Error En Taquilla".
                RETURN ERROR.
          END.
       
          IF Pro_Ahorros.Id_Retparcial THEN DO:
             IF Pro_Ahorros.Id_MontoMinimo THEN DO:
                IF Pro_Ahorros.Val_MinRetiro GT Producto.Haber AND Producto.EC EQ "E" THEN DO:
                   MESSAGE "El Valor minimo de Retiro en Efectivo" SKIP
                           "Para esta cuenta es de: $" STRING(Pro_Ahorros.Val_MinRetiro,">>>,>>>,>>9") SKIP(1)
                           "Desea Autorizar esta Transacción?"
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                   TITLE "Error En Taquilla" UPDATE Sw_Estado.
                   IF Sw_Estado EQ TRUE THEN DO:
                      RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                      IF W_Error EQ FALSE THEN RETURN ERROR.
                   END.
                   ELSE RETURN ERROR.
                END.
                
                IF Pro_Ahorros.Id_MontoMaximo AND Producto.EC EQ "E" AND
                   Producto.Haber GT Pro_Ahorros.Val_MaxRetEfectivo THEN DO:
                   MESSAGE "El Valor Maximo de Retiro en Efectivo" SKIP
                           "Para esta cuenta es de: $" STRING(Pro_Ahorros.Val_MaxRetEfectivo,">>>,>>>,>>9") SKIP(1)
                           "Desea Autorizar esta Transacción?"
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                   TITLE "Error En Taquilla" UPDATE Sw_Estado.
                   IF Sw_Estado EQ TRUE THEN DO:
                      RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                      IF W_Error EQ FALSE THEN RETURN ERROR.
                   END.
                   ELSE RETURN ERROR.
                END.
                
                IF Pro_Ahorros.Val_MinRetCheque GT Producto.Haber AND Producto.EC EQ "C" THEN DO:
                   MESSAGE "El Valor minimo de Retiro en Cheque" SKIP
                           "Para esta cuenta es de: $" STRING(Pro_Ahorros.Val_MinRetCheque,">>>,>>>,>>9") SKIP(1)
                           "Desea Autorizar esta Transacción?"
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                   TITLE "Error En Taquilla" UPDATE Sw_Estado.
                   IF Sw_Estado EQ TRUE THEN DO:
                      RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                      IF W_Error EQ FALSE THEN RETURN ERROR.
                   END.
                   ELSE RETURN ERROR.
                END.
             END.
          END.
          ELSE DO:
             MESSAGE "El Producto No Permite Retiros Parciales." SKIP
                     "Desea Autorizar la Transacción ...?"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
             TITLE "Error En Taquilla" UPDATE Sw_Estado.
             IF Sw_Estado THEN DO:
                RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                IF NOT W_Error THEN 
                   RETURN ERROR.
             END.
             ELSE RETURN ERROR.
          END.
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_RetContractual C-Win 
PROCEDURE Val_RetContractual :
/*------------------------------------------------------------------------------
  Observación :       
------------------------------------------------------------------------------*/
   DEFINE VAR W_vrDeducible LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR P_Dias AS INTEGER.
   
   FOR FIRST Pro_Ahorros WHERE pro_ahorros.tip_ahorro EQ INTEGER(SUBSTRING(Producto.TipPto,3,1))
                         AND   pro_ahorros.cod_ahorro EQ Producto.CodPto 
                         AND   Pro_Ahorros.Estado       EQ 1 NO-LOCK,
       FIRST Ahorros     WHERE Ahorros.Nit            EQ Producto.Nit
                         AND   ahorros.cod_ahorro     EQ pro_ahorros.cod_ahorro
                         AND   Ahorros.Cue_Ahorros    EQ Producto.Cuenta NO-LOCK:
       IF Producto.Retiro EQ TRUE THEN DO:
          IF (Ahorros.Sdo_Disponible + Ahorros.Int_Pagar - Producto.Haber) NE 0 THEN DO:
             MESSAGE "No Es Posible Retirar VALOR Diferente al (Disponible + Intereses x Pagar)." SKIP
                     "Es una cancelaciòn..."
                        VIEW-AS ALERT-BOX QUESTION BUTTONS OK
                TITLE "Error En Taquilla".
             RETURN ERROR.
          END.

          IF Ahorros.INT_Causado NE 0 THEN DO: 
             MESSAGE "No Es Posible Retirar, Tiene Int-Causados." SKIP
                     "Es una cancelaciòn..."
                        VIEW-AS ALERT-BOX QUESTION BUTTONS OK
                TITLE "Error En Taquilla".
             RETURN ERROR.
          END.
             
          IF Pro_Ahorros.Id_Vencimiento EQ TRUE THEN DO:
                IF Ahorros.Fec_Vencimiento GT TODAY THEN DO:
                   MESSAGE "El PLazo Para el Vencimiento del Contractual No Se Ha Cumplido...." SKIP
                           "              Si desea cancelarlo debe Validar la Transacción."
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                   RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                   IF NOT W_Error THEN 
                      RETURN ERROR.
                END.

                ELSE CASE Pro_Ahorros.Tip_Vencimiento:
                  WHEN 1 THEN DO:
                     IF Ahorros.Fec_Vencimiento GT TODAY THEN DO:
                        MESSAGE "El PLazo Para el Vencimiento del" SKIP
                                "Producto No Se Ha Cumplido."
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                        TITLE "Error En Taquilla".
                        RETURN ERROR.
                     END.
                  END.
                  WHEN 2 THEN DO:
                     IF (Ahorros.Cuota * Ahorros.Plazo) GT Ahorros.Sdo_Disponible THEN DO:
                        MESSAGE "El Monto Acordado Para el Vencimiento" SKIP
                                "es Mayor al Saldo Disponible Actual."
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                        TITLE "Error En Taquilla".
                        RETURN ERROR.
                     END. 
                   END.
                   WHEN 3 THEN DO:
                     IF (Ahorros.Sdo_Disponible / Ahorros.Cuota) LT Ahorros.Plazo THEN DO:
                        MESSAGE "El Número de Cuotas Acordado Para el " SKIP
                                "Vencimiento No Se Han Cumplido."
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                        TITLE "Error En Taquilla".
                        RETURN ERROR.
                     END.
                   END.
                END CASE.
          END.
       END.
       ELSE DO:
          IF Pro_Ahorros.Id_Retparcial EQ TRUE THEN DO:
             IF (Ahorros.Sdo_Disponible - Producto.Haber) LT 0 THEN DO:
                MESSAGE "El Valor a Retirar Excede el Disponible del Producto." SKIP
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                TITLE "Error En Taquilla".
                RETURN ERROR.
             END.
          END.
          ELSE DO:
             MESSAGE "El Producto No Permite Retiros Parciales." SKIP
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
             TITLE "Error En Taquilla".
             RETURN ERROR.
          END.
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_RetTermino C-Win 
PROCEDURE Val_RetTermino :
/*------------------------------------------------------------------------------
   Observaciones:       
   ------------------------------------------------------------------------------*/

   FOR FIRST Pro_Ahorros WHERE pro_ahorros.tip_ahorro EQ INTEGER(SUBSTRING(Producto.TipPto,3,1))
                         AND   pro_ahorros.cod_ahorro EQ Producto.CodPto 
                         AND   Pro_Ahorros.Estado       EQ 1 NO-LOCK,
       FIRST Ahorros     WHERE Ahorros.Nit            EQ Producto.Nit
                         AND   ahorros.cod_ahorro     EQ pro_ahorros.cod_ahorro
                         AND   Ahorros.Cue_Ahorros    EQ Producto.Cuenta NO-LOCK:
       IF Producto.Retiro THEN DO:
          IF Ahorros.Fec_Vencimiento GT W_Fecha AND Ahorros.Fec_Prorroga = ? THEN DO:
             MESSAGE "La Fecha Acordada Para El Vencimiento" SKIP
                     "del Producto No Se Ha Cumplido."
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
             TITLE "Error En Taquilla".
             RETURN ERROR.
          END.
          ELSE DO:
             /*IF NOT (W_Fecha GE Ahorros.Fec_Prorroga AND W_Fecha LE Ahorros.Fec_ProLiquidacion
                     AND Ahorros.Int_Causado = 0) THEN DO:
               MESSAGE "La Fecha de Gracia de la prorroga a expirado." 
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
               TITLE "Error En Taquilla".
               RETURN ERROR.
             END.*/

             IF Ahorros.Fec_Prorroga NE ? AND Ahorros.Int_Causado GT 0 THEN DO:
               MESSAGE "El Título ya tiene Int-Causados, deben ser trasladados antes de esta operación."
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
               TITLE "Error En Taquilla".
               RETURN ERROR.
             END.
             ELSE DO:
               IF (Ahorros.Sdo_Disponible + Ahorros.Int_Pagar) NE Producto.Haber THEN DO:
                  MESSAGE "El Saldo Disponible Para El Producto No" SKIP
                          "Corresponde Con el Valor Digitado, Saldo + Intereses."
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                  TITLE "Error En Taquilla".
                  RETURN ERROR.
               END.
             END.
          END.
       END.
       ELSE DO:
          /* 05/01/2005 SE PERMITE RETIRAR los intereses*/
    IF Pro_Ahorros.Tip_Interes EQ 1 THEN DO:
             /*Enero 26/06 Gaer*/
             IF Producto.Haber LE Ahorros.Int_Pagar  
             OR Producto.Haber EQ (Ahorros.Int_Pagar + Ahorros.sdo_disponible) THEN.
            ELSE DO:
                MESSAGE "El Producto Solo Permite Retirar parte o el total de Los Intereses Por Pagar." SKIP
                        "Si no es cancelacion total."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                TITLE "Error En Taquilla".
                RETURN ERROR.
             END.

            /* IF     NOT (Ahorros.Int_Pagar EQ Producto.Haber OR Producto.Haber = Ahorros.Int_Pagar + Ahorros.sdo_disponible) THEN DO:
                MESSAGE "El Producto Solo Permite Retirar el" SKIP
                        "Valor de Los Intereses Por Pagar."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                TITLE "Error En Taquilla".
                RETURN ERROR.
             END.Comentariado Enero 26/06 Gaer*/
          END.
          ELSE DO:
             MESSAGE "El Producto No Permite Retiros, Solo la" SKIP
                     "Cancelación al Cumplimiento del Plazo."
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
             TITLE "Error En Taquilla".
             RETURN ERROR.
          END.
       
     END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_RetVista C-Win 
PROCEDURE Val_RetVista :
DEFINE VAR W_SalMinimo LIKE Ahorros.Sdo_MinCta.
   DEFI   VAR W_MaxRetiro   LIKE Ahorros.Sdo_MinCta INIT 0.
   DEFINE VAR W_VrDeducible LIKE Ahorros.Sdo_MinCta.
   
   FOR FIRST Pro_Ahorros WHERE pro_ahorros.tip_ahorro EQ INTEGER(SUBSTRING(Producto.TipPto,3,1))
                         AND   pro_ahorros.cod_ahorro EQ Producto.CodPto 
                         AND   Pro_Ahorros.Estado       EQ 1 NO-LOCK,
       FIRST Ahorros     WHERE Ahorros.Nit            EQ Producto.Nit
                         AND   ahorros.cod_ahorro     EQ pro_ahorros.cod_ahorro
                         AND   Ahorros.Cue_Ahorros    EQ Producto.Cuenta NO-LOCK:
       IF Ahorros.Sdo_Disponible LT 0 THEN DO:                                               
                  MESSAGE "La Cuenta Esta en Sobregiro."       SKIP                                  
                         "No es Posible Afectar el Producto." SKIP                                  
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK                                           
                  TITLE "Error En Taquilla".                                                         
                  RETURN ERROR.                                                                      
       END.                                                                    
                         
       IF Pro_Ahorros.Id_Salminimo THEN DO:
          IF Pro_Ahorros.Tip_Salminimo EQ TRUE THEN DO:
             ASSIGN W_SalMinimo = Pro_Ahorros.Val_SdoMinimo
                    Producto.MinCta = Pro_Ahorros.Val_SdoMinimo.
          END.
          ELSE DO:
             ASSIGN W_SalMinimo = Ahorros.Sdo_MinCta
                    Producto.MinCta = Ahorros.Sdo_MinCta.
          END.
       END.
       ELSE 
          ASSIGN W_SalMinimo = 0.

       IF ahorros.ajuste = 1 THEN /* No condiciona Sdo_minimo porque es Convenio de N¢mina */
          ASSIGN W_SalMinimo = 0. /* 1 de julio de 2007 */
          
       IF Producto.Retiro EQ TRUE THEN DO:
          IF Ahorros.Sdo_Disponible GT 0 THEN DO:
             IF Ahorros.Sdo_Canje GT 0 THEN DO:
                MESSAGE "No Es Posible Cancelar el Producto." SKIP
                        "Exixte al Menos un Cheque en Canje." 
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                TITLE "Error En Taquilla".
                RETURN ERROR.
             END.
             
             IF Ahorros.Sdo_Disponible LT Producto.Haber THEN DO:
                MESSAGE "El Valor Digitado No Corresponde Con el Saldo"      SKIP    
                           "Disponible de la Cuenta Para Cancelar el Producto." SKIP    
                           "El valor del retiro debe ser Màximo: " Ahorros.Sdo_Disponible     
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Error En Taquilla".  
                RETURN ERROR.                                                        
             END.
          END.

        /*  IF Ahorros.Agencia NE W_Agencia AND Ahorros.Cod_Ahorro NE 23 THEN DO:
             MESSAGE "La cancelación de la cuenta de Ahorros solo puede realizarla" SKIP
                     "En la Agencia de la Cuenta."
                 VIEW-AS ALERT-BOX ERROR.
             RETURN ERROR.
          END.*/
       END.
       ELSE DO:
          IF W_SalMinimo LE (Ahorros.Sdo_Disponible - Producto.Haber - P_ImpAplic) THEN DO:
             IF (Ahorros.Sdo_Disponible - Producto.Haber - P_ImpAplic) LT W_SalMinimo THEN DO:
                MESSAGE "No Es Posible Hacer el Retiro. Se Debe Dejar" SKIP
                        "La Cantidad Que Cubra El Deducible y el Sdo-Mìnimo." SKIP
                        "El Valor del GMF es: " P_ImpAplic
                VIEW-AS ALERT-BOX QUESTION BUTTONS OK
                TITLE "Error En Taquilla".
                RETURN ERROR.
             END.
             
             IF Producto.EC EQ "E" THEN DO:
                IF Pro_Ahorros.Id_Montominimo EQ TRUE           AND
                   Pro_Ahorros.Val_MinRetiro  GT Producto.Haber THEN DO:
                   MESSAGE "El Valor No Supera el Monto Establecido"  SKIP
                           "Como Mínimo de Retiro en Efectivo."       SKIP
                           "Se puede retirar minimo: $ " STRING(Pro_Ahorros.Val_MinRetiro,">>>,>>>,>>9") SKIP(1)
                           "Desea Autorizar la Transacción ...?"
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                   TITLE "Error En Taquilla" UPDATE Sw_Estado.
                   IF Sw_Estado EQ TRUE THEN DO:
                      RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                      IF W_Error EQ FALSE THEN DO:
                         RETURN ERROR.
                      END.
                   END.
                   ELSE DO:
                     RETURN ERROR.
                   END.
                END.
                ELSE DO:
                   IF Pro_Ahorros.Id_Montomaximo     EQ TRUE           AND
                      Pro_Ahorros.Val_MaxRetEfectivo LT Producto.Haber THEN DO:
                      MESSAGE "El Valor Supera el Monto Establecido"  SKIP
                              "Como Máximo de Retiro en Efectivo."    SKIP
                              "Desea Autorizar la Transacción ...?"
                      VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                      TITLE "Error En Taquilla" UPDATE Sw_Estado.
                      IF Sw_Estado EQ TRUE THEN DO:
                         RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                         IF W_Error EQ FALSE THEN RETURN ERROR.
                      END.
                      ELSE RETURN ERROR.
                   END.
                END.
             END.
             ELSE DO:
                IF Pro_Ahorros.Id_Montominimo   EQ TRUE           AND
                   Pro_Ahorros.Val_MinRetcheque GT Producto.Haber THEN DO:
                   MESSAGE "El Valor No Supera el Monto Establecido"  SKIP
                           "Como Mínimo de Retiro en Cheque."         SKIP
                           "Se puede retirar minimo: $ " STRING(Pro_Ahorros.Val_MinRetCheque,">>>,>>>,>>9") SKIP(1)
                           "Desea Autorizar la Transacción ...?"
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
                   TITLE "Error En Taquilla" UPDATE Sw_Estado.
                   IF Sw_Estado EQ TRUE THEN DO:
                      RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                      IF W_Error EQ FALSE THEN RETURN ERROR.
                   END.
                   ELSE RETURN ERROR.
                END.
             END.
          END.
          ELSE DO:
            IF Pro_Ahorros.Id_AfeSdoMinimo EQ TRUE THEN DO:
               MESSAGE "La Cuenta No Tiene Saldo "   SKIP
                       "Disponible Para el Retiro."  SKIP
                       "Desea Afectar el Saldo Mínimo ...?"
               VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
               TITLE "Error En Taquilla" UPDATE Sw_Estado.
               IF Sw_Estado EQ TRUE THEN DO:
                  RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
                  IF W_Error EQ FALSE THEN RETURN ERROR.
                  ELSE DO:
                     RUN Sdo_Minimo NO-ERROR.
                     IF ERROR-STATUS:ERROR THEN RETURN ERROR.
                  END.
               END.
               ELSE RETURN ERROR.
            END.
            ELSE DO:
               W_MaxRetiro = 0.
               IF Ahorros.Sdo_Disponible GT W_SalMinimo THEN DO:
                  ASSIGN W_MaxRetiro = (Ahorros.Sdo_Disponible - W_SalMinimo).
                  IF P_ImpAplic GT 0 THEN DO:
                     RUN RutGMF.P (INPUT FALSE,W_Agencia,Ahorros.Agencia,1,Ahorros.Cod_Ahorro,Ahorros.Nit, 
                                   INPUT Ahorros.Cue_Ahorro,T_Ope,W_MaxRetiro,Operacion.Comprobante,          
                                   INPUT STRING(Producto.DtoRef),"Por Taquilla",0,0,OUTPUT P_ImpAplic) NO-ERROR.
                     ASSIGN W_MaxRetiro = W_MaxRetiro - P_ImpAplic.
                  END.
                  
                  IF W_MaxRetiro LT Producto.Haber THEN DO:
                     MESSAGE "La Cuenta No Tiene Saldo "  SKIP
                       "Disponible Para el Retiro..."  SKIP
                       "Puede Retirar solo hasta $ " W_MaxRetiro
                       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                       TITLE "Error En Taquilla" UPDATE Sw_Estado.

                     RETURN ERROR.
                  END.
               END.
            END.
          END.
       END.          
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VerFoto C-Win 
PROCEDURE VerFoto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF TRIM(F_Nit:SCREEN-VALUE IN FRAME DEFAULT-FRAME) NE "" THEN DO:
   ASSIGN RutaFoto = "imagenes\fotos\" + TRIM(F_Nit:SCREEN-VALUE IN FRAME DEFAULT-FRAME) + ".jpg".
   
   P_Foto:LOAD-IMAGE(RutaFoto) IN FRAME F_Foto NO-ERROR.

   /*  
   IF ERROR-STATUS:ERROR THEN
     MESSAGE "No Existe la foto del asociado" VIEW-AS ALERT-BOX.
  ELSE
  */
     VIEW FRAME F_Foto.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar_Estado C-Win 
PROCEDURE Verificar_Estado :
IF Producto.EstCta GT 2 AND Producto.EstCta EQ 11 AND 
   Producto.OfiTem NE W_Agencia THEN DO:
     FIND Varios WHERE Varios.Tipo EQ 21 AND Varios.Codigo EQ Producto.EstCta NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN DO:
        MESSAGE "La cuenta se encuentra bloqueada." SKIP
                "Tipo de Bloqueo: " Varios.Descripcion SKIP(1)
                "Solo se podrán hacer operaciones a esta" SKIP
                "cuenta, desde la agencia en donde fue" SKIP
                "matriculada, o sea en la agencia " Producto.OfiTem VIEW-AS ALERT-BOX ERROR.
        ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0"
               Producto.DtoRef:SCREEN-VALUE IN BROWSE BROWSE-5  = ""
               Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
        APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        MESSAGE "No se encuentra el estado de la cuenta" SKIP
                "tal ves no se encuentra matriculado" SKIP
                "el estado actual es: " Ahorros.Detalle_Estado VIEW-AS ALERT-BOX ERROR.
        ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
        ASSIGN Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
        APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
        RETURN NO-APPLY.
     END.
END.
ELSE IF Producto.EstCta EQ 12 THEN DO:
     FIND Varios WHERE Varios.Tipo EQ 21 AND Varios.Codigo EQ 12 NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN DO:
        MESSAGE "La cuenta se encuentra bloqueada." SKIP
                "Tipo de Bloqueo: " Varios.Descripcion SKIP(1)
                VIEW-AS ALERT-BOX ERROR.
        ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0"
               Producto.DtoRef:SCREEN-VALUE IN BROWSE BROWSE-5  = ""
               Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
        APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        MESSAGE "No se encuentra el estado de la cuenta" SKIP
                "tal ves no se encuentra matriculado" SKIP
                "el estado actual es: " Ahorros.Detalle_Estado VIEW-AS ALERT-BOX ERROR.
        ASSIGN Producto.Debe:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
        ASSIGN Producto.Haber:SCREEN-VALUE IN BROWSE BROWSE-5 = "0".
        APPLY "ENTRY" TO Producto.Debe IN BROWSE BROWSE-5.
        RETURN NO-APPLY.
     END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica_CobroJuridico C-Win 
PROCEDURE Verifica_CobroJuridico :
DEFINE VAR W_MensajeCobro AS CHARACTER FORMAT "X(100)".
FOR EACH Creditos WHERE Creditos.Nit EQ Clientes.Nit AND
                        Creditos.Abogado NO-LOCK:
   W_MensajeCobro = "Cdto: " + STRING(Creditos.Num_Credito) + " - Abogado: " + Creditos.Nom_Juzgado.
END.
IF W_MensajeCobro NE "" THEN
   MESSAGE W_MensajeCobro VIEW-AS ALERT-BOX WARNING TITLE "Se encuentra en cobro juridico".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

