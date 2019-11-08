&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-Mvto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Mvto 
CREATE WIDGET-POOL.

DEFINE VAR W_ManMov AS HANDLE.

{incluido\VARIABLE.I "SHARED"}

DEFINE VAR W_OfiOrigen AS INTEGER.
DEFINE VAR W_OfStr AS CHARACTER FORMAT "X(20)".
DEFINE VAR W_CbStr AS CHARACTER FORMAT "X(20)".
DEFINE VAR W_Rpta AS LOGICAL.
DEFINE VAR W_NroDocG AS INTEGER.
DEFINE VAR W_SiGirado AS LOGICAL.
DEFINE VAR W_RowidMC AS ROWID.
DEFINE VAR vcCuentaBase AS CHARACTER.

DEFINE TEMP-TABLE Tmp
    FIELD W_Fila AS INTEGER
    FIELD W_Cuenta AS CHARACTER LABEL "Cuenta":C FORMAT "X(10)"
    FIELD W_Comentario AS CHARACTER LABEL "Concepto":C FORMAT "X(70)"
    FIELD W_CCostos AS INTEGER LABEL " CC ":C FORMAT "ZZZ"
    FIELD W_Enlace AS CHARACTER LABEL "Enlace":C FORMAT "X(12)"
    FIELD W_Nit AS CHARACTER LABEL "Nit":C
    FIELD W_Documento AS CHARACTER LABEL "Doc.Ref":C
    FIELD W_Debito AS DECIMAL LABEL "Débitos":C FORMAT ">>>,>>>,>>>,>>9.99"
    FIELD W_Credito AS DECIMAL LABEL "Créditos":C FORMAT ">>>,>>>,>>>,>>9.99"
    FIELD W_Base AS DECIMAL

    /* oakley */

    FIELD W_Naturaleza AS CHARACTER
    FIELD W_Id_Nit AS LOGICAL
    FIELD W_Id_Doc AS LOGICAL
    FIELD W_Id_Det AS LOGICAL
    FIELD W_Id_Base AS LOGICAL
    FIELD W_Id_CenCostos AS LOGICAL
    FIELD W_Ctr_Natur AS LOGICAL
    FIELD W_Id_Enlace AS LOGICAL INITIAL NO
    FIELD W_Nomcta AS CHARACTER
    FIELD W_Nomccs AS CHARACTER
    FIELD W_Nomcli AS CHARACTER FORMAT "X(30)"
    FIELD W_NomBas AS CHARACTER FORMAT "X(30)"
    FIELD W_PorBas AS DECIMAL DECIMALS 2 FORMAT "99.99"
    FIELD W_Cod_Producto AS INTEGER
    FIELD W_Det_VAmort AS DECIMAL
    INDEX W_Fila IS PRIMARY W_Fila ASCENDING.

DEFINE VARIABLE W_Aux AS DECIMAL INITIAL 0.
DEFINE VARIABLE W_Auxstr AS CHARACTER.
DEFINE VARIABLE W_Secuencia AS INTEGER FORMAT "99999999".

/* oakley */

DEFINE VARIABLE W_ConAux AS CHARACTER FORMAT "X(25)".
DEFINE VARIABLE W_CcoAux AS INTEGER.
DEFINE VARIABLE W_DocAux AS CHARACTER.
DEFINE VARIABLE W_ConEfe AS LOGICAL.
DEFINE VARIABLE W_Metodo AS LOGICAL.
DEFINE VARIABLE W_NitAux AS CHARACTER.
DEFINE VARIABLE W_CompAct AS INTEGER.
DEFINE VARIABLE W_OfImpresion AS INTEGER.
DEFINE VARIABLE W_OfConsulta AS INTEGER.
DEFINE VARIABLE W_CbteImpresion AS INTEGER.
DEFINE VARIABLE W_ScreenCbte AS CHARACTER FORMAT "X(29)".
DEFINE VARIABLE W_AuxCbteScreen AS INTEGER.

W_Ofiorigen = W_Agencia.

DEFINE VARIABLE open-recid AS RECID.
DEFINE VARIABLE open-on-row AS INTEGER.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE Sw_SelfCr AS LOGICAL.
DEFINE VARIABLE P_Nit AS CHARACTER.
DEFINE VARIABLE p_Nombre AS CHARACTER.
DEFINE VARIABLE P_Apellido AS CHARACTER.
DEFINE VARIABLE P_AgeCli AS INTEGER.
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR j AS INTEGER.
DEFINE VAR W_DC AS CHARACTER FORMAT "X" INITIAL "D".
DEFINE VAR W_Existe AS LOGICAL.

DEFINE TEMP-TABLE reversar LIKE mov_contable.

/* Variables para las Cuentas de Activos Fijos*/
DEFINE VAR cuentaActivoFijo AS CHARACTER.

DEFINE TEMP-TABLE anotaActivosFijos
    FIELD id AS CHARACTER
    FIELD numDoc AS INTEGER
    FIELD valor AS DECIMAL.

DEFINE TEMP-TABLE ttActivos
    FIELD id AS CHARACTER
    FIELD valor AS DECIMAL.

DEFINE VAR secuenciaRev AS INTEGER.

/* oakley */

DEFINE TEMP-TABLE ttmovContable LIKE mov_contable2.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define BROWSE-NAME B_Costos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cen_Costos Cuentas TMP

/* Definitions for BROWSE B_Costos                                      */
&Scoped-define FIELDS-IN-QUERY-B_Costos Cen_Costos.Agencia Cen_Costos.Cen_Costos Cen_Costos.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Costos   
&Scoped-define SELF-NAME B_Costos
&Scoped-define QUERY-STRING-B_Costos FOR EACH Cen_Costos WHERE Cen_Costos.Estado = 1                                           AND Cen_Costos.Agencia = W_OfiOrigen NO-LOCK
&Scoped-define OPEN-QUERY-B_Costos OPEN QUERY B_Costos FOR EACH Cen_Costos WHERE Cen_Costos.Estado = 1                                           AND Cen_Costos.Agencia = W_OfiOrigen NO-LOCK.
&Scoped-define TABLES-IN-QUERY-B_Costos Cen_Costos
&Scoped-define FIRST-TABLE-IN-QUERY-B_Costos Cen_Costos


/* Definitions for BROWSE B_Cuentas                                     */
&Scoped-define FIELDS-IN-QUERY-B_Cuentas Cuentas.Cuenta Cuentas.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Cuentas   
&Scoped-define SELF-NAME B_Cuentas
&Scoped-define QUERY-STRING-B_Cuentas FOR EACH Cuentas WHERE            Cuentas.Estado    EQ 1  AND            Cuentas.Tipo      EQ 2  AND            Cuentas.Id_NoMvto EQ NO AND            Cuentas.id_nodlle EQ NO AND            Cuentas.Fec_Retiro EQ ?        NO-LOCK BY Cuentas.Cuenta INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Cuentas OPEN QUERY B_Cuentas        FOR EACH Cuentas WHERE            Cuentas.Estado    EQ 1  AND            Cuentas.Tipo      EQ 2  AND            Cuentas.Id_NoMvto EQ NO AND            Cuentas.id_nodlle EQ NO AND            Cuentas.Fec_Retiro EQ ?        NO-LOCK BY Cuentas.Cuenta INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Cuentas Cuentas
&Scoped-define FIRST-TABLE-IN-QUERY-B_Cuentas Cuentas


/* Definitions for BROWSE TEMP                                          */
&Scoped-define FIELDS-IN-QUERY-TEMP W_Cuenta W_Comentario W_CCostos W_Enlace W_Nit W_Documento W_Debito W_Credito   
&Scoped-define ENABLED-FIELDS-IN-QUERY-TEMP ALL   
&Scoped-define SELF-NAME TEMP
&Scoped-define QUERY-STRING-TEMP PRESELECT EACH TMP
&Scoped-define OPEN-QUERY-TEMP OPEN QUERY TEMP PRESELECT EACH TMP.
&Scoped-define TABLES-IN-QUERY-TEMP TMP
&Scoped-define FIRST-TABLE-IN-QUERY-TEMP TMP


/* Definitions for FRAME F_Costos                                       */

/* Definitions for FRAME F_Cuenta                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Cuenta ~
    ~{&OPEN-QUERY-B_Cuentas}

/* Definitions for FRAME F_Mov                                          */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fecDocumento E_DocEncabezado E_DocConsulta ~
W_OfiCon W_TipCon W_NumCon Btn_EjecutarCon Btn_CnlImp-2 
&Scoped-Define DISPLAYED-OBJECTS fecDocumento E_DocEncabezado E_DocConsulta ~
W_OfiCon W_TipCon W_NumCon 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 W_NomNit W_Valor W_NomCen W_Base W_NomCom W_DifCre ~
W_DifDeb W_Nombre W_TotCre W_Totdeb W_UltGra W_Fec_Contable W_FechaSis ~
W_Hora Nom_Agencia W_Saldo 
&Scoped-define List-6 FD_ValAmortizacion FD_Plazo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Mvto AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-TEMP 
       MENU-ITEM m_Insertar_registro LABEL "Inserta Fila"  
       MENU-ITEM m_Borrar_registro LABEL "Borra Fila"    
       RULE
       MENU-ITEM m_Grabar       LABEL "Grabar"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnReversar 
     LABEL "&Reversar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_CnlImp-2 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Cancelar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_EjecutarCon 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "&Consultar" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE W_OfiCon AS CHARACTER FORMAT "X(40)":U 
     LABEL " Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_TipCon AS CHARACTER FORMAT "X(29)":U 
     LABEL "Comprobante" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fecDocumento AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha del documento" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE W_NumCon AS INTEGER FORMAT "99999999":U INITIAL 0 
     LABEL "Número" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .92
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE E_DocConsulta AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 107 BY 10.23
     FONT 2 NO-UNDO.

DEFINE VARIABLE E_DocEncabezado AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 107 BY 3.23
     FONT 2 NO-UNDO.

DEFINE BUTTON Btn_TerminaCenCos 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Terminar Consulta" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE W_CAgencia AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NCostos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON Btn_SalCuenta AUTO-GO 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Terminar Consulta" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE W_CNombre AS CHARACTER FORMAT "X(25)":U 
     LABEL "Consulta" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81 TOOLTIP "Nombre o Cuenta a consultar, Con blanco consulta todas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON Bt_SalirDetalle 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Salir" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE FD_Agencia AS CHARACTER FORMAT "X(25)":U 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_CenCostos AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Cen.Costos" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_Credito AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Credito" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_Cuenta AS CHARACTER FORMAT "X(30)":U 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_Debito AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Debito" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_Documento AS CHARACTER FORMAT "X(10)" 
     LABEL "Documento Referencia" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_FecContable AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Contable" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_FecGrabacion AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de Grabación" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_Final AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Saldo Final" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_Nit AS CHARACTER FORMAT "X(30)":U 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_Plazo AS DECIMAL FORMAT "99":U INITIAL 0 
     LABEL "Plazo en Meses" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_UltActualizacion AS DATE FORMAT "99/99/9999":U 
     LABEL "Fec. Ultima Actualización" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_ValAmortizacion AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Cuota Amortizacion" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 5 NO-UNDO.

DEFINE VARIABLE FD_ValInicial AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE BUTTON Btn_CnlImp 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Cancelar" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Ejecutar" 
     SIZE 9 BY 1.62.

DEFINE VARIABLE W_OfiImp AS CHARACTER FORMAT "X(40)":U 
     LABEL " Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_TipImp AS CHARACTER FORMAT "X(29)":U 
     LABEL "     Tipo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_FecImp AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroDocF AS INTEGER FORMAT "99999999":U INITIAL 0 
     LABEL "Final" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroDoci AS INTEGER FORMAT "99999999":U INITIAL 0 
     LABEL "Inicial" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON btnRecalcular 
     IMAGE-UP FILE "imagenes/proceso.bmp":U
     LABEL "Button 159" 
     SIZE 5.14 BY 1.38.

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 17" 
     SIZE 5 BY 1.12.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "C&onsulta" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Btn_Grabar 
     LABEL "&Grabar" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Btn_Salir DEFAULT 
     LABEL "&Salir" 
     SIZE 9 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-25 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 25" 
     SIZE 9 BY 1.62.

DEFINE VARIABLE W_CmbOfip AS CHARACTER FORMAT "X(25)":U 
     LABEL "Origen" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Comprobante AS CHARACTER FORMAT "X(29)":U 
     LABEL "Comprobante" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_OfDestino AS CHARACTER FORMAT "X(25)":U 
     LABEL "Destino" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-ValBase AS DECIMAL FORMAT ">>>,>>,>>9.99":U INITIAL 0 
     LABEL "Valor Impuesto" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fec_Consulta AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Documento Consulta" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Agencia AS CHARACTER FORMAT "X(28)":U 
     VIEW-AS FILL-IN 
     SIZE 98 BY 1.35
     BGCOLOR 18 FGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE W_Agenciap AS CHARACTER FORMAT "X(28)":U 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .92
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Base AS DECIMAL FORMAT "->>9.99":U INITIAL 0 
     LABEL "Porcentaje" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CpteP AS CHARACTER FORMAT "X(28)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .92
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DifCre AS DECIMAL FORMAT ">>>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_DifDeb AS DECIMAL FORMAT ">>>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Diferencia" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_Doc AS INTEGER FORMAT "99999999":U INITIAL 0 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FechaSis AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Sistema" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Fec_Contable AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Contable" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Hora AS CHARACTER FORMAT "X(8)":U INITIAL "0" 
     LABEL "Hora" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Nombre AS CHARACTER FORMAT "X(40)":U 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCen AS CHARACTER FORMAT "X(25)":U 
     LABEL "Cen. Costos" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCom AS CHARACTER FORMAT "X(56)":U 
     LABEL "Concepto" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomNit AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Saldo AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotCre AS DECIMAL FORMAT ">>>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Totdeb AS DECIMAL FORMAT ">>>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Totales" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_UltGra AS CHARACTER FORMAT "X(7)":U 
     LABEL "Ultimo documento grabado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Valor AS DECIMAL FORMAT ">>>>>>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cálculo de Base" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-217
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY 5.38.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 3.77.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 3.73.

DEFINE VARIABLE W_NomBenCC AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 51.86 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_SiNo AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Si", yes,
"No", no
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE Rs_SiNoCheq AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Si", yes,
"No", no
     SIZE 15 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-315
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.57 BY 1.35.

DEFINE RECTANGLE RECT-316
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.57 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_Costos FOR 
      Cen_Costos SCROLLING.

DEFINE QUERY B_Cuentas FOR 
      Cuentas SCROLLING.

DEFINE QUERY TEMP FOR 
      TMP SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_Costos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Costos W-Mvto _FREEFORM
  QUERY B_Costos NO-LOCK DISPLAY
      Cen_Costos.Agencia
      Cen_Costos.Cen_Costos
      Cen_Costos.Nombre
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 5.38
         BGCOLOR 15 FGCOLOR 0 FONT 4.

DEFINE BROWSE B_Cuentas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Cuentas W-Mvto _FREEFORM
  QUERY B_Cuentas NO-LOCK DISPLAY
      Cuentas.Cuenta
      Cuentas.Nombre
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 44 BY 9.42
         BGCOLOR 15 FGCOLOR 0 FONT 4.

DEFINE BROWSE TEMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS TEMP W-Mvto _FREEFORM
  QUERY TEMP NO-LOCK DISPLAY
      W_Cuenta      WIDTH 12
      W_Comentario  WIDTH 16
      W_CCostos
      W_Enlace
      W_Nit         FORMAT "X(13)" WIDTH 13
      W_Documento
      W_Debito      FORMAT ">>>,>>>,>>>,>>9.99" WIDTH 12
      W_Credito     FORMAT ">>>,>>>,>>>,>>9.99" WIDTH 12
      ENABLE ALL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 11.58
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Costos
     B_Costos AT ROW 1.27 COL 2
     W_CAgencia AT ROW 7.73 COL 9 COLON-ALIGNED
     Btn_TerminaCenCos AT ROW 7.73 COL 35
     W_NCostos AT ROW 8.54 COL 9 COLON-ALIGNED
     "Digite el nombre a buscar" VIEW-AS TEXT
          SIZE 23 BY .81 AT ROW 6.65 COL 2
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 52.86 ROW 6.73
         SIZE 47.43 BY 9.58
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Consulta de Centros de Costo".

DEFINE FRAME F_Cuenta
     W_CNombre AT ROW 11.77 COL 11 COLON-ALIGNED
     B_Cuentas AT ROW 1.27 COL 2
     Btn_SalCuenta AT ROW 10.69 COL 35
     " Digite Nombre Ó Cuenta para buscar" VIEW-AS TEXT
          SIZE 32.72 BY .81 AT ROW 10.69 COL 1.72
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 20.57 ROW 6.92
         SIZE 46.43 BY 13.46
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Consulta de Cuenta".

DEFINE FRAME F_Mov
     TEMP AT ROW 6.92 COL 3
     btnRecalcular AT ROW 18.65 COL 98.86 WIDGET-ID 6
     F-ValBase AT ROW 22 COL 71 COLON-ALIGNED WIDGET-ID 2
     BUTTON-25 AT ROW 7.46 COL 104
     W_NomNit AT ROW 21.19 COL 16 COLON-ALIGNED
     W_Valor AT ROW 21.19 COL 71 COLON-ALIGNED
     W_NomCen AT ROW 20.38 COL 16 COLON-ALIGNED
     W_Base AT ROW 20.38 COL 71 COLON-ALIGNED
     W_NomCom AT ROW 19.58 COL 16 COLON-ALIGNED
     W_DifCre AT ROW 19.31 COL 83.72 COLON-ALIGNED NO-LABEL
     W_DifDeb AT ROW 19.31 COL 70.86 COLON-ALIGNED
     W_Nombre AT ROW 18.77 COL 16 COLON-ALIGNED
     W_TotCre AT ROW 18.5 COL 83.72 COLON-ALIGNED NO-LABEL
     W_Totdeb AT ROW 18.5 COL 70.86 COLON-ALIGNED
     Fec_Consulta AT ROW 5.85 COL 100 COLON-ALIGNED
     W_UltGra AT ROW 5.88 COL 100 COLON-ALIGNED
     Agencias.Agencia AT ROW 19.04 COL 1 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
     Comprobantes.Nombre AT ROW 19.04 COL 1 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.86 BY .81
     Agencias.Nombre AT ROW 19.04 COL 1 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
     W_Fec_Contable AT ROW 5.08 COL 100 COLON-ALIGNED HELP
          "Entre la fecha a la que pertenece el movimiento contable"
     W_CpteP AT ROW 5.77 COL 16 COLON-ALIGNED NO-LABEL
     Comprobantes.Comprobante AT ROW 19.04 COL 1 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
     W_FechaSis AT ROW 4.27 COL 100 COLON-ALIGNED
     W_Hora AT ROW 3.46 COL 100 COLON-ALIGNED
     Nom_Agencia AT ROW 1.23 COL 9.29 NO-LABEL
     W_CmbOfip AT ROW 3.69 COL 16 COLON-ALIGNED
     W_Agenciap AT ROW 3.69 COL 16 COLON-ALIGNED
     W_OfDestino AT ROW 4.73 COL 16 COLON-ALIGNED HELP
          "Elija el Destino con Alt+Cursor Abajo"
     W_Comprobante AT ROW 5.77 COL 16 COLON-ALIGNED HELP
          "Elija el Tipo de Comprobante con Alt+Cursor abajo"
     W_Doc AT ROW 5.77 COL 58 COLON-ALIGNED HELP
          "Entre el número de documento"
     Btn_Grabar AT ROW 13.65 COL 104
     Btn_Cancelar AT ROW 15.27 COL 104
     Btn_Salir AT ROW 16.88 COL 104
     Btn_Consulta AT ROW 10.69 COL 104
     Btn_Imprimir AT ROW 9.08 COL 104
     Btn_Ayuda AT ROW 22.81 COL 106
     W_Saldo AT ROW 22 COL 16 COLON-ALIGNED WIDGET-ID 4
     " Identificación del Comprobante Contable" VIEW-AS TEXT
          SIZE 36 BY 1.08 AT ROW 2.62 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-217 AT ROW 7.19 COL 103
     RECT-28 AT ROW 3.15 COL 73
     RECT-29 AT ROW 3.15 COL 3
    WITH 1 DOWN NO-BOX OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.57 BY 23.15
         BGCOLOR 17 FGCOLOR 0 FONT 5
         CANCEL-BUTTON Btn_Salir.

DEFINE FRAME F_Detalle
     FD_Agencia AT ROW 1.15 COL 9.14 COLON-ALIGNED
     FD_Cuenta AT ROW 1.96 COL 9.14 COLON-ALIGNED
     FD_Nit AT ROW 2.77 COL 9.14 COLON-ALIGNED
     FD_CenCostos AT ROW 3.58 COL 9.14 COLON-ALIGNED
     FD_Documento AT ROW 4.38 COL 24.57 COLON-ALIGNED
     FD_FecContable AT ROW 5.27 COL 24.57 COLON-ALIGNED
     FD_FecGrabacion AT ROW 6.19 COL 24.57 COLON-ALIGNED
     FD_UltActualizacion AT ROW 7.15 COL 24.57 COLON-ALIGNED
     FD_ValInicial AT ROW 8.12 COL 24.57 COLON-ALIGNED
     FD_Debito AT ROW 9.04 COL 24.57 COLON-ALIGNED
     FD_Credito AT ROW 9.96 COL 24.57 COLON-ALIGNED
     FD_Final AT ROW 10.88 COL 24.57 COLON-ALIGNED
     FD_ValAmortizacion AT ROW 11.88 COL 24.57 COLON-ALIGNED
     FD_Plazo AT ROW 12.77 COL 24.57 COLON-ALIGNED
     Bt_SalirDetalle AT ROW 13.77 COL 33
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 4.23
         SIZE 42 BY 15.35
         BGCOLOR 17 FONT 4
         TITLE "Información de la Factura a Cobrar o Pagar".

DEFINE FRAME F_Consulta
     fecDocumento AT ROW 5.04 COL 80 COLON-ALIGNED WIDGET-ID 4
     E_DocEncabezado AT ROW 6.85 COL 3 NO-LABEL NO-TAB-STOP 
     E_DocConsulta AT ROW 11.42 COL 3 NO-LABEL NO-TAB-STOP 
     W_OfiCon AT ROW 1.5 COL 58.29 COLON-ALIGNED
     W_TipCon AT ROW 2.69 COL 58.29 COLON-ALIGNED
     W_NumCon AT ROW 3.92 COL 58.29 COLON-ALIGNED
     Btn_EjecutarCon AT ROW 1.38 COL 98.86
     Btn_CnlImp-2 AT ROW 4.77 COL 98.86
     btnReversar AT ROW 3.08 COL 98.86 WIDGET-ID 2
     "Información de las Partidas Contables" VIEW-AS TEXT
          SIZE 36 BY .88 AT ROW 10.35 COL 3
          FGCOLOR 7 
     "Encabezado del Comprobante" VIEW-AS TEXT
          SIZE 30 BY .88 AT ROW 5.77 COL 3
          FGCOLOR 7 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.57 ROW 1.27
         SIZE 111.43 BY 22
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Consulta de un Documento".

DEFINE FRAME F_Imprimir
     W_FecImp AT ROW 5.85 COL 8 COLON-ALIGNED
     W_OfiImp AT ROW 1.54 COL 8 COLON-ALIGNED
     W_TipImp AT ROW 2.62 COL 8 COLON-ALIGNED
     W_NroDoci AT ROW 3.69 COL 8 COLON-ALIGNED
     W_NroDocF AT ROW 4.77 COL 8 COLON-ALIGNED
     Btn_Imp AT ROW 1.54 COL 44
     Btn_CnlImp AT ROW 3.69 COL 44
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 45.86 ROW 7.5
         SIZE 55 BY 6.92
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Documento a Imprimir".

DEFINE FRAME F_SiImp
     Rs_SiNoCheq AT ROW 2.73 COL 20.57 NO-LABEL
     W_NomBenCC AT ROW 4.88 COL 2.14 NO-LABEL
     Rs_SiNo AT ROW 8.31 COL 21.86 NO-LABEL
     "Desea Imprimir el documento...?" VIEW-AS TEXT
          SIZE 31 BY 1.08 AT ROW 6.85 COL 14
          BGCOLOR 7 FGCOLOR 15 
     "Nombre del Beneficiario del Cheque(Puede Modificarlo)" VIEW-AS TEXT
          SIZE 51.72 BY .81 AT ROW 4.12 COL 2.29
          BGCOLOR 7 FGCOLOR 15 
     "Desea Imprimir el Cheque ?" VIEW-AS TEXT
          SIZE 31 BY 1.08 AT ROW 1.31 COL 12.86
          BGCOLOR 7 FGCOLOR 15 
     RECT-315 AT ROW 8.04 COL 20.29
     RECT-316 AT ROW 2.46 COL 19
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 20.43 ROW 7.85
         SIZE 54.57 BY 9.81
         BGCOLOR 17 
         TITLE "Impresión del Comprobante Y/O El Cheque".


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
  CREATE WINDOW W-Mvto ASSIGN
         HIDDEN             = YES
         TITLE              = "Modulo de Digitación de la Contabilidad, Prog.W-Mov_Contable.W"
         HEIGHT             = 23.15
         WIDTH              = 117.57
         MAX-HEIGHT         = 36.62
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 36.62
         VIRTUAL-WIDTH      = 182.86
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Mvto:LOAD-ICON("imagenes/e_reporte.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/e_reporte.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Mvto
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Detalle:FRAME = FRAME F_Mov:HANDLE.

/* SETTINGS FOR FRAME F_Consulta
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME F_Consulta:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnReversar IN FRAME F_Consulta
   NO-ENABLE                                                            */
ASSIGN 
       E_DocConsulta:AUTO-RESIZE IN FRAME F_Consulta      = TRUE.

ASSIGN 
       E_DocEncabezado:AUTO-RESIZE IN FRAME F_Consulta      = TRUE.

/* SETTINGS FOR FRAME F_Costos
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_Costos TEXT-1 F_Costos */
ASSIGN 
       FRAME F_Costos:HIDDEN           = TRUE
       FRAME F_Costos:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN W_CAgencia IN FRAME F_Costos
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Cuenta
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB B_Cuentas W_CNombre F_Cuenta */
ASSIGN 
       FRAME F_Cuenta:HIDDEN           = TRUE
       FRAME F_Cuenta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Detalle
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Detalle:HIDDEN           = TRUE
       FRAME F_Detalle:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN FD_Agencia IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_CenCostos IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_Credito IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_Cuenta IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_Debito IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_Documento IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_FecContable IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_FecGrabacion IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_Final IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_Nit IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_Plazo IN FRAME F_Detalle
   6                                                                    */
/* SETTINGS FOR FILL-IN FD_UltActualizacion IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FD_ValAmortizacion IN FRAME F_Detalle
   6                                                                    */
/* SETTINGS FOR FILL-IN FD_ValInicial IN FRAME F_Detalle
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Imprimir
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Imprimir:HIDDEN           = TRUE
       FRAME F_Imprimir:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Mov
   Custom                                                               */
/* BROWSE-TAB TEMP 1 F_Mov */
/* SETTINGS FOR FILL-IN Agencias.Agencia IN FRAME F_Mov
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN Comprobantes.Comprobante IN FRAME F_Mov
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       Comprobantes.Comprobante:HIDDEN IN FRAME F_Mov           = TRUE.

/* SETTINGS FOR FILL-IN F-ValBase IN FRAME F_Mov
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fec_Consulta IN FRAME F_Mov
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Fec_Consulta:HIDDEN IN FRAME F_Mov           = TRUE.

/* SETTINGS FOR FILL-IN Comprobantes.Nombre IN FRAME F_Mov
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       Comprobantes.Nombre:HIDDEN IN FRAME F_Mov           = TRUE.

/* SETTINGS FOR FILL-IN Agencias.Nombre IN FRAME F_Mov
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN Nom_Agencia IN FRAME F_Mov
   NO-ENABLE ALIGN-L 4                                                  */
ASSIGN 
       TEMP:POPUP-MENU IN FRAME F_Mov             = MENU POPUP-MENU-TEMP:HANDLE.

/* SETTINGS FOR FILL-IN W_Agenciap IN FRAME F_Mov
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Base IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_CpteP IN FRAME F_Mov
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DifCre IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_DifDeb IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_FechaSis IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_Fec_Contable IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_Hora IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_Nombre IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_NomCen IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_NomCom IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_NomNit IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_Saldo IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_TotCre IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_Totdeb IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_UltGra IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN W_Valor IN FRAME F_Mov
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FRAME F_SiImp
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_SiImp:HIDDEN           = TRUE
       FRAME F_SiImp:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN W_NomBenCC IN FRAME F_SiImp
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Mvto)
THEN W-Mvto:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Costos
/* Query rebuild information for BROWSE B_Costos
     _START_FREEFORM
OPEN QUERY B_Costos FOR EACH Cen_Costos WHERE Cen_Costos.Estado = 1
                                          AND Cen_Costos.Agencia = W_OfiOrigen NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE B_Costos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Cuentas
/* Query rebuild information for BROWSE B_Cuentas
     _START_FREEFORM

   OPEN QUERY B_Cuentas
       FOR EACH Cuentas WHERE
           Cuentas.Estado    EQ 1  AND
           Cuentas.Tipo      EQ 2  AND
           Cuentas.Id_NoMvto EQ NO AND
           Cuentas.id_nodlle EQ NO AND
           Cuentas.Fec_Retiro EQ ?
       NO-LOCK BY Cuentas.Cuenta INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE B_Cuentas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Consulta
/* Query rebuild information for FRAME F_Consulta
     _Query            is NOT OPENED
*/  /* FRAME F_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Costos
/* Query rebuild information for FRAME F_Costos
     _Query            is NOT OPENED
*/  /* FRAME F_Costos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cuenta
/* Query rebuild information for FRAME F_Cuenta
     _Query            is NOT OPENED
*/  /* FRAME F_Cuenta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Detalle
/* Query rebuild information for FRAME F_Detalle
     _Query            is NOT OPENED
*/  /* FRAME F_Detalle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Imprimir
/* Query rebuild information for FRAME F_Imprimir
     _Query            is NOT OPENED
*/  /* FRAME F_Imprimir */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Mov
/* Query rebuild information for FRAME F_Mov
     _Query            is NOT OPENED
*/  /* FRAME F_Mov */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_SiImp
/* Query rebuild information for FRAME F_SiImp
     _Query            is NOT OPENED
*/  /* FRAME F_SiImp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE TEMP
/* Query rebuild information for BROWSE TEMP
     _START_FREEFORM
OPEN QUERY TEMP PRESELECT EACH TMP.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE TEMP */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Mvto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Mvto W-Mvto
ON END-ERROR OF W-Mvto /* Modulo de Digitación de la Contabilidad, Prog.W-Mov_Contable.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Mvto W-Mvto
ON WINDOW-CLOSE OF W-Mvto /* Modulo de Digitación de la Contabilidad, Prog.W-Mov_Contable.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  ON RETURN RETURN.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Detalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Detalle W-Mvto
ON GO OF FRAME F_Detalle /* Información de la Factura a Cobrar o Pagar */
DO:
  HIDE FRAME F_Detalle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME btnRecalcular
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRecalcular W-Mvto
ON CHOOSE OF btnRecalcular IN FRAME F_Mov /* Button 159 */
DO:
    w_totDeb = 0.
    w_totCre = 0.

    FOR EACH Tmp WHERE TRIM(Tmp.w_Cuenta) NE "" BY Tmp.W_Fila:
        ASSIGN W_TotDeb = W_TotDeb + W_Debito
               W_TotCre = W_TotCre + W_Credito.
    END.

    ASSIGN W_DifDeb = IF W_TotDeb LT W_TotCre THEN W_TotCre - W_TotDeb ELSE 0
           W_DifCre = IF W_TotDeb GT W_TotCre THEN W_TotDeb - W_TotCre ELSE 0.

    DISPLAY W_TotDeb W_TotCre W_DifDeb W_DifCre WITH FRAME F_Mov.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME btnReversar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReversar W-Mvto
ON CHOOSE OF btnReversar IN FRAME F_Consulta /* Reversar */
DO:
    DEFINE VAR comprobanteRev AS INTEGER.
    DEFINE VAR agenciaRev AS INTEGER.
    
    MESSAGE "Está seguro que desea Reversar este documento?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Reversión" UPDATE ynReversar AS LOGICAL.

    IF ynReversar = FALSE THEN
        RETURN NO-APPLY.

    FOR EACH reversar NO-LOCK BREAK BY reversar.agencia:
        IF FIRST-OF(reversar.agencia) THEN DO:
            FIND FIRST comprobantes WHERE comprobantes.comprobante = 9
                                      AND comprobantes.agencia = reversar.agencia NO-ERROR.
            secuenciaRev = comprobantes.secuencia + 1.
            comprobantes.secuencia = comprobantes.secuencia + 1.
            comprobanteRev = comprobantes.comprobante.
            agenciaRev = reversar.agencia.

            FIND CURRENT comprobantes NO-LOCK NO-ERROR.
        END.

        CREATE mov_contable.
        BUFFER-COPY reversar TO mov_contable.

        mov_contable.comprobante = 9.
        mov_contable.num_documento = secuenciaRev.
        mov_contable.fec_contable = DATE(w_fec_contable:SCREEN-VALUE IN FRAME F_Mov).
        mov_contable.usuario = w_usuario.
        mov_contable.fec_grabacion = TODAY.
        mov_contable.comentario = "Rev/" + mov_contable.comentario.

        IF mov_contable.db <> 0 THEN DO:
            mov_contable.cr = mov_contable.db.
            mov_contable.db = 0.
        END.
        ELSE DO:
            mov_contable.db = mov_contable.cr.
            mov_contable.cr = 0.
        END.

        IF SUBSTRING(mov_contable.cuenta,1,2) = "17" THEN
            RUN ActivosFijos(INPUT mov_contable.db,
                             INPUT mov_contable.cr,
                             INPUT mov_contable.cuenta,
                             INPUT mov_contable.num_documento).

        IF LAST-OF(reversar.agencia) THEN
            RUN reversarProducto.
    END.

    MESSAGE "Documento Reversado"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    j = E_DocConsulta:NUM-ITEMS.

    DO i = 1 TO j BY 1:
        W_Ok = E_DocConsulta:DELETE(1).
    END.

    j = E_DocEncabezado:NUM-ITEMS.

    DO i = 1 TO j BY 1:
        W_Ok = E_DocEncabezado:DELETE(1).
    END.

    btnReversar:SENSITIVE = FALSE.

    RUN RutinaImprimirReversar (INPUT comprobanteRev,
                                INPUT agenciaRev,
                                INPUT secuenciaRev,
                                INPUT w_fecha).

    APPLY "choose" TO Btn_CnlImp-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReversar W-Mvto
ON TAB OF btnReversar IN FRAME F_Consulta /* Reversar */
DO:
  APPLY "CHOOSE":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Mvto
ON CHOOSE OF Btn_Ayuda IN FRAME F_Mov /* Button 17 */
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "AYUDAS\CONTABIL" CONTEXT 31.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar W-Mvto
ON CHOOSE OF Btn_Cancelar IN FRAME F_Mov /* Cancelar */
DO:
  ASSIGN W_Conaux = ""
         W_Nitaux = "".
  RUN MostrarMensaje IN W_Manija (INPUT 6, OUTPUT W_Eleccion).
  IF NOT W_Eleccion THEN
     RETURN NO-APPLY.
  RUN Cancelar.
  BROWSE Temp:SENSITIVE = FALSE.
  APPLY "ENTRY" TO W_Comprobante IN FRAME F_Mov.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imprimir
&Scoped-define SELF-NAME Btn_CnlImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CnlImp W-Mvto
ON CHOOSE OF Btn_CnlImp IN FRAME F_Imprimir /* Cancelar */
DO:
  FRAME F_Mov:SENSITIVE = TRUE.
  FRAME F_Imprimir:HIDDEN = TRUE.
  APPLY "ENTRY" TO W_Comprobante IN FRAME F_Mov.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_CnlImp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CnlImp-2 W-Mvto
ON CHOOSE OF Btn_CnlImp-2 IN FRAME F_Consulta /* Cancelar */
DO:
  FRAME F_Consulta:HIDDEN = TRUE.   
  RUN Activar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-Mvto
ON CHOOSE OF Btn_Consulta IN FRAME F_Mov /* Consulta */
DO:
    DO WITH FRAME F_Consulta:
        j = E_DocConsulta:NUM-ITEMS.

        DO i = 1 TO j BY 1:
            W_Ok = E_DocConsulta:DELETE(1).
        END.

        j = E_DocEncabezado:NUM-ITEMS.

        DO i = 1 TO j BY 1:
            W_Ok = E_DocEncabezado:DELETE(1).
        END.

        btnReversar:SENSITIVE = FALSE.
    END.

    FIND FIRST Tmp NO-LOCK NO-ERROR.
    IF AVAILABLE(Tmp) THEN DO:
        IF Tmp.W_Cuenta NE "" THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 7,
                                            OUTPUT W_Eleccion).

            IF NOT W_Eleccion THEN
                RETURN.
        END.
    END.

    ASSIGN W_AgenciaP:SENSITIVE IN FRAME F_Mov = FALSE
           W_Doc:SENSITIVE = FALSE
           /*W_Fec_Contable:SENSITIVE = FALSE*/
           Btn_Grabar:SENSITIVE = FALSE
           Btn_Cancelar:SENSITIVE = FALSE
           Btn_Salir:SENSITIVE = FALSE
           Btn_Consulta:SENSITIVE = FALSE
           Btn_Imprimir:SENSITIVE = FALSE.

    DISPLAY E_DocConsulta WITH FRAME F_Consulta.

    ASSIGN FRAME F_Consulta:VISIBLE = TRUE
           FRAME F_Mov:SENSITIVE = FALSE
           W_OfiCon:SCREEN-VALUE IN FRAME F_Consulta = W_CmbOfip:SCREEN-VALUE IN FRAME F_Mov.

    APPLY "ENTRY" TO W_OfiCon IN FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_EjecutarCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_EjecutarCon W-Mvto
ON CHOOSE OF Btn_EjecutarCon IN FRAME F_Consulta /* Consultar */
DO:
    EMPTY TEMP-TABLE ttMovContable.

    DO WITH FRAME F_Consulta:
        j = E_DocConsulta:NUM-ITEMS.

        DO i = 1 TO j BY 1:
            W_Ok = E_DocConsulta:DELETE(1).
        END.

        j = E_DocEncabezado:NUM-ITEMS.

        DO i = 1 TO j BY 1:
            W_Ok = E_DocEncabezado:DELETE(1).
        END.
    END.

    ASSIGN fecDocumento.

    IF fecDocumento = ? THEN DO:
        MESSAGE "La fecha es obligatoria para poder" SKIP
                "realizar la consulta...!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "ENTRY" TO fecDocumento.
        RETURN NO-APPLY.
    END.

    FIND FIRST Mov_Contable WHERE Mov_Contable.Agencia EQ INTEGER(SUBSTRING(W_OfiCon:SCREEN-VALUE IN FRAME F_Consulta,1,3))
                              AND Mov_Contable.Comprobante EQ INTEGER(SUBSTRING(W_TipCon:SCREEN-VALUE IN FRAME F_Consulta,1,2))
                              AND Mov_Contable.Num_Documento = W_NumCon
                              AND mov_contable.fec_contable = fecDocumento NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Mov_Contable THEN DO:
        FIND FIRST Mov_Contable2 WHERE Mov_Contable2.Agencia = INTEGER(SUBSTRING(W_OfiCon:SCREEN-VALUE IN FRAME F_Consulta,1,3))
                                   AND Mov_Contable2.Comprobante = INTEGER(SUBSTRING(W_TipCon:SCREEN-VALUE IN FRAME F_Consulta,1,2))
                                   AND mov_contable.fec_contable = fecDocumento
                                   AND Mov_Contable.Num_Documento = W_NumCon NO-LOCK NO-ERROR.
        IF NOT AVAILABLE mov_contable2 THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 64,
                                            OUTPUT W_Rpta).

            RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE ttMovContable.
            BUFFER-COPY mov_contable2 TO ttMovContable.
        END.
    END.
    ELSE DO:
        CREATE ttMovContable.
        BUFFER-COPY mov_contable TO ttMovContable.
    END.

    RUN Consulta-Documento.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_EjecutarCon W-Mvto
ON TAB OF Btn_EjecutarCon IN FRAME F_Consulta /* Consultar */
DO:
  APPLY "CHOOSE":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar W-Mvto
ON CHOOSE OF Btn_Grabar IN FRAME F_Mov /* Grabar */
DO:
    ASSIGN w_fec_contable
           w_fechaSis.

    IF w_fec_contable <> w_fechaSis THEN DO:
        MESSAGE "Está grabando un documento contable con una fecha distinta a la fecha del Sistema (" STRING(w_fec_contable,"99/99/9999") ")." SKIP
                "Está seguro de realizar esta contabilización...?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Contabilizacion" UPDATE ynContabilizaOtraFecha AS LOGICAL.

        IF ynContabilizaOtraFecha = NO THEN
            RETURN NO-APPLY.
    END.

    ASSIGN W_Conaux = ""
           W_Nitaux = "".

    EMPTY TEMP-TABLE anotaActivosFijos.
    EMPTY TEMP-TABLE ttActivos.

    IF BROWSE Temp:READ-ONLY OR NUM-RESULTS("Temp") LT 1 OR NUM-RESULTS("Temp") EQ ? THEN
        RETURN NO-APPLY.

    ASSIGN W_ConEfe = FALSE
           W_CompAct = INTEGER(SUBSTRING(W_Comprobante,1,2)).

    FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_OfiOrigen
                              AND Comprobantes.Comprobante EQ W_CompAct NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Comprobantes) THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 268,
                                        OUTPUT W_Eleccion).
        RETURN.
    END.

    RUN Verificar-CtasEfectivo.
    
    IF Comprobantes.Id_Efectivo THEN DO:
        IF W_ConEfe THEN DO:
            RUN Verificar-Mes.
            RUN Grabar.
        END.
        ELSE
            RUN MostrarMensaje IN W_Manija (INPUT 136,
                                            OUTPUT W_Eleccion).
    END.
    ELSE DO:
        IF Comprobantes.Comprobante EQ 3 OR NOT W_ConEfe THEN DO:
            RUN Verificar-Mes.
            RUN Grabar.
        END.
        ELSE
            RUN MostrarMensaje IN W_Manija (INPUT 138, OUTPUT W_Eleccion).
    END.

    W_ConEfe = FALSE.

    IF ERROR-STATUS:ERROR THEN
        IF W_Doc:SENSITIVE IN FRAME F_Mov THEN
            APPLY "ENTRY":U TO W_Comprobante.
        ELSE
            APPLY "VALUE-CHANGED":U TO W_Comprobante.
    ELSE
        APPLY "ENTRY":U TO Tmp.W_Cuenta IN BROWSE Temp.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imprimir
&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp W-Mvto
ON CHOOSE OF Btn_Imp IN FRAME F_Imprimir /* Ejecutar */
DO:
    ASSIGN FRAME F_Imprimir W_FecImp.

    IF W_NroDocI > W_NroDocF THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 148, OUTPUT W_Rpta).
        APPLY "ENTRY" TO W_NroDocI.
        RETURN NO-APPLY.
    END.

    ASSIGN W_fecImp.

    IF w_fecImp = ? THEN DO:
        MESSAGE "La fecha es obligatoria para poder" SKIP
                "realizar la consulta...!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "ENTRY" TO W_fecImp.
        RETURN NO-APPLY.
    END.

    RUN Rutina_Imprimir (INPUT W_CbteImpresion,
                         INPUT W_OfImpresion,
                         INPUT W_NroDocI,
                         INPUT W_NroDocF,
                         INPUT W_FecImp).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-Mvto
ON CHOOSE OF Btn_Imprimir IN FRAME F_Mov /* Imprimir */
DO:
 ASSIGN FRAME F_Mov:SENSITIVE = FALSE
        FRAME F_Imprimir:VISIBLE = TRUE.
/*         w_OfiImp:SCREEN-VALUE IN FRAME F_Imprimir = W_cmbOfip:SCREEN-VALUE IN FRAME F_Mov. */
 /*giocam 02/22/08 - se pone en comentario para evitar error (4058)*/
 APPLY "ENTRY" TO W_OfiImp IN FRAME F_Imprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cuenta
&Scoped-define SELF-NAME Btn_SalCuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalCuenta W-Mvto
ON CHOOSE OF Btn_SalCuenta IN FRAME F_Cuenta /* Terminar Consulta */
DO:
   ASSIGN FRAME F_Cuenta:HIDDEN = TRUE
          FRAME F_Mov:SENSITIVE = TRUE.
   APPLY "ENTRY" TO W_Cuenta IN BROWSE Temp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir W-Mvto
ON CHOOSE OF Btn_Salir IN FRAME F_Mov /* Salir */
DO:
  FIND FIRST Tmp NO-LOCK NO-ERROR.
  IF AVAILABLE(Tmp) THEN DO:
    IF Tmp.W_Cuenta NE "" THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 9, OUTPUT W_Eleccion).
      IF NOT W_Eleccion THEN
        RETURN.
    END.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Costos
&Scoped-define SELF-NAME Btn_TerminaCenCos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_TerminaCenCos W-Mvto
ON CHOOSE OF Btn_TerminaCenCos IN FRAME F_Costos /* Terminar Consulta */
DO:
  CLOSE QUERY B_Costos.
  ASSIGN FRAME F_Costos:HIDDEN = TRUE
         FRAME F_Mov:SENSITIVE = TRUE.
  {&OPEN-QUERY-B_Costos}
  APPLY "ENTRY" TO tmp.W_CCostos IN BROWSE Temp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Detalle
&Scoped-define SELF-NAME Bt_SalirDetalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bt_SalirDetalle W-Mvto
ON CHOOSE OF Bt_SalirDetalle IN FRAME F_Detalle /* Salir */
DO:
    ON RETURN TAB.

    ASSIGN FRAME F_Detalle
        FD_ValAmortizacion
        FD_Plazo.

    /*IF FD_ValAmortizacion EQ 0 OR FD_Plazo EQ 0 THEN DO:
        MESSAGE "Falta Información por llenar. Rectifique!"
            VIEW-AS ALERT-BOX.

        APPLY "entry" TO FD_ValAmortizacion IN FRAME F_Detalle.
        RETURN NO-APPLY.
    END.*/

    ASSIGN Tmp.W_Det_VAmort = FD_ValAmortizacion.

    HIDE FRAME F_Detalle.

    IF W_DC EQ "D" THEN
        APPLY "ENTRY" TO Tmp.W_Debito IN BROWSE Temp.
    ELSE
        APPLY "ENTRY" TO Tmp.W_Credito IN BROWSE Temp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bt_SalirDetalle W-Mvto
ON ENTRY OF Bt_SalirDetalle IN FRAME F_Detalle /* Salir */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME BUTTON-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-25 W-Mvto
ON CHOOSE OF BUTTON-25 IN FRAME F_Mov /* Button 25 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Costos
&Scoped-define FRAME-NAME F_Costos
&Scoped-define SELF-NAME B_Costos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Costos W-Mvto
ON MOUSE-SELECT-CLICK OF B_Costos IN FRAME F_Costos
DO:
   APPLY "VALUE-CHANGED":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Costos W-Mvto
ON MOUSE-SELECT-DBLCLICK OF B_Costos IN FRAME F_Costos
DO:
   APPLY "CHOOSE":U TO Btn_TerminaCenCos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Costos W-Mvto
ON TAB OF B_Costos IN FRAME F_Costos
DO:
  APPLY "CHOOSE":U TO Btn_TerminaCenCos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Costos W-Mvto
ON VALUE-CHANGED OF B_Costos IN FRAME F_Costos
DO:
  IF AVAILABLE(Cen_Costos) THEN
     ASSIGN W_CCostos:SCREEN-VALUE IN BROWSE Temp = STRING(Cen_Costos.Cen_Costos)
            Tmp.W_NomCcs = STRING(Cen_Costos.Nombre).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Cuentas
&Scoped-define FRAME-NAME F_Cuenta
&Scoped-define SELF-NAME B_Cuentas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Cuentas W-Mvto
ON MOUSE-SELECT-CLICK OF B_Cuentas IN FRAME F_Cuenta
DO:
  APPLY "VALUE-CHANGED":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Cuentas W-Mvto
ON MOUSE-SELECT-DBLCLICK OF B_Cuentas IN FRAME F_Cuenta
DO:
  APPLY "CHOOSE":U TO Btn_SalCuenta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Cuentas W-Mvto
ON TAB OF B_Cuentas IN FRAME F_Cuenta
DO:
   APPLY "CHOOSE":U TO Btn_SalCuenta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Cuentas W-Mvto
ON VALUE-CHANGED OF B_Cuentas IN FRAME F_Cuenta
DO:
  IF AVAILABLE(Cuentas) THEN 
     ASSIGN W_Cuenta:SCREEN-VALUE IN BROWSE Temp = Cuentas.Cuenta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Borrar_registro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Borrar_registro W-Mvto
ON CHOOSE OF MENU-ITEM m_Borrar_registro /* Borra Fila */
DO:
  APPLY "CTRL-DEL":U TO BROWSE Temp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Grabar W-Mvto
ON CHOOSE OF MENU-ITEM m_Grabar /* Grabar */
DO:
   IF Btn_Grabar:SENSITIVE IN FRAME F_Mov THEN
      APPLY "CHOOSE":U TO Btn_Grabar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Insertar_registro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Insertar_registro W-Mvto
ON CHOOSE OF MENU-ITEM m_Insertar_registro /* Inserta Fila */
DO:
  IF BROWSE Temp:READ-ONLY
  OR NUM-RESULTS("Temp") LT 1
  OR NUM-RESULTS("Temp") EQ ?
  OR W_Cuenta:SCREEN-VALUE IN BROWSE Temp = "" 
  OR (     W_Debito:SCREEN-VALUE IN BROWSE Temp = "0" 
       AND W_Credito:SCREEN-VALUE IN BROWSE Temp = "0") THEN
     RETURN NO-APPLY.

  GET CURRENT Temp.
  IF NOT AVAILABLE Tmp THEN
     RETURN NO-APPLY.

  GET NEXT Temp.
  IF AVAILABLE Tmp THEN
     IF W_Cuenta = "" 
     OR (     W_Debito = 0 
          AND W_Credito = 0) THEN DO:
        GET PREV Temp.
        RETURN NO-APPLY.
  END.

  GET PREV Temp.
  APPLY "CTRL-INS" TO BROWSE TEMP.
  APPLY "ENTRY" TO Tmp.W_Cuenta IN BROWSE Temp.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_SiImp
&Scoped-define SELF-NAME Rs_SiNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_SiNo W-Mvto
ON MOUSE-SELECT-CLICK OF Rs_SiNo IN FRAME F_SiImp
DO:
   ASSIGN Rs_SiNo.

   IF Rs_SiNo THEN
     RUN Rutina_Imprimir (INPUT W_CompAct,
                          INPUT W_OfiOrigen,
                          INPUT W_NroDocG,
                          INPUT W_NroDocG,
                          INPUT w_fecha).
    
   RUN Cancelar.
    
   ASSIGN Rs_SiNo:SCREEN-VALUE = "No"
          Rs_SiNo.

   IF W_SiGirado THEN DO:
      MESSAGE "Segura(o) de Finalizar la Impresión, Recuerde El Cheque..." SKIP
              "Con SI FINALIZA, Con NO Puede Imprimir Cheque Y/O Docto"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR FINALIZAR...?"
                       UPDATE W_RptaImp AS LOGICAL.
      IF NOT W_RptaImp THEN
         RETURN.
   END.

   HIDE FRAME F_SiImp.
   ASSIGN FRAME F_Mov:SENSITIVE = TRUE.
   APPLY "ENTRY" TO W_Comprobante IN FRAME F_Mov.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_SiNoCheq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_SiNoCheq W-Mvto
ON MOUSE-SELECT-CLICK OF Rs_SiNoCheq IN FRAME F_SiImp
DO:
    DEFINE VAR C_Benef AS CHARACTER FORMAT "X(50)".
    DEFINE VAR C_Ciudad LIKE Ubicacion.Nombre.
    DEFINE VAR W_Cadena AS CHARACTER FORMAT "X(150)".
    DEFINE VAR W_Monto1 AS CHARACTER FORMAT "X(70)".
    DEFINE VAR W_Monto2 AS CHARACTER FORMAT "X(70)".
    DEFINE VAR W_Monto3 AS CHARACTER FORMAT "X(70)".
    DEFINE VAR W_Rpta AS LOGICAL.
    DEFINE VAR vctipo AS CHARACTER FORMAT "X(1)".
    DEFINE VAR vcnit AS CHARACTER FORMAT "X(12)" NO-UNDO.
    DEFINE VAR vinotran AS INTEGER INITIAL 0 NO-UNDO.
    
    ASSIGN Rs_SiNoCheq.

    IF Rs_SiNoCheq AND W_SiGirado THEN DO:
        MESSAGE "Desea Imprimir Ahora el Cheque?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Impresión de Cheque" UPDATE choice2 AS LOGICAL.

        IF CHOICE2 THEN DO:
            FIND FIRST Mov_Contable WHERE ROWID(Mov_Contable) EQ W_RowidMC NO-ERROR.

            IF Mov_Contable.Nit LE " " THEN
                MESSAGE "Recuerde que debe Tener Ced./Nit el Movimiento del Banco," SKIP
                        "Para el Nombre del Beneficiario del Cheque."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ELSE
                FIND FIRST Clientes WHERE Clientes.Nit EQ Mov_Contable.Nit NO-LOCK NO-ERROR.

            ASSIGN C_Benef = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2) WHEN AVAIL(Clientes).

            DISABLE TRIGGERS FOR LOAD OF mov_contable.
            Mov_Contable.Comentario = "Benef : " + TRIM(W_NomBenCC).

            FIND FIRST Mov_Contable WHERE ROWID(Mov_Contable) EQ W_RowidMC NO-LOCK NO-ERROR.

            ASSIGN vctipo = "2"
                   vcnit = ""
                   vinotran = 0.

            RUN MontoEsc.r (INPUT Mov_Contable.Cr,
                            INPUT 0,
                            OUTPUT W_Cadena).

            RUN PartirValor IN W_Manija (INPUT W_Cadena,
                                         INPUT 60,
                                         OUTPUT W_Monto1,
                                         OUTPUT W_Monto2,
                                         OUTPUT W_Monto3).

            RUN F-Cheque.p(INPUT W_monto1,
                           INPUT W_monto2,
                           INPUT C_benef,
                           INPUT c_ciudad,
                           INPUT Mov_Contable.Cr,
                           INPUT Mov_Contable.Comentario,
                           INPUT Mov_Contable.Comprobante,
                           INPUT "",  /*INPUT wxctacon,*/
                           INPUT Mov_Contable.Num_Documento).
        END.
    END.

    ASSIGN Rs_SiNoCheq:SCREEN-VALUE = "No"
           Rs_SiNoCheq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME TEMP
&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME TEMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TEMP W-Mvto
ON CTRL-DEL OF TEMP IN FRAME F_Mov
DO:
  IF Temp:NUM-SELECTED-ROWS EQ 0
  OR BROWSE Temp:READ-ONLY THEN
     RETURN NO-APPLY.

  ASSIGN W_TotDeb = DECIMAL(W_Totdeb:SCREEN-VALUE)
         W_TotCre = DECIMAL(W_TotCre:SCREEN-VALUE).

  DO i = 1 TO Temp:NUM-SELECTED-ROWS:
     ASSIGN W_Metodo = Temp:FETCH-SELECTED-ROW(i).
     IF AVAILABLE Tmp THEN DO:
         ASSIGN W_TotDeb = IF (W_Totdeb - Tmp.W_Debito) GT 0 THEN W_Totdeb -  Tmp.W_Debito 
                          ELSE 0
               W_TotCre = IF (W_TotCre - Tmp.W_Credito) GT 0 THEN W_TotCre - Tmp.W_Credito      
                          ELSE 0            
               W_DifDeb = IF W_TotDeb LT W_TotCre THEN W_TotCre - W_TotDeb ELSE 0
               W_DifCre = IF W_TotDeb GT W_TotCre THEN W_TotDeb - W_TotCre ELSE 0.
       DELETE Tmp.
     END.
  END.

  ASSIGN W_Metodo = Temp:DELETE-SELECTED-ROWS().
  ASSIGN W_TotDeb:SCREEN-VALUE = STRING(W_TotDeb)
         W_TotCre:SCREEN-VALUE = STRING(W_TotCre)
         W_DifDeb:SCREEN-VALUE = STRING(W_DifDeb)
         W_DifCre:SCREEN-VALUE = STRING(W_DifCre).

  IF NUM-RESULTS("Temp") LT 1
  OR NUM-RESULTS("Temp") EQ ? THEN DO:
     APPLY "ENTRY" TO W_Comprobante.
     RETURN NO-APPLY.
  END.  

  ASSIGN W_Base:SCREEN-VALUE IN FRAME F_Mov = "0.00"
         W_Valor:SCREEN-VALUE IN FRAME F_Mov = "0.00"
         F-ValBase:SCREEN-VALUE IN FRAME F_Mov = "0.00".
  
  DISPLAY W_Cuenta W_Comentario W_CCostos W_Enlace W_Nit W_Documento W_Debito W_Credito
          WITH BROWSE Temp.
/*MDB*/
  IF Tmp.W_Cuenta NE "" THEN do:
     FIND FIRST Cuentas WHERE Cuentas.Cuenta = Tmp.W_Cuenta
                          AND Cuentas.Tipo = 2 
                          AND Cuentas.Estado = 1
                          AND Cuentas.Fec_Retiro = ?
                          AND Cuentas.Id_NoMvto EQ NO NO-LOCK NO-ERROR.
     IF AVAILABLE(Cuentas) then
       RUN Cuenta-Encontrada.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TEMP W-Mvto
ON CTRL-INS OF TEMP IN FRAME F_Mov
DO:
   DEFINE VAR Old-Num AS INTEGER INITIAL 0 NO-UNDO. 
 
   IF BROWSE Temp:READ-ONLY THEN
      RETURN NO-APPLY.
   trn-create:
   DO TRANSACTION ON ERROR  UNDO trn-create, RETRY trn-create
                  ON ENDKEY UNDO trn-create, RETRY trn-create:
      IF NOT AVAILABLE Tmp THEN
         FIND LAST Tmp NO-ERROR.
      IF AVAILABLE Tmp THEN
         ASSIGN Old-Num = Tmp.W_Fila.
      CREATE Tmp.
      ASSIGN Tmp.W_Fila = old-num + 1
             Tmp.W_Cuenta = "" .
        ASSIGN Tmp.W_Cuenta = vcCuentaBase.
                
      RUN reorder-browse.
      ASSIGN open-recid  = RECID(Tmp)
             open-on-row = open-on-row + 1. 
      RUN reopen-query.
/*mdb*/ ASSIGN W_Base:SCREEN-VALUE IN FRAME F_Mov = "0.00"
               W_Valor:SCREEN-VALUE IN FRAME F_Mov = "0.00"
               F-ValBase:SCREEN-VALUE IN FRAME F_Mov = "0.00".
   END.
   APPLY "ENTRY" TO Tmp.W_Cuenta IN BROWSE Temp.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TEMP W-Mvto
ON PAGE-DOWN OF TEMP IN FRAME F_Mov
OR PAGE-UP OF Temp
DO:
  IF NOT Temp:READ-ONLY THEN
     RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TEMP W-Mvto
ON ROW-ENTRY OF TEMP IN FRAME F_Mov
DO:
    IF Tmp.W_Cuenta:SCREEN-VALUE IN BROWSE Temp NE "" AND AVAILABLE Tmp THEN DO:
        ASSIGN INPUT BROWSE Temp W_Cuenta.

        FIND FIRST Cuentas WHERE Cuentas.Cuenta = Tmp.W_Cuenta
                             AND Cuentas.Tipo = 2
                             AND Cuentas.Id_NoMvto EQ NO
                             AND cuentas.estado = 1 NO-LOCK.

        RUN Cuenta-Encontrada.
    END.
    ELSE
        ASSIGN W_Base:SCREEN-VALUE IN FRAME F_Mov = "0.00"
               W_Valor:SCREEN-VALUE IN FRAME F_Mov = "0.00"
               F-ValBase:SCREEN-VALUE IN FRAME F_Mov = "0.00".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TEMP W-Mvto
ON ROW-LEAVE OF TEMP IN FRAME F_Mov
DO:
  IF  FRAME F_Mov:SENSITIVE 
  AND (   LASTKEY EQ KEYCODE(KBLABEL("CURSOR-DOWN"))
       OR LASTKEY EQ KEYCODE(KBLABEL("TAB"))
       OR LASTKEY EQ KEYCODE(KBLABEL("RETURN"))) THEN
      IF Tmp.W_Cuenta:SCREEN-VALUE IN BROWSE Temp = "" 
         OR (    W_Debito:SCREEN-VALUE IN BROWSE Temp = "0" 
             AND W_Credito:SCREEN-VALUE IN BROWSE Temp = "0") THEN DO:
         RETURN NO-APPLY.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TEMP W-Mvto
ON VALUE-CHANGED OF TEMP IN FRAME F_Mov
DO:
    ASSIGN open-on-row = SELF:FOCUSED-ROW
           open-recid  = IF AVAILABLE Tmp THEN RECID(Tmp) ELSE ?.

    IF Tmp.W_Cuenta:SCREEN-VALUE IN BROWSE Temp NE "" AND AVAILABLE Tmp THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = Tmp.W_Cuenta
                             AND Cuentas.Tipo = 2 
                             AND Cuentas.Id_NoMvto EQ NO
                             AND cuentas.estado = 1 NO-LOCK.

        RUN Cuenta-Encontrada.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Costos
&Scoped-define SELF-NAME W_CAgencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CAgencia W-Mvto
ON LEAVE OF W_CAgencia IN FRAME F_Costos /* Agencia */
DO:
  ASSIGN FRAME F_Costos W_CAgencia.
         OPEN QUERY B_Costos FOR EACH Cen_Costos WHERE Cen_Costos.Agencia = W_CAgencia 
                                                   AND Cen_Costos.Estado = 1 NO-LOCK.
 IF NUM-RESULTS("B_Costos") EQ 0 THEN
    {&OPEN-QUERY-B_Costos}

  APPLY "ENTRY":U TO B_Costos IN FRAME F_Costos.
  RETURN NO-APPLY.                                                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME W_CmbOfip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfip W-Mvto
ON VALUE-CHANGED OF W_CmbOfip IN FRAME F_Mov /* Origen */
DO:
  DEFINE VAR W_EncontreCbte AS LOGICAL INITIAL FALSE.
  
  IF W_Agenciap:HIDDEN IN FRAME F_Mov THEN
     ASSIGN W_OfiOrigen = INTEGER(SUBSTRING(W_CmbOfiP:SCREEN-VALUE IN FRAME F_Mov, 1, 3)).
  ELSE
     ASSIGN W_OfiOrigen = INTEGER(SUBSTRING(W_Agenciap:SCREEN-VALUE IN FRAME F_Mov, 1, 3)).
  ASSIGN W_CAgencia:SCREEN-VALUE IN FRAME F_Costos = STRING(W_OfiOrigen).
  {&OPEN-QUERY-B_Costos}               
  {&OPEN-QUERY-B_Nit}               
  ASSIGN W_COMPROBANTE:LIST-ITEMS = "".
  FOR EACH Comprobantes FIELDS(Comprobantes.Agencia Comprobantes.Estado Comprobantes.Id_Consecutivo Comprobantes.Comprobante
                               Comprobantes.Nombre)
                        WHERE Comprobantes.Agencia        EQ W_OfiOrigen
                          AND Comprobante.Estado          EQ 1 
                          AND Comprobantes.Id_Consecutivo NE 1  NO-LOCK BY Comprobantes.Comprobante:
       ASSIGN W_CbStr = STRING(Comprobantes.Comprobante,"99") + "-" + STRING(Comprobantes.Nombre, "X(25)") +
                        STRING(Comprobantes.Id_Consecutivo,"9")
              W_Metodo = W_Comprobante:ADD-LAST(W_CbStr) IN FRAME F_Mov.
       IF Comprobantes.Comprobante = W_AuxCbteScreen THEN
         W_EncontreCbte = TRUE.         
  END.
/*mdb*/
  IF W_Comprobante:NUM-ITEMS GT 0 THEN DO:
    W_Comprobante:SCREEN-VALUE IN FRAME F_Mov   = W_Comprobante:ENTRY(1).
    IF W_EncontreCbte THEN DO:
      IF SUBSTRING(W_ScreenCbte,4,25) = SUBSTRING(W_Comprobante:SCREEN-VALUE IN FRAME F_Mov,4,25) THEN
        W_Comprobante:SCREEN-VALUE IN FRAME F_Mov = W_ScreenCbte.
      ELSE
        W_Comprobante:SCREEN-VALUE IN FRAME F_Mov = W_Comprobante:ENTRY(1).      
    END.
  END.
  ELSE DO:
      RUN MostrarMensaje IN W_Manija (INPUT 268, OUTPUT W_Eleccion).
      RETURN NO-APPLY.
  END.    
  APPLY "VALUE-CHANGED":U TO W_Comprobante.
  IF W_AgenciaP:VISIBLE IN FRAME F_Mov THEN
     ASSIGN SELF:SENSITIVE     = FALSE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cuenta
&Scoped-define SELF-NAME W_CNombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CNombre W-Mvto
ON LEAVE OF W_CNombre IN FRAME F_Cuenta /* Consulta */
DO:
  DEFI VAR W_SiNom AS DEC FORM "99999999999999".

  ASSIGN FRAME F_Cuenta W_CNombre
         W_Auxstr = "*" + W_CNombre + "*".

  ASSIGN W_SiNom = DEC(W_CNombre) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN DO:
     OPEN QUERY B_Cuentas FOR EACH Cuentas
               WHERE Cuentas.Cuenta    BEGINS W_CNombre
                 AND Cuentas.Estado    EQ 1
                 AND Cuentas.Tipo      EQ 2
                 AND Cuentas.Id_NoMvto EQ NO
                 AND Cuentas.Fec_Retiro EQ ?  NO-LOCK.
          IF NUM-RESULTS("B_Cuentas") EQ 0 THEN DO:
             {&OPEN-QUERY-B_Cuentas}
             APPLY "VALUE-CHANGED":U TO B_Cuentas.
          END.
  END.
  ELSE DO: 
    CASE W_CNombre:

      WHEN " " THEN
           {&OPEN-QUERY-B_Cuentas} 
      OTHERWISE DO:
          OPEN QUERY B_Cuentas FOR EACH Cuentas
               WHERE Cuentas.Nombre BEGINS W_CNombre
                 AND Cuentas.Estado    EQ 1
                 AND Cuentas.Tipo      EQ 2
                 AND Cuentas.Id_NoMvto EQ NO
                 AND Cuentas.Fec_Retiro EQ ?  NO-LOCK.
          IF NUM-RESULTS("B_Cuentas") EQ 0 THEN DO:
             {&OPEN-QUERY-B_Cuentas}
             APPLY "VALUE-CHANGED":U TO B_Cuentas.
          END.
      END.
   END CASE.
  END.

  IF NUM-RESULTS("B_Cuentas") GT 0 THEN
     W_Metodo = B_Cuentas:SELECT-FOCUSED-ROW().
  
  APPLY "ENTRY":U TO B_Cuentas IN FRAME F_Cuenta.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME W_Comprobante
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Comprobante W-Mvto
ON LEAVE OF W_Comprobante IN FRAME F_Mov /* Comprobante */
DO:
    ASSIGN W_CompAct = INTEGER(SUBSTRING(W_Comprobante, 1, 2)).
    APPLY "ENTRY":U TO BROWSE TEMP.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Comprobante W-Mvto
ON TAB OF W_Comprobante IN FRAME F_Mov /* Comprobante */
OR RETURN OF W_Comprobante
DO:
    APPLY "VALUE-CHANGED" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Comprobante W-Mvto
ON VALUE-CHANGED OF W_Comprobante IN FRAME F_Mov /* Comprobante */
DO:
    IF W_CmbOfip:SENSITIVE IN FRAME F_Mov THEN
        W_OfiOrigen = INTEGER(SUBSTRING(W_CmbOfiP:SCREEN-VALUE IN FRAME F_Mov, 1, 3)).
    ELSE
        W_OfiOrigen = INTEGER(SUBSTRING(W_Agenciap:SCREEN-VALUE IN FRAME F_Mov, 1, 3)).

    ASSIGN W_Comprobante
           W_Secuencia = INTEGER(SUBSTRING(W_Comprobante,29,1))
           W_CompAct = INTEGER(SUBSTRING(W_Comprobante, 1, 2))
           W_ScreenCbte = W_Comprobante:SCREEN-VALUE IN FRAME F_Mov
           W_AuxCbteScreen = W_CompAct.

    IF W_Secuencia EQ 2 THEN DO:
        FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_OfiOrigen
                                  AND Comprobantes.Comprobante EQ W_CompAct NO-LOCK NO-ERROR.
        IF AVAILABLE(Comprobantes) THEN DO:
            IF MONTH(w_fecha) = MONTH(TODAY) AND YEAR(w_fecha) = YEAR(TODAY) THEN
                W_Doc:SCREEN-VALUE IN FRAME F_Mov = STRING(Comprobantes.Secuencia + 1).
            ELSE DO:
                IF comprobantes.ReiniciaCierre = TRUE THEN
                    W_Doc:SCREEN-VALUE IN FRAME F_Mov = STRING(Comprobantes.SecuenciaCierre + 1).
                ELSE
                    W_Doc:SCREEN-VALUE IN FRAME F_Mov = STRING(Comprobantes.Secuencia + 1).
            END.

            W_Doc:SENSITIVE IN FRAME F_Mov = FALSE.
            APPLY "LEAVE":U TO W_Doc.
            RETURN NO-APPLY.
        END.
    END.
    ELSE DO:
        ASSIGN W_Doc:SCREEN-VALUE IN FRAME F_Mov = "0000000"
               W_Doc:SENSITIVE IN FRAME F_Mov = TRUE.
    END.

    IF W_CompAct = 10 THEN DO:
        MESSAGE "Está utilizando el comprobante para Activos Fijos. Tenga en cuenta que" SKIP
                "si no utiliza la cuenta correspondiente al tipo de Activo Fijo que va a" SKIP
                "modificar, el Activo Fijo no se verá afectado por este movimiento"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Doc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Doc W-Mvto
ON LEAVE OF W_Doc IN FRAME F_Mov /* Documento */
DO:
    ASSIGN FRAME F_Mov
        W_OfDestino
        W_Comprobante
        W_Doc
        W_Fec_Contable.

    IF W_Doc = 0 THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 10,
                                        OUTPUT W_Eleccion).
        RETURN NO-APPLY.
    END.

    FIND FIRST Mov_Contable WHERE Mov_Contable.Agencia = W_OfiOrigen
                              AND Mov_Contable.Comprobante = W_CompAct
                              AND Mov_Contable.Num_Documento = W_Doc
                              AND Mov_Contable.Fec_Contable = W_Fecha NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Mov_Contable) THEN DO:
        Temp:SENSITIVE IN FRAME F_Mov = TRUE.

        IF NUM-RESULTS("TEMP") > 0 THEN
            APPLY "ENTRY" TO Tmp.W_Cuenta IN BROWSE Temp.
        ELSE
            APPLY "CTRL-INS" TO BROWSE TEMP.
    END.
    ELSE
        RUN MostrarMensaje IN W_Manija (INPUT 11,
                                        OUTPUT W_Eleccion).

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Fec_Contable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Fec_Contable W-Mvto
ON LEAVE OF W_Fec_Contable IN FRAME F_Mov /* Fecha Contable */
DO:
    ASSIGN w_fec_contable.

    RUN verificar-Mes NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Costos
&Scoped-define SELF-NAME W_NCostos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NCostos W-Mvto
ON LEAVE OF W_NCostos IN FRAME F_Costos /* Nombre */
DO:
  ASSIGN FRAME F_Costos W_NCostos
         FRAME F_Costos W_CAgencia.
  W_Auxstr = "*" + W_NCostos + "*".
  OPEN QUERY B_Costos FOR EACH Cen_Costos WHERE Cen_Costos.Agencia = W_CAgencia
                           AND Cen_Costos.Estado = 1 
                           AND Cen_Costos.Nombre MATCHES W_Auxstr NO-LOCK.
 IF NUM-RESULTS("B_Costos") EQ 0 THEN
    {&OPEN-QUERY-B_Costos}

  APPLY "ENTRY":U TO B_Costos IN FRAME F_Costos.
  RETURN NO-APPLY.                               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_SiImp
&Scoped-define SELF-NAME W_NomBenCC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NomBenCC W-Mvto
ON LEAVE OF W_NomBenCC IN FRAME F_SiImp
DO:
  ASSIGN W_NomBenCC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imprimir
&Scoped-define SELF-NAME W_NroDocF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NroDocF W-Mvto
ON LEAVE OF W_NroDocF IN FRAME F_Imprimir /* Final */
DO:
  {Incluido\BTCANCEL.I}
  ASSIGN W_NroDocF.
  IF W_NroDocI > W_NroDocF THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 148, OUTPUT W_Rpta).
    APPLY "ENTRY" TO W_NroDocI.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NroDoci
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NroDoci W-Mvto
ON LEAVE OF W_NroDoci IN FRAME F_Imprimir /* Inicial */
DO:
  ASSIGN W_NroDocI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME W_NumCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NumCon W-Mvto
ON LEAVE OF W_NumCon IN FRAME F_Consulta /* Número */
DO:
  ASSIGN FRAME F_Consulta W_NumCon.
  APPLY 'entry' TO Btn_EjecutarCon IN FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Mov
&Scoped-define SELF-NAME W_OfDestino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OfDestino W-Mvto
ON LEAVE OF W_OfDestino IN FRAME F_Mov /* Destino */
DO:
  IF W_Doc:SENSITIVE IN FRAME F_Mov = FALSE THEN DO:
     BROWSE Temp:READ-ONLY = FALSE.
     ASSIGN FRAME F_Mov W_OfDestino 
                        W_Comprobante
                        W_Doc 
                        W_Fec_Contable.
     APPLY "ENTRY":U TO W_Comprobante.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME W_OfiCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OfiCon W-Mvto
ON VALUE-CHANGED OF W_OfiCon IN FRAME F_Consulta /*  Agencia */
DO:
   ASSIGN W_OfConsulta = INTEGER(SUBSTRING(W_OfiCon:SCREEN-VALUE, 1, 3))
          W_TipCon:LIST-ITEMS IN FRAME F_Consulta = "".
   FOR EACH Comprobantes FIELDS(Comprobantes.Agencia Comprobantes.Estado Comprobantes.Comprobante 
                                Comprobantes.Nombre Comprobantes.Id_Consecutivo)
                         WHERE Comprobantes.Agencia EQ W_OfConsulta
                           AND Comprobante.Estado   EQ 1 NO-LOCK BY Comprobantes.Comprobante:
       ASSIGN W_CbStr = STRING(Comprobantes.Comprobante,"99") + "-" + STRING(Comprobantes.Nombre, "X(25)") +
                        STRING(Comprobantes.Id_Consecutivo,"9")
              W_Metodo = W_TipCon:ADD-LAST(W_CbStr) IN FRAME F_Consulta.
  END.
  IF W_Tipcon:NUM-ITEMS GT 0 THEN
     ASSIGN  W_Tipcon:SCREEN-VALUE IN FRAME F_Consulta = W_TipCon:ENTRY(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imprimir
&Scoped-define SELF-NAME W_OfiImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OfiImp W-Mvto
ON VALUE-CHANGED OF W_OfiImp IN FRAME F_Imprimir /*  Agencia */
DO:
  ASSIGN W_OfImpresion = INTEGER(SUBSTRING(W_OfiImp:SCREEN-VALUE IN FRAME F_Imprimir, 1, 3))
         W_TipImp:LIST-ITEMS IN FRAME F_Imprimir = "".
  FOR EACH Comprobantes FIELDS(Comprobantes.Agencia Comprobantes.Estado Comprobantes.Comprobante Comprobantes.Nombre)
                        WHERE Comprobantes.Agencia        EQ W_OfImpresion
                          AND Comprobante.Estado          EQ 1 NO-LOCK BY Comprobantes.Comprobante:
       ASSIGN W_CbStr = STRING(Comprobantes.Comprobante,"99") + "-" + STRING(Comprobantes.Nombre, "X(25)")      
             W_Metodo = W_TipImp:ADD-LAST(W_CbStr) IN FRAME F_Imprimir.
  END.

  IF W_TipImp:NUM-ITEMS GT 0 THEN
     ASSIGN  W_TipImp:SCREEN-VALUE IN FRAME F_Imprimir = W_TipImp:ENTRY(1).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_TipImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_TipImp W-Mvto
ON VALUE-CHANGED OF W_TipImp IN FRAME F_Imprimir /*      Tipo */
DO:
  W_CbteImpresion = INTEGER(SUBSTRING(W_TipImp:SCREEN-VALUE IN FRAME F_Imprimir, 1,2)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define BROWSE-NAME B_Costos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Mvto 


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
ON RETURN TAB.

ON TAB, RETURN, MOUSE-SELECT-DBLCLICK OF Tmp.W_Cuenta IN BROWSE Temp DO:
    IF Tmp.W_Cuenta:SCREEN-VALUE IN BROWSE Temp NE "" THEN
        RETURN.

    ASSIGN FRAME F_Mov:SENSITIVE = FALSE
           FRAME F_Cuenta:VISIBLE = TRUE.

    IF NUM-RESULTS("B_Cuentas") GT 0 THEN
        W_Metodo = B_Cuentas:SELECT-FOCUSED-ROW().

    APPLY "VALUE-CHANGED":U TO B_Cuentas.
    APPLY "ENTRY":U TO B_Cuentas IN FRAME F_Cuenta.
    RETURN NO-APPLY.
END.

ON LEAVE OF Tmp.W_Cuenta IN BROWSE Temp DO:
    IF Tmp.W_Cuenta:SCREEN-VALUE IN BROWSE Temp EQ "" OR NOT AVAILABLE Tmp THEN
        RETURN.

    ASSIGN INPUT BROWSE Temp W_Cuenta.

    FIND FIRST Cuentas WHERE Cuentas.Cuenta = Tmp.W_Cuenta
                         AND Cuentas.Estado = 1
                         AND Cuentas.Tipo = 2
                         AND Cuentas.Fec_Retiro = ?
                         AND Cuentas.Id_NoMvto EQ NO NO-LOCK NO-ERROR.

    /* Valida cuentas restringidas por agencia */
    IF AVAILABLE(cuentas) THEN DO:
        IF cuentas.id_base THEN DO:
            ASSIGN vcCuentaBase = "".

            FIND FIRST Base_Ret WHERE Base_Ret.Cod_Base EQ Cuentas.Cod_Base NO-ERROR.
            IF AVAILABLE(Base_Ret) THEN
                vcCuentaBase = Base_Ret.cuenta.
        END.
    END.
    /* --------------------------------------- */

    IF NOT AVAILABLE Cuentas OR Cuentas.Estado EQ 2 OR Cuentas.Fec_Retiro NE ? THEN DO:
        ASSIGN W_Cuenta:SCREEN-VALUE IN BROWSE Temp = "".

        MESSAGE "Cuenta Inexistente"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    RUN Cuenta-Encontrada.
END.

ON ENTRY OF Tmp.w_comentario IN BROWSE Temp DO:
    IF Tmp.W_Comentario:SCREEN-VALUE IN BROWSE Temp EQ "" THEN
        Tmp.W_Comentario:SCREEN-VALUE IN BROWSE Temp = W_ConAux.
END.

ON LEAVE OF Tmp.w_comentario IN BROWSE Temp DO:
    IF NOT AVAILABLE Tmp THEN
        RETURN.

    ASSIGN INPUT BROWSE Temp W_Comentario
           W_NomCom:SCREEN-VALUE IN FRAME F_Mov = Tmp.w_Comentario
           W_ConAux = W_Comentario.
END.

ON ANY-PRINTABLE OF Tmp.W_CCostos IN BROWSE Temp DO:
    IF NOT Cuentas.Id_CenCostos THEN DO:
        Tmp.W_CCostos:SCREEN-VALUE IN BROWSE Temp = "".
        RETURN NO-APPLY.
    END.
END.

ON ENTRY OF Tmp.W_CCostos IN BROWSE Temp DO:
    IF NOT Cuentas.Id_CenCostos THEN DO:
        APPLY KEYFUNCTION(LASTKEY) TO SELF.
        RETURN NO-APPLY.
    END.

    IF Tmp.W_CCostos:SCREEN-VALUE IN BROWSE Temp EQ "" THEN
        Tmp.W_CCostos:SCREEN-VALUE IN BROWSE Temp = STRING(W_CCoAux).
END.

ON TAB, RETURN, MOUSE-SELECT-DBLCLICK OF Tmp.W_Ccostos IN BROWSE Temp DO:
    IF Tmp.W_Ccostos:SCREEN-VALUE IN BROWSE Temp NE "" OR NOT Cuentas.Id_CenCostos THEN
        RETURN.

    ASSIGN FRAME F_Mov:SENSITIVE  = FALSE
           FRAME F_Costos:VISIBLE = TRUE.

    IF NUM-RESULTS("B_Costos") GT 0 THEN
        W_Metodo = B_Costos:SELECT-FOCUSED-ROW().

    APPLY "VALUE-CHANGED":U TO B_Costos.
    APPLY "ENTRY":U TO B_Costos IN FRAME F_Costos.
    RETURN NO-APPLY.
END.

ON LEAVE OF Tmp.W_Ccostos IN BROWSE Temp DO:
    IF NOT AVAILABLE Tmp THEN
        RETURN.

    IF NOT Cuentas.Id_CenCostos THEN
        RETURN.

    ASSIGN INPUT BROWSE Temp W_CCostos
           W_CCoAux = Tmp.W_CCostos.

    IF Tmp.W_CCostos EQ 0 THEN
        RETURN.

    FIND FIRST Cen_Costos WHERE Cen_Costos.Cen_Costos EQ Tmp.W_CCostos
                            AND Cen_Costos.Agencia EQ W_OfiOrigen
                            AND Cen_Costos.Estado NE 2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cen_Costos THEN DO:
        W_CCostos:SCREEN-VALUE IN BROWSE Temp = "".
        APPLY "RETURN":U TO SELF.
        RETURN NO-APPLY.
    END.

    ASSIGN W_NomCen:SCREEN-VALUE IN FRAME F_Mov = STRING(Cen_Costos.Nombre)
           Tmp.W_NomCcs = STRING(Cen_Costos.Nombre).
END.

ON ANY-PRINTABLE OF Tmp.W_Enlace IN BROWSE Temp DO:
    IF NOT Tmp.W_Id_Enlace THEN DO:
        Tmp.W_Enlace:SCREEN-VALUE IN BROWSE Temp = "".
        RETURN NO-APPLY.
    END.
END.

ON ENTRY OF Tmp.W_Enlace IN BROWSE Temp DO:
    IF NOT Tmp.W_Id_Enlace THEN DO:
        APPLY KEYFUNCTION(LASTKEY) TO SELF.
        RETURN NO-APPLY.
    END.
END.

ON LEAVE OF Tmp.W_Enlace IN BROWSE Temp DO:
    IF NOT Tmp.W_Id_Enlace OR NOT AVAILABLE Tmp THEN
        RETURN.

    DEFINE VARIABLE W_Sw AS LOGICAL.
    ASSIGN INPUT BROWSE Temp W_Enlace.
END.

ON TAB, RETURN, MOUSE-SELECT-DBLCLICK OF Tmp.W_Nit IN BROWSE Temp DO:
    DEFINE VAR cont AS INTEGER.

    IF Tmp.W_Nit:SCREEN-VALUE IN BROWSE Temp <> "" OR NOT Cuentas.Id_Nit THEN DO:
        w_saldo = 0.

        FOR EACH anexos WHERE anexos.agencia = w_agencia
                          AND anexos.cuenta = cuentas.cuenta
                          AND anexos.ano = YEAR(w_fecha)
                          AND anexos.nit = Tmp.W_Nit:SCREEN-VALUE IN BROWSE Temp NO-LOCK:
            w_saldo = w_saldo + anexos.sdo_inicial.

            DO cont = 1 TO MONTH(w_fecha):
                IF cuentas.naturaleza = "DB" THEN
                    w_saldo = w_saldo + anexos.db[cont] - anexos.cr[cont].
                ELSE
                    w_saldo = w_saldo + anexos.cr[cont] - anexos.db[cont].
            END.

            w_saldo:SCREEN-VALUE IN FRAME F_Mov = STRING(w_saldo).
        END.
        
        RETURN.
    END.

    FRAME F_Mov:SENSITIVE = FALSE.

    FIND FIRST Clientes WHERE Clientes.Nit = Tmp.W_Nit:SCREEN-VALUE IN BROWSE Temp
                          AND clientes.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) AND tmp.w_nit:SCREEN-VALUE <> "" THEN
        W_NomNit:SCREEN-VALUE IN FRAME F_Mov = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE DO:
        W-Mvto:SENSITIVE = FALSE.

        RUN C-Clientes.R(INPUT 1,
                         INPUT W_Agencia,
                         OUTPUT P_Nit,
                         OUTPUT P_Nombre,
                         OUTPUT P_Apellido,
                         OUTPUT P_AgeCli).

        ASSIGN W-Mvto:SENSITIVE = TRUE
               W_NomNit:SCREEN-VALUE IN FRAME F_Mov  = TRIM(P_Nombre) + " " + P_Apellido
               Tmp.W_Nit:SCREEN-VALUE IN BROWSE Temp = P_Nit.
    END.

    w_saldo = 0.

    FOR EACH anexos WHERE anexos.agencia = w_agencia
                      AND anexos.cuenta = cuentas.cuenta
                      AND anexos.ano = YEAR(w_fecha)
                      AND anexos.nit = Tmp.W_Nit:SCREEN-VALUE IN BROWSE Temp NO-LOCK:
        w_saldo = w_saldo + anexos.sdo_inicial.

        DO cont = 1 TO MONTH(w_fecha):
            IF cuentas.naturaleza = "DB" THEN
                w_saldo = w_saldo + anexos.db[cont] - anexos.cr[cont].
            ELSE
                w_saldo = w_saldo + anexos.cr[cont] - anexos.db[cont].
        END.

        w_saldo:SCREEN-VALUE IN FRAME F_Mov = STRING(w_saldo).
    END.

    FRAME F_Mov:SENSITIVE  = TRUE.
    RETURN NO-APPLY.
END.

ON ANY-PRINTABLE OF Tmp.W_Nit IN BROWSE Temp DO:
    IF NOT Cuentas.Id_Nit THEN DO:
        /*ASSIGN Tmp.W_Nit:SCREEN-VALUE IN BROWSE Temp = "".
        RETURN NO-APPLY.*/
    END.
END.

ON ENTRY OF Tmp.W_Nit IN BROWSE Temp DO:
     IF NOT Cuentas.Id_Nit THEN DO:
        /*APPLY KEYFUNCTION(LASTKEY) TO SELF.
        RETURN NO-APPLY.*/
     END.
       
     IF Tmp.W_Nit:SCREEN-VALUE IN BROWSE Temp EQ "" THEN
        ASSIGN Tmp.W_Nit:SCREEN-VALUE IN BROWSE Temp = STRING(W_NitAux).
  END.

ON LEAVE OF Tmp.W_Nit IN BROWSE Temp DO:
    DEFINE VAR flagValidaDocumento AS LOGICAL.
    DEFINE VAR caracterInvalido AS CHARACTER.

    IF Tmp.W_Nit:SCREEN-VALUE IN BROWSE Temp <> "" THEN DO:
        RUN validaDocumento.r(INPUT Tmp.W_Nit:SCREEN-VALUE,
                              OUTPUT flagValidaDocumento,
                              OUTPUT caracterInvalido).

        IF flagValidaDocumento = TRUE THEN DO:
            MESSAGE "El documento ingresado posee" SKIP
                    "caracteres inválidos..." STRING("(" + caracterInvalido + ")")
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN NO-APPLY.
        END.

        FIND FIRST clientes WHERE clientes.nit = tmp.w_nit:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            IF clientes.estado = 2 THEN DO:
                MESSAGE "El Tercero se encuentra inactivo."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN NO-APPLY.
            END.
        END.
    END.

    IF /*NOT Cuentas.Id_Nit OR*/ NOT AVAILABLE Tmp THEN
        RETURN.

    ASSIGN INPUT BROWSE Temp W_Nit
           W_NitAux = Tmp.W_Nit.

    IF Tmp.W_Nit EQ "" AND cuentas.id_nit = TRUE THEN DO:
        APPLY "RETURN":U TO SELF.
        RETURN NO-APPLY.
    END.

    IF (Tmp.W_Cuenta BEGINS "1904" OR Tmp.W_Cuenta BEGINS "2705") AND LENGTH(Tmp.W_Nit) <> 4 AND DECIMAL(Tmp.W_Nit) > 14 THEN DO:
        MESSAGE "Esta es una cuenta de sucursales y agencias" SKIP
                "El nit a digitarse es el de la agencia destino" SKIP
                "con la cual se hará el cruce de cuentas." SKIP(1)
                "Digite de nuevo el nit!" VIEW-AS ALERT-BOX INFORMATION.
        APPLY "RETURN":U TO SELF.
        RETURN NO-APPLY.
    END.

    FIND FIRST Clientes WHERE Clientes.Nit EQ W_Nit:SCREEN-VALUE IN BROWSE Temp NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Clientes) THEN DO:
        W_Nit:SCREEN-VALUE IN BROWSE Temp = "".
        APPLY "RETURN":U TO SELF.
        RETURN NO-APPLY.
    END.

    ASSIGN W_NomNit:SCREEN-VALUE IN FRAME F_Mov = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
           Tmp.W_NomCli = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
END.

ON ENTRY OF Tmp.W_Documento IN BROWSE Temp DO:
     IF NOT Cuentas.Id_Doc THEN DO:
        APPLY KEYFUNCTION(LASTKEY) TO SELF.
        RETURN NO-APPLY.
     END.      
     
     IF Tmp.W_Documento:SCREEN-VALUE IN BROWSE Temp EQ "" THEN 
        ASSIGN Tmp.W_Documento:SCREEN-VALUE IN BROWSE Temp = W_DocAux.
  END.     
/*****/
  ON ANY-PRINTABLE OF Tmp.W_Documento IN BROWSE Temp DO:
     IF NOT Cuentas.Id_Doc THEN DO:
        ASSIGN Tmp.W_Documento:SCREEN-VALUE IN BROWSE Temp = "".
        RETURN NO-APPLY.
     END.
  END.

/*****/
  ON LEAVE OF Tmp.W_Documento IN BROWSE Temp DO:
     IF NOT Cuentas.Id_Doc
     OR NOT AVAILABLE Tmp THEN
        RETURN.
     ASSIGN INPUT BROWSE Temp W_Documento
            W_DocAux  = Tmp.W_Documento.
     IF W_Id_Det THEN 
        RUN Llenar_Detalle.

     IF Tmp.W_Documento EQ ""  THEN
        RETURN NO-APPLY.
  END.    


  /******/
  ON ENTRY OF Tmp.W_Debito IN BROWSE Temp DO:
     ASSIGN W_Aux = DECIMAL(Tmp.W_Debito:SCREEN-VALUE IN BROWSE Temp)
            FD_Debito = FD_Debito - DECIMAL(Tmp.W_Debito:SCREEN-VALUE IN BROWSE Temp).

     IF Tmp.W_Ctr_Natur = TRUE
     AND Tmp.W_Naturaleza NE "DB" THEN DO:
         APPLY KEYFUNCTION(LASTKEY) TO SELF.
         RETURN NO-APPLY.
     END.
  END.
/*****/
ON SELECTION, GO, MOUSE-SELECT-CLICK OF Tmp.W_Debito IN BROWSE Temp DO:
    W_Aux = DECIMAL(Tmp.W_Debito:SCREEN-VALUE IN BROWSE Temp).
END.

ON ANY-PRINTABLE OF Tmp.W_Debito IN BROWSE Temp DO:
     IF  Tmp.W_Ctr_Natur = TRUE
     AND Tmp.W_Naturaleza EQ "CR" THEN DO:
        ASSIGN Tmp.W_Debito:SCREEN-VALUE IN BROWSE Temp = STRING(0).
        RETURN NO-APPLY.
     END.
  END.

/*****/
ON TAB, RETURN OF Tmp.W_Debito IN BROWSE Temp DO:
    IF Tmp.W_Ctr_Natur = TRUE AND Tmp.W_Naturaleza EQ "DB" AND Tmp.W_Debito:SCREEN-VALUE IN BROWSE Temp EQ STRING(0) THEN
        RETURN NO-APPLY.
END.
/*****/

ON LEAVE OF Tmp.W_Debito IN BROWSE Temp DO:
    IF NOT AVAILABLE Tmp THEN
        RETURN.

    W_totDeb = W_totDeb - W_Aux.

    ASSIGN Tmp.W_Debito = DECIMAL(W_Debito:SCREEN-VALUE IN BROWSE Temp)
           Tmp.W_Credito = DECIMAL(W_Credito:SCREEN-VALUE IN BROWSE TEMP)
           INPUT FRAME F_Mov W_TotCre
           INPUT FRAME F_Mov W_TotDeb.

    IF Tmp.W_Debito NE 0 THEN DO:
        ASSIGN W_TotCre = W_TotCre - Tmp.W_Credito
               Tmp.W_Credito:SCREEN-VALUE IN BROWSE Temp = STRING(0)
               INPUT BROWSE Temp W_Credito
               W_TotCre:SCREEN-VALUE IN FRAME F_Mov = STRING(W_TotCre)
               W_Valor:SCREEN-VALUE = "".

        IF Tmp.W_Id_Base THEN DO:
            IF Tmp.W_PorBas NE 0 THEN
                Tmp.W_Base = (Tmp.W_Debito * 100) / Tmp.W_PorBas.
            ELSE
                Tmp.W_Base = 0.

            W_Valor:SCREEN-VALUE IN FRAME F_Mov = STRING(Tmp.W_Base).
        END.
    END.

    ASSIGN W_TotDeb = IF (W_Totdeb - W_Aux + Tmp.W_Debito) GT 0 THEN W_Totdeb - W_Aux + Tmp.W_Debito
                      ELSE 0
           W_DifDeb = IF W_TotDeb LT W_TotCre THEN W_TotCre - W_TotDeb ELSE 0
           W_DifCre = IF W_TotDeb GT W_TotCre THEN W_TotDeb - W_TotCre ELSE 0
           W_Aux = W_Debito.

    ASSIGN W_TotDeb:SCREEN-VALUE = STRING(W_TotDeb)
           W_DifDeb:SCREEN-VALUE = STRING(W_DifDeb)
           W_DifCre:SCREEN-VALUE = STRING(W_DifCre).

    IF Tmp.W_Id_Det THEN DO:
        IF NOT W_Ok AND Tmp.W_Debito NE 0 THEN DO:
            ASSIGN W_DC = "D"
                   W_Ok = YES.

            IF NOT W_Existe THEN
                ASSIGN FD_ValInicial = Tmp.W_Debito
                       FD_Debito = Tmp.W_Debito.
            ELSE
                FD_Debito = Detalle.Db + Tmp.W_debito.

            RUN SaldoFinal_Detalle.

            /*IF W_Existe THEN DO:
                MESSAGE "Desea ver la actualizacion del Detalle?"
                    VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE choice AS LOGICAL.

                IF choice THEN DO:
                    VIEW FRAME F_Detalle.
                    APPLY "ENTRY" TO Bt_SalirDetalle IN FRAME F_Detalle.
                    RETURN NO-APPLY.
                END.
            END.
            ELSE DO:
                VIEW FRAME F_Detalle.
                APPLY "ENTRY" TO FD_ValAmortizacion IN FRAME F_Detalle.
                RETURN NO-APPLY.
            END.*/
        END.
        ELSE
            ASSIGN FD_ValAmortizacion = 0
                   FD_Plazo = 0
                   W_Ok = NO.
    END.
END.
/*****/
  ON ANY-PRINTABLE OF Tmp.W_Credito IN BROWSE Temp DO:
     IF  Tmp.W_Ctr_Natur = TRUE
     AND Tmp.W_Naturaleza EQ "DB" THEN DO:
        ASSIGN Tmp.W_Credito:SCREEN-VALUE IN BROWSE Temp = STRING(0).
        RETURN NO-APPLY.
     END.
  END.

/*****/
  ON TAB, RETURN OF Tmp.W_Credito IN BROWSE Temp DO:
     IF  Tmp.W_Ctr_Natur = TRUE
     AND Tmp.W_Naturaleza EQ "CR"
     AND Tmp.W_Credito:SCREEN-VALUE IN BROWSE Temp EQ STRING(0) THEN
         RETURN NO-APPLY.  
  END.
/*****/
  ON ENTRY OF Tmp.W_Credito IN BROWSE Temp DO:
        ASSIGN W_Aux = DECIMAL(Tmp.W_Credito:SCREEN-VALUE IN BROWSE Temp)
               FD_Credito = FD_Credito - DECIMAL(Tmp.W_Credito:SCREEN-VALUE IN BROWSE Temp).
        Sw_SelfCr = FALSE.
        IF  Tmp.W_Ctr_Natur = TRUE AND Tmp.W_Naturaleza NE "CR" 
                                    OR Tmp.W_Debito NE 0 THEN DO:
            Sw_SelfCr = TRUE.
            APPLY KEYFUNCTION(LASTKEY) TO SELF.
            RETURN NO-APPLY.
        END.
  END. 
  
/****/
ON LEAVE OF Tmp.W_Credito IN BROWSE Temp DO:
    IF NOT AVAILABLE Tmp THEN
        RETURN.

    ASSIGN Tmp.W_Debito = DECIMAL(W_Debito:SCREEN-VALUE IN BROWSE Temp)
           Tmp.W_Credito = DECIMAL(W_Credito:SCREEN-VALUE IN BROWSE Temp)
           INPUT FRAME F_Mov W_TotCre
           INPUT FRAME F_Mov W_TotDeb.

    IF Tmp.W_Credito NE 0 THEN DO:
        ASSIGN W_TotDeb = W_TotDeb - Tmp.W_Debito
               W_Debito:SCREEN-VALUE IN BROWSE Temp = STRING(0)
               INPUT BROWSE Temp W_Debito
               W_TotDeb:SCREEN-VALUE IN FRAME F_Mov = STRING(W_TotDeb)
               W_Valor:SCREEN-VALUE = "".

        IF Tmp.W_Id_Base THEN DO:
            IF Tmp.W_PorBas NE 0 THEN
                Tmp.W_Base = (Tmp.W_Credito * 100) / Tmp.W_PorBas.
            ELSE
                Tmp.W_PorBas.

            W_Valor:SCREEN-VALUE IN FRAME F_Mov = STRING(Tmp.W_Base).
        END.
    END.

    ASSIGN W_TotCre = IF (W_TotCre - W_Aux + Tmp.W_Credito) GT 0 THEN W_TotCre - W_Aux + Tmp.W_Credito
                      ELSE 0
           W_DifDeb = IF W_TotDeb LT W_TotCre THEN W_TotCre - W_TotDeb ELSE 0
           W_DifCre = IF W_TotDeb GT W_TotCre THEN W_TotDeb - W_TotCre ELSE 0
           W_Aux = W_Credito.

    ASSIGN W_TotCre:SCREEN-VALUE = STRING(W_TotCre)
           W_DifDeb:SCREEN-VALUE = STRING(W_DifDeb)
           W_DifCre:SCREEN-VALUE = STRING(W_DifCre).

    IF LASTKEY EQ KEYCODE(KBLABEL("TAB")) OR LASTKEY EQ KEYCODE(KBLABEL("RETURN")) THEN DO:
        IF Tmp.W_Cuenta:SCREEN-VALUE IN BROWSE Temp = "" OR (W_Debito:SCREEN-VALUE IN BROWSE Temp = "0" AND W_Credito:SCREEN-VALUE IN BROWSE Temp = "0") THEN
            RETURN NO-APPLY.
        ELSE
            IF Tmp.W_Id_Det THEN DO:
                IF NOT W_Ok AND Tmp.W_Credito NE 0 THEN DO:
                    ASSIGN W_Ok = YES
                           W_DC = "C".

                    IF NOT W_Existe THEN
                        ASSIGN FD_ValInicial = Tmp.W_Credito
                               FD_Credito = Tmp.W_Credito.
                    ELSE
                        FD_Credito = Detalle.Cr + Tmp.W_Credito.

                    RUN SaldoFinal_Detalle.

                    /*IF W_Existe THEN DO:
                        MESSAGE "Desea ver la actualizacion del Detalle?"
                            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE choice AS LOGICAL.

                        IF choice THEN DO:
                            VIEW FRAME F_Detalle.
                            APPLY "ENTRY" TO Bt_SalirDetalle IN FRAME F_Detalle.
                            RETURN NO-APPLY.
                        END.
                    END.
                    ELSE DO:
                        VIEW FRAME F_Detalle.
                        APPLY "ENTRY" TO FD_ValAmortizacion IN FRAME F_Detalle.
                        RETURN NO-APPLY.
                    END.*/
                END.
                ELSE
                    ASSIGN FD_ValAmortizacion = 0
                           FD_Plazo = 0
                           W_Ok = NO.
            END.

        GET NEXT TEMP.
        
        IF NOT AVAILABLE Tmp THEN DO:
            GET PREV TEMP.
            APPLY "CTRL-INS" TO BROWSE TEMP.
            APPLY "ENTRY" TO Tmp.W_Cuenta IN BROWSE Temp.
            RETURN NO-APPLY.
        END.
            
        GET PREV TEMP.
    END.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  RUN Inicializar-Variables.
  RUN Verificar-Mes.
  ASSIGN W_Secuencia     = INTEGER(SUBSTRING(W_Comprobante,29,1))
         W_CompAct       = INTEGER(SUBSTRING(W_Comprobante, 1, 2))
         W_ScreenCbte    = W_Comprobante:SCREEN-VALUE IN FRAME F_Mov
         W_AuxCbteScreen = W_CompAct.
         
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
       WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Activar W-Mvto 
PROCEDURE Activar :
RUN Cancelar.
  ASSIGN BROWSE Temp:READ-ONLY                  = FALSE
         W_OfDestino:SENSITIVE  IN FRAME F_Mov  = TRUE
         W_Comprobante:SENSITIVE IN FRAME F_Mov = TRUE
         Btn_Grabar:SENSITIVE                   = TRUE
         Btn_Cancelar:SENSITIVE                 = TRUE
         Btn_Salir:SENSITIVE                    = TRUE
         Btn_Consulta:SENSITIVE                 = TRUE
         Btn_Imprimir:SENSITIVE                 = TRUE
         W_CmbOfip:SENSITIVE                    = TRUE
         W_CpteP:VISIBLE IN FRAME F_Mov         = FALSE
         W_AgenciaP:VISIBLE IN FRAME F_Mov      = FALSE
         Fec_Consulta:HIDDEN IN FRAME F_Mov     = TRUE
         FRAME F_Mov:SENSITIVE                  = TRUE .
  IF SUBSTRING(W_Comprobante:SCREEN-VALUE IN FRAME F_Mov, 29,1) = "2" THEN
     ASSIGN W_Doc:SENSITIVE IN FRAME F_Mov = FALSE.
  ELSE
     ASSIGN W_Doc:SENSITIVE IN FRAME F_Mov = TRUE.
  ASSIGN Fec_Consulta:HIDDEN IN FRAME F_Mov = TRUE.
  APPLY "ENTRY" TO W_Comprobante IN FRAME F_Mov.
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActivosFijos W-Mvto 
PROCEDURE ActivosFijos :
DEFINE INPUT PARAMETER vDB AS DECIMAL.
DEFINE INPUT PARAMETER vCR AS DECIMAL.
DEFINE INPUT PARAMETER vCuenta AS CHARACTER.
DEFINE INPUT PARAMETER numDoc AS INTEGER.

FIND FIRST activosFijos WHERE activosFijos.id = mov_contable.nit NO-ERROR.
IF AVAILABLE activosFijos THEN DO:
    IF SUBSTRING(mov_contable.cuenta,1,4) <> "1795" THEN DO:
        /*activosFijos.valorCompra = activosFijos.valorCompra + vDb - vCr.*/
        activosFijos.valorActual = activosFijos.valorActual + vDB - vCR.

        IF activosFijos.valorActual < 0 THEN
            activosFijos.valorActual = 0.

        IF activosFijos.valorActual = 0 THEN DO:
            MESSAGE "El nuevo valor para el activo fijo es de $0..." SKIP
                    "Desea dar de baja el Activo...?" SKIP
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Baja de Activo" UPDATE ynDarDeBaja AS LOGICAL.

            IF ynDarDeBaja = YES THEN
                activosFijos.estado = 2.
        END.
    END.
    ELSE DO:
        /*activosFijos.valorDepreciado = activosFijos.valorDepreciado + vDb - vCr.*/
        activosFijos.valorDepreciado = activosFijos.valorDepreciado - vDb + vCr. /* Por el manejo de la naturaleza */
    END.

    FIND FIRST anotaActivosFijos WHERE anotaActivosFijos.id = activosFijos.idActivo AND anotaActivosFijos.numDoc = W_Doc NO-ERROR.
    IF NOT AVAILABLE anotaActivosFijos THEN DO:
        CREATE anotaActivosFijos.
        anotaActivosFijos.id = activosFijos.idActivo.
        anotaActivosFijos.numDoc = W_Doc.
    END.

    activosFijos.contabilizado = YES.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cancelar W-Mvto 
PROCEDURE Cancelar :
/* deshace la transaccion en pantalla */
  FOR EACH Tmp:
      DELETE Tmp.
  END.
  ASSIGN  W_TotDeb:SCREEN-VALUE IN FRAME F_Mov = "0.00"
          W_TotCre:SCREEN-VALUE IN FRAME F_Mov = "0.00"
          W_DifDeb:SCREEN-VALUE IN FRAME F_Mov = "0.00"
          W_DifCre:SCREEN-VALUE IN FRAME F_Mov = "0.00"
          W_TotCre = 0
          W_TotDeb = 0
          W_DifCre = 0
          W_DifDeb = 0
          W_Nombre:SCREEN-VALUE IN FRAME F_Mov = ""
          W_Saldo:SCREEN-VALUE IN FRAME F_Mov = ""
          W_NomCom:SCREEN-VALUE IN FRAME F_Mov = ""
          W_NomNit:SCREEN-VALUE IN FRAME F_Mov = ""
          W_NomCen:SCREEN-VALUE IN FRAME F_Mov = ""
          W_Valor:SCREEN-VALUE  IN FRAME F_Mov = "".
  IF W_Doc:SENSITIVE IN FRAME F_Mov THEN
     W_Doc:SCREEN-VALUE IN FRAME F_Mov = "0000000".
  IF NUM-RESULTS("Temp") GT 0 THEN
     CLOSE QUERY Temp.
  {&OPEN-QUERY-Temp}
  ASSIGN FRAME F_Consulta:HIDDEN = TRUE
         FRAME F_Costos:HIDDEN   = TRUE
         FRAME F_Cuenta:HIDDEN   = TRUE
         FRAME F_Imprimir:HIDDEN = TRUE.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChequeoTotal W-Mvto 
PROCEDURE ChequeoTotal :
w_totDeb = 0.
w_totCre = 0.

FOR EACH Tmp WHERE TRIM(Tmp.w_Cuenta) <> "" BY Tmp.W_Fila:
    W_TotDeb = W_TotDeb + W_Debito.
    W_TotCre = W_TotCre + W_Credito.
END.

W_DifDeb = IF W_TotDeb < W_TotCre THEN W_TotCre - W_TotDeb ELSE 0.
W_DifCre = IF W_TotDeb > W_TotCre THEN W_TotDeb - W_TotCre ELSE 0.

DISPLAY W_TotDeb
        W_TotCre
        W_DifDeb
        W_DifCre
    WITH FRAME F_Mov.

IF W_TotDeb <> W_TotCre THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 14,
                                    OUTPUT W_Eleccion).

    APPLY "ENTRY" TO Tmp.W_Cuenta IN BROWSE Temp.
    RETURN ERROR.
END.

/* oakley */

FOR EACH Tmp WHERE TRIM(Tmp.w_Cuenta) <> "" BY Tmp.W_Fila:
    IF (Tmp.W_Debito = 0 AND Tmp.W_Credito <> 0) OR (Tmp.W_Debito <> 0 AND Tmp.W_Credito = 0) THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = Tmp.W_Cuenta AND Cuentas.Estado = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Cuentas) THEN DO:
            RUN MostrarMensaje IN W_Manija (INPUT 66,
                                            OUTPUT W_Eleccion).

            REPOSITION TEMP TO ROWID ROWID(Tmp).

            RETURN ERROR.
        END.

        IF Cuentas.Id_CenCostos = TRUE THEN DO:
            FIND FIRST Cen_Costos WHERE Cen_Costos.Cen_Costos = Tmp.W_CCostos
                                    AND Cen_Costos.Estado = 1
                                    AND Cen_Costos.Agencia = W_OfiOrigen NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(Cen_Costos) THEN DO:
                IF Tmp.W_CCostos <> W_CenCosGral THEN DO:
                    RUN MostrarMensaje IN W_Manija (INPUT 70,
                                                    OUTPUT W_Eleccion).

                    REPOSITION TEMP TO ROWID ROWID(Tmp).

                    RETURN ERROR.
                END.
            END.
        END.

        IF Cuentas.Id_Nit = TRUE THEN DO:
            FIND FIRST Clientes WHERE Clientes.Nit = Tmp.W_Nit AND clientes.nit <> "" NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(Clientes) THEN DO:
                RUN MostrarMensaje IN W_Manija (INPUT 320,
                                                OUTPUT W_Eleccion).

                REPOSITION TEMP TO ROWID ROWID(Tmp).

                RETURN ERROR.
            END.
        END.

        IF Cuentas.Id_Doc = TRUE THEN DO:
            IF Tmp.W_Documento = "" THEN DO:
                RUN MostrarMensaje IN W_Manija (INPUT 337,
                                                OUTPUT W_Eleccion).

                REPOSITION TEMP TO ROWID ROWID(Tmp).

                RETURN ERROR.
            END.
        END.
    END.

    /* Para Activos Fijos */
    IF comprobantes.comprobante = 10 AND SUBSTRING(Tmp.w_Cuenta,1,2) = "17" AND SUBSTRING(Tmp.w_Cuenta,1,6) <> "170595" THEN DO:
        FIND FIRST activosFijos WHERE activosFijos.idActivo = Tmp.W_Nit
                                  AND activosFijos.estado = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE activosFijos THEN DO:
            MESSAGE "El Activo Fijo que intenta modificar ("  + tmp.W_nit + ") no existe o ya fue" SKIP
                    "dado de baja. La operación será cancelada"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN ERROR.
        END.
        ELSE DO:
            IF activosFijos.agencia <> w_agencia THEN DO:
                MESSAGE "El activo Fijo pertenece a una agencia diferente." SKIP
                        "Debe ingresar con la agencia del Activo Fijo para" SKIP
                        "poder realizar cualquier modificación."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.

            IF activosFijos.cen_costos <> tmp.w_ccostos THEN DO:
                MESSAGE "El activo Fijo pertenece a un centro de costos" SKIP
                        "diferente.  Debe  ingresar  por  el  módulo de" SKIP
                        "Activos  Fijos  y realizar el cambio de centro" SKIP
                        "de costos antes de hacer cualquier modificación."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.

            FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = activosFijos.tipoActivo NO-LOCK NO-ERROR.
            IF cfg_activosFijos.activoFijo <> Tmp.w_Cuenta THEN DO:
                MESSAGE "Este Activo no está clasificado dentro del tipo (cuenta contable) que se está" SKIP
                        "moviendo. Por favor verifíquela."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.
            ELSE DO:
                IF activosFijos.contabilizado = NO THEN DO:
                    FIND FIRST ttActivos WHERE ttActivos.id = Tmp.W_Nit NO-ERROR.
                    IF NOT AVAILABLE ttActivos THEN DO:
                        CREATE ttActivos.
                        ttActivos.id = tmp.W_nit.
                    END.

                    ttActivos.valor = ttActivos.valor + tmp.w_debito - tmp.w_credito.
                END.
                ELSE DO:
                    IF activosFijos.valorCompra + tmp.w_debito - tmp.w_credito < 0 THEN DO:
                        MESSAGE "Usted está intentando sobregirar un Activo Fijo." SKIP
                                "Operación rechazada..."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.

                        RETURN ERROR.
                    END.
                END.
            END.
        END.
    END.
    ELSE DO:
        IF comprobantes.comprobante <> 10 AND SUBSTRING(Tmp.w_Cuenta,1,2) = "17" THEN DO:
            MESSAGE "Usted  está  utilizando  una  cuenta de" SKIP
                    "Activos Fijos. Estas cuentas únicamente" SKIP
                    "pueden utilizarse con el comprobante de" SKIP
                    "Activos Fijos. Revise por favor..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                
            RETURN ERROR.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Consulta-Documento W-Mvto 
PROCEDURE Consulta-Documento :
DEFINE VAR W_TDb AS DECIMAL.
DEFINE VAR W_TCr AS DECIMAL.
DEFINE VAR W_NomUsuario AS CHARACTER FORMAT "X(50)".
DEFINE VAR W_Linea AS CHARACTER FORMAT "X(100)".
DEFINE VAR fechaAux AS DATE.
DEFINE VAR flagReversar AS LOGICAL INITIAL TRUE.

W_NomUsuario = "Usuario        : " + mov_contable.usuario.

W_oK = E_DocEncabezado:ADD-LAST("Agencia        : " + STRING(W_OfiCon:SCREEN-VALUE IN FRAME F_Consulta)) IN FRAME F_Consulta.
W_oK = E_DocEncabezado:ADD-LAST("Comprobante    : " + STRING(W_TipCon:SCREEN-VALUE IN FRAME F_Consulta)).
W_ok = E_DocEncabezado:ADD-LAST("Fecha Contable : " + STRING(Mov_Contable.Fec_Contable) + " Fecha de Grabacion : " + STRING(Mov_Contable.Fec_Grabacion)).
W_Ok = E_DocEncabezado:ADD-LAST(W_NomUsuario).
W_Ok = E_DocConsulta:ADD-LAST("Cuenta         Nombre               Comentario           C.C Nit        DocRef      Debito     Credito") IN FRAME F_Consulta.

/* oakley */

EMPTY TEMP-TABLE reversar.

btnReversar:SENSITIVE = TRUE.

/* mov_contable */
FOR EACH Mov_Contable WHERE Mov_Contable.Agencia EQ INTEGER(SUBSTRING(W_OfiCon:SCREEN-VALUE IN FRAME F_Consulta,1,3))
                        AND Mov_Contable.Comprobante EQ INTEGER(SUBSTRING(W_TipCon:SCREEN-VALUE IN FRAME F_Consulta,1,2))
                        AND Mov_Contable.Num_Documento EQ W_NumCon
                        AND mov_contable.fec_contable = fecDocumento NO-LOCK BREAK BY mov_contable.fec_contable DESCENDING
                                                                                   BY ROWID(Mov_Contable):
    CREATE reversar.
    BUFFER-COPY mov_contable TO reversar.
    
    FIND FIRST Cuentas WHERE Cuentas.Cuenta = Mov_Contable.Cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE (Cuentas) THEN DO:
        W_Linea = W_Linea +
                  STRING(Mov_Contable.Cuenta,"X(14)") + " " +
                  STRING(Cuentas.Nombre,"X(20)") + " " +
                  STRING(Mov_Contable.Comentario,"X(20)") + " " +
                  STRING(Mov_Contable.Cen_Costos,"999") +  " " +
                  STRING(Mov_Contable.Nit,"X(10)") + " " +
                  STRING(Mov_Contable.Doc_Referencia,"999999") + " " +
                  STRING(Mov_Contable.Db,">>>>>>>>>>9") + " " +
                  STRING(Mov_Contable.Cr,">>>>>>>>>>9").

        IF cuentas.Id_NoMvto = TRUE THEN DO:
            btnReversar:SENSITIVE = FALSE.

            FIND FIRST movProductos WHERE movProductos.agencia = mov_contable.agencia
                                      AND movProductos.comprobante = mov_contable.comprobante
                                      AND movProductos.estado = 1
                                      AND movProductos.fecha = mov_contable.fec_contable
                                      AND movProductos.nit = mov_contable.nit
                                      AND movProductos.num_documento = mov_contable.num_documento NO-LOCK NO-ERROR.
            IF AVAILABLE movProductos THEN DO:
                IF movProductos.tipo_producto = 1 THEN DO:
                    FIND FIRST ahorros WHERE ahorros.agencia = movProductos.agencia
                                         AND ahorros.nit = movProductos.nit
                                         AND ahorros.cue_ahorros = movProductos.id_producto
                                         AND ahorros.estado = 1 NO-ERROR.
                    IF AVAILABLE ahorros THEN
                        btnReversar:SENSITIVE = TRUE.
                    ELSE DO:
                        MESSAGE "El producto de ahorro #" movProductos.id_producto "del Asociado con" SKIP
                                "documento de identidad #" movProductos.nit "no se encuentra activo." SKIP
                                "No se permite la reversión del documento."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.

                        flagReversar = FALSE.
                    END.
                END.
                ELSE DO:
                    FIND FIRST creditos WHERE creditos.agencia = movProductos.agencia
                                          AND creditos.nit = movProductos.nit
                                          AND creditos.num_credito = INTEGER(movProductos.id_producto)
                                          AND creditos.estado = 2 NO-ERROR.
                    IF AVAILABLE creditos THEN
                        btnReversar:SENSITIVE = TRUE.
                    ELSE DO:
                        MESSAGE "El producto de crédito #" movProductos.id_producto "del Asociado con" SKIP
                                "documento de identidad #" movProductos.nit "no se encuentra activo." SKIP
                                "No se permite la reversión del documento."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.

                        flagReversar = FALSE.
                    END.
                END.
            END.
        END.
    END.

    ASSIGN W_Tdb = W_TDb + Mov_Contable.Db
           W_TCr = W_TCr + Mov_Contable.Cr.

    W_Ok = E_DocConsulta:ADD-LAST(W_Linea).
    W_Linea = "".

    IF LAST-OF(mov_contable.fec_contable) THEN DO:
        fechaAux = mov_contable.fec_contable.
        LEAVE.
    END.
END.

/* mov_contable2 */
FOR EACH Mov_Contable2 WHERE Mov_Contable2.Agencia = INTEGER(SUBSTRING(W_OfiCon:SCREEN-VALUE IN FRAME F_Consulta,1,3))
                         AND Mov_Contable2.Comprobante = INTEGER(SUBSTRING(W_TipCon:SCREEN-VALUE IN FRAME F_Consulta,1,2))
                         AND mov_contable2.fec_contable = fecDocumento
                         AND Mov_Contable2.Num_Documento = W_NumCon NO-LOCK BREAK BY mov_contable2.fec_contable DESCENDING
                                                                                 BY ROWID(Mov_Contable2):
    CREATE reversar.
    BUFFER-COPY mov_contable2 TO reversar.

    FIND FIRST Cuentas WHERE Cuentas.Cuenta = Mov_Contable2.Cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE (Cuentas) THEN DO:
        W_Linea = W_Linea +
                  STRING(Mov_Contable2.Cuenta,"X(14)") + " " +
                  STRING(Cuentas.Nombre,"X(20)") + " " +
                  STRING(Mov_Contable2.Comentario,"X(20)") + " " +
                  STRING(Mov_Contable2.Cen_Costos,"999") +  " " +
                  STRING(Mov_Contable2.cliente_id,"X(10)") + " " +
                  STRING(Mov_Contable2.Doc_Referencia,"999999") + " " +
                  STRING(Mov_Contable2.Db,">>>>>>>>>>9") + " " +
                  STRING(Mov_Contable2.Cr,">>>>>>>>>>9").

        IF cuentas.Id_NoMvto = TRUE THEN DO:
            btnReversar:SENSITIVE = FALSE.

            FIND FIRST movProductos WHERE movProductos.agencia = mov_contable2.agencia
                                      AND movProductos.comprobante = mov_contable2.comprobante
                                      AND movProductos.estado = 1
                                      AND movProductos.fecha = mov_contable2.fec_contable
                                      AND movProductos.nit = mov_contable2.cliente_id
                                      AND movProductos.num_documento = mov_contable2.num_documento NO-LOCK NO-ERROR.
            IF AVAILABLE movProductos THEN DO:
                IF movProductos.tipo_producto = 1 THEN DO:
                    FIND FIRST ahorros WHERE ahorros.agencia = movProductos.agencia
                                         AND ahorros.nit = movProductos.nit
                                         AND ahorros.cue_ahorros = movProductos.id_producto
                                         AND ahorros.estado = 1 NO-ERROR.
                    IF AVAILABLE ahorros THEN
                        btnReversar:SENSITIVE = TRUE.
                    ELSE DO:
                        MESSAGE "El producto de ahorro #" movProductos.id_producto "del Asociado con" SKIP
                                "documento de identidad #" movProductos.nit "no se encuentra activo." SKIP
                                "No se permite la reversión del documento."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.

                        flagReversar = FALSE.
                    END.
                END.
                ELSE DO:
                    FIND FIRST creditos WHERE creditos.agencia = movProductos.agencia
                                          AND creditos.nit = movProductos.nit
                                          AND creditos.num_credito = INTEGER(movProductos.id_producto)
                                          AND creditos.estado = 2 NO-ERROR.
                    IF AVAILABLE creditos THEN
                        btnReversar:SENSITIVE = TRUE.
                    ELSE DO:
                        MESSAGE "El producto de crédito #" movProductos.id_producto "del Asociado con" SKIP
                                "documento de identidad #" movProductos.nit "no se encuentra activo." SKIP
                                "No se permite la reversión del documento."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.

                        flagReversar = FALSE.
                    END.
                END.
            END.
        END.
    END.

    ASSIGN W_Tdb = W_TDb + Mov_Contable2.Db
           W_TCr = W_TCr + Mov_Contable2.Cr.

    W_Ok = E_DocConsulta:ADD-LAST(W_Linea).
    W_Linea = "".

    IF LAST-OF(mov_contable2.fec_contable) THEN DO:
        fechaAux = mov_contable2.fec_contable.
        LEAVE.
    END.
END.

IF flagReversar = FALSE THEN
    btnReversar:SENSITIVE = FALSE.

IF INTEGER(SUBSTRING(W_TipCon:SCREEN-VALUE IN FRAME F_Consulta,1,2)) = 6 THEN DO:
    /* mov_contable */
    FOR EACH Mov_Contable WHERE Mov_Contable.Agencia <> INTEGER(SUBSTRING(W_OfiCon:SCREEN-VALUE IN FRAME F_Consulta,1,3))
                            AND Mov_Contable.Comprobante EQ INTEGER(SUBSTRING(W_TipCon:SCREEN-VALUE IN FRAME F_Consulta,1,2))
                            AND Mov_Contable.Num_Documento EQ W_NumCon
                            AND mov_contable.fec_Contable = fechaAux NO-LOCK BY ROWID(Mov_Contable):
        CREATE reversar.
        BUFFER-COPY mov_contable TO reversar.
    END.

    /* mov_contable2 */
    FOR EACH Mov_Contable2 WHERE Mov_Contable2.Agencia <> INTEGER(SUBSTRING(W_OfiCon:SCREEN-VALUE IN FRAME F_Consulta,1,3))
                             AND Mov_Contable2.Comprobante = INTEGER(SUBSTRING(W_TipCon:SCREEN-VALUE IN FRAME F_Consulta,1,2))
                             AND mov_contable2.fec_Contable = fechaAux
                             AND Mov_Contable2.Num_Documento = W_NumCon NO-LOCK BY ROWID(Mov_Contable2):
        CREATE reversar.
        BUFFER-COPY mov_contable2 TO reversar.
    END.
END.

W_Ok = E_DocConsulta:ADD-LAST("                                                                               -----------------------").
W_Ok = E_DocConsulta:ADD-LAST("                                                                  totales:     " + STRING(W_TDb,">>>>>>>>>>9") + " " + STRING(W_TcR,">>>>>>>>>>9")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crearMovAhorros W-Mvto 
PROCEDURE crearMovAhorros :
CREATE mov_ahorros.
ASSIGN Mov_Ahorros.Agencia = movProductos.agencia
       Mov_Ahorros.Age_Destino = movProductos.agencia
       Mov_Ahorros.Age_Fuente = movProductos.agencia
       Mov_Ahorros.Cod_Ahorro = ahorros.cod_ahorro
       Mov_Ahorros.Cpte = movProductos.comprobante
       Mov_Ahorros.Cue_Ahorros = ahorros.cue_ahorros
       Mov_Ahorros.Descrip = "Reversión"
       Mov_Ahorros.Fecha = w_fecha
       Mov_Ahorros.Hora = TIME
       Mov_Ahorros.Nit = movProductos.nit
       Mov_Ahorros.Nro_Auditoria = STRING(movProductos.num_documento)
       Mov_Ahorros.Num_Documento = STRING(secuenciaRev)
       Mov_Ahorros.Sdo_Disponible = ahorros.sdo_disponible + ahorros.sdo_canje
       Mov_Ahorros.Usuario = w_usuario
       Mov_Ahorros.Val_Cheque = movProductos.sdo_canje
       Mov_Ahorros.Val_Efectivo = movProductos.sdo_disponible.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cuenta-Encontrada W-Mvto 
PROCEDURE Cuenta-Encontrada :
DEFINE VAR cont AS INTEGER.
DEFINE VAR numCheque AS DECIMAL INITIAL 0.
DEFINE VAR aux AS INTEGER.

/* Asigna los datos de la cuenta a la tabla temporal.*/
ASSIGN W_Nombre:SCREEN-VALUE IN FRAME F_Mov = Cuentas.Nombre
       Tmp.W_Nomcta = Cuentas.Nombre
       Tmp.W_Naturaleza = Cuentas.Naturaleza
       Tmp.W_Ctr_Natur = Cuentas.Ctr_Naturaleza
       Tmp.W_Id_Nit = Cuentas.Id_Nit
       Tmp.W_Id_Doc = Cuentas.Id_Doc
       Tmp.W_Id_CenCostos = Cuentas.Id_CenCostos
       Tmp.W_Id_Base = Cuentas.Id_Base
       Tmp.W_Id_Enlace = Cuentas.Id_Enlace
       Tmp.W_Id_Det = Cuentas.Id_Detalle.

/* Visualiza el saldo contable */
FIND FIRST sal_cuenta WHERE sal_cuenta.cuenta = cuentas.cuenta NO-LOCK NO-ERROR.
IF AVAILABLE sal_cuenta THEN DO:
    W_saldo = sal_cuenta.sal_inicial.

    DO cont = 1 TO MONTH(w_fecha):
        IF cuentas.naturaleza = "DB" THEN
            w_saldo = w_saldo + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
        ELSE
            w_saldo = w_saldo + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.

    w_saldo:SCREEN-VALUE = STRING(w_saldo).
END.

IF NOT Cuentas.Id_CenCostos THEN
    ASSIGN W_CCostos:SCREEN-VALUE IN BROWSE Temp = ""
           INPUT BROWSE Temp W_CCostos.

/*IF NOT Cuentas.Id_Nit THEN
    ASSIGN W_Nit:SCREEN-VALUE IN BROWSE Temp = ""
           INPUT BROWSE Temp W_Nit.*/

IF NOT Cuentas.Id_Doc THEN
    ASSIGN W_Documento:SCREEN-VALUE IN BROWSE Temp = ""
           INPUT BROWSE Temp W_Documento.

IF Tmp.W_Id_Enlace THEN
    ASSIGN Tmp.W_Cod_Producto = Cuentas.Cod_Enlace.

IF Tmp.W_Ctr_Natur THEN DO:
    IF Tmp.W_Naturaleza = "DB" THEN
        ASSIGN W_TotCre = IF (W_TotCre - DECIMAL(Tmp.W_Credito:SCREEN-VALUE)) GT 0 THEN W_TotCre - DECIMAL(Tmp.W_Credito:SCREEN-VALUE)
                          ELSE 0
               W_Credito:SCREEN-VALUE IN BROWSE Temp = STRING(0).
    ELSE
        ASSIGN W_TotDeb = IF (W_Totdeb - DECIMAL(Tmp.W_Debito:SCREEN-VALUE)) GT 0 THEN W_Totdeb - DECIMAL(Tmp.W_Debito:SCREEN-VALUE) 
                          ELSE 0
               W_Debito:SCREEN-VALUE IN BROWSE Temp = STRING(0).

    ASSIGN W_DifDeb = IF W_TotDeb LT W_TotCre THEN W_TotCre - W_TotDeb
                      ELSE 0
           W_DifCre = IF W_TotDeb GT W_TotCre THEN W_TotDeb - W_TotCre
                      ELSE 0.

    ASSIGN W_TotDeb:SCREEN-VALUE = STRING(W_TotDeb)
           W_TotCre:SCREEN-VALUE = STRING(W_TotCre)
           W_DifDeb:SCREEN-VALUE = STRING(W_DifDeb)
           W_DifCre:SCREEN-VALUE = STRING(W_DifCre).
END.

ASSIGN W_Base:SCREEN-VALUE IN FRAME F_Mov = "0.00".

IF Tmp.W_Id_Base THEN DO:
    FIND FIRST Base_Ret WHERE Base_Ret.Cod_Base = Cuentas.Cod_Base
                          AND Base_Ret.Estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Base_Ret) THEN
        ASSIGN Tmp.W_PorBas = Base_Ret.Porcentaje
               W_Base:SCREEN-VALUE IN FRAME F_Mov = STRING(Tmp.W_PorBas).
    ELSE
        ASSIGN Tmp.W_PorBas = 100.
END.
ELSE
    ASSIGN Tmp.W_Base = 0.00
           W_Valor:SCREEN-VALUE IN FRAME F_Mov = "0.00".

IF Tmp.W_Debito GT 0 THEN DO:
    IF Tmp.W_Id_Base THEN DO:
        IF Tmp.W_PorBas NE 0 THEN
            Tmp.W_Base = (Tmp.W_Debito * 100) / Tmp.W_PorBas.
        ELSE
            Tmp.W_Base = 0.

        W_Valor:SCREEN-VALUE IN FRAME F_Mov = STRING(Tmp.W_Base).
    END.
END.
ELSE DO:
    IF Tmp.W_Id_Base THEN DO:
        IF Tmp.W_PorBas NE 0 THEN
            Tmp.W_Base = (Tmp.W_Credito * 100) / Tmp.W_PorBas.
        ELSE
            Tmp.W_Base = 0.

        W_Valor:SCREEN-VALUE IN FRAME F_Mov = STRING(Tmp.W_Base).
    END.
END.

ASSIGN W_Nombre:SCREEN-VALUE IN FRAME F_Mov = Tmp.w_nomcta
       W_NomCom:SCREEN-VALUE IN FRAME F_Mov = Tmp.W_Comentario
       W_NomNit:SCREEN-VALUE IN FRAME F_Mov = Tmp.W_NomCli
       W_NomCen:SCREEN-VALUE IN FRAME F_Mov = Tmp.W_NomCcs
       W_Valor:SCREEN-VALUE  IN FRAME F_Mov = STRING(Tmp.W_Base).

/* Es verdadero, cuando el valor credito donde estaba anteriormente esta en cero se maneja en el main block, en ENTRY OF Tmp.W_Credito*/
IF Sw_SelfCr THEN
    W_Aux = DECIMAL(Tmp.W_Credito:SCREEN-VALUE IN BROWSE Temp).

/* Consecutivo del cheque */
IF cuentas.cod_flujoEfec = "D" AND Cuentas.Car_Efectivo = 3 AND cuentas.estado = 1 THEN DO:
    /*FOR EACH mov_contable WHERE mov_contable.agencia = w_agencia
                            AND mov_contable.fec_contable >= 03/01/2011
                            AND mov_contable.cuenta = cuentas.cuenta
                            AND mov_contable.fec_contable <= w_fecha NO-LOCK BY mov_contable.num_documento:
        aux = INTEGER(mov_contable.doc_referencia) NO-ERROR.

        IF aux > numCheque THEN
            numCheque = aux.
    END.

    W_Documento:SCREEN-VALUE IN BROWSE Temp = STRING(numCheque + 1).*/

    FOR EACH mov_contable WHERE mov_contable.agencia = w_agencia
                            AND mov_contable.cuenta = cuentas.cuenta
                            AND mov_contable.fec_contable <= w_fecha NO-LOCK BY mov_contable.fec_contable DESCENDING
                                                                             BY mov_contable.num_documento DESCENDING:

        aux = INTEGER(mov_contable.doc_referencia) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN.
        ELSE DO:
            numCheque = aux.
            LEAVE.
        END.
    END.

    IF numCheque = 0 THEN DO:
        FOR EACH mov_contable2 WHERE mov_contable2.agencia = w_agencia
                                 AND mov_contable2.cuenta = cuentas.cuenta
                                 AND mov_contable2.fec_contable <= w_fecha NO-LOCK BY mov_contable2.fec_contable DESCENDING
                                                                                   BY mov_contable2.num_documento DESCENDING:
            aux = INTEGER(mov_contable2.doc_referencia) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN.
            ELSE DO:
                numCheque = aux.
                LEAVE.
            END.
        END.
    END.

    W_Documento:SCREEN-VALUE IN BROWSE Temp = STRING(numCheque + 1).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Mvto  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Mvto)
  THEN DELETE WIDGET W-Mvto.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Mvto  _DEFAULT-ENABLE
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
  DISPLAY F-ValBase W_NomNit W_Valor W_NomCen W_Base W_NomCom W_DifCre W_DifDeb 
          W_Nombre W_TotCre W_Totdeb W_UltGra W_Fec_Contable W_CpteP W_FechaSis 
          W_Hora Nom_Agencia W_CmbOfip W_Agenciap W_OfDestino W_Comprobante 
          W_Doc W_Saldo 
      WITH FRAME F_Mov IN WINDOW W-Mvto.
  ENABLE TEMP btnRecalcular BUTTON-25 W_CmbOfip W_OfDestino W_Comprobante W_Doc 
         Btn_Grabar Btn_Cancelar Btn_Salir Btn_Consulta Btn_Imprimir Btn_Ayuda 
         RECT-217 RECT-28 RECT-29 
      WITH FRAME F_Mov IN WINDOW W-Mvto.
  {&OPEN-BROWSERS-IN-QUERY-F_Mov}
  DISPLAY fecDocumento E_DocEncabezado E_DocConsulta W_OfiCon W_TipCon W_NumCon 
      WITH FRAME F_Consulta IN WINDOW W-Mvto.
  ENABLE fecDocumento E_DocEncabezado E_DocConsulta W_OfiCon W_TipCon W_NumCon 
         Btn_EjecutarCon Btn_CnlImp-2 
      WITH FRAME F_Consulta IN WINDOW W-Mvto.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  DISPLAY FD_Agencia FD_Cuenta FD_Nit FD_CenCostos FD_Documento FD_FecContable 
          FD_FecGrabacion FD_UltActualizacion FD_ValInicial FD_Debito FD_Credito 
          FD_Final FD_ValAmortizacion FD_Plazo 
      WITH FRAME F_Detalle IN WINDOW W-Mvto.
  ENABLE FD_ValAmortizacion FD_Plazo Bt_SalirDetalle 
      WITH FRAME F_Detalle IN WINDOW W-Mvto.
  {&OPEN-BROWSERS-IN-QUERY-F_Detalle}
  DISPLAY W_CAgencia W_NCostos 
      WITH FRAME F_Costos IN WINDOW W-Mvto.
  ENABLE B_Costos Btn_TerminaCenCos W_NCostos 
      WITH FRAME F_Costos IN WINDOW W-Mvto.
  {&OPEN-BROWSERS-IN-QUERY-F_Costos}
  DISPLAY W_CNombre 
      WITH FRAME F_Cuenta IN WINDOW W-Mvto.
  ENABLE W_CNombre B_Cuentas Btn_SalCuenta 
      WITH FRAME F_Cuenta IN WINDOW W-Mvto.
  {&OPEN-BROWSERS-IN-QUERY-F_Cuenta}
  DISPLAY W_FecImp W_OfiImp W_TipImp W_NroDoci W_NroDocF 
      WITH FRAME F_Imprimir IN WINDOW W-Mvto.
  ENABLE W_FecImp W_OfiImp W_TipImp W_NroDoci W_NroDocF Btn_Imp Btn_CnlImp 
      WITH FRAME F_Imprimir IN WINDOW W-Mvto.
  {&OPEN-BROWSERS-IN-QUERY-F_Imprimir}
  DISPLAY Rs_SiNoCheq W_NomBenCC Rs_SiNo 
      WITH FRAME F_SiImp IN WINDOW W-Mvto.
  ENABLE RECT-315 RECT-316 Rs_SiNoCheq W_NomBenCC Rs_SiNo 
      WITH FRAME F_SiImp IN WINDOW W-Mvto.
  {&OPEN-BROWSERS-IN-QUERY-F_SiImp}
  VIEW W-Mvto.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar W-Mvto 
PROCEDURE Grabar :
IF TRIM(W_Comprobante) = "" THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 13,
                                    OUTPUT W_Eleccion).

    APPLY "ENTRY" TO W_Comprobante IN FRAME F_Mov.

    RETURN ERROR.
END.

IF W_TotDeb <> W_TotCre THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 14,
                                    OUTPUT W_Eleccion).

    APPLY "ENTRY" TO Tmp.W_Cuenta IN BROWSE Temp.

    RETURN ERROR.
END.

IF w_TotDeb = 0 OR w_TotCre = 0 THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 15,
                                    OUTPUT W_Eleccion).

    APPLY "ENTRY" TO Tmp.W_Cuenta IN BROWSE Temp.

    RETURN ERROR.
END.
ELSE DO:
    ASSIGN W_TotDeb = 0
           W_TotCre = 0
           W_DifDeb = 0
           W_DifCre = 0.

    RUN ChequeoTotal.
    RUN Inicializar (INPUT 1).

    ASSIGN W_TotDeb:SCREEN-VALUE IN FRAME F_Mov = "0.00"
           W_TotCre:SCREEN-VALUE IN FRAME F_Mov = "0.00"
           W_DifDeb:SCREEN-VALUE IN FRAME F_Mov = "0.00"
           W_DifCre:SCREEN-VALUE IN FRAME F_Mov = "0.00"
           W_TotCre = 0
           W_TotDeb = 0
           W_DifDeb = 0
           W_DifCre = 0
           W_Doc:SCREEN-VALUE IN FRAME F_Mov = STRING(W_Doc + 1).

    BROWSE Temp:SENSITIVE = FALSE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar-Movimiento W-Mvto 
PROCEDURE Grabar-Movimiento :
DEFINE VAR vCuentaSES AS CHARACTER.

IF Tmp.W_Credito = 0 AND Tmp.W_Debito = 0 THEN
    RETURN.

CREATE Mov_Contable.
Mov_Contable.Agencia = W_OfiOrigen.
Mov_Contable.Destino = INTEGER(SUBSTRING(W_Ofdestino, 1, 3)).
Mov_Contable.Comprobante = INTEGER(SUBSTRING(W_Comprobante,1,2)).
Mov_Contable.Num_Documento = W_Doc.
Mov_Contable.Fec_Contable = W_Fec_Contable.
Mov_Contable.Fec_Grabacion = TODAY.
Mov_Contable.Cuenta = Tmp.W_Cuenta.
Mov_Contable.Comentario = Tmp.W_Comentario.
Mov_Contable.Usuario = W_Usuario.
Mov_Contable.Estacion = W_Estacion.
Mov_Contable.Nit = Tmp.W_Nit.

W_NroDocG = W_Doc.

FIND FIRST Cuentas WHERE Mov_Contable.Cuenta = Cuentas.Cuenta NO-LOCK NO-ERROR.

IF Tmp.W_Id_base THEN DO:
    IF AVAILABLE(Cuentas) THEN DO:
        IF Cuentas.Naturaleza = "DB" THEN DO:
            IF Mov_Contable.Db > 0 THEN
                Mov_Contable.Base = Tmp.W_Base.
            ELSE
                Mov_Contable.Base = Tmp.W_Base * (-1).
        END.
        ELSE DO:
            IF Mov_Contable.Cr > 0 THEN
                Mov_Contable.Base = Tmp.W_Base.
            ELSE
                Mov_Contable.Base = Tmp.W_Base * (-1).
        END.
    END.
END.

IF Tmp.W_Id_Det THEN
    Mov_Contable.Det_ValAmortizacion = Tmp.W_Det_VAmort.
ELSE DO:
    Mov_Contable.Det_ValAmortizacion = 0.
    Mov_Contable.Det_Plazo = 0.
END.

IF Tmp.W_Id_Doc THEN
    Mov_Contable.Doc_Referencia= Tmp.W_Documento.

IF Tmp.W_Id_Enlace THEN
    Mov_Contable.Enlace = Tmp.W_Enlace.

IF Tmp.W_Id_CenCostos THEN
    Mov_Contable.Cen_Costos = Tmp.W_CCostos.
ELSE
    Mov_Contable.Cen_Costos = W_CenCosGral.

IF Tmp.W_Debito <> 0 THEN
    ASSIGN Mov_Contable.Db = Tmp.W_Debito NO-ERROR.
ELSE
    ASSIGN Mov_Contable.Cr = Tmp.w_Credito NO-ERROR.

IF Tmp.W_Id_base THEN DO:
    FIND FIRST Cuentas WHERE Mov_Contable.Cuenta = Cuentas.Cuenta NO-LOCK.
    IF AVAILABLE(Cuentas) THEN DO:
        IF Cuentas.Naturaleza = "DB" THEN DO:
            IF Mov_Contable.DB > 0 THEN
                Mov_Contable.Base = Tmp.W_Base.
            ELSE
                Mov_Contable.Base = Tmp.W_Base * (-1).
        END.
        ELSE DO:
            IF Mov_Contable.Cr > 0 THEN
                Mov_Contable.Base = Tmp.W_Base.
            ELSE
                Mov_Contable.Base = Tmp.W_Base * (-1).
        END.
    END.
END.

IF Mov_Contable.Cr > 0 AND AVAILABLE(Cuentas) AND Cuentas.Cod_FlujoEfec = "D" AND Cuentas.Car_Efectivo = 3 AND Cuentas.Estado = 1 THEN DO:
    W_SiGirado = TRUE.
    W_RowidMC = ROWID(Mov_Contable).
END.

Temp:SENSITIVE IN FRAME F_Mov = FALSE.
W_ConEfe = No.

IF mov_contable.cuenta BEGINS '2442' AND TRIM(mov_contable.doc_referencia) <> "" THEN
    RUN grabar_RepGMF.

IF comprobantes.comprobante = 10 AND SUBSTRING(Tmp.w_Cuenta,1,2) = "17" THEN
    RUN ActivosFijos(INPUT Tmp.W_Debito,
                     INPUT Tmp.W_Credito,
                     INPUT Tmp.W_cuenta,
                     INPUT W_Doc).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_repGMF W-Mvto 
PROCEDURE Grabar_repGMF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR zimpto  AS  DECIMAL INITIAL 0.00. 
  DEFI VAR zvlr    AS  DECIMAL INITIAL 0.00. 
  CREATE Mov_GMF.
  
  IF mov_contable.cr GT 0 THEN ASSIGN zimpto = mov_contable.cr.
  ELSE ASSIGN zimpto = mov_contable.db * (-1).
  zvlr = ROUND((((zimpto) * 1000) / 4),2).

  ASSIGN Mov_GMF.Agencia             = mov_contable.agencia
         Mov_GMF.Agencia_Tx          = mov_contable.agencia
         Mov_GMF.Documento           = string(mov_contable.num_documento)
         Mov_GMF.Fecha               = mov_contable.fec_contable
         Mov_GMF.Hora                = TIME
         Mov_GMF.Descrip             = "GMF Grabar Movimiento Contable"
         Mov_GMF.Id_EfChTras         = 2
         Mov_GMF.Nit                 = mov_contable.nit
         Mov_GMF.Porc_Impto          = 0
         Mov_GMF.Renglon             = 00
         Mov_GMF.Tipo_Pdcto          = 3
         Mov_GMF.VrAcum_RetMes       = 0
         Mov_GMF.Cod_Pdcto           = INTEGER(SUBSTRING(mov_contable.doc_referencia,1,3))
         Mov_GMF.Cpte                = mov_contable.comprobante 
         Mov_GMF.CtaDb_ImptoCliente  = ''
         Mov_GMF.CtaDb_ImptoEntidad  = mov_contable.cuenta
         Mov_GMF.Cta_ContableCr      = mov_contable.cuenta
         Mov_GMF.Cta_PdctoOContab    = SUBSTRING(mov_contable.doc_referencia,4,14)
         Mov_GMF.VrBase_Cliente      = 0
         Mov_GMF.VrBase_Entidad      = zvlr
         Mov_GMF.VrBase_Exenta       = 0
         Mov_GMF.VrImpto_Cliente     = 0
         Mov_GMF.VrImpto_Entidad     = zImpto.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar W-Mvto 
PROCEDURE Inicializar :
DEFINE INPUT PARAMETER W_Sw AS INTEGER.

W_SiGirado = FALSE.

DO TRANSACTION:
    DO ON ENDKEY UNDO, RETRY:
        MESSAGE W_OfiOrigen W_CompAct
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_OfiOrigen
                                  AND Comprobantes.Comprobante EQ W_CompAct EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE Comprobantes THEN DO:
            IF LOCKED Comprobantes THEN
                RETRY.

            RUN MostrarMensaje IN W_Manija (INPUT 72, OUTPUT W_Eleccion).

            RETURN ERROR.
        END.

        IF Comprobantes.Id_Consecutivo EQ 2 THEN DO:
            IF MONTH(w_fecha) = MONTH(TODAY) AND YEAR(w_fecha) = YEAR(TODAY) OR comprobante.reiniciaCierre = FALSE THEN DO:
                FIND FIRST mov_contable WHERE mov_contable.agencia = comprobantes.agencia
                                          AND mov_contable.comprobante = comprobantes.comprobante
                                          AND mov_contable.num_documento = comprobantes.secuencia NO-LOCK NO-ERROR.
                IF AVAILABLE mov_contable OR comprobantes.secuencia = 0 THEN
                    Comprobantes.Secuencia = Comprobantes.Secuencia + 1.

                ASSIGN W_Doc = Comprobantes.Secuencia
                       W_Doc:SCREEN-VALUE IN FRAME F_Mov = STRING(W_Doc).
            END.
            ELSE DO:
                comprobantes.secuenciaCierre = comprobantes.secuenciaCierre + 1.
                    
                ASSIGN W_Doc = comprobantes.secuenciaCierre
                       W_Doc:SCREEN-VALUE IN FRAME F_Mov = STRING(W_Doc).
            END.
        END.

        FIND CURRENT comprobantes NO-LOCK NO-ERROR.
    END.

    ASSIGN W_UltGra:SCREEN-VALUE IN FRAME F_Mov = STRING(W_Doc)
           W_SiGirado = FALSE.

    /* Validar valor Compra Activo Fijo */
    FOR EACH ttActivos NO-LOCK:
        FIND FIRST activosFijos WHERE activosFijos.idActivo = ttActivos.id NO-LOCK NO-ERROR.
        
        IF activosFijos.valorCompra <> ttActivos.valor THEN DO:
            MESSAGE "El Activo Fijo se encuentra registrado con un valor de compra distinto al valor que" SKIP
                    "Usted está ingresando. Revise por favor..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
            RETURN ERROR.
        END.
    END.

    FOR EACH Tmp WHERE TRIM(Tmp.w_Cuenta) NE "" BY Tmp.W_Fila:
        RUN Grabar-Movimiento.
    END.

    /* Se hacen las anotaciones para los Activos Fijos */
    FOR EACH anotaActivosFijos NO-LOCK BREAK BY anotaActivosFijos.id:
        FIND FIRST activosFijos WHERE activosFijos.idActivo = anotaActivosFijos.id NO-ERROR.
        IF AVAILABLE activosFijos THEN DO:
            activosFijos.anotacion = activosFijos.anotacion + " - " + STRING(w_fecha,"99/99/9999") + " - Se modifica el valor del activo con documento contable #" +
                                     STRING(anotaActivosFijos.numDoc) + ", por valor de $" + STRING(anotaActivosFijos.valor,"->>>,>>>,>>>,>>9.99").

            IF LAST-OF(anotaActivosFijos.id) THEN DO:
                IF activosFijos.valorActual = 0 AND activosFijos.estado = 2 THEN
                    activosFijos.anotacion = activosFijos.anotacion + " - " + STRING(w_fecha,"99/99/9999") + " - Activo Dado de Baja".
            END.
        END.
    END.
END. /*TRANSACCION*/

ASSIGN FRAME F_Mov:SENSITIVE = FALSE
       Rs_SiNoCheq:SENSITIVE IN FRAME F_SiImp = TRUE
       W_NomBenCC:SCREEN-VALUE = ""
       W_NomBenCC.

IF NOT W_SiGirado THEN
    ASSIGN Rs_SiNoCheq:SENSITIVE = FALSE.
ELSE DO:
    FIND FIRST Mov_Contable WHERE ROWID(Mov_Contable) EQ W_RowidMC NO-LOCK NO-ERROR.
    IF Mov_Contable.Nit LE " " THEN
        MESSAGE "Recuerde que debe Tener Ced./Nit el Movimiento del Banco," SKIP
                "Para el Nombre del Beneficiario del Cheque."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE
        FIND FIRST Clientes WHERE Clientes.Nit EQ Mov_Contable.Nit NO-LOCK NO-ERROR.

    ASSIGN W_NomBenCC = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2) WHEN AVAIL(Clientes).

    W_NomBenCC:SCREEN-VALUE = W_NomBenCC.
END.

VIEW FRAME F_SiImp.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar-Variables W-Mvto 
PROCEDURE Inicializar-Variables :
/* Inicializar los campos de la pantalla.*/
W_OfiOrigen = W_Agencia.

FIND FIRST usuarios WHERE usuarios.usuario = w_usuario NO-LOCK NO-ERROR.
IF usuarios.permiteCambiarFecha = TRUE THEN
    w_fec_contable:SENSITIVE IN FRAME F_Mov = TRUE.

FOR EACH Agencias WHERE Agencias.Estado = 1 NO-LOCK BY Agencias.Agencia:
    FIND FIRST Comprobantes WHERE Comprobantes.Agencia = Agencias.Agencia
                              AND Comprobante.Estado = 1
                              AND Comprobantes.Id_Consecutivo <> 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Comprobantes) THEN DO:
        ASSIGN W_OfStr = STRING(Agencias.Agencia, Agencias.Agencia:FORMAT IN FRAME F_Mov) + "-" + STRING(Agencias.Nombre, Agencias.Nombre:FORMAT IN FRAME F_Mov)
               W_Metodo = W_OfiCon:ADD-LAST(W_OfStr) IN FRAME F_Consulta
               W_Metodo = W_OfiImp:ADD-LAST(W_OfStr) IN FRAME F_Imprimir
               W_Metodo = W_OfDestino:ADD-LAST(W_OfStr) IN FRAME F_Mov
               W_Metodo = W_CmbOfiP:ADD-LAST(W_OfStr) IN FRAME F_Mov.

        IF Agencias.Agencia = W_Agencia THEN
            ASSIGN W_AgenciaP = W_OfStr
                   W_OfiCon:SCREEN-VALUE IN FRAME F_Consulta = W_OfStr
                   W_OfiImp:SCREEN-VALUE IN FRAME F_Imprimir = W_OfStr
                   W_OfDestino:SCREEN-VALUE IN FRAME F_Mov = W_OfStr
                   W_CmbOfiP:SCREEN-VALUE IN FRAME F_Mov = W_OfStr.
    END.
END.

FIND FIRST usuarios WHERE Usuarios.Agencia = W_Agencia
                      AND Usuarios.Id_OpeOfi = TRUE
                      AND Usuarios.Usuario = W_Usuario
                      AND Usuarios.Prioridad > 2 NO-LOCK NO-ERROR.
IF NOT AVAILABLE(Usuarios) THEN DO:
    FIND FIRST Agencias WHERE Agencias.Estado = 1
                          AND Agencias.Agencia = W_Agencia NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Agencias THEN DO:
        MESSAGE "ERROR Agencia no definida"
            VIEW-AS ALERT-BOX.

        RETURN ERROR.
    END.

    ASSIGN W_OfStr = STRING(Agencias.Agencia, Agencias.Agencia:FORMAT IN FRAME F_Mov) + "-" + STRING(Agencias.Nombre, Agencias.Nombre:FORMAT IN FRAME F_Mov)
           W_AgenciaP:SCREEN-VALUE IN FRAME F_Mov = W_OfStr
           W_CmbOfiP:LIST-ITEMS = ""
           W_Metodo = W_CmbOfiP:ADD-LAST(W_OfStr) IN FRAME F_Mov
           W_CmbOfiP:SCREEN-VALUE = W_CmbOfiP:ENTRY(1) IN FRAME F_Mov.

    ASSIGN W_OfiOrigen = W_Agencia
           W_CAgencia:SCREEN-VALUE IN FRAME F_Costos = STRING(W_Agencia)
           W_CAgencia:SENSITIVE IN FRAME F_Costos = FALSE.
END.
ELSE
    ASSIGN W_AgenciaP:HIDDEN IN FRAME F_Mov = TRUE.

ASSIGN Nom_Agencia:SCREEN-VALUE IN FRAME F_Mov = W_Nom_Agencia
       W_Fec_Contable:SCREEN-VALUE IN FRAME F_Mov = STRING(W_Fecha)
       W_Hora:SCREEN-VALUE = STRING(TIME,"HH:MM AM")
       W_FechaSis:SCREEN-VALUE = STRING(TODAY).

APPLY "VALUE-CHANGED":U TO W_CmbOfip.
APPLY "VALUE-CHANGED":U TO W_OfiCon IN FRAME F_Consulta.
APPLY "VALUE-CHANGED":U TO W_OfiImp IN FRAME F_Imprimir.

APPLY "ENTRY":U TO W_CmbOfip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_Detalle W-Mvto 
PROCEDURE Llenar_Detalle :
/*permite llenar el frame de Detalle con la informacion de la tabla para poder ser
  vista por el usuario en el caso de que lo desee */
W_Existe = NO.
FIND Detalle WHERE Detalle.Nit            EQ Tmp.W_Nit       AND
                   Detalle.Cuenta         EQ Tmp.W_Cuenta    AND
                   Detalle.Doc_referencia EQ Tmp.W_Documento NO-ERROR.
IF AVAILABLE(Detalle) THEN DO:
   W_Existe = YES.
   DISABLE {&List-6} WITH FRAME F_Detalle.
   IF W_Agencia EQ Detalle.Agencia THEN
      FD_Agencia = W_Agenciap:SCREEN-VALUE IN FRAME F_Mov.
   ELSE DO:
      FIND Agencias WHERE Agencias.Agencia EQ Detalle.Agencia NO-LOCK NO-ERROR.
      IF AVAILABLE(Agencias) THEN
         FD_Agencia = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
      ELSE
         FD_Agencia = "Agencia No existe".
   END.
   DO WITH FRAME F_Detalle:
     ASSIGN FD_Cuenta           = Tmp.W_Cuenta + " - " + W_Nombre:SCREEN-VALUE IN FRAME F_Mov
            FD_Nit              = Tmp.W_Nit    + " - " + W_NomNit:SCREEN-VALUE IN FRAME F_Mov
            FD_CenCostos        = Detalle.Cen_Costos
            FD_Documento        = Tmp.W_Documento
            FD_FecContable      = Detalle.Fec_Contable
            FD_FecGrabacion     = Detalle.Fec_Grabacion
            FD_UltActualizacion = Detalle.Fec_ultActualizacion
            FD_ValInicial       = Detalle.Valor_inicial
            FD_Debito           = Detalle.Db
            FD_Credito          = Detalle.Cr
            FD_ValAmortizacion  = Detalle.Valor_amortizacion
            FD_Plazo            = Detalle.Plazo.
   END.
END.
ELSE DO:
    ENABLE {&List-6} WITH FRAME F_Detalle.
    DO WITH FRAME F_Detalle:
      ASSIGN FD_Agencia          = W_CmbOfip:SCREEN-VALUE IN FRAME F_Mov
             FD_Cuenta           = Tmp.W_Cuenta + " - " + W_Nombre:SCREEN-VALUE IN FRAME F_Mov
             FD_Nit              = Tmp.W_Nit    + " - " + W_NomNit:SCREEN-VALUE IN FRAME F_Mov
             FD_CenCostos        = Tmp.W_CCostos
             FD_Documento        = Tmp.W_Documento
             FD_FecContable      = W_Fec_Contable
             FD_FecGrabacion     = W_Fec_Contable
             FD_UltActualizacion = W_Fec_Contable.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query W-Mvto 
PROCEDURE reopen-query :
DO WITH FRAME F_MOV:
    {&OPEN-QUERY-TEMP}
    IF open-on-row > 0 THEN 
       W_Metodo = TEMP:SET-REPOSITIONED-ROW (open-on-row, "CONDITIONAL":U).
    REPOSITION Temp TO RECID open-recid NO-ERROR.
    W_Metodo = Temp:SELECT-ROW(Temp:FOCUSED-ROW) NO-ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reorder-browse W-Mvto 
PROCEDURE reorder-browse :
DEFINE BUFFER XTmp FOR Tmp.
  REPEAT PRESELECT EACH XTmp BY XTmp.W_Fila:
     FIND NEXT Xtmp.
     ASSIGN i = i + 2
            XTmp.W_Fila = i.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReversarProducto W-Mvto 
PROCEDURE ReversarProducto :
FOR EACH movProductos WHERE movProductos.agencia = reversar.agencia
                        AND movProductos.comprobante = reversar.comprobante
                        AND movProductos.estado = 1
                        AND movProductos.fecha = reversar.fec_contable
                        AND movProductos.num_documento = reversar.num_documento:
    IF movProductos.tipo_producto = 1 THEN DO:
        FIND FIRST ahorros WHERE ahorros.agencia = movProductos.agencia
                             AND ahorros.nit = movProductos.nit
                             AND ahorros.cue_ahorros = movProductos.id_producto
                             /*AND ahorros.estado = 1*/ NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            IF movProductos.tipo_transaccion = 1 THEN DO:
                ahorros.sdo_disponible = ahorros.sdo_disponible - movProductos.sdo_disponible.
                ahorros.INT_causado = ahorros.INT_causado - movProductos.INT_causado.
                ahorros.INT_pagar = ahorros.INT_pagar - movProductos.INT_pagar.
                
                IF ahorros.sdo_canje >= movProductos.sdo_canje THEN
                    ahorros.sdo_canje = ahorros.sdo_canje - movProductos.sdo_canje.
                ELSE DO:
                    ahorros.sdo_disponible = ahorros.sdo_disponible - (movProductos.sdo_canje - ahorros.sdo_canje).
                    ahorros.sdo_canje = 0.
                END.

                RUN crearMovAhorros.
                mov_ahorros.cod_operacion = 010102001.
            END.
            ELSE DO:
                ahorros.sdo_disponible = ahorros.sdo_disponible + movProductos.sdo_disponible.
                ahorros.sdo_canje = ahorros.sdo_canje + movProductos.sdo_canje.
                ahorros.INT_causado = ahorros.INT_causado + movProductos.INT_causado.
                ahorros.INT_pagar = ahorros.INT_pagar + movProductos.INT_pagar.
                
                IF ahorros.sdo_canje >= movProductos.sdo_canje THEN
                    ahorros.sdo_canje = ahorros.sdo_canje - movProductos.sdo_canje.
                ELSE DO:
                    ahorros.sdo_disponible = ahorros.sdo_disponible - (movProductos.sdo_canje - ahorros.sdo_canje).
                    ahorros.sdo_canje = 0.
                END.

                IF ahorros.estado = 2 THEN
                    ahorros.estado = 1.

                RUN crearMovAhorros.
                mov_ahorros.cod_operacion = 010101001.
            END.
                
            movProductos.estado = 2.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RutinaImprimirReversar W-Mvto 
PROCEDURE RutinaImprimirReversar :
DEFINE INPUT PARAMETER pComprobante AS INTEGER.
DEFINE INPUT PARAMETER pAgencia AS INTEGER.
DEFINE INPUT PARAMETER pNumDocumento AS INTEGER.
DEFINE INPUT PARAMETER pFechaCont AS DATE.

FIND FIRST Comprobantes WHERE Comprobantes.Comprobante EQ pComprobante
                          AND Comprobantes.Agencia EQ pAgencia NO-LOCK NO-ERROR.
IF AVAILABLE Comprobantes THEN DO:
    FIND FIRST Formatos WHERE Formatos.Agencia EQ pAgencia
                          AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato NO-LOCK NO-ERROR.
    IF AVAILABLE(Formatos) THEN
        RUN VALUE(Formatos.Nom_Proceso) (INPUT pComprobante,
                                         INPUT pNumDocumento,
                                         INPUT pNumDocumento,
                                         INPUT pAgencia,
                                         INPUT pFechaCont).
    ELSE
        RUN MostrarMensaje IN W_Manija (INPUT 345,
                                         OUTPUT W_Rpta).
END.
ELSE
    RUN MostrarMensaje IN W_Manija (INPUT 268,
                                    OUTPUT W_Rpta).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rutina_Imprimir W-Mvto 
PROCEDURE Rutina_Imprimir :
DEFINE INPUT PARAMETER P_CompAct LIKE Comprobantes.comprobante.
DEFINE INPUT PARAMETER P_OfiOrigen LIKE Agencias.Agencia.
DEFINE INPUT PARAMETER P_DocIni LIKE Comprobantes.Secuencia.
DEFINE INPUT PARAMETER P_DocFin LIKE Comprobantes.Secuencia.
DEFINE INPUT PARAMETER P_Fecha AS DATE.

FIND FIRST Comprobantes WHERE Comprobantes.Comprobante EQ P_CompAct
                          AND Comprobantes.Agencia EQ P_OfiOrigen NO-LOCK NO-ERROR.
IF AVAILABLE Comprobantes THEN DO:
    FIND FIRST Formatos WHERE Formatos.Agencia EQ P_OfiOrigen
                          AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato NO-LOCK NO-ERROR.
    
    DO i = p_docini TO p_docfin BY 1:
        FIND FIRST mov_contable WHERE mov_contable.agencia = P_OfiOrigen
                                  AND mov_contable.comprobante = P_CompAct
                                  AND mov_contable.num_documento = i
                                  AND Mov_Contable.Fec_Contable = P_fecha NO-LOCK NO-ERROR.
        IF AVAILABLE(mov_contable) THEN
            APPLY "entry" TO W_OfDestino IN FRAME F_Mov.
        ELSE DO:
            MESSAGE "El Documento : " i " No existe " SKIP(1)
                    "en la oficina: " P_OfiOrigen     SKIP(1)
                    "Comprobante  : " P_CompAct       SKIP(1)
                    "Verifique la informacion suministrada!!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN.
        END.
    END.

    IF AVAILABLE(Formatos) THEN DO:
        MESSAGE formatos.nom_proceso
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN VALUE(Formatos.Nom_Proceso) (INPUT P_CompAct,
                                         INPUT P_DocIni,
                                         INPUT P_DocFin,
                                         INPUT P_OfiOrigen,
                                         INPUT p_fecha).
    END.
    ELSE
        RUN MostrarMensaje IN W_Manija (INPUT 345, OUTPUT W_Rpta).
END.
ELSE
    RUN MostrarMensaje IN W_Manija (INPUT 268, OUTPUT W_Rpta).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaldoFinal_Detalle W-Mvto 
PROCEDURE SaldoFinal_Detalle :
IF Tmp.W_Naturaleza EQ "DB" THEN
    FD_Final = FD_Debito - FD_Credito.
 ELSE
    FD_Final = FD_Credito - FD_Debito.
 DISPLAY FD_Agencia FD_Cuenta FD_Nit FD_CenCostos FD_Documento FD_FecContable
         FD_FecGrabacion FD_UltActualizacion FD_ValInicial FD_Debito FD_Credito FD_Final WITH FRAME F_Detalle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar-CtasEfectivo W-Mvto 
PROCEDURE Verificar-CtasEfectivo :
FOR EACH Tmp WHERE Tmp.W_Debito NE 0 OR Tmp.W_Credito NE 0 BY Tmp.W_Fila:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta = Tmp.W_Cuenta 
                         AND Cuentas.Cod_FlujoEfec = "D"
                         AND cuentas.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Cuentas) THEN DO:
        W_ConEfe = TRUE.
        LEAVE.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar-Mes W-Mvto 
PROCEDURE Verificar-Mes :
FIND FIRST procdia WHERE procdia.Agencia = W_OfiOrigen
                     AND procdia.COD_Proceso = 7
                     AND MONTH(procdia.Fecha) = MONTH(W_fec_contable)
                     AND YEAR(procdia.Fecha) = YEAR(W_Fec_contable)
                     AND procdia.Estado = 2 NO-LOCK NO-ERROR.
IF AVAILABLE(procdia) THEN DO:
    MESSAGE "El Mes se encuentra cerrado." SKIP
            "No se pueden realizar operaciones contables"
        VIEW-AS ALERT-BOX INFORMATION.

    RETURN ERROR.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

