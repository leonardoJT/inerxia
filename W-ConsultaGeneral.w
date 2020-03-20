&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
DEFINE VAR W_sw AS LOGICAL.
DEFINE VAR WRel AS INTEGER.
DEFINE VAR W_NomEmpresa AS CHARACTER FORMAT "X(40)".
DEFINE VAR W_NomCodeudor AS CHARACTER FORMAT "X(40)".
DEFINE VAR P_Ubi AS CHARACTER.
DEFINE VAR P_NUbi AS CHARACTER FORMAT "X(80)".
DEFINE BUFFER TCli FOR Clientes.
DEFINE VAR i AS INTEGER.
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR NitWk AS CHARACTER.
DEFINE VAR TIni AS INTEGER INITIAL 0.
DEFINE VAR TFin AS INTEGER INITIAL 999.
DEFINE VAR CIni AS INTEGER INITIAL 0.
DEFINE VAR CFin AS INTEGER INITIAL 99999.
DEFINE VAR W_TextCombo AS CHARACTER FORMAT "X(25)".
DEFINE VAR W_SiIngr AS LOGICAL.
DEFINE VAR WOcur AS INTEGER.
DEFINE VAR W_SiMInst AS LOGICAL.
DEFINE VAR W_RowIdCobro AS ROWID.
DEFINE VAR IdAbo AS LOGICAL.
DEFINE VAR Listado AS CHARACTER.
DEFINE VAR WListado AS CHARACTER.
DEFINE VAR W_Texto AS CHARACTER FORMAT "X(132)".
DEFINE VAR gTexto AS CHARACTER FORMAT "X(60)".
DEF VAR W_Sarlaft AS LOG INIT FALSE.

/* oakley */

DEFINE VARIABLE gDis AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VARIABLE gCuo AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR W_CedCteC AS CHARACTER.

DEFINE TEMP-TABLE TInfo
    FIELD Tip AS CHARACTER FORMAT "X(3)"
    FIELD Reg AS INTEGER FORMAT "999"
    FIELD Tex AS CHARACTER FORMAT "X(132)"
    FIELD cedula AS CHARACTER FORMAT "X(12)"
    FIELD nombre AS CHARACTER FORMAT "X(50)"
    INDEX idxNit cedula.

DEFINE TEMP-TABLE TAhorros LIKE Ahorros.
DEFINE TEMP-TABLE TCreditos LIKE Creditos.
DEFINE TEMP-TABLE TSolicitud LIKE Solicitud.

DEFINE TEMP-TABLE TMovIns
    FIELD TOrd AS INTEGER
    FIELD TSol LIKE Mov_Instancias.Num_Solicitud
    FIELD TIns LIKE Mov_Instancias.Instancia
    FIELD TFci LIKE Mov_Instancias.Fec_Ingreso
    FIELD TFcr LIKE Mov_Instancias.Fec_Retiro
    FIELD TEst LIKE Mov_Instancias.Estado
    FIELD TDet AS CHARACTER FORMAT "X(50)"
    FIELD TUsu LIKE Usuario.Usuario.

/* oakley */

DEFINE TEMP-TABLE TMP LIKE TInfo.

DEFINE TEMP-TABLE TClientes LIKE Clientes.

DEFINE TEMP-TABLE TSaldos
    FIELD Cla AS INTEGER FORMAT "9" /* Ahorros, Creditos, Especiales*/
    FIELD Cod LIKE Ahorros.Cod_Ahorro
    FIELD Nom AS CHARACTER FORMAT "X(15)"
    FIELD Dis LIKE Ahorros.Sdo_Disponible
    FIELD Che LIKE Ahorros.Sdo_Canje
    FIELD Itr LIKE Ahorros.Int_Pagar
    FIELD Cuo LIKE Creditos.Cuota
    FIELD Tas AS DECIMAL FORMAT ">>,>>9.99"
    FIELD CuotasXpagar AS INTEGER
    FIELD cuoVencidas AS INTEGER
    FIELD fecGiro AS DATE
    FIELD nomAgencia AS CHARACTER
    FIELD fecApertura AS CHARACTER
    FIELD fecVcto AS CHARACTER
    FIELD cupoDisponible AS DECIMAL
    INDEX Ippal Cla Cod.

DEFINE TEMP-TABLE TRelaciones
    FIELD TpoR LIKE Relaciones.Cod_Relacion
    FIELD TipR LIKE Varios.Descripcion
    FIELD NitR LIKE Clientes.Nit
    FIELD ParR LIKE Relaciones.Descripcion
    FIELD NomR AS CHARACTER FORMAT "X(50)"
    FIELD TelR AS CHARACTER FORMAT "X(20)"
    FIELD ClaR AS CHARACTER FORMAT "X(3)"
    FIELD CodR LIKE Relaciones.Cod_Producto
    FIELD CueR LIKE Relaciones.Cuenta
    FIELD CodA AS INTEG FORM "9" INIT 0.

DEFINE TEMP-TABLE THojaVida
    FIELD FecH AS DATE FORMAT "99/99/9999"
    FIELD Cump AS LOG FORM "Si/No"
    FIELD AsuH AS CHARACTER FORMAT "X(25)"
    FIELD UsuH LIKE Usuarios.Usuario
    FIELD DocH AS DEC FORMAT "9999999999999"
    FIELD CueH AS CHARACTER FORMAT "X(15)"
    FIELD FecR AS DATE FORMAT "99/99/9999"
    FIELD DesH AS CHARACTER FORMAT "X(400)"
    FIELD CodA AS INTEG FORM "9" INIT 0
    FIELD InsH LIKE Instancias.Tipo_Instancia.

DEFINE TEMP-TABLE TScoring
    FIELD CodS LIKE Scoring.Codigo
    FIELD VarS LIKE Scoring.VARIABLE
    FIELD VVaS LIKE Scoring.Valor_Variable
    FIELD PunS LIKE Scoring.Puntaje
    FIELD FecS LIKE Scoring.Fec_Scoring.

DEFINE TEMP-TABLE TGarantias
    FIELD GTip AS INTEGER FORMAT "9"
    FIELD GNCr LIKE Garantias.Num_Credito
    FIELD GNit LIKE Garantias.Nit
    FIELD GNNt AS CHARACTER FORMAT "X(30)"
    FIELD GTel LIKE Clientes.Tel_Residencia
    FIELD GIde LIKE Garantias.Identificacion_Bien
    FIELD GNom LIKE Garantias.Nom_Bien
    FIELD GVal LIKE Garantias.Val_Bien
    FIELD GVSg AS CHARACTER FORMAT "X(10)"
    FIELD GPIm AS CHARACTER FORMAT "X(10)"
    FIELD GPAv AS CHARACTER FORMAT "X(10)".

DEFINE VAR P_Nit AS CHAR NO-UNDO.
DEFINE VAR P_Nombre AS CHAR NO-UNDO.
DEFINE VAR P_Apellido LIKE Clientes.Apellido1.
DEFINE VAR P_AgeCli LIKE Clientes.Agencia.

DEFINE TEMP-TABLE TCJ
    FIELD Nom_Juzgado LIKE Creditos.Nom_Juzgado.

/*para buscar cuentas destino y cuentas debito automatico*/
DEFINE VAR W_Age LIKE Ahorros.Agencia.
DEFINE VAR W_Pro LIKE Ahorros.Cod_Ahorro.
DEFINE VAR W_Nit LIKE Ahorros.Nit.
DEFINE VAR W_Cue LIKE Ahorros.Cue_Ahorros.

/*Variables Para Imprimir en Excel*/

DEF VAR Dato AS CHA NO-UNDO.
DEF VAR ValCol AS CHA NO-UNDO.
DEF VAR SwExiste AS CHA NO-UNDO.
DEF VAR InputFile AS CHA NO-UNDO.
DEF VAR PrinterName AS CHA NO-UNDO.
DEF VAR chExcelApp AS COM-HANDLE NO-UNDO.
DEF VAR hWorkBooks AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet AS COM-HANDLE NO-UNDO.

DEFINE BUFFER bfrAhorros FOR ahorros.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_ConsCred
&Scoped-define BROWSE-NAME BCMI

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES THojaVida TCreditos Relaciones Creditos Tmp ~
Cobros Clientes Hoja_Vida Plastico

/* Definitions for BROWSE BCMI                                          */
&Scoped-define FIELDS-IN-QUERY-BCMI THojaVida.InsH THojaVida.AsuH THojaVida.DocH THojaVida.CueH THojaVida.FecH THojaVida.FecR THojaVida.Cump   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BCMI   
&Scoped-define SELF-NAME BCMI
&Scoped-define QUERY-STRING-BCMI FOR EACH THojaVida WHERE THojaVida.CodA EQ 9
&Scoped-define OPEN-QUERY-BCMI OPEN QUERY {&SELF-NAME} FOR EACH THojaVida WHERE THojaVida.CodA EQ 9.
&Scoped-define TABLES-IN-QUERY-BCMI THojaVida
&Scoped-define FIRST-TABLE-IN-QUERY-BCMI THojaVida


/* Definitions for BROWSE BCobro                                        */
&Scoped-define FIELDS-IN-QUERY-BCobro TCreditos.Num_Credito TCreditos.Sdo_Capital TCreditos.Nit_Juzgado TCreditos.Nom_Juzgado   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BCobro   
&Scoped-define SELF-NAME BCobro
&Scoped-define QUERY-STRING-BCobro FOR EACH TCreditos WHERE TCreditos.Nit EQ W_CedCteC AND                                                  TCreditos.Estado EQ 2
&Scoped-define OPEN-QUERY-BCobro OPEN QUERY {&SELF-NAME} FOR EACH TCreditos WHERE TCreditos.Nit EQ W_CedCteC AND                                                  TCreditos.Estado EQ 2.
&Scoped-define TABLES-IN-QUERY-BCobro TCreditos
&Scoped-define FIRST-TABLE-IN-QUERY-BCobro TCreditos


/* Definitions for BROWSE BC_Codeudores                                 */
&Scoped-define FIELDS-IN-QUERY-BC_Codeudores Relaciones.Nit_Relacion W_NomCodeudor Relaciones.Descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BC_Codeudores   
&Scoped-define SELF-NAME BC_Codeudores
&Scoped-define QUERY-STRING-BC_Codeudores FOR EACH Relaciones WHERE                                  Relaciones.Cod_Relacion EQ 11  AND                                  Relaciones.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta AND                                  Relaciones.Clase_Producto EQ 2 AND                                  Relaciones.Cod_Producto EQ Creditos.Cod_Credito AND                                  Relaciones.Cuenta EQ STRING(Creditos.Num_Credito) AND                                  Relaciones.Estado EQ 1
&Scoped-define OPEN-QUERY-BC_Codeudores OPEN QUERY {&SELF-NAME} FOR EACH Relaciones WHERE                                  Relaciones.Cod_Relacion EQ 11  AND                                  Relaciones.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta AND                                  Relaciones.Clase_Producto EQ 2 AND                                  Relaciones.Cod_Producto EQ Creditos.Cod_Credito AND                                  Relaciones.Cuenta EQ STRING(Creditos.Num_Credito) AND                                  Relaciones.Estado EQ 1.
&Scoped-define TABLES-IN-QUERY-BC_Codeudores Relaciones
&Scoped-define FIRST-TABLE-IN-QUERY-BC_Codeudores Relaciones


/* Definitions for BROWSE BC_Creditos                                   */
&Scoped-define FIELDS-IN-QUERY-BC_Creditos Creditos.Agencia Creditos.Cod_Credito Creditos.Num_Credito Creditos.Num_Solicitud Creditos.Monto Creditos.Sdo_Capital Creditos.Cuota Creditos.Plazo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BC_Creditos   
&Scoped-define SELF-NAME BC_Creditos
&Scoped-define QUERY-STRING-BC_Creditos FOR EACH Creditos WHERE Creditos.Nit EQ TClientes.Nit
&Scoped-define OPEN-QUERY-BC_Creditos OPEN QUERY {&SELF-NAME} FOR EACH Creditos WHERE Creditos.Nit EQ TClientes.Nit.
&Scoped-define TABLES-IN-QUERY-BC_Creditos Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-BC_Creditos Creditos


/* Definitions for BROWSE BInfo                                         */
&Scoped-define FIELDS-IN-QUERY-BInfo TEX   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BInfo   
&Scoped-define SELF-NAME BInfo
&Scoped-define QUERY-STRING-BInfo FOR EACH Tmp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BInfo OPEN QUERY {&SELF-NAME} FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BInfo Tmp
&Scoped-define FIRST-TABLE-IN-QUERY-BInfo Tmp


/* Definitions for BROWSE BRCompromisos                                 */
&Scoped-define FIELDS-IN-QUERY-BRCompromisos Cobros.Agencia Cobros.Fec_Acuerdo Cobros.Fec_Compromiso Cobros.Nit Cobros.Num_Credito Cobros.Val_Compromiso Cobros.Val_Cumplido Cobros.Vr_Vencido Cobros.Usuario Cobros.Instanc Cobros.Estado Cobros.Usu_Recaudo Cobros.Categoria   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRCompromisos   
&Scoped-define SELF-NAME BRCompromisos
&Scoped-define OPEN-QUERY-BRCompromisos ASSIGN FRAME F_Compromisos REstado. OPEN QUERY {&SELF-NAME}     FOR EACH Cobros WHERE              Cobros.Nit         EQ Clientes.Nit AND              Cobros.Num_Credito EQ Creditos.Num_Credito AND              Cobros.Estado      EQ REstado.
&Scoped-define TABLES-IN-QUERY-BRCompromisos Cobros
&Scoped-define FIRST-TABLE-IN-QUERY-BRCompromisos Cobros


/* Definitions for BROWSE BRCreditos                                    */
&Scoped-define FIELDS-IN-QUERY-BRCreditos Creditos.Agencia Creditos.Cod_Credito Creditos.Num_Credito Creditos.Cuota Creditos.Cuo_Pagadas Creditos.Fec_Desembolso Creditos.Fec_UltPago Creditos.Monto Creditos.Sdo_Capital Creditos.Plazo Creditos.Tasa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRCreditos   
&Scoped-define SELF-NAME BRCreditos
&Scoped-define QUERY-STRING-BRCreditos FOR EACH Creditos WHERE Creditos.Nit EQ Clientes.Nit
&Scoped-define OPEN-QUERY-BRCreditos OPEN QUERY BrCreditos FOR EACH Creditos WHERE Creditos.Nit EQ Clientes.Nit.
&Scoped-define TABLES-IN-QUERY-BRCreditos Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-BRCreditos Creditos


/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 Creditos.Agencia Creditos.Cod_Credito Creditos.Fec_Desembolso Creditos.Num_Credito Creditos.Pagare Creditos.Sdo_Capital Creditos.For_Pago Creditos.Fec_Pago   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 /*OPEN QUERY {&SELF-NAME} FOR EACH Creditos WHERE            Creditos.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta        AND Sdo_Capital  GT 0 NO-LOCK INDEXED-REPOSITION.*/.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 Creditos


/* Definitions for BROWSE Br_Clientes                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Clientes Clientes.Agencia Clientes.Nit Clientes.Nombre Clientes.Apellido1 Clientes.Apellido2   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Clientes   
&Scoped-define SELF-NAME Br_Clientes
&Scoped-define QUERY-STRING-Br_Clientes FOR EACH Clientes NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Clientes OPEN QUERY {&SELF-NAME} FOR EACH Clientes NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Clientes Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Clientes Clientes


/* Definitions for BROWSE BR_HojaVida                                   */
&Scoped-define FIELDS-IN-QUERY-BR_HojaVida Hoja_Vida.Tipo Hoja_Vida.Codigo Hoja_Vida.Usuario Hoja_Vida.Fec_Grabacion Hoja_Vida.Observacion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR_HojaVida   
&Scoped-define SELF-NAME BR_HojaVida
&Scoped-define QUERY-STRING-BR_HojaVida FOR EACH Hoja_Vida WHERE              Hoja_Vida.Nit           EQ NitWk AND              Hoja_Vida.Tipo          GE TIni AND              Hoja_Vida.Tipo          LE TFin AND              Hoja_Vida.Codigo        GE CIni AND              Hoja_Vida.Codigo        LE CFin AND              Hoja_Vida.Fec_Grabacion GE FIni AND              Hoja_Vida.Fec_Grabacion LE FFin USE-INDEX Idx_HojaVida NO-LOCK
&Scoped-define OPEN-QUERY-BR_HojaVida OPEN QUERY {&SELF-NAME} FOR EACH Hoja_Vida WHERE              Hoja_Vida.Nit           EQ NitWk AND              Hoja_Vida.Tipo          GE TIni AND              Hoja_Vida.Tipo          LE TFin AND              Hoja_Vida.Codigo        GE CIni AND              Hoja_Vida.Codigo        LE CFin AND              Hoja_Vida.Fec_Grabacion GE FIni AND              Hoja_Vida.Fec_Grabacion LE FFin USE-INDEX Idx_HojaVida NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BR_HojaVida Hoja_Vida
&Scoped-define FIRST-TABLE-IN-QUERY-BR_HojaVida Hoja_Vida


/* Definitions for BROWSE B_Relaciones                                  */
&Scoped-define FIELDS-IN-QUERY-B_Relaciones W_NomEmpresa Relaciones.Descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Relaciones Relaciones.Descripcion   
&Scoped-define ENABLED-TABLES-IN-QUERY-B_Relaciones Relaciones
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-B_Relaciones Relaciones
&Scoped-define SELF-NAME B_Relaciones
&Scoped-define QUERY-STRING-B_Relaciones FOR EACH Relaciones WHERE Relaciones.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta      AND Relaciones.Cod_Relacion EQ 10 AND Relaciones.Estado EQ 1
&Scoped-define OPEN-QUERY-B_Relaciones OPEN QUERY {&SELF-NAME} FOR EACH Relaciones WHERE Relaciones.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta      AND Relaciones.Cod_Relacion EQ 10 AND Relaciones.Estado EQ 1.
&Scoped-define TABLES-IN-QUERY-B_Relaciones Relaciones
&Scoped-define FIRST-TABLE-IN-QUERY-B_Relaciones Relaciones


/* Definitions for FRAME FPlastico                                      */
&Scoped-define FIELDS-IN-QUERY-FPlastico Plastico.Cue_Ahorros ~
Plastico.Num_Plastico 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FPlastico Plastico.Num_Plastico 
&Scoped-define ENABLED-TABLES-IN-QUERY-FPlastico Plastico
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FPlastico Plastico
&Scoped-define QUERY-STRING-FPlastico FOR EACH Plastico SHARE-LOCK
&Scoped-define OPEN-QUERY-FPlastico OPEN QUERY FPlastico FOR EACH Plastico SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FPlastico Plastico
&Scoped-define FIRST-TABLE-IN-QUERY-FPlastico Plastico


/* Definitions for FRAME F_AdmCodeudores                                */
&Scoped-define FIELDS-IN-QUERY-F_AdmCodeudores Clientes.Dir_comercial ~
Clientes.Dir_Residencia Clientes.Tel_comercial Clientes.Tel_Residencia ~
Clientes.Salario Clientes.Email Clientes.Celular Clientes.Fec_Nacimiento 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_AdmCodeudores ~
Clientes.Dir_comercial Clientes.Dir_Residencia Clientes.Tel_comercial ~
Clientes.Tel_Residencia Clientes.Salario Clientes.Email Clientes.Celular ~
Clientes.Fec_Nacimiento 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_AdmCodeudores Clientes
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_AdmCodeudores Clientes
&Scoped-define QUERY-STRING-F_AdmCodeudores FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-F_AdmCodeudores OPEN QUERY F_AdmCodeudores FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_AdmCodeudores Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-F_AdmCodeudores Clientes


/* Definitions for FRAME F_Browser                                      */

/* Definitions for FRAME F_BuscarM                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_BuscarM ~
    ~{&OPEN-QUERY-BR_HojaVida}

/* Definitions for FRAME F_CobroJuridico                                */

/* Definitions for FRAME F_Compromisos                                  */
&Scoped-define FIELDS-IN-QUERY-F_Compromisos Cobros.Estado ~
Cobros.Num_Credito Cobros.Val_Compromiso Cobros.Fec_Compromiso 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Compromisos Cobros.Estado 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Compromisos Cobros
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Compromisos Cobros
&Scoped-define QUERY-STRING-F_Compromisos FOR EACH Cobros SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Compromisos OPEN QUERY F_Compromisos FOR EACH Cobros SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Compromisos Cobros
&Scoped-define FIRST-TABLE-IN-QUERY-F_Compromisos Cobros


/* Definitions for FRAME F_ConMovInstancias                             */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_ConMovInstancias ~
    ~{&OPEN-QUERY-BCMI}

/* Definitions for FRAME F_ConsCred                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_ConsCred ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Definitions for FRAME F_Consulta                                     */

/* Definitions for FRAME F_Controles                                    */
&Scoped-define QUERY-STRING-F_Controles FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Controles OPEN QUERY F_Controles FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Controles Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-F_Controles Clientes


/* Definitions for FRAME F_Relaciones                                   */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 BUTTON-181 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 R_Nombre R_Nit R_Tel_Comercial 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_w_captura-pqr AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_salplas 
     LABEL "Salir" 
     SIZE 15 BY 1.35.

DEFINE BUTTON BUTTON-227 
     LABEL "Asignar Numero de Plastico" 
     SIZE 31 BY 1.88.

DEFINE BUTTON BUTTON-228 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 228" 
     SIZE 3 BY .81.

DEFINE BUTTON Btn_SC 
     LABEL "SC" 
     SIZE 15 BY 1.12 TOOLTIP "Asigna un nit provisional cuando la persona no sabe el nit de su empresa".

DEFINE BUTTON BUTTON-205 
     LABEL "Grabar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-206 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE R_Nit AS CHARACTER FORMAT "X(12)" 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Nombre AS CHARACTER FORMAT "X(40)" 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Tel_Comercial AS CHARACTER FORMAT "X(15)":U 
     LABEL "Teléfono" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON btnVolverFrmFacturaCupo 
     IMAGE-UP FILE "imagenes/volver2.bmp":U
     LABEL "Button 230" 
     SIZE 6 BY 1.5.

DEFINE BUTTON Btn_ActBasica-2 
     LABEL "Actualizar Información" 
     SIZE 19 BY 1.12.

DEFINE BUTTON Btn_BorCode 
     LABEL "Quitar Relación" 
     SIZE 19 BY 1.12.

DEFINE BUTTON Btn_CrearCodeudor 
     LABEL "Crear Codeudor" 
     SIZE 19 BY 1.12.

DEFINE BUTTON Btn_Residencia-2 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_residencia 2" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-225 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 225" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-226 
     LABEL "Salir" 
     SIZE 19 BY 1.12.

DEFINE VARIABLE WC_UbicacionComercial AS CHARACTER FORMAT "X(50)":U 
     LABEL "Dpto - Mpio" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WC_UbicacionResidencia AS CHARACTER FORMAT "X(80)":U 
     LABEL "Dpto - Mpio" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-284
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 5.12.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 5.12.

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 3.5.

DEFINE BUTTON Btn_ActBasica 
     LABEL "Actualizar Información" 
     SIZE 18 BY 1.12.

DEFINE BUTTON Btn_AdmCode 
     LABEL "Administrar Codeudores" 
     SIZE 18 BY 1.08.

DEFINE BUTTON Btn_Residencia 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 103" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-208 
     LABEL "Salir" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-216 
     LABEL "Ver Relaciones de Empresas" 
     SIZE 22 BY 1.12.

DEFINE BUTTON BUTTON-219 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 219" 
     SIZE 3 BY .54.

DEFINE VARIABLE W_UbicacionComercial AS CHARACTER FORMAT "X(50)":U 
     LABEL "Dpto - Mpio" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_UbicacionResidencia AS CHARACTER FORMAT "X(80)":U 
     LABEL "Dpto - Mpio" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 3.5.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 4.31.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 4.58.

DEFINE BUTTON BBuscar 
     LABEL "Buscar" 
     SIZE 15 BY 2.96
     FONT 1.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 121" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE FApellido1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FApellido2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FNombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CodCpto AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Código de la Gestión" 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .73
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TApellido1 AS LOGICAL INITIAL no 
     LABEL "Apellido1" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TApellido2 AS LOGICAL INITIAL no 
     LABEL "Apellido2" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE TNombre AS LOGICAL INITIAL no 
     LABEL "Nombre" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE BUTTON BUTTON-146 
     LABEL "Salir" 
     SIZE 9 BY .81.

DEFINE BUTTON BUTTON-148 
     LABEL "Imprimir Carta" 
     SIZE 24 BY 1.88
     FONT 5.

DEFINE BUTTON BUTTON-149 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 149" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Cmb_Doc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 41 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WCargo AS CHARACTER FORMAT "X(40)":U 
     LABEL "Cargo" 
     VIEW-AS FILL-IN 
     SIZE 41 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WCC AS CHARACTER FORMAT "X(100)":U 
     LABEL "C.C" 
     VIEW-AS FILL-IN 
     SIZE 41 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WFirma AS CHARACTER FORMAT "X(45)":U 
     LABEL "Firma" 
     VIEW-AS FILL-IN 
     SIZE 41 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_OutCJ 
     LABEL "Salir" 
     SIZE 12 BY 1.12.

DEFINE BUTTON BUTTON-211 
     LABEL "Asentar Cobro Jurídico" 
     SIZE 19 BY 1.12.

DEFINE BUTTON BUTTON-229 
     LABEL "Borrar Cobro Jurídico" 
     SIZE 18 BY 1.12.

DEFINE VARIABLE Nit_Abogado AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit Abogado" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81 TOOLTIP "Con doble-click, Consulta"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Abogado AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_AgrCompromiso 
     LABEL "Agregar Compromiso" 
     SIZE 17 BY 1.12.

DEFINE BUTTON Btn_SalCom 
     LABEL "Salvar Compromiso" 
     SIZE 17 BY 1.12.

DEFINE BUTTON BUTTON-184 
     IMAGE-UP FILE "IMAGENES/volver.bmp":U
     LABEL "Button 184" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-212 
     LABEL "Informe" 
     SIZE 17 BY 1.12.

DEFINE VARIABLE CAgencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Cpto AS CHARACTER FORMAT "X(45)":U 
     LABEL "Gestión (Concepto)" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "00000 - Ninguno" 
     DROP-DOWN-LIST
     SIZE 51 BY 1 TOOLTIP "Concepto-Trámite realizado"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb_estcom AS CHARACTER FORMAT "X(256)":U INITIAL "2 - Normal Desembolsado" 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 - Aprobado No Desembolsado","2 - Normal Desembolsado","3 - Cancelado","4 - Retirado sin aprobar","5 - Castigado" 
     DROP-DOWN-LIST
     SIZE 29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Ecompromiso AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 500 SCROLLBAR-VERTICAL
     SIZE 67 BY 4.04
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE REstado AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tramites", 0,
"Acuerdos", 1,
"Todos", 2
     SIZE 37 BY .54
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 1.35.

DEFINE BUTTON Btn_OutConMovIns 
     LABEL "Salir" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE Cmb_TipIns AS CHARACTER FORMAT "X(256)":U INITIAL "00000 - Todas las instancias" 
     LABEL "Instancias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - Todas las Instancias" 
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE BUTTON BUTTON-181 
     IMAGE-UP FILE "imagenes/volver.bmp":U NO-FOCUS
     LABEL "Ocultar" 
     SIZE 10 BY 1.5.

DEFINE BUTTON BLista 
     LABEL "Browser" 
     SIZE 8 BY .85.

DEFINE BUTTON Btn_Basica 
     LABEL "Actualizar" 
     SIZE 18 BY .81 TOOLTIP "Para actualizar la información básica del asociado".

DEFINE BUTTON Btn_Cert 
     LABEL "Certificados" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_CJ 
     LABEL "Cobro Juridico" 
     SIZE 14 BY .81.

DEFINE BUTTON Btn_Firma 
     IMAGE-UP FILE "adeicon/edit%.ico":U
     LABEL "Button 122" 
     SIZE 6 BY 1.35.

DEFINE BUTTON Btn_Foto 
     IMAGE-UP FILE "imagenes/dog.bmp":U
     LABEL "Button 123" 
     SIZE 6 BY 1.35.

DEFINE BUTTON Btn_HojaVida 
     IMAGE-UP FILE "imagenes/calendario.bmp":U
     LABEL "Button 126" 
     SIZE 6 BY 1.35 TOOLTIP "Hoja de Vida".

DEFINE BUTTON Btn_Imprimir 
     LABEL "Imprimir" 
     SIZE 12 BY .81.

DEFINE BUTTON Btn_PQR 
     LABEL "Gestion PQR" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_Tipos 
     LABEL "Tipos de Información" 
     SIZE 17 BY .81.

DEFINE BUTTON BUTTON-151 
     LABEL "Instancias" 
     SIZE 10 BY .85.

DEFINE VARIABLE Cmb_Tipo AS CHARACTER FORMAT "X(256)":U INITIAL "Documento" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "Documento","Nombre","Apellido1","Apellido2","Tarjeta Visionamos","Imprimir todos" 
     DROP-DOWN-LIST
     SIZE 19 BY 1
     BGCOLOR 15  DROP-TARGET NO-UNDO.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CMensajeCJ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 62 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Nombre AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 80 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE Pantalla AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 125 BY 20.73
     BGCOLOR 15 FGCOLOR 0 FONT 3 NO-UNDO.

DEFINE BUTTON btn_cambios 
     LABEL "Salvar Cambios" 
     SIZE 15 BY 1.62.

DEFINE BUTTON BUTTON-183 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 183" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE VFiscalia AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE VProcredito AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE VSuperbancaria AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-281
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 3.77.

DEFINE VARIABLE TReportado_fiscalia AS LOGICAL INITIAL no 
     LABEL "Reportado a Fiscalia" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .77 NO-UNDO.

DEFINE VARIABLE TReportado_Procredito AS LOGICAL INITIAL no 
     LABEL "Reportado Procredito" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .77 NO-UNDO.

DEFINE VARIABLE TReportado_Super AS LOGICAL INITIAL no 
     LABEL "Reportado Superbancaria" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.29 BY .77.

DEFINE BUTTON BUTTON-125 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 125" 
     SIZE 8 BY 1.62.

DEFINE IMAGE Firma
     SIZE 54 BY 5.12.

DEFINE BUTTON BUTTON-124 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 124" 
     SIZE 7 BY 1.62.

DEFINE IMAGE Foto
     SIZE 17 BY 5.12.

DEFINE BUTTON Btn_Primero 
     IMAGE-UP FILE "imagenes/dobizq.bmp":U
     LABEL "Button 143" 
     SIZE 14 BY 1.12.

DEFINE BUTTON BUTTON-127 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 127" 
     SIZE 7 BY 1.62.

DEFINE BUTTON BUTTON-128 
     IMAGE-UP FILE "imagenes/btn_fwd.bmp":U
     LABEL "Button 128" 
     SIZE 14 BY 1.12.

DEFINE BUTTON BUTTON-129 
     IMAGE-UP FILE "imagenes/btn_bck.bmp":U
     LABEL "Button 129" 
     SIZE 14 BY 1.12.

DEFINE BUTTON BUTTON-142 
     LABEL "Agregar Mensaje" 
     SIZE 18 BY 1.12.

DEFINE BUTTON BUTTON-144 
     IMAGE-UP FILE "imagenes/dobder.bmp":U
     LABEL "Button 144" 
     SIZE 14 BY 1.12.

DEFINE BUTTON BUTTON-145 
     LABEL "Buscar Mensaje" 
     SIZE 18 BY 1.12.

DEFINE VARIABLE Cmb_CodHV AS CHARACTER FORMAT "X(256)":U INITIAL "00000 - Todos los Códigos" 
     LABEL "Códigos Encontrados" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - Todos los Códigos" 
     DROP-DOWN-LIST
     SIZE 44 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Tipos AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todos los Tipos" 
     LABEL "Tipos de Mensaje" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todos los Tipos" 
     DROP-DOWN-LIST
     SIZE 44 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE HV_Doc AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE HV_Hora AS CHARACTER FORMAT "X(12)":U 
     LABEL "Hora" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE HV_Instancia AS CHARACTER FORMAT "X(999)":U 
     LABEL "Instancia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE HV_NomUsuario AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomTipo AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Codigo AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 6" 
     SIZE 9 BY 1.88.

DEFINE BUTTON BUTTON-7 
     IMAGE-UP FILE "imagenes/volver.bmp":U NO-FOCUS
     LABEL "Button 7" 
     SIZE 9 BY 1.88.

DEFINE VARIABLE R_Encabezado AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Adicionar Encabezado", 1,
"Hoja Encabezado Preimpreso", 2
     SIZE 26 BY 1.62 NO-UNDO.

DEFINE BUTTON BUTTON-213 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-214 
     LABEL "Imprimir" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE ECompromisos AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 106 BY 15.08
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_Imp 
     LABEL "&Imprimir" 
     SIZE 11.43 BY 1.08.

DEFINE BUTTON BUTTON-152 
     LABEL "&Anterior" 
     SIZE 11.86 BY 1.12.

DEFINE BUTTON BUTTON-153 
     LABEL "&Siguiente" 
     SIZE 11.86 BY 1.12.

DEFINE BUTTON BUTTON-154 
     LABEL "Salir" 
     SIZE 10 BY 1.12.

DEFINE BUTTON BUTTON-155 
     LABEL "&Consultar" 
     SIZE 11 BY 1.12.

DEFINE VARIABLE HVDes AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 990 SCROLLBAR-VERTICAL LARGE
     SIZE 66.43 BY 4.85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE HVCuenta AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cuenta/NumCre" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE HVEstado AS CHARACTER FORMAT "X(15)":U 
     LABEL "Estado" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE HVFecIng AS CHARACTER FORMAT "X(08)":U 
     LABEL "Fec.Ingreso" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE HVFecRet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fec.Retiro" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE HVNomInstancia AS CHARACTER FORMAT "X(30)":U 
     LABEL "Instancia" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE HVNSol AS CHARACTER FORMAT "X(15)":U 
     LABEL "Num.Solicitud" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE HVUsu AS CHARACTER FORMAT "X(30)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/volver.bmp":U NO-FOCUS
     LABEL "Ocultar" 
     SIZE 8.57 BY 1.62.

DEFINE VARIABLE fechaCorte AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-280
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 8.88.

DEFINE RECTANGLE RECT-317
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 8.88.

DEFINE VARIABLE IdFacturaCupoRotativo AS LOGICAL INITIAL no 
     LABEL "Extracto Cupo Rotativo" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .69 NO-UNDO.

DEFINE VARIABLE Id_AhoCanc AS LOGICAL INITIAL no 
     LABEL "Ahorros Cancelados" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.43 BY .69 NO-UNDO.

DEFINE VARIABLE Id_Ahorros AS LOGICAL INITIAL no 
     LABEL "Detalle Ahorros" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.57 BY .69 NO-UNDO.

DEFINE VARIABLE Id_Atrasos AS LOGICAL INITIAL no 
     LABEL "Atrasos" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .69 NO-UNDO.

DEFINE VARIABLE Id_Codeudando AS LOGICAL INITIAL no 
     LABEL "Codeudando A:" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.57 BY .69 NO-UNDO.

DEFINE VARIABLE Id_Controles AS LOGICAL INITIAL no 
     LABEL "Reportes y Controles" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .69 NO-UNDO.

DEFINE VARIABLE Id_CredCanc AS LOGICAL INITIAL no 
     LABEL "Créditos Canc.y Castigados" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.57 BY .69 NO-UNDO.

DEFINE VARIABLE Id_Creditos AS LOGICAL INITIAL no 
     LABEL "Detalle Créditos" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.57 BY .69 NO-UNDO.

DEFINE VARIABLE Id_Economica AS LOGICAL INITIAL no 
     LABEL "Economica" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .69 NO-UNDO.

DEFINE VARIABLE Id_Especiales AS LOGICAL INITIAL no 
     LABEL "Detalle Especiales" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.57 BY .69 NO-UNDO.

DEFINE VARIABLE Id_GarAdm AS LOGICAL INITIAL no 
     LABEL "Garantías" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .69 NO-UNDO.

DEFINE VARIABLE Id_General AS LOGICAL INITIAL yes 
     LABEL "General" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .54 NO-UNDO.

DEFINE VARIABLE Id_GestCobro AS LOGICAL INITIAL no 
     LABEL "Gestion Cobranzas" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.72 BY .69 NO-UNDO.

DEFINE VARIABLE Id_HistCreditos AS LOGICAL INITIAL no 
     LABEL "Historial Crediticio" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.57 BY .69 NO-UNDO.

DEFINE VARIABLE Id_HojaVida AS LOGICAL INITIAL no 
     LABEL "Hoja Vida" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .69 NO-UNDO.

DEFINE VARIABLE Id_Relaciones AS LOGICAL INITIAL no 
     LABEL "Relaciones" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .69 NO-UNDO.

DEFINE VARIABLE Id_Saldos AS LOGICAL INITIAL yes 
     LABEL "Solo Saldos" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.43 BY .58 NO-UNDO.

DEFINE VARIABLE Id_SimulaPago AS LOGICAL INITIAL no 
     LABEL "Simula Abono a Crédito" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.14 BY .69 NO-UNDO.

DEFINE VARIABLE Id_Solicitudes AS LOGICAL INITIAL no 
     LABEL "Detalle Solicitudes" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .69 NO-UNDO.

DEFINE VARIABLE TCompromisos AS LOGICAL INITIAL no 
     LABEL "Compromisos" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .69 NO-UNDO.

DEFINE VARIABLE tgFecCorte AS LOGICAL INITIAL no 
     LABEL "Fecha de Corte" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.86 BY .77 NO-UNDO.

DEFINE VARIABLE Tg_CtasAho AS LOGICAL INITIAL no 
     LABEL "Estado Ctas-Ahorro" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.29 BY .69 NO-UNDO.

DEFINE VARIABLE Tg_Extras AS LOGICAL INITIAL no 
     LABEL "Cuotas extras" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.72 BY .69 NO-UNDO.

DEFINE VARIABLE Tg_Plastico AS LOGICAL INITIAL no 
     LABEL "Plastico" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE BUTTON BUTTON-217 
     LABEL "Crear Nueva Relacion de Empresa" 
     SIZE 27 BY 1.12.

DEFINE BUTTON BUTTON-218 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-220 
     LABEL "Quitar Relación" 
     SIZE 27 BY 1.12.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BCMI FOR 
      THojaVida SCROLLING.

DEFINE QUERY BCobro FOR 
      TCreditos SCROLLING.

DEFINE QUERY BC_Codeudores FOR 
      Relaciones SCROLLING.

DEFINE QUERY BC_Creditos FOR 
      Creditos SCROLLING.

DEFINE QUERY BInfo FOR 
      Tmp SCROLLING.

DEFINE QUERY BRCompromisos FOR 
      Cobros SCROLLING.

DEFINE QUERY BRCreditos FOR 
      Creditos SCROLLING.

DEFINE QUERY BROWSE-1 FOR 
      Creditos SCROLLING.

DEFINE QUERY Br_Clientes FOR 
      Clientes SCROLLING.

DEFINE QUERY BR_HojaVida FOR 
      Hoja_Vida SCROLLING.

DEFINE QUERY B_Relaciones FOR 
      Relaciones SCROLLING.

DEFINE QUERY FPlastico FOR 
      Plastico SCROLLING.

DEFINE QUERY F_AdmCodeudores FOR 
      Clientes SCROLLING.

DEFINE QUERY F_Compromisos FOR 
      Cobros SCROLLING.

DEFINE QUERY F_Controles FOR 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BCMI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BCMI wWin _FREEFORM
  QUERY BCMI DISPLAY
      THojaVida.InsH
 THojaVida.AsuH
 THojaVida.DocH
 THojaVida.CueH
 THojaVida.FecH
 THojaVida.FecR
 THojaVida.Cump
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 72 BY 9.15
         FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BCobro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BCobro wWin _FREEFORM
  QUERY BCobro DISPLAY
      TCreditos.Num_Credito
 TCreditos.Sdo_Capital   FORM "->>>>,>>>,>>9.99"
 TCreditos.Nit_Juzgado   COLUMN-LABEL "Ced.Abogado"
 TCreditos.Nom_Juzgado   COLUMN-LABEL "Nombre Abogado"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 3.5
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BC_Codeudores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BC_Codeudores wWin _FREEFORM
  QUERY BC_Codeudores DISPLAY
      Relaciones.Nit_Relacion LABEL "Nit Relación"
 W_NomCodeudor LABEL "Nombre Codeudor" FORMAT "X(70)"
 Relaciones.Descripcion LABEL "Telefono"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 102 BY 2.69
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BC_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BC_Creditos wWin _FREEFORM
  QUERY BC_Creditos DISPLAY
      Creditos.Agencia
 Creditos.Cod_Credito
 Creditos.Num_Credito
 Creditos.Num_Solicitud
 Creditos.Monto
 Creditos.Sdo_Capital
 Creditos.Cuota
 Creditos.Plazo
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 102 BY 2.69
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BInfo wWin _FREEFORM
  QUERY BInfo NO-LOCK DISPLAY
      TEX FORMAT "X(132)":U COLUMN-LABEL "Informacion del Cliente"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 125 BY 21.27
         BGCOLOR 15 FONT 2 ROW-HEIGHT-CHARS .42 FIT-LAST-COLUMN.

DEFINE BROWSE BRCompromisos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRCompromisos wWin _FREEFORM
  QUERY BRCompromisos DISPLAY
      Cobros.Agencia FORMAT "999":U                 COLUMN-LABEL "Ag."
      Cobros.Fec_Acuerdo FORMAT "99/99/9999":U
      Cobros.Fec_Compromiso FORMAT "99/99/9999":U   COLUMN-LABEL "FCompromiso"
      Cobros.Nit FORMAT "X(12)":U
      Cobros.Num_Credito FORMAT "999999999":U
      Cobros.Val_Compromiso FORMAT ">>>>,>>>,>>9":U
      Cobros.Val_Cumplido   FORMAT ">>>>,>>>,>>9":U COLUMN-LABEL "Vr.Cumplido"
      Cobros.Vr_Vencido     FORMAT ">>>>,>>>,>>9":U COLUMN-LABEL "Vencido Inicial"
      Cobros.Usuario FORMAT "X(4)":U
      Cobros.Instanc                                COLUMN-LABEL "Inst."
      Cobros.Estado FORMAT "9"                      COLUMN-LABEL "Est" 
      Cobros.Usu_Recaudo                            COLUMN-LABEL "Usu.M"
      Cobros.Categoria
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 106 BY 3.5
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE BRCreditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRCreditos wWin _FREEFORM
  QUERY BRCreditos DISPLAY
      Creditos.Agencia         COLUMN-LABEL "Age"
Creditos.Cod_Credito     COLUMN-LABEL "CodCre"
Creditos.Num_Credito     COLUMN-LABEL "NumCre"
Creditos.Cuota           COLUMN-LABEL "Cuota"
Creditos.Cuo_Pagadas     COLUMN-LABEL "Pagadas"
Creditos.Fec_Desembolso  COLUMN-LABEL "FecDesembolso"
Creditos.Fec_UltPago     COLUMN-LABEL "UltPago"
Creditos.Monto           COLUMN-LABEL "Monto"
Creditos.Sdo_Capital     COLUMN-LABEL "Saldo"
Creditos.Plazo           COLUMN-LABEL "Plazo"
Creditos.Tasa            COLUMN-LABEL "Tasa"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 106 BY 3.23
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 wWin _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      Creditos.Agencia FORMAT "999":U   COLUMN-LABEL "Ag."
      Creditos.Cod_Credito FORMAT "999":U    COLUMN-LABEL "Pdcto"
      Creditos.Fec_Desembolso FORMAT "99/99/9999":U  COLUMN-LABEL "Fec-Desemb"
      Creditos.Num_Credito FORMAT "999999999":U      COLUMN-LABEL "No.Crédito"
      Creditos.Pagare FORMAT "X(14)":U               COLUMN-LABEL "Pagaré"
      Creditos.Sdo_Capital FORMAT "->>>>,>>>,>>9":U        COLUMN-LABEL "Saldo de Capital"
      Creditos.For_Pago FORMAT "9":U                       COLUMN-LABEL "F.P"
      Creditos.Fec_Pago FORMAT "99/99/9999":U              COLUMN-LABEL "F.Prox-Pago"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 62.57 BY 7.81
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Clientes wWin _FREEFORM
  QUERY Br_Clientes NO-LOCK DISPLAY
      Clientes.Agencia FORMAT "999":U
      Clientes.Nit FORMAT "X(12)":U
      Clientes.Nombre FORMAT "X(40)":U
      Clientes.Apellido1 FORMAT "X(15)":U
      Clientes.Apellido2 FORMAT "X(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 10.77
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .6 FIT-LAST-COLUMN.

DEFINE BROWSE BR_HojaVida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR_HojaVida wWin _FREEFORM
  QUERY BR_HojaVida DISPLAY
      Hoja_Vida.Tipo         LABEL "Tipo"
 Hoja_Vida.Codigo       LABEL "Codigo"
 Hoja_Vida.Usuario      LABEL "Usuario"
 Hoja_Vida.Fec_Grabacion LABEL "Fec.Gra"
 Hoja_Vida.Observacion  LABEL "Observacion"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 61 BY 4.85
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE B_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Relaciones wWin _FREEFORM
  QUERY B_Relaciones DISPLAY
      W_NomEmpresa LABEL "Nombre Empresa"
 Relaciones.Descripcion LABEL "Telefono"
ENABLE Relaciones.Descripcion
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 59 BY 4.85
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN TOOLTIP "Seleccion el campo telefono para modificar su valor directamente".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_ConsCred
     BROWSE-1 AT ROW 1.27 COL 2
     BUTTON-181 AT ROW 9.35 COL 54
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 17.57 ROW 4.19
         SIZE 65 BY 10.88
         BGCOLOR 17 
         TITLE "Créditos del Asociado - Con Doble click simula el pago".

DEFINE FRAME F_Consulta
     BInfo AT ROW 4.23 COL 2
     Btn_Basica AT ROW 3.42 COL 82.29
     Btn_CJ AT ROW 3.42 COL 68.29
     CMensajeCJ AT ROW 3.42 COL 2 NO-LABEL
     BUTTON-151 AT ROW 1.27 COL 61.29
     Btn_Cert AT ROW 2.35 COL 111.29
     Btn_HojaVida AT ROW 2.08 COL 82.29
     Btn_Firma AT ROW 2.08 COL 94.29
     Btn_Foto AT ROW 2.08 COL 88.29
     BLista AT ROW 1.27 COL 52.29
     Pantalla AT ROW 4.77 COL 2 NO-LABEL
     W_Nombre AT ROW 2.35 COL 2 NO-LABEL
     Buscar AT ROW 1.27 COL 19.29 COLON-ALIGNED NO-LABEL
     Btn_Imprimir AT ROW 1.27 COL 71.29
     Btn_Tipos AT ROW 1.27 COL 83.29
     Btn_Salir AT ROW 1.27 COL 111.29
     Cmb_Tipo AT ROW 1.27 COL 2 NO-LABEL
     Btn_PQR AT ROW 3.35 COL 111.43 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126.86 BY 24.81
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Opciones
     BUTTON-5 AT ROW 12.58 COL 52
     Id_General AT ROW 1.27 COL 4
     Id_Saldos AT ROW 1.27 COL 35
     Id_Economica AT ROW 2.35 COL 4.14
     Id_Atrasos AT ROW 2.35 COL 34.29
     Id_Ahorros AT ROW 3.12 COL 4.14
     Id_Relaciones AT ROW 3.12 COL 34.29
     Id_AhoCanc AT ROW 3.88 COL 4.14
     Id_Codeudando AT ROW 3.88 COL 34.29
     Id_Creditos AT ROW 4.65 COL 4.14
     Id_HojaVida AT ROW 4.65 COL 34.29
     Id_HistCreditos AT ROW 5.42 COL 4.14
     Id_Controles AT ROW 5.42 COL 34.29
     Id_SimulaPago AT ROW 6.19 COL 4.14
     TCompromisos AT ROW 6.19 COL 34.29
     Id_CredCanc AT ROW 6.96 COL 4.14
     Id_GestCobro AT ROW 6.96 COL 34.29
     Id_Solicitudes AT ROW 7.73 COL 4.14
     Tg_Extras AT ROW 7.73 COL 34.29
     Id_Especiales AT ROW 8.54 COL 4.14
     Tg_CtasAho AT ROW 8.58 COL 34.29
     Id_GarAdm AT ROW 9.35 COL 4
     Tg_Plastico AT ROW 9.35 COL 34.29
     IdFacturaCupoRotativo AT ROW 10.12 COL 4 WIDGET-ID 6
     tgFecCorte AT ROW 12.42 COL 2.86 WIDGET-ID 2
     FIni AT ROW 12.58 COL 35 COLON-ALIGNED
     fechaCorte AT ROW 13.31 COL 2.43 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     FFin AT ROW 13.54 COL 35 COLON-ALIGNED
     RECT-280 AT ROW 2.08 COL 2
     RECT-317 AT ROW 2.08 COL 32
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 34 ROW 3.42
         SIZE 62 BY 13.73
         BGCOLOR 17 FONT 5.

DEFINE FRAME frmFacturaCupoRotativo
     btnVolverFrmFacturaCupo AT ROW 1.27 COL 119 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.15
         SIZE 125 BY 22.35
         TITLE "FacturaCupoRotativo" WIDGET-ID 100.

DEFINE FRAME F_Compromisos
     Cmb_estcom AT ROW 1.15 COL 78 COLON-ALIGNED
     BRCreditos AT ROW 2.08 COL 3
     REstado AT ROW 5.69 COL 72 NO-LABEL
     BRCompromisos AT ROW 6.38 COL 3
     Cobros.Estado AT ROW 10.69 COL 4 HELP
          "Estado en que se encuentra la agencia" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Solo Tramite", 0,
"Acuerdo de Pago", 1
          SIZE 35 BY .81
          FONT 1
     Cmb_Cpto AT ROW 10.69 COL 55 COLON-ALIGNED
     CAgencia AT ROW 12.04 COL 14 COLON-ALIGNED
     Ecompromiso AT ROW 12.04 COL 41 NO-LABEL
     Cobros.Num_Credito AT ROW 13.12 COL 14 COLON-ALIGNED
          LABEL "Num Credito" FORMAT "999999999"
          VIEW-AS FILL-IN 
          SIZE 24 BY .73
          BGCOLOR 18 FGCOLOR 15 
     Cobros.Val_Compromiso AT ROW 14.19 COL 14 COLON-ALIGNED
          LABEL "Val Compromiso" FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 24 BY .77 TOOLTIP "Vlr.del nuevo Compromiso"
          BGCOLOR 15 FGCOLOR 0 
     Cobros.Fec_Compromiso AT ROW 15.27 COL 14 COLON-ALIGNED
          LABEL "Fecha Compromiso" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 24 BY .77
          BGCOLOR 15 FGCOLOR 0 
     BUTTON-184 AT ROW 16.62 COL 99
     Btn_AgrCompromiso AT ROW 16.88 COL 42
     Btn_SalCom AT ROW 16.88 COL 61
     BUTTON-212 AT ROW 16.88 COL 80
     "  Créditos Disponibles del Cliente" VIEW-AS TEXT
          SIZE 70 BY .81 AT ROW 1.27 COL 3
          BGCOLOR 18 FGCOLOR 15 FONT 5
     "  Compromisos del Crédito Seleccionado" VIEW-AS TEXT
          SIZE 68 BY .81 AT ROW 5.58 COL 3
          BGCOLOR 18 FGCOLOR 15 FONT 5
     RECT-303 AT ROW 10.42 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 4.23
         SIZE 110 BY 18.31
         BGCOLOR 17 FONT 4
         TITLE "Compromisos".

DEFINE FRAME F_Basica
     Clientes.Dir_Residencia AT ROW 2.35 COL 10 COLON-ALIGNED
          LABEL "Dirección"
          VIEW-AS FILL-IN 
          SIZE 41.43 BY .81
          BGCOLOR 15 
     Clientes.Tel_Residencia AT ROW 3.19 COL 10 COLON-ALIGNED
          LABEL "Teléfono"
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .81
          BGCOLOR 15 
     W_UbicacionResidencia AT ROW 4.12 COL 3.28
     Btn_Residencia AT ROW 4.19 COL 52
     BUTTON-216 AT ROW 5.38 COL 34
     W_UbicacionComercial AT ROW 7.54 COL 10 COLON-ALIGNED
     BUTTON-219 AT ROW 7.54 COL 52
     Clientes.Dir_comercial AT ROW 8.42 COL 10 COLON-ALIGNED
          LABEL "Dirección"
          VIEW-AS FILL-IN 
          SIZE 40 BY .81
          BGCOLOR 15 
     Clientes.Tel_comercial AT ROW 9.31 COL 10 COLON-ALIGNED
          LABEL "Teléfono"
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .81
          BGCOLOR 15 
     Clientes.Salario AT ROW 10.19 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .81
          BGCOLOR 15 
     Clientes.Email AT ROW 12.04 COL 6 COLON-ALIGNED
          LABEL "e-mail"
          VIEW-AS FILL-IN 
          SIZE 47 BY .81
          BGCOLOR 15 
     Clientes.Celular AT ROW 13.12 COL 31 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 
     Clientes.Fec_Nacimiento AT ROW 13.96 COL 39 COLON-ALIGNED
          LABEL "FecNacimiento"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Clientes.fecPagare AT ROW 14.88 COL 39 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Btn_ActBasica AT ROW 16.54 COL 3
     Btn_AdmCode AT ROW 17.96 COL 3
     BUTTON-208 AT ROW 18.23 COL 41
     " Información de Empresa de Covenio" VIEW-AS TEXT
          SIZE 33 BY .81 AT ROW 6.65 COL 5
          FGCOLOR 7 FONT 5
     " Residencia" VIEW-AS TEXT
          SIZE 11 BY 1.08 AT ROW 1.27 COL 5
          FGCOLOR 7 FONT 5
     RECT-282 AT ROW 1.81 COL 2
     RECT-283 AT ROW 7.19 COL 2
     RECT-302 AT ROW 11.77 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 16 ROW 2.08
         SIZE 57 BY 19.65
         BGCOLOR 17 FONT 4
         TITLE "Actualización de Información Básica".

DEFINE FRAME F_MovInstancias
     HVNomInstancia AT ROW 1.27 COL 12 COLON-ALIGNED
     HVEstado AT ROW 1.27 COL 51 COLON-ALIGNED
     HVUsu AT ROW 2.08 COL 12 COLON-ALIGNED
     HVFecIng AT ROW 2.08 COL 51 COLON-ALIGNED
     HVNSol AT ROW 2.88 COL 12 COLON-ALIGNED
     HVFecRet AT ROW 2.88 COL 51 COLON-ALIGNED
     HVCuenta AT ROW 3.69 COL 12 COLON-ALIGNED
     HVDes AT ROW 4.77 COL 1.57 NO-LABEL
     BUTTON-155 AT ROW 10.15 COL 3
     BUTTON-152 AT ROW 10.15 COL 16
     BUTTON-153 AT ROW 10.15 COL 29.72
     Btn_Imp AT ROW 10.15 COL 43.57
     BUTTON-154 AT ROW 10.15 COL 57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 44 ROW 5.04
         SIZE 68 BY 11.31
         FONT 4
         TITLE "Movimientos de Instancias".

DEFINE FRAME F_InfCompromisos
     ECompromisos AT ROW 1.54 COL 3 NO-LABEL NO-TAB-STOP 
     BUTTON-214 AT ROW 16.88 COL 79
     BUTTON-213 AT ROW 16.88 COL 94
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 4.23
         SIZE 110 BY 18.31
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Reporte de Gestion de Cartera".

DEFINE FRAME FPlastico
     Plastico.Cue_Ahorros AT ROW 1.54 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-228 AT ROW 1.54 COL 32.86
     Plastico.Num_Plastico AT ROW 2.62 COL 14 COLON-ALIGNED FORMAT ">>>>>>>>>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 20 BY .81
          BGCOLOR 15 
     BUTTON-227 AT ROW 3.96 COL 3.72
     btn_salplas AT ROW 6.12 COL 12.57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 32 ROW 3.42
         SIZE 37 BY 7.54
         BGCOLOR 17 FONT 4
         TITLE "Plástico".

DEFINE FRAME FRelNva
     R_Nombre AT ROW 1.27 COL 8 COLON-ALIGNED HELP
          "Nombre del cliente"
     R_Nit AT ROW 2.35 COL 8 COLON-ALIGNED HELP
          "Número documento de identificación"
     R_Tel_Comercial AT ROW 3.42 COL 8 COLON-ALIGNED
     BUTTON-205 AT ROW 1.27 COL 41
     BUTTON-206 AT ROW 3.69 COL 41
     Btn_SC AT ROW 2.42 COL 41
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 49 ROW 14.19
         SIZE 57 BY 4.85
         BGCOLOR 17 FONT 4
         TITLE "Empresa Nueva".

DEFINE FRAME F_Cer
     WFirma AT ROW 1.27 COL 9 COLON-ALIGNED
     WCargo AT ROW 2.35 COL 9 COLON-ALIGNED
     WCC AT ROW 3.42 COL 9 COLON-ALIGNED
     Cmb_Doc AT ROW 4.5 COL 9 COLON-ALIGNED
     BUTTON-148 AT ROW 5.58 COL 17
     BUTTON-149 AT ROW 5.58 COL 42
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 59 ROW 5.04
         SIZE 53 BY 7.81
         BGCOLOR 17 FONT 5
         TITLE "Cartas y Certificados".

DEFINE FRAME F_CobroJuridico
     BCobro AT ROW 2.08 COL 2
     Nit_Abogado AT ROW 6.65 COL 12 COLON-ALIGNED
     Nom_Abogado AT ROW 6.65 COL 25 COLON-ALIGNED NO-LABEL
     BUTTON-211 AT ROW 7.73 COL 3
     BUTTON-229 AT ROW 7.73 COL 24 WIDGET-ID 2
     Btn_OutCJ AT ROW 7.73 COL 44
     "Elija el crédito que desea mandar a cobro jurídico" VIEW-AS TEXT
          SIZE 34 BY .5 AT ROW 1.27 COL 2
     "Elija el abogado que llevará el caso del crédito" VIEW-AS TEXT
          SIZE 53 BY .5 AT ROW 5.85 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.04
         SIZE 57 BY 8.88
         BGCOLOR 17 FONT 4
         TITLE "Cobro Jurídico".

DEFINE FRAME F_ConMovInstancias
     Cmb_TipIns AT ROW 1.27 COL 8 COLON-ALIGNED
     BCMI AT ROW 2.35 COL 2
     Btn_OutConMovIns AT ROW 11.77 COL 58
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 8.81
         SIZE 74 BY 13.19
         FONT 4
         TITLE "Consulta Movimiento de Instancias".

DEFINE FRAME F_Firma
     BUTTON-125 AT ROW 6.65 COL 27
     Firma AT ROW 1.27 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 54 ROW 4.77
         SIZE 57 BY 8.35
         TITLE "Firma del Cliente".

DEFINE FRAME F_Foto
     BUTTON-124 AT ROW 6.65 COL 8
     Foto AT ROW 1.27 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 90 ROW 4.08
         SIZE 22 BY 8.35
         TITLE "Fotografia del Cliente".

DEFINE FRAME F_HojaVida
     Cmb_Tipos AT ROW 1.27 COL 20 COLON-ALIGNED
     Hoja_Vida.Tipo AT ROW 2.35 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY .81
          BGCOLOR 18 FGCOLOR 15 
     NomTipo AT ROW 2.35 COL 27 COLON-ALIGNED NO-LABEL
     Cmb_CodHV AT ROW 3.42 COL 20 COLON-ALIGNED
     Hoja_Vida.Codigo AT ROW 4.77 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Nom_Codigo AT ROW 4.77 COL 30 COLON-ALIGNED NO-LABEL
     HV_Instancia AT ROW 5.85 COL 20 COLON-ALIGNED
     HV_Doc AT ROW 5.85 COL 30 COLON-ALIGNED NO-LABEL
     Hoja_Vida.Usuario AT ROW 6.92 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     HV_NomUsuario AT ROW 6.92 COL 30 COLON-ALIGNED NO-LABEL
     Hoja_Vida.DoctoRefer AT ROW 7.73 COL 51.57 COLON-ALIGNED
          LABEL "Documento Referencia"
          VIEW-AS FILL-IN 
          SIZE 12.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Hoja_Vida.Fec_Grabacion AT ROW 8.54 COL 51.57 COLON-ALIGNED
          LABEL "Fecha"
          VIEW-AS FILL-IN 
          SIZE 12.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     HV_Hora AT ROW 9.35 COL 51.57 COLON-ALIGNED
     BUTTON-142 AT ROW 10.15 COL 2
     BUTTON-145 AT ROW 10.15 COL 20
     Hoja_Vida.Asunto_Cumplido AT ROW 10.5 COL 53
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .81
     Hoja_Vida.Observacion AT ROW 11.5 COL 2 NO-LABEL
          VIEW-AS EDITOR
          SIZE 65 BY 3.5
          BGCOLOR 15 
     BUTTON-127 AT ROW 15 COL 60
     Btn_Primero AT ROW 15.27 COL 2
     BUTTON-129 AT ROW 15.27 COL 16
     BUTTON-128 AT ROW 15.27 COL 30
     BUTTON-144 AT ROW 15.27 COL 44
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 44 ROW 5.04
         SIZE 68 BY 16.69
         BGCOLOR 17 FONT 5
         TITLE "Hoja de Vida".

DEFINE FRAME F_BuscarM
     BR_HojaVida AT ROW 1.27 COL 3
     BUTTON-146 AT ROW 6.38 COL 55
     "Seleccione el Mensaje con Doble-Click sobre la linea" VIEW-AS TEXT
          SIZE 47 BY .62 AT ROW 6.38 COL 3
          FGCOLOR 7 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 7.73
         SIZE 65 BY 7.27
         BGCOLOR 17 FONT 5
         TITLE "Busqueda de Mensajes".

DEFINE FRAME F_Browser
     Br_Clientes AT ROW 1.27 COL 3
     TNombre AT ROW 12.31 COL 7
     FNombre AT ROW 12.31 COL 18 COLON-ALIGNED NO-LABEL
     BBuscar AT ROW 12.31 COL 49
     W_CodCpto AT ROW 12.31 COL 86 COLON-ALIGNED
     TApellido1 AT ROW 13.38 COL 7
     FApellido1 AT ROW 13.38 COL 18 COLON-ALIGNED NO-LABEL
     BUTTON-121 AT ROW 13.65 COL 87
     TApellido2 AT ROW 14.46 COL 7
     FApellido2 AT ROW 14.46 COL 18 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 18 ROW 5.04
         SIZE 95 BY 15.35
         BGCOLOR 17 FONT 4
         TITLE "Resultado de la Consulta".

DEFINE FRAME F_Controles
     VProcredito AT ROW 1.54 COL 24 COLON-ALIGNED NO-LABEL
     TReportado_Procredito AT ROW 1.62 COL 3.72 HELP
          "Si es o no es reportado a Procrédito"
     TReportado_fiscalia AT ROW 2.62 COL 3.72 HELP
          "Si es o no es reportado a la Fiscalía"
     VFiscalia AT ROW 2.62 COL 24 COLON-ALIGNED NO-LABEL
     TReportado_Super AT ROW 3.69 COL 3.72 HELP
          "Si es o no es reportado por la cartera a la Superbancaria"
     VSuperbancaria AT ROW 3.69 COL 24 COLON-ALIGNED NO-LABEL
     btn_cambios AT ROW 5.31 COL 2
     BUTTON-183 AT ROW 5.31 COL 29
     RECT-281 AT ROW 1.27 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 57 ROW 6.38
         SIZE 40 BY 7
         BGCOLOR 17 FONT 4
         TITLE "Reportes y Controles".

DEFINE FRAME F_Impresion
     BUTTON-7 AT ROW 3.04 COL 21
     R_Encabezado AT ROW 1.27 COL 3 NO-LABEL
     BUTTON-6 AT ROW 3.04 COL 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 57 ROW 5.31
         SIZE 31 BY 4.31
         FONT 4.

DEFINE FRAME F_AdmCodeudores
     BC_Creditos AT ROW 2.08 COL 5
     BC_Codeudores AT ROW 5.85 COL 5
     WC_UbicacionResidencia AT ROW 9.88 COL 6.28
     Btn_Residencia-2 AT ROW 9.88 COL 50
     WC_UbicacionComercial AT ROW 9.88 COL 65 COLON-ALIGNED
     BUTTON-225 AT ROW 9.88 COL 103
     Clientes.Dir_comercial AT ROW 10.96 COL 65 COLON-ALIGNED
          LABEL "Dirección"
          VIEW-AS FILL-IN 
          SIZE 36 BY .81
          BGCOLOR 15 
     Clientes.Dir_Residencia AT ROW 11.15 COL 13 COLON-ALIGNED
          LABEL "Dirección"
          VIEW-AS FILL-IN 
          SIZE 35 BY .81
          BGCOLOR 15 
     Clientes.Tel_comercial AT ROW 12.04 COL 65 COLON-ALIGNED
          LABEL "Teléfono"
          VIEW-AS FILL-IN 
          SIZE 36 BY .81
          BGCOLOR 15 
     Clientes.Tel_Residencia AT ROW 12.31 COL 13 COLON-ALIGNED
          LABEL "Teléfono"
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .81
          BGCOLOR 15 
     Clientes.Salario AT ROW 13.12 COL 65 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 36 BY .81
          BGCOLOR 15 
     Clientes.Email AT ROW 15 COL 13 COLON-ALIGNED
          LABEL "e-mail"
          VIEW-AS FILL-IN 
          SIZE 39 BY .81
          BGCOLOR 15 
     Btn_ActBasica-2 AT ROW 15.54 COL 65
     Btn_CrearCodeudor AT ROW 15.54 COL 85
     Clientes.Celular AT ROW 16.08 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 
     Btn_BorCode AT ROW 16.88 COL 65
     BUTTON-226 AT ROW 16.88 COL 85
     Clientes.Fec_Nacimiento AT ROW 17.15 COL 30 COLON-ALIGNED
          LABEL "FecNacimiento"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 
     " Residencia" VIEW-AS TEXT
          SIZE 11 BY 1.08 AT ROW 8.81 COL 7
          FGCOLOR 7 FONT 5
     "  Relaciones de Codeudor del Credito Seleccionado" VIEW-AS TEXT
          SIZE 102 BY .81 AT ROW 5.04 COL 5
          BGCOLOR 18 FGCOLOR 15 FONT 1
     " Comercial" VIEW-AS TEXT
          SIZE 12 BY .77 AT ROW 8.92 COL 58.43
          FGCOLOR 7 FONT 5
     "  Creditos Disponibles del Cliente" VIEW-AS TEXT
          SIZE 102 BY .81 AT ROW 1.27 COL 5
          BGCOLOR 18 FGCOLOR 15 FONT 1
     RECT-284 AT ROW 9.35 COL 5
     RECT-285 AT ROW 9.35 COL 57
     RECT-304 AT ROW 14.73 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 4.23
         SIZE 110 BY 18.31
         BGCOLOR 17 FONT 4
         TITLE "Administracion de Codeudores para analistas de Cobros".

DEFINE FRAME F_Relaciones
     B_Relaciones AT ROW 1.27 COL 3
     BUTTON-217 AT ROW 6.38 COL 3
     BUTTON-220 AT ROW 7.73 COL 3
     BUTTON-218 AT ROW 8 COL 47
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
         AT COL 21 ROW 8.54
         SIZE 63 BY 9.15
         BGCOLOR 17 FONT 4
         TITLE "Relaciones".


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
         TITLE              = "Consulta Clientes"
         HEIGHT             = 24.85
         WIDTH              = 126.86
         MAX-HEIGHT         = 31.42
         MAX-WIDTH          = 164.57
         VIRTUAL-HEIGHT     = 31.42
         VIRTUAL-WIDTH      = 164.57
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
ASSIGN FRAME FPlastico:FRAME = FRAME F_Consulta:HANDLE
       FRAME FRelNva:FRAME = FRAME F_Consulta:HANDLE
       FRAME frmFacturaCupoRotativo:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_AdmCodeudores:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Basica:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Browser:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_BuscarM:FRAME = FRAME F_HojaVida:HANDLE
       FRAME F_Cer:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_CobroJuridico:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Compromisos:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_ConMovInstancias:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Controles:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Firma:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Foto:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_HojaVida:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Impresion:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_InfCompromisos:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_MovInstancias:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Opciones:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Relaciones:FRAME = FRAME F_Consulta:HANDLE.

/* SETTINGS FOR FRAME FPlastico
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FPlastico:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Plastico.Cue_Ahorros IN FRAME FPlastico
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Plastico.Num_Plastico IN FRAME FPlastico
   EXP-FORMAT                                                           */
/* SETTINGS FOR FRAME FRelNva
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME FRelNva:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN R_Nit IN FRAME FRelNva
   1                                                                    */
/* SETTINGS FOR FILL-IN R_Nombre IN FRAME FRelNva
   1                                                                    */
/* SETTINGS FOR FILL-IN R_Tel_Comercial IN FRAME FRelNva
   1                                                                    */
/* SETTINGS FOR FRAME frmFacturaCupoRotativo
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME frmFacturaCupoRotativo:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_AdmCodeudores
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BC_Creditos RECT-304 F_AdmCodeudores */
/* BROWSE-TAB BC_Codeudores BC_Creditos F_AdmCodeudores */
ASSIGN 
       FRAME F_AdmCodeudores:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Clientes.Dir_comercial IN FRAME F_AdmCodeudores
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Dir_Residencia IN FRAME F_AdmCodeudores
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Email IN FRAME F_AdmCodeudores
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_Nacimiento IN FRAME F_AdmCodeudores
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Tel_comercial IN FRAME F_AdmCodeudores
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Tel_Residencia IN FRAME F_AdmCodeudores
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN WC_UbicacionComercial IN FRAME F_AdmCodeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WC_UbicacionResidencia IN FRAME F_AdmCodeudores
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME F_Basica
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Basica:HIDDEN           = TRUE
       FRAME F_Basica:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN Clientes.Dir_comercial IN FRAME F_Basica
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Dir_Residencia IN FRAME F_Basica
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Email IN FRAME F_Basica
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_Nacimiento IN FRAME F_Basica
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Tel_comercial IN FRAME F_Basica
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Tel_Residencia IN FRAME F_Basica
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_UbicacionComercial IN FRAME F_Basica
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_UbicacionResidencia IN FRAME F_Basica
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME F_Browser
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Clientes 1 F_Browser */
ASSIGN 
       FRAME F_Browser:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FApellido1 IN FRAME F_Browser
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FApellido2 IN FRAME F_Browser
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FNombre IN FRAME F_Browser
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_CodCpto:HIDDEN IN FRAME F_Browser           = TRUE.

/* SETTINGS FOR FRAME F_BuscarM
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BR_HojaVida TEXT-5 F_BuscarM */
ASSIGN 
       FRAME F_BuscarM:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Cer
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Cer:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_CobroJuridico
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BCobro TEXT-7 F_CobroJuridico */
ASSIGN 
       FRAME F_CobroJuridico:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Nom_Abogado IN FRAME F_CobroJuridico
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Compromisos
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRCreditos Cmb_estcom F_Compromisos */
/* BROWSE-TAB BRCompromisos REstado F_Compromisos */
ASSIGN 
       FRAME F_Compromisos:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_SalCom IN FRAME F_Compromisos
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR Ecompromiso IN FRAME F_Compromisos
   DEF-FORMAT                                                           */
ASSIGN 
       Ecompromiso:HIDDEN IN FRAME F_Compromisos           = TRUE.

/* SETTINGS FOR RADIO-SET Cobros.Estado IN FRAME F_Compromisos
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN Cobros.Fec_Compromiso IN FRAME F_Compromisos
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Cobros.Num_Credito IN FRAME F_Compromisos
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Cobros.Val_Compromiso IN FRAME F_Compromisos
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FRAME F_ConMovInstancias
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BCMI Cmb_TipIns F_ConMovInstancias */
ASSIGN 
       FRAME F_ConMovInstancias:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_ConsCred
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-1 1 F_ConsCred */
ASSIGN 
       FRAME F_ConsCred:HIDDEN           = TRUE
       FRAME F_ConsCred:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Consulta
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRelNva:MOVE-AFTER-TAB-ITEM (BUTTON-151:HANDLE IN FRAME F_Consulta)
       XXTABVALXX = FRAME FRelNva:MOVE-BEFORE-TAB-ITEM (Btn_Cert:HANDLE IN FRAME F_Consulta)
       XXTABVALXX = FRAME F_Browser:MOVE-AFTER-TAB-ITEM (Pantalla:HANDLE IN FRAME F_Consulta)
       XXTABVALXX = FRAME F_Browser:MOVE-BEFORE-TAB-ITEM (W_Nombre:HANDLE IN FRAME F_Consulta)
       XXTABVALXX = FRAME F_Impresion:MOVE-AFTER-TAB-ITEM (W_Nombre:HANDLE IN FRAME F_Consulta)
       XXTABVALXX = FRAME F_Opciones:MOVE-BEFORE-TAB-ITEM (Buscar:HANDLE IN FRAME F_Consulta)
       XXTABVALXX = FRAME F_Impresion:MOVE-BEFORE-TAB-ITEM (FRAME F_Opciones:HANDLE)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BInfo 1 F_Consulta */
ASSIGN 
       BInfo:HIDDEN  IN FRAME F_Consulta                = TRUE.

ASSIGN 
       Btn_PQR:HIDDEN IN FRAME F_Consulta           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Tipo IN FRAME F_Consulta
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN CMensajeCJ IN FRAME F_Consulta
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       CMensajeCJ:HIDDEN IN FRAME F_Consulta           = TRUE.

/* SETTINGS FOR FILL-IN W_Nombre IN FRAME F_Consulta
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME F_Controles
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Controles:HIDDEN           = TRUE
       FRAME F_Controles:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN VFiscalia IN FRAME F_Controles
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN VProcredito IN FRAME F_Controles
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN VSuperbancaria IN FRAME F_Controles
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FRAME F_Firma
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Firma:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Foto
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Foto:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_HojaVida
   NOT-VISIBLE                                                          */
ASSIGN XXTABVALXX = FRAME F_BuscarM:MOVE-AFTER-TAB-ITEM (HV_NomUsuario:HANDLE IN FRAME F_HojaVida)
       XXTABVALXX = FRAME F_BuscarM:MOVE-BEFORE-TAB-ITEM (Hoja_Vida.DoctoRefer:HANDLE IN FRAME F_HojaVida)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME F_HojaVida:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX Hoja_Vida.Asunto_Cumplido IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Hoja_Vida.Codigo IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Hoja_Vida.DoctoRefer IN FRAME F_HojaVida
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Hoja_Vida.Fec_Grabacion IN FRAME F_HojaVida
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN HV_Doc IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HV_Hora IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HV_Instancia IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HV_NomUsuario IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomTipo IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Codigo IN FRAME F_HojaVida
   NO-ENABLE                                                            */
ASSIGN 
       Hoja_Vida.Observacion:READ-ONLY IN FRAME F_HojaVida        = TRUE.

/* SETTINGS FOR FILL-IN Hoja_Vida.Tipo IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Hoja_Vida.Usuario IN FRAME F_HojaVida
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Impresion
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Impresion:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_InfCompromisos
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_InfCompromisos:HIDDEN           = TRUE
       FRAME F_InfCompromisos:MOVABLE          = TRUE.

ASSIGN 
       ECompromisos:READ-ONLY IN FRAME F_InfCompromisos        = TRUE.

/* SETTINGS FOR FRAME F_MovInstancias
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_MovInstancias:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN HVCuenta IN FRAME F_MovInstancias
   NO-ENABLE                                                            */
ASSIGN 
       HVDes:READ-ONLY IN FRAME F_MovInstancias        = TRUE.

/* SETTINGS FOR FILL-IN HVEstado IN FRAME F_MovInstancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HVFecIng IN FRAME F_MovInstancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HVFecRet IN FRAME F_MovInstancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HVNomInstancia IN FRAME F_MovInstancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HVNSol IN FRAME F_MovInstancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HVUsu IN FRAME F_MovInstancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Opciones
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Opciones:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fechaCorte IN FRAME F_Opciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FFin IN FRAME F_Opciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FIni IN FRAME F_Opciones
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Id_Atrasos IN FRAME F_Opciones
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Id_Especiales IN FRAME F_Opciones
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Tg_Extras IN FRAME F_Opciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Relaciones
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_Relaciones 1 F_Relaciones */
ASSIGN 
       FRAME F_Relaciones:HIDDEN           = TRUE
       FRAME F_Relaciones:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BCMI
/* Query rebuild information for BROWSE BCMI
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH THojaVida WHERE THojaVida.CodA EQ 9.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BCMI */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BCobro
/* Query rebuild information for BROWSE BCobro
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCreditos WHERE TCreditos.Nit EQ W_CedCteC AND
                                                 TCreditos.Estado EQ 2.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BCobro */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BC_Codeudores
/* Query rebuild information for BROWSE BC_Codeudores
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Relaciones WHERE
                                 Relaciones.Cod_Relacion EQ 11  AND
                                 Relaciones.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta AND
                                 Relaciones.Clase_Producto EQ 2 AND
                                 Relaciones.Cod_Producto EQ Creditos.Cod_Credito AND
                                 Relaciones.Cuenta EQ STRING(Creditos.Num_Credito) AND
                                 Relaciones.Estado EQ 1.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BC_Codeudores */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BC_Creditos
/* Query rebuild information for BROWSE BC_Creditos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Creditos WHERE Creditos.Nit EQ TClientes.Nit.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BC_Creditos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BInfo
/* Query rebuild information for BROWSE BInfo
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Tmp NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE BInfo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRCompromisos
/* Query rebuild information for BROWSE BRCompromisos
     _START_FREEFORM
ASSIGN FRAME F_Compromisos REstado.
OPEN QUERY {&SELF-NAME}
    FOR EACH Cobros WHERE
             Cobros.Nit         EQ Clientes.Nit AND
             Cobros.Num_Credito EQ Creditos.Num_Credito AND
             Cobros.Estado      EQ REstado.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BRCompromisos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRCreditos
/* Query rebuild information for BROWSE BRCreditos
     _START_FREEFORM
OPEN QUERY BrCreditos FOR EACH Creditos WHERE Creditos.Nit EQ Clientes.Nit.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BRCreditos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Creditos WHERE
           Creditos.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta
       AND Sdo_Capital  GT 0 NO-LOCK INDEXED-REPOSITION.*/
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Clientes
/* Query rebuild information for BROWSE Br_Clientes
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Clientes NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Clientes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR_HojaVida
/* Query rebuild information for BROWSE BR_HojaVida
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Hoja_Vida WHERE
             Hoja_Vida.Nit           EQ NitWk AND
             Hoja_Vida.Tipo          GE TIni AND
             Hoja_Vida.Tipo          LE TFin AND
             Hoja_Vida.Codigo        GE CIni AND
             Hoja_Vida.Codigo        LE CFin AND
             Hoja_Vida.Fec_Grabacion GE FIni AND
             Hoja_Vida.Fec_Grabacion LE FFin USE-INDEX Idx_HojaVida NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BR_HojaVida */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Relaciones
/* Query rebuild information for BROWSE B_Relaciones
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Relaciones WHERE Relaciones.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta
     AND Relaciones.Cod_Relacion EQ 10 AND Relaciones.Estado EQ 1.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE B_Relaciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FPlastico
/* Query rebuild information for FRAME FPlastico
     _TblList          = "bdcentral.Plastico"
     _Query            is NOT OPENED
*/  /* FRAME FPlastico */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRelNva
/* Query rebuild information for FRAME FRelNva
     _Query            is NOT OPENED
*/  /* FRAME FRelNva */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmFacturaCupoRotativo
/* Query rebuild information for FRAME frmFacturaCupoRotativo
     _Query            is NOT OPENED
*/  /* FRAME frmFacturaCupoRotativo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_AdmCodeudores
/* Query rebuild information for FRAME F_AdmCodeudores
     _TblList          = "bdcentral.Clientes"
     _Query            is NOT OPENED
*/  /* FRAME F_AdmCodeudores */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Basica
/* Query rebuild information for FRAME F_Basica
     _Query            is NOT OPENED
*/  /* FRAME F_Basica */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Browser
/* Query rebuild information for FRAME F_Browser
     _Query            is NOT OPENED
*/  /* FRAME F_Browser */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_BuscarM
/* Query rebuild information for FRAME F_BuscarM
     _Query            is NOT OPENED
*/  /* FRAME F_BuscarM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cer
/* Query rebuild information for FRAME F_Cer
     _Query            is NOT OPENED
*/  /* FRAME F_Cer */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_CobroJuridico
/* Query rebuild information for FRAME F_CobroJuridico
     _Query            is NOT OPENED
*/  /* FRAME F_CobroJuridico */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Compromisos
/* Query rebuild information for FRAME F_Compromisos
     _TblList          = "bdcentral.Cobros"
     _Query            is NOT OPENED
*/  /* FRAME F_Compromisos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_ConMovInstancias
/* Query rebuild information for FRAME F_ConMovInstancias
     _Query            is NOT OPENED
*/  /* FRAME F_ConMovInstancias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_ConsCred
/* Query rebuild information for FRAME F_ConsCred
     _Query            is NOT OPENED
*/  /* FRAME F_ConsCred */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Consulta
/* Query rebuild information for FRAME F_Consulta
     _Query            is NOT OPENED
*/  /* FRAME F_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Controles
/* Query rebuild information for FRAME F_Controles
     _TblList          = "bdcentral.Clientes"
     _Query            is OPENED
*/  /* FRAME F_Controles */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Firma
/* Query rebuild information for FRAME F_Firma
     _Query            is NOT OPENED
*/  /* FRAME F_Firma */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Foto
/* Query rebuild information for FRAME F_Foto
     _Query            is NOT OPENED
*/  /* FRAME F_Foto */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_HojaVida
/* Query rebuild information for FRAME F_HojaVida
     _Query            is NOT OPENED
*/  /* FRAME F_HojaVida */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Impresion
/* Query rebuild information for FRAME F_Impresion
     _Query            is NOT OPENED
*/  /* FRAME F_Impresion */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_InfCompromisos
/* Query rebuild information for FRAME F_InfCompromisos
     _Query            is NOT OPENED
*/  /* FRAME F_InfCompromisos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_MovInstancias
/* Query rebuild information for FRAME F_MovInstancias
     _Query            is NOT OPENED
*/  /* FRAME F_MovInstancias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Opciones
/* Query rebuild information for FRAME F_Opciones
     _Query            is NOT OPENED
*/  /* FRAME F_Opciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Relaciones
/* Query rebuild information for FRAME F_Relaciones
     _Query            is NOT OPENED
*/  /* FRAME F_Relaciones */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME frmFacturaCupoRotativo:HANDLE
       ROW             = 1.12
       COLUMN          = 2
       HEIGHT          = 21.15
       WIDTH           = 116
       WIDGET-ID       = 4
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {CA8A9780-280D-11CF-A24D-444553540000} type: Pdf */
      CtrlFrame:MOVE-BEFORE(btnVolverFrmFacturaCupo:HANDLE IN FRAME frmFacturaCupoRotativo).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta Clientes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta Clientes */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Browser
&Scoped-define SELF-NAME BBuscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BBuscar wWin
ON CHOOSE OF BBuscar IN FRAME F_Browser /* Buscar */
DO:
  ASSIGN FRAME F_Browser TNombre FNombre TApellido1 FApellido1 TApellido2 FApellido2.
  IF TNombre AND NOT TApellido1 AND NOT TApellido2 THEN DO:
     OPEN QUERY Br_Clientes FOR EACH Clientes WHERE
          Clientes.Nombre CONTAINS FNombre NO-LOCK  INDEXED-REPOSITION.
  END.
  IF TNombre AND NOT TApellido1 AND TApellido2 THEN DO:
     OPEN QUERY Br_Clientes FOR EACH Clientes WHERE
          Clientes.Nombre CONTAINS FNombre AND 
          Clientes.Apellido2 CONTAINS FApellido2 NO-LOCK  INDEXED-REPOSITION.
  END.
  IF TNombre AND TApellido1 AND NOT TApellido2 THEN DO:
     OPEN QUERY Br_Clientes  FOR EACH Clientes WHERE
          Clientes.Nombre CONTAINS FNombre AND
          Clientes.Apellido1 CONTAINS FApellido1 NO-LOCK  INDEXED-REPOSITION.
  END.
  IF TNombre AND TApellido1 AND TApellido2 THEN DO:
     OPEN QUERY Br_Clientes  FOR EACH Clientes WHERE
          Clientes.Nombre CONTAINS FNombre AND
          Clientes.Apellido1 CONTAINS FApellido1 AND 
          Clientes.Apellido2 CONTAINS FApellido2 NO-LOCK  INDEXED-REPOSITION.
  END.
/**/
  IF NOT TNombre AND TApellido1 AND NOT TApellido2 THEN DO:
     OPEN QUERY Br_Clientes  FOR EACH Clientes WHERE
          Clientes.Apellido1 CONTAINS FApellido1 NO-LOCK  INDEXED-REPOSITION.
  END.
  IF NOT TNombre AND TApellido1 AND TApellido2 THEN DO:
     OPEN QUERY Br_Clientes  FOR EACH Clientes WHERE
          Clientes.Apellido1 CONTAINS FApellido1 AND 
          Clientes.Apellido2 CONTAINS FApellido2 NO-LOCK  INDEXED-REPOSITION.
  END.

/**/
  IF NOT TNombre AND NOT TApellido1 AND TApellido2 THEN DO:
     OPEN QUERY Br_Clientes  FOR EACH Clientes WHERE
          Clientes.Apellido2 CONTAINS FApellido2 NO-LOCK  INDEXED-REPOSITION.
  END.
  IF NOT TNombre AND TApellido1 AND TApellido2 THEN DO:
     OPEN QUERY Br_Clientes  FOR EACH Clientes WHERE
          Clientes.Apellido1 CONTAINS FApellido1 AND 
          Clientes.Apellido2 CONTAINS FApellido2 NO-LOCK  INDEXED-REPOSITION.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BCMI
&Scoped-define FRAME-NAME F_ConMovInstancias
&Scoped-define SELF-NAME BCMI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCMI wWin
ON MOUSE-SELECT-DBLCLICK OF BCMI IN FRAME F_ConMovInstancias
DO:
  APPLY "choose" TO Btn_OutConMovIns.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BCobro
&Scoped-define FRAME-NAME F_CobroJuridico
&Scoped-define SELF-NAME BCobro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCobro wWin
ON MOUSE-SELECT-CLICK OF BCobro IN FRAME F_CobroJuridico
DO:
   ASSIGN Nit_Abogado              = TCreditos.Nit_Juzgado
          Nit_Abogado:SCREEN-VALUE = TCreditos.Nit_Juzgado
          Nom_Abogado              = TCreditos.Nom_Juzgado
          Nom_Abogado:SCREEN-VALUE = TCreditos.Nom_Juzgado.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BC_Codeudores
&Scoped-define FRAME-NAME F_AdmCodeudores
&Scoped-define SELF-NAME BC_Codeudores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BC_Codeudores wWin
ON MOUSE-SELECT-CLICK OF BC_Codeudores IN FRAME F_AdmCodeudores
DO: 
  RUN Mostrar_Codeudor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BC_Codeudores wWin
ON ROW-DISPLAY OF BC_Codeudores IN FRAME F_AdmCodeudores
DO:
  FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN W_NomCodeudor = Clientes.Nombre + " " + Clientes.Apellido1 + Clientes.Apellido2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BC_Creditos
&Scoped-define SELF-NAME BC_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BC_Creditos wWin
ON MOUSE-SELECT-CLICK OF BC_Creditos IN FRAME F_AdmCodeudores
DO:
  
    OPEN QUERY BC_Codeudores FOR EACH Relaciones WHERE
              Relaciones.Cod_Relacion    EQ 11  AND
              Relaciones.Nit             EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta AND
              Relaciones.Clase_Producto  EQ 2 AND
              Relaciones.Cod_Producto    EQ Creditos.Cod_Credito AND
              Relaciones.Cuenta          EQ STRING(Creditos.Num_Credito) AND
              Relaciones.Estado          EQ 1.
    RUN Mostrar_Codeudor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME BLista
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BLista wWin
ON CHOOSE OF BLista IN FRAME F_Consulta /* Browser */
DO:
    IF BLista:LABEL EQ "Browser" THEN DO:
        Blista:LABEL = "Lista".
        HIDE Pantalla. 
        VIEW BROWSE Binfo.
    END.
    ELSE DO:
        Blista:LABEL = "Browser".
        HIDE BROWSE BInfo.
        VIEW Pantalla.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRCompromisos
&Scoped-define FRAME-NAME F_Compromisos
&Scoped-define SELF-NAME BRCompromisos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRCompromisos wWin
ON MOUSE-SELECT-CLICK OF BRCompromisos IN FRAME F_Compromisos
DO:
    W_RowIdCobro = ROWID(Cobros).
    RUN Mostrar_Compromiso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRCreditos
&Scoped-define SELF-NAME BRCreditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRCreditos wWin
ON MOUSE-SELECT-CLICK OF BRCreditos IN FRAME F_Compromisos
DO:
   APPLY "value-changed" TO REstado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define FRAME-NAME F_ConsCred
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 wWin
ON MOUSE-SELECT-DBLCLICK OF BROWSE-1 IN FRAME F_ConsCred
DO:
     WWin:MOVE-TO-BOTTOM().
     ASSIGN WWin:SENSITIVE = FALSE.

     RUN W-Prorec_Ordinario.R
               (INPUT "",
                INPUT Creditos.Nit,
                INPUT creditos.Cod_Credito,
                INPUT creditos.Tip_credito,
                INPUT creditos.Num_Credito,
                INPUT STRING(0,"999999999"),
                INPUT 9).

     ASSIGN WWin:SENSITIVE = TRUE.
     WWin:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Clientes
&Scoped-define FRAME-NAME F_Browser
&Scoped-define SELF-NAME Br_Clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Clientes wWin
ON MOUSE-SELECT-DBLCLICK OF Br_Clientes IN FRAME F_Browser
DO:
    /*holaedwin*/
  CASE Cmb_Tipo:SCREEN-VALUE IN FRAME F_Consulta:
      WHEN "Documento" THEN Buscar:SCREEN-VALUE = Clientes.Nit.
  END CASE.
  /*NitWk = Clientes.Nit.*/
  ASSIGN Buscar:SCREEN-VALUE = Clientes.Nit
         Cmb_Tipo:SCREEN-VALUE = Cmb_Tipo:ENTRY(1).
  RUN Cliente_Encontrado.
  APPLY "leave" TO Buscar.

  IF clientes.nombre <> "" THEN
      ASSIGN W_Nombre:SCREEN-VALUE = "Documento: " + Clientes.Nit + " - Nombre: " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  ELSE DO:
      FIND FIRST anexos_clientes WHERE anexos_clientes.nit = clientes.nit NO-LOCK NO-ERROR.
      IF AVAILABLE anexos_clientes THEN
          W_Nombre:SCREEN-VALUE = "Documento: " + Clientes.Nit + " - Nombre: " + anexos_Clientes.Nombre1 + " " + anexos_clientes.nombre2 + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  END.
  HIDE FRAME F_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR_HojaVida
&Scoped-define FRAME-NAME F_BuscarM
&Scoped-define SELF-NAME BR_HojaVida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR_HojaVida wWin
ON MOUSE-SELECT-DBLCLICK OF BR_HojaVida IN FRAME F_BuscarM
DO:
  RUN Mostrar_HV.
  HIDE FRAME F_BuscarM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmFacturaCupoRotativo
&Scoped-define SELF-NAME btnVolverFrmFacturaCupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVolverFrmFacturaCupo wWin
ON CHOOSE OF btnVolverFrmFacturaCupo IN FRAME frmFacturaCupoRotativo /* Button 230 */
DO:
    HIDE FRAME frmFacturaCupoRotativo.

    idFacturaCupoRotativo:SCREEN-VALUE IN FRAME F_Opciones = "no".
    idFacturaCupoRotativo = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basica
&Scoped-define SELF-NAME Btn_ActBasica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ActBasica wWin
ON CHOOSE OF Btn_ActBasica IN FRAME F_Basica /* Actualizar Información */
DO:
    ASSIGN FRAME F_Basica
        W_UbicacionResidencia
        W_UbicacionComercial.

    DO WITH FRAME F_Basica:
        FIND FIRST Clientes WHERE Clientes.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta NO-ERROR.
        ASSIGN FRAME F_Basica
            Clientes.DIR_Residencia
            Clientes.Tel_Residencia
            Clientes.DIR_Comercial
            Clientes.Tel_Comercial
            Clientes.Salario
            Clientes.email
            Clientes.Celular
            Clientes.Fec_Nacimiento
            W_UbicacionResidencia
            clientes.fecPagare.

        IF W_UbicacionResidencia NE "" THEN
            Clientes.Lugar_Residencia = SUBSTRING(W_UbicacionResidencia:SCREEN-VALUE,1,8).

        IF W_UbicacionComercial NE "" THEN
            Clientes.Lugar_Comercial = SUBSTRING(W_UbicacionComercial:SCREEN-VALUE,1,8).

        FIND CURRENT Clientes NO-LOCK NO-ERROR.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmCodeudores
&Scoped-define SELF-NAME Btn_ActBasica-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ActBasica-2 wWin
ON CHOOSE OF Btn_ActBasica-2 IN FRAME F_AdmCodeudores /* Actualizar Información */
DO:
ASSIGN FRAME F_AdmCodeudores WC_UbicacionResidencia WC_UbicacionComercial.
DO WITH FRAME F_Adm_Codeudores:
   FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
   IF Usuarios.Id_cartera LE 1 THEN DO:
      MESSAGE "Solo actualizan Usuarios con Usuarios.Id_cartera GE 2"
         VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.   

   FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-ERROR.
   IF AVAILABLE Clientes THEN DO:
       ASSIGN FRAME F_AdmCodeudores
              Clientes.DIR_Residencia
              Clientes.Tel_Residencia
              Clientes.DIR_Comercial
              Clientes.Tel_Comercial
              Clientes.Salario
              Clientes.email
              Clientes.Celular
              Clientes.Fec_Nacimiento
              WC_UbicacionResidencia
              WC_UbicacionComercial.
       IF WC_UbicacionResidencia NE "" THEN
          Clientes.Lugar_Residencia = SUBSTRING(WC_UbicacionResidencia:SCREEN-VALUE,1,8).
       IF WC_UbicacionComercial NE "" THEN
          Clientes.Lugar_Comercial = SUBSTRING(WC_UbicacionComercial:SCREEN-VALUE,1,8).
   END.
   FIND CURRENT Clientes NO-LOCK NO-ERROR.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basica
&Scoped-define SELF-NAME Btn_AdmCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AdmCode wWin
ON CHOOSE OF Btn_AdmCode IN FRAME F_Basica /* Administrar Codeudores */
DO:
  IF Usuarios.Id_cartera LE 1 THEN DO:
      MESSAGE "Solo actualizan Usuarios con Usuarios.Id_cartera GE 2"
         VIEW-AS ALERT-BOX ERROR.
      RETURN.
  END.
  FIND Clientes WHERE Clientes.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta.
  OPEN QUERY BC_Creditos FOR EACH Creditos WHERE Creditos.Nit EQ Clientes.Nit AND
             Creditos.Estado EQ 2 AND Creditos.Sdo_Capital GT 0.
  OPEN QUERY BC_Codeudores FOR EACH Relaciones WHERE
                                 Relaciones.Cod_Relacion EQ 11  AND
                                 Relaciones.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta AND
                                 Relaciones.Clase_Producto EQ 2 AND
                                 Relaciones.Cod_Producto EQ Creditos.Cod_Credito AND
                                 Relaciones.Cuenta EQ STRING(Creditos.Num_Credito) AND
                                 Relaciones.Estado EQ 1.
  WRel = 11.
  RUN Mostrar_Codeudor.
  VIEW FRAME F_AdmCodeudores.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromisos
&Scoped-define SELF-NAME Btn_AgrCompromiso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AgrCompromiso wWin
ON CHOOSE OF Btn_AgrCompromiso IN FRAME F_Compromisos /* Agregar Compromiso */
DO:
    DISABLE Btn_AgrCompromiso WITH FRAME F_Compromisos.
    ENABLE Btn_SalCom WITH FRAME F_Compromisos.
    DEFINE VAR W_Age LIKE Creditos.Agencia.
    DEFINE VAR W_Pro LIKE Creditos.Cod_Credito.
    DEFINE VAR W_Cue LIKE Creditos.Num_Credito.  

    W_SiIngr = YES.        
    IF Clientes.Nit GT "0" THEN DO:
       RUN Inicializar_Compromiso.
       /*ENABLE {&List-1} WITH FRAME F_Compromisos.*/
       APPLY "ENTRY" TO CAgencia. 
    END.
    ELSE
       MESSAGE "Debe Teclear o Consultar una Cedula/Nit, para Ingresar Compromisos."
           VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_Basica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Basica wWin
ON CHOOSE OF Btn_Basica IN FRAME F_Consulta /* Actualizar */
DO:
FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
DO WITH FRAME F_Basica:
    ASSIGN W_UbicacionResidencia:SCREEN-VALUE = ""
           W_UbicacionResidencia = "".

    FIND FIRST Clientes WHERE Clientes.Nit EQ W_CedCteC NO-LOCK NO-ERROR.
    
    ASSIGN Clientes.DIR_Residencia:SCREEN-VALUE = Clientes.DIR_Residencia
           Clientes.Tel_Residencia:SCREEN-VALUE = Clientes.Tel_Residencia
           Clientes.DIR_Comercial:SCREEN-VALUE = Clientes.DIR_Comercial
           Clientes.Tel_Comercial:SCREEN-VALUE = Clientes.Tel_Comercial
           Clientes.Salario:SCREEN-VALUE = STRING(Clientes.Salario)
           Clientes.email:SCREEN-VALUE = Clientes.email
           Clientes.Celular:SCREEN-VALUE = Clientes.Celular
           Clientes.Fec_Nacimiento:SCREEN-VALUE = STRING(Clientes.Fec_Nacimiento)
           clientes.fecPagare:SCREEN-VALUE = STRING(clientes.fecPagare).

    DEFINE VAR WN AS CHARACTER FORMAT "X(8)".

    IF Clientes.Lugar_Residencia NE "" THEN DO:
      FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (SUBSTRING(Clientes.Lugar_Residencia,1,2) + "000000") NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN
         ASSIGN WN = SUBSTRING(Clientes.Lugar_Residencia,1,2)
                W_UbicacionResidencia = W_UbicacionResidencia + " " + Ubicacion.Nombre.
      FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (WN + SUBSTRING(Clientes.Lugar_Residencia,3,3) + "000") NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN
         ASSIGN WN = WN + SUBSTRING(Clientes.Lugar_Residencia,3,3)
                W_UbicacionResidencia = W_UbicacionResidencia + " " + Ubicacion.Nombre.
      FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (WN + SUBSTRING(Clientes.Lugar_Residencia,6,3)) NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN
         ASSIGN WN = WN + SUBSTRING(Clientes.Lugar_Residencia,6,3)
                W_UbicacionResidencia = W_UbicacionResidencia + " " + Ubicacion.Nombre.
      W_UbicacionResidencia:SCREEN-VALUE = WN + " - " + W_UbicacionResidencia.
   END.
   IF Clientes.Lugar_Comercial NE "" THEN DO:
      FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (SUBSTRING(Clientes.Lugar_Comercial,1,2) + "000000") NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN
         ASSIGN WN = SUBSTRING(Clientes.Lugar_Comercial,1,2)
                W_UbicacionComercial = W_UbicacionComercial + " " + Ubicacion.Nombre.
      FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (WN + SUBSTRING(Clientes.Lugar_Comercial,3,3) + "000") NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN
         ASSIGN WN = WN + SUBSTRING(Clientes.Lugar_Comercial,3,3)
                W_UbicacionComercial = W_UbicacionComercial + " " + Ubicacion.Nombre.
      FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (WN + SUBSTRING(Clientes.Lugar_Comercial,6,3)) NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN
         ASSIGN WN = WN + SUBSTRING(Clientes.Lugar_Comercial,6,3)
                W_UbicacionComercial = W_UbicacionComercial + " " + Ubicacion.Nombre.
      W_UbicacionComercial:SCREEN-VALUE = WN + " - " + W_UbicacionComercial.
   END.

END.
  VIEW FRAME F_Basica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmCodeudores
&Scoped-define SELF-NAME Btn_BorCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_BorCode wWin
ON CHOOSE OF Btn_BorCode IN FRAME F_AdmCodeudores /* Quitar Relación */
DO:
  FIND CURRENT Relaciones NO-ERROR.
  IF AVAILABLE Relaciones THEN
     Relaciones.Estado = 2.
  APPLY "choose" TO Btn_AdmCode IN FRAME F_Basica.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Controles
&Scoped-define SELF-NAME btn_cambios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_cambios wWin
ON CHOOSE OF btn_cambios IN FRAME F_Controles /* Salvar Cambios */
DO:
  
  FIND Ahorros WHERE 
           Ahorros.Tip_Ahorro EQ 4 
       AND Ahorros.Cod_Ahorro EQ 1 
       AND Ahorros.Nit EQ Clientes.Nit NO-LOCK NO-ERROR.
  ASSIGN FRAME F_Controles TReportado_Super TReportado_Fiscalia TReportado_Procredito
               VProcredito VFiscalia VSuperbancaria.
  IF AVAILABLE Ahorros THEN DO:
      IF TReportado_Procredito NE Clientes.Reportado_Procredito THEN DO:
          Clientes.Reportado_Procredito = TReportado_Procredito.          
          CREATE Mov_Ahorros.
          ASSIGN Mov_Ahorros.Cod_Operacion = 999999999
                 Mov_ahorros.cod_ahorro    = Ahorros.Cod_Ahorro
                 Mov_Ahorros.Cue_Ahorros   = Ahorros.Cue_Ahorros
                 Mov_ahorros.nit           = Ahorros.Nit
                 Mov_Ahorros.Fecha         = TODAY
                 Mov_Ahorros.Hora          = TIME
                 Mov_Ahorros.Agencia       = Ahorros.Agencia
                 Mov_Ahorros.Age_Fuente    = W_Agencia
                 Mov_Ahorros.Age_Destino   = W_Agencia
                 Mov_Ahorros.Usuario       = W_Usuario
                 Mov_Ahorros.Val_Efectivo  = 0.
          IF TReportado_Procredito THEN
             Mov_Ahorros.Descrip       = "PROCREDITO: Valor Reportado: " + STRING(VProcredito,">>>,>>>,>>9").
          ELSE
             ASSIGN Mov_Ahorros.Descrip       = "PROCREDITO: Se levanta el reporte".
      END.
      IF TReportado_Fiscalia NE Clientes.Reportado_Fiscalia THEN DO:
          Clientes.Reportado_Fiscalia   = TReportado_Fiscalia.
          CREATE Mov_Ahorros.
          ASSIGN Mov_Ahorros.Cod_Operacion = 999999999
                 Mov_ahorros.cod_ahorro    = Ahorros.Cod_Ahorro
                 Mov_Ahorros.Cue_Ahorros   = Ahorros.Cue_Ahorros
                 Mov_ahorros.nit           = Ahorros.Nit
                 Mov_Ahorros.Fecha         = TODAY
                 Mov_Ahorros.Hora          = TIME
                 Mov_Ahorros.Agencia       = Ahorros.Agencia
                 Mov_Ahorros.Age_Fuente    = W_Agencia
                 Mov_Ahorros.Age_Destino   = W_Agencia
                 Mov_Ahorros.Usuario       = W_Usuario
                 Mov_Ahorros.Val_Efectivo  = 0.
          IF TReportado_Fiscalia THEN
             Mov_Ahorros.Descrip       = "FISCALIA: Valor Reportado: " + STRING(VFiscalia,">>>,>>>,>>9").
          ELSE
             ASSIGN Mov_Ahorros.Descrip       = "FISCALIA: Se levanta el reporte".
      END.
      IF TReportado_Super NE Clientes.Reportado_Super THEN DO:
          Clientes.Reportado_Super      = TReportado_Super.
          CREATE Mov_Ahorros.
          ASSIGN Mov_Ahorros.Cod_Operacion = 999999999
                 Mov_ahorros.cod_ahorro    = Ahorros.Cod_Ahorro
                 Mov_Ahorros.Cue_Ahorros   = Ahorros.Cue_Ahorros
                 Mov_ahorros.nit           = Ahorros.Nit
                 Mov_Ahorros.Fecha         = TODAY
                 Mov_Ahorros.Hora          = TIME
                 Mov_Ahorros.Agencia       = Ahorros.Agencia
                 Mov_Ahorros.Age_Fuente    = W_Agencia
                 Mov_Ahorros.Age_Destino   = W_Agencia
                 Mov_Ahorros.Usuario       = W_Usuario
                 Mov_Ahorros.Val_Efectivo  = 0.
          IF TReportado_Super THEN
             Mov_Ahorros.Descrip       = "SUPERBANCARIA: Valor Reportado: " + STRING(VSuperbancaria,">>>,>>>,>>9").
          ELSE
             ASSIGN Mov_Ahorros.Descrip       = "SUPERBANCARIA: Se levanta el reporte".
      END.
      FIND CURRENT Clientes NO-LOCK.
  END.
  ELSE DO:
     IF TReportado_Procredito NE Clientes.Reportado_Procredito THEN
        Clientes.Reportado_Procredito = TReportado_Procredito.          
     IF TReportado_Fiscalia NE Clientes.Reportado_Fiscalia THEN
        Clientes.Reportado_Fiscalia   = TReportado_Fiscalia.
     IF TReportado_Super NE Clientes.Reportado_Super THEN
        Clientes.Reportado_Super      = TReportado_Super.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_Cert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cert wWin
ON CHOOSE OF Btn_Cert IN FRAME F_Consulta /* Certificados */
DO:
  Cmb_Doc:LIST-ITEMS IN FRAME F_Cer = "".
     FOR EACH Documentos NO-LOCK:
        W_Ok = Cmb_Doc:ADD-LAST(Documentos.Nombre) IN FRAME F_Cer.
     END.
  IF Cmb_Doc:LIST-ITEMS IN FRAME F_Cer NE "" THEN
     Cmb_Doc:SCREEN-VALUE IN FRAME F_Cer = Cmb_Doc:ENTRY(1).
  VIEW FRAME F_Cer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_CJ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CJ wWin
ON CHOOSE OF Btn_CJ IN FRAME F_Consulta /* Cobro Juridico */
DO:
  OPEN QUERY BCobro FOR EACH TCreditos WHERE TCreditos.Nit EQ Clientes.Nit AND
                                                 TCreditos.Sdo_Capital GT 0.
  VIEW FRAME F_CobroJuridico.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmCodeudores
&Scoped-define SELF-NAME Btn_CrearCodeudor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CrearCodeudor wWin
ON CHOOSE OF Btn_CrearCodeudor IN FRAME F_AdmCodeudores /* Crear Codeudor */
DO:
  FRAME FRelNva:TITLE = "Creación de Codeudor".
  VIEW FRAME FRelNva.
  WRel = 11.
  APPLY "entry" TO R_Nombre IN FRAME FRelNva.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_Firma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Firma wWin
ON CHOOSE OF Btn_Firma IN FRAME F_Consulta /* Button 122 */
DO:
  /*W_Ok = Firma:LOAD-IMAGE(gTexto) IN FRAME F_Firma NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
     MESSAGE "No Existe el Archivo JPG asociado al cliente" VIEW-AS ALERT-BOX.
  ELSE
     VIEW FRAME F_Firma.  */
  IF TClientes.Nit EQ ""  THEN
     MESSAGE "No se ha elegido ninguna cuenta o nit de trabajo" SKIP
             VIEW-AS ALERT-BOX INFORMATION.
  ELSE DO:
     ASSIGN WWin:SENSITIVE = FALSE.
     
     RUN C-Firma.r (INPUT TClientes.Nit, INPUT 0, INPUT 0, INPUT 0).
     
     ASSIGN WWin:SENSITIVE = TRUE.
     APPLY "entry" TO Btn_Firma IN FRAME F_Consulta.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Foto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Foto wWin
ON CHOOSE OF Btn_Foto IN FRAME F_Consulta /* Button 123 */
DO:
  gTexto = "imagenes\fotos\" + TRIM(Buscar:SCREEN-VALUE IN FRAME F_Consulta) + ".jpg".
  W_Ok = Foto:LOAD-IMAGE(gTexto) IN FRAME F_Foto NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
     MESSAGE "No Existe el Archivo JPG asociado al cliente" VIEW-AS ALERT-BOX.
  ELSE
     VIEW FRAME F_Foto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_HojaVida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_HojaVida wWin
ON CHOOSE OF Btn_HojaVida IN FRAME F_Consulta /* Button 126 */
DO:
ASSIGN FRAME F_Opciones Fini FFin.
DO WITH FRAME F_HojaVida:
  FIND FIRST Hoja_Vida WHERE 
             Hoja_Vida.Nit EQ NitWk AND
             Hoja_Vida.Fec_Grabacion GE FIni AND
             Hoja_Vida.Fec_Grabacion LE FFin USE-INDEX Idx_HojaVida NO-LOCK NO-ERROR.
  IF AVAILABLE Hoja_Vida THEN 
     APPLY "value-changed" TO Cmb_Tipos IN FRAME F_HojaVida.

  VIEW FRAME F_HojaVida.
  
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_MovInstancias
&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp wWin
ON CHOOSE OF Btn_Imp IN FRAME F_MovInstancias /* Imprimir */
DO:
  DEFINE VAR Listado AS CHARACTER FORM "X(35)" INITIAL "".
  ASSIGN Listado   = W_Pathspl + "ComentInst-" + W_Usuario + STRING(RANDOM(2000,10000)) + ".lst"
         W_SiMInst = TRUE.

  {Incluido/Imprimir.i "Listado"}.

  W_SiMInst = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME F_Consulta /* Imprimir */
DO:
  VIEW FRAME F_Impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_CobroJuridico
&Scoped-define SELF-NAME Btn_OutCJ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutCJ wWin
ON CHOOSE OF Btn_OutCJ IN FRAME F_CobroJuridico /* Salir */
DO:
  APPLY "leave" TO Buscar IN FRAME F_Consulta.
  HIDE FRAME F_CobroJuridico.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConMovInstancias
&Scoped-define SELF-NAME Btn_OutConMovIns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConMovIns wWin
ON CHOOSE OF Btn_OutConMovIns IN FRAME F_ConMovInstancias /* Salir */
DO:
    DO WITH FRAME F_MovInstancias:
        IF AVAILABLE THojaVida THEN DO:
            ASSIGN HvNomInstancia:SCREEN-VALUE = THojaVida.AsuH
                   HvUsu:SCREEN-VALUE          = STRING(THojaVida.UsuH)
                   HvNSol:SCREEN-VALUE         = STRING(THojaVida.DocH)
                   HvEstado:SCREEN-VALUE       = STRING(THojaVida.Cump,"si/no")
                   HvCuenta:SCREEN-VALUE       = STRING(THojaVida.CueH)
                   HvFecIng:SCREEN-VALUE       = STRING(THojaVida.FecH)
                   HvFecRet:SCREEN-VALUE       = STRING(THojaVida.FecR)
                   HvDes:SCREEN-VALUE          = STRING(LOWER(THojaVida.DesH)).
        END.
     END. 
     HIDE FRAME F_ConMovInstancias.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_PQR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_PQR wWin
ON CHOOSE OF Btn_PQR IN FRAME F_Consulta /* Gestion PQR */
DO:
  RUN Creacion-Pqr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Btn_Primero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Primero wWin
ON CHOOSE OF Btn_Primero IN FRAME F_HojaVida /* Button 143 */
DO:
ASSIGN FRAME F_HojaVida Cmb_Tipos Cmb_CodHV.
IF INTEGER(SUBSTRING(Cmb_Tipos,1,3)) EQ 0 THEN ASSIGN TIni = 0 TFin = 999.
ELSE ASSIGN TIni = INTEGER(SUBSTRING(Cmb_Tipos,1,3)) TFin = INTEGER(SUBSTRING(Cmb_Tipos,1,3)).
IF INTEGER(SUBSTRING(Cmb_CodHV,1,5)) EQ 0 THEN ASSIGN CIni = 0 CFin = 99999.
ELSE ASSIGN CIni = INTEGER(SUBSTRING(Cmb_CodHV,1,5)) CFin = INTEGER(SUBSTRING(Cmb_CodHV,1,5)).

DO WITH FRAME F_HojaVida:
  FIND FIRST  Hoja_Vida WHERE 
             Hoja_Vida.Nit           EQ NitWk AND
             Hoja_Vida.Tipo          GE TIni AND
             Hoja_Vida.Tipo          LE TFin AND
             Hoja_Vida.Codigo        GE CIni AND
             Hoja_Vida.Codigo        LE CFin AND
             Hoja_Vida.Fec_Grabacion GE FIni AND
             Hoja_Vida.Fec_Grabacion LE FFin USE-INDEX Idx_HojaVida NO-LOCK NO-ERROR.
  IF AVAILABLE Hoja_Vida THEN
     RUN Mostrar_HV.
END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basica
&Scoped-define SELF-NAME Btn_Residencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Residencia wWin
ON CHOOSE OF Btn_Residencia IN FRAME F_Basica /* Button 103 */
DO:
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
  IF INTEGER(P_Ubi) EQ 0 THEN DO:
     MESSAGE "No fue encontrada ninguna ubicación" VIEW-AS ALERT-BOX.
  END.

  ASSIGN W_UbicacionResidencia:SCREEN-VALUE = STRING(P_Ubi,"99999999") + " - " + LC(P_NUbi).
    /*MESSAGE clientes.lugar_residencia:SCREEN-VALUE p_ubi VIEW-AS ALERT-BOX.    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmCodeudores
&Scoped-define SELF-NAME Btn_Residencia-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Residencia-2 wWin
ON CHOOSE OF Btn_Residencia-2 IN FRAME F_AdmCodeudores /* Btn_residencia 2 */
DO:
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
  IF INTEGER(P_Ubi) EQ 0 THEN DO:
     MESSAGE "No fue encontrada ninguna ubicación" VIEW-AS ALERT-BOX.
  END.

  ASSIGN WC_UbicacionResidencia:SCREEN-VALUE = STRING(P_Ubi,"99999999") + " - " + LC(P_NUbi).
    /*MESSAGE clientes.lugar_residencia:SCREEN-VALUE p_ubi VIEW-AS ALERT-BOX.    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromisos
&Scoped-define SELF-NAME Btn_SalCom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalCom wWin
ON CHOOSE OF Btn_SalCom IN FRAME F_Compromisos /* Salvar Compromiso */
DO:
    ASSIGN ECompromiso.     
    IF ECompromiso LE " " OR Clientes.Nit LE "0" THEN DO:
       MESSAGE "Falta la Cedula/Nit Y/O el Comentario del Compromiso/Gestion."
           VIEW-AS ALERT-BOX ERROR.
       RETURN.
    END.
    IF INTEGER(SUBSTRING(Cmb_Cpto:SCREEN-VALUE IN FRAME F_Compromisos,1,5)) EQ 0 THEN DO:
       MESSAGE "Debe escogerse uno de los conceptos" VIEW-AS ALERT-BOX.
       APPLY "entry" TO Cmb_Cpto.
       RETURN NO-APPLY.
    END.
    IF W_SiIngr THEN DO: 
       IF Cobros.Estado:SCREEN-VALUE EQ "1" THEN DO:
           IF (Cobros.Fec_Compromiso:SCREEN-VALUE LE " " OR Cobros.Fec_Compromiso:SCREEN-VALUE EQ ?) 
           AND DEC(Cobros.Val_Compromiso:SCREEN-VALUE)  LE 0 THEN DO:
               IF W_CodCpto LE 0 THEN DO:
                  MESSAGE "Falta el Concepto de la Gestion."
                        VIEW-AS ALERT-BOX ERROR.
                  RETURN.
               END.
           END.
           ELSE IF Cobros.Fec_Compromiso:SCREEN-VALUE LE " " OR Cobros.Fec_Compromiso:SCREEN-VALUE EQ ? 
           OR DEC(Cobros.Val_Compromiso:SCREEN-VALUE)  LE 0           
           OR DATE(Cobros.Fec_Compromiso:SCREEN-VALUE) LT W_Fecha THEN DO:
              MESSAGE "Falta el Valor Y/O la Fecha del Compromiso(Igual O Posterior a Hoy)."
                   VIEW-AS ALERT-BOX ERROR.
              RETURN.
           END.
       END.

       FIND FIRST Creditos WHERE Creditos.Nit     EQ Clientes.Nit
              AND Creditos.Num_Credito EQ INTEG(Cobros.Num_Credito:SCREEN-VALUE)
              AND Creditos.Sdo_Capital GT 0 NO-LOCK NO-ERROR.
       IF NOT AVAIL(Creditos) THEN DO:
          MESSAGE "No existe el Credito vigente con Sdo-Capital para asignarle el Compromiso."
              VIEW-AS ALERT-BOX ERROR.
          RETURN.
       END.

       CREATE Cobros.
       ASSIGN Cobros.Nro_Cobro        = NEXT-VALUE(Sec_NumCobro)
              Cobros.Agencia          = Creditos.Agencia
              Cobros.Estado           = INTEGER(Cobros.Estado:SCREEN-VALUE)
              Cobros.Fec_Acuerdo      = W_Fecha
              Cobros.Fec_Compromiso   = DATE(Cobros.Fec_Compromiso:SCREEN-VALUE)
              Cobros.Nit              = Clientes.Nit
              Cobros.Num_Credito      = INTEG(Cobros.Num_Credito:SCREEN-VALUE)
              Cobros.Observacion      = ECompromiso
              Cobros.Usuario          = W_Usuario
              Cobros.Val_Compromiso   = DEC(Cobros.Val_Compromiso:SCREEN-VALUE)
              Cobros.Categoria        = Creditos.Categoria
              Cobros.Cod_Credito      = Creditos.Cod_Credito
              Cobros.Cuo_Vencidas     = Creditos.Cuo_Atraso 
              Cobros.Dias_Vencido     = Creditos.Dias_Atraso
              Cobros.Hora_Grabacion   = TIME
              Cobros.Sdo_Capital      = Creditos.Sdo_Capital
              Cobros.Vr_Vencido       = Creditos.Val_Atraso
              Cobros.Cod_Tipo         = INTEGER(SUBSTRING(Cmb_Cpto:SCREEN-VALUE IN FRAME F_Compromisos,1,5)).



       IF Cobros.Val_Compromiso LE 0 THEN
          ASSIGN Cobros.Fec_Compromiso = W_Fecha
                 Cobros.Estado         = 0.
                 

       FIND FIRST Mov_Inst WHERE Mov_Inst.Nit           EQ Creditos.Nit                 
                             AND Mov_Inst.Cuenta        EQ STRING(Creditos.Num_Credito) 
                             AND Mov_Inst.Estado        EQ NO                           
                             AND Mov_Inst.Num_Solicitud EQ Creditos.Num_Solicitud NO-LOCK NO-ERROR.
       IF AVAIL(Mov_Inst) THEN 
          Cobros.Instancia = Mov_Inst.Instancia. 
    END.
    ELSE DO:
       FIND FIRST Cobros WHERE ROWID(Cobros) EQ W_RowIdCobro NO-ERROR.
       IF NOT AVAIL(Cobros) THEN DO:
          MESSAGE "Debe seleccionar un Compromiso Valido."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
       END.

       IF Cobros.Estado EQ 0 OR Cobros.Estado EQ 2 THEN DO:
          MESSAGE "Compromisos Pagados o Gestiones de Cobranza, no se permiten modificar."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
       END.

       ASSIGN Cobros.Estado      = INTEG(Cobros.Estado:SCREEN-VALUE)
              Cobros.Usu_Recaudo = W_Usuario
              Cobros.Observacion = Cobros.Observacion + "; F-" + STRING(W_Fecha,"999999") + ": " + ECompromiso.
       FIND CURRENT Cobros NO-LOCK NO-ERROR.
    END.

    CLOSE QUERY BrCompromisos.

    /*RUN Compromi_Cte.  */
    APPLY "value-changed" TO REstado.
    
    ASSIGN W_SiIngr = FALSE.
    ENABLE Btn_AgrCompromiso WITH FRAME F_Compromisos.  
    DISABLE Btn_SalCom WITH FRAME F_Compromisos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Consulta /* Salir */
DO:
  W_NitGlobal = "".
/*  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE 
      RUN exitObject.*/
  /*  &ENDIF
  &ELSE */
      APPLY "CLOSE":U TO THIS-PROCEDURE. /*
  &ENDIF */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FPlastico
&Scoped-define SELF-NAME btn_salplas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_salplas wWin
ON CHOOSE OF btn_salplas IN FRAME FPlastico /* Salir */
DO:
  HIDE FRAME Fplastico.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME Btn_SC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SC wWin
ON CHOOSE OF Btn_SC IN FRAME FRelNva /* SC */
DO:
  R_Nit:SCREEN-VALUE = "SC_" + STRING(NEXT-VALUE(Sec_NitAuto)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_Tipos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Tipos wWin
ON CHOOSE OF Btn_Tipos IN FRAME F_Consulta /* Tipos de Información */
DO:
  VIEW FRAME F_Opciones.

  ASSIGN Id_Creditos = FALSE
         Id_Creditos:SCREEN-VALUE = "No"

         Id_AhoCanc = FALSE
         Id_AhoCanc:SCREEN-VALUE = "No"                
         
         Id_Solicitudes = FALSE
         Id_Solicitudes:SCREEN-VALUE = "No"
         
         Id_CredCanc = FALSE
         Id_CredCanc:SCREEN-VALUE = "No"
                       
         Id_GarAdm = FALSE
         Id_GarAdm:SCREEN-VALUE = "No"
         
         Id_Relaciones = FALSE
         Id_Relaciones:SCREEN-VALUE = "No"
                     
         Id_Codeudando = FALSE
         Id_Codeudando:SCREEN-VALUE = "No"
                         
         Id_HojaVida = FALSE
         Id_HojaVida:SCREEN-VALUE = "No" 

         Id_GestCobro = FALSE
         Id_GestCobro:SCREEN-VALUE = "No"

         idFacturaCupoRotativo = FALSE
         idFacturaCupoRotativo:SCREEN-VALUE = "no".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Buscar wWin
ON ENTRY OF Buscar IN FRAME F_Consulta
DO:
  HIDE FRAME F_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Buscar wWin
ON LEAVE OF Buscar IN FRAME F_Consulta
DO:
    DEFINE VAR i AS INTEGER.

    FOR EACH TSaldos WHERE TSaldos.Cla EQ 0:
        DELETE TSaldos.
    END.

    CmensajeCJ:SCREEN-VALUE IN FRAME F_Consulta = "                            ".

    DO WITH FRAME F_Consulta:
        IF Buscar:SCREEN-VALUE NE "" THEN DO:
            CASE Cmb_Tipo:SCREEN-VALUE:
                WHEN "Documento" THEN DO:
                    FIND FIRST Clientes WHERE Clientes.Nit BEGINS Buscar:SCREEN-VALUE NO-LOCK NO-ERROR.
                    IF AVAILABLE Clientes THEN
                        RUN Cliente_Encontrado.
                    ELSE DO:
                        W_Nombre:SCREEN-VALUE = "".

                        IF ERROR-STATUS:GET-NUMBER(1) EQ 3166 THEN DO:
                            OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Nit BEGINS Buscar:SCREEN-VALUE NO-LOCK INDEXED-REPOSITION.
                            VIEW FRAME F_Browser.
                        END.
                    END.
                END.

                WHEN "Nombre" THEN DO:
                    ASSIGN TNombre:SCREEN-VALUE IN FRAME F_Browser = "YES"
                           FNombre:SCREEN-VALUE IN FRAME F_Browser = SELF:SCREEN-VALUE IN FRAME F_Consulta
                           TApellido1:SCREEN-VALUE IN FRAME F_Browser = "no"
                           TApellido2:SCREEN-VALUE IN FRAME F_Browser = "no"
                           FApellido1:SCREEN-VALUE IN FRAME F_Browser = ""
                           FApellido1:VISIBLE IN FRAME F_Browser = NO
                           FApellido2:SCREEN-VALUE IN FRAME F_Browser = ""
                           FApellido2:VISIBLE IN FRAME F_Browser = NO.

                    ENABLE FNombre WITH FRAME F_Browser.

                    FIND FIRST Clientes WHERE Clientes.Nombre BEGINS Buscar:SCREEN-VALUE NO-LOCK NO-ERROR.
                    IF AVAILABLE Clientes THEN
                        RUN Cliente_Encontrado.
                    ELSE DO:
                        W_Nombre:SCREEN-VALUE = "".
                        
                        IF ERROR-STATUS:GET-NUMBER(1) EQ 3166 THEN DO:
                            OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Nombre CONTAINS Buscar:SCREEN-VALUE NO-LOCK INDEXED-REPOSITION.
                            VIEW FRAME F_Browser.
                        END.
                    END.
                END.

                WHEN "Apellido1" THEN DO:
                    ASSIGN TNombre:SCREEN-VALUE IN FRAME F_Browser = "no"
                           FNombre:SCREEN-VALUE IN FRAME F_Browser = ""
                           FNombre:VISIBLE IN FRAME F_Browser = NO
                           TApellido1:SCREEN-VALUE IN FRAME F_Browser = "yes"
                           FApellido1:SCREEN-VALUE IN FRAME F_Browser = SELF:SCREEN-VALUE IN FRAME F_Consulta
                           FApellido1:VISIBLE IN FRAME F_Browser = YES
                           TApellido2:SCREEN-VALUE IN FRAME F_Browser = "no"
                           FApellido2:SCREEN-VALUE IN FRAME F_Browser = ""
                           FApellido2:VISIBLE IN FRAME F_Browser = NO.

                    ENABLE FApellido1 WITH FRAME F_Browser.

                    FIND FIRST Clientes WHERE Clientes.Apellido1 BEGINS Buscar:SCREEN-VALUE NO-LOCK NO-ERROR.
                    IF AVAILABLE Clientes THEN
                        RUN Cliente_Encontrado.
                    ELSE DO:
                        W_Nombre:SCREEN-VALUE = "".
                        
                        IF ERROR-STATUS:GET-NUMBER(1) EQ 3166 THEN DO:
                            OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Apellido1 CONTAINS Buscar:SCREEN-VALUE NO-LOCK INDEXED-REPOSITION.
                            VIEW FRAME F_Browser.
                        END.
                    END.
                END.

                WHEN "Apellido2" THEN DO:
                    ASSIGN TNombre:SCREEN-VALUE IN FRAME F_Browser = "no"
                           FNombre:SCREEN-VALUE IN FRAME F_Browser = ""
                           FNombre:VISIBLE IN FRAME F_Browser = NO
                           TApellido1:SCREEN-VALUE IN FRAME F_Browser = "no"
                           FApellido1:SCREEN-VALUE IN FRAME F_Browser = ""
                           FApellido1:VISIBLE IN FRAME F_Browser = NO
                           TApellido2:SCREEN-VALUE IN FRAME F_Browser = "yes"
                           FApellido2:SCREEN-VALUE IN FRAME F_Browser = SELF:SCREEN-VALUE IN FRAME F_Consulta
                           FApellido2:VISIBLE IN FRAME F_Browser = YES.

                    ENABLE FApellido2 WITH FRAME F_Browser.

                    FIND FIRST Clientes WHERE Clientes.Apellido2 BEGINS Buscar:SCREEN-VALUE NO-LOCK NO-ERROR.
                    IF AVAILABLE Clientes THEN
                        RUN Cliente_Encontrado.
                    ELSE DO:
                        W_Nombre:SCREEN-VALUE = "".

                        IF ERROR-STATUS:GET-NUMBER(1) EQ 3166 THEN DO:
                            OPEN QUERY Br_Clientes FOR EACH Clientes WHERE Clientes.Apellido2 CONTAINS Buscar:SCREEN-VALUE NO-LOCK INDEXED-REPOSITION.
                            VIEW FRAME F_Browser.
                        END.
                    END.
                END.

                WHEN "Tarjeta Visionamos" THEN DO:
                    FIND FIRST tarjetas WHERE tarjetas.tarjetaDB = Buscar:SCREEN-VALUE NO-LOCK NO-ERROR.
                    IF AVAILABLE tarjetas THEN DO:
                        FIND FIRST Clientes WHERE Clientes.Nit = tarjetas.nit NO-LOCK NO-ERROR.
                        IF AVAILABLE Clientes THEN
                            RUN Cliente_Encontrado.
                        ELSE DO:
                            W_Nombre:SCREEN-VALUE = "".

                            MESSAGE "Este número de tarjeta no se encuentra registrado" SKIP
                                    "a ningún Cliente/Asociado del Fondo."
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                        END.
                    END.
                    ELSE DO:
                        MESSAGE "Este número de tarjeta no se encuentra registrado" SKIP
                                "a ningún Cliente/Asociado del Fondo."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    END.
                END.
            END CASE.

            Buscar:SCREEN-VALUE = CAPS(Buscar:SCREEN-VALUE).
        END.

        NitWk = SUBSTRING(Buscar:SCREEN-VALUE,1,12).
    END.

    IF FRAME F_Compromisos:HIDDEN EQ NO THEN RUN Ver_Compromisos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Browser
&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 wWin
ON CHOOSE OF BUTTON-121 IN FRAME F_Browser /* Button 121 */
DO:
  HIDE FRAME F_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Foto
&Scoped-define SELF-NAME BUTTON-124
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-124 wWin
ON CHOOSE OF BUTTON-124 IN FRAME F_Foto /* Button 124 */
DO:
  HIDE FRAME F_Foto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Firma
&Scoped-define SELF-NAME BUTTON-125
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-125 wWin
ON CHOOSE OF BUTTON-125 IN FRAME F_Firma /* Button 125 */
DO:
  HIDE FRAME F_Firma.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME BUTTON-127
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-127 wWin
ON CHOOSE OF BUTTON-127 IN FRAME F_HojaVida /* Button 127 */
DO:
  HIDE FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-128
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-128 wWin
ON CHOOSE OF BUTTON-128 IN FRAME F_HojaVida /* Button 128 */
DO:
DO WITH FRAME F_HojaVida:
  ASSIGN FRAME F_HojaVida Cmb_Tipos Cmb_CodHV.
  IF INTEGER(SUBSTRING(Cmb_Tipos,1,3)) EQ 0 THEN ASSIGN TIni = 0 TFin = 999.
  ELSE ASSIGN TIni = INTEGER(SUBSTRING(Cmb_Tipos,1,3)) TFin = INTEGER(SUBSTRING(Cmb_Tipos,1,3)).
  IF INTEGER(SUBSTRING(Cmb_CodHV:SCREEN-VALUE,1,5)) EQ 0 THEN 
     ASSIGN CIni = 0 CFin = 99999.
  ELSE 
     ASSIGN CIni = INTEGER(SUBSTRING(Cmb_CodHV:SCREEN-VALUE,1,5))
            CFin = INTEGER(SUBSTRING(Cmb_CodHV:SCREEN-VALUE,1,5)).
  FIND NEXT  Hoja_Vida WHERE 
             Hoja_Vida.Nit           EQ NitWk AND
             Hoja_Vida.Tipo          GE TIni AND
             Hoja_Vida.Tipo          LE TFin AND
             Hoja_Vida.Codigo        GE CIni AND
             Hoja_Vida.Codigo        LE CFin AND
             Hoja_Vida.Fec_Grabacion GE FIni AND
             Hoja_Vida.Fec_Grabacion LE FFin USE-INDEX Idx_HojaVida NO-LOCK NO-ERROR.
  IF AVAILABLE Hoja_Vida THEN RUN Mostrar_HV.
  ELSE MESSAGE "Este es el último mensajes" SKIP
               "en su hoja de vida, en el rango" SKIP
               "de fechas!" VIEW-AS ALERT-BOX INFORMATION TITLE "Sin mensajes".
END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-129
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-129 wWin
ON CHOOSE OF BUTTON-129 IN FRAME F_HojaVida /* Button 129 */
DO:
ASSIGN FRAME F_HojaVida Cmb_Tipos Cmb_CodHV.
IF INTEGER(SUBSTRING(Cmb_Tipos,1,3)) EQ 0 THEN ASSIGN TIni = 0 TFin = 999.
ELSE ASSIGN TIni = INTEGER(SUBSTRING(Cmb_Tipos,1,3)) TFin = INTEGER(SUBSTRING(Cmb_Tipos,1,3)).
IF INTEGER(SUBSTRING(Cmb_CodHV,1,5)) EQ 0 THEN ASSIGN CIni = 0 CFin = 99999.
ELSE ASSIGN CIni = INTEGER(SUBSTRING(Cmb_CodHV,1,5)) CFin = INTEGER(SUBSTRING(Cmb_CodHV,1,5)).

DO WITH FRAME F_HojaVida:
  FIND PREV  Hoja_Vida WHERE 
             Hoja_Vida.Nit           EQ NitWk AND
             Hoja_Vida.Tipo          GE TIni AND
             Hoja_Vida.Tipo          LE TFin AND
             Hoja_Vida.Codigo        GE CIni AND
             Hoja_Vida.Codigo        LE CFin AND
             Hoja_Vida.Fec_Grabacion GE FIni AND
             Hoja_Vida.Fec_Grabacion LE FFin USE-INDEX Idx_HojaVida NO-LOCK NO-ERROR.
  IF AVAILABLE Hoja_Vida THEN RUN Mostrar_HV.
  ELSE MESSAGE "Este es el primer mensajes" SKIP
               "en su hoja de vida, en el rango" SKIP
              "de fechas!" VIEW-AS ALERT-BOX INFORMATION TITLE "Sin mensajes".
END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-142
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-142 wWin
ON CHOOSE OF BUTTON-142 IN FRAME F_HojaVida /* Agregar Mensaje */
DO:
  W_NitGlobal = NitWk.
  RUN W-Hoja_Vida.r NO-ERROR.
  APPLY "choose" TO Btn_Primero IN FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-144
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-144 wWin
ON CHOOSE OF BUTTON-144 IN FRAME F_HojaVida /* Button 144 */
DO:
ASSIGN FRAME F_HojaVida Cmb_Tipos Cmb_CodHV.
IF INTEGER(SUBSTRING(Cmb_Tipos,1,3)) EQ 0 THEN ASSIGN TIni = 0 TFin = 999.
ELSE ASSIGN TIni = INTEGER(SUBSTRING(Cmb_Tipos,1,3)) TFin = INTEGER(SUBSTRING(Cmb_Tipos,1,3)).
IF INTEGER(SUBSTRING(Cmb_CodHV,1,5)) EQ 0 THEN ASSIGN CIni = 0 CFin = 99999.
ELSE ASSIGN CIni = INTEGER(SUBSTRING(Cmb_CodHV,1,5)) CFin = INTEGER(SUBSTRING(Cmb_CodHV,1,5)).

DO WITH FRAME F_HojaVida:
  FIND LAST  Hoja_Vida WHERE 
             Hoja_Vida.Nit           EQ NitWk AND
             Hoja_Vida.Tipo          GE TIni AND
             Hoja_Vida.Tipo          LE TFin AND
             Hoja_Vida.Codigo        GE CIni AND
             Hoja_Vida.Codigo        LE CFin AND
             Hoja_Vida.Fec_Grabacion GE FIni AND
             Hoja_Vida.Fec_Grabacion LE FFin USE-INDEX Idx_HojaVida NO-LOCK NO-ERROR.
  IF AVAILABLE Hoja_Vida THEN RUN Mostrar_HV.
END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-145
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-145 wWin
ON CHOOSE OF BUTTON-145 IN FRAME F_HojaVida /* Buscar Mensaje */
DO:
  ASSIGN FRAME F_HojaVida Cmb_Tipos Cmb_CodHV.
  IF INTEGER(SUBSTRING(Cmb_Tipos,1,3)) EQ 0 THEN ASSIGN TIni = 0 TFin = 999.
  ELSE ASSIGN TIni = INTEGER(SUBSTRING(Cmb_Tipos,1,3)) TFin = INTEGER(SUBSTRING(Cmb_Tipos,1,3)).
  IF INTEGER(SUBSTRING(Cmb_CodHV,1,5)) EQ 0 THEN ASSIGN CIni = 0 CFin = 99999.
  ELSE ASSIGN CIni = INTEGER(SUBSTRING(Cmb_CodHV,1,5)) CFin = INTEGER(SUBSTRING(Cmb_CodHV,1,5)).
  OPEN QUERY Br_HojaVida FOR EACH Hoja_Vida WHERE
             Hoja_Vida.Tipo GE TIni AND
             Hoja_Vida.Tipo LE TFin AND
             Hoja_Vida.Nit EQ NitWk AND
             Hoja_Vida.Codigo GE CIni AND
             Hoja_Vida.Codigo LE CFin AND
             Hoja_Vida.Fec_Grabacion GE FIni AND
             Hoja_Vida.Fec_Grabacion LE FFin USE-INDEX Idx_Fecha NO-LOCK.
   VIEW FRAME F_BuscarM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_BuscarM
&Scoped-define SELF-NAME BUTTON-146
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-146 wWin
ON CHOOSE OF BUTTON-146 IN FRAME F_BuscarM /* Salir */
DO:
  HIDE FRAME F_BuscarM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cer
&Scoped-define SELF-NAME BUTTON-148
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-148 wWin
ON CHOOSE OF BUTTON-148 IN FRAME F_Cer /* Imprimir Carta */
DO:
  ASSIGN FRAME F_Cer WFirma WCC WCargo Cmb_Doc.
  RUN F-Documento.r (INPUT INPUT Cmb_Doc, INPUT NitWk,INPUT WFirma, INPUT WCargo, INPUT WCC) NO-ERROR.
  HIDE FRAME F_Cer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-149
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-149 wWin
ON CHOOSE OF BUTTON-149 IN FRAME F_Cer /* Button 149 */
DO:
  HIDE FRAME F_Cer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME BUTTON-151
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-151 wWin
ON CHOOSE OF BUTTON-151 IN FRAME F_Consulta /* Instancias */
DO:
  RUN BMovInst.

  FIND FIRST THojaVida WHERE THojaVida.CodA EQ 8 NO-ERROR.
  IF AVAILABLE THojaVida THEN DO:
     DO WITH FRAME F_MovInstancias:
        ASSIGN HvNomInstancia:SCREEN-VALUE = THojaVida.AsuH
               HvUsu:SCREEN-VALUE          = STRING(THojaVida.UsuH)
               HvNSol:SCREEN-VALUE         = STRING(THojaVida.DocH)
               HvEstado:SCREEN-VALUE       = STRING(THojaVida.Cump,"si/no")
               HvCuenta:SCREEN-VALUE       = STRING(THojaVida.CueH)
               HvFecIng:SCREEN-VALUE       = STRING(THojaVida.FecH)
               HvFecRet:SCREEN-VALUE       = STRING(THojaVida.FecR)
               HvDes:SCREEN-VALUE          = STRING(LOWER(THojaVida.DesH)).
     END.
     VIEW FRAME F_MovInstancias.
  END.
  ELSE MESSAGE "No tiene ningún movimiento de instancias" VIEW-AS ALERT-BOX INFORMATION.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_MovInstancias
&Scoped-define SELF-NAME BUTTON-152
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-152 wWin
ON CHOOSE OF BUTTON-152 IN FRAME F_MovInstancias /* Anterior */
DO:
    FIND PREV THojaVida WHERE THojaVida.CodA EQ 8 NO-ERROR.
    IF AVAILABLE THojaVida THEN DO:
       DO WITH FRAME F_MovInstancias:
          ASSIGN HvNomInstancia:SCREEN-VALUE = THojaVida.AsuH
                 HvUsu:SCREEN-VALUE          = STRING(THojaVida.UsuH)
                 HvNSol:SCREEN-VALUE         = STRING(THojaVida.DocH)
                 HvEstado:SCREEN-VALUE       = STRING(THojaVida.Cump,"si/no")
                 HvCuenta:SCREEN-VALUE       = STRING(THojaVida.CueH)
                 HvFecIng:SCREEN-VALUE       = STRING(THojaVida.FecH)
                 HvFecRet:SCREEN-VALUE       = STRING(THojaVida.FecR)
                 HvDes:SCREEN-VALUE          = STRING(LOWER(THojaVida.DesH)).
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-153
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON CHOOSE OF BUTTON-153 IN FRAME F_MovInstancias /* Siguiente */
DO:
    FIND NEXT THojaVida WHERE THojaVida.CodA EQ 8 NO-ERROR.
    IF AVAILABLE THojaVida THEN DO:
       DO WITH FRAME F_MovInstancias:
          ASSIGN HvNomInstancia:SCREEN-VALUE = THojaVida.AsuH
                 HvUsu:SCREEN-VALUE          = STRING(THojaVida.UsuH)
                 HvNSol:SCREEN-VALUE         = STRING(THojaVida.DocH)
                 HvEstado:SCREEN-VALUE       = STRING(THojaVida.Cump,"si/no")
                 HvCuenta:SCREEN-VALUE       = STRING(THojaVida.CueH)
                 HvFecIng:SCREEN-VALUE       = STRING(THojaVida.FecH)
                 HvFecRet:SCREEN-VALUE       = STRING(THojaVida.FecR)
                 HvDes:SCREEN-VALUE          = STRING(LOWER(THojaVida.DesH)).
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-154
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-154 wWin
ON CHOOSE OF BUTTON-154 IN FRAME F_MovInstancias /* Salir */
DO:
  HIDE FRAME F_MovInstancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-155
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-155 wWin
ON CHOOSE OF BUTTON-155 IN FRAME F_MovInstancias /* Consultar */
DO:
  OPEN QUERY BCMI FOR EACH THojaVida WHERE THojaVida.CodA EQ 8.
  VIEW FRAME F_ConMovInstancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConsCred
&Scoped-define SELF-NAME BUTTON-181
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-181 wWin
ON CHOOSE OF BUTTON-181 IN FRAME F_ConsCred /* Ocultar */
DO:
  ASSIGN FRAME F_Consulta:SENSITIVE = TRUE 
         FRAME F_ConsCred:VISIBLE   = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Controles
&Scoped-define SELF-NAME BUTTON-183
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-183 wWin
ON CHOOSE OF BUTTON-183 IN FRAME F_Controles /* Button 183 */
DO:
  HIDE FRAME F_Controles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromisos
&Scoped-define SELF-NAME BUTTON-184
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-184 wWin
ON CHOOSE OF BUTTON-184 IN FRAME F_Compromisos /* Button 184 */
DO:
  HIDE FRAME F_Compromisos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME BUTTON-205
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-205 wWin
ON CHOOSE OF BUTTON-205 IN FRAME FRelNva /* Grabar */
DO:
  ASSIGN FRAME FRelNva R_Nit R_Nombre R_Tel_Comercial.
  MESSAGE Wrel.
  IF R_Nit EQ "" OR R_Nombre EQ "" THEN DO:
     MESSAGE "Debe entrarse minimo: Nit, Nombre, apellido1 y el tipo de relacion" SKIP
             "o parentesco de la persona. rectifique la informacion" VIEW-AS ALERT-BOX.
     APPLY "entry" TO R_Nit.
     RETURN NO-APPLY.
  END.
  ELSE DO:
      FIND Clientes WHERE Clientes.Nit EQ R_Nit NO-ERROR.
      IF NOT AVAILABLE Clientes THEN DO:
         CREATE Clientes.
         ASSIGN Clientes.Agencia   = W_Agencia
                Clientes.Nit       = R_Nit
                Clientes.Nombre    = R_Nombre
                Clientes.Tel_Comercial  = R_Tel_Comercial.
      END.
      FIND FIRST Relaciones WHERE Relaciones.Nit EQ TClientes.Nit AND
                            Relaciones.Nit_Relacion EQ R_Nit NO-ERROR.
      IF NOT AVAILABLE Relaciones THEN DO:
          CREATE Relaciones.
          ASSIGN Relaciones.Nit             = Buscar:SCREEN-VALUE IN FRAME F_Consulta
                 Relaciones.Cod_Relacion    = WRel
                 Relaciones.Nit_Relacion    = R_Nit
                 Relaciones.Fec_Ingreso     = W_Fecha.
          IF WRel EQ 11 THEN DO:
              MESSAGE "oe".
             ASSIGN Relaciones.Clase = 2
                    Relaciones.Cod_Producto = Creditos.Cod_Credito
                    Relaciones.Cuenta       = STRING(Creditos.Num_Credito).
          END.
      END.
      ASSIGN Relaciones.Usuario         = W_Usuario
             Relaciones.Descripcion     = R_Tel_Comercial
             Relaciones.Estado          = 1.
      DO WITH FRAME FNvaRel:
        ASSIGN R_Nit:SCREEN-VALUE = ""
               R_Nombre:SCREEN-VALUE = ""
               R_Tel_Comercial:SCREEN-VALUE  = "".
      END.
      HIDE FRAME FRelNva.
      IF WRel = 10 THEN DO:
          OPEN QUERY B_Relaciones FOR EACH Relaciones WHERE Relaciones.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta 
             AND Relaciones.Cod_Relacion EQ 10 AND Relaciones.Estado EQ 1.
          VIEW FRAME F_Relaciones.
      END.
      IF WRel EQ 11 THEN DO: 
         APPLY "choose" TO Btn_AdmCode IN FRAME F_Basica.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-206
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-206 wWin
ON CHOOSE OF BUTTON-206 IN FRAME FRelNva /* Ocultar */
DO:
    ASSIGN R_Nit:SCREEN-VALUE = ""
         R_Nombre:SCREEN-VALUE = ""
         R_Tel_Comercial:SCREEN-VALUE = "".
    HIDE FRAME FRelNva.
    OPEN QUERY B_Relaciones FOR EACH Relaciones WHERE Relaciones.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta 
         AND Relaciones.Cod_Relacion EQ 10 AND Relaciones.Estado EQ 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basica
&Scoped-define SELF-NAME BUTTON-208
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-208 wWin
ON CHOOSE OF BUTTON-208 IN FRAME F_Basica /* Salir */
DO:
  HIDE FRAME F_Basica.
  APPLY "leave" TO Buscar IN FRAME F_Consulta.
  FIND Clientes WHERE Clientes.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_CobroJuridico
&Scoped-define SELF-NAME BUTTON-211
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-211 wWin
ON CHOOSE OF BUTTON-211 IN FRAME F_CobroJuridico /* Asentar Cobro Jurídico */
DO:
   DEFI VAR W_RowidCte AS ROWID.
  ASSIGN FRAME F_CobroJuridico Nit_Abogado Nom_Abogado.

  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  IF Usuarios.Id_cobrojuridico LE 0 THEN DO:
     MESSAGE "Solo actualizan Usuarios con Id_cobrojuridico GE 1"
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
   
  IF Nit_Abogado EQ "" THEN DO:
     MESSAGE "Para Operar a cobro jurídico debe escogerse el nit del abogado"
            VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Btn_OutCJ.
     RETURN.
  END.

  ASSIGN W_RowidCte = ROWID(Clientes).

  FIND FIRST Clientes WHERE Clientes.Nit EQ Nit_Abogado NO-LOCK NO-ERROR.
  IF NOT AVAIL(Clientes) THEN DO:
     MESSAGE "Abogado no existe en clientes."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
     
     FIND FIRST Clientes WHERE ROWID(Clientes) EQ W_RowidCte NO-LOCK NO-ERROR.    

     RETURN. 
  END.

  FIND FIRST Clientes WHERE ROWID(Clientes) EQ W_RowidCte NO-LOCK NO-ERROR.  

  FIND FIRST Creditos WHERE Creditos.Nit   EQ W_CedCteC AND
                      Creditos.Num_Credito EQ TCreditos.Num_Credito NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
     ASSIGN Creditos.Abogado     = TRUE
            Creditos.Nit_Juzgado = Nit_Abogado
            Creditos.Nom_Juzgado = Nom_Abogado
            TCreditos.Abogado    = TRUE
            TCreditos.Nit_Juzgado = Nit_Abogado
            TCreditos.Nom_Juzgado = Nom_Abogado.
     CREATE Mov_Creditos.
     ASSIGN Mov_Creditos.Cod_Operacion = 999999999
            Mov_Creditos.Cod_Credito   = TCreditos.Cod_Credito
            Mov_Creditos.Num_Credito   = TCreditos.Num_Credito
            Mov_Creditos.nit           = TCreditos.Nit
            Mov_Creditos.Fecha         = TODAY
            Mov_Creditos.Hora          = TIME
            Mov_Creditos.Agencia       = TCreditos.Agencia
            Mov_Creditos.Ofi_Fuente    = W_Agencia
            Mov_Creditos.Ofi_Destino   = W_Agencia
            Mov_Creditos.Usuario       = W_Usuario
            Mov_Creditos.Val_Efectivo  = 0
            Mov_Creditos.Descrip       = "Entra a Cobro Juridico. Usu:" + W_Usuario + " - Abogado : " + Nit_Abogado + " - " + Nom_Abogado.
     FIND CURRENT Creditos NO-LOCK NO-ERROR.
  END.
  ELSE 
      MESSAGE "No se Halló El Credito Vigente...Revise por favor."
          VIEW-AS ALERT-BOX ERROR.

  CLOSE QUERY BCobro.
  OPEN QUERY BCobro FOR EACH TCreditos WHERE TCreditos.Nit EQ W_CedCteC AND TCreditos.Estado EQ 2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromisos
&Scoped-define SELF-NAME BUTTON-212
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-212 wWin
ON CHOOSE OF BUTTON-212 IN FRAME F_Compromisos /* Informe */
DO:
  ASSIGN FRAME F_Compromisos REstado.
  RUN Informe_Compromisos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfCompromisos
&Scoped-define SELF-NAME BUTTON-213
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-213 wWin
ON CHOOSE OF BUTTON-213 IN FRAME F_InfCompromisos /* Ocultar */
DO:
  HIDE FRAME F_InfCompromisos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-214
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-214 wWin
ON CHOOSE OF BUTTON-214 IN FRAME F_InfCompromisos /* Imprimir */
DO:
 IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    ASSIGN WListado = Listado.
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado,INPUT-OUTPUT W_Dispositivo).
  OS-RENAME VALUE(WListado) VALUE(Listado).
  /*MESSAGE listado VIEW-AS ALERT-BOX.*/
/*    IF W_Dispositivo = "" THEN
      RETURN.*/

    IF W_Dispositivo = "P" THEN
        RUN Pantalla IN W_Manija (INPUT Listado).
    ELSE                                                  
      IF W_Dispositivo = "I" THEN
        RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  2,INPUT  1,INPUT  1,
                                           INPUT  99999,OUTPUT W_sw).
    IF W_Dispositivo <> "A" THEN
        OS-DELETE VALUE(Listado).
    IF W_Dispositivo = "E" THEN
      RUN Imprimir_Excel.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basica
&Scoped-define SELF-NAME BUTTON-216
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-216 wWin
ON CHOOSE OF BUTTON-216 IN FRAME F_Basica /* Ver Relaciones de Empresas */
DO:
  OPEN QUERY B_Relaciones FOR EACH Relaciones WHERE Relaciones.Nit EQ TClientes.Nit 
         AND Relaciones.Cod_Relacion EQ 10 AND Relaciones.Estado EQ 1.
  VIEW FRAME F_Relaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME BUTTON-217
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-217 wWin
ON CHOOSE OF BUTTON-217 IN FRAME F_Relaciones /* Crear Nueva Relacion de Empresa */
DO:
  FRAME FRelNva:TITLE = "Relacionar Empresa".
  VIEW FRAME FRelNva.
  WRel = 10.
  APPLY "entry" TO R_Nombre IN FRAME FRelNva.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-218
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-218 wWin
ON CHOOSE OF BUTTON-218 IN FRAME F_Relaciones /* Ocultar */
DO:
  HIDE FRAME F_Relaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basica
&Scoped-define SELF-NAME BUTTON-219
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-219 wWin
ON CHOOSE OF BUTTON-219 IN FRAME F_Basica /* Button 219 */
DO:
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
  IF INTEGER(P_Ubi) EQ 0 THEN DO:
     MESSAGE "No fue encontrada ninguna ubicación" VIEW-AS ALERT-BOX.
  END.

  ASSIGN W_UbicacionComercial:SCREEN-VALUE = STRING(P_Ubi,"99999999") + " - " + LC(P_NUbi).
    /*MESSAGE clientes.lugar_residencia:SCREEN-VALUE p_ubi VIEW-AS ALERT-BOX.    */  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME BUTTON-220
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-220 wWin
ON CHOOSE OF BUTTON-220 IN FRAME F_Relaciones /* Quitar Relación */
DO:
  FIND CURRENT Relaciones NO-ERROR.
  IF AVAILABLE Relaciones THEN
     Relaciones.Estado = 2.
  OPEN QUERY B_Relaciones FOR EACH Relaciones WHERE Relaciones.Nit EQ TClientes.Nit 
         AND Relaciones.Cod_Relacion EQ 10 AND Relaciones.Estado EQ 1.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AdmCodeudores
&Scoped-define SELF-NAME BUTTON-225
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-225 wWin
ON CHOOSE OF BUTTON-225 IN FRAME F_AdmCodeudores /* Button 225 */
DO:
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
  IF INTEGER(P_Ubi) EQ 0 THEN DO:
     MESSAGE "No fue encontrada ninguna ubicación" VIEW-AS ALERT-BOX.
  END.

  ASSIGN WC_UbicacionComercial:SCREEN-VALUE = STRING(P_Ubi,"99999999") + " - " + LC(P_NUbi).
    /*MESSAGE clientes.lugar_residencia:SCREEN-VALUE p_ubi VIEW-AS ALERT-BOX.    */  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-226
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-226 wWin
ON CHOOSE OF BUTTON-226 IN FRAME F_AdmCodeudores /* Salir */
DO:
  FIND Clientes WHERE Clientes.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta NO-LOCK NO-ERROR.
  HIDE FRAME F_AdmCodeudores.
  APPLY "leave" TO Buscar IN FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FPlastico
&Scoped-define SELF-NAME BUTTON-227
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-227 wWin
ON CHOOSE OF BUTTON-227 IN FRAME FPlastico /* Asignar Numero de Plastico */
DO:
  DEF VAR IdOk AS LOG INIT NO.
  DO WITH FRAME FPlastico:
     IF Plastico.Cue_Ahorros:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "No puede asignarse el numero de tarjera" SKIP
                "hasta no relacionarlo con una cuenta de" SKIP
                "CREARDIARIO. El numero de tarjeta no sera" SKIP
                "Asignado al cliente" VIEW-AS ALERT-BOX ERROR.
     END.
     IF DEC(Plastico.Num_Plastico:SCREEN-VALUE) EQ 0 THEN DO:
        MESSAGE "No se ha ingresado el número de tarjeta a" SKIP
                "asignar al cliente. no se graba el numero" SKIP
                "del plastico" VIEW-AS ALERT-BOX ERROR.
     END.
     ELSE DO:
         FIND Plastico WHERE Plastico.Num_Plastico EQ
              DEC(Plastico.Num_Plastico:SCREEN-VALUE IN FRAME FPlastico) NO-ERROR.
         IF AVAIL Plastico THEN DO:
            IF Plastico.Nit NE "" THEN DO:
               MESSAGE "Este plastico ya ha sido anteriormente asignado a:" SKIP
                       "Nit    : " Plastico.Nit SKIP
                       "Agencia: " Plastico.Agencia VIEW-AS ALERT-BOX ERROR.
               ASSIGN Plastico.Num_Plastico:SCREEN-VALUE = "".
            END.
            ELSE DO:
                FIND Ahorros WHERE Ahorros.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta AND
                     Ahorros.Cue_Ahorros EQ Plastico.Cue_Ahorros:SCREEN-VALUE NO-ERROR.
                IF AVAIL Ahorros THEN DO:
                   IF Ahorros.Agencia NE Plastico.Agencia THEN DO:
                      MESSAGE "La agencia donde se encuentra matriculado el plastico" SKIP
                              "no corresponde a la agencia de la cuenta de ahorros" SKIP
                              "no se podra asignar el plastico a esta cuenta!!!" VIEW-AS ALERT-BOX ERROR.
                      ASSIGN Plastico.Num_Plastico:SCREEN-VALUE = "".
                   END.
                   ELSE DO: 
                       ASSIGN Plastico.Num_Plastico = DECIMAL(Plastico.Num_Plastico:SCREEN-VALUE)
                              Plastico.Nit          = Ahorros.Nit
                              Plastico.Cue_Ahorros  = Ahorros.Cue_Ahorros
                              Plastico.Cod_Ahorro   = Ahorros.Cod_Ahorro
                              Ahorros.Ajuste        = Plastico.Num_Plastico.
                       MESSAGE "Numero de Plastico Asignado Correctamente" SKIP(1)
                               "Aliste su Impresora Para Imprimir el Formato de Solicitud" VIEW-AS ALERT-BOX.
                       InputFile = "c:\SFG\Objetos\Formatos\AV - 318.xls".
                       RUN Abrir_Excel.
                       RUN Imprime_Formato_TDebito.
                       RUN Cerrar_Excel.
                       MESSAGE "Se ha Impreso el Formato de Solicitud" SKIP(1)
                               "Aliste su Impresora Para Imprimir el Reglamento" VIEW-AS ALERT-BOX.
                       InputFile = "c:\SFG\Objetos\Formatos\AV - 313.xls".
                       RUN Abrir_Excel.
                       RUN Imprime_Formato_TDebito.
                       RUN Cerrar_Excel.
                   END.
                END.
                ELSE
                MESSAGE "No se ha encontrado la cuenta de ahorros para asignarle" SKIP
                        "el numero de plastico" VIEW-AS ALERT-BOX.
            END.
         END.
         ELSE
         MESSAGE "El plastico no esta matriculado entre los numeros de plastico validos" SKIP 
                 "No se asigna el plastico a la cuenta de ahorros" VIEW-AS ALERT-BOX ERROR.
       /*FIND Plastico WHERE 
              Plastico.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta NO-ERROR.
         IF NOT AVAILABLE Plastico THEN DO:
            FIND Clientes WHERE Clientes.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta NO-LOCK NO-ERROR.
            CREATE Plastico.
            ASSIGN Plastico.Agencia = Clientes.Agencia
                   Plastico.Nit     = Clientes.Nit
                   Plastico.Cod_ahorro = 3.
         END.
         ASSIGN FRAME FPlastico Plastico.Num_Plastico Plastico.Cue_Ahorros.
         FIND Ahorros WHERE Ahorros.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta AND
              Ahorros.Cue_Ahorros EQ Plastico.Cue_Ahorros NO-ERROR.
         IF AVAILABLE Ahorros THEN
            Ahorros.Ajuste = Plastico.Num_Plastico.
         ELSE DO:
            MESSAGE "No se ha encontrado la cuenta de ahorros para asignarle" SKIP
                    "el numero de plastico" VIEW-AS ALERT-BOX.
         END.*/
     END.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-228
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-228 wWin
ON CHOOSE OF BUTTON-228 IN FRAME FPlastico /* Button 228 */
DO:
    RUN C-Ahorros.r (INPUT Buscar:SCREEN-VALUE IN FRAME F_Consulta, 
                     OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue).
    IF W_Cue EQ "" THEN DO:
       MESSAGE "Debe escogerse una cuenta de ahorros de creardiario" VIEW-AS ALERT-BOX.
    END.
    IF W_Pro NE 3 THEN DO:
       MESSAGE "Solo pueden escogerse cuentas de creardiario" VIEW-AS ALERT-BOX.
    END.
    ELSE DO:
        Plastico.Cue_Ahorros:SCREEN-VALUE = W_Cue.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_CobroJuridico
&Scoped-define SELF-NAME BUTTON-229
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-229 wWin
ON CHOOSE OF BUTTON-229 IN FRAME F_CobroJuridico /* Borrar Cobro Jurídico */
DO:
   DEFI VAR W_RowidCte AS ROWID.
  ASSIGN FRAME F_CobroJuridico Nit_Abogado Nom_Abogado.

  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  IF Usuarios.Id_cobrojuridico LE 0 THEN DO:
     MESSAGE "Solo actualizan Usuarios con Id_cobrojuridico GE 1"
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
   
  FIND FIRST Creditos WHERE Creditos.Nit   EQ W_CedCteC AND
                      Creditos.Num_Credito EQ TCreditos.Num_Credito NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
     ASSIGN Creditos.Abogado     = FALSE
            Creditos.Nit_Juzgado = " "
            Creditos.Nom_Juzgado = " "
            TCreditos.Abogado    = FALSE
            TCreditos.Nit_Juzgado = " "
            TCreditos.Nom_Juzgado = " ".
     CREATE Mov_Creditos.
     ASSIGN Mov_Creditos.Cod_Operacion = 999999999
            Mov_Creditos.Cod_Credito   = TCreditos.Cod_Credito
            Mov_Creditos.Num_Credito   = TCreditos.Num_Credito
            Mov_Creditos.nit           = TCreditos.Nit
            Mov_Creditos.Fecha         = TODAY
            Mov_Creditos.Hora          = TIME
            Mov_Creditos.Agencia       = TCreditos.Agencia
            Mov_Creditos.Ofi_Fuente    = W_Agencia
            Mov_Creditos.Ofi_Destino   = W_Agencia
            Mov_Creditos.Usuario       = W_Usuario
            Mov_Creditos.Val_Efectivo  = 0
            Mov_Creditos.Descrip       = "borra Cobro Juridico. Usu:" + W_Usuario + " - Abogado : " + Nit_Abogado + " - " + Nom_Abogado.
     FIND CURRENT Creditos NO-LOCK NO-ERROR.
  END.
  ELSE 
      MESSAGE "No se Halló El Credito Vigente...Revise por favor."
          VIEW-AS ALERT-BOX ERROR.

  CLOSE QUERY BCobro.
  OPEN QUERY BCobro FOR EACH TCreditos WHERE TCreditos.Nit EQ W_CedCteC AND TCreditos.Estado EQ 2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Opciones
&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 wWin
ON CHOOSE OF BUTTON-5 IN FRAME F_Opciones /* Ocultar */
DO:
  ASSIGN FRAME F_Opciones FIni FFin.
  HIDE FRAME F_Opciones.  
  IF Buscar:SCREEN-VALUE IN FRAME F_Consulta NE "" THEN 
     APPLY "leave" TO Buscar IN FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Impresion
&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 wWin
ON CHOOSE OF BUTTON-6 IN FRAME F_Impresion /* Button 6 */
DO:
  HIDE FRAME F_Impresion.
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_Pathspl + "Lst_Relaciones" + W_Usuario + STRING(RANDOM(2000,10000)) + ".lst".
  {incluido/imprimir_carta.i "Listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON CHOOSE OF BUTTON-7 IN FRAME F_Impresion /* Button 7 */
DO:
  HIDE FRAME F_Impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Relaciones
&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME B_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Relaciones wWin
ON ROW-DISPLAY OF B_Relaciones IN FRAME F_Relaciones
DO:
  FIND FIRST TCli WHERE TCli.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
  IF AVAILABLE TCli THEN W_NomEmpresa = TCli.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Cmb_CodHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_CodHV wWin
ON VALUE-CHANGED OF Cmb_CodHV IN FRAME F_HojaVida /* Códigos Encontrados */
DO:
  APPLY "choose" TO Btn_Primero.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromisos
&Scoped-define SELF-NAME Cmb_estcom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_estcom wWin
ON VALUE-CHANGED OF Cmb_estcom IN FRAME F_Compromisos /* Estado */
DO:
  ASSIGN FRAME F_Compromisos REstado.
  OPEN QUERY BrCreditos FOR EACH Creditos WHERE 
       Creditos.Nit EQ Clientes.Nit 
   AND Creditos.Estado EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,1)) NO-LOCK.
  w_ok = BROWSE BRCreditos:SELECT-ROW(1) NO-ERROR.
  APPLY "value-changed" TO REstado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConMovInstancias
&Scoped-define SELF-NAME Cmb_TipIns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TipIns wWin
ON VALUE-CHANGED OF Cmb_TipIns IN FRAME F_ConMovInstancias /* Instancias */
DO:
  ASSIGN FRAME F_ConMovInstancias Cmb_TipIns.
  IF SUBSTRING(Cmb_TipIns,1,5) EQ "00000" THEN
     OPEN QUERY BCMI FOR EACH THojaVida WHERE THojaVida.CodA EQ 8.
  ELSE
     OPEN QUERY BCMI FOR EACH THojaVida WHERE THojaVida.CodA EQ 8 AND
          THojaVida.InsH EQ INTEGER(SUBSTRING(Cmb_TipIns,1,5)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Cmb_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tipo wWin
ON VALUE-CHANGED OF Cmb_Tipo IN FRAME F_Consulta
DO:
    ASSIGN Buscar:SCREEN-VALUE = ""
           W_Nombre:SCREEN-VALUE = "".

    IF Cmb_Tipo:SCREEN-VALUE = "Imprimir todos" THEN
        RUN imprimirTodos.

    APPLY 'entry' TO Buscar.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Cmb_Tipos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tipos wWin
ON VALUE-CHANGED OF Cmb_Tipos IN FRAME F_HojaVida /* Tipos de Mensaje */
DO:
  ASSIGN FRAME F_HojaVida Cmb_Tipos.
  DO WITH FRAME F_HojaVida:
     ASSIGN Cmb_CodHV:LIST-ITEMS = ""
            Cmb_CodHV:SCREEN-VALUE = "".
     IF SUBSTRING(Cmb_Tipos,1,3) NE "000" THEN DO:
        ENABLE Cmb_CodHV.
        W_Ok = Cmb_CodHV:ADD-LAST("00000 - Todos los Códigos").
        FOR EACH Varios WHERE Varios.Tipo EQ INTEGER(SUBSTRING(Cmb_Tipos,1,3)) NO-LOCK:
           W_Ok = Cmb_CodHV:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
        END.
     END.
     ELSE DISABLE Cmb_CodHV.
     
  END.
  RUN Hoja_de_Vida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromisos
&Scoped-define SELF-NAME Ecompromiso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ecompromiso wWin
ON LEAVE OF Ecompromiso IN FRAME F_Compromisos
DO:
  ASSIGN ECompromiso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cobros.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cobros.Estado wWin
ON VALUE-CHANGED OF Cobros.Estado IN FRAME F_Compromisos /* Estado */
DO:
  ENABLE Btn_SalCom WITH FRAME F_Compromisos.
  ASSIGN Cobros.Val_Compromiso:SCREEN-VALUE = "0"
         Cobros.Fec_Compromiso:SCREEN-VALUE = "?".
  IF Cobros.Estado:SCREEN-VALUE EQ "0" THEN
     DISABLE Cobros.Val_Compromiso Cobros.Fec_Compromiso WITH FRAME F_Compromisos.
  ELSE
     ENABLE Cobros.Val_Compromiso Cobros.Fec_Compromiso WITH FRAME F_Compromisos.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Opciones
&Scoped-define SELF-NAME fechaCorte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fechaCorte wWin
ON LEAVE OF fechaCorte IN FRAME F_Opciones
DO:
    ASSIGN fechaCorte.

    IF fechaCorte > w_fecha THEN DO:
        MESSAGE "No se permiten fechas futuras."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        fechaCorte:SCREEN-VALUE = STRING(w_fecha - DAY(w_fecha)).
        ASSIGN fechaCorte.

        APPLY "Entry" TO fechaCorte IN FRAME F_Opciones.
    END.
    ELSE DO:
        IF DAY(fechaCorte + 1) <> 1 THEN
            MESSAGE "Recuerde que se generarán datos a corte de mes."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

        fechaCorte = ADD-INTERVAL(fechaCorte,1,"months").
        fechaCorte = fechaCorte - DAY(fechaCorte).
        fechaCorte:SCREEN-VALUE = STRING(fechaCorte).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FFin wWin
ON LEAVE OF FFin IN FRAME F_Opciones /* Hasta */
DO:
  ASSIGN FFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FIni wWin
ON LEAVE OF FIni IN FRAME F_Opciones /* Desde */
DO:
  ASSIGN FIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IdFacturaCupoRotativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IdFacturaCupoRotativo wWin
ON VALUE-CHANGED OF IdFacturaCupoRotativo IN FRAME F_Opciones /* Extracto Cupo Rotativo */
DO:
  ASSIGN idFacturaCupoRotativo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_AhoCanc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_AhoCanc wWin
ON VALUE-CHANGED OF Id_AhoCanc IN FRAME F_Opciones /* Ahorros Cancelados */
DO:
  ASSIGN Id_AhoCanc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_Atrasos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_Atrasos wWin
ON VALUE-CHANGED OF Id_Atrasos IN FRAME F_Opciones /* Atrasos */
DO:
  ASSIGN Id_Atrasos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_Codeudando
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_Codeudando wWin
ON VALUE-CHANGED OF Id_Codeudando IN FRAME F_Opciones /* Codeudando A: */
DO:
  ASSIGN Id_Codeudando.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_Controles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_Controles wWin
ON VALUE-CHANGED OF Id_Controles IN FRAME F_Opciones /* Reportes y Controles */
DO:
  FIND Clientes WHERE Clientes.Nit EQ TClientes.Nit NO-ERROR.
  ASSIGN TReportado_ProCredito = Clientes.Reportado_Procredito
         TReportado_Super      = Clientes.Reportado_Super
         TReportado_Fiscalia   = Clientes.Reportado_Fiscalia.
  DISPLAY TReportado_Procredito TReportado_Fiscalia TReportado_Super WITH FRAME F_Controles.
  HIDE VProcredito VFiscalia VSuperbancaria IN FRAME F_Controles.
  VIEW FRAME F_Controles.
  SELF:SCREEN-VALUE = "no".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_CredCanc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_CredCanc wWin
ON MOUSE-SELECT-CLICK OF Id_CredCanc IN FRAME F_Opciones /* Créditos Canc.y Castigados */
DO:
  ASSIGN Id_CredCanc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_CredCanc wWin
ON VALUE-CHANGED OF Id_CredCanc IN FRAME F_Opciones /* Créditos Canc.y Castigados */
DO:
  ASSIGN Id_CredCanc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_Creditos wWin
ON VALUE-CHANGED OF Id_Creditos IN FRAME F_Opciones /* Detalle Créditos */
DO:
  ASSIGN Id_Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_Economica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_Economica wWin
ON VALUE-CHANGED OF Id_Economica IN FRAME F_Opciones /* Economica */
DO:
  ASSIGN Id_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_GarAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_GarAdm wWin
ON VALUE-CHANGED OF Id_GarAdm IN FRAME F_Opciones /* Garantías */
DO:
  ASSIGN Id_GarAdm.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_General
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_General wWin
ON VALUE-CHANGED OF Id_General IN FRAME F_Opciones /* General */
DO:
  ASSIGN Id_General.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_GestCobro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_GestCobro wWin
ON VALUE-CHANGED OF Id_GestCobro IN FRAME F_Opciones /* Gestion Cobranzas */
DO:
  ASSIGN Id_GestCobro.

  IF SELF:SCREEN-VALUE EQ "no" THEN DO:
     ASSIGN FIni:SCREEN-VALUE = STRING(TODAY - DAY(TODAY) + 1)
            FFin:SCREEN-VALUE = STRING(TODAY).
     DISABLE FFin FIni WITH FRAME F_Opciones.
  END.
  ELSE DO:
    ENABLE FFin FIni WITH FRAME F_Opciones.
    APPLY "Entry" TO FIni IN FRAME F_Opciones.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_HistCreditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_HistCreditos wWin
ON MOUSE-SELECT-CLICK OF Id_HistCreditos IN FRAME F_Opciones /* Historial Crediticio */
DO:
  APPLY "Value-Changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_HistCreditos wWin
ON VALUE-CHANGED OF Id_HistCreditos IN FRAME F_Opciones /* Historial Crediticio */
DO:
  ASSIGN Id_HistCreditos.

  IF Id_HistCreditos THEN DO:
     ASSIGN WWin:SENSITIVE = NO.
     WWin:MOVE-TO-BOTTOM().

     RUN W-Hist_Creditos.r (INPUT Buscar:SCREEN-VALUE IN FRAME F_Consulta,999999).
  
     ASSIGN WWin:SENSITIVE               = YES
            Id_HistCreditos              = FALSE
            Id_HistCreditos:SCREEN-VALUE = "No".
     WWin:MOVE-TO-TOP().

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_HojaVida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_HojaVida wWin
ON VALUE-CHANGED OF Id_HojaVida IN FRAME F_Opciones /* Hoja Vida */
DO:
  ASSIGN Id_HojaVida.

  IF SELF:SCREEN-VALUE EQ "no" THEN DO:
     ASSIGN FIni:SCREEN-VALUE = STRING(TODAY - DAY(TODAY) + 1)
            FFin:SCREEN-VALUE = STRING(TODAY).
     DISABLE FFin FIni WITH FRAME F_Opciones.
  END.
  ELSE DO:
    ENABLE FFin FIni WITH FRAME F_Opciones.
    APPLY "Entry" TO FIni IN FRAME F_Opciones.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_Relaciones wWin
ON VALUE-CHANGED OF Id_Relaciones IN FRAME F_Opciones /* Relaciones */
DO:
  ASSIGN Id_Relaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_SimulaPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_SimulaPago wWin
ON MOUSE-SELECT-CLICK OF Id_SimulaPago IN FRAME F_Opciones /* Simula Abono a Crédito */
DO:
  APPLY "Value-Changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_SimulaPago wWin
ON VALUE-CHANGED OF Id_SimulaPago IN FRAME F_Opciones /* Simula Abono a Crédito */
DO:
  ASSIGN Id_SimulaPago.

  IF Id_SimulaPago THEN DO:
     ASSIGN FRAME F_Consulta:SENSITIVE = FALSE 
            Id_SimulaPago              = FALSE
            Id_SimulaPago:SCREEN-VALUE = "No" 
            FRAME F_ConsCred:VISIBLE   = TRUE.

     CLOSE QUERY BROWSE-1.

     OPEN QUERY BROWSE-1 FOR EACH Creditos WHERE
           Creditos.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta
       AND Sdo_Capital  GT 0 NO-LOCK INDEXED-REPOSITION.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_Solicitudes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_Solicitudes wWin
ON VALUE-CHANGED OF Id_Solicitudes IN FRAME F_Opciones /* Detalle Solicitudes */
DO:
  ASSIGN Id_Solicitudes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_CobroJuridico
&Scoped-define SELF-NAME Nit_Abogado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Nit_Abogado wWin
ON LEAVE OF Nit_Abogado IN FRAME F_CobroJuridico /* Nit Abogado */
DO:
   DEFI VAR W_RowidCte AS ROWID.

  ASSIGN Nit_Abogado.

  IF SELF:SCREEN-VALUE EQ "" THEN DO:
      RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
      ASSIGN Nom_Abogado:SCREEN-VALUE = TRIM(P_Nombre) + " " + P_Apellido
             Nom_Abogado
             Nit_Abogado:SCREEN-VALUE = P_Nit
             Nit_Abogado.
  END.
  ELSE DO:
      ASSIGN W_RowidCte = ROWID(Clientes).

      FIND FIRST Clientes WHERE Clientes.Nit EQ Nit_Abogado NO-LOCK NO-ERROR.
      IF AVAIL(Clientes) THEN
         ASSIGN Nom_Abogado:SCREEN-VALUE  = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + Clientes.Apellido2
                Nom_Abogado. 
      ELSE 
         MESSAGE "Abogado no existe en clientes."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.

      FIND FIRST Clientes WHERE ROWID(Clientes) EQ W_RowidCte NO-LOCK NO-ERROR.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Nit_Abogado wWin
ON MOUSE-SELECT-DBLCLICK OF Nit_Abogado IN FRAME F_CobroJuridico /* Nit Abogado */
DO:
       RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
       ASSIGN Nom_Abogado:SCREEN-VALUE = TRIM(P_Nombre) + " " + P_Apellido
              Nom_Abogado
              Nit_Abogado:SCREEN-VALUE = P_Nit
              Nit_Abogado.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromisos
&Scoped-define SELF-NAME REstado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL REstado wWin
ON VALUE-CHANGED OF REstado IN FRAME F_Compromisos
DO:
   RUN QCompromisos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME R_Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Nit wWin
ON LEAVE OF R_Nit IN FRAME FRelNva /* Nit */
DO:
  ASSIGN FRAME FrelNva R_Nit.
  FIND Clientes WHERE Clientes.Nit EQ R_Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN DO:
     R_Nombre:SCREEN-VALUE = Clientes.Nombre.
  END.
  FIND Relaciones WHERE Relaciones.Nit EQ Clientes.Nit AND
       Relaciones.Nit_Relacion EQ R_Nit AND Relaciones.Estado EQ 2 NO-ERROR.
  IF AVAILABLE Relaciones THEN
     Relaciones.Estado = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Nombre wWin
ON LEAVE OF R_Nombre IN FRAME FRelNva /* Nombre */
DO:
  ASSIGN FRAME FRelNva R_Nombre.
  FIND FIRST Clientes WHERE Clientes.Nombre BEGINS R_Nombre NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN DO:
     ASSIGN R_Nit = Clientes.Nit
            R_Nombre = Clientes.Nombre
            R_Tel_Comercial = Clientes.Tel_Comercial.
     DISPLAY R_Nit R_Nombre R_Tel_Comercial WITH FRAME FRelNva.
  END.
  ELSE APPLY "entry" TO R_Nit.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Browser
&Scoped-define SELF-NAME TApellido1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TApellido1 wWin
ON VALUE-CHANGED OF TApellido1 IN FRAME F_Browser /* Apellido1 */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ENABLE FApellido1 WITH FRAME F_Browser.
  ELSE
     DISABLE FApellido1 WITH FRAME F_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TApellido2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TApellido2 wWin
ON VALUE-CHANGED OF TApellido2 IN FRAME F_Browser /* Apellido2 */
DO:
    IF SELF:SCREEN-VALUE EQ "yes" THEN
       ENABLE FApellido2 WITH FRAME F_Browser.
    ELSE
       DISABLE FApellido2 WITH FRAME F_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Opciones
&Scoped-define SELF-NAME TCompromisos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TCompromisos wWin
ON VALUE-CHANGED OF TCompromisos IN FRAME F_Opciones /* Compromisos */
DO:
  RUN Ver_Compromisos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgFecCorte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgFecCorte wWin
ON VALUE-CHANGED OF tgFecCorte IN FRAME F_Opciones /* Fecha de Corte */
DO:
    ASSIGN tgFecCorte.

    IF SELF:SCREEN-VALUE EQ "no" THEN DO:
        fechaCorte:SCREEN-VALUE = "".
        DISABLE fechaCorte WITH FRAME F_Opciones.
    END.
    ELSE DO:
        ENABLE fechaCorte WITH FRAME F_Opciones.
        fechaCorte:SCREEN-VALUE = STRING(w_fecha - DAY(w_fecha)).
        ASSIGN fechaCorte.
        APPLY "Entry" TO fechaCorte IN FRAME F_Opciones.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_CtasAho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_CtasAho wWin
ON VALUE-CHANGED OF Tg_CtasAho IN FRAME F_Opciones /* Estado Ctas-Ahorro */
DO:
  ASSIGN Tg_CtasAho.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_Extras wWin
ON VALUE-CHANGED OF Tg_Extras IN FRAME F_Opciones /* Cuotas extras */
DO:
  ASSIGN Tg_Extras
         FFin
         FIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_Plastico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_Plastico wWin
ON VALUE-CHANGED OF Tg_Plastico IN FRAME F_Opciones /* Plastico */
DO:
  DO WITH FRAME FPlastico:
     ASSIGN Plastico.Cue_Ahorros:SCREEN-VALUE  = ""
            Plastico.Num_Plastico:SCREEN-VALUE = "".
     FIND Plastico WHERE Plastico.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta NO-ERROR.
     IF AVAILABLE Plastico THEN DO:
        ASSIGN Plastico.Cue_Ahorros:SCREEN-VALUE  = STRING(Plastico.Cue_Ahorros)
               Plastico.Num_Plastico:SCREEN-VALUE = STRING(Plastico.Num_Plastico).
     END.
  END.
  
  VIEW FRAME FPlastico.
  SELF:SCREEN-VALUE = "no".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Browser
&Scoped-define SELF-NAME TNombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TNombre wWin
ON VALUE-CHANGED OF TNombre IN FRAME F_Browser /* Nombre */
DO:
    IF SELF:SCREEN-VALUE EQ "yes" THEN
       ENABLE FNombre WITH FRAME F_Browser.
    ELSE
       DISABLE FNombre WITH FRAME F_Browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Controles
&Scoped-define SELF-NAME TReportado_fiscalia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TReportado_fiscalia wWin
ON VALUE-CHANGED OF TReportado_fiscalia IN FRAME F_Controles /* Reportado a Fiscalia */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE VFiscalia WITH FRAME F_Controles.
     APPLY "entry" TO VFiscalia IN FRAME F_Controles.
  END.
  ELSE DO:
     VFiscalia:SCREEN-VALUE = "".
     DISABLE VFiscalia WITH FRAME F_Controles.
     HIDE VFiscalia.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TReportado_Procredito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TReportado_Procredito wWin
ON VALUE-CHANGED OF TReportado_Procredito IN FRAME F_Controles /* Reportado Procredito */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE VProcredito WITH FRAME F_Controles.
     APPLY "entry" TO VProcredito IN FRAME F_Controles.
  END.
  ELSE DO:
     VProcredito:SCREEN-VALUE = "".
     DISABLE VProcredito WITH FRAME F_Controles.
     HIDE VProcredito.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TReportado_Super
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TReportado_Super wWin
ON VALUE-CHANGED OF TReportado_Super IN FRAME F_Controles /* Reportado Superbancaria */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE VSuperbancaria WITH FRAME F_Controles.
     APPLY "entry" TO VSuperbancaria IN FRAME F_Controles.
  END.
  ELSE DO:
     VSuperbancaria:SCREEN-VALUE = "".
     DISABLE VSuperbancaria WITH FRAME F_Controles.
     HIDE VSuperbancaria.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Browser
&Scoped-define SELF-NAME W_CodCpto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CodCpto wWin
ON LEAVE OF W_CodCpto IN FRAME F_Browser /* Código de la Gestión */
DO:
  ASSIGN W_CodCpto.

  FIND FIRST Varios WHERE Varios.Tipo EQ 30
                    AND Varios.Codigo EQ W_CODCpto
                    AND Varios.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Varios) THEN
     ASSIGN Cmb_Cpto:SCREEN-VALUE IN FRAME F_Compromisos = (STRING(Varios.Codigo,"99999") + " - " + Varios.Descrip).
  ELSE
     ASSIGN Cmb_Cpto:SCREEN-VALUE  = Cmb_Cpto:ENTRY(1)
            W_CodCpto              = 0
            W_CodCpto:SCREEN-VALUE = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConsCred
&Scoped-define BROWSE-NAME BCMI
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}
HIDE FRAME F_Browser.
/* DELETE WIDGET-POOL.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Abrir_Excel wWin 
PROCEDURE Abrir_Excel :
FIND FIRST Usuarios WHERE Usuarios.Usuario = W_Usuario NO-LOCK NO-ERROR.
FIND FIRST Clientes WHERE Clientes.Nit = Ahorros.Nit NO-LOCK NO-ERROR.
FIND FIRST Agencias WHERE Agencias.Agencia = Ahorros.Agencia NO-LOCK NO-ERROR.

CREATE "Excel.Application" chExcelApp.
SwExiste = SEARCH(InputFile).

IF SwExiste EQ ? THEN DO:
    MESSAGE InputFile "no Encontrado"
        VIEW-AS ALERT-BOX.

    RETURN.
END.

hWorkBooks = chExcelApp:WorkBooks:OPEN(InputFile,,TRUE,).
chExcelApp:Visible = TRUE.
chWorkSheet = chExcelApp:Sheets:Item(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'wPQRMnto.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_w_captura-pqr ).
       /* Position in AB:  ( 21.31 , 105.00 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aho_Cancel wWin 
PROCEDURE Aho_Cancel :
/*------------------------------------------------------------------------------
  Purpose:  Solo los ahorros con estado diferente de 1.   
  Notes:  Sept.20/05 GAER     
------------------------------------------------------------------------------*/
W_Texto = "------------------------------------------------------------------------------------------------------------------------".
RUN Grabar_Info (INPUT "ACA", INPUT W_Texto).     
W_Texto = "INFORMACION DE AHORROS CANCELADOS".
RUN Grabar_Info (INPUT "ACA", INPUT W_Texto).     
WOcur = 0.

FOR EACH TSaldos WHERE TSaldos.Cla EQ 1: DELETE TSaldos. END.
FOR EACH TAhorros: DELETE TAhorros. END.
FOR EACH Ahorros WHERE Ahorros.Nit EQ TClientes.Nit NO-LOCK BREAK BY Ahorros.Cod_Ahorro:
  IF Ahorros.Estado NE 1 THEN DO:
     FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_ahorro NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Ahorros THEN gTexto = Pro_Ahorros.Nom_Producto.
     FIND TSaldos WHERE TSaldos.Cla EQ 1 AND TSaldos.Cod EQ Ahorros.Cod_ahorro NO-ERROR.
     IF NOT AVAILABLE TSaldos THEN DO: 
        CREATE TSaldos.
        ASSIGN TSaldos.Cla = 1
               TSaldos.Cod = Ahorros.Cod_Ahorro
               TSaldos.Nom = gTexto
               TSaldos.Tas = TSaldos.Tas + Ahorros.Tasa
               WOcur = WOcur + 1.
     END.

     IF Pro_ahorros.tip_ahorro EQ 3 THEN TSaldos.Dis = TSaldos.Dis + Ahorros.Sdo_Disponible.
     ELSE TSaldos.Dis = TSaldos.Dis + Ahorros.Sdo_Disponible.

     ASSIGN TSaldos.Cuo = TSaldos.Cuo + Ahorros.Cuota
            TSaldos.Itr = TSaldos.Itr + Ahorros.Int_Pagar.

     CREATE TAhorros.
     BUFFER-COPY Ahorros TO TAhorros.
  END.

  IF LAST-OF(Ahorros.Cod_Ahorro) AND AVAIL(TSaldos) THEN DO:
     ASSIGN TSaldos.Tas = TSaldos.Tas / WOcur
            WOcur = 0.
     RELEASE TSaldos.
  END.
END.
 
RUN BSaldos(INPUT 1).
FOR EACH TAhorros: RUN Detalle_Ahorro(INPUT "ACA"). END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BAhorros wWin 
PROCEDURE BAhorros :
W_Texto = "------------------------------------------------------------------------------------------------------------------------".
RUN Grabar_Info (INPUT "AHO", INPUT W_Texto).

W_Texto = "INFORMACION DETALLADA DE LOS AHORROS".
RUN Grabar_Info (INPUT "AHO", INPUT W_Texto).

WOcur = 0.

FOR EACH TSaldos WHERE TSaldos.Cla EQ 1:
    DELETE TSaldos.
END.

EMPTY TEMP-TABLE TAhorros.

IF tgFecCorte = NO THEN DO:
    FOR EACH Ahorros WHERE Ahorros.Nit = TClientes.Nit AND ahorros.tip_ahorro <> 3 NO-LOCK BREAK BY Ahorros.Cod_Ahorro:
        IF Ahorros.Estado = 1 THEN DO:
            FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = Ahorros.Cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE Pro_Ahorros THEN
                gTexto = Pro_Ahorros.Nom_Producto.

            FIND FIRST TSaldos WHERE TSaldos.Cla = 1
                                 AND TSaldos.Cod = Ahorros.Cod_ahorro NO-ERROR.
            IF NOT AVAILABLE TSaldos THEN DO:
                CREATE TSaldos.
                TSaldos.Cla = 1.
                TSaldos.Cod = Ahorros.Cod_Ahorro.
                TSaldos.Nom = gTexto.
                TSaldos.Tas = TSaldos.Tas + Ahorros.Tasa.
                WOcur = WOcur + 1.
            END.

            TSaldos.Dis = TSaldos.Dis + Ahorros.Sdo_Disponible.
            TSaldos.Cuo = TSaldos.Cuo + Ahorros.Cuota.
            TSaldos.Itr = TSaldos.Itr + Ahorros.Int_Pagar + Ahorros.Sdo_canje.

            IF TSaldos.Dis = 0 THEN
                DELETE TSaldos.

            CREATE TAhorros.
            BUFFER-COPY Ahorros TO TAhorros.
        END.

        IF LAST-OF(Ahorros.Cod_Ahorro) AND AVAIL(TSaldos) THEN DO:
            ASSIGN TSaldos.Tas = TSaldos.Tas / WOcur
                   WOcur = 0.

            RELEASE TSaldos.
        END.
    END.

    FOR EACH Ahorros WHERE Ahorros.Nit = TClientes.Nit AND ahorros.tip_ahorro = 3 NO-LOCK BY ahorros.cod_ahorro
                                                                                          BY ahorros.fec_apertura:
        IF Ahorros.Estado = 1 THEN DO:
            FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = Ahorros.Cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE Pro_Ahorros THEN
                gTexto = Pro_Ahorros.Nom_Producto.

            CREATE TSaldos.
            TSaldos.Cla = 1.
            TSaldos.Cod = Ahorros.Cod_Ahorro.
            TSaldos.Nom = gTexto + " - " + STRING(ahorros.cue_ahorros).
            TSaldos.Tas = TSaldos.Tas + Ahorros.Tasa.
            TSaldos.fecApertura = STRING(ahorros.fec_apertura,"99/99/9999").
            TSaldos.fecVcto = STRING(ahorros.Fec_Vencimiento,"99/99/9999").
            WOcur = WOcur + 1.

            TSaldos.Dis = TSaldos.Dis + Ahorros.Sdo_Disponible.
            TSaldos.Cuo = TSaldos.Cuo + Ahorros.Cuota.
            TSaldos.Itr = TSaldos.Itr + Ahorros.Int_Pagar + Ahorros.Sdo_canje.

            IF TSaldos.Dis = 0 THEN
                DELETE TSaldos.

            CREATE TAhorros.

            BUFFER-COPY Ahorros TO TAhorros.

            RELEASE TSaldos.
        END.
    END.

    RUN BSaldos(INPUT 1).

    FOR EACH TAhorros:
        RUN Detalle_Ahorro(INPUT "AHO").
    END.
END.
ELSE DO:
    FOR EACH rep_Ahorros WHERE rep_ahorros.fecCorte = fechaCorte AND rep_Ahorros.Nit = TClientes.Nit AND rep_ahorros.tip_ahorro <> 3 NO-LOCK BREAK BY rep_Ahorros.Cod_Ahorro:
        IF rep_Ahorros.Estado = 1 THEN DO:
            FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = rep_Ahorros.Cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE Pro_Ahorros THEN
                gTexto = Pro_Ahorros.Nom_Producto.

            FIND FIRST TSaldos WHERE TSaldos.Cla = 1
                                 AND TSaldos.Cod = rep_Ahorros.Cod_ahorro NO-ERROR.
            IF NOT AVAILABLE TSaldos THEN DO:
                CREATE TSaldos.
                TSaldos.Cla = 1.
                TSaldos.Cod = rep_Ahorros.Cod_Ahorro.
                TSaldos.Nom = gTexto.
                TSaldos.Tas = TSaldos.Tas + rep_Ahorros.Tasa.
                WOcur = WOcur + 1.
            END.

            TSaldos.Dis = TSaldos.Dis + rep_Ahorros.Sdo_Disponible.
            TSaldos.Cuo = TSaldos.Cuo + rep_Ahorros.Cuota.
            TSaldos.Itr = TSaldos.Itr + rep_Ahorros.Int_Pagar + rep_Ahorros.Sdo_canje.

            IF TSaldos.Dis = 0 THEN
                DELETE TSaldos.

            CREATE TAhorros.
            BUFFER-COPY rep_Ahorros TO TAhorros.
        END.

        IF LAST-OF(rep_Ahorros.Cod_Ahorro) AND AVAIL(TSaldos) THEN DO:
            ASSIGN TSaldos.Tas = TSaldos.Tas / WOcur
                   WOcur = 0.

            RELEASE TSaldos.
        END.
    END.

    FOR EACH rep_Ahorros WHERE rep_Ahorros.fecCorte = fechaCorte AND rep_Ahorros.Nit = TClientes.Nit AND rep_Ahorros.tip_ahorro = 3 NO-LOCK BY rep_Ahorros.cod_ahorro
                                                                                                                                            BY rep_Ahorros.fec_apertura:
        IF rep_Ahorros.Estado = 1 THEN DO:
            FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = rep_Ahorros.Cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE Pro_Ahorros THEN
                gTexto = Pro_Ahorros.Nom_Producto.

            CREATE TSaldos.
            TSaldos.Cla = 1.
            TSaldos.Cod = rep_Ahorros.Cod_Ahorro.
            TSaldos.Nom = gTexto + " - " + STRING(rep_Ahorros.cue_ahorros).
            TSaldos.Tas = TSaldos.Tas + rep_Ahorros.Tasa.
            TSaldos.fecApertura = STRING(rep_Ahorros.fec_apertura,"99/99/9999").
            TSaldos.fecVcto = STRING(rep_Ahorros.Fec_Vencimiento,"99/99/9999").
            WOcur = WOcur + 1.

            TSaldos.Dis = TSaldos.Dis + rep_Ahorros.Sdo_Disponible.
            TSaldos.Cuo = TSaldos.Cuo + rep_Ahorros.Cuota.
            TSaldos.Itr = TSaldos.Itr + rep_Ahorros.Int_Pagar + rep_Ahorros.Sdo_canje.

            IF TSaldos.Dis = 0 THEN
                DELETE TSaldos.

            CREATE TAhorros.

            BUFFER-COPY rep_Ahorros TO TAhorros.

            RELEASE TSaldos.
        END.
    END.

    RUN BSaldos(INPUT 1).

    FOR EACH TAhorros:
        RUN Detalle_Ahorro(INPUT "AHO").
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BClientes wWin 
PROCEDURE BClientes :
DEFINE VAR TelRelacion LIKE Relaciones.Descripcion.
DEFINE VAR NitEmpresa LIKE Clientes.Nit.
DEFINE VAR vFecNacimiento AS CHARACTER.
DEFINE VAR vEdad AS INTEGER.

W_Texto = "                                           INFORMACIÓN GENERAL".
RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).
RUN Raya(INPUT "GEN").

W_Texto = "                                            DATOS PERSONALES".
RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).
RUN Raya(INPUT "GEN").

w_texto = "Nombre completo    : " + tclientes.nombre + " " + tclientes.apellido1 + " " + tclientes.apellido2.
RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).

IF tclientes.fec_nacimiento <> ? THEN DO:
    vFecNacimiento = STRING(tclientes.fec_nacimiento,"99/99/9999").

    RUN edad IN w_manija (INPUT tClientes.fec_nacimiento,
                          OUTPUT vEdad) NO-ERROR.
END.

w_texto = "Fecha de nacimiento: " + STRING(vFecNacimiento,"X(30)") + "Edad: " + STRING(vEdad) + " años".
RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).

w_texto = "Fecha de Ingreso   : " + STRING(STRING(Tclientes.Fec_ingreso,"99/99/9999"),"X(30)").

IF Tclientes.Fec_fallecido NE ? THEN DO:
    W_Texto = w_texto + "F A L L E C I D O       : " + STRING(Tclientes.Fec_fallecido,"99/99/9999").

    MESSAGE "Este cliente aparece como FALLECIDO fecha: " Tclientes.Fec_fallecido
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).

FIND FIRST ahorros WHERE ahorros.nit = TClientes.nit
                     AND ahorros.tip_ahorro = 4
                     AND ahorros.estado = 1 NO-LOCK NO-ERROR.
IF AVAILABLE ahorros THEN DO:
    IF ahorros.fec_apertura > TClientes.fec_ingreso THEN DO:
        w_texto = "Fecha de Reingreso : " + STRING(STRING(ahorros.fec_apertura,"99/99/9999"),"X(30)").
        RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).
    END.
END.

                                           
RUN raya(INPUT "GEN").


W_Texto = "                                              SEGMENTACION".
RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).
RUN Raya(INPUT "GEN").

FIND FIRST Agencias WHERE Agencias.Agencia EQ TClientes.Agencia NO-LOCK NO-ERROR.
IF AVAILABLE Agencias THEN
    gTexto = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
ELSE
    gTexto = "Agencia Inconsistente".

W_Texto = "Agencia                 : " + STRING(gTexto,"X(30)").

IF TClientes.Tipo_Cliente GT 2 THEN
    RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).

CASE TClientes.Tipo_Vinculo:
    WHEN 1 THEN gTexto = "Asociado".
    WHEN 2 THEN gTexto = "Cliente No Asociado".
    WHEN 3 THEN gTexto = "Tercero".
    WHEN 4 THEN gTexto = "Proveedor".
END CASE.

W_Texto = "Tipo de Vínculo         : " + STRING(gTexto,"X(30)").
RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).

IF TClientes.Tipo_Cliente LT 3 THEN DO:
    FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ TClientes.Cod_Empresa NO-LOCK NO-ERROR.
    IF AVAILABLE Empresas THEN
        gTexto = Empresas.ALIAS_empresa.
    ELSE
        gTexto = "".

    W_Texto = "Empresa Recaudo         : " + STRING(gTexto,"X(30)").
    RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).
END.

FIND FIRST facultades WHERE facultades.agencia = TClientes.agencia
                        AND facultades.tipo = "F"
                        AND facultades.codigo = STRING(TClientes.facultad,"99") NO-LOCK NO-ERROR.
IF AVAILABLE facultades THEN DO:
    gTexto = facultades.nombre.

    FIND FIRST facultades WHERE facultades.agencia = TClientes.agencia
                            AND facultades.tipo = "D"
                            AND facultades.codigo = STRING(TClientes.facultad,"99") + STRING(clientes.departamento,"999") NO-LOCK NO-ERROR.
    IF AVAILABLE facultades THEN
        gTexto = gTexto + " - " + facultades.nombre.
END.

IF AVAILABLE facultades THEN DO:
    W_Texto = "Facultad/Depto          : " + STRING(gTexto,"X(70)").
    RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).
END.

gtexto = STRING(Tclientes.fecPagare,"99/99/9999").
W_Texto = "Fecha Pagaré            : " + STRING(gTexto,"X(30)").
RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).

FIND FIRST creditos WHERE creditos.nit = TClientes.nit
                      AND creditos.estado = 2 NO-LOCK NO-ERROR.
IF AVAILABLE creditos THEN DO:
    IF TClientes.fecPagare = ? THEN
        MESSAGE "La fecha de firma de pagaré para este Asociado aun no ha sido diligenciada." SKIP
                "Por favor proceda a ingresar esta información por el módulo de afiliaciones."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
    IF ADD-INTERVAL(clientes.fecPagare,5,"years") <= w_fecha THEN
        MESSAGE "El pagaré para este Asociado se encuentra desactualizado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/*Ubicacion*****************************************************************/
RUN Raya (INPUT "GEN").

W_Texto = "                                               UBICACIÓN".
RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).

RUN Raya (INPUT "GEN").

IF TClientes.Tipo_Cliente LT 3 THEN DO:
    RUN Buscar_Ubicacion(INPUT TClientes.Lugar_Residencia,
                         OUTPUT gTexto).

    W_Texto = "Lugar de Residencia     : " + STRING(gTexto,"X(40)").
    RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).

    W_Texto = "Dirección de Residencia : " + STRING(TClientes.DIR_Residencia,"X(40)").
    RUN Grabar_Info(INPUT "GEN",INPUT W_Texto).

    ASSIGN W_Texto = "Teléfono de Residencia  : " + STRING(TClientes.Tel_Residencia,"X(30)").
    RUN Grabar_Info (INPUT "GEN",INPUT W_Texto).

    W_Texto = "Teléfono Celular        : " + STRING(TClientes.Celular,"X(30)").
    RUN Grabar_Info (INPUT "GEN",INPUT W_Texto).
END.

W_Texto = "Correo Electrónico      : " + STRING(TClientes.email,"X(30)").
RUN Grabar_Info (INPUT "GEN",INPUT W_Texto).

FOR EACH Relaciones WHERE Relaciones.Nit EQ TClientes.Nit
                      AND Relaciones.Cod_Relacion EQ 10 NO-LOCK:
    IF AVAILABLE Relaciones THEN DO:
        FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN
            ASSIGN gTexto = Clientes.Nombre
                   TelRelacion = Relaciones.Descripcion.
        ELSE
            gTexto = "".

        W_Texto = "Empresa No Convenio     : " + STRING(gTexto,"X(30)") + "  Telefono            : " +  Relaciones.Descripcion.
    END.

    RUN Grabar_Info (INPUT "GEN",
                     INPUT W_Texto).
END.

IF TClientes.Reportado_Procredito THEN DO:
    W_Texto = "                                          ------------------------------".
    RUN Grabar_Info (INPUT "GEN",
                     INPUT W_Texto).

    W_Texto = "                                          CLIENTE REPORTADO A PROCREDITO".
    RUN Grabar_Info (INPUT "GEN",
                     INPUT W_Texto).

    W_Texto = "                                          ------------------------------".
    RUN Grabar_Info (INPUT "GEN",
                     INPUT W_Texto).
END.

RUN Raya (INPUT "ECO").

W_Texto = "                                          INFORMACIÓN ECONOMICA".
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

RUN Raya (INPUT "ECO").

W_Texto = "                          Ingresos                                             Egresos".
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

RUN Raya (INPUT "ECO").

W_Texto = "Salario              : " + STRING(TClientes.Salario,">>>>>,>>>,>>9") + "                   " + "Gastos Familiares    :" +
          STRING(TClientes.Gto_Familiar,">>>>>,>>>,>>9").
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

W_Texto = "Arriendos            : " + STRING(TClientes.Ing_Arriendos,">>>>>,>>>,>>9") + "                   " + "Arriendos            :" +
          STRING(TClientes.Gto_Arriendo,">>>>>,>>>,>>9").
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

W_Texto = "Financieros          : " + STRING(TClientes.Ing_Financieros,">>>>>,>>>,>>9") + "                   " + "Obligacion           :" +
          STRING(TClientes.Gto_Obligacion,">>>>>,>>>,>>9").
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

W_Texto = "Honorarios           : " + STRING(TClientes.Ing_Honorarios,">>>>>,>>>,>>9").
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

W_Texto = "Otros Ingresos       : " + STRING(TClientes.Ing_Otros,">>>>>,>>>,>>9").
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

W_Texto = "Total Ingresos       : " + STRING(TClientes.Salario + TClientes.Ing_Arriendos + TClientes.Ing_Financieros + TClientes.Ing_Honorarios +
          TClientes.Ing_Otros,">>>>>,>>>,>>9") + "                   " + "Total Gastos         :" + STRING(TClientes.Gto_Familiar + TClientes.Gto_Arriendo +
          TClientes.Gto_Obligacion,">>>>>,>>>,>>9").
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

RUN Raya (INPUT "ECO").

W_Texto = "                          Activos                                              Pasivos".
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

RUN Raya (INPUT "ECO").

W_Texto = "Propiedades          : " + STRING(TClientes.Act_Casa,">>>>>,>>>,>>9") + "                   " + "Obligaciones         :" +
          STRING(TClientes.Sdo_Obligaciones,">>>>>,>>>,>>9").
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

W_Texto = "Vehiculos            : " + STRING(TClientes.Act_Vehiculo,">>>>>,>>>,>>9").
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

W_Texto = "Inversiones          : " + STRING(TClientes.Act_Inversion,">>>>>,>>>,>>9").
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

W_Texto = "Total Activos        : " + STRING(TClientes.Act_Casa + Clientes.Act_Vehiculo + TClientes.Act_Inversion,">>>>>,>>>,>>9") + "                   " +
          "Total Pasivos        :" + STRING(TClientes.Sdo_Obligaciones,">>>>>,>>>,>>9").
RUN Grabar_Info (INPUT "ECO",
                 INPUT W_Texto).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BCodeudando wWin 
PROCEDURE BCodeudando :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TRelaciones WHERE TRelaciones.CodA EQ 9: DELETE TRelaciones. END.

FOR EACH Relaciones WHERE Relaciones.Nit_Relacion EQ TClientes.Nit NO-LOCK:
  IF Relaciones.Estado         EQ 1 AND
     Relaciones.Clase_Producto EQ 2 THEN DO:
    /*RUN Buscar_Varios (INPUT 3, INPUT Relaciones.Cod_relacion, OUTPUT gTexto).*/
    CREATE TRelaciones.
    ASSIGN TRelaciones.TpoR = Relaciones.Cod_Relacion
           TRelaciones.TipR = "CODEUDOR"
           TRelaciones.NitR = Relaciones.Nit
           TRelaciones.ParR = Relaciones.Descripcion
           TRelaciones.CodA = 9.
    FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN 
       ASSIGN TRelaciones.NomR = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
              TRelaciones.TelR = Clientes.Tel_Residencia.

    FIND Creditos WHERE Creditos.Nit EQ Relaciones.Nit AND 
         STRING(DECIMAL(Creditos.Num_Credito))         EQ Relaciones.Cuenta AND
                        Creditos.Estado                EQ 2 NO-LOCK NO-ERROR.
    IF AVAILABLE Creditos THEN
       ASSIGN TRelaciones.CueR = STRING(DECIMAL(Creditos.Num_Credito))
              TRelaciones.ClaR = "CRE"
              TRelaciones.CodR = Creditos.Cod_Credito.
    ELSE DELETE TRelaciones.
  END.
END.

 RUN Raya (INPUT "COD").
 W_Texto = "                                         Codeudando A:".
 RUN Grabar_Info (INPUT "COD", INPUT W_Texto).
 RUN Raya (INPUT "COD").
 FOR EACH TRelaciones WHERE TRelaciones.CodA EQ 9 BY TRelaciones.TipR:
     W_Texto = STRING(TRelaciones.TipR,"X(25)") + " " +
               TRIM(STRING(TRelaciones.NitR,"X(14)")) + " " +
               TRIM(STRING(TRelaciones.NomR,"X(30)")) + " (" +
               TRIM(STRING(TRelaciones.ParR,"X(15)")) + ") - Tel: " + TRIM(STRING(TRelaciones.TelR)).
     RUN Grabar_Info (INPUT "COD", INPUT W_Texto).
     IF TRelaciones.CueR NE ? THEN DO:
        W_Texto =  "                          Producto: " + TRIM(STRING(TRelaciones.CLaR)) + ": " +
                                            TRIM(STRING(TRelaciones.CueR,"X(14)")). 
        RUN Grabar_Info (INPUT "COD", INPUT W_Texto).
     END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BCreditos wWin 
PROCEDURE BCreditos :
DEFINE VAR IdCas AS LOGICAL INITIAL NO.

W_Texto = "------------------------------------------------------------------------------------------------------------------------".
RUN Grabar_Info(INPUT "CRE",INPUT W_Texto).

W_Texto = "INFORMACION DETALLADA DE LOS CRÉDITOS".
RUN Grabar_Info (INPUT "CRE",INPUT W_Texto).

WOcur = 0.

FOR EACH TSaldos WHERE TSaldos.Cla EQ 2:
    DELETE TSaldos.
END.

EMPTY TEMP-TABLE TCreditos.

IdAbo = NO.

CmensajeCJ:SCREEN-VALUE IN FRAME F_Consulta = "                            ".

IF tgFecCorte = NO THEN DO:
    FOR EACH Creditos WHERE Creditos.Nit EQ TClientes.Nit NO-LOCK BREAK BY Creditos.Cod_Credito:
        IF Creditos.Estado EQ 2 THEN DO:
            FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
            IF AVAILABLE Pro_Creditos THEN
                gTexto = Pro_Creditos.Nom_Producto.

            FIND FIRST TSaldos WHERE TSaldos.Cla EQ 2
                                 AND TSaldos.Cod EQ Creditos.Cod_Credito NO-ERROR.
            IF NOT AVAILABLE TSaldos THEN DO:
                CREATE TSaldos.
                ASSIGN TSaldos.Cla = 2
                       TSaldos.Cod = Creditos.Cod_Credito
                       TSaldos.Nom = gTexto
                       TSaldos.fecGiro = creditos.fec_desembolso.

                IF creditos.fec_pago <> ? THEN
                    TSaldos.fecVcto = STRING(creditos.fec_pago,"99/99/9999").
                ELSE
                    TSaldos.fecVcto = "No asignada".
            END.

            ASSIGN TSaldos.Cuo = TSaldos.Cuo + Creditos.Cuota
                   TSaldos.Itr = TSaldos.Itr + Creditos.Int_morCobrar
                   TSaldos.Tas = TSaldos.Tas + Creditos.Tasa
                   WOcur = WOcur + 1
                   TSaldos.cuoVencidas = Tsaldos.cuoVencidas + creditos.cuo_atraso.

            TSaldos.Dis = TSaldos.Dis + Creditos.Sdo_Capital.
            
            IF TSaldos.cuotasXpagar < creditos.plazo - creditos.Cuo_Pagadas THEN
                TSaldos.cuotasXpagar = creditos.plazo - creditos.Cuo_Pagadas.

            IF creditos.cod_credito = 123 THEN
                TSaldos.cupoDisponible = TSaldos.cupoDisponible + (creditos.monto - creditos.sdo_Capital).

            CREATE TCreditos.
            BUFFER-COPY creditos TO TCreditos.

            /* Créditos congelados */
            IF creditos.detalle_estado = 2 THEN DO:

                MESSAGE "El crédito #"creditos.num_credito "-" gtexto + "," "se encuentra congelado." SKIP
                        "(Mensaje informativo)"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
        END.

        IF Creditos.Estado EQ 5 THEN DO:
            CmensajeCJ = "Credito No. " + STRING(Creditos.Num_Credito) + " Castigado".
            IdCas = YES.
        END.

        IF LAST-OF(Creditos.Cod_Credito) AND AVAIL(TSaldos) THEN DO:
            ASSIGN TSaldos.Tas = TSaldos.Tas / WOcur
                   WOcur = 0.

            RELEASE TSaldos.
        END.
    END.
END.
ELSE DO:
    FOR EACH rep_Creditos WHERE rep_Creditos.Nit = TClientes.Nit AND rep_creditos.fecCorte = fechaCorte NO-LOCK BREAK BY rep_Creditos.Cod_Credito:
        FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = rep_Creditos.Cod_Credito NO-LOCK NO-ERROR.
        IF AVAILABLE Pro_Creditos THEN
            gTexto = Pro_Creditos.Nom_Producto.

        FIND FIRST TSaldos WHERE TSaldos.Cla = 2
                             AND TSaldos.Cod = rep_Creditos.Cod_Credito NO-ERROR.
        IF NOT AVAILABLE TSaldos THEN DO:
            CREATE TSaldos.
            ASSIGN TSaldos.Cla = 2
                   TSaldos.Cod = rep_Creditos.Cod_Credito
                   TSaldos.Nom = gTexto
                   TSaldos.fecGiro = rep_creditos.fec_desembolso.

            IF rep_creditos.fec_pago <> ? THEN
                TSaldos.fecVcto = STRING(rep_creditos.fec_pago,"99/99/9999").
            ELSE
                TSaldos.fecVcto = "No asignada".
        END.

        ASSIGN TSaldos.Cuo = TSaldos.Cuo + rep_Creditos.Cuota
               TSaldos.Itr = TSaldos.Itr + rep_Creditos.Int_morCobrar
               TSaldos.Tas = TSaldos.Tas + rep_Creditos.Tasa
               WOcur = WOcur + 1
               TSaldos.cuoVencidas = Tsaldos.cuoVencidas + rep_creditos.cuo_atraso.

        TSaldos.Dis = TSaldos.Dis + rep_Creditos.Sdo_Capital.

        IF TSaldos.cuotasXpagar < rep_creditos.plazo - rep_creditos.Cuo_Pagadas THEN
            TSaldos.cuotasXpagar = rep_creditos.plazo - rep_creditos.Cuo_Pagadas.

        CREATE TCreditos.
        BUFFER-COPY rep_creditos TO TCreditos.

        /* Créditos congelados */
        IF rep_creditos.detalle_estado = 2 THEN DO:
            MESSAGE "El crédito #"creditos.num_credito "al día" STRING(fechaCorte,"99/99/9999") "se encontraba congelado. (Mensaje informativo)"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.

        IF rep_Creditos.Estado EQ 5 THEN DO:
            CmensajeCJ = "Credito No. " + STRING(rep_Creditos.Num_Credito) + " Castigado".
            IdCas = YES.
        END.

        IF LAST-OF(rep_Creditos.Cod_Credito) AND AVAIL(TSaldos) THEN DO:
            ASSIGN TSaldos.Tas = TSaldos.Tas / WOcur
                   WOcur = 0.

            RELEASE TSaldos.
        END.
    END.
END.


RUN BSaldos(INPUT 2).

ASSIGN CMensajeCJ = "" WHEN NOT IdCas.

idAbo = FALSE.

FOR EACH TCreditos: 
    IF TCreditos.Abogado THEN DO:
        idAbo = TRUE.
        CMensajeCJ = CMensajeCJ + " COBRO JURIDICO. Cdto: " + STRING(TCreditos.Num_Credito) + " - Abogado: " + TCreditos.Nom_Juzgado.
    END.

    RUN Detalle_Credito.
END.

IF NOT IdCas AND NOT IdAbo THEN
    HIDE CMensajeCJ IN FRAME F_Consulta.
ELSE
    DISPLAY CMensajeCj WITH FRAME F_Consulta.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BGarantias wWin 
PROCEDURE BGarantias :
FOR EACH TGarantias: DELETE TGarantias. END.
FOR EACH TCreditos:
    FOR EACH Garantias WHERE 
         Garantias.Num_Credito EQ TCreditos.Num_Credito AND 
         Garantias.Estado EQ 1 NO-LOCK:
       CREATE TGarantias.
       ASSIGN TGarantias.GTip = 1
              TGarantias.GIde = Garantias.Identificacion_Bien
              TGarantias.GNCr = Garantias.Num_Credito
              TGarantias.GNom = Garantias.Nom_Bien
              TGarantias.GVal = Garantias.Val_Bien.
       IF Garantias.Fec_FinSeguro NE ? THEN      
          TGarantias.GVSg = STRING(Garantias.Fec_FinSeguro).
       IF Garantias.Fec_VctoImpuesto NE ?THEN
          TGarantias.GPIm = STRING(Garantias.Fec_VctoImpuesto).
       IF Garantias.Fec_ProxAvaluo NE ? THEN
          TGarantias.GPAv = STRING(Garantias.Fec_ProxAvaluo).
       WOcur = WOcur + 1.
    END.
END.

  /*FOR EACH Relaciones WHERE Relaciones.Nit EQ TClientes.Nit NO-LOCK:
     IF Relaciones.Clase_Producto EQ 2 AND
        Relaciones.Cod_Relacion   EQ 11 AND
        Relaciones.Estado         EQ 1  THEN DO:
        FIND TGarantias WHERE 
             TGarantias.GTip EQ 2 NO-ERROR.
        IF NOT AVAILABLE TGarantias THEN DO: 
           CREATE TGarantias.
           ASSIGN TGarantias.GTip = 2
                  WOcur = WOcur + 1.
        END.
     END.
  END. Comentariado Enero 25/06 Gaer, el Informe ya es por codeudores*/
 
FOR EACH TGarantias BREAK BY TGarantias.GTip:
  IF FIRST-OF(TGarantias.Gtip) AND TGarantias.GTip EQ 1 THEN DO:
    W_Texto = "------------------------------------------------------------------------------------------------------------------------".
    RUN Grabar_Info (INPUT "ADM", INPUT W_Texto).     
    W_Texto = "GARANTIAS ADMISIBLES".
    RUN Grabar_Info (INPUT "ADM", INPUT W_Texto).     
    W_Texto = "NUM.CREDI ID.GARANTIA  NOMBRE GARANTIA   VALOR GARANTIA   FEC.VEN.SEGURO  VENC.IMPUESTO  PROX.AVALUO ".
    RUN Grabar_Info (INPUT "ADM", INPUT W_Texto).     
    WOcur = 0.
  END.
  IF FIRST-OF(TGarantias.Gtip) AND TGarantias.GTip EQ 2 THEN DO:
    W_Texto = "------------------------------------------------------------------------------------------------------------------------".
    RUN Grabar_Info (INPUT "ADM", INPUT W_Texto).     
    W_Texto = "GARANTIAS PERSONALES".
    RUN Grabar_Info (INPUT "ADM", INPUT W_Texto).     
    W_Texto = "NUM.CREDITO   NIT         NOMBRE                                TELEFONO".
    RUN Grabar_Info (INPUT "ADM", INPUT W_Texto).     
  END.
  IF TGarantias.GTip EQ 1 THEN DO:
     W_Texto = STRING(TGarantias.GNCr,"999999999") + " " +
               STRING(TGarantias.GIde,"X(12)") + " " +
               STRING(TGarantias.GNom,"X(15)") + "  " +
               STRING(TGarantias.GVal,">>,>>>,>>>,>>9") + "       " +
               STRING(TGarantias.GVsg) + "        " +
               STRING(TGarantias.GPIm) + "      " +
               STRING(TGarantias.GPAv).
  END.
  ELSE DO:
     W_Texto = STRING(TGarantias.GNCr,"999999999") + " " +
               STRING(TGarantias.GNit,"X(14)") + " " +
               STRING(TGarantias.GNnt,"X(30)") + "         " +
               STRING(TGarantias.GTel,"X(14)").
  END.
  RUN Grabar_Info (INPUT "ADM", INPUT W_Texto).
END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BHojaVida wWin 
PROCEDURE BHojaVida PRIVATE :
FOR EACH THojaVida: DELETE THojaVida. END.
ASSIGN FRAME F_Opciones Fini FFin.

FOR EACH Hoja_Vida WHERE Hoja_Vida.Nit EQ TClientes.Nit AND
         Hoja_Vida.Tipo EQ 18 NO-LOCK: 
    IF Hoja_Vida.Fec_Grabacion GE FIni AND 
       Hoja_Vida.Fec_Grabacion LE FFin THEN DO:
       RUN Buscar_Varios (INPUT 18, INPUT Hoja_Vida.Codigo, OUTPUT gTexto).
       CREATE THojaVida.
       ASSIGN THojaVida.AsuH  = gTexto
              THojaVida.UsuH  = Hoja_Vida.Usuario        
              THojaVida.FecH  = Hoja_Vida.Fec_Grabacion  
              THojaVida.Cump  = Hoja_Vida.Asunto_cumplido
              THojaVida.DesH  = Hoja_Vida.Observacion    
              THojaVida.DocH  = Hoja_Vida.DoctoRefer.    
    END.
 END.

 RUN Raya (INPUT "HVA").
 W_Texto = "                                           HOJA DE VIDA".
 RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).

 RUN Raya (INPUT "HVA").

 FOR EACH THojaVida BY THojaVida.FecH:
     W_Texto = "Fec.Grabacion: " + STRING(THojaVida.FecH,"99/99/99") + " " +
               STRING(THojaVida.AsuH,"X(25)") + " - Cumplido?: " +
               STRING(THojaVida.Cump,"Si/No") + " - Usuario: " +
               STRING(THojaVida.UsuH,"X(4)") + " - Doc: " +
               STRING(THojaVida.DocH,"9999999999999").
     RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
     IF SUBSTRING(THojaVida.DesH,1,100) NE "" THEN DO:
         W_Texto = "             " + SUBSTRING(THojaVida.DesH,1,100).
         RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
     END.
     IF SUBSTRING(THojaVida.DesH,101,100) NE "" THEN DO:
         W_Texto = "             " + SUBSTRING(THojaVida.DesH,101,100).
         RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
     END.
     IF SUBSTRING(THojaVida.DesH,201,100) NE "" THEN DO:
         W_Texto = "             " + SUBSTRING(THojaVida.DesH,201,100).
         RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
     END.
     IF SUBSTRING(THojaVida.DesH,301,100) NE "" THEN DO:
         W_Texto = "             " + SUBSTRING(THojaVida.DesH,301,100).
         RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
     END.
     IF SUBSTRING(THojaVida.DesH,401,100) NE "" THEN DO:
         W_Texto = "             " + SUBSTRING(THojaVida.DesH,401,100).
         RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
     END.
     IF SUBSTRING(THojaVida.DesH,501,100) NE "" THEN DO:
         W_Texto = "             " + SUBSTRING(THojaVida.DesH,501,100).
         RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
     END.
     IF SUBSTRING(THojaVida.DesH,601,100) NE "" THEN DO:
         W_Texto = "             " + SUBSTRING(THojaVida.DesH,601,100).
         RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
     END.
     IF SUBSTRING(THojaVida.DesH,701,100) NE "" THEN DO:
         W_Texto = "             " + SUBSTRING(THojaVida.DesH,701,100).
         RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
     END.
     IF SUBSTRING(THojaVida.DesH,801,100) NE "" THEN DO:
         W_Texto = "            " + SUBSTRING(THojaVida.DesH,801,100).
         RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
     END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BMovInst wWin 
PROCEDURE BMovInst :
/*------------------------------------------------------------------------------
  Purpose:   Feb.23/05 Gaer.
  Invocado desde Procedures BSolicitud y BCreditos.
 ------------------------------------------------------------------------------*/
  
  FOR EACH THojaVida WHERE THojaVida.CodA EQ 8: DELETE THojaVida. END.
 
  ASSIGN FRAME F_Opciones Fini FFin.

  FOR EACH Mov_Instancias WHERE Mov_Instancias.Nit         EQ TClientes.Nit 
                            /*AND Mov_Instancias.Fec_Ingreso GE FIni          
                            AND Mov_Instancias.Fec_Ingreso LE FFin*/  NO-LOCK:   
     FIND FIRST Instancias WHERE
           /*Instancias.Tipo_Instancia  LE 3 AND*/
           Instancias.Instancia       EQ Mov_Instancias.Instancia AND
           Instancias.Tipo_Producto   EQ 2 AND
           Instancias.Estado          EQ 1 NO-LOCK NO-ERROR.
     CREATE THojaVida.
     ASSIGN THojaVida.AsuH  = Instancias.Nom_Instancia WHEN AVAIL(Instancias).
     ASSIGN THojaVida.InsH  = Instancias.Tipo_Instancia WHEN AVAIL(Instancias).
     ASSIGN THojaVida.UsuH  = Mov_Instancias.Usuario
            THojaVida.FecH  = Mov_Instancias.Fec_Ingreso
            THojaVida.Cump  = Mov_Instancias.Estado
            THojaVida.FecR  = Mov_Instancias.Fec_Retiro
            THojaVida.DesH  = Mov_Instancias.Descripcion
            THojaVida.DocH  = Mov_Instancias.Num_Solicitud
            THojaVida.CueH  = Mov_Instancias.Cuenta
            
            THojaVida.CodA  = 8.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BRelaciones wWin 
PROCEDURE BRelaciones :
FOR EACH TRelaciones: DELETE TRelaciones. END.
FOR EACH Relaciones WHERE Relaciones.Nit EQ TClientes.Nit NO-LOCK:
    IF Relaciones.Estado NE 1 THEN
       NEXT.
    RUN Buscar_Varios (INPUT 3, INPUT Relaciones.Cod_relacion, OUTPUT gTexto).
    CREATE TRelaciones.
    ASSIGN TRelaciones.TpoR = Relaciones.Cod_Relacion
           TRelaciones.TipR = gTexto
           TRelaciones.NitR = Relaciones.Nit_relacion
           TRelaciones.ParR = Relaciones.Descripcion.
    FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN 
       ASSIGN TRelaciones.NomR = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
              TRelaciones.TelR = "Csa: " + Clientes.Tel_Residencia + ", Empresa: " + Clientes.Tel_Comercial.
    IF Relaciones.Clase_Producto EQ 1 THEN DO:
       FIND FIRST Ahorros WHERE Ahorros.Nit EQ TClientes.Nit AND 
                        Ahorros.Cue_Ahorros EQ Relaciones.Cuenta NO-LOCK NO-ERROR.
       IF AVAILABLE Ahorros THEN
          ASSIGN TRelaciones.CueR = Ahorros.Cue_Ahorros
                 TRelaciones.ClaR = "AHO"
                 TRelaciones.CodR = Ahorros.Cod_Ahorro.
    END.
    ELSE IF Relaciones.Clase_Producto EQ 2 THEN DO:
       FIND FIRST Creditos WHERE Creditos.Nit     EQ TClientes.Nit AND 
            STRING(DECIMAL(Creditos.Num_Credito)) EQ Relaciones.Cuenta AND 
                           Creditos.Sdo_Capital   GT 0 NO-LOCK NO-ERROR.
       IF AVAILABLE Creditos THEN
          ASSIGN TRelaciones.CueR = STRING(DECIMAL(Creditos.Num_Credito))
                 TRelaciones.ClaR = "CRE"
                 TRelaciones.CodR = Creditos.Cod_Credito.
       ELSE DELETE TRelaciones.
    END.
 END.
 RUN Raya (INPUT "REL").
 W_Texto = "                                             RELACIONES".
 RUN Grabar_Info (INPUT "REL", INPUT W_Texto).
 RUN Raya (INPUT "REL").
 /*FOR EACH TRelaciones BY TRelaciones.TipR:
     W_Texto = STRING(TRelaciones.TipR,"X(25)") + " " +
               TRIM(STRING(TRelaciones.NitR,"X(14)")) + " " +
               TRIM(STRING(TRelaciones.NomR,"X(30)")) + " (" +
               TRIM(STRING(TRelaciones.ParR,"X(15)")) + ") - Tel: " + TRIM(STRING(TRelaciones.TelR)).
     RUN Grabar_Info (INPUT "REL", INPUT W_Texto).
     IF TRelaciones.CueR NE ? THEN DO:
        W_Texto =  "                          Producto: " + TRIM(STRING(TRelaciones.CLaR)) + ": " +
                                            TRIM(STRING(TRelaciones.CueR,"X(14)")). 
        RUN Grabar_Info (INPUT "REL", INPUT W_Texto).
     END.
 END.*/
 
 FOR EACH TRelaciones BREAK BY TRelaciones.TipR:
     IF FIRST-OF(TRelaciones.TipR) THEN DO:
        W_Texto = STRING(TRelaciones.TipR,"X(25)").
        RUN Grabar_Info (INPUT "REL", INPUT W_Texto).
        W_Texto = "------------------------------------------------------------------------------------------------------------------------".
        RUN Grabar_Info (INPUT "REL", INPUT W_Texto).
     END.
     W_Texto = STRING(TRelaciones.NitR,"X(14)") + " " +
               STRING(TRelaciones.NomR,"X(30)") + " (" +
               TRIM(STRING(TRelaciones.ParR,"X(15)")) + ") - Tel: " + TRIM(STRING(TRelaciones.TelR)).
     RUN Grabar_Info (INPUT "REL", INPUT W_Texto).
     IF TRelaciones.CueR NE ? THEN DO:
        W_Texto =  "               Pdto Relacionado: " + TRIM(STRING(TRelaciones.CLaR)) + ": " +
                                            TRIM(STRING(TRelaciones.CueR,"X(14)")). 
        RUN Grabar_Info (INPUT "REL", INPUT W_Texto).
     END.
     IF LAST-OF(TRelaciones.TipR) THEN DO:
         W_Texto = "------------------------------------------------------------------------------------------------------------------------".
         RUN Grabar_Info (INPUT "REL", INPUT W_Texto).
     END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BSaldos wWin 
PROCEDURE BSaldos :
DEFINE INPUT PARAMETER WCLa AS INTEGER FORMAT "9".
DEFINE VAR TPon AS DECIMAL FORMAT ">>>,>>>,>>9.99999".
DEFINE VAR sumaCupoDisponible AS DECIMAL.

ASSIGN gDis = 0
       gCuo = 0.

FOR EACH TSaldos WHERE TSaldos.Cla EQ WCla BREAK BY TSaldos.Cla BY TSaldos.Cod:
    ASSIGN gDis = gDis + TSaldos.Dis
           gCuo = gCuo + TSaldos.Cuo
           TPon = TPon + (TSaldos.Dis * TSaldos.Tas).

    sumaCupoDisponible = sumaCupoDisponible + TSaldos.cupoDisponible.

    IF FIRST-OF(TSaldos.Cla) THEN DO:
        RUN Raya(INPUT "SDO").

        CASE WCla:
            WHEN 1 THEN W_Texto = "                                            SALDOS DE AHORRO".
            WHEN 2 THEN W_Texto = "                                            SALDOS DE CREDITO".
            WHEN 4 THEN W_Texto = "                                         SOLICITUDES EN ESTUDIO".
            WHEN 5 THEN W_Texto = "                                            CUENTAS POR COBRAR".
        END CASE.

        RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).

        /* Encabezados */
        RUN Raya (INPUT "SDO").

        IF WCla EQ 1 THEN DO:
            W_Texto = "     PRODUCTO                                               |  FEC.APERT |  FEC.VCTO  | SALDO DISPONIBLE |    CUOTA    |".
            RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).

            W_Texto = "------------------------------------------------------------|------------|------------|------------------|-------------|".
            RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).
        END.

        IF WCla EQ 2 THEN DO:
            W_Texto = "     PRODUCTO                     | SALDO CAPITAL |  CUPO DISP. |    CUOTA    |FEC_PROX_PAG| CxP | C_MORA | FECHA GIRO |".
            RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).

            W_Texto = "----------------------------------|---------------|-------------|-------------|------------|-----|--------|------------|".
            RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).
        END.

        IF WCla EQ 4 THEN DO:
            W_Texto = "Cod.Producto  Nombre Producto                         Monto a Prestar                            Cuota".
            RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).
        END.

        IF WCla <> 1 AND WCla <> 2 AND WCla <> 4 THEN DO:
            W_Texto = "Cod.Producto  Nombre Producto                        Saldo Actual".
            RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).
        END.
        /* ----------- */
    END.

    /* Detalles */
    IF WCla = 1 THEN 
        W_Texto = STRING(TSaldos.Cod,"999") + " - " + STRING(TSaldos.Nom,"X(30)") + "                        | " +
                  STRING(TSaldos.fecApertura,"X(10)") + " | " +
                  STRING(tSaldos.FecVcto,"X(10)") + " |  " +
                  STRING(TSaldos.Dis,"$->,>>>,>>>,>>9") + " | " +
                  STRING(TSaldos.Cuo,"$->,>>>,>>9") + " |".

    IF WCla = 2 THEN
        W_Texto = STRING(TSaldos.Cod,"999") + " - " + STRING(TSaldos.Nom,"X(28)") + "|" +
                  STRING(TSaldos.Dis,"$->,>>>,>>>,>>9") + "|" +
                  STRING(TSaldos.cupoDisponible,"$->>>,>>>,>>9") + "|" +
                  STRING(TSaldos.Cuo,"$->>,>>>,>>9") + " | " +
                  STRING(TSaldos.fecVcto,"X(10)") + " | " +
                  STRING(TSaldos.cuotasXpagar,"999") + " |   " +
                  STRING(TSaldos.cuoVencidas,"99") + "   | " +
                  STRING(TSaldos.fecGiro,"99/99/9999") + " |".

    IF WCla = 5 THEN
        W_Texto = STRING(TSaldos.Cod,"99999999") + "-" + Tsaldos.nomAgencia + "  " + STRING(TSaldos.Nom,"X(30)") + "     " + STRING(TSaldos.Dis,"$->>,>>>,>>>,>>9").

    IF WCla <> 1 AND WCla <> 2 AND WCla <> 5 THEN
        W_Texto = STRING(TSaldos.Cod,"999") + " - " + STRING(TSaldos.Nom,"X(30)") + "                                                  |  " +
                  STRING(TSaldos.Dis,"$->,>>>,>>>,>>9") + " | " +
                  STRING(TSaldos.Cuo,"$->,>>>,>>9") + " |".
    
    RUN Grabar_Info (INPUT "SDO",
                     INPUT W_Texto).

    IF LAST-OF(TSaldos.Cla) THEN DO:
        IF wcla = 1 THEN DO:
            W_Texto = "--------------------------------------------------------------------------------------|------------------|-------------|".
            RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).

            W_Texto = "                                                                          TOTALES --> | " + STRING(gDis,"$->>,>>>,>>>,>>9") + " |" +  STRING(gCuo,"$->>,>>>,>>9") + " |".
            RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).
        END.
        ELSE DO:
            IF wcla = 2 THEN DO:
                W_Texto = "----------------------------------|---------------|-------------|-------------------------------------------------------".
                RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).

                W_Texto = "                       TOTALES--> |  " + STRING(gDis,"$->>>,>>>,>>9") + "|" + STRING(sumaCupoDisponible,"$->>>,>>>,>>9") + "|" +  STRING(gCuo,"$->>,>>>,>>9") + " |                                        |".
                RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).
            END.
            ELSE DO:
                IF WCla = 5 THEN DO:
                    W_Texto = "                                                 ----------------".
                    RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).

                    W_Texto = "                                     TOTALES --> " + STRING(gDis,"$->>,>>>,>>>,>>9").
                    RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).
                END.
                ELSE DO:
                    W_Texto = "                                                       ----------------  -------------".
                    RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).

                    W_Texto = "                                           TOTALES:     " + STRING(gDis,"$->>,>>>,>>>,>>9") + "   " +  STRING(gCuo,"$->>>,>>>,>>9").
                    RUN Grabar_Info (INPUT "SDO", INPUT W_Texto).
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BScoring wWin 
PROCEDURE BScoring :
DEFINE VARIABLE Tot_Puntaje AS INTEGER.
RUN Prc_LlenarScoring.r (INPUT TClientes.Nit).

FOR EACH TScoring: DELETE TScoring. END.
FOR EACH Scoring WHERE Scoring.Nit EQ TClientes.Nit AND Scoring.Fec_Scoring EQ TODAY BREAK BY Scoring.Codigo:
  CREATE TScoring.
  ASSIGN TScoring.VarS = Scoring.VARIABLE
         TScoring.VVaS = Scoring.Valor_Variable
         TScoring.PunS = Scoring.Puntaje
         TScoring.FecS = Scoring.Fec_Scoring
         TScoring.CodS = Scoring.Codigo.
END.

 RUN Raya (INPUT "SCO").
 W_Texto = "                                         INFORMACION SCORING".
 RUN Grabar_Info (INPUT "SCO", INPUT W_Texto).
 RUN Raya (INPUT "SCO").
 FOR EACH TScoring BREAK BY TScoring.CodS:
     IF FIRST-OF(TScoring.CodS) THEN DO:
        Tot_Puntaje = 0.
        RUN Buscar_Varios (INPUT 19, INPUT TScoring.CodS, OUTPUT gTexto).
        W_Texto = "Tipo de Scoring :  " + CAPS(gTexto).
        RUN Grabar_Info (INPUT "SCO", INPUT W_Texto).
        W_Texto = "Variable de Estudio   Valor Variable          Puntaje Obtenido".
        RUN Grabar_Info (INPUT "SCO", INPUT W_Texto).
     END.
     W_Texto = STRING(TScoring.VarS,"X(25)") + " " +
               STRING(TScoring.VVaS,"X(25)") + " " +
               STRING(TScoring.PunS,">>,>>9").
     RUN Grabar_Info (INPUT "SCO", INPUT W_Texto).
     Tot_Puntaje = Tot_Puntaje + TScoring.PunS.
     IF LAST-OF(TScoring.CodS) THEN DO:
       W_Texto = "                                     Total Puntos:  " + STRING(Tot_Puntaje,">>,>99").
       RUN Grabar_Info (INPUT "SCO", INPUT W_Texto).
     END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BSolicitud wWin 
PROCEDURE BSolicitud :
W_Texto = "------------------------------------------------------------------------------------------------------------------------".
RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).     
W_Texto = "INFORMACION DETALLADA DE SOLICITUDES".
RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).     

FOR EACH TSaldos WHERE TSaldos.Cla EQ 4: DELETE TSaldos. END.
FOR EACH TSolicitud: DELETE TSolicitud. END.

/* SOLICITUDES */
 FOR EACH Solicitud WHERE Solicitud.Nit EQ TClientes.Nit NO-LOCK:
    IF Solicitud.Estado LE 4 THEN DO: 
       CREATE TSolicitud.
       BUFFER-COPY Solicitud TO TSolicitud.
    END.
 END.
     
 RUN BSaldos(INPUT 4).
 FOR EACH TSolicitud: RUN Detalle_Solicitud. END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Informacion wWin 
PROCEDURE Buscar_Informacion :
EMPTY TEMP-TABLE TInfo.

i = 1.

RUN BClientes.
RUN BAhorros.
RUN BCreditos.
RUN cuentasXcobrar.

IF Id_AhoCanc THEN
    RUN Aho_Cancel.

IF Id_Solicitudes THEN
    RUN BSolicitud.

IF Id_CredCanc THEN
    RUN Cred_Cancel.

IF Id_GarAdm THEN
    RUN BGarantias.

IF Id_Relaciones THEN
    RUN BRelaciones.

IF Id_Codeudando THEN
    RUN BCodeudando.

IF Id_HojaVida THEN
    RUN BHojaVida.

IF Tg_Extras THEN
    RUN Extras_Creditos.

IF Tg_CtasAho THEN
    RUN EstCta_Ahorro.

IF idFacturaCupoRotativo = YES THEN DO:
    FIND FIRST facturacion WHERE facturacion.nit = clientes.nit
                             AND facturacion.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE facturacion THEN
        RUN facturaCupoRotativo.
    ELSE DO:
        MESSAGE "No se encuentra factura pendiente de cupo rotativo" SKIP
                "para este Asociado"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        idFacturaCupoRotativo:SCREEN-VALUE IN FRAME F_Opciones = "no".
        idFacturaCupoRotativo = FALSE.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Ubicacion wWin 
PROCEDURE Buscar_Ubicacion :
DEFINE INPUT PARAMETER WUbi LIKE Clientes.Lugar_Residencia.
DEFINE OUTPUT PARAMETER WNom AS CHARACTER FORMAT "X(30)".

FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(WUbi,1,5) NO-LOCK NO-ERROR.
IF AVAILABLE Ubicacion THEN
    ASSIGN WNom = Ubicacion.Nombre.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Varios wWin 
PROCEDURE Buscar_Varios :
DEFINE INPUT PARAMETER  VTipo LIKE Varios.Tipo.
DEFINE INPUT PARAMETER  VCod  LIKE Varios.Codigo.
DEFINE OUTPUT PARAMETER VNom LIKE Varios.Descripcion.

 FIND Varios WHERE Varios.Tipo EQ VTipo AND Varios.Codigo EQ VCod NO-LOCK NO-ERROR.
 IF AVAILABLE Varios THEN VNom = Varios.Descripcion.
 ELSE VNom = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cerrar_Excel wWin 
PROCEDURE Cerrar_Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SYSTEM-DIALOG PRINTER-SETUP. 
    PrinterName = SESSION:PRINTER-NAME.
    hWorkBooks:PrintOut(1,2,1,FALSE,PrinterName,).
    chExcelApp:displayalerts = FALSE.
    chExcelApp:Application:WorkBooks:CLOSE() NO-ERROR.
    chExcelApp:Application:QUIT NO-ERROR.
    RELEASE OBJECT hWorkBooks.
    RELEASE OBJECT chExcelApp.      
    RELEASE OBJECT chWorksheet.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cliente_Encontrado wWin 
PROCEDURE Cliente_Encontrado :
IF clientes.nombre <> "" THEN
    W_Nombre:SCREEN-VALUE IN FRAME F_Consulta = "Documento: " + Clientes.Nit + " - Nombre: " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
ELSE DO:
    FIND FIRST anexos_clientes WHERE anexos_clientes.nit = clientes.nit NO-LOCK NO-ERROR.
    IF AVAILABLE anexos_clientes THEN
        W_Nombre:SCREEN-VALUE IN FRAME F_Consulta = "Documento: " + Clientes.Nit + " - Nombre: " + anexos_Clientes.Nombre1 + " " + anexos_clientes.nombre2 + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
END.

ASSIGN FRAME F_Opciones
    Id_General
    W_CedCteC = Clientes.Nit.

FIND FIRST Ahorros WHERE Ahorros.Tip_Ahorro = 4
                     AND Ahorros.Nit = Clientes.Nit NO-LOCK NO-ERROR.
IF AVAILABLE Ahorros AND Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje > 0 THEN DO:
    FIND CURRENT Clientes NO-ERROR.

    ASSIGN Clientes.Estado = 1
           Clientes.Tipo_Vinculo = 1.
END.

FIND FIRST ListaNegra WHERE ListaNegra.Nit EQ Clientes.Nit
                        AND (listaNegra.estado = 1 OR listaNegra.estado = 3) NO-ERROR.
IF AVAILABLE ListaNegra THEN DO:
    ASSIGN ListaNegra.Id_HaVenido = YES
           ListaNegra.Fec_HaVenido = W_Fecha.

    MESSAGE "Este  número  de identificación ha sido encontrado" SKIP
            "en la lista de asociados suspendidos de FODUN:" SKIP(2)
            "Nº de crédito y fecha de suspensión:" listaNegra.observacion SKIP
            "Fecha de vencimiento de la suspensión:" listaNegra.fec_exclusion
        VIEW-AS ALERT-BOX WARNING TITLE "Activo en Lista de Suspendidos".

    RELEASE listaNegra.
END.

/*RUN validarUsuarioSarlaft.r (INPUT Clientes.Nit, OUTPUT W_Sarlaft) NO-ERROR.*/

IF Clientes.Tipo_Cliente = 2 AND ADD-INTERVAL(clientes.fec_nacimiento,18,"years") <= w_fecha AND Clientes.Estado = 1 THEN
    MESSAGE "Este cliente ha pasado la mayoria de edad y" SKIP
            "se encuentra marcado como menor"
        VIEW-AS ALERT-BOX WARNING.

FIND CURRENT clientes NO-LOCK NO-ERROR.

EMPTY TEMP-TABLE TInfo.
EMPTY TEMP-TABLE TClientes.

BUFFER-COPY Clientes TO TClientes.

RUN Buscar_Informacion.

EMPTY TEMP-TABLE Tmp.

RUN Mostrar_Informacion.

FOR EACH Tmp BY Tmp.Reg:
    W_Ok = Pantalla:ADD-LAST(Tmp.Tex).
END.

OPEN QUERY BInfo FOR EACH Tmp /*WHERE SUBSTRING(TEx,1,1) NE "-"*/ NO-LOCK INDEXED-REPOSITION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
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

OCXFile = SEARCH( "W-ConsultaGeneral.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "W-ConsultaGeneral.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Creacion-Pqr wWin 
PROCEDURE Creacion-Pqr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
  ASSIGN P_Nit = Buscar:SCREEN-VALUE IN FRAME f_consulta.
         
  
  IF p_nit <> '' AND CAN-FIND(clientes WHERE clientes.nit = p_nit) THEN DO:
     RUN pRcbeDtos IN h_w_captura-pqr(P_Nit,w_usuario,STRING(w_agencia)).
     ASSIGN WWin:SENSITIVE = FALSE.
     RUN viewObject IN h_w_captura-pqr.

     /*RUN w_captura-pqr.r (INPUT P_Nit, INPUT w_usuario, INPUT w_agencia, INPUT w_Nom_Agencia,INPUT w_ciudad).*/
  END.
  ELSE DO:
    MESSAGE 'No Disponible sin Tercero Valido'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  /*
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cred_Cancel wWin 
PROCEDURE Cred_Cancel :
/*------------------------------------------------------------------------------
  Purpose:     Historial con los creditos ya cancelados.  
  Notes:       Mayo 27/05 GAER
------------------------------------------------------------------------------*/
DEFI VAR W_UltPago LIKE Creditos.Sdo_Capital INIT 0.
DEFI VAR W_UltMora LIKE Creditos.Sdo_Capital INIT 0.
DEFI VAR W_Todos   AS LOG INIT FALSE.

MESSAGE "Si desea Solo Los Cancelados, con SI," SKIP
        "         Solo los Castigados, con NO,"   SKIP
        "     Cancelados y Castigados, con CANCELAR"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "Creditos Cancelados/Castigados" 
    UPDATE RptaCC AS LOGICAL.

W_Texto = "------------------------------------------------------------------------------------------------------------------------".
RUN Grabar_Info (INPUT "CRC", INPUT W_Texto).

IF RptaCC THEN
   W_Texto = "DETALLE DE LOS CREDITOS YA CANCELADOS :". 
ELSE IF NOT RptaCC THEN
   W_Texto = "DETALLE DE LOS CREDITOS CASTIGADOS :".
ELSE ASSIGN W_Texto = "DETALLE DE LOS CREDITOS YA CANCELADOS Y CASTIGADOS:"
            W_Todos = TRUE.

RUN Grabar_Info (INPUT "CRC", INPUT W_Texto).  

W_Texto = "------------------------------------------------------------------------------------------------------------------------".
RUN Grabar_Info (INPUT "CRC", INPUT W_Texto).

W_Texto = "Producto            Pdad Plazo   Vlr-Cuota    Vlr-Monto F-Desemb. F-Cancel. %-Tasa Int  Vr.Ultimo Pago  Vr.Int-Mora Agen".
RUN Grabar_Info (INPUT "CRC", INPUT W_Texto).  

W_Texto = "------------------------------------------------------------------------------------------------------------------------".
RUN Grabar_Info (INPUT "CRC", INPUT W_Texto).

WOcur = 0.

FOR EACH Creditos WHERE  Creditos.Nit EQ TClientes.Nit NO-LOCK BY Creditos.Fec_Desemb:
  IF (Creditos.Estado EQ 3 AND RptaCC) 
  OR (Creditos.Estado EQ 5 AND NOT RptaCC)
  OR ((Creditos.Estado EQ 3 OR Creditos.Estado EQ 5) AND W_Todos) THEN DO:
     ASSIGN W_Texto = "Num.Cred : " + STRING(Creditos.Num_Credito) + " >> Num.Pagare: " + STRING(Creditos.Pagare).
     RUN Grabar_Info (INPUT "CRC", INPUT W_Texto). 
     W_Texto = "                    ". 
     FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Creditos THEN 
        ASSIGN W_Texto = STRING(Pro_Creditos.Nom_Producto,"X(20)").

     ASSIGN WOcur  = WOcur + 1.

     CASE Creditos.Per_Pago:
         WHEN 1 THEN W_Texto = W_Texto + " Sem ".
         WHEN 2 THEN W_Texto = W_Texto + " Dec ".
         WHEN 3 THEN W_Texto = W_Texto + " Qna ".
         WHEN 4 THEN W_Texto = W_Texto + " Mes ".
     END CASE.

     FIND LAST PlanPagos WHERE PlanPagos.Agencia   EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 1 NO-ERROR.
     IF AVAILABLE(PlanPagos) THEN 
        ASSIGN W_UltMora = PlanPagos.Int_MoraAcum + PlanPagos.Cargos_Acum
               W_UltPago = PlanPagos.Pagos_CapitalPdo + Pagos_IntPdo + Pagos_MoraPdo + Pagos_OtrosPdo.
     
     ASSIGN W_Texto = W_Texto + STRING(Creditos.Plazo,"99999")
                              + STRING(Creditos.Cuota,">>>>>>>,>>9")
                              + STRING(Creditos.Val_Desembolso,">>>>,>>>,>>9")        + " "
                              + STRING(Creditos.Fec_Desembolso,"99/99/9999") + " "
                              + STRING(Creditos.Fec_UltPago,"99/99/9999")    + " "
                              + STRING(Creditos.Tasa,">>9.99%")          + " "
                              + STRING(W_UltPago,"->>>>,>>>,>>9")             + " "
                              + STRING(W_UltMora,"->>>>,>>>,>>9").
     FIND LAST Mov_Credito WHERE Mov_Credito.agencia EQ Credito.Agencia AND
                                 Mov_Creditos.Cod_Credito EQ creditos.cod_credito AND
                                 Mov_Creditos.Num_Credito EQ Creditos.Num_Credito AND
                                 Mov_Creditos.Nit         EQ Creditos.Nit NO-LOCK NO-ERROR.

     ASSIGN W_Texto = W_Texto + (IF AVAILABLE Mov_credito THEN STRING(Mov_Credito.Ofi_Destino,"zz999") ELSE "").

     RUN Grabar_Info (INPUT "CRC", INPUT W_Texto). 

     IF Creditos.Estado EQ 5 THEN DO:
        W_Texto = "El anterior Credito fue Castigado en : " + STRING(Creditos.Fec_UltPago,"99/99/9999").
        RUN Grabar_Info (INPUT "CRC", INPUT W_Texto).
     END.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CuentasXcobrar wWin 
PROCEDURE CuentasXcobrar :
DEFI VAR NomEst AS CHAR FORM "X(10)".
DEFINE VAR sdoAnexosCxC AS DECIMAL INITIAL 0.
DEFINE VAR cont AS INTEGER.

FOR EACH TSaldos WHERE TSaldos.Cla EQ 5:
    DELETE TSaldos.
END.

FOR EACH agencias NO-LOCK:
    FOR EACH anexos WHERE anexos.agencia = agencias.agencia
                      AND anexos.nit = W_CedCteC
                      AND (anexos.cuenta = "16605003" OR anexos.cuenta = "16605004")
                      AND anexos.ano = YEAR(w_fecha) NO-LOCK BREAK BY anexos.cuenta:
        sdoAnexosCxC = sdoAnexosCxC + anexos.sdo_inicial.

        DO cont = 1 TO 12:
            sdoAnexosCxC = sdoAnexosCxC + anexos.db[cont] - anexos.cr[cont].
        END.

        IF LAST-OF(anexos.cuenta) THEN DO:
            IF sdoAnexosCxC <> 0 THEN DO:
                MESSAGE "Este Asociado tiene un saldo pendiente (" anexos.cuenta ")" SKIP
                        "por valor de" STRING(sdoAnexosCxC,"$->>>,>>>,>>9.99")
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.

                FIND FIRST TSaldos WHERE TSaldos.cod = DECIMAL(anexos.cuenta) NO-ERROR.
                IF NOT AVAILABLE TSaldos THEN DO:
                    CREATE TSaldos.
                    ASSIGN TSaldos.Cla = 5
                           TSaldos.Cod = DECIMAL(anexos.cuenta)
                           tSaldos.NomAgencia = CAPS(SUBSTRING(agencias.nombre,1,3))
                           TSaldos.Nom = cuentas.nombre.
                END.

                TSaldos.Dis = TSaldos.Dis + sdoAnexosCxC.

                sdoAnexosCxC = 0.
            END.
        END.
    END.
END.

RUN BSaldos(INPUT 5).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Detalle_Ahorro wWin 
PROCEDURE Detalle_Ahorro :
DEFI INPUT PARAM P_Tipo AS CHAR FORM "X(3)".

DEFI VAR zmensaje  AS CHARACTER FORMAT "X(80)".

FIND FIRST Agencias WHERE Agencias.Agencia EQ TAhorros.Agencia NO-LOCK NO-ERROR.
IF AVAILABLE Agencias THEN
    gTexto = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
ELSE
    gTexto = "Agencia Inconsistente".

FIND FIRST Pro_Ahorros WHERE Pro_ahorros.cod_ahorro EQ Tahorros.cod_ahorro NO-LOCK NO-ERROR.

RUN Raya(INPUT P_Tipo).

ASSIGN W_Texto = "Producto            : " + STRING(Pro_ahorros.cod_ahorro,"999") + " - " + STRING(Pro_Ahorro.Nom_Producto,"X(25)")
       W_Texto = W_Texto + FILL(" ", 73 - LENGTH(W_texto)) + "Agencia Matricula   : " + STRING(gTexto,"X(25)").

RUN Grabar_Info(INPUT P_Tipo,INPUT W_Texto).

W_Texto = "Número de Cuenta    : " + STRING(TAhorros.Cue_Ahorros).
 
IF tAhorros.num_formato <> "" THEN
    w_texto = w_texto + "/" + STRING(TAhorros.Num_Formato).

CASE Tahorros.FOR_Pago:
    WHEN 1 THEN gTexto = "Caja".
    WHEN 2 THEN gTexto = "Nómina".
    WHEN 3 THEN gTexto = "Débito Automático".
END CASE.

ASSIGN W_Texto = W_Texto + FILL(" ", 73 - LENGTH(W_texto)) + "Forma de pago       : " + gTexto.

RUN Grabar_Info(INPUT P_Tipo,INPUT W_Texto).

ASSIGN W_Texto = "Monto de Apertura   : " + TRIM(STRING(TAhorros.Monto_Apertura,"$>>>,>>>,>>>,>>9"))
       W_Texto = W_Texto +  FILL(" ", 73 - LENGTH(W_texto)) + "Fecha de Apertura   : " + STRING(TAhorros.Fec_Apertura,"99/99/9999").

RUN Grabar_Info(INPUT P_Tipo,
                INPUT W_Texto).

CASE TAhorros.Per_Liquidacion:
    WHEN 1 THEN gTexto = "Diario".
    WHEN 2 THEN gTexto = "Mensual".
    WHEN 3 THEN gTexto = "Trimestral".
    WHEN 4 THEN gTexto = "Semestral".
    WHEN 5 THEN gTexto = "Anual".
    WHEN 6 THEN gTexto = "Al Vencimiento".
END CASE.

IF tAhorros.fec_vencimiento <> ? THEN
    W_Texto = "Fecha Vencimiento   : " + STRING(TAhorros.Fec_Vencimiento,"99/99/9999").
ELSE
    W_Texto = "Fecha Vencimiento   : NA".

W_Texto = W_Texto + FILL(" ", 73 - LENGTH(W_texto)) + "Periodo Liquidación : " + gTexto.

RUN Grabar_Info(INPUT P_Tipo,INPUT W_Texto).

CASE TAhorros.Per_deduccion:
    WHEN 1 THEN gTexto = "Semanal".
    WHEN 2 THEN gTexto = "Decadal".
    WHEN 3 THEN gTexto = "Quincenal".
    WHEN 4 THEN gTexto = "Mensual".
    WHEN 8 THEN gTexto = "Semestral".
END CASE.

ASSIGN W_Texto = "Cuota               : " + TRIM(STRING(TAhorros.Cuota,"$>>>,>>>,>>>,>>9"))
       W_Texto = W_Texto + FILL(" ", 73 - LENGTH(W_texto)) + "Periodo Pago Cuota  : " + gTexto.

RUN Grabar_Info (INPUT P_Tipo, INPUT W_Texto).

IF TAhorros.Fec_UltTransaccion <> ? THEN
    W_Texto = "Fec.Ult.Transacción : " + STRING(TAhorros.Fec_UltTransaccion,"99/99/9999").
ELSE
    W_Texto = "Fec.Ult.Transacción : NA".

IF TAhorros.Fec_UltLiquidacion <> ? THEN
    W_Texto = W_Texto + FILL(" ", 73 - LENGTH(W_texto)) + "Fec.Ult.Liquidación : " + STRING(TAhorros.Fec_UltLiquidacion,"99/99/9999").
ELSE
    W_Texto = W_Texto + FILL(" ", 73 - LENGTH(W_texto)) + "Fec.Ult.Liquidación : NA".

RUN Grabar_Info (INPUT P_Tipo, INPUT W_Texto).

ASSIGN W_Texto = "Tasa                : " + TRIM(STRING(TAhorros.Tasa,">>9.99%"))
       W_Texto = W_Texto + FILL(" ", 73 - LENGTH(W_texto)) + "Plazo en Días       : " + TRIM(STRING(TAhorros.Plazo,">>>>9")).

RUN Grabar_Info (INPUT P_Tipo, INPUT W_Texto).

ASSIGN W_Texto = "Saldo Disponible    : " + TRIM(STRING(TAhorros.Sdo_Disponible,"$->>,>>>,>>>,>>9"))
       W_Texto = W_Texto + FILL(" ", 73 - LENGTH(W_texto)) + "Saldo en Canje      : " + TRIM(STRING(TAhorros.Sdo_Canje,"$>>>,>>>,>>>,>>9")).

RUN Grabar_Info (INPUT P_Tipo, INPUT W_Texto).

ASSIGN W_Texto = "Interés por Pagar   : " + TRIM(STRING(TAhorros.INT_Pagar,"$>>>,>>>,>>>,>>9"))
       W_Texto = W_Texto + FILL(" ", 73 - LENGTH(W_texto)) + "Interés Causado     : " + TRIM(STRING(TAhorros.INT_Causado,"$>>>,>>>,>>>,>>9")).

RUN Grabar_Info (INPUT P_Tipo, INPUT W_Texto).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Detalle_Credito wWin 
PROCEDURE Detalle_Credito :
DEFI VAR W_TSdoVdo LIKE Creditos.Sdo_Capital INIT 0.
DEFI VAR W_TCapVdo LIKE Creditos.Sdo_Capital INIT 0.
DEFI VAR W_SdoDeuda LIKE TCreditos.Honorarios INIT 0.
DEFI VAR W_CuoAtraso AS DEC FORM "->>9.99".
DEFI VAR calres LIKE tcreditos.categoria.
DEFINE VAR periodos AS CHARACTER.

FIND FIRST Agencias WHERE Agencias.Agencia EQ TCreditos.Agencia NO-LOCK NO-ERROR.
IF AVAILABLE Agencias THEN
    gTexto = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
ELSE
    gTexto = "Agencia Inconsistente".

FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ TCreditos.Cod_Credito NO-LOCK NO-ERROR.

RUN Raya(INPUT "CRE").

ASSIGN W_Texto = "Producto               : " + STRING(Pro_Creditos.Cod_Credito,"999") + " - " + STRING(Pro_Creditos.Nom_Producto,"X(25)")
       W_Texto = W_Texto + "   Agencia Matrícula      : " + STRING(gTexto,"X(30)").

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

ASSIGN W_Texto = "Pagaré / Nro.Crédito   : " + TRIM(TCreditos.Pagare) + "/" + STRING(TCreditos.Num_Credito).

IF TCreditos.cod_credito <> 123 THEN
    W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Número de solicitud    : " + STRING(TCreditos.Num_Solicitud).
ELSE DO:
    FIND FIRST tarjetas WHERE tarjetas.nit = TCreditos.nit
                          AND tarjetas.num_credito = TCreditos.num_credito
                          AND tarjetas.estado = "01" NO-LOCK NO-ERROR.
    IF AVAILABLE tarjetas THEN
        W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Tarjeta Visionamos     : " + STRING(tarjetas.tarjetaDB).
END.

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

ASSIGN W_Texto = "Fecha de Desembolso    : " + STRING(TCreditos.Fec_Desembolso,"99/99/9999")
       W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Fecha Inicio Crédito   : " + STRING(TCreditos.Fec_paganti,"99/99/9999").

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

CASE TCreditos.Per_Pago:
    WHEN 1 THEN
        ASSIGN gTexto = "Semanal"
               periodos = "Semanas".

    WHEN 2 THEN
        ASSIGN gTexto = "Decadal"
               periodos = "Décadas".

    WHEN 3 THEN
        ASSIGN gTexto = "Quincenal"
               periodos = "Quincenas".

    WHEN 4 THEN
        ASSIGN gTexto = "Mensual"
               periodos = "Meses".

    WHEN 8 THEN
        ASSIGN gTexto = "Semestral"
               periodos = "Semestres".

    WHEN 9 THEN
        ASSIGN gTexto = "Anual"
               periodos = "Años".
END CASE.

ASSIGN W_Texto = "Cuota                  : " + TRIM(STRING(TCreditos.Cuota,"$>>>,>>>,>>>,>>9"))
       W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Periodo Pago Cuota     : " + gTexto.

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

ASSIGN W_Texto = "Tasa                   : " + TRIM(STRING(TCreditos.Tasa,">>9.99%"))
       W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Plazo                  : " + TRIM(STRING(TCreditos.Plazo,">>>z9")) + " " + periodos.

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

IF TCreditos.cod_credito <> 123 THEN
    W_Texto = "Monto desembolsado     : " + TRIM(STRING(TCreditos.Val_Desembolso,"$>>>,>>>,>>>,>>9")).
ELSE
    W_Texto = "Cupo aprobado          : " + TRIM(STRING(TCreditos.monto,"$>>>,>>>,>>>,>>9")).

IF TCreditos.Fec_UltPago EQ ? THEN
    W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Último Pago            : No registra pago".
ELSE
    W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Último Pago            : " + STRING(TCreditos.Fec_UltPago,"99/99/9999").

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

ASSIGN W_Texto = "Saldo Capital          : " + TRIM(STRING(TCreditos.Sdo_Capital,"$>>>,>>>,>>>,>>9")).

IF Tcreditos.cod_credito = 123 THEN
    W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Cupo disponible        : " + TRIM(STRING(TCreditos.monto - TCreditos.sdo_capital,"$>>>,>>>,>>>,>>9")).
ELSE
    W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Interés Anticipado     : " + TRIM(STRING(TCreditos.Int_Anticipado,"$>>>,>>>,>>>,>>9")).

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

ASSIGN W_Texto = "Interés Corriente      : " + TRIM(STRING(TCreditos.Int_Corrientes,"$>>>,>>>,>>>,>>9"))
       W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Interés Difícil Cobro  : " + TRIM(STRING(TCreditos.INT_DifCobro,"$>>>,>>>,>>>,>>9")).
    
RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

IF TCreditos.Fec_Pago EQ ? THEN
    W_Texto = "Fecha de próximo pago  : " + "No Asignada".
ELSE
    W_Texto = "Fecha de próximo pago  : " + STRING(TCreditos.Fec_Pago,"99/99/9999").

IF TCreditos.detalle_estado = 2 THEN
    W_Texto = "Fecha de próximo pago  : CRÉDITO CONGELADO".

W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Días de atraso         : " + TRIM(STRING(TCreditos.Dias_Atraso,">>>9")).

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

W_TCapVdo = TCreditos.Val_Atraso.

IF W_TCapVdo GT TCreditos.Sdo_Capital THEN
    W_TCapVdo = TCreditos.Sdo_Capital.

IF W_tcapvdo LT 0 THEN
    W_tcapvdo = 0.

ASSIGN W_Texto = "Valor Capital atraso   : " + TRIM(STRING(W_TCapVdo,"$>>>,>>>,>>9")).

W_TSdoVdo = TCreditos.Val_Atraso +
            TCreditos.Honorarios +
            TCreditos.Costas +
            TCreditos.Polizas +
            TCreditos.Int_MorCobrar +
            TCreditos.Int_MoraDifCob +
            TCreditos.Int_Corrientes +
            TCreditos.INT_DifCobro -
            TCreditos.Int_Anticipado.

IF W_TSdoVdo GT W_SdoDeuda THEN
    W_TSdoVdo = W_SdoDeuda.

W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Atraso+Interés+Mora    : " + TRIM(STRING(W_TSdoVdo,"$>>>,>>>,>>>,>>9")).

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

ASSIGN W_Texto = "Interés de mora        : " + TRIM(STRING(TCreditos.Int_MorCobrar,"$>>,>>>,>>>,>>9"))
       W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Interés Mora Dif.Cobro : " + TRIM(STRING(TCreditos.Int_MoraDifCob,"$>>>,>>>,>>>,>>9")).

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

W_CuoAtraso = TCreditos.cuo_atraso.

W_Texto = "Cuotas por pagar       : " + TRIM(STRING(tCreditos.plazo - TCreditos.Cuo_Pagadas,"->>>9")).
W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Cuotas en Mora         : " + TRIM(STRING(W_CuoAtraso,"->>9")).

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

DEFINE VAR WF_Pago AS CHARACTER FORMAT "X(12)".

CASE TCreditos.FOR_Pago:
    WHEN 1 THEN WF_Pago = "Caja".
    WHEN 2 THEN WF_Pago = "Nómina".
    WHEN 3 THEN WF_Pago = "Débito Automático".
    WHEN 4 THEN WF_Pago = "Nómina Crece".
    WHEN 5 THEN WF_Pago = "Prima".
END CASE.

W_Texto = "Forma de Pago          : " + WF_Pago.
W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Saldo Cobro Jurídico   : " + TRIM(STRING(TCreditos.Honorarios + TCreditos.Costas + TCreditos.Polizas,"$>>>,>>>,>>>,>>9")).

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

ASSIGN W_SdoDeuda = TCreditos.Honorarios + TCreditos.Costas + TCreditos.Polizas + TCreditos.Int_MorCobrar + TCreditos.Int_MoraDifCob +
                    TCreditos.Int_Corrientes + TCreditos.Sdo_Capital + TCreditos.INT_DifCobro - TCreditos.Int_Anticipado.

W_Texto = "Saldo Total Deuda      : " + TRIM(STRING(W_SdoDeuda,"$>>>,>>>,>>>,>>9")).
W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Saldo Proyectado       : " + TRIM(STRING(TCreditos.Sdo_Proyect,"$>>>,>>>,>>>,>>9")).

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

ASSIGN W_Texto = "Calificación Credito   : " + STRING(TCreditos.categoria,"X(12)")
       W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Categoría de Arrastre  : " + STRING(TCreditos.categoriames,"X(12)").

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

ASSIGN W_Texto = "Provisión de capital   : " + TRIM(STRING(TCreditos.Provision,"$>>>,>>>,>>>,>>9"))
       W_Texto = W_Texto + FILL(" ", 59 - LENGTH(w_texto)) + "Provisión de Interés   : " + TRIM(STRING(TCreditos.Provision_Interes,"$>>>,>>>,>>>,>>9")).

RUN Grabar_Info(INPUT "CRE",
                INPUT W_Texto).

IF TCreditos.Abogado THEN
    W_Texto = "CRÉDITO EN ABOGADO     :" + STRING(TCreditos.Nit_Juzgado,"X(12)").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Detalle_Solicitud wWin 
PROCEDURE Detalle_Solicitud :
DEFINE VAR W_NomIns LIKE Instancias.Nom_Instancia.
DEFINE VAR W_Vigenc AS INTEGER FORMAT "999".
DEFI VAR Periodo  AS INTEGER FORMAT "99".
DEFI VAR Tas_Nominal LIKE Solicitud.Tasa.
DEFI VAR Nom_Estado  AS CHAR FORM "X(12)" INIT "Estudio".

CASE TSolicitud.Per_Pago:
    WHEN 1 THEN Periodo = 52.
    WHEN 2 THEN Periodo = 36.
    WHEN 3 THEN Periodo = 24.
    WHEN 4 THEN Periodo = 12.
    WHEN 8 THEN periodo = 2.
    WHEN 9 THEN periodo = 1.
END CASE.

IF TSolicitud.Estado EQ 2 THEN
    Nom_Estado = "Aprobada".
ELSE IF TSolicitud.Estado EQ 3 THEN
    Nom_Estado = "Negada".
ELSE
    IF TSolicitud.Estado EQ 4 THEN
        Nom_Estado = "Condicionada".

RUN EFNV IN W_ManFin (INPUT TSolicitud.Tasa / 100,
                      INPUT Periodo,
                      OUTPUT Tas_Nominal).

ASSIGN Tas_Nominal = ((Tas_Nominal * Periodo) * 100).

FIND FIRST Agencias WHERE Agencias.Agencia EQ TSolicitud.Agencia NO-LOCK NO-ERROR.
IF AVAILABLE Agencias THEN
    gTexto = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
ELSE
    gTexto = "Agencia Inconsistente".

FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ TSolicitud.Cod_Credito NO-LOCK NO-ERROR.

RUN Raya(INPUT "SOL").

ASSIGN W_Texto = "Producto            : " + STRING(Pro_Creditos.Cod_Credito,"999") + " - " + STRING(Pro_Creditos.Nom_Producto,"X(25)")
       W_Texto = W_Texto + " Agencia Matricula   : " + STRING(gTexto,"X(30)").
RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).

W_Texto = "Número de Solicitud : " + STRING(TSolicitud.Num_Solicitud,"999999999") + "                    Estado de la Solicitud : " + Nom_Estado.
RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).

ASSIGN W_Texto = "Monto de Apertura   : " + STRING(TSolicitud.Monto,">>>,>>>,>>>,>>9")
       W_Texto = W_Texto + "                 Fecha de Solicitud  : " + STRING(TSolicitud.Fec_Solicitud).
RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).

CASE TSolicitud.Per_Pago:
    WHEN 1 THEN gTexto = "Semanal".
    WHEN 2 THEN gTexto = "Decadal".
    WHEN 3 THEN gTexto = "Quincenal".
    WHEN 4 THEN gTexto = "Mensual".
    WHEN 8 THEN gTexto = "Semestral".
    WHEN 9 THEN gTexto = "Anual".
END CASE.

ASSIGN W_Texto = "Cuota               : " + STRING(TSolicitud.Cuota,">>>,>>>,>>>,>>9")
       W_Texto = W_Texto + "                 Periodo Pago        : " + gTexto.
RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).

ASSIGN W_Texto = "Tasa Nominal        : " + STRING(Tas_Nominal,">>9.99%")
            W_Texto = W_Texto + "                         Plazo               : " + STRING(TSolicitud.Plazo,">>>>9").
     RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).

     ASSIGN W_Texto = "Fec-Aprobac.: " + STRING(TSolicitud.Fec_Aprobac)
            W_Texto = W_Texto + "      N.Credito/Pagare : " + STRING(TSolicitud.Pagare,"X(14)").
     RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).

     W_Texto = "".
     RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).

     W_Texto = "Cod.  Nombre Instancia                                Fec.Ingreso   Fec.Retiro   Usuario".
     RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).
     
     FOR EACH Mov_Instancias WHERE 
         Mov_Instancias.Num_Solicitud EQ TSolicitud.Num_Solicitud  AND
         Mov_Instancias.Nit           EQ TSolicitud.Nit         
                                      NO-LOCK BY Mov_Instancias.Fec_ingreso :
         FIND Instancias WHERE
                 Instancias.Instancia      EQ Mov_Instancias.Instancia AND 
                 Instancias.Tipo_Producto  EQ 2  NO-LOCK NO-ERROR.
        IF AVAILABLE Instancias THEN W_NomIns = Instancias.Nom_Instancia.
        IF Mov_Instancias.Fec_Retiro EQ ? THEN
           gtexto = "Vigente".
        ELSE gtexto = STRING(Mov_Instancias.Fec_Retiro,"99/99/9999").
        FIND Usuarios WHERE Usuarios.Usuario EQ Mov_Instancias.Usuario NO-LOCK NO-ERROR.
        W_Texto = STRING(Mov_Instancias.Instancia,"999") + " - " + STRING(W_NomIns,"X(50)") +
                  STRING(Mov_Instancias.Fec_Ingreso) + "    " + STRING(gtexto,"X(10)") +
                  "     "  + Mov_instancias.Usuario + " - " + Usuarios.Nombre.
        IF W_Texto NE "" THEN RUN Grabar_Info (INPUT "SOL", INPUT W_Texto).        
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
  DISPLAY Pantalla W_Nombre Buscar Cmb_Tipo 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE BInfo Btn_Basica Btn_CJ BUTTON-151 Btn_Cert Btn_HojaVida Btn_Firma 
         Btn_Foto BLista Pantalla Buscar Btn_Imprimir Btn_Tipos Btn_Salir 
         Cmb_Tipo Btn_PQR 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  DISPLAY W_UbicacionResidencia W_UbicacionComercial 
      WITH FRAME F_Basica IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Dir_Residencia Clientes.Tel_Residencia Clientes.Dir_comercial 
          Clientes.Tel_comercial Clientes.Salario Clientes.Email 
          Clientes.Celular Clientes.Fec_Nacimiento Clientes.fecPagare 
      WITH FRAME F_Basica IN WINDOW wWin.
  ENABLE RECT-282 RECT-283 RECT-302 Clientes.Dir_Residencia 
         Clientes.Tel_Residencia Btn_Residencia BUTTON-216 BUTTON-219 
         Clientes.Dir_comercial Clientes.Tel_comercial Clientes.Salario 
         Clientes.Email Clientes.Celular Clientes.Fec_Nacimiento 
         Clientes.fecPagare Btn_ActBasica Btn_AdmCode BUTTON-208 
      WITH FRAME F_Basica IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Basica}
  ENABLE btnVolverFrmFacturaCupo 
      WITH FRAME frmFacturaCupoRotativo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-frmFacturaCupoRotativo}
  IF AVAILABLE Plastico THEN 
    DISPLAY Plastico.Cue_Ahorros Plastico.Num_Plastico 
      WITH FRAME FPlastico IN WINDOW wWin.
  ENABLE BUTTON-228 Plastico.Num_Plastico BUTTON-227 btn_salplas 
      WITH FRAME FPlastico IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FPlastico}
  DISPLAY Id_General Id_Saldos Id_Economica Id_Atrasos Id_Ahorros Id_Relaciones 
          Id_AhoCanc Id_Codeudando Id_Creditos Id_HojaVida Id_HistCreditos 
          Id_Controles Id_SimulaPago TCompromisos Id_CredCanc Id_GestCobro 
          Id_Solicitudes Tg_Extras Id_Especiales Tg_CtasAho Id_GarAdm 
          Tg_Plastico IdFacturaCupoRotativo tgFecCorte FIni fechaCorte FFin 
      WITH FRAME F_Opciones IN WINDOW wWin.
  ENABLE BUTTON-5 RECT-280 RECT-317 Id_General Id_Saldos Id_Economica 
         Id_Ahorros Id_Relaciones Id_AhoCanc Id_Codeudando Id_Creditos 
         Id_HojaVida Id_HistCreditos Id_Controles Id_SimulaPago TCompromisos 
         Id_CredCanc Id_GestCobro Id_Solicitudes Tg_CtasAho Id_GarAdm 
         Tg_Plastico IdFacturaCupoRotativo tgFecCorte 
      WITH FRAME F_Opciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Opciones}
  ENABLE Foto BUTTON-124 
      WITH FRAME F_Foto IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Foto}
  ENABLE BROWSE-1 BUTTON-181 
      WITH FRAME F_ConsCred IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConsCred}
  DISPLAY WC_UbicacionResidencia WC_UbicacionComercial 
      WITH FRAME F_AdmCodeudores IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Dir_comercial Clientes.Dir_Residencia Clientes.Tel_comercial 
          Clientes.Tel_Residencia Clientes.Salario Clientes.Email 
          Clientes.Celular Clientes.Fec_Nacimiento 
      WITH FRAME F_AdmCodeudores IN WINDOW wWin.
  ENABLE RECT-284 RECT-285 RECT-304 BC_Creditos BC_Codeudores Btn_Residencia-2 
         BUTTON-225 Clientes.Dir_comercial Clientes.Dir_Residencia 
         Clientes.Tel_comercial Clientes.Tel_Residencia Clientes.Salario 
         Clientes.Email Btn_ActBasica-2 Btn_CrearCodeudor Clientes.Celular 
         Btn_BorCode BUTTON-226 Clientes.Fec_Nacimiento 
      WITH FRAME F_AdmCodeudores IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_AdmCodeudores}
  DISPLAY ECompromisos 
      WITH FRAME F_InfCompromisos IN WINDOW wWin.
  ENABLE ECompromisos BUTTON-214 BUTTON-213 
      WITH FRAME F_InfCompromisos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_InfCompromisos}
  DISPLAY Cmb_estcom REstado Cmb_Cpto CAgencia Ecompromiso 
      WITH FRAME F_Compromisos IN WINDOW wWin.
  IF AVAILABLE Cobros THEN 
    DISPLAY Cobros.Estado Cobros.Num_Credito Cobros.Val_Compromiso 
          Cobros.Fec_Compromiso 
      WITH FRAME F_Compromisos IN WINDOW wWin.
  ENABLE RECT-303 Cmb_estcom BRCreditos REstado BRCompromisos Cobros.Estado 
         Cmb_Cpto CAgencia Ecompromiso BUTTON-184 Btn_AgrCompromiso BUTTON-212 
      WITH FRAME F_Compromisos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Compromisos}
  ENABLE Firma BUTTON-125 
      WITH FRAME F_Firma IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Firma}
  DISPLAY Nit_Abogado Nom_Abogado 
      WITH FRAME F_CobroJuridico IN WINDOW wWin.
  ENABLE BCobro Nit_Abogado BUTTON-211 BUTTON-229 Btn_OutCJ 
      WITH FRAME F_CobroJuridico IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_CobroJuridico}
  DISPLAY TNombre W_CodCpto TApellido1 TApellido2 
      WITH FRAME F_Browser IN WINDOW wWin.
  ENABLE Br_Clientes TNombre BBuscar W_CodCpto TApellido1 BUTTON-121 TApellido2 
      WITH FRAME F_Browser IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Browser}
  DISPLAY Cmb_Tipos NomTipo Cmb_CodHV Nom_Codigo HV_Instancia HV_Doc 
          HV_NomUsuario HV_Hora 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  IF AVAILABLE Hoja_Vida THEN 
    DISPLAY Hoja_Vida.Tipo Hoja_Vida.Codigo Hoja_Vida.Usuario Hoja_Vida.DoctoRefer 
          Hoja_Vida.Fec_Grabacion Hoja_Vida.Asunto_Cumplido 
          Hoja_Vida.Observacion 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  ENABLE Cmb_Tipos Cmb_CodHV BUTTON-142 BUTTON-145 Hoja_Vida.Observacion 
         BUTTON-127 Btn_Primero BUTTON-129 BUTTON-128 BUTTON-144 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_HojaVida}
  DISPLAY HVNomInstancia HVEstado HVUsu HVFecIng HVNSol HVFecRet HVCuenta HVDes 
      WITH FRAME F_MovInstancias IN WINDOW wWin.
  ENABLE HVDes BUTTON-155 BUTTON-152 BUTTON-153 Btn_Imp BUTTON-154 
      WITH FRAME F_MovInstancias IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_MovInstancias}
  DISPLAY WFirma WCargo WCC Cmb_Doc 
      WITH FRAME F_Cer IN WINDOW wWin.
  ENABLE WFirma WCargo WCC Cmb_Doc BUTTON-148 BUTTON-149 
      WITH FRAME F_Cer IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cer}
  DISPLAY R_Encabezado 
      WITH FRAME F_Impresion IN WINDOW wWin.
  ENABLE BUTTON-7 R_Encabezado BUTTON-6 
      WITH FRAME F_Impresion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Impresion}

  {&OPEN-QUERY-F_Controles}
  GET FIRST F_Controles.
  DISPLAY TReportado_Procredito TReportado_fiscalia TReportado_Super 
      WITH FRAME F_Controles IN WINDOW wWin.
  ENABLE RECT-281 TReportado_Procredito TReportado_fiscalia TReportado_Super 
         btn_cambios BUTTON-183 
      WITH FRAME F_Controles IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Controles}
  ENABLE BR_HojaVida BUTTON-146 
      WITH FRAME F_BuscarM IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_BuscarM}
  ENABLE B_Relaciones BUTTON-217 BUTTON-220 BUTTON-218 
      WITH FRAME F_Relaciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Relaciones}
  DISPLAY Cmb_TipIns 
      WITH FRAME F_ConMovInstancias IN WINDOW wWin.
  ENABLE Cmb_TipIns BCMI Btn_OutConMovIns 
      WITH FRAME F_ConMovInstancias IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConMovInstancias}
  DISPLAY R_Nombre R_Nit R_Tel_Comercial 
      WITH FRAME FRelNva IN WINDOW wWin.
  ENABLE R_Nombre R_Nit R_Tel_Comercial BUTTON-205 BUTTON-206 Btn_SC 
      WITH FRAME FRelNva IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRelNva}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EstCta_Ahorro wWin 
PROCEDURE EstCta_Ahorro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Raya(INPUT "ECA").

  ASSIGN W_Texto = "                                  ESTADO-CUENTAS DE AHORRO".         
  RUN Grabar_Info (INPUT "ECA", INPUT W_Texto).

  ASSIGN W_Texto = " Ag. Producto            No.de Cuenta   F-Apertura Sdo.Disponible + Canje  Interés X Pagar  Interés-Causado".    
  RUN Grabar_Info (INPUT "ECA", INPUT W_Texto).
  FOR EACH Ahorros WHERE Ahorros.Nit EQ TClientes.Nit NO-LOCK
                      BY Ahorros.Cod_Ahorro BY Ahorros.Cue_Ahorro:
      IF Ahorros.Estado EQ 1 THEN DO:
         FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_ahorro NO-LOCK NO-ERROR.
         IF AVAILABLE Pro_Ahorros THEN DO:
            IF Pro_Ahorros.Tip_Ahorro EQ 2 AND SUBSTRING(Ahorros.Cue_Ahorros,4,1) LE " " THEN DO:
               IF LENGTH(Ahorros.Cue_Ahorros) EQ 3 THEN
                  W_Texto = " " + STRING(Ahorros.Agencia,"999") + "  " +
                     STRING(Pro_Ahorros.Nom_Producto,"X(22)") + " 0" +                          
                     STRING(Ahorros.Cue_Ahorro,"X(11)") + " " +                                                          
                     STRING(Ahorros.Fec_Apertura) + "  " +                                                               
                     STRING(Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje,"->>>>,>>>,>>9.99") + " " +                            
                     STRING(Ahorros.INT_Pagar,"->>>>,>>>,>>9.99") + " " +                                                 
                     STRING(Ahorros.INT_Causado,"->>>>,>>>,>>9.99"). 
               ELSE IF LENGTH(Ahorros.Cue_Ahorros) EQ 2 THEN
                  W_Texto = " " + STRING(Ahorros.Agencia,"999") + "  " +
                     STRING(Pro_Ahorros.Nom_Producto,"X(22)") + " 00" +                          
                     STRING(Ahorros.Cue_Ahorro,"X(10)") + " " +                                                          
                     STRING(Ahorros.Fec_Apertura) + "  " +                                                               
                     STRING(Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje,"->>>>,>>>,>>9.99") + " " +                            
                     STRING(Ahorros.INT_Pagar,"->>>>,>>>,>>9.99") + " " +                                                 
                     STRING(Ahorros.INT_Causado,"->>>>,>>>,>>9.99"). 
               ELSE IF LENGTH(Ahorros.Cue_Ahorros) EQ 1 THEN
                  W_Texto = " " + STRING(Ahorros.Agencia,"999") + "  " +
                     STRING(Pro_Ahorros.Nom_Producto,"X(22)") + " 000" +                          
                     STRING(Ahorros.Cue_Ahorro,"X(9)") + " " +                                                          
                     STRING(Ahorros.Fec_Apertura) + "  " +                                                               
                     STRING(Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje,"->>>>,>>>,>>9.99") + " " +                            
                     STRING(Ahorros.INT_Pagar,"->>>>,>>>,>>9.99") + " " +                                                 
                     STRING(Ahorros.INT_Causado,"->>>>,>>>,>>9.99").
            END.
            ELSE
               W_Texto = " " + STRING(Ahorros.Agencia,"999") + "  " +                         
                     STRING(Pro_Ahorros.Nom_Producto,"X(22)") + " " +                      
                     STRING(Ahorros.Cue_Ahorro,"X(12)") + " " +                                
                     STRING(Ahorros.Fec_Apertura) + "  " +                                    
                     STRING(Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje,"->>>>,>>>,>>9.99") + " " +
                     STRING(Ahorros.INT_Pagar,"->>>>,>>>,>>9.99") + " " +                     
                     STRING(Ahorros.INT_Causado,"->>>>,>>>,>>9.99").                          


            RUN Grabar_Info (INPUT "ECA", INPUT W_Texto).
         END.
      END.
  END.

  RUN Raya(INPUT "ECA").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Extras_Creditos wWin 
PROCEDURE Extras_Creditos :
DEFI VAR NomEst AS CHAR FORM "X(10)".

RUN Raya(INPUT "EXT").

ASSIGN W_Texto = "                                  EXTRAS DE CREDITOS".
RUN Grabar_Info (INPUT "EXT", INPUT W_Texto).

ASSIGN W_Texto = " Pdcto  No.Pagaré   Nro.Pdo  Fec-Vcto.Extra    Estado       Valor Extra".
RUN Grabar_Info (INPUT "EXT", INPUT W_Texto).

FOR EACH Creditos WHERE Creditos.Nit EQ W_CedCteC
                    AND Creditos.Estado EQ 2 NO-LOCK:
    FOR EACH Extras WHERE Extras.Nit EQ Creditos.Nit
                      AND Extras.Cod_Credito EQ Creditos.Cod_Credito
                      AND Extras.Num_Solicitud EQ Creditos.Num_solicitud NO-LOCK BY Extras.Nro_Cuota:
        IF extras.Estado EQ  2 THEN
            NomEst = "Pagada".
        ELSE
            NomEst = "Pendiente".

        ASSIGN W_Texto = STRING(Creditos.Cod_Credito,"   999") + "  " +
                         STRING(Creditos.Pagare,"9999999999")  + "  " +
                         STRING(Extras.Nro_Cuota,"9999")    + "       " +
                         STRING(Extras.Fec_Vcto,"99/99/9999") + "      " +
                         STRING(NomEst,"X(10)") +
                         STRING(Extras.Vr_CuoExtra,"->>>>,>>>,>>9.99").

        RUN Grabar_Info (INPUT "EXT", INPUT W_Texto).
    END.
END.

RUN Raya(INPUT "EXT").
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE facturaCupoRotativo wWin 
PROCEDURE facturaCupoRotativo :
DEFINE VAR hPdf AS COM-HANDLE NO-UNDO.
DEFINE VAR l-file AS CHARACTER.

VIEW FRAME frmFacturaCupoRotativo.
HIDE FRAME F_Opciones.

l-file = "Reportes\FacturasCupos\" + clientes.nit + ".pdf".
hPdf = chCtrlFrame:pdf.
hPdf:LoadFile(l-file).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gestion_Cobros wWin 
PROCEDURE Gestion_Cobros :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:   Dic.19/05 GAER    
------------------------------------------------------------------------------*/
  DEFI VAR W_Fec AS CHAR FORM "X(8)".
  DEFINE VARIABLE icontador AS INTEGER NO-UNDO.
  ASSIGN FRAME F_Opciones Fini FFin.

  RUN Raya (INPUT "GCO").
  W_Texto = "                                       GESTION DE COBRANZA".
  RUN Grabar_Info (INPUT "GCO", INPUT W_Texto).

  ASSIGN W_Texto = "F-Acuerd F-Compro E Vr.Compromiso Vr.Cumplido NCredito Usu.     Observacion".
  RUN Grabar_Info (INPUT "GCO", INPUT W_Texto).

  ASSIGN W_Texto = "-------- -------- - ------------- ----------- -------- ---- ------------------------------------------------------------".

  RUN Grabar_Info (INPUT "GCO", INPUT W_Texto).
 
  FOR EACH Cobros WHERE Cobros.Nit EQ TClientes.Nit NO-LOCK
                    BY Cobros.Fec_Acuerdo: 
      IF Cobros.Fec_Acuerdo GE FIni AND 
         Cobros.Fec_Acuerdo LE FFin THEN DO:

         ASSIGN W_Fec = ""
                W_Fec = STRING(Cobros.Fec_Compromi,"99/99/99") WHEN Cobros.Fec_Compromi NE ?.

         W_Texto = STRING(Cobros.Fec_Acuerdo,"99/99/99") + " " + STRING(W_Fec,"X(8)") +
                   " " + STRING(Cobros.Estado,"9") + " " + STRING(Cobros.Val_Compromi,">>>>,>>>,>>9")   +
                   " " + STRING(Cobros.Val_Cumplido,">>>>,>>>,>>9") +  " " + 
                   STRING(Cobros.Num_Credito,"99999999") + " " + STRING(Cobros.Usuario,"9999") + " " +
                   SUBSTRING(Cobros.Observac,1,60).
              
         RUN Grabar_Info (INPUT "GCO", INPUT W_Texto).
         IF LENGTH(Cobros.Observac) > 60 THEN DO: /* Si la Observacion es lleva mas de una linea */
             DO icontador = 1 TO 10:
                IF LENGTH(SUBSTRING(Cobros.Observac,60 * iContador,60)) > 0 THEN DO:
                    W_Texto = FILL(" ",60) + SUBSTRING(Cobros.Observac,60 * iContador + 1,60).
                    RUN Grabar_Info (INPUT "GCO", INPUT W_Texto).
                END.
             END.
         END.
      END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Info wWin 
PROCEDURE Grabar_Info :
DEFINE INPUT PARAMETER WTipo AS CHARACTER FORMAT "X(3)".
DEFINE INPUT PARAMETER WText AS CHARACTER FORMAT "X(132)".

i = i + 1.

CREATE TInfo.
ASSIGN TInfo.Tip = WTipo
       TInfo.Reg = i
       TInfo.Tex = WText
       TInfo.cedula = clientes.nit
       TInfo.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_MovInst wWin 
PROCEDURE Halla_MovInst :
/*------------------------------------------------------------------------------
  Purpose:   Feb.23/05 Gaer.
  Invocado desde Procedures BSolicitud y BCreditos.
 ------------------------------------------------------------------------------*/
  DEFI INPUT PARAMETER W_Nro_SolCre LIKE Solicitud.Num_Solicitud.
 
  ASSIGN FRAME F_Opciones Fini FFin.

  FOR EACH Mov_Instancias WHERE Mov_Instancias.Nit         EQ TClientes.Nit 
                            AND Mov_Instancias.Fec_Ingreso GE FIni          
                            AND Mov_Instancias.Fec_Ingreso LE FFin  NO-LOCK:        
     CREATE THojaVida.
     ASSIGN THojaVida.AsuH  = STRING(Mov_Instancias.Instancia)
            THojaVida.UsuH  = Mov_Instancias.Usuario
            THojaVida.FecH  = Mov_Instancias.Fec_Ingreso
            THojaVida.Cump  = Mov_Instancias.Estado
            THojaVida.FecR  = Mov_Instancias.Fec_Retiro
            THojaVida.DesH  = Mov_Instancias.Descripcion
            THojaVida.DocH  = Mov_Instancias.Num_Solicitud.
  END.

 RUN Raya (INPUT "MIN").
 W_Texto = "                               Movimiento-Instancias".
 RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).

 RUN Raya (INPUT "HVA").

 FOR EACH THojaVida WHERE BY THojaVida.FecH BY THojaVida.AsuH:
     W_Texto = STRING(THojaVida.FecH,"99/99/9999") + " " +
               STRING(THojaVida.AsuH,"X(5)") + " " +
               STRING(THojaVida.FecR,"99/99/9999") + " " +
               STRING(THojaVida.Cump,"Si/No") + " " +
               STRING(THojaVida.UsuH,"X(4)") + " " +
               STRING(THojaVida.DocH,"9999999999999") + " " +
               STRING(THojaVida.DesH,"X(65)").
     RUN Grabar_Info (INPUT "HVA", INPUT W_Texto).
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE halla_proyectado wWin 
PROCEDURE halla_proyectado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bcred FOR creditos.
DEFINE VARIABLE xfec AS DATE NO-UNDO.
DEFINE VARIABLE xpro AS DECIMAL NO-UNDO.
DEFINE VARIABLE xatr AS DECIMAL NO-UNDO.
DEFINE VARIABLE xubica AS DATE NO-UNDO.
DEFINE VARIABLE xfpro AS DATE NO-UNDO.
DEFINE VARIABLE xdiasatr AS DECIMAL NO-UNDO.
DEFINE VARIABLE W_Frec AS INTEGER NO-UNDO.
DEFINE VARIABLE W_NroPer AS INTEGER     NO-UNDO.
DEFINE VARIABLE W_N AS DECIMAL     NO-UNDO.
 

xfec = Creditos.Fec_Desembolso.
IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desembolso THEN
xfec = Creditos.Fec_PagAnti.


/*
IF xfec < DATE(07,1,2006) AND creditos.FOR_pago = 1 THEN xfec = xfec - 45.
*/

RUN HSP IN W_manfin (INPUT xfec,
                     INPUT creditos.monto,
                     INPUT creditos.cuota,
                     INPUT creditos.monto,
                     INPUT creditos.tasa,
                     INPUT creditos.plazo,
                     INPUT creditos.sdo_capital,
                         OUTPUT xpro,
                         OUTPUT xatr,
                         OUTPUT xfpro,
                         OUTPUT xUbica,
                         OUTPUT xDiasatr).

 FOR EACH bcred OF creditos EXCLUSIVE-LOCK:
    ASSIGN bcred.sdo_proyectado = xpro
           bcred.val_atraso     = xatr 
           bcred.fec_pago       = xubica 
           bcred.dias_atraso    = xdiasatr.
 END.


/* Halla la frecuencia */
 ASSIGN W_Frec = 15
        W_NroPer = 24.
 
 CASE creditos.Per_Pago:
    WHEN 1 THEN DO:
        ASSIGN W_Frec = 7
               W_NroPer = 52.
        END.
    WHEN 2 THEN DO: 
        ASSIGN W_Frec = 10
               W_NroPer = 36.
    END.
    WHEN 3 THEN DO: 
        ASSIGN W_Frec = 15
               W_NroPer = 24.
    END.
    WHEN 4 THEN DO: 
        ASSIGN W_Frec = 30
               W_NroPer = 12.
    END.
    WHEN 5 THEN DO: 
        ASSIGN W_Frec =     60
               W_NroPer =   6.
    END.
    WHEN 6 THEN DO: 
        ASSIGN W_Frec = 90
               W_NroPer = 4.
    END.
    WHEN 7 THEN DO: 
        ASSIGN W_Frec = 120
               W_NroPer = 3.
    END.
    WHEN 8 THEN DO: 
        ASSIGN W_Frec = 180
               W_NroPer = 2.
    END.
    WHEN 9 THEN DO: 
        ASSIGN W_Frec = 360
               W_NroPer = 1.
    END.
END CASE.



ASSIGN W_N = decimal(creditos.tasa / W_NroPer) / 100.


/* HALLA CAPITAL MORA */
RUN HSM IN W_manfin (INPUT xfec,
                     INPUT creditos.cuota,
                     INPUT W_N,
                     INPUT creditos.plazo,
                     INPUT W_Frec,
                     OUTPUT xpro).


/*
DEFINE VAriable W_fec AS DATE NO-UNDO.                            /* Fecha 1er Pago */
DEFINE VARIABLE wcuo             LIKE creditos.cuota NO-UNDO.     /* Cuota */
DEFINE VARIABLE Wpla             LIKE creditos.plazo NO-UNDO.     /* Plazo */
DEFINE VARIABLE WFrec            AS INTEGER NO-UNDO.              /* Frecuencia de pago creditos.Per_Pago*/
DEFINE variable P_Proyectado    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE WPeriodoTrans           AS INTEGER NO-UNDO.

 ASSIGN W_fec = xfec
      wcuo  = creditos.cuota       
      wtas  = W_N 
      Wpla  = creditos.plazo       
      WFrec = W_Frec.
  ASSIGN WPeriodoTrans = TRUNCATE((TODAY - w_fec) / wfrec, 0).

  
  ASSIGN P_Proyectado =  wcuo  * (( exp((1 + wtas),(wpla - WPeriodoTrans)) - 1) / (wtas * exp ((1 + wtas),(wpla - WPeriodoTrans)))).
  ASSIGN  xpro = P_Proyectado.
*/


/***************************************************************************************/

 FOR EACH bcred OF creditos EXCLUSIVE-LOCK:
    ASSIGN bcred.sdo_proyectado = xpro
           bcred.val_atraso     = bcred.monto - xpro.

    IF bcred.val_atraso < 0 THEN
        bcred.val_atraso = 0.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hoja_de_Vida wWin 
PROCEDURE Hoja_de_Vida :
RUN Inicializa_HojaVida.
APPLY "choose" TO Btn_Primero IN FRAME F_HojaVida.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_Formato_TDebito wWin 
PROCEDURE Imprime_Formato_TDebito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN ValCol = "A4"  Dato = Agencias.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "L4"  Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "Z4"  Dato = Clientes.Nit.
    RUN Llenar_Celda.
    ASSIGN ValCol = "A7"  Dato = Clientes.DIR_Residencia.
    RUN Llenar_Celda.
    ASSIGN ValCol = "K7"  Dato = Clientes.Lugar_Residencia.
    RUN Llenar_Celda.
    ASSIGN ValCol = "Q7"  Dato = Ahorros.Cue_Ahorros.
    RUN Llenar_Celda.
    ASSIGN ValCol = "V7"  Dato = STRING(Plastico.Num_Plastico).
    RUN Llenar_Celda.
    ASSIGN ValCol = "AB7" Dato = STRING(Plastico.Num_Plastico,"->>>,>>>,>>>.99"). /* Cambiar por Cupo Autorizado */
    RUN Llenar_Celda.
    ASSIGN ValCol = "AB9" Dato = Usuarios.Nombre.
    RUN Llenar_Celda.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImprimirTodos wWin 
PROCEDURE ImprimirTodos :
ASSIGN FRAME F_Opciones
             Id_General.

EMPTY TEMP-TABLE TInfo.
EMPTY TEMP-TABLE TClientes.

FOR EACH bfrAhorros WHERE bfrAhorros.agencia = w_agencia
                      AND bfrAhorros.tip_ahorro = 4
                      AND bfrAhorros.cod_ahorro = 2
                      AND bfrAhorros.sdo_disponible > 0 NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = bfrAhorros.nit NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        BUFFER-COPY Clientes TO TClientes.

        RUN Buscar_Informacion.

        /*EMPTY TEMP-TABLE Tmp.*/

        RUN Mostrar_Informacion.
    END.
END.

FIND FIRST usuarios WHERE usuarios.usuario = w_usuario NO-LOCK NO-ERROR.

FOR EACH clientes NO-LOCK BY clientes.apellido1:
    FIND FIRST tmp WHERE tmp.cedula = clientes.nit NO-LOCK NO-ERROR.
    IF AVAILABLE tmp THEN DO:
        FOR EACH Tmp WHERE tmp.cedula = clientes.nit BREAK BY tmp.cedula
                                                           BY tmp.reg:
            IF FIRST-OF(tmp.cedula) THEN DO:
                W_Ok = Pantalla:ADD-LAST("FONDO EMPLEADOS DOCENTES UNIVERSIDAD NACIONAL DE COLOMBIA") IN FRAME f_consulta.
                W_Ok = Pantalla:ADD-LAST("USUARIO   : " + STRING(w_usuario) + " - " + usuarios.nombre).
                W_Ok = Pantalla:ADD-LAST("REPORTE   : CONSULTA DE INFORMACION - FECHA: " + STRING(w_fecha,"99/99/9999") + " - " + STRING(TIME,"HH:MM")).
                W_Ok = Pantalla:ADD-LAST("________________________________________________________________________________________________________________________").
                W_Ok = Pantalla:ADD-LAST("Documento: " + STRING(tmp.cedula) + " - " + tmp.nombre).
                W_Ok = Pantalla:ADD-LAST("------------------------------------------------------------------------------------------------------------------------").
            END.

            W_Ok = Pantalla:ADD-LAST(Tmp.Tex) IN FRAME f_consulta.
        END.
    END.
END.

OPEN QUERY BInfo FOR EACH Tmp /*WHERE SUBSTRING(TEx,1,1) NE "-"*/ NO-LOCK INDEXED-REPOSITION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_MovInst wWin 
PROCEDURE Imp_MovInst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ HVUsu:SCREEN-VALUE IN FRAME F_MovInstancias NO-LOCK NO-ERROR.

  DISPLAY SKIP (2)
          "Asociado  : "
          W_Nombre:SCREEN-VALUE IN FRAME F_Consulta  FORM "X(80)" SKIP(1)
          "Detalle de la Instancia : "  SKIP
          HVNomInstancia:SCREEN-VALUE IN FRAME F_MovInstancias FORM "X(40)" SKIP(1)
          "Usuario   : "
          HVUsu:SCREEN-VALUE FORM "X(8)"
          Usuarios.Nombre    FORM "X(35)"
          "     Estado        : "
          HVEstado:SCREEN-VALUE                                SKIP
          "Solicitud : "
          HVNSol:SCREEN-VALUE
          "                                              Fecha Ingreso : "
          HVFecIng:SCREEN-VALUE     FORM "X(10)"               SKIP
          "No.Credito: "
          HVCuenta:SCREEN-VALUE
          "                                              Fecha Retiro  : "
          HVFecRet:SCREEN-VALUE     FORM "X(10)" SKIP(2)
          SUBSTRING(HVDes:SCREEN-VALUE,1,  100)        FORM "X(100)"
          SUBSTRING(HVDes:SCREEN-VALUE,101,200)        FORM "X(100)"
          SUBSTRING(HVDes:SCREEN-VALUE,201,300)        FORM "X(100)"
          SUBSTRING(HVDes:SCREEN-VALUE,301,400)        FORM "X(100)"
          SUBSTRING(HVDes:SCREEN-VALUE,401,500)        FORM "X(100)"
          SUBSTRING(HVDes:SCREEN-VALUE,501,600)        FORM "X(100)"
          SUBSTRING(HVDes:SCREEN-VALUE,601,700)        FORM "X(100)"
          SUBSTRING(HVDes:SCREEN-VALUE,701,800)        FORM "X(100)"
          SUBSTRING(HVDes:SCREEN-VALUE,801,900)        FORM "X(100)"
          SUBSTRING(HVDes:SCREEN-VALUE,901,990)        FORM "X(100)"
       WITH DOWN WIDTH 150 FRAME F_MInst NO-BOX NO-LABELS STREAM-IO USE-TEXT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Compromisos wWin 
PROCEDURE Informe_Compromisos :
Listado = W_PathSpl + "\Clientes.txt".  
OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" INITIAL "AGE NIT            NOMBRE                APELLIDO1        APELLIDO2        ".
DEFINE VAR TotReg AS DECIMAL.
DEFINE VAR Motivo AS CHARACTER FORMAT "X(50)".
DEFINE VAR TotMot AS DECIMAL.
DEFINE VAR WEstado AS CHARACTER FORMAT "X(80)".
DEFI   VAR NEmp    LIKE Empresas.ALIAS_Emp EXTENT 3.
DEFI   VAR GCargo  AS CHARACTER FORMAT "X(30)" EXTENT 3.
DEFI   VAR NCodeu  LIKE Clientes.Nombre EXTENT 2.
DEFI   VAR ROWID_Cte AS ROWID.
DEFI   VAR ROWID_Rel AS ROWID.
DEFI   VAR CedCte    LIKE Clientes.Nit EXTENT 3.

ASSIGN ROWID_Cte = ROWID(Clientes)
       CedCte [1]= Clientes.Nit.

IF CmensajeCJ:HIDDEN IN FRAME F_Consulta EQ NO THEN 
   WEstado = CMensajeCJ:SCREEN-VALUE IN FRAME F_Consulta.

FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
ASSIGN NEmp[1] = Empresas.ALIAS_Emp WHEN AVAIL(Empresas).
RUN Buscar_Varios (INPUT 2, INPUT Clientes.Cod_Cargo, OUTPUT GCargo[1]).

FIND FIRST Relaciones WHERE Relaciones.Nit           EQ CedCte[1]    AND
                            Relaciones.Cod_Relacion  EQ 11           AND 
                            INTEG(Relaciones.Cuenta) EQ Creditos.Num_Credito AND
                            Relaciones.Estado        EQ 1 NO-LOCK NO-ERROR.
IF AVAILABLE Relaciones THEN DO:
   ASSIGN ROWID_Rel = ROWID(Relaciones)
          CedCte[2] = Relaciones.Nit_Relacion.
   FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN DO:
      NCodeu[1] = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
      FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
      ASSIGN NEmp[2] = Empresas.ALIAS_Emp WHEN AVAIL(Empresas).
      RUN Buscar_Varios (INPUT 2, INPUT Clientes.Cod_Cargo, OUTPUT GCargo[2]).
   END.
   FIND FIRST Relaciones WHERE Relaciones.Nit        EQ CedCte[1]       AND
                            Relaciones.Cod_Relacion  EQ 11              AND 
                            INTEG(Relaciones.Cuenta) EQ Creditos.Num_Credito AND
                            Relaciones.Estado        EQ 1                    AND
                            ROWID(Relaciones)        NE ROWID_Rel NO-LOCK NO-ERROR.
   IF AVAILABLE Relaciones THEN DO:
      CedCte[3] = Relaciones.Nit_Relacion.
      FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN DO:
         NCodeu[2] = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2. 
         FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
         ASSIGN NEmp[3] = Empresas.ALIAS_Emp WHEN AVAIL(Empresas).
         RUN Buscar_Varios (INPUT 2, INPUT Clientes.Cod_Cargo, OUTPUT GCargo[3]).
      END.
   END.
END.
      
FIND Clientes WHERE ROWID(Clientes) EQ ROWID_Cte NO-LOCK NO-ERROR.

  DEFINE FRAME F-Enc
      HEADER
        WEstado SKIP(1)
        W_Nom_Entidad AT 2
        "PAGINA:"     AT 90 PAGE-NUMBER FORMAT ">>>9" SKIP
        "Cliente      : " W_Nombre:SCREEN-VALUE IN FRAME F_Consulta FORMAT "X(100)" SKIP
        "Empresa      : " NEmp[1]    SKIP
        "Cargo        : " GCargo[1]  SKIP
        "Credito      : " Creditos.Num_Credito 
        "     Monto : " 
        Creditos.Monto FORMAT ">>>>>,>>>,>>>" 
        "     Plazo : " Creditos.Plazo SKIP (1)
        "Codeudor 1   : " CedCte[2]
        " - " NCodeu[1]  SKIP 
        "Empresa      : " NEmp[2]    SKIP
        "Cargo        : " GCargo[2]  SKIP (1)
        "Codeudor 2   : " CedCte[3]
        " - " NCodeu[2]  SKIP 
        "Empresa      : " NEmp[3]    SKIP
        "Cargo        : " GCargo[3]  SKIP
        "-----------------------------------------------------------------------------------------------------" AT 1
    WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Enc STREAM-IO.

    DEFINE FRAME f-Pie
      HEADER 
        "________________________________________" AT 2
        "Fin del Reporte de Gestion de Cobranza" AT 1
      WITH DOWN WIDTH 132 FRAME f-Pie PAGE-BOTTOM USE-TEXT STREAM-IO.

DEFINE VAR R1 AS INTEGER.
DEFINE VAR R2 AS INTEGER.
DEFINE VAR W_Usu AS CHARACTER FORMAT "X(50)".
DEFINE VAR W_Com AS CHARACTER FORMAT "X(100)".
DEFINE VAR W_Concepto AS CHARACTER FORMAT "X(100)".

IF REstado EQ 2 THEN ASSIGN R1 = 0 R2 = 1.
ELSE ASSIGN R1 = REstado R2 = REstado.
OUTPUT TO VALUE(Listado) PAGED PAGE-SIZE 65.
  VIEW FRAME F-Enc.
  FOR EACH Cobros WHERE
           Cobros.Nit         EQ Clientes.Nit AND
           Cobros.Num_Credito EQ Creditos.Num_Credito AND
           Cobros.Estado      GE R1 AND
           Cobros.Estado      LE R2 BREAK BY Cobros.Fec_Acuerdo:
     FIND Usuarios WHERE Usuarios.Usuario EQ Cobros.Usuario NO-LOCK NO-ERROR.
     W_Usu = "No Encontrado".
     IF AVAILABLE Usuario THEN W_Usu = Usuarios.Usuario + " - " + Usuarios.Nombre.
     W_Com = "".
     IF Cobros.Estado NE 0 THEN DO:
        W_Com = "Compromiso : " + STRING(Cobros.Fec_Compromiso) +  " Valor: " + STRING(Cobros.Val_Compromiso).
     END.
     FIND Varios WHERE Varios.Tipo EQ 30 AND Varios.Codigo EQ Cobros.Cod_Tipo NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN
        W_Concepto = STRING(Varios.Codigo,"99999") + " - " + Varios.descripcion.
     ELSE
        W_Concepto = "00000 - Ninguno".
     DISPLAY Cobros.Fec_Acuerdo " : " W_Concepto SKIP
             STRING(Cobros.Hora,"hh:mm am")  AT 3
             Cobros.Observacion AT 16 VIEW-AS EDITOR SIZE 84 BY 5 SKIP 
             W_Usu  AT 16 SKIP
             W_Com  AT 16 SKIP
             "-----------------------------------------------------------------------------------------------------" AT 1 SKIP
     WITH FRAME Fmov1 WIDTH 132 NO-LABELS USE-TEXT STREAM-IO NO-BOX.
  END.
  VIEW FRAME F-Pie.
  PAGE.
OUTPUT CLOSE.
W_ok = ecompromisos:READ-FILE(Listado) IN FRAME F_infCompromisos.
VIEW FRAME F_InfCompromisos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Compromiso wWin 
PROCEDURE Inicializar_Compromiso :
DO WITH FRAME F_Compromisos:
   ASSIGN CAgencia:SCREEN-VALUE = CAgencia:ENTRY(Clientes.Agencia)
       Cobros.Val_Compromiso:SCREEN-VALUE = "0"
       Cmb_Cpto:SCREEN-VALUE = Cmb_Cpto:ENTRY(1)
       ECompromiso:SCREEN-VALUE = ""
       Cobros.Estado:SCREEN-VALUE = "0".
   IF AVAILABLE Creditos THEN
       Cobros.Num_Credito:SCREEN-VALUE  = STRING(Creditos.Num_Credito).
   ELSE
       Cobros.Num_Credito:SCREEN-VALUE  = "".
   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializa_HojaVida wWin 
PROCEDURE Inicializa_HojaVida :
DO WITH FRAME F_HojaVida:
  ASSIGN Hoja_Vida.Tipo:SCREEN-VALUE = "0"
         NomTipo:SCREEN-VALUE = ""
         Hoja_Vida.Codigo:SCREEN-VALUE = "0"
         Nom_Codigo:SCREEN-VALUE = ""
         HV_Doc:SCREEN-VALUE = ""
         Hoja_Vida.DoctoRefer:SCREEN-VALUE = ""
         Hoja_Vida.Usuario:SCREEN-VALUE = ""
         HV_NomUsuario:SCREEN-VALUE = ""
         HV_Hora:SCREEN-VALUE = ""
         HV_Instancia:SCREEN-VALUE = "0"
         Hoja_Vida.Fec_Grabacion:SCREEN-VALUE = ""
         Hoja_Vida.Observacion:SCREEN-VALUE = "".
         
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

  DO WITH FRAME F_Opciones:
     ASSIGN FIni = TODAY - DAY(TODAY) + 1
            FFin = TODAY.
  END.
  DO WITH FRAME F_HojaVida:
    FOR EACH Cfg_Varios WHERE Cfg_Varios.Visualizar EQ YES BREAK BY Cfg_Varios.Tipo:
        W_Ok = Cmb_Tipos:ADD-LAST(STRING(Cfg_Varios.Tipo,"999") + " - " + Cfg_Varios.Descripcion).
    END.
  END.
  FOR EACH Varios WHERE Varios.Tipo   EQ 30 
                    AND Varios.Codigo GT 56
                    AND Varios.Estado EQ 1 NO-LOCK:
      Cmb_Cpto:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descrip) IN FRAME F_Compromisos.
  END.
  DO WITH FRAME F_ConMovInstancias:
      FOR EACH Varios WHERE Varios.Tipo EQ 9 BREAK BY Cfg_Varios.Tipo:
          W_Ok = Cmb_TipIns:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
      END.
  END.
  FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
      W_Ok = CAgencia:ADD-LAST (STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Compromisos.
  END.
  RUN SUPER.
  APPLY 'entry' TO Buscar IN FRAME F_Consulta.
  HIDE BROWSE binfo.
  HIDE FRAME F_ConsCred.

  Pantalla:DELIMITER = "!".
  /* Code placed here will execute AFTER standard behavior.    */
  RUN pPrntHndle IN h_w_captura-pqr(THIS-PROCEDURE).
  RUN hideObject IN h_w_captura-pqr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_Celda wWin 
PROCEDURE Llenar_Celda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    chWorkSheet:Range(ValCol):Value = Dato.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mensaje_Varios wWin 
PROCEDURE Mensaje_Varios :
DEFINE INPUT PARAMETER TipW LIKE Varios.Tipo.
DEFINE INPUT PARAMETER CodW LIKE Varios.Codigo.
DEFINE OUTPUT PARAMETER TexW AS CHARACTER FORMAT "X(25)".
     
DO WITH FRAME F_HojaVida.
  FIND Cfg_Varios WHERE Cfg_Varios.Tipo EQ TipW NO-LOCK NO-ERROR.
  IF AVAILABLE Cfg_Varios THEN
     ASSIGN Hoja_Vida.Tipo:SCREEN-VALUE = STRING(Cfg_Varios.Tipo,"999")
            NomTipo:SCREEN-VALUE = Cfg_Varios.Descripcion.
  FIND Varios WHERE Varios.Tipo EQ TipW AND Varios.Codigo EQ CodW NO-LOCK NO-ERROR.
  IF AVAILABLE Varios THEN
     ASSIGN Nom_Codigo:SCREEN-VALUE = Varios.Descripcion
            TexW = STRING(Varios.Tipo,"999") + " - " + Varios.Descripcion.

  IF Hoja_Vida.Instancia NE 0 THEN DO:
     FIND Instancias WHERE Instancias.Instancia EQ Hoja_Vida.Instancia NO-LOCK NO-ERROR.
     IF AVAILABLE Instancias THEN
        HV_Doc:SCREEN-VALUE = Instancias.Nom_Instancia.
  END.
  ELSE HV_Doc:SCREEN-VALUE = "".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Codeudor wWin 
PROCEDURE Mostrar_Codeudor :
DO WITH FRAME F_AdmCodeudores:
   ASSIGN WC_UbicacionResidencia:SCREEN-VALUE = ""
          WC_UbicacionResidencia = "".
   ASSIGN WC_UbicacionComercial:SCREEN-VALUE = ""
          WC_UbicacionComercial= "".
   IF AVAILABLE Relaciones THEN DO:
       FIND FIRST Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
       IF AVAILABLE Clientes THEN DO:
           ASSIGN Clientes.DIR_Residencia:SCREEN-VALUE = Clientes.DIR_Residencia
                  Clientes.Tel_Residencia:SCREEN-VALUE = Clientes.Tel_Residencia
                  Clientes.DIR_Comercial:SCREEN-VALUE  = Clientes.DIR_Comercial
                  Clientes.Tel_Comercial:SCREEN-VALUE  = Clientes.Tel_Comercial
                  Clientes.Salario:SCREEN-VALUE        = STRING(Clientes.Salario)
                  Clientes.email:SCREEN-VALUE          = Clientes.email
                  Clientes.Celular:SCREEN-VALUE        = Clientes.Celular
                  Clientes.Fec_Nacimiento:SCREEN-VALUE = STRING(Clientes.Fec_Nacimiento).
           DEFINE VAR WN AS CHARACTER FORMAT "X(8)".
           IF Clientes.Lugar_Residencia NE "" THEN DO:
              FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (SUBSTRING(Clientes.Lugar_Residencia,1,2) + "000000") NO-LOCK NO-ERROR.
              IF AVAILABLE Ubicacion THEN
                 ASSIGN WN = SUBSTRING(Clientes.Lugar_Residencia,1,2)
                        WC_UbicacionResidencia = WC_UbicacionResidencia + " " + Ubicacion.Nombre.
              FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (WN + SUBSTRING(Clientes.Lugar_Residencia,3,3) + "000") NO-LOCK NO-ERROR.
              IF AVAILABLE Ubicacion THEN
                 ASSIGN WN = WN + SUBSTRING(Clientes.Lugar_Residencia,3,3)
                        WC_UbicacionResidencia = WC_UbicacionResidencia + " " + Ubicacion.Nombre.
              FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (WN + SUBSTRING(Clientes.Lugar_Residencia,6,3)) NO-LOCK NO-ERROR.
              IF AVAILABLE Ubicacion THEN
                 ASSIGN WN = WN + SUBSTRING(Clientes.Lugar_Residencia,6,3)
                        WC_UbicacionResidencia = WC_UbicacionResidencia + " " + Ubicacion.Nombre.
              WC_UbicacionResidencia:SCREEN-VALUE = WN + " - " + WC_UbicacionResidencia.
           END.
           IF Clientes.Lugar_Comercial NE "" THEN DO:
              FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (SUBSTRING(Clientes.Lugar_Comercial,1,2) + "000000") NO-LOCK NO-ERROR.
              IF AVAILABLE Ubicacion THEN
                 ASSIGN WN = SUBSTRING(Clientes.Lugar_Comercial,1,2)
                        WC_UbicacionComercial = WC_UbicacionComercial + " " + Ubicacion.Nombre.
              FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (WN + SUBSTRING(Clientes.Lugar_Comercial,3,3) + "000") NO-LOCK NO-ERROR.
              IF AVAILABLE Ubicacion THEN
                 ASSIGN WN = WN + SUBSTRING(Clientes.Lugar_Comercial,3,3)
                        WC_UbicacionComercial = WC_UbicacionComercial + " " + Ubicacion.Nombre.
              FIND Ubicacion WHERE Ubicacion.Ubicacion EQ (WN + SUBSTRING(Clientes.Lugar_Comercial,6,3)) NO-LOCK NO-ERROR.
              IF AVAILABLE Ubicacion THEN
                 ASSIGN WN = WN + SUBSTRING(Clientes.Lugar_Comercial,6,3)
                        WC_UbicacionComercial = WC_UbicacionComercial + " " + Ubicacion.Nombre.
              WC_UbicacionComercial:SCREEN-VALUE = WN + " - " + WC_UbicacionComercial.
           END.
       END.
   END.
   ELSE DO:
       ASSIGN Clientes.DIR_Residencia:SCREEN-VALUE = ""
              Clientes.Tel_Residencia:SCREEN-VALUE = ""
              Clientes.email:SCREEN-VALUE = ""
              Clientes.Celular:SCREEN-VALUE = ""
              Clientes.Fec_Nacimiento:SCREEN-VALUE = ""
              Clientes.DIR_Comercial:SCREEN-VALUE = ""
              Clientes.Tel_Comercial:SCREEN-VALUE = ""
              Clientes.Salario:SCREEN-VALUE = "".
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Compromiso wWin 
PROCEDURE Mostrar_Compromiso :
IF AVAILABLE Cobros THEN DO:
    DISPLAY Cobros.Num_Credito Cobros.Val_Compromiso Cobros.Fec_Compromiso WITH FRAME F_Compromisos.
    IF Cobros.Estado GE 1 THEN
      Cobros.Estado:SCREEN-VALUE =  "1".
    ELSE
      Cobros.Estado:SCREEN-VALUE =  "0".
    ECompromiso:SCREEN-VALUE   = Cobros.Observacion.
    CAgencia:SCREEN-VALUE      = CAgencia:ENTRY(Clientes.Agencia).
    FIND Varios WHERE Varios.Tipo EQ 30 AND Varios.Codigo EQ Cobros.Cod_Tipo NO-LOCK NO-ERROR.
    IF AVAILABLE Varios THEN
       Cmb_Cpto:SCREEN-VALUE      = STRING(Varios.Codigo,"99999") + " - " + Varios.descripcion.
    ELSE
       Cmb_Cpto:SCREEN-VALUE      = "00000 - Ninguno".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_HV wWin 
PROCEDURE Mostrar_HV :
DO WITH FRAME F_HojaVida:
ASSIGN Hoja_Vida.Usuario:SCREEN-VALUE = Hoja_Vida.Usuario
            Hoja_Vida.Fec_Grabacion:SCREEN-VALUE = STRING(Hoja_Vida.Fec_Grabacion)
            Hoja_Vida.DoctoRefer:SCREEN-VALUE = STRING(Hoja_Vida.DoctoRefer)
            Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = STRING(Hoja_Vida.Asunto_Cumplido)
            HV_Instancia:SCREEN-VALUE = STRING(Hoja_Vida.Instancia)
            Hoja_Vida.Observacion:SCREEN-VALUE = Hoja_Vida.Observacion
            HV_Hora:SCREEN-VALUE = STRING(Hoja_Vida.Hora,"hh:mm:ss AM")
            Hoja_Vida.Codigo:SCREEN-VALUE = STRING(Hoja_Vida.Codigo).
     FIND Varios WHERE Varios.Tipo EQ Hoja_Vida.Tipo AND Varios.Codigo EQ Hoja_Vida.Codigo NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN DO:
        IF TIni NE 0 AND TFin NE 999 AND Cmb_CodHV:SCREEN-VALUE NE "00000 - Todos los Códigos" THEN
           Cmb_CodHV:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion.
        ASSIGN Nom_Codigo:SCREEN-VALUE = Varios.Descripcion.
     END.
     FIND Usuarios WHERE Usuarios.Usuario EQ Hoja_Vida.Usuario NO-LOCK NO-ERROR.
     IF AVAILABLE Usuarios THEN
        HV_NomUsuario:SCREEN-VALUE = Usuarios.Nombre.
     RUN Mensaje_Varios (INPUT Hoja_Vida.Tipo, INPUT Hoja_Vida.Codigo, OUTPUT W_TextCombo).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Informacion wWin 
PROCEDURE Mostrar_Informacion :
DEFINE VAR cont AS INTEGER.
DEFINE VAR contGen AS INTEGER.

ASSIGN FRAME F_Opciones Id_General Id_Economica Id_Relaciones Id_Saldos Id_Creditos
             Id_Ahorros Id_HojaVida Id_Solicitudes Id_GarAdm Id_CredCanc Id_GestCobro.

DO WITH FRAME F_Consulta:
    IF Cmb_tipo:SCREEN-VALUE <> "Imprimir todos" THEN DO:
        DO Cont = 1 TO 1000 BY 1:
            W_Ok = Pantalla:DELETE(1).
        END.
    END.
    
    /*Clientes*/
    IF Id_General THEN DO:
       FOR EACH TInfo WHERE TInfo.Tip = "GEN" BY TInfo.Reg:
           CREATE Tmp.
           BUFFER-COPY TInfo TO Tmp.
           contGen = TInfo.Reg.
       END.

       /*CREATE TInfo.
       ASSIGN TInfo.tip = "GEN"
              TInfo.reg = contGen + 1
              TInfo.cedula = clientes.nit.

       CREATE Tmp.
       BUFFER-COPY TInfo TO Tmp.*/
    END.
    
    IF Id_Economica THEN DO:
       FOR EACH TInfo WHERE TInfo.Tip = "ECO" BY TInfo.Reg:
           CREATE Tmp.
           BUFFER-COPY TInfo TO Tmp.
           /*W_Ok = Pantalla:ADD-LAST(TInfo.Tex).*/
       END.
    END.

    IF Id_Ahorros THEN DO:
        FOR EACH TInfo WHERE TInfo.Tip = "AHO" BY TInfo.Reg:
            CREATE Tmp.
            BUFFER-COPY TInfo TO Tmp.
            /*W_Ok = Pantalla:ADD-LAST(TInfo.Tex).*/
        END.
    END.
    
    IF Id_Creditos THEN DO:
       FOR EACH TInfo WHERE TInfo.Tip = "CRE" BY TInfo.Reg:
           CREATE Tmp.
           BUFFER-COPY TInfo TO Tmp.
           /*W_Ok = Pantalla:ADD-LAST(TInfo.Tex).*/
       END.
    END.

    FOR EACH TInfo WHERE TInfo.tip = "CxC" BY TInfo.Reg:
        CREATE Tmp.
        BUFFER-COPY TInfo TO Tmp.
    END.

    IF Id_CredCanc THEN FOR EACH TInfo WHERE TInfo.Tip = "CRC" BY TInfo.Reg:
       CREATE Tmp.
       BUFFER-COPY TInfo TO Tmp.                 
    END.

    IF Tg_CtasAho THEN DO:
       FOR EACH TInfo WHERE TInfo.Tip = "ECA" BY TInfo.Reg:
           CREATE Tmp.
           BUFFER-COPY TInfo TO Tmp.
       END.
       ASSIGN Tg_CtasAho              = FALSE
              Tg_CtasAho:SCREEN-VALUE = "No".
    END.

    IF Tg_Extras THEN DO:
       FOR EACH TInfo WHERE TInfo.Tip = "Ext" BY TInfo.Reg:
           CREATE Tmp.
           BUFFER-COPY TInfo TO Tmp.
           /*W_Ok = Pantalla:ADD-LAST(TInfo.Tex).*/
       END.
       /*ASSIGN Tg_Extras              = FALSE
              Tg_Extras:SCREEN-VALUE = "No".*/
    END.

    IF Id_Saldos THEN DO:
        FOR EACH TInfo WHERE TInfo.Tip = "SDO" BY TInfo.Reg:
            CREATE Tmp.
            BUFFER-COPY TInfo TO Tmp.
            /*W_Ok = Pantalla:ADD-LAST(TInfo.Tex).*/
        END.
    END.
    
    IF Id_GarAdm THEN DO:
       FOR EACH TInfo WHERE TInfo.Tip = "ADM" BY TInfo.Reg:
           CREATE Tmp.
           BUFFER-COPY TInfo TO Tmp.
           /*W_Ok = Pantalla:ADD-LAST(TInfo.Tex).*/
       END.
    END.
    
    IF Id_AhoCanc THEN DO: /*Sept.20/05 GAER*/
       FOR EACH TInfo WHERE TInfo.Tip = "ACA" BY TInfo.Reg:
           CREATE Tmp.
           BUFFER-COPY TInfo TO Tmp.           
       END.
    END.

    IF Id_Solicitudes THEN DO:
        FOR EACH TInfo WHERE TInfo.Tip = "SOL" BY TInfo.Reg:
            CREATE Tmp.
            BUFFER-COPY TInfo TO Tmp.
            /*W_Ok = Pantalla:ADD-LAST(TInfo.Tex).*/
        END.
    END.
    IF Id_Relaciones THEN DO:
        FOR EACH TInfo WHERE TInfo.Tip = "REL" BY TInfo.Reg:
            CREATE Tmp.
            BUFFER-COPY TInfo TO Tmp.
            /*W_Ok = Pantalla:ADD-LAST(TInfo.Tex).*/
        END.
    END.

    IF Id_HojaVida THEN DO:
        FOR EACH TInfo WHERE TInfo.Tip = "HVA": DELETE TInfo. END.
        RUN BHojaVida.
        FOR EACH TInfo WHERE TInfo.Tip = "HVA" BY TInfo.Reg:
            CREATE Tmp.
            BUFFER-COPY TInfo TO Tmp.
        END.
    END.

    IF Id_GestCobro THEN DO:
        FOR EACH TInfo WHERE TInfo.Tip = "GCO": DELETE TInfo. END.
        RUN Gestion_Cobros.
        FOR EACH TInfo WHERE TInfo.Tip = "GCO" BY TInfo.Reg:
            CREATE Tmp.
            BUFFER-COPY TInfo TO Tmp.
        END.
    END.

    /*IF Id_MovInst THEN DO:
        FOR EACH TInfo WHERE TInfo.Tip = "INS" BY TInfo.Reg:
            CREATE Tmp.
            BUFFER-COPY TInfo TO Tmp.
        END.
    END.*/

    IF Id_Codeudando THEN DO:
       FOR EACH TInfo WHERE TInfo.Tip = "COD" BY TInfo.Reg:
           CREATE Tmp.
           BUFFER-COPY TInfo TO Tmp.
            /*W_Ok = Pantalla:ADD-LAST(TInfo.Tex).*/
       END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovInst wWin 
PROCEDURE MovInst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pActvaVntna wWin 
PROCEDURE pActvaVntna :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    wwin:SENSITIVE = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParentHandle wWin 
PROCEDURE pParentHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER hParent AS HANDLE NO-UNDO.
    /*GHPARENT = hParent.     */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
IF cmb_Tipo:SCREEN-VALUE IN FRAME F_Consulta = "Imprimir todos" THEN
    RUN ProcesoImprimirTodos.
ELSE DO:
    IF W_SiMInst THEN DO:
        RUN Imp_MovInst.
        RETURN.
    END.

    IF R_Encabezado:SCREEN-VALUE IN FRAME F_Impresion EQ "1" THEN DO:
        {Incluido\RepEncabezadoConsultaGeneral.i}

        W_Reporte = "REPORTE   : CONSULTA DE INFORMACION - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
        W_EncColumna = W_Nombre:SCREEN-VALUE IN FRAME F_Consulta.

        VIEW FRAME F-Encabezado.
        /*VIEW FRAME F-Ftr.*/
    END.
    ELSE DO:
        gTexto = W_Nombre:SCREEN-VALUE IN FRAME F_Consulta.
        DISPLAY gTexto SKIP(1) WITH FRAME a WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
    END.

    FOR EACH Tmp BY Tmp.Reg:
        DISPLAY Tmp.Tex WITH FRAME b WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
    END.

    PAGE.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimirTodos wWin 
PROCEDURE ProcesoImprimirTodos :
FOR EACH clientes NO-LOCK BY clientes.apellido1:
    FIND FIRST tmp WHERE tmp.cedula = clientes.nit NO-LOCK NO-ERROR.
    IF AVAILABLE tmp THEN DO:
        IF R_Encabezado:SCREEN-VALUE IN FRAME F_Impresion EQ "1" THEN DO:
            DISPLAY "FONDO EMPLEADOS DOCENTES UNIVERSIDAD NACIONAL DE COLOMBIA" WITH FRAME a1 WIDTH 150 NO-BOX USE-TEXT NO-LABELS.
            DISPLAY STRING("USUARIO   : " + STRING(w_usuario) + " - " + usuarios.nombre) FORMAT "X(120)" WITH FRAME a2 WIDTH 150 NO-BOX USE-TEXT NO-LABELS.
            DISPLAY STRING("REPORTE   : CONSULTA DE INFORMACION - FECHA: " + STRING(w_fecha,"99/99/9999") + " - " + STRING(TIME,"HH:MM")) FORMAT "X(120)" WITH FRAME a3 WIDTH 150 NO-BOX USE-TEXT NO-LABELS.
            DISPLAY "________________________________________________________________________________________________________________________" WITH FRAME a4 WIDTH 150 NO-BOX USE-TEXT NO-LABELS.
            DISPLAY STRING("Documento: " + STRING(tmp.cedula) + " - " + tmp.nombre) FORMAT "X(120)" WITH FRAME a5 WIDTH 150 NO-BOX USE-TEXT NO-LABELS.
            DISPLAY "------------------------------------------------------------------------------------------------------------------------" WITH FRAME a6 WIDTH 150 NO-BOX USE-TEXT NO-LABELS.
        END.
        ELSE DO:
            gTexto = "Documento: " + STRING(tmp.cedula) + " - " + tmp.Nombre.
            DISPLAY gTexto SKIP(1) WITH FRAME a WIDTH 150 NO-BOX USE-TEXT NO-LABELS.
        END.

        FOR EACH tmp WHERE tmp.cedula = clientes.nit BY tmp.reg:
            IF W_SiMInst THEN DO:
                RUN Imp_MovInst.
                RETURN.
            END.

            DISPLAY Tmp.Tex WITH FRAME b WIDTH 150 NO-BOX USE-TEXT NO-LABELS.
        END.
    END.

    PAGE.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QCompromisos wWin 
PROCEDURE QCompromisos :
ASSIGN FRAME F_Compromisos REstado.

FIND FIRST Cobros WHERE 
           Cobros.Nit         EQ Creditos.Nit AND
           Cobros.Num_Credito EQ Creditos.Num_Credito NO-LOCK NO-ERROR.
IF AVAILABLE Cobros THEN DO:
    IF REstado EQ 2 THEN DO:
        OPEN QUERY BrCompromisos
          FOR EACH Cobros WHERE
                   Cobros.Nit         EQ Creditos.Nit AND
                   Cobros.Num_Credito EQ Creditos.Num_Credito AND
                   Cobros.Estado      LT REstado BY Cobros.Fec_Compromiso DESCENDING.
    END.
    ELSE DO:
        OPEN QUERY BrCompromisos
          FOR EACH Cobros WHERE
                   Cobros.Nit         EQ Creditos.Nit AND
                   Cobros.Num_Credito EQ Creditos.Num_Credito AND
                   Cobros.Estado      EQ REstado BY Cobros.Fec_Compromiso DESCENDING.
    END.
    RUN Mostrar_Compromiso.
END.
ELSE DO:
    OPEN QUERY BrCompromisos
      FOR EACH Cobros WHERE
                    Cobros.Nit         EQ Creditos.Nit AND
                    Cobros.Num_Credito EQ Creditos.Num_Credito.
    RUN Inicializar_Compromiso.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Raya wWin 
PROCEDURE Raya :
DEFINE INPUT PARAMETER W_Tabla AS CHARACTER FORMAT "X(3)".
  W_Texto = "------------------------------------------------------------------------------------------------------------------------".
  RUN Grabar_Info (INPUT W_Tabla, INPUT W_Texto).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SdosCoomeva wWin 
PROCEDURE SdosCoomeva :
/*------------------------------------------------------------------------------
  Purpose:  Llena sdos coomeva en temporal.
            invocado desde procedimiento Buscar_Informacion.
  Notes:    Abril 13/05 GAER.
------------------------------------------------------------------------------*/
 /* DEFI VAR Xsd LIKE Ahorros.SDo_Dispon INIT 0.

  FOR EACH TSaldos WHERE TSaldos.Cla EQ 0: DELETE TSaldos. END.

  W_Texto = "------------------------------------------------------------------------------------------------------------------------".
  RUN Grabar_Info (INPUT "COO", INPUT W_Texto).     
  W_Texto = "INFORMACION SALDOS COOMEVA".
  RUN Grabar_Info (INPUT "C00", INPUT W_Texto).     

  RUN HallarSdoTercero IN W_Manija 
     (INPUT TClientes.Nit, INPUT 1, 99,
      INPUT 999, 999, "16051501", YEAR(W_Fecha), MONTH(W_Fecha),
      OUTPUT Xsd, OUTPUT Xsd, OUTPUT Xsd).

  CREATE TSaldos.
  ASSIGN TSaldos.Cla = 0
         TSaldos.Cod = 1
         TSaldos.Nom = "Pdcto Coomeva"
         TSaldos.Dis = Xsd.
           
  RUN HallarSdoTercero IN W_Manija 
     (INPUT TClientes.Nit, INPUT 1, 99,
      INPUT 999, 999, "16559501", YEAR(W_Fecha), MONTH(W_Fecha),
      OUTPUT Xsd, OUTPUT Xsd, OUTPUT Xsd).

  CREATE TSaldos.
  ASSIGN TSaldos.Cla = 0
         TSaldos.Cod = 2
         TSaldos.Nom = "Intereses Coomeva"
         TSaldos.Dis = Xsd.

  RUN BSaldos(INPUT 0).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ver_Compromisos wWin 
PROCEDURE Ver_Compromisos :
FIND Clientes WHERE Clientes.Nit EQ Buscar:SCREEN-VALUE IN FRAME F_Consulta NO-ERROR.
  VIEW FRAME F_Compromisos.
  HIDE FRAME F_Opciones.
  OPEN QUERY BrCreditos FOR EACH Creditos WHERE Creditos.Nit EQ Clientes.Nit 
                                            AND Creditos.Estado EQ 2 NO-LOCK.
  TCompromisos:SCREEN-VALUE = "no".
  ENABLE Btn_AgrCompromiso WITH FRAME F_Compromisos.
  RUN QCompromisos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

