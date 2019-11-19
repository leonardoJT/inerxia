/*  Nombre      : StoreTableBuffers.i
    descripcion : Por cada tabla creada en sfg se debe crear un registro en la tabla temporal
    log         : Creado, 12 oct 2007, Ing. Edilberto Mariño
*/

/*************************/
/* base de datos central */
/*************************/

DEFINE TEMP-TABLE ttTableBuffers
 FIELD cTableName AS CHARACTER
 FIELD hTableHandle AS HANDLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Act_Fijo:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Agencias:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Ahorros:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Alm_Pub:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Anexos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Asesoria:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Atrasos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Bancos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Base_Ret:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.biometrico:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.BorradorSipla:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.cajero:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Calendario:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.CarteraVencida:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Cen_Costos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Cfg_Instancias:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Cfg_Novedades:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Cfg_OrigRegCredito:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Cfg_PUB:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Cfg_RegCredito:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Cfg_Varios:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Che_Blo:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Che_Transito:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Ciiu:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Clientes:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Cobros:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Comprobantes:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Conciliacion:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.ControlSipla:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.CortoLargo:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Creditos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Cuentas:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Deducible:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Desplazamiento:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Detalle:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Diferido:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Documentos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Empleados:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Empresas:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Entidad:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Especiales:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Estaciones:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Extras:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Facturacion:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Formatos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Garantias:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Grupos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Hoja_Vida:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.HVEmpleados:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Indicadores:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Ind_PUB:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Instancias:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Inversion_Sdos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Lib_Chequera:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Liqui_Int:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.ListaNegra:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Logs:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Men_Sistema:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Activos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Ahorros:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Cobros:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Contable:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Creditos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_CtaConta:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Diferido:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Distrib:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Especiales:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Extracto:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_GMF:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_InsSipla:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Instancias:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Inversion:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_juzgado:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_Ope:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_PUB:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Mov_valoracion:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Novedades_Nit:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Novedades_Nomina:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Operacion:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.PAGADURIA_INCON:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Perfiles_Nomina:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Per_Facturacion:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.PlanPagos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Plastico:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Presupuesto:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.ProcDia:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Programas:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Provision:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Pro_Activo:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Pro_Ahorros:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Pro_Creditos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Pro_Diferidos:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Pro_Especiales:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Pro_Grupo:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Pro_Inversiones:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Pro_scoring:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.PUB:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Ran_Intereses:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Rec_Nomina:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Relaciones:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Res_operacion:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Sal_Cuenta:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Sal_presupuesto:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Scoring:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Solicitud:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Taquilla:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.TarjetaDebito:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Tasas_Mercado:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Terceros:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Total_agencia:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Ubicacion:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Usuarios:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Varios:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER bdcentral.Zonas:HANDLE
 cTableName = hTableHandle:TABLE.




/*****************************/
/* base de datos repositorio */
/*****************************/

CREATE ttTableBuffers.
ASSIGN
 hTableHandle = BUFFER repositorio.repositorio:HANDLE
 cTableName = hTableHandle:TABLE.

CREATE tttablebuffers.
ASSIGN  hTableHandle = BUFFER repositorio.conf_repositorio:HANDLE
        cTableName = hTableHandle:TABLE.
