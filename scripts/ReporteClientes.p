DEFINE TEMP-TABLE ttclientes
    FIELD tipo_identificacion AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD genero AS CHARACTER
    FIELD fec_ingresoChr AS CHARACTER
    FIELD fec_retiroChr AS CHARACTER
    FIELD fec_ultActualizaChr AS CHARACTER
    FIELD fec_nacimientoChr AS CHARACTER
    FIELD motivo_ingreso AS CHARACTER
    FIELD med_publicitario AS CHARACTER
    FIELD motivo_retiro AS CHARACTER
    FIELD profesion AS CHARACTER
    FIELD codigo_ciiu AS INTEGER
    FIELD est_civil AS CHARACTER
    FIELD per_acargo AS INTEGER
    FIELD num_hijos AS INTEGER
    FIELD niv_educativo AS CHARACTER
    FIELD estrato AS INTEGER
    FIELD fec_expedicionChr AS CHARACTER
    FIELD ciudad_expedicionChr AS CHARACTER
    FIELD departamento_expedicionChr AS CHARACTER
    FIELD ciudad_nacimientoChr AS CHARACTER
    FIELD departamento_nacimientoChr AS CHARACTER
    FIELD tel_residencia AS CHARACTER
    FIELD DIR_residencia AS CHARACTER
    FIELD ciudad_residenciaChr AS CHARACTER
    FIELD departamento_residenciaChr AS CHARACTER
    FIELD zona AS CHARACTER
    FIELD email AS CHARACTER
    FIELD DIR_correspondenciaChr AS CHARACTER
    FIELD DIR_comercial AS CHARACTER
    FIELD tel_comercial AS CHARACTER
    FIELD celular AS CHARACTER
    FIELD ciudad_comercialChr AS CHARACTER
    FIELD departamento_comercialChr AS CHARACTER
    FIELD usuarioChr AS CHARACTER
    FIELD estadoChr AS CHARACTER
    FIELD tipo_viviendaChr AS CHARACTER
    FIELD nomEmpresa AS CHARACTER
    FIELD fec_ingEmpresaChr AS CHARACTER
    FIELD tip_contratoChr AS CHARACTER
    FIELD carnet AS CHARACTER
    FIELD cargo AS CHARACTER
    FIELD salario AS DECIMAL
    FIELD ing_arriendos AS DECIMAL
    FIELD ing_honorarios AS DECIMAL
    FIELD ing_financieros AS DECIMAL
    FIELD ing_otros AS DECIMAL
    FIELD act_casa AS DECIMAL
    FIELD act_vehiculos AS DECIMAL
    FIELD act_inversiones AS DECIMAL
    FIELD gto_familiar AS DECIMAL
    FIELD gto_obligacion AS DECIMAL
    FIELD sdo_obligaciones AS DECIMAL
    FIELD gto_arriendo AS DECIMAL
    FIELD nom_arrendatario AS CHARACTER
    FIELD tel_arrendatario AS CHARACTER
    FIELD gtoFinanc_indir AS DECIMAL
    FIELD endeud_indirecto AS DECIMAL
    FIELD tipo_actividad AS CHARACTER
    FIELD respaldo_patrimonial AS CHARACTER
    FIELD privilegiado AS CHARACTER
    FIELD fec_fallecidoChr AS CHARACTER
    FIELD id_exoneradoSiplaChr AS CHARACTER
    FIELD fecIni_NoSiplaChr AS CHARACTER
    FIELD mujer_cabezaFliaChr AS CHARACTER
    FIELD admin_recursosChr AS CHARACTER
    FIELD ejerce_poder_publicoChr AS CHARACTER
    FIELD reconocimiento_publicoChr AS CHARACTER
    FIELD administra_recursos_tercerosChr AS CHARACTER
    FIELD recibe_girosChr AS CHARACTER
    FIELD subsidiado AS CHARACTER
    FIELD nombre_subsidia AS CHARACTER
    FIELD parentesco_subsidia AS CHARACTER
    FIELD ID_subsidia AS CHARACTER.

FOR EACH clientes NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.nit = clientes.nit
                         AND ahorros.tip_ahorro = 4 NO-LOCK NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        CREATE ttclientes.
        BUFFER-COPY clientes TO ttclientes.

        CASE clientes.sexo:
            WHEN 1 THEN ttclientes.genero = "M".
            WHEN 2 THEN ttclientes.genero = "F".
        END CASE.

        ttclientes.fec_ingresoChr = STRING(clientes.fec_ingreso,"99/99/9999").
        ttclientes.fec_retiroChr = STRING(clientes.fec_retiro,"99/99/9999").
        ttclientes.fec_ultActualizaChr = STRING(clientes.fec_ultActualiza,"99/99/9999").
        ttclientes.fec_nacimientoChr = STRING(clientes.fec_nacimiento,"99/99/9999").

        FIND FIRST varios WHERE varios.tipo = 4
                            AND varios.codigo = clientes.cod_ingreso NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN
            ttclientes.motivo_ingreso = varios.descripcion.

        FIND FIRST varios WHERE varios.tipo = 5
                            AND varios.codigo = clientes.cod_retiro NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN
            ttclientes.motivo_retiro = varios.descripcion.

        FIND FIRST varios WHERE varios.tipo = 1
                            AND varios.codigo = clientes.cod_profesion NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN
            ttclientes.profesion = varios.descripcion.

        ttclientes.fec_expedicionChr = STRING(clientes.fec_expedicion,"99/99/9999").

        FIND FIRST ubicacion WHERE ubicacion.ubicacion = SUBSTRING(STRING(clientes.lugar_expedicion,"99999999"),1,5) + "000" NO-LOCK NO-ERROR.
        IF AVAILABLE ubicacion THEN
            ttclientes.ciudad_expedicionChr = ubicacion.nombre.

        FIND FIRST ubicacion WHERE ubicacion.ubicacion = SUBSTRING(STRING(clientes.lugar_expedicion,"99999999"),1,2) + "000000" NO-LOCK NO-ERROR.
        IF AVAILABLE ubicacion THEN
            ttclientes.departamento_expedicionChr = ubicacion.nombre.

        FIND FIRST ubicacion WHERE ubicacion.ubicacion = SUBSTRING(STRING(clientes.lugar_nacimiento,"99999999"),1,5) + "000" NO-LOCK NO-ERROR.
        IF AVAILABLE ubicacion THEN
            ttclientes.ciudad_nacimientoChr = ubicacion.nombre.

        FIND FIRST ubicacion WHERE ubicacion.ubicacion = SUBSTRING(STRING(clientes.lugar_nacimiento,"99999999"),1,2) + "000000" NO-LOCK NO-ERROR.
        IF AVAILABLE ubicacion THEN
            ttclientes.departamento_nacimientoChr = ubicacion.nombre.

        FIND FIRST ubicacion WHERE ubicacion.ubicacion = SUBSTRING(STRING(clientes.lugar_residencia,"99999999"),1,5) + "000" NO-LOCK NO-ERROR.
        IF AVAILABLE ubicacion THEN
            ttclientes.ciudad_residenciaChr = ubicacion.nombre.

        FIND FIRST ubicacion WHERE ubicacion.ubicacion = SUBSTRING(STRING(clientes.lugar_residencia,"99999999"),1,2) + "000000" NO-LOCK NO-ERROR.
        IF AVAILABLE ubicacion THEN
            ttclientes.departamento_residenciaChr = ubicacion.nombre.

        FIND FIRST zonas WHERE zonas.cod_zona = clientes.cod_zona NO-LOCK NO-ERROR.
        IF AVAILABLE zonas THEN
            ttclientes.zona = zonas.nombre.

        IF clientes.DIR_correspondencia = YES THEN
            ttclientes.DIR_correspondenciaChr = "Residencia".
        ELSE
            ttClientes.DIR_correspondenciaChr = "Trabajo".

        FIND FIRST ubicacion WHERE ubicacion.ubicacion = SUBSTRING(STRING(clientes.lugar_comercial,"99999999"),1,5) + "000" NO-LOCK NO-ERROR.
        IF AVAILABLE ubicacion THEN
            ttclientes.ciudad_comercialChr = ubicacion.nombre.

        FIND FIRST ubicacion WHERE ubicacion.ubicacion = SUBSTRING(STRING(clientes.lugar_comercial,"99999999"),1,2) + "000000" NO-LOCK NO-ERROR.
        IF AVAILABLE ubicacion THEN
            ttclientes.departamento_comercialChr = ubicacion.nombre.

        FIND FIRST usuarios WHERE usuarios.usuario = clientes.usuario NO-LOCK NO-ERROR.
        IF AVAILABLE usuarios THEN
            ttclientes.usuarioChr = usuarios.nombre.

        IF clientes.estado = 1 THEN
            ttclientes.estadoChr = "Activo".
        ELSE
            ttclientes.estadoChr = "Renunciado".

        CASE clientes.tipo_vivienda:
            WHEN 1 THEN ttclientes.tipo_viviendaChr = "Propia".
            WHEN 2 THEN ttclientes.tipo_viviendaChr = "Arrendada".
        END CASE.

        FIND FIRST empresas WHERE empresas.cod_empresa = clientes.cod_empresa NO-LOCK NO-ERROR.
        IF AVAILABLE empresas THEN
            ttClientes.nomEmpresa = empresas.Alias_Empresa.

        ttclientes.fec_ingEmpresaChr = STRING(clientes.fec_IngEmpresa,"99/99/9999").

        CASE clientes.tip_contrato:
            WHEN 0 THEN ttclientes.tip_contratoChr = "Ninguno".
            WHEN 1 THEN ttclientes.tip_contratoChr = "Indefinido".
            WHEN 2 THEN ttclientes.tip_contratoChr = "Fijo".
            WHEN 3 THEN ttclientes.tip_contratoChr = "Labor contratada".
            WHEN 4 THEN ttclientes.tip_contratoChr = "Prestación de servicios".
        END CASE.

        FIND FIRST varios WHERE varios.tipo = 2
                            AND varios.codigo = clientes.cod_cargo NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN
            ttclientes.cargo = varios.descripcion.

        CASE clientes.id_privilegiado:
            WHEN 0 THEN ttClientes.privilegiado = "No".
            WHEN 1 THEN ttClientes.privilegiado = "Sí".
        END CASE.

        ttclientes.fec_fallecidoChr = STRING(clientes.fec_fallecido,"99/99/9999").

        IF clientes.id_exoneradoSipla = YES THEN DO:
            ttclientes.id_exoneradoSiplaChr = "Sí".
            ttclientes.fecIni_NoSiplaChr = STRING(clientes.fecIni_NoSipla,"99/99/9999").
        END.
        ELSE
            ttclientes.id_exoneradoSiplaChr = "No".

        IF clientes.mujer_cabezaFlia = 1 THEN
            ttclientes.mujer_cabezaFliaChr = "Sí".
        ELSE
            ttclientes.mujer_cabezaFliaChr = "No".

        IF clientes.admin_recursos = YES THEN
            ttclientes.admin_recursosChr = "Sí".
        ELSE
            ttclientes.admin_recursosChr = "No".

        IF clientes.ejerce_poder_publico = YES THEN
            ttclientes.ejerce_poder_publicoChr = "Sí".
        ELSE
            ttclientes.ejerce_poder_publicoChr = "No".

        IF clientes.reconocimiento_publico = YES THEN
            ttclientes.reconocimiento_publicoChr = "Sí".
        ELSE
            ttclientes.reconocimiento_publicoChr = "No".

        IF clientes.administra_recursos_terceros = YES THEN
            ttclientes.administra_recursos_tercerosChr = "Sí".
        ELSE
            ttclientes.administra_recursos_tercerosChr = "No".

        IF clientes.recibe_giros = YES THEN
            ttclientes.recibe_girosChr = "Sí".
        ELSE
            ttclientes.recibe_girosChr = "No".

        IF clientes.subcidiado_x_tercero = YES THEN DO:
            ttclientes.subsidiado = "Sí".
            ttclientes.nombre_subsidia = clientes.nombre_que_subcidia.
            ttClientes.parentesco_subsidia = clientes.parentesco_que_subcidia.
            ttClientes.id_subsidia = clientes.identificacion_que_subcidia.
        END.
        ELSE
            ttclientes.subsidiado = "No".

    END.
END.

OUTPUT TO e:\inerxia\clientes.csv.
EXPORT DELIMITER ";"
    "Tipo_ID"
    "Identificación"
    "Nombre"
    "Apellido1"
    "Apellido2"
    "Género"
    "Fecha_ingreso"
    "Fecha_retiro"
    "Fecha_ultima_actualización"
    "Fecha_nacimiento"
    "Motivo_ingreso"
    "Medio_publicitario"
    "Motivo_retiro"
    "Profesión"
    "Código_CIIU"
    "Estado_civil"
    "Personas_a_cargo"
    "Número_de_hijos"
    "Nivel_educativo"
    "Estrato"
    "Fecha_expedición_ID"
    "Ciudad_expedición_ID"
    "Departamento_expedición_ID"
    "Ciudad_nacimiento"
    "Departamento_nacimiento"
    "Teléfono_residencia"
    "Dirección_residencia"
    "Ciudad_residencia"
    "Departamento_residencia"
    "Zona_residencia"
    "Correo_electrónico"
    "Dirección_de_correspondencia"
    "Dirección_comercial"
    "Teléfono_comercial"
    "Celular"
    "Ciudad_comercial"
    "Departamento_comercial"
    "Usuario_matricula"
    "Estado"
    "Tipo_de_vivienda"
    "Empresa"
    "Fecha_ingreso_empresa"
    "Tipo_contrato"
    "Carnet"
    "Cargo"
    "Salario"
    "Ingresos_x_arriendos"
    "Ingresos_x_honorarios"
    "Ingresos_financieros"
    "Otros_ingresos"
    "Valor_propiedades"
    "Valor_vehículos"
    "Valor_inversiones"
    "Gastos_familiares"
    "Gastos_obligaciones_financieras"
    "Total_obligaciones_financieras"
    "Gato_arrendamiento"
    "Nombre_arrendatario"
    "Teléfono_arrendatario"
    "Gastos_financieros_indirectos"
    "Endeudamiento_indirecto"
    "Tipo_de_actividad"
    "Respaldo_patrimonial"
    "Privilegiado"
    "Fecha_fallecimiento"
    "Exonerado_SIFLAFT"
    "Fecha_exoneración_SIPLAFT"
    "Mujer_Cabeza_de_familia"
    "Administra_recursos_públicos"
    "Ejerce_poder_público"
    "Reconocimiento_público"
    "Administra_recursos_de_terceros"
    "Recibe_giros"
    "Subsidiado"
    "Nombre_quien_subsidia"
    "Parenteco_quien_subsidia"
    "ID_quien_subsidia".

FOR EACH ttclientes NO-LOCK:
    EXPORT DELIMITER ";" ttclientes.
END.
OUTPUT CLOSE.

