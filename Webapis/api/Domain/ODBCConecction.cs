using System;
using System.Data;
using Newtonsoft.Json;
using System.Data.Odbc;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using CreditosAPI.Model;

namespace CreditosAPI.Domain
{
    public class ODBCConecction
    {
        public IConfiguration Configuration { get; set; }

        const string ahorroQuery = "select * from(select mov.fecha as 'Fecha',right('0' + cast(cast(mov.hora / 3600 as integer) as varchar(2)), 2) + ':' + right('0' + cast(cast(mod(mov.hora, 3600) / 60 as integer) as varchar(2)), 2) as 'Hora', mov.descrip as 'Descripcion'," +
                "'Consignación' as 'TipoOperacion', mov.val_efectivo + mov.val_cheque as 'Valor', mov.nit, mov.cue_ahorros as 'CuentaAhorros', mov.hora as 'Time' from pub.mov_ahorros mov left join pub.operacion op on op.cod_operacion = mov.cod_operacion where op.tipo_operacion = 1 " +
                "union select mov.fecha as 'Fecha', right('0' + cast(cast(mov.hora / 3600 as integer) as varchar(2)), 2) + ':' + right('0' + cast(cast(mod(mov.hora, 3600) / 60 as integer) as varchar(2)), 2) as 'Hora', mov.descrip as 'Descripcion', 'Retiro' as 'TipoOperacion', " +
                "mov.val_efectivo + mov.val_cheque as 'Valor', mov.nit as 'Nit', mov.cue_ahorros as 'CuentaAhorros', mov.hora as 'Time' from pub.mov_ahorros mov left join pub.operacion op on op.cod_operacion = mov.cod_operacion where op.tipo_operacion = 2) t where t.nit = '{0}' " +
                "and t.CuentaAhorros = '{1}' order by t.fecha, t.hora";
        const string creditoQuery = "select * from(select mov.fecha as 'Fecha',right('0' + cast(cast(mov.hora / 3600 as integer) as varchar(2)), 2) + ':' + right('0' + cast(cast(mod(mov.hora, 3600) / 60 as integer) as varchar(2)), 2) as 'Hora', mov.descrip as 'Descripcion', " +
                "'Abono-Pago' as 'TipoOperacion', mov.val_efectivo + mov.val_cheque as 'Valor',mov.sdo_capital as 'SaldoCapital',mov.nit as 'Nit',mov.num_credito as 'NumeroCredito',mov.hora as 'Time' from pub.mov_creditos mov left join pub.operacion op on op.cod_operacion = mov.cod_operacion " +
                "where op.tipo_operacion = 1 union select mov.fecha as 'Fecha',right('0' + cast(cast(mov.hora / 3600 as integer) as varchar(2)), 2) + ':' + right('0' + cast(cast(mod(mov.hora, 3600) / 60 as integer) as varchar(2)), 2) as 'Hora',mov.descrip as 'Descripcion', " +
                "'Avance-Desembolso' as 'TipoOperacion',mov.val_efectivo + mov.val_cheque as 'Valor',mov.sdo_capital as 'SaldoCapital',mov.nit as 'Nit',mov.num_credito as 'NumeroCredito',mov.hora as 'Time' from pub.mov_creditos mov left join pub.operacion op on op.cod_operacion = mov.cod_operacion " +
                "where op.tipo_operacion = 2) t where t.nit = '{0}' and t.NumeroCredito = {1} order by t.fecha, t.hora";
        const string ahorrosQuery = "SELECT proAh.nom_producto AS 'NombreProducto', ah.cue_ahorros as 'CuentaAhorros', ag.nombre as 'Agencia', ah.Sdo_Disponible as 'SaldoDisponible', ah.Cuota as 'Cuota', ah.Fec_Apertura as 'FechaApertura', ah.Fec_Vencimiento as 'FechaVencimiento' FROM PUB.ahorros ah " +
                "INNER JOIN PUB.Pro_Ahorros proAh ON ah.Cod_ahorro = proAh.Cod_ahorro " +
                "INNER JOIN PUB.Agencias ag ON ag.Agencia = ah.agencia " +
                "WHERE ah.nit = '{0}' AND ah.Estado = 1";
        const string creditosQuery = "SELECT proCr.nom_producto AS 'NombreProducto', cr.num_credito as 'NumeroCredito', ag.nombre as 'Agencia', cr.Cuota as 'Cuota', cr.sdo_Capital as 'SaldoCapital', " +
                "CASE cr.Cod_Credito " +
                    "WHEN 123 THEN cr.Monto - cr.Sdo_Capital " +
                    "ELSE 0 " +
                "END AS 'Disponible', " +
                "cr.Plazo - cr.Cuo_Pagadas AS 'CuotasPorPagar', " +
                "cr.Cuo_Atraso AS 'CuotasAtrasadas', " +
                "cr.Dias_Atraso AS 'DiasAtraso', " +
                "CASE cr.val_atraso " +
                    "WHEN 0 THEN 0 " +
                    "ELSE cp.valor_atrasado + fact.valor_atrasado " +
                "END AS 'ValorAtraso', " +
                "cr.Fec_Pago AS 'FechaProximoPago', " +
                    "CASE cr.For_Pago " +
                        "WHEN 1 THEN 'Caja' " +
                        "WHEN 2 THEN 'Nómina' " +
                        "ELSE 'Caja' " +
                    "END AS 'FormaPago' " +
                "FROM PUB.creditos cr " +
                "INNER JOIN PUB.pro_creditos proCr ON cr.Cod_credito = proCr.cod_credito " +
                "INNER JOIN PUB.Agencias ag ON ag.Agencia = cr.agencia " +
                "LEFT JOIN (SELECT nit, Num_Credito, sum(Cuota - Cap_pagado) AS valor_atrasado FROM PUB.control_pagos WHERE Id_PdoMes < 2 AND Fec_Vcto < SYSDATE() GROUP BY nit, num_credito) cp ON cp.Nit = cr.Nit AND cp.Num_Credito = cr.Num_Credito " +
                "LEFT JOIN (SELECT nit, Num_Credito, sum(cuota - pago_capital - pago_intCorriente - pago_intDifCobro - pago_mora) AS valor_atrasado FROM PUB.facturacion WHERE estado = 1 AND Fec_pago < SYSDATE() GROUP BY nit, num_credito) " +
                "fact ON fact.Nit = cr.Nit AND fact.Num_Credito = cr.Num_Credito " +
                "WHERE cr.nit = '{0}'AND cr.Estado = 2";
        const string asociadoQuery = "select NIT as 'Cedula', Fec_expedicion as 'FechaExpedicionCedula', cl.Nombre + ' ' + cl.Apellido1 + ' ' + cl.Apellido2 as 'NombreCompleto'," +
                "ag.Nombre as 'Sede', cl.Email, cl.Celular as 'NumeroCelular' from pub.clientes cl left join pub.agencias ag on ag.agencia = cl.agencia " +
                "Where NIT  = '{0}'";
    
        public Response GetAsociado(string nit)
        {
            try
            {
                DataTable dt = new DataTable();
                int rows;
                String conString = Configuration.GetConnectionString("ODBCString");
                using (OdbcConnection connection = new OdbcConnection(conString))
                using (OdbcCommand command = connection.CreateCommand())
                using (OdbcDataAdapter adapter = new OdbcDataAdapter(command))
                {
                    connection.Open();
                    command.CommandText = String.Format(asociadoQuery, nit);
                    rows = adapter.Fill(dt);
                }
                string JSONString = string.Empty;
                JSONString = JsonConvert.SerializeObject(dt);
                if (rows == 0) {
                    return new Response
                    {
                        statusCode = 1,
                        response = "La cédula especificada no existe"
                    };
                }
                return new Response
                {
                    statusCode = 0,
                    response = JSONString
                };
            }
            catch (System.Exception ex)
            {
                return new Response
                {
                    statusCode = 1,
                    response = $"{ex.Message} - {ex.StackTrace}"
                };
            }
        }

        public Response GetData(string tipo, string cuenta, string nit)
        {
            try
            {
                DataTable dt = new DataTable();
                int rows;
                String conString = Configuration.GetConnectionString("ODBCString");
                using (OdbcConnection connection = new OdbcConnection(conString))
                using (OdbcCommand command = connection.CreateCommand())
                using (OdbcDataAdapter adapter = new OdbcDataAdapter(command))
                {
                    connection.Open();
                    if (tipo.ToLower().Equals("ahorro"))
                    {
                        command.CommandText = String.Format(ahorroQuery, nit, cuenta);
                    }
                    if (tipo.ToLower().Equals("credito"))
                    {
                        command.CommandText = String.Format(creditoQuery, nit, cuenta);
                    }
                    rows = adapter.Fill(dt);
                }
                string JSONString = string.Empty;
                JSONString = JsonConvert.SerializeObject(dt);
                return new Response
                {
                    statusCode = 0,
                    response = JSONString
                };
            }
            catch (System.Exception ex)
            {
                return new Response
                {
                    statusCode = 1,
                    response = $"{ex.Message} - {ex.StackTrace}"
                };
            }
        }

        public Response GetCuentas(string tipo, string nit)
        {
            try
            {
                DataTable dt = new DataTable();
                int rows;
                String conString = Configuration.GetConnectionString("ODBCString");
                using (OdbcConnection connection = new OdbcConnection(conString))
                using (OdbcCommand command = connection.CreateCommand())
                using (OdbcDataAdapter adapter = new OdbcDataAdapter(command))
                {
                    connection.Open();
                    if (tipo.ToLower().Equals("ahorro"))
                    {
                        command.CommandText = String.Format(ahorrosQuery, nit);
                    }
                    if (tipo.ToLower().Equals("credito"))
                    {
                        command.CommandText = String.Format(creditosQuery, nit);
                    }
                    rows = adapter.Fill(dt);
                }
                string JSONString = string.Empty;
                JSONString = JsonConvert.SerializeObject(dt);
                return new Response
                {
                    statusCode = 0,
                    response = JSONString
                };
            }
            catch (System.Exception ex)
            {
                return new Response
                {
                    statusCode = 1,
                    response = $"{ex.Message} - {ex.StackTrace}"
                };
            }
        }
    }
}