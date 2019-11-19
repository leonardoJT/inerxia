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

        const string ahorroQuery = "select * from(select mov.fecha as 'Fecha',right('0' + cast(cast(mov.hora / 3600 as integer) as varchar(2)), 2) + ':' + right('0' + cast(cast(mod(mov.hora, 3600) / 60 as integer) as varchar(2)), 2) as 'Hora', mov.descrip as 'Descripción'," +
                "'Consignación' as 'Tipo de operación', mov.val_efectivo + mov.val_cheque as 'Valor', mov.nit, mov.cue_ahorros, mov.hora as 'time' from pub.mov_ahorros mov left join pub.operacion op on op.cod_operacion = mov.cod_operacion where op.tipo_operacion = 1 " +
                "union select mov.fecha as 'Fecha', right('0' + cast(cast(mov.hora / 3600 as integer) as varchar(2)), 2) + ':' + right('0' + cast(cast(mod(mov.hora, 3600) / 60 as integer) as varchar(2)), 2) as 'Hora', mov.descrip as 'Descripción', 'Retiro' as 'Tipo de operación', " +
                "mov.val_efectivo + mov.val_cheque as 'Valor', mov.nit, mov.cue_ahorros, mov.hora as 'time' from pub.mov_ahorros mov left join pub.operacion op on op.cod_operacion = mov.cod_operacion where op.tipo_operacion = 2) t where t.nit = '{0}' " +
                "and t.cue_ahorros = '{1}' order by t.fecha, t.hora";
        const string creditoQuery = "select* from(select mov.fecha as 'Fecha',right('0' + cast(cast(mov.hora / 3600 as integer) as varchar(2)), 2) + ':' + right('0' + cast(cast(mod(mov.hora, 3600) / 60 as integer) as varchar(2)), 2) as 'Hora', mov.descrip as 'Descripción', " +
                "'Abono / Pago' as 'Tipo de operación', mov.val_efectivo + mov.val_cheque as 'Valor',mov.sdo_capital as 'Saldo de capital',mov.nit,mov.num_credito,mov.hora as 'time' from pub.mov_creditos mov left join pub.operacion op on op.cod_operacion = mov.cod_operacion " +
                "where op.tipo_operacion = 1 union select mov.fecha as 'Fecha',right('0' + cast(cast(mov.hora / 3600 as integer) as varchar(2)), 2) + ':' + right('0' + cast(cast(mod(mov.hora, 3600) / 60 as integer) as varchar(2)), 2) as 'Hora',mov.descrip as 'Descripción', " +
                "'Avance / Desembolso' as 'Tipo de operación',mov.val_efectivo + mov.val_cheque as 'Valor',mov.sdo_capital as 'Saldo de capital',mov.nit,mov.num_credito,mov.hora as 'time' from pub.mov_creditos mov left join pub.operacion op on op.cod_operacion = mov.cod_operacion " +
                "where op.tipo_operacion = 2) t where t.nit = '{0}' and t.num_credito = {1} order by t.fecha, t.hora";
        const string ahorrosQuery = "SELECT proAh.nom_producto, ah.cue_ahorros, ag.nombre, ah.Sdo_Disponible, ah.Cuota, ah.Fec_Apertura, ah.Fec_Vencimiento FROM PUB.ahorros ah " +
                "INNER JOIN PUB.Pro_Ahorros proAh ON ah.Cod_ahorro = proAh.Cod_ahorro " +
                "INNER JOIN PUB.Agencias ag ON ag.Agencia = ah.agencia " +
                "WHERE ah.nit = '{0}' AND ah.Estado = 1";
        const string creditosQuery = "SELECT proCr.nom_producto, cr.num_credito, ag.nombre, cr.Cuota, cr.sdo_Capital, " +
                "CASE cr.Cod_Credito " +
                    "WHEN 123 THEN cr.Monto - cr.Sdo_Capital " +
                    "ELSE 0 " +
                "END AS 'disponible', " +
                "cr.Plazo - cr.Cuo_Pagadas AS 'cuotas por pagar', " +
                "cr.Cuo_Atraso AS 'cuotas atrasadas', " +
                "cr.Dias_Atraso, " +
                "CASE cr.val_atraso " +
                    "WHEN 0 THEN 0 " +
                    "ELSE cp.valor_atrasado + fact.valor_atrasado " +
                "END AS 'val_atraso', " +
                "cr.Fec_Pago AS 'Fecha próximo pago', " +
                    "CASE cr.For_Pago " +
                        "WHEN 1 THEN 'Caja' " +
                        "WHEN 2 THEN 'Nómina' " +
                        "ELSE 'Caja' " +
                    "END AS 'Forma de pago' " +
                "FROM PUB.creditos cr " +
                "INNER JOIN PUB.pro_creditos proCr ON cr.Cod_credito = proCr.cod_credito " +
                "INNER JOIN PUB.Agencias ag ON ag.Agencia = cr.agencia " +
                "LEFT JOIN (SELECT nit, Num_Credito, sum(Cuota - Cap_pagado) AS valor_atrasado FROM PUB.control_pagos WHERE Id_PdoMes < 2 AND Fec_Vcto < SYSDATE() GROUP BY nit, num_credito) cp ON cp.Nit = cr.Nit AND cp.Num_Credito = cr.Num_Credito " +
                "LEFT JOIN (SELECT nit, Num_Credito, sum(cuota - pago_capital - pago_intCorriente - pago_intDifCobro - pago_mora) AS valor_atrasado FROM PUB.facturacion WHERE estado = 1 AND Fec_pago < SYSDATE() GROUP BY nit, num_credito) " +
                "fact ON fact.Nit = cr.Nit AND fact.Num_Credito = cr.Num_Credito " +
                "WHERE cr.nit = '{0}'AND cr.Estado = 2";
    
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