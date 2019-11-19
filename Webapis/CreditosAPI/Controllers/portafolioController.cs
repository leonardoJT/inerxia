using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using CreditosAPI.Domain;
using Microsoft.Extensions.Configuration;
using CreditosAPI.Model;

namespace CreditosAPI.Controllers
{
    [Route("[controller]")]
    [ApiController]
    public class portafolioController : ControllerBase
    {

        public IConfiguration Configuration { get; }
        public portafolioController(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        [HttpGet("{tipo}/{nit}/{cuenta}")]
        public ActionResult<Response> Get(string tipo, string nit, string cuenta)
        {
            ODBCConecction con = new ODBCConecction();
            con.Configuration = Configuration;
            return con.GetData(tipo, cuenta, nit);
        }

        [HttpGet("{tipo}/{nit}")]
        public ActionResult<Response> Get(string tipo, string nit)
        {
            ODBCConecction con = new ODBCConecction();
            con.Configuration = Configuration;
            return con.GetCuentas(tipo, nit);
        }
    }
}
