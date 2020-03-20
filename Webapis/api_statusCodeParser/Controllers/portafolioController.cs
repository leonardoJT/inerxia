using System;
using System.Linq;
using System.Web.Http;
using CreditosAPI.Model;
using CreditosAPI.Domain;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Http;
using System.Collections.Generic;
using Microsoft.Extensions.Configuration;

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
            return statusCodeParser(con.GetData(tipo, cuenta, nit));
        }

        [HttpGet("{tipo}/{nit}")]
        public ActionResult<Response> Get(string tipo, string nit)
        {
            ODBCConecction con = new ODBCConecction();
            con.Configuration = Configuration;
            return statusCodeParser(con.GetCuentas(tipo, nit));
        }

        [HttpGet("/asociado/{nit}")]
        public ActionResult<Response> Get(string nit)
        {
            ODBCConecction con = new ODBCConecction();
            con.Configuration = Configuration;
            return statusCodeParser(con.GetAsociado(nit));
        }

        private ActionResult statusCodeParser(Response response){
            // 200-ok
            if(response.statusCode == 0){ return Ok(response.response); } 
            // 400-cuando no se encontraron registros
            else if(response.statusCode == 1){ return NotFound(response.response); } 
            // 500-cuando se generó una excepción posiblemente por drivers 
            else{ return BadRequest(response.response); } 
        }
    }
}
