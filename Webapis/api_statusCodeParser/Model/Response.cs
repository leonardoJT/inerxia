namespace CreditosAPI.Model
{
    public class Response
    {
        public int statusCode { get; set; }
        public string response { get; set; }

        public Response(int _statusCode,string _response)
        {
            statusCode=_statusCode;
            response=_response;
        }
    }
}