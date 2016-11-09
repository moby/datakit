using System;
using System.Runtime.Serialization;

namespace Sharp9P.Exceptions
{
    public class ServerErrorException : Exception
    {
        public ServerErrorException()
        {
        }

        public ServerErrorException(string message) : base(SetMessage(message))
        {
        }

        public ServerErrorException(string message, Exception innerException)
            : base(SetMessage(message), innerException)
        {
        }

        protected ServerErrorException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }

        private static string SetMessage(string message)
        {
            return $"Received Error Message from Server: {message}";
        }
    }
}