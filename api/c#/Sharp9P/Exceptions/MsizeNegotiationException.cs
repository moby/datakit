using System;
using System.Runtime.Serialization;

namespace Sharp9P.Exceptions
{
    public class MsizeNegotiationException : Exception
    {
        public MsizeNegotiationException()
        {
        }

        public MsizeNegotiationException(string message) : base(message)
        {
        }

        public MsizeNegotiationException(string message, Exception innerException) : base(message, innerException)
        {
        }

        public MsizeNegotiationException(uint expected, uint received) : base(SetMessage(expected, received))
        {
        }

        protected MsizeNegotiationException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }

        private static string SetMessage(uint expected, uint received)
        {
            return $"Msize negotiation failed. Received {expected}, Expected: < {received}";
        }
    }
}