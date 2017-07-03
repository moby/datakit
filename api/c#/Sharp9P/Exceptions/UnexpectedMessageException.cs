using System;
using System.Runtime.Serialization;

namespace Sharp9P.Exceptions
{
    public class UnexpectedMessageException : Exception
    {
        public UnexpectedMessageException()
        {
        }

        public UnexpectedMessageException(string message) : base(message)
        {
        }

        public UnexpectedMessageException(string message, Exception innerException) : base(message, innerException)
        {
        }

        public UnexpectedMessageException(byte expected, byte received) : base(SetMessage(expected, received))
        {
        }

        protected UnexpectedMessageException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }

        private static string SetMessage(byte expected, byte received)
        {
            return $"Received Unexpected Message from Server. Expected: {expected}, Received: {received}";
        }
    }
}