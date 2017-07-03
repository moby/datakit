using System;
using System.Runtime.Serialization;

namespace Sharp9P.Exceptions
{
    public class InsufficientDataException : Exception
    {
        public InsufficientDataException()
        {
        }

        public InsufficientDataException(string message) : base(message)
        {
        }

        public InsufficientDataException(string message, Exception innerException) : base(message, innerException)
        {
        }

        public InsufficientDataException(uint length, int offset) : base(SetMessage(length, offset))
        {
        }

        protected InsufficientDataException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }

        private static string SetMessage(uint length, int offset)
        {
            return $"Insufficient Data to fill buffer. Buffer length: {length}, Offset: {offset}";
        }
    }
}