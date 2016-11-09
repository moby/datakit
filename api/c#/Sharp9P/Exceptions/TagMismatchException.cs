using System;
using System.Runtime.Serialization;

namespace Sharp9P.Exceptions
{
    public class TagMismatchException : Exception
    {
        public TagMismatchException()
        {
        }

        public TagMismatchException(string message) : base(message)
        {
        }

        public TagMismatchException(string message, Exception innerException) : base(message, innerException)
        {
        }

        public TagMismatchException(ushort expected, ushort received) : base(SetMessage(expected, received))
        {
        }

        protected TagMismatchException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }

        private static string SetMessage(ushort expectcd, ushort received)
        {
            return $"Tag misatch: Expected response with tag: {expectcd}, Received: {received}";
        }
    }
}