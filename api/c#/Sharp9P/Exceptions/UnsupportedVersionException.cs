using System;
using System.Runtime.Serialization;

namespace Sharp9P.Exceptions
{
    public class UnsupportedVersionException : Exception
    {
        public UnsupportedVersionException()
        {
        }

        public UnsupportedVersionException(string message) : base(SetMessage(message))
        {
        }

        public UnsupportedVersionException(string message, Exception innerException)
            : base(SetMessage(message), innerException)
        {
        }

        protected UnsupportedVersionException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }

        private static string SetMessage(string version)
        {
            return $"Version {version} is not supported";
        }
    }
}