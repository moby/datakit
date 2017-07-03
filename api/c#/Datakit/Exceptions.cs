using System;

namespace Datakit
{
    public class NoHeadException : Exception
    {
        public NoHeadException(string message) : base(message)
        {
        }
    }
}
