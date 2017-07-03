namespace Sharp9P.Protocol.Messages
{
    public sealed class Rflush : Message
    {
        public Rflush()
        {
            Type = (byte) MessageType.Rflush;
        }

        public Rflush(byte[] bytes) : base(bytes)
        {
        }
    }
}