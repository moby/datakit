namespace Sharp9P.Protocol.Messages
{
    public sealed class Rwstat : Message
    {
        public Rwstat()
        {
            Type = (byte) MessageType.Rwstat;
        }

        public Rwstat(byte[] bytes) : base(bytes)
        {
        }
    }
}