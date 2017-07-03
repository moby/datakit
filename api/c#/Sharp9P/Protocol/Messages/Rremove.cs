namespace Sharp9P.Protocol.Messages
{
    public sealed class Rremove : Message
    {
        public Rremove()
        {
            Type = (byte) MessageType.Rremove;
        }

        public Rremove(byte[] bytes) : base(bytes)
        {
        }
    }
}