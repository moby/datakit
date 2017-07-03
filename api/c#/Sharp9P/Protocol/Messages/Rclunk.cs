namespace Sharp9P.Protocol.Messages
{
    public sealed class Rclunk : Message
    {
        public Rclunk()
        {
            Type = (byte) MessageType.Rclunk;
        }

        public Rclunk(byte[] bytes) : base(bytes)
        {
        }
    }
}