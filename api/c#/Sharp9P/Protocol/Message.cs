using System;

namespace Sharp9P.Protocol
{
    public abstract class Message
    {
        protected Message(byte[] bytes)
        {
            var offset = 0;
            Length = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Type = bytes[offset];
            offset += Constants.Bit8Sz;
            Tag = Protocol.ReadUShort(bytes, offset);
        }

        protected Message()
        {
            Length = Constants.Bit32Sz + Constants.Bit8Sz + Constants.Bit16Sz;
        }

        public uint Length { get; protected set; }
        public byte Type { get; protected set; }
        public ushort Tag { get; set; }

        public virtual byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);

            bytes[offset] = Type;
            offset += Constants.Bit8Sz;

            offset += Protocol.WriteUshort(bytes, Tag, offset);

            if (offset < Length)
            {
                throw new Exception($"Buffer underflow. Len: {Length}, Offset: {offset}");
            }
            return bytes;
        }

        protected bool Equals(Message other)
        {
            return Length == other.Length && Type == other.Type && Tag == other.Tag;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj.GetType() == GetType() && Equals((Message) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = (int) Length;
                hashCode = (hashCode*397) ^ Type.GetHashCode();
                hashCode = (hashCode*397) ^ Tag.GetHashCode();
                return hashCode;
            }
        }
    }
}