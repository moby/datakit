using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Tremove : Message
    {
        public Tremove(uint fid)
        {
            Type = (byte) MessageType.Tremove;
            Fid = fid;
            Length += Constants.Bit32Sz;
        }

        public Tremove(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Fid = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public uint Fid { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);

            offset += Protocol.WriteUint(bytes, Fid, offset);
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Tremove other)
        {
            return base.Equals(other) && Fid == other.Fid;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj.GetType() == GetType() && Equals((Tremove) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (base.GetHashCode()*397) ^ (int) Fid;
            }
        }
    }
}