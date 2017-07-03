using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Tstat : Message
    {
        public Tstat(uint fid)
        {
            Type = (byte) MessageType.Tstat;
            Fid = fid;
            Length += Constants.Bit32Sz;
        }

        public Tstat(byte[] bytes) : base(bytes)
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

        private bool Equals(Tstat other)
        {
            return base.Equals(other) && Fid == other.Fid;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj.GetType() == GetType() && Equals((Tstat) obj);
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