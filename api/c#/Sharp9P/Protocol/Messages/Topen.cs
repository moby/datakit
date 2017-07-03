using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Topen : Message
    {
        public Topen(uint fid, byte mode)
        {
            Type = (byte) MessageType.Topen;
            Fid = fid;
            Mode = mode;
            Length += Constants.Bit32Sz + Constants.Bit8Sz;
        }

        public Topen(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Fid = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Mode = bytes[offset];
            offset += Constants.Bit8Sz;
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public uint Fid { get; set; }
        public byte Mode { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);

            offset += Protocol.WriteUint(bytes, Fid, offset);
            bytes[offset] = Mode;
            offset += Constants.Bit8Sz;

            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Topen other)
        {
            return base.Equals(other) && Fid == other.Fid && Mode == other.Mode;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj.GetType() == GetType() && Equals((Topen) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = base.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Fid;
                hashCode = (hashCode*397) ^ Mode.GetHashCode();
                return hashCode;
            }
        }
    }
}