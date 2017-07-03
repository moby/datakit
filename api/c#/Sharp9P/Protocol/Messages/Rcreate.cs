using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Rcreate : Message
    {
        public Rcreate(Qid qid, uint iounit)
        {
            Type = (byte) MessageType.Rcreate;
            Qid = qid;
            Iounit = iounit;
            Length += Constants.Qidsz + Constants.Bit32Sz;
        }

        public Rcreate(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Qid = Protocol.ReadQid(bytes, offset);
            offset += Constants.Qidsz;
            Iounit = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public Qid Qid { get; set; }
        public uint Iounit { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);

            offset += Protocol.WriteQid(bytes, Qid, offset);
            offset += Protocol.WriteUint(bytes, Iounit, offset);

            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Rcreate other)
        {
            return base.Equals(other) && Equals(Qid, other.Qid) && Iounit == other.Iounit;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj is Rcreate && Equals((Rcreate) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = base.GetHashCode();
                hashCode = (hashCode*397) ^ (Qid?.GetHashCode() ?? 0);
                hashCode = (hashCode*397) ^ (int) Iounit;
                return hashCode;
            }
        }
    }
}