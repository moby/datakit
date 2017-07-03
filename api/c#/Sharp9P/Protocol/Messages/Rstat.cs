using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Rstat : Message
    {
        public Rstat(Stat stat)
        {
            Type = (byte) MessageType.Rstat;
            Stat = stat;
            Length += Stat.Size;
        }

        public Rstat(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Stat = Protocol.ReadStat(bytes, offset);
            offset += Stat.Size;
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public Stat Stat { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);

            offset += Protocol.WriteStat(bytes, Stat, offset);

            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Rstat other)
        {
            return base.Equals(other) && Equals(Stat, other.Stat);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != GetType()) return false;
            return Equals((Rstat) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (base.GetHashCode()*397) ^ (Stat?.GetHashCode() ?? 0);
            }
        }
    }
}