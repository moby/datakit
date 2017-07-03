using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Rversion : Message
    {
        public Rversion(uint msize, string version)
        {
            Type = (byte) MessageType.Rversion;
            Msize = msize;
            Version = version;
            Length += Constants.Bit32Sz + Protocol.GetStringLength(version);
        }

        public Rversion(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Msize = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Version = Protocol.ReadString(bytes, offset);
            offset += (int) Protocol.GetStringLength(Version);
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public uint Msize { get; set; }
        public string Version { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);

            bytes[offset] = Type;
            offset += Constants.Bit8Sz;

            offset += Protocol.WriteUshort(bytes, Tag, offset);

            offset += Protocol.WriteUint(bytes, Msize, offset);

            offset += Protocol.WriteString(bytes, Version, offset);

            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Rversion other)
        {
            return base.Equals(other) && Msize == other.Msize && string.Equals(Version, other.Version);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj is Rversion && Equals((Rversion) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = base.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Msize;
                hashCode = (hashCode*397) ^ (Version?.GetHashCode() ?? 0);
                return hashCode;
            }
        }
    }
}