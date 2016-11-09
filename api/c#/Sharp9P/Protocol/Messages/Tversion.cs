using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Tversion : Message
    {
        public Tversion(uint msize, string version)
        {
            Type = (byte) MessageType.Tversion;
            Msize = msize;
            Version = version;
            Length += Constants.Bit32Sz + Protocol.GetStringLength(version);
        }

        public Tversion(byte[] bytes) : base(bytes)
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

        private bool Equals(Tversion other)
        {
            return Msize == other.Msize && string.Equals(Version, other.Version);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj is Tversion && Equals((Tversion) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((int) Msize*397) ^ (Version?.GetHashCode() ?? 0);
            }
        }
    }
}