using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Tread : Message
    {
        public Tread(uint fid, ulong offset, uint count)
        {
            Type = (byte) MessageType.Tread;
            Fid = fid;
            Offset = offset;
            Count = count;
            Length += Constants.Bit32Sz + Constants.Bit64Sz + Constants.Bit32Sz;
        }

        public Tread(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Fid = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Offset = Protocol.ReadULong(bytes, offset);
            offset += Constants.Bit64Sz;
            Count = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public uint Fid { get; set; }
        public ulong Offset { get; set; }
        public uint Count { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);

            offset += Protocol.WriteUint(bytes, Fid, offset);
            offset += Protocol.WriteUlong(bytes, Offset, offset);
            offset += Protocol.WriteUint(bytes, Count, offset);

            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Tread other)
        {
            return base.Equals(other) && Fid == other.Fid && Offset == other.Offset && Count == other.Count;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj is Tread && Equals((Tread) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = base.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Fid;
                hashCode = (hashCode*397) ^ Offset.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Count;
                return hashCode;
            }
        }
    }
}