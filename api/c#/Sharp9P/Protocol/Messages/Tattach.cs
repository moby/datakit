using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Tattach : Message
    {
        public Tattach(uint fid, uint afid, string uname, string aname)
        {
            Type = (byte) MessageType.Tattach;
            Fid = fid;
            Afid = afid;
            Uname = uname;
            Aname = aname;
            Length += Constants.Bit32Sz + Constants.Bit32Sz +
                      Protocol.GetStringLength(uname) +
                      Protocol.GetStringLength(aname);
        }

        public Tattach(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Fid = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Afid = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Uname = Protocol.ReadString(bytes, offset);
            offset += (int) Protocol.GetStringLength(Uname);
            Aname = Protocol.ReadString(bytes, offset);
            offset += (int) Protocol.GetStringLength(Aname);
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public uint Fid { get; set; }
        public uint Afid { get; set; }
        public string Uname { get; set; }
        public string Aname { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);

            bytes[offset] = Type;
            offset += Constants.Bit8Sz;

            offset += Protocol.WriteUshort(bytes, Tag, offset);

            offset += Protocol.WriteUint(bytes, Fid, offset);
            offset += Protocol.WriteUint(bytes, Afid, offset);
            offset += Protocol.WriteString(bytes, Uname, offset);
            offset += Protocol.WriteString(bytes, Aname, offset);

            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Tattach other)
        {
            return Fid == other.Fid && Afid == other.Afid && string.Equals(Uname, other.Uname) &&
                   string.Equals(Aname, other.Aname);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj.GetType() == GetType() && Equals((Tattach) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = (int) Fid;
                hashCode = (hashCode*397) ^ (int) Afid;
                hashCode = (hashCode*397) ^ (Uname?.GetHashCode() ?? 0);
                hashCode = (hashCode*397) ^ (Aname?.GetHashCode() ?? 0);
                return hashCode;
            }
        }
    }
}