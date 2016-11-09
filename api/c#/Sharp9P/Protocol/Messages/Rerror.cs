using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Rerror : Message
    {
        public Rerror(string ename)
        {
            Type = (byte) MessageType.Rerror;
            Ename = ename;
            Length += Protocol.GetStringLength(Ename);
        }

        public Rerror(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Ename = Protocol.ReadString(bytes, offset);
            offset += (int) Protocol.GetStringLength(Ename);
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public string Ename { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);
            offset += Protocol.WriteString(bytes, Ename, offset);
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Rerror other)
        {
            return base.Equals(other) && string.Equals(Ename, other.Ename);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj is Rerror && Equals((Rerror) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (base.GetHashCode()*397) ^ (Ename?.GetHashCode() ?? 0);
            }
        }
    }
}