using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Tflush : Message
    {
        public Tflush(ushort oldTag)
        {
            Type = (byte) MessageType.Tflush;
            Oldtag = oldTag;
            Length += Constants.Bit16Sz;
        }

        public Tflush(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Oldtag = Protocol.ReadUShort(bytes, offset);
            offset += Constants.Bit16Sz;
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public ushort Oldtag { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);
            offset += Protocol.WriteUshort(bytes, Oldtag, offset);
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Tflush other)
        {
            return base.Equals(other) && Oldtag == other.Oldtag;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj is Tflush && Equals((Tflush) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (base.GetHashCode()*397) ^ Oldtag.GetHashCode();
            }
        }
    }
}