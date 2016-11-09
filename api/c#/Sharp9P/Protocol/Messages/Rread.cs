using System;
using System.Linq;
using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Rread : Message
    {
        public Rread(uint count, byte[] data)
        {
            Type = (byte) MessageType.Rread;
            Count = count;
            Data = data;
            Length += Constants.Bit32Sz + Count;
        }

        public Rread(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Count = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Data = new byte[Count];
            Array.Copy(bytes, offset, Data, 0, Count);
            offset += (int) Count;
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public uint Count { get; set; }
        public byte[] Data { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);

            offset += Protocol.WriteUint(bytes, Count, offset);
            Array.Copy(Data, 0, bytes, offset, Count);
            offset += (int) Count;

            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Rread other)
        {
            return base.Equals(other) && Count == other.Count && Data.SequenceEqual(other.Data);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj.GetType() == GetType() && Equals((Rread) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = base.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Count;
                hashCode = (hashCode*397) ^ (Data?.GetHashCode() ?? 0);
                return hashCode;
            }
        }
    }
}