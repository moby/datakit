using System;
using System.Linq;
using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public class Twrite : Message
    {
        public Twrite(uint fid, ulong offset, uint count, byte[] data)
        {
            Type = (byte) MessageType.Twrite;
            Fid = fid;
            Offset = offset;
            Count = count;
            Data = data;
            Length += Constants.Bit32Sz + Constants.Bit64Sz +
                      Constants.Bit32Sz + Count;
        }

        public Twrite(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Fid = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Offset = Protocol.ReadULong(bytes, offset);
            offset += Constants.Bit64Sz;
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

        public uint Fid { get; set; }
        public ulong Offset { get; set; }
        public uint Count { get; set; }
        public byte[] Data { get; set; }

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
            Array.Copy(Data, 0, bytes, offset, Count);
            offset += (int) Count;

            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        protected bool Equals(Twrite other)
        {
            return base.Equals(other) && Fid == other.Fid && Offset == other.Offset && Count == other.Count &&
                   Data.SequenceEqual(other.Data);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj.GetType() == GetType() && Equals((Twrite) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = base.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Fid;
                hashCode = (hashCode*397) ^ Offset.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Count;
                hashCode = (hashCode*397) ^ (Data?.GetHashCode() ?? 0);
                return hashCode;
            }
        }
    }
}