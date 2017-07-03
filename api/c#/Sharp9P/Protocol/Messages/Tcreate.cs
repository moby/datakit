using System;
using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Tcreate : Message
    {
        public Tcreate(uint fid, string name, uint perm, byte mode)
        {
            Type = (byte) MessageType.Tcreate;
            Fid = fid;
            Name = name;
            Perm = perm;
            Mode = mode;
            Length += Constants.Bit32Sz + Protocol.GetStringLength(Name) + Constants.Bit32Sz + Constants.Bit8Sz;
        }

        public Tcreate(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Fid = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Name = Protocol.ReadString(bytes, offset);
            offset += (int) Protocol.GetStringLength(Name);
            Perm = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Mode = bytes[offset];
            offset += Constants.Bit8Sz;
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public uint Fid { get; set; }

        public string Name { get; set; }
        public uint Perm { get; set; }
        public byte Mode { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);

            offset += Protocol.WriteUint(bytes, Fid, offset);
            offset += Protocol.WriteString(bytes, Name, offset);
            offset += Protocol.WriteUint(bytes, Perm, offset);
            bytes[offset] = Mode;
            offset += Constants.Bit8Sz;

            if (offset < Length)
            {
                throw new Exception($"Buffer underflow. Len: {Length}, Offset: {offset}");
            }
            return bytes;
        }

        private bool Equals(Tcreate other)
        {
            return base.Equals(other) && Fid == other.Fid && Name == other.Name && Perm == other.Perm &&
                   Mode == other.Mode;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj is Tcreate && Equals((Tcreate) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = base.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Fid;
                hashCode = (hashCode*397) ^ Name.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Perm;
                hashCode = (hashCode*397) ^ Mode.GetHashCode();
                return hashCode;
            }
        }
    }
}