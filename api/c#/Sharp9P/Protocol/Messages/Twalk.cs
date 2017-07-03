using System.Linq;
using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Twalk : Message
    {
        public Twalk(uint fid, uint newFid, ushort nwname, string[] wname)
        {
            Type = (byte) MessageType.Twalk;
            Fid = fid;
            NewFid = newFid;
            Nwname = nwname;
            Wname = wname;
            Length += Constants.Bit32Sz + Constants.Bit32Sz +
                      Constants.Bit16Sz;
            foreach (var name in wname)
            {
                Length += Protocol.GetStringLength(name);
            }
        }

        public Twalk(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Fid = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            NewFid = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Nwname += Protocol.ReadUShort(bytes, offset);
            offset += Constants.Bit16Sz;
            Wname = new string[Nwname];
            for (var i = 0; i < Nwname; i++)
            {
                Wname[i] = Protocol.ReadString(bytes, offset);
                offset += (int) Protocol.GetStringLength(Wname[i]);
            }
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public uint Fid { get; set; }
        public uint NewFid { get; set; }
        public ushort Nwname { get; set; }
        public string[] Wname { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);
            offset += Protocol.WriteUint(bytes, Fid, offset);
            offset += Protocol.WriteUint(bytes, NewFid, offset);
            offset += Protocol.WriteUshort(bytes, Nwname, offset);
            offset = Wname.Aggregate(offset, (current, name) => current + Protocol.WriteString(bytes, name, current));
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }

        private bool Equals(Twalk other)
        {
            return base.Equals(other) && Fid == other.Fid && NewFid == other.NewFid && Nwname == other.Nwname &&
                   Wname.SequenceEqual(other.Wname);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj is Twalk && Equals((Twalk) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = base.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Fid;
                hashCode = (hashCode*397) ^ (int) NewFid;
                hashCode = (hashCode*397) ^ Nwname.GetHashCode();
                hashCode = (hashCode*397) ^ (Wname?.GetHashCode() ?? 0);
                return hashCode;
            }
        }
    }
}