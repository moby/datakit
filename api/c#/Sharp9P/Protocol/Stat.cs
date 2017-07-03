using System;

namespace Sharp9P.Protocol
{
    // http://man.cat-v.org/plan_9/5/stat
    public class Stat
    {
        public Stat(
            ushort type,
            uint dev,
            Qid qid,
            uint mode,
            uint atime,
            uint mtime,
            ulong length,
            string name,
            string uid,
            string gid,
            string muid)
        {
            Type = type;
            Dev = dev;
            Qid = qid;
            Mode = mode;
            Atime = atime;
            Mtime = mtime;
            Length = length;
            Name = name;
            Uid = uid;
            Gid = gid;
            Muid = muid;
            Size = (ushort) (Constants.Bit16Sz + Constants.Bit16Sz +
                             Constants.Bit32Sz + Constants.Qidsz +
                             Constants.Bit32Sz + Constants.Bit32Sz +
                             Constants.Bit32Sz + Constants.Bit64Sz +
                             Protocol.GetStringLength(Name) +
                             Protocol.GetStringLength(Uid) +
                             Protocol.GetStringLength(Gid) +
                             Protocol.GetStringLength(Muid));
        }

        public Stat(byte[] bytes)
        {
            var offset = 0;
            Size = Protocol.ReadUShort(bytes, offset);
            offset += Constants.Bit16Sz;
            Type = Protocol.ReadUShort(bytes, offset);
            offset += Constants.Bit16Sz;
            Dev = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Qid = Protocol.ReadQid(bytes, offset);
            offset += Constants.Qidsz;
            Mode = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Atime = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Mtime = Protocol.ReadUInt(bytes, offset);
            offset += Constants.Bit32Sz;
            Length = Protocol.ReadULong(bytes, offset);
            offset += Constants.Bit64Sz;
            Name = Protocol.ReadString(bytes, offset);
            offset += (int) Protocol.GetStringLength(Name);
            Uid = Protocol.ReadString(bytes, offset);
            offset += (int) Protocol.GetStringLength(Uid);
            Gid = Protocol.ReadString(bytes, offset);
            offset += (int) Protocol.GetStringLength(Gid);
            Muid = Protocol.ReadString(bytes, offset);
            offset += (int) Protocol.GetStringLength(Muid);
            if (offset < Size)
            {
                throw new Exception("Too much data");
            }
        }

        public ushort Size { get; set; }
        public ushort Type { get; set; }
        public uint Dev { get; set; }
        public Qid Qid { get; set; }
        public uint Mode { get; set; }
        public uint Atime { get; set; }
        public uint Mtime { get; set; }
        public ulong Length { get; set; }
        public string Name { get; set; }
        public string Uid { get; set; }
        public string Gid { get; set; }
        public string Muid { get; set; }

        public byte[] ToBytes()
        {
            var bytes = new byte[Size];
            var offset = 0;

            offset += Protocol.WriteUshort(bytes, Size, offset);
            offset += Protocol.WriteUshort(bytes, Type, offset);
            offset += Protocol.WriteUint(bytes, Dev, offset);
            offset += Protocol.WriteQid(bytes, Qid, offset);
            offset += Protocol.WriteUint(bytes, Mode, offset);
            offset += Protocol.WriteUint(bytes, Atime, offset);
            offset += Protocol.WriteUint(bytes, Mtime, offset);
            offset += Protocol.WriteUlong(bytes, Length, offset);
            offset += Protocol.WriteString(bytes, Name, offset);
            offset += Protocol.WriteString(bytes, Uid, offset);
            offset += Protocol.WriteString(bytes, Gid, offset);
            offset += Protocol.WriteString(bytes, Muid, offset);

            if (offset < Size)
            {
                throw new Exception($"Buffer underflow. Len: {Size}, Offset: {offset}");
            }
            return bytes;
        }

        protected bool Equals(Stat other)
        {
            return Size == other.Size && Type == other.Type && Dev == other.Dev && Equals(Qid, other.Qid) &&
                   Mode == other.Mode && Atime == other.Atime && Mtime == other.Mtime && Length == other.Length &&
                   string.Equals(Name, other.Name) && string.Equals(Uid, other.Uid) && string.Equals(Gid, other.Gid) &&
                   string.Equals(Muid, other.Muid);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != GetType()) return false;
            return Equals((Stat) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = Size.GetHashCode();
                hashCode = (hashCode*397) ^ Type.GetHashCode();
                hashCode = (hashCode*397) ^ (int) Dev;
                hashCode = (hashCode*397) ^ (Qid?.GetHashCode() ?? 0);
                hashCode = (hashCode*397) ^ (int) Mode;
                hashCode = (hashCode*397) ^ (int) Atime;
                hashCode = (hashCode*397) ^ (int) Mtime;
                hashCode = (hashCode*397) ^ Length.GetHashCode();
                hashCode = (hashCode*397) ^ (Name?.GetHashCode() ?? 0);
                hashCode = (hashCode*397) ^ (Uid?.GetHashCode() ?? 0);
                hashCode = (hashCode*397) ^ (Gid?.GetHashCode() ?? 0);
                hashCode = (hashCode*397) ^ (Muid?.GetHashCode() ?? 0);
                return hashCode;
            }
        }
    }
}