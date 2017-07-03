using Sharp9P.Exceptions;

namespace Sharp9P.Protocol.Messages
{
    public sealed class Rwalk : Message
    {
        public Rwalk(ushort nwqid, Qid[] wqid)
        {
            Type = (byte) MessageType.Rwalk;
            Nwqid = nwqid;
            Wqid = wqid;
            Length += Constants.Bit16Sz + Nwqid*(uint) Constants.Qidsz;
        }

        public Rwalk(byte[] bytes) : base(bytes)
        {
            var offset = Constants.HeaderOffset;
            Nwqid = Protocol.ReadUShort(bytes, offset);
            offset += Constants.Bit16Sz;
            Wqid = new Qid[Nwqid];
            for (var i = 0; i < Nwqid; i++)
            {
                Wqid[i] = Protocol.ReadQid(bytes, offset);
                offset += Constants.Qidsz;
            }
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
        }

        public ushort Nwqid { get; set; }
        public Qid[] Wqid { get; set; }

        public override byte[] ToBytes()
        {
            var bytes = new byte[Length];
            var offset = Protocol.WriteUint(bytes, Length, 0);
            bytes[offset] = Type;
            offset += Constants.Bit8Sz;
            offset += Protocol.WriteUshort(bytes, Tag, offset);
            offset += Protocol.WriteUshort(bytes, Nwqid, offset);
            foreach (var qid in Wqid)
            {
                offset += Protocol.WriteQid(bytes, qid, offset);
            }
            if (offset < Length)
            {
                throw new InsufficientDataException(Length, offset);
            }
            return bytes;
        }
    }
}