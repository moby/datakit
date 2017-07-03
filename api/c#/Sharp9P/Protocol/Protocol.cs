using System;
using System.IO;
using System.Text;
using Sharp9P.Protocol.Messages;

namespace Sharp9P.Protocol
{
    public interface IProtocol
    {
        Message Read();
        void Write(Message message);
        uint Msize { get; set; }
    }

    public class Protocol : IProtocol
    {
        public uint Msize { get; set; } = Constants.DefaultMsize;
        private readonly Stream _stream;

        public Protocol(Stream stream)
        {
            _stream = stream;
        }

        private byte[] ReadBytes(int n)
        {
            var data = new byte[n];
            var r = _stream.Read(data, 0, n);
            if (r < n)
            {
                throw new Exception("Failed to read enough bytes");
            }
            return data;
        }

        internal static uint ReadUInt(byte[] data, int offset)
        {
            return BitConverter.ToUInt32(data, offset);
        }

        internal static ulong ReadULong(byte[] data, int offset)
        {
            return BitConverter.ToUInt64(data, offset);
        }

        internal static ushort ReadUShort(byte[] data, int offset)
        {
            return BitConverter.ToUInt16(data, offset);
        }

        internal static string ReadString(byte[] data, int offset)
        {
            var utf8 = new UTF8Encoding();
            var len = ReadUShort(data, offset);
            offset += Constants.Bit16Sz;
            var strdata = new char[utf8.GetCharCount(data, offset, len)];
            utf8.GetChars(data, offset, len, strdata, 0);
            return new string(strdata);
        }

        internal static Qid ReadQid(byte[] bytes, int offset)
        {
            var b = new byte[Constants.Qidsz];
            Array.Copy(bytes, offset, b, 0, Constants.Qidsz);
            return new Qid(b);
        }

        internal static Stat ReadStat(byte[] bytes, int offset)
        {
            var length = ReadUShort(bytes, offset);
            var b = new byte[length];
            Array.Copy(bytes, offset, b, 0, length);
            return new Stat(b);
        }

        private byte[] ReadMessage()
        {
            // Read length uint
            var length = ReadBytes(Constants.Bit32Sz);
            var pktlen = ReadUInt(length, 0);
            if (pktlen - Constants.Bit32Sz > Msize)
                throw new Exception("Message too large!");

            // Read the remainder of the packet (minus the uint length)
            var data = ReadBytes((int) pktlen - Constants.Bit32Sz);

            var pkt = new byte[pktlen];
            length.CopyTo(pkt, 0);
            data.CopyTo(pkt, Constants.Bit32Sz);
            return pkt;
        }

        public Message Read()
        {
            Message message;
            var bytes = ReadMessage();
            var offset = Constants.Bit32Sz;
            var type = bytes[offset];
            switch (type)
            {
                case (byte) MessageType.Tversion:
                    message = new Tversion(bytes);
                    break;
                case (byte) MessageType.Rversion:
                    message = new Rversion(bytes);
                    break;
                case (byte) MessageType.Tauth:
                    message = new Tauth(bytes);
                    break;
                case (byte) MessageType.Rauth:
                    message = new Rauth(bytes);
                    break;
                case (byte) MessageType.Tattach:
                    message = new Tattach(bytes);
                    break;
                case (byte) MessageType.Rattach:
                    message = new Rattach(bytes);
                    break;
                case (byte) MessageType.Rerror:
                    message = new Rerror(bytes);
                    break;
                case (byte) MessageType.Tflush:
                    message = new Tflush(bytes);
                    break;
                case (byte) MessageType.Rflush:
                    message = new Rflush(bytes);
                    break;
                case (byte) MessageType.Twalk:
                    message = new Twalk(bytes);
                    break;
                case (byte) MessageType.Rwalk:
                    message = new Rwalk(bytes);
                    break;
                case (byte) MessageType.Topen:
                    message = new Topen(bytes);
                    break;
                case (byte) MessageType.Ropen:
                    message = new Ropen(bytes);
                    break;
                case (byte) MessageType.Tcreate:
                    message = new Tcreate(bytes);
                    break;
                case (byte) MessageType.Rcreate:
                    message = new Rcreate(bytes);
                    break;
                case (byte) MessageType.Tread:
                    message = new Tread(bytes);
                    break;
                case (byte) MessageType.Rread:
                    message = new Rread(bytes);
                    break;
                case (byte) MessageType.Twrite:
                    message = new Twrite(bytes);
                    break;
                case (byte) MessageType.Rwrite:
                    message = new Rwrite(bytes);
                    break;
                case (byte) MessageType.Tclunk:
                    message = new Tclunk(bytes);
                    break;
                case (byte) MessageType.Rclunk:
                    message = new Rclunk(bytes);
                    break;
                case (byte) MessageType.Tremove:
                    message = new Tremove(bytes);
                    break;
                case (byte) MessageType.Rremove:
                    message = new Rremove(bytes);
                    break;
                case (byte) MessageType.Tstat:
                    message = new Tstat(bytes);
                    break;
                case (byte) MessageType.Rstat:
                    message = new Rstat(bytes);
                    break;
                case (byte) MessageType.Twstat:
                    message = new Twstat(bytes);
                    break;
                case (byte) MessageType.Rwstat:
                    message = new Rwstat(bytes);
                    break;
                default:
                    throw new Exception("Unsupported Message Type");
            }
            return message;
        }

        internal static int WriteUlong(byte[] data, ulong var, int offset)
        {
            var bytes = BitConverter.GetBytes(var);
            Array.Copy(bytes, 0, data, offset, bytes.Length);
            return Constants.Bit64Sz;
        }

        internal static int WriteUint(byte[] data, uint var, int offset)
        {
            var bytes = BitConverter.GetBytes(var);
            Array.Copy(bytes, 0, data, offset, bytes.Length);
            return Constants.Bit32Sz;
        }

        internal static int WriteUshort(byte[] data, ushort var, int offset)
        {
            var bytes = BitConverter.GetBytes(var);
            Array.Copy(bytes, 0, data, offset, bytes.Length);
            return Constants.Bit16Sz;
        }

        internal static int WriteString(byte[] data, string var, int offset)
        {
            var utf8 = new UTF8Encoding();

            WriteUshort(data, (ushort) var.Length, offset);
            offset += Constants.Bit16Sz;

            var bytes = utf8.GetBytes(var);
            Array.Copy(bytes, 0, data, offset, utf8.GetByteCount(var));
            return Constants.Bit16Sz + utf8.GetByteCount(var);
        }

        internal static int WriteQid(byte[] data, Qid qid, int offset)
        {
            var bytes = qid.ToBytes();
            Array.Copy(bytes, 0, data, offset, Constants.Qidsz);
            return Constants.Qidsz;
        }

        internal static int WriteStat(byte[] data, Stat stat, int offset)
        {
            var bytes = stat.ToBytes();
            Array.Copy(bytes, 0, data, offset, stat.Size);
            return stat.Size;
        }

        internal static uint GetStringLength(string var)
        {
            var utf8 = new UTF8Encoding();
            return (uint) (Constants.Bit16Sz + utf8.GetByteCount(var));
        }

        public void Write(Message message)
        {
            var bytes = message.ToBytes();
            _stream.Write(bytes, 0, bytes.Length);
            _stream.Flush();
        }
    }
}