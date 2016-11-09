using System;
using System.Collections;
using System.IO;
using Sharp9P.Exceptions;
using Sharp9P.Protocol;
using Sharp9P.Protocol.Messages;

namespace Sharp9P
{
    public class Client
    {
        private readonly Queue _fidQueue;
        private readonly IProtocol _protocol;
        private readonly Queue _tagQueue;
        private uint _msize;
        private string _version;

        public Client(IProtocol protocol)
        {
            _msize = Constants.DefaultMsize;
            _version = Constants.DefaultVersion;
            _protocol = protocol;
            _tagQueue = new Queue();
            for (ushort i = 1; i < 65535; i++)
            {
                _tagQueue.Enqueue(i);
            }
            _fidQueue = new Queue();
            for (uint i = 2; i < 100; i++)
            {
                _fidQueue.Enqueue(i);
            }
        }

        public static Client FromStream (Stream stream)
        {
            var p = new Protocol.Protocol(stream);
            return new Client(p);
        }

        public uint AllocateFid(uint parent)
        {
            var fid = (uint) _fidQueue.Dequeue();
            Walk(parent, fid, new string[0]);
            return fid;
        }

        public void FreeFid(uint fid)
        {
            Clunk(fid);
        }

        public void Version(uint msize, string version)
        {
            var request = new Tversion(_msize, _version);
            _protocol.Write(request);

            var r = _protocol.Read();
            Rversion response;
            try
            {
                response = (Rversion) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            /* The server responds with its own maxi-
            mum, msize, which must be less than or equal to the client's
            value */
            if (response.Msize > request.Msize)
            {
                throw new MsizeNegotiationException(request.Msize, response.Msize);
            }
            _msize = response.Msize;
            _protocol.Msize = _msize;
            if (response.Version != request.Version)
            {
                throw new UnsupportedVersionException(response.Version);
            }
            _version = response.Version;
        }

        public Qid Attach(uint fid, uint afid, string uname, string aname)
        {
            var request = new Tattach(fid, afid, uname, aname)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Rattach response;
            try
            {
                response = (Rattach) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _tagQueue.Enqueue(request.Tag);
            return response.Qid;
        }

        public Qid Auth(uint fid, string uname, string aname)
        {
            var request = new Tauth(fid, uname, aname)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Rauth response;
            try
            {
                response = (Rauth) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _tagQueue.Enqueue(request.Tag);
            return response.Aqid;
        }

        public Qid[] Walk(uint fid, uint newFid, string[] nwnames)
        {
            if (nwnames.Length > Constants.Maxwelem)
            {
                throw new Exception("No more thatn 16 elements allowed");
            }
            var request = new Twalk(fid, newFid, (ushort) nwnames.Length, nwnames)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Rwalk response;
            try
            {
                response = (Rwalk) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _tagQueue.Enqueue(request.Tag);
            return response.Wqid;
        }

        public void Clunk(uint fid)
        {
            var request = new Tclunk(fid)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Rclunk response;
            try
            {
                response = (Rclunk) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _fidQueue.Enqueue(fid);
            _tagQueue.Enqueue(request.Tag);
        }

        public Tuple<Qid, uint> Create(uint fid, string name, uint perm, byte mode)
        {
            var request = new Tcreate(fid, name, perm, mode)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Rcreate response;
            try
            {
                response = (Rcreate) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _tagQueue.Enqueue(request.Tag);
            return new Tuple<Qid, uint>(response.Qid, response.Iounit);
        }

        public Tuple<Qid, uint> Open(uint fid, byte mode)
        {
            var request = new Topen(fid, mode)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Ropen response;
            try
            {
                response = (Ropen) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _tagQueue.Enqueue(request.Tag);
            return new Tuple<Qid, uint>(response.Qid, response.Iounit);
        }

        public Tuple<uint, byte[]> Read(uint fid, ulong offset, uint count)
        {
            var request = new Tread(fid, offset, count)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Rread response;
            try
            {
                response = (Rread) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _tagQueue.Enqueue(request.Tag);
            return new Tuple<uint, byte[]>(response.Count, response.Data);
        }

        public uint Write(uint fid, ulong offset, uint count, byte[] data)
        {
            var request = new Twrite(fid, offset, count, data)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Rwrite response;
            try
            {
                response = (Rwrite) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _tagQueue.Enqueue(request.Tag);
            return response.Count;
        }

        public Stat Stat(uint fid)
        {
            var request = new Tstat(fid)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Rstat response;
            try
            {
                response = (Rstat) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _tagQueue.Enqueue(request.Tag);
            return response.Stat;
        }

        public void Wstat(uint fid, Stat stat)
        {
            var request = new Twstat(fid, stat)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Rwstat response;
            try
            {
                response = (Rwstat) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _tagQueue.Enqueue(request.Tag);
        }

        public void Flush(ushort tag)
        {
            var request = new Tflush(tag)
            {
                Tag = (ushort) _tagQueue.Dequeue()
            };
            _protocol.Write(request);
            var r = _protocol.Read();
            if (r.Tag != request.Tag)
                throw new TagMismatchException(r.Tag, request.Tag);
            Rflush response;
            try
            {
                response = (Rflush) r;
            }
            catch (InvalidCastException)
            {
                if (r.Type != (byte) MessageType.Rerror) throw new UnexpectedMessageException(request.Type, r.Type);
                var err = (Rerror) r;
                throw new ServerErrorException(err.Ename);
            }
            _tagQueue.Enqueue(request.Tag);
            _tagQueue.Enqueue(tag);
        }
    }
}