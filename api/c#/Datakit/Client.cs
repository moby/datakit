using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Pipes;
using System.Linq;
using System.Security.Principal;
using System.Text;

namespace Datakit
{
    public interface IClient
    {
        void Mkdir(List<string> path);
        uint Open(List<string> path, byte mode);
        void Close(uint fid);
        void Write(List<string> path, string data, bool create);
        string ReadAll(List<string> path);
        Tuple<uint, string> Read(uint fid, ulong offset);
    }

    public class Client : IClient
    {
        private const string PipeLocation = ".";
        private const string PipeName = "dockerdb";
        private const int ConnectTimeout = 1000;
        private const int MaxRetries = 100;
        private const string Anyone = "anyone";
        private const string DatabaseRoot = "/";
        private static Client _instance;
        private readonly Sharp9P.Client _client;

        private Client()
        {
            var stream = new NamedPipeClientStream(PipeLocation, PipeName, PipeDirection.InOut, PipeOptions.None,
                TokenImpersonationLevel.None);

            var retries = 0;
            while (true)
            {
                try
                {
                    stream.Connect(ConnectTimeout);
                    break;
                }
                catch (TimeoutException)
                {
                    retries++;
                    if (retries > MaxRetries)
                    {
                        throw new Exception("Unable to connect to database");
                    }
                }
            }
            _client = Sharp9P.Client.FromStream(stream);
            _client.Version(Sharp9P.Constants.DefaultMsize, Sharp9P.Constants.DefaultVersion);
            _client.Attach(Sharp9P.Constants.RootFid, Sharp9P.Constants.NoFid, Anyone, DatabaseRoot);
        }

        public static Client Instance => _instance ?? (_instance = new Client());

        public void Mkdir(List<string> path)
        {
            var fid = _client.AllocateFid(Sharp9P.Constants.RootFid);
            foreach (var dir in path)
            {
                var dirFid = _client.AllocateFid(fid);
                try
                {
                    _client.Create(dirFid, dir, Constants.DirPerm, Sharp9P.Constants.Oread);
                }
                catch
                {
                    // Ignore File Exists error
                }
                _client.FreeFid(dirFid);
                _client.Walk(fid, fid, new[] {dir});
            }
        }

        public uint Open(List<string> path, byte mode)
        {
            var fid = _client.AllocateFid(Sharp9P.Constants.RootFid);
            _client.Walk(fid, fid, path.ToArray());
            _client.Open(fid, mode);
            return fid;
        }

        public void Close(uint fid)
        {
            _client.FreeFid(fid);
        }

        public void Write(List<string> path, string data, bool create)
        {
            var fid = _client.AllocateFid(Sharp9P.Constants.RootFid);
            var dirs = path.Take(path.Count - 1).ToArray();
            var utf8 = new UTF8Encoding();
            _client.Walk(fid, fid, dirs);
            if (create)
            {
                try
                {
                    _client.Create(fid, path.Last(), Constants.FilePerm, Sharp9P.Constants.Ordwr);
                }
                catch
                {
                    // ignore File Exists error
                }
            }
            else
            {
                _client.Walk(fid, fid, new[] {path.Last()});
                _client.Open(fid, Sharp9P.Constants.Ordwr);
            }
            _client.Write(fid, 0, (uint) utf8.GetByteCount(data), utf8.GetBytes(data));
            _client.FreeFid(fid);
        }

        public string ReadAll(List<string> path)
        {
            var fid = _client.AllocateFid(Sharp9P.Constants.RootFid);
            var utf8 = new UTF8Encoding();
            _client.Walk(fid, fid, path.ToArray());
            _client.Open(fid, Sharp9P.Constants.Oread);
            var read = _client.Read(fid, 0, 16360);
            _client.FreeFid(fid);
            return read.Item1 > 0 ? utf8.GetString(read.Item2, 0, (int) read.Item1) : null;
        }

        public Tuple<uint, string> Read(uint fid, ulong offset)
        {
            var utf8 = new UTF8Encoding();
            Tuple<uint, byte[]> read;
            const int retryCount = 2;
            var currentRetry = 0;
            while (true)
            {
                read = _client.Read(fid, offset, 16360);
                if (read.Item1 > 0) break;
                currentRetry++;
                if (currentRetry < retryCount) continue;
                _client.FreeFid(fid);
                throw new EndOfStreamException();
            }
            return new Tuple<uint, string>(read.Item1, utf8.GetString(read.Item2, 0, (int) read.Item1));
        }
    }
}