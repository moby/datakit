using System;
using System.Collections.Generic;
using System.Linq;

namespace Datakit
{
    public interface IWatch
    {
        Snapshot Next();
    }

    public class Watch : IWatch
    {
        private const string WatchKey = "watch";
        private const string TreeLive = "tree.live";
        private const string NodeSuffix = ".node";
        private readonly IClient _client;
        private readonly uint _fid;
        private ulong _offset;

        public Watch(IClient client, IEnumerable<string> parentPath, IEnumerable<string> path)
        {
            _client = client;
            var watchPath = parentPath.ToList();
            watchPath.Add(WatchKey);
            var nodes = path.Select(node => $"{node}{NodeSuffix}").ToList();
            nodes.Add(TreeLive);
            watchPath.AddRange(nodes);
            _fid = _client.Open(watchPath, Sharp9P.Constants.Oread);
            _offset = 0;
        }

        public Snapshot Next()
        {
            Tuple<uint, string> data;
            while (true)
            {
                data = _client.Read(_fid, _offset);
                _offset += data.Item1;
                if (data.Item2 == "\n")
                {
                    // Path doesn't exist yet
                    continue;
                }
                break;
            }
            var results = data.Item2.Split(new[] {"\n"}, StringSplitOptions.None);
            var sha = Array.FindLast(results, item => item != "");
            if (sha == null)
            {
                throw new Exception($"sha was null. result was: {data}");
            }
            return new ObjectSnapsot(_client, sha);
        }

        ~Watch()
        {
            _client.Close(_fid);
        }
    }
}