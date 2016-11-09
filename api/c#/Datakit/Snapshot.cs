using System.Collections.Generic;
using System.Linq;

namespace Datakit
{
    public abstract class Snapshot
    {
        protected IClient Client;
        public List<string> Path;

        protected Snapshot(IClient client, string id)
        {
            Client = client;
            Id = id;
        }

        public string Id { get; }

        public string Read(List<string> key)
        {
            var path = Path.ToList();
            path.AddRange(key);
            var result = Client.ReadAll(path);
            return result;
        }
    }

    public class ObjectSnapsot : Snapshot
    {
        private const string Trees = "trees";

        public ObjectSnapsot(IClient client, string id) : base(client, id)
        {
            Path = new List<string> {Trees, Id};
        }
    }

    public class CommmitSnapshot : Snapshot
    {
        private const string Snapshots = "snapshots";
        private const string Ro = "ro";

        public CommmitSnapshot(IClient client, string id) : base(client, id)
        {
            Path = new List<string> {Snapshots, Id, Ro};
        }
    }
}