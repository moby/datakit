using System.Collections.Generic;
using System.Linq;

namespace Datakit
{
    public interface ITransaction
    {
        string Name { get; set; }
        string Message { get; set; }
        List<string> Path { get; set; }
        void Write(List<string> key, string value);
        void Commit();
        void Close();
    }

    public class Transaction : ITransaction
    {
        private const string Ctl = "ctl";
        private const string CtlCommit = "commit";
        private const string CtlClose = "close";
        private const string Transactions = "transactions";
        private const string Rw = "rw";
        private readonly IClient _client;

        public Transaction(IClient client, IBranch parent, string name)
        {
            _client = client;
            Name = name;
            var path = parent.Path.ToList();
            path.Add(Transactions);
            path.Add(name);
            Path = path;
            _client.Mkdir(path);
        }

        public string Name { get; set; }
        public string Message { get; set; }
        public List<string> Path { get; set; }

        public void Write(List<string> key, string value)
        {
            var path = Path.ToList();
            path.Add(Rw);
            path.AddRange(key);
            var dirs = path.Take(path.Count - 1).ToList();
            _client.Mkdir(dirs);
            _client.Write(path, value, true);
        }

        public void Commit()
        {
            var path = Path.ToList();
            path.Add(Ctl);
            _client.Write(path, CtlCommit, false);
        }

        public void Close()
        {
            var path = Path.ToList();
            path.Add(Ctl);
            _client.Write(path, CtlClose, false);
        }
    }
}