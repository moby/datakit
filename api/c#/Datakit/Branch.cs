using System;
using System.Collections.Generic;
using System.Linq;

namespace Datakit
{
    public interface IBranch
    {
        List<string> Path { get; set; }
        string Name { get; set; }
        string Head { get; }
        void FastForward(string sha);
        ITransaction NewTransaction(string name);
        IRecord NewRecord(List<string> path);
        string Read(List<string> key);
    }

    public class Branch : IBranch
    {
        private const string BranchKey = "branch";
        private const string HeadKey = "head";
        private const string Ro = "ro";
        private readonly IClient _client;

        public Branch(IClient client, string name)
        {
            _client = client;
            Name = name;
            Path = new List<string> {BranchKey, name};
            _client.Mkdir(Path);
        }

        public List<string> Path { get; set; }
        public string Name { get; set; }

        public string Head
        {
            get
            {
                var path = Path.ToList();
                path.Add(HeadKey);
                var result = _client.ReadAll(path);
                if (result == null || result == "\n" || result.Length < 2)
                {
                    throw new NoHeadException("Cannot get HEAD of branch");
                }
                return result;
            }
        }

        public void FastForward(string sha)
        {
            throw new NotImplementedException();
        }

        public ITransaction NewTransaction(string name)
        {
            return new Transaction(_client, this, name);
        }

        public IRecord NewRecord(List<string> path)
        {
            return new Record(_client, this, path);
        }

        public string Read(List<string> key)
        {
            var path = Path.ToList();
            path.Add(Ro);
            path.AddRange(key);
            return _client.ReadAll(path);
        }
    }
}