using System.Collections.Generic;
using System.Threading;
using NUnit.Framework;

namespace Datakit.Tests
{
    // This requires that you have the DB started and ready to receive connections on a named pipe
    // e.g ./com.docker.db.exe --git 'C:\Users\Dave\dev\docker\datakit\database' --verbose=debug --url \\.\\pipe\\datakit
    [TestFixture]
    public class IntegrationTest
    {
        [Test]
        public void TestAddFieldsAndSync()
        {
            var b = new Branch(Client.Instance, "master");
            var t = b.NewTransaction("foo");
            var r = b.NewRecord(new List<string> {"bar"});

            Assert.That(b.Path, Is.EquivalentTo(new List<string> {"branch", "master"}));
            Assert.That(t.Path, Is.EquivalentTo(new List<string> {"branch", "master", "transactions", "foo"}));
            Assert.That(r.Path, Is.EquivalentTo(new List<string> {"bar"}));

            var stringField = new Field<string>(r, "foo", "quux");
            var intField = new Field<int>(r, "bar", 1);
            var boolField = new Field<bool>(r, "baz", false);

            Assert.That(stringField.Path, Is.EquivalentTo(new List<string> {"foo"}));
            Assert.That(intField.Path, Is.EquivalentTo(new List<string> {"bar"}));
            Assert.That(boolField.Path, Is.EquivalentTo(new List<string> {"baz"}));
            Assert.That(stringField.Version, Is.EqualTo(1));
            Assert.That(intField.Version, Is.EqualTo(1));
            Assert.That(boolField.Version, Is.EqualTo(1));

            r.Fields.Add(stringField);
            r.Fields.Add(intField);
            r.Fields.Add(boolField);

            Assert.That(r.Fields.Count, Is.EqualTo(4));

            r.Sync();

            Assert.That(stringField.Version, Is.EqualTo(1));
            Assert.That(intField.Version, Is.EqualTo(1));
            Assert.That(boolField.Version, Is.EqualTo(1));

            var thread = new Thread(r.WaitForUpdates);
            thread.Start();

            stringField.SetValue("New String value", "quux");
            intField.SetValue("New int value", 5.ToString());
            boolField.SetValue("New bool value", true.ToString());

            Thread.Sleep(10000);

            Assert.That(stringField.Value, Is.EqualTo("quux"));
            Assert.That(intField.Value, Is.EqualTo(5));
            Assert.That(boolField.Value, Is.EqualTo(true));

            thread.Abort();
        }
    }
}