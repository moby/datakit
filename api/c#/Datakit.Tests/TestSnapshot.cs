using System.Collections.Generic;
using System.Linq;
using Moq;
using NUnit.Framework;

namespace Datakit.Tests
{
    [TestFixture]
    internal class TestSnapshot
    {
        [Test]
        public void TestCommitPath()
        {
            var mock = new Mock<IClient>();
            var s = new CommmitSnapshot(mock.Object, "test");
            var expected = new List<string> {"snapshots", "test", "ro"};
            Assert.That(s.Path, Is.EquivalentTo(expected));
        }

        [Test]
        public void TestCommitRead()
        {
            var p1 = new List<string> {"snapshots", "test", "ro", "test1"};
            var mock = new Mock<IClient>();
            mock.Setup(c => c.ReadAll(It.Is<List<string>>(l => l.SequenceEqual(p1)))).Returns("foo");
            var s = new CommmitSnapshot(mock.Object, "test");
            var result = s.Read(new List<string> {"test1"});
            Assert.That(result, Is.EqualTo("foo"));
        }

        [Test]
        public void TestObjectPath()
        {
            var mock = new Mock<IClient>();
            var s = new ObjectSnapsot(mock.Object, "test");
            var expected = new List<string> {"trees", "test"};
            Assert.That(s.Path, Is.EquivalentTo(expected));
        }

        [Test]
        public void TestObjectRead()
        {
            var p1 = new List<string> {"trees", "test", "test1"};
            var mock = new Mock<IClient>();
            mock.Setup(c => c.ReadAll(It.Is<List<string>>(l => l.SequenceEqual(p1)))).Returns("bar");
            var s = new ObjectSnapsot(mock.Object, "test");
            var result = s.Read(new List<string> {"test1"});
            Assert.That(result, Is.EqualTo("bar"));
        }
    }
}