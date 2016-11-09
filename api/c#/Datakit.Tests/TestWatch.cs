using System;
using System.Collections.Generic;
using System.Linq;
using Moq;
using NUnit.Framework;

namespace Datakit.Tests
{
    [TestFixture]
    public class TestWatch
    {
        [Test]
        public void TestNext()
        {
            var watch = new List<string> {"branch", "master", "watch", "test.node", "tree.live"};
            var mock = new Mock<IClient>();

            Tuple<uint, string>[] readResults =
            {
                Tuple.Create(1u, "\n"),
                Tuple.Create(1u, "\n"),
                Tuple.Create(1u, "\n"),
                Tuple.Create(4u, "test"),
                Tuple.Create(4u, "test2")
            };

            var calls = 0;
            mock.Setup(c => c.Open(It.Is<List<string>>(l => l.SequenceEqual(watch)), 0)).Returns(1);

            mock.Setup(c => c.Read(1, It.IsAny<ulong>()))
                .Returns(() => readResults[calls])
                .Callback(() => calls++);

            var b = new Branch(mock.Object, "master");
            var path = new List<string> {"test"};
            var w = new Watch(mock.Object, b.Path, path);

            var s = w.Next();
            Assert.That(s.Id, Is.EqualTo("test"));

            mock.Verify(c => c.Open(It.Is<List<string>>(l => l.SequenceEqual(watch)), 0), Times.Once);
            mock.Verify(c => c.Read(1, 0), Times.Once);
            mock.Verify(c => c.Read(1, 1), Times.Once);
            mock.Verify(c => c.Read(1, 2), Times.Once);
            mock.Verify(c => c.Read(1, 3), Times.Once);

            s = w.Next();
            Assert.That(s.Id, Is.EqualTo("test2"));
            mock.Verify(c => c.Read(1, 7), Times.Once);
        }

        [Test]
        public void TestWatchPath()
        {
            var watch = new List<string> {"branch", "master", "watch", "test.node", "tree.live"};
            var mock = new Mock<IClient>();
            mock.Setup(c => c.Open(It.Is<List<string>>(l => l.SequenceEqual(watch)), 0)).Returns(1);

            var b = new Branch(mock.Object, "master");
            var path = new List<string> {"test"};
            var w = new Watch(mock.Object, b.Path, path);
            mock.Verify(c => c.Open(It.Is<List<string>>(l => l.SequenceEqual(watch)), 0), Times.Once);
        }
    }
}