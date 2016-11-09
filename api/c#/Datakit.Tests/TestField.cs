using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;
using Moq;

namespace Datakit.Tests
{
    [TestFixture]
    public class TestField
    {
        [Test]
        public void TestBoolFieldConstruction()
        {
            var mock = new Mock<IClient>();
            var b = new Branch(mock.Object, "master");
            var path = new List<string> {"test"};
            var r = b.NewRecord(path);

            var f = new Field<bool>(r, "test/test-field", true);
            Assert.That(f.Key, Is.EqualTo("test/test-field"));
            Assert.That(f.Value, Is.EqualTo("true"));
            Assert.That(f.RawValue, Is.EqualTo(true));
            Assert.That(f.Path.Count, Is.EqualTo(2));
            Assert.That(f.Path[0], Is.EqualTo("test"));
            Assert.That(f.Path[1], Is.EqualTo("test-field"));


            var f1 = new Field<bool>(r, "test", true);
            Assert.That(f1.Key, Is.EqualTo("test"));
            Assert.That(f1.Value, Is.EqualTo("true"));
            Assert.That(f1.RawValue, Is.EqualTo(true));
            Assert.That(f1.Path.Count, Is.EqualTo(1));
            Assert.That(f1.Path[0], Is.EqualTo("test"));
        }

        [Test]
        public void TestIntFieldConstruction()
        {
            var mock = new Mock<IClient>();
            var b = new Branch(mock.Object, "master");
            var path = new List<string> { "test" };
            var r = b.NewRecord(path);

            var f = new Field<int>(r, "test/test-field", 1234);
            Assert.That(f.Key, Is.EqualTo("test/test-field"));
            Assert.That(f.Value, Is.EqualTo("1234"));
            Assert.That(f.RawValue, Is.EqualTo(1234));
            Assert.That(f.Path.Count, Is.EqualTo(2));
            Assert.That(f.Path[0], Is.EqualTo("test"));
            Assert.That(f.Path[1], Is.EqualTo("test-field"));


            var f1 = new Field<int>(r, "test", 1234);
            Assert.That(f1.Key, Is.EqualTo("test"));
            Assert.That(f1.Value, Is.EqualTo("1234"));
            Assert.That(f1.RawValue, Is.EqualTo(1234));
            Assert.That(f1.Path.Count, Is.EqualTo(1));
            Assert.That(f1.Path[0], Is.EqualTo("test"));
        }

        [Test]
        public void TestStringFieldConstruction()
        {
            var mock = new Mock<IClient>();
            var b = new Branch(mock.Object, "master");
            var path = new List<string> { "test" };
            var r = b.NewRecord(path);

            var f = new Field<string>(r, "test/test-field", "test");
            Assert.That(f.Key, Is.EqualTo("test/test-field"));
            Assert.That(f.Value, Is.EqualTo("test"));
            Assert.That(f.RawValue, Is.EqualTo("test"));
            Assert.That(f.Path.Count, Is.EqualTo(2));
            Assert.That(f.Path[0], Is.EqualTo("test"));
            Assert.That(f.Path[1], Is.EqualTo("test-field"));


            var f1 = new Field<string>(r, "test", "test");
            Assert.That(f1.Key, Is.EqualTo("test"));
            Assert.That(f1.Value, Is.EqualTo("test"));
            Assert.That(f1.RawValue, Is.EqualTo("test"));
            Assert.That(f1.Path.Count, Is.EqualTo(1));
            Assert.That(f1.Path[0], Is.EqualTo("test"));
        }

        [Test]
        public void TestStringFieldSet()
        {
            var head = new List<string> { "branch", "master", "head" };
            var mock = new Mock<IClient>();
            mock.Setup(c => c.ReadAll(It.Is<List<string>>(l => l.SequenceEqual(head)))).Returns("test");
            mock.Setup(c => c.Write(It.IsAny<List<string>>(), It.IsAny<string>(), It.IsAny<bool>()));
            var b = new Branch(mock.Object, "master");
            var path = new List<string> { "test" };
            var r = b.NewRecord(path);

            var field = new Field<string>(r, "test1", "foo");
            r.Fields.Add(field);
            r.Sync();
            field.SetValue("new value", "bar");

            var writePath = new List<string> {"branch", "master", "transactions", "new value", "rw", "test", "test1"};
            mock.Verify(c => c.Write(It.Is<List<string>>(l => l.SequenceEqual(writePath)), "bar", true), Times.Once);
        }
}
}