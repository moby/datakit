using System;
using System.IO;
using System.Text;
using NUnit.Framework;
using Sharp9P.Protocol;
using Sharp9P.Protocol.Messages;

namespace Sharp9P.Test
{
    [TestFixture]
    public class ProtocolTest
    {
        [Test]
        public void TestReadWriteQid()
        {
            var qid = new Qid((byte) QidType.QtFile, 1, 0x111L);
            var bytes = qid.ToBytes();
            var qid2 = new Qid(bytes);
            Assert.That(qid, Is.EqualTo(qid2));
        }

        [Test]
        public void TestReadWriteRattach()
        {
            var stream = new MemoryStream();
            var qid = new Qid((byte) QidType.QtFile, 1, 0x111L);
            var message = new Rattach(qid)
            {
                Tag = 1237
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rattach) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRauth()
        {
            var stream = new MemoryStream();
            var qid = new Qid((byte) QidType.QtFile, 1, 0x111L);
            var message = new Rauth(qid)
            {
                Tag = 1235
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rauth) p.Read();
            Assert.That(qid, Is.EqualTo(message2.Aqid));
        }

        [Test]
        public void TestReadWriteRclunk()
        {
            var stream = new MemoryStream();
            var message = new Rclunk
            {
                Tag = 1251
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rclunk) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRcreate()
        {
            var stream = new MemoryStream();
            var qid = new Qid((byte) QidType.QtFile, 1, 0x111L);
            var message = new Rcreate(qid, 1)
            {
                Tag = 1245
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rcreate) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRerror()
        {
            var stream = new MemoryStream();
            var message = new Rerror("ETEST")
            {
                Tag = 1238
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rerror) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRflush()
        {
            var stream = new MemoryStream();
            var message = new Rflush
            {
                Tag = 1239
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rflush) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRopen()
        {
            var stream = new MemoryStream();
            var qid = new Qid((byte) QidType.QtFile, 1, 0x111L);
            var message = new Ropen(qid, 1)
            {
                Tag = 1243
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Ropen) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRread()
        {
            var stream = new MemoryStream();
            var utf8 = new UTF8Encoding();

            var message = new Rread((uint) utf8.GetByteCount("test"),
                utf8.GetBytes("test"))
            {
                Tag = 1247
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rread) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRremove()
        {
            var stream = new MemoryStream();
            var message = new Rremove
            {
                Tag = 1251
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rremove) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRstat()
        {
            var stream = new MemoryStream();
            var qid = new Qid((byte) QidType.QtFile, 1, 0x111L);
            var stat = new Stat(
                1, 2, qid, 4, 5, 6, 65535L, "foo", "root", "root", "root");
            var message = new Rstat(stat)
            {
                Tag = 1253
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rstat) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRversion()
        {
            var stream = new MemoryStream();
            var message = new Rversion(16384, "9P2000")
            {
                Tag = 0
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rversion) p.Read();
            Assert.That(message2, Is.EqualTo(message));
        }

        [Test]
        public void TestReadWriteRwalk()
        {
            var stream = new MemoryStream();
            var qid = new Qid((byte) QidType.QtFile, 1, 0x111L);
            var message = new Rwalk(1, new[] {qid})
            {
                Tag = 1241
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rwalk) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRwrite()
        {
            var stream = new MemoryStream();
            var message = new Rwrite(8192)
            {
                Tag = 1249
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rwrite) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteRwstat()
        {
            var stream = new MemoryStream();
            var message = new Rwstat
            {
                Tag = 1255
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Rwstat) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTattach()
        {
            var stream = new MemoryStream();
            var message = new Tattach(1, 2, "uname", "aname")
            {
                Tag = 1236
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Tattach) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTauth()
        {
            var stream = new MemoryStream();
            var message = new Tauth(Constants.NoFid, "user", "tree")
            {
                Tag = 1234
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Tauth) p.Read();
            Assert.That(message2, Is.EqualTo(message));
        }

        [Test]
        public void TestReadWriteTclunk()
        {
            var stream = new MemoryStream();
            var message = new Tclunk(1)
            {
                Tag = 1250
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Tclunk) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTcreate()
        {
            var stream = new MemoryStream();
            var message = new Tcreate(1, "test", 1, 1)
            {
                Tag = 1244
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Tcreate) p.Read();
            Console.WriteLine(message.ToString());
            Console.WriteLine(message2.ToString());
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTflush()
        {
            var stream = new MemoryStream();
            var message = new Tflush(1234)
            {
                Tag = 1239
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Tflush) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTopen()
        {
            var stream = new MemoryStream();
            var message = new Topen(1, 1)
            {
                Tag = 1242
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Topen) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTread()
        {
            var stream = new MemoryStream();
            var message = new Tread(1, 65535L, 1)
            {
                Tag = 1246
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Tread) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTremove()
        {
            var stream = new MemoryStream();
            var message = new Tremove(1)
            {
                Tag = 1250
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Tremove) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTstat()
        {
            var stream = new MemoryStream();
            var message = new Tstat(1)
            {
                Tag = 1252
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Tstat) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTversion()
        {
            var stream = new MemoryStream();
            var message = new Tversion(16384, "9P2000")
            {
                Tag = 0
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Tversion) p.Read();
            Assert.That(message2, Is.EqualTo(message));
        }

        [Test]
        public void TestReadWriteTwalk()
        {
            var stream = new MemoryStream();
            var message = new Twalk(3, 4, 2, new[] {"hello", "world"})
            {
                Tag = 1240
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Twalk) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTwrite()
        {
            var stream = new MemoryStream();
            var utf8 = new UTF8Encoding();

            var message = new Twrite(
                1, 65535L,
                (uint) utf8.GetByteCount("test"),
                utf8.GetBytes("test"))
            {
                Tag = 1248
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Twrite) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }

        [Test]
        public void TestReadWriteTwstat()
        {
            var stream = new MemoryStream();
            var qid = new Qid((byte) QidType.QtFile, 1, 0x111L);
            var stat = new Stat(
                1, 2, qid, 4, 5, 6, 65535L, "foo", "root", "root", "root");
            var message = new Twstat(1, stat)
            {
                Tag = 1254
            };

            var p = new Protocol.Protocol(stream);
            p.Write(message);
            stream.Position = 0;

            var data = new byte[message.Length];
            var length = stream.Read(data, 0, (int) message.Length);

            Assert.That(length, Is.EqualTo(message.Length));

            stream.Position = 0;
            var message2 = (Twstat) p.Read();
            Assert.That(message, Is.EqualTo(message2));
        }
    }
}