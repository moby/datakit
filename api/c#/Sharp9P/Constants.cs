namespace Sharp9P
{
    public static class Constants
    {
        internal const int Bit8Sz = 1;
        internal const int Bit16Sz = 2;
        internal const int Bit32Sz = 4;
        internal const int Bit64Sz = 8;
        internal const int Qidsz = Bit8Sz + Bit32Sz + Bit64Sz;
        internal const int Maxwelem = 16;
        internal const int HeaderOffset = 7;

        internal const ushort NoTag = 0;
        public const uint NoFid = 0;

        public const uint Dmdir = 0x80000000;
        public const uint Dmappend = 0x40000000;
        public const uint Dmexcl = 0x20000000;
        public const uint Dmmount = 0x10000000;
        public const uint Dmauth = 0x08000000;
        public const uint Dmtmp = 0x04000000;
        public const uint Dmnone = 0xFC000000;
        public const uint Dmread = 0x4;
        public const uint Dmwrite = 0x2;
        public const uint Dmexec = 0x1;
        public const byte Oread = 0x00;
        public const byte Owrite = 0x01;
        public const byte Ordwr = 0x02;
        public const byte Oexec = 0x03;
        public const byte Otrunc = 0x10;
        public const byte Ocexec = 0x10;
        public const byte Orclose = 0x10;

        public const uint DefaultMsize = 16384;
        public const string DefaultVersion = "9P2000";
        public const uint RootFid = 1;
    }
}