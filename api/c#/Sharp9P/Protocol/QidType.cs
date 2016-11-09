namespace Sharp9P.Protocol
{
    public enum QidType
    {
        QtDir = 0x80, // type bit for directories
        QtAppend = 0x40, // type bit for append only files
        QtExcl = 0x20, // type bit for exclusive use files
        QtMount = 0x10, // type bit for mounted channel
        QtAuth = 0x08, // type bit for authentication file
        QtTmp = 0x04, // type bit for not-backed-up file
        QtFile = 0x00 // plain file
    }
}