namespace Sharp9P.Protocol
{
    public enum MessageType
    {
        Tversion = 100,
        Rversion,
        Tauth,
        Rauth,
        Tattach,
        Rattach,
        Terror,
        Rerror,
        Tflush,
        Rflush,
        Twalk,
        Rwalk,
        Topen,
        Ropen,
        Tcreate,
        Rcreate,
        Tread,
        Rread,
        Twrite,
        Rwrite,
        Tclunk,
        Rclunk,
        Tremove,
        Rremove,
        Tstat,
        Rstat,
        Twstat,
        Rwstat
    }
}