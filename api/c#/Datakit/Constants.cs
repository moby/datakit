namespace Datakit
{
    public class Constants
    {
        public const uint Rwx = Sharp9P.Constants.Dmread | Sharp9P.Constants.Dmwrite | Sharp9P.Constants.Dmexec;
        public const uint Rw = Sharp9P.Constants.Dmread | Sharp9P.Constants.Dmexec;
        public const uint R = Sharp9P.Constants.Dmread;
        public const uint DirPerm = Rwx << 6 | Rw << 3 | R | Sharp9P.Constants.Dmdir;
        public const uint FilePerm = Rw << 6 | R << 3 | R;
    }
}