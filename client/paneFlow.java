import java.net.InetAddress;

public class paneFlow {
	
	String _srcUser;
	String _dstUser;
	int _srcPort;
	int _dstPort;
	InetAddress _srcHost;
	InetAddress _dstHost;
	
	public paneFlow(String srcUser, String dstUser, int srcPort, int dstPort,
			InetAddress srcHost, InetAddress dstHost){
		_srcUser = srcUser;
		_dstUser = dstUser;
		_srcPort = srcPort;
		_dstPort = dstPort;
		_srcHost = srcHost;
		_dstHost = dstHost;
	}
}