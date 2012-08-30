package edu.brown.cs.paneclient;
import java.net.InetAddress;

public class PaneFlowGroup {
	
	String _srcUser;
	String _dstUser;
	int _srcPort;
	int _dstPort;
	InetAddress _srcHost;
	InetAddress _dstHost;
	int _transportProto;

	public final static int PROTO_ICMP = 1;
	public final static int PROTO_TCP = 6;
	public final static int PROTO_UDP = 17;
	
	public PaneFlowGroup() {
		_srcUser = null;
		_dstUser = null;
		_srcPort = -1;
		_dstPort = -1;
		_srcHost = null;
		_dstHost = null;
		_transportProto = -1;
	}
	
	public PaneFlowGroup(String srcUser, String dstUser, int srcPort, int dstPort,
			InetAddress srcHost, InetAddress dstHost, int transportProto) {
		_srcUser = srcUser;
		_dstUser = dstUser;
		_srcPort = srcPort;
		_dstPort = dstPort;
		_srcHost = srcHost;
		_dstHost = dstHost;
		_transportProto = transportProto;
	}
	//----------------srcUser
	public void setSrcUser(String srcUser) {
		_srcUser = srcUser;
	}
	
	public boolean isSetSrcUser() {
		return _srcUser == null?false:true;
	}
	
	public String getSrcUser() {
		return _srcUser;
	}
	
	//----------------dstUser
	public void setDstUser(String dstUser) {
		_dstUser = dstUser;
	}
	
	public boolean isSetDstUser() {
		return _dstUser == null?false:true;
	}
	
	public String getDstUser() {
		return _dstUser;
	}
	
	//----------------------srcPort
	public void setSrcPort(int srcPort) {
		_srcPort = srcPort;
	}
	
	public boolean isSetSrcPort() {
		return _srcPort == -1?false:true;
	}
	
	public int getSrcPort() {
		return _srcPort;
	}
	
	//----------------------dstPort
	public void setDstPort(int dstPort) {
		_dstPort = dstPort;
	}
	
	public boolean isSetDstPort() {
		return _dstPort == -1?false:true;
	}
	
	public int getDstPort() {
		return _dstPort;
	}
	
	//----------------------srcHost
	public void setSrcHost(InetAddress srcHost) {
		_srcHost = srcHost;
	}
	
	public boolean isSetSrcHost() {
		return _srcHost == null?false:true;
	}
	
	public InetAddress getSrcHost() {
		return _srcHost;
	}
	
	//----------------------dstHost
	public void setDstHost(InetAddress dstHost) {
		_dstHost = dstHost;
	}
	
	public boolean isSetDstHost() {
		return _dstHost == null?false:true;
	}
	
	public InetAddress getDstHost() {
		return _dstHost;
	}

	//----------------------transportProto
	public void setTransportProto(int transportProto) {
		_transportProto = transportProto;
	}

	public boolean isSetTransportProto() {
		return _transportProto == -1?false:true;
	}

	public int getTransportProto() {
		return _transportProto;
	}

	//---------------------generate the string
	public String generateConfig() {
		String config = "";
		String fill = ", ";
		if (isSetSrcUser()) {
			config += (fill + "srcUser=" + getSrcUser());
		}
		
		if (isSetDstUser()) {
			config += (fill + "dstUser=" + getDstUser());
		}
		
		if (isSetSrcPort()) {
			config += (fill + "srcPort=" + getSrcPort());
		}
		
		if (isSetDstPort()) {
			config += (fill + "dstPort=" + getDstPort());
		}
		
		if (isSetSrcHost()) {
			config += (fill + "srcHost=" + getSrcHost().getHostAddress());
		}
		
		if (isSetDstHost()) {
			config += (fill + "dstHost=" + getDstHost().getHostAddress());
		}
		
		if (isSetTransportProto()) {
			config += (fill + "transport=" + getTransportProto());
		}

		if (config == "") {
			config = "*";
		} else {
			config = config.replaceFirst(fill, "");
		}
		return config;
		
	}
	
	public String toString() {
		return "PaneFlowGroup: " + generateConfig();
	}
	
}
