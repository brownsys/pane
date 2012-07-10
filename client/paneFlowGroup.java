import java.net.InetAddress;
import java.util.ArrayList;


public class paneFlowGroup {
	
	ArrayList<paneFlow> _allflows;
	
	String _srcUser;
	String _dstUser;
	int _srcPort;
	int _dstPort;
	InetAddress _srcHost;
	InetAddress _dstHost;
	
	public paneFlowGroup(){
		_allflows = new ArrayList<paneFlow>();
		_srcUser = null;
		_dstUser = null;
		_srcPort = -1;
		_dstPort = -1;
		_srcHost = null;
		
	}
	
	public paneFlowGroup(String srcUser, String dstUser, int srcPort, int dstPort,
			InetAddress srcHost, InetAddress dstHost){
		_allflows = new ArrayList<paneFlow>();
		_srcUser = srcUser;
		_dstUser = dstUser;
		_srcPort = srcPort;
		_dstPort = dstPort;
		_srcHost = srcHost;
		_dstHost = dstHost;
	}
	//----------------srcUser
	public void setSrcUser(String srcUser){
		_srcUser = srcUser;
	}
	
	public boolean isSetSrcUser(){
		return _srcUser == null?false:true;
	}
	
	public String getSrcUser(){
		return _srcUser;
	}
	
	//----------------srcUser
	public void setDstUser(String dstUser){
		_dstUser = dstUser;
	}
	
	public boolean isSetDstUser(){
		return _dstUser == null?false:true;
	}
	
	public String getDstUser(){
		return _dstUser;
	}
	
	//----------------------srcPort
	public void setSrcPort(int srcPort){
		_srcPort = srcPort;
	}
	
	public boolean isSetSrcPort(){
		return _srcPort == -1?false:true;
	}
	
	public int getSrcPort(){
		return _srcPort;
	}
	
	//----------------------dstPort
	public void setDstPort(int dstPort){
		_dstPort = dstPort;
	}
	
	public boolean isSetDstPort(){
		return _dstPort == -1?false:true;
	}
	
	public int getDstPort(){
		return _dstPort;
	}
	
	//----------------------srcHost
	public void setSrcHost(InetAddress srcHost){
		_srcHost = srcHost;
	}
	
	public boolean isSetSrcHost(){
		return _srcHost == null?false:true;
	}
	
	public InetAddress getSrcHost(){
		return _srcHost;
	}
	
	//----------------------dstHost
	public void setDstHost(InetAddress dstHost){
		_dstHost = dstHost;
	}
	
	public boolean isSetDstHost(){
		return _dstHost == null?false:true;
	}
	
	public InetAddress getDstHost(){
		return _dstHost;
	}

	
	
}
