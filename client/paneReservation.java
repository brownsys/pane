
public class paneReservation {
	String _resvName;
	int _bandwidth;
	String _userName;
	int _dstPort;
	int _srcPort;
	int _from;
	int _to;
	
	public paneReservation(String resvName, int bandwidth) {
		_resvName = resvName;
		_bandwidth = bandwidth;
		_userName = null;
		_dstPort = -1;
		_srcPort = -1;
		_from = -1;
		_to = -1;
	}
	
	//-------------------userName
	public void setUsrName(String userName){
		_userName = userName;
	}

	public boolean isSetUsrName(){
		return _userName == null?false:true;
	}

	public String getUsrName(){
		return _userName;
	}
	
	//-------------------dstPort
	public void setDstPort(int dstPort){
		_dstPort = dstPort;
	}

	public boolean isSetDstPort(){
		return _dstPort == -1?false:true;
	}

	public int getDstPort(){
		return _dstPort;
	}
	
	//-------------------srcPort
	public void setSrcPort(int srcPort){
		_srcPort = srcPort;
	}

	public boolean isSetSrcPort(){
		return _srcPort == -1?false:true;
	}

	public int getSrcPort(){
		return _srcPort;
	}
	//-------------------start
	public void setStart(int start){
		_from = start;
	}

	public boolean isSetStart(){
		return _from == -1?false:true;
	}

	public int getStart(){
		return _from;
	}
	//-------------------end
	public void setEnd(int end){
		_to = end;
	}

	public boolean isSetEdn(){
		return _to == -1?false:true;
	}

	public int getEnd(){
		return _to;
	}
}
