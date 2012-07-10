
public class paneReservation {
	String _resvName;
	int _bandwidth;
	String _userName;
	int _start;
	int _end;
	paneFlowGroup _flowgroup;
	
	public paneReservation(String resvName, int bandwidth, paneFlowGroup flowgroup) {
		_resvName = resvName;
		_bandwidth = bandwidth;
		_userName = null;
		_start = -1;
		_end = -1;
		_flowgroup = flowgroup;
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
	
	//-------------------start
	public void setStart(int start){
		_start = start;
	}

	public boolean isSetStart(){
		return _start == -1?false:true;
	}

	public int getStart(){
		return _start;
	}
	//-------------------end
	public void setEnd(int end){
		_end = end;
	}

	public boolean isSetEdn(){
		return _end == -1?false:true;
	}

	public int getEnd(){
		return _end;
	}
}
