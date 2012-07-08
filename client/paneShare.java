import java.util.ArrayList;


public class paneShare {


	String _shareName;
	String _parentName;
	int _maxresv;
	int _minresv;
	String _usrname;
	Boolean _allow; 
	Boolean _deny;  
	int _reserveTBCapacity;
	int _reserveTBFill;
	ArrayList<String> _grantedSpks;//contains all speakers that are already granted
	ArrayList<String> _newSpks;//contains all new speakers, if they are granted, they are moved to grantedSpks
	
	enum Boolean{
		UNDEF, TRUE, FALSE
	}

	public paneShare(String shareName, String parentName, int maxresv){
		_shareName = shareName;
		_parentName = parentName;
		_maxresv = maxresv;
		_minresv = -1;
		_usrname = null;
		_allow = Boolean.UNDEF;
		_deny = Boolean.UNDEF;
		_reserveTBCapacity = -1;
		_reserveTBFill = -1;
		_grantedSpks = new ArrayList<String>();
		_newSpks = new ArrayList<String>();
	}

	//---------------------min resv
	public void setMinResv(int minresv){
		_minresv = minresv;
	}

	public boolean isSetMin(){
		return _minresv == -1?false:true;
	}

	public int getMinResv(){
		return _minresv;
	}

	//-------------------usr name
	public void setUsrName(String usrname){
		_usrname = usrname;
	}

	public boolean isSetUsrName(){
		return _usrname == null?false:true;
	}

	public String getUsrName(){
		return _usrname;
	}
	//------------------allow
	public void setAllow(boolean allow){
		_allow = allow == true?Boolean.TRUE:Boolean.FALSE;
	}

	public boolean isSetAllow(){
		return _allow == Boolean.UNDEF?false:true;
	}

	public boolean getAllow(){
		return _allow == Boolean.TRUE?true:false;
	}
	//----------------deny
	public void setDeny(boolean deny){
		_deny = deny == true?Boolean.TRUE:Boolean.FALSE;
	}

	public boolean isSetDeny(){
		return _deny == Boolean.UNDEF?false:true;
	}

	public boolean getDeny(){
		return _deny == Boolean.TRUE?true:false;
	}	

	//---------------reserveTBCapacity
	public void setReserveTBCapacity(int rtbc){
		_reserveTBCapacity = rtbc;
	}

	public boolean isSetReserveTBCapacity(){
		return _reserveTBCapacity == -1?false:true;
	}

	public int getReserveTBCapacity(){
		return _reserveTBCapacity;
	}
	//---------------reserveTBFill
	public void setReserveTBFill(int rtbf){
		_reserveTBFill = rtbf;
	}

	public boolean isSetReserveTBFill(){
		return _reserveTBFill == -1?false:true;
	}

	public int getReserveTBFill(){
		return _reserveTBFill;
	}
	//-------------------speakers
	public void addSpeaker(String spkName){
		_newSpks.add(spkName);
	}
	
	public boolean hasNewSpk(){
		return !_newSpks.isEmpty();
	}
	
	public ArrayList<String> getNewSpk(){
		return _newSpks;
	}
	
	public void confirmSpk(){
		for(String spk : _newSpks){
			_grantedSpks.add(spk);
		}
	}

}
