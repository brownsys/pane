import java.io.IOException;
import java.util.ArrayList;


public class PaneShare {


	String _shareName;
	int _maxresv;
	int _minresv;
	Boolean _allow; 
	Boolean _deny;  
	int _reserveTBCapacity;
	int _reserveTBFill;
	ArrayList<String> _principals;
	String _parentName; //for test and debug purpose
	
	PaneFlowGroup _initfg;
	
	PaneClient _client;

	public PaneShare(String shareName, int maxresv, PaneFlowGroup fg){
		/*
		 * fg here is the initial flow group of this share, if a null
		 * is given, '*' will be put into the command
		 */
		_shareName = shareName;
		_maxresv = maxresv;
		_minresv = -1;
		_allow = null;
		_deny = null;
		_reserveTBCapacity = -1;
		_reserveTBFill = -1;
		_principals = new ArrayList<String>();
		_client = null;
		_initfg = fg;
	}

	
	public String getShareName(){
		return _shareName;
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

	//------------------allow
	public void setAllow(boolean allow){
		_allow = allow == true?Boolean.TRUE:Boolean.FALSE;
	}

	public boolean isSetAllow(){
		return _allow == null?false:true;
	}

	public boolean getAllow(){
		return _allow == Boolean.TRUE?true:false;
	}
	//----------------deny
	public void setDeny(boolean deny){
		_deny = deny == true?Boolean.TRUE:Boolean.FALSE;
	}		


	public boolean isSetDeny(){
		return _deny == null?false:true;
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
		_principals.add(spkName);
	}
	
	public void removeSpeakers(String spkName){
		_principals.remove(spkName);
	}
	
	public void setClient(PaneClient client){
		_client = client;
	}

	public synchronized String grant(PaneUser user) throws IOException{
		
		String cmd = "grant " + getShareName() + " to " + user.getName();
		String response = contactServer(cmd);
//		if succeeded
//		_speakers.add(user.getName());
//		user.addShare(this);
		return response;
	}	

	public String newShare(PaneShare share) throws IOException{
		
		share.setParentName(_shareName);
		String cmd = share.generateCreationCmd();
		share.setClient(_client);
		String response = contactServer(cmd);
		return response;
	}
	
	public String reserve(PaneReservation resv) throws IOException{
		
		resv.setParent(this);
		String cmd = resv.generateCmd();
		String response = contactServer(cmd);
		return response;
	}
	
	public String allow(PaneAllow allow) throws IOException{
		
		allow.setParent(this);
		String cmd = allow.generateCmd();
		String response = contactServer(cmd);
		return response;
	}
	
	public String deny(PaneDeny deny) throws IOException{
		
		deny.setParent(this);
		String cmd = deny.generateCmd();
		String response = contactServer(cmd);		
		return response;
	}
	
	protected String contactServer(String cmd) throws IOException{
		String response = _client.sendAndWait(cmd);
		return response;
	}

	
	protected void setParentName(String parentName){
		_parentName = parentName;
	}
	
	protected String generateCreationCmd(){
		
		String fg = "*";
		if(_initfg != null)
			fg = _initfg.generateConfig();
		
		String cmd = "NewShare " + getShareName() + " for ("+fg+") ";
		cmd += "[reserve <= "+ _maxresv;
		if(_minresv != -1){
			cmd += " reserve >= "+_minresv;
		}
		if(_reserveTBCapacity != -1){
			cmd += " reserveTBCapacity = "+ _reserveTBCapacity;
		}
		if(_reserveTBFill != -1){
			cmd += " reserveTBFill = " + _reserveTBFill;
		}
		cmd += "] on "+ _parentName;
		return cmd;
		
	}
	
	public String toString(){
		return " PaneShare:"+generateCreationCmd();
	}
}
