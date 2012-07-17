import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.ArrayList;


public class paneShare {


	String _shareName;
	paneShare _parentShare;
	int _maxresv;
	int _minresv;
	Boolean _allow; 
	Boolean _deny;  
	int _reserveTBCapacity;
	int _reserveTBFill;
	ArrayList<String> _speakers;
	
	Socket _serverSock;	

	public paneShare(String shareName, paneShare parentShare, int maxresv){
		_shareName = shareName;
		_parentShare = parentShare;
		_maxresv = maxresv;
		_minresv = -1;
		_allow = null;
		_deny = null;
		_reserveTBCapacity = -1;
		_reserveTBFill = -1;
		_speakers = new ArrayList<String>();
		_serverSock = null;
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

//	//-------------------usr name
//	public void setUsrName(String usrname){
//		_usrname = usrname;
//	}
//
//	public boolean isSetUsrName(){
//		return _usrname == null?false:true;
//	}
//
//	public String getUsrName(){
//		return _usrname;
//	}
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
		_speakers.add(spkName);
	}
	
	public void removeSpeakers(String spkName){
		_speakers.remove(spkName);
	}
	
	public void setServerSoc(Socket soc){
		_serverSock = soc;
	}
	
	/**
	 * call 'grant' command to grant the current share to the user.
	 * 
	 * @param user
	 *            name of the user to be granted.
	 * @return
	 * @throws IOException          
	 */
	public synchronized String grant(paneUser user) throws IOException{
		
		String cmd = "grant " + getShareName() + " to " + user.getName();
		String response = contactServer(cmd);
//		if succeeded
//		_speakers.add(user.getName());
//		user.addShare(this);
		return response;
	}	
	
	/**
	 * call 'reserve' command to reserve bandwidth specified.
	 * 
	 * @param resv
	 *            configuration for the new reservation
	 * @return
	 * @throws IOException
	 */
	public String reserve(paneReservation resv) throws IOException{
		
		String cmd = resv.generateCmd();
		String response = contactServer(cmd);
		return response;
	}
	
	/**
	 * call 'newShare' command to create subshare according to the given parameters.
	 * 
	 * @param share
	 *            the new share to be created
	 * @return
	 * @throws IOException
	 */
	public String newShare(paneShare share) throws IOException{
		
		String cmd = share.generateCreationCmd();
		share.setServerSoc(_serverSock);
		String response = contactServer(cmd);
		return response;
	}
	

	/**
	 * call 'allow' command on current share for the given flowgroup.
	 * @param allow
	 *            the configuration for the allow command
	 * @return
	 * @throws IOException
	 */
	public String allow(paneAllow allow) throws IOException{
		
		String cmd = allow.generateCmd();
		String response = contactServer(cmd);
		return response;
	}
	
	/**
	 * call 'deny' command on current share for the given flowgroup. 
	 * @param deny
	 *            the configuration for the deny command
	 * @return
	 * @throws IOException
	 */
	public String deny(paneDeny deny) throws IOException{
		
		String cmd = deny.generateCmd();
		String response = contactServer(cmd);		
		return response;
	}
	
	/**
	 * 
	 * send the given command to the server ,then wait and return the response from server
	 * @return
	 * @throws IOException 
	 */
	protected String contactServer(String cmd) throws IOException{
		
		byte[] ret = new byte[1000];
		OutputStream os = _serverSock.getOutputStream();
		InputStream is = _serverSock.getInputStream();
		
		os.write(cmd.getBytes());
		os.flush();
		is.read(ret);
		
		String response = new String (ret);
		
		System.out.println("resposne:"+response);
		return response;
		
	}
	
	
	protected String generateCreationCmd(){
		/*
		 * so far, assume that it is always 'NewShare ... for (*)...', the question here
		 * is that is there a initial flowgroup associated with a share when the
		 * share is being created? so what is this flowgroup for? I guess maybe
		 * it is not possible to make reservation beyond the scope of this initial
		 * flowgroup
		 */
		
		String cmd = "NewShare " + getShareName() + " for (*) ";
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
		cmd += "] on "+_parentShare.getShareName();
		return cmd;
		
	}
}
