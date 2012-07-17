
public abstract class paneVerb {
	paneTime _start;
	paneTime _end;
	paneUser _user; // the issuer of this command
	paneShare _share;
	
	
	//-------------------start
	public void setStart(paneTime start){
		_start = start;
	}

	public boolean isSetStart(){
		return _start == null?false:true;
	}

	public paneTime getStart(){
		return _start;
	}
	
	//-------------------end
	public void setEnd(paneTime end){
		_end = end;
	}

	public boolean isSetEdn(){
		return _end == null?false:true;
	}

	public paneTime getEnd(){
		return _end;
	}
	
	//-------------------issuer
	public void setUser(paneUser user){
		_user = user;
	}

	public boolean isSetUser(){
		return _user == null?false:true;
	}

	public paneUser getUser(){
		return _user;
	}
	
	//-------------------share
	public void setUser(paneShare share){
		_share = share;
	}

	public boolean isSetParent(){
		return _share == null?false:true;
	}

	public paneShare getParent(){
		return _share;
	}
	
	/**
	 * generate the command according to all these settings
	 * @return
	 */
	public abstract String generateCmd();
}
