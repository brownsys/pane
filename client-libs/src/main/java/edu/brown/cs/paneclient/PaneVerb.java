package edu.brown.cs.paneclient;

public abstract class PaneVerb {
	PaneTime _start;
	PaneTime _end;
	PaneUser _principal; // the issuer of this command
	PaneShare _share;
	
	
	//-------------------start
	public void setStart(PaneTime start) {
		_start = start;
	}

	public boolean isSetStart() {
		return _start == null?false:true;
	}

	public PaneTime getStart() {
		return _start;
	}
	
	//-------------------end
	public void setEnd(PaneTime end) {
		_end = end;
	}

	public boolean isSetEnd() {
		return _end == null?false:true;
	}

	public PaneTime getEnd() {
		return _end;
	}
	
	//-------------------issuer
	public void setUser(PaneUser user) {
		_principal = user;
	}

	public boolean isSetUser() {
		return _principal == null?false:true;
	}

	public PaneUser getUser() {
		return _principal;
	}
	
	//-------------------share
	public void setSourceShare(PaneShare share) {
		_share = share;
	}

	public boolean isSetSourceShare() {
		return _share == null?false:true;
	}

	public PaneShare getSourceShare() {
		return _share;
	}
	
	/**
	 * generate the command according to all these settings
	 * @return
	 */
	public abstract String generateCmd();
	
	public void setParent(PaneShare share) {
		_share = share;
	}
	
	public abstract String toString();
}
