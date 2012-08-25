package edu.brown.cs.paneclient;
import java.util.List;


public class PaneUser {
	String _userName;
	PaneClient _client;
	
	public PaneUser(String userName, PaneClient client) {
		_userName = userName;
		_client = client;
	}
	
	
	/**
	 * I just didn't find any examples, so I'm not sure 
	 * about how shares and reservations are represented. Therefore
	 * I left the following two methods blank
	 * @return
	 */
	public List<PaneShare> listShares() {
		return null;
	}
	
	public List<PaneReservation> listReservations() {
		return null;
	}
	
	public String getName() {
		return _userName;
	}
	
	public String toString() {
		return "PaneUser: user: " + _userName+_client.toString();
	}

}
