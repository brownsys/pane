import java.util.ArrayList;


public class paneUser {
	String _userName;
	ArrayList<paneShare> _allShares;
	ArrayList<paneReservation> _allReservations;
	
	public paneUser(String userName){
		_userName = userName;
		_allShares = new ArrayList<paneShare>();
		_allReservations = new ArrayList<paneReservation>();
	}
	
	public void addShare(paneShare share){
		
	}
	
	public ArrayList<paneShare> listShares(){
		return null;
	}
	
	public void addReservation(paneReservation resv){
		
	}
	
	public ArrayList<paneReservation> listReservations(){
		return null;
	}

}
