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
		_allShares.add(share);
	}
	
	public ArrayList<paneShare> listShares(){
		return _allShares;
	}
	
	public void addReservation(paneReservation resv){
		_allReservations.add(resv);
	}
	
	public ArrayList<paneReservation> listReservations(){
		return _allReservations;
	}
	
	public String getName(){
		return _userName;
	}

}
