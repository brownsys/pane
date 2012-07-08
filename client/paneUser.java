import java.util.ArrayList;


public class paneUser {
	String _userName;
	ArrayList<paneShare> _allShares;
	
	public paneUser(String userName){
		_userName = userName;
		_allShares = new ArrayList<paneShare>();
	}
	
//	/**
//	 * once a share is granted to the user, record this locally
//	 * @param share
//	 */
//	public void grantShare(paneShare share){
//		_allShares.add(share);
//	}
}
