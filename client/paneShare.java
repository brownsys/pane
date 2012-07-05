import java.rmi.RemoteException;


public interface paneShare {
	
	/**
	 * call 'addUser' command.
	 * 
	 * @param userName
	 *            name of the new user to be added.
	 */
	public boolean addUser(String userName) throws RemoteException;
	
	/**
	 * call 'grant' command.
	 * 
	 * @param shareName
	 *            name of the share
	 * @param userName
	 *            name of the user.          
	 */
	public boolean grant(String shareName, String userName) throws RemoteException;
	
	/**
	 * call 'newShare' command.
	 * 
	 * @param shareName
	 *            the name of the new share
	 * @param parentName
	 *            the name of the parent share
	 * @param maxresv
	 *            the maximum bandwidth requested by the new share     
	 * @param minresv
	 *            the minimum bandwidth requested by the new share (optional)      
	 * @param userName
	 *            the user of the new share (optional)
	 * @param allow
	 *            argument for the command (optional)
	 * @param deny
	 *            argument for the command (optional)
	 * @param reserveTBCapacity
	 *            argument for the command (optional)
	 * @param reserveTBFill
	 *            argument for the command (optional)

	 * @return
	 */
	public boolean newShare(String sharename, String parentName, String userName, 
			int maxresv, int minresv, boolean allow, boolean deny, 
			int reserveTBCapacity, int reserveTBFill) throws RemoteException;
	
	/**
	 * call 'reserve' command.
	 * 
	 * @param shareName
	 *            the name of the share that is asking for bandwidth
	 * @param bandwidth
	 *            the bandwidth to be reserved
	 * @param usrName
	 *            the name of the user that is asking for bandwidth (optional)
	 * @param dstport
	 *            the dst port for the reservation (optional)
	 * @param srcport
	 *            the src port for the reservation (optional)
	 * @param from
	 *            the time for the reservation to be started (optional)
	 * @param to
	 *            the time for the reservation to be ended (optional)
	 * @return
	 */
	public boolean reserve(String shareName, int bandwidth,String usrName, 
			int dstport, int srcport,int from, int to) throws RemoteException;
	
	/**
	 * call 'allow' command.
	 * @param shareName
	 *            name of the share
	 * @param usrname
	 *            the user name (optinal)
	 * @param srcHost
	 *            src host (optional)
	 * @param srcPort
	 *            src port (optional)
	 * @param dstHost
	 *            dst host (optional)
	 * @param dstPort
	 *            dst port (optional)
	 * @return
	 */
	public boolean allow(String shareName, String usrname, String srcHost, int srcPort,
			String dstHost, int dstPort) throws RemoteException;
	
	
	/**
	 * call 'deny' command. 
	 * @param shareName
	 *            name of the share
	 * @param usrname
	 *            the user name (optinal)
	 * @param srcHost
	 *            src host (optional)
	 * @param srcPort
	 *            src port (optional)
	 * @param dstHost
	 *            dst host (optional)
	 * @param dstPort
	 *            dst port (optional)

	 * @return
	 */
	public boolean deny(String shareName, String usrName, String srcHost, int srcPort,
			String dstHost, int dstPort) throws RemoteException;
	
	/**
	 * call 'Tick' command
	 * 
	 * @param time
	 * @return
	 */
	public boolean tick(int time) throws RemoteException;

}

