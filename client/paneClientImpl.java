import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.LinkedList;


public class paneClientImpl implements paneClient{
	
	InetAddress _serverIP;
	int _serverPort;
	Socket _serverSock;
	
	public void initialize(InetAddress serverIP, int serverPort) throws IOException{
		_serverIP = serverIP;
		_serverPort = serverPort;
		_serverSock = new Socket(serverIP,serverPort);
	}
	
	public paneUser addUser(String userName) throws IOException{
		
		String cmd = "AddUser "+userName;
		byte[] ret = new byte[1000];
		OutputStream os = _serverSock.getOutputStream();
		InputStream is = _serverSock.getInputStream();
		
		os.write(cmd.getBytes());
		os.flush();
		is.read(ret);
		
		paneUser user = new paneUser(userName);
		
		return user;
	}

	public LinkedList<paneShare> listShares(paneUser user) throws IOException{
		return null;
	}
	
	public LinkedList<paneShare> listSharesByFlowGroup(paneFlowGroup flowgroup) throws IOException{
		return null;
	}

	@Override
	public paneShare getRootShare() throws IOException {
		

		//----------------------------------------
		paneShare root = new paneShare("root",null,1000);
		root.setServerSoc(_serverSock);
		//-----------------------------------
		
		return root;
	}

}
