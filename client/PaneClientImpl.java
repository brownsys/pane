import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.List;


public class PaneClientImpl implements PaneClient{
	
	InetAddress _serverIP;
	int _serverPort;
	Socket _serverSock;
	
	public PaneClientImpl(InetAddress serverIP, int serverPort) throws IOException{
		_serverIP = serverIP;
		_serverPort = serverPort;
		_serverSock = new Socket(serverIP,serverPort);
	}
	
	public PaneUser addUser(PaneUser user) throws IOException{
		
		String cmd = "AddUser "+user.getName();
		String response = sendAndWait(cmd);		
		//if response == succeed
		
		return user;
	}

	public List<PaneShare> listShares(PaneUser user) throws IOException{
		return null;
	}
	
	public List<PaneShare> listSharesByFlowGroup(PaneFlowGroup flowgroup) throws IOException{
		return null;
	}

	@Override
	public PaneShare getRootShare() throws IOException {
		

		//---------------only for test purpose-------------------------
		PaneShare root = new PaneShare("root",1000, null);
		root.setClient(this);
		//-------------------------------------------------------------
		
		return root;
	}

	@Override
	public boolean authenticate(PaneUser user) throws IOException {
		
		String response = sendAndWait(user.getName());
		//if response == succeed return true=
		//else return false
		return true;
	}
	
	@Override
	public String sendAndWait(String cmd) throws IOException{
		byte[] ret = new byte[1000];
		OutputStream os = _serverSock.getOutputStream();
		InputStream is = _serverSock.getInputStream();
		
		os.write(cmd.getBytes());
		os.flush();
		is.read(ret);
		
		return new String(ret);
	}
	
	@Override
	public void send(String cmd) throws IOException{
		OutputStream os = _serverSock.getOutputStream();
		os.write(cmd.getBytes());
		os.flush();
	}
	
	@Override
	public String toString(){
		return "PaneClientImpl: serverIP: " + _serverIP + " serverPort: " + _serverPort;
	}

}
