import socket, ssl, os

context = ssl.create_default_context()
sock = socket.socket()
conn = context.wrap_socket(sock, server_hostname=os.environ['HOST'])
conn.connect((os.environ['HOST'], int(os.environ['PORT'])))
print(conn.getpeercert())
conn.send(b'This is from the client')
print(conn.recv(1024))
conn.close()
