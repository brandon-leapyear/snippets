# Runs a server on port 8080 that takes in a curl request and outputs the IP address.

import socket
import sys

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
sock.bind(('0.0.0.0', 8080))
sock.listen(5)
while True:
    (conn, (ip, _)) = sock.accept()
    print(ip)
    sys.stdout.flush()
    conn.recv(4096)
    conn.send('Received connection from: {}'.format(ip))
    conn.close()
