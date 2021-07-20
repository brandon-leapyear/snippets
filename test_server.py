import socket, os
from datetime import datetime

sock = socket.socket()
sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
sock.bind(('localhost', 4408))
sock.listen(5)

while True:
    conn, addr = sock.accept()
    try:
        data = conn.recv(1024)
        conn.send(b'HTTP/1.0 502 Bad Gateway\n\n')
        print('[{}] Received from {}: {}'.format(datetime.now(), data, addr))
    finally:
        conn.close()
