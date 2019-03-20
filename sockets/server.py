import socket, ssl, os
context = ssl.create_default_context(ssl.Purpose.CLIENT_AUTH)
context.load_cert_chain(certfile=os.environ['CERT'], keyfile=os.environ['KEY'])

sock = socket.socket()
sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
sock.bind((os.environ['HOST'], int(os.environ['PORT'])))
sock.listen(5)

while True:
    client, addr = sock.accept()
    conn = None
    try:
        conn = context.wrap_socket(client, server_side=True)
    except Exception as e:
        print(e)
    else:
        try:
            data = conn.recv(1024)
            conn.send(b'Hello world')
            print('Received from {}: {}'.format(data, addr))
        finally:
            conn.close()
