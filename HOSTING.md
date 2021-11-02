# Running Hoogleplus

So you want to host and run Hoogle+?
The setup is done in two parts.
First, there's the backend, which runs on port 5000 by default.
Then there's the frontend, which runs on port 3000 by default.
Apache connects these two parts

## Apache Redirects
On goto, in the file /etc/apache2/sites-enabled/102-hoogleplus.conf:
```
<VirtualHost *:80>
        ServerName hoogleplus.goto.ucsd.edu
        # ServerAdmin ...?

        ProxyPass /api/ http://localhost:5000/
        ProxyPassReverse /api/ http://localhost:5000/

        ProxyPass / http://localhost:3000/
        ProxyPassReverse / http://localhost:3000/
</VirtualHost>
```
We have a similar file for https, which sends the port 443 traffic to the same internal addresses as above.

## Backend
Note the directory, `web_server`:
```
web_server/$ ./start_server.sh
```
If you wish to tweak the backend's port, edit the above file.


## Frontend
Note the directory, `new_webapp`:
```
new_webapp$ REACT_APP_DEVELOPMENT_HOST=api/ yarn start
```

The `REACT_APP_DEVELOPMENT_HOST` environment variable indicates the path prefix for all requests to the backend.
For Goto, this is `api/` because it sends requests to the API backend via the apache redirect.