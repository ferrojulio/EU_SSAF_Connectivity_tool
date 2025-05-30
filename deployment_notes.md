# Deployment Notes for Soil Connectivity Self-Assessment App

## Server Information

* **Provider**: DigitalOcean
* **Droplet OS**: Ubuntu (version as of May 2025)
* **Server hostname**: `securite-du-sol`
* **Domain name**: `www.securite-du-sol.com`

## Web Server Configuration

* **Web server**: Nginx
* **Reverse proxy**: Yes (Nginx forwards requests to Shiny Server on port 3838)
* **SSL certificate**: Let's Encrypt (via Certbot)
* **HTTPS Setup**: Nginx handles HTTPS termination with redirect from HTTP

### Key Nginx Snippet

```nginx
server {
    listen 80;
    server_name securite-du-sol.com www.securite-du-sol.com;
    location / {
        proxy_pass http://127.0.0.1:3838/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
        proxy_read_timeout 20d;
        proxy_buffering off;
    }
}
```

## Shiny Server Setup

* **Installed version**: v1.5.23.1030
* **Root directory**: `/srv/shiny-server`
* **Apps**:

  * `/srv/shiny-server/index/` → main landing app
  * `/srv/shiny-server/farmers/` → farmer-specific app
  * Land managers and policymakers folders are structured but not yet live.

## Project Files

* Project root contains:

  * `index/` and `farmers/` folders
  * `utils.R` shared by all apps
  * `all_gadm41_centroids_level2.rds` (used by `utils.R`)
* Each app has its own translations `.rds` file and `www/app.css` for custom styles. but they are copies of the same.

## SSL/TLS Certificates

* Certbot is used for certificate management
* DNS entries managed on o2switch
* Ensure TXT verification and propagation before running Certbot with `--manual`

## Git Repository

* Repo updated directly from droplet
* SSH key or HTTPS method used (HTTPS preferred)
* Run `git config --global user.email` and `user.name` before first commit on server

## R Environment

* R version: 4.4.1 (2024-06-14)
* `renv` used for dependency management
* `renv.lock` committed to repo

### Important Packages

```
DBI, dplyr, htmltools, htmlwidgets, leaflet, readr, RPostgres,
RSQLite, scales, sf, shiny, stringi
```

## Troubleshooting Tips

* 500 Internal Server Error → check `/var/log/shiny-server/*.log`
* `translations` object not found → ensure `.rds` is loaded early enough in `app.R`
* Use `sudo su - shiny -s /bin/bash` to test R as the `shiny` user
* Use `curl -I http://127.0.0.1:3838/<app>` to verify local port is working

## To Do

* Enable `robots.txt` and sitemap if SEO is needed
* Add Uptime monitoring (e.g., UptimeRobot)
* Finalise DNS + Certbot wildcard setup if desired
* Improve LCP / performance on Chrome
