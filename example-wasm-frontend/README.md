If running it throws an error about "configured user limit (128) on the number of inotify instances", try
```bash
echo fs.inotify.max_user_instances=524288 | sudo tee -a /etc/sysctl.conf && sudo sysctl -p
```
See issues like https://github.com/dotnet/aspnetcore/issues/8449
