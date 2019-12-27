# hauth

## PostgreSQL

```
sudo /etc/init.d/postgresql start
sudo /etc/init.d/postgresql stop
sudo /etc/init.d/postgresql restart
```

```
> sudo -i -u postgres
> psql
> \q
```

## RabbitMQ

```
service rabbitmq-server start
rabbitmq-plugins enable rabbitmq_management
```

```
http://guest:guest@localhost:15672/
```