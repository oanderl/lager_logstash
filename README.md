lager_logstash
=====

Logstash backend for [lager](https://github.com/erlang-lager/lager).

Configuration
-----
Add `lager` and `lager_logstash` to your rebar3 dependencies list.
```erlang
{deps, [lager, {git, "git@github.com:oanderl/lager_logstash.git", {branch, "main"}}]}.
```
Setup a [logstash](https://www.elastic.co/logstash) pipeline ([reference](https://www.elastic.co/guide/en/logstash/current/configuration.html))
using the `udp` input protocol on your preferred port with `json` codec.
```
input {
    udp {
        port => 1234
        codec => json
    }
}
...
```
Add the `lager_logstash_backend` to the `lager` handler list in your application configuration file.
```erlang
{lager, [
    {handlers, [
        {lager_logstash_backend, [
            {host, "your.logstash.host"}, %% required
            {port, 1234}, %% required
            {blacklist, [blacklisted, "metadata", <<"keys">>]}, %% optional
            {whitelist, [whitelisted, "metadata", <<"keys">>]} %% optional
            %% when a key is not whitelisted it will be turned into a string
            %% in the json that is sent to logstash
            %% be careful when whitelisting keys since whitelisting a key that could
            %% possibly include a field that is not json encodable by jsx,
            %% will crash the backend process and some logs may not reach logstash
        ]}
    ]}
]}
```
