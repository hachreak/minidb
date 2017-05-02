clean:
	rm data_* data.* log* ring_state_* ttb_last_config compile_commands.json -Rf

node1:
	rebar3 shell --name test1@127.0.0.1 --config config/vars_dev1.config --apps minidb

node2:
	rebar3 shell --name test2@127.0.0.1 --config config/vars_dev2.config --apps minidb
