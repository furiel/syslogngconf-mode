;;; syslogngconf-mode.el --- major mode for editing syslog-ng configuration file

;; Author: Antal Nemes
;; Version: 0.0.1
;; Package-version: 20181229

;;; Commentary:

;;; Code:

(defvar syslogngconf-mode-hook nil)

(defun syslogngconf-prepare-keywordlist-for-optimization (l)
  (flet ((hyphen-version (str) (replace-regexp-in-string "_" "-" str)))
    (delete-dups
     (append
      l
      (mapcar 'hyphen-version l)))))

(defvar syslogngconf-mode-map
  (let ((syslogngconf-mode-map (make-keymap)))
    syslogngconf-mode-map)
  "Keymap for syslogngconf major mode")

(defconst syslogngconf-keywords-builtins-regexp-optimized
  (concat "\\<"
          (regexp-opt
           (syslogngconf-prepare-keywordlist-for-optimization
            '(
              "source" "filter" "parser" "rewrite" "destination" "log"
              "junction" "channel" "options" "@include" "block" "if"
              "else" "elif" "@version" "@module" "@define" "@requires"))
            t)
          "\\>"))

(defconst syslogngconf-keywords-regexp-consts-optimized
  (concat "\\<"
          (regexp-opt
           (syslogngconf-prepare-keywordlist-for-optimization
            '(
              "internal" "yes" "on" "no" "off" "chars" "strings" "null"
              ))
           t)
          "\\>"))

(defconst syslogngconf-keywords-regexp-options-optimized
  (concat "\\<"
          (regexp-opt
           (syslogngconf-prepare-keywordlist-for-optimization
            '(
              "value_pairs" "exclude" "pair" "key" "scope" "rekey"
              "shift" "add_prefix" "replace" "replace_prefix" "flags"
              "pad_size" "mark_freq" "mark" "mark_mode" "stats_freq"
              "stats_lifetime" "stats_level" "stats"
              "stats_max_dynamics" "min_iw_size_per_reader"
              "flush_lines" "flush_timeout" "suppress" "sync_freq"
              "sync" "long_hostnames" "chain_hostnames"
              "normalize_hostnames" "keep_hostname" "check_hostname"
              "bad_hostname" "custom_domain" "keep_timestamp"
              "encoding" "ts_format" "frac_digits" "time_zone"
              "recv_time_zone" "send_time_zone" "local_time_zone"
              "format" "use_time_recvd" "use_fqdn" "use_dns"
              "time_reopen" "time_reap" "time_sleep" "file_template"
              "proto_template" "default_level" "default_priority"
              "default_facility" "threaded" "use_rcptid" "use_uniqid"
              "log_fifo_size" "log_fetch_limit" "log_iw_size"
              "log_msg_size" "log_prefix" "program_override"
              "host_override" "throttle" "create_dirs" "optional"
              "owner" "group" "perm" "dir_owner" "dir_group" "dir_perm"
              "template" "template_escape" "template_function"
              "on_error" "persist_only" "dns_cache_hosts" "dns_cache"
              "dns_cache_size" "dns_cache_expire"
              "dns_cache_expire_failed" "pass_unix_credentials"
              "persist_name" "retries" "batch_lines" "batch_timeout"
              "read_old_records" "type" "tags" "set_tag" "clear_tag"
              "condition" "groupset" "groupunset" "value" "values"
              "add_contextual_data" "database" "selector"
              "default_selector" "prefix" "filters" "ignore_case"
              "vhost" "host" "port" "exchange" "exchange_declare"
              "exchange_type" "routing_key" "persistent" "username"
              "password" "max_channel" "frame_size" "body" "ca_file"
              "key_file" "cert_file" "peer_verify" "tls" "base_dir"
              "filename_pattern" "recursive" "max_files"
              "monitor_method" "fsync" "remove_if_older"
              "overwrite_if_older" "follow_freq" "multi_line_mode"
              "multi_line_prefix" "multi_line_garbage"
              "multi_line_suffix" "uri" "collection" "servers"
              "safe_mode" "path" "keep_alive" "inherit_environment"
              "smtp" "subject" "from" "to" "cc" "bcc" "reply_to"
              "sender" "header" "dhparam_file" "pkcs12_file" "ca_dir"
              "crl_dir" "trusted_keys" "trusted_dn" "cipher_suite"
              "ecdh_curve_list" "curve_list" "ssl_options"
              "allow_compress" "localip" "ip" "interface" "localport"
              "destport" "ip_ttl" "ip_tos" "ip_freebind" "so_broadcast"
              "so_rcvbuf" "so_sndbuf" "so_keepalive" "so_reuseport"
              "tcp_keep_alive" "tcp_keepalive" "tcp_keepalive_time"
              "tcp_keepalive_probes" "tcp_keepalive_intvl"
              "spoof_source" "transport" "ip_protocol"
              "max_connections" "listen_backlog" "systemd_syslog"
              "failover_servers" "failover" "failback"
              "tcp_probe_interval" "successful_probes_required" "table"
              "columns" "indexes" "session_statements" "default" "null"
              "retry_sql_inserts" "create_statement_append"
              "ignore_tns_config" "dbd_option" "destination" "ack"
              "dialect" "delimiters" "quotes" "quote_pairs"
              "time_stamp" "file" "inject_mode" "drop_unmatched"
              "timeout" "aggregate" "inherit_mode" "where" "having"
              "trigger" "mem_buf_length" "disk_buf_size" "reliable"
              "mem_buf_size" "qout_size" "dir" "startup" "setup"
              "teardown" "shutdown" "user" "user_agent" "urlfasz"
              "headers" "method" "use_system_cert_store" "ssl_version"
              "accept_redirects" "flush_bytes" "batch_bytes"
              "body_prefix" "body_suffix" "delimiter" "workers"
              "class_path" "class_name" "option" "jvm_options"
              "json_parser" "marker" "extract_prefix" "value_separator"
              "pair_separator" "extract_stray_words_into" "openbsd"
              "class" "loaders" "command" "auth" "server" "service"
              "event_time" "seconds" "microseconds" "state"
              "description" "metric" "ttl" "attributes" "cacert" "cert"
              "set_message_macro" "syslog_parser" "max_field_size"
              "tags_parser" "drop_invalid" "exclude_tags"
              "strip_whitespaces" "freq" "bytes" "udp-port" "tcp-port"
              "rfc5424-tls-port" "rfc5424-tcp-port" "index"
              "flush_limit" "client_mode" "cluster" "custom_id"
              "resource" "client_lib_dir" "concurrent_requests"
              "skip_cluster_health_check" "cluster_url"
              "java_keystore_filepath" "java_keystore_password"
              "java_truststore_filepath" "java_truststore_password"
              "java_ssl_insecure" "http_auth_type"
              "http_auth_type_basic_username"
              "http_auth_type_basic_password" "payload"
              "hdfs_uri" "hdfs_file" "hdfs_archive_dir"
              "hdfs_resources" "hdfs_max_filename_length"
              "kerberos_principal" "kerberos_keytab_file"
              "hdfs_append_enabled" "topic" "kafka_bootstrap_servers"
              "properties_file" "sync_send" "filename" "token" "tag"
              "hook-url" "fallback" "colors" "color-chooser" "pretext"
              "author-name" "author-link" "author-icon" "title"
              "title-link" "image-url" "thumb-url" "footer" "footer-icon"
              "bot-id" "chat-id" "parse-mode" "disable-web-page-preview"))
            t)
           "\\>"))


(defconst syslogngconf-keywords-regexp-functions-optimized
  (concat "\\<"
          (regexp-opt
           (syslogngconf-prepare-keywordlist-for-optimization
            '(
              "or" "and" "not" "lt" "le" "eq" "ne" "ge" "gt" "<" "<="
              "==" "!=" ">=" ">" "level" "priority" "facility"
              "program" "host" "message" "match" "netmask" "tags"
              "in_list" "netmask6" "value" "flags" "set" "unset"
              "subst" "amqp" "file" "fifo" "pipe" "stdin"
              "wildcard_file" "mongodb" "unix_dgram" "unix_stream"
              "udp" "tcp" "syslog" "network" "udp6" "tcp6" "sql"
              "stomp" "sun_stream" "sun_streams" "door" "usertty"
              "application" "csv_parser" "date_parser" "db_parser"
              "disk_buffer" "grouping_by" "geoip" "geoip2"
              "hook_commands" "http" "java" "kv_parser"
              "linux_audit_parser" "map_value_pairs" "python"
              "python_fetcher" "pseudofile" "redis" "riemann"
              "snmptrapd_parser" "systemd_journal" "xml"
              "example_msg_generator" "example_random_generator"
              "example_diskq_source" "cisco-parser"
              "default-network-drivers" "elasticsearch"
              "elasticsearch2" "ewmm-parser" "syslog-ng" "ewmm"
              "graphite" "graylog2" "hdfs" "iptables-parser" "kafka"
              "network-load-balancer" "loggly" "logmatic" "nodejs"
              "osquery" "mbox" "credit-card-hash" "credit-card-mask"
              "pacct" "snmptrap" "extract-solaris-msgid" "sudo-parser"
              "windows-eventlog-parser" "system" "telegram" "slack"
              "linux-audit"))
            t)
          "\\>"))

(defvar syslogngconf-font-lock-keywords
  (list
   `(,syslogngconf-keywords-builtins-regexp-optimized . font-lock-builtin-face)
   `(,syslogngconf-keywords-regexp-consts-optimized . font-lock-constant-face)
   `(,syslogngconf-keywords-regexp-options-optimized . font-lock-type-face)
   `(,syslogngconf-keywords-regexp-functions-optimized . font-lock-function-name-face)
  "Default syntax highlighting expressions for syslogngconf mode"))

(defvar syslogngconf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "syntax table for syslogngconf mode")

(define-derived-mode syslogngconf-mode fundamental-mode "syslogngconf"
  "Major mode for editing syslog-ng configuration files"
  :syntax-table syslogngconf-mode-syntax-table
  (use-local-map syslogngconf-mode-map)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults '(syslogngconf-font-lock-keywords)))

(provide 'syslogngconf-mode)

;;; syslogngconf-mode.el ends here
