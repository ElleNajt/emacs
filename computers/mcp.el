;;; computers/mcp.el -*- lexical-binding: t; -*-



;;; MCP
;; taken from https://github.com/lizqwerscott/mcp.el



;; ("mcp_nixos" . (:command "/home/elle/code/mcp_servers/.venv/bin/python" :args ("-m" "mcp_nixos")))

(setq mcp-hub-servers
      '(("emacs_introspection" . (:command "npx" :args ("-y" "@lnajt/emacs-introspection-mcp@latest")) )
        ("nixos" . (:command "uvx" :args ("--install-deps" "mcp-nixos")))
        ("memory" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-memory")))
        ("sequential_thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
        ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/elle/code/ai_agents/")))))

(progn

  (gptel-mcp-close-use-tool)

  (mcp-hub-restart-all-server)

  (gptel-mcp-register-tool)

  (gptel-mcp-use-tool)
  )

(setq mysecretnumber "42.00")
