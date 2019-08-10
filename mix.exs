defmodule CHAT.Mixfile do
  use Mix.Project

  def project do
    [app: :chat,
     version: "3.8.0",
     description: "CHAT Protocol",
     package: package(),
     deps: deps()]
  end

  def application do
     [mod: {:chat, []}, applications: [:ranch, :cowboy, :kvs, :syn, :n2o]]
  end

  defp package do
    [files: ["include", "priv", "src", "LICENSE", "README.md", "rebar.config", "sys.config", "vm.args"],
     licenses: ["MIT"],
     maintainers: ["Namdak Tonpa","Vladimir Kirillov"],
     name: :chat,
     links: %{"GitHub" => "https://github.com/synrc/chat"}]
  end

  defp deps do
     [{:cowboy, "~> 2.5"},
      {:rocksdb, "~> 1.2.0"},
      {:syn, "~> 1.6.3"},
      {:n2o, "~> 6.8.1"},
      {:kvs, "~> 6.7.3"}]
  end
end
