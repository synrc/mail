defmodule CHAT.Mixfile do
  use Mix.Project

  def project do
    [app: :chat,
     version: "5.11.0",
     description: "CHAT Protocol",
     package: package(),
     deps: deps()]
  end

  def application do
     [mod: {:chat, []}, applications: [:ranch, :cowboy, :kvs, :syn, :n2o]]
  end

  defp package do
    [files: ~w(include priv src LICENSE rebar.config sys.config vm.args),
     licenses: ["ISC"],
     maintainers: ["Namdak Tonpa","Vladimir Kirillov"],
     name: :chat,
     links: %{"GitHub" => "https://github.com/synrc/chat"}]
  end

  defp deps do
     [{:ex_doc, "~> 0.11", only: :dev},
      {:cowboy, "~> 2.5"},
      {:rocksdb, "~> 1.3.2"},
      {:syn, "~> 1.6.3"},
      {:n2o, "~> 6.8.1"},
      {:kvs, "~> 6.7.7"}]
  end
end
