defmodule CHAT.Mixfile do
  use Mix.Project

  def project do
    [app: :chat,
     version: "3.4.0",
     description: "CHAT Protocol",
     package: package(),
     deps: deps()]
  end

  def application do
     [mod: {:chat, []}, applications: [:n2o, :kvx]]
  end

  defp package do
    [files: ["include", "priv", "src", "LICENSE", "README.md", "rebar.config", "sys.config", "vm.args"],
     licenses: ["MIT"],
     maintainers: ["Namdak Tonpa","Vladimir Kirillov"],
     name: :chat,
     links: %{"GitHub" => "https://github.com/synrc/chat"}]
  end

  defp deps do
     [{:syn,    github: "ostinelli/syn", tag: "1.5.0"},
      {:cowboy, github: "voxoz/cowboy2", override: :true},
      {:kvx,    github: "synrc/kvx", override: :true},
      {:n2o,    github: "synrc/n2o", override: :true},
      {:ex_doc, ">= 0.0.0", only: :dev}]
  end
end
